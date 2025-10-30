' Copyright (c) 2013-2025 Bruce A Henderson
'
' Based on the public domain Monkey "trans" by Mark Sibly
'
' This software is provided 'as-is', without any express or implied
' warranty. In no event will the authors be held liable for any damages
' arising from the use of this software.
'
' Permission is granted to anyone to use this software for any purpose,
' including commercial applications, and to alter it and redistribute it
' freely, subject to the following restrictions:
'
'    1. The origin of this software must not be misrepresented; you must not
'    claim that you wrote the original software. If you use this software
'    in a product, an acknowledgment in the product documentation would be
'    appreciated but is not required.
'
'    2. Altered source versions must be plainly marked as such, and must not be
'    misrepresented as being the original software.
'
'    3. This notice may not be removed or altered from any source
'    distribution.
'

Const DECL_EXTERN:Long=        $010000
Const DECL_PRIVATE:Long=       $020000
Const DECL_ABSTRACT:Long=      $040000
Const DECL_FINAL:Long=         $080000
Const DECL_READ_ONLY:Long=     $000100
Const DECL_STATIC:Long=      $20000000
Const DECL_OVERRIDE:Long=    $40000000
Const DECL_INLINE:Long=      $80000000
Const DECL_THREADED:Long=   $100000000:Long
Const DECL_NO_VAR:Long=     $200000000:Long

Const DECL_SEMANTED:Long=      $100000
Const DECL_SEMANTING:Long=     $200000
Const DECL_CYCLIC:Long=       $8000000

Const DECL_POINTER:Long=       $400000

Const DECL_ARG:Long=           $800000
Const DECL_INITONLY:Long=     $1000000

Const DECL_NODEBUG:Long=      $2000000
Const DECL_PROTECTED:Long=    $4000000
Const DECL_EXPORT:Long=       $8000000

Const DECL_API_CDECL:Long=   $00000000
Const DECL_API_STDCALL:Long= $10000000
Const DECL_API_DEFAULT:Long=DECL_API_CDECL
Const DECL_API_FLAGS:Long=   DECL_API_CDECL | DECL_API_STDCALL

Const DECL_NESTED:Long=      $20000000

Const CLASS_INTERFACE:Long=    $002000
Const CLASS_THROWABLE:Long=    $004000
Const CLASS_STRUCT:Long=       $008000
Const CLASS_GENERIC:Long=      $001000
Const CLASS_FLAGS:Long = CLASS_INTERFACE | CLASS_THROWABLE | CLASS_STRUCT | CLASS_GENERIC

Const SCOPE_FUNC:Int = 0
Const SCOPE_CLASS_LOCAL:Int = 1
Const SCOPE_CLASS_HEIRARCHY:Int = 2
Const SCOPE_MODULE:Int = 3
Const SCOPE_ALL:Int = 4

Const BLOCK_OTHER:Int =     $000
Const BLOCK_LOOP:Int =      $001
Const BLOCK_TRY:Int =       $002
Const BLOCK_CATCH:Int =     $004
Const BLOCK_FINALLY:Int =   $008
Const BLOCK_IF:Int =        $010
Const BLOCK_ELSE:Int =      $020
Const BLOCK_FUNCTION:Int =  $040

Const BLOCK_TRY_CATCH:Int = BLOCK_TRY | BLOCK_CATCH
Const BLOCK_IF_ELSE:Int =   BLOCK_IF | BLOCK_ELSE

Const OPTION_WANT_LOOP_LABEL:Int = 1
Const OPTION_WANT_DATA_LABEL:Int = 2

'Const CALL_CONV_CDECL:Int = 0
'Const CALL_CONV_STDCALL:Int = 1
'Const CALL_CONV_DEFAULT:Int = CALL_CONV_CDECL

Global _env:TScopeDecl
Global _envStack:TBCCObjectList=New TBCCObjectList

Global _appInstance:TAppDecl

Global _loopnest:Int

Function PushEnv( env:TScopeDecl )
	If _env _envStack.AddLast( _env )
	_env=env
End Function

Function PopEnv()
	_env=TScopeDecl( _envStack.RemoveLast() )
End Function

Type TFuncDeclList Extends TBCCObjectList
	Field ident:String
	Field _identLower:String
	
	Method IdentLower:String()
		If Not _identLower Then
			_identLower = ident.ToLower()
		End If
		Return _identLower
	End Method

	Method AddLast( value:Object )
		If Not Contains(value) Then
			Super.AddLast(value)
		End If
	End Method

End Type

Type TMetadata

	Field metadataString:String

	' key/value pairs
	Field meta:TMap

	Method InsertMeta(key:String, value:String)
		If Not meta Then
			meta = New TMap
		End If
		
		meta.Insert(key, value)
	End Method

	Method HasMeta:Int(key:String)
		Return meta And meta.Contains(key.ToLower())
	End Method
	
End Type

Type TDecl
	Field ident$
	Field munged$
	Field errInfo$
	Field actual:TDecl
	Field scope:TScopeDecl
	Field attrs:Long
	Field metadata:TMetadata = New TMetadata
	
	Field declImported:Int = False
	Field generated:Int
	
	Field _identLower:String
	Field scopeIndex:Int = -1
	
	Method New()
		errInfo=_errInfo
		actual=Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True) Abstract
	
	Method IdentLower:String()
		If Not _identLower Then
			_identLower = ident.ToLower()
		End If
		Return _identLower
	End Method
	
	Method ToString$()
		If TClassDecl( scope ) Return scope.ToString()+"."+ident
		Return ident
	End Method
	
	Method ToTypeString:String()
	End Method
	
	Method IsExtern:Int()
		Return (attrs & DECL_EXTERN)<>0
	End Method
	
	Method IsFinal:Int()
		Return (attrs & DECL_FINAL)<>0
	End Method

	Method IsPrivate:Int()
		Return (attrs & DECL_PRIVATE)<>0
	End Method

	Method IsProtected:Int()
		Return (attrs & DECL_PROTECTED)<>0
	End Method
	
	Method IsPublic:Int()
		Return Not (IsPrivate() Or IsProtected())
	End Method
	
	Method IsReadOnly:Int()
		Return (attrs & DECL_READ_ONLY)<>0
	End Method
	
	Method IsAbstract:Int()
		Return (attrs & DECL_ABSTRACT)<>0
	End Method

	Method IsStatic:Int()
		Return (attrs & DECL_STATIC)<>0
	End Method
	
	Method IsSemanted:Int()
		Return (attrs & DECL_SEMANTED)<>0
	End Method
	
	Method IsSemanting:Int()
		Return (attrs & DECL_SEMANTING)<>0
	End Method
	
	Method IsNoDebug:Int()
		Return (attrs & DECL_NODEBUG)<>0
	End Method
	
	Method IsThreaded:Int()
		Return (attrs & DECL_THREADED)<>0
	End Method

	Method IsImported:Int()
		Return declImported
	End Method
	
	Method FuncScope:TFuncDecl()
		If TFuncDecl( Self ) Return TFuncDecl( Self )
		If scope Return scope.FuncScope()
	End Method

	Method ClassScope:TClassDecl()
		If TClassDecl( Self ) Return TClassDecl( Self )
		If scope Return scope.ClassScope()
	End Method
	
	Method ModuleScope:TModuleDecl()
		If TModuleDecl( Self ) Return TModuleDecl( Self )
		' "app" is a module container
		If TAppDecl( Self ) Return TAppDecl( Self).mainModule
		If scope Return scope.ModuleScope()
	End Method
	
	Method AppScope:TAppDecl()
		If TAppDecl( Self ) Return TAppDecl( Self )
		If scope Return scope.AppScope()
	End Method
	
	' find an owning scope of function, class or module
	Method ParentScope:TScopeDecl()
		If scope Then
			' func scope
			If TFuncDecl( scope ) Return TFuncDecl( scope )
			' class scope
			If TClassDecl( scope ) Return TClassDecl( scope )
			' module scope
			If TModuleDecl( scope ) Return TModuleDecl( scope )

			Return scope.ParentScope()
		End If
	End Method
	
	Method CheckAccess:Int()
		If IsPrivate() And ModuleScope()<>_env.ModuleScope() Return False
		Return True
	End Method
	
	Method AssertAccess()
		If Not CheckAccess()
			If IsPrivate() Then
				Err ToString() +" is private."
			Else
				Err ToString() +" is protected."
			End If
		EndIf
	End Method

	Method Copy:TDecl(deep:Int = True)
		Local t:TDecl=OnCopy(deep)
		t.munged=munged
		t.errInfo=errInfo
		Return t
	End Method

	Method Semant()

		If IsSemanted() Return

		If IsSemanting() Then
			If attrs & DECL_CYCLIC Then
				Return
			End If
			Err "Cyclic declaration of '"+ident+"'."
		End If
		
		If actual<>Self
			actual.Semant
		EndIf

		PushErr errInfo
		
		If scope
			PushEnv scope
		EndIf
		
		attrs:|DECL_SEMANTING

		'If ident And ClassScope() Print "Semanting "+ToString()

		OnSemant
		
		attrs:&~DECL_SEMANTING

		attrs:|DECL_SEMANTED

		If scope 
			'If Not IsExtern()
			If TFuncDecl(Self) And attrs & FUNC_PTR
				'DebugLog "**** " + ident
			Else
			
				' a nested function/class needs to be scoped to another function, class or module.
				If attrs & FUNC_NESTED Or attrs & DECL_NESTED Then
					Local sc:TScopeDecl = ParentScope()
					
					' if our scope isn't one of the above, let it be so.
					If sc <> scope Then
						scope = Null
						sc.InsertDecl(Self)
					End If
				End If

				scope._semanted.AddLast Self
				
				If TGlobalDecl( Self )
					' FIXME
					If AppScope() Then
						If TGlobalDecl( Self ).mscope Then
							AppScope()._semanted.AddLast Self
						End If
						AppScope().semantedGlobals.AddLast TGlobalDecl( Self )
					End If
				Else
					If TModuleDecl( scope )
						' FIXME
						Local app:TAppDecl = AppScope()
						If app Then
							app._semanted.AddLast Self
						End If
					EndIf
				End If
			EndIf
			
			If TValDecl(Self) And TValDecl(Self).deferInit Then
				TValDecl(Self).SemantInit
			End If

			PopEnv
		Else
			If TValDecl(Self) And TValDecl(Self).deferInit Then
				TValDecl(Self).SemantInit
			End If
		EndIf

		Semant2()
		PopErr
	End Method

	Method Semant2()

		PushErr errInfo
	
		If scope
			PushEnv scope
		EndIf

		OnSemant2()

		If scope 
			PopEnv
		End If

		PopErr
	End Method
	
	Method InitInstance:TDecl( decl:TDecl )
		decl.ident=ident
		decl.munged=munged
		decl.errInfo=errInfo
		decl.actual=actual
		decl.scope=Null
		decl.attrs=attrs & ~(DECL_SEMANTED|DECL_SEMANTING)
		Return decl
	End Method
	
	Method GenInstance:TDecl()
		InternalErr "TDecl.GenInstance"
	End Method
	
	Method OnSemant() Abstract
	Method OnSemant2()
	End Method
	
	Method Clear()
	End Method

End Type

Type TValDecl Extends TDecl
	'pre-semant
	Field declTy:TType
	Field declInit:TExpr
	'post-semant
	Field ty:TType
	Field init:TExpr
	
	Field deferInit:Int = False
	
	Method ToString$()
		Local t$=Super.ToString()
		If ty Return t+":"+ty.ToString()
		If declTy Return t+":"+declTy.ToString()
		Return t+":?"
	End Method

	Method ToTypeString:String()
		If ty Return ty.ToString()
		If declTy Return declTy.ToString()
	End Method

	Method CopyInit:TExpr()
		If init Return init.Copy()
	End Method
	
	Method OnSemant()
'DebugStop	
		If declTy

			Local at:TType = TArrayType(declTy)
			
			While TArrayType(at)
				at = TArrayType(at).elemType
			Wend
		
			' ensure to set the scope for a function pointer array before semanting
			If TFunctionPtrType(at) Then
				If Not TFunctionPtrType(at).func.scope Then
					If scope Then
						TFunctionPtrType(at).func.scope = scope
					Else
						TFunctionPtrType(at).func.scope = _env
					End If
				End If
			End If

			' pass the scope into the function ptr
			If TFunctionPtrType(declTy) Then
				If Not TFunctionPtrType(declTy).func.scope Then
					If scope Then
						TFunctionPtrType(declTy).func.scope = scope
					Else
						TFunctionPtrType(declTy).func.scope = _env
					End If
				End If
			End If

			ty=declTy.Semant()

			If Not deferInit Then
				SemantInit()
			End If
			
		Else If declInit
			If Not deferInit Then
				SemantInit()
			End If
		Else
			InternalErr "TValDecl.OnSemant"
		EndIf
		
	End Method
	
	Method SemantInit()
		' for field initialisation, create a stub New() method to use as current scope
		' since fields are initialised in New(). Otherwise the scope would be "class", which is
		' incorrect for processing field inits.
		If TFieldDecl(Self) Then
			
			If Not declInit  And TClassDecl(scope) And Not TClassDecl(scope).IsStruct() Then
				declInit=New TConstExpr.Create( ty,"" )
			End If
			
			If declInit Then
				Local newScope:TFuncDecl = New TFuncDecl.CreateF( "new", Null,Null,FUNC_METHOD )
				newScope.scope = _env
				PushEnv(newScope)
			End If
		End If
	
		' for imported enum args with a default value, we need to set the type of the value to the enum
		' since at this point it's just a number with no context
		If TArgDecl(Self) And declInit And scope And scope.IsImported() And TEnumType(ty) Then
			If TConstExpr(declInit) Then
				declInit = New TConstExpr.Create(ty, TConstExpr(declInit).value).Semant()
			Else If TUnaryExpr(declInit) Then
				declInit = New TConstExpr.Create(ty, TUnaryExpr(declInit).Eval()).Semant()
			End If
		End If
			
		If declTy
			If declInit Then
				If TFunctionPtrType(ty) Then
					
					Local expr:TExpr
					
					If TInvokeExpr(declInit) Then
						expr = declInit.Copy()
					Else If TConstExpr(declInit) Then
						expr = declInit.Copy().Semant()
					Else If TFuncCallExpr(declInit) Then
						expr=declInit.Copy().Semant()
					Else If TNullExpr(declInit) Then
						expr = declInit
					Else
						' declInit can only be an expression, never a statement
						' this means that any function call in there is required to have parentheses, and will
						' thus appear in the form of a TFuncCallExpr
						' as such, trying SemantFunc in the Else branch seems pointless and will in fact wrongly
						' interpret function pointers (as TIdentExpr, TIndexExpr, possibly others?) as calls
						Rem
						Local argExpr:TExpr[] = New TExpr[0]

						For Local arg:TArgDecl = EachIn TFunctionPtrType(ty).func.argDecls
							Local ldecl:TLocalDecl = New TLocalDecl.Create(arg.ident, arg.declTy, Null, 0)
							ldecl.Semant()
							Local aexp:TVarExpr = New TVarExpr.Create(ldecl)
							'Local aexp:TIdentTypeExpr = New TIdentTypeExpr.Create(arg.declTy)
							aexp.Semant()
							argExpr :+ [aexp]
						Next

						expr=declInit.Copy().SemantFunc(argExpr, False, False)
						If Not expr Then
							expr = declInit.Copy().Semant()
						End If
						End Rem
						
						expr = declInit.Copy().Semant()
					End If
					
					If expr.exprType.EqualsType( ty ) Then
						init = expr
					Else
						init = New TCastExpr.Create( ty,expr,CAST_EXPLICIT ).Semant()
					End If
					
					
				Else
					If TArrayType(ty) And TArrayType(ty).isStatic Then
						init = declInit.Copy().Semant()
						If Not TConstExpr(init) and not TIdentEnumExpr(init) Then
							Err "Static array initialiser must be constant"
						End If
						If Not Int(TArrayType(ty).length) Then
							TArrayType(ty).length = init.Eval()
						End If
					Else
						If TArrayExpr(declInit) And TArrayType(ty) And TNumericType(TArrayType(ty).elemType) Then
							TArrayExpr(declInit).toType = TArrayType(ty).elemType
						End If
					
						init=declInit.Copy().SemantAndCast(ty)
						
						' check if struct has been initialised
						If TObjectType(ty) And TObjectType(ty).classDecl.IsStruct() And Not TObjectType(ty).classDecl.IsExtern() Then
						
							' new not used
							If TConstExpr(init) And Not TConstExpr(init).value And Not IsPointerType(ty,0,TType.T_POINTER) Then
								' always call the default constructor to init all the fields correctly
								init = New TNewObjectExpr.Create(ty, Null).Semant()
							End If
						End If
					End If
				End If
			End If
		Else If declInit
			init=declInit.Copy().Semant()
			ty=init.exprType.Copy()
			If attrs & DECL_NO_VAR And ty._flags & TType.T_VAR Then 
				ty._flags :~ TType.T_VAR ' remove var for variable 
			End If 
		End If
		
		If init Then
			If TVarExpr(init) And TVarExpr(init).decl = Self Then
				Err "Identifier '" + TVarExpr(init).decl.ident + "' not found."
			End If
			
			If TNewObjectExpr(init) And TVarExpr(TNewObjectExpr(init).instanceExpr) And TVarExpr(TNewObjectExpr(init).instanceExpr).decl = Self Then
				Err "Identifier '" + Self.ident + "' not found."
			End If
		End If
		
		' remove the temporary scope
		If TFieldDecl(Self) And declInit Then
			PopEnv()
		End If
	End Method
	
	Method Clear()
	End Method
	
End Type

Type TConstDecl Extends TValDecl
	Field value$
	
	Method Create:TConstDecl( ident$,ty:TType,init:TExpr,attrs:Long )
		Self.ident=ident
		Self.munged=ident
		Self.declTy=ty
		Self.declInit=init
		Self.attrs=attrs
		Return Self
	End Method

	Method GenInstance:TDecl()
		Local inst:TConstDecl = New TConstDecl
		InitInstance inst
		inst.declTy=declTy
		inst.declInit=declInit
		Return inst
	End Method

	Method OnCopy:TDecl(deep:Int = True)
		If IsSemanted() Then
			Return New TConstDecl.Create( ident,ty,CopyInit(), attrs )
		Else
			Return New TConstDecl.Create( ident, declTy, declInit, attrs)
		End If
	End Method
	
	Method OnSemant()
		Super.OnSemant()
		'If Not IsExtern() value=init.Eval()
		If init Then
			value=init.Eval()
			If TStringType(ty) And Not _appInstance.hasStringConst(value) Then
				_appInstance.mapStringConsts(value)
			End If
		End If
	End Method
	
	Method ToString$()
		Return "Const "+Super.ToString()
	End Method

End Type

Type TVarDecl Extends TValDecl

End Type

Type TLocalDecl Extends TVarDecl

	Field done:Int
	Field volatile:Int = False
	Field declaredInTry:TTryStmtDecl

	Method Create:TLocalDecl( ident$,ty:TType,init:TExpr,attrs:Long=0, generated:Int = False, volatile:Int = False )
		Self.ident=ident
		Self.declTy=ty
		Self.declInit=init
		Self.attrs=attrs
		Self.generated=generated
		Self.volatile=volatile
		Return Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Local decl:TLocalDecl = New TLocalDecl.Create( ident,declTy,declInit,attrs &~ DECL_SEMANTED, generated, volatile )
		decl.scope = scope
		decl.ty = ty
		decl.init = init
		decl.declaredInTry = declaredInTry
		Return decl
	End Method

	Method GetDeclPrefix:String()
		Return "Local "
	End Method

	Method OnSemant()
		If declTy Then
			If TObjectType(declTy) Then
				volatile = True
			End If
		End If
		Super.OnSemant()
	End Method
	
	Method ToString$()
		Return GetDeclPrefix() + Super.ToString()
	End Method

	Method Clear()
		done = False
	End Method

End Type

Type TArgDecl Extends TLocalDecl

	Field castTo:String
	
	Method Create:TArgDecl( ident$,ty:TType,init:TExpr,attrs:Long=0, generated:Int = False, volatile:Int = True )
		Self.ident=ident
		Self.declTy=ty
		Self.declInit=init
		Self.attrs=attrs
		Self.generated=generated
		Self.volatile=volatile
		Return Self
	End Method
	
	Method GenInstance:TDecl()
		Local inst:TArgDecl=New TArgDecl
		InitInstance inst
		inst.declTy=declTy
		inst.declInit=declInit
		Return inst
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Local d:TArgDecl = New TArgDecl.Create( ident,declTy,declInit,attrs,generated,volatile )
		d.ty = ty
		d.init = init
		Return d
	End Method

	Method GetDeclPrefix:String()
		Return ""
	End Method

	Method OnSemant()
		Super.OnSemant()
		If ty Then
			ty = ty.Semant()
		End If
		
		If attrs & DECL_STATIC Then
			If Not TArrayType(ty) Then
				Err "Expecting array"
			End If
			
			Local et:TType = TArrayType(ty).elemType
			If Not TNumericType(et) And Not (TObjectType(et) And TObjectType(et).classDecl.IsStruct()) Then
				Err "Static array elements must be numeric or a Struct"
			End If
		End If
		
		If init And Not TConstExpr(init) Then
			If TCastExpr(init) Then
				If TConstExpr(TCastExpr(init).expr) Or TNullExpr(TCastExpr(init).expr) Then
					Return
				End If
			End If
			If TInvokeExpr(init) And TFunctionPtrType(TInvokeExpr(init).exprType) Then
				Return
			End If
			If TIdentEnumExpr(init) Then
				Return
			End If
			Err "Function defaults must be constant"
		End If
	End Method

	Method ToString$()
		Return Super.ToString()
	End Method
	
End Type

Type TGlobalDecl Extends TVarDecl

	Field inited:Int
	Field funcGlobal:Int
	Field mscope:TScopeDecl
	
	Method Create:TGlobalDecl( ident$,ty:TType,init:TExpr,attrs:Long=0,funcGlobal:Int=False )
		Self.deferInit = True
		Self.ident=ident
		Self.declTy=ty
		Self.declInit=init
		Self.attrs=attrs
		Self.funcGlobal=funcGlobal
		Return Self
	End Method

	Method OnCopy:TDecl(deep:Int = True)
		Local g:TGlobalDecl = New TGlobalDecl.Create( ident,declTy,declInit,attrs,funcGlobal )
		g.ty = ty
		g.init = init
		g.mscope = mscope
		Return g
	End Method
	
	Method ToString$()
		Return "Global "+Super.ToString()
	End Method

	Method GenInstance:TDecl()
'		PushErr errInfo
'		Err "Global variables cannot be used inside generic classes."
		Local inst:TGlobalDecl=New TGlobalDecl
		InitInstance inst
		inst.declTy=declTy
		inst.declInit=declInit
		Return inst
	End Method

	Method CheckAccess:Int()
		Local cd:TClassDecl = ClassScope()
		If cd Then
			If cd.modulescope() = _env.modulescope() Return True
			If IsPrivate() And cd<>_env.ClassScope() Return False
			If IsProtected() Then
				Local ec:TClassDecl = _env.ClassScope()
				If Not ec Return False
				If Not ec.ExtendsClass(cd) Return False
			End If
			Return True
		End If
		Return Super.CheckAccess()
	End Method

End Type

Type TFieldDecl Extends TVarDecl

	' location offset in object variable data
	Field offset:Int

	Method Create:TFieldDecl( ident$,ty:TType,init:TExpr,attrs:Long=0 )
		Self.ident=ident
		Self.declTy=ty
		Self.declInit=init
		Self.attrs=attrs
		Return Self
	End Method

	Method OnCopy:TDecl(deep:Int = True)
		Local f:TFieldDecl = New TFieldDecl.Create( ident,declTy,declInit,attrs )
		f.ty = ty
		f.init = init
		f.metadata = metadata
		Return f
	End Method
	
	Method ToString$()
		Return "Field "+Super.ToString()
	End Method
	
	Method GenInstance:TDecl()
		Local inst:TFieldDecl=New TFieldDecl
		InitInstance inst
		inst.declTy=declTy
		inst.declInit=declInit
		Return inst
	End Method

	Method CheckAccess:Int()

		If ModuleScope() = _env.ModuleScope() Then
			Return True
		End If

		Local cs:TClassDecl = ClassScope()

		If IsPrivate() And cs Then
			Local ec:TClassDecl = _env.ClassScope()

			While ec

				If cs = ec Then
					Return True
				End If
				
				ec = ec.scope.ClassScope()
			Wend
			
			If Not ec Then
				Return False
			End If
		End If
		If IsProtected() And cs Then
			Local ec:TClassDecl = _env.ClassScope()
			
			While ec
				If ec.ExtendsClass(cs) Then
					Return True
				End If
				
				ec = ec.scope.ClassScope()
			Wend
			
			If Not ec Then
				Return False
			End If
		End If
		Return True
	End Method
	
	Method OnSemant()
		Super.OnSemant()
		
		If TObjectType(ty) And TObjectType(ty).classDecl.IsStruct() Then
			TObjectType(ty).classDecl.exposed = True
		End If
	End Method

End Type

Type TAliasDecl Extends TDecl

	Field decl:Object
	
	Method Create:TAliasDecl( ident$,decl:Object,attrs:Long=0 )
		Self.ident=ident
		Self.decl=decl
		Self.attrs=attrs
		Return Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Return New TAliasDecl.Create( ident,decl,attrs )
	End Method
	
	Method OnSemant()
	End Method
	
	Method Clear()
	End Method
	
End Type

Type TScopeDecl Extends TDecl

'Private

	Field _decls:TBCCObjectList=New TBCCObjectList'<TDecl>
	Field _semanted:TBCCObjectList=New TBCCObjectList'<TDecl>

	Field declsMap:TMap=New TMap'<Object>

'Public

	Method OnCopy:TDecl(deep:Int = True)
		InternalErr "TScopeDecl.OnCopy"
	End Method

	Method Decls:TBCCObjectList()
		Return _decls
	End Method
	
	Method Semanted:TBCCObjectList()
		Return _semanted
	End Method
	
	Method FuncDecls:TBCCObjectList( id$="" )
		Local fdecls:TBCCObjectList=New TBCCObjectList
		For Local decl:TDecl=EachIn _decls
			If id And decl.ident<>id Continue
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl fdecls.AddLast fdecl
		Next
		Return fdecls
	End Method
	
	Method MethodDecls:TBCCObjectList( id$="" )
		Local fdecls:TBCCObjectList=New TBCCObjectList
		For Local decl:TDecl=EachIn _decls
			If id And decl.ident<>id Continue
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl And fdecl.IsMethod() fdecls.AddLast fdecl
		Next
		Return fdecls
	End Method
	
	Method SemantedFuncs:TBCCObjectList( id$="" )
		Local fdecls:TBCCObjectList=New TBCCObjectList
		For Local decl:TDecl=EachIn _semanted
			If id And decl.ident<>id Continue
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl fdecls.AddLast fdecl
		Next
		Return fdecls
	End Method
	
	Method SemantedMethods:TBCCObjectList( id$="" )
		Local fdecls:TBCCObjectList=New TBCCObjectList
		For Local decl:TDecl=EachIn _decls
			If id And decl.ident<>id Continue
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl And fdecl.IsMethod()
				If Not fdecl.IsSemanted() Then
					fdecl.Semant()
				End If
				fdecls.AddLast fdecl
			End If
		Next
		Return fdecls
	End Method
	
	Method InsertDecl( decl:TDecl, isCopy:Int = False )

		If decl.scope And Not (decl.attrs & DECL_INITONLY) And Not isCopy InternalErr "TScopeDecl.InsertDecl"
		
		'Local ident$=decl.ident
		If Not decl.ident Return
		
		If Not decl.scope Or isCopy Then
			decl.scope=Self
		End If
		_decls.AddLast decl

		'Local _decls:TMap
		Local tdecl_:Object=declsMap.ValueForKey( decl.IdentLower() )
		
		If TFuncDecl( decl )
			Local funcs:TFuncDeclList=TFuncDeclList( tdecl_ )
			If funcs Or Not tdecl_
				If Not funcs
					funcs=New TFuncDeclList
					funcs.ident = decl.IdentLower()
					declsMap.Insert decl.IdentLower(),funcs
				EndIf

				funcs.AddLast TFuncDecl( decl )
				Return
			Else
				Err "Duplicate identifier '"+decl.ident+"'."
			EndIf
		Else If Not tdecl_
			declsMap.Insert decl.IdentLower(),decl
		Else
			Err "Duplicate identifier '"+decl.ident+"'."
		EndIf

	End Method

	Method InsertDecls( _decls:TBCCObjectList )
		For Local decl:TDecl=EachIn _decls
			InsertDecl decl
		Next
	End Method
	
	'This is overridden by TClassDecl and TModuleDecl
	Method GetDecl:Object( ident$ )
'DebugLog "GetDecl (" + Self.ident + ") : " + ident
		Local decl:Object=Object(declsMap.ValueForKey( ident ))

		If Not decl Then
			If Self.IdentLower() = ident Then
				' name matches but we are a "module", but not a *real* module..
				' .. so we can't be looking for ourself
				If TModuleDecl(Self) And Self.ident.Find(".") = - 1 Then
					decl = Null
				Else
					decl = Self
				End If
			End If
		End If
		
		If Not decl Return Null
		
		Local adecl:TAliasDecl=TAliasDecl( decl )
		If Not adecl Return decl
		
		If adecl.CheckAccess() Return adecl.decl
	End Method
	

	Method FindDecl:Object( ident$, _override:Int = False )
	
		If Not _override And _env<>Self Return GetDecl( ident )
		
		Local tscope:TScopeDecl=Self
		While tscope
			Local decl:Object=tscope.GetDecl( ident )
			If decl Return decl
			tscope=tscope.scope
		Wend
	End Method
	
	Method GetDeclList:Object( ident$, declList:TFuncDeclList = Null, maxSearchDepth:Int )

		If Not declList Then
			declList = New TFuncDeclList
		End If

		Local decl:Object=Object(declsMap.ValueForKey( ident ))

		If Not decl Return Null

		If TFuncDeclList(decl) Then
			For Local fdecl:TFuncDecl = EachIn TFuncDeclList(decl)

				If Not fdecl.IsSemanted() And Not fdecl.IsSemanting() Then
					fdecl.Semant
				End If
				
				Local found:Int
				' remove matching functions from decl list.
				' a match should match exactly. If an arg is a subclass of another
				' then that is not a match, and we will distance test later..
				For Local func:TFuncDecl = EachIn declList
					If func.equalsFunc(fdecl, True) Then
						found = True
						Exit
'Else
'Print func.ToString() + "  didn't match  " + fdecl.ToString()
					End If
				Next
				
				If Not found Then
					declList.AddLast(fdecl)
				End If
			Next
			
			Return declList
		End If		
		
		Return decl
		
	End Method
	
	' returns a list of all matching named decls in scope
	Method FindDeclList:Object(ident:String, _override:Int = False, declList:TFuncDeclList = Null, maxSearchDepth:Int = SCOPE_ALL, skipMultipleClassScopes:Int = False )

		If Not declList Then
			declList = New TFuncDeclList
		End If
	
		If Not _override And _env<>Self Return GetDeclList( ident, declList, maxSearchDepth )
		
		Local hadClassScope:Int
		Local tscope:TScopeDecl=Self
		While tscope
			If TClassDecl(tscope) Then
			
				If skipMultipleClassScopes And hadClassScope Then
					tscope=tscope.scope
					Continue
				End If
			
				hadClassScope = True
			End If
		
			Local decl:Object=tscope.GetDeclList( ident, declList, maxSearchDepth )
			'If decl And (Not TFuncDeclList(decl) And declList.IsEmpty()) Return decl
			If decl Then
				If TFuncDeclList(decl) Then
					If TFuncDeclList(decl) <> declList Then
						For Local d:TDecl = EachIn TFuncDeclList(decl)
							declList.AddLast(d)
						Next
					End If
				Else
					declList.AddLast(decl)
				End If
			End If

			' if scope is an interface, also check implemented/extended interfaces?
			If TClassDecl(tscope) Then'And TClassDecl(tscope).IsInterface() Then
				If TClassDecl(tscope).implments Then
					For Local idecl:TScopeDecl = EachIn TClassDecl(tscope).implments
						Local decl:Object=idecl.GetDeclList( ident, declList, maxSearchDepth )
						If decl Then
							If TFuncDeclList(decl) Then
								If TFuncDeclList(decl) <> declList Then
									For Local d:TDecl = EachIn TFuncDeclList(decl)
										declList.AddLast(d)
									Next
								End If
							Else
								declList.AddLast(decl)
							End If
						End If
					Next
				End If 
			End If
			
			tscope=tscope.scope
			
			If TClassDecl(tscope) And maxSearchDepth < SCOPE_CLASS_HEIRARCHY Then
				Exit
			Else If TModuleDecl(tscope) And maxSearchDepth < SCOPE_ALL Then
				Exit
			End If
		Wend
		
		Return declList
	End Method

	

'	Method FindDecl:Object( ident$, static:Int = False )
'		Local decl:Object=GetDecl( ident )
'		
'		If Not static Or Not decl Then
'			If decl Return decl
'		Else
'			If Not TFieldDecl(decl) And Not (TFuncDecl(decl) And TFuncDecl(decl).IsMethod()) Then
'				Return decl
'			End If
'		End If
'		If scope Return scope.FindDecl( ident, static )
'	End Method
	
	Method FindValDecl:TValDecl( ident$, static:Int = False )
		Local decl:TValDecl=TValDecl( FindDecl( ident ) )
		
		' we found a field but we don't have access to it?
		If TFieldDecl(decl) And static Then
			' see if there's another decl with the same name elsewhere that we may...
			' field's scope.scope will be a module.
			If decl.scope And decl.scope.scope Then
				Local vDecl:TValDecl = TValDecl( decl.scope.scope.FindDecl( ident, True ) )
				If vDecl Then
					decl = vDecl
				End If
			End If
		End If
		
		' if scope isn't static, and we didn't find it yet, look no further
		' otherwise, look harder...
		If Not decl And static Then
			' try scope search
			decl = TValDecl( FindDecl( ident, True ) )
			
			If Not decl Then
				' didn't find it? Maybe it is in module local scope?
				' issue arises when a global initialises with a local variable in the module scope.
				Local fdecl:Object = FindDecl("__localmain", True)
				If fdecl Then
					If TFuncDecl(fdecl) Then
						decl = TValDecl( TFuncDecl(fdecl).FindDecl( ident ) )
					Else If TFuncDeclList(fdecl) Then
						For Local func:TFuncDecl = EachIn TFuncDeclList(fdecl)
							func.Semant()
							decl = TValDecl( func.FindDecl( ident ) )
							If decl Then
								Exit
							End If
						Next
					End If
					
					' a local variable from module local scope can't be seen outside of module local scope...
					If TLocalDecl(decl) And static Then
						decl = Null
					End If
				End If
			End If
		End If
		If Not decl Return Null
		decl.AssertAccess
		decl.Semant
		Return decl
	End Method

	Method FindType:TType( ident$,args:TType[], callback:TCallback = Null )
'DebugLog Self.ident + "::FindType::" + ident
		Local decl:Object=(GetDecl( ident ))
		If decl Then
			If TModuleDecl(decl) Then
				decl = TModuleDecl(decl).GetDecl(ident)
			End If
			Local ty:TType=TType(decl)
			If ty
				If args.Length Err "Wrong number of type arguments"
				Return ty
			EndIf
			Local cdecl:TClassDecl=TClassDecl( decl )
			If cdecl
				cdecl.AssertAccess
				If Not cdecl.instanceof Then
					cdecl=cdecl.GenClassInstance( args, False, callback, Null )
					cdecl.Semant
				End If
				Return cdecl.objectType
			EndIf
			Local edecl:TEnumDecl = TEnumDecl(decl)
			If edecl Then
				Return New TEnumType.Create(edecl)
			End If
		EndIf
		If scope Return scope.FindType( ident,args, callback )
	End Method
	
	Method FindScopeDecl:TScopeDecl( ident$ )
		Local decl:TScopeDecl=TScopeDecl( FindDecl( ident ) )
		If Not decl Return Null
		decl.AssertAccess
		decl.Semant
		Return decl
	End Method
Rem	
	Method FindClassDecl:TClassDecl( ident$,args:TClassDecl[] = Null )
		Local decl:TClassDecl=TClassDecl( GetDecl( ident ) )
		If Not args Then
			args = New TClassDecl[0]
		End If
		If Not decl
			If scope Return scope.FindClassDecl( ident,args )
			Return Null
		EndIf
		decl.AssertAccess
		decl.Semant
		Return decl.GenClassInstance( args )
	End Method
End Rem	
	Method FindModuleDecl:TModuleDecl( ident$ )
'DebugStop
		Local decl:TModuleDecl=TModuleDecl( GetDecl( ident ) )
		If Not decl
			If scope Return scope.FindModuleDecl( ident )
			Return Null
		EndIf
		decl.AssertAccess
		
		' only semant on "real" module
		If Not decl.IsImported() Then
			decl.Semant
		End If
		Return decl
	End Method
	
	Method FindBestMatchForArgs:TFuncDecl(argExprs:TExpr[], matches:TBCCObjectList)

		Local bestMatch:TFuncDecl = Null
		Local totals:Int[] = New Int[matches.count()]
		Local index:Int
		
		For Local func:TFuncDecl = EachIn matches

			Local argDecls:TArgDecl[]=func.argDecls
			
			For Local i:Int=0 Until argDecls.Length
	
				If i<argExprs.Length And argExprs[i]
				
					Local declTy:TType=argDecls[i].ty
					Local exprTy:TType=argExprs[i].exprType

					If TFunctionPtrType(declTy) And TInvokeExpr(argExprs[i]) Then
						If TFunctionPtrType(declTy).equalsDecl(TInvokeExpr(argExprs[i]).decl) Then
							Continue
						End If
					End If

					' not ideal - since the arg is configured as a Byte Ptr, we can't check that the function is of the correct type.
					If IsPointerType(declTy, TType.T_BYTE) And TInvokeExpr(argExprs[i]) And TInvokeExpr(argExprs[i]).invokedWithBraces = 0 Then
						Continue
					End If
					
					If TFunctionPtrType(declTy) And IsPointerType(exprTy, TType.T_BYTE) Then
						Continue
					End If
					
					If exprTy.EqualsType( declTy ) Continue
					
					' not an exact match. increase distance...
					totals[index] :+ exprTy.DistanceToType(declTy)
					
				End If
				
			Next
			
			index :+ 1

		Next
		
		Local tot:Int = -1
		index = 0
		Local i:Int
		Local minArgs:Int
		For Local func:TFuncDecl = EachIn matches
			If tot = -1 Or totals[i] < tot Then
				tot = totals[i]
				bestMatch = func
				minArgs = func.argDecls.Length
			Else If tot = totals[i] Then
				If bestMatch.IsMethod() And Not func.IsMethod() Then
					' 
				Else If Not bestMatch.IsMethod() And func.IsMethod() Then
					bestMatch = func
				Else If (bestMatch.scope <> func.scope) And (TClassDecl(bestMatch.scope).ExtendsClass(TClassDecl(func.scope))) Then
					' match is in different level of class hierarchy
					Exit
				Else If func.generated <> bestMatch.generated Then
					If Not func.generated Then
						bestMatch = func
					End If
				Else If minArgs <> func.argDecls.Length
					If minArgs > func.argDecls.Length Then
						bestMatch = func
						minArgs = func.argDecls.Length
					End If
				Else
					' a tie?
					Err "Unable to determine overload to use: "+ bestMatch.ToString()+" or "+func.ToString()+"."
				End If
			End If
			i :+ 1
		Next
		
		Return bestMatch
		
	End Method
	
	Method FindFuncDecl:TFuncDecl( ident$,argExprs:TExpr[] = Null,explicit:Int=False, isArg:Int = False, isIdentExpr:Int = False, throwOnNotMatched:Int = False, maxSearchDepth:Int )
'DebugLog "FindFuncDecl : " + ident
'If ident = "new" Then DebugStop
		Local foundIdentMatch:Int
		Local funcs:TFuncDeclList

		' does ident exist?
		Local f:Object = FindDeclList(ident, True,,maxSearchDepth)
		If Not f Then Return Null
		
		funcs = TFuncDeclList( f )
		Local fp:TFuncDecl
		
		' not a function list, test for a function ptr var
		If Not funcs Or funcs.IsEmpty() Then

			' we found a funcdecl
			If TFuncDecl(f) Then
				funcs = New TFuncDeclList
				funcs.AddLast(f)
			End If
			
			If TVarDecl(f) Then
				If Not TVarDecl(f).IsSemanted() Then
					TVarDecl(f).Semant()
				End If
				If TFunctionPtrType(TVarDecl(f).ty) Then
					funcs = New TFuncDeclList
					fp = TFunctionPtrType(TVarDecl(f).ty).func
					If Not fp.scope Then
						fp.scope = TVarDecl(f).scope
					End If
					If Not fp.ident Then
						fp.ident = TVarDecl(f).ident
					End If
					funcs.AddLast fp
				End If
			End If
		End If
		' was neither... lets bug out
		If Not funcs Return Null
		
'		If Not funcs Then Return Null
		
		For Local func:TDecl = EachIn funcs
			If Not func.IsSemanting() Then
				func.Semant()
			End If
		Next
		
		'Local f:TDecl = TDecl(findDecl(ident))
		'If Not f Then Return Null
		
				
			'Local func:TFuncDecl = TFuncDecl(f)
'			If Not func Then
'				If TVarDecl(f) Then
'					If Not f.IsSemanted() Then
'						f.Semant()
'					End If
'					If TFunctionPtrType(TVarDecl(f).ty) Then
'						func = TFunctionPtrType(TVarDecl(f).ty).func
'						If Not func.scope Then
'							func.scope = f.scope
'						End If
'						If Not func.ident Then
'							func.ident = f.ident
'						End If
'					End If
'				End If
'			End If
'			If Not func Return Null
	
		If Not argExprs
			argExprs = New TExpr[0]
		End If
	
		'func.Semant()
		
		Local match:TFuncDecl,isexact:Int
		Local _err$
		Local errorDetails:String
		Local matches:TBCCObjectList = New TBCCObjectList

		Local noExtendString:Int = True
		Local generateWarnings:Int = False

		' double test for matches.
		' * first time through we don't allow up-casting args to String
		'    if we get a match on the first pass, we'll take it.
		' * second iteration we allow up-casting numerics to string
		' * third iteration is valid if opt_warnover is enabled
		'    this will allow down-casting of numerics (eg. double->float)
		'    warnings will be generated if this produces valid results.
		' if after all that, there's no match, then we can fail it.
		For Local n:Int = 0 Until 3
		
			If n > 1 Then
				If Not opt_warnover Then
					Continue
				Else
					generateWarnings = True
				End If
			End If
			
			errorDetails = ""
			
			If n Then
				noExtendString = False
			End If
			
			For Local iDecl:TDecl = EachIn funcs
			
				Local func:TFuncDecl = TFuncDecl(iDecl)
				
				If Not func Then
					If TVarDecl(iDecl) Then
'						If Not TVarDecl(iDecl).IsSemanted() Then
'							TVarDecl(f).Semant()
'						End If
						If TFunctionPtrType(TVarDecl(iDecl).ty) Then
							'funcs = New TFuncDeclList
							fp = TFunctionPtrType(TVarDecl(iDecl).ty).func
							If Not fp.scope Then
								fp.scope = TVarDecl(iDecl).scope
							End If
							If Not fp.ident Then
								fp.ident = TVarDecl(iDecl).ident
							End If
							'funcs.AddLast fp
							func = fp
						Else
							Err "Expression of type '" + TVarDecl(iDecl).ty.ToString() + "' cannot be invoked."
						End If
					End If
					
					If Not func Then
						Continue
					End If
				End If
				
	
			'While True
				If Not func.CheckAccess() Continue
				
				Local argDecls:TArgDecl[]=func.argDecls
				
				Local exact:Int=True
				Local possible:Int=True
				
				foundIdentMatch = True

				' we found a matching name - this is probably the one we mean...
				If isArg Then
					'match=func
					matches.AddLast(func)
					Exit
				End If

				If argExprs.Length>argDecls.Length
					exact = False
					Continue
				End If

				For Local i:Int=0 Until argDecls.Length
	
					If i<argExprs.Length And argExprs[i]
					
						' ensure arg is semanted
						Local arg:TExpr = argExprs[i].Semant()
					
						Local declTy:TType=argDecls[i].ty
						Local exprTy:TType=arg.exprType
						
						If Not exprTy Then
							InternalErr "TScopeDecl.FindFuncDecl"
						End If
						
						Local widensTest:Int = True
											 
						' for numeric constants, allow them to be auto-cast unless 
						If TConstExpr(arg) And IsNumericType(exprTy) And Not TConstExpr(arg).typeSpecific And TConstExpr(arg).CompatibleWithType(declTy) Then 
							widensTest = False 
						End If 
	 
						If TFunctionPtrType(declTy) And TInvokeExpr(arg) Then
							If TFunctionPtrType(declTy).equalsDecl(TInvokeExpr(arg).decl) Continue
						End If
	
						' not ideal - since the arg is configured as a Byte Ptr, we can't check that the function is of the correct type.
						If IsPointerType(declTy, TType.T_BYTE) And TInvokeExpr(arg) And TInvokeExpr(arg).invokedWithBraces = 0 Then
							Continue
						End If
						
						If TFunctionPtrType(declTy) And IsPointerType(exprTy, TType.T_BYTE) Then
							Continue
						End If

						' Give more detailed message for some const use cases. 
						If TConstExpr(arg) And IsNumericType(exprTy) And IsNumericType(declTy) And Not TConstExpr(arg).typeSpecific Then
						 	If Not TConstExpr(arg).CompatibleWithType(declTy) Then
						 		errorDetails :+ "~nArgument #"+(i+1)+" is not implicitly compatible with declared type ~q" + declTy.ToString() + "~q. Consider casting or explicitly typing it with ~q:" + declTy.ToString() + "~q if you want to use it as is."
						 	End If
						End If
						
						If exprTy.EqualsType( declTy ) Continue
						
						exact=False
						
						If Not generateWarnings Then
							If Not explicit And exprTy.ExtendsType( declTy, noExtendString, widensTest ) Continue
						Else
							If Not explicit Then
								' fails widen test
								If Not exprTy.ExtendsType( declTy, noExtendString, True ) Then
									' but passes non-widen test
									If exprTy.ExtendsType( declTy, noExtendString, False ) Then
										' generate a warning, and accept it
										Warn "In call to " + func.ToString()+ ": Argument #"+(i+1)+" is ~q" + exprTy.ToString()+"~q but declaration is ~q"+declTy.ToString()+"~q. "
										Continue
									End If
								Else
									Continue
								End If
							End If
						End If
	
						' make a more helpful error message
						errorDetails :+ "~nArgument #"+(i+1)+" is ~q" + exprTy.ToString() + "~q but declaration is ~q" + declTy.ToString() + "~q."

					Else If Not argDecls[i].init
	
						If (func.attrs & FUNC_PTR) Or isIdentExpr Then
							exact=False
							Exit
						End If
	
						' if this argument is missing and there isn't a default...
						errorDetails :+  "Missing function parameter '" + argDecls[i].ident + "'"
	
					Else ' for case of argdecls having default args
						exact=False
						Continue ' carry on to the next arg
					EndIf
				
					possible=False
					Exit
				Next
				
				If Not possible Continue
				
				If exact
					If isexact
						'Err "Unable to determine overload to use: "+match.ToString()+" or "+func.ToString()+"."
					Else
						_err=""
						match=func
						matches.AddLast(func)
						isexact=True
						'Exit
					EndIf
				Else
					'If Not isexact
						'If match 
						'	_err="Unable to determine overload to use: "+match.ToString()+" or "+func.ToString()+"."
						'Else
							'match=func
							matches.AddLast(func)
						'EndIf
					'EndIf
				EndIf
				'Exit
			Next
			
			If Not matches.IsEmpty() Then
				Exit
			End If
			
		Next
		
		If matches.Count() = 1 Then
			match = TFuncDecl(matches.First())
		Else
			' find best match
			match = FindBestMatchForArgs(argExprs, matches)
		End If
		
		If Not isexact
			If _err Err _err
			If explicit Return Null
		EndIf

		' last try... maybe we are trying to use it as a function pointer? (no args)
		If Not match Then
			If argExprs Then
				'match = func
'				match.maybeFunctionPtr = True
			End If
		Else If Not argExprs Then
			' if there are no args, the actual function may have none either... so we may still be trying to use it as a function pointer
			match.maybeFunctionPtr = True
		End If
		
		If Not match
			Local t$
			For Local i:Int=0 Until argExprs.Length
				If t t:+", "
				If argExprs[i] t:+argExprs[i].exprType.ToString()
			Next
			If foundIdentMatch Then
				If throwOnNotMatched Then
					Throw "Unable to find overload for "+ident+"("+t+")." + errorDetails
				Else
					Err "Unable to find overload for "+ident+"("+t+")." + errorDetails
				End If
			Else
				If throwOnNotMatched Then
					Throw "Identifier '" + ident + "' not found."
				Else
					Err "Identifier '" + ident + "' not found."
				End If
			End If
		EndIf
		
		match.AssertAccess

		' ensure any const arg expressions are converted to the correct type once we've found a match
		Local argDecls:TArgDecl[]=match.argDecls
		For Local i:Int=0 Until argExprs.Length
			Local arg:TExpr = argExprs[i]
			If arg Then
				Local declTy:TType=argDecls[i].ty
				Local exprTy:TType=arg.exprType

				If TConstExpr(arg) And IsNumericType(exprTy) And IsNumericType(declTy) And Not TConstExpr(arg).typeSpecific And TConstExpr(arg).CompatibleWithType(declTy) Then
					arg.exprType = declTy
				End If
			End If
		Next

		Return match
	End Method

	Method FindLoop:TStmt(ident:String = Null)

		If TBlockDecl(Self) And TBlockDecl(Self).extra Then
			Local loop:TLoopStmt = TLoopStmt(TBlockDecl(Self).extra)
			If ident Then
				If loop.loopLabel And loop.loopLabel.IdentLower() = ident Then
					Return loop
				End If
			Else
				Return loop
			End If
		End If

		If TFuncDecl(scope) Or TModuleDecl(scope)
			Return Null
		End If
		
		If scope Return scope.FindLoop( ident )
	End Method

	Method FindTry:TTryStmtDecl()

		If TTryStmtDecl(Self) Then
			Return TTryStmtDecl(Self)
		End If

		If TFuncDecl(scope) Or TModuleDecl(scope)
			Return Null
		End If
		
		If scope Return scope.FindTry()
	End Method

	Method OnSemant()
	End Method
	
	Method Clear()
	End Method

End Type

Type TBlockDecl Extends TScopeDecl
	Field stmts:TBCCObjectList=New TBCCObjectList
	Field extra:Object
	Field blockType:Int
	
	Method Create:TBlockDecl( scope:TScopeDecl, generated:Int = False, blockType:Int = BLOCK_OTHER )
		Self.scope = scope
		Self.generated = generated
		Self.blockType = blockType
		
		attrs :| (scope.attrs & DECL_NODEBUG)
		
		Return Self
	End Method
	
	Method AddStmt( stmt:TStmt )
		stmts.AddLast stmt
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Local t:TBlockDecl=New TBlockDecl
		t.scope = scope
		If deep Then
			For Local stmt:TStmt=EachIn stmts
				t.AddStmt stmt.Copy( t )
			Next
		End If
		t.extra = extra
		t.generated = generated
		t.blockType = blockType
		Return t
	End Method

	Method OnSemant()
		PushEnv Self
		
		' any nested functions?
		For Local fdecl:TFuncDecl = EachIn _decls
			fdecl.Semant
		Next

		' any nested classes?
		For Local cdecl:TClassDecl = EachIn _decls
			cdecl.Semant
		Next
		
		For Local stmt:TStmt=EachIn stmts
			stmt.Semant

			If opt_debug And Not IsNoDebug() Then
				If Not stmt.generated Then
					GenHash(stmt.errInfo[1..].Split(";")[0])
				End If
			End If
			
			If TReturnStmt(stmt) Then
				If SurroundingFinallyBlock(Self) Then PushErr stmt.errInfo; Err "Return cannot be used inside a Finally block."
			Else If TBreakStmt(stmt) Then
				Local loop:TLoopStmt
				If TLoopLabelExpr(TBreakStmt(stmt).label) Then
					loop = TLoopLabelExpr(TBreakStmt(stmt).label).loop
				Else
					loop = TLoopStmt(Self.FindLoop())
				End If
				Local f:TBlockDecl = SurroundingFinallyBlock(Self)
				If f And f <> SurroundingFinallyBlock(loop.block) Then PushErr stmt.errInfo; Err "Exit cannot be used to leave a Finally block."
			Else If TContinueStmt(stmt) Then
				Local loop:TLoopStmt
				If TLoopLabelExpr(TContinueStmt(stmt).label) Then
					loop = TLoopLabelExpr(TContinueStmt(stmt).label).loop
				Else
					loop = TLoopStmt(Self.FindLoop())
				End If
				Local f:TBlockDecl = SurroundingFinallyBlock(Self)
				If f And f <> SurroundingFinallyBlock(loop.block) Then PushErr stmt.errInfo; Err "Continue cannot be used to leave a Finally block."
			End If
			
			Function SurroundingFinallyBlock:TBlockDecl(block:TBlockDecl)
				' get the innermost Finally block surrounding the current statement
				While block And Not TFuncDecl(block)
					If block.blockType = BLOCK_FINALLY Then Return block
					block = TBlockDecl(block.scope)
				Wend
				Return Null
			End Function
		Next
		PopEnv
	End Method

	Method CopyBlock:TBlockDecl( scope:TScopeDecl )
		Local t:TBlockDecl=TBlockDecl( Copy() )
		t.scope=scope
		Return t
	End Method

	Method Clear()
		For Local stmt:TStmt=EachIn stmts
			stmt.Clear
		Next
	End Method

	Method ToString:String()
		Select blockType
			Case BLOCK_FUNCTION
				Return Super.ToString()
			Case BLOCK_OTHER
				Return "TBlockDecl:Other"
			Case BLOCK_LOOP
				Return "TBlockDecl:Loop"
			Case BLOCK_TRY
				Return "TBlockDecl:Try"
			Case BLOCK_CATCH
				Return "TBlockDecl:Catch"
			Case BLOCK_FINALLY
				Return "TBlockDecl:Finally"
			Case BLOCK_IF
				Return "TBlockDecl:If"
			Case BLOCK_ELSE
				Return "TBlockDecl:Else"
			Default
				Return "TBlockDecl:Unknown"
		End Select
	End Method
End Type

Const FUNC_METHOD:Long=   $0001			'mutually exclusive with ctor
Const FUNC_CTOR:Long=     $0002
Const FUNC_PROPERTY:Long= $0004
Const FUNC_DTOR:Long=     $0008
Const FUNC_BUILTIN:Long = $0080
Const FUNC_PTR:Long=      $0100
Const FUNC_INIT:Long =    $0200
Const FUNC_NESTED:Long =  $0400
Const FUNC_OPERATOR:Long= $0800
Const FUNC_FIELD:Long=    $1000
' ^ beware of collisions between these constants and the ones declared at the top of this file


'Fix! A func is NOT a block/scope!
'
Type TFuncDecl Extends TBlockDecl

	Field retType:TType
	Field retTypeExpr:TType
	Field argDecls:TArgDecl[]

	Field overrides:TFuncDecl
	Field superCtor:TInvokeSuperExpr
	
	Field castTo:String
	Field noCastGen:Int
	Field cdets:TCastDets
	
	Field maybeFunctionPtr:Int
	
	Field returnTypeSubclassed:Int
	
	Field mangled:String
	Field noMangle:Int
	Field exported:Int
	
	Field equalsBuiltIn:Int = -1
	field idx:Int
	
	Method CreateF:TFuncDecl( ident$,ty:TType,argDecls:TArgDecl[],attrs:Long )
		Self.ident=ident
		Self.retTypeExpr=ty
		If argDecls
			Self.argDecls=argDecls
		Else
			Self.argDecls = New TArgDecl[0]
		End If
		Self.attrs=attrs
		Self.blockType = BLOCK_FUNCTION
		Return Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Local args:TArgDecl[]=argDecls[..]
		For Local i:Int=0 Until args.Length
			args[i]=TArgDecl( args[i].Copy() )
		Next
		Local t:TFuncDecl=New TFuncDecl.CreateF( ident,retType,args,attrs)
		If deep Then
			For Local stmt:TStmt=EachIn stmts
				t.AddStmt stmt.Copy( t )
			Next
		End If
		t.retType = retType
		t.retTypeExpr = retTypeExpr
		t.scope = scope
		t.overrides = overrides
		t.superCtor = superCtor
		t.castTo = castTo
		t.noCastGen = noCastGen
		t.munged = munged
		t.metadata = metadata
		t.mangled = mangled
		t.noMangle = noMangle
		t.exported = exported
		t.blockType = blockType
		Return  t
	End Method

	Method GenInstance:TDecl()
		Local inst:TFuncDecl=New TFuncDecl
		InitInstance inst
		inst.retTypeExpr=retTypeExpr
		inst.argDecls=argDecls[..]
		For Local i:Int=0 Until argDecls.Length
			inst.argDecls[i]=TArgDecl( argDecls[i].GenInstance() )
		Next
		Return inst
	End Method
	
	Method ToString$()
		Local t$
		For Local decl:TArgDecl=EachIn argDecls
			If t t:+","
			t:+decl.ToString()
		Next
		Local q$
		If IsCtor()
			q="Method "+Super.ToString()
		Else
			If IsMethod() q="Method " Else q="Function "
			q:+Super.ToString()
			If retType
				If Not TVoidType(retType) Then
					q:+":"+retType.ToString()
				End If
			Else If retTypeExpr 
				q:+":"+retTypeExpr.ToString()
			'Else
			'	q:+":"+"?"
			EndIf
		EndIf
		Return q+"("+t+")"
	End Method

	Method ToTypeString:String()
		Local t$
		For Local decl:TArgDecl=EachIn argDecls
			If t t:+","
			t:+decl.ToTypeString()
		Next
		Local q$
		If Not IsCtor() Then
			If retType
				If Not TVoidType(retType) Then
					q:+retType.ToString()
				End If
			Else If retTypeExpr 
				q:+retTypeExpr.ToString()
			EndIf
		End If
		Return q+"("+t+")"
	End Method

	Method IsBuiltIn:Int()
		Return (attrs & FUNC_BUILTIN)<>0
	End Method
	
	Method IsCtor:Int()
		Return (attrs & FUNC_CTOR)<>0
	End Method

	Method IsDtor:Int()
		Return (attrs & FUNC_DTOR)<>0
	End Method

	Method IsMethod:Int()
		Return (attrs & FUNC_METHOD)<>0
	End Method
	
	Method IsAnyMethod:Int()
		Return IsMethod() Or IsCtor() Or IsDtor() 
	End Method
	
	Method IsStatic:Int()
		Return (attrs & (FUNC_METHOD|FUNC_CTOR))=0
	End Method
	
	Method IsProperty:Int()
		Return (attrs & FUNC_PROPERTY)<>0
	End Method

	Method IsField:Int()
		Return (attrs & FUNC_FIELD)<>0
	End Method
	
	' exactMatch requires args to be equal. If an arg is a subclass, that is not a match.
	Method EqualsArgs:Int( decl:TFuncDecl, exactMatch:Int = False, ignoreObjectSubclasses:Int = False ) ' careful, this is not commutative!
		If argDecls.Length<>decl.argDecls.Length Return False
		For Local i:Int=0 Until argDecls.Length
			' ensure arg decls have been semanted
			decl.argDecls[i].scope = decl
			decl.argDecls[i].attrs :| DECL_INITONLY
			decl.argDecls[i].Semant()

			argDecls[i].scope = Self
			argDecls[i].attrs :| DECL_INITONLY
			argDecls[i].Semant()
			
			' objects can be subclasses as well as the same.
			If TObjectType(decl.argDecls[i].ty) Then
				Local ty:TObjectType = TObjectType(decl.argDecls[i].ty)
				If Not ty.EqualsType( argDecls[i].ty ) And (exactMatch Or Not ty.ExtendsType( argDecls[i].ty, false , false, ignoreObjectSubclasses )) Return False
			Else
				If Not decl.argDecls[i].ty.EqualsType( argDecls[i].ty ) Return False
			End If
		Next
		Return True
	End Method

	' exactMatch requires args to be equal. If an arg is a subclass, that is not a match.
	Method EqualsFunc:Int( decl:TFuncDecl, exactMatch:Int = False) ' careful, this is not commutative!
		If IsCtor() Then
			Return EqualsArgs( decl, exactMatch )
		Else
			' matching args?
			If EqualsArgs( decl, exactMatch ) Then
				' matching return type?
				If TObjectType(retType) Or TArrayType(retType) Or TStringType(retType) Then
					Return retType.EqualsType( decl.retType ) Or retType.ExtendsType( decl.retType )' Or decl.retType.EqualsType( retType )) And EqualsArgs( decl )
				Else
					Return retType.EqualsType( decl.retType )
				End If
			End If
		End If
		Return False
	End Method

	Method OnSemant()

		Local strictVoidToInt:Int = False

		If isCtor() Or isDtor() Then
			If retTypeExpr And Not TVoidType(retTypeExpr) And Not generated Then
				Err ident + "() cannot specify a return type"
			End If
			Local sc:TClassDecl = ClassScope()
			If sc Then
				If sc.IsInterface() Then
					Err ident + "() cannot be declared in an Interface."
				Else If sc.IsStruct() And isDtor() Then
					Err ident + "() cannot be declared in a Struct."
				End If
			End If
			If IsCtor() retTypeExpr=New TObjectType.Create( TNewDecl(Self).cDecl )
		End If

		'semant ret type
		If Not retTypeExpr Then
			If Not retType Then ' may have previously been set (if this is a function pointer)
				retType = TType.voidType
			Else If TIdentType(retType)
				retType = retType.Semant()
			Else
				' for Strict code, a void return type becomes Int
				If TVoidType(retType) And Not ModuleScope().IsSuperStrict() Then
					strictVoidToInt = True
					retType = New TIntType
				End If
			End If
		Else
			' pass the scope into the function ptr
			Local retTypeExpr_:TType = retTypeExpr
			While TArrayType(retTypeExpr_) ' look into array types, since the element type might be function ptr
				retTypeExpr_ = TArrayType(retTypeExpr_).elemType
			Wend
			If TFunctionPtrType(retTypeExpr_) Then
				If Not TFunctionPtrType(retTypeExpr_).func.scope Then
					If scope Then
						TFunctionPtrType(retTypeExpr_).func.scope = scope
					Else
						TFunctionPtrType(retTypeExpr_).func.scope = _env
					End If
				End If
			End If
			retType=retTypeExpr.Semant()
			
			' for Strict code, a void return type becomes Int
			If TVoidType(retType) And Not ModuleScope().IsSuperStrict() And Not IsDTor() Then
				strictVoidToInt = True
				retType = New TIntType
			End If
		End If
		
		retType = retType.Semant()
		
		If TArrayType( retType ) And Not retType.EqualsType( retType.ActualType() )
'			Err "Return type cannot be an array of generic objects."
		EndIf

		If ClassScope() And TObjectType(retType) And TObjectType(retType).classDecl.IsStruct() And TObjectType(retType).classDecl.IsPrivate() Then
			TObjectType(retType).classDecl.exposed = True
		End If

		'semant args
		For Local arg:TArgDecl=EachIn argDecls
			InsertDecl arg
			arg.Semant

			If ClassScope() And TObjectType(arg.ty) And TObjectType(arg.ty).classDecl.IsStruct() And TObjectType(arg.ty).classDecl.IsPrivate() Then
				TObjectType(arg.ty).classDecl.exposed = True
			End If
		Next

		' if we are a function pointer declaration, we just want to semant the args here.
		If attrs & FUNC_PTR Return

		If actual<>Self Return
		
		'check for duplicate decl
		If ident Then
			For Local decl:TFuncDecl=EachIn scope.SemantedFuncs( ident )
				If decl<>Self And EqualsArgs( decl, True )
					Err "Duplicate declaration "+ToString()
				EndIf
				If noMangle Then
					If decl<>Self Then
						If decl.argDecls.Length = 0 Then
							Err "You cannot apply NoMangle to the function, as another function with no arguments exists."
						Else If decl.NoMangle Then
							Err "Another function is already declared with NoMangle."
						End If
					End If
				End If
				If exported Then
					If decl<>Self Then
						If decl.argDecls.Length = 0 Then
							Err "You cannot apply Export to the function, as another function with no arguments exists."
						Else If decl.exported Then
							Err "Another function is already declared with Export."
						End If
					End If
				End If
			Next
		End If
		
		' any nested functions?
		For Local fdecl:TFuncDecl = EachIn _decls
			fdecl.Semant
		Next
		
		'get cdecl, sclasss
		Local cdecl:TClassDecl=ClassScope(),sclass:TClassDecl
		If cdecl sclass=TClassDecl( cdecl.superClass )
		
		'prefix call to super ctor if necessary
'		If IsCtor() And superCtor=Null And sclass
'			If sclass.FindFuncDecl( "new", Null )
'				superCtor=New TInvokeSuperExpr.Create( "new" )
'				stmts.AddFirst New TExprStmt.Create( superCtor )
'			EndIf
'		EndIf
		
		'check we exactly match an override
		If sclass 'And IsMethod()

			Local found:Int

			While sclass
				Local errorDetails:String = ""

				found = MatchesFunction(sclass, strictVoidToInt, errorDetails)

				' check interfaces?
				If Not found Then
					If sclass = cdecl.superClass Then
						found = MatchesInterfaceFunction(cdecl, strictVoidToInt, errorDetails)
					End If

					If Not found Then
						found = MatchesInterfaceFunction(sclass, strictVoidToInt, errorDetails)
					End If
				End If
				
				If found
					If Not overrides Err "Overriding method does not match any overridden method. (Detail: " + errorDetails+")"
					If overrides.IsFinal() Err "Final methods cannot be overridden."
					If Not (attrs & DECL_OVERRIDE) And opt_require_override And Not IsImported() Then
						Local msg:String = "Overriding method '" + ident + "' must be declared with 'Override'."
						If Not opt_override_error Then
							Warn msg
						Else
							Err msg
						End If
					End If
					' for overrides, make the ident match that of the superclass
					ident = overrides.ident
					
					Exit
				EndIf
				sclass=sclass.superClass
			Wend
			
			If Not found And attrs & DECL_OVERRIDE Then
				Err "Method does not override method from its super type."
			End If
		EndIf

		'append a return statement if necessary
		If Not IsExtern() And Not TVoidType( retType ) And Not TReturnStmt( stmts.Last() )
			If Not isCtor() And Not isDtor()
				Local stmt:TReturnStmt

				stmt=New TReturnStmt.Create( New TConstExpr.Create( retType,"" ) )
				stmt.generated = True

				stmt.errInfo=errInfo
				stmts.AddLast stmt
			End If
		EndIf

		attrs:|DECL_SEMANTED
		
		Super.OnSemant()
	End Method
	
	Method MatchesInterfaceFunction:Int(cdecl:TClassDecl, strictVoidToInt:Int, errorDetails:String Var)
		Local found:Int

		If Not found Then
			For Local idecl:TClassDecl = EachIn cdecl.implments
				found = MatchesFunction(idecl, strictVoidToInt, errorDetails)
			
				If Not found Then
					found = MatchesInterfaceFunction(idecl, strictVoidToInt, errorDetails)
				End If
				
				If found Then
					Exit
				End If
			Next
		End If
		Return found
	End Method

	Method MatchesFunction:Int(sclass:TClassDecl, strictVoidToInt:Int, errorDetails:String Var)
		Local found:Int
		For Local decl:TFuncDecl=EachIn sclass.FuncDecls( )
			
			If decl.IdentLower() = IdentLower() Then

				If IdentLower() = "new" Continue
				If IdentLower() = "delete" Continue

				found=True

				If Not decl.IsSemanted() Then
					decl.Semant
				End If

				' check void return type strictness, and fail if appropriate.
				Local voidReturnTypeFail:Int = False
				' super has void return type... so it is superstrict (or inherited from)
				If TVoidType(decl.retType) And TIntType(retType) Then
					' if we are only strict, we may fail on type mismatch
					If Not ModuleScope().IsSuperStrict() Then
						' we have the option of upgrading our return type to match superstrict parent
						If opt_strictupgrade And strictVoidToInt Then
							retType = TType.voidType
						Else
							' otherwise...
							voidReturnTypeFail = True
						End If
					End If
				End If

				If EqualsFunc( decl ) And Not voidReturnTypeFail

					' check we aren't attempting to assign weaker access modifiers
					If (IsProtected() And decl.IsPublic()) Or (IsPrivate() And (decl.IsProtected() Or decl.IsPublic())) Then
					
						Err PrivilegeError(Self, decl)
					
					End If
				
					If (TObjectType(retType) And TObjectType(decl.retType )) Or (TArrayType(retType) And TArrayType(decl.retType)) Then
						If Not retType.EqualsType( decl.retType ) And retType.ExtendsType( decl.retType ) Then
							returnTypeSubclassed = True
						End If
					End If
					
					overrides=TFuncDecl( decl.actual )
				Else
					' method overloading?
					If Not EqualsArgs(decl) Then
						found = False
						Continue
					End If
					
					'prepare a more detailed error message
					If (Not retType.EqualsType( decl.retType ) Or Not retType.ExtendsType( decl.retType )) Or (decl.retType And Not decl.retType.EqualsType( retType )) Or voidReturnTypeFail
						errorDetails :+ "Return type is ~q"+retType.ToString()+"~q, expected ~q"+decl.retType.ToString()+"~q. "
						If voidReturnTypeFail Then
							errorDetails :+ "You may have Strict type overriding SuperStrict type. "
						End If
					Else
						found = False
						Continue
					End If

					Local argCount:Int = Min(argDecls.Length, decl.argDecls.Length)
					If argCount > 0
						For Local i:Int=0 Until argCount
							If Not argDecls[i].ty.EqualsType( decl.argDecls[i].ty )
								errorDetails :+ "Argument #"+(i+1)+" is ~q" + argDecls[i].ty.ToString()+"~q, expected ~q"+decl.argDecls[i].ty.ToString()+"~q. "
							End If
						Next
					EndIf
					'remove last space
					errorDetails = errorDetails.Trim()
				EndIf
			End If
			
			If found Exit
		Next
		Return found
	End Method

	Method CheckAccess:Int()
		If ModuleScope() = _env.ModuleScope() Return True
		Local cd:TClassDecl = ClassScope()
		If cd Then
			If IsPrivate() And cd<>_env.ClassScope() Return False
			If IsProtected() Then
				Local ec:TClassDecl = _env.ClassScope()
				If Not ec Return False
				If Not ec.ExtendsClass(cd) Return False
			End If
			Return True
		End If
		Return Super.CheckAccess()
	End Method

	Function PrivilegeError:String(decl:TFuncDecl, decl2:TFuncDecl)
		Local p:String
		If decl.IsProtected() Then
			p = "Protected"
		Else
			p = "Private"
		End If
		
		Local dp:String
		If decl2.IsPublic() Then
			dp = "Public"
		Else
			dp = "Protected"
		End If
	
		Return decl.ToString() + " clashes with " + decl2.ToString() + ". Attempt to assign weaker access privileges ('" + p + "'), was '" + dp + "'."
	End Function
	
	Method NextIdx:Int()
		Local i:Int = idx
		idx :+ 1
		return i
	End Method

End Type

Type TNewDecl Extends TFuncDecl
	
	Field chainedCtor:TNewExpr
	Field cdecl:TClassDecl
	
	Method OnCopy:TDecl(deep:Int = True)
		Local args:TArgDecl[]=argDecls[..]
		For Local i:Int=0 Until args.Length
			args[i]=TArgDecl( args[i].Copy() )
		Next
		Local retTypeCopy:TType
		If IsSemanted() Then
			retTypeCopy = Null
		Else
			retTypeCopy = retType
		End If
		Local t:TNewDecl = TNewDecl(New TNewDecl.CreateF( ident,retTypeCopy,args,attrs &~DECL_SEMANTED ))
		If deep Then
			For Local stmt:TStmt=EachIn stmts
				t.AddStmt stmt.Copy(t)
			Next
		End If
		t.retType = retType
		t.scope = scope
		t.overrides = overrides
		t.superCtor = superCtor
		t.castTo = castTo
		t.noCastGen = noCastGen
		t.munged = munged
		t.metadata = metadata
		t.mangled = mangled
		t.noMangle = noMangle
		t.chainedCtor = chainedCtor
		t.cdecl = cdecl
		
		Return  t
	End Method
	
	
End Type


'Const CLASS_INTERFACE:Int=1
'Const CLASS_TEMPLATEARG:Int=2
'Const CLASS_TEMPLATEINST:Int=4
'Const CLASS_INSTANCED:Int=8		'class used in New?
Const CLASS_INSTANCED:Int=1
Const CLASS_EXTENDSOBJECT:Int=2
Const CLASS_FINALIZED:Int=4

Type TNullDecl Extends TClassDecl

End Type

' used to handle recursive generics
' by setting the superclass as soon as we know it,
' which allows the semanting of the instance to complete.
Type TClassDeclCallback Extends TCallback
	Field decl:TClassDecl
	Method callback(obj:Object)
		decl.superClass = TClassDecl(obj)
	End Method
End Type

Type TClassDecl Extends TScopeDecl

	Field lastOffset:Int

	Field args:TTemplateArg[]
	Field superTy:TIdentType
	Field impltys:TIdentType[]

	Field superClass:TClassDecl
	
	Field implments:TClassDecl[]			'interfaces immediately implemented
	Field implmentsAll:TClassDecl[]		'all interfaces implemented
	
	Field instanceof:TClassDecl			'for instances
	Field instances:TBCCObjectList
	Field instanceIdents:TBCCObjectList		'for actual (non-arg, non-instance)
	Field instArgs:TType[]

	Field objectType:TObjectType '"canned" objectType
	Field globInit:Int
	Field templateSource:TTemplateRecord
	
	Field exposed:Int

	'Global nullObjectClass:TClassDecl=New TNullDecl.Create( "{NULL}",Null,Null,Null,DECL_ABSTRACT|DECL_EXTERN )
	
	Method Create:TClassDecl( ident$,args:TTemplateArg[],superTy:TIdentType,impls:TIdentType[],attrs:Long )
		Self.ident=ident
		Self.args=args
		Self.superTy=superTy
		Self.impltys=impls
		Self.attrs=attrs
		Self.objectType=New TObjectType.Create( Self )
		If args
			instances=New TBCCObjectList
			instanceIdents=New TBCCObjectList
		EndIf
		Return Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		InternalErr "TClassDecl.OnCopy"
	End Method
	
	Method ToString$()
		Local t$

		If args Then
			For Local i:Int=0 Until args.Length
				If i Then
					t :+ ","
				End If
				t:+args[i].ToString()
			Next
		ElseIf instargs
			For Local i:Int=0 Until instargs.Length
				If i Then
					t :+ ","
				End If
				t :+ instargs[i].ToString()
			Next
		End If
		If t t="<"+t+">"
		Return ident+t
	End Method

	Method ToTypeString:String()
		Return ToString()
	End Method
Rem
	Method GenClassInstance:TClassDecl( instArgs:TClassDecl[] )
		If Not IsSemanted() InternalErr
		
		'no args
		If Not instArgs
			If Not args Return Self
			If instanceof Return Self
			For Local inst:TClassDecl=EachIn instances
				If _env.ClassScope()=inst Return inst
			Next
		EndIf
		
		'If Not instanceof And Not instArgs Return Self
		
		'check number of args
		If instanceof Or args.Length<>instArgs.Length
			Err "Wrong number of class arguments for "+ToString()
		EndIf
		
		'look for existing instance
		For Local inst:TClassDecl=EachIn instances
			Local equal:Int=True
			For Local i:Int=0 Until args.Length
				If inst.args[i]=instArgs[i] Continue
				equal=False
				Exit
			Next
			If equal Return inst
		Next
		
		Local inst:TClassDecl=New TClassDecl

		InitInstance inst

		inst.scope=scope
		inst.attrs:|CLASS_TEMPLATEINST
		inst.args=instArgs
		inst.superTy=superTy
		inst.instanceof=Self
		instances.AddLast inst
		
		For Local i:Int=0 Until args.Length
			inst.InsertDecl New TAliasDecl.Create( args[i].ident,instArgs[i] )
		Next
		
		For Local decl:TDecl=EachIn _decls
			If TClassDecl( decl ) Continue
			inst.InsertDecl decl.GenInstance()
		Next

		'inst.Semant
		'A bit cheeky...
		inst.OnSemant
		inst.attrs:|DECL_SEMANTED
		
		Return inst
	End Method
End Rem

	Method HasClassInstance:Int(instanceIdent:String)
		If Not instances Return False
		Local identLower:String = instanceIdent.ToLower()
		For Local ident:String=EachIn instanceIdents
			If ident=identLower Return True
		Next
		Return False
	End Method

	Method GenClassInstance:TClassDecl( instArgs:TType[], declImported:Int = False, callback:TCallback = Null, templateDets:TTemplateDets = Null, instanceIdent:String = Null )

		If instanceof InternalErr "TClassDecl.GenClassInstance"
		
		'no args
		If Not instArgs
			If Not args Return Self
			For Local inst:TClassDecl=EachIn instances
				If _env.ClassScope()=inst Return inst
			Next
		EndIf
		
		Local originalInstArgs:TType[] = instArgs
		
		'check number of args
		If args.Length<>instArgs.Length Then
			If Not templateDets Or args.Length > instArgs.Length Then
				Err "Wrong number of type arguments for class "+ToString()
			Else
				' create new instArgs with matched aliases
				Local newInstArgs:TType[] = New TType[args.length]
				For Local i:Int = 0 Until args.length
					Local arg:TTemplateArg = args[i]
					Local instArg:TType
					' find match
					For Local n:Int = 0 Until templateDets.args.length
						Local templateArg:TTemplateArg = templateDets.args[n]
						If templateArg.ident.ToLower() = arg.ident.ToLower() Then
							instArg = instArgs[n]
							Exit
						End If
					Next
					If Not instArg Then
						Err "Cannot find argument type '" + arg.ident + "' for class " + ToString()
					End If
					newInstArgs[i] = instArg
				Next
				
				instArgs = newInstArgs
			End If
		EndIf
		
		'look for existing instance
		For Local inst:TClassDecl=EachIn instances
			Local equal:Int=True
			For Local i:Int=0 Until args.Length
				Local instArg:TType = inst.instArgs[i].Semant()
				inst.instArgs[i] = instArg
				
				Local otherInstArg:TType = instArgs[i].Semant()
				instArgs[i] = otherInstArg
				
				If Not instArg.EqualsType( otherInstArg )
					equal=False
					Exit
				EndIf
			Next
			If equal Then
				If Not instanceIdent Then
					instanceIdent = inst.ToString().ToLower()
				End If

				If Not HasClassInstance(instanceIdent) Then
					instanceIdents.AddLast instanceIdent.ToLower()
				End If
				Return inst
			End If
		Next
		
		If Not templateDets Then
			' pass in the original instargs, as an inner-inner type will be able to see all of the originals
			templateDets = New TTemplateDets.Create(originalInstArgs, args)
		End If

		Local inst:TClassDecl = TClassDecl(TGenProcessor.processor.ParseGeneric(templateSource, templateDets))
		inst.ident=ident
		inst.args=Null
		inst.instances = Null
		inst.superTy=superTy
		inst.impltys=impltys
		inst.attrs=attrs

		inst.attrs:&~DECL_SEMANTED

		inst.munged=munged
		inst.errInfo=errInfo
		inst.scope=scope
		inst.instanceof=Self
		inst.instArgs=instArgs
		inst.templateSource = templateSource
		instances.AddLast inst

		If instanceIdent Then
			instanceIdents.AddLast instanceIdent.ToLower()
		End If
		
		inst.declImported = declImported

		If callback Then
			callback.callback(inst)
		End If

		PushEnv inst
		
		' install aliases
		For Local i:Int=0 Until args.Length
			inst.InsertDecl New TAliasDecl.Create( args[i].ident,instArgs[i],0 )
		Next

		' process parameter types
		For Local i:Int=0 Until args.Length
		
			Local arg:TTemplateArg = args[i]

			' ensure parameter types are compatible
			If arg.superTy Then

				'If Not instArgs[i].IsSemanted() Then
				If TObjectType(instArgs[i]) Then
					TObjectType(instArgs[i]).classDecl.Semant()
				End If
				'End If
			
				For Local n:Int = 0 Until arg.superTy.length

					arg.superTy[n] = arg.superTy[n].Semant()

					If Not instArgs[i].EqualsType(arg.superTy[n]) And Not instArgs[i].ExtendsType(arg.superTy[n]) Then
						Err "Type parameter '" + instArgs[i].ToString() + "' is not within its bound; should extend '" + arg.superTy[n].ToString() + "'"
					End If
				Next
			End If
		
		Next
		
		PopEnv

'		For Local decl:TDecl=EachIn _decls
'			If TClassDecl(decl) Then
'				inst.InsertDecl TClassDecl(decl).GenClassInstance(instArgs, declImported), True
'			Else
'				inst.InsertDecl decl.Copy(), True
'			End If
'		Next

		If Not declImported Then
			inst.scope = _env.ModuleScope()
		End If

		Return inst
	End Method

	Method IsInterface:Int()
		Return (attrs & CLASS_INTERFACE)<>0
	End Method

	Method IsThrowable:Int()
		Return (attrs & CLASS_THROWABLE)<>0
	End Method

	Method IsFinalized:Int()
		Return (attrs & CLASS_FINALIZED)<>0
	End Method
	
	Method IsStruct:Int()
		Return (attrs & CLASS_STRUCT)<>0
	End Method

	Method ExtendsObject:Int()
		Return (attrs & CLASS_EXTENDSOBJECT)<>0
	End Method
	
	Method IsInstanced:Int()
		Return (attrs & CLASS_INSTANCED)<>0
	End Method

	Method IsImported:Int()
		Return declImported And Not (instanceof And opt_apptype)
	End Method
	
	Method GetDecl:Object( ident$ )
	
		Local cdecl:TClassDecl=Self
		While cdecl
			Local decl:Object=cdecl.GetDecl2( ident )
			If decl Return decl
			
			cdecl=cdecl.superClass
		Wend

	End Method
	
	'needs this 'coz you can't go blah.Super.GetDecl()...
	Method GetDecl2:Object( ident$ )
		Return Super.GetDecl( ident )
	End Method

	Method GetDeclList:Object( ident$, declList:TFuncDeclList = Null, maxSearchDepth:Int )
	
		If Not declList Then
			declList = New TFuncDeclList
		End If
	
		Local cdecl:TClassDecl=Self
		While cdecl
			Local decl:Object=cdecl.GetDeclList2( ident, declList, maxSearchDepth )
			'If decl And (Not TFuncDeclList(decl) And declList.IsEmpty()) Return decl
			If decl Then
				declList.AddLast(decl)
			End If

			cdecl=cdecl.superClass
			
			If maxSearchDepth < SCOPE_CLASS_HEIRARCHY Then
				Exit
			End If
		Wend

		Return declList
	End Method
	
	'needs this 'coz you can't go blah.Super.GetDecl()...
	Method GetDeclList2:Object( ident$, declList:TFuncDeclList = Null, maxSearchDepth:Int )
		Return Super.GetDeclList( ident, declList, maxSearchDepth )
	End Method
	
	Method FindFuncDecl:TFuncDecl( ident$,args:TExpr[] = Null ,explicit:Int=False, isArg:Int = False, isIdentExpr:Int = False, throwOnNotMatched:Int = False, maxSearchDepth:Int )
	
		' try the super first...
		Local funcDecl:TFuncDecl = Super.FindFuncDecl(ident, args, explicit, isArg, isIdentExpr, throwOnNotMatched, maxSearchDepth)
		If funcDecl Then
			Return funcDecl
		End If
	
	
		If args = Null Then
			args = New TExpr[0]
		End If
	
		If Not IsInterface()
			' try getdecl first&
			Local decl:TFuncDecl = TFuncDecl(GetDecl(ident))
			If decl Then
				Return decl
			End If
			Return FindFuncDecl2( ident,args,explicit,isIdentExpr )
		EndIf
		
		Local fdecl:TFuncDecl=FindFuncDecl2( ident,args,True )
		
		For Local iface:TClassDecl=EachIn implmentsAll
			Local decl:TFuncDecl=iface.FindFuncDecl2( ident,args,True )
			If Not decl Continue
			
			If fdecl
				If fdecl.EqualsFunc( decl ) Continue
				Err "Unable to determine overload to use: "+fdecl.ToString()+" or "+decl.ToString()+"."
			EndIf
			fdecl=decl
		Next
		
		If fdecl Or explicit Return fdecl
		
		fdecl=FindFuncDecl2( ident,args,False )
		
		For Local iface:TClassDecl=EachIn implmentsAll
			Local decl:TFuncDecl=iface.FindFuncDecl2( ident,args,False )
			If Not decl Continue
			
			If fdecl
				If fdecl.EqualsFunc( decl ) Continue
				Err "Unable to determine overload to use: "+fdecl.ToString()+" or "+decl.ToString()+"."
			EndIf
			fdecl=decl
		Next
		
		Return fdecl
	End Method
	
	Method FindFuncDecl2:TFuncDecl( ident$,args:TExpr[],explicit:Int, isIdentExpr:Int = False )
		Return Super.FindFuncDecl( ident,args,explicit,,isIdentExpr,0,0 )
	End Method
	
	Method GetAllFuncDecls:TFuncDecl[](funcs:TFuncDecl[] = Null, includeSuper:Int = True, onlyCtors:Int = False)

		If superClass And includeSuper Then
			funcs = superClass.GetAllFuncDecls(funcs)
		End If

		' interface methods
		If includeSuper Then
			For Local iface:TClassDecl=EachIn implmentsAll
				funcs = iface.GetAllFuncDecls(funcs)
			Next
		End If
		
		For Local func:TFuncDecl = EachIn _decls

			If onlyCtors And Not func.IsCtor() Then
				Continue
			End If

			Local matched:Int = False
			
			For Local i:Int = 0 Until funcs.length
				Local ofunc:TFuncDecl = funcs[i]
				' found a match - we are overriding it
				If func.IdentLower() = ofunc.IdentLower() And func.EqualsArgs(ofunc,,True) Then
					matched = True
					
					If func.scope <> ofunc.scope Then
						' but don't override if we are an interface and the function is implemented
						If IsInterface() And Not ofunc.ClassScope().IsInterface() Then
							Exit
						End If
						' set this to our own func
						funcs[i] = func
					End If
					Exit
				End If
			Next
			
			If Not matched Then
				funcs :+ [func]
			End If
		
		Next
		
		Return funcs
	End Method

	' returns a list of original function decls (i.e. decls in the scope of their original declarations).
	' this is useful for generating vtables for extern types
	Method GetAllOriginalFuncDecls:TFuncDecl[](funcs:TFuncDecl[] = Null, includeSuper:Int = True)
		If Not funcs Then
			funcs = New TFuncDecl[0]
		End If
		
		If superClass And includeSuper Then
			funcs = superClass.GetAllOriginalFuncDecls(funcs, True)
		End If

		' interface methods
		For Local iface:TClassDecl=EachIn implmentsAll
			For Local func:TFuncDecl=EachIn iface._decls
				Local matched:Int = False

'				For Local i:Int = 0 Until funcs.length
'					' found a match - we are overriding it
'					If func.IdentLower() = funcs[i].IdentLower() Then
'						matched = True
'						Exit
'					End If
'				Next
				
				If Not matched Then
					funcs :+ [func]
				End If
			Next
		Next

		
		For Local func:TFuncDecl = EachIn _decls
		
			Local matched:Int = False
			
			' dont count any that are already in the funcs list
			For Local i:Int = 0 Until funcs.length
				' found a match - we are overriding it
				If func.IdentLower() = funcs[i].IdentLower() And func.EqualsArgs(funcs[i]) Then
					matched = True
					' set this to our own func
					'funcs[i] = func
					Exit
				End If
			Next
			
			If Not matched Then
				funcs :+ [func]
			End If
		
		Next
		
		Return funcs
	End Method

	Method GetOriginalFuncDecl:TFuncDecl(fdecl:TFuncDecl)
		If Not TClassDecl(Self) Then
			Return fdecl
		End If
		
		If superClass Then
			Local decl:TFuncDecl = superClass.GetOriginalFuncDecl(fdecl)
			If decl <> fdecl Then
				Return decl
			End If
		End If
		
		For Local func:TFuncDecl = EachIn _decls
		
			If func.IdentLower() = fdecl.IdentLower() And func.EqualsArgs(fdecl) Then
				Return func
			End If
		
		Next
		
		Return fdecl
	End Method

	Method GetLatestFuncDecl:TFuncDecl(fdecl:TFuncDecl)
		If Not TClassDecl(Self) Then
			Return fdecl
		End If
		
		For Local func:TFuncDecl = EachIn _decls
		
			If func.IdentLower() = fdecl.IdentLower() And func.EqualsArgs(fdecl, True) Then
				Return func
			End If
		
		Next
		
		If superClass Then
			Local decl:TFuncDecl = superClass.GetLatestFuncDecl(fdecl)
			If decl <> fdecl Then
				Return decl
			End If
		End If
		

		Return fdecl
	End Method
	
	Method ExtendsClass:Int( cdecl:TClassDecl, ignoreObjectSubclasses:Int = False )
		'If Self=nullObjectClass Return True
		
'		If cdecl.IsTemplateArg()
'			cdecl=TType.objectType.FindClass()
'		EndIf
		
		Local tdecl_:TClassDecl=Self
		While tdecl_
			If tdecl_=cdecl Then
				If ignoreObjectSubclasses And tdecl_.ident = "Object"
					Return False
				Else
					Return True
				End If
			End If
			If cdecl.IsInterface()
				For Local iface:TClassDecl=EachIn tdecl_.implmentsAll
					If iface=cdecl Return True
				Next
			EndIf
			tdecl_=tdecl_.superClass
		Wend
		
		Return False
	End Method
	
	Method OnSemant()

		If args Then
			Return
		End If

		PushEnv Self

		'If Not IsTemplateInst()
		'	For Local i:Int=0 Until args.Length
		'		InsertDecl args[i]
		'		args[i].Semant
		'	Next
		'EndIf

		'Semant superclass		
		If superTy
			Local cb:TClassDeclCallback = New TClassDeclCallback
			cb.decl = Self

			attrs :| DECL_CYCLIC
			superClass=superTy.SemantClass(cb)
			If superClass.attrs & DECL_CYCLIC Then
				Err "Cyclic type dependency"
			End If
			attrs :~ DECL_CYCLIC
			If superClass.IsInterface() Then
				If Not IsExtern() Or Not superClass.IsExtern() Err superClass.ToString()+" is an interface, not a class."
				If (IsExtern() And Not superClass.IsExtern()) Or (superClass.IsExtern() And Not IsExtern()) Err "Extern and non extern types cannot be mixed."
			End If
			If superClass.IsFinal() Err "Final types cannot be extended."
		EndIf

		'Semant implemented interfaces
		Local impls:TClassDecl[]=New TClassDecl[impltys.Length]
		Local implsall:TStackList=New TStackList
		For Local i:Int=0 Until impltys.Length
			attrs :| DECL_CYCLIC
			Local cdecl:TClassDecl=impltys[i].SemantClass()
			attrs :~ DECL_CYCLIC
			If Not cdecl.IsInterface()
				Err cdecl.ToString()+" is a type, not an interface."
			EndIf
			For Local j:Int=0 Until i
				If impls[j]=cdecl
					Err "Duplicate interface "+cdecl.ToString()+"."
				EndIf
			Next
			impls[i]=cdecl
			implsall.Push cdecl
			For Local tdecl_:TDecl=EachIn cdecl.implmentsAll
				implsall.Push tdecl_
			Next
		Next
		Local length:Int = implsall.Length()
		implmentsAll=New TClassDecl[length]
		For Local i:Int=0 Until length
			implmentsAll[i]=TClassDecl(implsall.Get(length - i - 1))
		Next
		implments=impls

		Rem
		If IsInterface()
			'add implemented methods to our methods
			For Local iface:=EachIn implmentsAll
				For Local decl:=EachIn iface.FuncDecls
					InsertAlias decl.ident,decl
				Next
			Next
		EndIf
		EndRem
				
'		attrs|=DECL_SEMANTED
		
		PopEnv
		
		'If IsTemplateArg()
		'	actual=TType.objectType.FindClass()
		'	Return
		'EndIf
		
		'If IsTemplateInst()
		'	Return
		'EndIf
		
		If Not lastOffset And superClass Then
			lastOffset = superClass.LastOffset
		End If

		For Local decl:TFieldDecl=EachIn _decls
			GetFieldOffset(decl)
		Next

		For Local decl:TDecl=EachIn _decls
			If TClassDecl(decl) Then
				TClassDecl(decl).Semant
			End If
		Next

		' structs have a default New and Compare
		' if we haven't defined one, create one
		If attrs & CLASS_STRUCT And Not IsExtern() And Not IsImported() Then
			attrs :| DECL_CYCLIC
			Local func:TFuncDecl = FindFuncDecl("new", Null,True,,,,0)
			If Not func Then
				func = New TNewDecl.CreateF("New", Null, Null, FUNC_CTOR)
				func.generated = True
				TNewDecl(func).cdecl = Self
				InsertDecl(func)
			End If
			
			' add default compare if required
			Local list:TFuncDeclList = TFuncDeclList(FindDeclList("compare", , , SCOPE_CLASS_LOCAL))
			
			Local arg:TArgDecl = New TArgDecl.Create("o1", TType.MapToVarType(New TObjectType.Create(Self)), Null)
			func = New TFuncDecl.CreateF("Compare", New TIntType, [arg], FUNC_METHOD)
			func.generated = True
			func.retType = New TIntType
			BuildStructCompareStatements(func)
			
			Local found:Int
			If list And list.Count() Then
				For Local fdecl:TFuncDecl = EachIn list
					If fdecl.EqualsFunc(func, True) Then
						found = True
						Exit
					End If
				Next
			End If
			If Not found Then
				InsertDecl(func)
			End If
			
			' generate default comparator compare
			BuildStructDefaultComparatorCompare(attrs & DECL_PRIVATE <> 0)
			
			attrs :~ DECL_CYCLIC
		End If
		
		'NOTE: do this AFTER super semant so UpdateAttrs order is cool.

		If AppScope() Then
			AppScope().semantedClasses.AddLast Self
		End If
	End Method

	Method OnSemant2()

		If args or instargs Then
			Return
		End If

		If Not IsExtern() And Not IsInterface()
			Local fdecl:TFuncDecl
			For Local decl:TFuncDecl=EachIn GetAllFuncDecls(Null, True, True)
				If Not decl.IsCtor() Continue

				' only non default ctors
				If decl.argDecls.Length > 0 Then

					' method belongs to super? implement default
					If decl.Scope <> Self Then

						fdecl = TFuncDecl(decl.OnCopy(False))
						fdecl.generated = True
						fdecl.scope = Null
						fdecl.overrides = decl
						fdecl.castTo = Null
						fdecl.noCastGen = Null
						fdecl.munged = Null
						fdecl.metadata = metadata
						fdecl.mangled = Null
						fdecl.noMangle = Null
				
						TNewDecl(fdecl).cdecl = Self
						InsertDecl(fdecl)

					End If

				End If

			Next
			

			' Don't need default new?
			'If Not fdecl
			'	fdecl=New TFuncDecl.CreateF( "new",New TObjectType.Create( Self ),Null,FUNC_CTOR )
			'	fdecl.AddStmt New TReturnStmt.Create( Null )
			'	InsertDecl fdecl
			'EndIf
		EndIf
	End Method
	
	Method BuildStructCompareStatements(func:TFuncDecl)
		func.attrs :| DECL_NODEBUG
	
		'
		' Local cmp:Int = 0
		'
		Local cmp:TLocalDecl=New TLocalDecl.Create( "cmp",Null,New TConstExpr.Create( New TIntType,"0" ),,True )
		func.AddStmt New TDeclStmt.Create(cmp)
		Local cmpVar:TVarExpr = New TVarExpr.Create(cmp)
		
		' iterate fields
		For Local fdecl:TFieldDecl = EachIn _decls
			fdecl.Semant()
			
			'
			' If cmp <> 0 Then
			'    Return cmp
			' End If
			'
			Local ifExpr:TExpr = New TBinaryCompareExpr.Create( "<>",cmpVar, New TConstExpr.Create( New TIntType,"0" ))
			Local thenBlock:TBlockDecl=New TBlockDecl.Create( func, , BLOCK_IF )
			Local elseBlock:TBlockDecl=New TBlockDecl.Create( func, , BLOCK_ELSE )
			Local returnStmt:TReturnStmt = New TReturnStmt.Create( cmpVar )
			returnStmt.generated = True
			returnStmt.errInfo=errInfo
			thenBlock.AddStmt returnStmt
			func.AddStmt New TIfStmt.Create( ifExpr,thenBlock,elseBlock )
			
			'
			' cmp = DefaultComparator_Compare( _field, o1._field )
			'
			Local expr1:TExpr = New TIdentExpr.Create( fdecl.ident )
			Local expr2:TExpr = New TIdentExpr.Create( "o1")
			expr2 = New TIdentExpr.Create( fdecl.ident, expr2)
			
			If TEnumType(fdecl.ty) Then
				expr1 = New TFuncCallExpr.Create(New TIdentExpr.Create("Ordinal", expr1))
				expr2 = New TFuncCallExpr.Create(New TIdentExpr.Create("Ordinal", expr2))
			End If
			
			Local fcExpr:TExpr = New TIdentExpr.Create( "DefaultComparator_Compare")
			fcExpr = New TFuncCallExpr.Create( fcExpr, [expr1, expr2])
	
			func.AddStmt New TAssignStmt.Create( "=",cmpVar,fcExpr)
			
		Next
		
		'
		' Return cmp
		'
		Local returnStmt:TReturnStmt = New TReturnStmt.Create( cmpVar )
		returnStmt.generated = True
		returnStmt.errInfo=errInfo
		func.stmts.AddLast returnStmt
	End Method

	Method BuildStructDefaultComparatorCompare(isPrivate:Int = False)
		Local arg1:TArgDecl = New TArgDecl.Create("o1", TType.MapToVarType(New TObjectType.Create(Self)), Null)
		Local arg2:TArgDecl = New TArgDecl.Create("o2", TType.MapToVarType(New TObjectType.Create(Self)), Null)
		Local func:TFuncDecl = New TFuncDecl.CreateF("DefaultComparator_Compare", New TIntType, [arg1, arg2], 0)
		If isPrivate Then
			func.attrs :| DECL_PRIVATE
		End If

		Local expr:TExpr = New TIdentExpr.Create( "o1")
		expr = New TIdentExpr.Create( "Compare" ,expr )
		expr = New TFuncCallExpr.Create( expr, [New TIdentExpr.Create("o2")])
		
		Local returnStmt:TReturnStmt = New TReturnStmt.Create( expr )
		returnStmt.generated = True
		returnStmt.errInfo=errInfo
		func.stmts.AddLast returnStmt		
		
		ModuleScope().InsertDecl func
	End Method
	
	Method SemantParts()
'		If IsSemanted() Return
		
'		Super.Semant()
		If args Then
			Return
		End If
		
		For Local decl:TConstDecl = EachIn Decls()
			decl.Semant()
		Next

		For Local decl:TGlobalDecl = EachIn Decls()
			decl.Semant()
		Next

		' NOTE : we can't semant functions here as they cause cyclic errors.
		For Local decl:TFuncDecl = EachIn Decls()
			decl.Semant()
		Next

		For Local decl:TFieldDecl = EachIn Decls()
			decl.Semant()
		Next

		' nested classes
		For Local decl:TClassDecl = EachIn Decls()
			decl.Semant()
		Next

	End Method
	
	'Ok, this dodgy looking beast 'resurrects' methods that may not currently be alive, but override methods that ARE.
	Method UpdateLiveMethods:Int()
	
		If IsInterface() Return 0

		If Not superClass Return 0

		Local n:Int
		For Local decl:TFuncDecl=EachIn MethodDecls()
			If decl.IsSemanted() Continue
			
			Local live:Int
			Local unsem:TBCCObjectList=New TBCCObjectList'<TFuncDecl>
			
			unsem.AddLast decl
			
			Local sclass:TClassDecl=superClass
			While sclass
				For Local decl2:TFuncDecl=EachIn sclass.MethodDecls( decl.ident )
					If decl2.IsSemanted()
						live=True
					Else
						unsem.AddLast decl2
						If decl2.IsExtern() live=True
						If decl2.actual.IsSemanted() live=True
					EndIf
				Next
				sclass=sclass.superClass
			Wend
			
			If Not live
				Local cdecl:TClassDecl=Self
				While cdecl
					For Local iface:TClassDecl=EachIn cdecl.implmentsAll
						For Local decl2:TFuncDecl=EachIn iface.MethodDecls( decl.ident )
							If decl2.IsSemanted()
								live=True
							Else
								unsem.AddLast decl2
								If decl2.IsExtern() live=True
								If decl2.actual.IsSemanted() live=True
							EndIf
						Next
					Next
					cdecl=cdecl.superClass
				Wend
			EndIf
			
			If Not live Continue
			
			For Local decl:TDecl=EachIn unsem
				decl.Semant
				n:+1
			Next
		Next
		
		Return n
	End Method
	
	Method FinalizeClass()
	
		SemantParts()

		PushErr errInfo
		
		If Not IsInterface()

			' BlitzMax types are promoted to Abstract if they have an abstract method
			If Not IsAbstract()
				For Local fdecl:TFuncDecl = EachIn GetAllFuncDecls()
					If fdecl.IsMethod() And fdecl.IsAbstract()
						attrs:|DECL_ABSTRACT
					End If
				Next
			End If

			' Check we implement all abstract methods!
			If IsInstanced()
				For Local fdecl:TFuncDecl = EachIn GetAllFuncDecls()
					If fdecl.IsAbstract() Then
						Err "Can't create instance of type "+ToString()+" due to abstract "+fdecl.ToString()+"."
					End If
				Next
			EndIf
			'
			'Check we implement all interface methods!
			'
			If Not IsAbstract() Then

				Local ints:TMap = GetInterfaces()

				For Local iface:TClassDecl=EachIn ints.Values()
				
					If (Not IsExtern() And iface.IsExtern()) Or (IsExtern() And Not iface.IsExtern()) Then
						Err "Cannot mix Extern and non Extern Types and Interfaces."
					End If
				
					For Local decl:TFuncDecl=EachIn iface.SemantedMethods()
						Local found:Int

						Local voidReturnTypeFail:Int
						Local cdecl:TClassDecl=Self
						
						While cdecl And Not found
							For Local decl2:TFuncDecl=EachIn cdecl.SemantedMethods( decl.ident )
								' equals (or extends - for object types)
								If decl2.EqualsFunc( decl )
									If Not decl2.IsPublic() Then
										' error on function decl
										PushErr decl2.errInfo
										Err TFuncDecl.PrivilegeError(decl2, decl)
									End If
									found=True
									Exit
								Else
									If decl2.EqualsArgs( decl, False ) Then
										If TVoidType(decl.retType) And TIntType(decl2.retType) Then
											' if we are only strict, we may fail on type mismatch
											If Not ModuleScope().IsSuperStrict() Then
												' we have the option of upgrading our return type to match superstrict parent
												If Not opt_strictupgrade Then
													voidReturnTypeFail = True
												End If
											End If
										End If

									End If
								EndIf
							Next
						
							cdecl = cdecl.superClass
						Wend

						If Not found
							Local errorDetails:String = decl.ToString() + " must be implemented by type " + ToString()
							If voidReturnTypeFail Then
								errorDetails :+ " You may have Strict type overriding SuperStrict type. "
							End If
							Err errorDetails
						EndIf
					Next
				Next
			End If
		Else
			' check for compatible overloads, etc.

			Local impls:TBCCObjectList=New TBCCObjectList

			CheckInterface(Self, impls)
			
		EndIf
		
		PopErr
		
	End Method
	
	Method CheckInterface(cdecl:TClassDecl, impls:TBCCObjectList)
		While cdecl
			For Local decl:TFuncDecl=EachIn cdecl.SemantedMethods()
				Local found:Int
				For Local decl2:TFuncDecl=EachIn impls
					If decl.IdentLower() = decl2.IdentLower()
						If decl2.argDecls.Length = decl.argDecls.Length And Not decl2.EqualsFunc( decl )
						'	Err "Cannot mix incompatible method signatures. " + decl2.ToString() + " vs " + decl.ToString() + "."
						Else
							found = True
						End If
					EndIf
				Next
				If Not found Then
					impls.AddLast decl
				End If
				'EndIf
			Next
			
			For Local idecl:TClassDecl = EachIn cdecl.implments
				CheckInterface(idecl, impls)
			Next

			cdecl=cdecl.superClass
		Wend
	End Method
	
	Method GetFieldOffset(decl:TFieldDecl)
		
		Local ty:TType = decl.declTy
		
		Local modifier:Int = POINTER_SIZE
		
		If TIntType(ty) Or TFloatType(ty) Or TUIntType(ty) Then
			modifier = 4
		Else If TShortType(ty) Then
			modifier = 2
		Else If TLongType(ty) Or TDoubleType(ty) Or TULongType(ty) Then
			modifier = 8
		Else If TByteType(ty) Then
			modifier = 1
		Else If TSizeTType(ty) Then
			modifier = WORD_SIZE
		End If

		If modifier > 1 And lastOffset Mod modifier Then
			lastOffset :+ modifier - (lastOffset Mod modifier)
		End If
		
		decl.offset = lastOffset
		
		lastOffset :+ modifier
	End Method
	
	Method ImplementsInterface:Int(ident:String)
		ident = ident.ToLower()
		For Local iface:TClassDecl = EachIn implmentsAll
			If iface.IdentLower() = ident Then
				Return True
			End If
		Next
		
		' check hierarchy
		If superClass Then
			Return superClass.ImplementsInterface(ident)
		End If
		
		Return False
	End Method
	
	' returns a map of all interfaces implemented in this hierarchy
	Method GetInterfaces:TMap(map:TMap = Null)
		If Not map Then
			map = New TMap
		End If

		For Local iface:TClassDecl=EachIn implmentsAll
		
			If iface.IsInterface() Then
			
				Local cdecl:TClassDecl = iface
				While cdecl
				
					If cdecl.IsInterface() Then
						If Not map.Contains(cdecl) Then
							map.Insert(cdecl, cdecl)
						End If
					End If


					cdecl=cdecl.superClass
				Wend
			
			End If
		Next

		
		If superClass Then
			map = superClass.GetInterfaces(map)
		End If
		
		Return map
	End Method
	
	Method GetImplementedFuncs:TBCCObjectList(list:TBCCObjectList = Null)
		If Not list Then
			list = New TBCCObjectList
		End If
		
		For Local idecl:TClassDecl = EachIn implmentsAll
			idecl.GetImplementedFuncs(list)
		Next
		
		For Local decl:TFuncDecl = EachIn SemantedMethods()
			list.AddLast(decl)
		Next

		Return list
	End Method
	
End Type

Type TLoopLabelDecl Extends TDecl ' also used internally for Try constructs

	Field realIdent:String

	Method Create:TLoopLabelDecl( ident$, attrs:Long=0 )
		If Not ident.StartsWith("#") Then
			Self.ident="#" + ident
			Self.realIdent = ident
		Else
			Self.ident = ident
			Self.realIdent = ident[1..]
		End If
		Self.attrs=attrs
		Return Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Return New TLoopLabelDecl.Create( realIdent,attrs )
	End Method
	
	Method OnSemant()
	End Method
	
End Type

Type TDataLabelDecl Extends TDecl

	Field realIdent:String
	Field index:Int

	Method Create:TDataLabelDecl( ident$, attrs:Long=0 )
		If Not ident.StartsWith("#") Then
			Self.ident="#" + ident
			Self.realIdent = ident
		Else
			Self.ident = ident
			Self.realIdent = ident[1..]
		End If
		Self.attrs=attrs
		Return Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Return New TDataLabelDecl.Create( realIdent,attrs )
	End Method
	
	Method OnSemant()
	End Method
	
End Type

Type TDefDataDecl Extends TDecl

	Global count:Int

	Field label:TDataLabelDecl
	Field data:TExpr[]

	Method Create:TDefDataDecl(data:TExpr[], label:TDataLabelDecl = Null, attrs:Long=0 )
		Self.data=data
		Self.label=label
		Self.attrs=attrs
		Return Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Return New TDefDataDecl.Create(TExpr.CopyArgs(data),TDataLabelDecl(label.Copy()),attrs)
	End Method

	Method OnSemant()
		If data Then
			If label Then
				label.index = count
			End If
		
			For Local i:Int = 0 Until data.length
				data[i] = data[i].Semant()
				If Not TConstExpr(data[i]) Then
					Err "Data items must be numeric or strings"
				Else
					' todo : more type tests?
				End If
				count :+ 1
			Next
		Else
			' err?
		End If
	End Method
	
End Type

Type TTryStmtDecl Extends TBlockDecl
	Field tryStmt:TTryStmt
	
	Method ToString:String()
		Return "TTryStmtDecl"
	End Method
End Type

Type TEnumDecl Extends TScopeDecl
	Field ty:TType
	Field isFlags:Int
	Field values:TEnumValueDecl[]
	
	Method Create:TEnumDecl(id:String, ty:TType, isFlags:Int, values:TEnumValueDecl[])
		Self.ident = id
		Self.ty = ty
		Self.isFlags = isFlags
		Self.values = values
		Return Self
	End Method
	
	Method OnSemant()
		' validate type
		If Not TIntegralType(ty) Then
			Err "Invalid type '" + ty.ToString() + "'. Enums can only be declared as integral types."
		End If

		If values.Length = 0 Then
			Err "Enum '" + ident + "' must contain at least one value."
		End If
		
		For Local val:TEnumValueDecl = EachIn values
			val.scope = Self
			val.Semant()
		Next

		' prevent duplicate names
		If values.Length > 1 Then
			For Local i:Int = 0 Until values.Length
				For Local j:Int = i + 1 Until values.Length
					If values[i].IdentLower() = values[j].IdentLower() Then
						Err "Duplicate enum value name: " + values[i].Ident
					End If
				Next
			Next
		End If

		GenerateFuncs()
	End Method

	Method OnCopy:TDecl(deep:Int = True)
		Return New TEnumDecl.Create(ident, ty, isFlags, values)
	End Method
	
	Method GetDecl:Object( ident$ )
		For Local val:TEnumValueDecl = EachIn values
			If val.IdentLower() = ident And val.IsSemanted() Then
				Return val
			End If
		Next
		
		Return Super.GetDecl(ident)
	End Method
	
	Method CastsToEnum:Int(expr:TConstExpr)
		Local value:Long = StringToLong(expr.value)
		
		If isFlags Then
			' zero value special case
			If value = 0 Then
				For Local val:TEnumValueDecl = EachIn values
					If val.longValue = 0 Then
						Return True
					End If
				Next
				Return False
			End If
		
			Local result:Long
			For Local val:TEnumValueDecl = EachIn values
				value :~ val.longValue
			Next

			Return value = 0
		Else
			For Local val:TEnumValueDecl = EachIn values
				If value = val.longValue Then
					Return True
				End If
			Next
		End If
		
		Return False
	End Method
	
	Method GenerateFuncs()
		Local enumType:TEnumType = New TEnumType.Create(Self)
	
		Local fdecl:TFuncDecl = New TFuncDecl.CreateF("ToString", New TStringType, Null, FUNC_METHOD)
		InsertDecl fdecl
		fdecl.Semant()
		
		fdecl = New TFuncDecl.CreateF("Ordinal", ty, Null, FUNC_METHOD)
		InsertDecl fdecl
		fdecl.Semant()

		fdecl = New TFuncDecl.CreateF("Values", New TArrayType.Create(enumType, 1), Null, 0)
		InsertDecl fdecl
		fdecl.Semant()
		
		Local args:TArgDecl[2]
		args[0] = New TArgDecl.Create("value", ty, Null)
		args[1] = New TArgDecl.Create("result", TType.MapToVarType(enumType.Copy()), Null, 0)
		
		fdecl = New TFuncDecl.CreateF("TryConvert", New TIntType, args, 0)
		InsertDecl fdecl
		fdecl.Semant()

		' FromString, returning ordinal, or throws TIllegalArgumentException if not found
		args = New TArgDecl[1]
		args[0] = New TArgDecl.Create("name", New TStringType, Null)

		fdecl = New TFuncDecl.CreateF("FromString", enumType, args, 0)
		InsertDecl fdecl
		fdecl.Semant()

		If Not IsExtern() And Not IsImported() Then
			BuildDefaultComparatorCompare()
		End If
	End Method

	Method BuildDefaultComparatorCompare()
		Local enumType:TEnumType = New TEnumType.Create(Self)

		Local arg1:TArgDecl = New TArgDecl.Create("e1", enumType.Copy(), Null)
		Local arg2:TArgDecl = New TArgDecl.Create("e2", enumType.Copy(), Null)
		Local func:TFuncDecl = New TFuncDecl.CreateF("DefaultComparator_Compare", New TIntType, [arg1, arg2], 0)

		' 
		' If e1 < e2 Then Return -1
		' If e1 > e2 Then Return 1
		' Return 0
		'
		Local e1Var:TVarExpr = New TVarExpr.Create(arg1)
		Local e2Var:TVarExpr = New TVarExpr.Create(arg2)

		Local ifExpr:TExpr = New TBinaryCompareExpr.Create( "<",e1Var, e2Var )
		Local thenBlock:TBlockDecl=New TBlockDecl.Create( func, , BLOCK_IF )
		Local returnStmt:TReturnStmt = New TReturnStmt.Create( New TConstExpr.Create( New TIntType,"-1" ) )
		returnStmt.generated = True
		returnStmt.errInfo=errInfo
		thenBlock.AddStmt returnStmt
		func.AddStmt New TIfStmt.Create( ifExpr,thenBlock, Null )

		ifExpr = New TBinaryCompareExpr.Create( ">",e1Var, e2Var )
		thenBlock= New TBlockDecl.Create( func, , BLOCK_IF )
		returnStmt = New TReturnStmt.Create( New TConstExpr.Create( New TIntType,"1" ) )
		returnStmt.generated = True
		returnStmt.errInfo=errInfo
		thenBlock.AddStmt returnStmt
		func.AddStmt New TIfStmt.Create( ifExpr,thenBlock, Null )
		
		returnStmt = New TReturnStmt.Create( New TConstExpr.Create( New TIntType,"0" ) )
		returnStmt.generated = True
		returnStmt.errInfo=errInfo
		func.stmts.AddLast returnStmt
		
		ModuleScope().InsertDecl func
	End Method
	
	Method ToString:String()
		Return ident
	End Method
End Type

Type TEnumValueDecl Extends TDecl

	Field expr:TExpr
	Field index:Int
	Field longValue:Long
	
	Method Create:TEnumValueDecl(id:String, index:Int, expr:TExpr)
		Self.ident = id
		Self.index = index
		Self.expr = expr
		Return Self
	End Method

	Method OnSemant()
		Local parent:TEnumDecl = TEnumDecl(scope)
		Local previous:TEnumValueDecl
		If index > 0 Then
			previous = parent.values[index - 1]
		End If

		If expr Then

			If TConstExpr(expr) And Not TConstExpr(expr).ty.EqualsType(parent.ty) Then
				TConstExpr(expr).UpdateType(parent.ty)
			End If

			expr = expr.Semant()

			' 			
			If TIdentEnumExpr(expr) Then
				If TIdentEnumExpr(expr).value.scope = parent Then
					expr = New TConstExpr.Create(parent.ty, TIdentEnumExpr(expr).value.Value()).Semant()
				End If
			End If
			
			If parent.isFlags And TBinaryMathExpr(expr) Then
				expr = New TConstExpr.Create(parent.ty, TBinaryMathExpr(expr).Eval())
			End If
			
			If Not TConstExpr(expr) Or Not TIntegralType(TConstExpr(expr).ty) Then
				Err "Enum values must be integral constants."
			End If
			
			longValue = StringToLong(TConstExpr(expr).value)
		Else
			Local val:Long
			
			' initial flags value
			If index = 0 And parent.isFlags Then
				val = 1
			End If

			If previous Then
				'
				If TConstExpr(previous.expr)

					val = TConstExpr(previous.expr).value.ToLong()

					If parent.isFlags Then
						If val = 0 Then
							val = 1 
						Else
							If (val & (val - 1)) = 0 Then
								val :+ 1
							End If
							' find next power of 2

							Local res:Long
							bmx_enum_next_power(Asc(TypeCode(parent.ty)), val, res)

							If Not res Then
								Err "Flags out of bounds at '" + ident + "'."
							End If
							
							val = res
							
						End If
					Else
						val :+ 1
					End If
				Else
					InternalErr "TEnumValueDecl.OnSemant"
				End If
			End If

			expr = New TConstExpr.Create( parent.ty.Copy(), val).Semant()
			longValue = val
			
		End If
	End Method

	Method OnCopy:TDecl(deep:Int = True)
		Return New TEnumValueDecl.Create(ident, index, expr)
	End Method
	
	Method Value:String()
		Return TConstExpr(expr).value
	End Method

	Method TypeCode:String(ty:TType)
		If TByteType( ty ) Return "b"
		If TShortType( ty ) Return "s"
		If TIntType( ty ) Return "i"
		If TUIntType( ty ) Return "u"
		If TLongType( ty ) Return "l"
		If TULongType( ty ) Return "y"
		If TSizeTType( ty ) Return "t"
		If TLongIntType( ty ) Return "v"
		If TULongIntType( ty ) Return "e"
	End Method
	
	Method ToString:String()
		Return "TEnumValueDecl"
	End Method
End Type

Const MODULE_STRICT:Int=1
Const MODULE_SUPERSTRICT:Int=2
Const MODULE_ACTUALMOD:Int=4
Const MODULE_FRAMEWORK:Int=8
Const MODULE_MODULE:Int=16

Type TNamespaceDecl Extends TScopeDecl

'	Field mods:TMap = New TMap

	Method Create:TNamespaceDecl( ident$,munged$ )
		Self.ident=ident
		Self.munged=munged
		Return Self
	End Method

'	Method GetDecl:Object( ident$ )
'		Return mods.ValueForKey(ident.ToLower())
'	End Method
	
End Type

Type TModuleDecl Extends TScopeDecl

	Field filepath$
	Field relpath$
	Field imported:TUnorderedMap=New TUnorderedMap'<TModuleDecl>		'Maps filepath to modules
	Field pubImported:TUnorderedMap =New TUnorderedMap'<TModuleDecl>	'Ditto for publicly imported modules

	Field pmod:TModuleDecl

	Field fileImports:TBCCObjectList=New TBCCObjectList'StringList
	
	' cache of ModuleInfo lines
	Field modInfo:TBCCObjectList = New TBCCObjectList
	' cache of pragma lines
	Field pragmas:TBCCObjectList = New TBCCObjectList

	Field _getDeclTreeCache:TBCCObjectList
	
	Field _getDeclCache:TMap = New TMap
	Field _getDeclListCache:TMap = New TMap

	Method ToString$()
		Return "Module "+munged
	End Method
	
	Method Create:TModuleDecl( ident$,munged$,filepath$,attrs:Long )
		Self.ident=ident
		Self.munged=munged
		Self.filepath=filepath
		Self.attrs=attrs

		If ident.Find(".") <> -1 And ident.find("/") = -1 And ident.find("\") = -1 Then
			Local m:String = ident[..ident.Find(".")]
			Local ns:TNamespaceDecl = TNamespaceDecl(_appInstance.GetDecl(m.ToLower()))
			If Not ns Then
				ns = New TNamespaceDecl.Create(m, m)
				If _appInstance.mainModule Then
					_appInstance.mainModule.InsertDecl(ns)
				Else
					' this must be the main module...
					InsertDecl(ns)
				End If
			End If
			ns.InsertDecl(Self)
		End If

		Return Self
	End Method

	Method UpdateFilePath(fp:String)
		filepath = fp
	End Method
	
	Method AddImport(imp:String, obj:Object)
		imported.Insert(imp, obj)
		FlushCaches()
	End Method
	
	Method FlushCaches()
		_getDeclTreeCache = Null
		If TModuleDecl(pmod) Then
			TModuleDecl(pmod).FlushCaches()
		End If
	End Method
	
	Method IsStrict:Int()
		Return (attrs & MODULE_STRICT)<>0
	End Method

	Method IsSuperStrict:Int()
		Return (attrs & MODULE_SUPERSTRICT)<>0
	End Method
	
	Method IsActualModule:Int()
		Return (attrs & MODULE_ACTUALMOD)<>0
	End Method
	
	Method GetDecl:Object( ident$ )
		' if we previously found it, return it from the cache
		Local decl:Object = _getDeclCache.ValueForKey(ident)

		If decl Then
			Return decl
		End If
		
		If _getDeclTreeCache Then
		
			Local declmod$
		
			For Local mdecl:TModuleDecl = EachIn _getDeclTreeCache

				If ident = mdecl.ident And mdecl <> Self
					_getDeclCache.Insert(ident, mdecl)
					Return mdecl
				End If
			
				Local tdecl_:Object=mdecl.GetDecl2( ident )
			
				If tdecl_ And tdecl_<>decl
					If mdecl=Self
						_getDeclCache.Insert(ident, tdecl_)
						Return tdecl_
					End If
					If decl
						Err "Duplicate identifier '"+ident+"' found in module '"+declmod+"' and module '"+mdecl.ident+"'."
					EndIf
					decl=tdecl_
					declmod=mdecl.ident
				EndIf
			Next
		
		Else
		
			_getDeclTreeCache = New TBCCObjectList
	
			Local todo:TBCCObjectList=New TBCCObjectList'<TModuleDecl>
			'Local done:TIntMap=New TIntMap'<TModuleDecl>
			Local done:TMap = New TMap
			
			todo.AddLast Self
			'done.Insert _filePathId,Self
			done.Insert filePath,Self
			
			Local declmod$
			
			While Not todo.IsEmpty()
		
				Local mdecl:TModuleDecl=TModuleDecl(todo.RemoveLast())
				
				_getDeclTreeCache.AddLast(mdecl)
				
				Local imps:TUnorderedMap=mdecl.imported
	
				For Local mdecl2:TModuleDecl=EachIn imps.Values()
	
					'If Not done.Contains( mdecl2._filePathId )
					If Not done.Contains( mdecl2.filePath )
						todo.AddLast mdecl2
						'done.Insert mdecl2._filePathId,mdecl2
						done.Insert mdecl2.filePath,mdecl2
					EndIf
					
				Next
	
			Wend
	
			Return GetDecl(ident)
	
		End If
			
		' cache it for next time
		_getDeclCache.Insert(ident, decl)
		
		Return decl
	End Method
	
	Method GetDecl2:Object( ident$ )
		Return Super.GetDecl( ident )
	End Method


	Method GetDeclList:Object( ident$, declList:TFuncDeclList = Null, maxSearchDepth:Int )

		If Not declList Then
			declList = New TFuncDeclList
		End If

		Local decl:Object,declmod$

		If _getDeclTreeCache Then
		
'			Print "   Using Cache"
			
			Local declmod$
		
			For Local mdecl:TModuleDecl = EachIn _getDeclTreeCache
			
				If ident = mdecl.ident
					'_getDeclCache.Insert(identId, mdecl)
					Return mdecl
				End If
			
				Local tdecl_:Object=mdecl.GetDeclList2( ident, declList, maxSearchDepth )
			
				If tdecl_ And tdecl_<>decl
					If mdecl=Self
						_getDeclCache.Insert(ident, tdecl_)
						Return tdecl_
					End If
					If decl
						Err "Duplicate identifier '"+ident+"' found in module '"+declmod+"' and module '"+mdecl.ident+"'."
					EndIf
					decl=tdecl_
					declmod=mdecl.ident
				EndIf
			Next
		
		Else

			_getDeclTreeCache = New TBCCObjectList
	
			Local todo:TBCCObjectList=New TBCCObjectList'<TModuleDecl>
			Local done:TMap=New TMap'<TModuleDecl>
			
			todo.AddLast Self
			done.Insert filepath,Self
			
			'Local decl:Object,declmod$
			
			While Not todo.IsEmpty()
		
				Local mdecl:TModuleDecl=TModuleDecl(todo.RemoveLast())
				_getDeclTreeCache.AddLast(mdecl)
				
				Local imps:TUnorderedMap=mdecl.imported
	
				For Local mdecl2:TModuleDecl=EachIn imps.Values()
					If Not done.Contains( mdecl2.filepath )
						todo.AddLast mdecl2
						done.Insert mdecl2.filepath,mdecl2
					EndIf
					
				Next
	
			Wend
		
			Return GetDeclList( ident, declList, maxSearchDepth )
		
		End If
		
		Return decl
	End Method
	
	Method GetDeclList2:Object( ident$, declList:TFuncDeclList = Null, maxSearchDepth:Int )
		Return Super.GetDeclList( ident, declList, maxSearchDepth )
	End Method

	Method OnSemant()
		Local decl:TFuncDecl = FindFuncDecl( "__localmain", ,,,,,SCOPE_MODULE )
		If decl Then
			decl.Semant
		End If
	
		For Local gdecl:TGlobalDecl=EachIn _decls
			gdecl.Semant
		Next

		For Local cdecl:TClassDecl=EachIn _decls
			If cdecl.args Then
				For Local inst:TClassDecl = EachIn cdecl.instances
					For Local idecl:TDecl = EachIn inst.Decls()
						If TAliasDecl( idecl ) Continue
						idecl.Semant()
					Next
				Next
			Else
				cdecl.Semant
			End If
		Next

		For Local fdecl:TFuncDecl=EachIn _decls
			fdecl.Semant
		Next

		For Local cdecl:TConstDecl=EachIn _decls
			cdecl.Semant
		Next
	End Method

End Type

Type TAppDecl Extends TScopeDecl

	Field imported:TUnorderedMap=New TUnorderedMap'<TModuleDecl>			'maps modpath->mdecl
	
	Field globalImports:TUnorderedMap = New TUnorderedMap
	
	Field mainModule:TModuleDecl
	Field mainFunc:TFuncDecl	
		
	Field semantedClasses:TBCCObjectList=New TBCCObjectList'<TClassDecl>			'in-order (ie: base before derived) list of _semanted classes
	Field semantedGlobals:TBCCObjectList=New TBCCObjectList'<TGlobalDecl>			'in-order (ie: dependancy sorted) list of _semanted globals

	Field fileImports:TBCCObjectList=New TBCCObjectList'StringList
	Field headers:TBCCObjectList = New TBCCObjectList
	
	Field stringConsts:TMap = New TMap
	Field stringConstCount:Int
	
	Field incbins:TBCCObjectList = New TBCCObjectList
	Field genIncBinHeader:Int = False
	
	Field dataDefs:TBCCObjectList = New TBCCObjectList
	Field scopeDefs:TMap = New TMap
	
	Field exportDefs:TBCCObjectList = New TBCCObjectList
	
	Method GetPathPrefix:String()
		If opt_buildtype = BUILDTYPE_MODULE Then
			Local prefix:String
			Local path:String[] = mainModule.filepath.split("/")
			Local c:Int = 0
			For Local dir:String = EachIn path
				If c Then
					prefix :+ dir.Replace(".mod", "") + "_"
					c:- 1
				End If
				If dir = "mod" Then
					c = 2
				End If
			Next
		Else
			Return "bb_"
		End If
	End Method
	
	Method InsertModule( mdecl:TModuleDecl )
		mdecl.scope=Self
		imported.Insert mdecl.filepath,mdecl
		If Not mainModule
			mainModule=mdecl
		EndIf
	End Method
	
	Method IsImported:Int(modpath:String)
		Return globalImports.Contains(modpath)
	End Method
	
	Method GetDecl:Object( ident$ )
		Local obj:Object = Super.GetDecl(ident)
		If Not obj And mainModule Then
			Return mainModule.GetDecl(ident)
		End If
		Return obj
	End Method

	Method OnSemant()
'DebugStop		
		_env=Null
		pushenv Self
	
		SemantImports()

		SemantDataDefs()	

		mainModule.Semant

		mainFunc=mainModule.FindFuncDecl( "__localmain",,,,,,SCOPE_MODULE )
		
		
		' FIXME
		If Not mainFunc Err "Function 'Main' not found."
	
		SemantDecls()

		Repeat
			Local more:Int
			For Local cdecl:TClassDecl=EachIn semantedClasses
				more:+cdecl.UpdateLiveMethods()
			Next
			If Not more Exit
		Forever
		
		For Local cdecl:TClassDecl=EachIn semantedClasses
			cdecl.FinalizeClass
		Next
	End Method
	
	Method SemantImports()
		For Local decl:TModuleDecl = EachIn globalImports.Values()
			For Local cdecl:TClassDecl = EachIn decl._decls
				If Not cdecl.IsSemanted() Then
					cdecl.Semant()
					cdecl.SemantParts()
				End If
			Next
		Next
	End Method
	
	Method SemantDataDefs()
		TDefDataDecl.count = 0
		
		For Local decl:TDecl = EachIn dataDefs
			decl.Semant
		Next
	End Method
	
	Method SemantDecls()
		For Local decl:TDecl=EachIn mainModule._decls

			decl.Semant
			
			' consts
			Local cdecl:TConstDecl=TConstDecl( decl )
			If cdecl
				cdecl.Semant()
				Continue
			End If

			' classes
			Local tdecl:TClassDecl=TClassDecl( decl )
			If tdecl
				tdecl.Semant()
				tdecl.SemantParts()
				Continue
			EndIf

			' functions
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl And fdecl <> _appInstance.mainFunc Then
				fdecl.Semant()
				Continue
			End If

			' globals
			Local gdecl:TGlobalDecl=TGlobalDecl( decl )
			If gdecl
				gdecl.Semant()
				Continue
			End If
		Next

	End Method
	
	Method hasStringConst:Int(value:String)
		Return stringConsts.ValueForKey(value) <> Null
	End Method
	
	Method mapStringConsts(value:String)
		Local sc:TStringConst = TStringConst(stringConsts.ValueForKey(value))
		
		If Not sc Then
			Local sc:TStringConst = New TStringConst
			sc.count = 1
		
			If value Then
				sc.id = "_s" + stringConstCount
			Else
				sc.id = "bbEmptyString"
			End If

			stringConsts.Insert(value, sc)

			If value Then
				stringConstCount:+ 1
			End If
		Else
			sc.count :+ 1
		End If
	End Method
	
	Method removeStringConst(value:String)
		If value Then
			Local sc:TStringConst = TStringConst(stringConsts.ValueForKey(value))
			If sc Then
				If sc.count > 0 Then
					sc.count :- 1
					'stringConsts.Remove(value)
				End If
			End If
		End If
	End Method
	
	Method FindDataLabel:TDecl(ident:String)
		For Local dd:TDefDataDecl = EachIn dataDefs
			If dd.label And dd.label.ident.ToLower() = ident.ToLower() Then
				Return dd
			End If
		Next
	End Method
	
End Type

Type TStringConst

	Field id:String
	Field count:Int
	Field used:Int

End Type

Type TTemplateDets

	Field instArgs:TType[]
	Field args:TTemplateArg[]

	Method Create:TTemplateDets(instArgs:TType[], args:TTemplateArg[])
		Self.instArgs = instArgs
		Self.args = args
		Return Self
	End Method

End Type

Type TGenProcessor Abstract

	Global processor:TGenProcessor

	Method ParseGeneric:Object(templ:TTemplateRecord, dets:TTemplateDets)
	End Method
	
End Type

Type TCastDets

	Field name:String
	Field retType:String
	Field noGen:Int
	Field args:String[0]
	Field api:String

	Method TransCast:String()
		Local s:String = "(" + retType + " (*)"

		s :+ "("
		For Local i:Int = 0 Until args.length
			s :+ args[i]
			If i < args.length - 1 Then
				s :+ ","
			End If
		Next
		s :+ ")"

		s :+ ")"
		Return s
	End Method
	
End Type
