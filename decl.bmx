' Copyright (c) 2013-2015 Bruce A Henderson
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

Const DECL_EXTERN:Int=		$010000
Const DECL_PRIVATE:Int=	    $020000
Const DECL_ABSTRACT:Int=	$040000
Const DECL_FINAL:Int=		$080000

Const DECL_SEMANTED:Int=	$100000
Const DECL_SEMANTING:Int=	$200000

Const DECL_POINTER:Int=	$400000

Const DECL_ARG:Int=     $800000
Const DECL_INITONLY:Int=$1000000

Const DECL_NODEBUG:Int=$2000000

Const CLASS_INTERFACE:Int=	$001000
Const CLASS_THROWABLE:Int=	$002000

Global _env:TScopeDecl
Global _envStack:TList=New TList

Global _appInstance:TAppDecl

Global _loopnest:Int

Function PushEnv( env:TScopeDecl )
	If _env _envStack.AddLast( _env )
	_env=env
End Function

Function PopEnv()
	_env=TScopeDecl( _envStack.RemoveLast() )
End Function

Type TFuncDeclList Extends TList
End Type

Type TDecl
	Field ident$
	Field munged$
	Field errInfo$
	Field actual:TDecl
	Field scope:TScopeDecl
	Field attrs:Int
	Field metadata:String
	
	Field declImported:Int = False
	Field generated:Int
	
	Field _identLower:String
	
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
	
	Method IsExtern:Int()
		Return (attrs & DECL_EXTERN)<>0
	End Method
	
	Method IsPrivate:Int()
		Return (attrs & DECL_PRIVATE)<>0
	End Method
	
	Method IsAbstract:Int()
		Return (attrs & DECL_ABSTRACT)<>0
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
	
	Method CheckAccess:Int()
		If IsPrivate() And ModuleScope()<>_env.ModuleScope() Return False
		Return True
	End Method
	
	Method AssertAccess()
		If Not CheckAccess()
			Err ToString() +" is private."
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

		If IsSemanting() Err "Cyclic declaration of '"+ident+"'."
		
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

				scope._semanted.AddLast Self
				
				If TGlobalDecl( Self )
					' FIXME
					If AppScope() Then
						AppScope().semantedGlobals.AddLast TGlobalDecl( Self )
					End If
				EndIf
				
				If TModuleDecl( scope )
'DebugStop
					' FIXME
					Local app:TAppDecl = AppScope()
					If app Then
						app._semanted.AddLast Self
					End If
				EndIf
			
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
		InternalErr
	End Method
	
	Method OnSemant() Abstract

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

	Method CopyInit:TExpr()
		If init Return init.Copy()
	End Method
	
	Method OnSemant()

		If declTy
			' ensure to set the scope for a function pointer array before semanting
			If TArrayType(declTy) And TFunctionPtrType(TArrayType(declTy).elemType) Then
				If Not TFunctionPtrType(TArrayType(declTy).elemType).func.scope Then
					If scope Then
						TFunctionPtrType(TArrayType(declTy).elemType).func.scope = scope
					Else
						TFunctionPtrType(TArrayType(declTy).elemType).func.scope = _env
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
			InternalErr
		EndIf
		
	End Method
	
	Method SemantInit()
		' for field initialisation, create a stub New() method to use as current scope
		' since fields are initialised in New(). Otherwise the scope would be "class", which is
		' incorrect for processing field inits.
		If TFieldDecl(Self) And declInit Then
			Local newScope:TFuncDecl = New TFuncDecl.CreateF( "new", Null,Null,FUNC_METHOD )
			newScope.scope = _env
			PushEnv(newScope)
		End If
	
		If declTy
			If declInit Then
				If TFunctionPtrType(ty) Then
					
					' the default munged function value as defined in the interface
					If TInvokeExpr(declInit) Then
						init = declInit.Copy()
					Else If TConstExpr(declInit) Then
						init = declInit.Copy().Semant()
					Else
						Local expr:TExpr
						
						If TFuncCallExpr(declInit) Then
							expr=declInit.Copy().Semant()
						Else If TNullExpr(declInit) Then
							expr = declInit
						Else
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
						End If
						
						If expr.exprType.EqualsType( ty ) Then
							init = expr
						Else
							init = New TCastExpr.Create( ty,expr,CAST_EXPLICIT ).Semant()
						End If
					End If
					
					
				Else
					init=declInit.Copy().SemantAndCast(ty)
				End If
			End If
		Else If declInit
			init=declInit.Copy().Semant()
			ty=init.exprType
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
	
End Type

Type TConstDecl Extends TValDecl
	Field value$
	
	Method Create:TConstDecl( ident$,ty:TType,init:TExpr,attrs:Int )
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
		Return New TConstDecl.Create( ident,ty,CopyInit(), attrs )
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

	Method Create:TLocalDecl( ident$,ty:TType,init:TExpr,attrs:Int=0, generated:Int = False )
		Self.ident=ident
		Self.declTy=ty
		Self.declInit=init
		Self.attrs=attrs
		Self.generated=generated
		Return Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Return New TLocalDecl.Create( ident,ty,CopyInit(),attrs, generated )
	End Method
	
	Method ToString$()
		Return "Local "+Super.ToString()
	End Method

End Type

Type TArgDecl Extends TLocalDecl

	Field castTo:String
	
	Method Create:TArgDecl( ident$,ty:TType,init:TExpr,attrs:Int=0, generated:Int = False )
		Self.ident=ident
		Self.declTy=ty
		Self.declInit=init
		Self.attrs=attrs
		Self.generated=generated
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
		Local d:TArgDecl = New TArgDecl.Create( ident,ty,CopyInit(),attrs,generated )
		d.ty = d.declTy
		d.init = d.declInit
		Return d
	End Method
	
	Method ToString$()
		Return Super.ToString()
	End Method

	
End Type

Type TGlobalDecl Extends TVarDecl

	Field inited:Int
	
	Method Create:TGlobalDecl( ident$,ty:TType,init:TExpr,attrs:Int=0 )
		Self.deferInit = True
		Self.ident=ident
		Self.declTy=ty
		Self.declInit=init
		Self.attrs=attrs
		Return Self
	End Method

	Method OnCopy:TDecl(deep:Int = True)
		Return New TGlobalDecl.Create( ident,ty,CopyInit(),attrs )
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
	
End Type

Type TFieldDecl Extends TVarDecl

	' location offset in object variable data
	Field offset:Int

	Method Create:TFieldDecl( ident$,ty:TType,init:TExpr,attrs:Int=0 )
		Self.ident=ident
		Self.declTy=ty
		Self.declInit=init
		Self.attrs=attrs
		Return Self
	End Method

	Method OnCopy:TDecl(deep:Int = True)
		Local f:TFieldDecl = New TFieldDecl.Create( ident,ty,CopyInit(),attrs )
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
	
End Type

Type TAliasDecl Extends TDecl

	Field decl:Object
	
	Method Create:TAliasDecl( ident$,decl:Object,attrs:Int=0 )
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
	
End Type

Type TScopeDecl Extends TDecl

'Private

	Field _decls:TList=New TList'<TDecl>
	Field _semanted:TList=New TList'<TDecl>

	Field declsMap:TMap=New TMap'<Object>

'Public

	Method OnCopy:TDecl(deep:Int = True)
		InternalErr
	End Method

	Method Decls:TList()
		Return _decls
	End Method
	
	Method Semanted:TList()
		Return _semanted
	End Method
	
	Method FuncDecls:TList( id$="" )
		Local fdecls:TList=New TList
		For Local decl:TDecl=EachIn _decls
			If id And decl.ident<>id Continue
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl fdecls.AddLast fdecl
		Next
		Return fdecls
	End Method
	
	Method MethodDecls:TList( id$="" )
		Local fdecls:TList=New TList
		For Local decl:TDecl=EachIn _decls
			If id And decl.ident<>id Continue
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl And fdecl.IsMethod() fdecls.AddLast fdecl
		Next
		Return fdecls
	End Method
	
	Method SemantedFuncs:TList( id$="" )
		Local fdecls:TList=New TList
		For Local decl:TDecl=EachIn _semanted
			If id And decl.ident<>id Continue
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl fdecls.AddLast fdecl
		Next
		Return fdecls
	End Method
	
	Method SemantedMethods:TList( id$="" )
		Local fdecls:TList=New TList
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
	
	Method InsertDecl( decl:TDecl )

		If decl.scope And Not (decl.attrs & DECL_INITONLY) InternalErr
		
		'Local ident$=decl.ident
		If Not decl.ident Return
		
		If Not decl.scope Then
			decl.scope=Self
		End If
		_decls.AddLast decl

		'Local _decls:TMap
		Local tdecl_:Object=declsMap.ValueForKey( decl.IdentLower() )
		
		'If TFuncDecl( decl )
		'	Local funcs:TFuncDeclList=TFuncDeclList( tdecl_ )
		'	If funcs Or Not tdecl_
		'		If Not funcs
		'			funcs=New TFuncDeclList
		'			declsMap.Insert ident.ToLower(),funcs
		'		EndIf
		'		funcs.AddLast TFuncDecl( decl )
		'	Else
		'		Err "Duplicate identifier '"+ident+"'."
		'	EndIf
		'Else
		If Not tdecl_
'DebugLog "Adding " + decl.ident
			declsMap.Insert decl.IdentLower(),decl
		Else
			Err "Duplicate identifier '"+ident+"'."
		EndIf

	End Method

	Method InsertDecls( _decls:TList )
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
	

	Method FindDecl:Object( ident$, override:Int = False )
	
		If Not override And _env<>Self Return GetDecl( ident )
		
		Local tscope:TScopeDecl=Self
		While tscope
			Local decl:TDecl=TDecl(tscope.GetDecl( ident ))
			If decl Return decl
			tscope=tscope.scope
		Wend
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
		
		If Not decl Then
			' try scope search
			decl = TValDecl( FindDecl( ident, True ) )
			
			If Not decl Then
				' didn't find it? Maybe it is in module local scope?
				' issue arises when a global initialises with a local variable in the module scope.
				Local fdecl:TFuncDecl = TFuncDecl(FindDecl("__localmain", True))
				If fdecl Then
					decl = TValDecl( fdecl.FindDecl( ident ) )
					
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

	Method FindType:TType( ident$,args:TType[] )
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
				cdecl=cdecl.GenClassInstance( args )
				cdecl.Semant
				Return cdecl.objectType
			EndIf
		EndIf
		If scope Return scope.FindType( ident,args )
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
		If Not decl.declImported Then
			decl.Semant
		End If
		Return decl
	End Method
	
	Method FindFuncDecl:TFuncDecl( ident$,argExprs:TExpr[] = Null,explicit:Int=False, isArg:Int = False, isIdentExpr:Int = False )
'DebugLog "FindFuncDecl : " + ident
'If ident = "FixPath" Then DebugStop
		'Local funcs:TFuncDeclList=TFuncDeclList( FindDecl( ident ) )
		Local f:TDecl = TDecl(findDecl(ident))
		If Not f Then Return Null
		
		Local func:TFuncDecl = TFuncDecl(f)
		If Not func Then
			If TVarDecl(f) Then
				If Not f.IsSemanted() Then
					f.Semant()
				End If
				If TFunctionPtrType(TVarDecl(f).ty) Then
					func = TFunctionPtrType(TVarDecl(f).ty).func
					If Not func.scope Then
						func.scope = f.scope
					End If
					If Not func.ident Then
						func.ident = f.ident
					End If
				End If
			End If
		End If
		If Not func Return Null

		If Not argExprs
			argExprs = New TExpr[0]
		End If
	
		'For Local func:TFuncDecl=EachIn funcs
			func.Semant()
		'Next
		
		Local match:TFuncDecl,isexact:Int
		Local _err$
'DebugStop
		'For Local func:TFuncDecl=EachIn funcs
		While True
			If Not func.CheckAccess() Exit
			
			Local argDecls:TArgDecl[]=func.argDecls
			
			If argExprs.Length>argDecls.Length Exit
			
			Local exact:Int=True
			Local possible:Int=True
			
			' we found a matching name - this is probably the one we mean...
			If isArg Then
				match=func
				Exit
			End If
			
			For Local i:Int=0 Until argDecls.Length

				If i<argExprs.Length And argExprs[i]
				
					Local declTy:TType=argDecls[i].ty
					Local exprTy:TType=argExprs[i].exprType

					If TFunctionPtrType(declTy) And TInvokeExpr(argExprs[i]) Then
						If TFunctionPtrType(declTy).equalsDecl(TInvokeExpr(argExprs[i]).decl) Continue
					End If

					' not ideal - since the arg is configured as a Byte Ptr, we can't check that the function is of the correct type.
					If IsPointerType(declTy, TType.T_BYTE) And TInvokeExpr(argExprs[i]) And TInvokeExpr(argExprs[i]).invokedWithBraces = 0 Then
						Continue
					End If
					
					If TFunctionPtrType(declTy) And IsPointerType(exprTy, TType.T_BYTE) Then
						Continue
					End If
					
					If exprTy.EqualsType( declTy ) Continue
					
					exact=False
					
					If Not explicit And exprTy.ExtendsType( declTy ) Continue

				Else If Not argDecls[i].init

					If (func.attrs & FUNC_PTR) Or isIdentExpr Then
						exact=False
						Exit
					End If

					' if this argument is missing and there isn't a default...
					Err "Missing function parameter '" + argDecls[i].ident + "'"

				Else ' for case of argdecls having default args
					exact=False
					If Not explicit Exit
				EndIf
			
				possible=False
				Exit
			Next
			
			If Not possible Exit
			
			If exact
				If isexact
					Err "Unable to determine overload to use: "+match.ToString()+" or "+func.ToString()+"."
				Else
					_err=""
					match=func
					isexact=True
				EndIf
			Else
				If Not isexact
					If match 
						_err="Unable to determine overload to use: "+match.ToString()+" or "+func.ToString()+"."
					Else
						match=func
					EndIf
				EndIf
			EndIf
			Exit
		Wend
		
		If Not isexact
			If _err Err _err
			If explicit Return Null
		EndIf

		' last try... maybe we are trying to use it as a function pointer? (no args)
		If Not match Then
			If func And Not argExprs Then
				match = func
				match.maybeFunctionPtr = True
			End If
		Else If Not argExprs Then
			' if there are no args, the actual function may have none either... so we may still be trying to use it as a function pointer
			match.maybeFunctionPtr = True
		End If
		
		If Not match
			Local t$
			For Local i:Int=0 Until argExprs.Length
				If t t:+","
				If argExprs[i] t:+argExprs[i].exprType.ToString()
			Next
			Err "Unable to find overload for "+ident+"("+t+")."
		EndIf
		
		match.AssertAccess

		Return match
	End Method
	
	Method FindLoop:TStmt(ident:String = Null)

		If TBlockDecl(Self) And TBlockDecl(Self).extra Then
			Local loop:TLoopStmt = TLoopStmt(TBlockDecl(Self).extra)
			If ident Then
				If loop.loopLabel And loop.loopLabel.ident = ident Then
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
	
	Method OnSemant()
	End Method
	
End Type

Type TBlockDecl Extends TScopeDecl
	Field stmts:TList=New TList
	Field extra:Object
	
	Method Create:TBlockDecl( scope:TScopeDecl, generated:Int = False )
		Self.scope=scope
		Self.generated = generated
		
		attrs :| (scope.attrs & DECL_NODEBUG)
		
		Return Self
	End Method
	
	Method AddStmt( stmt:TStmt )
		stmts.AddLast stmt
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Local t:TBlockDecl=New TBlockDecl
		If deep Then
			For Local stmt:TStmt=EachIn stmts
				t.AddStmt stmt.Copy( t )
			Next
		End If
		t.extra = extra
		t.generated = generated
		Return t
	End Method

	Method OnSemant()
		PushEnv Self
		For Local stmt:TStmt=EachIn stmts
			stmt.Semant
		Next
		PopEnv
	End Method

	Method CopyBlock:TBlockDecl( scope:TScopeDecl )
		Local t:TBlockDecl=TBlockDecl( Copy() )
		t.scope=scope
		Return t
	End Method

End Type

Const FUNC_METHOD:Int=   $0001			'mutually exclusive with ctor
Const FUNC_CTOR:Int=     $0002
Const FUNC_PROPERTY:Int= $0004
Const FUNC_PTR:Int=      $0100
Const FUNC_BUILTIN:Int = $0080
Const FUNC_INIT:Int =    $0200
Const FUNC_NESTED:Int =  $0400

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
	
	Field maybeFunctionPtr:Int
	
	Field returnTypeSubclassed:Int
	
	Method CreateF:TFuncDecl( ident$,ty:TType,argDecls:TArgDecl[],attrs:Int )
		Self.ident=ident
		Self.retTypeExpr=ty
		If argDecls
			Self.argDecls=argDecls
		Else
			Self.argDecls = New TArgDecl[0]
		End If
		Self.attrs=attrs
		Return Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		Local args:TArgDecl[]=argDecls[..]
		For Local i:Int=0 Until args.Length
			args[i]=TArgDecl( args[i].Copy() )
		Next
		Local t:TFuncDecl=New TFuncDecl.CreateF( ident,retType,args,attrs )
		If deep Then
			For Local stmt:TStmt=EachIn stmts
				t.AddStmt stmt.Copy( t )
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
	
	Method IsBuiltIn:Int()
		Return (attrs & FUNC_BUILTIN)<>0
	End Method
	
	Method IsCtor:Int()
		Return (attrs & FUNC_CTOR)<>0
	End Method

	Method IsMethod:Int()
		Return (attrs & FUNC_METHOD)<>0
	End Method
	
	Method IsStatic:Int()
		Return (attrs & (FUNC_METHOD|FUNC_CTOR))=0
	End Method
	
	Method IsProperty:Int()
		Return (attrs & FUNC_PROPERTY)<>0
	End Method
	
	Method EqualsArgs:Int( decl:TFuncDecl )
		If argDecls.Length<>decl.argDecls.Length Return False
		For Local i:Int=0 Until argDecls.Length
			If Not argDecls[i].ty.EqualsType( decl.argDecls[i].ty ) Return False
		Next
		Return True
	End Method

	Method EqualsFunc:Int( decl:TFuncDecl )
		Return (retType.EqualsType( decl.retType ) Or retType.ExtendsType( decl.retType ) Or decl.retType.EqualsType( retType )) And EqualsArgs( decl )
	End Method

	Method OnSemant()
		'semant ret type
		If Not retTypeExpr Then
			If Not retType Then ' may have previously been set (if this is a function pointer)
				retType = TType.voidType
			Else If TIdentType(retType)
				retType = retType.Semant()
			End If
		Else
			retType=retTypeExpr.Semant()
		End If
		
		If TArrayType( retType ) And Not retType.EqualsType( retType.ActualType() )
'			Err "Return type cannot be an array of generic objects."
		EndIf
		
		'semant args
		For Local arg:TArgDecl=EachIn argDecls
			InsertDecl arg
			arg.Semant
		Next

		' if we are a function pointer declaration, we just want to semant the args here.
		If attrs & FUNC_PTR Return

		If actual<>Self Return
		
		'check for duplicate decl
		If ident Then
			For Local decl:TFuncDecl=EachIn scope.SemantedFuncs( ident )
				If decl<>Self And EqualsArgs( decl )
					Err "Duplicate declaration "+ToString()
				EndIf
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
		
		'append a return statement if necessary
		If Not IsExtern() And Not TVoidType( retType ) And Not TReturnStmt( stmts.Last() )
			If Not isCtor() And Not (isMethod() And IdentLower() = "delete") 
				Local stmt:TReturnStmt
			'If IsCtor()
			'	stmt=New TReturnStmt.Create( Null )
			'Else
				stmt=New TReturnStmt.Create( New TConstExpr.Create( retType,"" ) )
				stmt.generated = True
			'EndIf
				stmt.errInfo=errInfo
				stmts.AddLast stmt
			End If
		EndIf

		'check we exactly match an override
		If sclass 'And IsMethod()
'DebugStop
'DebugLog ident + "..."
			While sclass
				Local errorDetails:String = ""
'DebugLog "Checking Class : " + sclass.ident
				Local found:Int
				For Local decl:TFuncDecl=EachIn sclass.FuncDecls( )
					'If Not decl.IsSemanted() Then
					'	decl.Semant
					'End If
					
					If decl.IdentLower() = IdentLower() Then
'DebugLog "Method = " + decl.ident
					
						If IdentLower() = "new" Continue
						If IdentLower() = "delete" Continue
'If ident = "Create" DebugStop
						found=True

						If Not decl.IsSemanted() Then
							decl.Semant
						End If

						If EqualsFunc( decl ) 
'DebugLog "Found"
							If Not retType.EqualsType( decl.retType ) And retType.ExtendsType( decl.retType ) Then
								returnTypeSubclassed = True
							End If
							
							overrides=TFuncDecl( decl.actual )
						'If overrides.munged
						'	If munged And munged<>overrides.munged
						'		InternalErr
						'	EndIf
						'	munged=overrides.munged
						'EndIf
						Else
							'prepare a more detailed error message
							If (Not retType.EqualsType( decl.retType ) Or Not retType.ExtendsType( decl.retType )) Or (decl.retType And Not decl.retType.EqualsType( retType ))
								errorDetails :+ "Return type is ~q"+retType.ToString()+"~q, expected ~q"+decl.retType.ToString()+"~q. "
							End If

							If argDecls.Length <> decl.argDecls.Length
								errorDetails :+ "Argument count differs. Got " + argDecls.Length +", expected " + decl.argDecls.Length + " arguments."
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
				Next
				If found
					If Not overrides Err "Overriding method does not match any overridden method. (Detail: " + errorDetails+")"
					' for overrides, make the ident match that of the superclass
					ident = overrides.ident
					
					Exit
				EndIf
				sclass=sclass.superClass
			Wend
		EndIf
'If ident = "OnDebugStop" DebugStop
		attrs:|DECL_SEMANTED
		
		Super.OnSemant()
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

Type TClassDecl Extends TScopeDecl

	Field lastOffset:Int

	Field args:TClassDecl[]
	Field superTy:TIdentType
	Field impltys:TIdentType[]

	Field superClass:TClassDecl
	
	Field implments:TClassDecl[]			'interfaces immediately implemented
	Field implmentsAll:TClassDecl[]		'all interfaces implemented
	
	Field instanceof:TClassDecl			'for instances
	Field instances:TList		'for actual (non-arg, non-instance)
	Field instArgs:TType[]

	Field objectType:TObjectType '"canned" objectType

	'Global nullObjectClass:TClassDecl=New TNullDecl.Create( "{NULL}",Null,Null,Null,DECL_ABSTRACT|DECL_EXTERN )
	
	Method Create:TClassDecl( ident$,args:TClassDecl[],superTy:TIdentType,impls:TIdentType[],attrs:Int )
		Self.ident=ident
		Self.args=args
		Self.superTy=superTy
		Self.impltys=impls
		Self.attrs=attrs
		Self.objectType=New TObjectType.Create( Self )
		If args
			instances=New TList
			instances.AddLast Self
		EndIf
		Return Self
	End Method
	
	Method OnCopy:TDecl(deep:Int = True)
		InternalErr
	End Method
	
	Method ToString$()
		Local t$
		If args Then
				For Local i:Int=0 Until args.Length
				If i t:+","
				t:+args[i].ToString()
			Next
		ElseIf instargs
		End If
		If t t="<"+t+">"
		Return ident+t
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

	Method GenClassInstance:TClassDecl( instArgs:TType[] )

		If instanceof InternalErr
		
		'no args
		If Not instArgs
			If Not args Return Self
			For Local inst:TClassDecl=EachIn instances
				If _env.ClassScope()=inst Return inst
			Next
		EndIf
		
		'check number of args
		If args.Length<>instArgs.Length
			Err "Wrong number of type arguments for class "+ToString()
		EndIf
		
		'look for existing instance
		For Local inst:TClassDecl=EachIn instances
			Local equal:Int=True
			For Local i:Int=0 Until args.Length
				If Not inst.instArgs[i].EqualsType( instArgs[i] )
					equal=False
					Exit
				EndIf
			Next
			If equal Return inst
		Next
		
		Local inst:TClassDecl=New TClassDecl.Create( ident,Null,superTy,impltys, attrs )

		inst.attrs:&~DECL_SEMANTED
		inst.munged=munged
		inst.errInfo=errInfo
		inst.scope=scope
		inst.instanceof=Self
		inst.instArgs=instArgs
		instances.AddLast inst
		
		For Local i:Int=0 Until args.Length
			inst.InsertDecl New TAliasDecl.Create( args[i].ToString(),instArgs[i],0 )
		Next
		
		For Local decl:TDecl=EachIn _decls
			inst.InsertDecl decl.Copy()
		Next

		Return inst
	End Method

	Method IsInterface:Int()
		Return (attrs & CLASS_INTERFACE)<>0
	End Method

	Method IsFinal:Int()
		Return (attrs & DECL_FINAL)<>0
	End Method

	Method IsThrowable:Int()
		Return (attrs & CLASS_THROWABLE)<>0
	End Method

	Method IsFinalized:Int()
		Return (attrs & CLASS_FINALIZED)<>0
	End Method

	Method ExtendsObject:Int()
		Return (attrs & CLASS_EXTENDSOBJECT)<>0
	End Method
	
	Method IsInstanced:Int()
		Return (attrs & CLASS_INSTANCED)<>0
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
	
	Method FindFuncDecl:TFuncDecl( ident$,args:TExpr[] = Null ,explicit:Int=False, isArg:Int = False, isIdentExpr:Int = False )
	
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
		Return Super.FindFuncDecl( ident,args,explicit,,isIdentExpr )
	End Method
	
	Method GetAllFuncDecls:TFuncDecl[](funcs:TFuncDecl[] = Null, includeSuper:Int = True)
		If Not funcs Then
			funcs = New TFuncDecl[0]
		End If
		
		If superClass And includeSuper Then
			funcs = superClass.GetAllFuncDecls(funcs)
		End If

		' interface methods
		For Local iface:TClassDecl=EachIn implmentsAll
			For Local func:TFuncDecl=EachIn iface._decls
				Local matched:Int = False

				For Local i:Int = 0 Until funcs.length
					' found a match - we are overriding it
					If func.IdentLower() = funcs[i].IdentLower() Then
						matched = True
						Exit
					End If
				Next
				
				If Not matched Then
					funcs :+ [func]
				End If
			Next
		Next

		
		For Local func:TFuncDecl = EachIn _decls
		
			Local matched:Int = False
			
			For Local i:Int = 0 Until funcs.length
				' found a match - we are overriding it
				If func.IdentLower() = funcs[i].IdentLower() Then
					matched = True
					' set this to our own func
					funcs[i] = func
					Exit
				End If
			Next
			
			If Not matched Then
				funcs :+ [func]
			End If
		
		Next
		
		Return funcs
	End Method
	
	Method ExtendsClass:Int( cdecl:TClassDecl )
		'If Self=nullObjectClass Return True
		
'		If cdecl.IsTemplateArg()
'			cdecl=TType.objectType.FindClass()
'		EndIf
		
		Local tdecl_:TClassDecl=Self
		While tdecl_
			If tdecl_=cdecl Return True
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

		PushEnv Self

		'If Not IsTemplateInst()
		'	For Local i:Int=0 Until args.Length
		'		InsertDecl args[i]
		'		args[i].Semant
		'	Next
		'EndIf

		'Semant superclass		
		If superTy
			'superClass=superTy.FindClass()
			superClass=superTy.SemantClass()
			If superClass.IsInterface() Err superClass.ToString()+" is an interface, not a class."
			If superClass.IsFinal() Err "Final types cannot be extended."
		EndIf
		
		'Semant implemented interfaces
		Local impls:TClassDecl[]=New TClassDecl[impltys.Length]
		Local implsall:TStack=New TStack
		For Local i:Int=0 Until impltys.Length
			Local cdecl:TClassDecl=impltys[i].SemantClass()
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
		implmentsAll=New TClassDecl[implsall.Length()]
		For Local i:Int=0 Until implsall.Length()
			implmentsAll[i]=TClassDecl(implsall.Get(i))
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
		
		'Are we abstract?
		If Not IsAbstract()
			For Local decl:TDecl=EachIn _decls
				Local fdecl:TFuncDecl=TFuncDecl( decl )
				If fdecl And fdecl.IsAbstract()
					attrs:|DECL_ABSTRACT
					Exit
				EndIf
			Next
		EndIf
		
		If Not lastOffset And superClass Then
			lastOffset = superClass.LastOffset
		End If

		For Local decl:TFieldDecl=EachIn _decls
			GetFieldOffset(decl)
		Next

		If Not IsExtern() And Not IsInterface()
			Local fdecl:TFuncDecl
			For Local decl:TFuncDecl=EachIn FuncDecls()
				If Not decl.IsCtor() Continue
				Local nargs:Int
				For Local arg:TArgDecl=EachIn decl.argDecls
					If Not arg.init nargs:+1
				Next
				If nargs Continue
				fdecl=decl
				Exit
			Next
			
			
			' Don't need default new?
			'If Not fdecl
			'	fdecl=New TFuncDecl.CreateF( "new",New TObjectType.Create( Self ),Null,FUNC_CTOR )
			'	fdecl.AddStmt New TReturnStmt.Create( Null )
			'	InsertDecl fdecl
			'EndIf
		EndIf

		'NOTE: do this AFTER super semant so UpdateAttrs order is cool.
		If AppScope() Then
			AppScope().semantedClasses.AddLast Self
		End If
	End Method
	
	Method SemantParts()
'		If IsSemanted() Return
		
'		Super.Semant()
		
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

	End Method
	
	'Ok, this dodgy looking beast 'resurrects' methods that may not currently be alive, but override methods that ARE.
	Method UpdateLiveMethods:Int()
	
		If IsInterface() Return 0

		If Not superClass Return 0

		Local n:Int
		For Local decl:TFuncDecl=EachIn MethodDecls()
			If decl.IsSemanted() Continue
			
			Local live:Int
			Local unsem:TList=New TList'<TFuncDecl>
			
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
			'
			'check for duplicate fields! - BlitzMax supports fields with the same name in subclasses..
			'
			'For Local decl:TDecl=EachIn Semanted()
			'	Local fdecl:TFieldDecl=TFieldDecl( decl )
			'	If Not fdecl Continue
			'	Local cdecl:TClassDecl=superClass
			'	While cdecl
			'		For Local decl:TDecl=EachIn cdecl.Semanted()
			'			If decl.ident=fdecl.ident Err "Field '"+fdecl.ident+"' in class "+ToString()+" overrides existing declaration in class "+cdecl.ToString()
			'		Next
			'		cdecl=cdecl.superClass
			'	Wend
			'Next
			'
			'Check we implement all abstract methods!
			'
			If IsInstanced()
				Local cdecl:TClassDecl=Self
				Local impls:TList=New TList'<TFuncDecl>
				While cdecl
					For Local decl:TFuncDecl=EachIn cdecl.SemantedMethods()
						If decl.IsAbstract()
							Local found:Int
							For Local decl2:TFuncDecl=EachIn impls
								If decl.IdentLower() = decl2.IdentLower() And decl2.EqualsFunc( decl )
									found=True
									Exit
								EndIf
							Next
							If Not found
								Err "Can't create instance of type "+ToString()+" due to abstract method "+decl.ToString()+"."
							EndIf
						Else
							impls.AddLast decl
						EndIf
					Next
					cdecl=cdecl.superClass
				Wend
			EndIf
			'
			'Check we implement all interface methods!
			'
			If Not IsAbstract() Then

				Local ints:TMap = GetInterfaces()

				For Local iface:TClassDecl=EachIn ints.Values()
					For Local decl:TFuncDecl=EachIn iface.SemantedMethods()
						Local found:Int

						Local cdecl:TClassDecl=Self
						
						While cdecl And Not found
							For Local decl2:TFuncDecl=EachIn cdecl.SemantedMethods( decl.ident )
								If decl.EqualsFunc( decl2 )
									If decl2.munged
										Err "Extern methods cannot be used to implement interface methods."
									EndIf
									found=True
								EndIf
							Next
						
							cdecl = cdecl.superClass
						Wend

						If Not found
							Err decl.ToString() + " must be implemented by type " + ToString()
						EndIf
					Next
				Next
			End If
		EndIf
		
		PopErr
		
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
	
	Method GetImplementedFuncs:TList(list:TList = Null)
		If Not list Then
			list = New TList
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

Type TLoopLabelDecl Extends TDecl

	Field realIdent:String

	Method Create:TLoopLabelDecl( ident$, attrs:Int=0 )
		Self.ident="#" + ident
		Self.realIdent = ident
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

	Method Create:TDataLabelDecl( ident$, attrs:Int=0 )
		Self.ident="#" + ident
		Self.realIdent = ident
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

	Method Create:TDefDataDecl(data:TExpr[], label:TDataLabelDecl = Null, attrs:Int=0 )
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

Const MODULE_STRICT:Int=1
Const MODULE_SUPERSTRICT:Int=2
Const MODULE_ACTUALMOD:Int=4

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

	Field fileImports:TList=New TList'StringList
	
	' cache of ModuleInfo lines
	Field modInfo:TList = New TList

	Method ToString$()
		Return "Module "+munged
	End Method
	
	Method Create:TModuleDecl( ident$,munged$,filepath$,attrs:Int )
		Self.ident=ident
		Self.munged=munged
		Self.filepath=filepath
		Self.attrs=attrs

		If ident.Find(".") <> -1 Then
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
'DebugLog "GetDecl (" + ident + ") : " + filepath
		Local todo:TList=New TList'<TModuleDecl>
		Local done:TMap=New TMap'<TModuleDecl>
		
		todo.AddLast Self
		done.Insert filepath,Self
		
		Local decl:Object,declmod$
		
		While Not todo.IsEmpty()
	
			Local mdecl:TModuleDecl=TModuleDecl(todo.RemoveLast())
			Local tdecl_:Object=mdecl.GetDecl2( ident )
			
			If tdecl_ And tdecl_<>decl
				If mdecl=Self Return tdecl_
				If decl
					Err "Duplicate identifier '"+ident+"' found in module '"+declmod+"' and module '"+mdecl.ident+"'."
				EndIf
				decl=tdecl_
				declmod=mdecl.ident
			EndIf
			
			'If Not _env Exit
			
			Local imps:TUnorderedMap=mdecl.imported
			'If mdecl<>_env.ModuleScope() imps=mdecl.pubImported

			For Local mdecl2:TModuleDecl=EachIn imps.Values()
				If Not done.Contains( mdecl2.filepath )
					todo.AddLast mdecl2
					done.Insert mdecl2.filepath,mdecl2
				EndIf
				
				If ident = mdecl2.ident
					Return mdecl2
				End If
			Next

		Wend
		
		Return decl
	End Method
	
	Method GetDecl2:Object( ident$ )
		Return Super.GetDecl( ident )
	End Method

	Method OnSemant()
		Local decl:TFuncDecl = FindFuncDecl( "__localmain" )
		If decl Then
			decl.Semant
		End If
	
		For Local gdecl:TGlobalDecl=EachIn _decls
			gdecl.Semant
		Next

		For Local cdecl:TClassDecl=EachIn _decls
			cdecl.Semant
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
		
	Field semantedClasses:TList=New TList'<TClassDecl>			'in-order (ie: base before derived) list of _semanted classes
	Field semantedGlobals:TList=New TList'<TGlobalDecl>			'in-order (ie: dependancy sorted) list of _semanted globals

	Field fileImports:TList=New TList'StringList
	Field headers:TList = New TList
	
	Field stringConsts:TMap = New TMap
	Field stringConstCount:Int
	
	Field incbins:TList = New TList
	Field genIncBinHeader:Int = False
	
	Field dataDefs:TList = New TList
	Field scopeDefs:TMap = New TMap
	
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
		
		SemantDataDefs()	

		mainModule.Semant

		mainFunc=mainModule.FindFuncDecl( "__localmain" )
		
		
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

End Type
