' Copyright (c) 2013-2014 Bruce A Henderson
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

Type TExpr
	Field exprType:TType

	Method ToString$()
		Return "<TExpr>"
	End Method

	Method Copy:TExpr()
		InternalErr
	End Method

	Method Semant:TExpr()
		InternalErr
	End Method

	Method SemantSet:TExpr( op$,rhs:TExpr )
		Err ToString()+" cannot be assigned to."
	End Method

	Method SemantFunc:TExpr( args:TExpr[] , throwError:Int = True, funcCall:Int = False )
		Err ToString()+" cannot be invoked."
	End Method

	Method SemantScope:TScopeDecl()
		Return Null
	End Method

	Method Eval$()
		Err ToString()+" cannot be statically evaluated."
	End Method

	Method EvalConst:TExpr()
		Local expr:TExpr = New TConstExpr.Create( exprType,Eval() ).Semant()
		If TStringType(TConstExpr(expr).ty) Then
			_appInstance.mapStringConsts(TConstExpr(expr).value)
		End If
		Return expr
	End Method

	Method Trans$()
		Todo
	End Method

	Method TransStmt$()
		Return Trans()
	End Method

	Method TransVar$()
		InternalErr
	End Method

	'semant and cast
	Method SemantAndCast:TExpr( ty:TType,castFlags:Int=0 )
		Local expr:TExpr=Semant()
		If expr.exprType.EqualsType( ty ) Return expr
		Return New TCastExpr.Create( ty,expr,castFlags ).Semant()
	End Method

	'expr and ty already semanted!
	Method Cast:TExpr( ty:TType,castFlags:Int=0 )
		If Not exprType Then
			Semant()
		End If
		If exprType.EqualsType( ty ) Return Self
		Return New TCastExpr.Create( ty,Self,castFlags ).Semant()
	End Method

	Method SemantArgs:TExpr[]( args:TExpr[] )
		args=args[..]
		For Local i:Int=0 Until args.Length
			If args[i] Then
				If TIdentExpr(args[i]) Then
					TIdentExpr(args[i]).isArg = True
				End If
				args[i]=args[i].Semant()
			End If
		Next
		Return args
	End Method

	Method CastArgs:TExpr[]( args:TExpr[],funcDecl:TFuncDecl )
		If args.Length>funcDecl.argDecls.Length InternalErr

		' FIXME
		'args=args.Resize( funcDecl.argDecls.Length )
		' FIXME

		For Local i:Int=0 Until args.Length
			If args[i]
				If Not funcDecl.argDecls[i].IsSemanted() Then
					funcDecl.argDecls[i].Semant()
				End If

				If TInvokeExpr(args[i]) And Not TInvokeExpr(args[i]).invokedWithBraces Then
					If Not IsPointerType(funcDecl.argDecls[i].ty, TType.T_BYTE) And Not TFunctionPtrType(funcDecl.argDecls[i].ty) Then
						Err "Unable to convert from '" + args[i].exprType.ToString() + "()' to '" + funcDecl.argDecls[i].ty.ToString() + "'"
					End If
				End If
				args[i]=args[i].Cast( funcDecl.argDecls[i].ty )
			Else If funcDecl.argDecls[i].init
				args[i]=funcDecl.argDecls[i].init
			Else
				Err "Missing function argument '"+funcDecl.argDecls[i].ident+"'."
			EndIf
		Next
		Return args
	End Method

	Method BalanceTypes:TType( lhs:TType,rhs:TType )
'DebugStop
		'If lhs._flags & TType.T_VAR Then
		'	lhs = TType.MapVarPointerToPrim(lhs)
		'End If

		'If rhs._flags & TType.T_VAR Then
		'	rhs = TType.MapVarPointerToPrim(rhs)
		'End If

		If TStringType( lhs ) Or TStringType( rhs ) Return New TStringType
		If TDoubleType( lhs ) Or TDoubleType( rhs ) Return New TFloatType
		If TFloatType( lhs ) Or TFloatType( rhs ) Return New TFloatType
		If IsPointerType( lhs, 0, TType.T_POINTER ) Or IsPointerType( rhs, 0, TType.T_POINTER ) Then
			If IsPointerType( lhs, 0, TType.T_POINTER ) Return lhs
			If IsPointerType( rhs, 0, TType.T_POINTER ) Return rhs
		End If
		If TFunctionPtrType( lhs ) Or TFunctionPtrType( rhs ) Then
			If TFunctionPtrType( lhs ) Return lhs
			If TFunctionPtrType( rhs ) Return rhs
		End If
		If TLongType( lhs ) Or TLongType( rhs ) Return New TLongType
		If TIntType( lhs ) Or TIntType( rhs ) Return New TIntType
		If TObjectType( lhs ) And TNullDecl(TObjectType( lhs ).classDecl) Then
			Return rhs
		End If
		If TObjectType( rhs ) And TNullDecl(TObjectType( rhs ).classDecl) Then
			Return lhs
		End If
		If lhs.ExtendsType( rhs ) Return rhs
		If rhs.ExtendsType( lhs ) Return lhs
		' balance arrays - only for objects... to the lowest common denominator.
		If TArrayType( lhs ) And TArrayType( rhs ) Then

			If TObjectType(TArrayType( lhs ).elemType) And TObjectType(TArrayType( rhs ).elemType) Then
				' lhs = Object[]
				If TObjectType(TArrayType( lhs ).elemType).classDecl.ident = "Object" Then
					Return lhs
				End If
				' rhs = Object[]
				If TObjectType(TArrayType( rhs ).elemType).classDecl.ident = "Object" Then
					Return rhs
				End If
				
				' does one extend the other? If so, return the base type
				If TObjectType(TArrayType( lhs ).elemType).ExtendsType(TObjectType(TArrayType( rhs ).elemType)) Then
					Return rhs
				End If

				If TObjectType(TArrayType( rhs ).elemType).ExtendsType(TObjectType(TArrayType( lhs ).elemType)) Then
					Return lhs
				End If
				
				' no? then we will fallback to an Object type array
				
				' find the Object classdecl instance
				Local modid$="brl.classes"
				Local mdecl:TModuleDecl=_env.FindModuleDecl( modid )
				' return an array of Objects
				Return New TArrayType.Create(New TObjectType.Create(TClassDecl(mdecl.FindDecl( "object" ))))
			End If
			
			If TObjectType(TArrayType( lhs ).elemType) And TObjectType(TArrayType( lhs ).elemType).classDecl.ident = "Object" And TStringType(TArrayType( rhs ).elemType) Then
				Return lhs
			End If

			If TObjectType(TArrayType( rhs ).elemType) And TObjectType(TArrayType( rhs ).elemType).classDecl.ident = "Object"  And TStringType(TArrayType( lhs ).elemType) Then
				Return rhs
			End If

			If TObjectType(TArrayType( lhs ).elemType) And TObjectType(TArrayType( lhs ).elemType).classDecl.ident = "Object"  And TArrayType(TArrayType( rhs ).elemType) Then
				Return lhs
			End If

			If TObjectType(TArrayType( rhs ).elemType) And TObjectType(TArrayType( rhs ).elemType).classDecl.ident = "Object"  And TArrayType(TArrayType( lhs ).elemType) Then
				Return rhs
			End If

			' balancing primitive types
			If Not TArrayType( lhs ).elemType.EqualsType(TArrayType( rhs ).elemType) Then
				Err "Types '" + TArrayType( lhs ).elemType.ToString() + " Array' and '" + TArrayType( rhs ).elemType.ToString() + " Array' are unrelated"
			End If
			
		End If
		Err "Can't balance types "+lhs.ToString()+" and "+rhs.ToString()+"."
	End Method

	Function CopyExpr:TExpr( expr:TExpr )
		If Not expr Return Null
		Return expr.Copy()
	End Function

	Function CopyArgs:TExpr[]( exprs:TExpr[] )
		exprs=exprs[..]
		For Local i:Int=0 Until exprs.Length
			exprs[i]=CopyExpr( exprs[i] )
		Next
		Return exprs
	End Function

End Type

'	exec a stmt, return an expr
Type TStmtExpr Extends TExpr
	Field stmt:TStmt
	Field expr:TExpr

	Method Create:TStmtExpr( stmt:TStmt,expr:TExpr )
		Self.stmt=stmt
		Self.expr=expr
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TStmtExpr.Create( stmt,CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TStmtExpr(,"+expr.ToString()+")"
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		stmt.Semant()
		expr=expr.Semant()
		exprType=expr.exprType
		Return Self
	End Method

	Method Trans$()
		Return _trans.TransStmtExpr( Self )
	End Method

	Method TransVar$()
		Semant
		Return _trans.TransStmtExpr( Self )
	End Method

End Type

'	literal
Type TConstExpr Extends TExpr
	Field ty:TType
	Field value$

	Method Create:TConstExpr( ty:TType,value$ )
		If TIntType( ty )
			Local radix:Int
			If value.StartsWith( "%" )
				radix=1
			Else If value.StartsWith( "$" )
				radix=4
			EndIf

			If radix
				Local val:Long = 0

				For Local i:Int=1 Until value.Length
					Local ch:Int=value[i]
					If ch>=48 And ch<58
						val=val Shl radix | (ch & 15)
					Else
						val=val Shl radix | ((ch & 15)+9)
					EndIf
				Next
				If val >= 2147483648:Long Then
					value = String( -2147483648:Long + (val - 2147483648:Long))
				Else
					value=String( val )
				End If
			Else
				value = String.FromLong(value.ToLong())
			EndIf

		Else If TFloatType( ty )
			If Not (value.Contains("e") Or value.Contains("E") Or value.Contains("."))
				value:+".0"
			EndIf
		EndIf
		Self.ty=ty
		Self.value=value
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TConstExpr.Create( ty,value )
	End Method

	Method ToString$()
		Return "TConstExpr(~q"+value+"~q)"
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		exprType=ty.Semant()
		Return Self
	End Method

	Method Eval$()
		Return value
	End Method

	Method EvalConst:TExpr()
		Return Self
	End Method

	Method Trans$()
		Semant
		Return _trans.TransConstExpr( Self )
	End Method

	Method SemantAndCast:TExpr( ty:TType,castFlags:Int=0 )
		Local expr:TExpr=Semant()
		If expr.exprType.EqualsType( ty ) Return expr
		If value = "bbNullObject" Then
			Err "bbNullObject"
			Return expr
		End If
		Return New TCastExpr.Create( ty,expr,castFlags ).Semant()
	End Method

End Type

Type TVarExpr Extends TExpr
	Field decl:TVarDecl

	Method Create:TVarExpr( decl:TVarDecl )
		Self.decl=decl
		Return Self
	End Method

	Method Copy:TExpr()
		Return Self
	End Method

	Method ToString$()
		Return "TVarExpr("+decl.ToString()+")"
	End Method

	Method Semant:TExpr()
		If exprType Return Self
		If Not decl.IsSemanted() InternalErr
		exprType=decl.ty
		Return Self
	End Method

	Method SemantSet:TExpr( op$,rhs:TExpr )
		Return Semant()
	End Method

	Method Trans$()
		Semant
		Return _trans.TransTemplateCast( exprType,TVarDecl(decl.actual).ty,_trans.TransVarExpr( Self ) )
	End Method

	Method TransVar$()
		Semant
		Return _trans.TransVarExpr( Self )
	End Method

End Type

Type TMemberVarExpr Extends TExpr
	Field expr:TExpr
	Field decl:TVarDecl

	Method Create:TMemberVarExpr( expr:TExpr,decl:TVarDecl )
		Self.expr=expr
		Self.decl=decl
		Return Self
	End Method

	Method Copy:TExpr()
		Return Self
	End Method

	Method ToString$()
		Return "TMemberVarExpr("+expr.ToString()+","+decl.ToString()+")"
	End Method

	Method Semant:TExpr()
		If exprType Return Self
		If Not decl.IsSemanted() InternalErr
		exprType=decl.ty
		Return Self
	End Method

	Method SemantSet:TExpr( op$,rhs:TExpr )
		Return Semant()
	End Method

	Method Trans$()
		Return _trans.TransTemplateCast( exprType,TVarDecl(decl.actual).ty,_trans.TransMemberVarExpr( Self ) )
	End Method

	Method TransVar$()
		Return _trans.TransMemberVarExpr( Self )
 	End Method

End Type

Type TInvokeExpr Extends TExpr
	Field decl:TFuncDecl
	Field args:TExpr[]
	Field invokedWithBraces:Int

	Method Create:TInvokeExpr( decl:TFuncDecl,args:TExpr[]=Null,invokedWithBraces:Int=True )
		Self.decl=decl
		If args Then
			Self.args=args
		Else
			Self.args = New TExpr[0]
		End If
		Self.invokedWithBraces = invokedWithBraces
		Return Self
	End Method

	Method Copy:TExpr()
		Return Self
	End Method

	Method ToString$()
		Local t$="TInvokeExpr("+decl.ToString()
		For Local arg:TExpr=EachIn args
			t:+","+arg.ToString()
		Next
		Return t+")"
	End Method

	Method Semant:TExpr()

		If exprType Return Self

		' handle Sgn, Asc and Chr keywords/functions for const values
		Select decl.ident.ToLower()
			Case "sgn"
				Local arg:TExpr = args[0]
				If TConstExpr(arg) Then
					'use different calls to only return a "float sgn"
					'when param is a float
					Local val:String = TConstExpr(arg).value
					Local expr:TExpr
					If String(Int(val)) = val
						expr = New TConstExpr.Create(New TIntType, Sgn(Int(TConstExpr(arg).value)))
					Else
						expr = New TConstExpr.Create(New TIntType, Sgn(Float(TConstExpr(arg).value)))
					End If
					
					_appInstance.removeStringConst(TConstExpr(arg).value)
					expr.Semant()
					Return expr
				End If
			Case "asc"
				Local arg:TExpr = args[0]
				If TConstExpr(arg) Then
					Local expr:TExpr = New TConstExpr.Create(New TIntType, Asc(TConstExpr(arg).value))
					_appInstance.removeStringConst(TConstExpr(arg).value)
					expr.Semant()
					Return expr
				End If
			Case "chr"
				Local arg:TExpr = args[0]
				If TConstExpr(arg) Then
					Local expr:TConstExpr = New TConstExpr.Create(New TStringType, Chr(Int(TConstExpr(arg).value)))
					expr.Semant()
					_appInstance.mapStringConsts(expr.value)
					Return expr
				End If
		End Select


		If Not decl.retType
			decl.Semant()
		End If
		If TIdentType(decl.retType) Then
			exprType = decl.retType.Semant()
		Else
			exprType=decl.retType
		End If

		args=CastArgs( args,decl )
		Return Self
	End Method

	Method Trans$()
'		Return _trans.TransTemplateCast( exprType,TFuncDecl(decl.actual).retType,_trans.TransInvokeExpr( Self ) )
		Return _trans.TransInvokeExpr( Self )
	End Method

	Method TransStmt$()
		Return _trans.TransInvokeExpr( Self )
	End Method

	Method Eval$()
		Select decl.ident.ToLower()
			Case "sgn"
				If args.length = 1 Then
					'use different calls to only return a "float sgn"
					'when param is a float
					Local v:String = String(args[0].Eval())
					If String(Int(v)) = v
						Return Sgn(Int(v))
					Else
						Return Sgn(Float(v))
					End If
				End If
				DebugStop
			Case "asc"
				If args.length = 1 Then
					Local v:String = String(args[0].Eval())
					If v And v.length = 1 Then
						Return Asc(v)
					End If
				End If
				DebugStop
			Case "chr"
				DebugStop
		Default
			Return Super.Eval()
		End Select
	End Method

End Type

Type TInvokeMemberExpr Extends TExpr
	Field expr:TExpr
	Field decl:TFuncDecl
	Field args:TExpr[]
	Field isResize:Int	'FIXME - butt ugly!

	Method Create:TInvokeMemberExpr( expr:TExpr,decl:TFuncDecl,args:TExpr[]=Null )
		Self.expr=expr
		Self.decl=decl
		If args
			Self.args=args
		Else
			Self.args = New TExpr[0]
		End If
		Return Self
	End Method

	Method Copy:TExpr()
		Return Self
	End Method

	Method ToString$()
		Local t$="TInvokeMemberExpr("+expr.ToString()+","+decl.ToString()
		For Local arg:TExpr=EachIn args
			t:+","+arg.ToString()
		Next
		Return t+")"
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		If Not decl.IsSemanted() decl.Semant()
		exprType=decl.retType

		args=SemantArgs( args )
		args=CastArgs( args,decl )

		'Array $resize hack!
		If TArrayType( exprType ) And TVoidType( TArrayType( exprType ).elemType )
			isResize=True
			exprType=expr.exprType
		EndIf

		Return Self
	End Method

	Method Trans$()
		'Array $resize hack!
		If isResize Return _trans.TransInvokeMemberExpr( Self )

		Return _trans.TransTemplateCast( exprType,TFuncDecl(decl.actual).retType,_trans.TransInvokeMemberExpr( Self ) )
	End Method

	Method TransStmt$()
		Return _trans.TransInvokeMemberExpr( Self )
	End Method

End Type

Type TNewObjectExpr Extends TExpr
	Field ty:TType
	Field args:TExpr[]
	Field ctor:TFuncDecl
	Field classDecl:TClassDecl
	Field instanceExpr:TExpr

	Method Create:TNewObjectExpr( ty:TType,args:TExpr[] )
		Self.ty=ty
		Self.args=args
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TNewObjectExpr.Create( ty,CopyArgs(args) )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		Local it:TIdentType = TIdentType(ty)
		Local iArgs:TExpr[] = CopyArgs(args)

		ty=ty.Semant(True)
		If Not ty Then
			' maybe it's an instance of a type ?
			Local decl:TVarDecl = TVarDecl(_env.FindDecl(it.ident))
			If decl And TObjectType(decl.ty) Then
				ty = decl.ty
				instanceExpr = New TVarExpr.Create(decl).Semant()
			Else
				Err "Type '"+it.ident+"' not found"
			End If
		End If
		args=SemantArgs( args )

		Local objTy:TObjectType=TObjectType( ty )
		If Not objTy
			Err "Expression is not a class."
		EndIf

		classDecl=objTy.classDecl

		If classDecl.IsInterface() Err "Cannot create instance of an interface."
		If classDecl.IsAbstract() Err "Cannot create instance of an abstract class."
		'If classDecl.IsTemplateArg() Err "Cannot create instance of a generic argument."
		If classDecl.args And Not classDecl.instanceof Err "Cannot create instance of a generic class."

		If classDecl.IsExtern()
			If args Err "No suitable constructor found for class "+classDecl.ToString()+"."
'		Else
'DebugStop
'			ctor=classDecl.FindFuncDecl( "new",args )
'			If Not ctor	Err "No suitable constructor found for class "+classDecl.ToString()+"."
'			args=CastArgs( args,ctor )
		EndIf

		classDecl.attrs:|CLASS_INSTANCED

		exprType=ty
		
		If it Then
			Local parts:String[] = it.ident.Split(".")
			
			Local i:Int = 0
			
			While i < parts.length And parts[i].ToLower() <> classDecl.ident.ToLower()
				i :+ 1
			Wend
			
			i :+ 1

			Local expr:TExpr = Self
			Local cdecl:TClassDecl = classDecl
			Local eType:TType = objTy
			
			While i < parts.length
				Local id:String = parts[i]
				i :+ 1
				
				' find member function.method
				Local fdecl:TFuncDecl = cdecl.FindFuncDecl(id, iArgs)
				If fdecl Then
					expr = New TInvokeMemberExpr.Create( expr,fdecl, iArgs ).Semant()
					eType = expr.exprType
					If TObjectType(eType) Then
						cdecl = TObjectType(expr.exprType).classdecl
					End If
					If TArrayType(eType) Or TStringType(eType) Then
						cdecl = eType.GetClass()
					End If
					Continue
				End If
				' find other member decl (field, etc)
				Local decl:TVarDecl = TVarDecl(cdecl.GetDecl(id))
				If decl Then
					Local tmp:TLocalDecl=New TLocalDecl.Create( "", eType, expr )
					Local varExpr:TExpr = New TMemberVarExpr.Create(New TVarExpr.Create( tmp ), decl).Semant()
					expr = New TStmtExpr.Create( New TDeclStmt.Create( tmp ), varExpr ).Semant()
					eType = decl.ty
					If TObjectType(eType) Then
						cdecl = TObjectType(expr.exprType).classdecl
					End If
					If TArrayType(eType) Or TStringType(eType) Then
						cdecl = eType.GetClass()
					End If
					Continue
				End If
				
				' didn't match member or function??
				' probably an error...
				Err "Identifier '" + id + "' not found."
			Wend
			
			Return expr
		End If

		Return Self
	End Method

	Method Trans$()
		Return _trans.TransNewObjectExpr( Self )
	End Method
End Type

Type TNewArrayExpr Extends TExpr
	Field ty:TType

	Field expr:TExpr[]
	
	Method Create:TNewArrayExpr( ty:TType,expr:TExpr[] )

		Self.ty=ty
		Self.expr=expr
		Return Self
	End Method

	Method Copy:TExpr()
		If exprType InternalErr
		Local cexpr:TExpr[expr.length]
		For Local i:Int = 0 Until expr.length
			cexpr[i] = CopyExpr(expr[i])
		Next
		Return New TNewArrayExpr.Create( ty,cexpr )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		ty=ty.Semant()
		exprType=New TArrayType.Create( ty )
		For Local i:Int = 0 Until expr.length
			expr[i]=expr[i].SemantAndCast( New TIntType )
		Next
		Return Self
	End Method

	Method Trans$()
		Return _trans.TransNewArrayExpr( Self )
	End Method

End Type

'	super.ident( args )
Type TInvokeSuperExpr Extends TExpr
	Field ident$
	Field args:TExpr[]
	Field origFuncDecl:TFuncDecl
	Field funcDecl:TFuncDecl
	Field classScope:TClassDecl
	Field superClass:TClassDecl

	Method Create:TInvokeSuperExpr( ident$,args:TExpr[] = Null )
		Self.ident=ident
		If args Then
			Self.args=args
		Else
			Self.args = New TExpr[0]
		End If
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TInvokeSuperExpr.Create( ident,CopyArgs(args) )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		If _env.FuncScope().IsStatic() Err "Illegal use of Super."

		classScope=_env.ClassScope()
		superClass=classScope.superClass
		
		If Not superClass Err "Type has no super class."

		args=SemantArgs( args )
		origFuncDecl=classScope.FindFuncDecl(ident,args)
		funcDecl=superClass.FindFuncDecl( ident,args )
		' ensure the super function has been semanted
		funcDecl.Semant()
		If Not funcDecl Err "Can't find superclass method '"+ident+"'."
		args=CastArgs( args,funcDecl )
		exprType=funcDecl.retType
		Return Self
	End Method

	Method Trans$()
		Return _trans.TransInvokeSuperExpr( Self )
	End Method

End Type

'	Self
Type TSelfExpr Extends TExpr

	Method Copy:TExpr()
		Return New TSelfExpr
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		If _env.FuncScope().IsStatic() Err "Illegal use of Self within static scope."
		exprType=New TObjectType.Create( _env.ClassScope() )
		Return Self
	End Method

	Method Trans$()
		Return _trans.TransSelfExpr( Self )
	End Method

End Type

Const CAST_EXPLICIT:Int=1

Type TCastExpr Extends TExpr
	Field ty:TType
	Field expr:TExpr
	Field flags:Int

	Method Create:TCastExpr( ty:TType,expr:TExpr,flags:Int=0 )
		Self.ty=ty
		Self.expr=expr
		Self.flags=flags
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TCastExpr.Create( ty,CopyExpr(expr),flags )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		ty=ty.Semant()
		expr=expr.Semant()

		Local src:TType=expr.exprType

		'equal?
		If src.EqualsType( ty ) Return expr

		'upcast?
		If src.ExtendsType( ty )
			'cast from void[] to T[]
			If TArrayType(src) And TVoidType( TArrayType(src).elemType )
				Return New TConstExpr.Create( ty,"" ).Semant()
			EndIf
			
			If src._flags & TType.T_VARPTR Then
				exprType = ty
				Return Self
			End If


			If TStringType(src) And TObjectType(ty)
				exprType = ty
				Return expr
			End If
			'Box/unbox?...
			'If TObjectType( ty ) And Not TObjectType( src )

				'Box!
			'	expr=New TNewObjectExpr.Create( ty,[expr] ).Semant()

			'Else
			If TObjectType( src ) And Not TObjectType( ty ) And Not TStringType( ty )

				'Unbox!
				Local op$
				'If TBoolType( ty )
				'	op="ToBool"
				'Else
				If TIntType( ty )
					op="ToInt"
				Else If TFloatType( ty )
					op="ToFloat"
				Else If TStringType( ty )
					op="ToString"
				Else If IsPointerType( ty, TType.T_BYTE )
					exprType = ty
					If flags = CAST_EXPLICIT Then
						Return Self
					Else
						Return expr
					End If
				Else
					InternalErr
				EndIf
				Local fdecl:TFuncDecl=src.GetClass().FindFuncDecl( op )
				expr=New TInvokeMemberExpr.Create( expr,fdecl ).Semant()

			EndIf
			exprType=ty

		Else If TBoolType( ty )

			'If VoidType( src )
			'	Err "Cannot convert from Void to Bool."
			'EndIf

			If  flags & CAST_EXPLICIT
				exprType=ty
			EndIf

		Else If ty.ExtendsType( src )

			If flags & CAST_EXPLICIT
'DebugStop
				'if both objects or both non-objects...
				If (TObjectType(ty)<>Null)=(TObjectType(src)<>Null) Then
					exprType=ty
					Return Self
				End If

			'Else ' if not explicitly cast, we can't just auto-cast it ourselves here.
				'If (TObjectType(ty)<>Null) And (TObjectType(src)<>Null) exprType=ty
			EndIf

		EndIf

		'If TStringType(src) And TStringVarPtrType(ty) Then
		'	exprType = ty
		'	Return Self
		'End If

'		If TArrayType(src) And TPointerType(ty) Then
'			exprType = ty
'			Return expr
'		End If

		If TFunctionPtrType(ty) And TInvokeExpr(expr) Then
			exprType = ty
			Return expr
		End If

		If TIntType(ty) And Not IsPointerType(ty, 0, TType.T_POINTER) And IsPointerType(src, 0, TType.T_POINTER) Then
			exprType = ty
			If flags & CAST_EXPLICIT Then
				Return Self
			End If
			Return expr
		End If

		' explicit cast to number
		If IsNumericType(ty) And IsPointerType(src, 0, TType.T_POINTER) And flags = CAST_EXPLICIT Then
			exprType = ty
			Return Self
		End If

'		If TPointerType(ty) And TIntType(src) Then
'			exprType = ty
'			Return expr
'		End If

		If TIntType(ty) And TObjectType(src) Then
' DebugStop ' Bah woz ere
			exprType = ty
			Return expr
		End If

		If TObjectType(src) And TNullDecl(TObjectType(src).classDecl) Then
			exprType = ty
			Return expr
		End If

		If TObjectType(src) And (ty._flags & TType.T_VAR) Then ' TODO : May be VARPTR instead?
			exprType = NewPointerType(TType.T_BYTE)
			Return Self
		End If
		
		If TStringType(src) And ((src._flags & TType.T_CHAR_PTR) Or (src._flags & TType.T_SHORT_PTR)) And TStringType(ty) Then
			exprType = ty
			Return Self
		End If
		
		' cast from "some kind of object" array to Object[]
		If TArrayType(ty) And TArrayType(src)
			If (TObjectType(TArrayType(src).elemType) Or TStringType(TArrayType(src).elemType) Or TArrayType(TArrayType(src).elemType)) And TObjectType(TArrayType(ty).elemType) Then
				If TObjectType(TArrayType(ty).elemType).classDecl.ident = "Object" Then
					exprType = ty
					Return Self
				End If
			End If
		End If
		
		If TArrayType(ty) And TObjectType(src) 
			If TObjectType(src).classDecl.ident = "___Array" Then
				exprType = ty
				Return expr
			Else If  TObjectType(src).classDecl.ident = "Object" Then
				exprType = ty
				Return Self
			End If
		End If

		If IsPointerType(ty, 0, TType.T_POINTER | TType.T_CHAR_PTR | TType.T_SHORT_PTR) Then
			If IsNumericType(src) And Not (src._flags & TType.T_VARPTR) Then
				'If IsPointerType(ty,0,TType.T_POINTER) Then
				'	exprType = TNumericType(src).ToPointer()
				'Else
					exprType = ty
				'End If
				Return Self
			Else If TNumericType(src) And (src._flags & TType.T_VARPTR) Then
				exprType = expr.exprType
			Else If TArrayType(src) Then
				If TNumericType(TArrayType(src).elemType) Then
					exprType = TNumericType(TArrayType(src).elemType).ToPointer()
					Return Self
				End If
			Else If TStringType(src) Then
				exprType = ty
				Return Self
			End If
		End If
		
		If TStringType(src) And TStringType(ty) And (ty._flags & TType.T_VAR) Then
			exprType = ty
			Return Self
		End If

		If TVarPtrType(ty) Then
			exprType = src.Copy()
			exprType._flags :| TType.T_VARPTR
			ty = exprType
			Return Self
		End If
		
		If TFunctionPtrType(ty) And IsPointerType(src, 0, TType.T_POINTER) Then
			exprType = ty
			Return Self
		End If

		If Not exprType
			DebugStop
			Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
		EndIf

		If TConstExpr( expr ) Return EvalConst()
		Return Self
	End Method

	Method Eval$()
		Local val$=expr.Eval()
		If Not val Return val
		If TBoolType( exprType )
			If TIntType( expr.exprType )
				If Int( val ) Return "1"
				Return ""
			Else If TFloatType( expr.exprType )
				If Float( val ) Return "1"
				Return ""
			Else If TStringType( expr.exprType )
				If val.Length Return "1"
				Return ""
			EndIf
		Else If TIntType( exprType )
			If TBoolType( expr.exprType )
				If val Return "1"
				Return "0"
			EndIf
			Return Int( val )
		Else If TShortType( exprType )
			Return Short( val )
		Else If TFloatType( exprType )
			Return Float( val )
		Else If TDoubleType( exprType )
			Return Double( val )
		Else If TLongType( exprType )
			Return Long( val )
		Else If TStringType( exprType )
			Return String( val )
		Else If TByteType( exprType )
			Return Byte( val )
		EndIf
		Return Super.Eval()
	End Method

	Method Trans$()
		Return _trans.TransCastExpr( Self )
	End Method

	Method ToString$()
		Local t$="TCastExpr(" + ty.ToString()
		If expr t:+","+expr.ToString()
		Return t+")"
	End Method

End Type

'op = '+', '-', '~'
Type TUnaryExpr Extends TExpr
	Field op$,expr:TExpr

	Method Create:TUnaryExpr( op$,expr:TExpr )
		Self.op=op
		Self.expr=expr
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TUnaryExpr.Create( op,CopyExpr(expr) )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		Select op
		Case "+","-"
			expr=expr.Semant()
			If Not TNumericType( expr.exprType ) Or IsPointerType(expr.exprType) Then
				Err expr.ToString()+" must be numeric for use with unary operator '"+op+"'"
			End If
			exprType=expr.exprType
			' Remove Var-ness, if required. "expr" will still be "Var"
			If exprType._flags & TType.T_VAR Then
				exprType = exprType.Copy()
				exprType._flags :~ TType.T_VAR
			End If
		Case "~~"
			expr=expr.SemantAndCast( New TIntType )
			exprType=New TIntType
		Case "not"
			expr=expr.SemantAndCast( New TBoolType,CAST_EXPLICIT )
			exprType=New TBoolType
		Default
			InternalErr
		End Select

		If TConstExpr( expr ) Return EvalConst()
		Return Self
	End Method

	Method Eval$()
		Local val$=expr.Eval()
		Select op
		Case "~~"
			Return ~Int( val )
		Case "+"
			Return val
		Case "-"
			If val.StartsWith( "-" ) Return val[1..]
			Return "-"+val
		Case "not"
			If val Return ""
			Return "1"
		End Select
		InternalErr
	End Method

	Method Trans$()
		Return _trans.TransUnaryExpr( Self )
	End Method

End Type

Type TBinaryExpr Extends TExpr
	Field op$
	Field lhs:TExpr
	Field rhs:TExpr

	Method Trans$()
		Return _trans.TransBinaryExpr( Self )
	End Method

	Method ToString$()
		Return "(" + lhs.ToString() + " " + op + " " + rhs.ToString() + ")"
	End Method

End Type

' * / + / & ~ | ^ shl shr
Type TBinaryMathExpr Extends TBinaryExpr

	Method Create:TBinaryMathExpr( op$,lhs:TExpr,rhs:TExpr )
		Self.op=op
		Self.lhs=lhs
		Self.rhs=rhs
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TBinaryMathExpr.Create( op,CopyExpr(lhs),CopyExpr(rhs) )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		lhs=lhs.Semant()
		rhs=rhs.Semant()

		Select op
		Case "&","~~","|","mod","shl","shr"
			If TDoubleType(lhs.exprType) Then
				exprType=New TLongType
			Else If TFloatType(lhs.exprType) Then
				exprType=New TIntType
			Else If TNumericType(lhs.exprType) Then
				exprType=lhs.exprType.OnCopy()
			Else
				exprType=New TIntType
			End If
		Case "^"
			exprType=New TDoubleType
		Default
			exprType=BalanceTypes( lhs.exprType,rhs.exprType )
			If TStringType( exprType )
				If op<>"+"
					Err "Illegal string operator."
				EndIf
			Else If Not TNumericType( exprType ) And Not IsPointerType( exprType, 0, TType.T_POINTER ) And Not TArrayType( exprType )
				Err "Illegal expression type."
			Else If IsPointerType( exprType, 0, TType.T_POINTER ) And op <> "+" And op <> "-" Then
				Err "Illegal expression type."
			EndIf
		End Select

		If (op = "+" Or op = "-") And IsPointerType(exprType, 0, TType.T_POINTER) And TNumericType(lhs.exprType) Then
			' with pointer addition we don't cast the numeric to a pointer
		Else
			lhs=lhs.Cast( exprType )
		End If
		
		If (op = "+" Or op = "-") And IsPointerType(exprType, 0, TType.T_POINTER) And TNumericType(rhs.exprType) Then
			' with pointer addition we don't cast the numeric to a pointer
		Else
			rhs=rhs.Cast( exprType )
		End If

		If TConstExpr( lhs ) And TConstExpr( rhs ) Return EvalConst()

		Return Self
	End Method

	Method Eval$()
		Local lhs$=Self.lhs.Eval()
		Local rhs$=Self.rhs.Eval()
		If TIntType( exprType )
			Local x:Int=Int(lhs),y:Int=Int(rhs)
			Select op
			Case "^" Return x^y
			Case "*" Return x*y
			Case "/" Return x/y
			Case "mod" Return x Mod y
			Case "shl" Return x Shl y
			Case "shr" Return x Shr y
			Case "+" Return x + y
			Case "-" Return x - y
			Case "&" Return x & y
			Case "~~" Return x ~ y
			Case "|" Return x | y
			End Select
		Else If TLongType( exprType )
			Local x:Long=Long(lhs),y:Long=Long(rhs)
			Select op
			Case "^" Return x^y
			Case "*" Return x*y
			Case "/" Return x/y
			Case "mod" Return x Mod y
			Case "shl" Return x Shl y
			Case "shr" Return x Shr y
			Case "+" Return x + y
			Case "-" Return x - y
			Case "&" Return x & y
			Case "~~" Return x ~ y
			Case "|" Return x | y
			End Select
		Else If TFloatType( exprType )
			Local x:Float=Float(lhs),y:Float=Float(rhs)
			Select op
			Case "^" Return x^y
			Case "*" Return x * y
			Case "/" Return x / y
			Case "+" Return x + y
			Case "-" Return x - y
			End Select
		Else If TDoubleType( exprType )
			Local x:Double=Double(lhs),y:Double=Double(rhs)
			Select op
			Case "^" Return x^y
			Case "*" Return x * y
			Case "/" Return x / y
			Case "+" Return x + y
			Case "-" Return x - y
			End Select
		Else If TStringType( exprType )
			Select op
			Case "+" 
				_appInstance.removeStringConst(lhs)
				_appInstance.removeStringConst(rhs)
				Return lhs+rhs
			End Select
		EndIf
		InternalErr
	End Method

End Type

'=,<>,<,<=,>,>=
Type TBinaryCompareExpr Extends TBinaryExpr
	Field ty:TType

	Method Create:TBinaryCompareExpr( op$,lhs:TExpr,rhs:TExpr )
		Self.op=op
		Self.lhs=lhs
		Self.rhs=rhs
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TBinaryCompareExpr.Create( op,CopyExpr(lhs),CopyExpr(rhs) )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		lhs=lhs.Semant()
		rhs=rhs.Semant()

		ty=BalanceTypes( lhs.exprType,rhs.exprType )
		If TArrayType( ty )
			If TArrayType(lhs.exprType) And TArrayType(rhs.exprType) Then
				Err "Arrays cannot be compared."
			End If
		EndIf

		lhs=lhs.Cast( ty )
		rhs=rhs.Cast( ty )

		exprType=New TBoolType

		If TConstExpr( lhs ) And TConstExpr( rhs ) Return EvalConst()

		Return Self
	End Method

	Method Eval$()
		Local r:Int=-1
		If TBoolType( ty )
			Local lhs:Int=Int(Self.lhs.Eval())
			Local rhs:Int=Int(Self.rhs.Eval())
			Select op
			Case "="  r=(lhs= rhs)
			Case "<>" r=(lhs<>rhs)
			End Select
		Else If TIntType( ty )
			Local lhs:Int=Int( Self.lhs.Eval() )
			Local rhs:Int=Int( Self.rhs.Eval() )
			Select op
			Case "="  r=(lhs= rhs)
			Case "<>" r=(lhs<>rhs)
			Case "<"  r=(lhs< rhs)
			Case "<=", "=<" r=(lhs<=rhs)
			Case ">"  r=(lhs> rhs)
			Case ">=", "=>" r=(lhs>=rhs)
			End Select
		Else If TFloatType( ty )
			Local lhs:Float=Float( Self.lhs.Eval() )
			Local rhs:Float=Float( Self.rhs.Eval() )
			Select op
			Case "="  r=(lhs= rhs)
			Case "<>" r=(lhs<>rhs)
			Case "<"  r=(lhs< rhs)
			Case "<=", "=<" r=(lhs<=rhs)
			Case ">"  r=(lhs> rhs)
			Case ">=", "=>" r=(lhs>=rhs)
			End Select
		Else If TStringType( ty )
			Local lhs:String=String( Self.lhs.Eval() )
			Local rhs:String=String( Self.rhs.Eval() )
			Select op
			Case "="  r=(lhs= rhs)
			Case "<>" r=(lhs<>rhs)
			Case "<"  r=(lhs< rhs)
			Case "<=", "=<" r=(lhs<=rhs)
			Case ">"  r=(lhs> rhs)
			Case ">=", "=>" r=(lhs>=rhs)
			End Select
		EndIf
		If r=1 Return "1"
		If r=0 Return ""
		InternalErr
	End Method
End Type

'and, or
Type TBinaryLogicExpr Extends TBinaryExpr

	Method Create:TBinaryLogicExpr( op$,lhs:TExpr,rhs:TExpr )
		Self.op=op
		Self.lhs=lhs
		Self.rhs=rhs
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TBinaryLogicExpr.Create( op,CopyExpr(lhs),CopyExpr(rhs) )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		lhs=lhs.SemantAndCast( New TBoolType,CAST_EXPLICIT )
		rhs=rhs.SemantAndCast( New TBoolType,CAST_EXPLICIT )

		exprType=New TBoolType

		If TConstExpr( lhs ) And TConstExpr( rhs ) Return EvalConst()

		Return Self
	End Method

	Method Eval$()
		Select op
		Case "and" If lhs.Eval() And rhs.Eval() Return "1" Else Return ""
		Case "or"  If lhs.Eval() Or rhs.Eval() Return "1" Else Return ""
		End Select
		InternalErr
	End Method
End Type

Type TIndexExpr Extends TExpr
	Field expr:TExpr
	Field index:TExpr[]

	Method Create:TIndexExpr( expr:TExpr,index:TExpr[] )
		Self.expr=expr
		Self.index=index
		Return Self
	End Method

	Method Copy:TExpr()
		Local ind:TExpr[]
		For Local i:Int = 0 Until index.length
			ind = ind + [CopyExpr(index[i])]
		Next
		Return New TIndexExpr.Create( CopyExpr(expr),ind )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		expr=expr.Semant()
		For Local i:Int = 0 Until index.length
			If opt_arch = "x64" Then
				index[i]=index[i].SemantAndCast( New TLongType )
			Else
				index[i]=index[i].SemantAndCast( New TIntType )
			End If
		Next

		If TStringType( expr.exprType )
			exprType=New TIntType
			If index.length > 1 Then
				Err "Illegal subexpression for string index"
			End If
		Else If TArrayType( expr.exprType )
			exprType= TArrayType( expr.exprType ).elemType

			If TArrayType( expr.exprType ).dims > 1 Then
				' a multi-dimensional array of arrays is slightly more complex
				If TArrayType(exprType) Then

				'	Local tmpArr:TLocalDecl=New TLocalDecl.Create( "", NewPointerType(TType.T_ARRAY), expr )
				'	Local stmt:TExpr = New TStmtExpr.Create( New TDeclStmt.Create( tmp ), Self ).Semant()




					Local sizeExpr:TExpr = New TArraySizeExpr.Create(expr, Null, index)
					index = [sizeExpr]
					Local tmp:TLocalDecl=New TLocalDecl.Create( "", NewPointerType(TType.T_INT), sizeExpr )
					TArraySizeExpr(sizeExpr).val = tmp
					Local stmt:TExpr = New TStmtExpr.Create( New TDeclStmt.Create( tmp ), Self ).Semant()
					stmt.exprType = exprType

					Return stmt
				Else
					Local sizeExpr:TExpr = New TArraySizeExpr.Create(expr, Null, index)
					index = [sizeExpr]
					Local tmp:TLocalDecl=New TLocalDecl.Create( "", NewPointerType(TType.T_INT), sizeExpr )
					TArraySizeExpr(sizeExpr).val = tmp
					Local stmt:TExpr = New TStmtExpr.Create( New TDeclStmt.Create( tmp ), Self ).Semant()
					stmt.exprType = exprType
					Return stmt
				End If
			End If
			'If TObjectType(exprType) And Not TStringType(exprType) And Not TArrayType(exprType) Then
			'	Local tmp:TLocalDecl=New TLocalDecl.Create( "", exprType,expr )
			'	Local stmt:TExpr = New TStmtExpr.Create( New TDeclStmt.Create( tmp ),New TVarExpr.Create( tmp ) ).Semant()
			'	stmt.exprType = exprType
			'	Return stmt
			'End If
		Else If TNumericType(expr.exprType) And IsPointerType( expr.exprType, 0 , TType.T_POINTER | TType.T_VARPTR)' And Not TFunctionPtrType( expr.exprType )
			exprType=TType.MapPointerToPrim(TNumericType(expr.exprType))
			'exprType=TType.intType
		Else
			Err "Only strings, arrays and pointers may be indexed."
		EndIf

		Return Self
	End Method

	Method SemantSet:TExpr( op$,rhs:TExpr )
		Return Semant()
		'Return Self
	End Method
	
	Method SemantFunc:TExpr( args:TExpr[] , throwError:Int = True, funcCall:Int = False )
		Local ex:TExpr = Semant()
		
		If TArrayType( expr.exprType ) And TFunctionPtrType(exprType) Then
			exprType = TFunctionPtrType(exprType).func.retType
		End If
		
		Return ex
	End Method


	Method Trans$()
		Return _trans.TransIndexExpr( Self )
	End Method

	Method TransVar$()
		Return _trans.TransIndexExpr( Self )
	End Method

	Method ToString$()
		Return "<TIndexExpr<"+ expr.ToString() +"[" + index[0].ToString() + "]>>"
	End Method
	
End Type

Type TSliceExpr Extends TExpr
	Field expr:TExpr
	Field from:TExpr
	Field term:TExpr

	Method Create:TSliceExpr( expr:TExpr,from:TExpr,term:TExpr )
		Self.expr=expr
		Self.from=from
		Self.term=term
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TSliceExpr.Create( CopyExpr(expr),CopyExpr(from),CopyExpr(term) )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		expr=expr.Semant()
		If TArrayType( expr.exprType ) Or TStringType( expr.exprType )
			If from from=from.SemantAndCast( New TIntType )
			If term term=term.SemantAndCast( New TIntType )
			exprType=expr.exprType
		Else
			Err "Slices can only be used on strings or arrays."
		EndIf

'		If TConstExpr( expr ) And TConstExpr( from ) And TConstExpr( term ) Return EvalConst()

		Return Self
	End Method

	Method Eval$()
		Local from:Int=Int( Self.from.Eval() )
		Local term:Int=Int( Self.term.Eval() )
		If TStringType( expr.exprType )
			Return expr.Eval()[ from..term ]
		Else If TArrayType( expr.exprType )
			Todo
		EndIf
	End Method

	Method Trans$()
		Return _trans.TransSliceExpr( Self )
	End Method
End Type

Type TArrayExpr Extends TExpr
	Field exprs:TExpr[]

	Method Create:TArrayExpr( exprs:TExpr[] )
		Self.exprs=exprs
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TArrayExpr.Create( CopyArgs(exprs) )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		exprs[0]=exprs[0].Semant()
		Local ty:TType=exprs[0].exprType
		
		If TInvokeExpr(exprs[0]) And Not TInvokeExpr(exprs[0]).invokedWithBraces Then
			ty = New TFunctionPtrType
			Local cp:TDecl = TInvokeExpr(exprs[0]).decl
			TInvokeExpr(exprs[0]).decl = TFuncDecl(TInvokeExpr(exprs[0]).decl.Copy())
			TInvokeExpr(exprs[0]).decl.actual = cp
			TInvokeExpr(exprs[0]).decl.attrs :| FUNC_PTR
			TFunctionPtrType(ty).func = TInvokeExpr(exprs[0]).decl

			For Local i:Int=1 Until exprs.Length
				exprs[i]=exprs[i].Semant()
				
				If TInvokeExpr(exprs[i]) And Not TInvokeExpr(exprs[i]).invokedWithBraces
					cp = TInvokeExpr(exprs[i]).decl
					
					TInvokeExpr(exprs[i]).decl = TFuncDecl(TInvokeExpr(exprs[i]).decl.Copy())
					TInvokeExpr(exprs[i]).decl.actual = cp
					TInvokeExpr(exprs[i]).decl.attrs :| FUNC_PTR
					
					ty=BalanceTypes( ty, New TFunctionPtrType )
				Else
					ty=BalanceTypes( ty,exprs[i].exprType )
				End If
			Next
		Else
			For Local i:Int=1 Until exprs.Length
				exprs[i]=exprs[i].Semant()
				ty=BalanceTypes( ty,exprs[i].exprType )
			Next
		End If

		For Local i:Int=0 Until exprs.Length
			exprs[i]=exprs[i].Cast( ty )
		Next

		exprType=New TArrayType.Create( ty )
		Return Self
	End Method

	Method Trans$()
		Return _trans.TransArrayExpr( Self )
	End Method

End Type

Type TArraySizeExpr Extends TExpr

	Field expr:TExpr
	Field val:TDecl
	Field index:TExpr[]

	Method Create:TArraySizeExpr( expr:TExpr, val:TDecl, index:TExpr[] )
		Self.expr=expr
		Self.val=val
		Self.index=index
		Return Self
	End Method

	Method Copy:TExpr()
		Local ind:TExpr[]
		For Local i:Int = 0 Until index.length
			ind = ind + [CopyExpr(index[i])]
		Next
		Return New TArraySizeExpr.Create( CopyExpr(expr), val, ind )
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		expr=expr.Semant()
		
		For Local i:Int = 0 Until index.length
			index[i]=index[i].SemantAndCast( New TIntType )
		Next
		
		exprType=NewPointerType(TType.T_INT)
		Return Self
	End Method

	Method Trans$()
		Return _trans.TransArraySizeExpr( Self )
	End Method

	Method ToString$()
		Return expr.ToString() + ".Size"
	End Method

End Type

Type TIdentTypeExpr Extends TExpr
	Field cdecl:TClassDecl

	Method Create:TIdentTypeExpr( ty:TType )
		Self.exprType=ty
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TIdentTypeExpr.Create( exprType )
	End Method

	Method _Semant()
		If cdecl Return
		exprType=exprType.Semant()
		If TArrayType(exprType) And TObjectType(TArrayType(exprType).elemType) Then
			cdecl=TObjectType(TArrayType(exprType).elemType).classDecl
		Else
			cdecl=exprType.GetClass()
		End If
		If Not cdecl InternalErr
	End Method

	Method Semant:TExpr()
		_Semant
		Err "Expression can't be used in this way"
	End Method

	Method SemantFunc:TExpr( args:TExpr[] , throwError:Int = True, funcCall:Int = False )
		_Semant
		If args.Length=1 And args[0] Then
			If TArrayType(exprType) Then
				Return args[0].Cast( exprType,CAST_EXPLICIT )
			Else
				Return args[0].Cast( cdecl.objectType,CAST_EXPLICIT )
			End If
		End If
		Err "Illegal number of arguments for type conversion"
	End Method

	Method SemantScope:TScopeDecl()
		_Semant
		Return cdecl
	End	Method

End Type

Type TIdentExpr Extends TExpr
	Field ident$
	Field expr:TExpr
	Field scope:TScopeDecl
	Field static:Int
	Field isArg:Int

	Method Create:TIdentExpr( ident$,expr:TExpr=Null )
		Self.ident=ident
		Self.expr=expr
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TIdentExpr.Create( ident,CopyExpr(expr) )
	End Method

	Method ToString$()
		Local t$="TIdentExpr(~q"+ident+"~q"
		If expr t:+","+expr.ToString()
		Return t+")"
	End Method

	Method _Semant()

		If scope Return

		If expr Then
			scope=expr.SemantScope()
			If scope
				static=True
			Else
				expr=expr.Semant()
				scope=expr.exprType.GetClass()
				If Not scope Then
					DebugStop
					Err "Expression has no scope"
				End If
			End If
		Else
			scope=_env
			' determines if access is via static (like Function, or via a Type)
			' However, for Field->Field access this is not strictly true.
			static=_env.FuncScope()=Null Or _env.FuncScope().IsStatic()
		End If

	End Method

	Method IdentScope:TScopeDecl()
		If Not expr Return _env

		Local scope:TScopeDecl=expr.SemantScope()
		If scope
			expr=Null
		Else
			expr=expr.Semant()
			scope=expr.exprType.GetClass()
			If Not scope Err "Expression has no scope."
		EndIf
		Return scope
	End Method

	Method IdentErr( )
		If scope
			Local close$
			For Local decl:TDecl=EachIn scope.Decls()
				If ident.ToLower()=decl.ident.ToLower()
					close=decl.ident
				EndIf
			Next
			If close And ident<>close Then
				Err "Identifier '"+ident+"' not found - perhaps you meant '"+close+"'?"
			EndIf
		EndIf
		Err "Identifier '"+ident+"' not found."
	End Method

	Method IdentNotFound()
	End Method

	Method IsVar()
		InternalErr
	End Method

	Method Semant:TExpr()
		Return SemantSet( "",Null )
	End Method

	Method SemantSet:TExpr( op$,rhs:TExpr )
		_Semant

		'Local scope:TScopeDecl=IdentScope()
		Local vdecl:TValDecl=scope.FindValDecl( ident, static )
		If vdecl

			If TConstDecl( vdecl )
'				If rhs Err "Constant '"+ident+"' cannot be modified."
'				Return New TConstExpr.Create( vdecl.ty,TConstDecl( vdecl ).value ).Semant()
				If rhs Err "Constant '"+ident+"' cannot be modified."
				Local cexpr:TConstExpr =New TConstExpr.Create( vdecl.ty,TConstDecl( vdecl ).value )
				If Not static And (TInvokeExpr( expr ) Or TInvokeMemberExpr( expr )) Return New TStmtExpr.Create( New TExprStmt.Create( expr ),cexpr ).Semant()
				Return cexpr.Semant()

			Else If TFieldDecl( vdecl ) 
				If static Err "Field '"+ident+"' cannot be accessed from here."
				If expr Return New TMemberVarExpr.Create( expr,TVarDecl( vdecl ) ).Semant()
'				If expr Return New TMemberVarExpr.Create( expr,TVarDecl( vdecl ) ).Semant()
'				If scope<>_env Or Not _env.FuncScope() Or _env.FuncScope().IsStatic() Err "Field '"+ident+"' cannot be accessed from here."
			EndIf

			Return New TVarExpr.Create( TVarDecl( vdecl ) ).Semant()
		EndIf

		If op And op<>"="

			Local fdecl:TFuncDecl=scope.FindFuncDecl( ident )
			If Not fdecl IdentErr

			If _env.ModuleScope().IsStrict() And Not fdecl.IsProperty() Err "Identifier '"+ident+"' cannot be used in this way."

			Local lhs:TExpr

			If fdecl.IsStatic() Or (scope=_env And Not _env.FuncScope().IsStatic())
				lhs=New TInvokeExpr.Create( fdecl )
			Else If expr
				Local tmp:TLocalDecl=New TLocalDecl.Create( "",Null,expr )
				lhs=New TInvokeMemberExpr.Create( New TVarExpr.Create( tmp ),fdecl )
				lhs=New TStmtExpr.Create( New TDeclStmt.Create( tmp ),lhs )
			Else
				Return Null
			EndIf

			Local bop$=op[..1]
			Select bop
			Case "*","/","shl","shr","+","-","&","|","~~"
				rhs=New TBinaryMathExpr.Create( bop,lhs,rhs )
			Default
				InternalErr
			End Select
			rhs=rhs.Semant()
		EndIf

		Local args:TExpr[]
		If rhs args=[rhs]

		Local fdecl:TFuncDecl=scope.FindFuncDecl( ident,args, , isArg )

		If fdecl
			If _env.ModuleScope().IsStrict() And Not fdecl.IsProperty() And Not isArg And Not fdecl.maybeFunctionPtr Err "Identifier '"+ident+"' cannot be used in this way."

			fdecl.maybeFunctionPtr = False
			
			If Not fdecl.IsStatic()
				If expr Return New TInvokeMemberExpr.Create( expr,fdecl,args ).Semant()
				If scope<>_env Or Not _env.FuncScope() Or _env.FuncScope().IsStatic() Err "Method '"+ident+"' cannot be accessed from here."
			EndIf

			Return New TInvokeExpr.Create( fdecl,args, False ).Semant()
		End If
		
		' maybe it's a classdecl?
		Local cdecl:TClassDecl = TClassDecl(scope.FindDecl(ident))
		
		If cdecl Then
			Local e:TIdentTypeExpr = New TIdentTypeExpr.Create(cdecl.objectType)
			e.cdecl = cdecl
			Return e
		End If

		' maybe it's a loop label?
		Local stmt:TLoopStmt = TLoopStmt(scope.FindLoop(ident))
		
		If stmt Then
			Return New TLoopLabelExpr.Create(stmt)
		End If
		
		' maybe it's a data label?
		Local ddecl:TDefDataDecl = TDefDataDecl(_appInstance.FindDataLabel(ident))
		
		If ddecl Then
			Return New TDataLabelExpr.Create(ddecl)
		End If
		
		IdentErr
	End Method

	Method SemantFunc:TExpr( args:TExpr[], throwError:Int = True, funcCall:Int = False )

		_Semant

		'Local scope:TScopeDecl=IdentScope()
		Local fdecl:TFuncDecl=scope.FindFuncDecl( ident,args )

		' couldn't find it? try a global search
		If Not fdecl Then
			For Local mdecl:TModuleDecl = EachIn _appInstance.globalImports.Values()
				fdecl=mdecl.FindFuncDecl( ident, args )
				If fdecl Exit
			Next
		End If

		If fdecl
			If Not fdecl.IsStatic()
				If static Err "Method '"+ident+"' cannot be accessed from here."
				If expr Return New TInvokeMemberExpr.Create( expr,fdecl,args ).Semant()
				'If scope<>_env Or _env.FuncScope().IsStatic() Err "Method '"+ident+"' cannot be accessed from here."
			EndIf
			If expr And Not static Then
				Return New TInvokeMemberExpr.Create( expr,fdecl,args ).Semant()
			Else
				Return New TInvokeExpr.Create( fdecl,args, funcCall ).Semant()
			End If
		EndIf

		'If args.Length=1 And args[0] And TObjectType( args[0].exprType )
		'	Local cdecl:TClassDecl=TClassDecl( scope.FindScopeDecl( ident ) )
		'	If cdecl Return args[0].Cast( New TObjectType.Create(cdecl),CAST_EXPLICIT )
		'EndIf

		Local ty:TType=scope.FindType( ident,Null )
		If ty Then
			If args.Length=1 And args[0] Return args[0].Cast( ty,CAST_EXPLICIT )
			Err "Illegal number of arguments for type conversion"
		End If

		If throwError IdentErr
	End Method

	Method SemantScope:TScopeDecl()
		If Not expr Return _env.FindScopeDecl( ident )
		Local scope:TScopeDecl=expr.SemantScope()

		' If scope is a namespace, then we are a module. Look up the module id and return it as the real scope.
		If TNamespaceDecl(scope) Then
			Local mdecl:TModuleDecl=TModuleDecl(scope.FindDecl(scope.ident + "." + ident))
			If mdecl Then
				Return mdecl
			End If
		End If

		If scope Return scope.FindScopeDecl( ident )
	End Method

'	Method Trans$()
'		Return _trans.TransIdentExpr( Self )
'	End Method

End Type

Type TBuiltinExpr Extends TExpr

	Field id:String
	Field expr:TExpr

	Method Semant:TExpr()
		If exprType Return Self

		expr=expr.Semant()
		exprType=expr.exprType
		Return Self
	End Method

	Method Trans$()
		Return _trans.TransBuiltinExpr( Self )
	End Method

End Type

Type TLenExpr Extends TBuiltinExpr

	Method Create:TLenExpr( expr:TExpr )
		Self.id="len"
		Self.expr=expr
		Return Self
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		expr=expr.Semant()

		' anything other than a string or array will become "1", and
		' return a length of 1 accordingly.
		If Not TStringType(expr.exprType) And Not TArrayType(expr.exprType) Then
			expr = New TConstExpr.Create( New TIntType, 1 ).Semant()
			'this is not useful for numerics
			'expr = New TConstExpr.Create( TType.stringType, "1" ).Semant()
			_appInstance.mapStringConsts(TConstExpr(expr).value)
		End If

		exprType=New TIntType
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TLenExpr.Create( CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TLenExpr("+expr.ToString()+")"
	End Method

End Type

Type TAbsExpr Extends TBuiltinExpr

	Method Create:TAbsExpr( expr:TExpr )
		Self.id="abs"
		Self.expr=expr
		Return Self
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		expr=expr.Semant()

		If TIntType(expr.exprType) Or TByteType(expr.exprType) Or TShortType(expr.exprType) Then
			exprType=New TIntType
		Else If TLongType(expr.exprType) Then
			exprType=New TLongType
		Else
			exprType=New TDoubleType
		End If

		Return Self
	End Method

	Method Copy:TExpr()
		Return New TAbsExpr.Create( CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TAbsExpr("+expr.ToString()+")"
	End Method

End Type

Type TAscExpr Extends TBuiltinExpr

	Method Create:TAscExpr( expr:TExpr )
		Self.id="asc"
		Self.expr=expr
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TAscExpr.Create( CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TAscExpr("+expr.ToString()+")"
	End Method

End Type

Type TSgnExpr Extends TBuiltinExpr

	Method Create:TSgnExpr( expr:TExpr )
		Self.id="sgn"
		Self.expr=expr
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TSgnExpr.Create( CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TSgnExpr("+expr.ToString()+")"
	End Method

End Type

Type TMinExpr Extends TBuiltinExpr

	Field expr2:TExpr

	Method Create:TMinExpr( lhs:TExpr, rhs:TExpr )
		Self.id="min"
		Self.expr=lhs
		Self.expr2=rhs
		Return Self
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		expr=expr.Semant()
		expr2=expr2.Semant()

		exprType=BalanceTypes(expr.exprType, expr2.exprType)
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TMinExpr.Create( CopyExpr(expr), CopyExpr(expr2) )
	End Method

	Method ToString$()
		Return "TMinExpr("+expr.ToString()+"," + expr2.ToString() + ")"
	End Method

End Type

Type TMaxExpr Extends TBuiltinExpr

	Field expr2:TExpr

	Method Create:TMaxExpr( lhs:TExpr, rhs:TExpr )
		Self.id="max"
		Self.expr=lhs
		Self.expr2=rhs
		Return Self
	End Method

	Method Semant:TExpr()
		If exprType Return Self

		expr=expr.Semant()
		expr2=expr2.Semant()

		exprType=BalanceTypes(expr.exprType, expr2.exprType)
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TMaxExpr.Create( CopyExpr(expr), CopyExpr(expr2) )
	End Method

	Method ToString$()
		Return "TMaxExpr("+expr.ToString()+"," + expr2.ToString() + ")"
	End Method

End Type

Type TSizeOfExpr Extends TBuiltinExpr

	Method Create:TSizeOfExpr( expr:TExpr )
		Self.id="sizeof"
		Self.expr=expr
		Return Self
	End Method

	Method Semant:TExpr()
		If exprType Return Self
		expr=expr.Semant()
		exprType=New TIntType
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TSizeOfExpr.Create( CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TSizeOfExpr("+expr.ToString()+")"
	End Method

End Type

Type TFuncCallExpr Extends TExpr
	Field expr:TExpr
	Field args:TExpr[]

	Method Create:TFuncCallExpr( expr:TExpr,args:TExpr[]=Null )
		Self.expr=expr
		If args Then
			Self.args=args
		Else
			Self.args = New TExpr[0]
		End If
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TFuncCallExpr.Create( CopyExpr(expr),CopyArgs(args) )
	End Method

	Method ToString$()
		Local t$="TFuncCallExpr("+expr.ToString()
		For Local arg:TExpr=EachIn args
			t:+","+arg.ToString()
		Next
		Return t+")"
	End Method

	Method Semant:TExpr()
		args=SemantArgs( args )
		If TIndexExpr(expr) Then
			expr = expr.SemantFunc( args, True, True )
			exprType = expr.exprType
			Return Self
		Else
			Return expr.SemantFunc( args, True, True )
		End If
	End Method

	Method Trans$()
		Return _trans.TransFuncCallExpr( Self )
	End Method

End Type

Type TScopeExpr Extends TExpr
	Field scope:TScopeDecl

	Method Create:TScopeExpr( scope:TScopeDecl )
		Self.scope=scope
		Return Self
	End Method

	Method Copy:TExpr()
		Return Self
	End Method

	Method ToString$()
		Return "TScopeExpr("+scope.ToString()+")"
	End Method

	Method Semant:TExpr()
		Err "Syntax error."
	End Method

	Method SemantScope:TScopeDecl()
		Return scope
	End Method
End Type

Type TNullExpr Extends TExpr

	Method Create:TNullExpr(ty:TType)
		exprType = ty
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TNullExpr.Create(exprType)
	End Method

	Method Semant:TExpr()
		Return Self
	End Method

	Method Trans$()
		Return "NULL"
	End Method

	Method Eval$()
		Return ""
	End Method

End Type

Type TLoopLabelExpr Extends TExpr

	Field loop:TLoopStmt

	Method Create:TLoopLabelExpr(loop:TLoopStmt)
		Self.loop = loop
		Return Self
	End Method
	
	Method Copy:TExpr()
		Return New TLoopLabelExpr.Create(loop)
	End Method

	Method Semant:TExpr()
		Return Self
	End Method

	Method Trans$()
		DebugStop
	End Method

	Method Eval$()
		Return ""
	End Method

End Type

Type TDataLabelExpr Extends TExpr

	Field dataDef:TDefDataDecl
	
	Method Create:TDataLabelExpr(dataDef:TDefDataDecl)
		Self.dataDef = dataDef
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TDataLabelExpr.Create(dataDef)
	End Method

	Method Semant:TExpr()
		Return Self
	End Method

	Method Trans$()
		DebugStop
	End Method

	Method Eval$()
		Return ""
	End Method

End Type
