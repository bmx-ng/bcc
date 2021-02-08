' Copyright (c) 2013-2021 Bruce A Henderson
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
	Field static:Int

	Method ToString$()
		Return "<TExpr>"
	End Method

	Method Copy:TExpr()
		InternalErr "TExpr.Copy"
	End Method

	Method Semant:TExpr(options:Int = 0)
		InternalErr "TExpr.Semant"
	End Method

	Method SemantSet:TExpr( op$,rhs:TExpr, options:Int = 0 )
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
		InternalErr "TExpr.TransVar"
	End Method

	'semant and cast
	Method SemantAndCast:TExpr( ty:TType,castFlags:Int=0 )
		Local expr:TExpr=Semant()
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

				' if an arg is a invocation without braces, it is *probably* a function pointer.
				If TInvokeExpr(args[i]) And Not TInvokeExpr(args[i]).invokedWithBraces Then
					' but not if we've already processed it...
					If Not (TInvokeExpr(args[i]).decl.attrs & FUNC_PTR) Then
						TInvokeExpr(args[i]).exprType = New TFunctionPtrType
						Local cp:TDecl = TInvokeExpr(args[i]).decl
						cp.Semant
						TInvokeExpr(args[i]).decl = TFuncDecl(TInvokeExpr(args[i]).decl.Copy(False))
						TInvokeExpr(args[i]).decl.actual = cp
						TInvokeExpr(args[i]).decl.attrs :| FUNC_PTR
						TFunctionPtrType(TInvokeExpr(args[i]).exprType).func = TInvokeExpr(args[i]).decl

						TInvokeExpr(args[i]).decl.semant()
					End If
				End If
				
			End If
		Next
		Return args
	End Method

	Method CastArgs:TExpr[]( args:TExpr[],funcDecl:TFuncDecl )
		If args.Length>funcDecl.argDecls.Length Then
			Err "Too many function parameters"
		End If

		' FIXME
		'args=args.Resize( funcDecl.argDecls.Length )
		' FIXME

		For Local i:Int=0 Until funcDecl.argDecls.Length
			' ensure funcdecl args are semanted before trying to use them.
			If Not funcDecl.argDecls[i].IsSemanted() Then
				funcDecl.argDecls[i].Semant()
			End If
			
			If i < args.length And args[i]

				Local argExpr:TExpr = args[i]

				If TInvokeExpr(argExpr) And Not TInvokeExpr(argExpr).invokedWithBraces Then
					If Not IsPointerType(funcDecl.argDecls[i].ty, TType.T_BYTE) And Not TFunctionPtrType(funcDecl.argDecls[i].ty) Then
						Err "Unable to convert from '" + argExpr.exprType.ToString() + "()' to '" + funcDecl.argDecls[i].ty.ToString() + "'"
					End If
				End If

				If TInvokeMemberExpr(argExpr) And Not TInvokeMemberExpr(argExpr).invokedWithBraces Then
					If Not IsPointerType(funcDecl.argDecls[i].ty, TType.T_BYTE) And Not TFunctionPtrType(funcDecl.argDecls[i].ty) Then
						Err "Unable to convert from '" + argExpr.exprType.ToString() + "()' to '" + funcDecl.argDecls[i].ty.ToString() + "'"
					End If
				End If

				If funcDecl.argDecls[i].ty._flags & TType.T_VAR Then

					If TConstExpr(argExpr) Or TBinaryExpr(argExpr) Or (TIndexExpr(argExpr) And TStringType(TIndexExpr(argExpr).expr.exprType)) Or ..
							TInvokeExpr(argExpr) Or TInvokeMemberExpr(argExpr) Or TSelfExpr(argExpr) Then
						Err "Expression for 'Var' parameter must be a variable or an element of an array or pointer"
					End If

					' Passing a "new" object into a Var, requires us to create a local variable and pass its address instead.
					If TNewObjectExpr(argExpr) Then
						Local tmp:TLocalDecl=New TLocalDecl.Create( "",TNewObjectExpr(argExpr).ty,argExpr,, True )
						tmp.Semant()
						Local v:TVarExpr = New TVarExpr.Create( tmp )
						Local stmt:TExpr = New TStmtExpr.Create( New TDeclStmt.Create( tmp ), v ).Semant()
						stmt.exprType = TNewObjectExpr(argExpr).ty
						args[i] = stmt
						argExpr = args[i]
					End If
					
					If TVarExpr(argExpr) Or TMemberVarExpr(argExpr) Then
						Local decl:TDecl
						If TVarExpr(argExpr) Then
							decl = TVarExpr(argExpr).decl
						Else
							decl = TMemberVarExpr(argExpr).decl
						End If
						If decl.IsReadOnly() Then
							If TFieldDecl(decl) Then
								Local scope:TFuncDecl = _env.FuncScope()
								If Not scope Or Not scope.IsCtor() Or (decl.ClassScope() <> scope.ClassScope()) Then
									Err "Expression for 'Var' parameter cannot be a ReadOnly variable"
								End If
							Else
								Err "Expression for 'Var' parameter cannot be a ReadOnly variable"
							End If
						End If
					End If

					' passing a non volatile local as Var from within a Try block?					
					If TVarExpr(argExpr) Then
						Local ldecl:TLocalDecl = TLocalDecl(TVarExpr(argExpr).decl)
						If ldecl And Not ldecl.volatile Then
							Local tryStmtDecl:TTryStmtDecl = _env.FindTry()
							If tryStmtDecl And (Not ldecl.declaredInTry Or tryStmtDecl <> ldecl.declaredInTry) Then
								ldecl.volatile = True
							End If
						End If
					End If
				End If
				
				If (funcDecl.argDecls[i].ty._flags & TType.T_VAR) And Not (funcDecl.argDecls[i].ty.EqualsType(argExpr.exprType)) Then
					If (Not TObjectType(funcDecl.argDecls[i].ty)) Or (TObjectType(funcDecl.argDecls[i].ty) And Not argExpr.exprType.ExtendsType(funcDecl.argDecls[i].ty)) Then
						err "Variable for 'Var' parameter is not of matching type"
					End If
				End If

				' re-test auto array for compatible consts.
				If TArrayExpr(argExpr) And TArrayType(funcDecl.argDecls[i].ty) And TNumericType(TArrayType(funcDecl.argDecls[i].ty).elemType) Then
					TArrayExpr(argExpr).toType = TArrayType(funcDecl.argDecls[i].ty).elemType
					argExpr.exprType = Null
					argExpr.Semant()
				End If
				args[i]=argExpr.Cast( funcDecl.argDecls[i].ty )
			Else If funcDecl.argDecls[i].init
				If i = args.length Then
					' extend args to add default init entry
					args = args[..i + 1]
				End If
				args[i]=funcDecl.argDecls[i].init
			Else
				Err "Missing function argument '"+funcDecl.argDecls[i].ident+"'."
			EndIf
		Next
		Return args
	End Method

	Method BalanceTypes:TType( lhs:TType,rhs:TType )

		If TStringType( lhs ) Or TStringType( rhs ) Then
			If TObjectType(lhs) Or TObjectType(rhs) Then
				If TObjectType(lhs) And TObjectType(lhs).classDecl.ident = "Object" Then
					Return lhs
				End If
				If TObjectType(rhs) And TObjectType(rhs).classDecl.ident = "Object" Then
					Return rhs
				End If
				Return New TStringType
			Else
				Return New TStringType
			End If
		End If
		If IsPointerType( lhs, 0, TType.T_POINTER ) Or IsPointerType( rhs, 0, TType.T_POINTER ) Then
			If IsPointerType( lhs, 0, TType.T_POINTER ) Return lhs
			If IsPointerType( rhs, 0, TType.T_POINTER ) Return rhs
		End If
		If TDouble128Type( lhs ) Or TDouble128Type( rhs ) Return New TDouble128Type
		If TFloat128Type( lhs ) Or TFloat128Type( rhs ) Return New TFloat128Type
		If TFloat64Type( lhs ) Or TFloat64Type( rhs ) Return New TFloat64Type
		If TDoubleType( lhs ) Or TDoubleType( rhs ) Return New TDoubleType
		If TFloatType( lhs ) Or TFloatType( rhs ) Return New TFloatType
		If TFunctionPtrType( lhs ) Or TFunctionPtrType( rhs ) Then
			If TFunctionPtrType( lhs ) Return lhs
			If TFunctionPtrType( rhs ) Return rhs
		End If
		If TInt128Type( lhs ) Or TInt128Type( rhs ) Return New TInt128Type
		If TULongType( lhs ) Or TULongType( rhs ) Return New TULongType
		If TSizeTType( lhs ) Or TSizeTType( rhs ) Return New TSizeTType
		If TWParamType( lhs ) Or TWParamType( rhs ) Return New TWParamType
		If TLongType( lhs ) And TUIntType( rhs ) Return New TULongType
		If TUIntType( lhs ) And TLongType( rhs ) Return New TULongType
		If TLParamType( lhs ) Or TLParamType( rhs ) Return New TLParamType
		If TLongType( lhs ) Or TLongType( rhs ) Return New TLongType
		If TUIntType( lhs ) Or TUIntType( rhs ) Return New TUIntType
		If TIntType( lhs ) Or TIntType( rhs ) Return New TIntType
		If TEnumType( lhs ) Or TEnumType( rhs ) Then
			If TEnumType( lhs ) Return lhs
			If TEnumType( rhs ) Return rhs
		End If
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
		' balance structs
		If TObjectType( lhs ) And TObjectType( rhs ) And TObjectType( lhs ).EqualsType(rhs) And TObjectType( lhs ).classDecl.IsStruct() And TObjectType( rhs ).classDecl.IsStruct() Then
			Return lhs
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
		If exprType Return Self
		Return New TStmtExpr.Create( stmt,CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TStmtExpr(,"+expr.ToString()+")"
	End Method

	Method Semant:TExpr(options:Int = 0)
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
	Field originalValue$
	' True if the const was identified as a specific type.
	Field typeSpecific:Int

	Method Create:TConstExpr( ty:TType,value$ )
		originalValue = value
		
		If TNumericType( ty ) And IsPointerType(ty, 0, TType.T_POINTER) Then
			Self.ty=ty
			If value Then
				Self.value = value
			Else
				Self.value="0"
			End If
			Return Self
		End If
		
		If TIntType( ty ) Or TShortType( ty ) Or TByteType( ty ) Or TLongType( ty ) Or TUIntType( ty ) Or TULongType( ty ) Or TWParamType(ty) Or TLParamType(ty)
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
				If TIntType(ty) And val >= 2147483648:Long Then
					value = String( -2147483648:Long + (val - 2147483648:Long))
				Else
					If TShortType( ty ) Then
						value=String( Short(val) )
					Else If TByteType( ty ) Then
						value=String( Byte(val) )
					Else
						value=String( val )
					End If
				End If
			Else
				If TShortType( ty ) Then
					value = String.FromLong(Short(value.ToLong()))
				Else If TByteType( ty ) Then
					value = String.FromLong(Byte(value.ToLong()))
				Else
					Local buf:Byte[64]
					Local b:Int
					Local v:String = value.Trim()
					Local leading0:Int = True
					If v Then
						Local i:Int
						If v[0] = Asc("+") Then
							i = 1
						Else If v[0] = Asc("-") Then
							i = 1
							buf[b] = Asc("-")
							b:+ 1
						End If
						
						While i < value.Length
							If Not IsDigit(v[i]) Then
								Exit
							End If
							If leading0 And v[i] = Asc("0") Then
								i :+ 1
								Continue
							End If
							leading0 = False
							buf[b] = v[i]
							
							b :+ 1
							i :+ 1
						Wend
						
						If leading0 Then
							value = "0"
						Else
							value = String.FromBytes(buf, b)
						End If
					Else
						value = "0"
					End If
				End If
			EndIf

		Else If TDecimalType( ty )
			If Not (value.Contains("e") Or value.Contains("E") Or value.Contains(".") Or value.Contains("inf") Or value.Contains("nan"))
				If TFloatType(ty) Then
					value:+".00000000"
				Else
					value:+".0000000000000000"
				End If
			EndIf
		EndIf
		Self.ty=ty
		Self.value=value
		Return Self
	End Method

	Method UpdateType(ty:TType)
		typeSpecific = True
		Create(ty, originalValue)
	End Method

	Method Copy:TExpr()
		Local e:TConstExpr = New TConstExpr.Create( ty,value )
		e.originalValue = originalValue
		e.typeSpecific = typeSpecific
		Return e
	End Method

	Method ToString$()
		Return "TConstExpr(~q"+value+"~q)"
	End Method

	Method Semant:TExpr(options:Int = 0)
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
	
	Method CompatibleWithType:Int(ty:TType)
		If Not TDecimalType(ty) Then
			If value.Contains("e") Or value.Contains("E") Or value.Contains(".") Or value.Contains("inf") Or value.Contains("nan") Then
				Return False
			End If
			
			Local val:Long = value.ToLong()
			
			If val < 0 Then
				If TByteType(ty) Or TShortType(ty) Or TUIntType(ty) Or TULongType(ty) Or TSizeTType(ty) Or TInt128Type(ty) Or TWParamType(ty) Then
					Return False
				End If
			Else
				If TByteType(ty) Then
					If value <> String.FromInt(Byte(Val)) Then
						Return False
					End If
				End If

				If TUIntType(ty) Or ((TSizeTType(ty) Or TWParamType(ty)) And WORD_SIZE = 4) Then
					If val > 4294967296:Long Then
						Return False
					End If
				End If
				
				If TULongType(ty) Or ((TSizeTType(ty) Or TWParamType(ty)) And WORD_SIZE = 8) Then
					If value.length > 20 Then
						Return False
					Else If value.length = 20 Then
						For Local i:Int = 0 Until value.length
							Local v:Int = value[i]
							Local n:Int = "18446744073709551616"[i]
							If v < n Then
								Exit 
							Else If v > n Then
								Return False
							End If
						Next
					End If
				End If
			End If
			
			If TShortType(ty) Then
				If value <> String.FromInt(Short(val)) Then
					Return False
				End If
			End If

			If TIntType(ty) Or (TLParamType(ty) And WORD_SIZE = 4) Then
				If value <> String.FromInt(Int(val)) Then
					Return False
				End If
			End If

			If TLongType(ty) Or (TLParamType(ty) And WORD_SIZE = 8) Then
				If value <> String.FromLong(Long(val)) Then
					Return False
				End If
			End If
			
		End If
		
		Return True
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

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self
		If Not decl.IsSemanted() InternalErr "TVarExpr.Semant"
		exprType=decl.ty
		Return Self
	End Method

	Method SemantSet:TExpr( op$,rhs:TExpr, options:Int = 0 )
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

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self
		If Not decl.IsSemanted() InternalErr "TMemberVarExpr.Semant"
		exprType=decl.ty.Semant()
		Return Self
	End Method

	Method SemantSet:TExpr( op$,rhs:TExpr, options:Int = 0 )
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
	Field isArg:Int
	Field isRhs:Int

	Method Create:TInvokeExpr( decl:TFuncDecl,args:TExpr[]=Null,invokedWithBraces:Int=True, isArg:Int=False, isRhs:Int = False )
		Self.decl=decl
		If args Then
			Self.args=args
		Else
			Self.args = New TExpr[0]
		End If
		Self.invokedWithBraces = invokedWithBraces
		Self.isArg = isArg
		Self.isRhs = isRhs
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

	Method Semant:TExpr(options:Int = 0)

		If exprType Return Self

		If Not decl.retType
			decl.Semant()
		End If
		'If TIdentType(decl.retType) Then
			exprType = decl.retType.Semant()
		'Else
		'	exprType=decl.retType
		'End If

		'If ((isArg Or isRhs) And Not invokedWithBraces) And (args = Null Or args.length = 0) Then

		' if the call was a statement (even one written without parentheses), then invokedWithBraces is true
		' so no complicated checks are needed here; if invokedWithBraces is false, this is definitely not a call
		If Not invokedWithBraces Then
			' nothing to do here, as we are a function pointer. i.e. no braces
			' and our expression type is a function ptr...
			exprType = New TFunctionPtrType.Create(decl)
			
		Else
			args=CastArgs( args,decl )
		End If
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
		Return Super.Eval()
	End Method

End Type

Type TInvokeMemberExpr Extends TExpr
	Field expr:TExpr
	Field decl:TFuncDecl
	Field args:TExpr[]
	Field isResize:Int	'FIXME - butt ugly!
	Field invokedWithBraces:Int

	Method Create:TInvokeMemberExpr( expr:TExpr,decl:TFuncDecl,args:TExpr[]=Null, invokedWithBraces:Int = True )
		Self.expr=expr
		Self.decl=decl
		If args
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
		Local t$="TInvokeMemberExpr("+expr.ToString()+","+decl.ToString()
		For Local arg:TExpr=EachIn args
			t:+","+arg.ToString()
		Next
		Return t+")"
	End Method

	Method Semant:TExpr(options:Int = 0)
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

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		Local it:TIdentType = TIdentType(ty)
		Local iArgs:TExpr[] = SemantArgs(CopyArgs(args))

		Local isNewSelf:Int = (it And it.ident = "self")
		
		ty=ty.Semant(True)
		If Not ty Then
			' maybe it's an instance of a type ?
			Local decl:TVarDecl = TVarDecl(_env.FindDecl(it.ident))
			If decl And TObjectType(decl.ty) Then
				' this legacy feature is deprecated. Issue a warning but let it go for now...
				ty = decl.ty
				instanceExpr = New TVarExpr.Create(decl).Semant()
				' 
				Warn("Use of New <Object instance> is deprecated, and support will be removed in a future update.")
			Else
				Err "Type '"+it.ident+"' not found"
			End If
		Else If isNewSelf Then
			Warn("Use of New Self is deprecated, and support will be removed in a future update.")
		End If
		args=SemantArgs( args )

		Local objTy:TObjectType=TObjectType( ty )
		Local clsTy:TClassType=TClassType( ty )
		If Not objTy And Not clsTy
			Err "Expression is not a class."
		EndIf
		
		If objTy And Not objTy.classDecl.Semanted() Then
			objTy.classDecl.Semant()
		End If
		
		' 
		If clsTy And clsTy.instance Then
			instanceExpr = New TSelfExpr.Semant()
		End If

		If objTy Then
			classDecl=objTy.classDecl
		Else
			classDecl=clsTy.classDecl
		End If

		If Not instanceExpr Then
			If classDecl.IsInterface() Err "Cannot create instance of an interface."
			If classDecl.IsAbstract() Err "Cannot create instance of abstract type " + classDecl.ToString() + ..
				", which is either declared Abstract or has (or inherits) an abstract Method."
		End If
		'If classDecl.IsTemplateArg() Err "Cannot create instance of a generic argument."
		If classDecl.args And Not classDecl.instanceof Err "Cannot create instance of a generic class."

		Local parts:String[]
		If it Then
			parts = it.ident.ToLower().Split(".")
		End If

		If classDecl.IsExtern()
			Err "Cannot create instance of an extern type"
			'If args Err "No suitable constructor found for class "+classDecl.ToString()+"."
		Else
			' if the New Type doesn't have extra idents (like a create method), then don't use the args in the search.
			' otherwise, the args are for the constructor.
			If Not parts Or parts.length = 1 Then
				ctor=classDecl.FindFuncDecl( "new",args,,,,,SCOPE_CLASS_HEIRARCHY )
				If Not ctor	Err "No suitable constructor found for class "+classDecl.ToString()+"."
				args=CastArgs( args,ctor )
			Else
				ctor=classDecl.FindFuncDecl( "new",,,,,,SCOPE_CLASS_HEIRARCHY )
				If Not ctor	Err "No suitable constructor found for class "+classDecl.ToString()+"."
			End If
		EndIf

		' New Self doesn't necessarily create an instance of ourself - we might be an instance of
		' a subclass at the time...
		If Not isNewSelf Then
			classDecl.attrs:|CLASS_INSTANCED
		End If

		If TClassType(ty) Then
			exprType=New TObjectType.Create(TClassType(ty).classDecl)
		Else
			exprType=ty
		End If
		
		If it Then
			'Local parts:String[] = it.ident.ToLower().Split(".")

			Local i:Int = 0
			
			While i < parts.length And parts[i] <> classDecl.IdentLower() And parts[i] <> "self"
				i :+ 1
			Wend
			
			i :+ 1

			Local expr:TExpr = Self
			Local cdecl:TClassDecl = classDecl
			Local eType:TType = ty
			
			Local errorDetails:String

			While i < parts.length
				Local id:String = parts[i]
				i :+ 1
				
				' find other member decl (field, etc)
				Local decl:TValDecl = TValDecl(cdecl.GetDecl(id))
				If TVarDecl(decl) Then
					decl.Semant
					Local tmp:TLocalDecl=New TLocalDecl.Create( "", eType, expr,, True )
					Local varExpr:TExpr = New TMemberVarExpr.Create(New TVarExpr.Create( tmp ), TVarDecl(decl)).Semant()
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
				If TConstDecl(decl) Then
					decl.Semant()
					expr = New TConstExpr.Create(decl.ty, TConstDecl(decl).value).Semant()
					eType = decl.ty
					Continue
				End If

				' find member function.method
				Local fdecl:TFuncDecl
				Try
					fdecl = cdecl.FindFuncDecl(id, iArgs,,,,True,SCOPE_CLASS_HEIRARCHY)
				Catch errorMessage:String
					If errorMessage.StartsWith("Compile Error") Then
						Throw errorMessage
					Else
						' couldn't find an exact match, look elsewhere
						If errorMessage.StartsWith("Unable") Then
							errorDetails = errorMessage
						End If
					End If
				End Try
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
				
				' didn't match member or function??
				' probably an error...
				If errorDetails Then
					Err errorDetails
				Else
					Err "Identifier '" + id + "' not found."
				End If
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
		'If exprType InternalErr
		Local cexpr:TExpr[expr.length]
		For Local i:Int = 0 Until expr.length
			cexpr[i] = CopyExpr(expr[i])
		Next
		Return New TNewArrayExpr.Create( ty,cexpr )
	End Method

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		ty=ty.Semant()
		exprType=New TArrayType.Create( ty, expr.length )
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
	
	Field _identLower:String

	Method Create:TInvokeSuperExpr( ident$,args:TExpr[] = Null, _identLower:String = Null )
		Self.ident=ident
		If args Then
			Self.args=args
		Else
			Self.args = New TExpr[0]
		End If
		Self._identLower = _identLower
		Return Self
	End Method

	Method IdentLower:String()
		If Not _identLower Then
			_identLower = ident.ToLower()
		End If
		Return _identLower
	End Method

	Method Copy:TExpr()
		Return New TInvokeSuperExpr.Create( ident,CopyArgs(args), _identLower )
	End Method

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		'If _env.FuncScope().IsStatic() Err "Illegal use of Super."

		classScope=_env.ClassScope()
		superClass=classScope.superClass
		
		If Not superClass Err "Type has no super class."
		
		args=SemantArgs( args )
		Try
			' get the local version of the method from local class scope
			origFuncDecl=classScope.FindFuncDecl(IdentLower(),args,,,,True,SCOPE_CLASS_LOCAL)
		Catch errorMessage:String
			If errorMessage.StartsWith("Compile Error") Then
				Throw errorMessage
			Else
				' if there isn't one, we'll just use a Super version of it anyway as a reference.
				origFuncDecl=classScope.FindFuncDecl(IdentLower(),args,,,,,SCOPE_CLASS_HEIRARCHY)
			End If
		End Try

		funcDecl=superClass.FindFuncDecl( IdentLower(),args,,,,,SCOPE_CLASS_HEIRARCHY )

		If Not funcDecl Err "Can't find superclass method '"+ident+"'."

		' ensure the super function has been semanted
		funcDecl.Semant()
		
		' cannot directly call abstract methods
		If funcDecl.isAbstract() Then
			Err "Abstract method '" + funcDecl.ident + "' cannot be accessed directly."
		End If
		
		' for static scope, we need to change class scope to that of the super class
		If _env.FuncScope().IsStatic() Then
			classScope = TClassDecl(funcDecl.scope)
		End If
		
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
		Local expr:TExpr = New TSelfExpr
		expr.static = static
		Return expr
	End Method

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		If _env.FuncScope().IsStatic() Then
			static = True
		End If
		
		Local scope:TClassDecl = _env.ClassScope()
		If Not scope Then
			Err "'Self' can only be used within methods."
		End If
		
		Local funcScope:TFuncDecl = _env.FuncScope()
		If funcScope.IsAnyMethod() Then
			exprType=New TObjectType.Create( scope )
			TObjectType(exprType).instance = True
		Else
			exprType=New TClassType.Create( scope )
		End If

		Return Self
	End Method

	Method Trans$()
		Return _trans.TransSelfExpr( Self )
	End Method

End Type

Const CAST_EXPLICIT:Int=1
Const CAST_TWO:Int=2

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

	Method CheckArrayExpr:TType(src:TType, e:TExpr, last:TType)
		If TNullType(e.exprType) Then
			Err "Auto array element has no type"
		End If

		If TObjectType(TArrayType(ty).elemType) And TObjectType(TArrayType(ty).elemType).classDecl.ident = "Object" And (TStringType(e.exprType) Or TObjectType(e.exprType) Or TArrayType(e.exprType)) Then
			' array takes generic objects, so we don't care if source elements are the same kinds.
		Else
			If last <> Null And Not last.EqualsType(e.exprType) Then
				Err "Auto array elements must have identical types"
			End If
			If Not TArrayType(ty).elemType.EqualsType(e.exprType) Then
				If (TObjectType(TArrayType(ty).elemType) = Null And TStringType(TArrayType(ty).elemType) = Null) Or (TObjectType(e.exprType) = Null And TStringType(e.exprType) = Null) Then
					Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
				Else If TStringType(e.exprType) = Null And Not TObjectType(e.exprType).ExtendsType(TObjectType(TArrayType(ty).elemType)) Then
					Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
				End If
			End If
		End If
		
		Return e.exprType
	End Method
	
	Method CheckArrayType:Int(ty:TArrayType, src:TArrayType)
		If src.dims <> ty.dims Then
			Return False
		End If
		If TObjectType(TArrayType(ty).elemType) Then
			If TObjectType(TArrayType(ty).elemType).classDecl.ident = "Object" And (TStringType(TArrayType(src).elemType) Or TObjectType(TArrayType(src).elemType) Or TArrayType(TArrayType(src).elemType)) Then
				' array takes generic objects, so we don't care if source elements are the same kinds.
			Else
				If TObjectType(TArrayType(src).elemType) And Not (TObjectType(TArrayType(src).elemType)).ExtendsType(TArrayType(ty).elemType) And Not TArrayType(ty).elemType.EqualsType(TArrayType(src).elemType) Then
					Return False
				End If
			End If
		Else If TArrayType(TArrayType(ty).elemType) And TArrayType(TArrayType(src).elemType) Then
			If Not CheckArrayType(TArrayType(TArrayType(ty).elemType), TArrayType(TArrayType(src).elemType)) Then
				Return False
			End If
		Else If Not TArrayType(ty).elemType.EqualsType(TArrayType(src).elemType) Then
			Return False
		End If
		
		Return True
	End Method

	Method Semant:TExpr(options:Int = 0)

		If exprType Return Self

		ty=ty.Semant()
		
		If TInvokeExpr(expr) Then
			TInvokeExpr(expr).isRhs = True
		Else If TIdentExpr(expr) Then
			TIdentExpr(expr).isRhs = True
		End If
		
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

			If TStringType(ty) And TObjectType(src)
				' only if explicitly cast
				If flags & CAST_EXPLICIT Then
					exprType = ty
					'Return Self
				End If
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
				Else If IsPointerType(ty, 0, TType.T_POINTER)
					exprType = ty
					If flags = CAST_EXPLICIT Then
						Return Self
					Else
						If Not TObjectType( src ).classDecl.IsExtern() Then
							Return Self
						Else
							Return expr
						End If
					End If
				Else
					InternalErr "TCastExpr.Semant"
				EndIf
				Local fdecl:TFuncDecl=src.GetClass().FindFuncDecl( op,,,,,,SCOPE_ALL )
				expr=New TInvokeMemberExpr.Create( expr,fdecl ).Semant()

			EndIf
			
			If TNullType(src) And Not TVoidType(ty) And Not (TArrayType(ty) And TArrayType(ty).IsStatic) Then
				exprType = ty
			End If
			
			If TBoolType(src) And (TNumericType(ty) Or TStringType(ty)) Then
				exprType = ty
			End If
			
			If TNumericType(src) And (TNumericType(ty) Or TStringType(ty)) Then
				' intrinsics can only cast between selves
				If (TIntrinsicType(src) And TIntrinsicType(ty)=Null) Or (TIntrinsicType(ty) And TIntrinsicType(src)=Null) Then
					If TFloat64Type(src) Or TFloat64Type(ty) Then
						If (TFloat64Type(src) And (TLongType(ty) Or TULongType(ty))) Or (TFloat64Type(ty) And (TLongType(src) Or TULongType(src))) Then
							' ok
						Else
							Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
						End If
					Else
						Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
					End If
				Else If TIntrinsicType(src) And TIntrinsicType(ty) Then
					If (TFloat64Type(src) And TFloat64Type(ty)=Null) Or (TFloat64Type(ty) And TFloat64Type(src)=Null) Then
						Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
					End If
				End If
				exprType = ty
			End If
			
			If TObjectType(ty) And (TObjectType(src) Or TStringType(src) Or TArrayType(src)) Then
				exprType = ty
				Return Self
			End If
			
			If TFunctionPtrType(src) And IsPointerType(ty, 0, TType.T_POINTER) Then
				exprType = ty
			End If

		Else If TBoolType( ty )

			If TVoidType( src )
				Err "Cannot convert from Void to Int."
			EndIf

			If  flags & CAST_EXPLICIT
				exprType=ty
			EndIf

		Else If ty.ExtendsType( src )

			If flags & CAST_EXPLICIT

				'if both objects or both non-objects...
				If (TObjectType(ty)<>Null)=(TObjectType(src)<>Null) Then
					exprType=ty
					
					If TFunctionPtrType(ty) And TInvokeExpr(expr) And Not TInvokeExpr(expr).invokedWithBraces Then
						Return expr
					End If
					
					Return Self
				End If
				
				If (TStringType(ty) Or TArrayType(ty)) And TObjectType(src) Then
					exprType=ty
					Return Self
				End If
			'Else ' if not explicitly cast, we can't just auto-cast it ourselves here.
				'If (TObjectType(ty)<>Null) And (TObjectType(src)<>Null) exprType=ty
			EndIf

		EndIf


		If TArrayType(ty) And TArrayType(src) Then
			If TArrayType(ty).dims = TArrayType(src).dims Then
				If TArrayExpr(expr) Then
					Local last:TType
					For Local e:TExpr = EachIn TArrayExpr(expr).exprs
						last = CheckArrayExpr(src, e, last)
					Next
				Else
					If TObjectType(TArrayType(ty).elemType) Then
						If TObjectType(TArrayType(ty).elemType).classDecl.ident = "Object" And (TStringType(TArrayType(src).elemType) Or TObjectType(TArrayType(src).elemType) Or TArrayType(TArrayType(src).elemType)) Then
							' array takes generic objects, so we don't care if source elements are the same kinds.
						Else
							If TObjectType(TArrayType(src).elemType) Then
								If Not (TObjectType(TArrayType(src).elemType)).ExtendsType(TArrayType(ty).elemType) And Not TArrayType(ty).elemType.EqualsType(TArrayType(src).elemType) Then
									Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
								End If
							Else If Not TArrayType(ty).elemType.EqualsType(TArrayType(src).elemType) Then
								Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
							End If
						End If
					Else If TArrayType(TArrayType(ty).elemType) And TArrayType(TArrayType(src).elemType) Then
						If Not CheckArrayType(TArrayType(TArrayType(ty).elemType), TArrayType(TArrayType(src).elemType)) Then
							Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
						End If
					Else If Not TArrayType(ty).elemType.EqualsType(TArrayType(src).elemType) Then
						Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
					End If
				End If
				
				exprType = ty
				Return Self
			End If
		End If

		'If TStringType(src) And TStringVarPtrType(ty) Then
		'	exprType = ty
		'	Return Self
		'End If

'		If TArrayType(src) And TPointerType(ty) Then
'			exprType = ty
'			Return expr
'		End If

		If TFunctionPtrType(ty) And TInvokeExpr(expr) Then
			' a function ptr to function ptr
			If Not TInvokeExpr(expr).invokedWithBraces Then
				src = New TFunctionPtrType
				TFunctionPtrType(src).func = TInvokeExpr(expr).decl

				' signatures should match
				If TInvokeExpr(expr).decl.equalsFunc(TFunctionPtrType(ty).func)  Then
					exprType = ty
					Return expr
				End If
			Else
				' return type should be function ptr?
				Local retType:TType = expr.exprType
				If TFunctionPtrType(retType) And TFunctionPtrType(ty).func.EqualsFunc(TFunctionPtrType(retType).func) Then
					exprType = retType
					Return expr
				End If
			End If
		End If

		'If TIntType(ty) And Not IsPointerType(ty, 0, TType.T_POINTER) And IsPointerType(src, 0, TType.T_POINTER) Then
		'	exprType = ty
		'	If flags & CAST_EXPLICIT Then
		'		Return Self
		'	End If
		'	Return expr
		'End If

		' explicit cast to number
		If IsNumericType(ty) And IsPointerType(src, 0, TType.T_POINTER) Then
			If flags = CAST_EXPLICIT Then
				exprType = ty
				Return Self
			Else
				exprType = Null
			End If
		End If

'		If TPointerType(ty) And TIntType(src) Then
'			exprType = ty
'			Return expr
'		End If

'		If TIntType(ty) And TObjectType(src) Then
' DebugStop ' Bah woz ere
'			exprType = ty
'			Return expr
'		End If

		If TObjectType(src) And TNullDecl(TObjectType(src).classDecl) Then
			exprType = ty
			Return expr
		End If

		If TObjectType(src) And TObjectType(ty) And (ty._flags & TType.T_VAR) Then ' TODO : May be VARPTR instead?
			'exprType = NewPointerType(TType.T_BYTE)
			exprType = ty
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
			Else
				If TArrayType(ty).dims = TArrayType(src).dims Then
					exprType = ty
				End If
			End If
		End If
		
		If TArrayType(ty) And TObjectType(src)
			If TObjectType(src).classDecl.ident = "___Array" Then
				exprType = ty
				Return expr
			Else If TObjectType(src).classDecl.ident = "Object" And flags & CAST_EXPLICIT Then
				exprType = ty
				Return Self
			End If
		End If

		If IsPointerType(ty, 0, TType.T_POINTER | TType.T_CHAR_PTR | TType.T_SHORT_PTR) Then
			If IsNumericType(src) And Not (src._flags & TType.T_VARPTR) Then
			
				' no decimal casts to pointers
				If TDecimalType(src) Then
					exprType = Null
				Else
					exprType = ty
					Return Self
				End If
			Else If TNumericType(src) And (src._flags & TType.T_VARPTR) Then
				exprType = expr.exprType
			Else If TObjectType(src) And (src._flags & TType.T_VARPTR) Then
				exprType = expr.exprType
			Else If TArrayType(src) Then
			
				' for functions and index access, use a new local variable
				If Not TVarExpr(expr) And Not TMemberVarExpr(expr) Then
					Local tmp:TLocalDecl=New TLocalDecl.Create( "", expr.exprType, expr,, True )
					tmp.Semant()
					Local v:TVarExpr = New TVarExpr.Create( tmp )
					expr = New TStmtExpr.Create( New TDeclStmt.Create( tmp ), v ).Semant()
				End If
			
				If TNumericType(TArrayType(src).elemType) Then
					exprType = TNumericType(TArrayType(src).elemType).ToPointer()
					Return Self
				Else
					' map arrays to byte ptr
					exprType = TType.MapToPointerType(New TByteType)
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
			If Not TVarExpr(expr) And Not TMemberVarExpr(expr) And Not (TStmtExpr(expr) And TIndexExpr(TStmtExpr(expr).expr)) Then
				If Not TIndexExpr(expr) Or (TIndexExpr(expr) And Not TVarExpr(TIndexExpr(expr).expr) And Not TMemberVarExpr(TIndexExpr(expr).expr))  Then
					Err "Subexpression for 'Varptr' must be a variable or an element of an array, pointer or string"
				End If
			End If
			exprType = src.Copy()
			exprType._flags :| TType.T_VARPTR
			ty = exprType
			Return Self
		End If
		
		If TFunctionPtrType(ty) And IsPointerType(src, 0, TType.T_POINTER) Then
			exprType = ty
			Return Self
		End If

		If TObjectType(ty) And TObjectType(src) And TObjectType(src).classdecl.IsInterface() And flags & CAST_EXPLICIT Then
			exprType = ty
			Return Self
		End If

		If TObjectType(ty) And TObjectType(src) And TObjectType(ty).classdecl.IsInterface() And flags & CAST_EXPLICIT Then
			exprType = ty
			Return Self
		End If

		If TStringType(ty) And TEnumType(src) Then
			exprType = ty
			Return Self
		End If

		If TEnumType(src) And TEnumType(ty) And (ty._flags & TType.T_VAR) Then
			Return expr
		End If

		If TIntegralType(ty) And TEnumType(src) And (flags & CAST_EXPLICIT Or flags & CAST_TWO) Then
			exprType = ty
			Return Self
		End If
		
		If TIntegralType(src) And TEnumType(ty) Then
			If flags & CAST_TWO Then
				exprType = src
				Return Self
			Else If flags & CAST_EXPLICIT Then
				' validate const
				If TConstExpr( expr ) And Not TEnumType(ty).decl.CastsToEnum(TConstExpr( expr )) Then
					Err "The value " + TConstExpr( expr ).value  + " is not valid for enum " + TEnumType(ty).decl.ToString()
				End If
				exprType = ty
				Return Self
			End If
		End If

		If Not exprType
			Err "Unable to convert from "+src.ToString()+" to "+ty.ToString()+"."
		EndIf

		If TConstExpr( expr ) Then
			If TDecimalType(TConstExpr( expr ).ty) And TDecimalType(ty) Then
				Return New TConstExpr.Create(ty, TConstExpr( expr ).value).Semant()
			End If
			
			Local ex:TExpr = EvalConst()
			If flags & CAST_EXPLICIT Then
				Return New TCastExpr.Create(exprType, ex, CAST_EXPLICIT).Semant()
			Else
				Return ex
			End If
		End If
		
		Return Self
	End Method

	Method Eval$()
		Local val$=expr.Eval()
		If TBoolType( exprType )
			If TIntegralType(expr.exprType)
				If Long( val ) Return "1"
				Return ""
			Else If TDecimalType( expr.exprType )
				If Double( val ) Return "1"
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
		Else If TUIntType( exprType )
			Return Long( val )
		Else If TShortType( exprType )
			Return Short( val )
		Else If TFloatType( exprType )
			Return Float( val )
		Else If TDoubleType( exprType )
			Return Double( val )
		Else If TLongType( exprType )
			Return Long( val )
		Else If TULongType( exprType )
			Return Long( val )
		Else If TSizeTType( exprType )
			Return Long( val )
		Else If TInt128Type( exprType )
			Return Long( val )
		Else If TFloat128Type( exprType )
			Return Float( val )
		Else If TDouble128Type( exprType )
			Return Float( val )
		Else If TFloat64Type( exprType )
			Return Float( val )
		Else If TStringType( exprType )
			If TBoolType( expr.exprType )
				If val Return "1"
				Return "0"
			EndIf
			Return String( val )
		Else If TByteType( exprType )
			Return Byte( val )
		Else If TWParamType( exprType )
			Return Long( val )
		Else If TLParamType( exprType )
			Return Long( val )
		Else If TObjectType( exprType )
			If TStringType( expr.exprType )
				Return val
			End If
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

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		expr = expr.Semant()

		' operator overload?
		If TObjectType(expr.exprType) And (op = "+" Or op = "-" Or op = "~~") Then
			'Local args:TExpr[] = [rhs]
			Try
				Local decl:TFuncDecl = TFuncDecl(TObjectType(expr.exprType).classDecl.FindFuncDecl(op, Null,,,,True,SCOPE_CLASS_HEIRARCHY))
				If decl Then
					Return New TInvokeMemberExpr.Create( expr, decl, Null ).Semant()
				End If
			Catch error:String
				If error.StartsWith("Compile Error") Then
					Throw error
				Else
					Err "Operator " + op + " is not defined for type '" + expr.exprType.ToString() + "'"
				End If
			End Try
		End If

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
			expr=expr.Semant()
			If Not (TIntegralType(expr.exprType) Or (TEnumType(expr.exprType) And TEnumType(expr.exprType).decl.isFlags)) Or IsPointerType(expr.exprType) Then
				Err "Bitwise complement can only be used with integrals"
			End If
			If TByteType(expr.exprType) Or TShortType(expr.exprType) Then
				expr=expr.SemantAndCast( New TIntType )
				exprType=New TIntType
			Else
				exprType = expr.exprType
			End If
		Case "not"
			expr=expr.SemantAndCast( New TBoolType,CAST_EXPLICIT )
			exprType=New TBoolType
		Default
			InternalErr "TUnaryExpr.Semant"
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
		InternalErr "TUnaryExpr.Eval"
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

' * / + / & ~ | ^ shl shr sar
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

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		lhs=lhs.Semant()

		If TIdentExpr(rhs) Then
			TIdentExpr(rhs).isRhs = True
		End If

		rhs=rhs.Semant()
		
		' operator overload?
		If TObjectType(lhs.exprType) Then
			Local args:TExpr[] = [rhs]
			Try
				Local decl:TFuncDecl = TFuncDecl(TObjectType(lhs.exprType).classDecl.FindFuncDecl(op, args,,,,True,SCOPE_CLASS_HEIRARCHY))
				If decl Then
					Return New TInvokeMemberExpr.Create( lhs, decl, args ).Semant()
				End If
			Catch error:String
				If error.StartsWith("Compile Error") Then
					Throw error
				Else
					Err "Operator " + op + " is not defined between types '" + lhs.exprType.ToString() + "' and '" + rhs.exprType.ToString() + "'"
				End If
			End Try
		End If

		Local bitEnumOp:Int
		Select op
		Case "&","~~","|","shl","shr","sar"
			If TFloat128Type(lhs.exprType) Then
				exprType=New TInt128Type
			Else If TDouble128Type(lhs.exprType) Then
				exprType=New TInt128Type
			Else If TFloat64Type(lhs.exprType) Then
				exprType=New TInt128Type
			Else If TDoubleType(lhs.exprType) Then
				exprType=New TLongType
			Else If TFloatType(lhs.exprType) Then
				exprType=New TIntType
			Else If TUIntType(lhs.exprType) Then
				exprType=New TUIntType
			Else If TLongType(lhs.exprType) Then
				exprType=New TLongType
			Else If TULongType(lhs.exprType) Then
				exprType=New TULongType
			Else If TSizeTType(lhs.exprType) Then
				exprType=New TSizeTType
			Else If TWParamType(lhs.exprType) Then
				exprType=New TWParamType
			Else If TLParamType(lhs.exprType) Then
				exprType=New TLParamType
			Else If TEnumType(lhs.exprType) And TEnumType(lhs.exprType).decl.isFlags Then
				exprType = lhs.exprType.Copy()
				bitEnumOp = 2
			Else If TEnumType(rhs.exprType) And TEnumType(rhs.exprType).decl.isFlags Then
				exprType = rhs.exprType.Copy()
				bitEnumOp = 2
			Else
				exprType=New TIntType
			End If
		Case "^"
			If TIntegralType(lhs.exprType) And TIntegralType(rhs.exprType) Then
				exprType=New TLongType
			Else
				exprType=New TDoubleType
			End If
		Default
			exprType=BalanceTypes( lhs.exprType,rhs.exprType )
			If TStringType( exprType )
				If op<>"+"
					Err "Illegal string operator."
				EndIf
			Else If TVoidType( exprType ) Then
				Err "Illegal operation on a void expression."
			Else If Not TNumericType( exprType ) And Not IsPointerType( exprType, 0, TType.T_POINTER ) And Not TArrayType( exprType ) And Not TBoolType( exprType )
				Err "Operator " + op + " is not defined between types '" + lhs.exprType.ToString() + "' and '" + rhs.exprType.ToString() + "'"
			Else If IsPointerType( exprType, 0, TType.T_POINTER ) And op <> "+" And op <> "-" Then
				Err "Illegal expression type."
			Else If IsPointerType( lhs.exprType, 0, TType.T_POINTER ) And IsPointerType( rhs.exprType, 0, TType.T_POINTER ) And op <> "-" Then
				Err "Illegal expression type."
			EndIf
		End Select

		If (op = "+" Or op = "-") And IsPointerType(exprType, 0, TType.T_POINTER) And TNumericType(lhs.exprType) Then
			' with pointer addition we don't cast the numeric to a pointer
		Else
			lhs=lhs.Cast( exprType, bitEnumOp )
		End If
		
		If (op = "+" Or op = "-") And IsPointerType(exprType, 0, TType.T_POINTER) And TNumericType(rhs.exprType) Then
			' with pointer addition we don't cast the numeric to a pointer
		Else
			rhs=rhs.Cast( exprType, bitEnumOp )
		End If
		
		If IsPointerType( lhs.exprType, 0, TType.T_POINTER ) And IsPointerType( rhs.exprType, 0, TType.T_POINTER ) And op = "-" Then
			exprType = New TIntType
		End If

		If TConstExpr( lhs ) And TConstExpr( rhs ) Return EvalConst()
		
		If TConstExpr( rhs ) And (op = "/" Or op = "mod") And TIntegralType(rhs.exprType) And Not Long(rhs.Eval()) Then
			Err "Integer division by zero"
		End If

		Return Self
	End Method

	Method Eval$()
		Local lhs$=Self.lhs.Eval()
		Local rhs$=Self.rhs.Eval()
		If TIntType( exprType ) Or TByteType( exprType ) Or TShortType( exprType )
			Local x:Int=Int(lhs),y:Int=Int(rhs)
			Select op
			Case "^" Return Double(lhs)^Double(rhs)
			Case "*" Return x*y
			Case "/" 
				If Not y Then
					Err "Integer division by zero"
				End If
				Return x/y
			Case "mod"
				If Not y Then
					Err "Integer division by zero"
				End If
				Return x Mod y
			Case "shl" Return x Shl y
			Case "shr" Return x Shr y
			Case "sar" Return x Sar y
			Case "+" Return x + y
			Case "-" Return x - y
			Case "&" Return x & y
			Case "~~" Return x ~ y
			Case "|" Return x | y
			End Select
		Else If TLongType( exprType ) Or TSizeTType(exprType) Or TUIntType(exprType) Or TULongType(exprType) Or TInt128Type(exprType) Or TWParamType(exprType) Or TLParamType(exprType) 
			Local x:Long=Long(lhs),y:Long=Long(rhs)
			Select op
			Case "^" Return Double(lhs)^Double(rhs)
			Case "*" Return x*y
			Case "/"
				If Not y Then
					Err "Integer division by zero"
				End If
				Return x/y
			Case "mod"
				If Not y Then
					Err "Integer division by zero"
				End If
				Return x Mod y
			Case "shl" Return x Shl y
			Case "shr" Return x Shr y
			Case "sar" Return x Sar y
			Case "+" Return x + y
			Case "-" Return x - y
			Case "&" Return x & y
			Case "~~" Return x ~ y
			Case "|" Return x | y
			End Select
		Else If TFloatType( exprType )
			Local x:Double=Double(lhs),y:Double=Double(rhs)
			Select op
			Case "^" Return Double(x^y)
			Case "*" Return Float(x * y)
			Case "/" Return Float(x / y)
			Case "mod" Return Float(x Mod y)
			Case "+" Return Float(x + y)
			Case "-" Return Float(x - y)
			End Select
		Else If TDoubleType( exprType ) Or TFloat128Type(exprType) Or TDouble128Type(exprType) Or TFloat64Type(exprType)
			Local x:Double=Double(lhs),y:Double=Double(rhs)
			Select op
			Case "^" Return x^y
			Case "*" Return x * y
			Case "/" Return x / y
			Case "mod" Return x Mod y
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
		Else If TEnumType( exprType )
			Local x:Long=Long(lhs),y:Long=Long(rhs)
			Select op
			Case "shl" Return x Shl y
			Case "shr" Return x Shr y
			Case "sar" Return x Sar y
			Case "+" Return x + y
			Case "-" Return x - y
			Case "&" Return x & y
			Case "~~" Return x ~ y
			Case "|" Return x | y
			End Select
		EndIf
		InternalErr "TBinaryMathExpr.Eval"
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

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		lhs=lhs.Semant()
		rhs=rhs.Semant()

		' operator overload?
		If TObjectType(lhs.exprType) Then
		
			If TNullExpr(rhs) And TObjectType(lhs.exprType).classDecl.IsStruct() Then
				If op = "=" Then
					ty=New TBoolType
					exprType=New TBoolType
					lhs = New TConstExpr.Create(New TIntType, 1).Semant()
					rhs = New TConstExpr.Create(New TIntType, 0).Semant()
					Return Self
				Else
					op = "<>"
					ty = New TBoolType
					exprType=New TBoolType
					lhs = New TConstExpr.Create(New TIntType, 1).Semant()
					rhs = New TConstExpr.Create(New TIntType, 0).Semant()
					Return Self
				End If
			End If
		
			Local args:TExpr[] = [rhs]
			Try
				Local decl:TFuncDecl = TFuncDecl(TObjectType(lhs.exprType).classDecl.FindFuncDecl(op, args,,,,True,SCOPE_CLASS_HEIRARCHY))
				If decl Then
					Return New TInvokeMemberExpr.Create( lhs, decl, args ).Semant()
				End If
			Catch error:String
				' Structs must define an operator overload for the given op
				If TObjectType(lhs.exprType).classDecl.IsStruct() Then
					Err "No overloaded operator '" + op + "' found for " + TObjectType(lhs.exprType).classDecl.ToString()
				End If
				' otherwise, no overload, continue...
			End Try
		Else If (TArrayType(lhs.exprType) And TArrayType(lhs.exprType).isStatic) Or (TArrayType(rhs.exprType) And TArrayType(rhs.exprType).isStatic) Then
			Err "Static arrays cannot be compared"
		End If


		ty=BalanceTypes( lhs.exprType,rhs.exprType )

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
		Else If TLongType( ty ) Or TSizeTType( ty ) Or TUIntType( ty ) Or TULongType( ty ) Or TInt128Type(ty) Or TWParamType(ty) Or TLParamType(ty)
			Local lhs:Long=Long( Self.lhs.Eval() )
			Local rhs:Long=Long( Self.rhs.Eval() )
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
		Else If TDoubleType( ty ) Or TFloat128Type(ty) Or TDouble128Type(ty) Or TFloat64Type(ty)
			Local lhs:Double=Double( Self.lhs.Eval() )
			Local rhs:Double=Double( Self.rhs.Eval() )
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
		InternalErr "TBinaryCompareExpr.Eval"
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

	Method Semant:TExpr(options:Int = 0)
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
		InternalErr "TBinaryLogicExpr.Eval"
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
		If exprType Return Self
		
		Local ind:TExpr[]
		For Local i:Int = 0 Until index.length
			ind = ind + [CopyExpr(index[i])]
		Next
		Return New TIndexExpr.Create( CopyExpr(expr),ind )
	End Method

	Method _Semant:TExpr(set:Int, rhs:TExpr)
		If exprType Return Self

		expr=expr.Semant()

		' for functions and index access, use a new local variable
		If Not TVarExpr(expr) And Not TMemberVarExpr(expr) Then
			Local tmp:TLocalDecl=New TLocalDecl.Create( "", TType.MapVarPointerToPointerType(expr.exprType.Copy()), expr,, True )
			tmp.Semant()
			Local v:TVarExpr = New TVarExpr.Create( tmp )
			expr = New TStmtExpr.Create( New TDeclStmt.Create( tmp ), v ).Semant()
		End If

		For Local i:Int = 0 Until index.length
			If Not TObjectType(expr.exprType) And Not (TNumericType(expr.exprType) And IsPointerType( expr.exprType, 0 , TType.T_POINTER | TType.T_VARPTR)) Then
				index[i]=index[i].SemantAndCast( New TUIntType, True )
			Else
				index[i]=index[i].Semant()
			End If
		Next
		
		' operator overload?
		If TObjectType(expr.exprType) Then
			Local args:TExpr[]
			Local op:String
			If set Then
				args = index + [rhs]
				op = "[]="
			Else
				args = index
				op = "[]"
			End If
			Try
				Local decl:TFuncDecl = TFuncDecl(TObjectType(expr.exprType).classDecl.FindFuncDecl(op, args,,,,True,SCOPE_CLASS_HEIRARCHY))
				If decl Then
					Return New TInvokeMemberExpr.Create( expr, decl, args ).Semant()
				End If
			Catch error:String
				If error.StartsWith("Compile Error") Then
					Throw error
				Else
					Local istr:String
					Local vstr:String
					If index.length = 1 Then
						istr = " with '" + index[0].exprType.ToString() + "' index"
					Else
						For Local i:TExpr = EachIn index
							istr :+ ", '" + i.exprType.ToString() + "'"
						Next
						istr = " with " + istr[1..] + " indices"
					End If
					If set Then vstr = " and '" + rhs.exprType.ToString() + "' value"
					Err "Operator " + op + istr + vstr + " is not defined for type '" + expr.exprType.ToString() + "'"
				End If
			End Try
		End If
		
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
					Local sizeExpr:TExpr = New TArraySizeExpr.Create(expr, Null, index)
					index = [sizeExpr]
					Local tmp:TLocalDecl=New TLocalDecl.Create( "", NewPointerType(TType.T_UINT), sizeExpr,,True )
					TArraySizeExpr(sizeExpr).val = tmp
					Local stmt:TExpr = New TStmtExpr.Create( New TDeclStmt.Create( tmp ), Self ).Semant()
					stmt.exprType = exprType
					Return stmt
				Else
					Local sizeExpr:TExpr = New TArraySizeExpr.Create(expr, Null, index).Semant()
					index = [sizeExpr]
					Local tmp:TLocalDecl=New TLocalDecl.Create( "", NewPointerType(TType.T_UINT), sizeExpr,,True )
					TArraySizeExpr(sizeExpr).val = tmp
					Local stmt:TExpr = New TStmtExpr.Create( New TDeclStmt.Create( tmp ), Self ).Semant()
					stmt.exprType = exprType
					Return stmt
				End If
			End If
		Else If TNumericType(expr.exprType) And IsPointerType( expr.exprType, 0 , TType.T_POINTER | TType.T_VARPTR)' And Not TFunctionPtrType( expr.exprType )
			exprType=TType.MapPointerToPrim(TNumericType(expr.exprType))
		Else If TObjectType(expr.exprType) And TObjectType(expr.exprType).classDecl.IsStruct() And IsPointerType( expr.exprType, 0 , TType.T_POINTER | TType.T_VARPTR)' And Not TFunctionPtrType( expr.exprType )
			If IsPointerType( expr.exprType, 0 , TType.T_POINTER) Then
				exprType = TType.MapFromPointer(expr.exprType)
			Else
				exprType = expr.exprType
			End If
		Else
			Err "Expression of type '" + expr.exprType.ToString() + "' cannot be indexed"
		EndIf

		Return Self
	End Method
	
	Method Semant:TExpr(options:Int = 0)
		Return _Semant(False, Null)
	End Method

	Method SemantSet:TExpr( op$,rhs:TExpr, options:Int = 0 )
		Return _Semant(True, rhs)
	End Method
	
	Method SemantFunc:TExpr( args:TExpr[] , throwError:Int = True, funcCall:Int = False )
		Local ex:TExpr = Semant()
		
		If TArrayType( expr.exprType ) Then
			If TFunctionPtrType(exprType) Then
				exprType = TFunctionPtrType(exprType).func.retType
			Else
				If funcCall Then
					Err "Expression of type '" + exprType.ToString() + "' cannot be invoked."
				End If
			End If
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

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		expr=expr.Semant()
		If (TArrayType( expr.exprType ) And TArrayType( expr.exprType ).dims = 1) Or TStringType( expr.exprType )
			If from from=from.SemantAndCast( New TIntType )
			If term term=term.SemantAndCast( New TIntType )

			exprType=expr.exprType
			' remove var-ness
			If exprType._flags & TType.T_VAR Then
				exprType = exprType.Copy()
				exprType._flags :~ TType.T_VAR
			End If
		Else
			Err "Slices can only be used with strings or one dimensional arrays"
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
	
	Field toType:TType

	Method Create:TArrayExpr( exprs:TExpr[] )
		Self.exprs=exprs
		Return Self
	End Method

	Method Copy:TExpr()
		Local expr:TArrayExpr = New TArrayExpr.Create( CopyArgs(exprs) )
		expr.toType = toType
		Return expr
	End Method

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		If TIdentExpr(exprs[0]) Then
			TIdentExpr(exprs[0]).isRhs = True
		End If
		exprs[0]=exprs[0].Semant()
		Local ty:TType=exprs[0].exprType
		' convert from varptr to ptr if required
		ty = TType.MapVarPointerToPointerType(ty.Copy())
		
		If TInvokeExpr(exprs[0]) And Not TInvokeExpr(exprs[0]).invokedWithBraces Then
			ty = New TFunctionPtrType
			Local cp:TDecl = TInvokeExpr(exprs[0]).decl
			TInvokeExpr(exprs[0]).decl = TFuncDecl(TInvokeExpr(exprs[0]).decl.Copy())
			TInvokeExpr(exprs[0]).decl.actual = cp
			TInvokeExpr(exprs[0]).decl.attrs :| FUNC_PTR
			TFunctionPtrType(ty).func = TInvokeExpr(exprs[0]).decl

			For Local i:Int=1 Until exprs.Length
				If TIdentExpr(exprs[1]) Then
					TIdentExpr(exprs[1]).isRhs = True
				End If
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

		Local comp:Int = True
		Local last:TType
		Local base:TType
		For Local i:Int=0 Until exprs.Length

			Local expr:TExpr = exprs[i]

			' don't cast null types
			If TNullType(expr.exprType) <> Null Then
				Err "Auto array element has no type"
			End If

			Local ety:TType = expr.exprType
			If TBoolType(ety) Then
				ety = New TIntType
			End If
			
			If TObjectType(ety) And Not base Then
				base = ety
			End If
			
			If last <> Null And Not last.EqualsType(ety) Then
				If TObjectType(ety) Then
					If base.ExtendsType(ety) Then
						base = ety
					Else If Not ety.ExtendsType(base) Then
						Err "Auto array elements must be compatible types : Index " + i
					End If
				Else
					If (Not TConstExpr(expr) And Not IsNumericType(ety)) Or (TConstExpr(expr) And IsNumericType(ety) And Not TConstExpr(expr).CompatibleWithType(ty)) Then
						Err "Auto array elements must have identical types : Index " + i
					End If
				End If
			End If
			
			If toType And TConstExpr(expr) And Not TConstExpr(expr).CompatibleWithType(toType) Then
				comp = False
			End If
		
			last = ety
			
			exprs[i]=expr.Cast( ty )
		Next

		If comp And toType Then
			exprType=New TArrayType.Create( toType )
		Else
			exprType=New TArrayType.Create( ty )
		End If
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

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		expr=expr.Semant()
		
		For Local i:Int = 0 Until index.length
			index[i]=index[i].SemantAndCast( New TUIntType )
		Next
		
		exprType=NewPointerType(TType.T_UINT)
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
		If Not cdecl InternalErr "TIdentTypeExpr.Semant"
	End Method

	Method Semant:TExpr(options:Int = 0)
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

	Method Trans$()
		Return _trans.TransIdentTypeExpr( Self )
	End Method

	Method Cast:TExpr( ty:TType,castFlags:Int=0 )
		Err "Unable to convert from Type to " + ty.ToString()
	End Method

End Type

Type TIdentExpr Extends TExpr
	Field ident$
	Field expr:TExpr
	Field scope:TScopeDecl
	Field static:Int
	Field isArg:Int
	Field isRhs:Int
	Field fixedScope:Int
	
	Field _identLower:String
	Field unknownIdentsEvalFalse:Int

	Method IdentLower:String()
		If Not _identLower Then
			_identLower = ident.ToLower()
		End If
		Return _identLower
	End Method

	Method Create:TIdentExpr( ident$,expr:TExpr=Null, _identLower:String = Null, unknownIdentsEvalFalse:Int = False )
		Self.ident=ident
		Self.expr=expr
		Self._identLower = _identLower
		Self.unknownIdentsEvalFalse = unknownIdentsEvalFalse
		Return Self
	End Method

	Method Copy:TExpr()
		Local i:TIdentExpr = New TIdentExpr.Create( ident,CopyExpr(expr), _identLower )
		i.static = static
		i.isArg = isArg
		i.isRhs = isRhs
		i.fixedScope = fixedScope
		Return i
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
				static = expr.static
				scope=expr.exprType.GetClassScope()
				If Not scope Then
					Local e:String = "Member '" + ident + "' Not found in "
					If expr.exprType Then
						e :+ "type '" + expr.exprType.ToString() + "'"
					Else
						e :+ "'" + expr.ToString() + "'"
					End If
					Err e
				Else
					scope.Semant
				End If
			End If
			fixedScope = True
		Else
			scope=_env
			' determines if access is via static (like Function, or via a Type)
			' However, for Field->Field access this is not strictly true.
			If _env.FuncScope()=Null
				static = TModuleDecl(_env) = Null
			Else
				static=_env.FuncScope().IsStatic()
			End If
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

	Method IdentErr( errorDetails:String = Null )
		If errorDetails Then
			Err errorDetails
		Else
			Err "Identifier '"+ident+"' not found."
		End If
	End Method

	Method IdentNotFound()
	End Method

	Method IsVar()
		InternalErr "TIdentExpr.IsVar"
	End Method

	Method Semant:TExpr(options:Int = 0)
		Return SemantSet( "",Null, options )
	End Method

	Method SemantSet:TExpr( op$,rhs:TExpr, options:Int = 0 )
		_Semant

		Select options
		Case OPTION_WANT_LOOP_LABEL
			
			Local loopLabel:String = "#" + IdentLower()
	
			' maybe it's a loop label?
			Local stmt:TLoopStmt = TLoopStmt(scope.FindLoop(loopLabel))
			
			If stmt Then
				Return New TLoopLabelExpr.Create(stmt)
			End If
		
			Return Self
			
		Case OPTION_WANT_DATA_LABEL

			Local loopLabel:String = "#" + IdentLower()

			' maybe it's a data label?
			Local ddecl:TDefDataDecl = TDefDataDecl(_appInstance.FindDataLabel(loopLabel))
			
			If ddecl Then
				Return New TDataLabelExpr.Create(ddecl)
			End If
			
			Return Self
			
		Default

			'Local scope:TScopeDecl=IdentScope()
			Local vdecl:TValDecl=scope.FindValDecl( IdentLower(), static )
			
			If TLocalDecl( vdecl )
				' local variable should (at least) be in the same function scope.
				If vdecl.FuncScope() <> scope.FuncScope() Then
					' or the local can be in localmain..
					If TModuleDecl(scope) And vdecl.FuncScope() And vdecl.FuncScope().ident = "__LocalMain" Then
						' ok
					Else
						vdecl = Null
					End If
				End If
			End If
			
			If vdecl And fixedScope And static Then
				If TClassDecl(vdecl.scope) And TClassDecl(scope) Then
					If Not TClassDecl(scope).ExtendsClass(TClassDecl(vdecl.scope)) Then
						vdecl = Null
					End If
				Else
					If vdecl.scope <> scope Then
						vdecl = Null
					End If
				End If
			End If
			
			If vdecl
			
				If op And TLocalDecl( vdecl )
	
					Local ldecl:TLocalDecl = TLocalDecl( vdecl )
	
					If Not ldecl.volatile Then
						Local tryStmtDecl:TTryStmtDecl = scope.FindTry()
						If tryStmtDecl And (Not ldecl.declaredInTry Or tryStmtDecl <> ldecl.declaredInTry) Then
							ldecl.volatile = True
						End If
					End If
	
				Else If TConstDecl( vdecl )
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

			Local args:TExpr[]
			If rhs args=[rhs]

			Local decl:TDecl = TDecl(scope.FindDecl(IdentLower()))

			' maybe it's an enum?
			Local edecl:TEnumValueDecl = TEnumValueDecl(decl)
			If edecl Then
				Return New TIdentEnumExpr.Create(edecl)
			End If

			Local fdecl:TFuncDecl
			
			Try
				fdecl=scope.FindFuncDecl( IdentLower(),args, , isArg, True,True,SCOPE_ALL )
			Catch errorMessage:String
				If errorMessage.StartsWith("Compile Error") Then
					Throw errorMessage
				End If
			End Try
	
			If fdecl
				If Not isArg And Not fdecl.maybeFunctionPtr Err "Identifier '"+ident+"' cannot be used in this way."
	
				fdecl.maybeFunctionPtr = False
				
				If Not fdecl.IsStatic()
					If expr Return New TInvokeMemberExpr.Create( expr,fdecl,args, False ).Semant()
					If scope<>_env Or Not _env.FuncScope() Or _env.FuncScope().IsStatic() Err "Method '"+ident+"' cannot be accessed from here."
				EndIf
	
				Return New TInvokeExpr.Create( fdecl,args, False, isArg, isRhs ).Semant()
			End If
	
			' maybe it's a classdecl?
			Local cdecl:TClassDecl = TClassDecl(decl)
			
			If cdecl Then
				Local e:TIdentTypeExpr = New TIdentTypeExpr.Create(cdecl.objectType)
				e.cdecl = cdecl
				Return e
			End If
			
			If unknownIdentsEvalFalse Then
				Return New TConstExpr.Create( New TIntType, 0 ).Semant()
			End If
		
		End Select
		
		IdentErr
	End Method

	Method SemantFunc:TExpr( args:TExpr[], throwError:Int = True, funcCall:Int = False )

		_Semant

		Local errorDetails:String
		Local nearestScopeError:String

		'Local scope:TScopeDecl=IdentScope()
		Local initialScope:Int = SCOPE_ALL
		If scope Then
			If TClassDecl(scope) Then
				initialScope = SCOPE_CLASS_HEIRARCHY
			Else If TModuleDecl(scope) Then
				initialScope = SCOPE_MODULE
			End If
		End If
		
		Local fdecl:TFuncDecl
		Try
			fdecl=scope.FindFuncDecl( IdentLower(),args,,,,True,initialScope )
'			Local decl:Object=scope.FindFuncDecl( IdentLower(),args,,,,True,SCOPE_ALL )
'			If decl Then
'				If TFuncDecl(decl) Then
'					fdecl = TFuncDecl(decl)
'				Else If TFuncDeclList(decl) Then
'					If Not TFuncDeclList(decl).IsEmpty() Then
'						fdecl = TFuncDecl(TFuncDeclList(decl).First())
'					End If
'				End If
'			End If
		Catch errorMessage:String
			If errorMessage.StartsWith("Compile Error") Then
				Throw errorMessage
			Else
				' couldn't find an exact match, look elsewhere
				errorDetails = errorMessage
				If errorMessage.StartsWith("Unable") Then
					nearestScopeError = errorDetails
				End If
			End If
		End Try

		' if our scope is static, but the scope of the found function/method is not
		' then we should ignore it and continue looking higher up the scope stack.
		If static And fdecl And Not fdecl.IsStatic() Then
			Local scope2:TScopeDecl = fdecl.scope
			
			fdecl = Null
			
			' if fdecl was a method, this would be the Type's scope (ie. file/module)
			If scope2.scope Then
				fdecl = scope2.scope.FindFuncDecl( IdentLower(),args,,,,,SCOPE_CLASS_HEIRARCHY )
			End If
		Else If static And Not fdecl And Not fixedScope Then
			If _env.classScope() Then
				' try searching from our class scope
				'fdecl = _env.classScope().FindFuncDecl( IdentLower(),args )

				If Not fdecl Then				
					' try searching from our class parent scope
					Try
						fdecl = _env.classScope().scope.FindFuncDecl( IdentLower(),args,,,,True,SCOPE_ALL )
					Catch errorMessage:String
						If errorMessage.StartsWith("Compile Error") Then
							Throw errorMessage
						Else
							' couldn't find an exact match, look elsewhere
							errorDetails = errorMessage
							If Not nearestScopeError And errorDetails.StartsWith("Unable") Then
								nearestScopeError = errorDetails
							End If
						End If
					End Try
				End If
			Else If _env.ModuleScope() Then ' bah
				' finally, try searching from our module scope
				Try
					fdecl = _env.ModuleScope().FindFuncDecl( IdentLower(),args,,,,True,SCOPE_ALL )
				Catch errorMessage:String
					If errorMessage.StartsWith("Compile Error") Then
						Throw errorMessage
					Else
						' couldn't find an exact match, look elsewhere
						errorDetails = errorMessage
						If Not nearestScopeError And errorDetails.StartsWith("Unable") Then
							nearestScopeError = errorDetails
						End If
					End If
				End Try
			End If
		End If

		' couldn't find it? try a global search
		If Not fdecl And Not fixedScope Then
			For Local mdecl:TModuleDecl = EachIn _appInstance.globalImports.Values()
				Try
					fdecl=mdecl.FindFuncDecl( IdentLower(), args,,,,True,SCOPE_ALL )
				Catch errorMessage:String
					If errorMessage.StartsWith("Compile Error") Then
						Throw errorMessage
					Else
						' couldn't find an exact match, look elsewhere
						errorDetails = errorMessage
						If Not nearestScopeError And errorDetails.StartsWith("Unable") Then
							nearestScopeError = errorDetails
						End If
					End If
				End Try
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
				If fdecl.IsStatic() And fdecl.IsAbstract() Err "Cannot call abstract " + fdecl.ToString()
				Return New TInvokeExpr.Create( fdecl,args, funcCall ).Semant()
			End If
		EndIf

		'If args.Length=1 And args[0] And TObjectType( args[0].exprType )
		'	Local cdecl:TClassDecl=TClassDecl( scope.FindScopeDecl( ident ) )
		'	If cdecl Return args[0].Cast( New TObjectType.Create(cdecl),CAST_EXPLICIT )
		'EndIf

		If Not expr Then
			Local ty:TType=scope.FindType( IdentLower(),Null )
			If ty Then
				If args.Length=1 And args[0] Return args[0].Cast( ty,CAST_EXPLICIT )
				Err "Illegal number of arguments for type conversion"
			End If
		End If

		If throwError Then
			If nearestScopeError Then
				IdentErr(nearestScopeError)
			Else
				IdentErr(errorDetails)
			End If
		End If
	End Method

	Method SemantScope:TScopeDecl()
		If Not expr Return _env.FindScopeDecl( IdentLower() )
		Local scope:TScopeDecl=expr.SemantScope()

		' If scope is a namespace, then we are a module. Look up the module id and return it as the real scope.
		If TNamespaceDecl(scope) Then
			Local mdecl:TModuleDecl=TModuleDecl(scope.FindDecl(scope.IdentLower() + "." + IdentLower()))
			If mdecl Then
				Return mdecl
			End If
		End If

		If scope Return scope.FindScopeDecl( IdentLower() )
	End Method

'	Method Trans$()
'		Return _trans.TransIdentExpr( Self )
'	End Method

End Type

Type TBuiltinExpr Extends TExpr

	Field id:String
	Field expr:TExpr

	Method Semant:TExpr(options:Int = 0)
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

	Method Semant:TExpr(options:Int = 0)
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

Type TAscExpr Extends TBuiltinExpr

	Method Create:TAscExpr( expr:TExpr )
		Self.id="asc"
		Self.expr=expr
		Return Self
	End Method

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		If TConstExpr(expr) Then
			Local cexpr:TExpr = New TConstExpr.Create(New TIntType, Asc(TConstExpr(expr).value))
			_appInstance.removeStringConst(TConstExpr(expr).value)
			cexpr.Semant()
			Return cexpr
		End If
		
		expr = expr.SemantAndCast( New TStringType )
		exprType = New TIntType
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TAscExpr.Create( CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TAscExpr("+expr.ToString()+")"
	End Method

End Type

Type TSizeOfExpr Extends TBuiltinExpr

	Method Create:TSizeOfExpr( expr:TExpr )
		Self.id="sizeof"
		Self.expr=expr
		Return Self
	End Method

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self
		expr=expr.Semant()
		exprType=New TSizeTType
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TSizeOfExpr.Create( CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TSizeOfExpr("+expr.ToString()+")"
	End Method

End Type

Type TChrExpr Extends TBuiltinExpr

	Method Create:TChrExpr( expr:TExpr )
		Self.id="chr"
		Self.expr=expr
		Return Self
	End Method
	
	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		If TConstExpr(expr) Then
			Local cexpr:TConstExpr = New TConstExpr.Create(New TStringType, Chr(Int(TConstExpr(expr).value)))
			cexpr.Semant()
			_appInstance.mapStringConsts(cexpr.value)
			Return cexpr
		End If
		
		expr = expr.SemantAndCast( New TIntType )
		exprType = New TStringType
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TChrExpr.Create( CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TChrExpr("+expr.ToString()+")"
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

	Method Semant:TExpr(options:Int = 0)
		args=SemantArgs( args )
		If TIndexExpr(expr) Then
			expr = expr.SemantFunc( args, True, True )
			exprType = expr.exprType
			Return Self
		Else
			Return expr.SemantFunc( args, True, True )
		End If
	End Method

	Method SemantFunc:TExpr( args:TExpr[] , throwError:Int = True, funcCall:Int = False )
		' we are only likely to be called if a function returns and invokes a function pointer.

		Local ex:TExpr = Semant()
		
		If TFunctionPtrType(ex.exprType) Then
			exprType = TFunctionPtrType(ex.exprType).func.retType
		End If
		
		Self.args = SemantArgs(args)
		expr = ex
		
		Return Self
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

	Method Semant:TExpr(options:Int = 0)
		Err "Syntax error."
	End Method

	Method SemantScope:TScopeDecl()
		Return scope
	End Method
End Type

Type TNewExpr Extends TExpr
	Field isSuper:Int
	Field args:TExpr[]
	Field ctor:TFuncDecl

	Method Create:TNewExpr( args:TExpr[]=Null, isSuper:Int = False )
		If args Then
			Self.args=args
		Else
			Self.args = New TExpr[0]
		End If
		Self.isSuper = isSuper
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TNewExpr.Create(CopyArgs(args), isSuper)
	End Method

	Method Semant:TExpr(options:Int = 0)

		Local fdecl:TFuncDecl = _env.FuncScope()
		If Not fdecl Or TNewDecl(fdecl) = Null Or Not _env.ClassScope() Then
			Err "Call to constructor not valid in this context."
		End If
	
		' must be first statement of New() method
		Local stmt:TStmt = TStmt(fdecl.stmts.First())
		
		If TExprStmt(stmt) = Null Or TExprStmt(stmt).expr <> Self Then
			Err "Call to constructor must be first statement in New()."
		End If
	
		args=SemantArgs( args )
		
		' validate called constructor
		Try
			Local cDecl:TClassDecl = _env.ClassScope()
			If isSuper Then
				cDecl = cDecl.superClass
			End If
			ctor = cDecl.FindFuncDecl("new",args,,,,True,SCOPE_CLASS_HEIRARCHY )
		Catch errorMessage:String
			If errorMessage.StartsWith("Compile Error") Then
				Throw errorMessage
			Else
				Err errorMessage
			End If
		End Try
		
		' TODO : expand to full recursive test
		If ctor = fdecl Then
			Err "Recursive constructor invocation."
		End If
		
		ctor.Semant

		args=CastArgs(args, ctor)
		
		' attach to ctor
		TNewDecl(fdecl).chainedCtor = Self
		
		Return Self
	End Method

	Method Trans$()
		'Return _trans.TransFuncCallExpr( Self )
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

	Method Semant:TExpr(options:Int = 0)
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

	Method Semant:TExpr(options:Int = 0)
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

	Method Semant:TExpr(options:Int = 0)
		Return Self
	End Method

	Method Trans$()
		DebugStop
	End Method

	Method Eval$()
		Return ""
	End Method

End Type

Type TIdentEnumExpr Extends TExpr
	Field value:TEnumValueDecl

	Method Create:TIdentEnumExpr( value:TEnumValueDecl )
		Self.exprType=New TEnumType.Create(TEnumDecl(value.scope))
		Self.value = value
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TIdentEnumExpr.Create( value )
	End Method

	Method Semant:TExpr(options:Int = 0)
		Return Self
	End Method

	Method Trans$()
		Return value.Value()
	End Method

	Method Eval$()
		Return value.Value()
	End Method

End Type

Type TStackAllocExpr Extends TBuiltinExpr

	Method Create:TStackAllocExpr( expr:TExpr )
		Self.id="stackalloc"
		Self.expr=expr
		Return Self
	End Method

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self

		expr = expr.SemantAndCast( New TSizeTType )
		exprType = TType.MapToPointerType(New TByteType)
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TStackAllocExpr.Create( CopyExpr(expr) )
	End Method

	Method ToString$()
		Return "TStackAllocExpr("+expr.ToString()+")"
	End Method

End Type

Type TFieldOffsetExpr Extends TBuiltinExpr

	Field typeExpr:TExpr
	Field fieldExpr:TExpr

	Method Create:TFieldOffsetExpr( typeExpr:TExpr, fieldExpr:TExpr )
		Self.id="fieldoffset"
		Self.typeExpr=typeExpr
		Self.fieldExpr = fieldExpr
		Return Self
	End Method

	Method Semant:TExpr(options:Int = 0)
		If exprType Return Self
		
		' validate type and field
		typeExpr = typeExpr.Semant()
		
		If Not TIdentTypeExpr(typeExpr) Then
			Err "Expecting Type or Struct"
		End If
		
		TIdentExpr(fieldExpr).scope = TIdentTypeExpr(typeExpr).cdecl

		fieldExpr = fieldExpr.Semant()
		
		If Not TVarExpr(fieldExpr) Or Not TFieldDecl(TVarExpr(fieldExpr).decl) Then
			Err "Expecting Field"
		End If
		
		exprType = New TSizeTType
		Return Self
	End Method

	Method Copy:TExpr()
		Return New TFieldOffsetExpr.Create( typeExpr, fieldExpr )
	End Method

	Method ToString$()
		Return "TFieldOffsetExpr("+typeExpr.ToString()+"," + fieldExpr.ToString() + ")"
	End Method
End Type
