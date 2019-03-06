' Copyright (c) 2013-2019 Bruce A Henderson
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
SuperStrict

Import "parser.bmx"

Type TCTranslator Extends TTranslator

	'Field stringConstCount:Int

	Field prefix:String
	
	Field reserved_methods:String = ",New,Delete,ToString,Compare,SendMessage,_reserved1_,_reserved2_,_reserved3_,".ToLower()
	
	Method New()
		_trans = Self
	End Method

	Method TransSPointer$( ty:TType, withVar:Int = False )
		Local p:String
		
		If ty

			If withVar And (ty._flags & TType.T_VAR) Then
				p:+ "*"
			End If

			If ty._flags & TType.T_PTR Then
				p:+ "*"
			Else If ty._flags & TType.T_PTRPTR Then
				p:+ "**"
			Else If ty._flags & TType.T_PTRPTRPTR Then
				p:+ "***"
			End If

		End If
		
		Return p
	End Method

	Method TransArrayType$( ty:TType)
		Local p:String = TransSPointer(ty)
		
		If TBoolType( ty ) Return "~q" + p + "i~q"
		If TByteType( ty ) Return "~q" + p + "b~q"
		If TShortType( ty ) Return "~q" + p + "s~q"
		If TIntType( ty ) Return "~q" + p + "i~q"
		If TUIntType( ty ) Return "~q" + p + "u~q"
		If TFloatType( ty ) Return "~q" + p + "f~q"
		If TDoubleType( ty ) Return "~q" + p + "d~q"
		If TLongType( ty ) Return "~q" + p + "l~q"
		If TULongType( ty ) Return "~q" + p + "y~q"
		If TSizeTType( ty ) Return "~q" + p + "z~q"
		If TWParamType( ty ) Return "~q" + p + "w~q"
		If TLParamType( ty ) Return "~q" + p + "x~q"
		If TStringType( ty ) Return "~q$~q"
		If TInt128Type( ty ) Return "~q" + p + "j~q"
		If TFloat128Type( ty ) Return "~q" + p + "k~q"
		If TDouble128Type( ty ) Return "~q" + p + "m~q"
		If TFloat64Type( ty ) Return "~q" + p + "h~q"
		If TArrayType( ty ) Then
			Local s:String = "["
			For Local i:Int = 0 Until TArrayType( ty ).dims - 1
				s:+ ","
			Next
			s:+ "]"
			s:+ TransArrayType(TArrayType( ty ).elemType)
			Return Enquote(s.Replace("~q", ""))
		End If
		If TObjectType( ty ) Then
			If TObjectType( ty ).classdecl.IsStruct()
				Return "~q" + p + "@" + TObjectType(ty).classDecl.ident + "~q"
			Else
				If Not TObjectType( ty ).classdecl.IsExtern()
					Return "~q:" + TObjectType(ty).classDecl.ident + "~q"
				Else
					If TObjectType( ty ).classdecl.IsInterface() Then
						Return "~q" + p + "*#" + TObjectType(ty).classDecl.ident + "~q"
				'	ElseIf TObjectType( ty ).classdecl.IsStruct()
'						Return "~q" + p + "@" + TObjectType(ty).classDecl.ident + "~q"
					Else
						Return "~q" + p + "#" + TObjectType(ty).classDecl.ident + "~q"
					End If
				End If
			End If
		End If
		If TFunctionPtrType( ty ) Return "~q(~q"
		If TEnumType( ty ) Return "~q/" + TEnumType(ty).decl.ident + "~q"

	End Method

	Method TransDefDataType$( ty:TType)
		If TByteType( ty ) Return "~qb~q"
		If TShortType( ty ) Return "~qs~q"
		If TIntType( ty ) Return "~qi~q"
		If TUIntType( ty ) Return "~qu~q"
		If TFloatType( ty ) Return "~qf~q"
		If TDoubleType( ty ) Return "~qd~q"
		If TLongType( ty ) Return "~ql~q"
		If TULongType( ty ) Return "~qy~q"
		If TSizeTType( ty ) Return "~qz~q"
		If TStringType( ty ) Return "~q$~q"
		If TWParamType( ty ) Return "~qw~q"
		If TLParamType( ty ) Return "~qx~q"
	End Method

	Method TransDefDataConversion$(ty:TType)
		If TByteType( ty ) Return "bbConvertToInt"
		If TShortType( ty ) Return "bbConvertToInt"
		If TIntType( ty ) Return "bbConvertToInt"
		If TUIntType( ty ) Return "bbConvertToUInt"
		If TFloatType( ty ) Return "bbConvertToFloat"
		If TDoubleType( ty ) Return "bbConvertToDouble"
		If TLongType( ty ) Return "bbConvertToLong"
		If TULongType( ty ) Return "bbConvertToULong"
		If TSizeTType( ty ) Return "bbConvertToSizet"
		If TStringType( ty ) Return "bbConvertToString"
	End Method

	Method TransDefDataUnionType$(ty:TType)
		If TByteType( ty ) Return "b"
		If TShortType( ty ) Return "s"
		If TIntType( ty ) Return "i"
		If TUIntType( ty ) Return "u"
		If TFloatType( ty ) Return "f"
		If TDoubleType( ty ) Return "d"
		If TLongType( ty ) Return "l"
		If TULongType( ty ) Return "y"
		If TSizeTType( ty ) Return "z"
		If TWParamType( ty ) Return "w"
		If TLParamType( ty ) Return "x"
		If TStringType( ty ) Return "t"
	End Method
	
	Method TransDebugScopeType$(ty:TType)
		Local p:String = TransSPointer(ty)

		If TByteType( ty ) Return p + "b"
		If TShortType( ty ) Return p + "s"
		If TIntType( ty ) Return p + "i"
		If TUIntType( ty ) Return p + "u"
		If TFloatType( ty ) Return p + "f"
		If TDoubleType( ty ) Return p + "d"
		If TLongType( ty ) Return p + "l"
		If TULongType( ty ) Return p + "y"
		If TSizeTType( ty ) Return p + "t"
		If TWParamType( ty ) Return p + "W"
		If TLParamType( ty ) Return p + "X"
		If TInt128Type( ty ) Return p + "j"
		If TFloat128Type( ty ) Return p + "k"
		If TDouble128Type( ty ) Return p + "m"
		If TFloat64Type( ty ) Return p + "h"
		If TStringType( ty ) Return "$"
		If TArrayType( ty ) Then
			Local s:String = "["
			For Local i:Int = 0 Until TArrayType( ty ).dims - 1
				s:+ ","
			Next
			s:+ "]"
			Return s + TransDebugScopeType(TArrayType( ty ).elemType)
		End If
		If TObjectType( ty ) Then
			If TObjectType( ty ).classdecl.IsStruct() Then
					Return p + "@" + TObjectType(ty).classDecl.ident
			Else If Not TObjectType( ty ).classdecl.IsExtern()
				Return ":" + TObjectType( ty ).classDecl.ident
			Else
				If TObjectType( ty ).classdecl.IsInterface() Then
					Return p + "*#" + TObjectType(ty).classDecl.ident
				Else
					Return p + "#" + TObjectType(ty).classDecl.ident
				End If
			End If
		End If
		If TFunctionPtrType( ty ) Then
			Local func:TFuncDecl = TFunctionPtrType( ty ).func
			Local s:String = "("
			For Local i:Int = 0 Until func.argDecls.length
				If i Then
					s :+ ","
				End If
				s :+ TransDebugScopeType(func.argDecls[i].ty)
			Next
			Return s + ")" + TransDebugScopeType(func.retType)
		End If
		If TEnumType( ty ) Then
			Return "/" + TEnumType( ty ).decl.ident
		End If

	End Method

	Method TransType$( ty:TType, ident:String, fpReturnTypeFunctionArgs:String = Null, fpReturnTypeClassFunc:Int = False)
		Local p:String = TransSPointer(ty, True)
		
		If TVoidType( ty ) Or Not ty Then
			Return "void"
		End If
		If TBoolType( ty ) Return "BBINT" + p
		If TByteType( ty ) Return "BBBYTE" + p
		If TShortType( ty ) Return "BBSHORT" + p
		If TIntType( ty ) Return "BBINT" + p
		If TUIntType( ty ) Return "BBUINT" + p
		If TFloatType( ty ) Return "BBFLOAT" + p
		If TDoubleType( ty ) Return "BBDOUBLE" + p
		If TLongType( ty ) Return "BBLONG" + p
		If TULongType( ty ) Return "BBULONG" + p
		If TSizeTType( ty ) Return "BBSIZET" + p
		If TWParamType( ty ) Return "WPARAM" + p
		If TLParamType( ty ) Return "LPARAM" + p
		If TInt128Type( ty ) Return "BBINT128" + p
		If TFloat128Type( ty ) Return "BBFLOAT128" + p
		If TDouble128Type( ty ) Return "BBDOUBLE128" + p
		If TFloat64Type( ty ) Return "BBFLOAT64" + p
		If TStringType( ty ) Then
			If ty._flags & TType.T_CHAR_PTR Then
				Return "BBBYTE *"
			Else If ty._flags & TType.T_SHORT_PTR Then
				Return "BBSHORT *"
			End If
			Return "BBSTRING" + p
		End If
		If TArrayType( ty ) Return "BBARRAY" + p
		If TObjectType( ty ) Then
			Return TransObject(TObjectType(ty).classdecl) + p
		End If

		If TFunctionPtrType( ty ) Then

			TFunctionPtrType(ty).func.Semant

			Local api:String
			If TFunctionPtrType(ty).func.attrs & DECL_API_STDCALL Then
				api = " __stdcall "
			End If
			Local args:String
			For Local arg:TArgDecl = EachIn TFunctionPtrType(ty).func.argDecls
				arg.Semant()
				If args Then
					args :+ ","
				End If

				args :+ TransType(arg.ty, "")
			Next
			Local ret:String = ""
			If fpReturnTypeFunctionArgs Then
				ret = Bra(fpReturnTypeFunctionArgs)
			End If
			
			If fpReturnTypeClassFunc Then
				' typedef for function pointer return type
				Return ident + "x" + Bra(api + p +"* " + ident) + Bra(args)
			Else
				' if a function F returns another function (let's call it G),
				' then C syntax requires the declaration of F to be nested into that of the type of G
				' e.g. "Function F:RetG(ArgG)(ArgF)" in BlitzMax becomes "RetG(* F(ArgF) )(ArgG)" in C
				' solution: use "* F(ArgF)" as an ident to generate a declaration for G
				'           the result will be the declaration for F
				Local callable:String = Bra(api + p +"* " + ident + ret)
				If TFunctionPtrType(TFunctionPtrType(ty).func.retType) Then
					If Not args Then args = " " ' make sure the parentheses aren't ommited even if the parameter list is empty
					Return TransType(TFunctionPtrType(ty).func.retType, callable, args)
				Else
					Local retTypeStr:String = TransType(TFunctionPtrType(ty).func.retType, "")
					Return retTypeStr + callable + Bra(args)
				End If
			End If
		End If

		If TExternObjectType( ty ) Return "struct " + TExternObjectType( ty ).classDecl.munged + p
		If TEnumType( ty ) Return TransType( TEnumType( ty ).decl.ty, ident ) + p

		InternalErr "TCTranslator.TransType"
	End Method

	Method TransIfcType$( ty:TType, isSuperStrict:Int = False )
		Local p:String = TransSPointer(ty)
		If ty And (ty._flags & TType.T_VAR) Then
			p :+ " Var"
		End If
		
		If Not ty Then
			If opt_issuperstrict Or isSuperStrict Then
				Return p
			Else
				Return "%" + p
			End If
		End If
		If TVoidType( ty ) Then
			Return p
		End If
		If TByteType( ty ) Return "@" + p
		If TShortType( ty ) Return "@@" + p
		If TIntType( ty ) Return "%" + p
		If TUIntType( ty ) Return "|" + p
		If TFloatType( ty ) Return "#" + p
		If TDoubleType( ty ) Return "!" + p
		If TLongType( ty ) Return "%%" + p
		If TULongType( ty ) Return "||" + p
		If TSizeTType( ty ) Return "%z" + p
		If TWParamType( ty ) Return "%w" + p
		If TLParamType( ty ) Return "%x" + p
		If TInt128Type( ty ) Return "%j" + p
		If TFloat128Type( ty ) Return "!k" + p
		If TDouble128Type( ty ) Return "!m" + p
		If TFloat64Type( ty ) Return "!h" + p
		If TStringType( ty ) Then
			If ty._flags & TType.T_CHAR_PTR Then
				Return "$z"
			Else If ty._flags & TType.T_SHORT_PTR Then
				Return "$w"
			End If
			Return "$" + p
		End If
		If TArrayType( ty )  Then
			Local s:String = TransIfcType(TArrayType( ty ).elemType) + "&["
			For Local i:Int = 0 Until TArrayType( ty ).dims - 1
				s:+ ","
			Next
			Return s + "]" + p
		End If
		If TObjectType( ty ) Then
			Local t:String = ":"
			If TObjectType(ty).classDecl.IsExtern() Then
				If TObjectType(ty).classDecl.IsInterface() Then
					t = "??"
				ElseIf TObjectType(ty).classDecl.IsStruct() Then
					t = "~~"
				Else
					t = "?"
				End If
			End If
			Local cdecl:TClassDecl = TObjectType(ty).classDecl
			' find first type in hierarchy that isn't private
			While cdecl.IsPrivate() And cdecl.superClass <> Null
				cdecl = cdecl.superClass
			Wend
			Return t + cdecl.ident + p
		End If

		If TFunctionPtrType( ty ) Then

			Local t:String = TransIfcType(TFunctionPtrType(ty).func.retType, TFunctionPtrType(ty).func.ModuleScope().IsSuperStrict()) + TransIfcArgs(TFunctionPtrType(ty).func)
			If TFunctionPtrType( ty ).func.attrs & DECL_API_STDCALL Then
				t :+ "W"
			End If
	
			Return t
		End If
		If TExternObjectType( ty ) Return ":" + TExternObjectType(ty).classDecl.ident + p
		If TEnumType( ty ) Then
			Return "/" + TEnumType( ty ).decl.ident + p
		End If
		InternalErr "TCTranslator.TransIfcType"
	End Method

	Method TransRefType$( ty:TType, ident:String )
		Return TransType( ty, ident )
	End Method

	Method TransValue$( ty:TType,value$ )
		If value
			If IsPointerType(ty, 0, TType.T_POINTER) Return value
			If TBoolType( ty ) Return "1"
			If TShortType( ty ) Return value
			If TIntType( ty ) Return value
			If TUIntType( ty ) Return value+"U"
			If TLongType( ty ) Return value+"LL"
			If TULongType( ty ) Return value+"ULL"
			If TSizeTType( ty ) Return value
			If TWParamType( ty ) Return value
			If TLParamType( ty ) Return value
			If TInt128Type( ty ) Return value
			If TFloatType( ty ) Then
				If value = "nan" Or value.StartsWith("1.#IND0000") Then
					Return "bbPOSNANf"
				Else If value="-nan" Or value.StartsWith("-1.#IND0000") Then
					Return "bbNEGNANf"
				Else If value = "inf" Or value.StartsWith("1.#INF0000") Then
					Return "bbPOSINFf"
				Else If value = "-inf" Or value.StartsWith("-1.#INF0000") Then
					Return "bbNEGINFf"
				Else
					If value.ToLower().Find("e")>=0 Then
						Return value
					End If
					If value.Find(".") < 0 Then
						value :+ ".0"
					End If
					Return value+"f"
				End If
			End If
			If TDoubleType( ty ) Or TFloat128Type(ty) Or TDouble128Type(ty) Or TFloat64Type(ty) Then
				If value = "nan" Or value.StartsWith("1.#IND0000") Then
					Return "bbPOSNANd"
				Else If value="-nan" Or value.StartsWith("-1.#IND0000") Then
					Return "bbNEGNANd"
				Else If value = "inf" Or value.StartsWith("1.#INF0000") Then
					Return "bbPOSINFd"
				Else If value = "-inf" Or value.StartsWith("-1.#INF0000") Then
					Return "bbNEGINFd"
				Else
					If value.ToLower().Find("e") >=0 Then
						Return value
					End If
					If value.Find(".") < 0 Then
						value :+ ".0"
					End If
					Return value
				End If
			End If
			If TStringType( ty ) Return TransStringConst(value )
			If TByteType( ty ) Return value
			If TEnumType( ty ) Return value
		Else
			If TBoolType( ty ) Return "0"
			If TIntrinsicType( ty) Then
				If IsPointerType(ty, 0, TType.T_POINTER) Then
					Return "0"
				Else
					Return "{}"
				End If
			End If
			If TNumericType( ty ) Return "0" ' numeric and pointers
			If TStringType( ty ) Return Bra("&bbEmptyString")
			If TArrayType( ty ) Return Bra("&bbEmptyArray")
			If TObjectType( ty ) Then
				If TObjectType( ty ).classDecl.IsExtern() Or TObjectType( ty ).classDecl.IsStruct() Then
					If TObjectType( ty ).classDecl.IsInterface() Or IsPointerType(ty) Or (Not TObjectType( ty ).classDecl.IsStruct()) Then
						Return "0"
					Else
						Return "{}"
					End If
				Else
					Return Bra("&bbNullObject")
				End If
			End If
			If TFunctionPtrType( ty) Return "(&brl_blitz_NullFunctionError)" ' todo ??
			If TEnumType( ty ) Then
				If TEnumType( ty ).decl.isFlags Then
					Return "0"
				Else
					Return TEnumType( ty ).decl.values[0].Value()
				End If
			End If
		EndIf
		InternalErr "TCTranslator.TransValue"
	End Method
	
	Method TransArgs$( args:TExpr[],decl:TFuncDecl, objParam:String = Null )
'If decl.ident="AddS" DebugStop

		Local t$
		If objParam And (decl.IsMethod() Or decl.isCtor()) And ((Not decl.IsExtern()) Or (decl.IsExtern() And TClassDecl(decl.scope) And Not TClassDecl(decl.scope).IsStruct())) Then
			' object cast to match param type
			If TClassDecl(decl.scope) Then
				t :+ Bra(TransObject(TClassDecl(decl.scope).GetLatestFuncDecl(decl).scope, TClassDecl(decl.scope).IsStruct()))
			End If
			t:+ objParam
		End If
		For Local i:Int=0 Until decl.argDecls.Length
			Local ty:TType = TArgDecl(decl.argDecls[i].actual).ty
		
			If t t:+","
			If i < args.length
				Local arg:TExpr = args[i]
				
				' object cast to match param type
				If TObjectType(ty) And Not TObjectType(ty).classDecl.IsStruct() Then
					Local fdecl:TFuncDecl = decl
					If TClassDecl(decl.scope) Then
						fdecl = TClassDecl(decl.scope).GetOriginalFuncDecl(decl)
					End If
					t :+ Bra(TransObject(TObjectType(TArgDecl(fdecl.argDecls[i].actual).ty).classDecl))
				End If
				
				If TNullExpr(arg) Then
					t :+ TransValue(ty, Null)
					Continue
				Else If TIndexExpr(arg) And (ty._flags & TType.T_VAR) Then
						t:+ "&"
				Else If TStringType(ty) And (ty._flags & TType.T_VAR) Then
					If TCastExpr(arg) And TStringType(TCastExpr(arg).expr.exprType) Then
						t:+ "&"
					End If
				Else If TArrayType(ty) And (ty._flags & TType.T_VAR) Then
					If (TVarExpr(arg) And TArrayType(TVarExpr(arg).exprType) Or (TMemberVarExpr(arg) And TArrayType(TMemberVarExpr(arg).exprType))) And Not (arg.exprType._flags & TType.T_VAR) Then
						t:+ "&"
					End If
				Else If TObjectType(ty) And (ty._flags & TType.T_VAR) Then
					If (TVarExpr(arg) Or TMemberVarExpr(arg)) And TObjectType(arg.exprType) And Not (arg.exprType._flags & TType.T_VAR) Then
						t:+ "&"
					End If
				Else If TFunctionPtrType(ty) Or IsPointerType(ty, TType.T_BYTE) Then

					If TFunctionPtrType(ty) And (ty._flags & TType.T_VAR) Then
						t:+ "&"
					End If

					If TInvokeExpr(arg) And Not TInvokeExpr(arg).decl.IsMethod() Then
						If IsPointerType(ty, TType.T_BYTE) Then
							t:+ TInvokeExpr(arg).Trans()
						Else
							' need to test scopes to see if we need to use the current instance's function or not
							' use the "actual", not the copy we made for the function pointer.
							Local fdecl:TFuncDecl = TFuncDecl(TInvokeExpr(arg).decl.actual)
							If Not fdecl.munged Then
								MungDecl fdecl
								TInvokeExpr(arg).decl.munged = fdecl.munged
							End If

							If TClassDecl(fdecl.scope) Then
								' current scope is related to function scope?
								If _env.ClassScope() And _env.FuncScope() And _env.FuncScope().IsMethod() Then
									If _env.ClassScope().ExtendsClass(TClassDecl(fdecl.scope)) Then
										Local scope:TScopeDecl = _env.scope
										Local obj:String = Bra("struct " + scope.munged + "_obj*")
										Local class:String = "o->clas"
				
										t:+ class + "->f_" + fdecl.ident + MangleMethod(fdecl)
									Else
										t:+ fdecl.munged
									End If
								Else
									t:+ fdecl.munged
								End If
							Else
								t:+ fdecl.munged
							End If
						End If
						Continue
					End If
					' some cases where we are passing a function pointer via a void* parameter.
					If TCastExpr(arg) And TInvokeExpr(TCastExpr(arg).expr) And Not TInvokeExpr(TCastExpr(arg).expr).invokedWithBraces Then
						If Not TInvokeExpr(TCastExpr(arg).expr).decl.munged Then
							t:+ TInvokeExpr(TCastExpr(arg).expr).decl.actual.munged
						Else
							t:+ TInvokeExpr(TCastExpr(arg).expr).decl.munged
						End If
						Continue
					End If

					' Object -> Byte Ptr
					If IsPointerType(ty, TType.T_BYTE) And TObjectType(arg.exprType) Then
						t:+ Bra(Bra("(BBBYTE*)" + Bra(arg.Trans())) + "+" + Bra("sizeof(void*)"))
						Continue
					End If

				Else If IsNumericType(ty)  Then
					If TObjectType(arg.exprType) 'And TObjectType(args[i].exprType).classDecl = TClassDecl.nullObjectClass Then
					err "NULL"
						t:+ "0"
						Continue
					End If
				Else If TEnumType(ty) And (ty._flags & TType.T_VAR) Then
					If (TVarExpr(arg) Or TMemberVarExpr(arg)) And TEnumType(arg.exprType) And Not (arg.exprType._flags & TType.T_VAR) Then
						t:+ "&"
					End If
				End If
				
				If decl.argDecls[i].castTo Then
					t:+ Bra(decl.argDecls[i].castTo) + arg.Trans()
				Else

					Local tc:String = TransTemplateCast( ty,arg.exprType,arg.Trans() )
				
					' *sigh*
					' if var is going to var, remove any leading dereference character.
					' rather hacky. Would be better to cast variable to varptr during semanting (well done if you can work out where!)
					If arg.exprType.EqualsType( ty.ActualType() ) And (ty._flags & TType.T_VAR) And (arg.exprType._flags & TType.T_VAR) Then
						If tc.startswith("*") Then
							tc = tc[1..]
						End If
					End If

					t:+ tc
				
					't:+TransTemplateCast( ty,args[i].exprType,args[i].Trans() )
				End If
			Else
				decl.argDecls[i].Semant()
				' default values
				Local init:TExpr = decl.argDecls[i].init
				If init Then
					If TConstExpr(init) Then
						If TObjectType(TConstExpr(init).exprType) Then
t:+"NULLNULLNULL"
						' And TNullDecl(TObjectType(TConstExpr(init).exprType).classDecl)) Or (TConstExpr(init).value = "bbNullObject") Then
							If TStringType(decl.argDecls[i].ty) Then
								t :+ "&bbEmptyString"
							Else If TArrayType(decl.argDecls[i].ty) Then
								t :+ "&bbEmptyArray"
							Else
								t :+ "&bbNullObject"
							End If
						Else
							t:+ decl.argDecls[i].init.Trans()
						End If
					Else If TFunctionPtrType(ty) Then
						If TInvokeExpr(init) Then
							t:+ TInvokeExpr(init).decl.munged
						End If
					Else
						t:+ decl.argDecls[i].init.Trans()
					End If
				End If
			End If
		Next

		Return Bra(t)
	End Method

	Method TransArgsTypes$( args:TExpr[],declArgTypes:TType[])
		Local t$
		For Local i:Int=0 Until args.Length
			If t t:+","
			t:+TransTemplateCast( declArgTypes[i],args[i].exprType,args[i].Trans() )
		Next
		Return Bra(t)
	End Method

	Method TransPtrCast$( ty:TType,src:TType,expr$,cast$ )
		If IsPointerType(ty, 0, TType.T_POINTER | TType.T_VARPTR | TType.T_VAR) Or TFunctionPtrType(ty) Then
			' TODO : pointer stuff
			If TNullType(src) Return TransValue(ty, Null)
			Return expr
		End If
'If expr = "NULL" DebugStop
'		If TIntType(ty) And TStringType(src) Then
'DebugStop
'			Return "bbObjectDowncast" + Bra(expr + ",&" + TStringType(src).cDecl.munged)
'		End If

		If TNullType(src)
			Return TransValue(ty, Null)
		End If

		If TStringType(ty) And TObjectType(src) Then
			If Not TStringType(ty).cDecl Then
				ty.Semant()
			End If
			'If TNullDecl(TObjectType(src).classDecl) Then
			'	Return "&bbEmptyString"
			'End If
			Return Bra("(BBString *)bbObjectDowncast" + Bra("(BBOBJECT)" + expr + ",(BBClass*)&" + TStringType(ty).cDecl.munged))
		End If

		'If TArrayType(ty) And TObjectType(src) Then
		'	If TNullDecl(TObjectType(src).classDecl) Then
		'		Return "&bbEmptyArray"
		'	End If
		'End If

		If TVarPtrType(src) And TNumericType(ty) Then
			Return "*" + expr
		End If

		If TIntType(ty) And TStringType(src) Then
			Return Bra(expr + " != &bbEmptyString")
		End If

'		If TIntType(ty) And TObjectType(src) Then
'			Return Bra(expr + " != &bbNullObject")
'		End If
		If TObjectType(ty) And TStringType(src) Then
			Return expr
		End If

		If Not TObjectType(ty) Or Not TObjectType(src) Then
			InternalErr "TCTranslator.TransPtrCast"
		End If

		Local t$=TransType(ty, "TODO: TransPtrCast")

		If src.GetClass().IsInterface() Or ty.GetClass().IsInterface() cast="dynamic"

		If src.GetClass().IsInterface() And Not ty.GetClass().IsInterface() Then
			Return cast+"_cast<"+TransType(ty, "TODO: TransPtrCast")+">"+Bra( expr )
		End If

		'upcast?
		If src.GetClass().ExtendsClass( ty.GetClass() ) Return expr
		If TObjectType(ty) Then
			Return Bra(Bra(TransObject(TObjectType(ty).classDecl)) + "bbObjectDowncast" + Bra("(BBOBJECT)" + expr + ",(BBClass*)&" + TObjectType(ty).classDecl.munged))
		End If

		Return cast+"_cast<"+TransType(ty, "TODO: TransPtrCast")+">"+Bra( expr )

	End Method

	'***** Utility *****

	Method TransLocalDecl$( decl:TLocalDecl,init:TExpr, declare:Int = False, outputInit:Int = True )
		Local initTrans:String
		If outputInit Then
			Local cast:String
			If TObjectType(decl.ty) Then
				cast = Bra(TransObject(TObjectType(decl.ty).classDecl))
			End If
		
			If TInvokeExpr(init) And Not TInvokeExpr(init).invokedWithBraces Then
				initTrans = "=" + cast + TInvokeExpr(init).decl.munged
			Else
				initTrans = "=" + cast + init.Trans()
			End If
		End If
		
		Local volTrans:String = " "
		If decl.volatile Then
			volTrans = " volatile "
		End If
	
		If Not declare And opt_debug Then
			Local ty:TType = decl.ty
			If Not TObjectType( ty ) Or (TObjectType( ty ) And Not TObjectType( ty ).classDecl.IsStruct()) Then
				If TIntrinsicType(ty) Then
					If Not TConstExpr(init) Then
						Return decl.munged + initTrans
					End If
				Else
					Return decl.munged + initTrans
				End If
			Else If TObjectType( ty ) And TObjectType( ty ).classDecl.IsStruct() Then
				If Not TConstExpr(init) Then
					Return decl.munged + initTrans
				End If
			End If
		Else
			If TFunctionPtrType(decl.ty) Then
				If TInvokeExpr(init) And Not TInvokeExpr(init).invokedWithBraces Then
					Return TransType( decl.ty, decl.munged ) + " = " + TInvokeExpr(init).decl.munged
				Else
					Return TransType( decl.ty, decl.munged ) + initTrans
				End If
			Else
				Local ty:TType = decl.ty
				If TVoidType( ty ) Or Not ty Then
					ty = init.exprType
				End If
				If TObjectType(ty) Then
					If TObjectType(ty).classdecl.IsExtern() Then
						If TObjectType(ty).classdecl.IsInterface() Then
							Return TransType( ty, decl.munged )+" "+decl.munged + initTrans
						Else
							Return TransType( ty, decl.munged )+ volTrans +decl.munged + initTrans
						End If
					Else
						If TObjectType(ty).classdecl.IsStruct() Then
							Return TransType( ty, decl.munged )+" "+decl.munged + initTrans
						Else
							'If decl.volatile Then
								Return TransType( ty, decl.munged )+ volTrans +decl.munged + initTrans
							'Else
							'	Return TransType( ty, decl.munged )+" "+decl.munged + initTrans
							'End If
						End If
					End If
				Else
					Return TransType( ty, decl.munged )+ volTrans +decl.munged + initTrans
				End If
			End If
		End If
	End Method

	Method TransLocalDeclNoInit$( decl:TVarDecl )
		If TFunctionPtrType(decl.ty) Then
			Return TransType( decl.ty, decl.munged ) + "=" + TransValue(decl.ty, "")
		Else
			If TObjectType(decl.ty) Then
				If TObjectType(decl.ty).classdecl.IsExtern() Then
					If Not TObjectType(decl.ty).classdecl.IsStruct() Then
						Return TransType( decl.ty, decl.munged )+" "+decl.munged+"=" + TransValue(decl.ty, "")
					Else
						Return TransType( decl.ty, decl.munged )+" "+decl.munged
					End If
				Else
					If Not TObjectType(decl.ty).classdecl.IsStruct() Then
						Local cast:String = Bra(TransObject(TObjectType(decl.ty).classDecl))
					
						If TLocalDecl(decl) And TLocalDecl(decl).volatile Then
							Return TransType( decl.ty, decl.munged )+" volatile "+decl.munged + "=" + cast + TransValue(decl.ty, "")
						Else
							Return TransType( decl.ty, decl.munged )+" "+decl.munged + "=" + cast + TransValue(decl.ty, "")
						End If
					Else
						Return TransType( decl.ty, decl.munged )+" "+decl.munged + "=" + TransValue(decl.ty, "")
					End If
				End If
			Else
				If TLocalDecl(decl) And TLocalDecl(decl).volatile Then
					Return TransType( decl.ty, decl.munged )+" volatile "+decl.munged + "=" + TransValue(decl.ty, "")
				Else
					Return TransType( decl.ty, decl.munged )+" "+decl.munged + "=" + TransValue(decl.ty, "")
				End If
			End If
		End If
	End Method

	Method TransGlobalDecl$( gdecl:TGlobalDecl )
		Local glob:String

		If Not gdecl.funcGlobal Then
			If Not (gdecl.attrs & DECL_INITONLY) Then
				glob :+"static " + TransType( gdecl.init.exprType, gdecl.munged )+" "
			End If
	
			glob :+ gdecl.munged+"="
	
			If (TNewObjectExpr(gdecl.init) Or TNewArrayExpr(gdecl.init)) And Not (gdecl.attrs & DECL_INITONLY) Then
				glob :+ "0;~n"
				glob :+ indent + "if (" + gdecl.munged + "==0) {~n"
				glob :+ indent + "~t" + gdecl.munged + "=" + gdecl.init.Trans() + ";~n"
				glob :+ indent + "}"
			Else If TArrayExpr(gdecl.init) And Not (gdecl.attrs & DECL_INITONLY) Then
				glob :+ "0;~n"
				Emit glob
				Emit "if (" + gdecl.munged + "==0) {"
				
				glob = gdecl.munged + "=" + gdecl.init.Trans() + ";"
				Emit glob
				Emit "}"
				glob = ""
			Else
				If gdecl.init Then
					If TFunctionPtrType(gdecl.ty) Then
						If TInvokeExpr(gdecl.init) And Not TInvokeExpr(gdecl.init).invokedWithBraces Then
							glob :+ TInvokeExpr(gdecl.init).decl.munged
						Else
							glob :+ gdecl.init.Trans()
						End If
					Else If Not TConstExpr(gdecl.init) And Not (gdecl.attrs & DECL_INITONLY) Then
						' for non const, we need to add an initialiser
						glob :+ TransValue(gdecl.ty, "") + ";~n"
						glob :+ indent +"static int _" + gdecl.munged + "_inited = 0;~n"
						glob :+ indent + "if (!_" + gdecl.munged + "_inited) {~n"
						glob :+ indent + "~t_" + gdecl.munged + "_inited = 1;~n"
						glob :+ indent + "~t" + gdecl.munged + " = " + gdecl.init.Trans() + ";~n"
						glob :+ indent + "}"
					Else
						If TObjectType(gdecl.ty) Then
							glob :+ Bra(TransObject(TObjectType(gdecl.ty).classDecl))
						End If
						glob :+ gdecl.init.Trans()
					End If
				Else
					If TFunctionPtrType(gdecl.ty) Then
						glob :+ "&brl_blitz_NullFunctionError"
					Else
						glob :+ "0"
					End If
				End If
			End If
		Else
			glob :+ "static int _" + gdecl.munged + "_inited = 0;~n"
			glob :+ indent + "if (!_" + gdecl.munged + "_inited) {~n"
			glob :+ indent + "~t_" + gdecl.munged + "_inited = 1;~n"
			glob :+ indent + "~t" + gdecl.munged + " = " 
			If gdecl.init Then
				glob :+ gdecl.init.Trans()
			Else
				glob :+ TransValue(gdecl.ty, "")
			End If
			glob :+ ";~n"
			glob :+ indent + "}"
		End If

		Return glob
	End Method
	
	Method TransExportDef:String(decl:TFuncDecl, withApi:Int = True)
		Local t:String = decl.munged
		
		If withApi And decl.attrs & DECL_API_STDCALL Then
			t :+ "@"
			Local size:Int
			For Local arg:TArgDecl = EachIn decl.argDecls
				size :+ arg.ty.GetSize()
			Next
			t :+ size
		End If
		
		Return t
	End Method

	Method CreateLocal2$( ty:TType, t$ )
		Local tmp:TLocalDecl=New TLocalDecl.Create( "", ty,Null, True )
		MungDecl tmp
		If TShortType(ty) Then
			Emit TransType(ty, "") + " " + tmp.munged + " = bbStringToWString" + Bra(t)+ ";"
		Else
			Emit TransType(ty, "") + " " + tmp.munged + " = bbStringToCString" + Bra(t)+ ";"
		End If
		customVarStack.Push(tmp.munged)
		Return tmp.munged
	End Method

	Method EmitPushErr()
		Emit "pushErr();"
	End Method

	Method EmitSetErr( info$ )
		Emit "errInfo=~q"+info.Replace( "\","/" )+"~q;"
	End Method

	Method EmitPopErr()
		Emit "popErr();"
	End Method

	'***** Declarations *****

	Method TransStatic$( decl:TDecl )
		If decl.IsExtern() Then
			If Not decl.munged
				Return decl.ident
			End If
			Return decl.munged
		Else If _env And decl.scope And decl.scope=_env.ClassScope()
			' calling a class function from a method?
			If TFuncDecl(decl) And _env.ClassScope() And _env.FuncScope() And _env.FuncScope().IsMethod() And Not (decl.attrs & FUNC_PTR) And Not _env.ClassScope().IsStruct() Then
				Local scope:TScopeDecl = _env.ClassScope()
				Local obj:String = Bra("struct " + scope.munged + "_obj*")
				Local class:String = "o->clas"
				Return class + "->f_" + decl.ident + MangleMethod(TFuncDecl(decl))
			Else
				Return decl.munged
			End If
		Else If TClassDecl( decl.scope )
			'Return decl.scope.munged+"::"+decl.munged
			Return decl.munged
		Else If TModuleDecl( decl.scope )
			Return decl.munged
		Else If TFuncDecl(decl.scope)
			Return decl.munged
		Else If TGlobalDecl(decl)
			Return decl.munged
		Else If TBlockDecl(decl.scope)
			Return decl.munged
		Else If TEnumDecl(decl.scope)
			If decl.ident = "Values" Then
				Return "bbEnumValues"
			Else
				Return decl.munged
			End If
		EndIf
		InternalErr "TCTranslator.TransStatic"
	End Method

	Method TransTemplateCast$( ty:TType,src:TType,expr$ )

		' *sigh*
		' if var is going to var, remove any leading dereference character.
		' rather hacky. Would be better to cast variable to varptr during semanting (well done if you can work out where!)
		'If src.EqualsType( ty.ActualType() ) And (ty._flags & TType.T_VAR) And (src._flags & TType.T_VAR) Then
		'	If expr.startswith("*") Then
		'		expr = expr[1..]
		'	End If
		'End If

		If ty=src Return expr

		ty=ty.ActualType()
		'src=src.ActualType()

		If src.EqualsType( ty ) Return expr

		Return TransPtrCast( ty,src,expr,"static" )

	End Method

	Method TransGlobal$( decl:TGlobalDecl )
		Return TransStatic( decl )
	End Method

	Method TransField$( decl:TFieldDecl,lhs:TExpr )

		If lhs Then
			Return TransFieldRef(decl, TransSubExpr( lhs ), lhs.exprType)
		Else
			Return TransFieldRef(decl, "o", Null)
		End If
'		Local swiz$
'		If TObjectType( decl.ty )
'			If TObjectType( decl.ty ).classDecl.IsInterface() swiz=".p"
'		EndIf
'		If lhs Return TransSubExpr( lhs )+"->"+decl.munged+swiz
'		Return decl.munged+swiz
	End Method

	Method TransFunc$( decl:TFuncDecl,args:TExpr[],lhs:TExpr, sup:Int = False, scope:TScopeDecl = Null )

		' for calling the super class method instead
		Local tSuper:String
		If sup Then
			tSuper = "->super"
		End If

		If Not decl.munged
			MungDecl decl
		End If

		'If decl.IsMethod()
			If lhs And Not TSelfExpr(lhs) Then
				If TStringType(lhs.exprType) Then
					Return decl.munged + TransArgs(args, decl, TransSubExpr( lhs ))
				End If

				If TStmtExpr(lhs) Then
					lhs.Trans()
					lhs = TStmtExpr(lhs).expr
				End If

				If TVarExpr(lhs) Then
					Local cdecl:TClassDecl
					If TObjectType(TVarExpr(lhs).decl.ty) Then
						cdecl = TObjectType(TVarExpr(lhs).decl.ty).classDecl
					Else If TArrayType(TVarExpr(lhs).decl.ty) Then
						Return decl.munged+TransArgs( args,decl, TransSubExpr( lhs ) )
					End If

					If decl.attrs & FUNC_PTR Then
						'Return "(" + obj + TransSubExpr( lhs ) + ")->" + decl.munged+TransArgs( args,decl, Null)
					Local op:String
						If cdecl.IsStruct() Then op = "." Else op = "->"
						Return TransSubExpr( lhs ) + op + decl.munged+TransArgs( args,decl, Null)
					Else
						'Local lvar:String = CreateLocal(lhs, False)
						'Local lvarInit:String = Bra(lvar + " = " + lhs.Trans())
						
						If decl.scope.IsExtern()
							If Not cdecl.IsStruct()  Then
								'Return decl.munged + Bra(TransArgs( args,decl, TransSubExpr( lhs ) ))
								Return Bra(TransSubExpr( lhs )) + "->vtbl->" + decl.munged + Bra(TransArgs( args,decl, TransSubExpr( lhs ) ))
								'Return Bra(lvarInit) + "->vtbl->" + decl.munged + Bra(TransArgs( args,decl, lvar ))
							End If
							Err "TODO extern types not allowed methods"
						Else
							If cdecl And cdecl.IsInterface() And Not equalsBuiltInFunc(cdecl, decl) Then
								Local ifc:String = Bra("(struct " + cdecl.munged + "_methods*)" + Bra("bbObjectInterface(" + TransSubExpr( lhs ) + ", " + "&" + cdecl.munged + "_ifc)"))
								Return ifc + "->" + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, TransSubExpr( lhs ) )
'								Local ifc:String = Bra("(struct " + cdecl.munged + "_methods*)" + Bra("bbObjectInterface(" + lvarInit + ", " + "&" + cdecl.munged + "_ifc)"))
'								Return ifc + "->" + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, lvar )
							Else
								If cdecl And cdecl.IsStruct() Then
									If Not isPointerType(lhs.exprType) Then
										Return "_" + decl.munged+TransArgs( args,decl, "&" + TransSubExpr( lhs ) )
									Else
										Return "_" + decl.munged+TransArgs( args,decl, TransSubExpr( lhs ) )
									End If
								Else
									If cdecl Then
										Local obj:String = TransSubExpr( lhs )
										Local preObj:String = obj
										
										If opt_debug Then
											preObj = TransDebugNullObjectError(obj, cdecl)
										End If
										
										Local class:String = Bra(preObj) + "->clas" + tSuper
										Return class + "->" + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, obj )
									Else
										If TEnumDecl(decl.scope) Then
											' since we already have the ordinal, we can simply output that
											If decl.ident = "Ordinal" Then
												Return Bra(TransSubExpr( lhs ))
											Else
												Return decl.munged + Bra(TransSubExpr( lhs ))
											End If
										End If
									End If
'									Local class:String = Bra(lvarInit) + "->clas" + tSuper
'									Return class + "->" + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, lvar )
								End If
							End If
						End If
					End If
				Else If TNewObjectExpr(lhs) Then
					Local cdecl:TClassDecl = TNewObjectExpr(lhs).classDecl
					If cdecl.IsStruct() Then
						' create a local variable of the inner invocation
						Local lvar:String = CreateLocal(lhs)
						Return "_" + decl.munged+TransArgs( args,decl, "&" + lvar )
					Else
						If decl.IsMethod() Then
							Local class:String = cdecl.munged
							Return class + "." + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, TransSubExpr( lhs ) )
						Else
							Local class:String = Bra(Bra("struct " + cdecl.munged + "_obj*") + Bra(TransSubExpr( lhs ))) + "->clas" + tSuper
							Return class + "->" + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl )
						End If
					End If
				Else If TCastExpr(lhs) Then

					Local ty:TType = TCastExpr(lhs).ty

					If TObjectType(ty) Then
						' create a local variable of the inner invocation
						Local lvar:String = CreateLocal(lhs, False, False)
						Local lvarInit:String = Bra(lvar + " = " + lhs.Trans())

						Local cdecl:TClassDecl = TObjectType(ty).classDecl
						Local obj:String = Bra(TransObject(cdecl))
						If decl.attrs & FUNC_PTR Then
							Return "(" + obj + TransSubExpr( lhs ) + ")->" + decl.munged+TransArgs( args,decl, Null)
						Else
							' Null test
							If opt_debug Then
								lvarInit = TransDebugNullObjectError(lvarInit, cdecl)
							End If
	
							If cdecl.IsInterface() And Not equalsBuiltInFunc(cdecl, decl) Then
								Local ifc:String = Bra("(struct " + cdecl.munged + "_methods*)" + Bra("bbObjectInterface(" + obj + lvarInit + ", " + "&" + cdecl.munged + "_ifc)"))
								Return ifc + "->" + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, lvar )
							Else
								Local class:String = Bra("(" + obj + lvarInit + ")->clas" + tSuper)
								Return class + "->" + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, lvar )
							End If
						End If
					Else If TEnumType(ty) Then

						If TEnumDecl(decl.scope) Then
							' since we already have the ordinal, we can simply output that
							If decl.ident = "Ordinal" Then
								Return Bra(TransSubExpr( lhs ))
							Else
								Return decl.munged + Bra(TransSubExpr( lhs ))
							End If
						End If

					End If

				Else If TMemberVarExpr(lhs) Then
					If TObjectType(TMemberVarExpr(lhs).decl.ty) Then
						Local cdecl:TClassDecl = TObjectType(TMemberVarExpr(lhs).decl.ty).classDecl
						Local obj:String = Bra(TransObject(cdecl))
					
						If decl.scope.IsExtern()
							If TClassDecl(decl.scope) And Not TClassDecl(decl.scope).IsStruct() Then
								Local lvar:String = CreateLocal(lhs, False, False)
								Local lvarInit:String = Bra(lvar + " = " + lhs.Trans())
									
								Return Bra(lvarInit) + "->vtbl->" + decl.munged + Bra(TransArgs( args,decl, lvar ))
							Else
								Return decl.munged + Bra(TransArgs( args,decl, TransSubExpr( lhs ) ))
							End If
						Else
							If cdecl.IsStruct() Then

								' baaaaaaaaaaaaaaaaa
								If Not isPointerType(lhs.exprType) Then
									Return "_" + decl.munged+TransArgs( args,decl, "&" + TransSubExpr( lhs ) )
								Else
									Return "_" + decl.munged+TransArgs( args,decl, TransSubExpr( lhs ) )
								End If
							
							Else
								If decl.attrs & FUNC_PTR Then
									Return "(" + obj + TransSubExpr( lhs ) + ")->" + decl.munged+TransArgs( args,decl, Null)
								Else
									Local lvar:String = CreateLocal(lhs, False, False)
									Local lvarInit:String = Bra(lvar + " = " + lhs.Trans())
									
									' Null test
									If opt_debug Then
										lvarInit = TransDebugNullObjectError(lvarInit, cdecl)
									End If
		
									Local class:String = Bra("(" + obj + lvarInit + ")->clas" + tSuper)
									Return class + "->" + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, lvar )
								End If
							End If
						End If
						
					Else If TArrayType(TMemberVarExpr(lhs).decl.ty) Then
						Return decl.munged+TransArgs( args,decl, TransSubExpr( lhs ) )
					End If

				Else If TInvokeExpr(lhs) Then

					If TClassDecl(decl.scope) And TClassDecl(decl.scope).IsStruct() Then
						' create a local variable of the inner invocation
						Local lvar:String = CreateLocal(lhs, True)

						If Not isPointerType(lhs.exprType) Then
							Return "_" + decl.munged+TransArgs( args,decl, "&" + lvar )
						Else
							Return "_" + decl.munged+TransArgs( args,decl, lvar)
						End If
					Else
						' create a local variable of the inner invocation
						Local lvar:String = CreateLocal(lhs, False, False)
						Local lvarInit:String = Bra(lvar + " = " + lhs.Trans())

						' Null test
						If opt_debug Then
							Local cdecl:TClassDecl = TClassDecl(decl.scope)
							lvarInit = TransDebugNullObjectError(lvarInit, cdecl)
						End If
	
						Local obj:String = Bra(TransObject(decl.scope))
						Local class:String = Bra("(" + obj + lvarInit +")->clas" + tSuper)
						Return class + "->" + TransFuncPrefix(decl.scope, decl)+ FuncDeclMangleIdent(decl)+TransArgs( args,decl, lvar )

					End If
					'Local obj:String = Bra("struct " + decl.scope.munged + "_obj*")
					'Local class:String = Bra("(" + obj + TransSubExpr( lhs ) +")->clas" + tSuper)
					'Local class:String = Bra("&" + decl.scope.munged)
					'Return class + "->" + TransFuncPrefix(decl.scope, decl.ident) + decl.ident+TransArgs( args,decl, TransSubExpr( lhs ) )
				Else If TInvokeMemberExpr(lhs)
					' create a local variable of the inner invocation
					
					Local lvar:String
					Local lvarInit:String
					
					If Not decl.scope.IsExtern() And TClassDecl(decl.scope) And TClassDecl(decl.scope).IsStruct() Then
						lvar = CreateLocal(lhs, True)
					Else
						lvar = CreateLocal(lhs, False, False)
						lvarInit = Bra(lvar + " = " + lhs.Trans())
					End If

					If decl.scope.IsExtern()
						If TClassDecl(decl.scope) And Not TClassDecl(decl.scope).IsStruct() Then
							Return Bra(lvarInit) + "->vtbl->" + decl.munged + Bra(TransArgs( args,decl, lvar ))
						End If
						
						Return "// TODO"
					Else
						If TClassDecl(decl.scope) And TClassDecl(decl.scope).IsStruct() Then
							If Not isPointerType(lhs.exprType) Then
								Return "_" + decl.munged+TransArgs( args,decl, "&" + lvar )
							Else
								Return "_" + decl.munged+TransArgs( args,decl, lvar )
							End If
						Else
							Local cdecl:TClassDecl = TClassDecl(decl.scope)
							' Null test
							If opt_debug Then
								lvarInit = TransDebugNullObjectError(lvarInit, cdecl)
							End If
							If cdecl.IsInterface() And Not equalsBuiltInFunc(cdecl, decl) Then
								Local obj:String = Bra(TransObject(cdecl))
								Local ifc:String = Bra("(struct " + cdecl.munged + "_methods*)" + Bra("bbObjectInterface(" + obj + lvarInit + ", " + "&" + cdecl.munged + "_ifc)"))
								Return ifc + "->" + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, lvar )
							Else
								Local obj:String = lvarInit + "->clas" + tSuper
								Return obj + "->" + TransFuncPrefix(decl.scope, decl)+ FuncDeclMangleIdent(decl)+TransArgs( args,decl, lvar )
							End If
						End If
					End If

				Else If TIndexExpr(lhs) Then
					If TClassDecl(decl.scope) And TClassDecl(decl.scope).IsStruct() Then
					
						Local lvar:String = CreateLocal(lhs, True, False)
					
						If Not isPointerType(lhs.exprType) Then
							Return "_" + decl.munged+TransArgs( args,decl, "&" + lvar )
						Else
							Return "_" + decl.munged+TransArgs( args,decl, lvar )
						End If
					Else
						Local lvar:String = CreateLocal(lhs, False, False)
						Local lvarInit:String = Bra(lvar + " = " + lhs.Trans())
					
	'					Local loc:String = CreateLocal(lhs)
						Local obj:String = Bra(TransObject(decl.scope))
	
						Local cdecl:TClassDecl = TClassDecl(decl.scope)
	

						' Null test
						If opt_debug Then
							lvarInit = TransDebugNullObjectError(lvarInit, cdecl)
						End If
	
						If decl.attrs & FUNC_PTR Then
							Local op:String
							If cdecl.IsStruct() Then op = "." Else op = "->"
							Return lhs.Trans() + op + decl.munged+TransArgs( args,decl, Null)
						Else
							If decl.scope.IsExtern()
								'Local cdecl:TClassDecl = TClassDecl(decl.scope)
								
								If Not cdecl.IsStruct()  Then
									Return Bra(lvarInit) + "->vtbl->" + decl.munged + Bra(TransArgs( args,decl, lvar ))
								End If
								Err "TODO extern types not allowed methods"
							Else
								'Local cdecl:TClassDecl = TClassDecl(decl.scope)
		
								If cdecl And (cdecl.IsInterface() And Not equalsBuiltInFunc(cdecl, decl)) Then
									Local ifc:String = Bra("(struct " + cdecl.munged + "_methods*)" + Bra("bbObjectInterface(" + obj + lvarInit + ", " + "&" + cdecl.munged + "_ifc)"))
									Return ifc + "->" + TransFuncPrefix(cdecl, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, lvar )
								Else					
									Local class:String = Bra(lvarInit + "->clas" + tSuper)
									Return class + "->" + TransFuncPrefix(decl.scope, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, lvar )
								End If
							End If
						End If
					End If
				Else If TEnumType(lhs.exprType) Then

					If decl.ident = "Ordinal" Then
						Return Bra(TransSubExpr( lhs ))
					Else
						Return decl.munged + Bra(TransSubExpr( lhs ))
					End If

				Else
					InternalErr "TCTranslator.TransFunc"
				End If
				'Return TransSubExpr( lhs )+"->"+decl.munged+TransArgs( args,decl )
				'Return decl.munged+TransArgs( args,decl, TransSubExpr( lhs ) )
			End If

			' ((brl_standardio_TCStandardIO_obj*)o->clas)->md_Read(o, xxx, xxx)
		If decl.IsMethod() Or decl.IsField() Then
			If  Not (decl.attrs & FUNC_PTR) Then

				Local class:String
				
				If Not scope Then
					scope = decl.scope

					If TClassDecl(scope) And Not TClassDecl(scope).IsStruct() Then
						Local obj:String = Bra(TransObject(scope))
						class = "(" + obj + "o)->clas" + tSuper

						' Null test
						If opt_debug Then
							Emit TransDebugNullObjectError("o", TClassDecl(scope)) + ";"
						End If
					End If
				Else

					class = Bra("&" + scope.munged) + tSuper

				End If
				
				'Local obj:String = Bra("struct " + scope.munged + "_obj*")
				'Local class:String = Bra("(" + obj + "o)->clas" + tSuper)
				'Local class:String = Bra("&" + decl.scope.munged)
				If TClassDecl(scope).IsStruct() Then
					Return "_" + decl.munged+TransArgs( args,decl, "o" )
				Else
					Return class + "->" + TransFuncPrefix(scope, decl) + FuncDeclMangleIdent(decl)+TransArgs( args,decl, "o" )
				End If
			Else
				' Null test
				If opt_debug Then
					Emit TransDebugNullObjectError("o", TClassDecl(decl.scope)) + ";"
				End If
				
				Local obj:String
				If TClassDecl(scope) And Not TClassDecl(scope).IsStruct() Then
					obj = Bra(TransObject(decl.scope))
				End If
				Return Bra(obj + "o") + "->" + decl.munged+TransArgs( args,decl )
			End If
		End If
		
		' for want of a better place to put it...
		' It may be possible to have the generate via the TransStatic call below, but we'd need to inject a custom arg somewhere else then
		If TEnumDecl(decl.scope) And decl.ident = "Values" Then
			Return "bbEnumValues" + Bra(decl.scope.munged + "_BBEnum_impl")
		End If
		
		Return TransStatic( decl )+TransArgs( args,decl )
	End Method

	Method TransObject:String(decl:TScopeDecl, this:Int = False)
		If decl.ident = "Object"
			Return "BBOBJECT"
		Else If decl.ident = "String" Then
			Return "BBSTRING"
		Else
			If TClassDecl(decl) And TClassDecl(decl).IsStruct() Then
				Local t:String = "struct "
				If decl.IsExtern() Then
					t :+ decl.ident
				Else
					t :+ decl.munged
				End If
				
				If this Then
					Return t + "*"
				Else
					Return t
				End If

			Else
				If decl.IsExtern() Then
					Return "struct " + decl.ident + "*"
				Else
					Return "struct " + decl.munged + "_obj*"
				End If
			End If
		End If
	End Method

	Method TransFuncClass:String(decl:TClassDecl)
		If decl.ident = "Object"
			Return Bra("&bbObjectClass")
		Else
			Return Bra("&" + decl.munged)
		End If
	End Method

	Method TransFuncPrefix:String(decl:TScopeDecl, fdecl:TFuncDecl)
		Local ident:String = fdecl.ident

		If Not decl Or decl.ident = "Object" Or equalsBuiltInFunc(fdecl.ClassScope(), fdecl)
			Return ""
		Else
			If fdecl.IsMethod() Then
				Return "m_"
			Else
				Return "f_"
			End If
		End If
	End Method

	Method TransSuperFunc$( decl:TFuncDecl,args:TExpr[], scope:TScopeDecl )
		Return TransFunc(decl, args, Null, True, scope)
'		If decl.IsMethod()
'			Return decl.ClassScope().munged+".md_"+decl.ident+TransArgs( args,decl, "o" )
'		Else
'			Return decl.ClassScope().munged+".fn_"+decl.ident+TransArgs( args,decl)
'		End If
	End Method

	Method TransAscExpr:String(expr:TAscExpr)
		Return "bbStringAsc" + Bra(expr.expr.Trans())
	End Method

	Method TransChrExpr:String(expr:TChrExpr)
		Return "bbStringFromChar" + Bra(expr.expr.Trans())
	End Method

	Method TransLenExpr:String(expr:TLenExpr)
		'constant strings do not have "->length", so we use the
		'precalculated value
		If TConstExpr(expr.expr) Then
			If TStringType(expr.expr.exprType) Then
				Return TConstExpr(expr.expr).value.Length
			End If
		End If
		
		If TStringType(expr.expr.exprType) Then
			Return Bra(expr.expr.Trans()) + "->length"
		Else If TArrayType(expr.expr.exprType) Then
			Return Bra(expr.expr.Trans()) + "->scales[0]"
		Else If TCastExpr(expr.expr) Then
			If TArrayType(TCastExpr(expr.expr).expr.exprType) Then
				Return Bra(TCastExpr(expr.expr).expr.Trans()) + "->scales[0]"
			End If
		'other types just have a length of "1"
		Else
			Return "1"
		End If
	End Method

	Method TransSizeOfExpr:String(expr:TSizeOfExpr)
		Local cexpr:TConstExpr = TConstExpr(expr.expr)
		If cexpr Then
			If TNumericType(cexpr.exprType) Then
				Return "sizeof" + Bra(TransType(cexpr.exprType, ""))

			' strings
			Else If TStringType(cexpr.exprType) Then
				' length of const string * 2 bytes per char
				Return Len(cexpr.value) * 2
			End If
		Else
			If TNumericType(expr.expr.exprType) Then
				' remove Var-ness first, if any
				Local t:TType = expr.expr.exprType.Copy()
				If t._flags & TType.T_VAR Then
					t._flags :~ TType.T_VAR
				End If

				Return "sizeof" + Bra(TransType(t, ""))

			' strings
			Else If TStringType(expr.expr.exprType) Then
				'unicode chars each take 2 bytes
				Return Bra(expr.expr.Trans()) + "->length * 2"

			' arrays
			Else If TArrayType(expr.expr.exprType) Then
				'normal exprType is something like "int[]" that
				'is why it has to be checked against elemType
				Local elemType:TType = TArrayType( expr.expr.exprType ).elemType

				' numerics - including numeric pointers
				If TNumericType(elemType) Then
					'multiply element count * size of element type
					Return Bra(expr.expr.Trans()) + "->scales[0] * sizeof" + Bra(TransType(elemType, ""))

				' everything else : string, array, object, function pointer - are all pointers
				Else
					'arrays of objects are of size: elementCount * pointerSize
					Return  Bra(expr.expr.Trans()) + "->scales[0] * sizeof(void*)"
				EndIf
			
			' objects
			Else If TObjectType(expr.expr.exprType) Then
				If TObjectType( expr.expr.exprType ).classDecl.ident = "Object" Then
					Return "0"
				Else
					Local cdecl:TClassDecl = TObjectType( expr.expr.exprType ).classDecl
					
					If cdecl.IsStruct() Then
						If TIdentTypeExpr(expr.expr) Then
							If cdecl.IsExtern() Then
								Return "sizeof" + Bra("struct " + cdecl.ident)
							Else
								Return "sizeof" + Bra("struct " + cdecl.munged)
							End If
						Else
							Return "sizeof" + Bra(expr.expr.Trans())
						End If
					Else
					
						If TIdentTypeExpr(expr.expr) Then
							Return Bra(Bra(TransFuncClass(cdecl)) + "->obj_size")
						Else
							Return Bra(Bra(expr.expr.Trans()) + "->clas->obj_size")
						End If
						
					End If
				End If
			End If
		End If

		InternalErr "TCTranslator.TransSizeOfExpr"
	End Method

	'***** Expressions *****

	Method TransConstExpr$( expr:TConstExpr )
		If TStringType(expr.exprType) Then
			Return TransStringConst(expr.value)
		Else
			Return TransValue( expr.exprType,expr.value )
		End If
	End Method

	Field stringMap:TMap = New TMap

	Method TransStringConst:String(value:String)
		If value Then
			_appInstance.mapStringConsts(value)
		End If
		Local sc:TStringConst = TStringConst(_app.stringConsts.ValueForKey(value))
		Local s:String

		If Not sc Then
			s = "bbEmptyString"
		Else
			If Not sc.count Then
				sc.count :+ 1
			End If
			s = sc.id
		End If

		Return "&" + s
	End Method

	Method TransNewObjectExpr$( expr:TNewObjectExpr )

		Local t$

		If Not expr.classDecl.IsStruct() And (Not expr.ctor.argDecls Or expr.ctor.argDecls.length = 0) Then
			If expr.instanceExpr Then
				t = "bbObjectNew(" + Bra(expr.instanceExpr.Trans()) + "->clas)"
			Else
				If ClassHasObjectField(expr.classDecl) Then
					t = Bra(TransObject(TScopeDecl(expr.classDecl.actual))) + "bbObjectNew((BBClass *)&" + expr.classDecl.actual.munged + ")"
				Else
					t = Bra(TransObject(TScopeDecl(expr.classDecl.actual))) + "bbObjectAtomicNew((BBClass *)&" + expr.classDecl.actual.munged + ")"
				End If
			End If
		Else

			Local ctorMunged:String
			
			If expr.classDecl = expr.ctor.scope Then
				ctorMunged = expr.ctor.munged
			Else
				ctorMunged = expr.classDecl.actual.munged + "_" + expr.ctor.ident + MangleMethod(expr.ctor)
			End If

			If expr.instanceExpr Then
				If expr.classDEcl.IsStruct() Then
					t = ctorMunged + "_ObjectNew" + TransArgs( expr.args,expr.ctor)
				Else
					t = "_" + ctorMunged + "_ObjectNew" + TransArgs( expr.args,expr.ctor, Bra(expr.instanceExpr.Trans()) + "->clas" )
				End If
			Else
				If ClassHasObjectField(expr.classDecl) And Not expr.classDecl.IsStruct() Then
					t = "_" + ctorMunged + "_ObjectNew" + TransArgs( expr.args,expr.ctor, "&" + expr.classDecl.actual.munged )
				Else
					If expr.classDecl.IsStruct() Then
						t = ctorMunged + "_ObjectNew" + TransArgs( expr.args,expr.ctor)
					Else
						t = "_" + ctorMunged + "_ObjectNew" + TransArgs( expr.args,expr.ctor, "&" + expr.classDecl.actual.munged)
					End If
				End If
			End If
		End If
		'Local t$="(new "+expr.classDecl.actual.munged+")"
		'If expr.ctor t:+"->"+expr.ctor.actual.munged+TransArgs( expr.args,expr.ctor )
		Return t
	End Method

	Method TransNewArrayExpr$( expr:TNewArrayExpr )

		If expr.expr.length = 1 Then
			If TObjectType(expr.ty) And TObjectType(expr.ty).classdecl.IsStruct() And Not IsPointerType(expr.ty) Then
				Return "bbArrayNew1DStruct" + Bra(TransArrayType(expr.ty) + ", " + expr.expr[0].Trans() + ", sizeof" + Bra(TransObject(TObjectType(expr.ty).classdecl)))
			Else
				Return "bbArrayNew1D" + Bra(TransArrayType(expr.ty) + ", " + expr.expr[0].Trans())
			End If
		Else
			' multiple array
			Local s:String

			For Local i:Int = 0 Until expr.expr.length
				If i Then
					s:+ ", "
				End If

				s:+ expr.expr[i].Trans()
			Next

			If TObjectType(expr.ty) And TObjectType(expr.ty).classdecl.IsStruct() And Not IsPointerType(expr.ty) Then
				Return "bbArrayNewStruct" + Bra(TransArrayType(expr.ty) + ", sizeof" + Bra(TransObject(TObjectType(expr.ty).classdecl)) + ", " + expr.expr.length + ", " + s)
			Else
				Return "bbArrayNew" + Bra(TransArrayType(expr.ty) + ", " + expr.expr.length + ", " + s)
			End If
		End If

	End Method

	Method TransSelfExpr$( expr:TSelfExpr )
		If (TObjectType(expr.exprType) And TObjectType(expr.exprType).classDecl.IsStruct()) Or ..
				(TClassType(expr.exprType) And TClassType(expr.exprType).classDecl.IsStruct()) Then
			Return "*o"
		End If
		
		Return "o"
	End Method

	Method TransIdentTypeExpr:String(expr:TIdentTypeExpr)
		Return "struct " + expr.cdecl.munged + "_obj"
	End Method

	Method TransCastExpr$( expr:TCastExpr )

		Local t$= expr.expr.Trans()

		Local dst:TType=expr.exprType
		Local src:TType=expr.expr.exprType
		
		If TNumericType(src) And (src._flags & TType.T_VAR) Then
			' var number being cast to a varptr 
			If (dst._flags & TType.T_VARPTR) Then
				Return "&" + Bra(t)
			End If
		End If

		If (dst._flags & TType.T_VARPTR) Or (dst._flags & TType.T_VAR) Then
			If Not TConstExpr(expr.expr) Then
				If TInvokeExpr(expr.expr) Return t

				If TByteType( src) Return Bra("&"+t)
				If TShortType( src) Return Bra("&"+t)
				If TFloatType( src) Return Bra("&"+t)
				If TIntType( src) Return Bra("&"+t)
				If TUIntType( src) Return Bra("&"+t)
				If TLongType( src) Return Bra("&"+t)
				If TULongType( src) Return Bra("&"+t)
				If TSizeTType( src) Return Bra("&"+t)
				If TDoubleType( src) Return Bra("&"+t)
				If TInt128Type( src) Return Bra("&"+t)
				If TFloat128Type( src) Return Bra("&"+t)
				If TDouble128Type( src) Return Bra("&"+t)
				If TFloat64Type( src) Return Bra("&"+t)
				If TWParamType( src) Return Bra("&"+t)
				If TLParamType( src) Return Bra("&"+t)

				If TObjectType(src) Then
					If TObjectType(src).classDecl.IsExtern() Or (dst._flags & TType.T_VARPTR) Then
						Return Bra("&" + t)
					Else
						If TObjectType(dst) Then
							Return Bra("&" + t)
						Else
							Return Bra(Bra("(BBBYTE*)" + Bra("&" + t)) + "+" + Bra("sizeof(void*)"))
						End If
					End If
				End If
				
				If TFunctionPtrType(src) Return Bra("&"+t)
				'If TPointerType( src) Return Bra("&"+t)
			Else
				Return Bra(TransValue(TConstExpr(expr.expr).ty, TConstExpr(expr.expr).value))
			End If
		Else If IsPointerType( dst, 0, TType.T_POINTER | TType.T_CHAR_PTR | TType.T_SHORT_PTR )

			If TArrayType(src) Then
				Return Bra(Bra(TransType(dst, "")) + "BBARRAYDATA(" + t + ",1)")
			End If
			'If TByteType(src) And Not IsPointerType(src, TType.T_BYTE, TType.T_POINTER) Return Bra("&"+t)

			If TStringType(src) Then
				Local tmp:String

				If IsPointerType( dst, 0, TType.T_SHORT_PTR ) Or IsPointerType( dst, TType.T_SHORT, TType.T_PTR ) Then
					tmp = CreateLocal2(NewPointerType(TType.T_SHORT), t)
				Else
					tmp = CreateLocal2(NewPointerType(TType.T_BYTE), t)
				End If

				Return tmp
			End If
			
			If (TStringType(dst) And IsPointerType( dst, 0, TType.T_CHAR_PTR | TType.T_SHORT_PTR )) And TNullType(src) Then
				Return "0"
			End If

			If TObjectType(src) Then
				If TObjectType(src).classDecl.IsExtern() Or (src._flags & TType.T_VARPTR) Then
					Return Bra(t)
				Else
					Return Bra(Bra("(BBBYTE*)" + t) + "+" + Bra("sizeof(void*)"))
				End If
			End If

			Local p:String = TransSPointer(dst)
			If TByteType( dst )
				If IsPointerType(src, TType.T_BYTE, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBBYTE" + p + ")"+t)
			Else If TShortType( dst )
				If IsPointerType(src, TType.T_SHORT, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBSHORT" + p + ")"+t)
			Else If TIntType( dst )
				If IsPointerType(src, TType.T_INT, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBINT" + p + ")"+t)
			Else If TUIntType( dst )
				If IsPointerType(src, TType.T_UINT, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBUINT" + p + ")"+t)
			Else If TFloatType( dst )
				If IsPointerType(src, TType.T_FLOAT, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBFLOAT" + p + ")"+t)
			Else If TDoubleType( dst )
				If IsPointerType(src, TType.T_DOUBLE, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBDOUBLE" + p + ")"+t)
			Else If TLongType( dst )
				If IsPointerType(src, TType.T_LONG, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBLONG" + p + ")"+t)
			Else If TULongType( dst )
				If IsPointerType(src, TType.T_ULONG, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBULONG" + p + ")"+t)
			Else If TSizeTType( dst )
				If IsPointerType(src, TType.T_SIZET, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBSIZET" + p + ")"+t)
			Else If TWParamType( dst )
				If IsPointerType(src, TType.T_WPARAM, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(WPARAM" + p + ")"+t)
			Else If TLParamType( dst )
				If IsPointerType(src, TType.T_LPARAM, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(LPARAM" + p + ")"+t)
			Else If TInt128Type( dst )
				If IsPointerType(src, TType.T_INT128, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBINT128" + p + ")"+t)
			Else If TFloat128Type( dst )
				If IsPointerType(src, TType.T_FLOAT128, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBFLOAT128" + p + ")"+t)
			Else If TDouble128Type( dst )
				If IsPointerType(src, TType.T_DOUBLE128, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBDOUBLE128" + p + ")"+t)
			Else If TFloat64Type( dst )
				If IsPointerType(src, TType.T_FLOAT64, TType.T_POINTER & dst._flags) Return t
				If TNumericType( src ) Return Bra("(BBFLOAT64" + p + ")"+t)
				
			'Else If TIntPtrPtrType( dst )
			'	If TBytePtrType( src) Return Bra("(BBINT**)"+t)
			'	If TShortPtrType( src ) Return Bra("(BBINT**)"+t)
			'	If TIntPtrType( src ) Return Bra("(BBINT**)"+t)
			'	If TFloatPtrType( src ) Return Bra("(BBINT**)"+t)
			'	If TDoublePtrType( src ) Return Bra("(BBINT**)"+t)
			'	If TLongPtrType( src ) Return Bra("(BBINT**)"+t)
			'	If TNumericType( src ) Return Bra("(BBINT**)"+t)
			End If
		Else If TBoolType( dst )
			If TFunctionPtrType(src) Return Bra(Bra( t+"!=0" ) + " && " + Bra( t+"!=&brl_blitz_NullFunctionError" ))
			'If TFunctionPtrType(src) Return Bra( t+"!=0" )
			If IsPointerType( src, 0, TType.T_POINTER ) Return Bra( t )
			If TBoolType( src ) Return t
			If TByteType( src ) Return Bra( t+"!=0" )
			If TShortType( src ) Return Bra( t+"!=0" )
			If TIntType( src ) Return Bra( t+"!=0" )
			If TUIntType( src ) Return Bra( t+"!=0" )
			If TFloatType( src ) Return Bra( t+"!=0.0f" )
			'If TCastExpr(expr.expr) And (TArrayType( src ) Or TStringType( src ) Or TObjectType( src )) Then
			'	Return Bra( t+"!= &bbNullObject" )
			'End If
			If TLongType( src ) Return Bra( t+"!=0" )
			If TULongType( src ) Return Bra( t+"!=0" )
			If TSizeTType( src ) Return Bra( t+"!=0" )
			If TWParamType( src ) Return Bra( t+"!=0" )
			If TLParamType( src ) Return Bra( t+"!=0" )
			If TDoubleType( src ) Return Bra( t+"!=0.0f" )
			If TArrayType( src ) Return Bra( t+"!= &bbEmptyArray" )
			If TStringType( src ) Return Bra( t+"!= &bbEmptyString" )
			If TObjectType( src ) Then
				If TObjectType(src).classDecl.IsExtern() Then
					If Not TObjectType(src).classDecl.IsStruct() Then
						Return Bra( t+"!=0" )
					Else
						Return Bra("1")
					End If
				Else
					Return Bra( Bra(Bra("BBObject*") + t )+"!= &bbNullObject" )
				End If
			End If
		Else If TIntType( dst )
			If TBoolType( src ) Return Bra( t )
			If TByteType( src) Return Bra("(BBINT)"+t)
			If TShortType( src) Return Bra("(BBINT)"+t)
			If TBoolType( src ) Return t
			If TIntType( src ) Return t
			If TUIntType( src ) Return Bra("(BBINT)"+t)
			If TFloatType( src ) Return Bra("(BBINT)"+t)
			If TDoubleType( src ) Return Bra("(BBINT)"+t)
			If TLongType( src ) Return Bra("(BBINT)"+t)
			If TULongType( src ) Return Bra("(BBINT)"+t)
			If TSizeTType( src ) Return Bra("(BBINT)"+t)
			If TWParamType( src ) Return Bra("(BBINT)"+t)
			If TLParamType( src ) Return Bra("(BBINT)"+t)
			If TStringType( src ) Return "bbStringToInt" + Bra(t)
			If TEnumType( src) Return Bra("(BBINT)"+t)
			'If TIntVarPtrType( src ) Return Bra("*" + t)
			If IsPointerType(src,0,TType.T_POINTER) Return Bra("(BBINT)"+t)
			'If TPointerType( src ) Return Bra("(BBINT)"+t)
		 Else If TLongType( dst )
			If TBoolType( src ) Return Bra( t )
			If TByteType( src) Return Bra("(BBLONG)"+t)
			If TShortType( src) Return Bra("(BBLONG)"+t)
			If TIntType( src) Return Bra("(BBLONG)"+t)
			If TUIntType( src) Return Bra("(BBLONG)"+t)
			If TLongType( src ) Return t
			If TULongType( src ) Return Bra("(BBLONG)"+t)
			If TSizeTType( src ) Return Bra("(BBLONG)"+t)
			If TWParamType( src ) Return Bra("(BBLONG)"+t)
			If TLParamType( src ) Return Bra("(BBLONG)"+t)
			If TFloatType( src ) Return Bra("(BBLONG)"+t)
			If TDoubleType( src ) Return Bra("(BBLONG)"+t)
			If TStringType( src ) Return "bbStringToLong" + Bra(t)
			If IsPointerType(src,0,TType.T_POINTER) Return Bra("(BBLONG)"+t)
			If TFloat64Type( src ) Return Bra("(BBLONG)"+t)
			If TEnumType( src) Return Bra("(BBLONG)"+t)
			'If TPointerType( src ) Return Bra("(BBLONG)"+t)
		 Else If TSizeTType( dst )
			If TBoolType( src ) Return Bra( t )
			If TByteType( src) Return Bra("(BBSIZET)"+t)
			If TShortType( src) Return Bra("(BBSIZET)"+t)
			If TIntType( src) Return Bra("(BBSIZET)"+t)
			If TUIntType( src) Return Bra("(BBSIZET)"+t)
			If TLongType( src) Return Bra("(BBSIZET)"+t)
			If TULongType( src) Return Bra("(BBSIZET)"+t)
			If TSizeTType( src ) Return t
			If TWParamType( src ) Return Bra("(BBSIZET)"+t)
			If TLParamType( src ) Return Bra("(BBSIZET)"+t)
			If TFloatType( src ) Return Bra("(BBSIZET)"+t)
			If TDoubleType( src ) Return Bra("(BBSIZET)"+t)
			If TStringType( src ) Return "bbStringToSizet" + Bra(t)
			If IsPointerType(src,0,TType.T_POINTER) Return Bra("(BBSIZET)"+t)
			If TFloat64Type( src ) Return Bra("(BBSIZET)"+t)
			If TEnumType( src) Return Bra("(BBSIZET)"+t)
			'If TPointerType( src ) Return Bra("(BBLONG)"+t)
		Else If TFloatType( dst )
			If TBoolType( src ) Return Bra( t )
			If TByteType( src ) Return Bra("(BBFLOAT)"+t)
			If TIntType( src ) Return Bra("(BBFLOAT)"+t)
			If TUIntType( src ) Return Bra("(BBFLOAT)"+t)
			If TShortType( src ) Return Bra("(BBFLOAT)"+t)
			If TFloatType( src ) Return t
			If TDoubleType( src ) Return Bra("(BBFLOAT)"+t)
			If TLongType( src ) Return Bra("(BBFLOAT)"+t)
			If TULongType( src ) Return Bra("(BBFLOAT)"+t)
			If TSizeTType( src ) Return Bra("(BBFLOAT)"+t)
			If TWParamType( src ) Return Bra("(BBFLOAT)"+t)
			If TLParamType( src ) Return Bra("(BBFLOAT)"+t)
			If TStringType( src ) Return "bbStringToFloat" + Bra(t)
			'If TFloatVarPtrType( src ) Return Bra("*" + t)
			'If TPointerType( src ) Return Bra("(BBFLOAT)"+t)
		Else If TDoubleType( dst )
			If TBoolType( src ) Return Bra( t )
			If TByteType( src ) Return Bra("(BBDOUBLE)"+t)
			If TIntType( src ) Return Bra("(BBDOUBLE)"+t)
			If TUIntType( src ) Return Bra("(BBDOUBLE)"+t)
			If TShortType( src ) Return Bra("(BBDOUBLE)"+t)
			If TDoubleType( src ) Return t
			If TFloatType( src ) Return Bra("(BBDOUBLE)"+t)
			If TLongType( src ) Return Bra("(BBDOUBLE)"+t)
			If TULongType( src ) Return Bra("(BBDOUBLE)"+t)
			If TSizeTType( src ) Return Bra("(BBDOUBLE)"+t)
			If TWParamType( src ) Return Bra("(BBDOUBLE)"+t)
			If TLParamType( src ) Return Bra("(BBDOUBLE)"+t)
			If TStringType( src ) Return "bbStringToDouble" + Bra(t)
			'If TDoubleVarPtrType( src ) Return Bra("*" + t)
			'If TPointerType( src ) Return Bra("(BBDOUBLE)"+t)
		Else If TStringType( dst )
			If IsPointerType(src, 0, TType.T_POINTER) Return "bbStringFromSizet"+Bra( t )
			If TBoolType( src ) Return "bbStringFromInt"+Bra( t )
			If TByteType( src ) Return "bbStringFromInt"+Bra( t )
			If TShortType( src ) Return "bbStringFromInt"+Bra( t )
			If TIntType( src ) Return "bbStringFromInt"+Bra( t )
			If TUIntType( src ) Return "bbStringFromUInt"+Bra( t )
			If TLongType( src ) Return "bbStringFromLong"+Bra( t )
			If TULongType( src ) Return "bbStringFromULong"+Bra( t )
			If TSizeTType( src ) Return "bbStringFromSizet"+Bra( t )
			If TWParamType( src ) Return "bbStringFromWParam"+Bra( t )
			If TLParamType( src ) Return "bbStringFromLParam"+Bra( t )
			If TFloatType( src ) Return "bbStringFromFloat"+Bra( t )
			If TDoubleType( src ) Return "bbStringFromDouble"+Bra( t )
			If TStringType( src ) Then
				If src._flags & TType.T_CHAR_PTR Then
					Return "bbStringFromCString"+Bra( t )
				End If
				If src._flags & TType.T_SHORT_PTR Then
					Return "bbStringFromWString"+Bra( t )
				End If
				If src._flags & TType.T_VAR Then
					If TSliceExpr( expr.expr ) Then
						Return "&" + Bra(t)
					End If
					Return t
				End If
				Return t
			End If
			If TEnumType( src ) Then
				Local ty:TType = TEnumType( src ).decl.ty
				If TByteType( ty ) Return "bbStringFromInt"+Bra( t )
				If TShortType( ty ) Return "bbStringFromInt"+Bra( t )
				If TIntType( ty ) Return "bbStringFromInt"+Bra( t )
				If TUIntType( ty ) Return "bbStringFromUInt"+Bra( t )
				If TLongType( ty ) Return "bbStringFromLong"+Bra( t )
				If TULongType( ty ) Return "bbStringFromULong"+Bra( t )
				If TSizeTType( ty ) Return "bbStringFromSizet"+Bra( t )
			End If
			'If TStringVarPtrType( src ) Then
			'	If TSliceExpr( expr.expr ) Then
			'		Return t
			'	End If
			'	Return "*" + t
			'End If
			'If TStringCharPtrType( src ) Return "bbStringFromCString"+Bra( t )
		'Else If TStringVarPtrType( dst )
'DebugStop
		Else If TByteType( dst )
			If TBoolType( src ) Return Bra( t )
			If TByteType( src) Return t
			If TShortType( src ) Return Bra("(BBBYTE)"+t)
			If TIntType( src ) Return Bra("(BBBYTE)"+t)
			If TUIntType( src ) Return Bra("(BBBYTE)"+t)
			If TFloatType( src ) Return Bra("(BBBYTE)"+t)
			If TDoubleType( src ) Return Bra("(BBBYTE)"+t)
			If TLongType( src ) Return Bra("(BBBYTE)"+t)
			If TULongType( src ) Return Bra("(BBBYTE)"+t)
			If TSizeTType( src ) Return Bra("(BBBYTE)"+t)
			If TWParamType( src ) Return Bra("(BBBYTE)"+t)
			If TLParamType( src ) Return Bra("(BBBYTE)"+t)
			If TStringType( src ) Return "bbStringToInt" + Bra(t)
			If TEnumType( src) Return Bra("(BBYTE)"+t)
			'If TByteVarPtrType( src ) Return Bra("*" + t)
		Else If TShortType( dst )
			If TBoolType( src ) Return Bra( t )
			If TShortType( src) Return t
			If TByteType( src) Return Bra("(BBSHORT)"+t)
			If TIntType( src ) Return Bra("(BBSHORT)"+t)
			If TUIntType( src ) Return Bra("(BBSHORT)"+t)
			If TFloatType( src ) Return Bra("(BBSHORT)"+t)
			If TDoubleType( src ) Return Bra("(BBSHORT)"+t)
			If TLongType( src ) Return Bra("(BBSHORT)"+t)
			If TULongType( src ) Return Bra("(BBSHORT)"+t)
			If TSizeTType( src ) Return Bra("(BBSHORT)"+t)
			If TWParamType( src ) Return Bra("(BBSHORT)"+t)
			If TLParamType( src ) Return Bra("(BBSHORT)"+t)
			If TStringType( src ) Return "bbStringToInt" + Bra(t)
			If TEnumType( src) Return Bra("(BBSHORT)"+t)
			'If TShortVarPtrType( src ) Return Bra("*" + t)
		Else If TUIntType( dst )
			If TBoolType( src ) Return Bra( t )
			If TShortType( src ) Return Bra("(BBUINT)"+t)
			If TByteType( src) Return Bra("(BBUINT)"+t)
			If TIntType( src ) Return Bra("(BBUINT)"+t)
			If TUIntType( src) Return t
			If TFloatType( src ) Return Bra("(BBUINT)"+t)
			If TDoubleType( src ) Return Bra("(BBUINT)"+t)
			If TLongType( src ) Return Bra("(BBUINT)"+t)
			If TULongType( src ) Return Bra("(BBUINT)"+t)
			If TSizeTType( src ) Return Bra("(BBUINT)"+t)
			If TWParamType( src ) Return Bra("(BBUINT)"+t)
			If TLParamType( src ) Return Bra("(BBUINT)"+t)
			If TStringType( src ) Return "bbStringToUInt" + Bra(t)
			If TEnumType( src) Return Bra("(BBUINT)"+t)
		Else If TULongType( dst )
			If TBoolType( src ) Return Bra( t )
			If TShortType( src ) Return Bra("(BBULONG)"+t)
			If TByteType( src) Return Bra("(BBULONG)"+t)
			If TIntType( src ) Return Bra("(BBULONG)"+t)
			If TUIntType( src ) Return Bra("(BBULONG)"+t)
			If TFloatType( src ) Return Bra("(BBULONG)"+t)
			If TDoubleType( src ) Return Bra("(BBULONG)"+t)
			If TLongType( src ) Return Bra("(BBULONG)"+t)
			If TULongType( src) Return t
			If TSizeTType( src ) Return Bra("(BBULONG)"+t)
			If TWParamType( src ) Return Bra("(BBULONG)"+t)
			If TLParamType( src ) Return Bra("(BBULONG)"+t)
			If TStringType( src ) Return "bbStringToULong" + Bra(t)
			If TFloat64Type( src ) Return Bra("(BBULONG)"+t)
			If TEnumType( src) Return Bra("(BBULONG)"+t)
		Else If TFloat64Type( dst )
			If TFloat64Type( src) Return t
			If TLongType( src ) Return Bra("(BBFLOAT64)"+t)
			If TULongType( src ) Return Bra("(BBFLOAT64)"+t)
			If TSizeTType( src ) Return Bra("(BBFLOAT64)"+t)
		Else If TInt128Type( dst )
			If TInt128Type( src) Return t
			If TFloat128Type( src ) Return Bra("(BBINT128)"+t)
			If TDouble128Type( src ) Return Bra("(BBINT128)"+t)
		Else If TFloat128Type( dst )
			If TFloat128Type( src) Return t
			If TInt128Type( src ) Return Bra("(BBFLOAT128)"+t)
			If TDouble128Type( src ) Return Bra("(BBFLOAT128)"+t)
		Else If TDouble128Type( dst )
			If TDouble128Type( src) Return t
			If TInt128Type( src ) Return Bra("(BBDOUBLE128)"+t)
			If TFloat128Type( src ) Return Bra("(BBDOUBLE128)"+t)
		 Else If TWParamType( dst )
			If TBoolType( src ) Return Bra( t )
			If TByteType( src) Return Bra("(WPARAM)"+t)
			If TShortType( src) Return Bra("(WPARAM)"+t)
			If TIntType( src) Return Bra("(WPARAM)"+t)
			If TUIntType( src) Return Bra("(WPARAM)"+t)
			If TLongType( src) Return Bra("(WPARAM)"+t)
			If TULongType( src) Return Bra("(WPARAM)"+t)
			If TSizeTType( src ) Return Bra("(WPARAM)"+t)
			If TWParamType( src ) Return t
			If TLParamType( src ) Return Bra("(WPARAM)"+t)
			If TFloatType( src ) Return Bra("(WPARAM)"+t)
			If TDoubleType( src ) Return Bra("(WPARAM)"+t)
			If TStringType( src ) Return "bbStringToWParam" + Bra(t)
			If IsPointerType(src,0,TType.T_POINTER) Return Bra("(WPARAM)"+t)
		 Else If TLParamType( dst )
			If TBoolType( src ) Return Bra( t )
			If TByteType( src) Return Bra("(LPARAM)"+t)
			If TShortType( src) Return Bra("(LPARAM)"+t)
			If TIntType( src) Return Bra("(LPARAM)"+t)
			If TUIntType( src) Return Bra("(LPARAM)"+t)
			If TLongType( src) Return Bra("(LPARAM)"+t)
			If TULongType( src) Return Bra("(LPARAM)"+t)
			If TSizeTType( src ) Return Bra("(LPARAM)"+t)
			If TWParamType( src ) Return Bra("(LPARAM)"+t)
			If TLParamType( src ) Return t
			If TFloatType( src ) Return Bra("(LPARAM)"+t)
			If TDoubleType( src ) Return Bra("(LPARAM)"+t)
			If TStringType( src ) Return "bbStringToLParam" + Bra(t)
			If IsPointerType(src,0,TType.T_POINTER) Return Bra("(LPARAM)"+t)

		Else If TArrayType( dst )
			If TArrayType( src ) Then
				If TObjectType( TArrayType( dst ).elemType ) And TObjectType( TArrayType( dst ).elemType ).classDecl.ident = "Object" Then
					' if we are casting to Object[], don't actually cast.
					Return Bra(t)
				Else
					Return "bbArrayCastFromObject" + Bra(t + "," + TransArrayType(TArrayType( dst ).elemType))
				End If
			End If
			
			If TObjectType( src) And (TObjectType( src ).classDecl.ident = "___Array" Or TObjectType( src ).classDecl.ident = "Object") Then
				Return "bbArrayCastFromObject" + Bra(t + "," + TransArrayType(TArrayType( dst ).elemType))
			End If
		Else If TObjectType( dst )
			'If TArrayType( src ) Return Bra("(BBOBJECT)"+t)
			'If TStringType( src ) Return Bra("(BBOBJECT)"+t)
			'If TObjectType( src ) Return t
			If Not TObjectType( dst ).classDecl.IsExtern() Then
				If TNullType( src ) Return "&bbNullObject"
				If TObjectType(dst).classDecl.IsInterface() Then
					Return Bra(Bra(TransObject(TObjectType(dst).classDecl)) + "bbInterfaceDowncast" + Bra(t + ",&" + TObjectType(dst).classDecl.munged + "_ifc"))
				Else
					' no need to downcast to BBObject, as all objects extend it...
					If TObjectType( dst ).classDecl.ident = "Object" Then
						Return t
					Else
						Return Bra(Bra(TransObject(TObjectType(dst).classDecl)) + "bbObjectDowncast" + Bra("(BBOBJECT)" + t + ",(BBClass*)&" + TObjectType(dst).classDecl.munged))
					End If
				End If
			Else
				If TObjectType( dst ).classDecl.IsInterface() Then
					Return t
				Else
					Return "" ' TODO??
				End If
			End If
		Else If TEnumType( dst )
			If TEnumType( src) Return t			
		End If

		Return TransPtrCast( dst,src,t,"dynamic" )

		Err "C++ translator can't convert "+src.ToString()+" to "+dst.ToString()
	End Method

	Method TransUnaryExpr$( expr:TUnaryExpr )
		Local pri:Int=ExprPri( expr )
		Local t_expr$

		If TVarExpr(expr.expr) Then
			If TObjectType(TVarExpr(expr.expr).exprType) Then
				t_expr = Bra( expr.expr.Trans() + "!= &bbNullObject")
			Else If TStringType(TVarExpr(expr.expr).exprType)  Then
				t_expr = Bra( expr.expr.Trans() + "!= &bbEmptyString")
			Else
				t_expr = TransSubExpr( expr.expr,pri )
			End If
		Else
			t_expr = TransSubExpr( expr.expr,pri )
		End If

'		TransSubExpr( expr.expr,pri )
		Return TransUnaryOp( expr.op )+t_expr
	End Method

	Method TransBinaryExpr$( expr:TBinaryExpr )
		Local pri:Int=ExprPri( expr )
		
		Local t_lhs$=TransSubExpr( expr.lhs,pri )
'		If TVarPtrType(expr.lhs.exprType) Then
'			t_lhs = "*" + t_lhs
'		End If

		Local t_rhs$=TransSubExpr( expr.rhs,pri-1 )
'		If TVarPtrType(expr.rhs.exprType) Then
'			t_rhs = "*" + t_rhs
'		End If

		If expr.op = "+" Then
			If TStringType(expr.exprType) Then
				Return "bbStringConcat(" + t_lhs + "," + t_rhs + ")"
			Else If TArrayType(expr.exprType) Then
				Return "bbArrayConcat(" + TransArrayType(TArrayType(expr.lhs.exprType).elemType) + "," + t_lhs + "," + t_rhs + ")"
			End If
		End If
		
		If expr.op = "^" Then
			Return "bbFloatPow" + Bra(t_lhs + ", " + t_rhs)
		End If
		
		If expr.op = "mod" Or expr.op = "%" Then
			If TDecimalType(expr.lhs.exprType) Or TDecimalType(expr.rhs.exprType) Then
				Return "bbFloatMod" + Bra(t_lhs + ", " + t_rhs)
			End If
		End If
		
		If (expr.op = "shr" Or expr.op = "&" Or expr.op = "|") Then
			If TIntType(expr.exprType) Then
				t_lhs = "(unsigned int)(" + t_lhs + ")"
				t_rhs = "(unsigned int)(" + t_rhs + ")"
			Else If TLongType(expr.exprType) Then
				t_lhs = "(unsigned long long)(" + t_lhs + ")"
				t_rhs = "(unsigned long long)(" + t_rhs + ")"
			End If
		End If

		If TBinaryCompareExpr(expr) Then
			If TStringType(TBinaryCompareExpr(expr).ty) Then
				If t_lhs="&bbNullObject" Then
					err "NULL"
					t_lhs = "&bbEmptyString"
				End If
				If t_rhs="&bbNullObject" Then
					err "NULL"
					t_rhs = "&bbEmptyString"
				End If
				If t_lhs <> "&bbEmptyString" And t_rhs <> "&bbEmptyString" Then
					Return "bbStringCompare" + Bra(t_lhs + ", " + t_rhs) + TransBinaryOp(expr.op, "") + "0"
				End If
			End If
			If IsPointerType(TBinaryCompareExpr(expr).ty, 0, TType.T_POINTER) Then
				If t_lhs="&bbNullObject" Then
					t_lhs = "0"
				End If
				If t_rhs="&bbNullObject" Then
					t_rhs = "0"
				End If
			End If
			If TArrayType(TBinaryCompareExpr(expr).ty) Then
				If t_lhs="&bbNullObject" Then
					err "NULL"
					t_lhs = "&bbEmptyArray"
				End If
				If t_rhs="&bbNullObject" Then
					err "NULL"
					t_rhs = "&bbEmptyArray"
				End If
			End If
		End If

		Return bra(t_lhs+TransBinaryOp( expr.op,t_rhs )+t_rhs)
	End Method

	Method TransIndexExpr$( expr:TIndexExpr )

		Local t_expr$=TransSubExpr( expr.expr )

		Local t_index$
		If expr.index.length = 1 Then
			If TArraySizeExpr(expr.index[0]) Then
				Local sizes:TArraySizeExpr = TArraySizeExpr(expr.index[0])
				sizes.Trans()
				Local v:String = sizes.val.munged
				Local i:Int = 0
				For i = 0 Until sizes.index.length - 1
					If i Then
						t_index :+ " + "
					End If
					t_index :+ "(*(" + v
					If i Then
						t_index :+ "+" + i
					End If
					t_index :+ ")) * " + sizes.index[i].Trans()
				Next
				t_index :+ " + " + sizes.index[i].Trans()
				' (*(v+0)) * var1 + (*(v+1)) * var2 + var3
'DebugStop
			Else
				t_index=expr.index[0].Trans()
			End If
		End If

		If TStringType( expr.expr.exprType ) Then
			Return Bra(t_expr) + "->buf[" + t_index + "]"
			'Return "(BBINT)"+t_expr+"["+t_index+"]"
		End If

		If TArrayType( expr.expr.exprType ) Then
			If TFunctionPtrType(TArrayType( expr.expr.exprType ).elemType) Then
				If opt_debug Then
					Return Bra(Bra(TransType(TArrayType( expr.expr.exprType).elemType, "*")) + Bra("BBARRAYDATAINDEX(" + Bra(t_expr) + "," + Bra(t_expr) + "->dims," + t_index + ")")) + "[" + t_index + "]"
				Else
					Return Bra(Bra(TransType(TArrayType( expr.expr.exprType).elemType, "*")) + Bra("BBARRAYDATA(" + t_expr + ",1)")) + "[" + t_index + "]"
				End If
			Else
				If opt_debug Then
					Return Bra("(" + TransType(expr.exprType, "") + "*)BBARRAYDATAINDEX(" + Bra(t_expr) + "," + Bra(t_expr) + "->dims," + t_index + ")") + "[" + t_index + "]"
				Else
					Return Bra("(" + TransType(expr.exprType, "") + "*)BBARRAYDATA(" + t_expr + ",1)") + "[" + t_index + "]"
				End If
			End If
		End If

		'Local swiz$
		'If TObjectType( expr.exprType )And expr.exprType.GetClass().IsInterface() swiz=".p"

		'If ENV_CONFIG="debug" Return t_expr+".At("+t_index+")"+swiz

		Return t_expr+"["+t_index+"]"
	End Method

	Method TransSliceExpr$( expr:TSliceExpr )
'DebugStop
		Local t_expr:String=TransSubExpr( expr.expr )
		Local t_args$
		If expr.from Then
			t_args=expr.from.Trans()
		Else
			t_args = "0"
		End If
		If expr.term Then
			t_args:+","+expr.term.Trans()
		Else
			If TArrayType(expr.exprType) Then
				t_args :+ "," + Bra(t_expr) + "->scales[0]"
			'Else If TStringVarPtrType(expr.exprType) Then
			'	t_args :+ ",(*" + t_expr + ")->length"
			Else
				t_args :+ "," + Bra(t_expr) + "->length"
			End If
		End If

		If TArrayType(expr.exprType) Then
			Return "bbArraySlice" + Bra(TransArrayType(TArrayType(expr.exprType).elemType) + "," + t_expr + "," + t_args)
		'Else If TStringVarPtrType(expr.exprType) Then
		'	Return "bbStringSlice" + Bra("*" + t_expr + "," + t_args)
		Else
			Return "bbStringSlice" + Bra(t_expr + "," + t_args)
		End If
		'Return t_expr+".Slice("+t_args+")"
	End Method

	Method TransArrayExpr$( expr:TArrayExpr )
		Local elemType:TType=TArrayType( expr.exprType ).elemType

		Local tmpData:TLocalDecl =New TLocalDecl.Create( "",TType.voidType,Null )
		MungDecl tmpData

		Local tmpArray:TLocalDecl =New TLocalDecl.Create( "",TType.voidType,Null )
		MungDecl tmpArray

		Local t$
		Local count:Int
		For Local elem:TExpr=EachIn expr.exprs
			If t t:+","
			t:+elem.Trans()
			count :+ 1
		Next

		Local tt$
'		If Not _env tt="static "

		If Not TFunctionPtrType(elemType) Then
			tt :+ TransType( elemType, tmpData.munged ) + " "+tmpData.munged + "[]"
		Else
			tt :+ TransType( elemType, tmpData.munged + "[]" ) 
		End If
		Emit tt+"={"+t+"};"

		If TObjectType(elemType) And TObjectType(elemType).classdecl.IsStruct() And Not IsPointerType(elemType) Then
			Emit "BBARRAY " + tmpArray.munged + " = bbArrayFromDataStruct" + Bra(TransArrayType(elemType) + "," + count + "," + tmpData.munged + ", sizeof" + Bra(TransObject(TObjectType(elemType).classdecl))) + ";"
		Else
			Emit "BBARRAY " + tmpArray.munged + " = bbArrayFromData" + Bra(TransArrayType(elemType) + "," + count + "," + tmpData.munged ) + ";"
		End If

		Return tmpArray.munged
		'Return "bbArrayFromData" + Bra(TransArrayType(elemType) + "," + count + "," + tmp.munged )
		'Return "Array<"+TransRefType( elemType, "MM" )+" >("+tmp.munged+","+expr.exprs.Length+")"
	End Method

	Method TransArraySizeExpr$ ( expr:TArraySizeExpr )
		' scales[0] is the total size of the array
		' we start from [1] because it is the size of the next full dimension.
		' in the case of a 2-dimensional array, [1] represents the length of a row.
		Return Bra("(BBARRAY)" + expr.expr.Trans()) + "->scales + 1"
	End Method

	Method TransIntrinsicExpr$( decl:TDecl,expr:TExpr,args:TExpr[] )
		Local texpr$,arg0$,arg1$,arg2$

		If expr texpr=TransSubExpr( expr )

		If args.Length>0 And args[0] arg0=args[0].Trans()
		If args.Length>1 And args[1] arg1=args[1].Trans()
		If args.Length>2 And args[2] arg2=args[2].Trans()

		Local id$=decl.munged[1..]
		Local id2$=id[..1].ToUpper()+id[1..]

		Select id
		'
		'global functions
		Case "print" Return "Print"+Bra( arg0 )
		Case "error" Return "Error"+Bra( arg0 )
		'
		'string/array methods
		Case "length" Return texpr+".Length()"
		Case "resize" Return texpr+".Resize"+Bra( arg0 )

		'string methods
		Case "compare" Return texpr+".Compare"+Bra( arg0 )
		Case "find" Return texpr+".Find"+Bra( arg0+","+arg1 )
		Case "findlast" Return texpr+".FindLast"+Bra( arg0 )
		Case "findlast2" Return texpr+".FindLast"+Bra( arg0+","+arg1 )
		Case "trim" Return texpr+".Trim()"
		Case "join" Return texpr+".Join"+Bra( arg0 )
		Case "split" Return texpr+".Split"+Bra( arg0 )
		Case "replace" Return texpr+".Replace"+Bra( arg0+","+arg1 )
		Case "tolower" Return texpr+".ToLower()"
		Case "toupper" Return texpr+".ToUpper()"
		Case "contains" Return texpr+".Contains"+Bra( arg0 )
		Case "startswith" Return texpr+".StartsWith"+Bra( arg0 )
		Case "endswith" Return texpr+".EndsWith"+Bra( arg0 )

		'string functions
		Case "fromchar" Return "String"+Bra( "(Char)"+Bra(arg0)+",1" )

		'math methods
		Case "sin","cos","tan" Return "(float)"+id+Bra( Bra(arg0)+"*D2R" )
		Case "asin","acos","atan" Return "(float)"+Bra( id+Bra(arg0)+"*R2D" )
		Case "atan2" Return "(float)"+Bra( id+Bra(arg0+","+arg1)+"*R2D" )
		Case "sqrt","floor","ceil","log" Return "(float)"+id+Bra( arg0 )
		Case "pow" Return "(float)bbFloatPow"+Bra( arg0+","+arg1 )
		'
		End Select
		InternalErr "TCTranslator.TransIntrinsicExpr"
	End Method

	'***** Statements *****

	Method TransTryStmt$(tryStmt:TTryStmt)
		Emit "{"
		
		If tryStmt.finallyStmt Then MungDecl tryStmt.finallyStmt.finallyLabel
		MungDecl tryStmt.rethrowLabel
		MungDecl tryStmt.endTryLabel
		
		Emit "BBOBJECT ex;"
		If tryStmt.finallyStmt Then
			' for a nested Try construct, only declare this label once, because leaving such a construct
			' via Return, Exit Or Continue requires a jump to multiple Finally blocks in direct succession
			' and the "inner" declarations of retptr wouldn't be visible to the "outer" Finally blocks
			Local alreadyDeclared:Int = False
			For Local t:TTryStmt = EachIn tryStack
				If t.finallyStmt Then alreadyDeclared = True; Exit
			Next
			If Not alreadyDeclared Then
				Emit "void* retptr = &&" + tryStmt.rethrowLabel.munged + ";"
			Else
				Emit "retptr = &&" + tryStmt.rethrowLabel.munged + ";"
			End If
		End If
		Emit "jmp_buf* buf = bbExEnter();"
		If opt_platform = "macos" Or opt_platform = "ios" Or opt_platform = "osx" Then
			Emit "switch(_setjmp(*buf)) {"
		Else
		Emit "switch(setjmp(*buf)) {"
		End If
		
		' Try block:
		Emit "case 0: {"
		
		EmitLocalDeclarations tryStmt.block
		
		If opt_debug Then Emit "bbOnDebugPushExState();"
		PushLoopTryStack tryStmt
		tryStack.Push tryStmt
		EmitBlock tryStmt.block
		tryStack.Pop
		PopLoopTryStack
		Emit "bbExLeave();"
		If opt_debug Then Emit "bbOnDebugPopExState();"
		
		' run the Finally block if control reaches the end of the Try block
		If tryStmt.finallyStmt Then EmitFinallyJmp tryStmt.finallyStmt, tryStmt.endTryLabel
		Emit "}"
		Emit "break;"
		
		' Catch blocks:
		If tryStmt.catches Then
			Emit "case 1: {"
			If opt_debug Then Emit "bbOnDebugPopExState();"
			If tryStmt.finallyStmt Then
				If opt_debug Then Emit "bbOnDebugPushExState();"
				Emit "ex = bbExCatchAndReenter();"
			Else
				Emit "ex = bbExCatch();"
			End If
			Local s:String = ""
			For Local catchStmt:TCatchStmt = EachIn tryStmt.catches
				MungDecl catchStmt.init
				If TStringType(catchStmt.init.ty) Then
					Emit s + "if (bbObjectDowncast((BBOBJECT)ex,(BBClass*)&bbStringClass) != &bbEmptyString) {"
					Emit TransType(catchStmt.init.ty, catchStmt.init.munged) + " " + catchStmt.init.munged + "=(BBSTRING)ex;" 
				Else If TArrayType(catchStmt.init.ty) Then
					Emit s + "if (bbObjectDowncast((BBOBJECT)ex,(BBClass*)&bbArrayClass) != &bbEmptyArray) {"
					Emit TransType(catchStmt.init.ty, catchStmt.init.munged) + " " + catchStmt.init.munged + "=(BBARRAY)ex;" 
				Else If TObjectType(catchStmt.init.ty) Then
					If TObjectType(catchStmt.init.ty).classDecl.IsInterface() Then
						Emit s + "if (bbInterfaceDowncast(ex,&" + TObjectType(catchStmt.init.ty).classDecl.munged + "_ifc) != &bbNullObject) {"
					Else
						Emit s + "if (bbObjectDowncast((BBOBJECT)ex,(BBClass*)&" + TObjectType(catchStmt.init.ty).classDecl.munged + ") != &bbNullObject) {"
					End If
					Emit TransType(catchStmt.init.ty, catchStmt.init.munged) + " " + catchStmt.init.munged + "=" + Bra(TransType(catchStmt.init.ty, catchStmt.init.munged)) + "ex;" 
				Else
					Err "Not an object"
				End If
				
				EmitLocalDeclarations catchStmt.block, catchStmt.init
				
				If tryStmt.finallyStmt Then
					PushLoopTryStack tryStmt
					tryStack.Push tryStmt
					EmitBlock catchStmt.block
					tryStack.Pop
					PopLoopTryStack
				Else
					EmitBlock catchStmt.block
				End If
				
				s = "} else "
			Next
		
			If tryStmt.finallyStmt Then
				Emit s + "{"
				' run the Finally block if an exception was thrown from the Try block but not handled by any of the Catch blocks
				Emit "bbExLeave();"
				If opt_debug Then Emit "bbOnDebugPopExState();"
				EmitFinallyJmp tryStmt.finallyStmt, tryStmt.rethrowLabel
				Emit "}"
				
				' run the Finally block if an exception was thrown from the Try block and handled by one of the Catch blocks
				Emit "bbExLeave();"
				If opt_debug Then Emit "bbOnDebugPopExState();"
				EmitFinallyJmp tryStmt.finallyStmt, tryStmt.endTryLabel
			Else
				Emit s + "{"
				Emit "goto " + tryStmt.rethrowLabel.munged + ";"
				Emit "}"
				Emit "goto " + tryStmt.endTryLabel.munged + ";"
			End If
			
			Emit "}"
			Emit "break;"
		Else ' no catch blocks exist
			Emit "case 1:"
			'If opt_debug Then Emit "bbOnDebugPopExState();"
		End If
		
		If tryStmt.finallyStmt Then
			' run the Finally block if an exception was thrown from a Catch block
			Emit "case 2: {"
			If opt_debug Then Emit "bbOnDebugPopExState();"
			Emit "ex = bbExCatch();"
			Emit "retptr = &&" + tryStmt.rethrowLabel.munged + ";"
			Emit TransLabel(tryStmt.finallyStmt.finallyLabel)
			EmitFinallyStmt tryStmt.finallyStmt
			Emit "goto *retptr;"
			Emit TransLabel(tryStmt.rethrowLabel)
			Emit "bbExThrow(ex);"
			Emit "}"
			Emit "break;"
		Else
			Emit TransLabel(tryStmt.rethrowLabel)
			Emit "bbExThrow(ex);"
		End If
		
		Emit "}"
		Emit "}"
		Emit TransLabel(tryStmt.endTryLabel)
	End Method
	
	Method EmitFinallyJmp(stmt:TFinallyStmt, returnLabel:TLoopLabelDecl)
		Emit "retptr = &&" + returnLabel.munged + ";"
		Emit "goto " + stmt.finallyLabel.munged + ";"
	End Method
	
	Method EmitFinallyStmt(f:TFinallyStmt)
		Emit "{"
		EmitLocalDeclarations f.block
		EmitBlock f.block
		Emit "}"
	End Method

	Method EmitDebugEnterScope(block:TBlockDecl)
		Local count:Int
		For Local decl:TDecl = EachIn block.Decls()
			If TLocalDecl(decl) Then
				count :+ 1
			End If
			If TConstDecl(decl) Then
				count :+ 1
			End If
			If TGlobalDecl(decl) Then
				count :+ 1
			End If
		Next
		
		' a method also includes "Self" reference back to parent Type
		If TFuncDecl(block) And TFuncDecl(block).IsMethod() Then
			count :+ 1
		End If
		
		If _app.mainFunc = block Then
			For Local decl:TDecl = EachIn _app.mainModule.Decls()
				If TConstDecl(decl) Then
					count :+ 1
				End If
				If TGlobalDecl(decl) Then
					count :+ 1
				End If
			Next
		End If
		
		If Not count Then
			Emit "struct BBDebugScope __scope = {"
		Else
			Emit "struct BBDebugScope_" + count + " __scope = {"
			_app.scopeDefs.Insert(String(count), "")
		End If

		If TFuncDecl(block) Then
			Emit "BBDEBUGSCOPE_FUNCTION,"
			If _app.mainFunc = block Then
				' use the filename as the base function name
				Emit Enquote(StripExt(StripDir(_app.mainModule.filepath))) + ","
			Else
				Emit Enquote(TFuncDecl(block).ident) + ","
			End If
		Else
			Emit "BBDEBUGSCOPE_LOCALBLOCK,"
			Emit "0,"
		End If
			
			Emit "{"
			
			If TFuncDecl(block) And TFuncDecl(block).IsMethod() Then
				Emit "{"
				Emit "BBDEBUGDECL_LOCAL,"
				Emit "~qSelf~q,"
				Emit Enquote(TransDebugScopeType(TClassDecl(block.scope).objectType)) + ","
				Emit ".var_address=&o"
				Emit "},"
			End If
			
			' block consts and globals
			' consts
			For Local cdecl:TConstDecl = EachIn block.Decls()
				EmitConstDebugScope(cdecl)
			Next
			' globals
			For Local gdecl:TGlobalDecl = EachIn block.Decls()
				EmitGlobalDebugScope(gdecl)
			Next
			
			' iterate through decls and add as appropriate
			For Local decl:TDecl = EachIn block.Decls()
				Local ldecl:TLocalDecl = TLocalDecl(decl)
				If ldecl Then
					Emit "{"
					If ldecl.ty._flags & TType.T_VAR Then
						Emit "BBDEBUGDECL_VARPARAM,"
					Else
						Emit "BBDEBUGDECL_LOCAL,"
					End If
					Emit Enquote(ldecl.ident) + ","
					Emit Enquote(TransDebugScopeType(ldecl.ty)) + ","
					Emit ".var_address=&" + ldecl.munged
					Emit "},"
					
					Continue
				End If
			Next

			' add module consts and globals
			If _app.mainFunc = block Then
				' consts
				For Local cdecl:TConstDecl = EachIn _app.mainModule.Decls()
					EmitConstDebugScope(cdecl)
				Next
				' globals
				For Local gdecl:TGlobalDecl = EachIn _app.mainModule.Decls()
					EmitGlobalDebugScope(gdecl)
				Next
			End If
			
			Emit "BBDEBUGDECL_END "
			Emit "}"
			
			
		Emit "};"
		
		Emit "bbOnDebugEnterScope(&__scope);"
	End Method
	
	Method TransDebugNullObjectError:String(variable:String, cdecl:TClassDecl)
		If cdecl.IsStruct() Or cdecl.ident = "String" Or cdecl.ident = "___Array" Then
			'Return cdecl.munged + "NullObjectTest(" + variable + ")"
			Return variable
		Else
			Return Bra(Bra(TransObject(cdecl)) + "bbNullObjectTest(" + variable + ")")
		End If
	End Method
	
	Method TransAssignStmt$( stmt:TAssignStmt )
		If Not stmt.rhs Return stmt.lhs.Trans()

		Local rhs$=stmt.rhs.Trans()
		Local lhs$=stmt.lhs.TransVar()

		Local s:String
		Local cast:String
		
		If TObjectType(stmt.lhs.exprType) Then
			cast = Bra(TransObject(TObjectType(stmt.lhs.exprType).classDecl))
		End If

		If IsPointerType(stmt.lhs.exprType, TType.T_BYTE) And rhs = "&bbNullObject" Then
			rhs = "0"
		End If

		If stmt.op = ":%" Then
			If TDecimalType(stmt.lhs.exprType) Or TDecimalType(stmt.rhs.exprType) Then
				Return lhs + "=bbFloatMod" + Bra(lhs + "," + rhs)
			End If
		End If
		
		If TStringType(stmt.lhs.exprType) 'Or TStringVarPtrType(stmt.lhs.exprType) Then
'			s:+ "{"
'			s:+ "BBSTRING tmp=" + lhs + ";~n"

			If stmt.op = ":+" Then
				s :+ lhs+"=bbStringConcat("+lhs+","+rhs+")"
			Else If rhs = "&bbNullObject" Then
				s :+ lhs+TransAssignOp( stmt.op )+"&bbEmptyString"
			Else
				s :+ lhs+TransAssignOp( stmt.op )+rhs
			End If

'			s :+ ";~nBBRETAIN(" + lhs +")~n"
'			s :+ "BBRELEASE(tmp)~n"

'			s:+ "}"
		Else If TVarPtrType(stmt.lhs.exprType) Then

			If TCastExpr(stmt.rhs) And IsNumericType(TCastExpr(stmt.rhs).expr.exprType) Then
				rhs = TCastExpr(stmt.rhs).expr.Trans()
			End If

			s :+ lhs+TransAssignOp( stmt.op )+cast+rhs
		Else If TArrayType(stmt.lhs.exprType) Then
			If stmt.op = ":+" Then
				s :+ lhs+"=bbArrayConcat("+ TransArrayType(TArrayType(stmt.lhs.exprType).elemType) + "," + lhs+","+rhs+")"
			Else If rhs = "&bbNullObject" Then
				s :+ lhs+TransAssignOp( stmt.op )+"&bbEmptyArray"
			Else
				s :+ lhs+TransAssignOp( stmt.op )+cast+rhs
			End If
		Else If (TFunctionPtrType(stmt.lhs.exprType) <> Null Or IsPointerType(stmt.lhs.exprType, TType.T_BYTE)) And TInvokeExpr(stmt.rhs) And Not TInvokeExpr(stmt.rhs).invokedWithBraces Then
			rhs = TInvokeExpr(stmt.rhs).decl.munged
			s :+ lhs+TransAssignOp( stmt.op )+cast+rhs
		Else
			s :+ lhs+TransAssignOp( stmt.op )+cast+rhs
		End If

		If DEBUG Then
			DebugObject(stmt.lhs.exprType, lhs, Null, True)
		End If

		Return s
	End Method

	Method TransThrowStmt:String( stmt:TThrowStmt )
		Local s:String = "bbExThrow((BBObject *)"

		s:+ stmt.expr.Trans()

		s :+ ")"
		Return s
	End Method

	Method TransAssertStmt$( stmt:TAssertStmt )
		If opt_debug Then
			Emit "if (!" + Bra(stmt.expr.Trans()) + ") {"
			Emit "brl_blitz_RuntimeError(" + stmt.elseExpr.Trans() + ");"
			Emit "}"
		End If
	End Method

	Method TransEndStmt$( stmt:TEndStmt )
		Emit "bbEnd();"
	End Method

	Method TransReleaseStmt$( stmt:TReleaseStmt )
		Emit "bbHandleRelease" + Bra(stmt.expr.Trans()) + ";"
	End Method

	Method TransRestoreDataStmt$( stmt:TRestoreDataStmt )
		Emit "_defDataOffset = &_defData[" + TDataLabelExpr(stmt.expr).dataDef.label.index + "];"
	End Method

	Method TransReadDataStmt$( stmt:TReadDataStmt )
		For Local expr:TExpr = EachIn stmt.args
			' buffer overflow test
			If opt_debug Then
				Emit "if (_defDataOffset - _defData >= " + TDefDataDecl.count + ") brl_blitz_OutOfDataError();"
			End If
			Emit expr.Trans() + " = " + TransDefDataConversion(expr.exprType) + Bra("_defDataOffset++") + ";"
		Next
	End Method

	Method TransNativeStmt$( stmt:TNativeStmt)
		Emit stmt.raw
	End Method

	Method TransFullName:String(decl:TDecl)
		Local s:String
		
		If decl.scope Then
			s:+ TransFullName(decl.scope)
		End If
		
		If s Then
			s :+ " : "
		End If
		
		If TModuleDecl(decl) Then
			s:+ decl.ModuleScope().munged
		Else
			s :+ decl.ident
		End If
		
		If TFuncDecl(decl) Then
			s:+ "()"
		End If
		
		Return s
	End Method
	
	Method ClassHasObjectField:Int(classDecl:TClassDecl)
	
		If classDecl.superClass Then
			If ClassHasObjectField(classDecl.superClass) Then
				Return True
			End If
		End If

		For Local decl:TFieldDecl = EachIn classDecl.Decls()
			If Not decl.IsSemanted() Then
				decl.Semant()
			End If
			If TStringType(decl.ty) Or TArrayType(decl.ty) Or (TObjectType(decl.ty) And Not TObjectType(decl.ty).classDecl.IsStruct()) Then
				Return True
			End If
		Next
		
		Return False
	End Method


	'***** Declarations *****
Rem
	Method EmitFuncProto( decl:TFuncDecl )
		PushMungScope

		decl.Semant

		MungDecl decl

		'Find decl we override
		Local odecl:TFuncDecl=decl
		While odecl.overrides
			odecl=odecl.overrides
		Wend

		Local args$
		For Local arg:TArgDecl=EachIn odecl.argDecls
			If args args:+","
			args:+TransType( arg.ty )
		Next

		Local t$=TransType( odecl.retType )+" "+decl.munged+Bra( args )
		If decl.IsAbstract() t:+"=0"

		Local q$
		If decl.IsExtern() q:+"extern "
		If decl.IsMethod() q:+"virtual "
		If decl.IsStatic() And decl.ClassScope() q:+"static "

		Emit q+t+";"

		PopMungScope
	End Method
End Rem
	Method EmitBBClassFuncProto( decl:TFuncDecl)
		'PushMungScope
		BeginLocalScope
'DebugStop
'		decl.Semant

'		MungDecl decl

		'Find decl we override
		Local odecl:TFuncDecl=decl
		'If odecl.overrides And Not odecl.returnTypeSubclassed Then Return
'DebugLog decl.ident
'		While odecl.overrides
'			odecl=odecl.overrides
'		Wend

		Local id$=decl.munged
		Local pre:String

		If decl.IsMethod() Then
			id :+ "_m"
			pre = "m_"
		Else
			id :+ "_f"
			pre = "f_"
		End If

		Local bk:String = ";"
		'Local pre:String = "typedef "
		'If odecl.IsExtern() Then
		'	pre = "extern "
		'End If
'DebugLog "id = " + id
		Emit id + " " + pre + FuncDeclMangleIdent(odecl) + ";"

'		If Not proto Or (proto And Not odecl.IsExtern()) Then
Rem
			If Not TFunctionPtrType(odecl.retType) Then
				If Not odecl.castTo Then
					Emit pre + TransType( odecl.retType, "" )+" "+ Bra("*" + id)+Bra( args ) + bk
				Else
					If Not odecl.noCastGen Then
						Emit pre + odecl.castTo +" "+Bra("*" + id)+Bra( args ) + bk
					End If
				End If
			Else
				If Not odecl.castTo Then
					Emit pre + TransType( odecl.retType, id )+" "+Bra( args ) + bk
				Else
					If Not odecl.noCastGen Then
						Emit pre + odecl.castTo +" "+Bra( args ) + bk
					End If
				End If
			End If

			For Local t$=EachIn argCasts
				Emit t
			Next
'		End If
End Rem
		'PopMungScope
		EndLocalScope
	End Method
	
	Method FuncDeclMangleIdent:String(fdecl:TFuncDecl)

		If (Not fdecl.ClassScope()) Or (equalsBuiltInFunc(fdecl.classScope(), fdecl)) Then
			Return fdecl.ident
		End If	
	
		If Not fdecl.mangled Then
			Local id:String = fdecl.ident

			If fdecl.attrs & FUNC_OPERATOR Then
				id = MungSymbol(id)
			End If

			fdecl.mangled = id + MangleMethod(fdecl)
		End If

		Return fdecl.mangled		
'		If fdecl.olIndex Then
'			Return fdecl.ident + fdecl.olIndex
'		Else
'			Return fdecl.ident
'		End If
	End Method

	Method EmitClassFuncProto( decl:TFuncDecl, isStruct:Int = False, emitFuncProtos:Int = True)
		'PushMungScope
		BeginLocalScope

		decl.Semant

		MungDecl decl

		'Find decl we override
		Local odecl:TFuncDecl=decl
'		If odecl.overrides Then Return
		While odecl.overrides
			odecl=odecl.overrides
		Wend

		'Generate 'args' string and arg casts
		Local args$

		' pass object for method
		If decl.IsMethod() Then
			args :+ TransObject(decl.scope, True)
		End If

		Local argCasts:TStack =New TStack
		For Local i:Int=0 Until decl.argDecls.Length
			Local arg:TArgDecl=decl.argDecls[i]
			Local oarg:TArgDecl=odecl.argDecls[i]
			MungDecl arg
			If args args:+","
			If Not TFunctionPtrType(oarg.ty) Then
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )
				Else
					args:+ oarg.castTo + " " + arg.munged
				End If
			Else
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )
				Else
					args:+ oarg.castTo
				End If
			End If
			If arg.ty.EqualsType( oarg.ty ) Continue
			Local t$=arg.munged
			arg.munged=""
			MungDecl arg
			argCasts.Push TransType( arg.ty, arg.munged )+" "+arg.munged+"=static_cast<"+TransType(arg.ty, "")+" >"+Bra(t)+";"
		Next

		Local id$=decl.munged

		Local bk:String = ";"
		Local pre:String = "typedef "
		Local api:String
		If decl.IsMethod() Then
			id :+ "_m"
		Else
			id :+ "_f"
		End If
		
		If decl.attrs & DECL_API_STDCALL Then
			api = " __stdcall "
		End If

		'If odecl.IsExtern() Then
		'	pre = "extern "
		'End If

'		If Not proto Or (proto And Not odecl.IsExtern()) Then
		'If emitFuncProtos
			If Not TFunctionPtrType(decl.retType) Then
				If Not odecl.castTo Then
					If Not isStruct Then
						Emit pre + TransType( decl.retType, "" )+" "+ Bra(api + "*" + id)+Bra( args ) + bk
					End If
					If emitFuncProtos
						If decl.IsMethod() Then
							Emit TransType(decl.retType, "") + " _" + decl.munged +Bra( args ) + bk
						Else
							Emit TransType(decl.retType, "") + api + " " + decl.munged +Bra( args ) + bk
						End If
					End If
				Else
					If Not odecl.noCastGen Then
						If Not isStruct Then
							If Not decl.overrides Or decl.returnTypeSubclassed Then
								Emit pre + odecl.castTo +" "+Bra(api + "*" + id)+Bra( args ) + bk
							End If
						End If
						If emitFuncProtos
							If decl.IsMethod() Then
								Emit odecl.castTo + " _" + decl.munged +Bra( args ) + bk
							Else
								Emit odecl.castTo + " " + decl.munged +Bra( args ) + bk
							End If
						End If
					End If
				End If
			Else
				If Not odecl.castTo Then
					If Not args Then
						' for function pointer return type, we need to generate () regardless of whether there are
						' args or not.
						args = " "
					End If
					' emit function ptr typedef
					Emit pre + TransType( decl.retType, id + "x") + bk
					' emit actual typedef (with return type of above typedef)
					Emit pre + TransType( decl.retType, id, args, True ) + bk
				Else
					If Not odecl.noCastGen Then
						Emit pre + odecl.castTo +" "+Bra( args ) + bk
					End If
				End If
			End If

			For Local t$=EachIn argCasts
				Emit t
			Next
		'End If

		'PopMungScope
		EndLocalScope
	End Method



	Method EmitFuncDecl( decl:TFuncDecl, proto:Int = False, classFunc:Int = False )
		'If Not proto And decl.IsAbstract() Return

		Local tmpDebug:Int = opt_debug
		If decl.isNoDebug() Then
			opt_debug = False
		End If

		BeginLocalScope

		decl.Semant

		MungDecl decl
		
		' export defs?
		If opt_apptype And opt_def And decl.attrs & DECL_EXPORT Then
			If Not _appInstance.exportDefs.Contains(decl) Then
				_appInstance.exportDefs.AddLast(decl)
			End If
		End If

		' emit nested functions/classes
		If Not proto Then
			' emit nested classes
			For Local cdecl:TClassDecl = EachIn decl._decls
				MungDecl cdecl
				EmitClassProto(cdecl, False)
				EmitClassDecl(cdecl)
			Next
		
			' emit nested protos
			For Local fdecl:TFuncDecl = EachIn decl._decls
				EmitFuncDecl(fdecl, True, classFunc)
			Next
			
			' emit nested bodies
			For Local fdecl:TFuncDecl = EachIn decl._decls
				EmitFuncDecl(fdecl, proto, classFunc)
			Next
		End If

		'Find decl we override
		Local odecl:TFuncDecl=decl
		While odecl.overrides
			odecl=odecl.overrides
		Wend

		'Generate 'args' string and arg casts
		Local args$

		' pass object for method
		If decl.IsMethod() Then
			args :+ TransObject(decl.scope, True) + " o"
		End If

		Local argCasts:TStack =New TStack
		For Local i:Int=0 Until decl.argDecls.Length
			Local arg:TArgDecl=decl.argDecls[i]
			Local oarg:TArgDecl=odecl.argDecls[i]
			MungDecl arg, True
			If args args:+","
			If Not TFunctionPtrType(oarg.ty) Then
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )+" "+arg.munged
				Else
					args:+ oarg.castTo + " " + arg.munged
				End If
			Else
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )
				Else
					args:+ oarg.castTo
				End If
			End If
			If arg.ty.EqualsType( oarg.ty ) Continue
			Local t$=arg.munged
			arg.munged=""
			MungDecl arg
			argCasts.Push TransType( arg.ty, arg.munged )+" "+arg.munged+"=static_cast<"+TransType(arg.ty, "")+" >"+Bra(t)+";"
		Next

		Local id$=decl.munged

		If classFunc Then
			If decl.IsMethod() Then
				id = "_" + id
			End If
		Else
			If Not odecl.IsExtern() Then
				id = id
			End If
		End If

		Local bk:String = "{"
		Local pre:String
		Local api:String
		If proto Then
			If odecl.IsExtern() Then
				pre = "extern "
				If TFunctionPtrType(decl.retType) Then
					pre = ""
				End If
			End If
			bk = ";"
		End If

		If decl.attrs & DECL_API_STDCALL Then
			api = " __stdcall "
		End If

'		If Not proto Or (proto And Not odecl.IsExtern()) Then
		If Not IsStandardFunc(decl.munged) Then
			If Not TFunctionPtrType(odecl.retType) Then
				If Not odecl.castTo Then
					Emit pre + TransType( decl.retType, "" )+ api + " "+id+Bra( args ) + bk
				Else
					If Not odecl.noCastGen Then
						Emit pre + odecl.castTo + api + " "+id+Bra( args ) + bk
					End If
				End If
			Else
				If Not odecl.castTo Then
					If Not args Then
						' for function pointer return type, we need to generate () regardless of whether there are
						' args or not.
						args = " "
					End If
					Emit pre + TransType( decl.retType, id, args )+ bk
				Else
					If Not odecl.noCastGen Then
						Emit pre + odecl.castTo +" "+Bra( args ) + bk
					End If
				End If
			End If

			For Local t$=EachIn argCasts
				Emit t
			Next
		End If

		If Not proto Then

			If PROFILER Then
				Select decl.ident
					Case "WritePixel", "PixelPtr", "CopyPixels", "ConvertPixels", "ConvertPixelsToStdFormat", "ConvertPixelsFromStdFormat"
					Case "OnDebugEnterScope", "OnDebugEnterStm", "GetDbgState", "OnDebugLeaveScope", "OnDebugPopExState", "OnDebugPushExState"
					Default
						DebugPrint("", TransFullName(decl))
				End Select
			End If
				
			If DEBUG Then
				For Local i:Int=0 Until decl.argDecls.Length
					Local arg:TArgDecl=decl.argDecls[i]
					DebugObject(arg.ty, arg.munged, id)
				Next
			End If

			If decl.IsAbstract() Then
				Emit "brl_blitz_NullMethodError();"
				If Not TVoidType( decl.retType ) Then
					Local ret:TReturnStmt = New TReturnStmt.Create(New TConstExpr.Create( decl.retType,"" ).Semant())
					ret.fRetType = decl.retType
					Emit ret.Trans() + ";"
					unreachable = False
				End If
			Else

				decl.Semant()
				
				If opt_debug And decl.IsMethod() Then
					Emit TransDebugNullObjectError("o", TClassDecl(decl.scope)) + ";"
				End If

				EmitLocalDeclarations(decl)

				EmitBlock decl

			End If
			Emit "}"
		End If

		' reset label ids
		contLabelId = 0
		exitLabelId = 0

		EndLocalScope
		'PopMungScope
		
		opt_debug = tmpDebug
		
	End Method
	
	Method EmitLocalDeclarations(decl:TScopeDecl, ignoreVar:TValDecl = Null)
		If opt_debug Then
			For Local ldecl:TLocalDecl = EachIn decl.Decls()
				If ldecl <> ignoreVar Then
					If Not TArgDecl(ldecl) And Not ldecl.generated Then
						MungDecl ldecl
						Local ty:TType = ldecl.ty

						Local t:String = TransLocalDeclNoInit(ldecl)
						
'						If TObjectType( ty ) And TObjectType( ty ).classDecl.IsStruct() Then
'							t :+ "={}"
'						End If
						
						Emit t + ";"
					End If
				End If
			Next
		End If
	End Method

	Method EmitClassFieldsProto(classDecl:TClassDecl)

		If classDecl.superClass Then
			EmitClassFieldsProto(classDecl.superClass)
		End If

		For Local decl:TFieldDecl = EachIn classDecl.Decls()
			decl.Semant()

			If Not TFunctionPtrType(decl.ty) Then
				If classDecl.IsExtern() Then
					Emit TransType(decl.ty, "") + " " + decl.ident + ";"
				Else
					Emit TransType(decl.ty, classDecl.actual.munged) + " _" + classDecl.actual.munged.ToLower() + "_" + decl.IdentLower() + ";"
				End If
			Else
				If classDecl.IsExtern() Then
					Emit TransType(decl.ty, decl.ident) + ";"
				Else
					Emit TransType(decl.ty, "_" + classDecl.actual.munged.ToLower() + "_" + decl.IdentLower()) + ";"
				End If
			End If
		Next

	End Method

	Method EmitClassGlobalsProto(classDecl:TClassDecl)

		For Local decl:TGlobalDecl = EachIn classDecl.Decls()
			decl.Semant()

			If TFunctionPtrType(decl.ty) Then
					Emit TransRefType( decl.ty, decl.munged ) + ";"
			Else
				Emit "extern "+TransRefType( decl.ty, "" )+" "+ decl.munged+";"
			End If
		Next

	End Method

	Method BBClassClassFuncProtoBuildList( classDecl:TClassDecl, list:TList )

		'Local reserved:String = ",New,Delete,ToString,Compare,SendMessage,_reserved1_,_reserved2_,_reserved3_,".ToLower()

		If classDecl.superClass Then
			BBClassClassFuncProtoBuildList(classDecl.superClass, list)
		End If
		
		For Local idecl:TClassDecl = EachIn classDecl.implmentsAll
			BBClassClassFuncProtoBuildList(idecl, list)
		Next

		For Local decl:TDecl=EachIn classDecl.Decls()
			Local fdecl:TFuncDecl =TFuncDecl( decl )
			If fdecl
				If Not fdecl.IsSemanted()
					fdecl.Semant()
				End If

				If Not equalsBuiltInFunc(classDecl, fdecl) And Not equalsTorFunc(classDecl, fdecl) Then
				
					Local ignore:Int
					Local link:TLink=list._head._succ
					While link<>list._head
						Local ofdecl:TFuncDecl = TFuncDecl(link._value)
						If fdecl.ident = ofdecl.ident And fdecl.EqualsArgs(ofdecl) Then

							If fdecl.overrides Then
								link._value = fdecl
								ignore = True
								Exit
							End If
							
							ignore = True
						EndIf
						link = link._succ
					Wend

					If Not ignore Then
						list.AddLast(fdecl)
					End If
				
					Continue
				End If
			EndIf
		Next

	End Method

	Method EmitBBClassClassFuncProto( classDecl:TClassDecl )

		Local list:TList = New TList
		
		BBClassClassFuncProtoBuildList(classDecl, list)

		For Local fdecl:TFuncDecl = EachIn list
			EmitBBClassFuncProto( fdecl )
		Next

	End Method

	Method EmitClassProto( classDecl:TClassDecl, emitFuncProtos:Int = True )
	
		If classDecl.args Then
			Return
		End If

		Local classid$=classDecl.munged
		Local superid$
		If classDecl.superClass Then
			superid=classDecl.superClass.actual.munged
		End If

		'Emit "void _" + classid + "_New" + Bra(TransObject(classdecl) + " o") + ";"
		
		If emitFuncProtos Then
			EmitClassDeclNewListProto(classDecl)

			If classHierarchyHasFunction(classDecl, "Delete") Then
				Emit "void _" + classid + "_Delete" + Bra(TransObject(classdecl) + " o") + ";"
			End If
	
			If classHasFunction(classDecl, "ToString") Then
				Emit "BBSTRING _" + classid + "_ToString" + Bra(TransObject(classdecl) + " o") + ";"
			End If
	
			If classHasFunction(classDecl, "Compare") Then
				Emit "BBINT _" + classid + "_Compare(" + TransObject(classdecl) + " o, BBOBJECT otherObject);"
			End If
	
			If classHasFunction(classDecl, "SendMessage") Then
				Emit "BBOBJECT _" + classid + "_SendMessage(" + TransObject(classdecl) + " o, BBOBJECT message, BBOBJECT source);"
			End If

		End If
		'Local reserved:String = ",New,Delete,ToString,Compare,SendMessage,_reserved1_,_reserved2_,_reserved3_,".ToLower()

		classDecl.SemantParts()

		'Local fdecls:TFuncDecl[] = classDecl.GetAllFuncDecls(Null, False)
		For Local decl:TDecl=EachIn classDecl.Decls()
		'For Local fdecl:TFuncDecl = EachIn fdecls

			Local fdecl:TFuncDecl =TFuncDecl( decl )
			If fdecl
				If Not equalsBuiltInFunc(classDecl, fdecl) And Not equalsTorFunc(classDecl, fdecl) Then
					EmitClassFuncProto( fdecl, , emitFuncProtos )
					Continue
				End If
			EndIf

			Local gdecl:TGlobalDecl =TGlobalDecl( decl )
			If gdecl
				MungDecl gdecl
			'	Emit "static "+TransRefType( gdecl.ty )+" "+gdecl.munged+";"
				Continue
			EndIf
		Next

		Emit ""

		' emit the class structure
		Emit "struct BBClass_" + classid + " {"
		If classDecl.superClass.ident = "Object" Then
			Emit "BBClass*  super;"
		Else
			Emit "struct BBClass_" + classDecl.superClass.munged + "*  super;"
		End If
		Emit "void      (*free)( BBObject *o );"
		Emit "BBDebugScope* debug_scope;"
		Emit "unsigned int instance_size;"
		Emit "void      (*ctor)( BBOBJECT o );"
		Emit "void      (*dtor)( BBOBJECT o );"
		Emit "BBSTRING  (*ToString)( BBOBJECT x );"
		Emit "int       (*Compare)( BBOBJECT x,BBOBJECT y );"
		Emit "BBOBJECT  (*SendMessage)( BBOBJECT o,BBOBJECT m,BBOBJECT s );"
		Emit "BBINTERFACETABLE itable;"
		Emit "void*     extra;"
		Emit "unsigned int obj_size;"

		EmitBBClassClassFuncProto(classDecl)

		Emit "};~n"

		If classDecl.IsInterface() Then
			Emit "struct " + classid + "_methods {"
			EmitBBClassClassFuncProto(classDecl)
			Emit "};~n"
		End If

		Emit "struct " + classid + "_obj {"
		Emit "struct BBClass_" + classid + "* clas;"

		BeginLocalScope
		EmitClassFieldsProto(classDecl)
		EndLocalScope

		Emit "};"

		Emit "extern struct BBClass_" + classid + " " + classid + ";"

		EmitClassGlobalsProto(classDecl);

		' fields
		For Local decl:TFieldDecl = EachIn classDecl.Decls()
			MungDecl decl
		Next
		
	End Method


	Method EmitExternClassFuncProto( classDecl:TClassDecl )

		If classDecl.superClass Then
			EmitExternClassFuncProto(classDecl.superClass)
		End If

		For Local decl:TFuncDecl = EachIn classDecl.Decls()
			decl.Semant()

			' code is written as a method, but emitted as a function pointer
			' with self as the first parameter
			Local func:TFuncDecl = TFuncDecl(decl.Copy())
			Local argDecl:TArgDecl = New TArgDecl.Create("This", classDecl.objectType, Null)

			func.argDecls = [argDecl] + func.argDecls
			
			func.Semant()
			
			Local ty:TFunctionPtrType = New TFunctionPtrType
			ty.func = func
			
			Emit TransType(ty, decl.Ident) + ";"

		Next
	End Method

	Method EmitExternClassTypeFuncProto( classDecl:TClassDecl )

		Local doneCtorDtor:Int
		Local iDecl:TClassDecl

		For Local decl:TFuncDecl = EachIn classDecl.GetAllOriginalFuncDecls(Null, True)
			decl.Semant()
			
			' first interface preceeds ctor/dtor
			If Not doneCtorDtor
				If Not iDecl And TClassDecl(decl.scope).IsInterface() Then
					iDecl = TClassDecl(decl.scope)
				End If
				
				If iDecl
					If iDecl <> TClassDecl(decl.scope) Then
						' a different interface
						doneCtorDtor = True
						Emit "void(*_ctor)();"
						Emit "void(*_dtor)();"
					End If
				Else
					doneCtorDtor = True
					Emit "void(*_ctor)();"
					Emit "void(*_dtor)();"
				End If
				
			End If

			' code is written as a method, but emitted as a function pointer
			' with self as the first parameter
			Local func:TFuncDecl = TFuncDecl(decl.Copy())
			Local argDecl:TArgDecl = New TArgDecl.Create("This", classDecl.objectType, Null)

			func.argDecls = [argDecl] + func.argDecls
			
			func.Semant()
			
			Local ty:TFunctionPtrType = New TFunctionPtrType
			ty.func = func
			
			Emit TransType(ty, decl.Ident) + ";"

		Next
	End Method

	Method EmitExternClassProto( classDecl:TClassDecl )

		Emit "typedef struct " + classDecl.ident + " " + classDecl.ident + ";"
		
		' vtable
		Emit "struct " + classDecl.ident  + "Vtbl {"
		
		' methods
		If classDecl.IsInterface() Then
			EmitExternClassFuncProto(classDecl)
		Else
			EmitExternClassTypeFuncProto(classDecl)
		End If

		Emit "};"
		
		Emit "struct " + classDecl.ident + " {"
		Emit "struct " + classDecl.ident + "Vtbl* vtbl;"
		Emit "};"

	End Method

	Field emittedStructs:TList = New TList

	Method EmitStructClassProto( classDecl:TClassDecl )
	
		If emittedStructs.Contains(classDecl) Return
		
		emittedStructs.AddLast(classDecl)
		
		' emit any dependent structs first
		For Local decl:TFieldDecl = EachIn classDecl.Decls()
			decl.Semant()
			
			If TObjectType(decl.ty) And TObjectType(decl.ty).classDecl.IsStruct() Then
				If Not emittedStructs.Contains(TObjectType(decl.ty).classDecl) Then
					EmitStructClassProto(TObjectType(decl.ty).classDecl)
				End If
			End If
		Next

		If classDecl.IsExtern()
			Emit "struct " + classDecl.ident + " {"
		Else
			EmitClassDeclNewListProto( classDecl )


			For Local fdecl:TFuncDecl=EachIn classDecl.Decls()
	
				If fdecl.IdentLower() <> "new" Then
					EmitClassFuncProto( fdecl, True )
				End If

			Next
		
			Emit "struct " + classDecl.munged + " {"
		End If

		BeginLocalScope
		EmitClassFieldsProto(classDecl)
		EndLocalScope

		Emit "};"

	End Method

	Method classHasFunction:Int(classDecl:TClassDecl, func:String)
		Local f:String = func.ToLower()
		For Local decl:TFuncDecl = EachIn classDecl.Decls()
			If Not decl.IsSemanted() Then
				decl.Semant
			End If
			If decl.IdentLower() = f And equalsBuiltInFunc(classDecl.superClass, decl) Then
				Return True
			End If
		Next
		Return False
	End Method

	Method classHierarchyHasFunction:Int(classDecl:TClassDecl, func:String)
		If classHasFunction(classDecl, func) Return True
		If classDecl.superClass And classDecl.superClass.munged <> "bbObjectClass" Then
			Return classHierarchyHasFunction(classDecl.superClass, func)
		End If
		Return False
	End Method

	Method classidForFunction:String(classDecl:TClassDecl, func:String)
		If classHasFunction(classDecl, func) Return classDecl.munged
		If classDecl.superClass And classDecl.superClass.munged <> "bbObjectClass" Then
			Return classidForFunction(classDecl.superClass, func)
		End If
		Return Null
	End Method

	Method EmitMark( id$,ty:TType,queue:Int )

		If TObjectType( ty )

			If id.EndsWith( ".p" )
				If ty.GetClass().IsInterface() id=id[..-2] Else InternalErr "TCTranslator.EmitMark"
			Else
				If ty.GetClass().IsInterface() InternalErr "TCTranslator.EmitMark"
			EndIf

			If queue
				Emit "gc_mark_q("+id+");"
			Else
				Emit "gc_mark("+id+");"
			EndIf
		Else If TArrayType( ty )
			Emit "gc_mark("+id+");"
			Return
		EndIf
	End Method
	
	Method EmitClassConstsDebugScope(classDecl:TClassDecl)
	
		For Local decl:TConstDecl = EachIn classDecl.Decls()
			EmitConstDebugScope(decl)
		Next

	End Method

	Method EmitConstDebugScope(decl:TConstDecl)
	
		Emit "{"
		Emit "BBDEBUGDECL_CONST,"
		Emit Enquote(decl.ident) + ","
		Emit Enquote(TransDebugScopeType(decl.ty) + TransDebugMetaData(decl.metadata.metadataString)) + ","
		
		_appInstance.mapStringConsts(decl.value)
		
		Emit ".const_value=&" + TStringConst(_appInstance.stringConsts.ValueForKey(decl.value)).id
		Emit "},"

	End Method

	Method EmitClassFieldsDebugScope(classDecl:TClassDecl)

		' Don't list superclass fields in our debug scope
		'If classDecl.superClass Then
		'	EmitClassFieldsDebugScope(classDecl.superClass)
		'End If

		For Local decl:TFieldDecl = EachIn classDecl.Decls()
			Emit "{"
			Emit "BBDEBUGDECL_FIELD,"
			Emit Enquote(decl.ident) + ","
			Emit Enquote(TransDebugScopeType(decl.ty) + TransDebugMetaData(decl.metadata.metadataString)) + ","

			Local offset:String = ".field_offset=offsetof"
			
			If classDecl.IsStruct() Then
				offset :+ Bra("struct " + classDecl.munged + "," + decl.munged)
			Else
				offset :+ Bra("struct " + classDecl.munged + "_obj," + decl.munged)
			End If
'			If WORD_SIZE = 8 Then
'				Emit Bra("BBLONG") + offset
'			Else
			Emit offset
'			End If
			'If Not TFunctionPtrType(decl.ty) Then
			'	Emit TransType(decl.ty, classDecl.actual.munged) + " _" + classDecl.actual.munged.ToLower() + "_" + decl.ident.ToLower() + ";"
			'Else
			'	Emit TransType(decl.ty, "_" + classDecl.actual.munged.ToLower() + "_" + decl.ident.ToLower()) + ";"
			'End If
			Emit "},"
			
			'offset:+ decl.ty.GetSize()
		Next
		
		'Return offset
	End Method
	
	Method EmitClassStandardMethodDebugScope(ident:String, ty:String, munged:String)
			Emit "{"
			Emit "BBDEBUGDECL_TYPEMETHOD,"
			Emit Enquote(ident) + ","
			Emit Enquote(ty) + ","
			Emit ".var_address=(void*)&" + munged
			Emit "},"
	End Method
	
	Method TransDebugMetaData:String(meta:String)
		If meta Then
			Return "{" + meta + "}"
		End If
	End Method

	Method EmitBBClassFuncsDebugScope(decl:TFuncDecl)

			Emit "{"
			If decl.IsMethod() Then
				Emit "BBDEBUGDECL_TYPEMETHOD,"
			Else
				Emit "BBDEBUGDECL_TYPEFUNCTION,"
			End If
			Emit Enquote(decl.ident) + ","
			
			Local s:String = "("
			For Local i:Int = 0 Until decl.argDecls.length
				If i Then
					s:+ ","
				End If
				s:+ TransDebugScopeType(decl.argDecls[i].ty)
			Next
			s:+ ")"

			If decl.retType Then
				s:+ TransDebugScopeType(decl.retType)
			End If

			s:+ TransDebugMetaData(decl.metadata.metadataString)

			Emit Enquote(s) + ","
			If decl.IsMethod() Or decl.IsCTor() Then 
				Emit ".var_address=(void*)&_" + decl.munged
			Else
				Emit ".var_address=(void*)&" + decl.munged
			End If
			Emit "},"
	End Method

	Method BBClassClassFuncsDebugScopeBuildList(classDecl:TClassDecl, list:TList)
		'Local reserved:String = ",New,Delete,ToString,Compare,SendMessage,_reserved1_,_reserved2_,_reserved3_,".ToLower()

		For Local decl:TDecl=EachIn classDecl.GetAllFuncDecls(Null, False)
			Local fdecl:TFuncDecl =TFuncDecl( decl )
			If fdecl
				If Not fdecl.IsSemanted()
					fdecl.Semant()
				End If
				If Not classDecl.IsInterface() And fdecl.IsAbstract() Then
					Continue
				End If

				If Not equalsBuiltInFunc(classDecl, fdecl) Then
				
					Local ignore:Int
					Local link:TLink=list._head._succ
					While link<>list._head
						If fdecl.ident = TFuncDecl(link._value).ident Then
							If fdecl.overrides Then
								link._value = fdecl
								ignore = True
								Exit
							End If
						EndIf
						link = link._succ
					Wend

					If Not ignore Then
						list.AddLast(fdecl)
					End If
				
					Continue
				End If
			EndIf
		Next
	End Method
	

	Method EmitBBClassClassFuncsDebugScope(classDecl:TClassDecl)
	
		Local list:TList = New TList
		
		BBClassClassFuncsDebugScopeBuildList(classDecl, list)
	
		For Local fdecl:TFuncDecl = EachIn list
			EmitBBClassFuncsDebugScope( fdecl )
		Next

	End Method

	Method EmitClassFuncsDebugScope(classDecl:TClassDecl)

		If classDecl.IsExtern() Return

		Local classid$=classDecl.munged
		Local superid$
		If classDecl.superClass Then
			superid = classDecl.superClass.actual.munged
		End If

		Local ret:String = "()i"
		If opt_issuperstrict Then
			ret = "()"
		End If
		
		If Not classDecl.IsInterface() Then
			EmitClassStandardMethodDebugScope("New", ret, "_" + classid + "_New")
		End If
	
		If classHasFunction(classDecl, "ToString") Then
			EmitClassStandardMethodDebugScope("ToString", "()$", "_" + classidForFunction(classDecl, "ToString") + "_ToString")
			'Emit "_" + classid + "_ToString,"
		End If

		If classHasFunction(classDecl, "Compare") Then
			EmitClassStandardMethodDebugScope("Compare", "(:Object)i", "_" + classidForFunction(classDecl, "Compare") + "_Compare")
			'Emit "_" + classid + "_ObjectCompare,"
		End If

		If classHasFunction(classDecl, "SendMessage") Then
			EmitClassStandardMethodDebugScope("SendMessage", "(:Object, :Object):Object", "_" + classidForFunction(classDecl, "SendMessage") + "_SendMessage")
			'Emit "_" + classid + "_SendMessage,"
		End If

		EmitBBClassClassFuncsDebugScope(classDecl)

	End Method
	
	Method EmitClassGlobalDebugScope( classDecl:TClassDecl )
		For Local decl:TGlobalDecl = EachIn classDecl.Decls()
			EmitGlobalDebugScope(decl)
		Next
	End Method

	Method EmitGlobalDebugScope( decl:TGlobalDecl )
		Emit "{"
		Emit "BBDEBUGDECL_GLOBAL,"
		Emit Enquote(decl.ident) + ","
		Emit Enquote(TransDebugScopeType(decl.ty)) + ","
		Emit ".var_address=(void*)&" + decl.munged
		Emit "},"
	End Method

	Method CountBBClassClassFuncsDebugScope(classDecl:TClassDecl, count:Int Var)

		For Local decl:TDecl=EachIn classDecl.GetAllFuncDecls(Null, False)
			Local fdecl:TFuncDecl =TFuncDecl( decl )
			If fdecl
				If Not classDecl.IsInterface() And fdecl.IsAbstract() Then
					Continue
				End If

				If Not equalsBuiltInFunc(classDecl, fdecl) Then
					count :+ 1
				End If
			End If
		Next
	End Method

	Method CountClassConstsDebugScope(classDecl:TClassDecl, count:Int Var)

		For Local decl:TConstDecl = EachIn classDecl.Decls()
			count :+ 1
		Next
	End Method

	Method CountClassFieldsDebugScope(classDecl:TClassDecl, count:Int Var)

		For Local decl:TFieldDecl = EachIn classDecl.Decls()
			count :+ 1
		Next
	End Method
	
	Method DebugScopeDeclCount:Int(classDecl:TClassDecl)
		Local count:Int = 2 ' "New" counts as first one
		
		' but we don't use "New" for interfaces...
		If classDecl.IsInterface() Or (classDecl.IsExtern() And classDecl.IsStruct()) Then
			count :- 1
		End If

		' consts		
		CountClassConstsDebugScope(classDecl, count)

		' fields
		CountClassFieldsDebugScope(classDecl, count)
		
		' standard methods
		If classHasFunction(classDecl, "ToString") Then
			count :+ 1
		End If

		If classHasFunction(classDecl, "Compare") Then
			count :+ 1
		End If

		If classHasFunction(classDecl, "SendMessage") Then
			count :+ 1
		End If
		
		' methods and functions
		CountBBClassClassFuncsDebugScope(classDecl, count)
		
		' class globals
		For Local decl:TGlobalDecl = EachIn classDecl.Decls()
			count :+ 1
		Next
		
		Return count
	End Method

	Method EmitClassDecl( classDecl:TClassDecl )

		If classDecl.args Then
			Return
		End If

		PushEnv classDecl
		'If classDecl.IsTemplateInst()
		'	Return
		'EndIf

		If classDecl.IsExtern() And Not classDecl.IsStruct() Then
			Return
		EndIf

		Local classid$=classDecl.munged
		Local superid$
		If classDecl.superClass Then
			superid = classDecl.superClass.actual.munged
		End If


		If Not classDecl.IsExtern() Then
			' process nested classes
			For Local cdecl:TClassDecl = EachIn classDecl._decls
				MungDecl cdecl
				EmitClassProto(cdecl, False)
				EmitClassDecl(cdecl)
			Next
		
			' process nested functions for new
			Local decl:TFuncDecl
			Try
				decl = classDecl.FindFuncDecl("new",,,,,True,SCOPE_CLASS_HEIRARCHY)
			Catch e:String
			End Try
			If decl And decl.scope = classDecl Then ' only our own New method, not any from superclasses
				decl.Semant
				' emit nested protos
				For Local fdecl:TFuncDecl = EachIn decl._decls
					EmitFuncDecl(fdecl, True, False)
				Next
				
				' emit nested bodies
				For Local fdecl:TFuncDecl = EachIn decl._decls
					EmitFuncDecl(fdecl, False, False)
				Next
			End If
	
			EmitClassDeclNewList(classDecl)
			
			If Not (classDecl.attrs & CLASS_STRUCT) Then
				' process nested functions for delete
				decl = classDecl.FindFuncDecl("delete",,,,,,SCOPE_CLASS_HEIRARCHY)
				If decl Then
					decl.Semant
					' emit nested protos
					For Local fdecl:TFuncDecl = EachIn decl._decls
						EmitFuncDecl(fdecl, True, False)
					Next
					
					' emit nested bodies
					For Local fdecl:TFuncDecl = EachIn decl._decls
						EmitFuncDecl(fdecl, False, False)
					Next
				End If
		
				If classHierarchyHasFunction(classDecl, "Delete") Then
					EmitClassDeclDelete(classDecl)
				End If
			End If
	
			Rem
			'fields ctor
			Emit classid+"::"+classid+"(){"
			For Local decl:TDecl=EachIn classDecl.Semanted()
				Local fdecl:TFieldDecl=TFieldDecl( decl )
				If Not fdecl Continue
				Emit TransField(fdecl,Null)+"="+fdecl.init.Trans()+";"
			Next
			Emit "}"
			End Rem
			Local reserved:String = ",New,Delete,".ToLower()
	
			'methods
			For Local decl:TDecl=EachIn classDecl.Decls()
	
				Local fdecl:TFuncDecl=TFuncDecl( decl )
				If fdecl
					If reserved.Find("," + fdecl.IdentLower() + ",") = -1 Then
						EmitGDBDebug(fdecl)
						EmitFuncDecl fdecl, , True
						Continue
					End If
				EndIf
	
			'Local gdecl:TGlobalDecl=TGlobalDecl( decl )
			'If gdecl
			'		Emit TransRefType( gdecl.ty )+" "+classid+"::"+gdecl.munged+";"
			'		Continue
			'	EndIf
			Next
			Rem
			'gc_mark
			Emit "void "+classid+"::mark(){"
			If classDecl.superClass
				Emit classDecl.superClass.actual.munged+"::mark();"
			EndIf
			For Local decl:TDecl=EachIn classDecl.Semanted()
				Local fdecl:TFieldDecl=TFieldDecl( decl )
				If fdecl EmitMark TransField(fdecl,Null),fdecl.ty,True
			Next
			Emit "}"
			End Rem
	
			For Local decl:TDecl=EachIn classDecl.Semanted()
				Local gdecl:TGlobalDecl =TGlobalDecl( decl )
				If gdecl
					If TFunctionPtrType(gdecl.ty) Then
						Emit TransRefType( gdecl.ty, gdecl.munged ) + ";"
					Else
						Emit TransRefType( gdecl.ty, "" )+" "+gdecl.munged+";"
					End If
					Continue
				EndIf
			Next
	
			reserved = ",New,Delete,ToString,Compare,SendMessage,_reserved1_,_reserved2_,_reserved3_,".ToLower()

		End If

			
		'Emit "struct _" + classid + "_DebugScope{"
		'Emit "int kind;"
		'Emit "const char *name;"
		'Emit "BBDebugDecl decls[" + DebugScopeDeclCount(classDecl) + "];"
		'Emit "};"
		Local count:Int = DebugScopeDeclCount(classDecl)
		
		' debugscope
		If count > 1 Then
			_app.scopeDefs.Insert(String(count - 1), "")
			Emit "struct BBDebugScope_" + (count - 1) + " " + classid + "_scope ={"
		Else
			Emit "struct BBDebugScope " + classid + "_scope ={"
		End If
		
		If classDecl.IsInterface() Then
			Emit "BBDEBUGSCOPE_USERINTERFACE,"
		Else If classDecl.IsStruct() Then
			Emit "BBDEBUGSCOPE_USERSTRUCT,"
		Else
			Emit "BBDEBUGSCOPE_USERTYPE,"
		End If
		Emit EnQuote(classDecl.ident + TransDebugMetaData(classDecl.metadata.metadataString)) + ","

		Emit "{"
		
		' debug const decls
		EmitClassConstsDebugScope(classDecl)
		
		' debug field decls
		EmitClassFieldsDebugScope(classDecl)
		
		' debug global decls
		EmitClassGlobalDebugScope(classDecl)
		
		' debug func decls
		EmitClassFuncsDebugScope(classDecl)
		
		Emit "BBDEBUGDECL_END"
		Emit "}"

		Emit "};"

		Local fdecls:TFuncDecl[] = classDecl.GetAllFuncDecls()
		Local implementedInterfaces:TMap = classDecl.GetInterfaces()
		Local ifcCount:Int

		If Not classDecl.IsStruct() Then

			' interface class implementation
			If Not classDecl.IsInterface()
				If Not implementedInterfaces.IsEmpty() Then
					Emit "struct " + classid + "_vdef {"
					For Local ifc:TClassDecl = EachIn implementedInterfaces.Values()
						Emit "struct " + ifc.munged + "_methods interface_" + ifc.ident + ";"
						ifcCount :+ 1
					Next
					Emit "};~n"
				
					Emit "static struct BBInterfaceOffsets " + classid + "_ifc_offsets[] = {"
					For Local ifc:TClassDecl = EachIn implementedInterfaces.Values()
						Emit "{&" + ifc.munged + "_ifc, offsetof(struct " + classid + "_vdef, interface_" + ifc.ident + ")},"
					Next
					Emit "};~n"
		
					Emit "struct " + classid + "_vdef " + classid + "_ifc_vtable = {"
					For Local ifc:TClassDecl = EachIn implementedInterfaces.Values()
						Emit ".interface_" + ifc.ident + "={"

						Local dups:TMap = New TMap
						
						For Local func:TFuncDecl = EachIn ifc.GetImplementedFuncs()
						
							If func.IsMethod() Then
							
								For Local f:TFuncDecl = EachIn fdecls

									Mungdecl f
									If f.ident = func.ident And f.EqualsFunc(func) Then
									
										Local id:String = f.ident + "_"
										
										For Local arg:TArgDecl = EachIn f.argDecls
											id :+ TransMangleType(arg.ty)
										Next
										
										If Not dups.ValueForKey(id) Then

											Emit "_" + f.munged + ","
										
											dups.Insert(id, "")
										End If
										Exit
									End If
								Next
						
							End If
						Next
						Emit "},"
					Next
					Emit "};~n"
					
					Emit "struct BBInterfaceTable " + classid + "_itable = {"
					Emit classid + "_ifc_offsets,"
					Emit "&" + classid + "_ifc_vtable,"
					Emit ifcCount
					Emit "};~n"
				End If
			End If
			
			Emit "struct BBClass_" + classid + " " + classid + "={"
	
			' super class reference
			Emit "&" + classDecl.superClass.munged + ","
			Emit "bbObjectFree,"
			' debugscope
			Emit "(BBDebugScope*)&" + classid + "_scope,"
			' object instance size
			Emit "sizeof" + Bra("struct " + classid + "_obj") + ","
	
			' standard methods
			Emit "(void (*)(BBOBJECT))_" + classid + "_New,"
	
			If Not classHierarchyHasFunction(classDecl, "Delete") Then
				Emit "bbObjectDtor,"
			Else
				Emit "(void (*)(BBOBJECT))_" + classid + "_Delete,"
			End If
	
			If classHierarchyHasFunction(classDecl, "ToString") Then
				Emit "(BBSTRING (*)(BBOBJECT))_" + classidForFunction(classDecl, "ToString") + "_ToString,"
			Else
				Emit "bbObjectToString,"
			End If
	
			If classHierarchyHasFunction(classDecl, "Compare") Then
				Emit "(int (*)(BBOBJECT))_" + classidForFunction(classDecl, "Compare") + "_Compare,"
			Else
				Emit "bbObjectCompare,"
			End If
	
			If classHierarchyHasFunction(classDecl, "SendMessage") Then
				Emit "(BBOBJECT (*)(BBOBJECT, BBOBJECT, BBOBJECT))_" + classidForFunction(classDecl, "SendMessage") + "_SendMessage,"
			Else
				Emit "bbObjectSendMessage,"
			End If
	
			'Emit "public:"
	
			'fields
			'For Local decl:TDecl=EachIn classDecl.Semanted()
			'	Local fdecl:TFieldDecl =TFieldDecl( decl )
			'	If fdecl
			'		Emit TransRefType( fdecl.ty )+" "+fdecl.munged+";"
			'		Continue
			'	EndIf
			'Next
	
			'fields ctor
			'Emit classid+"();"
	
			'methods
			'For Local decl:TDecl=EachIn classDecl.Semanted()
			'
			'	Local fdecl:TFuncDecl =TFuncDecl( decl )
			'	If fdecl
			'		EmitFuncProto fdecl
			'		Continue
			'	EndIf
			'
			'	Local gdecl:TGlobalDecl =TGlobalDecl( decl )
			'	If gdecl
			'		Emit "static "+TransRefType( gdecl.ty )+" "+gdecl.munged+";"
			'		Continue
			'	EndIf
			'Next
	
			'gc mark
			'Emit "void mark();"
	
			If classDecl.IsInterface() Or implementedInterfaces.IsEmpty() Then
				' itable
				Emit "0,"
				' extra pointer
				Emit "0,"
			Else
				Emit "&" + classid + "_itable,"
				' extra pointer
				Emit "0,"
			End If
	
			' obj_size
			Emit TransObjectSize(classDecl)
	
	
			' methods/funcs
			'reserved = "New,Delete,ToString,ObjectCompare,SendMessage".ToLower()

			'For Local decl:TFuncDecl = EachIn classDecl.Decls()
			For Local decl:TFuncDecl = EachIn fdecls
			
				If Not equalsBuiltInFunc(classDecl, decl) And Not equalsTorFunc(classDecl, decl) Then
	
					Local fdecl:TFuncDecl = classDecl.GetLatestFuncDecl(decl)
	
					MungDecl decl
					
					Local t:String = ","

					If fdecl  <> decl Then

						MungDecl fdecl
						
						If decl.IsMethod() Then
							t :+ Bra(fdecl.munged + "_m")
						Else
							t :+ Bra(fdecl.munged + "_f")
						End If
					End If
	
					If decl.IsMethod() Then
						t:+ "_"
					End If
					
					t :+ decl.munged
					
					Emit t
				End If
			Next
	
			Emit "};~n"
	
			If classDecl.IsInterface()  Then
				Emit "const struct BBInterface " + classid + "_ifc = { &" + classid + ", (const char *) ~q" + classDecl.ident + "~q };"
			Else
				
			End If
			
		End If
		
		PopEnv

	End Method

	Method EmitEnumDecl(decl:TEnumDecl)

		Local id:String = decl.munged

		Emit "struct BBEnum" + decl.munged + "{"
		Emit "const char * name;"
		Emit "char * type;"
		Emit "char * atype;"
		Emit "int flags;"
		Emit "int length;"
		Emit "void * values;"
		Emit "BBString * names[" + decl.values.length + "];"
		Emit "};"
		
		' debugscope
		Emit "struct BBDebugScope " + id + "_scope ={"
		Emit "BBDEBUGSCOPE_USERENUM,"

		Emit EnQuote(decl.ident) + ","

		Emit "{"

		Local ty:TEnumType = New TEnumType.Create(decl)

		For Local value:TEnumValueDecl = EachIn decl.values
			Emit "{"
			Emit "BBDEBUGDECL_GLOBAL,"
			Emit Enquote(value.ident) + ","
			Emit Enquote(TransDebugScopeType(ty) + TransDebugMetaData(decl.metadata.metadataString)) + ","

			_appInstance.mapStringConsts(value.ident)
			_appInstance.mapStringConsts(value.Value())

			Emit ".const_value=&" + TStringConst(_appInstance.stringConsts.ValueForKey(value.Value())).id
			Emit "},"
		Next
		
		Emit "BBDEBUGDECL_END"
		Emit "}"

		Emit "};"
		
		Local t:String
		Local n:String
		For Local v:TEnumValueDecl = EachIn decl.values
			If t Then
				t :+ ","
				n :+ ","
			End If
			t :+ v.Value()
			n :+ "&" + TStringConst(_appInstance.stringConsts.ValueForKey(v.ident)).id
		Next
		
		Emit TransType(decl.ty, "") + " " + decl.munged + "_values[" + decl.values.length + "] = {" + t + "};"
		
		Emit "struct BBEnum" + decl.munged + " " + decl.munged + "_BBEnum = {"
		Emit EnQuote(decl.ident) + ","
		Emit TransArrayType(decl.ty) + ","
		Emit TransArrayType(New TEnumType.Create(decl)) + ","
		Emit decl.isFlags + ","
		Emit decl.values.length + ","
		Emit "&" + decl.munged + "_values,"
		Emit "{" + n + "}"
		Emit "};"
		
		Emit "BBEnum * " + decl.munged + "_BBEnum_impl;"


		For Local fdecl:TFuncDecl = EachIn decl.FuncDecls()
			MungDecl fdecl
			Select fdecl.ident
				Case "ToString"
					Emit "BBSTRING " + fdecl.munged + Bra(TransType(decl.ty, "") + " ordinal") + " {"
					Emit "return bbEnumToString_" + TransDebugScopeType(decl.ty) + Bra(decl.munged + "_BBEnum_impl, ordinal") + ";"
					Emit "}"
			End Select
		Next

	End Method

	Method EmitEnumProto(decl:TEnumDecl)

		Emit "extern BBEnum* " + decl.munged + "_BBEnum_impl;"

		For Local fdecl:TFuncDecl = EachIn decl.FuncDecls()
			MungDecl fdecl
			Select fdecl.ident
				Case "ToString"
					Emit "BBSTRING " + fdecl.munged + Bra(TransType(decl.ty, "")) + ";"
				Case "Ordinal"
					' nothing to generate
			End Select
		Next
	End Method

	Method TransObjectSize:String(classDecl:TClassDecl)
		Local t:String
		
		Local fieldDecl:TFieldDecl

		For Local decl:TFieldDecl = EachIn classDecl.Decls()
			fieldDecl = decl
		Next
		
		If fieldDecl Then
			t = "offsetof" + Bra("struct " + classDecl.munged + "_obj," + fieldDecl.munged) + " - sizeof(void*) + sizeof" + Bra(TransType(fieldDecl.ty, ""))
		Else
			t = "0"
		End If

		Return t
	End Method
	
	
	Method EmitClassDeclNew( classDecl:TClassDecl, fdecl:TFuncDecl )
		Local classid$=classDecl.munged
		Local superid$
		If classDecl.superClass Then
			superid = classDecl.superClass.actual.munged
		End If

		Local t:String = "void _" 
		
		If fdecl.argDecls.Length Then
			If classDecl = fdecl.scope Then
				t :+ fdecl.munged
			Else
				t :+ classDecl.munged + "_" + fdecl.ident + MangleMethod(fdecl)
			End If
		Else
			t :+ classid + "_New"
		End If
		
		'Find decl we override
		Local odecl:TFuncDecl=fdecl
		While odecl.overrides
			odecl=odecl.overrides
		Wend

		Local args:String = TransObject(classdecl, True) + " o"

		For Local i:Int=0 Until fdecl.argDecls.Length
			Local arg:TArgDecl=fdecl.argDecls[i]
			Local oarg:TArgDecl=odecl.argDecls[i]
			MungDecl arg, True
			If args args:+","
			If Not TFunctionPtrType(oarg.ty) Then
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )+" "+arg.munged
				Else
					args:+ oarg.castTo + " " + arg.munged
				End If
			Else
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )
				Else
					args:+ oarg.castTo
				End If
			End If
			If arg.ty.EqualsType( oarg.ty ) Continue
		Next
		
		Emit t + Bra(args) + " {"
		
		Local newDecl:TNewDecl = TNewDecl(fdecl)
		
		If Not classDecl.IsStruct() Then
			' calling constructor?
			If newDecl And newDecl.chainedCtor Then
				mungdecl newDecl.chainedCtor.ctor
				
				Emit "_" + newDecl.chainedCtor.ctor.ClassScope().munged + "_" + newDecl.chainedCtor.ctor.ident + MangleMethod(newDecl.chainedCtor.ctor) + TransArgs(newDecl.chainedCtor.args, newDecl.chainedCtor.ctor, "o") + ";"
			Else
				If classDecl.superClass.ident = "Object" Then
					Emit "bbObjectCtor((BBOBJECT)o);"
				Else
					Emit "_" + superid + "_New((" + TransObject(classDecl.superClass) + ")o);"
				End If
			End If
	
			Emit "o->clas = &" + classid + ";" ' TODO
		End If

		' only initialise fields if we are not chaining to a local (in our class) constructor.
		' this prevents fields being re-initialised through the call-chain.
		If Not newDecl.chainedCtor Or (newDecl.chainedCtor And classDecl <> newDecl.chainedCtor.ctor.scope) Then

			' field initialisation
			For Local decl:TFieldDecl=EachIn classDecl.Decls()
			
				If Not decl.IsSemanted() Then
					decl.Semant()
				End If
			
				Local fld:String
	
				' ((int*)((char*)o + 5))[0] =
				fld :+ TransFieldRef(decl, "o")
	
				If decl.init Then
					If TObjectType(decl.ty) And TObjectType(decl.ty).classdecl.IsExtern() And TObjectType(decl.ty).classdecl.IsStruct() Then
						' skip for uninitialised extern type
						If Not isPointerType(decl.ty) And TConstExpr(decl.init) And Not TConstExpr(decl.init).value Then
							Continue
						End If
					End If
	
					' initial value
					If (TConstExpr(decl.init) And Not TConstExpr(decl.init).value) And TIntrinsicType(decl.ty) Then
						fld :+ "= "
						If TFloat64Type(decl.ty) Then
							fld :+ "_mm_setzero_si64();"
						Else If TFloat128Type(decl.ty) Then
							fld :+ "_mm_setzero_ps();"
						Else If TDouble128Type(decl.ty) Then
							fld :+ "_mm_setzero_pd();"
						Else If TInt128Type(decl.ty) Then
							fld :+ "_mm_setzero_si128();"
						End If
					Else
						If TObjectType(decl.ty) And TObjectType(decl.ty).classdecl.IsStruct() And Not isPointerType(decl.ty) And (TConstExpr(decl.init) And Not TConstExpr(decl.init).value) Then
							fld = "memset(&" + fld + ", 0, sizeof" + Bra(TransType(decl.ty, "")) + ");"
						Else If TInvokeExpr(decl.init) And Not TInvokeExpr(decl.init).invokedWithBraces Then
							fld :+ "= " + TInvokeExpr(decl.init).decl.munged + ";"
						Else If TObjectType(decl.ty) Then
							fld :+ "= " + Bra(TransObject(TObjectType(decl.ty).classDecl)) + decl.init.Trans() + ";"
						Else
							fld :+ "= " + decl.init.Trans() + ";"
						End If
					End If
				Else
					If TNumericType(decl.ty) Or TObjectType(decl.ty) Or IsPointerType(decl.ty, 0, TType.T_POINTER) Then
						fld :+ "= 0;"
					Else If TFunctionPtrType(decl.ty) Then
						fld :+ "= &brl_blitz_NullFunctionError;"
					Else If TStringType(decl.ty) Then
						fld :+ "= &bbEmptyString;"
					Else If TArrayType(decl.ty) Then
						fld :+ "= &bbEmptyArray;"
					End If
				End If
	
				Emit fld
			Next
		
		End If

		'Local decl:TFuncDecl = classDecl.FindFuncDecl("new",,,,,,SCOPE_CLASS_LOCAL)
		If fdecl And (fdecl.scope = classDecl Or fdecl.argDecls.Length) Then ' only our own New method, not any from superclasses
			fdecl.Semant
			If fdecl.munged <> "bbObjectCtor" Then
				EmitLocalDeclarations(fdecl)
				EmitBlock fdecl
			End If
		End If

		'
		Emit "}"
	End Method

	Method EmitClassDeclNewList( classDecl:TClassDecl )
		Local classid$=classDecl.munged

		Local newDecls:TFuncDeclList = TFuncDeclList(classdecl.FindDeclList("new", True,,,True))
		
		For Local fdecl:TFuncDecl = EachIn newDecls
		
			MungDecl fdecl
		
			If fdecl.scope <> classDecl Then
				fdecl.Clear()
				EmitClassDeclNew(classDecl, fdecl)
			Else
				EmitClassDeclNew(classDecl, fdecl)
			End If

			' generate "objectNew" function if required
			If (fdecl.argDecls And fdecl.argDecls.length) Or classDecl.IsStruct() Then
				EmitClassDeclNewInit(classDecl, fdecl)
			End If
		
		Next

	End Method

	Method EmitClassDeclNewListProto( classDecl:TClassDecl )
		Local classid$=classDecl.munged
		'Local superid$=classDecl.superClass.actual.munged

		Local newDecls:TFuncDeclList = TFuncDeclList(classdecl.FindDeclList("new", True,,,True))
		
		For Local fdecl:TFuncDecl = EachIn newDecls
		
			EmitClassDeclNewProto(classDecl, fdecl)
		
			' generate "objectNew" function if required
			If (fdecl.argDecls And fdecl.argDecls.length) Or classDecl.IsStruct() Then
				EmitClassDeclObjectNewProto(classDecl, fdecl)
			End If
		
		Next

	End Method
	
	Method EmitClassDeclNewInit(classDecl:TClassDecl, fdecl:TFuncDecl)

		Local funcMunged:String
		
		If classDecl = fdecl.scope Then
			funcMunged = fdecl.munged
		Else
			funcMunged = classDecl.munged + "_" + fdecl.ident + MangleMethod(fdecl)
		End If

		Local t:String = TransObject(classdecl) + " "
		
		If Not classDecl.IsStruct() Then
			t :+ "_"
		End If

		t :+ funcMunged + "_ObjectNew"

		'Find decl we override
		Local odecl:TFuncDecl=fdecl
		While odecl.overrides
			odecl=odecl.overrides
		Wend

		Local args:String
		
		If Not classDecl.IsStruct() Then
			args = "BBClass * clas"
		End If

		For Local i:Int=0 Until fdecl.argDecls.Length
			Local arg:TArgDecl=fdecl.argDecls[i]
			Local oarg:TArgDecl=odecl.argDecls[i]
			MungDecl arg, True
			If args args:+","
			If Not TFunctionPtrType(oarg.ty) Then
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )+" "+arg.munged
				Else
					args:+ oarg.castTo + " " + arg.munged
				End If
			Else
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )
				Else
					args:+ oarg.castTo
				End If
			End If
			If arg.ty.EqualsType( oarg.ty ) Continue
		Next
		
		Emit t + Bra(args) + " {"

		t = TransObject(classdecl) + " o"

		If classDecl.IsStruct() Then
			Emit t + ";"
		Else
			t :+ " = "
			If ClassHasObjectField(classDecl) Then
				t :+ "bbObjectNewNC"
			Else
				t :+ "bbObjectAtomicNewNC"
			End If
		
			Emit t + "(clas);"
		End If
		
		t = "_" + funcMunged
		
		If classDecl.IsStruct() Then
			t :+ "(&o"
		Else
			t :+ "(o"
		End If
		
		For Local i:Int=0 Until fdecl.argDecls.Length
			Local arg:TArgDecl=fdecl.argDecls[i]
			t :+ ", " + arg.munged
		Next
		
		Emit t + ");"
		
		Emit "return o;"
		
		Emit "}"
		
	End Method

	Method EmitClassDeclNewProto( classDecl:TClassDecl, fdecl:TFuncDecl )
		Local classid$=classDecl.munged
		Local superid$
		If classDecl.superClass Then
			superid = classDecl.superClass.actual.munged
		End If

		Local t:String = "void _" 
		
		If fdecl.argDecls.Length Then
			If classDecl = fdecl.scope Then
				If Not fdecl.munged Then
					MungDecl fdecl
				End If
				t :+ fdecl.munged
			Else
				t :+ classDecl.munged + "_" + fdecl.ident + MangleMethod(fdecl)
			End If
		Else
			t :+ classid + "_New"
		End If
		
		'Find decl we override
		Local odecl:TFuncDecl=fdecl
		While odecl.overrides
			odecl=odecl.overrides
		Wend

		Local args:String = TransObject(classdecl, True) + " o"

		For Local i:Int=0 Until fdecl.argDecls.Length
			Local arg:TArgDecl=fdecl.argDecls[i]
			Local oarg:TArgDecl=odecl.argDecls[i]
			MungDecl arg, True
			If args args:+","
			If Not TFunctionPtrType(oarg.ty) Then
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )+" "+arg.munged
				Else
					args:+ oarg.castTo + " " + arg.munged
				End If
			Else
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )
				Else
					args:+ oarg.castTo
				End If
			End If
			If arg.ty.EqualsType( oarg.ty ) Continue
		Next
		
		Emit t + Bra(args) + ";"
	End Method
	
	Method EmitClassDeclObjectNewProto(classDecl:TClassDecl, fdecl:TFuncDecl)

		Local t:String = TransObject(classdecl) + " "
		
		If Not classDecl.IsStruct() Then
			t :+ "_"
		End If
		
		If classDecl = fdecl.scope Then
			t :+ fdecl.munged
		Else
			t :+ classDecl.munged + "_" + fdecl.ident + MangleMethod(fdecl)
		End If
		
		t:+ "_ObjectNew"
			
		'Find decl we override
		Local odecl:TFuncDecl=fdecl
		While odecl.overrides
			odecl=odecl.overrides
		Wend

		Local args:String
		If Not classDecl.IsStruct() Then
			args = "BBClass * clas"
		End If

		For Local i:Int=0 Until fdecl.argDecls.Length
			Local arg:TArgDecl=fdecl.argDecls[i]
			Local oarg:TArgDecl=odecl.argDecls[i]
			MungDecl arg, True
			If args args:+","
			If Not TFunctionPtrType(oarg.ty) Then
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )+" "+arg.munged
				Else
					args:+ oarg.castTo + " " + arg.munged
				End If
			Else
				If Not odecl.castTo Then
					args:+TransType( oarg.ty, arg.munged )
				Else
					args:+ oarg.castTo
				End If
			End If
			If arg.ty.EqualsType( oarg.ty ) Continue
		Next
		
		Emit t + Bra(args) + ";"

	End Method

	Method EmitClassDeclDelete( classDecl:TClassDecl )
		Local classid$=classDecl.munged
		Local superid$=classDecl.superClass.actual.munged

		' New
'		If opt_issuperstrict Then
			Emit "void _" + classid + "_Delete" + Bra(TransObject(classdecl) + " o") + " {"
'		Else
'			Emit "int _" + classid + "_Delete" + Bra(TransObject(classdecl) + " o") + " {"
'		End If

		Local decl:TFuncDecl = classDecl.FindFuncDecl("delete",,,,,,SCOPE_CLASS_HEIRARCHY)
		If decl Then
			decl.Semant
			EmitLocalDeclarations(decl)
			EmitBlock decl
		End If


		' field cleanup
		For Local decl:TFieldDecl=EachIn classDecl.Decls()


			' String
			If TStringType(decl.declTy) Then
				Emit "BBRELEASE(" + TransFieldRef(decl, "o") + ")"
			End If

			' object
			' TODO

		Next

		' finally, call super delete
		EmitClassDeclDeleteDtor(classDecl)

		'
		Emit "}"
	End Method
	
	Method EmitClassDeclDeleteDtor( classDecl:TClassDecl )
		Local superid$=classDecl.superClass.actual.munged
		
		If classDecl.superClass.ident = "Object" Or Not classHierarchyHasFunction(classDecl.superClass, "Delete") Then
			Emit "bbObjectDtor((BBOBJECT)o);"
		Else
			Emit "_" + superid + "_Delete((" + TransObject(TScopeDecl(classDecl.superClass.actual)) + ")o);"
		End If
	End Method

	Method TransFieldRef:String(decl:TFieldDecl, variable:String, exprType:TType = Null)
		Local s:String = variable
		
		Local ind:String = "->"
		If decl.scope And TClassDecl(decl.scope) And TClassDecl(decl.scope).IsStruct() Then
			Local exprIsStruct:Int = Not exprType Or (TObjectType(exprType) And TObjectType(exprType).classDecl.attrs & CLASS_STRUCT)
			If (exprIsStruct Or (exprType And Not IsPointerType(exprType))) And variable <> "o" Then
				ind = "."
			End If
		End If

		If variable.StartsWith("*") Then
			variable = Bra(variable)
		End If
		
		' Null test
		If opt_debug
			If TClassDecl(decl.scope) And TClassDecl(decl.scope).IsStruct() Then
				'
			Else
				variable = TransDebugNullObjectError(variable, TClassDecl(decl.scope))
			End If
		End If

		' array.length
		If decl.scope And decl.scope.ident = "___Array" Then
			If decl.ident = "length" Then
				Return Bra(variable + "->scales[0]")
			End If
			If decl.ident = "numberOfDimensions" Then
				Return Bra(variable + "->dims")
			End If
			If decl.ident = "sizeMinusHeader" Then
				Return Bra(variable + "->size")
			End If
			If decl.ident = "elementTypeEncoding" Then
				Return Bra(variable + "->type")
			End If
		End If

		' string methods
		If decl.scope And decl.scope.ident = "String" Then
			If decl.ident = "length" Then
				'If exprType._flags & TType.T_VAR Then
				'	Return Bra("(*" + variable + ")->length")
				'Else
					If variable.StartsWith("&_s") Then
						Return Bra(variable[1..] + ".length")
					Else
						Return Bra(variable + "->length")
					End If
				'End If
			End If
		End If

		'If TObjectType(exprType) And (exprType._flags & TType.T_VAR) Then
		'	' get the object from the pointer
		'	variable = Bra("*" + variable)
		'End If

		If IsNumericType(decl.ty) Then
			s = variable + ind + decl.munged + " "
		Else If TStringType(decl.ty) Then
			s = variable + ind + decl.munged + " "
		Else If TObjectType(decl.ty) Then
			s = variable + ind + decl.munged + " "
		Else If IsPointerType(decl.ty, 0, TType.T_POINTER) Then
			s = variable + ind + decl.munged + " "
		Else If TFunctionPtrType(decl.ty) Then
			s = variable + ind + decl.munged + " "
		Else If TArrayType(decl.ty) Then
			s = variable + ind + decl.munged + " "
		Else If TEnumType(decl.ty) Then
			s = variable + ind + decl.munged + " "
		End If

		Return s
	End Method

	' " _" + classDecl.actual.munged + "_" + decl.ident.ToLower(

	Method TransIfcArgs:String(funcDecl:TFuncDecl)
		Local args:String

		If Not funcDecl.IsSemanted() Then
			funcDecl.Semant()
		End If

		For Local i:Int=0 Until funcDecl.argDecls.Length
			Local arg:TArgDecl = funcDecl.argDecls[i]

			If args args:+","
			args:+ arg.ident + TransIfcType( arg.ty )

			If arg.init Then
				If TInvokeExpr(arg.init) Then
					args:+ "=" + Enquote(TInvokeExpr(arg.init).decl.munged)
				Else
					args:+ "=" + TransIfcConstExpr(arg.init)
				End If
			End If
		Next

		Return Bra(args)
	End Method

	Method EmitIfcClassFuncDecl(funcDecl:TFuncDecl)

		funcDecl.Semant

		Local func:String

		' method / function
		If funcDecl.IsMethod() Or funcDecl.IsCTor() Then
			func :+ "-"
		Else
			func :+ "+"
		End If

		If funcDecl.attrs & FUNC_OPERATOR Then
			func :+ BmxEnquote(funcDecl.ident)
		Else
			func :+ funcDecl.ident
		End If

		If Not TNewDecl(funcDecl) Then
			func :+ TransIfcType(funcDecl.retType, funcDecl.ModuleScope().IsSuperStrict())
		End If

		' function args
		func :+ TransIfcArgs(funcDecl)

		If funcDecl.attrs & DECL_FINAL Then
			func :+ "F"
		Else If funcDecl.attrs & DECL_ABSTRACT Then
			func :+ "A"
		End If
		
		If funcDecl.attrs & FUNC_OPERATOR Then
			func :+ "O"
		End If
		
		If funcDecl.attrs & DECL_PRIVATE Then
			func :+ "P"
		Else If funcDecl.attrs & DECL_PROTECTED Then
			func :+ "R"
		End If
		
		If funcDecl.attrs & DECL_API_STDCALL Then
			func :+ "W"
		End If
		
		If funcDecl.attrs & DECL_EXPORT Then
			func :+ "E"
		End If

		func :+ "="

		func :+ Enquote(funcDecl.munged)

		Emit func

	End Method

	Method EmitIfcFuncDecl(funcDecl:TFuncDecl)

		Local func:String

		func :+ funcDecl.ident

		' ensure the function has been semanted
		funcDecl.Semant()

		func :+ TransIfcType(funcDecl.retType, funcDecl.ModuleScope().IsSuperStrict())

		' function args
		func :+ TransIfcArgs(funcDecl)

		If funcDecl.attrs & DECL_API_STDCALL Then
			func :+ "W"
		End If

		func :+ "="

		func :+ Enquote(funcDecl.munged)

		If funcDecl.castTo Then
			func :+ ":" + funcDecl.castTo
			func :+ " " + funcDecl.munged + "("

			For Local i:Int = 0 Until funcDecl.argDecls.length
				If i Then
					func :+ ", "
				End If

				func :+ funcDecl.argDecls[i].castTo
			Next

			func :+ ")"
		End If

		Emit func

	End Method

	Method TransIfcConstExpr:String(expr:TExpr)

		If Not expr.exprType Then
			expr.Semant()
		End If

		If TStringType(expr.exprType) Then
			Return "$" + EscapeChars(BmxEnquote(expr.Eval()))
		EndIf

		If TArrayType(expr.exprType) Then
			Return Enquote("bbEmptyArray")
		End If

		If TFunctionPtrType(expr.exprType) Then
			If TCastExpr(expr) Then
				If TInvokeExpr(TCastExpr(expr).expr) Then
					Return Enquote(TInvokeExpr(TCastExpr(expr).expr).decl.munged)
				End If
				If TNullExpr(TCastExpr(expr).expr) Then
					Return Enquote("brl_blitz_NullFunctionError")
				End If
			End If

			InternalErr "TCTranslator.TransIfcConstExpr"
		End If

		If TObjectType(expr.exprType) Then
			If TCastExpr(expr) Then
				If TNullExpr(TCastExpr(expr).expr) Then
					Return Enquote("bbNullObject")
				End If
			End If
		End If
		
		If IsPointerType(expr.exprType, 0, TType.T_POINTER) Then
			If TCastExpr(expr) Then
				If TNullExpr(TCastExpr(expr).expr) Then
					Return "0"
				End If
				If TConstExpr(TCastExpr(expr).expr) Then
					Return TConstExpr(TCastExpr(expr).expr).value
				End If
			End If
		End If

		If IsNumericType(expr.exprType) Then
			Local s:String = expr.Eval()
			If Not s Then
				Return "0"
			Else
				If TDecimalType(expr.exprType) Then
					If s.StartsWith("1.#INF0000") Or s = "1e1000" Then
						s = "inf"
					Else If s.StartsWith("-1.#INF0000") Then
						s = "-inf"
					Else If s.StartsWith("-1.#IND0000") Then
						s = "nan"
					End If

					Return s + TransIfcType(expr.exprType)
				Else
					Return s
				End If
			End If
		EndIf

		'If TObjectType(expr.exprType) And TNullDecl(TObjectType(expr.exprType).classDecl) Then
		'	Return Enquote("bbNullObject")
		'End If

	End Method

	Method EmitIfcConstDecl(constDecl:TConstDecl)
		Local c:String
		c = constDecl.ident + TransIfcType(constDecl.ty)

		If TExpr(constDecl.init) Then
			c:+ "=" + TransIfcConstExpr(TExpr(constDecl.init))
		End If

		Emit c
	End Method

	Method EmitIfcFieldDecl(fieldDecl:TFieldDecl)
		Local f:String
		If fieldDecl.IsReadOnly() Then
			f :+ "@"
		Else
			f :+ "."
		End If
		f :+ fieldDecl.ident + TransIfcType(fieldDecl.ty, fieldDecl.ModuleScope().IsSuperStrict())

		f :+ "&"
		
		If fieldDecl.IsPrivate() Then
			f :+ "`"
		Else If fieldDecl.IsProtected() Then
			f :+ "``"
		End If

		Emit f
	End Method

	Method EmitIfcClassDecl(classDecl:TClassDecl)
	
		Local head:String = classDecl.ident + "^"
		If classDecl.superClass Then
			head :+ classDecl.superClass.ident
		Else
			head :+ "Null"
		End If
		
		If classDecl.implments Then
			head :+ "@"
			For Local i:Int = 0 Until classDecl.implments.length
				If i Then
					head :+ ","
				End If
				head :+ classDecl.implments[i].ident
			Next
		End If
		
		Emit head + "{", False

		'PushMungScope
		BeginLocalScope

		If Not classDecl.templateSource Then
	
			' const
			For Local cDecl:TConstDecl = EachIn classDecl.Decls()
				cDecl.Semant()
				
				EmitIfcConstDecl(cDecl)
			Next
	
				' global
			For Local gDecl:TGlobalDecl = EachIn classDecl.Decls()
				gDecl.Semant()
	
				EmitIfcGlobalDecl(gDecl)
			Next
	
	
				' field
			For Local fDecl:TFieldDecl = EachIn classDecl.Decls()
				fDecl.Semant()
	
				EmitIfcFieldDecl(fDecl)
			Next

		End If

		' functions
		If Not classDecl.IsExtern() Then
		
			If Not classDecl.templateSource Then

				If Not classDecl.attrs & CLASS_INTERFACE Then
					Emit "-New()=" + Enquote("_" + classDecl.munged + "_New")
				End If

				If classHierarchyHasFunction(classDecl, "Delete") Then
					Emit "-Delete()=" + Enquote("_" + classDecl.munged + "_Delete")
				End If
	
				For Local decl:TDecl=EachIn classDecl.Decls()
	
					Local fdecl:TFuncDecl=TFuncDecl( decl )
					If fdecl
						If Not equalsIfcBuiltInFunc(classDecl, fdecl) Then
							EmitIfcClassFuncDecl fdecl
						End If
						Continue
					EndIf
	
				Next
			
			End If
			
			Local flags:String

			If classDecl.IsAbstract() Then
				flags :+ "A"
			End If
			
			If classDecl.attrs & DECL_FINAL Then
				flags :+ "F"
			End If

			If classDecl.attrs & CLASS_INTERFACE Then
				flags :+ "I"
			Else If classDecl.IsStruct() Then
				flags :+ "S"
			End If
			
			If classDecl.templateSource Then
				flags :+ "G"
			End If
			
			Local t:String = "}" + flags + "="
			
			
			
			If classDecl.templateSource Then
				t :+ Enquote(classDecl.scope.munged)
			
				t :+ ",<"

				Local s:String
				
				If classDecl.instArgs Then
					For Local ty:TType = EachIn classDecl.instArgs
						If s Then
							s :+ ","
						End If
						s :+ ty.ToString()
					Next
				Else
					s = "?"
				End If
				
				t :+ s + ">" + classDecl.templateSource.ToString()
			Else
				t :+ Enquote(classDecl.munged)	
			End If
			
			Emit t, False
		Else
			For Local decl:TDecl=EachIn classDecl.Decls()

				Local fdecl:TFuncDecl=TFuncDecl( decl )
				If fdecl
					EmitIfcClassFuncDecl fdecl
					Continue
				EndIf

			Next
			
			Local flags:String = "E"
			
			If classDecl.IsInterface() Then
				flags :+ "I"
			Else If classDecl.IsStruct() Then
				flags :+ "S"
			End If
			
			If classDecl.attrs & DECL_API_STDCALL Then
				flags :+ "W"
			End If

			Emit "}" + flags + "=0", False
		End If

		'PopMungScope
		EndLocalScope

	End Method

	Method EmitIfcGlobalDecl(globalDecl:TGlobalDecl)
		globalDecl.Semant

		Local g:String = globalDecl.ident

		g:+ TransIfcType(globalDecl.ty, globalDecl.ModuleScope().IsSuperStrict())
		
		g:+ "&"

		If globalDecl.IsPrivate() Then
			g :+ "`"
		Else If globalDecl.IsProtected() Then
			g :+ "``"
		End If

		g :+ "="

		g :+ "mem:p("
		If TFunctionPtrType(globalDecl.ty) Then
			g :+ Enquote(TFunctionPtrType(globalDecl.ty).func.munged)
		Else
			g :+ Enquote(globalDecl.munged)
		End If
		g :+ ")"

		Emit g
	End Method

	Method EmitIfcEnumDecl(enumdecl:TEnumDecl)
		enumDecl.Semant
		
		Local e:String = enumDecl.ident + "/" + TransIfcType(enumDecl.ty)

		Emit e + "{", False
		
		For Local val:TEnumValueDecl = EachIn enumDecl.values
			Emit val.ident + "=" + val.Value()
		Next
		
		Local flags:String
		If enumDecl.isFlags Then
			flags = "F"
		End If
		
		Emit "}" + flags + "=" + Enquote(enumDecl.munged), False

	End Method
	
	Method EmitModuleInclude(moduleDecl:TModuleDecl, included:TMap = Null)
		If moduleDecl.filepath Then
			' a module import
			If FileType(moduleDecl.filepath) = FILETYPE_DIR Or (opt_ismain And moduleDecl.ident = opt_modulename) Then

				Local inc:String = ModuleHeaderFromIdent(moduleDecl.ident, True)

				If Not included Or (included And Not included.Contains(inc)) Then
					Emit "#include <" + inc + ">"
					If included Then
						included.Insert(inc, inc)
					End If
				End If
			Else
				' a file import...
				Local inc:String = FileHeaderFromFile(moduleDecl, False)

				If Not included Or (included And Not included.Contains(inc)) Then
					Emit "#include ~q" + inc + "~q"
					If included Then
						included.Insert(inc, inc)
					End If
				End If
			End If
'			DebugLog moduleDecl.filepath
		End If
	End Method

	Method EmitModuleInit(moduleDecl:TModuleDecl)
		If moduleDecl.filepath Then
			' a module import
			If FileType(moduleDecl.filepath) = FILETYPE_DIR Then
				Emit MungModuleName(moduleDecl) + "();"
			Else
				' maybe a file import...
				Emit MungImportFromFile(moduleDecl) + "();"
			End If
		End If
	End Method

	Method EmitIncBinFile(ib:TIncbin)

		If FileType(ib.path) = FILETYPE_FILE Then

			Local ident:String = _appInstance.munged + "_ib_" + ib.id

			Local buf:Byte[] = LoadByteArray(ib.path)
			ib.length = buf.length

			Emit "unsigned char " + ident + "[] = {"
			Local sb:TStringBuffer = New TStringBuffer

			Local hx:Short[2]
			Local lines:Int
			Local count:Int
			For Local i:Int = 0 Until buf.length
				Local val:Int = buf[i]

				For Local k:Int=1 To 0 Step -1
					Local n:Int=(val&15)+48
					If n>57 n:+39
					hx[k]=n
					val:Shr 4
				Next
				sb.Append("0x").AppendShorts( hx,2 )

				sb.Append(",")
				
				count :+ 5

				If count > 80 Then
					sb.Append("~n")
					count = 0
					lines :+ 1
				End If
				
				If lines > 100 Then
					Emit sb.ToString()
					sb.SetLength(0)
					lines = 0
				End If
				
			Next

			Emit sb.ToString()
			Emit "};"

		End If

	End Method

	Method TransHeader(app:TAppDecl)

		SetOutput("head")

		_app = app

		prefix = app.GetPathPrefix()

		' TODO

		If Not opt_apptype Then
			app.mainFunc.munged="bb_localmain"
		Else
			app.mainFunc.munged="bb_main"
		End If

		' track what's been included so far - avoid duplicates
		Local included:TMap = New TMap

		For Local decl:TModuleDecl=EachIn app.imported.Values()
			For Local mdecl:TDecl=EachIn decl.imported.Values()

				MungDecl mdecl

				'skip mdecls we are not interested in
				If Not TModuleDecl(mdecl) Then Continue
				If app.mainModule = mdecl Then Continue
				If mdecl.ident = "brl.classes" Then Continue
				If mdecl.ident = "brl.blitzkeywords" Then Continue

				EmitModuleInclude(TModuleDecl(mdecl), included)
			Next
		Next
		
		For Local header:String=EachIn app.headers
			Emit "#include ~q../" + header + "~q"
		Next

		Emit "int " + app.munged + "();"
		For Local decl:TDecl=EachIn app.Semanted()

			If decl.declImported And decl.munged Continue

			MungDecl decl

			Local cdecl:TClassDecl=TClassDecl( decl )
			If Not cdecl Continue

' mung, but don't emit
'			Emit prefix + decl.munged+";"

			'PushMungScope
			funcMungs = New TMap
			BeginLocalScope

			For Local decl:TDecl=EachIn cdecl.Semanted()
				MungDecl decl
				
				cdecl.SemantParts()
			Next

			EndLocalScope
			'PopMungScope
		Next

		' forward declarations
		For Local decl:TClassDecl=EachIn app.Semanted()
			If decl.declImported Or (decl.IsExtern() And Not decl.IsStruct()) Continue
			If Not decl.IsStruct()
				Emit "struct " + decl.munged + "_obj;"
			Else
				Emit "struct " + decl.munged + ";"
			End If
			If decl.IsInterface() Then
				Emit "extern const struct BBInterface " + decl.munged + "_ifc;"
			End If
		Next

		'prototypes/header! - structs first
		For Local decl:TDecl=EachIn app.Semanted()

			If decl.declImported Continue

			Local cdecl:TClassDecl=TClassDecl( decl )
			If cdecl
				If cdecl.IsStruct() Then
					EmitStructClassProto cdecl
				End If
			EndIf
		Next

		'prototypes/header!
		For Local decl:TDecl=EachIn app.Semanted()

			If decl.declImported Continue

			Local gdecl:TGlobalDecl=TGlobalDecl( decl )
			If gdecl
				MungDecl gdecl
				
				If Not TFunctionPtrType(gdecl.ty) Then
If Not gdecl.IsPrivate() Then
					Emit "extern "+TransRefType( gdecl.ty, "" )+" "+gdecl.munged+";"	'forward reference...
End If
				Else
					If Not TFunctionPtrType(gdecl.ty).func.noCastGen Then
						' generate function pointer refs if we haven't been told not to
'						If Not gdecl.IsExtern() Then
							Emit TransRefType( gdecl.ty, gdecl.munged )+";"	'forward reference...
'						End If
					End If
				End If
				Continue
			EndIf

			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl' And Not fdecl.IsExtern()
				' don't include the main function - it's handled separately
				If fdecl = app.mainFunc Then
					Continue
				End If

				EmitGDBDebug(fdecl)
				EmitFuncDecl( fdecl, True)
				Continue
			EndIf

			Local cdecl:TClassDecl=TClassDecl( decl )
			If cdecl
				If Not cdecl.IsStruct() Then
					If Not cdecl.IsExtern()
						EmitClassProto cdecl
					Else
						EmitExternClassProto cdecl
					End If
				'Else
				'	EmitStructClassProto cdecl
				End If
				'Continue
			EndIf

			Local edecl:TEnumDecl = TEnumDecl( decl )
			If edecl Then
				EmitEnumProto edecl
				Continue
			End If
		Next

	End Method

	Method IncBinRequiresRebuild:Int(file:String, incbins:TList)

		' file doesn't exist?
		If Not FileType(file) Then
			Return True
		End If

		Local timestamp:Int = FileTime(file)

		' file exists... read header and compare names
		' read lines until "// ----"
		' TODO
		Local files:TList = New TList
		Local stream:TStream = ReadFile(file)
		While True
			Local s:String = ReadLine(stream)
			If Not s.StartsWith("// ") Or s.StartsWith("// ----") Then
				Exit
			End If

			Local ind:Int = s.Find("// FILE : ")
			If ind = 0 Then
				files.AddLast(s[10..].Replace("~q",""))
			End If
		Wend
		stream.Close()

		' different number of files?
		If files.Count() <> incbins.Count() Then
			Return True
		End If

		' different file names?
		Local count:Int
		For Local s:String = EachIn files
			For Local ib:TIncbin = EachIn incbins
				If s = ib.file Then
					count :+ 1
					Exit
				End If
			Next
		Next

		If count <> files.count() Then
			Return True
		End If

		count = 0
		For Local ib:TIncbin = EachIn incbins
			For Local s:String = EachIn files
				If s = ib.file Then
					count :+ 1
					Exit
				End If
			Next
		Next

		If count <> incbins.count() Then
			Return True
		End If

		For Local ib:TIncbin = EachIn incbins
			If timestamp < FileTime(ib.path) Then
				Return True
			End If

			' set the length, as we will need this later if we aren't loading the files now.
			ib.length = FileSize(ib.path)
		Next

		Return False
	End Method

	Method TransIncBin(app:TAppDecl)
		If app.incbins.Count() > 0 Then

			SetOutput("incbin")

			Local mung:String = FileMung(False)

			Local name:String = StripAll(app.mainModule.filepath)
			Local file:String = name + ".bmx" + mung + ".incbin.c"
			Local filepath:String = OutputFilePath(opt_filepath, mung, "incbin.c")

			If IncBinRequiresRebuild(filepath, app.incbins) Then

				app.genIncBinHeader = True

				For Local ib:TIncbin = EachIn app.incbins
					Emit "// FILE : " + Enquote(ib.file)
				Next

				Emit "// ----"

				For Local ib:TIncbin = EachIn app.incbins
					EmitIncBinFile(ib)
				Next

			End If

			SetOutput("pre_source")
		End If
	End Method

	Method TransGlobalInit(decl:TGlobalDecl)
		If TFunctionPtrType(decl.ty) Then
			If TInvokeExpr(decl.init) And Not TInvokeExpr(decl.init).invokedWithBraces Then
				Emit TransGlobal( decl )+"="+TInvokeExpr(decl.init).decl.munged + ";"
			Else
				Emit TransGlobal( decl )+"="+decl.init.Trans()+";"
			End If
		Else
			If Not decl.funcGlobal Then
				If TObjectType(decl.ty) Then
					Emit TransGlobal( decl )+"="+Bra(TransObject(TObjectType(decl.ty).classDecl))+decl.init.Trans()+";"
				Else
					Emit TransGlobal( decl )+"="+decl.init.Trans()+";"
				End If
			End If
		End If
	End Method

	Method TransSource(app:TAppDecl)

		SetOutput("pre_source")

		' include our header
		EmitModuleInclude(app.mainModule)

		' incbins
		TransIncBin(app)

		SetOutput("source")


		' nested type forward declarations
		For Local decl:TClassDecl=EachIn app.Semanted()
			For Local cdecl:TClassDecl = EachIn decl._decls
				MungDecl decl
				MungDecl cdecl
				If cdecl.declImported Or (cdecl.IsExtern() And Not cdecl.IsStruct()) Continue
				If Not cdecl.IsStruct()
					Emit "struct " + cdecl.munged + "_obj;"
				Else
					Emit "struct " + cdecl.munged + ";"
				End If
				If cdecl.IsInterface() Then
					Emit "extern const struct BBInterface " + cdecl.munged + "_ifc;"
				End If
			Next
		Next

		' Private Global declarations
		' since we don't declare them in the header, they need to be near the top of the source
		For Local decl:TDecl=EachIn app.Semanted()

			If decl.declImported Continue

			Local gdecl:TGlobalDecl=TGlobalDecl( decl )
			If gdecl And gdecl.IsPrivate() Then

				If Not TFunctionPtrType(gdecl.ty) Then
					If TConstExpr(gdecl.init) Then
						Emit TransRefType( gdecl.ty, "WW" )+" "+TransGlobalDecl(gdecl)+";"
						gdecl.inited = True
					Else
If Not gdecl.IsExtern() Then
						Emit TransRefType( gdecl.ty, "WW" )+" "+gdecl.munged+";"
Else
					' delcare in source for any references to it locally in this module
					Emit "extern "+TransRefType( gdecl.ty, "WW" )+" "+gdecl.munged+";"
End If
					End If
				Else
					'Emit TransRefType( gdecl.ty, gdecl.munged ) + ";"
				End If
				Continue
			EndIf
			
		Next

		For Local gdecl:TGlobalDecl=EachIn app.SemantedGlobals
			If gdecl And gdecl.funcGlobal Then
				MungDecl gdecl
				
				If Not TFunctionPtrType(gdecl.ty) Then
					Emit "static " + TransRefType( gdecl.ty, "WW" )+" "+gdecl.munged+";"
				Else
					Emit "static " + TransRefType( gdecl.ty, gdecl.munged ) + ";"
				End If
				Continue
			End If
		Next


		'definitions!
		For Local decl:TDecl=EachIn app.Semanted()

			If decl.declImported Continue

			Local gdecl:TGlobalDecl=TGlobalDecl( decl )
			If gdecl
				If Not TFunctionPtrType(gdecl.ty) And Not gdecl.IsPrivate() Then
					If TConstExpr(gdecl.init) Then
						Emit TransRefType( gdecl.ty, "WW" )+" "+TransGlobalDecl(gdecl)+";"
						gdecl.inited = True
					Else
If Not gdecl.IsExtern() Then
						If TObjectType(gdecl.ty) Then
							Emit TransRefType( gdecl.ty, "WW" )+" "+gdecl.munged+ "=" + Bra(TransObject(TObjectType(gdecl.ty).classDecl)) + TransValue(gdecl.ty, "") + ";"
						Else
							Emit TransRefType( gdecl.ty, "WW" )+" "+gdecl.munged+ "=" + TransValue(gdecl.ty, "") + ";"
						End If
End If
					End If
				Else
					'Emit TransRefType( gdecl.ty, gdecl.munged ) + ";"
				End If
				Continue
			EndIf

			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl And Not fdecl.IsExtern()

				' don't include the main function - it's handled separately
				If fdecl = app.mainFunc Then
					Continue
				End If

				EmitGDBDebug(fdecl)
				EmitFuncDecl fdecl
				Continue
			EndIf

			Local cdecl:TClassDecl=TClassDecl( decl )
			If cdecl
				EmitGDBDebug(cdecl)
				EmitClassDecl cdecl
				Continue
			EndIf
			
			Local edecl:TEnumDecl = TEnumDecl( decl )
			If edecl Then
				EmitEnumDecl edecl
				Continue
			End If
		Next

		' emit nested functions/classes for localmain
		' emit nested protos
		For Local fdecl:TFuncDecl = EachIn app.mainFunc._decls
			EmitFuncDecl(fdecl, True)
		Next
		
		' emit nested bodies
		For Local fdecl:TFuncDecl = EachIn app.mainFunc._decls
			EmitFuncDecl(fdecl, False)
		Next

		' incbin decls
		For Local ib:TIncbin = EachIn app.incbins
			Emit "extern unsigned char * " + app.munged + "_ib_" + ib.id + ";"
		Next

		Emit "static int " + app.munged + "_inited" + " = 0;"

		Emit "int " + app.munged + "(){"

		' initialise stuff
		Emit "if (!" + app.munged + "_inited) {"
		Emit app.munged + "_inited = 1;"

		' add global roots
		Local first:TGlobalDecl
		Local last:TGlobalDecl
				
		For Local decl:TGlobalDecl=EachIn app.semantedGlobals

			If decl.declImported Continue

			decl.Semant

			If Not first Then
				first = decl
			End If
			
			last = decl
		Next

		If first Then
			Emit "GC_add_roots(&" + first.munged + ", &" + last.munged + " + 1);"
		End If

		' register incbins
		For Local ib:TIncbin = EachIn app.incbins
			Emit "bbIncbinAdd(&" + TStringConst(app.stringConsts.ValueForKey(ib.file)).id + ",&" + app.munged + "_ib_" + ib.id + "," + ib.length + ");"
		Next
		
		Local importOnce:TMap = New TMap
		
		' call any imported mod inits
		For Local decl:TModuleDecl=EachIn app.imported.Values()
			For Local mdecl:TDecl=EachIn decl.imported.Values()
				If TModuleDecl(mdecl) And app.mainModule <> mdecl And mdecl.ident <> "brl.classes" And mdecl.ident <> "brl.blitzkeywords" Then
					If Not importOnce.Contains(mdecl.ident) Then
						EmitModuleInit(TModuleDecl(mdecl))
						importOnce.Insert(mdecl.ident, "")
					End If
				End If
			Next
		Next

		' initialise enums
		For Local decl:TEnumDecl = EachIn app.Semanted()
			Emit decl.munged + "_BBEnum_impl = &" + decl.munged + "_BBEnum;"
		Next

		' register types
		For Local decl:TDecl=EachIn app.Semanted()

			If decl.declImported Continue

			Local cdecl:TClassDecl=TClassDecl( decl )
			If cdecl And Not cdecl.IsExtern() And Not cdecl.args
				If Not cdecl.IsInterface() Then
					If Not cdecl.IsStruct() Then
						Emit "bbObjectRegisterType((BBCLASS)&" + cdecl.munged + ");"
					Else
						Emit "bbObjectRegisterStruct(&" + cdecl.munged + "_scope);"
					End If
				Else
					Emit "bbObjectRegisterInterface(&" + cdecl.munged + "_ifc);"
				End If
				Continue
			EndIf
			Local edecl:TEnumDecl = TEnumDecl( decl )
			If edecl Then
				Emit "bbEnumRegister(" + decl.munged + "_BBEnum_impl, &" + edecl.munged + "_scope);"
			End If
		Next
		'

		' defdata init
		If Not app.dataDefs.IsEmpty() Then
			Emit "_defDataOffset = &_defData;"
		End If

		' initialise globals
		For Local decl:TGlobalDecl=EachIn app.semantedGlobals

			If decl.declImported Continue

			decl.Semant

			' TODO : what about OnDebugStop etc, who have no init ?
			If decl.init And Not (decl.attrs & DECL_INITONLY) Then

				If decl.scope And TClassDecl(decl.scope) Then

					' class global inits need to be generated in the correct order.
					' only generate global inits if the parent class hasn't already been processed,
					' otherwise, we will skip this global as it should already have been generated.
					If Not TClassDecl(decl.scope).globInit Then
					
						TClassDecl(decl.scope).globInit = True
					
						For Local gdecl:TGlobalDecl = EachIn decl.scope._decls
						
							If gdecl.declImported Continue
							
							gdecl.Semant
							
							If gdecl.init And Not (gdecl.attrs & DECL_INITONLY) Then
								TransGlobalInit(gdecl)
							End If
						Next
					End If
					
				Else
					TransGlobalInit(decl)
				End If

			End If
		Next

		' now do the local main stuff
		app.mainFunc.Semant()
		EmitLocalDeclarations(app.mainFunc)
		EmitBlock app.mainFunc


		Emit "}"
		Emit "return 0;"
		Emit "}"

		' redirect string generation to the top of the source
		SetOutput("pre_source")

		' strings
		For Local s:String = EachIn app.stringConsts.Keys()
			If s Then
				Local key:TStringConst = TStringConst(app.stringConsts.ValueForKey(s))

				If key.count > 0 Then
					Emit "static BBString " + key.id + "={"
					Emit "&bbStringClass,"
					'Emit "2147483647,"
					Emit s.length + ","

					Local t:String = "{"

					For Local i:Int = 0 Until s.length
						If i Then
							t:+ ","
						End If
						t:+ s[i]

						If i And Not (i Mod 16) Then
							Emit t
							t = ""
						End If
					Next

					Emit t + "}"

					Emit "};"
				End If
			End If
		Next
		
		' defdata
		EmitDefDataArray(app)
		
		' scope defs
		If Not app.scopedefs.IsEmpty() Then
			For Local val:String = EachIn app.scopedefs.Keys()
				Local i:Int = val.ToInt()
				Emit "struct BBDebugScope_" + i + "{int kind; const char *name; BBDebugDecl decls[" + (i + 1) + "]; };"
			Next
		End If

	End Method
	
	Method EmitDefDataArray(app:TAppDecl)
		If Not app.dataDefs.IsEmpty() Then
			' 
			Emit "static struct bbDataDef * _defDataOffset;"
			Emit "static struct bbDataDef _defData[" + TDefDataDecl.count + "]={"
			
			For Local decl:TDefDataDecl = EachIn app.dataDefs
				EmitDefData(decl)
			Next
			
			Emit "};"
		End If
	End Method

	Method EmitDefData(decl:TDefDataDecl)
		For Local i:Int = 0 Until decl.data.length
			Local expr:TExpr = decl.data[i]
			
			Emit "{"
		
			Emit TransDefDataType(expr.exprType) + ","
			
			Emit "{"
			Emit "." + TransDefDataUnionType(expr.exprType) + " = " + expr.Trans()
			Emit "}"
		
			Emit "},"
		Next
	End Method

	Method EmitIfcImports(impMod:TModuleDecl, processed:TMap)

		For Local decl:TDecl=EachIn impMod.imported.Values()
			Local mdecl:TModuleDecl=TModuleDecl( decl )
			If mdecl And mdecl.ident <> "brl.classes" And mdecl.ident <> "brl.blitzkeywords" Then
				If mdecl.filepath.EndsWith(".bmx")
					If _appInstance.mainModule<>mdecl
						EmitIfcImports(mdecl, processed)

						For Local s:String = EachIn mdecl.fileImports
							If Not processed.Contains("XX" + s + "XX") Then
								Emit "import " + BmxEnquote(s)
								processed.Insert("XX" + s + "XX", "")
							End If
						Next
					End If
				Else
					If Not processed.Contains(mdecl.ident)
						Emit "import " + mdecl.ident
						processed.Insert(mdecl.ident, "")
					End If
				End If
			End If
		Next

	End Method

	Method EmitIfcStructImports(impMod:TModuleDecl, processed:TMap)
		For Local decl:TDecl=EachIn impMod.imported.Values()
			Local mdecl:TModuleDecl=TModuleDecl( decl )
			If mdecl Then
				If mdecl.filepath.EndsWith(".bmx") And _appInstance.mainModule<>mdecl And Not processed.Contains(mdecl)
					EmitIfcStructImports(mdecl, processed)

					processed.Insert(mdecl, mdecl)

					For Local decl:TDecl=EachIn mdecl._decls

						decl.Semant
						
						' consts
						Local cdecl:TConstDecl=TConstDecl( decl )
						If cdecl
							EmitIfcConstDecl(cdecl)
							Continue
						End If

						' classes
						Local tdecl:TClassDecl=TClassDecl( decl )
						If tdecl
							EmitIfcClassDecl(tdecl)
							Continue
						EndIf

						' functions
						Local fdecl:TFuncDecl=TFuncDecl( decl )
						If fdecl And fdecl <> _appInstance.mainFunc Then
							EmitIfcFuncDecl(fdecl)
							Continue
						End If

						' globals
						Local gdecl:TGlobalDecl=TGlobalDecl( decl )
						If gdecl
							EmitIfcGlobalDecl(gdecl)
							Continue
						End If
					Next

				End If
			End If
		Next

	End Method

	Method FileHeaderFromFile:String(mdecl:TModuleDecl, includePath:Int = False)

		Local name:String = StripAll(mdecl.filepath)
		Local dir:String = ExtractDir(mdecl.filePath)

		Local file:String = name + ".bmx" + FileMung(opt_apptype And (Not mdecl.declImported)) + ".h"

		If mdecl.relPath Then
			Local dir:String = ExtractDir(mdecl.relPath)
			If dir Then
				file = "../" + dir + "/.bmx/" + file
			End If
		End If

		Return file
	End Method

	Method MungImportFromFile:String(mdecl:TModuleDecl)

		Local result:String
		If opt_buildtype <> BUILDTYPE_MODULE Then
			Local dir:String = ExtractDir(mdecl.filepath).ToLower()
			dir = dir[dir.findLast("/") + 1..]
			If dir.EndsWith(".mod") Then
				dir = dir.Replace(".mod", "")
			End If
			Local file:String = StripDir(mdecl.filepath).ToLower()
			result = "_bb_" + dir + "_" + StripExt(file)
		Else
			result = "_bb_" + mdecl.ident
		End If

		'return with all non-allowed chars (like "-" or " ") removed
		Return TStringHelper.Sanitize(result)
	End Method

	Method TransInterface(app:TAppDecl)

		SetOutput("interface")

		If app.mainModule.IsSuperStrict() Then
			Emit "superstrict"
		End If

		' module info
		For Local info:String = EachIn app.mainModule.modInfo
			Emit "ModuleInfo " + BmxEnquote(info)
		Next

		Local processed:TMap = New TMap

		' module imports
		For Local decl:TDecl=EachIn app.mainModule.imported.Values()
			Local mdecl:TModuleDecl=TModuleDecl( decl )
			If mdecl Then
				If mdecl.IsActualModule() Then
					Emit "import " + mdecl.ident
					processed.Insert(mdecl.ident, "")
				Else If Not opt_ismain And mdecl.filepath.EndsWith(".bmx") And app.mainModule<>mdecl
					Local file:String = StripDir(mdecl.filepath)
					If mdecl.relPath Then
						Local dir:String = ExtractDir(mdecl.relPath)
						If dir Then
							file = dir + "/" + file
						End If
					End If
					If Not processed.Contains(file) Then
						Emit "import " + Enquote(file)
						processed.Insert(file, "")
					End If
				End If
			End If
		Next

		' module imports from other files?
		If opt_buildtype = BUILDTYPE_MODULE And opt_ismain Then
			EmitIfcImports(app.mainModule, processed)
		End If

		' other imports
		For Local s:String = EachIn app.fileImports
			Emit "import " + BmxEnquote(s)
		Next


		processed = New TMap
		' imported module structure (consts, classes, functions, etc)
		If opt_buildtype = BUILDTYPE_MODULE And opt_ismain Then
			EmitIfcStructImports(app.mainModule, processed)
		End If

		' consts
		For Local decl:TDecl=EachIn app.Semanted()
			If decl.IsPrivate() Continue

			Local cdecl:TConstDecl=TConstDecl( decl )
			If cdecl And Not cdecl.declImported
				EmitIfcConstDecl(cdecl)
			End If
		Next

		' classes
		For Local decl:TDecl=EachIn app.Semanted()
			If decl.IsPrivate() Continue

			Local cdecl:TClassDecl=TClassDecl( decl )
			If cdecl And Not cdecl.declImported
				EmitIfcClassDecl(cdecl)
			EndIf
		Next

		' functions
		For Local decl:TDecl=EachIn app.Semanted()
			If decl.IsPrivate() Continue

			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl And fdecl <> app.mainFunc  And Not fdecl.declImported Then
				EmitIfcFuncDecl(fdecl)
			End If
		Next

		' globals
		For Local decl:TDecl=EachIn app.Semanted()
			If decl.IsPrivate() Continue

			Local gdecl:TGlobalDecl=TGlobalDecl( decl )
			If gdecl And Not gdecl.declImported
				EmitIfcGlobalDecl(gdecl)
			End If
		Next
		
		' enums
		For Local decl:TDecl=EachIn app.Semanted()
			If decl.IsPrivate() Continue
			
			Local edecl:TEnumDecl=TEnumDecl( decl )
			If edecl And Not edecl.declImported
				EmitIfcEnumDecl(edecl)
			End If
		Next
	End Method
	
	Method TransDef(app:TAppDecl)

		SetOutput("def")

		Emit "LIBRARY " + StripExt(StripDir(opt_filepath))
		Emit "EXPORTS"
		
		For Local decl:TFuncDecl=EachIn app.exportDefs
			Emit "~t" + TransExportDef(decl, opt_arch = "x86")
		Next

		Emit "~n"
	End Method

	Method TransApp( app:TAppDecl )

		If app.mainModule.IsSuperStrict()
			opt_issuperstrict = True
		End If

		TransHeader(app)

		TransSource(app)

		TransInterface(app)

		If opt_makelib Then
			If opt_def And opt_apptype Then
				TransDef(app)
			End If
		End If
		
	End Method
	
End Type

