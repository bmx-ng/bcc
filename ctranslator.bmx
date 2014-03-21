SuperStrict

Import "parser.bmx"

Type TCTranslator Extends TTranslator

	Field _app:TAppDecl
	'Field stringConstCount:Int

	Field prefix:String

	Method New()
		_trans = Self
	End Method
	
	Method TransArrayType$( ty:TType)
		If TByteType( ty ) Return "~qb~q"
		If TShortType( ty ) Return "~qs~q"
		If TIntType( ty ) Return "~qi~q"
		If TFloatType( ty ) Return "~qf~q"
		If TDoubleType( ty ) Return "~qd~q"
		If TLongType( ty ) Return "~ql~q"
		If TStringType( ty ) Return "~q$~q"
		If TArrayType( ty ) Return "~q[~q"
		If TObjectType( ty ) Return "~q:~q"
	End Method

	Method TransType$( ty:TType, ident:String)
		If TVoidType( ty ) Or Not ty Then
'DebugStop
			Return "void"
		End If
		If TBoolType( ty ) Return "BBINT"
		If TByteType( ty ) Return "BBBYTE"
		If TShortType( ty ) Return "BBSHORT"
		If TIntType( ty ) Return "BBINT"
		If TFloatType( ty ) Return "BBFLOAT"
		If TDoubleType( ty ) Return "BBDOUBLE"
		If TLongType( ty ) Return "BBLONG"
		If TStringType( ty ) Return "BBSTRING"
		If TStringVarPtrType( ty ) Return "BBSTRING *"
		If TArrayType( ty ) Return "BBARRAY"
		If TObjectType( ty ) Return "BBOBJECT"
		If TBytePtrType( ty ) Return "BBBYTE *"
		If TShortPtrType( ty ) Return "BBSHORT *"
		If TIntPtrType( ty ) Return "BBINT *"
		If TFloatPtrType( ty ) Return "BBFLOAT *"
		If TDoublePtrType( ty ) Return "BBDOUBLE *"
		If TLongPtrType( ty ) Return "BBLONG *"
		If TFunctionPtrType( ty ) Then
'DebugStop
			TFunctionPtrType(ty).func.Semant
			
			Local retType:String = TransType(TFunctionPtrType(ty).func.retType, "")
			Local args:String
			For Local arg:TArgDecl = EachIn TFunctionPtrType(ty).func.argDecls
				arg.Semant()
				If args Then
					args :+ ","
				End If
				
				args :+ TransType(arg.ty, "")
			Next
			Return retType + Bra("* " + ident) + Bra(args)
		End If
		If TBytePtrPtrType( ty ) Return "BBBYTE **"
		If TShortPtrPtrType( ty ) Return "BBSHORT **"
		If TIntPtrPtrType( ty ) Return "BBINT **"
		If TFloatPtrPtrType( ty ) Return "BBFLOAT **"
		If TDoublePtrPtrType( ty ) Return "BBDOUBLE **"
		If TLongPtrPtrType( ty ) Return "BBLONG **"
		If TStringCharPtrType( ty ) Return "BBBYTE *"
		If TStringShortPtrType( ty ) Return "BBSHORT *"
		If TIntVarPtrType( ty ) Return "BBINT *"
		InternalErr
	End Method

	Method TransIfcType$( ty:TType )
		If TVoidType( ty ) Or Not ty Return "%"
		If TByteType( ty ) Return "@"
		If TShortType( ty ) Return "@@"
		If TIntType( ty ) Return "%"
		If TFloatType( ty ) Return "#"
		If TDoubleType( ty ) Return "!"
		If TLongType( ty ) Return "%%"
		If TStringType( ty ) Return "$"
		If TArrayType( ty ) Return TransIfcType(TArrayType( ty ).elemType) + "&[]"
		If TObjectType( ty ) Return ":" + TObjectType(ty).classDecl.ident
		If TBytePtrType( ty ) Return "@*"
		If TShortPtrType( ty ) Return "@@*"
		If TIntPtrType( ty ) Return "%*"
		If TFloatPtrType( ty ) Return "#*"
		If TDoublePtrType( ty ) Return "!*"
		If TLongPtrType( ty ) Return "%%*"
		If TFunctionPtrType( ty ) Return TransIfcType(TFunctionPtrType(ty).func.retType) + TransIfcArgs(TFunctionPtrType(ty).func)
		If TBytePtrPtrType( ty ) Return "@**"
		If TShortPtrPtrType( ty ) Return "@@**"
		If TIntPtrPtrType( ty ) Return "%**"
		If TFloatPtrPtrType( ty ) Return "#**"
		If TDoublePtrPtrType( ty ) Return "!**"
		If TLongPtrPtrType( ty ) Return "%%**"
		If TByteVarPtrType( ty ) Return "@ Var"
		If TShortVarPtrType( ty ) Return "@@ Var"
		If TIntVarPtrType( ty ) Return "% Var"
		If TFloatVarPtrType( ty ) Return "# Var"
		If TDoubleVarPtrType( ty ) Return "! Var"
		If TLongVarPtrType( ty ) Return "%% Var"
		If TStringVarPtrType( ty ) Return "$ Var"
		If TByteVarPtrPtrType( ty ) Return "@* Var"
		If TShortVarPtrPtrType( ty ) Return "@@* Var"
		If TIntVarPtrPtrType( ty ) Return "%* Var"
		If TFloatVarPtrPtrType( ty ) Return "#* Var"
		If TDoubleVarPtrPtrType( ty ) Return "!* Var"
		If TLongVarPtrPtrType( ty ) Return "%%* Var"
		If TStringCharPtrType( ty ) Return "$z"
		If TStringShortPtrType( ty ) Return "$w"
		InternalErr
	End Method

	Method TransRefType$( ty:TType, ident:String )
		If TObjectType( ty ) And ty.GetClass().IsInterface() Return "gc_iptr<"+ty.GetClass().actual.munged+">"
		Return TransType( ty, ident )
	End Method
	
	Method TransValue$( ty:TType,value$ )
		If value
			If TBoolType( ty ) Return "1"
			If TShortType( ty ) Return value
			If TIntType( ty ) Return value
			If TLongType( ty ) Return value+"LL"
			If TFloatType( ty ) Then
				If value.Find(".") < 0 Then
					value :+ ".0"
				End If
				Return value+"f"
			End If
			If TDoubleType( ty ) Then
				If value.Find(".") < 0 Then
					value :+ ".0"
				End If
				Return value+"f"
			End If
			If TStringType( ty ) Return "String("+Enquote( value )+")"
			If TByteType( ty ) Return value
		Else
			If TBoolType( ty ) Return "0"
			If TNumericType( ty ) Return "0"
			If TStringType( ty ) Return "&bbEmptyString"
			If TArrayType( ty ) Return "&bbEmptyArray"
			If TObjectType( ty ) Return "0"
			If TPointerType( ty) Return "0" ' todo ??
			If TByteType( ty ) Return "0"
		EndIf
		InternalErr
	End Method
	
	Method TransArgs$( args:TExpr[],decl:TFuncDecl, objParam:String = Null )
		Local t$
		If objParam Then
			t:+ objParam
		End If
		For Local i:Int=0 Until decl.argDecls.Length
			If t t:+","
			If i < args.length
				If TStringVarPtrType(TArgDecl(decl.argDecls[i].actual).ty) Then
					If TCastExpr(args[i]) And TStringType(TCastExpr(args[i]).expr.exprType) Then
						t:+ "&"
					End If
				Else If TFunctionPtrType(TArgDecl(decl.argDecls[i].actual).ty) Then
					If TInvokeExpr(args[i]) Then
						t:+ "&" + TInvokeExpr(args[i]).decl.munged
						Continue
					End If
				End If
				t:+TransTemplateCast( TArgDecl(decl.argDecls[i].actual).ty,args[i].exprType,args[i].Trans() )
			Else
				decl.argDecls[i].Semant()
				' default values
				If decl.argDecls[i].init Then
					t:+ decl.argDecls[i].init.Trans()
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
'DebugStop
		If TPointerType(ty) Then
			' TODO : pointer stuff
			Return expr
		End If

'		If TIntType(ty) And TStringType(src) Then
'DebugStop
'			Return "bbObjectDowncast" + Bra(expr + ",&" + TStringType(src).cDecl.munged)
'		End If

		If TStringType(ty) And TObjectType(src) Then
			If Not TStringType(ty).cDecl Then
				ty.Semant()
			End If

			Return "bbObjectDowncast" + Bra(expr + ",&" + TStringType(ty).cDecl.munged)
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

		If Not TObjectType(ty) Or Not TObjectType(src) InternalErr
		
		Local t$=TransType(ty, "TODO: TransPtrCast")
		
		If src.GetClass().IsInterface() Or ty.GetClass().IsInterface() cast="dynamic"
		
		If src.GetClass().IsInterface() And Not ty.GetClass().IsInterface() Then
			Return cast+"_cast<"+TransType(ty, "TODO: TransPtrCast")+">"+Bra( expr )
		End If
		
		'upcast?
		If src.GetClass().ExtendsClass( ty.GetClass() ) Return expr
'DebugStop
		If TObjectType(ty) Then
			Return "bbObjectDowncast" + Bra(expr + ",&" + TObjectType(ty).classDecl.munged)
		End If

		Return cast+"_cast<"+TransType(ty, "TODO: TransPtrCast")+">"+Bra( expr )
		
	End Method
	
	'***** Utility *****
	
	Method TransLocalDecl$( munged$,init:TExpr )
		Return TransType( init.exprType, munged )+" "+munged+"="+init.Trans()
	End Method

	Method TransGlobalDecl$( munged$,init:TExpr )
		Return "static " + TransType( init.exprType, munged )+" "+munged+"="+init.Trans()
	End Method

	Method CreateLocal2$( ty:TType, t$ )
		Local tmp:TLocalDecl=New TLocalDecl.Create( "", ty,Null )
		MungDecl tmp
		Emit TransType(ty, "") + " " + tmp.munged + " = bbStringToCString" + Bra(t)+ ";"
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
			Return decl.munged
		Else If TClassDecl( decl.scope )
			'Return decl.scope.munged+"::"+decl.munged
			Return decl.munged
		Else If TModuleDecl( decl.scope )
			Return decl.munged
		Else If TFuncDecl(decl.scope)
			Return decl.munged
		EndIf
		InternalErr
	End Method
	
	Method TransTemplateCast$( ty:TType,src:TType,expr$ )

		If ty=src Return expr

		ty=ty.ActualType()
		'src=src.ActualType()
		
		If ty.EqualsType( src ) Return expr
		
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
		
	Method TransFunc$( decl:TFuncDecl,args:TExpr[],lhs:TExpr )
If decl.ident = "AddHook" DebugStop
		If decl.IsMethod()
			If lhs And Not TSelfExpr(lhs) Then
				If lhs.exprType = TType.stringType Then
					Return decl.munged + TransArgs(args, decl, TransSubExpr( lhs ))
'If decl.ident = "Print" DebugStop
'DebugStop		
				Else If lhs.exprType = TType.stringVarPointerType Then
'DebugStop
					Return decl.munged + TransArgs(args, decl, "*" + TransSubExpr( lhs ))
				End If
'If decl.ident = "Eof" DebugStop
				
				If TVarExpr(lhs) Then
					Local cdecl:TClassDecl = TObjectType(TVarExpr(lhs).decl.ty).classDecl
 					Local obj:String = TransFuncObj(cdecl)
					Local class:String = Bra("(" + obj + TransSubExpr( lhs ) + ")->clas")
					Return class + "->" + TransFuncPrefix(cdecl) + decl.ident+TransArgs( args,decl, TransSubExpr( lhs ) )
				Else If TNewObjectExpr(lhs) Then
					Local cdecl:TClassDecl = TNewObjectExpr(lhs).classDecl
					Local class:String = cdecl.munged
					Return class + "." + TransFuncPrefix(cdecl) + decl.ident+TransArgs( args,decl, TransSubExpr( lhs ) )
				Else If TCastExpr(lhs) Then
					Local cdecl:TClassDecl = TObjectType(TCastExpr(lhs).ty).classDecl
					Local obj:String = TransFuncObj(cdecl)
					Local class:String = Bra("(" + obj + TransSubExpr( lhs ) + ")->clas")
					Return class + "->" + TransFuncPrefix(cdecl) + decl.ident+TransArgs( args,decl, TransSubExpr( lhs ) )
				Else If TMemberVarExpr(lhs) Then
					Local cdecl:TClassDecl = TObjectType(TMemberVarExpr(lhs).decl.ty).classDecl
					Local obj:String = TransFuncObj(cdecl)
					Local class:String = Bra("(" + obj + TransSubExpr( lhs ) + ")->clas")
					Return class + "->" + TransFuncPrefix(cdecl) + decl.ident+TransArgs( args,decl, TransSubExpr( lhs ) )
				Else
					InternalErr
				End If
				'Return TransSubExpr( lhs )+"->"+decl.munged+TransArgs( args,decl )
				'Return decl.munged+TransArgs( args,decl, TransSubExpr( lhs ) )
			End If
 
			' ((brl_standardio_TCStandardIO_obj*)o->clas)->md_Read(o, xxx, xxx)
			Local obj:String = Bra("struct " + decl.scope.munged + "_obj*")
			Local class:String = Bra("(" + obj + "o)->clas")
			Return class + "->md_" + decl.ident+TransArgs( args,decl, "o" )
		EndIf

		' built-in functions
		Select decl.ident.ToLower()
			Case "min", "max", "len", "asc", "chr"
				Return TransBuiltin(decl, args)
		End Select

		Return TransStatic( decl )+TransArgs( args,decl )
	End Method
	
	Method TransFuncObj:String(decl:TClassDecl)
		If decl.ident = "Object"
			Return Bra("BBOBJECT")
		Else
			Return Bra("struct " + decl.munged + "_obj*")
		End If
	End Method

	Method TransFuncPrefix:String(decl:TClassDecl)
		If decl.ident = "Object"
			Return ""
		Else
			Return "md_"
		End If
	End Method
	
	Method TransSuperFunc$( decl:TFuncDecl,args:TExpr[] )
		If decl.IsMethod()
			Return decl.ClassScope().munged+".md_"+decl.ident+TransArgs( args,decl, "o" )
		Else
			Return decl.ClassScope().munged+".fn_"+decl.ident+TransArgs( args,decl)
		End If
	End Method

	Method TransBuiltin$( decl:TFuncDecl,args:TExpr[] )
		Select decl.ident.ToLower()
			Case "min", "max"
				Local isFloat:Int
				For Local arg:TExpr = EachIn args
					If TFloatType(arg.exprType) Or TDoubleType(arg.exprType) Then
						isFloat = True
					End If
				Next
				If isFloat Then
					Return "bbFloat" + decl.ident + TransArgsTypes(args, [TType.floatType, TType.floatType])
				Else
					' TODO : Long support
					Return "bbInt" + decl.ident + TransArgs(args, decl)
				End If
			Case "len"
				Local arg:TExpr = args[0]
				If TStringType(arg.exprType) Then
					Return TVarExpr(arg).decl.munged + "->length"
				End If
			Case "asc"
				Local arg:TExpr = args[0]
				If TConstExpr(arg) InternalErr ' we should have handled this case already
				Return "bbStringAsc" + TransArgs(args, decl)
			Case "chr"
				If TConstExpr(args[0]) InternalErr ' we should have handled this case already
				Return "bbStringFromChar" + TransArgs(args, decl)
		End Select
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
		Local sc:TStringConst = TStringConst(_app.stringConsts.ValueForKey(value))
		Local s:String
		
		If Not sc Then
			'InternalErr
			s = "bbEmptyString"
'			s = "_s" + stringConstCount
'
'			stringMap.Insert(value, s)
'
'			stringConstCount:+ 1e
		Else
			s = sc.id
		End If

		Return "&" + s
	End Method
	
	Method TransNewObjectExpr$( expr:TNewObjectExpr )
		Local t$ = "bbObjectNew(&" + expr.classDecl.actual.munged + ")"
		'Local t$="(new "+expr.classDecl.actual.munged+")"
		'If expr.ctor t:+"->"+expr.ctor.actual.munged+TransArgs( expr.args,expr.ctor )
		Return t
	End Method
	
	Method TransNewArrayExpr$( expr:TNewArrayExpr )
'		Local texpr$=expr.expr.Trans()
		'
		Return "bbArrayNew1D" + Bra(TransArrayType(expr.ty) + ", " + expr.expr.Trans())
		'Return "Array<"+TransRefType( expr.ty, "LL" )+" >"+Bra( expr.expr.Trans() )
	End Method
		
	Method TransSelfExpr$( expr:TSelfExpr )
		Return "o"
	End Method
	
	Method TransCastExpr$( expr:TCastExpr )
	
		Local t$= expr.expr.Trans()
		
		Local dst:TType=expr.exprType
		Local src:TType=expr.expr.exprType
		
		If TBoolType( dst )
			If TBoolType( src ) Return t
			If TIntType( src ) Return Bra( t+"!=0" )
			If TFloatType( src ) Return Bra( t+"!=0.0f" )
		'	If TArrayType( src ) Return Bra( t+".Length()!=0" )
			If TStringType( src ) Return Bra( t+"!= &bbEmptyString" )
			If TObjectType( src ) Return Bra( t+"!= &bbNullObject" )
			If TPointerType( src ) Return Bra( t )
		Else If TIntType( dst )
			If TBoolType( src ) Return Bra( t )
			If TByteType( src) Return Bra("(BBINT)"+t)
			If TShortType( src) Return Bra("(BBINT)"+t)
			If TBoolType( src ) Return t
			If TIntType( src ) Return t
			If TFloatType( src ) Return Bra("(BBINT)"+t)
			If TDoubleType( src ) Return Bra("(BBINT)"+t)
			If TStringType( src ) Return "bbStringToInt" + Bra(t)
		 Else If TLongType( dst )
			If TByteType( src) Return Bra("(BBLONG)"+t)
			If TShortType( src) Return Bra("(BBLONG)"+t)
			If TIntType( src) Return Bra("(BBLONG)"+t)
			If TLongType( src ) Return t
			If TFloatType( src ) Return Bra("(BBLONG)"+t)
			If TDoubleType( src ) Return Bra("(BBLONG)"+t)
			If TStringType( src ) Return "bbStringToLong" + Bra(t)
		Else If TFloatType( dst )
			If TIntType( src ) Return Bra("(BBFLOAT)"+t)
			If TFloatType( src ) Return t
			If TDoubleType( src ) Return Bra("(BBFLOAT)"+t)
			If TStringType( src ) Return "bbStringToFloat" + Bra(t)
		Else If TDoubleType( dst )
			If TIntType( src ) Return Bra("(BBDOUBLE)"+t)
			If TDoubleType( src ) Return t
			If TFloatType( src ) Return Bra("(BBDOUBLE)"+t)
			If TStringType( src ) Return "bbStringToDouble" + Bra(t)
		Else If TStringType( dst )
			If TByteType( src ) Return "bbStringFromInt"+Bra( t )
			If TShortType( src ) Return "bbStringFromInt"+Bra( t )
			If TIntType( src ) Return "bbStringFromInt"+Bra( t )
			If TLongType( src ) Return "bbStringFromLong"+Bra( t )
			If TFloatType( src ) Return "bbStringFromFloat"+Bra( t )
			If TDoubleType( src ) Return "bbStringFromDouble"+Bra( t )
			If TStringType( src ) Return t
			If TStringVarPtrType( src ) Return "*" + t
		Else If TStringVarPtrType( dst )
'DebugStop
		Else If TByteType( dst )
			If TByteType( src) Return t
			If TIntType( src ) Return Bra("(BBBYTE)"+t)
			If TFloatType( src ) Return Bra("(BBBYTE)"+t)
			If TDoubleType( src ) Return Bra("(BBBYTE)"+t)
			If TStringType( src ) Return t+".ToByte()"
		Else If TShortType( dst )
			If TShortType( src) Return t
			If TByteType( src) Return Bra("(BBSHORT)"+t)
			If TIntType( src ) Return Bra("(BBSHORT)"+t)
			If TFloatType( src ) Return Bra("(BBSHORT)"+t)
			If TDoubleType( src ) Return Bra("(BBSHORT)"+t)
			If TStringType( src ) Return "bbStringToShort" + Bra(t)
		Else If TVarPtrType( dst )
			If TByteType( src) Return Bra("&"+t)
			If TShortType( src) Return Bra("&"+t)
			If TFloatType( src) Return Bra("&"+t)
			If TIntType( src) Return Bra("&"+t)
			If TDoubleType( src) Return Bra("&"+t)
		
		Else If TPointerType( dst )
			If TArrayType(src) Then
				Return Bra(Bra(TransType(dst, "")) + "BBARRAYDATA(" + t + "," + t + "->dims)")
			End If
			If TByteType( src) Return Bra("&"+t)
			
			If TStringType(src) Then
				Local tmp:String = CreateLocal2(TType.bytePointerType, t)
				
				Return tmp
			End If
		EndIf
		
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
		Local t_rhs$=TransSubExpr( expr.rhs,pri-1 )

		If TStringType(expr.exprType) And expr.op = "+" Then
			Return "bbStringConcat(" + t_lhs + "," + t_rhs + ")"
		End If
		
		Return bra(t_lhs+TransBinaryOp( expr.op,t_rhs )+t_rhs)
	End Method
	
	Method TransIndexExpr$( expr:TIndexExpr )
	
		Local t_expr$=TransSubExpr( expr.expr )
		Local t_index$=expr.index.Trans()
		
		If TStringType( expr.expr.exprType ) Then
			Return Bra(t_expr) + "->buf[" + t_index + "]" 
			'Return "(BBINT)"+t_expr+"["+t_index+"]"
		End If
		
		If TArrayType( expr.expr.exprType ) Then
'DebugStop
			Return Bra("(" + TransType(expr.exprType, "") + "*)BBARRAYDATA(" + t_expr + "," + t_expr + "->dims)") + "[" + t_index + "]"
		End If
		
		Local swiz$
		If TObjectType( expr.exprType )And expr.exprType.GetClass().IsInterface() swiz=".p"
		
		'If ENV_CONFIG="debug" Return t_expr+".At("+t_index+")"+swiz
		
		Return t_expr+"["+t_index+"]"+swiz
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
				t_args :+ "," + t_expr + "->scales[0]"
			Else If TStringVarPtrType(expr.exprType) Then
				t_args :+ ",(*" + t_expr + ")->length"
			Else
				t_args :+ "," + t_expr + "->length"
			End If
		End If
		
		If TArrayType(expr.exprType) Then
			Return "bbArraySlice" + Bra(TransArrayType(TArrayType(expr.exprType).elemType) + "," + t_expr + "," + t_args)
		Else If TStringVarPtrType(expr.exprType) Then
			Return "bbStringSlice" + Bra("*" + t_expr + "," + t_args)
		Else
			Return "bbStringSlice" + Bra(t_expr + "," + t_args)
		End If
		'Return t_expr+".Slice("+t_args+")"
	End Method
	
	Method TransArrayExpr$( expr:TArrayExpr )
		Local elemType:TType=TArrayType( expr.exprType ).elemType

		Local tmp:TLocalDecl =New TLocalDecl.Create( "",TType.voidType,Null )
		MungDecl tmp
		
		Local t$
		Local count:Int
		For Local elem:TExpr=EachIn expr.exprs
			If t t:+","
			t:+elem.Trans()
			count :+ 1
		Next
		
		Local tt$
'		If Not _env tt="static "

		Emit tt+TransType( elemType, tmp.munged )+" "+tmp.munged+"[]={"+t+"};"
'DebugStop
		Return "bbArrayFromData" + Bra(TransArrayType(elemType) + "," + count + "," + tmp.munged )
		'Return "Array<"+TransRefType( elemType, "MM" )+" >("+tmp.munged+","+expr.exprs.Length+")"
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
		Case "pow" Return "(float)"+id+Bra( arg0+","+arg1 )
		'
		End Select
		InternalErr
	End Method

	'***** Statements *****

	Method TransTryStmt$( stmt:TTryStmt )
		Emit "// TODO : Try/Catch"
		Emit "//try{//"
		EmitBlock( stmt.block )
		For Local c:TCatchStmt=EachIn stmt.catches
			MungDecl c.init
			Emit "//}catch("+TransType( c.init.ty, "" )+" "+c.init.munged+"){//"
			'dbgLocals.Push c.init
			'EmitBlock( c.block )
		Next
		Emit "//}"
	End Method

	Method TransAssignStmt$( stmt:TAssignStmt )
		If Not stmt.rhs Return stmt.lhs.Trans()

		Local rhs$=stmt.rhs.Trans()
		Local lhs$=stmt.lhs.TransVar()
		
		Local s:String
		
'		If ObjectType( stmt.rhs.exprType )
'			If stmt.rhs.exprType.GetClass().IsInterface() rhs="GC_IPTR"+Bra(rhs)
'		Endif
		If TStringType(stmt.lhs.exprType) Then
'			s:+ "{"	
'			s:+ "BBSTRING tmp=" + lhs + ";~n"

			
			If stmt.op = "+=" Then
				s :+ lhs+"=bbStringConcat("+lhs+","+rhs+")"
			Else
				s :+ lhs+TransAssignOp( stmt.op )+rhs
			End If

'			s :+ ";~nBBRETAIN(" + lhs +")~n"
'			s :+ "BBRELEASE(tmp)~n"
			
'			s:+ "}"
		Else If TStringVarPtrType(stmt.lhs.exprType) Then
			s :+ "*" + lhs+TransAssignOp( stmt.op )+rhs
		Else
			s :+ lhs+TransAssignOp( stmt.op )+rhs
		End If
		
		Return s
	End Method
	
	Method TransThrowStmt:String( stmt:TThrowStmt )
		Local s:String = "bbExThrow("
		
		s:+ stmt.expr.Trans()
		
		s :+ ")"
		Return s
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
		If odecl.overrides Then Return
'DebugLog decl.ident
'		While odecl.overrides
'			odecl=odecl.overrides
'		Wend

		Local id$=decl.munged
		Local pre:String
		
		If decl.IsMethod() Then
			id :+ "_md"
			pre = "md_"
		Else
			id :+ "_fn"
			pre = "fn_"
		End If

		Local bk:String = ";"
		'Local pre:String = "typedef "
		'If odecl.IsExtern() Then
		'	pre = "extern "
		'End If
'DebugLog "id = " + id
		Emit id + " " + pre + odecl.ident + ";"

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

	Method EmitClassFuncProto( decl:TFuncDecl)
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
			args :+ "BBOBJECT "
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
		If decl.IsMethod() Then
			id :+ "_md"
		Else
			id :+ "_fn"
		End If
		'If odecl.IsExtern() Then
		'	pre = "extern "
		'End If

'		If Not proto Or (proto And Not odecl.IsExtern()) Then
			If Not TFunctionPtrType(odecl.retType) Then
				If Not odecl.castTo Then
					If Not decl.overrides Then
						Emit pre + TransType( odecl.retType, "" )+" "+ Bra("*" + id)+Bra( args ) + bk
					End If
					If decl.IsMethod() Then
						Emit TransType(odecl.retType, "") + " _" + decl.munged +Bra( args ) + bk
					Else
						Emit TransType(odecl.retType, "") + " " + decl.munged +Bra( args ) + bk
					End If
				Else
					If Not odecl.noCastGen Then
						If Not decl.overrides Then
							Emit pre + odecl.castTo +" "+Bra("*" + id)+Bra( args ) + bk
						End If
						If decl.IsMethod() Then
							Emit odecl.castTo + " _" + decl.munged +Bra( args ) + bk
						Else
							Emit odecl.castTo + " " + decl.munged +Bra( args ) + bk
						End If
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

		'PopMungScope
		EndLocalScope
	End Method



	Method EmitFuncDecl( decl:TFuncDecl, proto:Int = False, classFunc:Int = False )
		'If Not proto And decl.IsAbstract() Return
		
		'PushMungScope
		BeginLocalScope
'DebugStop
		decl.Semant
		
		MungDecl decl
		
		'Find decl we override
		Local odecl:TFuncDecl=decl
		While odecl.overrides
			odecl=odecl.overrides
		Wend

		'Generate 'args' string and arg casts
		Local args$

		' pass object for method
		If decl.IsMethod() Then
			args :+ "BBOBJECT o"
		End If

		Local argCasts:TStack =New TStack
		For Local i:Int=0 Until decl.argDecls.Length
			Local arg:TArgDecl=decl.argDecls[i]
			Local oarg:TArgDecl=odecl.argDecls[i]
			MungDecl arg
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
		If proto Then
			If odecl.IsExtern() Then
				pre = "extern "
			End If
			bk = ";"
		End If

'		If Not proto Or (proto And Not odecl.IsExtern()) Then
			If Not TFunctionPtrType(odecl.retType) Then
				If Not odecl.castTo Then
					Emit pre + TransType( odecl.retType, "" )+" "+id+Bra( args ) + bk
				Else
					If Not odecl.noCastGen Then
						Emit pre + odecl.castTo +" "+id+Bra( args ) + bk
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

		If Not proto Then
			If decl.IsAbstract() Then
				Emit "brl_blitz_NullMethodError();"
			Else
If decl.ident = "Eof" DebugStop

				decl.Semant()
'If decl.ident = "LoadByteArray" DebugStop
		' TODO : enable block output
				EmitBlock decl
		'		Emit "// TODO : enable block output"
		'		Emit "printf(~qTODO : " + decl.munged + "\n~q);fflush(stdout);"

			End If
			Emit "}"
		End If

		EndLocalScope
		'PopMungScope
	End Method
	
	Method EmitClassFieldsProto(classDecl:TClassDecl)
	
		If classDecl.superClass Then
			EmitClassFieldsProto(classDecl.superClass)
		End If
		
		For Local decl:TFieldDecl = EachIn classDecl.Decls()
			decl.Semant()
			
			If Not TFunctionPtrType(decl.ty) Then
				Emit TransType(decl.ty, classDecl.actual.munged) + " _" + classDecl.actual.munged.ToLower() + "_" + decl.ident.ToLower() + ";"
			Else
				Emit TransType(decl.ty, "_" + classDecl.actual.munged.ToLower() + "_" + decl.ident.ToLower()) + ";"
			End If
		Next
		
	End Method

	Method EmitClassGlobalsProto(classDecl:TClassDecl)
	
		For Local decl:TGlobalDecl = EachIn classDecl.Decls()
			decl.Semant()
			
			Emit "extern "+TransRefType( decl.ty, "" )+" "+ decl.munged+";"	'forward reference...
		Next
		
	End Method
	
	Method EmitBBClassClassFuncProto( classDecl:TClassDecl )

		Local reserved:String = "New,Delete,ToString,ObjectCompare,SendMessage,_reserved1_,_reserved2_,_reserved3_".ToLower()

		If classDecl.superClass Then
			EmitBBClassClassFuncProto(classDecl.superClass)
		End If

		' user defined functions and methods
		'Local fdecls:TFuncDecl[] = classDecl.GetAllFuncDecls()
		'For Local fdecl:TFuncDecl = EachIn fdecls
		For Local decl:TDecl=EachIn classDecl.Decls()
			Local fdecl:TFuncDecl =TFuncDecl( decl )
			If fdecl
				If reserved.Find(fdecl.ident.ToLower()) = -1 Then
					EmitBBClassFuncProto( fdecl )
					Continue
				End If
			EndIf
		Next
		
	End Method
	
	Method EmitClassProto( classDecl:TClassDecl )

		Local classid$=classDecl.munged
		Local superid$=classDecl.superClass.actual.munged


		If opt_issuperstrict Then
			Emit "void _" + classid + "_New(BBOBJECT o);"
			Emit "void _" + classid + "_Delete(BBOBJECT o);"
		Else
			Emit "int _" + classid + "_New(BBOBJECT o);"
			Emit "int _" + classid + "_Delete(BBOBJECT o);"
		End If

		If classHasFunction(classDecl, "ToString") Then
			Emit "BBSTRING _" + classid + "_ToString(BBOBJECT o);"
		End If
		
		If classHasFunction(classDecl, "ObjectCompare") Then
			Emit "BBINT _" + classid + "_ObjectCompare(BBOBJECT o, BBOBJECT otherObject);"
		End If

		If classHasFunction(classDecl, "SendMessage") Then
			Emit "void _" + classid + "_SendMessage(BBOBJECT o, BBOBJECT message, BBOBJECT source);"
		End If

		Local reserved:String = "New,Delete,ToString,ObjectCompare,SendMessage,_reserved1_,_reserved2_,_reserved3_".ToLower()
		
		'Local fdecls:TFuncDecl[] = classDecl.GetAllFuncDecls(Null, False)
		For Local decl:TDecl=EachIn classDecl.Decls()
		'For Local fdecl:TFuncDecl = EachIn fdecls
		
			Local fdecl:TFuncDecl =TFuncDecl( decl )
			If fdecl
				If reserved.Find(fdecl.ident.ToLower()) = -1 Then
					EmitClassFuncProto( fdecl )
					Continue
				End If
			EndIf
			
			'Local gdecl:TGlobalDecl =TGlobalDecl( decl )
			'If gdecl
			'	Emit "static "+TransRefType( gdecl.ty )+" "+gdecl.munged+";"
			'	Continue
			'EndIf
		Next

		Emit ""

		' bbobject def
		
		Rem
		struct BBString{
			BBClass*	clas;
			int		refs;
			int		length;
			BBChar	buf[];
		};
		End Rem
		
		
		' emit the class structure
		Emit "struct BBClass_" + classid + " {"
		If classDecl.superClass.ident = "Object" Then
			Emit "BBClass*  super;"
		Else
			Emit "struct BBClass_" + classDecl.superClass.munged + "*  super;"
		End If
		Emit "void      (*free)( BBObject *o );"
		Emit "BBDebugScope* debug_scope;"
		Emit "int       instance_size;"
		Emit "void      (*ctor)( BBOBJECT o );"
		Emit "void      (*dtor)( BBOBJECT o );"
		Emit "BBSTRING (*ToString)( BBOBJECT x );"
		Emit "int       (*Compare)( BBOBJECT x,BBOBJECT y );"
		Emit "BBOBJECT (*SendMessage)( BBOBJECT m,BBOBJECT s );"
		Emit "void      (*_reserved1_)();"
		Emit "void      (*_reserved2_)();"
		Emit "void      (*_reserved3_)();"
		
		EmitBBClassClassFuncProto(classDecl)
		
'		' user defined functions and methods
'		For Local decl:TDecl=EachIn classDecl.Decls()
'			Local fdecl:TFuncDecl =TFuncDecl( decl )
'			If fdecl
'				If reserved.Find(fdecl.ident.ToLower()) = -1 Then
'					EmitBBClassFuncProto( fdecl )
'					Continue
'				End If
'			EndIf
'		Next
		
		
		Emit "};~n"
		
		
		
		
		'Emit "typedef struct " + classid + "_obj {"
		Emit "struct " + classid + "_obj {"
		Emit "struct BBClass_" + classid + "* clas;"
		Emit "int refs;"

		BeginLocalScope
		EmitClassFieldsProto(classDecl)		
		EndLocalScope
'		For Local decl:TFieldDecl = EachIn classDecl.Decls()
'			Emit "~t" + TransType(decl.declTy) + " _" + decl.ident.ToLower() + ";"
'		Next
		
		'Emit "} " + classid + "_obj;"
		Emit "};"




		Emit "struct BBClass_" + classid + " " + classid + ";"

		EmitClassGlobalsProto(classDecl);

Rem	
		' super class
'		If Not classDecl.superClass Then
'			Emit "~t&bbObjectClass,"
'		Else
'		If classDecl.superClass.ident = "Object" Then
			Emit "&" + classDecl.superClass.munged + ","
'		Else
'			Emit "&_" + classDecl.superClass.munged + ","
'		End If
'		End If
		
		Emit "bbObjectFree,"
		
		Emit "0,"
		'Emit "~t" + (OBJECT_BASE_OFFSET + classDecl.lastOffset) + ","
		Emit "sizeof" + Bra("struct " + classid + "_obj") + ","
		
		
		Emit "_" + classid + "_New,"
		Emit "_" + classid + "_Delete,"
		
		If classHasFunction(classDecl, "ToString") Then
			Emit "_" + classid + "_ToString,"
		Else
			Emit "bbObjectToString,"
		End If
		
		If classHasFunction(classDecl, "ObjectCompare") Then
			Emit "_" + classid + "_ObjectCompare,"
		Else
			Emit "bbObjectCompare,"
		End If

		If classHasFunction(classDecl, "SendMessage") Then
			Emit "_" + classid + "_SendMessage,"
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

		Emit "bbObjectReserved,"
		Emit "bbObjectReserved,"
		Emit "bbObjectReserved"

		' methods/funcs
		'reserved = "New,Delete,ToString,ObjectCompare,SendMessage".ToLower()

		Local fdecls:TFuncDecl[] = classDecl.GetAllFuncDecls()
		'For Local decl:TFuncDecl = EachIn classDecl.Decls()
		For Local decl:TFuncDecl = EachIn fdecls
			If reserved.Find(decl.ident.ToLower()) = -1 Then
			
				MungDecl decl

				Emit ",_" + decl.munged
			End If
		Next
		
		Emit "};~n"
End Rem


		' fields
		For Local decl:TFieldDecl = EachIn classDecl.Decls()
'DebugStop
			MungDecl decl
			
			'Emit "#define " + decl.munged + " " + (OBJECT_BASE_OFFSET + decl.offset)
		
		Next

	End Method
	
	Method classHasFunction:Int(classDecl:TClassDecl, func:String)
'DebugStop
		For Local decl:TFuncDecl = EachIn classDecl.Decls()
			If decl.ident.ToLower() = func.toLower() Then
				Return True
			End If
		Next
		Return False
	End Method
	
	Method EmitMark( id$,ty:TType,queue:Int )
	
		If TObjectType( ty )
		
			If id.EndsWith( ".p" )
				If ty.GetClass().IsInterface() id=id[..-2] Else InternalErr
			Else 
				If ty.GetClass().IsInterface() InternalErr
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
	
	Method EmitClassDecl( classDecl:TClassDecl )

		'If classDecl.IsTemplateInst()
		'	Return
		'EndIf
		
		If classDecl.IsInterface()
			Return
		EndIf

		Local classid$=classDecl.munged
		Local superid$=classDecl.superClass.actual.munged
		
		EmitClassDeclNew(classDecl)
		EmitClassDeclDelete(classDecl)
		
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
		Local reserved:String = "New,Delete".ToLower()

		'methods		
		For Local decl:TDecl=EachIn classDecl.Decls()
		
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl
				If reserved.Find(fdecl.ident.ToLower()) = -1 Then
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
				Emit TransRefType( gdecl.ty, "" )+" "+gdecl.munged+";"
				Continue
			EndIf
		Next
	
	
		reserved = "New,Delete,ToString,ObjectCompare,SendMessage,_reserved1_,_reserved2_,_reserved3_".ToLower()
	
		Emit "struct BBClass_" + classid + " " + classid + "={"
		
		' super class
'		If Not classDecl.superClass Then
'			Emit "~t&bbObjectClass,"
'		Else
'		If classDecl.superClass.ident = "Object" Then
			Emit "&" + classDecl.superClass.munged + ","
'		Else
'			Emit "&_" + classDecl.superClass.munged + ","
'		End If
'		End If
		
		Emit "bbObjectFree,"
		
		Emit "0,"
		'Emit "~t" + (OBJECT_BASE_OFFSET + classDecl.lastOffset) + ","
		Emit "sizeof" + Bra("struct " + classid + "_obj") + ","
		
		
		Emit "_" + classid + "_New,"
		Emit "_" + classid + "_Delete,"
		
		If classHasFunction(classDecl, "ToString") Then
			Emit "_" + classid + "_ToString,"
		Else
			Emit "bbObjectToString,"
		End If
		
		If classHasFunction(classDecl, "ObjectCompare") Then
			Emit "_" + classid + "_ObjectCompare,"
		Else
			Emit "bbObjectCompare,"
		End If

		If classHasFunction(classDecl, "SendMessage") Then
			Emit "_" + classid + "_SendMessage,"
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

		Emit "bbObjectReserved,"
		Emit "bbObjectReserved,"
		Emit "bbObjectReserved"

		' methods/funcs
		'reserved = "New,Delete,ToString,ObjectCompare,SendMessage".ToLower()

		Local fdecls:TFuncDecl[] = classDecl.GetAllFuncDecls()
		'For Local decl:TFuncDecl = EachIn classDecl.Decls()
		For Local decl:TFuncDecl = EachIn fdecls
			If reserved.Find(decl.ident.ToLower()) = -1 Then
			
				MungDecl decl

				If decl.IsMethod() Then
					Emit ",_" + decl.munged
				Else
					Emit "," + decl.munged
				End If
			End If
		Next
		
		Emit "};~n"
	
	
	End Method
	
	Method EmitClassDeclNew( classDecl:TClassDecl )
		Local classid$=classDecl.munged
		Local superid$=classDecl.superClass.actual.munged
		
		' New
		If opt_issuperstrict Then
			Emit "void _" + classid + "_New(BBObject *o) {"
		Else
			Emit "int _" + classid + "_New(BBObject *o) {"
		End If
		
		If classDecl.superClass.ident = "Object" Then
			Emit "bbObjectCtor(o);"
		Else
			Emit "_" + superid + "_New(o);"
		End If
		
		Emit "o->clas = (BBClass*)&" + classid + ";"
		
		' field initialisation
		For Local decl:TFieldDecl=EachIn classDecl.Decls()
			Local fld:String

			' ((int*)((char*)o + 5))[0] = 
			fld :+ TransFieldRef(decl, "o")

			If decl.init Then
				' initial value
				fld :+ "= " + decl.init.Trans() + ";";
			Else
				If TNumericType(decl.ty) Or TObjectType(decl.ty) Or TPointerType(decl.ty) Then
					fld :+ "= 0;"
				Else If TStringType(decl.ty) Then
					fld :+ "= &bbEmptyString;"
				End If
			End If
			
			Emit fld
		Next

		Local decl:TFuncDecl = classDecl.FindFuncDecl("New")
		If decl Then
			decl.Semant
			EmitBlock decl
		End If

		'
		Emit "}"
	End Method

	Method EmitClassDeclDelete( classDecl:TClassDecl )
		Local classid$=classDecl.munged
		Local superid$=classDecl.superClass.actual.munged
		
		' New
		If opt_issuperstrict Then
			Emit "void _" + classid + "_Delete(BBObject *o) {"
		Else
			Emit "int _" + classid + "_Delete(BBObject *o) {"
		End If
		
		Local decl:TFuncDecl = classDecl.FindFuncDecl("Delete")
		If decl Then
			decl.Semant
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
		If classDecl.superClass.ident = "Object" Then
			Emit "bbObjectDtor(o);"
		Else
			Emit "_" + superid + "_Delete(o);"
		End If
		
		'
		Emit "}"
	End Method

	Method TransFieldRef:String(decl:TFieldDecl, variable:String, exprType:TType = Null)
		Local s:String = variable

		' array.length
		If decl.scope And decl.scope.ident = "Array" Then
			If decl.ident = "length" Then
				Return Bra(variable + "->scales[0]")
			End If
		End If
		
		' string methods
		If decl.scope And decl.scope.ident = "String" Then
			If decl.ident = "length" Then
				If TStringVarPtrType(exprType) Then
					Return Bra("(*" + variable + ")->length")
				Else
					Return Bra(variable + "->length")
				End If
			End If
		End If

		If TNumericType(decl.ty) Then
			' ((int*)((char*)o + 5))[0]
			's = "(" + TransType(decl.declTy) + "*((char*)" + variable + "+" + decl.munged + "))[0]"
			's = "((" + TransType(decl.declTy, "BB") + ")(struct " + decl.scope.munged + "_obj*)" + variable + ")->" + decl.munged + "))"
			s = "((struct " + decl.scope.munged + "_obj*)" + variable + ")->" + decl.munged + " "
		Else If TStringType(decl.ty) Then
			' ((BBString*)((char*)o+5))
			's = "((BBSTRING)(((" + decl.scope.munged + "_obj*)" + variable + ")->" + decl.munged + "))"
			s = "((struct " + decl.scope.munged + "_obj*)" + variable + ")->" + decl.munged + " "
		Else If TObjectType(decl.ty) Then
			' ((BBString*)((char*)o+5))
			's = "((BBSTRING)(((" + decl.scope.munged + "_obj*)" + variable + ")->" + decl.munged + "))"
			s = "((struct " + decl.scope.munged + "_obj*)" + variable + ")->" + decl.munged + " "
		Else If TPointerType(decl.ty) Then
			s = "((struct " + decl.scope.munged + "_obj*)" + variable + ")->" + decl.munged + " "
		End If
		
		Return s
	End Method
	
	' " _" + classDecl.actual.munged + "_" + decl.ident.ToLower(

	Method TransIfcArgs:String(funcDecl:TFuncDecl)
		Local args:String

		For Local i:Int=0 Until funcDecl.argDecls.Length
			Local arg:TArgDecl = funcDecl.argDecls[i]

			If args args:+","
			args:+ arg.ident + TransIfcType( arg.ty )

			If arg.init Then
				args:+ "=" + TransIfcConstExpr(arg.init)
			End If
		Next
			
		Return Bra(args)
	End Method
	
	Method EmitIfcClassFuncDecl(funcDecl:TFuncDecl)
	
		Local func:String
	
		' method / function	
		If funcDecl.IsMethod() Then
			func :+ "-"
		Else
			func :+ "+"
		End If

		func :+ funcDecl.ident
		
		func :+ TransIfcType(funcDecl.retType)
		
		' function args
		func :+ TransIfcArgs(funcDecl)
		
		func :+ "="
		
		func :+ Enquote(funcDecl.munged)
		
		Emit func
		
	End Method

	Method EmitIfcFuncDecl(funcDecl:TFuncDecl)
	
		Local func:String
	
		func :+ funcDecl.ident
		
		func :+ TransIfcType(funcDecl.retType)
		
		' function args
		func :+ TransIfcArgs(funcDecl)
		
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

		If TNumericType(expr.exprType) Then
			Local s:String = expr.Eval()
			If Not s Then
				Return "0"
			Else
				Return s
			End If
		EndIf

		If TStringType(expr.exprType) Then
			Return "$" + Enquote(expr.Eval())
		EndIf
		
		If TFunctionPtrType(expr.exprType) Then
			If TCastExpr(expr) Then
				If TInvokeExpr(TCastExpr(expr).expr) Then
					Return Enquote(TInvokeExpr(TCastExpr(expr).expr).decl.munged)
				End If
			End If
			
			InternalErr
		End If
		
		If TObjectType(expr.exprType) And TNullDecl(TObjectType(expr.exprType).classDecl) Then
			Return Enquote("bbNullObject")
		End If

	End Method

	Method EmitIfcConstDecl(constDecl:TConstDecl)
		Local c:String
		c = constDecl.ident + TransIfcType(constDecl.declTy)

		If TExpr(constDecl.declInit) Then
			c:+ "=" + TransIfcConstExpr(TExpr(constDecl.init))
		End If		
		
		Emit c
	End Method
	
	Method EmitIfcFieldDecl(fieldDecl:TFieldDecl)
		Local f:String = "." + fieldDecl.ident + TransIfcType(fieldDecl.ty)
		
		f :+ "&"
		
		Emit f
	End Method
	
	Method EmitIfcClassDecl(classDecl:TClassDecl)

		Emit classDecl.ident + "^" + classDecl.superClass.ident + "{", False
		
		'PushMungScope
		BeginLocalScope
		
		' fields, globals and consts
'		For Local decl:TDecl=EachIn classDecl.Decls()
		
		' const
		For Local cDecl:TConstDecl = EachIn classDecl.Decls()
			EmitIfcConstDecl(cDecl)
		Next
			
			' global
		For Local gDecl:TGlobalDecl = EachIn classDecl.Decls()
			EmitIfcGlobalDecl(gDecl)
		Next
			
			
			' field
		For Local fDecl:TFieldDecl = EachIn classDecl.Decls()
			EmitIfcFieldDecl(fDecl)
		Next
		
		
		' functions
		Emit "-New%()=" + Enquote("_" + classDecl.munged + "_New")
		Emit "-Delete%()=" + Enquote("_" + classDecl.munged + "_Delete")
		
		Local reserved:String = "New,Delete,ToString,ObjectCompare,SendMessage,_reserved1_,_reserved2_,_reserved3_".ToLower()

		For Local decl:TDecl=EachIn classDecl.Decls()
		
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl
				If reserved.Find(fdecl.ident.ToLower()) = -1 Then
					EmitIfcClassFuncDecl fdecl
				End If
				Continue
			EndIf

		Next
	
		
		Emit "}=" + Enquote(classDecl.munged), False

		'PopMungScope
		EndLocalScope
		
	End Method
	
	Method EmitIfcGlobalDecl(globalDecl:TGlobalDecl)
		Local g:String = globalDecl.ident
		
		g:+ TransIfcType(globalDecl.ty)
		
		g:+ "&"
		
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
	
	Method EmitModuleInclude(moduleDecl:TModuleDecl)
		If moduleDecl.filepath Then
			' a module import
			If FileType(moduleDecl.filepath) = FILETYPE_DIR Or (opt_ismain And moduleDecl.ident = opt_modulename) Then
				Emit "#include <" + ModuleHeaderFromIdent(moduleDecl.ident, True) + ">"
			Else
				' maybe a file import...
				Emit "#include ~q" + FileHeaderFromFile(moduleDecl.filepath) + "~q"
			End If
'			DebugLog moduleDecl.filepath
		End If
	End Method
	
	Method EmitModuleInit(moduleDecl:TModuleDecl)
		If moduleDecl.filepath Then
			' a module import
			If FileType(moduleDecl.filepath) = FILETYPE_DIR Then
				Emit MungModuleName(moduleDecl.ident) + "();"
			Else
				' maybe a file import...
				Emit MungImportFromFile(moduleDecl.ident) + "();"
			End If
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
		
		For Local decl:TModuleDecl=EachIn app.imported.Values()
			For Local mdecl:TDecl=EachIn decl.imported.Values()

				MungDecl mdecl
DebugLog mdecl.munged

				If TModuleDecl(mdecl) And app.mainModule <> mdecl And mdecl.ident <> "brl.classes" And mdecl.ident <> "brl.blitzkeywords" Then
					EmitModuleInclude(TModuleDecl(mdecl))
				End If
			Next
		Next

		Emit "int " + app.munged + "();"

		For Local decl:TDecl=EachIn app.Semanted()
		
			MungDecl decl

			Local cdecl:TClassDecl=TClassDecl( decl )
			If Not cdecl Continue

' mung, but don't emit			
'			Emit prefix + decl.munged+";"
			
			'PushMungScope
			BeginLocalScope
			
			For Local decl:TDecl=EachIn cdecl.Semanted()
				MungDecl decl
			Next
			
			EndLocalScope
			'PopMungScope
		Next

		'prototypes/header!
		For Local decl:TDecl=EachIn app.Semanted()
		
			Local gdecl:TGlobalDecl=TGlobalDecl( decl )
			If gdecl
				If Not TFunctionPtrType(gdecl.ty) Then
					Emit "extern "+TransRefType( gdecl.ty, "" )+" "+gdecl.munged+";"	'forward reference...
				Else
					Emit "extern "+TransRefType( gdecl.ty, gdecl.munged )+";"	'forward reference...
				End If
				Continue
			EndIf
		
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl' And Not fdecl.IsExtern()
				' don't include the main function - it's handled separately
				If fdecl = app.mainFunc Then
					Continue
				End If
			
				EmitFuncDecl( fdecl, True)
				Continue
			EndIf
		
			Local cdecl:TClassDecl=TClassDecl( decl )
			If cdecl
				EmitClassProto cdecl
				Continue
			EndIf
		Next
			
	End Method
	
	Method TransSource(app:TAppDecl)

		SetOutput("source")

		' include our header
		EmitModuleInclude(app.mainModule)

		' strings
		For Local s:String = EachIn app.stringConsts.Keys()
			If s Then
				Local key:TStringConst = TStringConst(app.stringConsts.ValueForKey(s))
	
				Emit "static BBString " + key.id + "={"
				Emit "&bbStringClass,"
				Emit "2147483647,"
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
		Next
		
		'definitions!
		For Local decl:TDecl=EachIn app.Semanted()
			
			Local gdecl:TGlobalDecl=TGlobalDecl( decl )
			If gdecl
				If Not TFunctionPtrType(gdecl.ty) Then
					Emit TransRefType( gdecl.ty, "WW" )+" "+gdecl.munged+";"
				Else
					Emit TransRefType( gdecl.ty, gdecl.munged ) + ";"
				End If
				Continue
			EndIf
			
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl And Not fdecl.IsExtern()
				' don't include the main function - it's handled separately
				If fdecl = app.mainFunc Then
					Continue
				End If

				EmitFuncDecl fdecl
				Continue
			EndIf

			Local cdecl:TClassDecl=TClassDecl( decl )
			If cdecl
				EmitClassDecl cdecl
				Continue
			EndIf
		Next

		Emit "static int " + app.munged + "_inited" + " = 0;"	
		
		Emit "int " + app.munged + "(){"
		
		' call any imported mod inits
		For Local decl:TModuleDecl=EachIn app.imported.Values()
			For Local mdecl:TDecl=EachIn decl.imported.Values()
				If TModuleDecl(mdecl) And app.mainModule <> mdecl And mdecl.ident <> "brl.classes" And mdecl.ident <> "brl.blitzkeywords" Then
					EmitModuleInit(TModuleDecl(mdecl))
				End If
			Next
		Next
		
		' initialise stuff
		Emit "if (!" + app.munged + "_inited) {"
		Emit app.munged + "_inited = 1;"
		
		' register types
		For Local decl:TDecl=EachIn app.Semanted()
			Local cdecl:TClassDecl=TClassDecl( decl )
			If cdecl
				Emit "bbObjectRegisterType(&" + cdecl.munged + ");"
			EndIf
		Next
		'
		
		' initialise globals
		For Local decl:TGlobalDecl=EachIn app.semantedGlobals
'DebugStop
			decl.Semant
			
			' TODO : what about OnDebugStop etc, who have no init ?
			If decl.init Then
				Emit TransGlobal( decl )+"="+decl.init.Trans()+";"
			End If
		Next
		
		' now do the local main stuff
		app.mainFunc.Semant()
		EmitBlock app.mainFunc
		
		
		Emit "}"
		Emit "return 0;"
		Emit "}"

		'Emit "void gc_mark(){"
		'For Local decl:TGlobalDecl=EachIn app.semantedGlobals
		'	EmitMark TransGlobal( decl ),decl.ty,False
		'Next
		'Emit "}"
		
	End Method
	
	Method TransInterface(app:TAppDecl)

		SetOutput("interface")

		' module info
		For Local info:String = EachIn app.mainModule.modInfo
			Emit "ModuleInfo ~q" + info + "~q"
		Next
		
		' module imports
		For Local decl:TDecl=EachIn app.mainModule.imported.Values()
			Local mdecl:TModuleDecl=TModuleDecl( decl )
			If mdecl Then
				If mdecl.IsActualModule() Then
					Emit "import " + mdecl.ident
				Else If Not opt_ismain And mdecl.filepath.EndsWith(".bmx") And app.mainModule<>mdecl
					Emit "import " + Enquote(StripDir(mdecl.filepath))
				End If
			End If
		Next
		
		' module imports from other files?
		If opt_buildtype = BUILDTYPE_MODULE And opt_ismain Then
			
		End If
		
		' other imports
		For Local s:String = EachIn app.fileImports
			Emit "import ~q" + s + "~q"
		Next

		' consts
		For Local decl:TDecl=EachIn app.Semanted()
			Local cdecl:TConstDecl=TConstDecl( decl )
			If cdecl
				EmitIfcConstDecl(cdecl)
			End If
		Next

		' classes
		For Local decl:TDecl=EachIn app.Semanted()
			Local cdecl:TClassDecl=TClassDecl( decl )
			If cdecl
				EmitIfcClassDecl(cdecl)
			EndIf
		Next

		' functions
		For Local decl:TDecl=EachIn app.Semanted()
			Local fdecl:TFuncDecl=TFuncDecl( decl )
			If fdecl And fdecl <> app.mainFunc Then
				EmitIfcFuncDecl(fdecl)
			End If
		Next
		
		' globals
		For Local decl:TDecl=EachIn app.Semanted()
			Local gdecl:TGlobalDecl=TGlobalDecl( decl )
			If gdecl
				EmitIfcGlobalDecl(gdecl)
			End If
		Next
		
	End Method
	
	Method TransApp( app:TAppDecl )
	
		If app.mainModule.IsSuperStrict()
			opt_issuperstrict = True
		End If
	
		TransHeader(app)
	
		TransSource(app)
		
		TransInterface(app)
	
	End Method
	
End Type
