

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
	
	Method SemantFunc:TExpr( args:TExpr[] )
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
'DebugStop
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
			If args[i] args[i]=args[i].Semant()
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
		If TStringType( lhs ) Or TStringType( rhs ) Return TType.stringType
		If TFloatType( lhs ) Or TFloatType( rhs ) Return TType.floatType
		If TPointerType( lhs ) Or TPointerType( rhs ) Then
			If TPointerType( lhs ) Return lhs
			If TPointerType( rhs ) Return rhs
		End If
		If TIntType( lhs ) Or TIntType( rhs ) Return TType.intType
		If lhs.ExtendsType( rhs ) Return rhs
		If rhs.ExtendsType( lhs ) Return lhs
		Err "Can't balance types "+lhs.ToString()+" and "+rhs.ToString()+"."
	End Method
	
	Method CopyExpr:TExpr( expr:TExpr )
		If Not expr Return Null
		Return expr.Copy()
	End Method
	
	Method CopyArgs:TExpr[]( exprs:TExpr[] )
		exprs=exprs[..]
		For Local i:Int=0 Until exprs.Length
			exprs[i]=CopyExpr( exprs[i] )
		Next
		Return exprs
	End Method

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

	Method Create:TInvokeExpr( decl:TFuncDecl,args:TExpr[]=Null )
		Self.decl=decl
		If args Then
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
		Local t$="TInvokeExpr("+decl.ToString()
		For Local arg:TExpr=EachIn args
			t:+","+arg.ToString()
		Next
		Return t+")"
	End Method
	
	Method Semant:TExpr()
		If exprType Return Self
		
		' handle Asc and Chr keywords/functions for const values
		Select decl.ident.ToLower()
			Case "asc"
				Local arg:TExpr = args[0]
				If TConstExpr(arg) Then
					Local expr:TExpr = New TConstExpr.Create(TType.intType, Asc(TConstExpr(arg).value))
					_appInstance.removeStringConst(TConstExpr(arg).value)
					expr.Semant()
					Return expr
				End If
			Case "chr"
				Local arg:TExpr = args[0]
				If TConstExpr(arg) Then
					Local expr:TConstExpr = New TConstExpr.Create(TType.stringType, Chr(Int(TConstExpr(arg).value)))
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

		ty=ty.Semant()
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
			Local i:Int=it.ident.FindLast( "." )
			If i > 0 Then
				Local fdecl:TFuncDecl = classDecl.FindFuncDecl(it.ident[i+1..], iArgs)
				If fdecl Then
					Return New TInvokeMemberExpr.Create( Self,fdecl, iArgs ).Semant()
				End If
			End If
		End If
		
		
		Return Self
	End Method
	
	Method Trans$()
		Return _trans.TransNewObjectExpr( Self )
	End Method
End Type

Type TNewArrayExpr Extends TExpr
	Field ty:TType
	Field expr:TExpr
	
	Method Create:TNewArrayExpr( ty:TType,expr:TExpr )
		Self.ty=ty
		Self.expr=expr
		Return Self
	End Method
	
	Method Copy:TExpr()
		If exprType InternalErr
		Return New TNewArrayExpr.Create( ty,CopyExpr(expr) )
	End Method
	
	Method Semant:TExpr()
		If exprType Return Self
		
		ty=ty.Semant()
		exprType=New TArrayType.Create( ty )
		expr=expr.SemantAndCast( TType.intType )
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
		funcDecl=superClass.FindFuncDecl( ident,args )
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
'DebugStop
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
				If (TObjectType(ty)<>Null)=(TObjectType(src)<>Null) exprType=ty

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
		
		If TIntType(ty) And TPointerType(src) Then
			exprType = ty
			Return expr
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
		
		If TPointerType(ty) Then
'DebugStop
			If TNumericType(src) Then
				If TType.pointerType = ty Then
					exprType = TNumericType(src).ToPointer()
				Else
					exprType = ty
				End If
				Return Self
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
		
		If Not exprType
			Err "Cannot convert from "+src.ToString()+" to "+ty.ToString()+"."
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
			If Not TNumericType( expr.exprType ) Err expr.ToString()+" must be numeric for use with unary operator '"+op+"'"
			exprType=expr.exprType
		Case "~~"
			expr=expr.SemantAndCast( TType.intType )
			exprType=TType.intType
		Case "not"
			expr=expr.SemantAndCast( TType.boolType,CAST_EXPLICIT )
			exprType=TType.boolType
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
			exprType=TType.intType
		Default
			exprType=BalanceTypes( lhs.exprType,rhs.exprType )
			If TStringType( exprType )
				If op<>"+" 
					Err "Illegal string operator."
				EndIf
			Else If Not TNumericType( exprType ) And Not TPointerType( exprType )
				Err "Illegal expression type."
			EndIf
		End Select
		
		lhs=lhs.Cast( exprType )
		rhs=rhs.Cast( exprType )
		
		If TConstExpr( lhs ) And TConstExpr( rhs ) Return EvalConst()

		Return Self
	End Method
	
	Method Eval$()
		Local lhs$=Self.lhs.Eval()
		Local rhs$=Self.rhs.Eval()
		If TIntType( exprType )
			Local x:Int=Int(lhs),y:Int=Int(rhs)
			Select op
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
			Local x#=Float(lhs),y#=Float(rhs)
			Select op
			Case "*" Return x * y
			Case "/" Return x / y
			Case "+" Return x + y
			Case "-" Return x - y
			End Select
		Else If TStringType( exprType )
			Select op
			Case "+" Return lhs+rhs
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
			Err "Arrays cannot be compared."
		EndIf

		lhs=lhs.Cast( ty )
		rhs=rhs.Cast( ty )

		exprType=TType.boolType
		
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
			Case "<=" r=(lhs<=rhs)
			Case ">"  r=(lhs> rhs)
			Case ">=" r=(lhs>=rhs)
			End Select
		Else If TFloatType( ty )
			Local lhs:Float=Float( Self.lhs.Eval() )
			Local rhs:Float=Float( Self.rhs.Eval() )
			Select op
			Case "="  r=(lhs= rhs)
			Case "<>" r=(lhs<>rhs)
			Case "<"  r=(lhs< rhs)
			Case "<=" r=(lhs<=rhs)
			Case ">"  r=(lhs> rhs)
			Case ">=" r=(lhs>=rhs)
			End Select
		Else If TStringType( ty )
			Local lhs:String=String( Self.lhs.Eval() )
			Local rhs:String=String( Self.rhs.Eval() )
			Select op
			Case "="  r=(lhs= rhs)
			Case "<>" r=(lhs<>rhs)
			Case "<"  r=(lhs< rhs)
			Case "<=" r=(lhs<=rhs)
			Case ">"  r=(lhs> rhs)
			Case ">=" r=(lhs>=rhs)
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
		
		lhs=lhs.SemantAndCast( TType.boolType,CAST_EXPLICIT )
		rhs=rhs.SemantAndCast( TType.boolType,CAST_EXPLICIT )
		
		exprType=TType.boolType
		
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
	Field index:TExpr
	
	Method Create:TIndexExpr( expr:TExpr,index:TExpr )
		Self.expr=expr
		Self.index=index
		Return Self
	End Method
	
	Method Copy:TExpr()
		Return New TIndexExpr.Create( CopyExpr(expr),CopyExpr(index) )
	End Method
	
	Method Semant:TExpr()
		If exprType Return Self
	
		expr=expr.Semant()
		index=index.SemantAndCast( TType.intType )
		
		If TStringType( expr.exprType )
			exprType=TType.intType
		Else If TArrayType( expr.exprType )
			exprType=TArrayType( expr.exprType ).elemType
		Else If TPointerType( expr.exprType) And Not TFunctionPtrType( expr.exprType )
			exprType=TType.intType
		Else
			Err "Only strings, arrays and pointers may be indexed."
		EndIf

		Return Self
	End Method
	
	Method SemantSet:TExpr( op$,rhs:TExpr )
		Semant
		Return Self
	End Method

	Method Trans$()
		Return _trans.TransIndexExpr( Self )
	End Method
	
	Method TransVar$()
		Return _trans.TransIndexExpr( Self )
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
		If TArrayType( expr.exprType ) Or TStringType( expr.exprType )  Or TStringVarPtrType( expr.exprType )
			If from from=from.SemantAndCast( TType.intType )
			If term term=term.SemantAndCast( TType.intType )
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
		
		For Local i:Int=1 Until exprs.Length
			exprs[i]=exprs[i].Semant()
			ty=BalanceTypes( ty,exprs[i].exprType )
		Next
		
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
		cdecl=exprType.GetClass()
		If Not cdecl InternalErr
	End Method
		
	Method Semant:TExpr()
		_Semant
		Err "Expression can't be used in this way"
	End Method
	
	Method SemantFunc:TExpr( args:TExpr[] )
		_Semant
		If args.Length=1 And args[0] Return args[0].Cast( cdecl.objectType,CAST_EXPLICIT )
		Err "Illegal number of arguments for type conversion"
	End Method
	
	Method SemantScope:TScopeDecl()
		_Semant
		Return cdecl
	End	Method

End Type
