' Copyright (c) 2013-2016 Bruce A Henderson
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

Global _trans:TTranslator

Type TTranslator

	Field _app:TAppDecl

	Field outputFiles:TMap = New TMap

	Field indent$
	Field LINES:TStringList'=New TStringList
	Field unreachable:Int,broken:Int
	
	Field contLabelId:Int
	Field exitLabelId:Int
	
	'Munging needs a big cleanup!
	
	Field mungScope:TMap=New TMap'<TDecl>
	Field mungStack:TStack=New TStack'< StringMap<TDecl> >
	Field funcMungs:TMap=New TMap'<FuncDeclList>
	Field customVarStack:TStack = New TStack
	Field varStack:TStack = New TStack

	Field tryStack:TStack = New TStack
	Field loopTryStack:TStack = New TStack

	Field mungedScopes:TMap=New TMap'<StringSet>
	'Field funcMungs:TFuncDeclList=New TFuncDeclList
	'Field mungedFuncs:TMap=New Map
	Field localScopeStack:TStack = New TStack
	Field localScope:TStack = New TStack
	Field ind:Int
	Field debugOut:String
	
	Field processingReturnStatement:Int

	Field _inBinary:Int

	Method PushVarScope()
		varStack.Push customVarStack
		customVarStack = New TStack
	End Method
	
	Method PopVarScope()
		customVarStack=TStack(varStack.Pop())
	End Method
	
	Method PushLoopLocalStack(stmt:Object)
		ind :+ 1
		If DEBUG Then
			Emit "// --> STACK = " + ind
		End If
		localScope.Push stmt
	End Method

	Method PopLoopLocalStack()
		If DEBUG Then
			Emit "// <-- STACK = " + ind
		End If
		ind :- 1
		If ind < 0 Then
			InternalErr
		End If
		localScope.Pop
	End Method

	Method LoopLocalScopeDepth:Int(findStmt:TStmt)
		Local count:Int = 0

		For Local stmt:Object = EachIn localScope
			If TBlockDecl(stmt) = Null Then
				If findStmt And TTryBreakCheck(stmt) And findStmt <> TTryBreakCheck(stmt).stmt Then
					Continue
				End If
				Exit
			End If

			count :+ 1
		Next
		
		Return count
	End Method

	Method GetTopLocalLoop:TTryBreakCheck(findStmt:TStmt)
		For Local tbc:TTryBreakCheck = EachIn localScope
			If findStmt And findStmt <> tbc.stmt Then
				Continue
			End If
			Return tbc
		Next
	End Method

	Method PushLoopTryStack(stmt:Object)
		If loopTryStack.Length() = 0 Then
			' Try statements can only be applied here to loops
			If TTryStmt(stmt) = Null Then
				loopTryStack.Push stmt
			End If
		Else
			loopTryStack.Push stmt
		End If
	End Method

	Method PopLoopTryStack()
		loopTryStack.Pop
	End Method
	
	Method LoopTryDepth:Int(findStmt:TStmt)
		Local count:Int = 0
		
		For Local stmt:Object = EachIn loopTryStack
			If TTryStmt(stmt) = Null Then
				If findStmt And findStmt <> stmt Then
					Continue
				End If
				
				Exit
			End If

			count :+ 1
		Next
		
		Return count
	End Method

	Method GetTopLoop:TTryBreakCheck(findStmt:TStmt)
		For Local tbc:TTryBreakCheck = EachIn loopTryStack
			If findStmt And findStmt <> tbc.stmt Then
				Continue
			End If
			Return tbc
		Next
	End Method

	Method TransManglePointer$( ty:TType )
		Local p:String
		
		If ty

			If ty._flags & TType.T_VAR Then
				p:+ "v"
			End If

			If ty._flags & TType.T_PTR Then
				p:+ "p"
			Else If ty._flags & TType.T_PTRPTR Then
				p:+ "pp"
			Else If ty._flags & TType.T_PTRPTRPTR Then
				p:+ "ppp"
			End If

		End If
		
		Return p
	End Method

	Method TransMangleType:String(ty:TType)
		Local p:String = TransManglePointer(ty)

		If TVoidType( ty ) Return "v"
		If TByteType( ty ) Return p + "b"
		If TShortType( ty ) Return p + "s"
		If TIntType( ty ) Return p + "i"
		If TUIntType( ty ) Return p + "u"
		If TFloatType( ty ) Return p + "f"
		If TDoubleType( ty ) Return p + "d"
		If TLongType( ty ) Return p + "l"
		If TULongType( ty ) Return p + "y"
		If TSizeTType( ty ) Return p + "z"
		If TFloat64Type( ty ) Return p + "h"
		If TFloat128Type( ty ) Return p + "k"
		If TInt128Type( ty ) Return p + "j"
		If TDouble128Type( ty ) Return p + "m"
		If TStringType( ty ) Return p + "S"
		If TArrayType( ty ) Then
			Return p + "a" + TransMangleType(TArrayType( ty ).elemType)
		End If
		If TObjectType( ty ) Then
			If Not TObjectType( ty ).classdecl.IsExtern()
				Return p + "T" + TObjectType( ty ).classDecl.ident
			Else
				If TObjectType( ty ).classdecl.IsInterface() Then
					Return p + "I" + TObjectType(ty).classDecl.ident
				ElseIf TObjectType( ty ).classdecl.IsStruct() Then
					Return p + "R" + TObjectType(ty).classDecl.ident
				Else
					Return p + "E" + TObjectType(ty).classDecl.ident
				End If
			End If
		End If
		If TFunctionPtrType( ty ) Then
			Local func:TFuncDecl = TFunctionPtrType( ty ).func
			Local s:String = "F" + MangleMethodArgs(func)
'			For Local i:Int = 0 Until func.argDecls.length
'				s :+ TransMangleType(func.argDecls[i].ty)
'			Next
			Return s + "_" + TransMangleType(func.retType) + "_"
		End If
		
		Err "Unsupported type for name mangling : " + ty.ToString()
	End Method

	Method MangleMethod:String(fdecl:TFuncDecl)
		If fdecl.IsMethod() Or fdecl.IsCtor() Then
			Return MangleMethodArgs(fdecl)
		Else
			Return MangleMethodRetType(fdecl) + MangleMethodArgs(fdecl)
		End If
	End Method
	
	Method MangleMethodRetType:String(fdecl:TFuncDecl)
		If fdecl.retType Then
			Return "_" + TransMangleType(fdecl.retType)
		Else
			Return "_v"
		End If
	End Method
	
	Method MangleMethodArgs:String(fdecl:TFuncDecl)
		Local s:String
		For Local arg:TArgDecl = EachIn fdecl.argDecls
			If Not s Then
				s = "_"
			End If
			s :+ TransMangleType(arg.ty)
		Next
		Return s
	End Method

	Method equalsTorFunc:Int(classDecl:TClassDecl, func:TFuncDecl)
		If func.IdentLower() = "new" Or func.IdentLower() = "delete" Then
			Return True
		End If
		Return False
	End Method

	Method equalsBuiltInFunc:Int(classDecl:TClassDecl, func:TFuncDecl, checked:Int = False)
		If func.equalsBuiltIn > -1 Then
			Return func.equalsBuiltIn
		End If
	
		If checked Or func.IdentLower() = "tostring" Or func.IdentLower() = "compare" Or func.IdentLower() = "sendmessage" Or func.IdentLower() = "new" Or func.IdentLower() = "delete" Then
			If classDecl.munged = "bbObjectClass" Then
				For Local decl:TFuncDecl = EachIn classDecl.Decls()
					If Not decl.IsSemanted() Then
						decl.Semant
					End If
					If decl.IdentLower() = func.IdentLower() Then
						Local res:Int = decl.EqualsFunc(func)
						If res Then
							func.equalsBuiltIn = True
						End If
						Return res
					End If
				Next
			End If
			If classDecl.superClass Then
				Return equalsBuiltInFunc(classDecl.superClass, func, True)
			End If
		End If
		func.equalsBuiltIn = False
		Return False
	End Method

	Method equalsIfcBuiltInFunc:Int(classDecl:TClassDecl, func:TFuncDecl, checked:Int = False)
		If checked Or func.IdentLower() = "new" Or func.IdentLower() = "delete" Then
			If classDecl.munged = "bbObjectClass" Then
				For Local decl:TFuncDecl = EachIn classDecl.Decls()
					If Not decl.IsSemanted() Then
						decl.Semant
					End If
					If decl.IdentLower() = func.IdentLower() Then
						Return decl.EqualsFunc(func)
					End If
				Next
			End If
			If classDecl.superClass Then
				Return equalsIfcBuiltInFunc(classDecl.superClass, func, True)
			End If
		End If
		Return False
	End Method

	Method MungFuncDecl( fdecl:TFuncDecl )

		If fdecl.munged Return
		
		Local funcs:TFuncDeclList=TFuncDeclList(funcMungs.ValueForKey( fdecl.ident ))
		If funcs
			For Local tdecl:TFuncDecl=EachIn funcs
				If fdecl.EqualsArgs( tdecl )
					fdecl.munged=tdecl.munged
					Return
				EndIf
			Next
		Else
			funcs=New TFuncDeclList
			funcMungs.Insert fdecl.ident,funcs
		EndIf

		If fdecl.scope Then
			fdecl.munged = fdecl.scope.munged + "_" + fdecl.ident
			
			If Not equalsBuiltInFunc(fdecl.classScope(), fdecl) And Not fdecl.noMangle Then
				fdecl.munged :+ MangleMethod(fdecl)
			End If
			
			' fields are lowercase with underscore prefix.
			' a function pointer with FUNC_METHOD is a field function pointer.
			'If TFieldDecl(fdecl) Or (TFuncDecl(decl) And (decl.attrs & FUNC_METHOD) And (decl.attrs & FUNC_PTR)) Then
			'	munged = "_" + munged.ToLower()
			'End If
		Else
			fdecl.munged="bb_"+fdecl.ident
		End If
		
		funcs.AddLast fdecl
	End Method
	
	Method MungDecl( decl:TDecl, addIfNotInScope:Int = False )

		If decl.munged Then
			' ensure function args get into local scope correctly.
			If addIfNotInScope Then
				If Not mungScope.Contains( decl.munged ) Then
					mungScope.Insert(decl.munged, decl)
				End If
			End If
			Return
		End If

		Local fdecl:TFuncDecl=TFuncDecl( decl )
		
		' apply mangling to methods and New (ctors)
		' but don't apply mangling to function pointers
		If fdecl And fdecl.ClassScope() And Not (fdecl.attrs & FUNC_PTR)
			MungFuncDecl( fdecl )
			Return
		End If
		
		Local id$=decl.ident,munged$
		
		'this lot just makes output a bit more readable...
'		Select ENV_LANG
'		Case "js"
'			If TModuleDecl( decl.scope ) Or TGlobalDecl( decl ) Or (fdecl And Not fdecl.IsMethod())
'				munged=decl.ModuleScope().munged+"_"+id
'			EndIf
'		Case "as"
'			If TModuleDecl( decl.scope )
'				munged=decl.ModuleScope().munged+"_"+id
'			EndIf
'		Case "cs"
'			If TClassDecl( decl )
'				munged=decl.ModuleScope().munged+"_"+id
'			EndIf
'		Case "java"
'			If TClassDecl( decl )
'				munged=decl.ModuleScope().munged+"_"+id
'			EndIf
'		Case "cpp"
			If TModuleDecl( decl.scope )
				munged=decl.ModuleScope().munged+"_"+id
			EndIf

			If TModuleDecl( decl )
				munged=decl.ModuleScope().munged+"_"+id
			EndIf

'		End Select
'DebugStop

		If Not munged
			If TLocalDecl( decl )
				munged="bbt_"+id
			Else
				If decl.scope Then
					munged = decl.scope.munged + "_" + id
					
					' fields are lowercase with underscore prefix.
					' a function pointer with FUNC_METHOD is a field function pointer.
					If TFieldDecl(decl) Or (TFuncDecl(decl) And (decl.attrs & FUNC_METHOD) And (decl.attrs & FUNC_PTR)) Then
						munged = "_" + munged.ToLower()
					End If
				Else
					munged="bb_"+id
				End If
			EndIf
		EndIf

		'sanitize non-mung-able characters
		munged = TStringHelper.Sanitize(munged)


		'add an increasing number to identifier if already used  
		If mungScope.Contains( munged )
			Local i:Int=1
			Repeat
				i:+1
			Until Not mungScope.Contains( munged + i )
			munged :+ i
		EndIf

		mungScope.Insert(munged, decl)
		decl.munged=munged
		
		' a function pointers' real function is stored in "func" - need to set its munged to match the parent.
		If TValDecl(decl) Then
			If TFunctionPtrType(TValDecl(decl).ty) Then
				TFunctionPtrType(TValDecl(decl).ty).func.munged = munged
			End If
		End If
		
	End Method

Rem
	Method MungDecl( decl:TDecl )

		If decl.munged Return

		Local fdecl:TFuncDecl=TFuncDecl( decl )
		If fdecl And fdecl.IsMethod() Then
'			DebugStop
			MungFuncDecl( fdecl )
			Return
		End If
		
		Local id:String=decl.ident,munged$,scope$
		
		If TLocalDecl( decl )
			scope="$"
			munged="t_"+id
		Else If TClassDecl( decl )
			scope=""
			munged="c_"+id
		Else If TModuleDecl( decl )
			scope=""
			munged="bb_"+id
		Else If TClassDecl( decl.scope )
			scope=decl.scope.munged
			munged="m_"+id
		Else If TModuleDecl( decl.scope )
			'If ENV_LANG="cs" Or ENV_LANG="java"
			'	scope=decl.scope.munged
			'	munged="g_"+id
			'Else
				scope=""
				munged=decl.scope.munged+"_"+id
			'EndIf
		Else
			InternalErr
		EndIf
		
		Local set:TMap=TMap(mungedScopes.ValueForKey( scope ))
		If set
			If set.Contains( munged.ToLower() )
				Local id:Int=1
				Repeat
					id :+ 1
					Local t$=munged+id
					If set.Contains( t.ToLower() ) Continue
					munged=t
					Exit
				Forever
			End If
		Else
			If scope="$"
				Print "OOPS2"
				InternalErr
			EndIf
			set=New TMap
			mungedScopes.Insert scope,set
		End If
		set.Insert munged.ToLower(), ""
		decl.munged=munged
	End Method
End Rem



	
	Method Bra$( str$ )
		If str.StartsWith( "(" ) And str.EndsWith( ")" )
			Local n:Int=1
			For Local i:Int=1 Until str.Length-1
				Select str[i..i+1]
				Case "("
					n:+1
				Case ")"
					n:-1
					If Not n Return "("+str+")"
				End Select
			Next
			If n=1 Return str
'			If str.FindLast("(")<str.Find(")") Return str
		EndIf
		Return "("+str+")"
	End Method
	
	'Utility C/C++ style...
	Method Enquote$( str$ )
		Return LangEnquote( str )
	End Method
	
	Method EscapeChars:String(str:String)
		If str Then
			Local found:Int = False
			For Local i:Int = 0 Until str.length
				If str[i] > 127 Then
					found = True
					Exit
				End If
			Next
			
			If Not found Then
				Return str
			End If
		
			Local s:String
			
			For Local i:Int = 0 Until str.length
				Local char:Int = str[i]
				
				If char < 128 Then
					s :+ Chr(char)
				Else
					s :+ "~~" + char + "~~"
				End If
			Next
			
			Return s
		End If
	End Method

	Method TransUnaryOp$( op$ )
		Select op
		Case "+" Return "+"
		Case "-" Return "-"
		Case "~~" Return op
		Case "not" Return "!"
		End Select
		InternalErr
	End Method
	
	Method TransBinaryOp$( op$,rhs$ )
'DebugLog "TransBinaryOp '" + op + "' : '" + rhs + "'"
		Select op
		Case "+","-"
			If rhs.StartsWith( op ) Return op+" "
			Return op
		Case "*","/" Return op
		Case "shl" Return "<<"
		Case "shr" Return ">>"
		Case "sar" Return ">>"
		Case "mod" Return " % "
		Case "and" Return " && "
		Case "or" Return " || "
		Case "=" Return "=="
		Case "<>" Return "!="
		Case "<","<=",">",">=" Return op
		Case "=<" Return "<="
		Case "=>" Return ">="
		Case "&","|" Return op
		Case "~~" Return "^"
		Case "<<", ">>" Return Op
		Case "%" Return Op
		End Select
		InternalErr
	End Method
	
	Method TransAssignOp$( op$ )
		Select op
		Case "mod=" Return "%="
		Case "shl=" Return "<<="
		Case "shr=" Return ">>="
		Case "sar=" Return ">>="
		End Select
		Return op
	End Method
	
	Method ExprPri:Int( expr:TExpr )
		'
		'1=primary,
		'2=postfix
		'3=prefix
		'
		If TNewObjectExpr( expr )
			Return 3
		Else If TUnaryExpr( expr )
			Select TUnaryExpr( expr ).op
			Case "+","-","~~","not" Return 3
			End Select
			InternalErr
		Else If TBinaryExpr( expr )
			Select TBinaryExpr( expr ).op
			Case "^" Return 4
			Case "*","/","mod","%" Return 5
			Case "+","-" Return 6
			Case "shl","shr", "sar","<<", ">>" Return 7
			Case "<","<=",">",">=", "=<", "=>" Return 8
			Case "=","<>" Return 9
			Case "&" Return 10
			Case "~~" Return 11
			Case "|" Return 12
			Case "and" Return 13
			Case "or" Return 14
			End Select
			InternalErr
		EndIf
		Return 2
	End Method
	
	Method TransSubExpr$( expr:TExpr,pri:Int=2 )
		Local t_expr$=expr.Trans()
		'If expr.exprType._flags & TTYPE.T_VAR Then
		'	t_expr = Bra("*" + t_expr)
		'End If
		If ExprPri( expr )>pri t_expr=Bra( t_expr )
		Return t_expr
	End Method
	
	Method TransExprNS$( expr:TExpr )
		If TVarExpr( expr ) Return expr.Trans()
		If TConstExpr( expr ) Return expr.Trans()
		Return CreateLocal( expr )
	End Method
	
	Method CreateLocal$( expr:TExpr )
		Local tmp:TLocalDecl=New TLocalDecl.Create( "",expr.exprType,expr, True )
		MungDecl tmp
		Emit TransLocalDecl( tmp,expr, True )+";"

		EmitGDBDebug(_errInfo)
		
		Return tmp.munged
	End Method

	'***** Utility *****

	Method TransLocalDecl$( decl:TLocalDecl,init:TExpr, declare:Int = False ) Abstract

	Method TransGlobalDecl$( gdecl:TGlobalDecl ) Abstract
	
	Method EmitPushErr()
	End Method
	
	Method EmitSetErr( errInfo$ )
	End Method
	
	Method EmitPopErr()
	End Method
	
	'***** Declarations *****
	
	Method TransGlobal$( decl:TGlobalDecl ) Abstract
	
	Method TransField$( decl:TFieldDecl,lhs:TExpr ) Abstract
	
	Method TransFunc$( decl:TFuncDecl,args:TExpr[],lhs:TExpr, sup:Int = False, scope:TScopeDecl = Null ) Abstract
	
	Method TransSuperFunc$( decl:TFuncDecl,args:TExpr[], scope:TScopeDecl ) Abstract
	
	
	'***** Expressions *****
	
	Method TransConstExpr$( expr:TConstExpr ) Abstract
	
	Method TransNewObjectExpr$( expr:TNewObjectExpr ) Abstract
	
	Method TransNewArrayExpr$( expr:TNewArrayExpr ) Abstract
	
	Method TransSelfExpr$( expr:TSelfExpr ) Abstract
	
	Method TransCastExpr$( expr:TCastExpr ) Abstract
	
	Method TransUnaryExpr$( expr:TUnaryExpr ) Abstract
	
	Method TransBinaryExpr$( expr:TBinaryExpr ) Abstract
	
	Method TransIndexExpr$( expr:TIndexExpr ) Abstract
	
	Method TransSliceExpr$( expr:TSliceExpr ) Abstract
	
	Method TransArrayExpr$( expr:TArrayExpr ) Abstract
	
	Method TransArraySizeExpr$ ( expr:TArraySizeExpr ) Abstract
	
	Method TransIntrinsicExpr$( decl:TDecl,expr:TExpr,args:TExpr[]=Null ) Abstract
	
	Method TransArgs$( args:TExpr[],decl:TFuncDecl, objParam:String = Null ) Abstract

	Method EmitDebugEnterScope(block:TBlockDecl) Abstract
	
	Method EmitLocalDeclarations(decl:TScopeDecl, v:TValDecl = Null) Abstract
	
	Method TransType$( ty:TType, ident:String, fpReturnTypeFunctionArgs:String = Null) Abstract
	
	Method BeginLocalScope()
		mungStack.Push mungScope
		mungScope:TMap=New TMap'<TDecl>
'		mungedScopes.Insert "$",New TMap
		
		If opt_debug Then
			localScopeStack.Push localScope
			localScope = New TStack
		End If
	End Method
	
	Method EndLocalScope()
		mungScope=TMap(mungStack.Pop())
'		mungedScopes.Insert "$",Null

		If opt_debug Then
			localScope = TStack(localScopeStack.Pop())
		End If
	End Method

Rem	
	Method MungMethodDecl( fdecl:TFuncDecl )

		If fdecl.munged Return
		
		If fdecl.overrides
			MungMethodDecl fdecl.overrides
			fdecl.munged=fdecl.overrides.munged
			Return
		EndIf
		
		Local funcs:=funcMungs.Get( fdecl.ident )
		If funcs
			For Local tdecl:=EachIn funcs
				If fdecl.EqualsArgs( tdecl )
					fdecl.munged=tdecl.munged
					Return
				EndIf
			Next
		Else
			funcs=New FuncDeclList
			funcMungs.Set fdecl.ident,funcs
		EndIf
		
		Local id:=fdecl.ident
		If mungedFuncs.Contains( id )
			Local n:=1
			Repeat
				n+=1
				id=fdecl.ident+String(n)
			Until Not mungedFuncs.Contains( id )
		EndIf
		
		mungedFuncs.Set id,fdecl
		fdecl.munged="p_"+id
		funcs.AddLast fdecl
	End
End Rem	
	'***** Simple statements *****
	
	'Expressions
	Method TransStmtExpr$( expr:TStmtExpr )
		Local t$=expr.stmt.Trans()
		If t Emit t+";"
		Return expr.expr.Trans()
	End Method
	
	Method TransTemplateCast$( ty:TType,src:TType,expr$ )
		Return expr
	End Method
	
	Method TransVarExpr$( expr:TVarExpr )
		Local decl:TVarDecl=TVarDecl( expr.decl.actual )

		If decl.munged.StartsWith( "$" ) Return TransIntrinsicExpr( decl,Null )
		
		If TLocalDecl( decl ) Then
			If decl.ty._flags & TType.T_VAR Then
				Return "*" + decl.munged
			Else
				Return decl.munged
			End If
		End If
		
		If TFieldDecl( decl ) Return TransField( TFieldDecl( decl ),Null )
		
		If TGlobalDecl( decl ) Return TransGlobal( TGlobalDecl( decl ) )
		
		InternalErr
	End Method
	
	Method TransMemberVarExpr$( expr:TMemberVarExpr )
		Local decl:TVarDecl=TVarDecl( expr.decl.actual )
		
		If decl.munged.StartsWith( "$" ) Return TransIntrinsicExpr( decl,expr.expr )
		
		If TFieldDecl( decl ) Return TransField( TFieldDecl( decl ),expr.expr )

		InternalErr
	End Method
	
	Method TransInvokeExpr$( expr:TInvokeExpr )
		Local decl:TFuncDecl=TFuncDecl( expr.decl.actual ),t$
'If decl.ident = "OnDebugStop" DebugStop	
		If Not decl.munged Then
			MungDecl decl
		End If

		If (decl.attrs & FUNC_PTR) And (decl.attrs & FUNC_INIT) And Not expr.InvokedWithBraces Return decl.munged
		
		If ((decl.attrs & FUNC_PTR) Or (expr.decl.attrs & FUNC_PTR)) And Not expr.InvokedWithBraces Return decl.munged
		
		If Not expr.InvokedWithBraces And expr.IsRhs And Not expr.args Return decl.munged
		
		If decl.munged.StartsWith( "$" ) Return TransIntrinsicExpr( decl,Null,expr.args )
		
		If processingReturnStatement = 1 Then
			If decl Then
				processingReturnStatement :+ 1
				Return CreateLocal(expr)
			End If
		Else
			If decl Return TransFunc( TFuncDecl(decl),expr.args,Null )
		End If
		
		InternalErr
	End Method
	
	Method TransInvokeMemberExpr$( expr:TInvokeMemberExpr )
		Local decl:TFuncDecl=TFuncDecl( expr.decl.actual ),t$

		If decl.munged.StartsWith( "$" ) Return TransIntrinsicExpr( decl,expr.expr,expr.args )
		
		If processingReturnStatement = 1 Then
			If decl Then
				processingReturnStatement :+ 1
				Return CreateLocal(expr)
			End If
		Else
			If decl Return TransFunc( TFuncDecl(decl),expr.args,expr.expr )	
		End If
		
		InternalErr
	End Method
	
	Method TransInvokeSuperExpr$( expr:TInvokeSuperExpr )
		Local decl:TFuncDecl=TFuncDecl( expr.origFuncDecl.actual ),t$

		If decl.munged.StartsWith( "$" ) Return TransIntrinsicExpr( decl,expr )
		
		If processingReturnStatement = 1 Then
			If decl Then
				processingReturnStatement :+ 1
				Return CreateLocal(expr)
			End If
		Else
			If decl Return TransSuperFunc( TFuncDecl( expr.funcDecl ),expr.args, expr.classScope )
		End If
		
		InternalErr
	End Method
	
	Method TransFuncCallExpr:String( expr:TFuncCallExpr )

		If TIndexExpr(expr.expr) And TArrayType(TIndexExpr(expr.expr).expr.exprType) And TFunctionPtrType(TArrayType(TIndexExpr(expr.expr).expr.exprType).elemType) Then
			Local decl:TDecl = TFunctionPtrType(TArrayType(TIndexExpr(expr.expr).expr.exprType).elemType).func.actual
			decl.Semant()
			expr.args=expr.CastArgs( expr.args,TFuncDecl(decl) )
			Return expr.expr.Trans() + TransArgs(expr.args, TFuncDecl(decl))
		End If
		
		InternalErr
	End Method
	
	Method TransExprStmt$( stmt:TExprStmt )
		Return stmt.expr.TransStmt()
	End Method
	
	Method TransAssignStmt$( stmt:TAssignStmt )
		If stmt.rhs Return stmt.lhs.TransVar()+TransAssignOp(stmt.op)+stmt.rhs.Trans()
		Return stmt.lhs.Trans()
	End Method
	
	Method TransReturnStmt$( stmt:TReturnStmt )

		Local t$="return"
		unreachable=True
		If stmt.expr Then

			If TObjectType(stmt.expr.exprType) And TNullDecl(TObjectType(stmt.expr.exprType).classDecl) Then
				If IsPointerType(stmt.fRetType, 0, TType.T_POINTER) Or IsNumericType(stmt.fRetType) Then
					t:+ " 0"
				End If
				If TStringType(stmt.fRetType) Then
					t:+ " &bbEmptyString"
				End If
				If TArrayType(stmt.fRetType) Then
					t:+ " &bbEmptyArray"
				End If

			Else
				
				Local s:String = stmt.expr.Trans()
				
				' we have some temp variables that need to be freed before we can return
				' put the results into a new variable, and return that.
				If customVarStack.Count() > 0 Then
					If Not TFunctionPtrType( stmt.expr.exprType ) Then
						Emit TransType(stmt.expr.exprType, "rt_") + " rt_ = " + s + ";"
					Else
						Emit TransType(stmt.expr.exprType, "rt_") + " = " + s + ";"
					End If
					t:+ " rt_"
				Else
					t:+" " + s
				End If
			End If
			
		End If

		FreeVarsIfRequired()
		
		' if this is a Delete() method, we need to call the dtor first
		Local funcScope:TFuncDecl = _env.FuncScope()
		If funcScope And funcScope.IdentLower() = "delete" Then
			Local classScope:TClassDecl = funcScope.ClassScope()
			If classScope Then
				EmitClassDeclDeleteDtor(classScope)
			End If
		End If
		
		Return t
	End Method
	
	Method NextExitId:Int(bc:TTryBreakCheck)
		If Not bc.exitId Then
			exitLabelId :+ 1
			bc.exitId = exitLabelId
		End If
		
		Return bc.exitId
	End Method

	Method NextContId:Int(bc:TTryBreakCheck)
		If Not bc.contId Then
			contLabelId :+ 1
			bc.contId = contLabelId
		End If
		
		Return bc.contId
	End Method

	Method TransContinueStmt$( stmt:TContinueStmt )
		unreachable=True

		Local contLoop:TStmt
		' if we are continuing with a loop label, we'll need to find it in the stack
		If stmt.label And TLoopLabelExpr(stmt.label) Then
			contLoop = TLoopLabelExpr(stmt.label).loop
		End If
		' get count of Try statements in the stack in this loop
		Local count:Int = LoopTryDepth(contLoop)
		If count > 0 Then
			Local bc:TTryBreakCheck = GetTopLoop(contLoop)
			If bc Then
				NextContId(bc)
				For Local i:Int = 0 Until count
					Emit "bbExLeave();"
					If opt_debug Then
						Emit "bbOnDebugPopExState();"
					End If
				Next
				Emit "goto " + TransLabelCont(bc, False)
			Else
				InternalErr
			End If
		Else
		 	' For debug builds, we need to rollback the local scope stack correctly
			count = 0
			
			If opt_debug And TLoopStmt(contLoop) And Not TLoopStmt(contLoop).block.IsNoDebug() Then
				count = LoopLocalScopeDepth(contLoop)
			End If
			
			If count > 0 Then
				Local bc:TTryBreakCheck = GetTopLocalLoop(contLoop)
				If bc Then
					NextContId(bc)
					For Local i:Int = 0 Until count
						Emit "bbOnDebugLeaveScope();"
					Next
					Emit "goto " + TransLabelCont(bc, False)
				Else
					InternalErr
				End If
			Else

				If opt_debug And stmt.loop And Not stmt.loop.block.IsNoDebug() Then
					count = LoopLocalScopeDepth(Null)
				End If
				For Local i:Int = 0 Until count
					Emit "bbOnDebugLeaveScope();"
				Next

				' No Try statements in the stack here..
				If stmt.label And TLoopLabelExpr(stmt.label) Then
					Emit "goto " + TransLoopLabelCont(TLoopLabelExpr(stmt.label).loop.loopLabel.realIdent, False)
				Else
					Return "continue"
				End If
			End If
		End If
	End Method
	
	Method TransBreakStmt$( stmt:TBreakStmt )
		unreachable=True
		broken:+1

		Local brkLoop:TStmt
		' if we are exiting with a loop label, we'll need to find it in the stack
		If stmt.label And TLoopLabelExpr(stmt.label) Then
			brkLoop = TLoopLabelExpr(stmt.label).loop
		End If
		' get count of Try statements in the stack in this loop
		Local count:Int = LoopTryDepth(brkLoop)
		If count > 0 Then
			Local bc:TTryBreakCheck = GetTopLoop(brkLoop)
			If bc Then
				NextExitId(bc)
				For Local i:Int = 0 Until count
					Emit "bbExLeave();"
					If opt_debug Then
						Emit "bbOnDebugPopExState();"
					End If
				Next
				Emit "goto " + TransLabelExit(bc, False)
			Else
				InternalErr
			End If
		Else
		 	' For debug builds, we need to rollback the local scope stack correctly
			count = 0
			
			If opt_debug And TLoopStmt(brkLoop) And Not TLoopStmt(brkLoop).block.IsNoDebug() Then
				count = LoopLocalScopeDepth(brkLoop)
			End If
			
			If count > 0 Then
				Local bc:TTryBreakCheck = GetTopLocalLoop(brkLoop)
				If bc Then
					NextExitId(bc)
					For Local i:Int = 0 Until count
						Emit "bbOnDebugLeaveScope();"
					Next
					Emit "goto " + TransLabelExit(bc, False)
				Else
					InternalErr
				End If
			Else

				If opt_debug And stmt.loop And Not stmt.loop.block.IsNoDebug() Then
					count = LoopLocalScopeDepth(Null)
				End If
				For Local i:Int = 0 Until count
					Emit "bbOnDebugLeaveScope();"
				Next
				
				' No Try statements in the stack here..
				If stmt.label And TLoopLabelExpr(stmt.label) Then
					Emit "goto " + TransLoopLabelExit(TLoopLabelExpr(stmt.label).loop.loopLabel.realIdent, False)
				Else
					Return "break"
				End If
			End If
		End If
	End Method
	
	Method TransTryStmt$( stmt:TTryStmt )
	End Method
	
	Method EmitTryStack() Abstract

	Method TransThrowStmt$( stmt:TThrowStmt )
	End Method
	

	Method TransBuiltinExpr$( expr:TBuiltinExpr )
		If TMinExpr(expr) Return TransMinExpr(TMinExpr(expr))
		If TMaxExpr(expr) Return TransMaxExpr(TMaxExpr(expr))
		If TAbsExpr(expr) Return TransAbsExpr(TAbsExpr(expr))
		If TAscExpr(expr) Return TransAscExpr(TAscExpr(expr))
		If TChrExpr(expr) Return TransChrExpr(TChrExpr(expr))
		If TSgnExpr(expr) Return TransSgnExpr(TSgnExpr(expr))
		If TLenExpr(expr) Return TransLenExpr(TLenExpr(expr))
		If TSizeOfExpr(expr) Return TransSizeOfExpr(TSizeOfExpr(expr))
		Err "TODO : TransBuiltinExpr()"
	End Method
	
	Method TransMinExpr:String(expr:TMinExpr)
	End Method

	Method TransMaxExpr:String(expr:TMaxExpr)
	End Method

	Method TransAbsExpr:String(expr:TAbsExpr)
	End Method

	Method TransAscExpr:String(expr:TAscExpr)
	End Method

	Method TransChrExpr:String(expr:TChrExpr)
	End Method

	Method TransSgnExpr:String(expr:TSgnExpr)
	End Method

	Method TransLenExpr:String(expr:TLenExpr)
	End Method

	Method TransSizeOfExpr:String(expr:TSizeOfExpr)
	End Method
	
	Method TransIdentTypeExpr:String(expr:TIdentTypeExpr) Abstract
	
	Method TransLabelCont:String(bc:TTryBreakCheck, jmp:Int = True)
		If jmp Then
			Return "_contjmp" + bc.contId + ": ;"
		Else
			Return "_contjmp" + bc.contId + ";"
		End If
	End Method
	
	Method TransLabelExit:String(bc:TTryBreakCheck, jmp:Int = True)
		If jmp Then
			Return "_exitjmp" + bc.exitId + ": ;"
		Else
			Return "_exitjmp" + bc.exitId + ";"
		End If
	End Method

	Method TransLoopLabelCont:String(id:String, jmp:Int = True)
		If jmp Then
			Return "_loopcont_" + id.ToLower() + ": ;"
		Else
			Return "_loopcont_" + id.ToLower() + ";"
		End If
	End Method

	Method TransLoopLabelExit:String(id:String, jmp:Int = True)
		If jmp Then
			Return "_loopexit_" + id.ToLower() + ": ;"
		Else
			Return "_loopexit_" + id.ToLower() + ";"
		End If
	End Method

	'***** Block statements - all very C like! *****
	
	Method Emit( t$, useIndent:Int = True )
		If Not t Return
		If useIndent
			If t.StartsWith( "}" )
				indent=indent[..indent.Length-1]
			EndIf
		End If
		LINES.AddLast indent+t
		'code+=indent+t+"~n"
		If useIndent
			If t.EndsWith( "{" )
				indent:+"~t"
			EndIf
		End If
	End Method
	
	Method JoinLines$( file:String )
		Local _lines:TStringList = TStringList(outputFiles.ValueForKey(file))
		
		Local code$=_lines.Join( "~n" )
		_lines.Clear
		Return code
	End Method
	
	'returns unreachable status!
	Method EmitBlock:Int( block:TBlockDecl )
		Local stmtCount:Int
'DebugStop
		'If ENV_CONFIG="debug"
		'	If TFuncDecl( block ) EmitPushErr
		'EndIf


		PushEnv block
		
		' enter scope
		If opt_debug And Not block.IsNoDebug() And Not block.generated Then
			PushLoopLocalStack(block)
			EmitDebugEnterScope(block)
		End If

		For Local stmt:TStmt=EachIn block.stmts
		
			_errInfo=stmt.errInfo
			
			If unreachable
				' Statements following cannot be reached - maybe we have Returned, etc
				' So don't process any more for this block - they won't be generated now!
				Exit
			EndIf

Rem
			If ENV_CONFIG="debug"
				Local rs:TReturnStmt=TReturnStmt( stmt )
				If rs
					If rs.expr
						EmitSetErr stmt.errInfo
						Local t_expr$=TransExprNS( rs.expr )
						EmitPopErr
						Emit "return "+t_expr+";"
					Else
						EmitPopErr
						Emit "return;"
					EndIf
					unreachable=True
					Continue
				EndIf
				EmitSetErr stmt.errInfo
			EndIf
End Rem
			If opt_debug And Not block.IsNoDebug() Then
				' only for user-made code
				If Not stmt.generated Then
					EmitDebugStmtErrInfo(stmt.errInfo, stmtCount)
					stmtCount :+ 1
				End If
			
			End If

			EmitGDBDebug(stmt)
			
			If TReturnStmt(stmt) And Not tryStack.IsEmpty() Then
				processingReturnStatement = True
			End If
			
			Local t$=stmt.Trans()
			
			processingReturnStatement = False
			
			If opt_debug And Not block.IsNoDebug() Then
				If TReturnStmt(stmt) Then
					For Local b:TBlockDecl = EachIn localScope
						Emit "bbOnDebugLeaveScope();"
					Next
				End If
			End If

			If TReturnStmt(stmt) Then
				EmitTryStack()
			End If
			
			If t Emit t+";"

			If DEBUG And debugOut Then
				Emit debugOut
				debugOut = Null
			End If
			
			Local v:String = String(customVarStack.Pop())
			While v
				Emit "bbMemFree" + Bra(v) + ";"
				v = String(customVarStack.Pop())
			Wend
			
		Next

		If opt_debug And Not block.IsNoDebug() And Not block.generated Then
			PopLoopLocalStack()
			If Not unreachable Then
				Emit "bbOnDebugLeaveScope();"
			End If
		End If

		Local r:Int=unreachable
		unreachable=False
		PopEnv
		Return r
	End Method
	
	Method TransDeclStmt$( stmt:TDeclStmt, declare:Int = False )
		Local decl:TLocalDecl=TLocalDecl( stmt.decl )
		If decl
			MungDecl decl
			' only generate local declarations once.
			If decl.generated Then
				If Not decl.done Then
					decl.done = True
				Else
					Return ""
				End If
			End If
			Return TransLocalDecl( decl,decl.init, decl.generated Or declare )
		EndIf
		Local cdecl:TConstDecl=TConstDecl( stmt.decl )
		If cdecl
			Return Null
		EndIf
		Local gdecl:TGlobalDecl=TGlobalDecl( stmt.decl )
		If gdecl Then
			MungDecl gdecl
			If gdecl.inited Return Null
			Return TransGlobalDecl( gdecl )
		End If
		InternalErr
	End Method
	
	Method TransIfStmt$( stmt:TIfStmt )
		If TConstExpr( stmt.expr )
			If TConstExpr( stmt.expr ).value
				Emit "{"
				EmitLocalDeclarations(stmt.thenBlock)
				If EmitBlock( stmt.thenBlock ) unreachable=True
				Emit "}"
			Else If stmt.elseBlock.stmts.First()
				Emit "{"
				EmitLocalDeclarations(stmt.elseBlock)
				If EmitBlock( stmt.elseBlock ) unreachable=True
				Emit "}"
			EndIf
		Else If stmt.elseBlock.stmts.First()
			_inBinary :+ 1
			Emit "if"+Bra( stmt.expr.Trans() )+"{"
			_inBinary :- 1
			EmitLocalDeclarations(stmt.thenBlock)
			FreeVarsIfRequired(False)
			PushVarScope
			Local unr:Int=EmitBlock( stmt.thenBlock )
			PopVarScope
			Emit "}else{"
			EmitLocalDeclarations(stmt.elseBlock)
			FreeVarsIfRequired
			Local unr2:Int=EmitBlock( stmt.elseBlock )
			Emit "}"
			If unr And unr2 unreachable=True
		Else

'			Emit "if"+ Bra(TransCondition(stmt.expr)) + "{"
'			If TVarExpr(stmt.expr) Then
'				If TObjectType(TVarExpr(stmt.expr).exprType) Then
'					Emit "if"+Bra( stmt.expr.Trans() + "!= &bbNullObject") + "{"
'				Else If TStringType(TVarExpr(stmt.expr).exprType)  Then
'					Emit "if"+Bra( stmt.expr.Trans() + "!= &bbEmptyString") + "{"
'				Else
'					Emit "if"+Bra( stmt.expr.Trans() )+"{"
'				End If
'			Else
			_inBinary :+ 1
				Emit "if"+Bra( stmt.expr.Trans() )+"{"
				FreeVarsIfRequired(False)
			_inBinary :- 1
'			End If
			EmitLocalDeclarations(stmt.thenBlock)
			PushVarScope
			Local unr:Int=EmitBlock( stmt.thenBlock )
			PopVarScope
			Emit "}"
			FreeVarsIfRequired
		EndIf
	End Method
	
	Method FreeVarsIfRequired(removeFromStack:Int = True)
		If removeFromStack
			Local v:String = String(customVarStack.Pop())
			While v
				Emit "bbMemFree" + Bra(v) + ";"
				v = String(customVarStack.Pop())
			Wend
		Else
			For Local v:String = EachIn customVarStack
				Emit "bbMemFree" + Bra(v) + ";"
			Next
		End If
	End Method
	
'	Method TransCondition:String(expr:TExpr)
'		If TVarExpr(expr) Then
'			If TObjectType(TVarExpr(expr).exprType) Then
'				Return Bra( expr.Trans() + "!= &bbNullObject")
'			Else If TStringType(TVarExpr(expr).exprType)  Then
'				Return Bra( expr.Trans() + "!= &bbEmptyString")
'			Else
'				Return Bra( expr.Trans() )
'			End If
'		Else
'			Return Bra( expr.Trans() )
'		End If
'	End Method
	
	Method TransWhileStmt$( stmt:TWhileStmt )
		Local nbroken:Int=broken

		_inBinary :+ 1
		Emit "while"+Bra( stmt.expr.Trans() )+"{"
		_inBinary :- 1
		
		Local check:TTryBreakCheck = New TTryBreakCheck
		check.stmt = stmt
		PushLoopTryStack(check)
		If opt_debug And Not stmt.block.IsNoDebug() And Not stmt.block.generated Then
			PushLoopLocalStack(check)
		End If
		EmitLocalDeclarations(stmt.block)
		Local unr:Int=EmitBlock( stmt.block )
		If opt_debug And Not stmt.block.IsNoDebug() And Not stmt.block.generated Then
			PopLoopLocalStack
		End If
		PopLoopTryStack
		
		If check.contId Then
			Emit TransLabelCont(check)
		End If

		If stmt.loopLabel Then
			Emit TransLoopLabelCont(stmt.loopLabel.realIdent, True)
		End If
		
		Emit "}"

		If check.exitId Then
			Emit TransLabelExit(check)
		End If

		If stmt.loopLabel Then
			Emit TransLoopLabelExit(stmt.loopLabel.realIdent, True)
		End If
		
		If broken=nbroken And TConstExpr( stmt.expr ) And TConstExpr( stmt.expr ).value unreachable=True
		broken=nbroken
	End Method

	Method TransRepeatStmt$( stmt:TRepeatStmt )
		Local nbroken:Int=broken

		SetOutputTemp()

		Emit "do{"
		
		Local check:TTryBreakCheck = New TTryBreakCheck
		check.stmt = stmt
		PushLoopTryStack(check)
		If opt_debug And Not stmt.block.IsNoDebug() And Not stmt.block.generated Then
			PushLoopLocalStack(check)
		End If
		EmitLocalDeclarations(stmt.block)
		Local unr:Int=EmitBlock( stmt.block )
		If opt_debug And Not stmt.block.IsNoDebug() And Not stmt.block.generated Then
			PopLoopLocalStack
		End If
		PopLoopTryStack

		If check.contId Then
			Emit TransLabelCont(check)
		End If

		If stmt.loopLabel Then
			Emit TransLoopLabelCont(stmt.loopLabel.realIdent, True)
		End If
		
		SetOutput("source")

		Local s:String = "}while(!"+Bra( stmt.expr.Trans() )+");"
		
		SetOutputTemp(True)
		
		Emit s

		If check.exitId Then
			Emit TransLabelExit(check)
		End If

		If stmt.loopLabel Then
			Emit TransLoopLabelExit(stmt.loopLabel.realIdent, True)
		End If

		If broken=nbroken And TConstExpr( stmt.expr ) And Not TConstExpr( stmt.expr ).value unreachable=True
		broken=nbroken
	End Method

	Method TransForStmt$( stmt:TForStmt )
		Local nbroken:Int=broken

		Local init$

		Local decl:Int
		Local vdecl:TValDecl
		If TDeclStmt(stmt.init) Then
			decl = True
			Emit "{"
			Emit TransDeclStmt(TDeclStmt(stmt.init), True) + ";"
			init = TDeclStmt(stmt.init).decl.munged
			vdecl = TValDecl(TDeclStmt(stmt.init).decl)
		Else
			init=stmt.init.Trans()
		End If
		Local expr$=stmt.expr.Trans()
		Local incr$=stmt.incr.Trans()

		Emit "for("+init+";"+expr+";"+incr+"){"

		Local check:TTryBreakCheck = New TTryBreakCheck
		check.stmt = stmt
		PushLoopTryStack(check)
		If opt_debug And Not stmt.block.IsNoDebug() And Not stmt.block.generated Then
			PushLoopLocalStack(check)
		End If
		EmitLocalDeclarations(stmt.block, vdecl)
		Local unr:Int=EmitBlock( stmt.block )
		If opt_debug And Not stmt.block.IsNoDebug() And Not stmt.block.generated Then
			PopLoopLocalStack
		End If
		PopLoopTryStack
		
		If check.contId Then
			Emit TransLabelCont(check)
		End If

		If stmt.loopLabel Then
			Emit TransLoopLabelCont(stmt.loopLabel.realIdent, True)
		End If
		
		Emit "}"
		
		If decl Then
			Emit "}"
		End If

		If check.exitId Then
			Emit TransLabelExit(check)
		End If

		If stmt.loopLabel Then
			Emit TransLoopLabelExit(stmt.loopLabel.realIdent, True)
		End If
		
		If broken=nbroken And TConstExpr( stmt.expr ) And TConstExpr( stmt.expr ).value unreachable=True
		broken=nbroken
	End Method

	Method TransAssertStmt$( stmt:TAssertStmt ) Abstract

	Method TransEndStmt$( stmt:TEndStmt ) Abstract

	Method TransReleaseStmt$( stmt:TReleaseStmt ) Abstract

	Method TransRestoreDataStmt$( stmt:TRestoreDataStmt ) Abstract

	Method TransReadDataStmt$( stmt:TReadDataStmt ) Abstract

	'module
	Method TransApp( app:TAppDecl ) Abstract

Rem	
	Method MungOverrides( cdecl:TClassDecl )
		For Local decl:=Eachin cdecl.Semanted
			Local fdecl:=TFuncDecl( decl )
			If fdecl And fdecl.overrides
				If Not fdecl.overrides.munged InternalErr
				fdecl.munged=fdecl.overrides.munged
				mungScope.Insert fdecl.munged,fdecl
			Endif
		Next
	End
End Rem
	
	Method PostProcess$( source$ ) 
		Return source
	End Method
	
	Method SetOutput( file:String )
		Local _lines:TStringList = TStringList(outputFiles.ValueForKey(file))
		
		If Not _lines Then
			_lines = New TStringList
			outputFiles.Insert(file, _lines)
		End If
		
		LINES = _lines
		
	End Method

	Method SetOutputTemp( fin:Int = False )
		Global tmpLevel:Int = 0
	
		If Not fin Then
			tmpLevel :+ 1

			Local _lines:TStringList = New TStringList
			outputFiles.Insert("tmp" + tmpLevel, _lines)
	
			LINES = _lines
		Else
			Local _lines:TStringList = TStringList(outputFiles.ValueForKey("tmp" + tmpLevel))
			
			tmpLevel :- 1
		
			If Not tmpLevel Then
				SetOutput("source")
			Else
				LINES = TStringList(outputFiles.ValueForKey("tmp" + tmpLevel))
			End If
			
			If _lines Then
				For Local line:String = EachIn _lines
					LINES.AddLast(line)
				Next
			End If
			
		End If
	End Method

	Method DebugPrint(text:String, func:String = Null, trans:Int = False)
		Global count:Int
		Global lastFunc:String
		
		If func Then
			lastFunc = func
		End If
		
		Local s:String = "fprintf(stderr," + "~q" + lastFunc + " : " + count + " :: " + text + "\n~q)" + ";fflush(stderr);"
		
		If trans Then
			debugOut :+ indent + s + "~n"
		Else
			Emit s
		End If
		count :+ 1
	End Method
	
	Method DebugString(s:String, func:String = Null, trans:Int = False)
		' bbNullObject test
		If trans Then
			debugOut :+ indent + "if (" + s + "==&bbNullObject) {~n"
		Else
			Emit "if (" + s + "==&bbNullObject) {"
		End If
		DebugPrint("Invalid Null String : " + s, func, trans)
		If trans Then
			debugOut :+ indent + "}~n"
		Else
			Emit "}"
		End If
	End Method

	Method DebugArray(s:String, func:String = Null, trans:Int = False)
		' bbNullObject test
		If trans Then
			debugOut :+ indent + "if (" + s + "==&bbNullObject) {~n"
		Else
			Emit "if (" + s + "==&bbNullObject) {"
		End If
		DebugPrint("Invalid Null Array : " + s, func, trans)
		If trans Then
			debugOut :+ indent + "}~n"
		Else
			Emit "}"
		End If
	End Method

	Method DebugObject(ty:TType, id:String, func:String = Null, trans:Int = False)
		If TObjectType(ty) Or TStringType(ty) Or TArrayType(ty) Then
			' null test
			If trans Then
				debugOut :+ indent + "if (" + id + "==NULL) {~n"
			Else
				Emit "if (" + id + "==NULL) {"
			End If
			DebugPrint("Null Pointer : " + id, func, trans)
			If trans Then
				If ABORT_ON_NULL Then
					debugOut :+ indent + "abort();~n"
				End If
				debugOut :+ indent + "}~n"
			Else
				If ABORT_ON_NULL Then
					Emit "abort();~n"
				End If
				Emit "}"
			End If
		End If
		
		If TStringType(ty) Then
			DebugString(id, func, trans)
		Else If TArrayType(ty) Then
			DebugArray(id, func, trans)
		End If
	End Method
	
	Method EmitDebugStmtErrInfo(info:String, count:Int)
		' extract from info
		info = info[1..info.length-1]
		Local infoArray:String[] = info.Split(";")

		Local dbg:String = "struct BBDebugStm __stmt_" + count + " = {"
		dbg :+ Enquote(infoArray[0]) + ", "
		dbg :+ infoArray[1] + ", "
		dbg :+ infoArray[2] + "};"
		Emit dbg
		Emit "bbOnDebugEnterStm(&__stmt_" + count + ");" 
	End Method
	
	Method EmitGDBDebug(obj:Object)
		If opt_gdbdebug Then
			If TStmt(obj) Then
				Local stmt:TStmt = TStmt(obj)
				Local infoArray:String[] = stmt.errInfo[1..stmt.errInfo.length-1].Split(";")
				If Not stmt.generated Then
					Emit "#line " + infoArray[1] + " " + Enquote(infoArray[0])
				End If
			Else If TDecl(obj) Then
				Local decl:TDecl = TDecl(obj)
				Local infoArray:String[] = decl.errInfo[1..decl.errInfo.length-1].Split(";")
				Emit "#line " + infoArray[1] + " " + Enquote(infoArray[0])
			Else If String(obj) Then
				Local errInfo:String = String(obj)
				Local infoArray:String[] = errInfo[1..errInfo.length-1].Split(";")
				Emit "#line " + infoArray[1] + " " + Enquote(infoArray[0])
			End If
		End If
	End Method
	
	Method EmitClassDeclDeleteDtor( classDecl:TClassDecl )
	End Method
	
End Type

Type TTryBreakCheck

	Field contId:Int
	Field exitId:Int

	Field stmt:TStmt
	
End Type
