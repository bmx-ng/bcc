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

Global _trans:TTranslator

Type TTranslator

	Field outputFiles:TMap = New TMap

	Field indent$
	Field LINES:TStringList'=New TStringList
	Field unreachable:Int,broken:Int
	
	'Munging needs a big cleanup!
	
	Field mungScope:TMap=New TMap'<TDecl>
	Field mungStack:TStack=New TStack'< StringMap<TDecl> >
	Field funcMungs:TMap=New TMap'<FuncDeclList>
	Field customVarStack:TStack = New TStack
	Field varStack:TStack = New TStack

	Field mungedScopes:TMap=New TMap'<StringSet>
'	Field funcMungs:=New StringMap<FuncDeclList>
'	Field mungedFuncs:=New StringMap<FuncDecl>

	Method PushVarScope()
		varStack.Push customVarStack
		customVarStack = New TStack
	End Method
	
	Method PopVarScope()
		customVarStack=TStack(varStack.Pop())
	End Method
	
	Method MungFuncDecl( fdecl:TFuncDecl )

		If fdecl.munged Return
		
		If fdecl.overrides
			MungFuncDecl fdecl.overrides
			fdecl.munged=fdecl.overrides.munged
			Return
		EndIf
		
		Local funcs:TFuncDeclList=TFuncDeclList(funcMungs.ValueForKey( fdecl.ident ))
		If funcs
			For Local tdecl:TFuncDecl=EachIn funcs
				If fdecl.argDecls.Length=tdecl.argDecls.Length
					Local match:Int=True
					For Local i:Int=0 Until fdecl.argDecls.Length
						Local ty:TType=TArgDecl( fdecl.argDecls[i].actual ).ty
						Local ty2:TType=TArgDecl( tdecl.argDecls[i].actual ).ty
						If ty.EqualsType( ty2 ) Continue
						match=False
						Exit
					Next
					If match
						fdecl.munged=tdecl.munged
						Return
					EndIf
				EndIf
			Next
		Else
			funcs=New TFuncDeclList
			funcMungs.Insert fdecl.ident,funcs
		EndIf
		
		fdecl.munged="bbm_"+fdecl.ident
		If Not funcs.IsEmpty() fdecl.munged:+String(funcs.Count()+1)
		funcs.AddLast fdecl
	End Method
	
	Method MungDecl( decl:TDecl )

		If decl.munged Return

		Local fdecl:TFuncDecl=TFuncDecl( decl )
		
		'If fdecl And fdecl.IsMethod() 
		'	MungFuncDecl( fdecl )
		'	Return
		'End If
		
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
				munged = munged.Replace(".", "_").Replace("-", "_")
			EndIf

'		End Select
'DebugStop
		
		If Not munged
			If TLocalDecl( decl )
				munged="bbt_"+id
			Else
				If decl.scope Then
					munged = decl.scope.munged + "_" + id
					
					If TFieldDecl(decl) Then
						munged = "_" + munged.ToLower()
					End If
				Else
					munged="bb_"+id
				End If
			EndIf
		EndIf
		
		If mungScope.Contains( munged )
			Local t$,i:Int=1
			Repeat
				i:+1
				t=munged+i
			Until Not mungScope.Contains( t )
			munged=t
		EndIf

		mungScope.Insert munged,decl
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
			Case "*","/","mod" Return 4
			Case "+","-" Return 5
			Case "shl","shr", "sar" Return 6
			Case "<","<=",">",">=", "=<", "=>" Return 7
			Case "=","<>" Return 8
			Case "&" Return 9
			Case "~~" Return 10
			Case "|" Return 11
			Case "and" Return 12
			Case "or" Return 13
			End Select
			InternalErr
		EndIf
		Return 2
	End Method
	
	Method TransSubExpr$( expr:TExpr,pri:Int=2 )
		Local t_expr$=expr.Trans()
		If ExprPri( expr )>pri t_expr=Bra( t_expr )
		Return t_expr
	End Method
	
	Method TransExprNS$( expr:TExpr )
		If TVarExpr( expr ) Return expr.Trans()
		If TConstExpr( expr ) Return expr.Trans()
		Return CreateLocal( expr )
	End Method
	
	Method CreateLocal$( expr:TExpr )
		Local tmp:TLocalDecl=New TLocalDecl.Create( "",expr.exprType,expr )
		MungDecl tmp
		Emit TransLocalDecl( tmp.munged,expr )+";"
		Return tmp.munged
	End Method

	'***** Utility *****

	Method TransLocalDecl$( munged$,init:TExpr ) Abstract

	Method TransGlobalDecl$( munged$,init:TExpr, attrs:Int ) Abstract
	
	Method EmitPushErr()
	End Method
	
	Method EmitSetErr( errInfo$ )
	End Method
	
	Method EmitPopErr()
	End Method
	
	'***** Declarations *****
	
	Method TransGlobal$( decl:TGlobalDecl ) Abstract
	
	Method TransField$( decl:TFieldDecl,lhs:TExpr ) Abstract
	
	Method TransFunc$( decl:TFuncDecl,args:TExpr[],lhs:TExpr ) Abstract
	
	Method TransSuperFunc$( decl:TFuncDecl,args:TExpr[] ) Abstract
	
	
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

	Method BeginLocalScope()
		mungStack.Push mungScope
		mungScope:TMap=New TMap'<TDecl>
'		mungedScopes.Insert "$",New TMap
	End Method
	
	Method EndLocalScope()
		mungScope=TMap(mungStack.Pop())
'		mungedScopes.Insert "$",Null
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
		
		If TLocalDecl( decl ) Return decl.munged
		
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
		If (decl.attrs & FUNC_PTR) And (decl.attrs & FUNC_INIT) Return decl.munged
		
		If decl.munged.StartsWith( "$" ) Return TransIntrinsicExpr( decl,Null,expr.args )
		
		If decl Return TransFunc( TFuncDecl(decl),expr.args,Null )
		
		InternalErr
	End Method
	
	Method TransInvokeMemberExpr$( expr:TInvokeMemberExpr )
		Local decl:TFuncDecl=TFuncDecl( expr.decl.actual ),t$

		If decl.munged.StartsWith( "$" ) Return TransIntrinsicExpr( decl,expr.expr,expr.args )
		
		If decl Return TransFunc( TFuncDecl(decl),expr.args,expr.expr )	
		
		InternalErr
	End Method
	
	Method TransInvokeSuperExpr$( expr:TInvokeSuperExpr )
		Local decl:TFuncDecl=TFuncDecl( expr.funcDecl.actual ),t$

		If decl.munged.StartsWith( "$" ) Return TransIntrinsicExpr( decl,expr )
		
		If decl Return TransSuperFunc( TFuncDecl( decl ),expr.args )
		
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
'DebugStop
			If TObjectType(stmt.expr.exprType) And TNullDecl(TObjectType(stmt.expr.exprType).classDecl) Then
				If TPointerType(stmt.fRetType) Or TNumericType(stmt.fRetType) Then
					Return t + " 0"
				End If
				If TStringType(stmt.fRetType) Then
					Return t + " &bbEmptyString"
				End If
				If TArrayType(stmt.fRetType) Then
					Return t + " &bbEmptyArray"
				End If
			End If
			t:+" "+stmt.expr.Trans()
		End If
		Return t
	End Method
	
	Method TransContinueStmt$( stmt:TContinueStmt )
		unreachable=True
		Return "continue"
	End Method
	
	Method TransBreakStmt$( stmt:TBreakStmt )
		unreachable=True
		broken:+1
		Return "break"
	End Method
	
	Method TransTryStmt$( stmt:TTryStmt )
	End Method

	Method TransThrowStmt$( stmt:TThrowStmt )
	End Method
	

	Method TransBuiltinExpr$( expr:TBuiltinExpr )
		If TMinExpr(expr) Return TransMinExpr(TMinExpr(expr))
		If TMaxExpr(expr) Return TransMaxExpr(TMaxExpr(expr))
		If TAbsExpr(expr) Return TransAbsExpr(TAbsExpr(expr))
		If TAscExpr(expr) Return TransAscExpr(TAscExpr(expr))
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

	Method TransLenExpr:String(expr:TLenExpr)
	End Method

	Method TransSizeOfExpr:String(expr:TSizeOfExpr)
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
'DebugStop
		'If ENV_CONFIG="debug"
		'	If TFuncDecl( block ) EmitPushErr
		'EndIf
		
		PushEnv block

		For Local stmt:TStmt=EachIn block.stmts
		
			_errInfo=stmt.errInfo
			
			If unreachable And ENV_LANG<>"as"
				'If stmt.errInfo Print "Unreachable:"+stmt.errInfo
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
			Local t$=stmt.Trans()
			If t Emit t+";"
			
			Local v:String = String(customVarStack.Pop())
			While v
				Emit "bbMemFree" + Bra(v) + ";"
				v = String(customVarStack.Pop())
			Wend
			
		Next
		Local r:Int=unreachable
		unreachable=False
		PopEnv
		Return r
	End Method
	
	Method TransDeclStmt$( stmt:TDeclStmt )
		Local decl:TLocalDecl=TLocalDecl( stmt.decl )
		If decl
			MungDecl decl
			Return TransLocalDecl( decl.munged,decl.init )
		EndIf
		Local cdecl:TConstDecl=TConstDecl( stmt.decl )
		If cdecl
			Return Null
		EndIf
		Local gdecl:TGlobalDecl=TGlobalDecl( stmt.decl )
		If gdecl Then
			MungDecl gdecl
			Return TransGlobalDecl( gdecl.munged, gdecl.init, gdecl.attrs )
		End If
		InternalErr
	End Method
	
	Method TransIfStmt$( stmt:TIfStmt )
		If TConstExpr( stmt.expr )
			If TConstExpr( stmt.expr ).value
'				Emit "if"+Bra( stmt.expr.Trans() )+"{"
				If EmitBlock( stmt.thenBlock ) unreachable=True
'				Emit "}"
			Else If stmt.elseBlock.stmts.First()
'				Emit "if(!"+Bra( stmt.expr.Trans() )+"){"
				If EmitBlock( stmt.elseBlock ) unreachable=True
'				Emit "}"
			EndIf
		Else If stmt.elseBlock.stmts.First()
			Emit "if"+Bra( stmt.expr.Trans() )+"{"
			FreeVarsIfRequired(False)
			PushVarScope
			Local unr:Int=EmitBlock( stmt.thenBlock )
			PopVarScope
			Emit "}else{"
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
				Emit "if"+Bra( stmt.expr.Trans() )+"{"
				FreeVarsIfRequired(False)
'			End If
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

		Emit "while"+Bra( stmt.expr.Trans() )+"{"
		
		Local unr:Int=EmitBlock( stmt.block )
		
		Emit "}"
		
		If broken=nbroken And TConstExpr( stmt.expr ) And TConstExpr( stmt.expr ).value unreachable=True
		broken=nbroken
	End Method

	Method TransRepeatStmt$( stmt:TRepeatStmt )
		Local nbroken:Int=broken

		Emit "do{"
		
		Local unr:Int=EmitBlock( stmt.block )
		
		Emit "}while(!"+Bra( stmt.expr.Trans() )+");"

		If broken=nbroken And TConstExpr( stmt.expr ) And Not TConstExpr( stmt.expr ).value unreachable=True
		broken=nbroken
	End Method

	Method TransForStmt$( stmt:TForStmt )
		Local nbroken:Int=broken

		Local init$

		Local decl:Int
		If TDeclStmt(stmt.init) Then
			decl = True
			Emit "{"
			Emit stmt.init.Trans() + ";"
			init = TDeclStmt(stmt.init).decl.munged
		Else
			init=stmt.init.Trans()
		End If
		Local expr$=stmt.expr.Trans()
		Local incr$=stmt.incr.Trans()

		Emit "for("+init+";"+expr+";"+incr+"){"
		
		Local unr:Int=EmitBlock( stmt.block )
		
		Emit "}"
		
		If decl Then
			Emit "}"
		End If
		
		If broken=nbroken And TConstExpr( stmt.expr ) And TConstExpr( stmt.expr ).value unreachable=True
		broken=nbroken
	End Method

	Method TransAssertStmt$( stmt:TAssertStmt )
		
		Emit "// TODO : assert statement"

	End Method

	Method TransEndStmt$( stmt:TEndStmt ) Abstract
	
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
	
End Type


