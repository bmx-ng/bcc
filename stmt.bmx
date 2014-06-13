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

Type TStmt
	Field errInfo$
	
	Method New()
		errInfo=_errInfo
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl ) Abstract
	
	Method Semant()
		PushErr errInfo
		OnSemant
		PopErr
	End Method

	Method Copy:TStmt( scope:TScopeDecl )
		Local t:TStmt=OnCopy( scope )
		t.errInfo=errInfo
		Return t
	End Method
	
	Method OnSemant() Abstract

	Method Trans$() Abstract

End Type

Type TDeclStmt Extends TStmt
	Field decl:TDecl
	
	Method Create:TDeclStmt( decl:TDecl )
		Self.decl=decl
		Return Self
	End Method
	
	Method CreateWithId:TDeclStmt( id$,ty:TType,init:TExpr )
		Self.decl=New TLocalDecl.Create( id,ty,init,0 )	
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TDeclStmt.Create( decl.Copy() )
	End Method
	
	Method OnSemant()
		decl.Semant
		_env.InsertDecl decl
	End Method
	
	Method Trans$()
		Return _trans.TransDeclStmt( Self )
	End Method
End Type

Type TAssignStmt Extends TStmt
	Field op$
	Field lhs:TExpr
	Field rhs:TExpr
	
	Method Create:TAssignStmt( op$,lhs:TExpr,rhs:TExpr )
		Self.op=op
		Self.lhs=lhs
		Self.rhs=rhs
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TAssignStmt.Create( op,lhs.Copy(),rhs.Copy() )
	End Method
	
	Method OnSemant()
		rhs=rhs.Semant()
		lhs=lhs.SemantSet( op,rhs )
		If TInvokeExpr( lhs ) Or TInvokeMemberExpr( lhs )
			rhs=Null
		Else
			If TPointerType(lhs.exprType) And TNumericType(rhs.exprType) Then
				' with pointer assignment we don't cast the numeric to a pointer
			Else
				rhs=rhs.Cast( lhs.exprType )
			End If
		EndIf
	End Method
	
	Method Trans$()
		_errInfo=errInfo
		Return _trans.TransAssignStmt( Self )
	End Method
End Type

Type TExprStmt Extends TStmt
	Field expr:TExpr
	
	Method Create:TExprStmt( expr:TExpr )
		Self.expr=expr
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TExprStmt.Create( expr.Copy() )
	End Method
		
	Method OnSemant()
		expr=expr.Semant()
		If Not expr InternalErr
	End Method

	Method Trans$()
		Return _trans.TransExprStmt( Self )
	End Method
End Type

Type TReturnStmt Extends TStmt
	Field expr:TExpr
	Field fRetType:TType

	Method Create:TReturnStmt( expr:TExpr )
		Self.expr=expr
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		If expr Return New TReturnStmt.Create( expr.Copy() )
		Return New TReturnStmt.Create( Null )
	End Method
	
	Method OnSemant()
		Local fdecl:TFuncDecl=_env.FuncScope()
		If expr
			If fdecl.IsCtor() Err "Constructors may not return a value."
			If TVoidType( fdecl.retType ) Err "Void functions may not return a value."
			fRetType = fdecl.retType
			expr=expr.SemantAndCast( fdecl.retType )
		Else If fdecl.IsCtor()
			expr=New TSelfExpr.Semant()
		Else If Not TVoidType( fdecl.retType )
			If _env.ModuleScope().IsSuperStrict() Err "Missing return expression."
			expr=New TConstExpr.Create( fdecl.retType,"" ).Semant()
		EndIf
	End Method
	
	Method Trans$()
		Return _trans.TransReturnStmt( Self )
	End Method
End Type

Type TTryStmt Extends TStmt

	Field block:TBlockDecl
	Field catches:TCatchStmt[]
	
	Method Create:TTryStmt( block:TBlockDecl,catches:TCatchStmt[] )
		Self.block=block
		Self.catches=catches
		Return Self
	End Method
	
	Method OnCopy:TStmt( scope:TScopeDecl )
		Local tcatches:TCatchStmt[] = Self.catches[..]
		For Local i:Int=0 Until tcatches.Length
			tcatches[i]=TCatchStmt( tcatches[i].Copy( scope ) )
		Next
		Return New TTryStmt.Create( block.CopyBlock( scope ),tcatches )
	End Method
	
	Method OnSemant()
		block.Semant
		Local hasObject:Int = False
		For Local i:Int = 0 Until catches.Length
			catches[i].Semant
			If hasObject Then
				PushErr catches[i].errInfo
				Err "Catch variable class extends earlier catch variable class"
			End If
			If TObjectType(catches[i].init.ty) And TObjectType(catches[i].init.ty).classdecl.ident = "Object" Then
				hasObject = True
				Continue
			End If
			For Local j:Int = 0 Until i
				If catches[i].init.ty.ExtendsType( catches[j].init.ty )
					PushErr catches[i].errInfo
					Err "Catch variable class extends earlier catch variable class"
				EndIf
			Next
		Next
	End Method
	
	Method Trans$()
		Return _trans.TransTryStmt( Self )
	End Method
	
End Type

Type TCatchStmt Extends TStmt

	Field init:TLocalDecl
	Field block:TBlockDecl
	
	Method Create:TCatchStmt( init:TLocalDecl,block:TBlockDecl )
		Self.init=init
		Self.block=block
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TCatchStmt.Create( TLocalDecl( init.Copy() ),block.CopyBlock( scope ) )
	End Method
	
	Method OnSemant()
		init.Semant
		If Not TObjectType( init.ty )  And Not TStringType(init.ty) Err "Variable type must extend Throwable"
		'If Not init.Type.GetClass().IsThrowable() Err "Variable type must extend Throwable"
		block.InsertDecl init
		block.Semant
	End Method
	
	Method Trans$()
	End Method

End Type

Type TThrowStmt Extends TStmt
	Field expr:TExpr

	Method Create:TThrowStmt( expr:TExpr )
		Self.expr=expr
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TThrowStmt.Create( expr.Copy() )
	End Method
	
	Method OnSemant()
		expr=expr.Semant()
		If Not TObjectType( expr.exprType ) And Not TStringType(expr.exprType) Err "Expression Type must extend Throwable"
		'If Not expr.exprType.GetClass().IsThrowable() Err "Expression type must extend Throwable"
	End Method
	
	Method Trans$()
	' TODO
		Return _trans.TransThrowStmt( Self )
	End Method
End Type

Type TBreakStmt Extends TStmt

	Method OnSemant()
		If Not _loopnest Err "Exit statement must appear inside a loop."
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TBreakStmt
	End Method
	
	Method Trans$()
		Return _trans.TransBreakStmt( Self )
	End Method
	
End Type

Type TContinueStmt Extends TStmt

	Method OnSemant()
		If Not _loopnest Err "Continue statement must appear inside a loop."
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TContinueStmt
	End Method
	
	Method Trans$()
		Return _trans.TransContinueStmt( Self )
	End Method
	
End Type

Type TIfStmt Extends TStmt
	Field expr:TExpr
	Field thenBlock:TBlockDecl
	Field elseBlock:TBlockDecl
	
	Method Create:TIfStmt( expr:TExpr,thenBlock:TBlockDecl,elseBlock:TBlockDecl )
		Self.expr=expr
		Self.thenBlock=thenBlock
		Self.elseBlock=elseBlock
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TIfStmt.Create( expr.Copy(),thenBlock.CopyBlock( scope ),elseBlock.CopyBlock( scope ) )
	End Method
	
	Method OnSemant()
		expr=expr.SemantAndCast( TType.boolType,CAST_EXPLICIT )
		thenBlock.Semant
		elseBlock.Semant
	End Method
	
	Method Trans$()
		Return _trans.TransIfStmt( Self )
	End Method
End Type

Type TWhileStmt Extends TStmt
	Field expr:TExpr
	Field block:TBlockDecl
	
	Method Create:TWhileStmt( expr:TExpr,block:TBlockDecl )
		Self.expr=expr
		Self.block=block
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TWhileStmt.Create( expr.Copy(),block.CopyBlock( scope ) )
	End Method
	
	Method OnSemant()
		expr=expr.SemantAndCast( TType.boolType,CAST_EXPLICIT )
		_loopnest:+1
		block.Semant
		_loopnest:-1
	End Method
	
	Method Trans$()
		Return _trans.TransWhileStmt( Self )
	End Method
End Type

Type TRepeatStmt Extends TStmt
	Field block:TBlockDecl
	Field expr:TExpr
	
	Method Create:TRepeatStmt( block:TBlockDecl,expr:TExpr )
		Self.block=block
		Self.expr=expr
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TRepeatStmt.Create( block.CopyBlock( scope ),expr.Copy() )
	End Method
	
	Method OnSemant()
		_loopnest:+1
		block.Semant
		_loopnest:-1
		expr=expr.SemantAndCast( TType.boolType,CAST_EXPLICIT )
	End Method
	
	Method Trans$()
		Return _trans.TransRepeatStmt( Self )
	End Method
End Type

Type TForStmt Extends TStmt
	Field init:TStmt	'assignment or local decl...
	Field expr:TExpr
	Field incr:TStmt	'assignment...
	Field block:TBlockDecl
	
	Method Create:TForStmt( init:TStmt,expr:TExpr,incr:TStmt,block:TBlockDecl )
		Self.init=init
		Self.expr=expr
		Self.incr=incr
		Self.block=block
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TForStmt.Create( init.Copy( scope ),expr.Copy(),incr.Copy( scope ),block.CopyBlock( scope ) )
	End Method
	
	Method OnSemant()

		PushEnv block
		init.Semant
		PopEnv
		
		expr=expr.Semant()
		
		_loopnest:+1
		block.Semant
		_loopnest:-1

		incr.Semant
		
		'dodgy as hell! Reverse comparison for backward loops!
		Local assop:TAssignStmt=TAssignStmt( incr )
		Local addop:TBinaryExpr=TBinaryExpr( assop.rhs )
		Local stpval$=addop.rhs.Eval()
		If stpval.StartsWith( "-" )
			Local bexpr:TBinaryExpr=TBinaryExpr( expr )
			Select bexpr.op
			Case "<" bexpr.op=">"
			Case "<=" bexpr.op=">="
			End Select
		EndIf
		
	End Method
	
	Method Trans$()
		Return _trans.TransForStmt( Self )
	End Method
End Type

Type TAssertStmt Extends TStmt
	Field expr:TExpr
	Field elseExpr:TExpr
	
	Method Create:TAssertStmt( expr:TExpr, elseExpr:TExpr )
		Self.expr=expr
		Self.elseExpr=elseExpr
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		If elseExpr Then
			Return New TAssertStmt.Create( expr.Copy(),elseExpr.Copy() )
		Else
			Return New TAssertStmt.Create( expr.Copy(), Null )
		End If
	End Method
	
	Method OnSemant()
		expr=expr.SemantAndCast( TType.boolType,CAST_EXPLICIT )
		If elseExpr Then
			elseExpr = elseExpr.SemantAndCast(TType.stringType,CAST_EXPLICIT)
		End If
	End Method
	
	Method Trans$()
		Return _trans.TransAssertStmt( Self )
	End Method
End Type

Type TEndStmt Extends TStmt
	
	Method Create:TEndStmt( )
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TEndStmt.Create( )
	End Method
	
	Method OnSemant()
	End Method
	
	Method Trans$()
		Return _trans.TransEndStmt( Self )
	End Method
End Type
