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
SuperStrict

Import BRL.MaxUtil
Import "toker.bmx"
Import "iparser.bmx"


Global FILE_EXT$="bmx"

Type TForEachinStmt Extends TLoopStmt
	Field varid$
	Field varty:TType
	Field varlocal:Int
	Field expr:TExpr
	Field block:TBlockDecl
	
	Field stmts:TList=New TList

	Method Create:TForEachinStmt( varid$,varty:TType,varlocal:Int,expr:TExpr,block:TBlockDecl,loopLabel:TLoopLabelDecl )
		Self.varid=varid
		Self.varty=varty
		Self.varlocal=varlocal
		Self.expr=expr
		Self.block=block
		Self.loopLabel=loopLabel
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		Return New TForEachinStmt.Create( varid,varty,varlocal,expr.Copy(),block.CopyBlock( scope ),TLoopLabelDecl(loopLabel.Copy()) )
	End Method

	Method OnSemant()
		expr=expr.Semant()

		If TArrayType( expr.exprType ) Or TStringType( expr.exprType )

			Local exprTmp:TLocalDecl=New TLocalDecl.Create( "",Null,expr,,True )
			Local indexTmp:TLocalDecl=New TLocalDecl.Create( "",Null,New TConstExpr.Create( New TIntType,"0" ),,True )

			Local lenExpr:TExpr=New TIdentExpr.Create( "Length",New TVarExpr.Create( exprTmp ) )

			Local cmpExpr:TExpr=New TBinaryCompareExpr.Create( "<",New TVarExpr.Create( indexTmp ),lenExpr )

			Local indexExpr:TExpr=New TIndexExpr.Create( New TVarExpr.Create( exprTmp ),[New TVarExpr.Create( indexTmp )] )
			Local addExpr:TExpr=New TBinaryMathExpr.Create( "+",New TVarExpr.Create( indexTmp ),New TConstExpr.Create( New TIntType,"1" ) )


			If varlocal

				' array of object ?
				
				If TObjectType(TArrayType( expr.exprType ).elemType) Then

					Local cExpr:TExpr
					
					If TIdentType(varty) And TIdentType(varty).ident = "Object" Then
						cExpr = indexExpr
					Else
						cExpr = New TCastExpr.Create( varty, indexExpr,CAST_EXPLICIT )
					End If

					' local variable
					Local varTmp:TLocalDecl=New TLocalDecl.Create( varid,varty,cExpr )

					' local var as expression
					Local expr:TExpr=New TVarExpr.Create( varTmp )

					' var = Null
					expr=New TBinaryCompareExpr.Create( "=",expr, New TNullExpr.Create(TType.nullObjectType))

					' then continue
					Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope )
					Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope )
					thenBlock.AddStmt New TContinueStmt

					block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock )
					block.stmts.AddFirst New TAssignStmt.Create( "=",New TVarExpr.Create( indexTmp ),addExpr )
					block.stmts.AddFirst New TDeclStmt.Create( varTmp )

				Else
					Local varTmp:TLocalDecl=New TLocalDecl.Create( varid,varty,indexExpr )
					block.stmts.AddFirst New TAssignStmt.Create( "=",New TVarExpr.Create( indexTmp ),addExpr, True )
					block.stmts.AddFirst New TDeclStmt.Create( varTmp, True )
				End If
			Else
				
				If TObjectType(TArrayType( expr.exprType ).elemType) Then
				' var = Null
					If Not varty Then
						Local decl:TValDecl = block.scope.FindValDecl(varid)
						
						If decl Then
							decl.Semant()
							
							varty = decl.ty.Copy()
						End If
					End If

					expr=New TBinaryCompareExpr.Create( "=",New TIdentExpr.Create( varid ), New TNullExpr.Create(TType.nullObjectType))

					' then continue
					Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope )
					Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope )
					thenBlock.AddStmt New TContinueStmt

					block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock )
					'block.stmts.AddFirst New TDeclStmt.Create( varTmp )

					block.stmts.AddFirst New TAssignStmt.Create( "=",New TVarExpr.Create( indexTmp ),addExpr, True )
					block.stmts.AddFirst New TAssignStmt.Create( "=",New TIdentExpr.Create( varid ),New TCastExpr.Create( varty, indexExpr,CAST_EXPLICIT ), True )
				Else
					block.stmts.AddFirst New TAssignStmt.Create( "=",New TVarExpr.Create( indexTmp ),addExpr, True )
					block.stmts.AddFirst New TAssignStmt.Create( "=",New TIdentExpr.Create( varid ),indexExpr, True )
				End If

			EndIf

			Local whileStmt:TWhileStmt=New TWhileStmt.Create( cmpExpr,block,Null, True )

			block=New TBlockDecl.Create( block.scope, True )
			block.AddStmt New TDeclStmt.Create( exprTmp, True )
			block.AddStmt New TDeclStmt.Create( indexTmp, True )
			block.AddStmt whileStmt

		Else If TObjectType( expr.exprType )
			Local tmpDecl:TDeclStmt

			If TInvokeExpr(expr) Or TInvokeMemberExpr(expr) Then
				Local tmpVar:TLocalDecl=New TLocalDecl.Create( "",expr.exprType,expr,,True )
				tmpVar.Semant()
				tmpDecl = New TDeclStmt.Create( tmpVar, True )
				expr = New TVarExpr.Create( tmpVar )
			End If

			Local enumerInit:TExpr=New TFuncCallExpr.Create( New TIdentExpr.Create( "ObjectEnumerator",expr ) )
			Local enumerTmp:TLocalDecl=New TLocalDecl.Create( "",Null,enumerInit,,True )

			Local hasNextExpr:TExpr=New TFuncCallExpr.Create( New TIdentExpr.Create( "HasNext",New TVarExpr.Create( enumerTmp ) ) )
			Local nextObjExpr:TExpr=New TFuncCallExpr.Create( New TIdentExpr.Create( "NextObject",New TVarExpr.Create( enumerTmp ) ) )

			If varlocal
'				Local varTmp:TLocalDecl=New TLocalDecl.Create( varid,varty,nextObjExpr )
'				block.stmts.AddFirst New TDeclStmt.Create( varTmp )

				Local cExpr:TExpr
				
				If TIdentType(varty) And TIdentType(varty).ident = "Object" Then
					cExpr = nextObjExpr
				Else
					cExpr = New TCastExpr.Create( varty, nextObjExpr,CAST_EXPLICIT )
				End If

				' local variable
				Local varTmp:TLocalDecl=New TLocalDecl.Create( varid,varty,cExpr)

				' local var as expression
				Local expr:TExpr=New TVarExpr.Create( varTmp )

				' var = Null
				expr=New TBinaryCompareExpr.Create( "=",expr, New TNullExpr.Create(TType.nullObjectType))

				' then continue
				Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope, True )
				Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope, True )
				thenBlock.AddStmt New TContinueStmt.Create(Null, True)

				block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock, True )
				block.stmts.AddFirst New TDeclStmt.Create( varTmp, True )
			Else
				
				If Not varty Then
					Local decl:TValDecl = block.scope.FindValDecl(varid)
					
					If decl Then
						decl.Semant()
						
						varty = decl.ty.Copy()
					End If
				End If
				
				' var = Null
				Local expr:TExpr=New TBinaryCompareExpr.Create( "=",New TIdentExpr.Create( varid ), New TNullExpr.Create(TType.nullObjectType))

				' then continue
				Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope )
				Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope )
				thenBlock.AddStmt New TContinueStmt

				block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock )
				'block.stmts.AddFirst New TDeclStmt.Create( varTmp )

				block.stmts.AddFirst New TAssignStmt.Create( "=",New TIdentExpr.Create( varid ),New TCastExpr.Create( varty, nextObjExpr,CAST_EXPLICIT ))
			EndIf

			Local whileStmt:TWhileStmt=New TWhileStmt.Create( hasNextExpr,block,Null, True )

			block=New TBlockDecl.Create( block.scope, True )
			If tmpDecl Then
				block.AddStmt tmpDecl
			End If
			block.AddStmt New TDeclStmt.Create( enumerTmp, True )
			block.AddStmt whileStmt

		Else
			InternalErr
		EndIf

		block.Semant
	End Method

	Method Trans$()
		_trans.EmitBlock block
	End Method

End Type



Type TIncbin

	Field file:String
	Field path:String
	Field id:Int
	Field length:Int

	Global count:Int

	Method Create:TIncbin(file:String, source:String)
		count :+ 1

		Self.file = file

		' find the file
		If Not FileType(file) Then
			' maybe relative to source
			Local dir:String = ExtractDir(source) + "/" + file
			If FileType(dir) = FILETYPE_FILE Then
				path = RealPath(dir)
			Else
				Internalerr '?
			End If
		Else
			path = RealPath(file)
		End If

		id = count
		Return Self
	End Method

End Type

'***** Parser *****
Type TParser

	Field _toker:TToker
	Field _toke:String
	Field _tokeType:Int
	'Ronny: _tokerStack is unused
	'Field _tokerStack:TList=New TList'<TToker>

	Field _block:TBlockDecl
	Field _blockStack:TList=New TList'<TBlockDecl>
	Field _errStack:TStringList=New TStringList

	Field _app:TAppDecl
	Field _module:TModuleDecl

	Field _externCasts:TMap = New TMap

	Method SetErr()
		If _toker.Path()
			_errInfo=FormatError(_toker.Path(),_toker.Line(),0)
		EndIf
	End Method

	Method DoErr(error:String)
		SetErr()
		Err error
	End Method

	Method PushBlock( block:TBlockDecl )
		If _block <> Null Then
			_blockStack.AddLast _block
		End If
		_errStack.AddLast _errInfo
		_block=block
	End Method

	Method PopBlock()
		_block=TBlockDecl(_blockStack.RemoveLast())
		_errInfo=String(_errStack.RemoveLast())
	End Method

	Method RealPath$( path$ )
		Local popDir$=CurrentDir()
		ChangeDir ExtractDir( _toker.Path() )
		path=BRL.FileSystem.RealPath( path )
		ChangeDir popDir
		Return path
	End Method

	Method NextToke$()
		Local toke$=_toke

		Repeat
			_toke=_toker.NextToke()
			_tokeType=_toker.TokeType()
		Until _tokeType<>TOKE_SPACE

		If _tokeType=TOKE_KEYWORD _toke=_toke.ToLower()

		If toke="," SkipEols

		Return _toke
	End Method

	Method NextTokeToker$(toker:TToker)
'		Local toke$=toker._toke

		Repeat
			toker.NextToke()
		Until toker.tokeType()<>TOKE_SPACE

		Return toker._toke
	End Method

	Method DescribeToke:String( toke:String )
		Select toke
			Case "~n"
				Return "end-of-line"
		End Select
		Return toke
	End Method

	Method CParse:Int( toke$ )
		If _toke.ToLower()<>toke
			Return False
		EndIf
		NextToke
		Return True
	End Method

	Method CParseToker:Int( toker:TToker, toke$ )
		If toker._toke.ToLower()<>toke
			Return False
		EndIf
		NextTokeToker(toker)
		Return True
	End Method

	Method Parse( toke$ )
		If Not CParse( toke )
			DoErr "Syntax error - expecting '"+toke+"'."
		EndIf
	End Method

	Method ParseToker( toker:TToker, toke$ )
		If Not CParseToker( toker, toke )
			DoErr "Syntax error - expecting '"+toke+"'."
		EndIf
	End Method

	Method AtEos:Int()
		Return _toke="" Or _toke=";" Or _toke="~n" Or _toke="else"
	End Method

	Method SkipEols()
		While CParse( "~n" ) Or CParse(";")
		Wend
		SetErr
	End Method

	Method SkipEolsToker(toker:TToker)
		While CParseToker( toker, "~n" )
		Wend
		SetErr
	End Method

	Method ParseStringLit$()
		If _tokeType<>TOKE_STRINGLIT Err "Expecting string literal."
		Local str$=BmxUnquote( _toke )
		NextToke
		Return str
	End Method

	Method ParseIdent$()
		Select _toke
		Case "@" NextToke
		Case "string","object"
		Default
			If _tokeType<>TOKE_IDENT Err "Syntax error - expecting identifier."
		End Select
		Local id$=_toke
		NextToke
		Return id
	End Method

	Method ParseIdentType:TIdentType()
		Local id$=ParseIdent()
'DebugLog "ParseIdentType : " + id
		If CParse( "." ) id:+"."+ParseIdent()
		If CParse( "." ) id:+"."+ParseIdent()

		Local args:TIdentType[]
		If CParse( "<" )
			Local nargs:Int
			Repeat
				Local arg:TIdentType=ParseIdentType()
				If args.Length=nargs args=args+ New TIdentType[10]
				args[nargs]=arg
				nargs:+1
			Until Not CParse(",")
			args=args[..nargs]
			Parse ">"
		EndIf
		Return New TIdentType.Create( id,args )
	End Method

	Method CParseIdentType:TIdentType( inner:Int=False )
		If _tokeType<>TOKE_IDENT Return Null
		Local id$=ParseIdent()
		If CParse( "." )
			If _tokeType<>TOKE_IDENT Return Null
			id:+"."+ParseIdent()
		End If
		If Not CParse( "<" )
			If inner Return New TIdentType.Create( id,Null )
			Return Null
		EndIf
		Local args:TType[]
		Local nargs:Int
		Repeat
			Local arg:TType=CParsePrimitiveType()
			If Not arg
				arg=CParseIdentType( True )
				If Not arg Return Null
			EndIf
			While IsArrayDef()
				arg = ParseArrayType(arg)
			Wend
'			While CParse( "[]" )
'				arg=arg.ArrayOf()
'			Wend
			args = args + [arg]
			nargs :+ 1
		Until Not CParse(",")
		If Not CParse( ">" ) Return Null
		Return New TIdentType.Create( id,args )
	End Method

	Method CParsePrimitiveType:TType()
		If CParse( "string" ) Return TType.stringType
		If CParse( "object" ) Return New TIdentType.Create( "brl.classes.object" )

		Local ty:TType
		If CParse( "short" ) ty = New TShortType
		If CParse( "byte" ) ty = New TByteType
		If CParse( "int" ) ty = New TIntType
		If CParse( "float" ) ty = New TFloatType
		If CParse( "long" ) ty = New TLongType
		If CParse( "double" ) ty = New TDoubleType

		While CParse("ptr")
			ty = TType.MapToPointerType(ty)
		Wend
		Return ty
	End	Method

	Method CParsePrimitiveNumberType:TType()
		If CParse( "short" ) Return New TShortType
		If CParse( "byte" ) Return New TByteType
		If CParse( "int" ) Return New TIntType
		If CParse( "float" ) Return New TFloatType
		If CParse( "long" ) Return New TLongType
		If CParse( "double" ) Return New TDoubleType
	End	Method

	Method ParseNewType:TType()
		If CParse( "void" ) Return New TVoidType
		If CParse( "short" ) Return New TShortType
		If CParse( "byte" ) Return New TByteType
		If CParse( "int" ) Return New TIntType
		If CParse( "float" ) Return New TFloatType
		If CParse( "string" ) Return TType.stringType
		If CParse( "object" ) Return New TIdentType.Create( "brl.classes.object" )
		If CParse( "long" ) Return New TLongType
		If CParse( "double" ) Return New TDoubleType
		Return ParseIdentType()
	End Method

	Method ParseType:TType()
		Local ty:TType=CParsePrimitiveType()
		If ty Return ty
		Return ParseIdentType()
	End Method

	Method ParseConstNumberType:TType()
		Local ty:TType
		Select _toke
		Case "@"
			NextToke
			ty=New TByteType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case "@@"
			NextToke
			ty=New TShortType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case "%"
			NextToke
			ty=New TIntType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case "#"
			NextToke
			ty=New TFloatType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case "$"
			NextToke
			ty=New TStringType
		Case "!"
			NextToke
			ty=New TDoubleType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case "%%"
			NextToke
			ty=New TLongType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case ":"
			NextToke
			ty=CParsePrimitiveNumberType()
			If Not ty Then
				ty = ParseIdentType()
			Else
				While CParse("ptr")
					ty = TType.MapToPointerType(ty)
				Wend
			End If
		End Select

		While IsArrayDef()
			ty = ParseArrayType(ty)
		Wend
		'While CParse( "[]" )
		'	ty=New TArrayType.Create( ty )
		'Wend

		Return ty
	End Method

	Method ParseDeclType:TType()
		Local ty:TType
		Select _toke
		Case "@"
			NextToke
			ty=New TByteType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case "@@"
			NextToke
			ty=New TShortType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case "%"
			NextToke
			ty=New TIntType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case "%%"
			NextToke
			ty=New TLongType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case "#"
			NextToke
			ty=New TFloatType

			If CParse("ptr") Then
				ty = TType.MapToPointerType(ty)
			End If
		Case "$"
			NextToke
			ty=New TStringType

			If CParse("z") Then
				ty._flags :| TType.T_CHAR_PTR
			Else If CParse("w") Then
				ty._flags :| TType.T_SHORT_PTR
			End If

		Case "!"
			NextToke
			ty=New TDoubleType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		Case ":"
			NextToke
			ty=ParseType()

			If CParse("ptr") Then

				ty = TType.MapToPointerType(ty)

				While CParse("ptr")
					ty = TType.MapToPointerType(ty)
				Wend

				If Not ty DoErr "Invalid Pointer type."
			End If

		Case "("
			' nothing to see here.
			If _module.IsSuperStrict() Then
				' BaH : default return type when not defined
				ty=New TVoidType
			Else
				ty=New TIntType
			End If
		Default
			If _module.IsSuperStrict() Err "Illegal type expression."
			ty=New TIntType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		End Select
		
		' array ?
		While IsArrayDef()
			ty = ParseArrayType(ty)
		Wend
		
		Return ty
	End Method

	Method ParseArrayExpr:TArrayExpr()
		Parse "["
		Local args:TExpr[],nargs:Int
		Repeat
			Local arg:TExpr=ParseExpr()
			If args.Length=nargs args=args + New TExpr[10]
			args[nargs]=arg
			nargs:+1
		Until Not CParse(",")
		args=args[..nargs]
		Parse "]"
		Return New TArrayExpr.Create( args )
	End Method

	' replaces While CParse( "[]" ) sections, with support for multi-dimension arrays
	Method ParseArrayType:TType(ty:TType)
		While True
			Local dims:Int = 1
			
			If CParse("[]") Then
				ty=New TArrayType.Create( ty )
				Exit
			End If
			
			If Not CParse("[") Then
				Exit
			End If
		
			While CParse( ",")
				dims :+ 1
			Wend
			
			Parse "]"
			
			ty=New TArrayType.Create( ty, dims )
			Exit
		Wend
		Return ty
	End Method
	
	Method IsArrayDef:Int()
		Local isDef:Int = True
		Local toker:TToker=New TToker.Copy(_toker)
		While True
			'Local dims:Int = 1
			
			If CParseToker(toker, "[]") Then
				Exit
			End If
			
			If Not CParseToker(toker, "[") Then
				isDef = False
				Exit
			End If
		
			While CParseToker(toker, ",")
				'dims :+ 1
			Wend
			
			If Not CParseToker(toker, "]") Then
				isDef = False
				Exit
			End If
			Exit
		Wend
		Return isDef
	End Method

	Method ParseArgs:TExpr[]( stmt:Int )
		Local args:TExpr[]

		If stmt
			If AtEos() Return args
		Else
			If _toke<>"(" Return args
		EndIf

		Local nargs:Int,eat:Int

		If _toke="("
			If stmt
				Local toker:TToker=New TToker.Copy(_toker),bra:Int=1
				Repeat
					toker.NextToke
					toker.SkipSpace
					Select toker.Toke().ToLower()
					Case "","else"
						Err "Parenthesis mismatch error."
					Case "(","["
						bra:+1
					Case "]",")"
						bra:-1
						If bra Continue
						toker.NextToke
						toker.SkipSpace
						Select toker.Toke().ToLower()
						Case ".","(","[","",";","~n","else"
							eat=True
						End Select
						Exit
					Case ","
						If bra<>1 Continue
						eat=True
						Exit
					End Select
				Forever
			Else
				eat=True
			EndIf
			If eat And NextToke()=")"
				NextToke
				Return args
			EndIf
		EndIf

		Repeat
			Local arg:TExpr
			If _toke And _toke<>"," arg=ParseExpr()
			If args.Length=nargs args=args + New TExpr[10]
			args[nargs]=arg
			nargs:+1
		Until Not CParse(",")
		args=args[..nargs]

		If eat Parse ")"

		Return args
	End Method

	Method ParsePrimaryExpr:TExpr( stmt:Int )

		Local expr:TExpr

		Select _toke.ToLower()
		Case "("
			NextToke
			expr=ParseExpr()
			Parse ")"
		Case "["
			expr=ParseArrayExpr()
		Case "[]"
			NextToke
			expr=New TConstExpr.Create( TType.emptyArrayType,"" )
		Case "."
			expr=New TScopeExpr.Create( _module )
		Case "new"
'DebugStop
			NextToke
			Local ty:TType=ParseType()

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend

			If _toke = "[" Or _toke = "[]" Then
				Local depth:Int = 0
				Local ln:TExpr[]
				Local tmpTy:TType = ty.Copy()

				Repeat
					Local dims:Int = 1
					
					If CParse("[]") Then
						tmpTy=New TArrayType.Create( tmpTy )
						depth :+ 1
						Continue
					End If

					' looking for an array with expression					
					If Not ln Then
						Parse "["
					Else
						If Not CParse("[") Then
							Exit
						Else
							Err "Unexpected '[' after array size declaration"
						End If
					End If

					Repeat
						If CParse(",") Then
							dims :+ 1
							Continue
						End If
						If CParse("]") Exit
						ln = ln + [ParseExpr()]
						If CParse("]") Exit
						Parse(",")
					Forever

					If Not ln Then
						tmpTy=New TArrayType.Create( tmpTy, dims )
					End If
				Forever

				If ln Then
					ty = tmpTy
				End If


'				Repeat
					'If CParse( "[" )
'					Repeat
'						ln = ln + [ParseExpr()]
'						If CParse("]") Exit
'						Parse ","
'					Forever
					'Parse "]"
'					ty = ParseArrayType(ty)
'				Forever
				'While CParse( "[]" )
				'	ty=New TArrayType.Create( ty)
				'Wend
				expr=New TNewArrayExpr.Create( ty,ln )
			Else
				expr=New TNewObjectExpr.Create( ty,ParseArgs( stmt ) )
			EndIf
		Case "null"
			NextToke
			expr = New TNullExpr.Create(TType.nullObjectType)
			'expr=New TConstExpr.Create( TType.nullObjectType,"" )
		Case "true"
			NextToke
			expr=New TConstExpr.Create( New TIntType,"1" )
		Case "false"
			NextToke
			expr=New TConstExpr.Create( New TIntType,"" )
		Case "int","long","float","double","object","short","byte"
			Local id$=_toke
			Local ty:TType=ParseType()

			If TIntType(ty) And id.ToLower() <> "int" Then
				Select id.ToLower()
					Case "byte"
						ty = New TByteType
					Case "short"
						ty = New TShortType
					Case "long"
						ty = New TLongType
					Case "float"
						ty = New TFloatType
					Case "double"
						ty = New TDoubleType
				End Select
			End If

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend

			' array
			ty = ParseArrayType(ty)
			'While CParse( "[]" )
			'	ty=New TArrayType.Create( ty)
			'Wend

			' optional brackets
			If CParse( "(" )
				expr=ParseExpr()
				Parse ")"
				expr=New TCastExpr.Create( ty,expr,CAST_EXPLICIT )
			Else
				expr=ParseExpr()
'				Parse ")"
				expr=New TCastExpr.Create( ty,expr,CAST_EXPLICIT )
'				expr=New TIdentExpr.Create( id )
			EndIf
		Case "sizeof"
			NextToke
			' optional brackets
			If CParse( "(" )
				expr=ParseExpr()
				Parse ")"
				expr=New TSizeOfExpr.Create( expr )
			Else
				expr=ParseExpr()
				expr=New TSizeOfExpr.Create( expr )
			EndIf
		Case "len"
			NextToke
			' optional brackets
			If CParse( "(" )
				expr=ParseExpr()
				Parse ")"
				expr=New TLenExpr.Create( expr )
			Else
				expr=ParseExpr()
				expr=New TLenExpr.Create( expr )
			EndIf
		Case "abs"
			NextToke
			' optional brackets
			If CParse( "(" )
				expr=ParseExpr()
				Parse ")"
				expr=New TAbsExpr.Create( expr )
			Else
				expr=ParseExpr()
				expr=New TAbsExpr.Create( expr )
			EndIf
		Case "min"
			NextToke
			' optional brackets
			Local b:Int = CParse( "(" )

			expr=ParseExpr()
			Parse ","
			Local expr2:TExpr=ParseExpr()

			If b Then
				Parse ")"
			End If

			expr=New TMinExpr.Create( expr, expr2 )
		Case "max"
			NextToke
			' optional brackets
			Local b:Int = CParse( "(" )

			expr=ParseExpr()
			Parse ","
			Local expr2:TExpr=ParseExpr()

			If b Then
				Parse ")"
			End If

			expr=New TMaxExpr.Create( expr, expr2 )
		Case "string"
			Local id$=_toke
			Local ty:TType=ParseType()

			If CParse("ptr") Then
				ty = TType.MapToPointerType(ty)
			End If

			' string array
			ty = ParseArrayType(ty)
			'While CParse( "[]" )
			'	ty=New TArrayType.Create( ty)
			'Wend

			If CParse( "(" )
				expr=ParseExpr()
				Parse ")"
				expr=New TCastExpr.Create( ty,expr,CAST_EXPLICIT )
			Else
				expr=New TIdentExpr.Create( id )
			EndIf

		Case "varptr"
			NextToke
			expr=ParseExpr()
			expr=New TCastExpr.Create( New TVarPtrType, expr, CAST_EXPLICIT )
		Case "pi"
			NextToke
			expr=New TConstExpr.Create( New TDoubleType, Pi )
		Case "self"
			NextToke
			expr=New TSelfExpr
		Case "super"
			NextToke
			Parse "."
			If _toke="new"
				Err "Call to super class constructor must be first statement in a constructor."
			EndIf
			Local id$=ParseIdent()
			expr=New TInvokeSuperExpr.Create( id,ParseArgs( stmt ) )
		Default
			Select _tokeType
			Case TOKE_IDENT
				Local tok:TToker=New TToker.Copy( _toker )

				Local ty:TType=CParseIdentType()
				If ty
					expr=New TIdentTypeExpr.Create( ty )
				Else
					_toker=tok
					_toke=_toker.Toke()
					_tokeType=_toker.TokeType()
					expr=New TIdentExpr.Create( ParseIdent() )
					ty = ParseConstNumberType()
					
					If TArrayType(ty) Then
						If Not TArrayType(ty).elemType Then
							TArrayType(ty).elemType = New TIdentType.Create(TIdentExpr(expr).ident)
							expr=New TIdentTypeExpr.Create( ty )
						End If
					End If

				EndIf

				'expr=New TIdentExpr.Create( ParseIdent() )
			Case TOKE_INTLIT

				expr=New TConstExpr.Create( New TIntType,_toke )
				NextToke

				Local ty:TType = ParseConstNumberType()
				If ty Then
					TConstExpr(expr).ty = ty
				End If
			Case TOKE_LONGLIT
				expr=New TConstExpr.Create( New TLongType,_toke )
				NextToke
			Case TOKE_FLOATLIT
				expr=New TConstExpr.Create( New TFloatType,_toke )
				NextToke

				Local ty:TType = ParseConstNumberType()
				If ty Then
					TConstExpr(expr).ty = ty
				End If
			Case TOKE_STRINGLIT
				expr=New TConstExpr.Create( TType.stringType,BmxUnquote( _toke ) )
				_app.mapStringConsts(BmxUnquote( _toke ))
				NextToke
			Default
				Err "Expecting expression but encountered '"+_toke+"'"
			End Select
		End Select

		Repeat

			Select _toke
			Case "."
				NextToke
				expr=New TIdentExpr.Create( ParseIdent(),expr )
				
				ParseConstNumberType()
'DebugLog expr.ToString()
			Case "("

				If expr = Null Then
					NextToke
					expr=ParseExpr()
					Parse ")"
				Else
					expr=New TFuncCallExpr.Create( expr,ParseArgs( stmt ) )
				End If

			Case "["
				NextToke
				If CParse( ".." )
					If _toke="]"
						expr=New TSliceExpr.Create( expr,Null,Null )
					Else
						expr=New TSliceExpr.Create( expr,Null,ParseExpr() )
					EndIf
					Parse "]"
				Else
					Local from:TExpr=ParseExpr()
					If CParse( ".." )
						If _toke="]"
							expr=New TSliceExpr.Create( expr,from,Null )
						Else
							expr=New TSliceExpr.Create( expr,from,ParseExpr() )
						EndIf
						Parse "]"
					Else
						Local ind:TExpr[] = [from]
						Repeat
							If CParse("]") Then
								Exit
							End If

							Parse ","

							ind = ind + [ParseExpr()]
						Forever

						expr=New TIndexExpr.Create( expr,ind )
					EndIf
				EndIf
			Default
				Return expr
			End Select
		Forever

	End Method

	Method ParseUnaryExpr:TExpr()

		SkipEols

		Local op$=_toke
		Select op
		Case "+","-","~~","not"
			NextToke
			Local expr:TExpr=ParseUnaryExpr()
			Return New TUnaryExpr.Create( op,expr )
		End Select
		Return ParsePrimaryExpr( False )
	End Method

	Method ParseMulDivExpr:TExpr()
		Local expr:TExpr=ParseUnaryExpr()
		Repeat
			Local op$=_toke
			Select op
			Case "^","*","/","mod","shl","shr", "sar"
				NextToke
				Local rhs:TExpr=ParseUnaryExpr()
				expr=New TBinaryMathExpr.Create( op,expr,rhs )
			Default
				Return expr
			End Select
		Forever
	End Method

	Method ParseAddSubExpr:TExpr()
		Local expr:TExpr=ParseMulDivExpr()
		Repeat
			Local op$=_toke
			Select op
			Case "+","-"
				NextToke
				Local rhs:TExpr=ParseMulDivExpr()
				expr=New TBinaryMathExpr.Create( op,expr,rhs )
			Default
				Return expr
			End Select
		Forever
	End Method

	Method ParseBitandExpr:TExpr()
		Local expr:TExpr=ParseAddSubExpr()
		Repeat
			Local op$=_toke
			Select op
			Case "&","~~"
				NextToke
				Local rhs:TExpr=ParseAddSubExpr()
				expr=New TBinaryMathExpr.Create( op,expr,rhs )
			Default
				Return expr
			End Select
		Forever
	End Method

	Method ParseBitorExpr:TExpr()
		Local expr:TExpr=ParseBitandExpr()
		Repeat
			Local op$=_toke
			Select op
			Case "|"
				NextToke
				Local rhs:TExpr=ParseBitandExpr()
				expr=New TBinaryMathExpr.Create( op,expr,rhs )
			Default
				Return expr
			End Select
		Forever
	End Method

	Method ParseCompareExpr:TExpr()
		Local expr:TExpr=ParseBitorExpr()
		Repeat
			Local op$=_toke
			Select op
			Case "=","<",">","<=","=<",">=","=>","<>"
				NextToke
				' <= or =>
				If (op=">" And (_toke="=")) Or (op="=" And (_toke=">"))
					op:+_toke
					NextToke
				' <> or <= or =<
				Else If (op="<" And _toke=">") Or (op="<" And _toke="=") Or (op="=" And _toke="<")
					op:+_toke
					NextToke
				EndIf
				Local rhs:TExpr=ParseBitorExpr()
				expr=New TBinaryCompareExpr.Create( op,expr,rhs )
			Default
				Return expr
			End Select
		Forever
	End Method

	Method ParseAndExpr:TExpr()
		Local expr:TExpr=ParseCompareExpr()
		Repeat
			Local op$=_toke
			If op="and"
				NextToke
				Local rhs:TExpr=ParseCompareExpr()
				expr=New TBinaryLogicExpr.Create( op,expr,rhs )
			Else
				Return expr
			EndIf
		Forever
	End Method

	Method ParseOrExpr:TExpr()
		Local expr:TExpr=ParseAndExpr()
		Repeat
			Local op$=_toke
			If op="or"
				NextToke
				Local rhs:TExpr=ParseAndExpr()
				expr=New TBinaryLogicExpr.Create( op,expr,rhs )
			Else
				Return expr
			EndIf
		Forever
	End Method

	Method ParseExpr:TExpr()
		Return ParseOrExpr()
	End Method

Rem
	unused atm
	Method ReadTillNextToken:string(amount:int=1)
		'copy current toker and move one token forward
		local tok:TToker = New TToker.Copy(_toker)
		local result:string = _toker._toke
		for local i:int = 0 until amount
			NextTokeToker(tok)
			result :+ " "+ tok._toke
		Next
		return _toker._toke+" "+tok._toke
	End Method
End Rem
	
	Method ParseIfStmt( term$, elseIfEndIfReadAheadCheck:Int = False )

		Local tok:TToker
		'rules:
		'- the command "end" cannot be used as condition
		'- "endif" or "end if" is not allowed in singleline-ifs

		'if current toke is "if", move on to the next toke
		CParse "if"

		'read in the expression/condition following after "if"
		Local expr:TExpr=ParseExpr()

		'if current toke is "then", move to next, else stay at this
		'position -> makes "then" usage voluntary
		CParse "then"

		'create empty blocks for then/else
		Local thenBlock:TBlockDecl=New TBlockDecl.Create( _block )
		Local elseBlock:TBlockDecl=New TBlockDecl.Create( _block )

		'define if the current if is a "singleline if"
		'"singleline ifs" are not allowed to contain "endif" "end if"
		Local singleLineIf:Int = True

		'to know if it is a multiline or singleline if we have to check
		'for certain situations
		Select _toke
			Case "~n"
				'if a  <- newline
				'  print "a"
				'endif
				singleLineIf = False
			Case "if"
				'another "if" means the outer one is a singleline if!
				singleLineIf = True
			Case "else"
				'if ReadTillNextToken().toLower() = "else if"
				'	print "IF: found if X then Y else if ..."
				'else
				'	print "IF: found if X then Y else ..."
				'endif

				'also read "else if"
				singleLineIf = True
			Case "elseif"
				singleLineIf = True
		End Select


		'set thenBlock as the active block
		PushBlock( thenBlock )

		'now check each toke until we reach our desired term
		'for singleline-if this is "~n", for multiline-if this is
		'"endif" or "end if"
		If singleLineIf
			term = "~n"
		Else
			term = "end" 'endif, end if
		EndIf

		'only read until reaching the limit - or no valid toke was returned
		While _toke <> term
			Local currentToke:String = _toke

			Select currentToke
				'file end before endif/end/elseif
				Case ""
                   Err("Expecting expression but encountered end-of-file")
				'"endif" / "end if"
				Case "endif", "end"
					NextToke()
				
					If singleLineIf Then
						'check for "end"-command ("if a=1 end")
						If currentToke = "end" And (currentToke + _toke) <> "endif" Then
							ParseEndStmt(False)
						'found "end if"
						Else
							Err "'End If' without matching 'If'"
							Exit
						EndIf
					EndIf
				
					'If currentToke = "endif" or (currentToke + _toke)="endif"
					'	'do something if "endif/end if" happens ?
					'Endif

					'finish this if-statement
					Exit

				'"else" and "elseif" / "else if"
				Case "else","elseif"
'					print "parsing "+currentToke

					If _block = elseBlock
						Err("If statement can only have one 'else' block.")
					EndIf

					'switch from thenBlock to elseBlock
					PopBlock()
					PushBlock(elseBlock)

					'move to next token, might contain "if" for "else if"
					'doing it this way avoids to parse "elseif if" as
					'else-statement
					NextToke()
					If currentToke = "elseif" Or (currentToke + _toke)="elseif"
						'create a new if-statement and exit current handling
						ParseIfStmt(term, True)
						Exit
					EndIf
					
				Default
					'parse the current and next tokens
					ParseStmt()

					currentToke = _toke

					'handle the end-function and "end if"
					Select currentToke
						Case "end"
							'check next toke too
							NextToke()

							'found end-function
							If currentToke = "end" And (currentToke + _toke)<>"endif"
'								print "   parsing end .... handling"
								ParseEndStmt(False)
							'found "end if"
							Else
								If CParse("if") Then
									If singleLineIf Then
										Err "'End If' without matching 'If'"
									End If
									
									Exit
								End If
								
								'NextToke()
							EndIf
					End Select
			End Select
		Wend
		
		'change block
		PopBlock()

		'create a if-then[-else]-statement
		Local stmt:TIfStmt=New TIfStmt.Create( expr,thenBlock,elseBlock )
		_block.AddStmt stmt
	End Method


	Method ParseWhileStmt(loopLabel:TLoopLabelDecl = Null)
		Parse "while"

		Local expr:TExpr=ParseExpr()
		Local block:TBlockDecl=New TBlockDecl.Create( _block )

		PushBlock block
		While Not CParse( "wend" )
'			If CParse( "end" )
'				CParse "while"
'				Exit
'			EndIf
			ParseStmt

			' to handle "end" statement
			If _toke = "end" Then
				NextToke
				If _toke = "while" Then
					NextToke
					Exit
				Else
					ParseEndStmt(False)
				End If
			End If
		Wend
		PopBlock

		Local stmt:TWhileStmt=New TWhileStmt.Create( expr,block,loopLabel )

		_block.AddStmt stmt
	End Method

	Method ParseRepeatStmt(loopLabel:TLoopLabelDecl = Null)

		Parse "repeat"

		Local block:TBlockDecl=New TBlockDecl.Create( _block )

		PushBlock block
		While _toke<>"until" And _toke<>"forever"
			ParseStmt
		Wend
		PopBlock

		SetErr

		Local expr:TExpr
		If CParse( "until" )
			expr=ParseExpr()
		Else
			Parse "forever"
			expr=New TConstExpr.Create( New TBoolType,"" )
		EndIf

		Local stmt:TRepeatStmt=New TRepeatStmt.Create( block,expr,loopLabel )

		_block.AddStmt stmt
	End Method

	Method ParseForStmt(loopLabel:TLoopLabelDecl = Null)
'DebugStop
		Parse "for"

		Local varid$,varty:TType,varlocal:Int

		If CParse( "local" )
			varlocal=True
			varid=ParseIdent()
			'If Not CParse( ":=" )
				varty=ParseDeclType()
				If varty._flags & (TType.T_CHAR_PTR | TType.T_SHORT_PTR) Then
					DoErr "Illegal variable type"
				End If
				Parse( "=" )
			'EndIf
		Else
			varlocal=False
			varid=ParseIdent()

			' eat any type stuff
			ParseConstNumberType()

			Parse "="
		EndIf

		If CParse( "eachin" )
			Local expr:TExpr=ParseExpr()
			Local block:TBlockDecl=New TBlockDecl.Create( _block )

			PushBlock block
			While Not CParse( "next" )
				'If CParse( "end" )
				'	CParse "for"
				'	Exit
				'EndIf
				ParseStmt
			Wend
			PopBlock

			Local stmt:TForEachinStmt=New TForEachinStmt.Create( varid,varty,varlocal,expr,block,loopLabel )

			_block.AddStmt stmt

			Return
		EndIf

		Local from:TExpr=ParseExpr()

		Local op$
		If CParse( "to" )
			op="<="
		Else If CParse( "until" )
			op="<"
		Else
			Err "Expecting 'To' or 'Until'."
		EndIf

		Local term:TExpr=ParseExpr()

		Local stp:TExpr

		If CParse( "step" )
			stp=ParseExpr()
		Else
			stp=New TConstExpr.Create( New TIntType,"1" )
		EndIf

		Local init:TStmt,expr:TExpr,incr:TStmt

		If varlocal
			Local indexVar:TLocalDecl=New TLocalDecl.Create( varid,varty,from,0 )
			init=New TDeclStmt.Create( indexVar )
			expr=New TBinaryCompareExpr.Create( op,New TVarExpr.Create( indexVar ),term )
			incr=New TAssignStmt.Create( "=",New TVarExpr.Create( indexVar ),New TBinaryMathExpr.Create( "+",New TVarExpr.Create( indexVar ),stp ) )
		Else
			init=New TAssignStmt.Create( "=",New TIdentExpr.Create( varid ),from )
			expr=New TBinaryCompareExpr.Create( op,New TIdentExpr.Create( varid ),term )
			incr=New TAssignStmt.Create( "=",New TIdentExpr.Create( varid ),New TBinaryMathExpr.Create( "+",New TIdentExpr.Create( varid ),stp ) )
		EndIf

		Local block:TBlockDecl=New TBlockDecl.Create( _block )

		PushBlock block
		While Not CParse( "next" )
			'If CParse( "end" )
			'	CParse "for"
			'	Exit
			'EndIf
			ParseStmt
		Wend
		PopBlock

		NextToke

		Local stmt:TForStmt=New TForStmt.Create( init,expr,incr,block,loopLabel )

		_block.AddStmt stmt
	End Method

	Method ParseDefDataStmt(label:TLoopLabelDecl = Null)
		Parse "defdata"
		
		If AtEos() Then
			Err "Expecting expression but encountered " + DescribeToke(_toke)
		End If
		
		Local args:TExpr[]
		Local nargs:Int

		Repeat
			Local arg:TExpr
			If _toke And _toke<>"," arg=ParseExpr()
			If args.Length=nargs args=args + New TExpr[10]
			args[nargs]=arg
			nargs:+1
		Until Not CParse(",")
		args=args[..nargs]
		
		Local dataLabel:TDataLabelDecl
		If label Then
			dataLabel = New TDataLabelDecl.Create(label.ident, label.attrs)
		End If
		
		Local decl:TDefDataDecl = New TDefDataDecl.Create(args, dataLabel)
		
		_app.dataDefs.AddLast(decl)
		
	End Method

	Method ParseReadDataStmt()
		Parse "readdata"

		Local args:TExpr[]
		Local nargs:Int

		If Not AtEos() Then
			Repeat
				Local arg:TExpr
				If _toke And _toke<>"," arg=ParseExpr()
				If args.Length=nargs args=args + New TExpr[10]
				args[nargs]=arg
				nargs:+1
			Until Not CParse(",")
			args=args[..nargs]
		End If

		_block.AddStmt New TReadDataStmt.Create( args )
	End Method

	Method ParseRestoreDataStmt()
		Parse "restoredata"
		
		Local expr:TExpr = ParseExpr()

		_block.AddStmt New TRestoreDataStmt.Create( expr )
	End Method
	
	Method ParseReturnStmt()
		Parse "return"
		Local expr:TExpr
		If Not AtEos() expr=ParseExpr()
		_block.AddStmt New TReturnStmt.Create( expr )
	End Method

	Method ParseExitStmt()
		Parse "exit"
		Local expr:TExpr
		If Not AtEos() expr=ParseExpr()
		_block.AddStmt New TBreakStmt.Create(expr)
	End Method

	Method ParseContinueStmt()
		Parse "continue"
		Local expr:TExpr
		If Not AtEos() expr=ParseExpr()
		_block.AddStmt New TContinueStmt.Create(expr)
	End Method

	Method ParseTryStmt()
		Parse "try"

		Local block:TBlockDecl=New TBlockDecl.Create( _block )
		Local catches:TList=New TList

		PushBlock block
		While _toke<>"end" And _toke<>"endtry"
			If CParse( "catch" )
				Local id:String=ParseIdent()
				Local ty:TType
				If Not CParse(":") Then
					Parse "$"
					ty= TType.stringType
				Else
					ty=ParseType()
				End If
				Local init:TLocalDecl=New TLocalDecl.Create( id,ty,Null,0 )
				Local block:TBlockDecl=New TBlockDecl.Create( _block )
				catches.AddLast(New TCatchStmt.Create( init,block ))
				PopBlock
				PushBlock block
			Else
				ParseStmt

				If _toke = "end" Then
					NextToke
					If _toke = "try" Then
						' we are done with the try statement
						Exit
					Else
						ParseEndStmt(False)
					End If
				End If

			End If
		Wend

		PopBlock
		
		If Not CParse("endtry") Then
			' TODO : handle case of no catch - perhaps throw the exception again.
			'If Not catches.Length() Err "Try block must have at least one catch block"
			NextToke
			CParse "try"
		End If

		_block.AddStmt New TTryStmt.Create( block,TCatchStmt[](catches.ToArray()) )
	End Method

	Method ParseThrowStmt()
		Parse "throw"
		Local expr:TExpr = ParseExpr()
		_block.AddStmt New TThrowStmt.Create( expr )
	End Method

	Method ParseReleaseStmt()
		Parse "release"
		Local expr:TExpr = ParseExpr()
		_block.AddStmt New TReleaseStmt.Create( expr )
	End Method
	
	Method ParseAssertStmt()
		Parse "assert"
		Local expr:TExpr = ParseExpr()
		Local elseExpr:TExpr

		If _toke = "," Or _toke = "else" Then
			NextToke
			elseExpr = ParseExpr()
		End If

		_block.AddStmt New TAssertStmt.Create( expr, elseExpr )
	End Method

	Method ParseEndStmt(eatEnd:Int = True)
		If eatEnd Then
			Parse "end"
		End If

		_block.AddStmt New TEndStmt.Create( )
	End Method

	Method ParseSelectStmt()
		Parse "select"

		Local block:TBlockDecl=_block

		Local tmpVar:TLocalDecl=New TLocalDecl.Create( "",Null,ParseExpr(),,True )

		block.AddStmt New TDeclStmt.Create( tmpVar )

		While _toke<>"end" And _toke<>"default" And _toke<>"endselect"
			SetErr
			Select _toke
			Case "~n"
				NextToke
			Case "case"
				NextToke
				Local comp:TExpr
				Repeat
					Local expr:TExpr=New TVarExpr.Create( tmpVar )
					expr=New TBinaryCompareExpr.Create( "=",expr,ParseExpr() )
					If comp
						comp=New TBinaryLogicExpr.Create( "or",comp,expr )
					Else
						comp=expr
					EndIf
				Until Not CParse(",")

				Local thenBlock:TBlockDecl=New TBlockDecl.Create( _block )
				Local elseBlock:TBlockDecl=New TBlockDecl.Create( _block )

				Local ifstmt:TIfStmt=New TIfStmt.Create( comp,thenBlock,elseBlock )
				block.AddStmt ifstmt
				block=ifstmt.thenBlock

				PushBlock block
				Local fin:Int = False
				While _toke<>"case" And _toke<>"default" And _toke<>"end" And _toke<>"endselect"
					ParseStmt

					If _toke = "end" Then
						NextToke
						If _toke = "select" Then
							' we are done with the select statement, full exit
							fin = True
							Exit
						Else
							ParseEndStmt(False)
						End If
					End If
				Wend
				PopBlock

				block=elseBlock
				If fin Exit
			Default
				Err "Syntax error - expecting 'Case', 'Default' or 'End'."
			End Select
		Wend

		If _toke="default"
			NextToke
			PushBlock block
			While _toke<>"end" And _toke<>"endselect"
				SetErr
				Select _toke
				Case "case"
					Err "Case can not appear after default."
				Case "default"
					Err "Select statement can have only one default block."
				End Select
				ParseStmt

				If _toke = "end" Then
					NextToke
					If _toke = "select" Then
						Exit
					Else
						ParseEndStmt(False)
					End If
				End If
			Wend
			PopBlock
		EndIf

		SetErr

		If Not CParse("endselect") Then
			If Not CParse("select")
				Parse "end"
				Parse "select"
			End If
		End If
	End Method

	Method ParseRemStmt()
		Parse "rem"

' TODO : end/rem should be at the beginning of a line... ignore otherwise
		While _toke
			SkipEols()
'			If CParse( "endrem" ) Then
'DebugStop
			 	Local line:String = _toker._lines[_toker._line - 1].Trim().toLower()
				If line.startswith("endrem") Then
					Exit
				End If

				If CParse( "end" )
					CParse "rem"
				End If

				If line.startswith("end rem") Then
					Exit
				End If

'			EndIf
			NextToke
		Wend

		NextToke

	End Method

	Method ParseExternBlock(mdecl:TModuleDecl, attrs:Int)

		NextToke

		If _tokeType=TOKE_STRINGLIT
			DebugLog "EXTERN : " + ParseStringLit()
		End If


		attrs = attrs | DECL_EXTERN
		If CParse( "private" ) attrs=attrs|DECL_PRIVATE

		While _toke<>"endextern"
			If CParse( "end" )
				Parse "extern"
				Exit
			EndIf

			SetErr
			Select _toke
				Case "~n"
					NextToke
				Case "const","global"
					mdecl.InsertDecls ParseDecls( _toke,attrs )
				Case "type"
					mdecl.InsertDecl ParseClassDecl( _toke,attrs )
				Case "function"
					mdecl.InsertDecl ParseFuncDecl( _toke,attrs )
				Case "rem"
					ParseRemStmt()
			End Select

		Wend
		
		If _toke="endextern" Then
			NextToke
		End If

	End Method

	Method ParseStmt()
		SetErr
		Select _toke
			Case ";","~n"
				NextToke
			Case "rem"
				ParseRemStmt()
			Case "const","local","global"
				ParseDeclStmts
			' nested function - needs to get added to the "module"
			Case "function"
				_block.InsertDecl ParseFuncDecl( _toke,FUNC_NESTED)
			Case "return"
				ParseReturnStmt()
			Case "exit"
				ParseExitStmt()
			Case "continue"
				ParseContinueStmt()
			Case "if"
				ParseIfStmt( "" )
			Case "while"
				ParseWhileStmt()
			Case "repeat"
				ParseRepeatStmt()
			Case "for"
				ParseForStmt()
			Case "select"
				ParseSelectStmt()
			Case "assert"
				ParseAssertStmt()
			Case "try"
				ParseTryStmt()
			Case "throw"
				ParseThrowStmt()
			Case "end"
				ParseEndStmt()
			Case "extern"
				ParseExternBlock(_module, 0)
			Case "#"
				Local decl:TLoopLabelDecl = ParseLoopLabelDecl()
				NextToke
				Select _toke.ToLower()
					Case "while"
						ParseWhileStmt(decl)
					Case "repeat"
						ParseRepeatStmt(decl)
					Case "for"
						ParseForStmt(decl)
					Case "defdata"
						ParseDefDataStmt(decl)
					Default
						Err "Labels must appear before a loop or DefData statement"
				End Select
			Case "release"
				ParseReleaseStmt()
			Case "defdata"
				ParseDefDataStmt()
			Case "readdata"
				ParseReadDataStmt()
			Case "restoredata"
				ParseRestoreDataStmt()
			Default
				Local expr:TExpr=ParsePrimaryExpr( True )

				Select _toke.ToLower()
				'"=","*=","/=","+=","-=","&=","|=","~~=","Mod","Shl","Shr"
				Case "=",":*",":/",":+",":-",":&",":|",":~~","mod","shl","shr", ":shl", ":shr", "sar", ":sar", ":mod"
	'DebugLog _toke
					' remap symbols...
					For Local i:Int = 0 Until TToker._symbols.length
						Local sym$= TToker._symbols[i]
						If _toke.ToLower() = sym
							_toke = TToker._symbols_map[i]
							Exit
						EndIf
					Next


					If TIdentExpr( expr ) Or TIndexExpr( expr )
						Local op$=_toke
						NextToke
						If Not op.EndsWith( "=" ) And Not op.StartsWith("=")
							Parse "="
							op:+"="
						EndIf
						_block.AddStmt New TAssignStmt.Create( op,expr,ParseExpr() )
					Else
						Err "Assignment operator '"+_toke+"' cannot be used this way."
					EndIf
					Return
				End Select

				If TIdentExpr( expr )

					expr=New TFuncCallExpr.Create( expr,ParseArgs( True ) )

				Else If TFuncCallExpr( expr) Or TInvokeSuperExpr( expr ) Or TNewObjectExpr( expr )

				Else
					Err "Expression cannot be used as a statement."
				EndIf

				_block.AddStmt New TExprStmt.Create( expr )

		End Select
	End Method

	Method ParseDecl:TDecl( toke$,attrs:Int )
		SetErr
		Local id$=ParseIdent()
		Local ty:TType
		Local init:TExpr

		If attrs & DECL_EXTERN
			ty=ParseDeclType()

'			If CParse("(") Then
			If _toke = "(" Then

				' function pointer?
				Local decl:TFuncDecl = ParseFuncDecl("", attrs | FUNC_PTR)

				If Not ty Then
					ty = New TFunctionPtrType
					TFunctionPtrType(ty).func = decl
				Else
					decl.retType = ty
					ty = New TFunctionPtrType
					TFunctionPtrType(ty).func = decl
				End If

				TFunctionPtrType(ty).func.ident = id

			Else If toke = "const" Then
				If CParse("=") Then
					init=ParseExpr()
				End If
			End If
		Else If CParse( ":=" )
			init=ParseExpr()
		Else
			ty=ParseDeclType()

			If CParse( "=" )
				init=ParseExpr()
			Else If CParse( "[" )
				Local ln:TExpr[]
				Repeat
					If CParse(",") Then
						ln = ln + [New TNullExpr]
						Continue
					End If
					If CParse("]") Exit
					ln = ln + [ParseExpr()]
					If CParse("]") Exit
					Parse(",")
				Forever
				'Parse "]"
				ty = ParseArrayType(ty)
				'While CParse( "[]" )
				'	ty=New TArrayType.Create(ty)
				'Wend
				init=New TNewArrayExpr.Create( ty,ln)
				ty=New TArrayType.Create( ty, ln.length )
			Else If _toke = "(" Then
	 			' function pointer?

				Local fdecl:TFuncDecl = ParseFuncDecl("", FUNC_PTR)
				If toke = "field" Then
					fdecl.attrs :| FUNC_METHOD
				End If

				If Not ty Then
					ty = New TFunctionPtrType
					TFunctionPtrType(ty).func = fdecl
				Else
					fdecl.retType = ty
					ty = New TFunctionPtrType
					TFunctionPtrType(ty).func = fdecl
				End If

				TFunctionPtrType(ty).func.ident = ""

				While IsArrayDef()
					ty = ParseArrayType(ty)
				Wend
				'While CParse( "[]" )
				'	ty=New TArrayType.Create(ty)
				'Wend

				' check for function pointer init
				If CParse("=") Then
					init=ParseExpr()
				Else
					init=New TConstExpr.Create( ty,"" )
				End If

			Else If toke<>"const"
				init=New TConstExpr.Create( ty,"" )
			Else
				Err "Constants must be initialized."
			EndIf
		EndIf

		Local decl:TValDecl

		Select toke
		Case "global" decl=New TGlobalDecl.Create( id,ty,init,attrs )
		Case "field"  decl=New TFieldDecl.Create( id,ty,init,attrs )
		Case "const"  decl=New TConstDecl.Create( id,ty,init,attrs )
		Case "local"  decl=New TLocalDecl.Create( id,ty,init,attrs )
		End Select

		If decl.IsExtern()
'DebugStop
			If CParse( "=" )
				decl.munged=ParseStringLit()
			Else
				decl.munged=decl.ident
			EndIf

				If TFunctionPtrType(ty) Then
					TFunctionPtrType(ty).func.munged = decl.munged
					
					Local cdets:TCastDets = TCastDets(_externCasts.ValueForKey(TFunctionPtrType(ty).func.munged))
					If cdets Then
						TFunctionPtrType(ty).func.castTo = cdets.retType
						If cdets.noGen Then
							TFunctionPtrType(ty).func.noCastGen = True
						End If
						For Local i:Int = 0 Until cdets.args.length
							If i < TFunctionPtrType(ty).func.argDecls.length Then
								TFunctionPtrType(ty).func.argDecls[i].castTo = cdets.args[i]
							End If
						Next
					End If
	
				End If

		EndIf

		'meta data for variables
		If CParse( "{" ) Then
			'print "meta for variable: "+id+ " -> "+ParseMetaData()
			decl.metadata = ParseMetaData()
		EndIf

		Return decl
	End Method

	Method ParseDecls:TList( toke$,attrs:Int )
		If toke Parse toke

		Local decls:TList=New TList'<Decl>
		Repeat
			Local decl:TDecl=ParseDecl( toke,attrs )
			decls.AddLast decl
			If Not CParse(",") Return decls
		Forever
	End Method

	Method ParseDeclStmts()
		Local toke$=_toke
		NextToke
		Repeat
			Local decl:TDecl=ParseDecl( toke,0 )
			_block.AddStmt New TDeclStmt.Create( decl )
		Until Not CParse(",")
	End Method
	
	Method ParseLoopLabelDecl:TLoopLabelDecl()
		NextToke
		Local id:String = ParseIdent()
		Return New TLoopLabelDecl.Create(id, 0)
	End Method

	'handle end-of-line "dot dot return"-line connector
	'-> skips EOL tokens
	Method HandleDotsLineConnector(eatToke:Int = False)

		Local tok:TToker = New TToker.Copy(_toker)

		Local t:String = tok.NextToke()

		Local count:Int = tok.SkipSpace()
		For Local i:Int = 0 Until count
			NextToke
		Next
		
		t = tok._toke

		If t = "~r" Then
			t = tok.NextToke()
			If t = "~n" Then
				NextToke
				NextToke
			Else
				NextToke
			End If
		Else
			If t = "~n" Then
				NextToke
			End If
		End If
		
		If eatToke And Not count Then
			NextToke
		End If
	End Method

	'should return a specific "metadata object" ?
	' metadata is in the form : {key key=value key="value"}
	Method ParseMetaData:String()
		Local metaDataString:String = ""
		SkipEols

		Repeat
			
			If metaDataString Then
				metaDataString :+ " "
			End If
			
			Select _tokeType
				Case TOKE_INTLIT
					Err "Expecting '}' but encountered integer literal"
				Case TOKE_FLOATLIT
					Err "Expecting '}' but encountered floating point literal"
				Case TOKE_STRINGLIT
					Err "Expecting '}' but encountered string literal"
				Case TOKE_SYMBOL
					Err "Expecting '}' but encountered " + _toke
			End Select
			
			'append current token to metaDataString
			metaDataString :+ _toke

			'read next token
			NextToke()

			' got a value
			If CParse("=") Then
				
				If _tokeType = TOKE_IDENT Then
					Err "Meta data must be literal constant"
				End If
				
				metaDataString :+ "=" + _toke

				'read next token
				NextToke()
			Else
				metaDataString :+ "=1"	
			End If
			
			'reached end of meta data declaration
			If _toke="}" Then Exit
		Forever

		'continue to next token
		NextToke()

		'parse this into something
		Return metaDataString
	End Method


	Method ParseFuncDecl:TFuncDecl( toke$,attrs:Int )
		SetErr

		If toke Parse toke

		Local id$
		Local ty:TType
		Local meth:Int = attrs & FUNC_METHOD
		Local meta:String

		If attrs & FUNC_METHOD
			If _toke="new"
'DebugStop
				If attrs & DECL_EXTERN
					Err "Extern classes cannot have constructors"
				EndIf
				id=_toke
				NextToke
				attrs:|FUNC_CTOR
				attrs:&~FUNC_METHOD
			Else
				id=ParseIdent()
				ty=ParseDeclType()
				If ty._flags & (TType.T_CHAR_PTR | TType.T_SHORT_PTR) Then
					DoErr "Illegal function return type"
				End If

			EndIf
		Else
			If Not (attrs & FUNC_PTR) Then
				id=ParseIdent()
				ty=ParseDeclType()
				' can only return "$z" and "$w" from an extern function.
				If ty._flags & (TType.T_CHAR_PTR | TType.T_SHORT_PTR) And Not (attrs & DECL_EXTERN) Then
					DoErr "Illegal function return type"
				End If
			End If
		EndIf

		Local args:TArgDecl[]

		Parse "("
		SkipEols
		If _toke<>")"
			Local nargs:Int
			Repeat

				Local argId$=ParseIdent()

				Local ty:TType=ParseDeclType()

				' var argument?
				If CParse("var") Then
					ty = TType.MapToVarType(ty)
				End If

				Local init:TExpr
				' function pointer ?
				If _toke = "(" Then

					Local fdecl:TFuncDecl = ParseFuncDecl("", FUNC_PTR | DECL_ARG)

					If Not ty Then
						ty = New TFunctionPtrType
						TFunctionPtrType(ty).func = fdecl
					Else
						fdecl.retType = ty
						ty = New TFunctionPtrType
						TFunctionPtrType(ty).func = fdecl
					End If

					TFunctionPtrType(ty).func.ident = argId

				End If
				If CParse( "=" ) init=ParseExpr()
				Local arg:TArgDecl=New TArgDecl.Create( argId,ty,init )
				If args.Length=nargs args=args + New TArgDecl[10]
				args[nargs]=arg
				nargs:+1
				If _toke=")" Exit

				Parse ","
			Forever
			args=args[..nargs]
		EndIf
		Parse ")"

		Repeat
			If CParse( "final" )
				attrs:|DECL_FINAL
			Else If CParse( "abstract" )
				attrs:|DECL_ABSTRACT
			Else If CParse( "property" )
				If attrs & FUNC_METHOD
					attrs:|FUNC_PROPERTY
				Else
					Err "Only methods can be properties."
				EndIf
			Else If CParse( "nodebug" )
				' TODO : NoDebug
				attrs :| DECL_NODEBUG
			Else If CParse( "{" ) 'meta data
				' TODO : do something with the metadata
				'meta data for functions/methods
				'print "meta for func/meth: "+id+ " -> "+ParseMetaData()
				meta = ParseMetaData()
			Else If _tokeType=TOKE_STRINGLIT
				' "win32", etc
				' TODO ? something with this??
				ParseStringLit()
			Else
				Exit
			EndIf
		Forever

		Local funcDecl:TFuncDecl=New TFuncDecl.CreateF( id,ty,args,attrs )
		If meta Then
			funcDecl.metadata = meta
		End If

		If funcDecl.IsExtern() Or (attrs & FUNC_PTR)
			funcDecl.munged=funcDecl.ident

			' a normal function pointer definition *probably* can't be defined with a munged name?
			' If there is an equals here, one can assume it is for an initialisation...
			'If (Not (attrs & FUNC_PTR)) Or (attrs & FUNC_PTR And Not (attrs & DECL_ARG)) Then
			If Not (attrs & FUNC_PTR) Then
				If CParse( "=" )
					funcDecl.munged=ParseStringLit()
				End If

				'Array $resize hack!
				'If funcDecl.munged="$resize"
				'	funcDecl.retTypeExpr=TType.emptyArrayType
				'EndIf
			EndIf

			If funcDecl.munged Then
				' look up extern cast list
				Local cdets:TCastDets = TCastDets(_externCasts.ValueForKey(funcDecl.munged))
				If cdets Then
					funcDecl.castTo = cdets.retType
					If cdets.noGen Then
						funcDecl.noCastGen = True
					End If
					For Local i:Int = 0 Until cdets.args.length
						If i < funcDecl.argDecls.length Then
							funcDecl.argDecls[i].castTo = cdets.args[i]
						End If
					Next
				End If
			End If

			Return funcDecl
		EndIf

		If funcDecl.IsAbstract() Return funcDecl

		'Ok, only first statement of a constructor can call super constructor - not pretty, should be in semant.
		If attrs & FUNC_CTOR
			SkipEols
			If CParse( "super" )
				Parse "."
				If _toke="new"
					Local id$=_toke
					NextToke
					funcDecl.superCtor=New TInvokeSuperExpr.Create( id,ParseArgs( True ) )
					funcDecl.AddStmt New TExprStmt.Create( funcDecl.superCtor )
				Else
					Local id$=ParseIdent()
					funcDecl.AddStmt New TExprStmt.Create( New TInvokeSuperExpr.Create( id,ParseArgs( True ) ) )
				EndIf
			Else
					'Invoke super default ctor
					'funcDecl.superCtor=New InvokeSuperExpr( "new",[] )
					'funcDecl.AddStmt New TExprStmt( funcDecl.superCtor )
			EndIf
		EndIf

		PushBlock funcDecl
		While (Not meth And _toke.ToLower()<>"endfunction") Or (meth And _toke.ToLower()<>"endmethod")
			If CParse( "end" )
				If (Not meth And CParse("function")) Or (meth And CParse("method"))
					Exit
				End If

				' handle "end" statement
				ParseEndStmt(False)
			EndIf

			ParseStmt
		Wend
		PopBlock

		NextToke
		'If toke CParse toke

		Return funcDecl
	End Method

	Method ParseClassDecl:TClassDecl( toke$,attrs:Int )
		SetErr

		If toke Parse toke

		Local id$=ParseIdent()
		Local args:TClassDecl[]
		Local superTy:TIdentType
		Local imps:TIdentType[]
		Local meta:String

		If (attrs & CLASS_INTERFACE) And (attrs & DECL_EXTERN)
			Err "Interfaces cannot be extern."
		EndIf
Rem
		If CParse( "<" )

			If attrs & DECL_EXTERN
				Err "Extern classes cannot be generic."
			EndIf

			If attrs & CLASS_INTERFACE
				Err "Interfaces cannot be generic."
			EndIf

			If attrs & CLASS_TEMPLATEARG
				Err "Class parameters cannot be generic."
			EndIf

			Local nargs:Int
			Repeat
				Local decl:TClassDecl=ParseClassDecl( "",CLASS_TEMPLATEARG )
				If args.Length=nargs args=args + New TClassDecl[10]
				args[nargs]=decl
				nargs:+1
			Until Not CParse(",")
			args=args[..nargs]

			Parse ">"
		EndIf
End Rem
		If CParse( "extends" )
			'If attrs & CLASS_TEMPLATEARG
			'	Err "Extends cannot be used with class parameters."
			'EndIf

			If CParse( "null" )

				If attrs & CLASS_INTERFACE
					Err "Interfaces cannot extend null"
				EndIf

				If Not (attrs & DECL_EXTERN)
					Err "Only extern objects can extend null."
				EndIf

				superTy=Null

			Else If attrs & CLASS_INTERFACE

				Local nimps:Int
				Repeat
					If imps.Length=nimps imps=imps + New TIdentType[10]
					imps[nimps]=ParseIdentType()
					nimps:+1
				Until Not CParse(",")
				imps=imps[..nimps]
				superTy=New TIdentType.Create( "brl.classes.object" )
			Else
				superTy=ParseIdentType()
			EndIf
		Else
			If Not (attrs & DECL_EXTERN) Then
				superTy=New TIdentType.Create( "brl.classes.object" )
			End If
		EndIf
Rem
		If CParse( "implements" )

			If attrs & DECL_EXTERN
				Err "Implements cannot be used with external classes."
			EndIf

			If attrs & CLASS_INTERFACE
				Err "Implements cannot be used with interfaces."
			EndIf

			If attrs & CLASS_TEMPLATEARG
				Err "Implements cannot be used with class parameters."
			EndIf

			Local nimps:Int
			Repeat
				If imps.Length=nimps imps=imps + New TIdentType[10]
				imps[nimps]=ParseIdentType()
				nimps:+1
			Until Not CParse(",")
			imps=imps[..nimps]
		EndIf
End Rem
		Repeat
			If CParse( "final" )

				If attrs & CLASS_INTERFACE
					Err "Final cannot be used with interfaces."
				EndIf

				attrs:|DECL_FINAL

			Else If CParse( "abstract" )

				If attrs & CLASS_INTERFACE
					Err "Abstract cannot be used with interfaces."
				EndIf

				attrs:|DECL_ABSTRACT
			Else
				Exit
			EndIf
		Forever

		'check for metadata
		If CParse( "{" )
			' TODO : do something with the metadata
			'metadata for "type"s
			'print "meta for type: "+id+ " -> "+ParseMetaData()
			meta = ParseMetaData()
		End If


		Local classDecl:TClassDecl=New TClassDecl.Create( id,args,superTy,imps,attrs )
		
		If meta Then
			classDecl.metadata = meta
		End If

		If classDecl.IsExtern()
			classDecl.munged=classDecl.ident
			If CParse( "=" ) classDecl.munged=ParseStringLit()
		EndIf

		'If classDecl.IsTemplateArg() Return classDecl

		Local decl_attrs:Int=(attrs & DECL_EXTERN) | (attrs & DECL_NODEBUG)

		Local method_attrs:Int=decl_attrs|FUNC_METHOD | (attrs & DECL_NODEBUG)
		If attrs & CLASS_INTERFACE method_attrs:|DECL_ABSTRACT

		Repeat
			SkipEols
			Select _toke
			Case "end", "endtype"
				NextToke
				Exit
			Case "private"
				NextToke
				decl_attrs=decl_attrs | DECL_PRIVATE
			Case "public"
				NextToke
				decl_attrs=decl_attrs & ~DECL_PRIVATE
			Case "const","global","field"
				If (attrs & CLASS_INTERFACE) And _toke<>"const"
					Err "Interfaces can only contain constants and methods."
				EndIf
				classDecl.InsertDecls ParseDecls( _toke,decl_attrs )
			Case "method"
				Local decl:TFuncDecl=ParseFuncDecl( _toke,method_attrs )
				If decl.IsCtor() decl.retTypeExpr=New TObjectType.Create( classDecl )
				classDecl.InsertDecl decl
			Case "function"
				If (attrs & CLASS_INTERFACE) And _toke<>"const"
					Err "Interfaces can only contain constants and methods."
				EndIf
				Local decl:TFuncDecl=ParseFuncDecl( _toke,decl_attrs )
				classDecl.InsertDecl decl
			Case "rem"
				ParseRemStmt()
			Default
				Err "Syntax error - expecting class member declaration, not '" + _toke + "'"
			End Select
		Forever

		If toke CParse toke

		Return classDecl
	End Method

	Method ParseModuleDecl:String( toke$,attrs:Int )
		NextToke

		' namespace . module
		Return ParseModPath().ToLower()
	End Method

	Method ParseModPath$()
		Local path$=ParseIdent()
		While CParse( "." )
			path:+"."+ParseIdent()
		Wend
		Return path
	End Method

	Method ExtractModIdent$( modpath$ )
		Local i:Int=modpath.FindLast( "." )
		If i<>-1 Return modpath[i+1..]
		Return modpath
	End Method

	Method ImportFile( filepath$ )

		If filepath.Endswith(".bmx") Then

			Local origPath:String = RealPath(filepath)
			Local path:String = OutputFilePath(origPath, FileMung(), "i")

			If FileType( path )<>FILETYPE_FILE
				Err "File '"+ path +"' not found."
			EndIf


			If _module.imported.Contains( path ) Return

			Local modpath:String
			If opt_buildtype = BUILDTYPE_MODULE Then

				Local dir:String = ExtractDir(origPath).ToLower()
				dir = dir[dir.findLast("/") + 1..]
				If dir.EndsWith(".mod") Then
					dir = ""
				Else
					dir :+ "_"
				End If
				Local file:String = StripDir(origPath).ToLower()

				modpath = opt_modulename + "_" + dir + StripExt(file)
			Else
				modpath = StripExt(filepath)
			End If

			'sanitize the path, remove non-allowed chars
			modpath = TStringHelper.Sanitize(modpath.ToLower())

			' try to import interface
			Local par:TIParser = New TIParser
			If par.ParseModuleImport(_module, modpath, origPath, path, , , filepath) Return
		Else If filepath.startswith("-") Then
			If Not _app.fileimports.Contains(filepath) Then
				_app.fileimports.AddLast filepath
			End If
		Else
			If filepath.EndsWith(".h") And filepath.Find("*") = -1 Then
				_app.headers.AddLast filepath
			End If
		End If

	End Method

	Method ImportAllModules(attrs:Int)

		' get all brl and pub modules
		Local mods:TList = EnumModules("brl")
		mods = EnumModules("pub", mods)

		For Local m:String = EachIn mods
			ImportModule(m, attrs)
		Next

	End Method
	
	Method ImportModule( modpath$,attrs:Int )
		SetErr
		
		modpath = modpath.ToLower()
		Local basepath:String = ModulePath(modpath.ToLower())

		If _module.imported.Contains( basepath ) Return

		' try to import interface
		Local par:TIParser = New TIParser

		If par.ParseModuleImport(_module, modpath, basepath, , , attrs) Return

'DebugStop

		'Local mdecl:TDecl=_app.imported.ValueForKey( basepath )

		'If Not mdecl
		'	mdecl=ParseModule( filepath,_app )
		'EndIf

		'_module.imported.Insert mdecl.filepath,mdecl

		'If Not (attrs & DECL_PRIVATE) _module.pubImported.Insert mdecl.filepath,mdecl

		'_module.InsertDecl New AliasDecl( mdecl.ident,mdecl,attrs )
'End Rem
	End Method

	Method ValidateModIdent( id$ )
		If id.Length
			If IsAlpha( id[0] ) Or id[0]="_"[0]
				Local err:Int
				For Local i:Int=1 Until id.Length
					If IsAlpha( id[i] ) Or IsDigit( id[i] ) Or id[i]="_"[0] Continue
					err=1
					Exit
				Next
				If Not err Return
			EndIf
		EndIf
		Err "Invalid module identifier '"+id+"'."
	End Method

	Method MungAppDecl(app:TAppDecl)
		If opt_buildtype = BUILDTYPE_MODULE And opt_ismain Then
			app.munged = MungModuleName(opt_modulename)
		Else If opt_buildtype = BUILDTYPE_MODULE Then
			Local dir:String = ExtractDir(opt_filepath).ToLower()
			dir = dir[dir.findLast("/") + 1..]
			If dir.EndsWith(".mod") Then
				dir = ""
			Else
				dir :+ "_"
			End If
			app.munged = "_bb_" + opt_modulename + "_" + dir + StripExt(StripDir(opt_filepath).ToLower())
		Else
			' main application file?
			If opt_apptype Then
				app.munged = "_bb_main"
			Else
				Local dir:String = ExtractDir(opt_filepath).ToLower()
				dir = dir[dir.findLast("/") + 1..]
				If dir.EndsWith(".mod") Then
					dir = dir.Replace(".mod", "")
				End If
				Local file:String = StripDir(opt_filepath).ToLower()
				app.munged = "_bb_" + dir + "_" + StripExt(file)
			End If
		End If

		'sanitize, remove non-allowed chars
		app.munged = TStringHelper.Sanitize(app.munged)
	End Method

	' load external cast defs
	Method LoadExternCasts(path:String)

		For Local externs:Int = 0 Until 3
		
			Local ePath:String
		
			' we will iterate through all possibilities as there may be different sets
			' of explicit casts/no gen funcs for each.
			Select externs
				Case 0
					' eg. file.win32.x86.x
					ePath = StripExt(path) + "." + opt_platform + "." + opt_arch + ".x"
				Case 1
					' eg. file.win32.x
					ePath = StripExt(path) + "." + opt_platform + ".x"
				Case 2
					' eg. file.x
					ePath = StripExt(path) + ".x"
			End Select


			If FileType(ePath) = FILETYPE_FILE Then
	
				Local toker:TToker=New TToker.Create( ePath,LoadText( ePath ) )
				toker.NextToke
	
				While True
	
					SkipEolsToker(toker)
	
					If toker._tokeType = TOKE_EOF Exit
	
					Local rt$=toker._toke
					NextTokeToker(toker)
					
					If CParseToker(toker,"*") Then
						rt:+ "*"
	
						If CParseToker(toker,"*") Then
							rt:+ "*"
						End If
					End If
	
	
					Local dets:TCastDets = New TCastDets
	
					' fname
					Local fn$=toker._toke
					NextTokeToker(toker)
	
					dets.name = fn
					dets.retType = rt
	
					_externCasts.Insert(fn, dets)
	
					' args
					ParseToker(toker, "(")
	
					If CParseToker(toker, ")") Then
	
						' don't generate header extern
						If CParseToker(toker, "!") Then
							dets.noGen = True
						End If
	
						Continue
					End If
	
					Local i:Int = 0
					Repeat
						Local at$=toker._toke
	
						If CParseToker(toker, "const") Then
							at :+ " " + toker._toke
						End If
	
						If CParseToker(toker, "unsigned") Then
							at :+ " " + toker._toke
						End If
	
						NextTokeToker(toker)
						If CParseToker(toker, "*") Then
							at:+ "*"
	
							If CParseToker(toker, "*") Then
								at:+ "*"
							End If
						End If
	
						' function pointer
						If CParseToker(toker, "(") Then
	
							ParseToker(toker, "*")
							ParseToker(toker, ")")
							at :+ "(*)"
	
							ParseToker(toker, "(")
							at :+ "("
	
							While Not CParseToker(toker, ")")
								NextTokeToker(toker)
								at :+ toker._toke
							Wend
	
							at :+ ")"
						End If
	
	
						dets.args :+ [at]
	
						If toker._toke=")" Exit
						ParseToker(toker, ",")
	
						i:+ 1
					Forever
	
					NextTokeToker(toker)
	
					' don't generate header extern
					If CParseToker(toker, "!") Then
						dets.noGen = True
					End If
	
				Wend
				
			End If
			
		Next

	End Method


	Method ParseCurrentFile:Int(path:String, attrs:Int)

		LoadExternCasts(path)

		While _toke
			SetErr
			Select _toke.toLower()
			Case "~n"
				NextToke
			Case "public"
				NextToke
				attrs=attrs & ~DECL_PRIVATE
			Case "private"
				NextToke
				attrs=attrs | DECL_PRIVATE
			Case "extern"

				ParseExternBlock(_module, attrs)
Rem
				NextToke

				If _tokeType=TOKE_STRINGLIT
					DebugLog "EXTERN : " + ParseStringLit()
				End If


				attrs=DECL_EXTERN
				If CParse( "private" ) attrs=attrs|DECL_PRIVATE


				While _toke<>"endextern"
					If CParse( "end" )
						If Parse("extern")
							Exit
						End If
					EndIf

					SetErr
					Select _toke
						Case "~n"
							NextToke
						Case "const","global"
							_module.InsertDecls ParseDecls( _toke,attrs )
						Case "type"
							_module.InsertDecl ParseClassDecl( _toke,attrs )
						Case "function"
							_module.InsertDecl ParseFuncDecl( _toke,attrs )
						Case "rem"
							ParseRemStmt()
					End Select

				Wend

				attrs = 0
End Rem
			Case "const"
				_module.InsertDecls ParseDecls( _toke,attrs )
			Case "global"
				Local list:TList = ParseDecls( _toke,attrs )
				_module.InsertDecls list
				For Local gdecl:TGlobalDecl = EachIn list
					gdecl.attrs :| DECL_INITONLY
					_block.AddStmt New TDeclStmt.Create( gdecl )
				Next
			Case "type"
				_module.InsertDecl ParseClassDecl( _toke,attrs )
			'Case "interface"
			'	_module.InsertDecl ParseClassDecl( _toke,attrs|CLASS_INTERFACE|DECL_ABSTRACT )
			Case "function"
				_module.InsertDecl ParseFuncDecl( _toke,attrs )
			Case "rem"
				ParseRemStmt()
			Case "incbin"
				NextToke
				Local s:String = ParseStringLit()
				_app.mapStringConsts(s)
				_app.incbins.AddLast(New TIncbin.Create(s, path))
			Case "include"
				'include command is NOT just a pattern to replace with
				'content. BlitzMax parses each included file before the
				'content gets appended to the source (right before
				'semanting or analyzing content)
				NextToke
				Local includeFile:String = ParseStringLit()

				'convert the URI of the to include file as it might be
				'a relative one
				includeFile = RealPath(includeFile)

				'instead of merging the data of multiple parsers, the
				'same parser is used for all included files - but each
				'of them uses an individual toker

				'instead of "LoadText" "PreProcess" is used to include
				'handling of conditionals and comments
				Local includeSource:String = PreProcess(includeFile)
				Local includeToker:TToker = New TToker.Create(includeFile, includeSource)

				'backup old vars
				Local oldToker:TToker = Self._toker

				'assign temporary vars
				Self._toker = includeToker

				'parse the include file
				parseCurrentFile(includeFile, attrs)

				'restore backup vars
				Self._toker = oldToker

				'move on to next toke (after include "xyz.bmx")
				NextToke

Rem
	old idea
				'each parser holds multiple "_blocks" (TBlockDecl) in a
				'list named "_blockStack" (TList)
				'so the idea is to parse the included file and append
				'their blocklist to the calling one
				'instead of "LoadText" "PreProcess" is used to include
				'handling of conditionals and comments
				Local includeSource:String = PreProcess(includeFile)
				Local includeToker:TToker = New TToker.Create(includeFile, includeSource)

				Local includeParser:TParser = New TParser.Create(includeToker, _app)
				includeParser.parseMain()

				If includeParser._blockStack
					For local blockDecl:TBlockDecl = EachIn includeParser._blockStack
						_blockStack.AddLast(blockDecl)
					Next
				Endif

				If includeParser._module and includeParser._module._decls
					For local decl:TDecl = EachIn includeParser._module._decls
						'skip "localMain"-function
						if decl.ident.ToLower() = "LocalMain".toLower() then continue

						print "appending decl: "+decl.ToString()
						_module._decls.AddLast(decl)
					Next
				Endif
endrem

			Default
				ParseStmt
				'Err "Syntax error - expecting declaration."
			End Select
		Wend

		Return attrs
	End Method


	Method ParseMain()

		SkipEols

		Local mattrs:Int
		'If CParse( "strict" ) mattrs:|MODULE_STRICT
		'If CParse( "superstrict" ) mattrs:|MODULE_SUPERSTRICT

		Local path$=_toker.Path()
		Local ident$=StripAll( path )
		Local munged$	'="bb_"+ident+"_"

		If opt_buildtype = BUILDTYPE_MODULE And opt_ismain
			ValidateModIdent ident
		Else If opt_buildtype = BUILDTYPE_MODULE Then
			Local dir:String = ExtractDir(opt_filepath).ToLower()
			dir = dir[dir.findLast("/") + 1..]
			If dir.EndsWith(".mod") Then
				dir = ""
			Else
				dir :+ "_"
			End If

			munged = opt_modulename + "_" + dir + ident

			'sanitize, remove non-allowed chars
			munged = TStringHelper.Sanitize(munged.ToLower())
		End If

		If opt_ismain Then 'And opt_modulename <> "brl.blitz" Then
			ident = opt_modulename
		End If

		_module=New TModuleDecl.Create( ident,munged,path,mattrs )

		_module.imported.Insert path,_module

		_app.InsertModule _module

		' mung the app decl
		MungAppDecl(_app)


		If opt_buildtype = BUILDTYPE_MODULE And opt_modulename = "brl.blitz" Then
			' import Object and String definitions
			Local par:TIParser = New TIParser
			par.ParseModuleImport(_module, "brl.classes", modulepath("brl.blitz"), modulepath("brl.blitz") + "/blitz_classes.i")

			' set up built-in keywords
			par = New TIParser
			par.ParseModuleImport(_module, "brl.blitzkeywords", "", "", MakeKeywords())
		End If

		' don't import ourself
		If opt_modulename <> "brl.blitz" Then
			Local par:TIParser = New TIParser
			par.ParseModuleImport(_module, "brl.blitz", modulepath("brl.blitz"), , , MODULE_ACTUALMOD)
		End If

		Local mainFunc:TFuncDecl = New TFuncDecl.CreateF("__LocalMain", New TIntType,Null,0)
'DebugStop
		'_app.InsertDecl mainFunc
		_module.insertDecl(mainFunc)
		'Local mainBlock:TBlockDecl = New TBlockDecl.Create( _block )


		' import all brl and pub modules if we haven't specified one
		If opt_buildtype <> BUILDTYPE_MODULE And Not opt_framework Then
			ImportAllModules MODULE_ACTUALMOD
		Else
			If opt_framework Then
				Local par:TIParser = New TIParser
				par.ParseModuleImport(_module, opt_framework, modulepath(opt_framework), , , MODULE_ACTUALMOD)
			End If
		End If


		Local attrs:Int

		'Parse header - imports etc.
		While _toke
			SetErr
			Select _toke.ToLower()
			Case "~n"
				NextToke
			Case "public"
				NextToke
				attrs=attrs & ~DECL_PRIVATE
			Case "private"
				NextToke
				attrs=attrs | DECL_PRIVATE
			Case "import"
				NextToke
				If _tokeType=TOKE_STRINGLIT
' TODO
					'ImportFile ReplaceEnvTags( ParseStringLit() )
					ImportFile ParseStringLit()
				Else
					ImportModule ParseModPath(),attrs | MODULE_ACTUALMOD
				EndIf
			Case "framework"
				NextToke
				ImportModule ParseModPath(),attrs
			Case "alias"
				NextToke
				Repeat
					Local ident$=ParseIdent()
					Parse "="

					Local decl:Object
					Local scope:TScopeDecl=_module

					_env=_module	'naughty! Shouldn't be doing GetDecl in parser...

					Repeat
						Local id$=ParseIdent()
						decl=scope.FindDecl( id )
						If Not decl Err "Identifier '"+id+"' not found."
						If Not CParse( "." ) Exit
						scope=TScopeDecl( decl )
						If Not scope Or TFuncDecl( scope ) Err "Invalid scope '"+id+"'."
					Forever

					_env=Null	'/naughty

					_module.InsertDecl New TAliasDecl.Create( ident,decl,attrs )

				Until Not CParse(",")
			Case "module"
				Local m:String = ParseModuleDecl(_toke, attrs)

				If m.ToLower() <> opt_modulename Then
					Err "Module does not match commandline module"
				End If

				'sanitize, remove non-allowed chars
				_module.munged = TStringHelper.Sanitize(m)
			Case "rem"
				ParseRemStmt()
			Case "nodebug"
				mainFunc.attrs :| DECL_NODEBUG
				attrs :| DECL_NODEBUG
				NextToke
			Case "strict"
				If _module.attrs & (MODULE_STRICT | MODULE_SUPERSTRICT) Then
					Err "Strict or SuperStrict already specified"
				End If

				_module.attrs :| MODULE_STRICT
				nextToke
			Case "superstrict"
				If _module.attrs & (MODULE_STRICT | MODULE_SUPERSTRICT) Then
					Err "Strict or SuperStrict already specified"
				End If

				_module.attrs :| MODULE_SUPERSTRICT
				opt_issuperstrict = True
				nextToke
			Case "moduleinfo"
				NextToke
				Local info:String = ParseStringLit()
				_module.modInfo.AddLast(info)
			Default
				Exit
			End Select
		Wend

		' app code
		PushBlock(mainFunc)

		'Parse main app
		attrs = ParseCurrentFile(path, attrs)

		PopBlock
	End Method

	Method ParseModule()

	End Method


	Method Create:TParser( toker:TToker,app:TAppDecl )
		_toke="~n"
		_toker=toker
		_app=app
		SetErr
		NextToke
		Return Self
	End Method
End Type

Function Eval$( toker:TToker,_type:TType )
	Local src$
	While toker.Toke() And toker.Toke()<>"'" And toker.Toke()<>"~n" And toker.Toke()<>"~r"
		src:+toker.Toke()
		toker.NextToke()
	Wend
	Local t:String=EvalS( src,_type )
	Return t
End Function

Function PreProcessNextToke$(toker:TToker)

	Repeat
		toker.NextToke()
	Until toker.tokeType()<>TOKE_SPACE

	Return toker._toke
End Function

Function PreProcess$( path$ )

	Local ifnest:Int,con:Int=1,line:Int,source:TStringList=New TStringList

	Local toker:TToker=New TToker.Create( path,LoadText( path ) )

	PreProcessNextToke(toker)

	Repeat

		If line
			source.AddLast "~n"
			While toker.Toke() And toker.Toke()<>"~n" And toker.TokeType()<>TOKE_LINECOMMENT
				PreProcessNextToke(toker)
			Wend
			If Not toker.Toke() Exit
			PreProcessNextToke(toker)
		EndIf
		line:+1

		_errInfo=toker.Path()+"<"+toker.Line()+">"

		If toker.TokeType()=TOKE_SPACE PreProcessNextToke(toker)

		If toker.Toke()<>"?"
			If con
				Local line$
				While toker.Toke() And toker.Toke()<>"~n" And toker.TokeType()<>TOKE_LINECOMMENT
					Local toke$=toker.Toke()
					line:+toke
					toker.NextToke()
				Wend
				If line source.AddLast line

			EndIf
			Continue
		EndIf

		Local stm$= PreProcessNextToke(toker).ToLower()
		'toker.NextToke()

		Local isNot:Int = False

		If stm = "not" Then
			If toker.TokeType()=TOKE_SPACE PreProcessNextToke(toker)
			stm = toker.Toke().ToLower()
			isNot = True
		End If

		'If stm="end" Or stm="else"
		'	If toker.TokeType()=TOKE_SPACE toker.NextToke()
		'	If toker.Toke().ToLower()="if"
		'		toker.NextToke()
		'		stm:+"if"
		'	EndIf
		'EndIf
Rem
Debug	True if program is being compiled in debug mode.
Threaded	True if program is being compiled in threaded mode.
Win32	True if program is being compiled for the Windows operating system.
MacOS	True if program is being compiled for the MacOS operating system.
Linux	True if program is being compiled for the Linux operating system.
X86	True if program is being compiled for the Intel CPU.
PPC	True if program is being compiled for the PowerPC CPU.
MacOSX86	True if program is being compiled for an Intel Mac.
MacOSPPC	True if program is being compiled for a PowerPC Mac.
BigEndian	True if program is being compiled for a big endian CPU.
LittleEndian
End Rem
		Select stm
		Case "~r", "~n"
			'ifnest = 0
			con = 1

		Default

			' test for EOF
			If Not toker.Toke() Exit

			con = 0
			Try
				If Eval( toker,New TIntType ) = "1" con = 1
			Catch error:String
				con = 0
			End Try

Rem
		Case "macos", "macosx86", "x86", "littleendian", "bigendian"
			con = 1
		'	If con=ifnest
		'		If Eval( toker,TType.intType ) con:+1
		'	EndIf
		'
			ifnest = 1

'		Case "rem"
'
'			ifnest:+1
		Case "threaded", "win32", "linux", "ppc", "win32x86", "linuxx86", "macosppc"

			If isNot Then
				con = 1
			Else
				con = 0
			End If

			ifnest = 1

		Case "else","elseif"

			If Not ifnest Err "#Else without #If"

			If con=ifnest
				con=-con
			Else If con=ifnest-1
				If stm="elseif"
					If Eval( toker,TType.intType ) con:+1
				Else
					con:+1
				EndIf
			EndIf

		Case "end","endif"

			If Not ifnest Err "#End without #If"

			ifnest:-1
			If con<0 con=-con
			If ifnest<con con=ifnest

'		Case "print"

'			If con=ifnest
' TODO
				'Print ReplaceEnvTags( Eval( toker,TType.stringType ) )
'			EndIf

'		Case "error"

'			If con=ifnest
' TODO
				'Err ReplaceEnvTags( Eval( toker,TType.stringType ) )
'			EndIf

		Default
			Err "Unrecognized preprocessor directive '"+stm+"'."
End Rem
		End Select
	Forever

	Return source.Join( "" )
End Function

Function ParseModule:TModuleDecl( path$,app:TAppDecl )

	'Local source$=PreProcess( path )
	Local source:String = LoadText(path)

	Local toker:TToker=New TToker.Create( path,source )

	Local parser:TParser=New TParser.Create( toker,app )

	parser.ParseMain

	Return parser._module
End Function

'***** PUBLIC API ******

Function ParseApp:TAppDecl( path$ )

	Local app:TAppDecl=New TAppDecl

	_appInstance = app

	Local source$=PreProcess( path )
	'Local source:String = LoadString(path)

	Local toker:TToker=New TToker.Create( path,source )

	Local parser:TParser=New TParser.Create( toker,app )
	parser.ParseMain

	Return app
End Function

Function MungModuleName:String(ident:Object)
	Local mung:String
	If String(ident) Then
		Local id:String = String(ident)
		mung = "__bb_" + id + "_" + id[id.Find(".") + 1..]
	Else
		Local mdecl:TModuleDecl = TModuleDecl(ident)
		If mdecl Then
			Local id:String = mdecl.ident
			Local dir:String = ExtractDir(mdecl.filepath).ToLower()
			dir = dir[dir.findLast("/") + 1..]
			If dir.EndsWith(".mod") Then
				dir = ""
			Else
				dir :+ "_"
			End If
			mung = "__bb_" + id + "_" + dir + id[id.Find(".") + 1..]
		End If
	End If

	'return sanitized, remove non-allowed chars
	Return TStringHelper.Sanitize(mung)
End Function

Function EvalS$( source$,ty:TType )

	Local env:TScopeDecl=New TScopeDecl

Rem
Debug	True if program is being compiled in debug mode.
Threaded	True if program is being compiled in threaded mode.
Win32	True if program is being compiled for the Windows operating system.
MacOS	True if program is being compiled for the MacOS operating system.
Linux	True if program is being compiled for the Linux operating system.
X86	True if program is being compiled for the Intel CPU.
PPC	True if program is being compiled for the PowerPC CPU.
MacOSX86	True if program is being compiled for an Intel Mac.
MacOSPPC	True if program is being compiled for a PowerPC Mac.
BigEndian	True if program is being compiled for a big endian CPU.
LittleEndian
End Rem

	' debug/release
	env.InsertDecl New TConstDecl.Create( "debug",New TIntType,New TConstExpr.Create( New TIntType,opt_debug ),0 )
	'env.InsertDecl New TConstDecl.Create( "release",TType.intType,New TConstExpr.Create( TType.intType,opt_release ),0 )

	' threaded
	env.InsertDecl New TConstDecl.Create( "threaded",New TIntType,New TConstExpr.Create( New TIntType,opt_threaded ),0 )

	' macos
	env.InsertDecl New TConstDecl.Create( "macos",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="macos" ),0 )
	env.InsertDecl New TConstDecl.Create( "macosx86",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="macos" And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "macosppc",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="macos" And opt_arch="ppc"),0 )
	env.InsertDecl New TConstDecl.Create( "macosx64",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="macos" And opt_arch="x64"),0 )

	' windows
	env.InsertDecl New TConstDecl.Create( "win32",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="win32" ),0 )
	env.InsertDecl New TConstDecl.Create( "win32x86",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="win32" And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "win32x64",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="win64" And opt_arch="x64") Or (opt_platform="win32" And opt_arch="x64")),0 )
	env.InsertDecl New TConstDecl.Create( "win64",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="win64" And opt_arch="x64") Or (opt_platform="win32" And opt_arch="x64")),0 )

	' linux
	env.InsertDecl New TConstDecl.Create( "linux",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="linux" Or opt_platform="android" Or opt_platform="raspberrypi")),0 )
	env.InsertDecl New TConstDecl.Create( "linuxx86",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="linux" Or opt_platform="android") And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "linuxx64",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="linux" Or opt_platform="android") And opt_arch="x64"),0 )
	env.InsertDecl New TConstDecl.Create( "linuxARM",New TIntType,New TConstExpr.Create( New TIntType, ((opt_platform="android" Or opt_platform="linux") And (opt_arch="arm" Or opt_arch="armeabi" Or opt_arch="armeabiv7a" Or opt_arch="arm64v8a")) Or (opt_platform="raspberrypi" And opt_arch="arm")),0 )

	' android
	env.InsertDecl New TConstDecl.Create( "android",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" ),0 )
	env.InsertDecl New TConstDecl.Create( "androidx86",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "androidx64",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And opt_arch="x64"),0 )
	env.InsertDecl New TConstDecl.Create( "androidarm",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And (opt_arch="arm" Or opt_arch="armeabi" Or opt_arch="armeabiv7a" Or opt_arch="arm64v8a") ),0 )
	env.InsertDecl New TConstDecl.Create( "androidarmeabi",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And opt_arch="armeabi"),0 )
	env.InsertDecl New TConstDecl.Create( "androidarmeabiv7a",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And opt_arch="armeabiv7a"),0 )
	env.InsertDecl New TConstDecl.Create( "androidarm64v8a",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And opt_arch="arm64v8a"),0 )

	' raspberrypi - ARM only
	env.InsertDecl New TConstDecl.Create( "raspberrypi",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="raspberrypi" And opt_arch="arm"),0 )
	env.InsertDecl New TConstDecl.Create( "raspberrypiARM",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="raspberrypi" And opt_arch="arm"),0 )

	' arch
	env.InsertDecl New TConstDecl.Create( "ppc",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="ppc" ),0 )
	env.InsertDecl New TConstDecl.Create( "x86",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="x86" ),0 )
	env.InsertDecl New TConstDecl.Create( "x64",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="x64" ),0 )
	env.InsertDecl New TConstDecl.Create( "arm",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="arm" Or opt_arch="armeabi" Or opt_arch="armeabiv7a" Or opt_arch="arm64v8a" ),0 )
	env.InsertDecl New TConstDecl.Create( "armeabi",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="armeabi" ),0 )
	env.InsertDecl New TConstDecl.Create( "armeabiv7a",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="armeabiv7a" ),0 )
	env.InsertDecl New TConstDecl.Create( "arm64v8a",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="arm64v8a" ),0 )

	' endian
	env.InsertDecl New TConstDecl.Create( "bigendian",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="ppc" ),0 )
	env.InsertDecl New TConstDecl.Create( "littleendian",New TIntType,New TConstExpr.Create( New TIntType,opt_arch<>"ppc" ),0 )

	' new compiler
	env.InsertDecl New TConstDecl.Create( "bmxng",New TIntType,New TConstExpr.Create( New TIntType, True ),0 )

'	env.InsertDecl New TConstDecl.Create( "LANG",TType.stringType,New TConstExpr.Create( TType.stringType,ENV_LANG ),0 )
'	env.InsertDecl New TConstDecl.Create( "TARGET",TType.stringType,New TConstExpr.Create( TType.stringType,ENV_TARGET ),0 )
'	env.InsertDecl New TConstDecl.Create( "CONFIG",TType.stringType,New TConstExpr.Create( TType.stringType,ENV_CONFIG ),0 )

	PushEnv env

	Local toker:TToker=New TToker.Create( "",source )

	Local parser:TParser=New TParser.Create( toker,Null )

	Local expr:TExpr=parser.ParseExpr()

	expr=expr.Semant()

	If ty expr=expr.Cast( ty )

	Local val$=expr.Eval()

	PopEnv

	Return val
End Function

Type TCastDets

	Field name:String
	Field retType:String
	Field noGen:Int
	Field args:String[0]

End Type

