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
SuperStrict

Import BRL.MaxUtil
Import "toker.bmx"

Include "iparser.bmx"


Global FILE_EXT$="bmx"

Type TForEachinStmt Extends TLoopStmt
	Field varid$
	Field varty:TType
	Field varlocal:Int
	Field expr:TExpr
	Field varExpr:TExpr
	
	Method Create:TForEachinStmt( varid$,varty:TType,varlocal:Int,expr:TExpr,block:TBlockDecl,loopLabel:TLoopLabelDecl,varExpr:TExpr )
		Self.varid=varid
		Self.varty=varty
		Self.varlocal=varlocal
		Self.expr=expr
		Self.block=block
		block.extra = Self
		Self.loopLabel=loopLabel
		Self.varExpr = varExpr
		Return Self
	End Method

	Method OnCopy:TStmt( scope:TScopeDecl )
		If loopLabel Then
			If varExpr Then
				Return New TForEachinStmt.Create( varid,varty,varlocal,expr.Copy(),block.CopyBlock( scope ),TLoopLabelDecl(loopLabel.Copy()), varExpr.Copy() )
			Else
				Return New TForEachinStmt.Create( varid,varty,varlocal,expr.Copy(),block.CopyBlock( scope ),TLoopLabelDecl(loopLabel.Copy()), Null )
			End If
		Else
			If varExpr Then
				Return New TForEachinStmt.Create( varid,varty,varlocal,expr.Copy(),block.CopyBlock( scope ),Null, varExpr.Copy() )
			Else
				Return New TForEachinStmt.Create( varid,varty,varlocal,expr.Copy(),block.CopyBlock( scope ),Null, Null )
			End If
		End If
	End Method

	Method OnSemant()
		Const NotIterableError:String = "EachIn requires a type that implements IIterable or has a suitable ObjectEnumerator method."
		
		expr=expr.Semant()

		If TArrayType( expr.exprType ) Or TStringType( expr.exprType )

			Local exprTmp:TLocalDecl
			Local exprVar:TExpr
			If TArrayType( expr.exprType ).isStatic And (TVarExpr(expr) Or TMemberVarExpr(expr)) Then ' TODO TSliceExpr
				exprVar = expr
			Else
				exprTmp = New TLocalDecl.Create( "",Null,expr,,True )
				exprVar = New TVarExpr.Create( exprTmp )
			End If
			
			Local indexTmp:TLocalDecl=New TLocalDecl.Create( "",Null,New TConstExpr.Create( New TUIntType,"0" ),,True )

			Local lenExpr:TExpr=New TIdentExpr.Create( "Length",exprVar )

			Local cmpExpr:TExpr=New TBinaryCompareExpr.Create( "<",New TVarExpr.Create( indexTmp ),lenExpr )

			Local indexExpr:TExpr=New TIndexExpr.Create( exprVar,[New TVarExpr.Create( indexTmp )] )
			Local addExpr:TExpr=New TBinaryMathExpr.Create( "+",New TVarExpr.Create( indexTmp ),New TConstExpr.Create( New TIntType,"1" ) )

			Local cont:TContinueStmt
			
			If varlocal

				' array of object ?

				If TArrayType( expr.exprType ) And TObjectType(TArrayType( expr.exprType ).elemType) And (Not TObjectType(TArrayType( expr.exprType ).elemType).classdecl.IsExtern() ..
							Or (TObjectType(TArrayType( expr.exprType ).elemType).classdecl.IsExtern() ..
							And IsPointerType(TArrayType( expr.exprType ).elemType))) Then

					Local isStruct:Int = TObjectType(TArrayType( expr.exprType ).elemType).classdecl.IsStruct()

					Local cExpr:TExpr
					Local varObjTmp:TLocalDecl
					Local varObjStmt:TStmt
					
					If exprTmp Then
						exprTmp.Semant()
					End If
					indexTmp.Semant()
					
					If TIdentType(varty) And TIdentType(varty).ident = "Object" Then
						cExpr = indexExpr
					Else
						If TStringType(varty) Then
							varObjTmp = New TLocalDecl.Create( "",TType.objectType,indexExpr,,True)
							varObjTmp.Semant()
							Local varObjExpr:TExpr=New TVarExpr.Create( varObjTmp )
							
							Local expr:TExpr = New TFuncCallExpr.Create( New TIdentExpr.Create( "ObjectIsString"), [varObjExpr])
							expr=New TBinaryCompareExpr.Create( "=",expr, New TConstExpr.Create( New TIntType,"0" ))
							
							Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope, True, BLOCK_IF )
							Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope, True, BLOCK_ELSE )
							cont = New TContinueStmt.Create(Null, True)
							thenBlock.AddStmt cont
		
							varObjStmt = New TIfStmt.Create( expr,thenBlock,elseBlock, True )
							'block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock, True )
							
							cExpr = New TCastExpr.Create( varty, varObjExpr,CAST_EXPLICIT )
						Else
							cExpr = New TCastExpr.Create( varty, indexExpr,CAST_EXPLICIT )
						End If 

						'cExpr = New TCastExpr.Create( varty, indexExpr,CAST_EXPLICIT )
					End If

					' local variable
					Local varTmp:TLocalDecl=New TLocalDecl.Create( varid,varty,cExpr )

					' local var as expression
					Local expr:TExpr=New TVarExpr.Create( varTmp )

					If Not isStruct And Not varObjTmp Then
						' var = Null
						expr=New TBinaryCompareExpr.Create( "=",expr, New TNullExpr.Create(TType.nullObjectType))
	
						' then continue
						Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope, , BLOCK_IF )
						Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope, , BLOCK_ELSE )
						cont = New TContinueStmt
						thenBlock.AddStmt cont
	
						block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock )
					End If
					block.stmts.AddFirst New TAssignStmt.Create( "=",New TVarExpr.Create( indexTmp ),addExpr )
					block.stmts.AddFirst New TDeclStmt.Create( varTmp )
					If varObjTmp Then
						block.stmts.AddFirst varObjStmt
						block.stmts.AddFirst New TDeclStmt.Create( varObjTmp, True )
					End If


				Else
					Local varTmp:TLocalDecl=New TLocalDecl.Create( varid,varty,indexExpr )
					block.stmts.AddFirst New TAssignStmt.Create( "=",New TVarExpr.Create( indexTmp ),addExpr, True )
					block.stmts.AddFirst New TDeclStmt.Create( varTmp, True )
				End If
			Else
				
				If TArrayType( expr.exprType ) And TObjectType(TArrayType( expr.exprType ).elemType) Then
				' var = Null
					If Not varty Then
						varExpr = varExpr.Semant()
						varty = varExpr.exprType
						'Local decl:TValDecl = block.scope.FindValDecl(varid.ToLower())
						
						'If decl Then
						'	decl.Semant()
						'	
						'	varty = decl.ty.Copy()
						'End If
					End If

					Local isStruct:Int = TObjectType(TArrayType( expr.exprType ).elemType).classdecl.IsStruct()

'					expr=New TBinaryCompareExpr.Create( "=",New TIdentExpr.Create( varid ), New TNullExpr.Create(TType.nullObjectType))

					If Not isStruct Then
						expr=New TBinaryCompareExpr.Create( "=",varExpr, New TNullExpr.Create(TType.nullObjectType))
		
						' then continue
						Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope, , BLOCK_IF )
						Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope, , BLOCK_ELSE )
						cont = New TContinueStmt
						thenBlock.AddStmt cont
		
						block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock )
					End If
					'block.stmts.AddFirst New TDeclStmt.Create( varTmp )

					block.stmts.AddFirst New TAssignStmt.Create( "=",New TVarExpr.Create( indexTmp ),addExpr, True )
'					block.stmts.AddFirst New TAssignStmt.Create( "=",New TIdentExpr.Create( varid ),New TCastExpr.Create( varty, indexExpr,CAST_EXPLICIT ), True )
					block.stmts.AddFirst New TAssignStmt.Create( "=",varExpr,New TCastExpr.Create( varty, indexExpr,CAST_EXPLICIT ), True )
				Else
					block.stmts.AddFirst New TAssignStmt.Create( "=",New TVarExpr.Create( indexTmp ),addExpr, True )
'					block.stmts.AddFirst New TAssignStmt.Create( "=",New TIdentExpr.Create( varid ),indexExpr, True )
					block.stmts.AddFirst New TAssignStmt.Create( "=",varExpr,indexExpr, True )
				End If

			EndIf

			Local whileStmt:TWhileStmt=New TWhileStmt.Create( cmpExpr,block,loopLabel, True )

			block=New TBlockDecl.Create( block.scope, True, BLOCK_LOOP )
			If exprTmp Then
				block.AddStmt New TDeclStmt.Create( exprTmp, True )
			End If
			block.AddStmt New TDeclStmt.Create( indexTmp, True )
			block.AddStmt whileStmt
			
			If cont Then
				cont.loop = whileStmt
			End If

		Else If TObjectType( expr.exprType )
			Local tmpDecl:TDeclStmt
			Local iterable:Int

			' ensure semanted
			TObjectType(expr.exprType).classDecl.Semant()
			
			If TObjectType(expr.exprType).classDecl.ImplementsInterface("iiterable") Or (TObjectType(expr.exprType).classDecl.ident="IIterable" And TObjectType(expr.exprType).classDecl.IsInterface()) Then
				iterable = True
			Else
				Local declList:TFuncDeclList = TFuncDeclList(TObjectType(expr.exprType).classDecl.GetDecl("objectenumerator"))
				If Not declList Then
					Err NotIterableError
				End If
			End If

			If TInvokeExpr(expr) Or TInvokeMemberExpr(expr) Then
				Local tmpVar:TLocalDecl=New TLocalDecl.Create( "",expr.exprType,expr,,True )
				tmpVar.Semant()
				tmpDecl = New TDeclStmt.Create( tmpVar, True )
				expr = New TVarExpr.Create( tmpVar )
			End If

			Local enumerInit:TExpr
			If iterable Then
				enumerInit = New TFuncCallExpr.Create( New TIdentExpr.Create( "GetIterator",expr ) )
			Else
				enumerInit = New TFuncCallExpr.Create( New TIdentExpr.Create( "ObjectEnumerator",expr ) )
			End If
			Local enumerTmp:TLocalDecl=New TLocalDecl.Create( "",Null,enumerInit,,True )
			enumerTmp.Semant()

			Local hasNextExpr:TExpr
			If iterable Then
				hasNextExpr = New TFuncCallExpr.Create( New TIdentExpr.Create( "MoveNext",New TVarExpr.Create( enumerTmp ) ) )
			Else
				hasNextExpr = New TFuncCallExpr.Create( New TIdentExpr.Create( "HasNext",New TVarExpr.Create( enumerTmp ) ) )
			End If
			
			Local nextObjExpr:TExpr
			If iterable Then
				nextObjExpr = New TFuncCallExpr.Create( New TIdentExpr.Create( "Current",New TVarExpr.Create( enumerTmp ) ) )
			Else
				nextObjExpr = New TFuncCallExpr.Create( New TIdentExpr.Create( "NextObject",New TVarExpr.Create( enumerTmp ) ) )
			End If

			Local cont:TContinueStmt
			
			If varlocal
'				Local varTmp:TLocalDecl=New TLocalDecl.Create( varid,varty,nextObjExpr )
'				block.stmts.AddFirst New TDeclStmt.Create( varTmp )

				Local cExpr:TExpr
				
				Local varObjTmp:TLocalDecl
				Local varObjStmt:TStmt
				
				If iterable Or (TIdentType(varty) And TIdentType(varty).ident = "Object") Then
					cExpr = nextObjExpr
				Else
					If TStringType(varty) Then
						varObjTmp = New TLocalDecl.Create( "",TType.objectType,nextObjExpr,,True)
						varObjTmp.Semant()
						Local varObjExpr:TExpr=New TVarExpr.Create( varObjTmp )
						
						Local expr:TExpr = New TFuncCallExpr.Create( New TIdentExpr.Create( "ObjectIsString"), [varObjExpr])
						expr=New TBinaryCompareExpr.Create( "=",expr, New TConstExpr.Create( New TIntType,"0" ))
						
						Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope, True, BLOCK_IF )
						Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope, True, BLOCK_ELSE )
						cont = New TContinueStmt.Create(Null, True)
						thenBlock.AddStmt cont
	
						varObjStmt = New TIfStmt.Create( expr,thenBlock,elseBlock, True )
						'block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock, True )
						
						cExpr = New TCastExpr.Create( varty, varObjExpr,CAST_EXPLICIT )
					Else
						cExpr = New TCastExpr.Create( varty, nextObjExpr,CAST_EXPLICIT )
					End If 
				End If

				' local variable
				Local varTmp:TLocalDecl=New TLocalDecl.Create( varid,varty,cExpr)

				If Not TNumericType(varty) And Not varObjTmp Then
					' local var as expression
					Local expr:TExpr=New TVarExpr.Create( varTmp )
	
					' var = Null
					expr=New TBinaryCompareExpr.Create( "=",expr, New TNullExpr.Create(TType.nullObjectType))
	
					' then continue
					Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope, True, BLOCK_IF )
					Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope, True, BLOCK_ELSE )
					cont = New TContinueStmt.Create(Null, True)
					thenBlock.AddStmt cont
	
					block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock, True )
				End If
				block.stmts.AddFirst New TDeclStmt.Create( varTmp, True )
				If varObjTmp Then
					block.stmts.AddFirst varObjStmt
					block.stmts.AddFirst New TDeclStmt.Create( varObjTmp, True )
				End If
			Else

				If Not varty Then
					varExpr = varExpr.Semant()
					varty = varExpr.exprType
				End If
				
				Local varObjTmp:TLocalDecl
				Local varObjStmt:TStmt
				Local cExpr:TExpr
				
				If TStringType(varty) Then
					varObjTmp = New TLocalDecl.Create( "",TType.objectType,nextObjExpr,,True)
					varObjTmp.Semant()
					Local varObjExpr:TExpr=New TVarExpr.Create( varObjTmp )
					
					Local expr:TExpr = New TFuncCallExpr.Create( New TIdentExpr.Create( "ObjectIsString"), [varObjExpr])
					expr=New TBinaryCompareExpr.Create( "=",expr, New TConstExpr.Create( New TIntType,"0" ))
					
					Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope, True, BLOCK_IF )
					Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope, True, BLOCK_ELSE )
					cont = New TContinueStmt.Create(Null, True)
					thenBlock.AddStmt cont

					varObjStmt = New TIfStmt.Create( expr,thenBlock,elseBlock, True )
					'block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock, True )
					
					cExpr = New TCastExpr.Create( varty, varObjExpr,CAST_EXPLICIT )
				Else
					cExpr = New TCastExpr.Create( varty, nextObjExpr,CAST_EXPLICIT )
				End If 
'				If Not varty Then
'					Local decl:TValDecl = block.scope.FindValDecl(varid.ToLower())
'					
'					If decl Then
'						decl.Semant()
'						
'						varty = decl.ty.Copy()
'					End If
'				End If
				
				' var = Null
'				Local expr:TExpr=New TBinaryCompareExpr.Create( "=",New TIdentExpr.Create( varid ), New TNullExpr.Create(TType.nullObjectType))
				If Not TNumericType(varty) And Not varObjTmp Then

					Local expr:TExpr=New TBinaryCompareExpr.Create( "=",varExpr, New TNullExpr.Create(TType.nullObjectType))
	
					' then continue
					Local thenBlock:TBlockDecl=New TBlockDecl.Create( block.scope, ,BLOCK_IF )
					Local elseBlock:TBlockDecl=New TBlockDecl.Create( block.scope, ,BLOCK_ELSE )
					cont = New TContinueStmt
					thenBlock.AddStmt cont
	
					block.stmts.AddFirst New TIfStmt.Create( expr,thenBlock,elseBlock )
					'block.stmts.AddFirst New TDeclStmt.Create( varTmp )

				End If
'				block.stmts.AddFirst New TAssignStmt.Create( "=",New TIdentExpr.Create( varid ),New TCastExpr.Create( varty, nextObjExpr,CAST_EXPLICIT ))
				block.stmts.AddFirst New TAssignStmt.Create( "=",varExpr,cExpr)
				If varObjTmp Then
					block.stmts.AddFirst varObjStmt
					block.stmts.AddFirst New TDeclStmt.Create( varObjTmp, True )
				End If
			EndIf

			Local whileStmt:TWhileStmt=New TWhileStmt.Create( hasNextExpr,block, loopLabel, True )

			block=New TBlockDecl.Create( block.scope, True, BLOCK_LOOP )
			If tmpDecl Then
				block.AddStmt tmpDecl
			End If
			block.AddStmt New TDeclStmt.Create( enumerTmp, True )
			block.AddStmt whileStmt
			
			If cont Then
				cont.loop = whileStmt
			End If

		Else
			Err NotIterableError
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
				Return Null
			End If
		Else
			path = RealPath(file)
		End If

		id = count
		Return Self
	End Method
	
	Method GeneratedDataName:String(app:TAppDecl)
		Return "_ib" + app.munged + "_" + id + "_data"
	End Method

	Method GeneratedSizeName:String(app:TAppDecl)
		Return "_ib" + app.munged + "_" + id + "_size"
	End Method

End Type

'***** Parser *****
Type TParser Extends TGenProcessor

	Field _toker:TToker
	Field _toke:String
	Field _tokeType:Int

	Field _block:TBlockDecl
	Field _blockStack:TList=New TList'<TBlockDecl>
	Field _errStack:TStringList=New TStringList

	Field _app:TAppDecl
	Field _module:TModuleDecl

	Field _externCasts:TMap = New TMap

	
	Field unknownIdentsEvalFalse:Int
	Method SetErr(toker:TToker = Null)
		Local t:TToker = _toker
		If toker Then
			t = toker
		End If
		If t.Path()
			_errInfo=FormatError(t.Path(),t.Line(),0)
		EndIf
	End Method

	Method DoErr(error:String, toker:TToker = Null)
		SetErr(toker)
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

	Method ActualPath:String(path:String)
		Local dir:String = ExtractDir(path)
		Local origFile:String = StripDir(path)
		Local lowerFile:String = origFile.ToLower()
		
		Local actualDir:String = ExtractDir(RealPath(path))

		Local files:String[] = LoadDir(actualDir)
		For Local file:String = EachIn files

			If file.ToLower() = lowerFile Then
				If file <> origFile Then
					' we could raise as a warning instead, but an error encourages the user to fix their code ;-)
					Err "Actual file '" + file + "' differs in case with import '" + origFile + "'"
					
					' what we might do were we to warn instead...
					If dir Then
						Return dir + "/" + file
					Else
						Return file
					End If
				End If
				Exit
			End If
		Next
		Return path
	End Method
	

	Method NextToke$()
		Local toke$=_toke

		Repeat
			_toke=_toker.NextToke()
			_tokeType=_toker.TokeType()
		Until _tokeType<>TOKE_SPACE

		If _tokeType=TOKE_KEYWORD _toke=_toker._tokeLower

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
		Local uni:String
		If toke.length > 0 And toke[0] > 255 Then
			uni = " (unicode : " + _toker._lastTCHR + ")"
		End If
		Return "'" + toke + "'" + uni
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
			DoErr "Syntax error - expecting '"+toke+"' but found " + DescribeToke(_toke)
		EndIf
	End Method

	Method ParseToker( toker:TToker, toke$ )
		If Not CParseToker( toker, toke )
			DoErr "Syntax error - expecting '"+toke+"'.", toker
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
		Case "string","object", "self"
		Default
			If _tokeType<>TOKE_IDENT Then
				Local kw:String
				If _tokeType = TOKE_KEYWORD Then
					kw = " keyword"
				End If
				Err "Syntax error - expecting identifier, but found" + kw + " '" + EscapeLines(_toke) + "'"
			End If
		End Select
		Local id$=_toke
		NextToke
		Return id
	End Method

	Method ParseIdentType:TIdentType()
		Local id$=ParseIdent()

		If CParse( "." ) id:+"."+ParseIdent()
		If CParse( "." ) id:+"."+ParseIdent()

		Local args:TType[]
		If CParse( "<" )
			Local nargs:Int
			Repeat
				Local arg:TType = ParseType()
				
				Repeat
					If (_toke = "[" Or _toke = "[]") And IsArrayDef()
						arg = ParseArrayType(arg)
					Else If _toke = "(" Then
						Local argDecls:TArgDecl[] = ParseFuncParamDecl()
						arg = New TFunctionPtrType.Create(New TFuncDecl.CreateF("", arg, argDecls, FUNC_PTR))
					Else
						Exit
					End If
				Forever
				
				If args.Length=nargs args=args+ New TType[10]
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
		While CParse( "." )
			If _tokeType<>TOKE_IDENT Return Null
			id:+"."+ParseIdent()
		Wend
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
		If CParse( "short" )
			ty = New TShortType
		Else If CParse( "byte" )
			ty = New TByteType
		Else If CParse( "int" )
			ty = New TIntType
		Else If CParse( "uint" )
			ty = New TUIntType
		Else If CParse( "float" )
			ty = New TFloatType
		Else If CParse( "long" )
			ty = New TLongType
		Else If CParse( "ulong" )
			ty = New TULongType
		Else If CParse( "double" )
			ty = New TDoubleType
		Else If CParse( "size_t" )
			ty = New TSizeTType
		Else If CParse( "int128" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			ty = New TInt128Type
		Else If CParse( "float128" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			ty = New TFloat128Type
		Else If CParse( "double128" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			ty = New TDouble128Type
		Else If CParse( "float64" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			ty = New TFloat64Type
		Else If CParse( "wparam" ) Then
			If opt_platform <> "win32" Err "WParam types only available on Win32"
			ty = New TWParamType
		Else If CParse( "lparam" ) Then
			If opt_platform <> "win32" Err "LParam types only available on Win32"
			ty = New TLParamType
		End If

		While CParse("ptr")
			ty = TType.MapToPointerType(ty)
		Wend
		Return ty
	End	Method

	Method CParsePrimitiveNumberType:TType()
		If CParse( "short" ) Return New TShortType
		If CParse( "byte" ) Return New TByteType
		If CParse( "int" ) Return New TIntType
		If CParse( "uint" ) Return New TUIntType
		If CParse( "float" ) Return New TFloatType
		If CParse( "long" ) Return New TLongType
		If CParse( "ulong" ) Return New TULongType
		If CParse( "double" ) Return New TDoubleType
		If CParse( "size_t" ) Return New TSizeTType
		If CParse( "int128" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			Return New TInt128Type
		End If
		If CParse( "float128" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			Return New TFloat128Type
		End If
		If CParse( "double128" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			Return New TDouble128Type
		End If
		If CParse( "float64" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			Return New TFloat64Type
		End If
		If CParse( "wparam" ) Then
			If opt_platform <> "win32" Err "WParam types only available on Win32"
			Return New TWParamType
		End If
		If CParse( "lparam" ) Then
			If opt_platform <> "win32" Err "LParam types only available on Win32"
			Return New TLParamType
		End If
	End	Method

	Method ParseNewType:TType()
		If CParse( "void" ) Return New TVoidType
		If CParse( "short" ) Return New TShortType
		If CParse( "byte" ) Return New TByteType
		If CParse( "int" ) Return New TIntType
		If CParse( "uint" ) Return New TUIntType
		If CParse( "float" ) Return New TFloatType
		If CParse( "string" ) Return TType.stringType
		If CParse( "object" ) Return New TIdentType.Create( "brl.classes.object" )
		If CParse( "long" ) Return New TLongType
		If CParse( "ulong" ) Return New TULongType
		If CParse( "double" ) Return New TDoubleType
		If CParse( "size_t" ) Return New TSizeTType
		If CParse( "int128" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			Return New TInt128Type
		End If
		If CParse( "float128" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			Return New TFloat128Type
		End If
		If CParse( "double128" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			Return New TDouble128Type
		End If
		If CParse( "float64" ) Then
			If opt_arch <> "x64" Err "Intrinsic types only available on x64"
			Return New TFloat64Type
		End If
		If CParse( "wparam" ) Then
			If opt_platform <> "win32" Err "WParam types only available on Win32"
			Return New TWParamType
		End If
		If CParse( "lparam" ) Then
			If opt_platform <> "win32" Err "LParam types only available on Win32"
			Return New TLParamType
		End If
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
				If CParse("string") Then
					ty=New TStringType
				Else
					ty = ParseIdentType()
				End If
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

	Method ParseDeclType:TType(attr:Long = 0)
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
			
				' FIXME #200
				'If TStringType(ty) = Null And (TObjectType(ty) = Null Or (TObjectType(ty) <> Null And TObjectType(ty).classDecl.IsExtern())) And TArrayType(ty) = Null Then
					ty = TType.MapToPointerType(ty)
	
					While CParse("ptr")
						ty = TType.MapToPointerType(ty)
					Wend
				'Else
				'	ty = Null
				'End If

				If Not ty DoErr "Invalid Pointer type."
			End If

		Case "("
			' for Strict code, void will be converted to Int during semanting.
			ty=New TVoidType
		Default
			If _module.IsSuperStrict() Err "Missing type specifier."
			ty=New TIntType

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
		End Select
		
		' array or function pointer?
		Repeat
			If (_toke = "[" Or _toke = "[]") And (IsArrayDef() Or IsArrayDef(attr & DECL_STATIC > 0)) Then
				If Not IsArrayDef(attr & DECL_STATIC > 0) Then
					Err "Invalid static array initialization."
				Else
					If attr & DECL_STATIC > 0 Then
						Exit
					End If
					ty = ParseArrayType(ty)
				End If
			Else If _toke = "(" Then
				Local args:TArgDecl[] = ParseFuncParamDecl()
				attr :| ParseCallConvention(attr & DECL_API_STDCALL)
				ty = New TFunctionPtrType.Create(New TFuncDecl.CreateF("", ty, args, FUNC_PTR | (attr & DECL_API_STDCALL)))
			Else
				Exit
			End If
		Forever
		
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
	Method ParseArrayType:TType(ty:TType, isStatic:Int = False)
		If isStatic Then
			Parse("[")
			Local expr:TExpr = ParseUnaryExpr()
			ty = New TArrayType.Create( ty )
			TArrayType(ty).isStatic = True
			Parse("]")
			Return ty
		End If

		While True
			Local dims:Int = 1
			
			If CParse("[]") Then
				ty=New TArrayType.Create( ty )
				
				' test for array of arrays
				If IsArrayTypeNext(_toker) Continue
				
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
			
			' test for array of arrays
			If IsArrayTypeNext(_toker) Continue
			
			Exit
		Wend
		Return ty
	End Method
	
	Method IsArrayTypeNext:Int(tok:TToker)
		Local toker:TToker=New TToker.Copy(tok)
		If CParseToker(toker, "[]") Return True
		If CParseToker(toker, "[") Then
			' look ahead to see if this is an array decl, or something else..
			If CParseToker(toker, "]") Or CParseToker(toker, ",") Then
				Return True
			End If
		End If
		Return False
	End Method
	
	Method IsArrayDef:Int(isStatic:Int = False)
		Local isDef:Int = True
		Local toker:TToker=New TToker.Copy(_toker)
		If isStatic Then
			If Not CParseToker(toker, "[") Then
				Return False
			End If
			NextTokeToker(toker)
			If Not CParseToker(toker, "]") Then
				Return False
			End If
			Return True
		End If
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
			NextToke
			
			If _toke = "(" Then
				' call constructor
				expr=New TNewExpr.Create( ParseArgs(stmt) )
			Else
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
							dims :+ 1
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
			End If
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
		Case "int","long","float","double","object","short","byte","size_t","uint","ulong","int128","float64","float128","double128","lparam","wparam","string"
			Local id$=_toke
			Local ty:TType=ParseType()

			If TIntType(ty) And id.ToLower() <> "int" Then
				Select id.ToLower()
					Case "byte"
						ty = New TByteType
					Case "short"
						ty = New TShortType
					Case "uint"
						ty = New TUIntType
					Case "long"
						ty = New TLongType
					Case "ulong"
						ty = New TULongType
					Case "float"
						ty = New TFloatType
					Case "double"
						ty = New TDoubleType
					Case "size_t"
						ty = New TSizeTType
					Case "int128"
						If opt_arch <> "x64" Err "Intrinsic types only available on x64"
						ty = New TInt128Type
					Case "float128"
						If opt_arch <> "x64" Err "Intrinsic types only available on x64"
						ty = New TFloat128Type
					Case "double128"
						If opt_arch <> "x64" Err "Intrinsic types only available on x64"
						ty = New TDouble128Type
					Case "float64"
						If opt_arch <> "x64" Err "Intrinsic types only available on x64"
						ty = New TFloat64Type
					Case "wparam"
						If opt_platform <> "win32" Err "WParam types only available on Win32"
						ty = New TWParamType
					Case "lparam"
						If opt_platform <> "win32" Err "LParam types only available on Win32"
						ty = New TLParamType
				End Select
			End If

			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend

			' array
			ty = ParseArrayType(ty)

			' optional brackets
			If CParse( "(" )
				expr=ParseExpr()
				Parse ")"
				expr=New TCastExpr.Create( ty,expr,CAST_EXPLICIT )
			Else
				Local tok:TToker=New TToker.Copy( _toker )

				If id="string" And CParseToker(tok, ".") Then
					expr=New TIdentExpr.Create( id )
				Else
					expr=ParseExpr()
					
					If TBinaryExpr(expr) Then
						' cast lhs and apply to rhs
						Local cexpr:TCastExpr=New TCastExpr.Create( ty,TBinaryExpr(expr).lhs,CAST_EXPLICIT )
						TBinaryExpr(expr).lhs = cexpr 
					Else
						expr=New TCastExpr.Create( ty,expr,CAST_EXPLICIT )
					End If
				End If
			EndIf
		Case "sizeof"
			NextToke

			Local ty:TType = ParseConstNumberType()
			If ty Then
				If Not TIntType(ty) Then
					Err "Return type for 'SizeOf' must be Int"
				End If
			End If

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
			
			Local ty:TType = ParseConstNumberType()
			If ty Then
				If Not TIntType(ty) Then
					Err "Return type for 'Len' must be Int"
				End If
			End If

			' optional brackets
			If CParse( "(" )
				expr=ParseExpr()
				Parse ")"
				expr=New TLenExpr.Create( expr )
			Else
				expr=ParseExpr()
				expr=New TLenExpr.Create( expr )
			EndIf
		Case "asc"
			NextToke

			Local ty:TType = ParseConstNumberType()
			If ty Then
				If Not TIntType(ty) Then
					Err "Return type for 'Asc' must be Int"
				End If
			End If
			
			' optional brackets
			If CParse( "(" )
				expr=ParseExpr()
				Parse ")"
				expr=New TAscExpr.Create( expr )
			Else
				expr=ParseExpr()
				expr=New TAscExpr.Create( expr )
			EndIf
		Case "chr"
			NextToke

			Local ty:TType = ParseConstNumberType()
			If ty Then
				If Not TStringType(ty) Then
					Err "Return type for 'Chr' must be String"
				End If
			End If
			
			' optional brackets
			If CParse( "(" )
				expr=ParseExpr()
				Parse ")"
				expr=New TChrExpr.Create( expr )
			Else
				expr=ParseExpr()
				expr=New TChrExpr.Create( expr )
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
			
			' eat any type stuff
			ParseConstNumberType()

			expr=New TInvokeSuperExpr.Create( id,ParseArgs( stmt ) )
		Case "stackalloc"
			NextToke
			
			' optional brackets
			If CParse( "(" )
				expr=ParseExpr()
				Parse ")"
				expr=New TStackAllocExpr.Create( expr )
			Else
				expr=ParseExpr()
				expr=New TStackAllocExpr.Create( expr )
			EndIf
		Case "fieldoffset"
			NextToke
			
			Local withBrackets:Int
			
			If CParse("(")
				withBrackets = True
			End If
			
			Local typeExpr:TExpr = ParseExpr()
			Parse ","
			Local fieldExpr:TExpr = ParseExpr()
			
			If withBrackets Then
				Parse(")")
			End If
			
			expr=New TFieldOffsetExpr.Create( typeExpr, fieldExpr )
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
					expr=New TIdentExpr.Create( ParseIdent(),,,unknownIdentsEvalFalse )
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
					TConstExpr(expr).UpdateType(ty)
				End If
			Case TOKE_LONGLIT
				expr=New TConstExpr.Create( New TLongType,_toke )
				NextToke
				
				Local ty:TType = ParseConstNumberType()
				If ty Then
					TConstExpr(expr).UpdateType(ty)
				End If
			Case TOKE_FLOATLIT
				expr=New TConstExpr.Create( New TFloatType,_toke )
				NextToke

				Local ty:TType = ParseConstNumberType()
				If ty Then
					TConstExpr(expr).ty = ty
				End If
			Case TOKE_STRINGLIT
				Local s:String = BmxUnquote( _toke )
				expr=New TConstExpr.Create( TType.stringType,s )
				_app.mapStringConsts(s)
				NextToke
			Case TOKE_STRINGMULTI
				Local s:String = BmxProcessMultiString( _toke )
				expr=New TConstExpr.Create( TType.stringType,s )
				_app.mapStringConsts(s)
				NextToke
			Default
				Err "Expecting expression but encountered "+DescribeToke(_toke)
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

		Local op$=_toke
		Select op
		Case "+","-","~~","not"
			NextToke
			Local expr:TExpr=ParseUnaryExpr()
			Return New TUnaryExpr.Create( op,expr )
		End Select
		Return ParsePrimaryExpr( False )
	End Method

	Method ParsePowExpr:TExpr()
		Local expr:TExpr=ParseUnaryExpr()
		Repeat
			Local op$=_toke
			Select op
			Case "^"
				NextToke
				Local rhs:TExpr=ParseUnaryExpr()
				expr=New TBinaryMathExpr.Create( op,expr,rhs )
			Default
				Return expr
			End Select
		Forever
	End Method

	Method ParseMulDivExpr:TExpr()
		Local expr:TExpr=ParsePowExpr()
		Repeat
			Local op$=_toke
			Select op
			Case "*","/","mod","shl","shr", "sar"
				NextToke
				Local rhs:TExpr=ParsePowExpr()
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
		Local thenBlock:TBlockDecl=New TBlockDecl.Create( _block, ,BLOCK_IF )
		Local elseBlock:TBlockDecl=New TBlockDecl.Create( _block, ,BLOCK_ELSE )

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
						SetErr
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
		Local block:TBlockDecl=New TBlockDecl.Create( _block, , BLOCK_LOOP )

		PushBlock block
		While Not CParse( "wend" ) And Not CParse( "endwhile" )
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

		Local block:TBlockDecl=New TBlockDecl.Create( _block, , BLOCK_LOOP )

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

		Parse "for"

		Local varid$,varty:TType,varlocal:Int
		Local varExpr:TExpr

		If CParse( "local" )
			varlocal=True
			varid=ParseIdent()

			varty=ParseDeclType()
			If varty._flags & (TType.T_CHAR_PTR | TType.T_SHORT_PTR) Then
				DoErr "Illegal variable type"
			End If
			
			Parse( "=" )
			
			' use an ident expr to pass the variable to different parts of the statement.
			' the original implementation passed decl references, which cause problems if we wanted to
			' copy the statement later.
			varExpr = New TIdentExpr.Create(varid)
		Else
			varlocal=False

			varExpr=ParsePrimaryExpr( False )

			Parse "="
		EndIf

		If CParse( "eachin" )
			Local expr:TExpr=ParseExpr()
			Local block:TBlockDecl=New TBlockDecl.Create( _block, , BLOCK_LOOP )

			PushBlock block
			While Not CParse( "next" )
				ParseStmt
			Wend
			PopBlock

			Local stmt:TForEachinStmt=New TForEachinStmt.Create( varid,varty,varlocal,expr,block,loopLabel, varExpr )

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
			Local indexVar:TLocalDecl=New TLocalDecl.Create( varid,varty,New TCastExpr.Create( varty,from,CAST_EXPLICIT ),0 )
			init=New TDeclStmt.Create( indexVar )
'			expr=New TBinaryCompareExpr.Create( op,New TVarExpr.Create( indexVar ),New TCastExpr.Create( varty,term,1 ) )
'			incr=New TAssignStmt.Create( "=",New TVarExpr.Create( indexVar ),New TBinaryMathExpr.Create( "+",New TVarExpr.Create( indexVar ),New TCastExpr.Create( varty,stp,1 ) ) )
			expr=New TBinaryCompareExpr.Create( op, varExpr,New TCastExpr.Create( varty,term,CAST_EXPLICIT ) )
			incr=New TAssignStmt.Create( "=",varExpr,New TBinaryMathExpr.Create( "+",varExpr,New TCastExpr.Create( varty,stp,CAST_EXPLICIT ) ) )
		Else
			' varty is NULL here for the casts. We will back-populate it later.
'			init=New TAssignStmt.Create( "=",New TIdentExpr.Create( varid ),from )
'			expr=New TBinaryCompareExpr.Create( op,New TIdentExpr.Create( varid ),New TCastExpr.Create( varty,term,1 ) )
'			incr=New TAssignStmt.Create( "=",New TIdentExpr.Create( varid ),New TBinaryMathExpr.Create( "+",New TIdentExpr.Create( varid ),New TCastExpr.Create( varty,stp,1 ) ) )
			init=New TAssignStmt.Create( "=",varExpr,from )
			expr=New TBinaryCompareExpr.Create( op,varExpr,New TCastExpr.Create( varty,term,CAST_EXPLICIT ) )
			incr=New TAssignStmt.Create( "=",varExpr,New TBinaryMathExpr.Create( "+",varExpr,New TCastExpr.Create( varty,stp,CAST_EXPLICIT ) ) )
		EndIf

		Local block:TBlockDecl=New TBlockDecl.Create( _block, , BLOCK_LOOP )

		PushBlock block
		While Not CParse( "next" )
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

		Local tryStmtDecl:TTryStmtDecl = TTryStmtDecl(New TTryStmtDecl.Create( _block ))
		
		PushBlock tryStmtDecl

		Local block:TBlockDecl=New TBlockDecl.Create( tryStmtDecl, , BLOCK_TRY )
		Local catches:TList=New TList
		Local finallyStmt:TFinallyStmt = Null

		PushBlock block
		While _toke<>"end" And _toke<>"endtry"
			If CParse( "catch" )
				If finallyStmt Then Err "'Catch' can not appear after 'Finally'."
				Local id:String=ParseIdent()
				Local ty:TType
				If Not CParse(":") Then
					Parse "$"
					ty= TType.stringType
				Else
					ty=ParseType()
					While IsArrayDef()
						ty=ParseArrayType(ty)
					Wend
				End If
				PopBlock
				Local init:TLocalDecl=New TLocalDecl.Create( id,ty,Null,0 )
				Local block:TBlockDecl=New TBlockDecl.Create( _block, , BLOCK_CATCH )
				catches.AddLast(New TCatchStmt.Create( init,block ))
				PushBlock block
			Else If CParse("finally") Then
				If finallyStmt Then Err "Try statement cannot have more than one Finally block."
				PopBlock
				Local block:TBlockDecl = New TBlockDecl.Create(_block, , BLOCK_FINALLY)
				finallyStmt = New TFinallyStmt.Create(block)
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

		If catches.Count() = 0 And Not finallyStmt Then Err "Expecting 'Catch' or 'Finally'."
		
		PopBlock ' try block
		
		If Not CParse("endtry") Then
			NextToke
			CParse "try"
		End If

		PopBlock ' tryStmtDecl
		
		Local tryStmt:TTryStmt = New TTryStmt.Create(block,TCatchStmt[](catches.ToArray()), finallyStmt)

		tryStmtDecl.tryStmt = tryStmt

		_block.AddStmt tryStmt
		
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
		
		Local tmpVar:TLocalDecl
		Local selectExpr:TExpr = ParseExpr()
		If Not TNullType(selectExpr.exprType)
			tmpVar = New TLocalDecl.Create("", Null, selectExpr, , True)
			block.AddStmt New TDeclStmt.Create(tmpVar)
		End If
		
		While _toke<>"end" And _toke<>"default" And _toke<>"endselect"
			SetErr
			Select _toke
			Case "~n"
				NextToke
			Case "case"
				NextToke
				Local comp:TExpr
				Repeat
					Local expr:TExpr
					If TNullType(selectExpr.exprType)
						expr = New TNullExpr.Create(TType.nullObjectType)
					Else
						expr = New TVarExpr.Create(tmpVar)
					End If
					expr=New TBinaryCompareExpr.Create( "=",expr,ParseExpr() )
					If comp
						comp=New TBinaryLogicExpr.Create( "or",comp,expr )
					Else
						comp=expr
					EndIf
				Until Not CParse(",")

				Local thenBlock:TBlockDecl=New TBlockDecl.Create( _block, , BLOCK_IF )
				Local elseBlock:TBlockDecl=New TBlockDecl.Create( _block, , BLOCK_ELSE )

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
					Err "Case can not appear after Default."
				Case "default"
					Err "Select statement can have only one Default block."
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

	Method ParseExternBlock(mdecl:TModuleDecl, attrs:Long)

		NextToke

		attrs :| ParseCallConvention()
		
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
				Case "const"
					mdecl.InsertDecls ParseDecls( _toke,attrs )
				Case "global"
					ParseDeclStmts(True, attrs, mdecl)
				Case "threadedglobal"
					ParseDeclStmts(True, attrs | DECL_THREADED, mdecl)
				Case "struct"
					mdecl.InsertDecl ParseClassDecl( _toke,attrs | CLASS_STRUCT )
				Case "type"
					mdecl.InsertDecl ParseClassDecl( _toke,attrs )
				Case "function"
					mdecl.InsertDecl ParseFuncDecl( _toke,attrs )
				Case "interface"
					mdecl.InsertDecl ParseClassDecl( _toke,attrs | CLASS_INTERFACE )
				Default
					If _toke <> "end" And _toke <> "endextern" Then
						Err "Expecting expression but encountered '"+_toke+"'"
					End If
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
			Case "const","local","global","threadedglobal"
				ParseDeclStmts
			' nested function - needs to get added to the "module"
			Case "function"
				_block.InsertDecl ParseFuncDecl( _toke,FUNC_NESTED)
			Case "type"
				_block.InsertDecl ParseClassDecl( _toke,DECL_NESTED)
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
				While _toke
					SetErr
					Select _toke.ToLower()
						Case "~n"
							NextToke
						Case "while"
							ParseWhileStmt(decl)
							Exit
						Case "repeat"
							ParseRepeatStmt(decl)
							Exit
						Case "for"
							ParseForStmt(decl)
							Exit
						Case "defdata"
							ParseDefDataStmt(decl)
							Exit
						Default
							Err "Labels must appear before a loop or DefData statement"
					End Select
				Wend
			Case "release"
				ParseReleaseStmt()
			Case "defdata"
				ParseDefDataStmt()
			Case "readdata"
				ParseReadDataStmt()
			Case "restoredata"
				ParseRestoreDataStmt()
			Default
				If _toke.StartsWith("'!") Then
					If _tokeType = TOKE_NATIVE Then
						ParseNativeStmt()
					End If
				Else
					Local expr:TExpr=ParsePrimaryExpr( True )
	
					Select _toke.ToLower()
					'"=","*=","/=","+=","-=","&=","|=","~~=","Mod","Shl","Shr"
					Case "=",":*",":/",":+",":-",":&",":|",":~~","mod","shl","shr", ":shl", ":shr", "sar", ":sar", ":mod"

						' remap symbols...
						'For Local i:Int = 0 Until TToker._symbols.length
						'	Local sym$= TToker._symbols[i]
						'	If _toke.ToLower() = sym
						'		_toke = TToker._symbols_map[i]
						'		Exit
						'	EndIf
						'Next
	
	
						If TIdentExpr( expr ) Or TIndexExpr( expr )
							Local op$=_toke.ToLower()
							NextToke
						'	If Not op.EndsWith( "=" ) And Not op.StartsWith("=")
						'		Parse "="
						'		op:+"="
						'	EndIf
							_block.AddStmt New TAssignStmt.Create( op,expr,ParseExpr() )
						Else
							Err "Assignment operator '"+_toke+"' cannot be used this way."
						EndIf
						Return
					End Select
	
					If TIdentExpr( expr )
	
						expr=New TFuncCallExpr.Create( expr,ParseArgs( True ) )
	
					Else If TFuncCallExpr( expr) Or TInvokeSuperExpr( expr ) Or TNewObjectExpr( expr ) Or TNewExpr(expr)
					
					Else If TIndexExpr(expr)
						expr = New TFuncCallExpr.Create( expr, ParseArgs( True ) )
					Else
						Err "Expression cannot be used as a statement."
					EndIf
	
					_block.AddStmt New TExprStmt.Create( expr )
				End If
		End Select
	End Method

	Method ParseDecl:TDecl( toke$,attrs:Long )

		SetErr

		If CParse("staticarray") Then
			If toke = "const" Then
				Err "Const cannot be used in this way"
			End If
			If attrs & DECL_STATIC Then
				Err "Already declared as a static array"
			End If
			attrs :| DECL_STATIC
		End If

		Local id$=ParseIdent()
		Local ty:TType
		Local init:TExpr
		
		
		If attrs & DECL_EXTERN
			ty=ParseDeclType(attrs & (DECL_STATIC | DECL_API_STDCALL))
			
			If toke = "const" Then
				If CParse("=") Then
					init=ParseExpr()
				End If
			End If
'		Else If CParse( ":=" )
'			init=ParseExpr()
'			ty = init.exprType
		Else
			ty=ParseDeclType(attrs & (DECL_STATIC | DECL_API_STDCALL))

			If CParse( "=" )
				If (attrs & DECL_STATIC) Then
					Err "Static arrays cannot be initialized in this way"
				End If
				init=ParseExpr()
			Else If CParse( "[" ) ' an initialised array?
				If (attrs & DECL_STATIC) Then
					init = ParseExpr()
					Parse "]"
					ty=New TArrayType.Create( ty,1,, attrs & DECL_STATIC > 0 )
				Else
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
					ty=New TArrayType.Create( ty, ln.length,, attrs & DECL_STATIC > 0 )
				End If
			Else If toke <> "const"
				If toke="global" Or toke="local" Or toke="threadedglobal" Then
					init=New TConstExpr.Create( ty,"" )
				End If
			Else
				Err "Constants must be initialized."
			EndIf
			
		EndIf
		

		Local decl:TValDecl

		Select toke
		Case "global"
			decl=New TGlobalDecl.Create( id,ty,init,attrs )
		Case "threadedglobal"
			decl=New TGlobalDecl.Create( id,ty,init,attrs | DECL_THREADED )
		Case "field"
			decl=New TFieldDecl.Create( id,ty,init,attrs )

			If TFunctionPtrType(ty) Then
				TFunctionPtrType(ty).func.attrs :| FUNC_FIELD
			End If

		Case "const"  decl=New TConstDecl.Create( id,ty,init,attrs )
		Case "local"  decl=New TLocalDecl.Create( id,ty,init,attrs )
		End Select

		If decl.IsExtern()
			Local cdets:TCastDets

			If CParse( "=" )
				Local munged:String = ParseStringLit()
				
				If munged.Find("(") > 0 Then
					cdets = ParseExternCast(munged, True)
					If cdets Then
						decl.munged = cdets.name
					End If
				Else
					decl.munged = munged
				End If
			Else
				decl.munged=decl.ident
			EndIf

				If TFunctionPtrType(ty) Then
					TFunctionPtrType(ty).func.munged = decl.munged
					
					If Not cdets Then
						cdets = TCastDets(_externCasts.ValueForKey(TFunctionPtrType(ty).func.munged))
					End If
					
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

		' apply any function ptr metadata to decl
		If TFunctionPtrType(ty) Then
			If TFunctionPtrType(ty).func And TFunctionPtrType(ty).func.metadata Then
				decl.metadata = TFunctionPtrType(ty).func.metadata
			End If
		End If

		'meta data for variables
		Local meta:TMetaData = ParseMetaData()
		If meta Then
			decl.metadata = meta
		End If

		Return decl
	End Method

	Method ParseDecls:TList( toke$,attrs:Long, isField:Int = False )
		If toke Parse toke

		If isField Then
			Repeat
				If CParse("readonly") Then
					If attrs & DECL_READ_ONLY
						Err "Duplicate modifier 'ReadOnly'."
					End If

					attrs :| DECL_READ_ONLY

				Else If CParse("staticarray") Then
					If attrs & DECL_STATIC
						Err "Duplicate modifier 'Static'."
					End If

					attrs :| DECL_STATIC
				Else
					Exit
				End If
			Forever
		End If

		Local decls:TList=New TList'<Decl>
		Repeat
			Local decl:TDecl=ParseDecl( toke,attrs )
			decls.AddLast decl
			If Not CParse(",") Return decls
		Forever
	End Method

	Method ParseDeclStmts(initOnly:Int = False, attrs:Long = 0, mdecl:TModuleDecl  = Null)
		Local toke$=_toke
		NextToke
		Repeat
			Local decl:TDecl=ParseDecl( toke,attrs )
			If Not (attrs & DECL_EXTERN) Then
				_block.AddStmt New TDeclStmt.Create( decl )
			End If
			
			' reset the decl scope, adding decl to the block decl list.
			' this improves scope visibilty - for decls such as embedded functions
			If TConstDecl(decl) Or TGlobalDecl(decl) Then

				If mdecl Then
					mdecl.InsertDecl decl
				End If

				If Not (attrs & DECL_EXTERN) Then
					decl.scope = Null
					_block.InsertDecl(decl)
				End If
				
				If TGlobalDecl(decl) Then
					If initOnly Then
						decl.attrs :| DECL_INITONLY
						TGlobalDecl(decl).mscope = _module
					Else
						TGlobalDecl(decl).funcGlobal = True
					End If
				End If

			End If
			
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
	Method ParseMetaData:TMetadata()
		If Not CParse( "{" ) Then
			Return Null
		End If

		Local meta:TMetadata = New TMetadata

		SkipEols

		Repeat
			'reached end of meta data declaration
			If _toke="}" Then Exit

			If meta.metadataString Then
				meta.metadataString :+ " "
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
			Local key:String = _toke
			meta.metadataString :+ key

			'read next token
			NextToke()

			Local value:String
			' got a value
			If CParse("=") Then
				
				If _tokeType = TOKE_IDENT Then
					Err "Meta data must be literal constant"
				End If
				
				value = _toke
				meta.metadataString :+ "=" + value

				'read next token
				NextToke()
			Else
				value = "1"
				meta.metadataString :+ "=1"	
			End If
			
			meta.InsertMeta(key.ToLower(), value)
		Forever

		'continue to next token
		NextToke()

		'parse this into something
		Return meta
	End Method
	
	
	Method ParseFuncDecl:TFuncDecl( toke$, attrs:Long, parent:TScopeDecl = Null )
		SetErr

		If toke Parse toke

		Local id$
		Local ty:TType
		Local meth:Int = attrs & FUNC_METHOD
		Local meta:TMetadata
		Local noMangle:Int
		Local exported:Int
		Local inInterface:Int = attrs & DECL_ABSTRACT

		Local classDecl:TClassDecl = TClassDecl(parent)

		If attrs & FUNC_METHOD
			If _toke="new"
				If attrs & DECL_EXTERN
					Err "Extern classes cannot have constructors"
				EndIf
				id="New"
				NextToke
				attrs:|FUNC_CTOR
				attrs:&~FUNC_METHOD
				ty=ParseDeclType()
			Else If _toke="operator" Then
				attrs:|FUNC_OPERATOR
				NextToke
				
				Local t:String = _toke.ToLower()
				NextToke
				
				Select t
					Case "*","/","+","-","&","|","~~","^"
						id = t
					Case ":*",":/",":+",":-",":&",":|",":~~",":^"
						id = t
					Case "<",">"',"="',"<=",">=","=","<>"
						If CParse("=") Then
							t :+ "="
						Else If t = "<" And CParse(">") Then
							t :+ ">"
						End If
						id = t
					Case "="
						id = t
					Case "mod", "shl", "shr"
						id = t
					Case ":mod", ":shl", ":shr"
						id = t
					Case "[]"
						If CParse("=") Then t :+ "="
						id = t
					Default
						DoErr "Operator must be one of: * / + - & | ~~ :* :/ :+ :- :& :| :~~ < > <= >= = <> mod shl shr :mod :shl :shr [] []="
				End Select
				ty=ParseDeclType()
			Else
				id=ParseIdent()
				ty=ParseDeclType(attrs & DECL_API_STDCALL)
				If ty._flags & (TType.T_CHAR_PTR | TType.T_SHORT_PTR) Then
					DoErr "Illegal function return type"
				End If

				' Delete() return type should always be Void
				If id.ToLower() = "delete" Then
					attrs:|FUNC_DTOR
					If TIntType(ty) Then
						ty = New TVoidType
					End If
				End If
				' TODO: make sure Delete cannot be declared with parameters?
			EndIf
		Else
			'If Not (attrs & FUNC_PTR) Then
				id=ParseIdent()
				ty=ParseDeclType(attrs & DECL_API_STDCALL)
				' can only return "$z" and "$w" from an extern function.
				If ty._flags & (TType.T_CHAR_PTR | TType.T_SHORT_PTR) And Not (attrs & DECL_EXTERN) Then
					DoErr "Illegal function return type"
				End If
			'End If
		EndIf

		' every branch in that nested If block up there contains the line "ty=ParseDeclType()";
		' this already consumed all sets of parentheses and brackets belonging to this function declaration
		' so we will now extract our actual return type and args from the result
		Local args:TArgDecl[]
		If Not TFunctionPtrType(ty) Then
			DoErr "Expecting function type"
		Else
			Local fdecl:TFuncDecl = TFunctionPtrType(ty).func
			ty = fdecl.retTypeExpr
			args = fdecl.argDecls
			attrs :| (fdecl.attrs & DECL_API_FLAGS)
		End If
		
		Local declaredAttrs:Long
		While True
			If CParse( "nodebug" ) Then
				If declaredAttrs & DECL_NODEBUG Then Err "Duplicate modifier 'NoDebug'"
				declaredAttrs :| DECL_NODEBUG
				Continue
			End If
				
			If CParse( "final" )
				If Not classDecl Then
					Err "Final cannot be used with global functions"
				End If
				If inInterface Then
					If attrs & FUNC_METHOD Then
						Err "Final methods cannot appear in interfaces"
					Else
						Err "Final functions cannot appear in interfaces"
					End If
				End If
				If declaredAttrs & DECL_FINAL Then Err "Duplicate modifier 'Final'"
				declaredAttrs :| DECL_FINAL
				Continue
			End If
			
			If CParse( "abstract" )
				If Not classDecl Then
					Err "Abstract cannot be used with global functions"
				End If
				If classDecl And classDecl.attrs & DECL_FINAL Then
					Err "Abstract methods cannot appear in final types"
				End If
				If inInterface Then
					If attrs & FUNC_METHOD Then
						Err "Abstract cannot be used in interfaces (interface methods are automatically abstract)"
					Else
						Err "Abstract cannot be used in interfaces (interface functions are automatically abstract)"
					End If
				End If
				If declaredAttrs & DECL_ABSTRACT Then Err "Duplicate modifier 'Abstract'"
				declaredAttrs :| DECL_ABSTRACT
				Continue
			End If
			
			If CParse("override") Then
				If Not classDecl Then
					Err "Override cannot be used with global functions"
				End If
				If declaredAttrs & DECL_OVERRIDE Then Err "Duplicate modifier 'Override'"
				declaredAttrs :| DECL_OVERRIDE
				Continue
			End If
			
			If CParse("inline") And Not opt_debug Then
				If classDecl Then
					Err "Inline can only be used with global functions"
				End If
				If declaredAttrs & DECL_INLINE Then Err "Duplicate modifier 'Inline'"
				declaredAttrs :| DECL_INLINE
				Continue
			End If
				
			Exit
		Wend
		attrs :| declaredAttrs

		'meta data for functions/methods
		meta = ParseMetaData()
		
		If meta And meta.HasMeta("nomangle") Then
			If attrs & FUNC_METHOD Then
				Err "Only functions can specify NoMangle"
			Else
				noMangle = True
			End If
		End If
		
		If CParse("export") Then
			attrs :| DECL_EXPORT
			If attrs & FUNC_METHOD Then
				Err "Only functions can specify Export"
			Else
				exported = True
			End If
		End If
		
		attrs :| ParseCallConvention(attrs & DECL_API_STDCALL)
		
		If CParse( "nodebug" ) Then
			attrs :| DECL_NODEBUG
		End If

		Local funcDecl:TFuncDecl
		If attrs & FUNC_CTOR Then
			funcDecl=New TNewDecl.CreateF( id,ty,args,attrs )
			TNewDecl(funcDecl).cdecl = classdecl
		Else
			'If fdecl Then
			'	funcDecl = fdecl
			'	funcDecl.ident = id
			'Else
				funcDecl=New TFuncDecl.CreateF( id,ty,args,attrs )
			'End If
			funcDecl.noMangle = noMangle
			funcDecl.exported = exported
		End If
		If meta Then
			funcDecl.metadata = meta
		End If

		If funcDecl.IsExtern() Or (attrs & FUNC_PTR)
			funcDecl.munged=funcDecl.ident

			' a normal function pointer definition *probably* can't be defined with a munged name?
			' If there is an equals here, one can assume it is for an initialisation...
			'If (Not (attrs & FUNC_PTR)) Or (attrs & FUNC_PTR And Not (attrs & DECL_ARG)) Then
			Local cdets:TCastDets
			
			If Not (attrs & FUNC_PTR) Then
				If CParse( "=" )
					Local munged:String = ParseStringLit()
					
					If munged.Find("(") > 0 Then
						cdets = ParseExternCast(munged, True)
						If cdets Then
							funcDecl.munged = cdets.name
						End If
					Else
						funcDecl.munged = munged
					End If
				End If

				'Array $resize hack!
				'If funcDecl.munged="$resize"
				'	funcDecl.retTypeExpr=TType.emptyArrayType
				'EndIf
			EndIf

			If funcDecl.munged Then
				' look up extern cast list
				If Not cdets Then
					cdets = TCastDets(_externCasts.ValueForKey(funcDecl.munged))
				End If
				
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
					Local id$="New"
					NextToke
					'funcDecl.superCtor=New TInvokeSuperExpr.Create( id,ParseArgs( True ) )
					'funcDecl.AddStmt New TExprStmt.Create( funcDecl.superCtor )
					funcDecl.AddStmt New TExprStmt.Create( New TNewExpr.Create(ParseArgs(True), True))
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
	
	Method ParseCallConvention:Long(callConvention:Long = DECL_API_DEFAULT)
		If _tokeType <> TOKE_STRINGLIT Then
			Return callConvention
		End If
		
		Local api:String = ParseStringLit().ToLower()
		
		If api = "os" Then
			Select opt_platform
				Case "macos", "osx", "ios"
					api = "macos"
				Case "linux", "android", "raspberrypi", "haiku"
					api = "linux"
				Case "win32"
					api = "win32"
				Case "nx"
					api = "nx"
			End Select
		End If

		Select api
			Case "c", "blitz", "macos", "linux", "nx"
				Return DECL_API_CDECL
			Case "win32"
				' only if we are compiling for win32
				If opt_platform = "win32"
					Return DECL_API_STDCALL
				Else
					Return DECL_API_CDECL
				End If
		End Select
		
		Err "Unrecognized calling convention '" + api+ "'"
	End Method

	Method ParseFuncParamDecl:TArgDecl[]()
		Local args:TArgDecl[]
		Parse "("
		SkipEols
		If _toke<>")"
			Local nargs:Int
			Repeat
				Local attrs:Long
				If CParse("staticarray") Then
					attrs :| DECL_STATIC
				End If
				
				Local argId$=ParseIdent()

				Local ty:TType=ParseDeclType(attrs)

				Local init:TExpr
				
				' var argument?
				If CParse("var") Then
					If attrs & DECL_STATIC Then
						Err "Unexpected 'Var' for static array argument"
					End If
					ty = TType.MapToVarType(ty)
				Else If CParse( "=" )
					init=ParseExpr()
				Else
					If CParse( "[" ) And (attrs & DECL_STATIC) Then
						init = ParseExpr()
						Parse "]"
						ty=New TArrayType.Create( ty,1,, attrs & DECL_STATIC > 0 )
					End If
				End If
				
				Local arg:TArgDecl=New TArgDecl.Create( argId,ty,init,attrs )
				If args.Length=nargs args=args + New TArgDecl[10]
				args[nargs]=arg
				nargs:+1
				If _toke=")" Exit

				Parse ","
			Forever
			args=args[..nargs]
		EndIf
		Parse ")"
		Return args
	End Method
	
	Method ParseEnumDecl:TEnumDecl( toke:String )
		SetErr

		If toke Parse toke

		Local id:String = ParseIdent()
		Local ty:TType = ParseConstNumberType()

		If Not ty Then
			ty = New TIntType
		End If

		Local isFlags:Int = 0
		Local values:TEnumValueDecl[0]
		
		If CParse("flags")
			isFlags = True
		End If
		
		Local decl:TEnumDecl = New TEnumDecl.Create(id, ty, isFlags, values)

		Local nValues:Int
		
		Repeat
		
			SkipEols

			If CParse("end") Then
				Parse("enum")
				Exit
			End If
			
			If CParse("endenum") Then
				Exit
			End If
			
			Local valId:String = ParseIdent()
			Local value:TExpr
			
			If CParse( "=" ) Then
				value = ParseExpr()
			End If
			
			Local v:TEnumValueDecl = New TEnumValueDecl.Create(valId, nValues, value)
			If decl.values.Length = nValues Then
				decl.values = decl.values + New TEnumValueDecl[10]
			End If

			decl.values[nValues] = v
			nValues :+ 1
			
			CParse(",")
		
		Forever

		decl.values = decl.values[..nValues]

		Return decl
		
	End Method

	Method ParseClassDecl:TClassDecl( toke$,attrs:Long, templateDets:TTemplateDets = Null )
		SetErr

		Local calculatedStartLine:Int = _toker.Line()
		Local startLine:Int = _toker._line

		If toke Parse toke

		Local id$=ParseIdent()

		Local args:TList = New TList
		Local superTy:TIdentType
		Local imps:TIdentType[]
		Local meta:TMetadata

		'If (attrs & CLASS_INTERFACE) And (attrs & DECL_EXTERN)
		'	Err "Interfaces cannot be extern."
		'EndIf

		If CParse( "<" )

			If attrs & DECL_EXTERN
				Err "Extern classes cannot be generic."
			EndIf

			'If attrs & CLASS_INTERFACE
			'	Err "Interfaces cannot be generic."
			'EndIf

			'If attrs & CLASS_TEMPLATEARG
			'	Err "Class parameters cannot be generic."
			'EndIf

			Local nargs:Int
			Repeat
				'Local decl:TClassDecl=ParseClassDecl( "",CLASS_TEMPLATEARG )
				'If args.Length=nargs args=args + New TClassDecl[10]
				'args[nargs]=decl
				'nargs:+1
				Local arg:TTemplateArg = New TTemplateArg
				arg.ident = ParseIdent()
				
'				If CParse("extends") Then
'					arg.superTy = ParseIdentType()
'				End If
				
				args.AddLast arg

			Until Not CParse(",")
			'args=args[..nargs]

			Parse ">"

			If CParse( "where" ) Then
'DebugStop
				Repeat
					Local argIdent:String = ParseIdent()
					
					Parse("extends")
					
					Local found:Int
					For Local arg:TTemplateArg = EachIn args
						If arg.ident = argIdent Then
						
							Repeat
							
								arg.ExtendsType(ParseIdentType())
							
							Until Not CParse("and")
						
							found = True
							Exit
						EndIf
					Next
					If Not found Then
						Err "Use of undeclared type '" + argIdent + "'."
					End If
					
				Until Not CParse(",")
			End If
		EndIf

		If CParse( "extends" )
			'If attrs & CLASS_TEMPLATEARG
			'	Err "Extends cannot be used with class parameters."
			'EndIf

'			If CParse( "null" )
'
				If attrs & CLASS_STRUCT
					Err "Structs cannot be extended"
				EndIf
'
'				If Not (attrs & DECL_EXTERN)
'					Err "Only extern objects can extend null."
'				EndIf
'
'				superTy=Null
'
'			Else
			If attrs & CLASS_INTERFACE And Not (attrs & DECL_EXTERN)

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
			If Not (attrs & DECL_EXTERN) And Not (attrs & CLASS_STRUCT) Then
				superTy=New TIdentType.Create( "brl.classes.object" )
			End If
		EndIf

		If CParse( "implements" )

			If attrs & CLASS_STRUCT
				Err "Implements cannot be used with Structs"
			EndIf

			'If attrs & DECL_EXTERN
			'	Err "Implements cannot be used with external classes."
			'EndIf

			If attrs & CLASS_INTERFACE
				Err "Implements cannot be used with interfaces."
			EndIf

			'If attrs & CLASS_TEMPLATEARG
			'	Err "Implements cannot be used with class parameters."
			'EndIf

			Local nimps:Int
			Repeat
				If imps.Length=nimps imps=imps + New TIdentType[10]
				imps[nimps]=ParseIdentType()
				nimps:+1
			Until Not CParse(",")
			imps=imps[..nimps]
		EndIf

		Repeat
			If CParse( "final" )

				If attrs & CLASS_INTERFACE
					Err "Final cannot be used with interfaces."
				End If
				
				If attrs & CLASS_STRUCT
					Err "Final cannot be used with structs."
				End If
				
				If attrs & DECL_FINAL
					Err "Duplicate modifier 'Final'."
				End If

				If attrs & DECL_ABSTRACT
					Err "Classes cannot be both final and abstract."
				End If
				
				attrs:|DECL_FINAL

			Else If CParse( "abstract" )

				If attrs & CLASS_INTERFACE
					Err "Abstract cannot be used with interfaces."
				EndIf
				
				If attrs & CLASS_STRUCT
					Err "Abstract cannot be used with structs."
				EndIf
				
				If attrs & DECL_ABSTRACT
					Err "Duplicate modifier 'Abstract'."
				End If
				
				If attrs & DECL_FINAL
					Err "Types cannot be both final and abstract."
				End If

				attrs:|DECL_ABSTRACT
			Else
				Exit
			EndIf
		Forever

		'check for metadata
		meta = ParseMetaData()

		
		Local sargs:TTemplateArg[] = New TTemplateArg[args.Count()]
		Local i:Int = 0
		For Local arg:TTemplateArg = EachIn args
			sargs[i] = arg
			i :+ 1
		Next

		Local classDecl:TClassDecl=New TClassDecl.Create( id,sargs,superTy,imps,attrs )
		
		If meta Then
			If attrs & CLASS_STRUCT
				Err "Structs cannot store metadata."
			EndIf

			classDecl.metadata = meta
		End If

		If classDecl.IsExtern()
			classDecl.munged=classDecl.ident
			If CParse( "=" ) classDecl.munged=ParseStringLit()
		EndIf

		'If classDecl.IsTemplateArg() Return classDecl

		Local decl_attrs:Long=(attrs & DECL_EXTERN) | (attrs & DECL_NODEBUG) | (attrs & DECL_API_STDCALL)

		Repeat
			Local method_attrs:Long=decl_attrs|FUNC_METHOD | (attrs & DECL_NODEBUG)
			Local abst_attrs:Long = 0
			If attrs & CLASS_INTERFACE abst_attrs:|DECL_ABSTRACT
		
			SkipEols
			Select _toke
			Case "end"
				NextToke
				Exit
			Case "endtype"
				If attrs & CLASS_INTERFACE Then
					Err "Syntax error - expecting End Interface, not 'EndType'"
				End If
				If attrs & CLASS_STRUCT Then
					Err "Syntax error - expecting End Struct, not 'EndType'"
				End If
				toke = Null
				NextToke
				Exit
			Case "endstruct"
				If attrs & CLASS_INTERFACE Then
					Err "Syntax error - expecting End Interface, not 'EndStruct'"
				End If
				If Not (attrs & CLASS_STRUCT) Then
					Err "Syntax error - expecting End Type, not 'EndStruct'"
				End If
				toke = Null
				NextToke
				Exit
			Case "endinterface"
				If Not (attrs & CLASS_INTERFACE) And Not (attrs & CLASS_STRUCT) Then
					Err "Syntax error - expecting End Type, not 'EndInterface'"
				End If
				If Not (attrs & CLASS_INTERFACE) And (attrs & CLASS_STRUCT) Then
					Err "Syntax error - expecting End Struct, not 'EndInterface'"
				End If
				toke = Null
				NextToke
				Exit
			Case "private"
				If attrs & CLASS_INTERFACE Then
					Err "Private cannot be used with interfaces."
				End If
				NextToke
				decl_attrs=decl_attrs | DECL_PRIVATE
				decl_attrs:& ~DECL_PROTECTED
			Case "protected"
				If attrs & CLASS_INTERFACE Then
					Err "Protected cannot be used with interfaces."
				End If
				NextToke
				decl_attrs=decl_attrs | DECL_PROTECTED
				decl_attrs:& ~DECL_PRIVATE
			Case "public"
				NextToke
				decl_attrs:& ~DECL_PRIVATE
				decl_attrs:& ~DECL_PROTECTED
			Case "const","global","field","threadedglobal"
				Local extra_attrs:Long
				If _toke = "threadedglobal" Then
					extra_attrs = DECL_THREADED
				End If
			
				If attrs & DECL_EXTERN Then
					If (attrs & CLASS_INTERFACE) Then
						Err "Extern Interfaces can only contain methods."
					End If
					If Not (attrs & CLASS_STRUCT) Then
						Err "Extern Types can only contain methods."
					End If
				End If
				If (attrs & CLASS_INTERFACE) And _toke<>"const"
					Err "Interfaces can only contain constants and methods."
				EndIf
				If (attrs & CLASS_STRUCT) And _toke<>"field" And _toke<>"global" And _toke<>"threadedglobal"
					Err "Structs can only contain fields."
				EndIf
				
				classDecl.InsertDecls ParseDecls( _toke,decl_attrs | extra_attrs, _toke = "field")
			Case "method"
				If (attrs & CLASS_STRUCT) And (attrs & DECL_EXTERN) Then
					Err "Structs can only contain fields."
				EndIf
				Local decl:TFuncDecl=ParseFuncDecl( _toke,method_attrs | abst_attrs,classDecl )
				classDecl.InsertDecl decl
			Case "function"
				'If (attrs & CLASS_INTERFACE)
				'	Err "Interfaces can only contain constants and methods."
				'EndIf
				If attrs & CLASS_STRUCT Then
					If (attrs & DECL_EXTERN) Then
						Err "Structs can only contain fields."
					End If
				EndIf
				If attrs & DECL_EXTERN Then
					Err "Extern Types can only contain methods."
				End If
				Local decl:TFuncDecl=ParseFuncDecl( _toke,decl_attrs | abst_attrs,classDecl )
				classDecl.InsertDecl decl
			Case "type"
				If templateDets Then
					Local cdecl:TClassDecl = ParseClassDecl( _toke,DECL_NESTED, templateDets)
					cdecl = cdecl.GenClassInstance(templateDets.instArgs, False, Null, templateDets)
					classDecl.InsertDecl cdecl, True
				Else
					classDecl.InsertDecl ParseClassDecl( _toke,DECL_NESTED)
				End If
			Default
				Err "Syntax error - expecting class member declaration, not '" + _toke + "'"
			End Select
		Forever
		
		If Not args.IsEmpty() Then
			Local endline:Int = _toker._line
			classDecl.templateSource = New TTemplateRecord.Create(calculatedStartLine - 1, _toker._path, _toker.Join(startLine, endLine, "~n"))
		End If

		If toke Parse toke

		Return classDecl
	End Method
	
	Method ParseNativeStmt()
		If Not _toke.StartsWith("'!") Then
			Err "Syntax error - expecting '!"
		End If
		Local raw:String = _toke[2..]
		_block.AddStmt New TNativeStmt.Create( raw )
		NextToke
	End Method

	Method ParseModuleDecl:String( toke$,attrs:Long )
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
			filepath = ActualPath(filepath)
			Local origPath:String = RealPath(filepath)
			Local path:String = OutputFilePath(origPath, FileMung(), "i")

			If FileType( origPath )<>FILETYPE_FILE
				Err "File '"+ origPath +"' not found."
			EndIf

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
			If par.ParseModuleImport(_module, modpath, origPath, path, , , filepath, True) Return
		Else If filepath.startswith("-") Then
			If Not _app.fileimports.Contains(filepath) Then
				_app.fileimports.AddLast filepath
			End If
		Else
			If filepath.EndsWith(".h") Or filepath.EndsWith(".hpp") Or filepath.EndsWith(".hxx") Then
				If filepath.Find("*") = -1 Then
					_app.headers.AddLast filepath
				End If
			Else
				Local path:String = ActualPath(RealPath(filepath))
				If FileType( path )<>FILETYPE_FILE
					Err "File '"+ path +"' not found."
				End If
			End If
		End If

	End Method

	Method ImportAllModules(attrs:Long)

		' get all brl and pub modules
		Local mods:TList = EnumModules("brl")
		mods = EnumModules("pub", mods)

		For Local m:String = EachIn mods
			ImportModule(m, attrs)
		Next

	End Method
	
	Method ImportModule( modpath$,attrs:Long )
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
			
				Print "Warning: .x cast definition files are deprecated. You should now place the details in the extern function's alias string. (" + path + ")"

				ParseExternCast(LoadText( ePath ), False, ePath)

			End If
			
		Next

	End Method

	Method ParseExternCast:TCastDets(txt:String, single:Int = False, path:String = "")
		Local toker:TToker = New TToker.Create(path, txt)
		toker.NextToke

		Local dets:TCastDets
			
		While True

			SkipEolsToker(toker)

			If toker._tokeType = TOKE_EOF Exit

			dets = New TCastDets

			Local rt$=toker._toke

			If CParseToker(toker, "const") Then
				rt :+ " " + toker._toke
			End If

			If CParseToker(toker, "unsigned") Then
				rt :+ " " + toker._toke
			End If

			NextTokeToker(toker)
			
			If CParseToker(toker,"*") Then
				rt:+ "*"

				If CParseToker(toker,"*") Then
					rt:+ "*"
				End If
			End If


			If CParseToker(toker, "__stdcall") Then
				dets.api = "__stdcall"
			End If

			' fname
			Local fn$=toker._toke
			NextTokeToker(toker)

			dets.name = fn
			dets.retType = rt

			' add to global map (may be referenced by function ptr defs)
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

				If CParseToker(toker, "struct") Then
					at :+ " " + toker._toke
				End If

				NextTokeToker(toker)
				If CParseToker(toker, "*") Then
					at:+ "*"

					If CParseToker(toker, "const") Then
						at :+ " const"
					End If

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
			
			If single Then
				Exit
			End If
		Wend
		
		Return dets
	End Method

	Method ParseCurrentFile:Long(path:String, attrs:Long)

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

			Case "const"
				_module.InsertDecls ParseDecls( _toke,attrs )
			Case "global"
				ParseDeclStmts(True, attrs, _module)
			Case "threadedglobal"
				ParseDeclStmts(True, attrs | DECL_THREADED, _module)
			Case "struct"
				_module.InsertDecl ParseClassDecl( _toke,attrs | CLASS_STRUCT )
			Case "type"
				_module.InsertDecl ParseClassDecl( _toke,attrs)
			Case "interface"
				_module.InsertDecl ParseClassDecl( _toke,attrs|CLASS_INTERFACE|DECL_ABSTRACT )
			Case "enum"
				_module.InsertDecl ParseEnumDecl( _toke )
			Case "function"
				_module.InsertDecl ParseFuncDecl( _toke,attrs )
			Case "incbin"
				NextToke
				Local s:String = ParseStringLit()
				_app.mapStringConsts(s)
				Local ib:TIncBin = New TIncbin.Create(s, path)
				If Not ib Then
					DoErr "Incbin file '"+ s +"' not found."
				End If
				_app.incbins.AddLast(ib)
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

				If FileType( includeFile )<>FILETYPE_FILE
					DoErr "File '"+ includeFile +"' not found."
				EndIf

				'instead of "LoadText" "PreProcess" is used to include
				'handling of conditionals and comments
				Try
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
				Catch e:TStreamException
					DoErr "Failed to include file '" + includeFile + "' : '" + e.ToString() + "'"
				End Try

				'move on to next toke (after include "xyz.bmx")
				NextToke

			Default
				ParseStmt
				'Err "Syntax error - expecting declaration."
			End Select
		Wend

		Return attrs
	End Method

	Method ParseGeneric:Object(templateSource:TTemplateRecord, templateDets:TTemplateDets)
		Local toker:TToker = New TToker.Create(templateSource.file, templateSource.source, False, templateSource.start)
		Local parser:TParser = New TParser.Create( toker, _appInstance )
		
		Local m:TModuleDecl = New TModuleDecl
		parser._module = m
		
		Local cdecl:TClassDecl = Null
		
		Select parser._toke
		Case "type"
			cdecl = parser.ParseClassDecl(parser._toke,0, templateDets )
		Case "interface"
			cdecl = parser.ParseClassDecl(parser._toke, CLASS_INTERFACE|DECL_ABSTRACT, templateDets )
		End Select
		
		Return cdecl
	End Method

	Method ParseMain()

		SkipEols

		Local mattrs:Long
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
		
		If opt_buildtype = BUILDTYPE_APP Then
			ident = "m_" + ident
		End If

		_module=New TModuleDecl.Create( ident,munged,path,mattrs )

		_module.AddImport path,_module

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


		Local attrs:Long

		While _toke
			SetErr
			Select _toke.ToLower()
			Case "~n"
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
				NextToke
			Default
				Exit
			End Select
		Wend

		If opt_need_strict And Not (_module.attrs & (MODULE_STRICT | MODULE_SUPERSTRICT)) Then
			Err "Strict or SuperStrict must be declared at the start of the file."
		End If

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
			Case "nodebug"
				mainFunc.attrs :| DECL_NODEBUG
				attrs :| DECL_NODEBUG
				NextToke
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


	Method Create:TParser( toker:TToker,app:TAppDecl, unknownIdentsEvalFalse:Int = False )
		_toke="~n"
		_toker=toker
		_app=app
		SetErr
		NextToke
		Self.unknownIdentsEvalFalse = unknownIdentsEvalFalse
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
	Until toker.tokeType()<>TOKE_SPACE Or toker.Toke().Endswith("~n")

	Return toker._toke
End Function

Function PreProcess$( path$ )

	Local ifnest:Int,con:Int=1,line:Int,source:TStringList=New TStringList

	Local toker:TToker=New TToker.Create( path,LoadText( path ), True )

	PreProcessNextToke(toker)

	Repeat

		If line
			source.AddLast "~n"
			While toker.Toke() And Not toker.Toke().Endswith("~n") And toker.TokeType()<>TOKE_LINECOMMENT
				PreProcessNextToke(toker)
			Wend 

			If Not toker.Toke() Exit

			PreProcessNextToke(toker)
		EndIf
		
		line :+ 1

		' catch up with any skipped lines
		While line < toker._line - 1
			line:+1
			source.AddLast "~n"
		Wend

		If toker.TokeType()=TOKE_SPACE And Not toker.Toke().Endswith("~n") PreProcessNextToke(toker)

		If toker.Toke()<>"?"
			If con
				Local textline$
				While toker.Toke() And toker.Toke()<>"~n" And toker.TokeType()<>TOKE_LINECOMMENT
					Local toke$=toker.Toke()
					textline:+toke
					toker.NextToke()
				Wend
				If textline Then
					source.AddLast textline
				EndIf

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
			Catch Error:String
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

Function AppendLibInit:String(source:String)

	Local sb:TStringBuffer = TStringBuffer.Create(source)
	
	sb.Append("~n")
	sb.Append("Extern~n")
	sb.Append("Function bbLibInit()~n")
	sb.Append("End Extern~n")

	sb.Append("Function InitBRL() Export~n")
	sb.Append("bbLibInit()~n")
	sb.Append("End Function~n")
	
	Return sb.ToString()
End Function

'***** PUBLIC API ******

Function ParseApp:TAppDecl( path$ )

	Local app:TAppDecl=New TAppDecl

	_appInstance = app

	Local source$=PreProcess( path )
	'Local source:String = LoadString(path)
	
	If opt_makelib And opt_apptype Then
		source = AppendLibInit(source)
	End If

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

	' debug/release
	env.InsertDecl New TConstDecl.Create( "debug",New TIntType,New TConstExpr.Create( New TIntType,opt_debug ),0 )
	env.InsertDecl New TConstDecl.Create( "gdbdebug",New TIntType,New TConstExpr.Create( New TIntType,opt_gdbdebug ),0 )

	' threaded
	env.InsertDecl New TConstDecl.Create( "threaded",New TIntType,New TConstExpr.Create( New TIntType,opt_threaded ),0 )

	' macos
	env.InsertDecl New TConstDecl.Create( "macos",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="macos" Or opt_platform="osx" Or opt_platform="ios"),0 )
	env.InsertDecl New TConstDecl.Create( "macosx86",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="macos" Or opt_platform="osx" Or opt_platform="ios") And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "macosppc",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="macos" Or opt_platform="osx") And opt_arch="ppc"),0 )
	env.InsertDecl New TConstDecl.Create( "macosx64",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="macos" Or opt_platform="osx" Or opt_platform="ios") And opt_arch="x64"),0 )
	env.InsertDecl New TConstDecl.Create( "macosarm64",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="macos" Or opt_platform="osx" Or opt_platform="ios") And opt_arch="arm64"),0 )

	' osx
	env.InsertDecl New TConstDecl.Create( "osx",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="macos" Or opt_platform="osx" ),0 )
	env.InsertDecl New TConstDecl.Create( "osxx86",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="macos" Or opt_platform="osx")  And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "osxppc",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="macos" Or opt_platform="osx") And opt_arch="ppc"),0 )
	env.InsertDecl New TConstDecl.Create( "osxx64",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="macos" Or opt_platform="osx") And opt_arch="x64"),0 )
	env.InsertDecl New TConstDecl.Create( "osxarm64",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="macos" Or opt_platform="osx") And opt_arch="arm64"),0 )

	' ios
	env.InsertDecl New TConstDecl.Create( "ios",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="ios" ),0 )
	env.InsertDecl New TConstDecl.Create( "iosx86",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="ios" And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "iosx64",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="ios" And opt_arch="x64"),0 )
	env.InsertDecl New TConstDecl.Create( "iosarmv7",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="ios" And opt_arch="armv7"),0 )
	env.InsertDecl New TConstDecl.Create( "iosarm64",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="ios" And opt_arch="arm64"),0 )

	' windows
	env.InsertDecl New TConstDecl.Create( "win32",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="win32" ),0 )
	env.InsertDecl New TConstDecl.Create( "win32x86",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="win32" And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "win32x64",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="win64" And opt_arch="x64") Or (opt_platform="win32" And opt_arch="x64")),0 )
	env.InsertDecl New TConstDecl.Create( "win64",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="win64" And opt_arch="x64") Or (opt_platform="win32" And opt_arch="x64") Or (opt_platform="win32" And opt_arch="arm64")),0 )
	env.InsertDecl New TConstDecl.Create( "win32armv7",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="win32" And opt_arch="armv7"),0 )
	env.InsertDecl New TConstDecl.Create( "win32arm64",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="win32" And opt_arch="arm64"),0 )

	' linux
	env.InsertDecl New TConstDecl.Create( "linux",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="linux" Or opt_platform="android" Or opt_platform="raspberrypi")),0 )
	env.InsertDecl New TConstDecl.Create( "linuxx86",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="linux" Or opt_platform="android") And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "linuxx64",New TIntType,New TConstExpr.Create( New TIntType,(opt_platform="linux" Or opt_platform="android") And opt_arch="x64"),0 )
	env.InsertDecl New TConstDecl.Create( "linuxarm",New TIntType,New TConstExpr.Create( New TIntType, ((opt_platform="android" Or opt_platform="linux") And (opt_arch="arm" Or opt_arch="armeabi" Or opt_arch="armeabiv7a" Or opt_arch="arm64v8a")) Or (opt_platform="raspberrypi" And opt_arch="arm")),0 )
	env.InsertDecl New TConstDecl.Create( "linuxarm64",New TIntType,New TConstExpr.Create( New TIntType, (opt_platform="android" And opt_arch="arm64v8a") Or ((opt_platform="linux" Or opt_platform="raspberrypi") And opt_arch="arm64")),0 )

	' android
	env.InsertDecl New TConstDecl.Create( "android",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" ),0 )
	env.InsertDecl New TConstDecl.Create( "androidx86",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "androidx64",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And opt_arch="x64"),0 )
	env.InsertDecl New TConstDecl.Create( "androidarm",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And (opt_arch="arm" Or opt_arch="armeabi" Or opt_arch="armeabiv7a" Or opt_arch="arm64v8a") ),0 )
	env.InsertDecl New TConstDecl.Create( "androidarmeabi",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And opt_arch="armeabi"),0 )
	env.InsertDecl New TConstDecl.Create( "androidarmeabiv7a",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And opt_arch="armeabiv7a"),0 )
	env.InsertDecl New TConstDecl.Create( "androidarm64v8a",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="android" And opt_arch="arm64v8a"),0 )

	' raspberrypi - ARM only
	env.InsertDecl New TConstDecl.Create( "raspberrypi",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="raspberrypi" And (opt_arch="arm" Or opt_arch="arm64")),0 )
	env.InsertDecl New TConstDecl.Create( "raspberrypiarm",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="raspberrypi" And opt_arch="arm"),0 )
	env.InsertDecl New TConstDecl.Create( "raspberrypiarm64",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="raspberrypi" And opt_arch="arm64"),0 )

	' haiku
	env.InsertDecl New TConstDecl.Create( "haiku",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="haiku" And (opt_arch="x86" Or opt_arch="x64")),0 )
	env.InsertDecl New TConstDecl.Create( "haikux86",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="haiku" And opt_arch="x86"),0 )
	env.InsertDecl New TConstDecl.Create( "haikux64",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="haiku" And opt_arch="x64"),0 )

	' emscripten
	env.InsertDecl New TConstDecl.Create( "emscripten",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="emscripten" ),0 )
	env.InsertDecl New TConstDecl.Create( "emscriptenjs",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="emscripten" And opt_arch="js"),0 )

	' arch
	env.InsertDecl New TConstDecl.Create( "ppc",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="ppc" ),0 )
	env.InsertDecl New TConstDecl.Create( "x86",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="x86" ),0 )
	env.InsertDecl New TConstDecl.Create( "x64",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="x64" ),0 )
	env.InsertDecl New TConstDecl.Create( "arm",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="arm" Or opt_arch="armeabi" Or opt_arch="armeabiv7a" Or opt_arch="arm64v8a" ),0 )
	env.InsertDecl New TConstDecl.Create( "armeabi",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="armeabi" ),0 )
	env.InsertDecl New TConstDecl.Create( "armeabiv7a",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="armeabiv7a" ),0 )
	env.InsertDecl New TConstDecl.Create( "arm64v8a",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="arm64v8a" ),0 )
	env.InsertDecl New TConstDecl.Create( "js",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="js" ),0 )
	env.InsertDecl New TConstDecl.Create( "armv7",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="armv7" ),0 )
	env.InsertDecl New TConstDecl.Create( "arm64",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="arm64" ),0 )

	env.InsertDecl New TConstDecl.Create( "ptr32",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="x86" Or opt_arch="ppc" Or opt_arch="armv7" Or opt_arch="arm" Or opt_arch="armeabi" Or opt_arch="armeabiv7a" ),0 )
	env.InsertDecl New TConstDecl.Create( "ptr64",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="x64" Or opt_arch="arm64" Or opt_arch="arm64v8a" ),0 )

	' endian
	env.InsertDecl New TConstDecl.Create( "bigendian",New TIntType,New TConstExpr.Create( New TIntType,opt_arch="ppc" ),0 )
	env.InsertDecl New TConstDecl.Create( "littleendian",New TIntType,New TConstExpr.Create( New TIntType,opt_arch<>"ppc" ),0 )

	' opengles target platform
	env.InsertDecl New TConstDecl.Create( "opengles",New TIntType,New TConstExpr.Create( New TIntType, opt_platform="android" Or opt_platform="raspberrypi" Or opt_platform="emscripten" Or opt_platform="ios" Or (opt_platform="linux" And (opt_arch="arm" Or opt_arch="arm64"))),0 )

	' musl - linux only
	env.InsertDecl New TConstDecl.Create( "musl",New TIntType,New TConstExpr.Create( New TIntType,(opt_musl And (opt_platform="linux" Or opt_platform="android" Or opt_platform="raspberrypi"))),0 )

	' nx / switch
	env.InsertDecl New TConstDecl.Create( "nx",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="nx" ),0 )
	env.InsertDecl New TConstDecl.Create( "nxarm64",New TIntType,New TConstExpr.Create( New TIntType,opt_platform="nx" And opt_arch="arm64"),0 )	
		
	' new compiler
	env.InsertDecl New TConstDecl.Create( "bmxng",New TIntType,New TConstExpr.Create( New TIntType, True ),0 )

	' user defines
	If opt_userdefs Then
		Local defs:String[] = opt_userdefs.ToLower().Split(",")
		For Local def:String = EachIn defs
			def = def.Trim()
			If def Then
			
				Local name:String = def
				Local value:Int = 1
				
				Local dp:String[] = def.Split("=")
				If dp.length = 2 Then
					name = dp[0].Trim()
					value = Int(dp[1])
				End If
			
				env.InsertDecl New TConstDecl.Create( name,New TIntType,New TConstExpr.Create( New TIntType, value ),0 )
			End If
		Next
	End If

	PushEnv env

	Local toker:TToker=New TToker.Create( "",source )

	Local parser:TParser=New TParser.Create( toker,Null,True )

	Local val:String
	Try
		Local expr:TExpr=parser.ParseExpr()
	
		expr=expr.Semant()
	
		If ty expr=expr.Cast( ty )
	
		val=expr.Eval()
	Catch error:String
		val = "0"
	End Try
	
	PopEnv
	
	Return val
End Function

Type TCastDets

	Field name:String
	Field retType:String
	Field noGen:Int
	Field args:String[0]
	Field api:String
	
End Type
