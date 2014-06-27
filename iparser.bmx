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
SuperStrict

Import BRL.MaxUtil

Import "toker.bmx"


Const DECL_GLOBAL:Int = $10
Const DECL_FIELD:Int = $20
Const DECL_CONST:Int = $40
Const DECL_LOCAL:Int = $80


Type TIParser

	Field _toker:TToker
	Field _toke$
	Field _tokeType:Int
	Field _tokeSpace:Int
	Field _tokerStack:TList=New TList'<TToker>

	Method ParseModuleImport:Int(pmod:TModuleDecl, modpath:String, path:String, imp:String = Null, iData:String = Null, attrs:Int = 0, relPath:String = "")

		Const STATE_CLASS:Int = 1
		
		If Not modpath Then
			modpath = imp
		End If

		' already imported??
		If _appInstance.IsImported(modpath)
			' add import to the scope (so we can find decls in it later)
			pmod.imported.Insert(modpath, _appInstance.globalImports.ValueForKey(modpath))
			Return False
		End If
		
		Local _mod:TModuleDecl = New TModuleDecl.Create(modpath, "bb" + modpath, path, attrs)
		Select modpath
			Case "brl.classes", "brl.blitzkeywords"
				_mod.filepath :+ "." + modpath
		End Select

		_mod.declImported = True
		_mod.relpath = relPath

		If modpath = "brl.blitz" Then
			If pmod.imported.Contains(modpath) Then
				_mod = TModuleDecl(pmod.imported.ValueForKey(modpath))
			Else
				pmod.imported.Insert(modpath, _mod)
			End If
			
			' import Object and String definitions
			Local par:TIParser = New TIParser
			par.ParseModuleImport(_mod, "brl.classes", modulepath("brl.blitz"), modulepath("brl.blitz") + "\blitz_classes.i")
	
			' set up built-in keywords
			par = New TIParser
			par.ParseModuleImport(_mod, "brl.blitzkeywords", "", "", MakeKeywords())

		Else
			pmod.imported.Insert(modpath, _mod)
		End If

		_appInstance.globalImports.Insert(modpath, _mod)
		
		Local ipath:String
		
		'Local ipath:String = path + "\" + ModuleIdent(modpath) + ".release.macos.x86.i"
		If imp Then
			ipath = imp

			' add to imports
			pmod.imported.Insert(ipath, _mod)
			_appInstance.globalImports.Insert(ipath, _mod)
		Else
			ipath = path + "/" + ModuleIdent(modpath) + FileMung() + ".i"
		End If
		
		If Not iData Then

			If Not FileType(ipath) Then
				Err "Can't find interface for module '" + modpath + "'"
				Return False
			End If

			'Local ifile:String[] = LoadString(ipath).Split("~n")

			_toker = New TToker.Create( ipath,LoadString( ipath ) )
		Else
			_toker = New TToker.Create( ipath, iData)
		End If


	Local toker:TToker = _toker
	
		
	 Repeat
		Local pos:Int
	
		pos = toker._tokePos
		toker.NextToke
	
		
		Local line:Int
		Local state:Int
		Local class:TClassDecl
		Local stm:String
		
		
		
		Select toker.Toke().ToLower()
			Case "import"
			
				toker.NextToke()
				If toker.TokeType()=TOKE_SPACE toker.NextToke()
				
				' skip non-module imports
				If toker.TokeType()=TOKE_STRINGLIT

					Local iRelPath:String = ParseStringLit()

					SetErr
					
					If iRelPath.EndsWith(".bmx") Then
							
						Local origPath:String = RealPath(ExtractDir(path) + "/" + iRelPath)
						Local iPath:String = OutputFilePath(origPath, FileMung(), "i")
				
						If FileType( iPath )<>FILETYPE_FILE
							Err "File '"+ iPath +"' not found."
						EndIf
	
	
						If _mod.imported.Contains( iPath ) Continue
				
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
							'sanitize the path, remove non-allowed chars
							modpath = TStringHelper.Sanitize(modpath.ToLower())
						Else
							' todo file imports for apps
							'internalErr
						End If


'					Local mdecl:TDecl=TDecl(pmod.GetDecl( modpath ))

'					If Not mdecl
						New TIParser.ParseModuleImport( _mod, modpath, origPath, iPath, , , iRelPath)
'					Else
'						_mod.imported.Insert(modpath, mdecl)
'					EndIf
					Else
						If iRelPath.StartsWith("-") Then
							If Not _mod.fileImports.Contains(iRelPath) Then
								_mod.fileImports.AddLast(iRelPath)
							End If
						End If
					End If
				Else
				

					Local m:String = toker._toke
					toker.NextToke()
					
					Parse(".")
					
					m :+ "." + toker._toke
					
					SetErr
					Local mdecl:TDecl=TDecl(pmod.GetDecl( m ))
	
					If Not mdecl
						Local path:String = modulepath(m)
						' parse the imported module
						New TIParser.ParseModuleImport( _mod, m, path )
					Else
						_mod.imported.Insert(m, mdecl)
					EndIf

				End If
				
				Continue
				
			
			Case "moduleinfo"
				toker.nextToke()
				If toker.TokeType()=TOKE_SPACE toker.NextToke()
				Continue
			Case "~r", "~n"
				Continue
			Default
'DebugStop
				
				stm = toker.Toke()
				
				Local v:String = toker.NextToke()			

				Select v
					Case "^"

						toker.rollback(pos)
						toker.NextToke()
						' class decl
						class = ParseClassDecl( stm,0 )
						class.declImported = True
						_mod.InsertDecl(class)

						If CParse("F")
							class.attrs :| DECL_FINAL
						Else If CParse("A")
							class.attrs :| DECL_ABSTRACT
						Else If CParse("AF")
							class.attrs :| DECL_ABSTRACT | DECL_FINAL
						Else If CParse("E")
							class.attrs :| DECL_EXTERN
						End If
'DebugStop
						If CParse( "=" )
'DebugStop
							If Not class.IsExtern() Then
								class.munged=ParseStringLit()

								If class.ident <> "String" Then
									For Local fdecl:TFieldDecl = EachIn class._decls
										fdecl.munged = "_" + class.munged + "_" + fdecl.ident
										fdecl.munged = fdecl.munged.ToLower()
									Next
								End If
							Else
								Parse "0"
								If Not class.munged Then
									class.munged = class.ident
									
								End If
							End If
						EndIf

						DebugLog ""
						'state = STATE_CLASS
						'Exit
				'	Case "%"
				Default
					If toker._tokeType = TOKE_EOF
'DebugStop
						Exit
					End If

					Local a:Int
					Local ty:TType = ParseDeclType(a)

					If CParse("(") Then
						toker.rollback(pos)
						toker.NextToke()

						Local decl:TFuncDecl = ParseFuncDecl( _toke, 0 )
						decl.declImported = True
						
						If decl.attrs & FUNC_PTR Then
							ty = New TFunctionPtrType
							TFunctionPtrType(ty).func = decl
							'Local declInit:TExpr = decl.declInit
							'decl.declInit = Null
							Local gdecl:TGlobalDecl = New TGlobalDecl.Create( decl.ident,ty, Null, DECL_GLOBAL )
							gdecl.munged = decl.munged
							_mod.InsertDecl gdecl
							gdecl.declImported = True
						Else
							_mod.InsertDecl decl
						End If
						

					Else
'DebugStop
						toker.rollback(pos)
						toker.NextToke()
						
						Local decl:TDecl = ParseDecl( _toke, DECL_CONST | DECL_EXTERN)'DECL_GLOBAL | DECL_EXTERN )
						_mod.InsertDecl decl
						decl.declImported = True

					End If
				
				End Select
				
				'Continue
		End Select
			line :+ 1
			
		Forever
		
		
		Return True
		
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

	Method ParsePrimaryExpr:TExpr( stmt:Int )
	
		Local expr:TExpr

			Select _tokeType
			'Case TOKE_IDENT
			'	expr=New TIdentExpr.Create( ParseIdent() )
			Case TOKE_INTLIT
				expr=New TConstExpr.Create( New TIntType,_toke )
				NextToke
			Case TOKE_LONGLIT
				expr=New TConstExpr.Create( New TLongType,_toke )
				NextToke
			Case TOKE_FLOATLIT
				expr=New TConstExpr.Create( New TFloatType,_toke )
				NextToke
			Case TOKE_STRINGLIT
				expr=New TConstExpr.Create( New TStringType,BmxUnquote( _toke ) )
				NextToke
			Default
				Err "Syntax error - unexpected token '"+_toke+"'"
			End Select

		Return expr
		
	End Method

	Method ParseClassDecl:TClassDecl( toke$,attrs:Int )
		SetErr

		'If toke Parse toke
		
		Local id$=ParseIdent()
		Local args:TClassDecl[]
		Local superTy:TIdentType
		Local imps:TIdentType[]

		DebugLog "Found Class :  " + id
'End If
		If CParse( "^" )

			If CParse( "null" )
			
				superTy=Null
				
			Else
				superTy=ParseIdentType()
				'If superTy.ident <> "Object" Then
				'	superTy = TIdentType(superTy.Semant())
				'EndIf
			EndIf
		Else
			superTy = New TIdentType.Create( "brl.classes.object" )
		EndIf
'DebugStop
		Local classDecl:TClassDecl=New TClassDecl.Create( id,args,superTy,imps,attrs )
		
		If classDecl.IsExtern()
			classDecl.munged=classDecl.ident
			If CParse( "=" ) classDecl.munged=ParseStringLit()
		EndIf
		
		'If classDecl.IsTemplateArg() Return classDecl

		Local decl_attrs:Int=(attrs & DECL_EXTERN)
		
		Local method_attrs:Int=decl_attrs
		If attrs & CLASS_INTERFACE method_attrs:|DECL_ABSTRACT

		Repeat
			SkipEols
			'If IsSpace(Asc(_toker._toke))
			'	_toker.NextToke
			'End If
			
			Select _toker._toke
			Case "{"
				'_toker.
				NextToke
			Case "}"
				'_toker.
				NextToke
				Exit
			Case "-" ' method
				'DebugStop
				'_toker.
				NextToke
				
				Local decl:TFuncDecl = ParseFuncDecl( _toke,method_attrs|FUNC_METHOD )
				'If decl.IsCtor() decl.retTypeExpr=New TObjectType.Create( classDecl )
				classDecl.InsertDecl decl
				
			Case "+" ' function
				NextToke
				
				Local decl:TFuncDecl = ParseFuncDecl( _toke,method_attrs )
				'If decl.IsCtor() decl.retTypeExpr=New TObjectType.Create( classDecl )
				classDecl.InsertDecl decl

			Case "." ' field
				NextToke
				decl_attrs :| DECL_FIELD
				Local decl:TDecl= ParseDecl( _toke,decl_attrs )
				classDecl.InsertDecl decl
			Rem
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
				End Rem
			'Default
			'	Err "Syntax error - expecting class member declaration."
			End Select
			
			If _toker._tokeType = TOKE_IDENT Then
				' Const / Global?
				'NextToke
				
				'decl_attrs :| DECL_CONST
				
				Local decl:TDecl= ParseDecl( _toke,decl_attrs | DECL_CONST)
				classDecl.InsertDecl decl
			End If
			
		Forever
		
		If toke CParse toke
		
		Return classDecl

	End Method

	Method Parse( toke$ )
		If Not CParse( toke )
			Err "Syntax error - expecting '"+toke+"'."
		EndIf
	End Method

	Method ParseIdent$()
		Select _toker._toke.tolower()
		Case "@" _toker.NextToke
		Case "string","array","object"
		Default	
			If _toker._tokeType<>TOKE_IDENT Err "Syntax error - expecting identifier."
		End Select
		Local id$=_toker._toke
		NextToke
		Return id
	End Method

	Method ParseIdentType:TIdentType()
		Local id$=ParseIdent()
		
		While CParse( "." )
			id:+"."+ParseIdent()
		Wend
		Local args:TIdentType[]
		Return New TIdentType.Create( id,args )
	End Method

	Method NextToke$()
		Local toke$=_toke
		
		_tokeSpace=False
		
		Repeat
			_toke=_toker.NextToke()
			_tokeType=_toker.TokeType()
			If _tokeType<>TOKE_SPACE Exit
			_tokeSpace=True
		Forever
		
		If _tokeType=TOKE_KEYWORD _toke=_toke.ToLower()

		If toke="," SkipEols

		Return _toke
	End Method
	
	Method CParse:Int( toke$ )
		If _toker._toke.tolower()<>toke.tolower()
			Return False
		EndIf
		'_toker.
		NextToke
		Return True
	End Method

	Method SkipEols()
		Local found:Int = True
		While found
			found = False
			If CParse( "~n" )
				found = True
			End If
			If CParse("~r")
				found = True
			End If
		Wend
		
		SetErr
	End Method

	Method ParseStringLit$()
		If _toker._tokeType<>TOKE_STRINGLIT Err "Expecting string literal."
		Local str$=BmxUnquote( _toker._toke )
		'_toker.
		NextToke
		Return str
	End Method

	Method ParseFuncDecl:TFuncDecl( toke$,attrs:Int )
		SetErr

		'If toke Parse toke
	
		Local id$
		Local ty:TType
		Local meth:Int = attrs & FUNC_METHOD
		
		If attrs & FUNC_METHOD
			If _toker._toke.tolower() = "new"
'DebugStop
				If attrs & DECL_EXTERN
					Err "Extern classes cannot have constructors"
				EndIf
				id=_toker._toke
				NextToke
				attrs:|FUNC_CTOR
				attrs:&~FUNC_METHOD
				ty=ParseDeclType(attrs)
			Else
				id=ParseIdent()
				ty=ParseDeclType(attrs)
			EndIf
		Else
			id=ParseIdent()
			ty=ParseDeclType(attrs)
		EndIf
		
		If attrs & FUNC_METHOD
'DebugLog "Found Method :  " + id
		Else
'DebugLog "Found Function :  " + id
		End If
		
		Local args:TArgDecl[]
		
		Parse "("
		SkipEols

		If _toker._toke<>")"
			Local nargs:Int
			Repeat

				Local pos:Int, tokeType:Int
				pos = _toker._tokePos
				tokeType = _toker._tokeType
'DebugStop
				Local id$=ParseIdent()
'If id = "compareFunc" DebugStop
				Local ty:TType=ParseDeclType(attrs)
				Local init:TExpr

				If CParse( "(") Then
'DebugStop
					' function pointer
					_toker.rollback(pos, tokeType)
					_toker._toke = id
					'_toker.NextToke()
					Local decl:TFuncDecl = ParseFuncDecl( id, FUNC_PTR | FUNC_INIT )
					ty = New TFunctionPtrType
					TFunctionPtrType(ty).func = decl
					
				End If
				
				If CParse("Var") Then
					ty = TType.MapToVarType(ty)
				End If

				If CParse( "=" ) Then
					'DebugLog "TODO : parse default values..."
					If CParse("$") Then
						' a string default
						init = ParseUnaryExpr()
					Else
						If Not TFunctionPtrType(ty) Then
							init = ParseUnaryExpr()
							If TArrayType(ty) Then
								If TConstExpr(init) And TConstExpr(init).value="bbEmptyArray" Then
									init = New TNullExpr.Create(TType.nullObjectType)
								End If
							Else If TObjectType(ty) Or TIdentType(ty) Then
								If TConstExpr(init) And TConstExpr(init).value="bbNullObject" Then
									init = New TNullExpr.Create(TType.nullObjectType)
								End If
							End If
						Else
							' munged reference to default function pointer
							Local defaultFunc:String = ParseStringLit()
							Local func:TFuncDecl = TFuncDecl(TFunctionPtrType(ty).func.Copy())
							init = New TInvokeExpr.Create(func)
							func.munged = defaultFunc
							init.exprType = ty
							
						End If
					End If
					' has a default value
					'DebugStop
					'init=ParseExpr()
				End If
				
				Local arg:TArgDecl=New TArgDecl.Create( id,ty,init )
				If args.Length=nargs args=args + New TArgDecl[10]
				args[nargs]=arg
				nargs:+1
				If _toker._toke=")" Exit
				Parse ","
			Forever
			args=args[..nargs]
		EndIf
		Parse ")"
		
		Repeat		
			If CParse( "F" )
				attrs:|DECL_FINAL
			Else If CParse( "A" )
				attrs:|DECL_ABSTRACT
			Else If CParse( "property" )
				If attrs & FUNC_METHOD
					attrs:|FUNC_PROPERTY
				Else
					Err "Only methods can be properties."
				EndIf
			Else
				Exit
			EndIf
		Forever
		
		Local funcDecl:TFuncDecl=New TFuncDecl.CreateF( id,ty,args,attrs )
		
		funcDecl.retType = ty
		
		If CParse("&") Then
			funcDecl.attrs :| DECL_POINTER
		End If
		
		'If funcDecl.IsExtern()
		If Not (funcDecl.attrs & (FUNC_PTR | FUNC_INIT)) Then
		'	funcDecl.munged=funcDecl.ident
			If CParse( "=" )

				If CParse("mem")
					If CParse(":")
						If CParse("p")
							If CParse("(") Then
								
								'DebugStop

								'_toker.NextToke()
								
								funcDecl.munged = ParseStringLit()
								
								Cparse(")")

							EndIf
						End If
					End If
				Else
					'If Not (funcDecl.attrs & (FUNC_PTR | FUNC_INIT)) Then
						funcDecl.munged=ParseStringLit()
					'Else
'DebugStop
					'	funcDecl.init = ParseStringLit()
					'End If
					
				End If



				
				'Array $resize hack!
				'If funcDecl.munged="$resize"
				'	funcDecl.retTypeExpr=TType.emptyArrayType
				'EndIf
				
			EndIf
		End If
		
		' read function cast stuff
		If CParse(":") Then
			' ret type
			Local rt$=_toker._toke
			NextToke
			If CParse("*") Then
				rt:+ "*"
				
				If CParse("*") Then
					rt:+ "*"
				End If
			End If
			
			funcDecl.castTo = rt

			' fname
			Local fn$=_toker._toke
			NextToke

			' args
			Parse("(")

			If Not CParse(")") Then
				Local i:Int = 0			
				Repeat
					Local at$=_toker._toke
					
					If CParse("const") Then
						at :+ " " + _toker._toke
					End If
					
					If CParse("unsigned") Then
						at :+ " " + _toker._toke
					End If


					NextToke
					If CParse("*") Then
						at:+ "*"
						
						If CParse("*") Then
							at:+ "*"
						End If
					End If


					' function pointer
					If CParse("(") Then

						Parse("*")
						Parse(")")
						at :+ "(*)"
						
						Parse("(")
						at :+ "("
						
						While Not CParse(")")
							NextToke
							at :+ _toker._toke
						Wend
						
						at :+ ")"
					End If



					args[i].castTo = at
					
				
					If _toker._toke=")" Exit
					Parse ","
					
					i:+ 1
				Forever
			End If
				

		End If
			
		'	Return funcDecl
		'EndIf
		
		If funcDecl.attrs & DECL_POINTER Then
			funcDecl.attrs :| FUNC_PTR
		End If
		
		'If funcDecl.IsAbstract() Return funcDecl
		Return funcDecl
		
	End Method
	
	Method ParseDecl:TDecl( toke$,attrs:Int )
		SetErr
		Local pos:Int, tokeType:Int
		pos = _toker._tokePos
		tokeType = _toker._tokeType

		Local id$=ParseIdent()
		Local ty:TType
		Local init:TExpr

		If attrs & DECL_EXTERN
			ty=ParseDeclType(attrs)

		'Else If CParse( ":=" )
	'		init=ParseExpr()
		Else
			ty=ParseDeclType(attrs)
			
			
				If CParse( "(") Then
'DebugStop
					' function pointer
					_toker.rollback(pos, tokeType)
					_toker._toke = id
					'_toker.NextToke()
					Local decl:TFuncDecl = ParseFuncDecl( id, FUNC_PTR | FUNC_INIT )
					ty = New TFunctionPtrType
					TFunctionPtrType(ty).func = decl
					
				End If

Rem
			If CParse( "=" )
				' TODO init=ParseExpr()
				If CParse("$") Then
					' string value
					init = ParseUnaryExpr()
				Else
					init = ParseUnaryExpr()
				End If
				'DebugLog "TODO : ParseExpression"
			Else If CParse( "[" )
				'Local ln:TExpr=ParseExpr()
				Parse "]"
				'While CParse( "[]" )
				'	ty=New TArrayType.Create(ty)
				'Wend
				'init=New TNewArrayExpr.Create( ty,ln)
				'ty=New TArrayType.Create( ty )
			Else If toke<>"const"
				init=New TConstExpr.Create( ty,"" )
			Else
				Err "Constants must be initialized."
			EndIf
End Rem
		EndIf
		
		Local decl:TValDecl
		
		If attrs & DECL_GLOBAL
			decl=New TGlobalDecl.Create( id,ty,init,attrs )
		Else If attrs & DECL_FIELD
			decl=New TFieldDecl.Create( id,ty,init,attrs )
		Else If attrs & DECL_CONST
			decl=New TConstDecl.Create( id,ty,init,attrs )
		Else If attrs & DECL_LOCAL
			decl=New TLocalDecl.Create( id,ty,init,attrs )
		EndIf
'DebugStop
'		If decl.IsExtern() 
			If CParse( "=" )
				'decl.munged=ParseStringLit()
				
				If CParse("mem")
					' change to global
'DebugStop
					If attrs & DECL_CONST Then
						attrs :| DECL_GLOBAL
						attrs :~ DECL_CONST
						decl=New TGlobalDecl.Create( id,ty,init,attrs )
					End If
				
				
					If CParse(":")
						If CParse("p")
							If CParse("(") Then
								
'								DebugStop

								'_toker.NextToke()
								
								decl.munged = ParseStringLit()
								
								Cparse(")")

							EndIf
						End If
					Else
						If CParse("(") Then
								
'							DebugStop

								'_toker.NextToke()
								
							decl.munged = ParseStringLit()
							
							Cparse(")")

						EndIf
					End If
				Else
'					init = ParseUnaryExpr()
					
					If TStringType(ty)
						If CParse("$") Then
							decl.declInit = ParseUnaryExpr()
'							decl.init=New TConstExpr.Create(ty, ParseStringLit())
						End If
					Else
						' a default value ?
						decl.declInit = ParseUnaryExpr()
'					Local value:String
						
						'_toker.NextToke()
						
'						If CParse("-") Then
'DebugStop
'							value = "-"
'							_toker.NextToke()
'						End If
						
'						decl.init = New TConstExpr.Create(ty, value + _toker._toke)
'						_toker.NextToke()
					End If
				End If
				
				
				
			Else
				decl.munged=decl.ident
			EndIf
'		EndIf

		Return decl
	End Method

	Method ParseDeclType:TType(attrs:Int Var)
		Local ty:TType
		Select _toker._toke
		'Case "?"
		'	NextToke
		'	ty=TType.boolType
		Case "%"
			NextToke
			ty=New TIntType

			If CParse("%") Then
				ty = New TLongType
			End If
			
			If CParse("&") And Not (attrs & DECL_FIELD) Then
				attrs :| DECL_GLOBAL
				attrs :~ DECL_CONST
			End If

			' pointer
			If CParse( "*" ) Then

				ty = TType.MapToPointerType(ty)
				'If ty = TType.longType Then
				'	ty = TType.longPointerType
				'Else
				'	ty = TType.intPointerType
				'End If

				' pointer pointer
				If CParse( "*" ) Then
					ty = TType.MapToPointerType(ty)
					'If ty = TType.longPointerType Then
					'	ty = TType.longPointerPtrType
					'Else
					'	ty = TType.intPointerPtrType
					'End If
				End If
				
			End If
			
		Case "#"
			NextToke
			ty=New TFloatType

			If CParse("&")  And Not (attrs & DECL_FIELD) Then
				attrs :| DECL_GLOBAL
				attrs :~ DECL_CONST
			End If

			If CParse( "*" ) Then
				ty = TType.MapToPointerType(ty)
				'ty = TType.floatPointerType

				' pointer pointer
				If CParse( "*" ) Then
					ty = TType.MapToPointerType(ty)
					'ty = TType.floatPointerPtrType
				End If
			End If
			
		Case "$"
			NextToke
			ty=New TStringType

			If CParse("z") Then
				ty._flags :| TType.T_CHAR_PTR
			Else If CParse("w") Then
				ty._flags :| TType.T_SHORT_PTR
			End If

			If CParse( "&" )  And Not (attrs & DECL_FIELD)
				attrs :| DECL_GLOBAL
				attrs :~ DECL_CONST
			End If
		Case "!"
			NextToke
			ty=New TDoubleType

			If CParse("&")  And Not (attrs & DECL_FIELD) Then
				attrs :| DECL_GLOBAL
				attrs :~ DECL_CONST
			End If

			If CParse( "*" ) Then
				ty = TType.MapToPointerType(ty)
				'ty = TType.doublePointerType

				' pointer pointer
				If CParse( "*" ) Then
					ty = TType.MapToPointerType(ty)
					'ty = TType.doublePointerPtrType
				End If
			End If

		Case ":"
			NextToke
			ty=ParseNewType()
			
			If CParse("*") Then
				If TIdentType(ty) Then
					ty = TType.MapToPointerType(ty)
					'ty = New TIdentPtrType.Create(TIdentType(ty).ident, TIdentType(ty).args)
				End If
			End If
			
			CParse("&")
		Case "@"
			NextToke
			ty=New TByteType
			
			If CParse("@") Then
				ty = New TShortType
			End If

			
			If CParse( "&" )
'DebugStop
			End If
			
			If CParse( "*" ) Then

				ty = TType.MapToPointerType(ty)
				'If ty = TType.byteType Then
				'	ty = TType.bytePointerType
				'Else
				'	ty = TType.shortPointerType
				'End If
				
				' byte pointer pointer ?
				If CParse("*")  Then
					ty = TType.MapToPointerType(ty)
					'If ty = TType.byteType Then
					'	ty = TType.bytePointerPtrType
					'Else
					'	ty = TType.shortPointerPtrType
					'End If
				End If
				
			End If
' TODO
'		Case "!" ' BaH Double
'			NextToke
'			ty=TType.doubleType
		Default
			'If _module.IsStrict() Err "Illegal type expression."
'DebugStop
			ty=New TIntType
		End Select
		While CParse( "[]" )
			ty=New TArrayType.Create( ty )
		Wend
		
		If CParse( "&" ) Then
		End If

		While CParse( "[]" )
			ty=New TArrayType.Create( ty )
		Wend
		
		Return ty
	End Method

	Method ParseNewType:TType()
		If CParse( "byte" ) Or CParse( "@" )
			Local ty:TType = New TByteType
			If CParse("ptr") Or CParse( "*" ) Then
				Return TType.MapToPointerType(ty)
			End If
			Return ty
		End If
		If CParse( "short" )
			Local ty:TType = New TShortType
			If CParse("ptr") Then
				Return TType.MapToPointerType(ty)
			End If
			Return ty
		End If
		If CParse( "int" ) Or CParse( "%" )
			Local ty:TType = New TIntType
			If CParse("ptr") Or CParse( "*" ) Then
				Return TType.MapToPointerType(ty)
			End If
			Return ty
		End If
		If CParse( "float" )
			Local ty:TType = New TFloatType
			If CParse("ptr") Then
				Return TType.MapToPointerType(ty)
			End If
			Return ty
		End If
		If CParse( "string" ) Return New TStringType
		If CParse( "object" ) Return New TIdentType.Create( "brl.classes.object" )
		If CParse( "long" )
			Local ty:TType = New TLongType
			If CParse("ptr") Then
				Return TType.MapToPointerType(ty)
			End If
			Return ty
		End If
		If CParse( "double" )
			Local ty:TType = New TDoubleType
			If CParse("ptr") Then
				Return TType.MapToPointerType(ty)
			End If
			Return ty
		End If
		Return ParseIdentType()
	End Method

	Method SetErr()
		If _toker.Path()
			_errInfo=FormatError(_toker.Path(), _toker.Line(), 0)
		EndIf
	End Method

End Type


