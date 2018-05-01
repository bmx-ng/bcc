' Copyright (c) 2013-2018 Bruce A Henderson
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
'SuperStrict

'Import BRL.MaxUtil

'Import "parser_factory.bmx"


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

	Method ParseModuleImport:Int(pmod:TModuleDecl, modpath:String, path:String, imp:String = Null, iData:String = Null, attrs:Int = 0, relPath:String = "", isFileImport:Int = 0)

		Const STATE_CLASS:Int = 1

		If Not modpath Then
			modpath = imp
		End If

		' already imported??
		If _appInstance.IsImported(modpath)
			' add import to the scope (so we can find decls in it later)
			' but don't add it if pmod is the apps' main module
			If _appInstance.mainModule <> pmod Then
				pmod.AddImport(modpath, _appInstance.globalImports.ValueForKey(modpath))
			End If
			Return False
		Else If imp Then
			' if "imp" is set, this is a file import. We need to check for it too, or we may end up importing it twice.
			If _appInstance.IsImported(imp)
				' add import to the scope (so we can find decls in it later)
				' but don't add it if pmod is the apps' main module
				If _appInstance.mainModule <> pmod Then
					pmod.AddImport(imp, _appInstance.globalImports.ValueForKey(imp))
				End If
				Return False
			End If
		End If
		
		Local prefix:String
		If isFileImport And opt_buildtype = BUILDTYPE_APP Then
			prefix = "m_"
		End If
		Local _mod:TModuleDecl = New TModuleDecl.Create(prefix + modpath, "bb" + modpath, path, attrs)
		Select modpath
			Case "brl.classes", "brl.blitzkeywords"
				_mod.UpdateFilePath(_mod.filepath + "." + modpath)
		End Select

		_mod.declImported = True
		_mod.relpath = relPath
		_mod.pmod = pmod

		If modpath = "brl.blitz" Then
			If pmod.imported.Contains(modpath) Then
				_mod = TModuleDecl(pmod.imported.ValueForKey(modpath))
			Else
				pmod.AddImport(modpath, _mod)
			End If
			
			' import Object and String definitions
			Local par:TIParser = New TIParser
			If FileType(modulepath("brl.blitz") + "\blitz_classes." + opt_platform + ".i") Then
				par.ParseModuleImport(_mod, "brl.classes", modulepath("brl.blitz"), modulepath("brl.blitz") + "\blitz_classes." + opt_platform + ".i")
			Else
				par.ParseModuleImport(_mod, "brl.classes", modulepath("brl.blitz"), modulepath("brl.blitz") + "\blitz_classes.i")
			End If
	
			' set up built-in keywords
			par = New TIParser
			par.ParseModuleImport(_mod, "brl.blitzkeywords", "", "", MakeKeywords())

		Else
			pmod.AddImport(modpath, _mod)
		End If

		_appInstance.globalImports.Insert(modpath, _mod)
		
		Local ipath:String
		
		'Local ipath:String = path + "\" + ModuleIdent(modpath) + ".release.macos.x86.i"
		If imp Then
			ipath = imp

			' add to imports
			pmod.AddImport(ipath, _mod)
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
			Case "superstrict"
				_mod.attrs :| MODULE_SUPERSTRICT
				Continue
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
						_mod.AddImport(m, mdecl)
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

				stm = toker.Toke()
				
				Local v:String = toker.NextToke()			

				Select v
					Case "^"

						toker.rollback(pos)
						toker.NextToke()
						' class decl
						class = ParseClassDecl( stm,0 )
						class.declImported = True

						Local parsed:Int
						For Local i:Int = 0 Until _toke.Length
							Select _toke[i]
								Case Asc("F")
									class.attrs :| DECL_FINAL
									parsed = True
								Case Asc("A")
									class.attrs :| DECL_ABSTRACT
									'ApplyFunctionAttributes(class, DECL_ABSTRACT)
									parsed = True
								Case Asc("S")
									class.attrs :| CLASS_STRUCT
									parsed = True
								Case Asc("E")
									class.attrs :| DECL_EXTERN
									ApplyFunctionAttributes(class, DECL_EXTERN)
									parsed = True
								Case Asc("W")
									class.attrs :| DECL_API_STDCALL
									ApplyFunctionAttributes(class, DECL_API_STDCALL)
									parsed = True
								Case Asc("I")
									class.attrs :| CLASS_INTERFACE
									ApplyFunctionAttributes(class, DECL_ABSTRACT)
									parsed = True
								Case Asc("G")
									class.attrs :| CLASS_GENERIC
									parsed = True
							End Select
						Next

						If parsed Then
							NextToke
						End If

						If CParse( "=" )

							If Not class.IsExtern() Then
								class.munged=ParseStringLit()

								If class.ident <> "String" Then
									For Local fdecl:TFieldDecl = EachIn class._decls
										fdecl.munged = "_" + class.munged + "_" + fdecl.ident
										fdecl.munged = fdecl.munged.ToLower()
									Next
								End If
								
								' process generic details
								If class.attrs & CLASS_GENERIC Then

									If _toke <> "," Then
										Err "Syntax error - unexpected token '" + _toke + "'"
									End If
									
									NextToke

									Parse "<"
									Local args:TType[]
									Local nargs:Int
									Repeat
										Local arg:TType = ParseType()
										If args.Length=nargs args=args+ New TType[10]
										args[nargs]=arg
										nargs:+1
									Until Not CParse(",")
									args=args[..nargs]
									Parse ">"

									Parse "{"
									' line no
									
									If _tokeType <> TOKE_INTLIT Then
										Err "Syntax error - unexpected token '" + _toke + "'"
									End If
									
									Local line:Int = _toke.ToInt()
									
									NextToke
									Parse ","
									
									' source size
									If _tokeType <> TOKE_INTLIT Then
										Err "Syntax error - unexpected token '" + _toke + "'"
									End If
									
									Local size:Int = _toke.ToInt()

									NextToke
									Parse ","
									
									' path
									If _tokeType <> TOKE_STRINGLIT Then
										Err "Syntax error - unexpected token '" + _toke + "'"
									End If
									
									Local path:String = BmxUnquote(_toke)
									
									NextToke
									Parse ","


									' source
									If _tokeType <> TOKE_STRINGLIT Then
										Err "Syntax error - unexpected token '" + _toke + "'"
									End If
									
									Local source:String = BmxUnquote(_toke)
									
									NextToke

									Parse "}"

									class.templateSource = TTemplateRecord.Load(line, path, size, source)

									Local toker:TToker = New TToker.Create(path, class.templateSource.source, False, line)
									Local parser:TParser = New TParser.Create( toker, _appInstance )
									
									Local m:TModuleDecl = New TModuleDecl
									parser._module = m
									
									Local cdecl:TClassDecl = Null
									
									Select parser._toke
										Case "type"
											cdecl = parser.ParseClassDecl(parser._toke,0)
										Case "interface"
											cdecl = parser.ParseClassDecl(parser._toke, CLASS_INTERFACE|DECL_ABSTRACT )
									End Select

									Local ty:TType = args[0]

									Local genDecl:TClassDecl = TClassDecl(_mod.GetDecl(cdecl.IdentLower()))

									If Not genDecl Then
										genDecl = TClassDecl(pmod.GetDecl(cdecl.identLower()))
									End If
									
									If genDecl Then
									
										If Not TIdentType(ty) Or (TIdentType(ty) And TIdentType(ty).ident <> "?") Then
										
											cdecl = genDecl.GenClassInstance(args, True)

											cdecl.scope.munged = class.munged
											cdecl.scope.scope = _appInstance
										
										End If
									
										' don't add to module
										class = Null
									
									Else

										class = cdecl
										class.declImported = True
										
										If Not TIdentType(ty) Or (TIdentType(ty) And TIdentType(ty).ident <> "?") Then
										
											cdecl = class.GenClassInstance(args)
											cdecl.declImported = True
											
											_mod.munged = class.munged
										End If
										
									End If
									
								End If
								
							Else
								Parse "0"
								If Not class.munged Then
									class.munged = class.ident
									
								End If
							End If
						EndIf

						If class Then
							_mod.InsertDecl(class)
						End If

						'state = STATE_CLASS
						'Exit
				'	Case "%"
				Default
					If toker._tokeType = TOKE_EOF
						Exit
					End If

					Local a:Int
					Local ty:TType = ParseDeclType(a)

					If CParse("(") Then
						toker.rollback(pos)
						toker.NextToke()

						Local decl:TFuncDecl = ParseFuncDecl( _toke, 0 )
						decl.declImported = True

						' an array of function pointers?
						If CParse( "&" ) Then
						End If

						While IsArrayDef()
							ty = ParseArrayType(ty)
				
							If CParse( "&" ) Then
							End If
						Wend
						
						If decl.attrs & FUNC_PTR Then

							Local fpty:TType = New TFunctionPtrType
							TFunctionPtrType(fpty).func = decl
							
							If TArrayType(ty) Then
								TArrayType(ty).elemType = fpty
							Else
								ty = fpty
							End If
							
							'Local declInit:TExpr = decl.declInit
							'decl.declInit = Null
							Local gdecl:TGlobalDecl = New TGlobalDecl.Create( decl.ident,ty, Null, DECL_GLOBAL )
							gdecl.munged = decl.munged
							_mod.InsertDecl gdecl
							gdecl.declImported = True
							
							If CParse( "=" )
				
								If CParse("mem")
								
									If CParse(":")
										If CParse("p")
											If CParse("(") Then
				
												gdecl.munged = ParseStringLit()
				
												' for function pointers, ensure actual function reference is set too.
												'If TFunctionPtrType(gdecl.declTy) Then
												'	TFunctionPtrType(gdecl.declTy).func.munged = gdecl.munged
												'Else If TArrayType(gdecl.declTy) Then
												'	
												'End If
												TFunctionPtrType(fpty).func.munged = gdecl.munged
				
												Parse(")")
				
											EndIf
										End If
									Else
										If CParse("(") Then
				
											gdecl.munged = ParseStringLit()
				
											Parse(")")
				
										EndIf
									End If
								Else
									If TStringType(ty)
										If CParse("$") Then
											gdecl.declInit = ParseUnaryExpr()
										End If
									Else
										' a default value ?
										gdecl.declInit = ParseUnaryExpr()
									End If
								End If
							End If
	
						Else
							_mod.InsertDecl decl
						End If
						
					Else

						toker.rollback(pos)
						toker.NextToke()

						Local decl:TDecl = ParseDecl( _toke, DECL_CONST | DECL_EXTERN)'DECL_GLOBAL | DECL_EXTERN )
						_mod.InsertDecl decl
						decl.declImported = True

					End If

				End Select
				
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
				Local value:String = _toke
				NextToke
				If CParse("!") Then
					expr=New TConstExpr.Create( New TDoubleType,value )
				Else
					CParse("#")
					expr=New TConstExpr.Create( New TFloatType,value )
				End If
			Case TOKE_STRINGLIT
				expr=New TConstExpr.Create( New TStringType,BmxUnquote( _toke, True ) )
				NextToke
			Case TOKE_IDENT
				If _toke = "nan" Or _toke = "inf" Then
					Local value:String = _toke
					NextToke
					If CParse("!") Then
						expr=New TConstExpr.Create( New TDoubleType,value )
					Else
						CParse("#")
						expr=New TConstExpr.Create( New TFloatType,value )
					End If
				Else
					Err "Syntax error - unexpected token '"+_toke+"'"
				End If
			Default
				Err "Syntax error - unexpected token '"+_toke+"'"
			End Select

		Return expr
		
	End Method

	Method ParseClassDecl:TClassDecl( toke$,attrs:Int )
		SetErr

		'If toke Parse toke
		
		Local id$=ParseIdent()
		Local args:TTemplateArg[]
		Local superTy:TIdentType
		Local imps:TIdentType[]

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

		' implements
		If CParse("@") Then
			Local nimps:Int
			Repeat
				If imps.Length=nimps imps=imps + New TIdentType[10]
				imps[nimps]=ParseIdentType()
				nimps:+1
			Until Not CParse(",")
			imps=imps[..nimps]
		End If
		
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
		Case "string","___array","object"
		Case "?"
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
		
		Local args:TType[]
		If CParse( "<" )
			Local nargs:Int
			Repeat
				Local arg:TType = ParseType()
				If args.Length=nargs args=args+ New TType[10]
				args[nargs]=arg
				nargs:+1
			Until Not CParse(",")
			args=args[..nargs]
			Parse ">"
		EndIf
		
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

	Method CParseToker:Int( toker:TToker, toke$ )
		If toker._toke.ToLower()<>toke
			Return False
		EndIf
		NextTokeToker(toker)
		Return True
	End Method

	Method NextTokeToker$(toker:TToker)
		Repeat
			toker.NextToke()
		Until toker.tokeType()<>TOKE_SPACE

		Return toker._toke
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
		Local str$=BmxUnquote( _toker._toke, True )
		'_toker.
		NextToke
		Return str
	End Method

	Method ParseFuncDecl:TFuncDecl( toke$,attrs:Int, returnType:TType = Null )
		SetErr

		'If toke Parse toke
	
		Local id$
		Local ty:TType
		Local meth:Int = attrs & FUNC_METHOD

		If Not returnType Then		
			If attrs & FUNC_METHOD
				If _toker._toke.tolower() = "new"
					If attrs & DECL_EXTERN
						Err "Extern classes cannot have constructors"
					EndIf
					id=_toker._toke
					NextToke
					attrs:|FUNC_CTOR
					attrs:&~FUNC_METHOD
					ty=ParseDeclType(attrs, True)
				Else
					If _toker._tokeType = TOKE_STRINGLIT Then
						id = ParseStringLit()
					Else
						id=ParseIdent()
					End If
					ty=ParseDeclType(attrs, True)
				EndIf
			Else
				id=ParseIdent()
				ty=ParseDeclType(attrs, True)
			EndIf
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
		
		If returnType Then
			Return New TFuncDecl.CreateF(Null, returnType, args, 0)
		End If

		Local fdecl:TFuncDecl
		' wait.. so everything until now was a function pointer return type, and we still have to process the function declaration...
		If _toke = "(" Then
			Local retTy:TType = New TFunctionPtrType
			TFunctionPtrType(retTy).func = New TFuncDecl.CreateF("",ty,args,attrs )
			TFunctionPtrType(retTy).func.attrs :| FUNC_PTR
			fdecl = ParseFuncDecl("", attrs, retTy)
			ty = retTy
		End If

		Local parsed:Int
		For Local i:Int = 0 Until _toke.Length
			Select _toke[i]
				Case Asc("F")
					attrs:| DECL_FINAL
					parsed = True
				Case Asc("W")
					attrs:| DECL_API_STDCALL
					parsed = True
				Case Asc("P")
					attrs:| DECL_PRIVATE
					parsed = True
				Case Asc("A")
					attrs:| DECL_ABSTRACT
					parsed = True
				Case Asc("O")
					attrs:| FUNC_OPERATOR
					parsed = True
				Case Asc("R")
					attrs:| DECL_PROTECTED
					parsed = True
			End Select
		Next

		If parsed Then
			NextToke
		End If

		Local funcDecl:TFuncDecl
		If attrs & FUNC_CTOR Then
			funcDecl = New TNewDecl.CreateF( id,ty,args,attrs )
		Else
			If fdecl Then
				funcDecl = fdecl
				funcDecl.ident = id
			Else
				funcDecl = New TFuncDecl.CreateF( id,ty,args,attrs )
			End If
		End If
		
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
								
								funcDecl.munged = ParseStringLit()
								
								Cparse(")")

							EndIf
						End If
					End If
				Else
					funcDecl.munged=ParseStringLit()
				End If

			End If
		End If
		
		' read function cast stuff
		If CParse(":") Then
			' ret type
			Local rt$=_toker._toke

			If CParse("unsigned") Then
				rt :+ " " + _toker._toke
			End If

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
					
					If attrs & DECL_FIELD Then
						decl.attrs :| FUNC_METHOD
					End If
					
					' an array of function pointers?
					If CParse( "&" ) Then
					End If

					While IsArrayDef()
						ty = ParseArrayType(ty)
			
						If CParse( "&" ) Then
						End If
					Wend

				End If
				
				If CParse("`") Then
					If CParse("`") Then
						attrs :| DECL_PROTECTED
					Else
						attrs :| DECL_PRIVATE
					End If
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
			
			If TFunctionPtrType(ty) Then
				TFunctionPtrType(ty).func.attrs :| FUNC_FIELD
			End If
			
		Else If attrs & DECL_CONST
			decl=New TConstDecl.Create( id,ty,init,attrs )
		Else If attrs & DECL_LOCAL
			decl=New TLocalDecl.Create( id,ty,init,attrs )
		EndIf
'DebugStop
'		If decl.IsExtern() 
			If CParse( "=" )

				If CParse("mem")
					' Change to global
					' Until this point, it was "probably" a const, but now we know for sure
					' that it must be a global.
					If attrs & DECL_CONST Then
						attrs :| DECL_GLOBAL
						attrs :~ DECL_CONST
						decl=New TGlobalDecl.Create( id,ty,init,attrs )
					End If
				
				
					If CParse(":")
						If CParse("p")
							If CParse("(") Then

								decl.munged = ParseStringLit()

								' for function pointers, ensure actual function reference is set too.
								If TFunctionPtrType(decl.declTy) Then
									TFunctionPtrType(decl.declTy).func.munged = decl.munged
								End If

								Parse(")")

							EndIf
						End If
					Else
						If CParse("(") Then

							decl.munged = ParseStringLit()

							Parse(")")

						EndIf
					End If
				Else
					If TStringType(ty)
						If CParse("$") Then
							decl.declInit = ParseUnaryExpr()
						End If
					Else
						' a default value ?
						decl.declInit = ParseUnaryExpr()
					End If
				End If
				
				
				
			Else
				decl.munged=decl.ident
			EndIf
'		EndIf

		Return decl
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
			If CParseToker(toker, "[]") Then
				Exit
			End If
			
			If Not CParseToker(toker, "[") Then
				isDef = False
				Exit
			End If
		
			While CParseToker(toker, ",")
			Wend
			
			If Not CParseToker(toker, "]") Then
				isDef = False
				Exit
			End If
			Exit
		Wend
		Return isDef
	End Method

	Method ParseDeclType:TType(attrs:Int Var, fn:Int = False)
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
			ElseIf CParse("z") Then
				ty = New TSizetType
			ElseIf CParse("j") Then
				ty = New TInt128Type
			ElseIf CParse("w") Then
				ty = New TWParamType
			ElseIf CParse("x") Then
				ty = New TLParamType
			End If
			
			If CParse("&") And Not (attrs & DECL_FIELD) Then
				attrs :| DECL_GLOBAL
				attrs :~ DECL_CONST
			End If

			' pointer
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			
		Case "|"
			NextToke
			ty=New TUIntType

			If CParse("|") Then
				ty = New TULongType
			End If
			
			If CParse("&") And Not (attrs & DECL_FIELD) Then
				attrs :| DECL_GLOBAL
				attrs :~ DECL_CONST
			End If

			' pointer
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			
		Case "#"
			NextToke
			ty=New TFloatType

			If CParse("&")  And Not (attrs & DECL_FIELD) Then
				attrs :| DECL_GLOBAL
				attrs :~ DECL_CONST
			End If

			' pointer
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			
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
			
			If CParse("k") Then
				ty = New TFloat128Type
			Else If CParse("m") Then
				ty = New TDouble128Type
			Else If CParse("h") Then
				ty = New TFloat64Type
			End If

			If CParse("&")  And Not (attrs & DECL_FIELD) Then
				attrs :| DECL_GLOBAL
				attrs :~ DECL_CONST
			End If

			' pointer
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend

		Case ":"
			NextToke
			ty=ParseNewType()
			
			If CParse("*") Then
				If TIdentType(ty) Then
					ty = TType.MapToPointerType(ty)

					While CParse( "*" )
						ty = TType.MapToPointerType(ty)
					Wend

				End If
			End If
			
			CParse("&")
		Case "?"
			NextToke
			
			attrs :| DECL_EXTERN
			
			If CParse("?") Then
				attrs :| CLASS_INTERFACE
			End If
			
			ty=ParseNewType()
			
			If CParse("*") Then
				If TIdentType(ty) Then
					ty = TType.MapToPointerType(ty)

					While CParse( "*" )
						ty = TType.MapToPointerType(ty)
					Wend

				End If
			End If
			
			CParse("&")
		Case "~~"
			NextToke
			
			attrs :| DECL_EXTERN | CLASS_STRUCT
			
			ty=ParseNewType()
			
			If CParse("*") Then
				If TIdentType(ty) Then
					ty = TType.MapToPointerType(ty)

					While CParse( "*" )
						ty = TType.MapToPointerType(ty)
					Wend

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
			
			' pointer
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
' TODO
'		Case "!" ' BaH Double
'			NextToke
'			ty=TType.doubleType
		Default
			'If _module.IsStrict() Err "Illegal type expression."
'DebugStop
			If Not fn Then
				ty=New TIntType
			End If
		End Select

		If CParse( "&" ) Then
		End If

		While IsArrayDef()
			ty = ParseArrayType(ty)

			If CParse( "&" ) Then
			End If
		Wend

		If CParse( "&" ) Then
		End If
		
		Return ty
	End Method

	Method ParseNewType:TType()
		If CParse( "byte" ) Or CParse( "@" )
			Local ty:TType = New TByteType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "short" )
			Local ty:TType = New TShortType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "int" ) Or CParse( "%" )
			Local ty:TType = New TIntType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "uint" ) Or CParse( "|" )
			Local ty:TType = New TUIntType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "float" )
			Local ty:TType = New TFloatType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "string" ) Return New TStringType
		If CParse( "object" ) Return New TIdentType.Create( "brl.classes.object" )
		If CParse( "long" )
			Local ty:TType = New TLongType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "ulong" )
			Local ty:TType = New TULongType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "double" )
			Local ty:TType = New TDoubleType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "size_t" )
			Local ty:TType = New TSizeTType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "int128" )
			Local ty:TType = New TInt128Type
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "float64" )
			Local ty:TType = New TFloat64Type
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "float128" )
			Local ty:TType = New TFloat128Type
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "double128" )
			Local ty:TType = New TDouble128Type
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "wparam" )
			Local ty:TType = New TWParamType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		If CParse( "lparam" )
			Local ty:TType = New TLParamType
			While CParse("ptr")
				ty = TType.MapToPointerType(ty)
			Wend
			While CParse( "*" )
				ty = TType.MapToPointerType(ty)
			Wend
			Return ty
		End If
		Return ParseIdentType()
	End Method
	
	Method ApplyFunctionAttributes(classDecl:TClassDecl, attrs:Int)
		For Local decl:TFuncDecl = EachIn classDecl._decls
			decl.attrs :| attrs
		Next
	End Method

	Method SetErr()
		If _toker.Path()
			_errInfo=FormatError(_toker.Path(), _toker.Line(), 0)
		EndIf
	End Method

	Method ParseType:TType()
		Local ty:TType=CParsePrimitiveType()
		If ty Return ty
		Return ParseIdentType()
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

End Type
