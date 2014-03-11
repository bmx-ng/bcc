SuperStrict

Framework brl.StandardIO

Import "ctranslator.bmx"

Local args:String[] = ParseArgs(AppArgs[1..])

If args.length = 0 Then
	Print "bcc[ng] Release Version " + version
End If

If args.length <> 1 Then
	CmdError("Command line error")
End If

opt_filepath = args[0]


If opt_buildtype = BUILDTYPE_MODULE Then
	If opt_filepath.ToLower() = ModuleSource(opt_modulename).ToLower() Then
		opt_ismain = True
	End If
End If

Local app:TAppDecl = ParseApp(opt_filepath)

app.Semant()

Local trans:TCTranslator = New TCTranslator

trans.TransApp(app)

Local makeApp:Int = False
If opt_apptype Then
	makeApp = True
End If

Local mung:String = FileMung(makeApp)

SaveInterface(opt_filepath, trans, mung)
SaveHeader(opt_filepath, trans, mung)
SaveSource(opt_filepath, trans, mung)


Function SaveInterface(file:String, trans:TCTranslator, mung:String)

	Local path:String

	If opt_buildtype = BUILDTYPE_MODULE Then

		If opt_ismain Then
			' module interface
			path = ModuleInterface(opt_modulename, mung)
		Else
			' file interface
			path = OutputFilePath(file, mung, "i")
		End If

	Else

		' file interface
		path = OutputFilePath(file, mung, "i")

	End If

	SaveText(trans.JoinLines("interface"), path)

End Function

Function SaveHeader(file:String, trans:TCTranslator, mung:String)

	Local path:String = OutputFilePath(file, mung, "h")

	Local header:String = BuildHeaderName(path).ToUpper().Replace(".", "_")
	Local text:String = HeaderComment()
	text :+ "#ifndef " + header + "~n"
	text :+ "#define " + header + "~n~n"

	If opt_buildtype = BUILDTYPE_MODULE And opt_modulename = "brl.blitz" Then
		text :+ "#include <brl.mod/blitz.mod/blitz.h>~n"
	End If

	text :+ trans.JoinLines("head")
	text :+ "~n~n#endif~n"

	SaveText(text, path)

End Function

Function SaveSource(file:String, trans:TCTranslator, mung:String)

	Local path:String = OutputFilePath(file, mung, "c")

	SaveText(trans.JoinLines("source"), path)

End Function
