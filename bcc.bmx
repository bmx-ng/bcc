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

Framework brl.StandardIO

Import "ctranslator.bmx"
Import "base.stringhelper.bmx"

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
SaveIncBinHeader(opt_filepath, trans, FileMung(False), app)


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

	Local header:String = BuildHeaderName(path).ToUpper()
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

Function SaveIncBinHeader(file:String, trans:TCTranslator, mung:String, app:TAppDecl)

	If app.genIncBinHeader Then
		Local path:String = OutputFilePath(file, mung, "incbin.h")

		SaveText(trans.JoinLines("incbin"), path)
	End If

End Function
