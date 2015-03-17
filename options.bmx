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

Import "base.configmap.bmx"

Const version:String = "0.55"

Const BUILDTYPE_APP:Int = 0
Const BUILDTYPE_MODULE:Int = 1

Const APPTYPE_NONE:Int = 0
Const APPTYPE_CONSOLE:Int = 1
Const APPTYPE_GUI:Int = 2

Global WORD_SIZE:Int = 4

' buildtype
'    module
'    app
Global opt_buildtype:Int = BUILDTYPE_APP
' modulename
'    name of the module to build
Global opt_modulename:String
' arch
'    x86
'    ppc
'    x64
'    arm
'    armeabi
'    armeabiv7a
Global opt_arch:String
' platform
'    win32
'    macos
'    linux
'    android
'    raspberrypi
Global opt_platform:String
' framework
Global opt_framework:String
' filename
'    the base filename for app/module to compile against
Global opt_filename:String
' outfile
'    full path to the outputfile (excluding final extension - there will be a .h, .c and .i generated)
Global opt_outfile:String
' apptype
'    console
'    gui
Global opt_apptype:Int = APPTYPE_NONE
' debug
Global opt_debug:Int = True
' threaded
Global opt_threaded:Int = False
' release
Global opt_release:Int = False
' quiet
Global opt_quiet:Int = False
' verbose
Global opt_verbose:Int = False
' ismain
'    this is the main file for either the module, or the application.
Global opt_ismain:Int = False
' issuperstrict
'
Global opt_issuperstrict:Int = False
' gdbdebug
'    output debug useful for gdb, #line <bmx line> <bmx file> 
Global opt_gdbdebug:Int = False

Global opt_filepath:String

Function CmdError(details:String = Null, fullUsage:Int = False)
	Local s:String = "Compile Error"
	If details Then
		s:+ ": " + details
	End If
	s:+ "~n"
	
	's:+ Usage(fullUsage)
	
	Throw s
End Function

Function ParseArgs:String[](args:String[])

	DefaultOptions()
	
	CheckConfig()
	
	Local count:Int

	While count < args.length
	
		Local arg:String = args[count]
		
		If arg[..1] <> "-" Then
			Exit
		End If
		
		Select arg[1..]
			Case "q"
				opt_quiet=True
			Case "v"
				opt_verbose=True
			Case "r"
				opt_debug=False
				opt_release=True
			Case "h"
				opt_threaded=True
			Case "g"
				count:+1
				If count = args.length Then
					CmdError "Command line error - Missing arg for '-g'"
				End If
				opt_arch = args[count].ToLower()
			Case "m"
				count:+1
				If count = args.length Then
					CmdError "Command line error - Missing arg for '-m'"
				End If
				opt_buildtype = BUILDTYPE_MODULE
				opt_modulename = args[count].ToLower()
			Case "o"
				count:+1
				If count = args.length Then
					CmdError "Command line error - Missing arg for '-o'"
				End If
				opt_outfile = args[count]
			Case "p"
				count:+1
				If count = args.length Then
					CmdError "Command line error - Missing arg for '-p'"
				End If
				opt_platform = args[count].ToLower()
			Case "t"
				count:+1
				If count = args.length Then
					CmdError "Command line error - Missing arg for '-t'"
				End If
				Local apptype:String = args[count].ToLower()
				Select apptype
					Case "console"
						opt_apptype = APPTYPE_CONSOLE
					Case "gui"
						opt_apptype = APPTYPE_GUI
					Default
						CmdError "Command line error - Invalid app type '" + opt_apptype + "'"
				End Select
			Case "f"
				count:+1
				If count = args.length Then
					CmdError "Command line error - Missing arg for '-f'"
				End If
				opt_framework = args[count]
			Case "d"
				opt_gdbdebug=True
		End Select
	
		count:+ 1
	Wend
	
	If opt_buildtype = BUILDTYPE_MODULE Then
		opt_apptype = APPTYPE_NONE
	End If
	
	If opt_arch = "x64" Then
		WORD_SIZE = 8
	End If

	Return args[count..]

End Function

Function DefaultOptions()
?x86
	opt_arch = "x86"
?ppc
	opt_arch = "ppc"
?x64
	opt_arch = "x64"
?arm
	opt_arch = "arm"
?armeabi
	opt_arch = "armeabi"
?armeabiv7a
	opt_arch = "armeabiv7a"
?arm64v8a
	opt_arch = "arm64v8a"
?js
	opt_arch = "js"
?

?win32
	opt_platform = "win32"
?macos
	opt_platform = "macos"
?linux
	opt_platform = "linux"
?android
	opt_platform = "android"
?raspberrypi
	opt_platform = "raspberrypi"
?emscripten
	opt_platform = "emscripten"
?
End Function

Function CheckConfig()
	Local config:TConfigMap = New TConfigMap.Init("bcc.conf")

	'try to load an OS-specific path (so all bcc builds could share
	'one single bcc.conf)
	Local osBmxPath:String = ""
	?win32
		osBmxPath = config.GetString("BMXPATH_WIN32")
	?linux
		osBmxPath = config.GetString("BMXPATH_LINUX")
	?macos
		osBmxPath = config.GetString("BMXPATH_MACOS")
	?android
		' override BMXPATH_LINUX if available
		Local tmp:String = config.GetString("BMXPATH_ANDROID")
		If tmp Then
			osBmxPath = tmp
		End If
	?raspberrypi
		' override BMXPATH_LINUX if available
		Local tmp:String = config.GetString("BMXPATH_RASPBERRYPI")
		If tmp Then
			osBmxPath = tmp
		End If
	?
	'load default/generic path
	If osBmxPath = "" Then osBmxPath = config.GetString("BMXPATH")

	'replace windows backslashes with crossplatform slashes
	osBmxPath = osBmxPath.Replace("\", "/")

	If osBmxPath <> "" Then putenv_("BMXPATH="+osBmxPath)
End Function
