SuperStrict

Import BRL.LinkedList
Import BRL.Map
Import BRL.FileSystem

Import "options.bmx"

Global ENV_LANG$

Global _errInfo$
Global _errStack:TList = New TList

' bytes offset to the first field
Global OBJECT_BASE_OFFSET:Int = 8
' 4 bytes on 32-bit, 8 bytes on 64-bit
Global POINTER_SIZE:Int = 4

Function PushErr( errInfo$ )
	_errStack.AddLast _errInfo
	_errInfo=errInfo
End Function

Function PopErr()
	_errInfo=String(_errStack.RemoveLast())
End Function

Function Err( err$ )
	Print _errInfo+" : Error : "+err
'	Print _errInfo+" "+err
DebugStop
	End
End Function

Function InternalErr()
	Print _errInfo+" : Error : Internal error."
	DebugStop
	'Error _errInfo+" : Error : Internal error."
End Function

Function IsSpace:Int( ch:Int )
	Return ch<=Asc(" ")
End Function

Function IsDigit:Int( ch:Int )
	Return ch>=Asc("0") And ch<=Asc("9")
End Function

Function IsAlpha:Int( ch:Int )
	Return (ch>=Asc("A") And ch<=Asc("Z")) Or (ch>=Asc("a") And ch<=Asc("z"))
End Function

Function IsBinDigit:Int( ch:Int )
	Return ch=Asc("0") Or ch=Asc("1")
End Function

Function IsHexDigit:Int( ch:Int )
	Return IsDigit(ch) Or (ch>=Asc("A") And ch<=Asc("F")) Or (ch>=Asc("a") And ch<=Asc("f"))
End Function

Function Todo() 
	Err "TODO!"
End Function

'enquote depending on ENV_LANG
'
Function LangEnquote$( str$ )
	str=str.Replace( "\","\\" )
	str=str.Replace( "~q","\~q" )
	str=str.Replace( "~n","\n" )
	str=str.Replace( "~r","\r" )
	str=str.Replace( "~t","\t" )
'	str=str.Replace( "~0","\0" )	'Fix me?
	For Local i:Int=0 Until str.Length
		If str[i]>=32 And str[i]<128 Continue
		Local t$,n:Int=str[i]
		While n
			Local c:Int=(n&15)+48
			If c>=58 c:+97-58
			t=Chr( c )+t
			n=(n Shr 4) & $0fffffff
		Wend
		If Not t t="0"
		If ENV_LANG = "cpp" Then
		'Case "cpp"
			t="~q~q\x"+t+"~q~q"
		Else
			t="\u"+("0000"+t)[-4..]
		End If
		str=str[..i]+t+str[i+1..]
		i:+t.Length-1
	Next
	str="~q"+str+"~q"
	If ENV_LANG="cpp" str="L"+str
	Return str
End Function

Function BmxEnquote$( str$ )
	str=str.Replace( "~~","~~~~" )
	str=str.Replace( "~q","~~q" )
	str=str.Replace( "~n","~~n" )
	str=str.Replace( "~r","~~r" )
	str=str.Replace( "~t","~~t" )
	str=str.Replace( "~0","~~0" )
	str="~q"+str+"~q"
	Return str
End Function

Function BmxUnquote$( str$ )
	str=str[1..str.Length-1]
	str=str.Replace( "~~~~","~~z" )	'a bit dodgy - uses bad esc sequence ~z 
	str=str.Replace( "~~q","~q" )
	str=str.Replace( "~~n","~n" )
	str=str.Replace( "~~r","~r" )
	str=str.Replace( "~~t","~t" )
	str=str.Replace( "~~0","~0" )
	str=str.Replace( "~~z","~~" )
	Return str
End Function

Type TStack Extends TList

	Method Push(obj:Object)
		AddFirst(obj)
	End Method

	Method Length:Int()
		Return count()
	End Method
	
	Method Get:Object(index:Int)
		Return ValueAtIndex(index)
	End Method
	
	Method Pop:Object()
		Return RemoveFirst()
	End Method
	
End Type

Type TStringList Extends TList
	Method Join:String(s:String)
		Local str:String
		For Local t:String = EachIn Self
			If str Then
				str :+ s
			End If
			str:+ t
		Next
		Return str
	End Method
End Type

Function MakeKeywords:String()
	Local keywords:String
	
	keywords :+ "Asc%(v$)=~qbrl_blitz_keywords_asc~q~n"
	keywords :+ "Chr$(v%)=~qbrl_blitz_keywords_chr~q~n"
	keywords :+ "Len%(v$)=~qbrl_blitz_keywords_len~q~n"
	keywords :+ "Min%(v1%,v2%)=~qbrl_blitz_keywords_min~q~n"
	keywords :+ "SizeOf%(v%)=~qbrl_blitz_keywords_sizeof~q~n"
 
	Return keywords
End Function

Function FilePath:String(path:String)
	Local baseDir:String = ExtractDir(path)
	Local bmxDir:String = baseDir + "/.bmx"
	
	If FileType(bmxDir) <> FILETYPE_DIR Then
		Throw "Missing : " + bmxDir
	End If
	
	Return bmxDir
End Function

Function BuildHeaderName:String(path:String)
	path = StripDir(path)

	If opt_buildtype = BUILDTYPE_MODULE Then
		path = opt_modulename + "." + path
	End If
	
	Return path
End Function

Function MungModuleName:String(ident:String)
	Local mung:String = "__bb_" + ident + "_" + ident[ident.Find(".") + 1..]
	Return mung.Replace(".", "_")
End Function

Function MungImportFromFile:String(file:String)
	Return "// TODO : MungImportFromFile()"
End Function

Rem
bbdoc: Get the header file name from a given module ident, optionally with include path.
End Rem
Function ModuleHeaderFromIdent:String(ident:String, includePath:Int = False)
	Local ns:String = ident[..ident.find(".")]
	Local name:String = ident[ident.find(".") + 1..]
	
	Local file:String = name + ".bmx" + FileMung() + ".h"
	
	If includePath Then
		file = ns + ".mod/" + name + ".mod/.bmx/" + file
	End If
	
	Return file
End Function

Function FileHeaderFromFile:String(filepath:String, includePath:Int = False)

	Local name:String = StripAll(filepath)
	Local dir:String = ExtractDir(filePath)

	Local file:String = name + ".bmx" + FileMung(opt_apptype) + ".h"

	If includePath Then
		'Local parent:String = dir[dir.FindLast("/") + 1..]
		'file = parent + "/.bmx/" + file
	End If

	Return file
End Function

Function HeaderFile:String(path:String, mung:String)
	Local fileDir:String = FilePath(path)
	Local file:String = StripDir(path)
	
	Return fileDir + "/" + file + mung + ".h"
End Function

Function OutputFilePath:String(path:String, mung:String, suffix:String, bmxDir:Int = False)
	Local fileDir:String = FilePath(path)
	If bmxDir Then
		fileDir :+ "/.bmx"
	End If
	Local file:String = StripDir(path)
	
	Return fileDir + "/" + file + mung + "." + suffix
End Function

Function FileMung:String(makeApp:Int = False)
	Local m:String = "."
	
	If makeApp Then
		Select opt_apptype
			Case APPTYPE_CONSOLE
				m :+ "console."
			Case APPTYPE_GUI
				m :+ "gui."
		End Select
	End If
	
	If opt_release Then
		m :+ "release"
	Else
		m :+ "debug"
	End If
	
	If opt_threaded Then
		m :+ ".mt"
	End If
	
	m :+ "." + opt_platform
	
	m :+ "." + opt_arch
	
	Return m
End Function

Function HeaderComment:String()
	' TODO
End Function
