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

Import BRL.LinkedList
Import BRL.Map
Import BRL.FileSystem

Import "options.bmx"
Import "base.stringhelper.bmx"


' debugging help
Const DEBUG:Int = False
Const ABORT_ON_NULL:Int = True
Const PROFILER:Int = False

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
	'WriteStderr "Compile Error: "+err + "~n"
	'WriteStderr _errInfo + "~n"
'	Print _errInfo+" "+err
DebugStop ' useful for debugging!
	Throw "Compile Error: "+err + "~n" + _errInfo + "~n"
End Function

Function FormatError:String(path:String, line:Int, char:Int)
	Return "[" + path + ";" + line + ";" + char + "]"
End Function

Function InternalErr()
	Print _errInfo+" : Error : Internal error."
	DebugStop
	'Error _errInfo+" : Error : Internal error."
End Function

Function IsSpace:Int( ch:Int )
	Return ch<=Asc(" ") Or ch=$A0 ' NO-BREAK SPACE (U+00A0)
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

Function IsStandardFunc:Int(func:String)
	func = func.ToLower()
	
	Global funcs:String = ";isalnum;isalpha;isascii;isblank;iscntrl;isdigit;isgraph;islower;isprint;ispunct;isspace;isupper;isxdigit;" + ..
		"strlen;"
	
	Return funcs.Find(func) > 0
End Function

'enquote depending on ENV_LANG
'
Function LangEnquote$( str$ )
	str=EscapeString(str)
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

Function EscapeString$(str$)
	str=str.Replace( "\","\\" )
	str=str.Replace( "~q","\~q" )
	str=str.Replace( "~n","\n" )
	str=str.Replace( "~r","\r" )
	str=str.Replace( "~t","\t" )
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
		Local arr:String[] = New String[count()]
		Local index:Int
		For Local t:String = EachIn Self
			arr[index] = t
			index :+ 1
		Next
		
		Return s.Join(arr)
	End Method
End Type

Type TKeyValue
	Field key:Object
	Field value:Object
	
	Method Create:TKeyValue(key:Object,value:Object)
		Self.key = key
		Self.value = value
		Return Self
	End Method
	
	Method Compare:Int(other:Object)
		If Not TKeyValue(other) Return 0
		Return key.Compare(TKeyValue(other).key)
	End Method
	
End Type

Type TUnorderedMap

	Field list:TList = New TList
	Field map:TMap = New TMap

	Method Insert( key:Object,value:Object )
		list.AddLAst(New TKeyValue.Create(key, value))
		map.Insert(key, value)
	End Method
	
	Method Keys:TList()
		Local klist:TList = New TList
		For Local kv:TKeyValue = EachIn list
			klist.AddLast(kv.key)
		Next
		Return klist
	End Method
	
	Method Values:TList()
		Local vlist:TList = New TList
		For Local kv:TKeyValue = EachIn list
			vlist.AddLast(kv.value)
		Next
		Return vlist
	End Method
	
	Method Contains:Int( key:Object )
		Return map.Contains(key)
	End Method
	
	Method ValueForKey:Object( key:Object )
		Return map.ValueForKey(key)
	End Method
End Type

Function MakeKeywords:String()
	Local keywords:String
	
	keywords :+ "import brl.classes~n"
	keywords :+ "Asc%(v$)=~qbrl_blitz_keywords_asc~q~n"
	keywords :+ "Sgn#(v#)=~qbrl_blitz_keywords_sgn~q~n"
	keywords :+ "Chr$(v%)=~qbrl_blitz_keywords_chr~q~n"
	keywords :+ "Len%(v:Object)=~qbrl_blitz_keywords_len~q~n"
	keywords :+ "Min%(v1%,v2%)=~qbrl_blitz_keywords_min~q~n"
	keywords :+ "Max%(v1%,v2%)=~qbrl_blitz_keywords_max~q~n"
	'keywords :+ "SizeOf%(v%)=~qbrl_blitz_keywords_sizeof~q~n"
	'keywords :+ "Incbin(v$)=~qbrl_blitz_keywords_incbin~q~n"
	keywords :+ "IncbinPtr@*(v$)=~qbbIncbinPtr~q~n"
	keywords :+ "IncbinLen%(v$)=~qbbIncbinLen~q~n"
 
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
	If opt_buildtype = BUILDTYPE_MODULE Then
		path = opt_modulename + "_" + StripDir(path)
	Else
		Local dir:String = ExtractDir(path).ToLower().Replace("/.bmx","")
		dir = dir[dir.findLast("/") + 1..]
		If dir.EndsWith(".mod") Then
			dir = dir.Replace(".mod", "")
		End If
		Local file:String = StripDir(path).ToLower()
		path = dir + "_" + file
	End If
	
	Return TStringHelper.Sanitize(path)
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
