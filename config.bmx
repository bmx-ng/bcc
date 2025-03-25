' Copyright (c) 2013-2024 Bruce A Henderson
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
Import BRL.Math

Import "options.bmx"
Import "base.stringhelper.bmx"
Import "base64.bmx"
Import "enums.c"
Import "hash.c"
Import "math.c"
Import "zlib/zlib.bmx"

' debugging help
Const DEBUG:Int = False
Const ABORT_ON_NULL:Int = True
Const PROFILER:Int = False
Const DEBUGSTOP_ON_ERROR:Int = False
Const SHOW_INTERNALERR_LOCATION:Int = True

Global ENV_LANG$

Global _errInfo$
Global _errStack:TList = New TList

' bytes offset to the first field
Global OBJECT_BASE_OFFSET:Int = 8
' 4 bytes on 32-bit, 8 bytes on 64-bit
Global POINTER_SIZE:Int = 4

Global _symbols$[]=[ "..","[]",":*",":/",":+",":-",":|",":&",":~~",":shr",":shl",":sar",":mod"]
Global _symbols_map$[]=[ "..","[]","*=","/=","+=","-=","|=","&=","^=",">>=", "<<=",">>=","%=" ]

Global _fileHasher:TFileHash

Function PushErr( errInfo$ )
	_errStack.AddLast _errInfo
	_errInfo=errInfo
End Function

Function PopErr()
	_errInfo=String(_errStack.RemoveLast())
End Function

Function Err( err$ )
	If DEBUGSTOP_ON_ERROR Then
		DebugStop ' useful for debugging!
	End If
	Throw "Compile Error: "+err + "~n" + _errInfo + "~n"
End Function

Function Warn( err$ )
	'If DEBUGSTOP_ON_ERROR Then
	'	DebugStop ' useful for debugging!
	'End If
	Print "Compile Warning: "+err + "~n" + _errInfo + "~n"
End Function

Function FormatError:String(path:String, line:Int, char:Int)
	Return "[" + path + ";" + line + ";" + char + "]"
End Function

Function InternalErr(errorLocation:String)
	If DEBUGSTOP_ON_ERROR Then
		DebugStop ' useful for debugging!
	End If
	Local locationMsg:String
	If SHOW_INTERNALERR_LOCATION And errorLocation Then locationMsg = " in " + errorLocation
	Throw "Compile Error: Internal Error" + locationMsg + ".~nPlease report the issue, with an example if possible, to https://github.com/bmx-ng/bcc/issues/new~n" + _errInfo + "~n"
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

Function StringToLong:Long(value:String)
	Local Sign:Int = 1
	Local i:Int
	While i < value.length And (value[i] = Asc("+") Or value[i] = Asc("-"))
		If value[i] = Asc("-") Then
			Sign = -1
		End If
		i :+ 1
	Wend
	
	Local n:Long = 0
	While i < value.length
		Local c:Int = value[i]
		If Not IsDigit(c) Exit
		n = n * 10 + (c-Asc("0"))
		i :+ 1
	Wend
	Return n
End Function

Function IsStandardFunc:Int(func:String)
	func = func.ToLower()
	
	Global funcs:String = ";isalnum;isalpha;isascii;isblank;iscntrl;isdigit;isgraph;islower;isprint;ispunct;isspace;isupper;isxdigit;" + ..
		"strlen;_wgetenv;_wputenv;"
	
	Return funcs.Find(func) > 0
End Function

Function mapSymbol:String(sym:String)
	For Local i:Int = 0 Until _symbols.length
		If sym = _symbols[i] Then
			Return _symbols_map[i]
		End If
	Next
	Return sym
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

Function EscapeLines:String(str:String)
	str=str.Replace("~n", "Newline")
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

Function BmxUnquote$( str$, unquoted:Int = False )
	Local length:Int
	Local i:Int
	If Not unquoted Then
		If str.length < 2 Or str[str.length - 1] <> Asc("~q") Then
			Err "Expecting expression but encountered malformed string literal"
		End If
		length = str.length - 1
		i = 1
	Else
		length = str.length
	End If

	Local sb:TStringBuffer = New TStringBuffer

	While i < length
		Local c:Int = str[i]
		i :+ 1
		If c <> Asc("~~") Then
			sb.AppendChar(c)
			Continue
		End If

		If i = length Err "Bad escape sequence in string"
		
		c = str[i]
		i :+ 1
		
		Select c
			Case Asc("~~")
				sb.AppendChar(c)
			Case Asc("0")
				sb.AppendChar(0)
			Case Asc("t")
				sb.AppendChar(Asc("~t"))
			Case Asc("r")
				sb.AppendChar(Asc("~r"))
			Case Asc("n")
				sb.AppendChar(Asc("~n"))
			Case Asc("q")
				sb.AppendChar(Asc("~q"))
			Case Asc("$") ' hex
				c = str[i]
				i :+ 1
				Local n:Int
				While True
					Local v:Int
					If c >= Asc("0") And c <= Asc("9") Then
						v = c-Asc("0")
					Else If c >= Asc("a") And c <= Asc("f") Then
						v = c-Asc("a")+10
					Else If c >= Asc("A") And c <= Asc("F") Then
						v = c-Asc("A")+10
					Else If c <> Asc("~~")
						Err "Bad escape sequence in string"
					Else
						Exit
					End If
					n = (n Shl 4) | (v & $f)
					If i = length Err "Bad escape sequence in string"
					c = str[i]
					i :+ 1
				Wend
				If c <> Asc("~~") Err "Bad escape sequence in string"
				sb.AppendChar(n)
			Case Asc("%") ' bin
				c = str[i]
				i :+ 1
				Local n:Int
				While c = Asc("1") Or c = Asc("0")
					n :Shl 1
					If c = Asc("1") Then
						n :| 1
					End If
					If i = length Err "Bad escape sequence in string"
					c = str[i]
					i :+ 1
				Wend
				If c <> Asc("~~") Err "Bad escape sequence in string"
				sb.AppendChar(n)
			Default
				If c >= Asc("1") And c <= Asc("9") Then
					Local n:Int
					While c >= Asc("0") And c <= Asc("9") 
						n = n * 10 + (c-Asc("0"))
						If i = length Err "Bad escape sequence in string"
						c = str[i]
						i :+ 1
					Wend
					If c <> Asc("~~") Err "Bad escape sequence in string"
					sb.AppendChar(n)
				Else
					Err "Bad escape sequence in string"
				End If
		End Select
	Wend
	Return sb.ToString()
End Function

Function BmxProcessMultiString:String( str:String )
	Local valid:Int
	If str.length < 7 Then
		Err "Expecting expression but encountered malformed multiline string literal"
	End If
	
	For Local i:Int = 0 Until 3
		If str[i] <> Asc("~q") Or str[str.length -1 -i] <> Asc("~q") Then
			Err "Expecting expression but encountered malformed multiline string literal"
		End If
	Next
	
	str = str[3..str.length - 3]
	' normalise line endings
	str = str.Replace("~r~n", "~n").Replace("~r", "~n")

	If str[0] <> Asc("~n") Then
		Err "Expecting EOL but encountered malformed multiline string literal"
	End If

	str = str[1..]

	Local LINES:String[] = str.Split("~n")

	Local lineCount:Int = LINES.length - 1
	Local last:String = LINES[lineCount]
	
	Local i:Int = last.length - 1
	While i >= 0
		If last[i] <> Asc(" ") And last[i] <> Asc("~t") Then
			Err "Expecting trailing whitespace"
		End If
		i :- 1
	Wend
	
	Local trailingIndent:String = last
	
	' strip indent
	If trailingIndent Then
		For i = 0 Until lineCount
			Local line:String = LINES[i]
			If line.StartsWith(trailingIndent) Then
				line = line[trailingIndent.length..]
				LINES[i] = line
			End If
		Next
	End If

	' right trim
	For i = 0 Until lineCount
		Local line:String = LINES[i]
		Local index:Int = line.length
		While index
			index :- 1
			If line[index] <> Asc(" ") And line[index] <> Asc("~t") Then
				Exit
			End If
		Wend
		If index < line.length - 1 Then
			line = line[..index + 1]
			LINES[i] = line
		End If
	Next

	Local sb:TStringBuffer = New TStringBuffer
	For i = 0 Until lineCount
		Local line:String = LINES[i]
		Local length:Int = line.length
		Local softWrap:Int
		If line And line[line.length-1] = Asc("\") Then
			softWrap = True
			length :- 1
		End If
		If line Then
			sb.Append(line[..length])
		End If
		If Not softWrap And i < lineCount - 1 Then
			sb.Append("~n")
		End If
	Next

	Return BmxUnquote(sb.ToString(), True)
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
	
	Field valuesList:TList = New TList

	Method Insert( key:Object,value:Object )
		list.AddLAst(New TKeyValue.Create(key, value))
		valuesList.AddLast(value)
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
		'Local vlist:TList = New TList
		'For Local kv:TKeyValue = EachIn list
		'	vlist.AddLast(kv.value)
		'Next
		Return valuesList
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
	keywords :+ "Chr$(v%)=~qbrl_blitz_keywords_chr~q~n"
	keywords :+ "Len%(v:Object)=~qbrl_blitz_keywords_len~q~n"
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
	
	Return TStringHelper.Sanitize(path, , True)
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
	
	If opt_coverage Then
		m :+ ".cov"
	End If

'	If opt_threaded Then
'		m :+ ".mt"
'	End If
	
	m :+ "." + opt_platform
	
	m :+ "." + opt_arch
	
	Return m
End Function

Function HeaderComment:String()
	' TODO
End Function

Global fileRegister:TMap = New TMap

Function GenHash:String(file:String)
	Local Hash:String = bmx_gen_hash(file)

	If Not fileRegister.Contains(Hash) Then
		fileRegister.Insert(Hash, file)
	End If
	
	Return Hash
End Function

Type TTemplateRecord

	Field start:Int
	Field file:String
	Field source:String
	
	Method Create:TTemplateRecord(start:Int, file:String, source:String)
		Self.start = start
		Self.file = file
		Self.source = source
		Return Self
	End Method
	
	Method ToString:String()

		Local s:Byte Ptr = source.ToUTF8String()
?Not bmxng
		Local slen:Int = strlen_(s)
?bmxng
		Local slen:ULongInt = strlen_(s)
?

?Not bmxng		
		Local dlen:Int = slen + 12
?bmxng
		Local dlen:ULongInt = slen + 12
?
		Local data:Byte[dlen]
		
		compress2(data, dlen, s, slen, 9)
		
		MemFree(s)
		
		Local t:String = "{" + start +","+ slen +","+ LangEnquote(file) + ","
		
		t :+ LangEnquote(TBase64.Encode(data, Int(dlen), 0, TBase64.DONT_BREAK_LINES))

		Return t + "}"

	End Method
	
	Function Load:TTemplateRecord(start:Int, file:String, size:Int, source:String)
		
?Not bmxng		
		Local dlen:Int = size + 1
?bmxng
		Local dlen:ULongInt = size + 1
?
		Local data:Byte[dlen]
		
		Local s:Byte[] = TBase64.Decode(source)
?Not bmxng
		uncompress(data, dlen, s, s.length)
?bmxng
		uncompress(data, dlen, s, ULongInt(s.length))
?	
		Return New TTemplateRecord.Create(start, file, String.FromUTF8String(data))
	End Function
End Type

Type TCallback
	Method Callback(obj:Object) Abstract
End Type

Type TFileHash

	Field statePtr:Byte Ptr
	
	Method Create:TFileHash()
		statePtr = bmx_hash_createState()
		Return Self
	End Method
	
	Method CalculateHash:String(stream:TStream)
		Const BUFFER_SIZE:Int = 8192
	
	
		bmx_hash_reset(statePtr)
		
		Local data:Byte[BUFFER_SIZE]
		
		While True
			Local read:Int = stream.Read(data, BUFFER_SIZE)

			bmx_hash_update(statePtr, data, read)
			
			If read < BUFFER_SIZE Then
				Exit
			End If

		Wend
		
		Return bmx_hash_digest(statePtr)
		
	End Method

End Type

Function CalculateFileHash:String(path:String)
	If Not _fileHasher Then
		_fileHasher = New TFileHash.Create()
	End If

	If FileType(path) = FILETYPE_FILE Then
		Local stream:TStream = ReadStream(path)
		Local fileHash:String = _fileHasher.CalculateHash(stream)
		stream.Close()
		
		Return fileHash
	End If
	
	Return Null
End Function

?Not bmxng
Const OP_MUL:Int = 0
Const OP_DIV:Int = 1
Const OP_MOD:Int = 2
Const OP_SHL:Int = 3
Const OP_SHR:Int = 4
Const OP_SAR:Int = 5
Const OP_ADD:Int = 6
Const OP_SUB:Int = 7
Const OP_AND:Int = 8
Const OP_XOR:Int = 9
Const OP_OR:Int = 10

Function OpToInt:Int(op:String)
	Select op
		Case "*" Return OP_MUL
		Case "/" Return OP_DIV
		Case "mod" Return OP_MOD
		Case "shl" Return OP_SHL
		Case "shr" Return OP_SHR
		Case "sar" Return OP_SAR
		Case "+" Return OP_ADD
		Case "-" Return OP_SUB
		Case "&" Return OP_AND
		Case "~~" Return OP_XOR
		Case "|" Return OP_OR
	End Select
	InternalErr "TBinaryMathExpr.Eval.OpToInt : " + op
End Function
?

Extern
	Function strlen_:Int(s:Byte Ptr)="strlen"
	Function bmx_enum_next_power(char:Int, val:Long Var, ret:Long Var)
	Function bmx_gen_hash:String(txt:String)
	Function bmx_hash_createState:Byte Ptr()
	Function bmx_hash_reset(state:Byte Ptr)
	Function bmx_hash_update(state:Byte Ptr, data:Byte Ptr, length:Int)
	Function bmx_hash_digest:String(state:Byte Ptr)

	Function bmx_bitwise_not_uint:String(value:String)
	Function bmx_bitwise_not_sizet:String(value:String)
	Function bmx_bitwise_not_ulong:String(value:String)
	Function bmx_bitwise_not_longint:String(value:String, size:Int)
	Function bmx_bitwise_not_ulongint:String(value:String, size:Int)
	Function bmx_binarymathexpr_sizet:String(op:Int, lhs:String, rhs:String)
	Function bmx_binarymathexpr_uint:String(op:Int, lhs:String, rhs:String)
	Function bmx_binarymathexpr_ulong:String(op:Int, lhs:String, rhs:String)
	Function bmx_binarymathexpr_longint:String(op:Int, lhs:String, rhs:String, size:Int)
	Function bmx_binarymathexpr_ulongint:String(op:Int, lhs:String, rhs:String, size:Int)

End Extern
