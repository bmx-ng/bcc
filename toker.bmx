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

Import "config.bmx"
Import "type.bmx"

'toke_type
Const TOKE_EOF:Int=0
Const TOKE_SPACE:Int=1
Const TOKE_IDENT:Int=2
Const TOKE_KEYWORD:Int=3
Const TOKE_INTLIT:Int=4
Const TOKE_FLOATLIT:Int=5
Const TOKE_STRINGLIT:Int=6
Const TOKE_STRINGLITEX:Int=7
Const TOKE_SYMBOL:Int=8
Const TOKE_LINECOMMENT:Int=9
Const TOKE_LONGLIT:Int=10

'***** Tokenizer *****
Type TToker

	Const _keywords$=";"+ ..
	"strict;superstrict;"+ ..
	"public;private;"+ ..
	"short;int;float;double;long;string;object;ptr;var;varptr;mod;continue;exit;"+ ..
	"include;import;module;extern;framework;"+ ..
	"new;self;super;eachin;true;false;null;not;"+ ..
	"extends;abstract;select;case;default;"+ ..
	"const;local;global;field;method;function;type;"+ ..
	"and;or;shl;shr;sar;end;if;then;else;elseif;endif;while;wend;repeat;until;forever;"+ ..
	"for;to;step;next;return;"+ ..
	"alias;rem;endrem;throw;assert;try;catch;nodebug;incbin;"+ ..
	"endselect;endmethod;endfunction;endtype;endextern;endtry;pi;release;defdata;readdata;restoredata;"

	Global _symbols$[]=[ "..","[]",":*",":/",":+",":-",":|",":&",":~~",":shr",":shl",":sar",":mod" ]
	Global _symbols_map$[]=[ "..","[]","*=","/=","+=","-=","|=","&=","^=",">>=", "<<=",">>=","%=" ]
	'Global _symbols$[]=[ "..","[]",":=",":*",":/",":+",":-",":|",":&",":~~" ]
	'Global _symbols_map$[]=[ "..","[]",":=","*=","/=","+=","-=","|=","&=","~~=" ]

	Field _path$
	Field _line:Int
	Field _lines:String[]
	Field _source$
	Field _toke$
	Field _tokeType:Int
	Field _tokePos:Int
	
	Method Create:TToker( path$,source$ )
		_path=path
		_line=1
		_source=source
		_toke=""
		_tokeType=TOKE_EOF
		_tokePos=0
		_lines = source.split("~n")
		Return Self
	End Method
	
	Method rollback(pos:Int, toketype:Int = -1)
		_tokePos = pos
		If toketype >= 0
			_tokeType = toketype
		End If
	End Method
	
	Method Copy:TToker( toker:TToker )
		_path=toker._path
		_line=toker._line
		_source=toker._source
		_toke=toker._toke
		_tokeType=toker._tokeType
		_tokePos=toker._tokePos
		_lines=toker._lines
		Return Self
	End Method
	
	Method Path$()
		Return _path
	End Method
	
	Method Line:Int()
		Return _line
	End Method
	
	Method NextToke$()

		If _tokePos=_source.Length
			_toke=""
			_tokeType=TOKE_EOF
			Return _toke
		EndIf
				
		Local char:Int=_source[_tokePos]
		Local str$=Chr( char )
		
		Local start:Int=_tokePos
		_tokePos:+1
		_toke=""

		If str="~n"
			_line:+1
			_tokeType=TOKE_SYMBOL
		Else If IsSpace( char )
			While _tokePos<_source.Length And IsSpace( TCHR() ) And TSTR()<>"~n"
				_tokePos:+1
			Wend
			_tokeType=TOKE_SPACE
		Else If str="_" Or IsAlpha( char )
			_tokeType=TOKE_IDENT
			While _tokePos<_source.Length
				Local char:Int=_source[_tokePos]
				If char<>Asc("_") And Not IsAlpha( char ) And Not IsDigit( char ) Exit
				_tokePos:+1
			Wend
			_toke=_source[start.._tokePos]
			If _keywords.Contains( ";"+_toke.ToLower()+";" )
				_tokeType=TOKE_KEYWORD
			EndIf
		Else If IsDigit( char ) Or (str="." And IsDigit( TCHR() ))
'Print "IS DIGIT..."
			_tokeType=TOKE_INTLIT
			If str="." _tokeType=TOKE_FLOATLIT
			While IsDigit( TCHR() )
				_tokePos:+1
			Wend
			If _tokeType=TOKE_INTLIT And TSTR()="." And IsDigit( TCHR(1) )
				_tokeType=TOKE_FLOATLIT
				_tokePos:+2
				While IsDigit( TCHR() )
					_tokePos:+1
				Wend
			EndIf
			If TSTR().ToLower()="e"
				_tokeType=TOKE_FLOATLIT
				_tokePos:+1
				If TSTR()="+" Or TSTR()="-" _tokePos:+1
				While IsDigit( TCHR() )
					_tokePos:+1
				Wend
			EndIf
			If _tokeType=TOKE_INTLIT And _tokePos-start > 10 ' BaH Long
				_tokeType=TOKE_LONGLIT
			EndIf
		Else If str="%" And IsBinDigit( TCHR() )
			_tokeType=TOKE_INTLIT
			_tokePos:+1
			While IsBinDigit( TCHR() )
				_tokePos:+1
			Wend
		Else If str="$" And IsHexDigit( TCHR() )
			_tokeType=TOKE_INTLIT
			_tokePos:+1
			While IsHexDigit( TCHR() )
				_tokePos:+1
			Wend
		Else If str="~q"
			_tokeType=TOKE_STRINGLIT
			While TSTR() And TSTR()<>"~q"
				' Strings can't cross line boundries
				If TSTR()="~n" Then
					_tokePos:-1
					Exit
				End If
				_tokePos:+1
			Wend
			If _tokePos<_source.Length _tokePos:+1 Else _tokeType=TOKE_STRINGLITEX
		Else If str="'"
			_tokeType=TOKE_LINECOMMENT
			While TSTR() And TSTR()<>"~n"
				_tokePos:+1
			Wend
			If _tokePos<_source.Length
				_tokePos:+1
				_line:+1
			EndIf
		Else If str="." And TSTR()="." Then

			Local pos:Int = _tokePos
			Local isValidTilEOL:Int = True
			_tokePos:+1
			While TSTR() And TSTR()<>"~n"
				If Not IsSpace(TCHR()) Then
					isValidTilEOL = False
				End If
				_tokePos:+1
			Wend
			
			If Not isValidTilEOL Then
				_tokePos = pos + 1
				_tokeType=TOKE_SYMBOL
			Else
				_tokePos:+1
				_line:+1
				_tokeType=TOKE_SPACE
			End If
			
		Else

			_tokeType=TOKE_SYMBOL
			For Local i:Int = 0 Until _symbols.length

				Local sym$=_symbols[i]
				If char<>sym[0] Continue

				'do not try to read beyond source length
				If TCHR(sym.length) <= 0 Then Continue

				If _source[_tokePos-1.._tokePos+sym.length-1].ToLower()=sym

					' if symbol has alpha, test for trailing alphanumeric
					' char - in which case this is not a symbol
					If IsAlpha(sym[sym.length-1]) Then

						'found the sym-string-representation in the code...
						'but alphanumeric-symbols need a space or ".."-
						'line-concat to work, skip other matches.
						'Without that checks "Method My:ModObject" would
						'tokenize ":mod"
						'do not just check for "alpha" or "digit", as a
						'newline is not possible after "shortcut"
						'so "a :mod~n" is invalid

						'compare following char according our rules
						Local followUpChar:String = Chr(_source[_tokePos + sym.length - 1])
						Local isSym:int = False

						'concat via "double dot"?
						'("." is still existing when using raw source
						' access ... so we cannot skip this check)
						If followUpChar = "."
							'enough space left for double-dot-check
							If _source.Length >= _tokePos + sym.length + 1
								followUpChar = Chr(_source[_tokePos + sym.length])
								If followUpChar = "." Then isSym = True
							EndIf
						EndIf

						'space?
						If IsSpace(Asc(followUpChar)) Then isSym = True
						'valid symbol, but invalid useage
						If followUpChar = "~n" Then isSym = True

						If not isSym then continue 
					End If

					_tokePos:+sym.length-1
					
					Exit
				EndIf
			Next
		EndIf

		If Not _toke _toke=_source[start.._tokePos]
				
'Print "TOKE... : " + _toke		
		Return _toke
	End Method
	
	Method Toke$()
		Return _toke
	End Method
	
	Method TokeType:Int()
		Return _tokeType
	End Method
	
	Method SkipSpace:Int()
		Local count:Int
		While _tokeType=TOKE_SPACE
			NextToke
			count :+ 1
		Wend
		Return count
	End Method
	
'Private

	Method TCHR:Int( i:Int=0 )
		If _tokePos+i<_source.Length Return _source[_tokePos+i]
	End Method
	
	Method TSTR$( i:Int=0 )
		If _tokePos+i<_source.Length Return Chr( _source[_tokePos+i] )
	End Method
	
	
End Type
