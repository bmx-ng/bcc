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
	"short;int;float;double;long;string;array;object;ptr;var;varptr;mod;continue;exit;"+ ..
	"include;import;module;extern;framework;"+ ..
	"new;self;super;eachin;true;false;null;not;"+ ..
	"extends;abstract;select;case;default;"+ ..
	"const;local;global;field;method;function;type;"+ ..
	"and;or;shl;shr;end;if;then;else;elseif;endif;while;wend;repeat;until;forever;"+ ..
	"for;to;step;next;return;"+ ..
	"alias;rem;endrem;throw;assert;try;catch;nodebug;incbin;"

	Global _symbols$[]=[ "..","[]",":*",":/",":+",":-",":|",":&",":~~",":shr",":shl" ]
	Global _symbols_map$[]=[ "..","[]","*=","/=","+=","-=","|=","&=","~~=","=>>", "=<<" ]
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
		Else
			_tokeType=TOKE_SYMBOL
			For Local i:Int = 0 Until _symbols.length
'If i = 9 And _line = 42 DebugStop
				Local sym$=_symbols[i]
				If char<>sym[0] Continue
'Local a:String = _source[_tokePos-1.._tokePos+sym.length-1]
				If _source[_tokePos-1.._tokePos+sym.length-1].ToLower()=sym
					'_toke = _symbols_map[i]
					
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
	
	Method SkipSpace()
		While _tokeType=TOKE_SPACE
			NextToke
		Wend
	End Method
	
'Private

	Method TCHR:Int( i:Int=0 )
		If _tokePos+i<_source.Length Return _source[_tokePos+i]
	End Method
	
	Method TSTR$( i:Int=0 )
		If _tokePos+i<_source.Length Return Chr( _source[_tokePos+i] )
	End Method
	
	
End Type
