' Copyright (c) 2014-2016 Ronny Otto
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

Import "transform.c"

Extern
	Function transform:Byte Ptr(i:Int)
End Extern

'Print TStringHelper.Sanitize("äääßßwa dhhh :D")
'Print TStringHelper.Sanitize("c:\MyPath\contains spaces\yeah")
'Print TStringHelper.Sanitize("has.dots.and-some-minuses", "_", ".-")

Type TStringHelper
	'this function replaces non-alphanumerical characters with
	'the string "replaceInvalidCharsWith"
	'certain characters (German umlauts, French accents) are replaced
	'with their basic characters (é = e)
	Function Sanitize:String(value:String, replaceInvalidCharsWith:String="_")
		Local result:String = ""
		Local char:String = ""

		For Local i:Int = 0 Until value.length
			char = ""

			Local c:Byte Ptr = transform(value[i])

			If c Then
				char = String.FromCString( c )
			End If

			'append the char - or the replacement
			If char <> ""
				result :+ char
			Else
				result :+ replaceInvalidCharsWith
			EndIf
		Next

		Return result
	End Function
End Type
