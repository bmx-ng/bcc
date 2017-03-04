' Copyright (c) 2014-2017 Bruce A Henderson
' Copyright (c) 2014-2017 Ronny Otto
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

Import "stringbuffer_core.bmx"

Extern
	Function transform:Byte Ptr(i:Int)
End Extern

Type TStringHelper
	'this function replaces non-alphanumerical characters with
	'the string "replaceInvalidCharsWith"
	'certain characters (German umlauts, French accents) are replaced
	'with their basic characters (é = e)

	Function Sanitize:String(value:String, replaceInvalidCharsWith:String="_", requiresAlphaPrefix:Int = False)
		Local result:TStringBuffer = New TStringBuffer

		For Local i:Int = 0 Until value.length

			Local c:Byte Ptr = transform(value[i])

			'append the char - or the replacement
			If c
				If Not i Then
					Local n:Int = c[0]
					If n >= Asc("0") And n <= Asc("9") Then
						result.Append(replaceInvalidCharsWith)
					End If
				End If
				result.AppendCString(c)
			Else
				result.Append(replaceInvalidCharsWith)
			EndIf
		Next

		Return result.ToString()
	End Function

End Type
