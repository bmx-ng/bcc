' Copyright (c) 2014 Ronny Otto
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
Import Brl.Retro


'print TStringHelper.Sanitize("äääßßwa dhhh :D")
'print TStringHelper.Sanitize("c:\MyPath\contains spaces\yeah")
'print TStringHelper.Sanitize("has.dots.and-some-minuses", "_", ".-")

Type TStringHelper
	'this function replaces non-alphanumerical characters with
	'the string "replaceInvalidCharsWith"
	'certain characters (German umlauts, French accents) are replaced
	'with their basic characters (é = e)
	Function Sanitize:string(value:string, replaceInvalidCharsWith:string="_", allowedNonAlphanumericCharacters:string="")
		Local result:string = ""
		Local char:string = ""

		For Local i:int = 0 until Len(value)
			char = ""

			'numbers
			If value[i] > 47 and value[i] < 58 then char = chr(value[i])
			'upper alpha
			If value[i] > 64 and value[i] < 91 then char = chr(value[i])
			'lower alpha
			If value[i] > 96 and value[i] < 123 then char = chr(value[i])

			'check special chars
			If char="" and allowedNonAlphanumericCharacters.Find(chr(value[i])) > -1
				char = chr(value[i])
			Endif

			'check for German umlauts or French accents
			if char=""
				'replace value[i] "äöü" or "ÄÖÜ" with similar ones
				if value[i] = 228 then char = "a"	'ä to a
				if value[i] = 246 then char = "o"	'ö to o
				if value[i] = 252 then char = "u"	'ü to u
				if value[i] = 196 then char = "A"	'Ä to A
				if value[i] = 214 then char = "O"	'Ö to O
				if value[i] = 220 then char = "U"	'Ü to U
				if value[i] = 223 then char = "ss"	'ß to ss

				'é è ê to e + upper
				if value[i] = 232 or value[i]=233 or value[i]=234 then char = "e"
				if value[i] = 200 or value[i]=201 or value[i]=202 then char = "E"
				'á à â to a + upper
				if value[i] = 224 or value[i]=225 or value[i]=226 then char = "a"
				if value[i] = 192 or value[i]=193 or value[i]=194 then char = "A"
				'ó ò ô to o + upper
				if value[i] = 242 or value[i]=243 or value[i]=244 then char = "o"
				if value[i] = 210 or value[i]=211 or value[i]=212 then char = "O"
			endif

			'append the char - or the replacement
			If char <> ""
				result :+ char
			Else
				result :+ replaceInvalidCharsWith
			Endif
		Next

		return result
	End Function
End Type