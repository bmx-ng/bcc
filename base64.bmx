' Copyright (c) 2008-2019 Bruce A Henderson
' 
' Permission is hereby granted, free of charge, to any person obtaining a copy
' of this software and associated documentation files (the "Software"), to deal
' in the Software without restriction, including without limitation the rights
' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
' copies of the Software, and to permit persons to whom the Software is
' furnished to do so, subject to the following conditions:
' 
' The above copyright notice and this permission notice shall be included in
' all copies or substantial portions of the Software.
' 
' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
' THE SOFTWARE.
' 
SuperStrict

Rem
bbdoc: Encode/Decode Base64 data.
about:
To Encode :
<pre>
SuperStrict
Import BaH.Base64

Local someData:String = "Woo! BlitzMax Modules rock!"
Local encoded:String = TBase64.Encode(someData, someData.length)
Print "Encoded : " + encoded
</pre>
To Decode :
<pre>
SuperStrict
Import BaH.Base64

Local encodedData:String = "V29vISBCbGl0ek1heCBNb2R1bGVzIHJvY2sh"
Local data:Byte[] = TBase64.Decode(encodedData)
Local decoded:String = String.FromBytes(data, data.length)
Print "Decoded : " + decoded
</pre>
End Rem
Type TBase64

	Const DONT_BREAK_LINES:Int = 8
	
	' Maximum line length (76) of Base64 output.
	Const MAX_LINE_LENGTH:Int = 76
	
	' The equals sign (=) as int.
	Const EQUALS_SIGN:Int = Asc("=")
	
	' The new line character (\n) as int.
	Const NEW_LINE:Int = Asc("~n")
	
	Const WHITE_SPACE_ENC:Int = -5 ' Indicates white space in encoding
	Const EQUALS_SIGN_ENC:Int = -1 ' Indicates equals sign in encoding
	
	Const _STANDARD_ALPHABET:String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
	
	Global _STANDARD_DECODABET:Int[] = [-9,-9,-9,-9,-9,-9,-9,-9,-9, ..
						        -5,-5, ..
						        -9,-9, ..
						        -5, ..
						        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9, ..
						        -9,-9,-9,-9,-9, ..
						        -5, ..
						        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9, ..
						        62, ..
						        -9,-9,-9, ..
						        63, ..
						        52,53,54,55,56,57,58,59,60,61, ..
						        -9,-9,-9, ..
						        -1, ..
						        -9,-9,-9, ..
						        0,1,2,3,4,5,6,7,8,9,10,11,12,13, ..
						        14,15,16,17,18,19,20,21,22,23,24,25, ..    
						        -9,-9,-9,-9,-9,-9, ..
						        26,27,28,29,30,31,32,33,34,35,36,37,38, ..
						        39,40,41,42,43,44,45,46,47,48,49,50,51, ..
						        -9,-9,-9,-9]
	
	Rem
	bbdoc: Encode byte data to a Base64 encoded String, starting at @offset of @length bytes.
	End Rem
	Function Encode:String( source:Byte Ptr, length:Int, offset:Int = 0, options:Int = 0)
    
		' Isolate options
		Local dontBreakLines:Int =  options & DONT_BREAK_LINES 
		
		' Convert option To boolean in way that code likes it.
		Local breakLines:Int = True
		If dontBreakLines Then
			breakLines = False
		End If
		
		Local len43:Int = length * 4 / 3
		Local nl:Int = len43 / MAX_LINE_LENGTH
		Local pad:Int = 0
		If length Mod 3 > 0 Then
			pad = 4
		End If
		
		If Not breakLines Then
			nl = 0
		End If
		
		Local outBuff:Byte[] = New Byte[ len43 + pad + nl ]     
		Local d:Int = 0
		Local e:Int = 0
		Local len2:Int = length - 2
		Local lineLength:Int = 0
		While d < len2
			encode3to4( source, d + offset, 3, outBuff, e, options )
			
			lineLength :+ 4
			If  breakLines And (lineLength = MAX_LINE_LENGTH) Then
				outBuff[e+4] = NEW_LINE
				e:+1
				lineLength = 0
			End If
			d:+3
			e:+4
		Wend
		
		' pad?
		If d < Length Then
			encode3to4( source, d + offset, Length - d, outBuff, e, options )
			e :+ 4
		End If

		Return String.FromBytes(outBuff, e)
	End Function
	
	Rem
	bbdoc: Decode Base64 encoded String to an array of Bytes, starting at @offset.
	End Rem
	Function Decode:Byte[]( source:String, offset:Int = 0, options:Int = 0 )
		
		Local length:Int = source.length
		Local len34:Int   = Length * 3 / 4
		Local outBuff:Byte[] = New Byte[ len34 ]
		Local outBuffPosn:Int = 0
		
		Local b4:Byte[]     = New Byte[4]
		Local b4Posn:Int    = 0
		Local sbiCrop:Int   = 0
		Local sbiDecode:Int = 0
		For Local i:Int = offset Until offset + length
		
			sbiCrop = source[i] & $7f
			sbiDecode = _STANDARD_DECODABET[ sbiCrop ]
			
			If sbiDecode >= WHITE_SPACE_ENC Then
			
				If sbiDecode >= EQUALS_SIGN_ENC Then
				
					b4[ b4Posn ] = sbiCrop
					b4Posn:+1
					If b4Posn > 3 Then
				
						outBuffPosn :+ decode4to3( b4, 0, outBuff, outBuffPosn, options )
						b4Posn = 0
				
						' If that was the equals sign, break out of 'for' loop
						If sbiCrop = EQUALS_SIGN Then
							Exit
						End If
					End If
			
				End If
			
			Else
				DebugLog "Bad Base64 input character at " + i + ": " + source[i]
				Return Null
			End If
		Next
		
		Return outBuff[0..outBuffPosn]
	End Function

	Function encode3to4:Byte[](source:Byte Ptr, srcOffset:Int, numSigBytes:Int, destination:Byte[], destOffset:Int, options:Int )
    
		Local inBuff:Int
		If numSigBytes > 0 Then
			inBuff = (source[ srcOffset     ] Shl 24) Shr 8
			
			If numSigBytes > 1 Then
				inBuff :| (source[ srcOffset + 1 ] Shl 24) Shr 16

				If numSigBytes > 2 Then
					inBuff :| (source[ srcOffset + 2 ] Shl 24) Shr 24
				End If
			End If
		End If
		
		Select numSigBytes
			Case 3
				destination[ destOffset     ] = _STANDARD_ALPHABET[ (inBuff Shr 18)       ]
				destination[ destOffset + 1 ] = _STANDARD_ALPHABET[ (inBuff Shr 12) & $3f ]
				destination[ destOffset + 2 ] = _STANDARD_ALPHABET[ (inBuff Shr  6) & $3f ]
				destination[ destOffset + 3 ] = _STANDARD_ALPHABET[ (inBuff       ) & $3f ]
				Return destination
			
			Case 2
				destination[ destOffset     ] = _STANDARD_ALPHABET[ (inBuff Shr 18)       ]
				destination[ destOffset + 1 ] = _STANDARD_ALPHABET[ (inBuff Shr 12) & $3f ]
				destination[ destOffset + 2 ] = _STANDARD_ALPHABET[ (inBuff Shr  6) & $3f ]
				destination[ destOffset + 3 ] = EQUALS_SIGN
				Return destination
			
			Case 1
				destination[ destOffset     ] = _STANDARD_ALPHABET[ (inBuff Shr 18)       ]
				destination[ destOffset + 1 ] = _STANDARD_ALPHABET[ (inBuff Shr 12) & $3f ]
				destination[ destOffset + 2 ] = EQUALS_SIGN
				destination[ destOffset + 3 ] = EQUALS_SIGN
				Return destination
			
			Default
				Return destination
		End Select
	End Function 
	
	Function decode4to3:Int( source:Byte[] , srcOffset:Int, destination:Byte[] , destOffset:Int, options:Int )
	
		' Example: Dk==
		If source[ srcOffset + 2] = EQUALS_SIGN Then
		
			Local outBuff:Int =   ( ( _STANDARD_DECODABET[ source[ srcOffset    ] ] & $FF ) Shl 18 ) ..
				| ( ( _STANDARD_DECODABET[ source[ srcOffset + 1] ] & $FF ) Shl 12 )
			
			destination[ destOffset ] = outBuff Shr 16
			Return 1
		
		' Example: DkL=
		Else If source[ srcOffset + 3 ] = EQUALS_SIGN Then
		
			Local outBuff:Int =   ( ( _STANDARD_DECODABET[ source[ srcOffset     ] ] & $FF ) Shl 18 ) ..
				| ( ( _STANDARD_DECODABET[ source[ srcOffset + 1 ] ] & $FF ) Shl 12 ) ..
				| ( ( _STANDARD_DECODABET[ source[ srcOffset + 2 ] ] & $FF ) Shl  6 )
			
			destination[ destOffset     ] = outBuff Shr 16
			destination[ destOffset + 1 ] = outBuff Shr  8
			Return 2
			
		' Example: DkLE
		Else
		
			Local outBuff:Int =   ( ( _STANDARD_DECODABET[ source[ srcOffset     ] ] & $FF ) Shl 18 ) ..
				| ( ( _STANDARD_DECODABET[ source[ srcOffset + 1 ] ] & $FF ) Shl 12 ) ..
				| ( ( _STANDARD_DECODABET[ source[ srcOffset + 2 ] ] & $FF ) Shl  6) ..
				| ( ( _STANDARD_DECODABET[ source[ srcOffset + 3 ] ] & $FF )      )
		
			destination[ destOffset     ] =  outBuff Shr 16
			destination[ destOffset + 1 ] =  outBuff Shr  8
			destination[ destOffset + 2 ] =  outBuff
		
			Return 3
		End If
	End Function 

End Type

