SuperStrict

Framework BRL.StandardIO
Import BRL.FreeTypeFont

Local font:TFreeTypeFont = TFreeTypeFont.Load("DroidSansMono.ttf", 12, SMOOTHFONT)

If font Then
	Print "Family = " + String.FromCString(font._face.fname)
	Print "Style  = " + String.FromCString(font._face.sname)
	Print "Height = " + font.Height()
Else
	Print "Could not load font"
End If
