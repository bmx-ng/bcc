SuperStrict

Framework BRL.StandardIO
Import BRL.FreeTypeFont

Local font:TFreeTypeFont = TFreeTypeFont.Load("DroidSansMono.ttf", 12, SMOOTHFONT)

If font Then
	Print "Family = " + font.FamilyName()
	Print "Style  = " + font.StyleName() 
	Print "Height = " + font.Height()
Else
	Print "Could not load font"
End If
