Rem
	This test checks:
	- and if loading as animimage works too
End Rem
SuperStrict
Framework BRL.StandardIO
Import Brl.Pixmap
Import Brl.GLMax2D

Graphics 640,480,0

'create a pixmap
local pix:TPixmap = CreatePixmap(64,64, PF_RGBA8888)
local res:int = TRUE
'blackout
pix.ClearPixels(0)

For local x:int = 0 until pix.width
	For local y:int = 0 until pix.height
		pix.WritePixel(x,y, RGBA_Color(255, 3*x,4*y, 2*(x+y)) )
	Next
Next

local animImage:TImage = LoadAnimImage(pix,32,32,0,4)


Cls
DrawPixmap(pix, 50,50)
DrawImage(animImage, 150, 50, 0)
DrawImage(animImage, 200, 50, 1)
DrawImage(animImage, 150,100, 2)
DrawImage(animImage, 200,100, 3)
Flip 0
Delay(250)

print "success: "+res





Function RGBA_Color:Int(alpha:int,r:int,g:int,b:int)
	Return (Int(alpha * $1000000) + Int(r * $10000) + Int(g * $100) + Int(b))
EndFunction