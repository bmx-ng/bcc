Rem
	This test checks:
	- creation and modification of pixmaps work
End Rem
SuperStrict
Framework BRL.StandardIO
Import Brl.Pixmap

local pix:TPixmap = CreatePixmap(50,50, PF_RGBA8888)
local res:int = TRUE
'blackout
pix.ClearPixels(0)

For local x:int = 0 until pix.width
	For local y:int = 0 until pix.height
		pix.WritePixel(x,y, y*2)
	Next
Next

For local x:int = 0 until pix.width
	For local y:int = 0 until pix.height
		if y*2 <> pix.ReadPixel(x,y)
			res = FALSE
			exit
		endif
	Next
Next

print "success: "+res