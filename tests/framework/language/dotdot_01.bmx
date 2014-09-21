Strict

Framework brl.standardio






Function RenderText(text:String, xpos:Int, ypos:Int, centered:Int = False, ..
					  shadow:Int = False, caps:Int = False)

End Function


Extern

Function Decode_Ogg:Byte Ptr(..
	datasource:Object,..
	reado(buf@Ptr,size,nmemb,src:Object),..
	seeko(src:Object,offset:Long,whence),..
	closeo(src:Object),..
	tello(src:Object ),..
	samples Var,channels Var,freq Var)

End Extern


Function createIO:Int()
	Return test(test2(10, ..
			15, Null, Null))
End Function

Function test:Int(val:Int)
End Function

Function test2:Int(a:Int, b:Int, x:Object, y:Object)
End Function

