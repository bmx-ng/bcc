Rem
	This test checks:
	- if adding to arrays is possible
End Rem
SuperStrict
Framework BRL.StandardIO

Type obj
End Type

local objArray:obj[]

'style A
objArray = objArray[..1]
objArray[objArray.length-1] = new obj

'style B
objArray :+ [new obj]
