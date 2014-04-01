'code checks if objects are passed by reference
SuperStrict
Framework Brl.Standardio


Type TMyObject
	field prop:int = 10
End Type

local obj:TMyObject = new TMyObject

Function Modify:int(obj:TMyObject)
	obj.prop = 20
End Function

Function ModifyVar:int(obj:TMyObject var)
	obj.prop = 30
End Function

print obj.prop
Modify(obj)
print obj.prop
ModifyVar(obj)
print obj.prop