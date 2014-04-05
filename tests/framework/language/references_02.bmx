'code checks if objects are passed by reference
SuperStrict
Framework Brl.Standardio


Type TMyObject
	field prop:int = 10
	field stringProp:string = "unmodified"
End Type

local obj:TMyObject = new TMyObject

Function Modify:int(obj:TMyObject)
	obj.prop = 20
End Function

Function ModifyVar:int(obj:TMyObject var)
	obj.prop = 30
End Function

Function ModifyVarNew:int(obj:TMyObject var)
	obj = New TMyObject
	obj.prop = 40
End Function

Function ModifyProp:int(val:int var)
	val = 50
End Function

'this should do not change the given param
Function ModifyNoChange:int(val:int)
	val = 60
End Function

'this should do not change the given param
Function ModifyString:int(val:string var)
	val = "modified"
End Function

'this should do not change the given param
Function ModifyStringNoChange:int(val:string)
	val = "modified"
End Function

print obj.prop
Modify(obj)
print obj.prop
ModifyVar(obj)
print obj.prop
ModifyVarNew(obj)
print obj.prop
ModifyProp(obj.prop)
print obj.prop
ModifyNoChange(obj.prop)
print obj.prop
ModifyStringNoChange(obj.stringProp)
print obj.stringProp
ModifyString(obj.stringProp)
print obj.stringProp
