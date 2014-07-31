'
' Test function pointer arrays
'
SuperStrict

Framework brl.standardio

Local t:TType = New TType
t.funcArray = [func1, func2, Null]

Print t.funcArray[0]("1", 1, 1.0)
Print t.funcArray[1]("2", 2, 2.0)

t.update(2, func3)

Print t.funcArray[2]("3", 3, 3.0)

Type TType

	Field funcArray:String(param1:String, param2:Int, param3:Double)[]
	
	Method update(pos:Int, func_:String(param1:String, param2:Int, param3:Double))
		funcArray[pos] = func_
	End Method
	
End Type


Function func1:String(param1:String, param2:Int, param3:Double)
	Return "func1 " + param1 + " " + param2 + " " + param3
End Function

Function func2:String(param1:String, param2:Int, param3:Double)
	Return "func2 " + param1 + " " + param2 + " " + param3
End Function

Function func3:String(param1:String, param2:Int, param3:Double)
	Return "func3 " + param1 + " " + param2 + " " + param3
End Function
