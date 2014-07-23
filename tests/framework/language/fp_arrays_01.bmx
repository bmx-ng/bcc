'
' Test function pointer arrays
'
SuperStrict

Framework brl.standardio

Local t:TType = New TType
t.funcArray = [func1, func2]

Print t.funcArray[0]("1", 1, 1.0)
Print t.funcArray[1]("2", 2, 2.0)


Type TType

	Field funcArray:String(param1:String, param2:Int, param3:Double)[]

End Type


Function func1:String(param1:String, param2:Int, param3:Double)
	Return "func1"
End Function

Function func2:String(param1:String, param2:Int, param3:Double)
	Return "func2"
End Function
