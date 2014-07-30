SuperStrict

Framework brl.standardio


Local obj:TObject = New TObject

obj.someMethod()


Type TObject

	Method someMethod()
	
		Local s:String
		Local i:Int
		
		Local _func:String(font:TObject, charKey:String, char:Int, config:String)
		
		_func = _callback
		
		Print _func(Self, "a", 0, "b")
		
	End Method
	
End Type

Function _callback:String(font:TObject, charKey:String, char:Int, config:String)
	Return "Hello!"
End Function
