SuperStrict

Framework BRL.StandardIO

Try
	TBase.Call()
Catch ex:Object
	Print "Caught illegal call to abstract"
End Try

Local impl:TBase = New TImpl
Try
impl.abstractFunction
Catch ex:Object
	Print "Error : should not throw exception!"
End Try


Local impl2:TBase = New TSubImpl
Try
impl2.abstractFunction
Catch ex:Object
	Print "Error : should not throw exception!"
End Try


Type TBase

	Function abstractFunction() Abstract

	Function Call()
		abstractFunction()
	End Function
	
End Type


Type TImpl Extends TBase

	Function abstractFunction()
		Print "Hello"
	End Function

End Type

Type TSubImpl Extends TImpl

	Function abstractFunction()
		Print "World"
	End Function

	
End Type