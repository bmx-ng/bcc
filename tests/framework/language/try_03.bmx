' Tests whether exceptions are caught and Finally blocks are executed correctly in simple Try-Catch-Finally constructs.

SuperStrict
Framework BRL.StandardIO


Print; Print 1
Try
	Print "try"
Catch e:String
	Print "catch " + e
Finally
	Print "finally"
End Try


Print; Print 2
Try
	Print "try"
	Throw "ex"
Catch e:String
	Print "catch " + e
Finally
	Print "finally"
End Try


Print; Print 3
Try
	Try
		Print "try"
		Throw "ex"
	Finally
		Print "finally"
	End Try
Catch e:String
	Print "catch2 " + e
End Try


Print; Print 4
Try
	Try
		Print "try"
		Throw "ex"
	Catch e:TStream
		Print "this should not happen"
	Finally
		Print "finally"
	End Try
Catch e:String
	Print "catch2 " + e
End Try

Print; Print 5
Try
	Try
		Print "try"
		Throw "ex"
	Catch e:String
		Print "catch " + e
		Throw "ex2"
	Finally
		Print "finally"
	End Try
Catch e:String
	Print "catch2 " + e
End Try

