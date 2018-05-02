' Tests whether Finally blocks are executed correctly in simple Try-Finally constructs.

SuperStrict
Framework BRL.StandardIO


Print; Print 1
Try
	Print "try"
Finally
	Print "finally"
End Try


Print; Print 2
F
Function F()
	Try
		Print "try"
		Return
	Finally
		Print "finally"
	End Try
End Function


Print; Print 3
For Local i:Int = 1 To 2
	Try
		Print "try"
		Exit
	Finally
		Print "finally"
	End Try
Next


Print; Print 4
For Local i:Int = 1 To 2
	Try
		Print "try"
		Continue
	Finally
		Print "finally"
	End Try
Next

