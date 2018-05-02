' Tests whether exceptions are caught and Finally blocks are executed correctly in complex Try-Catch-Finally constructs.

SuperStrict
Framework BRL.StandardIO


Function GetNumber:Int()
	Print "getting 5"
	Return 5
End Function

Function ThrowEx:Int()
	Print "throw ex"
	Throw "ex"
End Function

Function ThrowEx2:Int()
	Print "throw ex2"
	Throw "ex2"
End Function


Print; Print 1
Print "returned " + F()
Function F:Int()
	Try
		Print "try"
		Return GetNumber()
	Finally
		Print "finally"
	End Try
End Function


Print; Print 2
Print "returned " + F2()
Function F2:Int()
	Try
		Print "try"
		Return GetNumber()
	Catch e:Int[]
		Print "this should not happen"
	Catch e:String
		Print "this should not happen"
	Finally
		Print "finally"
	End Try
End Function


Print; Print 3
Print "returned " + F3()
Function F3:Int()
	Try
		Print "try"
		Return ThrowEx()
	Catch e:Int[]
		Print "this should not happen"
	Catch e:String
		Print "catch " + e
	Finally
		Print "finally"
	End Try
End Function


Print; Print 4
Print "returned " + F4()
Function F4:Int()
	Try
		Print "try"
		Return ThrowEx()
	Catch e:Int[]
		Print "this should not happen"
	Catch e:String
		Print "catch " + e
		Return 7
	Finally
		Print "finally"
	End Try
End Function


Print; Print 5
Print "returned " + F5()
Function F5:Int()
	Try
		Try
			Print "try"
			Return ThrowEx()
		Catch e:Int[]
			Print "this should not happen"
		Catch e:String
			Print "catch " + e
			Return ThrowEx2()
		Finally
			Print "finally"
		End Try
	Catch e:String
		Print "catch2 " + e
	Finally
		Print "finally2"
	End Try
End Function


Print; Print 6
Try
	F6
Catch e:String
	Print "catch3 " + e
End Try
Function F6()
	For Local i:Int = 1 To 2
		Try
			Print "try"
			Return
		Finally
			Print "finally"
			Try
				Print "try2"
				Throw "asdf"
			Finally
				Print "finally2"
			End Try
		End Try
	Next
End Function


Print; Print 7
F7
Function F7()
	For Local i:Int = 1 To 2
		Try
			Print "try"
			Try
				Print "try2"
				Exit
			Finally
				Print "finally2"
			End Try
		Finally
			Print "finally"
		End Try
	Next
End Function


Print; Print 8
F8
Function F8()
	For Local i:Int = 1 To 2
		Try
			Print "try"
			Try
				Print "try2"
				Continue
			Finally
				Print "finally2"
			End Try
		Finally
			Print "finally"
		End Try
	Next
End Function


Print; Print 9
F9
Function F9()
	#loop
	For Local i:Int = 1 To 2
		Try
			Print "try"
			Try
				Print "try2"
				For Local j:Int = 1 To 2
					Exit loop
				Next
			Finally
				Print "finally2"
			End Try
		Finally
			Print "finally"
		End Try
	Next
End Function


Print; Print 10
F10
Function F10()
	#loop
	For Local i:Int = 1 To 2
		Try
			Print "try"
			Try
				Print "try2"
				For Local j:Int = 1 To 2
					Continue loop
				Next
			Finally
				Print "finally2"
			End Try
		Finally
			Print "finally"
		End Try
	Next
End Function


Print; Print 11
Try
	Print "try"
	Try
		Print "try2"
		Throw "ex"
	Catch e:String
		Print "catch2 " + e
		Throw "ex2"
	Finally
		Print "finally2"
		Try
			Print "try3"
		Catch e:String
			Print "catch3"
		Finally
			Print "finally3"
			Throw "ex3"
		End Try
	End Try
Catch e:String
	Print "catch " + e
End Try


Print; Print 12
Try
	Print "returned " + F12()
Catch e:String
	Print "catch5 " + e
End Try
Function F12:Int()
	Try
		Try
			Try
				Try
					Print "try4"
					Return 7
				Finally
					Print "finally4"
				End Try
			Finally
				Print "finally3"
			End Try
		Finally
			Print "finally2"
			Throw "ex"
		End Try
	Finally
		Print "finally"
	End Try
	Return 2
End Function
