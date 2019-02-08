SuperStrict
Framework BRL.StandardIO

Local i:Int = 0
If i = 0 Then
	If i <> 0 Then If i <> 3 Then Print i+" is not 1 or 2 or 3"
	Print "hmm"
EndIf


Local a:Int=1, b:Int =0

If a Then Print "single line"
If a Print "single line" Else Print "single line b"

If a Then Print "a" Else If b Then Print "b" Else Print "not b"
If b Then Print "not b" Else If Not a Then Print "not a" Else Print "a"

If a If b Then Print "chained single line"

If a If b
	Print "chained multi line"
EndIf

If a Then If b
	Print "chained multi line then"
EndIf

If a Then Print "single line then" Else Print "single line else"

If b Then Else Print "single line skipped then"


'use variables, else compiler possibly "optimizes out" the if-thens
Local c:Int = 1
If c + 2 = 3
	Print "calculation ok"
Else
	Print "cannot calculate"
EndIf


'end check
If 1 = 1 Then
	Print "hi"
	End
	Print "wont show"
EndIf


Function testme:int()
EndFunction

If Not testme() End
