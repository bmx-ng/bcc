SuperStrict

Framework BRL.Standardio


' instance no init
Global instance:TInstanceType

Global instInit:TInstanceType = New TInstanceType

If Not instance Then
	Print "No Instance"
Else
	Print "Has Instance"
End If

If Not instInit Then
	Print "No InstInit"
Else
	Print "Has InstInit"
End If


Type TInstanceType

End Type


