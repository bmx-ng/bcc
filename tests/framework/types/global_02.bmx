SuperStrict

Framework BRL.Standardio


If Not TInstanceType.instance Then
	Print "No Instance"
Else
	Print "Has Instance"
End If

If Not TInstanceType.instInit Then
	Print "No InstInit"
Else
	Print "Has InstInit"
End If

Type TInstanceType
	Global instance:TInstanceType
	Global instInit:TInstanceType = New TInstanceType

End Type
