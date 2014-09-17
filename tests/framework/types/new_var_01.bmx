SuperStrict

Framework brl.standardio

Local t:TType = New TType
Global c:TType = New TType

Local n:Object = cloneObject(t)
Local m:Object = New c

Local x:TXType = New TXType
' doesn't work... yet
Local o:Object' = New x.s

If TType(t) Then
	Print "t is a TType"
End If

If TType(n) Then
	Print "n is a TType"
End If

If TType(m) Then
	Print "m is a TType"
End If

If TType(o) Then
	Print "o is a TType"
End If


Function cloneObject:Object(obj:Object)
	'create a new instance of the objects type
	 Local clone:Object = New obj
	Return clone
End Function

Type TType

End Type

Type TXType

	Field s:TType = New TType

End Type