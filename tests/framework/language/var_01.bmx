SuperStrict

Framework BRL.StandardIO


Local s:String = "Hello"
Local a:Int[] = New Int[10]
Local i:Int = 20
Local b:Byte Ptr

vars(s, a, i, b)

Print "s = " + s
Print "a = " + a.length
Print "i = " + i

Local v:TVars = New TVars
v.s = "Horse"
v.a = New Int[20]
v.i = 11

vars2(v)

Print "v.s = " + v.s
Print "v.a = " + v.a.length
Print "v.i = " + v.i

Local i1:Int = 15
Local i2:Int = 17
Local i3:Int = 20

vars3(i1, i2, i3)

vars4(Varptr i1)

Function vars(s:String Var, a:Int[] Var, i:Int Var, b:Byte Ptr Var)
	s = "World"
	a[0] = 10
	i = 15
	b = Null

	If s Then
		Print "Hello " + s
	End If
	
	If s = "Fish" Then
		Print "Fishy!"
	End If
	
	Print "a = " + a.length
	
	a = New Int[15]
	
End Function

Function vars2(v:TVars Var)
	v = New TVars
	v.a = New Int[5]
	v.i = 13
End Function

Function vars3(i1:Int Var, i2:Int Var, i3:Int Var)
	i1 = i2
	If Varptr i1 = Varptr i2 Then
		Print "i1 = i2"
	End If
	
	vars4(Varptr i3)
End Function

Function vars4(i:Int Ptr)
	Print i[0]
End Function

Type TVars

	Field s:String = "Piggy"
	Field a:Int[]
	Field i:Int

End Type
