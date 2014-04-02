SuperStrict

Framework BRL.Standardio

Local a1:Int[] = [1,2,3,4,5,6,7,8,9,10]

Local b1:Int[10]

Local c1:Int[2,5]

For Local i:Int = 0 Until a1.length
	b1[i] = a1[i]
	
	c1[i/5,i Mod 5] = a1[i]
Next

Local s:String = "b1 = ["
For Local i:Int = 0 Until a1.length
	If i Then
		s :+ ","
	End If

	s :+ b1[i]
Next

Print s + "]"

s = "c1 = ["
Local n:Int
For Local x:Int = 0 Until 2
	For Local y:Int = 0 Until 5
		If n Then
			s :+ ","
		End If
		n :+ 1
		s:+ c1[x,y]
	Next
Next

Print s + "]"
