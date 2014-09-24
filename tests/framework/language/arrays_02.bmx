' test array of multi-dimensional array

SuperStrict

Framework brl.standardio


Local t:TType = New TType
t.big_array = New Float[,][10]

For Local i:Int = 0 Until 10

t.big_array[i] = New Float[5, 5]
	
	For Local x:Int = 0 Until 5
		For Local y:Int = 0 Until 5
			t.big_array[i][x, y] = i + x * y
		Next
	Next

Next

Local s:String
For Local i:Int = 0 Until 10

	For Local x:Int = 0 Until 5
		For Local y:Int = 0 Until 5
			s:+ " " + t.big_array[i][x, y]
		Next
		
		Print s
		s = ""
	Next

	Print ""
Next


Type TType

    Field big_array:Float[,][]

End Type
