SuperStrict

Framework brl.standardio

Local s:String[] = ["AAAAAAAA", "BBBBBBBB", "CCCCCCCC", "DDDDDDDD", "EEEEEEEE", "FFFFFFFF", "GGGGGGGG", "HHHHHHHH"]

Print "Before:"
dumpArray(s)

For Local i:Int = 0 Until s.length
	Print i + " : "
	Local slice1:String[] = s[i..i+1]
	dumpArray(slice1)
Next

For Local i:Int = 0 Until s.length
	Print i + " : "
	Local slice1:String[] = s[..i+1]
	dumpArray(slice1)
Next

For Local i:Int = 0 Until s.length
	Print i + " : "
	Local slice1:String[] = s[i..]
	dumpArray(slice1)
Next

For Local i:Int = 0 Until s.length - 1
	Print i + " : "
	Local slice1:String[] = s[i..i+2]
	dumpArray(slice1)
Next


Function dumpArray(arr:String[])
	Print "["
	For Local i:Int = 0 Until arr.length
		Print "  " + arr[i] + "   :   " + arr[i].length
	Next
	Print "]~n"
End Function

