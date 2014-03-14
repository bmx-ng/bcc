SuperStrict

Framework BRL.LinkedList
Import BRL.StandardIO

Local digits:int[] = [3, 2, 1, 6, 5, 4, 9, 8, 7, 10]

Local list:TList = CreateList()

For Local digit:int = EachIn digits
	list.AddLast(string(digit))
Next

'sort the list now (alphabetical sort)
SortList(list)

For local digit:string = EachIn list
	print digit
Next