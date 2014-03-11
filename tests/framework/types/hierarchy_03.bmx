SuperStrict

'needed to get it compile
Framework BRL.StandardIO

Type GrandParent
	Field prop:String = "A"
End Type


Type Parent Extends GrandParent
	Field prop:String = "B"
End Type


Type Myself Extends Parent
	Field prop:String = "C"
End Type


Local B:Parent = new Parent

If GrandParent(B) then  print "Parent can cast to GrandParent"
If Parent(B) then print "Parent can cast to self"
If Myself(B) then print "Parent can cast to child - FORBIDDEN!"
