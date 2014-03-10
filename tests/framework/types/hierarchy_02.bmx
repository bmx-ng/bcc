SuperStrict

'needed to get it compile
Framework BRL.StandardIO

Type TypeA
	Field property:String = "propA"
	Field prop:String = "A"
	Field prop2:String = "A"
	Global glob:String = "A"


	Method GetProp:String()
		Return prop
	End Method

	Method GetGlob:String()
		Return glob
	End Method
End Type


Type TypeB Extends TypeA
	'not overwriting, but replacing the whole property
	Field prop:String = "B"
	Global glob:String = "B"


	'overwrite getter
	Method GetProp:String()
		Return prop
	End Method


	Method PrintProperty()
		'this wont work with original BlitzMax
		'  Print "super: " + Super.prop
		'so we use getters to access Super.PropertyX
		Print "Super.GetProp: " + Super.GetProp() + "  =  A"
		Print "Self.GetProp : " + GetProp() + " =/= A"
		Print "Self.prop    : " + Self.prop + " =/= A"
		Print "Self.prop2   : " + Self.prop2 + "  =  A"

		Print "Self.GetGlob : " + GetGlob() + "  =  A"
		Print "Self.glob    : " + glob + " =/= A"
	End Method
End Type


Local B:TypeB = New TypeB
Rem
should print

Super.GetProp: A  =  A
Self.GetProp : B  =  B
Self.prop    : B  =  B
Self.prop2   : A =/= A
Self.GetGlob : A  =  A
self.glob    : B =/= A

End Rem

B.PrintProperty()