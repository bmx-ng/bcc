Rem
	This test checks:
	- if function pointers are possible
End Rem
SuperStrict
Framework BRL.StandardIO


Type MyType
	Field _func:int()

	Method Run:int()
		if _func then _func()
	End Method
End Type