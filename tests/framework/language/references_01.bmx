Rem
	This test checks:
	- if there is a cyclic reference error
End Rem
SuperStrict
Framework BRL.StandardIO

Type TMyClass
	Global instance:TMyClass
End Type