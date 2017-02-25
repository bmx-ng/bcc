Rem
	This test checks:
	- if you can define "metadata" {meta="bla"}
	- if you use an "empty" meta tag
End Rem
SuperStrict
Framework BRL.StandardIO


Type TMyClass {typemetadata="available"}
	Field myfield:int  {fieldmetadata="available"}
	Global instance:TMyClass

	Function myfunc:int() {funcmetadata="available"}
	End Function

	Function emptyfunc:int() {}
	End Function
End Type