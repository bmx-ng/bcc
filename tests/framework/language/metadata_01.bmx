Rem
	This test checks:
	- if you can define "metadata" {meta="bla"}
End Rem
SuperStrict
Framework BRL.StandardIO


Type TMyClass {typemetadata="available"}
	Field myfield:int  {fieldmetadata="available"}
	Global instance:TMyClass

	Function myfunc:int() {funcmetadata="available"}
	End Function
End Type