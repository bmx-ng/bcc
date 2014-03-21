Rem
	This test checks:
	- is passing variables by reference is possible
	- is passing functions as parameter is possible
End Rem
SuperStrict

Import BRL.StandardIO


local originalNumber:int = 10

Function PassByReference:int(number:int var, modifierFunction:int(number:int var))
	number :+ 5
	print number

	modifierFunction(number)
End Function

Function modifierFunction:int(number:int var)
	number :* 2
	print number
End Function


'should output:
'10
'15
'30
'30
print originalNumber
PassByReference(originalNumber, modifierFunction)
print originalNumber
