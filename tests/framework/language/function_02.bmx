'Test for nested functions
'Source: https://github.com/bmx-ng/bcc/issues/255
'Author: HurryStarfish
SuperStrict
Framework BRL.StandardIO

Type T
	Function F1()
		Function G()
		End Function
	End Function
	
	Function F2()
		Function G()
		End Function
	End Function
End Type