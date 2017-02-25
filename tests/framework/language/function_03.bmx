'Test for nested functions
'Source: https://github.com/bmx-ng/bcc/issues/251
'Author: HurryStarfish
SuperStrict
Framework BRL.Blitz

Function F()
	G
	Function G()
		F
	End Function
End Function



Function F2()
	G2
	Function G2()
		H2
	End Function
End Function

Function H2()
	F2
End Function