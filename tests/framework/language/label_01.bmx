'Test for nested functions
'Source: https://github.com/bmx-ng/bcc/issues/236
'Author: HurryStarfish
SuperStrict
Framework BRL.StandardIO

#Loop
For Local a:Int = EachIn [1, 2, 3]
	If a = 2 Then Exit Loop
	Print a
Next