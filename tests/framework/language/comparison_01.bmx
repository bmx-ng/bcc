'test checks if comparison is done properly
SuperStrict
Framework Brl.Standardio

local myVar:int = 1
if myVar > 1 then print "myVar 1 > 1"

Function CompareParam:int(myvar:int)
	return myVar>1
End Function

Function CompareParamVar:int(myvar:int var)
	return myVar>1
End Function


print "intcasted comparison: "+int(myVar>2)

'fails currently
print "automatic comparison: "+(myVar>2)

If CompareParam(1) then print "function param failed"
If CompareParamVar(myvar) then print "function param var failed"

'would segfault in bcc-ng
'vanilla bcc: Compile Error: Expression for 'Var' parameter must be a variable
'If CompareParamVar(1) then print "function param var failed"