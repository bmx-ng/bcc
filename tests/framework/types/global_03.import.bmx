SuperStrict
Import BRL.StandardIO

Global test:int = 10

print "global_03.import.bmx: "+test

Function FuncImport()
	print "global_03.import.bmx function using global: "+test
End Function

Function FuncImportOwnDecl()
	global test:int = 30
	print "global_03.import.bmx function with own global: "+test
End Function

Function FuncImportSuperDecl()
	global test:int = 40
	'"super.test" is NOT possible (no method) but "." accesses the
	'global scope
	print "global_03.import.bmx function with super global: "+ .test
End Function


Type ImportType
	global test:int = ImportType.GetNewGlobalValue()

	Function GetNewGlobalValue:int()
		'this creates some kind of "cyclic reference" as test
		'gets assigned a value consisting of "test" + x
		return test + 50
	End Function

	Function Func()
		print "global_03.import.bmx type function using global: "+ test
	End Function

	Function FuncSuperDecl()
		print "global_03.import.bmx type function using super global: "+ .test
	End Function
End Type	