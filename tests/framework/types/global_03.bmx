SuperStrict
Framework BRL.StandardIO

Import "global_03.import.bmx"


test = 20
print "global_03.bmx: " + test

FuncImport()
FuncImportOwnDecl()
FuncImportSuperDecl()
ImportType.Func()
ImportType.FuncSuperDecl()

print "global_03.bmx: " + test
