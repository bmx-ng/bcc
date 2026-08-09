SuperStrict

Framework BRL.StandardIO

Type TBase
	Method Test(value:Int = 5)
		Print "base:" + value
	End Method

	Method Conversion(value:Int)
		Print "base-conversion"
	End Method
End Type

Type TDerived Extends TBase
	Method Test(value:Int = 5, second:Int = 10)
		Print "derived:" + value + ":" + second
	End Method

	Method Conversion(value:Long, second:Int = 10)
		Print "derived-conversion"
	End Method
End Type

Type TSameLevel
	Method Test(value:Int = 5)
		Print "same-short:" + value
	End Method

	Method Test(value:Int = 5, second:Int = 10)
		Print "same-long:" + value + ":" + second
	End Method
End Type

Local derived:TDerived = New TDerived
derived.Test()
derived.Test(20)
derived.Test(20, 30)
derived.Conversion(40)

Local base:TBase = derived
base.Test()
base.Test(50)

Local same:TSameLevel = New TSameLevel
same.Test()
same.Test(60)
same.Test(60, 70)
