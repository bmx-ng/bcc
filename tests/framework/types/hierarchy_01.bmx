SuperStrict

Framework BRL.StandardIO


Local elise:TVehicle = New TSportsCar.Create("Lotus", "Elise")
Local esprit:TVehicle = New TSportsCar.Create("Lotus", "Esprit")
TSportsCar(esprit).SetTurbo()

Print elise.Dump()
Print esprit.Dump()


Type TVehicle

	Field make:String
	Field model:String
	
	Method Create:TVehicle(make:String, model:String)
		Self.make = make
		Self.model = model
		Return Self
	End Method
	
	Method Wheels:Int() Abstract
	
	Method Dump:String()
		Return make + " " + model
	End Method

End Type


Type TMotorbike Extends TVehicle

	Method Wheels:Int()
		Return 2
	End Method

End Type

Type TCar Extends TVehicle

	Method Wheels:Int()
		Return 4
	End Method

End Type


Type TSportsCar Extends TCar

	Field turbo:Int
	
	Method SetTurbo()
		turbo = True
	End Method

	Method Dump:String()
		Local t:String
		If Not turbo Then
			t = "No "
		End If
		Return Super.Dump() + " : " + t + "Turbo"
	End Method

End Type
