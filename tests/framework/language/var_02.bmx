SuperStrict

Framework brl.standardio
Import brl.math

Local n1:Byte   = 10
Local n2:Short  = 10
Local n3:Int    = 10
Local n4:Long   = 10
Local n5:Float  = 10.0
Local n6:Double = 10.0

Print Testb1(n1)
Print Testb2(n2)
Print Testb3(n3)
Print Testb4(n4)
Print Testb5(n5)
Print Testb6(n6)

Print Tests1(n1)
Print Tests2(n2)
Print Tests3(n3)
Print Tests4(n4)
Print Tests5(n5)
Print Tests6(n6)

Print Testi1(n1)
Print Testi2(n2)
Print Testi3(n3)
Print Testi4(n4)
Print Testi5(n5)
Print Testi6(n6)

Print Testl1(n1)
Print Testl2(n2)
Print Testl3(n3)
Print Testl4(n4)
Print Testl5(n5)
Print Testl6(n6)

Print Testf1(n1)
Print Testf2(n2)
Print Testf3(n3)
Print Testf4(n4)
Print Testf5(n5)
Print Testf6(n6)

Print Testd1(n1)
Print Testd2(n2)
Print Testd3(n3)
Print Testd4(n4)
Print Testd5(n5)
Print Testd6(n6)


Function Testb1:Byte( point_x:Byte Var) 
     Local a:Byte = point_x
     Return a
End Function

Function Testb2:Byte( point_x:Short Var) 
     Local a:Byte = point_x
     Return a
End Function

Function Testb3:Byte( point_x:Int Var) 
     Local a:Byte = point_x
     Return a
End Function

Function Testb4:Byte( point_x:Long Var) 
     Local a:Byte = point_x
     Return a
End Function

Function Testb5:Byte( point_x:Float Var) 
     Local a:Byte = point_x
     Return a
End Function

Function Testb6:Byte( point_x:Double Var) 
     Local a:Byte = point_x
     Return a
End Function


Function Tests1:Short( point_x:Byte Var) 
     Local a:Short = point_x
     Return a
End Function

Function Tests2:Short( point_x:Short Var) 
     Local a:Short = point_x
     Return a
End Function

Function Tests3:Short( point_x:Int Var) 
     Local a:Short = point_x
     Return a
End Function

Function Tests4:Short( point_x:Long Var) 
     Local a:Short = point_x
     Return a
End Function

Function Tests5:Short( point_x:Float Var) 
     Local a:Short = point_x
     Return a
End Function

Function Tests6:Short( point_x:Double Var) 
     Local a:Short = point_x
     Return a
End Function


Function Testi1:Int( point_x:Byte Var) 
     Local a:Int = point_x
     Return a
End Function

Function Testi2:Int( point_x:Short Var) 
     Local a:Int = point_x
     Return a
End Function

Function Testi3:Int( point_x:Int Var) 
     Local a:Int = point_x
     Return a
End Function

Function Testi4:Int( point_x:Long Var) 
     Local a:Int = point_x
     Return a
End Function

Function Testi5:Int( point_x:Float Var) 
     Local a:Int = point_x
     Return a
End Function

Function Testi6:Int( point_x:Double Var) 
     Local a:Int = point_x
     Return a
End Function


Function Testl1:Long( point_x:Byte Var) 
     Local a:Long = point_x
     Return a
End Function

Function Testl2:Long( point_x:Short Var) 
     Local a:Long = point_x
     Return a
End Function

Function Testl3:Long( point_x:Int Var) 
     Local a:Long = point_x
     Return a
End Function

Function Testl4:Long( point_x:Long Var) 
     Local a:Long = point_x
     Return a
End Function

Function Testl5:Long( point_x:Float Var) 
     Local a:Long = point_x
     Return a
End Function

Function Testl6:Long( point_x:Double Var) 
     Local a:Long = point_x
     Return a
End Function


Function Testf1:Float( point_x:Byte Var) 
     Local a:Float=Sqr(point_x)
     Return a
End Function

Function Testf2:Float( point_x:Short Var) 
     Local a:Float=Sqr(point_x)
     Return a
End Function

Function Testf3:Float( point_x:Int Var) 
     Local a:Float=Sqr(point_x)
     Return a
End Function

Function Testf4:Float( point_x:Long Var) 
     Local a:Float=Sqr(point_x)
     Return a
End Function

Function Testf5:Float( point_x:Float Var) 
     Local a:Float=Sqr(point_x)
     Return a
End Function

Function Testf6:Float( point_x:Double Var) 
     Local a:Float=Sqr(point_x)
     Return a
End Function


Function Testd1:Double( point_x:Byte Var) 
     Local a:Double=Sqr(point_x)
     Return a
End Function

Function Testd2:Double( point_x:Short Var) 
     Local a:Double=Sqr(point_x)
     Return a
End Function

Function Testd3:Double( point_x:Int Var) 
     Local a:Double=Sqr(point_x)
     Return a
End Function

Function Testd4:Double( point_x:Long Var) 
     Local a:Double=Sqr(point_x)
     Return a
End Function

Function Testd5:Double( point_x:Float Var) 
     Local a:Double=Sqr(point_x)
     Return a
End Function

Function Testd6:Double( point_x:Double Var) 
     Local a:Double=Sqr(point_x)
     Return a
End Function
