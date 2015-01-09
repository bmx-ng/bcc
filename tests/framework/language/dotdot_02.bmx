Rem
	The following code checks various variants to use the ".." concatenator
End Rem
SuperStrict
Framework brl.StandardIO

Type Test
	Field fa:Int = 12, ..
	      fb:Int = 13
	Field fc:Int = 14 ..
	      , fd:Int = 15

	Function MyFunction:Int( ..    
	          a:Int, ..
	          b:Int ..
	         )
		Print a+","+b
	End Function

	Function MyFunctionB:Int(..
	          a:Int ..
	          , b:Int ..
	         )
		Print a+","+b
	End Function

	Method MyMethod()
		Print fa+","+fb+","+fc+","+fd
	End Method
End Type

Global T:Test = New Test
T.MyFunction(8,9)
T.MyFunctionB(10,11)
T.MyMethod()


Local a:Int=1, b:Int=2, ..
      c:Int=3
Local d:Int=4, e:Int=5 ..
      , f:Int=6

Print a+","+b+","+c+","+d+","+e+","+f



Global ga:Int=1, gb:Int=2, ..
       gc:Int=3
Global gd:Int=4, ge:Int=5 ..
       , gf:Int=6

Print ga+","+gb+","+gc+","+gd+","+ge+","+gf



Global ..
 gg:Int = 7

Print gg

