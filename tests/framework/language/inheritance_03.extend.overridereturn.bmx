SuperStrict
Framework Brl.StandardIO

Type TBase
	Field child:TBase

	Method MyChildren:TBase[](param:Int, param2:String)
		Return [child]
	End Method
End Type
	

Type TExtend Extends TBase
	Method MyChildren:TExtend[](param:Int, param2:String)
		Local children:TBase[] = Super.MyChildren(param, param2)
		Local result:TExtend[]
		For Local e:TExtend = EachIn children
			result :+ [e]
		Next
		Return result
	End Method
End Type
