SuperStrict
Import Brl.StandardIO


Type MyTypeQueue
	Field _content:MyType[]

	Method Insert:int(obj:MyType)
		_content :+ [obj]
		print "added"
	End Method
End Type


Type MyType
	Field myOthervar:int
End Type
