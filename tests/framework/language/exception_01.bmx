SuperStrict
Framework Brl.Standardio


Type TMyException Extends TBlitzException
	Field message:String

	Method ToString:String()
		If message = Null  then Return GetDefaultMessage()
		Return message
	End Method


	Method GetDefaultMessage:String()
		Return "Undefined TMyException!"
	End Method
End Type

print "start TRY"
Try
	local tmpObj:TMyException = null
	if not tmpObj
		local myException:TMyException = New TMyException
		myException.message = "assignment to null is not allowed"
		Throw myException
	endif
	tmpObj.message = "123" 'fails
	print "assignment possible while not allowed"
Catch ex:Object
	if not TBlitzException(ex) then print "unknown exception:"
	print ex.ToString()
End Try
print "end TRY"
