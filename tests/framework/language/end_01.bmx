Rem
	This test checks:
	- if the command "end" is recognized correct
	- on success this sample is compileable :D
	- no "comparable result" as "end" ends the program :D
End Rem
SuperStrict
Framework BRL.StandardIO

'not possible in blitzmax
'If 1=0 then end endif

'not possible in blitzmax
'If 1=0 then end end if

'possible
If 1=0 End

'check for correct singleline-ending check
If 1=0 End;Print"ok"

'possible
If 1=0 Then End

'not possible in blitzmax -albeit ";" should connect things?
'If 1=0;end;endif

'possible - whatever this does ?
If 1=0;End

'possible
If 1=0
	End
EndIf

'possible
If 1=0
	End
End If

'possible - I prefer "Wend"
While 1=0
	End
End While

'not possible in blitzmax ;D
'if end if end end

'not possible in blitzmax ;D
'if (end) if (end) end

'not possible in blitzmax ;D
'if (end) then if (end) then end

'not possible in blitzmax ;D
Rem
if end
	if end
		end
	end if
end if
endrem

'not possible in blitzmax
'while End
'	'do
'end While


'possible
Function Call:Int();End;EndFunction

'possible
Function Call2:Int();End;End Function

