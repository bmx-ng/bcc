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
If 1=0 end

'possible
If 1=0 then end

'not possible in blitzmax -albeit ";" should connect things?
'If 1=0;end;endif

'possible - whatever this does ?
If 1=0;end

'possible
If 1=0
	end
endif

'possible
If 1=0
	end
end if

'possible - I prefer "Wend"
while 1=0
	End
End While

'not possible in blitzmax ;D
'if end if end end

'not possible in blitzmax ;D
'if (end) if (end) end

'not possible in blitzmax ;D
'if (end) then if (end) then end

'not possible in blitzmax ;D
rem
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
Function Call:int();end;EndFunction

'possible
Function Call2:int();end;End Function

