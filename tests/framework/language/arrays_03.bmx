SuperStrict

Framework brl.standardio

Type Base
    Field name:String="Base"
End Type

Type Other
    Field name:String="Other"
End Type


Function CollectObjects:Int(obj:Object[])
    Print "got "+obj.length+" objects"
End Function


Local baseA:Base = New Base
Local baseB:Base = New Base
Local otherA:Other = New Other
Local otherB:Other = New Other


Local bases:Base[] = [baseA, baseB]
Local others:Other[] = [otherA, otherB]

CollectObjects(bases + others) 'prints 4

