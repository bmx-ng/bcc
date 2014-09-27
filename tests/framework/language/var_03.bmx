SuperStrict

Framework brl.standardio

Type testtype
    Field f:Float

    Function test(f:Float Var)
        'fails
        f = -f*0.5
        'would work
        'f = -1 * f*0.5
    End Function
EndType


