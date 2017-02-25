'Test for negative array indices
'Source: https://github.com/bmx-ng/bcc/issues/248
'Author: HurryStarfish
SuperStrict
Framework BRL.StandardIO

Local a:Int[] = [7,8,9]
Local p:Int Ptr = Varptr a[1]
Print p[-1]