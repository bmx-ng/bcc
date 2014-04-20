SuperStrict
Framework Brl.StandardIO

'this "queue" import IS NOT NEEDED because it gets imported by
'"extend" already. BUT : if you import "queue" you check if the
'information is correctly processed by the compiler.

Import "inheritance_01.queue.bmx"
Import "inheritance_01.extend.bmx"

local queue:MyTypeQueue = new MyTypeQueue

queue.insert(new MyOtherType)
