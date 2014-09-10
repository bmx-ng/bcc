TODO
====

Debug / Reflection
------------------
Need to find a way to store the data that the debugger and reflection uses.
Will need to calculate field/method offsets at generation-time. This will vary depending on the architecture we are building on.

OpenGL/EGL
----------
See what needs to be done to port GL to EGL (for such platforms as the RPi)

Extern Types
------------
Represents a C structure directly in the code.

Does not support internal methods/functions.
We may be able to simply create a struct here that we can pass to functions, etc.
