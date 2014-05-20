TODO
====

Debug / Reflection
------------------
Need to find a way to store the data that the debugger and reflection uses.
Will need to calculate field/method offsets at generation-time. This will vary depending on the architecture we are building on.

Private/Public
--------------
Declaring sections as Private should prevent generation to the interface (.i)

OpenGL/EGL
----------
See what needs to be done to port GL to EGL (for such platforms as the RPi)

Framework
---------
When compiling an application with a "Framework", the framework chosen is passed down to all the imported source files and applied to them too. Without it, each imported source is assumed to have access to *all* the standard modules (and will include all of them in their interface).
Currently not supporting an application build without framework...

Data
----
DefData, ReadData and RestoreData and data labels (#) are not currently implemented.

See blitz_types.h for tag types.
Data is stored thus :
  data type
  data
  data type
  data
  ...
An index/pointer will need to keep track of the current position in the data.
RestoreData can reset this to a 'data label' position.

Extern Types
------------
Represents a C structure directly in the code.

Does not support internal methods/functions.
We may be able to simply create a struct here that we can pass to functions, etc.
