TODO
====

Exceptions
Debug

Private/Public
--------------
Declaring sections as Private should prevent generation to the interface (.i)

Merge Interfaces
----------------
Modules that require multiple files need to merge the imported interfaces into the main interface, visible as its public API.

OpenGL/EGL
----------
See what needs to be done to port GL to EGL (for such platforms as the RPi)

Incbin
------
Embed files in headers, and add code to register them at runtime.

Solve the Method Calls problem
------------------------------
There is an issue with the way the following code is currently handled :

pixmap.Window(n*8,0,8,16).Copy()

By definition, a method requires an instance of the object in order to look up the correct method to call. So in this case, the result from Window() needs to be passed to Copy(). The current code looks like this :

[pixmap_class->md_window(pixmap_obj,n*8,0,8,16)_class]->md_copy(pixmap_class->md_window(pixmap_obj,n*8,0,8,16))

One option might be to pull the call to Window out into a new statement, and use the result from that to call Copy(). You'd end up with something like :

tmpPixmap = pixmap_class->md_window(pixmap_obj,n*8,0,8,16);
...
tmpPixmap_class->md_copy(tmpPixmap)
...
