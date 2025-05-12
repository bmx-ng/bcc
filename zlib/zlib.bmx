SuperStrict

Import "*.h"
Import "adler32.c"
Import "compress.c"
Import "crc32.c"
Import "deflate.c"
Import "gzclose.c"
Import "gzlib.c"
Import "gzread.c"
Import "gzwrite.c"
Import "infback.c"
Import "inffast.c"
Import "inflate.c"
Import "inftrees.c"
Import "trees.c"
Import "uncompr.c"
Import "zutil.c"

? BmxNG
Extern

Rem
bbdoc: Compress a block of data at default compression level
End Rem
Function compress:Int( dest:Byte Ptr,dest_len:ULongInt Var,source:Byte Ptr,source_len:ULongInt )="int compress(void *, unsigned long *, const void *, unsigned long)"

Rem
bbdoc: Compress a block of data at specified compression level
end rem
Function compress2:Int( dest:Byte Ptr,dest_len:ULongInt Var,source:Byte Ptr,source_len:ULongInt,level:Int )="int compress2(void *, unsigned long *, const void *, unsigned long , int)"

Rem
bbdoc: Uncompress a block of data
end rem
Function uncompress:Int( dest:Byte Ptr,dest_len:ULongInt Var,source:Byte Ptr,source_len:ULongInt )="int uncompress(void *, unsigned long *, const void *, unsigned long)"

End Extern

? Not BmxNG
Extern

Rem
bbdoc: Compress a block of data at default compression level
End Rem
Function compress:Int( dest:Byte Ptr,dest_len:Int Var,source:Byte Ptr,source_len:Int )

Rem
bbdoc: Compress a block of data at specified compression level
end rem
Function compress2:Int( dest:Byte Ptr,dest_len:Int Var,source:Byte Ptr,source_len:Int,level:Int )

Rem
bbdoc: Uncompress a block of data
end rem
Function uncompress:Int( dest:Byte Ptr,dest_len:Int Var,source:Byte Ptr,source_len:Int )

End Extern
?
