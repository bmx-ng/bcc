
#define XXH_STATIC_LINKING_ONLY
#define XXH_IMPLEMENTATION

#include "xxhash.h"
#include "brl.mod/blitz.mod/blitz.h"

BBString * bmx_gen_hash(BBString * txt) {
	char * buf[64];
	snprintf(buf, 64, "0x%llx", XXH3_64bits(txt->buf, txt->length * sizeof(BBChar)));
	return bbStringFromCString(buf);
}
