
#include "brl.mod/blitz.mod/blitz.h"

#ifndef XXHASH_H_5627135585666179
#define XXH_STATIC_LINKING_ONLY
#define XXH_IMPLEMENTATION

#include "xxhash.h"
#endif

BBString * bmx_gen_hash(BBString * txt) {
	char * buf[64];
	snprintf(buf, 64, "0x%llx", XXH3_64bits(txt->buf, txt->length * sizeof(BBChar)));
	return bbStringFromCString(buf);
}
