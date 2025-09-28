
#include "brl.mod/blitz.mod/blitz.h"

#ifndef XXHASH_H_5627135585666179
#define XXH_STATIC_LINKING_ONLY
#define XXH_IMPLEMENTATION

#include "xxhash.h"
#endif

BBString * bmx_gen_hash(BBString * txt) {
	char buf[64];
	snprintf(buf, 64, "0x%llx", XXH3_64bits(txt->buf, txt->length * sizeof(BBChar)));
	return bbStringFromCString(buf);
}

BBString * bmx_gen_hash32(BBString * txt) {
	char buf[64];
	XXH64_hash_t hash = XXH3_64bits(txt->buf, txt->length * sizeof(BBChar));
	snprintf(buf, 64, "0x%x", (uint32_t)(hash ^ (hash >> 32)));
	return bbStringFromCString(buf);
}

XXH3_state_t * bmx_hash_createState() {
	return XXH3_createState();
}

void bmx_hash_reset(XXH3_state_t * state) {
	XXH3_64bits_reset(state);
}

void bmx_hash_update(XXH3_state_t * state, void * data, int length) {
	XXH3_64bits_update(state, data, length);
}

BBString * bmx_hash_digest(XXH3_state_t * state) {
	char buf[64];
	snprintf(buf, 64, "%llx", XXH3_64bits_digest(state));
	return bbStringFromCString(buf);
}
