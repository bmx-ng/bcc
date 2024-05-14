
#include <brl.mod/blitz.mod/blitz.h>

enum binaryOps {
	OP_MUL,
	OP_DIV,
	OP_MOD,
	OP_SHL,
	OP_SHR,
	OP_SAR,
	OP_ADD,
	OP_SUB,
	OP_AND,
	OP_XOR,
	OP_OR
};

unsigned int bmx_string_to_uint( BBString *t ){
	int i=0,neg=0;
	unsigned n=0;
	
	while( i<t->length && isspace(t->buf[i]) ) ++i;
	if( i==t->length ) return 0;
	
	if( t->buf[i]=='+' ) ++i;
	else if( (neg = t->buf[i]=='-') ) ++i;
	if( i==t->length ) return 0;

	if( t->buf[i]=='%' ){
		for( ++i;i<t->length;++i ){
			int c=t->buf[i];
			if( c!='0' && c!='1' ) break;
			n=n*2+(c-'0');
		}
	}else if( t->buf[i]=='$' ){
		for( ++i;i<t->length;++i ){
			int c=toupper(t->buf[i]);
			if( !isxdigit(c) ) break;
			if( c>='A' ) c-=('A'-'0'-10);
			n=n*16+(c-'0');
		}
	}else{
		for( ;i<t->length;++i ){
			int c=t->buf[i];
			if( !isdigit(c) ) break;
			n=n*10+(c-'0');
		}
	}
	return neg ? -n : n;
}

unsigned long long bmx_string_to_ulong( BBString *t ){
	int i=0,neg=0;
	unsigned long long n=0;
	
	while( i<t->length && isspace(t->buf[i]) ) ++i;
	if( i==t->length ){ return 0; }
	
	if( t->buf[i]=='+' ) ++i;
	else if( (neg = t->buf[i]=='-') ) ++i;
	if( i==t->length ){ return 0; }
	
	if( t->buf[i]=='%' ){
		for( ++i;i<t->length;++i ){
			int c=t->buf[i];
			if( c!='0' && c!='1' ) break;
			n=n*2+(c-'0');
		}
	}else if( t->buf[i]=='$' ){
		for( ++i;i<t->length;++i ){
			int c=toupper(t->buf[i]);
			if( !isxdigit(c) ) break;
			if( c>='A' ) c-=('A'-'0'-10);
			n=n*16+(c-'0');
		}
	}else{
		for( ;i<t->length;++i ){
			int c=t->buf[i];
			if( !isdigit(c) ) break;
			n=n*10+(c-'0');
		}
	}
	return neg ? -n : n;
}

size_t bmx_string_to_size_t( BBString *t ){
	int i=0,neg=0;
	size_t n=0;
	
	while( i<t->length && isspace(t->buf[i]) ) ++i;
	if( i==t->length ){ return 0; }
	
	if( t->buf[i]=='+' ) ++i;
	else if( (neg=(t->buf[i]=='-')) ) ++i;
	if( i==t->length ){ return 0; }
	
	if( t->buf[i]=='%' ){
		for( ++i;i<t->length;++i ){
			int c=t->buf[i];
			if( c!='0' && c!='1' ) break;
			n=n*2+(c-'0');
		}
	}else if( t->buf[i]=='$' ){
		for( ++i;i<t->length;++i ){
			int c=toupper(t->buf[i]);
			if( !isxdigit(c) ) break;
			if( c>='A' ) c-=('A'-'0'-10);
			n=n*16+(c-'0');
		}
	}else{
		for( ;i<t->length;++i ){
			int c=t->buf[i];
			if( !isdigit(c) ) break;
			n=n*10+(c-'0');
		}
	}
	return neg ? -n : n;
}

BBString *bmx_string_from_uint( unsigned int n ){
	char buf[64];
	sprintf(buf, "%u", n);
	return bbStringFromBytes( (unsigned char*)buf, strlen(buf) );
}

BBString *bmx_string_from_ulong( unsigned long long n ){
	char buf[64];
	sprintf(buf, "%llu", n);
	return bbStringFromBytes( (unsigned char*)buf, strlen(buf) );
}

BBString *bmx_string_from_size_t( size_t n ){
	char buf[64];
#if UINTPTR_MAX == 0xffffffff
	sprintf(buf, "%u", n);
#else
	sprintf(buf, "%llu", n);
#endif
	return bbStringFromBytes( (unsigned char*)buf, strlen(buf) );
}

BBString * bmx_bitwise_not_uint(BBString * value) {
	unsigned int v = bmx_string_to_uint(value);
	return bmx_string_from_uint(~v);
}

BBString * bmx_bitwise_not_sizet(BBString * value) {
	size_t v = bmx_string_to_size_t(value);
	return bmx_string_from_size_t(~v);
}

BBString * bmx_bitwise_not_ulong(BBString * value) {
	unsigned long long v = bmx_string_to_ulong(value);
	return bmx_string_from_ulong(~v);
}

BBString * bmx_bitwise_not_longint(BBString * value, int size) {
	if (size == 4) {
		int v = bbStringToInt(value);
		return bbStringFromInt(~v);
	} else { // 8
		BBInt64 v = bbStringToLong(value);
		return bbStringFromLong(~v);
	}
}

BBString * bmx_bitwise_not_ulongint(BBString * value, int size) {
	if (size == 4) {
		unsigned int v = bmx_string_to_uint(value);
		return bmx_string_from_uint(~v);
	} else { // 8
		unsigned long long v = bmx_string_to_ulong(value);
		return bmx_string_from_ulong(~v);
	}
}


BBString * bmx_binarymathexpr_sizet(enum binaryOps op, BBString * slhs, BBString * srhs) {
	size_t lhs = bmx_string_to_size_t(slhs);
	size_t rhs = bmx_string_to_size_t(srhs);
	size_t res = 0;
	switch (op) {
		case OP_MUL:
			res = lhs * rhs;
			break;
		case OP_DIV:
			res = lhs / rhs;
			break;
		case OP_MOD:
			res = lhs % rhs;
			break;
		case OP_SHL:
			res = lhs << rhs;
			break;
		case OP_SHR:
		case OP_SAR:
			res = lhs >> rhs;
			break;
		case OP_ADD:
			res = lhs + rhs;
			break;
		case OP_SUB:
			res = lhs - rhs;
			break;
		case OP_AND:
			res = lhs & rhs;
			break;
		case OP_XOR:
			res = lhs ^ rhs;
			break;
		case OP_OR:
			res = lhs | rhs;
			break;
	}
	return bmx_string_from_size_t(res);
}

BBString * bmx_binarymathexpr_uint(enum binaryOps op, BBString * slhs, BBString * srhs) {
	unsigned int lhs = bmx_string_to_uint(slhs);
	unsigned int rhs = bmx_string_to_uint(srhs);
	unsigned int res = 0;
	switch (op) {
		case OP_MUL:
			res = lhs * rhs;
			break;
		case OP_DIV:
			res = lhs / rhs;
			break;
		case OP_MOD:
			res = lhs % rhs;
			break;
		case OP_SHL:
			res = lhs << rhs;
			break;
		case OP_SHR:
		case OP_SAR:
			res = lhs >> rhs;
			break;
		case OP_ADD:
			res = lhs + rhs;
			break;
		case OP_SUB:
			res = lhs - rhs;
			break;
		case OP_AND:
			res = lhs & rhs;
			break;
		case OP_XOR:
			res = lhs ^ rhs;
			break;
		case OP_OR:
			res = lhs | rhs;
			break;
	}
	return bmx_string_from_uint(res);
}

BBString * bmx_binarymathexpr_ulong(enum binaryOps op, BBString * slhs, BBString * srhs) {
	unsigned long long lhs = bmx_string_to_ulong(slhs);
	unsigned long long rhs = bmx_string_to_ulong(srhs);
	unsigned long long res = 0;
	switch (op) {
		case OP_MUL:
			res = lhs * rhs;
			break;
		case OP_DIV:
			res = lhs / rhs;
			break;
		case OP_MOD:
			res = lhs % rhs;
			break;
		case OP_SHL:
			res = lhs << rhs;
			break;
		case OP_SHR:
		case OP_SAR:
			res = lhs >> rhs;
			break;
		case OP_ADD:
			res = lhs + rhs;
			break;
		case OP_SUB:
			res = lhs - rhs;
			break;
		case OP_AND:
			res = lhs & rhs;
			break;
		case OP_XOR:
			res = lhs ^ rhs;
			break;
		case OP_OR:
			res = lhs | rhs;
			break;
	}
	return bmx_string_from_ulong(res);
}

BBString * bmx_binarymathexpr_longint(enum binaryOps op, BBString * slhs, BBString * srhs, int size) {
	if (size == 4) {
		int lhs = bbStringToInt(slhs);
		int rhs = bbStringToInt(srhs);
		int res = 0;
		switch (op) {
			case OP_MUL:
				res = lhs * rhs;
				break;
			case OP_DIV:
				res = lhs / rhs;
				break;
			case OP_MOD:
				res = lhs % rhs;
				break;
			case OP_SHL:
				res = lhs << rhs;
				break;
			case OP_SHR:
			case OP_SAR:
				res = lhs >> rhs;
				break;
			case OP_ADD:
				res = lhs + rhs;
				break;
			case OP_SUB:
				res = lhs - rhs;
				break;
			case OP_AND:
				res = lhs & rhs;
				break;
			case OP_XOR:
				res = lhs ^ rhs;
				break;
			case OP_OR:
				res = lhs | rhs;
				break;
		}
		return bbStringFromInt(res);
	} else { // 8
		BBInt64 lhs = bbStringToLong(slhs);
		BBInt64 rhs = bbStringToLong(srhs);
		BBInt64 res = 0;
		switch (op) {
			case OP_MUL:
				res = lhs * rhs;
				break;
			case OP_DIV:
				res = lhs / rhs;
				break;
			case OP_MOD:
				res = lhs % rhs;
				break;
			case OP_SHL:
				res = lhs << rhs;
				break;
			case OP_SHR:
			case OP_SAR:
				res = lhs >> rhs;
				break;
			case OP_ADD:
				res = lhs + rhs;
				break;
			case OP_SUB:
				res = lhs - rhs;
				break;
			case OP_AND:
				res = lhs & rhs;
				break;
			case OP_XOR:
				res = lhs ^ rhs;
				break;
			case OP_OR:
				res = lhs | rhs;
				break;
		}
		return bbStringFromLong(res);
	}
}

BBString * bmx_binarymathexpr_ulongint(enum binaryOps op, BBString * slhs, BBString * srhs, int size) {
	if (size == 4) {
		return bmx_binarymathexpr_uint(op, slhs, srhs);
	} else { // 8
		return bmx_binarymathexpr_ulong(op, slhs, srhs);
	}
}
