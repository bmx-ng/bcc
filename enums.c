
#include <math.h>
#include <stdio.h>

void bmx_enum_next_power(int t, long long * val, unsigned long long * ret) {
	unsigned long long result = pow(2.0, ceil(log(*val)/log(2))) + 0.5;

	switch (t) {
		case 'b':
			*ret = result < 0xffLLU ? result : 0;
			break;	
		case 's':
			*ret  = result < 0xffffLLU ? result : 0;
			break;	
		case 'i':
		case 'u':
			*ret = result < 0xffffffffLLU ? result : 0;
			break;	
		case 'l':
		case 'y':
			*ret = result < 0xffffffffffffffffLLU ? result : 0;
			break;	
		case 'z':
			if (sizeof(size_t) == 8) {
				*ret = result < 0xffffffffLLU ? result : 0;
			} else {
				*ret = result < 0xffffffffffffffffLLU ? result : 0;
			}
			break;	
	}
}

