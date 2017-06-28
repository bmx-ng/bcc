' Copyright (c) 2008-2017 Bruce A Henderson
' 
' Permission is hereby granted, free of charge, to any person obtaining a copy
' of this software and associated documentation files (the "Software"), to deal
' in the Software without restriction, including without limitation the rights
' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
' copies of the Software, and to permit persons to whom the Software is
' furnished to do so, subject to the following conditions:
' 
' The above copyright notice and this permission notice shall be included in
' all copies or substantial portions of the Software.
' 
' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
' THE SOFTWARE.
' 
SuperStrict

Import "src/*.h"

Import "src/mapmhasn.c"
Import "src/mapmhsin.c"
Import "src/mapm_pow.c"
Import "src/mapm_log.c"
Import "src/mapm_lg2.c"
Import "src/mapm_lg4.c"
Import "src/mapm_exp.c"
Import "src/mapm_lg3.c"
Import "src/mapmasin.c"
Import "src/mapmasn0.c"
Import "src/mapm_sin.c"
Import "src/mapm5sin.c"
Import "src/mapmrsin.c"
Import "src/mapm_cpi.c"
Import "src/mapmsqrt.c"
Import "src/mapmcbrt.c"
Import "src/mapmgues.c"
Import "src/mapmfact.c"
Import "src/mapm_gcd.c"
Import "src/mapmipwr.c"
Import "src/mapmpwr2.c"
Import "src/mapm_rnd.c"
Import "src/mapm_flr.c"
Import "src/mapm_fpf.c"
Import "src/mapm_rcp.c"
Import "src/mapmstck.c"
Import "src/mapm_div.c"
Import "src/mapm_mul.c"
Import "src/mapmfmul.c"
Import "src/mapm_fft.c"
Import "src/mapm_add.c"
Import "src/mapmistr.c"
Import "src/mapm_set.c"
Import "src/mapm_fam.c"
Import "src/mapmutil.c"
Import "src/mapmutl2.c"
Import "src/mapmutl1.c"
Import "src/mapmcnst.c"

Extern

	Function m_apm_init:Byte Ptr()
	Function m_apm_free(handle:Byte Ptr)

	Function m_apm_set_string(handle:Byte Ptr, value:Byte Ptr)
	Function m_apm_set_long(handle:Byte Ptr, value:Int)
	Function m_apm_set_double(handle:Byte Ptr, value:Double)

	Function m_apm_to_string(buf:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_to_fixpt_string(buf:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_to_integer_string(buf:Byte Ptr, handle:Byte Ptr)
	Function m_apm_to_fixpt_stringex(buf:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr, radix:Byte, separator:Byte, separatorCount:Int)


	Function m_apm_absolute_value(mapm:Byte Ptr, handle:Byte Ptr)
	Function m_apm_negate(mapm:Byte Ptr, handle:Byte Ptr)
	Function m_apm_copy(mapm:Byte Ptr, handle:Byte Ptr)

	Function m_apm_round(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_compare:Int(handle:Byte Ptr, mapmPtr:Byte Ptr)
	Function m_apm_sign:Int(handle:Byte Ptr)
	Function m_apm_exponent:Int(handle:Byte Ptr)
	Function m_apm_significant_digits:Int(handle:Byte Ptr)
	Function m_apm_is_integer:Int(handle:Byte Ptr)
	Function m_apm_is_even:Int(handle:Byte Ptr)
	Function m_apm_is_odd:Int(handle:Byte Ptr)

	Function m_apm_set_random_seed(seed:Byte Ptr)
	Function m_apm_get_random(mapm:Byte Ptr)
	Function m_apm_add(mapm:Byte Ptr, handle:Byte Ptr, value:Byte Ptr)
	Function m_apm_subtract(mapm:Byte Ptr, handle:Byte Ptr, value:Byte Ptr)
	Function m_apm_multiply(mapm:Byte Ptr, handle:Byte Ptr, value:Byte Ptr)

	Function m_apm_divide(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr, value:Byte Ptr)
	Function m_apm_reciprocal(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_integer_divide(mapm:Byte Ptr, handle:Byte Ptr, value:Byte Ptr)
	Function m_apm_integer_div_rem(quotient:Byte Ptr, remainder:Byte Ptr, handle:Byte Ptr, value:Byte Ptr)
	Function m_apm_factorial(mapm:Byte Ptr, handle:Byte Ptr)
	Function m_apm_floor(mapm:Byte Ptr, handle:Byte Ptr)
	Function m_apm_ceil(mapm:Byte Ptr, handle:Byte Ptr)
	Function m_apm_gcd(mapm:Byte Ptr, handle:Byte Ptr, value:Byte Ptr)
	Function m_apm_lcm(mapm:Byte Ptr, handle:Byte Ptr, value:Byte Ptr)
	Function m_apm_sqrt(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_cbrt(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_log(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_log10(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_exp(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_pow(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr, power:Byte Ptr)
	Function m_apm_integer_pow(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr, power:Int)
	Function m_apm_integer_pow_nr(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr, power:Int)

	Function m_apm_sin(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_cos(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_sin_cos(_sin:Byte Ptr, _cos:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_tan(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_arcsin(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_arccos(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_arctan(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_arctan2(mapm:Byte Ptr, decimalPlaces:Int, y:Byte Ptr, x:Byte Ptr)
	Function m_apm_sinh(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_cosh(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_tanh(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_arcsinh(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_arccosh(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)
	Function m_apm_arctanh(mapm:Byte Ptr, decimalPlaces:Int, handle:Byte Ptr)

End Extern


