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

Import "common.bmx"

Rem
bbdoc: Maximum number of digits when converting to string.
End Rem
Global MAPM_MAX_DIGITS:Int = 8192

Rem
bbdoc: A numeric type for very large numbers.
End Rem
Type TMAPM

	Field mapmPtr:Byte Ptr
	
	Rem
	bbdoc: Creates a new MAPM object.
	End Rem
	Method New()
		mapmPtr = m_apm_init()
	End Method
	
	Rem
	bbdoc: Creates a new MAPM object setting it to the optional @value.
	End Rem
	Function CreateMAPM:TMAPM(value:String = Null)
		Return New TMAPM.Create(value)
	End Function
	
	Rem
	bbdoc: Creates a new MAPM object setting it to the optional @value.
	End Rem
	Method Create:TMAPM(value:String = Null)
		If value Then
			SetString(value)
		End If
		Return Self
	End Method
	
	Function _create:TMAPM(mapmPtr:Byte Ptr)
		If mapmPtr Then
			Local this:TMAPM = New TMAPM
			this.mapmPtr = mapmPtr
			Return this
		End If
	End Function


	Rem
	bbdoc: Sets the MAPM value to the value specified by the string variable.
	about: Integers and floating point are supported as is floating point with scientific notation.
	<ul>
	<li>Lead-in whitespace is ignored.</li>
	<li>A lead-in '+' sign is optional.</li>
	<li>A negative number must have '-' as the first non-whitespace char</li>
	<li>An exponent, 'E' or 'e', is optional.</li>
	<li>The decimal point is optional. The decimal point may be anywhere in the number, but before the exponent.</li>
	<li>The exponent may have an optional '+' sign.</li>
	<li>The exponent must be an integer value (no decimal point)</li>
	</ul>
	End Rem
	Method SetString(value:String)
		m_apm_set_string(mapmPtr, value)
	End Method
		
	Rem
	bbdoc: Sets the MAPM value to the value specified by the int variable.
	End Rem
	Method SetInt(value:Int)
		m_apm_set_long(mapmPtr, value)
	End Method
	
	Rem
	bbdoc: Sets the MAPM value to the value specified by the double variable.
	about: The double value will be rounded to the 15 most significant digits and then converted
	to the MAPM value. If you want an 'exact' conversion, use the SetString method since some
	C floating point library's may round your double in an unpredictable manner.
	End Rem
	Method SetDouble(value:Double)
		m_apm_set_double(mapmPtr, value)
	End Method
	
	Rem
	bbdoc: Converts an MAPM value into a string and is meant to be used with floating point MAPM values.
	about: The output string will always be in scientific (exponential) notation. There will
	be a leading '-' sign for negative numbers. There will be 'decimal_places' number of digits
	after the decimal point. If decimal_places is &gt;= 0, the value will be rounded to that number
	of digits and then the string will be filled, with trailing zero's appended if necessary to
	fill out the decimal place specification. If decimal_places &lt; 0, ALL the significant digits
	of the MAPM number will be output. In some applications, it is convienent to round the value
	yourself (see 'm_apm_round') and then display ALL the digits.
	<p>
	If value = 3.640083E-4
	<pre>
            1)  ToExpString(4)
	        string -&gt; "3.6401E-4"

            2)  ToExpString(14)
	        string -&gt; "3.64008300000000E-4"

            3)  ToExpString(-1)
	        string -&gt; "3.640083E-4"
	</pre>
	</p>
	End Rem
	Method ToExpString:String(decimalPlaces:Int)
		Local buf:Byte[MAPM_MAX_DIGITS + decimalPlaces]
		m_apm_to_string(buf, decimalPlaces, mapmPtr)
		Return String.FromCString(buf)
	End Method
	
	Rem
	bbdoc: Converts a MAPM value into a string and the output will be formatted in fixed point notation.
	about: The output string must be large enough to hold the result.
	<p>
	    If decimal_places &lt; 0, ALL the significant digits of the MAPM
	    number will be output.
	</p>
	<p>
	    If decimal_places = 0, the output will be the MAPM value rounded
	    to the nearest integer and the decimal point will be suppressed.
	</p>
	<p>
	    If decimal_places is &gt; 0, the value will be rounded to that number
	    of digits and then the string will be filled, with trailing zero's
	    appended if necessary to fill out the decimal place specification.
	</p>
	<p>
	    In some applications, it is convienent to round the value yourself
	    (see 'Round()') and then display ALL the digits.
	</p>
	<pre>
	    If value is = 3.6487451E+2 :

	    1)  ToFixPtString(10)
	        string -&gt; "364.8745100000"

	    2)  ToFixPtString(1)
	        string -&gt; "364.9"

	    3)  ToFixPtString(0)
	        string -&gt; "365"

	    4)  ToFixPtString(-1)
	        string -&gt; "364.87451"
	</pre>
	End Rem
	Method ToFixPtString:String(decimalPlaces:Int)
		Local buf:Byte[MAPM_MAX_DIGITS + SignificantDigits()]
		m_apm_to_fixpt_string(buf, decimalPlaces, mapmPtr)
		Return String.FromCString(buf)
	End Method
	
	Rem
	bbdoc: Converts a MAPM value into a string, outputting all significant digits.
	about: This is equivalent to  ToFixPtString(-1)
	End Rem
	Method ToString:String()
		Return ToFixPtString(-1)
	End Method

	Rem
	bbdoc: This method is an extended version of ToFixPtString which includes 3 additional function parameters:
	about:
	<ul>
	<li>@radix -  Specify the radix character desired. For example, use ',' to set the radix char to a comma.</li>
	<li> @separator and @separatorCount - Specify a character separator every 'separator_count' characters.
	    This is used to split up a large number with a 'delimiter' for easier readability. For example,
		<p>
	    If separator_char = ',' and separator_count = 3, there will be a
	    comma inserted before every group of 3 digits in the output string.
		</p>
	<p>
	    6123456789.098765321 will be formatted as "6,123,456,789.098765321"
	</p>
	</li>
	</ul>
	<p>
	    Note that only digits before the radix char are separated.
	</p>
	<p>
	    @separator OR @separatorCount = 0 is used to disable
	    the 'char separator' feature. This would typically be used
	    when it is only desired to change the radix character.
	</p>
	End Rem
	Method ToFixPtStringEx:String(decimalPlaces:Int, radix:String, separator:String, separatorCount:Int)
		Local buf:Byte[MAPM_MAX_DIGITS + SignificantDigits()]
		m_apm_to_fixpt_stringex(buf, decimalPlaces, mapmPtr, Byte(radix[0]), Byte(separator[0]), separatorCount)
		Return String.FromCString(buf)
	End Method
	
	Rem
	bbdoc: Converts an MAPM value into a string and is meant to be used with integer values.
	about: If the MAPM number is not
	    an integer, the function will truncate the value to the nearest
	    integer and the output will be formatted as an integer, with a
	    possible leading '-' sign.
	<p>
	Examples:
	<pre>
	    MAPM Value            Output String
	    -----------            -------------
	    3.28E+2                "328"
	    -4.56993E+2            "-456"
	    4.32E-3                "0"
	    -1.62E+5               "-162000"
	</pre>
	    If you want the value 'rounded' to the nearest integer (NNN.99
	    becomes NNN+1), use ToFixPtString with 0 decimal places.
	</p>
	End Rem
	Method ToIntString:String()
		Local buf:Byte[MAPM_MAX_DIGITS + SignificantDigits()]
		m_apm_to_integer_string(buf, mapmPtr)
		Return String.FromCString(buf)
	End Method
	
	Rem
	bbdoc: Returns the absolute MAPM value.
	End Rem
	Method AbsoluteValue:TMAPM()
		Local mapm:TMAPM = New TMAPM
		m_apm_absolute_value(mapm.mapmPtr, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns the MAPM value, negated.
	End Rem
	Method Negate:TMAPM()
		Local mapm:TMAPM = New TMAPM
		m_apm_negate(mapm.mapmPtr, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns a copy of the number.
	End Rem
	Method Copy:TMAPM()
		Local mapm:TMAPM = New TMAPM
		m_apm_copy(mapm.mapmPtr, mapmPtr)
		Return mapm
	End Method

	Rem
	bbdoc: Rounds the value of the number to the number of decimal places specified.
	about: The decimal places parameter is referenced to the number when the
	    number is in scientific notation.
	End Rem
	Method Round:TMAPM(decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_round(mapm.mapmPtr, decimalPlaces, mapmPtr)
		Return mapm
	End Method
		
	Rem
	bbdoc: Compares the number to @other.
	about: The method will return :
	<pre>
	    -1 : num &lt; other
	     0 : num = other
	     1 : num &gt; other
	</pre>
	End Rem
	Method Compare:Int(other:Object)
		If TMAPM(other) Then
			Return m_apm_compare(mapmPtr, TMAPM(other).mapmPtr)
		End If
		Return -1
	End Method
	
	Rem
	bbdoc: Returns the sign of the number.
	about: The method will return :
	<pre>
	    -1 : num < 0
	     0 : num = 0
	     1 : num > 0
	</pre>

	End Rem
	Method Sign:Int()
		Return m_apm_sign(mapmPtr)
	End Method
	
	Rem
	bbdoc: Returns the exponent of the number.
	about: <pre>
     If apm_num = 3.86742E+12,    12 will be returned.
                = 9.61082E-56,   -56 will be returned.
                = 0.0              0 will be returned.
	</pre>
	End Rem
	Method Exponent:Int()
		Return m_apm_exponent(mapmPtr)
	End Method
	
	Rem
	bbdoc: Returns the number of significant digits of the number.
	End Rem
	Method SignificantDigits:Int()
		Return m_apm_significant_digits(mapmPtr)
	End Method
	
	Rem
	bbdoc: Returns True if the number is an integer, False otherwise.
	End Rem
	Method IsInteger:Int()
		Return m_apm_is_integer(mapmPtr)
	End Method
	
	Rem
	bbdoc: Returns True if the number is even, False otherwise.
	about: It the number is not an integer, it will result in a warning on stderr and the
	return value is undefined.
	End Rem
	Method IsEven:Int()
		Return m_apm_is_even(mapmPtr)
	End Method
	
	Rem
	bbdoc: Returns True if the number is odd, False otherwise.
	about: It the number is not an integer, it will result in a warning on stderr and the
	return value is undefined.
	End Rem
	Method IsOdd:Int()
		Return m_apm_is_odd(mapmPtr)
	End Method
	
	Rem
	bbdoc: Sets the randon number generator to a known starting value.
	about: The argument should correspond to any *integer* value between 0 and (1.0E+15 - 1)
	End Rem
	Function SetRandomSeed(seed:String)
		m_apm_set_random_seed(seed)
	End Function
	
	Rem
	bbdoc: Returns a random floating point number between the values 0 and 1.
	about: The first time the function is called the generator is initialized with the system
	time. This generator will not repeat its pattern until 1.0E+15 numbers have been generated.
	End Rem
	Function Random:TMAPM()
		Local mapm:TMAPM = New TMAPM
		m_apm_get_random(mapm.mapmPtr)
		Return mapm
	End Function
	
	Rem
	bbdoc: Adds @value to the number, returning the result.
	End Rem
	Method Add:TMAPM(value:TMAPM)
		Local mapm:TMAPM = New TMAPM
		m_apm_add(mapm.mapmPtr, mapmPtr, value.mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Subtracts @value from the number, returning the result.
	End Rem
	Method Subtract:TMAPM(value:TMAPM)
		Local mapm:TMAPM = New TMAPM
		m_apm_subtract(mapm.mapmPtr, mapmPtr, value.mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Multiplies @value with the number, returning the result.
	End Rem
	Method Multiply:TMAPM(value:TMAPM)
		Local mapm:TMAPM = New TMAPM
		m_apm_multiply(mapm.mapmPtr, mapmPtr, value.mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Divides the number by @value.
	about: Unlike the other three basic operations, division cannot be
	    counted on to produce non-repeating decimals, so the
	    @decimalPlaces parameter is used to tell this routine how many
            digits are to be calculated before stopping.
	<p>
	    Note that the number of decimal places is referenced to the
	    value as if the number was in fixed point notation. Divide
	    is the only method where decimal places is referenced to
	    fixed point notation, all other methods are referenced to
	    scientific notation. This was an intentional design decision
	    so IntegerDivide' and IntegerDivRem would
	    work as expected.
	</p>
	<p>
            Division by zero creates a zero result and a warning on stderr.
	</p>

	End Rem
	Method Divide:TMAPM(value:TMAPM, decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_divide(mapm.mapmPtr, decimalPlaces, mapmPtr, value.mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns the reciprocal of the number (compute 1.0 / number).
	about: The result will be accurate to the number of decimal places specified.
	<p>
     An input of zero creates a zero result and a warning on stderr.
	</p>
	End Rem
	Method Reciprocal:TMAPM(decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_reciprocal(mapm.mapmPtr, decimalPlaces, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Divides the number by @value, truncating the result to an integer.
	End Rem
	Method IntegerDivide:TMAPM(value:TMAPM)
		Local mapm:TMAPM = New TMAPM
		m_apm_integer_divide(mapm.mapmPtr, mapmPtr, value.mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Divides the number by @value, truncating the result to an integer and putting the result in @quotient and the remainder in @remainder.
	about: So, 173 / 26 will yield a quotient of 6 and a remainder of 17.
	<p>
	Note that the input numbers do not necessarily have to be
	    integers. This method can be used to split up the integer
	    portion and fractional portion of a floating point number
	    by calling the function with @value set to 'MM_One'. So,
	    32.17042 can be split up into '32' and '0.17042'.
	</p>
	End Rem
	Method IntegerDivRem(value:TMAPM, quotient:TMAPM, remainder:TMAPM)
		m_apm_integer_div_rem(quotient.mapmPtr, remainder.mapmPtr, mapmPtr, value.mapmPtr)
	End Method
	
	Rem
	bbdoc: Computes the factorial of the number and returns the result.
	about: A non-integer number will yield nonsense. Actually, the algorithm simply multiplies
	 : (though 0! and 1! return 1)
	<pre>
	    N * (N - 1) * (N - 2) ... until N < 2.
	</pre>
	End Rem
	Method Factorial:TMAPM()
		Local mapm:TMAPM = New TMAPM
		m_apm_factorial(mapm.mapmPtr, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns the number rounded downwards to the nearest integer.
	End Rem
	Method Floor:TMAPM()
		Local mapm:TMAPM = New TMAPM
		m_apm_floor(mapm.mapmPtr, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns the number rounded upwards to the nearest integer.
	End Rem
	Method Ceil:TMAPM()
		Local mapm:TMAPM = New TMAPM
		m_apm_ceil(mapm.mapmPtr, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Computes the GCD (greatest common divisor) of this number and @value.
	End Rem
	Method GCD:TMAPM(value:TMAPM)
		Local mapm:TMAPM = New TMAPM
		m_apm_gcd(mapm.mapmPtr, mapmPtr, value.mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Computes the LCM (least common multi) of this number and @value.
	End Rem
	Method LCM:TMAPM(value:TMAPM)
		Local mapm:TMAPM = New TMAPM
		m_apm_lcm(mapm.mapmPtr, mapmPtr, value.mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns the square root of the number.
	about: The result will be accurate to the number of decimal places specified.
	End Rem
	Method Sqrt:TMAPM(decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_sqrt(mapm.mapmPtr, decimalPlaces, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns the cube root of the number.
	about: The result will be accurate to the number of decimal places specified.
	End Rem
	Method Cbrt:TMAPM(decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_cbrt(mapm.mapmPtr, decimalPlaces, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns the natural log (base 2.718 ...) of the number.
	about: The result will be accurate to the number of decimal places specified.
	End Rem
	Method Log:TMAPM(decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_log(mapm.mapmPtr, decimalPlaces, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns the common log (base 10) of the number.
	about: The result will be accurate to the number of decimal places specified.
	End Rem
	Method Log10:TMAPM(decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_log10(mapm.mapmPtr, decimalPlaces, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Performs E ^ the number where 'E' is 2.718... (the exponential function).
	about: The result will be accurate to the number of decimal places specified.
	End Rem
	Method Exp:TMAPM(decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_exp(mapm.mapmPtr, decimalPlaces, mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Raises the number to @power.
	about: The result will be accurate to the number of decimal places specified.
	End Rem
	Method Pow:TMAPM(power:TMAPM, decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_pow(mapm.mapmPtr, decimalPlaces, mapmPtr, power.mapmPtr)
		Return mapm
	End Method
	
	Rem
	bbdoc: Raises the number to @power.
	about: The result will be accurate to the number of decimal places specified.
	<p>
	    This method is considerably faster than the
	    generic Pow method (when @power is not excessively
	    large). The number and/or @power may be negative.
	</p>
	<p>
	    See the IntPowNr for a @Pow method that does not
	    perform any rounding operation and is more appropriate for
	    integer only applications.
	</p>
	End Rem
	Method IntPow:TMAPM(power:Int, decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_integer_pow(mapm.mapmPtr, decimalPlaces, mapmPtr, power)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns the number raised to @power.
	about: This method is similiar to IntPow except the result is NOT ROUNDED (Nr). This
	method would typically be used in an integer only application where the full precision
	of the result is desired.
	<p>
	Note that @power is an integer and not a MAPM number.
	</p>
	<p>
    @power must be >= zero. @power < 0 creates a zero result and a warning on stderr.
	</p>
	End Rem
	Method IntPowNr:TMAPM(power:Int, decimalPlaces:Int)
		Local mapm:TMAPM = New TMAPM
		m_apm_integer_pow_nr(mapm.mapmPtr, decimalPlaces, mapmPtr, power)
		Return mapm
	End Method
	
	Rem
	bbdoc: Returns the remainder of the number divided by @divisor.
	End Rem
	Method Modulo:TMAPM(divisor:TMAPM)
		Local a:TMAPM = IntegerDivide(divisor)
		Local b:TMAPM = a.Multiply(divisor)
		Return Subtract(b)
	End Method

	Method Delete()
		If mapmPtr Then
			m_apm_free(mapmPtr)
			mapmPtr = Null
		End If
	End Method

End Type

Global MM_MaxByte:TMAPM = TMAPM.CreateMAPM("255")
Global MM_MaxByteP1:TMAPM = TMAPM.CreateMAPM("256")
Global MM_MaxByteMod:TMAPM = TMAPM.CreateMAPM("256")

Global MM_MaxShort:TMAPM = TMAPM.CreateMAPM("65535")
Global MM_MaxShortP1:TMAPM = TMAPM.CreateMAPM("65536")
Global MM_MaxShortMod:TMAPM = TMAPM.CreateMAPM("65536")

Global MM_MaxInt:TMAPM = TMAPM.CreateMAPM("2147483647")
Global MM_MinInt:TMAPM = TMAPM.CreateMAPM("-2147483648")
Global MM_MaxIntNeg:TMAPM = TMAPM.CreateMAPM("-4294967296")
Global MM_MinIntPos:TMAPM = TMAPM.CreateMAPM("4294967294")

Global MM_MaxLong:TMAPM = TMAPM.CreateMAPM("9223372036854775807")
Global MM_MinLong:TMAPM = TMAPM.CreateMAPM("-9223372036854775808")
Global MM_MaxLongNeg:TMAPM = TMAPM.CreateMAPM("-18446744073709551616")
Global MM_MinLongPos:TMAPM = TMAPM.CreateMAPM("18446744073709551615")

Global MM_MaxUInt:TMAPM = TMAPM.CreateMAPM("4294967295")
Global MM_MaxUIntP1:TMAPM = TMAPM.CreateMAPM("4294967296")
Global MM_MaxUIntMod:TMAPM = TMAPM.CreateMAPM("4294967296")

Global MM_MaxULong:TMAPM = TMAPM.CreateMAPM("18446744073709551615")
Global MM_MaxULongP1:TMAPM = TMAPM.CreateMAPM("18446744073709551616")
Global MM_MaxULongMod:TMAPM = TMAPM.CreateMAPM("18446744073709551616")


Global MM_Zero:TMAPM = TMAPM.CreateMAPM()
Global MM_One:TMAPM = TMAPM.CreateMAPM("1")
Global MM_Two:TMAPM = TMAPM.CreateMAPM("2")
Global MM_Sixteen:TMAPM = TMAPM.CreateMAPM("16")

Rem
Global RadToDeg:TMAPM = TMAPM.CreateMAPM("57.29577951308232087679815481410517033240547246656432154916024386120284714832155263244096899585111094418622338163286489328144826460124831503606826786341194212252638809746726792630798870289311076793826144263826315820961046048702050644425965684112017191205773856628043128496262420337618793729762387079034031598071962408952204518620545992339631484190696622011512660969180151478763736692316410712677403851469016549959419251571198647943521066162438903520230675617779675711331568350620573131336015650134889801878870991777643918273")
Global DegToRad:TMAPM = TMAPM.CreateMAPM("0.017453292519943295769236907684886127134428718885417254560971914401710091146034494436822415696345094822123044925073790592483854692275281012398474218934047117319168245015010769561697553581238605305168788691271172087032963589602642490187704350918173343939698047594019224158946968481378963297818112495229298469927814479531045416008449560904606967176196468710514390888951836280826780369563245260844119508941294762613143108844183845478429899625621072806214155969235444237497596399365292916062377434350066384054631518680225870239")

Global MM_Two:TMAPM = TMAPM.CreateMAPM("2")
Global MM_Three:TMAPM = TMAPM.CreateMAPM("3")
Global MM_Four:TMAPM = TMAPM.CreateMAPM("4")
Global MM_Five:TMAPM = TMAPM.CreateMAPM("5")
Global MM_Ten:TMAPM = TMAPM.CreateMAPM("10")

Global MM_PI:TMAPM = TMAPM.CreateMAPM("3.1415926535897932384626433832795028841971693993751058209749445923078" + ..
	"164062862089986280348253421170679821480865132823066470938446095505822" + ..
	"317253594081284811174502841027019385211055596446229489549303819644288" + ..
	"109756659334461284756482337867831652712019091456485669234603486104543" + ..
	"266482133936072602491412737245870066063155881748815209209628292540917" + ..
	"153643678925903600113305305488204665213841469519415116094330572703657" + ..
	"595919530921861173819326117931051185480744623799627495673518857527248" + ..
	"91227938183011949129833673362440656643")
End Rem
