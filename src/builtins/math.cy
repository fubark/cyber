--| Euler's number and the base of natural logarithms; approximately 2.718.
@host var Root.e float

--| Infinity.
@host var Root.inf float

--| Base-10 logarithm of E; approximately 0.434.
@host var Root.log10e float

--| Base-2 logarithm of E; approximately 1.443.
@host var Root.log2e float

--| Natural logarithm of 10; approximately 2.303.
@host var Root.ln10 float

--| Natural logarithm of 2; approximately 0.693.
@host var Root.ln2 float

--| The maximum integer value that can be safely represented as a float. 2^53-1 or 9007199254740991.
@host var Root.maxSafeInt float

--| The minumum integer value that can be safely represented as a float. -(2^53-1) or -9007199254740991.
@host var Root.minSafeInt float

--| Not a number. Note that nan == nan.
--| However, if a nan came from an arithmetic operation, the comparison is undefined.
--| Use `isNaN` instead.
@host var Root.nan float

--| Negative infinity.
@host var Root.neginf float

--| Ratio of a circle's circumference to its diameter; approximately 3.14159.
@host var Root.pi float

--| Square root of ½; approximately 0.707.
@host var Root.sqrt1_2 float

--| Square root of 2; approximately 1.414.
@host var Root.sqrt2 float

--| Returns the absolute value of x.
@host func abs(a float) float

--| Returns the arccosine of x.
@host func acos(a float) float

--| Returns the hyperbolic arccosine of x.
@host func acosh(a float) float

--| Returns the arcsine of x.
@host func asin(a float) float

--| Returns the hyperbolic arcsine of a number.
@host func asinh(a float) float

--| Returns the arctangent of x.
@host func atan(a float) float

--| Returns the arctangent of the quotient of its arguments.
@host func atan2(a float, b float) float

--| Returns the hyperbolic arctangent of x.
@host func atanh(a float) float

--| Returns the cube root of x.
@host func cbrt(a float) float

--| Returns the smallest integer greater than or equal to x.
@host func ceil(a float) float

--| Returns the number of leading zero bits of the 32-bit integer x.
@host func clz32(a float) float

--| Returns the cosine of x.
@host func cos(a float) float

--| Returns the hyperbolic cosine of x.
@host func cosh(a float) float

--| Returns e^x, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm).
@host func exp(a float) float

--| Returns subtracting 1 from exp(x).
@host func expm1(a float) float

--| Returns the largest integer less than or equal to x.
@host func floor(a float) float

--| Returns the fractional or decimal part of a float value.
@host func frac(a float) float

--| Returns the square root of the sum of squares of its arguments.
@host func hypot(a float, b float) float

--| Returns true if the float has no fractional part, otherwise false.
@host func isInt(a float) bool

--| Returns whether x is not a number.
@host func isNaN(a float) bool

--| Returns the natural logarithm (㏒e; also, ㏑) of x.
@host func ln(a float) float

--| Returns the logarithm of y with base x.
@host func log(a float, b float) float

--| Returns the base-10 logarithm of x.
@host func log10(a float) float

--| Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x.
@host func log1p(a float) float

--| Returns the base-2 logarithm of x.
@host func log2(a float) float

--| Returns the largest of two numbers.
@host func max(a float, b float) float   

--| Returns the smallest of two numbers.
@host func min(a float, b float) float   

--| Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed.
@host func mul32(a float, b float) float 

--| Returns base x to the exponent power y (that is, x^y).
@host func pow(a float, b float) float   

--| Returns a pseudo-random number between 0 and 1.
@host func random() float

--| Returns the value of the number x rounded to the nearest integer.
@host func round(a float) float

--| Returns the sign of the x, indicating whether x is positive, negative, or zero.
@host func sign(a float) float

--| Returns the sine of x.
@host func sin(a float) float

--| Returns the hyperbolic sine of x.
@host func sinh(a float) float

--| Returns the positive square root of x.
@host func sqrt(a float) float

--| Returns the tangent of x.
@host func tan(a float) float

--| Returns the hyperbolic tangent of x.
@host func tanh(a float) float

--| Returns the integer portion of x, removing any fractional digits.
@host func trunc(a float) float