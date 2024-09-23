--| Euler's number and the base of natural logarithms; approximately 2.718.
@host var .e float

--| Infinity.
@host var .inf float

--| Base-10 logarithm of E; approximately 0.434.
@host var .log10e float

--| Base-2 logarithm of E; approximately 1.443.
@host var .log2e float

--| Natural logarithm of 10; approximately 2.303.
@host var .ln10 float

--| Natural logarithm of 2; approximately 0.693.
@host var .ln2 float

--| The maximum integer value that can be safely represented as a float. 2^53-1 or 9007199254740991.
@host var .maxSafeInt float

--| The minumum integer value that can be safely represented as a float. -(2^53-1) or -9007199254740991.
@host var .minSafeInt float

--| Not a number. Note that nan == nan.
--| However, if a nan came from an arithmetic operation, the comparison is undefined.
--| Use `isNaN` instead.
@host var .nan float

--| Negative infinity.
@host var .neginf float

--| Ratio of a circle's circumference to its diameter; approximately 3.14159.
@host var .pi float

--| Square root of ½; approximately 0.707.
@host var .sqrt1_2 float

--| Square root of 2; approximately 1.414.
@host var .sqrt2 float

--| Returns the absolute value of x.
@host fn abs(a float) float

--| Returns the arccosine of x.
@host fn acos(a float) float

--| Returns the hyperbolic arccosine of x.
@host fn acosh(a float) float

--| Returns the arcsine of x.
@host fn asin(a float) float

--| Returns the hyperbolic arcsine of a number.
@host fn asinh(a float) float

--| Returns the arctangent of x.
@host fn atan(a float) float

--| Returns the arctangent of the quotient of its arguments.
@host fn atan2(a float, b float) float

--| Returns the hyperbolic arctangent of x.
@host fn atanh(a float) float

--| Returns the cube root of x.
@host fn cbrt(a float) float

--| Returns the smallest integer greater than or equal to x.
@host fn ceil(a float) float

--| Returns the number of leading zero bits of the 32-bit integer x.
@host fn clz32(a float) float

--| Returns the cosine of x.
@host fn cos(a float) float

--| Returns the hyperbolic cosine of x.
@host fn cosh(a float) float

--| Returns e^x, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm).
@host fn exp(a float) float

--| Returns subtracting 1 from exp(x).
@host fn expm1(a float) float

--| Returns the largest integer less than or equal to x.
@host fn floor(a float) float

--| Returns the fractional or decimal part of a float value.
@host fn frac(a float) float

--| Returns the square root of the sum of squares of its arguments.
@host fn hypot(a float, b float) float

--| Returns true if the float has no fractional part, otherwise false.
@host fn isInt(a float) bool

--| Returns whether x is not a number.
@host fn isNaN(a float) bool

--| Returns the natural logarithm (㏒e; also, ㏑) of x.
@host fn ln(a float) float

--| Returns the logarithm of y with base x.
@host fn log(a float, b float) float

--| Returns the base-10 logarithm of x.
@host fn log10(a float) float

--| Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x.
@host fn log1p(a float) float

--| Returns the base-2 logarithm of x.
@host fn log2(a float) float

--| Returns the largest of two numbers.
@host fn max(a float, b float) float   

--| Returns the smallest of two numbers.
@host fn min(a float, b float) float   

--| Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed.
@host fn mul32(a float, b float) float 

--| Returns base x to the exponent power y (that is, x^y).
@host fn pow(a float, b float) float   

--| Returns a pseudo-random number between 0 and 1.
@host fn random() float

--| Returns the value of the number x rounded to the nearest integer.
@host fn round(a float) float

--| Returns the sign of the x, indicating whether x is positive, negative, or zero.
@host fn sign(a float) float

--| Returns the sine of x.
@host fn sin(a float) float

--| Returns the hyperbolic sine of x.
@host fn sinh(a float) float

--| Returns the positive square root of x.
@host fn sqrt(a float) float

--| Returns the tangent of x.
@host fn tan(a float) float

--| Returns the hyperbolic tangent of x.
@host fn tanh(a float) float

--| Returns the integer portion of x, removing any fractional digits.
@host fn trunc(a float) float