-- Euler's number and the base of natural logarithms; approximately 2.718.
@host var e float

-- Infinity.
@host var inf float

-- Base-10 logarithm of E; approximately 0.434.
@host var log10e float

-- Base-2 logarithm of E; approximately 1.443.
@host var log2e float

-- Natural logarithm of 10; approximately 2.303.
@host var ln10 float

-- Natural logarithm of 2; approximately 0.693.
@host var ln2 float

-- Not a number.
@host var nan float

-- Neg infinity.
@host var neginf float

-- Ratio of a circle's circumference to its diameter; approximately 3.14159.
@host var pi float

-- Square root of ½; approximately 0.707.
@host var sqrt1_2 float

-- Square root of 2; approximately 1.414.
@host var sqrt2 float

@host func abs(a float) float
@host func acos(a float) float
@host func acosh(a float) float
@host func asin(a float) float
@host func asinh(a float) float
@host func atan(a float) float
@host func atan2(a float, b float) float
@host func atanh(a float) float
@host func cbrt(a float) float
@host func ceil(a float) float
@host func clz32(a float) float
@host func cos(a float) float
@host func cosh(a float) float
@host func exp(a float) float
@host func expm1(a float) float
@host func floor(a float) float
@host func hypot(a float, b float) float
@host func isNaN(a float) boolean
@host func ln(a float) float
@host func log(a float, b float) float
@host func log10(a float) float
@host func log1p(a float) float
@host func log2(a float) float
@host func max(a float, b float) float   
@host func min(a float, b float) float   
@host func mul32(a float, b float) float 
@host func pow(a float, b float) float   
@host func random() float
@host func round(a float) float
@host func sign(a float) float
@host func sin(a float) float
@host func sinh(a float) float
@host func sqrt(a float) float
@host func tan(a float) float
@host func tanh(a float) float
@host func trunc(a float) float