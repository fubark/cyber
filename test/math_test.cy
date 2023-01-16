import m 'math'
import t 'test'

try t.eqNear(m.pi, 3.14159265)
try t.eqNear(m.ln2, 0.69314718)
try t.eqNear(m.ln10, 2.30258509)
try t.eqNear(m.log2e, 1.44269504)
try t.eqNear(m.log10e, 0.43429448)
try t.eqNear(m.e, 2.71828182)
try t.eqNear(m.sqrt1_2, 0.70710678)
try t.eqNear(m.sqrt2, 1.41421356)

-- nan
try t.eq(m.isNaN(m.nan), true)
try t.eq(m.isNaN(1), false)
try t.eq(m.nan == m.nan, false)

try t.eq(m.abs(1), 1)
try t.eq(m.abs(-1), 1)
try t.eq(m.ceil(0), 0)
try t.eq(m.ceil(0.1), 1)
try t.eq(m.floor(0), 0)
try t.eq(m.floor(0.1), 0)
try t.eq(m.round(0), 0)
try t.eq(m.round(0.1), 0)
try t.eq(m.round(0.5), 1)
try t.eq(m.trunc(1.12), 1)
try t.eq(m.trunc(-1.12), -1)
try t.eq(m.max(1, 2), 2)
try t.eq(m.max(3, 2), 3)
try t.eq(m.min(1, 2), 1)
try t.eq(m.min(3, 2), 2)
try t.eq(m.sign(2), 1)
try t.eq(m.sign(0), 0)
try t.eq(m.sign(-2), -1)
try t.eq(m.clz32(1), 31)
try t.eq(m.clz32(2), 30)
try t.eq(m.clz32(-1), 0)
try t.eq(m.clz32(-2), 0)
try t.eq(m.mul32(2, 3), 6)
try t.eq(m.mul32(2147483647, 2), -2)
try t.eq(m.exp(0), 1)
try t.eqNear(m.exp(1), 2.7182818)
try t.eqNear(m.exp(-1), 0.36787944)
try t.eqNear(m.exp(2), 7.38905609)
try t.eq(m.expm1(0), 0)
try t.eqNear(m.expm1(1), 1.7182818)
try t.eqNear(m.expm1(-1), -0.63212055)
try t.eqNear(m.expm1(2), 6.38905609)
try t.eq(m.log(2, 8), 3)
try t.eq(m.log(10, 100), 2)
try t.eq(m.ln(1), 0)
try t.eqNear(m.ln(10), 2.302585092)

-- log1p
try t.eqNear(m.log1p(1), 0.6931471805)
try t.eq(m.log1p(0), 0)
try t.eq(m.log1p(-1), m.neginf)
try t.eq(m.isNaN(m.log1p(-2)), true)

-- log10
try t.eq(m.log10(100000), 5)
try t.eqNear(m.log10(2), 0.30102999)
try t.eq(m.log10(1), 0)
try t.eq(m.log10(0), m.neginf)

-- log2
try t.eqNear(m.log2(3), 1.5849625007)
try t.eq(m.log2(2), 1)
try t.eq(m.log2(1), 0)
try t.eq(m.log2(0), m.neginf)

-- pow
try t.eq(m.pow(7, 3), 343)
try t.eq(m.pow(4, 0.5), 2)
try t.eqNear(m.pow(7, -2), 0.020408163)
try t.eq(m.isNaN(m.pow(-7, 0.5)), true)

-- hypot
try t.eq(m.hypot(3, 4), 5)
try t.eq(m.hypot(5, 12), 13)

-- sqrt
try t.eq(m.sqrt(4), 2)
try t.eq(m.sqrt(9), 3)

-- cbrt
try t.eq(m.cbrt(8), 2)
try t.eq(m.cbrt(27), 3)

-- random
rand = m.random()
try t.eq(rand >= 0 and rand < 1, true)

-- cos
try t.eq(m.cos(m.pi), -1)
try t.eq(m.cos(0), 1)

-- sin
try t.eqNear(m.sin(m.pi), 0)
try t.eqNear(m.sin(m.pi/2), 1)

-- tan
try t.eqNear(m.tan(0), 0)
try t.eqNear(m.tan(m.pi/4), 1)

-- cosh
try t.eq(m.cosh(0), 1)
try t.eqNear(m.cosh(1), 1.54308063)
try t.eqNear(m.cosh(-1), 1.54308063)
try t.eqNear(m.cosh(2), 3.76219569)

-- sinh
try t.eq(m.sinh(0), 0)
try t.eqNear(m.sinh(1), 1.17520119)
try t.eqNear(m.sinh(-1), -1.17520119)
try t.eqNear(m.sinh(2), 3.6268604)

-- tanh
try t.eqNear(m.tanh(-1), -0.76159415)
try t.eq(m.tanh(0), 0)
try t.eq(m.tanh(m.inf), 1)
try t.eqNear(m.tanh(1), 0.76159415)

-- acos
try t.eq(m.acos(1), 0)

-- asin
try t.eqNear(m.asin(1), m.pi/2)

-- atan
try t.eq(m.atan(0), 0)
try t.eqNear(m.atan(1), 0.7853981)

-- atan2
try t.eq(m.atan2(0, 0), 0)

-- acosh
try t.eq(m.isNaN(m.acosh(0.999999999999)), true)
try t.eq(m.acosh(1), 0)
try t.eqNear(m.acosh(2), 1.31695789)
try t.eqNear(m.acosh(2.5), 1.56679923)

-- asinh
try t.eqNear(m.asinh(1), 0.88137358)
try t.eq(m.asinh(0), 0)
try t.eqNear(m.asinh(-1), -0.88137358)
try t.eqNear(m.asinh(2), 1.44363547)

-- atanh
try t.eq(m.atanh(-1), m.neginf)
try t.eq(m.atanh(0), 0)
try t.eqNear(m.atanh(0.5), 0.54930614)
try t.eq(m.atanh(1), m.inf)