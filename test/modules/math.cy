use m 'math'
use t 'test'

t.eqNear(m.pi, 3.14159265)
t.eqNear(m.ln2, 0.69314718)
t.eqNear(m.ln10, 2.30258509)
t.eqNear(m.log2e, 1.44269504)
t.eqNear(m.log10e, 0.43429448)
t.eqNear(m.e, 2.71828182)
t.eqNear(m.sqrt1_2, 0.70710678)
t.eqNear(m.sqrt2, 1.41421356)

-- nan
t.eq(m.isNaN(m.nan), true)
t.eq(m.isNaN(1), false)
t.eq(m.nan == m.nan, true)
t.eq(m.nan == m.nan + 1, true)

t.eq(m.abs(1), 1.0)
t.eq(m.abs(-1), 1.0)
t.eq(m.ceil(0), 0.0)
t.eq(m.ceil(0.1), 1.0)
t.eq(m.floor(0), 0.0)
t.eq(m.floor(0.1), 0.0)
t.eq(m.round(0), 0.0)
t.eq(m.round(0.1), 0.0)
t.eq(m.round(0.5), 1.0)
t.eq(m.trunc(1.12), 1.0)
t.eq(m.trunc(-1.12), -1.0)
t.eq(m.max(1, 2), 2.0)
t.eq(m.max(3, 2), 3.0)
t.eq(m.min(1, 2), 1.0)
t.eq(m.min(3, 2), 2.0)
t.eq(m.sign(2), 1.0)
t.eq(m.sign(0), 0.0)
t.eq(m.sign(-2), -1.0)
t.eq(m.clz32(1), 31.0)
t.eq(m.clz32(2), 30.0)
t.eq(m.clz32(-1), 0.0)
t.eq(m.clz32(-2), 0.0)
t.eq(m.mul32(2, 3), 6.0)
t.eq(m.mul32(2147483647, 2), -2.0)
t.eq(m.exp(0), 1.0)
t.eqNear(m.exp(1), 2.7182818)
t.eqNear(m.exp(-1), 0.36787944)
t.eqNear(m.exp(2), 7.38905609)
t.eq(m.expm1(0), 0.0)
t.eqNear(m.expm1(1), 1.7182818)
t.eqNear(m.expm1(-1), -0.63212055)
t.eqNear(m.expm1(2), 6.38905609)
t.eq(m.log(2, 8), 3.0)
t.eq(m.log(10, 100), 2.0)
t.eq(m.ln(1), 0.0)
t.eqNear(m.ln(10), 2.302585092)

-- log1p
t.eqNear(m.log1p(1), 0.6931471805)
t.eq(m.log1p(0), 0.0)
t.eq(m.log1p(-1), m.neginf)
t.eq(m.isNaN(m.log1p(-2)), true)

-- log10
t.eq(m.log10(100000), 5.0)
t.eqNear(m.log10(2), 0.30102999)
t.eq(m.log10(1), 0.0)
t.eq(m.log10(0), m.neginf)

-- log2
t.eqNear(m.log2(3), 1.5849625007)
t.eq(m.log2(2), 1.0)
t.eq(m.log2(1), 0.0)
t.eq(m.log2(0), m.neginf)

-- pow
t.eq(m.pow(7, 3), 343.0)
t.eq(m.pow(4, 0.5), 2.0)
t.eqNear(m.pow(7, -2), 0.020408163)
t.eq(m.isNaN(m.pow(-7, 0.5)), true)

-- hypot
t.eq(m.hypot(3, 4), 5.0)
t.eq(m.hypot(5, 12), 13.0)

-- sqrt
t.eq(m.sqrt(4), 2.0)
t.eq(m.sqrt(9), 3.0)

-- cbrt
t.eq(m.cbrt(8), 2.0)
t.eq(m.cbrt(27), 3.0)

-- random
var rand = m.random()
t.eq(rand >= 0 and rand < 1, true)

-- cos
t.eq(m.cos(m.pi), -1.0)
t.eq(m.cos(0), 1.0)

-- sin
t.eqNear(m.sin(m.pi), 0.0)
t.eqNear(m.sin(m.pi/2), 1.0)

-- tan
t.eqNear(m.tan(0), 0.0)
t.eqNear(m.tan(m.pi/4), 1.0)

-- cosh
t.eq(m.cosh(0), 1.0)
t.eqNear(m.cosh(1), 1.54308063)
t.eqNear(m.cosh(-1), 1.54308063)
t.eqNear(m.cosh(2), 3.76219569)

-- sinh
t.eq(m.sinh(0), 0.0)
t.eqNear(m.sinh(1), 1.17520119)
t.eqNear(m.sinh(-1), -1.17520119)
t.eqNear(m.sinh(2), 3.6268604)

-- tanh
t.eqNear(m.tanh(-1), -0.76159415)
t.eq(m.tanh(0), 0.0)
t.eq(m.tanh(m.inf), 1.0)
t.eqNear(m.tanh(1), 0.76159415)

-- acos
t.eq(m.acos(1), 0.0)

-- asin
t.eqNear(m.asin(1), m.pi/2)

-- atan
t.eq(m.atan(0), 0.0)
t.eqNear(m.atan(1), 0.7853981)

-- atan2
t.eq(m.atan2(0, 0), 0.0)

-- acosh
t.eq(m.isNaN(m.acosh(0.999999999999)), true)
t.eq(m.acosh(1), 0.0)
t.eqNear(m.acosh(2), 1.31695789)
t.eqNear(m.acosh(2.5), 1.56679923)

-- asinh
t.eqNear(m.asinh(1), 0.88137358)
t.eq(m.asinh(0), 0.0)
t.eqNear(m.asinh(-1), -0.88137358)
t.eqNear(m.asinh(2), 1.44363547)

-- atanh
t.eq(m.atanh(-1), m.neginf)
t.eq(m.atanh(0), 0.0)
t.eqNear(m.atanh(0.5), 0.54930614)
t.eq(m.atanh(1), m.inf)

-- isInt
t.eq(m.isInt(1.0), true)
t.eq(m.isInt(40000000.0), true)
t.eq(m.isInt(40000000.1), false)
t.eq(m.isInt(40000000.01), false)
t.eq(m.isInt(40000000.001), false)
t.eq(m.isInt(m.nan), false)
t.eq(m.isInt(m.inf), false)

-- frac
t.eqNear(m.frac(1.0), 0.0)
t.eqNear(m.frac(40000000.0), 0.0)
t.eqNear(m.frac(40000000.6), 0.6)
t.eqNear(m.frac(40000000.1), 0.1)
t.eqNear(m.frac(40000000.01), 0.01)
t.eqNear(m.frac(40000000.001), 0.001)

--cytest: pass