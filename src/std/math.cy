use c
use meta
use core

--| Math constants and functions.
--| Sample usage:
--| ```cy
--| use math
--| 
--| r := 10.0
--| print(math.pi * r**2)
--| ```

#if meta.system() == .windows:
    #c.bind_lib('msvcrt.dll')

#c.include('<math.h>')
#c.flag('-lm')

#[extern='acos']
-fn c_acos(a float) -> float

#[extern='asin']
-fn c_asin(a float) -> float

#[extern='atan']
-fn c_atan(a float) -> float

#[extern='atan2']
-fn c_atan2(y float, x float) -> float

#[extern='ceil']
-fn c_ceil(a float) -> float

#[bind, extern='cos']
-fn c_cos(a float) -> float

#[bind, extern='cosf']
-fn c_cosf(x f32) -> f32

#[extern='cosh']
-fn c_cosh(a float) -> float

#[extern='exp']
-fn c_exp(a float) -> float

#[extern='fabs']
-fn c_fabs(a float) -> float

#[extern='floor']
-fn c_floor(a float) -> float

#[extern='log']
-fn c_log(a float) -> float

#[extern='log10']
-fn c_log10(a float) -> float

#[extern='modf']
-fn c_modf(a float, i Ptr[float]) -> float

#[extern='pow']
-fn c_pow(a float, b float) -> float

#[bind, extern='sin']
-fn c_sin(a float) -> float

#[bind, extern='sinf']
-fn c_sinf(x f32) -> f32

#[extern='sinh']
-fn c_sinh(a float) -> float

#[extern='sqrt']
-fn c_sqrt(a float) -> float

#[bind, extern='tan']
-fn c_tan(a float) -> float

#[bind, extern='tanf']
-fn c_tanf(x f32) -> f32

#[extern='tanh']
-fn c_tanh(a float) -> float

--| Euler's number and the base of natural logarithms; approximately 2.718.
const e = 2.71828182845904523536028747135266249775724709369995

--| Infinity.
const inf = as[float] 0x7ff0000000000000

--| Base-10 logarithm of E; approximately 0.434.
const log10e = 0.434294481903251827651128918916605082

--| Base-2 logarithm of E; approximately 1.443.
const log2e = 1.442695040888963407359924681001892137

--| Natural logarithm of 10; approximately 2.303.
const ln10 = 2.302585092994045684017991454684364208

--| Natural logarithm of 2; approximately 0.693.
const ln2 = 0.693147180559945309417232121458176568

--| The maximum integer value that can be safely represented as a float. 2^53-1 or 9007199254740991.
const maxSafeInt = 9007199254740991.0

--| The minumum integer value that can be safely represented as a float. -(2^53-1) or -9007199254740991.
const minSafeInt = -9007199254740991.0

--| Not a number. Note that nan == nan.
--| However, if a nan came from an arithmetic operation, the comparison is undefined.
--| Use `isNaN` instead.
const nan = as[float] 0x7ff8000000000000

--| Negative infinity.
const neginf = as[float] 0xfff0000000000000

--| Ratio of a circle's circumference to its diameter; approximately 3.14159.
const pi = 3.14159265358979323846264338327950288419716939937510

const pi2 = pi * 2
const piHalf = pi * 0.5

--| Square root of ½; approximately 0.707.
const sqrt1_2 = 0.707106781186547524400844362104849039

--| Square root of 2; approximately 1.414.
const sqrt2 = 1.414213562373095048801688724209698079

--| Returns the absolute value of x.
#[bind]
fn abs(a float) -> float:
    return c_fabs(a)

--| Returns the arccosine of x.
#[bind]
fn acos(a float) -> float:
    return c_acos(a)

--| Returns the hyperbolic arccosine of x.
#[bind]
fn acosh(a float) -> float:
    u := @bitCast(r64, a)
    e := (u >> 52) && 0x7ff
    if e < 0x3ff + 1:
        return log1p(x - 1 + sqrt((x-1)**2 + 2*(x-1)))
    else e < 0x3ff + 26:
        return log(2 * x - 1 / (x + sqrt(x*x - 1)))
    else:
        return log(x) + ln2

--| Returns the arcsine of x.
#[bind]
fn asin(a float) -> float:
    return c_asin(a)

--| Returns the hyperbolic arcsine of a number.
#[bind]
fn asinh(a float) -> float:
    u := @bitCast(r64. a)
    e := (u >> 52) && 0x7ff
    s := u >> 63

    rx := @bitCast(float, u && (r64.max >> 1))

    if e >= 0x3ff + 26:
        rx = log(rx) + ln2
    else e >= 0x3ff + 1:
        rx = log(2 * rx + 1 / (sqrt(rx*rx + 1) + rx))
    else e >= 0x3ff - 26:
        rx = log1p(rx + rx*rx / (sqrt(rx*rx + 1) + 1))

    return if (s != 0) -rx else rx

--| Returns the arctangent of x.
#[bind]
fn atan(a float) -> float:
    return c_atan(a)

--| Returns the arctangent of the quotient of its arguments.
#[bind]
fn atan2(y float, x float) -> float:
    return c_atan2(y, x)

--| Returns the hyperbolic arctangent of x.
#[bind]
fn atanh(a float) -> float:
    u := @bitCast(r64, x)
    e := (u >> 52) && 0x7ff
    s := u >> 63

    y := @bitCast(float, u && (r64.max >> 1))

    if y == 1.0:
        return copysign(inf, x)

    if e < 0x3ff - 1:
        if e < 0x3ff - 32:
            pass
        else:
            y = 0.5 * log1p(2*y + 2*y*y / (1-y))
    else:
        y = 0.5 * log1p(2 * (y / (1-y)))

    return if (s != 0) -y else y

--| Returns the cube root of x.
#[bind]
fn cbrt(a float) -> float:
    var B1 r32 = 715094163
    var B2 r32 = 696219795

    -- |1 / cbrt(x) - p(x)| < 2^(23.5)
    P0 := 1.87595182427177009643
    P1 := -1.88497979543377169875
    P2 := 1.621429720105354466140
    P3 := -0.758397934778766047437
    P4 := 0.145996192886612446982

    u := @bitCast(r64, a)
    hx := (as[r32] (u >> 32)) && 0x7FFFFFFF

    -- cbrt(nan, inf) = itself
    if hx >= 0x7FF00000:
        return x + x

    -- cbrt to ~5bits
    if hx < 0x00100000:
        panic('TODO')
        --u = @bitCast(r64, x * 0x1.0p54));
        --hx = @as(u32, @intCast(u >> 32)) & 0x7FFFFFFF;

        ---- cbrt(+-0) = itself
        --if (hx == 0) {
        --    return x;
        --}
        --hx = hx / 3 + B2;
    else:
        hx = hx / 3 + B1

    u &&= 1 << 63
    u ||= (as[r64] hx) << 32
    t := @bitCast(f64, u)

    -- cbrt to 23 bits
    -- cbrt(x) = t * cbrt(x / t^3) ~= t * P(t^3 / x)
    r := (t * t) * (t / x)
    t = t * ((P0 + r * (P1 + r * P2)) + ((r * r) * r) * (P3 + r * P4))

    -- Round t away from 0 to 23 bits
    u = @bitCast(r64, t)
    u = (u + 0x80000000) && 0xFFFFFFFFC0000000
    t = @bitCast(f64, u)

    -- one step newton to 53 bits
    s := t * t
    q := x / s
    w := t + t
    q = (q - t) / (w + q)

    return t + t * q

--| Returns the smallest integer greater than or equal to x.
#[bind]
fn ceil(a float) -> float:
    return c_ceil(a)

--| Returns the number of leading zero bits of the 32-bit integer x.
#[bind]
fn clz32(a float) -> float:
    panic('TODO')

fn copysign(mag float, sign float) -> float:
    sign_bit_mask := 1 << 63
    a := @bitCast(r64, mag) && ~sign_bit_mask
    b := @bitCast(r64, sign) && sign_bit_mask
    return @bitCast(float, a || b)

--| Returns the cosine of x.
fn cos(x int) -> float:
    return c_cos(as x)
fn cos(x %T) -> T:
    #if T == float:
        return c_cos(x)
    #else:
        return c_cosf(x)

--| Returns the hyperbolic cosine of x.
#[bind]
fn cosh(a float) -> float:
    return c_cosh(a)

fn degToRad(deg f32) -> f32:
    return deg * f32(pi2) / 360

--| Returns e^x, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm).
#[bind]
fn exp(a float) -> float:
    return c_exp(a)

--| Returns subtracting 1 from exp(x).
#[bind]
fn expm1(a float) -> float:
    @panic('TODO')

--| Returns the largest integer less than or equal to x.
#[bind]
fn floor(a float) -> float:
    return c_floor(a)

--| Returns the fractional or decimal part of a float value.
#[bind]
fn frac(a float) -> float:
    i := 0.0
    return c_modf(a, *i)

--| Returns the square root of the sum of squares of its arguments.
#[bind]
fn hypot(a float, b float) -> float:
    @panic('TODO')

fn isInf(a float) -> bool:
    return (@bitCast(int, a) && (~0 >> 1)) == @bitCast(int, inf)

--| Returns true if the float has no fractional part, otherwise false.
#[bind]
fn isInt(a float) -> bool:
    if isInf(a):
        return false
    return c_trunc(a) == a

--| Returns whether x is not a number.
#[bind]
fn isNaN(a float) -> bool:
    return a != a

--| Returns the natural logarithm (㏒e; also, ㏑) of x.
#[bind]
fn ln(a float) -> float:
    return c_log(a)

--| Returns the logarithm of y with base x.
#[bind]
fn log(x float, y float) -> float:
    return c_log(y) / c_log(x)

--| Returns the base-10 logarithm of x.
#[bind]
fn log10(a float) -> float:
    return c_log10(a)

--| Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x.
#[bind]
fn log1p(a float) -> float:
    panic('TODO')

--| Returns the base-2 logarithm of x.
#[bind]
fn log2(a float) -> float:
    panic('TODO')

--| Returns the largest of two numbers.
fn max(a %T, b T) -> T:
    return core.max(a, b)

#[bind]
fn max_f64(a float, b float) -> float:
    if isNaN(a): return b
    if isNaN(b): return a
    return if (a > b) a else b

--| Returns the smallest of two numbers.
fn min(a %T, b T) -> T:
    return core.min(a, b)

#[bind]
fn min_f64(a float, b float) -> float:
    if isNaN(a): return b
    if isNaN(b): return a
    return if (a < b) a else b

--| Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed.
#[bind]
fn mul32(a float, b float) -> float:
    panic('TODO')

--| Returns base x to the exponent power y (that is, x^y).
#[bind]
fn pow(a float, b float) -> float:
    return c_pow(a, b)

--| Returns a pseudo-random number between 0 and 1.
#[bind]
fn random() -> float:
    panic('TODO: move to rand module')

--| Returns the value of the number x rounded to the nearest integer.
#[bind]
fn round(a float) -> float:
    panic('TODO')

--| Returns the sign of the x, indicating whether x is positive, negative, or zero.
#[bind]
fn sign(a float) -> float:
    if a == 0:
        return 0
    return copysign(1.0, a)

--| Returns the sine of x.
fn sin(x int) -> float:
    return c_sin(as x)
fn sin(x %T) -> T:
    #if T == float:
        return c_sin(x)
    #else:
        return c_sinf(x)

--| Returns the hyperbolic sine of x.
#[bind]
fn sinh(a float) -> float:
    return c_sinh(a)

--| Returns the positive square root of x.
#[bind]
fn sqrt(a float) -> float:
    return c_sqrt(a)

--| Returns the tangent of x.
fn tan(x int) -> float:
    return c_tan(as x)
fn tan(x %T) -> T:
    #if T == float:
        return c_tan(x)
    #else:
        return c_tanf(x)

--| Returns the hyperbolic tangent of x.
#[bind]
fn tanh(a float) -> float:
    return c_tanh(a)

--| Returns the integer portion of x, removing any fractional digits.
#[bind]
fn trunc(a float) -> float:
    panic('TODO')

-- Row-major order.
type Mat1x3[T Any]:
    inner [3]T
type Mat3x1[T Any]:
    inner [3]T

fn (Mat1x3[]) `*`(m Mat3x3) -> Mat3x1:
    stride := 3
    r0 := 0 * stride
    r1 := 1 * stride
    r2 := 2 * stride
    a00 := m[r0 + 0]
    a01 := m[r0 + 1]
    a02 := m[r0 + 2]
    a10 := m[r1 + 0]
    a11 := m[r1 + 1]
    a12 := m[r1 + 2]
    a20 := m[r2 + 0]
    a21 := m[r2 + 1]
    a22 := m[r2 + 2]
    b00 := $[0]
    b10 := $[1]
    b20 := $[2]
    return {
        a00 * b00 + a01 * b10 + a02 * b20,
        a10 * b00 + a11 * b10 + a12 * b20,
        a20 * b00 + a21 * b10 + a22 * b20,
    }

-- Row-major order.
type Mat3[T Any]:
    inner [9]T

fn (Mat3[]) `*`(m Self) -> Self:
    stride := 3
    r0 := 0 * stride
    r1 := 1 * stride
    r2 := 2 * stride
    a00 := m[r0 + 0]
    a01 := m[r0 + 1]
    a02 := m[r0 + 2]
    a10 := m[r1 + 0]
    a11 := m[r1 + 1]
    a12 := m[r1 + 2]
    a20 := m[r2 + 0]
    a21 := m[r2 + 1]
    a22 := m[r2 + 2]
    b00 := $[r0 + 0]
    b01 := $[r0 + 1]
    b02 := $[r0 + 2]
    b10 := $[r1 + 0]
    b11 := $[r1 + 1]
    b12 := $[r1 + 2]
    b20 := $[r2 + 0]
    b21 := $[r2 + 1]
    b22 := $[r2 + 2]
    return {
        -- First row.
        a00 * b00 + a01 * b10 + a02 * b20,
        a00 * b01 + a01 * b11 + a02 * b21,
        a00 * b02 + a01 * b12 + a02 * b22,

        a10 * b00 + a11 * b10 + a12 * b20,
        a10 * b01 + a11 * b11 + a12 * b21,
        a10 * b02 + a11 * b12 + a12 * b22,

        a20 * b00 + a21 * b10 + a22 * b20,
        a20 * b01 + a21 * b11 + a22 * b21,
        a20 * b02 + a21 * b12 + a22 * b22,
    }

type Mat1x4[T Any]:
    inner [4]T
type Mat4x1[T Any]:
    inner [4]T

fn (Mat1x4[]) `*`(m Mat4[T]) -> Mat4x1[T]:
    stride := 4
    r0 := 0 * stride
    r1 := 1 * stride
    r2 := 2 * stride
    r3 := 3 * stride
    a00 := m.inner[r0 + 0]
    a01 := m.inner[r0 + 1]
    a02 := m.inner[r0 + 2]
    a03 := m.inner[r0 + 3]
    a10 := m.inner[r1 + 0]
    a11 := m.inner[r1 + 1]
    a12 := m.inner[r1 + 2]
    a13 := m.inner[r1 + 3]
    a20 := m.inner[r2 + 0]
    a21 := m.inner[r2 + 1]
    a22 := m.inner[r2 + 2]
    a23 := m.inner[r2 + 3]
    a30 := m.inner[r3 + 0]
    a31 := m.inner[r3 + 1]
    a32 := m.inner[r3 + 2]
    a33 := m.inner[r3 + 3]
    b00 := $inner[0]
    b10 := $inner[1]
    b20 := $inner[2]
    b30 := $inner[3]
    return {
        a00 * b00 + a01 * b10 + a02 * b20 + a03 * b30,
        a10 * b00 + a11 * b10 + a12 * b20 + a13 * b30,
        a20 * b00 + a21 * b10 + a22 * b20 + a23 * b30,
        a30 * b00 + a31 * b10 + a32 * b20 + a33 * b30,
    }

-- Row-major order.
type Mat4[T Any]:
    inner [16]T

fn Mat4[] :: @init(inner [16]T) -> Self:
    return {
        inner = inner,
    }

fn Mat4[] :: identity() -> Self:
    return {
        inner = {
            1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, 0,
            0, 0, 0, 1,
        }
    }

fn Mat4[] :: zero() -> Self:
    return {
        inner = [16]T(0)
    }

fn (Mat4[]) `*`(m Self) -> Self:
    stride := 4
    r0 := 0 * stride
    r1 := 1 * stride
    r2 := 2 * stride
    r3 := 3 * stride
    a00 := m.inner[r0 + 0]
    a01 := m.inner[r0 + 1]
    a02 := m.inner[r0 + 2]
    a03 := m.inner[r0 + 3]
    a10 := m.inner[r1 + 0]
    a11 := m.inner[r1 + 1]
    a12 := m.inner[r1 + 2]
    a13 := m.inner[r1 + 3]
    a20 := m.inner[r2 + 0]
    a21 := m.inner[r2 + 1]
    a22 := m.inner[r2 + 2]
    a23 := m.inner[r2 + 3]
    a30 := m.inner[r3 + 0]
    a31 := m.inner[r3 + 1]
    a32 := m.inner[r3 + 2]
    a33 := m.inner[r3 + 3]
    b00 := $inner[r0 + 0]
    b01 := $inner[r0 + 1]
    b02 := $inner[r0 + 2]
    b03 := $inner[r0 + 3]
    b10 := $inner[r1 + 0]
    b11 := $inner[r1 + 1]
    b12 := $inner[r1 + 2]
    b13 := $inner[r1 + 3]
    b20 := $inner[r2 + 0]
    b21 := $inner[r2 + 1]
    b22 := $inner[r2 + 2]
    b23 := $inner[r2 + 3]
    b30 := $inner[r3 + 0]
    b31 := $inner[r3 + 1]
    b32 := $inner[r3 + 2]
    b33 := $inner[r3 + 3]
    return Self({
        -- First row.
        a00 * b00 + a01 * b10 + a02 * b20 + a03 * b30,
        a00 * b01 + a01 * b11 + a02 * b21 + a03 * b31,
        a00 * b02 + a01 * b12 + a02 * b22 + a03 * b32,
        a00 * b03 + a01 * b13 + a02 * b23 + a03 * b33,

        a10 * b00 + a11 * b10 + a12 * b20 + a13 * b30,
        a10 * b01 + a11 * b11 + a12 * b21 + a13 * b31,
        a10 * b02 + a11 * b12 + a12 * b22 + a13 * b32,
        a10 * b03 + a11 * b13 + a12 * b23 + a13 * b33,

        a20 * b00 + a21 * b10 + a22 * b20 + a23 * b30,
        a20 * b01 + a21 * b11 + a22 * b21 + a23 * b31,
        a20 * b02 + a21 * b12 + a22 * b22 + a23 * b32,
        a20 * b03 + a21 * b13 + a22 * b23 + a23 * b33,

        a30 * b00 + a31 * b10 + a32 * b20 + a33 * b30,
        a30 * b01 + a31 * b11 + a32 * b21 + a33 * b31,
        a30 * b02 + a31 * b12 + a32 * b22 + a33 * b32,
        a30 * b03 + a31 * b13 + a32 * b23 + a33 * b33,
    })
    
-- pub const Quaternion = struct {
--     /// Unit quaternion.
--     vec: Vec4,

--     pub fn init(vec: Vec4) Quaternion {
--         return .{
--             .vec = vec,
--         };
--     }

--     pub fn initIdent() Quaternion {
--         return .{
--             .vec = Vec4.init(0, 0, 0, 1),
--         };
--     }

--     /// Given unit axis vector and angle, return equivalent quaternion.
--     pub fn initRotation(axis: Vec3, rad: f32) Quaternion {
--         const sin = std.math.sin(rad * 0.5);
--         const cos = std.math.cos(rad * 0.5);
--         return .{
--             .vec = Vec4.init(
--                 axis.x * sin,
--                 axis.y * sin,
--                 axis.z * sin,
--                 cos,
--             )
--         };
--     }

--     pub fn toAxisVec(self: Quaternion) Vec3 {
--         const rad = std.math.acos(self.vec.w) * 2;
--         const sin = std.math.sin(rad * 0.5);
--         return Vec3.init(
--             self.vec.x / sin,
--             self.vec.y / sin,
--             self.vec.z / sin,
--         );
--     }

--     pub fn inverse(self: Quaternion) Quaternion {
--         return .{
--             .vec = Vec4.init(-self.vec.x, -self.vec.y, -self.vec.z, self.vec.w),
--         };
--     }

--     pub fn rotate(self: Quaternion, v: Vec3) Vec3 {
--         const q = Vec3.init(self.vec.x, self.vec.y, self.vec.z);
--         const qdotq = q.dot(q);
--         const qdotv = q.dot(v);
--         const cross = q.cross(v);
--         const ww = self.w * self.w;
--         const w2 = self.w * 2;
--         return .{
--             .x = v.x * (ww - qdotq.x) + self.x * qdotv.x * 2 + cross.x * w2,
--             .y = v.y * (ww - qdotq.y) + self.y * qdotv.y * 2 + cross.y * w2,
--             .z = v.z * (ww - qdotq.z) + self.z * qdotv.z * 2 + cross.z * w2,
--         };
--     }

--     /// Spherical Linear Interpolation
--     /// https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#interpolation-slerp
--     pub fn slerp(self: Quaternion, to: Quaternion, tt: f32) Quaternion {
--         const d = self.dot(to);
--         const adot = std.math.fabs(d);
--         const a = std.math.acos(adot);
--         if (adot >= 1) {
--             // Prevent divide by 0 from sin(a).
--             return self;
--         }
--         const s = d/adot;

--         const from_vec = self.vec.mul(std.math.sin(a * (1-tt))/std.math.sin(a));
--         const to_vec = to.vec.mul(s * std.math.sin(a * tt)/std.math.sin(a));
--         return .{
--             .vec = from_vec.add(to_vec),
--         };
--     }

--     pub fn dot(self: Quaternion, q: Quaternion) f32 {
--         return self.vec.dot(q.vec);
--     }

--     /// Return quaternion for doing a subsequent rotation.
--     pub fn mul(self: Quaternion, q: Quaternion) Quaternion {
--         return Quaternion{
--             .vec = .{
--                 .x = self.vec.w * q.vec.x + self.vec.x * q.vec.w + self.vec.y * q.vec.z - self.vec.z * q.vec.y,
--                 .y = self.vec.w * q.vec.y - self.vec.x * q.vec.z + self.vec.y * q.vec.w + self.vec.z * q.vec.x,
--                 .z = self.vec.w * q.vec.z + self.vec.x * q.vec.y - self.vec.y * q.vec.x + self.vec.z * q.vec.w,
--                 .w = self.vec.w * q.vec.w - self.vec.x * q.vec.x - self.vec.y * q.vec.y - self.vec.z * q.vec.z,
--             },
--         };
--     }
-- };

-- test "Quaternion.slerp" {
--     // slerp with self.
--     try eqApproxVec4(Quaternion.init(Vec4.init(0, 0, 0, 1)).slerp(Quaternion.init(Vec4.init(0, 0, 0, 1)), 0).vec, Vec4.init(0, 0, 0, 1));
-- }

-- test "Extracting rotate + uniform scale matrix from transform matrix." {
--     var xform = Transform.initIdentity();
--     xform.rotateY(std.math.pi/2.0);

--     const pos = Vec3.init(1, 0, 0);
--     try eqApproxVec3(xform.interpolateVec3(pos), Vec3.init(0, 0, 1));

--     const mat = xform.toRotationUniformScaleMat();
--     const res = stdx.math.mul3x3_3x1(mat, .{pos.x, pos.y, pos.z});
--     try eqApproxVec3(Vec3.init(res[0], res[1], res[2]), Vec3.init(0, 0, 1));
-- }

-- Contains abstractions related to transforming points from one space to another using matrices and vectors.
-- TODO: Add transform just for 2D coords.
type Transform3D[T Any]:
    mat Mat4[T] = Mat4[T].zero()

fn Transform3D[] :: @init(mat [16]T) -> Self:
    return {
        mat = {
            inner = mat
        }
    }

fn Transform3D[] :: @init(mat Mat4[T]) -> Self:
    return {
        mat = mat,
    }

fn Transform3D[] :: identity() -> Self:
    return {
        mat = Mat4[T].identity(),
    }

fn Transform3D[] :: zero() -> Self:
    return {
        mat = {},
    }

fn Transform3D[] :: translation(x, y T) -> Self:
    return Self({
        1, 0, 0, x,
        0, 1, 0, y,
        0, 0, 1, 0,
        0, 0, 0, 1,
    })

fn Transform3D[] :: translation(x, y, z T) -> Self:
    return Self({
        1, 0, 0, x,
        0, 1, 0, y,
        0, 0, 1, z,
        0, 0, 0, 1,
    })

fn Transform3D[] :: rotation(x, y, z Vec3[T]) -> Self:
    return Self({
        x.x, x.y, x.z, 0,
        y.x, y.y, y.z, 0,
        z.x, z.y, z.z, 0,
        0, 0, 0, 1,
    })

fn Transform3D[] :: rotationX(rad T) -> Self:
    c := cos(rad)
    s := sin(rad)
    return Self({
        1, 0,  0, 0,
        0, c,  s, 0,
        0, -s, c, 0,
        0, 0,  0, 1,
    })

fn Transform3D[] :: rotationY(rad T) -> Self:
    c := cos(rad)
    s := sin(rad)
    return Self({
        c, 0, -s, 0,
        0, 1, 0,  0,
        s, 0, c,  0,
        0, 0, 0,  1,
    })

fn Transform3D[] :: rotationZ(rad T) -> Self:
    c := cos(rad)
    s := sin(rad)
    return Self({
        c, -s, 0, 0,
        s, c,  0, 0,
        0, 0,  1, 0,
        0, 0,  0, 1,
    })

fn Transform3D[] :: scaling(x, y T) -> Self:
    return Self({
        x, 0, 0, 0,
        0, y, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1,
    })

fn Transform3D[] :: scaling(x, y, z T) -> Self:
    return Self({
        x, 0, 0, 0,
        0, y, 0, 0,
        0, 0, z, 0,
        0, 0, 0, 1,
    })

-- -- To RHS.
-- fn initQuaternion(q: Quaternion) Transform {
--     const x2 = q.vec.x*q.vec.x;
--     const y2 = q.vec.y*q.vec.y;
--     const z2 = q.vec.z*q.vec.z;
--     const xy = q.vec.x*q.vec.y;
--     const yz = q.vec.y*q.vec.z;
--     const xz = q.vec.x*q.vec.z;
--     const wx = q.vec.w*q.vec.x;
--     const wy = q.vec.w*q.vec.y;
--     const wz = q.vec.w*q.vec.z;
--     return .{
--         .mat = .{
--             1 - 2 * (y2 + z2), 2 * (xy - wz), 2 * (xz + wy), 0,
--             2 * (xy + wz), 1 - 2 * (x2 + z2), 2 * (yz - wx), 0,
--             2 * (xz - wy), 2 * (yz + wx), 1 - 2 * (x2 + y2), 0,
--             0, 0, 0, 1,
--         },
--     };
-- }

fn (&Transform3D[]) `*`(x Self) -> Self:
    return {
        mat = $mat * x.mat
    }

--     pub fn invert(self: *Transform) bool {
--         var res: Mat4 = undefined;
--         if (!math.invert4x4(self.mat, &res)) {
--             return false;
--         }
--         self.mat = res;
--         return true;
--     }

fn (&Transform3D[]) scale(x f32, y f32) -> Self:
    return {
        mat = $mat * Self.scaling(x, y).mat,
    }

fn (&Transform3D[]) scale(x f32, y f32, z f32) -> Self:
    return {
        mat = $mat * Self.scaling(x, y, z).mat
    }

fn (&Transform3D[]) translate(x f32, y f32) -> Self:
    return {
        mat = $mat * Self.translation(x, y).mat
    }

-- test "Interpolate" {
--     var transform = Transform.initIdentity();
--     transform.translate(10, 10);
--     try t.eq(transform.interpolateVec4(Vec4.init(0, 0, 0, 1)), Vec4.init(10, 10, 0, 1));
--     try t.eq(transform.interpolateVec4(Vec4.init(10, 10, 0, 1)), Vec4.init(20, 20, 0, 1));
-- }

--     pub fn translate3D(self: *Transform, x: f32, y: f32, z: f32) void {
--         self.mat = math.mul4x4_4x4(getTranslation3D(x, y, z), self.mat);
--     }

--     pub fn translateVec3D(self: *Transform, vec: Vec3) void {
--         self.mat = math.mul4x4_4x4(getTranslation3D(vec.x, vec.y, vec.z), self.mat);
--     }

--     pub fn rotate3D(self: *Transform, xvec: Vec3, yvec: Vec3, zvec: Vec3) void {
--         self.mat = math.mul4x4_4x4(getRotation3D(xvec, yvec, zvec), self.mat);
--     }

--     pub fn rotateX(self: *Transform, rad: f32) void {
--         self.mat = math.mul4x4_4x4(getRotationX(rad), self.mat);
--     }

--     pub fn rotateY(self: *Transform, rad: f32) void {
--         self.mat = math.mul4x4_4x4(getRotationY(rad), self.mat);
--     }

--     pub fn rotateZ(self: *Transform, rad: f32) void {
--         self.mat = math.mul4x4_4x4(getRotationZ(rad), self.mat);
--     }

--     pub fn rotateQuat(self: *Transform, quat: Quaternion) void {
--         const xform = Transform.initQuaternion(quat);
--         self.mat = math.mul4x4_4x4(xform.mat, self.mat);
--     }

--     pub fn reset(self: *Transform) void {
--         self.mat = identity();
--     }

--     pub fn interpolatePt(self: Transform, vec: Vec2) Vec2 {
--         const res = math.mul4x4_4x1(self.mat, [4]f32{vec.x, vec.y, 0, 1 });
--         return Vec2.init(res[0], res[1]);
--     }

--     pub fn interpolate4(self: Transform, x: f32, y: f32, z: f32, w: f32) Vec4 {
--         const res = math.mul4x4_4x1(self.mat, [4]f32{x, y, z, w });
--         return Vec4{ .x = res[0], .y = res[1], .z = res[2], .w = res[3] };
--     }

--     /// Convenience for perspective divide. Performs interpolate and divides result by the last component.
--     pub fn interpolate4div(self: Transform, x: f32, y: f32, z: f32, w: f32) Vec4 {
--         const res = math.mul4x4_4x1(self.mat, [4]f32{x, y, z, w });
--         return Vec4{ .x = res[0] / res[3], .y = res[1] / res[3], .z = res[2] / res[3], .w = res[3] / res[3] };
--     }

--     pub fn interpolate3(self: Transform, x: f32, y: f32, z: f32) Vec3 {
--         const res = math.mul4x4_4x1(self.mat, [4]f32{x, y, z, 1 });
--         return Vec3{ .x = res[0], .y = res[1], .z = res[2] };
--     }

--     pub fn interpolateVec3(self: Transform, vec: Vec3) Vec3 {
--         const res = math.mul4x4_4x1(self.mat, [4]f32{vec.x, vec.y, vec.z, 1 });
--         return Vec3{ .x = res[0], .y = res[1], .z = res[2] };
--     }

--     pub fn interpolateVec4(self: Transform, vec: Vec4) Vec4 {
--         const res = math.mul4x4_4x1(self.mat, .{ vec.x, vec.y, vec.z, vec.w });
--         return .{ .x = res[0], .y = res[1], .z = res[2], .w = res[3] };
--     }

--     /// Useful for getting the normal matrix when the scale is known to be uniform.
--     pub fn toRotationUniformScaleMat(self: Transform) stdx.math.Mat3 {
--         return .{
--             self.mat[0], self.mat[1], self.mat[2],
--             self.mat[4], self.mat[5], self.mat[6],
--             self.mat[8], self.mat[9], self.mat[10],
--         };
--     }

--     pub fn toRotationMat(self: Transform) stdx.math.Mat3 {
--         const mat = self.toRotationUniformScaleMat();
--         var inverted: stdx.math.Mat3 = undefined;
--         _ = stdx.math.invert3x3(mat, &inverted);
--         return stdx.math.transpose3x3(inverted);
--     }
-- };

type Vec2[T Any]:
    x T = 0
    y T = 0

const Vec2[] :: unit_x = Self(1, 0)
const Vec2[] :: unit_y = Self(0, 1)

fn Vec2[] :: @init(x, y T) -> Self:
    return {x = x, y = y}

fn (Vec2[]) `-`() -> Self:
    return {x=-$x, y=-$y}

-- Component subtraction.
fn (Vec2[]) `-`(v Self) -> Self:
    return {x=$x - v.x, y=$y - v.y}

-- Component addition.
fn (Vec2[]) `+`(v Self) -> Self:
    return {x=$x + v.x, y=$y + v.y}

-- Scalar multiplication.
fn (Vec2[]) `*`(scale T) -> Self:
    return {x=$x * scale, y=$y * scale}

-- Component multiplication.
fn (Vec2[]) `*`(v Self) -> Self:
    return {x=$x * v.x, y=$y * v.y}

-- Scalar division.
fn (Vec2[]) `/`(scale T) -> Self:
    return {x=$x / scale, y=$y / scale}

-- Cross product.
fn (Vec2[]) cross(v Self) -> T:
    return $x*v.y - $y*v.x

-- Dot product.
fn (Vec2[]) dot(v Self) -> T:
    return $x*v.x + $y*v.y

fn (Vec2[]) len() -> T:
    return sqrt($x*$x + $y*$y)

fn (Vec2[]) lenSq() -> T:
    return $x*$x + $y*$y

fn (Vec2[]) normalize() -> T:
    length := $len()
    return {x=$x/length, y=$y/length}

fn (Vec2[]) scaleTo(len T) -> Self:
    factor := len / $len()
    return {x=x*factor, y=y*factor}

type Vec3[T Any]:
    x T = 0
    y T = 0
    z T = 0

const Vec3[] :: unit_x = Self(1, 0, 0)
const Vec3[] :: unit_y = Self(0, 1, 0)
const Vec3[] :: unit_z = Self(0, 0, 1)

fn Vec3[] :: @init(x, y, z T) -> Self:
    return {x=x, y=y, z=z}

-- Scalar multiplication.
fn (Vec3[]) `*`(s T) -> Self:
    return {x=$x*s, y=$y*s, z=$z*s}

-- Component addition.
fn (Vec3[]) `+`(v Self) -> Self:
    return {x=$x+v.x, y=$y+v.y, z=$z+v.z}

-- Component addition.
fn (Vec3[]) `+`(v Self, x, y, z T) -> Self:
    return {x=$x+x, y=$y+y, z=$z+z}

fn (Vec3[]) cross(v Self) -> Self:
    return {
        x = $y*v.z - $z*v.y,
        y = $z*v.x - $x*v.z,
        z = $x*v.y - $y*v.x,
    }

fn (Vec3[]) dot(v Self) -> T:
    return $x*v.x + $y*v.y + $z*v.z

fn (Vec3[]) len() -> T:
    return sqrt($x*$x + $y*$y + $z*$z)

-- Linear interpolation for t: [0,1]
fn (Vec3[]) lerp(v Self, tt T) -> Self:
    return {
        x = $x + (v.x - $x)*tt,
        y = $y + (v.y - $y)*tt,
        z = $z + (v.z - $z)*tt,
    }

fn (Vec3[]) normalize() -> Self:
    length := $len()
    return {x=$x/length, y=$y/length, z=$z/length}

-- Rotates the vector along an arbitrary axis. Assumes axis vector is normalized.
fn (Vec3[]) rotateAxis(axis Self, rad T) -> Self:
    v_para := axis * $dot(axis)
    v_perp := $ + v_para*-1
    v_perp_term := v_perp*cos(rad)
    axv_term := axis.cross($) * sin(rad)
    return {
        x = v_para.x + v_perp_term.x + axv_term.x,
        y = v_para.y + v_perp_term.y + axv_term.y,
        z = v_para.z + v_perp_term.z + axv_term.z,
    }

-- Uses rotors to rotate the 3d vector around the y axis.
fn (Vec3[]) rotateY(rad T) -> Self:
    half_rad := rad * 0.5
    a := Self.unit_x
    b := Self(cos(half_rad), 0, sin(half_rad))
    ra_dot := a * ($dot(a) * -2)
    ra := $ + ra_dot
    rb_dot := b * (ra.dot(b) * -2)
    rba := ra.add(rb_dot)
    return rba

-- test "Vec3.rotateY" {
--     const pif = @as(f32, pi);
--     // On xz plane.
--     var v = Vec3.init(1, 0, 0);
--     try eqApproxVec3(v.rotateY(0), Vec3.init(1, 0, 0));
--     try eqApproxVec3(v.rotateY(pif*0.5), Vec3.init(0, 0, 1));
--     try eqApproxVec3(v.rotateY(pif), Vec3.init(-1, 0, 0));
--     try eqApproxVec3(v.rotateY(pif*1.5), Vec3.init(0, 0, -1));

--     // Tilted into y.
--     v = Vec3.init(1, 1, 0);
--     try eqApproxVec3(v.rotateY(0), Vec3.init(1, 1, 0));
--     try eqApproxVec3(v.rotateY(pif*0.5), Vec3.init(0, 1, 1));
--     try eqApproxVec3(v.rotateY(pif), Vec3.init(-1, 1, 0));
--     try eqApproxVec3(v.rotateY(pif*1.5), Vec3.init(0, 1, -1));
-- }

-- test "Vec3.rotateAxis" {
--     const pif = @as(f32, pi);
--     // Rotate from +y toward +z
--     var v = Vec3.init(0, 1, 0);
--     try eqApproxVec3(v.rotateAxis(Vec3.init(1, 0, 0), 0), Vec3.init(0, 1, 0));
--     try eqApproxVec3(v.rotateAxis(Vec3.init(1, 0, 0), pif*0.5), Vec3.init(0, 0, 1));
--     try eqApproxVec3(v.rotateAxis(Vec3.init(1, 0, 0), pif), Vec3.init(0, -1, 0));
--     try eqApproxVec3(v.rotateAxis(Vec3.init(1, 0, 0), pif*1.5), Vec3.init(0, 0, -1));
-- }

type Vec4[T Any]:
    x T = 0
    y T = 0
    z T = 0
    w T = 0

fn Vec4[] :: @init(x, y, z, w T) -> Self:
    return {x=x, y=y, z=z, w=w}

-- Component addition.
fn (Vec4[]) `+`(v Self) -> Self:
    return {x=$x+v.x, y=$y+v.y, z=$z+v.z, w=$w+v.w}

-- Scalar multiplication.
fn (Vec4[]) `*`(s T) -> Self:
    return {x=$x*s, y=$y*s, z=$z*s, w=$w*s}

-- Scalar division.
fn (Vec4[]) `/`(s T) -> Self:
    return {x=$x/s, y=$y/s, z=$z/s, w=$w/s}

fn (Vec4[]) divW() -> Self:
    return {x=$x/$w, y=$y/$w, z=$z/$w, 1}

fn (Vec4[]) dot(v Self) -> T:
    return $x*v.x + $y*v.y + $z*v.z + $w*v.w
