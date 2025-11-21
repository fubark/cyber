const std = @import("std");
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const vmc = cy.vmc;
const Value = cy.Value;
const bt = cy.types.BuiltinTypes;

pub const Src = @embedFile("math.cy");

comptime {
    @export(&bind, .{ .name = "cl_mod_math", .linkage = .strong });
}

pub fn bind(_: *cy.VM, mod: *C.Sym) callconv(.c) void {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }
}

const cFunc = cy.core.cFunc;

const funcs = [_]struct{[]const u8, C.BindFunc}{
    .{"abs",    cFunc(abs)},
    .{"acos",   cFunc(acos)},
    .{"acosh",  cFunc(acosh)},
    .{"asin",   cFunc(asin)},
    .{"asinh",  cFunc(asinh)},
    .{"atan",   cFunc(atan)},
    .{"atan2",  cFunc(atan2)},
    .{"atanh",  cFunc(atanh)},
    .{"cbrt",   cFunc(cbrt)},
    .{"ceil",   cFunc(ceil)},
    .{"clz32",  cFunc(clz32)},
    .{"c_cos",  cFunc(cos)},
    .{"c_cosf", cFunc(cosf)},
    .{"cosh",   cFunc(cosh)},
    .{"exp",    cFunc(exp)},
    .{"expm1",  cFunc(expm1)},
    .{"floor",  cFunc(floor)},
    .{"frac",   cFunc(frac)},
    .{"hypot",  cFunc(hypot)},
    .{"isInt",  cFunc(isInt)},
    .{"isNaN",  cFunc(isNaN)},
    .{"ln",     cFunc(ln)},
    .{"log",    cFunc(log)},
    .{"log10",  cFunc(log10)},
    .{"log1p",  cFunc(log1p)},
    .{"log2",   cFunc(log2)},
    .{"max_f64", cFunc(max_f64)},
    .{"min_f64", cFunc(min_f64)},
    .{"mul32",  cFunc(mul32)},
    .{"pow",    cFunc(pow)},
    .{"random", cFunc(random)},
    .{"round",  cFunc(round)},
    .{"sign",   cFunc(sign)},
    .{"c_sin",  cFunc(sin)},
    .{"c_sinf", cFunc(sinf)},
    .{"sinh",   cFunc(sinh)},
    .{"sqrt",   cFunc(sqrt)},
    .{"c_tan",  cFunc(tan)},
    .{"c_tanf", cFunc(tanf)},
    .{"tanh",   cFunc(tanh)},
    .{"trunc",  cFunc(trunc)},
};

pub fn abs(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = @abs(t.param(f64));
    return C.RetOk;
}

pub fn acos(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.acos(t.param(f64));
    return C.RetOk;
}

pub fn acosh(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.acosh(t.param(f64));
    return C.RetOk;
}

pub fn asin(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.asin(t.param(f64));
    return C.RetOk;
}

pub fn asinh(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.asinh(t.param(f64));
    return C.RetOk;
}

pub fn atan(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.atan(t.param(f64));
    return C.RetOk;
}

/// Returns the arctangent of the quotient of its arguments.
pub fn atan2(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.atan2(t.param(f64), t.param(f64));
    return C.RetOk;
}

/// Returns the hyperbolic arctangent of x.
pub fn atanh(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.atanh(t.param(f64));
    return C.RetOk;
}

/// Returns the cube root of x.
pub fn cbrt(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.cbrt(t.param(f64));
    return C.RetOk;
}

/// Returns the smallest integer greater than or equal to x.
pub fn ceil(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.ceil(t.param(f64));
    return C.RetOk;
}

/// Returns the number of leading zero bits of the 32-bit integer x.
pub fn clz32(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.ceil(@as(f64, @floatFromInt(@clz(@as(i32, @intFromFloat(t.param(f64)))))));
    return C.RetOk;
}

/// Returns the cosine of x.
pub fn cos(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.cos(t.param(f64));
    return C.RetOk;
}
pub fn cosf(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f32);
    ret.* = std.math.cos(t.param(f32));
    return C.RetOk;
}

/// Returns the hyperbolic cosine of x.
pub fn cosh(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.cosh(t.param(f64));
    return C.RetOk;
}

/// Returns ex, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm).
pub fn exp(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.exp(t.param(f64));
    return C.RetOk;
}

/// Returns subtracting 1 from exp(x).
pub fn expm1(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.expm1(t.param(f64));
    return C.RetOk;
}

/// Returns the largest integer less than or equal to x.
pub fn floor(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.floor(t.param(f64));
    return C.RetOk;
}

pub fn frac(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    const f = t.param(f64);
    ret.* = std.math.modf(f).fpart;
    return C.RetOk;
}

/// Returns the square root of the sum of squares of its arguments.
pub fn hypot(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.hypot(t.param(f64), t.param(f64));
    return C.RetOk;
}

pub fn isInt(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(bool);
    const f = t.param(f64);
    if (std.math.isNan(f) or std.math.isInf(f)) {
        ret.* = false;
    } else {
        ret.* = std.math.trunc(f) == f;
    }
    return C.RetOk;
}

/// Returns the absolute value of x.
pub fn isNaN(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(bool);
    ret.* = std.math.isNan(t.param(f64));
    return C.RetOk;
}

/// Returns the natural logarithm (㏒e; also, ㏑) of x.
pub fn ln(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = @log(t.param(f64));
    return C.RetOk;
}

/// Returns the logarithm of y with base x.
pub fn log(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.log(f64, t.param(f64), t.param(f64));
    return C.RetOk;
}

/// Returns the base-10 logarithm of x.
pub fn log10(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.log10(t.param(f64));
    return C.RetOk;
}

/// Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x.
pub fn log1p(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.log1p(t.param(f64));
    return C.RetOk;
}

/// Returns the base-2 logarithm of x.
pub fn log2(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.log2(t.param(f64));
    return C.RetOk;
}

/// Returns the largest of two numbers.
pub fn max_f64(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = @max(t.param(f64), t.param(f64));
    return C.RetOk;
}

/// Returns the smallest of two numbers.
pub fn min_f64(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = @min(t.param(f64), t.param(f64));
    return C.RetOk;
}

/// Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed.
pub fn mul32(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = @floatFromInt(@as(i32, @intFromFloat(t.param(f64))) *% @as(i32, @intFromFloat(t.param(f64))));
    return C.RetOk;
}

/// Returns base x to the exponent power y (that is, x^y).
pub fn pow(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.pow(f64, t.param(f64), t.param(f64));
    return C.RetOk;
}

/// Returns a pseudo-random number between 0 and 1.
pub var rand = std.Random.DefaultPrng.init(0);
pub fn random(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = rand.random().float(f64);
    return C.RetOk;
}

/// Returns the value of the number x rounded to the nearest integer.
pub fn round(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.round(t.param(f64));
    return C.RetOk;
}

/// Returns the sign of the x, indicating whether x is positive, negative, or zero.
pub fn sign(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.sign(t.param(f64));
    return C.RetOk;
}

/// Returns the sine of x.
pub fn sin(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.sin(t.param(f64));
    return C.RetOk;
}
pub fn sinf(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f32);
    ret.* = std.math.sin(t.param(f32));
    return C.RetOk;
}

/// Returns the hyperbolic sine of x.
pub fn sinh(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.sinh(t.param(f64));
    return C.RetOk;
}

/// Returns the positive square root of x.
pub fn sqrt(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.sqrt(t.param(f64));
    return C.RetOk;
}

/// Returns the tangent of x.
pub fn tan(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.tan(t.param(f64));
    return C.RetOk;
}
pub fn tanf(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f32);
    ret.* = std.math.tan(t.param(f32));
    return C.RetOk;
}

/// Returns the hyperbolic tangent of x.
pub fn tanh(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.tanh(t.param(f64));
    return C.RetOk;
}

/// Returns the integer portion of x, removing any fractional digits.
pub fn trunc(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.trunc(t.param(f64));
    return C.RetOk;
}