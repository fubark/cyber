const std = @import("std");
const cy = @import("../cyber.zig");
const Value = cy.Value;

pub fn initModule(c: *cy.VMcompiler) !cy.Module {
    var mod = cy.Module{
        .syms = .{},
        .chunkId = cy.NullId,
    };

    // Euler's number and the base of natural logarithms; approximately 2.718.
    try mod.setVar(c, "e", Value.initF64(std.math.e));

    // Infinity.
    try mod.setVar(c, "inf", Value.initF64(std.math.inf_f64));

    // Base-10 logarithm of E; approximately 0.434.
    try mod.setVar(c, "log10e", Value.initF64(std.math.log10e));

    // Base-2 logarithm of E; approximately 1.443.
    try mod.setVar(c, "log2e", Value.initF64(std.math.log2e));

    // Natural logarithm of 10; approximately 2.303.
    try mod.setVar(c, "ln10", Value.initF64(std.math.ln10));

    // Natural logarithm of 2; approximately 0.693.
    try mod.setVar(c, "ln2", Value.initF64(std.math.ln2));

    // Not a number.
    try mod.setVar(c, "nan", Value.initF64(-std.math.nan_f64));

    // Neg infinity.
    try mod.setVar(c, "neginf", Value.initF64(-std.math.inf_f64));

    // Ratio of a circle's circumference to its diameter; approximately 3.14159.
    try mod.setVar(c, "pi", Value.initF64(std.math.pi));

    // Square root of ½; approximately 0.707.
    try mod.setVar(c, "sqrt1_2", Value.initF64(std.math.sqrt1_2));

    // Square root of 2; approximately 1.414.
    try mod.setVar(c, "sqrt2", Value.initF64(std.math.sqrt2));

    try mod.setNativeFunc(c, "abs", 1, abs);
    try mod.setNativeFunc(c, "acos", 1, acos);
    try mod.setNativeFunc(c, "acosh", 1, acosh);
    try mod.setNativeFunc(c, "asin", 1, asin);
    try mod.setNativeFunc(c, "asinh", 1, asinh);
    try mod.setNativeFunc(c, "atan", 1, atan);
    try mod.setNativeFunc(c, "atan2", 2, atan2);
    try mod.setNativeFunc(c, "atanh", 1, atanh);
    try mod.setNativeFunc(c, "cbrt", 1, cbrt);
    try mod.setNativeFunc(c, "ceil", 1, ceil);
    try mod.setNativeFunc(c, "clz32", 1, clz32);
    try mod.setNativeFunc(c, "cos", 1, cos);
    try mod.setNativeFunc(c, "cosh", 1, cosh);
    try mod.setNativeFunc(c, "exp", 1, exp);
    try mod.setNativeFunc(c, "expm1", 1, expm1);
    try mod.setNativeFunc(c, "floor", 1, floor);
    try mod.setNativeFunc(c, "hypot", 2, hypot);
    try mod.setNativeFunc(c, "isNaN", 1, isNaN);
    try mod.setNativeFunc(c, "ln", 1, ln);
    try mod.setNativeFunc(c, "log", 2, log);
    try mod.setNativeFunc(c, "log10", 1, log10);
    try mod.setNativeFunc(c, "log1p", 1, log1p);
    try mod.setNativeFunc(c, "log2", 1, log2);
    try mod.setNativeFunc(c, "max", 2, max);
    try mod.setNativeFunc(c, "min", 2, min);
    try mod.setNativeFunc(c, "mul32", 2, mul32);
    try mod.setNativeFunc(c, "pow", 2, pow);
    try mod.setNativeFunc(c, "random", 0, random);
    try mod.setNativeFunc(c, "round", 1, round);
    try mod.setNativeFunc(c, "sign", 1, sign);
    try mod.setNativeFunc(c, "sin", 1, sin);
    try mod.setNativeFunc(c, "sinh", 1, sinh);
    try mod.setNativeFunc(c, "sqrt", 1, sqrt);
    try mod.setNativeFunc(c, "tan", 1, tan);
    try mod.setNativeFunc(c, "tanh", 1, tanh);
    try mod.setNativeFunc(c, "trunc", 1, trunc);
    return mod;
}

/// Returns the absolute value of x.
pub fn abs(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@fabs(args[0].toF64()));
}

/// Returns the arccosine of x.
pub fn acos(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.acos(args[0].toF64()));
}

/// Returns the hyperbolic arccosine of x.
pub fn acosh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.acosh(args[0].toF64()));
}

/// Returns the arcsine of x.
pub fn asin(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.asin(args[0].toF64()));
}

/// Returns the hyperbolic arcsine of a number.
pub fn asinh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.asinh(args[0].toF64()));
}

/// Returns the arctangent of x.
pub fn atan(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.atan(args[0].toF64()));
}

/// Returns the arctangent of the quotient of its arguments.
pub fn atan2(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.atan2(f64, args[0].toF64(), args[1].toF64()));
}

/// Returns the hyperbolic arctangent of x.
pub fn atanh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.atanh(args[0].toF64()));
}

/// Returns the cube root of x.
pub fn cbrt(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.cbrt(args[0].toF64()));
}

/// Returns the smallest integer greater than or equal to x.
pub fn ceil(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.ceil(args[0].toF64()));
}

/// Returns the number of leading zero bits of the 32-bit integer x.
pub fn clz32(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@intToFloat(f64, @clz(@floatToInt(i32, args[0].toF64()))));
}

/// Returns the cosine of x.
pub fn cos(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.cos(args[0].toF64()));
}

/// Returns the hyperbolic cosine of x.
pub fn cosh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.cosh(args[0].toF64()));
}

/// Returns ex, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm).
pub fn exp(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.exp(args[0].toF64()));
}

/// Returns subtracting 1 from exp(x).
pub fn expm1(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.expm1(args[0].toF64()));
}

/// Returns the largest integer less than or equal to x.
pub fn floor(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.floor(args[0].toF64()));
}

/// Returns the square root of the sum of squares of its arguments.
pub fn hypot(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.hypot(f64, args[0].toF64(), args[1].toF64()));
}

/// Returns the absolute value of x.
pub fn isNaN(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initBool(std.math.isNan(args[0].toF64()));
}

/// Returns the natural logarithm (㏒e; also, ㏑) of x.
pub fn ln(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.ln(args[0].toF64()));
}

/// Returns the logarithm of y with base x.
pub fn log(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log(f64, args[0].toF64(), args[1].toF64()));
}

/// Returns the base-10 logarithm of x.
pub fn log10(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log10(args[0].toF64()));
}

/// Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x.
pub fn log1p(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log1p(args[0].toF64()));
}

/// Returns the base-2 logarithm of x.
pub fn log2(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log2(args[0].toF64()));
}

/// Returns the largest of two numbers.
pub fn max(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.max(args[0].toF64(), args[1].toF64()));
}

/// Returns the smallest of two numbers.
pub fn min(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.min(args[0].toF64(), args[1].toF64()));
}

/// Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed.
pub fn mul32(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@intToFloat(f64, @floatToInt(i32, args[0].toF64()) *% @floatToInt(i32, args[1].toF64())));
}

/// Returns base x to the exponent power y (that is, x^y).
pub fn pow(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.pow(f64, args[0].toF64(), args[1].toF64()));
}

/// Returns a pseudo-random number between 0 and 1.
var rand = std.rand.DefaultPrng.init(0);
pub fn random(_: *cy.UserVM, _: [*]const Value, _: u8) Value {
    return Value.initF64(rand.random().float(f64));
}

/// Returns the value of the number x rounded to the nearest integer.
pub fn round(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.round(args[0].toF64()));
}

/// Returns the sign of the x, indicating whether x is positive, negative, or zero.
pub fn sign(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sign(args[0].toF64()));
}

/// Returns the sine of x.
pub fn sin(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sin(args[0].toF64()));
}

/// Returns the hyperbolic sine of x.
pub fn sinh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sinh(args[0].toF64()));
}

/// Returns the positive square root of x.
pub fn sqrt(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sqrt(args[0].toF64()));
}

/// Returns the tangent of x.
pub fn tan(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.tan(args[0].toF64()));
}

/// Returns the hyperbolic tangent of x.
pub fn tanh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.tanh(args[0].toF64()));
}

/// Returns the integer portion of x, removing any fractional digits.
pub fn trunc(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.trunc(args[0].toF64()));
}