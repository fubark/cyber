const std = @import("std");
const cy = @import("../cyber.zig");
const Value = cy.Value;
const bt = cy.types.BuiltinTypeSymIds;

pub fn initModule(c: *cy.VMcompiler, modId: cy.ModuleId) anyerror!void {
    const b = cy.bindings.ModuleBuilder.init(c, modId);

    // Euler's number and the base of natural logarithms; approximately 2.718.
    try b.setVar("e", bt.Float, Value.initF64(std.math.e));

    // Infinity.
    try b.setVar("inf", bt.Float, Value.initF64(std.math.inf(f64)));

    // Base-10 logarithm of E; approximately 0.434.
    try b.setVar("log10e", bt.Float, Value.initF64(std.math.log10e));

    // Base-2 logarithm of E; approximately 1.443.
    try b.setVar("log2e", bt.Float, Value.initF64(std.math.log2e));

    // Natural logarithm of 10; approximately 2.303.
    try b.setVar("ln10", bt.Float, Value.initF64(std.math.ln10));

    // Natural logarithm of 2; approximately 0.693.
    try b.setVar("ln2", bt.Float, Value.initF64(std.math.ln2));

    // Not a number.
    try b.setVar("nan", bt.Float, Value.initF64(-std.math.nan_f64));

    // Neg infinity.
    try b.setVar("neginf", bt.Float, Value.initF64(-std.math.inf(f64)));

    // Ratio of a circle's circumference to its diameter; approximately 3.14159.
    try b.setVar("pi", bt.Float, Value.initF64(std.math.pi));

    // Square root of ½; approximately 0.707.
    try b.setVar("sqrt1_2", bt.Float, Value.initF64(std.math.sqrt1_2));

    // Square root of 2; approximately 1.414.
    try b.setVar("sqrt2", bt.Float, Value.initF64(std.math.sqrt2));

    const num1: []const cy.sema.ResolvedSymId = &.{bt.Float};
    const num2: []const cy.sema.ResolvedSymId = &.{bt.Float, bt.Float};

    try b.setFunc("abs",    num1, bt.Float, abs);
    try b.setFunc("acos",   num1, bt.Float, acos);
    try b.setFunc("acosh",  num1, bt.Float, acosh);
    try b.setFunc("asin",   num1, bt.Float, asin);
    try b.setFunc("asinh",  num1, bt.Float, asinh);
    try b.setFunc("atan",   num1, bt.Float, atan);
    try b.setFunc("atan2",  num2, bt.Float, atan2);
    try b.setFunc("atanh",  num1, bt.Float, atanh);
    try b.setFunc("cbrt",   num1, bt.Float, cbrt);
    try b.setFunc("ceil",   num1, bt.Float, ceil);
    try b.setFunc("clz32",  num1, bt.Float, clz32);
    try b.setFunc("cos",    num1, bt.Float, cos);
    try b.setFunc("cosh",   num1, bt.Float, cosh);
    try b.setFunc("exp",    num1, bt.Float, exp);
    try b.setFunc("expm1",  num1, bt.Float, expm1);
    try b.setFunc("floor",  num1, bt.Float, floor);
    try b.setFunc("hypot",  num2, bt.Float, hypot);
    try b.setFunc("isNaN",  num1, bt.Float, isNaN);
    try b.setFunc("ln",     num1, bt.Float, ln);
    try b.setFunc("log",    num2, bt.Float, log);
    try b.setFunc("log10",  num1, bt.Float, log10);
    try b.setFunc("log1p",  num1, bt.Float, log1p);
    try b.setFunc("log2",   num1, bt.Float, log2);
    try b.setFunc("max",    num2, bt.Float, max);
    try b.setFunc("min",    num2, bt.Float, min);
    try b.setFunc("mul32",  num2, bt.Float, mul32);
    try b.setFunc("pow",    num2, bt.Float, pow);
    try b.setFunc("random", &.{}, bt.Float, random);
    try b.setFunc("round",  num1, bt.Float, round);
    try b.setFunc("sign",   num1, bt.Float, sign);
    try b.setFunc("sin",    num1, bt.Float, sin);
    try b.setFunc("sinh",   num1, bt.Float, sinh);
    try b.setFunc("sqrt",   num1, bt.Float, sqrt);
    try b.setFunc("tan",    num1, bt.Float, tan);
    try b.setFunc("tanh",   num1, bt.Float, tanh);
    try b.setFunc("trunc",  num1, bt.Float, trunc);
}

/// Returns the absolute value of x.
pub fn abs(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@fabs(args[0].asF64()));
}

/// Returns the arccosine of x.
pub fn acos(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.acos(args[0].asF64()));
}

/// Returns the hyperbolic arccosine of x.
pub fn acosh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.acosh(args[0].asF64()));
}

/// Returns the arcsine of x.
pub fn asin(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.asin(args[0].asF64()));
}

/// Returns the hyperbolic arcsine of a number.
pub fn asinh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.asinh(args[0].asF64()));
}

/// Returns the arctangent of x.
pub fn atan(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.atan(args[0].asF64()));
}

/// Returns the arctangent of the quotient of its arguments.
pub fn atan2(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.atan2(f64, args[0].asF64(), args[1].asF64()));
}

/// Returns the hyperbolic arctangent of x.
pub fn atanh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.atanh(args[0].asF64()));
}

/// Returns the cube root of x.
pub fn cbrt(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.cbrt(args[0].asF64()));
}

/// Returns the smallest integer greater than or equal to x.
pub fn ceil(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.ceil(args[0].asF64()));
}

/// Returns the number of leading zero bits of the 32-bit integer x.
pub fn clz32(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@floatFromInt(@clz(@as(i32, @intFromFloat(args[0].asF64())))));
}

/// Returns the cosine of x.
pub fn cos(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.cos(args[0].asF64()));
}

/// Returns the hyperbolic cosine of x.
pub fn cosh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.cosh(args[0].asF64()));
}

/// Returns ex, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm).
pub fn exp(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.exp(args[0].asF64()));
}

/// Returns subtracting 1 from exp(x).
pub fn expm1(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.expm1(args[0].asF64()));
}

/// Returns the largest integer less than or equal to x.
pub fn floor(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.floor(args[0].asF64()));
}

/// Returns the square root of the sum of squares of its arguments.
pub fn hypot(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.hypot(f64, args[0].asF64(), args[1].asF64()));
}

/// Returns the absolute value of x.
pub fn isNaN(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initBool(std.math.isNan(args[0].asF64()));
}

/// Returns the natural logarithm (㏒e; also, ㏑) of x.
pub fn ln(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@log(args[0].asF64()));
}

/// Returns the logarithm of y with base x.
pub fn log(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log(f64, args[0].asF64(), args[1].asF64()));
}

/// Returns the base-10 logarithm of x.
pub fn log10(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log10(args[0].asF64()));
}

/// Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x.
pub fn log1p(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log1p(args[0].asF64()));
}

/// Returns the base-2 logarithm of x.
pub fn log2(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log2(args[0].asF64()));
}

/// Returns the largest of two numbers.
pub fn max(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@max(args[0].asF64(), args[1].asF64()));
}

/// Returns the smallest of two numbers.
pub fn min(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@min(args[0].asF64(), args[1].asF64()));
}

/// Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed.
pub fn mul32(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@floatFromInt(@as(i32, @intFromFloat(args[0].asF64())) *% @as(i32, @intFromFloat(args[1].asF64()))));
}

/// Returns base x to the exponent power y (that is, x^y).
pub fn pow(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.pow(f64, args[0].asF64(), args[1].asF64()));
}

/// Returns a pseudo-random number between 0 and 1.
pub var rand = std.rand.DefaultPrng.init(0);
pub fn random(_: *cy.UserVM, _: [*]const Value, _: u8) Value {
    return Value.initF64(rand.random().float(f64));
}

/// Returns the value of the number x rounded to the nearest integer.
pub fn round(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.round(args[0].asF64()));
}

/// Returns the sign of the x, indicating whether x is positive, negative, or zero.
pub fn sign(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sign(args[0].asF64()));
}

/// Returns the sine of x.
pub fn sin(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sin(args[0].asF64()));
}

/// Returns the hyperbolic sine of x.
pub fn sinh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sinh(args[0].asF64()));
}

/// Returns the positive square root of x.
pub fn sqrt(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sqrt(args[0].asF64()));
}

/// Returns the tangent of x.
pub fn tan(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.tan(args[0].asF64()));
}

/// Returns the hyperbolic tangent of x.
pub fn tanh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.tanh(args[0].asF64()));
}

/// Returns the integer portion of x, removing any fractional digits.
pub fn trunc(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.trunc(args[0].asF64()));
}