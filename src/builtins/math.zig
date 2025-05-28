const std = @import("std");
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const vmc = cy.vmc;
const Value = cy.Value;
const bt = cy.types.BuiltinTypes;

const Src = @embedFile("math.cy");

pub fn create(vm: *cy.VM, r_uri: []const u8) C.Module {
    const mod = C.createModule(@ptrCast(vm), C.toStr(r_uri), C.toStr(Src));
    var config = C.ModuleConfig{
        .funcs = C.toSlice(C.HostFuncEntry, &funcs),
        .varLoader = varLoader,
        .types = C.toSlice(C.HostTypeEntry, &.{}),
    };
    C.setModuleConfig(@ptrCast(vm), mod, &config);
    return mod;
}

fn varLoader(_: ?*C.VM, v: C.VarInfo, out_: [*c]C.Value) callconv(.C) bool {
    const out: *C.Value = out_;
    const name = C.fromStr(v.name);
    if (std.mem.eql(u8, vars[v.idx].@"0", name)) {
        out.* = vars[v.idx].@"1".val;
        return true;
    }
    return false;
}

const NameVar = struct { []const u8, cy.Value };
const vars = [_]NameVar{
    .{"e", Value.initF64(std.math.e)},
    .{"inf", Value.initF64(std.math.inf(f64))},
    .{"log10e", Value.initF64(std.math.log10e)},
    .{"log2e", Value.initF64(std.math.log2e)},
    .{"ln10", Value.initF64(std.math.ln10)},
    .{"ln2", Value.initF64(std.math.ln2)},
    .{"maxSafeInt", Value.initF64(9007199254740991)},
    .{"minSafeInt", Value.initF64(-9007199254740991)},
    .{"nan", Value.initF64(std.math.nan(f64))},
    .{"neginf", Value.initF64(-std.math.inf(f64))},
    .{"pi", Value.initF64(std.math.pi)},
    .{"sqrt1_2", Value.initF64(std.math.sqrt1_2)},
    .{"sqrt2", Value.initF64(std.math.sqrt2)},
};

const func = cy.hostFuncEntry;
const funcs = [_]C.HostFuncEntry{
    func("abs",    abs),
    func("acos",   acos),
    func("acosh",  acosh),
    func("asin",   asin),
    func("asinh",  asinh),
    func("atan",   atan),
    func("atan2",  atan2),
    func("atanh",  atanh),
    func("cbrt",   cbrt),
    func("ceil",   ceil),
    func("clz32",  clz32),
    func("cos",    cos),
    func("cosh",   cosh),
    func("exp",    exp),
    func("expm1",  expm1),
    func("floor",  floor),
    func("frac",   frac),
    func("hypot",  hypot),
    func("isInt",  isInt),
    func("isNaN",  isNaN),
    func("ln",     ln),
    func("log",    log),
    func("log10",  log10),
    func("log1p",  log1p),
    func("log2",   log2),
    func("max",    max),
    func("min",    min),
    func("mul32",  mul32),
    func("pow",    pow),
    func("random", random),
    func("round",  round),
    func("sign",   sign),
    func("sin",    sin),
    func("sinh",   sinh),
    func("sqrt",   sqrt),
    func("tan",    tan),
    func("tanh",   tanh),
    func("trunc",  trunc),
};

/// Returns the absolute value of x.
pub fn abs(vm: *cy.VM) Value {
    return Value.initF64(@abs(vm.getFloat(0)));
}

/// Returns the arccosine of x.
pub fn acos(vm: *cy.VM) Value {
    return Value.initF64(std.math.acos(vm.getFloat(0)));
}

/// Returns the hyperbolic arccosine of x.
pub fn acosh(vm: *cy.VM) Value {
    return Value.initF64(std.math.acosh(vm.getFloat(0)));
}

/// Returns the arcsine of x.
pub fn asin(vm: *cy.VM) Value {
    return Value.initF64(std.math.asin(vm.getFloat(0)));
}

/// Returns the hyperbolic arcsine of a number.
pub fn asinh(vm: *cy.VM) Value {
    return Value.initF64(std.math.asinh(vm.getFloat(0)));
}

/// Returns the arctangent of x.
pub fn atan(vm: *cy.VM) Value {
    return Value.initF64(std.math.atan(vm.getFloat(0)));
}

/// Returns the arctangent of the quotient of its arguments.
pub fn atan2(vm: *cy.VM) Value {
    return Value.initF64(std.math.atan2(vm.getFloat(0), vm.getFloat(1)));
}

/// Returns the hyperbolic arctangent of x.
pub fn atanh(vm: *cy.VM) Value {
    return Value.initF64(std.math.atanh(vm.getFloat(0)));
}

/// Returns the cube root of x.
pub fn cbrt(vm: *cy.VM) Value {
    return Value.initF64(std.math.cbrt(vm.getFloat(0)));
}

/// Returns the smallest integer greater than or equal to x.
pub fn ceil(vm: *cy.VM) Value {
    return Value.initF64(std.math.ceil(vm.getFloat(0)));
}

/// Returns the number of leading zero bits of the 32-bit integer x.
pub fn clz32(vm: *cy.VM) Value {
    return Value.initF64(@floatFromInt(@clz(@as(i32, @intFromFloat(vm.getFloat(0))))));
}

/// Returns the cosine of x.
pub fn cos(vm: *cy.VM) Value {
    return Value.initF64(std.math.cos(vm.getFloat(0)));
}

/// Returns the hyperbolic cosine of x.
pub fn cosh(vm: *cy.VM) Value {
    return Value.initF64(std.math.cosh(vm.getFloat(0)));
}

/// Returns ex, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm).
pub fn exp(vm: *cy.VM) Value {
    return Value.initF64(std.math.exp(vm.getFloat(0)));
}

/// Returns subtracting 1 from exp(x).
pub fn expm1(vm: *cy.VM) Value {
    return Value.initF64(std.math.expm1(vm.getFloat(0)));
}

/// Returns the largest integer less than or equal to x.
pub fn floor(vm: *cy.VM) Value {
    return Value.initF64(std.math.floor(vm.getFloat(0)));
}

pub fn frac(vm: *cy.VM) Value {
    const f = vm.getFloat(0);
    const res = std.math.modf(f);
    return Value.initF64(res.fpart);
}

/// Returns the square root of the sum of squares of its arguments.
pub fn hypot(vm: *cy.VM) Value {
    return Value.initF64(std.math.hypot(vm.getFloat(0), vm.getFloat(1)));
}

pub fn isInt(vm: *cy.VM) Value {
    const f = vm.getFloat(0);
    if (std.math.isNan(f) or std.math.isInf(f)) {
        return Value.False;
    }
    return Value.initBool(std.math.trunc(f) == f);
}

/// Returns the absolute value of x.
pub fn isNaN(vm: *cy.VM) Value {
    return Value.initBool(std.math.isNan(vm.getFloat(0)));
}

/// Returns the natural logarithm (㏒e; also, ㏑) of x.
pub fn ln(vm: *cy.VM) Value {
    return Value.initF64(@log(vm.getFloat(0)));
}

/// Returns the logarithm of y with base x.
pub fn log(vm: *cy.VM) Value {
    return Value.initF64(std.math.log(f64, vm.getFloat(0), vm.getFloat(1)));
}

/// Returns the base-10 logarithm of x.
pub fn log10(vm: *cy.VM) Value {
    return Value.initF64(std.math.log10(vm.getFloat(0)));
}

/// Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x.
pub fn log1p(vm: *cy.VM) Value {
    return Value.initF64(std.math.log1p(vm.getFloat(0)));
}

/// Returns the base-2 logarithm of x.
pub fn log2(vm: *cy.VM) Value {
    return Value.initF64(std.math.log2(vm.getFloat(0)));
}

/// Returns the largest of two numbers.
pub fn max(vm: *cy.VM) Value {
    return Value.initF64(@max(vm.getFloat(0), vm.getFloat(1)));
}

/// Returns the smallest of two numbers.
pub fn min(vm: *cy.VM) Value {
    return Value.initF64(@min(vm.getFloat(0), vm.getFloat(1)));
}

/// Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed.
pub fn mul32(vm: *cy.VM) Value {
    return Value.initF64(@floatFromInt(@as(i32, @intFromFloat(vm.getFloat(0))) *% @as(i32, @intFromFloat(vm.getFloat(1)))));
}

/// Returns base x to the exponent power y (that is, x^y).
pub fn pow(vm: *cy.VM) Value {
    return Value.initF64(std.math.pow(f64, vm.getFloat(0), vm.getFloat(1)));
}

/// Returns a pseudo-random number between 0 and 1.
pub var rand = std.Random.DefaultPrng.init(0);
pub fn random(_: *cy.VM) Value {
    return Value.initF64(rand.random().float(f64));
}

/// Returns the value of the number x rounded to the nearest integer.
pub fn round(vm: *cy.VM) Value {
    return Value.initF64(std.math.round(vm.getFloat(0)));
}

/// Returns the sign of the x, indicating whether x is positive, negative, or zero.
pub fn sign(vm: *cy.VM) Value {
    return Value.initF64(std.math.sign(vm.getFloat(0)));
}

/// Returns the sine of x.
pub fn sin(vm: *cy.VM) Value {
    return Value.initF64(std.math.sin(vm.getFloat(0)));
}

/// Returns the hyperbolic sine of x.
pub fn sinh(vm: *cy.VM) Value {
    return Value.initF64(std.math.sinh(vm.getFloat(0)));
}

/// Returns the positive square root of x.
pub fn sqrt(vm: *cy.VM) Value {
    return Value.initF64(std.math.sqrt(vm.getFloat(0)));
}

/// Returns the tangent of x.
pub fn tan(vm: *cy.VM) Value {
    return Value.initF64(std.math.tan(vm.getFloat(0)));
}

/// Returns the hyperbolic tangent of x.
pub fn tanh(vm: *cy.VM) Value {
    return Value.initF64(std.math.tanh(vm.getFloat(0)));
}

/// Returns the integer portion of x, removing any fractional digits.
pub fn trunc(vm: *cy.VM) Value {
    return Value.initF64(std.math.trunc(vm.getFloat(0)));
}