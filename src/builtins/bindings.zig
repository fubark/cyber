const std = @import("std");
const stdx = @import("stdx");
const fatal = cy.fatal;
const builtin = @import("builtin");

const cy = @import("../cyber.zig");
const vmc = cy.vmc;
const types = cy.types;
const rt = cy.rt;
const sema = cy.sema;
const ir = cy.ir;
const bt = cy.types.BuiltinTypes;
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const TrackGlobalRC = vm_.TrackGlobalRC;
const fmt = @import("../fmt.zig");
const C = @import("../capi.zig");

const debug = builtin.mode == .Debug;
const log = cy.log.scoped(.bindings);

const NullId = std.math.maxInt(u32);

pub const Symbol = enum {
    bool,
    char,
    uchar,
    short,
    ushort,
    int,
    uint,
    long,
    ulong,
    usize,
    double,
    charPtr,
    voidPtr,
    funcPtr,
    void,

    little,
    big,

    left,
    right,
    ends,

    // Encoding.
    utf8,
    bytes,

    AssertError,
    EvalError,
    FileNotFound,
    MissingSymbol,
    EndOfStream,
    OutOfBounds,
    InvalidResult,
    InvalidArgument,
    InvalidSignature,
    InvalidRune,
    StreamTooLong,
    NotAllowed,
    Closed,
    PermissionDenied,
    UnknownError,
    Unicode,
    Unsupported,
    ParseError,

    running,
    paused,
    done,

    float,

    // Open modes.
    read,
    write,
    readWrite,

    // File types.
    file,
    dir,

    unknown,
};

const anyNone = cy.builtins.anyNone;
const anySome = cy.builtins.anySome;

pub fn getBuiltinSymbol(id: u64) ?Symbol {
    return std.meta.intToEnum(Symbol, id) catch {
        return null;
    };
}

pub fn bindCore(self: *cy.VM) !void {
    @branchHint(.cold);
    for (std.enums.values(Symbol)) |sym| {
        try ensureSymbol(self, @tagName(sym), sym);
    }
}

fn ensureSymbol(vm: *cy.VM, name: []const u8, sym: Symbol) !void {
    const id = try vm.sema.ensureSymbol(name);
    std.debug.assert(id == @intFromEnum(sym));
}

// Keep as reference in case resume should be a function call.
// Although it works, it requires native func calls to perform additional copies of pc and framePtr back to the eval loop,
// which is a bad tradeoff for every other function call that doesn't need to.
// One solution is to add another bytecode to call nativeFunc1 with control over execution context.
// fn fiberResume(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) Value {
//     const obj = recv.asHeapObject();
//     if (&obj.fiber != @ptrCast(*cy.VM, vm).curFiber) {
//         // Only resume fiber if it's not done.
//         if (obj.fiber.pc != NullId) {
//             // Obtain the startLocal from looking at previous inst operand.
//             const startLocal = (@ptrCast(*cy.VM, vm).pc - 14 + 1)[0].arg;
//             // Obtain previous framePtr by subtracting from args pointer.
//             const prevFramePtr = @intToPtr([*]Value, @intFromPtr(args - startLocal - 4));

//             const pcOffset = @intCast(u32, @intFromPtr(@ptrCast(*cy.VM, vm).pc) - @intFromPtr(@ptrCast(*cy.VM, vm).ops.ptr));
//             const res = cy.pushFiber(@ptrCast(*cy.VM, vm), pcOffset, prevFramePtr, &obj.fiber, startLocal);
//             @ptrCast(*cy.VM, vm).pc = res.pc;
//             @ptrCast(*cy.VM, vm).framePtr = res.framePtr;
//             return Value.None;
//         }
//     }
//     vm.releaseObject(obj);
//     return Value.None;
// }

pub fn byteNot(vm: *cy.VM) callconv(.c) Value {
    return Value.initMask8(~vm.getByte(0));
}

pub fn bytePow(vm: *cy.VM) callconv(.c) Value {
    const right = vm.getByte(1);
    if (right == 0) return vm.prepPanic("Division by zero.");
    return Value.initMask8(std.math.powi(i8, vm.getByte(0), right) catch |err| {
        switch (err) {
            error.Underflow => return vm.prepPanic("Underflow."),
            error.Overflow => return vm.prepPanic("Overflow."),
        }
    });
}

pub fn Int_gt_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    switch (ctx.func.parent.parent.?.cast(.type).instance.?.params[0].as_int_lit()) {
        8 => {
            const left = ctx.args[0].asI8();
            const right = ctx.args[1].asI8();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left > right));
        },
        16 => {
            const left = ctx.args[0].asI16();
            const right = ctx.args[1].asI16();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left > right));
        },
        32 => {
            const left = ctx.args[0].asI32();
            const right = ctx.args[1].asI32();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left > right));
        },
        64 => {
            const left = ctx.args[0].asInt();
            const right = ctx.args[1].asInt();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left > right));
        },
        else => @panic("unexpected"),
    }
}

pub fn Int_lt_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    switch (ctx.func.parent.parent.?.cast(.type).instance.?.params[0].as_int_lit()) {
        8 => {
            const left = ctx.args[0].asI8();
            const right = ctx.args[1].asI8();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left < right));
        },
        16 => {
            const left = ctx.args[0].asI16();
            const right = ctx.args[1].asI16();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left < right));
        },
        32 => {
            const left = ctx.args[0].asI32();
            const right = ctx.args[1].asI32();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left < right));
        },
        64 => {
            const left = ctx.args[0].asInt();
            const right = ctx.args[1].asInt();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left < right));
        },
        else => @panic("unexpected"),
    }
}

pub fn Int_uge_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    switch (ctx.func.parent.parent.?.cast(.type).instance.?.params[0].as_int_lit()) {
        8 => {
            const left = ctx.args[0].asU8();
            const right = ctx.args[1].asU8();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left >= right));
        },
        16 => {
            const left = ctx.args[0].asU16();
            const right = ctx.args[1].asU16();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left >= right));
        },
        32 => {
            const left = ctx.args[0].asU32();
            const right = ctx.args[1].asU32();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left >= right));
        },
        64 => {
            const left = ctx.args[0].asUint();
            const right = ctx.args[1].asUint();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left >= right));
        },
        else => @panic("unexpected"),
    }
}

pub fn Int_ugt_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    switch (ctx.func.parent.parent.?.cast(.type).instance.?.params[0].as_int_lit()) {
        8 => {
            const left = ctx.args[0].asU8();
            const right = ctx.args[1].asU8();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left > right));
        },
        16 => {
            const left = ctx.args[0].asU16();
            const right = ctx.args[1].asU16();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left > right));
        },
        32 => {
            const left = ctx.args[0].asU32();
            const right = ctx.args[1].asU32();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left > right));
        },
        64 => {
            const left = ctx.args[0].asUint();
            const right = ctx.args[1].asUint();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left > right));
        },
        else => @panic("unexpected"),
    }
}

pub fn Int_ult_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    switch (ctx.func.parent.parent.?.cast(.type).instance.?.params[0].as_int_lit()) {
        8 => {
            const left = ctx.args[0].asU8();
            const right = ctx.args[1].asU8();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left < right));
        },
        16 => {
            const left = ctx.args[0].asU16();
            const right = ctx.args[1].asU16();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left < right));
        },
        32 => {
            const left = ctx.args[0].asU32();
            const right = ctx.args[1].asU32();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left < right));
        },
        64 => {
            const left = ctx.args[0].asUint();
            const right = ctx.args[1].asUint();
            return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left < right));
        },
        else => @panic("unexpected"),
    }
}

pub fn Int_neg_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const val = ctx.args[0].asInt();
    return cy.TypeValue.init(c.sema.i64_t, Value.initInt(-val));
}

pub fn Int_neg(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const expr = ctx.args[0].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.neg, expr.type, ctx.node, .{
        .expr = expr,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_add(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.add, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_mul_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const arg_t = ctx.func.sig.params_ptr[0].get_type();
    if (arg_t.id() == bt.I64) {
        const left = ctx.args[0].asInt();
        const right = ctx.args[1].asInt();
        return cy.TypeValue.init(c.sema.i64_t, Value.initInt(left * right));
    } else {
        return c.reportErrorFmt("TODO: `{}`", &.{cy.fmt.v(arg_t.name())}, ctx.node);
    }
}

pub fn Int_div_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const arg_t = ctx.func.sig.params_ptr[0].get_type();
    if (arg_t.id() == bt.I64) {
        const left = ctx.args[0].asInt();
        const right = ctx.args[1].asInt();
        return cy.TypeValue.init(c.sema.i64_t, Value.initInt(@divTrunc(left, right)));
    } else {
        return c.reportErrorFmt("TODO: `{}`", &.{cy.fmt.v(arg_t.name())}, ctx.node);
    }
}

pub fn Int_add_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const arg_t = ctx.func.sig.params_ptr[0].get_type();
    if (arg_t.id() == bt.I64) {
        const left = ctx.args[0].asInt();
        const right = ctx.args[1].asInt();
        return cy.TypeValue.init(c.sema.i64_t, Value.initInt(left + right));
    } else {
        return c.reportErrorFmt("TODO: `{}`", &.{cy.fmt.v(arg_t.name())}, ctx.node);
    }
}

pub fn Int_sub_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const arg_t = ctx.func.sig.params_ptr[0].get_type();
    if (arg_t.id() == bt.I64) {
        const left = ctx.args[0].asInt();
        const right = ctx.args[1].asInt();
        return cy.TypeValue.init(c.sema.i64_t, Value.initInt(left - right));
    } else {
        return c.reportErrorFmt("TODO: `{}`", &.{cy.fmt.v(arg_t.name())}, ctx.node);
    }
}

pub fn Int_sub(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.sub, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_mul(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.imul, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_umul(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.mul, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_udiv(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.div, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_div(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.idiv, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_umod(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.mod, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_mod(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.imod, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_and(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.and_op, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_or(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.or_op, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_xor(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.xor, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_asr(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.asr, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_lsr(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.lsr, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_lsr_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const arg_t = ctx.func.sig.params_ptr[0].get_type();
    if (arg_t.id() == bt.I64) {
        const left = ctx.args[0].asInt();
        const right = ctx.args[1].asInt();
        return cy.TypeValue.init(arg_t, Value.initInt(left >> @intCast(right)));
    } else {
        return c.reportErrorFmt("TODO: `{}`", &.{cy.fmt.v(arg_t.name())}, ctx.node);
    }
}

pub fn Int_lsl_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const arg_t = ctx.func.sig.params_ptr[0].get_type();
    if (arg_t.id() == bt.I64) {
        const left = ctx.args[0].asInt();
        const right = ctx.args[1].asInt();
        return cy.TypeValue.init(arg_t, Value.initInt(left << @intCast(right)));
    } else {
        return c.reportErrorFmt("TODO: `{}`", &.{cy.fmt.v(arg_t.name())}, ctx.node);
    }
}

pub fn Int_lsl(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.lsl, left.type, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_not(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const expr = ctx.args[0].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.not, expr.type, ctx.node, .{
        .expr = expr,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn Int_not_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const arg_t = ctx.func.sig.params_ptr[0].get_type();
    switch (arg_t.id()) {
        bt.R8,
        bt.R16,
        bt.R32,
        bt.R64,
        bt.I8,
        bt.I16,
        bt.I32,
        bt.I64 => {
            return cy.TypeValue.init(arg_t, Value.initRaw(~ctx.args[0].val));
        },
        else => {
            const name = try c.sema.allocTypeName(arg_t);
            return c.reportErrorFmt("TODO: `{}`", &.{cy.fmt.v(name)}, ctx.node);
        },
    }
}

pub fn float_neg_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const float_t = ctx.func.sig.params_ptr[0].get_type();
    if (float_t.id() == bt.F64) {
        const val = ctx.args[0].asF64();
        return cy.TypeValue.init(c.sema.f64_t, Value.initFloat64(-val));
    } else {
        const val = ctx.args[0].asF32();
        return cy.TypeValue.init(c.sema.f32_t, Value.initFloat32(-val));
    }
}

pub fn float_neg(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const expr = ctx.args[0].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.fneg, ctx.func.sig.ret, ctx.node, .{
        .expr = expr,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn cmp(comptime cond: ir.Condition) fn(*cy.Chunk, *cy.CtFuncContext, *cy.sema.ExprResult) anyerror!void {
    const S = struct {
        fn func(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
            const left = ctx.args[0].asPtr(*ir.Expr);
            const right = ctx.args[1].asPtr(*ir.Expr);
            const loc = try c.ir.newExpr(.cmp, c.sema.bool_t, ctx.node, .{
                .left = left,
                .right = right,
                .cond = cond,
            });
            out.* = cy.sema.ExprResult.init2(loc);
        }
    };
    return S.func;
}

pub fn float_add(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.fadd, ctx.func.sig.ret, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn float_sub(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.fsub, ctx.func.sig.ret, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn float_abs(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const child = ctx.args[0].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.fabs, ctx.func.sig.ret, ctx.node, .{
        .expr = child,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn float_mul(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.fmul, ctx.func.sig.ret, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn float_mul_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const float_t = ctx.func.sig.params_ptr[0].get_type();
    if (float_t.id() == bt.F64) {
        const left = ctx.args[0].asF64();
        const right = ctx.args[1].asF64();
        return cy.TypeValue.init(c.sema.f64_t, Value.initFloat64(left * right));
    } else {
        const left = ctx.args[0].asF32();
        const right = ctx.args[1].asF32();
        return cy.TypeValue.init(c.sema.f32_t, Value.initFloat32(left * right));
    }
}

pub fn float_div(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.fdiv, ctx.func.sig.ret, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn float_mod(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *cy.sema.ExprResult) !void {
    const left = ctx.args[0].asPtr(*ir.Expr);
    const right = ctx.args[1].asPtr(*ir.Expr);
    const loc = try c.ir.newExpr(.fmod, ctx.func.sig.ret, ctx.node, .{
        .left = left,
        .right = right,
    });
    out.* = cy.sema.ExprResult.init2(loc);
}

pub fn float_pow(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = std.math.pow(f64, t.param(f64), t.param(f64));
    return C.RetOk;
}

pub fn float_pow32(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f32);
    ret.* = std.math.pow(f32, t.param(f32), t.param(f32));
    return C.RetOk;
}

pub const QuickenType = enum(u8) {
};

pub fn nop(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    return vm.returnPanic("Unsupported.");
}

pub const ModuleBuilder = struct {
    sym: *cy.Sym,
    chunk: *cy.Chunk,
    compiler: *cy.Compiler,
    vm: *cy.VM,

    pub fn init(c: *cy.Compiler, symOpt: ?*cy.Sym) ModuleBuilder {
        var new = ModuleBuilder{
            .sym = undefined,
            .compiler = c,
            .chunk = undefined,
            .vm = c.vm,
        };
        if (symOpt) |sym| {
            new.chunk = c.chunks.items[sym.declNode().src()];
            new.sym = sym;
        }
        return new;
    }

    pub fn setVar(self: *const ModuleBuilder, name: []const u8, typeId: types.TypeId, val: Value) !void {
        try self.getMod().setTypedVar(self.compiler, name, typeId, val);
    }

    pub fn declareFuncSig(self: *const ModuleBuilder, name: [*:0]const u8, params: []const *cy.Type, ret: *cy.Type, ptr: cy.ZHostFuncFn) !void {
        C.declareFunc(self.sym.toC(), name, @ptrCast(@constCast(params.ptr)), params.len, @ptrCast(ret), @ptrCast(ptr));
    }

    pub fn ensureMethodGroup(self: *const ModuleBuilder, name: []const u8) !vmc.MethodGroupId {
        return self.vm.ensureMethodGroup(name);
    }

    pub fn addOptimizingMethod(
        self: *const ModuleBuilder, typeId: cy.TypeId, mgId: vmc.MethodGroupId,
        params: []const types.TypeId, ret: types.TypeId, ptr: cy.QuickenFuncFn,
    ) !void {
        const funcSigId = try sema.ensureFuncSig(self.compiler, params, ret);
        const funcSig = self.compiler.sema.getFuncSig(funcSigId);
        if (funcSig.isParamsTyped) {
            return error.Unsupported;
        }
        try self.vm.addMethod(typeId, mgId, rt.MethodInit.initHostQuicken(funcSigId, ptr, @intCast(params.len)));
    }

    pub fn addMethod(
        self: *const ModuleBuilder, typeId: cy.TypeId, mgId: vmc.MethodGroupId,
        params: []const types.TypeId, ret: types.TypeId, ptr: cy.ZHostFuncFn,
    ) !void {
        const funcSigId = try self.compiler.sema.ensureFuncSig(params, ret);
        const funcSig = self.compiler.sema.getFuncSig(funcSigId);
        if (funcSig.reqCallTypeCheck) {
            try self.vm.addMethod(typeId, mgId, rt.MethodInit.initHostTyped(funcSigId, ptr, @intCast(params.len)));
        } else {
            try self.vm.addMethod(typeId, mgId, rt.MethodInit.initHostUntyped(funcSigId, ptr, @intCast(params.len)));
        }
    }

    pub fn getMod(self: *const ModuleBuilder) *cy.Module {
        return self.compiler.sema.getModulePtr(self.modId);
    }

    pub fn createAndSetTypeObject(self: *const ModuleBuilder, name: []const u8, fields: []const []const u8) !cy.TypeId {
        const sym = try self.chunk.declareObjectType(self.sym, name, cy.NullId);

        const modFields = try self.compiler.alloc.alloc(cy.sym.FieldInfo, fields.len);

        for (fields, 0..) |field, i| {
            const id = try self.vm.ensureFieldSym(field);
            try self.vm.addFieldSym(sym.type, id, @intCast(i), bt.Object);
            _ = try self.chunk.declareField(@ptrCast(sym), field, @intCast(i), bt.Object, cy.NullId);
        }
        sym.fields = modFields.ptr;
        sym.numFields = @intCast(modFields.len);
        return sym.type;
    }
};
