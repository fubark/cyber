const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const cy = @import("../cyber.zig");
const cc = @import("../capi.zig");
const vmc = cy.vmc;
const Value = cy.Value;
const fmt = @import("../fmt.zig");
const bindings = @import("../builtins/bindings.zig");
const Symbol = bindings.Symbol;
const prepareThrowSymbol = bindings.prepareThrowSymbol;
const bt = cy.types.BuiltinTypes;
const v = fmt.v;
const rt = cy.rt;
const log = cy.log.scoped(.testmod);

pub const Src = @embedFile("test.cy");
pub fn funcLoader(_: ?*cc.VM, func: cc.FuncInfo, out_: [*c]cc.FuncResult) callconv(.C) bool {
    const out: *cc.FuncResult = out_;
    const name = cc.strSlice(func.name);
    if (std.mem.eql(u8, funcs[func.idx].@"0", name)) {
        out.ptr = @ptrCast(funcs[func.idx].@"1");
        return true;
    }
    return false;
}

const zErrFunc = cy.builtins.zErrFunc2;
const cFunc = cy.builtins.cFunc;

const NameHostFunc = struct { []const u8, cy.ZHostFuncFn };
const funcs = [_]NameHostFunc{
    .{"assert", zErrFunc(assert)},
    .{"eq", eq},
    .{"eqList", zErrFunc(eqList)},
    .{"eqNear", zErrFunc(eqNear)},
    .{"fail", fail},
};

pub fn onLoad(vm_: ?*cc.VM, mod: cc.ApiModule) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const b = bindings.ModuleBuilder.init(vm.compiler, @ptrCast(@alignCast(mod.sym)));
    if (builtin.is_test) {
        // Only available for zig test, until `any` local type specifier is implemented.
        b.declareFuncSig("erase", &.{bt.Any}, bt.Dynamic, erase) catch cy.fatal();
    }
}

fn fail(vm: *cy.VM, _: [*]const Value, _: u8) Value {
    return rt.prepThrowError(vm, .AssertError);
}

/// Simply returns the value so the caller get's an erased `any` type.
fn erase(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    vm.retain(args[0]);
    return args[0];
}

fn getComparableTag(val: Value) cy.ValueUserTag {
    return val.getUserTag();
}

pub fn eq(vm: *cy.VM, args: [*]const cy.Value, _: u8) Value {
    const res = eq_c(vm, args[0], args[1]);
    if (res.hasError()) {
        return Value.Interrupt;
    }
    return Value.initBool(res.val);
}

pub fn eq_c(c: cy.Context, act: rt.Any, exp: rt.Any) callconv(.C) rt.ErrorUnion(bool) {
    if (eq2(c, act, exp)) {
        return rt.wrapErrorValue(bool, true);
    } else {
        const err = cy.builtins.prepThrowZError2(c, error.AssertError, null);
        return rt.wrapError(bool, err);
    }
}

fn eq2(c: cy.Context, act: rt.Any, exp: rt.Any) bool {
    const act_t = act.getTypeId();
    const exp_t = exp.getTypeId();

    if (act_t != exp_t) {
        rt.errFmt(c, "Types do not match:", &.{});
        rt.errFmt(c, "actual: {} != {}", &.{v(rt.getTypeName(c, act_t)), v(rt.getTypeName(c, exp_t))});
        return false;
    }

    // PM compares by value if `is_struct` is true. Compares reference otherwise.
    // VM checks the type id and performs unboxing.
    if (build_options.rt == .pm) {
        if (act.vtable.type.is_struct) {
            const size = act.vtable.type.data.@"struct".size;
            if (size <= 8) {
                const act_s = std.mem.asBytes(&act.value)[0..size];
                const exp_s = std.mem.asBytes(&exp.value)[0..size];
                if (std.mem.eql(u8, act_s, exp_s)) {
                    return true;
                } else {
                    rt.errZFmt(c, "actual: {any} != {any}", .{act_s, exp_s});
                    return false;
                }
            } else {
                cy.panic("TODO");
            }
        } else {
            if (act.value == exp.value) {
                return true;
            } else {
                rt.errFmt(c, "actual: {} != {}", &.{v(act.value), v(exp.value)});
                return false;
            }
        }
    } else {
        switch (act_t) {
            bt.Integer => {
                if (act.asInteger() == exp.asInteger()) {
                    return true;
                } else {
                    rt.errFmt(c, "actual: {} != {}", &.{v(act.asInteger()), v(exp.asInteger())});
                    return false;
                }
            },
            bt.Float => {
                if (act.asF64() == exp.asF64()) {
                    return true;
                } else {
                    rt.errFmt(c, "actual: {} != {}", &.{v(act.asF64()), v(exp.asF64())});
                    return false;
                }
            },
            bt.String => {
                const actStr = act.asString();
                const expStr = exp.asString();
                if (std.mem.eql(u8, actStr, expStr)) {
                    return true;
                } else {
                    rt.errFmt(c, "actual: '{}' != '{}'", &.{v(actStr), v(expStr)});
                    return false;
                }
            },
            bt.Array => {
                const actStr = act.asArray();
                const expStr = exp.asArray();
                if (std.mem.eql(u8, actStr, expStr)) {
                    return true;
                } else {
                    rt.errFmt(c, "actual: '{}' != '{}'", &.{v(actStr), v(expStr)});
                    return false;
                }
            },
            bt.Pointer => {
                const actPtr = act.castHeapObject(*cy.Pointer).ptr;
                const expPtr = exp.castHeapObject(*cy.Pointer).ptr;
                if (actPtr == expPtr) {
                    return true;
                } else {
                    rt.errFmt(c, "actual: {} != {}", &.{v(actPtr), v(expPtr)});
                    return false;
                }
            },
            bt.Boolean => {
                const actv = act.asBool();
                const expv = exp.asBool();
                if (actv == expv) {
                    return true;
                } else {
                    rt.errFmt(c, "actual: {} != {}", &.{v(actv), v(expv)});
                    return false;
                }
            },
            bt.Symbol => {
                const actv = act.asSymbolId();
                const expv = exp.asSymbolId();
                if (actv == expv) {
                    return true;
                } else {
                    rt.errFmt(c, "actual: {} != {}", &.{v(rt.getSymName(c, actv)), v(rt.getSymName(c, expv))});
                    return false;
                }
            },
            bt.Error => {
                const actv = act.asErrorSymbol();
                const expv = exp.asErrorSymbol();
                if (actv == expv) {
                    return true;
                } else {
                    const actName: []const u8 = if (act.isInterrupt()) "Interrupt" else rt.getSymName(c, actv);
                    const expName: []const u8 = if (exp.isInterrupt()) "Interrupt" else rt.getSymName(c, expv);
                    rt.errFmt(c, "actual: error.{} != error.{}", &.{v(actName), v(expName)});
                    return false;
                }
            },
            bt.MetaType => {
                const actv = act.asHeapObject().metatype;
                const expv = exp.asHeapObject().metatype;
                if (std.meta.eql(actv, expv)) {
                    return true;
                } else {
                    rt.errFmt(c, "actual: {} != {}", &.{v(actv.type), v(expv.type)});
                    return false;
                }
            },
            bt.Map,
            bt.List => {
                const actv = act.asAnyOpaque();
                const expv = exp.asAnyOpaque();
                if (actv == expv) {
                    return true;
                } else {
                    rt.errFmt(c, "actual: {} != {}", &.{v(actv), v(expv)});
                    return false;
                }
            },
            else => {
                if (act_t < cy.types.BuiltinEnd) {
                    cy.panicFmt("Unsupported type {}", .{act_t});
                } else {
                    const actv = act.asAnyOpaque();
                    const expv = exp.asAnyOpaque();
                    if (actv == expv) {
                        return true;
                    } else {
                        rt.errFmt(c, "actual: {} != {}", &.{v(actv), v(expv)});
                        return false;
                    }
                }
            }
        }
    }
}

pub fn assert(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (args[0].asBool()) {
        return Value.Void;
    } else {
        rt.errFmt(vm, "Assertion failed.", &.{});
        return rt.prepThrowError(vm, .AssertError);
    }
}

pub fn eqNear(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const act = args[0];
    const exp = args[1];

    const actType = act.getUserTag();
    const expType = exp.getUserTag();
    if (actType == expType) {
        if (actType == .float) {
            if (std.math.approxEqAbs(f64, act.asF64(), exp.asF64(), 1e-5)) {
                return Value.True;
            } else {
                rt.errFmt(vm, "actual: {} != {}", &.{v(act.asF64()), v(exp.asF64())});
                return rt.prepThrowError(vm, .AssertError);
            }
        } else {
            rt.errFmt(vm, "Expected float, actual: {}", &.{v(actType)});
            return rt.prepThrowError(vm, .AssertError);
        }
    } else {
        rt.errFmt(vm, "Types do not match:", &.{});
        rt.errFmt(vm, "actual: {} != {}", &.{v(actType), v(expType)});
        return rt.prepThrowError(vm, .AssertError);
    }
}

pub fn eqList(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const act = args[0];
    const exp = args[1];

    const actType = act.getUserTag();
    const expType = exp.getUserTag();
    if (actType == expType) {
        if (actType == .list) {
            const acto = act.asHeapObject();
            const expo = exp.asHeapObject();
            if (acto.list.list.len == expo.list.list.len) {
                var i: u32 = 0;
                const actItems = acto.list.items();
                const expItems = expo.list.items();
                while (i < acto.list.list.len) : (i += 1) {
                    if (!eq2(vm, actItems[i], expItems[i])) {
                        rt.errFmt(vm, "Item mismatch at idx: {}", &.{v(i)});
                        return rt.prepThrowError(vm, .AssertError);
                    }
                }
                return Value.True;
            } else {
                rt.errFmt(vm, "actual list len: {} != {}", &.{v(acto.list.list.len), v(expo.list.list.len)});
                return rt.prepThrowError(vm, .AssertError);
            }
        } else {
            rt.errFmt(vm, "Expected list, actual: {}", &.{v(actType)});
            return rt.prepThrowError(vm, .AssertError);
        }
    } else {
        rt.errFmt(vm, "Types do not match:", &.{});
        rt.errFmt(vm, "actual: {} != {}", &.{v(actType), v(expType)});
        return rt.prepThrowError(vm, .AssertError);
    }
}