const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const vmc = cy.vmc;
const Value = cy.Value;
const fmt = @import("../fmt.zig");
const bindings = @import("../builtins/bindings.zig");
const Symbol = bindings.Symbol;
const bt = cy.types.BuiltinTypes;
const v = fmt.v;
const rt = cy.rt;
const log = cy.log.scoped(.testmod);

pub const Src = @embedFile("test.cy");

const zErrFunc = cy.builtins.zErrFunc;

const funcs = [_]struct{[]const u8, C.BindFunc}{
};

pub fn bind(_: *cy.VM, mod: *C.Sym) callconv(.c) void {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }

    C.mod_on_load(mod, onLoad);
}

fn funcLoader(_: ?*C.VM, info: C.FuncInfo, out: [*c]C.BindFunc) callconv(.c) bool {
    const name = C.from_bytes(info.name);
    if (funcs.get(name)) |func| {
        out.* = func;
        return true;
    }
    return false;
}

fn onLoad(vm_: ?*C.VM, mod: ?*C.Sym) callconv(.c) void {
    const chunk: *cy.sym.Chunk = @ptrCast(@alignCast(mod));
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const b = bindings.ModuleBuilder.init(vm.compiler, @ptrCast(chunk));
    _ = b;
}

/// Simply returns the value so the caller get's an erased `any` type.
fn erase(vm: *cy.VM) Value {
    vm.retain(vm.getValue(0));
    return vm.getValue(0);
}

/// TODO: remove
pub fn eq(vm: *cy.VM) Value {
    var type_id: cy.TypeId = @intCast(vm.getInt(0));

    var act = vm.getValue(1);
    var exp = vm.getValue(2);
    if (type_id == bt.Any or type_id == bt.Dyn) {
        const act_t = act.getBoxType();
        const exp_t = exp.getBoxType();
        if (act_t != exp_t) {
            rt.errFmt(vm, "Types do not match:\n", &.{});
            rt.errFmt(vm, "actual: {} != {}\n", &.{v(rt.getTypeName(vm, act_t)), v(rt.getTypeName(vm, exp_t))});
            return cy.builtins.prepThrowZError(vm, error.AssertError, null);
        }
        if (!vm.getType(act_t).isBoxed()) {
            act = cy.vm.unbox(vm, act, act_t);
            exp = cy.vm.unbox(vm, exp, act_t);
        }
        type_id = act_t;
    }
    const res = eq2(vm, type_id, act, exp);
    return cy.Value.initBool(res);
}

/// Assumes unboxed values.
fn eq2(c: *cy.VM, type_id: cy.TypeId, act: cy.Value, exp: cy.Value) bool {
    if (type_id == bt.Integer) {
        if (act.val == exp.val) {
            return true;
        } else {
            rt.errZFmt(c, "actual: {}, expected: {}\n", .{act.asInt(), exp.asInt()});
            return false;
        }
    }

    // PM compares by value if `is_struct` is true. Compares reference otherwise.
    // VM checks the type id and performs unboxing.
    switch (type_id) {
        bt.Void => {
            return true;
        },
        bt.Byte => {
            if (act.asByte() == exp.asByte()) {
                return true;
            } else {
                rt.errFmt(c, "actual: {} != {}\n", &.{v(act.asByte()), v(exp.asByte())});
                return false;
            }
        },
        bt.Integer => {
            if (act.asInt() == exp.asInt()) {
                return true;
            } else {
                rt.errFmt(c, "actual: {} != {}\n", &.{v(act.asInt()), v(exp.asInt())});
                return false;
            }
        },
        bt.Float => {
            if (act.asF64() == exp.asF64()) {
                return true;
            } else {
                rt.errFmt(c, "actual: {} != {}\n", &.{v(act.asF64()), v(exp.asF64())});
                return false;
            }
        },
        bt.String => {
            const actStr = act.asString();
            const expStr = exp.asString();
            if (std.mem.eql(u8, actStr, expStr)) {
                return true;
            } else {
                rt.errFmt(c, "actual: '{}' != '{}'\n", &.{v(actStr), v(expStr)});
                return false;
            }
        },
        bt.Boolean => {
            const actv = act.asBool();
            const expv = exp.asBool();
            if (actv == expv) {
                return true;
            } else {
                rt.errFmt(c, "actual: {} != {}\n", &.{v(actv), v(expv)});
                return false;
            }
        },
        bt.Symbol => {
            const actv = act.asSymbolId();
            const expv = exp.asSymbolId();
            if (actv == expv) {
                return true;
            } else {
                rt.errFmt(c, "actual: {} != {}\n", &.{v(rt.getSymName(c, actv)), v(rt.getSymName(c, expv))});
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
                rt.errFmt(c, "actual: error.{} != error.{}\n", &.{v(actName), v(expName)});
                return false;
            }
        },
        bt.Type => {
            const act_t = c.sema.getType(act.asHeapObject().type.type);
            const exp_t = c.sema.getType(exp.asHeapObject().type.type);
            if (act_t == exp_t) {
                return true;
            } else {
                const act_name = c.sema.allocTypeName(act_t) catch @panic("");
                defer c.alloc.free(act_name);
                const exp_name = c.sema.allocTypeName(exp_t) catch @panic("");
                defer c.alloc.free(exp_name);
                rt.errFmt(c, "actual: {} != {}\n", &.{v(act_name), v(exp_name)});
                return false;
            }
        },
        bt.Table => {
            const actv = act.asBoxAnyPtr();
            const expv = exp.asBoxAnyPtr();
            if (actv == expv) {
                return true;
            } else {
                rt.errFmt(c, "actual: {} != {}\n", &.{v(actv), v(expv)});
                return false;
            }
        },
        else => {
            if (type_id < cy.types.BuiltinEnd) {
                cy.panicFmt("Unsupported type {}", .{type_id});
            } else {
                const type_ = c.getType(type_id);
                switch (type_.kind()) {
                    .int => {
                        const act_v = act.asInt();
                        const exp_v = exp.asInt();
                        if (act_v == exp_v) {
                            return true;
                        } else {
                            rt.errFmt(c, "actual: {} != {}\n", &.{v(act_v), v(exp_v)});
                            return false;
                        }
                    },
                    .func_ptr => {
                        const act_v = act.asPtr(*cy.heap.FuncPtr);
                        const exp_v = exp.asPtr(*cy.heap.FuncPtr);
                        if (act_v.kind != exp_v.kind) {
                            return false;
                        }
                        if (act_v.sig != exp_v.sig) {
                            return false;
                        }
                        return true;
                    },
                    else => {
                        const actv = act.asBoxAnyPtr();
                        const expv = exp.asBoxAnyPtr();
                        if (actv == expv) {
                            return true;
                        } else {
                            rt.errFmt(c, "actual: {} != {}\n", &.{v(actv), v(expv)});
                            return false;
                        }
                    }
                }
            }
        }
    }
}

pub fn eqNear(vm: *cy.VM) anyerror!Value {
    const type_id: cy.TypeId = @intCast(vm.getInt(0));
    const act = vm.getValue(1);
    const exp = vm.getValue(2);

    if (type_id == bt.Float) {
        if (std.math.approxEqAbs(f64, act.asF64(), exp.asF64(), 1e-5)) {
            return Value.True;
        } else {
            rt.errFmt(vm, "actual: {} != {}\n", &.{v(act.asF64()), v(exp.asF64())});
            return vm.prepPanic("AssertError");
        }
    } else {
        rt.errFmt(vm, "Expected float, actual: {}\n", &.{v(type_id)});
        return vm.prepPanic("AssertError");
    }
}