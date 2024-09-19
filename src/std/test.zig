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
const prepareThrowSymbol = bindings.prepareThrowSymbol;
const bt = cy.types.BuiltinTypes;
const v = fmt.v;
const rt = cy.rt;
const log = cy.log.scoped(.testmod);

const Src = @embedFile("test.cy");

const zErrFunc = cy.builtins.zErrFunc;
const cFunc = cy.builtins.cFunc;

const func = cy.hostFuncEntry;
const funcs = [_]C.HostFuncEntry{
    func("assert",  zErrFunc(assert)),
    func("eq_",     eq),
    func("eqNear_", zErrFunc(eqNear)),
};

pub fn create(vm: *cy.VM, r_uri: []const u8) C.Module {
    const mod = C.createModule(@ptrCast(vm), C.toStr(r_uri), C.toStr(Src));
    var config = C.ModuleConfig{
        .funcs = C.toSlice(C.HostFuncEntry, &funcs),
        .onLoad = onLoad,
    };
    C.setModuleConfig(@ptrCast(vm), mod, &config);
    return mod;
}

fn onLoad(vm_: ?*C.VM, mod: C.Sym) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const b = bindings.ModuleBuilder.init(vm.compiler, cy.Sym.fromC(mod));
    _ = b;
}

/// Simply returns the value so the caller get's an erased `any` type.
fn erase(vm: *cy.VM) Value {
    vm.retain(vm.getValue(0));
    return vm.getValue(0);
}

pub fn eq(vm: *cy.VM) Value {
    var type_id: cy.TypeId = @intCast(vm.getInt(0));

    var act = vm.getValue(1);
    var exp = vm.getValue(2);
    if (type_id == bt.Any or type_id == bt.Dyn) {
        const act_t = act.getTypeId();
        const exp_t = exp.getTypeId();
        if (act_t != exp_t) {
            rt.errFmt(vm, "Types do not match:\n", &.{});
            rt.errFmt(vm, "actual: {} != {}\n", &.{v(rt.getTypeName(vm, act_t)), v(rt.getTypeName(vm, exp_t))});
            return cy.builtins.prepThrowZError(vm, error.AssertError, null);
        }
        if (vm.sema.isUnboxedType(act_t)) {
            act = cy.vm.unbox(vm, act, act_t);
            exp = cy.vm.unbox(vm, exp, act_t);
        }
        type_id = act_t;
    }
    const res = eq_c(vm, type_id, act, exp);
    if (res.hasError()) {
        return Value.Interrupt;
    }
    return Value.initBool(res.val);
}

pub fn eq_c(c: cy.Context, type_id: cy.TypeId, act: rt.Any, exp: rt.Any) callconv(.C) rt.ErrorUnion(bool) {
    if (eq2(c, type_id, act, exp)) {
        return rt.wrapErrorValue(bool, true);
    } else {
        const err = cy.builtins.prepThrowZError2(c, error.AssertError, null);
        return rt.wrapError(bool, err);
    }
}

/// Assumes unboxed values.
fn eq2(c: cy.Context, type_id: cy.TypeId, act: rt.Any, exp: rt.Any) bool {
    if (build_options.rt != .pm) {
        if (type_id == bt.Integer) {
            if (act.val == exp.val) {
                return true;
            } else {
                rt.errZFmt(c, "actual: {}, expected: {}\n", .{act.asInt(), exp.asInt()});
                return false;
            }
        }
    }

    // PM compares by value if `is_struct` is true. Compares reference otherwise.
    // VM checks the type id and performs unboxing.
    if (build_options.rt == .pm) {
        if (act.type.kind == rt.TypeKindStruct) {
            const size = act.type.size;
            if (size <= 8) {
                const act_s = std.mem.asBytes(&act.value)[0..size];
                const exp_s = std.mem.asBytes(&exp.value)[0..size];
                if (std.mem.eql(u8, act_s, exp_s)) {
                    return true;
                } else {
                    rt.errZFmt(c, "actual: {any} != {any}\n", .{act_s, exp_s});
                    return false;
                }
            } else {
                cy.panic("TODO");
            }
        } else {
            if (act.value == exp.value) {
                return true;
            } else {
                rt.errFmt(c, "actual: {} != {}\n", &.{v(act.value), v(exp.value)});
                return false;
            }
        }
    } else {
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
                const actv = act.asHeapObject().type;
                const expv = exp.asHeapObject().type;
                if (actv.type == expv.type) {
                    return true;
                } else {
                    const act_name = c.sema.allocTypeName(actv.type) catch @panic("");
                    defer c.alloc.free(act_name);
                    const exp_name = c.sema.allocTypeName(expv.type) catch @panic("");
                    defer c.alloc.free(exp_name);
                    rt.errFmt(c, "actual: {} != {}\n", &.{v(act_name), v(exp_name)});
                    return false;
                }
            },
            bt.Table,
            bt.Map => {
                const actv = act.asAnyOpaque();
                const expv = exp.asAnyOpaque();
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
                    switch (c.c.types[type_id].kind) {
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
                            const act_v = act.castHeapObject(*cy.heap.FuncPtr);
                            const exp_v = exp.castHeapObject(*cy.heap.FuncPtr);
                            if (act_v.kind != exp_v.kind) {
                                return false;
                            }
                            if (act_v.sig != exp_v.sig) {
                                return false;
                            }
                            return true;
                        },
                        else => {
                            const actv = act.asAnyOpaque();
                            const expv = exp.asAnyOpaque();
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
}

pub fn assert(vm: *cy.VM) anyerror!Value {
    if (vm.getBool(0)) {
        return Value.Void;
    } else {
        rt.err(vm, "Assertion failed.\n");
        return rt.prepThrowError(vm, .AssertError);
    }
}

pub fn eqNear(vm: *cy.VM) anyerror!Value {
    const act = vm.getValue(0);
    const exp = vm.getValue(1);

    const actType = act.getTypeId();
    const expType = exp.getTypeId();
    if (actType == expType) {
        if (actType == bt.Float) {
            if (std.math.approxEqAbs(f64, act.asF64(), exp.asF64(), 1e-5)) {
                return Value.True;
            } else {
                rt.errFmt(vm, "actual: {} != {}\n", &.{v(act.asF64()), v(exp.asF64())});
                return rt.prepThrowError(vm, .AssertError);
            }
        } else {
            rt.errFmt(vm, "Expected float, actual: {}\n", &.{v(actType)});
            return rt.prepThrowError(vm, .AssertError);
        }
    } else {
        rt.errFmt(vm, "Types do not match:\n", &.{});
        rt.errFmt(vm, "actual: {} != {}\n", &.{v(rt.getTypeName(vm, actType)), v(rt.getTypeName(vm, expType))});
        return rt.prepThrowError(vm, .AssertError);
    }
}