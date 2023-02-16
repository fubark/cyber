const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const cy = @import("../cyber.zig");
const Value = cy.Value;
const TagLit = @import("bindings.zig").TagLit;
const vm_ = @import("../vm.zig");
const gvm = &vm_.gvm;
const fmt = @import("../fmt.zig");
const v = fmt.v;

pub fn initModule(c: *cy.VMcompiler) linksection(cy.InitSection) !cy.Module {
    var mod = cy.Module{
        .syms = .{},
        .chunkId = cy.NullId,
        .resolvedRootSymId = cy.NullId,
    };
    try mod.setNativeFunc(c, "eq", 2, eq);
    try mod.setNativeFunc(c, "eqList", 2, eqList);
    try mod.setNativeFunc(c, "eqNear", 2, eqNear);
    return mod;
}

fn getComparableTag(val: Value) cy.ValueUserTag {
    return val.getUserTag();
}

fn eq2(vm: *cy.UserVM, act: Value, exp: Value) linksection(cy.StdSection) bool {
    const actType = getComparableTag(act);
    const expType = getComparableTag(exp);
    if (actType == expType) {
        switch (actType) {
            .int => {
                if (act.asI32() == exp.asI32()) {
                    return true;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(act.asI32()), v(exp.asI32())});
                    return false;
                }
            },
            .number => {
                if (act.asF64() == exp.asF64()) {
                    return true;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(act.asF64()), v(exp.asF64())});
                    return false;
                }
            },
            .string => {
                const actStr = vm.valueAsString(act);
                const expStr = vm.valueAsString(exp);
                if (std.mem.eql(u8, actStr, expStr)) {
                    return true;
                } else {
                    printStderr("actual: '{}' != '{}'\n", &.{v(actStr), v(expStr)});
                    return false;
                }
            },
            .rawstring => {
                const actStr = act.asRawString();
                const expStr = exp.asRawString();
                if (std.mem.eql(u8, actStr, expStr)) {
                    return true;
                } else {
                    printStderr("actual: '{}' != '{}'\n", &.{v(actStr), v(expStr)});
                    return false;
                }
            },
            .opaquePtr => {
                const actPtr = act.asPointer(*cy.OpaquePtr).ptr;
                const expPtr = exp.asPointer(*cy.OpaquePtr).ptr;
                if (actPtr == expPtr) {
                    return true;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(actPtr), v(expPtr)});
                    return false;
                }
            },
            .boolean => {
                const actv = act.asBool();
                const expv = exp.asBool();
                if (actv == expv) {
                    return true;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(actv), v(expv)});
                    return false;
                }
            },
            .tagLiteral => {
                const actv = act.asTagLiteralId();
                const expv = exp.asTagLiteralId();
                if (actv == expv) {
                    return true;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(gvm.tagLitSyms.buf[actv].name), v(gvm.tagLitSyms.buf[expv].name)});
                    return false;
                }
            },
            .none => {
                return true;
            },
            .errorVal => {
                const actv = act.asErrorTagLit();
                const expv = exp.asErrorTagLit();
                if (actv == expv) {
                    return true;
                } else {
                    printStderr("actual: error({}) != error({})\n", &.{v(gvm.tagLitSyms.buf[actv].name), v(gvm.tagLitSyms.buf[expv].name)});
                    return false;
                }
            },
            .map,
            .list,
            .object => {
                const actv = act.asAnyOpaque();
                const expv = exp.asAnyOpaque();
                if (actv == expv) {
                    return true;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(actv), v(expv)});
                    return false;
                }
            },
            else => {
                stdx.panicFmt("Unsupported type {}", .{actType});
            }
        }
    } else {
        printStderr("Types do not match:\n", &.{});
        printStderr("actual: {} != {}\n", &.{v(actType), v(expType)});
        return false;
    }

}

pub fn eq(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    _ = nargs;
    const act = args[0];
    const exp = args[1];
    defer {
        vm.release(act);
        vm.release(exp);
    }
    if (eq2(vm, act, exp)) {
        return Value.True;
    } else {
        return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
    }
}

pub fn eqNear(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = vm;
    _ = nargs;
    const act = args[0];
    const exp = args[1];

    const actType = act.getUserTag();
    const expType = exp.getUserTag();
    if (actType == expType) {
        if (actType == .number) {
            if (std.math.approxEqAbs(f64, act.asF64(), exp.asF64(), 1e-5)) {
                return Value.True;
            } else {
                printStderr("actual: {} != {}\n", &.{v(act.asF64()), v(exp.asF64())});
                return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
            }
        } else {
            printStderr("Expected number, actual: {}\n", &.{v(actType)});
            return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
        }
    } else {
        printStderr("Types do not match:\n", &.{});
        printStderr("actual: {} != {}\n", &.{v(actType), v(expType)});
        return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
    }
}

pub fn eqList(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const act = args[0];
    const exp = args[1];
    defer {
        vm.release(act);
        vm.release(exp);
    }

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
                        printStderr("Item mismatch at idx: {}\n", &.{v(i)});
                        return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                    }
                }
                return Value.True;
            } else {
                printStderr("actual list len: {} != {}\n", &.{v(acto.list.list.len), v(expo.list.list.len)});
                return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
            }
        } else {
            printStderr("Expected list, actual: {}\n", &.{v(actType)});
            return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
        }
    } else {
        printStderr("Types do not match:\n", &.{});
        printStderr("actual: {} != {}\n", &.{v(actType), v(expType)});
        return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
    }
}

fn printStderr(format: []const u8, vals: []const fmt.FmtValue) void {
    if (!cy.silentError) {
        fmt.printStderr(format, vals);
    }
}