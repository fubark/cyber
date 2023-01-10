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

pub fn initModule(alloc: std.mem.Allocator, spec: []const u8) !cy.Module {
    var mod = cy.Module{
        .syms = .{},
        .prefix = spec,
    };
    try mod.setNativeFunc(alloc, "eq", 2, eq);
    try mod.setNativeFunc(alloc, "eqNear", 2, eqNear);
    return mod;
}

pub fn eq(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    _ = nargs;
    const act = args[0];
    const exp = args[1];
    defer {
        vm.release(act);
        vm.release(exp);
    }

    const actType = act.getUserTag();
    const expType = exp.getUserTag();
    if (actType == expType) {
        switch (actType) {
            .number => {
                if (act.asF64() == exp.asF64()) {
                    return Value.True;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(act.asF64()), v(exp.asF64())});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            .string => {
                const actStr = vm.valueAsString(act);
                const expStr = vm.valueAsString(exp);
                if (std.mem.eql(u8, actStr, expStr)) {
                    return Value.True;
                } else {
                    printStderr("actual: '{}' != '{}'\n", &.{v(actStr), v(expStr)});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            .opaquePtr => {
                const actPtr = stdx.ptrAlignCast(*cy.OpaquePtr, act.asPointer().?).ptr;
                const expPtr = stdx.ptrAlignCast(*cy.OpaquePtr, exp.asPointer().?).ptr;
                if (actPtr == expPtr) {
                    return Value.True;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(actPtr), v(expPtr)});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            .boolean => {
                const actv = act.asBool();
                const expv = exp.asBool();
                if (actv == expv) {
                    return Value.True;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(actv), v(expv)});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            .tagLiteral => {
                const actv = act.asTagLiteralId();
                const expv = exp.asTagLiteralId();
                if (actv == expv) {
                    return Value.True;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(gvm.tagLitSyms.buf[actv].name), v(gvm.tagLitSyms.buf[expv].name)});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            .none => {
                return Value.True;
            },
            .errorVal => {
                const actv = act.asErrorTagLit();
                const expv = exp.asErrorTagLit();
                if (actv == expv) {
                    return Value.True;
                } else {
                    printStderr("actual: error({}) != error({})\n", &.{v(gvm.tagLitSyms.buf[actv].name), v(gvm.tagLitSyms.buf[expv].name)});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            .map,
            .list,
            .object => {
                const actv = act.asPointer().?;
                const expv = exp.asPointer().?;
                if (actv == expv) {
                    return Value.True;
                } else {
                    printStderr("actual: {} != {}\n", &.{v(actv), v(expv)});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            else => {
                stdx.panicFmt("Unsupported type {}", .{actType});
            }
        }
    } else {
        printStderr("Types do not match:\n", &.{});
        printStderr("actual: {} != {}\n", &.{v(actType), v(expType)});
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

fn printStderr(format: []const u8, vals: []const fmt.FmtValue) void {
    if (!cy.silentError) {
        fmt.printStderr(format, vals);
    }
}