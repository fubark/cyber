const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const cy = @import("../cyber.zig");
const Value = cy.Value;
const TagLit = @import("bindings.zig").TagLit;
const vm_ = @import("../vm.zig");
const gvm = &vm_.gvm;

pub fn initModule(alloc: std.mem.Allocator, spec: []const u8) !cy.Module {
    var mod = cy.Module{
        .syms = .{},
        .prefix = spec,
    };
    try mod.setNativeFunc(alloc, "eq", 2, eq);
    try mod.setNativeFunc(alloc, "eqNear", 2, eqNear);
    return mod;
}

pub fn eq(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
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
                    println("actual: {} != {}", .{act.asF64(), exp.asF64()});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            .string => {
                if (std.mem.eql(u8, gvm.valueAsString(act), gvm.valueAsString(exp))) {
                    return Value.True;
                } else {
                    println("actual: '{s}' != '{s}'", .{gvm.valueAsString(act), gvm.valueAsString(exp)});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            .opaquePtr => {
                const actPtr = stdx.ptrAlignCast(*cy.OpaquePtr, act.asPointer().?).ptr;
                const expPtr = stdx.ptrAlignCast(*cy.OpaquePtr, exp.asPointer().?).ptr;
                if (actPtr == expPtr) {
                    return Value.True;
                } else {
                    println("actual: {*} != {*}", .{actPtr, expPtr});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            .boolean => {
                const actv = act.asBool();
                const expv = exp.asBool();
                if (actv == expv) {
                    return Value.True;
                } else {
                    println("actual: {} != {}", .{actv, expv});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            .tagLiteral => {
                const actv = act.asTagLiteralId();
                const expv = exp.asTagLiteralId();
                if (actv == expv) {
                    return Value.True;
                } else {
                    println("actual: {s} != {s}", .{gvm.tagLitSyms.buf[actv].name, gvm.tagLitSyms.buf[expv].name});
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
                    println("actual: error({s}) != error({s})", .{gvm.tagLitSyms.buf[actv].name, gvm.tagLitSyms.buf[expv].name});
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
                    println("actual: {*} != {*}", .{actv, expv});
                    return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
                }
            },
            else => {
                stdx.panicFmt("Unsupported type {}", .{actType});
            }
        }
    } else {
        println("Types do not match:", .{});
        println("actual: {} != {}", .{actType, expType});
        return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
    }
}

const testStdOutLog = stdx.log.scoped(.stdout);

fn println(comptime format: []const u8, args: anytype) void {
    if (builtin.is_test) {
        testStdOutLog.debug(format, args);
    } else {
        const stdout = std.io.getStdOut().writer();
        stdout.print(format ++ "\n", args) catch stdx.fatal();
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
                println("actual: {} != {}", .{act.asF64(), exp.asF64()});
                return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
            }
        } else {
            println("Expected number, actual: {}", .{actType});
            return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
        }
    } else {
        println("Types do not match:", .{});
        println("actual: {} != {}", .{actType, expType});
        return Value.initErrorTagLit(@enumToInt(TagLit.AssertError));
    }
}