const std = @import("std");
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");

const core = @import("../builtins/core.zig");
const zErrFunc = core.zErrFunc;

pub const Src = @embedFile("io.cy");

const funcs = [_]struct{[]const u8, C.BindFunc}{
    .{"indexOfNewLine",    zErrFunc(indexOfNewLine)},
};

pub fn bind(_: *cy.VM, mod: *C.Sym) callconv(.c) void {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }
}

fn indexOfNewLine(t: *cy.Thread) !C.Ret {
    const ret = t.ret(cy.value.Option(i64));
    const buf = t.param(cy.heap.Span);
    const ptr: [*]const u8 = @ptrFromInt(buf.ptr);
    if (cy.string.indexOfNewLine(ptr[0..buf.len])) |idx| {
        ret.* = cy.value.Option(i64).some(@intCast(idx));
    } else {
        ret.* = cy.value.Option(i64).none();
    }
    return C.RetOk;
}
