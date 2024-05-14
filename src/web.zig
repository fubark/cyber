const std = @import("std");
const c = @import("capi.zig");
const cy = @import("cyber.zig");

comptime {
    // Include lib exports.
    const lib = @import("lib.zig");
    inline for (std.meta.declarations(lib)) |decl| {
        _ = &@field(lib, decl.name);
    }
}

export fn clSetupForWeb(vm: *cy.VM) void {
    c.setModuleLoader(@ptrCast(vm), loader);
    c.setPrinter(@ptrCast(vm), print);
}

pub fn loader(vm: ?*c.VM, spec_: c.Str, out_: [*c]c.ModuleLoaderResult) callconv(.C) bool {
    const out: *c.ModuleLoaderResult = out_;
    const spec = c.fromSlice(spec_);
    if (std.mem.eql(u8, "web", spec)) {
        const src = (
            \\--| Evals JS from the host environment.
            \\@host func eval(val any) any
        );
        out.* = std.mem.zeroInit(c.ModuleLoaderResult, .{
            .src = src,
            .srcLen = src.len,
            .funcLoader = funcLoader,
        });
        return true;
    } else {
        return cy.compiler.defaultModuleLoader(vm, spec_, out);
    }
}

pub fn funcLoader(_: ?*c.VM, func: c.FuncInfo, out_: [*c]c.FuncResult) callconv(.C) bool {
    const out: *c.FuncResult = out_;
    const name = c.fromSlice(func.name);
    if (std.mem.eql(u8, "eval", name)) {
        out.ptr = @ptrCast(&eval);
        return true;
    }
    return false;
}

extern fn hostPrint(str: [*]const u8, strLen: usize) void;
fn print(_: ?*c.VM, str: c.Str) callconv(.C) void {
    hostPrint(str.ptr, str.len);
}

extern fn hostEvalJS(vm: *cy.VM, ptr: [*]const u8, len: usize) cy.Value;
pub fn eval(vm: *cy.VM, args: [*]const cy.Value, _: u8) cy.Value {
    const str = vm.getOrBufPrintValueStr(&cy.tempBuf, args[0]) catch cy.fatal();
    return hostEvalJS(vm, str.ptr, str.len);
}