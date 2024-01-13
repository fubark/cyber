const std = @import("std");
const c = @import("clib.zig");
const cy = @import("cyber.zig");

comptime {
    // Include lib exports.
    const lib = @import("lib.zig");
    inline for (std.meta.declarations(lib)) |decl| {
        _ = &@field(lib, decl.name);
    }
}

export fn csSetupForWeb(vm: *cy.VM) void {
    c.setModuleLoader(@ptrCast(vm), loader);
    c.setPrint(@ptrCast(vm), print);
}

pub fn loader(vm: ?*c.VM, spec_: c.Str, out_: [*c]c.ModuleLoaderResult) callconv(.C) bool {
    const out: *c.ModuleLoaderResult = out_;
    const spec = c.strSlice(spec_);
    if (std.mem.eql(u8, "web", spec)) {
        const src = (
            \\--| Evals JS from the host environment.
            \\#host func eval(val any) none
        );
        out.* = std.mem.zeroInit(c.ModuleLoaderResult, .{
            .src = src,
            .srcLen = src.len,
            .funcLoader = funcLoader,
        });
        return true;
    } else {
        return cy.vm_compiler.defaultModuleLoader(vm, spec_, out);
    }
}

pub fn funcLoader(_: ?*c.VM, func: c.FuncInfo, out_: [*c]c.FuncResult) callconv(.C) bool {
    const out: *c.FuncResult = out_;
    const name = c.strSlice(func.name);
    if (std.mem.eql(u8, "eval", name)) {
        out.ptr = @ptrCast(&eval);
        return true;
    }
    return false;
}

extern fn hostPrint(str: [*]const u8, strLen: usize) void;
fn print(_: ?*c.VM, str: c.Str) callconv(.C) void {
    hostPrint(str.buf, str.len);
}

extern fn hostEvalJS(ptr: [*]const u8, len: usize) void;
pub fn eval(vm: *cy.VM, args: [*]const cy.Value, _: u8) cy.Value {
    const str = vm.getOrBufPrintValueStr(&cy.tempBuf, args[0]) catch cy.fatal();
    hostEvalJS(str.ptr, str.len);
    return cy.Value.None;
}