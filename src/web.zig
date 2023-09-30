const std = @import("std");
const cy = @import("cyber.zig");

comptime {
    // Include lib exports.
    const lib = @import("lib.zig");
    inline for (std.meta.declarations(lib)) |decl| {
        _ = &@field(lib, decl.name);
    }
}

export fn csSetupForWeb(vm: *cy.UserVM) void {
    vm.setModuleLoader(loader);
    vm.setPrint(print);
}

pub fn loader(uvm: *cy.UserVM, spec_: cy.Str, out: *cy.ModuleLoaderResult) callconv(.C) bool {
    const spec = spec_.slice();
    if (std.mem.eql(u8, "web", spec)) {
        out.* = .{
            .src = cy.Str.initSlice(
                \\@host func eval(val any) none
            ),
            .srcIsStatic = true,
            .funcLoader = funcLoader,
        };
        return true;
    } else {
        return cy.vm_compiler.defaultModuleLoader(uvm, spec_, out);
    }
}

pub fn funcLoader(_: *cy.UserVM, func: cy.HostFuncInfo, out: *cy.HostFuncResult) callconv(.C) bool {
    if (std.mem.eql(u8, "eval", func.name.slice())) {
        out.ptr = @ptrCast(&eval);
        return true;
    }
    return false;
}

extern fn hostPrint(str: [*]const u8, strLen: usize) void;
fn print(_: *cy.UserVM, str: cy.Str) callconv(.C) void {
    hostPrint(str.buf, str.len);
}

extern fn hostEvalJS(ptr: [*]const u8, len: usize) void;
pub fn eval(vm: *cy.UserVM, args: [*]const cy.Value, _: u8) linksection(cy.StdSection) cy.Value {
    const str = vm.valueToTempString(args[0]);
    hostEvalJS(str.ptr, str.len);
    return cy.Value.None;
}