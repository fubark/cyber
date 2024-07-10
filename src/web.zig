const std = @import("std");
const C = @import("capi.zig");
const cy = @import("cyber.zig");
const zErrFunc = cy.builtins.zErrFunc;
const log = cy.log.scoped(.web);

comptime {
    // Include lib exports.
    const lib = @import("lib.zig");
    for (std.meta.declarations(lib)) |decl| {
        _ = &@field(lib, decl.name);
    }
}

export fn clInitWeb(vm: *cy.VM) void {
    C.setModuleLoader(@ptrCast(vm), loader);
    C.setPrinter(@ptrCast(vm), print);
    C.setErrorPrinter(@ptrCast(vm), errPrint);
    C.setLog(logger);
    const ctx = vm.alloc.create(WebContext) catch @panic("error");
    ctx.* = .{
        .next_id = 1,
        .futures = .{},
    };
    vm.data.put(vm.alloc, "web", ctx) catch @panic("error");
}

const WebContext = struct {
    next_id: u32,
    futures: std.AutoHashMapUnmanaged(u32, cy.Value),
};

pub const Src = @embedFile("web.cy");

pub fn loader(vm: ?*C.VM, spec_: C.Str, res: ?*C.Module) callconv(.C) bool {
    const spec = C.fromStr(spec_);
    if (std.mem.eql(u8, "web", spec)) {
        const mod = C.createModule(vm, spec_, C.toStr(Src));
        var config = C.ModuleConfig{
            .funcs = C.toSlice(C.HostFuncEntry, &funcs),
        };
        C.setModuleConfig(vm, mod, &config);
        res.?.* = mod;
        return true;
    } else {
        return cy.compiler.defaultModuleLoader(vm, spec_, res);
    }
}

const func = cy.hostFuncEntry;
pub const funcs = [_]C.HostFuncEntry{
    func("eval",       eval),
};

extern fn hostPrint(str: [*]const u8, strLen: usize) void;
fn print(_: ?*C.VM, str: C.Str) callconv(.C) void {
    hostPrint(str.ptr, str.len);
}

extern fn hostErrPrint(str: [*]const u8, strLen: usize) void;
fn errPrint(_: ?*C.VM, str: C.Str) callconv(.C) void {
    hostErrPrint(str.ptr, str.len);
}

extern fn hostLogInfo(ptr: [*]const u8, len: usize) void;
fn logger(str: C.Str) callconv(.C) void {
    hostLogInfo(str.ptr, str.len);
}

extern fn hostEvalJS(vm: *cy.VM, ptr: [*]const u8, len: usize) cy.Value;
pub fn eval(vm: *cy.VM) cy.Value {
    const str = vm.getOrBufPrintValueStr(&cy.tempBuf, vm.getValue(0)) catch cy.fatal();
    return hostEvalJS(vm, str.ptr, str.len);
}

export fn clWebHasTasks(vm: *cy.VM) bool {
    const ctx = vm.getData(*WebContext, "web");
    return ctx.futures.size > 0 or vm.ready_tasks.count > 0;
}

export fn clWebCompleteFuture(vm: *cy.VM, id: u32, val: cy.Value) void {
    const ctx = vm.getData(*WebContext, "web");
    const futurev = ctx.futures.get(id).?;
    const future = futurev.castHostObject(*cy.heap.Future);
    cy.builtins.completeFuture(vm, future, val) catch @panic("error");
    vm.release(futurev);
    _ = ctx.futures.remove(id);
}

export fn clWebGetFuture(vm: *cy.VM, id: u32) cy.Value {
    const ctx = vm.getData(*WebContext, "web");
    const futurev = ctx.futures.get(id).?;
    vm.retain(futurev);
    return futurev;
}

export fn clWebNewFuture(vm: *cy.VM) u32 {
    const ctx = vm.getData(*WebContext, "web");

    const any_t = C.newType(@ptrCast(vm), cy.types.BuiltinTypes.Any);
    defer C.release(@ptrCast(vm), any_t);
    const FutureAnyT = C.expandTemplateType(vm.sema.future_tmpl.head.toC(), &any_t, 1);
    const future = vm.allocFuture(FutureAnyT) catch @panic("error");

    const id = ctx.next_id;
    ctx.futures.put(vm.alloc, id, future) catch @panic("error");
    ctx.next_id += 1;
    return id;
}
