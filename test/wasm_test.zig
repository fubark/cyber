const std = @import("std");
const lib  = @import("../src/lib.zig");

const cy = @cImport({
    @cInclude("cyber.h");
});

export fn _start() void {
    _ = lib;
    runTest(@embedFile("object_test.cy"));
}

fn runTest(src: []const u8) void {
    const vm = cy.cyVmCreate();
    defer cy.cyVmDestroy(vm);
    _ = cy.cyVmEval(vm, src.ptr, src.len);
}