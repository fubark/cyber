const std = @import("std");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;

const cy = @import("../src/cyber.zig");
const log = stdx.log.scoped(.trace_test);

test "Automatic reference counting." {
    var run: VMrunner = undefined;
    run.init();
    defer run.deinit();

    const trace = &run.trace;

    // List literal does not escape expression. No ref count.
    var val = try run.eval(
        \\[1, 2]
        \\return
    );
    try t.eq(trace.numRetains, 0);
    try t.eq(trace.numReleases, 0);

    // List literal is assigned to a local. Increase ref count.
    val = try run.eval(
        \\a = [1, 2]
    );
    try t.eq(trace.numRetains, 1);
    try t.eq(trace.numReleases, 1);

    // Assigning to another variable increases the ref count.
    val = try run.eval(
        \\a = [1, 2]
        \\b = a
    );
    try t.eq(trace.numRetains, 2);
    try t.eq(trace.numReleases, 2);

    // Object is retained when assigned to struct literal.
    val = try run.eval(
        \\struct S:
        \\  value
        \\func foo():
        \\  a = [123]
        \\  return S{ value: a }
        \\s = foo()
        \\s.value[0]
    );
    try t.eq(val.asI32(), 123);
    try t.eq(trace.numRetains, 3);
    try t.eq(trace.numReleases, 3);

    // Object is retained when returned from non-literal expression in return clause.
    val = try run.eval(
        \\struct S:
        \\  value
        \\func foo():
        \\  a = S{ value: 123 }
        \\  return a
        \\s = foo()
        \\s.value
    );
    try t.eq(val.asI32(), 123);
    try t.eq(trace.numRetains, 2);
    try t.eq(trace.numReleases, 2);

    // Object is released when returned from a function if no followup assignment.
    val = try run.eval(
        \\struct S:
        \\  value
        \\func foo():
        \\  return S{ value: 123 }
        \\foo()
        \\return
    );
    try t.eq(trace.numRetains, 1);
    try t.eq(trace.numReleases, 1);

    // Object is released when returned rvalue field access.
    val = try run.eval(
        \\struct S:
        \\  value
        \\1 + S{ value: 123 }.value
    );
    try t.eq(val.asI32(), 124);
    try t.eq(trace.numRetains, 1);
    try t.eq(trace.numReleases, 1);

    // Map entry access expression retains the entry.
    val = try run.eval(
        \\a = { foo: 'abc' + 123 }
        \\b = a.foo
    );
    try t.eq(trace.numRetains, 3);
    try t.eq(trace.numReleases, 3);

    // Non-initializer expr in if expr true branch is retained.
    val = try run.eval(
        \\a = [ 123 ]
        \\b = if true then a else 234
    );
    try t.eq(trace.numRetains, 2);
    try t.eq(trace.numReleases, 2);

    // Non-initializer expr in if expr false branch is retained.
    val = try run.eval(
        \\a = [ 123 ]
        \\b = if false then 234 else a
    );
    try t.eq(trace.numRetains, 2);
    try t.eq(trace.numReleases, 2);

    // vm.checkMemory is able to detect retain cycle.
    val = try run.eval(
        \\a = []
        \\b = []
        \\a.add(b)
        \\b.add(a)
    );
    try t.eq(trace.numRetains, 6);
    try t.eq(trace.numReleases, 4);
    try t.eq(trace.numForceReleases, 0);
    try t.eq(try run.checkMemory(), false);
    try t.eq(trace.numRetainCycles, 1);
    try t.eq(trace.numRetainCycleRoots, 2);
    try t.eq(trace.numForceReleases, 2);
}

test "ARC in expressions." {
    var run: VMrunner = undefined;
    run.init();
    defer run.deinit();

    const trace = &run.trace;

    // Only the map literal is retained and released at the end of the arc expression.
    var val = try run.eval(
        \\{ a: [123] }.a[0]
    );
    try run.valueIsI32(val, 123);
    try t.eq(trace.numRetains, 2);
    try t.eq(trace.numReleases, 2);

    // The string template literal is released at the end of the arc expression.
    val = try run.eval(
        \\foo = 'World'
        \\`Hello \(foo) ` + 123
    );
    try run.valueIsString(val, "Hello World 123");
    run.deinitValue(val);
    try t.eq(trace.numRetains, 2);
    try t.eq(trace.numRetains, 2);
}

test "ARC in loops." {
    var run: VMrunner = undefined;
    run.init();
    defer run.deinit();

    const trace = &run.trace;

    // A non-rc var is reassigned to a rc var inside a loop.
    _ = try run.eval(
        \\a = 123
        \\for 0..3:
        \\  a = 'abc' + 123
    );
    try t.eq(trace.numRetains, 3);
    // The inner set inst should be a releaseSet.
    try t.eq(trace.numReleases, 3);

    // An rc var first used inside a loop.
    _ = try run.eval(
        \\for 0..3:
        \\  a = 'abc' + 123
    );
    try t.eq(trace.numRetains, 3);
    // The inner set inst should be a releaseSet.
    try t.eq(trace.numReleases, 3);
}

const VMrunner = struct {
    vm: cy.UserVM,
    trace: cy.TraceInfo,

    fn init(self: *VMrunner) void {
        self.* = .{
            .vm = cy.getUserVM(),
            .trace = undefined,
        };
        self.vm.init(t.alloc) catch stdx.fatal();
        self.vm.setTrace(&self.trace);
    }

    fn deinit(self: *VMrunner) void {
        self.vm.deinit();
    }

    fn deinitValue(self: *VMrunner, val: cy.Value) void {
        self.vm.release(val);
    }

    fn checkMemory(self: *VMrunner) !bool {
        return self.vm.checkMemory();
    }

    fn compile(self: *VMrunner, src: []const u8) !cy.ByteCodeBuffer {
        return self.vm.compile(src);
    }

    fn getStackTrace(self: *VMrunner) *const cy.StackTrace {
        return self.vm.getStackTrace();
    }

    fn eval(self: *VMrunner, src: []const u8) !cy.Value {
        const rc = self.vm.getGlobalRC();
        if (rc != 0) {
            log.debug("{} unreleased objects from previous eval", .{rc});
            return error.UnreleasedObjects;
        }
        // Eval with new env.
        self.vm.deinit();
        try self.vm.init(t.alloc);
        self.vm.setTrace(&self.trace);
        return self.vm.eval(src) catch |err| {
            if (err == error.Panic) {
                self.vm.dumpPanicStackTrace();
            }
            return err;
        };
    }

    fn eval2(self: *VMrunner, src: []const u8, embed_interrupts: bool) !cy.Value {
        _ = self;
        _ = src;
        _ = embed_interrupts;
        return undefined;
    }

    pub fn valueIsI32(self: *VMrunner, act: cy.Value, exp: i32) !void {
        _ = self;
        if (act.isNumber()) {
            const actf = act.asF64();
            if (cy.Value.floatCanBeInteger(actf)) {
                try t.eq(act.asI32(), exp);
                return;
            }
        }
        return error.NotI32;
    }

    pub fn assertValueString(self: *VMrunner, val: cy.Value) ![]const u8 {
        if (val.isString()) {
            return self.vm.valueAsString(val);
        } else {
            return error.NotAString;
        }
    }

    pub fn valueToIntSlice(self: *VMrunner, val: cy.Value) ![]const i32 {
        _ = self;
        const obj = stdx.ptrCastAlign(*cy.HeapObject, val.asPointer());
        const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(cy.Value), &obj.list.list);
        const dupe = try t.alloc.alloc(i32, list.items.len);
        for (list.items) |it, i| {
            dupe[i] = @floatToInt(i32, it.toF64());
        }
        return dupe;
    }

    fn parse(self: *VMrunner, src: []const u8) !cy.ParseResultView {
        _ = self;
        _ = src;
        return undefined;
    }
};