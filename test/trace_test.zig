const std = @import("std");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;

const cy = @import("../src/cyber.zig");
const log = stdx.log.scoped(.trace_test);

test "ARC." {
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
    _ = try run.eval(
        \\import t 'test'
        \\object S:
        \\  value
        \\a = [123]
        \\s = S{ value: a }
        \\try t.eq(s.value[0], 123)
    );
    try t.eq(trace.numRetains, 3);
    try t.eq(trace.numReleases, 3);

    // Object is released when returned rvalue field access.
    val = try run.eval(
        \\object S:
        \\  value
        \\1 + S{ value: 123 }.value
    );
    try t.eq(val.asF64toI32(), 124);
    try t.eq(trace.numRetains, 1);
    try t.eq(trace.numReleases, 1);

    // Map entry access expression retains the entry.
    val = try run.eval(
        \\a = { foo: 'abc{123}' }
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
        \\a.append(b)
        \\b.append(a)
    );
    try t.eq(trace.numRetains, 6);
    try t.eq(trace.numReleases, 4);
    try t.eq(trace.numForceReleases, 0);
    try t.eq(try run.checkMemory(), false);
    try t.eq(trace.numRetainCycles, 1);
    try t.eq(trace.numRetainCycleRoots, 2);
    try t.eq(trace.numForceReleases, 2);
}

test "ARC for static variable declarations." {
    var run: VMrunner = undefined;
    run.init();
    defer run.deinit();
    const trace = &run.trace;

    // Static variable is freed on vm end.
    _ = try run.eval(
        \\import t 'test'
        \\var a = [123]
        \\try t.eq(a[0], 123)
    );
    run.deinit();
    try t.eq(trace.numRetainAttempts, 2);
    try t.eq(trace.numReleaseAttempts, 4);
    try t.eq(trace.numRetains, 1);
    try t.eq(trace.numReleases, 1);
}

test "ARC assignments." {
    var run: VMrunner = undefined;
    run.init();
    defer run.deinit();

    const trace = &run.trace;

    // Set index on rc-candidate child to primitive.
    _ = try run.eval(
        \\import t 'test'
        \\a = [123]
        \\b = 234
        \\a[0] = b
        \\try t.eq(a[0], 234)
    );
    try t.eq(trace.numRetainAttempts, 2);
    try t.eq(trace.numReleaseAttempts, 5);
    try t.eq(trace.numRetains, 1);
    try t.eq(trace.numReleases, 1);

    // Set index on rc-candidate child to rc-candidate.
    _ = try run.eval(
        \\import t 'test'
        \\a = [123]
        \\b = {}
        \\a[0] = b
        \\try t.eq(valtag(a[0]), #map)
    );
    try t.eq(trace.numRetainAttempts, 4);
    try t.eq(trace.numReleaseAttempts, 7);
    try t.eq(trace.numRetains, 4);
    try t.eq(trace.numReleases, 4);
}

test "ARC for passing call args." {
    var run: VMrunner = undefined;
    run.init();
    defer run.deinit();

    const trace = &run.trace;

    // Temp list is retained when passed into function.
    _ = try run.eval(
        \\import t 'test'
        \\func foo(list):
        \\  return list[0]
        \\try t.eq(foo([1]), 1)
    );
    try t.eq(trace.numRetains, 1);
    try t.eq(trace.numReleases, 1);
}

test "ARC for function returns values." {
    var run: VMrunner = undefined;
    run.init();
    defer run.deinit();

    const trace = &run.trace;

    // Local object is retained when returned.
    _ = try run.eval(
        \\import t 'test'
        \\object S:
        \\  value
        \\func foo():
        \\  a = S{ value: 123 }
        \\  return a
        \\s = foo()
        \\try t.eq(s.value, 123)
    );
    try t.eq(trace.numRetains, 2);
    try t.eq(trace.numReleases, 2);

    // Object is released when returned from a function if no followup assignment.
    _ = try run.eval(
        \\object S:
        \\  value
        \\func foo():
        \\  return S{ value: 123 }
        \\foo()
        \\return
    );
    try t.eq(trace.numRetains, 1);
    try t.eq(trace.numReleases, 1);
}

test "ARC on temp locals in expressions." {
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
        \\'Hello {foo} {123}'
    );
    try run.valueIsString(val, "Hello World 123");
    run.deinitValue(val);
    try t.eq(trace.numRetains, 1);
    try t.eq(trace.numRetains, 1);
}

test "ARC in loops." {
    var run: VMrunner = undefined;
    run.init();
    defer run.deinit();

    const trace = &run.trace;

    // A non-rcCandidate var is reassigned to a rcCandidate var inside a loop.
    _ = try run.eval(
        \\a = 123
        \\for 0..3:
        \\  a = 'abc{123}'   -- copyReleaseDst
    );
    try t.eq(trace.numRetains, 3);
    try t.eq(trace.numReleases, 3);

    // A non-rcCandidate var is reassigned to a rcCandidate var inside a loop and if branch.
    _ = try run.eval(
        \\a = 123
        \\for 0..3:
        \\  if true:
        \\    a = 'abc{123}'    -- copyReleaseDst
    );
    try t.eq(trace.numRetains, 3);
    try t.eq(trace.numReleases, 3);

    // A non-rcCandidate var is reassigned to a rcCandidate var (field access on the right) inside a loop.
    _ = try run.eval(
        \\object S:
        \\  foo
        \\a = 123
        \\for 0..3:
        \\  a = S{ foo: 123 }.foo
    );
    try t.eq(trace.numRetainAttempts, 6);
    try t.eq(trace.numRetains, 3);
    try t.eq(trace.numReleaseAttempts, 10);
    try t.eq(trace.numReleases, 3);

    // An rc var first used inside a loop.
    _ = try run.eval(
        \\for 0..3:
        \\  a = 'abc{123}'
    );
    try t.eq(trace.numRetains, 3);
    // The inner set inst should be a releaseSet.
    try t.eq(trace.numReleases, 3);

    // For iter initializes the temp value as the `any` type if the iterator has an `any` type,
    // so using it as a call arg will attempt to retain it.
    _ = try run.eval(
        \\func foo(it):
        \\  pass
        \\list = [123, 234] -- +1a +1 
        \\for list each it:   -- +7a +5 (iterator is retained once, list is retained four times: one for iterator and others for calls to next(), and 2 retains for next() returning the child item.)
        \\  foo(it)         -- +2a
    );
    try t.eq(trace.numRetainAttempts, 10);
    try t.eq(trace.numRetains, 6);

    // For iter with `any` temp value, the last temp value is released at the end of the block.
    _ = try run.eval(
        \\list = [{a: 123}, {a: 234}] -- +3a +3
        \\for list each it:             -- +7a +7 -2
        \\  pass                      
        \\                            --        -8
    );
    try t.eq(trace.numRetainAttempts, 10);
    try t.eq(trace.numRetains, 10);
    try t.eq(trace.numReleases, 10);
}

const VMrunner = struct {
    vm: *cy.UserVM,
    trace: cy.TraceInfo,

    fn init(self: *VMrunner) void {
        self.* = .{
            .vm = cy.getUserVM(),
            .trace = undefined,
        };
        self.trace.opCounts = &.{};
        self.vm.init(t.alloc) catch stdx.fatal();
        self.vm.setTrace(&self.trace);
    }

    fn deinit(self: *VMrunner) void {
        t.alloc.free(self.trace.opCounts);
        self.trace.opCounts = &.{};
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
        self.deinit();
        try self.vm.init(t.alloc);
        self.vm.setTrace(&self.trace);
        return self.vm.eval("main", src, .{ .singleRun = false }) catch |err| {
            if (err == error.Panic) {
                try self.vm.dumpPanicStackTrace();
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
                try t.eq(act.asF64toI32(), exp);
                return;
            }
        }
        return error.NotI32;
    }

    pub fn valueIsString(self: *VMrunner, val: cy.Value, exp: []const u8) !void {
        if (val.isString()) {
            try t.eqStr(self.vm.valueAsString(val), exp);
        } else {
            return error.NotAString;
        }
    }

    pub fn valueToIntSlice(self: *VMrunner, val: cy.Value) ![]const i32 {
        _ = self;
        const obj = stdx.ptrAlignCast(*cy.HeapObject, val.asPointer());
        const list = stdx.ptrAlignCast(*std.ArrayListUnmanaged(cy.Value), &obj.list.list);
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