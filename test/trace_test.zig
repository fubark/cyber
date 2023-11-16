const std = @import("std");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;

const cy = @import("../src/cyber.zig");
const vmc = @import("../src/vm_c.zig");
const log = cy.log.scoped(.trace_test);

const setup = @import("setup.zig");
const evalPass = setup.evalPass;
const eval = setup.eval;
const Runner = setup.VMrunner;
const EvalResult = setup.EvalResult;

test {
    // Include exports for C vm.
    std.testing.refAllDecls(cy.vm);
}

test "ARC." {
    // Assigning to another variable increases the ref count.
    try eval(.{},
        \\var a = [1, 2]
        \\var b = a
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);

    // Non-initializer expr in if expr true branch is retained.
    try eval(.{},
        \\var a = [ 123 ]
        \\var b = true ? a else 234
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);

    // Code is still generated for unused expr stmt.
    try eval(.{},
        \\[1, 2]
        \\return
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 1);
        try t.eq(trace.numReleases, 1);
    }}.func);

    // List literal is assigned to a local. Increase ref count.
    try eval(.{},
        \\var a = [1, 2]
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 1);
        try t.eq(trace.numReleases, 1);
    }}.func);

    // Object is retained when assigned to struct literal.
    try eval(.{},
        \\import t 'test'
        \\type S object:
        \\  var value
        \\var a = [123]
        \\var s = [S value: a]
        \\t.eq(s.value[0], 123)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 4);
        try t.eq(trace.numReleases, 4);
    }}.func);

    // Object is released when returned rvalue field access.
    try eval(.{},
        \\type S object:
        \\  var value
        \\1 + [S value: 123].value
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        const val = try res;
        var trace = run.getTrace();
        try t.eq(val.asInteger(), 124);
        try t.eq(trace.numRetains, 1);
        try t.eq(trace.numReleases, 1);
    }}.func);

    // Map entry access expression retains the entry.
    try eval(.{},
        \\var a = [ foo: 'abc\(123)' ]
        \\var b = a.foo
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 4);
        try t.eq(trace.numReleases, 4);
    }}.func);

    // Non-initializer expr in if expr false branch is retained.
    try eval(.{},
        \\var a = [ 123 ]
        \\var b = false ? 234 else a
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);
}

test "ARC for static variable declarations." {
    // Static variable is freed on vm end.
    try eval(.{},
        \\import t 'test'
        \\var Root.a = [123]
        \\t.eq(a[0], 123)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        run.vm.internal().deinitRtObjects();
        var trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 3);
        try t.eq(trace.numRetains, 2);
    }}.func);
}

test "ARC assignments." {
    // Set index on rc-candidate child to primitive.
    try eval(.{},
        \\import t 'test'
        \\var a = [123]
        \\var b = 234
        \\a[0] = b
        \\t.eq(a[0], 234)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 3);
        try t.eq(trace.numReleaseAttempts, 4);
        try t.eq(trace.numRetains, 1);
        try t.eq(trace.numReleases, 1);
    }}.func);

    // Set index on rc-candidate child to rc-candidate.
    try eval(.{},
        \\import t 'test'
        \\var a = [123]
        \\var b = [:]
        \\a[0] = b
        \\t.eq(typesym(a[0]), .map)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 4);
        try t.eq(trace.numReleaseAttempts, 5);
        try t.eq(trace.numRetains, 4);
        try t.eq(trace.numReleases, 4);
    }}.func);
}

test "ARC for passing call args." {
    // Temp list is retained when passed into function.
    try eval(.{},
        \\import t 'test'
        \\func foo(list):
        \\  return list[0]
        \\t.eq(foo([1]), 1)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 1);
        try t.eq(trace.numReleases, 1);
    }}.func);
}

test "ARC for function return values." {
    // Local object is retained when returned.
    try eval(.{},
        \\import t 'test'
        \\type S object:
        \\  var value
        \\func foo():
        \\  var a = [S value: 123]
        \\  return a
        \\my s = foo()
        \\t.eq(s.value, 123)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);

    // Object is released when returned from a function if no followup assignment.
    try eval(.{},
        \\type S object:
        \\  var value
        \\func foo():
        \\  return [S value: 123]
        \\foo()
        \\return
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 1);
        try t.eq(trace.numReleases, 1);
    }}.func);
}

test "ARC on temp locals in expressions." {
    // Only the map literal is retained and released at the end of the arc expression.
    try evalPass(.{},
        \\import test
        \\var ret = traceRetains()
        \\var rel = traceReleases()
        \\var res = [ a: [123] ].a[0]
        \\test.eq(traceRetains() - ret, 4)
        \\test.eq(traceReleases() - rel, 4)
        \\test.eq(res, 123)
    );

    // The string template literal is released at the end of the arc expression.
    try eval(.{},
        \\var foo = 'World'
        \\'Hello $(foo) $(123)'
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        const val = try res;
        try run.valueIsString(val, "Hello World 123");
        run.vm.release(val);
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);
}

test "ARC in loops." {
    // A non-rcCandidate var is reassigned to a rcCandidate var inside a loop.
    try eval(.{},
        \\my a = 123
        \\for 0..3:
        \\  a = 'abc{123}'   -- copyReleaseDst
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 3);
        try t.eq(trace.numReleases, 3);
    }}.func);

    // A non-rcCandidate var is reassigned to a rcCandidate var inside a loop and if branch.
    try eval(.{},
        \\my a = 123
        \\for 0..3:
        \\  if true:
        \\    a = 'abc{123}'    -- copyReleaseDst
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 3);
        try t.eq(trace.numReleases, 3);
    }}.func);

    // A non-rcCandidate var is reassigned to a rcCandidate var (field access on the right) inside a loop.
    try eval(.{},
        \\type S object:
        \\  var foo
        \\my a = 123
        \\for 0..3:
        \\  a = [S foo: 123].foo
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 6);
        try t.eq(trace.numRetains, 3);
        try t.eq(trace.numReleaseAttempts, 10);
        try t.eq(trace.numReleases, 3);
    }}.func);

    // An rc var first used inside a loop.
    try eval(.{},
        \\for 0..3:
        \\  var a = 'abc{123}'
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 3);
        // The inner set inst should be a releaseSet.
        try t.eq(trace.numReleases, 3);
    }}.func);

    // For iter initializes the temp value as the `any` type if the iterator has an `any` type,
    // so using it as a call arg will attempt to retain it.
    try eval(.{},
        \\func foo(it):
        \\  pass
        \\var list = [123, 234] -- +1a +1 
        \\for list -> it:       -- +4a +4 (iterator is retained once, list is retained for iterator, and 2 retains for next() returning the child item.)
        \\  foo(it)             -- +0a +0
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 5);
        try t.eq(trace.numRetains, 3);
    }}.func);

    // For iter with `any` temp value, the last temp value is released at the end of the block.
    try eval(.{},
        \\var list = [[a: 123], [a: 234]] -- +3a +3
        \\for list -> it:                 -- +7a +7 -2
        \\  pass                      
        \\                                --        -8
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res;
        var trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 9);
        try t.eq(trace.numRetains, 9);
        try t.eq(trace.numReleases, 9);
    }}.func);
}

var testVm: cy.VM = undefined;

const VMrunner = struct {
    vm: *cy.UserVM,

    fn init(self: *VMrunner) !void {
        self.* = .{
            .vm = @ptrCast(&testVm),
        };
        self.vm.init(t.alloc) catch cy.fatal();
        cy.cli.setupVMForCLI(self.vm);
    }

    fn deinit(self: *VMrunner) void {
        self.vm.deinit();
    }

    fn deinitValue(self: *VMrunner, val: cy.Value) void {
        self.vm.release(val);
    }

    fn compile(self: *VMrunner, src: []const u8) !cy.ByteCodeBuffer {
        return self.vm.compile(src);
    }

    fn getStackTrace(self: *VMrunner) *const cy.StackTrace {
        return self.vm.getStackTrace();
    }

    fn getTrace(self: *VMrunner) *vmc.TraceInfo {
        return self.vm.internal().trace;
    }

    fn eval(self: *VMrunner, src: []const u8) !cy.Value {
        const rc = self.vm.getGlobalRC();
        if (rc != 0) {
            log.debug("{} unreleased objects from previous eval", .{rc});
            return error.UnreleasedObjects;
        }
        // Eval with new env.
        self.deinit();
        try self.init();
        return self.vm.eval("main", src, .{ .singleRun = false }) catch |err| {
            if (err == error.Panic) {
                try self.vm.printLastUserPanicError();
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
        if (act.isFloat()) {
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
        const obj = cy.ptrAlignCast(*cy.HeapObject, val.asPointer());
        const list = cy.ptrAlignCast(*std.ArrayListUnmanaged(cy.Value), &obj.list.list);
        const dupe = try t.alloc.alloc(i32, list.items.len);
        for (list.items, 0..) |it, i| {
            dupe[i] = @intFromFloat(it.toF64());
        }
        return dupe;
    }

    fn parse(self: *VMrunner, src: []const u8) !cy.ParseResultView {
        _ = self;
        _ = src;
        return undefined;
    }
};