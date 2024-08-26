const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;

const all = @import("all");
const cy = all.cy;
const bt = cy.types.BuiltinTypes;
const cli = all.cli;
const c = all.cy.C;
const bindings = cy.bindings;
const http = cy.http;
const log = cy.log.scoped(.trace_test);

const setup = @import("setup.zig");
const Config = setup.Config;
const VMrunner = setup.VMrunner;
const evalPass = setup.evalPass;
const eval = setup.eval;
const Runner = setup.VMrunner;
const EvalResult = setup.EvalResult;
const eqUserError = setup.eqUserError;

// Tracing tests and tests that depend on Zig source code.

test "ARC." {
    // Assigning to another variable increases the ref count.
    try eval(.{},
        \\var a = {1, 2}
        \\var b = a
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 4);
        try t.eq(trace.numReleases, 4);
    }}.func);

    // Local in if expr true branch is retained.
    try eval(.{},
        \\var a = 'abc'
        \\var b = if (true) a else 'xyz'
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);

    // Code is still generated for unused expr stmt.
    try eval(.{},
        \\{1, 2}
        \\return
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 3);
        try t.eq(trace.numReleases, 3);
    }}.func);

    // List literal is assigned to a local. Increase ref count.
    try eval(.{},
        \\var a = {1, 2}
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 3);
        try t.eq(trace.numReleases, 3);
    }}.func);

    // Object is retained when assigned to struct literal.
    try eval(.{},
        \\use t 'test'
        \\type S:
        \\  value List[dyn]
        \\var a = {123}
        \\var s = S{value=a}
        \\t.eq(s.value[0], 123)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 8);
        try t.eq(trace.numReleases, 8);
    }}.func);

    // Object is released when returned rvalue field access.
    try eval(.{},
        \\type S:
        \\  value dyn
        \\1 + S{value=123}.value
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        const val = try res.getValue();
        const trace = run.getTrace();
        try t.eq(val.asBoxInt(), 124);
        const vm = run.internal();
        vm.release(val);
        try t.eq(trace.numRetains, 4);
        try t.eq(trace.numReleases, 4);
    }}.func);

    // Map entry access expression retains the entry.
    try eval(.{},
        \\var a = Map{foo="abc$(123)"}
        \\var b = a['foo']
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 9);
        try t.eq(trace.numReleases, 9);
    }}.func);

    // Local in if expr false branch is retained.
    try eval(.{},
        \\var a = 'abc'
        \\var b = if (false) 'xyz' else a
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);
}

test "ARC for static variable declarations." {
    // Static variable is freed on vm end.
    try eval(.{},
        \\use t 'test'
        \\var .a = {123}
        \\t.eq(a[0], 123)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        c.deinit(run.vm);
        const trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 8);
        try t.eq(trace.numRetains, 7);
    }}.func);
}

test "ARC assignments." {
    // Set index on rc-candidate child to primitive.
    try eval(.{},
        \\use t 'test'
        \\var a = {123}
        \\var b = 234
        \\a[0] = b
        \\t.eq(a[0], 234)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 8);
        try t.eq(trace.numReleaseAttempts, 8);
        try t.eq(trace.numRetains, 7);
        try t.eq(trace.numReleases, 7);
    }}.func);

    // Set index on rc-candidate child to rc-candidate.
    try eval(.{},
        \\use t 'test'
        \\var a = {123}
        \\var b = Map{}
        \\a[0] = b
        \\t.eq(typeof(a[0]), Map)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 9);
        try t.eq(trace.numReleaseAttempts, 9);
        try t.eq(trace.numRetains, 8);
        try t.eq(trace.numReleases, 8);
    }}.func);
}

test "ARC for passing call args." {
    // Temp list is retained when passed into function.
    try eval(.{},
        \\use t 'test'
        \\func foo(list List[dyn]) dyn:
        \\  return list[0]
        \\t.eq(foo({1}), 1)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 6);
        try t.eq(trace.numReleases, 6);
    }}.func);
}

test "ARC for function return values." {
    // Local object is retained when returned.
    try eval(.{},
        \\use t 'test'
        \\type S:
        \\  value any
        \\func foo() S:
        \\  var a = S{value=123}
        \\  return a
        \\let s = foo()
        \\t.eq(s.value, 123)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 6);
        try t.eq(trace.numReleases, 6);
    }}.func);

    // Object is released when returned from a function if no followup assignment.
    try eval(.{},
        \\type S:
        \\  value any
        \\func foo() S:
        \\  return S{value=123}
        \\foo()
        \\return
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);
}

test "ARC on temp locals in expressions." {
    // Only the map literal is retained and released at the end of the arc expression.
    try evalPass(.{},
        \\use test
        \\var ret = traceRetains()
        \\var rel = traceReleases()
        \\var res = Map{a={123}}['a'][0]
        \\test.eq(traceRetains() - ret, 11)
        \\test.eq(traceReleases() - rel, 12)
        \\test.eq(res, 123)
    );

    // The string template literal is released at the end of the arc expression.
    try eval(.{},
        \\var foo = 'World'
        \\"Hello $(foo) $(123)"
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        const val = try res.getValue();
        try run.valueIsString(val, "Hello World 123");
        const vm = run.internal();
        vm.release(val);
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 3);
        try t.eq(trace.numReleases, 3);
    }}.func);
}

test "ARC in loops." {
    // A non-rcCandidate var is reassigned to a rcCandidate var inside a loop.
    try eval(.{},
        \\let a = 123
        \\for 0..3:
        \\  a = 'abc{123}'   -- copyReleaseDst
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 4);
        try t.eq(trace.numReleases, 4);
    }}.func);

    // A non-rcCandidate var is reassigned to a rcCandidate var inside a loop and if branch.
    try eval(.{},
        \\let a = 123
        \\for 0..3:
        \\  if true:
        \\    a = "abc$(123)"    -- copyReleaseDst
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 10);
        try t.eq(trace.numReleases, 10);
    }}.func);

    // A non-rcCandidate var is reassigned to a rcCandidate var (field access on the right) inside a loop.
    try eval(.{},
        \\type S:
        \\  foo any
        \\let a = 123
        \\for 0..3:
        \\  a = S{foo=123}.foo
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 13);
        try t.eq(trace.numRetains, 13);
        try t.eq(trace.numReleaseAttempts, 13);
        try t.eq(trace.numReleases, 13);
    }}.func);

    // An rc var first used inside a loop.
    try eval(.{},
        \\for 0..3:
        \\  var a = "abc$(123)"
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetains, 6);
        // The inner set inst should be a releaseSet.
        try t.eq(trace.numReleases, 6);
    }}.func);

    // For iter initializes the temp value as the `any` type if the iterator has an `any` type,
    // so using it as a call arg will attempt to retain it.
    try eval(.{},
        \\func foo(it int):
        \\  pass
        \\var list = {123, 234} -- +1a +1 
        \\for list -> it:       -- +4a +4 (iterator is retained once, list is retained for iterator, and 2 retains for next() returning the child item.)
        \\  foo(it)             -- +0a +0
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 21);
        try t.eq(trace.numRetains, 21);
    }}.func);

    // For iter with `any` temp value, the last temp value is released at the end of the block.
    try eval(.{},
        \\var list = {Map{a=123}, Map{a=234}} -- +3a +3
        \\for list -> it:                 -- +7a +7 -2
        \\  pass                      
        \\                                --        -8
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        const trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 31);
        try t.eq(trace.numRetains, 31);
        try t.eq(trace.numReleases, 31);
    }}.func);
}

test "Debug labels." {
    try eval(.{},
        \\var a = 1
        \\#genLabel('MyLabel')
        \\a = 1
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        _ = try res.getValue();
        const vm = run.internal();
        for (vm.compiler.buf.debugMarkers.items) |marker| {
            if (marker.etype() == .label) {
                try t.eqStr(marker.getLabelName(), "MyLabel");
                return;
            }
        }
        try t.fail();
    }}.func);
}

test "Import http spec." {
    if (cy.isWasm) {
        return;
    }

    var run = VMrunner.init();
    defer run.deinit();

    const vm = run.internal();

    const basePath = try std.fs.realpathAlloc(t.alloc, ".");
    defer t.alloc.free(basePath);

    // Import error.UnknownHostName.
    var client: *http.MockHttpClient = @ptrCast(@alignCast(c.setNewMockHttp(@ptrCast(vm))));
    client.retReqError = .UnknownHostName;
    try run.eval(Config.initFileModules("./test/modules/import.cy").withSilent().withReload(),
        \\use a 'https://doesnotexist123.com/'
        \\b = a
    , struct { fn func(run_: *VMrunner, res: EvalResult) !void {
        try run_.expectErrorReport(res, c.ErrorCompile,
            \\CompileError: Can not connect to `doesnotexist123.com`.
            \\
            \\@AbsPath(test/modules/import.cy):1:8:
            \\use a 'https://doesnotexist123.com/'
            \\       ^
            \\
        );
    }}.func);
    vm.alloc.destroy(client);

    // Import NotFound response code.
    client = @ptrCast(@alignCast(c.setNewMockHttp(@ptrCast(vm))));
    client.retStatusCode = std.http.Status.not_found;
    try run.eval(Config.initFileModules("./test/modules/import.cy").withSilent().withReload(),
        \\use a 'https://exists.com/missing'
        \\b = a
    , struct { fn func(run_: *VMrunner, res: EvalResult) !void {
        try run_.expectErrorReport(res, c.ErrorCompile,
            \\CompileError: Can not load `https://exists.com/missing`. Response code: not_found
            \\
            \\@AbsPath(test/modules/import.cy):1:8:
            \\use a 'https://exists.com/missing'
            \\       ^
            \\
        );

    }}.func);
    vm.alloc.destroy(client);

    // Successful import.
    client = @ptrCast(@alignCast(c.setNewMockHttp(@ptrCast(vm))));
    client.retBody =
        \\var .foo = 123
        ;
    _ = try run.evalPass(Config.initFileModules("./test/modules/import.cy").withReload(),
        \\use a 'https://exists.com/a.cy'
        \\use t 'test'
        \\t.eq(a.foo, 123)
    );
    vm.alloc.destroy(client);
}

test "Multiple evals persisting state." {
    var run = VMrunner.init();
    defer run.deinit();

    var global = c.newEmptyMap(run.vm);
    defer c.release(run.vm, global);
    c.setUserData(@ptrCast(run.vm), &global);

    c.setResolver(@ptrCast(run.vm), cy.compiler.defaultModuleResolver);
    c.setModuleLoader(@ptrCast(run.vm), struct {
        fn onLoad(vm_: ?*c.VM, mod: c.Sym) callconv(.C) void {
            const vm: *cy.VM = @ptrCast(@alignCast(vm_));
            const g = cy.ptrAlignCast(*cy.Value, vm.userData).*;
            c.declareVar(mod, "g", bt.Dyn, @bitCast(g));
        }
        fn loader(vm: ?*c.VM, spec: c.Str, res: ?*c.Module) callconv(.C) bool {
            if (std.mem.eql(u8, c.fromStr(spec), "mod")) {
                const mod = c.createModule(vm, spec, c.toStr(""));
                var config = c.ModuleConfig{
                    .onLoad = onLoad,
                };
                c.setModuleConfig(vm, mod, &config);
                res.?.* = mod;
                return true;
            } else {
                return cli.loader(vm, spec, res);
            }
        }
    }.loader);

    _ = try run.evalPass(.{ 
        .enableFileModules = false,
        .checkGlobalRc = false,
        .check_object_count = false,
    },
        \\use m 'mod'
        \\m.g['a'] = 1
    );

    _ = try run.evalPass(.{ 
        .enableFileModules = false,
        .checkGlobalRc = false,
        .check_object_count = false,
    },
        \\use m 'mod'
        \\use t 'test'
        \\t.eq(m.g['a'], 1)
    );
}

test "os constants" {
    try eval(.{},
        \\use os
        \\os.system
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res.getValue();
        try t.eqStr(try run.assertValueString(val), @tagName(builtin.os.tag));
        c.release(run.vm, val.toC());
    }}.func);

    try eval(.{},
        \\use os
        \\os.cpu
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res.getValue();
        try t.eqStr(try run.assertValueString(val), @tagName(builtin.cpu.arch));
        c.release(run.vm, val.toC());
    }}.func);

    try eval(.{},
        \\use os
        \\os.endian
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res.getValue();
        if (builtin.cpu.arch.endian() == .little) {
            try t.eq(val.asSymbolId(), @intFromEnum(bindings.Symbol.little));
        } else {
            try t.eq(val.asSymbolId(), @intFromEnum(bindings.Symbol.big));
        }
        c.release(run.vm, val.toC());
    }}.func);
}

test "Stack trace unwinding." {
    try eval(.{ .silent = true },
        \\use test
        \\let a = test.erase(123)
        \\1 + a.foo
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, c.ErrorPanic,
            \\panic: Missing field in object.
            \\
            \\main:3:7 main:
            \\1 + a.foo
            \\      ^
            \\
        );
        const trace = run.getStackTrace();
        try t.eq(trace.frames.len, 1);
        try eqStackFrame(trace.frames[0], .{
            .name = "main",
            .chunkId = 1,
            .line = 2,
            .col = 6,
            .lineStartPos = 33,
        });
    }}.func);

    // Function stack trace.
    try eval(.{ .silent = true },
        \\use test
        \\func foo() int:
        \\  let a = test.erase(123)
        \\  return 1 + a.foo
        \\foo()
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, c.ErrorPanic,
            \\panic: Missing field in object.
            \\
            \\main:4:16 foo:
            \\  return 1 + a.foo
            \\               ^
            \\main:5:1 main:
            \\foo()
            \\^
            \\
        );
        const trace = run.getStackTrace();
        try t.eq(trace.frames.len, 2);
        try eqStackFrame(trace.frames[0], .{
            .name = "foo",
            .chunkId = 1,
            .line = 3,
            .col = 15,
            .lineStartPos = 51,
        });
        try eqStackFrame(trace.frames[1], .{
            .name = "main",
            .chunkId = 1,
            .line = 4,
            .col = 0,
            .lineStartPos = 70,
        });
    }}.func);

    if (!cy.isWasm) {
    
        // panic from another module.
        try eval(.{ .silent = true, .uri = "./test/main.cy" },
            \\use a 'modules/test_mods/init_panic_error.cy'
            \\use t 'test'
            \\t.eq(a.foo, 123)
        , struct { fn func(run: *VMrunner, res: EvalResult) !void {
            try run.expectErrorReport(res, c.ErrorPanic,
                \\panic: boom
                \\
                \\@AbsPath(test/modules/test_mods/init_panic_error.cy):1:12 $init:
                \\var .foo = panic('boom')
                \\           ^
                \\./test/main.cy: main
                \\
            );

            const trace = run.getStackTrace();
            try t.eq(trace.frames.len, 2);
            try eqStackFrame(trace.frames[0], .{
                .name = "$init",
                .chunkId = 2,
                .line = 0,
                .col = 11,
                .lineStartPos = 0,
            });
            try eqStackFrame(trace.frames[1], .{
                .name = "main",
                .chunkId = 1,
                .line = 0,
                .col = 0,
                .lineStartPos = cy.NullId,
            });
        }}.func);

        // `throw` from another module's var initializer.
        try eval(.{ .silent = true, .uri = "./test/main.cy" },
            \\use a 'modules/test_mods/init_throw_error.cy'
            \\use t 'test'
            \\t.eq(a.foo, 123)
        , struct { fn func(run: *VMrunner, res: EvalResult) !void {
            try run.expectErrorReport(res, c.ErrorPanic,
                \\panic: error.boom
                \\
                \\@AbsPath(test/modules/test_mods/init_throw_error.cy):1:12 $init:
                \\var .foo = throw error.boom
                \\           ^
                \\./test/main.cy: main
                \\
            );
            const trace = run.getStackTrace();
            try t.eq(trace.frames.len, 2);
            try eqStackFrame(trace.frames[0], .{
                .name = "$init",
                .chunkId = 2,
                .line = 0,
                .col = 11,
                .lineStartPos = 0,
            });
            try eqStackFrame(trace.frames[1], .{
                .name = "main",
                .chunkId = 1,
                .line = 0,
                .col = 0,
                .lineStartPos = cy.NullId,
            });
        }}.func);
    }
}

fn eqStackFrame(act: cy.StackFrame, exp: cy.StackFrame) !void {
    try t.eqStr(act.name, exp.name);
    try t.eq(act.chunkId, exp.chunkId);
    try t.eq(act.line, exp.line);
    try t.eq(act.col, exp.col);
    try t.eq(act.lineStartPos, exp.lineStartPos);
}

test "Custom modules." {
    var run = VMrunner.init();
    defer run.deinit();

    var count: usize = 0;
    c.setUserData(@ptrCast(run.vm), &count);

    c.setResolver(@ptrCast(run.vm), c.defaultResolver);
    const S = struct {
        fn test1(vm: *c.VM) cy.Value {
            const count_: *usize = @ptrCast(@alignCast(c.getUserData(vm)));
            count_.* += 1;
            return cy.Value.Void;
        }
        fn test2(vm: *c.VM) cy.Value {
            const count_: *usize = @ptrCast(@alignCast(c.getUserData(vm)));
            count_.* += 2;
            return cy.Value.Void;
        }
        fn test3(vm: *c.VM) cy.Value {
            const count_: *usize = @ptrCast(@alignCast(c.getUserData(vm)));
            count_.* += 3;
            return cy.Value.Void;
        }
        fn postLoadMod2(_: ?*c.VM, mod: c.Sym) callconv(.C) void {
            // Test dangling pointer.
            const s1 = allocString("test\x00");
            defer t.alloc.free(s1);
            c.declareFuncDyn(mod, s1.ptr, 0, @ptrCast(&test3));
        }
        fn postLoadMod1(_: ?*c.VM, mod: c.Sym) callconv(.C) void {
            // Test dangling pointer.
            const s1 = allocString("test\x00");
            const s2 = allocString("test2\x00");
            defer t.alloc.free(s1);
            defer t.alloc.free(s2);

            c.declareFuncDyn(mod, s1.ptr, 0, @ptrCast(&test1));
            c.declareFuncDyn(mod, s2.ptr, 0, @ptrCast(&test2));
        }
        fn loader(vm_: ?*c.VM, spec: c.Str, res: ?*c.Module) callconv(.C) bool {
            const name = c.fromStr(spec);
            if (std.mem.eql(u8, name, "core")) {
                const defaultLoader = c.defaultModuleLoader;
                return defaultLoader(vm_, spec, res);
            }
            if (std.mem.eql(u8, name, "mod1")) {
                const mod = c.createModule(vm_, spec, c.toStr(""));
                var config = c.ModuleConfig{
                    .onLoad = postLoadMod1,
                };
                c.setModuleConfig(vm_, mod, &config);
                res.?.* = mod;
                return true;
            } else if (std.mem.eql(u8, name, "mod2")) {
                const mod = c.createModule(vm_, spec, c.toStr(""));
                var config = c.ModuleConfig{
                    .onLoad = postLoadMod2,
                };
                c.setModuleConfig(vm_, mod, &config);
                res.?.* = mod;
                return true;
            }
            return false;
        }
    };
    c.setModuleLoader(@ptrCast(run.vm), @ptrCast(&S.loader));

    const src1 = try t.alloc.dupe(u8, 
        \\use m 'mod1'
        \\use n 'mod2'
        \\m.test()
        \\m.test2()
        \\n.test()
    );
    _ = try run.evalPass(.{}, src1);

    // Test dangling pointer.
    t.alloc.free(src1);

    _ = try run.evalPass(.{},
        \\use m 'mod1'
        \\use n 'mod2'
        \\m.test()
        \\m.test2()
        \\n.test()
    );

    try t.eq(count, 12);
}

test "Return from main." {
    try eval(.{},
        \\return 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res.getValueC();
        const vm = run.internal();
        try t.eq(c.asBoxInt(val), 123);
        vm.release(try res.getValue());
    }}.func);
}

fn allocString(str: []const u8) []const u8 {
    return t.alloc.dupe(u8, str) catch @panic("");
}