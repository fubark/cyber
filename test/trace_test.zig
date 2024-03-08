const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;

const cy = @import("../src/cyber.zig");
const bt = cy.types.BuiltinTypes;
const bindings = @import("../src/builtins/bindings.zig");
const cli = @import("../src/cli.zig");
const c = @import("../src/capi.zig");
const vmc = @import("../src/vm_c.zig");
const http = @import("../src/http.zig");
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

test {
    // Include exports for C vm.
    std.testing.refAllDecls(cy.vm);

    const lib = @import("../src/lib.zig");
    std.testing.refAllDecls(lib);
}

test "ARC." {
    // Assigning to another variable increases the ref count.
    try eval(.{},
        \\var a = [1, 2]
        \\var b = a
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);

    // Non-initializer expr in if expr true branch is retained.
    try eval(.{},
        \\var a = [ 123 ]
        \\var b = if (true) a else 234
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);

    // Code is still generated for unused expr stmt.
    try eval(.{},
        \\[1, 2]
        \\return
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 1);
        try t.eq(trace.numReleases, 1);
    }}.func);

    // List literal is assigned to a local. Increase ref count.
    try eval(.{},
        \\var a = [1, 2]
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 1);
        try t.eq(trace.numReleases, 1);
    }}.func);

    // Object is retained when assigned to struct literal.
    try eval(.{},
        \\import t 'test'
        \\type S:
        \\  value List
        \\var a = [123]
        \\var s = [S value: a]
        \\t.eq(s.value[0], 123)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 4);
        try t.eq(trace.numReleases, 4);
    }}.func);

    // Object is released when returned rvalue field access.
    try eval(.{},
        \\type S:
        \\  value any
        \\1 + [S value: 123].value
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        const val = try res.getValue();
        var trace = run.getTrace();
        try t.eq(val.asInteger(), 124);
        try t.eq(trace.numRetains, 1);
        try t.eq(trace.numReleases, 1);
    }}.func);

    // Map entry access expression retains the entry.
    try eval(.{},
        \\var a = [ foo: "abc$(123)" ]
        \\var b = a.foo
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 4);
        try t.eq(trace.numReleases, 4);
    }}.func);

    // Non-initializer expr in if expr false branch is retained.
    try eval(.{},
        \\var a = [ 123 ]
        \\var b = if (false) 234 else a
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);
}

test "ARC for static variable declarations." {
    // Static variable is freed on vm end.
    try eval(.{},
        \\import t 'test'
        \\var .a = [123]
        \\t.eq(a[0], 123)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        c.deinit(run.vm);
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
        _ = try res.getValue();
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
        _ = try res.getValue();
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
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 1);
        try t.eq(trace.numReleases, 1);
    }}.func);
}

test "ARC for function return values." {
    // Local object is retained when returned.
    try eval(.{},
        \\import t 'test'
        \\type S:
        \\  value any
        \\func foo():
        \\  var a = [S value: 123]
        \\  return a
        \\my s = foo()
        \\t.eq(s.value, 123)
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 2);
        try t.eq(trace.numReleases, 2);
    }}.func);

    // Object is released when returned from a function if no followup assignment.
    try eval(.{},
        \\type S:
        \\  value any
        \\func foo():
        \\  return [S value: 123]
        \\foo()
        \\return
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
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
        \\"Hello $(foo) $(123)"
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        const val = try res.getValue();
        try run.valueIsString(val, "Hello World 123");
        const vm = run.internal();
        vm.release(val);
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
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 3);
        try t.eq(trace.numReleases, 3);
    }}.func);

    // A non-rcCandidate var is reassigned to a rcCandidate var inside a loop and if branch.
    try eval(.{},
        \\my a = 123
        \\for 0..3:
        \\  if true:
        \\    a = "abc$(123)"    -- copyReleaseDst
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetains, 3);
        try t.eq(trace.numReleases, 3);
    }}.func);

    // A non-rcCandidate var is reassigned to a rcCandidate var (field access on the right) inside a loop.
    try eval(.{},
        \\type S:
        \\  foo any
        \\my a = 123
        \\for 0..3:
        \\  a = [S foo: 123].foo
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 6);
        try t.eq(trace.numRetains, 3);
        try t.eq(trace.numReleaseAttempts, 10);
        try t.eq(trace.numReleases, 3);
    }}.func);

    // An rc var first used inside a loop.
    try eval(.{},
        \\for 0..3:
        \\  var a = "abc$(123)"
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
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
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 10);
        try t.eq(trace.numRetains, 6);
    }}.func);

    // For iter with `any` temp value, the last temp value is released at the end of the block.
    try eval(.{},
        \\var list = [[a: 123], [a: 234]] -- +3a +3
        \\for list -> it:                 -- +7a +7 -2
        \\  pass                      
        \\                                --        -8
    , struct { fn func(run: *Runner, res: EvalResult) !void {
        _ = try res.getValue();
        var trace = run.getTrace();
        try t.eq(trace.numRetainAttempts, 14);
        try t.eq(trace.numRetains, 14);
        try t.eq(trace.numReleases, 14);
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
    var client = http.MockHttpClient.init(t.alloc);
    client.retReqError = error.UnknownHostName;
    vm.httpClient = client.iface();
    try run.eval(Config.initFileModules("./test/modules/import.cy").withSilent(),
        \\import a 'https://doesnotexist123.com/'
        \\b = a
    , struct { fn func(run_: *VMrunner, res: EvalResult) !void {
        try run_.expectErrorReport(res, c.ErrorCompile,
            \\CompileError: Can not connect to `doesnotexist123.com`.
            \\
            \\@AbsPath(test/modules/import.cy):1:11:
            \\import a 'https://doesnotexist123.com/'
            \\          ^
            \\
        );
    }}.func);

    // Import NotFound response code.
    client = http.MockHttpClient.init(t.alloc);
    client.retStatusCode = std.http.Status.not_found;
    vm.httpClient = client.iface();
    try run.eval(Config.initFileModules("./test/modules/import.cy").withSilent(),
        \\import a 'https://exists.com/missing'
        \\b = a
    , struct { fn func(run_: *VMrunner, res: EvalResult) !void {
        try run_.expectErrorReport(res, c.ErrorCompile,
            \\CompileError: Can not load `https://exists.com/missing`. Response code: not_found
            \\
            \\@AbsPath(test/modules/import.cy):1:11:
            \\import a 'https://exists.com/missing'
            \\          ^
            \\
        );

    }}.func);

    // Successful import.
    client = http.MockHttpClient.init(t.alloc);
    client.retBody =
        \\var .foo = 123
        ;
    vm.httpClient = client.iface();
    _ = try run.evalPass(Config.initFileModules("./test/modules/import.cy"),
        \\import a 'https://exists.com/a.cy'
        \\import t 'test'
        \\t.eq(a.foo, 123)
    );
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
            const sym: *cy.Sym = cy.Sym.fromC(mod);
            const g = cy.ptrAlignCast(*cy.Value, vm.userData).*;
            const chunk = sym.getMod().?.chunk;
            _ = chunk.declareHostVar(sym, "g", cy.NullId, bt.Dynamic, g) catch cy.fatal();
        }
        fn loader(vm: ?*c.VM, spec: c.Str, out_: [*c]c.ModuleLoaderResult) callconv(.C) bool {
            const out: *c.ModuleLoaderResult = out_;
            if (std.mem.eql(u8, c.fromStr(spec), "mod")) {
                out.* = std.mem.zeroInit(c.ModuleLoaderResult, .{
                    .src = "",
                    .onLoad = onLoad,
                });
                return true;
            } else {
                return cli.loader(vm, spec, out);
            }
        }
    }.loader);

    _ = try run.evalPass(.{ 
        .enableFileModules = false,
        .checkGlobalRc = false,
    },
        \\import m 'mod'
        \\m.g['a'] = 1
    );

    _ = try run.evalPass(.{ 
        .enableFileModules = false,
        .checkGlobalRc = false,
    },
        \\import m 'mod'
        \\import t 'test'
        \\t.eq(m.g['a'], 1)
    );
}

test "os constants" {
    try eval(.{},
        \\import os
        \\os.system
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res.getValue();
        try t.eqStr(try run.assertValueString(val), @tagName(builtin.os.tag));
        c.release(run.vm, val.toC());
    }}.func);

    try eval(.{},
        \\import os
        \\os.cpu
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res.getValue();
        try t.eqStr(try run.assertValueString(val), @tagName(builtin.cpu.arch));
        c.release(run.vm, val.toC());
    }}.func);

    try eval(.{},
        \\import os
        \\os.endian
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res.getValue();
        if (builtin.cpu.arch.endian() == .Little) {
            try t.eq(val.asSymbolId(), @intFromEnum(bindings.Symbol.little));
        } else {
            try t.eq(val.asSymbolId(), @intFromEnum(bindings.Symbol.big));
        }
        c.release(run.vm, val.toC());
    }}.func);
}

test "Stack trace unwinding." {
    try eval(.{ .silent = true },
        \\import test
        \\my a = test.erase(123)
        \\1 + a.foo
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, c.ErrorPanic,
            \\panic: Field not found in value.
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
            .lineStartPos = 35,
        });
    }}.func);

    // Function stack trace.
    try eval(.{ .silent = true },
        \\import test
        \\func foo():
        \\  my a = test.erase(123)
        \\  return 1 + a.foo
        \\foo()
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, c.ErrorPanic,
            \\panic: Field not found in value.
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
            .lineStartPos = 49,
        });
        try eqStackFrame(trace.frames[1], .{
            .name = "main",
            .chunkId = 1,
            .line = 4,
            .col = 0,
            .lineStartPos = 68,
        });
    }}.func);

    if (!cy.isWasm) {
    
        // panic from another module.
        try eval(.{ .silent = true, .uri = "./test/main.cy" },
            \\import a 'modules/test_mods/init_panic_error.cy'
            \\import t 'test'
            \\t.eq(a.foo, 123)
        , struct { fn func(run: *VMrunner, res: EvalResult) !void {
            try run.expectErrorReport(res, c.ErrorPanic,
                \\panic: .boom
                \\
                \\@AbsPath(test/modules/test_mods/init_panic_error.cy):1:12 $init:
                \\var .foo = panic(.boom)
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
            \\import a 'modules/test_mods/init_throw_error.cy'
            \\import t 'test'
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
        fn test1(vm: *c.VM, _: [*]const c.Value, _: u8) cy.Value {
            const count_: *usize = @ptrCast(@alignCast(c.getUserData(vm)));
            count_.* += 1;
            return cy.Value.Void;
        }
        fn test2(vm: *c.VM, _: [*]const c.Value, _: u8) cy.Value {
            const count_: *usize = @ptrCast(@alignCast(c.getUserData(vm)));
            count_.* += 2;
            return cy.Value.Void;
        }
        fn test3(vm: *c.VM, _: [*]const c.Value, _: u8) cy.Value {
            const count_: *usize = @ptrCast(@alignCast(c.getUserData(vm)));
            count_.* += 3;
            return cy.Value.Void;
        }
        fn postLoadMod2(_: ?*c.VM, mod: c.Sym) callconv(.C) void {
            // Test dangling pointer.
            const s1 = allocString("test\x00");
            defer t.alloc.free(s1);
            c.declareUntypedFunc(mod, s1.ptr, 0, @ptrCast(&test3));
        }
        fn postLoadMod1(_: ?*c.VM, mod: c.Sym) callconv(.C) void {
            // Test dangling pointer.
            const s1 = allocString("test\x00");
            const s2 = allocString("test2\x00");
            defer t.alloc.free(s1);
            defer t.alloc.free(s2);

            c.declareUntypedFunc(mod, s1.ptr, 0, @ptrCast(&test1));
            c.declareUntypedFunc(mod, s2.ptr, 0, @ptrCast(&test2));
        }
        fn loader(vm_: ?*c.VM, spec: c.Str, out_: [*c]c.ModuleLoaderResult) callconv(.C) bool {
            const out: *c.ModuleLoaderResult = out_;

            const name = c.fromStr(spec);
            if (std.mem.eql(u8, name, "builtins")) {
                const defaultLoader = c.defaultModuleLoader;
                return defaultLoader(vm_, spec, @ptrCast(out));
            }
            if (std.mem.eql(u8, name, "mod1")) {
                out.* = std.mem.zeroInit(c.ModuleLoaderResult, .{
                    .src = "",
                    .onLoad = &postLoadMod1,
                });
                return true;
            } else if (std.mem.eql(u8, name, "mod2")) {
                out.* = std.mem.zeroInit(c.ModuleLoaderResult, .{
                    .src = "",
                    .onLoad = &postLoadMod2,
                });
                return true;
            }
            return false;
        }
    };
    c.setModuleLoader(@ptrCast(run.vm), @ptrCast(&S.loader));

    const src1 = try t.alloc.dupe(u8, 
        \\import m 'mod1'
        \\import n 'mod2'
        \\m.test()
        \\m.test2()
        \\n.test()
    );
    _ = try run.evalPass(.{}, src1);

    // Test dangling pointer.
    t.alloc.free(src1);

    _ = try run.evalPass(.{},
        \\import m 'mod1'
        \\import n 'mod2'
        \\m.test()
        \\m.test2()
        \\n.test()
    );

    try t.eq(count, 12);
}

fn allocString(str: []const u8) []const u8 {
    return t.alloc.dupe(u8, str) catch @panic("");
}