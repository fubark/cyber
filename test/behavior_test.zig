const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const fatal = cy.fatal;
const t = stdx.testing;
const zeroInit = std.mem.zeroInit;

const cy = @import("../src/cyber.zig");
const bt = cy.types.BuiltinTypes;
const vmc = cy.vmc;
const http = @import("../src/http.zig");
const bindings = @import("../src/builtins/bindings.zig");
const log = cy.log.scoped(.behavior_test);
const c = @import("../src/clib.zig");
comptime {
    const lib = @import("../src/lib.zig");
    std.testing.refAllDecls(lib);
}
const setup = @import("setup.zig");
const eval = setup.eval;
const evalPass = setup.evalPass;
const VMrunner = setup.VMrunner;
const Config = setup.Config;
const eqUserError = setup.eqUserError;
const EvalResult = setup.EvalResult;

test "Call typed function with dynamic arg." {
    // Runtime type check.
    try eval(.{ .silent = true },
        \\import t 'test'
        \\my a = foo(123)
        \\a = foo(a)
        \\func foo(a float):
        \\  return 'foo'
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Can not find compatible function for `foo(string) any`.
            \\Only `func foo(float) dynamic` exists.
            \\
            \\main:3:5 main:
            \\a = foo(a)
            \\    ^
            \\
        );
    }}.func);
}

test "Specific ARC cases." {
    try evalPass(.{}, @embedFile("arc_cases_test.cy"));
}

test "Type casting." {
    // Failed to cast to exact type at runtime.
    try eval(.{ .silent = true },
        \\var a = 123
        \\print(a as pointer)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Can not cast `int` to `pointer`.
            \\
            \\main:2:9 main:
            \\print(a as pointer)
            \\        ^
            \\
        );
    }}.func);

    // symbol cast fails.
    try eval(.{ .silent = true },
        \\123 as symbol
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Can not cast `int` to `symbol`.
            \\
            \\main:1:5 main:
            \\123 as symbol
            \\    ^
            \\
        );
    }}.func);

    // list cast fails.
    try eval(.{ .silent = true },
        \\123 as List
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Can not cast `int` to `List`.
            \\
            \\main:1:5 main:
            \\123 as List
            \\    ^
            \\
        );
    }}.func);

    // Failed to cast to abstract type at runtime.
    try eval(.{ .silent = true },
        \\var a = 123
        \\print(a as string)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Can not cast `int` to `string`.
            \\
            \\main:2:9 main:
            \\print(a as string)
            \\        ^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("typecast_test.cy"));
}

test "Typed object." {
    // Wrong param type.
    try eval(.{ .silent = true },
        \\type Foo object:
        \\  var a float
        \\func foo(a Foo):
        \\  pass
        \\foo(123)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(Foo) dynamic
            \\
            \\main:5:1:
            \\foo(123)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{},
        \\import t 'test'
        \\
        \\type Foo object:
        \\  var a int
        \\
        \\func foo(a Foo):
        \\  return a.a == 123
        \\
        \\-- Literal.
        \\t.eq(foo([Foo a: 123]), true)
        \\        
        \\-- From var.
        \\var o = [Foo a: 123]
        \\t.eq(foo(o), true)
        \\
        \\-- Cast erased type.
        \\o = t.erase([Foo a: 123])
        \\t.eq(foo(o as Foo), true)
    );
}

test "metatype" {
    try evalPass(.{}, @embedFile("metatype_test.cy"));
}

test "Typed metatype." {
    // Wrong param type.
    try eval(.{ .silent = true },
        \\func foo(a metatype):
        \\  pass
        \\foo(true)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(bool) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(metatype) dynamic
            \\
            \\main:3:1:
            \\foo(true)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{},
        \\import t 'test'
        \\
        \\func foo(a metatype):
        \\  return a == float
        \\
        \\-- Literal.
        \\t.eq(foo(float), true)
        \\        
        \\-- From var.
        \\var mt = float
        \\t.eq(foo(mt), true)
        \\
        \\-- Cast erased type.
        \\mt = t.erase(float)
        \\t.eq(foo(mt as metatype), true)
    );
}

test "Typed fiber." {
    // Wrong param type.
    try eval(.{ .silent = true },
        \\func foo(a Fiber):
        \\  pass
        \\foo(true)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(bool) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(Fiber) dynamic
            \\
            \\main:3:1:
            \\foo(true)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{},
        \\import t 'test'
        \\
        \\func foo(a Fiber):
        \\  return typesym(a) == .fiber
        \\
        \\func start():
        \\  pass
        \\
        \\-- Literal.
        \\t.eq(foo(coinit(start)), true)
        \\        
        \\-- From var.
        \\var f = coinit(start)
        \\t.eq(foo(f), true)
        \\
        \\-- Cast erased type.
        \\f = t.erase(coinit(start))
        \\t.eq(foo(f as Fiber), true)
    );
}

test "Type constraints." {
    try evalPass(.{}, @embedFile("typeconstraint_test.cy"));

    // Wrong arg type to number param.
    try eval(.{ .silent = true },
        \\func foo(a float):
        \\  pass
        \\foo(true)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(bool) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(float) dynamic
            \\
            \\main:3:1:
            \\foo(true)
            \\^
            \\
        );
    }}.func);

    // Wrong arg type to none param.
    try eval(.{ .silent = true },
        \\func foo(a none):
        \\  pass
        \\foo(123)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(none) dynamic
            \\
            \\main:3:1:
            \\foo(123)
            \\^
            \\
        );
    }}.func);
}

test "Typed pointer." {
    // Wrong param type.
    try eval(.{ .silent = true },
        \\func foo(a pointer):
        \\  pass
        \\foo(123)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(pointer) dynamic
            \\
            \\main:3:1:
            \\foo(123)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{},
        \\import t 'test'
        \\
        \\func foo(a pointer):
        \\  return a.value() == 123
        \\
        \\-- From var.
        \\var ptr = pointer(123)
        \\t.eq(foo(ptr), true)
        \\
        \\-- Cast erased type.
        \\ptr = t.erase(pointer(123))
        \\t.eq(foo(pointer(ptr)), true)
    );
}

test "Typed string." {
    // Wrong param type.
    try eval(.{ .silent = true },
        \\func foo(a string):
        \\  pass
        \\foo(123)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(string) dynamic
            \\
            \\main:3:1:
            \\foo(123)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{},
        \\import t 'test'
        \\
        \\func foo(a string):
        \\  return a == 'true'
        \\
        \\-- Literal.
        \\t.eq(foo('true'), true)
        \\
        \\-- From var.
        \\var str = 'true'
        \\t.eq(foo(str), true)
        \\
        \\-- Cast erased type.
        \\str = t.erase('true')
        \\t.eq(foo(string(str)), true)
    );
}

test "Typed bool." {
    // Wrong param type.
    try eval(.{ .silent = true },
        \\func foo(a bool):
        \\  pass
        \\foo(123)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(bool) dynamic
            \\
            \\main:3:1:
            \\foo(123)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{},
        \\import t 'test'
        \\
        \\func foo(a bool):
        \\  return a
        \\
        \\-- Literal.
        \\t.eq(foo(true), true)
        \\
        \\-- From var.
        \\var b = true
        \\t.eq(foo(b), true)
        \\
        \\-- Cast erased type.
        \\b = t.erase(true)
        \\t.eq(foo(bool(b)), true)
    );
}

test "Typed Map." {
    // Wrong param type.
    try eval(.{ .silent = true },
        \\func foo(a Map):
        \\  pass
        \\foo(123)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(Map) dynamic
            \\
            \\main:3:1:
            \\foo(123)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{},
        \\import t 'test'
        \\
        \\func foo(a Map):
        \\  return a['a'] == 123
        \\
        \\-- Literal.
        \\t.eq(foo([a: 123]), true)
        \\
        \\-- From var.
        \\var map = [a: 123]
        \\t.eq(foo(map), true)
        \\
        \\-- Cast erased type.
        \\map = t.erase([a: 123])
        \\t.eq(foo(map as Map), true)
    );
}

test "Typed List." {
    // Wrong param type.
    try eval(.{ .silent = true },
        \\func foo(a List):
        \\  pass
        \\foo(123)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(List) dynamic
            \\
            \\main:3:1:
            \\foo(123)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{},
        \\import t 'test'
        \\
        \\func foo(a List):
        \\  return a[0] == 123
        \\
        \\-- Literal.
        \\t.eq(foo([123]), true)
        \\
        \\-- From var.
        \\var list = [123]
        \\t.eq(foo(list), true)
        \\
        \\-- Cast erased type.
        \\list = t.erase([123])
        \\t.eq(foo(list as List), true)
    );
}

test "Typed symbol." {
    // Wrong param type.
    try eval(.{ .silent = true },
        \\func foo(a symbol):
        \\  pass
        \\foo(123)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(symbol) dynamic
            \\
            \\main:3:1:
            \\foo(123)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{},
        \\import t 'test'
        \\
        \\func foo(a symbol):
        \\  return a == .sometag
        \\
        \\-- Literal.
        \\t.eq(foo(.sometag), true)
        \\
        \\-- From var.
        \\var tag = .sometag
        \\t.eq(foo(tag), true)
        \\
        \\-- Cast erased type.
        \\tag = t.erase(.sometag)
        \\t.eq(foo(tag as symbol), true)
    );
}

test "Compile-time typed function calls." {
    // Error from param type mismatch.
    try eval(.{ .silent = true },
        \\func foo(a float):
        \\  return a + 3
        \\foo(none)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(none) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(float) dynamic
            \\
            \\main:3:1:
            \\foo(none)
            \\^
            \\
        );
    }}.func);

    // Error from different number of params.
    try eval(.{ .silent = true },
        \\func foo(a int):
        \\  return a + 3
        \\foo(1, 2)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int, int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo(int) dynamic
            \\
            \\main:3:1:
            \\foo(1, 2)
            \\^
            \\
        );
    }}.func);
}

test "Typed recursive function." {
    try evalPass(.{}, @embedFile("typed_recursion_test.cy"));
}

test "Custom modules." {
    const run = VMrunner.create();
    defer run.destroy();

    var count: usize = 0;
    run.vm.setUserData(&count);

    run.vm.setModuleResolver(cy.vm_compiler.defaultModuleResolver);
    const S = struct {
        fn test1(vm: *cy.UserVM, _: [*]const cy.Value, _: u8) cy.Value {
            const count_ = cy.ptrAlignCast(*usize, vm.getUserData());
            count_.* += 1;
            return cy.Value.None;
        }
        fn test2(vm: *cy.UserVM, _: [*]const cy.Value, _: u8) cy.Value {
            const count_ = cy.ptrAlignCast(*usize, vm.getUserData());
            count_.* += 2;
            return cy.Value.None;
        }
        fn test3(vm: *cy.UserVM, _: [*]const cy.Value, _: u8) cy.Value {
            const count_ = cy.ptrAlignCast(*usize, vm.getUserData());
            count_.* += 3;
            return cy.Value.None;
        }
        fn postLoadMod2(_: ?*c.VM, mod: c.ApiModule) callconv(.C) void {
            // Test dangling pointer.
            const s1 = allocString("test\x00");
            defer t.alloc.free(s1);
            c.declareUntypedFunc(mod, s1.ptr, 0, @ptrCast(&test3));
        }
        fn postLoadMod1(_: ?*c.VM, mod: c.ApiModule) callconv(.C) void {
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

            const name = c.strSlice(spec);
            if (std.mem.eql(u8, name, "builtins")) {
                const defaultLoader = cy.vm_compiler.defaultModuleLoader;
                return defaultLoader(vm_, spec, @ptrCast(out));
            }
            if (std.mem.eql(u8, name, "mod1")) {
                out.* = zeroInit(c.ModuleLoaderResult, .{
                    .src = "",
                    .onLoad = &postLoadMod1,
                });
                return true;
            } else if (std.mem.eql(u8, name, "mod2")) {
                out.* = zeroInit(c.ModuleLoaderResult, .{
                    .src = "",
                    .onLoad = &postLoadMod2,
                });
                return true;
            }
            return false;
        }
    };
    run.vm.setModuleLoader(@ptrCast(&S.loader));

    const src1 = try t.alloc.dupe(u8, 
        \\import m 'mod1'
        \\import n 'mod2'
        \\m.test()
        \\m.test2()
        \\n.test()
    );
    _ = try run.evalExtNoReset(.{}, src1);

    // Test dangling pointer.
    t.alloc.free(src1);

    _ = try run.evalExtNoReset(.{},
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

test "Multiple evals persisting state." {
    const run = VMrunner.create();
    defer run.destroy();

    var global = run.vm.allocEmptyMap() catch fatal();
    defer run.vm.release(global);
    run.vm.setUserData(&global);

    run.vm.setModuleResolver(cy.vm_compiler.defaultModuleResolver);
    run.vm.setModuleLoader(struct {
        fn onLoad(vm_: ?*c.VM, mod: c.ApiModule) callconv(.C) void {
            const vm: *cy.VM = @ptrCast(@alignCast(vm_));
            const sym: *cy.Sym = @ptrCast(@alignCast(mod.sym));
            const g = cy.ptrAlignCast(*cy.Value, vm.userData).*;
            const chunk = sym.getMod().?.chunk;
            _ = chunk.declareHostVar(sym, "g", cy.NullId, bt.Dynamic, g) catch fatal();
        }
        fn loader(vm: ?*c.VM, spec: c.Str, out_: [*c]c.ModuleLoaderResult) callconv(.C) bool {
            const out: *c.ModuleLoaderResult = out_;
            if (std.mem.eql(u8, c.strSlice(spec), "mod")) {
                out.* = zeroInit(c.ModuleLoaderResult, .{
                    .src = "",
                    .onLoad = onLoad,
                });
                return true;
            } else {
                return cy.cli.loader(vm, spec, out);
            }
        }
    }.loader);

    const src1 =
        \\import m 'mod'
        \\m.g['a'] = 1
        ;
    _ = try run.vm.eval("main", src1, .{ 
        .singleRun = false,
        .enableFileModules = false,
        .genAllDebugSyms = false,
    });

    const src2 = 
        \\import m 'mod'
        \\import t 'test'
        \\t.eq(m.g['a'], 1)
        ;
    _ = try run.vm.eval("main", src2, .{ 
        .singleRun = false,
        .enableFileModules = false,
        .genAllDebugSyms = false,
    });
}

test "Multiple evals with same VM." {
    const run = VMrunner.create();
    defer run.destroy();

    const src =
        \\import t 'test'
        \\var a = 1
        \\t.eq(a, 1)
        ;

    _ = try run.vm.eval("main", src, .{ 
        .singleRun = false,
        .enableFileModules = false,
        .genAllDebugSyms = false,
    });
    _ = try run.vm.eval("main", src, .{ 
        .singleRun = false,
        .enableFileModules = false,
        .genAllDebugSyms = false,
    });
    _ = try run.vm.eval("main", src, .{ 
        .singleRun = false,
        .enableFileModules = false,
        .genAllDebugSyms = false,
    });
}

test "Debug labels." {
    try eval(.{},
        \\var a = 1
        \\#genLabel('MyLabel')
        \\a = 1
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        _ = try res;
        const vm = run.vm.internal();
        for (vm.compiler.buf.debugMarkers.items) |marker| {
            if (marker.pc == 3) {
                try t.eqStr(marker.getLabelName(), "MyLabel");
                return;
            }
        }
        try t.fail();
    }}.func);
}

test "User parse errors." {
    const run = VMrunner.create();
    defer run.destroy();

    // Test parse error on first line.
    var val = run.evalExt(.{ .silent = true },
        \\var
        \\var Root.a = 123
        \\var Root.b = 234
    );
    try run.expectErrorReport(val, error.ParseError,
        \\ParseError: Expected local name identifier.
        \\
        \\main:1:4:
        \\var
        \\   ^
        \\
    );

    // Test parse error on middle line.
    val = run.evalExt(.{ .silent = true },
        \\var Root.a = 123
        \\var
        \\var Root.b = 234
    );
    try run.expectErrorReport(val, error.ParseError,
        \\ParseError: Expected local name identifier.
        \\
        \\main:2:4:
        \\var
        \\   ^
        \\
    );

    // Test parse error on last line.
    val = run.evalExt(.{ .silent = true },
        \\var Root.a = 123
        \\var Root.b = 234
        \\var
    );
    try run.expectErrorReport(val, error.ParseError,
        \\ParseError: Expected local name identifier.
        \\
        \\main:3:4:
        \\var
        \\   ^
        \\
    );
}

test "Type specifiers." {
    if (cy.isWasm) {
        return;
    }
    try evalPass(Config.initFileModules("./test/typespec_test.cy"), @embedFile("typespec_test.cy"));
}

test "Type alias." {
    if (cy.isWasm) {
        return;
    }
    try evalPass(Config.initFileModules("./test/typealias_test.cy"), @embedFile("typealias_test.cy"));
}

test "Import http spec." {
    if (cy.isWasm) {
        return;
    }

    const run = VMrunner.create();
    defer run.destroy();

    const basePath = try std.fs.realpathAlloc(t.alloc, ".");
    defer t.alloc.free(basePath);

    // Import error.UnknownHostName.
    try run.resetEnv();
    var client = http.MockHttpClient.init(t.alloc);
    client.retReqError = error.UnknownHostName;
    run.vm.internal().httpClient = client.iface();
    var res = run.evalExtNoReset(Config.initFileModules("./test/import_test.cy").withSilent(),
        \\import a 'https://doesnotexist123.com/'
        \\b = a
    );
    try t.expectError(res, error.CompileError);
    var err = try run.vm.allocLastUserCompileError();
    try eqUserError(t.alloc, err,
        \\CompileError: Can not connect to `doesnotexist123.com`.
        \\
        \\@AbsPath(test/import_test.cy):1:11:
        \\import a 'https://doesnotexist123.com/'
        \\          ^
        \\
    );

    // Import NotFound response code.
    try run.resetEnv();
    client = http.MockHttpClient.init(t.alloc);
    client.retStatusCode = std.http.Status.not_found;
    run.vm.internal().httpClient = client.iface();
    res = run.evalExtNoReset(Config.initFileModules("./test/import_test.cy").withSilent(),
        \\import a 'https://exists.com/missing'
        \\b = a
    );
    try t.expectError(res, error.CompileError);
    err = try run.vm.allocLastUserCompileError();
    try eqUserError(t.alloc, err,
        \\CompileError: Can not load `https://exists.com/missing`. Response code: not_found
        \\
        \\@AbsPath(test/import_test.cy):1:11:
        \\import a 'https://exists.com/missing'
        \\          ^
        \\
    );

    // Successful import.
    try run.resetEnv();
    client = http.MockHttpClient.init(t.alloc);
    client.retBody =
        \\var Root.foo = 123
        ;
    run.vm.internal().httpClient = client.iface();
    _ = try run.evalExtNoReset(Config.initFileModules("./test/import_test.cy"),
        \\import a 'https://exists.com/a.cy'
        \\import t 'test'
        \\t.eq(a.foo, 123)
    );
}

test "Imports." {
    if (cy.isWasm) {
        return;
    }

    const run = VMrunner.create();
    defer run.destroy();

    // Import missing file.
    var res = run.evalExt(Config.initFileModules("./test/import_test.cy").withSilent(),
        \\import a 'test_mods/missing.cy'
        \\var b = a
    );
    try t.expectError(res, error.CompileError);
    const errMsg = run.vm.getCompileErrorMsg();
    try t.expect(std.mem.startsWith(u8, errMsg, "Import path does not exist:"));
    try t.expect(std.mem.indexOf(u8, errMsg, "test/test_mods/missing.cy") != null);

    run.deinit();

    // TODO: Not needed for @hidden
    // // Using unexported func symbol.
    // res = run.evalExt(Config.initFileModules("./test/import_test.cy").withSilent(),
    //     \\import a 'test_mods/a.cy'
    //     \\b = a.barNoExport
    // );
    // try t.expectError(res, error.CompileError);
    // try t.eqStr(run.vm.getCompileErrorMsg(), "Symbol is not exported: `barNoExport`");

    // TODO: Not needed for @hidden
    // // Using unexported var symbol.
    // res = run.evalExt(Config.initFileModules("./test/import_test.cy").withSilent(),
    //     \\import a 'test_mods/a.cy'
    //     \\b = a.varNoExport
    // );
    // try t.expectError(res, error.CompileError);
    // try t.eqStr(run.vm.getCompileErrorMsg(), "Symbol is not exported: `varNoExport`");

    // Using missing symbol.
    try eval(Config.initFileModules("./test/import_test.cy").withSilent(),
        \\import a 'test_mods/a.cy'
        \\var b = a.missing
    , struct { fn func(runner: *VMrunner, evalRes: EvalResult) !void {
        try runner.expectErrorReport(evalRes, error.CompileError,
            \\CompileError: Can not find the symbol `missing` in `a`.
            \\`a` resolves to `@AbsPath(test/test_mods/a.cy)`.
            \\
            \\@AbsPath(test/import_test.cy):2:11:
            \\var b = a.missing
            \\          ^
            \\
        );
    }}.func);

    // Import using relative path prefix.
    try evalPass(Config.initFileModules("./test/import_test.cy"),
        \\import a './test_mods/a.cy'
        \\import t 'test'
        \\t.eq(a.varInt, 123)
    );

    // Import using implied relative path prefix.
    try evalPass(Config.initFileModules("./test/import_test.cy"),
        \\import a 'test_mods/a.cy'
        \\import t 'test'
        \\t.eq(a.varInt, 123)
    );

    // Import using unresolved relative path.
    try evalPass(Config.initFileModules("./test/import_test.cy"),
        \\import a './test_mods/../test_mods/a.cy'
        \\import t 'test'
        \\t.eq(a.varInt, 123)
    );

    // Import when running main script in the cwd.
    try std.os.chdir("./test");
    try evalPass(Config.initFileModules("./import_test.cy"),
        \\import a 'test_mods/a.cy'
        \\import t 'test'
        \\t.eq(a.varInt, 123)
    );

    // Import when running main script in a child directory.
    try std.os.chdir("./test_mods");
    try evalPass(Config.initFileModules("../import_test.cy"),
        \\import a 'test_mods/a.cy'
        \\import t 'test'
        \\t.eq(a.varInt, 123)
    );

    try std.os.chdir("../..");
    try evalPass(Config.initFileModules("./test/import_test.cy"), @embedFile("import_test.cy"));
}

test "Dump locals." {
    const run = VMrunner.create();
    defer run.destroy();

    cy.silentInternal = true;
    defer cy.silentInternal = false;
    _ = try run.eval(
        \\func foo(a):
        \\  #dumpLocals()
    );
}

test "core module" {
    try evalPass(.{}, @embedFile("core_test.cy"));
}

test "os module" {
    try eval(.{},
        \\import os
        \\os.system
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res;
        try t.eqStr(try run.assertValueString(val), @tagName(builtin.os.tag));
        run.vm.release(val);
    }}.func);

    try eval(.{},
        \\import os
        \\os.cpu
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res;
        try t.eqStr(try run.assertValueString(val), @tagName(builtin.cpu.arch));
        run.vm.release(val);
    }}.func);

    try eval(.{},
        \\import os
        \\os.endian
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res;
        if (builtin.cpu.arch.endian() == .Little) {
            try t.eq(val.asSymbolId(), @intFromEnum(bindings.Symbol.little));
        } else {
            try t.eq(val.asSymbolId(), @intFromEnum(bindings.Symbol.big));
        }
        run.vm.release(val);
    }}.func);

    if (cy.isWasm) {
        return;
    }

    try evalPass(.{}, @embedFile("os_test.cy"));
}

test "Fibers" {
    try evalPass(.{}, @embedFile("fiber_test.cy"));
}

test "throw." {
    // Uncaught in main scope.
    try eval(.{ .silent = true },
        \\throw error.boom
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: error.boom
            \\
            \\main:1:1 main:
            \\throw error.boom
            \\^
            \\
        );
    }}.func);

    // Uncaught from function call.
    try eval(.{ .silent = true },
        \\func foo():
        \\  throw error.boom
        \\foo()
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: error.boom
            \\
            \\main:2:3 foo:
            \\  throw error.boom
            \\  ^
            \\main:3:1 main:
            \\foo()
            \\^
            \\
        );
    }}.func);

    // Uncaught from nested function call.
    try eval(.{ .silent = true },
        \\func bar():
        \\  throw error.boom
        \\func foo():
        \\  bar()
        \\foo()
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: error.boom
            \\
            \\main:2:3 bar:
            \\  throw error.boom
            \\  ^
            \\main:4:3 foo:
            \\  bar()
            \\  ^
            \\main:5:1 main:
            \\foo()
            \\^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("throw_test.cy"));
}

test "try expression." {
    try evalPass(.{}, @embedFile("try_expr_test.cy"));
}

test "try catch expr." {
    try evalPass(.{}, @embedFile("try_catch_expr_test.cy"));
}

test "try catch." {
    try evalPass(.{}, @embedFile("try_catch_test.cy"));
}

test "Errors." {
    try evalPass(.{}, @embedFile("error_test.cy"));
}

test "FFI." {
    if (cy.isWasm) {
        return;
    }
    const S = struct {
        export fn testAdd(a: i32, b: i32) i32 {
            return a + b;
        }
        export fn testI8(n: i8) i8 {
            return n;
        }
        export fn testU8(n: u8) u8 {
            return n;
        }
        export fn testI16(n: i16) i16 {
            return n;
        }
        export fn testU16(n: u16) u16 {
            return n;
        }
        export fn testI32(n: i32) i32 {
            return n;
        }
        export fn testU32(n: u32) u32 {
            return n;
        }
        export fn testI64(n: i64) i64 {
            return n;
        }
        export fn testU64(n: u64) u64 {
            return n;
        }
        export fn testUSize(n: usize) usize {
            return n;
        }
        export fn testF32(n: f32) f32 {
            return n;
        }
        export fn testF64(n: f64) f64 {
            return n;
        }
        export fn testCharPtr(ptr: [*:0]u8) [*:0]const u8 {
            return ptr;
        }
        export fn testVoidPtr(ptr: *anyopaque) *anyopaque {
            return ptr;
        }
        export fn testVoid() void {
        }
        export fn testBool(b: bool) bool {
            return b;
        }
        const MyObject = extern struct {
            a: f64,
            b: i32,
            c: [*:0]u8,
            d: bool,
        };
        export fn testObject(o: MyObject) MyObject {
            return MyObject{
                .a = o.a,
                .b = o.b,
                .c = o.c,
                .d = o.d,
            };
        }
        export fn testRetObjectPtr(o: MyObject) *MyObject {
            temp = .{
                .a = o.a,
                .b = o.b,
                .c = o.c,
                .d = o.d,
            };
            return &temp;
        }
        export fn testArray(arr: [*c]f64) f64 {
            return arr[0] + arr[1];
        }

        export fn testCallback(a: i32, b: i32, add: *const fn (i32, i32) callconv(.C) i32) i32 {
            return add(a, b);
        }

        var temp: MyObject = undefined;
    };
    _ = S;

    // Wrong param type.
    try eval(.{ .silent = true },
        \\import os 'os'
        \\my libPath = none
        \\if os.system == 'macos':
        \\  -- rdynamic doesn't work atm for MacOS.
        \\  libPath = 'test/macos_lib.dylib'
        \\else os.system == 'windows':
        \\  libPath = 'test/win_lib.dll'
        \\else:
        \\  libPath = none
        \\
        \\var ffi = os.newFFI()
        \\ffi.cfunc('testAdd', [.int, .int], .int)
        \\var lib = ffi.bindLib(libPath)
        \\lib.testAdd(123, '321')
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Can not find compatible function for `testAdd(any, int, string) any` in `BindLib1`.
            \\Only `func testAdd(any, int, int) int` exists for the symbol `testAdd`.
            \\
            \\main:14:1 main:
            \\lib.testAdd(123, '321')
            \\^
            \\
        );
    }}.func);

    // Wrong num params.
    try eval(.{ .silent = true },
        \\import os 'os'
        \\my libPath = none
        \\if os.system == 'macos':
        \\  -- rdynamic doesn't work atm for MacOS.
        \\  libPath = 'test/macos_lib.dylib'
        \\else os.system == 'windows':
        \\  libPath = 'test/win_lib.dll'
        \\else:
        \\  libPath = none
        \\
        \\var ffi = os.newFFI()
        \\ffi.cfunc('testAdd', [.int, .int], .int)
        \\var lib = ffi.bindLib(libPath)
        \\lib.testAdd(123, 234, 345)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Can not find compatible function for `testAdd(any, int, int, int) any` in `BindLib1`.
            \\Only `func testAdd(any, int, int) int` exists for the symbol `testAdd`.
            \\
            \\main:14:1 main:
            \\lib.testAdd(123, 234, 345)
            \\^
            \\
        );
    }}.func);

    // TODO: Test callback failure and verify stack trace.
    // Currently, the VM aborts when encountering a callback error.
    // A config could be added to make the initial FFI call detect an error and throw a panic instead.

    try evalPass(.{}, @embedFile("ffi_test.cy"));
}

test "Symbols." {
    // Literal.
    try eval(.{},
        \\var n = .Tiger
        \\int(n)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        const val = try res;
        const id = try run.vm.internal().ensureSymbol("Tiger");
        try t.eq(val.asInteger(), @as(i48, @intCast(id)));
    }}.func);
}

test "Enum types." {
    try evalPass(.{}, @embedFile("enum_test.cy"));
}

test "test module" {
    try eval(.{ .silent = true },
        \\import t 'test'
        \\t.eq(123, 234)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: error.AssertError
            \\
            \\main:2:1 main:
            \\t.eq(123, 234)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("test_mod_test.cy"));
}

test "Objects." {
    try evalPass(.{}, @embedFile("object_test.cy"));

    // Missing semicolon.
    try eval(.{ .silent = true },
        \\type Vec2 object
        \\  var x
        \\  var y
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.ParseError,
            \\ParseError: Unexpected indentation.
            \\
            \\main:2:3:
            \\  var x
            \\  ^
            \\
        );
    }}.func);

    // Field declaration ends the file without parser error.
    try evalPass(.{}, 
        \\type Vec2 object:
        \\  var x
        \\  var y
    );

    // Initialize with undeclared field.
    try eval(.{ .silent = true },
        \\type S object:
        \\  var a
        \\var o = [S b: 100]
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Field `b` does not exist in `S`.
            \\
            \\main:3:12:
            \\var o = [S b: 100]
            \\           ^
            \\
        );
    }}.func);

    // Write to undeclared field.
    try eval(.{ .silent = true },
        \\type S object:
        \\  var a
        \\var o = [S a: 100]
        \\o.b = 200
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Field `b` does not exist in `S`.
            \\
            \\main:4:3:
            \\o.b = 200
            \\  ^
            \\
        );
    }}.func);
}

test "Object fields." {
    // Initialize field with dynamic value does not gen `objectTypeCheck`.
    try eval(.{ .silent = true },
        \\type S object:
        \\  my a
        \\func foo():
        \\  return 123
        \\var s = [S a: foo()]
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        _ = try res;

        const ops = run.vm.internal().ops;
        var pc: u32 = 0;
        while (pc < ops.len) {
            if (@as(cy.OpCode, @enumFromInt(ops[pc].val)) == .objectTypeCheck) {
                return error.Failed;
            }
            pc += cy.bytecode.getInstLenAt(ops.ptr + pc);
        }
    }}.func);

    // Initialize field with typed value does not gen `objectTypeCheck`.
    try eval(.{ .silent = true },
        \\type S object:
        \\  my a
        \\var s = [S a: 123]
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        _ = try res;

        const ops = run.vm.internal().ops;
        var pc: u32 = 0;
        while (pc < ops.len) {
            if (@as(cy.OpCode, @enumFromInt(ops[pc].val)) == .objectTypeCheck) {
                return error.Failed;
            }
            pc += cy.bytecode.getInstLenAt(ops.ptr + pc);
        }
    }}.func);
}

test "Typed object fields." {
    // Field declaration ends the file without parser error.
    try evalPass(.{},
        \\type Vec2 object:
        \\  var x float
        \\  var y float
    );

    // Initialize field with exact type match.
    try evalPass(.{},
        \\import test
        \\type S object:
        \\  var x float
        \\var s = [S x: 1.23]
        \\test.eq(s.x, 1.23)
    );

    // Inferred initializer to field type.
    try evalPass(.{},
        \\import test
        \\type S object:
        \\  var x float
        \\var s = [S x: 123]
        \\test.eq(s.x, 123.0)
    );

    // Initialize field with incompatible dynamic value.
    try eval(.{ .silent = true },
        \\type S object:
        \\  var a float
        \\func foo():
        \\  return 123
        \\var s = [S a: foo()]
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Initializing `float` field with incompatible type `int`.
            \\
            \\main:5:9 main:
            \\var s = [S a: foo()]
            \\        ^
            \\
        );
    }}.func);

    // Zero initialize missing field.
    try evalPass(.{},
        \\import test
        \\type S object:
        \\  var a float
        \\var o = [S:]
        \\test.eq(o.a, 0.0)
    );

    // Zero initialize missing field. Nested object.
    try evalPass(.{},
        \\import test
        \\type T object:
        \\  var a float
        \\type S object:
        \\  var a T
        \\var o = [S:]
        \\test.eq(o.a.a, 0.0)
    );

    // Zero initialize field with circular dep.
    try eval(.{ .silent = true },
        \\type S object:
        \\  var a S
        \\var o = [S:]
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not zero initialize `S` because of circular dependency.
            \\
            \\main:3:9:
            \\var o = [S:]
            \\        ^
            \\
        );
    }}.func);

    // No such field.
    try eval(.{ .silent = true },
        \\type S object:
        \\  var a float
        \\var o = [S b: 123]
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Field `b` does not exist in `S`.
            \\
            \\main:3:12:
            \\var o = [S b: 123]
            \\           ^
            \\
        );
    }}.func);

    // Set field with exact type.
    try evalPass(.{},
        \\type S object:
        \\  var a float
        \\var o = [S a: 123.0]
        \\o.a = 234.0
    );

    // Set field inferring rhs type.
    try evalPass(.{},
        \\type S object:
        \\  var a float
        \\var o = [S a: 123.0]
        \\o.a = 234
    );

    // Op set field inferring rhs type.
    try evalPass(.{},
        \\type S object:
        \\  var a float
        \\var o = [S a: 123.0]
        \\o.a += 234
    );

    // Set field with incompatible type. Static rhs.
    try eval(.{ .silent = true },
        \\type S object:
        \\  var a float
        \\var o = [S a: 123]
        \\o.a = []
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Expected type `float`, got `List`.
            \\
            \\main:4:7:
            \\o.a = []
            \\      ^
            \\
        );
    }}.func);

    // Set field with incompatiable type. Dynamic rhs.
    try eval(.{ .silent = true },
        \\type S object:
        \\  var a float
        \\func foo(): return []
        \\var o = [S a: 123]
        \\o.a = foo()
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Assigning to `float` field with incompatible type `List`.
            \\
            \\main:5:1 main:
            \\o.a = foo()
            \\^
            \\
        );
    }}.func);

    // Set field with incompatible type. Dynamic lhs.
    try eval(.{ .silent = true },
        \\import t 'test'
        \\type S object:
        \\  var a float
        \\my o = t.erase([S a: 123])
        \\o.a = []
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Assigning to `float` field with incompatible type `List`.
            \\
            \\main:5:1 main:
            \\o.a = []
            \\^
            \\
        );
    }}.func);
}

test "Object funcs/methods." {
    // Calling a missing method name.
    try eval(.{ .silent = true },
        \\type S object:
        \\  var a
        \\var o = [S:]
        \\o.foo()
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: `func foo(any) any` can not be found in `S`.
            \\
            \\main:4:1 main:
            \\o.foo()
            \\^
            \\
        );
    }}.func);

    // Calling a method with the wrong signature.
    try eval(.{ .silent = true },
        \\type S object:
        \\  var a
        \\  func foo():
        \\    return 123
        \\var o = [S:]
        \\o.foo(234)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Can not find compatible function for `foo(any, int) any` in `S`.
            \\Only `func foo(S) dynamic` exists for the symbol `foo`.
            \\
            \\main:6:1 main:
            \\o.foo(234)
            \\^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("object_func_test.cy"));
}

test "must()" {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\var a = 123
        \\-- no error, must returns argument.
        \\t.eq(must(a), 123)
    );

    var res = run.evalExt(.{ .silent = true },
        \\var a = error.boom
        \\must(a)
    );
    try t.expectError(res, error.Panic);
    var trace = run.getStackTrace();
    try run.assertPanicMsg("error.boom");
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .chunkId = 0,
        .line = 1,
        .col = 0,
        .lineStartPos = 19,
    });
}

test "panic()" {
    const run = VMrunner.create();
    defer run.destroy();

    var res = run.evalExt(.{ .silent = true },
        \\var a = 123
        \\1 + panic(.boom)
    );
    try t.expectError(res, error.Panic);
    var trace = run.getStackTrace();
    try run.assertPanicMsg(".boom");
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .chunkId = 0,
        .line = 1,
        .col = 4,
        .lineStartPos = 12,
    });
}

test "Error reporting." {
    // Parse error considers shebang line.
    try eval(.{ .silent = true },
        \\#!cyber
        \\import
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.ParseError,
            \\ParseError: Expected import clause.
            \\
            \\main:2:7:
            \\import
            \\      ^
            \\
        );
    }}.func);

    // Panic error considers shebang line.
    try eval(.{ .silent = true },
        \\#!cyber
        \\throw error.Boom
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: error.Boom
            \\
            \\main:2:1 main:
            \\throw error.Boom
            \\^
            \\
        );
    }}.func);

    // Windows new lines in source code.
    try eval(.{ .silent = true }, "a = 123\r\nb = 234\r\nc =",
    struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.ParseError,
            \\ParseError: Expected right expression for assignment statement.
            \\
            \\main:3:4:
            \\c =
            \\   ^
            \\
        );
    }}.func);
}

test "Stack trace unwinding." {
    const run = VMrunner.create();
    defer run.destroy();

    var res = run.evalExt(.{ .silent = true },
        \\import test
        \\my a = test.erase(123)
        \\1 + a.foo
    );
    try run.expectErrorReport(res, error.Panic,
        \\panic: Field not found in value.
        \\
        \\main:3:7 main:
        \\1 + a.foo
        \\      ^
        \\
    );
    var trace = run.getStackTrace();
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .chunkId = 0,
        .line = 2,
        .col = 6,
        .lineStartPos = 35,
    });

    // Function stack trace.
    res = run.evalExt(.{ .silent = true },
        \\import test
        \\func foo():
        \\  my a = test.erase(123)
        \\  return 1 + a.foo
        \\foo()
    );
    try run.expectErrorReport(res, error.Panic,
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
    trace = run.getStackTrace();
    try t.eq(trace.frames.len, 2);
    try eqStackFrame(trace.frames[0], .{
        .name = "foo",
        .chunkId = 0,
        .line = 3,
        .col = 15,
        .lineStartPos = 49,
    });
    try eqStackFrame(trace.frames[1], .{
        .name = "main",
        .chunkId = 0,
        .line = 4,
        .col = 0,
        .lineStartPos = 68,
    });

    if (!cy.isWasm) {
    
        // panic from another module.
        res = run.evalExt(.{ .silent = true, .uri = "./test/main.cy" },
            \\import a 'test_mods/init_panic_error.cy'
            \\import t 'test'
            \\t.eq(a.foo, 123)
        );
        try t.expectError(res, error.Panic);
        trace = run.getStackTrace();
        try t.eq(trace.frames.len, 2);
        try eqStackFrame(trace.frames[0], .{
            .name = "init",
            .chunkId = 2,
            .line = 0,
            .col = 15,
            .lineStartPos = 0,
        });
        try eqStackFrame(trace.frames[1], .{
            .name = "main",
            .chunkId = 0,
            .line = 0,
            .col = 0,
            .lineStartPos = cy.NullId,
        });

        run.deinit();

        // `throw` from another module's var initializer.
        try eval(.{ .silent = true, .uri = "./test/main.cy" },
            \\import a 'test_mods/init_throw_error.cy'
            \\import t 'test'
            \\t.eq(a.foo, 123)
        , struct { fn func(run_: *VMrunner, res_: EvalResult) !void {
            try run_.expectErrorReport(res_, error.Panic,
                \\panic: error.boom
                \\
                \\@AbsPath(test/test_mods/init_throw_error.cy):1:16 init:
                \\var Root.foo = throw error.boom
                \\               ^
                \\./test/main.cy: main
                \\
            );
            const trace_ = run_.getStackTrace();
            try t.eq(trace_.frames.len, 2);
            try eqStackFrame(trace_.frames[0], .{
                .name = "init",
                .chunkId = 2,
                .line = 0,
                .col = 15,
                .lineStartPos = 0,
            });
            try eqStackFrame(trace_.frames[1], .{
                .name = "main",
                .chunkId = 0,
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

test "Optionals." {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\var foo = none
        \\foo
    );
    try t.eq(val.isNone(), true);
}

// test "Binary expr with generator function call." {
//     const run = VMrunner.create();
//     defer run.destroy();

//     var val = try run.eval2(
//         \\func foo():
//         \\  return 1
//         \\foo() + 2
//     , true);
//     try t.eq(val.asF64toI32(), 3);
//     run.deinitValue(val);
// }

test "Compare numbers." {
    try evalPass(.{}, @embedFile("compare_num_test.cy"));
}

test "Compare equals." {
    try evalPass(.{}, @embedFile("compare_eq_test.cy"));
}

test "Compare not equals." {
    try evalPass(.{}, @embedFile("compare_neq_test.cy"));
}

test "Truthy evaluation." {
    try evalPass(.{}, @embedFile("truthy_test.cy"));
}

test "Logic operators" {
    try evalPass(.{}, @embedFile("logic_op_test.cy"));
}

test "bool" {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\true
    );
    try t.eq(val.asBool(), true);

    val = try run.eval(
        \\false
    );
    try t.eq(val.asBool(), false);
}

// test "@name" {
//     const run = VMrunner.create();
//     defer run.destroy();

//     const parse_res = try run.parse(
//         \\@name foo
//     );
//     try t.eqStr(parse_res.name, "foo");

//     if (build_options.cyEngine == .qjs) {
//         // Compile step skips the statement.
//         const compile_res = try run.compile(
//             \\@name foo
//         );
//         try t.eqStr(compile_res.output, "(function () {});");
//     }
// }

// test "implicit await" {
//     const run = VMrunner.create();
//     defer run.destroy();

//     var val = try run.eval(
//         \\func foo() apromise:
//         \\  task = @asyncTask()
//         \\  @queueTask(func () => task.resolve(123))
//         \\  return task.promise
//         \\1 + foo()
//     );
//     try t.eq(val.asF64toI32(), 124);
//     run.deinitValue(val);
// }

// test "await" {
//     const run = VMrunner.create();
//     defer run.destroy();

//     var val = try run.eval(
//         \\func foo():
//         \\  task = @asyncTask()
//         \\  @queueTask(func () => task.resolve(123))
//         \\  return task.promise
//         \\await foo()
//     );
//     try t.eq(val.asF64toI32(), 123);
//     run.deinitValue(val);

//     // await on value.
//     val = try run.eval(
//         \\func foo():
//         \\  return 234
//         \\await foo()
//     );
//     try t.eq(val.asF64toI32(), 234);
//     run.deinitValue(val);
// }

test "Expression statement." {
    // Releases returned value from call expression.
    try evalPass(.{},
        \\var a = 'abc'
        \\a.concat('xyz')
        \\pass
    );
}

test "Statements." {
    try evalPass(.{},
        \\import t 'test'
        \\
        \\-- Expressions are allowed to wrap to the next line.
        \\var a = 0
        \\if true or
        \\   true:
        \\  a = 10
        \\t.eq(a, 10)
    );

    // Invalid single line block.
    try eval(.{ .silent = true },
        \\if true: foo = 123 foo = 234
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.ParseError,
            \\ParseError: Expected end of line or file. Got ident.
            \\
            \\main:1:20:
            \\if true: foo = 123 foo = 234
            \\                   ^
            \\
        );
    }}.func);
    try eval(.{ .silent = true },
        \\if true: foo = 123: foo = 234
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.ParseError,
            \\ParseError: Expected end of line or file. Got colon.
            \\
            \\main:1:19:
            \\if true: foo = 123: foo = 234
            \\                  ^
            \\
        );
    }}.func);
    try eval(.{ .silent = true },
        \\if true: foo = 123
        \\  foo = 234
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.ParseError,
            \\ParseError: Unexpected indentation.
            \\
            \\main:2:3:
            \\  foo = 234
            \\  ^
            \\
        );
    }}.func);

    // Allows compiling without any statements.
    try evalPass(.{}, "   ");
}

test "Indentation." {
    try evalPass(.{}, @embedFile("indentation_test.cy"));

    // Mixing tabs and spaces is an error.
    try eval(.{ .silent = true },
         \\if true:
         \\	 return 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.ParseError,
            \\ParseError: Can not mix tabs and spaces for indentation.
            \\
            \\main:2:2:
            \\	 return 123
            \\ ^
            \\
        );
    }}.func);

    // Changing to tabs is not allowed.
    try eval(.{ .silent = true },
        \\if true:
        \\  if true:
        \\      -- Previously spaces, now tabs.
        \\		return 123
        \\
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.ParseError,
            \\ParseError: Expected spaces for indentation.
            \\
            \\main:4:3:
            \\		return 123
            \\  ^
            \\
        );
    }}.func);

    // Changing to spaces is not allowed.
    try eval(.{ .silent = true },
        \\if true:
        \\	if true:
        \\     -- Previously tabs, now spaces.
        \\     return 123
        \\
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.ParseError,
            \\ParseError: Expected tabs for indentation.
            \\
            \\main:4:6:
            \\     return 123
            \\     ^
            \\
        );
    }}.func);

    // Last line with just indents.
    try evalPass(.{},
        \\pass
        \\      
    );

    // New block requires at least one statement.
    // const parse_res = try run.parse(
    //     \\if true:
    //     \\return 123 
    // );
    // try t.eq(parse_res.has_error, true);
    // try t.expect(std.mem.indexOf(u8, parse_res.err_msg, "Block requires at least one statement. Use the `pass` statement as a placeholder.") != null);
}

test "Integers." {
    // Unsupported integer notation.
    try eval(.{ .silent = true },
        \\var a = 0z000
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.TokenError,
            \\ParseError: Unsupported integer notation: z
            \\
            \\main:1:10:
            \\var a = 0z000
            \\         ^
            \\
        );
    }}.func);

    // Empty char is not allowed.
    try eval(.{ .silent = true },
        \\var a = 0u''
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Invalid UTF-8 Rune.
            \\
            \\main:1:9:
            \\var a = 0u''
            \\        ^
            \\
        );
    }}.func);

    // More than one rune in literal. 
    try eval(.{ .silent = true },
        \\var a = 0u'🦊a'
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Invalid UTF-8 Rune.
            \\
            \\main:1:9:
            \\var a = 0u'🦊a'
            \\        ^
            \\
        );
    }}.func);

    // More than one rune in literal. (Grapheme cluster)
    try eval(.{ .silent = true },
        \\var a = 0u'👨‍👨‍👦‍👦'
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Invalid UTF-8 Rune.
            \\
            \\main:1:9:
            \\var a = 0u'👨‍👨‍👦‍👦'
            \\        ^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("int_test.cy"));
}

test "Floats." {
    try evalPass(.{}, @embedFile("float_test.cy"));
}

test "Operator precedence." {
    try evalPass(.{}, @embedFile("op_precedence_test.cy"));
}

test "Comments" {
    const run = VMrunner.create();
    defer run.destroy();

    // Single line comment.
    var val = try run.eval(
        \\-- 1
        \\2
    );
    try t.eq(val.asInteger(), 2);
    val = try run.eval(
        \\2
        \\-- 1
    );
    try t.eq(val.asInteger(), 2);

    // Multiple single line comments.
    val = try run.eval(
        \\-- 1
        \\-- 2
        \\-- 3
        \\4
    );
    try t.eq(val.asInteger(), 4);
}

test "Escape sequence." {
    try evalPass(.{}, @embedFile("escape_seq_test.cy"));
}

test "String cases." {
    try evalPass(.{}, @embedFile("string_test.cy"));
}

test "ASCII strings." {
    try evalPass(.{}, @embedFile("astring_test.cy"));
}

test "UTF-8 strings." {
    try evalPass(.{}, @embedFile("ustring_test.cy"));
}

test "ASCII string slice." {
    try evalPass(.{}, @embedFile("astring_slice_test.cy"));
}

test "UTF-8 string slice." {
    try evalPass(.{}, @embedFile("ustring_slice_test.cy"));
}

test "Array." {
    try evalPass(.{}, @embedFile("array_test.cy"));
}

test "Array slice." {
    try evalPass(.{}, @embedFile("array_slice_test.cy"));
}

test "String interpolation." {
    try evalPass(.{}, @embedFile("string_interpolation_test.cy"));
}

test "Lists" {
    try evalPass(.{}, @embedFile("list_test.cy"));
}

test "Maps" {
    try evalPass(.{}, @embedFile("map_test.cy"));
}

test "Op assignment statement." {
    try evalPass(.{}, @embedFile("opassign_test.cy"));
}

test "Undeclared variable references." {
    // Reading an undeclared variable is a compile error.
    try eval(.{ .silent = true },
        \\import t 'test'
        \\t.eq(a, 123)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Undeclared variable `a`.
            \\
            \\main:2:6:
            \\t.eq(a, 123)
            \\     ^
            \\
        );
    }}.func);

    // Reading an undeclared variable from unrelated block is a compile error.
    try eval(.{ .silent = true },
        \\if true:
        \\  var a = 123
        \\func foo():
        \\  return a
        \\
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Undeclared variable `a`.
            \\
            \\main:4:10:
            \\  return a
            \\         ^
            \\
        );
    }}.func);

    // Using an undeclared variable as a callee is a compile error.
    try eval(.{ .silent = true },
        \\a()
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Undeclared variable `a`.
            \\
            \\main:1:1:
            \\a()
            \\^
            \\
        );
    }}.func);
}

test "Typed static variable declaration." {
    // Type check on initializer.
    try eval(.{ .silent = true },
        \\var Root.a float = []
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Expected type `float`, got `List`.
            \\
            \\main:1:20:
            \\var Root.a float = []
            \\                   ^
            \\
        );
    }}.func);
}

test "Static variable declaration." {
    // Capturing a local variable in a static var initializer is not allowed.
    try eval(.{ .silent = true },
        \\var b = 123
        \\var Root.a = b
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Could not find the symbol `b`.
            \\
            \\main:2:14:
            \\var Root.a = b
            \\             ^
            \\
        );
    }}.func);

    // Can't reference self.
    try eval(.{ .silent = true },
        \\var Root.c = c
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Reference to `c` creates a circular dependency.
            \\
            \\main:1:14:
            \\var Root.c = c
            \\             ^
            \\
        );
    }}.func);

    // Can't have circular reference.
    try eval(.{ .silent = true },
        \\var Root.a = b
        \\var Root.b = a
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Referencing `a` creates a circular dependency in the module.
            \\
            \\main:2:14:
            \\var Root.b = a
            \\             ^
            \\
        );
    }}.func);

    // Declaration that depends on another.
    _ = try evalPass(.{}, 
        \\import t 'test'
        \\var Root.a = 123
        \\var Root.b = a + 321
        \\var Root.c = a + b
        \\t.eq(a, 123) 
        \\t.eq(b, 444) 
        \\t.eq(c, 567) 
    );

    // Depends on and declared before another.
    _ = try evalPass(.{}, 
        \\import t 'test'
        \\var Root.c = a + b
        \\var Root.b = a + 321
        \\var Root.a = 123
        \\t.eq(a, 123) 
        \\t.eq(b, 444) 
        \\t.eq(c, 567) 
    );

    // Declaration over using builtin module. 
    try evalPass(.{},
        \\var Root.print = 123
    );

    try evalPass(.{}, @embedFile("staticvar_decl_test.cy"));
}

test "Static variable assignment." {
    try evalPass(.{}, @embedFile("staticvar_assign_test.cy"));
}

test "Typed local variable." {
    try eval(.{ .silent = true },
        \\var a float = "123"
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Expected type `float`, got `string`.
            \\
            \\main:1:16:
            \\var a float = "123"
            \\               ^
            \\
        );
    }}.func);

    // Reassign with incompatible type.
    try eval(.{ .silent = true },
        \\var a float = 123 
        \\a = "123"
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Expected type `float`, got `string`.
            \\
            \\main:2:6:
            \\a = "123"
            \\     ^
            \\
        );
    }}.func);

    // Reassign with compatible type.
    try evalPass(.{},
        \\var a float = 123 
        \\a = 234
    );
}

test "Local variable declaration." {
    // Can't redeclare var.    
    try eval(.{ .silent = true },
        \\var a = 1
        \\var a = 2
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Variable `a` is already declared in the block.
            \\
            \\main:2:5:
            \\var a = 2
            \\    ^
            \\
        );
    }}.func);

    // Declaring a var that is already referencing a parent local.
    try eval(.{ .silent = true },
        \\var a = 1
        \\var foo = func():
        \\  -- Captured for read.
        \\  print a
        \\  -- Attempting to declare `a`.
        \\  var a = 3
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: `a` already references a parent local variable.
            \\
            \\main:6:7:
            \\  var a = 3
            \\      ^
            \\
        );
    }}.func);

    // Declaring a var that is already referencing a static var.
    try eval(.{ .silent = true },
        \\var Root.a = 1
        \\var foo = func():
        \\  print a
        \\  -- Attempting to declare `a`.
        \\  var a = 3
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: `a` already references a static variable.
            \\
            \\main:5:7:
            \\  var a = 3
            \\      ^
            \\
        );
    }}.func);

    // New local inside sub-block.
    try evalPass(.{},
        \\import test
        \\var a = 1
        \\if true:
        \\  var a = 2
        \\  test.eq(a, 2)
        \\test.eq(a, 1)
    );

    // Can't reference var from non parent if block.
    try eval(.{ .silent = true },
        \\if true:
        \\  var a = 1
        \\a
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Undeclared variable `a`.
            \\
            \\main:3:1:
            \\a
            \\^
            \\
        );
    }}.func);

    // Can't reference iter var from non parent for block.
    try eval(.{ .silent = true },
        \\for 0..10 -> i:
        \\  pass
        \\i
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Undeclared variable `i`.
            \\
            \\main:3:1:
            \\i
            \\^
            \\
        );
    }}.func);

    // Can't reference var from non parent for block.
    try eval(.{ .silent = true },
        \\for 0..10:
        \\  var i = 0
        \\i
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Undeclared variable `i`.
            \\
            \\main:3:1:
            \\i
            \\^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("localvar_decl_test.cy"));
}

test "Local variable assignment." {
    try evalPass(.{}, @embedFile("localvar_assign_test.cy"));

    // Assign to missing $setIndex method.
    try eval(.{ .silent = true },
        \\1[0] = 2
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: `func $setIndex(any, int, int) any` can not be found in `int`.
            \\
            \\main:1:1 main:
            \\1[0] = 2
            \\^
            \\
        );
    }}.func);

    // Assign to undeclared var.
    try eval(.{ .silent = true },
        \\foo = 1
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Undeclared variable `foo`.
            \\
            \\main:1:1:
            \\foo = 1
            \\^
            \\
        );
    }}.func);

    // Initializing an object in a branch will auto generate initializers at the start of
    // the function. This test sets freed object values along the undefined stack space.
    // If the initializers were generated, the release on `a` would succeed.
    // If not `a` would refer to a freed object value and fail the release op.
    try evalPass(.{
        .preEval = struct {
            fn func(run: *VMrunner) void {
                run.vm.fillUndefinedStackSpace(cy.Value.initNoCycPtr(null));
            }
        }.func,
    },
        \\type S object:
        \\  var value
        \\if false:
        \\  var a = [S value: 123]
    );

    // Same test in method scope.
    try evalPass(.{
        .preEval = struct {
            fn func(run: *VMrunner) void {
                run.vm.fillUndefinedStackSpace(cy.Value.initNoCycPtr(null));
            }
        }.func,
    },
        \\type S object:
        \\  var value
        \\  func foo():
        \\    if false:
        \\      var a = [S value: 123]
        \\var s = [S value: 234]
        \\s.foo()
    );
}

test "Cond expression" {
    try evalPass(.{},
        \\import test
        \\var foo = true
        \\test.eq(foo ? 123 else 456, 123)
        \\foo = false
        \\test.eq(foo ? 123 else 456, 456)
        \\
        \\-- Types are merged.
        \\var a = false ? 123 else '$(123)456'
        \\test.eq(a, '123456')
        \\-- `a` should be released since else returns a heap string.
    );
}

test "Return statement." {
    // return multi-line lambda
    try evalPass(.{},
        \\import t 'test'
        \\func foo():
        \\  return func():
        \\    return 123
        \\t.eq(foo()(), 123)
    );

    const run = VMrunner.create();
    defer run.destroy();

    // If/else.
    var val = try run.eval(
        \\var foo = true
        \\if foo:
        \\  return 123
    );
    try t.eq(val.asInteger(), 123);

    val = try run.eval(
        \\var foo = false
        \\if foo:
        \\  return 123
        \\else:
        \\  return 456
    );
    try t.eq(val.asInteger(), 456);

    // else if condition.
    val = try run.eval(
        \\if false:
        \\  return 456
        \\else true:
        \\  return 123
        \\else:
        \\  return 456
    );
    try t.eq(val.asInteger(), 123);
}

test "Switch." {
    try evalPass(.{}, @embedFile("switch_test.cy"));
}

test "If statement." {
    try evalPass(.{}, @embedFile("if_test.cy"));
}

test "Infinite while loop." {
    try evalPass(.{}, @embedFile("while_inf_test.cy"));
}

test "While conditional." {
    try evalPass(.{}, @embedFile("while_cond_test.cy"));
}

test "While optional." {
    try evalPass(.{}, @embedFile("while_opt_test.cy"));
}

test "For iterator." {
    // Iterable does not have iterator().
    try eval(.{ .silent = true },
        \\for 123 -> i:
        \\  print i
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: `func iterator(any) any` can not be found in `int`.
            \\
            \\main:1:5 main:
            \\for 123 -> i:
            \\    ^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("for_iter_test.cy"));
}

test "For loop over range." {
    try evalPass(.{}, @embedFile("for_range_test.cy"));
}

test "Native function call." {
    try evalPass(.{},
        \\import t 'test'
        \\var list = []
        \\for 0..10 -> i:
        \\   list.append(i)
        \\t.eq(list[9], 9)
    );
}

test "Closures." {
    try evalPass(.{}, @embedFile("closure_test.cy"));
}

test "Function recursion." {
    try evalPass(.{}, @embedFile("recursion_test.cy"));
}

test "Function overloading." {
    try evalPass(.{}, @embedFile("function_overload_test.cy"));
}

test "Typed static functions." {
    // Check return type. 
    try eval(.{ .silent = true },
        \\func foo() int:
        \\  return 1.2
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Expected type `int`, got `float`.
            \\
            \\main:2:10:
            \\  return 1.2
            \\         ^
            \\
        );
    }}.func);

    // Can't resolve param type.
    try eval(.{ .silent = true },
        \\func foo(a Vec2):
        \\  pass
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Could not find the symbol `Vec2`.
            \\
            \\main:1:12:
            \\func foo(a Vec2):
            \\           ^
            \\
        );
    }}.func);

    // Infer type from param.
    try evalPass(.{},
        \\import test
        \\func foo(a float):
        \\  return a
        \\test.eq(foo(2), 2.0)
    );

    // Infer type from object func param. 
    try evalPass(.{},
        \\import test
        \\type S object
        \\func S.foo(a float):
        \\  return a
        \\test.eq(S.foo(2), 2.0)
    );
}

test "Static functions." {
    // Call with missing func sym.
    try eval(.{ .silent = true },
        \\foo(1)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Undeclared variable `foo`.
            \\
            \\main:1:1:
            \\foo(1)
            \\^
            \\
        );
    }}.func);

    // Call with wrong number of arguments.
    try eval(.{ .silent = true },
        \\func foo():
        \\  return 1
        \\foo(1)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo() dynamic
            \\
            \\main:3:1:
            \\foo(1)
            \\^
            \\
        );
    }}.func);

    // Call with wrong number of arguments for overloaded function.
    try eval(.{ .silent = true },
        \\func foo():
        \\  return 1
        \\func foo(n):
        \\  return n
        \\foo(1, 2)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function for call signature: `foo(int, int) any`.
            \\Functions named `foo` in `main`:
            \\    func foo() dynamic
            \\    func foo(dynamic) dynamic
            \\
            \\main:5:1:
            \\foo(1, 2)
            \\^
            \\
        );
    }}.func);

    // Reassign to a function with a different signature.
    try eval(.{ .silent = true },
        \\func toNum(a):
        \\    pass
        \\func foo():
        \\    pass
        \\foo = toNum
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Assigning to static function `func () dynamic` with a different function signature `func (dynamic) dynamic`.
            \\
            \\main:5:1 main:
            \\foo = toNum
            \\^
            \\
        );
    }}.func);

    // Allow declaring over using builtins namespace.
    try evalPass(.{},
        \\import t 'test'
        \\func print(v):
        \\  return v + 2
        \\t.eq(print(1), 3)
    );

    // Capture local from static function is not allowed.
    // TODO: Provide better error message. Since function declarations are now analyzed separately this info
    //       isn't available but the parser could accumulate local var decls.
    try eval(.{ .silent = true },
        \\var a = 123
        \\func foo():
        \\  return a
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Undeclared variable `a`.
            \\
            \\main:3:10:
            \\  return a
            \\         ^
            \\
        );
    }}.func);

    // Capturing from static function is not allowed.
    try eval(.{ .silent = true },
        \\var a = 123
        \\func foo():
        \\  a = 234
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Undeclared variable `a`.
            \\
            \\main:3:3:
            \\  a = 234
            \\  ^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("static_func_test.cy"));
}

test "Lambdas." {
    _ = try evalPass(.{}, @embedFile("lambda_test.cy"));
}

// test "Function named parameters call." {
//     const run = VMrunner.create();
//     defer run.destroy();

//     var val = try run.eval(
//         \\func foo(a, b):
//         \\  return a - b
//         \\foo(a: 3, b: 1)
//     );
//     try t.eq(val.asF64toI32(), 2);
//     run.deinitValue(val);

//     val = try run.eval(
//         \\func foo(a, b):
//         \\  return a - b
//         \\foo(a: 1, b: 3)
//     );
//     try t.eq(val.asF64toI32(), -2);
//     run.deinitValue(val);

//     // New line as arg separation.
//     val = try run.eval(
//         \\func foo(a, b):
//         \\  return a - b
//         \\foo(
//         \\  a: 3
//         \\  b: 1
//         \\)
//     );
//     try t.eq(val.asF64toI32(), 2);
//     run.deinitValue(val);
// }

test "access expression" {
    try evalPass(.{},
        \\import t 'test'
        \\
        \\-- One level of access from parent.
        \\var map = [ a: 5 ]
        \\t.eq(map.a, 5)
        \\
        \\-- Multiple levels of access from parent.
        \\map = [ a: [ b: 5 ] ]
        \\t.eq(map.a.b, 5)
    );
}

test "Math module." {
    try evalPass(.{}, @embedFile("math_test.cy"));
}

test "Bitwise operators." {
    try evalPass(.{}, @embedFile("bitwise_op_test.cy"));
}

test "Arithmetic operators." {
    // Can't add objects by default.
    try eval(.{ .silent = true },
        \\type S object:
        \\  var a any
        \\var a = [S a: 123] + 123
        \\
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: `func $infix+(any, int) any` can not be found in `S`.
            \\
            \\main:3:20 main:
            \\var a = [S a: 123] + 123
            \\                   ^
            \\
        );
    }}.func);

    // Can only subtract numbers.
    try eval(.{ .silent = true },
        \\var a = 'foo' - 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: `func $infix-(any, int) any` can not be found in `string`.
            \\
            \\main:1:15 main:
            \\var a = 'foo' - 123
            \\              ^
            \\
        );
    }}.func);

    // Can only multiply numbers.
    try eval(.{ .silent = true },
        \\var a = 'foo' * 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: `func $infix*(any, int) any` can not be found in `string`.
            \\
            \\main:1:15 main:
            \\var a = 'foo' * 123
            \\              ^
            \\
        );
    }}.func);

    // Can only divide numbers.
    try eval(.{ .silent = true },
        \\var a = 'foo' / 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: `func $infix/(any, int) any` can not be found in `string`.
            \\
            \\main:1:15 main:
            \\var a = 'foo' / 123
            \\              ^
            \\
        );
    }}.func);

    // Can not mod string
    try eval(.{ .silent = true },
        \\var a = 'foo' % 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: `func $infix%(any, int) any` can not be found in `string`.
            \\
            \\main:1:15 main:
            \\var a = 'foo' % 123
            \\              ^
            \\
        );
    }}.func);

    // Can not pow string.
    try eval(.{ .silent = true },
        \\var a = 'foo' ^ 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: `func $infix^(any, int) any` can not be found in `string`.
            \\
            \\main:1:15 main:
            \\var a = 'foo' ^ 123
            \\              ^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("arithmetic_op_test.cy"));
}

test "ARC cycles." {
    // GC is able to detect reference cycle.
    _ = try evalPass(.{},
        \\import t 'test'
        \\func foo():
        \\  var a = []
        \\  var b = []
        \\  a.append(b)
        \\  b.append(a)
        \\  var res = performGC()
        \\  -- Cycle still alive in the current stack so no gc.
        \\  t.eq(res['numCycFreed'], 0)
        \\foo()
        \\var res = performGC()
        \\t.eq(res['numCycFreed'], 2)
    );

    // Reference cycle but still reachable from a root value.
    _ = try evalPass(.{ .cleanupGC = true }, 
        \\import t 'test'
        \\var Root.g = none
        \\var a = []
        \\var b = []
        \\a.append(b)
        \\b.append(a)
        \\g = a
        \\var res = performGC()
        \\t.eq(res['numCycFreed'], 0)
    );

    // Reference cycle with child non cyclable.
    _ = try evalPass(.{}, 
        \\import t 'test'
        \\func foo():
        \\  var a = []
        \\  var b = []
        \\  a.append(b)
        \\  b.append(a)
        \\  a.append(pointer(1))
        \\foo()
        \\var res = performGC()
        \\t.eq(res['numCycFreed'], 2)
    );

    // Reference cycle with non pool objects.
    _ = try evalPass(.{}, 
        \\import t 'test'
        \\type T object:
        \\  var a
        \\  var b
        \\  var c
        \\  var d
        \\  var e
        \\func foo():
        \\  var a = [T:]
        \\  var b = [T:]
        \\  a.c = b
        \\  b.c = a
        \\foo()
        \\var res = performGC()
        \\t.eq(res['numCycFreed'], 2)
    );
}
