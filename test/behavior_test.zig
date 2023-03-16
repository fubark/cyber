const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const t = stdx.testing;

const vm_ = @import("../src/vm.zig");
const cy = @import("../src/cyber.zig");
const http = @import("../src/http.zig");
const bindings = @import("../src/builtins/bindings.zig");
const log = stdx.log.scoped(.behavior_test);

test "Typed taglit." {
    // Wrong param type.
    try eval(.{ .silent = true },
        \\func foo(a taglit):
        \\  pass
        \\foo(123)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function signature for `foo(number) any`.
            \\Only `func foo(taglit) any` exists for the symbol `foo`.
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
        \\func foo(a taglit):
        \\  return a == #sometag
        \\
        \\-- Literal.
        \\try t.eq(foo(#sometag), true)
        \\
        \\-- From var.
        \\tag = #sometag
        \\try t.eq(foo(tag), true)
        \\
        \\-- Cast erased type.
        \\tag = t.erase(#sometag)
        \\try t.eq(foo(taglit(tag)), true)
    );
}

test "Typed function params." {
    // Can't resolve param type.
    try eval(.{ .silent = true },
        \\func foo(a Vec2):
        \\  pass
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Could not find type symbol `Vec2`.
            \\
            \\main:1:12:
            \\func foo(a Vec2):
            \\           ^
            \\
        );
    }}.func);
}

test "Typed function calls." {
    // Calling static function with different type.
    try eval(.{ .silent = true },
        \\func foo(a number):
        \\  return a + 3
        \\foo(none)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function signature for `foo(any) any`.
            \\Only `func foo(number) any` exists for the symbol `foo`.
            \\
            \\main:3:1:
            \\foo(none)
            \\^
            \\
        );
    }}.func);
}

test "Typed recursive function." {
    try evalPass(.{}, @embedFile("typed_rec_func_test.cy"));
}

test "Custom modules." {
    const run = VMrunner.create();
    defer run.destroy();

    var count: usize = 0;
    run.vm.setUserData(&count);

    const S = struct {
        fn test1(vm: *cy.UserVM, _: [*]const cy.Value, _: u8) cy.Value {
            const count_ = stdx.ptrAlignCast(*usize, vm.getUserData());
            count_.* += 1;
            return cy.Value.None;
        }
        fn test2(vm: *cy.UserVM, _: [*]const cy.Value, _: u8) cy.Value {
            const count_ = stdx.ptrAlignCast(*usize, vm.getUserData());
            count_.* += 2;
            return cy.Value.None;
        }
        fn test3(vm: *cy.UserVM, _: [*]const cy.Value, _: u8) cy.Value {
            const count_ = stdx.ptrAlignCast(*usize, vm.getUserData());
            count_.* += 3;
            return cy.Value.None;
        }
    };

    try run.vm.addModuleLoader("mod1", struct {
        fn loader(vm: *cy.UserVM, mod: *cy.Module) bool {
            // Test dangling pointer.
            const s1 = allocString("test");
            const s2 = allocString("test2");
            defer t.alloc.free(s1);
            defer t.alloc.free(s2);
            mod.setNativeFuncExt(&vm.internal().compiler, s1, true, 0, S.test1) catch fatal();
            mod.setNativeFuncExt(&vm.internal().compiler, s2, true, 0, S.test2) catch fatal();
            return true;
        }
    }.loader);

    try run.vm.addModuleLoader("mod2", struct {
        fn loader(vm: *cy.UserVM, mod: *cy.Module) bool {
            // Test dangling pointer.
            const s1 = allocString("test");
            defer t.alloc.free(s1);
            mod.setNativeFuncExt(&vm.internal().compiler, s1, true, 0, S.test3) catch fatal();
            return true;
        }
    }.loader);

    const src1 = try t.alloc.dupe(u8, 
        \\import m 'mod1'
        \\import n 'mod2'
        \\m.test()
        \\m.test2()
        \\n.test()
    );
    _ = try run.evalExtNoReset(.{},
        src1
    );

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

    try run.vm.addModuleLoader("core", struct {
        fn loader(vm: *cy.UserVM, mod: *cy.Module) bool {
            const g = stdx.ptrAlignCast(*cy.Value, vm.getUserData()).*;
            mod.setVar(&vm.internal().compiler, "g", g) catch fatal();
            return true;
        }
    }.loader);

    const src1 =
        \\g['a'] = 1
        ;
    _ = try run.vm.eval("main", src1, .{ 
        .singleRun = false,
        .enableFileModules = false,
        .genAllDebugSyms = false,
    });

    const src2 = 
        \\import t 'test'
        \\try t.eq(g['a'], 1)
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
        \\a = 1
        \\try t.eq(a, 1)
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
        \\a = 1
        \\@genLabel('MyLabel')
        \\a = 1
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try t.eq(std.meta.isError(res), false);
        const vm = run.vm.internal();
        try t.eq(vm.compiler.buf.debugLabels.items.len, 1);
        const actLabel = vm.compiler.buf.debugLabels.items[0];
        try t.eq(actLabel.pc, 3);
        try t.eqStr(actLabel.getName(), "MyLabel");
    }}.func);
}

test "User parse errors." {
    const run = VMrunner.create();
    defer run.destroy();

    // Test parse error on first line.
    var val = run.evalExt(.{ .silent = true },
        \\var
        \\var a: 123
        \\var b: 234
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
        \\var a: 123
        \\var
        \\var b: 234
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
        \\var a: 123
        \\var b: 234
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
    try evalPass(Config.initFileModules("./test/typespec_test.cy"), @embedFile("typespec_test.cy"));
}

test "Type alias." {
    // TODO: Rename to typealias_test.cy
    try evalPass(Config.initFileModules("./test/atype_test.cy"), @embedFile("atype_test.cy"));
}

test "Import http spec." {
    const run = VMrunner.create();
    defer run.destroy();

    const basePath = try std.fs.realpathAlloc(t.alloc, ".");
    defer t.alloc.free(basePath);

    // Import error.UnknownHostName.
    try run.resetEnv();
    var client = http.MockHttpClient.init();
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
    client = http.MockHttpClient.init();
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
    client = http.MockHttpClient.init();
    client.retBody =
        \\export var foo: 123
        ;
    run.vm.internal().httpClient = client.iface();
    _ = try run.evalExtNoReset(Config.initFileModules("./test/import_test.cy"),
        \\import a 'https://exists.com/a.cy'
        \\import t 'test'
        \\try t.eq(a.foo, 123)
    );
}

test "Imports." {
    const run = VMrunner.create();
    defer run.destroy();

    // Import missing file.
    var res = run.evalExt(Config.initFileModules("./test/import_test.cy").withSilent(),
        \\import a 'test_mods/missing.cy'
        \\b = a
    );
    try t.expectError(res, error.CompileError);
    const errMsg = run.vm.getCompileErrorMsg();
    try t.expect(std.mem.startsWith(u8, errMsg, "Import path does not exist:"));
    try t.expect(std.mem.indexOf(u8, errMsg, "test/test_mods/missing.cy") != null);

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
    res = run.evalExt(Config.initFileModules("./test/import_test.cy").withSilent(),
        \\import a 'test_mods/a.cy'
        \\b = a.missing
    );
    try run.expectErrorReport(res, error.CompileError,
        \\CompileError: Missing symbol: `missing`
        \\
        \\@AbsPath(test/import_test.cy):2:7:
        \\b = a.missing
        \\      ^
        \\
    );

    // Failed to set func from another module
    res = run.evalExt(Config.initFileModules("./test/import_test.cy").withSilent(),
        \\import a 'test_mods/init_func_error.cy'
        \\import t 'test'
        \\try t.eq(valtag(a.foo), #function)
    );
    try run.expectErrorReport(res, error.Panic,
        \\panic: Assigning to static function with a different function signature.
        \\
        \\@AbsPath(test/test_mods/init_func_error.cy):1:21 main:
        \\export func foo() = number
        \\                    ^
        \\
    );

    // Import using relative path prefix.
    _ = try run.evalExt(Config.initFileModules("./test/import_test.cy"),
        \\import a './test_mods/a.cy'
        \\import t 'test'
        \\try t.eq(a.varNum, 123)
    );

    // Import using implied relative path prefix.
    _ = try run.evalExt(Config.initFileModules("./test/import_test.cy"),
        \\import a 'test_mods/a.cy'
        \\import t 'test'
        \\try t.eq(a.varNum, 123)
    );

    // Import using unresolved relative path.
    _ = try run.evalExt(Config.initFileModules("./test/import_test.cy"),
        \\import a './test_mods/../test_mods/a.cy'
        \\import t 'test'
        \\try t.eq(a.varNum, 123)
    );

    // Import when running main script in the cwd.
    try std.os.chdir("./test");
    _ = try run.evalExt(Config.initFileModules("./import_test.cy"),
        \\import a 'test_mods/a.cy'
        \\import t 'test'
        \\try t.eq(a.varNum, 123)
    );

    // Import when running main script in a child directory.
    try std.os.chdir("./test_mods");
    _ = try run.evalExt(Config.initFileModules("../import_test.cy"),
        \\import a 'test_mods/a.cy'
        \\import t 'test'
        \\try t.eq(a.varNum, 123)
    );

    run.deinit();

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
        \\  @dumpLocals()
    );
}

test "core module" {
    try evalPass(.{}, @embedFile("core_test.cy"));
}

test "os module" {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\import os 'os'
        \\os.system
    );
    try t.eqStr(try run.assertValueString(val), @tagName(builtin.os.tag));

    val = try run.eval(
        \\import os 'os'
        \\os.cpu
    );
    try t.eqStr(try run.assertValueString(val), @tagName(builtin.cpu.arch));

    val = try run.eval(
        \\import os 'os'
        \\os.endian
    );
    if (builtin.cpu.arch.endian() == .Little) {
        try t.eq(val.asTagLiteralId(), @enumToInt(bindings.TagLit.little));
    } else {
        try t.eq(val.asTagLiteralId(), @enumToInt(bindings.TagLit.big));
    }

    run.deinit();

    try evalPass(.{}, @embedFile("os_test.cy"));
}

test "Fibers" {
    try evalPass(.{}, @embedFile("fiber_test.cy"));
}

test "try value" {
    const run = VMrunner.create();
    defer run.destroy();

    // Try failure at root block panics.
    const res = run.evalExt(.{ .silent = true },
        \\try error(#boom)
    );
    try t.expectError(res, error.Panic);

    run.deinit();
    try evalPass(.{}, @embedFile("try_test.cy"));
}

test "Errors." {
    try evalPass(.{}, @embedFile("error_test.cy"));
}

test "FFI." {
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
        export fn testCharPtrZ(ptr: [*:0]u8) [*:0]const u8 {
            const slice = std.mem.span(ptr);
            std.mem.copy(u8, &buf, slice);
            buf[slice.len] = 0;
            return @ptrCast([*:0]const u8, &buf);
        }
        export fn testDupeCharPtrZ(ptr: [*:0]u8) [*:0]const u8 {
            const slice = std.mem.span(ptr);
            std.mem.copy(u8, &buf, slice);
            buf[slice.len] = 0;
            std.c.free(ptr);
            return @ptrCast([*:0]const u8, &buf);
        }
        export fn testPtr(ptr: *anyopaque) *anyopaque {
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
            const slice = std.mem.span(o.c);
            std.mem.copy(u8, &buf, slice);
            buf[slice.len] = 0;
            return MyObject{
                .a = o.a,
                .b = o.b,
                .c = @ptrCast([*:0]u8, &buf),
                .d = o.d,
            };
        }
        export fn testRetObjectPtr(o: MyObject) *MyObject {
            const slice = std.mem.span(o.c);
            std.mem.copy(u8, &buf, slice);
            buf[slice.len] = 0;
            temp = .{
                .a = o.a,
                .b = o.b,
                .c = @ptrCast([*:0]u8, &buf),
                .d = o.d,
            };
            return &temp;
        }
        var temp: MyObject = undefined;
        var buf: [1024]u8 = undefined;
    };
    _ = S;

    try evalPass(.{}, @embedFile("ffi_test.cy"));
}

test "Tag types." {
    const run = VMrunner.create();
    defer run.destroy();

    // Tag literal.
    var val = try run.eval(
        \\n = #Tiger
        \\number(n)
    );
    const id = try vm_.gvm.ensureTagLitSym("Tiger");
    try t.eq(val.asF64toI32(), @intCast(i32, id));

    _ = try run.eval(@embedFile("tagtype_test.cy"));
}

test "test module" {
    const run = VMrunner.create();
    defer run.destroy();

    const res = run.evalExt(.{ .silent = true },
        \\import t 'test'
        \\try t.eq(123, 234)
    );
    try t.expectError(res, error.Panic);

    _ = try run.eval(@embedFile("test_mod_test.cy"));
}

test "Objects." {
    const run = VMrunner.create();
    defer run.destroy();

    // Missing semicolon.
    var res = run.evalExt(.{ .silent = true },
        \\object Vec2
        \\  x
        \\  y
    );
    try run.expectErrorReport(res, error.ParseError,
        \\ParseError: Expected colon to start an object type block.
        \\
        \\main:1:12:
        \\object Vec2
        \\           ^
        \\
    );

    // Field declaration ends the file without parser error.
    _ = try run.eval(
        \\object Vec2:
        \\  x
        \\  y
    );
    _ = try run.eval(
        \\object Vec2:
        \\  x number
        \\  y number
    );

    // Initialize with undeclared field.
    res = run.evalExt(.{ .silent = true },
        \\object S:
        \\  a
        \\o = S{ b: 100 }
    );
    try run.expectErrorReport(res, error.CompileError,
        \\CompileError: Missing field `b` in `S`.
        \\
        \\main:3:8:
        \\o = S{ b: 100 }
        \\       ^
        \\
    );

    // Write to undeclared field.
    res = run.evalExt(.{ .silent = true },
        \\object S:
        \\  a
        \\o = S{ a: 100 }
        \\o.b = 200
    );
    try run.expectErrorReport(res, error.Panic,
        \\panic: Field not found in value.
        \\
        \\main:4:1 main:
        \\o.b = 200
        \\^
        \\
    );

    // Calling a missing method name.
    res = run.evalExt(.{ .silent = true },
        \\object S:
        \\  a
        \\o = S{}
        \\o.foo()
    );
    try run.expectErrorReport(res, error.Panic,
        \\panic: `foo` is either missing in `S` or the call signature: foo(self, 0 args) is unsupported.
        \\
        \\main:4:1 main:
        \\o.foo()
        \\^
        \\
    );

    // Calling a method with the wrong signature.
    res = run.evalExt(.{ .silent = true },
        \\object S:
        \\  a
        \\  func foo(self):
        \\    return 123
        \\o = S{}
        \\o.foo(234)
    );
    try run.expectErrorReport(res, error.Panic,
        \\panic: `foo` is either missing in `S` or the call signature: foo(self, 1 args) is unsupported.
        \\
        \\main:6:1 main:
        \\o.foo(234)
        \\^
        \\
    );

    _ = try run.eval(@embedFile("object_test.cy"));
}

test "Object methods." {
    const run = VMrunner.create();
    defer run.destroy();

    // self param.
    var val = try run.eval(
        \\object Node:
        \\  value
        \\  func get(self):
        \\    return self.value
        \\n = Node{ value: 123 }
        \\n.get()
    );
    try t.eq(val.asF64toI32(), 123);

    // self param with regular param.
    val = try run.eval(
        \\object Node:
        \\  value
        \\  func get(self, param):
        \\    return self.value + param
        \\n = Node{ value: 123 }
        \\n.get(321)
    );
    try t.eq(val.asF64toI32(), 444);

    // self param with many regular param.
    val = try run.eval(
        \\object Node:
        \\  value
        \\  func get(self, param, param2):
        \\    return self.value + param - param2
        \\n = Node{ value: 123 }
        \\n.get(321, 1)
    );
    try t.eq(val.asF64toI32(), 443);

    // Static method, no params.
    val = try run.eval(
        \\object Node:
        \\  value
        \\  func get():
        \\    return 123
        \\Node.get()
    );
    try t.eq(val.asF64toI32(), 123);

    // Static method, one params.
    val = try run.eval(
        \\object Node:
        \\  value
        \\  func get(param):
        \\    return 123 + param
        \\Node.get(321)
    );
    try t.eq(val.asF64toI32(), 444);

    // Static method, many params.
    val = try run.eval(
        \\object Node:
        \\  value
        \\  func get(param, param2):
        \\    return 123 + param - param2
        \\Node.get(321, 1)
    );
    try t.eq(val.asF64toI32(), 443);
}

test "must()" {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\a = 123
        \\-- no error, must returns argument.
        \\try t.eq(must(a), 123)
    );

    var res = run.evalExt(.{ .silent = true },
        \\a = error(#boom)
        \\must(a)
    );
    try t.expectError(res, error.Panic);
    var trace = run.getStackTrace();
    try run.assertPanicMsg("error#boom");
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .chunkId = 0,
        .line = 1,
        .col = 0,
        .lineStartPos = 17,
    });
}

test "panic()" {
    const run = VMrunner.create();
    defer run.destroy();

    var res = run.evalExt(.{ .silent = true },
        \\a = 123
        \\1 + panic(#boom)
    );
    try t.expectError(res, error.Panic);
    var trace = run.getStackTrace();
    try run.assertPanicMsg("#boom");
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .chunkId = 0,
        .line = 1,
        .col = 4,
        .lineStartPos = 8,
    });
}

test "Stack trace unwinding." {
    const run = VMrunner.create();
    defer run.destroy();

    var res = run.evalExt(.{ .silent = true },
        \\a = 123
        \\1 + a.foo
    );
    try run.expectErrorReport(res, error.Panic,
        \\panic: Field not found in value.
        \\
        \\main:2:5 main:
        \\1 + a.foo
        \\    ^
        \\
    );
    var trace = run.getStackTrace();
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .chunkId = 0,
        .line = 1,
        .col = 4,
        .lineStartPos = 8,
    });

    // Function stack trace.
    res = run.evalExt(.{ .silent = true },
        \\func foo():
        \\  a = 123
        \\  return 1 + a.foo
        \\foo()
    );
    try run.expectErrorReport(res, error.Panic,
        \\panic: Field not found in value.
        \\
        \\main:3:14 foo:
        \\  return 1 + a.foo
        \\             ^
        \\main:4:1 main:
        \\foo()
        \\^
        \\
    );
    trace = run.getStackTrace();
    try t.eq(trace.frames.len, 2);
    try eqStackFrame(trace.frames[0], .{
        .name = "foo",
        .chunkId = 0,
        .line = 2,
        .col = 13,
        .lineStartPos = 22,
    });
    try eqStackFrame(trace.frames[1], .{
        .name = "main",
        .chunkId = 0,
        .line = 3,
        .col = 0,
        .lineStartPos = 41,
    });

    // panic from another module.
    res = run.evalExt(.{ .silent = true, .uri = "./test/main.cy" },
        \\import a 'test_mods/init_panic_error.cy'
        \\import t 'test'
        \\try t.eq(a.foo, 123)
    );
    try t.expectError(res, error.Panic);
    trace = run.getStackTrace();
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .chunkId = 1,
        .line = 0,
        .col = 16,
        .lineStartPos = 0,
    });

    // `try` panic from another module.
    res = run.evalExt(.{ .silent = true, .uri = "./test/main.cy" },
        \\import a 'test_mods/init_try_error.cy'
        \\import t 'test'
        \\try t.eq(a.foo, 123)
    );
    try t.expectError(res, error.Panic);
    trace = run.getStackTrace();
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .chunkId = 1,
        .line = 0,
        .col = 16,
        .lineStartPos = 0,
    });
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
        \\foo = none
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

test "boolean" {
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

test "Statements." {
    try evalPass(.{},
        \\import t 'test'
        \\
        \\-- Expressions are allowed to wrap to the next line.
        \\if true or
        \\   true:
        \\  a = 10
        \\try t.eq(a, 10)
    );

    // Invalid single line block.
    try eval(.{ .silent = true },
        \\if true: foo = 123 foo = 234
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.ParseError,
            \\ParseError: Unexpected token: ident
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
}

test "Indentation." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("indentation_test.cy"));

    // Mixing tabs and spaces is an error.
    const res = run.evalExt(.{ .silent = true },
         \\if true:
         \\	 return 123
    );
    try t.expectError(res, error.ParseError);
    try t.eqStr(run.vm.getParserErrorMsg(), "Can not mix tabs and spaces for indentation.");

    // New block requires at least one statement.
    // const parse_res = try run.parse(
    //     \\if true:
    //     \\return 123 
    // );
    // try t.eq(parse_res.has_error, true);
    // try t.expect(std.mem.indexOf(u8, parse_res.err_msg, "Block requires at least one statement. Use the `pass` statement as a placeholder.") != null);
}

test "Integers." {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\
        \\-- Once a int requestable number constant is assigned to a local, it loses that trait and becomes a number.
        \\a = 10
        \\try t.eq(a < 4, false) -- This should generate less op. If lessInt is generated this would be `true` because the @bitCast(i32, lower a) == 0.
    );
}

test "Numbers." {
    // Unsupported integer notation.
    try eval(.{ .silent = true },
        \\a = 0z000
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.TokenError,
            \\ParseError: Unsupported integer notation: z
            \\
            \\main:1:6:
            \\a = 0z000
            \\     ^
            \\
        );
    }}.func);

    // Empty char is not allowed.
    try eval(.{ .silent = true },
        \\a = 0u''
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Invalid UTF-8 Rune.
            \\
            \\main:1:5:
            \\a = 0u''
            \\    ^
            \\
        );
    }}.func);

    // Can't use escape char other than single quote or backslash.
    try eval(.{ .silent = true },
        \\a = 0u'\a'
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.TokenError,
            \\ParseError: Expected single quote or backslash.
            \\
            \\main:1:9:
            \\a = 0u'\a'
            \\        ^
            \\
        );
    }}.func);

    // More than one rune in literal. 
    try eval(.{ .silent = true },
        \\a = 0u'🦊a'
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Invalid UTF-8 Rune.
            \\
            \\main:1:5:
            \\a = 0u'🦊a'
            \\    ^
            \\
        );
    }}.func);

    // More than one rune in literal. (Grapheme cluster)
    try eval(.{ .silent = true },
        \\a = 0u'👨‍👨‍👦‍👦'
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Invalid UTF-8 Rune.
            \\
            \\main:1:5:
            \\a = 0u'👨‍👨‍👦‍👦'
            \\    ^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("number_test.cy"));
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
    try t.eq(val.asF64toI32(), 2);
    val = try run.eval(
        \\2
        \\-- 1
    );
    try t.eq(val.asF64toI32(), 2);

    // Multiple single line comments.
    val = try run.eval(
        \\-- 1
        \\-- 2
        \\-- 3
        \\4
    );
    try t.eq(val.asF64toI32(), 4);
}

test "Escape sequence." {
    try evalPass(.{}, @embedFile("escape_seq_test.cy"));
}

test "Static ASCII strings." {
    try evalPass(.{}, @embedFile("static_astring_test.cy"));
}

test "Static UTF-8 strings." {
    try evalPass(.{}, @embedFile("static_ustring_test.cy"));
}

test "Heap ASCII String." {
    try evalPass(.{}, @embedFile("astring_test.cy"));
}

test "Heap UTF-8 String." {
    try evalPass(.{}, @embedFile("ustring_test.cy"));
}

test "Heap ASCII String Slice." {
    try evalPass(.{}, @embedFile("astring_slice_test.cy"));
}

test "Heap UTF-8 String Slice." {
    try evalPass(.{}, @embedFile("ustring_slice_test.cy"));
}

test "Heap RawString." {
    try evalPass(.{}, @embedFile("rawstring_test.cy"));
}

test "Heap RawString Slice." {
    try evalPass(.{}, @embedFile("rawstring_slice_test.cy"));
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

test "Assignment statements" {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\-- Assign to variable.
        \\a = 1
        \\a += 10
        \\try t.eq(a, 11)
        \\-- Assign to field.
        \\object S:
        \\  foo
        \\s = S{ foo: 1 }
        \\s.foo += 10
        \\try t.eq(s.foo, 11)
        \\-- Other operator assignments.
        \\a = 100
        \\a *= 2
        \\try t.eq(a, 200)
        \\a /= 4
        \\try t.eq(a, 50)
        \\a -= 1
        \\try t.eq(a, 49)
    );
}

test "Undefined variable references." {
    const run = VMrunner.create();
    defer run.destroy();

    // Reading an undefined variable assumes it's a symbol and returns a CompileError. (TODO: should be runtime error).
    var res = run.evalExt(.{ .silent = true },
        \\import t 'test'
        \\try t.eq(a, 123)
    );
    try run.expectErrorReport(res, error.CompileError,
        \\CompileError: Missing symbol: `a`
        \\
        \\main:2:10:
        \\try t.eq(a, 123)
        \\         ^
        \\
    );

    // Using an undefined variable as a callee is a runtime panic error.
    res = run.evalExt(.{ .silent = true },
        \\a()
    );
    try run.expectErrorReport(res, error.CompileError,
        \\CompileError: Can not find compatible function signature for `a() any`.
        \\`a` does not exist.
        \\
        \\main:1:1:
        \\a()
        \\^
        \\
    );
}

test "Static variable declaration." {
    const run = VMrunner.create();
    defer run.destroy();

    // Capturing a local variable in a static var initializer is not allowed.
    var res = run.evalExt(.{ .silent = true },
        \\b = 123
        \\var a: b
    );
    try t.expectError(res, error.CompileError);
    try t.eqStr(run.vm.getCompileErrorMsg(), "The declaration of static variable `a` can not reference the local variable `b`.");

    // Declaration with a circular reference.
    _ = try run.eval(
        \\import t 'test'
        \\var a: number(b) + 123
        \\var b: a
        \\try t.eq(b, none)
        \\try t.eq(a, 123)
        \\-- Reference self.
        \\var c: c 
        \\try t.eq(c, none)
    );

    // Declaration that depends on another.
    _ = try run.eval(
        \\import t 'test'
        \\var a: 123
        \\var b: a + 321
        \\var c: a + b
        \\try t.eq(a, 123) 
        \\try t.eq(b, 444) 
        \\try t.eq(c, 567) 
    );

    // Depends on and declared before another.
    _ = try run.eval(
        \\import t 'test'
        \\var c: a + b
        \\var b: a + 321
        \\var a: 123
        \\try t.eq(a, 123) 
        \\try t.eq(b, 444) 
        \\try t.eq(c, 567) 
    );

    // Declaration for an existing alias symbol.
    res = run.evalExt(.{ .silent = true },
        \\var print: 123
    );
    try t.expectError(res, error.CompileError);
    const err = try run.vm.allocLastUserCompileError();
    try t.eqStrFree(t.alloc, err,
        \\CompileError: The symbol `print` was already declared.
        \\
        \\main:1:5:
        \\var print: 123
        \\    ^
        \\
    );

    _ = try run.eval(@embedFile("staticvar_decl_test.cy"));
}

test "Static variable assignment." {
    const run = VMrunner.create();
    defer run.destroy();

    // Assigning to a implicit static var alias is not allowed.
    var res = run.evalExt(.{ .silent = true },
        \\var a: 1
        \\func foo():
        \\  -- Implicitly references static var.
        \\  print a
        \\  -- Attempting to assign.
        \\  a = 3
    );
    try run.expectErrorReport(res, error.CompileError, 
        \\CompileError: `a` already references a static variable. The variable must be declared with `static` before assigning to it.
        \\
        \\main:6:7:
        \\  a = 3
        \\      ^
        \\
    );

    run.deinit();

    _ = try evalPass(.{}, @embedFile("staticvar_assign_test.cy"));
}

test "Local variable declaration." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("localvar_decl_test.cy"));
}

test "Local variable assignment." {
    try evalPass(.{}, @embedFile("localvar_assign_test.cy"));

    // Initializing an object in a branch will auto generate initializers at the start of
    // the function. This test sets freed object values along the undefined stack space.
    // If the initializers were generated, the release on `a` would succeed.
    // If not `a` would refer to a freed object value and fail the release op.
    try evalPass(.{
        .preEval = struct {
            fn func(run: *VMrunner) void {
                run.vm.fillUndefinedStackSpace(cy.Value.initPtr(null));
            }
        }.func,
    },
        \\object S:
        \\  value
        \\if false:
        \\  a = S{ value: 123 }
    );

    // Same test in method scope.
    try evalPass(.{
        .preEval = struct {
            fn func(run: *VMrunner) void {
                run.vm.fillUndefinedStackSpace(cy.Value.initPtr(null));
            }
        }.func,
    },
        \\object S:
        \\  value
        \\  func foo(self):
        \\    if false:
        \\      a = S{ value: 123 }
        \\s = S{ value: 234 }
        \\s.foo()
    );
}

test "if expression" {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\foo = true
        \\if foo then 123 else 456
    );
    try t.eq(val.asF64toI32(), 123);

    val = try run.eval(
        \\foo = false
        \\if foo then 123 else 456
    );
    try t.eq(val.asF64toI32(), 456);

    // Types are merged.
    _ = try run.eval(
        \\import t 'test'
        \\a = if false then 123 else '{123}456'
        \\try t.eq(a, '123456')
        \\-- `a` should be released since else returns a heap string.
    );
}

test "Return statement." {
    const run = VMrunner.create();
    defer run.destroy();

    // If/else.
    var val = try run.eval(
        \\foo = true
        \\if foo:
        \\  return 123
    );
    try t.eq(val.asF64toI32(), 123);

    val = try run.eval(
        \\foo = false
        \\if foo:
        \\  return 123
        \\else:
        \\  return 456
    );
    try t.eq(val.asF64toI32(), 456);

    // else if condition.
    val = try run.eval(
        \\if false:
        \\  return 456
        \\else true:
        \\  return 123
        \\else:
        \\  return 456
    );
    try t.eq(val.asF64toI32(), 123);

    // return multi-line lambda
    _ = try run.eval(
        \\import t 'test'
        \\func foo():
        \\  return func():
        \\    return 123
        \\try t.eq(foo()(), 123)
    );
}

test "Match statement." {
    try evalPass(.{}, @embedFile("match_test.cy"));
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

test "For optional." {
    try evalPass(.{}, @embedFile("for_opt_test.cy"));
}

test "For iterator." {
    // Iterable does not have iterator().
    try eval(.{ .silent = true },
        \\for 123 each i:
        \\  print i
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: `iterator` is either missing in `number` or the call signature: iterator(self, 0 args) is unsupported.
            \\
            \\main:1:5 main:
            \\for 123 each i:
            \\    ^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("for_iter_test.cy"));
}

test "For loop over range." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("for_range_test.cy"));

    // Custom step.
    // val = try run.eval(
    //     \\sum = 0
    //     \\for 0..10, 2 each i:
    //     \\  sum += i
    //     \\sum
    // );
    // try t.eq(val.asF64toI32(), 20);

    // Custom step variable.
    // val = try run.eval(
    //     \\iters = 0
    //     \\step = 3
    //     \\for 0..10 each i += step:
    //     \\   iters += 1
    //     \\for 0..10 each i += step:
    //     \\   iters += 1
    //     \\iters
    // );
    // try t.eq(val.asF64toI32(), 8);
    // run.deinitValue(val);
}

test "Native function call." {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\list = []
        \\for 0..10 each i:
        \\   list.append(i)
        \\list[9]
    );
    try t.eq(val.asF64toI32(), 9);
}

test "Closures." {
    const run = VMrunner.create();
    defer run.destroy();

    // Assigning to a implicitly captured var is not allowed.
    var res = run.evalExt(.{ .silent = true },
        \\a = 1
        \\foo = func():
        \\  -- Implicitly captured for read.
        \\  print a
        \\  -- Attempting to assign.
        \\  a = 3
    );
    try run.expectErrorReport(res, error.CompileError, 
        \\CompileError: `a` already references a captured variable. The variable must be declared with `capture` before assigning to it.
        \\
        \\main:6:7:
        \\  a = 3
        \\      ^
        \\
    );

    run.deinit();

    try evalPass(.{}, @embedFile("closure_test.cy"));
}

test "Function recursion." {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\func foo(n):
        \\  if n is 0:
        \\    return 0
        \\  return n + foo(n-1)
        \\foo(10)
    );
    try t.eq(val.asF64toI32(), 55);

    // Recursion with long lived object.
    val = try run.eval(
        \\object S:
        \\  n
        \\func foo(o):
        \\  if o.n is 0:
        \\    return 0
        \\  n = o.n
        \\  o.n = o.n - 1
        \\  return n + foo(o)
        \\foo(S{ n: 10 })
    );
    try t.eq(val.asF64toI32(), 55);

    // Recursion with new objects.
    val = try run.eval(
        \\object S:
        \\  n
        \\func foo(o):
        \\  if o.n is 0:
        \\    return 0
        \\  return o.n + foo(S{ n: o.n - 1 })
        \\foo(S{ n: 10 })
    );
    try t.eq(val.asF64toI32(), 55);
}

test "Function overloading." {
    try evalPass(.{},
        \\import t 'test'
        \\func foo():
        \\    return 2 + 2
        \\func foo(n):
        \\    return 2 + n
        \\func foo(n, m):
        \\    return n * m
        \\try t.eq(foo(), 4)
        \\try t.eq(foo(10), 12)
        \\try t.eq(foo(3, 5), 15)
    );
}

test "Static functions." {

    // Call with missing func sym.
    try eval(.{ .silent = true },
        \\foo(1)
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not find compatible function signature for `foo(number) any`.
            \\`foo` does not exist.
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
            \\CompileError: Can not find compatible function signature for `foo(number) any`.
            \\Only `func foo() any` exists for the symbol `foo`.
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
            \\CompileError: Can not find compatible function signature for `foo(number, number) any`.
            \\There are multiple overloaded functions named `foo`.
            \\
            \\main:5:1:
            \\foo(1, 2)
            \\^
            \\
        );
    }}.func);

    // Declaration initializer has a reference to a local.
    try eval(.{ .silent = true },
        \\a = 123
        \\func foo() = a
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: The declaration initializer of static function `foo` can not reference the local variable `a`.
            \\
            \\main:2:1:
            \\func foo() = a
            \\^
            \\
        );
    }}.func);

    // Declaration initializer has a function value with a different signature.
    try eval(.{ .silent = true },
        \\func foo() = number
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Assigning to static function with a different function signature.
            \\
            \\main:1:14 main:
            \\func foo() = number
            \\             ^
            \\
        );
    }}.func);

    // Declaration initializer for a function already imported from core.
    try eval(.{ .silent = true },
        \\func print(val) = number
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: The symbol `print` was already declared.
            \\
            \\main:1:6:
            \\func print(val) = number
            \\     ^
            \\
        );
    }}.func);

    // Capture local from static function is not allowed.
    try eval(.{ .silent = true },
        \\a = 123
        \\func foo():
        \\  return a
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not capture the local variable `a` from static function `foo`.
            \\Only lambdas (function values) can capture local variables.
            \\
            \\main:3:10:
            \\  return a
            \\         ^
            \\
        );
    }}.func);

    // Explicit capture from static function is not allowed.
    try eval(.{ .silent = true },
        \\a = 123
        \\func foo():
        \\  capture a = 234
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.CompileError,
            \\CompileError: Can not capture the local variable `a` from static function `foo`.
            \\Only lambdas (function values) can capture local variables.
            \\
            \\main:3:15:
            \\  capture a = 234
            \\              ^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("static_func_test.cy"));
}

test "Lambdas." {
    _ = try evalPass(.{}, @embedFile("lambda_test.cy"));
//     // Lambda assign declaration.
//     val = try run.eval(
//         \\foo = {}
//         \\func foo.bar():
//         \\  return 2
//         \\foo.bar()
//     );
//     try t.eq(val.asF64toI32(), 2);
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
    const run = VMrunner.create();
    defer run.destroy();

    // One level of access from parent.
    var val = try run.eval(
        \\map = { a: 5 }
        \\map.a
    );
    try t.eq(val.asF64toI32(), 5);

    // Multiple levels of access from parent.
    val = try run.eval(
        \\map = { a: { b: 5 } }
        \\map.a.b
    );
    try t.eq(val.asF64toI32(), 5);
}

test "Math" {
    const run = VMrunner.create();
    defer run.destroy();

    // Infinity.
    var val = try run.eval(
        \\1 / 0
    );
    try run.valueIsF64(val, std.math.inf_f64);

    // NaN.
    val = try run.eval(
        \\0 * (1 / 0)
    );
    try t.expect(val.isNumber());
    try t.expect(std.math.isNan(val.asF64()));

    // Math module.
    _ = try run.eval(@embedFile("math_test.cy"));
}

test "Bitwise operators." {
    try evalPass(.{}, @embedFile("bitwise_op_test.cy"));
}

test "Arithmetic operators." {
    // Can only add numbers.
    try eval(.{ .silent = true },
        \\a = 'foo' + 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Cannot convert `string` to number.
            \\
            \\main:1:6 main:
            \\a = 'foo' + 123
            \\     ^
            \\
        );
    }}.func);

    // Can only subtract numbers.
    try eval(.{ .silent = true },
        \\a = 'foo' - 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Cannot convert `string` to number.
            \\
            \\main:1:6 main:
            \\a = 'foo' - 123
            \\     ^
            \\
        );
    }}.func);

    // Can only multiply numbers.
    try eval(.{ .silent = true },
        \\a = 'foo' * 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Cannot convert `string` to number.
            \\
            \\main:1:6 main:
            \\a = 'foo' * 123
            \\     ^
            \\
        );
    }}.func);

    // Can only divide numbers.
    try eval(.{ .silent = true },
        \\a = 'foo' / 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Cannot convert `string` to number.
            \\
            \\main:1:6 main:
            \\a = 'foo' / 123
            \\     ^
            \\
        );
    }}.func);

    // Can only mod numbers.
    try eval(.{ .silent = true },
        \\a = 'foo' % 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Cannot convert `string` to number.
            \\
            \\main:1:6 main:
            \\a = 'foo' % 123
            \\     ^
            \\
        );
    }}.func);

    // Can only pow numbers.
    try eval(.{ .silent = true },
        \\a = 'foo' ^ 123
    , struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, error.Panic,
            \\panic: Cannot convert `string` to number.
            \\
            \\main:1:6 main:
            \\a = 'foo' ^ 123
            \\     ^
            \\
        );
    }}.func);

    try evalPass(.{}, @embedFile("arithmetic_op_test.cy"));
}

const VMrunner = struct {
    vm: *cy.UserVM,
    trace: cy.TraceInfo,

    fn create() *VMrunner {
        var new = t.alloc.create(VMrunner) catch fatal();
        new.init() catch fatal();
        return new;
    }

    fn destroy(self: *VMrunner) void {
        self.deinit();
        t.alloc.destroy(self);
    }

    fn init(self: *VMrunner) !void {
        self.* = .{
            .vm = cy.getUserVM(),
            .trace = undefined,
        };
        self.vm.init(t.alloc) catch fatal();
        self.vm.setTrace(&self.trace);
    }

    fn deinit(self: *VMrunner) void {
        self.vm.deinit();
        const rc = self.vm.getGlobalRC();
        if (rc != 0) {
            stdx.panicFmt("{} unreleased refcount from previous eval", .{rc});
        }
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

    fn assertPanicMsg(self: *VMrunner, exp: []const u8) !void {
        const msg = try self.allocPanicMsg();
        defer self.vm.allocator().free(msg);
        try t.eqStr(msg, exp);
    }

    fn allocPanicMsg(self: *VMrunner) ![]const u8 {
        return self.vm.allocPanicMsg();
    }

    fn expectErrorReport(self: *VMrunner, val: anytype, expErr: UserError, expReport: []const u8) !void {
        try t.expectError(val, expErr);
        const report = try self.vm.allocLastErrorReport();
        try eqUserError(t.alloc, report, expReport);
    }

    fn evalExt(self: *VMrunner, config: Config, src: []const u8) !cy.Value {
        if (config.silent) {
            cy.silentError = true;
        }
        defer {
            if (config.silent) {
                cy.silentError = false;
            }
        }
        try self.resetEnv();    
        return self.vm.eval(config.uri, src, .{ .singleRun = false, .enableFileModules = config.enableFileModules }) catch |err| {
            switch (err) {
                error.Panic,
                error.TokenError,
                error.ParseError,
                error.CompileError => {
                    if (!cy.silentError) {
                        const report = try self.vm.allocLastErrorReport();
                        defer t.alloc.free(report);
                        std.debug.print("{s}", .{report});
                    }
                },
                else => {},
            }
            return err;
        };
    }

    fn evalExtNoReset(self: *VMrunner, config: Config, src: []const u8) !cy.Value {
        if (config.silent) {
            cy.silentError = true;
        }
        defer {
            if (config.silent) {
                cy.silentError = false;
            }
        }
        return self.vm.eval(config.uri, src, .{ .singleRun = false, .enableFileModules = config.enableFileModules, .reload = true }) catch |err| {
            switch (err) {
                error.Panic,
                error.TokenError,
                error.ParseError,
                error.CompileError => {
                    if (!cy.silentError) {
                        const report = try self.vm.allocLastErrorReport();
                        defer t.alloc.free(report);
                        std.debug.print("{s}", .{report});
                    }
                },
                else => {},
            }
            return err;
        };
    }

    fn eval(self: *VMrunner, src: []const u8) !cy.Value {
        return self.evalExt(.{ .uri = "main" }, src);
    }

    fn resetEnv(self: *VMrunner) !void {
        self.vm.deinit();
        const rc = self.vm.getGlobalRC();
        if (rc != 0) {
            log.debug("{} unreleased refcount from previous eval", .{rc});
            return error.UnreleasedObjects;
        }
        try self.vm.init(t.alloc);
        self.vm.setTrace(&self.trace);
    }

    fn evalNoReset(self: *VMrunner, src: []const u8) !cy.Value {
        return self.evalExtNoReset(.{ .uri = "main" }, src);
    }

    pub fn valueIsF64(self: *VMrunner, act: cy.Value, exp: f64) !void {
        _ = self;
        if (act.isNumber()) {
            try t.eq(act.asF64(), exp);
            return;
        }
        return error.NotF64;
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

    pub fn assertValueString(self: *VMrunner, val: cy.Value) ![]const u8 {
        if (val.isString()) {
            return self.vm.valueAsString(val);
        } else {
            return error.NotAString;
        }
    }

    pub fn valueToIntSlice(self: *VMrunner, val: cy.Value) ![]const i32 {
        _ = self;
        const obj = stdx.ptrAlignCast(*cy.HeapObject, val.asPointer());
        const list = stdx.ptrAlignCast(*cy.List(cy.Value), &obj.list.list);
        const dupe = try t.alloc.alloc(i32, list.len);
        for (list.items(), 0..) |it, i| {
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

const Config = struct {
    uri: []const u8 = "main",

    /// Don't print panic errors.
    silent: bool = false,

    enableFileModules: bool = false,

    checkGlobalRc: bool = true,

    preEval: ?*const fn (run: *VMrunner) void = null,

    debug: bool = false,

    fn withSilent(self: Config) Config {
        var new = self;
        new.silent = true;
        return new;
    }

    fn withDebug(self: Config) Config {
        var new = self;
        new.debug = true;
        return new;
    }

    fn initFileModules(uri: []const u8) Config {
        return .{
            .enableFileModules = true,
            .uri = uri,
        };
    }
};

/// relPath does not have to physically exist.
fn eqUserError(alloc: std.mem.Allocator, act: []const u8, expTmpl: []const u8) !void {
    defer alloc.free(act);
    var exp: std.ArrayListUnmanaged(u8) = .{};
    defer exp.deinit(alloc);
    var pos: usize = 0;
    while (true) {
        if (std.mem.indexOfPos(u8, expTmpl, pos, "@AbsPath(")) |idx| {
            if (std.mem.indexOfScalarPos(u8, expTmpl, idx, ')')) |endIdx| {
                try exp.appendSlice(alloc, expTmpl[pos..idx]);
                const basePath = try std.fs.realpathAlloc(alloc, ".");
                defer t.alloc.free(basePath);
                try exp.appendSlice(alloc, basePath);
                try exp.append(alloc, std.fs.path.sep);

                const relPathStart = exp.items.len;
                try exp.appendSlice(alloc, expTmpl[idx+9..endIdx]);
                if (builtin.os.tag == .windows) {
                    _ = std.mem.replaceScalar(u8, exp.items[relPathStart..], '/', '\\');
                }

                pos = endIdx + 1;
            }
        }
        try exp.appendSlice(alloc, expTmpl[pos..]);
        break;
    }
    try t.eqStr(act, exp.items);
}

const UserError = error{Panic, CompileError, TokenError, ParseError};

/// TODO: Refactor to use eval.
fn eval(config: Config, src: []const u8, optCb: ?*const fn (*VMrunner, anyerror!cy.Value) anyerror!void) !void {
    const run = VMrunner.create();
    defer run.destroy();

    if (config.silent) {
        cy.silentError = true;
    }
    if (config.debug) {
        cy.verbose = true;
        t.setLogLevel(.debug);
    }
    defer {
        if (config.silent) {
            cy.silentError = false;
        }
        if (config.debug) {
            cy.verbose = false;
            t.setLogLevel(.warn);
        }
    }

    if (config.preEval) |preEval| {
        preEval(run);
    }

    const res = run.vm.eval(config.uri, src, .{ 
        .singleRun = false,
        .enableFileModules = config.enableFileModules,
        .genAllDebugSyms = config.debug,
    });

    if (optCb) |cb| {
        try cb(run, res);
    }  else {
        _ = res catch |err| {
            switch (err) {
                error.Panic,
                error.TokenError,
                error.ParseError,
                error.CompileError => {
                    if (!cy.silentError) {
                        const report = try run.vm.allocLastErrorReport();
                        defer t.alloc.free(report);
                        std.debug.print("{s}", .{report});
                    }
                },
                else => {},
            }
            return err;
        };
    }

    // Deinit, so global objects from builtins are released.
    run.vm.internal().compiler.deinitRtObjects();
    run.vm.internal().deinitRtObjects();

    if (config.checkGlobalRc) {
        try cy.arc.checkGlobalRC(run.vm.internal());
    }
}

fn evalPass(config: Config, src: []const u8) !void {
    return eval(config, src, null);
}

const EvalResult = anyerror!cy.Value;