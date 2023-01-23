const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const t = stdx.testing;

const vm_ = @import("../src/vm.zig");
const cy = @import("../src/cyber.zig");
const bindings = @import("../src/builtins/bindings.zig");
const log = stdx.log.scoped(.behavior_test);

test "compile time" {
    const run = VMrunner.create();
    defer run.destroy();

    // compt is valid syntax
    cy.silentInternal = true;
    defer cy.silentInternal = false;
    _ = try run.eval(
        \\func foo(a):
        \\  compt compilerDumpLocals()
    );
}

test "core module" {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("core_test.cy"));
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

    _ = try run.eval(@embedFile("os_test.cy"));
}

test "Fibers" {
    const run = VMrunner.create();
    defer run.destroy();

    // Init fiber without starting.
    var val = try run.eval(
        \\func foo(list):
        \\  coyield
        \\  list.append(123)
        \\list = []
        \\f = coinit foo(list)
        \\list.len()
    );
    try run.valueIsI32(val, 0);

    // Start fiber with yield at start.
    val = try run.eval(
        \\func foo(list):
        \\  coyield
        \\  list.append(123)
        \\list = []
        \\f = coinit foo(list)
        \\coresume f
        \\list.len()
    );
    try run.valueIsI32(val, 0);

    // Start fiber without yield.
    val = try run.eval(
        \\func foo(list):
        \\  list.append(123)
        \\list = []
        \\f = coinit foo(list)
        \\coresume f
        \\list[0]
    );
    try run.valueIsI32(val, 123);

    // coresume returns final value.
    val = try run.eval(
        \\import t 'test'
        \\func foo(list):
        \\  list.append(123)
        \\  return list[0]
        \\list = []
        \\f = coinit foo(list)
        \\try t.eq(coresume f, 123)
    );

    // Start fiber with yield in nested function.
    val = try run.eval(
        \\func bar():
        \\  alist = [] --This should be released after fiber is freed.
        \\  coyield
        \\func foo(list):
        \\  bar()
        \\  list.append(123)
        \\list = []
        \\f = coinit foo(list)
        \\coresume f
        \\list.len()
    );
    try run.valueIsI32(val, 0);

    // Continue to resume fiber.
    val = try run.eval(
        \\func foo(list):
        \\  list.append(123)
        \\  coyield
        \\  list.append(234)
        \\list = []
        \\f = coinit foo(list)
        \\coresume f
        \\coresume f
        \\list.len()
    );
    try run.valueIsI32(val, 2);

    // Fiber status.
    _ = try run.eval(
        \\import t 'test'
        \\func foo():
        \\  coyield
        \\f = coinit foo()
        \\try t.eq(f.status(), #paused)
        \\coresume f
        \\try t.eq(f.status(), #paused)
        \\coresume f
        \\try t.eq(f.status(), #done)
    );

    // Resuming after fiber is done is a nop.
    _ = try run.eval(
        \\import t 'test'
        \\func foo():
        \\  coyield
        \\f = coinit foo()
        \\coresume f
        \\coresume f
        \\try t.eq(f.status(), #done)
        \\coresume f
        \\try t.eq(f.status(), #done)
    );

    // Grow fiber stack.
    _ = try run.eval(
        \\import t 'test'
        \\func sum(n):
        \\  if n == 0:
        \\    return 0
        \\  return n + sum(n - 1)
        \\f = coinit sum(20)
        \\res = coresume f
        \\try t.eq(res, 210)
    );
}

test "Errors." {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\err = error(#FileNotFound)
        \\try t.eq(valtag(err), #error)
        \\try t.eq(err, error(#FileNotFound))
    );
}

test "FFI." {
    const run = VMrunner.create();
    defer run.destroy();

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
        export fn testF32(n: f32) f32 {
            return n;
        }
        export fn testF64(n: f64) f64 {
            return n;
        }
        export fn testCharPtrZ(ptr: [*:0]const u8) [*:0]const u8 {
            const S = struct {
                var buf: [10]u8 = undefined;
            };
            const slice = std.mem.span(ptr);
            std.mem.copy(u8, &S.buf, slice);
            S.buf[slice.len] = 0;
            return @ptrCast([*:0]const u8, &S.buf);
        }
        export fn testPtr(ptr: *anyopaque) *anyopaque {
            return ptr;
        }
        export fn testVoid() void {
        }
        export fn testBool(b: bool) bool {
            return b;
        }
    };
    _ = S;

    _ = try run.eval(@embedFile("ffi_test.cy"));
}

test "Tag types." {
    const run = VMrunner.create();
    defer run.destroy();

    // TagType to number.
    var val = try run.eval(
        \\tagtype Animal:
        \\  Bear
        \\  Tiger
        \\n = Animal#Tiger
        \\number(n)
    );
    try t.eq(val.asF64toI32(), 1);

    // Using TagType declared afterwards.
    val = try run.eval(
        \\n = Animal#Tiger
        \\tagtype Animal:
        \\  Bear
        \\  Tiger
        \\number(n)
    );
    try t.eq(val.asF64toI32(), 1);

    // Reassign using tag literal.
    val = try run.eval(
        \\tagtype Animal:
        \\  Bear
        \\  Tiger
        \\  Dragon
        \\n = Animal#Tiger
        \\n = #Dragon
        \\number(n)
    );
    try t.eq(val.asF64toI32(), 2);

    // Tag literal.
    val = try run.eval(
        \\n = #Tiger
        \\number(n)
    );
    const id = try vm_.gvm.ensureTagLitSym("Tiger");
    try t.eq(val.asF64toI32(), @intCast(i32, id));
}

test "test module" {
    const run = VMrunner.create();
    defer run.destroy();

    const res = run.evalSilent(
        \\import t 'test'
        \\try t.eq(123, 234)
    );
    try t.expectError(res, error.Panic);

    var val = try run.eval(
        \\import t 'test'
        \\t.eq(123, 123)
        \\t.eq(1.2345, 1.2345)
    );
    try t.expect(val.isTrue());
}

test "Objects." {
    const run = VMrunner.create();
    defer run.destroy();
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

    var res = run.evalSilent(
        \\a = error(#boom)
        \\must(a)
    );
    try t.expectError(res, error.Panic);
    var trace = run.getStackTrace();
    try t.eqStr(run.getPanicMsg(), "error#boom");
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .uri = "main",
        .line = 1,
        .col = 0,
        .lineStartPos = 17,
    });
}

test "panic()" {
    const run = VMrunner.create();
    defer run.destroy();

    var res = run.evalSilent(
        \\a = 123
        \\1 + panic(#boom)
    );
    try t.expectError(res, error.Panic);
    var trace = run.getStackTrace();
    try t.eqStr(run.getPanicMsg(), "#boom");
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .uri = "main",
        .line = 1,
        .col = 4,
        .lineStartPos = 8,
    });
}

test "Stack trace unwinding." {
    const run = VMrunner.create();
    defer run.destroy();

    var res = run.evalSilent(
        \\a = 123
        \\1 + a.foo
    );
    try t.expectError(res, error.Panic);
    var trace = run.getStackTrace();
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .uri = "main",
        .line = 1,
        .col = 4,
        .lineStartPos = 8,
    });

    // Function stack trace.
    res = run.evalSilent(
        \\func foo():
        \\  a = 123
        \\  return 1 + a.foo
        \\foo()
    );
    try t.expectError(res, error.Panic);
    trace = run.getStackTrace();
    try t.eq(trace.frames.len, 2);
    try eqStackFrame(trace.frames[0], .{
        .name = "foo",
        .uri = "main",
        .line = 2,
        .col = 13,
        .lineStartPos = 22,
    });
    try eqStackFrame(trace.frames[1], .{
        .name = "main",
        .uri = "main",
        .line = 3,
        .col = 0,
        .lineStartPos = 41,
    });
}

fn eqStackFrame(act: cy.StackFrame, exp: cy.StackFrame) !void {
    try t.eqStr(act.name, exp.name);
    try t.eqStr(act.uri, exp.uri);
    try t.eq(act.line, exp.line);
    try t.eq(act.col, exp.col);
    try t.eq(act.lineStartPos, exp.lineStartPos);
}

test "Optionals" {
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
    const run = VMrunner.create();
    defer run.destroy();

    _= try run.eval(
        \\import t 'test'
        \\try t.eq(1 < 2, true)
        \\try t.eq(2 < 1, false)
        \\try t.eq(1 > 2, false)
        \\try t.eq(2 > 1, true)
        \\try t.eq(1 <= 2, true)
        \\try t.eq(2 <= 2, true)
        \\try t.eq(3 <= 2, false)
        \\try t.eq(1 >= 2, false)
        \\try t.eq(2 >= 2, true)
        \\try t.eq(3 >= 2, true)
        \\
    );
}

test "Compare equals." {
    const run = VMrunner.create();
    defer run.destroy();
    _= try run.eval(@embedFile("compare_eq_test.cy"));
}

test "Compare not equals." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("compare_neq_test.cy"));
}

test "Truthy evaluation." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("truthy_test.cy"));
}

test "Logic operators" {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("logic_op_test.cy"));
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
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\
        \\-- Expressions are allowed to wrap to the next line.
        \\if true or
        \\   true:
        \\  a = 10
        \\try t.eq(a, 10)
    );
}

test "Indentation." {
    const run = VMrunner.create();
    defer run.destroy();

    // Detect end of block.
    var val = try run.eval(
        \\func foo():
        \\  return 123
        \\foo()
    );
    try t.eq(val.asF64toI32(), 123);

    // Comment before end of block.
    val = try run.eval(
        \\func foo():
        \\  return 123
        \\  -- Comment.
        \\foo()
    );
    try t.eq(val.asF64toI32(), 123);

    // Indented comment at the end of the source.
    _ = try run.eval(
        \\func foo():
        \\  return 123
        \\     -- Comment.
    );

    // New block requires at least one statement.
    // const parse_res = try run.parse(
    //     \\if true:
    //     \\return 123 
    // );
    // try t.eq(parse_res.has_error, true);
    // try t.expect(std.mem.indexOf(u8, parse_res.err_msg, "Block requires at least one statement. Use the `pass` statement as a placeholder.") != null);

    // Continue from parent indentation.
    val = try run.eval(
        \\func foo():
        \\  if false:
        \\    pass
        \\  return 123 
        \\foo()
    );
    try t.eq(val.asF64toI32(), 123);

    // Continue from grand parent indentation.
    val = try run.eval(
        \\func foo():
        \\  if false:
        \\    if false:
        \\      pass
        \\  return 123 
        \\foo()
    );
    try t.eq(val.asF64toI32(), 123);
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
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\try t.eq(1, 1)
        \\try t.eq(-1, -1)
        \\try t.eq(-(-1), 1)
        \\try t.eq(1.23e2, 123)
        \\try t.eqNear(123e-2, 1.23)
        \\try t.eq(0xff, 255)
        \\try t.eq(0xFF, 255)
        \\try t.eq(0o77, 63)
        \\try t.eq(0b11, 3)
    );
}

test "Parentheses" {
    const run = VMrunner.create();
    defer run.destroy();

    // Parentheses at left of binary expression.
    var val = try run.eval(
        \\(2 + 3) * 4
    );
    try t.eq(val.asF64toI32(), 20);

    // Parentheses at right of binary expression.
    val = try run.eval(
        \\2 * (3 + 4)
    );
    try t.eq(val.asF64toI32(), 14);

    // Nested parentheses.
    val = try run.eval(
        \\2 + ((3 + 4) / 7)
    );
    try t.eq(val.asF64toI32(), 3);
}

test "Operator precedence." {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\-- Multiplication before addition.
        \\try t.eq(2 + 3 * 4, 14)
        \\-- Division before addition.
        \\try t.eq(2 + 4 / 4, 3)
        \\-- Power before addition.
        \\try t.eq(2 + 3 ^ 2, 11)
        \\-- Power before multiplication.
        \\try t.eq(2 * 3 ^ 2, 18)
        \\-- Math operator before comparison.
        \\try t.eq(5 == 2 + 3, true)
        \\try t.eq(5 == 21 % 16, true)
        \\-- Bitwise shift before bitwise or.
        \\try t.eq(1 | 1 << 1, 3)
    );

    // Variables and parenthesis.
    var val = try run.eval(
        \\time = 50
        \\minTime = 50
        \\timeRange = 100
        \\5 + 90 * (time - minTime) / timeRange
    );
    try t.eq(val.asF64toI32(), 5);

    // Left recursion with different operators.
    val = try run.eval(
        \\5 + 2 * 3 / 3 
    );
    try t.eq(val.asF64toI32(), 7);
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

test "Static ASCII strings." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("static_astring_test.cy"));
}

test "Static UTF-8 strings." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("static_ustring_test.cy"));
}

test "Heap ASCII String." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("astring_test.cy"));
}

test "Heap UTF-8 String." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("ustring_test.cy"));
}

test "Heap ASCII String Slice." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("astring_slice_test.cy"));
}

test "Heap UTF-8 String Slice." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("ustring_slice_test.cy"));
}

test "Heap RawString." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("rawstring_test.cy"));
}

test "Heap RawString Slice." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("rawstring_slice_test.cy"));
}

test "toString." {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\try t.eq(string(123.4), '123.4000000000')
        \\try t.eq(string(int(123)), '123')
        \\try t.eq(string(error(#foo)), 'error#foo')
        \\try t.eq(string(#foo), '#foo')
    );
}

test "String interpolation." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("string_interpolation_test.cy"));
}

test "Lists" {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\
        \\-- Index access.
        \\a = [1, 2, 3]
        \\try t.eq(a[0], 1)
        \\
        \\-- Reverse index access.
        \\try t.eq(a[-1], 3)
        \\
        \\-- Set index
        \\a = []
        \\a.resize(3)
        \\a[2] = 3
        \\try t.eq(a[2], 3)
        \\
        \\-- append()
        \\a = []
        \\a.append(1)
        \\try t.eq(a.len(), 1)
        \\try t.eq(a[0], 1)
        \\
        \\-- concat()
        \\a = [1, 2, 3]
        \\a.concat([4, 5, 6])
        \\try t.eqList(a, [1, 2, 3, 4, 5, 6])
        \\
        \\-- insert() in empty
        \\a = []
        \\a.insert(0, 1)
        \\try t.eq(a[0], 1)
        \\
        \\-- insert() at start
        \\a.insert(0, 2)
        \\try t.eqList(a, [2, 1])
        \\
        \\-- insert() at end
        \\a.insert(2, 3)
        \\try t.eqList(a, [2, 1, 3])
        \\
        \\-- insert() in middle
        \\a.insert(1, 4)
        \\try t.eqList(a, [2, 4, 1, 3])
        \\
        \\-- insert() at index out of bounds.
        \\try t.eq(a.insert(-1, 123), error(#OutOfBounds))
        \\try t.eq(a.insert(100, 123), error(#OutOfBounds))
        \\
        \\-- joinString()
        \\try t.eq([].joinString(','), '')
        \\try t.eq([1].joinString(','), '1')
        \\try t.eq([1, 2, 3].joinString(','), '1,2,3')
        \\try t.eq([1, 2, 3].joinString(',').isAscii(), true)
        \\try t.eq([1, 2, 3].joinString('🦊'), '1🦊2🦊3')
        \\try t.eq([1, 2, 3].joinString('🦊').isAscii(), false)
        \\
        \\-- len()
        \\a = [1, 2, 3, 4]
        \\try t.eq(a.len(), 4)
        \\
        \\-- remove()
        \\a = [1, 2, 3]
        \\a.remove(1)
        \\try t.eq(a.len(), 2)
        \\try t.eq(a[0], 1)
        \\try t.eq(a[1], 3)
        \\
        \\-- remove() first item.
        \\a = [1, 2, 3]
        \\a.remove(0)
        \\try t.eq(a.len(), 2)
        \\try t.eq(a[0], 2)
        \\try t.eq(a[1], 3)
        \\
        \\-- remove() last item.
        \\a = [1, 2, 3]
        \\a.remove(2)
        \\try t.eq(a.len(), 2)
        \\try t.eq(a[0], 1)
        \\try t.eq(a[1], 2)
        \\
        \\-- remove() out of bounds.
        \\a = [1, 2, 3]
        \\try t.eq(a.remove(-1), error(#OutOfBounds))
        \\try t.eq(a.remove(3), error(#OutOfBounds))
        \\try t.eq(a.len(), 3)
        \\
        \\-- remove() rc item.
        \\a = [1, [123], 3]
        \\a.remove(1)
        \\try t.eq(a.len(), 2)
        \\try t.eq(a[0], 1)
        \\try t.eq(a[1], 3)
        \\
        \\-- resize()
        \\a = [1, 2, 3]
        \\a.resize(4)
        \\try t.eq(a.len(), 4)
        \\try t.eq(a[3], none)
        \\a.resize(2)
        \\try t.eq(a.len(), 2)
        \\try t.eq(a[1], 2)
        \\
        \\-- sort()
        \\a = [3, 1, 2]
        \\a.sort((a, b) => a < b)
        \\try t.eqList(a, [1, 2, 3])
        \\a = [[3], [1], [2]]
        \\a.sort((a, b) => a[0] < b[0])
        \\try t.eq(a[0][0], 1)
        \\try t.eq(a[1][0], 2)
        \\try t.eq(a[2][0], 3)
    );

    // Start to end index slice.
    var val = try run.eval(
        \\a = [1, 2, 3, 4, 5]
        \\a[1..4]
    );
    var val_slice = try run.valueToIntSlice(val);
    try t.eqSlice(i32, val_slice, &.{ 2, 3, 4 });
    run.deinitValue(val);
    t.alloc.free(val_slice);

    // Start index to end of list.
    val = try run.eval(
        \\a = [1, 2, 3, 4, 5]
        \\a[3..]
    );
    val_slice = try run.valueToIntSlice(val);
    try t.eqSlice(i32, val_slice, &.{ 4, 5 });
    run.deinitValue(val);
    t.alloc.free(val_slice);

    // Start of list to end index.
    val = try run.eval(
        \\a = [1, 2, 3, 4, 5]
        \\a[..3]
    );
    val_slice = try run.valueToIntSlice(val);
    try t.eqSlice(i32, val_slice, &.{ 1, 2, 3 });
    run.deinitValue(val);
    t.alloc.free(val_slice);

    // Iterate list
    _ = try run.eval(
        \\import t 'test'
        \\
        \\-- Iteration.
        \\let a = [1, 2, 3, 4, 5]
        \\let sum = 0
        \\for a each it:
        \\  sum += it
        \\try t.eq(sum, 15)
        \\
        \\-- Pair iteration.
        \\a = [10, 20, 30]
        \\sum = 0
        \\let idxSum = 0
        \\for a each idx, it:
        \\  sum += it
        \\  idxSum += idx
        \\try t.eq(sum, 60)
        \\try t.eq(idxSum, 3)
        \\
        \\-- Nested iteration.
        \\a = [1, 2, 3]
        \\res = 0
        \\for a each n:
        \\  innerSum = 0
        \\  for a each m:
        \\    innerSum += m
        \\  res += n * innerSum
        \\try t.eq(res, 36)
        \\
        \\-- Nested pair iteration.
        \\a = [1, 2, 3]
        \\res = 0
        \\idxRes = 0
        \\for a each i, n:
        \\  innerSum = 0
        \\  idxSum = 0
        \\  for a each j, m:
        \\    innerSum += m
        \\    idxSum += j
        \\  res += n * innerSum
        \\  idxRes += i * idxSum
        \\try t.eq(res, 36)
        \\try t.eq(idxRes, 9)
    );
}

test "Maps" {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\a = {
        \\  b: 123
        \\  'c': 234
        \\}
        \\-- Get size.
        \\try t.eq(a.size(), 2)
    );

    // Number entry.
    var val = try run.eval(
        \\a = {
        \\  b: 32
        \\}
        \\a['b']
    );
    try t.eq(val.asF64toI32(), 32);

    // Access expression.
    val = try run.eval(
        \\a = {
        \\  b: 32
        \\}
        \\a.b
    );
    try t.eq(val.asF64toI32(), 32);

    // String entry.
    val = try run.eval(
        \\a = {
        \\  b: 'hello'
        \\}
        \\a['b']
    );
    const str = try run.assertValueString(val);
    try t.eqStr(str, "hello");
    run.deinitValue(val);

    // Add to empty map.
    val = try run.eval(
        \\a = {}
        \\a[123] = 234
        \\a[123]
    );
    try t.eq(val.asF64toI32(), 234);

    // Nested list.
    val = try run.eval(
        \\a = {
        \\  b: [ 1, 2 ]
        \\}
        \\a.b[1]
    );
    try t.eq(val.asF64toI32(), 2);

    // Nested list with items separated by new line.
    val = try run.eval(
        \\a = {
        \\  b: [
        \\    1
        \\    2
        \\  ]
        \\}
        \\a.b[1]
    );
    try t.eq(val.asF64toI32(), 2);

    // Iterate maps.
    _ = try run.eval(
        \\import t 'test'
        \\
        \\-- Iterator.
        \\let m = { a: 2, b: 3, c: 4 }
        \\let sum = 0
        \\for m each v:
        \\  sum += v 
        \\try t.eq(sum, 9)
        \\
        \\-- Pair Iterator.
        \\sum = 0
        \\codeSum = 0
        \\for m each k, v:
        \\  sum += v
        \\  codeSum += asciiCode(k)
        \\try t.eq(sum, 9)
        \\try t.eq(codeSum, 294)
        \\
        \\-- Nested iteration.
        \\m = { a: 1, b: 2, c: 3 }
        \\res = 0
        \\for m each n:
        \\  innerSum = 0
        \\  for m each nn:
        \\    innerSum += nn
        \\  res += n * innerSum
        \\try t.eq(res, 36)
        \\
        \\-- Nested pair iteration.
        \\m = { a: 1, b: 2, c: 3 }
        \\res = 0
        \\codeSum = 0
        \\for m each k, n:
        \\  innerSum = 0
        \\  for m each kk, nn:
        \\    innerSum += nn
        \\    codeSum += asciiCode(kk)
        \\  res += n * innerSum
        \\  codeSum += asciiCode(k)
        \\try t.eq(res, 36)
        \\try t.eq(codeSum, 294 * 4)
        \\
        \\-- Iterate rc values.
        \\m = { a: [2], b: [3], c: [4] }
        \\sum = 0
        \\codeSum = 0
        \\for m each k, v:
        \\  sum += v[0]
        \\  codeSum += asciiCode(k)
        \\try t.eq(sum, 9)
        \\try t.eq(codeSum, 294)
    );

    // Remove from map.
    _ = try run.eval(
        \\import t 'test'
        \\let m = { a: 2, b: 3, c: 4 }
        \\m.remove('a')
        \\try t.eq(m.size(), 2)
        \\try t.eq(m['a'], none)
        \\try t.eq(m['b'], 3)
        \\try t.eq(m['c'], 4)
    );
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

    // Reading an undefined variable declares a new local variable with the `none` value.
    _ = try run.eval(
        \\import t 'test'
        \\func foo(arg):
        \\  pass
        \\foo(a)
        \\try t.eq(a, none)
    );

    // Using an undefined variable as a callee uses it as a sym so the runtime call panics.
    const res = run.evalSilent(
        \\a()
    );
    try t.expectError(res, error.Panic);
}

test "Static variable declaration." {
    const run = VMrunner.create();
    defer run.destroy();

    // Using a local variable in a static var initializer is not allowed.
    var res = run.evalSilent(
        \\let b = 123
        \\var a = b
    );
    try t.expectError(res, error.CompileError);
    try t.eqStr(run.vm.getCompileErrorMsg(), "The declaration of static variable `a` can not reference the local variable `b`.");

    // Reading from a static variable.
    _ = try run.eval(
        \\import t 'test'
        \\var a = 123
        \\let b = a
        \\try t.eq(b, 123)
    );

    // Reading from a static variable before it is declared.
    _ = try run.eval(
        \\import t 'test'
        \\let b = a
        \\try t.eq(b, 123)
        \\var a = 123
    );

    // Declaration with a circular reference.
    _ = try run.eval(
        \\import t 'test'
        \\var a = b + 123
        \\var b = a
        \\try t.eq(b, none)
        \\try t.eq(a, 123)
        \\-- Reference self.
        \\var c = c 
        \\try t.eq(c, none)
    );

    // Declaration that depends on another.
    _ = try run.eval(
        \\import t 'test'
        \\var a = 123
        \\var b = a + 321
        \\var c = a + b
        \\try t.eq(a, 123) 
        \\try t.eq(b, 444) 
        \\try t.eq(c, 567) 
    );

    // Depends on and declared before another.
    _ = try run.eval(
        \\import t 'test'
        \\var c = a + b
        \\var b = a + 321
        \\var a = 123
        \\try t.eq(a, 123) 
        \\try t.eq(b, 444) 
        \\try t.eq(c, 567) 
    );
}

test "Static variable assignment." {
    const run = VMrunner.create();
    defer run.destroy();

    // Assignment to a static variable.
    _ = try run.eval(
        \\import t 'test'
        \\var a = 123
        \\func foo():
        \\  a = 234
        \\foo()
        \\try t.eq(a, 234)
    );

    // Assignment to a static variable before it is declared.
    _ = try run.eval(
        \\import t 'test'
        \\func foo():
        \\  a = 234
        \\foo()
        \\try t.eq(a, 234)
        \\var a = 123
    );

    // Operator assignment to a static variable.
    _ = try run.eval(
        \\import t 'test'
        \\var a = 123
        \\func foo():
        \\  a += 321
        \\foo()
        \\try t.eq(a, 444)
    );
}

test "Local variable declaration." {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\
        \\-- Declare local and read it.
        \\let a = 1
        \\try t.eq(a, 1)
        \\func foo():
        \\  -- Captured `a` from main block.
        \\  try t.eq(a, 1)
        \\  a = 2
        \\try foo()
        \\
        \\-- Changed `a` from main block.
        \\try t.eq(a, 2)
        \\func bar():
        \\  -- Captured `a` from main block.
        \\  try t.eq(a, 2)
        \\  -- New `a` in `bar`.
        \\  let a = 3
        \\  try t.eq(a, 3)
        \\try bar()
        \\
        \\-- `a` from main remains the same.
        \\try t.eq(a, 2)
    );
}

test "Local variable assignment." {
    const run = VMrunner.create();
    defer run.destroy();

    // Variable assignment.
    var val = try run.eval(
        \\a = 1
        \\a
    );
    try t.eq(val.asF64toI32(), 1);

    // Overwrite existing var.
    val = try run.eval(
        \\a = 1
        \\a = 2
        \\a
    );
    try t.eq(val.asF64toI32(), 2);

    // Use existing var.
    val = try run.eval(
        \\a = 1
        \\b = a + 2
        \\b
    );
    try t.eq(val.asF64toI32(), 3);

    // Using a variable that was conditionally assigned.
    val = try run.eval(
        \\if true:
        \\  a = 1
        \\a
    );
    try t.eq(val.asF64toI32(), 1);

    // Using a variable that was conditionally not assigned.
    val = try run.eval(
        \\if false:
        \\  a = 1
        \\a
    );
    try t.eq(val.isNone(), true);

    // Using a variable that was assigned in a loop.
    val = try run.eval(
        \\for 2..3 each i:
        \\  a = i
        \\a
    );
    try t.eq(val.asF64toI32(), 2);

    // Using a variable that was not assigned in a loop.
    val = try run.eval(
        \\for 2..2 each i:
        \\  a = i
        \\a
    );
    try t.eq(val.isNone(), true);

    // Using a variable that was conditionally assigned in a function.
    val = try run.eval(
        \\func foo():
        \\  if true:
        \\    a = 1
        \\  return a
        \\foo()
    );
    try t.eq(val.asF64toI32(), 1);

    // Using a variable that was conditionally not assigned in a function.
    val = try run.eval(
        \\func foo():
        \\  if false:
        \\    a = 1
        \\  return a
        \\foo()
    );
    try t.eq(val.isNone(), true);

    // Initializing an object in a branch will auto generate initializers at the start of
    // the function. This test sets freed object values along the undefined stack space.
    // If the initializers were generated, the release on `a` would succeed.
    // If not `a` would refer to a freed object value and fail the release op.
    try run.resetEnv();
    run.vm.fillUndefinedStackSpace(cy.Value.initPtr(null));
    val = try run.evalNoReset(
        \\object S:
        \\  value
        \\if false:
        \\  a = S{ value: 123 }
    );

    // Same test in method scope.
    try run.resetEnv();
    run.vm.fillUndefinedStackSpace(cy.Value.initPtr(null));
    val = try run.evalNoReset(
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
}

test "if statement" {
    const run = VMrunner.create();
    defer run.destroy();

    // If/else.
    var val = try run.eval(
        \\if true:
        \\  foo = 123
        \\else:
        \\  foo = 456
        \\foo
    );
    try t.eq(val.asF64toI32(), 123);

    val = try run.eval(
        \\if false:
        \\  foo = 123
        \\else:
        \\  foo = 456
        \\foo
    );
    try t.eq(val.asF64toI32(), 456);

    // else if condition.
    val = try run.eval(
        \\if false:
        \\  foo = 456
        \\else true:
        \\  foo = 123
        \\else:
        \\  foo = 456
        \\foo
    );
    try t.eq(val.asF64toI32(), 123);
}

test "Infinite while loop." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("while_inf_test.cy"));
}

test "While conditional." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("while_cond_test.cy"));
}

test "For optional." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("for_opt_test.cy"));
}

test "For iterator." {
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("for_iter_test.cy"));
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

test "Static closures." {
    const run = VMrunner.create();
    defer run.destroy();

    // Closure read over number in main block.
    _ = try run.eval(
        \\import t 'test'
        \\a = 123
        \\func foo():
        \\  return a
        \\try t.eq(foo(), 123)
    );

    // Closure write over number in main block.
    var val = try run.eval(
        \\a = 123
        \\func foo():
        \\  a = 234
        \\foo()
        \\a
    );
    try run.valueIsI32(val, 234);

    // Closure read then write over number in main block.
    _ = try run.eval(
        \\import t 'test'
        \\a = 123
        \\func foo():
        \\  b = a
        \\  try t.eq(b, 123)
        \\  a = 234
        \\try foo()
        \\try t.eq(a, 234)
    );

    // Closure add assign over number in main block.
    val = try run.eval(
        \\a = 123
        \\func foo():
        \\  a += 1
        \\foo()
        \\a
    );
    try run.valueIsI32(val, 124);

    // Closure called before declaration.
    _ = try run.eval(
        \\import t 'test'
        \\a = 123
        \\try t.eq(foo(), 123)
        \\func foo():
        \\  return a
    );

    // Closure called before declaration and captured var.
    _ = try run.eval(
        \\import t 'test'
        \\try t.eq(foo(), none)
        \\a = 123
        \\func foo():
        \\  return a
    );

    // Closure with more than 3 captured vars forces allocation outside of object pool.
    _ = try run.eval(
        \\import t 'test'
        \\a = 123
        \\b = 234
        \\c = 345
        \\d = 456
        \\func foo():
        \\  return a + b + c + d
        \\try t.eq(foo(), 1158)
    );
}

test "Value closures." {
    const run = VMrunner.create();
    defer run.destroy();

    // Closure read over number in main block.
    var val = try run.eval(
        \\a = 123
        \\foo = () => a
        \\foo()
    );
    try run.valueIsI32(val, 123);

    // Closure write over number in main block.
    val = try run.eval(
        \\a = 123
        \\foo = func():
        \\  a = 234
        \\foo()
        \\a
    );
    try run.valueIsI32(val, 234);

    // Closure over local number in function.
    val = try run.eval(
        \\func foo():
        \\  a = 123
        \\  return () => a
        \\fn = foo()
        \\fn()
    );
    try run.valueIsI32(val, 123);

    // Closure over local number in function using a param.
    val = try run.eval(
        \\func foo():
        \\  a = 123
        \\  return b => a + b
        \\fn = foo()
        \\fn(1)
    );
    try run.valueIsI32(val, 124);

    // Closure over local number in function using a param in parentheses.
    val = try run.eval(
        \\func foo():
        \\  a = 123
        \\  return (b) => a + b
        \\fn = foo()
        \\fn(1)
    );
    try run.valueIsI32(val, 124);

    // Closure over local number in function using a multiple params.
    val = try run.eval(
        \\func foo():
        \\  a = 123
        \\  return (b, c) => a + b + c
        \\fn = foo()
        \\fn(1, 2)
    );
    try run.valueIsI32(val, 126);

    // Closure over local retained object in function.
    val = try run.eval(
        \\func foo():
        \\  a = [ 123 ]
        \\  return () => a[0]
        \\fn = foo()
        \\fn()
    );
    try run.valueIsI32(val, 123);

    // Closure with more than 3 captured vars forces allocation outside of object pool.
    _ = try run.eval(
        \\import t 'test'
        \\a = 123
        \\b = 234
        \\c = 345
        \\d = 456
        \\foo = () => a + b + c + d
        \\try t.eq(foo(), 1158)
    );
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
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
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
    const run = VMrunner.create();
    defer run.destroy();
    _ = try run.eval(@embedFile("static_func_test.cy"));
}

test "Lambdas." {
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\
        \\-- No params.
        \\foo = () => 2 + 2
        \\try t.eq(foo(), 4)
        \\
        \\-- One param.
        \\foo = a => a + 1
        \\try t.eq(foo(10), 11)
        \\
        \\-- Multiple params.
        \\foo = (bar, inc) => bar + inc
        \\try t.eq(foo(20, 10), 30)
        \\
        \\-- Pass lambda as arg.
        \\func call(f):
        \\  return f(14)
        \\try t.eq(call(a => a + 1), 15)
        \\
        \\-- Using parentheses.
        \\m = { a: () => 4 }
        \\try t.eq((m.a)(), 4)
        \\
        // // \\-- Invoking lambda temp.
        // // \\try t.eq((a => a + 1)(14), 15)
    );
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
    const run = VMrunner.create();
    defer run.destroy();

    _ = try run.eval(
        \\import t 'test'
        \\-- Bitwise and
        \\try t.eq(4 & 2, 0)
        \\try t.eq(4 & 4, 4)
        \\try t.eq(7 & 2, 2)
        \\-- Bitwise or
        \\try t.eq(4 | 2, 6)
        \\try t.eq(4 | 4, 4)
        \\-- Bitwise xor
        \\try t.eq(4 || 2, 6)
        \\try t.eq(4 || 4, 0)
        \\-- Bitwise not
        \\try t.eq(~0, -1)
        \\try t.eq(~-1, 0)
        \\try t.eq(~1, -2)
        \\-- Bitwise right shift
        \\try t.eq(16 >> 2, 4)
        \\try t.eq(2 >> 2, 0)
        \\try t.eq(-1 >> 1, -1)  -- Performs sign extension
        \\-- Bitwise left shift
        \\try t.eq(2 << 4, 32)
        \\try t.eq(-2147483648 << 1, 0)  -- Does not perform sign extension
    );
}

test "Binary Expressions" {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\1 + 2
    );
    try t.eq(val.asF64toI32(), 3);

    val = try run.eval(
        \\1 + 2 + 3
    );
    try t.eq(val.asF64toI32(), 6);

    val = try run.eval(
        \\3 - 1
    );
    try t.eq(val.asF64toI32(), 2);

    val = try run.eval(
        \\3 * 4
    );
    try t.eq(val.asF64toI32(), 12);

    val = try run.eval(
        \\20 / 5
    );
    try t.eq(val.asF64toI32(), 4);

    // Power
    val = try run.eval(
        \\2 ^ 5
    );
    try t.eq(val.asF64toI32(), 32);

    // Modulus
    _ = try run.eval(
        \\import t 'test'
        \\try t.eq(3 % 2, 1)
    );

    // Right function call.
    val = try run.eval(
        \\func foo():
        \\  return 123
        \\1 + foo()
    );
    try t.eq(val.asF64toI32(), 124);
}

const VMrunner = struct {
    vm: *cy.UserVM,
    trace: cy.TraceInfo,

    fn create() *VMrunner {
        var new = t.alloc.create(VMrunner) catch fatal();
        new.init();
        return new;
    }

    fn destroy(self: *VMrunner) void {
        self.deinit();
        t.alloc.destroy(self);
    }

    fn init(self: *VMrunner) void {
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
            stdx.panicFmt("{} unreleased objects from previous eval", .{rc});
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

    fn getPanicMsg(self: *VMrunner) []const u8 {
        return self.vm.getPanicMsg();
    }

    /// Don't print panic errors.
    fn evalSilent(self: *VMrunner, src: []const u8) !cy.Value {
        cy.silentError = true;
        defer cy.silentError = false;
        try self.resetEnv();
        return self.vm.eval("main", src, .{ .singleRun = false }) catch |err| {
            return err;
        };
    }

    fn eval(self: *VMrunner, src: []const u8) !cy.Value {
        // Eval with new env.
        try self.resetEnv();
        return self.vm.eval("main", src, .{ .singleRun = false }) catch |err| {
            if (err == error.Panic) {
                try self.vm.dumpPanicStackTrace();
            }
            return err;
        };
    }

    fn resetEnv(self: *VMrunner) !void {
        self.vm.deinit();
        const rc = self.vm.getGlobalRC();
        if (rc != 0) {
            log.debug("{} unreleased objects from previous eval", .{rc});
            return error.UnreleasedObjects;
        }
        try self.vm.init(t.alloc);
        self.vm.setTrace(&self.trace);
    }

    fn evalNoReset(self: *VMrunner, src: []const u8) !cy.Value {
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
        for (list.items()) |it, i| {
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
