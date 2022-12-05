const std = @import("std");
const build_options = @import("build_options");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const t = stdx.testing;

const cy = @import("../src/cyber.zig");
const log = stdx.log.scoped(.behavior_test);

test "Fibers" {
    const run = VMrunner.create();
    defer run.destroy();

    // Init fiber without starting.
    var val = try run.eval(
        \\func foo(list):
        \\  coyield
        \\  list.add(123)
        \\list = []
        \\f = coinit foo(list)
        \\list.size()
    );
    try run.valueIsI32(val, 0);

    // Start fiber with yield at start.
    val = try run.eval(
        \\func foo(list):
        \\  coyield
        \\  list.add(123)
        \\list = []
        \\f = coinit foo(list)
        \\coresume f
        \\list.size()
    );
    try run.valueIsI32(val, 0);

    // Start fiber without yield.
    val = try run.eval(
        \\func foo(list):
        \\  list.add(123)
        \\list = []
        \\f = coinit foo(list)
        \\coresume f
        \\list[0]
    );
    try run.valueIsI32(val, 123);

    // Start fiber with yield in nested function.
    val = try run.eval(
        \\func bar():
        \\  alist = [] --This should be released after fiber is freed.
        \\  coyield
        \\func foo(list):
        \\  bar()
        \\  list.add(123)
        \\list = []
        \\f = coinit foo(list)
        \\coresume f
        \\list.size()
    );
    try run.valueIsI32(val, 0);

    // Continue to resume fiber.
    val = try run.eval(
        \\func foo(list):
        \\  list.add(123)
        \\  coyield
        \\  list.add(234)
        \\list = []
        \\f = coinit foo(list)
        \\coresume f
        \\coresume f
        \\list.size()
    );
    try run.valueIsI32(val, 2);
}

test "Structs." {
    const run = VMrunner.create();
    defer run.destroy();

    // Initialization.
    var val = try run.eval(
        \\type Node:
        \\  value
        \\n = Node{ value: 123 }
        \\n.value
    );
    try t.eq(val.asI32(), 123);

    // Initialize with heap value field.
    val = try run.eval(
        \\type Node:
        \\  value
        \\n = Node{ value: [123] }
        \\n.value[0]
    );
    try t.eq(val.asI32(), 123);

    // Set to struct field.
    val = try run.eval(
        \\type Node:
        \\  value
        \\n = Node{ value: 123 }
        \\n.value = 234
        \\n.value
    );
    try t.eq(val.asI32(), 234);

    // Set to field with heap value.
    val = try run.eval(
        \\type Node:
        \\  value
        \\n = Node{ value: [123] }
        \\n.value = 234
        \\n.value
    );
    try t.eq(val.asI32(), 234);

    // Struct to string returns struct's name. 
    val = try run.eval(
        \\type Node:
        \\  value
        \\n = Node{ value: 123 }
        \\toString(n)
    );
    try t.eqStr(try run.assertValueString(val), "Node");
    run.deinitValue(val);
}

test "Struct methods." {
    const run = VMrunner.create();
    defer run.destroy();

    // self param.
    var val = try run.eval(
        \\type Node:
        \\  value
        \\  func get(self):
        \\    return self.value
        \\n = Node{ value: 123 }
        \\n.get()
    );
    try t.eq(val.asI32(), 123);

    // self param with regular param.
    val = try run.eval(
        \\type Node:
        \\  value
        \\  func get(self, param):
        \\    return self.value + param
        \\n = Node{ value: 123 }
        \\n.get(321)
    );
    try t.eq(val.asI32(), 444);

    // self param with many regular param.
    val = try run.eval(
        \\type Node:
        \\  value
        \\  func get(self, param, param2):
        \\    return self.value + param - param2
        \\n = Node{ value: 123 }
        \\n.get(321, 1)
    );
    try t.eq(val.asI32(), 443);

    // Static method, no params.
    val = try run.eval(
        \\type Node:
        \\  value
        \\  func get():
        \\    return 123
        \\Node.get()
    );
    try t.eq(val.asI32(), 123);

    // Static method, one params.
    val = try run.eval(
        \\type Node:
        \\  value
        \\  func get(param):
        \\    return 123 + param
        \\Node.get(321)
    );
    try t.eq(val.asI32(), 444);

    // Static method, many params.
    val = try run.eval(
        \\type Node:
        \\  value
        \\  func get(param, param2):
        \\    return 123 + param - param2
        \\Node.get(321, 1)
    );
    try t.eq(val.asI32(), 443);
}

test "Stack trace unwinding." {
    const run = VMrunner.create();
    defer run.destroy();

    var res = run.eval(
        \\a = 123
        \\1 + a.foo
    );
    try t.expectError(res, error.Panic);
    var trace = run.getStackTrace();
    try t.eq(trace.frames.len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = "main",
        .line = 1,
        .col = 4,
    });

    // Function stack trace.
    res = run.eval(
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
        .line = 2,
        .col = 13,
    });
    try eqStackFrame(trace.frames[1], .{
        .name = "main",
        .line = 3,
        .col = 0,
    });
}

fn eqStackFrame(act: cy.StackFrame, exp: cy.StackFrame) !void {
    try t.eqStr(act.name, exp.name);
    try t.eq(act.line, exp.line);
    try t.eq(act.col, exp.col);
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
//     try t.eq(val.asI32(), 3);
//     run.deinitValue(val);
// }

test "Comparison ops." {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\1 < 2
    );
    try t.eq(val.asBool(), true);

    val = try run.eval(
        \\2 < 1
    );
    try t.eq(val.asBool(), false);

    val = try run.eval(
        \\1 > 2
    );
    try t.eq(val.asBool(), false);

    val = try run.eval(
        \\2 > 1
    );
    try t.eq(val.asBool(), true);

    val = try run.eval(
        \\1 <= 2
    );
    try t.eq(val.asBool(), true);
    val = try run.eval(
        \\2 <= 2
    );
    try t.eq(val.asBool(), true);
    val = try run.eval(
        \\3 <= 2
    );
    try t.eq(val.asBool(), false);

    val = try run.eval(
        \\1 >= 2
    );
    try t.eq(val.asBool(), false);
    val = try run.eval(
        \\2 >= 2
    );
    try t.eq(val.asBool(), true);
    val = try run.eval(
        \\3 >= 2
    );
    try t.eq(val.asBool(), true);

    // Using `is` keyword.
    val = try run.eval(
        \\3 is 2
    );
    try t.eq(val.asBool(), false);
    val = try run.eval(
        \\3 is 3
    );
    try t.eq(val.asBool(), true);

    // Number equals.
    val = try run.eval(
        \\3 == 2
    );
    try t.eq(val.asBool(), false);
    val = try run.eval(
        \\3 == 3
    );
    try t.eq(val.asBool(), true);

    // Const string equals.
    val = try run.eval(
        \\'foo' == 'bar'
    );
    try t.eq(val.asBool(), false);
    val = try run.eval(
        \\'foo' == 'foo'
    );
    try t.eq(val.asBool(), true);

    // Heap string equals.
    val = try run.eval(
        \\foo = 'fo' + 'o'
        \\foo == 'bar'
    );
    try t.eq(val.asBool(), false);
    val = try run.eval(
        \\foo = 'fo' + 'o'
        \\foo == 'foo'
    );
    try t.eq(val.asBool(), true);

    // Object equals.
    val = try run.eval(
        \\type S:
        \\  value
        \\s = S{ value: 123 }
        \\a = S{ value: 123 }
        \\a == s
    );
    try t.eq(val.asBool(), false);
    val = try run.eval(
        \\type S:
        \\  value
        \\s = S{ value: 123 }
        \\a = s
        \\a == s
    );
    try t.eq(val.asBool(), true);
}

test "Not equal comparison." {
    const run = VMrunner.create();
    defer run.destroy();

    // Using `is not` op.
    var val = try run.eval(
        \\3 is not 2
    );
    try t.eq(val.asBool(), true);
    val = try run.eval(
        \\3 is not 3
    );
    try t.eq(val.asBool(), false);

    // Comparing objects.
    val = try run.eval(
        \\type S:
        \\  value
        \\s = S{ value: 3 }
        \\s != 123
    );
    try t.eq(val.asBool(), true);
    val = try run.eval(
        \\type S:
        \\  value
        \\s = S{ value: 3 }
        \\t = s
        \\s != t
    );
    try t.eq(val.asBool(), false);
}

test "Logic operators" {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\false or false
    );
    try t.eq(val.asBool(), false);

    val = try run.eval(
        \\false or true
    );
    try t.eq(val.asBool(), true);

    // If first `or` operand evaluates to true, the second expression is not evaluated
    // and the first operand is returned.
    val = try run.eval(
        \\a = none
        \\123 or a.foo
    );
    try t.eq(val.asI32(), 123);

    // If first `or` operand evaluates to false, the second expression is evaluated and returned.
    val = try run.eval(
        \\a = 123
        \\0 or a
    );
    try t.eq(val.asI32(), 123);

    val = try run.eval(
        \\false and true
    );
    try t.eq(val.asBool(), false);

    val = try run.eval(
        \\true and true
    );
    try t.eq(val.asBool(), true);

    // If first `and` operand evaluates to false, the second expression is not evaluated
    // and the first operand is returned
    val = try run.eval(
        \\a = none
        \\0 and a.foo
    );
    try t.eq(val.asI32(), 0);

    // If first `and` operand evaluates to true, the second expression is evaluated and returned.
    val = try run.eval(
        \\123 and 234
    );
    try t.eq(val.asI32(), 234);

    val = try run.eval(
        \\not false
    );
    try t.eq(val.asBool(), true);

    val = try run.eval(
        \\not true
    );
    try t.eq(val.asBool(), false);
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
//     try t.eq(val.asI32(), 124);
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
//     try t.eq(val.asI32(), 123);
//     run.deinitValue(val);

//     // await on value.
//     val = try run.eval(
//         \\func foo():
//         \\  return 234
//         \\await foo()
//     );
//     try t.eq(val.asI32(), 234);
//     run.deinitValue(val);
// }

test "Indentation." {
    const run = VMrunner.create();
    defer run.destroy();

    // Detect end of block.
    var val = try run.eval(
        \\func foo():
        \\  return 123
        \\foo()
    );
    try t.eq(val.asI32(), 123);

    // Comment before end of block.
    val = try run.eval(
        \\func foo():
        \\  return 123
        \\  -- Comment.
        \\foo()
    );
    try t.eq(val.asI32(), 123);

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
    try t.eq(val.asI32(), 123);

    // Continue from grand parent indentation.
    val = try run.eval(
        \\func foo():
        \\  if false:
        \\    if false:
        \\      pass
        \\  return 123 
        \\foo()
    );
    try t.eq(val.asI32(), 123);
}

test "Numbers." {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\1
    );
    try t.eq(val.asI32(), 1);

    val = try run.eval(
        \\-1
    );
    try t.eq(val.asI32(), -1);
}

test "Parentheses" {
    const run = VMrunner.create();
    defer run.destroy();

    // Parentheses at left of binary expression.
    var val = try run.eval(
        \\(2 + 3) * 4
    );
    try t.eq(val.asI32(), 20);

    // Parentheses at right of binary expression.
    val = try run.eval(
        \\2 * (3 + 4)
    );
    try t.eq(val.asI32(), 14);

    // Nested parentheses.
    val = try run.eval(
        \\2 + ((3 + 4) / 7)
    );
    try t.eq(val.asI32(), 3);
}

test "Operator precedence." {
    const run = VMrunner.create();
    defer run.destroy();

    // Multiplication before addition.
    var val = try run.eval(
        \\2 + 3 * 4
    );
    try t.eq(val.asI32(), 14);

    // Division before addition.
    val = try run.eval(
        \\2 + 4 / 4
    );
    try t.eq(val.asI32(), 3);

    // Power before addition.
    val = try run.eval(
        \\2 + 3 ^ 2
    );
    try t.eq(val.asI32(), 11);

    // Variables and parenthesis.
    val = try run.eval(
        \\time = 50
        \\minTime = 50
        \\timeRange = 100
        \\5 + 90 * (time - minTime) / timeRange
    );
    try t.eq(val.asI32(), 5);

    // Left recursion with different operators.
    val = try run.eval(
        \\5 + 2 * 3 / 3 
    );
    try t.eq(val.asI32(), 7);
}

test "Comments" {
    const run = VMrunner.create();
    defer run.destroy();

    // Single line comment.
    var val = try run.eval(
        \\-- 1
        \\2
    );
    try t.eq(val.asI32(), 2);
    val = try run.eval(
        \\2
        \\-- 1
    );
    try t.eq(val.asI32(), 2);

    // Multiple single line comments.
    val = try run.eval(
        \\-- 1
        \\-- 2
        \\-- 3
        \\4
    );
    try t.eq(val.asI32(), 4);
}

test "Strings" {
    const run = VMrunner.create();
    defer run.destroy();

    // Const string with single quotes.
    var val = try run.eval(
        \\str = 'abc'
        \\str
    );
    var str = try run.assertValueString(val);
    try t.eqStr(str, "abc");

    // Const string with unicode.
    val = try run.eval(
        \\str = 'abc🦊xyz🐶'
        \\str
    );
    str = try run.assertValueString(val);
    try t.eqStr(str, "abc🦊xyz🐶");

    // Const string with escaped single quote.
    val = try run.eval(
        \\str = 'ab\'c'
        \\str
    );
    str = try run.assertValueString(val);
    try t.eqStr(str, "ab'c");

    // Const string multi-line backtick literal.
    val = try run.eval(
        \\str = `abc
        \\abc`
        \\str
    );
    str = try run.assertValueString(val);
    try t.eqStr(str, "abc\nabc");

    // Heap string. 
    val = try run.eval(
        \\str = 'abc'
        \\str + 'xyz'
    );
    str = try run.assertValueString(val);
    try t.eqStr(str, "abcxyz");
    run.deinitValue(val);

    // String interpolation using double quotes.
    val = try run.eval(
        \\a = 'World'
        \\b = 123
        \\"Hello {a} {b}"
    );
    str = try run.assertValueString(val);
    try t.eqStr(str, "Hello World 123");
    run.deinitValue(val);

    // String interpolation using back ticks.
    val = try run.eval(
        \\a = 'World'
        \\b = 123
        \\`Hello {a} {b}`
    );
    str = try run.assertValueString(val);
    try t.eqStr(str, "Hello World 123");
    run.deinitValue(val);

    // String interpolation with expr at start.
    val = try run.eval(
        \\"{10}"
    );
    str = try run.assertValueString(val);
    try t.eqStr(str, "10");
    run.deinitValue(val);

    // String interpolation with nested paren group.
    val = try run.eval(
        \\"{(1 + 2) * 3}"
    );
    str = try run.assertValueString(val);
    try t.eqStr(str, "9");
    run.deinitValue(val);
}

test "Lists" {
    const run = VMrunner.create();
    defer run.destroy();

    // Index access.
    var val = try run.eval(
        \\a = [1, 2, 3]
        \\a[0]
    );
    try t.eq(val.asI32(), 1);

    // Negative index access.
    val = try run.eval(
        \\a = [1, 2, 3]
        \\a[-1]
    );
    try t.eq(val.asI32(), 3);

    // Set to index.
    val = try run.eval(
        \\a = []
        \\a.resize(3)
        \\a[2] = 3
        \\a[2]
    );
    try t.eq(val.asI32(), 3);

    // Start to end index slice.
    val = try run.eval(
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
}

test "Maps" {
    const run = VMrunner.create();
    defer run.destroy();

    // Number entry.
    var val = try run.eval(
        \\a = {
        \\  b: 32
        \\}
        \\a['b']
    );
    try t.eq(val.asI32(), 32);

    // Access expression.
    val = try run.eval(
        \\a = {
        \\  b: 32
        \\}
        \\a.b
    );
    try t.eq(val.asI32(), 32);

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
    try t.eq(val.asI32(), 234);

    // Nested list.
    val = try run.eval(
        \\a = {
        \\  b: [ 1, 2 ]
        \\}
        \\a.b[1]
    );
    try t.eq(val.asI32(), 2);

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
    try t.eq(val.asI32(), 2);
}

test "Assignment statements" {
    const run = VMrunner.create();
    defer run.destroy();

    // Assign to variable.
    var val = try run.eval(
        \\a = 1
        \\a += 10
        \\a
    );
    try t.eq(val.asI32(), 11);

    // Assign to field.
    val = try run.eval(
        \\type S:
        \\  foo
        \\s = S{ foo: 1 }
        \\s.foo += 10
        \\s.foo
    );
    try t.eq(val.asI32(), 11);
}

test "Variables and scope" {
    const run = VMrunner.create();
    defer run.destroy();

    // Variable declaration.
    var val = try run.eval(
        \\a = 1
        \\a
    );
    try t.eq(val.asI32(), 1);

    // Overwrite existing var.
    val = try run.eval(
        \\a = 1
        \\a = 2
        \\a
    );
    try t.eq(val.asI32(), 2);

    // Use existing var.
    val = try run.eval(
        \\a = 1
        \\b = a + 2
        \\b
    );
    try t.eq(val.asI32(), 3);

    // Using a variable that was conditionally assigned.
    val = try run.eval(
        \\if true:
        \\  a = 1
        \\a
    );
    try t.eq(val.asI32(), 1);

    // Using a variable that was conditionally not assigned.
    val = try run.eval(
        \\if false:
        \\  a = 1
        \\a
    );
    try t.eq(val.isNone(), true);

    // Using a variable that was assigned in a loop.
    val = try run.eval(
        \\for 2..3 as i:
        \\  a = i
        \\a
    );
    try t.eq(val.asI32(), 2);

    // Using a variable that was not assigned in a loop.
    val = try run.eval(
        \\for 2..2 as i:
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
    try t.eq(val.asI32(), 1);

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
        \\type S:
        \\  value
        \\if false:
        \\  a = S{ value: 123 }
    );

    // Same test in method scope.
    try run.resetEnv();
    run.vm.fillUndefinedStackSpace(cy.Value.initPtr(null));
    val = try run.evalNoReset(
        \\type S:
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
    try t.eq(val.asI32(), 123);

    val = try run.eval(
        \\foo = false
        \\if foo then 123 else 456
    );
    try t.eq(val.asI32(), 456);
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
    try t.eq(val.asI32(), 123);

    val = try run.eval(
        \\foo = false
        \\if foo:
        \\  return 123
        \\else:
        \\  return 456
    );
    try t.eq(val.asI32(), 456);

    // else if condition.
    val = try run.eval(
        \\if false:
        \\  return 456
        \\else true:
        \\  return 123
        \\else:
        \\  return 456
    );
    try t.eq(val.asI32(), 123);
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
    try t.eq(val.asI32(), 123);

    val = try run.eval(
        \\if false:
        \\  foo = 123
        \\else:
        \\  foo = 456
        \\foo
    );
    try t.eq(val.asI32(), 456);

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
    try t.eq(val.asI32(), 123);
}

test "Infinite for loop." {
    const run = VMrunner.create();
    defer run.destroy();

    // Infinite loop clause.
    var val = try run.eval(
        \\i = 0
        \\for:
        \\  i += 1
        \\  if i == 10:
        \\    break
        \\i
    );
    try t.eq(val.asI32(), 10);
}

test "Conditional for loop." {
    const run = VMrunner.create();
    defer run.destroy();

    // `for` with condition expression.
    var val = try run.eval(
        \\i = 0
        \\for i != 10:
        \\  i += 1
        \\i
    );
    try t.eq(val.asI32(), 10);
}

test "For loop over list." {
    const run = VMrunner.create();
    defer run.destroy();

    // Basic.
    var val = try run.eval(
        \\list = [1, 2, 3]
        \\sum = 0
        \\for list as it:
        \\   sum += it
        \\sum
    );
    try t.eq(val.asI32(), 6);
}

test "For iterator." {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\list = []
        \\for 0..10 as i:
        \\  list.add(i)
        \\sum = 0
        \\for list as i:
        \\  sum += i
        \\sum
    );
    try t.eq(val.asI32(), 45);

    // Loop iterator var overwrites the user var.
    val = try run.eval(
        \\elem = 123
        \\list = [1, 2, 3]
        \\for list as elem:
        \\  pass
        \\elem
    );
    try t.eq(val.asI32(), 3);
}

test "For loop over range." {
    const run = VMrunner.create();
    defer run.destroy();

    // Basic.
    var val = try run.eval(
        \\iters = 0
        \\for 0..10 as i:
        \\   iters += 1
        \\iters
    );
    try t.eq(val.asI32(), 10);

    // two `for` with range don't interfere with each other
    val = try run.eval(
        \\iters = 0
        \\for 0..10 as i:
        \\   iters += 1
        \\for 0..10 as i:
        \\   iters += 1
        \\iters
    );
    try t.eq(val.asI32(), 20);

    // two `for` with non const max value don't interfere with each other
    val = try run.eval(
        \\foo = 10
        \\iters = 0
        \\for 0..foo as i:
        \\   iters += 1
        \\for 0..foo as i:
        \\   iters += 1
        \\iters
    );
    try t.eq(val.asI32(), 20);

    // Nested for loop.
    val = try run.eval(
        \\count = 0
        \\for 0..10 as i:
        \\  inner = 0
        \\  for 0..10 as j:
        \\    inner += 1
        \\  count += inner
        \\count
    );
    try t.eq(val.asI32(), 100);

    // Index vars overwrites user var.
    val = try run.eval(
        \\i = 123
        \\sum = 0
        \\for 0..10 as i:
        \\  sum += i
        \\i
    );
    try t.eq(val.asI32(), 9);

    // Reverse direction.
    val = try run.eval(
        \\sum = 0
        \\for 10..0 as i:
        \\  sum += i
        \\sum
    );
    try t.eq(val.asI32(), 55);

    // Custom step.
    // val = try run.eval(
    //     \\sum = 0
    //     \\for 0..10, 2 as i:
    //     \\  sum += i
    //     \\sum
    // );
    // try t.eq(val.asI32(), 20);

    // Custom step variable.
    // val = try run.eval(
    //     \\iters = 0
    //     \\step = 3
    //     \\for 0..10 as i += step:
    //     \\   iters += 1
    //     \\for 0..10 as i += step:
    //     \\   iters += 1
    //     \\iters
    // );
    // try t.eq(val.asI32(), 8);
    // run.deinitValue(val);
}

test "Native function call." {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\list = []
        \\for 0..10 as i:
        \\   list.add(i)
        \\list[9]
    );
    try t.eq(val.asI32(), 9);
}

test "Closures." {
    const run = VMrunner.create();
    defer run.destroy();

    // Closure read over number in main block static function.
    var val = try run.eval(
        \\a = 123
        \\func foo():
        \\  return a
        \\foo()
    );
    try run.valueIsI32(val, 123);

    // Closure write over number in main block static function.
    val = try run.eval(
        \\a = 123
        \\func foo():
        \\  a = 234
        \\foo()
        \\a
    );
    try run.valueIsI32(val, 234);

    // Closure add assign over number in main block static function.
    val = try run.eval(
        \\a = 123
        \\func foo():
        \\  a += 1
        \\foo()
        \\a
    );
    try run.valueIsI32(val, 124);

    // Closure read over number in main block function value.
    val = try run.eval(
        \\a = 123
        \\foo = () => a
        \\foo()
    );
    try run.valueIsI32(val, 123);

    // Closure write over number in main block function value.
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
    try t.eq(val.asI32(), 55);

    // Recursion with long lived object.
    val = try run.eval(
        \\type S:
        \\  n
        \\func foo(o):
        \\  if o.n is 0:
        \\    return 0
        \\  n = o.n
        \\  o.n = o.n - 1
        \\  return n + foo(o)
        \\foo(S{ n: 10 })
    );
    try t.eq(val.asI32(), 55);

    // Recursion with new objects.
    val = try run.eval(
        \\type S:
        \\  n
        \\func foo(o):
        \\  if o.n is 0:
        \\    return 0
        \\  return o.n + foo(S{ n: o.n - 1 })
        \\foo(S{ n: 10 })
    );
    try t.eq(val.asI32(), 55);
}

test "function declaration" {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\func foo():
        \\    return 2 + 2
        \\sum = 0
        \\for 0..10 as i:
        \\   sum += foo()
        \\sum
    );
    try t.eq(val.asI32(), 40);

    // Function with no params.
    val = try run.eval(
        \\func foo():
        \\    return 2 + 2
        \\foo()
    );
    try t.eq(val.asI32(), 4);

    // Function with one param.
    val = try run.eval(
        \\func foo(bar):
        \\    return bar + 2
        \\foo(1)
    );
    try t.eq(val.asI32(), 3);

    // Function with multiple param.
    val = try run.eval(
        \\func foo(bar, inc):
        \\    return bar + inc
        \\foo(20, 10)
    );
    try t.eq(val.asI32(), 30);
}

test "Lambdas." {
    const run = VMrunner.create();
    defer run.destroy();

    // No params.
    var val = try run.eval(
        \\foo = () => 2 + 2
        \\foo()
    );
    try t.eq(val.asI32(), 4);

    // One param.
    val = try run.eval(
        \\foo = a => a + 1
        \\foo(10)
    );
    try t.eq(val.asI32(), 11);

    // Lambda with multiple param.
    val = try run.eval(
        \\foo = (bar, inc) => bar + inc
        \\foo(20, 10)
    );
    try t.eq(val.asI32(), 30);

//     // Lambda assign declaration.
//     val = try run.eval(
//         \\foo = {}
//         \\func foo.bar():
//         \\  return 2
//         \\foo.bar()
//     );
//     try t.eq(val.asI32(), 2);
}

// test "Function named parameters call." {
//     const run = VMrunner.create();
//     defer run.destroy();

//     var val = try run.eval(
//         \\func foo(a, b):
//         \\  return a - b
//         \\foo(a: 3, b: 1)
//     );
//     try t.eq(val.asI32(), 2);
//     run.deinitValue(val);

//     val = try run.eval(
//         \\func foo(a, b):
//         \\  return a - b
//         \\foo(a: 1, b: 3)
//     );
//     try t.eq(val.asI32(), -2);
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
//     try t.eq(val.asI32(), 2);
//     run.deinitValue(val);
// }

test "access expression" {
    const run = VMrunner.create();
    defer run.destroy();

    // One level of access from parent.
    var val = try run.eval(
        \\map = { a: () => 5 }
        \\map.a()
    );
    try t.eq(val.asI32(), 5);

    // Multiple levels of access from parent.
    val = try run.eval(
        \\map = { a: { b: () => 5 } }
        \\map.a.b()
    );
    try t.eq(val.asI32(), 5);
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
}

test "Binary Expressions" {
    const run = VMrunner.create();
    defer run.destroy();

    var val = try run.eval(
        \\1 + 2
    );
    try t.eq(val.asI32(), 3);

    val = try run.eval(
        \\1 + 2 + 3
    );
    try t.eq(val.asI32(), 6);

    val = try run.eval(
        \\3 - 1
    );
    try t.eq(val.asI32(), 2);

    val = try run.eval(
        \\3 * 4
    );
    try t.eq(val.asI32(), 12);

    val = try run.eval(
        \\20 / 5
    );
    try t.eq(val.asI32(), 4);

    // Power
    val = try run.eval(
        \\2 ^ 5
    );
    try t.eq(val.asI32(), 32);

    // Modulus
    val = try run.eval(
        \\3 % 2
    );
    try t.eq(val.asI32(), 1);

    // Right function call.
    val = try run.eval(
        \\func foo():
        \\  return 123
        \\1 + foo()
    );
    try t.eq(val.asI32(), 124);
}

const VMrunner = struct {
    vm: cy.UserVM,
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

    fn eval(self: *VMrunner, src: []const u8) !cy.Value {
        // Eval with new env.
        try self.resetEnv();
        return self.vm.eval(src) catch |err| {
            if (err == error.Panic) {
                self.vm.dumpPanicStackTrace();
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

    fn evalNoReset(self: *VMrunner, src: []const u8) cy.EvalError!cy.Value {
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
        const list = stdx.ptrCastAlign(*cy.List(cy.Value), &obj.list.list);
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