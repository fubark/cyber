const std = @import("std");
const build_options = @import("build_options");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const t = stdx.testing;
// const qjs = @import("qjs");

const cy = @import("../src/cyber.zig");
const QJS = cy.QJS;
const log = stdx.log.scoped(.behavior_test);

test "Automatic reference counting." {
    const run = Runner.create();
    defer run.destroy();

    const trace = run.getTrace();

    // List literal does not escape expression. No ref count.
    var val = try run.traceEval(
        \\[1, 2]
        \\return
    );
    try t.eq(trace.numRetains, 0);
    try t.eq(trace.numReleases, 0);

    // List literal is assigned to a local. Increase ref count.
    val = try run.traceEval(
        \\a = [1, 2]
    );
    try t.eq(trace.numRetains, 1);
    try t.eq(trace.numReleases, 1);

    // Assigning to another variable increases the ref count.
    val = try run.traceEval(
        \\a = [1, 2]
        \\b = a
    );
    try t.eq(trace.numRetains, 2);
    try t.eq(trace.numReleases, 2);

    // vm.checkMemory is able to detect retain cycle.
    val = try run.traceEval(
        \\a = []
        \\b = []
        \\a.add(b)
        \\b.add(a)
    );
    try t.eq(trace.numRetains, 4);
    try t.eq(trace.numReleases, 2);
    try t.eq(try run.checkMemory(), false);
    try t.eq(trace.numRetainCycles, 1);
    try t.eq(trace.numRetainCycleRoots, 2);
    try t.eq(trace.numReleases, 4);
}

// test "Optionals" {
//     const run = Runner.create();
//     defer run.destroy();

//     var val = try run.eval(
//         \\foo = none
//         \\foo
//     );
//     try t.eq(val.isNone(), true);
//     run.deinitValue(val);
// }

// test "Binary expr with generator function call." {
//     const run = Runner.create();
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
    const run = Runner.create();
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
}

test "Logic operators" {
    const run = Runner.create();
    defer run.destroy();

    var val = try run.eval(
        \\false or false
    );
    try t.eq(val.asBool(), false);

    val = try run.eval(
        \\false or true
    );
    try t.eq(val.asBool(), true);

    val = try run.eval(
        \\false and true
    );
    try t.eq(val.asBool(), false);

    val = try run.eval(
        \\true and true
    );
    try t.eq(val.asBool(), true);

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
    const run = Runner.create();
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
//     const run = Runner.create();
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
//     const run = Runner.create();
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
//     const run = Runner.create();
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
    const run = Runner.create();
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
        \\  // Comment.
        \\foo()
    );
    try t.eq(val.asI32(), 123);

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

// test "Numbers." {
//     const run = Runner.create();
//     defer run.destroy();

//     var val = try run.eval(
//         \\1
//     );
//     try t.eq(val.asI32(), 1);
//     run.deinitValue(val);

//     val = try run.eval(
//         \\-1
//     );
//     try t.eq(val.asI32(), -1);
//     run.deinitValue(val);
// }

// test "Parentheses" {
//     const run = Runner.create();
//     defer run.destroy();

//     // Parentheses at left of binary expression.
//     var val = try run.eval(
//         \\(2 + 3) * 4
//     );
//     try t.eq(val.asI32(), 20);
//     run.deinitValue(val);

//     // Parentheses at right of binary expression.
//     val = try run.eval(
//         \\2 * (3 + 4)
//     );
//     try t.eq(val.asI32(), 14);
//     run.deinitValue(val);

//     // Nested parentheses.
//     val = try run.eval(
//         \\2 + ((3 + 4) / 7)
//     );
//     try t.eq(val.asI32(), 3);
//     run.deinitValue(val);
// }

// test "Operator precedence." {
//     const run = Runner.create();
//     defer run.destroy();

//     // Multiplication before addition.
//     var val = try run.eval(
//         \\2 + 3 * 4
//     );
//     try t.eq(val.asI32(), 14);
//     run.deinitValue(val);

//     // Division before addition.
//     val = try run.eval(
//         \\2 + 4 / 4
//     );
//     try t.eq(val.asI32(), 3);
//     run.deinitValue(val);

//     // Power before addition.
//     val = try run.eval(
//         \\2 + 3 ** 2
//     );
//     try t.eq(val.asI32(), 11);
//     run.deinitValue(val);
// }

// test "Comments" {
//     const run = Runner.create();
//     defer run.destroy();

//     // Single line comment.
//     var val = try run.eval(
//         \\// 1
//         \\2
//     );
//     try t.eq(val.asI32(), 2);
//     run.deinitValue(val);

//     // Multiple single line comments.
//     val = try run.eval(
//         \\// 1
//         \\// 2
//         \\// 3
//         \\4
//     );
//     try t.eq(val.asI32(), 4);
//     run.deinitValue(val);
// }

test "Strings" {
    const run = Runner.create();
    defer run.destroy();

    // Const string with single quotes.
    var val = try run.eval(
        \\str = 'abc'
        \\str
    );
    var str = try run.valueString(val);
    try t.eqStr(str, "abc");

    // Const string with unicode.
    val = try run.eval(
        \\str = 'abc🦊xyz🐶'
        \\str
    );
    str = try run.valueString(val);
    try t.eqStr(str, "abc🦊xyz🐶");

    // Const string with escaped single quote.
    val = try run.eval(
        \\str = 'ab\'c'
        \\str
    );
    str = try run.valueString(val);
    try t.eqStr(str, "ab'c");

    // Const string multi-line backtick literal.
    val = try run.eval(
        \\str = `abc
        \\abc`
        \\str
    );
    str = try run.valueString(val);
    try t.eqStr(str, "abc\nabc");

    // Heap string. 
    val = try run.eval(
        \\str = 'abc'
        \\str + 'xyz'
    );
    str = try run.valueString(val);
    try t.eqStr(str, "abcxyz");
    run.deinitValue(val);

    t.setLogLevel(.debug);
}

test "Lists" {
    const run = Runner.create();
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
    const run = Runner.create();
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
    // val = try run.eval(
    //     \\a = {
    //     \\  b: 'hello'
    //     \\}
    //     \\a['b']
    // );
    // const str = try run.valueToString(val);
    // defer t.alloc.free(str);
    // try t.eqStr(str, "hello");
    // run.deinitValue(val);

    // Add to empty map.
    val = try run.eval(
        \\a = {}
        \\a[123] = 234
        \\a[123]
    );
    try t.eq(val.asI32(), 234);

//     // Nested list.
//     val = try run.eval(
//         \\a = {
//         \\  b: [ 1, 2 ]
//         \\}
//         \\a.b[1]
//     );
//     try t.eq(val.asI32(), 2);
//     run.deinitValue(val);

//     // Nested list with items separated by new line.
//     val = try run.eval(
//         \\a = {
//         \\  b: [
//         \\    1
//         \\    2
//         \\  ]
//         \\}
//         \\a.b[1]
//     );
//     try t.eq(val.asI32(), 2);
//     run.deinitValue(val);
}

test "Variables" {
    const run = Runner.create();
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
}

// test "if expression" {
//     const run = Runner.create();
//     defer run.destroy();

//     var val = try run.eval(
//         \\foo = true
//         \\if foo then 123 else 456
//     );
//     try t.eq(val.asI32(), 123);
//     run.deinitValue(val);
//     val = try run.eval(
//         \\foo = false
//         \\if foo then 123 else 456
//     );
//     try t.eq(val.asI32(), 456);
//     run.deinitValue(val);
// }

test "if statement" {
    const run = Runner.create();
    defer run.destroy();

    // If/else.
    var val = try run.eval(
        \\foo = true
        \\if foo:
        \\  return 123
        \\else:
        \\  return 456
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

test "Infinite for loop." {
    const run = Runner.create();
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
    const run = Runner.create();
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
    const run = Runner.create();
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
    const run = Runner.create();
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

    // Temporary vars only exist in their for loop scope.
    // They don't interfere with vars declared by the user.
    val = try run.eval(
        \\elem = 123
        \\list = [1, 2, 3]
        \\for list as elem:
        \\  pass
        \\elem
    );
    try t.eq(val.asI32(), 123);
}

test "For loop over range." {
    const run = Runner.create();
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

    // Temporary vars only exist in their for loop scope.
    // They don't interfere with vars declared by the user.
    val = try run.eval(
        \\i = 123
        \\sum = 0
        \\for 0..10 as i:
        \\  sum += i
        \\i
    );
    try t.eq(val.asI32(), 123);

    // // Increment by step.
    // val = try run.eval(
    //     \\iters = 0
    //     \\for 0..10 as i += 3:
    //     \\   iters += 1
    //     \\iters
    // );
    // try t.eq(val.asI32(), 4);
    // run.deinitValue(val);

    // // Increment by non const step value. Two loops after another.
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
    const run = Runner.create();
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
    const run = Runner.create();
    defer run.destroy();

    // Closure over number in main scope.
    // var val = try run.eval(
    //     \\a = 123
    //     \\func foo():
    //     \\  return a
    //     \\foo()
    // );
    // try t.eq(val.asI32(), 123);

    // Closure over local number in function.
    var val = try run.eval(
        \\func foo():
        \\  a = 123
        \\  return () => a
        \\fn = foo()
        \\fn()
    );
    try t.eq(val.asI32(), 123);

    // Closure over local number in function using a param.
    val = try run.eval(
        \\func foo():
        \\  a = 123
        \\  return b => a + b
        \\fn = foo()
        \\fn(1)
    );
    try t.eq(val.asI32(), 124);

    // Closure over local number in function using a param in parentheses.
    val = try run.eval(
        \\func foo():
        \\  a = 123
        \\  return (b) => a + b
        \\fn = foo()
        \\fn(1)
    );
    try t.eq(val.asI32(), 124);

    // Closure over local number in function using a multiple params.
    val = try run.eval(
        \\func foo():
        \\  a = 123
        \\  return (b, c) => a + b + c
        \\fn = foo()
        \\fn(1, 2)
    );
    try t.eq(val.asI32(), 126);

    // Closure over local retained object in function.
    val = try run.eval(
        \\func foo():
        \\  a = [ 123 ]
        \\  return () => a[0]
        \\fn = foo()
        \\fn()
    );
    try t.eq(val.asI32(), 123);
}

test "function declaration" {
    const run = Runner.create();
    defer run.destroy();

    // Stack top should return to main.
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
    const run = Runner.create();
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
//     const run = Runner.create();
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

// test "access expression" {
//     const run = Runner.create();
//     defer run.destroy();

//     // One level of access from parent.
//     var val = try run.eval(
//         \\map = { a: func () => 5 }
//         \\map.a()
//     );
//     try t.eq(val.asI32(), 5);
//     run.deinitValue(val);

//     // Multiple levels of access from parent.
//     val = try run.eval(
//         \\map = { a: { b: func () => 5 } }
//         \\map.a.b()
//     );
//     try t.eq(val.asI32(), 5);
//     run.deinitValue(val);
// }

test "Binary Expressions" {
    const run = Runner.create();
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

const RunnerImpl = switch (build_options.cyEngine) {
    .vm => VMrunner,
    // .qjs => QJSrunner,
    else => void,
};

const Runner = struct {
    inner: RunnerImpl,

    fn create() *Runner {
        var new = t.alloc.create(Runner) catch fatal();
        new.inner.init();
        return new;
    }

    fn destroy(self: *Runner) void {
        self.inner.deinit();
        t.alloc.destroy(self);
    }

    fn deinitValue(self: *Runner, val: cy.Value) void {
        self.inner.deinitValue(val);
    }

    fn parse(self: *Runner, src: []const u8) !cy.ParseResultView {
        return self.inner.parse(src);
    }

    fn getTrace(self: *Runner) *cy.TraceInfo {
        return &self.inner.trace;
    }

    fn traceEval(self: *Runner, src: []const u8) !cy.Value {
        return self.inner.traceEval(src);
    }

    fn checkMemory(self: *Runner) !bool {
        return self.inner.checkMemory();
    }

    fn eval(self: *Runner, src: []const u8) !cy.Value {
        return self.inner.eval(src);
    }

    fn eval2(self: *Runner, src: []const u8, embed_interrupts: bool) !cy.Value {
        return self.inner.eval2(src, embed_interrupts);
    }

    pub fn valueString(self: *Runner, val: cy.Value) ![]const u8 {
        return self.inner.valueString(val);
    }

    pub fn valueToIntSlice(self: *Runner, val: cy.Value) ![]const i32 {
        return self.inner.valueToIntSlice(val);
    }
};

const VMrunner = struct {
    vm: cy.UserVM,
    trace: cy.TraceInfo,

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
    }

    fn deinitValue(self: *VMrunner, val: cy.Value) void {
        self.vm.release(val, true);
    }

    fn checkMemory(self: *VMrunner) !bool {
        return self.vm.checkMemory(true);
    }

    fn traceEval(self: *VMrunner, src: []const u8) !cy.Value {
        return self.vm.eval(src, true);
    }

    fn eval(self: *VMrunner, src: []const u8) !cy.Value {
        return self.vm.eval(src, false);
    }

    fn eval2(self: *VMrunner, src: []const u8, embed_interrupts: bool) !cy.Value {
        _ = self;
        _ = src;
        _ = embed_interrupts;
        return undefined;
    }

    pub fn valueString(self: *VMrunner, val: cy.Value) ![]const u8 {
        if (self.vm.isValueString(val)) {
            return self.vm.valueAsString(val);
        } else {
            return error.NotAString;
        }
    }

    pub fn valueToIntSlice(self: *VMrunner, val: cy.Value) ![]const i32 {
        _ = self;
        const obj = stdx.ptrCastAlign(*cy.HeapObject, val.asPointer());
        const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(cy.Value), &obj.retainedList.list);
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

// const QJSrunner = struct {
//     parser: cy.Parser,
//     compiler: cy.JsTargetCompiler,

//     engine: cy.JsEngine,
//     promise: qjs.JSValue,
//     watchPromiseFunc: qjs.JSValue,
//     evalGeneratorSrcFunc: qjs.JSValue,

//     tasks: std.ArrayList(qjs.JSValue),
//     eval_promise_res: ?qjs.JSValue,

//     const Undefined = cy.JsValue.initQJS(qjs.Undefined);
//     const qjs_init_js = @embedFile("qjs_init.js");

//     fn init(self: *QJSrunner) void {
//         self.* = .{
//             .parser = cy.Parser.init(t.alloc),
//             .compiler = undefined,
//             .engine = cy.JsEngine.init(t.alloc),
//             .promise = undefined,
//             .tasks = std.ArrayList(qjs.JSValue).init(t.alloc),
//             .watchPromiseFunc = undefined,
//             .evalGeneratorSrcFunc = undefined,
//             .eval_promise_res = undefined,
//         };
//         self.compiler.init(t.alloc);
//         const engine = &self.engine;
//         const ctx = self.engine.inner.ctx;
//         qjs.JS_SetContextOpaque(ctx, self);

//         const global = qjs.JS_GetGlobalObject(ctx);
//         defer qjs.JS_FreeValue(ctx, global);

//         self.promise = qjs.JS_GetPropertyStr(ctx, global, "Promise");

//         // Run qjs_init.js
//         const val = cy.JsValue{
//             .inner = qjs.JS_Eval(ctx, qjs_init_js, qjs_init_js.len, "eval", qjs.JS_EVAL_TYPE_GLOBAL),
//         };
//         defer qjs.JS_FreeValue(ctx, val.inner);
//         const val_t = engine.getValueTag(val);
//         if (val_t == .exception) {
//             const exception = qjs.JS_GetException(ctx);
//             const str = qjs.JS_ToCString(ctx, exception);
//             defer qjs.JS_FreeCString(ctx, str);
//             stdx.panicFmt("init js exception {s}", .{ str });
//         }

//         const internal = qjs.JS_GetPropertyStr(ctx, global, "_internal");
//         defer qjs.JS_FreeValue(ctx, internal);
//         self.watchPromiseFunc = qjs.JS_GetPropertyStr(ctx, internal, "watchPromise");
//         self.evalGeneratorSrcFunc = qjs.JS_GetPropertyStr(ctx, internal, "evalGeneratorSrc");

//         var func = qjs.JS_NewCFunction(ctx, promiseResolved, "promiseResolved", 2);
//         var ret = qjs.JS_SetPropertyStr(ctx, internal, "promiseResolved", func);
//         if (ret != 1) {
//             stdx.panicFmt("set property {}", .{ret});
//         }

//         func = qjs.JS_NewCFunction(ctx, runEventLoop, "runEventLoop", 0);
//         ret = qjs.JS_SetPropertyStr(ctx, internal, "runEventLoop", func);
//         if (ret != 1) {
//             stdx.panicFmt("set property {}", .{ret});
//         }
//     }

//     fn deinit(self: *Runner) void {
//         self.tasks.deinit();

//         const ctx = self.engine.inner.ctx;
//         qjs.JS_FreeValue(ctx, self.promise);
//         qjs.JS_FreeValue(ctx, self.watchPromiseFunc);
//         qjs.JS_FreeValue(ctx, self.evalGeneratorSrcFunc);
//         self.engine.deinit();
//         self.parser.deinit();
//         self.compiler.deinit();
//     }

//     fn parse(self: *Runner, src: []const u8) !cy.ParseResultView {
//         return try self.parser.parse(src);
//     }

//     fn compile(self: *Runner, src: []const u8) !cy.JsTargetResultView {
//         const ast_res = try self.parser.parse(src);
//         if (ast_res.has_error) {
//             log.debug("Parse Error: {s}", .{ast_res.err_msg});
//             return error.ParseError;
//         }
//         return try self.compiler.compile(ast_res, .{ .wrap_in_func = true });
//     }

//     fn eval2(self: *Runner, src: []const u8, embed_interrupts: bool) !cy.JsValue {
//         const ast_res = try self.parser.parse(src);
//         if (ast_res.has_error) {
//             log.debug("Parse Error: {s}", .{ast_res.err_msg});
//             return error.ParseError;
//         }

//         const res = try self.compiler.compile(ast_res, .{
//             .gas_meter = if (embed_interrupts) .yield_interrupt else .none,
//             .wrap_in_func = true,
//         });
//         if (res.has_error) {
//             log.debug("Compile Error: {s}", .{res.err_msg});
//             return error.CompileError;
//         }

//         log.debug("out: {s}", .{res.output});

//         const ctx = self.engine.inner.ctx;

//         const csrc = try std.cstr.addNullByte(t.alloc, res.output);
//         defer t.alloc.free(csrc);

//         if (res.wrapped_in_generator) {
//             const js_src = qjs.JS_NewStringLen(ctx, res.output.ptr, res.output.len);
//             const val = cy.JsValue{
//                 .inner = qjs.JS_Call(ctx, self.evalGeneratorSrcFunc, qjs.Undefined, 1, &[_]qjs.JSValue{ js_src }),
//             };
//             const tag = self.engine.getValueTag(val);
//             if (tag == .exception) {
//                 const str = try self.getExceptionString(val);
//                 defer t.alloc.free(str);
//                 log.err("Runtime exception: {s}", .{str});
//                 return error.RuntimeError;
//             }
//             return val;
//         }

//         // Wrapped in function.
//         const func = self.engine.eval(csrc);
//         defer self.engine.deinitValue(func);

//         const val = self.engine.call(func, Undefined, &.{});
//         const tag = self.engine.getValueTag(val);
//         if (tag == .exception) {
//             const str = try self.getExceptionString(val);
//             defer t.alloc.free(str);
//             log.err("Runtime exception: {s}", .{str});
//             return error.RuntimeError;
//         } else {
//             self.eval_promise_res = null;
//             if (qjs.JS_IsInstanceOf(ctx, val.inner, self.promise) == 1) {
//                 defer qjs.JS_FreeValue(ctx, val.inner);
                    
//                 const id = qjs.JS_NewInt32(ctx, 1);
//                 _ = qjs.JS_Call(ctx, self.watchPromiseFunc, qjs.Undefined, 2, &[_]qjs.JSValue{ id, val.inner });
//                 qjs.js_std_loop(ctx);
//                 if (self.eval_promise_res) |promise_res| {
//                     return cy.JsValue{ .inner = promise_res };
//                 }

//                 if (self.tasks.items.len == 0) {
//                     return error.UnresolvedPromise;
//                 }
//                 while (self.tasks.items.len > 0) {
//                     const num_tasks = self.tasks.items.len;
//                     for (self.tasks.items[0..num_tasks]) |task| {
//                         const task_res = qjs.JS_Call(ctx, task, qjs.Undefined, 0, null);
//                         qjs.JS_FreeValue(ctx, task);

//                         const task_res_tag = QJS.getTag(ctx, task_res);
//                         if (task_res_tag == .exception) {
//                             const str = try self.getExceptionString(.{ .inner = task_res });
//                             defer t.alloc.free(str);
//                             log.err("Task exception: {s}", .{str});
//                             return error.RuntimeError;
//                         }
//                         log.debug("call task {}", .{task_res_tag});
//                     }
//                     try self.tasks.replaceRange(0, num_tasks, &.{});

//                     // Deplete event loop.
//                     qjs.js_std_loop(ctx);

//                     if (self.eval_promise_res) |promise_res| {
//                         return cy.JsValue{ .inner = promise_res };
//                     }
//                 }
//                 return error.UnresolvedPromise;
//             }
//         }
//         return val;
//     }

//     fn eval(self: *Runner, src: []const u8) !cy.JsValue {
//         return self.eval2(src, false);
//     }

//     fn deinitValue(self: *Runner, val: cy.JsValue) void {
//         self.engine.deinitValue(val);
//     }

//     pub fn valueToString(self: *Runner, val: cy.JsValue) ![]const u8 {
//         return self.engine.valueToString(val);
//     }

//     pub fn valueToIntSlice(self: *Runner, val: cy.JsValue) ![]const i32 {
//         return self.engine.valueToIntSlice(val);
//     }

//     pub fn getExceptionString(self: *Runner, val: cy.JsValue) ![]const u8 {
//         // Assumes val is the exception of last execution.
//         _ = val;
//         const ctx = self.engine.inner.ctx;
//         const exception = qjs.JS_GetException(ctx);
//         defer qjs.JS_FreeValue(ctx, exception);

//         return self.engine.valueToString(.{ .inner = exception });
//     }
// };

// fn promiseResolved(ctx: ?*qjs.JSContext, _: qjs.JSValueConst, _: c_int, argv: [*c]qjs.JSValueConst) callconv(.C) qjs.JSValue {
//     const runner = stdx.ptrCastAlign(*Runner, qjs.JS_GetContextOpaque(ctx));
//     const id = QJS.getInt32(argv[0]);
//     _ = id;
//     const js_ctx = runner.engine.inner.ctx;
//     runner.eval_promise_res = qjs.JS_DupValue(js_ctx, argv[1]);
//     return qjs.Undefined;
// }

// fn runEventLoop(ctx: ?*qjs.JSContext, _: qjs.JSValueConst, _: c_int, argv: [*c]qjs.JSValueConst) callconv(.C) qjs.JSValue {
//     _ = argv;
//     const runner = stdx.ptrCastAlign(*Runner, qjs.JS_GetContextOpaque(ctx));
//     const js_ctx = runner.engine.inner.ctx;
//     qjs.js_std_loop(js_ctx);
//     return qjs.Undefined;
// }