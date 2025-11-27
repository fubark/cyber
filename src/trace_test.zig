const std = @import("std");
const t = std.testing;
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const log = std.log.scoped(.trace_test);
const setup = @import("test/setup.zig");
const c = @import("capi.zig");
const Config = setup.Config;
const VMrunner = setup.VMrunner;
const evalPass = setup.evalPass;
const eval = setup.eval;
const Runner = setup.VMrunner;
const EvalResult = setup.EvalResult;
const eqUserError = setup.eqUserError;
const NullId = std.math.maxInt(u32);

// Tests that require `Trace` build option.

test "ARC global ref." {
    try evalPass(.{},
        \\use test
        \\use meta
        \\global a ^int = ^123
        \\test.eq(123, a.*)
        \\test.eq(1, meta.trace_retains())
        \\-- Releases at end of program.
        \\test.eq(0, meta.trace_releases())
    );
}

test "ARC assign ref to local." {
    try evalPass(.{},
        \\use test
        \\use meta
        \\begin:
        \\  a := ^123
        \\test.eq(1, meta.trace_retains())
        \\test.eq(1, meta.trace_releases())
    );
}

test "ARC assign local ref to another local." {
    try evalPass(.{},
        \\use test
        \\use meta
        \\begin:
        \\  a := ^123
        \\  b := a
        \\test.eq(2, meta.trace_retains())
        \\test.eq(2, meta.trace_releases())
    );
}

test "ARC index retains ref." {
    try evalPass(.{},
        \\use test
        \\use meta
        \\begin:
        \\  a := [1]^int{^123}
        \\  b := a[0]
        \\test.eq(2, meta.trace_retains())
        \\test.eq(2, meta.trace_releases())
    );
}

test "ARC assign ref to field." {
    try evalPass(.{},
        \\use test
        \\use meta
        \\type S:
        \\  value ^int
        \\begin:
        \\  s := S{value=^123}
        \\  s.value = ^234
        \\  test.eq(234, s.value.*)
        \\test.eq(2, meta.trace_retains())
        \\test.eq(2, meta.trace_releases())
    );
}

test "ARC assign ref to index." {
    try evalPass(.{},
        \\use test
        \\use meta
        \\begin:
        \\  a := [1]^int{^123}
        \\  test.eq(1, meta.trace_retains())
        \\  test.eq(1, meta.trace_retains())
        \\  a[0] = ^234
        \\  test.eq(2, meta.trace_retains())
        \\  test.eq(1, meta.trace_releases())
        \\  test.eq(234, a[0].*)
        \\test.eq(2, meta.trace_retains())
        \\test.eq(2, meta.trace_releases())
    );
}

test "ARC doesn't retain reference local when passed as call arg." {
    try evalPass(.{},
        \\use test
        \\use meta
        \\type S:
        \\  a int
        \\fn foo(s ^S):
        \\  test.eq(123, s.a)
        \\begin:
        \\  s := ^S{a=123}
        \\  foo(s)
        \\test.eq(1, meta.trace_retains())
        \\test.eq(1, meta.trace_releases())
    );
}

test "ARC releases temp call arg reference." {
    try evalPass(.{},
        \\use test
        \\use meta
        \\type S:
        \\  a int
        \\fn foo(s ^S):
        \\  test.eq(123, s.a)
        \\foo(^S{a=123})
        \\test.eq(1, meta.trace_retains())
        \\test.eq(1, meta.trace_releases())
    );
}

test "ARC for function return values." {
    // Assignment from owned return does need anymore retains.
    try evalPass(.{},
        \\use test
        \\use meta
        \\type S:
        \\  value int
        \\fn foo() -> ^S:
        \\  return ^S{value=123}
        \\begin:
        \\  f := foo()
        \\  test.eq(f.value, 123)
        \\test.eq(1, meta.trace_retains())
        \\test.eq(1, meta.trace_releases())
    );

    // Discarding an owned return.
    try evalPass(.{},
        \\use test
        \\use meta
        \\type S:
        \\  value int
        \\fn foo() -> ^S:
        \\  return ^S{value=123}
        \\begin:
        \\  _ = foo()
        \\test.eq(1, meta.trace_retains())
        \\test.eq(1, meta.trace_releases())
    );
}

test "ARC releases temp ref after access expression." {
    try evalPass(.{},
        \\use test
        \\use meta
        \\type S:
        \\  a int
        \\res := (^S{a=123}).a
        \\test.eq(123, res)
        \\test.eq(1, meta.trace_retains())
        \\test.eq(1, meta.trace_releases())
    );
}

test "ARC in loops." {
    // An rc value is assigned to a local inside loop.
    try evalPass(.{},
        \\use test
        \\use meta
        \\for 0..3:
        \\  a := ^123
        \\test.eq(3, meta.trace_retains())
        \\test.eq(3, meta.trace_releases())
    );

    // An rc value is assigned to a local outside loop.
    try evalPass(.{},
        \\use test
        \\use meta
        \\
        \\begin:
        \\  a := ^123
        \\  for 0..3:
        \\    a = ^234
        \\test.eq(4, meta.trace_retains())
        \\test.eq(4, meta.trace_releases())
    );

    // Iterating Slice.
    try evalPass(.{},
        \\use test
        \\use meta
        \\begin:
        \\  a := []^int{^123, ^234}
        \\  test.eq(5, meta.trace_retains())
        \\  test.eq(2, meta.trace_releases())
        \\  for a |ref|:
        \\    b := ref.*
        \\test.eq(11, meta.trace_retains())
        \\test.eq(11, meta.trace_releases())
    );
}
