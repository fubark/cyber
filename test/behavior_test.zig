const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const zeroInit = std.mem.zeroInit;

const all = @import("all");
const cy = all.cy;
const cli = all.cli;
const log = cy.log.scoped(.behavior_test);
const c = cy.C;
const setup = @import("setup.zig");
const eval = setup.eval;
const compile = setup.compile;
const evalPass = setup.evalPass;
const VMrunner = setup.VMrunner;
const Config = setup.Config;
const eqUserError = setup.eqUserError;
const EvalResult = setup.EvalResult;

const Case = struct {
    config: ?Config,
    path: []const u8,
};

const Runner = struct {
    cases: std.ArrayListUnmanaged(Case),

    fn case(s: *Runner, path: []const u8) void {
        s.case2(null, path);
    }

    fn case2(s: *Runner, config: ?Config, path: []const u8) void {
        s.cases.append(t.alloc, Case{ .config = config, .path = path }) catch @panic("error");
    }
};

const caseFilter: ?[]const u8 = null;
const failFast: bool = true;

// TODO: This could be split into compiler only tests and backend tests.
//       Compiler tests would only need to be run once.
//       Right now we just run everything again since it's not that much.
test "Tests." {
    var run = Runner{ .cases = .{} };
    defer run.cases.deinit(t.alloc);

    const backend = cy.fromTestBackend(build_options.testBackend);
    const aot = cy.isAot(backend);

    run.case("syntax/adjacent_stmt_error.cy");
    run.case("syntax/block_no_child_error.cy");
    run.case("syntax/change_to_spaces_error.cy");
    run.case("syntax/change_to_tabs_error.cy");
    run.case("syntax/comment_first_line.cy");
    run.case("syntax/comment_last_line.cy");
    run.case("syntax/comment_multiple.cy");
    run.case("syntax/compact_block_error.cy");
    run.case("syntax/func_missing_param_type_error.cy");
if (!aot) {
    run.case("syntax/indentation.cy");
}
    run.case("syntax/last_line_empty_indent.cy");
    run.case("syntax/let_func_param_error.cy");
    run.case("syntax/no_stmts.cy");
    run.case("syntax/object_decl_eof.cy");
    run.case("syntax/object_decl_typespec_eof.cy");
    run.case("syntax/object_missing_semicolon_error.cy");
    run.case("syntax/parse_end_error.cy");
    run.case("syntax/parse_middle_error.cy");
    run.case("syntax/parse_skip_shebang_error.cy");
if (!aot) {
    run.case("syntax/parse_skip_shebang_panic.cy");
    run.case("syntax/parse_start_error.cy");
    run.case("syntax/skip_utf8_bom.cy");
    run.case("syntax/stmt_end_error.cy");
    run.case("syntax/tabs_spaces_error.cy");
    run.case("syntax/visibility.cy");
    run.case("syntax/wrap_stmts.cy");

    run.case("functions/assign_capture_local_error.cy");
    run.case("functions/assign_error.cy");
    run.case("functions/call_at_ct.cy");
    run.case("functions/call_at_ct_error.cy");
    run.case("functions/call_block.cy");
}
    run.case("functions/call_bool_param_error.cy");
if (!aot) {
    run.case("functions/call_closure.cy");
    run.case("functions/call_closure_param_panic.cy");
}

    run.case("functions/call_dyn_object_member.cy");
    run.case("functions/call_excess_args_error.cy");
    run.case("functions/call_excess_args_overloaded_error.cy");
if (!aot) {
    run.case("functions/call_fiber_param.cy");
}
    run.case("functions/call_float_param_error.cy");
if (!aot) {
    run.case("functions/call_metatype_param.cy");
}
    run.case("functions/call_method_missing_error.cy");
if (!aot) {
    run.case("functions/call_method_missing_panic.cy");
}
    run.case("functions/call_method_sig_error.cy");
if (!aot) {
    run.case("functions/call_method_sig_panic.cy");
    run.case("functions/call_host.cy");
    run.case("functions/call_host_param_panic.cy");
}
if (!aot) {
    run.case("functions/call_object_param.cy");
}
    run.case("functions/call_object_param_error.cy");
if (!aot) {
    run.case("functions/call_op.cy");
    run.case("functions/call_overloaded_dyn_arg_panic.cy");
    run.case("functions/call_param_panic.cy");
}
    run.case("functions/call_pointer_param_error.cy");
    run.case("functions/call_recursive.cy");
if (!aot) {
    run.case("functions/call_shorthand.cy");
    run.case("functions/call_static_lambda_incompat_arg_panic.cy");
    run.case("functions/call_string_param_error.cy");
    run.case("functions/call_symbol_param_error.cy");
    run.case("functions/call_typed_param.cy");
    run.case("functions/call_undeclared_error.cy");   
    run.case("functions/call_void_return_error.cy");
    run.case("functions/decl_over_builtin.cy");
    run.case("functions/funcsym_type.cy");
    run.case("functions/func_type.cy");
    run.case("functions/func_type_closure_error.cy");
    run.case("functions/func_type_error.cy");
    run.case("functions/func_union_type.cy");
    run.case("functions/func_union_type_error.cy");
    run.case("functions/lambda.cy");
    run.case("functions/lambda_incompat_arg_panic.cy");
    run.case("functions/let_call_recursive.cy");
    run.case("functions/let_func.cy");
    run.case("functions/namespace_call.cy");
    run.case("functions/object_funcs.cy");
    run.case("functions/overload.cy");
    run.case("functions/read_capture_local_error.cy");
    run.case("functions/static.cy");
    run.case("functions/template_ct_func.cy");
    run.case("functions/template_ct_func_host_error.cy");
    run.case("functions/template_ct_func_throw_error.cy");
    run.case("functions/template_functions.cy");
    // run.case("functions/void_param_error.cy");

    run.case("memory/arc_cases.cy");
if (!cy.isWasm) {
    run.case("memory/default_memory.cy");
}
    run.case("memory/gc_reference_cycle_unreachable.cy");
    run.case2(.{ .cleanupGC = true }, "memory/gc_reference_cycle_reachable.cy");
    run.case("memory/release_expr_stmt_return.cy");
    run.case("memory/release_scope_end.cy");

    run.case("types/cast.cy");
    run.case("types/cast_error.cy");
    run.case("types/cast_narrow_panic.cy");
    run.case("types/cast_panic.cy");
    // try case("types/cast_union_panic.cy")
    // // Failed to cast to abstract type at runtime.
    // try eval(.{ .silent = true },
    //     \\let a = 123
    //     \\print(a as String)
    // , struct { fn func(run: *VMrunner, res: EvalResult) !void {
    //     try run.expectErrorReport(res, error.Panic,
    //         \\panic: Can not cast `int` to `String`.
    //         \\
    //         \\main:2:9 main:
    //         \\print(a as String)
    //         \\        ^
    //         \\
    //     );
    // }}.func);
    run.case("types/choice_access_error.cy");
    run.case("types/choice_access_panic.cy");
    run.case("types/choice_type.cy");
    run.case("types/cstructs.cy");
    run.case("types/distinct.cy");
}
    run.case("types/dyn_recent_type_error.cy");
if (!aot) {
    run.case("types/enums.cy");
}
    run.case("types/func_return_type_error.cy");
    run.case("types/func_param_type_undeclared_error.cy");
if (!aot) {
    run.case("types/let_table.cy");
    run.case("types/object_init_dyn_field.cy");
    run.case("types/object_init_field.cy");
    run.case("types/object_init_field_error.cy");
    run.case("types/object_init_field_panic.cy");
    run.case("types/object_init_undeclared_field_error.cy");
    run.case("types/object_set_field.cy");
    run.case("types/object_set_field_dyn_recv_panic.cy");
    run.case("types/object_set_field_error.cy");
    run.case("types/object_set_field_panic.cy");
    run.case("types/object_set_undeclared_field_error.cy");
    run.case("types/object_zero_init.cy");
    run.case("types/object_zero_init_error.cy");
    run.case("types/objects.cy");
    run.case("types/optionals_incompat_value_error.cy");
    run.case("types/optionals_unwrap_panic.cy");
    run.case("types/optionals.cy");
    run.case("types/pointers.cy");
    run.case("types/self_type.cy");
    run.case("types/slices.cy");
    run.case("types/struct_circular_dep_error.cy");
    run.case("types/structs.cy");
    run.case("types/template_choices.cy");
    run.case("types/template_dep_param_type.cy");
    run.case("types/template_dep_param_type_error.cy");
    run.case("types/template_object_init_noexpand_error.cy");
    run.case("types/template_object_spec_noexpand_error.cy");
    run.case("types/template_object_expand_error.cy");
    run.case("types/template_objects.cy");
    run.case("types/template_structs.cy");
    run.case("types/template_builtin_ints.cy");
    run.case("types/trait_error.cy");
    run.case("types/trait.cy");
    run.case("types/tuple.cy");
    run.case("types/type_alias.cy");
    run.case("types/type_spec.cy");
    run.case("types/unnamed_object.cy");

    if (!cy.isWasm) {
        run.case2(Config.initFileModules("./test/modules/type_spec.cy"), "modules/type_spec.cy");
        run.case2(Config.initFileModules("./test/modules/type_alias.cy"), "modules/type_alias.cy");
        run.case2(Config.initFileModules("./test/modules/import_not_found_error.cy").withSilent(), "modules/import_not_found_error.cy");
        run.case2(Config.initFileModules("./test/modules/import_missing_sym_error.cy").withSilent(), "modules/import_missing_sym_error.cy");
        run.case2(Config.initFileModules("./test/modules/import_rel_path.cy"), "modules/import_rel_path.cy");
        run.case2(Config.initFileModules("./test/modules/import_implied_rel_path.cy"), "modules/import_implied_rel_path.cy");
        run.case2(Config.initFileModules("./test/modules/import_stmt_error.cy"), "modules/import_stmt_error.cy");
        run.case2(Config.initFileModules("./test/modules/import_unresolved_rel_path.cy"), "modules/import_unresolved_rel_path.cy");
        
        // Import when running main script in the cwd.
        run.case2(Config.initFileModules("./import_rel_path.cy").withChdir("./test/modules"), "modules/import_rel_path.cy");
        // Import when running main script in a child directory.
        run.case2(Config.initFileModules("../import_rel_path.cy").withChdir("./test/modules/test_mods"), "modules/import_rel_path.cy");

        run.case2(Config.initFileModules("./test/modules/import.cy"), "modules/import.cy");
        run.case2(Config.initFileModules("./test/modules/import_all.cy"), "modules/import_all.cy");
        run.case2(Config.initFileModules("./test/modules/import_sym_alias.cy"), "modules/import_sym_alias.cy");
    }
    run.case("modules/core.cy");
    run.case("modules/cy.cy");
    run.case("modules/math.cy");
    run.case("modules/test_eq_panic.cy");
    run.case("modules/test.cy");
    if (!cy.isWasm and !build_options.link_test) {
        // Disabled when linking with lib because os.args().len check fails.
        run.case("modules/os.cy");
    }

    // Disabled test: printing to stdout hangs test runner.
    // run.case2(.{ .silent = true }, "meta/dump_locals.cy");
    run.case("meta/get_panic.cy"); 
    run.case("meta/get_set.cy"); 
    run.case("meta/init_pair.cy"); 
    run.case("meta/init_pair_error.cy"); 
    run.case("meta/metatype.cy");
    run.case("meta/set_panic.cy"); 

    run.case("concurrency/fibers.cy");
    run.case("concurrency/await.cy");

    run.case("errors/error_values.cy");
    run.case("errors/throw.cy");
    run.case("errors/throw_func_panic.cy");
    run.case("errors/throw_main_panic.cy");
    run.case("errors/throw_nested_func_panic.cy");
    run.case("errors/try_catch.cy");
    run.case("errors/try_catch_expr.cy");
    run.case("errors/try_expr.cy");

    run.case("core/arithmetic_ops.cy");
    run.case("core/arithmetic_unsupported_panic.cy");
    run.case("core/arrays.cy");
    run.case("core/bool.cy");
    run.case("core/byte.cy");
    run.case("core/compare_eq.cy");
    run.case("core/compare_neq.cy");
    run.case("core/dynamic_ops.cy");
    run.case("core/escape_sequences.cy");
    run.case("core/floats.cy");
    run.case("core/ints.cy");
    run.case("core/int_unsupported_notation_error.cy");
    run.case("core/list_neg_index_oob_panic.cy");
    run.case("core/lists.cy");
    run.case("core/logic_ops.cy");
    run.case("core/map_index_panic.cy");
    run.case("core/maps.cy");
    run.case("core/must.cy");
    run.case("core/must_panic.cy");
    run.case("core/op_precedence.cy");
    run.case("core/panic_panic.cy");
    run.case("core/raw_string_single_quote_error.cy");
    run.case("core/raw_string_new_line_error.cy");
    run.case("core/rune_empty_lit_error.cy");
    run.case("core/rune_multiple_lit_error.cy");
    run.case("core/rune_grapheme_cluster_lit_error.cy");
    run.case("core/set_index_unsupported_error.cy");
    run.case("core/string_new_line_error.cy");
    run.case("core/string_interpolation.cy");
}
    run.case("core/strings.cy");
    run.case("core/strings_ascii.cy");
    run.case("core/strings_utf8.cy");
    run.case("core/string_slices_ascii.cy");
    run.case("core/string_slices_utf8.cy");
if (!aot) {
    run.case("core/symbols.cy");
    run.case("core/table.cy");
    run.case("core/table_access_panic.cy"); 
    run.case("core/tag_lit.cy");
}

    run.case("vars/context_var_redeclare_error.cy");
    run.case("vars/context_var.cy");
    run.case("vars/local_assign_dyn_panic.cy");
    run.case("vars/local_assign_error.cy");
if (!aot) {
    run.case("vars/local_assign.cy");
}
    run.case("vars/local_attr_error.cy");
    run.case("vars/local_decl_infers_dyn.cy");

    run.case("vars/local_dup_error.cy");
    run.case("vars/local_init_dyn_panic.cy");
    run.case("vars/local_init_error.cy");
if (!aot) {
    run.case("vars/local_init.cy");
    run.case("vars/local_shadow_after_ref.cy");
    run.case("vars/op_assign.cy");
}
    run.case("vars/read_undeclared_error.cy");
    run.case("vars/read_undeclared_error.cy");
    run.case("vars/read_undeclared_diff_scope_error.cy");
    run.case("vars/read_outside_if_var_error.cy");
    run.case("vars/read_outside_for_iter_error.cy");
    run.case("vars/read_outside_for_var_error.cy");
    run.case("vars/set_undeclared_error.cy");
if (!aot) {
    run.case("vars/static_assign.cy");
    run.case("vars/static_init.cy");
}
    run.case("vars/static_init_capture_error.cy");
    run.case("vars/static_init_circular_ref_error.cy");
if (!aot) {
    run.case("vars/static_init_dependencies.cy");
    run.case("vars/static_init_error.cy");
    run.case("vars/static_init_read_self_error.cy");
    run.case("vars/use_global.cy");
    run.case("vars/use_global_panic.cy");

    run.case("control_flow/for_iter.cy");
    run.case("control_flow/for_iter_unsupported_panic.cy");
    run.case("control_flow/for_range.cy");
    run.case("control_flow/if_expr.cy");
    run.case("control_flow/if_expr_error.cy");
    run.case("control_flow/if_stmt.cy");
    run.case("control_flow/if_unwrap.cy");
    run.case("control_flow/switch.cy");
    run.case("control_flow/switch_error.cy");
    run.case("control_flow/return.cy");
    run.case("control_flow/while_cond.cy");
    run.case("control_flow/while_inf.cy");
    run.case("control_flow/while_unwrap.cy");
}

    var numPassed: u32 = 0;
    for (run.cases.items) |run_case| {
        if (caseFilter) |filter| {
            if (std.mem.indexOf(u8, run_case.path, filter) == null) {
                continue;
            }
        }
        std.debug.print("test: {s}\n", .{run_case.path});
        case2(run_case.config, run_case.path) catch |err| {
            std.debug.print("Failed: {}\n", .{err});
            if (failFast) {
                return err;
            } else {
                continue;
            }
        };
        numPassed += 1;
    }
    std.debug.print("Tests: {}/{}\n", .{numPassed, run.cases.items.len});
    if (numPassed < run.cases.items.len) {
        return error.Failed;
    }
}

test "Compile." {
    // examples.
    try compileCase(.{}, "../examples/fiber.cy");
    try compileCase(.{}, "../examples/fizzbuzz.cy");
    try compileCase(.{}, "../examples/hello.cy");
    try compileCase(.{}, "../examples/ffi.cy");
    try compileCase(.{}, "../examples/account.cy");
    try compileCase(.{}, "../examples/fibonacci.cy");

    // tools.
    try compileCase(.{}, "../src/tools/bench.cy");
    try compileCase(.{}, "../src/tools/llvm.cy");
    try compileCase(.{}, "../src/tools/clang_bs.cy");
    try compileCase(.{}, "../src/tools/md4c.cy");
    if (!cy.isWasm) {
        try compileCase(Config.initFileModules("./src/tools/cbindgen.cy"), "../src/tools/cbindgen.cy");
        try compileCase(Config.initFileModules("./docs/gen-docs.cy"), "../docs/gen-docs.cy");
        try compileCase(Config.initFileModules("./src/jit/gen-stencils-a64.cy"), "../src/jit/gen-stencils-a64.cy");
        try compileCase(Config.initFileModules("./src/jit/gen-stencils-x64.cy"), "../src/jit/gen-stencils-x64.cy");
    }

    // benchmarks.
    try compileCase(.{}, "bench/fib/fib.cy");
    try compileCase(.{}, "bench/fiber/fiber.cy");
    try compileCase(.{}, "bench/for/for.cy");
    try compileCase(.{}, "bench/heap/heap.cy");
    try compileCase(.{}, "bench/string/index.cy");
}

fn compileCase(config: Config, path: []const u8) !void {
    const fpath = try std.mem.concat(t.alloc, u8, &.{ thisDir(), "/", path });
    defer t.alloc.free(fpath);
    const contents = try std.fs.cwd().readFileAlloc(t.alloc, fpath, 1e9);
    defer t.alloc.free(contents);
    try compile(config, contents);
}

test "Multiple evals with same VM." {
    var run = VMrunner.init();
    defer run.deinit();

    const src =
        \\use t 'test'
        \\var a = 1
        \\t.eq(a, 1)
        ;

    _ = try run.evalPass(.{ 
        .enableFileModules = false,
    }, src);
    _ = try run.evalPass(.{ 
        .enableFileModules = false,
    }, src);
    _ = try run.evalPass(.{ 
        .enableFileModules = false,
    }, src);
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

    try case("ffi/call_incompat_arg_panic.cy");
    try case("ffi/call_excess_args_panic.cy");

    // TODO: Test callback failure and verify stack trace.
    // Currently, the VM aborts when encountering a callback error.
    // A config could be added to make the initial FFI call detect an error and throw a panic instead.

    try case("ffi/ffi.cy");
}

test "windows new lines" {
    try eval(.{ .silent = true }, "a = 123\r\nb = 234\r\nc =",
    struct { fn func(run: *VMrunner, res: EvalResult) !void {
        try run.expectErrorReport(res, c.ErrorCompile,
            \\ParseError: Expected right expression for assignment statement.
            \\
            \\main:3:4:
            \\c =
            \\   ^
            \\
        );
    }}.func);
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

inline fn thisDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse @panic("error");
}

fn case(path: []const u8) !void {
    try case2(null, path);
}

fn case2(config: ?Config, path: []const u8) !void {
    const fpath = try std.mem.concat(t.alloc, u8, &.{ thisDir(), "/", path });
    defer t.alloc.free(fpath);
    const contents = try std.fs.cwd().readFileAllocOptions(t.alloc, fpath, 1e9, null, @alignOf(u8), 0);
    defer t.alloc.free(contents);

    var idx = std.mem.indexOf(u8, contents, "cytest:") orelse {
        return error.MissingCyTest;
    };

    var rest = contents[idx+7..];
    idx = std.mem.indexOfScalar(u8, rest, '\n') orelse rest.len;
    const test_t = std.mem.trim(u8, rest[0..idx], " ");

    if (std.mem.eql(u8, test_t, "error")) {
        // Find end of last comment.
        const start = idx+1;
        while (true) {
            if (rest[idx..].len >= 3 and rest[idx] == '\n' and rest[idx+1] == '-' and rest[idx+2] == '-') {
                idx += 1;
                if (std.mem.indexOfScalarPos(u8, rest, idx, '\n')) |nl| {
                    idx = nl;
                } else {
                    idx = rest.len;
                }
            } else {
                break;
            }
        }

        const exp = rest[start..idx];

        var buf: [1024]u8 = undefined;
        const len = std.mem.replacementSize(u8, exp, "--", "");
        _ = std.mem.replace(u8, exp, "--", "", &buf);

        const Context = struct {
            exp: []const u8,
        };
        var ctx = Context{ .exp = buf[0..len]};
        var fconfig: Config = config orelse .{ .silent = true };
        fconfig.ctx = &ctx;
        try eval(fconfig, contents
        , struct { fn func(run: *VMrunner, res: EvalResult) !void {
            const ctx_: *Context = @ptrCast(@alignCast(run.ctx));
            try run.expectErrorReport2(res, ctx_.exp);
        }}.func);
    } else if (std.mem.eql(u8, test_t, "pass")) {
        const fconfig: Config = config orelse .{};
        try evalPass(fconfig, contents);
    } else {
        return error.UnsupportedTestType;
    }
}
