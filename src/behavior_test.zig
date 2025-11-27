const std = @import("std");
const builtin = @import("builtin");
const build_config = @import("build_config");
const test_config = @import("test_config");
const stdx = @import("stdx");
const t = stdx.testing;
const zeroInit = std.mem.zeroInit;

const cli = @import("cli.zig");
const log = std.log.scoped(.behavior_test);
const c = @import("capi.zig");
const setup = @import("test/setup.zig");
const eval = setup.eval;
const compile = setup.compile;
const evalPass = setup.evalPass;
const VMrunner = setup.VMrunner;
const Config = setup.Config;
const eqUserError = setup.eqUserError;
const EvalResult = setup.EvalResult;
const is_wasm = builtin.cpu.arch.isWasm();

const Case = struct {
    config: Config,
    skip: bool = false,
};

const Runner = struct {
    cases: std.ArrayListUnmanaged(Case),

    fn case(s: *Runner, path: []const u8) void {
        s.case2(.{ .uri = path });
    }

    fn case2(s: *Runner, config: Config) void {
        s.cases.append(t.alloc, Case{ .config = config }) catch @panic("error");
    }

    fn caseSkip(s: *Runner, path: []const u8) void {
        s.cases.append(t.alloc, Case{ .config = .{ .uri = path }, .skip = true }) catch @panic("error");
    }

    fn caseDebug(s: *Runner, path: []const u8) void {
        s.case2(.{ .debug=true, .uri=path});
    }
};

const caseFilter: ?[]const u8 = null;
// const caseFilter: ?[]const u8 = "meta.cy";
const failFast: bool = true;

pub fn isAot(backend: c.Backend) bool {
    return backend == c.BackendTCC or backend == c.BackendCC;
}

// TODO: This could be split into compiler only tests and backend tests.
//       Compiler tests would only need to be run once.
//       Right now we just run everything again since it's not that much.
test "Tests." {
    var run = Runner{ .cases = .{} };
    defer run.cases.deinit(t.alloc);

    const backend = setup.fromTestBackend(test_config.test_backend);
    const aot = isAot(backend);

    run.case("syntax/block_no_child_error.cy");
    run.case("syntax/change_to_spaces_error.cy");
    run.case("syntax/change_to_tabs_error.cy");
    run.case("syntax/comment_first_line.cy");
    run.case("syntax/comment_last_line.cy");
    run.case("syntax/comment_multiple.cy");
    run.case("syntax/compact_block_error.cy");
    run.case("syntax/func_missing_param_type_error.cy");
    run.case("syntax/func_param_group.cy");
    run.case("syntax/indentation.cy");
    run.case("syntax/last_line_empty_indent.cy");
    run.case("syntax/no_stmts.cy");
    run.case("syntax/parse_end_error.cy");
    run.case("syntax/parse_middle_error.cy");
    run.case("syntax/parse_skip_shebang_error.cy");
    run.case("syntax/parse_skip_shebang_panic.cy");
    run.case("syntax/parse_start_error.cy");
    run.case("syntax/skip_utf8_bom.cy");
    run.case("syntax/stmt_end_error.cy");
    run.case("syntax/tabs_spaces_error.cy");
    run.case("syntax/tuple_field_group.cy");
    run.case("syntax/type_decl_eof.cy");
    run.case("syntax/type_missing_colon_error.cy");
    run.case("syntax/visibility.cy");
    run.case("syntax/wrap_stmts.cy");
    run.case("functions/assign_capture_local_error.cy");
    run.case("functions/assign_error.cy");
    run.case("functions/call_at_ct.cy");
    run.case("functions/call_at_ct_error.cy");
    run.case("functions/call_block.cy");
    run.case("functions/call_incompat_param_error.cy");
    run.case("functions/call_closure.cy");
    run.case("functions/call_closure_param_error.cy");
    run.case("functions/call_excess_args_error.cy");
    run.case("functions/call_excess_args_overloaded_error.cy");
    run.case("functions/call_overload_incompat_arg_error.cy");
    run.case("functions/call_float_param_error.cy");
    run.case("functions/call_method_missing_error.cy");
    run.case("functions/call_method_sig_error.cy");
    run.case("functions/call_host.cy");
    run.case("functions/call_host_param_error.cy");
    run.case("functions/call_op.cy");
    run.case("functions/call_recursive.cy");
    run.case("functions/call_static_lambda_incompat_arg_error.cy");
    run.case("functions/call_struct_param.cy");
    run.case("functions/call_struct_param_error.cy");
    run.case("functions/call_template_method_error.cy");
    run.case("functions/call_typed_param.cy");
    run.case("functions/call_undeclared_error.cy");   
    run.case("functions/call_missing_assignment_error.cy");
    run.case("functions/decl_over_builtin.cy");
    run.case("functions/funcsym_type.cy");
    run.case("functions/func_type.cy");
    run.case("functions/func_type_closure_error.cy");
    run.case("functions/func_type_error.cy");
    run.case("functions/func_union_type.cy");
    run.case("functions/func_union_type_error.cy");
    run.case("functions/lambda.cy");
    run.case("functions/lambda_incompat_arg_error.cy");
    run.case("functions/main_func_overload_error.cy");
    run.case("functions/main_func_panic.cy");
    run.case("functions/main_func_sig_error.cy");
    run.case("functions/main_func_top_stmt_error.cy");
    run.case("functions/@init.cy");
    run.case("functions/overload.cy");
    run.case("functions/read_capture_local_error.cy");
    run.case("functions/capture_non_ref_error.cy");
    run.case("functions/static.cy");
    run.case("functions/struct_funcs.cy");
    run.case("functions/template_functions.cy");
    run.case("functions/template_method_error.cy");
    // run.case("functions/void_param_error.cy");
    run.case("memory/arc_cases.cy");
    run.case("memory/borrow.cy");
    run.case("memory/borrow_index_addr_error.cy");
    run.case("memory/call_with_ref_local.cy");
    run.case("memory/call_with_ref_rvalue.cy");
    run.case("memory/custom_deinit.cy");
if (!is_wasm) {
    // run.case("memory/default_memory.cy");
}
    // run.case("memory/gc_reference_cycle_unreachable.cy");
    // run.case2(.{ .cleanupGC = true }, "memory/gc_reference_cycle_reachable.cy");
    run.case("memory/release_expr_stmt_return.cy");
    run.case("memory/release_scope_end.cy");
    run.case("memory/lift_borrow_error.cy");
    run.case("memory/lift_borrow_container_error.cy");
    run.case("memory/lift_moves_rvalue.cy");
    run.case("memory/move_local_to_return.cy");
    run.case("memory/move_use_after_error.cy");
    run.case("memory/override_copy.cy");
    run.case("memory/partial_move.cy");
    run.case("memory/partial_move_use_after_error.cy");
    run.case("memory/partial_move_ref_child_error.cy");
    run.case("memory/return_moves_rvalue.cy");
    run.case("memory/return_borrow_scope_missing_error.cy");
    run.case("memory/return_borrow_container_scope_missing_error.cy");
    run.case("memory/scope_param_missing_error.cy");
    run.case("memory/scope_too_many_params_error.cy");
    run.case("memory/scope_return_error.cy");
    run.case("memory/scope_return.cy");
    run.case("memory/scope_assign_shorter_span_lifetime_error.cy");
    run.case("memory/scope_assign_shorter_borrow_lifetime_error.cy");
    run.case("memory/sink_use_rec_after_error.cy");
    run.case("memory/sink_use_arg_after_error.cy");
    run.case("types/bitcast.cy");
    run.case("types/cast.cy");
    run.case("types/cast_error.cy");
    // // Failed to cast to abstract type at runtime.
    // try eval(.{ .silent = true },
    //     \\let a = 123
    //     \\print(a as string)
    // , struct { fn func(run: *VMrunner, res: EvalResult) !void {
    //     try run.expectErrorReport(res, error.Panic,
    //         \\panic: Can not cast `int` to `string`.
    //         \\
    //         \\main:2:9 main:
    //         \\print(a as string)
    //         \\        ^
    //         \\
    //     );
    // }}.func);
    run.case("types/choice_access_error.cy");
    run.case("types/choice_type.cy");
    run.case("types/choice_unwrap_panic.cy");
    run.case("types/cstructs.cy");
    run.case("types/enums.cy");
    run.case("types/func_return_type_error.cy");
    run.case("types/func_param_type_undeclared_error.cy");
    run.case("types/method_implicit_self_assign_error.cy");
    run.case("types/method_shadow_member.cy");
    run.case("types/objects.cy");
    run.case("types/object_downcast_panic.cy");
    run.case("types/object_init_object_field.cy");
    run.case("types/object_init_field.cy");
    run.case("types/object_init_field_error.cy");
    run.case("types/object_set_field.cy");
    run.case("types/object_set_field_error.cy");
    run.case("types/pointers.cy");
    run.case("types/PtrSpan.cy");
    run.case("types/Self.cy");
    run.case("types/struct_circular_dep_error.cy");
    run.case("types/structs.cy");
    run.case("types/struct_default_initializer.cy");
    run.case("types/struct_default_initializer_error.cy");
    run.case("types/struct_init_undeclared_field_error.cy");
    run.case("types/struct_set_undeclared_field_error.cy");
    run.case("types/struct_require_field_error.cy");
    run.case("types/struct_nested.cy");
    run.case("types/template_choices.cy");
    run.case("types/template_dep_param_type.cy");
    run.case("types/template_dep_param_type_error.cy");
    run.case("types/template_object_init_noexpand_error.cy");
    run.case("types/template_object_spec_noexpand_error.cy");
    run.case("types/template_object_expand_error.cy");
    run.case("types/template_structs.cy");
    run.case("types/trait_error.cy");
    run.case("types/trait.cy");
    run.case("types/tuple.cy");
    run.case("types/type_alias.cy");
    run.case("types/type_alias_path_decl_error.cy");
    run.case("types/type_embedding.cy");
    run.case("types/type_spec.cy");
    run.case("types/void.cy");

    if (!is_wasm) {
        run.case("modules/type_spec.cy");
        run.case("modules/type_alias.cy");
        run.case("modules/import_not_found_error.cy");
        run.case("modules/import_missing_sym_error.cy");
        run.case("modules/import_rel_path.cy");
        run.case("modules/import_implied_rel_path.cy");
        run.case("modules/import_stmt_error.cy");
        run.case("modules/import_unresolved_rel_path.cy");
        
        // Import when running main script in the cwd.
        run.case2(Config.init("./import_rel_path.cy").withChdir("./src/test/modules"));
        // Import when running main script in a child directory.
        run.case2(Config.init("../import_rel_path.cy").withChdir("./src/test/modules/test_mods"));

        run.case("modules/import.cy");
        run.case("modules/import_all.cy");
        run.case("modules/import_sym_alias.cy");
    }
    run.case("modules/core.cy");
if (!aot) {
    run.case("modules/cy.cy");
}

    run.case("modules/math.cy");
    run.case("modules/meta.cy");
    run.case("modules/test_eq_panic.cy");
    run.case("modules/test.cy");
    if (!is_wasm) {
        run.case("modules/libc.cy");
        run.case("modules/os.cy");
        // run.case("modules/io.cy");
    }

    // Disabled test: printing to stdout hangs test runner.
    // run.case2(.{ .silent = true }, "meta/dump_locals.cy");
    run.case("meta/ct_if.cy");
    run.case("meta/get_panic.cy"); 
    run.case("meta/get_set.cy"); 
    run.case("meta/init_record.cy"); 
    run.case("meta/init_record_error.cy"); 
    run.case("meta/set_panic.cy"); 
    run.case("meta/type.cy");

if (!aot) {
    run.case("concurrency/await.cy");
    run.case("concurrency/generator.cy");
    run.case("concurrency/spawn.cy");
}

    run.case("core/Array.cy");
    run.case("core/Array_oob_panic.cy");
    run.case("core/Array_neg_oob_panic.cy");
    run.case("core/arithmetic_ops.cy");
    run.case("core/arithmetic_unsupported_error.cy");
    run.case("core/bool.cy");
    run.case("core/byte.cy");
    run.case("core/compare_eq.cy");
    run.case("core/compare_neq.cy");
    run.case("core/error_values.cy");
    run.case("core/escape_sequences.cy");
    run.case("core/floats.cy");
    run.case("core/ints.cy");
    run.case("core/int_unsupported_notation_error.cy");
    run.case("core/logic_ops.cy");
    run.case("core/map_index_panic.cy");
    run.case("core/Map.cy");
    run.case("core/op_precedence.cy");
    run.case("core/optionals_incompat_value_error.cy");
    run.case("core/optionals_unwrap_panic.cy");
    run.case("core/Option.cy");
    run.case("core/option_unwrap_block_reachable_error.cy");
    run.case("core/panic_panic.cy");
    run.case("core/PartialVector.cy");
    run.case("core/raw_string_single_quote_error.cy");
    run.case("core/raw_string_new_line_error.cy");
    run.case("core/Result.cy");
    run.case("core/result_unwrap_block_reachable_error.cy");
    run.case("core/result_infer_return.cy");
    run.case("core/rune_empty_lit_error.cy");
    run.case("core/rune_multiple_lit_error.cy");
    run.case("core/rune_grapheme_cluster_lit_error.cy");
    run.case("core/set_index_unsupported_error.cy");
    run.case("core/Slice.cy");
    run.case("core/Slice_oob_panic.cy");
    run.case("core/Slice_neg_oob_panic.cy");
    run.case("core/str_new_line_error.cy");
    run.case("core/str_interpolation.cy");
    run.case("core/str_runeAt_neg_oob_panic.cy");
    run.case("core/str_runeAt_oob_panic.cy");
    run.case("core/str_index_neg_oob_panic.cy");
    run.case("core/str_index_oob_panic.cy");
    run.case("core/strings.cy");
    run.case("core/strings_ascii.cy");
    run.case("core/strings_utf8.cy");
    run.case("core/symbols.cy");
    // run.case("core/table.cy");
    // run.case("core/table_access_panic.cy"); 
    run.case("core/Vector.cy");
    run.case("core/Vector_neg_oob_panic.cy");
    run.case("core/Vector_oob_panic.cy");
    run.case("core/wyhash.cy");

    run.case("vars/const.cy");
    run.case("vars/const_init_rtval_error.cy");
    run.case("vars/const_init_ctval_error.cy");
    run.case("vars/const_write_error.cy");
    run.case("vars/local_assign_error.cy");
    run.case("vars/local_assign.cy");
    run.case("vars/local_attr_error.cy");
    run.case("vars/local_dup_error.cy");
    run.case("vars/local_init.cy");
    run.case("vars/local_shadow.cy");
    run.case("vars/local_no_shadow_for_capture.cy");
    run.case("vars/op_assign.cy");
    run.case("vars/read_undeclared_error.cy");
    run.case("vars/read_undeclared_diff_scope_error.cy");
    run.case("vars/read_outside_if_var_error.cy");
    run.case("vars/read_outside_for_iter_error.cy");
    run.case("vars/read_outside_for_var_error.cy");
    run.case("vars/set_undeclared_error.cy");
    run.case("vars/global_assign.cy");
    run.case("vars/global_init.cy");
    // run.case("vars/global_init_call_error.cy");
    run.case("vars/global_init_capture_error.cy");
    run.case("vars/global_init_ref_error.cy");
    run.case("vars/global_init_type_error.cy");

    run.case("control_flow/for_iter.cy");
    run.case("control_flow/for_iter_unsupported_error.cy");
    run.case("control_flow/for_range.cy");
    run.case("control_flow/if_expr.cy");
    run.case("control_flow/if_expr_error.cy");
    run.case("control_flow/if_stmt.cy");
    run.case("control_flow/if_unwrap.cy");
    run.case("control_flow/return.cy");
    run.case("control_flow/switch.cy");
    run.case("control_flow/switch_error.cy");
    run.case("control_flow/switch_choice_else_error.cy");
    run.case("control_flow/switch_choice_dup_case_error.cy");
    run.case("control_flow/switch_choice_unhandled_error.cy");
    run.case("control_flow/try_error.cy");
    run.case("control_flow/try.cy");
    run.case("control_flow/try_panic.cy");
    run.case("control_flow/unreachable.cy");
    // run.case("control_flow/unreachable_error.cy");
    run.case("control_flow/while_cond.cy");
    run.case("control_flow/while_inf.cy");
    run.case("control_flow/while_unwrap.cy");

    run.case("../tokenizer.cy");

    var numPassed: u32 = 0;
    var skipped: u32 = 0;
    defer std.debug.print("Tests passed: {}/{}, skipped: {}\n", .{numPassed, run.cases.items.len, skipped});
    for (run.cases.items) |run_case| {
        if (caseFilter) |filter| {
            if (std.mem.indexOf(u8, run_case.path, filter) == null) {
                continue;
            }
        }
        std.debug.print("test: {s}\n", .{run_case.config.uri});

        if (run_case.skip) {
            skipped += 1;
            continue;
        }

        errdefer std.debug.print("failed test: {s}\n", .{run_case.config.uri});
        case2(run_case.config) catch |err| {
            std.debug.print("Failed: {}\n", .{err});
            if (failFast) {
                return err;
            } else {
                continue;
            }
        };
        numPassed += 1;
    }
    if (numPassed < run.cases.items.len) {
        return error.Failed;
    }
}

test "Compile." {
    // examples.
    // try compileCase(.{}, "../examples/fiber.cy");
    try compileCase("../examples/fizzbuzz.cy");
    try compileCase("../examples/hello.cy");
    try compileCase("../examples/ffi.cy");
    try compileCase("../examples/account.cy");
    try compileCase("../examples/fibonacci.cy");

    // tools.
    try compileCase("tools/bench.cy");
    try compileCase("tools/llvm.cy");
    try compileCase("tools/clang_bs.cy");
    try compileCase("tools/md4c.cy");
    if (!is_wasm) {
        try compileCase("tools/cbindgen.cy");
        try compileCase("../docs/gen-docs.cy");
        // try compileCase("jit/gen-stencils-a64.cy"); TODO: Re-enable.
        // try compileCase("jit/gen-stencils-x64.cy");
    }

    // benchmarks.
    try compileCase("test/bench/fib/fib.cy");
    // try compileCase("test/bench/fiber/fiber.cy"); TODO: Re-enable.
    try compileCase("test/bench/for/for.cy");
    // try compileCase("test/bench/heap/heap.cy"); TODO: Re-enable
    try compileCase("test/bench/string/index.cy");
}

fn compileCase(path: []const u8) !void {
    const fpath = try std.mem.concat(t.alloc, u8, &.{ "src/", path });
    defer t.alloc.free(fpath);
    try compile(.{ .uri = fpath, .silent = false }, null);
}

test "FFI." {
    if (is_wasm) {
        return;
    }

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
            \\@MainPath():3:4:
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

fn case(path: []const u8) !void {
    try case2(Config.init(path));
}

fn seekCommentEnd(str: []const u8, start: usize) usize {
    var idx = start;
    while (true) {
        if (str[idx..].len >= 3 and str[idx] == '\n' and str[idx+1] == '-' and str[idx+2] == '-') {
            idx += 1;
            if (std.mem.indexOfScalarPos(u8, str, idx, '\n')) |nl| {
                idx = nl;
            } else {
                idx = str.len;
            }
        } else {
            break;
        }
    }
    return idx;
}

fn case2(config: Config) !void {
    var contents: []const u8 = undefined;
    var final_path: []const u8 = undefined;
    if (config.chdir) |chdir| {
        const path = try std.fmt.allocPrint(t.alloc, "{s}/{s}", .{chdir, config.uri});
        defer t.alloc.free(path);
        contents = try std.fs.cwd().readFileAllocOptions(t.alloc, path, 1e9, null, .@"1", null);
        final_path = try t.alloc.dupe(u8, config.uri);
    } else {
        final_path = try std.mem.concat(t.alloc, u8, &.{"src/test/", config.uri});
        contents = try std.fs.cwd().readFileAllocOptions(t.alloc, final_path, 1e9, null, .@"1", null);
    }
    defer t.alloc.free(final_path);
    defer t.alloc.free(contents);

    var idx = std.mem.indexOf(u8, contents, "cytest:") orelse {
        return error.MissingTestDefinition;
    };

    var rest = contents[idx+7..];
    idx = std.mem.indexOfScalar(u8, rest, '\n') orelse rest.len;
    const test_t = std.mem.trim(u8, rest[0..idx], " ");

    if (std.mem.eql(u8, test_t, "error")) {
        const start = idx+1;
        const end = seekCommentEnd(rest, idx);
        const exp = rest[start..end];

        var buf: [1024]u8 = undefined;
        const len = std.mem.replacementSize(u8, exp, "--", "");
        _ = std.mem.replace(u8, exp, "--", "", &buf);

        const Context = struct {
            exp: []const u8,
        };
        var ctx = Context{ .exp = buf[0..len]};
        var fconfig: Config = config;
        fconfig.uri = final_path;
        fconfig.silent = true;
        fconfig.ctx = &ctx;
        try eval(fconfig, null
        , struct { fn func(run: *VMrunner, res: EvalResult) !void {
            const ctx_: *Context = @ptrCast(@alignCast(run.ctx));
            try run.expectErrorReport2(res, ctx_.exp, false);
        }}.func);
    } else if (std.mem.eql(u8, test_t, "panic")) {
        const start = idx;
        var buf: [1024]u8 = undefined;
        const len = std.mem.replacementSize(u8, rest[start..], "\n--", "\n");
        _ = std.mem.replace(u8, rest[start..], "\n--", "\n", &buf);
        const cur = buf[0..len];
        _ = cur;

        var exp_str: []const u8 = undefined;
        const backend = setup.fromTestBackend(test_config.test_backend);
        const aot = isAot(backend);

        const trace_idx = std.mem.indexOf(u8, buf[0..len], "[trace]") orelse {
            return error.MissingExpectedTrace;
        };
        if (!aot) {
            const starts_with = buf[1..trace_idx];
            const ends_with = buf[trace_idx+8..len];
            const Context2 = struct {
                exp_start: []const u8,
                exp_end: []const u8,
            };
            var ctx = Context2{ .exp_start = starts_with, .exp_end = ends_with };
            var fconfig: Config = config;
            fconfig.uri = final_path;
            fconfig.ctx = &ctx;
            try eval(fconfig, null
            , struct { fn func(run: *VMrunner, res: EvalResult) !void {
                const ctx_: *Context2 = @ptrCast(@alignCast(run.ctx));
                try run.expectErrorReport3(res, ctx_.exp_start, ctx_.exp_end);
            }}.func);
            return;
        } else {
            exp_str = buf[1..trace_idx-1];
        }

        const Context = struct {
            exp: []const u8,
        };
        var ctx = Context{ .exp = exp_str };
        var fconfig: Config = config;
        fconfig.uri = final_path;
        fconfig.silent = true;
        fconfig.ctx = &ctx;
        try eval(fconfig, null
        , struct { fn func(run: *VMrunner, res: EvalResult) !void {
            const ctx_: *Context = @ptrCast(@alignCast(run.ctx));
            const backend_ = setup.fromTestBackend(test_config.test_backend);
            const aot_ = isAot(backend_);
            try run.expectErrorReport2(res, ctx_.exp, aot_);
        }}.func);
    } else if (std.mem.eql(u8, test_t, "pass")) {
        var fconfig: Config = config;
        fconfig.silent = false;
        fconfig.uri = final_path;
        try evalPass(fconfig, null);
    } else {
        return error.UnsupportedTestType;
    }
}
