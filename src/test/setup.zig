const std = @import("std");
const t = @import("stdx").testing;
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const test_config = @import("test_config");
const app_debug = @import("../app_debug.zig");
const cli = @import("../cli.zig");
const c = @import("../capi.zig");
const log = std.log.scoped(.setup);

pub fn fromTestBackend(backend: @TypeOf(test_config.test_backend)) c.Backend {
    return switch (backend) {
        .jit => c.BackendJIT,
        .vm => c.BackendVM,
        .tcc => c.BackendTCC,
        .cc => c.BackendCC,
    };
}

pub const Config = struct {
    uri: []const u8 = "src/test/main.cy",

    cli: bool = true,

    /// Don't print panic errors.
    silent: bool = true,

    // Whether to collectCycles at end of eval.
    cleanupGC: bool = false,

    preEval: ?*const fn (run: *VMrunner) void = null,

    debug: bool = false,

    reload: bool = false,

    ctx: ?*anyopaque = null,

    chdir: ?[]const u8 = null,

    pub fn withReload(self: Config) Config {
        var new = self;
        new.reload = true;
        return new;
    }

    pub fn withSilent(self: Config) Config {
        var new = self;
        new.silent = true;
        return new;
    }

    pub fn withDebug(self: Config) Config {
        var new = self;
        new.debug = true;
        return new;
    }

    pub fn withChdir(self: Config, dir: []const u8) Config {
        var new = self;
        new.chdir = dir;
        return new;
    }

    pub fn init(uri: []const u8) Config {
        return .{
            .uri = uri,
        };
    }
};

export fn test_init() void {
    if (!is_wasm) {
        app_debug.attachSegfaultHandler(sig_handler);
    }
}

fn sig_handler(sig: i32, info: *const std.posix.siginfo_t, ctx_ptr: ?*anyopaque) callconv(.c) noreturn {
    app_debug.handleSegfaultPosix(sig, info, ctx_ptr, sig_handler_inner);
}

fn sig_handler_inner() callconv(.c) void {
    if (test_vm) |vm| {
        app_debug.vm_segv_handler(@ptrCast(@alignCast(vm))) catch @panic("error");
    }
}

export fn zpanic(msg: [*]const u8, msg_len: usize, first_trace_addr: usize) noreturn {
    app_debug.defaultPanic(msg[0..msg_len], first_trace_addr, panic_handler);
}

fn panic_handler() !void {
    if (test_vm) |vm| {
        try app_debug.vm_panic_handler(@ptrCast(@alignCast(vm)));
    }
}

var test_vm: ?*c.VM = null;

fn vm_logger(_: ?*c.VM, str: c.Bytes) callconv(.c) void {
    std.debug.print("{s}\n", .{c.from_bytes(str)});
}

fn logger(str: c.Bytes) callconv(.c) void {
    std.debug.print("{s}\n", .{c.from_bytes(str)});
}

pub const VMrunner = struct {
    vm: *c.VM,
    ctx: ?*anyopaque = null,
    cli: bool,
    main_path: []const u8 = "",

    pub fn init(enable_cli: bool) !VMrunner {
        const vm = c.vm_initx(t.alloc);
        test_vm = vm;
        if (enable_cli) {
            try cli.init_cli(vm, t.alloc);
        } else {
            c.vm_set_logger(vm, vm_logger);
            c.set_logger(logger);
        }
        return .{
            .vm = @ptrCast(vm),
            .cli = enable_cli,
        };
    }

    pub fn deinit(self: *VMrunner) void {
        if (self.cli) {
            cli.deinit_cli(self.vm);
        }
        // std.debug.print("total ops: {}\n", .{self.main_trace().totalOpCounts});
        c.vm_deinit(self.vm);
        test_vm = null;
    }

    // pub fn main_trace(self: *VMrunner) *vmc.TraceInfo {
    //     return self.internal().main_thread.c.trace;
    // }

    pub fn expectErrorReport(self: *VMrunner, res: EvalResult, expErr: c.ResultCode, expReport: []const u8) !void {
        var errorMismatch = false;
        if (res.code == c.Success) {
            const val_dump = c.value_desc(self.vm, res.val_t, res.value);
            defer c.vm_freeb(self.vm, val_dump);
            std.debug.print("expected error.{s}, found: {s}\n", .{
                c.from_bytes(c.resultName(expErr)), val_dump,
            });
            return error.TestUnexpectedError;
        } else {
            if (res.code != expErr) {
                std.debug.print("expected error.{s}, found error.{s}\n", .{ c.from_bytes(c.resultName(expErr)), c.from_bytes(c.resultName(res.code)) });
                errorMismatch = true;
                // Continue to compare report.
            }
        }
        const report = try self.getErrorSummary(res.code);
        defer c.vm_freeb(self.vm, report);

        const final_exp = try expand_tmpl_macros(self.main_path, expReport);
        defer t.alloc.free(final_exp);
        try t.eqStr(final_exp, report);

        if (errorMismatch) {
            return error.TestUnexpectedError;
        }
    }

    pub fn expectErrorReport2(self: *VMrunner, res: EvalResult, expReport: []const u8, starts_with: bool) !void {
        const errorMismatch = false;
        if (res.code == c.Success) {
            const val_dump = c.value_desc(self.vm, res.val_t, res.value);
            defer c.vm_freeb(self.vm, val_dump);
            std.debug.print("expected error, found: {s}\n", .{ val_dump });
            return error.TestUnexpectedError;
        }
        // Continue to compare report.
        const report = try self.getErrorSummary(res.code);
        defer c.vm_freeb(self.vm, report);

        const report2 = try std.mem.replaceOwned(u8, t.alloc, report, "\x1b[31m", "");
        defer t.alloc.free(report2);
        const report3 = try std.mem.replaceOwned(u8, t.alloc, report2, "\x1b[0m", "");
        defer t.alloc.free(report3);

        if (starts_with) {
            try std.testing.expectStringStartsWith(expReport, report3);
        } else {
            const final_exp = try expand_tmpl_macros(self.main_path, expReport);
            defer t.alloc.free(final_exp);
            try t.eqStr(final_exp, report3);
        }

        if (errorMismatch) {
            return error.TestUnexpectedError;
        }
    }

    pub fn expectErrorReport3(self: *VMrunner, res: EvalResult, exp_start: []const u8, exp_end: []const u8) !void {
        const errorMismatch = false;
        if (res.code == c.Success) {
            const val_dump = c.value_desc(self.vm, res.val_t, res.value);
            defer c.vm_freeb(self.vm, val_dump);
            std.debug.print("expected error, found: {s}\n", .{ val_dump });
            return error.TestUnexpectedError;
        }
        // Continue to compare report.
        const report = try self.getErrorSummary(res.code);
        defer c.vm_freeb(self.vm, report);
        try std.testing.expectStringStartsWith(report, exp_start);

        const final_exp_end = try expand_tmpl_macros(self.main_path, exp_end);
        defer t.alloc.free(final_exp_end);
        try std.testing.expectStringEndsWith(report, final_exp_end);

        if (errorMismatch) {
            return error.TestUnexpectedError;
        }
    }

    fn getErrorSummary(self: *VMrunner, code: c.ResultCode) ![]const u8 {
        if (code == c.ErrorCompile) {
            return c.vm_compile_error_summary(self.vm);
        } else if (code == c.ErrorPanic) {
            const main_thread = c.vm_main_thread(self.vm);
            return c.thread_panic_summary(main_thread);
        }
        return error.Unsupported;
    }

    pub fn evalPass(run: *VMrunner, config: Config, src: []const u8) !void {
        return run.eval(config, src, null);
    }

    pub fn eval(run: *VMrunner, config: Config, opt_src: ?[]const u8, optCb: ?*const fn (*VMrunner, EvalResult) anyerror!void) !void {
        const vm = run.vm;
        defer {
            if (config.silent) {
                c.setSilent(false);
            }
            if (config.debug) {
                c.setVerbose(false);
                t.setLogLevel(.warn);
            }
        }
        if (config.silent) {
            c.setSilent(true);
        }
        if (config.debug) {
            c.setVerbose(true);
            t.setLogLevel(.debug);
        }

        // Set and restore cwd.
        var cwdBuf: [1024]u8 = undefined;
        const cwd = try std.posix.getcwd(&cwdBuf);
        if (config.chdir) |chdir| {
            try std.posix.chdir(chdir);
        }
        defer {
            if (config.chdir != null) {
                std.posix.chdir(cwd) catch @panic("error");
            }
        }

        run.ctx = config.ctx;
        if (config.preEval) |preEval| {
            preEval(run);
        }

        if (!is_wasm) {
            run.main_path = c.vm_resolve(vm, config.uri);
        } else {
            run.main_path = try std.fmt.allocPrint(t.alloc, "/{s}", .{config.uri});
        }
        defer {
            c.vm_freeb(vm, run.main_path);
        }

        var res: c.EvalResult = undefined;
        const c_config = c.EvalConfig{
            .single_run = false,
            .gen_all_debug_syms = config.debug,
            .backend = fromTestBackend(test_config.test_backend),
            .spawn_exe = false,
        };
        c.vm_reset(vm);
        var res_code: c.ResultCode = undefined;
        if (opt_src) |src| {
            res_code = c.vm_evalx(vm, config.uri, src, c_config, &res);
        } else {
            res_code = c.vm_eval_path(vm, config.uri, c_config, &res);
        }

        defer {
            const main_thread = c.vm_main_thread(vm);
            if (c.TRACE()) {
                const grc = c.thread_rc(main_thread);
                if (grc != 0) {
                    c.thread_dump_live_objects(main_thread);
                    std.debug.panic("unreleased refcount: {}", .{grc});
                }
            }

            const count = c.thread_count_objects(main_thread);
            if (count != 0) {
                c.thread_dump_live_objects(main_thread);
                std.debug.panic("unfreed objects: {}", .{count});
            }
        }

        if (optCb) |cb| {
            const res_ = EvalResult{
                .code = res_code,
                .value = @ptrCast(res.res),
                .val_t = res.res_t,
            };
            cb(run, res_) catch |err| {
                if (err == error.EvalError) {
                    errReport(vm, res_code);
                }
                return err;
            };
        } else {
            if (res_code == c.Success) {
            }
            if (res_code == c.Await) {
                // // Consume all ready tasks.
                // var cont_code = res_code;
                // while (cont_code == c.Await) {
                //     cont_code = vm.runReadyTasks();
                // }
                // if (cont_code != c.Success) {
                //     errReport(vm, cont_code);
                //     return error.EvalError;
                // }
                return error.EvalError;
            } else {
                if (res_code != c.Success) {
                    errReport(vm, res_code);
                    return error.EvalError;
                }
            }
        }
    }
};

pub fn compile(config: Config, opt_src: ?[]const u8) !void {
    var run = try VMrunner.init(config.cli);
    defer run.deinit();
    
    defer {
        if (config.silent) {
            c.setSilent(false);
        }
        if (config.debug) {
            c.setVerbose(false);
            t.setLogLevel(.warn);
        }
    }

    if (config.silent) {
        c.setSilent(true);
    }
    if (config.debug) {
        c.setVerbose(true);
        t.setLogLevel(.debug);
    }

    var compile_c = c.defaultCompileConfig();
    compile_c.single_run = false;
    // .genAllDebugSyms = config.debug,
    compile_c.backend = fromTestBackend(test_config.test_backend);

    var res_code: c.ResultCode = undefined;
    if (opt_src) |src| {
        res_code = c.vm_compile(run.vm, config.uri, src, compile_c);
    } else {
        res_code = c.vm_compile_path(run.vm, config.uri, compile_c);
    }
    if (res_code != c.Success) {
        errReport(run.vm, res_code);
        return error.CompileError;
    }
}

pub const EvalResult = struct {
    code: c.ResultCode,
    value: ?*c.Value,
    val_t: c.TypeId,

    pub fn getValueC(self: EvalResult) !*c.Value {
        if (self.code != c.Success) {
            return error.EvalError;
        }
        return self.value.?;
    }
};

pub fn eval(config: Config, src: ?[]const u8, optCb: ?*const fn (*VMrunner, EvalResult) anyerror!void) !void {
    var run = try VMrunner.init(config.cli);
    run.main_path = config.uri;
    defer {
        if (config.debug) {
            c.setVerbose(true);
        }
        defer {
            if (config.debug) {
                c.setVerbose(false);
            }
        }
        run.deinit();
    }
    try run.eval(config, src, optCb);
}

pub fn errReport(vm: *c.VM, code: c.ResultCode) void {
    if (c.silent()) {
        return;
    }
    switch (code) {
        c.ErrorPanic => {
            const main_thread = c.vm_main_thread(vm);
            const summary = c.thread_panic_summary(main_thread);
            defer c.vm_freeb(vm, summary);
            std.debug.print("{s}", .{summary});
        },
        c.ErrorCompile => {
            const summary = c.vm_compile_error_summary(vm);
            defer c.vm_freeb(vm, summary);
            std.debug.print("{s}", .{summary});
        },
        c.ErrorUnknown => {
            std.debug.print("Unknown error.\n", .{});
        },
        else => {},
    }
}

pub fn evalPass(config: Config, src: ?[]const u8) !void {
    var final_config = config;
    final_config.silent = false;
    return eval(final_config, src, null);
}

fn expand_tmpl_macros(main_path: []const u8, tmpl: []const u8) ![]const u8 {
    var cur = try t.alloc.dupe(u8, tmpl);
    while (true) {
        if (std.mem.indexOf(u8, cur, "@AbsPath(")) |idx| {
            if (std.mem.indexOfScalarPos(u8, cur, idx, ')')) |endIdx| {
                var buf: [1024]u8 = undefined;
                var w = std.Io.Writer.fixed(&buf);

                _ = try w.writeAll(cur[0..idx]);
                const basePath = try std.fs.realpathAlloc(t.alloc, ".");
                defer t.alloc.free(basePath);
                _ = try w.writeAll(basePath);
                try w.writeByte(std.fs.path.sep);

                const relPathStart = w.buffered().len;
                _ = try w.writeAll(cur[idx+9..endIdx]);
                if (builtin.os.tag == .windows) {
                    _ = std.mem.replaceScalar(u8, w.buffered()[relPathStart..], '/', '\\');
                }

                _ = try w.writeAll(cur[endIdx+1..]);
                t.alloc.free(cur);
                cur = try t.alloc.dupe(u8, w.buffered());
                continue;
            }
        }
        if (std.mem.indexOf(u8, cur, "@MainPath()")) |idx| {
            var buf: [1024]u8 = undefined;
            var w = std.Io.Writer.fixed(&buf);

            _ = try w.writeAll(cur[0..idx]);
            _ = try w.writeAll(main_path);
            _ = try w.writeAll(cur[idx+11..]);
            t.alloc.free(cur);
            cur = try t.alloc.dupe(u8, w.buffered());
            continue;
        }
        break;
    }
    return cur;
}
