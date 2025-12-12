const std = @import("std");
const builtin = @import("builtin");
const build_config = @import("build_config");
const stdx = @import("stdx");
const app_debug = @import("app_debug.zig");
const c = @import("capi.zig");
const log = std.log.scoped(.main);
const cli = @import("cli.zig");
const os_mod = @import("std/os.zig");
const fmt = @import("fmt.zig");
const is_wasi = builtin.os.tag == .wasi;

test {
    std.testing.refAllDecls(cli);
}

var verbose = false;
var reload = false;
var backend: c.Backend = c.BackendVM;
var dumpStats = false; // Only for trace build.

const CP_UTF8 = 65001;
var prevWinConsoleOutputCP: u32 = undefined;

// Default VM.
var gvm: *c.VM = undefined;

pub fn main() !void {
    if (!is_wasi) {
        app_debug.attachSegfaultHandler(sig_handler);
    }

    if (builtin.os.tag == .windows) {
        prevWinConsoleOutputCP = std.os.windows.kernel32.GetConsoleOutputCP();
        _ = std.os.windows.kernel32.SetConsoleOutputCP(CP_UTF8);
    }
    defer {
        if (builtin.os.tag == .windows) {
            _ = std.os.windows.kernel32.SetConsoleOutputCP(prevWinConsoleOutputCP);
        }
    }

    const alloc = cli.getAllocator();
    defer cli.deinitAllocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    var cmd = Command.repl;
    var arg0: ?[]const u8 = null;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (arg[0] == '-') {
            if (std.mem.eql(u8, arg, "-v")) {
                verbose = true;
            } else if (std.mem.eql(u8, arg, "-r")) {
                reload = true;
            } else if (std.mem.eql(u8, arg, "-vm")) {
                backend = c.BackendVM;
            } else if (std.mem.eql(u8, arg, "-cc")) {
                backend = c.BackendCC;
            } else if (std.mem.eql(u8, arg, "-tcc")) {
                backend = c.BackendTCC;
            } else if (std.mem.eql(u8, arg, "-jit")) {
                backend = c.BackendJIT;
            } else if (std.mem.eql(u8, arg, "-h")) {
                cmd = .help;
            } else if (std.mem.eql(u8, arg, "--help")) {
                cmd = .help;
            } else {
                if (build_config.trace) {
                    if (std.mem.eql(u8, arg, "-stats")) {
                        dumpStats = true;
                        continue;
                    }
                }
                // Ignore unrecognized options so a script can use them.
            }
        } else {
            if (cmd == .repl) {
                if (std.mem.eql(u8, arg, "compile")) {
                    cmd = .compile;
                } else if (std.mem.eql(u8, arg, "fmt")) {
                    cmd = .fmt;
                } else if (std.mem.eql(u8, arg, "version")) {
                    cmd = .version;
                } else if (std.mem.eql(u8, arg, "help")) {
                    cmd = .help;
                } else {
                    cmd = .eval;
                    if (arg0 == null) {
                        arg0 = arg;
                        os_mod.argv_start = i;
                        break;
                    }
                }
            } else {
                if (arg0 == null) {
                    arg0 = arg;
                    break;
                }
            }
        }
    }

    switch (cmd) {
        .eval => {
            const path = arg0 orelse return error.MissingFilePath;
            try evalPath(alloc, path);
        },
        .compile => {
            const path = arg0 orelse return error.MissingFilePath;
            try compilePath(alloc, path);
        },
        .fmt => {
            std.debug.panic("TODO: Embed src/tools/fmt.cy", .{});
        },
        .help => {
            help();
        },
        .version => {
            version();
        },
        .repl => {
            try repl(alloc);
        },
    }
}

fn exit(code: u8) noreturn {
    if (builtin.os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(prevWinConsoleOutputCP);
    }
    std.posix.exit(code);
}

const Command = enum {
    eval,
    compile,
    fmt,
    help,
    version,
    repl,
};

fn compilePath(alloc: std.mem.Allocator, path: []const u8) !void {
    c.setVerbose(verbose);

    const vm = c.vm_initx(alloc);
    defer c.vm_deinit(vm);
    gvm = vm;
    try cli.init_cli(vm, alloc);
    defer cli.deinit_cli(vm);

    var config = c.defaultCompileConfig();
    config.single_run = builtin.mode == .ReleaseFast;
    config.backend = backend;
    const res = c.vm_compile_path(vm, path, config);
    if (res != c.Success) {
        switch (res) {
            c.ErrorCompile => {
                if (!c.silent()) {
                    const report = c.vm_compile_error_summary(vm);
                    defer c.vm_freeb(vm, report);
                    std.debug.print("{s}", .{report});
                }
                exit(1);
            },
            else => {
                std.debug.panic("unexpected {}\n", .{res});
            },
        }
    }
    c.vm_dump_bytecode(vm);
}

fn repl(alloc: std.mem.Allocator) !void {
    c.setVerbose(verbose);

    const vm: *c.VM = c.vm_initx(alloc);
    defer c.vm_deinit(vm);
    gvm = vm;

    try cli.init_cli(vm, alloc);
    defer cli.deinit_cli(vm);

    const a: *cli.App = @ptrCast(@alignCast(c.vm_user_data(vm)));
    a.config.reload = reload;

    var config = c.defaultEvalConfig();
    config.single_run = builtin.mode == .ReleaseFast;
    config.backend = c.BackendVM;
    config.spawn_exe = false;

    const src =
        \\use cli
        \\
        \\cli.repl()
        \\
        ;
    var eval_res: c.EvalResult = undefined;
    const res = c.vm_evalx(vm, "main", src, config, &eval_res);
    if (res != c.Success) {
        const thread = c.vm_main_thread(vm);
        switch (res) {
            c.ErrorPanic => {
                if (!c.silent()) {
                    const report = c.thread_panic_summary(thread);
                    defer c.vm_freeb(vm, report);
                    std.debug.print("{s}", .{report});
                }
            },
            c.ErrorCompile => {
                if (!c.silent()) {
                    const report = c.vm_compile_error_summary(vm);
                    defer c.vm_freeb(vm, report);
                    std.debug.print("{s}", .{report});
                }
            },
            else => {
                std.debug.print("unexpected {}\n", .{res});
            },
        }
        if (builtin.mode == .Debug) {
            return error.EvalError;
        } else {
            exit(1);
        }
    }

    if (verbose) {
        // std.debug.print("\n==VM Info==\n", .{});
        // try vm.dumpInfo();
    }
    const main_thread = c.vm_main_thread(vm);
    if (build_config.trace and dumpStats) {
        c.thread_dump_stats(main_thread);
    }
    if (c.TRACE()) {
        const grc = c.thread_rc(main_thread);
        if (grc != 0) {
            std.debug.print("unreleased refcount: {}\n", .{grc});
            c.thread_dump_live_objects(main_thread);
        }
    }
}

fn evalPath(alloc: std.mem.Allocator, path: []const u8) !void {
    c.setVerbose(verbose);

    const vm: *c.VM = c.vm_initx(alloc);
    defer c.vm_deinit(vm);
    gvm = vm;

    try cli.init_cli(vm, alloc);
    defer cli.deinit_cli(vm);

    const a: *cli.App = @ptrCast(@alignCast(c.vm_user_data(vm)));
    a.config.reload = reload;

    var config = c.defaultEvalConfig();
    config.single_run = builtin.mode == .ReleaseFast;
    config.backend = backend;
    config.spawn_exe = true;

    var eval_res: c.EvalResult = undefined;
    const res = c.vm_eval_path(vm, path, config, &eval_res);
    if (res != c.Success) {
        switch (res) {
            c.ErrorPanic => {
                if (!c.silent()) {
                    const thread = c.vm_main_thread(vm);
                    const report = c.thread_panic_summary(thread);
                    defer c.vm_freeb(vm, report);
                    std.debug.print("{s}", .{report});
                }
            },
            c.ErrorCompile => {
                if (!c.silent()) {
                    const report = c.vm_compile_error_summary(vm);
                    defer c.vm_freeb(vm, report);
                    std.debug.print("{s}", .{report});
                }
            },
            else => {
                std.debug.print("unexpected {}\n", .{res});
            },
        }
        if (builtin.mode == .Debug) {
            return error.EvalError;
        } else {
            exit(1);
        }
    }

    if (verbose) {
        if (eval_res.res_t != c.TypeVoid) {
            const val_s = c.value_desc(vm, eval_res.res_t, eval_res.res);
            defer c.vm_freeb(vm, val_s);
            std.debug.print("\nmain return: {s}", .{val_s});
        }
        // std.debug.print("\n==VM Info==\n", .{});
        // try c.vm_dump_info() vm.dumpInfo();
    }
    const main_thread = c.vm_main_thread(vm);
    if (build_config.trace and dumpStats) {
        c.thread_dump_stats(main_thread);
    }
    if (c.TRACE()) {
        const grc = c.thread_rc(main_thread);
        if (grc != 0) {
            std.debug.print("unreleased refcount: {}\n", .{grc});
            c.thread_dump_live_objects(main_thread);
        }
    }
}

fn help() void {
    std.debug.print(
        \\Cyber {s}
        \\
        \\Usage: cyber [command?] [options] [source]
        \\
        \\Commands:
        \\  cyber                   Run the REPL.
        \\  cyber [source]          Compile and run.
        \\  cyber compile [source]  Compile and dump the code.
        \\  cyber fmt [source]      Reformat the source code.
        \\  cyber help              Print usage.
        \\  cyber version           Print version number.
        \\
        \\Backend options:
        \\  -vm     Compile to bytecode. (Default)
        \\          Fast build, slow perf.
        \\  -cc     Compile to C with optimizations. Experimental.
        \\          Slow build, fast perf.
        \\  -tcc    Compile to C with builtin TinyC compiler. Experimental.
        \\          Mid build, mid perf.
        \\  -jit    Compile to machine code based on bytecode. Experimental.
        \\          Fast build, mid perf.
        \\
        \\General options:
        \\  -r      Refetch url imports and cached assets.
        \\  -v      Verbose.
        \\                            
        \\`cyber compile` options:
        \\
    , .{c.version()});
}

fn version() void {
    std.debug.print("{s}\n", .{c.full_version()});
}

pub fn panic(msg: []const u8, trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    _ = trace;
    _ = ret_addr;
    app_debug.defaultPanic(msg, @returnAddress(), panic_handler);
}

fn panic_handler() !void {
    try app_debug.vm_panic_handler(gvm);
}

fn sig_handler(sig: i32, info: *const std.posix.siginfo_t, ctx_ptr: ?*anyopaque) callconv(.c) noreturn {
    app_debug.handleSegfaultPosix(sig, info, ctx_ptr, sig_handler_inner);
}

fn sig_handler_inner() callconv(.c) void {
    app_debug.vm_segv_handler(gvm) catch |err| {
        std.debug.panic("failed during segfault: {}", .{err});
    };
}
