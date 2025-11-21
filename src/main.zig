const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const cy = @import("cyber.zig");
const c = @import("capi.zig");
const log = cy.log.scoped(.main);
const cli = @import("cli.zig");
const os_mod = @import("std/os.zig");
const build_options = @import("build_options");
const fmt = @import("fmt.zig");

comptime {
    const lib = @import("lib.zig");
    for (std.meta.declarations(lib)) |decl| {
        _ = &@field(lib, decl.name);
    }
}

var verbose = false;
var reload = false;
var backend: c.Backend = c.BackendVM;
var dumpStats = false; // Only for trace build.

const CP_UTF8 = 65001;
var prevWinConsoleOutputCP: u32 = undefined;

// Default VM.
var vm: cy.VM = undefined;

pub fn main() !void {
    cy.debug.attachSegfaultHandler(sig_handler);

    if (builtin.os.tag == .windows) {
        prevWinConsoleOutputCP = std.os.windows.kernel32.GetConsoleOutputCP();
        _ = std.os.windows.kernel32.SetConsoleOutputCP(CP_UTF8);
    }
    defer {
        if (builtin.os.tag == .windows) {
            _ = std.os.windows.kernel32.SetConsoleOutputCP(prevWinConsoleOutputCP);
        }
    }

    const alloc = cy.heap.getAllocator();
    defer cy.heap.deinitAllocator();

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
                if (cy.Trace) {
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

    const cvm: *c.VM = @ptrCast(&vm);
    try vm.init(alloc);
    cli.clInitCLI(&vm);
    defer {
        cli.clDeinitCLI(&vm);
        vm.deinit(false);
    }

    var config = c.defaultCompileConfig();
    config.single_run = builtin.mode == .ReleaseFast;
    config.file_modules = true;
    config.backend = backend;
    _ = vm.compile(path, null, config) catch |err| {
        if (err == error.CompileError) {
            if (!c.silent()) {
                const report = c.vm_compile_error_summary(cvm);
                defer c.vm_free(cvm, report);
                cy.debug.prints(report);
            }
            exit(1);
        } else {
            std.debug.panic("unexpected {}\n", .{err});
        }
    };
    try cy.debug.dumpBytecode(&vm, .{});
}

fn repl(alloc: std.mem.Allocator) !void {
    c.setVerbose(verbose);

    const cvm: *c.VM = @ptrCast(&vm);
    try vm.init(alloc);
    cli.clInitCLI(&vm);
    defer {
        cli.clDeinitCLI(&vm);
        vm.deinit(false);
    }

    var config = c.defaultEvalConfig();
    config.single_run = builtin.mode == .ReleaseFast;
    config.file_modules = true;
    config.reload = reload;
    config.backend = c.BackendVM;
    config.spawn_exe = false;

    const src =
        \\use cli
        \\
        \\cli.repl()
        \\
        ;
    _ = vm.eval("main", src, config) catch |err| {
        const thread = c.vm_main_thread(cvm);
        switch (err) {
            error.Panic => {
                if (!c.silent()) {
                    const report = c.thread_panic_summary(thread);
                    defer c.vm_free(cvm, report);
                    cy.debug.prints(report);
                }
            },
            error.CompileError => {
                if (!c.silent()) {
                    const report = c.vm_compile_error_summary(cvm);
                    defer c.vm_free(cvm, report);
                    cy.debug.prints(report);
                }
            },
            else => {
                std.debug.print("unexpected {}\n", .{err});
            },
        }
        if (builtin.mode == .Debug) {
            // Report error trace.
            return err;
        } else {
            // Exit early.
            exit(1);
        }
    };

    if (verbose) {
        std.debug.print("\n==VM Info==\n", .{});
        try vm.dumpInfo();
    }
    if (cy.Trace and dumpStats) {
        vm.dumpStats();
    }
    if (cy.TrackGlobalRC) {
        try vm.main_thread.checkGlobalRC();
    }
}

fn evalPath(alloc: std.mem.Allocator, path: []const u8) !void {
    c.setVerbose(verbose);

    const cvm: *c.VM = @ptrCast(&vm);
    try vm.init(alloc);
    cli.clInitCLI(&vm);
    defer {
        cli.clDeinitCLI(&vm);
        vm.deinit(false);
    }

    var config = c.defaultEvalConfig();
    config.single_run = builtin.mode == .ReleaseFast;
    config.file_modules = true;
    config.reload = reload;
    config.backend = backend;
    config.spawn_exe = true;
    const res = vm.eval(path, null, config) catch |err| {
        const thread = c.vm_main_thread(cvm);
        switch (err) {
            error.Panic => {
                if (!c.silent()) {
                    const report = c.thread_panic_summary(thread);
                    defer c.vm_free(cvm, report);
                    cy.debug.prints(report);
                }
            },
            error.CompileError => {
                if (!c.silent()) {
                    const report = c.vm_compile_error_summary(cvm);
                    defer c.vm_free(cvm, report);
                    cy.debug.prints(report);
                }
            },
            else => {
                std.debug.print("unexpected {}\n", .{err});
            },
        }
        if (builtin.mode == .Debug) {
            // Report error trace.
            return err;
        } else {
            // Exit early.
            exit(1);
        }
    };
    if (verbose) {
        if (res.res_t != cy.types.BuiltinTypes.Void) {
            const val_s = try cy.debug.alloc_value_desc(&vm, res.res_t, @ptrCast(res.res));
            defer vm.alloc.free(val_s);
            std.debug.print("\nmain return: {s}", .{val_s});
        }
        std.debug.print("\n==VM Info==\n", .{});
        try vm.dumpInfo();
    }
    if (cy.Trace and dumpStats) {
        vm.dumpStats();
    }
    if (cy.TrackGlobalRC) {
        if (res.res_t == cy.types.BuiltinTypes.Void) {
            try vm.main_thread.checkGlobalRC();
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
    , .{build_options.version});
}

fn version() void {
    std.debug.print("{s}\n", .{build_options.full_version});
}

pub fn panic(msg: []const u8, trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    _ = trace;
    _ = ret_addr;
    cy.debug.defaultPanic(msg, @returnAddress(), panic_handler);
}

fn panic_handler() !void {
    try cy.debug.vm_panic_handler(@ptrCast(@alignCast(&vm)));
}

fn sig_handler(sig: i32, info: *const std.posix.siginfo_t, ctx_ptr: ?*anyopaque) callconv(.c) noreturn {
    cy.debug.vm_segv_handler(@ptrCast(@alignCast(&vm))) catch |err| {
        std.debug.panic("failed during segfault: {}", .{err});
    };
    cy.debug.handleSegfaultPosix(sig, info, ctx_ptr);
}
