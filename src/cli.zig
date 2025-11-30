const std = @import("std");
const zt = @import("stdx").testing;
const builtin = @import("builtin");
const build_config = @import("build_config");
const os_mod = @import("std/os.zig");
const test_mod = @import("std/test.zig");
const cache = @import("cache.zig");
const C = @import("capi.zig");
const log = std.log.scoped(.cli);
const http = @import("http.zig");
const NullId = std.math.maxInt(u32);
const vmc = @import("vmc");
const malloc = build_config.malloc;
const is_wasm = builtin.cpu.arch.isWasm();
const is_wasm_freestanding = is_wasm and builtin.os.tag == .freestanding;
const mi = @import("mimalloc");

pub const Src = @embedFile("std/cli.cy");

const funcs = [_]struct{[]const u8, C.BindFunc}{
    .{"replReadLine", zErrFunc(replReadLine)},
};

pub fn bind(_: *C.VM, mod: *C.Sym) callconv(.c) void {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }
}

const Config = struct {
    /// Whether url imports and cached assets should be reloaded.
    reload: bool = false,
};

pub const App = struct {
    alloc: std.mem.Allocator,
    config: Config,

    /// Interface used for imports and fetch.
    httpClient: http.HttpClient,
    stdHttpClient: *http.StdHttpClient,

    mod_root: []const u8,

    // Short name to abs path.
    mod_uris: std.StringHashMapUnmanaged([]const u8) = .{},

    // Abs path to bind function.
    mod_bindings: std.StringHashMapUnmanaged(C.ModuleBindFn) = .{},

    pub fn deinit(self: *App) void {
        var iter = self.mod_bindings.keyIterator();
        while (iter.next()) |abs_path| {
            self.alloc.free(abs_path.*);
        }
        self.mod_bindings.deinit(self.alloc);
        self.mod_uris.deinit(self.alloc);
        self.alloc.free(self.mod_root);
    }
};

pub fn init_cli(vm: *C.VM, alloc: std.mem.Allocator) !void {
    // Determine module root.
    var mod_root: []const u8 = undefined;
    if (build_config.mod_root) |mod_root_| {
        mod_root = try alloc.dupe(u8, mod_root_);
    } else {
        mod_root = try std.fs.selfExeDirPathAlloc(alloc);
    }

    const app = try alloc.create(App);
    app.* = .{
        .config = .{},
        .alloc = alloc,
        .httpClient = undefined,
        .stdHttpClient = undefined,
        .mod_root = mod_root,
    };

    // Init builtin module lookup tables.
    const ModInfo = struct {
        name: []const u8,
        uri: []const u8,
        bind_fn: C.ModuleBindFn,
    };
    const mods = [_]ModInfo{
        .{.name="core", .uri = "src/builtins/core.cy", .bind_fn = C.mod_bind_core },
        .{.name="meta", .uri = "src/builtins/meta.cy", .bind_fn = C.mod_bind_meta },
        .{.name="math", .uri = "src/std/math.cy", .bind_fn = C.mod_bind_math },
        .{.name="cy", .uri = "src/builtins/cy.cy", .bind_fn = C.mod_bind_cy },
        .{.name="c", .uri = "src/builtins/c.cy", .bind_fn = C.mod_bind_c },
        .{.name="libc", .uri = "src/std/libc.cy", .bind_fn = null },
        .{.name="os", .uri = "src/std/os.cy", .bind_fn = @ptrCast(&os_mod.bind) },
        .{.name="io", .uri = "src/std/io.cy", .bind_fn = C.mod_bind_io },
        .{.name="cli", .uri = "src/std/cli.cy", .bind_fn = @ptrCast(&bind) },
        .{.name="test", .uri = "src/std/test.cy", .bind_fn = C.mod_bind_test },
    };
    for (mods) |mod| {
        const abs_path = try std.fmt.allocPrint(alloc, "{s}/{s}", .{app.mod_root, mod.uri});
        try app.mod_uris.putNoClobber(alloc, mod.name, abs_path);
        try app.mod_bindings.putNoClobber(alloc, abs_path, mod.bind_fn);
    }

    app.stdHttpClient = try alloc.create(http.StdHttpClient);
    app.stdHttpClient.* = http.StdHttpClient.init(alloc);
    app.httpClient = app.stdHttpClient.iface();
    C.vm_set_user_data(vm, app);

    C.vm_set_resolver(vm, resolver);
    C.vm_set_dl_resolver(vm, dl_resolver);
    C.vm_set_loader(vm, loader);
    C.vm_set_printer(vm, print);
    C.vm_set_eprinter(vm, err_);
    C.vm_set_logger(vm, logFn);
    C.set_logger(global_logger);
}

pub fn deinit_cli(vm: *C.VM) void {
    const app: *App = @ptrCast(@alignCast(C.vm_user_data(vm)));
    app.deinit();
    const alloc = app.alloc;

    app.httpClient.deinit();
    alloc.destroy(app.stdHttpClient);

    alloc.destroy(app);
}

pub fn set_new_mock_http(vm: *C.VM) !*anyopaque {
    const app: *App = @ptrCast(@alignCast(C.vm_user_data(vm)));
    const client = try app.alloc.create(http.MockHttpClient);
    client.* = http.MockHttpClient.init(app.alloc);
    app.httpClient = client.iface();
    return client;
}

// Thread-safe.
fn logFn(_: ?*C.VM, str: C.Bytes) callconv(.c) void {
    std.debug.print("{s}\n", .{C.from_bytes(str)});
}

fn global_logger(str: C.Bytes) callconv(.c) void {
    std.debug.print("{s}\n", .{C.from_bytes(str)});
}

// Not thread-safe.
fn err_(_: ?*C.Thread, str: C.Bytes) callconv(.c) void {
    if (C.silent()) {
        return;
    }
    std.fs.File.stderr().writeAll(C.from_bytes(str)) catch @panic("error");
}

// Not thread-safe.
fn print(_: ?*C.Thread, str: C.Bytes) callconv(.c) void {
    // Temporarily redirect to error for tests to avoid hanging the Zig runner.
    if (builtin.is_test) {
        const slice = C.from_bytes(str);
        std.fs.File.stderr().writeAll(slice) catch @panic("error");
        return;
    }

    const slice = C.from_bytes(str);
    std.fs.File.stdout().writeAll(slice) catch @panic("error");
}

pub fn loader(vm_: ?*C.VM, mod_: ?*C.Sym, spec_: C.Bytes, res: [*c]C.LoaderResult) callconv(.c) bool {
    const vm = vm_.?;
    const spec = C.from_bytes(spec_);

    const app: *App = @ptrCast(@alignCast(C.vm_user_data(vm)));
    if (app.mod_bindings.get(spec)) |opt_binding| {
        if (opt_binding) |binding| {
            binding(vm_, mod_);
        }
    }

    if (is_wasm) {
        return false;
    }

    // Load from file or http.
    var src: []const u8 = undefined;
    if (std.mem.startsWith(u8, spec, "http://") or std.mem.startsWith(u8, spec, "https://")) {
        src = loadUrl(vm, app.alloc, spec) catch |e| {
            if (e == error.HandledError) {
                return false;
            } else {
                const msg = std.fmt.allocPrint(app.alloc, "Can not load `{s}`. {}", .{ spec, e }) catch return false;
                defer app.alloc.free(msg);
                C.reportApiError(@ptrCast(vm), C.to_bytes(msg));
                return false;
            }
        };
    } else {
        src = std.fs.cwd().readFileAlloc(app.alloc, spec, 1e10) catch |e| {
            const msg = std.fmt.allocPrint(app.alloc, "Can not load `{s}`. {}", .{ spec, e }) catch return false;
            defer app.alloc.free(msg);
            C.reportApiError(@ptrCast(vm), C.to_bytes(msg));
            return false;
        };
    }

    res[0] = .{
        .src = C.to_bytes(src),
        .manage_src = true,
    };
    return true;
}

fn dl_resolver(vm: ?*C.VM, uri: C.Bytes, out: [*c]C.Bytes) callconv(.c) bool {
    if (std.mem.startsWith(u8, C.from_bytes(uri), "https://")) {
        const final_path = os_mod.allocCacheUrl(vm.?, C.from_bytes(uri)) catch @panic("error");
        out[0] = C.to_bytes(final_path);
        return true;
    }
    const final_uri = C.vm_allocb(vm.?, uri.len);
    @memcpy(final_uri, C.from_bytes(uri));
    out[0] = C.to_bytes(final_uri);
    return true;
}

fn resolver(vm_: ?*C.VM, params: C.ResolverParams, res_uri_len: ?*usize) callconv(.c) bool {
    const vm: *C.VM = @ptrCast(vm_);
    const uri = C.from_bytes(params.uri);

    if (is_wasm) {
        return false;
    }

    const app: *App = @ptrCast(@alignCast(C.vm_user_data(vm)));

    var cur_uri: []const u8 = "";
    if (app.mod_uris.get(uri)) |abs_path| {
        const out = params.buf[0..params.bufLen];
        const res = std.fmt.bufPrint(out, "{s}", .{abs_path}) catch @panic("error");
        res_uri_len.?.* = res.len;
        return true;
    } else {
        if (params.chunkId != NullId) {
            cur_uri = C.from_bytes(params.curUri);
        }
    }

    const r_uri = zResolve(vm, app.alloc, params.chunkId, params.buf[0..params.bufLen], cur_uri, uri) catch |e| {
        if (e == error.HandledError) {
            return false;
        } else {
            const msg = std.fmt.allocPrint(app.alloc, "Resolve module `{s}`, error: `{}`", .{uri, e}) catch return false;
            defer app.alloc.free(msg);
            C.reportApiError(@ptrCast(vm), C.to_bytes(msg));
            return false;
        }
    };

    res_uri_len.?.* = r_uri.len;
    return true;
}

fn zResolve(vm: *C.VM, alloc: std.mem.Allocator, chunkId: u32, buf: []u8, cur_uri: []const u8, spec: []const u8) ![]const u8 {
    _ = chunkId;
    if (std.mem.startsWith(u8, spec, "http://") or std.mem.startsWith(u8, spec, "https://")) {
        const uri = try std.Uri.parse(spec);
        if (std.mem.endsWith(u8, uri.host.?.percent_encoded, "github.com")) {
            if (std.mem.count(u8, uri.path.percent_encoded, "/") == 2 and uri.path.percent_encoded[uri.path.percent_encoded.len - 1] != '/') {
                var fbuf = std.io.fixedBufferStream(buf);
                const w = fbuf.writer();

                try w.writeAll(uri.scheme);
                try w.writeAll("://raw.githubusercontent.com");
                try w.writeAll(uri.path.percent_encoded);
                try w.writeAll("/master/mod.cy");
                std.debug.print("{s}\n", .{fbuf.getWritten()});
                return fbuf.getWritten();
            }
        }
        return spec;
    }

    var pathBuf: [4096]u8 = undefined;
    var fbuf = std.io.fixedBufferStream(&pathBuf);
    if (cur_uri.len > 0) b: {
        // Create path from the current script.
        // There should always be a parent directory since `curUri` should be absolute when dealing with file modules.
        const dir = std.fs.path.dirname(cur_uri) orelse {
            // Likely from an in-memory chunk uri such as `main` or `input` (repl).
            _ = try fbuf.write(spec);
            break :b;
        };
        try fbuf.writer().print("{s}/{s}", .{ dir, spec });
    } else {
        _ = try fbuf.write(spec);
    }
    const path = fbuf.getWritten();

    // Get canonical path.
    const absPath = std.fs.cwd().realpath(path, buf) catch |e| {
        if (e == error.FileNotFound) {
            const msg = try std.fmt.allocPrint(alloc, "Import path does not exist: `{s}`", .{path});
            if (builtin.os.tag == .windows) {
                _ = std.mem.replaceScalar(u8, msg, '/', '\\');
            }
            defer alloc.free(msg);
            C.reportApiError(@ptrCast(vm), C.to_bytes(msg));
            return error.HandledError;
        } else {
            return e;
        }
    };
    return absPath;
}

fn loadUrl(vm: *C.VM, alloc: std.mem.Allocator, url: []const u8) ![]const u8 {
    const specGroup = try cache.getSpecHashGroup(alloc, url);
    defer specGroup.deinit(alloc);

    const a: *App = @ptrCast(@alignCast(C.vm_user_data(vm)));

    if (a.config.reload) {
        // Remove cache entry.
        try specGroup.markEntryBySpecForRemoval(url);
    } else {
        // First check local cache.
        if (try specGroup.findEntryBySpec(url)) |entry| {
            var found = true;
            const src = cache.allocSpecFileContents(alloc, entry) catch |e| b: {
                if (e == error.FileNotFound) {
                    // Fallthrough.
                    found = false;
                    break :b "";
                } else {
                    return e;
                }
            };
            if (found) {
                if (C.verbose()) {
                    const cachePath = try cache.allocSpecFilePath(alloc, entry);
                    defer alloc.free(cachePath);
                    std.debug.print("Using cached `{s}` at `{s}`.\n", .{url, cachePath});
                }
                return src;
            }
        }
    }

    const client = a.httpClient;

    if (C.verbose()) {
        std.debug.print("Fetching `{s}`.\n", .{url});
    }

    const uri = try std.Uri.parse(url);
    const res = client.fetch(alloc, .GET, uri, .{}) catch |e| {
        if (e == error.UnknownHostName) {
            const msg = try std.fmt.allocPrint(alloc, "Can not connect to `{s}`.", .{uri.host.?.percent_encoded});
            defer alloc.free(msg);
            C.reportApiError(@ptrCast(vm), C.to_bytes(msg));
            return error.HandledError;
        } else {
            return e;
        }
    };
    errdefer alloc.free(res.body);

    if (res.status != .ok) {
        // Stop immediately.
        const e = try std.fmt.allocPrint(alloc, "Can not load `{s}`. Response code: {}", .{ url, res.status });
        defer alloc.free(e);
        C.reportApiError(@ptrCast(vm), C.to_bytes(e));
        return error.HandledError;
    }

    // Cache to local.
    const entry = try cache.saveNewSpecFile(alloc, specGroup, url, res.body);
    entry.deinit(alloc);

    return res.body;
}

pub const use_ln = builtin.os.tag != .windows and builtin.os.tag != .wasi;
const ln = @import("linenoise");

pub fn replReadLine(t: *C.Thread) anyerror!C.Ret {
    const ret = C.thread_ret(t, C.str);

    const alloc = C.thread_allocator(t);

    var prefix = C.thread_param(t, C.str);
    defer C.str_deinit(t, &prefix);
    const prefix_dup = try alloc.dupeZ(u8, C.str_bytes(prefix));
    defer alloc.free(prefix_dup);
    if (use_ln) {
        const line = try LnReadLine.read(undefined, prefix_dup);
        defer LnReadLine.free(undefined, line);
        ret.* = C.str_init(t, line);
    } else {
        var read_line = FallbackReadLine{
            .alloc = alloc,
        };
        const line = try FallbackReadLine.read(&read_line, prefix_dup);
        defer FallbackReadLine.free(&read_line, line);
        ret.* = C.str_init(t, line);
    }
    return C.RetOk;
}

pub const LnReadLine = struct {
    pub fn read(ptr: *anyopaque, prefix: [:0]const u8) anyerror![]const u8 {
        _ = ptr;
        const linez = ln.linenoise(prefix);
        if (linez == null) {
            return error.EndOfStream;
        }
        _ = ln.linenoiseHistoryAdd(linez);
        return std.mem.sliceTo(linez, 0);
    }

    pub fn free(ptr: *anyopaque, line: []const u8) void {
        _ = ptr;
        ln.linenoiseFree(@constCast(line.ptr));
    }
};

pub const FallbackReadLine = struct {
    alloc: std.mem.Allocator,

    pub fn read(ptr: *anyopaque, prefix: [:0]const u8) anyerror![]const u8 {
        const self: *@This() = @ptrCast(@alignCast(ptr));
        try std.fs.File.stdout().writeAll(prefix);
        return std.fs.File.stdin().readToEndAllocOptions(self.alloc, 10e8, null, .of(u8), '\n');
    }

    pub fn free(ptr: *anyopaque, line: []const u8) void {
        const self: *@This() = @ptrCast(@alignCast(ptr));
        self.alloc.free(line);
    }
};

test "Stack traces." {
    if (!is_wasm) {
        const vm = C.vm_initx(zt.alloc);
        defer C.vm_deinit(vm);

        try init_cli(vm, zt.alloc);
        defer deinit_cli(vm);

        // panic from global init in another module.
        var res: C.EvalResult = undefined;
        const config = C.defaultEvalConfig();
        const code = C.vm_evalx(vm, "./src/test/main.cy",
            \\use a 'modules/test_mods/init_panic_error.cy'
            \\use t 'test'
            \\t.eq(a.foo, 123)
        , config, &res);

        try zt.eq(C.ErrorPanic, code);
        const thread = C.vm_main_thread(vm);
        {
            const report = C.thread_panic_summary(thread);
            defer C.vm_freeb(vm, report);

            const root_raw = build_config.mod_root.?;
            const root = if (builtin.os.tag == .windows)
                try std.mem.replaceOwned(u8, zt.alloc, root_raw, "\\", "/")
            else
                try zt.alloc.dupe(u8, root_raw);
            defer zt.alloc.free(root);

            const exp = try std.fmt.allocPrint(zt.alloc,
                \\panic: boom
                \\
                \\{s}/src/test/modules/test_mods/init_panic_error.cy:1:18 @init:
                \\global foo int = panic('boom')
                \\                 ^
                \\{s}/src/test/main.cy: @program_init
                \\{s}/src/test/main.cy: main
                \\
            , .{ root, root, root });
            defer zt.alloc.free(exp);

            const normalized_report = if (builtin.os.tag == .windows)
                try std.mem.replaceOwned(u8, zt.alloc, report, "\\", "/")
            else
                try zt.alloc.dupe(u8, report);
            defer zt.alloc.free(normalized_report);

            try zt.eqStr(exp, normalized_report);
        }

        const trace = C.thread_panic_trace(thread);
        try zt.eq(trace.frames_len, 3);
        try eqStackFrame(trace.frames[0], .{
            .name = C.to_bytes("@init"),
            .chunk = 3,
            .line = 0,
            .col = 17,
            .line_pos = 0,
        });
        try eqStackFrame(trace.frames[1], .{
            .name = C.to_bytes("@program_init"),
            .chunk = 0,
            .line = 0,
            .col = 0,
            .line_pos = NullId,
        });
        try eqStackFrame(trace.frames[2], .{
            .name = C.to_bytes("main"),
            .chunk = 0,
            .line = 0,
            .col = 0,
            .line_pos = NullId,
        });
    }
}

fn eqStackFrame(act: C.StackFrame, exp: C.StackFrame) !void {
    try zt.eqStr(C.from_bytes(act.name), C.from_bytes(exp.name));
    try zt.eq(exp.chunk, act.chunk);
    try zt.eq(exp.line, act.line);
    try zt.eq(exp.col, act.col);
    try zt.eq(exp.line_pos, act.line_pos);
}

// test "Import http spec." {
//     if (is_wasm) {
//         return;
//     }

//     var run = try VMrunner.init();
//     defer run.deinit();

//     const vm = run.internal();

//     const basePath = try std.fs.realpathAlloc(t.alloc, ".");
//     defer t.alloc.free(basePath);

//     // Import error.UnknownHostName.
//     var client: *http.MockHttpClient = @ptrCast(@alignCast(c.setNewMockHttp(@ptrCast(vm))));
//     client.retReqError = .UnknownHostName;
//     try run.eval(Config.initFileModules("./test/modules/import.cy").withSilent().withReload(),
//         \\use a 'https://doesnotexist123.com/'
//         \\b = a
//     , struct { fn func(run_: *VMrunner, res: EvalResult) !void {
//         try run_.expectErrorReport(res, c.ErrorCompile,
//             \\CompileError: Can not connect to `doesnotexist123.com`.
//             \\
//             \\@AbsPath(test/modules/import.cy):1:7:
//             \\use a 'https://doesnotexist123.com/'
//             \\      ^
//             \\
//         );
//     }}.func);
//     vm.alloc.destroy(client);

//     // Import NotFound response code.
//     client = @ptrCast(@alignCast(c.setNewMockHttp(@ptrCast(vm))));
//     client.retStatusCode = std.http.Status.not_found;
//     try run.eval(Config.initFileModules("./test/modules/import.cy").withSilent().withReload(),
//         \\use a 'https://exists.com/missing'
//         \\b = a
//     , struct { fn func(run_: *VMrunner, res: EvalResult) !void {
//         try run_.expectErrorReport(res, c.ErrorCompile,
//             \\CompileError: Can not load `https://exists.com/missing`. Response code: not_found
//             \\
//             \\@AbsPath(test/modules/import.cy):1:7:
//             \\use a 'https://exists.com/missing'
//             \\      ^
//             \\
//         );

//     }}.func);
//     vm.alloc.destroy(client);

//     // Successful import.
//     client = @ptrCast(@alignCast(c.setNewMockHttp(@ptrCast(vm))));
//     client.retBody =
//         \\var .foo = 123
//         ;
//     _ = try run.evalPass(Config.initFileModules("./test/modules/import.cy").withReload(),
//         \\use a 'https://exists.com/a.cy'
//         \\use t 'test'
//         \\t.eq(a.foo, 123)
//     );
//     vm.alloc.destroy(client);
// }

pub fn tracev(comptime format: []const u8, args: anytype) void {
    if (build_config.trace and C.verbose()) {
        std.debug.print(format, args);
    }
}

pub fn zErrFunc(comptime f: fn (t: *C.Thread) anyerror!C.Ret) C.BindFunc {
    const S = struct {
        pub fn genFunc(t: *C.Thread) callconv(.c) C.Ret {
            return @call(.always_inline, f, .{t}) catch |err| {
                return @call(.never_inline, prepPanicZError, .{t, err, @errorReturnTrace()});
            };
        }
    };
    return .{
        .kind = C.BindFuncVm,
        .ptr = @ptrCast(@constCast(&S.genFunc)),
    };
}

pub const isFreestanding = builtin.os.tag == .freestanding;

pub fn prepPanicZError(t: *C.Thread, err: anyerror, optTrace: ?*std.builtin.StackTrace) C.Ret {
    if (!isFreestanding and C.verbose()) {
        std.debug.print("{}", .{err});
        if (optTrace) |trace_| {
            std.debug.dumpStackTrace(trace_.*);
        }
    }
    return C.thread_ret_panic(t, @errorName(err));
}

var gpa: std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = false,
    .stack_trace_frames = if (builtin.mode == .Debug) 12 else 0,
}) = .{};
var miAlloc: mi.Allocator = undefined;
var trace_allocator: TraceAllocator = undefined;
var initedAllocator = false;

fn initAllocator() void {
    defer initedAllocator = true;
    switch (malloc) {
        .zig, .malloc => return,
        .mimalloc => {
            miAlloc.init();
        },
    }
    // var traceAlloc: stdx.heap.TraceAllocator = undefined;
    // traceAlloc.init(miAlloc.allocator());
    // traceAlloc.init(child);
    // defer traceAlloc.dump();
    // const alloc = traceAlloc.allocator();
}

pub fn getAllocator() std.mem.Allocator {
    if (!initedAllocator) {
        initAllocator();
    }
    switch (malloc) {
        .mimalloc => {
            return miAlloc.allocator();
        },
        .malloc => {
            return std.heap.c_allocator;
        },
        .zig => {
            if (builtin.is_test) {
                if (build_config.trace) {
                    trace_allocator.alloc_ = zt.alloc;
                    return trace_allocator.allocator();
                } else {
                    return zt.alloc;
                }
            }
            if (is_wasm) {
                return std.heap.wasm_allocator;
            } else {
                return gpa.allocator();
            }
        },
    }
}

pub fn deinitAllocator() void {
    switch (malloc) {
        .mimalloc => {
            miAlloc.deinit();
            initedAllocator = false;
        },
        .malloc => {
            return;
        },
        .zig => {
            if (is_wasm) {
                return;
            } else {
                _ = gpa.deinit();
                initedAllocator = false;
            }
        },
    }
}

// Uses a backing allocator and zeros the freed memory to surface UB more consistently.
pub const TraceAllocator = struct {
    alloc_: std.mem.Allocator,

    const vtable = std.mem.Allocator.VTable{
        .alloc = alloc,
        .resize = resize,
        .free = free,
        .remap = remap,
    };

    pub fn allocator(self: *TraceAllocator) std.mem.Allocator {
        return std.mem.Allocator{
            .ptr = self,
            .vtable = &vtable,
        };
    }

    fn alloc(ptr: *anyopaque, len: usize, log2_align: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *TraceAllocator = @ptrCast(@alignCast(ptr));
        return self.alloc_.rawAlloc(len, log2_align, ret_addr);
    }

    fn resize(ptr: *anyopaque, buf: []u8, log2_align: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *TraceAllocator = @ptrCast(@alignCast(ptr));
        return self.alloc_.rawResize(buf, log2_align, new_len, ret_addr);
    }

    fn remap(ptr: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *TraceAllocator = @ptrCast(@alignCast(ptr));
        return self.alloc_.rawRemap(memory, alignment, new_len, ret_addr);
    }

    fn free(ptr: *anyopaque, buf: []u8, log2_align: std.mem.Alignment, ret_addr: usize) void {
        const self: *TraceAllocator = @ptrCast(@alignCast(ptr));
        @memset(buf, 0);
        return self.alloc_.rawFree(buf, log2_align, ret_addr);
    }
};