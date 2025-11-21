const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const cy = @import("cyber.zig");
const os_mod = @import("std/os.zig");
const io_mod = @import("std/io.zig");
const test_mod = @import("std/test.zig");
const cache = @import("cache.zig");
const C = @import("capi.zig");
const zErrFunc = cy.core.zErrFunc;
const v = cy.fmt.v;
const log = cy.log.scoped(.cli);

pub const Src = @embedFile("std/cli.cy");

const funcs = [_]struct{[]const u8, C.BindFunc}{
    .{"replReadLine", zErrFunc(replReadLine)},
};

pub fn bind(_: *cy.VM, mod: *cy.sym.Chunk) callconv(.c) void {
    for (funcs) |e| {
        C.mod_add_func(@ptrCast(mod), e.@"0", e.@"1");
    }
}

const ModuleInfo = struct {
    uri: []const u8,
    bind_fn: C.ModuleBindFn,
};

var mods: std.StringHashMapUnmanaged(ModuleInfo) = undefined;

comptime {
    if (!builtin.is_test or !build_options.link_test) {
        @export(&clInitCLI, .{ .name = "clInitCLI", .linkage = .strong });
        @export(&clDeinitCLI, .{ .name = "clDeinitCLI", .linkage = .strong });
    }
}

pub fn clInitCLI(vm: *cy.VM) callconv(.c) void {
    mods = .{};
    mods.putNoClobber(vm.alloc, "core", .{ .uri = "src/builtins/core.cy", .bind_fn = C.mod_core }) catch @panic("error");
    mods.putNoClobber(vm.alloc, "meta", .{ .uri = "src/builtins/meta.cy", .bind_fn = C.mod_meta }) catch @panic("error");
    mods.putNoClobber(vm.alloc, "math", .{ .uri = "src/std/math.cy", .bind_fn = C.mod_math }) catch @panic("error");
    mods.putNoClobber(vm.alloc, "cy", .{ .uri = "src/builtins/cy.cy", .bind_fn = C.mod_cy }) catch @panic("error");
    mods.putNoClobber(vm.alloc, "c", .{ .uri = "src/builtins/c.cy", .bind_fn = C.mod_c }) catch @panic("error");
    mods.putNoClobber(vm.alloc, "os", .{ .uri = "src/std/os.cy", .bind_fn = @ptrCast(&os_mod.bind) }) catch @panic("error");
    mods.putNoClobber(vm.alloc, "io", .{ .uri = "src/std/io.cy", .bind_fn = @ptrCast(&io_mod.bind) }) catch @panic("error");
    mods.putNoClobber(vm.alloc, "cli", .{ .uri = "src/std/cli.cy", .bind_fn = @ptrCast(&bind) }) catch @panic("error");
    mods.putNoClobber(vm.alloc, "test", .{ .uri = "src/std/test.cy", .bind_fn = @ptrCast(&test_mod.bind) }) catch @panic("error");

    C.set_resolver(@ptrCast(vm), resolve);
    C.vm_set_loader(@ptrCast(vm), loader);
    C.vm_set_printer(@ptrCast(vm), print);
    C.vm_set_eprinter(@ptrCast(vm), err);
    C.vm_set_logger(@ptrCast(vm), logFn);
}

pub fn clDeinitCLI(vm: *cy.VM) callconv(.c) void {
    mods.deinit(vm.alloc);
}

// Thread-safe.
fn logFn(_: ?*C.VM, str: C.Bytes) callconv(.c) void {
    if (cy.isWasmFreestanding) {
        os_mod.hostFileWrite(2, str.buf, str.len);
        os_mod.hostFileWrite(2, "\n", 1);
    } else {
        cy.debug.logs(C.from_bytes(str));
    }
}

// Not thread-safe.
fn err(_: ?*C.Thread, str: C.Bytes) callconv(.c) void {
    if (C.silent()) {
        return;
    }
    if (cy.isWasmFreestanding) {
        os_mod.hostFileWrite(2, str.buf, str.len);
    } else {
        std.fs.File.stderr().writeAll(C.from_bytes(str)) catch cy.fatal();
    }
}

// Not thread-safe.
fn print(_: ?*C.Thread, str: C.Bytes) callconv(.c) void {
    if (cy.isWasmFreestanding) {
        os_mod.hostFileWrite(1, str.buf, str.len);
    } else {
        // Temporarily redirect to error for tests to avoid hanging the Zig runner.
        if (builtin.is_test) {
            const slice = C.from_bytes(str);
            std.fs.File.stderr().writeAll(slice) catch cy.fatal();
            return;
        }

        const slice = C.from_bytes(str);
        std.fs.File.stdout().writeAll(slice) catch cy.fatal();
    }
}

pub fn loader(vm_: ?*C.VM, mod_: ?*C.Sym, spec_: C.Bytes, out: ?*C.Bytes) callconv(.c) bool {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const spec = C.from_bytes(spec_);

    var basename = std.fs.path.basename(spec);
    if (std.mem.lastIndexOfScalar(u8, basename, '.')) |idx| {
        basename = basename[0..idx];
    }
    if (mods.get(basename)) |info| {
        info.bind_fn.?(vm_, mod_);
    }

    if (cy.isWasm) {
        return false;
    }

    const alloc = vm.alloc;

    // Load from file or http.
    var src: []const u8 = undefined;
    if (std.mem.startsWith(u8, spec, "http://") or std.mem.startsWith(u8, spec, "https://")) {
        src = loadUrl(vm, alloc, spec) catch |e| {
            if (e == error.HandledError) {
                return false;
            } else {
                const msg = cy.fmt.allocFormat(alloc, "Can not load `{}`. {}", &.{ v(spec), v(e) }) catch return false;
                defer alloc.free(msg);
                C.reportApiError(@ptrCast(vm), C.to_bytes(msg));
                return false;
            }
        };
    } else {
        src = std.fs.cwd().readFileAlloc(alloc, spec, 1e10) catch |e| {
            const msg = cy.fmt.allocFormat(alloc, "Can not load `{}`. {}", &.{ v(spec), v(e) }) catch return false;
            defer alloc.free(msg);
            C.reportApiError(@ptrCast(vm), C.to_bytes(msg));
            return false;
        };
    }

    out.?.* = C.to_bytes(src);
    return true;
}

fn resolve(vm_: ?*C.VM, params: C.ResolverParams, res_uri_len: ?*usize) callconv(.c) bool {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    var uri = C.from_bytes(params.uri);

    if (cy.isWasm) {
        return false;
    }

    var cur_uri: []const u8 = "";
    var buf: [1024]u8 = undefined;
    if (mods.get(uri)) |info| {
        uri = info.uri;
        if (builtin.is_test) {
            var path = std.fs.selfExeDirPath(&buf) catch @panic("error");
            // Append dummy file to path since zResolve will use the parent directory.
            path.len += (std.fmt.bufPrint(buf[path.len..], "/../../../x", .{}) catch @panic("error")).len;
            cur_uri = path;
        } else {
            var path = std.fs.selfExeDirPath(&buf) catch @panic("error");
            buf[path.len] = '/';
            buf[path.len+1] = 'x';
            path.len += 2;
            cur_uri = path;
        }
    } else {
        if (params.chunkId != cy.NullId) {
            cur_uri = C.from_bytes(params.curUri);
        }
    }

    const r_uri = zResolve(vm, vm.alloc, params.chunkId, params.buf[0..params.bufLen], cur_uri, uri) catch |e| {
        if (e == error.HandledError) {
            return false;
        } else {
            const msg = cy.fmt.allocFormat(vm.alloc, "Resolve module `{}`, error: `{}`", &.{v(uri), v(e)}) catch return false;
            defer vm.alloc.free(msg);
            C.reportApiError(@ptrCast(vm), C.to_bytes(msg));
            return false;
        }
    };

    res_uri_len.?.* = r_uri.len;
    return true;
}

fn zResolve(vm: *cy.VM, alloc: std.mem.Allocator, chunkId: cy.ChunkId, buf: []u8, cur_uri: []const u8, spec: []const u8) ![]const u8 {
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
            const msg = try cy.fmt.allocFormat(alloc, "Import path does not exist: `{}`", &.{v(path)});
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

fn loadUrl(vm: *cy.VM, alloc: std.mem.Allocator, url: []const u8) ![]const u8 {
    const specGroup = try cache.getSpecHashGroup(alloc, url);
    defer specGroup.deinit(alloc);

    if (vm.config.reload) {
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
                    cy.debug.print("Using cached `{s}` at `{s}`.\n", .{url, cachePath});
                }
                return src;
            }
        }
    }

    const client = vm.httpClient;

    if (C.verbose()) {
        cy.debug.print("Fetching `{s}`.\n", .{url});
    }

    const uri = try std.Uri.parse(url);
    const res = client.fetch(vm.alloc, .GET, uri, .{}) catch |e| {
        if (e == error.UnknownHostName) {
            const msg = try cy.fmt.allocFormat(alloc, "Can not connect to `{}`.", &.{v(uri.host.?.percent_encoded)});
            defer alloc.free(msg);
            C.reportApiError(@ptrCast(vm), C.to_bytes(msg));
            return error.HandledError;
        } else {
            return e;
        }
    };
    errdefer vm.alloc.free(res.body);

    if (res.status != .ok) {
        // Stop immediately.
        const e = try cy.fmt.allocFormat(alloc, "Can not load `{}`. Response code: {}", &.{ v(url), v(res.status) });
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

pub fn replReadLine(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Str);

    const prefix = try t.alloc.dupeZ(u8, t.param(cy.heap.Str).slice());
    defer t.alloc.free(prefix);
    if (use_ln) {
        const line = try LnReadLine.read(undefined, prefix);
        defer LnReadLine.free(undefined, line);
        ret.* = try t.heap.init_str(line);
    } else {
        var read_line = FallbackReadLine{
            .alloc = t.alloc,
        };
        const line = try FallbackReadLine.read(&read_line, prefix);
        defer FallbackReadLine.free(&read_line, line);
        ret.* = try t.heap.init_str(line);
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
        try std.io.getStdOut().writeAll(prefix);
        return std.io.getStdIn().reader().readUntilDelimiterAlloc(self.alloc, '\n', 10e8);
    }

    pub fn free(ptr: *anyopaque, line: []const u8) void {
        const self: *@This() = @ptrCast(@alignCast(ptr));
        self.alloc.free(line);
    }
};
