const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const cy = @import("cyber.zig");
const os_mod = @import("std/os.zig");
const test_mod = @import("std/test.zig");
const cache = @import("cache.zig");
const C = @import("capi.zig");
const zErrFunc = cy.builtins.zErrFunc;
const v = cy.fmt.v;
const log = cy.log.scoped(.cli);
const rt = cy.rt;

const builtins = std.StaticStringMap(void).initComptime(.{
    .{"core"},
    .{"math"},
    .{"cy"},
});

const Src = @embedFile("std/cli.cy");

const func = cy.hostFuncEntry;
const funcs = [_]C.HostFuncEntry{
    func("replReadLine", zErrFunc(replReadLine)),
};

pub fn create(vm: *cy.VM, r_uri: []const u8) C.Module {
    const mod = C.createModule(@ptrCast(vm), C.toStr(r_uri), C.toStr(Src));
    var config = C.ModuleConfig{
        .funcs = C.toSlice(C.HostFuncEntry, &funcs),
    };
    C.setModuleConfig(@ptrCast(vm), mod, &config);
    return mod;
}

const stdMods = std.StaticStringMap(*const fn (*cy.VM, r_uri: []const u8) C.Module).initComptime(.{
    .{ "cli", create },
    .{ "os", os_mod.create },
    .{ "test", test_mod.create },
});

pub const CliData = struct {
    FileT: cy.TypeId,
    DirT: cy.TypeId,
    DirIterT: cy.TypeId,
    CArrayT: cy.TypeId,
    CDimArrayT: cy.TypeId,
    FFIT: cy.TypeId,
};

comptime {
    if (!builtin.is_test or !build_options.link_test) {
        @export(clInitCLI, .{ .name = "clInitCLI", .linkage = .strong });
        @export(clDeinitCLI, .{ .name = "clDeinitCLI", .linkage = .strong });
    }
}

pub fn clInitCLI(vm: *cy.VM) callconv(.C) void {
    C.setResolver(@ptrCast(vm), resolve);
    C.setModuleLoader(@ptrCast(vm), loader);
    C.setPrinter(@ptrCast(vm), print);
    C.setErrorPrinter(@ptrCast(vm), err);
    C.setLog(logFn);

    const cli_data = vm.alloc.create(CliData) catch @panic("error");
    vm.data.put(vm.alloc, "cli", cli_data) catch @panic("error");
}

pub fn clDeinitCLI(vm: *cy.VM) callconv(.C) void {
    const cli_data = vm.getData(*CliData, "cli");
    vm.alloc.destroy(cli_data);
}

fn logFn(str: C.Str) callconv(.C) void {
    if (cy.isWasmFreestanding) {
        os_mod.hostFileWrite(2, str.buf, str.len);
        os_mod.hostFileWrite(2, "\n", 1);
    } else {
        const w = std.io.getStdErr().writer();
        w.writeAll(C.fromStr(str)) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

fn err(_: ?*C.VM, str: C.Str) callconv(.C) void {
    if (C.silent()) {
        return;
    }
    if (cy.isWasmFreestanding) {
        os_mod.hostFileWrite(2, str.buf, str.len);
    } else {
        const w = std.io.getStdErr().writer();
        w.writeAll(C.fromStr(str)) catch cy.fatal();
    }
}

fn print(_: ?*C.VM, str: C.Str) callconv(.C) void {
    if (cy.isWasmFreestanding) {
        os_mod.hostFileWrite(1, str.buf, str.len);
    } else {
        // Temporarily redirect to error for tests to avoid hanging the Zig runner.
        if (builtin.is_test) {
            const w = std.io.getStdErr().writer();
            const slice = C.fromStr(str);
            w.writeAll(slice) catch cy.fatal();
            return;
        }

        const w = std.io.getStdOut().writer();
        const slice = C.fromStr(str);
        w.writeAll(slice) catch cy.fatal();
    }
}

pub fn loader(vm_: ?*C.VM, spec_: C.Str, res: ?*C.Module) callconv(.C) bool {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const spec = C.fromStr(spec_);
    if (builtins.get(spec) != null) {
        return C.defaultModuleLoader(vm_, spec_, res);
    }
    if (stdMods.get(spec)) |create_fn| {
        res.?.* = create_fn(vm, spec);
        return true;
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
                C.reportApiError(@ptrCast(vm), C.toStr(msg));
                return false;
            }
        };
    } else {
        src = std.fs.cwd().readFileAlloc(alloc, spec, 1e10) catch |e| {
            const msg = cy.fmt.allocFormat(alloc, "Can not load `{}`. {}", &.{ v(spec), v(e) }) catch return false;
            defer alloc.free(msg);
            C.reportApiError(@ptrCast(vm), C.toStr(msg));
            return false;
        };
    }

    const mod = C.createModule(vm_, C.toStr(spec), C.toStr(src));
    alloc.free(src);
    res.?.* = mod;
    return true;
}

fn resolve(vm_: ?*C.VM, params: C.ResolverParams) callconv(.C) bool {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const uri = C.fromStr(params.uri);
    if (builtins.get(uri) != null) {
        params.resUri.* = uri.ptr;
        params.resUriLen.* = uri.len;
        return true;
    }
    if (stdMods.get(uri) != null) {
        params.resUri.* = uri.ptr;
        params.resUriLen.* = uri.len;
        return true;
    }

    if (cy.isWasm) {
        return false;
    }

    var cur_uri: ?[]const u8 = null;
    if (params.chunkId != cy.NullId) {
        cur_uri = C.fromStr(params.curUri);
    }

    const alloc = vm.alloc;
    const r_uri = zResolve(vm, alloc, params.chunkId, params.buf[0..params.bufLen], cur_uri, uri) catch |e| {
        if (e == error.HandledError) {
            return false;
        } else {
            const msg = cy.fmt.allocFormat(alloc, "Resolve module `{}`, error: `{}`", &.{ v(uri), v(e) }) catch return false;
            defer alloc.free(msg);
            C.reportApiError(@ptrCast(vm), C.toStr(msg));
            return false;
        }
    };

    params.resUri.* = r_uri.ptr;
    params.resUriLen.* = r_uri.len;
    return true;
}

fn zResolve(vm: *cy.VM, alloc: std.mem.Allocator, chunkId: cy.ChunkId, buf: []u8, cur_uri_opt: ?[]const u8, spec: []const u8) ![]const u8 {
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
    if (cur_uri_opt) |cur_uri| b: {
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
            C.reportApiError(@ptrCast(vm), C.toStr(msg));
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
                    rt.logZFmt("Using cached `{s}` at `{s}`", .{ url, cachePath });
                }
                return src;
            }
        }
    }

    const client = vm.httpClient;

    if (C.verbose()) {
        rt.logZFmt("Fetching `{s}`.", .{url});
    }

    const uri = try std.Uri.parse(url);
    const res = client.fetch(vm.alloc, .GET, uri, .{}) catch |e| {
        if (e == error.UnknownHostName) {
            const msg = try cy.fmt.allocFormat(alloc, "Can not connect to `{}`.", &.{v(uri.host.?.percent_encoded)});
            defer alloc.free(msg);
            C.reportApiError(@ptrCast(vm), C.toStr(msg));
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
        C.reportApiError(@ptrCast(vm), C.toStr(e));
        return error.HandledError;
    }

    // Cache to local.
    const entry = try cache.saveNewSpecFile(alloc, specGroup, url, res.body);
    entry.deinit(alloc);

    return res.body;
}

pub const use_ln = builtin.os.tag != .windows and builtin.os.tag != .wasi;
const ln = @import("linenoise");

pub fn replReadLine(vm: *cy.VM) anyerror!cy.Value {
    const prefix = try vm.alloc.dupeZ(u8, vm.getString(0));
    defer vm.alloc.free(prefix);
    if (use_ln) {
        const line = try LnReadLine.read(undefined, prefix);
        defer LnReadLine.free(undefined, line);
        return vm.allocString(line);
    } else {
        var read_line = FallbackReadLine{
            .alloc = vm.alloc,
        };
        const line = try FallbackReadLine.read(&read_line, prefix);
        defer FallbackReadLine.free(&read_line, line);
        return vm.allocString(line);
    }
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
