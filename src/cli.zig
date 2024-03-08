const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const cy = @import("cyber.zig");
const os_mod = @import("std/os.zig");
const test_mod = @import("std/test.zig");
const cache = @import("cache.zig");
const c = @import("capi.zig");
const v = cy.fmt.v;
const log = cy.log.scoped(.cli);
const rt = cy.rt;

const builtins = std.ComptimeStringMap(void, .{
    .{"builtins"},
    .{"math"},
});

const os_res = c.ModuleLoaderResult{
    .src = os_mod.Src,
    .srcLen = os_mod.Src.len,
    .varLoader = os_mod.varLoader,
    .funcLoader = os_mod.funcLoader,
    .typeLoader = os_mod.typeLoader,
    .onTypeLoad = os_mod.onTypeLoad,
    .onLoad = os_mod.onLoad,
    .onReceipt = null,
    .onDestroy = null,
};

const test_res = c.ModuleLoaderResult{
    .src = test_mod.Src,
    .srcLen = test_mod.Src.len,
    .funcLoader = test_mod.funcLoader,
    .onLoad = test_mod.onLoad,
    .onReceipt = null,
    .varLoader = null,
    .typeLoader = null,
    .onDestroy = null,
    .onTypeLoad = null,
};

const stdMods = std.ComptimeStringMap(c.ModuleLoaderResult, .{
    .{"os", os_res},
    .{"test", test_res},
});

pub export fn csSetupForCLI(vm: *c.VM) void {
    c.setResolver(@ptrCast(vm), resolve);
    c.setModuleLoader(@ptrCast(vm), loader);
    c.setPrinter(@ptrCast(vm), print);
    c.setErrorPrinter(@ptrCast(vm), printError);
    c.setLog(logFn);
}

fn logFn(str: c.Str) callconv(.C) void {
    if (cy.isWasmFreestanding) {
        os_mod.hostFileWrite(2, str.buf, str.len);
        os_mod.hostFileWrite(2, "\n", 1);
    } else {
        const w = std.io.getStdErr().writer();
        w.writeAll(c.fromStr(str)) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

fn printError(_: ?*c.VM, str: c.Str) callconv(.C) void {
    if (c.silent()) {
        return;
    }
    if (cy.isWasmFreestanding) {
        os_mod.hostFileWrite(2, str.buf, str.len);
    } else {
        const w = std.io.getStdErr().writer();
        w.writeAll(c.fromStr(str)) catch cy.fatal();
    }
}

fn print(_: ?*c.VM, str: c.Str) callconv(.C) void {
    if (cy.isWasmFreestanding) {
        os_mod.hostFileWrite(1, str.buf, str.len);
    } else {
        // Temporarily redirect to error for tests to avoid hanging the Zig runner.
        if (builtin.is_test) {
            const w = std.io.getStdErr().writer();
            const slice = c.fromStr(str);
            w.writeAll(slice) catch cy.fatal();
            return;
        }

        const w = std.io.getStdOut().writer();
        const slice = c.fromStr(str);
        w.writeAll(slice) catch cy.fatal();
    }
}

pub fn loader(vm_: ?*c.VM, spec_: c.Str, out_: [*c]c.ModuleLoaderResult) callconv(.C) bool {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const out: *c.ModuleLoaderResult = out_;
    const spec = c.fromStr(spec_);
    if (builtins.get(spec) != null) {
        return c.defaultModuleLoader(vm_, spec_, out);
    }
    if (stdMods.get(spec)) |res| {
        out.* = res;
        return true;
    }

    if (cy.isWasm) {
        return false;
    }

    const alloc = vm.alloc;

    // Load from file or http.
    var src: []const u8 = undefined;
    if (std.mem.startsWith(u8, spec, "http://") or std.mem.startsWith(u8, spec, "https://")) {
        src = loadUrl(vm, alloc, spec) catch |err| {
            if (err == error.HandledError) {
                return false;
            } else {
                const msg = cy.fmt.allocFormat(alloc, "Can not load `{}`. {}", &.{v(spec), v(err)}) catch return false;
                defer alloc.free(msg);
                c.reportApiError(@ptrCast(vm), c.toStr(msg));
                return false;
            }
        };
    } else {
        src = std.fs.cwd().readFileAlloc(alloc, spec, 1e10) catch |err| {
            const msg = cy.fmt.allocFormat(alloc, "Can not load `{}`. {}", &.{v(spec), v(err)}) catch return false;
            defer alloc.free(msg);
            c.reportApiError(@ptrCast(vm), c.toStr(msg));
            return false;
        };
    }

    out.*.src = src.ptr;
    out.*.srcLen = src.len;
    out.*.onReceipt = onModuleReceipt;
    return true;
}

fn onModuleReceipt(vm_: ?*c.VM, res_: [*c]c.ModuleLoaderResult) callconv(.C) void {
    const vm: *c.VM = @ptrCast(vm_);
    const res: *c.ModuleLoaderResult = res_;
    c.free(vm, c.toSlice(res.src[0..res.srcLen]));
}

fn resolve(vm_: ?*c.VM, params: c.ResolverParams) callconv(.C) bool {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const spec = c.fromStr(params.spec);
    if (builtins.get(spec) != null) {
        params.resUri.* = spec.ptr;
        params.resUriLen.* = spec.len;
        return true;
    }
    if (stdMods.get(spec) != null) {
        params.resUri.* = spec.ptr;
        params.resUriLen.* = spec.len;
        return true;
    }

    if (cy.isWasm) {
        return false;
    }

    const alloc = vm.alloc;
    const uri = zResolve(vm, alloc, params.chunkId, params.buf[0..params.bufLen], c.fromStr(params.curUri), spec) catch |err| {
        if (err == error.HandledError) {
            return false;
        } else {
            const msg = cy.fmt.allocFormat(alloc, "Resolve module `{}`, error: `{}`", &.{v(spec), v(err)}) catch return false;
            defer alloc.free(msg);
            c.reportApiError(@ptrCast(vm), c.toStr(msg));
            return false;
        }
    };

    params.resUri.* = uri.ptr;
    params.resUriLen.* = uri.len;
    return true;
}

fn zResolve(vm: *cy.VM, alloc: std.mem.Allocator, chunkId: cy.ChunkId, buf: []u8, curUri: []const u8, spec: []const u8) ![]const u8 {
    _ = chunkId;
    if (std.mem.startsWith(u8, spec, "http://") or std.mem.startsWith(u8, spec, "https://")) {
        const uri = try std.Uri.parse(spec);
        if (std.mem.endsWith(u8, uri.host.?, "github.com")) {
            if (std.mem.count(u8, uri.path, "/") == 2 and uri.path[uri.path.len-1] != '/') {
                var fbuf = std.io.fixedBufferStream(buf);
                const w = fbuf.writer();

                try w.writeAll(uri.scheme);
                try w.writeAll("://raw.githubusercontent.com");
                try w.writeAll(uri.path);
                try w.writeAll("/master/mod.cy");
                std.debug.print("{s}\n", .{fbuf.getWritten()});
                return fbuf.getWritten();
            }
        }
        return spec;
    }

    // Create path from the current script.
    // There should always be a parent directory since `curUri` should be absolute when dealing with file modules.
    const dir = std.fs.path.dirname(curUri) orelse return error.NoParentDir;

    var pathBuf: [4096]u8 = undefined;
    var fbuf = std.io.fixedBufferStream(&pathBuf);
    try fbuf.writer().print("{s}/{s}", .{dir, spec});
    const path = fbuf.getWritten();

    // Get canonical path.
    const absPath = std.fs.cwd().realpath(path, buf) catch |err| {
        if (err == error.FileNotFound) {
            var msg = try cy.fmt.allocFormat(alloc, "Import path does not exist: `{}`", &.{v(path)});
            if (builtin.os.tag == .windows) {
                _ = std.mem.replaceScalar(u8, msg, '/', '\\');
            }
            defer alloc.free(msg);
            c.reportApiError(@ptrCast(vm), c.toStr(msg));
            return error.HandledError;
        } else {
            return err;
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
            const src = cache.allocSpecFileContents(alloc, entry) catch |err| b: {
                if (err == error.FileNotFound) {
                    // Fallthrough.
                    found = false;
                    break :b "";
                } else {
                    return err;
                }
            };
            if (found) {
                if (c.verbose()) {
                    const cachePath = try cache.allocSpecFilePath(alloc, entry);
                    defer alloc.free(cachePath);
                    rt.logZFmt("Using cached `{s}` at `{s}`", .{url, cachePath});
                }
                return src;
            }
        }
    }

    const client = vm.httpClient;

    if (c.verbose()) {
        rt.logZFmt("Fetching `{s}`.", .{url});
    }

    const uri = try std.Uri.parse(url);
    var req = client.request(.GET, uri, .{ .allocator = vm.alloc }) catch |err| {
        if (err == error.UnknownHostName) {
            const msg = try cy.fmt.allocFormat(alloc, "Can not connect to `{}`.", &.{v(uri.host.?)});
            defer alloc.free(msg);
            c.reportApiError(@ptrCast(vm), c.toStr(msg));
            return error.HandledError;
        } else {
            return err;
        }
    };
    defer client.deinitRequest(&req);

    try client.startRequest(&req);
    try client.waitRequest(&req);

    switch (req.response.status) {
        .ok => {
            // Whitelisted status codes.
        },
        else => {
            // Stop immediately.
            const err = try cy.fmt.allocFormat(alloc, "Can not load `{}`. Response code: {}", &.{v(url), v(req.response.status)});
            defer alloc.free(err);
            c.reportApiError(@ptrCast(vm), c.toStr(err));
            return error.HandledError;
        },
    }

    var buf: std.ArrayListUnmanaged(u8) = .{};
    errdefer buf.deinit(alloc);
    var readBuf: [4096]u8 = undefined;
    var read: usize = readBuf.len;

    while (read == readBuf.len) {
        read = try client.readAll(&req, &readBuf);
        try buf.appendSlice(alloc, readBuf[0..read]);
    }

    // Cache to local.
    const entry = try cache.saveNewSpecFile(alloc, specGroup, url, buf.items);
    entry.deinit(alloc);

    return try buf.toOwnedSlice(alloc);
}
