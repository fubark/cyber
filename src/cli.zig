const std = @import("std");
const builtin = @import("builtin");
const cy = @import("cyber.zig");
const bindings = @import("builtins/bindings.zig");
const os_mod = @import("std/os.zig");
const test_mod = @import("std/test.zig");
const cache = @import("cache.zig");
const bt = cy.types.BuiltinTypes;
const v = cy.fmt.v;
const log = cy.log.scoped(.cli);

const builtins = std.ComptimeStringMap(void, .{
    .{"builtins"},
    .{"math"},
});

const stdMods = std.ComptimeStringMap(cy.ModuleLoaderResult, .{
    .{"os", cy.ModuleLoaderResult{
        .src = os_mod.Src,
        .srcLen = os_mod.Src.len,
        .varLoader = os_mod.varLoader,
        .funcLoader = os_mod.funcLoader,
        .typeLoader = os_mod.typeLoader,
        .postTypeLoad = os_mod.postTypeLoad,
        .postLoad = os_mod.postLoad,
    }},
    .{"test", cy.ModuleLoaderResult{
        .src = test_mod.Src,
        .srcLen = test_mod.Src.len,
        .funcLoader = test_mod.funcLoader,
        .postLoad = test_mod.postLoad,
    }},
});

pub fn setupVMForCLI(vm: *cy.UserVM) void {
    vm.setModuleResolver(resolve);
    vm.setModuleLoader(loader);
    vm.setPrint(print);
}

fn print(_: *cy.UserVM, str: cy.Str) callconv(.C) void {
    if (cy.isWasmFreestanding) {
        os_mod.hostFileWrite(1, str.buf, str.len);
        os_mod.hostFileWrite(1, "\n", 1);
    } else {
        const w = std.io.getStdOut().writer();
        w.writeAll(str.slice()) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn loader(uvm: *cy.UserVM, spec_: cy.Str, out: *cy.ModuleLoaderResult) callconv(.C) bool {
    const spec = spec_.slice();
    if (builtins.get(spec) != null) {
        return cy.vm_compiler.defaultModuleLoader(uvm, spec_, out);
    }
    if (stdMods.get(spec)) |res| {
        out.* = res;
        return true;
    }

    if (cy.isWasm) {
        return false;
    }

    const vm = uvm.internal();

    // Load from file or http.
    var src: []const u8 = undefined;
    if (std.mem.startsWith(u8, spec, "http://") or std.mem.startsWith(u8, spec, "https://")) {
        src = loadUrl(uvm, spec) catch |err| {
            if (err == error.HandledError) {
                return false;
            } else {
                const msg = cy.fmt.allocFormat(vm.alloc, "Can not load `{}`. {}", &.{v(spec), v(err)}) catch return false;
                defer vm.alloc.free(msg);
                uvm.setApiError(msg);
                return false;
            }
        };
    } else {
        src = std.fs.cwd().readFileAlloc(vm.alloc, spec, 1e10) catch |err| {
            const msg = cy.fmt.allocFormat(vm.alloc, "Can not load `{}`. {}", &.{v(spec), v(err)}) catch return false;
            defer vm.alloc.free(msg);
            uvm.setApiError(msg);
            return false;
        };
    }

    out.* = .{
        .src = src.ptr,
        .srcLen = src.len,
        .onReceipt = onModuleReceipt,
    };
    return true;
}

fn onModuleReceipt(uvm: *cy.UserVM, res: *cy.ModuleLoaderResult) callconv(.C) void {
    const alloc = uvm.allocator();
    alloc.free(res.src[0..res.srcLen]);
}

fn resolve(uvm: *cy.UserVM, chunkId: cy.ChunkId, curUri: cy.Str, spec_: cy.Str, res: *cy.ResolverResult) callconv(.C) bool {
    const spec = spec_.slice();
    if (builtins.get(spec) != null) {
        res.uri = spec_.buf;
        res.uriLen = spec_.len;
        return true;
    }
    if (stdMods.get(spec) != null) {
        res.uri = spec_.buf;
        res.uriLen = spec_.len;
        return true;
    }

    if (cy.isWasm) {
        return false;
    }

    const vm = uvm.internal();

    const uri = zResolve(uvm, chunkId, curUri.slice(), spec) catch |err| {
        if (err == error.HandledError) {
            return false;
        } else {
            const msg = cy.fmt.allocFormat(vm.alloc, "Resolve module `{}`, error: `{}`", &.{v(spec), v(err)}) catch return false;
            defer vm.alloc.free(msg);
            uvm.setApiError(msg);
            return false;
        }
    };

    res.uri = uri.ptr;
    res.uriLen = uri.len;
    return true;
}

fn zResolve(uvm: *cy.UserVM, chunkId: cy.ChunkId, curUri: []const u8, spec: []const u8) ![]const u8 {
    const vm = uvm.internal();
    const chunk = vm.compiler.chunks.items[chunkId];
    if (std.mem.startsWith(u8, spec, "http://") or std.mem.startsWith(u8, spec, "https://")) {
        const uri = try std.Uri.parse(spec);
        if (std.mem.endsWith(u8, uri.host.?, "github.com")) {
            if (std.mem.count(u8, uri.path, "/") == 2 and uri.path[uri.path.len-1] != '/') {
                chunk.tempBufU8.clearRetainingCapacity();
                try chunk.tempBufU8.appendSlice(vm.alloc, uri.scheme);
                try chunk.tempBufU8.appendSlice(vm.alloc, "://raw.githubusercontent.com");
                try chunk.tempBufU8.appendSlice(vm.alloc, uri.path);
                try chunk.tempBufU8.appendSlice(vm.alloc, "/master/mod.cy");
                std.debug.print("{s}\n", .{chunk.tempBufU8.items});
                return chunk.tempBufU8.items;
            }
        }
        return spec;
    }

    chunk.tempBufU8.clearRetainingCapacity();

    // Create path from the current script.
    // There should always be a parent directory since `curUri` should be absolute when dealing with file modules.
    const dir = std.fs.path.dirname(curUri) orelse return error.NoParentDir;
    try chunk.tempBufU8.ensureTotalCapacity(vm.alloc, dir.len + 1 + spec.len + std.fs.MAX_PATH_BYTES);
    try chunk.tempBufU8.appendSlice(vm.alloc, dir);
    try chunk.tempBufU8.append(vm.alloc, '/');
    try chunk.tempBufU8.appendSlice(vm.alloc, spec);
    const path = chunk.tempBufU8.items;

    // Get canonical path.
    chunk.tempBufU8.items.len += std.fs.MAX_PATH_BYTES;
    const absPath = std.fs.cwd().realpath(path, chunk.tempBufU8.items[path.len..]) catch |err| {
        if (err == error.FileNotFound) {
            const msg = try cy.fmt.allocFormat(vm.alloc, "Import path does not exist: `{}`", &.{v(path)});
            defer vm.alloc.free(msg);
            uvm.setApiError(msg);
            return error.HandledError;
        } else {
            return err;
        }
    };
    return absPath;
}

fn loadUrl(uvm: *cy.UserVM, url: []const u8) ![]const u8 {
    const vm = uvm.internal();
    const specGroup = try cache.getSpecHashGroup(vm.alloc, url);
    defer specGroup.deinit(vm.alloc);

    if (vm.config.reload) {
        // Remove cache entry.
        try specGroup.markEntryBySpecForRemoval(url);
    } else {
        // First check local cache.
        if (try specGroup.findEntryBySpec(url)) |entry| {
            var found = true;
            const src = cache.allocSpecFileContents(vm.alloc, entry) catch |err| b: {
                if (err == error.FileNotFound) {
                    // Fallthrough.
                    found = false;
                    break :b "";
                } else {
                    return err;
                }
            };
            if (found) {
                log.debug("Using cached {s}", .{url});
                return src;
            }
        }
    }

    const client = vm.httpClient;

    const uri = try std.Uri.parse(url);
    var req = client.request(.GET, uri, .{ .allocator = vm.alloc }) catch |err| {
        if (err == error.UnknownHostName) {
            const msg = try cy.fmt.allocFormat(vm.alloc, "Can not connect to `{}`.", &.{v(uri.host.?)});
            defer vm.alloc.free(msg);
            uvm.setApiError(msg);
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
            const err = try cy.fmt.allocFormat(vm.alloc, "Can not load `{}`. Response code: {}", &.{v(url), v(req.response.status)});
            defer vm.alloc.free(err);
            uvm.setApiError(err);
            return error.HandledError;
        },
    }

    var buf: std.ArrayListUnmanaged(u8) = .{};
    errdefer buf.deinit(vm.alloc);
    var readBuf: [4096]u8 = undefined;
    var read: usize = readBuf.len;

    while (read == readBuf.len) {
        read = try client.readAll(&req, &readBuf);
        try buf.appendSlice(vm.alloc, readBuf[0..read]);
    }

    // Cache to local.
    const entry = try cache.saveNewSpecFile(vm.alloc, specGroup, url, buf.items);
    entry.deinit(vm.alloc);

    return try buf.toOwnedSlice(vm.alloc);
}
