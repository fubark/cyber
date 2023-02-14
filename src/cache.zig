const std = @import("std");
const platform = @import("platform.zig");

/// Loaded on demand.
/// Once loaded, common sub directories are assumed to exist.
var CyberPath: []const u8 = "";

const CyberDir = ".cyber";
const EntriesDir = "entries";

fn getCyberPath(alloc: std.mem.Allocator) ![]const u8 {
    const S = struct {
        var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    };
    if (CyberPath.len == 0) {
        const homePath = (try platform.getPath(alloc, .home)) orelse return error.MissingHomeDir;
        defer alloc.free(homePath);
        std.mem.copy(u8, &S.buf, homePath);
        S.buf[homePath.len] = std.fs.path.sep;
        std.mem.copy(u8, S.buf[homePath.len+1..homePath.len+1+CyberDir.len], CyberDir);
        CyberPath = S.buf[0..homePath.len+1+CyberDir.len];

        // Also ensure sub directories exist.
        var dir = try std.fs.cwd().openDir(homePath, .{});
        defer dir.close();
        var cyberDir = try dir.makeOpenPath(CyberDir, .{});
        defer cyberDir.close();
        try cyberDir.makePath(EntriesDir);
    }
    return CyberPath;
}

fn toCacheSpec(spec: []const u8) ![]const u8 {
    // Remove scheme part.
    if (std.mem.startsWith(u8, spec, "http://")) {
        return spec[7..];
    } else if (std.mem.startsWith(u8, spec, "https://")) {
        return spec[8..];
    } else {
        return error.UnsupportedScheme;
    }
}

pub fn saveNewSpecFile(alloc: std.mem.Allocator, specGroup: SpecHashGroup, spec: []const u8, contents: []const u8) !void {
    const cacheSpec = try toCacheSpec(spec);
    const cyberPath = try getCyberPath(alloc);

    const now = @intCast(u64, std.time.timestamp());

    const filePath = try std.fs.path.join(alloc, &.{cyberPath, cacheSpec});
    defer alloc.free(filePath);

    // Ensure path exists.
    try std.fs.cwd().makePath(std.fs.path.dirname(filePath).?);

    const file = try std.fs.cwd().createFile(filePath, .{ .truncate = true, .exclusive = false });
    defer file.close();
    try file.writeAll(contents);

    const new = SpecEntry{
        .spec = cacheSpec,
        .cacheDate = now,
    };

    const path = try std.fs.path.join(alloc, &.{cyberPath, EntriesDir, &specGroup.hash});
    defer alloc.free(path);

    // Save spec entries.
    const entryFile = std.fs.cwd().createFile(path, .{ .truncate = true, .exclusive = false }) catch |err| {
        if (err == error.FileNotFound) {
            return error.ExpectedSpecFile;
        } else {
            return err;
        }
    };
    defer entryFile.close();
    for (specGroup.entries) |e| {
        try writeSpecEntry(entryFile, e);
    }
    try writeSpecEntry(entryFile, new);
}

fn writeSpecEntry(file: std.fs.File, entry: SpecEntry) !void {
    const w = file.writer();
    try std.fmt.format(w, "@{s}\n", .{entry.spec});
    try std.fmt.format(w, "cacheDate={}\n", .{entry.cacheDate});
}

/// Given absolute specifier, return the cached spec entries.
/// If the file does not exist, an empty slice is returned.
pub fn getSpecHashGroup(alloc: std.mem.Allocator, spec: []const u8) !SpecHashGroup {
    const cacheSpec = try toCacheSpec(spec);
    const hash = computeSpecHashStr(cacheSpec);
    const cyberPath = try getCyberPath(alloc);
    const path = try std.fs.path.join(alloc, &.{cyberPath, EntriesDir, &hash});
    defer alloc.free(path);
    const entries = readEntryFile(alloc, path) catch |err| {
        if (err == error.FileNotFound) {
            return SpecHashGroup{
                .hash = hash,
                .entries = &.{},
            };
        } else {
            return err;
        }
    };
    return SpecHashGroup{
        .hash = hash,
        .entries = entries,
    };
}

fn readEntryFile(alloc: std.mem.Allocator, path: []const u8) ![]SpecEntry {
    const content = try std.fs.cwd().readFileAlloc(alloc, path, 1e10);
    defer alloc.free(content);

    var entries: std.ArrayListUnmanaged(SpecEntry) = .{};
    defer entries.deinit(alloc);

    var iter = std.mem.tokenize(u8, content, "\r\n");
    while (iter.next()) |line| {
        if (line.len > 0 and line[0] == '@') {
            const spec = try alloc.dupe(u8, line[1..]);
            const bodyLine = iter.next() orelse return error.InvalidEntryFile;
            var bodyIter = std.mem.split(u8, bodyLine, ",");
            var entry = SpecEntry{
                .spec = spec,
                .cacheDate = 0,
            };
            while (bodyIter.next()) |field| {
                const idx = std.mem.indexOfScalar(u8, field, '=') orelse return error.InvalidEntryFile;
                if (std.mem.eql(u8, field[0..idx], "cacheDate")) {
                    if (entry.cacheDate > 0) {
                        return error.InvalidEntryFile;
                    }
                    entry.cacheDate = try std.fmt.parseInt(u64, field[idx+1..], 10);
                }
            }
            if (entry.cacheDate == 0) {
                return error.InvalidEntryFile;
            }
            try entries.append(alloc, entry);
        }
    }
    return entries.toOwnedSlice(alloc);
}

pub fn allocSpecFileContents(alloc: std.mem.Allocator, entry: SpecEntry) ![]const u8 {
    const cyberPath = try getCyberPath(alloc);
    const path = try std.fs.path.join(alloc, &.{cyberPath, entry.spec});
    defer alloc.free(path);
    return std.fs.cwd().readFileAlloc(alloc, path, 1e10);
}

fn computeSpecHashStr(spec: []const u8) [16]u8 {
    var res: [16]u8 = undefined;
    const hash = std.hash.Wyhash.hash(0, spec);
    _ = std.fmt.formatIntBuf(&res, hash, 16, .lower, .{});
    return res;
}

const SpecEntry = struct {
    /// Specifier name. Does not include the scheme.
    spec: []const u8,

    /// Unix timestamp (seconds) of when the file was cached.
    cacheDate: u64,

    fn deinit(self: *const SpecEntry, alloc: std.mem.Allocator) void {
        alloc.free(self.spec);
    }
};

const SpecHashGroup = struct {
    hash: [16]u8,
    entries: []const SpecEntry,

    pub fn deinit(self: *const SpecHashGroup, alloc: std.mem.Allocator) void {
        for (self.entries) |e| {
            e.deinit(alloc);
        }
        alloc.free(self.entries);
    }

    pub fn findEntryBySpec(self: *const SpecHashGroup, spec: []const u8) !?SpecEntry {
        const cacheSpec = try toCacheSpec(spec);
        for (self.entries) |e| {
            if (std.mem.eql(u8, e.spec, cacheSpec)) {
                return e;
            }
        }
        return null;
    }
};