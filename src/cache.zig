const std = @import("std");
const platform = @import("platform.zig");

/// Loaded on demand.
/// Once loaded, common sub directories are assumed to exist.
var CyberPath: []const u8 = "";

const CyberDir = "cyber";
const EntriesDir = "entries";
const FilesDir = "files";

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
        try cyberDir.makePath(FilesDir);
    }
    return CyberPath;
}

pub fn saveNewSpecFile(alloc: std.mem.Allocator, specGroup: SpecHashGroup, spec: []const u8, contents: []const u8) !void {
    const cyberPath = try getCyberPath(alloc);

    const now = @intCast(u64, std.time.timestamp());
    // Obtain a name hash based on the spec, timestamp, and attempt count.
    var attempts: u64 = 0;
    const MaxAttempts = 10;
    var name: [16]u8 = undefined;
    while (attempts < MaxAttempts) : (attempts += 1) {
        name = computeFileNameHashStr(spec, now, attempts);
        const path = try std.fs.path.join(alloc, &.{cyberPath, FilesDir, &name});
        defer alloc.free(path);

        const file = std.fs.cwd().createFile(path, .{ .exclusive = true }) catch |err| {
            if (err == error.PathAlreadyExists) {
                continue;
            } else {
                return err;
            }
        };
        defer file.close();
        try file.writeAll(contents);
        break;
    }
    if (attempts == MaxAttempts) {
        return error.TooManyHashConflicts;
    }

    const new = SpecEntry{
        .spec = spec,
        .cacheDate = now,
        .fileName = &name,
    };

    const path = try std.fs.path.join(alloc, &.{cyberPath, EntriesDir, &specGroup.hash});
    defer alloc.free(path);

    // Save spec entries.
    const file = std.fs.cwd().createFile(path, .{ .truncate = true, .exclusive = false }) catch |err| {
        if (err == error.FileNotFound) {
            return error.ExpectedSpecFile;
        } else {
            return err;
        }
    };
    defer file.close();
    for (specGroup.entries) |e| {
        try writeSpecEntry(file, e);
    }
    try writeSpecEntry(file, new);
}

fn writeSpecEntry(file: std.fs.File, entry: SpecEntry) !void {
    const w = file.writer();
    try std.fmt.format(w, "@{s}\n", .{entry.spec});
    try std.fmt.format(w, "cacheDate={},fileName={s}\n", .{entry.cacheDate, entry.fileName});
}

/// Given absolute specifier, return the cached spec entries.
/// If the file does not exist, an empty slice is returned.
pub fn getSpecHashGroup(alloc: std.mem.Allocator, spec: []const u8) !SpecHashGroup {
    const hash = computeSpecHashStr(spec);
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
                .fileName = "",
            };
            while (bodyIter.next()) |field| {
                const idx = std.mem.indexOfScalar(u8, field, '=') orelse return error.InvalidEntryFile;
                if (std.mem.eql(u8, field[0..idx], "cacheDate")) {
                    if (entry.cacheDate > 0) {
                        return error.InvalidEntryFile;
                    }
                    entry.cacheDate = try std.fmt.parseInt(u64, field[idx+1..], 10);
                } else if (std.mem.eql(u8, field[0..idx], "fileName")) {
                    if (entry.fileName.len > 0) {
                        return error.InvalidEntryFile;
                    }
                    entry.fileName = try alloc.dupe(u8, field[idx+1..]);
                }
            }
            if (entry.cacheDate == 0) {
                return error.InvalidEntryFile;
            }
            if (entry.fileName.len != 16) {
                return error.InvalidEntryFile;
            }
            try entries.append(alloc, entry);
        }
    }
    return entries.toOwnedSlice(alloc);
}

pub fn allocFileContents(alloc: std.mem.Allocator, name: []const u8) ![]const u8 {
    const cyberPath = try getCyberPath(alloc);
    const path = try std.fs.path.join(alloc, &.{cyberPath, FilesDir, name});
    defer alloc.free(path);
    return std.fs.cwd().readFileAlloc(alloc, path, 1e10);
}

fn computeSpecHashStr(spec: []const u8) [16]u8 {
    var res: [16]u8 = undefined;
    const hash = std.hash.Wyhash.hash(0, spec);
    _ = std.fmt.formatIntBuf(&res, hash, 16, .lower, .{});
    return res;
}

fn computeFileNameHashStr(spec: []const u8, ts: u64, extra: u64) [16]u8 {
    var res: [16]u8 = undefined;
    var b = std.hash.Wyhash.init(0);
    @call(.always_inline, b.update, .{spec});
    @call(.always_inline, b.update, .{std.mem.asBytes(&ts)});
    @call(.always_inline, b.update, .{std.mem.asBytes(&extra)});
    const hash = @call(.always_inline, b.final, .{});
    _ = std.fmt.formatIntBuf(&res, hash, 16, .lower, .{});
    return res;
}

const SpecEntry = struct {
    /// Specifier name.
    spec: []const u8,

    /// Name of the cached file.
    fileName: []const u8,

    /// Unix timestamp (seconds) of when the file was cached.
    cacheDate: u64,

    fn deinit(self: *const SpecEntry, alloc: std.mem.Allocator) void {
        alloc.free(self.spec);
        alloc.free(self.fileName);
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

    pub fn findEntryBySpec(self: *const SpecHashGroup, spec: []const u8) ?SpecEntry {
        for (self.entries) |e| {
            if (std.mem.eql(u8, e.spec, spec)) {
                return e;
            }
        }
        return null;
    }
};