const std = @import("std");
const builtin = @import("builtin");

/// Extracted from https://github.com/ziglibs/known-folders/blob/master/known-folders.zig

pub const KnownFolder = enum {
    home,
};

pub const Error = error{ ParseError, OutOfMemory };

/// Returns the path to the folder or, if the folder does not exist, `null`.
pub fn getPath(allocator: std.mem.Allocator, folder: KnownFolder) Error!?[]const u8 {
    // used for temporary allocations
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    switch (builtin.os.tag) {
        .windows => {
            const folder_spec = windows_folder_spec.get(folder);

            switch (folder_spec) {
                .by_guid => |guid| {
                    var dir_path_ptr: [*:0]u16 = undefined;
                    switch (std.os.windows.shell32.SHGetKnownFolderPath(
                        &guid,
                        std.os.windows.KF_FLAG_CREATE, // TODO: Chose sane option here?
                        null,
                        &dir_path_ptr,
                    )) {
                        std.os.windows.S_OK => {
                            defer std.os.windows.ole32.CoTaskMemFree(@ptrCast(*anyopaque, dir_path_ptr));
                            const global_dir = std.unicode.utf16leToUtf8Alloc(allocator, std.mem.span(dir_path_ptr)) catch |err| switch (err) {
                                error.UnexpectedSecondSurrogateHalf => return null,
                                error.ExpectedSecondSurrogateHalf => return null,
                                error.DanglingSurrogateHalf => return null,
                                error.OutOfMemory => return error.OutOfMemory,
                            };
                            return global_dir;
                        },
                        std.os.windows.E_OUTOFMEMORY => return error.OutOfMemory,
                        else => return null,
                    }
                },
                .by_env => |env_path| {
                    if (env_path.subdir) |sub_dir| {
                        const root_path = std.process.getEnvVarOwned(arena.allocator(), env_path.env_var) catch |err| switch (err) {
                            error.EnvironmentVariableNotFound => return null,
                            error.InvalidUtf8 => return null,
                            error.OutOfMemory => |e| return e,
                        };
                        return try std.fs.path.join(allocator, &[_][]const u8{ root_path, sub_dir });
                    } else {
                        return std.process.getEnvVarOwned(allocator, env_path.env_var) catch |err| switch (err) {
                            error.EnvironmentVariableNotFound => return null,
                            error.InvalidUtf8 => return null,
                            error.OutOfMemory => |e| return e,
                        };
                    }
                },
            }
        },
        .macos => {
            switch (mac_folder_spec.get(folder)) {
                .absolute => |abs| {
                    return try allocator.dupe(u8, abs);
                },
                .suffix => |s| {
                    const home_dir = if (std.os.getenv("HOME")) |home|
                        home
                    else
                        return null;

                    if (s) |suffix| {
                        return try std.fs.path.join(allocator, &[_][]const u8{ home_dir, suffix });
                    } else {
                        return try allocator.dupe(u8, home_dir);
                    }
                },
            }
        },

        // Assume unix derivatives with XDG
        else => {
            return getPathXdg(allocator, &arena, folder);
        },
    }
    unreachable;
}

fn getPathXdg(allocator: std.mem.Allocator, arena: *std.heap.ArenaAllocator, folder: KnownFolder) Error!?[]const u8 {
    const folder_spec = xdg_folder_spec.get(folder);

    var env_opt = std.os.getenv(folder_spec.env.name);

    // TODO: add caching so we only need to read once in a run
    if (env_opt == null and folder_spec.env.user_dir) block: {
        const config_dir_path = if (std.io.is_async) blk: {
            var frame = arena.allocator().create(@Frame(getPathXdg)) catch break :block;
            _ = @asyncCall(frame, {}, getPathXdg, .{ arena.allocator(), arena, .local_configuration });
            break :blk (await frame) catch null orelse break :block;
        } else blk: {
            break :blk getPathXdg(arena.allocator(), arena, .local_configuration) catch null orelse break :block;
        };

        const config_dir = std.fs.cwd().openDir(config_dir_path, .{}) catch break :block;
        const home = std.os.getenv("HOME") orelse break :block;
        const user_dirs = config_dir.openFile("user-dirs.dirs", .{}) catch null orelse break :block;

        var read: [1024 * 8]u8 = undefined;
        _ = user_dirs.readAll(&read) catch null orelse break :block;
        const start = folder_spec.env.name.len + "=\"$HOME".len;

        var line_it = std.mem.split(u8, &read, "\n");
        while (line_it.next()) |line| {
            if (std.mem.startsWith(u8, line, folder_spec.env.name)) {
                const end = line.len - 1;
                if (start >= end) {
                    return error.ParseError;
                }

                var subdir = line[start..end];

                env_opt = try std.mem.concat(arena.allocator(), u8, &[_][]const u8{ home, subdir });
                break;
            }
        }
    }

    if (env_opt) |env| {
        if (folder_spec.env.suffix) |suffix| {
            return try std.mem.concat(allocator, u8, &[_][]const u8{ env, suffix });
        } else {
            if (std.mem.eql(u8, folder_spec.env.name, "XDG_CONFIG_DIRS")) {
                var iter = std.mem.split(u8, env, ":");
                return try allocator.dupe(u8, iter.next() orelse "");
            } else {
                return try allocator.dupe(u8, env);
            }
        }
    } else {
        const default = folder_spec.default orelse return null;
        if (default[0] == '~') {
            const home = std.os.getenv("HOME") orelse return null;
            return try std.mem.concat(allocator, u8, &[_][]const u8{ home, default[1..] });
        } else {
            return try allocator.dupe(u8, default);
        }
    }
}

/// Stores how to find each known folder on windows.
const windows_folder_spec = blk: {
    // workaround for zig eval branch quota when parsing the GUIDs
//     @setEvalBranchQuota(10_000);
    break :blk KnownFolderSpec(WindowsFolderSpec){
        .home = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{5E6C858F-0E22-4760-9AFE-EA3317B67173}") }, // FOLDERID_Profile
    };
};

const mac_folder_spec = KnownFolderSpec(MacFolderSpec){
    .home = MacFolderSpec{ .suffix = null },
};

/// Stores how to find each known folder in xdg.
const xdg_folder_spec = KnownFolderSpec(XdgFolderSpec){
    .home = XdgFolderSpec{ .env = .{ .name = "HOME", .user_dir = false, .suffix = null }, .default = null },
};

/// This returns a struct type with one field per KnownFolder of type `T`.
/// used for storing different config data per field
fn KnownFolderSpec(comptime T: type) type {
    return struct {
        const Self = @This();

        home: T,

        fn get(self: Self, folder: KnownFolder) T {
            inline for (std.meta.fields(Self)) |fld| {
                if (folder == @field(KnownFolder, fld.name))
                    return @field(self, fld.name);
            }
            unreachable;
        }
    };
}

/// Contains the GUIDs for each available known-folder on windows
const WindowsFolderSpec = union(enum) {
    by_guid: std.os.windows.GUID,
    by_env: struct {
        env_var: []const u8,
        subdir: ?[]const u8,
    },
};

/// Contains the folder items for macOS
const MacFolderSpec = union(enum) {
    suffix: ?[]const u8,
    absolute: []const u8,
};

/// Contains the xdg environment variable amd the default value for each available known-folder on windows
const XdgFolderSpec = struct {
    env: struct {
        name: []const u8,
        user_dir: bool,
        suffix: ?[]const u8,
    },
    default: ?[]const u8,
};

test "query each known folders" {
    inline for (std.meta.fields(KnownFolder)) |fld| {
        var path_or_null = try getPath(std.testing.allocator, @field(KnownFolder, fld.name));
        if (path_or_null) |path| {
            // TODO: Remove later
            std.debug.print("{s} => '{s}'\n", .{ fld.name, path });
            std.testing.allocator.free(path);
        }
    }
}