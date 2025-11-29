/// Extracted from https://github.com/ziglibs/known-folders/blob/master/known-folders.zig
const std = @import("std");
const root = @import("root");
const builtin = @import("builtin");

pub const KnownFolder = enum {
    home,
    documents,
    pictures,
    music,
    videos,
    desktop,
    downloads,
    public,
    fonts,
    app_menu,
    cache,
    roaming_configuration,
    local_configuration,
    global_configuration,
    data,
    runtime,
    executable_dir,
};

// Explicitly define possible errors to make it clearer what callers need to handle
pub const Error = error{ ParseError, OutOfMemory };

pub const KnownFolderConfig = struct {
    xdg_force_default: bool = false,
    xdg_on_mac: bool = false,
};

/// Returns a directory handle, or, if the folder does not exist, `null`.
pub fn open(allocator: std.mem.Allocator, folder: KnownFolder, args: std.fs.Dir.OpenOptions) (std.fs.Dir.OpenError || Error)!?std.fs.Dir {
    const path_or_null = try getPath(allocator, folder);
    if (path_or_null) |path| {
        defer allocator.free(path);

        return try std.fs.cwd().openDir(path, args);
    } else {
        return null;
    }
}

/// Returns the path to the folder or, if the folder does not exist, `null`.
pub fn getPath(allocator: std.mem.Allocator, folder: KnownFolder) Error!?[]const u8 {
    if (folder == .executable_dir) {
        if (builtin.os.tag == .wasi) return null;
        return std.fs.selfExeDirPathAlloc(allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => null,
        };
    }

    // used for temporary allocations
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    switch (builtin.os.tag) {
        .windows => {
            const funcs = struct {
                extern "shell32" fn SHGetKnownFolderPath(
                    rfid: *const std.os.windows.GUID,
                    dwFlags: std.os.windows.DWORD,
                    hToken: ?std.os.windows.HANDLE,
                    ppszPathL: *std.os.windows.PWSTR,
                ) callconv(.winapi) std.os.windows.HRESULT;
                extern "ole32" fn CoTaskMemFree(pv: std.os.windows.LPVOID) callconv(.winapi) void;
            };

            const folder_spec = windows_folder_spec.get(folder);

            switch (folder_spec) {
                .by_guid => |guid| {
                    var dir_path_ptr: [*:0]u16 = undefined;
                    switch (funcs.SHGetKnownFolderPath(
                        &guid,
                        std.os.windows.KF_FLAG_CREATE, // TODO: Chose sane option here?
                        null,
                        &dir_path_ptr,
                    )) {
                        std.os.windows.S_OK => {
                            defer funcs.CoTaskMemFree(@ptrCast(dir_path_ptr));
                            const global_dir = std.unicode.utf16LeToUtf8Alloc(allocator, std.mem.span(dir_path_ptr)) catch |err| switch (err) {
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
                            error.InvalidWtf8 => return null,
                            error.OutOfMemory => |e| return e,
                        };
                        return try std.fs.path.join(allocator, &[_][]const u8{ root_path, sub_dir });
                    } else {
                        return std.process.getEnvVarOwned(allocator, env_path.env_var) catch |err| switch (err) {
                            error.EnvironmentVariableNotFound => return null,
                            error.InvalidWtf8 => return null,
                            error.OutOfMemory => |e| return e,
                        };
                    }
                },
            }
        },
        .macos => {
            if (@hasDecl(root, "known_folders_config") and root.known_folders_config.xdg_on_mac) {
                return getPathXdg(allocator, &arena, folder);
            }

            switch (mac_folder_spec.get(folder)) {
                .absolute => |abs| {
                    return try allocator.dupe(u8, abs);
                },
                .suffix => |s| {
                    const home_dir = if (std.posix.getenv("HOME")) |home|
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

    if (@hasDecl(root, "known_folders_config") and root.known_folders_config.xdg_force_default) {
        if (folder_spec.default) |default| {
            if (default[0] == '~') {
                const home = std.process.getEnvVarOwned(arena.allocator(), "HOME") catch null orelse return null;
                return try std.mem.concat(allocator, u8, &[_][]const u8{ home, default[1..] });
            } else {
                return try allocator.dupe(u8, default);
            }
        }
    }

    const env_opt = env_opt: {
        if (std.process.getEnvVarOwned(arena.allocator(), folder_spec.env.name) catch null) |env_opt| break :env_opt env_opt;

        if (!folder_spec.env.user_dir) break :env_opt null;

        // TODO: add caching so we only need to read once in a run
        const config_dir_path = getPathXdg(arena.allocator(), arena, .local_configuration) catch null orelse break :env_opt null;
        const config_dir = std.fs.cwd().openDir(config_dir_path, .{}) catch break :env_opt null;
        const home = std.process.getEnvVarOwned(arena.allocator(), "HOME") catch null orelse break :env_opt null;
        const user_dirs = config_dir.openFile("user-dirs.dirs", .{}) catch null orelse break :env_opt null;

        var read: [1024 * 8]u8 = undefined;
        _ = user_dirs.readAll(&read) catch null orelse break :env_opt null;
        const start = folder_spec.env.name.len + "=\"$HOME".len;

        var line_it = std.mem.splitScalar(u8, &read, '\n');
        while (line_it.next()) |line| {
            if (std.mem.startsWith(u8, line, folder_spec.env.name)) {
                const end = line.len - 1;
                if (start >= end) {
                    return error.ParseError;
                }

                const subdir = line[start..end];

                break :env_opt try std.mem.concat(arena.allocator(), u8, &[_][]const u8{ home, subdir });
            }
        }
        break :env_opt null;
    };

    if (env_opt) |env| {
        if (folder_spec.env.suffix) |suffix| {
            return try std.mem.concat(allocator, u8, &[_][]const u8{ env, suffix });
        } else {
            if (std.mem.eql(u8, folder_spec.env.name, "XDG_CONFIG_DIRS")) {
                var iter = std.mem.splitScalar(u8, env, ':');
                return try allocator.dupe(u8, iter.next() orelse "");
            } else {
                return try allocator.dupe(u8, env);
            }
        }
    } else {
        const default = folder_spec.default orelse return null;
        if (default[0] == '~') {
            const home = std.process.getEnvVarOwned(arena.allocator(), "HOME") catch null orelse return null;
            return try std.mem.concat(allocator, u8, &[_][]const u8{ home, default[1..] });
        } else {
            return try allocator.dupe(u8, default);
        }
    }
}

/// Contains the GUIDs for each available known-folder on windows
const WindowsFolderSpec = union(enum) {
    by_guid: std.os.windows.GUID,
    by_env: struct {
        env_var: []const u8,
        subdir: ?[]const u8,
    },
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

/// Contains the folder items for macOS
const MacFolderSpec = union(enum) {
    suffix: ?[]const u8,
    absolute: []const u8,
};

/// This returns a struct type with one field per KnownFolder of type `T`.
/// used for storing different config data per field
fn KnownFolderSpec(comptime T: type) type {
    return struct {
        const Self = @This();

        home: T,
        documents: T,
        pictures: T,
        music: T,
        videos: T,
        desktop: T,
        downloads: T,
        public: T,
        fonts: T,
        app_menu: T,
        cache: T,
        roaming_configuration: T,
        local_configuration: T,
        global_configuration: T,
        data: T,
        runtime: T,

        fn get(self: Self, folder: KnownFolder) T {
            inline for (std.meta.fields(Self)) |fld| {
                if (folder == @field(KnownFolder, fld.name))
                    return @field(self, fld.name);
            }
            unreachable;
        }
    };
}

/// Stores how to find each known folder on windows.
const windows_folder_spec = blk: {
    // workaround for zig eval branch quota when parsing the GUIDs
    @setEvalBranchQuota(10_000);
    break :blk KnownFolderSpec(WindowsFolderSpec){
        .home = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{5E6C858F-0E22-4760-9AFE-EA3317B67173}") }, // FOLDERID_Profile
        .documents = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{FDD39AD0-238F-46AF-ADB4-6C85480369C7}") }, // FOLDERID_Documents
        .pictures = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{33E28130-4E1E-4676-835A-98395C3BC3BB}") }, // FOLDERID_Pictures
        .music = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{4BD8D571-6D19-48D3-BE97-422220080E43}") }, // FOLDERID_Music
        .videos = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{18989B1D-99B5-455B-841C-AB7C74E4DDFC}") }, // FOLDERID_Videos
        .desktop = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{B4BFCC3A-DB2C-424C-B029-7FE99A87C641}") }, // FOLDERID_Desktop
        .downloads = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{374DE290-123F-4565-9164-39C4925E467B}") }, // FOLDERID_Downloads
        .public = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{DFDF76A2-C82A-4D63-906A-5644AC457385}") }, // FOLDERID_Public
        .fonts = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{FD228CB7-AE11-4AE3-864C-16F3910AB8FE}") }, // FOLDERID_Fonts
        .app_menu = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{625B53C3-AB48-4EC1-BA1F-A1EF4146FC19}") }, // FOLDERID_StartMenu
        .cache = WindowsFolderSpec{ .by_env = .{ .env_var = "LOCALAPPDATA", .subdir = "Temp" } }, // %LOCALAPPDATA%\Temp
        .roaming_configuration = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{3EB685DB-65F9-4CF6-A03A-E3EF65729F3D}") }, // FOLDERID_RoamingAppData
        .local_configuration = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{F1B32785-6FBA-4FCF-9D55-7B8E7F157091}") }, // FOLDERID_LocalAppData
        .global_configuration = WindowsFolderSpec{ .by_guid = std.os.windows.GUID.parse("{62AB5D82-FDC1-4DC3-A9DD-070D1D495D97}") }, // FOLDERID_ProgramData
        .data = WindowsFolderSpec{ .by_env = .{ .env_var = "APPDATA", .subdir = null } }, // %LOCALAPPDATA%\Temp
        .runtime = WindowsFolderSpec{ .by_env = .{ .env_var = "LOCALAPPDATA", .subdir = "Temp" } },
    };
};

/// Stores how to find each known folder in xdg.
const xdg_folder_spec = KnownFolderSpec(XdgFolderSpec){
    .home = XdgFolderSpec{ .env = .{ .name = "HOME", .user_dir = false, .suffix = null }, .default = null },
    .documents = XdgFolderSpec{ .env = .{ .name = "XDG_DOCUMENTS_DIR", .user_dir = true, .suffix = null }, .default = "~/Documents" },
    .pictures = XdgFolderSpec{ .env = .{ .name = "XDG_PICTURES_DIR", .user_dir = true, .suffix = null }, .default = "~/Pictures" },
    .music = XdgFolderSpec{ .env = .{ .name = "XDG_MUSIC_DIR", .user_dir = true, .suffix = null }, .default = "~/Music" },
    .videos = XdgFolderSpec{ .env = .{ .name = "XDG_VIDEOS_DIR", .user_dir = true, .suffix = null }, .default = "~/Videos" },
    .desktop = XdgFolderSpec{ .env = .{ .name = "XDG_DESKTOP_DIR", .user_dir = true, .suffix = null }, .default = "~/Desktop" },
    .downloads = XdgFolderSpec{ .env = .{ .name = "XDG_DOWNLOAD_DIR", .user_dir = true, .suffix = null }, .default = "~/Downloads" },
    .public = XdgFolderSpec{ .env = .{ .name = "XDG_PUBLICSHARE_DIR", .user_dir = true, .suffix = null }, .default = "~/Public" },
    .fonts = XdgFolderSpec{ .env = .{ .name = "XDG_DATA_HOME", .user_dir = false, .suffix = "/fonts" }, .default = "~/.local/share/fonts" },
    .app_menu = XdgFolderSpec{ .env = .{ .name = "XDG_DATA_HOME", .user_dir = false, .suffix = "/applications" }, .default = "~/.local/share/applications" },
    .cache = XdgFolderSpec{ .env = .{ .name = "XDG_CACHE_HOME", .user_dir = false, .suffix = null }, .default = "~/.cache" },
    .roaming_configuration = XdgFolderSpec{ .env = .{ .name = "XDG_CONFIG_HOME", .user_dir = false, .suffix = null }, .default = "~/.config" },
    .local_configuration = XdgFolderSpec{ .env = .{ .name = "XDG_CONFIG_HOME", .user_dir = false, .suffix = null }, .default = "~/.config" },
    .global_configuration = XdgFolderSpec{ .env = .{ .name = "XDG_CONFIG_DIRS", .user_dir = false, .suffix = null }, .default = "/etc" },
    .data = XdgFolderSpec{ .env = .{ .name = "XDG_DATA_HOME", .user_dir = false, .suffix = null }, .default = "~/.local/share" },
    .runtime = XdgFolderSpec{ .env = .{ .name = "XDG_RUNTIME_DIR", .user_dir = false, .suffix = null }, .default = null },
};

const mac_folder_spec = KnownFolderSpec(MacFolderSpec){
    .home = MacFolderSpec{ .suffix = null },
    .documents = MacFolderSpec{ .suffix = "Documents" },
    .pictures = MacFolderSpec{ .suffix = "Pictures" },
    .music = MacFolderSpec{ .suffix = "Music" },
    .videos = MacFolderSpec{ .suffix = "Movies" },
    .desktop = MacFolderSpec{ .suffix = "Desktop" },
    .downloads = MacFolderSpec{ .suffix = "Downloads" },
    .public = MacFolderSpec{ .suffix = "Public" },
    .fonts = MacFolderSpec{ .suffix = "Library/Fonts" },
    .app_menu = MacFolderSpec{ .suffix = "Applications" },
    .cache = MacFolderSpec{ .suffix = "Library/Caches" },
    .roaming_configuration = MacFolderSpec{ .suffix = "Library/Preferences" },
    .local_configuration = MacFolderSpec{ .suffix = "Library/Application Support" },
    .global_configuration = MacFolderSpec{ .absolute = "/Library/Preferences" },
    .data = MacFolderSpec{ .suffix = "Library/Application Support" },
    .runtime = MacFolderSpec{ .suffix = "Library/Application Support" },
};

// Ref decls
comptime {
    _ = KnownFolder;
    _ = Error;
    _ = open;
    _ = getPath;
}

test "query each known folders" {
    inline for (std.meta.fields(KnownFolder)) |fld| {
        const path_or_null = try getPath(std.testing.allocator, @field(KnownFolder, fld.name));
        if (path_or_null) |path| {
            // TODO: Remove later
            // std.debug.print("{s} => '{s}'\n", .{ fld.name, path });
            std.testing.allocator.free(path);
        }
    }
}

test "open each known folders" {
    inline for (std.meta.fields(KnownFolder)) |fld| {
        var dir_or_null = open(std.testing.allocator, @field(KnownFolder, fld.name), .{ .access_sub_paths = true }) catch |e| switch (e) {
            error.FileNotFound => return,
            else => return e,
        };
        if (dir_or_null) |*dir| {
            dir.close();
        }
    }
}
