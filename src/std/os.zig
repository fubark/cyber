const std = @import("std");
const stdx = @import("stdx");
const fatal = cy.fatal;
const builtin = @import("builtin");
const cy = @import("../cyber.zig");
const vmc = cy.vmc;
const rt = cy.rt;
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const fmt = @import("../fmt.zig");
const bindings = @import("../builtins/bindings.zig");
const Symbol = bindings.Symbol;
const prepareThrowSymbol = bindings.prepareThrowSymbol;
const fromUnsupportedError = bindings.fromUnsupportedError;
const bt = cy.types.BuiltinTypeSymIds;
const ffi = @import("os_ffi.zig");
const http = @import("../http.zig");
const cache = @import("../cache.zig");

const log = cy.log.scoped(.os);

pub var CFuncT: rt.TypeId = undefined;
pub var CStructT: rt.TypeId = undefined;
pub var CArrayT: rt.TypeId = undefined;

pub const Src = @embedFile("os.cy");
pub fn defaultFuncLoader(_: *cy.UserVM, func: cy.HostFuncInfo) callconv(.C) vmc.HostFuncFn {
    if (std.mem.eql(u8, funcs[func.idx].@"0", func.name.slice())) {
        return @ptrCast(funcs[func.idx].@"1");
    }
    return null;
}

const NameHostFunc = struct { []const u8, cy.ZHostFuncFn };
const funcs = [_]NameHostFunc{
    .{"access", access},
    .{"args", osArgs},
    .{"bindLib", bindLib},
    .{"bindLib", bindLibExt},
    .{"cacheUrl", cacheUrl},
    .{"copyFile", copyFile},
    .{"createDir", createDir},
    .{"createFile", createFile},
    .{"cstr", cstr},
    .{"cwd", cwd},
    .{"dirName", dirName},
    .{"execCmd", execCmd},
    .{"exePath", exePath},
    .{"exit", exit},
    .{"fetchUrl", fetchUrl},
    .{"free", free},
    .{"fromCstr", fromCstr},
    .{"getEnv", getEnv},
    .{"getEnvAll", getEnvAll},
    .{"getInput", getInput},
    .{"malloc", malloc},
    .{"milliTime", milliTime},
    .{"openDir", openDir},
    .{"openDir", openDir2},
    .{"openFile", openFile},
    .{"parseArgs", parseArgs},
    .{"readAll", readAll},
    .{"readFile", readFile},
    .{"readLine", readLine},
    .{"realPath", realPath},
    .{"removeDir", removeDir},
    .{"removeFile", removeFile},
    .{"setEnv", setEnv},
    .{"sleep", sleep},
    .{"unsetEnv", unsetEnv},
    .{"writeFile", writeFile},
};

pub fn postLoad(vm: *cy.UserVM, modId: cy.ModuleId) callconv(.C) void {
    zPostLoad(vm.internal().compiler, modId) catch |err| {
        cy.panicFmt("os module: {}", .{err});
    };
}

fn zPostLoad(self: *cy.VMcompiler, modId: cy.ModuleId) linksection(cy.InitSection) anyerror!void {
    const b = bindings.ModuleBuilder.init(self, modId);

    // Object Types.
    CFuncT = try b.createAndSetTypeObject("CFunc", &.{"sym", "args", "ret"});
    CStructT = try b.createAndSetTypeObject("CStruct", &.{"fields", "type"});
    CArrayT = try b.createAndSetTypeObject("CArray", &.{"n", "elem"});

    // Variables.
    try b.setVar("cpu", bt.String, try self.buf.getOrPushStringValue(@tagName(builtin.cpu.arch)));
    if (builtin.cpu.arch.endian() == .Little) {
        try b.setVar("endian", bt.Symbol, cy.Value.initSymbol(@intFromEnum(Symbol.little)));
    } else {
        try b.setVar("endian", bt.Symbol, cy.Value.initSymbol(@intFromEnum(Symbol.big)));
    }
    if (cy.hasStdFiles) {
        const stdin = try cy.heap.allocFile(self.vm, std.io.getStdIn().handle);
        stdin.asHeapObject().file.closeOnFree = false;
        try b.setVar("stdin", bt.Any, stdin);
        const stdout = try cy.heap.allocFile(self.vm, std.io.getStdOut().handle);
        stdout.asHeapObject().file.closeOnFree = false;
        try b.setVar("stdout", bt.Any, stdout);
        const stderr = try cy.heap.allocFile(self.vm, std.io.getStdErr().handle);
        stderr.asHeapObject().file.closeOnFree = false;
        try b.setVar("stderr", bt.Any, stderr);
    } else {
        try b.setVar("stdin", bt.Any, Value.None);
        try b.setVar("stdout", bt.Any, Value.None);
        try b.setVar("stderr", bt.Any, Value.None);
    }
    try b.setVar("system", bt.String, try self.buf.getOrPushStringValue(@tagName(builtin.os.tag)));
    
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        try b.setVar("vecBitSize", bt.Integer, cy.Value.initI32(VecSize * 8));
    } else {
        try b.setVar("vecBitSize", bt.Integer, cy.Value.initI32(0));
    }
}

pub fn destroy(vm: *cy.UserVM, modId: cy.ModuleId) callconv(.C) void {
    const c = vm.internal().compiler;
    const mod = c.sema.getModulePtr(modId);
    if (cy.hasStdFiles) {
        const stdin = (mod.getVarVal(c, "stdin") catch cy.fatal()).?;
        cy.arc.release(c.vm, stdin);

        const stdout = (mod.getVarVal(c, "stdout") catch cy.fatal()).?;
        cy.arc.release(c.vm, stdout);

        const stderr = (mod.getVarVal(c, "stderr") catch cy.fatal()).?;
        cy.arc.release(c.vm, stderr);
    }
}

fn openDir(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    return openDir2(vm, &[_]Value{ args[0], Value.False }, nargs);
}

fn openDir2(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = vm.valueToTempRawString(args[0]);
    const iterable = args[1].asBool();
    var fd: std.os.fd_t = undefined;
    if (iterable) {
        const dir = std.fs.cwd().openIterableDir(path, .{}) catch |err| {
            if (err == error.FileNotFound) {
                return prepareThrowSymbol(vm, .FileNotFound);
            } else {
                return fromUnsupportedError(vm, "openDir", err, @errorReturnTrace());
            }
        };
        fd = dir.dir.fd;
    } else {
        const dir = std.fs.cwd().openDir(path, .{}) catch |err| {
            if (err == error.FileNotFound) {
                return prepareThrowSymbol(vm, .FileNotFound);
            } else {
                return fromUnsupportedError(vm, "openDir", err, @errorReturnTrace());
            }
        };
        fd = dir.fd;
    }
    return vm.allocDir(fd, iterable) catch fatal();
}

fn removeDir(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = vm.valueToTempRawString(args[0]);
    std.fs.cwd().deleteDir(path) catch |err| {
        if (err == error.FileNotFound) {
            return prepareThrowSymbol(vm, .FileNotFound);
        } else {
            return fromUnsupportedError(vm, "removeDir", err, @errorReturnTrace());
        }
    };
    return Value.True;
}

fn copyFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) {
        return vm.returnPanic("Unsupported.");
    }
    const src = vm.valueToTempRawString(args[0]);
    const alloc = vm.allocator();
    const srcDupe = alloc.dupe(u8, src) catch fatal();
    defer alloc.free(srcDupe);
    const dst = vm.valueToTempRawString(args[1]);
    std.fs.cwd().copyFile(srcDupe, std.fs.cwd(), dst, .{}) catch |err| {
        if (err == error.FileNotFound) {
            return prepareThrowSymbol(vm, .FileNotFound);
        } else {
            return fromUnsupportedError(vm, "copyFile", err, @errorReturnTrace());
        }
    };
    return Value.True;
}

fn removeFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = vm.valueToTempRawString(args[0]);
    std.fs.cwd().deleteFile(path) catch |err| {
        if (err == error.FileNotFound) {
            return prepareThrowSymbol(vm, .FileNotFound);
        } else {
            return fromUnsupportedError(vm, "removeFile", err, @errorReturnTrace());
        }
    };
    return Value.True;
}

fn createDir(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = vm.valueToTempRawString(args[0]);
    std.fs.cwd().makeDir(path) catch |err| {
        return fromUnsupportedError(vm, "createDir", err, @errorReturnTrace());
    };
    return Value.True;
}

fn createFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = vm.valueToTempRawString(args[0]);
    const truncate = args[1].asBool();
    const file = std.fs.cwd().createFile(path, .{ .truncate = truncate }) catch |err| {
        return fromUnsupportedError(vm, "createFile", err, @errorReturnTrace());
    };
    return vm.allocFile(file.handle) catch fatal();
}

pub fn access(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm) {
        return vm.returnPanic("Unsupported.");
    }

    const path = vm.valueToTempRawString(args[0]);

    const mode: Symbol = @enumFromInt(args[1].asSymbolId());
    const zmode: std.fs.File.OpenMode = switch (mode) {
        .read => .read_only,
        .write => .write_only,
        .readWrite => .read_write,
        else => {
            return Value.None;
        }
    };
    std.fs.cwd().access(path, .{ .mode = zmode }) catch |err| {
        if (err == error.FileNotFound) {
            return prepareThrowSymbol(vm, .FileNotFound);
        } else if (err == error.PermissionDenied) {
            return prepareThrowSymbol(vm, .PermissionDenied);
        } else {
            return fromUnsupportedError(vm, "access", err, @errorReturnTrace());
        }
    };
    return Value.True;
}

fn openFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = vm.valueToTempRawString(args[0]);
    const mode: Symbol = @enumFromInt(args[1].asSymbolId());
    const zmode: std.fs.File.OpenMode = switch (mode) {
        .read => .read_only,
        .write => .write_only,
        .readWrite => .read_write,
        else => {
            return Value.None;
        }
    };
    const file = std.fs.cwd().openFile(path, .{ .mode = zmode }) catch |err| {
        if (err == error.FileNotFound) {
            return prepareThrowSymbol(vm, .FileNotFound);
        } else {
            return fromUnsupportedError(vm, "openFile", err, @errorReturnTrace());
        }
    };
    return vm.allocFile(file.handle) catch fatal();
}

fn parseArgs(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const ivm = vm.internal();
    const alloc = vm.allocator();

    const list = args[0].asHeapObject().list.items();

    // Build options map.
    const OptionType = enum {
        string,
        number,
        boolean,
    };
    const Option = struct {
        name: Value,
        type: OptionType,
        default: Value,

        found: bool,
    };
    var optionMap: std.StringHashMapUnmanaged(Option) = .{};
    defer optionMap.deinit(alloc);
    for (list) |opt| {
        if (opt.isObjectType(rt.MapT)) {
            const entry = opt.asHeapObject().map.map();
            const name = entry.getByString(ivm, "name") orelse return prepareThrowSymbol(vm, .InvalidArgument);
            if (!name.isString()) {
                return prepareThrowSymbol(vm, .InvalidArgument);
            }
            const entryType = entry.getByString(ivm, "type") orelse return prepareThrowSymbol(vm, .InvalidArgument);
            if (!entryType.isObjectType(rt.MetaTypeT)) {
                return prepareThrowSymbol(vm, .InvalidArgument);
            }
            var optType: OptionType = undefined;
            switch (entryType.asHeapObject().metatype.symId) {
                rt.StringUnionT => {
                    optType = .string;
                },
                rt.FloatT => {
                    optType = .number;
                },
                rt.BooleanT => {
                    optType = .boolean;
                },
                else => {
                    return prepareThrowSymbol(vm, .InvalidArgument);
                },
            }
            const default = entry.getByString(ivm, "default") orelse Value.None;
            optionMap.put(alloc, vm.valueAsString(name), .{
                .name = name,
                .type = optType,
                .default = default,
                .found = false,
            }) catch fatal();
        } else {
            return prepareThrowSymbol(vm, .InvalidArgument);
        }
    }

    const res = vm.allocEmptyMap() catch fatal();
    const map = res.asHeapObject().map.map();

    var iter = std.process.argsWithAllocator(alloc) catch fatal();
    defer iter.deinit();
    const rest = vm.allocEmptyList() catch fatal();
    const restList = rest.asHeapObject().list.getList();
    while (iter.next()) |arg| {
        if (arg[0] == '-') {
            const optName = arg[1..];
            if (optionMap.getPtr(optName)) |opt| {
                if (opt.found) {
                    continue;
                }
                switch (opt.type) {
                    .string => {
                        if (iter.next()) |nextArg| {
                            const val = vm.allocStringInfer(nextArg) catch fatal();
                            vm.retain(opt.name);
                            map.put(alloc, ivm, opt.name, val) catch fatal();
                            opt.found = true;
                        } else {
                            return prepareThrowSymbol(vm, .InvalidArgument);
                        }
                    },
                    .number => {
                        if (iter.next()) |nextArg| {
                            const num = std.fmt.parseFloat(f64, nextArg) catch {
                                return prepareThrowSymbol(vm, .InvalidArgument);
                            };
                            vm.retain(opt.name);
                            map.put(alloc, ivm, opt.name, Value.initF64(num)) catch fatal();
                            opt.found = true;
                        } else {
                            return prepareThrowSymbol(vm, .InvalidArgument);
                        }
                    },
                    .boolean => {
                        vm.retain(opt.name);
                        map.put(alloc, ivm, opt.name, Value.True) catch fatal();
                        opt.found = true;
                    }
                }
            }
            continue;
        }
        const str = vm.allocStringOrRawstring(arg) catch fatal();
        restList.append(alloc, str) catch fatal();
    }

    // Fill missing with defaults.
    var optIter = optionMap.valueIterator();
    while (optIter.next()) |opt| {
        if (!opt.*.found) {
            vm.retain(opt.*.name);
            vm.retain(opt.*.default);
            map.put(alloc, ivm, opt.*.name, opt.*.default) catch fatal();
        }
    }

    map.put(alloc, ivm, vm.allocAstring("rest") catch fatal(), rest) catch fatal();
    return res;
}

fn osArgs(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) {
        return vm.returnPanic("Unsupported.");
    }
    const alloc = vm.allocator();
    var iter = std.process.argsWithAllocator(alloc) catch fatal();
    defer iter.deinit();
    const listv = vm.allocEmptyList() catch fatal();
    const listo = listv.asHeapObject();
    while (iter.next()) |arg| {
        const str = vm.allocStringOrRawstring(arg) catch fatal();
        listo.list.append(vm.allocator(), str);
    }
    return listv;
}

pub fn cwd(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const res = std.process.getCwdAlloc(vm.allocator()) catch fatal();
    defer vm.allocator().free(res);
    // TODO: Use allocOwnedString
    return vm.allocStringInfer(res) catch fatal();
}

pub fn exePath(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = std.fs.selfExePathAlloc(vm.allocator()) catch fatal();
    defer vm.allocator().free(path);
    // TODO: Use allocOwnedString
    return vm.allocStringInfer(path) catch fatal();
}

pub fn getEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.returnPanic("Unsupported.");
    const key = vm.valueToTempRawString(args[0]);
    const res = std.os.getenv(key) orelse return Value.None;
    return vm.allocStringInfer(res) catch cy.fatal();
}

pub fn getEnvAll(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.returnPanic("Unsupported.");
    var env = std.process.getEnvMap(vm.allocator()) catch cy.fatal();
    defer env.deinit();

    const map = vm.allocEmptyMap() catch cy.fatal();
    var iter = env.iterator();
    while (iter.next()) |entry| {
        const key = vm.allocStringInfer(entry.key_ptr.*) catch cy.fatal();
        const val = vm.allocStringInfer(entry.value_ptr.*) catch cy.fatal();
        defer {
            vm.release(key);
            vm.release(val);
        }
        map.asHeapObject().map.set(vm.internal(), key, val) catch cy.fatal();
    }
    return map;
}

pub fn free(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const ptr = args[0].asHeapObject().pointer.ptr;
    std.c.free(ptr);
    return Value.None;
}

pub fn malloc(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const size: usize = @intCast(args[0].asInteger());
    const ptr = std.c.malloc(size);
    return cy.heap.allocPointer(vm.internal(), ptr) catch cy.fatal();
}

fn fromCstr(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const bytes = std.mem.span(@as([*:0]const u8, @ptrCast(args[0].asHeapObject().pointer.ptr)));
    return vm.allocRawString(bytes) catch fatal();
}

fn cstr(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const bytes = vm.valueToTempRawString(args[0]);
    const new: [*]u8 = @ptrCast(std.c.malloc(bytes.len + 1));
    @memcpy(new[0..bytes.len], bytes);
    new[bytes.len] = 0;
    return cy.heap.allocPointer(vm.internal(), new) catch fatal();
}

pub fn milliTime(_: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return Value.initF64(@floatFromInt(stdx.time.getMilliTimestamp()));
}

pub fn dirName(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = vm.valueToTempRawString(args[0]);
    if (std.fs.path.dirname(path)) |res| {
        return vm.allocStringInfer(res) catch cy.fatal();
    } else {
        return Value.None;
    }
}

pub fn realPath(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = vm.valueToTempRawString(args[0]);
    const res = std.fs.cwd().realpathAlloc(vm.allocator(), path) catch |err| {
        return fromUnsupportedError(vm, "realPath", err, @errorReturnTrace());
    };
    defer vm.allocator().free(res);
    // TODO: Use allocOwnedString.
    return vm.allocStringInfer(res) catch cy.fatal();
}

pub fn setEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.returnPanic("Unsupported.");
    const key = vm.valueToTempRawString(args[0]);
    const keyz = vm.allocator().dupeZ(u8, key) catch cy.fatal();
    defer vm.allocator().free(keyz);

    const value = vm.valueToTempRawString(args[1]);
    const valuez = vm.allocator().dupeZ(u8, value) catch cy.fatal();
    defer vm.allocator().free(valuez);
    _ = setenv(keyz, valuez, 1);
    return Value.None;
}
pub extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;

pub fn sleep(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (builtin.os.tag == .windows) {
        const ms: u32 = @intFromFloat(args[0].asF64());
        std.os.windows.kernel32.Sleep(ms);
    } else {
        const ms = args[0].asF64();
        const secs: u64 = @intFromFloat(@divFloor(ms, 1000));
        const nsecs: u64 = @intFromFloat(1e6 * (std.math.mod(f64, ms, 1000) catch cy.fatal()));
        if (cy.isWasm) {
            hostSleep(secs, nsecs);
        } else {
            std.os.nanosleep(secs, nsecs);
        }
    }
    return Value.None;
}

extern fn hostSleep(secs: u64, nsecs: u64) void;

pub fn unsetEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.returnPanic("Unsupported.");
    const key = vm.valueToTempRawString(args[0]);
    const keyz = vm.allocator().dupeZ(u8, key) catch cy.fatal();
    defer vm.allocator().free(keyz);
    _ = unsetenv(keyz);
    return Value.None;
}
pub extern "c" fn unsetenv(name: [*:0]const u8) c_int;

pub fn bindLib(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasFFI) {
        return vm.returnPanic("Unsupported.");
    }
    return @call(.never_inline, ffi.bindLib, .{vm, args, .{}}) catch |err| {
        if (builtin.mode == .Debug) {
            std.debug.dumpStackTrace(@errorReturnTrace().?.*);
        }
        if (err == error.InvalidArgument) {
            return prepareThrowSymbol(vm, .InvalidArgument);
        } else {
            return fromUnsupportedError(vm, "bindLib", err, @errorReturnTrace());
        }
    };
}

pub fn bindLibExt(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasFFI) {
        return vm.returnPanic("Unsupported.");
    }
    var configV = args[2];
    const ivm = vm.internal();
    const genMapV = vm.allocAstring("genMap") catch cy.fatal();
    defer vm.release(genMapV);
    var config: ffi.BindLibConfig = .{};
    const val = configV.asHeapObject().map.map().get(ivm, genMapV) orelse Value.False;
    if (val.isTrue()) {
        config.genMap = true;
    }
    return @call(.never_inline, ffi.bindLib, .{vm, args, config}) catch |err| {
        if (builtin.mode == .Debug) {
            std.debug.dumpStackTrace(@errorReturnTrace().?.*);
        }
        if (err == error.InvalidArgument) {
            return prepareThrowSymbol(vm, .InvalidArgument);
        } else {
            return fromUnsupportedError(vm, "bindLib", err, @errorReturnTrace());
        }
    };
}

pub extern fn hostFileWrite(fid: u32, str: [*]const u8, strLen: usize) void;

fn cacheUrl(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const alloc = vm.allocator();
    const url = vm.valueToTempString(args[0]);

    const specGroup = cache.getSpecHashGroup(alloc, url) catch cy.fatal();
    defer specGroup.deinit(alloc);

    if (vm.internal().config.reload) {
        specGroup.markEntryBySpecForRemoval(url) catch cy.fatal();
    } else {
        // First check local cache.
        if (specGroup.findEntryBySpec(url) catch cy.fatal()) |entry| {
            const path = cache.allocSpecFilePath(alloc, entry) catch cy.fatal();
            defer alloc.free(path);
            return vm.allocStringInfer(path) catch cy.fatal();
        }
    }

    const resp = http.get(alloc, vm.internal().httpClient, url) catch |err| {
        log.debug("cacheUrl error: {}", .{err});
        return prepareThrowSymbol(vm, .UnknownError);
    };
    defer alloc.free(resp.body);
    if (resp.status != .ok) {
        log.debug("cacheUrl response status: {}", .{resp.status});
        return prepareThrowSymbol(vm, .UnknownError);
    } else {
        const entry = cache.saveNewSpecFile(alloc, specGroup, url, resp.body) catch cy.fatal();
        defer entry.deinit(alloc);
        const path = cache.allocSpecFilePath(alloc, entry) catch cy.fatal();
        defer alloc.free(path);
        return vm.allocStringInfer(path) catch cy.fatal();
    }
}

pub fn execCmd(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const alloc = vm.allocator();
    const ivm = vm.internal();

    const obj = args[0].asHeapObject();
    var buf: std.ArrayListUnmanaged([]const u8) = .{};
    defer {
        for (buf.items) |arg| {
            alloc.free(arg);
        }
        buf.deinit(alloc);
    }
    for (obj.list.items()) |arg| {
        buf.append(alloc, vm.valueToString(arg) catch cy.fatal()) catch cy.fatal();
    }

    const res = std.ChildProcess.exec(.{
        .allocator = alloc,
        .argv = buf.items,
        .max_output_bytes = 1024 * 1024 * 10,
    }) catch |err| {
        switch (err) {
            error.FileNotFound => 
                return prepareThrowSymbol(vm, .FileNotFound),
            error.StdoutStreamTooLong =>
                return prepareThrowSymbol(vm, .StreamTooLong),
            error.StderrStreamTooLong =>
                return prepareThrowSymbol(vm, .StreamTooLong),
            else => cy.panicFmt("exec err {}\n", .{err}),
        }
    };

    const map = vm.allocEmptyMap() catch cy.fatal();
    const outKey = vm.allocAstring("out") catch cy.fatal();
    const errKey = vm.allocAstring("err") catch cy.fatal();
    defer {
        vm.release(outKey);
        vm.release(errKey);
    }

    // TODO: Use allocOwnedString
    defer alloc.free(res.stdout);
    const out = vm.allocStringInfer(res.stdout) catch cy.fatal();
    defer vm.release(out);
    map.asHeapObject().map.set(ivm, outKey, out) catch cy.fatal();
    // TODO: Use allocOwnedString
    defer alloc.free(res.stderr);
    const err = vm.allocStringInfer(res.stderr) catch cy.fatal();
    defer vm.release(err);
    map.asHeapObject().map.set(ivm, errKey, err) catch cy.fatal();
    if (res.term == .Exited) {
        const exitedKey = vm.allocAstring("exited") catch cy.fatal();
        defer vm.release(exitedKey);
        map.asHeapObject().map.set(ivm, exitedKey, Value.initF64(@floatFromInt(res.term.Exited))) catch cy.fatal();
    }
    return map;
}

pub fn exit(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const status: u8 = @intCast(args[0].asInteger());
    std.os.exit(status);
}

pub fn fetchUrl(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const alloc = vm.allocator();
    const url = vm.valueToTempString(args[0]);
    if (cy.isWasm) {
        hostFetchUrl(url.ptr, url.len);
        return Value.None;
    } else {
        const resp = http.get(alloc, vm.internal().httpClient, url) catch |err| {
            log.debug("fetchUrl error: {}", .{err});
            return prepareThrowSymbol(vm, .UnknownError);
        };
        defer alloc.free(resp.body);
        // TODO: Use allocOwnedString
        return vm.allocRawString(resp.body) catch cy.fatal();
    }
}

extern fn hostFetchUrl(url: [*]const u8, urlLen: usize) void;

pub fn getInput(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    const input = std.io.getStdIn().reader().readUntilDelimiterAlloc(vm.allocator(), '\n', 10e8) catch |err| {
        if (err == error.EndOfStream) {
            return prepareThrowSymbol(vm, .EndOfStream);
        } else cy.fatal();
    };
    defer vm.allocator().free(input);
    // TODO: Use allocOwnedString
    return vm.allocRawString(input) catch cy.fatal();
}

pub fn readAll(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    const input = std.io.getStdIn().readToEndAlloc(vm.allocator(), 10e8) catch cy.fatal();
    defer vm.allocator().free(input);
    // TODO: Use allocOwnString.
    return vm.allocRawString(input) catch cy.fatal();
}

pub fn readFile(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    const path = vm.valueToTempRawString(args[0]);
    const content = std.fs.cwd().readFileAlloc(vm.allocator(), path, 10e8) catch |err| {
        if (err == error.FileNotFound) {
            return prepareThrowSymbol(vm, .FileNotFound);
        }
        log.debug("readFile {}", .{err});
        return prepareThrowSymbol(vm, .UnknownError);
    };
    defer vm.allocator().free(content);
    // TODO: Use allocOwnedString.
    return vm.allocRawString(content) catch cy.fatal();
}

pub fn readLine(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    fmt.printDeprecated("readLine", "0.1", "Use getInput() instead.", &.{});
    return getInput(vm, args, nargs);
}

pub fn writeFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    const path = vm.valueToTempRawString(args[0]);
    const pathDupe = vm.allocator().dupe(u8, path) catch fatal();
    defer vm.allocator().free(pathDupe);
    const content = vm.valueToTempRawString(args[1]);
    std.fs.cwd().writeFile(path, content) catch cy.fatal();
    return Value.None;
}