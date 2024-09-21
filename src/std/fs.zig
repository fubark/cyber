const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const log = cy.log.scoped(.fs);
const cy = @import("../cyber.zig");
const rt = cy.rt;
const Value = cy.Value;
const C = @import("../capi.zig");
const cli = @import("../cli.zig");

pub const File = extern struct {
    readBuf: [*]u8,
    /// Can be up to 8 bytes on windows, otherwise 4 bytes.
    fd: if (cy.hasStdFiles) std.posix.fd_t else u32,
    curPos: u32,
    readBufCap: u32,
    readBufEnd: u32,
    iterLines: bool,
    hasReadBuf: bool,
    closeOnFree: bool,
    closed: bool,

    pub fn getStdFile(self: *const File) std.fs.File {
        return std.fs.File{
            .handle = self.fd,
        };
    }

    pub fn close(self: *File) void {
        if (!self.closed) {
            const file = self.getStdFile();
            file.close();
            self.closed = true;
        }
    }
};

pub fn fileFinalizer(vm_: ?*C.VM, obj: ?*anyopaque) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    if (cy.hasStdFiles) {
        const file: *File = @ptrCast(@alignCast(obj));
        if (file.hasReadBuf) {
            vm.alloc.free(file.readBuf[0..file.readBufCap]);
        }
        if (file.closeOnFree) {
            file.close();
        }
    }
}

pub fn dirFinalizer(_: ?*C.VM, obj: ?*anyopaque) callconv(.C) void {
    if (cy.hasStdFiles) {
        const dir: *Dir = @ptrCast(@alignCast(obj));
        dir.close();
    }
}

pub const Dir = extern struct {
    /// Padding to make Dir.fd match the offset of File.fd.
    padding: usize = 0,
    fd: if (cy.hasStdFiles) std.posix.fd_t else u32,
    iterable: bool,
    closed: bool,

    pub fn getStdDir(self: *const Dir) std.fs.Dir {
        return std.fs.Dir{
            .fd = self.fd,
        };
    }

    pub fn close(self: *Dir) void {
        if (!self.closed) {
            var dir = self.getStdDir();
            dir.close();
            self.closed = true;
        }
    }
};

pub fn dirIterFinalizer(_: ?*C.VM, obj: ?*anyopaque) callconv(.C) void {
    if (cy.hasStdFiles) {
        var dir: *DirIterator = @ptrCast(@alignCast(obj));
        if (dir.recursive) {
            const walker = cy.ptrAlignCast(*std.fs.Dir.Walker, &dir.inner.walker);
            walker.deinit();   
        }
    }
}

pub fn dirIterGetChildren(_: ?*C.VM, obj: ?*anyopaque) callconv(.C) C.ValueSlice {
    const dirIter: *DirIterator = @ptrCast(@alignCast(obj));
    return .{
        .ptr = @ptrCast(&dirIter.dir),
        .len = 1,
    };
}

pub const DirIterator = extern struct {
    dir: Value, // `Dir` object.
    inner: extern union {
        iter: if (cy.hasStdFiles) [@sizeOf(std.fs.Dir.Iterator)]u8 else void,
        walker: if (cy.hasStdFiles) [@sizeOf(std.fs.Dir.Walker)]u8 else void,
    },
    /// If `recursive` is true, `walker` is used.
    recursive: bool,
};

pub fn allocFile(vm: *cy.VM, fd: if (cy.hasStdFiles) std.posix.fd_t else u32) !Value {
    const cli_data = vm.getData(*cli.CliData, "cli");
    const file: *File = @ptrCast(@alignCast(try cy.heap.allocHostObject(vm, cli_data.FileT.id(), @sizeOf(File))));
    file.* = .{
        .fd = fd,
        .curPos = 0,
        .iterLines = false,
        .hasReadBuf = false,
        .readBuf = undefined,
        .readBufCap = 0,
        .readBufEnd = 0,
        .closed = false,
        .closeOnFree = true,
    };
    return Value.initHostPtr(file);
}

pub fn allocDir(vm: *cy.VM, fd: std.posix.fd_t, iterable: bool) !Value {
    const cli_data = vm.getData(*cli.CliData, "cli");
    const dir: *Dir = @ptrCast(@alignCast(try cy.heap.allocHostObject(vm, cli_data.DirT.id(), @sizeOf(Dir))));
    dir.* = .{
        .fd = fd,
        .iterable = iterable,
        .closed = false,
    };
    return Value.initHostPtr(dir);
}

pub fn allocDirIterator(vm: *cy.VM, dirv: Value, recursive: bool) !Value {
    const cli_data = vm.getData(*cli.CliData, "cli");
    const dirIter: *DirIterator = @ptrCast(@alignCast(try cy.heap.allocHostObject(vm, cli_data.DirIterT.id(), @sizeOf(DirIterator))));
    dirIter.* = .{
        .dir = dirv,
        .inner = undefined,
        .recursive = recursive,
    };
    const dir = dirv.castHostObject(*Dir);
    if (recursive) {
        const walker = cy.ptrAlignCast(*std.fs.Dir.Walker, &dirIter.inner.walker);
        walker.* = try dir.getStdDir().walk(vm.alloc);
    } else {
        const iter = cy.ptrAlignCast(*std.fs.Dir.Iterator, &dirIter.inner.iter);
        iter.* = dir.getStdDir().iterate();
    }
    return Value.initHostPtr(dirIter);
}

test "fs internals" {
    if (cy.is32Bit) {
        try t.eq(@alignOf(Dir), 4);
    } else {
        try t.eq(@alignOf(Dir), 8);
    }
    try t.eq(@alignOf(DirIterator), 8);

    if (builtin.os.tag != .windows) {
        if (cy.hasStdFiles) {
            try t.eq(@sizeOf(File), 32);
            try t.eq(@sizeOf(Dir), 16);
        }
    }

    try t.eq(@offsetOf(File, "readBuf"), @offsetOf(Dir, "padding"));
    try t.eq(@offsetOf(File, "fd"), @offsetOf(Dir, "fd"));
}

pub fn fileStreamLines(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");
    vm.setInt(1, 4096);
    return fileStreamLines1(vm);
}

pub fn fileStreamLines1(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");
    // Don't need to release obj since it's being returned.
    const file = vm.getHostObject(*File, 0);
    const bufSize: usize = @intCast(vm.getInt(1));
    var createReadBuf = true;
    if (file.hasReadBuf) {
        if (bufSize != file.readBufCap) {
            // Cleanup previous buffer.
            vm.alloc.free(file.readBuf[0..file.readBufCap]);
        } else {
            createReadBuf = false;
        }
    }
    // Allocate read buffer.
    file.iterLines = true;
    if (createReadBuf) {
        const readBuf = vm.alloc.alloc(u8, bufSize) catch cy.fatal();
        file.readBuf = readBuf.ptr;
        file.readBufCap = @intCast(readBuf.len);
        file.hasReadBuf = true;
    }

    vm.retain(vm.getValue(0));
    return vm.getValue(0);
}

pub fn dirWalk(vm: *cy.VM) Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");
    const dir = vm.getHostObject(*Dir, 0);
    if (dir.iterable) {
        vm.retain(vm.getValue(0));
        return allocDirIterator(vm, vm.getValue(0), true) catch cy.fatal();
    } else {
        return rt.prepThrowError(vm, .NotAllowed);
    }
}

pub fn dirIterator(vm: *cy.VM) Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");
    const dir = vm.getHostObject(*Dir, 0);
    if (dir.iterable) {
        vm.retain(vm.getValue(0));
        return allocDirIterator(vm, vm.getValue(0), false) catch cy.fatal();
    } else {
        return rt.prepThrowError(vm, .NotAllowed);
    }
}

pub fn fileIterator(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    // Don't need to release obj since it's being returned.
    const file = vm.getHostObject(*File, 0);
    file.curPos = 0;
    file.readBufEnd = 0;
    vm.retain(vm.getValue(0));
    return vm.getValue(0);
}

pub fn fileSeekFromEnd(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const fileo = vm.getHostObject(*File, 0);
    if (fileo.closed) {
        return rt.prepThrowError(vm, .Closed);
    }

    const numBytes = vm.getInt(1);
    if (numBytes > 0) {
        return rt.prepThrowError(vm, .InvalidArgument);
    }

    const file = fileo.getStdFile();
    try file.seekFromEnd(numBytes);
    return Value.Void;
}

pub fn fileSeekFromCur(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const fileo = vm.getHostObject(*File, 0);
    if (fileo.closed) {
        return rt.prepThrowError(vm, .Closed);
    }

    const numBytes = vm.getInt(1);

    const file = fileo.getStdFile();
    try file.seekBy(numBytes);
    return Value.Void;
}

pub fn fileSeek(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const fileo = vm.getHostObject(*File, 0);
    if (fileo.closed) {
        return rt.prepThrowError(vm, .Closed);
    }

    const numBytes = vm.getInt(1);
    if (numBytes < 0) {
        return rt.prepThrowError(vm, .InvalidArgument);
    }

    const file = fileo.getStdFile();
    const unumBytes: u32 = @intCast(numBytes);
    try file.seekTo(unumBytes);
    return Value.Void;
}

pub fn fileWrite(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const fileo = vm.getHostObject(*File, 0);
    if (fileo.closed) {
        return rt.prepThrowError(vm, .Closed);
    }

    const str = vm.getString(1);
    const file = fileo.getStdFile();
    const numWritten = try file.write(str);
    return Value.initInt(@intCast(numWritten));
}

pub fn fileClose(vm: *cy.VM) Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const file = vm.getHostObject(*File, 0);
    file.close();
    return Value.Void;
}

pub fn fileRead(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const fileo = vm.getHostObject(*File, 0);
    if (fileo.closed) {
        return rt.prepThrowError(vm, .Closed);
    }

    const numBytes = vm.getInt(1);
    if (numBytes <= 0) {
        return error.InvalidArgument;
    }
    const unumBytes: usize = @intCast(numBytes);
    const file = fileo.getStdFile();

    const tempBuf = &vm.u8Buf;
    tempBuf.clearRetainingCapacity();
    defer tempBuf.ensureMaxCapOrClear(vm.alloc, 4096) catch cy.fatal();
    try tempBuf.ensureTotalCapacityPrecise(vm.alloc, unumBytes);

    const numRead = try file.read(tempBuf.buf[0..unumBytes]);
    // Can return empty string when numRead == 0.
    return vm.allocString(tempBuf.buf[0..numRead]);
}

pub fn fileReadAll(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const fileo = vm.getHostObject(*File, 0);
    if (fileo.closed) {
        return rt.prepThrowError(vm, .Closed);
    }

    const file = fileo.getStdFile();

    const tempBuf = &vm.u8Buf;
    tempBuf.clearRetainingCapacity();
    defer tempBuf.ensureMaxCapOrClear(vm.alloc, 4096) catch cy.fatal();

    const MinReadBufSize = 4096;
    tempBuf.ensureTotalCapacity(vm.alloc, MinReadBufSize) catch cy.fatal();

    while (true) {
        const buf = tempBuf.buf[tempBuf.len .. tempBuf.buf.len];
        const numRead = try file.readAll(buf);
        tempBuf.len += numRead;
        if (numRead < buf.len) {
            // Done.
            const all = tempBuf.items();
            // Can return empty string.
            return vm.allocString(all);
        } else {
            try tempBuf.ensureUnusedCapacity(vm.alloc, MinReadBufSize);
        }
    }
}

pub fn fileOrDirStat(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const cli_data = vm.getData(*cli.CliData, "cli");

    const obj = vm.getValue(0).asHeapObject();
    const typeId = obj.getTypeId();
    if (typeId == cli_data.FileT.id()) {
        const file = vm.getHostObject(*File, 0);
        if (file.closed) {
            return rt.prepThrowError(vm, .Closed);
        }
    } else if (typeId == cli_data.DirT.id()) {
        const dir = vm.getHostObject(*Dir, 0);
        if (dir.closed) {
            return rt.prepThrowError(vm, .Closed);
        }
    } else return error.Unexpected;

    // File/Dir share the same fd member offset.
    const fileo = vm.getHostObject(*File, 0);
    const file = fileo.getStdFile();
    const stat = try file.stat();

    const mapv = try vm.allocEmptyMap();
    const map = mapv.castHeapObject(*cy.heap.Map);

    const sizeKey = try vm.retainOrAllocAstring("size");
    const modeKey = try vm.retainOrAllocAstring("mode");
    const typeKey = try vm.retainOrAllocAstring("type");
    const atimeKey = try vm.retainOrAllocAstring("atime");
    const ctimeKey = try vm.retainOrAllocAstring("ctime");
    const mtimeKey = try vm.retainOrAllocAstring("mtime");
    defer {
        vm.release(sizeKey);
        vm.release(modeKey);
        vm.release(typeKey);
        vm.release(atimeKey);
        vm.release(ctimeKey);
        vm.release(mtimeKey);
    }
    try map.set(vm, sizeKey, Value.initInt(@intCast(stat.size)));
    try map.set(vm, modeKey, Value.initInt(@intCast(stat.mode)));
    const typeTag: cy.bindings.Symbol = switch (stat.kind) {
        .file => .file,
        .directory => .dir,
        else => .unknown,
    };
    try map.set(vm, typeKey, Value.initSymbol(@intFromEnum(typeTag)));
    try map.set(vm, atimeKey, Value.initInt(@intCast(@divTrunc(stat.atime, 1000000))));
    try map.set(vm, ctimeKey, Value.initInt(@intCast(@divTrunc(stat.ctime, 1000000))));
    try map.set(vm, mtimeKey, Value.initInt(@intCast(@divTrunc(stat.mtime, 1000000))));
    return mapv;
}

pub fn fileNext(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const fileo = vm.getHostObject(*File, 0);
    if (fileo.iterLines) {
        const readBuf = fileo.readBuf[0..fileo.readBufCap];
        if (cy.string.getLineEnd(readBuf[fileo.curPos..fileo.readBufEnd])) |end| {
            // Found new line.
            const line = try vm.allocString(readBuf[fileo.curPos..fileo.curPos+end]);

            // Advance pos.
            fileo.curPos += @intCast(end);

            return StringSome(vm, line);
        }

        var lineBuf = try cy.string.HeapStringBuilder.init(vm);
        defer lineBuf.deinit();
        // Start with previous string without line delimiter.
        try lineBuf.appendString(readBuf[fileo.curPos..fileo.readBufEnd]);

        // Read into buffer.
        const file = fileo.getStdFile();
        const reader = file.reader();

        while (true) {
            const bytesRead = try reader.read(readBuf);
            if (bytesRead == 0) {
                // End of stream.
                fileo.iterLines = false;
                if (lineBuf.len > 0) {
                    return StringSome(vm, Value.initPtr(try lineBuf.build()));
                } else {
                    return StringNone(vm);
                }
            }
            if (cy.string.getLineEnd(readBuf[0..bytesRead])) |end| {
                // Found new line.
                try lineBuf.appendString(readBuf[0..end]);

                // Advance pos.
                fileo.curPos = @intCast(end);
                fileo.readBufEnd = @intCast(bytesRead);

                return StringSome(vm, Value.initPtr(try lineBuf.build()));
            } else {
                try lineBuf.appendString(readBuf[0..bytesRead]);

                // Advance pos.
                fileo.curPos = @intCast(bytesRead);
                fileo.readBufEnd = @intCast(bytesRead);
            }
        }
    } else {
        return StringNone(vm);
    }
}

const MapNone = cy.builtins.MapNone;
const MapSome = cy.builtins.MapSome;
const StringNone = cy.builtins.StringNone;
const StringSome = cy.builtins.StringSome;

pub fn dirIteratorNext(vm: *cy.VM) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const iter = vm.getHostObject(*DirIterator, 0);
    if (iter.recursive) {
        const walker = cy.ptrAlignCast(*std.fs.Dir.Walker, &iter.inner.walker);
        const entryOpt = try walker.next();
        if (entryOpt) |entry| {
            const mapv = try vm.allocEmptyMap();
            const map = mapv.castHeapObject(*cy.heap.Map);
            const pathKey = try vm.retainOrAllocAstring("path");
            const nameKey = try vm.retainOrAllocAstring("name");
            const typeKey = try vm.retainOrAllocAstring("type");
            defer {
                vm.release(pathKey);
                vm.release(nameKey);
                vm.release(typeKey);
            }
            const entryPath = try vm.allocString(entry.path);
            const entryName = try vm.allocString(entry.basename);
            defer {
                vm.release(entryPath);
                vm.release(entryName);
            }
            try map.set(vm, pathKey, entryPath);
            try map.set(vm, nameKey, entryName);
            const typeTag: cy.bindings.Symbol = switch (entry.kind) {
                .file => .file,
                .directory => .dir,
                else => .unknown,
            };
            try map.set(vm, typeKey, Value.initSymbol(@intFromEnum(typeTag)));
            return MapSome(vm, mapv);
        } else {
            return MapNone(vm);
        }
    } else {
        const stdIter = cy.ptrAlignCast(*std.fs.Dir.Iterator, &iter.inner.iter);
        const entryOpt = try stdIter.next();
        if (entryOpt) |entry| {
            const mapv = try vm.allocEmptyMap();
            const map = mapv.castHeapObject(*cy.heap.Map);
            const nameKey = try vm.retainOrAllocAstring("name");
            const typeKey = try vm.retainOrAllocAstring("type");
            const entryName = try vm.allocString(entry.name);
            defer {
                vm.release(nameKey);
                vm.release(typeKey);
                vm.release(entryName);
            }
            try map.set(vm, nameKey, entryName);
            const typeTag: cy.bindings.Symbol = switch (entry.kind) {
                .file => .file,
                .directory => .dir,
                else => .unknown,
            };
            try map.set(vm, typeKey, Value.initSymbol(@intFromEnum(typeTag)));
            return MapSome(vm, mapv);
        } else {
            return MapNone(vm);
        }
    }
}

pub fn prepareThrowSymbol(vm: *cy.UserVM, sym: cy.bindings.Symbol) Value {
    return vm.prepareThrowSymbol(@intFromEnum(sym));
}