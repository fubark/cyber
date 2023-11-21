const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const log = cy.log.scoped(.fs);
const cy = @import("../cyber.zig");
const Value = cy.Value;
const cc = @import("../clib.zig");

pub var FileT: cy.TypeId = undefined;
pub var DirT: cy.TypeId = undefined;
pub var DirIterT: cy.TypeId = undefined;

pub const File = extern struct {
    readBuf: [*]u8,
    /// Can be up to 8 bytes on windows, otherwise 4 bytes.
    fd: if (cy.hasStdFiles) std.os.fd_t else u32,
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
            .capable_io_mode = .blocking,
            .intended_io_mode = .blocking,
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

pub fn fileFinalizer(vm_: ?*cc.VM, obj: ?*anyopaque) callconv(.C) void {
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

pub fn dirFinalizer(_: ?*cc.VM, obj: ?*anyopaque) callconv(.C) void {
    if (cy.hasStdFiles) {
        const dir: *Dir = @ptrCast(@alignCast(obj));
        dir.close();
    }
}

pub const Dir = extern struct {
    /// Padding to make Dir.fd match the offset of File.fd.
    padding: usize = 0,
    fd: if (cy.hasStdFiles) std.os.fd_t else u32,
    iterable: bool,
    closed: bool,

    pub fn getStdDir(self: *const Dir) std.fs.Dir {
        return std.fs.Dir{
            .fd = self.fd,
        };
    }

    pub fn getStdIterableDir(self: *const Dir) std.fs.IterableDir {
        return std.fs.IterableDir{
            .dir = std.fs.Dir{
                .fd = self.fd,
            },
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

pub fn dirIteratorFinalizer(_: ?*cc.VM, obj: ?*anyopaque) callconv(.C) void {
    if (cy.hasStdFiles) {
        var dir: *DirIterator = @ptrCast(@alignCast(obj));
        if (dir.recursive) {
            const walker = cy.ptrAlignCast(*std.fs.IterableDir.Walker, &dir.inner.walker);
            walker.deinit();   
        }
    }
}

pub fn dirIteratorGetChildren(_: ?*cc.VM, obj: ?*anyopaque) callconv(.C) cc.ValueSlice {
    const dirIter: *DirIterator = @ptrCast(@alignCast(obj));
    return .{
        .ptr = @ptrCast(&dirIter.dir),
        .len = 1,
    };
}

pub const DirIterator = extern struct {
    dir: Value, // `Dir` object.
    inner: extern union {
        iter: if (cy.hasStdFiles) [@sizeOf(std.fs.IterableDir.Iterator)]u8 else void,
        walker: if (cy.hasStdFiles) [@sizeOf(std.fs.IterableDir.Walker)]u8 else void,
    },
    /// If `recursive` is true, `walker` is used.
    recursive: bool,
};

pub fn allocFile(vm: *cy.VM, fd: std.os.fd_t) linksection(cy.StdSection) !Value {
    const file: *File = @ptrCast(@alignCast(try cy.heap.allocHostNoCycObject(vm, FileT, @sizeOf(File))));
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
    return Value.initHostNoCycPtr(file);
}

pub fn allocDir(vm: *cy.VM, fd: std.os.fd_t, iterable: bool) linksection(cy.StdSection) !Value {
    const dir: *Dir = @ptrCast(@alignCast(try cy.heap.allocHostNoCycObject(vm, DirT, @sizeOf(Dir))));
    dir.* = .{
        .fd = fd,
        .iterable = iterable,
        .closed = false,
    };
    return Value.initHostNoCycPtr(dir);
}

pub fn allocDirIterator(vm: *cy.VM, dirv: Value, recursive: bool) linksection(cy.StdSection) !Value {
    const dirIter: *DirIterator = @ptrCast(@alignCast(try cy.heap.allocHostNoCycObject(vm, DirIterT, @sizeOf(DirIterator))));
    dirIter.* = .{
        .dir = dirv,
        .inner = undefined,
        .recursive = recursive,
    };
    const dir = dirv.castHostObject(*Dir);
    if (recursive) {
        const walker = cy.ptrAlignCast(*std.fs.IterableDir.Walker, &dirIter.inner.walker);
        walker.* = try dir.getStdIterableDir().walk(vm.alloc);
    } else {
        const iter = cy.ptrAlignCast(*std.fs.IterableDir.Iterator, &dirIter.inner.iter);
        iter.* = dir.getStdIterableDir().iterate();
    }
    return Value.initHostNoCycPtr(dirIter);
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

pub fn fileStreamLines(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    return fileStreamLines1(vm, &[_]Value{ args[0], Value.initF64(@floatFromInt(4096)) }, nargs);
}

pub fn fileStreamLines1(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    // Don't need to release obj since it's being returned.
    const file = args[0].castHostObject(*File);
    const bufSize: usize = @intCast(args[1].asInteger());
    var createReadBuf = true;
    if (file.hasReadBuf) {
        if (bufSize != file.readBufCap) {
            // Cleanup previous buffer.
            vm.allocator().free(file.readBuf[0..file.readBufCap]);
        } else {
            createReadBuf = false;
        }
    }
    // Allocate read buffer.
    file.iterLines = true;
    if (createReadBuf) {
        const readBuf = vm.allocator().alloc(u8, bufSize) catch cy.fatal();
        file.readBuf = readBuf.ptr;
        file.readBufCap = @intCast(readBuf.len);
        file.hasReadBuf = true;
    }
    return args[0];
}

pub fn dirWalk(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    const dir = args[0].castHostObject(*Dir);
    if (dir.iterable) {
        vm.retain(args[0]);
        return vm.allocDirIterator(args[0], true) catch cy.fatal();
    } else {
        return prepareThrowSymbol(vm, .NotAllowed);
    }
}

pub fn dirIterator(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    const dir = args[0].castHostObject(*Dir);
    if (dir.iterable) {
        vm.retain(args[0]);
        return vm.allocDirIterator(args[0], false) catch cy.fatal();
    } else {
        return prepareThrowSymbol(vm, .NotAllowed);
    }
}

pub fn fileIterator(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");

    // Don't need to release obj since it's being returned.
    const file = args[0].castHostObject(*File);
    file.curPos = 0;
    file.readBufEnd = 0;
    return args[0];
}

pub fn fileSeekFromEnd(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");

    const fileo = args[0].castHostObject(*File);
    if (fileo.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes = args[1].asInteger();
    if (numBytes > 0) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }

    const file = fileo.getStdFile();
    file.seekFromEnd(numBytes) catch |err| {
        return cy.apiUnsupportedError(vm, "seekFromEnd", err, @errorReturnTrace());
    };
    return Value.None;
}

pub fn fileSeekFromCur(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");

    const fileo = args[0].castHostObject(*File);
    if (fileo.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes = args[1].asInteger();

    const file = fileo.getStdFile();
    file.seekBy(numBytes) catch |err| {
        return cy.apiUnsupportedError(vm, "seekFromCur", err, @errorReturnTrace());
    };
    return Value.None;
}

pub fn fileSeek(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");

    const fileo = args[0].castHostObject(*File);
    if (fileo.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes = args[1].asInteger();
    if (numBytes < 0) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }

    const file = fileo.getStdFile();
    const unumBytes: u32 = @intCast(numBytes);
    file.seekTo(unumBytes) catch |err| {
        return cy.apiUnsupportedError(vm, "seek", err, @errorReturnTrace());
    };
    return Value.None;
}

pub fn fileWrite(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const fileo = args[0].castHostObject(*File);
    if (fileo.closed) {
        return vm.prepThrowError(.Closed);
    }

    var buf = try vm.getOrBufPrintValueRawStr(&cy.tempBuf, args[1]);
    const file = fileo.getStdFile();
    const numWritten = try file.write(buf);
    return Value.initInt(@intCast(numWritten));
}

pub fn fileClose(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");

    const file = args[0].castHostObject(*File);
    file.close();
    return Value.None;
}

pub fn fileRead(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const fileo = args[0].castHostObject(*File);
    if (fileo.closed) {
        return vm.prepThrowError(.Closed);
    }

    const numBytes = args[1].asInteger();
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
    return vm.allocArray(tempBuf.buf[0..numRead]);
}

pub fn fileReadToEnd(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");

    const fileo = args[0].castHostObject(*File);
    if (fileo.closed) {
        return vm.prepThrowError(.Closed);
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
            return vm.allocArray(all);
        } else {
            try tempBuf.ensureUnusedCapacity(vm.alloc, MinReadBufSize);
        }
    }
}

pub fn fileOrDirStat(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");

    const obj = args[0].asHeapObject();
    const typeId = obj.getTypeId();
    if (typeId == FileT) {
        const file = args[0].castHostObject(*File);
        if (file.closed) {
            return prepareThrowSymbol(vm, .Closed);
        }
    } else if (typeId == DirT) {
        const dir = args[0].castHostObject(*Dir);
        if (dir.closed) {
            return prepareThrowSymbol(vm, .Closed);
        }
    } else {
        cy.unexpected();
    }

    // File/Dir share the same fd member offset.
    const fileo = args[0].castHostObject(*File);
    const file = fileo.getStdFile();
    const stat = file.stat() catch |err| {
        return cy.apiUnsupportedError(vm, "stat", err, @errorReturnTrace());
    };

    const ivm = vm.internal();

    const map = vm.allocEmptyMap() catch cy.fatal();
    const sizeKey = vm.retainOrAllocAstring("size") catch cy.fatal();
    const modeKey = vm.retainOrAllocAstring("mode") catch cy.fatal();
    const typeKey = vm.retainOrAllocAstring("type") catch cy.fatal();
    const atimeKey = vm.retainOrAllocAstring("atime") catch cy.fatal();
    const ctimeKey = vm.retainOrAllocAstring("ctime") catch cy.fatal();
    const mtimeKey = vm.retainOrAllocAstring("mtime") catch cy.fatal();
    defer {
        vm.release(sizeKey);
        vm.release(modeKey);
        vm.release(typeKey);
        vm.release(atimeKey);
        vm.release(ctimeKey);
        vm.release(mtimeKey);
    }
    map.asHeapObject().map.set(ivm, sizeKey, Value.initF64(@floatFromInt(stat.size))) catch cy.fatal();
    map.asHeapObject().map.set(ivm, modeKey, Value.initF64(@floatFromInt(stat.mode))) catch cy.fatal();
    const typeTag: cy.bindings.Symbol = switch (stat.kind) {
        .file => .file,
        .directory => .dir,
        else => .unknown,
    };
    map.asHeapObject().map.set(ivm, typeKey, Value.initSymbol(@intFromEnum(typeTag))) catch cy.fatal();
    map.asHeapObject().map.set(ivm, atimeKey, Value.initF64(@floatFromInt(@divTrunc(stat.atime, 1000000)))) catch cy.fatal();
    map.asHeapObject().map.set(ivm, ctimeKey, Value.initF64(@floatFromInt(@divTrunc(stat.ctime, 1000000)))) catch cy.fatal();
    map.asHeapObject().map.set(ivm, mtimeKey, Value.initF64(@floatFromInt(@divTrunc(stat.mtime, 1000000)))) catch cy.fatal();
    return map;
}

pub fn fileNext(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");

    const fileo = args[0].castHostObject(*File);
    if (fileo.iterLines) {
        const readBuf = fileo.readBuf[0..fileo.readBufCap];
        if (cy.getLineEnd(readBuf[fileo.curPos..fileo.readBufEnd])) |end| {
            // Found new line.
            const line = try vm.allocArray(readBuf[fileo.curPos..fileo.curPos+end]);

            // Advance pos.
            fileo.curPos += @intCast(end);

            return line;
        }

        var lineBuf = try cy.HeapArrayBuilder.init(vm);
        defer lineBuf.deinit();
        // Start with previous string without line delimiter.
        try lineBuf.appendString(vm.alloc, readBuf[fileo.curPos..fileo.readBufEnd]);

        // Read into buffer.
        const file = fileo.getStdFile();
        const reader = file.reader();

        while (true) {
            const bytesRead = try reader.read(readBuf);
            if (bytesRead == 0) {
                // End of stream.
                fileo.iterLines = false;
                if (lineBuf.len > 0) {
                    return Value.initNoCycPtr(lineBuf.ownObject(vm.alloc));
                } else {
                    return Value.None;
                }
            }
            if (cy.getLineEnd(readBuf[0..bytesRead])) |end| {
                // Found new line.
                try lineBuf.appendString(vm.alloc, readBuf[0..end]);

                // Advance pos.
                fileo.curPos = @intCast(end);
                fileo.readBufEnd = @intCast(bytesRead);

                return Value.initNoCycPtr(lineBuf.ownObject(vm.alloc));
            } else {
                try lineBuf.appendString(vm.alloc, readBuf[0..bytesRead]);

                // Advance pos.
                fileo.curPos = @intCast(bytesRead);
                fileo.readBufEnd = @intCast(bytesRead);
            }
        }
    } else {
        return Value.None;
    }
}

pub fn dirIteratorNext(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");

    const ivm = vm.internal();
    const iter = args[0].castHostObject(*DirIterator);
    if (iter.recursive) {
        const walker = cy.ptrAlignCast(*std.fs.IterableDir.Walker, &iter.inner.walker);
        const entryOpt = walker.next() catch |err| {
            return cy.apiUnsupportedError(vm, "next", err, @errorReturnTrace());
        };
        if (entryOpt) |entry| {
            const map = vm.allocEmptyMap() catch cy.fatal();
            const pathKey = vm.retainOrAllocAstring("path") catch cy.fatal();
            const nameKey = vm.retainOrAllocAstring("name") catch cy.fatal();
            const typeKey = vm.retainOrAllocAstring("type") catch cy.fatal();
            defer {
                vm.release(pathKey);
                vm.release(nameKey);
                vm.release(typeKey);
            }
            const entryPath = cy.heap.retainOrAllocPreferString(ivm, entry.path) catch cy.fatal();
            const entryName = cy.heap.retainOrAllocPreferString(ivm, entry.basename) catch cy.fatal();
            defer {
                vm.release(entryPath);
                vm.release(entryName);
            }
            map.asHeapObject().map.set(ivm, pathKey, entryPath) catch cy.fatal();
            map.asHeapObject().map.set(ivm, nameKey, entryName) catch cy.fatal();
            const typeTag: cy.bindings.Symbol = switch (entry.kind) {
                .file => .file,
                .directory => .dir,
                else => .unknown,
            };
            map.asHeapObject().map.set(ivm, typeKey, Value.initSymbol(@intFromEnum(typeTag))) catch cy.fatal();
            return map;
        } else {
            return Value.None;
        }
    } else {
        const stdIter = cy.ptrAlignCast(*std.fs.IterableDir.Iterator, &iter.inner.iter);
        const entryOpt = stdIter.next() catch |err| {
            return cy.apiUnsupportedError(vm, "next", err, @errorReturnTrace());
        };
        if (entryOpt) |entry| {
            const map = vm.allocEmptyMap() catch cy.fatal();
            const nameKey = vm.retainOrAllocAstring("name") catch cy.fatal();
            const typeKey = vm.retainOrAllocAstring("type") catch cy.fatal();
            const entryName = cy.heap.retainOrAllocPreferString(ivm, entry.name) catch cy.fatal();
            defer {
                vm.release(nameKey);
                vm.release(typeKey);
                vm.release(entryName);
            }
            map.asHeapObject().map.set(ivm, nameKey, entryName) catch cy.fatal();
            const typeTag: cy.bindings.Symbol = switch (entry.kind) {
                .file => .file,
                .directory => .dir,
                else => .unknown,
            };
            map.asHeapObject().map.set(ivm, typeKey, Value.initSymbol(@intFromEnum(typeTag))) catch cy.fatal();
            return map;
        } else {
            return Value.None;
        }
    }
}


pub fn prepareThrowSymbol(vm: *cy.UserVM, sym: cy.bindings.Symbol) Value {
    return vm.prepareThrowSymbol(@intFromEnum(sym));
}