// Copyright (c) 2023 Cyber (See LICENSE)

/// Heap objects, object allocation and deinitializers.

const cy = @import("cyber.zig");
const Value = cy.Value;
const stdx = @import("stdx");
const t = stdx.testing;
const std = @import("std");
const builtin = @import("builtin");
const tcc = @import("tcc");
const log = stdx.log.scoped(.heap);
const NullId = std.math.maxInt(u32);
const NullU8 = std.math.maxInt(u8);

/// Reserved object types known at comptime.
/// Starts at 9 since primitive types go up to 8.
pub const ListS: cy.TypeId = 9;
pub const ListIteratorT: cy.TypeId = 10;
pub const MapS: cy.TypeId = 11;
pub const MapIteratorT: cy.TypeId = 12;
pub const ClosureS: cy.TypeId = 13;
pub const LambdaS: cy.TypeId = 14;
pub const AstringT: cy.TypeId = 15;
pub const UstringT: cy.TypeId = 16;
pub const StringSliceT: cy.TypeId = 17;
pub const RawStringT: cy.TypeId = 18;
pub const RawStringSliceT: cy.TypeId = 19;
pub const FiberS: cy.TypeId = 20;
pub const BoxS: cy.TypeId = 21;
pub const NativeFunc1S: cy.TypeId = 22;
pub const TccStateS: cy.TypeId = 23;
pub const OpaquePtrS: cy.TypeId = 24;
pub const FileT: cy.TypeId = 25;
pub const DirT: cy.TypeId = 26;
pub const DirIteratorT: cy.TypeId = 27;
pub const SymbolT: cy.TypeId = 28;

// Keep it just under 4kb page.
pub const HeapPage = struct {
    objects: [102]HeapObject,
};

const HeapObjectId = u32;

/// Total of 40 bytes per object. If objects are bigger, they are allocated on the gpa.
pub const HeapObject = extern union {
    common: extern struct {
        structId: cy.TypeId,
    },
    freeSpan: extern struct {
        structId: cy.TypeId,
        len: u32,
        start: *HeapObject,
        next: ?*HeapObject,
    },
    retainedCommon: extern struct {
        structId: cy.TypeId,
        rc: u32,
    },
    list: List,
    listIter: ListIterator,
    fiber: cy.Fiber,
    map: Map,
    mapIter: MapIterator,
    closure: Closure,
    lambda: Lambda,
    astring: Astring,
    ustring: Ustring,
    stringSlice: StringSlice,
    rawstring: RawString,
    rawstringSlice: RawStringSlice,
    object: Object,
    box: Box,
    nativeFunc1: NativeFunc1,
    tccState: if (cy.hasJit) TccState else void,
    file: if (cy.hasStdFiles) File else void,
    dir: if (cy.hasStdFiles) Dir else void,
    opaquePtr: OpaquePtr,
    symbol: Symbol,

    pub fn getUserTag(self: *const HeapObject) cy.ValueUserTag {
        switch (self.common.structId) {
            cy.ListS => return .list,
            cy.MapS => return .map,
            cy.AstringT => return .string,
            cy.UstringT => return .string,
            cy.RawStringT => return .rawstring,
            cy.ClosureS => return .closure,
            cy.LambdaS => return .lambda,
            cy.FiberS => return .fiber,
            cy.NativeFunc1S => return .nativeFunc,
            cy.TccStateS => return .tccState,
            cy.OpaquePtrS => return .opaquePtr,
            cy.FileT => return .file,
            cy.DirT => return .dir,
            cy.DirIteratorT => return .dirIter,
            cy.BoxS => return .box,
            else => {
                return .object;
            },
        }
    }
};

pub const SymbolType = enum {
    object,
};

pub const Symbol = extern struct {
    structId: cy.TypeId,
    rc: u32,
    symType: u32,
    symId: u32,
};

pub const List = extern struct {
    structId: cy.TypeId,
    rc: u32,
    list: extern struct {
        ptr: [*]Value,
        cap: usize,
        len: usize,
    },

    pub inline fn items(self: *const List) []Value {
        return self.list.ptr[0..self.list.len];
    }

    /// Assumes `val` is retained.
    pub fn append(self: *List, alloc: std.mem.Allocator, val: Value) linksection(cy.Section) void {
        const list = stdx.ptrAlignCast(*cy.List(Value), &self.list);
        if (list.len == list.buf.len) {
            // After reaching a certain size, use power of two ceil.
            // This reduces allocations for big lists while not over allocating for smaller lists.
            if (list.len > 512) {
                const newCap = std.math.ceilPowerOfTwo(u32, @intCast(u32, list.len) + 1) catch stdx.fatal();
                list.growTotalCapacityPrecise(alloc, newCap) catch stdx.fatal();
            } else {
                list.growTotalCapacity(alloc, list.len + 1) catch stdx.fatal();
            }
        }
        list.appendAssumeCapacity(val);
    }
};

pub const ListIterator = extern struct {
    structId: cy.TypeId,
    rc: u32,
    list: *List,
    nextIdx: u32,
};

pub const MapInner = cy.ValueMap;
pub const Map = extern struct {
    structId: cy.TypeId,
    rc: u32,
    inner: extern struct {
        metadata: ?[*]u64,
        entries: ?[*]cy.ValueMapEntry,
        size: u32,
        cap: u32,
        available: u32,
        /// This prevents `inner` from having offset=0 in Map.
        /// Although @offsetOf(Map, "inner") returns 8 in that case &map.inner returns the same address as &map.
        padding: u32 = 0,
    },
};

pub const MapIterator = extern struct {
    structId: cy.TypeId,
    rc: u32,
    map: *Map,
    nextIdx: u32,
};

pub const Closure = extern struct {
    structId: cy.TypeId,
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    numCaptured: u8,
    /// Includes locals, captured vars, and return info. Does not include params.
    numLocals: u8,
    padding: u8,
    firstCapturedVal: Value,

    pub inline fn getCapturedValuesPtr(self: *Closure) [*]Value {
        return @ptrCast([*]Value, &self.firstCapturedVal);
    }
};

const Lambda = extern struct {
    structId: cy.TypeId,
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    /// Includes locals and return info. Does not include params.
    numLocals: u8,
};

/// 28 byte length can fit inside a Heap pool object.
pub const MaxPoolObjectAstringByteLen = 28;

pub const Astring = extern struct {
    structId: cy.TypeId,
    rc: u32,
    len: u32,
    bufStart: u8,

    const BufOffset = @offsetOf(Astring, "bufStart");

    pub inline fn getSlice(self: *Astring) []u8 {
        return @ptrCast([*]u8, &self.bufStart)[0..self.len];
    }

    pub inline fn getConstSlice(self: *const Astring) []const u8 {
        return @ptrCast([*]const u8, &self.bufStart)[0..self.len];
    }
};

/// 16 byte length can fit inside a Heap pool object.
pub const MaxPoolObjectUstringByteLen = 16;

const Ustring = extern struct {
    structId: cy.TypeId,
    rc: u32,
    len: u32,
    charLen: u32,
    mruIdx: u32,
    mruCharIdx: u32,
    bufStart: u8,

    const BufOffset = @offsetOf(Ustring, "bufStart");

    pub inline fn getSlice(self: *Ustring) []u8 {
        return @ptrCast([*]u8, &self.bufStart)[0..self.len];
    }

    pub inline fn getConstSlice(self: *const Ustring) []const u8 {
        return @ptrCast([*]const u8, &self.bufStart)[0..self.len];
    }
};

const StringSlice = extern struct {
    structId: cy.TypeId,
    rc: u32,
    buf: [*]const u8,
    len: u32,

    uCharLen: u32,
    uMruIdx: u32,
    uMruCharIdx: u32,

    /// A Ustring slice may have a null or 0 parentPtr if it's sliced from StaticUstring.
    /// The lower 63 bits contains the parentPtr.
    /// The last bit contains an isAscii flag.
    extra: u64,

    pub inline fn getParentPtr(self: *const StringSlice) ?*cy.HeapObject {
        return @intToPtr(?*cy.HeapObject, @intCast(usize, self.extra & 0x7fffffffffffffff));
    }

    pub inline fn isAstring(self: *const StringSlice) bool {
        return self.extra & (1 << 63) > 0;
    }

    pub inline fn getConstSlice(self: *const StringSlice) []const u8 {
        return self.buf[0..self.len];
    }
};

pub const MaxPoolObjectRawStringByteLen = 28;

pub const RawString = extern struct {
    structId: if (cy.isWasm) cy.TypeId else cy.TypeId align(8),
    rc: u32,
    len: u32,
    bufStart: u8,

    pub const BufOffset = @offsetOf(RawString, "bufStart");

    pub inline fn getSlice(self: *RawString) []u8 {
        return @ptrCast([*]u8, &self.bufStart)[0..self.len];
    }

    pub inline fn getConstSlice(self: *const RawString) []const u8 {
        return @ptrCast([*]const u8, &self.bufStart)[0..self.len];
    }
};

const RawStringSlice = extern struct {
    structId: cy.TypeId,
    rc: u32,
    buf: [*]const u8,
    len: u32,
    padding: u32 = 0,
    parent: *RawString,

    pub inline fn getConstSlice(self: *const RawStringSlice) []const u8 {
        return self.buf[0..self.len];
    }
};

pub const Object = extern struct {
    structId: cy.TypeId,
    rc: u32,
    firstValue: Value,

    pub inline fn getValuesConstPtr(self: *const Object) [*]const Value {
        return @ptrCast([*]const Value, &self.firstValue);
    }

    pub inline fn getValuesPtr(self: *Object) [*]Value {
        return @ptrCast([*]Value, &self.firstValue);
    }

    pub inline fn getValuePtr(self: *Object, idx: u32) *Value {
        return @ptrCast(*Value, @ptrCast([*]Value, &self.firstValue) + idx);
    }

    pub inline fn getValue(self: *const Object, idx: u32) Value {
        return @ptrCast([*]const Value, &self.firstValue)[idx];
    }
};

const Box = extern struct {
    structId: cy.TypeId,
    rc: u32,
    val: Value,
};

const NativeFunc1 = extern struct {
    structId: cy.TypeId,
    rc: u32,
    func: *const fn (*cy.UserVM, [*]const Value, u8) Value,
    numParams: u32,
    tccState: Value,
    hasTccState: bool,
};

const TccState = extern struct {
    structId: cy.TypeId,
    rc: u32,
    state: *tcc.TCCState,
    lib: *std.DynLib,
};

const File = extern struct {
    structId: cy.TypeId,
    rc: u32,
    readBuf: [*]u8,
    /// Can be up to 8 bytes on windows, otherwise 4 bytes.
    fd: std.os.fd_t,
    curPos: u32,
    readBufCap: u32,
    readBufEnd: u32,
    iterLines: bool,
    hasReadBuf: bool,
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

pub const DirIterator = extern struct {
    structId: cy.TypeId,
    rc: u32,
    dir: *Dir,
    inner: extern union {
        iter: if (cy.hasStdFiles) [@sizeOf(std.fs.IterableDir.Iterator)]u8 else void,
        walker: if (cy.hasStdFiles) [@sizeOf(std.fs.IterableDir.Walker)]u8 else void,
    },
    /// If `recursive` is true, `walker` is used.
    recursive: bool,
};

pub const Dir = extern struct {
    structId: cy.TypeId align(8),
    rc: u32,
    /// Padding to make Dir.fd match the offset of File.fd.
    padding: usize = 0,
    fd: if (cy.hasStdFiles) std.os.fd_t else u32,
    iterable: bool,

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
};

pub const OpaquePtr = extern struct {
    structId: cy.TypeId,
    rc: u32,
    ptr: ?*anyopaque,
};

/// Initializes the page with freed object slots and returns the pointer to the first slot.
fn initHeapPage(page: *HeapPage) *HeapObject {
    // First HeapObject at index 0 is reserved so that freeObject can get the previous slot without a bounds check.
    page.objects[0].common = .{
        .structId = 0, // Non-NullId so freeObject doesn't think it's a free span.
    };
    const first = &page.objects[1];
    first.freeSpan = .{
        .structId = NullId,
        .len = page.objects.len - 1,
        .start = first,
        .next = null,
    };
    // The rest initialize as free spans so checkMemory doesn't think they are retained objects.
    std.mem.set(HeapObject, page.objects[2..], .{
        .common = .{
            .structId = NullId,
        }
    });
    page.objects[page.objects.len-1].freeSpan.start = first;
    return first;
}

/// Returns the first free HeapObject.
pub fn growHeapPages(self: *cy.VM, numPages: usize) !*HeapObject {
    var idx = self.heapPages.len;
    try self.heapPages.resize(self.alloc, self.heapPages.len + numPages);

    // Allocate first page.
    var page = try self.alloc.create(HeapPage);
    self.heapPages.buf[idx] = page;

    const first = initHeapPage(page);
    var last = first;
    idx += 1;
    while (idx < self.heapPages.len) : (idx += 1) {
        page = try self.alloc.create(HeapPage);
        self.heapPages.buf[idx] = page;
        const first_ = initHeapPage(page);
        last.freeSpan.next = first_;
        last = first_;
    }
    return first;
}

pub fn allocPoolObject(self: *cy.VM) linksection(cy.HotSection) !*HeapObject {
    if (self.heapFreeHead == null) {
        self.heapFreeHead = try growHeapPages(self, std.math.max(1, (self.heapPages.len * 15) / 10));
    }
    const ptr = self.heapFreeHead.?;
    if (ptr.freeSpan.len == 1) {
        // This is the only free slot, move to the next free span.
        self.heapFreeHead = ptr.freeSpan.next;

        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        return ptr;
    } else {
        const next = &@ptrCast([*]HeapObject, ptr)[1];
        next.freeSpan = .{
            .structId = NullId,
            .len = ptr.freeSpan.len - 1,
            .start = next,
            .next = ptr.freeSpan.next,
        };
        const last = &@ptrCast([*]HeapObject, ptr)[ptr.freeSpan.len-1];
        last.freeSpan.start = next;
        self.heapFreeHead = next;

        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (builtin.mode == .Debug) {
            // log.debug("alloc {*}", .{ptr});
            self.objectTraceMap.put(self.alloc, ptr, .{
                .allocPc = cy.pcOffset(self, self.debugPc),
                .freePc = cy.NullId,
            }) catch stdx.fatal();
        }
        return ptr;
    }
}

pub fn freePoolObject(self: *cy.VM, obj: *HeapObject) linksection(cy.HotSection) void {
    const prev = &(@ptrCast([*]HeapObject, obj) - 1)[0];
    if (prev.common.structId == NullId) {
        // Left is a free span. Extend length.
        prev.freeSpan.start.freeSpan.len += 1;
        obj.freeSpan.start = prev.freeSpan.start;
    } else {
        // Add single slot free span.
        obj.freeSpan = .{
            .structId = NullId,
            .len = 1,
            .start = obj,
            .next = self.heapFreeHead,
        };
        self.heapFreeHead = obj;
    }
}

pub fn allocSymbol(self: *cy.VM, symType: u8, symId: u32) !Value {
    const obj = try allocPoolObject(self);
    obj.symbol = .{
        .structId = SymbolT,
        .rc = 1,
        .symType = symType,
        .symId = symId,
    };
    return Value.initPtr(obj);
}

pub fn allocEmptyList(self: *cy.VM) linksection(cy.Section) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .structId = ListS,
        .rc = 1,
        .list = .{
            .ptr = undefined,
            .len = 0,
            .cap = 0,
        },
    };
    return Value.initPtr(obj);
}

pub fn allocOwnedList(self: *cy.VM, elems: []Value) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .structId = ListS,
        .rc = 1,
        .list = .{
            .ptr = elems.ptr,
            .len = elems.len,
            .cap = elems.len,
        },
    };
    return Value.initPtr(obj);
}

pub fn allocListFill(self: *cy.VM, val: Value, n: u32) linksection(cy.StdSection) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .structId = ListS,
        .rc = 1,
        .list = .{
            .ptr = undefined,
            .len = 0,
            .cap = 0,
        },
    };
    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
    // Initializes capacity to exact size.
    try list.ensureTotalCapacityPrecise(self.alloc, n);
    list.len = n;
    if (!val.isPointer()) {
        std.mem.set(Value, list.items(), val);
    } else {
        var i: u32 = 0;
        while (i < n) : (i += 1) {
            list.buf[i] = cy.value.shallowCopy(self, val);
        }
    }
    return Value.initPtr(obj);
}

pub fn allocList(self: *cy.VM, elems: []const Value) linksection(cy.HotSection) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .structId = ListS,
        .rc = 1,
        .list = .{
            .ptr = undefined,
            .len = 0,
            .cap = 0,
        },
    };
    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
    // Initializes capacity to exact size.
    try list.ensureTotalCapacityPrecise(self.alloc, elems.len);
    list.len = elems.len;
    std.mem.copy(Value, list.items(), elems);
    return Value.initPtr(obj);
}

/// Assumes list is already retained for the iterator.
pub fn allocListIterator(self: *cy.VM, list: *List) linksection(cy.HotSection) !Value {
    const obj = try allocPoolObject(self);
    obj.listIter = .{
        .structId = ListIteratorT,
        .rc = 1,
        .list = list,
        .nextIdx = 0,
    };
    return Value.initPtr(obj);
}

pub fn allocEmptyMap(self: *cy.VM) !Value {
    const obj = try allocPoolObject(self);
    obj.map = .{
        .structId = MapS,
        .rc = 1,
        .inner = .{
            .metadata = null,
            .entries = null,
            .size = 0,
            .cap = 0,
            .available = 0,
        },
    };
    return Value.initPtr(obj);
}

pub fn allocMap(self: *cy.VM, keyIdxs: []const cy.OpData, vals: []const Value) !Value {
    const obj = try allocPoolObject(self);
    obj.map = .{
        .structId = MapS,
        .rc = 1,
        .inner = .{
            .metadata = null,
            .entries = null,
            .size = 0,
            .cap = 0,
            .available = 0,
        },
    };

    const inner = @ptrCast(*MapInner, &obj.map.inner);
    for (keyIdxs) |idx, i| {
        const val = vals[i];

        const keyVal = Value{ .val = self.consts[idx.arg].val };
        const res = try inner.getOrPut(self.alloc, self, keyVal);
        if (res.foundExisting) {
            // TODO: Handle reference count.
            res.valuePtr.* = val;
        } else {
            res.valuePtr.* = val;
        }
    }

    return Value.initPtr(obj);
}

/// Assumes map is already retained for the iterator.
pub fn allocMapIterator(self: *cy.VM, map: *Map) linksection(cy.HotSection) !Value {
    const obj = try allocPoolObject(self);
    obj.mapIter = .{
        .structId = MapIteratorT,
        .rc = 1,
        .map = map,
        .nextIdx = 0,
    };
    return Value.initPtr(obj);
}

pub fn allocEmptyClosure(self: *cy.VM, funcPc: usize, numParams: u8, numLocals: u8, numCaptured: u8) !Value {
    var obj: *HeapObject = undefined;
    if (numCaptured <= 3) {
        obj = try allocPoolObject(self);
    } else {
        const objSlice = try self.alloc.alignedAlloc(Value, @alignOf(HeapObject), 2 + numCaptured);
        obj = @ptrCast(*HeapObject, objSlice.ptr);
        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
    }
    obj.closure = .{
        .structId = ClosureS,
        .rc = 1,
        .funcPc = @intCast(u32, funcPc),
        .numParams = numParams,
        .numLocals = numLocals,
        .numCaptured = numCaptured,
        .padding = undefined,
        .firstCapturedVal = undefined,
    };
    const dst = obj.closure.getCapturedValuesPtr()[0..numCaptured];
    std.mem.set(Value, dst, Value.None);
    return Value.initPtr(obj);
}

pub fn allocClosure(self: *cy.VM, framePtr: [*]Value, funcPc: usize, numParams: u8, numLocals: u8, capturedVals: []const cy.OpData) !Value {
    var obj: *HeapObject = undefined;
    if (capturedVals.len <= 3) {
        obj = try allocPoolObject(self);
    } else {
        const objSlice = try self.alloc.alignedAlloc(Value, @alignOf(HeapObject), 2 + capturedVals.len);
        obj = @ptrCast(*HeapObject, objSlice.ptr);
        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
    }
    obj.closure = .{
        .structId = ClosureS,
        .rc = 1,
        .funcPc = @intCast(u32, funcPc),
        .numParams = numParams,
        .numLocals = numLocals,
        .numCaptured = @intCast(u8, capturedVals.len),
        .padding = undefined,
        .firstCapturedVal = undefined,
    };
    const dst = obj.closure.getCapturedValuesPtr();
    for (capturedVals) |local, i| {
        dst[i] = framePtr[local.arg];
    }
    return Value.initPtr(obj);
}

pub fn allocLambda(self: *cy.VM, funcPc: usize, numParams: u8, numLocals: u8) !Value {
    const obj = try allocPoolObject(self);
    obj.lambda = .{
        .structId = LambdaS,
        .rc = 1,
        .funcPc = @intCast(u32, funcPc),
        .numParams = numParams,
        .numLocals = numLocals,
    };
    return Value.initPtr(obj);
}

pub fn allocStringTemplate(self: *cy.VM, strs: []const cy.OpData, vals: []const Value) !Value {
    const firstStr = self.valueAsStaticString(Value.initRaw(self.consts[strs[0].arg].val));
    try self.u8Buf.resize(self.alloc, firstStr.len);
    std.mem.copy(u8, self.u8Buf.items(), firstStr);

    const writer = self.u8Buf.writer(self.alloc);
    for (vals) |val, i| {
        self.writeValueToString(writer, val);
        cy.arc.release(self, val);
        try self.u8Buf.appendSlice(self.alloc, self.valueAsStaticString(Value.initRaw(self.consts[strs[i+1].arg].val)));
    }

    // TODO: As string is built, accumulate charLen and detect rawstring to avoid doing validation.
    return getOrAllocStringInfer(self, self.u8Buf.items());
}

pub fn getOrAllocOwnedAstring(self: *cy.VM, obj: *HeapObject) linksection(cy.HotSection) !Value {
    return getOrAllocOwnedString(self, obj, obj.astring.getConstSlice());
}

pub fn getOrAllocOwnedUstring(self: *cy.VM, obj: *HeapObject) linksection(cy.HotSection) !Value {
    return getOrAllocOwnedString(self, obj, obj.ustring.getConstSlice());
}

pub fn allocRawStringConcat(self: *cy.VM, str: []const u8, str2: []const u8) linksection(cy.Section) !Value {
    const len = @intCast(u32, str.len + str2.len);
    var obj: *HeapObject = undefined;
    if (len <= MaxPoolObjectRawStringByteLen) {
        obj = try allocPoolObject(self);
    } else {
        const objSlice = try self.alloc.alignedAlloc(u8, @alignOf(HeapObject), len + 12);
        obj = @ptrCast(*HeapObject, objSlice.ptr);
        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
    }
    obj.rawstring = .{
        .structId = RawStringT,
        .rc = 1,
        .len = len,
        .bufStart = undefined,
    };
    const dst = @ptrCast([*]u8, &obj.rawstring.bufStart)[0..len];
    std.mem.copy(u8, dst[0..str.len], str);
    std.mem.copy(u8, dst[str.len..], str2);
    return Value.initPtr(obj);
}

fn allocUstringConcat3Object(self: *cy.VM, str1: []const u8, str2: []const u8, str3: []const u8, charLen: u32) linksection(cy.Section) !*HeapObject {
    const obj = try allocUnsetUstringObject(self, str1.len + str2.len + str3.len, charLen);
    const dst = obj.ustring.getSlice();
    std.mem.copy(u8, dst[0..str1.len], str1);
    std.mem.copy(u8, dst[str1.len..str1.len+str2.len], str2);
    std.mem.copy(u8, dst[str1.len+str2.len..], str3);
    return obj;
}

fn allocUstringConcatObject(self: *cy.VM, str1: []const u8, str2: []const u8, charLen: u32) linksection(cy.Section) !*HeapObject {
    const obj = try allocUnsetUstringObject(self, str1.len + str2.len, charLen);
    const dst = obj.ustring.getSlice();
    std.mem.copy(u8, dst[0..str1.len], str1);
    std.mem.copy(u8, dst[str1.len..], str2);
    return obj;
}
    
fn allocAstringObject(self: *cy.VM, str: []const u8) linksection(cy.Section) !*HeapObject {
    const obj = try allocUnsetAstringObject(self, str.len);
    const dst = obj.astring.getSlice();
    std.mem.copy(u8, dst, str);
    return obj;
}

fn allocAstringConcat3Object(self: *cy.VM, str1: []const u8, str2: []const u8, str3: []const u8) linksection(cy.Section) !*HeapObject {
    const obj = try allocUnsetAstringObject(self, str1.len + str2.len + str3.len);
    const dst = obj.astring.getSlice();
    std.mem.copy(u8, dst[0..str1.len], str1);
    std.mem.copy(u8, dst[str1.len..str1.len+str2.len], str2);
    std.mem.copy(u8, dst[str1.len+str2.len..], str3);
    return obj;
}

fn allocAstringConcatObject(self: *cy.VM, str1: []const u8, str2: []const u8) linksection(cy.Section) !*HeapObject {
    const obj = try allocUnsetAstringObject(self, str1.len + str2.len);
    const dst = obj.astring.getSlice();
    std.mem.copy(u8, dst[0..str1.len], str1);
    std.mem.copy(u8, dst[str1.len..], str2);
    return obj;
}

pub fn getOrAllocAstringConcat(self: *cy.VM, str: []const u8, str2: []const u8) linksection(cy.Section) !Value {
    if (str.len + str2.len <= DefaultStringInternMaxByteLen) {
        const ctx = cy.string.StringConcatContext{};
        const concat = cy.string.StringConcat{
            .left = str,
            .right = str2,
        };
        const res = try self.strInterns.getOrPutAdapted(self.alloc, concat, ctx);
        if (res.found_existing) {
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initPtr(res.value_ptr.*);
        } else {
            const obj = try allocAstringConcatObject(self, str, str2);
            res.key_ptr.* = obj.astring.getConstSlice();
            res.value_ptr.* = obj;
            return Value.initPtr(obj);
        }
    } else {
        const obj = try allocAstringConcatObject(self, str, str2);
        return Value.initPtr(obj);
    }
}

pub fn getOrAllocAstringConcat3(self: *cy.VM, str1: []const u8, str2: []const u8, str3: []const u8) linksection(cy.Section) !Value {
    if (str1.len + str2.len + str3.len <= DefaultStringInternMaxByteLen) {
        const ctx = cy.string.StringConcat3Context{};
        const concat = cy.string.StringConcat3{
            .str1 = str1,
            .str2 = str2,
            .str3 = str3,
        };
        const res = try self.strInterns.getOrPutAdapted(self.alloc, concat, ctx);
        if (res.found_existing) {
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initPtr(res.value_ptr.*);
        } else {
            const obj = try allocAstringConcat3Object(self, str1, str2, str3);
            res.key_ptr.* = obj.astring.getConstSlice();
            res.value_ptr.* = obj;
            return Value.initPtr(obj);
        }
    } else {
        const obj = try allocAstringConcat3Object(self, str1, str2, str3);
        return Value.initPtr(obj);
    }
}

pub fn getOrAllocUstringConcat3(self: *cy.VM, str1: []const u8, str2: []const u8, str3: []const u8, charLen: u32) linksection(cy.Section) !Value {
    if (str1.len + str2.len + str3.len <= DefaultStringInternMaxByteLen) {
        const ctx = cy.string.StringConcat3Context{};
        const concat = cy.string.StringConcat3{
            .str1 = str1,
            .str2 = str2,
            .str3 = str3,
        };
        const res = try self.strInterns.getOrPutAdapted(self.alloc, concat, ctx);
        if (res.found_existing) {
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initPtr(res.value_ptr.*);
        } else {
            const obj = try allocUstringConcat3Object(self, str1, str2, str3, charLen);
            res.key_ptr.* = obj.ustring.getConstSlice();
            res.value_ptr.* = obj;
            return Value.initPtr(obj);
        }
    } else {
        const obj = try allocUstringConcat3Object(self, str1, str2, str3, charLen);
        return Value.initPtr(obj);
    }
}

pub fn getOrAllocUstringConcat(self: *cy.VM, str: []const u8, str2: []const u8, charLen: u32) linksection(cy.Section) !Value {
    if (str.len + str2.len <= DefaultStringInternMaxByteLen) {
        const ctx = cy.string.StringConcatContext{};
        const concat = cy.string.StringConcat{
            .left = str,
            .right = str2,
        };
        const res = try self.strInterns.getOrPutAdapted(self.alloc, concat, ctx);
        if (res.found_existing) {
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initPtr(res.value_ptr.*);
        } else {
            const obj = try allocUstringConcatObject(self, str, str2, charLen);
            res.key_ptr.* = obj.ustring.getConstSlice();
            res.value_ptr.* = obj;
            return Value.initPtr(obj);
        }
    } else {
        const obj = try allocUstringConcatObject(self, str, str2, charLen);
        return Value.initPtr(obj);
    }
}

const DefaultStringInternMaxByteLen = 64;

// If no such string intern exists, `obj` is added as a string intern.
// Otherwise, `obj` is released and the existing string intern is retained and returned.
pub fn getOrAllocOwnedString(self: *cy.VM, obj: *HeapObject, str: []const u8) linksection(cy.HotSection) !Value {
    if (str.len <= DefaultStringInternMaxByteLen) {
        const res = try self.strInterns.getOrPut(self.alloc, str);
        if (res.found_existing) {
            cy.arc.releaseObject(self, obj);
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initPtr(res.value_ptr.*);
        } else {
            res.key_ptr.* = str;
            res.value_ptr.* = obj;
            return Value.initPtr(obj);
        }
    } else {
        return Value.initPtr(obj);
    }
}

pub fn getOrAllocStringInfer(self: *cy.VM, str: []const u8) linksection(cy.Section) !Value {
    if (cy.validateUtf8(str)) |charLen| {
        if (str.len == charLen) {
            return try getOrAllocAstring(self, str);
        } else {
            return try getOrAllocUstring(self, str, @intCast(u32, charLen));
        }
    } else {
        return allocRawString(self, str);
    }
}

pub fn allocAstring(self: *cy.VM, str: []const u8) linksection(cy.Section) !Value {
    const obj = try allocUnsetAstringObject(self, str.len);
    const dst = obj.astring.getSlice();
    std.mem.copy(u8, dst, str);
    return Value.initPtr(obj);
}

pub fn allocUnsetAstringObject(self: *cy.VM, len: usize) linksection(cy.Section) !*HeapObject {
    var obj: *HeapObject = undefined;
    if (len <= MaxPoolObjectAstringByteLen) {
        obj = try allocPoolObject(self);
    } else {
        const objSlice = try self.alloc.alignedAlloc(u8, @alignOf(HeapObject), len + Astring.BufOffset);
        obj = @ptrCast(*HeapObject, objSlice.ptr);
        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
    }
    obj.astring = .{
        .structId = AstringT,
        .rc = 1,
        .len = @intCast(u32, len),
        .bufStart = undefined,
    };
    return obj;
}

pub fn getOrAllocUstring(self: *cy.VM, str: []const u8, charLen: u32) linksection(cy.Section) !Value {
    if (str.len <= DefaultStringInternMaxByteLen) {
        const res = try self.strInterns.getOrPut(self.alloc, str);
        if (res.found_existing) {
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initPtr(res.value_ptr.*);
        } else {
            const obj = try allocUstringObject(self, str, charLen);
            res.key_ptr.* = obj.ustring.getConstSlice();
            res.value_ptr.* = obj;
            return Value.initPtr(obj);
        }
    } else {
        const obj = try allocUstringObject(self, str, charLen);
        return Value.initPtr(obj);
    }
}

pub fn getOrAllocAstring(self: *cy.VM, str: []const u8) linksection(cy.Section) !Value {
    if (str.len <= DefaultStringInternMaxByteLen) {
        const res = try self.strInterns.getOrPut(self.alloc, str);
        if (res.found_existing) {
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initPtr(res.value_ptr.*);
        } else {
            const obj = try allocAstringObject(self, str);
            res.key_ptr.* = obj.astring.getConstSlice();
            res.value_ptr.* = obj;
            return Value.initPtr(obj);
        }
    } else {
        const obj = try allocAstringObject(self, str);
        return Value.initPtr(obj);
    }
}

pub fn allocUstring(self: *cy.VM, str: []const u8, charLen: u32) linksection(cy.Section) !Value {
    const obj = try allocUstringObject(self, str, charLen);
    return Value.initPtr(obj);
}

pub fn allocUstringObject(self: *cy.VM, str: []const u8, charLen: u32) linksection(cy.Section) !*HeapObject {
    const obj = try allocUnsetUstringObject(self, str.len, charLen);
    const dst = obj.ustring.getSlice();
    std.mem.copy(u8, dst, str);
    return obj;
}

pub fn allocUnsetUstringObject(self: *cy.VM, len: usize, charLen: u32) linksection(cy.Section) !*HeapObject {
    var obj: *HeapObject = undefined;
    if (len <= MaxPoolObjectUstringByteLen) {
        obj = try allocPoolObject(self);
    } else {
        const objSlice = try self.alloc.alignedAlloc(u8, @alignOf(HeapObject), len + Ustring.BufOffset);
        obj = @ptrCast(*HeapObject, objSlice.ptr);
        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
    }
    obj.ustring = .{
        .structId = UstringT,
        .rc = 1,
        .len = @intCast(u32, len),
        .charLen = charLen,
        .mruIdx = 0,
        .mruCharIdx = 0,
        .bufStart = undefined,
    };
    return obj;
}

pub fn allocUstringSlice(self: *cy.VM, slice: []const u8, charLen: u32, parent: ?*HeapObject) !Value {
    const obj = try allocPoolObject(self);
    obj.stringSlice = .{
        .structId = StringSliceT,
        .rc = 1,
        .buf = slice.ptr,
        .len = @intCast(u32, slice.len),
        .uCharLen = charLen,
        .uMruIdx = 0,
        .uMruCharIdx = 0,
        .extra = @intCast(u63, @ptrToInt(parent)),
    };
    return Value.initPtr(obj);
}

pub fn allocAstringSlice(self: *cy.VM, slice: []const u8, parent: *HeapObject) !Value {
    const obj = try allocPoolObject(self);
    obj.stringSlice = .{
        .structId = StringSliceT,
        .rc = 1,
        .buf = slice.ptr,
        .len = @intCast(u32, slice.len),
        .uCharLen = undefined,
        .uMruIdx = undefined,
        .uMruCharIdx = undefined,
        .extra = @as(u64, @ptrToInt(parent)) | (1 << 63),
    };
    return Value.initPtr(obj);
}

pub fn allocUnsetRawStringObject(self: *cy.VM, len: usize) linksection(cy.Section) !*HeapObject {
    var obj: *HeapObject = undefined;
    if (len <= MaxPoolObjectRawStringByteLen) {
        obj = try allocPoolObject(self);
    } else {
        const objSlice = try self.alloc.alignedAlloc(u8, @alignOf(HeapObject), len + RawString.BufOffset);
        obj = @ptrCast(*HeapObject, objSlice.ptr);
        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
    }
    obj.rawstring = .{
        .structId = RawStringT,
        .rc = 1,
        .len = @intCast(u32, len),
        .bufStart = undefined,
    };
    return obj;
}

pub fn allocRawString(self: *cy.VM, str: []const u8) linksection(cy.Section) !Value {
    const obj = try allocUnsetRawStringObject(self, str.len);
    const dst = @ptrCast([*]u8, &obj.rawstring.bufStart)[0..str.len];
    std.mem.copy(u8, dst, str);
    return Value.initPtr(obj);
}

pub fn allocRawStringSlice(self: *cy.VM, slice: []const u8, parent: *HeapObject) !Value {
    const obj = try allocPoolObject(self);
    obj.rawstringSlice = .{
        .structId = RawStringSliceT,
        .rc = 1,
        .buf = slice.ptr,
        .len = @intCast(u32, slice.len),
        .parent = @ptrCast(*RawString, parent),
    };
    return Value.initPtr(obj);
}

pub fn allocBox(vm: *cy.VM, val: Value) !Value {
    const obj = try allocPoolObject(vm);
    obj.box = .{
        .structId = BoxS,
        .rc = 1,
        .val = val,
    };
    return Value.initPtr(obj);
}

pub fn allocNativeFunc1(self: *cy.VM, func: *const fn (*cy.UserVM, [*]const Value, u8) Value, numParams: u32, tccState: ?Value) !Value {
    const obj = try allocPoolObject(self);
    obj.nativeFunc1 = .{
        .structId = NativeFunc1S,
        .rc = 1,
        .func = func,
        .numParams = numParams,
        .tccState = undefined,
        .hasTccState = false,
    };
    if (tccState) |state| {
        obj.nativeFunc1.tccState = state;
        obj.nativeFunc1.hasTccState = true;
    }
    return Value.initPtr(obj);
}

pub fn allocTccState(self: *cy.VM, state: *tcc.TCCState, lib: *std.DynLib) linksection(cy.StdSection) !Value {
    const obj = try allocPoolObject(self);
    obj.tccState = .{
        .structId = TccStateS,
        .rc = 1,
        .state = state,
        .lib = lib,
    };
    return Value.initPtr(obj);
}

pub fn allocOpaquePtr(self: *cy.VM, ptr: ?*anyopaque) !Value {
    const obj = try allocPoolObject(self);
    obj.opaquePtr = .{
        .structId = OpaquePtrS,
        .rc = 1,
        .ptr = ptr,
    };
    return Value.initPtr(obj);
}

pub fn allocFile(self: *cy.VM, fd: std.os.fd_t) linksection(cy.StdSection) !Value {
    const obj = try allocPoolObject(self);
    obj.file = .{
        .structId = FileT,
        .rc = 1,
        .fd = fd,
        .curPos = 0,
        .iterLines = false,
        .hasReadBuf = false,
        .readBuf = undefined,
        .readBufCap = 0,
        .readBufEnd = 0,
        .closed = false,
    };
    return Value.initPtr(obj);
}

pub fn allocDir(self: *cy.VM, fd: std.os.fd_t, iterable: bool) linksection(cy.StdSection) !Value {
    const obj = try allocPoolObject(self);
    obj.dir = .{
        .structId = DirT,
        .rc = 1,
        .fd = fd,
        .iterable = iterable,
    };
    return Value.initPtr(obj);
}

pub fn allocDirIterator(self: *cy.VM, dir: *Dir, recursive: bool) linksection(cy.StdSection) !Value {
    const objSlice = try self.alloc.alignedAlloc(u8, @alignOf(HeapObject), @sizeOf(cy.DirIterator));
    const obj = @ptrCast(*cy.DirIterator, objSlice.ptr);
    obj.* = .{
        .structId = DirIteratorT,
        .rc = 1,
        .dir = dir,
        .inner = undefined,
        .recursive = recursive,
    };
    if (recursive) {
        const walker = stdx.ptrAlignCast(*std.fs.IterableDir.Walker, &obj.inner.walker);
        walker.* = try dir.getStdIterableDir().walk(self.alloc);
    } else {
        const iter = stdx.ptrAlignCast(*std.fs.IterableDir.Iterator, &obj.inner.iter);
        iter.* = dir.getStdIterableDir().iterate();
    }
    if (cy.TrackGlobalRC) {
        self.refCounts += 1;
    }
    if (cy.TraceEnabled) {
        self.trace.numRetains += 1;
        self.trace.numRetainAttempts += 1;
    }
    return Value.initPtr(obj);
}

/// Allocates an object outside of the object pool.
pub fn allocObject(self: *cy.VM, sid: cy.TypeId, fields: []const Value) !Value {
    // First slot holds the structId and rc.
    const objSlice = try self.alloc.alignedAlloc(Value, @alignOf(HeapObject), 1 + fields.len);
    const obj = @ptrCast(*Object, objSlice.ptr);
    obj.* = .{
        .structId = sid,
        .rc = 1,
        .firstValue = undefined,
    };
    if (cy.TraceEnabled) {
        self.trace.numRetains += 1;
        self.trace.numRetainAttempts += 1;
    }
    if (cy.TrackGlobalRC) {
        self.refCounts += 1;
    }

    const dst = obj.getValuesPtr();
    std.mem.copy(Value, dst[0..fields.len], fields);

    return Value.initPtr(obj);
}

pub fn allocEmptyObject(self: *cy.VM, sid: cy.TypeId, numFields: u32) !Value {
    // First slot holds the structId and rc.
    const objSlice = try self.alloc.alignedAlloc(Value, @alignOf(HeapObject), 1 + numFields);
    const obj = @ptrCast(*Object, objSlice.ptr);
    obj.* = .{
        .structId = sid,
        .rc = 1,
        .firstValue = undefined,
    };
    if (cy.TraceEnabled) {
        self.trace.numRetains += 1;
        self.trace.numRetainAttempts += 1;
    }
    if (cy.TrackGlobalRC) {
        self.refCounts += 1;
    }
    return Value.initPtr(obj);
}

pub fn allocObjectSmall(self: *cy.VM, sid: cy.TypeId, fields: []const Value) !Value {
    const obj = try allocPoolObject(self);
    obj.object = .{
        .structId = sid,
        .rc = 1,
        .firstValue = undefined,
    };

    const dst = obj.object.getValuesPtr();
    std.mem.copy(Value, dst[0..fields.len], fields);

    return Value.initPtr(obj);
}

pub fn allocEmptyObjectSmall(self: *cy.VM, sid: cy.TypeId) !Value {
    const obj = try allocPoolObject(self);
    obj.object = .{
        .structId = sid,
        .rc = 1,
        .firstValue = undefined,
    };
    return Value.initPtr(obj);
}

pub fn allocFuncFromSym(self: *cy.VM, symId: cy.vm.SymbolId) !Value {
    const sym = self.funcSyms.buf[symId];
    switch (@intToEnum(cy.vm.FuncSymbolEntryType, sym.entryT)) {
        .nativeFunc1 => {
            return allocNativeFunc1(self, sym.inner.nativeFunc1, sym.innerExtra.nativeFunc1.numParams, null);
        },
        .func => {
            return allocLambda(self, sym.inner.func.pc, @intCast(u8, sym.inner.func.numParams), @intCast(u8, sym.inner.func.numLocals));
        },
        .closure => {
            cy.arc.retainObject(self, @ptrCast(*HeapObject, sym.inner.closure));
            return Value.initPtr(sym.inner.closure);
        },
        .none => return Value.None,
    }
}

pub fn freeObject(vm: *cy.VM, obj: *HeapObject) linksection(cy.HotSection) void {
    log.debug("free {}", .{obj.getUserTag()});
    switch (obj.retainedCommon.structId) {
        ListS => {
            const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
            for (list.items()) |it| {
                cy.arc.release(vm, it);
            }
            list.deinit(vm.alloc);
            freePoolObject(vm, obj);
        },
        ListIteratorT => {
            cy.arc.releaseObject(vm, stdx.ptrAlignCast(*HeapObject, obj.listIter.list));
            freePoolObject(vm, obj);
        },
        MapS => {
            const map = stdx.ptrAlignCast(*MapInner, &obj.map.inner);
            var iter = map.iterator();
            while (iter.next()) |entry| {
                cy.arc.release(vm, entry.key);
                cy.arc.release(vm, entry.value);
            }
            map.deinit(vm.alloc);
            freePoolObject(vm, obj);
        },
        MapIteratorT => {
            cy.arc.releaseObject(vm, stdx.ptrAlignCast(*HeapObject, obj.mapIter.map));
            freePoolObject(vm, obj);
        },
        ClosureS => {
            const src = obj.closure.getCapturedValuesPtr()[0..obj.closure.numCaptured];
            for (src) |capturedVal| {
                cy.arc.release(vm, capturedVal);
            }
            if (obj.closure.numCaptured <= 3) {
                freePoolObject(vm, obj);
            } else {
                const slice = @ptrCast([*]u8, obj)[0..2 + obj.closure.numCaptured];
                vm.alloc.free(slice);
            }
        },
        LambdaS => {
            freePoolObject(vm, obj);
        },
        AstringT => {
            if (obj.astring.len <= DefaultStringInternMaxByteLen) {
                // Check both the key and value to make sure this object is the intern entry.
                const key = obj.astring.getConstSlice();
                if (vm.strInterns.get(key)) |val| {
                    if (val == obj) {
                        _ = vm.strInterns.remove(key);
                    }
                }
            }
            if (obj.astring.len <= MaxPoolObjectAstringByteLen) {
                freePoolObject(vm, obj);
            } else {
                const slice = @ptrCast([*]u8, obj)[0..Astring.BufOffset + obj.astring.len];
                vm.alloc.free(slice);
            }
        },
        UstringT => {
            if (obj.ustring.len <= DefaultStringInternMaxByteLen) {
                const key = obj.ustring.getConstSlice();
                if (vm.strInterns.get(key)) |val| {
                    if (val == obj) {
                        _ = vm.strInterns.remove(key);
                    }
                }
            }
            if (obj.ustring.len <= MaxPoolObjectUstringByteLen) {
                freePoolObject(vm, obj);
            } else {
                const slice = @ptrCast([*]u8, obj)[0..Ustring.BufOffset + obj.ustring.len];
                vm.alloc.free(slice);
            }
        },
        StringSliceT => {
            if (obj.stringSlice.getParentPtr()) |parent| {
                cy.arc.releaseObject(vm, parent);
            }
            freePoolObject(vm, obj);
        },
        RawStringT => {
            if (obj.rawstring.len <= MaxPoolObjectRawStringByteLen) {
                freePoolObject(vm, obj);
            } else {
                const slice = @ptrCast([*]u8, obj)[0..RawString.BufOffset + obj.rawstring.len];
                vm.alloc.free(slice);
            }
        },
        RawStringSliceT => {
            const parent = @ptrCast(*cy.HeapObject, obj.rawstringSlice.parent);
            cy.arc.releaseObject(vm, parent);
            freePoolObject(vm, obj);
        },
        FiberS => {
            cy.fiber.releaseFiberStack(vm, &obj.fiber);
            freePoolObject(vm, obj);
        },
        BoxS => {
            cy.arc.release(vm, obj.box.val);
            freePoolObject(vm, obj);
        },
        NativeFunc1S => {
            if (obj.nativeFunc1.hasTccState) {
                cy.arc.releaseObject(vm, obj.nativeFunc1.tccState.asHeapObject());
            }
            freePoolObject(vm, obj);
        },
        TccStateS => {
            if (cy.hasJit) {
                tcc.tcc_delete(obj.tccState.state);
                obj.tccState.lib.close();
                vm.alloc.destroy(obj.tccState.lib);
                freePoolObject(vm, obj);
            } else {
                unreachable;
            }
        },
        OpaquePtrS => {
            freePoolObject(vm, obj);
        },
        FileT => {
            if (cy.hasStdFiles) {
                if (obj.file.hasReadBuf) {
                    vm.alloc.free(obj.file.readBuf[0..obj.file.readBufCap]);
                }
                obj.file.close();
            }
            freePoolObject(vm, obj);
        },
        DirT => {
            if (cy.hasStdFiles) {
                var dir = obj.dir.getStdDir();
                dir.close();   
            }
            freePoolObject(vm, obj);
        },
        DirIteratorT => {
            if (cy.hasStdFiles) {
                var dir = @ptrCast(*DirIterator, obj);
                if (dir.recursive) {
                    const walker = stdx.ptrAlignCast(*std.fs.IterableDir.Walker, &dir.inner.walker);
                    walker.deinit();   
                }
                cy.arc.releaseObject(vm, @ptrCast(*HeapObject, dir.dir));
            }
            const slice = @ptrCast([*]align(@alignOf(HeapObject)) u8, obj)[0..@sizeOf(DirIterator)];
            vm.alloc.free(slice);
        },
        SymbolT => {
            freePoolObject(vm, obj);
        },
        else => {
            log.debug("free {s}", .{vm.structs.buf[obj.retainedCommon.structId].name});
            // Struct deinit.
            if (builtin.mode == .Debug) {
                // Check range.
                if (obj.retainedCommon.structId >= vm.structs.len) {
                    log.debug("unsupported struct type {}", .{obj.retainedCommon.structId});
                    stdx.fatal();
                }
            }
            const numFields = vm.structs.buf[obj.retainedCommon.structId].numFields;
            for (obj.object.getValuesConstPtr()[0..numFields]) |child| {
                cy.arc.release(vm, child);
            }
            if (numFields <= 4) {
                freePoolObject(vm, obj);
            } else {
                const slice = @ptrCast([*]Value, obj)[0..1 + numFields];
                vm.alloc.free(slice);
            }
        },
    }
    if (builtin.mode == .Debug) {
        if (vm.objectTraceMap.getPtr(obj)) |trace| {
            trace.freePc = cy.pcOffset(vm, vm.debugPc);
        } else {
            log.debug("Missing object trace {*}", .{obj});
        }
    }
}

test "Internals." {
    try t.eq(@sizeOf(MapInner), 32);
    try t.eq(@sizeOf(HeapObject), 40);
    try t.eq(@alignOf(HeapObject), 8);
    try t.eq(@sizeOf(HeapPage), 40 * 102);
    try t.eq(@alignOf(HeapPage), 8);

    try t.eq(@alignOf(List), 8);
    var list: List = undefined;
    try t.eq(@ptrToInt(&list.list.ptr), @ptrToInt(&list) + 8);
    try t.eq(@alignOf(ListIterator), 8);

    try t.eq(@alignOf(Dir), 8);
    var dir: Dir = undefined;
    try t.eq(@ptrToInt(&dir.structId), @ptrToInt(&dir));
    try t.eq(@ptrToInt(&dir.rc), @ptrToInt(&dir) + 4);

    try t.eq(@alignOf(DirIterator), 8);
    var dirIter: DirIterator = undefined;
    try t.eq(@ptrToInt(&dirIter.structId), @ptrToInt(&dirIter));
    try t.eq(@ptrToInt(&dirIter.rc), @ptrToInt(&dirIter) + 4);

    const rstr = RawString{
        .structId = RawStringT,
        .rc = 1,
        .len = 1,
        .bufStart = undefined,
    };
    try t.eq(@ptrToInt(&rstr.structId), @ptrToInt(&rstr));
    try t.eq(@ptrToInt(&rstr.rc), @ptrToInt(&rstr) + 4);
    try t.eq(@ptrToInt(&rstr.len), @ptrToInt(&rstr) + 8);
    try t.eq(RawString.BufOffset, 12);
    try t.eq(@ptrToInt(&rstr.bufStart), @ptrToInt(&rstr) + RawString.BufOffset);

    const astr = Astring{
        .structId = AstringT,
        .rc = 1,
        .len = 1,
        .bufStart = undefined,
    };
    try t.eq(@ptrToInt(&astr.structId), @ptrToInt(&astr));
    try t.eq(@ptrToInt(&astr.rc), @ptrToInt(&astr) + 4);
    try t.eq(@ptrToInt(&astr.len), @ptrToInt(&astr) + 8);
    try t.eq(Astring.BufOffset, 12);
    try t.eq(@ptrToInt(&astr.bufStart), @ptrToInt(&astr) + Astring.BufOffset);

    const ustr = Ustring{
        .structId = UstringT,
        .rc = 1,
        .len = 1,
        .charLen = 1,
        .mruIdx = 0,
        .mruCharIdx = 0,
        .bufStart = undefined,
    };
    try t.eq(@ptrToInt(&ustr.structId), @ptrToInt(&ustr));
    try t.eq(@ptrToInt(&ustr.rc), @ptrToInt(&ustr) + 4);
    try t.eq(@ptrToInt(&ustr.len), @ptrToInt(&ustr) + 8);
    try t.eq(@ptrToInt(&ustr.charLen), @ptrToInt(&ustr) + 12);
    try t.eq(@ptrToInt(&ustr.mruIdx), @ptrToInt(&ustr) + 16);
    try t.eq(@ptrToInt(&ustr.mruCharIdx), @ptrToInt(&ustr) + 20);
    try t.eq(Ustring.BufOffset, 24);
    try t.eq(@ptrToInt(&ustr.bufStart), @ptrToInt(&ustr) + Ustring.BufOffset);

    const slice = StringSlice{
        .structId = StringSliceT,
        .rc = 1,
        .buf = undefined,
        .len = 1,
        .uCharLen = undefined,
        .uMruIdx = undefined,
        .uMruCharIdx = undefined,
        .extra = undefined,
    };
    try t.eq(@ptrToInt(&slice.structId), @ptrToInt(&slice));
    try t.eq(@ptrToInt(&slice.rc), @ptrToInt(&slice) + 4);
    try t.eq(@ptrToInt(&slice.buf), @ptrToInt(&slice) + 8);
    try t.eq(@ptrToInt(&slice.len), @ptrToInt(&slice) + 16);
    try t.eq(@ptrToInt(&slice.uCharLen), @ptrToInt(&slice) + 20);
    try t.eq(@ptrToInt(&slice.uMruIdx), @ptrToInt(&slice) + 24);
    try t.eq(@ptrToInt(&slice.uMruCharIdx), @ptrToInt(&slice) + 28);
    try t.eq(@ptrToInt(&slice.extra), @ptrToInt(&slice) + 32);

    const rslice = RawStringSlice{
        .structId = RawStringSliceT,
        .rc = 1,
        .buf = undefined,
        .len = 1,
        .parent = undefined,
    };
    try t.eq(@ptrToInt(&rslice.structId), @ptrToInt(&rslice));
    try t.eq(@ptrToInt(&rslice.rc), @ptrToInt(&rslice) + 4);
    try t.eq(@ptrToInt(&rslice.buf), @ptrToInt(&rslice) + 8);
    try t.eq(@ptrToInt(&rslice.len), @ptrToInt(&rslice) + 16);
    try t.eq(@ptrToInt(&rslice.parent), @ptrToInt(&rslice) + 24);
}