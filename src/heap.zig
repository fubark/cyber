// Copyright (c) 2023 Cyber (See LICENSE)

/// Heap objects, object allocation and deinitializers.

const cy = @import("cyber.zig");
const rt = cy.rt;
const Value = cy.Value;
const mi = @import("mimalloc");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const std = @import("std");
const builtin = @import("builtin");
const tcc = @import("tcc");
const log = stdx.log.scoped(.heap);
const NullId = std.math.maxInt(u32);
const NullU8 = std.math.maxInt(u8);

/// Use mimalloc for fast builds.
const UseMimalloc = builtin.mode == .ReleaseFast and !cy.isWasm;

var gpa: std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = false,
    .stack_trace_frames = if (builtin.mode == .Debug) 10 else 0,
}) = .{};
var miAlloc: mi.Allocator = undefined;
var initedAllocator = false;

fn initAllocator() void {
    defer initedAllocator = true;
    if (build_options.useMalloc) {
        return;
    } else {
        if (UseMimalloc) {
            miAlloc.init();
        } else {
            return;
        }
    }
    // var traceAlloc: stdx.heap.TraceAllocator = undefined;
    // traceAlloc.init(miAlloc.allocator());
    // traceAlloc.init(child);
    // defer traceAlloc.dump();
    // const alloc = traceAlloc.allocator();
}

pub fn getAllocator() std.mem.Allocator {
    if (!initedAllocator) {
        initAllocator();
    }
    if (build_options.useMalloc) {
        return std.heap.c_allocator;
    } else {
        if (UseMimalloc) {
            return miAlloc.allocator();
        } else {
            if (cy.isWasm) {
                return std.heap.wasm_allocator;
            } else {
                return gpa.allocator();
            }
        }
    }
}

pub fn deinitAllocator() void {
    if (builtin.mode == .Debug) {
        if (build_options.useMalloc) {
            return;
        } else {
            if (UseMimalloc) {
                miAlloc.deinit();
            } else {
                if (cy.isWasm) {
                    return;
                } else {
                    _ = gpa.deinit();
                }
            }
        }
        initedAllocator = false;
    }
}

// Keep it just under 4kb page.
pub const HeapPage = struct {
    objects: [102]HeapObject,
};

const HeapObjectId = u32;

/// Total of 40 bytes per object. If objects are bigger, they are allocated on the gpa.
pub const HeapObject = extern union {
    headType: extern struct {
        typeId: rt.TypeId,
    },
    head: extern struct {
        typeId: rt.TypeId,
        rc: u32,
    },
    freeSpan: extern struct {
        typeId: rt.TypeId,
        len: u32,
        start: *HeapObject,
        next: ?*HeapObject,
    },
    list: List,
    listIter: ListIterator,
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
    pointer: Pointer,
    metatype: MetaType,

    pub fn getUserTag(self: *const HeapObject) cy.ValueUserTag {
        switch (self.head.typeId) {
            rt.ListT => return .list,
            rt.MapT => return .map,
            rt.AstringT => return .string,
            rt.UstringT => return .string,
            rt.RawstringT => return .rawstring,
            rt.ClosureT => return .closure,
            rt.LambdaT => return .lambda,
            rt.FiberT => return .fiber,
            rt.NativeFuncT => return .nativeFunc,
            rt.TccStateT => return .tccState,
            rt.PointerT => return .pointer,
            rt.FileT => return .file,
            rt.DirT => return .dir,
            rt.DirIteratorT => return .dirIter,
            rt.BoxT => return .box,
            else => {
                return .object;
            },
        }
    }
};

pub const MetaTypeKind = enum {
    object,
};

/// MetaType needs a RC to prevent ARC cleanup of the internal type and source code.
pub const MetaType = extern struct {
    typeId: rt.TypeId,
    rc: u32,
    type: u32,
    symId: u32,
};

pub const List = extern struct {
    structId: rt.TypeId,
    rc: u32,
    list: extern struct {
        ptr: [*]Value,
        cap: usize,
        len: usize,
    },

    pub fn getList(self: *List) *cy.List(Value) {
        return stdx.ptrAlignCast(*cy.List(Value), &self.list);
    }

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
                const newCap = std.math.ceilPowerOfTwo(u32, @as(u32, @intCast(list.len)) + 1) catch stdx.fatal();
                list.growTotalCapacityPrecise(alloc, newCap) catch stdx.fatal();
            } else {
                list.growTotalCapacity(alloc, list.len + 1) catch stdx.fatal();
            }
        }
        list.appendAssumeCapacity(val);
    }
};

pub const ListIterator = extern struct {
    structId: rt.TypeId,
    rc: u32,
    list: *List,
    nextIdx: u32,
};

pub const MapInner = cy.ValueMap;
pub const Map = extern struct {
    structId: rt.TypeId,
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

    pub fn map(self: *Map) *MapInner {
        return stdx.ptrAlignCast(*MapInner, &self.inner);
    }
};

pub const MapIterator = extern struct {
    structId: rt.TypeId,
    rc: u32,
    map: *Map,
    nextIdx: u32,
};

pub const Closure = extern struct {
    structId: rt.TypeId,
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    numCaptured: u8,
    stackSize: u8,
    /// Closure value is copied to this local to provide captured var lookup.
    local: u8,
    rFuncSigId: u64,

    // Begins array of `Box` values.
    firstCapturedVal: Value,

    pub inline fn getCapturedValuesPtr(self: *Closure) [*]Value {
        return @ptrCast(&self.firstCapturedVal);
    }
};

const Lambda = extern struct {
    structId: rt.TypeId,
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    stackSize: u8,
    padding: u16 = 0,
    rFuncSigId: u64,
};

/// 28 byte length can fit inside a Heap pool object.
pub const MaxPoolObjectAstringByteLen = 28;

pub const Astring = extern struct {
    structId: rt.TypeId,
    rc: u32,
    len: u32,
    bufStart: u8,

    const BufOffset = @offsetOf(Astring, "bufStart");

    pub inline fn getSlice(self: *Astring) []u8 {
        return @as([*]u8, @ptrCast(&self.bufStart))[0..self.len];
    }

    pub inline fn getConstSlice(self: *const Astring) []const u8 {
        return @as([*]const u8, @ptrCast(&self.bufStart))[0..self.len];
    }
};

/// 16 byte length can fit inside a Heap pool object.
pub const MaxPoolObjectUstringByteLen = 16;

const Ustring = extern struct {
    structId: rt.TypeId,
    rc: u32,
    len: u32,
    charLen: u32,
    mruIdx: u32,
    mruCharIdx: u32,
    bufStart: u8,

    const BufOffset = @offsetOf(Ustring, "bufStart");

    pub inline fn getSlice(self: *Ustring) []u8 {
        return @as([*]u8, @ptrCast(&self.bufStart))[0..self.len];
    }

    pub inline fn getConstSlice(self: *const Ustring) []const u8 {
        return @as([*]const u8, @ptrCast(&self.bufStart))[0..self.len];
    }
};

/// One data structure for astring/ustring slice it can fit into a pool object
/// and use the same layout.
const StringSlice = extern struct {
    structId: rt.TypeId,
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
        return @ptrFromInt(@as(usize, @intCast(self.extra & 0x7fffffffffffffff)));
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
    structId: if (cy.isWasm) rt.TypeId else rt.TypeId align(8),
    rc: u32,
    len: u32,
    bufStart: u8,

    pub const BufOffset = @offsetOf(RawString, "bufStart");

    pub inline fn getSlice(self: *RawString) []u8 {
        return @as([*]u8, @ptrCast(&self.bufStart))[0..self.len];
    }

    pub inline fn getConstSlice(self: *const RawString) []const u8 {
        return @as([*]const u8, @ptrCast(&self.bufStart))[0..self.len];
    }
};

const RawStringSlice = extern struct {
    structId: rt.TypeId,
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
    structId: rt.TypeId,
    rc: u32,
    firstValue: Value,

    pub inline fn getValuesConstPtr(self: *const Object) [*]const Value {
        return @ptrCast(&self.firstValue);
    }

    pub inline fn getValuesPtr(self: *Object) [*]Value {
        return @ptrCast(&self.firstValue);
    }

    pub inline fn getValuePtr(self: *Object, idx: u32) *Value {
        return @ptrCast(@as([*]Value, @ptrCast(&self.firstValue)) + idx);
    }

    pub inline fn getValue(self: *const Object, idx: u32) Value {
        return @as([*]const Value, @ptrCast(&self.firstValue))[idx];
    }
};

const Box = extern struct {
    structId: rt.TypeId,
    rc: u32,
    val: Value,
};

const NativeFunc1 = extern struct {
    structId: rt.TypeId,
    rc: u32,
    func: *const fn (*cy.UserVM, [*]const Value, u8) Value,
    numParams: u32,
    rFuncSigId: u32,
    tccState: Value,
    hasTccState: bool,
};

const TccState = extern struct {
    structId: rt.TypeId,
    rc: u32,
    state: *tcc.TCCState,
    lib: *std.DynLib,
};

const File = extern struct {
    structId: rt.TypeId,
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
    structId: rt.TypeId,
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
    structId: rt.TypeId align(8),
    rc: u32,
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

pub const Pointer = extern struct {
    structId: rt.TypeId,
    rc: u32,
    ptr: ?*anyopaque,
};

/// Initializes the page with freed object slots and returns the pointer to the first slot.
fn initHeapPage(page: *HeapPage) *HeapObject {
    // First HeapObject at index 0 is reserved so that freeObject can get the previous slot without a bounds check.
    page.objects[0].headType = .{
        .typeId = 0, // Non-NullId so freeObject doesn't think it's a free span.
    };
    const first = &page.objects[1];
    first.freeSpan = .{
        .typeId = NullId,
        .len = page.objects.len - 1,
        .start = first,
        .next = null,
    };
    // The rest initialize as free spans so checkMemory doesn't think they are retained objects.
    @memset(page.objects[2..], .{
        .headType = .{
            .typeId = NullId,
        }
    });
    page.objects[page.objects.len-1].freeSpan.start = first;
    return first;
}

/// Returns the first free HeapObject.
pub fn growHeapPages(self: *cy.VM, numPages: usize) !HeapObjectList {
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
    return .{
        .head = first,
        .tail = last,
    };
}

const HeapObjectList = struct {
    head: *HeapObject,
    tail: *HeapObject,
};

pub fn allocExternalObject(vm: *cy.VM, size: usize) !*HeapObject {
    // Align with HeapObject so it can be casted.
    const slice = try vm.alloc.alignedAlloc(u8, @alignOf(HeapObject), size);
    defer {
        if (builtin.mode == .Debug) {
            cy.heap.traceAlloc(vm, @ptrCast(slice.ptr));
        }
    }
    if (cy.TrackGlobalRC) {
        vm.refCounts += 1;
    }
    if (cy.TraceEnabled) {
        vm.trace.numRetains += 1;
        vm.trace.numRetainAttempts += 1;
    }
    return @ptrCast(slice.ptr);
}

/// Assumes new object will have an RC = 1.
pub fn allocPoolObject(self: *cy.VM) linksection(cy.HotSection) !*HeapObject {
    if (self.heapFreeHead == null) {
        const list = try growHeapPages(self, @max(1, (self.heapPages.len * 15) / 10));
        self.heapFreeHead = list.head;
        if (builtin.mode == .Debug) {
            self.heapFreeTail = list.tail;
        }
    }
    const ptr = self.heapFreeHead.?;
    defer {
        if (builtin.mode == .Debug) {
            traceAlloc(self, ptr);
            if (self.heapFreeTail == ptr) {
                // Ensure tail is updated if it was the same segment as head.
                self.heapFreeTail = self.heapFreeHead;
            }
        }
        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
    }
    if (ptr.freeSpan.len == 1) {
        // This is the only free slot, move to the next free span.
        self.heapFreeHead = ptr.freeSpan.next;
        return ptr;
    } else {
        const next = &@as([*]HeapObject, @ptrCast(ptr))[1];
        next.freeSpan = .{
            .typeId = NullId,
            .len = ptr.freeSpan.len - 1,
            .start = next,
            .next = ptr.freeSpan.next,
        };
        const last = &@as([*]HeapObject, @ptrCast(ptr))[ptr.freeSpan.len-1];
        last.freeSpan.start = next;
        self.heapFreeHead = next;
        return ptr;
    }
}

fn freeExternalObject(vm: *cy.VM, obj: *HeapObject, len: usize) void {
    if (builtin.mode == .Debug) {
        if (vm.objectTraceMap.getPtr(obj)) |trace| {
            trace.freePc = vm.debugPc;
            trace.freeTypeId = obj.head.typeId;
        } else {
            log.debug("Missing object trace {*} {}", .{obj, obj.head.typeId});
        }
    }
    const slice = @as([*]align(@alignOf(HeapObject)) u8, @ptrCast(obj))[0..len];
    vm.alloc.free(slice);
}

pub fn freePoolObject(vm: *cy.VM, obj: *HeapObject) linksection(cy.HotSection) void {
    if (builtin.mode == .Debug) {
        if (vm.objectTraceMap.getPtr(obj)) |trace| {
            trace.freePc = vm.debugPc;
            trace.freeTypeId = obj.head.typeId;
        } else {
            log.debug("Missing object trace {*} {}", .{obj, obj.head.typeId});
        }
    }
    const prev = &(@as([*]HeapObject, @ptrCast(obj)) - 1)[0];
    if (prev.head.typeId == NullId) {
        // Left is a free span. Extend length.
        prev.freeSpan.start.freeSpan.len += 1;
        obj.freeSpan.start = prev.freeSpan.start;
        obj.freeSpan.typeId = cy.NullId;
    } else {
        if (builtin.mode == .Debug) {
            // Debug mode performs LRU allocation to surface double free errors better.
            // When an object is constantly reused, the most recent traced alloc/free info
            // can mask an earlier alloc/free on the same object pointer.
            obj.freeSpan = .{
                .typeId = NullId,
                .len = 1,
                .start = obj,
                .next = null,
            };
            if (vm.heapFreeTail == null) {
                vm.heapFreeTail = obj;
                vm.heapFreeHead = obj;
            } else {
                vm.heapFreeTail.?.freeSpan.next = obj;
            }
        } else {
            // Add single slot free span.
            obj.freeSpan = .{
                .typeId = NullId,
                .len = 1,
                .start = obj,
                .next = vm.heapFreeHead,
            };
            vm.heapFreeHead = obj;
        }
    }
}

pub fn allocMetaType(self: *cy.VM, symType: u8, symId: u32) !Value {
    const obj = try allocPoolObject(self);
    obj.metatype = .{
        .typeId = rt.MetaTypeT,
        .rc = 1,
        .type = symType,
        .symId = symId,
    };
    return Value.initPtr(obj);
}

pub fn allocEmptyList(self: *cy.VM) linksection(cy.Section) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .structId = rt.ListT,
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
        .structId = rt.ListT,
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
        .structId = rt.ListT,
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
        @memset(list.items(), val);
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
        .structId = rt.ListT,
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
        .structId = rt.ListIteratorT,
        .rc = 1,
        .list = list,
        .nextIdx = 0,
    };
    return Value.initPtr(obj);
}

pub fn allocEmptyMap(self: *cy.VM) !Value {
    const obj = try allocPoolObject(self);
    obj.map = .{
        .structId = rt.MapT,
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

pub fn allocMap(self: *cy.VM, keyIdxs: []const align(1) u16, vals: []const Value) !Value {
    const obj = try allocPoolObject(self);
    obj.map = .{
        .structId = rt.MapT,
        .rc = 1,
        .inner = .{
            .metadata = null,
            .entries = null,
            .size = 0,
            .cap = 0,
            .available = 0,
        },
    };

    const inner: *MapInner = @ptrCast(&obj.map.inner);
    for (keyIdxs, 0..) |idx, i| {
        const val = vals[i];

        const keyVal = Value{ .val = self.consts[idx].val };
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
        .structId = rt.MapIteratorT,
        .rc = 1,
        .map = map,
        .nextIdx = 0,
    };
    return Value.initPtr(obj);
}

/// Captured values are retained during alloc.
pub fn allocClosure(
    self: *cy.VM, fp: [*]Value, funcPc: usize, numParams: u8, stackSize: u8,
    rFuncSigId: u16, capturedVals: []const cy.Inst, closureLocal: u8,
) !Value {
    var obj: *HeapObject = undefined;
    if (capturedVals.len <= 2) {
        obj = try allocPoolObject(self);
    } else {
        obj = try allocExternalObject(self, (2 + capturedVals.len) * @sizeOf(Value));
    }
    obj.closure = .{
        .structId = rt.ClosureT,
        .rc = 1,
        .funcPc = @intCast(funcPc),
        .numParams = numParams,
        .stackSize = stackSize,
        .numCaptured = @intCast(capturedVals.len),
        .local = closureLocal,
        .rFuncSigId = rFuncSigId,
        .firstCapturedVal = undefined,
    };
    const dst = obj.closure.getCapturedValuesPtr();
    for (capturedVals, 0..) |local, i| {
        if (builtin.mode == .Debug) {
            if (!fp[local.arg].isBox()) {
                stdx.panic("Expected box value.");
            }
        }
        cy.arc.retain(self, fp[local.arg]);
        dst[i] = fp[local.arg];
    }
    return Value.initPtr(obj);
}

pub fn allocLambda(self: *cy.VM, funcPc: usize, numParams: u8, stackSize: u8, rFuncSigId: u16) !Value {
    const obj = try allocPoolObject(self);
    obj.lambda = .{
        .structId = rt.LambdaT,
        .rc = 1,
        .funcPc = @intCast(funcPc),
        .numParams = numParams,
        .stackSize = stackSize,
        .rFuncSigId = rFuncSigId,
    };
    return Value.initPtr(obj);
}

pub fn allocStringTemplate(self: *cy.VM, strs: []const cy.Inst, vals: []const Value) !Value {
    const firstStr = self.valueAsStaticString(Value.initRaw(self.consts[strs[0].arg].val));
    try self.u8Buf.resize(self.alloc, firstStr.len);
    std.mem.copy(u8, self.u8Buf.items(), firstStr);

    const writer = self.u8Buf.writer(self.alloc);
    for (vals, 0..) |val, i| {
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
    const len: u32 = @intCast(str.len + str2.len);
    var obj: *HeapObject = undefined;
    if (len <= MaxPoolObjectRawStringByteLen) {
        obj = try allocPoolObject(self);
    } else {
        obj = try allocExternalObject(self, len + RawString.BufOffset);
    }
    obj.rawstring = .{
        .structId = rt.RawstringT,
        .rc = 1,
        .len = len,
        .bufStart = undefined,
    };
    const dst = @as([*]u8, @ptrCast(&obj.rawstring.bufStart))[0..len];
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
            return try getOrAllocUstring(self, str, @intCast(charLen));
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
        obj = try allocExternalObject(self, len + Astring.BufOffset);
    }
    obj.astring = .{
        .structId = rt.AstringT,
        .rc = 1,
        .len = @intCast(len),
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
        obj = try allocExternalObject(self, len + Ustring.BufOffset);
    }
    obj.ustring = .{
        .structId = rt.UstringT,
        .rc = 1,
        .len = @intCast(len),
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
        .structId = rt.StringSliceT,
        .rc = 1,
        .buf = slice.ptr,
        .len = @intCast(slice.len),
        .uCharLen = charLen,
        .uMruIdx = 0,
        .uMruCharIdx = 0,
        .extra = @as(u63, @intCast(@intFromPtr(parent))),
    };
    return Value.initPtr(obj);
}

pub fn allocAstringSlice(self: *cy.VM, slice: []const u8, parent: *HeapObject) !Value {
    const obj = try allocPoolObject(self);
    obj.stringSlice = .{
        .structId = rt.StringSliceT,
        .rc = 1,
        .buf = slice.ptr,
        .len = @intCast(slice.len),
        .uCharLen = undefined,
        .uMruIdx = undefined,
        .uMruCharIdx = undefined,
        .extra = @as(u64, @intFromPtr(parent)) | (1 << 63),
    };
    return Value.initPtr(obj);
}

pub fn allocUnsetRawStringObject(self: *cy.VM, len: usize) linksection(cy.Section) !*HeapObject {
    var obj: *HeapObject = undefined;
    if (len <= MaxPoolObjectRawStringByteLen) {
        obj = try allocPoolObject(self);
    } else {
        obj = try allocExternalObject(self, len + RawString.BufOffset);
    }
    obj.rawstring = .{
        .structId = rt.RawstringT,
        .rc = 1,
        .len = @intCast(len),
        .bufStart = undefined,
    };
    return obj;
}

pub fn allocRawString(self: *cy.VM, str: []const u8) linksection(cy.Section) !Value {
    const obj = try allocUnsetRawStringObject(self, str.len);
    const dst = @as([*]u8, @ptrCast(&obj.rawstring.bufStart))[0..str.len];
    std.mem.copy(u8, dst, str);
    return Value.initPtr(obj);
}

pub fn allocRawStringSlice(self: *cy.VM, slice: []const u8, parent: *HeapObject) !Value {
    const obj = try allocPoolObject(self);
    obj.rawstringSlice = .{
        .structId = rt.RawstringSliceT,
        .rc = 1,
        .buf = slice.ptr,
        .len = @intCast(slice.len),
        .parent = @ptrCast(parent),
    };
    return Value.initPtr(obj);
}

pub fn allocBox(vm: *cy.VM, val: Value) !Value {
    const obj = try allocPoolObject(vm);
    obj.box = .{
        .structId = rt.BoxT,
        .rc = 1,
        .val = val,
    };
    return Value.initPtr(obj);
}

pub fn allocNativeFunc1(self: *cy.VM, func: *const fn (*cy.UserVM, [*]const Value, u8) Value, numParams: u32, rFuncSigId: cy.sema.ResolvedFuncSigId, tccState: ?Value) !Value {
    const obj = try allocPoolObject(self);
    obj.nativeFunc1 = .{
        .structId = rt.NativeFuncT,
        .rc = 1,
        .func = func,
        .numParams = numParams,
        .rFuncSigId = rFuncSigId,
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
        .structId = rt.TccStateT,
        .rc = 1,
        .state = state,
        .lib = lib,
    };
    return Value.initPtr(obj);
}

pub fn allocPointer(self: *cy.VM, ptr: ?*anyopaque) !Value {
    const obj = try allocPoolObject(self);
    obj.pointer = .{
        .structId = rt.PointerT,
        .rc = 1,
        .ptr = ptr,
    };
    return Value.initPtr(obj);
}

pub fn allocFile(self: *cy.VM, fd: std.os.fd_t) linksection(cy.StdSection) !Value {
    const obj = try allocPoolObject(self);
    obj.file = .{
        .structId = rt.FileT,
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
        .structId = rt.DirT,
        .rc = 1,
        .fd = fd,
        .iterable = iterable,
        .closed = false,
    };
    return Value.initPtr(obj);
}

pub fn allocDirIterator(self: *cy.VM, dir: *Dir, recursive: bool) linksection(cy.StdSection) !Value {
    const obj: *cy.DirIterator = @ptrCast(try allocExternalObject(self, @sizeOf(cy.DirIterator)));
    obj.* = .{
        .structId = rt.DirIteratorT,
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
    return Value.initPtr(obj);
}

/// Allocates an object outside of the object pool.
pub fn allocObject(self: *cy.VM, sid: rt.TypeId, fields: []const Value) !Value {
    // First slot holds the structId and rc.
    const obj: *Object = @ptrCast(try allocExternalObject(self, (1 + fields.len) * @sizeOf(Value)));
    obj.* = .{
        .structId = sid,
        .rc = 1,
        .firstValue = undefined,
    };
    const dst = obj.getValuesPtr();
    std.mem.copy(Value, dst[0..fields.len], fields);

    return Value.initPtr(obj);
}

pub fn allocEmptyObject(self: *cy.VM, sid: rt.TypeId, numFields: u32) !Value {
    // First slot holds the structId and rc.
    const obj: *Object = @ptrCast(try allocExternalObject(self, (1 + numFields) * @sizeOf(Value)));
    obj.* = .{
        .structId = sid,
        .rc = 1,
        .firstValue = undefined,
    };
    return Value.initPtr(obj);
}

pub fn allocObjectSmall(self: *cy.VM, sid: rt.TypeId, fields: []const Value) !Value {
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

pub fn allocEmptyObjectSmall(self: *cy.VM, sid: rt.TypeId) !Value {
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
    switch (@as(cy.rt.FuncSymbolEntryType, @enumFromInt(sym.entryT))) {
        .nativeFunc1 => {
            const rFuncSigId = sym.innerExtra.nativeFunc1.rFuncSigId;
            const numParams = sym.innerExtra.nativeFunc1.numParams();
            return allocNativeFunc1(self, sym.inner.nativeFunc1, numParams, rFuncSigId, null);
        },
        .func => {
            return allocLambda(self, sym.inner.func.pc,
                @intCast(sym.inner.func.numParams),
                @intCast(sym.inner.func.stackSize),
                @intCast(sym.innerExtra.func.rFuncSigId)
            );
        },
        .closure => {
            cy.arc.retainObject(self, @ptrCast(sym.inner.closure));
            return Value.initPtr(sym.inner.closure);
        },
        .none => return Value.None,
    }
}

pub fn freeObject(vm: *cy.VM, obj: *HeapObject) linksection(cy.HotSection) void {
    if (builtin.mode == .Debug) {
        if (obj.head.typeId == cy.NullId) {
            stdx.panicFmt("Double free object: {*} Should have been discovered in release op.", .{obj});
        } else {
            if (cy.verbose) {
                log.debug("free type={}({s})", .{
                    obj.head.typeId, cy.heap.getTypeName(vm, obj.head.typeId),
                });
            }
        }
    }
    switch (obj.head.typeId) {
        rt.ListT => {
            const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
            for (list.items()) |it| {
                cy.arc.release(vm, it);
            }
            list.deinit(vm.alloc);
            freePoolObject(vm, obj);
        },
        rt.ListIteratorT => {
            cy.arc.releaseObject(vm, stdx.ptrAlignCast(*HeapObject, obj.listIter.list));
            freePoolObject(vm, obj);
        },
        rt.MapT => {
            const map = stdx.ptrAlignCast(*MapInner, &obj.map.inner);
            var iter = map.iterator();
            while (iter.next()) |entry| {
                cy.arc.release(vm, entry.key);
                cy.arc.release(vm, entry.value);
            }
            map.deinit(vm.alloc);
            freePoolObject(vm, obj);
        },
        rt.MapIteratorT => {
            cy.arc.releaseObject(vm, stdx.ptrAlignCast(*HeapObject, obj.mapIter.map));
            freePoolObject(vm, obj);
        },
        rt.ClosureT => {
            const src = obj.closure.getCapturedValuesPtr()[0..obj.closure.numCaptured];
            for (src) |capturedVal| {
                cy.arc.release(vm, capturedVal);
            }
            if (obj.closure.numCaptured <= 3) {
                freePoolObject(vm, obj);
            } else {
                freeExternalObject(vm, obj, (2 + obj.closure.numCaptured) * @sizeOf(Value));
            }
        },
        rt.LambdaT => {
            freePoolObject(vm, obj);
        },
        rt.AstringT => {
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
                freeExternalObject(vm, obj, Astring.BufOffset + obj.astring.len);
            }
        },
        rt.UstringT => {
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
                freeExternalObject(vm, obj, Ustring.BufOffset + obj.ustring.len);
            }
        },
        rt.StringSliceT => {
            if (obj.stringSlice.getParentPtr()) |parent| {
                cy.arc.releaseObject(vm, parent);
            }
            freePoolObject(vm, obj);
        },
        rt.RawstringT => {
            if (obj.rawstring.len <= MaxPoolObjectRawStringByteLen) {
                freePoolObject(vm, obj);
            } else {
                freeExternalObject(vm, obj, RawString.BufOffset + obj.rawstring.len);
            }
        },
        rt.RawstringSliceT => {
            const parent: *cy.HeapObject = @ptrCast(obj.rawstringSlice.parent);
            cy.arc.releaseObject(vm, parent);
            freePoolObject(vm, obj);
        },
        rt.FiberT => {
            const fiber: *cy.fiber.Fiber = @ptrCast(obj);
            cy.fiber.releaseFiberStack(vm, fiber) catch |err| {
                stdx.panicFmt("release fiber: {}", .{err});
            };
            freeExternalObject(vm, obj, @sizeOf(cy.fiber.Fiber));
        },
        rt.BoxT => {
            cy.arc.release(vm, obj.box.val);
            freePoolObject(vm, obj);
        },
        rt.NativeFuncT => {
            if (obj.nativeFunc1.hasTccState) {
                cy.arc.releaseObject(vm, obj.nativeFunc1.tccState.asHeapObject());
            }
            freePoolObject(vm, obj);
        },
        rt.TccStateT => {
            if (cy.hasJit) {
                tcc.tcc_delete(obj.tccState.state);
                obj.tccState.lib.close();
                vm.alloc.destroy(obj.tccState.lib);
                freePoolObject(vm, obj);
            } else {
                unreachable;
            }
        },
        rt.PointerT => {
            freePoolObject(vm, obj);
        },
        rt.FileT => {
            if (cy.hasStdFiles) {
                if (obj.file.hasReadBuf) {
                    vm.alloc.free(obj.file.readBuf[0..obj.file.readBufCap]);
                }
                obj.file.close();
            }
            freePoolObject(vm, obj);
        },
        rt.DirT => {
            if (cy.hasStdFiles) {
                obj.dir.close();
            }
            freePoolObject(vm, obj);
        },
        rt.DirIteratorT => {
            if (cy.hasStdFiles) {
                var dir: *DirIterator = @ptrCast(obj);
                if (dir.recursive) {
                    const walker = stdx.ptrAlignCast(*std.fs.IterableDir.Walker, &dir.inner.walker);
                    walker.deinit();   
                }
                cy.arc.releaseObject(vm, @ptrCast(dir.dir));
            }
            freeExternalObject(vm, obj, @sizeOf(DirIterator));
        },
        rt.MetaTypeT => {
            freePoolObject(vm, obj);
        },
        else => {
            // Struct deinit.
            if (builtin.mode == .Debug) {

                if (cy.verbose) {
                    log.debug("free {s}", .{vm.types.buf[obj.head.typeId].name});
                }

                // Check range.
                if (obj.head.typeId >= vm.types.len) {
                    log.debug("unsupported struct type {}", .{obj.head.typeId});
                    stdx.fatal();
                }
            }
            const numFields = vm.types.buf[obj.head.typeId].numFields;
            for (obj.object.getValuesConstPtr()[0..numFields]) |child| {
                cy.arc.release(vm, child);
            }
            if (numFields <= 4) {
                freePoolObject(vm, obj);
            } else {
                freeExternalObject(vm, obj, (1 + numFields) * @sizeOf(Value));
            }
        },
    }
}

pub fn traceAlloc(vm: *cy.VM, ptr: *HeapObject) void {
    // log.debug("alloc {*} {} {}", .{ptr, ptr.head.typeId, vm.debugPc});
    vm.objectTraceMap.put(vm.alloc, ptr, .{
        .allocPc = vm.debugPc,
        .freePc = cy.NullId,
        .freeTypeId = cy.NullId,
    }) catch stdx.fatal();
}

pub fn getTypeName(vm: *const cy.VM, typeId: rt.TypeId) []const u8 {
    return vm.types.buf[typeId].name;
}

test "Free object invalidation." {
    var vm: cy.VM = undefined;
    try vm.init(t.alloc);
    if (cy.TraceEnabled) {
        var trace = cy.TraceInfo{};
        vm.trace = &trace;
    }
    defer vm.deinit(false);

    // Invalidation only happens in Debug mode.
    if (builtin.mode == .Debug) {
        // Free pool object with no previous free slot invalidates object pointer.
        var obj = try allocPoolObject(&vm);
        obj.head.typeId = 100;
        freePoolObject(&vm, obj);
        try t.eq(obj.head.typeId, cy.NullId);

        // Free pool object with previous free slot invalidates object pointer.
        var obj1 = try allocPoolObject(&vm);
        obj1.head.typeId = 100;
        var obj2 = try allocPoolObject(&vm);
        obj2.head.typeId = 100;
        freePoolObject(&vm, obj1); // Previous slot is freed.
        freePoolObject(&vm, obj2);
        try t.eq(obj1.head.typeId, cy.NullId);
        try t.eq(obj2.head.typeId, cy.NullId);

        // Free external object invalidates object pointer.
        obj = try allocExternalObject(&vm, 40);
        obj.head.typeId = 100;
        vm.debugPc = 123;
        freeExternalObject(&vm, obj, 40);
        try t.eq(cy.arc.isObjectAlreadyFreed(&vm, obj), true);
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
    try t.eq(@intFromPtr(&list.list.ptr), @intFromPtr(&list) + 8);
    try t.eq(@alignOf(ListIterator), 8);

    try t.eq(@alignOf(Dir), 8);
    var dir: Dir = undefined;
    try t.eq(@intFromPtr(&dir.structId), @intFromPtr(&dir));
    try t.eq(@intFromPtr(&dir.rc), @intFromPtr(&dir) + 4);

    try t.eq(@alignOf(DirIterator), 8);
    var dirIter: DirIterator = undefined;
    try t.eq(@intFromPtr(&dirIter.structId), @intFromPtr(&dirIter));
    try t.eq(@intFromPtr(&dirIter.rc), @intFromPtr(&dirIter) + 4);

    const rstr = RawString{
        .structId = rt.RawstringT,
        .rc = 1,
        .len = 1,
        .bufStart = undefined,
    };
    try t.eq(@intFromPtr(&rstr.structId), @intFromPtr(&rstr));
    try t.eq(@intFromPtr(&rstr.rc), @intFromPtr(&rstr) + 4);
    try t.eq(@intFromPtr(&rstr.len), @intFromPtr(&rstr) + 8);
    try t.eq(RawString.BufOffset, 12);
    try t.eq(@intFromPtr(&rstr.bufStart), @intFromPtr(&rstr) + RawString.BufOffset);

    const astr = Astring{
        .structId = rt.AstringT,
        .rc = 1,
        .len = 1,
        .bufStart = undefined,
    };
    try t.eq(@intFromPtr(&astr.structId), @intFromPtr(&astr));
    try t.eq(@intFromPtr(&astr.rc), @intFromPtr(&astr) + 4);
    try t.eq(@intFromPtr(&astr.len), @intFromPtr(&astr) + 8);
    try t.eq(Astring.BufOffset, 12);
    try t.eq(@intFromPtr(&astr.bufStart), @intFromPtr(&astr) + Astring.BufOffset);

    const ustr = Ustring{
        .structId = rt.UstringT,
        .rc = 1,
        .len = 1,
        .charLen = 1,
        .mruIdx = 0,
        .mruCharIdx = 0,
        .bufStart = undefined,
    };
    try t.eq(@intFromPtr(&ustr.structId), @intFromPtr(&ustr));
    try t.eq(@intFromPtr(&ustr.rc), @intFromPtr(&ustr) + 4);
    try t.eq(@intFromPtr(&ustr.len), @intFromPtr(&ustr) + 8);
    try t.eq(@intFromPtr(&ustr.charLen), @intFromPtr(&ustr) + 12);
    try t.eq(@intFromPtr(&ustr.mruIdx), @intFromPtr(&ustr) + 16);
    try t.eq(@intFromPtr(&ustr.mruCharIdx), @intFromPtr(&ustr) + 20);
    try t.eq(Ustring.BufOffset, 24);
    try t.eq(@intFromPtr(&ustr.bufStart), @intFromPtr(&ustr) + Ustring.BufOffset);

    const slice = StringSlice{
        .structId = rt.StringSliceT,
        .rc = 1,
        .buf = undefined,
        .len = 1,
        .uCharLen = undefined,
        .uMruIdx = undefined,
        .uMruCharIdx = undefined,
        .extra = undefined,
    };
    try t.eq(@intFromPtr(&slice.structId), @intFromPtr(&slice));
    try t.eq(@intFromPtr(&slice.rc), @intFromPtr(&slice) + 4);
    try t.eq(@intFromPtr(&slice.buf), @intFromPtr(&slice) + 8);
    try t.eq(@intFromPtr(&slice.len), @intFromPtr(&slice) + 16);
    try t.eq(@intFromPtr(&slice.uCharLen), @intFromPtr(&slice) + 20);
    try t.eq(@intFromPtr(&slice.uMruIdx), @intFromPtr(&slice) + 24);
    try t.eq(@intFromPtr(&slice.uMruCharIdx), @intFromPtr(&slice) + 28);
    try t.eq(@intFromPtr(&slice.extra), @intFromPtr(&slice) + 32);

    const rslice = RawStringSlice{
        .structId = rt.RawstringSliceT,
        .rc = 1,
        .buf = undefined,
        .len = 1,
        .parent = undefined,
    };
    try t.eq(@intFromPtr(&rslice.structId), @intFromPtr(&rslice));
    try t.eq(@intFromPtr(&rslice.rc), @intFromPtr(&rslice) + 4);
    try t.eq(@intFromPtr(&rslice.buf), @intFromPtr(&rslice) + 8);
    try t.eq(@intFromPtr(&rslice.len), @intFromPtr(&rslice) + 16);
    try t.eq(@intFromPtr(&rslice.parent), @intFromPtr(&rslice) + 24);
}