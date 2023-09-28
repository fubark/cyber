// Copyright (c) 2023 Cyber (See LICENSE)

/// Heap objects, object allocation and deinitializers.

const cy = @import("cyber.zig");
const vmc = cy.vmc;
const rt = cy.rt;
const Value = cy.Value;
const mi = @import("mimalloc");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const std = @import("std");
const builtin = @import("builtin");
const tcc = @import("tcc");
const log = cy.log.scoped(.heap);
const NullId = std.math.maxInt(u32);
const NullU8 = std.math.maxInt(u8);

var gpa: std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = false,
    .stack_trace_frames = if (builtin.mode == .Debug) 10 else 0,
}) = .{};
var miAlloc: mi.Allocator = undefined;
var initedAllocator = false;

fn initAllocator() void {
    defer initedAllocator = true;
    switch (cy.Malloc) {
        .zig,
        .malloc => return,
        .mimalloc => {
            miAlloc.init();
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
    switch (cy.Malloc) {
        .mimalloc => {
            return miAlloc.allocator();
        },
        .malloc => {
            return std.heap.c_allocator;
        },
        .zig => {
            if (cy.isWasm) {
                return std.heap.wasm_allocator;
            } else {
                return gpa.allocator();
            }
        },
    }
}

pub fn deinitAllocator() void {
    switch (cy.Malloc) {
        .mimalloc => {
            miAlloc.deinit();
            initedAllocator = false;
        },
        .malloc => {
            return;
        },
        .zig => {
            if (cy.isWasm) {
                return;
            } else {
                _ = gpa.deinit();
                initedAllocator = false;
            }
        },
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
    tuple: Tuple,
    list: List,
    listIter: ListIterator,
    map: Map,
    mapIter: MapIterator,
    closure: Closure,
    lambda: Lambda,
    astring: Astring,
    ustring: Ustring,
    stringSlice: vmc.StringSlice,
    rawstring: RawString,
    rawstringSlice: RawStringSlice,
    object: Object,
    box: Box,
    nativeFunc1: NativeFunc1,
    tccState: if (cy.hasFFI) TccState else void,
    pointer: Pointer,
    metatype: MetaType,

    pub inline fn getTypeId(self: HeapObject) u32 {
        return self.head.typeId & vmc.TYPE_MASK;
    }

    pub inline fn isFreed(self: HeapObject) bool {
        return self.head.typeId == cy.NullId;
    }

    pub inline fn isGcMarked(self: HeapObject) bool {
        return (self.head.typeId & vmc.GC_MARK_MASK) == vmc.GC_MARK_MASK;
    }

    pub inline fn setGcMarked(self: *HeapObject) void {
        self.head.typeId = self.head.typeId | vmc.GC_MARK_MASK;
    }

    pub inline fn resetGcMarked(self: *HeapObject) void {
        self.head.typeId = self.head.typeId & ~vmc.GC_MARK_MASK;
    }

    pub inline fn isGcConfirmedCyc(self: *HeapObject) bool {
        return (self.head.typeId & vmc.GC_MARK_CYC_TYPE_MASK) == vmc.CYC_TYPE_MASK;
    }

    pub inline fn isCyclable(self: *HeapObject) bool {
        return (self.head.typeId & vmc.CYC_TYPE_MASK) == vmc.CYC_TYPE_MASK;
    }

    pub inline fn isPoolObject(self: *HeapObject) bool {
        return (self.head.typeId & vmc.POOL_TYPE_MASK) == vmc.POOL_TYPE_MASK;
    }

    pub inline fn getDListNode(self: *HeapObject) *DListNode {
        if (cy.Malloc == .zig) {
            return @ptrFromInt(@intFromPtr(self) - @sizeOf(DListNode) - @sizeOf(u64));
        } else {
            return @ptrCast(@as([*]DListNode, @ptrCast(self)) - 1);
        }
    }

    pub fn getUserTag(self: *const HeapObject) cy.ValueUserTag {
        switch (self.getTypeId()) {
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

pub const Tuple = extern struct {
    typeId: rt.TypeId,
    rc: u32,
    len: u32,
    padding: u32,
    firstValue: Value,

    pub fn getElemsPtr(self: *Tuple) [*]Value {
        return @ptrCast(&self.firstValue);
    }
};

pub const List = extern struct {
    typeId: rt.TypeId,
    rc: u32,
    list: extern struct {
        ptr: [*]Value,
        cap: usize,
        len: usize,
    },

    pub fn getList(self: *List) *cy.List(Value) {
        return cy.ptrAlignCast(*cy.List(Value), &self.list);
    }

    pub inline fn items(self: *const List) []Value {
        return self.list.ptr[0..self.list.len];
    }

    /// Assumes `val` is retained.
    pub fn append(self: *List, alloc: std.mem.Allocator, val: Value) linksection(cy.Section) void {
        const list = cy.ptrAlignCast(*cy.List(Value), &self.list);
        if (list.len == list.buf.len) {
            // After reaching a certain size, use power of two ceil.
            // This reduces allocations for big lists while not over allocating for smaller lists.
            if (list.len > 512) {
                const newCap = std.math.ceilPowerOfTwo(u32, @as(u32, @intCast(list.len)) + 1) catch cy.fatal();
                list.growTotalCapacityPrecise(alloc, newCap) catch cy.fatal();
            } else {
                list.growTotalCapacity(alloc, list.len + 1) catch cy.fatal();
            }
        }
        list.appendAssumeCapacity(val);
    }
};

pub const ListIterator = extern struct {
    typeId: rt.TypeId,
    rc: u32,
    list: *List,
    nextIdx: u32,
};

pub const MapInner = cy.ValueMap;
pub const Map = extern struct {
    typeId: rt.TypeId,
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
        return cy.ptrAlignCast(*MapInner, &self.inner);
    }

    pub fn set(self: *Map, vm: *cy.VM, index: Value, val: Value) !void {
        const m = cy.ptrAlignCast(*cy.MapInner, &self.inner);
        const res = try m.getOrPut(vm.alloc, vm, index);
        if (res.foundExisting) {
            cy.arc.release(vm, res.valuePtr.*);
        } else {
            // No previous entry, retain key.
            cy.arc.retain(vm, index);
        }
        cy.arc.retain(vm, val);
        res.valuePtr.* = val;
    }
};

pub const MapIterator = extern struct {
    typeId: rt.TypeId,
    rc: u32,
    map: *Map,
    nextIdx: u32,
};

pub const Closure = extern struct {
    typeId: rt.TypeId,
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    numCaptured: u8,
    stackSize: u8,
    /// Closure value is copied to this local to provide captured var lookup.
    local: u8,
    funcSigId: u64,

    // Begins array of `Box` values.
    firstCapturedVal: Value,

    pub inline fn getCapturedValuesPtr(self: *Closure) [*]Value {
        return @ptrCast(&self.firstCapturedVal);
    }
};

const Lambda = extern struct {
    typeId: rt.TypeId,
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    stackSize: u8,
    padding: u16 = 0,
    funcSigId: u64,
};

/// 28 byte length can fit inside a Heap pool object.
pub const MaxPoolObjectAstringByteLen = 28;

pub const Astring = extern struct {
    typeId: rt.TypeId,
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
    typeId: rt.TypeId,
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

pub const StringSlice = struct {
    pub inline fn getParentPtr(self: vmc.StringSlice) ?*cy.HeapObject {
        return @ptrFromInt(@as(usize, @intCast(self.extra & 0x7fffffffffffffff)));
    }

    pub inline fn isAstring(self: vmc.StringSlice) bool {
        return self.extra & (1 << 63) > 0;
    }

    pub inline fn getConstSlice(self: vmc.StringSlice) []const u8 {
        return self.buf[0..self.len];
    }
};

pub const MaxPoolObjectRawStringByteLen = 28;

pub const RawString = extern struct {
    typeId: if (cy.isWasm) rt.TypeId else rt.TypeId align(8),
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
    typeId: rt.TypeId,
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
    typeId: rt.TypeId,
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
    typeId: rt.TypeId,
    rc: u32,
    val: Value,
};

const NativeFunc1 = extern struct {
    typeId: rt.TypeId,
    rc: u32,
    func: vmc.HostFuncFn,
    numParams: u32,
    funcSigId: u32,
    tccState: Value,
    hasTccState: bool,
};

const TccState = extern struct {
    typeId: rt.TypeId,
    rc: u32,
    state: *tcc.TCCState,
    lib: *std.DynLib,
};

pub const Pointer = extern struct {
    typeId: rt.TypeId,
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

pub const DListNode = extern struct {
    prev: ?*DListNode,
    next: ?*DListNode,

    pub fn getHeapObject(self: *DListNode) *HeapObject {
        if (cy.Malloc == .zig) {
            return @ptrFromInt(@intFromPtr(self) + @sizeOf(DListNode) + @sizeOf(u64));
        } else {
            return @ptrFromInt(@intFromPtr(self) + @sizeOf(DListNode));
        }
    }
};

pub fn allocExternalObject(vm: *cy.VM, size: usize, comptime cyclable: bool) !*HeapObject {
    // Align with HeapObject so it can be casted.
    const addToCyclableList = comptime (cy.hasGC and cyclable);

    // An extra size field is included for the Zig allocator.
    // u64 so it can be 8 byte aligned.
    const ZigLenSize = if (cy.Malloc == .zig) @sizeOf(u64) else 0;
    const PayloadSize = (if (addToCyclableList) @sizeOf(DListNode) else 0) + ZigLenSize;

    const slice = try vm.alloc.alignedAlloc(u8, @alignOf(HeapObject), size + PayloadSize);
    defer {
        if (cy.Trace) {
            cy.heap.traceAlloc(vm, @ptrCast(slice.ptr + PayloadSize));
        }
    }
    if (addToCyclableList) {
        const node: *DListNode = @ptrCast(slice.ptr);
        vm.cyclableHead.prev = node;
        node.* = .{
            .prev = null,
            .next = vm.cyclableHead,
        };
        vm.cyclableHead = node;
    }
    if (cy.Malloc == .zig) {
        @as(*u64, @ptrCast(slice.ptr + PayloadSize - ZigLenSize)).* = size;
    }
    if (cy.TrackGlobalRC) {
        vm.refCounts += 1;
    }
    if (cy.Trace) {
        vm.trace.numRetains += 1;
        vm.trace.numRetainAttempts += 1;
    }
    return @ptrCast(slice.ptr + PayloadSize);
}

/// Assumes new object will have an RC = 1.
pub fn allocPoolObject(self: *cy.VM) linksection(cy.HotSection) !*HeapObject {
    if (self.heapFreeHead == null) {
        const list = try growHeapPages(self, @max(1, (self.heapPages.len * 15) / 10));
        self.heapFreeHead = list.head;
        if (cy.Trace) {
            self.heapFreeTail = list.tail;
        }
    }
    const ptr = self.heapFreeHead.?;
    defer {
        if (cy.Trace) {
            traceAlloc(self, ptr);
            if (self.heapFreeTail == ptr) {
                // Ensure tail is updated if it was the same segment as head.
                self.heapFreeTail = self.heapFreeHead;
            }
        }
        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.Trace) {
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

fn freeExternalObject(vm: *cy.VM, obj: *HeapObject, len: usize, comptime cyclable: bool) void {
    // Unlink.
    if (cy.hasGC) {
        if (cyclable) {
            const node = obj.getDListNode();
            if (node.prev) |prev| {
                prev.next = node.next;
                if (node.next) |next| {
                    next.prev = prev;
                }
            } else {
                // It's the head.
                vm.cyclableHead = node.next.?;
                node.next.?.prev = null;
            }
        }
    }
    if (cy.Trace) {
        if (vm.objectTraceMap.getPtr(obj)) |trace| {
            trace.freePc = vm.debugPc;
            trace.freeTypeId = obj.getTypeId();
        } else {
            log.debug("Missing object trace {*} {}", .{obj, obj.getTypeId()});
        }
    }
    const ZigLenSize = if (cy.Malloc == .zig) @sizeOf(u64) else 0;
    const PayloadSize = (if (cy.hasGC and cyclable) @sizeOf(DListNode) else 0) + ZigLenSize;
    const slice = (@as([*]align(@alignOf(HeapObject)) u8, @ptrCast(obj)) - PayloadSize)[0..len + PayloadSize];
    vm.alloc.free(slice);
}

/// typeId should be cleared in trace mode since tracking may still hold a reference to the object.
/// The gc also needs it for now to traverse objects in pages.
pub fn freePoolObject(vm: *cy.VM, obj: *HeapObject) linksection(cy.HotSection) void {
    if (cy.Trace) {
        if (vm.objectTraceMap.getPtr(obj)) |trace| {
            trace.freePc = vm.debugPc;
            trace.freeTypeId = obj.getTypeId();
        } else {
            log.debug("Missing object trace {*} {}", .{obj, obj.getTypeId()});
        }
    }
    const prev = &(@as([*]HeapObject, @ptrCast(obj)) - 1)[0];
    if (prev.freeSpan.typeId == NullId) {
        // Left is a free span. Extend length.
        prev.freeSpan.start.freeSpan.len += 1;
        obj.freeSpan.start = prev.freeSpan.start;
        obj.freeSpan.typeId = cy.NullId;
    } else {
        if (cy.Trace) {
            // Tracing performs LRU allocation to surface double free errors better.
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
            // Update MRU free list head.
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
        .typeId = rt.ListT | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .list = .{
            .ptr = undefined,
            .len = 0,
            .cap = 0,
        },
    };
    return Value.initCycPtr(obj);
}

pub fn allocOwnedList(self: *cy.VM, elems: []Value) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .typeId = rt.ListT | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .list = .{
            .ptr = elems.ptr,
            .len = elems.len,
            .cap = elems.len,
        },
    };
    return Value.initCycPtr(obj);
}

pub fn allocListFill(self: *cy.VM, val: Value, n: u32) linksection(cy.StdSection) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .typeId = rt.ListT | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .list = .{
            .ptr = undefined,
            .len = 0,
            .cap = 0,
        },
    };
    const list = cy.ptrAlignCast(*cy.List(Value), &obj.list.list);
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
    return Value.initCycPtr(obj);
}

pub fn allocHostObject(vm: *cy.VM, typeId: rt.TypeId, numBytes: usize) linksection(cy.HotSection) !*anyopaque {
    if (numBytes <= MaxPoolObjectUserBytes) {
        const obj = try allocPoolObject(vm);
        obj.head = .{
            .typeId = typeId | vmc.POOL_TYPE_MASK,
            .rc = 1,
        };
        return @ptrFromInt(@intFromPtr(obj) + 8);
    } else {
        const obj = try allocExternalObject(vm, numBytes + 8, false);
        obj.head = .{
            .typeId = typeId,
            .rc = 1,
        };
        return @ptrFromInt(@intFromPtr(obj) + 8);
    }
}

pub fn allocHostCycObject(vm: *cy.VM, typeId: rt.TypeId, numBytes: usize) linksection(cy.HotSection) !*anyopaque {
    if (numBytes <= MaxPoolObjectUserBytes) {
        const obj = try allocPoolObject(vm);
        obj.head = .{
            .typeId = typeId | vmc.CYC_TYPE_MASK | vmc.POOL_TYPE_MASK,
            .rc = 1,
        };
        return @ptrFromInt(@intFromPtr(obj) + 8);
    } else {
        const obj = try allocExternalObject(vm, numBytes + 8, true);
        obj.head = .{
            .typeId = typeId | vmc.CYC_TYPE_MASK,
            .rc = 1,
        };
        return @ptrFromInt(@intFromPtr(obj) + 8);
    }
}

pub fn allocTuple(vm: *cy.VM, elems: []const Value) linksection(cy.HotSection) !Value {
    var obj: *HeapObject = undefined;
    if (elems.len <= 3) {
        obj = try allocPoolObject(vm);
    } else {
        obj = try allocExternalObject(vm, elems.len * 8 + 16, true);
    }
    obj.tuple = .{
        .typeId = rt.TupleT | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .len = @intCast(elems.len),
        .padding = undefined,
        .firstValue = undefined,
    };
    const dst = obj.tuple.getElemsPtr()[0..elems.len];
    std.mem.copy(Value, dst, elems);
    return Value.initCycPtr(obj);
}

pub fn allocList(self: *cy.VM, elems: []const Value) linksection(cy.HotSection) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .typeId = rt.ListT | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .list = .{
            .ptr = undefined,
            .len = 0,
            .cap = 0,
        },
    };
    const list = cy.ptrAlignCast(*cy.List(Value), &obj.list.list);
    // Initializes capacity to exact size.
    try list.ensureTotalCapacityPrecise(self.alloc, elems.len);
    list.len = elems.len;
    std.mem.copy(Value, list.items(), elems);
    return Value.initCycPtr(obj);
}

/// Assumes list is already retained for the iterator.
pub fn allocListIterator(self: *cy.VM, list: *List) linksection(cy.HotSection) !Value {
    const obj = try allocPoolObject(self);
    obj.listIter = .{
        .typeId = rt.ListIteratorT | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .list = list,
        .nextIdx = 0,
    };
    return Value.initCycPtr(obj);
}

pub fn allocEmptyMap(self: *cy.VM) !Value {
    const obj = try allocPoolObject(self);
    obj.map = .{
        .typeId = rt.MapT | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .inner = .{
            .metadata = null,
            .entries = null,
            .size = 0,
            .cap = 0,
            .available = 0,
        },
    };
    return Value.initCycPtr(obj);
}

pub fn allocMap(self: *cy.VM, keyIdxs: []const align(1) u16, vals: []const Value) !Value {
    const obj = try allocPoolObject(self);
    obj.map = .{
        .typeId = rt.MapT | vmc.CYC_TYPE_MASK,
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

    return Value.initCycPtr(obj);
}

/// Assumes map is already retained for the iterator.
pub fn allocMapIterator(self: *cy.VM, map: *Map) linksection(cy.HotSection) !Value {
    const obj = try allocPoolObject(self);
    obj.mapIter = .{
        .typeId = rt.MapIteratorT | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .map = map,
        .nextIdx = 0,
    };
    return Value.initCycPtr(obj);
}

/// Captured values are retained during alloc.
pub fn allocClosure(
    self: *cy.VM, fp: [*]Value, funcPc: usize, numParams: u8, stackSize: u8,
    funcSigId: u16, capturedVals: []const cy.Inst, closureLocal: u8,
) !Value {
    var obj: *HeapObject = undefined;
    if (capturedVals.len <= 2) {
        obj = try allocPoolObject(self);
    } else {
        obj = try allocExternalObject(self, (2 + capturedVals.len) * @sizeOf(Value));
    }
    obj.closure = .{
        .typeId = rt.ClosureT | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .funcPc = @intCast(funcPc),
        .numParams = numParams,
        .stackSize = stackSize,
        .numCaptured = @intCast(capturedVals.len),
        .local = closureLocal,
        .funcSigId = funcSigId,
        .firstCapturedVal = undefined,
    };
    const dst = obj.closure.getCapturedValuesPtr();
    for (capturedVals, 0..) |local, i| {
        if (cy.Trace) {
            if (!fp[local.val].isBox()) {
                cy.panic("Expected box value.");
            }
        }
        cy.arc.retain(self, fp[local.val]);
        dst[i] = fp[local.val];
    }
    return Value.initCycPtr(obj);
}

pub fn allocLambda(self: *cy.VM, funcPc: usize, numParams: u8, stackSize: u8, funcSigId: u16) !Value {
    const obj = try allocPoolObject(self);
    obj.lambda = .{
        .typeId = rt.LambdaT,
        .rc = 1,
        .funcPc = @intCast(funcPc),
        .numParams = numParams,
        .stackSize = stackSize,
        .funcSigId = funcSigId,
    };
    return Value.initPtr(obj);
}

pub fn allocStringTemplate(self: *cy.VM, strs: []const cy.Inst, vals: []const Value) !Value {
    const firstStr = self.valueAsStaticString(Value.initRaw(self.consts[strs[0].val].val));
    try self.u8Buf.resize(self.alloc, firstStr.len);
    std.mem.copy(u8, self.u8Buf.items(), firstStr);

    const writer = self.u8Buf.writer(self.alloc);
    for (vals, 0..) |val, i| {
        self.writeValueToString(writer, val);
        try self.u8Buf.appendSlice(self.alloc, self.valueAsStaticString(Value.initRaw(self.consts[strs[i+1].val].val)));
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
        obj = try allocExternalObject(self, len + RawString.BufOffset, false);
    }
    obj.rawstring = .{
        .typeId = rt.RawstringT,
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
        obj = try allocExternalObject(self, len + Astring.BufOffset, false);
    }
    obj.astring = .{
        .typeId = rt.AstringT,
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
        obj = try allocExternalObject(self, len + Ustring.BufOffset, false);
    }
    obj.ustring = .{
        .typeId = rt.UstringT,
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
        .typeId = rt.StringSliceT,
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
        .typeId = rt.StringSliceT,
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
        obj = try allocExternalObject(self, len + RawString.BufOffset, false);
    }
    obj.rawstring = .{
        .typeId = rt.RawstringT,
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
        .typeId = rt.RawstringSliceT,
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
        .typeId = rt.BoxT | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .val = val,
    };
    return Value.initCycPtr(obj);
}

pub fn allocNativeFunc1(self: *cy.VM, func: cy.ZHostFuncFn, numParams: u32, funcSigId: cy.sema.FuncSigId, tccState: ?Value) !Value {
    const obj = try allocPoolObject(self);
    obj.nativeFunc1 = .{
        .typeId = rt.NativeFuncT,
        .rc = 1,
        .func = @ptrCast(func),
        .numParams = numParams,
        .funcSigId = funcSigId,
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
        .typeId = rt.TccStateT,
        .rc = 1,
        .state = state,
        .lib = lib,
    };
    return Value.initPtr(obj);
}

pub fn allocPointer(self: *cy.VM, ptr: ?*anyopaque) !Value {
    const obj = try allocPoolObject(self);
    obj.pointer = .{
        .typeId = rt.PointerT,
        .rc = 1,
        .ptr = ptr,
    };
    return Value.initPtr(obj);
}

/// Allocates an object outside of the object pool.
pub fn allocObject(self: *cy.VM, sid: rt.TypeId, fields: []const Value) !Value {
    // First slot holds the typeId and rc.
    const obj: *Object = @ptrCast(try allocExternalObject(self, (1 + fields.len) * @sizeOf(Value), true));
    obj.* = .{
        .typeId = sid | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .firstValue = undefined,
    };
    const dst = obj.getValuesPtr();
    std.mem.copy(Value, dst[0..fields.len], fields);

    return Value.initCycPtr(obj);
}

pub fn allocEmptyObject(self: *cy.VM, sid: rt.TypeId, numFields: u32) !Value {
    // First slot holds the typeId and rc.
    const obj: *Object = @ptrCast(try allocExternalObject(self, (1 + numFields) * @sizeOf(Value), true));
    obj.* = .{
        .typeId = sid | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .firstValue = undefined,
    };
    return Value.initCycPtr(obj);
}

pub fn allocObjectSmall(self: *cy.VM, sid: rt.TypeId, fields: []const Value) !Value {
    const obj = try allocPoolObject(self);
    obj.object = .{
        .typeId = sid | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .firstValue = undefined,
    };

    const dst = obj.object.getValuesPtr();
    std.mem.copy(Value, dst[0..fields.len], fields);

    return Value.initCycPtr(obj);
}

pub fn allocEmptyObjectSmall(self: *cy.VM, sid: rt.TypeId) !Value {
    const obj = try allocPoolObject(self);
    obj.object = .{
        .typeId = sid | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .firstValue = undefined,
    };
    return Value.initCycPtr(obj);
}

pub fn allocFuncFromSym(self: *cy.VM, symId: cy.vm.SymbolId) !Value {
    const sym = self.funcSyms.buf[symId];
    switch (@as(cy.rt.FuncSymbolType, @enumFromInt(sym.entryT))) {
        .nativeFunc1 => {
            const funcSigId = sym.innerExtra.nativeFunc1.funcSigId;
            const numParams = sym.innerExtra.nativeFunc1.numParams();
            return allocNativeFunc1(self, sym.inner.nativeFunc1, numParams, funcSigId, null);
        },
        .func => {
            return allocLambda(self, sym.inner.func.pc,
                @intCast(sym.inner.func.numParams),
                @intCast(sym.inner.func.stackSize),
                @intCast(sym.innerExtra.func.funcSigId)
            );
        },
        .closure => {
            cy.arc.retainObject(self, @ptrCast(sym.inner.closure));
            return Value.initPtr(sym.inner.closure);
        },
        .none => return Value.None,
    }
}

/// Use comptime options to keep closely related logic together.
/// TODO: flatten recursion.
pub fn freeObject(vm: *cy.VM, obj: *HeapObject,
    comptime releaseChildren: bool, comptime skipCycChildren: bool, comptime free: bool,
) linksection(cy.HotSection) void {
    if (cy.Trace) {
        if (obj.isFreed()) {
            cy.panicFmt("Double free object: {*} Should have been discovered in release op.", .{obj});
        } else {
            log.tracev("free type={}({s}) {*}", .{
                obj.getTypeId(), vm.getTypeName(obj.getTypeId()), obj,
            });
        }
    }
    switch (obj.getTypeId()) {
        rt.TupleT => {
            if (releaseChildren) {
                for (obj.tuple.getElemsPtr()[0..obj.tuple.len]) |it| {
                    if (skipCycChildren and it.isGcConfirmedCyc()) {
                        continue;
                    }
                    cy.arc.release(vm, it);
                }
            }
            if (free) {
                if (obj.tuple.len <= 3) {
                    freePoolObject(vm, obj);
                } else {
                    freeExternalObject(vm, obj, (2 + obj.tuple.len) * @sizeOf(Value), true);
                }
            }
        },
        rt.ListT => {
            const list = cy.ptrAlignCast(*cy.List(Value), &obj.list.list);
            if (releaseChildren) {
                for (list.items()) |it| {
                    if (skipCycChildren and it.isGcConfirmedCyc()) {
                        continue;
                    }
                    cy.arc.release(vm, it);
                }
            }
            if (free) {
                list.deinit(vm.alloc);
                freePoolObject(vm, obj);
            }
        },
        rt.ListIteratorT => {
            if (releaseChildren) {
                if (skipCycChildren) {
                    if (!@as(*HeapObject, @ptrCast(@alignCast(obj.listIter.list))).isGcConfirmedCyc()) {
                        cy.arc.releaseObject(vm, cy.ptrAlignCast(*HeapObject, obj.listIter.list));
                    }
                } else {
                    cy.arc.releaseObject(vm, cy.ptrAlignCast(*HeapObject, obj.listIter.list));
                }
            }
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        rt.MapT => {
            const map = cy.ptrAlignCast(*MapInner, &obj.map.inner);
            if (releaseChildren) {
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    if (skipCycChildren) {
                        if (!entry.key.isGcConfirmedCyc()) {
                            cy.arc.release(vm, entry.key);
                        }
                        if (!entry.value.isGcConfirmedCyc()) {
                            cy.arc.release(vm, entry.value);
                        }
                    } else {
                        cy.arc.release(vm, entry.key);
                        cy.arc.release(vm, entry.value);
                    }
                }
            }
            if (free) {
                map.deinit(vm.alloc);
                freePoolObject(vm, obj);
            }
        },
        rt.MapIteratorT => {
            if (releaseChildren) {
                if (skipCycChildren) {
                    if (!@as(*HeapObject, @ptrCast(@alignCast(obj.mapIter.map))).isGcConfirmedCyc()) {
                        cy.arc.releaseObject(vm, cy.ptrAlignCast(*HeapObject, obj.mapIter.map));
                    }
                } else {
                    cy.arc.releaseObject(vm, cy.ptrAlignCast(*HeapObject, obj.mapIter.map));
                }
            }
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        rt.ClosureT => {
            if (releaseChildren) {
                const src = obj.closure.getCapturedValuesPtr()[0..obj.closure.numCaptured];
                for (src) |capturedVal| {
                    if (skipCycChildren and capturedVal.isGcConfirmedCyc()) {
                        continue;
                    }
                    cy.arc.release(vm, capturedVal);
                }
            }
            if (free) {
                if (obj.closure.numCaptured <= 3) {
                    freePoolObject(vm, obj);
                } else {
                    freeExternalObject(vm, obj, (2 + obj.closure.numCaptured) * @sizeOf(Value), true);
                }
            }
        },
        rt.LambdaT => {
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        rt.AstringT => {
            if (free) {
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
                    freeExternalObject(vm, obj, Astring.BufOffset + obj.astring.len, false);
                }
            }
        },
        rt.UstringT => {
            if (free) {
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
                    freeExternalObject(vm, obj, Ustring.BufOffset + obj.ustring.len, false);
                }
            }
        },
        rt.StringSliceT => {
            if (releaseChildren) {
                if (cy.heap.StringSlice.getParentPtr(obj.stringSlice)) |parent| {
                    cy.arc.releaseObject(vm, parent);
                }
            }
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        rt.RawstringT => {
            if (free) {
                if (obj.rawstring.len <= MaxPoolObjectRawStringByteLen) {
                    freePoolObject(vm, obj);
                } else {
                    freeExternalObject(vm, obj, RawString.BufOffset + obj.rawstring.len, false);
                }
            }
        },
        rt.RawstringSliceT => {
            if (releaseChildren) {
                const parent: *cy.HeapObject = @ptrCast(obj.rawstringSlice.parent);
                cy.arc.releaseObject(vm, parent);
            }
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        rt.FiberT => {
            if (releaseChildren) {
                const fiber: *vmc.Fiber = @ptrCast(obj);
                // TODO: isCyc.
                cy.fiber.releaseFiberStack(vm, fiber) catch |err| {
                    cy.panicFmt("release fiber: {}", .{err});
                };
            }
            if (free) {
                freeExternalObject(vm, obj, @sizeOf(vmc.Fiber), true);
            }
        },
        rt.BoxT => {
            if (releaseChildren) {
                if (skipCycChildren) {
                    if (!obj.box.val.isGcConfirmedCyc()) {
                        cy.arc.release(vm, obj.box.val);
                    }
                } else {
                    cy.arc.release(vm, obj.box.val);
                }
            }
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        rt.NativeFuncT => {
            if (releaseChildren) {
                if (obj.nativeFunc1.hasTccState) {
                    cy.arc.releaseObject(vm, obj.nativeFunc1.tccState.asHeapObject());
                }
            }
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        rt.TccStateT => {
            if (cy.hasFFI) {
                if (free) {
                    tcc.tcc_delete(obj.tccState.state);
                    obj.tccState.lib.close();
                    vm.alloc.destroy(obj.tccState.lib);
                    freePoolObject(vm, obj);
                }
            } else {
                unreachable;
            }
        },
        rt.PointerT => {
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        rt.MetaTypeT => {
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        else => {
            // Struct deinit.
            if (cy.Trace) {
                if (cy.verbose) {
                    log.debug("free {s}", .{vm.getTypeName(obj.getTypeId())});
                }

                // Check range.
                if (obj.getTypeId() >= vm.types.len) {
                    log.debug("unsupported struct type {}", .{obj.getTypeId()});
                    cy.fatal();
                }
            }
            // TODO: Determine isHostObject from object to avoid extra read from `rt.Type`
            const entry = &vm.types.buf[obj.getTypeId()];
            if (!entry.isHostObject) {
                const numFields = entry.data.numFields;
                if (releaseChildren) {
                    for (obj.object.getValuesConstPtr()[0..numFields]) |child| {
                        if (skipCycChildren and child.isGcConfirmedCyc()) {
                            continue;
                        }
                        cy.arc.release(vm, child);
                    }
                }
                if (free) {
                    if (numFields <= 4) {
                        freePoolObject(vm, obj);
                    } else {
                        freeExternalObject(vm, obj, (1 + numFields) * @sizeOf(Value), true);
                    }
                }
            } else {
                if (releaseChildren) {
                    if (entry.data.hostObject.getChildren) |getChildren| {
                        const children = @as(cy.ObjectGetChildrenFn, @ptrCast(@alignCast(getChildren)))(@ptrCast(vm), @ptrFromInt(@intFromPtr(obj) + 8));
                        for (children.slice()) |child| {
                            if (skipCycChildren and child.isGcConfirmedCyc()) {
                                continue;
                            }
                            cy.arc.release(vm, child);
                        }
                    }
                }
                if (free) {
                    if (entry.data.hostObject.finalizer) |finalizer| {
                        @as(cy.ObjectFinalizerFn, @ptrCast(@alignCast(finalizer)))(@ptrCast(vm), @ptrFromInt(@intFromPtr(obj) + 8));
                        if (obj.isPoolObject()) {
                            freePoolObject(vm, obj);
                        } else {
                            if (obj.isCyclable()) {
                                if (cy.Malloc == .zig) {
                                    const size = (@as([*]u64, @ptrCast(obj)) - 1)[0];
                                    freeExternalObject(vm, obj, @intCast(size), true);
                                } else {
                                    freeExternalObject(vm, obj, 1, true);
                                }
                            } else {
                                if (cy.Malloc == .zig) {
                                    const size = (@as([*]u64, @ptrCast(obj)) - 1)[0];
                                    freeExternalObject(vm, obj, @intCast(size), false);
                                } else {
                                    freeExternalObject(vm, obj, 1, false);
                                }
                            }
                        }
                    }
                }
            }
        },
    }
}

// User bytes excludes the type header.
const MaxPoolObjectUserBytes = @sizeOf(HeapObject) - 8;

pub fn traceAlloc(vm: *cy.VM, ptr: *HeapObject) void {
    // log.debug("alloc {*} {} {}", .{ptr, ptr.getTypeId(), vm.debugPc});
    vm.objectTraceMap.put(vm.alloc, ptr, .{
        .allocPc = vm.debugPc,
        .freePc = cy.NullId,
        .freeTypeId = cy.NullId,
    }) catch cy.fatal();
}

test "Free object invalidation." {
    var vm: cy.VM = undefined;
    try vm.init(t.alloc);
    defer vm.deinit(false);

    // Invalidation only happens in trace mode.
    if (cy.Trace) {
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
        obj = try allocExternalObject(&vm, 40, false);
        obj.head.typeId = 100;
        vm.debugPc = 123;
        freeExternalObject(&vm, obj, 40, false);
        try t.eq(cy.arc.isObjectAlreadyFreed(&vm, obj), true);
    }
}

test "heap internals." {
    if (cy.is32Bit) {
        try t.eq(@sizeOf(MapInner), 20);
        try t.eq(@alignOf(List), 4);
        try t.eq(@alignOf(ListIterator), 4);
        try t.eq(@sizeOf(List), 20);
        try t.eq(@sizeOf(ListIterator), 16);
        try t.eq(@sizeOf(Map), 32);
        try t.eq(@sizeOf(MapIterator), 16);
        try t.eq(@sizeOf(RawStringSlice), 24);
        try t.eq(@sizeOf(Pointer), 12);
    } else {
        try t.eq(@sizeOf(MapInner), 32);
        try t.eq(@alignOf(List), 8);
        try t.eq(@alignOf(ListIterator), 8);

        if (builtin.os.tag != .windows) {
            try t.eq(@sizeOf(List), 32);
            try t.eq(@sizeOf(ListIterator), 24);
            try t.eq(@sizeOf(Map), 40);
            try t.eq(@sizeOf(MapIterator), 24);
            try t.eq(@sizeOf(RawStringSlice), 32);
            try t.eq(@sizeOf(Pointer), 16);
        }
    }
    if (builtin.os.tag != .windows) {
        try t.eq(@sizeOf(Closure), 32);
        try t.eq(@sizeOf(Lambda), 24);
        try t.eq(@sizeOf(Astring), 16);
        try t.eq(@sizeOf(Ustring), 28);
        try t.eq(@sizeOf(vmc.StringSlice), 40);
        try t.eq(@sizeOf(RawString), 16);
        try t.eq(@sizeOf(Object), 16);
        try t.eq(@sizeOf(Box), 16);
        try t.eq(@sizeOf(NativeFunc1), 40);
        try t.eq(@sizeOf(MetaType), 16);
        if (cy.hasFFI) {
            try t.eq(@sizeOf(TccState), 24);
        }
    }

    try t.eq(@sizeOf(HeapObject), 40);
    try t.eq(@alignOf(HeapObject), 8);
    try t.eq(@sizeOf(HeapPage), 40 * 102);
    try t.eq(@alignOf(HeapPage), 8);

    var list: List = undefined;
    try t.eq(@intFromPtr(&list.list.ptr), @intFromPtr(&list) + 8);

    const rstr = RawString{
        .typeId = rt.RawstringT,
        .rc = 1,
        .len = 1,
        .bufStart = undefined,
    };
    try t.eq(@intFromPtr(&rstr.typeId), @intFromPtr(&rstr));
    try t.eq(@intFromPtr(&rstr.rc), @intFromPtr(&rstr) + 4);
    try t.eq(@intFromPtr(&rstr.len), @intFromPtr(&rstr) + 8);
    try t.eq(RawString.BufOffset, 12);
    try t.eq(@intFromPtr(&rstr.bufStart), @intFromPtr(&rstr) + RawString.BufOffset);

    const astr = Astring{
        .typeId = rt.AstringT,
        .rc = 1,
        .len = 1,
        .bufStart = undefined,
    };
    try t.eq(@intFromPtr(&astr.typeId), @intFromPtr(&astr));
    try t.eq(@intFromPtr(&astr.rc), @intFromPtr(&astr) + 4);
    try t.eq(@intFromPtr(&astr.len), @intFromPtr(&astr) + 8);
    try t.eq(Astring.BufOffset, 12);
    try t.eq(@intFromPtr(&astr.bufStart), @intFromPtr(&astr) + Astring.BufOffset);

    const ustr = Ustring{
        .typeId = rt.UstringT,
        .rc = 1,
        .len = 1,
        .charLen = 1,
        .mruIdx = 0,
        .mruCharIdx = 0,
        .bufStart = undefined,
    };
    try t.eq(@intFromPtr(&ustr.typeId), @intFromPtr(&ustr));
    try t.eq(@intFromPtr(&ustr.rc), @intFromPtr(&ustr) + 4);
    try t.eq(@intFromPtr(&ustr.len), @intFromPtr(&ustr) + 8);
    try t.eq(@intFromPtr(&ustr.charLen), @intFromPtr(&ustr) + 12);
    try t.eq(@intFromPtr(&ustr.mruIdx), @intFromPtr(&ustr) + 16);
    try t.eq(@intFromPtr(&ustr.mruCharIdx), @intFromPtr(&ustr) + 20);
    try t.eq(Ustring.BufOffset, 24);
    try t.eq(@intFromPtr(&ustr.bufStart), @intFromPtr(&ustr) + Ustring.BufOffset);

    try t.eq(@offsetOf(List, "typeId"), 0);    
    try t.eq(@offsetOf(List, "rc"), 4);    
}