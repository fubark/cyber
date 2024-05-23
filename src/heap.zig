// Copyright (c) 2023 Cyber (See LICENSE)

/// Heap objects, object allocation and deinitializers.

const cy = @import("cyber.zig");
const cc = @import("capi.zig");
const vmc = cy.vmc;
const rt = cy.rt;
const bt = cy.types.BuiltinTypes;
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
const log_mem = build_options.log_mem;

var gpa: std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = false,
    .stack_trace_frames = if (builtin.mode == .Debug) 12 else 0,
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
            if (builtin.is_test) {
                return t.alloc;
            }
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
        typeId: cy.TypeId,
    },
    head: extern struct {
        typeId: cy.TypeId,
        rc: u32,
    },
    freeSpan: extern struct {
        typeId: cy.TypeId,
        len: u32,
        start: *HeapObject,
        next: ?*HeapObject,
    },
    tuple: Tuple,
    list: List,
    listIter: ListIterator,
    table: Table,
    map: Map,
    mapIter: MapIterator,
    range: Range,

    // Functions.
    closure: Closure,
    lambda: Lambda,
    hostFunc: HostFunc,
    externFunc: ExternFunc,

    // Strings.
    string: String,
    astring: Astring,
    ustring: Ustring,
    aslice: AstringSlice,
    uslice: UstringSlice,

    // Arrays.
    array: Array,
    arraySlice: ArraySlice,

    object: Object,
    box: Box,
    tccState: if (cy.hasFFI) TccState else void,
    pointer: Pointer,
    type: Type,
    metatype: MetaType,

    pub inline fn getTypeId(self: HeapObject) cy.TypeId {
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

    pub inline fn isExternalObject(self: *HeapObject) bool {
        return (self.head.typeId & vmc.EXTERNAL_MASK) == vmc.EXTERNAL_MASK;
    }

    pub inline fn getDListNode(self: *HeapObject) *DListNode {
        if (cy.Malloc == .zig) {
            return @ptrFromInt(@intFromPtr(self) - @sizeOf(DListNode) - @sizeOf(u64));
        } else {
            return @ptrCast(@as([*]DListNode, @ptrCast(self)) - 1);
        }
    }
};

pub const Type = extern struct {
    typeId: cy.TypeId align (8),
    rc: u32,
    type: cy.TypeId,
};    

pub const MetaTypeKind = enum {
    object,
};

/// MetaType needs a RC to prevent ARC cleanup of the internal type and source code.
pub const MetaType = extern struct {
    typeId: cy.TypeId align (8),
    rc: u32,
    typeKind: u32,
    type: u32,
};

pub const Tuple = extern struct {
    typeId: cy.TypeId,
    rc: u32,
    len: u32,
    padding: u32,
    firstValue: Value,

    pub fn getElemsPtr(self: *Tuple) [*]Value {
        return @ptrCast(&self.firstValue);
    }
};

pub const Range = extern struct {
    typeId: cy.TypeId,
    rc: u32,
    has_start: bool,
    has_end: bool,
    start: i64,
    end: i64,
};

pub const ListInner = extern struct {
    ptr: [*]Value,
    cap: usize,
    len: usize,

    pub fn getList(self: *ListInner) *cy.List(Value) {
        return cy.ptrAlignCast(*cy.List(Value), self);
    }

    pub inline fn items(self: *ListInner) []Value {
        return self.ptr[0..self.len];
    }
};

pub const List = extern struct {
    typeId: cy.TypeId,
    rc: u32,
    list: ListInner,

    pub fn getList(self: *List) *cy.List(Value) {
        return cy.ptrAlignCast(*cy.List(Value), &self.list);
    }

    pub inline fn items(self: *const List) []Value {
        return self.list.ptr[0..self.list.len];
    }

    /// Assumes `val` is retained.
    pub fn append(self: *List, alloc: std.mem.Allocator, val: Value) !void {
        const list = cy.ptrAlignCast(*cy.List(Value), &self.list);
        if (list.len == list.buf.len) {
            // After reaching a certain size, use power of two ceil.
            // This reduces allocations for big lists while not over allocating for smaller lists.
            if (list.len > 512) {
                const newCap = std.math.ceilPowerOfTwo(u32, @as(u32, @intCast(list.len)) + 1) catch cy.fatal();
                try list.growTotalCapacityPrecise(alloc, newCap);
            } else {
                try list.growTotalCapacity(alloc, list.len + 1);
            }
        }
        list.appendAssumeCapacity(val);
    }
};

pub const ListIterInner = extern struct {
    list: Value,
    nextIdx: u32,
};

pub const ListIterator = extern struct {
    typeId: cy.TypeId,
    rc: u32,
    inner: ListIterInner
};

pub const Table = extern struct {
    typeId: cy.TypeId align (8),
    rc: u32,

    /// Map occupies the first slot so that custom tables can be created using the same host method bindings.
    inner_map: cy.Value,

    pub fn get(self: *Table, key: Value) ?Value {
        return self.map().get(key);
    }

    pub fn set(self: *Table, vm: *cy.VM, index: Value, val: Value) !void {
        const m = self.map();
        const res = try m.getOrPut(vm.alloc, index);
        if (res.foundExisting) {
            cy.arc.release(vm, res.valuePtr.*);
        } else {
            // No previous entry, retain key.
            cy.arc.retain(vm, index);
        }
        cy.arc.retain(vm, val);
        res.valuePtr.* = val;
    }

    pub fn map(self: *Table) *MapInner {
        return self.inner_map.asHeapObject().map.map();
    }
};

pub const MapInner = cy.ValueMap;
pub const Map = extern struct {
    typeId: cy.TypeId,
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

    /// index and val already have +1 retain.
    pub fn setConsume(self: *Map, vm: *cy.VM, index: Value, val: Value) !void {
        const m = cy.ptrAlignCast(*cy.MapInner, &self.inner);
        const res = try m.getOrPut(vm.alloc, index);
        if (res.foundExisting) {
            cy.arc.release(vm, res.valuePtr.*);
            cy.arc.release(vm, index);
        } else {
            // No previous entry, nop.
        }
        res.valuePtr.* = val;
    }

    pub fn set(self: *Map, vm: *cy.VM, index: Value, val: Value) !void {
        const m = cy.ptrAlignCast(*cy.MapInner, &self.inner);
        const res = try m.getOrPut(vm.alloc, index);
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
    typeId: cy.TypeId,
    rc: u32,
    map: *Map,
    nextIdx: u32,
};

pub const Closure = extern struct {
    typeId: cy.TypeId align (8),
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    numCaptured: u8,
    stackSize: u8,
    /// Closure value is copied to this local to provide captured var lookup.
    local: u8,
    funcSigId: u32,
    reqCallTypeCheck: bool,

    // Begins array of `Box` values.
    firstCapturedVal: Value,

    pub inline fn getCapturedValuesPtr(self: *Closure) [*]Value {
        return @ptrCast(&self.firstCapturedVal);
    }
};

const Lambda = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    stackSize: u8,
    reqCallTypeCheck: bool,
    padding: u8 = 0,
    funcSigId: u32,
};

pub const StringType = enum(u2) {
    ustring = 0,
    uslice = 1,
    astring = 2,
    aslice = 3,

    pub inline fn isUstring(self: StringType) bool {
        return self == .ustring or self == .uslice;
    }

    pub inline fn isAstring(self: StringType) bool {
        return self == .astring or self == .aslice;
    }
};

pub const String = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    headerAndLen: u32,

    pub fn getParentByType(self: *String, stype: StringType) *cy.HeapObject {
        switch (stype) {
            .ustring => return @ptrCast(self),
            .uslice => return @as(*const UstringSlice, @ptrCast(self)).parent.?,
            .aslice => return @as(*const AstringSlice, @ptrCast(self)).parent.?,
            .astring => return @ptrCast(self),
        }
    }

    pub fn getSlice(self: *const String) []const u8 {
        switch (self.getType()) {
            .ustring => return @as(*const Ustring, @ptrCast(self)).getSlice(),
            .uslice => return @as(*const UstringSlice, @ptrCast(self)).getSlice(),
            .astring => return @as(*const Astring, @ptrCast(self)).getSlice(),
            .aslice => return @as(*const AstringSlice, @ptrCast(self)).getSlice(),
        }
    }

    pub fn getType(self: *const String) StringType {
        return @enumFromInt(self.headerAndLen >> 30);
    }

    pub fn isSlice(self: *const String) bool {
        return (self.headerAndLen & 0x40000000) > 0;
    }

    pub fn len(self: *const String) u32 {
        return self.headerAndLen & 0x3fffffff;
    }
};

/// 28 byte length can fit inside a Heap pool object.
pub const MaxPoolObjectAstringByteLen = 28;

pub const Astring = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    headerAndLen: u32,
    bufStart: u8,

    const BufOffset = @offsetOf(Astring, "bufStart");

    pub inline fn getMutSlice(self: *Astring) []u8 {
        return @as([*]u8, @ptrCast(&self.bufStart))[0..@as(*const String, @ptrCast(self)).len()];
    }

    pub inline fn getSlice(self: *const Astring) []const u8 {
        return @as([*]const u8, @ptrCast(&self.bufStart))[0..@as(*const String, @ptrCast(self)).len()];
    }
};

/// 16 byte length can fit inside a Heap pool object.
pub const MaxPoolObjectUstringByteLen = 16;

const Ustring = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,

    /// Most significant bit indicates whether the string is ASCII.
    /// Next bit indicates whether the string is a slice.
    // headerAndLen: u32,

    headerAndLen: u32,
    bufStart: u8,

    const BufOffset = @offsetOf(Ustring, "bufStart");

    pub inline fn getMutSlice(self: *Ustring) []u8 {
        return @as([*]u8, @ptrCast(&self.bufStart))[0..@as(*const String, @ptrCast(self)).len()];
    }

    pub inline fn getSlice(self: *const Ustring) []const u8 {
        return @as([*]const u8, @ptrCast(&self.bufStart))[0..@as(*const String, @ptrCast(self)).len()];
    }
};

pub const AstringSlice = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    headerAndLen: u32,
    parent: ?*HeapObject,
    /// Pointer to the first byte of the slice.
    buf: [*]const u8,

    pub inline fn getSlice(self: *const AstringSlice) []const u8 {
        return self.buf[0..@as(*const String, @ptrCast(self)).len()];
    }
};

pub const UstringSlice = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    headerAndLen: u32,
    parent: ?*HeapObject,
    /// Pointer to the first byte of the slice.
    buf: [*]const u8,

    pub inline fn getSlice(self: *const UstringSlice) []const u8 {
        return self.buf[0..@as(*const String, @ptrCast(self)).len()];
    }
};

pub const MaxPoolObjectArrayByteLen = 28;

pub const Array = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    /// Most significant bit contains the slice flag.
    headerAndLen: u32,
    bufStart: u8,

    pub const BufOffset = @offsetOf(Array, "bufStart");

    pub fn getParent(self: *Array) *cy.HeapObject {
        if (self.isSlice()) {
            return @as(*const ArraySlice, @ptrCast(self)).getParentPtr().?;
        } else {
            return @ptrCast(self);
        }
    }

    pub inline fn getMutSlice(self: *Array) []u8 {
        if (self.isSlice()) {
            return @as(*ArraySlice, @ptrCast(self)).getMutSlice();
        } else {
            return @as([*]u8, @ptrCast(&self.bufStart))[0..self.len()];
        }
    }

    pub inline fn getSlice(self: *const Array) []const u8 {
        if (self.isSlice()) {
            return @as(*const ArraySlice, @ptrCast(self)).getSlice();
        } else {
            return @as([*]const u8, @ptrCast(&self.bufStart))[0..self.len()];
        }
    }

    pub inline fn isSlice(self: *const Array) bool {
        return (self.headerAndLen & 0x80000000) > 0;
    }

    pub inline fn len(self: *const Array) u32 {
        return self.headerAndLen & 0x7fffffff;
    }
};

const ArraySlice = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    headerAndLen: u32,

    /// `buf` offset from the parent Array object.
    offset: u32,
    buf: [*]const u8,

    pub inline fn getMutSlice(self: *ArraySlice) []u8 {
        return @constCast(self.buf[0..@as(*const Array, @ptrCast(self)).len()]);
    }

    pub inline fn getSlice(self: *const ArraySlice) []const u8 {
        return self.buf[0..@as(*const Array, @ptrCast(self)).len()];
    }

    pub inline fn getParentPtr(self: *const ArraySlice) ?*cy.HeapObject {
        if (self.offset != 0) {
            return @ptrFromInt(@intFromPtr(self.buf) - self.offset);
        } else return null;
    }
};

pub const Object = extern struct {
    typeId: cy.TypeId,
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

const AsyncTaskType = enum(u8) {
    /// Invoke callback on a new fiber.
    callback,

    /// Resumes a fiber.
    cont,
};

pub const AsyncTaskNode = extern struct {
    /// Tasks are linked together by a Future's continuation.
    next: ?*AsyncTaskNode,

    task: AsyncTask,
};

pub const AsyncTask = extern struct {
    type: AsyncTaskType,
    data: extern union {
        callback: Value,
        cont: *vmc.Fiber,
    },
};

pub const FutureResolver = extern struct {
    future: Value,
};

pub const Future = extern struct {
    val: Value,
    cont_head: ?*AsyncTaskNode,
    cont_tail: ?*AsyncTaskNode,
    completed: bool,

    pub fn appendCont(self: *Future, node: *AsyncTaskNode) void {
        if (self.cont_head == null) {
            self.cont_head = node;
            self.cont_tail = node;
            return;
        }

        self.cont_tail.?.next = node;
        self.cont_tail = node;
    }
};

pub fn allocFuture(vm: *cy.VM, type_id: cy.TypeId) !Value {
    const cyclable = vm.c.types[type_id].cyclable;
    var new: *Future = undefined;
    if (cyclable) {
        new = @ptrCast(try allocHostCycObject(vm, type_id, @sizeOf(Future)));
    } else {
        new = @ptrCast(try allocHostNoCycObject(vm, type_id, @sizeOf(Future)));
    }
    new.* = .{
        .val = undefined,
        .completed = false,
        .cont_head = null,
        .cont_tail = null,
    };
    if (cyclable) {
        return Value.initHostCycPtr(new);
    } else {
        return Value.initHostNoCycPtr(new);
    }
}

pub fn allocFutureResolver(vm: *cy.VM, type_id: cy.TypeId, future: Value) !Value {
    const cyclable = vm.c.types[type_id].cyclable;
    var new: *FutureResolver = undefined;
    if (cyclable) {
        new = @ptrCast(try allocHostCycObject(vm, type_id, @sizeOf(FutureResolver)));
    } else {
        new = @ptrCast(try allocHostNoCycObject(vm, type_id, @sizeOf(FutureResolver)));
    }
    new.* = .{
        .future = future,
    };
    if (cyclable) {
        return Value.initHostCycPtr(new);
    } else {
        return Value.initHostNoCycPtr(new);
    }
}

const Box = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    val: Value,
};

const HostFunc = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    func: vmc.HostFuncFn,
    numParams: u32,
    funcSigId: u32,
    tccState: Value,
    hasTccState: bool,
    reqCallTypeCheck: bool,
};

const TccState = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    state: *tcc.TCCState,
    lib: *std.DynLib,
    hasDynLib: bool,
};

pub const Pointer = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    ptr: ?*anyopaque,
};

pub const ExternFunc = extern struct {
    typeId: cy.TypeId align(8),
    rc: u32,
    ptr: ?*anyopaque,
    func: Value,
    tccState: Value,
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
    if (cy.TraceRC) {
        cy.arc.log.tracev("0 +1 alloc external object: {*}", .{slice.ptr + PayloadSize});
    }
    if (cy.TrackGlobalRC) {
        vm.c.refCounts += 1;
    }
    if (cy.Trace) {
        vm.c.trace.numRetains += 1;
        vm.c.trace.numRetainAttempts += 1;
    }
    return @ptrCast(slice.ptr + PayloadSize);
}

/// Assumes new object will have an RC = 1.
pub fn allocPoolObject(self: *cy.VM) !*HeapObject {
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
        if (cy.TraceRC) {
            cy.arc.log.tracev("0 +1 alloc pool object: {*}", .{ptr});
        }
        if (cy.TrackGlobalRC) {
            self.c.refCounts += 1;
        }
        if (cy.Trace) {
            self.c.trace.numRetains += 1;
            self.c.trace.numRetainAttempts += 1;
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
            trace.free_pc = vm.c.debugPc;
            trace.free_type = obj.getTypeId();
        } else {
            log.trace("Missing object trace {*} {}", .{obj, obj.getTypeId()});
        }
    }
    const ZigLenSize = if (cy.Malloc == .zig) @sizeOf(u64) else 0;
    const PayloadSize = (if (cy.hasGC and cyclable) @sizeOf(DListNode) else 0) + ZigLenSize;
    const slice = (@as([*]align(@alignOf(HeapObject)) u8, @ptrCast(obj)) - PayloadSize)[0..len + PayloadSize];
    vm.alloc.free(slice);
}

/// typeId should be cleared in trace mode since tracking may still hold a reference to the object.
/// The gc also needs it for now to traverse objects in pages.
pub fn freePoolObject(vm: *cy.VM, obj: *HeapObject) void {
    if (cy.Trace) {
        if (vm.objectTraceMap.getPtr(obj)) |trace| {
            trace.free_pc = vm.c.debugPc;
            trace.free_type = obj.getTypeId();
        } else {
            log.trace("Missing object trace {*} {}", .{obj, obj.getTypeId()});
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

pub fn allocType(self: *cy.VM, typeId: cy.TypeId) !Value {
    const obj = try allocPoolObject(self);
    obj.type = .{
        .typeId = bt.Type,
        .rc = 1,
        .type = typeId,
    };
    return Value.initNoCycPtr(obj);
}

pub fn allocMetaType(self: *cy.VM, typeKind: u8, typeId: cy.TypeId) !Value {
    const obj = try allocPoolObject(self);
    obj.metatype = .{
        .typeId = bt.MetaType,
        .rc = 1,
        .typeKind = typeKind,
        .type = typeId,
    };
    return Value.initNoCycPtr(obj);
}

pub fn allocEmptyListDyn(self: *cy.VM) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .typeId = bt.ListDyn | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .list = .{
            .ptr = undefined,
            .len = 0,
            .cap = 0,
        },
    };
    return Value.initCycPtr(obj);
}

pub fn allocEmptyList(self: *cy.VM, type_id: cy.TypeId) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .typeId = type_id | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .list = .{
            .ptr = undefined,
            .len = 0,
            .cap = 0,
        },
    };
    if (self.c.types[type_id].cyclable) {
        obj.list.typeId |= vmc.CYC_TYPE_MASK;
        return Value.initCycPtr(obj);
    } else {
        return Value.initNoCycPtr(obj);
    }
}

pub fn allocOwnedList(self: *cy.VM, elems: []Value) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .typeId = bt.ListDyn | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .list = .{
            .ptr = elems.ptr,
            .len = elems.len,
            .cap = elems.len,
        },
    };
    return Value.initCycPtr(obj);
}

pub fn allocListFill(self: *cy.VM, val: Value, n: u32) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .typeId = bt.ListDyn | vmc.CYC_TYPE_MASK,
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

pub fn allocHostNoCycObject(vm: *cy.VM, typeId: cy.TypeId, numBytes: usize) !*align(8) anyopaque {
    if (numBytes <= MaxPoolObjectUserBytes) {
        const obj = try allocPoolObject(vm);
        obj.head = .{
            .typeId = typeId,
            .rc = 1,
        };
        return @ptrFromInt(@intFromPtr(obj) + 8);
    } else {
        const obj = try allocExternalObject(vm, numBytes + 8, false);
        obj.head = .{
            .typeId = typeId | vmc.EXTERNAL_MASK,
            .rc = 1,
        };
        return @ptrFromInt(@intFromPtr(obj) + 8);
    }
}

pub fn allocHostCycObject(vm: *cy.VM, typeId: cy.TypeId, numBytes: usize) !*align(8) anyopaque {
    if (numBytes <= MaxPoolObjectUserBytes) {
        const obj = try allocPoolObject(vm);
        obj.head = .{
            .typeId = typeId | vmc.CYC_TYPE_MASK,
            .rc = 1,
        };
        return @ptrFromInt(@intFromPtr(obj) + 8);
    } else {
        const obj = try allocExternalObject(vm, numBytes + 8, true);
        obj.head = .{
            .typeId = typeId | vmc.CYC_TYPE_MASK | vmc.EXTERNAL_MASK,
            .rc = 1,
        };
        return @ptrFromInt(@intFromPtr(obj) + 8);
    }
}

pub fn allocTuple(vm: *cy.VM, elems: []const Value) !Value {
    var obj: *HeapObject = undefined;
    if (elems.len <= 3) {
        obj = try allocPoolObject(vm);
    } else {
        obj = try allocExternalObject(vm, elems.len * 8 + 16, true);
    }
    obj.tuple = .{
        .typeId = bt.Tuple | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .len = @intCast(elems.len),
        .padding = undefined,
        .firstValue = undefined,
    };
    const dst = obj.tuple.getElemsPtr()[0..elems.len];
    @memcpy(dst, elems);
    return Value.initCycPtr(obj);
}

pub fn allocList(self: *cy.VM, type_id: cy.TypeId, elems: []const Value) !Value {
    const cyclable = self.c.types[type_id].cyclable;
    const obj = try allocPoolObject(self);
    const cyc_mask = if (cyclable) vmc.CYC_TYPE_MASK else 0;
    obj.list = .{
        .typeId = type_id | cyc_mask,
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
    @memcpy(list.items(), elems);
    if (cyclable) {
        return Value.initCycPtr(obj);
    } else {
        return Value.initNoCycPtr(obj);
    }
}

pub fn allocListDyn(self: *cy.VM, elems: []const Value) !Value {
    const obj = try allocPoolObject(self);
    obj.list = .{
        .typeId = bt.ListDyn | vmc.CYC_TYPE_MASK,
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
    @memcpy(list.items(), elems);
    return Value.initCycPtr(obj);
}

pub fn allocListIter(self: *cy.VM, type_id: cy.TypeId, list: Value) !Value {
    const cyclable = self.c.types[type_id].cyclable;
    const obj = try allocPoolObject(self);
    const cyc_mask = if (cyclable) vmc.CYC_TYPE_MASK else 0;
    obj.listIter = .{
        .typeId = type_id | cyc_mask,
        .rc = 1,
        .inner = .{
            .list = list,
            .nextIdx = 0,
        },
    };
    if (cyclable) {
        return Value.initCycPtr(obj);
    } else {
        return Value.initNoCycPtr(obj);
    }
}

/// Assumes list is already retained for the iterator.
pub fn allocListIterDyn(self: *cy.VM, list: Value) !Value {
    const obj = try allocPoolObject(self);
    obj.listIter = .{
        .typeId = bt.ListIterDyn | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .inner = .{
            .list = list,
            .nextIdx = 0,
        },
    };
    return Value.initCycPtr(obj);
}

pub fn allocTable(self: *cy.VM) !Value {
    const obj = try allocPoolObject(self);
    obj.table = .{
        .typeId = bt.Table | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .inner_map = try self.allocEmptyMap(),
    };
    return Value.initCycPtr(obj);
}

pub fn allocEmptyMap(self: *cy.VM) !Value {
    const obj = try allocPoolObject(self);
    obj.map = .{
        .typeId = bt.Map | vmc.CYC_TYPE_MASK,
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
        .typeId = bt.Map | vmc.CYC_TYPE_MASK,
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
        cy.arc.retain(self, keyVal);
        const res = try inner.getOrPut(self.alloc, keyVal);
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
pub fn allocMapIterator(self: *cy.VM, map: *Map) !Value {
    const obj = try allocPoolObject(self);
    obj.mapIter = .{
        .typeId = bt.MapIter | vmc.CYC_TYPE_MASK,
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
        obj = try allocExternalObject(self, (3 + capturedVals.len) * @sizeOf(Value));
    }
    obj.closure = .{
        .typeId = bt.Closure | vmc.CYC_TYPE_MASK,
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

pub fn allocLambda(self: *cy.VM, funcPc: usize, numParams: u8, stackSize: u8, funcSigId: u16, req_type_check: bool) !Value {
    const obj = try allocPoolObject(self);
    obj.lambda = .{
        .typeId = bt.Lambda,
        .rc = 1,
        .funcPc = @intCast(funcPc),
        .numParams = numParams,
        .stackSize = stackSize,
        .funcSigId = funcSigId,
        .reqCallTypeCheck = req_type_check,
    };
    return Value.initNoCycPtr(obj);
}

pub fn allocStringTemplate(self: *cy.VM, strs: []const cy.Inst, vals: []const Value) !Value {
    const firstStr = Value.initRaw(self.c.consts[strs[0].val].val).asHeapObject();
    const firstSlice = firstStr.string.getSlice();
    try self.u8Buf.resize(self.alloc, firstSlice.len);
    @memcpy(self.u8Buf.items(), firstSlice);

    const writer = self.u8Buf.writer(self.alloc);
    var is_ascii = true;
    for (vals, 0..) |val, i| {
        var out_ascii: bool = undefined;
        const valstr = try self.getOrBufPrintValueStr2(&cy.tempBuf, val, &out_ascii);
        is_ascii = is_ascii and out_ascii;
        _ = try writer.write(valstr);

        const str = Value.initRaw(self.c.consts[strs[i+1].val].val).asHeapObject();
        is_ascii = is_ascii and str.string.getType().isAstring();
        try self.u8Buf.appendSlice(self.alloc, str.string.getSlice());
    }

    if (is_ascii) {
        return retainOrAllocUstring(self, self.u8Buf.items());
    } else {
        return retainOrAllocAstring(self, self.u8Buf.items());
    }
}

pub fn allocStringTemplate2(self: *cy.VM, strs: []const Value, vals: []const Value) !Value {
    const firstStr = strs[0].asHeapObject();
    const firstSlice = firstStr.string.getSlice();
    try self.u8Buf.resize(self.alloc, firstSlice.len);
    @memcpy(self.u8Buf.items(), firstSlice);

    const writer = self.u8Buf.writer(self.alloc);
    var is_ascii = true;
    for (vals, 0..) |val, i| {
        var out_ascii: bool = undefined;
        const valstr = try self.getOrBufPrintValueStr2(&cy.tempBuf, val, &out_ascii);
        is_ascii = is_ascii and out_ascii;
        _ = try writer.write(valstr);

        const str = strs[i+1].asHeapObject();
        is_ascii = is_ascii and str.string.getType().isAstring();

        try self.u8Buf.appendSlice(self.alloc, str.string.getSlice());
    }

    if (is_ascii) {
        return retainOrAllocUstring(self, self.u8Buf.items());
    } else {
        return retainOrAllocAstring(self, self.u8Buf.items());
    }
}

pub fn getOrAllocOwnedAstring(self: *cy.VM, obj: *HeapObject) !Value {
    return getOrAllocOwnedString(self, obj, obj.astring.getSlice());
}

pub fn getOrAllocOwnedUstring(self: *cy.VM, obj: *HeapObject) !Value {
    return getOrAllocOwnedString(self, obj, obj.ustring.getSlice());
}

pub fn allocArrayConcat(self: *cy.VM, slice: []const u8, other: []const u8) !Value {
    const len: u32 = @intCast(slice.len + other.len);
    var obj: *HeapObject = undefined;
    if (len <= MaxPoolObjectArrayByteLen) {
        obj = try allocPoolObject(self);
    } else {
        obj = try allocExternalObject(self, len + Array.BufOffset, false);
    }
    obj.array = .{
        .typeId = bt.Array,
        .rc = 1,
        .headerAndLen = len,
        .bufStart = undefined,
    };
    const dst = @as([*]u8, @ptrCast(&obj.array.bufStart))[0..len];
    @memcpy(dst[0..slice.len], slice);
    @memcpy(dst[slice.len..], other);
    return Value.initNoCycPtr(obj);
}

fn allocUstringConcat3Object(self: *cy.VM, str1: []const u8, str2: []const u8, str3: []const u8) !*HeapObject {
    const obj = try allocUnsetUstringObject(self, str1.len + str2.len + str3.len);
    const dst = obj.ustring.getMutSlice();
    @memcpy(dst[0..str1.len], str1);
    @memcpy(dst[str1.len..str1.len+str2.len], str2);
    @memcpy(dst[str1.len+str2.len..], str3);
    return obj;
}

fn allocUstringConcatObject(self: *cy.VM, str1: []const u8, str2: []const u8) !*HeapObject {
    const obj = try allocUnsetUstringObject(self, str1.len + str2.len);
    const dst = obj.ustring.getMutSlice();
    @memcpy(dst[0..str1.len], str1);
    @memcpy(dst[str1.len..], str2);
    return obj;
}
    
fn allocAstringObject(self: *cy.VM, str: []const u8) !*HeapObject {
    const obj = try allocUnsetAstringObject(self, str.len);
    const dst = obj.astring.getMutSlice();
    @memcpy(dst, str);
    return obj;
}

fn allocAstringConcat3Object(self: *cy.VM, str1: []const u8, str2: []const u8, str3: []const u8) !*HeapObject {
    const obj = try allocUnsetAstringObject(self, str1.len + str2.len + str3.len);
    const dst = obj.astring.getMutSlice();
    @memcpy(dst[0..str1.len], str1);
    @memcpy(dst[str1.len..str1.len+str2.len], str2);
    @memcpy(dst[str1.len+str2.len..], str3);
    return obj;
}

fn allocAstringConcatObject(self: *cy.VM, str1: []const u8, str2: []const u8) !*HeapObject {
    const obj = try allocUnsetAstringObject(self, str1.len + str2.len);
    const dst = obj.astring.getMutSlice();
    @memcpy(dst[0..str1.len], str1);
    @memcpy(dst[str1.len..], str2);
    return obj;
}

pub fn getOrAllocAstringConcat(self: *cy.VM, str: []const u8, str2: []const u8) !Value {
    if (str.len + str2.len <= DefaultStringInternMaxByteLen) {
        const concat = self.tempBuf[0..str.len+str2.len];
        @memcpy(concat[0..str.len], str);
        @memcpy(concat[str.len..], str2);

        const res = try self.strInterns.getOrPut(self.alloc, concat);
        if (res.found_existing) {
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initNoCycPtr(res.value_ptr.*);
        } else {
            const obj = try allocAstringObject(self, concat);
            res.key_ptr.* = obj.astring.getSlice();
            res.value_ptr.* = obj;
            return Value.initNoCycPtr(obj);
        }
    } else {
        const obj = try allocAstringConcatObject(self, str, str2);
        return Value.initNoCycPtr(obj);
    }
}

pub fn getOrAllocAstringConcat3(self: *cy.VM, str1: []const u8, str2: []const u8, str3: []const u8) !Value {
    if (str1.len + str2.len + str3.len <= DefaultStringInternMaxByteLen) {
        const concat = self.tempBuf[0..str1.len+str2.len+str3.len];
        @memcpy(concat[0..str1.len], str1);
        @memcpy(concat[str1.len..str1.len+str2.len], str2);
        @memcpy(concat[str1.len+str2.len..], str3);

        const res = try self.strInterns.getOrPut(self.alloc, concat);
        if (res.found_existing) {
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initNoCycPtr(res.value_ptr.*);
        } else {
            const obj = try allocAstringObject(self, concat);
            res.key_ptr.* = obj.astring.getSlice();
            res.value_ptr.* = obj;
            return Value.initNoCycPtr(obj);
        }
    } else {
        const obj = try allocAstringConcat3Object(self, str1, str2, str3);
        return Value.initNoCycPtr(obj);
    }
}

pub fn getOrAllocUstringConcat3(self: *cy.VM, str1: []const u8, str2: []const u8, str3: []const u8) !Value {
    if (str1.len + str2.len + str3.len <= DefaultStringInternMaxByteLen) {
        const concat = self.tempBuf[0..str1.len+str2.len+str3.len];
        @memcpy(concat[0..str1.len], str1);
        @memcpy(concat[str1.len..str1.len+str2.len], str2);
        @memcpy(concat[str1.len+str2.len..], str3);

        const res = try self.strInterns.getOrPut(self.alloc, concat);
        if (res.found_existing) {
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initNoCycPtr(res.value_ptr.*);
        } else {
            const obj = try allocUstringObject(self, concat);
            res.key_ptr.* = obj.ustring.getSlice();
            res.value_ptr.* = obj;
            return Value.initNoCycPtr(obj);
        }
    } else {
        const obj = try allocUstringConcat3Object(self, str1, str2, str3);
        return Value.initNoCycPtr(obj);
    }
}

pub fn getOrAllocUstringConcat(self: *cy.VM, str: []const u8, str2: []const u8) !Value {
    if (str.len + str2.len <= DefaultStringInternMaxByteLen) {
        const concat = self.tempBuf[0..str.len+str2.len];
        @memcpy(concat[0..str.len], str);
        @memcpy(concat[str.len..], str2);

        const res = try self.strInterns.getOrPut(self.alloc, concat);
        if (res.found_existing) {
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initNoCycPtr(res.value_ptr.*);
        } else {
            const obj = try allocUstringObject(self, concat);
            res.key_ptr.* = obj.ustring.getSlice();
            res.value_ptr.* = obj;
            return Value.initNoCycPtr(obj);
        }
    } else {
        const obj = try allocUstringConcatObject(self, str, str2);
        return Value.initNoCycPtr(obj);
    }
}

const DefaultStringInternMaxByteLen = 64;

// If no such string intern exists, `obj` is added as a string intern.
// Otherwise, `obj` is released and the existing string intern is retained and returned.
pub fn getOrAllocOwnedString(self: *cy.VM, obj: *HeapObject, str: []const u8) !Value {
    if (str.len <= DefaultStringInternMaxByteLen) {
        const res = try self.strInterns.getOrPut(self.alloc, str);
        if (res.found_existing) {
            cy.arc.releaseObject(self, obj);
            cy.arc.retainObject(self, res.value_ptr.*);
            return Value.initNoCycPtr(res.value_ptr.*);
        } else {
            res.key_ptr.* = str;
            res.value_ptr.* = obj;
            return Value.initNoCycPtr(obj);
        }
    } else {
        return Value.initNoCycPtr(obj);
    }
}

const Root = @This();
pub const VmExt = struct {
    pub const allocString = Root.allocString;
    pub const retainOrAllocAstring = Root.retainOrAllocAstring;
    pub const retainOrAllocUstring = Root.retainOrAllocUstring;
    pub const allocAstringConcat = Root.getOrAllocAstringConcat;
    pub const allocUstringConcat = Root.getOrAllocUstringConcat;
    pub const allocAstringConcat3 = Root.getOrAllocAstringConcat3;
    pub const allocUstringConcat3 = Root.getOrAllocUstringConcat3;
    pub const allocOwnedAstring = Root.getOrAllocOwnedAstring;
    pub const allocOwnedUstring = Root.getOrAllocOwnedUstring;
    pub const allocAstringSlice = Root.allocAstringSlice;
    pub const allocUstringSlice = Root.allocUstringSlice;
    pub const allocArraySlice = Root.allocArraySlice;
    pub const allocArrayConcat = Root.allocArrayConcat;
    pub const allocHostFunc = Root.allocHostFunc;
    pub const allocTable = Root.allocTable;
    pub const allocEmptyMap = Root.allocEmptyMap;
    pub const allocEmptyListDyn = Root.allocEmptyListDyn;
    pub const allocArray = Root.allocArray;
    pub const allocPointer = Root.allocPointer;
    pub const allocUnsetArrayObject = Root.allocUnsetArrayObject;
    pub const allocUnsetAstringObject = Root.allocUnsetAstringObject;
    pub const allocUnsetUstringObject = Root.allocUnsetUstringObject;
    pub const allocListFill = Root.allocListFill;
    pub const allocListIter = Root.allocListIter;
    pub const allocListIterDyn = Root.allocListIterDyn;
    pub const allocMapIterator = Root.allocMapIterator;
    pub const allocObjectSmall = Root.allocObjectSmall;
    pub const allocType = Root.allocType;
    pub const allocFuture = Root.allocFuture;
    pub const allocFutureResolver = Root.allocFutureResolver;

    pub fn mapSet(vm: *cy.VM, map: *Map, key: Value, val: Value) !void {
        try map.map().put(vm.alloc, key, val);
    }
};

pub fn allocString(self: *cy.VM, str: []const u8) !Value {
    if (cy.string.isAstring(str)) {
        return self.retainOrAllocAstring(str);
    } else {
        return self.retainOrAllocUstring(str);
    }
}

pub fn allocAstring(self: *cy.VM, str: []const u8) !Value {
    const obj = try allocUnsetAstringObject(self, str.len);
    const dst = obj.astring.getSlice();
    @memcpy(dst, str);
    return Value.initNoCycPtr(obj);
}

pub fn allocUnsetAstringObject(self: *cy.VM, len: usize) !*HeapObject {
    var obj: *HeapObject = undefined;
    if (len <= MaxPoolObjectAstringByteLen) {
        obj = try allocPoolObject(self);
    } else {
        obj = try allocExternalObject(self, len + Astring.BufOffset, false);
    }
    obj.astring = .{
        .typeId = bt.String,
        .rc = 1,
        .headerAndLen = (@as(u32, @intFromEnum(StringType.astring)) << 30) | @as(u32, @intCast(len)),
        .bufStart = undefined,
    };
    return obj;
}

pub fn retainOrAllocUstring(self: *cy.VM, str: []const u8) !Value {
    var allocated: bool = undefined;
    const res = try getOrAllocUstring(self, str, &allocated);
    if (!allocated) {
        cy.arc.retain(self, res);
    }
    return res;
}

pub fn getOrAllocUstring(self: *cy.VM, str: []const u8, outAllocated: *bool) !Value {
    if (str.len <= DefaultStringInternMaxByteLen) {
        const res = try self.strInterns.getOrPut(self.alloc, str);
        if (res.found_existing) {
            outAllocated.* = false;
            return Value.initNoCycPtr(res.value_ptr.*);
        } else {
            outAllocated.* = true;
            const obj = try allocUstringObject(self, str);
            res.key_ptr.* = obj.ustring.getSlice();
            res.value_ptr.* = obj;
            return Value.initNoCycPtr(obj);
        }
    } else {
        outAllocated.* = true;
        const obj = try allocUstringObject(self, str);
        return Value.initNoCycPtr(obj);
    }
}

pub fn retainOrAllocAstring(self: *cy.VM, str: []const u8) !Value {
    var allocated: bool = undefined;
    const res = try getOrAllocAstring(self, str, &allocated);
    if (!allocated) {
        cy.arc.retain(self, res);
    }
    return res;
}

pub fn getOrAllocAstring(self: *cy.VM, str: []const u8, outAllocated: *bool) !Value {
    if (str.len <= DefaultStringInternMaxByteLen) {
        const res = try self.strInterns.getOrPut(self.alloc, str);
        if (res.found_existing) {
            outAllocated.* = false;
            return Value.initNoCycPtr(res.value_ptr.*);
        } else {
            outAllocated.* = true;
            const obj = try allocAstringObject(self, str);
            res.key_ptr.* = obj.astring.getSlice();
            res.value_ptr.* = obj;
            return Value.initNoCycPtr(obj);
        }
    } else {
        outAllocated.* = true;
        const obj = try allocAstringObject(self, str);
        return Value.initNoCycPtr(obj);
    }
}

pub fn allocUstring(self: *cy.VM, str: []const u8, charLen: u32) !Value {
    const obj = try allocUstringObject(self, str, charLen);
    return Value.initNoCycPtr(obj);
}

pub fn allocUstringObject(self: *cy.VM, str: []const u8) !*HeapObject {
    const obj = try allocUnsetUstringObject(self, str.len);
    const dst = obj.ustring.getMutSlice();
    @memcpy(dst, str);
    return obj;
}

pub fn allocUnsetUstringObject(self: *cy.VM, len: usize) !*HeapObject {
    var obj: *HeapObject = undefined;
    if (len <= MaxPoolObjectUstringByteLen) {
        obj = try allocPoolObject(self);
    } else {
        obj = try allocExternalObject(self, len + Ustring.BufOffset, false);
    }
    obj.ustring = .{
        .typeId = bt.String,
        .rc = 1,
        .headerAndLen = (@as(u32, @intFromEnum(StringType.ustring)) << 30) | @as(u32, @intCast(len)),
        .bufStart = undefined,
    };
    return obj;
}

pub fn allocUstringSlice(self: *cy.VM, slice: []const u8, parent: ?*HeapObject) !Value {
    const obj = try allocPoolObject(self);
    obj.uslice = .{
        .typeId = bt.String,
        .rc = 1,
        .buf = slice.ptr,
        .headerAndLen = (@as(u32, @intFromEnum(StringType.uslice)) << 30) | @as(u32, @intCast(slice.len)),
        .parent = parent,
    };
    return Value.initNoCycPtr(obj);
}

pub fn allocAstringSlice(self: *cy.VM, slice: []const u8, parent: *HeapObject) !Value {
    const obj = try allocPoolObject(self);
    obj.aslice = .{
        .typeId = bt.String,
        .rc = 1,
        .headerAndLen = (@as(u32, @intFromEnum(StringType.aslice)) << 30) | @as(u32, @intCast(slice.len)),
        .buf = slice.ptr,
        .parent = parent,
    };
    return Value.initNoCycPtr(obj);
}

pub fn allocUnsetArrayObject(self: *cy.VM, len: usize) !*HeapObject {
    var obj: *HeapObject = undefined;
    if (len <= MaxPoolObjectArrayByteLen) {
        obj = try allocPoolObject(self);
    } else {
        obj = try allocExternalObject(self, len + Array.BufOffset, false);
    }
    obj.array = .{
        .typeId = bt.Array,
        .rc = 1,
        .headerAndLen = @intCast(len),
        .bufStart = undefined,
    };
    return obj;
}

pub fn allocArray(vm: *cy.VM, buf: []const u8) !Value {
    const obj = try allocUnsetArrayObject(vm, buf.len);
    const dst = @as([*]u8, @ptrCast(&obj.array.bufStart))[0..buf.len];
    @memcpy(dst, buf);
    return Value.initNoCycPtr(obj);
}

pub fn allocArraySlice(self: *cy.VM, slice: []const u8, parent: *HeapObject) !Value {
    const obj = try allocPoolObject(self);
    obj.arraySlice = .{
        .typeId = bt.Array,
        .rc = 1,
        .buf = slice.ptr,
        .headerAndLen = 0x80000000 | @as(u32, @intCast(slice.len)),
        .offset = @intCast(@intFromPtr(slice.ptr) - @intFromPtr(parent)),
    };
    return Value.initNoCycPtr(obj);
}

pub fn allocBox(vm: *cy.VM, val: Value) !Value {
    const obj = try allocPoolObject(vm);
    obj.box = .{
        .typeId = bt.Box | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .val = val,
    };
    return Value.initCycPtr(obj);
}

pub fn allocExternFunc(self: *cy.VM, cyFunc: Value, funcPtr: *anyopaque, tccState: Value) !Value {
    const obj = try allocPoolObject(self);
    obj.externFunc = .{
        .typeId = bt.ExternFunc,
        .rc = 1,
        .func = cyFunc,
        .ptr = funcPtr,
        .tccState = tccState,
    };
    const cyclable = cyFunc.isCycPointer();
    if (cyclable) {
        return Value.initCycPtr(obj);
    } else {
        return Value.initNoCycPtr(obj);
    }
}

pub fn allocHostFunc(self: *cy.VM, func: cy.ZHostFuncFn, numParams: u32, funcSigId: cy.sema.FuncSigId, tccState: ?Value, reqCallTypeCheck: bool) !Value {
    const obj = try allocPoolObject(self);
    obj.hostFunc = .{
        .typeId = bt.HostFunc,
        .rc = 1,
        .func = @ptrCast(func),
        .numParams = numParams,
        .funcSigId = funcSigId,
        .tccState = undefined,
        .hasTccState = false,
        .reqCallTypeCheck = reqCallTypeCheck,
    };
    if (tccState) |state| {
        obj.hostFunc.tccState = state;
        obj.hostFunc.hasTccState = true;
    }
    return Value.initNoCycPtr(obj);
}

pub fn allocTccState(self: *cy.VM, state: *tcc.TCCState, optLib: ?*std.DynLib) !Value {
    const obj = try allocPoolObject(self);
    obj.tccState = .{
        .typeId = bt.TccState,
        .rc = 1,
        .state = state,
        .lib = undefined,
        .hasDynLib = false,
    };
    if (optLib) |lib| {
        obj.tccState.lib = lib;
        obj.tccState.hasDynLib = true;
    }
    return Value.initNoCycPtr(obj);
}

pub fn allocPointer(self: *cy.VM, ptr: ?*anyopaque) !Value {
    const obj = try allocPoolObject(self);
    obj.pointer = .{
        .typeId = bt.Pointer,
        .rc = 1,
        .ptr = ptr,
    };
    return Value.initNoCycPtr(obj);
}

/// Allocates an object outside of the object pool.
pub fn allocObject(self: *cy.VM, type_id: cy.TypeId, fields: []const Value) !Value {
    // First slot holds the typeId and rc.
    const obj: *Object = @ptrCast(try allocExternalObject(self, (1 + fields.len) * @sizeOf(Value), true));
    obj.* = .{
        .typeId = type_id | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .firstValue = undefined,
    };
    const dst = obj.getValuesPtr();
    @memcpy(dst[0..fields.len], fields);

    return Value.initCycPtr(obj);
}

pub fn allocEmptyObject(self: *cy.VM, type_id: cy.TypeId, nfields: u32) !Value {
    // First slot holds the typeId and rc.
    const obj: *Object = @ptrCast(try allocExternalObject(self, (1 + nfields) * @sizeOf(Value), true));
    obj.* = .{
        .typeId = type_id | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .firstValue = undefined,
    };
    return Value.initCycPtr(obj);
}

pub fn allocObjectSmall(self: *cy.VM, type_id: cy.TypeId, fields: []const Value) !Value {
    const obj = try allocPoolObject(self);
    obj.object = .{
        .typeId = type_id | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .firstValue = undefined,
    };

    const dst = obj.object.getValuesPtr();
    @memcpy(dst[0..fields.len], fields);

    return Value.initCycPtr(obj);
}

pub fn allocEmptyObjectSmall(self: *cy.VM, type_id: cy.TypeId) !Value {
    const obj = try allocPoolObject(self);
    obj.object = .{
        .typeId = type_id | vmc.CYC_TYPE_MASK,
        .rc = 1,
        .firstValue = undefined,
    };
    return Value.initCycPtr(obj);
}

pub fn allocFuncFromSym(self: *cy.VM, func: rt.FuncSymbol) !Value {
    switch (func.type) {
        .host_func => {
            return allocHostFunc(self, @ptrCast(func.data.host_func), func.nparams, func.sig, null, func.req_type_check);
        },
        .func => {
            return allocLambda(self, func.data.func.pc,
                @intCast(func.nparams),
                @intCast(func.data.func.stackSize),
                @intCast(func.sig),
                func.req_type_check
            );
        },
        .null => return error.Unexpected,
    }
}

/// Use comptime options to keep closely related logic together.
/// TODO: flatten recursion.
pub fn freeObject(vm: *cy.VM, obj: *HeapObject,
    comptime releaseChildren: bool, comptime skipCycChildren: bool, comptime free: bool,
) void {
    if (cy.Trace) {
        if (obj.isFreed()) {
            cy.panicFmt("Double free object: {*} Should have been discovered in release op.", .{obj});
        } else {
            const desc = vm.bufPrintValueShortStr(&cy.tempBuf, Value.initPtr(obj)) catch cy.fatal();
            log.tracevIf(log_mem, "free type={}({s}) {*}: `{s}`", .{
                obj.getTypeId(), vm.getTypeName(obj.getTypeId()), obj, desc,
            });
        }
    }
    const typeId = obj.getTypeId();
    switch (typeId) {
        bt.Tuple => {
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
        bt.Range => {
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        bt.Map => {
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
        bt.MapIter => {
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
        bt.Closure => {
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
                if (obj.closure.numCaptured <= 2) {
                    freePoolObject(vm, obj);
                } else {
                    freeExternalObject(vm, obj, (3 + obj.closure.numCaptured) * @sizeOf(Value), true);
                }
            }
        },
        bt.Lambda => {
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        bt.String => {
            switch (obj.string.getType()) {
                .astring => {
                    if (free) {
                        const len = obj.string.len();
                        if (len <= DefaultStringInternMaxByteLen) {

                            // Check both the key and value to make sure this object is the intern entry.
                            // TODO: Use a flag bit instead of a map query.
                            const key = obj.astring.getSlice();
                            if (vm.strInterns.get(key)) |val| {
                                if (val == obj) {
                                    _ = vm.strInterns.remove(key);
                                }
                            }
                        }
                        if (len <= MaxPoolObjectAstringByteLen) {
                            freePoolObject(vm, obj);
                        } else {
                            freeExternalObject(vm, obj, Astring.BufOffset + len, false);
                        }
                    }
                },
                .ustring => {
                    if (free) {
                        const len = obj.string.len();
                        if (len <= DefaultStringInternMaxByteLen) {
                            const key = obj.ustring.getSlice();
                            if (vm.strInterns.get(key)) |val| {
                                if (val == obj) {
                                    _ = vm.strInterns.remove(key);
                                }
                            }
                        }
                        if (len <= MaxPoolObjectUstringByteLen) {
                            freePoolObject(vm, obj);
                        } else {
                            freeExternalObject(vm, obj, Ustring.BufOffset + len, false);
                        }
                    }
                },
                .aslice => {
                    if (releaseChildren) {
                        if (obj.aslice.parent) |parent| {
                            cy.arc.releaseObject(vm, parent);
                        }
                    }
                    if (free) {
                        freePoolObject(vm, obj);
                    }
                },
                .uslice => {
                    if (releaseChildren) {
                        if (obj.uslice.parent) |parent| {
                            cy.arc.releaseObject(vm, parent);
                        }
                    }
                    if (free) {
                        freePoolObject(vm, obj);
                    }
                },
            }
        },
        bt.Array => {
            if (obj.array.isSlice()) {
                if (releaseChildren) {
                    if (obj.arraySlice.getParentPtr()) |parent| {
                        cy.arc.releaseObject(vm, parent);
                    }
                }
                if (free) {
                    freePoolObject(vm, obj);
                }
            } else {
                if (free) {
                    const len = obj.array.len();
                    if (len <= MaxPoolObjectArrayByteLen) {
                        freePoolObject(vm, obj);
                    } else {
                        freeExternalObject(vm, obj, Array.BufOffset + len, false);
                    }
                }
            }
        },
        bt.Fiber => {
            if (releaseChildren) {
                const fiber: *vmc.Fiber = @ptrCast(obj);
                // TODO: isCyc.
                cy.fiber.releaseFiberStack(vm, fiber) catch |err| {
                    cy.panicFmt("release fiber: {}", .{err});
                };
            }
            if (free) {
                cy.fiber.freeFiberPanic(vm, @ptrCast(obj));
                freeExternalObject(vm, obj, @sizeOf(vmc.Fiber), true);
            }
        },
        bt.Box => {
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
        bt.ExternFunc => {
            if (releaseChildren) {
                cy.arc.releaseObject(vm, obj.externFunc.tccState.asHeapObject());
                cy.arc.releaseObject(vm, obj.externFunc.func.asHeapObject());
            }
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        bt.HostFunc => {
            if (releaseChildren) {
                if (obj.hostFunc.hasTccState) {
                    cy.arc.releaseObject(vm, obj.hostFunc.tccState.asHeapObject());
                }
            }
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        bt.TccState => {
            if (cy.hasFFI) {
                if (free) {
                    tcc.tcc_delete(obj.tccState.state);
                    if (obj.tccState.hasDynLib) {
                        obj.tccState.lib.close();
                        vm.alloc.destroy(obj.tccState.lib);
                    }
                    freePoolObject(vm, obj);
                }
            } else {
                unreachable;
            }
        },
        bt.Pointer => {
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        bt.Type => {
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        bt.MetaType => {
            if (free) {
                freePoolObject(vm, obj);
            }
        },
        else => {
            if (cy.Trace) {
                log.tracev("free {s} {}", .{vm.getTypeName(typeId), typeId});

                // Check range.
                if (typeId >= vm.c.types_len) {
                    log.tracev("unsupported struct type {}", .{typeId});
                    cy.fatal();
                }
            }
            // TODO: Determine isHostObject from object to avoid extra read from `rt.Type`
            // TODO: Use a dispatch table for host objects only.
            const entry = vm.c.types[typeId];
            switch (entry.kind) {
                .option => {
                    if (releaseChildren) {
                        const child = obj.object.getValuesConstPtr()[1];
                        if (!skipCycChildren or !child.isGcConfirmedCyc()) {
                            cy.arc.release(vm, child);
                        }
                    }
                    if (free) {
                        freePoolObject(vm, obj);
                    }
                },
                .@"struct" => {
                    const numFields = entry.data.@"struct".numFields;
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
                },
                .object => {
                    const numFields = entry.data.object.numFields;
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
                },
                .choice => {
                    if (releaseChildren) {
                        const value = obj.object.getValuesConstPtr()[1];
                        if (skipCycChildren and value.isGcConfirmedCyc()) {
                            // nop.
                        } else {
                            cy.arc.release(vm, value);
                        }
                    }
                    if (free) {
                        freePoolObject(vm, obj);
                    }
                },
                .custom => {
                    if (releaseChildren) {
                        if (entry.data.custom.getChildrenFn) |getChildren| {
                            const children = getChildren(@ptrCast(vm), @ptrFromInt(@intFromPtr(obj) + 8));
                            for (Value.fromSliceC(children)) |child| {
                                if (skipCycChildren and child.isGcConfirmedCyc()) {
                                    continue;
                                }
                                cy.arc.release(vm, child);
                            }
                        }
                    }
                    if (free) {
                        if (entry.data.custom.finalizerFn) |finalizer| {
                            finalizer(@ptrCast(vm), @ptrFromInt(@intFromPtr(obj) + 8));
                        }
                        if (!obj.isExternalObject()) {
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
                },
                else => {},
            }
        },
    }
}

// User bytes excludes the type header.
pub const MaxPoolObjectUserBytes = @sizeOf(HeapObject) - 8;

pub fn traceAlloc(vm: *cy.VM, ptr: *HeapObject) void {
    // log.tracev("alloc {*} {} {}", .{ptr, ptr.getTypeId(), vm.c.debugPc});
    vm.objectTraceMap.put(vm.alloc, ptr, .{
        .alloc_pc = vm.c.debugPc,
        .free_pc = null,
        .free_type = null,
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
        vm.c.debugPc = 123;
        freeExternalObject(&vm, obj, 40, false);
        try t.eq(cy.arc.isObjectAlreadyFreed(&vm, obj), true);
    }
}

test "heap internals." {
    try t.eq(@sizeOf(AsyncTask), 16);
    if (cy.is32Bit) {
        try t.eq(@sizeOf(MapInner), 20);
        try t.eq(@alignOf(List), 4);
        try t.eq(@alignOf(ListIterator), 8);
        try t.eq(@sizeOf(List), 20);
        try t.eq(@sizeOf(ListIterator), 24);
        try t.eq(@sizeOf(Map), 32);
        try t.eq(@sizeOf(MapIterator), 16);
        try t.eq(@sizeOf(Pointer), 16);
    } else {
        try t.eq(@sizeOf(MapInner), 32);
        try t.eq(@alignOf(List), 8);
        try t.eq(@alignOf(ListIterator), 8);

        if (builtin.os.tag != .windows) {
            try t.eq(@sizeOf(List), 32);
            try t.eq(@sizeOf(ListIterator), 24);
            try t.eq(@sizeOf(Map), 40);
            try t.eq(@sizeOf(MapIterator), 24);
            try t.eq(@sizeOf(ArraySlice), 24);
            try t.eq(@sizeOf(Pointer), 16);
        }
    }

    if (builtin.os.tag != .windows) { 
        try t.eq(@sizeOf(Closure), 32);
        try t.eq(@sizeOf(Lambda), 24);
        try t.eq(@sizeOf(Astring), 16);
        try t.eq(@sizeOf(Ustring), 16);
        if (cy.is32Bit) {
            try t.eq(@sizeOf(AstringSlice), 24);
            try t.eq(@sizeOf(UstringSlice), 24);
        } else {
            try t.eq(@sizeOf(AstringSlice), 32);
            try t.eq(@sizeOf(UstringSlice), 32);
        }
        try t.eq(@sizeOf(Array), 16);
        try t.eq(@sizeOf(ArraySlice), 24);
        try t.eq(@sizeOf(Object), 16);
        try t.eq(@sizeOf(Box), 16);
        try t.eq(@sizeOf(HostFunc), 40);
        try t.eq(@sizeOf(MetaType), 16);
        if (cy.hasFFI) {
            try t.eq(@sizeOf(TccState), 32);
        }
    }

    try t.eq(@sizeOf(HeapObject), 40);
    try t.eq(@alignOf(HeapObject), 8);
    try t.eq(@sizeOf(HeapPage), 40 * 102);
    try t.eq(@alignOf(HeapPage), 8);

    var list: List = undefined;
    try t.eq(@intFromPtr(&list.list.ptr), @intFromPtr(&list) + 8);

    const arr = Array{
        .typeId = bt.Array,
        .rc = 1,
        .headerAndLen = 1,
        .bufStart = undefined,
    };
    try t.eq(@intFromPtr(&arr.typeId), @intFromPtr(&arr));
    try t.eq(@intFromPtr(&arr.rc), @intFromPtr(&arr) + 4);
    try t.eq(@intFromPtr(&arr.headerAndLen), @intFromPtr(&arr) + 8);
    try t.eq(Array.BufOffset, 12);
    try t.eq(@intFromPtr(&arr.bufStart), @intFromPtr(&arr) + Array.BufOffset);

    const astr = Astring{
        .typeId = bt.String,
        .rc = 1,
        .headerAndLen = (@as(u32, @intFromEnum(StringType.astring)) << 30) | @as(u32, 1),
        .bufStart = undefined,
    };
    try t.eq(@intFromPtr(&astr.typeId), @intFromPtr(&astr));
    try t.eq(@intFromPtr(&astr.rc), @intFromPtr(&astr) + 4);
    try t.eq(@intFromPtr(&astr.headerAndLen), @intFromPtr(&astr) + 8);
    try t.eq(Astring.BufOffset, 12);
    try t.eq(@intFromPtr(&astr.bufStart), @intFromPtr(&astr) + Astring.BufOffset);

    const ustr = Ustring{
        .typeId = bt.String,
        .rc = 1,
        .headerAndLen = (@as(u32, @intFromEnum(StringType.ustring)) << 30) | @as(u32, 1),
        .bufStart = undefined,
    };
    try t.eq(@intFromPtr(&ustr.typeId), @intFromPtr(&ustr));
    try t.eq(@intFromPtr(&ustr.rc), @intFromPtr(&ustr) + 4);
    try t.eq(@intFromPtr(&ustr.headerAndLen), @intFromPtr(&ustr) + 8);
    try t.eq(Ustring.BufOffset, 12);
    try t.eq(@intFromPtr(&ustr.bufStart), @intFromPtr(&ustr) + Ustring.BufOffset);

    try t.eq(@offsetOf(List, "typeId"), 0);    
    try t.eq(@offsetOf(List, "rc"), 4);    

    try t.eq(@offsetOf(Closure, "typeId"), @offsetOf(vmc.Closure, "typeId"));
    try t.eq(@offsetOf(Closure, "rc"), @offsetOf(vmc.Closure, "rc"));
    try t.eq(@offsetOf(Closure, "funcPc"), @offsetOf(vmc.Closure, "funcPc"));
    try t.eq(@offsetOf(Closure, "numParams"), @offsetOf(vmc.Closure, "numParams"));
    try t.eq(@offsetOf(Closure, "numCaptured"), @offsetOf(vmc.Closure, "numCaptured"));
    try t.eq(@offsetOf(Closure, "stackSize"), @offsetOf(vmc.Closure, "stackSize"));
    try t.eq(@offsetOf(Closure, "local"), @offsetOf(vmc.Closure, "local"));
    try t.eq(@offsetOf(Closure, "funcSigId"), @offsetOf(vmc.Closure, "rFuncSigId"));
    try t.eq(@offsetOf(Closure, "reqCallTypeCheck"), @offsetOf(vmc.Closure, "reqCallTypeCheck"));
    try t.eq(@offsetOf(Closure, "firstCapturedVal"), @offsetOf(vmc.Closure, "firstCapturedVal"));

    try t.eq(@offsetOf(cy.ValueMap, "metadata"), @offsetOf(vmc.ValueMap, "metadata"));
    try t.eq(@offsetOf(cy.ValueMap, "entries"), @offsetOf(vmc.ValueMap, "entries"));
    try t.eq(@offsetOf(cy.ValueMap, "size"), @offsetOf(vmc.ValueMap, "size"));
    try t.eq(@offsetOf(cy.ValueMap, "cap"), @offsetOf(vmc.ValueMap, "cap"));
    try t.eq(@offsetOf(cy.ValueMap, "available"), @offsetOf(vmc.ValueMap, "available"));
}