/// Heap objects, object allocation and deinitializers.
const cy = @import("cyber.zig");
const heap_value = @import("heap_value.zig");
const C = @import("capi.zig");
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
var trace_allocator: TraceAllocator = undefined;
var initedAllocator = false;

fn initAllocator() void {
    defer initedAllocator = true;
    switch (cy.Malloc) {
        .zig, .malloc => return,
        .mimalloc => {
            miAlloc.init();
        },
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
                if (cy.Trace) {
                    trace_allocator.alloc_ = t.alloc;
                    return trace_allocator.allocator();
                } else {
                    return t.alloc;
                }
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

// Uses a backing allocator and zeros the freed memory to surface UB more consistently.
pub const TraceAllocator = struct {
    alloc_: std.mem.Allocator,

    const vtable = std.mem.Allocator.VTable{
        .alloc = alloc,
        .resize = resize,
        .free = free,
        .remap = remap,
    };

    pub fn allocator(self: *TraceAllocator) std.mem.Allocator {
        return std.mem.Allocator{
            .ptr = self,
            .vtable = &vtable,
        };
    }

    fn alloc(ptr: *anyopaque, len: usize, log2_align: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *TraceAllocator = @ptrCast(@alignCast(ptr));
        return self.alloc_.rawAlloc(len, log2_align, ret_addr);
    }

    fn resize(ptr: *anyopaque, buf: []u8, log2_align: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *TraceAllocator = @ptrCast(@alignCast(ptr));
        return self.alloc_.rawResize(buf, log2_align, new_len, ret_addr);
    }

    fn remap(ptr: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *TraceAllocator = @ptrCast(@alignCast(ptr));
        return self.alloc_.rawRemap(memory, alignment, new_len, ret_addr);
    }

    fn free(ptr: *anyopaque, buf: []u8, log2_align: std.mem.Alignment, ret_addr: usize) void {
        const self: *TraceAllocator = @ptrCast(@alignCast(ptr));
        @memset(buf, 0);
        return self.alloc_.rawFree(buf, log2_align, ret_addr);
    }
};

pub const ObjectTrace = struct {
    /// Triggered at a specific event_id.
    alloc_event: u64,

    /// Info about where the allocation was triggered. Can be the pc if allocated from VM.
    alloc_ctx: u64,

    /// Info about where the free was triggered.
    /// `NullId` indicates allocation from outside the VM.
    free_ctx: ?u64,

    /// Type when freed.
    free_type: ?cy.TypeId,

    free_event: u64,
};

/// The purpose of Value life cycle operations (destroy, destruct, copy)
/// is to be used for CTE or bindings that use simple types.
/// It tries to mirror the runtime behavior as defined in `builtins.cy`
/// but will not be able to handle custom type operations defined by the user.
pub const Heap = struct {
    alloc: std.mem.Allocator, 

    /// Object heap pages.
    heapPages: std.ArrayList(*cy.heap.HeapPage),
    heapFreeHead: ?*HeapObject,

    /// Trace mode.
    heapFreeTail: ?*HeapObject,

    buffers: std.AutoHashMapUnmanaged([*]align(8) u8, usize),

    /// Still depend on sema for type info, symbol names, writeTypeName.
    sema: *cy.Sema,

    /// Tracks object's lifecycle. For trace mode.
    objectTraceMap: std.AutoHashMapUnmanaged(*HeapObject, ObjectTrace),

    // Trace mode.
    countFrees: bool,
    numFreed: u32,

    c: vmc.Heap,

    pub fn init(alloc: std.mem.Allocator, sema: *cy.Sema) !Heap {
        var new = Heap{
            .alloc = alloc,
            .sema = sema,
            .heapPages = .{},
            .buffers = .{},
            .heapFreeHead = null,
            .heapFreeTail = if (cy.Trace) null else undefined,
            .objectTraceMap = .{},
            .countFrees = false,
            .numFreed = 0,
            .c = .{
                .ctx = cy.NullId,
                .numRetains = 0,
                .numReleases = 0,
                .refCounts = if (cy.TrackGlobalRC) 0 else undefined,
                .trace_event = 1,
            },
        };

        // Initialize heap.
        const list = try new.growHeapPages(1);
        new.heapFreeHead = list.head;
        if (cy.Trace) {
            new.heapFreeTail = list.tail;
        }
        return new;
    }

    pub fn deinit(self: *Heap, reset: bool) void {
        if (self.buffers.size > 0) {
            var iter = self.buffers.iterator();
            while (iter.next()) |e| {
                std.debug.print("unfreed: {*}\n", .{e.key_ptr.*});
            }
            std.debug.panic("Unfreed raw buffers: {}", .{self.buffers.size});
        }
        if (!reset) {
            self.buffers.deinit(self.alloc);
        }

        if (reset) {
            // Does not clear heap pages so objects can persist.
        } else {
            for (self.heapPages.items) |page| {
                self.alloc.destroy(page);
            }
            self.heapPages.deinit(self.alloc);
        }

        if (reset) {
            self.objectTraceMap.clearRetainingCapacity();
        } else {
            self.objectTraceMap.deinit(self.alloc);
        }
    }

    pub fn next_trace_event(self: *Heap) u64 {
        defer self.c.trace_event += 1;
        if (cy.Trace and self.c.trace_panic_at_event == self.c.trace_event) {
            std.debug.panic("stop at heap event: {}", .{self.c.trace_panic_at_event});
        }
        return self.c.trace_event;
    }

    pub fn count_objects(self: *Heap) usize {
        var count: usize = 0;
        for (self.heapPages.items) |page| {
            var i: u32 = 1;
            while (i < page.objects.len) {
                const obj = &page.objects[i];
                if (obj.head.free_span.meta != cy.NullId) {
                    count += 1;
                }
                i += 1;
            }
        }
        return count;
    }

    /// Generic dump. VM has its own dump that recovers an allocation trace.
    pub fn dumpLiveObjects(self: *Heap) void {
        var iter = self.objectTraceMap.iterator();
        while (iter.next()) |it| {
            const trace = it.value_ptr.*;
            if (trace.free_ctx == null) {
                const obj = it.key_ptr.*;
                std.debug.print("Unfreed {*} {}, rc={}, ev={}\n", .{
                    obj, obj.getTypeId(), obj.rc(), trace.alloc_event
                });
            }
        }
    }

    pub fn getType(self: *const Heap, id: cy.TypeId) *cy.Type {
        return self.sema.types.items[id];
    }

    pub fn getSymbolName(self: *const Heap, id: u64) []const u8 {
        return self.sema.symbols.items[@intCast(id)];
    }

    pub fn freeBigObject(self: *cy.Heap, obj: *HeapObject, len: usize) void {
        const header = obj.bigHeader();
        freePoolObject(self, header.ptr_obj);
        if (cy.Trace) {
            if (self.objectTraceMap.getPtr(obj)) |trace| {
                trace.free_ctx = self.c.ctx;
                trace.free_type = obj.getTypeId();
                trace.free_event = self.next_trace_event();
            } else {
                log.trace("Missing object trace {*} {}", .{obj, obj.getTypeId()});
            }
        }
        const HeaderSize = 8 + 8;
        const slice: []align(8) u8 = @alignCast(@as([*]u8, @ptrCast(header))[0..len+HeaderSize]);

        // Allocator should automatically zero the memory for trace mode.
        self.alloc.free(slice);
    }

    // Free the object given its byte size.
    pub fn freeObject(self: *Heap, obj: *HeapObject, size: usize) void {
        if (size <= 4 * 8) {
            freePoolObject(self, obj);
        } else {
            freeBigObject(self, obj, size);
        }
    }

    /// typeId should be cleared in trace mode since tracking may still hold a reference to the object.
    /// The gc also needs it for now to traverse objects in pages.
    pub fn freePoolObject(self: *Heap, obj: *HeapObject) void {
        if (cy.Trace) {
            if (self.objectTraceMap.getPtr(obj)) |trace| {
                trace.free_ctx = self.c.ctx;
                trace.free_type = obj.getTypeId();
                trace.free_event = self.next_trace_event();
            } else {
                log.trace("Missing object trace {*} {}", .{obj, obj.getTypeId()});
            }
        }
        const pobj = obj.asPoolObject();
        if (cy.Trace) {
            // Clear any memory unused by freeSpan to help surface dangling pointer access.
            pobj.data = std.mem.zeroes([32]u8);

            // Tracing performs LRU allocation to surface double free errors better.
            // When an object is constantly reused, the most recent traced alloc/free info
            // can mask an earlier alloc/free on the same object pointer.

            pobj.head.free_span = .{
                .meta = NullId,
                .len = 0,
            };
            obj.free_span.next = null;
            if (self.heapFreeTail == null) {
                self.heapFreeTail = obj;
                self.heapFreeHead = obj;
            } else {
                self.heapFreeTail.?.free_span.next = obj;
            }
        } else {
            // Prepend free slot to MRU free list.
            pobj.head.free_span = .{
                .meta = NullId,
                .len = 0,
            };
            obj.free_span.next = self.heapFreeHead;
            self.heapFreeHead = obj;
        }
    }

    pub fn checkDoubleFree(self: *Heap, obj: *cy.HeapObject) bool {
        if (isObjectAlreadyFreed(self, obj)) {
            return true;
        }
        return false;
    }

    pub fn checkRetainDanglingPointer(self: *cy.Heap, obj: *cy.HeapObject) bool {
        if (isObjectAlreadyFreed(self, obj)) {
            return true;
        }
        if (!self.objectTraceMap.contains(obj)) {
            std.debug.panic("Retaining untracked heap object: {*}", .{obj});
        }
        return false;
    }

    pub fn newRes(self: *Heap, res_t: *cy.types.Result, val: cy.Value) !cy.Value {
        const new = try self.new_object_undef(res_t.base.id(), res_t.size);
        const dst = new.object.getValuesPtr();
        dst[0] = Value.initInt(0);

        const child_t = res_t.child_t;
        try self.copyValueTo2(child_t, @ptrCast(dst+1), val);
        self.destructValue2(child_t, val);
        return Value.initPtr(new);
    }

    pub fn newErr(self: *Heap, res_t: *cy.types.Result, err: cy.Value) !cy.Value {
        const new = try self.new_object_undef(res_t.base.id(), res_t.size);
        const dst = new.object.getValuesPtr();
        dst[0] = Value.initInt(1);
        dst[1] = err;
        return Value.initPtr(new);
    }

    /// Arguments are consumed.
    pub fn newInstance(self: *Heap, type_: *cy.Type, field_inits: []const C.FieldInit) !Value {
        if (type_.kind() != .struct_t) {
            cy.panicFmt("Expected struct type. Found `{}`.", .{type_.kind()});
        }

        const struct_t = type_.cast(.struct_t);
        const fields = struct_t.fields();

        const new = try self.new_object_undef(type_.id(), type_.size());
        const dst = new.object.getBytePtr();

        const mod = type_.sym().getMod();
        for (field_inits) |field_init| {
            const name = C.from_bytes(field_init.name);
            const sym = mod.getSym(name) orelse {
                cy.panicFmt("No such field `{s}`.", .{name});
            };
            if (sym.type != .field) {
                cy.panicFmt("`{s}` is not a field.", .{name});
                return self.prepPanic("Not a field.");
            }
            const field = fields[sym.cast(.field).idx];

            const val = @as(Value, @bitCast(field_init.value));
            self.moveValueTo(field.type, dst + field.offset, val);
        }
        return Value.initPtr(new);
    }

    pub fn newNone(self: *Heap, option_t: *cy.types.Option) !cy.Value {
        if (option_t.zero_union) {
            return Value.initInt(0);
        } else {
            const new = try self.new_object_undef(option_t.base.id(), option_t.size);
            const dst = new.object.getValuesPtr();
            dst[0] = Value.initInt(0);
            dst[1] = Value.initInt(0);
            return Value.initPtr(new);
        }
    }

    pub fn newSome(self: *Heap, option_t: *cy.types.Option, val: cy.Value) !cy.Value {
        if (option_t.zero_union) {
            return val;
        } else {
            const new = try self.new_object_undef(option_t.base.id(), option_t.size);
            const dst = new.object.getValuesPtr();
            dst[0] = Value.initInt(1);

            const child_t = option_t.child_t;
            try self.copyValueTo2(child_t, @ptrCast(dst+1), val);
            self.destructValue2(child_t, val);
            return Value.initPtr(new);
        }
    }

    pub fn newChoice(self: *Heap, new_t: *cy.Type, tag_name: []const u8, val: cy.Value) !cy.Value {
        if (new_t.kind() != .choice) {
            return cy.panicFmt("Expected choice type. Found `{}`.", .{new_t.kind()});
        }
        const choice_t = new_t.cast(.choice);

        const sym = new_t.sym().getMod().getSym(tag_name) orelse {
            return cy.panicFmt("Can not find case `{s}`.", .{tag_name});
        };
        if (sym.type != .choice_case) {
            return cy.panicFmt("`{s}` is not choice case.", .{tag_name});
        }
        const case = sym.cast(.choice_case);

        const new = try self.new_object_undef(new_t.id(), choice_t.size);
        const dst = new.object.getValuesPtr();
        dst[0] = Value.initInt(@intCast(case.val));

        try self.copyValueTo2(case.payload_t, @ptrCast(dst+1), val);
        self.destructValue2(case.payload_t, val);
        return Value.initPtr(new);
    }

    /// Returns the first free HeapObject.
    pub fn growHeapPages(self: *Heap, numPages: usize) !HeapObjectList {
        var idx = self.heapPages.items.len;
        try self.heapPages.resize(self.alloc, self.heapPages.items.len + numPages);

        // Allocate first page.
        var page = try self.alloc.create(HeapPage);
        self.heapPages.items[idx] = page;

        const first = initHeapPage(page);
        var last = first;
        idx += 1;
        while (idx < self.heapPages.items.len) : (idx += 1) {
            page = try self.alloc.create(HeapPage);
            self.heapPages.items[idx] = page;
            const next = initHeapPage(page);
            last.tail.free_span.next = next.head;
            last = next;
        }
        return .{
            .head = first.head,
            .tail = last.tail,
        };
    }

    pub const init_str = Root.init_str;
    pub const init_astr = Root.init_astr;
    pub const init_astr_undef = Root.init_astr_undef;
    pub const init_ustr = Root.init_ustr;
    pub const init_ustr_undef = Root.init_ustr_undef;
    pub const init_astr_concat = Root.init_astr_concat;
    pub const init_ustr_concat = Root.init_ustr_concat;
    pub const init_astr_concat3 = Root.init_astr_concat3;
    pub const init_ustr_concat3 = Root.init_ustr_concat3;
    pub const newStr = Root.newStr;
    pub const newGenericStr = Root.new_str_lit;
    pub const newEmptyString = Root.newEmptyString;
    pub const init_empty_str = Root.init_empty_str;
    pub const newAstr = Root.newAstr;
    pub const newUstr = Root.newUstr;
    pub const newAstrConcat = Root.newAstrConcat;
    pub const newUstrConcat = Root.newUstrConcat;
    pub const newAstrConcat3 = Root.newAstrConcat3;
    pub const newUstrConcat3 = Root.newUstrConcat3;
    pub const newAstrSlice = Root.newAstrSlice;
    pub const init_astr_slice = Root.init_astr_slice;
    pub const init_ustr_slice = Root.init_ustr_slice;
    pub const newUstrSlice = Root.newUstrSlice;
    pub const new_byte_buffer = Root.new_byte_buffer;
    pub const free_byte_buffer = Root.free_byte_buffer;
    pub const allocHostObject = Root.allocHostObject;
    pub const allocHostFuncPtr = Root.allocHostFuncPtr;
    pub const allocHostFuncUnion = Root.allocHostFuncUnion;
    pub const allocTable = Root.allocTable;
    pub const allocMapZero = Root.allocMapZero;
    pub const newEmptyArray = Root.newEmptyArray;
    pub const newArray = Root.newArray;
    pub const init_slice_undef = Root.init_slice_undef;
    pub const new_slice = Root.new_slice;
    pub const new_raw_buffer = Root.new_raw_buffer;
    pub const new_buffer = Root.new_buffer;
    pub const new_buffer_empty = Root.new_buffer_empty;
    pub const new_buffer_undef = Root.new_buffer_undef;
    pub const allocVector = Root.allocVector;
    pub const allocPointer = Root.allocPointer;
    pub const allocInt = Root.allocInt;
    pub const allocBool = Root.allocBool;
    pub const allocFuncSig = Root.allocFuncSig;
    pub const allocBoxValue = Root.allocBoxValue;
    pub const allocPoolObject = Root.allocPoolObject;
    pub const allocBigObject = Root.allocBigObject;
    pub const newSmallObjectUndef = Root.newSmallObjectUndef;
    pub const newBigObjectUndef = Root.newBigObjectUndef;
    pub const lift = Root.lift;
    pub const new_object_undef = Root.new_object_undef;
    pub const newAstrUndef = Root.newAstrUndef;
    pub const newUstrUndef = Root.newUstrUndef;
    pub const allocObjectSmall = Root.allocObjectSmall;
    pub const allocObject = Root.allocObject;
    pub const allocObject2 = Root.allocObject2;
    pub const newStringBufferUndef = Root.newStringBufferUndef;
    pub const allocFunc = Root.allocFunc;
    pub const allocClosure = Root.allocClosure;
    pub const allocTrait = Root.allocTrait;
    pub const allocTccState = Root.allocTccState;

    pub const destroyObject = heap_value.destroyObject;
    pub const destructStr = heap_value.destructStr;
    pub const destructValue = heap_value.destructValue;
    pub const destructValue2 = heap_value.destructValue2;
    pub const destructValueAt = heap_value.destructValueAt;
    pub const moveValueAt = heap_value.moveValueAt;
    pub const moveValueTo = heap_value.moveValueTo;
    pub const copy_str = heap_value.copy_str;
    pub const copyValue2 = heap_value.copyValue2;
    pub const copyValue3 = heap_value.copyValue3;
    pub const copyValueTo = heap_value.copyValueTo;
    pub const copyValueTo2 = heap_value.copyValueTo2;
    pub const copyStructTo = heap_value.copyStructTo;
    pub const freeOnly = heap_value.freeOnly;
    pub const release = heap_value.release;
    pub const releaseOnly = heap_value.releaseOnly;
    pub const releaseObject = heap_value.releaseObject;
    pub const releaseOpaque = heap_value.releaseOpaque;
    pub const retain = heap_value.retain;
    pub const retainObject = heap_value.retainObject;
    pub const retainObjectOpt = heap_value.retainObjectOpt;
    pub const unbox = heap_value.unbox;
    pub const unwrap_option_or = heap_value.unwrap_option_or;
    pub const unwrapChoice = heap_value.unwrapChoice;
    pub const get_or_buf_print_object = heap_value.get_or_buf_print_object;
    pub const get_or_buf_print_object2 = heap_value.get_or_buf_print_object2;
    pub const bufPrintValueShortStr = heap_value.bufPrintValueShortStr;
    pub const bufPrintBoxValueShortStr = heap_value.bufPrintBoxValueShortStr;
    pub const writeValue = heap_value.writeValue;
};

pub const PoolObject = extern struct {
    head: PoolObjectHeader,
    data: [32]u8 align(8),
};

// Keep it just under 4kb page.
pub const HeapPage = struct {
    objects: [102]PoolObject,
};

const HeapObjectId = u32;

const BigObjectHeader = extern struct {
    ptr_obj: *HeapObject,
    meta: u32,
    rc: u32,
};

pub const PoolObjectHeader = extern union {
    object: extern struct {
        meta: u32 align(8),
        rc: u32,
    },
    free_span: extern struct {
        meta: u32 align(8),
        len: u32,
    },
};

/// Can be allocated in pool by gpa depending on the size.
pub const HeapObject = extern union {
    free_span: extern struct {
        unused: u64 = 0,
        next: ?*HeapObject,
    },
    raw_buffer: RawBuffer,
    buffer: Buffer,
    array: Array,
    map: MapValue,
    mapIter: MapIterator,
    range: Range,

    // Functions.
    func: Func,

    string: Str,
    str_lit: StrLit,
    object: Object,
    trait: Trait,
    tccState: if (cy.hasFFI) TccState else void,
    pointer: Pointer,
    integer: Int,
    byte: Byte,
    symbol: u32,

    pub inline fn bigHeader(self: *HeapObject) *BigObjectHeader {
        return @ptrFromInt(@intFromPtr(self) - 16);
    }

    pub inline fn header(self: *HeapObject) *PoolObjectHeader {
        return @ptrFromInt(@intFromPtr(self) - 8);
    }

    pub inline fn asPoolObject(self: *HeapObject) *PoolObject {
        return @ptrFromInt(@intFromPtr(self) - 8);
    }

    pub inline fn rc(self: *HeapObject) u32 {
        const head = self.header();
        return head.object.rc;
    }

    pub inline fn release(self: *HeapObject) void {
        const head = self.header();
        head.object.rc -= 1;
    }

    pub inline fn retain(self: *HeapObject) void {
        const head = self.header();
        head.object.rc += 1;
    }

    pub inline fn getTypeId(self: *HeapObject) cy.TypeId {
        const head = self.header();
        return head.object.meta & vmc.TYPE_MASK;
    }

    pub inline fn isFreed(self: *HeapObject) bool {
        const head = self.header();
        return head.object.meta == cy.NullId;
    }

    pub inline fn isGcMarked(self: *HeapObject) bool {
        const head = self.header();
        return (head.object.meta & vmc.GC_MARK_BIT) != 0;
    }

    pub inline fn setGcMarked(self: *HeapObject) void {
        const head = self.header();
        head.object.meta = head.object.meta | vmc.GC_MARK_BIT;
    }

    pub inline fn resetGcMarked(self: *HeapObject) void {
        const head = self.header();
        head.object.meta = head.object.meta & ~vmc.GC_MARK_BIT;
    }

    pub inline fn isExternalObject(self: *HeapObject) bool {
        const head = self.header();
        return (head.object.meta & vmc.EXTERNAL_TYPE_BIT) == vmc.EXTERNAL_TYPE_BIT;
    }
};

pub const Range = extern struct {
    start: i64,
    end: i64,
};

pub const Buffer = extern struct {
    base: u64,
    len: u64,
    header: u64,

    pub fn slice(self: *Buffer, comptime T: type) []T {
        return @as([*]T, @ptrFromInt(self.base))[0..self.len];
    }

    pub fn cap(self: *Buffer) usize {
        return @intCast(self.header & ~(@as(u64, 3) << 62));
    }

    pub fn initElem(self: *Buffer, heap: *cy.Heap, elem_t: *cy.Type, idx: usize, src: [*]u8) !void {
        const byte_dst: [*]u8 = @ptrFromInt(self.base);
        const dst = byte_dst + idx * elem_t.size();
        try heap.copyValueTo(elem_t, dst, src);
    }
};

pub const RawBuffer = extern struct {
    len: u64,
    data: void,

    pub fn slice(self: *RawBuffer, comptime T: type) []T {
        return @as([*]T, @ptrCast(&self.data))[0..self.len];
    }

    pub fn elemsPtr(self: *RawBuffer) [*]Value {
        return @ptrCast(&self.data);
    }

    pub fn getElemPtr(self: *RawBuffer, idx: usize, elem_size: usize) [*]u8 {
        const dst: [*]u8 = @ptrCast(&self.data);
        return dst + idx * elem_size;
    }

    pub fn index(self: *RawBuffer, heap: *Heap, elem_t: *cy.Type, idx: usize) !cy.Value {
        const src = self.getElemPtr(idx, elem_t.size());
        return heap.copyValue3(elem_t, @ptrCast(@alignCast(src)));
    }

    pub fn initElem(self: *RawBuffer, heap: *cy.Heap, elem_t: *cy.Type, idx: usize, src: [*]Value) !void {
        const byte_dst: [*]u8 = @ptrCast(&self.data);
        const dst = byte_dst + idx * elem_t.size();
        try heap.copyValueTo(elem_t, dst, @ptrCast(src));
    }
};

pub const Slice = extern struct {
    buf: u64,  // *void
    ptr: u64,
    len: u64, 
    header: u64,

    pub fn items(self: *const Slice, comptime T: type) []T {
        var buf: [*]T = @ptrFromInt(self.ptr);
        return buf[0..self.len];
    }
};

pub const Span = extern struct {
    ptr: u64,  // *void
    len: u64, 

    pub fn items(self: *const Span, comptime T: type) []T {
        var buf: [*]T = @ptrFromInt(self.ptr);
        return buf[0..self.len];
    }
};

pub const Array = extern struct {
    ptr: u64,  // *void
    len: u64, 
    cap: u64,

    pub fn items(self: *Array, comptime T: type) []T {
        var buf: [*]T = @ptrFromInt(self.ptr);
        return buf[0..self.len];
    }
};

pub const NumberFormatConfig = extern struct {
    has_pad: u64,
    pad: u64,
    has_width: u64,
    width: u64,
};

pub const MapValue = extern struct {
    meta: [*]u64,
    keys: [*]u64,
    vals: [*]u64,
    size: u64,
    nvac: u64,
    cap: u64,

    // pub fn get(self: *Table, key: Value) ?Value {
    //     return self.map().get(key);
    // }

    /// index and val already have +1 retain.
    pub fn setConsume(self: *MapValue, vm: *cy.VM, index: Value, val: Value) !void {
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

    pub fn set(self: *MapValue, vm: *cy.VM, index: Value, val: Value) !void {
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
    map: Value,
    nextIdx: u32,
};

pub const PtrSpan = extern struct {
    ptr: u64,
    len: u64,
};

pub const StrLit = extern struct {
    ptr: u64,  // [*]u8
    header: u64,

    pub fn ascii(self: *const StrLit) bool {
        return self.header & (1 << 63) != 0;
    }

    pub fn slice(self: *const StrLit) []const u8 {
        return @as([*]const u8, @ptrFromInt(self.ptr))[0..self.len()];
    }

    pub fn len(self: *const StrLit) u64 {
        return @intCast(self.header & ~(@as(u64, 1) << 63));
    }
};

pub const StrBuffer = RawBuffer;
pub const Str = extern struct {
    buf_: u64, // ?^StringBuffer
    ptr: u64,  // *void
    len_: u64, // sign bit indicates ascii

    pub fn buf(self: *const Str) ?*StrBuffer {
        return @ptrFromInt(self.buf_);
    }

    pub fn setBuffer(self: *Str, new: *StrBuffer) void {
        self.buf_ = @intFromPtr(new);
        self.ptr = @intFromPtr(&new.data);
    }

    pub fn ascii(self: *const Str) bool {
        return self.len_ & (1 << 63) != 0;
    }

    pub fn slice(self: *const Str) []const u8 {
        return @as([*]const u8, @ptrFromInt(self.ptr))[0..self.len()];
    }

    pub fn mutSlice(self: *const Str) []u8 {
        return @as([*]u8, @ptrFromInt(self.ptr))[0..self.len()];
    }

    pub fn len(self: *const Str) u32 {
        return @intCast(self.len_ & ~(@as(u64, 1) << 63));
    }
};

pub const Trait = extern struct {
    vtable: u64,
    impl: Value,
};

fn allocTrait(vm: *cy.VM, type_id: cy.TypeId, vtable: u16, impl: cy.Value) !cy.Value {
    const obj = try allocPoolObject(vm, type_id);
    obj.trait = .{
        .impl = impl,
        .vtable = vtable,
    };
    return Value.initPtr(obj);
}

pub const Object = extern struct {
    firstValue: Value,

    pub inline fn getValuesConstPtr(self: *const Object) [*]const Value {
        return @ptrCast(&self.firstValue);
    }

    pub inline fn getValuesPtr(self: *Object) [*]Value {
        return @ptrCast(&self.firstValue);
    }

    pub inline fn getBytePtr(self: *Object) [*]u8 {
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

    dead,
};

pub const ContinuationTask = struct {
    thread: *cy.Thread,
};

pub const AsyncTaskNode = struct {
    /// Tasks are linked together by a Future's continuation.
    next: u64,

    task: AsyncTask,
};

pub const AsyncTask = struct {
    type: AsyncTaskType,
    data: union {
        callback: Value,
        cont: ContinuationTask,
    },
};

pub const FutureResolver = extern struct {
    future: *Future,
};

pub const Future = extern struct {
    inner: *FutureValue,
};

pub const FutureValue = extern struct {
    cont_head: u64,
    cont_tail: u64,
    shared_state: ?*cy.sync.FutureSyncState,
    completed: bool,
    data: u64,
};

const FuncKind = enum(u8) {
    bc,
    host,
    closure,
    pinned_closure,
};

// Currently an object but will be turned into a value instead.
pub const Func = extern struct {
    kind: FuncKind align(8),
    padding: u8 = undefined,
    padding2: u16 = undefined,
    padding3: u32 = undefined,

    data: extern union {
        host: extern struct {
            ptr: vmc.HostFn,
        },
        bc: extern struct {
            pc: [*]cy.Inst,
        },
        closure: extern struct {
            pc: [*]cy.Inst,

            numCaptured: u64,

            // Begins array of boxed values.
            firstCapturedVal: void,
        },
    },

    pub inline fn getCapturedValuesPtr(self: *Func) [*]Value {
        return @ptrCast(@alignCast(&self.data.closure.firstCapturedVal));
    }
};

pub const FuncSym = extern struct {
    func: *cy.Func align(8),
};

const TccState = extern struct {
    state: *tcc.TCCState align(8),
    lib: *std.DynLib,
    hasDynLib: bool,
};

pub const Pointer = extern struct {
    ptr: ?*anyopaque align(8),
};

pub const Int = extern struct {
    val: i64,
};

pub const Byte = extern struct {
    val: u8,
};

/// Initializes the page with freed object slots and returns the pointer to the first slot.
fn initHeapPage(page: *HeapPage) HeapObjectList {
    // Set next up to the last slot.
    for (page.objects[0..page.objects.len-1]) |*pool_obj| {
        pool_obj.head.free_span = .{
            .meta = NullId,
            .len = 0,
        };
        const obj: *HeapObject = @ptrCast(&pool_obj.data);
        obj.free_span = .{
            .next = @ptrFromInt(@intFromPtr(obj) + @sizeOf(PoolObject)),
        };
    }
    const last = &page.objects[page.objects.len-1];
    last.head.free_span = .{
        .meta = NullId,
        .len = 0,
    };
    const last_obj: *HeapObject = @ptrCast(&last.data);
    last_obj.free_span = .{
        .next = null,
    };
    return .{
        .head = @ptrCast(&page.objects[0].data),
        .tail = last_obj,
    };
}

const HeapObjectList = struct {
    head: *HeapObject,
    tail: *HeapObject,
};

pub const BigObjectPtrType = cy.NullId - 1;

pub fn allocBigObject(self: *Heap, type_id: cy.TypeId, size: usize) !*HeapObject {
    // Allocate pointer object to keep track of the big object.
    const ptr_obj = try allocBigObjectPtr(self);

    // Meta type field, rc field, and a pointer to the pointer object.
    const HeaderSize = 8 + 8;

    // Align with HeapObject so it can be casted.
    const slice = try self.alloc.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(HeapObject)), size + HeaderSize);
    const obj: *HeapObject = @ptrFromInt(@intFromPtr(slice.ptr) + HeaderSize);
    const header: *BigObjectHeader = @ptrCast(slice.ptr);
    header.* = .{
        .ptr_obj = ptr_obj,
        .meta = type_id,
        .rc = 1,
    };
    ptr_obj.pointer.ptr = obj;

    log.tracevIf(log_mem, "0 +1 alloc big object: {*} type={} evt={}", .{obj, type_id, cy.event_id});
    if (cy.TrackGlobalRC) {
        self.c.refCounts += 1;
    }
    if (cy.Trace) {
        self.c.numRetains += 1;
    }
    if (cy.Trace) {
        try traceAlloc(self, obj);
    }
    return obj;
}

pub fn allocBigObjectPtr(self: *Heap) !*HeapObject {
    if (self.heapFreeHead == null) {
        const list = try self.growHeapPages(@max(1, self.heapPages.items.len));
        self.heapFreeHead = list.head;
        if (cy.Trace) {
            self.heapFreeTail = list.tail;
        }
    }
    const ptr = self.heapFreeHead.?;

    self.heapFreeHead = ptr.free_span.next;
    const header = ptr.header();
    header.object = .{
        .meta = BigObjectPtrType,
        .rc = 0,
    };

    if (cy.Trace) {
        try traceAlloc(self, ptr);
        if (self.heapFreeTail == ptr) {
            // Ensure tail is updated if it was the same segment as head.
            self.heapFreeTail = self.heapFreeHead;
        }
    }
    log.tracevIf(log_mem, "alloc big object ptr: {*}", .{ptr});

    return ptr;
}

/// Allocates with RC = 1.
pub fn allocPoolObject(self: *Heap, type_id: cy.TypeId) !*HeapObject {
    if (self.heapFreeHead == null) {
        const list = try self.growHeapPages(@max(1, self.heapPages.items.len));
        self.heapFreeHead = list.head;
        if (cy.Trace) {
            self.heapFreeTail = list.tail;
        }
    }
    const ptr = self.heapFreeHead.?;

    self.heapFreeHead = ptr.free_span.next;
    const header = ptr.header();
    header.object = .{
        .meta = type_id,
        .rc = 1,
    };

    log.tracevIf(log_mem, "0 +1 alloc pool object: {*} type={} evt={}", .{ptr, type_id, cy.event_id});
    if (cy.Trace) {
        try traceAlloc(self, ptr);
        if (self.heapFreeTail == ptr) {
            // Ensure tail is updated if it was the same segment as head.
            self.heapFreeTail = self.heapFreeHead;
        }
    }
    if (cy.TrackGlobalRC) {
        self.c.refCounts += 1;
    }
    if (cy.Trace) {
        self.c.numRetains += 1;
    }

    return ptr;
}

pub fn new_buffer_undef(self: *Heap, buffer_t: *cy.Type, len: usize) !Value {
    const elem_t = buffer_t.sym().instance.?.params[0].asPtr(*cy.Type);
    const ptr = try self.new_byte_buffer(elem_t.size() * len);
    return self.newInstance(buffer_t, &.{
        C.field_init("base", Value.initPtr(ptr)),
        C.field_init("length", Value.initInt(@intCast(len))),
        C.field_init("header", Value.initInt(@intCast(len))),
    });
}

pub fn new_buffer_empty(self: *Heap, buffer_t: *cy.Type, cap: usize) !Value {
    const elem_t = buffer_t.sym().instance.?.params[0].asPtr(*cy.Type);
    const ptr = try self.new_byte_buffer(elem_t.size() * cap);
    return self.newInstance(buffer_t, &.{
        C.field_init("base", Value.initPtr(ptr)),
        C.field_init("length", Value.initInt(0)),
        C.field_init("header", Value.initInt(@intCast(cap))),
    });
}

pub fn new_buffer(self: *Heap, buffer_t: *cy.Type, args: []Value) !Value {
    const elem_t = buffer_t.sym().instance.?.params[0].asPtr(*cy.Type);
    const ptr = try self.new_byte_buffer(elem_t.size() * args.len);
    for (args, 0..) |arg, i| {
        const dst: [*]u8 = @ptrCast(ptr + i * elem_t.size());
        try self.copyValueTo2(elem_t, dst, arg);
    }

    return self.newInstance(buffer_t, &.{
        C.field_init("base", Value.initPtr(ptr)),
        C.field_init("length", Value.initInt(@intCast(args.len))),
        C.field_init("header", Value.initInt(@intCast(args.len))),
    });
}

pub fn new_raw_buffer(self: *Heap, buffer_t: cy.TypeId, n: usize, elem_size: usize) !*cy.HeapObject {
    var obj: *HeapObject = undefined;
    const nbytes = n * elem_size;
    const size = 8 + nbytes;
    if (size <= 4 * 8) {
        obj = try allocPoolObject(self, buffer_t);
    } else {
        obj = try allocBigObject(self, buffer_t, size);
    }
    obj.raw_buffer.len = n;
    return obj;
}

pub fn init_slice_undef(self: *Heap, byte_buffer_t: cy.TypeId, n: usize, elem_size: usize) !Slice {
    const buffer = try self.new_raw_buffer(byte_buffer_t, n * elem_size, 1);
    return .{
        .buf = @intFromPtr(buffer),
        .ptr = @intFromPtr(buffer.raw_buffer.elemsPtr()),
        .len = n,
        .header = n,
    };
}

pub fn new_slice(self: *Heap, slice_t: *cy.Type, args: []Value) !Value {
    const elem_t = slice_t.sym().instance.?.params[0].asPtr(*cy.Type);
    const raw_buffer_t = slice_t.cast(.struct_t).fields()[0].type.cast(.option).child_t.cast(.pointer).child_t;
    const elem_size = raw_buffer_t.sym().instance.?.params[0].asPtr(*cy.Type).size();
    const raw_buffer = try self.new_raw_buffer(raw_buffer_t.id(), args.len, elem_size);
    const dst: [*]u8 = @ptrCast(raw_buffer.raw_buffer.elemsPtr());
    for (args, 0..) |arg, i| {
        try self.copyValueTo2(elem_t, dst + i * elem_t.size(), arg);
    }
    return self.newInstance(slice_t, &.{
        C.field_init("buf", Value.initPtr(raw_buffer)),
        C.field_init("ptr", Value.initPtr(dst)),
        C.field_init("_len", Value.initInt(@intCast(args.len))),
        C.field_init("header", Value.initInt(@intCast(args.len))),
    });
}

pub fn newArray(self: *Heap, arr_t: *cy.Type, args: []Value) !Value {
    const elem_t = arr_t.sym().instance.?.params[0].asPtr(*cy.Type);
    const buf = try self.alloc.alloc(u8, elem_t.size() * args.len);
    for (args, 0..) |arg, i| {
        const dst: [*]u8 = @ptrCast(buf.ptr + i * elem_t.size());
        try self.copyValueTo2(elem_t, dst, arg);
    }

    return self.newInstance(arr_t, &.{
        C.field_init("buf", Value.initPtr(buf.ptr)),
        C.field_init("length", Value.initInt(@intCast(args.len))),
        C.field_init("header", Value.initInt(@intCast(args.len))),
    });
}

pub fn newEmptyArray(self: *Heap, type_id: cy.TypeId) !Value {
    const obj = try allocPoolObject(self, type_id);
    obj.array = .{
        .ptr = undefined,
        .len = 0,
        .cap = 0,
    };
    return Value.initPtr(obj);
}

pub fn newStringBufferUndef(self: *Heap, nbytes: usize) !*cy.HeapObject {
    var obj: *HeapObject = undefined;
    const size = 8 + nbytes;
    if (size <= 4 * 8) {
        obj = try allocPoolObject(self, bt.StrBuffer);
    } else {
        obj = try allocBigObject(self, bt.StrBuffer, size);
    }
    obj.raw_buffer.len = nbytes;
    return obj;
}

/// Allocates a raw byte buffer. Does not have reference counting unlike `RawBuffer`.
pub fn new_byte_buffer(self: *Heap, size: usize) ![*]align(8) u8 {
    const slice = try self.alloc.alignedAlloc(u8, .@"8", size);
    // @as(*u64, @ptrCast(slice.ptr)).* = size;
    // self.num_ex_buffers += 1;
    // return slice.ptr + 8;
    log.tracevIf(log_mem, "alloc byte buffer: {*}\n", .{slice.ptr});
    try self.buffers.put(self.alloc, slice.ptr, slice.len);
    return slice.ptr;
}

pub fn free_byte_buffer(self: *Heap, opt_ptr: ?[*]align(8) u8) void {
    const ptr = opt_ptr orelse {
        return;
    };
    // const header = ptr - 8;
    // const size = @as(*u64, @ptrCast(header)).*;
    // self.num_ex_buffers -= 1;
    // self.alloc.free(header[0..8+size]);
    const e = self.buffers.fetchRemove(ptr).?;
    self.alloc.free(ptr[0..e.value]);
}

pub fn allocArrayUndef(heap: *Heap, type_id: cy.TypeId, size: usize) !*cy.HeapObject {
    var arr: *HeapObject = undefined;
    if (size <= 8 * 4) {
        arr = try allocPoolObject(heap, type_id);
    } else {
        arr = try allocBigObject(heap, type_id, size);
    }
    return arr;
}

/// Reuse `Object` so that address_of refers to the first element for both structs and arrays.
pub fn allocVector(self: *Heap, type_id: cy.TypeId, elems: []const Value) !Value {
    const type_e = self.getType(type_id);
    const child_t = type_e.cast(.vector).elem_t;
    const elem_size = child_t.size();
    const arr = try allocArrayUndef(self, type_id, elem_size * elems.len);

    const dst: [*]u8 = @ptrCast(arr);
    if (child_t.isVmStackLifted()) {
        for (elems, 0..) |elem, i| {
            const src = elem.asBytes()[0..elem_size];
            @memcpy(dst[i*elem_size..i*elem_size + elem_size], src);
        }
    } else {
        if (elem_size == 8) {
            const src: [*]const u8 = @ptrCast(elems.ptr);
            @memcpy(dst[0..elems.len * elem_size], src[0..elems.len * elem_size]);
        } else {
            for (elems, 0..) |*elem, i| {
                const elem_bytes: [*]const u8 = @ptrCast(elem);
                @memcpy(dst[i*elem_size..i*elem_size+elem_size], elem_bytes[0..elem_size]);
            }
        }
    }
    return Value.initPtr(arr);
}

pub fn allocTable(self: *Heap) !Value {
    const obj = try allocPoolObject(self);
    obj.table = .{
        .typeId = bt.Table,
        .rc = 1,
        .inner_map = try self.allocEmptyMap(),
    };
    return Value.initPtr(obj);
}

pub fn allocMapZero(self: *Heap, type_id: cy.TypeId) !Value {
    const obj = try allocBigObject(self, type_id, 6 * @sizeOf(Value));
    obj.map = .{
        .meta = undefined,
        .keys = undefined,
        .vals = undefined,
        .size = 0,
        .nvac = 0,
        .cap = 0,
    };
    return Value.initPtr(obj);
}

/// Captured values are retained during alloc.
pub fn allocClosure(
    self: *Heap, fp: [*]Value, func_pc: [*]cy.Inst, union_t: cy.TypeId, captured: [*]const cy.Inst, num_captured: u8, pinned_closure: bool,
) !Value {
    var obj: *HeapObject = undefined;
    if (num_captured <= 1) {
        obj = try allocPoolObject(self, union_t);
    } else {
        obj = try allocBigObject(self, union_t, (3 + num_captured) * @sizeOf(Value));
    }
    obj.func = .{
        .kind = if (pinned_closure) .pinned_closure else .closure,
        .data = .{ .closure = .{
            .pc = func_pc,
            .numCaptured = @intCast(num_captured),
            .firstCapturedVal = undefined,
        } },
    };
    const dst = obj.func.getCapturedValuesPtr();
    for (0..num_captured) |i| {
        const reg = @as(*const align(1) u16, @ptrCast(captured + i*2)).*;
        if (!pinned_closure) {
            self.retain(fp[reg]);
        }
        dst[i] = fp[reg];
    }
    return Value.initPtr(obj);
}

pub fn allocBcFuncUnion(self: *Heap, union_t: cy.TypeId, pc: [*]cy.Inst) !Value {
    const obj = try allocPoolObject(self, union_t);
    obj.func = .{
        .kind = .bc,
        .data = .{ .bc = .{
            .pc = pc,
        }},
    };
    return Value.initPtr(obj);
}

pub fn init_empty_str(self: *Heap) !Str {
    const empty = "";
    return self.init_astr_slice(empty, null);
} 

pub fn newEmptyString(self: *Heap) !Value {
    const empty = "";
    return self.newAstrSlice(empty, null);
} 

pub fn new_str_lit(self: *Heap, str: []const u8) !Value {
    const dupe = try self.alloc.dupe(u8, str);
    const obj = try allocPoolObject(self, bt.StrLit);
    obj.str_lit = .{
        .ptr = @intFromPtr(dupe.ptr),
        .header = str.len,
    };
    if (cy.string.isAstring(str)) {
        obj.str_lit.header |= 1 << 63;
    }
    return Value.initPtr(obj);
}

pub fn init_str(self: *Heap, str: []const u8) !Str {
    if (cy.string.isAstring(str)) {
        return self.init_astr(str);
    } else {
        return self.init_ustr(str);
    }
}

pub fn newStr(self: *Heap, str: []const u8) !Value {
    if (cy.string.isAstring(str)) {
        return self.newAstr(str);
    } else {
        return self.newUstr(str);
    }
}

pub fn newUstr(self: *Heap, str: []const u8) !Value {
    const obj = try newUstrUndef(self, str.len);
    const dst = obj.string.mutSlice();
    @memcpy(dst, str);
    return Value.initPtr(obj);
}

pub fn newAstr(self: *Heap, str: []const u8) !Value {
    const val = try newAstrUndef(self, str.len);
    const dst = val.string.mutSlice();
    @memcpy(dst, str);
    return Value.initPtr(val);
}

pub fn init_astr(self: *Heap, str: []const u8) !Str {
    const res = try init_astr_undef(self, str.len);
    const dst = res.mutSlice();
    @memcpy(dst, str);
    return res;
}

pub fn init_ustr(self: *Heap, str: []const u8) !Str {
    const res = try init_ustr_undef(self, str.len);
    const dst = res.mutSlice();
    @memcpy(dst, str);
    return res;
}

pub fn init_astr_concat(self: *Heap, str: []const u8, str2: []const u8) !Str {
    const res = try init_astr_undef(self, str.len + str2.len);
    const dst = res.mutSlice();
    @memcpy(dst[0..str.len], str);
    @memcpy(dst[str.len..], str2);
    return res;
}

pub fn newAstrConcat(self: *Heap, str: []const u8, str2: []const u8) !Value {
    const obj = try newAstrUndef(self, str.len + str2.len);
    const dst = obj.string.mutSlice();
    @memcpy(dst[0..str.len], str);
    @memcpy(dst[str.len..], str2);
    return Value.initPtr(obj);
}

pub fn init_astr_concat3(self: *Heap, str1: []const u8, str2: []const u8, str3: []const u8) !Str {
    const res = try init_astr_undef(self, str1.len + str2.len + str3.len);
    const dst = res.mutSlice();
    @memcpy(dst[0..str1.len], str1);
    @memcpy(dst[str1.len..str1.len+str2.len], str2);
    @memcpy(dst[str1.len+str2.len..], str3);
    return res;
}

pub fn newAstrConcat3(self: *Heap, str1: []const u8, str2: []const u8, str3: []const u8) !Value {
    const obj = try newAstrUndef(self, str1.len + str2.len + str3.len);
    const dst = obj.string.mutSlice();
    @memcpy(dst[0..str1.len], str1);
    @memcpy(dst[str1.len..str1.len+str2.len], str2);
    @memcpy(dst[str1.len+str2.len..], str3);
    return Value.initPtr(obj);
}

pub fn init_ustr_concat3(self: *Heap, str1: []const u8, str2: []const u8, str3: []const u8) !Str {
    const res = try init_ustr_undef(self, str1.len + str2.len + str3.len);
    const dst = res.mutSlice();
    @memcpy(dst[0..str1.len], str1);
    @memcpy(dst[str1.len..str1.len+str2.len], str2);
    @memcpy(dst[str1.len+str2.len..], str3);
    return res;
}

pub fn newUstrConcat3(self: *Heap, str1: []const u8, str2: []const u8, str3: []const u8) !Value {
    const obj = try newUstrUndef(self, str1.len + str2.len + str3.len);
    const dst = obj.string.mutSlice();
    @memcpy(dst[0..str1.len], str1);
    @memcpy(dst[str1.len..str1.len+str2.len], str2);
    @memcpy(dst[str1.len+str2.len..], str3);
    return Value.initPtr(obj);
}

pub fn init_ustr_concat(self: *Heap, str: []const u8, str2: []const u8) !Str {
    const res = try init_ustr_undef(self, str.len + str2.len);
    const dst = res.mutSlice();
    @memcpy(dst[0..str.len], str);
    @memcpy(dst[str.len..], str2);
    return res;
}

pub fn newUstrConcat(self: *Heap, str: []const u8, str2: []const u8) !Value {
    const obj = try newUstrUndef(self, str.len + str2.len);
    const dst = obj.string.mutSlice();
    @memcpy(dst[0..str.len], str);
    @memcpy(dst[str.len..], str2);
    return Value.initPtr(obj);
}

const Root = @This();

pub fn newBigObjectUndef(self: *cy.Heap, type_id: cy.TypeId, size: usize) !*cy.HeapObject {
    // First slot holds the typeId and rc.
    const obj = try allocBigObject(self, type_id, size);
    obj.object = .{
        .firstValue = undefined,
    };
    return obj;
}

pub fn lift(self: *cy.Heap, type_id: cy.TypeId, value: anytype) !Value {
    const val_t = @TypeOf(value);
    const object = try self.new_object_undef(type_id, @sizeOf(val_t));
    const ptr: *val_t = @ptrCast(object);
    ptr.* = value;
    return Value.initPtr(object);
}

pub fn new_object_undef(self: *cy.Heap, type_id: cy.TypeId, size: usize) !*cy.HeapObject {
    if (size <= 4 * 8) {
        return self.newSmallObjectUndef(type_id);
    } else {
        return self.newBigObjectUndef(type_id, size);
    }
}

pub fn init_astr_undef(self: *Heap, len: usize) !Str {
    const buf = try newStringBufferUndef(self, len);
    return .{
        .buf_ = @intFromPtr(buf),
        .ptr = @intFromPtr(&buf.raw_buffer.data),
        .len_ = len | (1 << 63),
    };
}

pub fn newAstrUndef(self: *Heap, len: usize) !*cy.HeapObject {
    const obj = try allocPoolObject(self, bt.Str);
    obj.string = try init_astr_undef(self, len);
    return obj;
}

pub fn init_ustr_undef(self: *Heap, len: usize) !Str {
    const buf = try newStringBufferUndef(self, len);
    return .{
        .buf_ = @intFromPtr(buf),
        .ptr = @intFromPtr(&buf.raw_buffer.data),
        .len_ = len,
    };
}

pub fn newUstrUndef(self: *Heap, len: usize) !*HeapObject {
    const obj = try allocPoolObject(self, bt.Str);
    obj.string = try init_ustr_undef(self, len);
    return obj;
}

/// Consumes `buf` +1.
pub fn newUstrSlice(self: *Heap, slice: []const u8, buf: ?*StrBuffer) !Value {
    const obj = try allocPoolObject(self, bt.Str);
    obj.string = .{
        .buf_ = @intFromPtr(buf),
        .ptr = @intFromPtr(slice.ptr),
        .len_ = slice.len,
    };
    return Value.initPtr(obj);
}

/// Consumes `buf` +1.
pub fn init_ustr_slice(self: *Heap, slice: []const u8, buf: ?*StrBuffer) !Str {
    _ = self;
    return .{
        .buf_ = @intFromPtr(buf),
        .ptr = @intFromPtr(slice.ptr),
        .len_ = slice.len,
    };
}

/// Consumes `buf` +1.
pub fn init_astr_slice(self: *Heap, slice: []const u8, buf: ?*StrBuffer) !Str {
    _ = self;
    return .{
        .buf_ = @intFromPtr(buf),
        .ptr = @intFromPtr(slice.ptr),
        .len_ = slice.len | (1 << 63),
    };
}

/// Consumes `buf` +1.
pub fn newAstrSlice(self: *Heap, slice: []const u8, buf: ?*StrBuffer) !Value {
    const obj = try allocPoolObject(self, bt.Str);
    obj.string = .{
        .buf_ = @intFromPtr(buf),
        .ptr = @intFromPtr(slice.ptr),
        .len_ = slice.len | (1 << 63),
    };
    return Value.initPtr(obj);
}

pub fn allocHostFuncUnion(self: *Heap, union_t: cy.TypeId, func: cy.ZHostFn) !Value {
    const obj = try allocPoolObject(self, union_t);
    obj.func = .{
        .kind = .host,
        .data = .{ .host = .{
            .ptr = @ptrCast(func),
        }},
    };
    return Value.initPtr(obj);
}

pub fn allocTccState(self: *Heap, state: *tcc.TCCState, optLib: ?*std.DynLib) !Value {
    const obj = try allocPoolObject(self, bt.TccState);
    obj.tccState = .{
        .state = state,
        .lib = undefined,
        .hasDynLib = false,
    };
    if (optLib) |lib| {
        obj.tccState.lib = lib;
        obj.tccState.hasDynLib = true;
    }
    return Value.initPtr(obj);
}

pub fn allocBool(self: *Heap, pred: bool) !Value {
    const obj = try allocPoolObject(self, bt.Bool);
    obj.integer = .{
        .val = @intFromBool(pred),
    };
    return Value.initPtr(obj);
}

pub fn allocInt(self: *Heap, val: i64) !Value {
    const obj = try allocPoolObject(self, bt.I64);
    obj.integer = .{
        .val = val,
    };
    return Value.initPtr(obj);
}

pub fn allocPointer(self: *Heap, type_id: cy.TypeId, ptr: ?*anyopaque) !Value {
    const obj = try allocPoolObject(self, type_id);
    obj.pointer = .{
        .ptr = ptr,
    };
    return Value.initPtr(obj);
}

pub fn allocBoxValue(self: *Heap, type_id: cy.TypeId, value: Value) !Value {
    const obj = try allocPoolObject(self, type_id);
    obj.object = .{
        .firstValue = value,
    };
    return Value.initPtr(obj);
}

pub fn allocObject2(self: *Heap, type_id: cy.TypeId, size: usize, fields: []const Value) !Value {
    if (size <= 4) {
        return self.allocObjectSmall(type_id, fields);
    } else {
        return self.allocObject(type_id, fields);
    }
}

/// Allocates an object outside of the object pool.
pub fn allocObject(self: *Heap, type_id: cy.TypeId, fields: []const Value) !Value {
    // First slot holds the typeId and rc.
    const obj = try allocBigObject(self, type_id, fields.len * @sizeOf(Value));
    obj.object = .{
        .firstValue = undefined,
    };
    const dst = obj.object.getValuesPtr();
    @memcpy(dst[0..fields.len], fields);

    return Value.initPtr(obj);
}

pub fn allocObjectSmall(self: *Heap, type_id: cy.TypeId, fields: []const Value) !Value {
    const obj = try allocPoolObject(self, type_id);
    obj.object = .{
        .firstValue = undefined,
    };

    const dst = obj.object.getValuesPtr();
    @memcpy(dst[0..fields.len], fields);

    return Value.initPtr(obj);
}

pub fn newSmallObjectUndef(self: *Heap, type_id: cy.TypeId) !*HeapObject {
    const obj = try allocPoolObject(self, type_id);
    obj.object = .{
        .firstValue = undefined,
    };
    return obj;
}

pub fn allocFunc(self: *Heap, union_t: cy.TypeId, func_ptr: Value) !Value {
    if (func_ptr.val >> 63 != 0) {
        const ptr = func_ptr.val & ~(@as(u64, 1) << 63);
        return allocHostFuncUnion(self, union_t, @ptrFromInt(ptr));
    } else {
        return allocBcFuncUnion(self, union_t, @ptrFromInt(func_ptr.val));
    }
}

// User bytes excludes the type header.
pub const MaxPoolObjectUserBytes = @sizeOf(PoolObject) - 8;

pub fn traceAlloc(self: *Heap, ptr: *HeapObject) !void {
    // log.tracev("alloc {*} {} {}", .{ptr, ptr.getTypeId(), vm.c.debugPc});
    const event_id = self.next_trace_event();
    // if (event_id == 7821) {
    //     // std.debug.print("DEBUG: {s}\n", .{vm.getType(type_id).name()});
    //     return error.Panic;
    // }
    self.objectTraceMap.put(self.alloc, ptr, .{
        .alloc_event = event_id,
        .alloc_ctx = self.c.ctx,
        .free_ctx = null,
        .free_type = null,
        .free_event = 0,
    }) catch cy.fatal();
}

pub fn getObjectAllocEvent(vm: *Heap, ptr: *HeapObject) u64 {
    return vm.objectTraceMap.get(ptr).?.alloc_event;
}

pub fn isObjectAlreadyFreed(self: *Heap, obj: *cy.HeapObject) bool {
    if (obj.isFreed()) {
        // Can check structId for pool objects since they are still in memory.
        return true;
    }
    if (self.objectTraceMap.get(obj)) |trace| {
        // For external objects check for trace entry.
        if (trace.free_ctx != null) {
            return true;
        }
    }
    return false;
}

test "Free object invalidation." {
    var vm: cy.VM = undefined;
    try vm.init(t.alloc);
    defer vm.deinit(false);

    // Invalidation only happens in trace mode.
    if (cy.Trace) {
        // Free pool object with no previous free slot invalidates object pointer.
        var obj = try allocPoolObject(&vm, 100);
        vm.heap.freePoolObject(obj);
        try t.eq(obj.getTypeId(), cy.NullId);

        // Free pool object with previous free slot invalidates object pointer.
        var obj1 = try allocPoolObject(&vm, 100);
        var obj2 = try allocPoolObject(&vm, 100);
        vm.heap.freePoolObject(obj1); // Previous slot is freed.
        vm.heap.freePoolObject(obj2);
        try t.eq(obj1.getTypeId(), cy.NullId);
        try t.eq(obj2.getTypeId(), cy.NullId);

        // Free external object invalidates object pointer.
        obj = try allocBigObject(&vm, 100, 40);
        vm.c.debugPc = 123;
        vm.heap.freeBigObject(obj, 40, false);
        try t.eq(cy.arc.isObjectAlreadyFreed(&vm, obj), true);
    }
}

test "heap internals." {
    try t.eq(@offsetOf(Heap, "c"), @offsetOf(vmc.ZHeap, "c"));

    try t.eq(24, @sizeOf(AsyncTask));
    if (cy.is32Bit) {
        try t.eq(@sizeOf(MapValue), 48);
        try t.eq(@sizeOf(MapIterator), 24);
        try t.eq(@sizeOf(Pointer), 16);
    } else {
        if (builtin.os.tag != .windows) {
            try t.eq(@sizeOf(MapValue), 48);
            try t.eq(@sizeOf(MapIterator), 16);
            try t.eq(@sizeOf(Pointer), 8);
        }
    }

    if (builtin.os.tag != .windows) { 
        try t.eq(@sizeOf(Func), 24);
        try t.eq(@sizeOf(Str), 24);
        try t.eq(@sizeOf(Object), 8);
        if (cy.hasFFI) {
            try t.eq(@sizeOf(TccState), 24);
        }
    }

    try t.eq(@sizeOf(PoolObject), 40);
    try t.eq(@alignOf(HeapObject), 8);
    try t.eq(@sizeOf(HeapPage), 40 * 102);
    try t.eq(@alignOf(HeapPage), 8);

    var func_u: Func = undefined;
    const c_func_u: *vmc.FuncUnion = @ptrCast(&func_u);
    try t.eq(@intFromPtr(&func_u.data.closure.pc), @intFromPtr(&c_func_u.data.closure.pc));
    try t.eq(&func_u.data.closure.numCaptured, &c_func_u.data.closure.numCaptured);
    try t.eq(@intFromPtr(&func_u.data.closure.firstCapturedVal), @intFromPtr(&c_func_u.data.closure.firstCapturedVal));
    try t.eq(@intFromPtr(&func_u.data.closure.firstCapturedVal), @intFromPtr(&func_u) + 24);
}
