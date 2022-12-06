const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const bindings = @import("bindings.zig");
const Value = cy.Value;
const debug = builtin.mode == .Debug;
const TraceEnabled = @import("build_options").trace;

const log = stdx.log.scoped(.vm);

const UseGlobalVM = true;
pub const TrackGlobalRC = builtin.mode != .ReleaseFast;

/// Reserved symbols known at comptime.
pub const ListS: StructId = 0;
pub const MapS: StructId = 1;
pub const ClosureS: StructId = 2;
pub const LambdaS: StructId = 3;
pub const StringS: StructId = 4;
pub const FiberS: StructId = 5;
pub const BoxS: StructId = 6;

var tempU8Buf: [256]u8 = undefined;

/// Accessing global vars is faster with direct addressing.
pub var gvm: VM = undefined;

pub fn getUserVM() UserVM {
    return UserVM{};
}

pub const VM = struct {
    alloc: std.mem.Allocator,
    parser: cy.Parser,
    compiler: cy.VMcompiler,

    /// [Eval context]

    /// Program counter. Pointer to the current instruction data in `ops`.
    pc: [*]const cy.OpData,
    /// Current stack frame ptr.
    framePtr: [*]Value,

    /// Value stack.
    stack: []Value,

    ops: []const cy.OpData,
    consts: []const cy.Const,
    strBuf: []const u8,

    /// Object heap pages.
    heapPages: cy.List(*HeapPage),
    heapFreeHead: ?*HeapObject,

    refCounts: if (TrackGlobalRC) usize else void,

    /// Symbol table used to lookup object methods.
    /// First, the SymbolId indexes into the table for a SymbolMap to lookup the final SymbolEntry by StructId.
    methodSyms: cy.List(SymbolMap),
    methodTable: std.AutoHashMapUnmanaged(MethodKey, SymbolEntry),
    methodSymExtras: cy.List([]const u8),

    /// Used to track which method symbols already exist. Only considers the name right now.
    methodSymSigs: std.StringHashMapUnmanaged(SymbolId),

    /// Regular function symbol table.
    funcSyms: cy.List(FuncSymbolEntry),
    funcSymSignatures: std.StringHashMapUnmanaged(SymbolId),
    funcSymNames: cy.List([]const u8),

    /// Struct fields symbol table.
    fieldSyms: cy.List(FieldSymbolMap),
    fieldSymSignatures: std.StringHashMapUnmanaged(SymbolId),

    /// Structs.
    structs: cy.List(Struct),
    structSignatures: std.StringHashMapUnmanaged(StructId),
    iteratorObjSym: SymbolId,
    pairIteratorObjSym: SymbolId,
    nextObjSym: SymbolId,

    globals: std.StringHashMapUnmanaged(SymbolId),

    u8Buf: cy.List(u8),

    stackTrace: StackTrace,

    debugTable: []const cy.OpDebug,
    panicMsg: []const u8,

    curFiber: *Fiber,
    mainFiber: Fiber,

    /// Local to be returned back to eval caller.
    /// 255 indicates no return value.
    endLocal: u8,

    trace: if (TraceEnabled) *TraceInfo else void,

    pub fn init(self: *VM, alloc: std.mem.Allocator) !void {
        self.* = .{
            .alloc = alloc,
            .parser = cy.Parser.init(alloc),
            .compiler = undefined,
            .ops = undefined,
            .consts = undefined,
            .strBuf = undefined,
            .stack = &.{},
            .heapPages = .{},
            .heapFreeHead = null,
            .pc = undefined,
            .framePtr = undefined,
            .methodSymExtras = .{},
            .methodSyms = .{},
            .methodSymSigs = .{},
            .methodTable = .{},
            .funcSyms = .{},
            .funcSymSignatures = .{},
            .funcSymNames = .{},
            .fieldSyms = .{},
            .fieldSymSignatures = .{},
            .structs = .{},
            .structSignatures = .{},
            .iteratorObjSym = undefined,
            .pairIteratorObjSym = undefined,
            .nextObjSym = undefined,
            .trace = undefined,
            .globals = .{},
            .u8Buf = .{},
            .stackTrace = .{},
            .debugTable = undefined,
            .refCounts = if (TrackGlobalRC) 0 else undefined,
            .panicMsg = "",
            .mainFiber = undefined,
            .curFiber = undefined,
            .endLocal = undefined,
        };
        // Pointer offset from gvm to avoid deoptimization.
        self.curFiber = &gvm.mainFiber;
        try self.compiler.init(self);

        // Perform decently sized allocation for hot data paths since the allocator
        // will likely use a more consistent allocation.
        // Also try to allocate them in the same bucket.
        try self.stackEnsureTotalCapacityPrecise(511);
        try self.methodTable.ensureTotalCapacity(self.alloc, 96);
        try self.funcSyms.ensureTotalCapacityPrecise(self.alloc, 255);
        try self.methodSyms.ensureTotalCapacityPrecise(self.alloc, 102);

        try self.parser.tokens.ensureTotalCapacityPrecise(alloc, 511);
        try self.parser.nodes.ensureTotalCapacityPrecise(alloc, 127);

        try self.structs.ensureTotalCapacityPrecise(alloc, 170);
        try self.fieldSyms.ensureTotalCapacityPrecise(alloc, 127);

        // Initialize heap.
        self.heapFreeHead = try self.growHeapPages(1);

        // Core bindings.
        try bindings.bindCore(self);
    }

    pub fn deinit(self: *VM) void {
        self.parser.deinit();
        self.compiler.deinit();
        self.alloc.free(self.stack);
        self.stack = &.{};

        self.methodSyms.deinit(self.alloc);
        self.methodSymExtras.deinit(self.alloc);
        self.methodSymSigs.deinit(self.alloc);
        self.methodTable.deinit(self.alloc);

        for (self.funcSyms.items()) |sym| {
            if (sym.entryT == .closure) {
                releaseObject(@ptrCast(*HeapObject, sym.inner.closure));
            }
        }
        self.funcSyms.deinit(self.alloc);
        self.funcSymSignatures.deinit(self.alloc);
        for (self.funcSymNames.items()) |name| {
            self.alloc.free(name);
        }
        self.funcSymNames.deinit(self.alloc);

        self.fieldSyms.deinit(self.alloc);
        self.fieldSymSignatures.deinit(self.alloc);

        for (self.heapPages.items()) |page| {
            self.alloc.destroy(page);
        }
        self.heapPages.deinit(self.alloc);

        self.structs.deinit(self.alloc);
        self.structSignatures.deinit(self.alloc);

        self.globals.deinit(self.alloc);
        self.u8Buf.deinit(self.alloc);
        self.stackTrace.deinit(self.alloc);
        self.alloc.free(self.panicMsg);
    }

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
    fn growHeapPages(self: *VM, numPages: usize) !*HeapObject {
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

    pub fn compile(self: *VM, src: []const u8) !cy.ByteCodeBuffer {
        var tt = stdx.debug.trace();
        const astRes = try self.parser.parse(src);
        if (astRes.has_error) {
            log.debug("Parse Error: {s}", .{astRes.err_msg});
            return error.ParseError;
        }
        tt.endPrint("parse");

        tt = stdx.debug.trace();
        const res = try self.compiler.compile(astRes);
        if (res.hasError) {
            log.debug("Compile Error: {s}", .{self.compiler.lastErr});
            return error.CompileError;
        }
        tt.endPrint("compile");

        return res.buf;
    }

    pub fn eval(self: *VM, src: []const u8) !Value {
        var tt = stdx.debug.trace();
        const astRes = try self.parser.parse(src);
        if (astRes.has_error) {
            log.debug("Parse Error: {s}", .{astRes.err_msg});
            return error.ParseError;
        }
        tt.endPrint("parse");

        tt = stdx.debug.trace();
        const res = try self.compiler.compile(astRes);
        if (res.hasError) {
            log.debug("Compile Error: {s}", .{self.compiler.lastErr});
            return error.CompileError;
        }
        tt.endPrint("compile");

        if (TraceEnabled) {
            try res.buf.dump();
            const numOps = comptime std.enums.values(cy.OpCode).len;
            self.trace.opCounts = try self.alloc.alloc(cy.OpCount, numOps);
            var i: u32 = 0;
            while (i < numOps) : (i += 1) {
                self.trace.opCounts[i] = .{
                    .code = i,
                    .count = 0,
                };
            }
            self.trace.totalOpCounts = 0;
            self.trace.numReleases = 0;
            self.trace.numReleaseAttempts = 0;
            self.trace.numForceReleases = 0;
            self.trace.numRetains = 0;
            self.trace.numRetainAttempts = 0;
            self.trace.numRetainCycles = 0;
            self.trace.numRetainCycleRoots = 0;
        }
        tt = stdx.debug.trace();
        defer {
            tt.endPrint("eval");
            if (TraceEnabled) {
                self.dumpInfo();
            }
        }

        return self.evalByteCode(res.buf);
    }

    pub fn dumpStats(self: *const VM) void {
        const S = struct {
            fn opCountLess(_: void, a: cy.OpCount, b: cy.OpCount) bool {
                return a.count > b.count;
            }
        };
        std.debug.print("total ops evaled: {}\n", .{self.trace.totalOpCounts});
        std.sort.sort(cy.OpCount, self.trace.opCounts, {}, S.opCountLess);
        var i: u32 = 0;

        const numOps = comptime std.enums.values(cy.OpCode).len;
        while (i < numOps) : (i += 1) {
            if (self.trace.opCounts[i].count > 0) {
                const op = std.meta.intToEnum(cy.OpCode, self.trace.opCounts[i].code) catch continue;
                std.debug.print("\t{s} {}\n", .{@tagName(op), self.trace.opCounts[i].count});
            }
        }
    }

    pub fn dumpInfo(self: *VM) void {
        const print = if (builtin.is_test) log.debug else std.debug.print;
        print("stack size: {}\n", .{self.stack.len});
        print("stack framePtr: {}\n", .{framePtrOffset(self.framePtr)});
        print("heap pages: {}\n", .{self.heapPages.len});

        // Dump object symbols.
        {
            print("obj syms:\n", .{});
            var iter = self.funcSymSignatures.iterator();
            while (iter.next()) |it| {
                print("\t{s}: {}\n", .{it.key_ptr.*, it.value_ptr.*});
            }
        }

        // Dump object fields.
        {
            print("obj fields:\n", .{});
            var iter = self.fieldSymSignatures.iterator();
            while (iter.next()) |it| {
                print("\t{s}: {}\n", .{it.key_ptr.*, it.value_ptr.*});
            }
        }
    }

    pub fn popStackFrameCold(self: *VM, comptime numRetVals: u2) linksection(".eval") void {
        _ = self;
        @setRuntimeSafety(debug);
        switch (numRetVals) {
            2 => {
                log.err("unsupported", .{});
            },
            3 => {
                // unreachable;
            },
            else => @compileError("Unsupported num return values."),
        }
    }

    fn popStackFrameLocal(self: *VM, pc: *usize, retLocal: u8, comptime numRetVals: u2) linksection(".eval") bool {
        @setRuntimeSafety(debug);
        _ = retLocal;
        _ = self;
        _ = pc;

        // If there are fewer return values than required from the function call, 
        // fill the missing slots with the none value.
        switch (numRetVals) {
            0 => @compileError("Not supported."),
            1 => @compileError("Not supported."),
            else => @compileError("Unsupported num return values."),
        }
    }

    fn prepareEvalCold(self: *VM, buf: cy.ByteCodeBuffer) void {
        @setCold(true);
        self.alloc.free(self.panicMsg);
        self.panicMsg = "";
        self.debugTable = buf.debugTable.items;
    }

    pub fn evalByteCode(self: *VM, buf: cy.ByteCodeBuffer) !Value {
        if (buf.ops.items.len == 0) {
            return error.NoEndOp;
        }

        @call(.{ .modifier = .never_inline }, self.prepareEvalCold, .{buf});

        // Set these last to hint location to cache before eval.
        self.pc = @ptrCast([*]const cy.OpData, buf.ops.items.ptr);
        try self.stackEnsureTotalCapacity(buf.mainStackSize);
        self.framePtr = @ptrCast([*]Value, self.stack.ptr);

        self.ops = buf.ops.items;
        self.consts = buf.mconsts;
        self.strBuf = buf.strBuf.items;

        try @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{});
        if (TraceEnabled) {
            log.info("main stack size: {}", .{buf.mainStackSize});
        }

        if (self.endLocal == 255) {
            return Value.initNone();
        } else {
            return self.stack[self.endLocal];
        }
    }

    fn sliceList(self: *VM, listV: Value, startV: Value, endV: Value) !Value {
        if (listV.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, listV.asPointer().?);
            if (obj.retainedCommon.structId == ListS) {
                const list = stdx.ptrCastAlign(*cy.List(Value), &obj.list.list);
                var start = @floatToInt(i32, startV.toF64());
                if (start < 0) {
                    start = @intCast(i32, list.len) + start + 1;
                }
                var end = @floatToInt(i32, endV.toF64());
                if (end < 0) {
                    end = @intCast(i32, list.len) + end + 1;
                }
                if (start < 0 or start > list.len) {
                    return self.panic("Index out of bounds");
                }
                if (end < start or end > list.len) {
                    return self.panic("Index out of bounds");
                }
                return self.allocList(list.buf[@intCast(u32, start)..@intCast(u32, end)]);
            } else {
                stdx.panic("expected list");
            }
        } else {
            stdx.panic("expected pointer");
        }
    }

    pub fn allocEmptyMap(self: *VM) !Value {
        const obj = try self.allocObject();
        obj.map = .{
            .structId = MapS,
            .rc = 1,
            .inner = .{
                .metadata = null,
                .entries = null,
                .size = 0,
                .cap = 0,
                .available = 0,
                .extra = 0,
            },
        };
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        return Value.initPtr(obj);
    }

    fn allocSmallObject(self: *VM, sid: StructId, offsets: []const cy.OpData, props: []const Value) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        obj.smallObject = .{
            .structId = sid,
            .rc = 1,
            .val0 = undefined,
            .val1 = undefined,
            .val2 = undefined,
            .val3 = undefined,
        };
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }

        const dst = @ptrCast([*]Value, &obj.smallObject.val0);
        for (offsets) |offset, i| {
            dst[offset.arg] = props[i];
        }

        const res = Value.initPtr(obj);
        return res;
    }

    fn allocMap(self: *VM, keyIdxs: []const cy.OpData, vals: []const Value) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        obj.map = .{
            .structId = MapS,
            .rc = 1,
            .inner = .{
                .metadata = null,
                .entries = null,
                .size = 0,
                .cap = 0,
                .available = 0,
                .extra = 0,
            },
        };
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }

        const inner = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
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

        const res = Value.initPtr(obj);
        return res;
    }

    fn freeObject(self: *VM, obj: *HeapObject) linksection(".eval") void {
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

    fn allocObject(self: *VM) !*HeapObject {
        if (self.heapFreeHead == null) {
            self.heapFreeHead = try self.growHeapPages(std.math.max(1, (self.heapPages.len * 15) / 10));
        }
        const ptr = self.heapFreeHead.?;
        if (ptr.freeSpan.len == 1) {
            // This is the only free slot, move to the next free span.
            self.heapFreeHead = ptr.freeSpan.next;
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
            return ptr;
        }
    }

    fn allocLambda(self: *VM, funcPc: usize, numParams: u8, numLocals: u8) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        obj.lambda = .{
            .structId = LambdaS,
            .rc = 1,
            .funcPc = @intCast(u32, funcPc),
            .numParams = numParams,
            .numLocals = numLocals,
        };
        if (TraceEnabled) {
            gvm.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        return Value.initPtr(obj);
    }

    fn allocClosure(self: *VM, framePtr: [*]Value, funcPc: usize, numParams: u8, numLocals: u8, capturedVals: []const cy.OpData) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        obj.closure = .{
            .structId = ClosureS,
            .rc = 1,
            .funcPc = @intCast(u32, funcPc),
            .numParams = numParams,
            .numLocals = numLocals,
            .numCaptured = @intCast(u8, capturedVals.len),
            .padding = undefined,
            .capturedVal0 = undefined,
            .capturedVal1 = undefined,
            .extra = undefined,
        };
        if (TraceEnabled) {
            gvm.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        switch (capturedVals.len) {
            0 => unreachable,
            1 => {
                obj.closure.capturedVal0 = framePtr[capturedVals[0].arg];
            },
            2 => {
                obj.closure.capturedVal0 = framePtr[capturedVals[0].arg];
                obj.closure.capturedVal1 = framePtr[capturedVals[1].arg];
            },
            3 => {
                obj.closure.capturedVal0 = framePtr[capturedVals[0].arg];
                obj.closure.capturedVal1 = framePtr[capturedVals[1].arg];
                obj.closure.extra.capturedVal2 = framePtr[capturedVals[2].arg];
            },
            else => {
                log.debug("Unsupported number of closure captured values: {}", .{capturedVals.len});
                return error.Panic;
            }
        }
        return Value.initPtr(obj);
    }

    pub fn allocOwnedString(self: *VM, str: []u8) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        obj.string = .{
            .structId = StringS,
            .rc = 1,
            .ptr = str.ptr,
            .len = str.len,
        };
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        return Value.initPtr(obj);
    }

    pub fn allocString(self: *VM, str: []const u8) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        const dupe = try self.alloc.dupe(u8, str);
        // log.debug("alloc str {*} {s}", .{dupe.ptr, dupe});
        obj.string = .{
            .structId = StringS,
            .rc = 1,
            .ptr = dupe.ptr,
            .len = dupe.len,
        };
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        return Value.initPtr(obj);
    }

    pub fn allocStringTemplate(self: *VM, strs: []const cy.OpData, vals: []const Value) !Value {
        @setRuntimeSafety(debug);

        const firstStr = self.valueAsString(Value.initRaw(gvm.consts[strs[0].arg].val));
        try self.u8Buf.resize(self.alloc, firstStr.len);
        std.mem.copy(u8, self.u8Buf.items(), firstStr);

        var writer = self.u8Buf.writer(self.alloc);
        for (vals) |val, i| {
            self.writeValueToString(writer, val);
            release(val);
            try self.u8Buf.appendSlice(self.alloc, self.valueAsString(Value.initRaw(gvm.consts[strs[i+1].arg].val)));
        }

        const obj = try self.allocObject();
        const buf = try self.alloc.alloc(u8, self.u8Buf.len);
        std.mem.copy(u8, buf, self.u8Buf.items());
        // log.debug("alloc str template {*} {s}", .{buf.ptr, buf});
        obj.string = .{
            .structId = StringS,
            .rc = 1,
            .ptr = buf.ptr,
            .len = buf.len,
        };
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        return Value.initPtr(obj);
    }

    fn allocStringConcat(self: *VM, str: []const u8, str2: []const u8) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        const buf = try self.alloc.alloc(u8, str.len + str2.len);
        std.mem.copy(u8, buf[0..str.len], str);
        std.mem.copy(u8, buf[str.len..], str2);
        obj.string = .{
            .structId = StringS,
            .rc = 1,
            .ptr = buf.ptr,
            .len = buf.len,
        };
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        return Value.initPtr(obj);
    }

    pub fn allocOwnedList(self: *VM, elems: []Value) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        obj.list = .{
            .structId = ListS,
            .rc = 1,
            .list = .{
                .ptr = elems.ptr,
                .len = elems.len,
                .cap = elems.len,
            },
            .nextIterIdx = 0,
        };
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        return Value.initPtr(obj);
    }

    fn allocList(self: *VM, elems: []const Value) linksection(".eval") !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        obj.list = .{
            .structId = ListS,
            .rc = 1,
            .list = .{
                .ptr = undefined,
                .len = 0,
                .cap = 0,
            },
            .nextIterIdx = 0,
        };
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        const list = stdx.ptrCastAlign(*cy.List(Value), &obj.list.list);
        try list.appendSlice(self.alloc, elems);
        return Value.initPtr(obj);
    }

    inline fn getLocal(self: *const VM, offset: u8) linksection(".eval") Value {
        @setRuntimeSafety(debug);
        return self.stack[self.framePtr + offset];
    }

    inline fn setLocal(self: *const VM, offset: u8, val: Value) linksection(".eval") void {
        @setRuntimeSafety(debug);
        self.stack[self.framePtr + offset] = val;
    }

    pub fn ensureStruct(self: *VM, name: []const u8) !StructId {
        const res = try self.structSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            return self.addStruct(name);
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn getStructFieldIdx(self: *const VM, sid: StructId, propName: []const u8) ?u32 {
        const fieldId = self.fieldSymSignatures.get(propName) orelse return null;
        const entry = self.fieldSyms.buf[fieldId];
        if (entry.mapT == .oneStruct) {
            if (entry.inner.oneStruct.id == sid) {
                return entry.inner.oneStruct.fieldIdx;
            }
        }
        return null;
    }

    pub inline fn getStruct(self: *const VM, name: []const u8) ?StructId {
        return self.structSignatures.get(name);
    }

    pub fn addStruct(self: *VM, name: []const u8) !StructId {
        const s = Struct{
            .name = name,
            .numFields = 0,
        };
        const vm = self.getVM();
        const id = @intCast(u32, vm.structs.len);
        try vm.structs.append(vm.alloc, s);
        try vm.structSignatures.put(vm.alloc, name, id);
        return id;
    }

    inline fn getVM(self: *VM) *VM {
        if (UseGlobalVM) {
            return &gvm;
        } else {
            return self;
        }
    }

    pub fn ensureGlobalFuncSym(self: *VM, ident: []const u8, funcSymName: []const u8) !void {
        const id = try self.ensureFuncSym(funcSymName);
        try self.globals.put(self.alloc, ident, id);
    }

    pub fn getGlobalFuncSym(self: *VM, ident: []const u8) ?SymbolId {
        return self.globals.get(ident);
    }

    pub inline fn getFuncSym(self: *const VM, name: []const u8) ?SymbolId {
        return self.funcSymSignatures.get(name);
    }
    
    pub fn ensureFuncSym(self: *VM, name: []const u8) !SymbolId {
        const res = try self.funcSymSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            const id = @intCast(u32, self.funcSyms.len);
            try self.funcSyms.append(self.alloc, .{
                .entryT = .none,
                .inner = undefined,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn ensureFieldSym(self: *VM, name: []const u8) !SymbolId {
        const res = try self.fieldSymSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            const id = @intCast(u32, self.fieldSyms.len);
            try self.fieldSyms.append(self.alloc, .{
                .mapT = .empty,
                .inner = undefined,
                .name = name,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn hasMethodSym(self: *const VM, sid: StructId, methodId: SymbolId) bool {
        const map = self.methodSyms.buf[methodId];
        if (map.mapT == .oneStruct) {
            return map.inner.oneStruct.id == sid;
        }
        return false;
    }

    pub fn ensureMethodSymKey(self: *VM, name: []const u8) !SymbolId {
        const res = try self.methodSymSigs.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            const id = @intCast(u32, self.methodSyms.len);
            try self.methodSyms.append(self.alloc, .{
                .mapT = .empty,
                .inner = undefined,
            });
            try self.methodSymExtras.append(self.alloc, name);
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub inline fn setFieldSym(self: *VM, sid: StructId, symId: SymbolId, offset: u32, isSmallObject: bool) void {
        self.fieldSyms.buf[symId].mapT = .oneStruct;
        self.fieldSyms.buf[symId].inner = .{
            .oneStruct = .{
                .id = sid,
                .fieldIdx = @intCast(u16, offset),
                .isSmallObject = isSmallObject,
            },
        };
    }

    pub inline fn setFuncSym(self: *VM, symId: SymbolId, sym: FuncSymbolEntry) void {
        self.funcSyms.buf[symId] = sym;
    }

    pub fn addMethodSym(self: *VM, id: StructId, symId: SymbolId, sym: SymbolEntry) !void {
        switch (self.methodSyms.buf[symId].mapT) {
            .empty => {
                self.methodSyms.buf[symId].mapT = .oneStruct;
                self.methodSyms.buf[symId].inner = .{
                    .oneStruct = .{
                        .id = id,
                        .sym = sym,
                    },
                };
            },
            .oneStruct => {
                // Convert to manyStructs.
                var key = MethodKey{
                    .structId = self.methodSyms.buf[symId].inner.oneStruct.id,
                    .methodId = symId,
                };
                self.methodTable.putAssumeCapacityNoClobber(key, self.methodSyms.buf[symId].inner.oneStruct.sym);

                key = .{
                    .structId = id,
                    .methodId = symId,
                };
                self.methodTable.putAssumeCapacityNoClobber(key, sym);

                self.methodSyms.buf[symId].mapT = .manyStructs;
                self.methodSyms.buf[symId].inner = .{
                    .manyStructs = .{
                        .mruStructId = id,
                        .mruSym = sym,
                    },
                };
            },
            .manyStructs => {
                const key = MethodKey{
                    .structId = id,
                    .methodId = symId,
                };
                self.methodTable.putAssumeCapacityNoClobber(key, sym);
            },
            // else => stdx.panicFmt("unsupported {}", .{self.methodSyms.buf[symId].mapT}),
        }
    }

    fn setIndex(self: *VM, left: Value, index: Value, right: Value) !void {
        @setRuntimeSafety(debug);
        if (left.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                ListS => {
                    const list = stdx.ptrCastAlign(*cy.List(Value), &obj.list.list);
                    const idx = @floatToInt(u32, index.toF64());
                    if (idx < list.len) {
                        list.buf[idx] = right;
                    } else {
                        // var i: u32 = @intCast(u32, list.val.items.len);
                        // try list.val.resize(self.alloc, idx + 1);
                        // while (i < idx) : (i += 1) {
                        //     list.val.items[i] = Value.none();
                        // }
                        // list.val.items[idx] = right;
                        return self.panic("Index out of bounds.");
                    }
                },
                MapS => {
                    const map = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
                    try map.put(self.alloc, self, index, right);
                },
                else => {
                    return stdx.panic("unsupported struct");
                },
            }
        } else {
            return stdx.panic("expected pointer");
        }
    }

    /// Assumes sign of index is preserved.
    fn getReverseIndex(self: *const VM, left: Value, index: Value) !Value {
        @setRuntimeSafety(debug);
        if (left.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                ListS => {
                    @setRuntimeSafety(debug);
                    const list = stdx.ptrCastAlign(*cy.List(Value), &obj.list.list);
                    const idx = @intCast(i32, list.len) + @floatToInt(i32, index.toF64());
                    if (idx < list.len) {
                        return list.buf[@intCast(u32, idx)];
                    } else {
                        return error.OutOfBounds;
                    }
                },
                MapS => {
                    @setRuntimeSafety(debug);
                    const map = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
                    const key = Value.initF64(index.toF64());
                    if (map.get(self, key)) |val| {
                        return val;
                    } else return Value.initNone();
                },
                else => {
                    stdx.panic("expected map or list");
                },
            }
        } else {
            return stdx.panic("expected pointer");
        }
    }

    fn getIndex(self: *VM, left: Value, index: Value) !Value {
        @setRuntimeSafety(debug);
        if (left.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                ListS => {
                    @setRuntimeSafety(debug);
                    const list = stdx.ptrCastAlign(*cy.List(Value), &obj.list.list);
                    const idx = @floatToInt(u32, index.toF64());
                    if (idx < list.len) {
                        return list.buf[idx];
                    } else {
                        return error.OutOfBounds;
                    }
                },
                MapS => {
                    @setRuntimeSafety(debug);
                    const map = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
                    if (map.get(self, index)) |val| {
                        return val;
                    } else return Value.initNone();
                },
                else => {
                    return stdx.panic("expected map or list");
                },
            }
        } else {
            return stdx.panic("expected pointer");
        }
    }

    fn panic(self: *VM, comptime msg: []const u8) error{Panic, OutOfMemory} {
        @setCold(true);
        @setRuntimeSafety(debug);
        self.panicMsg = try self.alloc.dupe(u8, msg);
        log.debug("{s}", .{self.panicMsg});
        return error.Panic;
    }

    pub fn getGlobalRC(self: *const VM) usize {
        if (TrackGlobalRC) {
            return self.refCounts;
        } else {
            stdx.panic("Enable TrackGlobalRC.");
        }
    }

    /// Performs an iteration over the heap pages to check whether there are retain cycles.
    pub fn checkMemory(self: *VM) !bool {
        var nodes: std.AutoHashMapUnmanaged(*HeapObject, RcNode) = .{};
        defer nodes.deinit(self.alloc);

        var cycleRoots: std.ArrayListUnmanaged(*HeapObject) = .{};
        defer cycleRoots.deinit(self.alloc);

        // No concept of root vars yet. Just report any existing retained objects.
        // First construct the graph.
        for (self.heapPages.items()) |page| {
            for (page.objects[1..]) |*obj| {
                if (obj.common.structId != NullId) {
                    try nodes.put(self.alloc, obj, .{
                        .visited = false,
                        .entered = false,
                    });
                }
            }
        }
        const S = struct {
            fn visit(alloc: std.mem.Allocator, graph: *std.AutoHashMapUnmanaged(*HeapObject, RcNode), cycleRoots_: *std.ArrayListUnmanaged(*HeapObject), obj: *HeapObject, node: *RcNode) bool {
                if (node.visited) {
                    return false;
                }
                if (node.entered) {
                    return true;
                }
                node.entered = true;

                switch (obj.retainedCommon.structId) {
                    ListS => {
                        const list = stdx.ptrCastAlign(*cy.List(Value), &obj.list.list);
                        for (list.items()) |it| {
                            if (it.isPointer()) {
                                const ptr = stdx.ptrCastAlign(*HeapObject, it.asPointer().?);
                                if (visit(alloc, graph, cycleRoots_, ptr, graph.getPtr(ptr).?)) {
                                    cycleRoots_.append(alloc, obj) catch stdx.fatal();
                                    return true;
                                }
                            }
                        }
                    },
                    else => {
                    },
                }
                node.entered = false;
                node.visited = true;
                return false;
            }
        };
        var iter = nodes.iterator();
        while (iter.next()) |*entry| {
            if (S.visit(self.alloc, &nodes, &cycleRoots, entry.key_ptr.*, entry.value_ptr)) {
                if (TraceEnabled) {
                    self.trace.numRetainCycles = 1;
                    self.trace.numRetainCycleRoots = @intCast(u32, cycleRoots.items.len);
                }
                for (cycleRoots.items) |root| {
                    // Force release.
                    self.forceRelease(root);
                }
                return false;
            }
        }
        return true;
    }

    pub inline fn retainObject(self: *const VM, obj: *HeapObject) linksection(".eval") void {
        @setRuntimeSafety(debug);
        obj.retainedCommon.rc += 1;
        log.debug("retain {} {}", .{obj.getUserTag(), obj.retainedCommon.rc});
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
    }

    pub inline fn retain(self: *const VM, val: Value) linksection(".eval") void {
        @setRuntimeSafety(debug);
        if (TraceEnabled) {
            self.trace.numRetainAttempts += 1;
        }
        if (val.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, val.asPointer());
            obj.retainedCommon.rc += 1;
            log.debug("retain {} {}", .{obj.getUserTag(), obj.retainedCommon.rc});
            if (TrackGlobalRC) {
                gvm.refCounts += 1;
            }
            if (TraceEnabled) {
                self.trace.numRetains += 1;
            }
        }
    }

    pub fn forceRelease(self: *VM, obj: *HeapObject) void {
        if (TraceEnabled) {
            self.trace.numForceReleases += 1;
        }
        switch (obj.retainedCommon.structId) {
            ListS => {
                const list = stdx.ptrCastAlign(*cy.List(Value), &obj.list.list);
                list.deinit(self.alloc);
                self.freeObject(obj);
                if (TrackGlobalRC) {
                    gvm.refCounts -= obj.retainedCommon.rc;
                }
            },
            MapS => {
                const map = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
                map.deinit(self.alloc);
                self.freeObject(obj);
                if (TrackGlobalRC) {
                    gvm.refCounts -= obj.retainedCommon.rc;
                }
            },
            else => {
                return stdx.panic("unsupported struct type");
            },
        }
    }

    fn setFieldRelease(self: *VM, recv: Value, fieldId: SymbolId, val: Value) linksection(".eval") !void {
        @setRuntimeSafety(debug);
        if (recv.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, recv.asPointer());
            const symMap = self.fieldSyms.buf[fieldId];
            switch (symMap.mapT) {
                .oneStruct => {
                    @setRuntimeSafety(debug);
                    if (obj.common.structId == symMap.inner.oneStruct.id) {
                        if (symMap.inner.oneStruct.isSmallObject) {
                            release(obj.smallObject.getValuesPtr()[symMap.inner.oneStruct.fieldIdx]);
                            obj.smallObject.getValuesPtr()[symMap.inner.oneStruct.fieldIdx] = val;
                        } else {
                            stdx.panic("TODO: big object");
                        }
                    } else {
                        stdx.panic("TODO: set field fallback");
                    }
                },
                .manyStructs => {
                    @setRuntimeSafety(debug);
                    stdx.fatal();
                },
                .empty => {
                    @setRuntimeSafety(debug);
                    stdx.panic("TODO: set field fallback");
                },
            } 
        } else {
            try self.setFieldNotObjectError();
        }
    }

    fn setField(self: *VM, recv: Value, fieldId: SymbolId, val: Value) linksection(".eval") !void {
        @setRuntimeSafety(debug);
        if (recv.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, recv.asPointer());
            const symMap = self.fieldSyms.buf[fieldId];
            switch (symMap.mapT) {
                .oneStruct => {
                    @setRuntimeSafety(debug);
                    if (obj.common.structId == symMap.inner.oneStruct.id) {
                        if (symMap.inner.oneStruct.isSmallObject) {
                            obj.smallObject.getValuesPtr()[symMap.inner.oneStruct.fieldIdx] = val;
                        } else {
                            stdx.panic("TODO: big object");
                        }
                    } else {
                        stdx.panic("TODO: set field fallback");
                    }
                },
                .manyStructs => {
                    @setRuntimeSafety(debug);
                    stdx.fatal();
                },
                .empty => {
                    @setRuntimeSafety(debug);
                    stdx.panic("TODO: set field fallback");
                },
            } 
        } else {
            try self.setFieldNotObjectError();
        }
    }

    fn getFieldMissingSymbolError(self: *VM) error{Panic, OutOfMemory} {
        @setCold(true);
        return self.panic("Field not found in value.");
    }

    fn setFieldNotObjectError(self: *VM) !void {
        @setCold(true);
        return self.panic("Can't assign to value's field since the value is not an object.");
    }

    fn getAndRetainField(self: *VM, symId: SymbolId, recv: Value) linksection(".eval") !Value {
        @setRuntimeSafety(debug);
        if (recv.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, recv.asPointer());
            const symMap = self.fieldSyms.buf[symId];
            switch (symMap.mapT) {
                .oneStruct => {
                    if (obj.retainedCommon.structId == symMap.inner.oneStruct.id) {
                        if (symMap.inner.oneStruct.isSmallObject) {
                            const val = obj.smallObject.getValuesConstPtr()[symMap.inner.oneStruct.fieldIdx];
                            self.retain(val);
                            return val;
                        } else {
                            stdx.panic("TODO: big object");
                        }
                    } else {
                        return self.getAndRetainFieldFallback(obj, symMap.name);
                    }
                },
                .manyStructs => {
                    @setRuntimeSafety(debug);
                    stdx.fatal();
                },
                .empty => {
                    return self.getAndRetainFieldFallback(obj, symMap.name);
                },
            } 
        } else {
            return self.getFieldMissingSymbolError();
        }
    }

    fn getField(self: *VM, symId: SymbolId, recv: Value) linksection(".eval") !Value {
        @setRuntimeSafety(debug);
        if (recv.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, recv.asPointer());
            const symMap = self.fieldSyms.buf[symId];
            switch (symMap.mapT) {
                .oneStruct => {
                    if (obj.common.structId == symMap.inner.oneStruct.id) {
                        if (symMap.inner.oneStruct.isSmallObject) {
                            return obj.smallObject.getValuesConstPtr()[symMap.inner.oneStruct.fieldIdx];
                        } else {
                            stdx.panic("TODO: big object");
                        }
                    } else {
                        return self.getFieldOther(obj, symMap.name);
                    }
                },
                .manyStructs => {
                    @setRuntimeSafety(debug);
                    stdx.fatal();
                },
                .empty => {
                    return self.getFieldOther(obj, symMap.name);
                },
                // else => {
                //     // stdx.panicFmt("unsupported {}", .{symMap.mapT});
                //     unreachable;
                // },
            } 
        } else {
            return self.getFieldMissingSymbolError();
        }
    }

    fn getAndRetainFieldFallback(self: *const VM, obj: *const HeapObject, name: []const u8) linksection(".eval") Value {
        @setCold(true);
        if (obj.common.structId == MapS) {
            const map = stdx.ptrCastAlign(*const MapInner, &obj.map.inner);
            if (map.getByString(self, name)) |val| {
                self.retain(val);
                return val;
            } else return Value.initNone();
        } else {
            log.debug("Missing symbol for object: {} {s}", .{obj.common.structId, name});
            return Value.initNone();
        }
    }

    fn getFieldOther(self: *const VM, obj: *const HeapObject, name: []const u8) linksection(".eval") Value {
        @setCold(true);
        if (obj.common.structId == MapS) {
            const map = stdx.ptrCastAlign(*const MapInner, &obj.map.inner);
            if (map.getByString(self, name)) |val| {
                return val;
            } else return Value.initNone();
        } else {
            log.debug("Missing symbol for object: {}", .{obj.common.structId});
            return Value.initNone();
        }
    }

    /// startLocal points to the first arg in the current stack frame.
    fn callSym(self: *VM, pc: *[*]const cy.OpData, framePtr: *[*]Value, symId: SymbolId, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") !void {
        @setRuntimeSafety(debug);
        const sym = self.funcSyms.buf[symId];
        switch (sym.entryT) {
            .nativeFunc1 => {
                @setRuntimeSafety(debug);
                pc.* += 4;
                // const newFramePtr = self.framePtr + startLocal;
                const newFramePtr = framePtr.* + startLocal;
                // const res = sym.inner.nativeFunc1(undefined, @ptrCast([*]const Value, &self.stack[newFramePtr+2]), numArgs);
                const res = sym.inner.nativeFunc1(undefined, @ptrCast([*]const Value, newFramePtr + 2), numArgs);
                if (reqNumRetVals == 1) {
                    if (@ptrToInt(newFramePtr) >= @ptrToInt(self.stack.ptr) + 8*self.stack.len) {
                    // if (newFramePtr >= self.stack.len) {
                        // Already made state changes, so grow stack here instead of
                        // returning StackOverflow.
                        // try self.stackGrowTotalCapacity(newFramePtr);
                        try self.stackGrowTotalCapacity((@ptrToInt(newFramePtr) - @ptrToInt(self.stack.ptr))/8);
                    }
                    // self.stack[newFramePtr] = res;
                    newFramePtr[0] = res;
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            // Nop.
                        },
                        1 => stdx.panic("not possible"),
                        2 => {
                            stdx.panic("unsupported require 2 ret vals");
                        },
                        3 => {
                            stdx.panic("unsupported require 3 ret vals");
                        },
                    }
                }
            },
            .func => {
                @setRuntimeSafety(debug);
                log.debug("req stack: {} {}", .{framePtrOffset(self.framePtr) + startLocal + sym.inner.func.numLocals, self.stack.len});
                if (@ptrToInt(framePtr.* + startLocal + sym.inner.func.numLocals) >= @ptrToInt(gvm.stack.ptr) + 8 * self.stack.len) {
                    return error.StackOverflow;
                }

                const retInfo = buildReturnInfo(pcOffset(pc.* + 4), (@ptrToInt(framePtr.*) - @ptrToInt(gvm.stack.ptr))/8, reqNumRetVals, true);
                // const retInfo = self.buildReturnInfo2(self.pc + 3, reqNumRetVals, true);
                pc.* = toPc(sym.inner.func.pc);
                // self.framePtr = self.framePtr + startLocal;
                framePtr.* += startLocal;

                // self.stack[self.framePtr + 1] = retInfo;
                framePtr.*[1] = retInfo;
            },
            .closure => {
                @setRuntimeSafety(debug);
                if (@ptrToInt(framePtr.* + startLocal + sym.inner.closure.numLocals) >= @ptrToInt(gvm.stack.ptr) + 8 * self.stack.len) {
                    return error.StackOverflow;
                }

                const retInfo = buildReturnInfo(pcOffset(pc.* + 4), (@ptrToInt(framePtr.*) - @ptrToInt(gvm.stack.ptr))/8, reqNumRetVals, true);
                pc.* = toPc(sym.inner.closure.funcPc);
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;

                // Copy over captured vars to new call stack locals.
                if (sym.inner.closure.numCaptured <= 3) {
                    const src = @ptrCast([*]Value, &sym.inner.closure.capturedVal0)[0..sym.inner.closure.numCaptured];
                    std.mem.copy(Value, framePtr.*[numArgs + 2..numArgs + 2 + sym.inner.closure.numCaptured], src);
                } else {
                    stdx.panic("unsupported closure > 3 captured args.");
                }
            },
            else => {
                return self.panic("unsupported callsym");
            },
        }
    }

    inline fn callSymEntry(self: *VM, pc: *[*]const cy.OpData, framePtr: *[*]Value, sym: SymbolEntry, obj: *HeapObject, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") !void {
        @setRuntimeSafety(debug);
        switch (sym.entryT) {
            .func => {
                @setRuntimeSafety(debug);
                // if (self.framePtr + startLocal + sym.inner.func.numLocals >= self.stack.len) {
                //     return error.StackOverflow;
                // }
                if (@ptrToInt(framePtr.* + startLocal + sym.inner.func.numLocals) >= @ptrToInt(gvm.stack.ptr) + 8 * self.stack.len) {
                    return error.StackOverflow;
                }

                // const retInfo = self.buildReturnInfo2(self.pc + 3, reqNumRetVals, true);
                // const retInfo = self.buildReturnInfo2(pc.*, reqNumRetVals, true);
                const retInfo = buildReturnInfo(pcOffset(pc.*), (@ptrToInt(framePtr.*) - @ptrToInt(gvm.stack.ptr)) / 8, reqNumRetVals, true);
                pc.* = toPc(sym.inner.func.pc);
                // self.framePtr = self.framePtr + startLocal;
                framePtr.* += startLocal;
                // self.stack[self.framePtr + 1] = retInfo;
                framePtr.*[1] = retInfo;
            },
            .nativeFunc1 => {
                @setRuntimeSafety(debug);
                // self.pc += 3;
                // const newFramePtr = self.framePtr + startLocal;

                // const framePtrSave = self.framePtr;
                // gvm.framePtr = self.framePtr + startLocal;
                gvm.framePtr = framePtr.* + startLocal;
                gvm.pc = pc.*;
                const res = sym.inner.nativeFunc1(undefined, obj, @ptrCast([*]const Value, gvm.framePtr + 2), numArgs);
                pc.* = gvm.pc;
                if (reqNumRetVals == 1) {
                    gvm.framePtr[0] = res;
                    // gvm.framePtr = framePtrSave;
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            // Nop.
                            // gvm.framePtr = framePtrSave;
                        },
                        1 => stdx.panic("not possible"),
                        2 => {
                            stdx.panic("unsupported require 2 ret vals");
                        },
                        3 => {
                            stdx.panic("unsupported require 3 ret vals");
                        },
                    }
                }
            },
            .nativeFunc2 => {
                @setRuntimeSafety(debug);
                // self.pc += 3;
                const newFramePtr = self.framePtr + startLocal;
                gvm.pc = pc.*;
                const res = sym.inner.nativeFunc2(undefined, obj, @ptrCast([*]const Value, newFramePtr + 2), numArgs);
                pc.* = gvm.pc;
                if (reqNumRetVals == 2) {
                    self.stack[newFramePtr] = res.left;
                    self.stack[newFramePtr+1] = res.right;
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            // Nop.
                        },
                        1 => unreachable,
                        2 => {
                            unreachable;
                        },
                        3 => {
                            unreachable;
                        },
                    }
                }
            },
            // else => {
            //     // stdx.panicFmt("unsupported {}", .{sym.entryT});
            //     unreachable;
            // },
        }
    }

    fn getCallObjSym(self: *VM, obj: *HeapObject, symId: SymbolId) linksection(".eval") ?SymbolEntry {
        @setRuntimeSafety(debug);
        const map = self.methodSyms.buf[symId];
        switch (map.mapT) {
            .oneStruct => {
                @setRuntimeSafety(debug);
                if (obj.retainedCommon.structId == map.inner.oneStruct.id) {
                    return map.inner.oneStruct.sym;
                } else return null;
            },
            .manyStructs => {
                @setRuntimeSafety(debug);
                if (map.inner.manyStructs.mruStructId == obj.retainedCommon.structId) {
                    return map.inner.manyStructs.mruSym;
                } else {
                    // Compiler wants to inline this function, but it causes a noticeable slow down in for.cy benchmark.
                    const sym = @call(.{ .modifier = .never_inline }, self.methodTable.get, .{.{ .structId = obj.retainedCommon.structId, .methodId = symId }}) orelse return null;
                    self.methodSyms.buf[symId].inner.manyStructs = .{
                        .mruStructId = obj.retainedCommon.structId,
                        .mruSym = sym,
                    };
                    return sym;
                }
            },
            .empty => {
                @setRuntimeSafety(debug);
                return null;
            },
            // else => {
            //     unreachable;
            //     // stdx.panicFmt("unsupported {}", .{map.mapT});
            // },
        } 
    }

    /// Stack layout: arg0, arg1, ..., receiver
    /// numArgs includes the receiver.
    /// Return new pc to avoid deoptimization.
    fn callObjSym(self: *VM, pc: usize, framePtr: [*]Value, recv: Value, symId: SymbolId, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") !PcFramePtr {
        @setRuntimeSafety(debug);
        if (recv.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, recv.asPointer().?);
            const map = self.methodSyms.buf[symId];
            switch (map.mapT) {
                .oneStruct => {
                    @setRuntimeSafety(debug);
                    if (obj.retainedCommon.structId == map.inner.oneStruct.id) {
                        return try @call(.{.modifier = .never_inline }, callSymEntryNoInline, .{pc, framePtr, map.inner.oneStruct.sym, obj, startLocal, numArgs, reqNumRetVals});
                    } else return self.panic("Symbol does not exist for receiver.");
                },
                .manyStructs => {
                    @setRuntimeSafety(debug);
                    if (map.inner.manyStructs.mruStructId == obj.retainedCommon.structId) {
                        return try @call(.{ .modifier = .never_inline }, callSymEntryNoInline, .{pc, framePtr, map.inner.manyStructs.mruSym, obj, startLocal, numArgs, reqNumRetVals});
                    } else {
                        const sym = self.methodTable.get(.{ .structId = obj.retainedCommon.structId, .methodId = symId }) orelse {
                            log.debug("Symbol does not exist for receiver.", .{});
                            stdx.fatal();
                        };
                        self.methodSyms.buf[symId].inner.manyStructs = .{
                            .mruStructId = obj.retainedCommon.structId,
                            .mruSym = sym,
                        };
                        return try @call(.{ .modifier = .never_inline }, callSymEntryNoInline, .{pc, framePtr, sym, obj, startLocal, numArgs, reqNumRetVals});
                    }
                },
                .empty => {
                    @setRuntimeSafety(debug);
                    return try @call(.{ .modifier = .never_inline }, callObjSymFallback, .{pc, framePtr, obj, symId, startLocal, numArgs, reqNumRetVals});
                },
                // else => {
                //     unreachable;
                //     // stdx.panicFmt("unsupported {}", .{map.mapT});
                // },
            } 
        }
        return PcFramePtr{
            .pc = pc,
            .framePtr = undefined,
        };
    }

    pub fn getStackTrace(self: *const VM) *const StackTrace {
        return &self.stackTrace;
    }

    fn indexOfDebugSym(self: *const VM, pc: usize) ?usize {
        for (self.debugTable) |sym, i| {
            if (sym.pc == pc) {
                return i;
            }
        }
        return null;
    }

    fn computeLinePos(self: *const VM, loc: u32, outLine: *u32, outCol: *u32) void {
        var line: u32 = 0;
        var lineStart: u32 = 0;
        for (self.compiler.tokens) |token| {
            if (token.tag() == .new_line) {
                line += 1;
                lineStart = token.pos() + 1;
                continue;
            }
            if (token.pos() == loc) {
                outLine.* = line;
                outCol.* = loc - lineStart;
                return;
            }
        }
    }

    pub fn buildStackTrace(self: *VM) !void {
        @setCold(true);
        self.stackTrace.deinit(self.alloc);
        var frames: std.ArrayListUnmanaged(StackFrame) = .{};

        var framePtr = framePtrOffset(self.framePtr);
        var pc = pcOffset(self.pc);
        while (true) {
            const idx = self.indexOfDebugSym(pc) orelse return error.NoDebugSym;
            const sym = self.debugTable[idx];

            if (sym.frameLoc == NullId) {
                const node = self.compiler.nodes[sym.loc];
                var line: u32 = undefined;
                var col: u32 = undefined;
                self.computeLinePos(self.compiler.tokens[node.start_token].pos(), &line, &col);
                try frames.append(self.alloc, .{
                    .name = "main",
                    .line = line,
                    .col = col,
                });
                break;
            } else {
                const frameNode = self.compiler.nodes[sym.frameLoc];
                const func = self.compiler.funcDecls[frameNode.head.func.decl_id];
                const name = self.compiler.src[func.name.start..func.name.end];

                const node = self.compiler.nodes[sym.loc];
                var line: u32 = undefined;
                var col: u32 = undefined;
                self.computeLinePos(self.compiler.tokens[node.start_token].pos(), &line, &col);
                try frames.append(self.alloc, .{
                    .name = name,
                    .line = line,
                    .col = col,
                });
                pc = self.stack[framePtr + 1].retInfo.pc;
                framePtr = self.stack[framePtr + 1].retInfo.framePtr;
            }
        }

        self.stackTrace.frames = try frames.toOwnedSlice(self.alloc);
    }

    pub fn valueAsString(self: *const VM, val: Value) []const u8 {
        @setRuntimeSafety(debug);
        if (val.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, val.asPointer().?);
            return obj.string.ptr[0..obj.string.len];
        } else {
            // Assume const string.
            const slice = val.asConstStr();
            return self.strBuf[slice.start..slice.end];
        }
    }

    /// Conversion goes into a temporary buffer. Must use the result before a subsequent call.
    pub fn valueToTempString(self: *const VM, val: Value) linksection(".eval2") []const u8 {
        if (val.isNumber()) {
            const f = val.asF64();
            if (Value.floatCanBeInteger(f)) {
                return std.fmt.bufPrint(&tempU8Buf, "{d:.0}", .{f}) catch stdx.fatal();
            } else {
                return std.fmt.bufPrint(&tempU8Buf, "{d:.10}", .{f}) catch stdx.fatal();
            }
        } else {
            if (val.isPointer()) {
                const obj = stdx.ptrCastAlign(*HeapObject, val.asPointer().?);
                if (obj.common.structId == StringS) {
                    return obj.string.ptr[0..obj.string.len];
                } else if (obj.common.structId == ListS) {
                    return std.fmt.bufPrint(&tempU8Buf, "List ({})", .{obj.list.list.len}) catch stdx.fatal();
                } else if (obj.common.structId == MapS) {
                    return std.fmt.bufPrint(&tempU8Buf, "Map ({})", .{obj.map.inner.size}) catch stdx.fatal();
                } else {
                    return self.structs.buf[obj.common.structId].name;
                }
            } else {
                switch (val.getTag()) {
                    cy.TagBoolean => {
                        if (val.asBool()) return "true" else return "false";
                    },
                    cy.TagNone => return "none",
                    cy.TagConstString => {
                        // Convert into heap string.
                        const slice = val.asConstStr();
                        return self.strBuf[slice.start..slice.end];
                    },
                    else => {
                        log.debug("unexpected tag {}", .{val.getTag()});
                        stdx.fatal();
                    },
                }
            }
        }
    }

    fn writeValueToString(self: *const VM, writer: anytype, val: Value) void {
        if (val.isNumber()) {
            const f = val.asF64();
            if (Value.floatIsSpecial(f)) {
                std.fmt.format(writer, "{}", .{f}) catch stdx.fatal();
            } else {
                if (Value.floatCanBeInteger(f)) {
                    std.fmt.format(writer, "{}", .{@floatToInt(u64, f)}) catch stdx.fatal();
                } else {
                    std.fmt.format(writer, "{d:.10}", .{f}) catch stdx.fatal();
                }
            }
        } else {
            if (val.isPointer()) {
                const obj = stdx.ptrCastAlign(*HeapObject, val.asPointer().?);
                if (obj.common.structId == StringS) {
                    const str = obj.string.ptr[0..obj.string.len];
                    _ = writer.write(str) catch stdx.fatal();
                } else {
                    log.debug("unexpected struct {}", .{obj.common.structId});
                    stdx.fatal();
                }
            } else {
                switch (val.getTag()) {
                    cy.TagBoolean => {
                        if (val.asBool()) {
                            _ = writer.write("true") catch stdx.fatal();
                        } else {
                            _ = writer.write("false") catch stdx.fatal();
                        }
                    },
                    cy.TagNone => {
                        _ = writer.write("none") catch stdx.fatal();
                    },
                    cy.TagConstString => {
                        // Convert into heap string.
                        const slice = val.asConstStr();
                        _ = writer.write(self.strBuf[slice.start..slice.end]) catch stdx.fatal();
                    },
                    else => {
                        log.debug("unexpected tag {}", .{val.getTag()});
                        stdx.fatal();
                    },
                }
            }
        }
    }

    pub inline fn stackEnsureUnusedCapacity(self: *VM, unused: u32) linksection(".eval") !void {
        @setRuntimeSafety(debug);
        if (@ptrToInt(self.framePtr) + 8 * unused >= @ptrToInt(self.stack.ptr + self.stack.len)) {
            try self.stackGrowTotalCapacity((@ptrToInt(self.framePtr) + 8 * unused) / 8);
        }
    }

    inline fn stackEnsureTotalCapacity(self: *VM, newCap: usize) linksection(".eval") !void {
        @setRuntimeSafety(debug);
        if (newCap > self.stack.len) {
            try self.stackGrowTotalCapacity(newCap);
        }
    }

    pub fn stackEnsureTotalCapacityPrecise(self: *VM, newCap: usize) !void {
        @setRuntimeSafety(debug);
        if (newCap > self.stack.len) {
            try self.stackGrowTotalCapacityPrecise(newCap);
        }
    }

    pub fn stackGrowTotalCapacity(self: *VM, newCap: usize) !void {
        @setRuntimeSafety(debug);
        var betterCap = newCap;
        while (true) {
            betterCap +|= betterCap / 2 + 8;
            if (betterCap >= newCap) {
                break;
            }
        }
        if (self.alloc.resize(self.stack, betterCap)) {
            self.stack.len = betterCap;
        } else {
            self.stack = try self.alloc.realloc(self.stack, betterCap);
        }
    }

    pub fn stackGrowTotalCapacityPrecise(self: *VM, newCap: usize) !void {
        @setRuntimeSafety(debug);
        if (self.alloc.resize(self.stack, newCap)) {
            self.stack.len = newCap;
        } else {
            self.stack = try self.alloc.realloc(self.stack, newCap);
        }
    }
};

pub fn releaseObject(obj: *HeapObject) linksection(".eval") void {
    if (builtin.mode == .Debug or builtin.is_test) {
        if (obj.retainedCommon.structId == NullId) {
            stdx.panic("object already freed.");
        }
    }
    obj.retainedCommon.rc -= 1;
    log.debug("release {} {}", .{obj.getUserTag(), obj.retainedCommon.rc});
    if (TrackGlobalRC) {
        gvm.refCounts -= 1;
    }
    if (TraceEnabled) {
        gvm.trace.numReleases += 1;
        gvm.trace.numReleaseAttempts += 1;
    }
    if (obj.retainedCommon.rc == 0) {
        @call(.{ .modifier = .never_inline }, freeObject, .{obj});
    }
}

fn freeObject(obj: *HeapObject) linksection(".eval") void {
    log.debug("free {}", .{obj.getUserTag()});
    switch (obj.retainedCommon.structId) {
        ListS => {
            const list = stdx.ptrCastAlign(*cy.List(Value), &obj.list.list);
            for (list.items()) |it| {
                release(it);
            }
            list.deinit(gvm.alloc);
            gvm.freeObject(obj);
        },
        MapS => {
            const map = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
            var iter = map.iterator();
            while (iter.next()) |entry| {
                release(entry.key);
                release(entry.value);
            }
            map.deinit(gvm.alloc);
            gvm.freeObject(obj);
        },
        ClosureS => {
            if (obj.closure.numCaptured <= 3) {
                const src = @ptrCast([*]Value, &obj.closure.capturedVal0)[0..obj.closure.numCaptured];
                for (src) |capturedVal| {
                    release(capturedVal);
                }
                gvm.freeObject(obj);
            } else {
                stdx.panic("unsupported");
            }
        },
        LambdaS => {
            gvm.freeObject(obj);
        },
        StringS => {
            gvm.alloc.free(obj.string.ptr[0..obj.string.len]);
            gvm.freeObject(obj);
        },
        FiberS => {
            releaseFiberStack(&obj.fiber);
            gvm.freeObject(obj);
        },
        BoxS => {
            release(obj.box.val);
            gvm.freeObject(obj);
        },
        else => {
            // Struct deinit.
            if (builtin.mode == .Debug) {
                // Check range.
                if (obj.retainedCommon.structId >= gvm.structs.len) {
                    log.debug("unsupported struct type {}", .{obj.retainedCommon.structId});
                    stdx.fatal();
                }
            }
            const numFields = gvm.structs.buf[obj.retainedCommon.structId].numFields;
            if (numFields <= 4) {
                for (obj.smallObject.getValuesConstPtr()[0..numFields]) |child| {
                    release(child);
                }
                gvm.freeObject(obj);
            } else {
                log.debug("unsupported release big object", .{});
                stdx.fatal();
            }
        },
    }
}

pub fn release(val: Value) linksection(".eval") void {
    @setRuntimeSafety(debug);
    if (TraceEnabled) {
        gvm.trace.numReleaseAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = stdx.ptrCastAlign(*HeapObject, val.asPointer().?);
        if (builtin.mode == .Debug or builtin.is_test) {
            if (obj.retainedCommon.structId == NullId) {
                stdx.panic("object already freed.");
            }
        }
        obj.retainedCommon.rc -= 1;
        log.debug("release {} {}", .{val.getUserTag(), obj.retainedCommon.rc});
        if (TrackGlobalRC) {
            gvm.refCounts -= 1;
        }
        if (TraceEnabled) {
            gvm.trace.numReleases += 1;
        }
        if (obj.retainedCommon.rc == 0) {
            @call(.{ .modifier = .never_inline }, freeObject, .{obj});
        }
    }
}

fn evalBitwiseAnd(left: Value, right: Value) Value {
    @setRuntimeSafety(debug);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asI32() & @floatToInt(i32, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalGreaterOrEqual(left: cy.Value, right: cy.Value) cy.Value {
    return Value.initBool(left.toF64() >= right.toF64());
}

fn evalGreater(left: cy.Value, right: cy.Value) cy.Value {
    return Value.initBool(left.toF64() > right.toF64());
}

fn evalLessOrEqual(left: cy.Value, right: cy.Value) cy.Value {
    return Value.initBool(left.toF64() <= right.toF64());
}

fn evalLessFallback(left: cy.Value, right: cy.Value) linksection(".eval") cy.Value {
    @setRuntimeSafety(debug);
    @setCold(true);
    return Value.initBool(left.toF64() < right.toF64());
}

fn evalCompareNotFallback(left: cy.Value, right: cy.Value) linksection(".eval") cy.Value {
    @setRuntimeSafety(debug);
    @setCold(true);
    if (left.isPointer()) {
        const obj = stdx.ptrAlignCast(*HeapObject, left.asPointer().?);
        if (obj.common.structId == StringS) {
            if (right.isString()) {
                const str = obj.string.ptr[0..obj.string.len];
                return Value.initBool(!std.mem.eql(u8, str, gvm.valueAsString(right)));
            } else return Value.True;
        } else {
            if (right.isPointer()) {
                return Value.initBool(@ptrCast(*anyopaque, obj) != right.asPointer().?);
            } else return Value.True;
        }
    } else {
        switch (left.getTag()) {
            cy.TagNone => return Value.initBool(!right.isNone()),
            cy.TagBoolean => return Value.initBool(left.asBool() != right.toBool()),
            cy.TagConstString => {
                if (right.isString()) {
                    const slice = left.asConstStr();
                    const str = gvm.strBuf[slice.start..slice.end];
                    return Value.initBool(!std.mem.eql(u8, str, gvm.valueAsString(right)));
                } return Value.True;
            },
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalCompareFallback(left: Value, right: Value) linksection(".eval") Value {
    @setRuntimeSafety(debug);
    @setCold(true);
    if (left.isPointer()) {
        const obj = stdx.ptrCastAlign(*HeapObject, left.asPointer().?);
        if (obj.common.structId == StringS) {
            if (right.isString()) {
                const str = obj.string.ptr[0..obj.string.len];
                return Value.initBool(std.mem.eql(u8, str, gvm.valueAsString(right)));
            } else return Value.False;
        } else {
            if (right.isPointer()) {
                return Value.initBool(@ptrCast(*anyopaque, obj) == right.asPointer().?);
            } else return Value.False;
        }
    } else {
        switch (left.getTag()) {
            cy.TagNone => return Value.initBool(right.isNone()),
            cy.TagBoolean => return Value.initBool(left.asBool() == right.toBool()),
            cy.TagConstString => {
                if (right.isString()) {
                    const slice = left.asConstStr();
                    const str = gvm.strBuf[slice.start..slice.end];
                    return Value.initBool(std.mem.eql(u8, str, gvm.valueAsString(right)));
                } return Value.False;
            },
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalMinusFallback(left: Value, right: Value) linksection(".eval") Value {
    @setCold(true);
    if (left.isPointer()) {
        return Value.initF64(left.toF64() - right.toF64());
    } else {
        switch (left.getTag()) {
            cy.TagBoolean => {
                if (left.asBool()) {
                    return Value.initF64(1 - right.toF64());
                } else {
                    return Value.initF64(-right.toF64());
                }
            },
            cy.TagNone => return Value.initF64(-right.toF64()),
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalPower(left: cy.Value, right: cy.Value) cy.Value {
    @setRuntimeSafety(debug);
    if (left.isNumber()) {
        return Value.initF64(std.math.pow(f64, left.asF64(), right.toF64()));
    } else {
        switch (left.getTag()) {
            cy.TagBoolean => {
                if (left.asBool()) {
                    return Value.initF64(1);
                } else {
                    return Value.initF64(0);
                }
            },
            cy.TagNone => return Value.initF64(0),
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalDivide(left: cy.Value, right: cy.Value) cy.Value {
    @setRuntimeSafety(debug);
    if (left.isNumber()) {
        return Value.initF64(left.asF64() / right.toF64());
    } else {
        switch (left.getTag()) {
            cy.TagBoolean => {
                if (left.asBool()) {
                    return Value.initF64(1.0 / right.toF64());
                } else {
                    return Value.initF64(0);
                }
            },
            cy.TagNone => return Value.initF64(0),
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalMod(left: cy.Value, right: cy.Value) cy.Value {
    @setRuntimeSafety(debug);
    if (left.isNumber()) {
        return Value.initF64(std.math.mod(f64, left.asF64(), right.toF64()) catch std.math.nan_f64);
    } else {
        switch (left.getTag()) {
            cy.TagBoolean => {
                if (left.asBool()) {
                    const rightf = right.toF64();
                    if (rightf > 0) {
                        return Value.initF64(1);
                    } else if (rightf == 0) {
                        return Value.initF64(std.math.nan_f64);
                    } else {
                        return Value.initF64(rightf + 1);
                    }
                } else {
                    if (right.toF64() != 0) {
                        return Value.initF64(0);
                    } else {
                        return Value.initF64(std.math.nan_f64);
                    }
                }
            },
            cy.TagNone => {
                if (right.toF64() != 0) {
                    return Value.initF64(0);
                } else {
                    return Value.initF64(std.math.nan_f64);
                }
            },
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalMultiply(left: cy.Value, right: cy.Value) cy.Value {
    @setRuntimeSafety(debug);
    if (left.isNumber()) {
        return Value.initF64(left.asF64() * right.toF64());
    } else {
        switch (left.getTag()) {
            cy.TagBoolean => {
                if (left.asBool()) {
                    return Value.initF64(right.toF64());
                } else {
                    return Value.initF64(0);
                }
            },
            cy.TagNone => return Value.initF64(0),
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalAddFallback(left: cy.Value, right: cy.Value) linksection(".eval") !cy.Value {
    @setRuntimeSafety(debug);
    @setCold(true);
    if (left.isNumber()) {
        return Value.initF64(left.asF64() + try toF64OrPanic(right));
    } else {
        if (left.isPointer()) {
            const obj = stdx.ptrAlignCast(*cy.HeapObject, left.asPointer().?);
            if (obj.common.structId == cy.StringS) {
                const str = obj.string.ptr[0..obj.string.len];
                return gvm.allocStringConcat(str, gvm.valueToTempString(right)) catch stdx.fatal();
            } else return gvm.panic("Cannot add struct instance.");
        } else {
            switch (left.getTag()) {
                cy.TagBoolean => {
                    if (left.asBool()) {
                        return Value.initF64(1 + right.toF64());
                    } else {
                        return Value.initF64(right.toF64());
                    }
                },
                cy.TagNone => return Value.initF64(right.toF64()),
                cy.TagError => stdx.fatal(),
                cy.TagConstString => {
                    // Convert into heap string.
                    const slice = left.asConstStr();
                    const str = gvm.strBuf[slice.start..slice.end];
                    return gvm.allocStringConcat(str, gvm.valueToTempString(right)) catch stdx.fatal();
                },
                else => stdx.panic("unexpected tag"),
            }
        }
    }
}

fn toF64OrPanic(val: Value) linksection(".eval") !f64 {
    @setRuntimeSafety(debug);
    if (val.isNumber()) {
        return val.asF64();
    } else {
        return try @call(.{ .modifier = .never_inline }, convToF64OrPanic, .{val});
    }
}

fn convToF64OrPanic(val: Value) linksection(".eval") !f64 {
    if (val.isPointer()) {
        const obj = stdx.ptrAlignCast(*cy.HeapObject, val.asPointer().?);
        if (obj.common.structId == cy.StringS) {
            const str = obj.string.ptr[0..obj.string.len];
            return std.fmt.parseFloat(f64, str) catch 0;
        } else return gvm.panic("Cannot convert struct to number");
    } else {
        switch (val.getTag()) {
            cy.TagNone => return 0,
            cy.TagBoolean => return if (val.asBool()) 1 else 0,
            else => stdx.panicFmt("unexpected tag {}", .{val.getTag()}),
        }
    }
}

fn evalNeg(val: Value) Value {
    @setRuntimeSafety(debug);
    // @setCold(true);
    if (val.isNumber()) {
        return Value.initF64(-val.asF64());
    } else {
        switch (val.getTag()) {
            cy.TagNone => return Value.initF64(0),
            cy.TagBoolean => {
                if (val.asBool()) {
                    return Value.initF64(-1);
                } else {
                    return Value.initF64(0);
                }
            },
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalNot(val: cy.Value) cy.Value {
    if (val.isNumber()) {
        return Value.False;
    } else {
        switch (val.getTag()) {
            cy.TagNone => return Value.True,
            cy.TagBoolean => return Value.initBool(!val.asBool()),
            else => stdx.panic("unexpected tag"),
        }
    }
}

const NullByteId = std.math.maxInt(u8);
const NullId = std.math.maxInt(u32);

const String = packed struct {
    structId: StructId,
    rc: u32,
    ptr: [*]u8,
    len: usize,
};

const Lambda = packed struct {
    structId: StructId,
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    /// Includes locals and return info. Does not include params.
    numLocals: u8,
};

const Closure = packed struct {
    structId: StructId,
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    numCaptured: u8,
    /// Includes locals, captured vars, and return info. Does not include params.
    numLocals: u8,
    padding: u8,
    capturedVal0: Value,
    capturedVal1: Value,
    extra: packed union {
        capturedVal2: Value,
        ptr: ?*anyopaque,
    },
};

pub const MapInner = cy.ValueMap;
const Map = packed struct {
    structId: StructId,
    rc: u32,
    inner: packed struct {
        metadata: ?[*]u64,
        entries: ?[*]cy.ValueMapEntry,
        size: u32,
        cap: u32,
        available: u32,
        extra: u32,
    },
    // nextIterIdx: u32,
};

const Box = packed struct {
    structId: StructId,
    rc: u32,
    val: Value,
};

const Fiber = packed struct {
    structId: StructId,
    rc: u32,
    prevFiber: ?*Fiber,
    stackPtr: [*]Value,
    stackLen: u32,
    pc: u32,
    framePtr: [*]Value,
};

const List = packed struct {
    structId: StructId,
    rc: u32,
    list: packed struct {
        ptr: [*]Value,
        cap: usize,
        len: usize,
    },
    nextIterIdx: u32,
};

// Keep it just under 4kb page.
const HeapPage = struct {
    objects: [102]HeapObject,
};

const HeapObjectId = u32;

/// Total of 40 bytes per object. If structs are bigger they are allocated on the gpa.
pub const HeapObject = packed union {
    common: packed struct {
        structId: StructId,
    },
    freeSpan: packed struct {
        structId: StructId,
        len: u32,
        start: *HeapObject,
        next: ?*HeapObject,
    },
    retainedCommon: packed struct {
        structId: StructId,
        rc: u32,
    },
    list: List,
    fiber: Fiber,
    map: Map,
    closure: Closure,
    lambda: Lambda,
    string: String,
    smallObject: packed struct {
        structId: StructId,
        rc: u32,
        val0: Value,
        val1: Value,
        val2: Value,
        val3: Value,

        pub inline fn getValuesConstPtr(self: *const @This()) [*]const Value {
            return @ptrCast([*]const Value, &self.val0);
        }

        pub inline fn getValuesPtr(self: *@This()) [*]Value {
            return @ptrCast([*]Value, &self.val0);
        }
    },
    object: packed struct {
        structId: StructId,
        rc: u32,
        ptr: *anyopaque,
        val0: Value,
        val1: Value,
        val2: Value,
    },
    box: Box,

    pub fn getUserTag(self: *const HeapObject) cy.ValueUserTag {
        switch (self.common.structId) {
            cy.ListS => return .list,
            cy.MapS => return .map,
            cy.StringS => return .string,
            cy.ClosureS => return .closure,
            cy.LambdaS => return .lambda,
            cy.FiberS => return .fiber,
            else => {
                return .object;
            },
        }
    }
};

const SymbolMapType = enum {
    oneStruct, // TODO: rename to one.
    // two,
    // ring, // Sorted mru, up to 8 syms.
    manyStructs, // TODO: rename to many.
    empty,
};

/// Keeping this small is better for function calls. TODO: Reduce size.
/// Secondary symbol data should be moved to `methodSymExtras`.
const SymbolMap = struct {
    mapT: SymbolMapType,
    inner: union {
        oneStruct: struct {
            id: StructId,
            sym: SymbolEntry,
        },
        // two: struct {
        // },
        // ring: struct {
        // },
        manyStructs: struct {
            mruStructId: StructId,
            mruSym: SymbolEntry,
        },
    },
};

const FieldSymbolMap = struct {
    mapT: SymbolMapType,
    inner: union {
        oneStruct: struct {
            id: StructId,
            fieldIdx: u16,
            isSmallObject: bool,
        },
    },
    name: []const u8,
};

test "Internals." {
    try t.eq(@alignOf(VM), 8);
    try t.eq(@alignOf(SymbolMap), 8);
    try t.eq(@sizeOf(SymbolMap), 40);
    try t.eq(@sizeOf(SymbolEntry), 16);
    try t.eq(@sizeOf(MapInner), 32);
    try t.eq(@sizeOf(HeapObject), 40);
    try t.eq(@alignOf(HeapObject), 8);
    try t.eq(@sizeOf(HeapPage), 40 * 102);
    try t.eq(@alignOf(HeapPage), 8);
    try t.eq(@sizeOf(FuncSymbolEntry), 16);

    try t.eq(@sizeOf(Struct), 24);
    try t.eq(@sizeOf(FieldSymbolMap), 32);
}

const SymbolEntryType = enum {
    func,
    nativeFunc1,
    nativeFunc2,
};

pub const SymbolEntry = struct {
    entryT: SymbolEntryType,
    inner: packed union {
        nativeFunc1: std.meta.FnPtr(fn (*UserVM, *anyopaque, [*]const Value, u8) Value),
        nativeFunc2: std.meta.FnPtr(fn (*UserVM, *anyopaque, [*]const Value, u8) cy.ValuePair),
        func: packed struct {
            pc: u32,
            /// Includes function params, locals, and return info slot.
            numLocals: u32,
        },
    },

    pub fn initFunc(pc: u32, numLocals: u32) SymbolEntry {
        return .{
            .entryT = .func,
            .inner = .{
                .func = .{
                    .pc = pc,
                    .numLocals = numLocals,
                },
            },
        };
    }

    pub fn initNativeFunc1(func: std.meta.FnPtr(fn (*UserVM, *anyopaque, [*]const Value, u8) Value)) SymbolEntry {
        return .{
            .entryT = .nativeFunc1,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    fn initNativeFunc2(func: std.meta.FnPtr(fn (*UserVM, *anyopaque, [*]const Value, u8) cy.ValuePair)) SymbolEntry {
        return .{
            .entryT = .nativeFunc2,
            .inner = .{
                .nativeFunc2 = func,
            },
        };
    }
};

const FuncSymbolEntryType = enum {
    nativeFunc1,
    func,
    closure,
    none,
};

pub const FuncSymbolEntry = struct {
    entryT: FuncSymbolEntryType,
    inner: packed union {
        nativeFunc1: std.meta.FnPtr(fn (*UserVM, [*]const Value, u8) Value),
        func: packed struct {
            // pc: usize,
            pc: u32,
            /// Includes locals, and return info slot. Does not include params.
            numLocals: u32,
        },
        closure: *Closure,
    },

    pub fn initNativeFunc1(func: std.meta.FnPtr(fn (*UserVM, [*]const Value, u8) Value)) FuncSymbolEntry {
        return .{
            .entryT = .nativeFunc1,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    pub fn initFunc(pc: u32, numLocals: u32) FuncSymbolEntry {
        return .{
            .entryT = .func,
            .inner = .{
                .func = .{
                    .pc = pc,
                    .numLocals = numLocals,
                },
            },
        };
    }
};

pub const StructId = u32;

const Struct = struct {
    name: []const u8,
    numFields: u32,
};

// const StructSymbol = struct {
//     name: []const u8,
// };

const SymbolId = u32;

pub const TraceInfo = struct {
    opCounts: []OpCount = &.{},
    totalOpCounts: u32,
    numRetains: u32,
    numRetainAttempts: u32,
    numReleases: u32,
    numReleaseAttempts: u32,
    numForceReleases: u32,
    numRetainCycles: u32,
    numRetainCycleRoots: u32,
};

pub const OpCount = struct {
    code: u32,
    count: u32,
};

const RcNode = struct {
    visited: bool,
    entered: bool,
};

const Root = @This();

/// Force users to use the global vm instance (to avoid deoptimization).
pub const UserVM = struct {
    dummy: u32 = 0,

    pub fn init(_: UserVM, alloc: std.mem.Allocator) !void {
        try gvm.init(alloc);
    }

    pub fn deinit(_: UserVM) void {
        gvm.deinit();
    }

    pub fn setTrace(_: UserVM, trace: *TraceInfo) void {
        if (!TraceEnabled) {
            return;
        }
        gvm.trace = trace;
    }

    pub fn getStackTrace(_: UserVM) *const StackTrace {
        return gvm.getStackTrace();
    }

    pub fn getPanicMsg(_: UserVM) []const u8 {
        return gvm.panicMsg;
    }

    pub fn dumpPanicStackTrace(_: UserVM) void {
        if (builtin.is_test) {
            log.debug("panic: {s}", .{gvm.panicMsg});
        } else {
            std.debug.print("panic: {s}\n", .{gvm.panicMsg});
        }
        const trace = gvm.getStackTrace();
        trace.dump();
    }

    pub fn dumpInfo(_: UserVM) void {
        gvm.dumpInfo();
    }

    pub fn dumpStats(_: UserVM) void {
        gvm.dumpStats();
    }

    pub fn fillUndefinedStackSpace(_: UserVM, val: Value) void {
        std.mem.set(Value, gvm.stack, val);
    }

    pub inline fn release(_: UserVM, val: Value) void {
        Root.release(val);
    }

    pub inline fn getGlobalRC(_: UserVM) usize {
        return gvm.getGlobalRC();
    }

    pub inline fn checkMemory(_: UserVM) !bool {
        return gvm.checkMemory();
    }

    pub inline fn compile(_: UserVM, src: []const u8) !cy.ByteCodeBuffer {
        return gvm.compile(src);
    }

    pub inline fn eval(_: UserVM, src: []const u8) !Value {
        return gvm.eval(src);
    }

    pub inline fn allocString(_: UserVM, str: []const u8) !Value {
        return gvm.allocString(str);
    }

    pub inline fn valueAsString(_: UserVM, val: Value) []const u8 {
        return gvm.valueAsString(val);
    }
};

/// To reduce the amount of code inlined in the hot loop, handle StackOverflow at the top and resume execution.
/// This is also the entry way for native code to call into the VM without deoptimizing the hot loop.
pub fn evalLoopGrowStack() linksection(".eval") error{StackOverflow, OutOfMemory, Panic, OutOfBounds, NoDebugSym, End}!void {
    @setRuntimeSafety(debug);
    while (true) {
        @call(.{ .modifier = .always_inline }, evalLoop, .{}) catch |err| {
            if (err == error.StackOverflow) {
                log.debug("grow stack", .{});
                try gvm.stackGrowTotalCapacity(gvm.stack.len + 1);
                continue;
            } else if (err == error.End) {
                return;
            } else if (err == error.Panic) {
                try @call(.{ .modifier = .never_inline }, gvm.buildStackTrace, .{});
                return error.Panic;
            } else return err;
        };
        return;
    }
}

fn evalLoop() linksection(".eval") error{StackOverflow, OutOfMemory, Panic, OutOfBounds, NoDebugSym, End}!void {
    @setRuntimeSafety(debug);
    var pc = gvm.pc;
    var framePtr = gvm.framePtr;
    defer {
        gvm.pc = pc;
        gvm.framePtr = framePtr;
    }

    while (true) {
        if (TraceEnabled) {
            const op = pc[0].code;
            gvm.trace.opCounts[@enumToInt(op)].count += 1;
            gvm.trace.totalOpCounts += 1;
        }
        if (builtin.mode == .Debug) {
            dumpEvalOp(pc);
        }
        switch (pc[0].code) {
            .true => {
                @setRuntimeSafety(debug);
                framePtr[pc[1].arg] = Value.True;
                pc += 2;
                continue;
            },
            .false => {
                @setRuntimeSafety(debug);
                framePtr[pc[1].arg] = Value.False;
                pc += 2;
                continue;
            },
            .none => {
                @setRuntimeSafety(debug);
                framePtr[pc[1].arg] = Value.None;
                pc += 2;
                continue;
            },
            .constOp => {
                @setRuntimeSafety(debug);
                framePtr[pc[2].arg] = Value.initRaw(gvm.consts[pc[1].arg].val);
                pc += 3;
                continue;
            },
            .constI8 => {
                @setRuntimeSafety(debug);
                framePtr[pc[2].arg] = Value.initF64(@intToFloat(f64, @bitCast(i8, pc[1].arg)));
                pc += 3;
                continue;
            },
            .release => {
                const local = pc[1].arg;
                pc += 2;
                // TODO: Inline if heap object.
                @call(.{ .modifier = .never_inline }, release, .{framePtr[local]});
                continue;
            },
            .field => {
                const fieldId = pc[1].arg;
                const left = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const recv = framePtr[left];
                const val = try gvm.getField(fieldId, recv);
                framePtr[dst] = val;
                continue;
            },
            .copyRetainSrc => {
                @setRuntimeSafety(debug);
                const src = pc[1].arg;
                const dst = pc[2].arg;
                pc += 3;
                const val = framePtr[src];
                framePtr[dst] = val;
                gvm.retain(val);
                continue;
            },
            .jumpNotCond => {
                @setRuntimeSafety(debug);
                const jump = @ptrCast(*const align(1) u16, pc + 1).*;
                const cond = framePtr[pc[3].arg];
                const condVal = if (cond.isBool()) b: {
                    break :b cond.asBool();
                } else b: {
                    break :b @call(.{ .modifier = .never_inline }, cond.toBool, .{});
                };
                if (!condVal) {
                    pc += jump;
                } else {
                    pc += 4;
                }
                continue;
            },
            .neg => {
                @setRuntimeSafety(debug);
                const val = framePtr[pc[1].arg];
                // gvm.stack[gvm.framePtr + pc[2].arg] = if (val.isNumber())
                //     Value.initF64(-val.asF64())
                // else 
                    // @call(.{ .modifier = .never_inline }, evalNegFallback, .{val});
                framePtr[pc[2].arg] = evalNeg(val);
                pc += 3;
                continue;
            },
            .not => {
                @setRuntimeSafety(debug);
                const val = framePtr[pc[1].arg];
                framePtr[pc[2].arg] = evalNot(val);
                pc += 3;
                continue;
            },
            .compareNot => {
                @setRuntimeSafety(debug);
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initBool(left.asF64() != right.asF64());
                } else {
                    framePtr[pc[3].arg] = @call(.{.modifier = .never_inline }, evalCompareNotFallback, .{left, right});
                }
                pc += 4;
                continue;
            },
            .compare => {
                @setRuntimeSafety(debug);
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initBool(left.asF64() == right.asF64());
                } else {
                    framePtr[pc[3].arg] = @call(.{.modifier = .never_inline }, evalCompareFallback, .{left, right});
                }
                pc += 4;
                continue;
            },
            // .lessNumber => {
            //     @setRuntimeSafety(debug);
            //     const left = gvm.stack[gvm.framePtr + pc[1].arg];
            //     const right = gvm.stack[gvm.framePtr + pc[2].arg];
            //     const dst = pc[3].arg;
            //     pc += 4;
            //     gvm.stack[gvm.framePtr + dst] = Value.initBool(left.asF64() < right.asF64());
            //     continue;
            // },
            .less => {
                @setRuntimeSafety(debug);
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = if (Value.bothNumbers(left, right))
                    Value.initBool(left.asF64() < right.asF64())
                else
                    @call(.{ .modifier = .never_inline }, evalLessFallback, .{left, right});
                pc += 4;
                continue;
            },
            .greater => {
                @setRuntimeSafety(debug);
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = evalGreater(srcLeft, srcRight);
                continue;
            },
            .lessEqual => {
                @setRuntimeSafety(debug);
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = evalLessOrEqual(srcLeft, srcRight);
                continue;
            },
            .greaterEqual => {
                @setRuntimeSafety(debug);
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = evalGreaterOrEqual(srcLeft, srcRight);
                continue;
            },
            // .addNumber => {
            //     @setRuntimeSafety(debug);
            //     const srcLeft = gvm.stack[gvm.framePtr + pc[1].arg];
            //     const srcRight = gvm.stack[gvm.framePtr + pc[2].arg];
            //     const dstLocal = pc[3].arg;
            //     pc += 4;
            //     gvm.stack[gvm.framePtr + dstLocal] = evalAddNumber(srcLeft, srcRight);
            //     continue;
            // },
            .add => {
                @setRuntimeSafety(debug);
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initF64(left.asF64() + right.asF64());
                } else {
                    framePtr[pc[3].arg] = try @call(.{ .modifier = .never_inline }, evalAddFallback, .{ left, right });
                }
                pc += 4;
                continue;
            },
            .minus => {
                @setRuntimeSafety(debug);
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = if (Value.bothNumbers(left, right))
                    Value.initF64(left.asF64() - right.asF64())
                else @call(.{ .modifier = .never_inline }, evalMinusFallback, .{left, right});
                pc += 4;
                continue;
            },
            .stringTemplate => {
                @setRuntimeSafety(debug);
                const startLocal = pc[1].arg;
                const exprCount = pc[2].arg;
                const dst = pc[3].arg;
                const strCount = exprCount + 1;
                const strs = pc[4 .. 4 + strCount];
                pc += 4 + strCount;
                const vals = framePtr[startLocal .. startLocal + exprCount];
                const res = try @call(.{ .modifier = .never_inline }, gvm.allocStringTemplate, .{strs, vals});
                framePtr[dst] = res;
                continue;
            },
            .list => {
                @setRuntimeSafety(debug);
                const startLocal = pc[1].arg;
                const numElems = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const elems = framePtr[startLocal..startLocal + numElems];
                const list = try gvm.allocList(elems);
                framePtr[dst] = list;
                continue;
            },
            .mapEmpty => {
                @setRuntimeSafety(debug);
                const dst = pc[1].arg;
                pc += 2;
                framePtr[dst] = try gvm.allocEmptyMap();
                continue;
            },
            .structSmall => {
                @setRuntimeSafety(debug);
                const sid = pc[1].arg;
                const startLocal = pc[2].arg;
                const numProps = pc[3].arg;
                const dst = pc[4].arg;
                const offsets = pc[5..5+numProps];
                pc += 5 + numProps;

                const props = framePtr[startLocal .. startLocal + numProps];
                framePtr[dst] = try gvm.allocSmallObject(sid, offsets, props);
                continue;
            },
            .map => {
                @setRuntimeSafety(debug);
                const startLocal = pc[1].arg;
                const numEntries = pc[2].arg;
                const dst = pc[3].arg;
                const keyIdxes = pc[4..4+numEntries];
                pc += 4 + numEntries;
                const vals = framePtr[startLocal .. startLocal + numEntries];
                framePtr[dst] = try gvm.allocMap(keyIdxes, vals);
                continue;
            },
            .slice => {
                @setRuntimeSafety(debug);
                const list = framePtr[pc[1].arg];
                const start = framePtr[pc[2].arg];
                const end = framePtr[pc[3].arg];
                const dst = pc[4].arg;
                pc += 5;
                framePtr[dst] = try gvm.sliceList(list, start, end);
                continue;
            },
            .setInitN => {
                @setRuntimeSafety(debug);
                const numLocals = pc[1].arg;
                const locals = pc[2..2+numLocals];
                pc += 2 + numLocals;
                for (locals) |local| {
                    framePtr[local.arg] = Value.None;
                }
                continue;
            },
            .setIndex => {
                @setRuntimeSafety(debug);
                const left = pc[1].arg;
                const index = pc[2].arg;
                const right = pc[3].arg;
                pc += 4;
                const rightv = framePtr[right];
                const indexv = framePtr[index];
                const leftv = framePtr[left];
                try gvm.setIndex(leftv, indexv, rightv);
                continue;
            },
            .copy => {
                @setRuntimeSafety(debug);
                const src = pc[1].arg;
                const dst = pc[2].arg;
                pc += 3;
                framePtr[dst] = framePtr[src];
                continue;
            },
            .copyRetainRelease => {
                @setRuntimeSafety(debug);
                const src = pc[1].arg;
                const dst = pc[2].arg;
                pc += 3;
                gvm.retain(framePtr[src]);
                release(framePtr[dst]);
                framePtr[dst] = framePtr[src];
                continue;
            },
            .copyReleaseDst => {
                @setRuntimeSafety(debug);
                const src = pc[1].arg;
                const dst = pc[2].arg;
                pc += 3;
                release(framePtr[dst]);
                framePtr[dst] = framePtr[src];
                continue;
            },
            .retain => {
                @setRuntimeSafety(debug);
                const local = pc[1].arg;
                pc += 2;
                gvm.retain(framePtr[local]);
                continue;
            },
            .index => {
                @setRuntimeSafety(debug);
                const left = pc[1].arg;
                const index = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const indexv = framePtr[index];
                const leftv = framePtr[left];
                framePtr[dst] = try @call(.{.modifier = .never_inline}, gvm.getIndex, .{leftv, indexv});
                continue;
            },
            .indexRetain => {
                @setRuntimeSafety(debug);
                const left = pc[1].arg;
                const index = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const indexv = framePtr[index];
                const leftv = framePtr[left];
                framePtr[dst] = try @call(.{.modifier = .never_inline}, gvm.getIndex, .{leftv, indexv});
                gvm.retain(framePtr[dst]);
                continue;
            },
            .reverseIndex => {
                @setRuntimeSafety(debug);
                const left = pc[1].arg;
                const index = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const indexv = framePtr[index];
                const leftv = framePtr[left];
                framePtr[dst] = try @call(.{.modifier = .never_inline}, gvm.getReverseIndex, .{leftv, indexv});
                continue;
            },
            .reverseIndexRetain => {
                @setRuntimeSafety(debug);
                const left = pc[1].arg;
                const index = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const indexv = framePtr[index];
                const leftv = framePtr[left];
                framePtr[dst] = try @call(.{.modifier = .never_inline}, gvm.getReverseIndex, .{leftv, indexv});
                gvm.retain(framePtr[dst]);
                continue;
            },
            .jumpBack => {
                @setRuntimeSafety(debug);
                pc -= @ptrCast(*const align(1) u16, &pc[1]).*;
                continue;
            },
            .jump => {
                @setRuntimeSafety(debug);
                pc += @ptrCast(*const align(1) u16, &pc[1]).*;
                continue;
            },
            // .jumpCondNone => {
            //     @setRuntimeSafety(debug);
            //     const pcOffset = @ptrCast(*const align(1) u16, &pc[1]).*;
            //     const local = pc[3].arg;
            //     if (gvm.getLocal(local).isNone()) {
            //         pc += pcOffset;
            //     } else {
            //         pc += 4;
            //     }
            //     continue;
            // },
            .jumpCond => {
                @setRuntimeSafety(debug);
                const jump = @ptrCast(*const align(1) u16, pc + 1).*;
                const cond = framePtr[pc[3].arg];
                const condVal = if (cond.isBool()) b: {
                    break :b cond.asBool();
                } else b: {
                    break :b @call(.{ .modifier = .never_inline }, cond.toBool, .{});
                };
                if (condVal) {
                    pc += jump;
                } else {
                    pc += 4;
                }
                continue;
            },
            .call0 => {
                @setRuntimeSafety(debug);
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                pc += 3;

                const callee = framePtr[startLocal + numArgs + 2 - 1];
                const retInfo = buildReturnInfo(pcOffset(pc), framePtrOffset(framePtr), 0, true);
                // try @call(.{ .modifier = .never_inline }, gvm.call, .{&pc, callee, numArgs, retInfo});
                try @call(.{ .modifier = .always_inline }, call, .{&pc, &framePtr, callee, startLocal, numArgs, retInfo});
                continue;
            },
            .call1 => {
                @setRuntimeSafety(debug);
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                pc += 3;

                const callee = framePtr[startLocal + numArgs + 2 - 1];
                const retInfo = buildReturnInfo(pcOffset(pc), framePtrOffset(framePtr), 1, true);
                // try @call(.{ .modifier = .never_inline }, gvm.call, .{&pc, callee, numArgs, retInfo});
                try @call(.{ .modifier = .always_inline }, call, .{&pc, &framePtr, callee, startLocal, numArgs, retInfo});
                continue;
            },
            .callObjSym0 => {
                @setRuntimeSafety(debug);
                const symId = pc[1].arg;
                const startLocal = pc[2].arg;
                const numArgs = pc[3].arg;
                pc += 4;

                const recv = framePtr[startLocal + numArgs + 2 - 1];
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer().?);
                    // if (@call(.{ .modifier = .never_inline }, gvm.getCallObjSym, .{obj, symId})) |sym| {
                    if (gvm.getCallObjSym(obj, symId)) |sym| {
                        // try @call(.{ .modifier = .never_inline }, gvm.callSymEntry, .{&pc, sym, obj, startLocal, numArgs, 0});
                        try gvm.callSymEntry(&pc, &framePtr, sym, obj, startLocal, numArgs, 0);
                    } else {
                        const res = try @call(.{ .modifier = .never_inline }, callObjSymFallback, .{pcOffset(pc), framePtr, obj, symId, startLocal, numArgs, 0});
                        pc = toPc(res.pc);
                        framePtr = res.framePtr;
                    }
                } else {
                    return gvm.panic("Missing function symbol in value.");
                }
                // try gvm.callObjSym(recv, symId, numArgs, 0);
                // try @call(.{.modifier = .always_inline }, gvm.callObjSym, .{recv, symId, numArgs, 0});
                continue;
            },
            .callObjSym1 => {
                @setRuntimeSafety(debug);
                const symId = pc[1].arg;
                const startLocal = pc[2].arg;
                const numArgs = pc[3].arg;
                pc += 4;

                const recv = framePtr[startLocal + numArgs + 2 - 1];
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer().?);
                    // if (@call(.{ .modifier = .never_inline }, gvm.getCallObjSym, .{obj, symId})) |sym| {
                    if (gvm.getCallObjSym(obj, symId)) |sym| {
                        // try @call(.{ .modifier = .never_inline }, gvm.callSymEntry, .{&pc, sym, obj, startLocal, numArgs, 1});
                        try gvm.callSymEntry(&pc, &framePtr, sym, obj, startLocal, numArgs, 1);
                    } else {
                        const res = try @call(.{ .modifier = .never_inline }, callObjSymFallback, .{pcOffset(pc), framePtr, obj, symId, startLocal, numArgs, 1});
                        pc = toPc(res.pc);
                        framePtr = res.framePtr;
                    }
                } else {
                    return gvm.panic("Missing function symbol in value.");
                }
                // try gvm.callObjSym(recv, symId, numArgs, 1);
                // try @call(.{.modifier = .always_inline }, gvm.callObjSym, .{recv, symId, numArgs, 1});
                continue;
            },
            .callSym0 => {
                @setRuntimeSafety(debug);
                const symId = pc[1].arg;
                const startLocal = pc[2].arg;
                const numArgs = pc[3].arg;
                // pc += 4;

                try gvm.callSym(&pc, &framePtr, symId, startLocal, numArgs, 0);
                continue;
            },
            .callSym1 => {
                @setRuntimeSafety(debug);
                const symId = pc[1].arg;
                const startLocal = pc[2].arg;
                const numArgs = pc[3].arg;
                // pc += 4;

                try gvm.callSym(&pc, &framePtr, symId, startLocal, numArgs, 1);
                continue;
            },
            .setFieldRelease => {
                @setRuntimeSafety(debug);
                const fieldId = pc[1].arg;
                const left = pc[2].arg;
                const right = pc[3].arg;
                pc += 4;

                const recv = framePtr[left];
                const val = framePtr[right];
                try gvm.setFieldRelease(recv, fieldId, val);
                continue;
            },
            .setField => {
                @setRuntimeSafety(debug);
                const fieldId = pc[1].arg;
                const left = pc[2].arg;
                const right = pc[3].arg;
                pc += 4;

                const recv = framePtr[left];
                const val = framePtr[right];
                try gvm.setField(recv, fieldId, val);
                continue;
            },
            .fieldRelease => {
                @setRuntimeSafety(debug);
                const fieldId = pc[1].arg;
                const left = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const recv = framePtr[left];
                const val = try gvm.getField(fieldId, recv);
                framePtr[dst] = val;
                release(recv);
                continue;
            },
            .fieldRetain => {
                @setRuntimeSafety(debug);
                const fieldId = pc[1].arg;
                const left = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;

                const recv = framePtr[left];
                const val = try gvm.getAndRetainField(fieldId, recv);
                framePtr[dst] = val;
                continue;
            },
            .lambda => {
                @setRuntimeSafety(debug);
                const funcPc = pcOffset(pc) - pc[1].arg;
                const numParams = pc[2].arg;
                const numLocals = pc[3].arg;
                const dst = pc[4].arg;
                pc += 5;
                framePtr[dst] = try gvm.allocLambda(funcPc, numParams, numLocals);
                continue;
            },
            .closure => {
                @setRuntimeSafety(debug);
                const funcPc = pcOffset(pc) - pc[1].arg;
                const numParams = pc[2].arg;
                const numCaptured = pc[3].arg;
                const numLocals = pc[4].arg;
                const dst = pc[5].arg;
                const capturedVals = pc[6..6+numCaptured];
                pc += 6 + numCaptured;

                framePtr[dst] = try gvm.allocClosure(framePtr, funcPc, numParams, numLocals, capturedVals);
                continue;
            },
            .forIter => {
                @setRuntimeSafety(debug);
                const startLocal = pc[1].arg;
                const iterable = pc[2].arg;
                const local = pc[3].arg;
                pc += 6;

                const iterableVal = framePtr[iterable];
                framePtr[startLocal + 2] = iterableVal;
                const res = try @call(.{ .modifier = .never_inline }, gvm.callObjSym, .{pcOffset(pc), framePtr, iterableVal, gvm.iteratorObjSym, startLocal, 1, 1});
                pc = toPc(res.pc);
                framePtr = res.framePtr;
                const iter = framePtr[startLocal];

                if (!iter.isPointer()) {
                    return gvm.panic("Not an iterator.");
                }

                gvm.pc = pc;
                if (local == 255) {
                    while (true) {
                        // try gvm.callObjSym(recv, gvm.nextObjSym, 1, 1);
                        framePtr[startLocal + 2] = iter;
                        // pc = try @call(.{ .modifier = .never_inline }, gvm.callObjSym, .{pc, iter, gvm.nextObjSym, startLocal, 1, 1});
                        const obj = stdx.ptrAlignCast(*HeapObject, iter.asPointer().?);
                        if (gvm.getCallObjSym(obj, gvm.nextObjSym)) |sym| {
                            framePtr[startLocal + 2] = iter;
                            try gvm.callSymEntry(&pc, &framePtr, sym, obj, startLocal, 1, 1);
                        } else return gvm.panic("Missing function symbol in value.");

                        const next = framePtr[startLocal];
                        if (next.isNone()) {
                            break;
                        }
                        gvm.framePtr = framePtr;
                        @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{}) catch |err| {
                            if (err == error.BreakLoop) {
                                break;
                            } else return err;
                        };
                    }
                } else {
                    while (true) {
                        // try gvm.callObjSym(recv, gvm.nextObjSym, 1, 1);
                        framePtr[startLocal + 2] = iter;
                        // pc = try @call(.{ .modifier = .never_inline }, gvm.callObjSym, .{pc, iter, gvm.nextObjSym, startLocal, 1, 1});
                        const obj = stdx.ptrAlignCast(*HeapObject, iter.asPointer().?);
                        if (gvm.getCallObjSym(obj, gvm.nextObjSym)) |sym| {
                            framePtr[startLocal + 2] = iter;
                            try gvm.callSymEntry(&pc, &framePtr, sym, obj, startLocal, 1, 1);
                        } else return gvm.panic("Missing function symbol in value.");

                        const next = framePtr[startLocal];
                        if (next.isNone()) {
                            break;
                        }
                        const localPtr = &framePtr[local];
                        release(localPtr.*);
                        localPtr.* = next;
                        gvm.framePtr = framePtr;
                        @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{}) catch |err| {
                            if (err == error.BreakLoop) {
                                break;
                            } else return err;
                        };
                    }
                }
                release(iter);
                pc = pc + @ptrCast(*const align(1) u16, pc-2).* - 6;
                continue;
            },
            .forRange => {
                @setRuntimeSafety(debug);
                const local = pc[1].arg;
                const startValueLocal = pc[2].arg;
                const endValueLocal = pc[3].arg;
                const stepValueLocal = pc[4].arg;
                const endPc = pc + @ptrCast(*const align(1) u16, &pc[5]).*;
                pc += 7;

                const step = framePtr[stepValueLocal].toF64();
                const rangeEnd = framePtr[endValueLocal].toF64();
                var i = framePtr[startValueLocal].toF64();

                gvm.pc = pc;
                gvm.framePtr = framePtr;
                if (i <= rangeEnd) {
                    if (local == 255) {
                        while (i < rangeEnd) : (i += step) {
                            @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{}) catch |err| {
                                if (err == error.BreakLoop) {
                                    break;
                                } else return err;
                            };
                        }
                    } else {
                        while (i < rangeEnd) : (i += step) {
                            framePtr[local] = Value.initF64(i);
                            @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{}) catch |err| {
                                if (err == error.BreakLoop) {
                                    break;
                                } else return err;
                            };
                        }
                    }
                } else {
                    if (local == 255) {
                        while (i > rangeEnd) : (i -= step) {
                            @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{}) catch |err| {
                                if (err == error.BreakLoop) {
                                    break;
                                } else return err;
                            };
                        }
                    } else {
                        while (i > rangeEnd) : (i -= step) {
                            framePtr[local] = Value.initF64(i);
                            @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{}) catch |err| {
                                if (err == error.BreakLoop) {
                                    break;
                                } else return err;
                            };
                        }
                    }
                }
                pc = endPc;
                continue;
            },
            .coreturn => {
                @setRuntimeSafety(debug);
                pc += 1;
                if (gvm.curFiber != &gvm.mainFiber) {
                    const res = popFiber(pcOffset(pc), framePtr);
                    pc = toPc(res.pc);
                    framePtr = res.framePtr;
                }
                continue;
            },
            .coresume => {
                @setRuntimeSafety(debug);
                const fiber = framePtr[pc[1].arg];
                // const dst = pc[2];
                if (fiber.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, fiber.asPointer().?);
                    if (obj.common.structId == FiberS) {
                        if (&obj.fiber != gvm.curFiber) {
                            const res = pushFiber(pcOffset(pc + 3), framePtr, &obj.fiber);
                            pc = toPc(res.pc);
                            framePtr = res.framePtr;
                            continue;
                        }
                    }
                }
                pc += 3;
                continue;
            },
            .coyield => {
                @setRuntimeSafety(debug);
                if (gvm.curFiber != &gvm.mainFiber) {
                    // Only yield on user fiber.
                    const res = popFiber(pcOffset(pc), framePtr);
                    pc = toPc(res.pc);
                    framePtr = res.framePtr;
                } else {
                    pc += 3;
                }
                continue;
            },
            .coinit => {
                @setRuntimeSafety(debug);
                const startArgsLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const jump = pc[3].arg;
                const initialStackSize = pc[4].arg;
                const dst = pc[5].arg;

                const args = framePtr[startArgsLocal..startArgsLocal + numArgs];
                const fiber = try @call(.{ .modifier = .never_inline }, allocFiber, .{pcOffset(pc + 6), args, initialStackSize});
                framePtr[dst] = fiber;
                pc += jump;
                continue;
            },
            .cont => {
                @setRuntimeSafety(debug);
                pc -= @ptrCast(*const align(1) u16, &pc[1]).*;
                return;
            },
            // .ret2 => {
            //     @setRuntimeSafety(debug);
            //     // Using never_inline seems to work against the final compiler optimizations.
            //     // @call(.{ .modifier = .never_inline }, gvm.popStackFrameCold, .{2});
            //     gvm.popStackFrameCold(2);
            //     continue;
            // },
            .ret1 => {
                @setRuntimeSafety(debug);
                if (@call(.{ .modifier = .always_inline }, popStackFrameLocal1, .{&pc, &framePtr})) {
                    continue;
                } else {
                    return;
                }
            },
            .ret0 => {
                @setRuntimeSafety(debug);
                if (@call(.{ .modifier = .always_inline }, popStackFrameLocal0, .{&pc, &framePtr})) {
                    continue;
                } else {
                    return;
                }
            },
            .mul => {
                @setRuntimeSafety(debug);
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.{ .modifier = .never_inline }, evalMultiply, .{srcLeft, srcRight});
                continue;
            },
            .div => {
                @setRuntimeSafety(debug);
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.{ .modifier = .never_inline }, evalDivide, .{srcLeft, srcRight});
                continue;
            },
            .mod => {
                @setRuntimeSafety(debug);
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.{ .modifier = .never_inline }, evalMod, .{srcLeft, srcRight});
                continue;
            },
            .pow => {
                @setRuntimeSafety(debug);
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.{ .modifier = .never_inline }, evalPower, .{srcLeft, srcRight});
                continue;
            },
            .box => {
                @setRuntimeSafety(debug);
                const value = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                pc += 3;
                gvm.retain(value);
                framePtr[dst] = try allocBox(value);
                continue;
            },
            .setBoxValue => {
                @setRuntimeSafety(debug);
                const box = framePtr[pc[1].arg];
                const rval = framePtr[pc[2].arg];
                pc += 3;
                if (builtin.mode == .Debug) {
                    std.debug.assert(box.isPointer());
                }
                const obj = stdx.ptrAlignCast(*HeapObject, box.asPointer().?);
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.common.structId == BoxS);
                }
                obj.box.val = rval;
                continue;
            },
            .setBoxValueRelease => {
                @setRuntimeSafety(debug);
                const box = framePtr[pc[1].arg];
                const rval = framePtr[pc[2].arg];
                pc += 3;
                if (builtin.mode == .Debug) {
                    std.debug.assert(box.isPointer());
                }
                const obj = stdx.ptrAlignCast(*HeapObject, box.asPointer().?);
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.common.structId == BoxS);
                }
                @call(.{ .modifier = .never_inline }, release, .{obj.box.val});
                obj.box.val = rval;
                continue;
            },
            .boxValue => {
                @setRuntimeSafety(debug);
                const box = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                pc += 3;
                if (builtin.mode == .Debug) {
                    std.debug.assert(box.isPointer());
                }
                const obj = stdx.ptrAlignCast(*HeapObject, box.asPointer().?);
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.common.structId == BoxS);
                }
                framePtr[dst] = obj.box.val;
                continue;
            },
            .boxValueRetain => {
                @setRuntimeSafety(debug);
                const box = framePtr[pc[1].arg];
                // if (builtin.mode == .Debug) {
                //     std.debug.assert(box.isPointer());
                // }
                // const obj = stdx.ptrAlignCast(*HeapObject, box.asPointer().?);
                // if (builtin.mode == .Debug) {
                //     // const obj = stdx.ptrAlignCast(*HeapObject, box.asPointer().?);
                //     std.debug.assert(obj.common.structId == BoxS);
                // }
                // gvm.stack[gvm.framePtr + pc[2].arg] = obj.box.val;
                // gvm.retain(obj.box.val);
                // pc += 3;
                framePtr[pc[2].arg] = @call(.{ .modifier = .never_inline }, boxValueRetain, .{box});
                pc += 3;
                continue;
            },
            .funcSymClosure => {
                @setRuntimeSafety(debug);
                const symId = pc[1].arg;
                const numParams = pc[2].arg;
                const numCaptured = pc[3].arg;
                const captured = pc[4..4+numCaptured];
                pc += 4 + numCaptured;
                try @call(.{ .modifier = .never_inline }, funcSymClosure, .{ framePtr, symId, numParams, captured });
                continue;
            },
            .bitwiseAnd => {
                @setRuntimeSafety(debug);
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = evalBitwiseAnd(srcLeft, srcRight);
                continue;
            },
            .end => {
                @setRuntimeSafety(debug);
                gvm.endLocal = pc[1].arg;
                pc += 2;
                gvm.curFiber.pc = @intCast(u32, pcOffset(pc));
                return error.End;
            },
        }
    }
}

fn popStackFrameLocal0(pc: *[*]const cy.OpData, framePtr: *[*]Value) linksection(".eval") bool {
    @setRuntimeSafety(debug);
    const retInfo = framePtr.*[1];
    const reqNumArgs = retInfo.retInfo.numRetVals;
    if (reqNumArgs == 0) {
        // Restore pc.
        framePtr.* = toFramePtr(retInfo.retInfo.framePtr);
        pc.* = toPc(retInfo.retInfo.pc);
        return retInfo.retInfo.retFlag == 0;
    } else {
        switch (reqNumArgs) {
            0 => unreachable,
            1 => {
                @setRuntimeSafety(debug);
                framePtr.*[0] = Value.initNone();
            },
            2 => {
                @setRuntimeSafety(debug);
                framePtr.*[0] = Value.initNone();
                framePtr.*[1] = Value.initNone();
            },
            3 => {
                @setRuntimeSafety(debug);
                framePtr.*[0] = Value.initNone();
                framePtr.*[1] = Value.initNone();
                framePtr.*[2] = Value.initNone();
            },
        }
        // Restore pc.
        framePtr.* = toFramePtr(retInfo.retInfo.framePtr);
        pc.* = toPc(retInfo.retInfo.pc);
        return retInfo.retInfo.retFlag == 0;
    }
}

fn popStackFrameLocal1(pc: *[*]const cy.OpData, framePtr: *[*]Value) linksection(".eval") bool {
    @setRuntimeSafety(debug);
    const retInfo = framePtr.*[1];
    const reqNumArgs = retInfo.retInfo.numRetVals;
    if (reqNumArgs == 1) {
        // Restore pc.
        framePtr.* = toFramePtr(retInfo.retInfo.framePtr);
        pc.* = toPc(retInfo.retInfo.pc);
        return retInfo.retInfo.retFlag == 0;
    } else {
        switch (reqNumArgs) {
            0 => {
                @setRuntimeSafety(debug);
                release(framePtr.*[0]);
            },
            1 => unreachable,
            2 => {
                @setRuntimeSafety(debug);
                framePtr.*[1] = Value.initNone();
            },
            3 => {
                @setRuntimeSafety(debug);
                // Only start checking for space at 3 since function calls should have at least two slot after framePtr.
                // self.ensureStackTotalCapacity(self.stack.top + 1) catch stdx.fatal();
                framePtr.*[1] = Value.initNone();
                framePtr.*[2] = Value.initNone();
            },
        }
        // Restore pc.
        framePtr.* = toFramePtr(retInfo.retInfo.framePtr);
        pc.* = toPc(retInfo.retInfo.pc);
        return retInfo.retInfo.retFlag == 0;
    }
}

fn dumpEvalOp(pc: [*]const cy.OpData) void {
    const offset = pcOffset(pc);
    switch (pc[0].code) {
        .callObjSym0 => {
            const methodId = pc[1].arg;
            const startLocal = pc[2].arg;
            const numArgs = pc[3].arg;
            log.debug("{} op: {s} {} {} {}", .{offset, @tagName(pc[0].code), methodId, startLocal, numArgs});
        },
        .callObjSym1 => {
            const methodId = pc[1].arg;
            const startLocal = pc[2].arg;
            const numArgs = pc[3].arg;
            log.debug("{} op: {s} {} {} {}", .{offset, @tagName(pc[0].code), methodId, startLocal, numArgs});
        },
        .callSym1 => {
            const funcId = pc[1].arg;
            const startLocal = pc[2].arg;
            const numArgs = pc[3].arg;
            log.debug("{} op: {s} {} {} {}", .{offset, @tagName(pc[0].code), funcId, startLocal, numArgs});
        },
        .callSym0 => {
            const funcId = pc[1].arg;
            const startLocal = pc[2].arg;
            const numArgs = pc[3].arg;
            log.debug("{} op: {s} {} {} {}", .{offset, @tagName(pc[0].code), funcId, startLocal, numArgs});
        },
        .release => {
            const local = pc[1].arg;
            log.debug("{} op: {s} {}", .{offset, @tagName(pc[0].code), local});
        },
        .copy => {
            const local = pc[1].arg;
            const dst = pc[2].arg;
            log.debug("{} op: {s} {} {}", .{offset, @tagName(pc[0].code), local, dst});
        },
        .copyRetainSrc => {
            const src = pc[1].arg;
            const dst = pc[2].arg;
            log.debug("{} op: {s} {} {}", .{offset, @tagName(pc[0].code), src, dst});
        },
        .fieldRetain => {
            const fieldId = pc[1].arg;
            log.debug("{} op: {s} {}", .{offset, @tagName(pc[0].code), fieldId});
        },
        .map => {
            const startLocal = pc[1].arg;
            const numEntries = pc[2].arg;
            const startConst = pc[3].arg;
            log.debug("{} op: {s} {} {} {}", .{offset, @tagName(pc[0].code), startLocal, numEntries, startConst});
        },
        .constI8 => {
            const val = pc[1].arg;
            const dst = pc[2].arg;
            log.debug("{} op: {s} [{}] -> %{}", .{offset, @tagName(pc[0].code), @bitCast(i8, val), dst});
        },
        .constOp => {
            const idx = pc[1].arg;
            const dst = pc[2].arg;
            const val = Value{ .val = gvm.consts[idx].val };
            log.debug("{} op: {s} [{s}] -> %{}", .{offset, @tagName(pc[0].code), gvm.valueToTempString(val), dst});
        },
        .end => {
            const endLocal = pc[1].arg;
            log.debug("{} op: {s} {}", .{offset, @tagName(pc[0].code), endLocal});
        },
        .setInitN => {
            const numLocals = pc[1].arg;
            const locals = pc[2..2+numLocals];
            log.debug("{} op: {s} {}", .{offset, @tagName(pc[0].code), numLocals});
            for (locals) |local| {
                log.debug("{}", .{local.arg});
            }
        },
        else => {
            log.debug("{} op: {s}", .{offset, @tagName(pc[0].code)});
        },
    }
}

pub const EvalError = error{
    Panic,
    ParseError,
    CompileError,
    OutOfMemory,
    NoEndOp,
    End,
    OutOfBounds,
    StackOverflow,
    NoDebugSym,
};

pub const StackTrace = struct {
    frames: []const StackFrame = &.{},

    fn deinit(self: *StackTrace, alloc: std.mem.Allocator) void {
        alloc.free(self.frames);
    }

    pub fn dump(self: *const StackTrace) void {
        for (self.frames) |frame| {
            if (builtin.is_test) {
                log.debug("{s}:{}:{}", .{frame.name, frame.line + 1, frame.col + 1});
            } else {
                std.debug.print("{s}:{}:{}\n", .{frame.name, frame.line + 1, frame.col + 1});
            }
        }
    }
};

pub const StackFrame = struct {
    name: []const u8,
    /// Starts at 0.
    line: u32,
    /// Starts at 0.
    col: u32,
};

const MethodKey = struct {
    structId: StructId,
    methodId: SymbolId,
};

/// Stack layout for lambda: arg0, arg1, ..., callee
/// Stack layout for closure: arg0, arg1, ..., callee, capturedVar0, capturedVar1, ...
/// numArgs includes the callee.
pub fn call(pc: *[*]const cy.OpData, framePtr: *[*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !void {
    @setRuntimeSafety(debug);
    if (callee.isPointer()) {
        const obj = stdx.ptrCastAlign(*HeapObject, callee.asPointer().?);
        switch (obj.common.structId) {
            ClosureS => {
                if (numArgs - 1 != obj.closure.numParams) {
                    stdx.panic("params/args mismatch");
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.closure.numLocals) >= @ptrToInt(gvm.stack.ptr) + (gvm.stack.len << 3)) {
                    return error.StackOverflow;
                }

                pc.* = toPc(obj.closure.funcPc);
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;

                // Copy over captured vars to new call stack locals.
                if (obj.closure.numCaptured <= 3) {
                    const src = @ptrCast([*]Value, &obj.closure.capturedVal0)[0..obj.closure.numCaptured];
                    std.mem.copy(Value, framePtr.*[numArgs + 2..numArgs + 2 + obj.closure.numCaptured], src);
                } else {
                    stdx.panic("unsupported closure > 3 captured args.");
                }
            },
            LambdaS => {
                if (numArgs - 1 != obj.lambda.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    stdx.fatal();
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.lambda.numLocals) >= @ptrToInt(gvm.stack.ptr) + (gvm.stack.len << 3)) {
                    return error.StackOverflow;
                }

                pc.* = toPc(obj.lambda.funcPc);
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;
            },
            else => {},
        }
    } else {
        stdx.panic("not a function");
    }
}

pub fn callNoInline(pc: *[*]const cy.OpData, framePtr: *[*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !void {
    @setRuntimeSafety(debug);
    if (callee.isPointer()) {
        const obj = stdx.ptrCastAlign(*HeapObject, callee.asPointer().?);
        switch (obj.common.structId) {
            ClosureS => {
                if (numArgs - 1 != obj.closure.numParams) {
                    stdx.panic("params/args mismatch");
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.closure.numLocals) >= @ptrToInt(gvm.stack.ptr) + (gvm.stack.len << 3)) {
                    return error.StackOverflow;
                }

                pc.* = toPc(obj.closure.funcPc);
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;

                // Copy over captured vars to new call stack locals.
                if (obj.closure.numCaptured <= 3) {
                    const src = @ptrCast([*]Value, &obj.closure.capturedVal0)[0..obj.closure.numCaptured];
                    std.mem.copy(Value, framePtr.*[numArgs + 2..numArgs + 2 + obj.closure.numCaptured], src);
                } else {
                    stdx.panic("unsupported closure > 3 captured args.");
                }
            },
            LambdaS => {
                if (numArgs - 1 != obj.lambda.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    stdx.fatal();
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.lambda.numLocals) >= @ptrToInt(gvm.stack.ptr) + (gvm.stack.len << 3)) {
                    return error.StackOverflow;
                }

                pc.* = toPc(obj.lambda.funcPc);
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;
            },
            else => {},
        }
    } else {
        stdx.panic("not a function");
    }
}

fn getObjectFunctionFallback(obj: *const HeapObject, symId: SymbolId) !Value {
    @setCold(true);
    @setRuntimeSafety(debug);
    const name = gvm.methodSymExtras.buf[symId];
    if (obj.common.structId == MapS) {
        const heapMap = stdx.ptrCastAlign(*const MapInner, &obj.map.inner);
        if (heapMap.getByString(&gvm, name)) |val| {
            return val;
        }
    }
    return gvm.panic("Missing function symbol in value");
}

// Use new pc local to avoid deoptimization.
fn callObjSymFallback(pc: usize, framePtr: [*]Value, obj: *HeapObject, symId: SymbolId, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2) !PcFramePtr {
    @setCold(true);
    @setRuntimeSafety(debug);
    const func = try getObjectFunctionFallback(obj, symId);

    gvm.retain(func);
    release(Value.initPtr(obj));

    // Replace receiver with function.
    framePtr[startLocal + 2 + numArgs - 1] = func;
    // const retInfo = gvm.buildReturnInfo2(gvm.pc + 3, reqNumRetVals, true);
    const retInfo = buildReturnInfo(pc, framePtrOffset(framePtr), reqNumRetVals, true);
    var newPc = toPc(pc);
    var newFramePtr = framePtr;
    try @call(.{ .modifier = .always_inline }, callNoInline, .{&newPc, &newFramePtr, func, startLocal, numArgs, retInfo});
    return PcFramePtr{
        .pc = pcOffset(newPc),
        .framePtr = newFramePtr,
    };
}

fn callSymEntryNoInline(pc: usize, framePtr: [*]Value, sym: SymbolEntry, obj: *HeapObject, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") !PcFramePtr {
    @setRuntimeSafety(debug);
    switch (sym.entryT) {
        .func => {
            @setRuntimeSafety(debug);
            if (@ptrToInt(framePtr + startLocal + sym.inner.func.numLocals) >= @ptrToInt(gvm.stack.ptr) + 8 * gvm.stack.len) {
                return error.StackOverflow;
            }

            // const retInfo = gvm.buildReturnInfo2(gvm.pc + 3, reqNumRetVals, true);
            const retInfo = buildReturnInfo(pc, framePtrOffset(framePtr), reqNumRetVals, true);
            const newFramePtr = framePtr + startLocal;
            newFramePtr[1] = retInfo;
            return PcFramePtr{
                .pc = sym.inner.func.pc,
                .framePtr = newFramePtr,
            };
        },
        .nativeFunc1 => {
            @setRuntimeSafety(debug);
            // gvm.pc += 3;
            // const newFramePtr = gvm.framePtr + startLocal;
            const newFramePtr = framePtr + startLocal;
            gvm.pc = toPc(pc);
            gvm.framePtr = framePtr;
            const res = sym.inner.nativeFunc1(undefined, obj, newFramePtr+2, numArgs);
            if (reqNumRetVals == 1) {
                newFramePtr[0] = res;
            } else {
                switch (reqNumRetVals) {
                    0 => {
                        // Nop.
                    },
                    1 => stdx.panic("not possible"),
                    2 => {
                        stdx.panic("unsupported require 2 ret vals");
                    },
                    3 => {
                        stdx.panic("unsupported require 3 ret vals");
                    },
                }
            }
            return PcFramePtr{
                .pc = pcOffset(gvm.pc),
                .framePtr = framePtr,
            };
        },
        .nativeFunc2 => {
            @setRuntimeSafety(debug);
            // gvm.pc += 3;
            const newFramePtr = gvm.framePtr + startLocal;
            gvm.pc = toPc(pc);
            const res = sym.inner.nativeFunc2(undefined, obj, @ptrCast([*]const Value, newFramePtr+2), numArgs);
            if (reqNumRetVals == 2) {
                gvm.stack[newFramePtr] = res.left;
                gvm.stack[newFramePtr+1] = res.right;
            } else {
                switch (reqNumRetVals) {
                    0 => {
                        // Nop.
                    },
                    1 => unreachable,
                    2 => {
                        unreachable;
                    },
                    3 => {
                        unreachable;
                    },
                }
            }
        },
        // else => {
        //     // stdx.panicFmt("unsupported {}", .{sym.entryT});
        //     unreachable;
        // },
    }
    return pc;
}

fn popFiber(curFiberEndPc: usize, curFramePtr: [*]Value) PcFramePtr {
    @setRuntimeSafety(debug);
    gvm.curFiber.stackPtr = gvm.stack.ptr;
    gvm.curFiber.stackLen = @intCast(u32, gvm.stack.len);
    gvm.curFiber.pc = @intCast(u32, curFiberEndPc);
    gvm.curFiber.framePtr = curFramePtr;

    // Release current fiber.
    const nextFiber = gvm.curFiber.prevFiber.?;
    releaseObject(@ptrCast(*HeapObject, gvm.curFiber));

    // Set to next fiber.
    gvm.curFiber = nextFiber;

    gvm.stack = gvm.curFiber.stackPtr[0..gvm.curFiber.stackLen];
    gvm.framePtr = gvm.curFiber.framePtr;
    log.debug("fiber set to {} {*}", .{gvm.curFiber.pc, gvm.framePtr});
    return PcFramePtr{
        .pc = gvm.curFiber.pc,
        .framePtr = gvm.curFiber.framePtr,
    };
}

/// Since this is called from a coresume expression, the fiber should already be retained.
fn pushFiber(curFiberEndPc: usize, curFramePtr: [*]Value, fiber: *Fiber) PcFramePtr {
    @setRuntimeSafety(debug);
    // Save current fiber.
    gvm.curFiber.stackPtr = gvm.stack.ptr;
    gvm.curFiber.stackLen = @intCast(u32, gvm.stack.len);
    gvm.curFiber.pc = @intCast(u32, curFiberEndPc);
    gvm.curFiber.framePtr = curFramePtr;

    // Push new fiber.
    fiber.prevFiber = gvm.curFiber;
    gvm.curFiber = fiber;
    gvm.stack = fiber.stackPtr[0..fiber.stackLen];
    gvm.framePtr = fiber.framePtr;
    // Check if fiber was previously yielded.
    if (gvm.ops[fiber.pc].code == .coyield) {
        log.debug("fiber set to {} {*}", .{fiber.pc + 3, gvm.framePtr});
        return .{
            .pc = fiber.pc + 3,
            .framePtr = fiber.framePtr,
        };
    } else {
        log.debug("fiber set to {} {*}", .{fiber.pc, gvm.framePtr});
        return .{
            .pc = fiber.pc,
            .framePtr = fiber.framePtr,
        };
    }
}

fn allocFiber(pc: usize, args: []const Value, initialStackSize: u32) linksection(".eval") !Value {
    @setRuntimeSafety(debug);

    // Args are copied over to the new stack.
    var stack = try gvm.alloc.alloc(Value, initialStackSize);
    // Assumes initial stack size generated by compiler is enough to hold captured args.
    std.mem.copy(Value, stack[2..2+args.len], args);

    const obj = try gvm.allocObject();
    obj.fiber = .{
        .structId = FiberS,
        .rc = 1,
        .stackPtr = stack.ptr,
        .stackLen = @intCast(u32, stack.len),
        .pc = @intCast(u32, pc),
        .framePtr = @ptrCast([*]Value, &stack[0]),
        .prevFiber = undefined,
    };
    if (TraceEnabled) {
        gvm.trace.numRetainAttempts += 1;
        gvm.trace.numRetains += 1;
    }
    if (TrackGlobalRC) {
        gvm.refCounts += 1;
    }

    return Value.initPtr(obj);
}

fn runReleaseOps(stack: []const Value, framePtr: usize, startPc: usize) void {
    var pc = startPc;
    while (gvm.ops[pc].code == .release) {
        const local = gvm.ops[pc+1].arg;
        // stack[framePtr + local].dump();
        release(stack[framePtr + local]);
        pc += 2;
    }
}

/// Unwinds the stack and releases the locals.
/// This also releases the initial captured vars since it's on the stack.
fn releaseFiberStack(fiber: *Fiber) void {
    log.debug("release fiber stack", .{});
    var stack = fiber.stackPtr[0..fiber.stackLen];
    var framePtr = (@ptrToInt(fiber.framePtr) - @ptrToInt(stack.ptr)) >> 3;
    var pc = fiber.pc;

    // Check if fiber is still in init state.
    switch (gvm.ops[pc].code) {
        .callSym0,
        .callSym1 => {
            if (gvm.ops[pc + 4].code == .coreturn) {
                const numArgs = gvm.ops[pc - 4].arg;
                for (fiber.framePtr[2..2 + numArgs]) |arg| {
                    release(arg);
                }
            }
        },
        else => {},
    }

    // Check if fiber was previously on a yield op.
    if (gvm.ops[pc].code == .coyield) {
        const jump = @ptrCast(*const align(1) u16, &gvm.ops[pc+1]).*;
        log.debug("release on frame {} {}", .{pc, pc + jump});
        // The yield statement already contains the end locals pc.
        runReleaseOps(stack, framePtr, pc + jump);
    }
    // Unwind stack and release all locals.
    while (framePtr > 0) {
        pc = stack[framePtr + 1].retInfo.pc;
        framePtr = stack[framePtr + 1].retInfo.framePtr;
        const endLocalsPc = pcToEndLocalsPc(pc);
        log.debug("release on frame {} {}", .{pc, endLocalsPc});
        if (endLocalsPc != NullId) {
            runReleaseOps(stack, framePtr, endLocalsPc);
        }
    }
    // Finally free stack.
    gvm.alloc.free(stack);
}

/// Given pc position, return the end locals pc in the same frame.
/// TODO: Memoize this function.
fn pcToEndLocalsPc(pc: usize) u32 {
    const idx = gvm.indexOfDebugSym(pc) orelse {
        stdx.panic("Missing debug symbol.");
    };
    const sym = gvm.debugTable[idx];
    if (sym.frameLoc != NullId) {
        const node = gvm.compiler.nodes[sym.frameLoc];
        return node.head.func.genEndLocalsPc;
    } else return NullId;
}

pub inline fn buildReturnInfo(pc: usize, framePtr: usize, comptime numRetVals: u2, comptime cont: bool) linksection(".eval") Value {
    @setRuntimeSafety(debug);
    return Value{
        .retInfo = .{
            .pc = @intCast(u32, pc),
            .framePtr = @intCast(u29, framePtr),
            .numRetVals = numRetVals,
            .retFlag = if (cont) 0 else 1,
        },
    };
}

pub inline fn pcOffset(pc: [*]const cy.OpData) u32 {
    // Divide by eight.
    return @intCast(u32, @ptrToInt(pc) - @ptrToInt(gvm.ops.ptr));
}

pub inline fn toPc(offset: usize) [*]const cy.OpData {
    return @ptrCast([*]const cy.OpData, &gvm.ops.ptr[offset]);
}

pub inline fn framePtrOffset(framePtr: [*]Value) usize {
    // Divide by eight.
    return (@ptrToInt(framePtr) - @ptrToInt(gvm.stack.ptr)) >> 3;
}

pub inline fn toFramePtr(offset: usize) [*]Value {
    return @ptrCast([*]Value, &gvm.stack[offset]);
}

const PcFramePtr = struct {
    pc: usize,
    framePtr: [*]Value,
};

fn boxValueRetain(box: Value) linksection(".eval") Value {
    @setCold(true);
    if (builtin.mode == .Debug) {
        std.debug.assert(box.isPointer());
    }
    const obj = stdx.ptrAlignCast(*HeapObject, box.asPointer().?);
    if (builtin.mode == .Debug) {
        std.debug.assert(obj.common.structId == BoxS);
    }
    gvm.retain(obj.box.val);
    return obj.box.val;
}

fn allocBox(val: Value) !Value {
    @setRuntimeSafety(debug);
    const obj = try gvm.allocObject();
    obj.box = .{
        .structId = BoxS,
        .rc = 1,
        .val = val,
    };
    if (TraceEnabled) {
        gvm.trace.numRetainAttempts += 1;
        gvm.trace.numRetains += 1;
    }
    if (TrackGlobalRC) {
        gvm.refCounts += 1;
    }

    return Value.initPtr(obj);
}

fn funcSymClosure(framePtr: [*]Value, symId: SymbolId, numParams: u8, capturedLocals: []const cy.OpData) !void {
    @setCold(true);
    const sym = gvm.funcSyms.buf[symId];
    const pc = sym.inner.func.pc;
    const numLocals = @intCast(u8, sym.inner.func.numLocals);

    const closure = try gvm.allocClosure(framePtr, pc, numParams, numLocals, capturedLocals);
    gvm.funcSyms.buf[symId].entryT = .closure;
    gvm.funcSyms.buf[symId].inner.closure = stdx.ptrAlignCast(*Closure, closure.asPointer().?);
}