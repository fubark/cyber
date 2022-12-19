const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const tcc = @import("tcc");

const fmt = @import("fmt.zig");
const cy = @import("cyber.zig");
const bindings = @import("bindings.zig");
const Value = cy.Value;
const debug = builtin.mode == .Debug;
const TraceEnabled = @import("build_options").trace;

const log = stdx.log.scoped(.vm);

const UseGlobalVM = true;
pub const TrackGlobalRC = builtin.mode != .ReleaseFast;
const section = ".eval";

/// Reserved symbols known at comptime.
pub const ListS: StructId = 8;
pub const MapS: StructId = 9;
pub const ClosureS: StructId = 10;
pub const LambdaS: StructId = 11;
pub const StringS: StructId = 12;
pub const FiberS: StructId = 13;
pub const BoxS: StructId = 14;
pub const NativeFunc1S: StructId = 15;
pub const TccStateS: StructId = 16;
pub const OpaquePtrS: StructId = 17;

var tempU8Buf: [256]u8 = undefined;

/// Accessing global vars is faster with direct addressing.
pub var gvm: VM = undefined;

pub fn getUserVM() *UserVM {
    return @ptrCast(*UserVM, &gvm);
}

pub const VM = struct {
    alloc: std.mem.Allocator,
    parser: cy.Parser,
    compiler: cy.VMcompiler,

    /// [Eval context]

    /// Program counter. Pointer to the current instruction data in `ops`.
    pc: [*]cy.OpData,
    /// Current stack frame ptr.
    framePtr: [*]Value,

    /// Value stack.
    stack: []Value,
    stackEndPtr: [*]const Value,

    ops: []cy.OpData,
    consts: []const cy.Const,
    strBuf: []const u8,

    /// Object heap pages.
    heapPages: cy.List(*HeapPage),
    heapFreeHead: ?*HeapObject,

    refCounts: if (TrackGlobalRC) usize else void,

    /// Symbol table used to lookup object methods.
    /// First, the SymbolId indexes into the table for a SymbolMap to lookup the final SymbolEntry by StructId.
    methodSyms: cy.List(SymbolMap),
    methodTable: std.AutoHashMapUnmanaged(ObjectSymKey, SymbolEntry),

    /// Used to track which method symbols already exist. Only considers the name right now.
    methodSymSigs: std.StringHashMapUnmanaged(SymbolId),

    /// Regular function symbol table.
    funcSyms: cy.List(FuncSymbolEntry),
    funcSymSignatures: std.StringHashMapUnmanaged(SymbolId),
    funcSymDetails: cy.List(FuncSymDetail),

    varSyms: cy.List(VarSymbol),
    varSymSignatures: std.StringHashMapUnmanaged(SymbolId),

    /// Struct fields symbol table.
    fieldSyms: cy.List(FieldSymbolMap),
    fieldTable: std.AutoHashMapUnmanaged(ObjectSymKey, u16),
    fieldSymSignatures: std.StringHashMapUnmanaged(SymbolId),

    /// Structs.
    structs: cy.List(Struct),
    structSignatures: std.StringHashMapUnmanaged(StructId),
    iteratorObjSym: SymbolId,
    pairIteratorObjSym: SymbolId,
    nextObjSym: SymbolId,
    nextPairObjSym: SymbolId,

    /// Tag types.
    tagTypes: cy.List(TagType),
    tagTypeSignatures: std.StringHashMapUnmanaged(TagTypeId),

    /// Tag literals.
    tagLitSyms: cy.List(TagLitSym),
    tagLitSymSignatures: std.StringHashMapUnmanaged(SymbolId),

    u8Buf: cy.List(u8),

    stackTrace: StackTrace,

    methodSymExtras: cy.List([]const u8),
    debugTable: []const cy.OpDebug,
    panicMsg: []const u8,

    curFiber: *Fiber,
    mainFiber: Fiber,

    /// Local to be returned back to eval caller.
    /// 255 indicates no return value.
    endLocal: u8,

    mainUri: []const u8,

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
            .stackEndPtr = undefined,
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
            .funcSymDetails = .{},
            .varSyms = .{},
            .varSymSignatures = .{},
            .fieldSyms = .{},
            .fieldTable = .{},
            .fieldSymSignatures = .{},
            .structs = .{},
            .structSignatures = .{},
            .tagTypes = .{},
            .tagTypeSignatures = .{},
            .tagLitSyms = .{},
            .tagLitSymSignatures = .{},
            .iteratorObjSym = undefined,
            .pairIteratorObjSym = undefined,
            .nextObjSym = undefined,
            .nextPairObjSym = undefined,
            .trace = undefined,
            .u8Buf = .{},
            .stackTrace = .{},
            .debugTable = undefined,
            .refCounts = if (TrackGlobalRC) 0 else undefined,
            .panicMsg = "",
            .mainFiber = undefined,
            .curFiber = undefined,
            .endLocal = undefined,
            .mainUri = "",
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
        try @call(.{ .modifier = .never_inline }, bindings.bindCore, .{self});
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
                releaseObject(self, @ptrCast(*HeapObject, sym.inner.closure));
            }
        }
        self.funcSyms.deinit(self.alloc);
        self.funcSymSignatures.deinit(self.alloc);
        for (self.funcSymDetails.items()) |detail| {
            self.alloc.free(detail.name);
        }
        self.funcSymDetails.deinit(self.alloc);

        self.varSyms.deinit(self.alloc);
        self.varSymSignatures.deinit(self.alloc);

        self.fieldSyms.deinit(self.alloc);
        self.fieldTable.deinit(self.alloc);
        self.fieldSymSignatures.deinit(self.alloc);

        for (self.heapPages.items()) |page| {
            self.alloc.destroy(page);
        }
        self.heapPages.deinit(self.alloc);

        self.structs.deinit(self.alloc);
        self.structSignatures.deinit(self.alloc);

        self.tagTypes.deinit(self.alloc);
        self.tagTypeSignatures.deinit(self.alloc);

        self.tagLitSyms.deinit(self.alloc);
        self.tagLitSymSignatures.deinit(self.alloc);

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

    pub fn compile(self: *VM, srcUri: []const u8, src: []const u8) !cy.ByteCodeBuffer {
        var tt = stdx.debug.trace();
        const astRes = try self.parser.parse(src);
        if (astRes.has_error) {
            if (astRes.isTokenError) {
                try printUserError(self, "TokenError", astRes.err_msg, srcUri, self.parser.last_err_pos, true);
                return error.TokenError;
            } else {
                try printUserError(self, "ParseError", astRes.err_msg, srcUri, self.parser.last_err_pos, false);
                return error.ParseError;
            }
        }
        tt.endPrint("parse");

        tt = stdx.debug.trace();
        const res = try self.compiler.compile(astRes);
        if (res.hasError) {
            if (self.compiler.lastErrNode != NullId) {
                const token = self.parser.nodes.items[self.compiler.lastErrNode].start_token;
                const pos = self.parser.tokens.items[token].pos();
                try printUserError(self, "CompileError", self.compiler.lastErr, srcUri, pos, false);
            } else {
                try printUserError(self, "CompileError", self.compiler.lastErr, srcUri, NullId, false);
            }
            return error.CompileError;
        }
        tt.endPrint("compile");

        return res.buf;
    }

    pub fn eval(self: *VM, srcUri: []const u8, src: []const u8) !Value {
        var tt = stdx.debug.trace();
        const astRes = try self.parser.parse(src);
        if (astRes.has_error) {
            if (astRes.isTokenError) {
                try printUserError(self, "TokenError", astRes.err_msg, srcUri, self.parser.last_err_pos, true);
                return error.TokenError;
            } else {
                try printUserError(self, "ParseError", astRes.err_msg, srcUri, self.parser.last_err_pos, false);
                return error.ParseError;
            }
        }
        tt.endPrint("parse");

        tt = stdx.debug.trace();
        const res = try self.compiler.compile(astRes);
        if (res.hasError) {
            if (self.compiler.lastErrNode != NullId) {
                const token = self.parser.nodes.items[self.compiler.lastErrNode].start_token;
                const pos = self.parser.tokens.items[token].pos();
                try printUserError(self, "CompileError", self.compiler.lastErr, srcUri, pos, false);
            } else {
                try printUserError(self, "CompileError", self.compiler.lastErr, srcUri, NullId, false);
            }
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

        self.mainUri = srcUri;
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
        self.pc = @ptrCast([*]cy.OpData, buf.ops.items.ptr);
        try self.stackEnsureTotalCapacity(buf.mainStackSize);
        self.framePtr = @ptrCast([*]Value, self.stack.ptr);

        self.ops = buf.ops.items;
        self.consts = buf.mconsts;
        self.strBuf = buf.strBuf.items;

        try @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{self});
        if (TraceEnabled) {
            log.info("main stack size: {}", .{buf.mainStackSize});
        }

        if (self.endLocal == 255) {
            return Value.None;
        } else {
            return self.stack[self.endLocal];
        }
    }

    fn sliceList(self: *VM, listV: Value, startV: Value, endV: Value) !Value {
        if (listV.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, listV.asPointer().?);
            if (obj.retainedCommon.structId == ListS) {
                const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
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
        const obj = try self.allocPoolObject();
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

    /// Allocates an object outside of the object pool.
    fn allocObject(self: *VM, sid: StructId, offsets: []const cy.OpData, props: []const Value) !Value {
        // First slot holds the structId and rc.
        const objSlice = try self.alloc.alloc(Value, 1 + props.len);
        const obj = @ptrCast(*Object, objSlice.ptr);
        obj.* = .{
            .structId = sid,
            .rc = 1,
            .firstValue = undefined,
        };
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }

        const dst = obj.getValuesPtr();
        for (offsets) |offset, i| {
            dst[offset.arg] = props[i];
        }

        const res = Value.initPtr(obj);
        return res;
    }

    fn allocObjectSmall(self: *VM, sid: StructId, offsets: []const cy.OpData, props: []const Value) !Value {
        const obj = try self.allocPoolObject();
        obj.object = .{
            .structId = sid,
            .rc = 1,
            .firstValue = undefined,
        };
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }

        const dst = obj.object.getValuesPtr();
        for (offsets) |offset, i| {
            dst[offset.arg] = props[i];
        }

        const res = Value.initPtr(obj);
        return res;
    }

    fn allocMap(self: *VM, keyIdxs: []const cy.OpData, vals: []const Value) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocPoolObject();
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

        const inner = stdx.ptrAlignCast(*MapInner, &obj.map.inner);
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

    fn allocPoolObject(self: *VM) !*HeapObject {
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
        const obj = try self.allocPoolObject();
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
        const obj = try self.allocPoolObject();
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

    pub fn allocOwnedString(self: *VM, str: []u8) linksection(section) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocPoolObject();
        obj.string = .{
            .structId = StringS,
            .rc = 1,
            .ptr = str.ptr,
            .len = str.len,
        };
        log.debug("alloc owned str {*} {s}", .{str.ptr, str});
        if (TraceEnabled) {
            self.trace.numRetains += 1;
            self.trace.numRetainAttempts += 1;
        }
        if (TrackGlobalRC) {
            gvm.refCounts += 1;
        }
        return Value.initPtr(obj);
    }

    pub fn allocOpaquePtr(self: *VM, ptr: ?*anyopaque) !Value {
        const obj = try self.allocPoolObject();
        obj.opaquePtr = .{
            .structId = OpaquePtrS,
            .rc = 1,
            .ptr = ptr,
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

    pub fn allocTccState(self: *VM, state: *tcc.TCCState) !Value {
        const obj = try self.allocPoolObject();
        obj.tccState = .{
            .structId = TccStateS,
            .rc = 1,
            .state = state,
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

    pub fn allocNativeFunc1(self: *VM, func: *const fn (*UserVM, [*]Value, u8) Value, tccState: ?Value) !Value {
        const obj = try self.allocPoolObject();
        obj.nativeFunc1 = .{
            .structId = NativeFunc1S,
            .rc = 1,
            .func = func,
            .tccState = undefined,
            .hasTccState = false,
        };
        if (tccState) |state| {
            obj.nativeFunc1.tccState = state;
            obj.nativeFunc1.hasTccState = true;
        }
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
        const obj = try self.allocPoolObject();
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
        const firstStr = self.valueAsString(Value.initRaw(gvm.consts[strs[0].arg].val));
        try self.u8Buf.resize(self.alloc, firstStr.len);
        std.mem.copy(u8, self.u8Buf.items(), firstStr);

        var writer = self.u8Buf.writer(self.alloc);
        for (vals) |val, i| {
            self.writeValueToString(writer, val);
            release(self, val);
            try self.u8Buf.appendSlice(self.alloc, self.valueAsString(Value.initRaw(gvm.consts[strs[i+1].arg].val)));
        }

        const obj = try self.allocPoolObject();
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
        const obj = try self.allocPoolObject();
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
        const obj = try self.allocPoolObject();
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
        const obj = try self.allocPoolObject();
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
        const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
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

    pub fn ensureTagType(self: *VM, name: []const u8) !TagTypeId {
        const res = try self.tagTypeSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            return self.addTagType(name);
        } else {
            return res.value_ptr.*;
        }
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
        switch (entry.mapT) {
            .one => {
                if (entry.inner.one.id == sid) {
                    return entry.inner.one.offset;
                }
            },
            .many => {
                if (entry.inner.many.mruStructId == sid) {
                    return entry.inner.many.mruOffset;
                } else {
                    const offset = self.fieldTable.get(.{ .structId = sid, .symId = fieldId }).?;
                    self.fieldSyms.buf[fieldId].inner.many = .{
                        .mruStructId = sid,
                        .mruOffset = offset,
                    };
                    return offset;
                }
            },
            .empty => {
            },
        }
        return null;
    }

    pub fn addTagType(self: *VM, name: []const u8) !TagTypeId {
        const s = TagType{
            .name = name,
            .numMembers = 0,
        };
        const id = @intCast(u32, self.tagTypes.len);
        try self.tagTypes.append(self.alloc, s);
        try self.tagTypeSignatures.put(self.alloc, name, id);
        return id;
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

    pub inline fn getFuncSym(self: *const VM, name: []const u8) ?SymbolId {
        return self.funcSymSignatures.get(name);
    }

    pub inline fn getVarSym(self: *const VM, name: []const u8) ?SymbolId {
        return self.varSymSignatures.get(name);
    }

    pub fn ensureVarSym(self: *VM, name: []const u8) !SymbolId {
        const res = try self.varSymSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            const id = @intCast(u32, self.varSyms.len);
            try self.varSyms.append(self.alloc, .{
                .symT = .none,
                .inner = undefined,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
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

    pub fn getTagLitName(self: *const VM, id: u32) []const u8 {
        return self.tagLitSyms.buf[id].name;
    }

    pub fn ensureTagLitSym(self: *VM, name: []const u8) !SymbolId {
        _ = self;
        const res = try gvm.tagLitSymSignatures.getOrPut(gvm.alloc, name);
        if (!res.found_existing) {
            const id = @intCast(u32, gvm.tagLitSyms.len);
            try gvm.tagLitSyms.append(gvm.alloc, .{
                .symT = .empty,
                .inner = undefined,
                .name = name,
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
        if (map.mapT == .one) {
            return map.inner.one.id == sid;
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

    pub fn addFieldSym(self: *VM, sid: StructId, symId: SymbolId, offset: u16) !void {
        switch (self.fieldSyms.buf[symId].mapT) {
            .empty => {
                self.fieldSyms.buf[symId].mapT = .one;
                self.fieldSyms.buf[symId].inner = .{
                    .one = .{
                        .id = sid,
                        .offset = @intCast(u16, offset),
                    },
                };
            },
            .one => {
                // Convert to many.
                var key = ObjectSymKey{
                    .structId = self.fieldSyms.buf[symId].inner.one.id,
                    .symId = symId,
                };
                try self.fieldTable.putNoClobber(self.alloc, key, self.fieldSyms.buf[symId].inner.one.offset);

                key = .{
                    .structId = sid,
                    .symId = symId,
                };
                try self.fieldTable.putNoClobber(self.alloc, key, offset);

                self.fieldSyms.buf[symId].mapT = .many;
                self.fieldSyms.buf[symId].inner = .{
                    .many = .{
                        .mruStructId = sid,
                        .mruOffset = offset,
                    },
                };
            },
            .many => {
                const key = ObjectSymKey{
                    .structId = sid,
                    .symId = symId,
                };
                try self.fieldTable.putNoClobber(self.alloc, key, offset);
            },
            // else => stdx.panicFmt("unsupported {}", .{self.fieldSyms.buf[symId].mapT}),
        }
    }

    pub inline fn setTagLitSym(self: *VM, tid: TagTypeId, symId: SymbolId, val: u32) void {
        self.tagLitSyms.buf[symId].symT = .one;
        self.tagLitSyms.buf[symId].inner = .{
            .one = .{
                .id = tid,
                .val = val,
            },
        };
    }

    pub inline fn setVarSym(self: *VM, symId: SymbolId, sym: VarSymbol) void {
        self.varSyms.buf[symId] = sym;
    }

    pub inline fn setFuncSym(self: *VM, symId: SymbolId, sym: FuncSymbolEntry) void {
        self.funcSyms.buf[symId] = sym;
    }

    pub fn addMethodSym(self: *VM, id: StructId, symId: SymbolId, sym: SymbolEntry) !void {
        switch (self.methodSyms.buf[symId].mapT) {
            .empty => {
                self.methodSyms.buf[symId].mapT = .one;
                self.methodSyms.buf[symId].inner = .{
                    .one = .{
                        .id = id,
                        .sym = sym,
                    },
                };
            },
            .one => {
                // Convert to many.
                var key = ObjectSymKey{
                    .structId = self.methodSyms.buf[symId].inner.one.id,
                    .symId = symId,
                };
                self.methodTable.putAssumeCapacityNoClobber(key, self.methodSyms.buf[symId].inner.one.sym);

                key = .{
                    .structId = id,
                    .symId = symId,
                };
                self.methodTable.putAssumeCapacityNoClobber(key, sym);

                self.methodSyms.buf[symId].mapT = .many;
                self.methodSyms.buf[symId].inner = .{
                    .many = .{
                        .mruStructId = id,
                        .mruSym = sym,
                    },
                };
            },
            .many => {
                const key = ObjectSymKey{
                    .structId = id,
                    .symId = symId,
                };
                self.methodTable.putAssumeCapacityNoClobber(key, sym);
            },
            // else => stdx.panicFmt("unsupported {}", .{self.methodSyms.buf[symId].mapT}),
        }
    }

    pub fn setIndex(self: *VM, left: Value, index: Value, right: Value) !void {
        @setRuntimeSafety(debug);
        if (left.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                ListS => {
                    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                    const idx = @floatToInt(u32, index.toF64());
                    if (idx < list.len) {
                        list.buf[idx] = right;
                    } else {
                        // var i: u32 = @intCast(u32, list.val.items.len);
                        // try list.val.resize(self.alloc, idx + 1);
                        // while (i < idx) : (i += 1) {
                        //     list.val.items[i] = Value.None;
                        // }
                        // list.val.items[idx] = right;
                        return self.panic("Index out of bounds.");
                    }
                },
                MapS => {
                    const map = stdx.ptrAlignCast(*MapInner, &obj.map.inner);
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
            const obj = stdx.ptrAlignCast(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                ListS => {
                    @setRuntimeSafety(debug);
                    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                    const idx = @intCast(i32, list.len) + @floatToInt(i32, index.toF64());
                    if (idx < list.len) {
                        return list.buf[@intCast(u32, idx)];
                    } else {
                        return error.OutOfBounds;
                    }
                },
                MapS => {
                    @setRuntimeSafety(debug);
                    const map = stdx.ptrAlignCast(*MapInner, &obj.map.inner);
                    const key = Value.initF64(index.toF64());
                    if (map.get(self, key)) |val| {
                        return val;
                    } else return Value.None;
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
        if (left.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                ListS => {
                    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                    const idx = @floatToInt(u32, index.toF64());
                    if (idx < list.len) {
                        return list.buf[idx];
                    } else {
                        return error.OutOfBounds;
                    }
                },
                MapS => {
                    const map = stdx.ptrAlignCast(*MapInner, &obj.map.inner);
                    if (@call(.{ .modifier = .never_inline }, map.get, .{self, index})) |val| {
                        return val;
                    } else return Value.None;
                },
                else => {
                    return stdx.panic("expected map or list");
                },
            }
        } else {
            return stdx.panic("expected pointer");
        }
    }

    fn panicFmt(self: *VM, comptime format: []const u8, args: []const fmt.FmtValue) error{Panic, OutOfMemory} {
        @setCold(true);
        self.panicMsg = fmt.allocFormat(self.alloc, format, args) catch |err| {
            if (err == error.OutOfMemory) {
                return error.OutOfMemory;
            } else {
                stdx.panic("unexpected");
            }
        };
        log.debug("{s}", .{self.panicMsg});
        return error.Panic;
    }

    fn panic(self: *VM, comptime msg: []const u8) error{Panic, OutOfMemory} {
        @setCold(true);
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
                        const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                        for (list.items()) |it| {
                            if (it.isPointer()) {
                                const ptr = stdx.ptrAlignCast(*HeapObject, it.asPointer().?);
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

    pub inline fn retain(self: *VM, val: Value) linksection(".eval") void {
        if (TraceEnabled) {
            self.trace.numRetainAttempts += 1;
        }
        if (val.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer());
            obj.retainedCommon.rc += 1;
            log.debug("retain {} {}", .{obj.getUserTag(), obj.retainedCommon.rc});
            if (TrackGlobalRC) {
                self.refCounts += 1;
            }
            if (TraceEnabled) {
                self.trace.numRetains += 1;
            }
        }
    }

    pub inline fn retainInc(self: *const VM, val: Value, inc: u32) linksection(".eval") void {
        @setRuntimeSafety(debug);
        if (TraceEnabled) {
            self.trace.numRetainAttempts += inc;
        }
        if (val.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer());
            obj.retainedCommon.rc += inc;
            log.debug("retain {} {}", .{obj.getUserTag(), obj.retainedCommon.rc});
            if (TrackGlobalRC) {
                gvm.refCounts += inc;
            }
            if (TraceEnabled) {
                self.trace.numRetains += inc;
            }
        }
    }

    pub fn forceRelease(self: *VM, obj: *HeapObject) void {
        if (TraceEnabled) {
            self.trace.numForceReleases += 1;
        }
        switch (obj.retainedCommon.structId) {
            ListS => {
                const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                list.deinit(self.alloc);
                self.freeObject(obj);
                if (TrackGlobalRC) {
                    gvm.refCounts -= obj.retainedCommon.rc;
                }
            },
            MapS => {
                const map = stdx.ptrAlignCast(*MapInner, &obj.map.inner);
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

    fn setField(self: *VM, recv: Value, fieldId: SymbolId, val: Value) linksection(".eval") !void {
        if (recv.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
            const symMap = self.fieldSyms.buf[fieldId];
            switch (symMap.mapT) {
                .one => {
                    if (obj.common.structId == symMap.inner.one.id) {
                        obj.object.getValuePtr(symMap.inner.one.offset).* = val;
                    } else {
                        stdx.panic("TODO: set field fallback");
                    }
                },
                .many => {
                    stdx.fatal();
                },
                .empty => {
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

    fn getFieldOffsetFromTable(self: *VM, sid: StructId, symId: SymbolId) u8 {
        if (self.fieldTable.get(.{ .structId = sid, .symId = symId })) |offset| {
            self.fieldSyms.buf[symId].inner.many = .{
                .mruStructId = sid,
                .mruOffset = offset,
            };
            return @intCast(u8, offset);
        } else {
            return NullByteId;
        }
    }

    pub fn getFieldOffset(self: *VM, obj: *HeapObject, symId: SymbolId) linksection(section) u8 {
        const symMap = self.fieldSyms.buf[symId];
        switch (symMap.mapT) {
            .one => {
                if (obj.common.structId == symMap.inner.one.id) {
                    return @intCast(u8, symMap.inner.one.offset);
                } else {
                    return NullByteId;
                }
            },
            .many => {
                if (obj.common.structId == symMap.inner.many.mruStructId) {
                    return @intCast(u8, symMap.inner.many.mruOffset);
                } else {
                    return @call(.{ .modifier = .never_inline }, self.getFieldOffsetFromTable, .{obj.common.structId, symId});
                }
            },
            .empty => {
                return NullByteId;
            },
            // else => {
            //     // stdx.panicFmt("unsupported {}", .{symMap.mapT});
            //     unreachable;
            // },
        } 
    }

    pub fn setFieldRelease(self: *VM, recv: Value, symId: SymbolId, val: Value) linksection(section) !void {
        @setCold(true);
        if (recv.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer().?);
            const offset = self.getFieldOffset(obj, symId);
            if (offset != NullByteId) {
                const lastValue = obj.object.getValuePtr(offset);
                release(self, lastValue.*);
                lastValue.* = val;
            } else {
                return self.getFieldMissingSymbolError();
            }
        } else {
            return self.getFieldMissingSymbolError();
        }
    }

    pub fn getField(self: *VM, recv: Value, symId: SymbolId) linksection(section) !Value {
        if (recv.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer().?);
            const offset = self.getFieldOffset(obj, symId);
            if (offset != NullByteId) {
                return obj.object.getValue(offset);
            } else {
                return self.getFieldFallback(obj, self.fieldSyms.buf[symId].name);
            }
        } else {
            return self.getFieldMissingSymbolError();
        }
    }

    fn getFieldFallback(self: *const VM, obj: *const HeapObject, name: []const u8) linksection(".eval") Value {
        @setCold(true);
        if (obj.common.structId == MapS) {
            const map = stdx.ptrAlignCast(*const MapInner, &obj.map.inner);
            if (map.getByString(self, name)) |val| {
                return val;
            } else return Value.None;
        } else {
            log.debug("Missing symbol for object: {}", .{obj.common.structId});
            return Value.None;
        }
    }

    /// startLocal points to the first arg in the current stack frame.
    fn callSym(self: *VM, pc: [*]cy.OpData, framePtr: [*]Value, symId: SymbolId, startLocal: u8, numArgs: u8, reqNumRetVals: u2) linksection(section) !PcFramePtr {
        const sym = self.funcSyms.buf[symId];
        switch (sym.entryT) {
            .nativeFunc1 => {
                const newFramePtr = framePtr + startLocal;

                // Optimize.
                pc[0] = cy.OpData{ .code = .callNativeFuncIC };
                @ptrCast(*align(1) u48, pc + 5).* = @intCast(u48, @ptrToInt(sym.inner.nativeFunc1));

                gvm.framePtr = newFramePtr;
                const res = sym.inner.nativeFunc1(@ptrCast(*UserVM, self), @ptrCast([*]const Value, newFramePtr + 4), numArgs);
                if (reqNumRetVals == 1) {
                    newFramePtr[0] = res;
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            // Nop.
                        },
                        1 => stdx.panic("not possible"),
                        else => stdx.panic("unsupported"),
                    }
                }
                return PcFramePtr{
                    .pc = pc + 11,
                    .framePtr = framePtr,
                };
            },
            .func => {
                if (@ptrToInt(framePtr + startLocal + sym.inner.func.numLocals) >= @ptrToInt(self.stackEndPtr)) {
                    return error.StackOverflow;
                }

                // Optimize.
                pc[0] = cy.OpData{ .code = .callFuncIC };
                pc[4] = cy.OpData{ .arg = @intCast(u8, sym.inner.func.numLocals) };
                @ptrCast(*align(1) u48, pc + 5).* = @intCast(u48, @ptrToInt(toPc(sym.inner.func.pc)));

                const newFramePtr = framePtr + startLocal;
                newFramePtr[1] = buildReturnInfo(reqNumRetVals, true);
                newFramePtr[2] = Value{ .retPcPtr = pc + 11 };
                newFramePtr[3] = Value{ .retFramePtr = framePtr };
                return PcFramePtr{
                    .pc = toPc(sym.inner.func.pc),
                    .framePtr = newFramePtr,
                };
            },
            .closure => {
                if (@ptrToInt(framePtr + startLocal + sym.inner.closure.numLocals) >= @ptrToInt(gvm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const newFramePtr = framePtr + startLocal;
                newFramePtr[1] = buildReturnInfo(reqNumRetVals, true);
                newFramePtr[2] = Value{ .retPcPtr = pc + 11 };
                newFramePtr[3] = Value{ .retFramePtr = framePtr };

                // Copy over captured vars to new call stack locals.
                if (sym.inner.closure.numCaptured <= 3) {
                    const src = @ptrCast([*]Value, &sym.inner.closure.capturedVal0)[0..sym.inner.closure.numCaptured];
                    std.mem.copy(Value, newFramePtr[numArgs + 4..numArgs + 4 + sym.inner.closure.numCaptured], src);
                } else {
                    stdx.panic("unsupported closure > 3 captured args.");
                }

                return PcFramePtr{
                    .pc = toPc(sym.inner.closure.funcPc),
                    .framePtr = newFramePtr,
                };
            },
            else => {
                return self.panic("unsupported callsym");
            },
        }
    }

    fn callSymEntry(self: *VM, pc: [*]cy.OpData, framePtr: [*]Value, sym: SymbolEntry, obj: *HeapObject, typeId: u32, startLocal: u8, numArgs: u8, reqNumRetVals: u8) linksection(".eval") !PcFramePtr {
        switch (sym.entryT) {
            .func => {
                if (@ptrToInt(framePtr + startLocal + sym.inner.func.numLocals) >= @ptrToInt(gvm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                // Optimize.
                pc[0] = cy.OpData{ .code = .callObjFuncIC };
                pc[5] = cy.OpData{ .arg = @intCast(u8, sym.inner.func.numLocals) };
                @ptrCast(*align(1) u48, pc + 6).* = @intCast(u48, @ptrToInt(toPc(sym.inner.func.pc)));
                @ptrCast(*align(1) u16, pc + 12).* = @intCast(u16, typeId);
                
                const newFramePtr = framePtr + startLocal;
                newFramePtr[1] = buildReturnInfo2(reqNumRetVals, true);
                newFramePtr[2] = Value{ .retPcPtr = pc + 14 };
                newFramePtr[3] = Value{ .retFramePtr = framePtr };
                return PcFramePtr{
                    .pc = toPc(sym.inner.func.pc),
                    .framePtr = newFramePtr,
                };
            },
            .nativeFunc1 => {
                // Optimize.
                pc[0] = cy.OpData{ .code = .callObjNativeFuncIC };
                @ptrCast(*align(1) u48, pc + 6).* = @intCast(u48, @ptrToInt(sym.inner.nativeFunc1));
                @ptrCast(*align(1) u16, pc + 12).* = @intCast(u16, typeId);

                const newFramePtr = framePtr + startLocal;
                gvm.framePtr = newFramePtr;
                const res = sym.inner.nativeFunc1(@ptrCast(*UserVM, self), obj, @ptrCast([*]const Value, newFramePtr + 4), numArgs);
                if (reqNumRetVals == 1) {
                    newFramePtr[0] = res;
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            // Nop.
                        },
                        1 => stdx.panic("not possible"),
                        else => {
                            stdx.panic("unsupported");
                        },
                    }
                }
                return PcFramePtr{
                    .pc = pc + 14,
                    .framePtr = framePtr,
                };
            },
            .nativeFunc2 => {
                const newFramePtr = framePtr + startLocal;
                gvm.framePtr = newFramePtr;
                const res = sym.inner.nativeFunc2(@ptrCast(*UserVM, self), obj, @ptrCast([*]const Value, newFramePtr + 4), numArgs);
                if (reqNumRetVals == 2) {
                    newFramePtr[0] = res.left;
                    newFramePtr[1] = res.right;
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            release(self, res.left);
                            release(self, res.right);
                        },
                        1 => {
                            newFramePtr[0] = res.left;
                            release(self, res.right);
                        },
                        else => {
                            stdx.panic("unsupported");
                        },
                    }
                }
                return PcFramePtr{
                    .pc = pc + 14,
                    .framePtr = framePtr,
                };
            },
            // else => {
            //     // stdx.panicFmt("unsupported {}", .{sym.entryT});
            //     unreachable;
            // },
        }
    }

    fn getCallObjSymFromTable(self: *VM, sid: StructId, symId: SymbolId) ?SymbolEntry {
        if (self.methodTable.get(.{ .structId = sid, .symId = symId })) |entry| {
            self.methodSyms.buf[symId].inner.many = .{
                .mruStructId = sid,
                .mruSym = entry,
            };
            return entry;
        } else {
            return null;
        }
    }

    fn getCallObjSym(self: *VM, typeId: u32, symId: SymbolId) linksection(section) ?SymbolEntry {
        const map = self.methodSyms.buf[symId];
        switch (map.mapT) {
            .one => {
                if (typeId == map.inner.one.id) {
                    return map.inner.one.sym;
                } else return null;
            },
            .many => {
                if (map.inner.many.mruStructId == typeId) {
                    return map.inner.many.mruSym;
                } else {
                    return @call(.{ .modifier = .never_inline }, self.getCallObjSymFromTable, .{typeId, symId});
                }
            },
            .empty => {
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
    fn callObjSym(self: *VM, pc: [*]const cy.OpData, framePtr: [*]Value, recv: Value, symId: SymbolId, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") !PcFramePtr {
        if (recv.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer().?);
            const map = self.methodSyms.buf[symId];
            switch (map.mapT) {
                .one => {
                    if (obj.retainedCommon.structId == map.inner.one.id) {
                        return try @call(.{.modifier = .never_inline }, callSymEntryNoInline, .{pc, framePtr, map.inner.one.sym, obj, startLocal, numArgs, reqNumRetVals});
                    } else return self.panic("Symbol does not exist for receiver.");
                },
                .many => {
                    if (map.inner.many.mruStructId == obj.retainedCommon.structId) {
                        return try @call(.{ .modifier = .never_inline }, callSymEntryNoInline, .{pc, framePtr, map.inner.many.mruSym, obj, startLocal, numArgs, reqNumRetVals});
                    } else {
                        const sym = self.methodTable.get(.{ .structId = obj.retainedCommon.structId, .methodId = symId }) orelse {
                            log.debug("Symbol does not exist for receiver.", .{});
                            stdx.fatal();
                        };
                        self.methodSyms.buf[symId].inner.many = .{
                            .mruStructId = obj.retainedCommon.structId,
                            .mruSym = sym,
                        };
                        return try @call(.{ .modifier = .never_inline }, callSymEntryNoInline, .{pc, framePtr, sym, obj, startLocal, numArgs, reqNumRetVals});
                    }
                },
                .empty => {
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

    fn computeLinePosFromSrc(self: *const VM, loc: u32, outLine: *u32, outCol: *u32, outLineStart: *u32) void {
        var line: u32 = 0;
        var lineStart: u32 = 0;
        for (self.parser.src.items) |ch, i| {
            if (i == loc) {
                outLine.* = line;
                outCol.* = loc - lineStart;
                outLineStart.* = lineStart;
                return;
            }
            if (ch == '\n') {
                line += 1;
                lineStart = @intCast(u32, i + 1);
            }
        }
    }

    fn computeLinePos(self: *const VM, loc: u32, outLine: *u32, outCol: *u32, outLineStart: *u32) void {
        var line: u32 = 0;
        var lineStart: u32 = 0;
        for (self.parser.tokens.items) |token| {
            if (token.pos() == loc) {
                outLine.* = line;
                outCol.* = loc - lineStart;
                outLineStart.* = lineStart;
                return;
            }
            if (token.tag() == .new_line) {
                line += 1;
                lineStart = token.pos() + 1;
            }
        }
    }

    pub fn buildStackTrace(self: *VM, fromPanic: bool) !void {
        @setCold(true);
        self.stackTrace.deinit(self.alloc);
        var frames: std.ArrayListUnmanaged(StackFrame) = .{};

        var framePtr = framePtrOffset(self.framePtr);
        var pc = pcOffset(self.pc);
        var isTopFrame = true;
        while (true) {
            const idx = b: {
                if (isTopFrame) {
                    isTopFrame = false;
                    if (fromPanic) {
                        const len = cy.getInstLenAt(self.ops.ptr + pc);
                        break :b self.indexOfDebugSym(pc + len) orelse return error.NoDebugSym;
                    }
                }
                break :b self.indexOfDebugSym(pc) orelse return error.NoDebugSym;
            };
            const sym = self.debugTable[idx];

            if (sym.frameLoc == NullId) {
                const node = self.compiler.nodes[sym.loc];
                var line: u32 = undefined;
                var col: u32 = undefined;
                var lineStart: u32 = undefined;
                self.computeLinePos(self.compiler.tokens[node.start_token].pos(), &line, &col, &lineStart);
                try frames.append(self.alloc, .{
                    .name = "main",
                    .uri = self.mainUri,
                    .line = line,
                    .col = col,
                    .lineStartPos = lineStart,
                });
                break;
            } else {
                const frameNode = self.compiler.nodes[sym.frameLoc];
                const func = self.compiler.funcDecls[frameNode.head.func.decl_id];
                const name = self.compiler.src[func.name.start..func.name.end];

                const node = self.compiler.nodes[sym.loc];
                var line: u32 = undefined;
                var col: u32 = undefined;
                var lineStart: u32 = undefined;
                self.computeLinePos(self.compiler.tokens[node.start_token].pos(), &line, &col, &lineStart);
                try frames.append(self.alloc, .{
                    .name = name,
                    .uri = self.mainUri,
                    .line = line,
                    .col = col,
                    .lineStartPos = lineStart,
                });
                pc = pcOffset(self.stack[framePtr + 2].retPcPtr);
                framePtr = framePtrOffset(self.stack[framePtr + 3].retFramePtr);
            }
        }

        self.stackTrace.frames = try frames.toOwnedSlice(self.alloc);
    }

    pub fn valueAsString(self: *const VM, val: Value) []const u8 {
        if (val.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
            return obj.string.ptr[0..obj.string.len];
        } else {
            // Assume const string.
            const slice = val.asConstStr();
            return self.strBuf[slice.start..slice.end];
        }
    }

    pub fn valueToString(self: *const VM, val: Value) ![]const u8 {
        const str = self.valueToTempString(val);
        return try self.alloc.dupe(u8, str);
    }

    /// Conversion goes into a temporary buffer. Must use the result before a subsequent call.
    pub fn valueToTempString(self: *const VM, val: Value) linksection(".eval2") []const u8 {
        if (val.isNumber()) {
            const f = val.asF64();
            if (Value.floatIsSpecial(f)) {
                return std.fmt.bufPrint(&tempU8Buf, "{}", .{f}) catch stdx.fatal();
            } else {
                if (Value.floatCanBeInteger(f)) {
                    return std.fmt.bufPrint(&tempU8Buf, "{d:.0}", .{f}) catch stdx.fatal();
                } else {
                    return std.fmt.bufPrint(&tempU8Buf, "{d:.10}", .{f}) catch stdx.fatal();
                }
            }
        } else {
            if (val.isPointer()) {
                const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
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
                    cy.BooleanT => {
                        if (val.asBool()) return "true" else return "false";
                    },
                    cy.NoneT => return "none",
                    cy.ConstStringT => {
                        // Convert into heap string.
                        const slice = val.asConstStr();
                        return self.strBuf[slice.start..slice.end];
                    },
                    cy.IntegerT => return std.fmt.bufPrint(&tempU8Buf, "{}", .{val.asI32()}) catch stdx.fatal(),
                    else => {
                        log.debug("unexpected tag {}", .{val.getTag()});
                        stdx.fatal();
                    },
                }
            }
        }
    }

    fn writeValueToString(self: *const VM, writer: anytype, val: Value) void {
        const str = self.valueToTempString(val);
        _ = writer.write(str) catch stdx.fatal();
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
        var betterCap = newCap;
        while (true) {
            betterCap +|= betterCap / 2 + 8;
            if (betterCap >= newCap) {
                break;
            }
        }
        if (self.alloc.resize(self.stack, betterCap)) {
            self.stack.len = betterCap;
            self.stackEndPtr = self.stack.ptr + betterCap;
        } else {
            self.stack = try self.alloc.realloc(self.stack, betterCap);
            self.stackEndPtr = self.stack.ptr + betterCap;
        }
    }

    pub fn stackGrowTotalCapacityPrecise(self: *VM, newCap: usize) !void {
        if (self.alloc.resize(self.stack, newCap)) {
            self.stack.len = newCap;
            self.stackEndPtr = self.stack.ptr + newCap;
        } else {
            self.stack = try self.alloc.realloc(self.stack, newCap);
            self.stackEndPtr = self.stack.ptr + newCap;
        }
    }
};

pub fn releaseObject(vm: *VM, obj: *HeapObject) linksection(section) void {
    if (builtin.mode == .Debug or builtin.is_test) {
        if (obj.retainedCommon.structId == NullId) {
            stdx.panic("object already freed.");
        }
    }
    obj.retainedCommon.rc -= 1;
    log.debug("release {} {}", .{obj.getUserTag(), obj.retainedCommon.rc});
    if (TrackGlobalRC) {
        vm.refCounts -= 1;
    }
    if (TraceEnabled) {
        vm.trace.numReleases += 1;
        vm.trace.numReleaseAttempts += 1;
    }
    if (obj.retainedCommon.rc == 0) {
        @call(.{ .modifier = .never_inline }, freeObject, .{vm, obj});
    }
}

fn freeObject(vm: *VM, obj: *HeapObject) linksection(".eval") void {
    log.debug("free {}", .{obj.getUserTag()});
    switch (obj.retainedCommon.structId) {
        ListS => {
            const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
            for (list.items()) |it| {
                release(vm, it);
            }
            list.deinit(vm.alloc);
            vm.freeObject(obj);
        },
        MapS => {
            const map = stdx.ptrAlignCast(*MapInner, &obj.map.inner);
            var iter = map.iterator();
            while (iter.next()) |entry| {
                release(vm, entry.key);
                release(vm, entry.value);
            }
            map.deinit(vm.alloc);
            vm.freeObject(obj);
        },
        ClosureS => {
            if (obj.closure.numCaptured <= 3) {
                const src = @ptrCast([*]Value, &obj.closure.capturedVal0)[0..obj.closure.numCaptured];
                for (src) |capturedVal| {
                    release(vm, capturedVal);
                }
                vm.freeObject(obj);
            } else {
                stdx.panic("unsupported");
            }
        },
        LambdaS => {
            vm.freeObject(obj);
        },
        StringS => {
            vm.alloc.free(obj.string.ptr[0..obj.string.len]);
            vm.freeObject(obj);
        },
        FiberS => {
            releaseFiberStack(vm, &obj.fiber);
            vm.freeObject(obj);
        },
        BoxS => {
            release(vm, obj.box.val);
            vm.freeObject(obj);
        },
        NativeFunc1S => {
            if (obj.nativeFunc1.hasTccState) {
                releaseObject(vm, stdx.ptrAlignCast(*HeapObject, obj.nativeFunc1.tccState.asPointer().?));
            }
            vm.freeObject(obj);
        },
        TccStateS => {
            tcc.tcc_delete(obj.tccState.state);
            vm.freeObject(obj);
        },
        OpaquePtrS => {
            vm.freeObject(obj);
        },
        else => {
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
                release(vm, child);
            }
            if (numFields <= 4) {
                vm.freeObject(obj);
            } else {
                vm.alloc.destroy(obj);
            }
        },
    }
}

pub fn release(vm: *VM, val: Value) linksection(".eval") void {
    if (TraceEnabled) {
        vm.trace.numReleaseAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
        if (builtin.mode == .Debug or builtin.is_test) {
            if (obj.retainedCommon.structId == NullId) {
                stdx.panic("object already freed.");
            }
        }
        obj.retainedCommon.rc -= 1;
        log.debug("release {} {}", .{val.getUserTag(), obj.retainedCommon.rc});
        if (TrackGlobalRC) {
            vm.refCounts -= 1;
        }
        if (TraceEnabled) {
            vm.trace.numReleases += 1;
        }
        if (obj.retainedCommon.rc == 0) {
            @call(.{ .modifier = .never_inline }, freeObject, .{vm, obj});
        }
    }
}

fn evalBitwiseOr(left: Value, right: Value) linksection(".eval") Value {
    @setCold(true);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asF64toI32() | @floatToInt(i32, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalBitwiseXor(left: Value, right: Value) linksection(".eval") Value {
    @setCold(true);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asF64toI32() ^ @floatToInt(i32, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalBitwiseAnd(left: Value, right: Value) linksection(".eval") Value {
    @setCold(true);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asF64toI32() & @floatToInt(i32, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalBitwiseLeftShift(left: Value, right: Value) linksection(".eval") Value {
    @setCold(true);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asF64toI32() << @floatToInt(u5, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalBitwiseRightShift(left: Value, right: Value) linksection(".eval") Value {
    @setCold(true);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asF64toI32() >> @floatToInt(u5, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalBitwiseNot(val: Value) linksection(".eval") Value {
    @setCold(true);
    if (val.isNumber()) {
       const f = @intToFloat(f64, ~val.asF64toI32());
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
        const ltag = left.getTag();
        const rtag = left.getTag();
        if (ltag != rtag) {
            return Value.True;
        }
        switch (ltag) {
            cy.NoneT => return Value.initBool(!right.isNone()),
            cy.BooleanT => return Value.initBool(left.asBool() != right.asBool()),
            cy.ConstStringT => {
                if (right.isString()) {
                    const slice = left.asConstStr();
                    const str = gvm.strBuf[slice.start..slice.end];
                    return Value.initBool(!std.mem.eql(u8, str, gvm.valueAsString(right)));
                } return Value.True;
            },
            cy.UserTagLiteralT => return Value.initBool(left.asTagLiteralId() != right.asTagLiteralId()),
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalCompareFallback(left: Value, right: Value) linksection(".eval") Value {
    @setRuntimeSafety(debug);
    @setCold(true);
    if (left.isPointer()) {
        const obj = stdx.ptrAlignCast(*HeapObject, left.asPointer().?);
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
            cy.NoneT => return Value.initBool(right.isNone()),
            cy.BooleanT => return Value.initBool(left.asBool() == right.toBool()),
            cy.ConstStringT => {
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
            cy.BooleanT => {
                if (left.asBool()) {
                    return Value.initF64(1 - right.toF64());
                } else {
                    return Value.initF64(-right.toF64());
                }
            },
            cy.NoneT => return Value.initF64(-right.toF64()),
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
            cy.BooleanT => {
                if (left.asBool()) {
                    return Value.initF64(1);
                } else {
                    return Value.initF64(0);
                }
            },
            cy.NoneT => return Value.initF64(0),
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
            cy.BooleanT => {
                if (left.asBool()) {
                    return Value.initF64(1.0 / right.toF64());
                } else {
                    return Value.initF64(0);
                }
            },
            cy.NoneT => return Value.initF64(0),
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
            cy.BooleanT => {
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
            cy.NoneT => {
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
            cy.BooleanT => {
                if (left.asBool()) {
                    return Value.initF64(right.toF64());
                } else {
                    return Value.initF64(0);
                }
            },
            cy.NoneT => return Value.initF64(0),
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalAddFallback(left: cy.Value, right: cy.Value) linksection(".eval") !cy.Value {
    @setCold(true);
    return Value.initF64(try toF64OrPanic(left) + try toF64OrPanic(right));
}

fn toF64OrPanic(val: Value) linksection(".eval") !f64 {
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
            cy.NoneT => return 0,
            cy.BooleanT => return if (val.asBool()) 1 else 0,
            cy.IntegerT => return @intToFloat(f64, val.asI32()),
            cy.ErrorT => stdx.fatal(),
            cy.ConstStringT => {
                const slice = val.asConstStr();
                const str = gvm.strBuf[slice.start..slice.end];
                return std.fmt.parseFloat(f64, str) catch 0;
            },
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
            cy.NoneT => return Value.initF64(0),
            cy.BooleanT => {
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
            cy.NoneT => return Value.True,
            cy.BooleanT => return Value.initBool(!val.asBool()),
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

pub const OpaquePtr = packed struct {
    structId: StructId,
    rc: u32,
    ptr: ?*anyopaque,
};

const TccState = packed struct {
    structId: StructId,
    rc: u32,
    state: *tcc.TCCState,
};

const NativeFunc1 = packed struct {
    structId: StructId,
    rc: u32,
    func: *const fn (*UserVM, [*]Value, u8) Value,
    tccState: Value,
    hasTccState: bool,
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
};

const Box = packed struct {
    structId: StructId,
    rc: u32,
    val: Value,
};

pub const Fiber = packed struct {
    structId: StructId,
    rc: u32,
    prevFiber: ?*Fiber,
    stackPtr: [*]Value,
    stackLen: u32,
    /// If pc == NullId, the fiber is done.
    pc: u32,

    /// [*]Value reduced to 48 bits.
    framePtr: u48,

    /// Where coyield and coreturn should copy the return value to.
    /// If this is the NullByteId, no value is copied and instead released.
    parentDstLocal: u8,
};

pub const List = packed struct {
    structId: StructId,
    rc: u32,
    list: packed struct {
        ptr: [*]Value,
        cap: usize,
        len: usize,
    },
    nextIterIdx: u32,

    pub inline fn items(self: *const List) []Value {
        return self.list.ptr[0..self.list.len];
    }
};

const Object = packed struct {
    structId: StructId,
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
    object: Object,
    box: Box,
    nativeFunc1: NativeFunc1,
    tccState: TccState,
    opaquePtr: OpaquePtr,

    pub fn getUserTag(self: *const HeapObject) cy.ValueUserTag {
        switch (self.common.structId) {
            cy.ListS => return .list,
            cy.MapS => return .map,
            cy.StringS => return .string,
            cy.ClosureS => return .closure,
            cy.LambdaS => return .lambda,
            cy.FiberS => return .fiber,
            cy.NativeFunc1S => return .nativeFunc,
            cy.TccStateS => return .tccState,
            cy.OpaquePtrS => return .opaquePtr,
            cy.BoxS => return .box,
            else => {
                return .object;
            },
        }
    }
};

const SymbolMapType = enum {
    one,
    // two,
    // ring, // Sorted mru, up to 8 syms.
    many,
    empty,
};

/// Keeping this small is better for function calls. TODO: Reduce size.
/// Secondary symbol data should be moved to `methodSymExtras`.
const SymbolMap = struct {
    mapT: SymbolMapType,
    inner: union {
        one: struct {
            id: StructId,
            sym: SymbolEntry,
        },
        // two: struct {
        // },
        // ring: struct {
        // },

        many: struct {
            mruStructId: StructId,
            mruSym: SymbolEntry,
        },
    },
};

const TagLitSym = struct {
    symT: SymbolMapType,
    inner: union {
        one: struct {
            id: TagTypeId,
            val: u32,
        },
    },
    name: []const u8,
};

const FieldSymbolMap = struct {
    mapT: SymbolMapType,
    inner: union {
        one: struct {
            id: StructId,
            offset: u16,
        },
        many: struct {
            mruStructId: StructId,
            mruOffset: u16,
        },
    },
    name: []const u8,
};

test "Internals." {
    try t.eq(@alignOf(VM), 8);
    try t.eq(@sizeOf(SymbolEntry), 16);
    try t.eq(@alignOf(SymbolMap), 8);
    try t.eq(@sizeOf(SymbolMap), 40);
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

const NativeObjFuncPtr = *const fn (*UserVM, *anyopaque, [*]const Value, u8) Value;
const NativeObjFunc2Ptr = *const fn (*UserVM, *anyopaque, [*]const Value, u8) cy.ValuePair;
const NativeFuncPtr = *const fn (*UserVM, [*]const Value, u8) Value;

pub const SymbolEntry = struct {
    entryT: SymbolEntryType,
    inner: packed union {
        nativeFunc1: NativeObjFuncPtr,
        nativeFunc2: *const fn (*UserVM, *anyopaque, [*]const Value, u8) cy.ValuePair,
        func: packed struct {
            // pc: packed union {
            //     ptr: [*]const cy.OpData,
            //     offset: usize,
            // },
            pc: u32,
            /// Includes function params, locals, and return info slot.
            numLocals: u32,
        },
    },

    pub fn initFuncOffset(pc: usize, numLocals: u32) SymbolEntry {
        return .{
            .entryT = .func,
            .inner = .{
                .func = .{
                    .pc = @intCast(u32, pc),
                    .numLocals = numLocals,
                },
            },
        };
    }

    pub fn initNativeFunc1(func: NativeObjFuncPtr) SymbolEntry {
        return .{
            .entryT = .nativeFunc1,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    pub fn initNativeFunc2(func: NativeObjFunc2Ptr) SymbolEntry {
        return .{
            .entryT = .nativeFunc2,
            .inner = .{
                .nativeFunc2 = func,
            },
        };
    }
};

const VarSymbolType = enum {
    value,
    none,
};

pub const VarSymbol = struct {
    symT: VarSymbolType,
    inner: packed union {
        value: Value,
    },

    pub fn initValue(val: Value) VarSymbol {
        return .{
            .symT = .value,
            .inner = .{
                .value = val,
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

pub const FuncSymDetail = struct {
    name: []const u8,
};

/// TODO: Rename to FuncSymbol.
pub const FuncSymbolEntry = struct {
    entryT: FuncSymbolEntryType,
    inner: packed union {
        nativeFunc1: *const fn (*UserVM, [*]const Value, u8) Value,
        func: packed struct {
            // pc: packed union {
            //     ptr: [*]const cy.OpData,
            //     offset: usize,
            // },
            pc: u32,
            /// Includes locals, and return info slot. Does not include params.
            numLocals: u32,
        },
        closure: *Closure,
    },

    pub fn initNativeFunc1(func: *const fn (*UserVM, [*]const Value, u8) Value) FuncSymbolEntry {
        return .{
            .entryT = .nativeFunc1,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    pub fn initFuncOffset(pc: usize, numLocals: u32) FuncSymbolEntry {
        return .{
            .entryT = .func,
            .inner = .{
                .func = .{
                    .pc = @intCast(u32, pc),
                    .numLocals = numLocals,
                },
            },
        };
    }
};

const TagTypeId = u32;
const TagType = struct {
    name: []const u8,
    numMembers: u32,
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

/// A simplified VM handle.
pub const UserVM = struct {
    dummy: u64 = 0,

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

    pub fn dumpPanicStackTrace(self: *UserVM) !void {
        @setCold(true);
        const vm = @ptrCast(*VM, self);
        if (builtin.is_test) {
            log.debug("panic: {s}", .{vm.panicMsg});
        } else {
            try fmt.printStderr("panic: {}\n\n", &.{fmt.v(vm.panicMsg)});
        }
        const trace = vm.getStackTrace();
        try trace.dump(vm);
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

    pub inline fn releaseObject(self: *UserVM, obj: *HeapObject) void {
        Root.releaseObject(@ptrCast(*VM, self), obj);
    }

    pub inline fn release(self: *UserVM, val: Value) void {
        Root.release(@ptrCast(*VM, self), val);
    }

    pub inline fn retainObject(self: *UserVM, obj: *HeapObject) void {
        @ptrCast(*VM, self).retainObject(obj);
    }

    pub inline fn getGlobalRC(_: UserVM) usize {
        return gvm.getGlobalRC();
    }

    pub inline fn checkMemory(_: UserVM) !bool {
        return gvm.checkMemory();
    }

    pub inline fn compile(_: UserVM, srcUri: []const u8, src: []const u8) !cy.ByteCodeBuffer {
        return gvm.compile(srcUri, src);
    }

    pub inline fn eval(_: UserVM, srcUri: []const u8, src: []const u8) !Value {
        return gvm.eval(srcUri, src);
    }

    pub inline fn allocator(self: *const UserVM) std.mem.Allocator {
        return @ptrCast(*const VM, self).alloc;
    }

    pub inline fn allocString(self: *UserVM, str: []const u8) !Value {
        return @ptrCast(*VM, self).allocString(str);
    }

    pub inline fn allocOwnedString(self: *UserVM, str: []u8) !Value {
        return @ptrCast(*VM, self).allocOwnedString(str);
    }

    pub inline fn valueAsString(self: *UserVM, val: Value) []const u8 {
        return @ptrCast(*const VM, self).valueAsString(val);
    }

    pub inline fn valueToTempString(self: *UserVM, val: Value) []const u8 {
        return @ptrCast(*const VM, self).valueToTempString(val);
    }

    pub inline fn valueToString(self: *UserVM, val: Value) ![]const u8 {
        return @ptrCast(*const VM, self).valueToString(val);
    }
};

/// To reduce the amount of code inlined in the hot loop, handle StackOverflow at the top and resume execution.
/// This is also the entry way for native code to call into the VM without deoptimizing the hot loop.
pub fn evalLoopGrowStack(vm: *VM) linksection(section) error{StackOverflow, OutOfMemory, Panic, OutOfBounds, NoDebugSym, End}!void {
    while (true) {
        @call(.{ .modifier = .always_inline }, evalLoop, .{vm}) catch |err| {
            if (err == error.StackOverflow) {
                log.debug("grow stack", .{});
                try @call(.{ .modifier = .never_inline }, growStackAuto, .{ vm });
                continue;
            } else if (err == error.End) {
                return;
            } else if (err == error.Panic) {
                try @call(.{ .modifier = .never_inline }, gvm.buildStackTrace, .{true});
                return error.Panic;
            } else return err;
        };
        return;
    }
}

fn evalLoop(vm: *VM) linksection(section) error{StackOverflow, OutOfMemory, Panic, OutOfBounds, NoDebugSym, End}!void {
    var pc = vm.pc;
    var framePtr = vm.framePtr;
    defer {
        vm.pc = pc;
        vm.framePtr = framePtr;
    }

    while (true) {
        if (TraceEnabled) {
            const op = pc[0].code;
            vm.trace.opCounts[@enumToInt(op)].count += 1;
            vm.trace.totalOpCounts += 1;
        }
        if (builtin.mode == .Debug) {
            dumpEvalOp(vm, pc);
        }
        switch (pc[0].code) {
            .true => {
                framePtr[pc[1].arg] = Value.True;
                pc += 2;
                continue;
            },
            .false => {
                framePtr[pc[1].arg] = Value.False;
                pc += 2;
                continue;
            },
            .none => {
                framePtr[pc[1].arg] = Value.None;
                pc += 2;
                continue;
            },
            .constOp => {
                framePtr[pc[2].arg] = Value.initRaw(vm.consts[pc[1].arg].val);
                pc += 3;
                continue;
            },
            .constI8 => {
                framePtr[pc[2].arg] = Value.initF64(@intToFloat(f64, @bitCast(i8, pc[1].arg)));
                pc += 3;
                continue;
            },
            .constI8Int => {
                framePtr[pc[2].arg] = Value.initI32(@intCast(i32, @bitCast(i8, pc[1].arg)));
                pc += 3;
                continue;
            },
            .release => {
                release(vm, framePtr[pc[1].arg]);
                pc += 2;
                continue;
            },
            .releaseN => {
                const numLocals = pc[1].arg;
                for (pc[2..2+numLocals]) |local| {
                    release(vm, framePtr[local.arg]);
                }
                pc += 2 + numLocals;
                continue;
            },
            .fieldIC => {
                const recv = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    if (obj.common.structId == @ptrCast(*align (1) u16, pc + 4).*) {
                        framePtr[dst] = obj.object.getValue(pc[6].arg);
                        pc += 7;
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.OpData{ .code = .field };
                // framePtr[dst] = try gvm.getField(recv, pc[3].arg);
                framePtr[dst] = try @call(.{ .modifier = .never_inline }, gvm.getField, .{ recv, pc[3].arg });
                pc += 7;
                continue;
            },
            .copyRetainSrc => {
                const val = framePtr[pc[1].arg];
                framePtr[pc[2].arg] = val;
                vm.retain(val);
                pc += 3;
                continue;
            },
            .jumpNotCond => {
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
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = if (Value.bothNumbers(left, right))
                    Value.initBool(left.asF64() < right.asF64())
                else
                    @call(.{ .modifier = .never_inline }, evalLessFallback, .{left, right});
                pc += 4;
                continue;
            },
            .lessInt => {
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = Value.initBool(left.asI32() < right.asI32());
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
            .add => {
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
            .addInt => {
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = Value.initI32(left.asI32() + right.asI32());
                pc += 4;
                continue;
            },
            .minus => {
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = if (Value.bothNumbers(left, right))
                    Value.initF64(left.asF64() - right.asF64())
                else @call(.{ .modifier = .never_inline }, evalMinusFallback, .{left, right});
                pc += 4;
                continue;
            },
            .minusInt => {
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = Value.initI32(left.asI32() - right.asI32());
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
            .objectSmall => {
                const sid = pc[1].arg;
                const startLocal = pc[2].arg;
                const numProps = pc[3].arg;
                const dst = pc[4].arg;
                const offsets = pc[5..5+numProps];
                pc += 5 + numProps;

                const props = framePtr[startLocal .. startLocal + numProps];
                framePtr[dst] = try gvm.allocObjectSmall(sid, offsets, props);
                continue;
            },
            .object => {
                const sid = pc[1].arg;
                const startLocal = pc[2].arg;
                const numProps = pc[3].arg;
                const dst = pc[4].arg;
                const offsets = pc[5..5+numProps];
                pc += 5 + numProps;

                const props = framePtr[startLocal .. startLocal + numProps];
                framePtr[dst] = try gvm.allocObject(sid, offsets, props);
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
                framePtr[pc[2].arg] = framePtr[pc[1].arg];
                pc += 3;
                continue;
            },
            .copyRetainRelease => {
                @setRuntimeSafety(debug);
                const src = pc[1].arg;
                const dst = pc[2].arg;
                pc += 3;
                vm.retain(framePtr[src]);
                release(vm, framePtr[dst]);
                framePtr[dst] = framePtr[src];
                continue;
            },
            .copyReleaseDst => {
                const dst = pc[2].arg;
                release(vm, framePtr[dst]);
                framePtr[dst] = framePtr[pc[1].arg];
                pc += 3;
                continue;
            },
            .retain => {
                vm.retain(framePtr[pc[1].arg]);
                pc += 2;
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
                vm.retain(framePtr[dst]);
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
                const left = pc[1].arg;
                const index = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const indexv = framePtr[index];
                const leftv = framePtr[left];
                framePtr[dst] = try @call(.{.modifier = .never_inline}, gvm.getReverseIndex, .{leftv, indexv});
                vm.retain(framePtr[dst]);
                continue;
            },
            .jumpBack => {
                pc -= @ptrCast(*const align(1) u16, &pc[1]).*;
                continue;
            },
            .jump => {
                pc += @ptrCast(*const align(1) u16, &pc[1]).*;
                continue;
            },
            .jumpCond => {
                const jump = @ptrCast(*const align(1) i16, pc + 1).*;
                const cond = framePtr[pc[3].arg];
                const condVal = if (cond.isBool()) b: {
                    break :b cond.asBool();
                } else b: {
                    break :b @call(.{ .modifier = .never_inline }, cond.toBool, .{});
                };
                if (condVal) {
                    @setRuntimeSafety(false);
                    pc += @intCast(usize, jump);
                } else {
                    pc += 4;
                }
                continue;
            },
            .call0 => {
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                pc += 3;

                const callee = framePtr[startLocal + numArgs + 4 - 1];
                const retInfo = buildReturnInfo(0, true);
                // const retInfo = buildReturnInfo(pcOffset(pc), framePtrOffset(framePtr), 0, true);
                // try @call(.{ .modifier = .never_inline }, gvm.call, .{&pc, callee, numArgs, retInfo});
                try @call(.{ .modifier = .always_inline }, call, .{&pc, &framePtr, callee, startLocal, numArgs, retInfo});
                continue;
            },
            .call1 => {
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                pc += 3;

                const callee = framePtr[startLocal + numArgs + 4 - 1];
                const retInfo = buildReturnInfo(1, true);
                // const retInfo = buildReturnInfo(pcOffset(pc), framePtrOffset(framePtr), 1, true);
                // try @call(.{ .modifier = .never_inline }, gvm.call, .{&pc, callee, numArgs, retInfo});
                try @call(.{ .modifier = .always_inline }, call, .{&pc, &framePtr, callee, startLocal, numArgs, retInfo});
                continue;
            },
            .callObjFuncIC => {
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const recv = framePtr[startLocal + numArgs + 4 - 1];
                const typeId: u32 = if (recv.isPointer())
                    recv.asHeapObject(*HeapObject).common.structId
                else recv.getPrimitiveTypeId();

                const cachedStruct = @ptrCast(*align (1) u16, pc + 12).*;
                if (typeId == cachedStruct) {
                    const numLocals = pc[5].arg;
                    if (@ptrToInt(framePtr + startLocal + numLocals) >= @ptrToInt(vm.stackEndPtr)) {
                        return error.StackOverflow;
                    }
                    const retFramePtr = Value{ .retFramePtr = framePtr };
                    framePtr += startLocal;
                    @ptrCast([*]u8, framePtr + 1)[0] = pc[3].arg;
                    @ptrCast([*]u8, framePtr + 1)[1] = 0;
                    framePtr[2] = Value{ .retPcPtr = pc + 14 };
                    framePtr[3] = retFramePtr;
                    pc = @intToPtr([*]cy.OpData, @ptrCast(*align(1) u48, pc + 6).*);
                    continue;
                }

                // Deoptimize.
                pc[0] = cy.OpData{ .code = .callObjSym };
                continue;
            },
            .callObjNativeFuncIC => {
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const recv = framePtr[startLocal + numArgs + 4 - 1];
                var obj: *HeapObject = undefined;
                var typeId: u32 = undefined;
                if (recv.isPointer()) {
                    obj = recv.asHeapObject(*HeapObject);
                    typeId = obj.common.structId;
                } else {
                    obj = @intToPtr(*HeapObject, recv.val);
                    typeId = recv.getPrimitiveTypeId();
                }

                const cachedStruct = @ptrCast(*align (1) u16, pc + 12).*;
                if (typeId == cachedStruct) {
                    const newFramePtr = framePtr + startLocal;
                    vm.framePtr = newFramePtr;
                    const func = @intToPtr(NativeObjFuncPtr, @ptrCast(*align (1) u48, pc + 6).*);
                    const res = func(@ptrCast(*UserVM, vm), obj, @ptrCast([*]const Value, newFramePtr + 4), numArgs);
                    const numRet = pc[3].arg;
                    pc += 14;
                    if (numRet == 1) {
                        newFramePtr[0] = res;
                    } else {
                        switch (numRet) {
                            0 => {
                                // Nop.
                            },
                            1 => stdx.panic("not possible"),
                            else => {
                                stdx.panic("unsupported numret");
                            },
                        }
                    }
                    continue;
                }

                // Deoptimize.
                pc[0] = cy.OpData{ .code = .callObjSym };
                continue;
            },
            .callFuncIC => {
                const startLocal = pc[1].arg;
                const numLocals = pc[4].arg;
                if (@ptrToInt(framePtr + startLocal + numLocals) >= @ptrToInt(vm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr };
                framePtr += startLocal;
                @ptrCast([*]u8, framePtr + 1)[0] = pc[3].arg;
                @ptrCast([*]u8, framePtr + 1)[1] = 0;
                framePtr[2] = Value{ .retPcPtr = pc + 11 };
                framePtr[3] = retFramePtr;

                pc = @intToPtr([*]cy.OpData, @ptrCast(*align(1) u48, pc + 5).*);
                continue;
            },
            .callNativeFuncIC => {
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;

                const newFramePtr = framePtr + startLocal;
                vm.framePtr = newFramePtr;
                const func = @intToPtr(NativeFuncPtr, @ptrCast(*align (1) u48, pc + 5).*);
                const res = func(@ptrCast(*UserVM, vm), @ptrCast([*]const Value, newFramePtr + 4), numArgs);
                const numRet = pc[3].arg;
                if (numRet == 1) {
                    newFramePtr[0] = res;
                } else {
                    switch (numRet) {
                        0 => {
                            // Nop.
                        },
                        1 => stdx.panic("not possible"),
                        else => stdx.panic("unsupported"),
                    }
                }
                pc += 11;
                continue;
            },
            .ret1 => {
                if (@call(.{ .modifier = .always_inline }, popStackFrameLocal1, .{vm, &pc, &framePtr})) {
                    continue;
                } else {
                    return;
                }
            },
            .ret0 => {
                if (@call(.{ .modifier = .always_inline }, popStackFrameLocal0, .{&pc, &framePtr})) {
                    continue;
                } else {
                    return;
                }
            },
            .setFieldReleaseIC => {
                const recv = framePtr[pc[1].arg];
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    if (obj.common.structId == @ptrCast(*align (1) u16, pc + 4).*) {
                        const lastValue = obj.object.getValuePtr(pc[6].arg);
                        release(vm, lastValue.*);
                        lastValue.* = framePtr[pc[2].arg];
                        pc += 7;
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.OpData{ .code = .setFieldRelease };
                // framePtr[dst] = try gvm.getField(recv, pc[3].arg);
                try @call(.{ .modifier = .never_inline }, gvm.setFieldRelease, .{ recv, pc[3].arg, framePtr[pc[2].arg] });
                pc += 7;
                continue;
            },
            .fieldRetainIC => {
                const recv = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    if (obj.common.structId == @ptrCast(*align (1) u16, pc + 4).*) {
                        framePtr[dst] = obj.object.getValue(pc[6].arg);
                        vm.retain(framePtr[dst]);
                        pc += 7;
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.OpData{ .code = .fieldRetain };
                // framePtr[dst] = try gvm.getField(recv, pc[3].arg);
                framePtr[dst] = try @call(.{ .modifier = .never_inline }, gvm.getField, .{ recv, pc[3].arg });
                vm.retain(framePtr[dst]);
                pc += 7;
                continue;
            },
            .forRangeInit => {
                const start = framePtr[pc[1].arg].toF64();
                const end = framePtr[pc[2].arg].toF64();
                framePtr[pc[2].arg] = Value.initF64(end);
                var step = framePtr[pc[3].arg].toF64();
                if (step < 0) {
                    step = -step;
                }
                framePtr[pc[3].arg] = Value.initF64(step);
                if (start == end) {
                    pc += @ptrCast(*const align(1) u16, pc + 6).* + 7;
                } else {
                    framePtr[pc[4].arg] = Value.initF64(start);
                    framePtr[pc[5].arg] = Value.initF64(start);
                    const offset = @ptrCast(*const align(1) u16, pc + 6).*;
                    pc[offset] = if (start < end)
                        cy.OpData{ .code = .forRange }
                    else
                        cy.OpData{ .code = .forRangeReverse };
                    pc += 8;
                }
            },
            .forRange => {
                const counter = framePtr[pc[1].arg].asF64() + framePtr[pc[2].arg].asF64();
                if (counter < framePtr[pc[3].arg].asF64()) {
                    framePtr[pc[1].arg] = Value.initF64(counter);
                    framePtr[pc[4].arg] = Value.initF64(counter);
                    pc -= @ptrCast(*const align(1) u16, pc + 5).*;
                } else {
                    pc += 7;
                }
            },
            .forRangeReverse => {
                const counter = framePtr[pc[1].arg].asF64() - framePtr[pc[2].arg].asF64();
                if (counter > framePtr[pc[3].arg].asF64()) {
                    framePtr[pc[1].arg] = Value.initF64(counter);
                    framePtr[pc[4].arg] = Value.initF64(counter);
                    pc -= @ptrCast(*const align(1) u16, pc + 5).*;
                } else {
                    pc += 7;
                }
            },
            .jumpNotNone => {
                const offset = @ptrCast(*const align(1) i16, &pc[1]).*;
                if (!framePtr[pc[3].arg].isNone()) {
                    @setRuntimeSafety(false);
                    pc += @intCast(usize, offset);
                } else {
                    pc += 4;
                }
                continue;
            },
            .setField => {
                const fieldId = pc[1].arg;
                const left = pc[2].arg;
                const right = pc[3].arg;
                pc += 4;

                const recv = framePtr[left];
                const val = framePtr[right];
                try gvm.setField(recv, fieldId, val);
                // try @call(.{ .modifier = .never_inline }, gvm.setField, .{recv, fieldId, val});
                continue;
            },
            .fieldRelease => {
                const fieldId = pc[1].arg;
                const left = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const recv = framePtr[left];
                framePtr[dst] = try @call(.{ .modifier = .never_inline }, gvm.getField, .{recv, fieldId});
                release(vm, recv);
                continue;
            },
            .field => {
                const left = pc[1].arg;
                const dst = pc[2].arg;
                const symId = pc[3].arg;
                const recv = framePtr[left];
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    // const offset = @call(.{ .modifier = .never_inline }, gvm.getFieldOffset, .{obj, symId });
                    const offset = gvm.getFieldOffset(obj, symId);
                    if (offset != NullByteId) {
                        framePtr[dst] = obj.object.getValue(offset);
                        // Inline cache.
                        pc[0] = cy.OpData{ .code = .fieldIC };
                        @ptrCast(*align (1) u16, pc + 4).* = @intCast(u16, obj.common.structId);
                        pc[6] = cy.OpData { .arg = offset };
                    } else {
                        framePtr[dst] = @call(.{ .modifier = .never_inline }, gvm.getFieldFallback, .{obj, gvm.fieldSyms.buf[symId].name});
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                pc += 7;
                continue;
            },
            .fieldRetain => {
                const recv = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                const symId = pc[3].arg;
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    // const offset = @call(.{ .modifier = .never_inline }, gvm.getFieldOffset, .{obj, symId });
                    const offset = gvm.getFieldOffset(obj, symId);
                    if (offset != NullByteId) {
                        framePtr[dst] = obj.object.getValue(offset);
                        // Inline cache.
                        pc[0] = cy.OpData{ .code = .fieldRetainIC };
                        @ptrCast(*align (1) u16, pc + 4).* = @intCast(u16, obj.common.structId);
                        pc[6] = cy.OpData { .arg = offset };
                    } else {
                        framePtr[dst] = @call(.{ .modifier = .never_inline }, gvm.getFieldFallback, .{obj, gvm.fieldSyms.buf[symId].name});
                    }
                    vm.retain(framePtr[dst]);
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                pc += 7;
                continue;
            },
            .setFieldRelease => {
                const recv = framePtr[pc[1].arg];
                const val = framePtr[pc[2].arg];
                const symId = pc[3].arg;
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    // const offset = @call(.{ .modifier = .never_inline }, gvm.getFieldOffset, .{obj, symId });
                    const offset = gvm.getFieldOffset(obj, symId);
                    if (offset != NullByteId) {
                        const lastValue = obj.object.getValuePtr(offset);
                        release(vm, lastValue.*);
                        lastValue.* = val;

                        // Inline cache.
                        pc[0] = cy.OpData{ .code = .setFieldReleaseIC };
                        @ptrCast(*align (1) u16, pc + 4).* = @intCast(u16, obj.common.structId);
                        pc[6] = cy.OpData { .arg = offset };
                        pc += 7;
                        continue;
                    } else {
                        return vm.getFieldMissingSymbolError();
                    }
                } else {
                    return vm.setFieldNotObjectError();
                }
                pc += 7;
                continue;
            },
            .lambda => {
                const funcPc = pcOffset(pc) - pc[1].arg;
                const numParams = pc[2].arg;
                const numLocals = pc[3].arg;
                const dst = pc[4].arg;
                pc += 5;
                framePtr[dst] = try gvm.allocLambda(funcPc, numParams, numLocals);
                continue;
            },
            .closure => {
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
            .varSym => {
                const symId = pc[1].arg;
                const sym = vm.varSyms.buf[symId];
                switch (sym.symT) {
                    .value => {
                        framePtr[pc[2].arg] = sym.inner.value;
                        pc += 3;
                        continue;
                    },
                    .none => return vm.panic("Missing var symbol."),
                }
            },
            .coreturn => {
                pc += 1;
                if (vm.curFiber != &vm.mainFiber) {
                    const res = popFiber(vm, NullId, framePtr, framePtr[1]);
                    pc = res.pc;
                    framePtr = res.framePtr;
                }
                continue;
            },
            .coresume => {
                const fiber = framePtr[pc[1].arg];
                if (fiber.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, fiber.asPointer().?);
                    if (obj.common.structId == FiberS) {
                        if (&obj.fiber != vm.curFiber) {
                            // Only resume fiber if it's not done.
                            if (obj.fiber.pc != NullId) {
                                const res = pushFiber(vm, pcOffset(pc + 3), framePtr, &obj.fiber, pc[2].arg);
                                pc = res.pc;
                                framePtr = res.framePtr;
                                continue;
                            } else {
                                releaseObject(vm, obj);
                            }
                        }
                    }
                }
                pc += 3;
                continue;
            },
            .coyield => {
                if (vm.curFiber != &vm.mainFiber) {
                    // Only yield on user fiber.
                    const res = popFiber(vm, pcOffset(pc), framePtr, Value.None);
                    pc = res.pc;
                    framePtr = res.framePtr;
                } else {
                    pc += 3;
                }
                continue;
            },
            .coinit => {
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
            .mul => {
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.{ .modifier = .never_inline }, evalMultiply, .{srcLeft, srcRight});
                continue;
            },
            .div => {
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.{ .modifier = .never_inline }, evalDivide, .{srcLeft, srcRight});
                continue;
            },
            .mod => {
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.{ .modifier = .never_inline }, evalMod, .{srcLeft, srcRight});
                continue;
            },
            .pow => {
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.{ .modifier = .never_inline }, evalPower, .{srcLeft, srcRight});
                continue;
            },
            .box => {
                const value = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                pc += 3;
                vm.retain(value);
                framePtr[dst] = try allocBox(value);
                continue;
            },
            .setBoxValue => {
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
                @call(.{ .modifier = .never_inline }, release, .{vm, obj.box.val});
                obj.box.val = rval;
                continue;
            },
            .boxValue => {
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
                // vm.retain(obj.box.val);
                // pc += 3;
                framePtr[pc[2].arg] = @call(.{ .modifier = .never_inline }, boxValueRetain, .{box});
                pc += 3;
                continue;
            },
            .tag => {
                const tagId = pc[1].arg;
                const val = pc[2].arg;
                framePtr[pc[3].arg] = Value.initTag(tagId, val);
                pc += 4;
                continue;
            },
            .tagLiteral => {
                const symId = pc[1].arg;
                framePtr[pc[2].arg] = Value.initTagLiteral(symId);
                pc += 3;
                continue;
            },
            .tryValue => {
                const val = framePtr[pc[1].arg];
                if (!val.isError()) {
                    framePtr[pc[2].arg] = val;
                    pc += 3;
                    continue;
                } else {
                    return vm.panicFmt("{}", &.{fmt.v(vm.tagLitSyms.buf[val.asErrorTagLit()].name)});
                }
            },
            .bitwiseAnd => {
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = @call(.{ .modifier = .never_inline }, evalBitwiseAnd, .{left, right});
                pc += 4;
                continue;
            },
            .bitwiseOr => {
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = @call(.{ .modifier = .never_inline }, evalBitwiseOr, .{left, right});
                pc += 4;
                continue;
            },
            .bitwiseXor => {
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = @call(.{ .modifier = .never_inline }, evalBitwiseXor, .{left, right});
                pc += 4;
                continue;
            },
            .bitwiseNot => {
                const val = framePtr[pc[1].arg];
                framePtr[pc[2].arg] = @call(.{ .modifier = .never_inline }, evalBitwiseNot, .{val});
                pc += 3;
                continue;
            },
            .bitwiseLeftShift => {
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = @call(.{ .modifier = .never_inline }, evalBitwiseLeftShift, .{left, right});
                pc += 4;
                continue;
            },
            .bitwiseRightShift => {
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = @call(.{ .modifier = .never_inline }, evalBitwiseRightShift, .{left, right});
                pc += 4;
                continue;
            },
            .funcSymClosure => {
                const symId = pc[1].arg;
                const numParams = pc[2].arg;
                const numCaptured = pc[3].arg;
                const captured = pc[4..4+numCaptured];
                pc += 4 + numCaptured;
                try @call(.{ .modifier = .never_inline }, funcSymClosure, .{ framePtr, symId, numParams, captured });
                continue;
            },
            .callObjSym => {
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const numRet = pc[3].arg;
                const symId = pc[4].arg;

                const recv = framePtr[startLocal + numArgs + 4 - 1];
                var obj: *HeapObject = undefined;
                var typeId: u32 = undefined;
                if (recv.isPointer()) {
                    obj = recv.asHeapObject(*HeapObject);
                    typeId = obj.common.structId;
                } else {
                    obj = @intToPtr(*HeapObject, recv.val);
                    typeId = recv.getPrimitiveTypeId();
                }

                if (gvm.getCallObjSym(typeId, symId)) |sym| {
                    const res = try @call(.{ .modifier = .never_inline }, gvm.callSymEntry, .{pc, framePtr, sym, obj, typeId, startLocal, numArgs, numRet });
                    pc = res.pc;
                    framePtr = res.framePtr;
                } else {
                    const res = try @call(.{ .modifier = .never_inline }, callObjSymFallback, .{pc, framePtr, obj, typeId, symId, startLocal, numArgs, numRet});
                    pc = res.pc;
                    framePtr = res.framePtr;
                }
                continue;
            },
            .callSym => {
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const numRet = pc[3].arg;
                const symId = pc[4].arg;
                const res = try @call(.{ .modifier = .never_inline }, gvm.callSym, .{pc, framePtr, symId, startLocal, numArgs, @intCast(u2, numRet)});
                pc = res.pc;
                framePtr = res.framePtr;
                continue;
            },
            .end => {
                vm.endLocal = pc[1].arg;
                pc += 2;
                vm.curFiber.pc = @intCast(u32, pcOffset(pc));
                return error.End;
            },
        }
    }
}

fn popStackFrameLocal0(pc: *[*]const cy.OpData, framePtr: *[*]Value) linksection(".eval") bool {
    const retFlag = framePtr.*[1].retInfo.retFlag;
    const reqNumArgs = framePtr.*[1].retInfo.numRetVals;
    if (reqNumArgs == 0) {
        pc.* = framePtr.*[2].retPcPtr;
        framePtr.* = framePtr.*[3].retFramePtr;
        // return retFlag == 0;
        return !retFlag;
    } else {
        switch (reqNumArgs) {
            0 => unreachable,
            1 => {
                framePtr.*[0] = Value.None;
            },
            // 2 => {
            //     framePtr.*[0] = Value.None;
            //     framePtr.*[1] = Value.None;
            // },
            // 3 => {
            //     framePtr.*[0] = Value.None;
            //     framePtr.*[1] = Value.None;
            //     framePtr.*[2] = Value.None;
            // },
            else => unreachable,
        }
        pc.* = framePtr.*[2].retPcPtr;
        framePtr.* = framePtr.*[3].retFramePtr;
        // return retFlag == 0;
        return !retFlag;
    }
}

fn popStackFrameLocal1(vm: *VM, pc: *[*]const cy.OpData, framePtr: *[*]Value) linksection(section) bool {
    const retFlag = framePtr.*[1].retInfo.retFlag;
    const reqNumArgs = framePtr.*[1].retInfo.numRetVals;
    if (reqNumArgs == 1) {
        pc.* = framePtr.*[2].retPcPtr;
        framePtr.* = framePtr.*[3].retFramePtr;
        // return retFlag == 0;
        return !retFlag;
    } else {
        switch (reqNumArgs) {
            0 => {
                release(vm, framePtr.*[0]);
            },
            1 => unreachable,
            // 2 => {
            //     framePtr.*[1] = Value.None;
            // },
            // 3 => {
            //     framePtr.*[1] = Value.None;
            //     framePtr.*[2] = Value.None;
            // },
            else => unreachable,
        }
        pc.* = framePtr.*[2].retPcPtr;
        framePtr.* = framePtr.*[3].retFramePtr;
        // return retFlag == 0;
        return !retFlag;
    }
}

fn dumpEvalOp(vm: *const VM, pc: [*]const cy.OpData) void {
    const offset = pcOffset(pc);
    switch (pc[0].code) {
        .callObjSym => {
            log.debug("{} op: {s} {any}", .{offset, @tagName(pc[0].code), std.mem.sliceAsBytes(pc[1..14])});
        },
        .callSym => {
            log.debug("{} op: {s} {any}", .{offset, @tagName(pc[0].code), std.mem.sliceAsBytes(pc[1..11])});
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
        .add => {
            const left = pc[1].arg;
            const right = pc[2].arg;
            const dst = pc[3].arg;
            log.debug("{} op: {s} {} {} -> %{}", .{offset, @tagName(pc[0].code), left, right, dst});
        },
        .constOp => {
            const idx = pc[1].arg;
            const dst = pc[2].arg;
            const val = Value{ .val = vm.consts[idx].val };
            log.debug("{} op: {s} [{s}] -> %{}", .{offset, @tagName(pc[0].code), vm.valueToTempString(val), dst});
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
            const len = cy.getInstLenAt(pc);
            log.debug("{} op: {s} {any}", .{offset, @tagName(pc[0].code), std.mem.sliceAsBytes(pc[1..len])});
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

    pub fn dump(self: *const StackTrace, vm: *const VM) !void {
        @setCold(true);
        var arrowBuf: std.ArrayListUnmanaged(u8) = .{};
        var w = arrowBuf.writer(vm.alloc);
        defer arrowBuf.deinit(vm.alloc);

        for (self.frames) |frame| {
            const lineEnd = std.mem.indexOfScalarPos(u8, vm.compiler.src, frame.lineStartPos, '\n') orelse vm.compiler.src.len;
            arrowBuf.clearRetainingCapacity();
            try w.writeByteNTimes(' ', frame.col);
            try w.writeByte('^');
            if (builtin.is_test) {
                log.debug(
                    \\{s}:{}:{} {s}:
                    \\{s}
                    \\{s}
                    \\
                , .{
                    frame.uri, frame.line + 1, frame.col + 1, frame.name,
                    vm.compiler.src[frame.lineStartPos..lineEnd], arrowBuf.items,
                });
            } else {
                try fmt.printStderr(
                    \\{}:{}:{} {}:
                    \\{}
                    \\{}
                    \\
                , &.{
                    fmt.v(frame.uri), fmt.v(frame.line+1), fmt.v(frame.col+1), fmt.v(frame.name),
                    fmt.v(vm.compiler.src[frame.lineStartPos..lineEnd]), fmt.v(arrowBuf.items),
                });
            }
        }
    }
};

pub const StackFrame = struct {
    /// Name identifier (eg. function name)
    name: []const u8,
    /// Source location.
    uri: []const u8,
    /// Starts at 0.
    line: u32,
    /// Starts at 0.
    col: u32,
    /// Where the line starts in the source file.
    lineStartPos: u32,
};

const ObjectSymKey = struct {
    structId: StructId,
    symId: SymbolId,
};

/// Stack layout for lambda: arg0, arg1, ..., callee
/// Stack layout for closure: arg0, arg1, ..., callee, capturedVar0, capturedVar1, ...
/// numArgs includes the callee.
pub fn call(pc: *[*]const cy.OpData, framePtr: *[*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !void {
    if (callee.isPointer()) {
        const obj = stdx.ptrAlignCast(*HeapObject, callee.asPointer().?);
        switch (obj.common.structId) {
            ClosureS => {
                if (numArgs - 1 != obj.closure.numParams) {
                    stdx.panic("params/args mismatch");
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.closure.numLocals) >= @ptrToInt(gvm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr.* };
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;
                framePtr.*[2] = Value{ .retPcPtr = pc.* };
                framePtr.*[3] = retFramePtr;
                pc.* = toPc(obj.closure.funcPc);

                // Copy over captured vars to new call stack locals.
                if (obj.closure.numCaptured <= 3) {
                    const src = @ptrCast([*]Value, &obj.closure.capturedVal0)[0..obj.closure.numCaptured];
                    std.mem.copy(Value, framePtr.*[numArgs + 4..numArgs + 4 + obj.closure.numCaptured], src);
                } else {
                    stdx.panic("unsupported closure > 3 captured args.");
                }
            },
            LambdaS => {
                if (numArgs - 1 != obj.lambda.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    stdx.fatal();
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.lambda.numLocals) >= @ptrToInt(gvm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr.* };
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;
                framePtr.*[2] = Value{ .retPcPtr = pc.* };
                framePtr.*[3] = retFramePtr;
                pc.* = toPc(obj.lambda.funcPc);
            },
            else => {},
        }
    } else {
        stdx.panic("not a function");
    }
}

pub fn callNoInline(pc: *[*]cy.OpData, framePtr: *[*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !void {
    if (callee.isPointer()) {
        const obj = stdx.ptrAlignCast(*HeapObject, callee.asPointer().?);
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

                const retFramePtr = Value{ .retFramePtr = framePtr.* };
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;
                framePtr.*[2] = Value{ .retPcPtr = pc.* + 14 };
                framePtr.*[3] = retFramePtr;
                pc.* = toPc(obj.lambda.funcPc);
            },
            NativeFunc1S => {
                gvm.pc = pc.*;
                const newFramePtr = framePtr.* + startLocal;
                gvm.framePtr = newFramePtr;
                const res = obj.nativeFunc1.func(@ptrCast(*UserVM, &gvm), newFramePtr + 4, numArgs);
                newFramePtr[0] = res;
                releaseObject(&gvm, obj);
                pc.* += 14;
            },
            else => {},
        }
    } else {
        stdx.panic("not a function");
    }
}

fn getObjectFunctionFallback(obj: *const HeapObject, typeId: u32, symId: SymbolId) !Value {
    @setCold(true);
    if (typeId == MapS) {
        const name = gvm.methodSymExtras.buf[symId];
        const heapMap = stdx.ptrAlignCast(*const MapInner, &obj.map.inner);
        if (heapMap.getByString(&gvm, name)) |val| {
            return val;
        }
    }
    return gvm.panic("Missing function symbol in value");
}

/// Use new pc local to avoid deoptimization.
fn callObjSymFallback(pc: [*]cy.OpData, framePtr: [*]Value, obj: *HeapObject, typeId: u32, symId: SymbolId, startLocal: u8, numArgs: u8, reqNumRetVals: u8) !PcFramePtr {
    @setCold(true);
    // const func = try @call(.{ .modifier = .never_inline }, getObjectFunctionFallback, .{obj, symId});
    const func = try getObjectFunctionFallback(obj, typeId, symId);

    gvm.retain(func);
    releaseObject(&gvm, obj);

    // Replace receiver with function.
    framePtr[startLocal + 4 + numArgs - 1] = func;
    // const retInfo = buildReturnInfo(pc, framePtrOffset(framePtr), reqNumRetVals, true);
    const retInfo = buildReturnInfo2(reqNumRetVals, true);
    var newPc = pc;
    var newFramePtr = framePtr;
    try @call(.{ .modifier = .always_inline }, callNoInline, .{&newPc, &newFramePtr, func, startLocal, numArgs, retInfo});
    return PcFramePtr{
        .pc = newPc,
        .framePtr = newFramePtr,
    };
}

fn callSymEntryNoInline(pc: [*]const cy.OpData, framePtr: [*]Value, sym: SymbolEntry, obj: *HeapObject, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") !PcFramePtr {
    switch (sym.entryT) {
        .func => {
            if (@ptrToInt(framePtr + startLocal + sym.inner.func.numLocals) >= @ptrToInt(gvm.stack.ptr) + 8 * gvm.stack.len) {
                return error.StackOverflow;
            }

            // const retInfo = buildReturnInfo(pc, framePtrOffset(framePtr), reqNumRetVals, true);
            const retInfo = buildReturnInfo(reqNumRetVals, true);
            const newFramePtr = framePtr + startLocal;
            newFramePtr[1] = retInfo;
            return PcFramePtr{
                .pc = toPc(sym.inner.func.pc),
                .framePtr = newFramePtr,
            };
        },
        .nativeFunc1 => {
            // gvm.pc += 3;
            const newFramePtr = framePtr + startLocal;
            gvm.pc = pc;
            gvm.framePtr = framePtr;
            const res = sym.inner.nativeFunc1(@ptrCast(*UserVM, &gvm), obj, newFramePtr+4, numArgs);
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
                .pc = gvm.pc,
                .framePtr = framePtr,
            };
        },
        .nativeFunc2 => {
            // gvm.pc += 3;
            const newFramePtr = gvm.framePtr + startLocal;
            gvm.pc = pc;
            const res = sym.inner.nativeFunc2(@ptrCast(*UserVM, &gvm), obj, @ptrCast([*]const Value, newFramePtr+4), numArgs);
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

fn popFiber(vm: *VM, curFiberEndPc: usize, curFramePtr: [*]Value, retValue: Value) PcFramePtr {
    vm.curFiber.stackPtr = vm.stack.ptr;
    vm.curFiber.stackLen = @intCast(u32, vm.stack.len);
    vm.curFiber.pc = @intCast(u32, curFiberEndPc);
    vm.curFiber.framePtr = @intCast(u48, @ptrToInt(curFramePtr));
    const dstLocal = vm.curFiber.parentDstLocal;

    // Release current fiber.
    const nextFiber = vm.curFiber.prevFiber.?;
    releaseObject(vm, @ptrCast(*HeapObject, vm.curFiber));

    // Set to next fiber.
    vm.curFiber = nextFiber;

    // Copy return value to parent local.
    if (dstLocal != NullByteId) {
        @intToPtr([*]Value, vm.curFiber.framePtr)[dstLocal] = retValue;
    } else {
        release(vm, retValue);
    }

    vm.stack = vm.curFiber.stackPtr[0..vm.curFiber.stackLen];
    vm.stackEndPtr = vm.stack.ptr + vm.curFiber.stackLen;
    log.debug("fiber set to {} {*}", .{vm.curFiber.pc, vm.framePtr});
    return PcFramePtr{
        .pc = toPc(vm.curFiber.pc),
        .framePtr = @intToPtr([*]Value, vm.curFiber.framePtr),
    };
}

/// Since this is called from a coresume expression, the fiber should already be retained.
fn pushFiber(vm: *VM, curFiberEndPc: usize, curFramePtr: [*]Value, fiber: *Fiber, parentDstLocal: u8) PcFramePtr {
    // Save current fiber.
    vm.curFiber.stackPtr = vm.stack.ptr;
    vm.curFiber.stackLen = @intCast(u32, vm.stack.len);
    vm.curFiber.pc = @intCast(u32, curFiberEndPc);
    vm.curFiber.framePtr = @intCast(u48, @ptrToInt(curFramePtr));

    // Push new fiber.
    fiber.prevFiber = vm.curFiber;
    fiber.parentDstLocal = parentDstLocal;
    vm.curFiber = fiber;
    vm.stack = fiber.stackPtr[0..fiber.stackLen];
    vm.stackEndPtr = vm.stack.ptr + fiber.stackLen;
    // Check if fiber was previously yielded.
    if (vm.ops[fiber.pc].code == .coyield) {
        log.debug("fiber set to {} {*}", .{fiber.pc + 3, vm.framePtr});
        return .{
            .pc = toPc(fiber.pc + 3),
            .framePtr = @intToPtr([*]Value, fiber.framePtr),
        };
    } else {
        log.debug("fiber set to {} {*}", .{fiber.pc, vm.framePtr});
        return .{
            .pc = toPc(fiber.pc),
            .framePtr = @intToPtr([*]Value, fiber.framePtr),
        };
    }
}

fn allocFiber(pc: usize, args: []const Value, initialStackSize: u32) linksection(".eval") !Value {
    // Args are copied over to the new stack.
    var stack = try gvm.alloc.alloc(Value, initialStackSize);
    // Assumes initial stack size generated by compiler is enough to hold captured args.
    // Assumes call start local is at 1.
    std.mem.copy(Value, stack[5..5+args.len], args);

    const obj = try gvm.allocPoolObject();
    obj.fiber = .{
        .structId = FiberS,
        .rc = 1,
        .stackPtr = stack.ptr,
        .stackLen = @intCast(u32, stack.len),
        .pc = @intCast(u32, pc),
        .framePtr = @intCast(u48, @ptrToInt(stack.ptr)),
        .prevFiber = undefined,
        .parentDstLocal = NullByteId,
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

fn runReleaseOps(vm: *VM, stack: []const Value, framePtr: usize, startPc: usize) void {
    var pc = startPc;
    while (vm.ops[pc].code == .release) {
        const local = vm.ops[pc+1].arg;
        // stack[framePtr + local].dump();
        release(vm, stack[framePtr + local]);
        pc += 2;
    }
}

/// Unwinds the stack and releases the locals.
/// This also releases the initial captured vars since it's on the stack.
fn releaseFiberStack(vm: *VM, fiber: *Fiber) void {
    log.debug("release fiber stack", .{});
    var stack = fiber.stackPtr[0..fiber.stackLen];
    var framePtr = (@intCast(usize, fiber.framePtr) - @ptrToInt(stack.ptr)) >> 3;
    var pc = fiber.pc;

    if (pc != NullId) {

        // Check if fiber is still in init state.
        switch (vm.ops[pc].code) {
            .callFuncIC,
            .callSym => {
                if (vm.ops[pc + 11].code == .coreturn) {
                    const numArgs = vm.ops[pc - 4].arg;
                    for (@intToPtr([*]Value, fiber.framePtr)[5..5 + numArgs]) |arg| {
                        release(vm, arg);
                    }
                }
            },
            else => {},
        }

        // Check if fiber was previously on a yield op.
        if (vm.ops[pc].code == .coyield) {
            const jump = @ptrCast(*const align(1) u16, &vm.ops[pc+1]).*;
            log.debug("release on frame {} {} {}", .{framePtr, pc, pc + jump});
            // The yield statement already contains the end locals pc.
            runReleaseOps(vm, stack, framePtr, pc + jump);
        }
        // Unwind stack and release all locals.
        while (framePtr > 0) {
            pc = pcOffset(stack[framePtr + 2].retPcPtr);
            framePtr = (@ptrToInt(stack[framePtr + 3].retFramePtr) - @ptrToInt(stack.ptr)) >> 3;
            const endLocalsPc = pcToEndLocalsPc(vm, pc);
            log.debug("release on frame {} {} {}", .{framePtr, pc, endLocalsPc});
            if (endLocalsPc != NullId) {
                runReleaseOps(vm, stack, framePtr, endLocalsPc);
            }
        }
    }
    // Finally free stack.
    vm.alloc.free(stack);
}

/// Given pc position, return the end locals pc in the same frame.
/// TODO: Memoize this function.
fn pcToEndLocalsPc(vm: *const VM, pc: usize) u32 {
    const idx = vm.indexOfDebugSym(pc) orelse {
        stdx.panic("Missing debug symbol.");
    };
    const sym = vm.debugTable[idx];
    if (sym.frameLoc != NullId) {
        const node = vm.compiler.nodes[sym.frameLoc];
        return node.head.func.genEndLocalsPc;
    } else return NullId;
}

pub inline fn buildReturnInfo2(numRetVals: u8, comptime cont: bool) linksection(section) Value {
    return Value{
        .retInfo = .{
            .numRetVals = numRetVals,
            // .retFlag = if (cont) 0 else 1,
            .retFlag = !cont,
        },
    };
}

pub inline fn buildReturnInfo(comptime numRetVals: u2, comptime cont: bool) linksection(section) Value {
    return Value{
        .retInfo = .{
            .numRetVals = numRetVals,
            // .retFlag = if (cont) 0 else 1,
            .retFlag = !cont,
        },
    };
}

pub inline fn pcOffset(pc: [*]const cy.OpData) u32 {
    // Divide by eight.
    return @intCast(u32, @ptrToInt(pc) - @ptrToInt(gvm.ops.ptr));
}

pub inline fn toPc(offset: usize) [*]cy.OpData {
    return @ptrCast([*]cy.OpData, &gvm.ops.ptr[offset]);
}

inline fn framePtrOffsetFrom(stackPtr: [*]Value, framePtr: [*]Value) usize {
    // Divide by eight.
    return (@ptrToInt(framePtr) - @ptrToInt(stackPtr)) >> 3;
}

pub inline fn framePtrOffset(framePtr: [*]Value) usize {
    // Divide by eight.
    return (@ptrToInt(framePtr) - @ptrToInt(gvm.stack.ptr)) >> 3;
}

pub inline fn toFramePtr(offset: usize) [*]Value {
    return @ptrCast([*]Value, &gvm.stack[offset]);
}

const PcFramePtr = struct {
    pc: [*]cy.OpData,
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
    const obj = try gvm.allocPoolObject();
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

fn printUserError(vm: *const VM, title: []const u8, msg: []const u8, srcUri: []const u8, pos: u32, isTokenError: bool) !void {
    if (pos != NullId) {
        var line: u32 = undefined;
        var col: u32 = undefined;
        var lineStart: u32 = undefined;
        if (isTokenError) {
            vm.computeLinePosFromSrc(pos, &line, &col, &lineStart);
        } else {
            vm.computeLinePos(pos, &line, &col, &lineStart);
        }
        const lineEnd = std.mem.indexOfScalarPos(u8, vm.parser.src.items, lineStart, '\n') orelse vm.parser.src.items.len;
        var arrowBuf: std.ArrayListUnmanaged(u8) = .{};
        defer arrowBuf.deinit(vm.alloc);
        var w = arrowBuf.writer(vm.alloc);
        try w.writeByteNTimes(' ', col);
        try w.writeByte('^');
        try fmt.printStderr(
            \\{}: {}
            \\
            \\{}:{}:{}:
            \\{}
            \\{}
            \\
        , &.{
            fmt.v(title), fmt.v(msg), fmt.v(srcUri),
            fmt.v(line+1), fmt.v(col+1), fmt.v(vm.parser.src.items[lineStart..lineEnd]),
            fmt.v(arrowBuf.items)
        });
    } else {
        try fmt.printStderr(
            \\{}: {}
            \\
            \\in {}
            \\
        , &.{
            fmt.v(title), fmt.v(msg), fmt.v(srcUri),
        });
    }
}

// Performs stackGrowTotalCapacityPrecise in addition to patching the frame pointers.
fn growStackAuto(vm: *VM) !void {
    @setCold(true);
    // Grow by 50% with minimum of 16.
    var growSize = vm.stack.len / 2;
    if (growSize < 16) {
        growSize = 16;
    }
    const newCap = vm.stack.len + growSize;
    if (vm.alloc.resize(vm.stack, newCap)) {
        vm.stack.len = newCap;
        vm.stackEndPtr = vm.stack.ptr + newCap;
    } else {
        const newStack = try vm.alloc.alloc(Value, newCap);

        // Copy to new stack.
        std.mem.copy(Value, newStack[0..vm.stack.len], vm.stack);

        // Patch frame ptrs. 
        var curFpOffset = framePtrOffsetFrom(vm.stack.ptr, vm.framePtr);
        while (curFpOffset != 0) {
            const prevFpOffset = framePtrOffsetFrom(vm.stack.ptr, newStack[curFpOffset + 3].retFramePtr);
            newStack[curFpOffset + 3].retFramePtr = newStack.ptr + prevFpOffset;
            curFpOffset = prevFpOffset;
        }

        // Free old stack.
        vm.alloc.free(vm.stack);

        // Update to new frame ptr.
        vm.framePtr = newStack.ptr + framePtrOffsetFrom(vm.stack.ptr, vm.framePtr);
        vm.stack = newStack;
        vm.stackEndPtr = vm.stack.ptr + newCap;
    }
}