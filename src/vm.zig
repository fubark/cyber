const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const bindings = @import("bindings.zig");
const Value = cy.Value;
const debug = builtin.mode == .Debug;

const log = stdx.log.scoped(.vm);

/// Reserved symbols known at comptime.
pub const ListS: StructId = 0;
pub const MapS: StructId = 1;
pub const ClosureS: StructId = 2;
pub const LambdaS: StructId = 3;
pub const StringS: StructId = 4;

var tempU8Buf: [256]u8 = undefined;

/// Accessing the immediate VM vars is faster when the virtual address is known at compile time.
/// This also puts it in the .data section which can be closer to the eval hot loop.
pub var gvm: VM = undefined;

pub fn getUserVM() UserVM {
    return UserVM{};
}

pub const VM = struct {
    alloc: std.mem.Allocator,
    parser: cy.Parser,
    compiler: cy.VMcompiler,

    /// [Eval context]

    /// Program counter. Index to the next instruction op in `ops`.
    pc: usize,
    /// Current stack frame ptr. Previous stack frame info is saved as a Value after all the reserved locals.
    framePtr: usize,

    ops: []const cy.OpData,
    consts: []const cy.Const,
    strBuf: []const u8,

    /// Value stack.
    stack: stdx.Stack(Value),

    /// Object heap pages.
    heapPages: std.ArrayListUnmanaged(*HeapPage),
    heapFreeHead: ?*HeapObject,

    /// Symbol table used to lookup object fields and methods.
    /// First, the SymbolId indexes into the table for a SymbolMap to lookup the final SymbolEntry by StructId.
    symbols: std.ArrayListUnmanaged(SymbolMap),
    symbolsInfo: cy.List([]const u8),

    /// Used to track which symbols already exist. Only considers the name right now.
    symSignatures: std.StringHashMapUnmanaged(SymbolId),

    /// Regular function symbol table.
    funcSyms: std.ArrayListUnmanaged(FuncSymbolEntry),
    funcSymSignatures: std.StringHashMapUnmanaged(SymbolId),

    /// Struct fields symbol table.
    fieldSyms: std.ArrayListUnmanaged(FieldSymbolMap),
    fieldSymSignatures: std.StringHashMapUnmanaged(SymbolId),

    /// Structs.
    structs: std.ArrayListUnmanaged(Struct),
    iteratorObjSym: SymbolId,
    pairIteratorObjSym: SymbolId,
    nextObjSym: SymbolId,

    globals: std.StringHashMapUnmanaged(SymbolId),

    u8Buf: std.ArrayListUnmanaged(u8),

    panicMsg: []const u8,

    trace: *TraceInfo,

    pub fn init(self: *VM, alloc: std.mem.Allocator) !void {
        self.* = .{
            .alloc = alloc,
            .parser = cy.Parser.init(alloc),
            .compiler = undefined,
            .ops = undefined,
            .consts = undefined,
            .strBuf = undefined,
            .stack = .{},
            .heapPages = .{},
            .heapFreeHead = null,
            .pc = 0,
            .framePtr = 0,
            .symbolsInfo = .{},
            .symbols = .{},
            .symSignatures = .{},
            .funcSyms = .{},
            .funcSymSignatures = .{},
            .fieldSyms = .{},
            .fieldSymSignatures = .{},
            .structs = .{},
            .iteratorObjSym = undefined,
            .pairIteratorObjSym = undefined,
            .nextObjSym = undefined,
            .trace = undefined,
            .panicMsg = "",
            .globals = .{},
            .u8Buf = .{},
        };
        self.compiler.init(self);
        try self.ensureStackTotalCapacity(100);

        // Initialize heap.
        self.heapFreeHead = try self.growHeapPages(1);

        // Core bindings.
        try bindings.bindCore(self);
    }

    pub fn deinit(self: *VM) void {
        self.parser.deinit();
        self.compiler.deinit();
        self.stack.deinit(self.alloc);

        // for (self.symbols.items) |*map| {
            // if (map.mapT == .manyStructs) {
            //     map.inner.manyStructs.deinit(self.alloc);
            // }
        // }
        self.symbols.deinit(self.alloc);
        self.symbolsInfo.deinit(self.alloc);
        self.symSignatures.deinit(self.alloc);

        self.funcSyms.deinit(self.alloc);
        self.funcSymSignatures.deinit(self.alloc);

        self.fieldSyms.deinit(self.alloc);
        self.fieldSymSignatures.deinit(self.alloc);

        for (self.heapPages.items) |page| {
            self.alloc.destroy(page);
        }
        self.heapPages.deinit(self.alloc);

        self.structs.deinit(self.alloc);
        self.alloc.free(self.panicMsg);

        self.globals.deinit(self.alloc);
        self.u8Buf.deinit(self.alloc);
    }

    /// Returns the first free HeapObject.
    fn growHeapPages(self: *VM, numPages: usize) !*HeapObject {
        var idx = self.heapPages.items.len;
        try self.heapPages.resize(self.alloc, self.heapPages.items.len + numPages);

        // Allocate first page.
        var page = try self.alloc.create(HeapPage);
        self.heapPages.items[idx] = page;
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
        var last = first;
        idx += 1;
        while (idx < self.heapPages.items.len) : (idx += 1) {
            page = try self.alloc.create(HeapPage);
            self.heapPages.items[idx] = page;

            page.objects[0].common = .{
                .structId = 0,
            };
            const ptr = &page.objects[1];
            ptr.freeSpan = .{
                .structId = NullId,
                .len = page.objects.len - 1,
                .start = ptr,
                .next = null,
            };
            std.mem.set(HeapObject, page.objects[2..], .{
                .common = .{
                    .structId = NullId,
                }
            });
            page.objects[page.objects.len-1].freeSpan.start = ptr;
            last.freeSpan.next = ptr;
            last = ptr;
        }
        return first;
    }

    pub fn eval(self: *VM, src: []const u8, comptime trace: bool) !Value {
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

        if (trace) {
            try res.buf.dump();
            const numOps = @enumToInt(cy.OpCode.end) + 1;
            var opCounts: [numOps]cy.OpCount = undefined;
            self.trace.opCounts = &opCounts;
            var i: u32 = 0;
            while (i < numOps) : (i += 1) {
                self.trace.opCounts[i] = .{
                    .code = i,
                    .count = 0,
                };
            }
            self.trace.totalOpCounts = 0;
            self.trace.numReleases = 0;
            self.trace.numRetains = 0;
            self.trace.numRetainCycles = 0;
            self.trace.numRetainCycleRoots = 0;
        }
        tt = stdx.debug.trace();
        defer {
            tt.endPrint("eval");
            if (trace) {
                self.dumpInfo();
                const S = struct {
                    fn opCountLess(_: void, a: cy.OpCount, b: cy.OpCount) bool {
                        return a.count > b.count;
                    }
                };
                log.info("total ops evaled: {}", .{self.trace.totalOpCounts});
                std.sort.sort(cy.OpCount, self.trace.opCounts, {}, S.opCountLess);
                var i: u32 = 0;
                const numOps = @enumToInt(cy.OpCode.end) + 1;
                while (i < numOps) : (i += 1) {
                    if (self.trace.opCounts[i].count > 0) {
                        const op = std.meta.intToEnum(cy.OpCode, self.trace.opCounts[i].code) catch continue;
                        log.info("\t{s} {}", .{@tagName(op), self.trace.opCounts[i].count});
                    }
                }
            }
        }

        return self.evalByteCode(res.buf, trace) catch |err| {
            if (err == error.Panic) {
                log.debug("panic: {s}", .{self.panicMsg});
            }
            return err;
        };
    }

    pub fn dumpInfo(self: *VM) void {
        log.info("stack cap: {}", .{self.stack.buf.len});
        log.info("stack top: {}", .{self.stack.top});
        log.info("heap pages: {}", .{self.heapPages.items.len});

        // Dump object symbols.
        {
            log.info("obj syms:", .{});
            var iter = self.funcSymSignatures.iterator();
            while (iter.next()) |it| {
                log.info("\t{s}: {}", .{it.key_ptr.*, it.value_ptr.*});
            }
        }

        // Dump object fields.
        {
            log.info("obj fields:", .{});
            var iter = self.fieldSymSignatures.iterator();
            while (iter.next()) |it| {
                log.info("\t{s}: {}", .{it.key_ptr.*, it.value_ptr.*});
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

    /// Returns whether to continue execution loop.
    pub fn popStackFrame(self: *VM, comptime numRetVals: u2) linksection(".eval") bool {
        @setRuntimeSafety(debug);

        // If there are fewer return values than required from the function call, 
        // fill the missing slots with the none value.
        switch (numRetVals) {
            0 => {
                @setRuntimeSafety(debug);
                const retInfo = self.stack.buf[self.stack.top-1];
                const reqNumArgs = retInfo.retInfo.numRetVals;
                if (reqNumArgs == 0) {
                    self.stack.top = self.framePtr;
                    // Restore pc.
                    self.framePtr = retInfo.retInfo.framePtr;
                    self.pc = retInfo.retInfo.pc;
                    return retInfo.retInfo.retFlag == 0;
                } else {
                    switch (reqNumArgs) {
                        0 => unreachable,
                        1 => {
                            @setRuntimeSafety(debug);
                            self.stack.buf[self.framePtr] = Value.initNone();
                            self.stack.top = self.framePtr + 1;
                        },
                        2 => {
                            @setRuntimeSafety(debug);
                            // Only start checking for space after 2 since function calls should have at least one slot after framePtr.
                            self.ensureStackTotalCapacity(self.stack.top + 1) catch stdx.fatal();
                            self.stack.buf[self.framePtr] = Value.initNone();
                            self.stack.buf[self.framePtr+1] = Value.initNone();
                            self.stack.top = self.framePtr + 2;
                        },
                        3 => {
                            @setRuntimeSafety(debug);
                            self.ensureStackTotalCapacity(self.stack.top + 2) catch stdx.fatal();
                            self.stack.buf[self.framePtr] = Value.initNone();
                            self.stack.buf[self.framePtr+1] = Value.initNone();
                            self.stack.buf[self.framePtr+2] = Value.initNone();
                            self.stack.top = self.framePtr + 3;
                        },
                    }
                    // Restore pc.
                    self.framePtr = retInfo.retInfo.framePtr;
                    self.pc = retInfo.retInfo.pc;
                    return retInfo.retInfo.retFlag == 0;
                }
            },
            1 => {
                @setRuntimeSafety(debug);
                const retInfo = self.stack.buf[self.stack.top-2];
                const reqNumArgs = retInfo.retInfo.numRetVals;
                if (reqNumArgs == 1) {
                    // Copy return value to framePtr.
                    self.stack.buf[self.framePtr] = self.stack.buf[self.stack.top-1];
                    self.stack.top = self.framePtr + 1;

                    // Restore pc.
                    self.framePtr = retInfo.retInfo.framePtr;
                    self.pc = retInfo.retInfo.pc;
                    return retInfo.retInfo.retFlag == 0;
                } else {
                    switch (reqNumArgs) {
                        0 => {
                            @setRuntimeSafety(debug);
                            self.stack.top = self.framePtr;
                        },
                        1 => unreachable,
                        2 => {
                            @setRuntimeSafety(debug);
                            self.stack.buf[self.framePtr+1] = Value.initNone();
                            self.stack.top = self.framePtr + 2;
                        },
                        3 => {
                            @setRuntimeSafety(debug);
                            // Only start checking for space at 3 since function calls should have at least two slot after framePtr.
                            // self.ensureStackTotalCapacity(self.stack.top + 1) catch stdx.fatal();
                            self.stack.buf[self.framePtr+1] = Value.initNone();
                            self.stack.buf[self.framePtr+2] = Value.initNone();
                            self.stack.top = self.framePtr + 3;
                        },
                    }
                    // Restore pc.
                    self.framePtr = retInfo.retInfo.framePtr;
                    self.pc = retInfo.retInfo.pc;
                    return retInfo.retInfo.retFlag == 0;
                }
            },
            else => @compileError("Unsupported num return values."),
        }
    }

    pub fn evalByteCode(self: *VM, buf: cy.ByteCodeBuffer, comptime trace: bool) !Value {
        if (buf.ops.items.len == 0) {
            return error.NoEndOp;
        }

        self.stack.clearRetainingCapacity();
        self.ops = buf.ops.items;
        self.consts = buf.consts.items;
        self.strBuf = buf.strBuf.items;
        self.pc = 0;
        self.framePtr = 0;

        try self.ensureStackTotalCapacity(buf.mainLocalSize);
        self.stack.top = buf.mainLocalSize;

        try @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{trace});
        if (trace) {
            log.info("main local size: {}", .{buf.mainLocalSize});
        }

        if (self.stack.top == buf.mainLocalSize) {
            self.stack.top = 0;
            return Value.initNone();
        } else if (self.stack.top == buf.mainLocalSize + 1) {
            defer self.stack.top = 0;
            return self.popRegister();
        } else {
            log.debug("unexpected stack top: {}", .{self.stack.top});
            return error.BadTop;
        }
    }

    fn sliceList(self: *VM, listV: Value, startV: Value, endV: Value) !Value {
        if (listV.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, listV.asPointer().?);
            if (obj.retainedCommon.structId == ListS) {
                const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &obj.retainedList.list);
                var start = @floatToInt(i32, startV.toF64());
                if (start < 0) {
                    start = @intCast(i32, list.items.len) + start + 1;
                }
                var end = @floatToInt(i32, endV.toF64());
                if (end < 0) {
                    end = @intCast(i32, list.items.len) + end + 1;
                }
                if (start < 0 or start > list.items.len) {
                    return self.panic("Index out of bounds");
                }
                if (end < start or end > list.items.len) {
                    return self.panic("Index out of bounds");
                }
                return self.allocList(list.items[@intCast(u32, start)..@intCast(u32, end)]);
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
        return Value.initPtr(obj);
    }

    fn allocMap(self: *VM, keys: []const cy.Const, vals: []const Value) !Value {
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

        const inner = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
        for (keys) |key, i| {
            const val = vals[i];

            const keyVal = Value{ .val = key.val };
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
            self.heapFreeHead = try self.growHeapPages(std.math.max(1, (self.heapPages.items.len * 15) / 10));
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
        return Value.initPtr(obj);
    }

    fn allocClosure(self: *VM, funcPc: usize, numParams: u8, numLocals: u8, capturedVals: []const Value) !Value {
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
        switch (capturedVals.len) {
            0 => unreachable,
            1 => {
                obj.closure.capturedVal0 = capturedVals[0];
            },
            2 => {
                obj.closure.capturedVal0 = capturedVals[0];
                obj.closure.capturedVal1 = capturedVals[1];
            },
            3 => {
                obj.closure.capturedVal0 = capturedVals[0];
                obj.closure.capturedVal1 = capturedVals[1];
                obj.closure.extra.capturedVal2 = capturedVals[2];
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
        return Value.initPtr(obj);
    }

    pub fn allocString(self: *VM, str: []const u8) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        const dupe = try self.alloc.dupe(u8, str);
        obj.string = .{
            .structId = StringS,
            .rc = 1,
            .ptr = dupe.ptr,
            .len = dupe.len,
        };
        return Value.initPtr(obj);
    }

    pub fn allocStringTemplate(self: *VM, strs: []const Value, vals: []const Value) !Value {
        @setRuntimeSafety(debug);

        const firstStr = self.valueAsString(strs[0]);
        try self.u8Buf.resize(self.alloc, firstStr.len);
        std.mem.copy(u8, self.u8Buf.items, firstStr);

        var writer = self.u8Buf.writer(self.alloc);
        for (vals) |val, i| {
            self.writeValueToString(writer, val);
            try self.u8Buf.appendSlice(self.alloc, self.valueAsString(strs[i+1]));
        }

        const obj = try self.allocObject();
        const buf = try self.alloc.alloc(u8, self.u8Buf.items.len);
        std.mem.copy(u8, buf, self.u8Buf.items);
        obj.string = .{
            .structId = StringS,
            .rc = 1,
            .ptr = buf.ptr,
            .len = buf.len,
        };
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
        return Value.initPtr(obj);
    }

    pub fn allocOwnedList(self: *VM, elems: []Value) !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        obj.retainedList = .{
            .structId = ListS,
            .rc = 1,
            .list = .{
                .ptr = elems.ptr,
                .len = elems.len,
                .cap = elems.len,
            },
            .nextIterIdx = 0,
        };
        return Value.initPtr(obj);
    }

    fn allocList(self: *VM, elems: []const Value) linksection(".eval") !Value {
        @setRuntimeSafety(debug);
        const obj = try self.allocObject();
        obj.retainedList = .{
            .structId = ListS,
            .rc = 1,
            .list = .{
                .ptr = undefined,
                .len = 0,
                .cap = 0,
            },
            .nextIterIdx = 0,
        };
        const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &obj.retainedList.list);
        try list.appendSlice(self.alloc, elems);
        return Value.initPtr(obj);
    }

    inline fn getStackFrameValue(self: *const VM, offset: u8) linksection(".eval") Value {
        @setRuntimeSafety(debug);
        return self.stack.buf[self.framePtr + offset];
    }

    inline fn setStackFrameValue(self: *const VM, offset: u8, val: Value) linksection(".eval") void {
        @setRuntimeSafety(debug);
        self.stack.buf[self.framePtr + offset] = val;
    }

    pub inline fn popRegister(self: *VM) linksection(".eval") Value {
        @setRuntimeSafety(debug);
        self.stack.top -= 1;
        return self.stack.buf[self.stack.top];
    }

    inline fn ensureStackTotalCapacity(self: *VM, newCap: usize) linksection(".eval") !void {
        if (newCap > self.stack.buf.len) {
            try self.stack.growTotalCapacity(self.alloc, newCap);
        }
    }

    inline fn checkStackHasOneSpace(self: *const VM) linksection(".eval") !void {
        if (self.stack.top == self.stack.buf.len) {
            return error.StackOverflow;
        }
    }

    pub inline fn ensureUnusedStackSpace(self: *VM, unused: u32) linksection(".eval") !void {
        if (self.stack.top + unused > self.stack.buf.len) {
            try self.stack.growTotalCapacity(self.alloc, self.stack.top + unused);
        }
    }

    inline fn pushValueNoCheck(self: *VM, val: Value) linksection(".eval") void {
        @setRuntimeSafety(debug);
        self.stack.buf[self.stack.top] = val;
        self.stack.top += 1;
    }

    pub fn addStruct(self: *VM, name: []const u8) !StructId {
        _ = name;
        const s = Struct{
            .name = "",
        };
        const id = @intCast(u32, self.structs.items.len);
        try self.structs.append(self.alloc, s);
        return id;
    }

    pub fn ensureGlobalFuncSym(self: *VM, ident: []const u8, funcSymName: []const u8) !void {
        const id = try self.ensureFuncSym(funcSymName);
        try self.globals.put(self.alloc, ident, id);
    }

    pub fn getGlobalFuncSym(self: *VM, ident: []const u8) ?SymbolId {
        return self.globals.get(ident);
    }
    
    pub fn ensureFuncSym(self: *VM, name: []const u8) !SymbolId {
        const res = try self.funcSymSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            const id = @intCast(u32, self.funcSyms.items.len);
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
            const id = @intCast(u32, self.fieldSyms.items.len);
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

    pub fn ensureStructSym(self: *VM, name: []const u8) !SymbolId {
        const res = try self.symSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            const id = @intCast(u32, self.symbols.items.len);
            try self.symbols.append(self.alloc, .{
                .mapT = .empty,
                .inner = undefined,
            });
            try self.symbolsInfo.append(self.alloc, name);
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub inline fn setFuncSym(self: *VM, symId: SymbolId, sym: FuncSymbolEntry) !void {
        self.funcSyms.items[symId] = sym;
    }

    pub fn addStructSym(self: *VM, id: StructId, symId: SymbolId, sym: SymbolEntry) !void {
        switch (self.symbols.items[symId].mapT) {
            .empty => {
                self.symbols.items[symId].mapT = .oneStruct;
                self.symbols.items[symId].inner = .{
                    .oneStruct = .{
                        .id = id,
                        .sym = sym,
                    },
                };
            },
            else => stdx.panicFmt("unsupported {}", .{self.symbols.items[symId].mapT}),
        }
    }

    fn setIndex(self: *VM, left: Value, index: Value, right: Value) !void {
        @setRuntimeSafety(debug);
        if (left.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                ListS => {
                    const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &obj.retainedList.list);
                    const idx = @floatToInt(u32, index.toF64());
                    if (idx < list.items.len) {
                        list.items[idx] = right;
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

    fn getReverseIndex(self: *const VM, left: Value, index: Value) !Value {
        @setRuntimeSafety(debug);
        if (left.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                ListS => {
                    @setRuntimeSafety(debug);
                    const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &obj.retainedList.list);
                    const idx = list.items.len - @floatToInt(u32, index.toF64());
                    if (idx < list.items.len) {
                        return list.items[idx];
                    } else {
                        return error.OutOfBounds;
                    }
                },
                MapS => {
                    @setRuntimeSafety(debug);
                    const map = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
                    const key = Value.initF64(-index.toF64());
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
                    const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &obj.retainedList.list);
                    const idx = @floatToInt(u32, index.toF64());
                    if (idx < list.items.len) {
                        return list.items[idx];
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

    fn panic(self: *VM, msg: []const u8) error{Panic, OutOfMemory} {
        self.panicMsg = try self.alloc.dupe(u8, msg);
        return error.Panic;
    }

    /// Performs an iteration over the heap pages to check whether there are retain cycles.
    pub fn checkMemory(self: *VM, comptime trace: bool) !bool {
        var nodes: std.AutoHashMapUnmanaged(*HeapObject, RcNode) = .{};
        defer nodes.deinit(self.alloc);

        var cycleRoots: std.ArrayListUnmanaged(*HeapObject) = .{};
        defer cycleRoots.deinit(self.alloc);

        // No concept of root vars yet. Just report any existing retained objects.
        // First construct the graph.
        for (self.heapPages.items) |page| {
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
                        const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &obj.retainedList.list);
                        for (list.items) |it| {
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
                if (trace) {
                    self.trace.numRetainCycles = 1;
                    self.trace.numRetainCycleRoots = @intCast(u32, cycleRoots.items.len);
                }
                for (cycleRoots.items) |root| {
                    // Force release.
                    self.forceRelease(root, trace);
                }
                return false;
            }
        }
        return true;
    }

    pub inline fn retain(self: *VM, val: Value) void {
        _ = self;
        @setRuntimeSafety(debug);
        if (val.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, val.asPointer());
            obj.retainedCommon.rc += 1;
        }
    }

    pub fn forceRelease(self: *VM, obj: *HeapObject, comptime trace: bool) void {
        if (trace) {
            self.trace.numReleases += 1;
        }
        switch (obj.retainedCommon.structId) {
            ListS => {
                const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &obj.retainedList.list);
                list.deinit(self.alloc);
                self.freeObject(obj);
            },
            MapS => {
                const map = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
                map.deinit(self.alloc);
                self.freeObject(obj);
            },
            else => {
                return stdx.panic("unsupported struct type");
            },
        }
    }

    pub fn release(self: *VM, val: Value, comptime trace: bool) linksection(".eval") void {
        @setRuntimeSafety(debug);
        // log.info("release", .{});
        // val.dump();
        if (val.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, val.asPointer().?);
            obj.retainedCommon.rc -= 1;
            if (trace) {
                self.trace.numReleases += 1;
            }
            if (obj.retainedCommon.rc == 0) {
                switch (obj.retainedCommon.structId) {
                    ListS => {
                        const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &obj.retainedList.list);
                        for (list.items) |it| {
                            self.release(it, trace);
                        }
                        list.deinit(self.alloc);
                        self.freeObject(obj);
                    },
                    MapS => {
                        const map = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
                        var iter = map.iterator();
                        while (iter.next()) |entry| {
                            self.release(entry.key, trace);
                            self.release(entry.value, trace);
                        }
                        map.deinit(self.alloc);
                        self.freeObject(obj);
                    },
                    ClosureS => {
                        if (obj.closure.numCaptured <= 3) {
                            const src = @ptrCast([*]Value, &obj.closure.capturedVal0)[0..obj.closure.numCaptured];
                            for (src) |capturedVal| {
                                self.release(capturedVal, trace);
                            }
                            self.freeObject(obj);
                        } else {
                            stdx.panic("unsupported");
                        }
                    },
                    LambdaS => {
                        self.freeObject(obj);
                    },
                    StringS => {
                        self.alloc.free(obj.string.ptr[0..obj.string.len]);
                        self.freeObject(obj);
                    },
                    else => {
                        // return stdx.panicFmt("unsupported struct type {}", .{obj.common.structId});
                        unreachable;
                    },
                }
            }
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

    fn getField(self: *const VM, symId: SymbolId, recv: Value) linksection(".eval") Value {
        @setRuntimeSafety(debug);
        if (recv.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, recv.asPointer());
            const symMap = self.fieldSyms.items[symId];
            switch (symMap.mapT) {
                .oneStruct => {
                    if (obj.common.structId == symMap.inner.oneStruct.id) {
                        stdx.panic("TODO: get field");
                    } else {
                        return self.getFieldOther(obj, symMap.name);
                    }
                },
                .empty => {
                    return self.getFieldOther(obj, symMap.name);
                },
                // else => {
                //     // @setCold(true);
                //     // stdx.panicFmt("unsupported {}", .{symMap.mapT});
                //     unreachable;
                // },
            } 
        } else {
            // log.debug("Object missing symbol: {}", .{symId});
            // return error.MissingSymbol;
            unreachable;
        }
    }

    /// Stack layout: arg0, arg1, ..., callee
    /// numArgs includes the callee.
    pub fn call(self: *VM, callee: Value, numArgs: u8, retInfo: Value) !void {
        @setRuntimeSafety(debug);
        if (callee.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, callee.asPointer().?);
            switch (obj.common.structId) {
                ClosureS => {
                    if (numArgs - 1 != obj.closure.numParams) {
                        stdx.panic("params/args mismatch");
                    }

                    self.pc = obj.closure.funcPc;
                    self.framePtr = self.stack.top - numArgs;
                    // numLocals includes the function params as well as the return info value.
                    self.stack.top = self.framePtr + obj.closure.numLocals;

                    if (self.stack.top > self.stack.buf.len) {
                        try self.stack.growTotalCapacity(self.alloc, self.stack.top);
                    }
                    // Push return pc address and previous current framePtr onto the stack.
                    self.stack.buf[self.stack.top-1] = retInfo;

                    // Copy over captured vars to new call stack locals.
                    if (obj.closure.numCaptured <= 3) {
                        const src = @ptrCast([*]Value, &obj.closure.capturedVal0)[0..obj.closure.numCaptured];
                        std.mem.copy(Value, self.stack.buf[self.stack.top-1-obj.closure.numCaptured..self.stack.top-1], src);
                    } else {
                        stdx.panic("unsupported closure > 3 captured args.");
                    }
                },
                LambdaS => {
                    if (numArgs - 1 != obj.lambda.numParams) {
                        stdx.panicFmt("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    }
                    self.pc = obj.lambda.funcPc;
                    self.framePtr = self.stack.top - numArgs;
                    // numLocals includes the function params as well as the return info value.
                    self.stack.top = self.framePtr + obj.lambda.numLocals;

                    if (self.stack.top > self.stack.buf.len) {
                        try self.stack.growTotalCapacity(self.alloc, self.stack.top);
                    }
                    // Push return pc address and previous current framePtr onto the stack.
                    self.stack.buf[self.stack.top-1] = retInfo;
                },
                else => {},
            }
        } else {
            stdx.panic("not a function");
        }
    }

    /// Current stack top is already pointing past the last arg.
    fn callSym(self: *VM, symId: SymbolId, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") !void {
        @setRuntimeSafety(debug);
        const sym = self.funcSyms.items[symId];
        switch (sym.entryT) {
            .nativeFunc1 => {
                const args = self.stack.buf[self.stack.top - numArgs..self.stack.top];
                const res = sym.inner.nativeFunc1(self, args.ptr, @intCast(u8, args.len));
                if (reqNumRetVals == 1) {
                    self.stack.top = self.stack.top - numArgs + 1;
                    self.ensureStackTotalCapacity(self.stack.top) catch stdx.fatal();
                    self.stack.buf[self.stack.top-1] = res;
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            self.stack.top = self.stack.top - numArgs;
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
                const retInfo = self.buildReturnInfo(reqNumRetVals, true);
                self.pc = sym.inner.func.pc;
                self.framePtr = self.stack.top - numArgs;
                // numLocals includes the function params as well as the return info value.
                self.stack.top = self.framePtr + sym.inner.func.numLocals;

                if (self.stack.top > self.stack.buf.len) {
                    try self.stack.growTotalCapacity(self.alloc, self.stack.top);
                }
                // Push return pc address and previous current framePtr onto the stack.
                self.stack.buf[self.stack.top-1] = retInfo;
            },
            // .none => {
            //     @setCold(true);
            //     // Function doesn't exist.
            //     log.debug("Symbol {} doesn't exist.", .{symId});
            //     // TODO: script panic.
            //     return error.MissingSymbol;
            // },
            else => stdx.panic("unsupported callsym"),
        }
    }

    inline fn callSymEntry(self: *VM, entry: SymbolEntry, objPtr: *anyopaque, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") void {
        @setRuntimeSafety(debug);
        const argStart = self.stack.top - numArgs;
        switch (entry.entryT) {
            .nativeFunc1 => {
                const args = self.stack.buf[argStart .. self.stack.top - 1];
                const res = entry.inner.nativeFunc1(.{}, objPtr, args.ptr, @intCast(u8, args.len));
                if (reqNumRetVals == 1) {
                    self.stack.buf[argStart] = res;
                    self.stack.top = argStart + 1;
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            self.stack.top = argStart;
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
                const args = self.stack.buf[argStart .. self.stack.top - 1];
                const func = @ptrCast(std.meta.FnPtr(fn (*VM, *anyopaque, []const Value) cy.ValuePair), entry.inner.nativeFunc2);
                const res = func(self, objPtr, args);
                if (reqNumRetVals == 2) {
                    self.stack.buf[argStart] = res.left;
                    self.stack.buf[argStart+1] = res.right;
                    self.stack.top = argStart + 2;
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            self.stack.top = argStart;
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
            else => {
                // @setCold(true);
                // stdx.panicFmt("unsupported {}", .{entry.entryT});
                unreachable;
            },
        }
    }

    fn callObjSymOther(self: *VM, obj: *const HeapObject, name: []const u8, numArgs: u8, comptime reqNumRetVals: u2) void {
        @setCold(true);
        if (obj.common.structId == MapS) {
            const heapMap = stdx.ptrCastAlign(*const MapInner, &obj.map.inner);
            if (heapMap.getByString(self, name)) |val| {
                // Replace receiver with function.
                self.stack.buf[self.stack.top-1] = val;
                const retInfo = self.buildReturnInfo(reqNumRetVals, true);
                self.call(val, numArgs, retInfo) catch stdx.fatal();
                return;
            }
        }
        log.debug("Missing object symbol. {s}", .{name});
        stdx.fatal();
    }

    /// Stack layout: arg0, arg1, ..., receiver
    /// numArgs includes the receiver.
    fn callObjSym(self: *VM, symId: SymbolId, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") void {
        @setRuntimeSafety(debug);
        const recv = self.stack.buf[self.stack.top - 1];
        if (recv.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, recv.asPointer().?);
            const map = self.symbols.items[symId];
            switch (map.mapT) {
                .oneStruct => {
                    @setRuntimeSafety(debug);
                    if (obj.retainedCommon.structId == map.inner.oneStruct.id) {
                        self.callSymEntry(map.inner.oneStruct.sym, obj, numArgs, reqNumRetVals);
                    } else stdx.panic("Symbol does not exist for receiver.");
                },
                .empty => {
                    @setRuntimeSafety(debug);
                    self.callObjSymOther(obj, self.symbolsInfo.buf[symId], numArgs, reqNumRetVals);
                },
                // else => {
                //     unreachable;
                //     // stdx.panicFmt("unsupported {}", .{map.mapT});
                // },
            } 
        }
    }

    pub inline fn buildReturnInfo(self: *const VM, comptime numRetVals: u2, comptime cont: bool) linksection(".eval") Value {
        @setRuntimeSafety(debug);
        return Value{
            .retInfo = .{
                .pc = @intCast(u32, self.pc),
                .framePtr = @intCast(u29, self.framePtr),
                .numRetVals = numRetVals,
                .retFlag = if (cont) 0 else 1,
            },
        };
    }

    fn evalLoop(self: *VM, comptime trace: bool) linksection(".eval") error{StackOverflow, OutOfMemory, Panic, OutOfBounds, End}!void {
        @setRuntimeSafety(debug);
        while (true) {
            if (trace) {
                const op = self.ops[self.pc].code;
                self.trace.opCounts[@enumToInt(op)].count += 1;
                self.trace.totalOpCounts += 1;
            }
            log.debug("{} op: {s}", .{self.pc, @tagName(self.ops[self.pc].code)});
            switch (self.ops[self.pc].code) {
                .pushTrue => {
                    @setRuntimeSafety(debug);
                    try self.checkStackHasOneSpace();
                    self.pc += 1;
                    self.pushValueNoCheck(Value.initTrue());
                    continue;
                },
                .pushFalse => {
                    @setRuntimeSafety(debug);
                    try self.checkStackHasOneSpace();
                    self.pc += 1;
                    self.pushValueNoCheck(Value.initFalse());
                    continue;
                },
                .pushNone => {
                    @setRuntimeSafety(debug);
                    try self.checkStackHasOneSpace();
                    self.pc += 1;
                    self.pushValueNoCheck(Value.initNone());
                    continue;
                },
                .pushConst => {
                    @setRuntimeSafety(debug);
                    try self.checkStackHasOneSpace();
                    const idx = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    self.pushValueNoCheck(Value{ .val = self.consts[idx].val });
                    continue;
                },
                .pushStringTemplate => {
                    @setRuntimeSafety(debug);
                    const exprCount = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    const count = exprCount * 2 + 1;
                    const strs = self.stack.buf[self.stack.top-count..self.stack.top-exprCount];
                    const vals = self.stack.buf[self.stack.top-exprCount..self.stack.top];
                    const res = try @call(.{ .modifier = .never_inline }, self.allocStringTemplate, .{strs, vals});
                    self.stack.top = self.stack.top - count + 1;
                    self.stack.buf[self.stack.top-1] = res;
                    continue;
                },
                .pushNeg => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    const val = self.stack.buf[self.stack.top-1];
                    self.stack.buf[self.stack.top-1] = evalNeg(val);
                    continue;
                },
                .pushNot => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    const val = self.stack.buf[self.stack.top-1];
                    self.stack.buf[self.stack.top-1] = evalNot(val);
                    continue;
                },
                .pushNotCompare => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const right = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    self.stack.buf[self.stack.top-1] = evalNotCompare(left, right);
                    continue;
                },
                .pushCompare => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const right = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    self.stack.buf[self.stack.top-1] = evalCompare(left, right);
                    continue;
                },
                .pushLess => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const right = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    self.stack.buf[self.stack.top-1] = evalLess(left, right);
                    continue;
                },
                .pushGreater => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const right = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    self.stack.buf[self.stack.top-1] = evalGreater(left, right);
                    continue;
                },
                .pushLessEqual => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const right = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    self.stack.buf[self.stack.top-1] = evalLessOrEqual(left, right);
                    continue;
                },
                .pushGreaterEqual => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const right = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    self.stack.buf[self.stack.top-1] = evalGreaterOrEqual(left, right);
                    continue;
                },
                .pushOr => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const right = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    self.stack.buf[self.stack.top-1] = evalOr(left, right);
                    continue;
                },
                .pushAnd => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const right = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    self.stack.buf[self.stack.top-1] = evalAnd(left, right);
                    continue;
                },
                .pushAdd => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const left = self.stack.buf[self.stack.top-1];
                    const right = self.stack.buf[self.stack.top];
                    if (left.isNumber()) {
                        self.stack.buf[self.stack.top-1] = evalAddNumber(left, right);
                    } else {
                        self.stack.buf[self.stack.top-1] = evalAddOther(self, left, right);
                    }
                    continue;
                },
                .pushMinus => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const left = self.stack.buf[self.stack.top-1];
                    const right = self.stack.buf[self.stack.top];
                    self.stack.buf[self.stack.top-1] = evalMinus(left, right);
                    continue;
                },
                .pushMinus1 => {
                    @setRuntimeSafety(debug);
                    const leftOffset = self.ops[self.pc+1].arg;
                    const rightOffset = self.ops[self.pc+2].arg;
                    self.pc += 3;

                    if (leftOffset == NullByteId) {
                        const left = self.stack.buf[self.stack.top-1];
                        const right = self.getStackFrameValue(rightOffset);
                        self.stack.buf[self.stack.top-1] = evalMinus(left, right);
                        continue;
                    } else {
                        const left = self.getStackFrameValue(leftOffset);
                        const right = self.stack.buf[self.stack.top-1];
                        self.stack.buf[self.stack.top-1] = evalMinus(left, right);
                        continue;
                    }
                },
                .pushMinus2 => {
                    @setRuntimeSafety(debug);
                    try self.checkStackHasOneSpace();
                    const leftOffset = self.ops[self.pc+1].arg;
                    const rightOffset = self.ops[self.pc+2].arg;
                    self.pc += 3;

                    const left = self.getStackFrameValue(leftOffset);
                    const right = self.getStackFrameValue(rightOffset);
                    self.pushValueNoCheck(@call(.{ .modifier = .never_inline }, evalMinus, .{left, right}));
                    continue;
                },
                .pushList => {
                    @setRuntimeSafety(debug);
                    const numElems = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    const elems = self.stack.buf[self.stack.top-numElems..self.stack.top];
                    const list = try self.allocList(elems);
                    if (trace) {
                        self.trace.numRetains += 1;
                    }
                    self.stack.top = self.stack.top - numElems + 1;
                    self.stack.buf[self.stack.top-1] = list;
                    continue;
                },
                .pushMapEmpty => {
                    @setRuntimeSafety(debug);
                    try self.checkStackHasOneSpace();
                    self.pc += 1;

                    const map = try self.allocEmptyMap();
                    if (trace) {
                        self.trace.numRetains += 1;
                    }
                    self.pushValueNoCheck(map);
                    continue;
                },
                .pushMap => {
                    @setRuntimeSafety(debug);
                    const numEntries = self.ops[self.pc+1].arg;
                    const startConst = self.ops[self.pc+2].arg;
                    self.pc += 3;

                    const keys = self.consts[startConst..startConst+numEntries];
                    const vals = self.stack.buf[self.stack.top-numEntries..self.stack.top];
                    self.stack.top = self.stack.top-numEntries+1;

                    const map = try self.allocMap(keys, vals);
                    self.stack.buf[self.stack.top-1] = map;
                    continue;
                },
                .pushSlice => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 2;
                    const end = self.stack.buf[self.stack.top+1];
                    const start = self.stack.buf[self.stack.top];
                    const list = self.stack.buf[self.stack.top-1];
                    const newList = try self.sliceList(list, start, end);
                    self.stack.buf[self.stack.top-1] = newList;
                    continue;
                },
                .addSet => {
                    @setRuntimeSafety(debug);
                    const offset = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    const val = self.popRegister();

                    const left = self.getStackFrameValue(offset);
                    if (left.isNumber()) {
                        self.setStackFrameValue(offset, evalAddNumber(left, val));
                    } else {
                        self.setStackFrameValue(offset, evalAddOther(self, left, val));
                    }
                    continue;
                },
                .releaseSet => {
                    @setRuntimeSafety(debug);
                    const offset = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    const val = self.popRegister();
                    const existing = self.getStackFrameValue(offset);
                    self.release(existing, trace);
                    self.setStackFrameValue(offset, val);
                    continue;
                },
                .set => {
                    @setRuntimeSafety(debug);
                    const offset = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    const val = self.popRegister();
                    self.setStackFrameValue(offset, val);
                    continue;
                },
                .setIndex => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 3;
                    const right = self.stack.buf[self.stack.top+2];
                    const index = self.stack.buf[self.stack.top+1];
                    const left = self.stack.buf[self.stack.top];
                    try self.setIndex(left, index, right);
                    continue;
                },
                .load => {
                    @setRuntimeSafety(debug);
                    try self.checkStackHasOneSpace();
                    const offset = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    const val = self.getStackFrameValue(offset);
                    self.pushValueNoCheck(val);
                    continue;
                },
                .loadRetain => {
                    @setRuntimeSafety(debug);
                    try self.checkStackHasOneSpace();
                    const offset = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    const val = self.getStackFrameValue(offset);
                    self.pushValueNoCheck(val);
                    self.retain(val);
                    if (trace) {
                        self.trace.numRetains += 1;
                    }
                    continue;
                },
                .pushIndex => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const index = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    const val = try @call(.{.modifier = .never_inline}, self.getIndex, .{left, index});
                    self.stack.buf[self.stack.top - 1] = val;
                    continue;
                },
                .pushReverseIndex => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const index = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    const val = try @call(.{.modifier = .never_inline}, self.getReverseIndex, .{left, index});
                    self.stack.buf[self.stack.top - 1] = val;
                    continue;
                },
                .jumpBack => {
                    @setRuntimeSafety(debug);
                    self.pc -= self.ops[self.pc+1].arg;
                    continue;
                },
                .jump => {
                    @setRuntimeSafety(debug);
                    self.pc += self.ops[self.pc+1].arg;
                    continue;
                },
                .jumpNotCond => {
                    @setRuntimeSafety(debug);
                    const pcOffset = self.ops[self.pc+1].arg;
                    const cond = self.popRegister();
                    const condVal = if (cond.isBool()) b: {
                        break :b cond.asBool();
                    } else b: {
                        @setCold(true);
                        break :b @call(.{ .modifier = .never_inline }, cond.toBool, .{});
                    };
                    if (!condVal) {
                        self.pc += pcOffset;
                    } else {
                        self.pc += 2;
                    }
                    continue;
                },
                .release => {
                    @setRuntimeSafety(debug);
                    const local = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    // TODO: Inline if heap object.
                    @call(.{ .modifier = .never_inline }, self.release, .{self.getStackFrameValue(local), trace});
                    continue;
                },
                .pushCall0 => {
                    @setRuntimeSafety(debug);
                    const numArgs = self.ops[self.pc+1].arg;
                    self.pc += 2;

                    const callee = self.stack.buf[self.stack.top - 1];
                    const retInfo = self.buildReturnInfo(0, true);
                    try @call(.{ .modifier = .never_inline }, self.call, .{callee, numArgs, retInfo});
                    continue;
                },
                .pushCall1 => {
                    @setRuntimeSafety(debug);
                    const numArgs = self.ops[self.pc+1].arg;
                    self.pc += 2;

                    const callee = self.stack.buf[self.stack.top - 1];
                    const retInfo = self.buildReturnInfo(1, true);
                    try @call(.{ .modifier = .never_inline }, self.call, .{callee, numArgs, retInfo});
                    continue;
                },
                .call => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    continue;
                },
                .callStr => {
                    @setRuntimeSafety(debug);
                    // const numArgs = self.ops[self.pc+1].arg;
                    // const str = self.extras[self.extraPc].two;
                    self.pc += 3;

                    // const top = self.registers.items.len;
                    // const vals = self.registers.items[top-numArgs..top];
                    // self.registers.items.len = top-numArgs;

                    // self.callStr(vals[0], self.strBuf[str[0]..str[1]], vals[1..]);
                    continue;
                },
                .pushCallObjSym0 => {
                    @setRuntimeSafety(debug);
                    const symId = self.ops[self.pc+1].arg;
                    const numArgs = self.ops[self.pc+2].arg;
                    self.pc += 3;

                    self.callObjSym(symId, numArgs, 0);
                    continue;
                },
                .pushCallObjSym1 => {
                    @setRuntimeSafety(debug);
                    const symId = self.ops[self.pc+1].arg;
                    const numArgs = self.ops[self.pc+2].arg;
                    self.pc += 3;

                    self.callObjSym(symId, numArgs, 1);
                    continue;
                },
                .pushCallSym0 => {
                    @setRuntimeSafety(debug);
                    const symId = self.ops[self.pc+1].arg;
                    const numArgs = self.ops[self.pc+2].arg;
                    self.pc += 3;

                    try self.callSym(symId, numArgs, 0);
                    // try @call(.{ .modifier = .always_inline }, self.callSym, .{ symId, vals, false });
                    continue;
                },
                .pushCallSym1 => {
                    @setRuntimeSafety(debug);
                    const symId = self.ops[self.pc+1].arg;
                    const numArgs = self.ops[self.pc+2].arg;
                    self.pc += 3;

                    try self.callSym(symId, numArgs, 1);
                    // try @call(.{ .modifier = .always_inline }, self.callSym, .{ symId, vals, true });
                    continue;
                },
                .pushField => {
                    @setRuntimeSafety(debug);
                    const symId = self.ops[self.pc+1].arg;
                    self.pc += 2;

                    const recv = self.stack.buf[self.stack.top-1];
                    // const val = try self.getField(symId, recv);
                    const val = self.getField(symId, recv);
                    self.stack.buf[self.stack.top-1] = val;
                    continue;
                },
                .pushLambda => {
                    @setRuntimeSafety(debug);
                    try self.checkStackHasOneSpace();
                    const funcPc = self.pc - self.ops[self.pc+1].arg;
                    const numParams = self.ops[self.pc+2].arg;
                    const numLocals = self.ops[self.pc+3].arg;
                    self.pc += 4;

                    const lambda = try self.allocLambda(funcPc, numParams, numLocals);
                    self.pushValueNoCheck(lambda);
                    continue;
                },
                .pushClosure => {
                    @setRuntimeSafety(debug);
                    const funcPc = self.pc - self.ops[self.pc+1].arg;
                    const numParams = self.ops[self.pc+2].arg;
                    const numCaptured = self.ops[self.pc+3].arg;
                    const numLocals = self.ops[self.pc+4].arg;
                    self.pc += 5;

                    const capturedVals = self.stack.buf[self.stack.top-numCaptured..self.stack.top];
                    const closure = try self.allocClosure(funcPc, numParams, numLocals, capturedVals);
                    self.stack.top = self.stack.top-numCaptured+1;
                    self.stack.buf[self.stack.top-1] = closure;
                    continue;
                },
                .forIter => {
                    @setRuntimeSafety(debug);
                    const local = self.ops[self.pc+1].arg;
                    const endPc = self.pc + self.ops[self.pc+2].arg;
                    const innerPc = self.pc + 3;

                    // try self.callObjSym(self.iteratorObjSym, 1, 1);
                    self.callObjSym(self.iteratorObjSym, 1, 1);
                    const iter = self.popRegister();
                    if (local == 255) {
                        while (true) {
                            self.pushValueNoCheck(iter);
                            // try self.callObjSym(self.nextObjSym, 1, 1);
                            self.callObjSym(self.nextObjSym, 1, 1);
                            const next = self.popRegister();
                            if (next.isNone()) {
                                break;
                            }
                            self.pc = innerPc;
                            @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{trace}) catch |err| {
                                if (err == error.BreakLoop) {
                                    break;
                                } else return err;
                            };
                        }
                    } else {
                        while (true) {
                            self.pushValueNoCheck(iter);
                            // try self.callObjSym(self.nextObjSym, 1, 1);
                            self.callObjSym(self.nextObjSym, 1, 1);
                            const next = self.popRegister();
                            if (next.isNone()) {
                                break;
                            }
                            self.setStackFrameValue(local, next);
                            self.pc = innerPc;
                            @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{trace}) catch |err| {
                                if (err == error.BreakLoop) {
                                    break;
                                } else return err;
                            };
                        }
                    }
                    self.pc = endPc;
                    continue;
                },
                .forRange => {
                    @setRuntimeSafety(debug);
                    const local = self.ops[self.pc+1].arg;
                    const endPc = self.pc + self.ops[self.pc+2].arg;
                    const innerPc = self.pc + 3;

                    self.stack.top -= 3;
                    const step = self.stack.buf[self.stack.top+2].toF64();
                    const rangeEnd = self.stack.buf[self.stack.top+1].toF64();
                    var i = self.stack.buf[self.stack.top].toF64();

                    if (i <= rangeEnd) {
                        if (local == 255) {
                            while (i < rangeEnd) : (i += step) {
                                self.pc = innerPc;
                                @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{trace}) catch |err| {
                                    if (err == error.BreakLoop) {
                                        break;
                                    } else return err;
                                };
                            }
                        } else {
                            while (i < rangeEnd) : (i += step) {
                                self.setStackFrameValue(local, .{ .val = @bitCast(u64, i) });
                                self.pc = innerPc;
                                @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{trace}) catch |err| {
                                    if (err == error.BreakLoop) {
                                        break;
                                    } else return err;
                                };
                            }
                        }
                    } else {
                        if (local == 255) {
                            while (i > rangeEnd) : (i -= step) {
                                self.pc = innerPc;
                                @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{trace}) catch |err| {
                                    if (err == error.BreakLoop) {
                                        break;
                                    } else return err;
                                };
                            }
                        } else {
                            while (i > rangeEnd) : (i -= step) {
                                self.setStackFrameValue(local, .{ .val = @bitCast(u64, i) });
                                self.pc = innerPc;
                                @call(.{ .modifier = .never_inline }, evalLoopGrowStack, .{trace}) catch |err| {
                                    if (err == error.BreakLoop) {
                                        break;
                                    } else return err;
                                };
                            }
                        }
                    }
                    self.pc = endPc;
                    continue;
                },
                .cont => {
                    @setRuntimeSafety(debug);
                    return;
                },
                // .ret2 => {
                //     @setRuntimeSafety(debug);
                //     // Using never_inline seems to work against the final compiler optimizations.
                //     // @call(.{ .modifier = .never_inline }, self.popStackFrameCold, .{2});
                //     self.popStackFrameCold(2);
                //     continue;
                // },
                .ret1 => {
                    @setRuntimeSafety(debug);
                    if (@call(.{ .modifier = .always_inline }, self.popStackFrame, .{1})) {
                        continue;
                    } else {
                        return;
                    }
                },
                .ret0 => {
                    @setRuntimeSafety(debug);
                    if (@call(.{ .modifier = .always_inline }, self.popStackFrame, .{0})) {
                        continue;
                    } else {
                        return;
                    }
                },
                .pushMultiply => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const left = self.stack.buf[self.stack.top-1];
                    const right = self.stack.buf[self.stack.top];
                    self.stack.buf[self.stack.top-1] = @call(.{ .modifier = .never_inline }, evalMultiply, .{left, right});
                    continue;
                },
                .pushDivide => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const left = self.stack.buf[self.stack.top-1];
                    const right = self.stack.buf[self.stack.top];
                    self.stack.buf[self.stack.top-1] = @call(.{ .modifier = .never_inline }, evalDivide, .{left, right});
                    continue;
                },
                .pushMod => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const left = self.stack.buf[self.stack.top-1];
                    const right = self.stack.buf[self.stack.top];
                    self.stack.buf[self.stack.top-1] = @call(.{ .modifier = .never_inline }, evalMod, .{left, right});
                    continue;
                },
                .pushPower => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const left = self.stack.buf[self.stack.top-1];
                    const right = self.stack.buf[self.stack.top];
                    self.stack.buf[self.stack.top-1] = @call(.{ .modifier = .never_inline }, evalPower, .{left, right});
                    continue;
                },
                .end => {
                    return error.End;
                },
            }
        }
    }

    pub fn isValueString(self: *VM, val: Value) bool {
        @setRuntimeSafety(debug);
        _ = self;
        if (val.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, val.asPointer().?);
            return obj.common.structId == StringS;
        } else {
            const tag = val.getTag();
            return tag == cy.TagConstString;
        }
    }

    pub fn valueAsString(self: *VM, val: Value) []const u8 {
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
            if (Value.floatCanBeInteger(val.asF64())) {
                return std.fmt.bufPrint(&tempU8Buf, "{}", .{@floatToInt(u64, val.asF64())}) catch stdx.fatal();
            } else {
                return std.fmt.bufPrint(&tempU8Buf, "{d:.10}", .{val.asF64()}) catch stdx.fatal();
            }
        } else {
            if (val.isPointer()) {
                const obj = stdx.ptrCastAlign(*HeapObject, val.asPointer().?);
                if (obj.common.structId == StringS) {
                    return obj.string.ptr[0..obj.string.len];
                } else {
                    stdx.panicFmt("unexpected struct {}", .{obj.common.structId});
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
                    else => stdx.panicFmt("unexpected tag {}", .{val.getTag()}),
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
                    stdx.panicFmt("unexpected struct {}", .{obj.common.structId});
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
                    else => stdx.panicFmt("unexpected tag {}", .{val.getTag()}),
                }
            }
        }
    }
};

fn evalAnd(left: cy.Value, right: cy.Value) cy.Value {
    if (left.isNumber()) {
        if (left.asF64() == 0) {
            return left;
        } else {
            return right;
        }
    } else {
        switch (left.getTag()) {
            cy.TagBoolean => {
                if (left.asBool()) return right else return left;
            },
            cy.TagNone => return left,
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalOr(left: cy.Value, right: cy.Value) cy.Value {
    if (left.isNumber()) {
        if (left.asF64() == 0) {
            return right;
        } else {
            return left;
        }
    } else {
        switch (left.getTag()) {
            cy.TagBoolean => {
                if (left.asBool()) return left else return right;
            },
            cy.TagNone => return right,
            else => stdx.panic("unexpected tag"),
        }
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

fn evalLess(left: cy.Value, right: cy.Value) linksection(".eval") cy.Value {
    @setRuntimeSafety(debug);
    return Value.initBool(left.toF64() < right.toF64());
}

fn evalNotCompare(left: cy.Value, right: cy.Value) cy.Value {
    if (left.isNumber()) {
        return Value.initBool(right.isNumber() and left.asF64() != right.asF64());
    } else {
        switch (left.getTag()) {
            cy.TagNone => return Value.initBool(!right.isNone()),
            cy.TagBoolean => return Value.initBool(left.asBool() != right.toBool()),
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalCompare(left: cy.Value, right: cy.Value) cy.Value {
    if (left.isNumber()) {
        return Value.initBool(right.isNumber() and left.asF64() == right.asF64());
    } else {
        switch (left.getTag()) {
            cy.TagNone => return Value.initBool(right.isNone()),
            cy.TagBoolean => return Value.initBool(left.asBool() == right.toBool()),
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalMinus(left: cy.Value, right: cy.Value) linksection(".eval") cy.Value {
    @setRuntimeSafety(debug);
    if (left.isNumber()) {
        return Value.initF64(left.asF64() - right.toF64());
    } else {
        return @call(.{ .modifier = .never_inline }, evalMinusOther, .{left, right});
    }
}

fn evalMinusOther(left: Value, right: Value) linksection(".eval") Value {
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

fn evalAddOther(vm: *VM, left: cy.Value, right: cy.Value) linksection(".eval") cy.Value {
    @setRuntimeSafety(debug);
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
            const str = vm.strBuf[slice.start..slice.end];
            return vm.allocStringConcat(str, vm.valueToTempString(right)) catch stdx.fatal();
        },
        else => stdx.panic("unexpected tag"),
    }
}

inline fn evalAddNumber(left: cy.Value, right: cy.Value) linksection(".eval") cy.Value {
    @setRuntimeSafety(debug);
    return Value.initF64(left.asF64() + right.toF64());
}

fn evalNeg(val: Value) Value {
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
        return cy.Value.initFalse();
    } else {
        switch (val.getTag()) {
            cy.TagNone => return cy.Value.initTrue(),
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
    numLocals: u8,
};

const Closure = packed struct {
    structId: StructId,
    rc: u32,
    funcPc: u32, 
    numParams: u8,
    numCaptured: u8,
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

const List = packed struct {
    structId: StructId,
    rc: u32,
    // inner: std.ArrayListUnmanaged(Value),
    list: packed struct {
        ptr: [*]Value,
        len: usize,
        cap: usize,
    },
    nextIterIdx: u32,
};

const HeapPage = struct {
    objects: [1600]HeapObject,
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
    retainedList: List,
    map: Map,
    closure: Closure,
    lambda: Lambda,
    string: String,
    retainedObject: packed struct {
        structId: StructId,
        rc: u32,
        ptr: *anyopaque,
        val0: Value,
        val1: Value,
        val2: Value,
    },
};

const SymbolMapType = enum {
    oneStruct,
    // twoStructs,
    // manyStructs,
    empty,
};

const FieldSymbolMap = struct {
    mapT: SymbolMapType,
    inner: union {
        oneStruct: struct {
            id: StructId,
            fieldIdx: u32,
        },
    },
    name: []const u8,
};

/// Keeping this small is better for function calls. TODO: Reduce size.
/// Secondary symbol data should be moved to `symbolsInfo`.
const SymbolMap = struct {
    mapT: SymbolMapType,
    inner: union {
        oneStruct: struct {
            id: StructId,
            sym: SymbolEntry,
        },
        // twoStructs: struct {
        // },
        manyStructs: struct {
            map: std.AutoHashMapUnmanaged(StructId, SymbolEntry),

            fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
                self.map.deinit(alloc);
            }
        },
    },
};

test "Internals." {
    try t.eq(@sizeOf(SymbolMap), 40);
    try t.eq(@sizeOf(SymbolEntry), 16);
    try t.eq(@sizeOf(MapInner), 32);
    try t.eq(@sizeOf(HeapObject), 40);
    try t.eq(@sizeOf(HeapPage), 40 * 1600);
}

const SymbolEntryType = enum {
    func,
    nativeFunc1,
    nativeFunc2,
};

pub const SymbolEntry = struct {
    entryT: SymbolEntryType,
    inner: packed union {
        nativeFunc1: std.meta.FnPtr(fn (UserVM, *anyopaque, [*]const Value, u8) Value),
        nativeFunc2: std.meta.FnPtr(fn (UserVM, *anyopaque, [*]const Value, u8) cy.ValuePair),
        func: packed struct {
            pc: u32,
        },
    },

    pub fn initNativeFunc1(func: std.meta.FnPtr(fn (UserVM, *anyopaque, [*]const Value, u8) Value)) SymbolEntry {
        return .{
            .entryT = .nativeFunc1,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    fn initNativeFunc2(func: std.meta.FnPtr(fn (UserVM, *anyopaque, [*]const Value, u8) cy.ValuePair)) SymbolEntry {
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
    none,
};

pub const FuncSymbolEntry = struct {
    entryT: FuncSymbolEntryType,
    inner: packed union {
        nativeFunc1: std.meta.FnPtr(fn (*VM, [*]const Value, u8) Value),
        func: packed struct {
            pc: usize,
            /// Includes function params, locals, and return info slot.
            numLocals: u32,
        },
    },

    pub fn initNativeFunc1(func: std.meta.FnPtr(fn (*VM, [*]const Value, u8) Value)) FuncSymbolEntry {
        return .{
            .entryT = .nativeFunc1,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    pub fn initFunc(pc: usize, numLocals: u32) FuncSymbolEntry {
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

const StructId = u32;

const Struct = struct {
    name: []const u8,
};

// const StructSymbol = struct {
//     name: []const u8,
// };

const SymbolId = u32;

pub const TraceInfo = struct {
    opCounts: []OpCount,
    totalOpCounts: u32,
    numRetains: u32,
    numReleases: u32,
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
        gvm.trace = trace;
    }

    pub inline fn release(_: UserVM, val: Value, comptime trace: bool) void {
        gvm.release(val, trace);
    }

    pub inline fn checkMemory(_: UserVM, comptime trace: bool) !bool {
        return gvm.checkMemory(trace);
    }

    pub inline fn eval(_: UserVM, src: []const u8, comptime trace: bool) !Value {
        return gvm.eval(src, trace);
    }

    pub inline fn isValueString(_: UserVM, val: Value) bool {
        return gvm.isValueString(val);
    }

    pub inline fn valueAsString(_: UserVM, val: Value) []const u8 {
        return gvm.valueAsString(val);
    }
};

/// To reduce the amount of code inlined in the hot loop, handle StackOverflow at the top and resume execution.
/// This is also the entry way for native code to call into the VM without deoptimizing the hot loop.
pub fn evalLoopGrowStack(comptime trace: bool) linksection(".eval") error{StackOverflow, OutOfMemory, Panic, OutOfBounds, End}!void {
    @setRuntimeSafety(debug);
    while (true) {
        @call(.{ .modifier = .always_inline }, gvm.evalLoop, .{trace}) catch |err| {
            @setCold(true);
            if (err == error.StackOverflow) {
                try gvm.stack.growTotalCapacity(gvm.alloc, gvm.stack.buf.len + 1);
                continue;
            } else if (err == error.End) {
                return;
            } else return err;
        };
        return;
    }
}