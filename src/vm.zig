const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const cy = @import("cyber.zig");
const Value = cy.Value;
const debug = builtin.mode == .Debug;

const log = stdx.log.scoped(.vm);

pub const VM = struct {
    alloc: std.mem.Allocator,
    parser: cy.Parser,
    compiler: cy.VMcompiler,

    /// [Eval context]

    /// Program counter. Index to the next instruction op in `ops`.
    pc: usize,
    /// Current stack frame ptr. Previous stack frame info is saved as a Value after all the reserved locals.
    framePtr: usize,
    contFlag: bool,

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

    panicMsg: []const u8,

    trace: *TraceInfo,

    /// Reserved symbols known at comptime.
    const ListS: StructId = 0;
    const MapS: StructId = 1;
    const ClosureS: StructId = 2;
    const StringS: StructId = 3;

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
            .contFlag = false,
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
        };
        self.compiler.init(self);
        try self.ensureStackTotalCapacity(100);

        // Initialize heap.
        self.heapFreeHead = try self.growHeapPages(1);

        // Init compile time builtins.
        const resize = try self.ensureStructSym("resize");
        var id = try self.addStruct("List");
        std.debug.assert(id == ListS);
        try self.addStructSym(ListS, resize, SymbolEntry.initNativeFunc1(nativeListResize));
        self.iteratorObjSym = try self.ensureStructSym("iterator");
        try self.addStructSym(ListS, self.iteratorObjSym, SymbolEntry.initNativeFunc1(nativeListIterator));
        self.nextObjSym = try self.ensureStructSym("next");
        try self.addStructSym(ListS, self.nextObjSym, SymbolEntry.initNativeFunc1(nativeListNext));
        const add = try self.ensureStructSym("add");
        try self.addStructSym(ListS, add, SymbolEntry.initNativeFunc1(nativeListAdd));

        id = try self.addStruct("Map");
        std.debug.assert(id == MapS);
        const remove = try self.ensureStructSym("remove");
        try self.addStructSym(MapS, remove, SymbolEntry.initNativeFunc1(nativeMapRemove));

        id = try self.addStruct("Closure");
        std.debug.assert(id == ClosureS);

        id = try self.addStruct("String");
        std.debug.assert(id == StringS);
    }

    pub fn deinit(self: *VM) void {
        self.parser.deinit();
        self.compiler.deinit();
        self.stack.deinit(self.alloc);

        for (self.symbols.items) |*map| {
            if (map.mapT == .manyStructs) {
                map.inner.manyStructs.deinit(self.alloc);
            }
        }
        self.symbols.deinit(self.alloc);
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
            res.buf.dump();
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
                std.sort.sort(cy.OpCount, self.trace.opCounts, {}, S.opCountLess);
                var i: u32 = 0;
                const numOps = @enumToInt(cy.OpCode.end) + 1;
                while (i < numOps) : (i += 1) {
                    if (self.trace.opCounts[i].count > 0) {
                        const op = std.meta.intToEnum(cy.OpCode, self.trace.opCounts[i].code) catch continue;
                        log.info("{} {}", .{op, self.trace.opCounts[i].count});
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

    pub inline fn popStackFrame(self: *VM, comptime numRetVals: u2) linksection(".eval") void {
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
                    return;
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
                    return;
                }
            },
            1 => {
                @setRuntimeSafety(debug);
                const retInfo = self.stack.buf[self.stack.top-2];
                const reqNumArgs = retInfo.retInfo.numRetVals;
                if (reqNumArgs == 1) {
                    // Copy return value to retInfo.
                    self.stack.buf[self.framePtr] = self.stack.buf[self.stack.top-1];
                    self.stack.top = self.framePtr + 1;

                    // Restore pc.
                    self.framePtr = retInfo.retInfo.framePtr;
                    self.pc = retInfo.retInfo.pc;
                    return;
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
                    return;
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

        try self.evalStackFrame(trace);

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
                try stdx.panic("expected list");
            }
        } else {
            try stdx.panic("expected pointer");
        }
    }

    fn allocEmptyMap(self: *VM) !Value {
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

    fn freeObject(self: *VM, obj: *HeapObject) void {
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
                return error.Unsupported;
            }
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
        return Value.initPtr(obj);
    }

    fn allocList(self: *VM, elems: []const Value) !Value {
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

    inline fn popRegister(self: *VM) linksection(".eval") Value {
        @setRuntimeSafety(debug);
        self.stack.top -= 1;
        return self.stack.buf[self.stack.top];
    }

    inline fn ensureStackTotalCapacity(self: *VM, newCap: usize) linksection(".eval") !void {
        if (newCap > self.stack.buf.len) {
            try self.stack.growTotalCapacity(self.alloc, newCap);
        }
    }

    inline fn pushRegister(self: *VM, val: Value) linksection(".eval") !void {
        @setRuntimeSafety(debug);
        if (self.stack.top == self.stack.buf.len) {
            try self.stack.growTotalCapacity(self.alloc, self.stack.top + 1);
        }
        self.stack.buf[self.stack.top] = val;
        self.stack.top += 1;
    }

    fn addStruct(self: *VM, name: []const u8) !StructId {
        _ = name;
        const s = Struct{
            .name = "",
        };
        const id = @intCast(u32, self.structs.items.len);
        try self.structs.append(self.alloc, s);
        return id;
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
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub inline fn setFuncSym(self: *VM, symId: SymbolId, sym: FuncSymbolEntry) !void {
        self.funcSyms.items[symId] = sym;
    }

    fn addStructSym(self: *VM, id: StructId, symId: SymbolId, sym: SymbolEntry) !void {
        switch (self.symbols.items[symId].mapT) {
            .empty => {
                self.symbols.items[symId] = .{
                    .mapT = .oneStruct,
                    .inner = .{
                        .oneStruct = .{
                            .id = id,
                            .sym = sym,
                        },
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

    fn nativeMapRemove(self: *VM, ptr: *anyopaque, args: []const Value) Value {
        @setRuntimeSafety(debug);
        if (args.len == 0) {
            stdx.panic("Args mismatch");
        }
        // const key = toMapKey(args[0]);
        const obj = stdx.ptrCastAlign(*HeapObject, ptr);
        const inner = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
        _ = inner.remove(self, args[0]);
        return Value.initNone();
    }

    fn nativeListAdd(self: *VM, ptr: *anyopaque, args: []const Value) Value {
        @setRuntimeSafety(debug);
        if (args.len == 0) {
            stdx.panic("Args mismatch");
        }
        const list = stdx.ptrCastAlign(*HeapObject, ptr);
        const inner = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &list.retainedList.list);
        inner.append(self.alloc, args[0]) catch stdx.fatal();
        return Value.initNone();
    }

    fn nativeListNext(self: *VM, ptr: *anyopaque, args: []const Value) Value {
        @setRuntimeSafety(debug);
        _ = self;
        _ = args;
        const list = stdx.ptrCastAlign(*HeapObject, ptr);
        if (list.retainedList.nextIterIdx < list.retainedList.list.len) {
            defer list.retainedList.nextIterIdx += 1;
            return list.retainedList.list.ptr[list.retainedList.nextIterIdx];
        } else return Value.initNone();
    }

    fn nativeListIterator(self: *VM, ptr: *anyopaque, args: []const Value) Value {
        _ = self;
        _ = args;
        const list = stdx.ptrCastAlign(*HeapObject, ptr);
        list.retainedList.nextIterIdx = 0;
        return Value.initPtr(ptr);
    }

    fn nativeListResize(self: *VM, ptr: *anyopaque, args: []const Value) Value {
        if (args.len == 0) {
            stdx.panic("Args mismatch");
        }
        const list = stdx.ptrCastAlign(*HeapObject, ptr);
        const inner = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &list.retainedList.list);
        const size = @floatToInt(u32, args[0].toF64());
        inner.resize(self.alloc, size) catch stdx.fatal();
        return Value.initNone();
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
            const obj = stdx.ptrCastAlign(*GenericObject, val.asPointer());
            obj.rc += 1;
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
                        list.deinit(self.alloc);
                        self.freeObject(obj);
                    },
                    MapS => {
                        const map = stdx.ptrCastAlign(*MapInner, &obj.map.inner);
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
                    StringS => {
                        self.alloc.free(obj.string.ptr[0..obj.string.len]);
                        self.freeObject(obj);
                    },
                    else => {
                        return stdx.panic("unsupported struct type");
                    },
                }
            }
        }
    }

    fn pushField(self: *const VM, symId: SymbolId, recv: Value) void {
        @setRuntimeSafety(debug);
        if (recv.isPointer()) {
            const obj = stdx.ptrCastAlign(*GenericObject, recv.asPointer());
            const map = self.fieldSyms.items[symId];
            switch (map.mapT) {
                .oneStruct => {
                    if (obj.structId == map.inner.oneStruct.id) {
                        stdx.panic("TODO: get field");
                    } else stdx.panic("Symbol does not exist.");
                },
                .empty => stdx.panic("Symbol does not exist."),
                else => stdx.panicFmt("unsupported {}", .{map.mapT}),
            } 
        } else stdx.panic("Symbol does not exist.");
    }

    fn call(self: *VM, callee: Value, numArgs: u8, retInfo: Value) !void {
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
                const res = sym.inner.nativeFunc1(self, args);
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
                const retInfo = self.buildReturnInfo(reqNumRetVals);
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
            .none => stdx.panic("Symbol doesn't exist."),
            // else => stdx.panic("unsupported callsym"),
        }
    }

    inline fn callSymEntry(self: *VM, entry: SymbolEntry, argStart: usize, objPtr: *anyopaque, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") void {
        _ = numArgs;
        @setRuntimeSafety(debug);
        switch (entry.entryT) {
            .nativeFunc1 => {
                const args = self.stack.buf[argStart + 1..self.stack.top];
                const res = entry.inner.nativeFunc1(self, objPtr, args);
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
                const func = @ptrCast(std.meta.FnPtr(fn (*VM, *anyopaque, []const Value) cy.ValuePair), entry.inner.nativeFunc2);
                const args = self.stack.buf[argStart + 1..self.stack.top];
                const res = func(self, objPtr, args);
                if (reqNumRetVals == 2) {
                    self.stack.buf[argStart] = res.left;
                    self.stack.buf[argStart + 1] = res.right;
                    self.stack.top = argStart + 1;
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
            else => stdx.panicFmt("unsupported {}", .{entry.entryT}),
        }
    }

    fn callObjSym(self: *VM, symId: SymbolId, numArgs: u8, comptime reqNumRetVals: u2) linksection(".eval") void {
        @setRuntimeSafety(debug);
        // numArgs includes the receiver.
        const argStart = self.stack.top - numArgs;
        const recv = self.stack.buf[argStart];
        if (recv.isPointer()) {
            const obj = stdx.ptrCastAlign(*HeapObject, recv.asPointer().?);
            const map = self.symbols.items[symId];
            switch (map.mapT) {
                .oneStruct => {
                    if (obj.retainedCommon.structId == map.inner.oneStruct.id) {
                        self.callSymEntry(map.inner.oneStruct.sym, argStart, obj, numArgs, reqNumRetVals);
                    } else stdx.panic("Symbol does not exist for receiver.");
                },
                else => stdx.panicFmt("unsupported {}", .{map.mapT}),
            } 
        }
    }

    inline fn buildReturnInfo(self: *const VM, comptime numRetVals: u2) linksection(".eval") Value {
        @setRuntimeSafety(debug);
        return Value{
            .retInfo = .{
                .pc = @intCast(u32, self.pc),
                .framePtr = @intCast(u30, self.framePtr),
                .numRetVals = numRetVals,
            },
        };
    }

    fn evalStackFrame(self: *VM, comptime trace: bool) linksection(".eval") anyerror!void {
        @setRuntimeSafety(debug);
        while (true) {
            if (trace) {
                const op = self.ops[self.pc].code;
                self.trace.opCounts[@enumToInt(op)].count += 1;
                self.trace.totalOpCounts += 1;
            }
            log.debug("{} op: {}", .{self.pc, self.ops[self.pc].code});
            switch (self.ops[self.pc].code) {
                .pushTrue => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    try self.pushRegister(Value.initTrue());
                    continue;
                },
                .pushFalse => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    try self.pushRegister(Value.initFalse());
                    continue;
                },
                .pushNone => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    try self.pushRegister(Value.initNone());
                    continue;
                },
                .pushConst => {
                    @setRuntimeSafety(debug);
                    const idx = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    try self.pushRegister(Value{ .val = self.consts[idx].val });
                    continue;
                },
                .pushNot => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    const val = self.popRegister();
                    try self.pushRegister(evalNot(val));
                    continue;
                },
                .pushCompare => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    const right = self.popRegister();
                    const left = self.popRegister();
                    try self.pushRegister(evalCompare(left, right));
                    continue;
                },
                .pushLess => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    const right = self.popRegister();
                    const left = self.popRegister();
                    try self.pushRegister(evalLess(left, right));
                    continue;
                },
                .pushGreater => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    const right = self.popRegister();
                    const left = self.popRegister();
                    try self.pushRegister(evalGreater(left, right));
                    continue;
                },
                .pushLessEqual => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    const right = self.popRegister();
                    const left = self.popRegister();
                    try self.pushRegister(evalLessOrEqual(left, right));
                    continue;
                },
                .pushGreaterEqual => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    const right = self.popRegister();
                    const left = self.popRegister();
                    try self.pushRegister(evalGreaterOrEqual(left, right));
                    continue;
                },
                .pushOr => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    const right = self.popRegister();
                    const left = self.popRegister();
                    try self.pushRegister(evalOr(left, right));
                    continue;
                },
                .pushAnd => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    const right = self.popRegister();
                    const left = self.popRegister();
                    try self.pushRegister(evalAnd(left, right));
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
                    const leftOffset = self.ops[self.pc+1].arg;
                    const rightOffset = self.ops[self.pc+2].arg;
                    self.pc += 3;

                    const left = self.getStackFrameValue(leftOffset);
                    const right = self.getStackFrameValue(rightOffset);
                    try self.pushRegister(evalMinus(left, right));
                    continue;
                },
                .pushList => {
                    @setRuntimeSafety(debug);
                    const numElems = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    const top = self.stack.top;
                    const elems = self.stack.buf[top-numElems..top];
                    const list = try self.allocList(elems);
                    if (trace) {
                        self.trace.numRetains += 1;
                    }
                    self.stack.top = top-numElems;
                    try self.pushRegister(list);
                    continue;
                },
                .pushMapEmpty => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;

                    const map = try self.allocEmptyMap();
                    if (trace) {
                        self.trace.numRetains += 1;
                    }
                    try self.pushRegister(map);
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
                    const end = self.popRegister();
                    const start = self.popRegister();
                    const list = self.popRegister();
                    const newList = try self.sliceList(list, start, end);
                    try self.pushRegister(newList);
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
                    const right = self.popRegister();
                    const index = self.popRegister();
                    const left = self.popRegister();
                    try self.setIndex(left, index, right);
                    continue;
                },
                .load => {
                    @setRuntimeSafety(debug);
                    const offset = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    const val = self.getStackFrameValue(offset);
                    try self.pushRegister(val);
                    continue;
                },
                .loadRetain => {
                    @setRuntimeSafety(debug);
                    const offset = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    const val = self.getStackFrameValue(offset);
                    try self.pushRegister(val);
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
                    const val = try self.getIndex(left, index);
                    self.stack.buf[self.stack.top - 1] = val;
                    continue;
                },
                .pushReverseIndex => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const index = self.stack.buf[self.stack.top];
                    const left = self.stack.buf[self.stack.top-1];
                    const val = try self.getReverseIndex(left, index);
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
                    if (!cond.toBool()) {
                        self.pc += pcOffset;
                    } else {
                        self.pc += 2;
                    }
                    continue;
                },
                .release => {
                    @setRuntimeSafety(debug);
                    const offset = self.ops[self.pc+1].arg;
                    self.pc += 2;
                    self.release(self.getStackFrameValue(offset), trace);
                    continue;
                },
                .pushCall0 => {
                    @setRuntimeSafety(debug);
                    const numArgs = self.ops[self.pc+1].arg;
                    self.pc += 2;

                    const callee = self.stack.buf[self.stack.top - 1];
                    const retInfo = self.buildReturnInfo(0);
                    try self.call(callee, numArgs, retInfo);
                    continue;
                },
                .pushCall1 => {
                    @setRuntimeSafety(debug);
                    const numArgs = self.ops[self.pc+1].arg;
                    self.pc += 2;

                    const callee = self.stack.buf[self.stack.top - 1];
                    const retInfo = self.buildReturnInfo(1);
                    try self.call(callee, numArgs, retInfo);
                    continue;
                },
                .call => {
                    @setRuntimeSafety(debug);
                    stdx.unsupported();
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
                .callObjSym => {
                    @setRuntimeSafety(debug);
                    const symId = self.ops[self.pc+1].arg;
                    const numArgs = self.ops[self.pc+2].arg;
                    self.pc += 3;

                    self.callObjSym(symId, numArgs, 0);
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

                    const recv = self.popRegister();
                    self.pushField(symId, recv);
                    continue;
                },
                .pushLambda => {
                    @setRuntimeSafety(debug);
                    const funcOffset = self.ops[self.pc+1].arg;
                    const numParams = self.ops[self.pc+2].arg;
                    const numLocals = self.ops[self.pc+3].arg;
                    self.pc += 4;
                    _ = funcOffset;
                    _ = numParams;
                    _ = numLocals;
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

                    self.callObjSym(self.iteratorObjSym, 1, 1);
                    const iter = self.popRegister();
                    if (local == 255) {
                        while (true) {
                            try self.pushRegister(iter);
                            // const retInfo2 = self.buildReturnInfo(1);
                            self.callObjSym(self.nextObjSym, 1, 1);
                            const next = self.popRegister();
                            if (next.isNone()) {
                                break;
                            }
                            self.pc = innerPc;
                            _ = try @call(.{ .modifier = .never_inline }, self.evalStackFrame, .{trace});
                            if (!self.contFlag) {
                                break;
                            }
                        }
                    } else {
                        while (true) {
                            try self.pushRegister(iter);
                            // const retInfo2 = self.buildReturnInfo(1);
                            self.callObjSym(self.nextObjSym, 1, 1);
                            const next = self.popRegister();
                            if (next.isNone()) {
                                break;
                            }
                            self.setStackFrameValue(local, next);
                            self.pc = innerPc;
                            _ = try @call(.{ .modifier = .never_inline }, self.evalStackFrame, .{trace});
                            if (!self.contFlag) {
                                break;
                            }
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

                    const step = self.popRegister().toF64();
                    const rangeEnd = self.popRegister().toF64();
                    var i = self.popRegister().toF64();

                    // defer stdx.panicFmt("forrange {}", .{self.stack.top});

                    if (local == 255) {
                        while (i < rangeEnd) : (i += step) {
                            self.pc = innerPc;
                            _ = try @call(.{ .modifier = .never_inline }, self.evalStackFrame, .{trace});
                            if (!self.contFlag) {
                                break;
                            }
                        }
                    } else {
                        while (i < rangeEnd) : (i += step) {
                            self.setStackFrameValue(local, .{ .val = @bitCast(u64, i) });
                            self.pc = innerPc;
                            _ = try @call(.{ .modifier = .never_inline }, self.evalStackFrame, .{trace});
                            if (!self.contFlag) {
                                break;
                            }
                        }
                    }
                    self.pc = endPc;
                    continue;
                },
                .cont => {
                    @setRuntimeSafety(debug);
                    self.contFlag = true;
                    return;
                },
                .ret2 => {
                    @setRuntimeSafety(debug);
                    // Using never_inline seems to work against the final compiler optimizations.
                    // @call(.{ .modifier = .never_inline }, self.popStackFrameCold, .{2});
                    self.popStackFrameCold(2);
                    continue;
                },
                .ret1 => {
                    @setRuntimeSafety(debug);
                    @call(.{ .modifier = .always_inline }, self.popStackFrame, .{1});
                    continue;
                },
                .ret0 => {
                    @setRuntimeSafety(debug);
                    @call(.{ .modifier = .always_inline }, self.popStackFrame, .{0});
                    continue;
                },
                .pushMultiply => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const left = self.stack.buf[self.stack.top-1];
                    const right = self.stack.buf[self.stack.top];
                    self.stack.buf[self.stack.top-1] = evalMultiply(left, right);
                    continue;
                },
                .pushDivide => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const left = self.stack.buf[self.stack.top-1];
                    const right = self.stack.buf[self.stack.top];
                    self.stack.buf[self.stack.top-1] = evalDivide(left, right);
                    continue;
                },
                .pushMod => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const left = self.stack.buf[self.stack.top-1];
                    const right = self.stack.buf[self.stack.top];
                    self.stack.buf[self.stack.top-1] = evalMod(left, right);
                    continue;
                },
                .pushPower => {
                    @setRuntimeSafety(debug);
                    self.pc += 1;
                    self.stack.top -= 1;
                    const left = self.stack.buf[self.stack.top-1];
                    const right = self.stack.buf[self.stack.top];
                    self.stack.buf[self.stack.top-1] = evalPower(left, right);
                    continue;
                },
                .end => {
                    return;
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
            cy.TagFalse => return left,
            cy.TagTrue => return right,
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
            cy.TagFalse => return right,
            cy.TagTrue => return left,
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

fn evalLess(left: cy.Value, right: cy.Value) cy.Value {
    @setRuntimeSafety(debug);
    return Value.initBool(left.toF64() < right.toF64());
}

fn evalCompare(left: cy.Value, right: cy.Value) cy.Value {
    if (left.isNumber()) {
        return Value.initBool(right.isNumber() and left.asF64() == right.asF64());
    } else {
        switch (left.getTag()) {
            cy.TagFalse => return Value.initBool(right.isFalse()),
            cy.TagTrue => return Value.initBool(right.isTrue()),
            cy.TagNone => return Value.initBool(right.isNone()),
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalMinus(left: cy.Value, right: cy.Value) cy.Value {
    @setRuntimeSafety(debug);
    if (left.isNumber()) {
        return Value.initF64(left.asF64() - right.toF64());
    } else {
        switch (left.getTag()) {
            cy.TagFalse => return Value.initF64(-right.toF64()),
            cy.TagTrue => return Value.initF64(1 - right.toF64()),
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
            cy.TagFalse => return Value.initF64(0),
            cy.TagTrue => return Value.initF64(1),
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
            cy.TagFalse => return Value.initF64(0),
            cy.TagTrue => return Value.initF64(1.0 / right.toF64()),
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
            cy.TagFalse => {
                if (right.toF64() != 0) {
                    return Value.initF64(0);
                } else {
                    return Value.initF64(std.math.nan_f64);
                }
            },
            cy.TagTrue => {
                const rightf = right.toF64();
                if (rightf > 0) {
                    return Value.initF64(1);
                } else if (rightf == 0) {
                    return Value.initF64(std.math.nan_f64);
                } else {
                    return Value.initF64(rightf + 1);
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
            cy.TagFalse => return Value.initF64(0),
            cy.TagTrue => return Value.initF64(right.toF64()),
            cy.TagNone => return Value.initF64(0),
            else => stdx.panic("unexpected tag"),
        }
    }
}

fn evalAddOther(vm: *VM, left: cy.Value, right: cy.Value) cy.Value {
    @setRuntimeSafety(debug);
    switch (left.getTag()) {
        cy.TagFalse => return Value.initF64(right.toF64()),
        cy.TagTrue => return Value.initF64(1 + right.toF64()),
        cy.TagNone => return Value.initF64(right.toF64()),
        cy.TagError => stdx.fatal(),
        cy.TagConstString => {
            // Convert into heap string.
            const slice = left.asConstStr();
            const str = vm.strBuf[slice.start..slice.end];
            return vm.allocStringConcat(str, toTempString(vm, right)) catch stdx.fatal();
        },
        else => stdx.panic("unexpected tag"),
    }
}

inline fn evalAddNumber(left: cy.Value, right: cy.Value) linksection(".eval") cy.Value {
    @setRuntimeSafety(debug);
    return Value.initF64(left.asF64() + right.toF64());
}

fn evalNot(val: cy.Value) cy.Value {
    if (val.isNumber()) {
        return cy.Value.initFalse();
    } else {
        switch (val.getTag()) {
            cy.TagFalse => return cy.Value.initTrue(),
            cy.TagTrue => return cy.Value.initFalse(),
            cy.TagNone => return cy.Value.initTrue(),
            else => stdx.panic("unexpected tag"),
        }
    }
}

var tempU8Buf: [256]u8 = undefined;

/// Conversion goes into a temporary buffer. Must use the result before a subsequent call.
fn toTempString(vm: *const VM, val: Value) []const u8 {
    if (val.isNumber()) {
        if (Value.floatCanBeInteger(val.asF64())) {
            return std.fmt.bufPrint(&tempU8Buf, "{}", .{@floatToInt(u64, val.asF64())}) catch stdx.fatal();
        } else {
            return std.fmt.bufPrint(&tempU8Buf, "{d:.10}", .{val.asF64()}) catch stdx.fatal();
        }
    } else {
        switch (val.getTag()) {
            cy.TagFalse => return "false",
            cy.TagTrue => return "true",
            cy.TagNone => return "none",
            cy.TagConstString => {
                // Convert into heap string.
                const slice = val.asConstStr();
                return vm.strBuf[slice.start..slice.end];
            },
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

comptime {
    std.debug.assert(@sizeOf(MapInner) == 32);
}

const MapInner = cy.ValueMap;
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

comptime {
    std.debug.assert(@sizeOf(HeapObject) == 40);
    std.debug.assert(@sizeOf(HeapPage) == 40 * 1600);
}

const GenericObject = struct {
    structId: StructId,
    rc: u32,
};

const SymbolMapType = enum {
    oneStruct,
    // twoStructs,
    manyStructs,
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
};

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

const SymbolEntryType = enum {
    func,
    nativeFunc1,
    nativeFunc2,
};

const SymbolEntry = struct {
    entryT: SymbolEntryType,
    inner: packed union {
        nativeFunc1: std.meta.FnPtr(fn (*VM, *anyopaque, []const Value) Value),
        nativeFunc2: std.meta.FnPtr(fn (*VM, *anyopaque, []const Value) cy.ValuePair),
        func: packed struct {
            pc: u32,
        },
    },

    fn initNativeFunc1(func: std.meta.FnPtr(fn (*VM, *anyopaque, []const Value) Value)) SymbolEntry {
        return .{
            .entryT = .nativeFunc1,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    fn initNativeFunc2(func: std.meta.FnPtr(fn (*VM, *anyopaque, []const Value) cy.ValuePair)) SymbolEntry {
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
        nativeFunc1: std.meta.FnPtr(fn (*VM, []const Value) Value),
        func: packed struct {
            pc: usize,
            /// Includes function params, locals, and return info slot.
            numLocals: u32,
        },
    },

    fn initNativeFunc1(func: std.meta.FnPtr(fn (*VM, []const Value) Value)) FuncSymbolEntry {
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