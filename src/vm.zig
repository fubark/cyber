const std = @import("std");
const builtin = @import("builtin");
const aarch64 = builtin.cpu.arch == .aarch64;
const stdx = @import("stdx");
const t = stdx.testing;
const tcc = @import("tcc");

const fmt = @import("fmt.zig");
const v = fmt.v;
const cy = @import("cyber.zig");
const sema = @import("sema.zig");
const bindings = @import("builtins/bindings.zig");
const Value = cy.Value;
const debug = @import("debug.zig");
const TraceEnabled = @import("build_options").trace;
const HeapObject = cy.HeapObject;
const release = cy.arc.release;
const retain = cy.arc.retain;
const retainObject = cy.arc.retainObject;
const UserVM = cy.UserVM;

const log = stdx.log.scoped(.vm);

const UseGlobalVM = true;
pub const TrackGlobalRC = builtin.mode != .ReleaseFast;
const StdSection = cy.StdSection;

/// Temp buf for toString conversions when the len is known to be small.
var tempU8Buf: [256]u8 = undefined;
var tempU8BufIdx: u32 = undefined;
var tempU8Writer = SliceWriter{ .buf = &tempU8Buf, .idx = &tempU8BufIdx };

/// Going forward, references to gvm should be replaced with pointer access to allow multiple VMs.
/// Once all references are replaced, gvm can be removed and the default VM can be allocated from the heap.
pub var gvm: VM = undefined;

pub fn getUserVM() *UserVM {
    return @ptrCast(*UserVM, &gvm);
}

pub const VM = struct {
    alloc: std.mem.Allocator,
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

    /// Static string data.
    strBuf: []u8,

    /// Holds unique heap string interns (*Astring, *Ustring).
    /// Does not include static strings (those are interned at compile time).
    /// By default, small strings (at most 64 bytes) are interned.
    strInterns: std.StringHashMapUnmanaged(*HeapObject),

    /// Object heap pages.
    heapPages: cy.List(*cy.heap.HeapPage),
    heapFreeHead: ?*HeapObject,

    refCounts: if (TrackGlobalRC) usize else void,

    /// Symbol table used to lookup object methods.
    /// A `SymbolId` indexes into `methodSyms`. If the `mruStructId` matches it uses `mruSym`.
    /// Otherwise, the sym is looked up from the hashmap `methodTable`.
    methodSyms: cy.List(MethodSym),
    methodTable: std.AutoHashMapUnmanaged(ObjectSymKey, MethodSym),

    /// Maps a method signature to a symbol id in `methodSyms`.
    methodSymSigs: std.HashMapUnmanaged(RelFuncSigKey, SymbolId, KeyU64Context, 80),

    /// Regular function symbol table.
    funcSyms: cy.List(FuncSymbolEntry),
    funcSymSigs: std.HashMapUnmanaged(AbsFuncSigKey, SymbolId, KeyU96Context, 80),
    funcSymDetails: cy.List(FuncSymDetail),

    varSyms: cy.List(VarSym),
    varSymSigs: std.HashMapUnmanaged(AbsVarSigKey, SymbolId, KeyU64Context, 80),

    /// Struct fields symbol table.
    fieldSyms: cy.List(FieldSymbolMap),
    fieldTable: std.AutoHashMapUnmanaged(ObjectSymKey, u16),
    fieldSymSignatures: std.StringHashMapUnmanaged(SymbolId),

    /// Structs.
    structs: cy.List(Struct),
    structSignatures: std.HashMapUnmanaged(StructKey, TypeId, KeyU64Context, 80),
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

    u8Buf: cy.ListAligned(u8, 8),
    u8Buf2: cy.ListAligned(u8, 8),

    stackTrace: StackTrace,

    /// Since func syms can be reassigned, the ones that retain a tcc state will be tracked here.
    funcSymTccStates: std.AutoHashMapUnmanaged(SymbolId, Value),
    methodSymExtras: cy.List([]const u8),
    debugTable: []const cy.OpDebug,

    curFiber: *cy.Fiber,
    mainFiber: cy.Fiber,

    /// Local to be returned back to eval caller.
    /// 255 indicates no return value.
    endLocal: u8,

    panicType: debug.PanicType,
    panicPayload: debug.PanicPayload,

    trace: if (TraceEnabled) *TraceInfo else void,

    /// Object to pc of instruction that allocated it.
    objectTraceMap: if (builtin.mode == .Debug) std.AutoHashMapUnmanaged(*HeapObject, u32) else void,

    /// Whether this VM is already deinited. Used to skip the next deinit to avoid using undefined memory.
    deinited: bool,

    pub fn init(self: *VM, alloc: std.mem.Allocator) !void {
        self.* = .{
            .alloc = alloc,
            .compiler = undefined,
            .ops = undefined,
            .consts = undefined,
            .strBuf = undefined,
            .strInterns = .{},
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
            .funcSymSigs = .{},
            .funcSymDetails = .{},
            .varSyms = .{},
            .varSymSigs = .{},
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
            .u8Buf2 = .{},
            .stackTrace = .{},
            .debugTable = undefined,
            .refCounts = if (TrackGlobalRC) 0 else undefined,
            .panicType = .none,
            .panicPayload = Value.None.val,
            .mainFiber = undefined,
            .curFiber = undefined,
            .endLocal = undefined,
            .objectTraceMap = if (builtin.mode == .Debug) .{} else undefined,
            .deinited = false,
            .funcSymTccStates = .{},
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
        try self.methodSyms.ensureTotalCapacityPrecise(self.alloc, 255);

        try self.structs.ensureTotalCapacityPrecise(alloc, 170);
        try self.fieldSyms.ensureTotalCapacityPrecise(alloc, 170);

        // Initialize heap.
        self.heapFreeHead = try cy.heap.growHeapPages(self, 1);

        // Force linksection order. Using `try` makes this work.
        try @call(.never_inline, cy.forceSectionDeps, .{});

        // Core bindings.
        try @call(.never_inline, bindings.bindCore, .{self});
    }

    pub fn deinit(self: *VM) void {
        if (self.deinited) {
            return;
        }

        debug.freePanicPayload(self);

        // Deinit runtime related resources first, since they may depend on
        // compiled/debug resources.
        for (self.funcSyms.items()) |sym| {
            if (sym.entryT == @enumToInt(FuncSymbolEntryType.closure)) {
                cy.arc.releaseObject(self, @ptrCast(*HeapObject, sym.inner.closure));
            }
        }
        self.funcSyms.deinit(self.alloc);
        for (self.varSyms.items()) |vsym| {
            release(self, vsym.value);
        }
        self.varSyms.deinit(self.alloc);

        // Deinit compiler first since it depends on buffers from parser.
        self.compiler.deinit();
        self.alloc.free(self.stack);
        self.stack = &.{};

        self.methodSyms.deinit(self.alloc);
        self.methodSymExtras.deinit(self.alloc);
        self.methodSymSigs.deinit(self.alloc);
        self.methodTable.deinit(self.alloc);

        self.funcSymSigs.deinit(self.alloc);
        for (self.funcSymDetails.items()) |detail| {
            self.alloc.free(detail.name);
        }
        self.funcSymDetails.deinit(self.alloc);
        {
            var iter = self.funcSymTccStates.iterator();
            while (iter.next()) |e| {
                release(self, e.value_ptr.*);
            }
            self.funcSymTccStates.deinit(self.alloc);
        }

        self.varSymSigs.deinit(self.alloc);

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
        self.u8Buf2.deinit(self.alloc);
        self.stackTrace.deinit(self.alloc);

        self.strInterns.deinit(self.alloc);

        if (builtin.mode == .Debug) {
            self.objectTraceMap.deinit(self.alloc);
        }

        self.deinited = true;
    }

    pub fn compile(self: *VM, srcUri: []const u8, src: []const u8) !cy.ByteCodeBuffer {
        var tt = stdx.debug.trace();
        const res = try self.compiler.compile(srcUri, src, .{
            .genMainScopeReleaseOps = true,
        });
        if (res.err) |err| {
            const chunk = self.compiler.chunks.items[self.compiler.lastErrChunk];
            switch (err) {
                .tokenize => {
                    try debug.printUserError(self, "TokenError", chunk.parser.last_err, chunk.id, chunk.parser.last_err_pos, true);
                    return error.TokenError;
                },
                .parse => {
                    try debug.printUserError(self, "ParseError", chunk.parser.last_err, chunk.id, chunk.parser.last_err_pos, false);
                    return error.ParseError;
                },
                .compile => {
                    if (self.compiler.lastErrNode != cy.NullId) {
                        const token = chunk.nodes[self.compiler.lastErrNode].start_token;
                        const pos = chunk.tokens[token].pos();
                        try debug.printUserError(self, "CompileError", self.compiler.lastErr, chunk.id, pos, false);
                    } else {
                        try debug.printUserError(self, "CompileError", self.compiler.lastErr, chunk.id, cy.NullId, false);
                    }
                    return error.CompileError;
                },
            }
        }
        tt.endPrint("compile");
        return res.buf;
    }

    pub fn eval(self: *VM, srcUri: []const u8, src: []const u8, config: EvalConfig) !Value {
        var tt = stdx.debug.trace();
        const res = try self.compiler.compile(srcUri, src, .{
            .genMainScopeReleaseOps = !config.singleRun,
        });
        if (res.err) |err| {
            const chunk = self.compiler.chunks.items[self.compiler.lastErrChunk];
            switch (err) {
                .tokenize => {
                    try debug.printUserError(self, "TokenError", chunk.parser.last_err, chunk.id, chunk.parser.last_err_pos, true);
                    return error.TokenError;
                },
                .parse => {
                    try debug.printUserError(self, "ParseError", chunk.parser.last_err, chunk.id, chunk.parser.last_err_pos, false);
                    return error.ParseError;
                },
                .compile => {
                    if (self.compiler.lastErrNode != cy.NullId) {
                        const token = chunk.nodes[self.compiler.lastErrNode].start_token;
                        const pos = chunk.tokens[token].pos();
                        try debug.printUserError(self, "CompileError", self.compiler.lastErr, chunk.id, pos, false);
                    } else {
                        try debug.printUserError(self, "CompileError", self.compiler.lastErr, chunk.id, cy.NullId, false);
                    }
                    return error.CompileError;
                },
            }
        }
        tt.endPrint("compile");

        if (TraceEnabled) {
            if (!builtin.is_test and debug.atLeastTestDebugLevel()) {
                res.buf.dump();
            }
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
        } else {
            if (builtin.is_test and debug.atLeastTestDebugLevel()) {
                // Only visible for tests with .debug log level.
                res.buf.dump();
            }
        }

        tt = stdx.debug.trace();
        defer {
            tt.endPrint("eval");
            if (TraceEnabled) {
                if (!builtin.is_test or debug.atLeastTestDebugLevel()) {
                    self.dumpInfo();
                }
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
        fmt.printStderr("stack size: {}\n", &.{v(self.stack.len)});
        fmt.printStderr("stack framePtr: {}\n", &.{v(framePtrOffset(self.framePtr))});
        fmt.printStderr("heap pages: {}\n", &.{v(self.heapPages.len)});

        // Dump object symbols.
        {
            fmt.printStderr("obj syms:\n", &.{});
            var iter = self.funcSymSigs.iterator();
            while (iter.next()) |it| {
                const key = it.key_ptr.*;
                const name = sema.getName(&self.compiler, key.rtFuncSymKey.nameId);
                if (key.rtFuncSymKey.numParams == cy.NullId) {
                    fmt.printStderr("\t{}: {}\n", &.{v(name), v(it.value_ptr.*)});
                } else {
                    fmt.printStderr("\t{}({}): {}\n", &.{v(name), v(key.rtFuncSymKey.numParams), v(it.value_ptr.*)});
                }
            }
        }

        // Dump object fields.
        {
            fmt.printStderr("obj fields:\n", &.{});
            var iter = self.fieldSymSignatures.iterator();
            while (iter.next()) |it| {
                fmt.printStderr("\t{}: {}\n", &.{v(it.key_ptr.*), v(it.value_ptr.*)});
            }
        }
    }

    pub fn popStackFrameCold(self: *VM, comptime numRetVals: u2) linksection(cy.HotSection) void {
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

    fn popStackFrameLocal(self: *VM, pc: *usize, retLocal: u8, comptime numRetVals: u2) linksection(cy.HotSection) bool {
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
        debug.freePanicPayload(self);
        self.panicType = .none;
        self.debugTable = buf.debugTable.items;
    }

    pub fn evalByteCode(self: *VM, buf: cy.ByteCodeBuffer) !Value {
        if (buf.ops.items.len == 0) {
            return error.NoEndOp;
        }

        @call(.never_inline, self.prepareEvalCold, .{buf});

        // Set these last to hint location to cache before eval.
        self.pc = @ptrCast([*]cy.OpData, buf.ops.items.ptr);
        try self.stackEnsureTotalCapacity(buf.mainStackSize);
        self.framePtr = @ptrCast([*]Value, self.stack.ptr);

        self.ops = buf.ops.items;
        self.consts = buf.mconsts;
        self.strBuf = buf.strBuf.items;

        try @call(.never_inline, evalLoopGrowStack, .{self});
        if (TraceEnabled) {
            log.info("main stack size: {}", .{buf.mainStackSize});
        }

        if (self.endLocal == 255) {
            return Value.None;
        } else {
            return self.stack[self.endLocal];
        }
    }

    fn sliceOp(self: *VM, recv: *Value, startV: Value, endV: Value) !Value {
        if (recv.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer().?);
            switch (obj.retainedCommon.structId) {
                cy.ListS => {
                    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                    var start = @floatToInt(i32, startV.toF64());
                    if (start < 0) {
                        start = @intCast(i32, list.len) + start;
                    }
                    var end = if (endV.isNone()) @intCast(i32, list.len) else @floatToInt(i32, endV.toF64());
                    if (end < 0) {
                        end = @intCast(i32, list.len) + end;
                    }
                    if (start < 0 or start > list.len) {
                        return self.panic("Index out of bounds");
                    }
                    if (end < start or end > list.len) {
                        return self.panic("Index out of bounds");
                    }
                    return cy.heap.allocList(self, list.buf[@intCast(u32, start)..@intCast(u32, end)]);
                },
                cy.AstringT => {
                    retainObject(self, obj);
                    return bindings.stringSlice(.astring)(@ptrCast(*UserVM, self), obj, &[_]Value{startV, endV}, 2);
                },
                cy.UstringT => {
                    retainObject(self, obj);
                    return bindings.stringSlice(.ustring)(@ptrCast(*UserVM, self), obj, &[_]Value{startV, endV}, 2);
                },
                cy.StringSliceT => {
                    retainObject(self, obj);
                    return bindings.stringSlice(.slice)(@ptrCast(*UserVM, self), obj, &[_]Value{startV, endV}, 2);
                },
                cy.RawStringT => {
                    retainObject(self, obj);
                    return bindings.stringSlice(.rawstring)(@ptrCast(*UserVM, self), obj, &[_]Value{startV, endV}, 2);
                },
                cy.RawStringSliceT => {
                    retainObject(self, obj);
                    return bindings.stringSlice(.rawSlice)(@ptrCast(*UserVM, self), obj, &[_]Value{startV, endV}, 2);
                },
                else => {
                    return self.panicFmt("Unsupported slice operation on type `{}`.", &.{v(self.structs.buf[obj.retainedCommon.structId].name)});
                },
            }
        } else {
            if (recv.isNumber()) {
                return self.panic("Unsupported slice operation on type `number`.");
            } else {
                switch (recv.getTag()) {
                    cy.StaticAstringT => return bindings.stringSlice(.staticAstring)(@ptrCast(*UserVM, self), recv, &[_]Value{startV, endV}, 2),
                    cy.StaticUstringT => return bindings.stringSlice(.staticUstring)(@ptrCast(*UserVM, self), recv, &[_]Value{startV, endV}, 2),
                    else => {
                        return self.panicFmt("Unsupported slice operation on type `{}`.", &.{v(@intCast(u8, recv.getTag()))});
                    },
                }
            }
        }
    }

    pub fn ensureTagType(self: *VM, name: []const u8) !TagTypeId {
        const res = try self.tagTypeSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            return self.addTagType(name);
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn ensureStruct(self: *VM, nameId: sema.NameSymId, uniqId: u32) !TypeId {
        const res = try @call(.never_inline, self.structSignatures.getOrPut, .{self.alloc, .{
            .structKey = .{
                .nameId = nameId,
                .uniqId = uniqId,
            },
        }});
        if (!res.found_existing) {
            return self.addStructExt(nameId, uniqId);
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn getStructFieldIdx(self: *const VM, sid: TypeId, propName: []const u8) ?u32 {
        const fieldId = self.fieldSymSignatures.get(propName) orelse return null;
        const entry = &self.fieldSyms.buf[fieldId];

        if (entry.mruTypeId == sid) {
            return entry.mruOffset;
        } else {
            const offset = self.fieldTable.get(.{ .structId = sid, .symId = fieldId }) orelse return null;
            entry.mruTypeId = sid;
            entry.mruOffset = offset;
            return offset;
        }
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

    pub inline fn getStruct(self: *const VM, nameId: sema.NameSymId, uniqId: u32) ?TypeId {
        return self.structSignatures.get(.{
            .structKey = .{
                .nameId = nameId,
                .uniqId = uniqId,
            },
        });
    }

    pub fn addStructExt(self: *VM, nameId: sema.NameSymId, uniqId: u32) !TypeId {
        const name = sema.getName(&self.compiler, nameId);
        const s = Struct{
            .name = name,
            .numFields = 0,
        };
        const vm = self.getVM();
        const id = @intCast(u32, vm.structs.len);
        try vm.structs.append(vm.alloc, s);
        try vm.structSignatures.put(vm.alloc, .{
            .structKey = .{
                .nameId = nameId,
                .uniqId = uniqId,
            },
        }, id);
        return id;
    }

    pub fn addStruct(self: *VM, name: []const u8) !TypeId {
        const nameId = try sema.ensureNameSym(&self.compiler, name);
        return self.addStructExt(nameId, 0);
    }

    inline fn getVM(self: *VM) *VM {
        if (UseGlobalVM) {
            return &gvm;
        } else {
            return self;
        }
    }

    pub inline fn getFuncSym(self: *const VM, resolvedParentId: u32, nameId: u32, numParams: u32) ?SymbolId {
        const key = AbsFuncSigKey{
            .rtFuncSymKey = .{
                .resolvedParentSymId = resolvedParentId,
                .nameId = nameId,
                .numParams = numParams,
            },
        };
        return self.funcSymSigs.get(key);
    }

    pub inline fn getVarSym(self: *const VM, resolvedParentId: u32, nameId: u32) ?SymbolId {
        const key = AbsVarSigKey{
            .rtVarSymKey = .{
                .resolvedParentSymId = resolvedParentId,
                .nameId = nameId,
            }
        };
        return self.varSymSigs.get(key);
    }

    pub fn ensureVarSym(self: *VM, resolvedParentId: SymbolId, nameId: u32) !SymbolId {
        const key = KeyU64{
            .rtVarSymKey = .{
                .resolvedParentSymId = resolvedParentId,
                .nameId = nameId,
            },
        };
        const res = try self.varSymSigs.getOrPut(self.alloc, key);
        if (!res.found_existing) {
            const id = @intCast(u32, self.varSyms.len);
            try self.varSyms.append(self.alloc, VarSym.init(Value.None));
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }
    
    pub fn ensureFuncSym(self: *VM, resolvedParentId: SymbolId, nameId: u32, numParams: u32) !SymbolId {
        const key = KeyU96{
            .rtFuncSymKey = .{
                .resolvedParentSymId = resolvedParentId,
                .nameId = nameId,
                .numParams = numParams,
            },
        };
        const res = try self.funcSymSigs.getOrPut(self.alloc, key);
        if (!res.found_existing) {
            const id = @intCast(u32, self.funcSyms.len);
            try self.funcSyms.append(self.alloc, .{
                .entryT = @enumToInt(FuncSymbolEntryType.none),
                .innerExtra = .{
                    .none = .{
                        .numParams = numParams,
                    },
                },
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
                .mruTypeId = cy.NullId,
                .mruOffset = undefined,
                .name = name,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn hasMethodSym(self: *const VM, sid: TypeId, methodId: SymbolId) bool {
        const map = self.methodSyms.buf[methodId];
        if (map.mapT == .one) {
            return map.inner.one.id == sid;
        }
        return false;
    }

    pub fn ensureMethodSymKey(self: *VM, name: []const u8, numParams: u32) !SymbolId {
        const nameId = try sema.ensureNameSym(&self.compiler, name);
        const key = RelFuncSigKey{
            .relFuncSigKey = .{
                .nameId = nameId,
                .numParams = numParams,
            },
        };
        const res = try @call(.never_inline, self.methodSymSigs.getOrPut, .{self.alloc, key});
        if (!res.found_existing) {
            const id = @intCast(u32, self.methodSyms.len);
            try self.methodSyms.append(self.alloc, .{
                .entryT = undefined,
                .mruStructId = cy.NullId,
                .inner = undefined,
            });
            try self.methodSymExtras.append(self.alloc, name);
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn addFieldSym(self: *VM, sid: TypeId, symId: SymbolId, offset: u16) !void {
        const sym = &self.fieldSyms.buf[symId];
        if (sym.mruTypeId != cy.NullId) {
            // Add prev mru if it doesn't exist in hashmap.
            const prev = ObjectSymKey{
                .structId = sym.mruTypeId,
                .symId = symId,
            };
            if (!self.fieldTable.contains(prev)) {
                try self.fieldTable.putNoClobber(self.alloc, prev, sym.mruOffset);
            }
            const key = ObjectSymKey{
                .structId = sid,
                .symId = symId,
            };
            try self.fieldTable.putNoClobber(self.alloc, key, offset);
            sym.mruTypeId = sid;
            sym.mruOffset = offset;
        } else {
            sym.mruTypeId = sid;
            sym.mruOffset = offset;
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

    pub inline fn setVarSym(self: *VM, symId: SymbolId, sym: VarSym) void {
        self.varSyms.buf[symId] = sym;
    }

    pub inline fn setFuncSym(self: *VM, symId: SymbolId, sym: FuncSymbolEntry) void {
        self.funcSyms.buf[symId] = sym;
    }

    pub fn addMethodSym(self: *VM, id: TypeId, symId: SymbolId, entry: MethodSym) !void {
        const sym = &self.methodSyms.buf[symId];
        if (sym.mruStructId != cy.NullId) {
            const prev = ObjectSymKey{
                .structId = sym.mruStructId,
                .symId = symId,
            };
            if (!self.methodTable.contains(prev)) {
                try self.methodTable.putNoClobber(self.alloc, prev, sym.*);
            }
            const key = ObjectSymKey{
                .structId = id,
                .symId = symId,
            };
            try self.methodTable.putNoClobber(self.alloc, key, entry);
            sym.* = .{
                .entryT = entry.entryT,
                .mruStructId = id,
                .inner = entry.inner,
            };
        } else {
            sym.* = .{
                .entryT = entry.entryT,
                .mruStructId = id,
                .inner = entry.inner,
            };
        }
    }

    pub fn setIndexRelease(self: *VM, left: Value, index: Value, right: Value) !void {
        if (left.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                cy.ListS => {
                    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                    const idx = @floatToInt(u32, index.toF64());
                    if (idx < list.len) {
                        release(self, list.buf[idx]);
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
                cy.MapS => {
                    const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                    const res = try map.getOrPut(self.alloc, self, index);
                    if (res.foundExisting) {
                        release(self, res.valuePtr.*);
                    }
                    res.valuePtr.* = right;
                },
                else => {
                    return stdx.panic("unsupported struct");
                },
            }
        } else {
            return stdx.panic("expected pointer");
        }
    }

    pub fn setIndex(self: *VM, left: Value, index: Value, right: Value) !void {
        if (left.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                cy.ListS => {
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
                cy.MapS => {
                    const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                    try map.put(self.alloc, self, index, right);
                },
                else => {
                    log.debug("unsupported object: {}", .{obj.retainedCommon.structId});
                    stdx.fatal();
                },
            }
        } else {
            return stdx.panic("expected pointer");
        }
    }

    /// Assumes sign of index is preserved.
    fn getReverseIndex(self: *VM, left: *Value, index: Value) linksection(cy.Section) !Value {
        if (left.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                cy.ListS => {
                    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                    const idx = @intCast(i32, list.len) + @floatToInt(i32, index.toF64());
                    if (idx < list.len) {
                        const res = list.buf[@intCast(u32, idx)];
                        retain(self, res);
                        return res;
                    } else {
                        return error.OutOfBounds;
                    }
                },
                cy.MapS => {
                    const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                    const key = Value.initF64(index.toF64());
                    if (map.get(self, key)) |val| {
                        retain(self, val);
                        return val;
                    } else return Value.None;
                },
                cy.AstringT => {
                    const idx = @intToFloat(f64, @intCast(i32, obj.astring.len) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    return bindings.stringCharAt(.astring)(@ptrCast(*UserVM, self), obj, &[_]Value{Value.initF64(idx)}, 1);
                },
                cy.UstringT => {
                    const idx = @intToFloat(f64, @intCast(i32, obj.ustring.charLen) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    return bindings.stringCharAt(.ustring)(@ptrCast(*UserVM, self), obj, &[_]Value{Value.initF64(idx)}, 1);
                },
                cy.StringSliceT => {
                    if (obj.stringSlice.isAstring()) {
                        const idx = @intToFloat(f64, @intCast(i32, obj.stringSlice.len) + @floatToInt(i32, index.toF64()));
                        retainObject(self, obj);
                        return bindings.stringCharAt(.slice)(@ptrCast(*UserVM, self), obj, &[_]Value{Value.initF64(idx)}, 1);
                    } else {
                        const idx = @intToFloat(f64, @intCast(i32, obj.stringSlice.uCharLen) + @floatToInt(i32, index.toF64()));
                        retainObject(self, obj);
                        return bindings.stringCharAt(.slice)(@ptrCast(*UserVM, self), obj, &[_]Value{Value.initF64(idx)}, 1);
                    }
                },
                cy.RawStringT => {
                    const idx = @intToFloat(f64, @intCast(i32, obj.rawstring.len) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    return bindings.stringCharAt(.rawstring)(@ptrCast(*UserVM, self), obj, &[_]Value{Value.initF64(idx)}, 1);
                },
                cy.RawStringSliceT => {
                    const idx = @intToFloat(f64, @intCast(i32, obj.rawstringSlice.len) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    return bindings.stringCharAt(.rawSlice)(@ptrCast(*UserVM, self), obj, &[_]Value{Value.initF64(idx)}, 1);
                },
                else => {
                    return self.panicFmt("Unsupported reverse index operation on type `{}`.", &.{v(self.structs.buf[obj.common.structId].name)});
                },
            }
        } else {
            if (left.isNumber()) {
                return self.panic("Unsupported reverse index operation on type `number`.");
            } else {
                switch (left.getTag()) {
                    cy.StaticAstringT => {
                        const idx = @intToFloat(f64, @intCast(i32, left.asStaticStringSlice().len()) + @floatToInt(i32, index.toF64()));
                        return bindings.stringCharAt(.staticAstring)(@ptrCast(*UserVM, self), left, &[_]Value{Value.initF64(idx)}, 1);
                    },
                    cy.StaticUstringT => {
                        const start = left.asStaticStringSlice().start;
                        const idx = @intToFloat(f64, @intCast(i32, cy.string.getStaticUstringHeader(self, start).charLen) + @floatToInt(i32, index.toF64()));
                        return bindings.stringCharAt(.staticUstring)(@ptrCast(*UserVM, self), left, &[_]Value{Value.initF64(idx)}, 1);
                    },
                    else => {
                        return self.panicFmt("Unsupported reverse index operation on type `{}`.", &.{v(@intCast(u8, left.getTag()))});
                    },
                }
            }
        }
    }

    pub fn getIndex(self: *VM, left: *Value, index: Value) linksection(cy.Section) !Value {
        if (left.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, left.asPointer().?);
            switch (obj.retainedCommon.structId) {
                cy.ListS => {
                    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                    const idx = @floatToInt(u32, index.toF64());
                    if (idx < list.len) {
                        retain(self, list.buf[idx]);
                        return list.buf[idx];
                    } else {
                        return error.OutOfBounds;
                    }
                },
                cy.MapS => {
                    const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                    if (@call(.never_inline, map.get, .{self, index})) |val| {
                        retain(self, val);
                        return val;
                    } else return Value.None;
                },
                cy.AstringT => {
                    retainObject(self, obj);
                    return bindings.stringCharAt(.astring)(@ptrCast(*UserVM, self), obj, &[_]Value{index}, 1);
                },
                cy.UstringT => {
                    retainObject(self, obj);
                    return bindings.stringCharAt(.ustring)(@ptrCast(*UserVM, self), obj, &[_]Value{index}, 1);
                },
                cy.StringSliceT => {
                    retainObject(self, obj);
                    return bindings.stringCharAt(.slice)(@ptrCast(*UserVM, self), obj, &[_]Value{index}, 1);
                },
                cy.RawStringT => {
                    retainObject(self, obj);
                    return bindings.stringCharAt(.rawstring)(@ptrCast(*UserVM, self), obj, &[_]Value{index}, 1);
                },
                cy.RawStringSliceT => {
                    retainObject(self, obj);
                    return bindings.stringCharAt(.rawSlice)(@ptrCast(*UserVM, self), obj, &[_]Value{index}, 1);
                },
                else => {
                    return self.panicFmt("Unsupported index operation on type `{}`.", &.{v(self.structs.buf[obj.common.structId].name)});
                },
            }
        } else {
            if (left.isNumber()) {
                return self.panic("Unsupported index operation on type `number`.");
            } else {
                switch (left.getTag()) {
                    cy.StaticAstringT => return bindings.stringCharAt(.staticAstring)(@ptrCast(*UserVM, self), left, &[_]Value{index}, 1),
                    cy.StaticUstringT => return bindings.stringCharAt(.staticUstring)(@ptrCast(*UserVM, self), left, &[_]Value{index}, 1),
                    else => {
                        return self.panicFmt("Unsupported index operation on type `{}`.", &.{v(@intCast(u8, left.getTag()))});
                    },
                }
            }
        }
    }

    fn panicFmt(self: *VM, format: []const u8, args: []const fmt.FmtValue) error{Panic, OutOfMemory} {
        @setCold(true);
        const msg = fmt.allocFormat(self.alloc, format, args) catch |err| {
            if (err == error.OutOfMemory) {
                return error.OutOfMemory;
            } else {
                stdx.panic("unexpected");
            }
        };
        self.panicPayload = @intCast(u64, @ptrToInt(msg.ptr)) | (@as(u64, msg.len) << 48);
        self.panicType = .msg;
        log.debug("{s}", .{msg});
        return error.Panic;
    }

    fn panic(self: *VM, comptime msg: []const u8) error{Panic, OutOfMemory} {
        @setCold(true);
        const dupe = try self.alloc.dupe(u8, msg);
        self.panicPayload = @intCast(u64, @ptrToInt(dupe.ptr)) | (@as(u64, dupe.len) << 48);
        self.panicType = .msg;
        log.debug("{s}", .{dupe});
        return error.Panic;
    }

    fn setField(self: *VM, recv: Value, fieldId: SymbolId, val: Value) linksection(cy.HotSection) !void {
        if (recv.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
            const symMap = &self.fieldSyms.buf[fieldId];

            if (obj.common.structId == symMap.mruTypeId) {
                obj.object.getValuePtr(symMap.mruOffset).* = val;
            } else {
                const offset = self.getFieldOffset(obj, fieldId);
                if (offset != cy.NullU8) {
                    symMap.mruTypeId = obj.common.structId;
                    symMap.mruOffset = offset;
                    obj.object.getValuePtr(offset).* = val;
                } else {
                    return self.getFieldMissingSymbolError();
                }
            }
        } else {
            return self.setFieldNotObjectError();
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

    fn getFieldOffsetFromTable(self: *VM, sid: TypeId, symId: SymbolId) u8 {
        if (self.fieldTable.get(.{ .structId = sid, .symId = symId })) |offset| {
            const sym = &self.fieldSyms.buf[symId];
            sym.mruTypeId = sid;
            sym.mruOffset = offset;
            return @intCast(u8, offset);
        } else {
            return cy.NullU8;
        }
    }

    pub fn getFieldOffset(self: *VM, obj: *HeapObject, symId: SymbolId) linksection(cy.HotSection) u8 {
        const symMap = self.fieldSyms.buf[symId];
        if (obj.common.structId == symMap.mruTypeId) {
            return @intCast(u8, symMap.mruOffset);
        } else {
            return @call(.never_inline, self.getFieldOffsetFromTable, .{obj.common.structId, symId});
        }
    }

    pub fn setFieldRelease(self: *VM, recv: Value, symId: SymbolId, val: Value) linksection(cy.HotSection) !void {
        @setCold(true);
        if (recv.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer().?);
            const offset = self.getFieldOffset(obj, symId);
            if (offset != cy.NullU8) {
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

    pub fn getField2(self: *VM, recv: Value, symId: SymbolId) linksection(cy.Section) !Value {
        if (recv.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer().?);
            const offset = self.getFieldOffset(obj, symId);
            if (offset != cy.NullU8) {
                return obj.object.getValue(offset);
            } else {
                return self.getFieldFallback(obj, self.fieldSyms.buf[symId].name);
            }
        } else {
            return self.getFieldMissingSymbolError();
        }
    }

    pub fn getField(self: *VM, recv: Value, symId: SymbolId) linksection(cy.HotSection) !Value {
        if (recv.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer().?);
            const offset = self.getFieldOffset(obj, symId);
            if (offset != cy.NullU8) {
                return obj.object.getValue(offset);
            } else {
                return self.getFieldFallback(obj, self.fieldSyms.buf[symId].name);
            }
        } else {
            return self.getFieldMissingSymbolError();
        }
    }

    fn getFieldFallback(self: *const VM, obj: *const HeapObject, name: []const u8) linksection(cy.HotSection) Value {
        @setCold(true);
        if (obj.common.structId == cy.MapS) {
            const map = stdx.ptrAlignCast(*const cy.MapInner, &obj.map.inner);
            if (map.getByString(self, name)) |val| {
                return val;
            } else return Value.None;
        } else {
            log.debug("Missing symbol for object: {}", .{obj.common.structId});
            return Value.None;
        }
    }

    /// startLocal points to the first arg in the current stack frame.
    fn callSym(self: *VM, pc: [*]cy.OpData, framePtr: [*]Value, symId: SymbolId, startLocal: u8, numArgs: u8, reqNumRetVals: u2) linksection(cy.HotSection) !PcFramePtr {
        const sym = self.funcSyms.buf[symId];
        switch (@intToEnum(FuncSymbolEntryType, sym.entryT)) {
            .nativeFunc1 => {
                const newFramePtr = framePtr + startLocal;

                // Optimize.
                pc[0] = cy.OpData{ .code = .callNativeFuncIC };
                @ptrCast(*align(1) u48, pc + 5).* = @intCast(u48, @ptrToInt(sym.inner.nativeFunc1));

                gvm.framePtr = newFramePtr;
                const res = sym.inner.nativeFunc1(@ptrCast(*UserVM, self), @ptrCast([*]const Value, newFramePtr + 4), numArgs);
                if (res.isPanic()) {
                    return error.Panic;
                }
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
                @ptrCast(*align(1) u48, pc + 5).* = @intCast(u48, @ptrToInt(self.toPc(sym.inner.func.pc)));

                const newFramePtr = framePtr + startLocal;
                newFramePtr[1] = buildReturnInfo(reqNumRetVals, true);
                newFramePtr[2] = Value{ .retPcPtr = pc + 11 };
                newFramePtr[3] = Value{ .retFramePtr = framePtr };
                return PcFramePtr{
                    .pc = self.toPc(sym.inner.func.pc),
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
                const src = sym.inner.closure.getCapturedValuesPtr()[0..sym.inner.closure.numCaptured];
                std.mem.copy(Value, newFramePtr[numArgs + 4 + 1..numArgs + 4 + 1 + sym.inner.closure.numCaptured], src);

                return PcFramePtr{
                    .pc = self.toPc(sym.inner.closure.funcPc),
                    .framePtr = newFramePtr,
                };
            },
            .none => {
                return self.panic("Symbol is not defined.");
            },
            // else => {
            //     return self.panic("unsupported callsym");
            // },
        }
    }

    fn callSymEntry(self: *VM, pc: [*]cy.OpData, framePtr: [*]Value, sym: MethodSym, obj: *HeapObject, typeId: u32, startLocal: u8, numArgs: u8, reqNumRetVals: u8) linksection(cy.HotSection) !PcFramePtr {
        switch (sym.entryT) {
            .func => {
                if (@ptrToInt(framePtr + startLocal + sym.inner.func.numLocals) >= @ptrToInt(gvm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                // Optimize.
                pc[0] = cy.OpData{ .code = .callObjFuncIC };
                pc[5] = cy.OpData{ .arg = @intCast(u8, sym.inner.func.numLocals) };
                @ptrCast(*align(1) u48, pc + 6).* = @intCast(u48, @ptrToInt(self.toPc(sym.inner.func.pc)));
                @ptrCast(*align(1) u16, pc + 12).* = @intCast(u16, typeId);
                
                const newFramePtr = framePtr + startLocal;
                newFramePtr[1] = buildReturnInfo2(reqNumRetVals, true);
                newFramePtr[2] = Value{ .retPcPtr = pc + 14 };
                newFramePtr[3] = Value{ .retFramePtr = framePtr };
                return PcFramePtr{
                    .pc = self.toPc(sym.inner.func.pc),
                    .framePtr = newFramePtr,
                };
            },
            .nativeFunc1 => {
                // Optimize.
                pc[0] = cy.OpData{ .code = .callObjNativeFuncIC };
                @ptrCast(*align(1) u48, pc + 6).* = @intCast(u48, @ptrToInt(sym.inner.nativeFunc1));
                @ptrCast(*align(1) u16, pc + 12).* = @intCast(u16, typeId);

                self.framePtr = framePtr;
                const res = sym.inner.nativeFunc1(@ptrCast(*UserVM, self), obj, @ptrCast([*]const Value, framePtr + startLocal + 4), numArgs);
                if (res.isPanic()) {
                    return error.Panic;
                }
                if (reqNumRetVals == 1) {
                    framePtr[startLocal] = res;
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
                self.framePtr = framePtr;
                const res = sym.inner.nativeFunc2(@ptrCast(*UserVM, self), obj, @ptrCast([*]const Value, framePtr + startLocal + 4), numArgs);
                if (res.left.isPanic()) {
                    return error.Panic;
                }
                if (reqNumRetVals == 2) {
                    framePtr[startLocal] = res.left;
                    framePtr[startLocal+1] = res.right;
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            release(self, res.left);
                            release(self, res.right);
                        },
                        1 => {
                            framePtr[startLocal] = res.left;
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

    fn getCallObjSymFromTable(self: *VM, sid: TypeId, symId: SymbolId) ?MethodSym {
        if (self.methodTable.get(.{ .structId = sid, .symId = symId })) |entry| {
            const sym = &self.methodSyms.buf[symId];
            sym.* = .{
                .entryT = entry.entryT,
                .mruStructId = sid,
                .inner = entry.inner,
            };
            return entry;
        } else {
            return null;
        }
    }

    fn getCallObjSym(self: *VM, typeId: u32, symId: SymbolId) linksection(cy.HotSection) ?MethodSym {
        const entry = self.methodSyms.buf[symId];
        if (entry.mruStructId == typeId) {
            return entry;
        } else {
            return @call(.never_inline, self.getCallObjSymFromTable, .{typeId, symId});
        }
    }

    /// Stack layout: arg0, arg1, ..., receiver
    /// numArgs includes the receiver.
    /// Return new pc to avoid deoptimization.
    fn callObjSym(self: *VM, pc: [*]const cy.OpData, framePtr: [*]Value, recv: Value, symId: SymbolId, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2) linksection(cy.HotSection) !PcFramePtr {
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
                    if (map.inner.many.mruTypeId == obj.retainedCommon.structId) {
                        return try @call(.never_inline, callSymEntryNoInline, .{pc, framePtr, map.inner.many.mruSym, obj, startLocal, numArgs, reqNumRetVals});
                    } else {
                        const sym = self.methodTable.get(.{ .structId = obj.retainedCommon.structId, .methodId = symId }) orelse {
                            log.debug("Symbol does not exist for receiver.", .{});
                            stdx.fatal();
                        };
                        self.methodSyms.buf[symId].inner.many = .{
                            .mruTypeId = obj.retainedCommon.structId,
                            .mruSym = sym,
                        };
                        return try @call(.never_inline, callSymEntryNoInline, .{pc, framePtr, sym, obj, startLocal, numArgs, reqNumRetVals});
                    }
                },
                .empty => {
                    return try @call(.never_inline, callObjSymFallback, .{self, pc, framePtr, obj, symId, startLocal, numArgs, reqNumRetVals});
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

    pub fn buildStackTrace(self: *VM, fromPanic: bool) !void {
        @setCold(true);
        self.stackTrace.deinit(self.alloc);
        var frames: std.ArrayListUnmanaged(StackFrame) = .{};

        var framePtr = framePtrOffset(self.framePtr);
        var pc = pcOffset(self, self.pc);
        var isTopFrame = true;
        while (true) {
            const idx = b: {
                if (isTopFrame) {
                    isTopFrame = false;
                    if (fromPanic) {
                        const len = cy.getInstLenAt(self.ops.ptr + pc);
                        break :b debug.indexOfDebugSym(self, pc + len) orelse return error.NoDebugSym;
                    }
                }
                break :b debug.indexOfDebugSym(self, pc) orelse return error.NoDebugSym;
            };
            const sym = self.debugTable[idx];

            if (sym.frameLoc == cy.NullId) {
                const chunk = self.compiler.chunks.items[sym.file];
                const node = chunk.nodes[sym.loc];
                var line: u32 = undefined;
                var col: u32 = undefined;
                var lineStart: u32 = undefined;
                const pos = chunk.tokens[node.start_token].pos();
                debug.computeLinePosWithTokens(chunk.tokens, chunk.src, pos, &line, &col, &lineStart);
                try frames.append(self.alloc, .{
                    .name = "main",
                    .chunkId = sym.file,
                    .line = line,
                    .col = col,
                    .lineStartPos = lineStart,
                });
                break;
            } else {
                const chunk = self.compiler.chunks.items[sym.file];
                const frameNode = chunk.nodes[sym.frameLoc];
                const func = chunk.funcDecls[frameNode.head.func.decl_id];
                const name = chunk.src[func.name.start..func.name.end];

                const node = chunk.nodes[sym.loc];
                var line: u32 = undefined;
                var col: u32 = undefined;
                var lineStart: u32 = undefined;
                const pos = chunk.tokens[node.start_token].pos();
                debug.computeLinePosWithTokens(chunk.tokens, chunk.src, pos, &line, &col, &lineStart);
                try frames.append(self.alloc, .{
                    .name = name,
                    .chunkId = sym.file,
                    .line = line,
                    .col = col,
                    .lineStartPos = lineStart,
                });
                pc = pcOffset(self, self.stack[framePtr + 2].retPcPtr);
                framePtr = framePtrOffset(self.stack[framePtr + 3].retFramePtr);
            }
        }

        self.stackTrace.frames = try frames.toOwnedSlice(self.alloc);
    }

    pub fn valueAsStaticString(self: *const VM, val: Value) linksection(cy.HotSection) []const u8 {
        const slice = val.asStaticStringSlice();
        return self.strBuf[slice.start..slice.end];
    }

    /// A comparable string can be any string or a rawstring.
    pub fn tryValueAsComparableString(self: *const VM, val: Value) linksection(cy.Section) ?[]const u8 {
        if (val.isPointer()) {
            const obj = val.asHeapObject(*HeapObject);
            if (obj.common.structId == cy.AstringT) {
                return obj.astring.getConstSlice();
            } else if (obj.common.structId == cy.UstringT) {
                return obj.ustring.getConstSlice();
            } else if (obj.common.structId == cy.StringSliceT) {
                return obj.stringSlice.getConstSlice();
            } else if (obj.common.structId == cy.RawStringT) {
                return obj.rawstring.getConstSlice();
            } else if (obj.common.structId == cy.RawStringSliceT) {
                return obj.rawstringSlice.getConstSlice();
            } else return null;
        } else {
            if (val.assumeNotPtrIsStaticString()) {
                const slice = val.asStaticStringSlice();
                return self.strBuf[slice.start..slice.end];
            } else return null;
        }
        return null;
    }

    fn valueAsStringType(self: *const VM, val: Value, strT: StringType) linksection(cy.Section) []const u8 {
        switch (strT) {
            .staticAstring,
            .staticUstring => {
                const slice = val.asStaticStringSlice();
                return self.strBuf[slice.start..slice.end];
            },
            .astring => {
                const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
                return obj.astring.getConstSlice();
            },
            .ustring => {
                const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
                return obj.ustring.getConstSlice();
            },
            .slice => {
                const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
                return obj.stringSlice.getConstSlice();
            },
            .rawstring => {
                const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
                return obj.rawstring.getConstSlice();
            },
            .rawSlice => {
                const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
                return obj.rawstringSlice.getConstSlice();
            },
        }
    }

    pub fn valueAsString(self: *const VM, val: Value) linksection(cy.Section) []const u8 {
        if (val.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
            if (obj.common.structId == cy.AstringT) {
                return obj.astring.getConstSlice();
            } else if (obj.common.structId == cy.UstringT) {
                return obj.ustring.getConstSlice();
            } else if (obj.common.structId == cy.StringSliceT) {
                return obj.stringSlice.getConstSlice();
            } else unreachable;
        } else {
            // Assume const string.
            const slice = val.asStaticStringSlice();
            return self.strBuf[slice.start..slice.end];
        }
    }

    pub fn valueToString(self: *const VM, val: Value) ![]const u8 {
        const str = self.valueToTempString(val);
        return try self.alloc.dupe(u8, str);
    }

    pub fn valueToTempString(self: *const VM, val: Value) linksection(cy.Section) []const u8 {
        tempU8Writer.reset();
        return self.getOrWriteValueString(tempU8Writer, val, undefined, false);
    }

    pub fn valueToTempString2(self: *const VM, val: Value, outCharLen: *u32) linksection(cy.Section) []const u8 {
        tempU8Writer.reset();
        return self.getOrWriteValueString(tempU8Writer, val, outCharLen, true);
    }

    pub fn valueToNextTempString(self: *const VM, val: Value) linksection(cy.Section) []const u8 {
        return self.getOrWriteValueString(tempU8Writer, val, undefined, false);
    }

    pub fn valueToNextTempString2(self: *const VM, val: Value, outCharLen: *u32) linksection(cy.Section) []const u8 {
        return self.getOrWriteValueString(tempU8Writer, val, outCharLen, true);
    }

    /// Conversion goes into a temporary buffer. Must use the result before a subsequent call.
    pub fn getOrWriteValueString(self: *const VM, writer: anytype, val: Value, outCharLen: *u32, comptime getCharLen: bool) linksection(cy.Section) []const u8 {
        if (val.isNumber()) {
            const f = val.asF64();
            const start = writer.pos();
            if (Value.floatIsSpecial(f)) {
                std.fmt.format(writer, "{}", .{f}) catch stdx.fatal();
            } else {
                if (Value.floatCanBeInteger(f)) {
                    std.fmt.format(writer, "{d:.0}", .{f}) catch stdx.fatal();
                } else {
                    std.fmt.format(writer, "{d}", .{f}) catch stdx.fatal();
                }
            }
            const slice = writer.sliceFrom(start);
            if (getCharLen) {
                outCharLen.* = @intCast(u32, slice.len);
            }
            return slice;
        } else {
            if (val.isPointer()) {
                const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
                if (obj.common.structId == cy.AstringT) {
                    const res = obj.astring.getConstSlice();
                    if (getCharLen) {
                        outCharLen.* = @intCast(u32, res.len);
                    }
                    return res;
                } else if (obj.common.structId == cy.UstringT) {
                    if (getCharLen) {
                        outCharLen.* = obj.ustring.charLen;
                    }
                    return obj.ustring.getConstSlice();
                } else if (obj.common.structId == cy.StringSliceT) {
                    if (getCharLen) {
                        if (obj.stringSlice.isAstring()) {
                            outCharLen.* = obj.stringSlice.len;
                        } else {
                            outCharLen.* = obj.stringSlice.uCharLen;
                        }
                    }
                    return obj.stringSlice.getConstSlice();
                } else if (obj.common.structId == cy.RawStringT) {
                    const start = writer.pos();
                    std.fmt.format(writer, "rawstring ({})", .{obj.rawstring.len}) catch stdx.fatal();
                    const slice = writer.sliceFrom(start);
                    if (getCharLen) {
                        outCharLen.* = @intCast(u32, slice.len);
                    }
                    return slice;
                } else if (obj.common.structId == cy.RawStringSliceT) {
                    const start = writer.pos();
                    std.fmt.format(writer, "rawstring ({})", .{obj.rawstringSlice.len}) catch stdx.fatal();
                    const slice = writer.sliceFrom(start);
                    if (getCharLen) {
                        outCharLen.* = @intCast(u32, slice.len);
                    }
                    return slice;
                } else if (obj.common.structId == cy.ListS) {
                    const start = writer.pos();
                    std.fmt.format(writer, "List ({})", .{obj.list.list.len}) catch stdx.fatal();
                    const slice = writer.sliceFrom(start);
                    if (getCharLen) {
                        outCharLen.* = @intCast(u32, slice.len);
                    }
                    return slice;
                } else if (obj.common.structId == cy.MapS) {
                    const start = writer.pos();
                    std.fmt.format(writer, "Map ({})", .{obj.map.inner.size}) catch stdx.fatal();
                    const slice = writer.sliceFrom(start);
                    if (getCharLen) {
                        outCharLen.* = @intCast(u32, slice.len);
                    }
                    return slice;
                } else if (obj.common.structId == cy.SymbolT) {
                    const start = writer.pos();
                    const symType = @intToEnum(cy.heap.SymbolType, obj.symbol.symId);
                    var slice: []const u8 = undefined;
                    if (symType == .object) {
                        const name = self.structs.buf[obj.symbol.symId].name;
                        std.fmt.format(writer, "Object Symbol ({s})", .{name}) catch stdx.fatal();
                        slice = writer.sliceFrom(start);
                    } else {
                        slice = "Unknown Symbol";
                    }
                    if (getCharLen) {
                        outCharLen.* = @intCast(u32, slice.len);
                    }
                    return slice;
                } else {
                    const buf = self.structs.buf[obj.common.structId].name;
                    if (getCharLen) {
                        outCharLen.* = @intCast(u32, buf.len);
                    }
                    return buf;
                }
            } else {
                switch (val.getTag()) {
                    cy.NoneT => {
                        if (getCharLen) {
                            outCharLen.* = "none".len;
                        }
                        return "none";
                    },
                    cy.BooleanT => {
                        if (val.asBool()) {
                            if (getCharLen) {
                                outCharLen.* = "true".len;
                            }
                            return "true";
                        } else {
                            if (getCharLen) {
                                outCharLen.* = "false".len;
                            }
                            return "false";
                        }
                    },
                    cy.ErrorT => {
                        const start = writer.pos();
                        const litId = val.asErrorTagLit();
                        std.fmt.format(writer, "error#{s}", .{self.getTagLitName(litId)}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    cy.StaticAstringT => {
                        const slice = val.asStaticStringSlice();
                        const buf = self.strBuf[slice.start..slice.end];
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, buf.len);
                        }
                        return buf;
                    },
                    cy.StaticUstringT => {
                        const slice = val.asStaticStringSlice();
                        if (getCharLen) {
                            outCharLen.* = @ptrCast(*align (1) cy.StaticUstringHeader, self.strBuf.ptr + slice.start - 12).charLen;
                        }
                        return self.strBuf[slice.start..slice.end];
                    },
                    cy.UserTagLiteralT => {
                        const start = writer.pos();
                        const litId = val.asTagLiteralId();
                        std.fmt.format(writer, "#{s}", .{self.getTagLitName(litId)}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    cy.IntegerT => {
                        const start = writer.pos();
                        std.fmt.format(writer, "{}", .{val.asI32()}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    else => {
                        log.debug("unexpected tag {}", .{val.getTag()});
                        stdx.fatal();
                    },
                }
            }
        }
    }

    pub fn writeValueToString(self: *const VM, writer: anytype, val: Value) void {
        const str = self.valueToTempString(val);
        _ = writer.write(str) catch stdx.fatal();
    }

    pub inline fn stackEnsureUnusedCapacity(self: *VM, unused: u32) linksection(cy.HotSection) !void {
        if (@ptrToInt(self.framePtr) + 8 * unused >= @ptrToInt(self.stack.ptr + self.stack.len)) {
            try self.stackGrowTotalCapacity((@ptrToInt(self.framePtr) + 8 * unused) / 8);
        }
    }

    inline fn stackEnsureTotalCapacity(self: *VM, newCap: usize) linksection(cy.HotSection) !void {
        if (newCap > self.stack.len) {
            try self.stackGrowTotalCapacity(newCap);
        }
    }

    pub fn stackEnsureTotalCapacityPrecise(self: *VM, newCap: usize) !void {
        if (newCap > self.stack.len) {
            try self.stackGrowTotalCapacityPrecise(newCap);
        }
    }

    pub fn stackGrowTotalCapacity(self: *VM, newCap: usize) !void {
        var betterCap = self.stack.len;
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

    pub inline fn toPc(self: *const VM, offset: usize) [*]cy.OpData {
        return @ptrCast([*]cy.OpData, self.ops.ptr + offset);
    }

    // Performs stackGrowTotalCapacityPrecise in addition to patching the frame pointers.
    fn growStackAuto(vm: *VM) !void {
        @setCold(true);
        // Grow by 50% with minimum of 16.
        var growSize = vm.stack.len / 2;
        if (growSize < 16) {
            growSize = 16;
        }
        try growStackPrecise(vm, vm.stack.len + growSize);
    }

    pub fn ensureTotalStackCapacity(vm: *VM, newCap: usize) !void {
        if (newCap > vm.stack.len) {
            var betterCap = vm.stack.len;
            while (true) {
                betterCap +|= betterCap / 2 + 8;
                if (betterCap >= newCap) {
                    break;
                }
            }
            try growStackPrecise(vm, betterCap);
        }
    }

    fn growStackPrecise(vm: *VM, newCap: usize) !void {
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
};

fn evalBitwiseOr(left: Value, right: Value) linksection(cy.HotSection) Value {
    @setCold(true);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asF64toI32() | @floatToInt(i32, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalBitwiseXor(left: Value, right: Value) linksection(cy.HotSection) Value {
    @setCold(true);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asF64toI32() ^ @floatToInt(i32, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalBitwiseAnd(left: Value, right: Value) linksection(cy.HotSection) Value {
    @setCold(true);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asF64toI32() & @floatToInt(i32, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalBitwiseLeftShift(left: Value, right: Value) linksection(cy.HotSection) Value {
    @setCold(true);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asF64toI32() << @floatToInt(u5, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalBitwiseRightShift(left: Value, right: Value) linksection(cy.HotSection) Value {
    @setCold(true);
    if (left.isNumber()) {
       const f = @intToFloat(f64, left.asF64toI32() >> @floatToInt(u5, right.toF64()));
       return Value.initF64(f);
    } else {
        log.debug("unsupported", .{});
        unreachable;
    }
}

fn evalBitwiseNot(val: Value) linksection(cy.HotSection) Value {
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

fn evalLessFallback(left: cy.Value, right: cy.Value) linksection(cy.HotSection) cy.Value {
    @setCold(true);
    return Value.initBool(left.toF64() < right.toF64());
}

pub const StringType = enum {
    staticAstring,
    staticUstring,
    astring,
    ustring,
    slice,
    rawstring,
    rawSlice,
};

fn getComparableStringType(val: Value) ?StringType {
    if (val.isPointer()) {
        const obj = stdx.ptrAlignCast(*HeapObject, val.asPointer().?);
        if (obj.common.structId == cy.AstringT) {
            return .astring;
        } else if (obj.common.structId == cy.UstringT) {
            return .ustring;
        } else if (obj.common.structId == cy.StringSliceT) {
            return .slice;
        } else if (obj.common.structId == cy.RawStringT) {
            return .rawstring;
        } else if (obj.common.structId == cy.RawStringSliceT) {
            return .rawSlice;
        }
        return null;
    } else {
        if (val.isNumber()) {
            return null;
        }
        switch (val.getTag()) {
            cy.StaticUstringT => {
                return .staticUstring;
            },
            cy.StaticAstringT => {
                return .staticAstring;
            },
            else => return null,
        }
    }
}

fn evalCompareNot(vm: *const VM, left: cy.Value, right: cy.Value) linksection(cy.HotSection) cy.Value {
    if (getComparableStringType(left)) |lstrT| {
        if (getComparableStringType(right)) |rstrT| {
            const lstr = vm.valueAsStringType(left, lstrT);
            const rstr = vm.valueAsStringType(right, rstrT);
            return Value.initBool(!std.mem.eql(u8, lstr, rstr));
        }
    }
    return Value.True;
}

fn evalCompareBool(vm: *const VM, left: Value, right: Value) linksection(cy.HotSection) bool {
    if (getComparableStringType(left)) |lstrT| {
        if (getComparableStringType(right)) |rstrT| {
            const lstr = vm.valueAsStringType(left, lstrT);
            const rstr = vm.valueAsStringType(right, rstrT);
            return std.mem.eql(u8, lstr, rstr);
        }
    }
    return false;
}

fn evalCompare(vm: *const VM, left: Value, right: Value) linksection(cy.HotSection) Value {
    if (getComparableStringType(left)) |lstrT| {
        if (getComparableStringType(right)) |rstrT| {
            const lstr = vm.valueAsStringType(left, lstrT);
            const rstr = vm.valueAsStringType(right, rstrT);
            return Value.initBool(std.mem.eql(u8, lstr, rstr));
        }
    }
    return Value.False;
}

fn evalMinusFallback(left: Value, right: Value) linksection(cy.HotSection) Value {
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

fn evalAddFallback(left: cy.Value, right: cy.Value) linksection(cy.HotSection) !cy.Value {
    @setCold(true);
    return Value.initF64(try toF64OrPanic(left) + try toF64OrPanic(right));
}

fn toF64OrPanic(val: Value) linksection(cy.HotSection) !f64 {
    if (val.isNumber()) {
        return val.asF64();
    } else {
        return try @call(.never_inline, convToF64OrPanic, .{val});
    }
}

fn convToF64OrPanic(val: Value) linksection(cy.HotSection) !f64 {
    if (val.isPointer()) {
        const obj = stdx.ptrAlignCast(*cy.HeapObject, val.asPointer().?);
        if (obj.common.structId == cy.AstringT) {
            const str = obj.astring.getConstSlice();
            return std.fmt.parseFloat(f64, str) catch 0;
        } else if (obj.common.structId == cy.UstringT) {
            const str = obj.ustring.getConstSlice();
            return std.fmt.parseFloat(f64, str) catch 0;
        } else if (obj.common.structId == cy.RawStringT) {
            const str = obj.rawstring.getConstSlice();
            return std.fmt.parseFloat(f64, str) catch 0;
        } else return gvm.panic("Cannot convert struct to number");
    } else {
        switch (val.getTag()) {
            cy.NoneT => return 0,
            cy.BooleanT => return if (val.asBool()) 1 else 0,
            cy.IntegerT => return @intToFloat(f64, val.asI32()),
            cy.ErrorT => stdx.fatal(),
            cy.StaticAstringT => {
                const slice = val.asStaticStringSlice();
                const str = gvm.strBuf[slice.start..slice.end];
                return std.fmt.parseFloat(f64, str) catch 0;
            },
            cy.StaticUstringT => {
                const slice = val.asStaticStringSlice();
                const str = gvm.strBuf[slice.start..slice.end];
                return std.fmt.parseFloat(f64, str) catch 0;
            },
            else => stdx.panicFmt("unexpected tag {}", .{val.getTag()}),
        }
    }
}

fn evalNeg(val: Value) Value {
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

const SymbolMapType = enum {
    one,
    many,
    empty,
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
    mruTypeId: TypeId,
    mruOffset: u16,
    name: []const u8,
};

test "Internals." {
    try t.eq(@alignOf(VM), 8);
    try t.eq(@alignOf(MethodSym), 8);
    try t.eq(@sizeOf(MethodSym), 16);

    try t.eq(@sizeOf(FuncSymbolEntry), 16);
    var funcSymEntry: FuncSymbolEntry = undefined;
    try t.eq(@ptrToInt(&funcSymEntry.entryT), @ptrToInt(&funcSymEntry));
    try t.eq(@ptrToInt(&funcSymEntry.innerExtra), @ptrToInt(&funcSymEntry) + 4);
    try t.eq(@ptrToInt(&funcSymEntry.inner), @ptrToInt(&funcSymEntry) + 8);

    try t.eq(@sizeOf(AbsFuncSigKey), 16);
    try t.eq(@sizeOf(RelFuncSigKey), 8);

    try t.eq(@sizeOf(Struct), 24);
    try t.eq(@sizeOf(FieldSymbolMap), 24);


    try t.eq(@sizeOf(KeyU64), 8);
}

const MethodSymType = enum {
    func,
    nativeFunc1,
    nativeFunc2,
};

const NativeObjFuncPtr = *const fn (*UserVM, *anyopaque, [*]const Value, u8) Value;
const NativeObjFunc2Ptr = *const fn (*UserVM, *anyopaque, [*]const Value, u8) cy.ValuePair;
const NativeFuncPtr = *const fn (*UserVM, [*]const Value, u8) Value;

/// Keeping this small is better for function calls.
/// Secondary symbol data should be moved to `methodSymExtras`.
pub const MethodSym = struct {
    entryT: MethodSymType,
    /// Most recent sym used is cached avoid hashmap lookup. 
    mruStructId: TypeId,
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

    pub fn initFuncOffset(pc: usize, numLocals: u32) MethodSym {
        return .{
            .entryT = .func,
            .mruStructId = undefined,
            .inner = .{
                .func = .{
                    .pc = @intCast(u32, pc),
                    .numLocals = numLocals,
                },
            },
        };
    }

    pub fn initNativeFunc1(func: NativeObjFuncPtr) MethodSym {
        return .{
            .entryT = .nativeFunc1,
            .mruStructId = undefined,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    pub fn initNativeFunc2(func: NativeObjFunc2Ptr) MethodSym {
        return .{
            .entryT = .nativeFunc2,
            .mruStructId = undefined,
            .inner = .{
                .nativeFunc2 = func,
            },
        };
    }
};

pub const VarSym = struct {
    value: Value,

    pub fn init(val: Value) VarSym {
        return .{
            .value = val,
        };
    }
};

pub const FuncSymbolEntryType = enum {
    nativeFunc1,
    func,
    closure,
    none,
};

pub const FuncSymDetail = struct {
    name: []const u8,
};

/// TODO: Rename to FuncSymbol.
pub const FuncSymbolEntry = extern struct {
    entryT: u32,
    innerExtra: extern union {
        nativeFunc1: extern struct {
            /// Used to wrap a native func as a function value.
            numParams: u32,
        },
        none: extern struct {
            numParams: u32,
        },
    } = undefined,
    inner: extern union {
        nativeFunc1: *const fn (*UserVM, [*]const Value, u8) Value,
        func: packed struct {
            pc: u32,
            /// Includes locals, and return info slot. Does not include params.
            numLocals: u16,
            /// Num params used to wrap as function value.
            numParams: u16,
        },
        closure: *cy.Closure,
    },

    pub fn initNativeFunc1(func: *const fn (*UserVM, [*]const Value, u8) Value, numParams: u32) FuncSymbolEntry {
        return .{
            .entryT = @enumToInt(FuncSymbolEntryType.nativeFunc1),
            .innerExtra = .{
                .nativeFunc1 = .{
                    .numParams = numParams,
                }
            },
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    pub fn initFunc(pc: usize, numLocals: u16, numParams: u16) FuncSymbolEntry {
        return .{
            .entryT = @enumToInt(FuncSymbolEntryType.func),
            .inner = .{
                .func = .{
                    .pc = @intCast(u32, pc),
                    .numLocals = numLocals,
                    .numParams = numParams,
                },
            },
        };
    }

    pub fn initClosure(closure: *cy.Closure) FuncSymbolEntry {
        return .{
            .entryT = @enumToInt(FuncSymbolEntryType.closure),
            .inner = .{
                .closure = closure,
            },
        };
    }
};

const TagTypeId = u32;
const TagType = struct {
    name: []const u8,
    numMembers: u32,
};

const StructKey = KeyU64;

pub const TypeId = u32;

const Struct = struct {
    name: []const u8,
    numFields: u32,
};

// const StructSymbol = struct {
//     name: []const u8,
// };

pub const SymbolId = u32;

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

const Root = @This();

/// To reduce the amount of code inlined in the hot loop, handle StackOverflow at the top and resume execution.
/// This is also the entry way for native code to call into the VM, assuming pc, framePtr, and virtual registers are already set.
pub fn evalLoopGrowStack(vm: *VM) linksection(cy.HotSection) error{StackOverflow, OutOfMemory, Panic, OutOfBounds, NoDebugSym, End}!void {
    while (true) {
        @call(.always_inline, evalLoop, .{vm}) catch |err| {
            if (err == error.StackOverflow) {
                log.debug("grow stack", .{});
                try @call(.never_inline, vm.growStackAuto, .{});
                continue;
            } else if (err == error.End) {
                return;
            } else if (err == error.Panic) {
                try @call(.never_inline, gvm.buildStackTrace, .{true});
                return error.Panic;
            } else return err;
        };
        return;
    }
}

inline fn gotoNext(pc: *[*]cy.OpData, jumpTablePtr: u64) void {
    if (EnableAarch64ComputedGoto) {
        const asmPc = asm (
            \\ldrb w8, [%[pc]]
            \\ldrsw  x10, [x28, x8, lsl #2]
            \\adr x9, LOpBase
            // TODO: Remove extra offset addition somehow.
            \\add x9, x9, #4
            \\add x9, x9, x10
            : [ret] "={x9}" (-> [*]const u32)
            : [pc] "r" (pc.*),
              [jt] "{x28}" (jumpTablePtr),
            : "x10", "x8"
        );
        // Splitting into two inline assembly prevents
        // it from using a temporary register for `pc`, which would be a problem
        // since the switch case would generate the mov back to the dedicated pc register
        // after the inlined br inst.
        _ = asm volatile (
            \\br %[asmPc]
            :
            : [asmPc] "r" (asmPc),
            : 
        );
    }
}

const AarchPcRelativeAddressOp = packed struct {
    rd: u5,
    immhi: u19,
    fixed: u5 = 0b10000,
    immlo: u2,
    op: u1,

    fn getImm21(self: AarchPcRelativeAddressOp) u21 {
        return (self.immhi << 2) | self.immlo;
    }
};

const AarchAddSubtractImmOp = packed struct {
    rd: u5,
    rn: u5,
    imm12: u12,
    sh: u1,
    fixed: u6 = 0b100010,
    s: u1,
    op: u1,
    sf: u1,
};

/// Generate assembly labels to find sections easier.
const GenLabels = builtin.mode != .Debug and !builtin.cpu.arch.isWasm() and true;
const EnableAarch64ComputedGoto = aarch64 and builtin.mode != .Debug;
const useGoto = EnableAarch64ComputedGoto;

fn getJumpTablePtr(asmPc: [*]const u32) u64 {
    // Find the first adrp op above asmPc since codegen can be slightly different.
    var i: usize = 2;
    while (i < 20) : (i += 1) {
        const inst = @bitCast(AarchPcRelativeAddressOp, (asmPc - i)[0]);
        if (inst.fixed == 0b10000 and inst.op == 1) {
            const addOffsetInst = @bitCast(AarchAddSubtractImmOp, (asmPc - i + 1)[0]);
            var jumpTablePtr = (@ptrToInt(asmPc - i) + (inst.getImm21() << 12)) & ~((@as(u64, 1) << 12) - 1);
            jumpTablePtr += addOffsetInst.imm12;
            return jumpTablePtr;
        }
    }
    unreachable;
}

fn evalLoop(vm: *VM) linksection(cy.HotSection) error{StackOverflow, OutOfMemory, Panic, OutOfBounds, NoDebugSym, End}!void {
    if (GenLabels) {
        _ = asm volatile ("LEvalLoop:"::);
    }

    var jumpTablePtr: u64 = undefined;
    if (EnableAarch64ComputedGoto) {
        // For aarch64, this is a hack to get computed gotos.
        // Zig already generates the jump table for us, but it doesn't inline the jumps for each switch case.
        // This looks up the jump table address at runtime by looking at a relative adrp instruction.
        // Then LOpBase is placed before the switch. It doesn't end up exactly where the generated base address is,
        // so an offset is applied.
        // Finally, `gotoNext` is added to each switch case which performs an inline jump.

        // Get the jump table ptr from decoding two arm64 insts.
        const asmPc = asm (
            \\adr x28, .
            : [ret] "={x28}" (-> [*]const u32)
            : 
            :
        );

        jumpTablePtr = @call(.never_inline, getJumpTablePtr, .{asmPc});
    }

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
        if (EnableAarch64ComputedGoto) {
            // Base address to jump from.
            _ = asm volatile ("LOpBase:"::);
        }
        switch (pc[0].code) {
            .none => {
                if (GenLabels) {
                    _ = asm volatile ("LOpNone:"::);
                }
                framePtr[pc[1].arg] = Value.None;
                pc += 2;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .constOp => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstOp:"::);
                }
                framePtr[pc[2].arg] = Value.initRaw(vm.consts[pc[1].arg].val);
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .constI8 => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstI8:"::);
                }
                framePtr[pc[2].arg] = Value.initF64(@intToFloat(f64, @bitCast(i8, pc[1].arg)));
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .constI8Int => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstI8Int:"::);
                }
                framePtr[pc[2].arg] = Value.initI32(@intCast(i32, @bitCast(i8, pc[1].arg)));
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .release => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRelease:"::);
                }
                release(vm, framePtr[pc[1].arg]);
                pc += 2;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .releaseN => {
                if (GenLabels) {
                    _ = asm volatile ("LOpReleaseN:"::);
                }
                const numLocals = pc[1].arg;
                for (pc[2..2+numLocals]) |local| {
                    release(vm, framePtr[local.arg]);
                }
                pc += 2 + numLocals;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .fieldIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFieldIC:"::);
                }
                const recv = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    if (obj.common.structId == @ptrCast(*align (1) u16, pc + 4).*) {
                        framePtr[dst] = obj.object.getValue(pc[6].arg);
                        pc += 7;
                        if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.OpData{ .code = .field };
                // framePtr[dst] = try gvm.getField(recv, pc[3].arg);
                framePtr[dst] = try @call(.never_inline, gvm.getField, .{ recv, pc[3].arg });
                pc += 7;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .copyRetainSrc => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCopyRetainSrc:"::);
                }
                const val = framePtr[pc[1].arg];
                framePtr[pc[2].arg] = val;
                retain(vm, val);
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .jumpNotCond => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJumpNotCond:"::);
                }
                const jump = @ptrCast(*const align(1) u16, pc + 1).*;
                const cond = framePtr[pc[3].arg];
                const condVal = if (cond.isBool()) b: {
                    break :b cond.asBool();
                } else b: {
                    break :b @call(.never_inline, cond.toBool, .{});
                };
                if (!condVal) {
                    pc += jump;
                } else {
                    pc += 4;
                }
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .neg => {
                if (GenLabels) {
                    _ = asm volatile ("LOpNeg:"::);
                }
                const val = framePtr[pc[1].arg];
                // gvm.stack[gvm.framePtr + pc[2].arg] = if (val.isNumber())
                //     Value.initF64(-val.asF64())
                // else 
                    // @call(.never_inline, evalNegFallback, .{val});
                framePtr[pc[2].arg] = evalNeg(val);
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .compare => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCompare:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                // Can immediately match numbers, objects, primitives.
                framePtr[pc[3].arg] = if (left.val == right.val) Value.True else 
                    @call(.never_inline, evalCompare, .{vm, left, right});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .compareNot => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCompareNot:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                // Can immediately match numbers, objects, primitives.
                framePtr[pc[3].arg] = if (left.val == right.val) Value.False else 
                    @call(.never_inline, evalCompareNot, .{vm, left, right});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
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
            .add => {
                if (GenLabels) {
                    _ = asm volatile ("LOpAdd:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initF64(left.asF64() + right.asF64());
                } else {
                    framePtr[pc[3].arg] = try @call(.never_inline, evalAddFallback, .{ left, right });
                }
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .addInt => {
                if (GenLabels) {
                    _ = asm volatile ("LOpAddInt:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = Value.initI32(left.asI32() + right.asI32());
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .minus => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMinus:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = if (Value.bothNumbers(left, right))
                    Value.initF64(left.asF64() - right.asF64())
                else @call(.never_inline, evalMinusFallback, .{left, right});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .minusInt => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMinusInt:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = Value.initI32(left.asI32() - right.asI32());
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .less => {
                if (GenLabels) {
                    _ = asm volatile ("LOpLess:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = if (Value.bothNumbers(left, right))
                    Value.initBool(left.asF64() < right.asF64())
                else
                    @call(.never_inline, evalLessFallback, .{left, right});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .lessInt => {
                if (GenLabels) {
                    _ = asm volatile ("LOpLessInt:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = Value.initBool(left.asI32() < right.asI32());
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .greater => {
                if (GenLabels) {
                    _ = asm volatile ("LOpGreater:"::);
                }
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = evalGreater(srcLeft, srcRight);
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .lessEqual => {
                if (GenLabels) {
                    _ = asm volatile ("LOpLessEqual:"::);
                }
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = evalLessOrEqual(srcLeft, srcRight);
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .greaterEqual => {
                if (GenLabels) {
                    _ = asm volatile ("LOpGreaterEqual:"::);
                }
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = evalGreaterOrEqual(srcLeft, srcRight);
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .true => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTrue:"::);
                }
                framePtr[pc[1].arg] = Value.True;
                pc += 2;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .false => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFalse:"::);
                }
                framePtr[pc[1].arg] = Value.False;
                pc += 2;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .not => {
                if (GenLabels) {
                    _ = asm volatile ("LOpNot:"::);
                }
                const val = framePtr[pc[1].arg];
                framePtr[pc[2].arg] = evalNot(val);
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .stringTemplate => {
                if (GenLabels) {
                    _ = asm volatile ("LOpStringTemplate:"::);
                }
                const startLocal = pc[1].arg;
                const exprCount = pc[2].arg;
                const dst = pc[3].arg;
                const strCount = exprCount + 1;
                const strs = pc[4 .. 4 + strCount];
                pc += 4 + strCount;
                const vals = framePtr[startLocal .. startLocal + exprCount];
                const res = try @call(.never_inline, cy.heap.allocStringTemplate, .{vm, strs, vals});
                framePtr[dst] = res;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .list => {
                if (GenLabels) {
                    _ = asm volatile ("LOpList:"::);
                }
                const startLocal = pc[1].arg;
                const numElems = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const elems = framePtr[startLocal..startLocal + numElems];
                const list = try cy.heap.allocList(vm, elems);
                framePtr[dst] = list;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .mapEmpty => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMapEmpty:"::);
                }
                const dst = pc[1].arg;
                pc += 2;
                framePtr[dst] = try cy.heap.allocEmptyMap(vm);
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .objectSmall => {
                if (GenLabels) {
                    _ = asm volatile ("LOpObjectSmall:"::);
                }
                const sid = pc[1].arg;
                const startLocal = pc[2].arg;
                const numFields = pc[3].arg;
                const fields = framePtr[startLocal .. startLocal + numFields];
                framePtr[pc[4].arg] = try cy.heap.allocObjectSmall(vm, sid, fields);
                if (builtin.mode == .Debug) {
                    vm.objectTraceMap.put(vm.alloc, framePtr[pc[4].arg].asHeapObject(*HeapObject), pcOffset(vm, pc)) catch stdx.fatal();
                }
                pc += 5;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .object => {
                if (GenLabels) {
                    _ = asm volatile ("LOpObject:"::);
                }
                const sid = pc[1].arg;
                const startLocal = pc[2].arg;
                const numFields = pc[3].arg;
                const fields = framePtr[startLocal .. startLocal + numFields];
                framePtr[pc[4].arg] = try cy.heap.allocObject(vm, sid, fields);
                pc += 5;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .map => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMap:"::);
                }
                const startLocal = pc[1].arg;
                const numEntries = pc[2].arg;
                const dst = pc[3].arg;
                const keyIdxes = pc[4..4+numEntries];
                pc += 4 + numEntries;
                const vals = framePtr[startLocal .. startLocal + numEntries];
                framePtr[dst] = try cy.heap.allocMap(vm, keyIdxes, vals);
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .slice => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSlice:"::);
                }
                const slice = &framePtr[pc[1].arg];
                const start = framePtr[pc[2].arg];
                const end = framePtr[pc[3].arg];
                framePtr[pc[4].arg] = try @call(.never_inline, vm.sliceOp, .{slice, start, end});
                pc += 5;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .setInitN => {
                if (GenLabels) {
                    _ = asm volatile ("LSetInitN:"::);
                }
                const numLocals = pc[1].arg;
                const locals = pc[2..2+numLocals];
                pc += 2 + numLocals;
                for (locals) |local| {
                    framePtr[local.arg] = Value.None;
                }
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .setIndex => {
                if (GenLabels) {
                    _ = asm volatile ("LSetIndex:"::);
                }
                const leftv = framePtr[pc[1].arg];
                const indexv = framePtr[pc[2].arg];
                const rightv = framePtr[pc[3].arg];
                try @call(.never_inline, vm.setIndex, .{leftv, indexv, rightv});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .setIndexRelease => {
                if (GenLabels) {
                    _ = asm volatile ("LSetIndexRelease:"::);
                }
                const leftv = framePtr[pc[1].arg];
                const indexv = framePtr[pc[2].arg];
                const rightv = framePtr[pc[3].arg];
                try @call(.never_inline, vm.setIndexRelease, .{leftv, indexv, rightv});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .copy => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCopy:"::);
                }
                framePtr[pc[2].arg] = framePtr[pc[1].arg];
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .copyRetainRelease => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCopyRetainRelease:"::);
                }
                const src = pc[1].arg;
                const dst = pc[2].arg;
                pc += 3;
                retain(vm, framePtr[src]);
                release(vm, framePtr[dst]);
                framePtr[dst] = framePtr[src];
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .copyReleaseDst => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCopyReleaseDst:"::);
                }
                const dst = pc[2].arg;
                release(vm, framePtr[dst]);
                framePtr[dst] = framePtr[pc[1].arg];
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .retain => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRetain:"::);
                }
                retain(vm, framePtr[pc[1].arg]);
                pc += 2;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .index => {
                if (GenLabels) {
                    _ = asm volatile ("LOpIndex:"::);
                }
                const recv = &framePtr[pc[1].arg];
                const indexv = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = try @call(.never_inline, vm.getIndex, .{recv, indexv});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .reverseIndex => {
                if (GenLabels) {
                    _ = asm volatile ("LOpReverseIndex:"::);
                }
                const recv = &framePtr[pc[1].arg];
                const indexv = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = try @call(.never_inline, vm.getReverseIndex, .{recv, indexv});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .jump => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJump:"::);
                }
                @setRuntimeSafety(false);
                pc += @intCast(usize, @ptrCast(*const align(1) i16, &pc[1]).*);
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .jumpCond => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJumpCond:"::);
                }
                const jump = @ptrCast(*const align(1) i16, pc + 1).*;
                const cond = framePtr[pc[3].arg];
                const condVal = if (cond.isBool()) b: {
                    break :b cond.asBool();
                } else b: {
                    break :b @call(.never_inline, cond.toBool, .{});
                };
                if (condVal) {
                    @setRuntimeSafety(false);
                    pc += @intCast(usize, jump);
                } else {
                    pc += 4;
                }
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .call0 => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCall0:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                pc += 3;

                const callee = framePtr[startLocal + numArgs + 4];
                const retInfo = buildReturnInfo(0, true);
                // const retInfo = buildReturnInfo(pcOffset(pc), framePtrOffset(framePtr), 0, true);
                // try @call(.never_inline, gvm.call, .{&pc, callee, numArgs, retInfo});
                try @call(.always_inline, call, .{vm, &pc, &framePtr, callee, startLocal, numArgs, retInfo});
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .call1 => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCall1:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                pc += 3;

                const callee = framePtr[startLocal + numArgs + 4];
                const retInfo = buildReturnInfo(1, true);
                // const retInfo = buildReturnInfo(pcOffset(pc), framePtrOffset(framePtr), 1, true);
                // try @call(.never_inline, gvm.call, .{&pc, callee, numArgs, retInfo});
                try @call(.always_inline, call, .{vm, &pc, &framePtr, callee, startLocal, numArgs, retInfo});
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .callObjFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallObjFuncIC:"::);
                }
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
                    pc = @intToPtr([*]cy.OpData, @intCast(usize, @ptrCast(*align(1) u48, pc + 6).*));
                    if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                    continue;
                }

                // Deoptimize.
                pc[0] = cy.OpData{ .code = .callObjSym };
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .callObjNativeFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallObjNativeFuncIC:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const recv = &framePtr[startLocal + numArgs + 4 - 1];
                var obj: *HeapObject = undefined;
                var typeId: u32 = undefined;
                if (recv.isPointer()) {
                    obj = recv.asHeapObject(*HeapObject);
                    typeId = obj.common.structId;
                } else {
                    obj = @ptrCast(*HeapObject, recv);
                    typeId = recv.getPrimitiveTypeId();
                }

                const cachedStruct = @ptrCast(*align (1) u16, pc + 12).*;
                if (typeId == cachedStruct) {
                    // const newFramePtr = framePtr + startLocal;
                    vm.framePtr = framePtr;
                    const func = @intToPtr(NativeObjFuncPtr, @intCast(usize, @ptrCast(*align (1) u48, pc + 6).*));
                    const res = func(@ptrCast(*UserVM, vm), obj, @ptrCast([*]const Value, framePtr + startLocal + 4), numArgs);
                    if (res.isPanic()) {
                        return error.Panic;
                    }
                    const numRet = pc[3].arg;
                    if (numRet == 1) {
                        framePtr[startLocal] = res;
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
                    pc += 14;
                    // In the future, we might allow native functions to change the pc and framePtr.
                    // pc = vm.pc;
                    // framePtr = vm.framePtr;
                    if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                    continue;
                }

                // Deoptimize.
                pc[0] = cy.OpData{ .code = .callObjSym };
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .callFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallFuncIC:"::);
                }
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

                pc = @intToPtr([*]cy.OpData, @intCast(usize, @ptrCast(*align(1) u48, pc + 5).*));
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .callNativeFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallNativeFuncIC:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;

                const newFramePtr = framePtr + startLocal;
                vm.framePtr = newFramePtr;
                const func = @intToPtr(NativeFuncPtr, @intCast(usize, @ptrCast(*align (1) u48, pc + 5).*));
                const res = func(@ptrCast(*UserVM, vm), @ptrCast([*]const Value, newFramePtr + 4), numArgs);
                if (res.isPanic()) {
                    return error.Panic;
                }
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
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .ret1 => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRet1:"::);
                }
                if (@call(.always_inline, popStackFrameLocal1, .{vm, &pc, &framePtr})) {
                    if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                    continue;
                } else {
                    return;
                }
            },
            .ret0 => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRet0:"::);
                }
                if (@call(.always_inline, popStackFrameLocal0, .{&pc, &framePtr})) {
                    if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                    continue;
                } else {
                    return;
                }
            },
            .setFieldReleaseIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetFieldReleaseIC:"::);
                }
                const recv = framePtr[pc[1].arg];
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    if (obj.common.structId == @ptrCast(*align (1) u16, pc + 4).*) {
                        const lastValue = obj.object.getValuePtr(pc[6].arg);
                        release(vm, lastValue.*);
                        lastValue.* = framePtr[pc[2].arg];
                        pc += 7;
                        if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.OpData{ .code = .setFieldRelease };
                // framePtr[dst] = try gvm.getField(recv, pc[3].arg);
                try @call(.never_inline, gvm.setFieldRelease, .{ recv, pc[3].arg, framePtr[pc[2].arg] });
                pc += 7;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .fieldRetainIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFieldRetainIC:"::);
                }
                const recv = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    if (obj.common.structId == @ptrCast(*align (1) u16, pc + 4).*) {
                        framePtr[dst] = obj.object.getValue(pc[6].arg);
                        retain(vm, framePtr[dst]);
                        pc += 7;
                        if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.OpData{ .code = .fieldRetain };
                // framePtr[dst] = try gvm.getField(recv, pc[3].arg);
                framePtr[dst] = try @call(.never_inline, gvm.getField, .{ recv, pc[3].arg });
                retain(vm, framePtr[dst]);
                pc += 7;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .forRangeInit => {
                if (GenLabels) {
                    _ = asm volatile ("LOpForRangeInit:"::);
                }
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
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .forRange => {
                if (GenLabels) {
                    _ = asm volatile ("LOpForRange:"::);
                }
                const counter = framePtr[pc[1].arg].asF64() + framePtr[pc[2].arg].asF64();
                if (counter < framePtr[pc[3].arg].asF64()) {
                    framePtr[pc[1].arg] = Value.initF64(counter);
                    framePtr[pc[4].arg] = Value.initF64(counter);
                    pc -= @ptrCast(*const align(1) u16, pc + 5).*;
                } else {
                    pc += 7;
                }
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .forRangeReverse => {
                if (GenLabels) {
                    _ = asm volatile ("LOpForRangeReverse:"::);
                }
                const counter = framePtr[pc[1].arg].asF64() - framePtr[pc[2].arg].asF64();
                if (counter > framePtr[pc[3].arg].asF64()) {
                    framePtr[pc[1].arg] = Value.initF64(counter);
                    framePtr[pc[4].arg] = Value.initF64(counter);
                    pc -= @ptrCast(*const align(1) u16, pc + 5).*;
                } else {
                    pc += 7;
                }
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .jumpNotNone => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJumpNotNone:"::);
                }
                const offset = @ptrCast(*const align(1) i16, &pc[1]).*;
                if (!framePtr[pc[3].arg].isNone()) {
                    @setRuntimeSafety(false);
                    pc += @intCast(usize, offset);
                } else {
                    pc += 4;
                }
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .setField => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetField:"::);
                }
                const fieldId = pc[1].arg;
                const left = pc[2].arg;
                const right = pc[3].arg;
                pc += 4;

                const recv = framePtr[left];
                const val = framePtr[right];
                try gvm.setField(recv, fieldId, val);
                // try @call(.never_inline, gvm.setField, .{recv, fieldId, val});
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .fieldRelease => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFieldRelease:"::);
                }
                const fieldId = pc[1].arg;
                const left = pc[2].arg;
                const dst = pc[3].arg;
                pc += 4;
                const recv = framePtr[left];
                framePtr[dst] = try @call(.never_inline, gvm.getField, .{recv, fieldId});
                release(vm, recv);
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .field => {
                if (GenLabels) {
                    _ = asm volatile ("LOpField:"::);
                }
                const left = pc[1].arg;
                const dst = pc[2].arg;
                const symId = pc[3].arg;
                const recv = framePtr[left];
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    // const offset = @call(.never_inline, gvm.getFieldOffset, .{obj, symId });
                    const offset = gvm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        framePtr[dst] = obj.object.getValue(offset);
                        // Inline cache.
                        pc[0] = cy.OpData{ .code = .fieldIC };
                        @ptrCast(*align (1) u16, pc + 4).* = @intCast(u16, obj.common.structId);
                        pc[6] = cy.OpData { .arg = offset };
                    } else {
                        framePtr[dst] = @call(.never_inline, gvm.getFieldFallback, .{obj, gvm.fieldSyms.buf[symId].name});
                    }
                    pc += 7;
                    if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                    continue;
                } else {
                    return vm.getFieldMissingSymbolError();
                }
            },
            .fieldRetain => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFieldRetain:"::);
                }
                const recv = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                const symId = pc[3].arg;
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    // const offset = @call(.never_inline, gvm.getFieldOffset, .{obj, symId });
                    const offset = gvm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        framePtr[dst] = obj.object.getValue(offset);
                        // Inline cache.
                        pc[0] = cy.OpData{ .code = .fieldRetainIC };
                        @ptrCast(*align (1) u16, pc + 4).* = @intCast(u16, obj.common.structId);
                        pc[6] = cy.OpData { .arg = offset };
                    } else {
                        framePtr[dst] = @call(.never_inline, gvm.getFieldFallback, .{obj, gvm.fieldSyms.buf[symId].name});
                    }
                    retain(vm, framePtr[dst]);
                    pc += 7;
                    if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                    continue;
                } else {
                    return vm.getFieldMissingSymbolError();
                }
            },
            .setFieldRelease => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetFieldRelease:"::);
                }
                const recv = framePtr[pc[1].arg];
                const val = framePtr[pc[2].arg];
                const symId = pc[3].arg;
                if (recv.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer());
                    // const offset = @call(.never_inline, gvm.getFieldOffset, .{obj, symId });
                    const offset = gvm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        const lastValue = obj.object.getValuePtr(offset);
                        release(vm, lastValue.*);
                        lastValue.* = val;

                        // Inline cache.
                        pc[0] = cy.OpData{ .code = .setFieldReleaseIC };
                        @ptrCast(*align (1) u16, pc + 4).* = @intCast(u16, obj.common.structId);
                        pc[6] = cy.OpData { .arg = offset };
                        pc += 7;
                        if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                        continue;
                    } else {
                        return vm.getFieldMissingSymbolError();
                    }
                } else {
                    return vm.setFieldNotObjectError();
                }
            },
            .lambda => {
                if (GenLabels) {
                    _ = asm volatile ("LOpLambda:"::);
                }
                const funcPc = pcOffset(vm, pc) - pc[1].arg;
                const numParams = pc[2].arg;
                const numLocals = pc[3].arg;
                const dst = pc[4].arg;
                pc += 5;
                framePtr[dst] = try @call(.never_inline, cy.heap.allocLambda, .{vm, funcPc, numParams, numLocals});
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .closure => {
                if (GenLabels) {
                    _ = asm volatile ("LOpClosure:"::);
                }
                const funcPc = pcOffset(vm, pc) - pc[1].arg;
                const numParams = pc[2].arg;
                const numCaptured = pc[3].arg;
                const numLocals = pc[4].arg;
                const dst = pc[5].arg;
                const capturedVals = pc[6..6+numCaptured];
                pc += 6 + numCaptured;

                framePtr[dst] = try @call(.never_inline, cy.heap.allocClosure, .{vm, framePtr, funcPc, numParams, numLocals, capturedVals});
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .staticVar => {
                if (GenLabels) {
                    _ = asm volatile ("LOpStaticVar:"::);
                }
                const symId = pc[1].arg;
                const sym = vm.varSyms.buf[symId];
                retain(vm, sym.value);
                framePtr[pc[2].arg] = sym.value;
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .setStaticVar => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetStaticVar:"::);
                }
                const symId = pc[1].arg;
                const prev = vm.varSyms.buf[symId].value;
                vm.varSyms.buf[symId].value = framePtr[pc[2].arg];
                release(vm, prev);
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .staticFunc => {
                if (GenLabels) {
                    _ = asm volatile ("LOpStaticFunc:"::);
                }
                const symId = pc[1].arg;
                framePtr[pc[2].arg] = try cy.heap.allocFuncFromSym(vm, symId);
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .coreturn => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCoreturn:"::);
                }
                pc += 1;
                if (vm.curFiber != &vm.mainFiber) {
                    const res = cy.fiber.popFiber(vm, cy.NullId, framePtr, framePtr[1]);
                    pc = res.pc;
                    framePtr = res.framePtr;
                }
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .coresume => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCoresume:"::);
                }
                const fiber = framePtr[pc[1].arg];
                if (fiber.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, fiber.asPointer().?);
                    if (obj.common.structId == cy.FiberS) {
                        if (&obj.fiber != vm.curFiber) {
                            // Only resume fiber if it's not done.
                            if (obj.fiber.pc != cy.NullId) {
                                const res = cy.fiber.pushFiber(vm, pcOffset(vm, pc + 3), framePtr, &obj.fiber, pc[2].arg);
                                pc = res.pc;
                                framePtr = res.framePtr;
                                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                                continue;
                            }
                        }
                    }
                    cy.arc.releaseObject(vm, obj);
                }
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .coyield => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCoyield:"::);
                }
                if (vm.curFiber != &vm.mainFiber) {
                    // Only yield on user fiber.
                    const res = cy.fiber.popFiber(vm, pcOffset(vm, pc), framePtr, Value.None);
                    pc = res.pc;
                    framePtr = res.framePtr;
                } else {
                    pc += 3;
                }
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .coinit => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCoinit:"::);
                }
                const startArgsLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const jump = pc[3].arg;
                const initialStackSize = pc[4].arg;
                const dst = pc[5].arg;

                const args = framePtr[startArgsLocal..startArgsLocal + numArgs];
                const fiber = try @call(.never_inline, cy.fiber.allocFiber, .{vm, pcOffset(vm, pc + 6), args, initialStackSize});
                framePtr[dst] = fiber;
                pc += jump;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .mul => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMul:"::);
                }
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.never_inline, evalMultiply, .{srcLeft, srcRight});
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .div => {
                if (GenLabels) {
                    _ = asm volatile ("LOpDiv:"::);
                }
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.never_inline, evalDivide, .{srcLeft, srcRight});
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .mod => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMod:"::);
                }
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.never_inline, evalMod, .{srcLeft, srcRight});
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .pow => {
                if (GenLabels) {
                    _ = asm volatile ("LOpPow:"::);
                }
                const srcLeft = framePtr[pc[1].arg];
                const srcRight = framePtr[pc[2].arg];
                const dstLocal = pc[3].arg;
                pc += 4;
                framePtr[dstLocal] = @call(.never_inline, evalPower, .{srcLeft, srcRight});
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .box => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBox:"::);
                }
                const value = framePtr[pc[1].arg];
                retain(vm, value);
                framePtr[pc[2].arg] = try cy.heap.allocBox(vm, value);
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .setBoxValue => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetBoxValue:"::);
                }
                const box = framePtr[pc[1].arg];
                const rval = framePtr[pc[2].arg];
                pc += 3;
                if (builtin.mode == .Debug) {
                    std.debug.assert(box.isPointer());
                }
                const obj = stdx.ptrAlignCast(*HeapObject, box.asPointer().?);
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.common.structId == cy.BoxS);
                }
                obj.box.val = rval;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .setBoxValueRelease => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBoxValueRelease:"::);
                }
                const box = framePtr[pc[1].arg];
                const rval = framePtr[pc[2].arg];
                pc += 3;
                if (builtin.mode == .Debug) {
                    std.debug.assert(box.isPointer());
                }
                const obj = stdx.ptrAlignCast(*HeapObject, box.asPointer().?);
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.common.structId == cy.BoxS);
                }
                @call(.never_inline, release, .{vm, obj.box.val});
                obj.box.val = rval;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .boxValue => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBoxValue:"::);
                }
                const box = framePtr[pc[1].arg];
                if (box.isPointer()) {
                    const obj = stdx.ptrAlignCast(*HeapObject, box.asPointer().?);
                    if (builtin.mode == .Debug) {
                        std.debug.assert(obj.common.structId == cy.BoxS);
                    }
                    framePtr[pc[2].arg] = obj.box.val;
                } else {
                    if (builtin.mode == .Debug) {
                        std.debug.assert(box.isNone());
                    }
                    framePtr[pc[2].arg] = Value.None;
                }
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .boxValueRetain => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBoxValueRetain:"::);
                }
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
                // retain(vm, obj.box.val);
                // pc += 3;
                framePtr[pc[2].arg] = @call(.never_inline, boxValueRetain, .{vm, box});
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .tag => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTag:"::);
                }
                const tagId = pc[1].arg;
                const val = pc[2].arg;
                framePtr[pc[3].arg] = Value.initTag(tagId, val);
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .tagLiteral => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTagLiteral:"::);
                }
                const symId = pc[1].arg;
                framePtr[pc[2].arg] = Value.initTagLiteral(symId);
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .tryValue => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTryValue:"::);
                }
                const val = framePtr[pc[1].arg];
                if (!val.isError()) {
                    framePtr[pc[2].arg] = val;
                    pc += 5;
                    if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                    continue;
                } else {
                    if (framePtr != vm.stack.ptr) {
                        framePtr[0] = val;
                        pc += @ptrCast(*const align(1) u16, pc + 3).*;
                        if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                        continue;
                    } else {
                        // Panic on root block.
                        vm.panicType = .err;
                        vm.panicPayload = val.val;
                        return error.Panic;
                    }
                }
            },
            .bitwiseAnd => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseAnd:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = @call(.never_inline, evalBitwiseAnd, .{left, right});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .bitwiseOr => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseOr:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = @call(.never_inline, evalBitwiseOr, .{left, right});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .bitwiseXor => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseXor:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = @call(.never_inline, evalBitwiseXor, .{left, right});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .bitwiseNot => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseNot:"::);
                }
                const val = framePtr[pc[1].arg];
                framePtr[pc[2].arg] = @call(.never_inline, evalBitwiseNot, .{val});
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .bitwiseLeftShift => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseLeftShift:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = @call(.never_inline, evalBitwiseLeftShift, .{left, right});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .bitwiseRightShift => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseRightShift:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = @call(.never_inline, evalBitwiseRightShift, .{left, right});
                pc += 4;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .setCapValToFuncSyms => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetCapValToFuncSyms:"::);
                }
                const capVal = framePtr[pc[1].arg];
                const numSyms = pc[2].arg;
                const syms = pc[3..3+numSyms*2];
                @call(.never_inline, setCapValToFuncSyms, .{ vm, capVal, numSyms, syms });
                pc += 3 + numSyms*2;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .callObjSym => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallObjSym:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const numRet = pc[3].arg;
                const symId = pc[4].arg;

                const recv = &framePtr[startLocal + numArgs + 4 - 1];
                var obj: *HeapObject = undefined;
                var typeId: u32 = undefined;
                if (recv.isPointer()) {
                    obj = recv.asHeapObject(*HeapObject);
                    typeId = obj.common.structId;
                } else {
                    obj = @ptrCast(*HeapObject, recv);
                    typeId = recv.getPrimitiveTypeId();
                }

                if (vm.getCallObjSym(typeId, symId)) |sym| {
                    const res = try @call(.never_inline, vm.callSymEntry, .{pc, framePtr, sym, obj, typeId, startLocal, numArgs, numRet });
                    pc = res.pc;
                    framePtr = res.framePtr;
                } else {
                    const res = try @call(.never_inline, callObjSymFallback, .{vm, pc, framePtr, obj, typeId, symId, startLocal, numArgs, numRet});
                    pc = res.pc;
                    framePtr = res.framePtr;
                }
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .callSym => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallSym:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const numRet = pc[3].arg;
                const symId = pc[4].arg;
                const res = try @call(.never_inline, vm.callSym, .{pc, framePtr, symId, startLocal, numArgs, @intCast(u2, numRet)});
                pc = res.pc;
                framePtr = res.framePtr;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .match => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMatch:"::);
                }
                pc += @call(.never_inline, opMatch, .{vm, pc, framePtr});
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .sym => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSym:"::);
                }
                const symType = pc[1].arg;
                const symId = @ptrCast(*const align(1) u32, pc + 2).*;
                framePtr[pc[6].arg] = try cy.heap.allocSymbol(vm, symType, symId);
                pc += 7;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .setStaticFunc => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetStaticFunc:"::);
                }
                const symId = pc[1].arg;
                try @call(.never_inline, setStaticFunc, .{vm, symId, framePtr[pc[2].arg]});
                pc += 3;
                if (useGoto) { gotoNext(&pc, jumpTablePtr); }
                continue;
            },
            .end => {
                if (GenLabels) {
                    _ = asm volatile ("LOpEnd:"::);
                }
                vm.endLocal = pc[1].arg;
                pc += 2;
                vm.curFiber.pc = @intCast(u32, pcOffset(vm, pc));
                return error.End;
            },
        }
    }
}

fn popStackFrameLocal0(pc: *[*]const cy.OpData, framePtr: *[*]Value) linksection(cy.HotSection) bool {
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

fn popStackFrameLocal1(vm: *VM, pc: *[*]const cy.OpData, framePtr: *[*]Value) linksection(cy.HotSection) bool {
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
    const offset = pcOffset(vm, pc);
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
            const chunk = vm.compiler.chunks.items[frame.chunkId];
            const lineEnd = std.mem.indexOfScalarPos(u8, chunk.src, frame.lineStartPos, '\n') orelse chunk.src.len;
            arrowBuf.clearRetainingCapacity();
            try w.writeByteNTimes(' ', frame.col);
            try w.writeByte('^');
            fmt.printStderr(
                \\{}:{}:{} {}:
                \\{}
                \\{}
                \\
            , &.{
                fmt.v(chunk.srcUri), fmt.v(frame.line+1), fmt.v(frame.col+1), fmt.v(frame.name),
                fmt.v(chunk.src[frame.lineStartPos..lineEnd]), fmt.v(arrowBuf.items),
            });
        }
    }
};

pub const StackFrame = struct {
    /// Name identifier (eg. function name)
    name: []const u8,
    /// Starts at 0.
    line: u32,
    /// Starts at 0.
    col: u32,
    /// Where the line starts in the source file.
    lineStartPos: u32,
    chunkId: u32,
};

const ObjectSymKey = struct {
    structId: TypeId,
    symId: SymbolId,
};

/// See `reserveFuncParams` for stack layout.
/// numArgs does not include the callee.
pub fn call(vm: *VM, pc: *[*]cy.OpData, framePtr: *[*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !void {
    if (callee.isPointer()) {
        const obj = stdx.ptrAlignCast(*HeapObject, callee.asPointer().?);
        switch (obj.common.structId) {
            cy.ClosureS => {
                if (numArgs != obj.closure.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    // Release func and args.
                    for (framePtr.*[startLocal + 4..startLocal + 4 + numArgs]) |val| {
                        release(vm, val);
                    }
                    framePtr.*[startLocal] = Value.initErrorTagLit(@enumToInt(bindings.TagLit.InvalidSignature));
                    return;
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.closure.numLocals) >= @ptrToInt(vm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr.* };
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;
                framePtr.*[2] = Value{ .retPcPtr = pc.* };
                framePtr.*[3] = retFramePtr;
                pc.* = vm.toPc(obj.closure.funcPc);

                // Copy over captured vars to new call stack locals.
                const src = obj.closure.getCapturedValuesPtr()[0..obj.closure.numCaptured];
                std.mem.copy(Value, framePtr.*[numArgs + 4 + 1..numArgs + 4 + 1 + obj.closure.numCaptured], src);
            },
            cy.LambdaS => {
                if (numArgs != obj.lambda.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    // Release func and args.
                    for (framePtr.*[startLocal + 4..startLocal + 4 + numArgs]) |val| {
                        release(vm, val);
                    }
                    framePtr.*[startLocal] = Value.initErrorTagLit(@enumToInt(bindings.TagLit.InvalidSignature));
                    return;
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.lambda.numLocals) >= @ptrToInt(vm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr.* };
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;
                framePtr.*[2] = Value{ .retPcPtr = pc.* };
                framePtr.*[3] = retFramePtr;
                pc.* = vm.toPc(obj.lambda.funcPc);
            },
            cy.NativeFunc1S => {
                if (numArgs != obj.nativeFunc1.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    for (framePtr.*[startLocal + 4..startLocal + 4 + numArgs]) |val| {
                        release(vm, val);
                    }
                    framePtr.*[startLocal] = Value.initErrorTagLit(@enumToInt(bindings.TagLit.InvalidSignature));
                    return;
                }

                vm.pc = pc.*;
                const newFramePtr = framePtr.* + startLocal;
                vm.framePtr = newFramePtr;
                const res = obj.nativeFunc1.func(@ptrCast(*UserVM, vm), newFramePtr + 4, numArgs);
                newFramePtr[0] = res;
            },
            else => {},
        }
    } else {
        stdx.panic("not a function");
    }
}

pub fn callNoInline(vm: *VM, pc: *[*]cy.OpData, framePtr: *[*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !void {
    if (callee.isPointer()) {
        const obj = stdx.ptrAlignCast(*cy.HeapObject, callee.asPointer().?);
        switch (obj.common.structId) {
            cy.ClosureS => {
                if (numArgs != obj.closure.numParams) {
                    stdx.panic("params/args mismatch");
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.closure.numLocals) >= @ptrToInt(vm.stack.ptr) + (vm.stack.len << 3)) {
                    return error.StackOverflow;
                }

                pc.* = vm.toPc(obj.closure.funcPc);
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;

                // Copy over captured vars to new call stack locals.
                const src = obj.closure.getCapturedValuesPtr()[0..obj.closure.numCaptured];
                std.mem.copy(Value, framePtr.*[numArgs + 4 + 1..numArgs + 4 + 1 + obj.closure.numCaptured], src);
            },
            cy.LambdaS => {
                if (numArgs != obj.lambda.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    stdx.fatal();
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.lambda.numLocals) >= @ptrToInt(vm.stack.ptr) + (vm.stack.len << 3)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr.* };
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;
                framePtr.*[2] = Value{ .retPcPtr = pc.* + 14 };
                framePtr.*[3] = retFramePtr;
                pc.* = vm.toPc(obj.lambda.funcPc);
            },
            cy.NativeFunc1S => {
                vm.pc = pc.*;
                const newFramePtr = framePtr.* + startLocal;
                vm.framePtr = newFramePtr;
                const res = obj.nativeFunc1.func(@ptrCast(*UserVM, vm), newFramePtr + 4, numArgs);
                newFramePtr[0] = res;
                cy.arc.releaseObject(vm, obj);
                pc.* += 14;
            },
            else => {},
        }
    } else {
        stdx.panic("not a function");
    }
}

fn getObjectFunctionFallback(vm: *VM, obj: *const HeapObject, typeId: u32, symId: SymbolId) !Value {
    @setCold(true);
    _ = obj;
    // Map fallback is no longer supported since cleanup of recv is not auto generated by the compiler.
    // In the future, this may invoke the exact method signature or call a custom overloaded function.
    // if (typeId == MapS) {
    //     const name = vm.methodSymExtras.buf[symId];
    //     const heapMap = stdx.ptrAlignCast(*const MapInner, &obj.map.inner);
    //     if (heapMap.getByString(vm, name)) |val| {
    //         return val;
    //     }
    // }

    return vm.panicFmt("Missing method symbol `{}` from receiver of type `{}`.", &.{
        v(vm.methodSymExtras.buf[symId]), v(vm.structs.buf[typeId].name),
    });
}

/// Use new pc local to avoid deoptimization.
fn callObjSymFallback(vm: *VM, pc: [*]cy.OpData, framePtr: [*]Value, obj: *HeapObject, typeId: u32, symId: SymbolId, startLocal: u8, numArgs: u8, reqNumRetVals: u8) linksection(cy.Section) !PcFramePtr {
    @setCold(true);
    // const func = try @call(.never_inline, getObjectFunctionFallback, .{obj, symId});
    const func = try getObjectFunctionFallback(vm, obj, typeId, symId);

    retain(vm, func);
    cy.arc.releaseObject(vm, obj);

    // Replace receiver with function.
    framePtr[startLocal + 4 + numArgs - 1] = func;
    // const retInfo = buildReturnInfo(pc, framePtrOffset(framePtr), reqNumRetVals, true);
    const retInfo = buildReturnInfo2(reqNumRetVals, true);
    var newPc = pc;
    var newFramePtr = framePtr;
    try @call(.always_inline, callNoInline, .{vm, &newPc, &newFramePtr, func, startLocal, numArgs-1, retInfo});
    return PcFramePtr{
        .pc = newPc,
        .framePtr = newFramePtr,
    };
}

fn callSymEntryNoInline(vm: *VM, pc: [*]const cy.OpData, framePtr: [*]Value, sym: MethodSym, obj: *HeapObject, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2) linksection(cy.HotSection) !PcFramePtr {
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
                .pc = vm.toPc(sym.inner.func.pc),
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


/// Given pc position, return the end locals pc in the same frame.
/// TODO: Memoize this function.
pub fn pcToEndLocalsPc(vm: *const VM, pc: usize) u32 {
    const idx = debug.indexOfDebugSym(vm, pc) orelse {
        stdx.panic("Missing debug symbol.");
    };
    const sym = vm.debugTable[idx];
    if (sym.frameLoc != cy.NullId) {
        const chunk = vm.compiler.chunks.items[sym.file];
        const node = chunk.nodes[sym.frameLoc];
        return node.head.func.genEndLocalsPc;
    } else return cy.NullId;
}

pub inline fn buildReturnInfo2(numRetVals: u8, comptime cont: bool) linksection(cy.HotSection) Value {
    return Value{
        .retInfo = .{
            .numRetVals = numRetVals,
            // .retFlag = if (cont) 0 else 1,
            .retFlag = !cont,
        },
    };
}

pub inline fn buildReturnInfo(comptime numRetVals: u2, comptime cont: bool) linksection(cy.HotSection) Value {
    return Value{
        .retInfo = .{
            .numRetVals = numRetVals,
            // .retFlag = if (cont) 0 else 1,
            .retFlag = !cont,
        },
    };
}

pub inline fn pcOffset(vm: *const VM, pc: [*]const cy.OpData) u32 {
    return @intCast(u32, @ptrToInt(pc) - @ptrToInt(vm.ops.ptr));
}

pub inline fn framePtrOffsetFrom(stackPtr: [*]const Value, framePtr: [*]const Value) usize {
    // Divide by eight.
    return (@ptrToInt(framePtr) - @ptrToInt(stackPtr)) >> 3;
}

pub inline fn framePtrOffset(framePtr: [*]const Value) usize {
    // Divide by eight.
    return (@ptrToInt(framePtr) - @ptrToInt(gvm.stack.ptr)) >> 3;
}

pub inline fn toFramePtr(offset: usize) [*]Value {
    return @ptrCast([*]Value, &gvm.stack[offset]);
}

pub const PcFramePtr = struct {
    pc: [*]cy.OpData,
    framePtr: [*]Value,
};

fn boxValueRetain(vm: *VM, box: Value) linksection(cy.HotSection) Value {
    @setCold(true);
    if (box.isPointer()) {
        const obj = stdx.ptrAlignCast(*HeapObject, box.asPointer().?);
        if (builtin.mode == .Debug) {
            std.debug.assert(obj.common.structId == cy.BoxS);
        }
        retain(vm, obj.box.val);
        return obj.box.val;
    } else {
        // Box can be none if used before captured var was initialized.
        if (builtin.mode == .Debug) {
            std.debug.assert(box.isNone());
        }
        return Value.None;
    }
}

fn setCapValToFuncSyms(vm: *VM, capVal: Value, numSyms: u8, syms: []const cy.OpData) void {
    @setCold(true);
    var i: u32 = 0;
    while (i < numSyms) : (i += 1) {
        const capVarIdx = syms[i * 2 + 1].arg;
        const sym = vm.funcSyms.buf[syms[i*2].arg];
        const ptr = sym.inner.closure.getCapturedValuesPtr();
        ptr[capVarIdx] = capVal;
    }
    cy.arc.retainInc(vm, capVal, numSyms);
}

/// Like Value.dump but shows heap values.
pub fn dumpValue(vm: *const VM, val: Value) void {
    if (val.isNumber()) {
        fmt.printStdout("Number {}\n", &.{ v(val.asF64()) });
    } else {
        if (val.isPointer()) {
            const obj = stdx.ptrAlignCast(*cy.HeapObject, val.asPointer().?);
            switch (obj.common.structId) {
                cy.ListS => fmt.printStdout("List {} len={}\n", &.{v(obj), v(obj.list.list.len)}),
                cy.MapS => fmt.printStdout("Map {} size={}\n", &.{v(obj), v(obj.map.inner.size)}),
                cy.AstringT => {
                    const str = obj.astring.getConstSlice();
                    if (str.len > 20) {
                        fmt.printStdout("String {} len={} str=\"{}\"...\n", &.{v(obj), v(str.len), v(str[0..20])});
                    } else {
                        fmt.printStdout("String {} len={} str=\"{}\"\n", &.{v(obj), v(str.len), v(str)});
                    }
                },
                cy.UstringT => {
                    const str = obj.ustring.getConstSlice();
                    if (str.len > 20) {
                        fmt.printStdout("String {} len={} str=\"{}\"...\n", &.{v(obj), v(str.len), v(str[0..20])});
                    } else {
                        fmt.printStdout("String {} len={} str=\"{}\"\n", &.{v(obj), v(str.len), v(str)});
                    }
                },
                cy.LambdaS => fmt.printStdout("Lambda {}\n", &.{v(obj)}),
                cy.ClosureS => fmt.printStdout("Closure {}\n", &.{v(obj)}),
                cy.FiberS => fmt.printStdout("Fiber {}\n", &.{v(obj)}),
                cy.NativeFunc1S => fmt.printStdout("NativeFunc {}\n", &.{v(obj)}),
                else => {
                    fmt.printStdout("HeapObject {} {} {}\n", &.{v(obj), v(obj.common.structId), v(vm.structs.buf[obj.common.structId].name)});
                },
            }
        } else {
            switch (val.getTag()) {
                cy.NoneT => {
                    fmt.printStdout("None\n", &.{});
                },
                cy.StaticUstringT,
                cy.StaticAstringT => {
                    const slice = val.asStaticStringSlice();
                    if (slice.len() > 20) {
                        fmt.printStdout("Const String len={} str=\"{s}\"...\n", &.{v(slice.len()), v(vm.strBuf[slice.start..20])});
                    } else {
                        fmt.printStdout("Const String len={} str=\"{}\"\n", &.{v(slice.len()), v(vm.strBuf[slice.start..slice.end])});
                    }
                },
                else => {
                    fmt.printStdout("{}\n", &.{v(val.val)});
                },
            }
        }
    }
}

const RelFuncSigKey = KeyU64;

pub const KeyU96 = extern union {
    val: extern struct {
        a: u64,
        b: u32,
    },
    absLocalSymKey: extern struct {
        localParentSymId: u32,
        nameId: u32,
        numParams: u32,
    },
    rtFuncSymKey: extern struct {
        // TODO: Is it enough to just use the final resolved func sym id?
        resolvedParentSymId: u32,
        nameId: u32,
        numParams: u32,
    },
};

pub const KeyU96Context = struct {
    pub fn hash(_: @This(), key: KeyU96) u64 {
        var hasher = std.hash.Wyhash.init(0);
        @call(.always_inline, hasher.update, .{std.mem.asBytes(&key.val.a)});
        @call(.always_inline, hasher.update, .{std.mem.asBytes(&key.val.b)});
        return hasher.final();
    }
    pub fn eql(_: @This(), a: KeyU96, b: KeyU96) bool {
        return a.val.a == b.val.a and a.val.b == b.val.b;
    }
};

pub const KeyU64 = extern union {
    val: u64,
    absResolvedSymKey: extern struct {
        resolvedParentSymId: u32,
        nameId: u32,
    },
    absResolvedFuncSymKey: extern struct {
        resolvedSymId: sema.ResolvedSymId,
        numParams: u32,
    },
    relModuleSymKey: extern struct {
        nameId: u32,
        numParams: u32,
    },
    rtVarSymKey: extern struct {
        resolvedParentSymId: u32,
        nameId: u32,
    },
    relFuncSigKey: extern struct {
        nameId: u32,
        numParams: u32,
    },
    structKey: extern struct {
        nameId: u32,
        uniqId: u32,
    },
};

pub const KeyU64Context = struct {
    pub fn hash(_: @This(), key: KeyU64) linksection(cy.Section) u64 {
        return std.hash.Wyhash.hash(0, std.mem.asBytes(&key.val));
    }
    pub fn eql(_: @This(), a: KeyU64, b: KeyU64) linksection(cy.Section) bool {
        return a.val == b.val;
    }
};

/// Absolute func symbol signature key.
pub const AbsFuncSigKey = KeyU96;

/// Absolute var signature key.
const AbsVarSigKey = KeyU64;

const SliceWriter = struct {
    buf: []u8,
    idx: *u32,

    pub const Error = error{OutOfMemory};

    fn reset(self: *SliceWriter) void {
        self.idx.* = 0;
    }

    inline fn pos(self: *const SliceWriter) u32 {
        return self.idx.*;
    }

    inline fn sliceFrom(self: *const SliceWriter, start: u32) []const u8 {
        return self.buf[start..self.idx.*];
    }

    pub fn write(self: SliceWriter, data: []const u8) linksection(cy.Section) Error!usize {
        if (builtin.mode != .ReleaseFast) {
            if (self.idx.* + data.len > self.buf.len) {
                return Error.OutOfMemory;
            }
        }
        std.mem.copy(u8, self.buf[self.idx.*..self.idx.*+data.len], data);
        self.idx.* += @intCast(u32, data.len);
        return data.len;
    }

    pub fn writeAll(self: SliceWriter, data: []const u8) linksection(cy.Section) Error!void {
        _ = try self.write(data);
    }

    pub fn writeByteNTimes(self: SliceWriter, byte: u8, n: usize) linksection(cy.Section) Error!void {
        if (builtin.mode != .ReleaseFast) {
            if (self.idx.* + n > self.buf.len) {
                return Error.OutOfMemory;
            }
        }
        std.mem.set(u8, self.buf[self.idx.*..self.idx.*+n], byte);
        self.idx.* += @intCast(u32, n);
    }
};

pub const EvalConfig = struct {
    /// Whether this process intends to perform eval once and exit.
    /// In that scenario, the compiler can skip generating the final release ops for the main block.
    singleRun: bool = false,
};

fn opMatch(vm: *const VM, pc: [*]const cy.OpData, framePtr: [*]const Value) u16 {
    const expr = framePtr[pc[1].arg];
    const numCases = pc[2].arg;
    var i: u32 = 0;
    while (i < numCases) : (i += 1) {
        const right = framePtr[pc[3 + i * 3].arg];
        // Can immediately match numbers, objects, primitives.
        const cond = if (expr.val == right.val) true else 
            @call(.never_inline, evalCompareBool, .{vm, expr, right});
        if (cond) {
            // Jump.
            return @ptrCast(*const align (1) u16, pc + 4 + i * 3).*;
        }
    }
    // else case
    return @ptrCast(*const align (1) u16, pc + 4 + i * 3 - 1).*;
}

fn setStaticFunc(vm: *VM, symId: SymbolId, val: Value) linksection(cy.Section) !void {
    errdefer {
        // TODO: This should be taken care of by panic stack unwinding.
        cy.arc.release(vm, val);
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject(*cy.HeapObject);
        switch (obj.common.structId) {
            cy.NativeFunc1S => {
                const reqNumParams = getFuncSymSig(vm, symId);
                if (reqNumParams != obj.nativeFunc1.numParams) {
                    return vm.panic("Assigning to static function with a different function signature.");
                }
                if (vm.funcSyms.buf[symId].entryT == @enumToInt(FuncSymbolEntryType.nativeFunc1)) {
                    // Check to cleanup previous tcc state.
                    if (vm.funcSymTccStates.get(symId)) |state| {
                        release(vm, state);
                    }
                }
                vm.funcSyms.buf[symId] = .{
                    .entryT = @enumToInt(FuncSymbolEntryType.nativeFunc1),
                    .innerExtra = .{
                        .nativeFunc1 = .{
                            .numParams = reqNumParams,
                        }
                    },
                    .inner = .{
                        .nativeFunc1 = obj.nativeFunc1.func,
                    },
                };
                if (obj.nativeFunc1.hasTccState) {
                    retain(vm, obj.nativeFunc1.tccState);
                    vm.funcSymTccStates.put(vm.alloc, symId, obj.nativeFunc1.tccState) catch stdx.fatal();
                }
                cy.arc.release(vm, val);
            },
            else => {
                return vm.panicFmt("Assigning to static function with unsupported type {}.", &.{v(obj.common.structId)});
            }
        }
    } else {
        return vm.panic("Assigning to static function with a non-function value.");
    }
}

fn getFuncSymSig(vm: *const VM, symId: SymbolId) u32 {
    switch (@intToEnum(FuncSymbolEntryType, vm.funcSyms.buf[symId].entryT)) {
        .nativeFunc1 => {
            return vm.funcSyms.buf[symId].innerExtra.nativeFunc1.numParams;
        },
        .func => {
            return vm.funcSyms.buf[symId].inner.func.numParams;
        },
        .closure => {
            return vm.funcSyms.buf[symId].inner.closure.numParams;
        },
        .none => {
            return vm.funcSyms.buf[symId].innerExtra.none.numParams;
        },
    }
}