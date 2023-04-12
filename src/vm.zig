const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const aarch64 = builtin.cpu.arch == .aarch64;
const stdx = @import("stdx");
const fatal = stdx.fatal;
const t = stdx.testing;
const tcc = @import("tcc");

const vmc = @import("vm_c.zig");
const fmt = @import("fmt.zig");
const v = fmt.v;
const cy = @import("cyber.zig");
const rt = cy.rt;
const sema = cy.sema;
const types = cy.types;
const bindings = @import("builtins/bindings.zig");
const math_mod = @import("builtins/math.zig");
const Value = cy.Value;
const debug = @import("debug.zig");
const http = @import("http.zig");
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

    /// Program counter. Pointer to the current instruction data in `ops`.
    pc: [*]cy.InstDatum,
    /// Current stack frame ptr.
    framePtr: [*]Value,

    /// Value stack.
    stack: []Value,
    stackEndPtr: [*]const Value,

    ops: []cy.InstDatum,
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
    /// Tail is only used in debug mode.
    heapFreeTail: if (builtin.mode == .Debug) ?*HeapObject else void,

    tryStack: cy.List(cy.fiber.TryFrame),

    refCounts: if (TrackGlobalRC) usize else void,

    /// Symbol table used to lookup object methods.
    /// A `SymbolId` indexes into `methodSyms`. If the `mruStructId` matches it uses `mruSym`.
    /// Otherwise, the sym is looked up from the hashmap `methodTable`.
    methodSyms: cy.List(rt.MethodSym),
    methodTable: std.HashMapUnmanaged(rt.MethodTableKey, rt.MethodEntry, cy.hash.KeyU64Context, 80),

    /// Maps a method signature to a symbol id in `methodSyms`.
    methodSymSigs: std.HashMapUnmanaged(rt.MethodKey, rt.MethodId, cy.hash.KeyU64Context, 80),

    /// Regular function symbol table.
    funcSyms: cy.List(FuncSymbolEntry),
    funcSymSigs: std.HashMapUnmanaged(AbsFuncSigKey, SymbolId, KeyU96Context, 80),
    funcSymDetails: cy.List(FuncSymDetail),

    varSyms: cy.List(VarSym),
    varSymSigs: std.HashMapUnmanaged(rt.VarKey, SymbolId, cy.hash.KeyU64Context, 80),

    /// Struct fields symbol table.
    fieldSyms: cy.List(FieldSymbolMap),
    fieldTable: std.HashMapUnmanaged(rt.FieldTableKey, FieldEntry, cy.hash.KeyU64Context, 80),
    fieldSymSignatures: std.StringHashMapUnmanaged(rt.FieldId),

    /// Types.
    types: cy.List(rt.Type),
    typeSignatures: std.HashMapUnmanaged(rt.TypeKey, rt.TypeId, cy.hash.KeyU64Context, 80),

    /// Enums.
    enums: cy.List(Enum),
    enumSignatures: std.StringHashMapUnmanaged(EnumId),

    /// Symbols.
    syms: cy.List(Symbol),
    symSignatures: std.StringHashMapUnmanaged(SymbolId),

    u8Buf: cy.ListAligned(u8, 8),
    u8Buf2: cy.ListAligned(u8, 8),

    stackTrace: cy.StackTrace,

    /// Since func syms can be reassigned, track any RC dependencies.
    funcSymDeps: std.AutoHashMapUnmanaged(SymbolId, Value),
    methodSymExtras: cy.List(MethodSymExtra),
    debugTable: []const cy.DebugSym,

    curFiber: *cy.Fiber,
    mainFiber: cy.Fiber,

    iteratorObjSym: SymbolId,
    pairIteratorObjSym: SymbolId,
    nextObjSym: SymbolId,
    nextPairObjSym: SymbolId,

    /// Local to be returned back to eval caller.
    /// 255 indicates no return value.
    endLocal: u8,

    panicType: debug.PanicType,
    panicPayload: debug.PanicPayload,

    /// Records a minimal trace during stack unwinding.
    /// Only includes frames visited during stack unwinding.
    /// Further frames would need to be queried when building a full stack trace.
    throwTrace: cy.List(cy.debug.CompactFrame),

    /// Interface used for imports and fetch.
    httpClient: http.HttpClient,
    stdHttpClient: if (!cy.isWasm) http.StdHttpClient else void,

    trace: if (TraceEnabled) *TraceInfo else void,

    /// Object to pc of instruction that allocated it.
    objectTraceMap: if (builtin.mode == .Debug) std.AutoHashMapUnmanaged(*HeapObject, debug.ObjectTrace) else void,

    /// In debug mode, always save the current pc so tracing can be obtained.
    /// debugPc == NullId indicates execution has not started.
    debugPc: if (builtin.mode == .Debug) u32 else void,

    config: EvalConfig,

    /// Whether this VM is already deinited. Used to skip the next deinit to avoid using undefined memory.
    deinited: bool,
    deinitedRtObjects: bool,

    /// User data ptr. Useful for embedders.
    userData: ?*anyopaque,

    compiler: cy.VMcompiler,

    lastError: ?error{TokenError, ParseError, CompileError, Panic},

    expGlobalRC: usize,

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
            .heapFreeTail = if (builtin.mode == .Debug) null else undefined,
            .pc = undefined,
            .framePtr = undefined,
            .tryStack = .{},
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
            .types = .{},
            .typeSignatures = .{},
            .enums = .{},
            .enumSignatures = .{},
            .syms = .{},
            .symSignatures = .{},
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
            .throwTrace = .{},
            .mainFiber = undefined,
            .curFiber = undefined,
            .endLocal = undefined,
            .objectTraceMap = if (builtin.mode == .Debug) .{} else undefined,
            // Initialize to NullId to indicate vm is still in initing.
            .debugPc = if (builtin.mode == .Debug) cy.NullId else undefined,
            .deinited = false,
            .deinitedRtObjects = false,
            .funcSymDeps = .{},
            .config = undefined,
            .httpClient = undefined,
            .stdHttpClient = undefined,
            .lastError = null,
            .userData = null,
            .expGlobalRC = 0,
        };
        // Pointer offset from gvm to avoid deoptimization.
        self.curFiber = &gvm.mainFiber;
        try self.compiler.init(self);

        if (!cy.isWasm) {
            self.stdHttpClient = http.StdHttpClient.init(self.alloc);
            self.httpClient = self.stdHttpClient.iface();
        }

        // Perform decently sized allocation for hot data paths since the allocator
        // will likely use a more consistent allocation.
        // Also try to allocate them in the same bucket.
        try cy.fiber.stackEnsureTotalCapacityPrecise(self, 511);
        try self.methodTable.ensureTotalCapacity(self.alloc, 96);

        try self.funcSyms.ensureTotalCapacityPrecise(self.alloc, 255);
        try self.methodSyms.ensureTotalCapacityPrecise(self.alloc, 170);

        try self.types.ensureTotalCapacityPrecise(alloc, 170);
        try self.fieldSyms.ensureTotalCapacityPrecise(alloc, 170);

        // Initialize heap.
        const list = try cy.heap.growHeapPages(self, 1);
        self.heapFreeHead = list.head;
        if (builtin.mode == .Debug) {
            self.heapFreeTail = list.tail;
        }

        // Force linksection order. Using `try` makes this work.
        try @call(.never_inline, cy.forceSectionDeps, .{});
    }

    pub fn deinitRtObjects(self: *VM) void {
        if (self.deinitedRtObjects) {
            return;
        }
        for (self.funcSyms.items()) |sym| {
            if (sym.entryT == @enumToInt(FuncSymbolEntryType.closure)) {
                cy.arc.releaseObject(self, @ptrCast(*HeapObject, sym.inner.closure));
            }
        }

        for (self.varSyms.items()) |vsym| {
            release(self, vsym.value);
        }

        {
            var iter = self.funcSymDeps.iterator();
            while (iter.next()) |e| {
                release(self, e.value_ptr.*);
            }
        }
        self.deinitedRtObjects = true;
    }

    pub fn deinit(self: *VM, comptime reset: bool) void {
        if (self.deinited) {
            return;
        }

        debug.freePanicPayload(self);

        // Deinit runtime related resources first, since they may depend on
        // compiled/debug resources.
        self.deinitRtObjects();

        // Deinit compiler first since it depends on buffers from parser.
        if (reset) {
            self.compiler.deinit(true);
        } else {
            self.compiler.deinit(false);
        }

        if (reset) {
            // `stack` is kept with previous size.
        } else {
            self.alloc.free(self.stack);
            self.stack = &.{};
        }

        if (reset) {
            self.tryStack.clearRetainingCapacity();
            self.throwTrace.clearRetainingCapacity();
        } else {
            self.tryStack.deinit(self.alloc);
            self.throwTrace.deinit(self.alloc);
        }

        for (self.methodSymExtras.items()) |extra| {
            if (extra.nameIsOwned) {
                self.alloc.free(extra.getName());
            }
        }
        if (reset) {
            self.methodSyms.clearRetainingCapacity();
            self.methodSymExtras.clearRetainingCapacity();
            self.methodSymSigs.clearRetainingCapacity();
            self.methodTable.clearRetainingCapacity();
        } else {
            self.methodSyms.deinit(self.alloc);
            self.methodSymExtras.deinit(self.alloc);
            self.methodSymSigs.deinit(self.alloc);
            self.methodTable.deinit(self.alloc);
        }

        for (self.funcSymDetails.items()) |detail| {
            self.alloc.free(detail.name);
        }
        if (reset) {
            self.funcSyms.clearRetainingCapacity();
            self.funcSymSigs.clearRetainingCapacity();
            self.funcSymDetails.clearRetainingCapacity();
            self.funcSymDeps.clearRetainingCapacity();
        } else {
            self.funcSyms.deinit(self.alloc);
            self.funcSymSigs.deinit(self.alloc);
            self.funcSymDetails.deinit(self.alloc);
            self.funcSymDeps.deinit(self.alloc);
        }

        if (reset) {
            self.varSyms.clearRetainingCapacity();
            self.varSymSigs.clearRetainingCapacity();
        } else {
            self.varSyms.deinit(self.alloc);
            self.varSymSigs.deinit(self.alloc);
        }

        if (reset) {
            self.fieldSyms.clearRetainingCapacity();
            self.fieldTable.clearRetainingCapacity();
            self.fieldSymSignatures.clearRetainingCapacity();
        } else {
            self.fieldSyms.deinit(self.alloc);
            self.fieldTable.deinit(self.alloc);
            self.fieldSymSignatures.deinit(self.alloc);
        }

        if (reset) {
            // Does not clear heap pages so objects can persist.
        } else {
            for (self.heapPages.items()) |page| {
                self.alloc.destroy(page);
            }
            self.heapPages.deinit(self.alloc);
        }

        if (reset) {
            self.types.clearRetainingCapacity();
            self.typeSignatures.clearRetainingCapacity();
        } else {
            self.types.deinit(self.alloc);
            self.typeSignatures.deinit(self.alloc);
        }

        for (self.syms.items()) |sym| {
            if (sym.nameOwned) {
                self.alloc.free(sym.name);
            }
        }
        for (self.enums.items()) |sym| {
            self.alloc.free(sym.members);
        }
        if (reset) {
            self.enums.clearRetainingCapacity();
            self.enumSignatures.clearRetainingCapacity();
            self.syms.clearRetainingCapacity();
            self.symSignatures.clearRetainingCapacity();
        } else {
            self.enums.deinit(self.alloc);
            self.enumSignatures.deinit(self.alloc);
            self.syms.deinit(self.alloc);
            self.symSignatures.deinit(self.alloc);
        }

        self.stackTrace.deinit(self.alloc);
        if (reset) {
            self.u8Buf.clearRetainingCapacity();
            self.u8Buf2.clearRetainingCapacity();
            self.strInterns.clearRetainingCapacity();
        } else {
            self.u8Buf.deinit(self.alloc);
            self.u8Buf2.deinit(self.alloc);
            self.strInterns.deinit(self.alloc);
        }

        if (builtin.mode == .Debug) {
            if (reset) {
                self.objectTraceMap.clearRetainingCapacity();
            } else {
                self.objectTraceMap.deinit(self.alloc);
            }
        }

        if (!reset) {
            if (!cy.isWasm) {
                self.stdHttpClient.deinit();
            }
        }

        self.deinited = true;
    }

    pub fn validate(self: *VM, srcUri: []const u8, src: []const u8, config: cy.ValidateConfig) !ValidateResult {
        const res = try self.compile(srcUri, src, .{
            .enableFileModules = config.enableFileModules,
            .skipCodegen = true,
        });
        return ValidateResult{
            .err = res.err,
        };
    }

    pub fn compile(self: *VM, srcUri: []const u8, src: []const u8, config: cy.CompileConfig) !cy.CompileResultView {
        try self.resetVM();
        self.config = .{
            .singleRun = false,
            .enableFileModules = config.enableFileModules,
            .genAllDebugSyms = true,
        };
        var tt = stdx.debug.trace();
        const res = try self.compiler.compile(srcUri, src, .{});
        if (res.err) |err| {
            switch (err) {
                .tokenize => {
                    self.lastError = error.TokenError;
                },
                .parse => {
                    self.lastError = error.ParseError;
                },
                .compile => {
                    self.lastError = error.CompileError;
                },
            }
        }
        tt.endPrint("compile");
        return res;
    }

    fn resetVM(self: *VM) !void {
        self.deinit(true);

        // Reset flags for next reset/deinit.
        self.deinitedRtObjects = false;
        self.deinited = false;

        // Before reinit, everything in VM, VMcompiler should be cleared.

        try self.compiler.reinit();

        // Core bindings.
        try @call(.never_inline, bindings.bindCore, .{self});
    }

    pub fn eval(self: *VM, srcUri: []const u8, src: []const u8, config: EvalConfig) !Value {
        try self.resetVM();
        self.config = config;
        var tt = stdx.debug.trace();
        const res = try self.compiler.compile(srcUri, src, .{});
        if (res.err) |err| {
            switch (err) {
                .tokenize => {
                    self.lastError = error.TokenError;
                    return error.TokenError;
                },
                .parse => {
                    self.lastError = error.ParseError;
                    return error.ParseError;
                },
                .compile => {
                    self.lastError = error.CompileError;
                    return error.CompileError;
                },
            }
        }
        tt.endPrint("compile");

        if (TraceEnabled) {
            if (builtin.is_test and debug.atLeastTestDebugLevel()) {
                try debug.dumpBytecode(self, null);
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
                try debug.dumpBytecode(self, null);
            }
        }

        tt = stdx.debug.trace();
        defer {
            tt.endPrint("eval");
            if (TraceEnabled) {
                if (!builtin.is_test or debug.atLeastTestDebugLevel()) {
                    self.dumpInfo() catch fatal();
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

    pub fn dumpInfo(self: *VM) !void {
        fmt.printStderr("stack size: {}\n", &.{v(self.stack.len)});
        fmt.printStderr("stack framePtr: {}\n", &.{v(getStackOffset(self, self.framePtr))});
        fmt.printStderr("heap pages: {}\n", &.{v(self.heapPages.len)});
        if (cy.TrackGlobalRC) {
            fmt.printStderr("global rc: {}\n", &.{v(self.refCounts)});
        }

        // Dump object symbols.
        {
            fmt.printStderr("obj syms:\n", &.{});
            var iter = self.funcSymSigs.iterator();
            while (iter.next()) |it| {
                const key = it.key_ptr.*;
                const name = sema.getName(&self.compiler, key.rtFuncSymKey.nameId);
                if (key.rtFuncSymKey.rFuncSigId == cy.NullId) {
                    fmt.printStderr("\t{}: {}\n", &.{v(name), v(it.value_ptr.*)});
                } else {
                    const sigStr = try sema.getResolvedFuncSigTempStr(&self.compiler, key.rtFuncSymKey.rFuncSigId);
                    fmt.printStderr("\t{}{}: {}\n", &.{v(name), v(sigStr), v(it.value_ptr.*)});
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
        self.pc = @ptrCast([*]cy.InstDatum, buf.ops.items.ptr);
        try cy.fiber.stackEnsureTotalCapacity(self, buf.mainStackSize);
        self.framePtr = @ptrCast([*]Value, self.stack.ptr);

        self.ops = buf.ops.items;
        self.consts = buf.mconsts;
        self.strBuf = buf.strBuf.items;

        @call(.never_inline, evalLoopGrowStack, .{self}) catch |err| {
            if (err == error.Panic) {
                self.lastError = error.Panic;
                try cy.fiber.unwindReleaseStack(self, self.stack, self.framePtr, self.pc);
            }
            return err;
        };
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
            const obj = recv.asHeapObject();
            const res = switch (obj.head.typeId) {
                rt.ListT => {
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
                        return self.interruptThrowSymbol(.OutOfBounds);
                    }
                    if (end < start or end > list.len) {
                        return self.interruptThrowSymbol(.OutOfBounds);
                    }

                    const elems = list.buf[@intCast(u32, start) .. @intCast(u32, end)];
                    for (elems) |elem| {
                        retain(self, elem);
                    }
                    return cy.heap.allocList(self, elems);
                },
                rt.AstringT => b: {
                    retainObject(self, obj);
                    break :b bindings.stringSlice(.astring)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2);
                },
                rt.UstringT => b: {
                    retainObject(self, obj);
                    break :b bindings.stringSlice(.ustring)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2);
                },
                rt.StringSliceT => b: {
                    retainObject(self, obj);
                    break :b bindings.stringSlice(.slice)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2);
                },
                rt.RawstringT => b: {
                    retainObject(self, obj);
                    break :b bindings.stringSlice(.rawstring)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2);
                },
                rt.RawstringSliceT => b: {
                    retainObject(self, obj);
                    break :b bindings.stringSlice(.rawSlice)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2);
                },
                else => {
                    return self.panicFmt("Unsupported slice operation on type `{}`.", &.{v(self.types.buf[obj.head.typeId].name)});
                },
            };
            if (res.isInterrupt()) {
                return error.Panic;
            }
            return res;
        } else {
            if (recv.isNumber()) {
                return self.panic("Unsupported slice operation on type `number`.");
            } else {
                const res = switch (recv.getTag()) {
                    rt.StaticAstringT => bindings.stringSlice(.staticAstring)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2),
                    rt.StaticUstringT => bindings.stringSlice(.staticUstring)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2),
                    else => {
                        return self.panicFmt("Unsupported slice operation on type `{}`.", &.{v(@intCast(u8, recv.getTag()))});
                    },
                };
                if (res.isInterrupt()) {
                    return error.Panic;
                }
                return res;
            }
        }
    }

    pub fn allocLastErrorReport(self: *VM) ![]const u8 {
        if (self.lastError) |err| {
            switch (err) {
                error.TokenError => return debug.allocLastUserParseError(self),
                error.ParseError => return debug.allocLastUserParseError(self),
                error.CompileError => return debug.allocLastUserCompileError(self),
                error.Panic => return debug.allocLastUserPanicError(self),
            }
        } else {
            return error.NoLastError;
        }
    }

    pub fn ensureEnum(self: *VM, name: []const u8) !EnumId {
        const res = try self.enumSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            return self.addEnum(name);
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn addAnonymousStruct(self: *VM, baseName: []const u8) !rt.TypeId {
        const name = try self.alloc.alloc(u8, baseName.len + 16);
        defer self.alloc.free(name);

        var attempts: u32 = 0;
        while (attempts < 100): (attempts += 1) {
            const next = math_mod.rand.next();
            std.mem.copy(u8, name[0..baseName.len], baseName);
            _ = std.fmt.formatIntBuf(name[baseName.len..], next, 16, .lower, .{ .width = 16, .fill = '0'});
            const nameId = try cy.sema.ensureNameSymExt(&self.compiler, name, true);
            const key = rt.TypeKey.initTypeKey(cy.NullId, nameId);
            if (!self.typeSignatures.contains(key)) {
                return try self.addObjectTypeExt(cy.NullId, nameId, baseName, cy.NullId);
            }
        }
        return error.TooManyAttempts;
    }

    pub fn ensureObjectType(
        self: *VM, rParentSymId: sema.ResolvedSymId, nameId: sema.NameSymId, rSymId: sema.ResolvedSymId
    ) !rt.TypeId {
        const key = rt.TypeKey.initTypeKey(rParentSymId, nameId);
        const res = try @call(.never_inline, self.typeSignatures.getOrPut, .{self.alloc, key });
        if (!res.found_existing) {
            const name = sema.getName(&self.compiler, nameId);
            return self.addObjectTypeExt(rParentSymId, nameId, name, rSymId);
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn getStructFieldIdx(self: *const VM, typeId: rt.TypeId, propName: []const u8) ?u32 {
        const fieldId = self.fieldSymSignatures.get(propName) orelse return null;
        const entry = &self.fieldSyms.buf[fieldId];

        if (entry.mruTypeId == typeId) {
            return entry.mruOffset;
        } else {
            const key = rt.FieldTableKey.initFieldTableKey(typeId, fieldId);
            const res = self.fieldTable.get(key) orelse return null;
            entry.mruTypeId = typeId;
            entry.mruOffset = res.offset;
            entry.mruFieldTypeSymId = res.typeSymId;
            return res.offset;
        }
    }

    pub fn addEnum(self: *VM, name: []const u8) !EnumId {
        const s = Enum{
            .name = name,
            .members = &.{},
        };
        const id = @intCast(u32, self.enums.len);
        try self.enums.append(self.alloc, s);
        try self.enumSignatures.put(self.alloc, name, id);
        return id;
    }

    pub inline fn getObjectTypeId(
        self: *const VM, rParentSymId: sema.ResolvedSymId, nameId: sema.NameSymId
    ) ?rt.TypeId {
        const key = rt.TypeKey.initTypeKey(rParentSymId, nameId);
        return self.typeSignatures.get(key);
    }

    /// Can provide a different display name.
    pub fn addObjectTypeExt(
        self: *VM, rParentSymId: sema.ResolvedSymId,
        nameId: sema.NameSymId, name: []const u8, rTypeSymId: sema.ResolvedSymId
    ) !rt.TypeId {
        const s = rt.Type{
            .name = name,
            .numFields = 0,
            .rTypeSymId = rTypeSymId,
        };
        const id = @intCast(u32, self.types.len);
        try self.types.append(self.alloc, s);
        const key = rt.TypeKey.initTypeKey(rParentSymId, nameId);
        try self.typeSignatures.put(self.alloc, key, id);
        return id;
    }

    pub fn addBuiltinType(self: *VM, name: []const u8) !rt.TypeId {
        const nameId = try sema.ensureNameSym(&self.compiler, name);
        return self.addObjectTypeExt(cy.NullId, nameId, name, cy.NullId);
    }

    pub inline fn getFuncSym(self: *const VM, rParentSymId: u32, nameId: u32, rFuncSigId: sema.ResolvedFuncSigId) ?SymbolId {
        const key = AbsFuncSigKey{
            .rtFuncSymKey = .{
                .rParentSymId = rParentSymId,
                .nameId = nameId,
                .rFuncSigId = rFuncSigId,
            },
        };
        return self.funcSymSigs.get(key);
    }

    pub inline fn getVarSym(self: *const VM, rParentSymId: u32, nameId: u32) ?SymbolId {
        const key = rt.VarKey.initVarKey(rParentSymId, nameId);
        return self.varSymSigs.get(key);
    }

    pub fn ensureVarSym(self: *VM, rParentSymId: sema.ResolvedSymId, nameId: u32) !SymbolId {
        const key = rt.VarKey.initVarKey(rParentSymId, nameId);
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
    
    pub fn ensureFuncSym(self: *VM, rParentSymId: sema.ResolvedSymId, nameId: u32, rFuncSigId: sema.ResolvedFuncSigId) !SymbolId {
        const key = RtFuncSymKey{
            .rtFuncSymKey = .{
                .rParentSymId = rParentSymId,
                .nameId = nameId,
                .rFuncSigId = rFuncSigId,
            },
        };
        const res = try self.funcSymSigs.getOrPut(self.alloc, key);
        if (!res.found_existing) {
            const id = @intCast(u32, self.funcSyms.len);

            // const rFuncSig = self.compiler.sema.resolvedFuncSigs.items[rFuncSigId];
            // const typedFlag: u16 = if (rFuncSig.isTyped) 1 << 15 else 0;
            try self.funcSyms.append(self.alloc, .{
                .entryT = @enumToInt(FuncSymbolEntryType.none),
                .innerExtra = .{
                    .none = .{
                        // .typedFlagNumParams = typedFlag | rFuncSig.numParams(),
                        // .rFuncSigId = @intCast(u16, rFuncSigId),
                        .rFuncSigId = rFuncSigId,
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

    pub fn getSymbolName(self: *const VM, id: u32) []const u8 {
        return self.syms.buf[id].name;
    }

    pub fn ensureSymbol(self: *VM, name: []const u8) !SymbolId {
        return self.ensureSymbolExt(name, false);
    }

    pub fn ensureSymbolExt(self: *VM, name: []const u8, owned: bool) !SymbolId {
        const res = try self.symSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            const id = @intCast(u32, self.syms.len);
            var effName = name;
            if (owned) {
                effName = try self.alloc.dupe(u8, name);
            }
            try self.syms.append(self.alloc, .{
                .symT = .empty,
                .inner = undefined,
                .name = effName,
                .nameOwned = owned,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn ensureFieldSym(self: *VM, name: []const u8) !SymbolId {
        const nameId = try sema.ensureNameSym(&self.compiler, name);
        const res = try self.fieldSymSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            const id = @intCast(u32, self.fieldSyms.len);
            try self.fieldSyms.append(self.alloc, .{
                .mruTypeId = cy.NullId,
                .mruOffset = undefined,
                .mruFieldTypeSymId = undefined,
                .nameId = nameId,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn hasMethodSym(self: *const VM, sid: rt.TypeId, methodId: SymbolId) bool {
        const map = self.methodSyms.buf[methodId];
        if (map.mapT == .one) {
            return map.inner.one.id == sid;
        }
        return false;
    }

    pub fn ensureMethodSym(self: *VM, name: []const u8, numParams: u32) !SymbolId {
        return self.ensureMethodSym2(name, numParams, false);
    }

    pub fn ensureMethodSym2(self: *VM, name: []const u8, numParams: u32, dupeName: bool) !SymbolId {
        const nameId = try sema.ensureNameSym(&self.compiler, name);
        const key = rt.MethodKey.initMethodKey(nameId, numParams);
        const res = try @call(.never_inline, self.methodSymSigs.getOrPut, .{self.alloc, key});
        if (!res.found_existing) {
            const id = @intCast(u32, self.methodSyms.len);

            try self.methodSyms.append(self.alloc, .{
                .entryT = undefined,
                .mruTypeId = cy.NullId,
                .rFuncSigId = cy.NullId,
                .inner = undefined,
            });
            var newName = name;
            if (dupeName) {
                newName = try self.alloc.dupe(u8, name);
            }
            try self.methodSymExtras.append(self.alloc, .{
                .namePtr = newName.ptr,
                .nameLen = @intCast(u32, newName.len),
                .nameIsOwned = dupeName,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn addFieldSym(self: *VM, sid: rt.TypeId, symId: SymbolId, offset: u16, typeSymId: sema.ResolvedSymId) !void {
        const sym = &self.fieldSyms.buf[symId];
        if (sym.mruTypeId != cy.NullId) {
            // Add prev mru if it doesn't exist in hashmap.
            const prev = rt.FieldTableKey.initFieldTableKey(sym.mruTypeId, symId);
            if (!self.fieldTable.contains(prev)) {
                try self.fieldTable.putNoClobber(self.alloc, prev, .{
                    .offset = sym.mruOffset,
                    .typeSymId = sym.mruFieldTypeSymId,
                });
            }
            const key = rt.FieldTableKey.initFieldTableKey(sid, symId);
            try self.fieldTable.putNoClobber(self.alloc, key, .{
                .offset = offset,
                .typeSymId = typeSymId,
            });
            sym.mruTypeId = sid;
            sym.mruOffset = offset;
            sym.mruFieldTypeSymId = typeSymId;
        } else {
            sym.mruTypeId = sid;
            sym.mruOffset = offset;
            sym.mruFieldTypeSymId = typeSymId;
        }
    }

    pub inline fn setTagLitSym(self: *VM, tid: EnumId, symId: SymbolId, val: u32) void {
        self.syms.buf[symId].symT = .one;
        self.syms.buf[symId].inner = .{
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

    pub fn addMethodSym(self: *VM, id: rt.TypeId, symId: SymbolId, entry: rt.MethodSym) !void {
        const sym = &self.methodSyms.buf[symId];
        if (sym.mruTypeId != cy.NullId) {
            const prev = rt.MethodTableKey.initMethodTableKey(sym.mruTypeId, symId);
            if (!self.methodTable.contains(prev)) {
                try self.methodTable.putNoClobber(self.alloc, prev, .{
                    .entryT = sym.entryT,
                    .rFuncSigId = sym.rFuncSigId,
                    .inner = sym.inner,
                });
            }
            const key = rt.MethodTableKey.initMethodTableKey(id, symId);
            try self.methodTable.putNoClobber(self.alloc, key, .{
                .entryT = entry.entryT,
                .rFuncSigId = entry.rFuncSigId,
                .inner = entry.inner,
            });
            sym.* = .{
                .entryT = entry.entryT,
                .mruTypeId = id,
                .rFuncSigId = entry.rFuncSigId,
                .inner = entry.inner,
            };
        } else {
            sym.* = .{
                .entryT = entry.entryT,
                .mruTypeId = id,
                .rFuncSigId = entry.rFuncSigId,
                .inner = entry.inner,
            };
        }
    }

    pub fn setIndexRelease(self: *VM, left: Value, index: Value, right: Value) !void {
        if (left.isPointer()) {
            const obj = left.asHeapObject();
            switch (obj.head.typeId) {
                rt.ListT => {
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
                        return self.interruptThrowSymbol(.OutOfBounds);
                    }
                },
                rt.MapT => {
                    const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                    const res = try map.getOrPut(self.alloc, self, index);
                    if (res.foundExisting) {
                        release(self, res.valuePtr.*);
                    } else {
                        // No previous entry, retain key.
                        retain(self, index);
                    }
                    res.valuePtr.* = right;
                },
                else => {
                    return self.interruptThrowSymbol(.InvalidArgument);
                },
            }
        } else {
            return self.interruptThrowSymbol(.InvalidArgument);
        }
    }

    pub fn setIndex(self: *VM, left: Value, index: Value, right: Value) !void {
        if (left.isPointer()) {
            const obj = left.asHeapObject();
            switch (obj.head.typeId) {
                rt.ListT => {
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
                        return self.interruptThrowSymbol(.OutOfBounds);
                    }
                },
                rt.MapT => {
                    const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                    const res = try map.getOrPut(self.alloc, self, index);
                    if (!res.foundExisting) {
                        // No previous entry, retain key.
                        retain(self, index);
                    }
                    res.valuePtr.* = right;
                },
                else => {
                    return self.interruptThrowSymbol(.InvalidArgument);
                },
            }
        } else {
            return self.interruptThrowSymbol(.InvalidArgument);
        }
    }

    /// Assumes sign of index is preserved.
    fn getReverseIndex(self: *VM, left: *Value, index: Value) linksection(cy.Section) !Value {
        if (left.isPointer()) {
            const obj = left.asHeapObject();
            const res = switch (obj.head.typeId) {
                rt.ListT => {
                    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                    const idx = @intCast(i32, list.len) + @floatToInt(i32, index.toF64());
                    if (idx < list.len) {
                        const res = list.buf[@intCast(u32, idx)];
                        retain(self, res);
                        return res;
                    } else {
                        return self.interruptThrowSymbol(.OutOfBounds);
                    }
                },
                rt.MapT => {
                    const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                    const key = Value.initF64(index.toF64());
                    if (map.get(self, key)) |val| {
                        retain(self, val);
                        return val;
                    } else return Value.None;
                },
                rt.AstringT => b: {
                    const idx = @intToFloat(f64, @intCast(i32, obj.astring.len) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    break :b bindings.stringSliceAt(.astring)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                },
                rt.UstringT => b: {
                    const idx = @intToFloat(f64, @intCast(i32, obj.ustring.charLen) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    break :b bindings.stringSliceAt(.ustring)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                },
                rt.StringSliceT => b: {
                    if (obj.stringSlice.isAstring()) {
                        const idx = @intToFloat(f64, @intCast(i32, obj.stringSlice.len) + @floatToInt(i32, index.toF64()));
                        retainObject(self, obj);
                        break :b bindings.stringSliceAt(.slice)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                    } else {
                        const idx = @intToFloat(f64, @intCast(i32, obj.stringSlice.uCharLen) + @floatToInt(i32, index.toF64()));
                        retainObject(self, obj);
                        break :b bindings.stringSliceAt(.slice)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                    }
                },
                rt.RawstringT => b: {
                    const idx = @intToFloat(f64, @intCast(i32, obj.rawstring.len) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    break :b bindings.stringSliceAt(.rawstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                },
                rt.RawstringSliceT => b: {
                    const idx = @intToFloat(f64, @intCast(i32, obj.rawstringSlice.len) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    break :b bindings.stringSliceAt(.rawSlice)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                },
                else => {
                    return self.panicFmt("Unsupported reverse index operation on type `{}`.", &.{v(self.types.buf[obj.head.typeId].name)});
                },
            };
            if (res.isInterrupt()) {
                return error.Panic;
            }
            return res;
        } else {
            if (left.isNumber()) {
                return self.panic("Unsupported reverse index operation on type `number`.");
            } else {
                const res = switch (left.getTag()) {
                    rt.StaticAstringT => {
                        const idx = @intToFloat(f64, @intCast(i32, left.asStaticStringSlice().len()) + @floatToInt(i32, index.toF64()));
                        return bindings.stringSliceAt(.staticAstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                    },
                    rt.StaticUstringT => {
                        const start = left.asStaticStringSlice().start;
                        const idx = @intToFloat(f64, @intCast(i32, cy.string.getStaticUstringHeader(self, start).charLen) + @floatToInt(i32, index.toF64()));
                        return bindings.stringSliceAt(.staticUstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                    },
                    else => {
                        return self.panicFmt("Unsupported reverse index operation on type `{}`.", &.{v(@intCast(u8, left.getTag()))});
                    },
                };
                if (res.isInterrupt()) {
                    return error.Panic;
                }
                return res;
            }
        }
    }

    pub fn getIndex(self: *VM, left: *Value, index: Value) linksection(cy.Section) !Value {
        if (left.isPointer()) {
            const obj = left.asHeapObject();
            const res = switch (obj.head.typeId) {
                rt.ListT => {
                    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                    const idx = @floatToInt(u32, index.toF64());
                    if (idx < list.len) {
                        retain(self, list.buf[idx]);
                        return list.buf[idx];
                    } else {
                        return self.interruptThrowSymbol(.OutOfBounds);
                    }
                },
                rt.MapT => {
                    const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                    if (@call(.never_inline, map.get, .{self, index})) |val| {
                        retain(self, val);
                        return val;
                    } else return Value.None;
                },
                rt.AstringT => b: {
                    retainObject(self, obj);
                    break :b bindings.stringSliceAt(.astring)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1);
                },
                rt.UstringT => b: {
                    retainObject(self, obj);
                    break :b bindings.stringSliceAt(.ustring)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1);
                },
                rt.StringSliceT => b: {
                    retainObject(self, obj);
                    break :b bindings.stringSliceAt(.slice)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1);
                },
                rt.RawstringT => b: {
                    retainObject(self, obj);
                    break :b bindings.stringSliceAt(.rawstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1);
                },
                rt.RawstringSliceT => b: {
                    retainObject(self, obj);
                    break :b bindings.stringSliceAt(.rawSlice)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1);
                },
                else => {
                    return self.panicFmt("Unsupported index operation on type `{}`.", &.{v(self.types.buf[obj.head.typeId].name)});
                },
            };
            if (res.isInterrupt()) {
                return error.Panic;
            }
            return res;
        } else {
            if (left.isNumber()) {
                return self.panic("Unsupported index operation on type `number`.");
            } else {
                const res = switch (left.getTag()) {
                    rt.StaticAstringT => bindings.stringSliceAt(.staticAstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1),
                    rt.StaticUstringT => bindings.stringSliceAt(.staticUstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1),
                    else => {
                        return self.panicFmt("Unsupported index operation on type `{}`.", &.{v(@intCast(u8, left.getTag()))});
                    },
                };
                if (res.isInterrupt()) {
                    return error.Panic;
                }
                return res;
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

    pub fn interruptThrowSymbol(self: *VM, sym: bindings.Symbol) error{Panic} {
        self.panicPayload = Value.initErrorSymbol(@enumToInt(sym)).val;
        self.panicType = .nativeThrow;
        return error.Panic;
    }

    pub fn panicWithUncaughtError(self: *VM, err: Value) error{Panic} {
        @setCold(true);
        self.panicPayload = err.val;
        self.panicType = .uncaughtError;
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
            const obj = recv.asHeapObject();
            const symMap = &self.fieldSyms.buf[fieldId];

            if (obj.head.typeId == symMap.mruTypeId) {
                obj.object.getValuePtr(symMap.mruOffset).* = val;
            } else {
                const offset = self.getFieldOffset(obj, fieldId);
                if (offset != cy.NullU8) {
                    symMap.mruTypeId = obj.head.typeId;
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

    pub fn getFieldOffsetFromTable(self: *VM, typeId: rt.TypeId, symId: SymbolId) u8 {
        const key = rt.FieldTableKey.initFieldTableKey(typeId, symId);
        if (self.fieldTable.get(key)) |res| {
            const sym = &self.fieldSyms.buf[symId];
            sym.mruTypeId = typeId;
            sym.mruOffset = res.offset;
            sym.mruFieldTypeSymId = res.typeSymId;
            return @intCast(u8, res.offset);
        } else {
            return cy.NullU8;
        }
    }

    pub fn getFieldOffset(self: *VM, obj: *HeapObject, symId: SymbolId) linksection(cy.HotSection) u8 {
        const symMap = self.fieldSyms.buf[symId];
        if (obj.head.typeId == symMap.mruTypeId) {
            return @intCast(u8, symMap.mruOffset);
        } else {
            return @call(.never_inline, self.getFieldOffsetFromTable, .{obj.head.typeId, symId});
        }
    }

    pub fn setFieldRelease(self: *VM, recv: Value, symId: SymbolId, val: Value) linksection(cy.HotSection) !void {
        @setCold(true);
        if (recv.isPointer()) {
            const obj = recv.asHeapObject();
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
            const obj = recv.asHeapObject();
            const offset = self.getFieldOffset(obj, symId);
            if (offset != cy.NullU8) {
                return obj.object.getValue(offset);
            } else {
                const sym = self.fieldSyms.buf[symId];
                return self.getFieldFallback(obj, sym.nameId);
            }
        } else {
            return self.getFieldMissingSymbolError();
        }
    }

    pub fn getField(self: *VM, recv: Value, symId: SymbolId) linksection(cy.HotSection) !Value {
        if (recv.isPointer()) {
            const obj = recv.asHeapObject();
            const offset = self.getFieldOffset(obj, symId);
            if (offset != cy.NullU8) {
                return obj.object.getValue(offset);
            } else {
                const sym = self.fieldSyms.buf[symId];
                return self.getFieldFallback(obj, sym.nameId);
            }
        } else {
            return self.getFieldMissingSymbolError();
        }
    }

    fn getFieldFallback(self: *const VM, obj: *const HeapObject, nameId: sema.NameSymId) linksection(cy.HotSection) Value {
        @setCold(true);
        const name = sema.getName(&self.compiler, nameId);
        if (obj.head.typeId == rt.MapT) {
            const map = stdx.ptrAlignCast(*const cy.MapInner, &obj.map.inner);
            if (map.getByString(self, name)) |val| {
                return val;
            } else return Value.None;
        } else {
            log.debug("Missing symbol for object: {}", .{obj.head.typeId});
            return Value.None;
        }
    }

    /// startLocal points to the first arg in the current stack frame.
    fn callSym(self: *VM, pc: [*]cy.InstDatum, framePtr: [*]Value, symId: SymbolId, startLocal: u8, numArgs: u8, reqNumRetVals: u2) linksection(cy.HotSection) !cy.fiber.PcSp {
        const sym = self.funcSyms.buf[symId];
        switch (@intToEnum(FuncSymbolEntryType, sym.entryT)) {
            .nativeFunc1 => {
                const newFramePtr = framePtr + startLocal;

                // Optimize.
                pc[0] = cy.InstDatum{ .code = .callNativeFuncIC };
                @ptrCast(*align(1) u48, pc + 6).* = @intCast(u48, @ptrToInt(sym.inner.nativeFunc1));

                self.pc = pc;
                self.framePtr = newFramePtr;
                const res = sym.inner.nativeFunc1(@ptrCast(*UserVM, self), @ptrCast([*]const Value, newFramePtr + 4), numArgs);
                if (res.isInterrupt()) {
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
                return cy.fiber.PcSp{
                    .pc = pc + cy.bytecode.CallSymInstLen,
                    .sp = framePtr,
                };
            },
            .func => {
                if (@ptrToInt(framePtr + startLocal + sym.inner.func.numLocals) >= @ptrToInt(self.stackEndPtr)) {
                    return error.StackOverflow;
                }

                // Optimize.
                pc[0] = cy.InstDatum{ .code = .callFuncIC };
                pc[4] = cy.InstDatum{ .arg = @intCast(u8, sym.inner.func.numLocals) };
                @ptrCast(*align(1) u48, pc + 6).* = @intCast(u48, @ptrToInt(cy.fiber.toVmPc(self, sym.inner.func.pc)));

                const newFramePtr = framePtr + startLocal;
                newFramePtr[1] = buildReturnInfo2(reqNumRetVals, true, cy.bytecode.CallSymInstLen);
                newFramePtr[2] = Value{ .retPcPtr = pc + cy.bytecode.CallSymInstLen };
                newFramePtr[3] = Value{ .retFramePtr = framePtr };
                return cy.fiber.PcSp{
                    .pc = cy.fiber.toVmPc(self, sym.inner.func.pc),
                    .sp = newFramePtr,
                };
            },
            .closure => {
                if (@ptrToInt(framePtr + startLocal + sym.inner.closure.numLocals) >= @ptrToInt(self.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const newFp = framePtr + startLocal;
                newFp[1] = buildReturnInfo2(reqNumRetVals, true, cy.bytecode.CallSymInstLen);
                newFp[2] = Value{ .retPcPtr = pc + cy.bytecode.CallSymInstLen };
                newFp[3] = Value{ .retFramePtr = framePtr };

                // Copy closure value to new frame pointer local.
                newFp[sym.inner.closure.local] = Value.initPtr(sym.inner.closure);

                return cy.fiber.PcSp{
                    .pc = cy.fiber.toVmPc(self, sym.inner.closure.funcPc),
                    .sp = newFp,
                };
            },
            .none => {
                return @call(.never_inline, reportMissingFuncSym, .{self, pc, symId});
            },
            // else => {
            //     return self.panic("unsupported callsym");
            // },
        }
    }

    fn reportMissingFuncSym(vm: *VM, pc: [*]const cy.InstDatum, rtSymId: SymbolId) error{Panic, OutOfMemory} {
        const relPc = getInstOffset(vm, pc);
        if (debug.getDebugSym(vm, relPc)) |sym| {
            const chunk = vm.compiler.chunks.items[sym.file];
            const callExpr = chunk.nodes[sym.loc];
            const callee = chunk.nodes[callExpr.head.callExpr.callee];
            var crSymId: sema.CompactResolvedSymId = undefined;
            if (callee.node_t == .accessExpr) {
                crSymId = @bitCast(sema.CompactResolvedSymId, callee.head.accessExpr.sema_crSymId);
            } else if (callee.node_t == .ident) {
                crSymId = @bitCast(sema.CompactResolvedSymId, callee.head.ident.sema_crSymId);
            } else {
                return vm.panic("Failed to generate error report.");
            }
            const rFuncSymId = crSymId.id;

            const rFuncSym = chunk.compiler.sema.resolvedFuncSyms.items[rFuncSymId];
            const funcChunk = &vm.compiler.chunks.items[rFuncSym.chunkId];
            const func = funcChunk.semaFuncDecls.items[rFuncSym.declId];

            const name = func.getName(funcChunk);
            const rSym = chunk.compiler.sema.resolvedSyms.items[func.rSymId];

            if (rSym.symT == .func) {
                const rtSym = vm.funcSyms.buf[rtSymId];
                if (rSym.inner.func.rFuncSymId == cy.NullId) {
                    const sigStr = try sema.getResolvedFuncSigTempStr(&vm.compiler, rtSym.innerExtra.none.rFuncSigId);
                    return vm.panicFmt("Unsupported call signature: `{}{}`.\nThere are multiple overloaded functions named `{}`", &.{
                        v(name), v(sigStr), v(name),
                    });
                } else {
                    const rFuncSym_ = vm.compiler.sema.getResolvedFuncSym(rSym.inner.func.rFuncSymId);

                    vm.u8Buf.clearRetainingCapacity();
                    const w = vm.u8Buf.writer(vm.alloc);
                    cy.sema.writeResolvedFuncSigStr(&vm.compiler, w, rtSym.innerExtra.none.rFuncSigId) catch fatal();
                    const rtSigStr = vm.u8Buf.items();
                    const start = vm.u8Buf.len;
                    cy.sema.writeResolvedFuncSigStr(&vm.compiler, w, rFuncSym_.getResolvedFuncSigId()) catch fatal();
                    const existingSigStr = vm.u8Buf.buf[start..vm.u8Buf.len];

                    return vm.panicFmt("Unsupported call signature: `{}{}`.\nOnly `func {}{}` exists for the symbol `{}`.", &.{
                        v(name), v(rtSigStr), v(name), v(existingSigStr), v(name),
                    });
                }
            }
            return vm.panic("Failed to generate error report.");
        } else {
            return vm.panicFmt("Missing debug sym at {}", &.{v(getInstOffset(vm, pc))});
        }
    }

    fn callSymEntry(
        self: *VM, pc: [*]cy.InstDatum, framePtr: [*]Value, sym: rt.MethodSym,
        recv: Value, typeId: u32, startLocal: u8, numArgs: u8, reqNumRetVals: u8
    ) linksection(cy.HotSection) !cy.fiber.PcSp {
        switch (sym.entryT) {
            .func => {
                if (@ptrToInt(framePtr + startLocal + sym.inner.func.numLocals) >= @ptrToInt(self.stackEndPtr)) {
                    return error.StackOverflow;
                }

                // Optimize.
                pc[0] = cy.InstDatum{ .code = .callObjFuncIC };
                pc[7] = cy.InstDatum{ .arg = @intCast(u8, sym.inner.func.numLocals) };
                @ptrCast(*align(1) u32, pc + 8).* = sym.inner.func.pc;
                @ptrCast(*align(1) u16, pc + 14).* = @intCast(u16, typeId);

                const newFp = framePtr + startLocal;
                newFp[1] = buildReturnInfo2(reqNumRetVals, true, cy.bytecode.CallObjSymInstLen);
                newFp[2] = Value{ .retPcPtr = pc + cy.bytecode.CallObjSymInstLen };
                newFp[3] = Value{ .retFramePtr = framePtr };
                return cy.fiber.PcSp{
                    .pc = cy.fiber.toVmPc(self, sym.inner.func.pc),
                    .sp = newFp,
                };
            },
            .nativeFunc1 => {
                // Optimize.
                pc[0] = cy.InstDatum{ .code = .callObjNativeFuncIC };
                @ptrCast(*align(1) u48, pc + 8).* = @intCast(u48, @ptrToInt(sym.inner.nativeFunc1));
                @ptrCast(*align(1) u16, pc + 14).* = @intCast(u16, typeId);

                self.framePtr = framePtr;
                const res = sym.inner.nativeFunc1(@ptrCast(*UserVM, self), recv, @ptrCast([*]const Value, framePtr + startLocal + 4), numArgs);
                if (res.isInterrupt()) {
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
                return cy.fiber.PcSp{
                    .pc = pc + cy.bytecode.CallObjSymInstLen,
                    .sp = framePtr,
                };
            },
            .nativeFunc2 => {
                self.framePtr = framePtr;
                const res = sym.inner.nativeFunc2(@ptrCast(*UserVM, self), recv, @ptrCast([*]const Value, framePtr + startLocal + 4), numArgs);
                if (res.left.isInterrupt()) {
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
                return cy.fiber.PcSp{
                    .pc = pc + cy.bytecode.CallObjSymInstLen,
                    .sp = framePtr,
                };
            },
            // else => {
            //     // stdx.panicFmt("unsupported {}", .{sym.entryT});
            //     unreachable;
            // },
        }
    }

    fn getCallObjSymFallback(
        self: *VM, typeId: rt.TypeId, symId: SymbolId, rFuncSigId: sema.ResolvedFuncSigId
    ) ?rt.MethodSym {
        const sym = self.methodSyms.buf[symId];
        if (sym.mruTypeId == typeId) {
            // Types match, but function signatures don't. Try to fit type constraints.
            if (types.isFuncSigCompat(&self.compiler, rFuncSigId, sym.rFuncSigId)) {
                return sym;
            }
        }
        // Lookup from table.
        return getCallObjSymFromTable(self, typeId, symId, rFuncSigId);
    }

    fn getCallObjSymFromTable(
        self: *VM, typeId: rt.TypeId, symId: SymbolId, rFuncSigId: sema.ResolvedFuncSigId
    ) ?rt.MethodSym {
        const key = rt.MethodTableKey.initMethodTableKey(typeId, symId);
        if (self.methodTable.get(key)) |entry| {
            if (types.isFuncSigCompat(&self.compiler, rFuncSigId, entry.rFuncSigId)) {
                const sym = &self.methodSyms.buf[symId];
                sym.* = .{
                    .entryT = entry.entryT,
                    .mruTypeId = typeId,
                    .rFuncSigId = entry.rFuncSigId,
                    .inner = entry.inner,
                };
                return sym.*;
            }
        }
        return null;
    }

    fn getCallObjSym(self: *VM, typeId: u32, symId: SymbolId, rFuncSigId: sema.ResolvedFuncSigId) linksection(cy.HotSection) ?rt.MethodSym {
        const entry = self.methodSyms.buf[symId];
        if (entry.mruTypeId == typeId and entry.rFuncSigId == rFuncSigId) {
            return entry;
        } else {
            return @call(.never_inline, self.getCallObjSymFallback, .{typeId, symId, rFuncSigId});
        }
    }

    /// Stack layout: arg0, arg1, ..., receiver
    /// numArgs includes the receiver.
    /// Return new pc to avoid deoptimization.
    fn callObjSym(self: *VM, pc: [*]const cy.InstDatum, framePtr: [*]Value, recv: Value, symId: SymbolId, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2) linksection(cy.HotSection) !cy.fiber.PcSp {
        if (recv.isPointer()) {
            const obj = stdx.ptrAlignCast(*HeapObject, recv.asPointer().?);
            const map = self.methodSyms.buf[symId];
            switch (map.mapT) {
                .one => {
                    if (obj.head.typeId == map.inner.one.id) {
                        return try @call(.{.modifier = .never_inline }, callSymEntryNoInline, .{pc, framePtr, map.inner.one.sym, obj, startLocal, numArgs, reqNumRetVals});
                    } else return self.panic("Symbol does not exist for receiver.");
                },
                .many => {
                    if (map.inner.many.mruTypeId == obj.head.typeId) {
                        return try @call(.never_inline, callSymEntryNoInline, .{pc, framePtr, map.inner.many.mruSym, obj, startLocal, numArgs, reqNumRetVals});
                    } else {
                        const sym = self.methodTable.get(.{ .structId = obj.head.typeId, .methodId = symId }) orelse {
                            log.debug("Symbol does not exist for receiver.", .{});
                            stdx.fatal();
                        };
                        self.methodSyms.buf[symId].inner.many = .{
                            .mruTypeId = obj.head.typeId,
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
        return cy.fiber.PcSp{
            .pc = pc,
            .framePtr = undefined,
        };
    }

    pub fn getStackTrace(self: *const VM) *const cy.StackTrace {
        return &self.stackTrace;
    }

    pub fn valueAsStaticString(self: *const VM, val: Value) linksection(cy.HotSection) []const u8 {
        const slice = val.asStaticStringSlice();
        return self.strBuf[slice.start..slice.end];
    }

    /// A comparable string can be any string or a rawstring.
    pub fn tryValueAsComparableString(self: *const VM, val: Value) linksection(cy.Section) ?[]const u8 {
        if (val.isPointer()) {
            const obj = val.asHeapObject();
            if (obj.head.typeId == rt.AstringT) {
                return obj.astring.getConstSlice();
            } else if (obj.head.typeId == rt.UstringT) {
                return obj.ustring.getConstSlice();
            } else if (obj.head.typeId == rt.StringSliceT) {
                return obj.stringSlice.getConstSlice();
            } else if (obj.head.typeId == rt.RawstringT) {
                return obj.rawstring.getConstSlice();
            } else if (obj.head.typeId == rt.RawstringSliceT) {
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
                const obj = val.asHeapObject();
                return obj.astring.getConstSlice();
            },
            .ustring => {
                const obj = val.asHeapObject();
                return obj.ustring.getConstSlice();
            },
            .slice => {
                const obj = val.asHeapObject();
                return obj.stringSlice.getConstSlice();
            },
            .rawstring => {
                const obj = val.asHeapObject();
                return obj.rawstring.getConstSlice();
            },
            .rawSlice => {
                const obj = val.asHeapObject();
                return obj.rawstringSlice.getConstSlice();
            },
        }
    }

    pub fn valueAsString(self: *const VM, val: Value) linksection(cy.Section) []const u8 {
        if (val.isPointer()) {
            const obj = val.asHeapObject();
            if (obj.head.typeId == rt.AstringT) {
                return obj.astring.getConstSlice();
            } else if (obj.head.typeId == rt.UstringT) {
                return obj.ustring.getConstSlice();
            } else if (obj.head.typeId == rt.StringSliceT) {
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

    /// Like valueToTempString but does not guarantee a valid string.
    pub fn valueToTempRawString(self: *const VM, val: Value) linksection(cy.StdSection) []const u8 {
        if (val.isRawString()) {
            return val.asRawString();
        } else {
            return self.valueToTempString(val);
        }
    }

    /// String is guaranteed to be valid UTF-8.
    /// Uses a short desc for rawstring instead of performing validation.
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
                const obj = val.asHeapObject();
                switch (obj.head.typeId) {
                    rt.AstringT => {
                        const res = obj.astring.getConstSlice();
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, res.len);
                        }
                        return res;
                    },
                    rt.UstringT => {
                        if (getCharLen) {
                            outCharLen.* = obj.ustring.charLen;
                        }
                        return obj.ustring.getConstSlice();
                    },
                    rt.StringSliceT => {
                        if (getCharLen) {
                            if (obj.stringSlice.isAstring()) {
                                outCharLen.* = obj.stringSlice.len;
                            } else {
                                outCharLen.* = obj.stringSlice.uCharLen;
                            }
                        }
                        return obj.stringSlice.getConstSlice();
                    },
                    rt.RawstringT => {
                        const start = writer.pos();
                        std.fmt.format(writer, "rawstring ({})", .{obj.rawstring.len}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    rt.RawstringSliceT => {
                        const start = writer.pos();
                        std.fmt.format(writer, "rawstring ({})", .{obj.rawstringSlice.len}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    rt.ListT => {
                        const start = writer.pos();
                        std.fmt.format(writer, "List ({})", .{obj.list.list.len}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    rt.MapT => {
                        const start = writer.pos();
                        std.fmt.format(writer, "Map ({})", .{obj.map.inner.size}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    rt.MetaTypeT => {
                        const start = writer.pos();
                        const symType = @intToEnum(cy.heap.MetaTypeKind, obj.metatype.type);
                        var slice: []const u8 = undefined;
                        if (symType == .object) {
                            const name = self.types.buf[obj.metatype.symId].name;
                            std.fmt.format(writer, "type: {s}", .{name}) catch stdx.fatal();
                            slice = writer.sliceFrom(start);
                        } else {
                            slice = "Unknown Symbol";
                        }
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    else => {
                        const buf = self.types.buf[obj.head.typeId].name;
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, buf.len);
                        }
                        return buf;
                    }
                }
            } else {
                switch (val.getTag()) {
                    rt.NoneT => {
                        if (getCharLen) {
                            outCharLen.* = "none".len;
                        }
                        return "none";
                    },
                    rt.BooleanT => {
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
                    rt.ErrorT => {
                        const start = writer.pos();
                        const symId = val.asErrorSymbol();
                        std.fmt.format(writer, "error.{s}", .{self.getSymbolName(symId)}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    rt.StaticAstringT => {
                        const slice = val.asStaticStringSlice();
                        const buf = self.strBuf[slice.start..slice.end];
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, buf.len);
                        }
                        return buf;
                    },
                    rt.StaticUstringT => {
                        const slice = val.asStaticStringSlice();
                        if (getCharLen) {
                            outCharLen.* = @ptrCast(*align (1) cy.StaticUstringHeader, self.strBuf.ptr + slice.start - 12).charLen;
                        }
                        return self.strBuf[slice.start..slice.end];
                    },
                    rt.EnumT => {
                        const start = writer.pos();
                        const enumv = val.asEnum();
                        const enumSym = self.enums.buf[enumv.enumId];
                        const memberName = sema.getName(&self.compiler, enumSym.members[enumv.memberId]);
                        std.fmt.format(writer, "{s}.{s}", .{enumSym.name, memberName}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    rt.SymbolT => {
                        const start = writer.pos();
                        const litId = val.asSymbolId();
                        std.fmt.format(writer, "#{s}", .{self.getSymbolName(litId)}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                    rt.IntegerT => {
                        const start = writer.pos();
                        std.fmt.format(writer, "{}", .{val.asInteger()}) catch stdx.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(u32, slice.len);
                        }
                        return slice;
                    },
                }
            }
        }
    }

    pub fn writeValueToString(self: *const VM, writer: anytype, val: Value) void {
        const str = self.valueToTempString(val);
        _ = writer.write(str) catch stdx.fatal();
    }
};

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
        const obj = val.asHeapObject();
        if (obj.head.typeId == rt.AstringT) {
            return .astring;
        } else if (obj.head.typeId == rt.UstringT) {
            return .ustring;
        } else if (obj.head.typeId == rt.StringSliceT) {
            return .slice;
        } else if (obj.head.typeId == rt.RawstringT) {
            return .rawstring;
        } else if (obj.head.typeId == rt.RawstringSliceT) {
            return .rawSlice;
        }
        return null;
    } else {
        if (val.isNumber()) {
            return null;
        }
        switch (val.getTag()) {
            rt.StaticUstringT => {
                return .staticUstring;
            },
            rt.StaticAstringT => {
                return .staticAstring;
            },
            else => return null,
        }
    }
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
    switch (left.getTypeId()) {
        rt.MetaTypeT => {
            if (right.getTypeId() == rt.MetaTypeT) {
                const l = left.asHeapObject().metatype;
                const r = right.asHeapObject().metatype;
                return Value.initBool(l.type == r.type and l.symId == r.symId);
            }
        },
        else => {},
    }
    return Value.False;
}

fn evalCompareNot(vm: *const VM, left: cy.Value, right: cy.Value) linksection(cy.HotSection) cy.Value {
    if (getComparableStringType(left)) |lstrT| {
        if (getComparableStringType(right)) |rstrT| {
            const lstr = vm.valueAsStringType(left, lstrT);
            const rstr = vm.valueAsStringType(right, rstrT);
            return Value.initBool(!std.mem.eql(u8, lstr, rstr));
        }
    }
    switch (left.getTypeId()) {
        rt.MetaTypeT => {
            if (right.getTypeId() == rt.MetaTypeT) {
                const l = left.asHeapObject().metatype;
                const r = right.asHeapObject().metatype;
                return Value.initBool(l.type != r.type or l.symId != r.symId);
            }
        },
        else => {},
    }
    return Value.True;
}

fn toF64OrPanic(vm: *cy.VM, val: Value) linksection(cy.HotSection) !f64 {
    if (val.isNumber()) {
        return val.asF64();
    } else if (val.isInteger()) {
        return @intToFloat(f64, val.asInteger());
    } else {
        return @call(.never_inline, panicConvertNumberError, .{vm, val});
    }
}

fn panicExpectedNumber(vm: *cy.VM) error{Panic, OutOfMemory} {
    @setCold(true);
    return vm.panic("Expected number operand.");
}

fn panicConvertNumberError(vm: *cy.VM, val: Value) error{Panic, OutOfMemory} {
    @setCold(true);
    const typeId = val.getTypeId();
    return vm.panicFmt("Cannot convert `{}` to number.", &.{v(vm.types.buf[typeId].name)});
}

fn convToF64OrPanic(val: Value) linksection(cy.HotSection) !f64 {
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        if (obj.head.typeId == rt.AstringT) {
            const str = obj.astring.getConstSlice();
            return std.fmt.parseFloat(f64, str) catch 0;
        } else if (obj.head.typeId == rt.UstringT) {
            const str = obj.ustring.getConstSlice();
            return std.fmt.parseFloat(f64, str) catch 0;
        } else if (obj.head.typeId == rt.RawstringT) {
            const str = obj.rawstring.getConstSlice();
            return std.fmt.parseFloat(f64, str) catch 0;
        } else return gvm.panic("Cannot convert struct to number");
    } else {
        switch (val.getTag()) {
            rt.NoneT => return 0,
            rt.BooleanT => return if (val.asBool()) 1 else 0,
            rt.IntegerT => return @intToFloat(f64, val.asI32()),
            rt.ErrorT => stdx.fatal(),
            rt.StaticAstringT => {
                const slice = val.asStaticStringSlice();
                const str = gvm.strBuf[slice.start..slice.end];
                return std.fmt.parseFloat(f64, str) catch 0;
            },
            rt.StaticUstringT => {
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
            rt.NoneT => return Value.initF64(0),
            rt.BooleanT => {
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
            rt.NoneT => return Value.True,
            rt.BooleanT => return Value.initBool(!val.asBool()),
            else => stdx.panic("unexpected tag"),
        }
    }
}

const SymbolMapType = enum {
    one,
    many,
    empty,
};

const Symbol = struct {
    symT: SymbolMapType,
    inner: union {
        one: struct {
            id: EnumId,
            val: u32,
        },
    },
    name: []const u8,
    nameOwned: bool,
};

const FieldSymbolMap = vmc.FieldSymbolMap;

test "Internals." {
    try t.eq(@alignOf(VM), 8);

    try t.eq(@sizeOf(FuncSymbolEntry), 16);
    var funcSymEntry: FuncSymbolEntry = undefined;
    try t.eq(@ptrToInt(&funcSymEntry.entryT), @ptrToInt(&funcSymEntry));
    try t.eq(@ptrToInt(&funcSymEntry.innerExtra), @ptrToInt(&funcSymEntry) + 4);
    try t.eq(@ptrToInt(&funcSymEntry.inner), @ptrToInt(&funcSymEntry) + 8);

    try t.eq(@sizeOf(AbsFuncSigKey), 16);

    try t.eq(@sizeOf(FieldSymbolMap), 16);

    // Check Zig/C structs.
    try t.eq(@offsetOf(VM, "alloc"), @offsetOf(vmc.VM, "alloc"));
    try t.eq(@offsetOf(VM, "pc"), @offsetOf(vmc.VM, "curPc"));
    try t.eq(@offsetOf(VM, "framePtr"), @offsetOf(vmc.VM, "curStack"));
    try t.eq(@offsetOf(VM, "stack"), @offsetOf(vmc.VM, "stackPtr"));
    try t.eq(@offsetOf(VM, "stackEndPtr"), @offsetOf(vmc.VM, "stackEndPtr"));
    try t.eq(@offsetOf(VM, "ops"), @offsetOf(vmc.VM, "instPtr"));
    try t.eq(@offsetOf(VM, "consts"), @offsetOf(vmc.VM, "constPtr"));
    try t.eq(@offsetOf(VM, "strBuf"), @offsetOf(vmc.VM, "strBufPtr"));
    try t.eq(@offsetOf(VM, "strInterns"), @offsetOf(vmc.VM, "strInterns"));
    try t.eq(@offsetOf(VM, "heapPages"), @offsetOf(vmc.VM, "heapPages"));
    try t.eq(@offsetOf(VM, "heapFreeHead"), @offsetOf(vmc.VM, "heapFreeHead"));
    try t.eq(@offsetOf(VM, "tryStack"), @offsetOf(vmc.VM, "tryStack"));
    if (cy.TrackGlobalRC) {
        try t.eq(@offsetOf(VM, "refCounts"), @offsetOf(vmc.VM, "refCounts"));
    }
    try t.eq(@offsetOf(VM, "methodSyms"), @offsetOf(vmc.VM, "methodSyms"));
    try t.eq(@offsetOf(VM, "methodTable"), @offsetOf(vmc.VM, "methodTable"));
    try t.eq(@offsetOf(VM, "methodSymSigs"), @offsetOf(vmc.VM, "methodSymSigs"));
    try t.eq(@offsetOf(VM, "funcSyms"), @offsetOf(vmc.VM, "funcSyms"));
    try t.eq(@offsetOf(VM, "funcSymSigs"), @offsetOf(vmc.VM, "funcSymSigs"));
    try t.eq(@offsetOf(VM, "funcSymDetails"), @offsetOf(vmc.VM, "funcSymDetails"));
    try t.eq(@offsetOf(VM, "varSyms"), @offsetOf(vmc.VM, "varSyms"));
    try t.eq(@offsetOf(VM, "varSymSigs"), @offsetOf(vmc.VM, "varSymSigs"));
    try t.eq(@offsetOf(VM, "fieldSyms"), @offsetOf(vmc.VM, "fieldSyms"));
    try t.eq(@offsetOf(VM, "fieldTable"), @offsetOf(vmc.VM, "fieldTable"));
    try t.eq(@offsetOf(VM, "fieldSymSignatures"), @offsetOf(vmc.VM, "fieldSymSignatures"));
    try t.eq(@offsetOf(VM, "types"), @offsetOf(vmc.VM, "types"));
    try t.eq(@offsetOf(VM, "typeSignatures"), @offsetOf(vmc.VM, "typeSignatures"));
    try t.eq(@offsetOf(VM, "enums"), @offsetOf(vmc.VM, "enums"));
    try t.eq(@offsetOf(VM, "enumSignatures"), @offsetOf(vmc.VM, "enumSignatures"));
    try t.eq(@offsetOf(VM, "syms"), @offsetOf(vmc.VM, "syms"));
    try t.eq(@offsetOf(VM, "symSignatures"), @offsetOf(vmc.VM, "symSignatures"));
    try t.eq(@offsetOf(VM, "u8Buf"), @offsetOf(vmc.VM, "u8Buf"));
    try t.eq(@offsetOf(VM, "u8Buf2"), @offsetOf(vmc.VM, "u8Buf2"));
    try t.eq(@offsetOf(VM, "stackTrace"), @offsetOf(vmc.VM, "stackTrace"));
    try t.eq(@offsetOf(VM, "funcSymDeps"), @offsetOf(vmc.VM, "funcSymDeps"));
    try t.eq(@offsetOf(VM, "methodSymExtras"), @offsetOf(vmc.VM, "methodSymExtras"));
    try t.eq(@offsetOf(VM, "debugTable"), @offsetOf(vmc.VM, "debugTablePtr"));
    try t.eq(@offsetOf(VM, "curFiber"), @offsetOf(vmc.VM, "curFiber"));
    try t.eq(@offsetOf(VM, "mainFiber"), @offsetOf(vmc.VM, "mainFiber"));
}

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
            typedFlagNumParams: u16,
            rFuncSigId: u16,

            pub fn numParams(self: @This()) u16 {
                return self.typedFlagNumParams & ~(@as(u16, 1) << 15);
            }
        },
        none: extern struct {
            rFuncSigId: u32,
        },
        func: extern struct {
            rFuncSigId: u32,
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

    pub fn initNativeFunc1(func: *const fn (*UserVM, [*]const Value, u8) Value, isTyped: bool, numParams: u32, rFuncSigId: sema.ResolvedFuncSigId) FuncSymbolEntry {
        const isTypedMask: u16 = if (isTyped) 1 << 15 else 0;
        return .{
            .entryT = @enumToInt(FuncSymbolEntryType.nativeFunc1),
            .innerExtra = .{
                .nativeFunc1 = .{
                    .typedFlagNumParams = isTypedMask | @intCast(u16, numParams),
                    .rFuncSigId = @intCast(u16, rFuncSigId),
                }
            },
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    pub fn initFunc(pc: usize, numLocals: u16, numParams: u16, rFuncSigId: cy.sema.ResolvedFuncSigId) FuncSymbolEntry {
        return .{
            .entryT = @enumToInt(FuncSymbolEntryType.func),
            .innerExtra = .{
                .func = .{
                    .rFuncSigId = rFuncSigId,
                },
            },
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

const EnumId = u32;
const Enum = struct {
    name: []const u8,
    members: []const sema.NameSymId,
};

pub const SymbolId = u32;

pub const TraceInfo = struct {
    opCounts: []OpCount = &.{},
    totalOpCounts: u32 = 0,
    numRetains: u32 = 0,
    numRetainAttempts: u32 = 0,
    numReleases: u32 = 0,
    numReleaseAttempts: u32 = 0,
    numForceReleases: u32 = 0,
    numRetainCycles: u32 = 0,
    numRetainCycleRoots: u32 = 0,

    pub fn deinit(self: *TraceInfo, alloc: std.mem.Allocator) void {
        alloc.free(self.opCounts);
    }
};

pub const OpCount = struct {
    code: u32,
    count: u32,
};

const Root = @This();

/// To reduce the amount of code inlined in the hot loop, handle StackOverflow at the top and resume execution.
/// This is also the entry way for native code to call into the VM, assuming pc, framePtr, and virtual registers are already set.
pub fn evalLoopGrowStack(vm: *VM) linksection(cy.HotSection) error{StackOverflow, OutOfMemory, Panic, NoDebugSym, End}!void {
    if (comptime build_options.engine == .zig) {
        while (true) {
            @call(.always_inline, evalLoop, .{vm}) catch |err| {
                if (err == error.StackOverflow) {
                    log.debug("grow stack", .{});
                    try @call(.never_inline, cy.fiber.growStackAuto, .{vm});
                    continue;
                } else if (err == error.End) {
                    return;
                } else if (err == error.Panic) {
                    if (vm.panicType == .nativeThrow) {
                        if (try @call(.never_inline, cy.fiber.throw, .{vm, vm.framePtr, vm.pc, Value.initRaw(vm.panicPayload) })) |res| {
                            vm.pc = res.pc;
                            vm.framePtr = res.sp;
                            continue;
                        } else {
                            vm.panicType = .uncaughtError;
                        }
                    }
                    try @call(.never_inline, debug.buildStackTrace, .{vm});
                    return error.Panic;
                } else return err;
            };
            return;
        }
    } else if (comptime build_options.engine == .c) {
        while (true) {
            const res = vmc.execBytecode(@ptrCast(*vmc.VM, vm));
            if (res != vmc.RES_CODE_SUCCESS) {
                return error.Panic;
            }
            return;
        }
    } else {
        @compileError("Unsupported engine.");
    }
}

/// Generate assembly labels to find sections easier.
const GenLabels = builtin.mode != .Debug and !builtin.cpu.arch.isWasm() and false;

fn evalLoop(vm: *VM) linksection(cy.HotSection) error{StackOverflow, OutOfMemory, Panic, NoDebugSym, End}!void {
    if (GenLabels) {
        _ = asm volatile ("LEvalLoop:"::);
    }

    var pc = vm.pc;
    var framePtr = vm.framePtr;
    defer {
        vm.pc = pc;
        vm.framePtr = framePtr;
        if (builtin.mode == .Debug) {
            vm.debugPc = getInstOffset(vm, pc);
        }
    } 

    while (true) {
        if (TraceEnabled) {
            const op = pc[0].code;
            vm.trace.opCounts[@enumToInt(op)].count += 1;
            vm.trace.totalOpCounts += 1;
        }
        if (builtin.mode == .Debug) {
            if (cy.verbose) {
                dumpEvalOp(vm, pc) catch stdx.fatal();
            }
            vm.debugPc = getInstOffset(vm, pc);
        }
        if (GenLabels) {
            // _ = asm volatile ("LOpBase:"::);
        }
        switch (pc[0].code) {
            .none => {
                if (GenLabels) {
                    _ = asm volatile ("LOpNone:"::);
                }
                framePtr[pc[1].arg] = Value.None;
                pc += 2;
                continue;
            },
            .constOp => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstOp:"::);
                }
                const idx = @ptrCast(*align (1) u16, pc + 1).*;
                framePtr[pc[3].arg] = Value.initRaw(vm.consts[idx].val);
                pc += 4;
                continue;
            },
            .constI8 => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstI8:"::);
                }
                framePtr[pc[2].arg] = Value.initF64(@intToFloat(f64, @bitCast(i8, pc[1].arg)));
                pc += 3;
                continue;
            },
            .constI8Int => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstI8Int:"::);
                }
                framePtr[pc[2].arg] = Value.initI32(@intCast(i32, @bitCast(i8, pc[1].arg)));
                pc += 3;
                continue;
            },
            .release => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRelease:"::);
                }
                release(vm, framePtr[pc[1].arg]);
                pc += 2;
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
                continue;
            },
            .fieldIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFieldIC:"::);
                }
                const recv = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    if (obj.head.typeId == @ptrCast(*align (1) u16, pc + 5).*) {
                        framePtr[dst] = obj.object.getValue(pc[7].arg);
                        pc += 8;
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.InstDatum{ .code = .field };
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
                continue;
            },
            .jumpNotCond => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJumpNotCond:"::);
                }
                const cond = framePtr[pc[1].arg];
                const condVal = if (cond.isBool()) b: {
                    break :b cond.asBool();
                } else b: {
                    break :b cond.assumeNotBoolToBool();
                };
                if (!condVal) {
                    pc += @ptrCast(*const align(1) u16, pc + 2).*;
                } else {
                    pc += 4;
                }
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
                    return panicExpectedNumber(vm);
                }
                pc += 4;
                continue;
            },
            .addInt => {
                if (GenLabels) {
                    _ = asm volatile ("LOpAddInt:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = Value.initI32(left.asInteger() + right.asInteger());
                pc += 4;
                continue;
            },
            .sub => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSub:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initF64(left.asF64() - right.asF64());
                } else {
                    return panicExpectedNumber(vm);
                }
                pc += 4;
                continue;
            },
            .subInt => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSubInt:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = Value.initI32(left.asInteger() - right.asInteger());
                pc += 4;
                continue;
            },
            .less => {
                if (GenLabels) {
                    _ = asm volatile ("LOpLess:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initBool(left.asF64() < right.asF64());
                } else {
                    return panicExpectedNumber(vm);
                }
                pc += 4;
                continue;
            },
            .lessInt => {
                if (GenLabels) {
                    _ = asm volatile ("LOpLessInt:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = Value.initBool(left.asInteger() < right.asInteger());
                pc += 4;
                continue;
            },
            .greater => {
                if (GenLabels) {
                    _ = asm volatile ("LOpGreater:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initBool(left.asF64() > right.asF64());
                } else {
                    return panicExpectedNumber(vm);
                }
                pc += 4;
                continue;
            },
            .lessEqual => {
                if (GenLabels) {
                    _ = asm volatile ("LOpLessEqual:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initBool(left.asF64() <= right.asF64());
                } else {
                    return panicExpectedNumber(vm);
                }
                pc += 4;
                continue;
            },
            .greaterEqual => {
                if (GenLabels) {
                    _ = asm volatile ("LOpGreaterEqual:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initBool(left.asF64() >= right.asF64());
                } else {
                    return panicExpectedNumber(vm);
                }
                pc += 4;
                continue;
            },
            .true => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTrue:"::);
                }
                framePtr[pc[1].arg] = Value.True;
                pc += 2;
                continue;
            },
            .false => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFalse:"::);
                }
                framePtr[pc[1].arg] = Value.False;
                pc += 2;
                continue;
            },
            .not => {
                if (GenLabels) {
                    _ = asm volatile ("LOpNot:"::);
                }
                const val = framePtr[pc[1].arg];
                framePtr[pc[2].arg] = evalNot(val);
                pc += 3;
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
                continue;
            },
            .mapEmpty => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMapEmpty:"::);
                }
                const dst = pc[1].arg;
                pc += 2;
                framePtr[dst] = try cy.heap.allocEmptyMap(vm);
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
                pc += 5;
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
                continue;
            },
            .map => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMap:"::);
                }
                const startLocal = pc[1].arg;
                const numEntries = pc[2].arg;
                const dst = pc[3].arg;
                const keyIdxes = @ptrCast([*]const align(1) u16, pc + 4)[0..numEntries];
                const vals = framePtr[startLocal .. startLocal + numEntries];
                framePtr[dst] = try cy.heap.allocMap(vm, keyIdxes, vals);
                pc += 4 + numEntries * 2;
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
                continue;
            },
            .copy => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCopy:"::);
                }
                framePtr[pc[2].arg] = framePtr[pc[1].arg];
                pc += 3;
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
                continue;
            },
            .retain => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRetain:"::);
                }
                retain(vm, framePtr[pc[1].arg]);
                pc += 2;
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
                continue;
            },
            .jump => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJump:"::);
                }
                @setRuntimeSafety(false);
                pc += @intCast(usize, @ptrCast(*const align(1) i16, &pc[1]).*);
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
                    break :b cond.assumeNotBoolToBool();
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
                if (GenLabels) {
                    _ = asm volatile ("LOpCall0:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                pc += 3;

                const callee = framePtr[startLocal + numArgs + 4];
                const retInfo = buildReturnInfo(0, true, cy.bytecode.CallInstLen);
                // const retInfo = buildReturnInfo(getInstOffset(pc), framePtrOffset(framePtr), 0, true);
                // try @call(.never_inline, gvm.call, .{&pc, callee, numArgs, retInfo});
                try @call(.always_inline, call, .{vm, &pc, &framePtr, callee, startLocal, numArgs, retInfo});
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
                const retInfo = buildReturnInfo(1, true, cy.bytecode.CallInstLen);
                // const retInfo = buildReturnInfo(getInstOffset(pc), framePtrOffset(framePtr), 1, true);
                // try @call(.never_inline, gvm.call, .{&pc, callee, numArgs, retInfo});
                try @call(.always_inline, call, .{vm, &pc, &framePtr, callee, startLocal, numArgs, retInfo});
                continue;
            },
            .callObjFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallObjFuncIC:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const recv = framePtr[startLocal + numArgs + 4 - 1];
                const typeId = recv.getTypeId();

                const cachedStruct = @ptrCast(*align (1) u16, pc + 14).*;
                if (typeId == cachedStruct) {
                    const numLocals = pc[7].arg;
                    if (@ptrToInt(framePtr + startLocal + numLocals) >= @ptrToInt(vm.stackEndPtr)) {
                        return error.StackOverflow;
                    }
                    const retFramePtr = Value{ .retFramePtr = framePtr };
                    framePtr += startLocal;
                    framePtr[1] = buildReturnInfo2(pc[3].arg, true, cy.bytecode.CallObjSymInstLen);
                    framePtr[2] = Value{ .retPcPtr = pc + cy.bytecode.CallObjSymInstLen };
                    framePtr[3] = retFramePtr;
                    pc = vm.ops.ptr + @ptrCast(*align(1) u32, pc + 8).*;
                    continue;
                }

                // Deoptimize.
                pc[0] = cy.InstDatum{ .code = .callObjSym };
                continue;
            },
            .callObjNativeFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallObjNativeFuncIC:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const recv = framePtr[startLocal + numArgs + 4 - 1];
                const typeId = recv.getTypeId();

                const cachedStruct = @ptrCast(*align (1) u16, pc + 14).*;
                if (typeId == cachedStruct) {
                    // const newFramePtr = framePtr + startLocal;
                    vm.framePtr = framePtr;
                    const func = @intToPtr(cy.NativeObjFuncPtr, @intCast(usize, @ptrCast(*align (1) u48, pc + 8).*));
                    const res = func(@ptrCast(*UserVM, vm), recv, @ptrCast([*]const Value, framePtr + startLocal + 4), numArgs);
                    if (res.isInterrupt()) {
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
                    pc += cy.bytecode.CallObjSymInstLen;
                    // In the future, we might allow native functions to change the pc and framePtr.
                    // pc = vm.pc;
                    // framePtr = vm.framePtr;
                    continue;
                }

                // Deoptimize.
                pc[0] = cy.InstDatum{ .code = .callObjSym };
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
                framePtr[1] = buildReturnInfo2(pc[3].arg, true, cy.bytecode.CallSymInstLen);
                framePtr[2] = Value{ .retPcPtr = pc + cy.bytecode.CallSymInstLen };
                framePtr[3] = retFramePtr;

                pc = @intToPtr([*]cy.InstDatum, @intCast(usize, @ptrCast(*align(1) u48, pc + 6).*));
                continue;
            },
            .callNativeFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallNativeFuncIC:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;

                const newFramePtr = framePtr + startLocal;
                vm.pc = pc;
                vm.framePtr = newFramePtr;
                const func = @intToPtr(cy.NativeFuncPtr, @intCast(usize, @ptrCast(*align (1) u48, pc + 6).*));
                const res = func(@ptrCast(*UserVM, vm), @ptrCast([*]const Value, newFramePtr + 4), numArgs);
                if (res.isInterrupt()) {
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
                pc += cy.bytecode.CallSymInstLen;
                continue;
            },
            .ret1 => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRet1:"::);
                }
                if (@call(.always_inline, popStackFrameLocal1, .{vm, &pc, &framePtr})) {
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
                    continue;
                } else {
                    return;
                }
            },
            .pushTry => {
                const errDst = pc[1].arg;
                const catchPcOffset = @ptrCast(*align (1) u16, pc + 2).*;
                if (vm.tryStack.len == vm.tryStack.buf.len) {
                    try @call(.never_inline, vm.tryStack.growTotalCapacity, .{vm.alloc, vm.tryStack.len + 1});
                }
                vm.tryStack.appendAssumeCapacity(.{
                    .fp = framePtr,
                    .catchPc = getInstOffset(vm, pc) + catchPcOffset,
                    .catchErrDst = errDst,
                });
                pc += 4;
                continue;
            },
            .popTry => {
                vm.tryStack.len -= 1;
                pc += @ptrCast(*align (1) u16, pc + 1).*;
                continue;
            },
            .throw => {
                const err = framePtr[pc[1].arg];
                if (err.isError()) {
                    if (try @call(.never_inline, cy.fiber.throw, .{vm, framePtr, pc, err})) |res| {
                        framePtr = res.sp;
                        pc = res.pc;
                        continue;
                    } else {
                        return vm.panicWithUncaughtError(err);
                    }
                } else {
                    // Not an error.
                    return @call(.never_inline, vm.panic, .{"Not an error."});
                }
            },
            .setFieldReleaseIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetFieldReleaseIC:"::);
                }
                const recv = framePtr[pc[1].arg];
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    if (obj.head.typeId == @ptrCast(*align (1) u16, pc + 4).*) {
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
                pc[0] = cy.InstDatum{ .code = .setFieldRelease };
                // framePtr[dst] = try gvm.getField(recv, pc[3].arg);
                try @call(.never_inline, gvm.setFieldRelease, .{ recv, pc[3].arg, framePtr[pc[2].arg] });
                pc += 7;
                continue;
            },
            .fieldRetainIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFieldRetainIC:"::);
                }
                const recv = framePtr[pc[1].arg];
                const dst = pc[2].arg;
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    if (obj.head.typeId == @ptrCast(*align (1) u16, pc + 5).*) {
                        framePtr[dst] = obj.object.getValue(pc[7].arg);
                        retain(vm, framePtr[dst]);
                        pc += 8;
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.InstDatum{ .code = .fieldRetain };
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
                        cy.InstDatum{ .code = .forRange }
                    else
                        cy.InstDatum{ .code = .forRangeReverse };
                    pc += 8;
                }
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
                continue;
            },
            .field => {
                if (GenLabels) {
                    _ = asm volatile ("LOpField:"::);
                }
                const left = pc[1].arg;
                const dst = pc[2].arg;
                const symId = @ptrCast(*align (1) u16, pc + 3).*;
                const recv = framePtr[left];
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    // const offset = @call(.never_inline, gvm.getFieldOffset, .{obj, symId });
                    const offset = vm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        framePtr[dst] = obj.object.getValue(offset);
                        // Inline cache.
                        pc[0] = cy.InstDatum{ .code = .fieldIC };
                        @ptrCast(*align (1) u16, pc + 5).* = @intCast(u16, obj.head.typeId);
                        pc[7] = cy.InstDatum{ .arg = offset };
                    } else {
                        const sym = vm.fieldSyms.buf[symId];
                        framePtr[dst] = @call(.never_inline, vm.getFieldFallback, .{obj, sym.nameId});
                    }
                    pc += 8;
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
                const symId = @ptrCast(*align (1) u16, pc + 3).*;
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    // const offset = @call(.never_inline, gvm.getFieldOffset, .{obj, symId });
                    const offset = vm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        framePtr[dst] = obj.object.getValue(offset);
                        // Inline cache.
                        pc[0] = cy.InstDatum{ .code = .fieldRetainIC };
                        @ptrCast(*align (1) u16, pc + 5).* = @intCast(u16, obj.head.typeId);
                        pc[7] = cy.InstDatum { .arg = offset };
                    } else {
                        const sym = vm.fieldSyms.buf[symId];
                        framePtr[dst] = @call(.never_inline, vm.getFieldFallback, .{obj, sym.nameId});
                    }
                    retain(vm, framePtr[dst]);
                    pc += 8;
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
                    const obj = recv.asHeapObject();
                    // const offset = @call(.never_inline, gvm.getFieldOffset, .{obj, symId });
                    const offset = vm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        const lastValue = obj.object.getValuePtr(offset);
                        release(vm, lastValue.*);
                        lastValue.* = val;

                        // Inline cache.
                        pc[0] = cy.InstDatum{ .code = .setFieldReleaseIC };
                        @ptrCast(*align (1) u16, pc + 4).* = @intCast(u16, obj.head.typeId);
                        pc[6] = cy.InstDatum { .arg = offset };
                        pc += 7;
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
                const funcPc = getInstOffset(vm, pc) - pc[1].arg;
                const numParams = pc[2].arg;
                const numLocals = pc[3].arg;
                const rFuncSigId = @ptrCast(*const align(1) u16, pc + 4).*;
                const dst = pc[6].arg;
                pc += 7;
                framePtr[dst] = try @call(.never_inline, cy.heap.allocLambda, .{vm, funcPc, numParams, numLocals, rFuncSigId });
                continue;
            },
            .closure => {
                if (GenLabels) {
                    _ = asm volatile ("LOpClosure:"::);
                }
                const funcPc = getInstOffset(vm, pc) - pc[1].arg;
                const numParams = pc[2].arg;
                const numCaptured = pc[3].arg;
                const numLocals = pc[4].arg;
                const rFuncSigId = @ptrCast(*const align(1) u16, pc + 5).*;
                const local = pc[7].arg;
                const dst = pc[8].arg;
                const capturedVals = pc[9..9+numCaptured];
                pc += 9 + numCaptured;

                framePtr[dst] = try @call(.never_inline, cy.heap.allocClosure, .{vm, framePtr, funcPc, numParams, numLocals, rFuncSigId, capturedVals, local});
                continue;
            },
            .staticVar => {
                if (GenLabels) {
                    _ = asm volatile ("LOpStaticVar:"::);
                }
                const symId = @ptrCast(*const align(1) u16, pc + 1).*;
                const sym = vm.varSyms.buf[symId];
                retain(vm, sym.value);
                framePtr[pc[3].arg] = sym.value;
                pc += 4;
                continue;
            },
            .setStaticVar => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetStaticVar:"::);
                }
                const symId = @ptrCast(*const align(1) u16, pc + 1).*;
                const prev = vm.varSyms.buf[symId].value;
                vm.varSyms.buf[symId].value = framePtr[pc[3].arg];
                release(vm, prev);
                pc += 4;
                continue;
            },
            .staticFunc => {
                if (GenLabels) {
                    _ = asm volatile ("LOpStaticFunc:"::);
                }
                const symId = @ptrCast(*const align(1) u16, pc + 1).*;
                framePtr[pc[3].arg] = try cy.heap.allocFuncFromSym(vm, symId);
                pc += 4;
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
                    framePtr = res.sp;
                }
                continue;
            },
            .coresume => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCoresume:"::);
                }
                const val = framePtr[pc[1].arg];
                if (val.isPointer()) {
                    const fiber = val.asPointer(*cy.fiber.Fiber);
                    if (fiber.structId == rt.FiberT) {
                        if (fiber != vm.curFiber) {
                            // Only resume fiber if it's not done.
                            if (fiber.pcOffset != cy.NullId) {
                                const res = cy.fiber.pushFiber(vm, getInstOffset(vm, pc + 3), framePtr, fiber, pc[2].arg);
                                pc = res.pc;
                                framePtr = res.sp;
                                continue;
                            }
                        }
                    }
                    cy.arc.releaseObject(vm, stdx.ptrAlignCast(*HeapObject, fiber));
                }
                pc += 3;
                continue;
            },
            .coyield => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCoyield:"::);
                }
                if (vm.curFiber != &vm.mainFiber) {
                    // Only yield on user fiber.
                    const res = cy.fiber.popFiber(vm, getInstOffset(vm, pc), framePtr, Value.None);
                    pc = res.pc;
                    framePtr = res.sp;
                } else {
                    pc += 3;
                }
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
                const fiber = try @call(.never_inline, cy.fiber.allocFiber, .{vm, getInstOffset(vm, pc + 6), args, initialStackSize});
                framePtr[dst] = fiber;
                pc += jump;
                continue;
            },
            .mul => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMul:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initF64(left.asF64() * right.asF64());
                } else {
                    return panicExpectedNumber(vm);
                }
                pc += 4;
                continue;
            },
            .div => {
                if (GenLabels) {
                    _ = asm volatile ("LOpDiv:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initF64(left.asF64() / right.asF64());
                } else {
                    return panicExpectedNumber(vm);
                }
                pc += 4;
                continue;
            },
            .mod => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMod:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initF64(std.math.mod(f64, left.asF64(), right.asF64()) catch std.math.nan_f64);
                } else {
                    return panicExpectedNumber(vm);
                }
                pc += 4;
                continue;
            },
            .pow => {
                if (GenLabels) {
                    _ = asm volatile ("LOpPow:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    framePtr[pc[3].arg] = Value.initF64(std.math.pow(f64, left.asF64(), right.asF64()));
                } else {
                    return panicExpectedNumber(vm);
                }
                pc += 4;
                continue;
            },
            .box => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBox:"::);
                }
                const value = framePtr[pc[1].arg];
                framePtr[pc[2].arg] = try cy.heap.allocBox(vm, value);
                pc += 3;
                continue;
            },
            .setBoxValue => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetBoxValue:"::);
                }
                const box = framePtr[pc[1].arg];
                const rval = framePtr[pc[2].arg];
                if (builtin.mode == .Debug) {
                    std.debug.assert(box.isPointer());
                }
                const obj = box.asHeapObject();
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.head.typeId == rt.BoxT);
                }
                obj.box.val = rval;
                pc += 3;
                continue;
            },
            .setBoxValueRelease => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBoxValueRelease:"::);
                }
                const box = framePtr[pc[1].arg];
                const rval = framePtr[pc[2].arg];
                if (builtin.mode == .Debug) {
                    std.debug.assert(box.isPointer());
                }
                const obj = box.asHeapObject();
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.head.typeId == rt.BoxT);
                }
                @call(.never_inline, release, .{vm, obj.box.val});
                obj.box.val = rval;
                pc += 3;
                continue;
            },
            .boxValue => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBoxValue:"::);
                }
                const box = framePtr[pc[1].arg];
                if (builtin.mode == .Debug) {
                    if (!box.isBox()) {
                        stdx.panic("Expected box value.");
                    }
                }
                framePtr[pc[2].arg] = box.asHeapObject().box.val;
                pc += 3;
                continue;
            },
            .boxValueRetain => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBoxValueRetain:"::);
                }
                const box = framePtr[pc[1].arg];
                if (builtin.mode == .Debug) {
                    if (!box.isBox()) {
                        stdx.panic("Expected box value.");
                    }
                }
                const val = box.asHeapObject().box.val;
                framePtr[pc[2].arg] = val;
                retain(vm, val);
                pc += 3;
                continue;
            },
            .captured => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCaptured:"::);
                }
                const closure = framePtr[pc[1].arg];
                if (builtin.mode == .Debug) {
                    if (!closure.isClosure()) {
                        stdx.panic("Expected closure value.");
                    }
                }
                framePtr[pc[3].arg] = closure.asHeapObject().closure.getCapturedValuesPtr()[pc[2].arg];
                pc += 4;
                continue;
            },
            .tag => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTag:"::);
                }
                const tagId = pc[1].arg;
                const val = pc[2].arg;
                framePtr[pc[3].arg] = Value.initEnum(tagId, val);
                pc += 4;
                continue;
            },
            .tagLiteral => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTagLiteral:"::);
                }
                const symId = pc[1].arg;
                framePtr[pc[2].arg] = Value.initSymbol(symId);
                pc += 3;
                continue;
            },
            .cast => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCast:"::);
                }
                const val = framePtr[pc[1].arg];
                const expTypeId = @ptrCast(*const align(1) u16, pc + 2).*;
                if (val.getTypeId() == expTypeId) {
                    framePtr[pc[4].arg] = val;
                    pc += 5;
                    continue;
                } else {
                    return @call(.never_inline, panicCastError, .{ vm, val, expTypeId });
                }
            },
            .castAbstract => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCastAbstract:"::);
                }
                const val = framePtr[pc[1].arg];
                const expTypeSymId = @ptrCast(*const align(1) u16, pc + 2).*;
                if (expTypeSymId == cy.types.BuiltinTypeSymIds.Any) {
                    framePtr[pc[4].arg] = val;
                    pc += 5;
                    continue;
                } else if (expTypeSymId == cy.types.BuiltinTypeSymIds.String) {
                    if (val.isString()) {
                        framePtr[pc[4].arg] = val;
                        pc += 5;
                        continue;
                    }
                } else if (expTypeSymId == cy.types.BuiltinTypeSymIds.Rawstring) {
                    if (val.isRawString()) {
                        framePtr[pc[4].arg] = val;
                        pc += 5;
                        continue;
                    }
                }
                return @call(.never_inline, panicCastAbstractError, .{ vm, val, expTypeSymId });
            },
            .bitwiseAnd => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseAnd:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    const f = @intToFloat(f64, left.asF64toI32() & right.asF64toI32());
                    framePtr[pc[3].arg] = Value.initF64(f);
                } else {
                    return @call(.never_inline, panicExpectedNumber, .{vm});
                }
                pc += 4;
                continue;
            },
            .bitwiseOr => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseOr:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    const f = @intToFloat(f64, left.asF64toI32() | right.asF64toI32());
                    framePtr[pc[3].arg] = Value.initF64(f);
                } else {
                    return @call(.never_inline, panicExpectedNumber, .{vm});
                }
                pc += 4;
                continue;
            },
            .bitwiseXor => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseXor:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    const f = @intToFloat(f64, left.asF64toI32() ^ right.asF64toI32());
                    framePtr[pc[3].arg] = Value.initF64(f);
                } else {
                    return @call(.never_inline, panicExpectedNumber, .{vm});
                }
                pc += 4;
                continue;
            },
            .bitwiseNot => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseNot:"::);
                }
                const val = framePtr[pc[1].arg];
                if (val.isNumber()) {
                    const f = @intToFloat(f64, ~val.asF64toI32());
                    framePtr[pc[2].arg] = Value.initF64(f);
                } else {
                    return @call(.never_inline, panicExpectedNumber, .{vm});
                }
                pc += 3;
                continue;
            },
            .bitwiseLeftShift => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseLeftShift:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    const amt = @intCast(u5, @bitCast(u64, right.asF64toI64()) % 32);
                    const res = @intToFloat(f64, left.asF64toI32() << amt);
                    framePtr[pc[3].arg] = Value.initF64(res);
                } else {
                    return @call(.never_inline, panicExpectedNumber, .{vm});
                }
                pc += 4;
                continue;
            },
            .bitwiseRightShift => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseRightShift:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                if (Value.bothNumbers(left, right)) {
                    const amt = @intCast(u5, @bitCast(u64, right.asF64toI64()) % 32);
                    const res = @intToFloat(f64, left.asF64toI32() >> amt);
                    framePtr[pc[3].arg] = Value.initF64(res);
                } else {
                    return @call(.never_inline, panicExpectedNumber, .{vm});
                }
                pc += 4;
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
                const rFuncSigId = @ptrCast(*const align(1) u16, pc + 5).*;

                const recv = framePtr[startLocal + numArgs + 4 - 1];
                const typeId = recv.getTypeId();

                if (vm.getCallObjSym(typeId, symId, rFuncSigId)) |sym| {
                    const res = try @call(.never_inline, vm.callSymEntry, .{pc, framePtr, sym, recv, typeId, startLocal, numArgs, numRet });
                    pc = res.pc;
                    framePtr = res.sp;
                } else {
                    const res = try @call(.never_inline, callObjSymFallback, .{vm, pc, framePtr, recv, typeId, symId, rFuncSigId, startLocal, numArgs, numRet});
                    pc = res.pc;
                    framePtr = res.sp;
                }

                continue;
            },
            .callSym => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallSym:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const numRet = pc[3].arg;
                const symId = @ptrCast(*const align(1) u16, pc + 4).*;
                const res = try @call(.never_inline, vm.callSym, .{pc, framePtr, symId, startLocal, numArgs, @intCast(u2, numRet)});
                pc = res.pc;
                framePtr = res.sp;
                continue;
            },
            .match => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMatch:"::);
                }
                pc += @call(.never_inline, opMatch, .{vm, pc, framePtr});
                continue;
            },
            .sym => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSym:"::);
                }
                const symType = pc[1].arg;
                const symId = @ptrCast(*const align(1) u32, pc + 2).*;
                framePtr[pc[6].arg] = try cy.heap.allocMetaType(vm, symType, symId);
                pc += 7;
                continue;
            },
            .setStaticFunc => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetStaticFunc:"::);
                }
                const symId = @ptrCast(*const align(1) u16, pc + 1).*;
                try @call(.never_inline, setStaticFunc, .{vm, symId, framePtr[pc[3].arg]});
                pc += 4;
                continue;
            },
            .end => {
                if (GenLabels) {
                    _ = asm volatile ("LOpEnd:"::);
                }
                vm.endLocal = pc[1].arg;
                pc += 2;
                vm.curFiber.pcOffset = @intCast(u32, getInstOffset(vm, pc));
                return error.End;
            },
        }
    }
}

fn popStackFrameLocal0(pc: *[*]const cy.InstDatum, framePtr: *[*]Value) linksection(cy.HotSection) bool {
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

fn popStackFrameLocal1(vm: *VM, pc: *[*]const cy.InstDatum, framePtr: *[*]Value) linksection(cy.HotSection) bool {
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

fn dumpEvalOp(vm: *const VM, pc: [*]const cy.InstDatum) !void {
    if (builtin.is_test and !debug.atLeastTestDebugLevel()) {
        return;
    }
    var buf: [1024]u8 = undefined;
    var extra: []const u8 = "";

    const offset = getInstOffset(vm, pc);
    switch (pc[0].code) {
        .constOp => {
            const idx = @ptrCast(*const align (1) u16, pc + 1).*;
            const dst = pc[3].arg;
            _ = dst;
            const val = Value{ .val = vm.consts[idx].val };
            extra = try std.fmt.bufPrint(&buf, "[constVal={s}]", .{vm.valueToTempString(val)});
        },
        else => {},
    }
    const len = cy.getInstLenAt(pc);
    cy.bytecode.dumpInst(offset, pc[0].code, pc, len, extra);
}

pub const EvalError = error{
    Panic,
    ParseError,
    CompileError,
    OutOfMemory,
    NoEndOp,
    End,
    StackOverflow,
    NoDebugSym,
};

const FieldEntry = struct {
    offset: u32,
    typeSymId: sema.ResolvedSymId,
};

/// See `reserveFuncParams` for stack layout.
/// numArgs does not include the callee.
pub fn call(vm: *VM, pc: *[*]cy.InstDatum, framePtr: *[*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !void {
    if (callee.isPointer()) {
        const obj = callee.asHeapObject();
        switch (obj.head.typeId) {
            rt.ClosureT => {
                if (numArgs != obj.closure.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    // Release func and args.
                    for (framePtr.*[startLocal + 4..startLocal + 4 + numArgs]) |val| {
                        release(vm, val);
                    }
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.closure.numLocals) >= @ptrToInt(vm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr.* };
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;
                framePtr.*[2] = Value{ .retPcPtr = pc.* };
                framePtr.*[3] = retFramePtr;
                pc.* = cy.fiber.toVmPc(vm, obj.closure.funcPc);

                // Copy closure to local.
                framePtr.*[obj.closure.local] = callee;
            },
            rt.LambdaT => {
                if (numArgs != obj.lambda.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    // Release func and args.
                    for (framePtr.*[startLocal + 4..startLocal + 4 + numArgs]) |val| {
                        release(vm, val);
                    }
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.lambda.numLocals) >= @ptrToInt(vm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr.* };
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;
                framePtr.*[2] = Value{ .retPcPtr = pc.* };
                framePtr.*[3] = retFramePtr;
                pc.* = cy.fiber.toVmPc(vm, obj.lambda.funcPc);
            },
            rt.NativeFuncT => {
                if (numArgs != obj.nativeFunc1.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    for (framePtr.*[startLocal + 4..startLocal + 4 + numArgs]) |val| {
                        release(vm, val);
                    }
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                vm.pc = pc.*;
                const newFramePtr = framePtr.* + startLocal;
                vm.framePtr = newFramePtr;
                const res = obj.nativeFunc1.func(@ptrCast(*UserVM, vm), newFramePtr + 4, numArgs);
                newFramePtr[0] = res;
            },
            else => {
                // TODO: Throw error.
                stdx.panic("not a function");
            },
        }
    } else {
        // TODO: Throw error.
        stdx.panic("not a function");
    }
}

pub fn callNoInline(vm: *VM, pc: *[*]cy.InstDatum, framePtr: *[*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !void {
    if (callee.isPointer()) {
        const obj = callee.asHeapObject();
        switch (obj.head.typeId) {
            rt.ClosureT => {
                if (numArgs != obj.closure.numParams) {
                    stdx.panic("params/args mismatch");
                }

                if (@ptrToInt(framePtr.* + startLocal + obj.closure.numLocals) >= @ptrToInt(vm.stack.ptr) + (vm.stack.len << 3)) {
                    return error.StackOverflow;
                }

                pc.* = cy.fiber.toVmPc(vm, obj.closure.funcPc);
                framePtr.* += startLocal;
                framePtr.*[1] = retInfo;

                // Copy over captured vars to new call stack locals.
                const src = obj.closure.getCapturedValuesPtr()[0..obj.closure.numCaptured];
                std.mem.copy(Value, framePtr.*[numArgs + 4 + 1..numArgs + 4 + 1 + obj.closure.numCaptured], src);
            },
            rt.LambdaT => {
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
                pc.* = cy.fiber.toVmPc(vm, obj.lambda.funcPc);
            },
            rt.NativeFuncT => {
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

fn panicCastAbstractError(vm: *cy.VM, val: Value, expTypeSymId: cy.sema.ResolvedSymId) !void {
    const sym = vm.compiler.sema.getResolvedSym(expTypeSymId);
    const name = cy.sema.getName(&vm.compiler, sym.key.absResolvedSymKey.nameId);
    return vm.panicFmt("Can not cast `{}` to `{}`.", &.{
         v(vm.types.buf[val.getTypeId()].name), v(name), 
    });
}

fn panicCastError(vm: *cy.VM, val: Value, expTypeId: rt.TypeId) !void {
    return vm.panicFmt("Can not cast `{}` to `{}`.", &.{
         v(vm.types.buf[val.getTypeId()].name), v(vm.types.buf[expTypeId].name), 
    });
}

/// TODO: Once methods are recorded in the object/builtin type's module, this should look there instead of the rt table.
fn panicIncompatibleFuncSig(vm: *cy.VM, typeId: rt.TypeId, rtSymId: SymbolId, rFuncSigId: sema.ResolvedFuncSigId) !Value {
    const name = vm.methodSymExtras.buf[rtSymId].getName();
    const methodSym = vm.methodSyms.buf[rtSymId];
    if (methodSym.mruTypeId == typeId) {
        const sigStr = try sema.allocResolvedFuncSigStr(&vm.compiler, rFuncSigId);
        const existingSigStr = try sema.allocResolvedFuncSigStr(&vm.compiler, methodSym.rFuncSigId);
        defer {
            vm.alloc.free(sigStr);
            vm.alloc.free(existingSigStr);
        }
        return vm.panicFmt(
            \\Can not find compatible function for `{}{}` in `{}`.
            \\Only `func {}{}` exists for the symbol `{}`.
            , &.{
                v(name), v(sigStr), v(vm.types.buf[typeId].name),
                v(name), v(existingSigStr), v(name),
            },
        );
    } else {
        const key = rt.MethodTableKey.initMethodTableKey(typeId, rtSymId);
        if (vm.methodTable.get(key)) |entry| {
            const sigStr = try sema.allocResolvedFuncSigStr(&vm.compiler, rFuncSigId);
            const existingSigStr = try sema.allocResolvedFuncSigStr(&vm.compiler, entry.rFuncSigId);
            defer {
                vm.alloc.free(sigStr);
                vm.alloc.free(existingSigStr);
            }
            return vm.panicFmt(
                \\Can not find compatible function for `{}{}` in `{}`.
                \\Only `func {}{}` exists for the symbol `{}`.
                , &.{
                    v(name), v(sigStr), v(vm.types.buf[typeId].name),
                    v(name), v(existingSigStr), v(name),
                },
            );
        } else {
            const sigStr = try sema.allocResolvedFuncSigStr(&vm.compiler, rFuncSigId);
            defer vm.alloc.free(sigStr);
            return vm.panicFmt("`func {}{}` can not be found in `{}`.", &.{
                 v(name), v(sigStr), v(vm.types.buf[typeId].name), 
            });
        }
    }
}

fn getObjectFunctionFallback(vm: *VM, pc: [*]cy.InstDatum, recv: Value, typeId: u32, rtSymId: SymbolId, rFuncSigId: u16) !Value {
    @setCold(true);
    // Map fallback is no longer supported since cleanup of recv is not auto generated by the compiler.
    // In the future, this may invoke the exact method signature or call a custom overloaded function.
    // if (typeId == MapS) {
    //     const name = vm.methodSymExtras.buf[symId];
    //     const heapMap = stdx.ptrAlignCast(*const MapInner, &obj.map.inner);
    //     if (heapMap.getByString(vm, name)) |val| {
    //         return val;
    //     }
    // }

    // Once methods are added to resolved func syms,
    // this error reporting can be more descriptive depending on these scenarios:
    // - a method with the name is missing.
    // - a method exists with the name but the call signature doesn't match up
    // - multiple methods exist with the name but the call signature doesn't match up
    release(vm, recv);
    const relPc = getInstOffset(vm, pc);
    if (debug.getDebugSym(vm, relPc)) |sym| {
        const chunk = vm.compiler.chunks.items[sym.file];
        const node = chunk.nodes[sym.loc];
        if (node.node_t == .callExpr) {
            return try panicIncompatibleFuncSig(vm, typeId, rtSymId, rFuncSigId);
        } else {
            // Debug node is from:
            // `for [iterable]:`
            const name = vm.methodSymExtras.buf[rtSymId].getName();
            return vm.panicFmt("`{}` is either missing in `{}` or the call signature: {}(self, 0 args) is unsupported.", &.{
                 v(name), v(vm.types.buf[typeId].name), v(name),
            });
        }
    } else {
        return vm.panicFmt("Missing debug sym at {}", &.{v(getInstOffset(vm, pc))});
    }
}

/// Use new pc local to avoid deoptimization.
fn callObjSymFallback(vm: *VM, pc: [*]cy.InstDatum, framePtr: [*]Value, recv: Value, typeId: u32, symId: SymbolId, rFuncSigId: u16, startLocal: u8, numArgs: u8, reqNumRetVals: u8) linksection(cy.Section) !cy.fiber.PcSp {
    @setCold(true);
    // const func = try @call(.never_inline, getObjectFunctionFallback, .{obj, symId});
    const func = try getObjectFunctionFallback(vm, pc, recv, typeId, symId, rFuncSigId);

    retain(vm, func);
    cy.arc.releaseObject(vm, recv.asHeapObject());

    // Replace receiver with function.
    framePtr[startLocal + 4 + numArgs - 1] = func;
    // const retInfo = buildReturnInfo(pc, framePtrOffset(framePtr), reqNumRetVals, true);
    const retInfo = buildReturnInfo2(reqNumRetVals, true, cy.bytecode.CallObjSymInstLen);
    var newPc = pc;
    var newFramePtr = framePtr;
    try @call(.always_inline, callNoInline, .{vm, &newPc, &newFramePtr, func, startLocal, numArgs-1, retInfo});
    return cy.fiber.PcSp{
        .pc = newPc,
        .sp = newFramePtr,
    };
}

fn callSymEntryNoInline(
    vm: *VM, pc: [*]const cy.InstDatum, framePtr: [*]Value,
    sym: rt.MethodSym, obj: *HeapObject, startLocal: u8, numArgs: u8, comptime reqNumRetVals: u2
) linksection(cy.HotSection) !cy.fiber.PcSp {
    switch (sym.entryT) {
        .func => {
            if (@ptrToInt(framePtr + startLocal + sym.inner.func.numLocals) >= @ptrToInt(vm.stack.ptr) + 8 * vm.stack.len) {
                return error.StackOverflow;
            }

            // const retInfo = buildReturnInfo(pc, framePtrOffset(framePtr), reqNumRetVals, true);
            const retInfo = buildReturnInfo(reqNumRetVals, true);
            const newFramePtr = framePtr + startLocal;
            newFramePtr[1] = retInfo;
            return cy.fiber.PcSp{
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
            return cy.fiber.PcSp{
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

pub inline fn buildReturnInfo2(numRetVals: u8, comptime cont: bool, comptime callInstOffset: u8) Value {
    return Value{
        .retInfo = .{
            .numRetVals = numRetVals,
            // .retFlag = if (cont) 0 else 1,
            .retFlag = !cont,
            .callInstOffset = callInstOffset,
        },
    };
}

pub inline fn buildReturnInfo(comptime numRetVals: u2, comptime cont: bool, comptime callInstOffset: u8) Value {
    return Value{
        .retInfo = .{
            .numRetVals = numRetVals,
            // .retFlag = if (cont) 0 else 1,
            .retFlag = !cont,
            .callInstOffset = callInstOffset,
        },
    };
}

pub inline fn getInstOffset(vm: *const VM, to: [*]const cy.InstDatum) u32 {
    return @intCast(u32, cy.fiber.getInstOffset(vm.ops.ptr, to));
}

pub inline fn getStackOffset(vm: *const VM, to: [*]const Value) u32 {
    return @intCast(u32, cy.fiber.getStackOffset(vm.stack.ptr, to));
}

pub inline fn toFramePtr(offset: usize) [*]Value {
    return @ptrCast([*]Value, &gvm.stack[offset]);
}

/// Like Value.dump but shows heap values.
pub fn dumpValue(vm: *const VM, val: Value) void {
    if (val.isNumber()) {
        fmt.printStdout("Number {}\n", &.{ v(val.asF64()) });
    } else {
        if (val.isPointer()) {
            const obj = val.asHeapObject();
            switch (obj.head.typeId) {
                rt.ListT => fmt.printStdout("List {} len={}\n", &.{v(obj), v(obj.list.list.len)}),
                rt.MapT => fmt.printStdout("Map {} size={}\n", &.{v(obj), v(obj.map.inner.size)}),
                rt.AstringT => {
                    const str = obj.astring.getConstSlice();
                    if (str.len > 20) {
                        fmt.printStdout("String {} len={} str=\"{}\"...\n", &.{v(obj), v(str.len), v(str[0..20])});
                    } else {
                        fmt.printStdout("String {} len={} str=\"{}\"\n", &.{v(obj), v(str.len), v(str)});
                    }
                },
                rt.UstringT => {
                    const str = obj.ustring.getConstSlice();
                    if (str.len > 20) {
                        fmt.printStdout("String {} len={} str=\"{}\"...\n", &.{v(obj), v(str.len), v(str[0..20])});
                    } else {
                        fmt.printStdout("String {} len={} str=\"{}\"\n", &.{v(obj), v(str.len), v(str)});
                    }
                },
                rt.LambdaT => fmt.printStdout("Lambda {}\n", &.{v(obj)}),
                rt.ClosureT => fmt.printStdout("Closure {}\n", &.{v(obj)}),
                rt.FiberT => fmt.printStdout("Fiber {}\n", &.{v(obj)}),
                rt.NativeFuncT => fmt.printStdout("NativeFunc {}\n", &.{v(obj)}),
                else => {
                    fmt.printStdout("HeapObject {} {} {}\n", &.{v(obj), v(obj.head.typeId), v(vm.types.buf[obj.head.typeId].name)});
                },
            }
        } else {
            switch (val.getTag()) {
                rt.NoneT => {
                    fmt.printStdout("None\n", &.{});
                },
                rt.StaticUstringT,
                rt.StaticAstringT => {
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

const RtFuncSymKey = KeyU96;

pub const KeyU96 = extern union {
    val: extern struct {
        a: u64,
        b: u32,
    },
    rtFuncSymKey: extern struct {
        // TODO: Is it enough to just use the final resolved func sym id?
        // One use for splitting up rParentSymId/nameId is to look for <call>() magic function.
        rParentSymId: sema.ResolvedSymId,
        nameId: sema.NameSymId,
        rFuncSigId: sema.ResolvedFuncSigId,
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

/// Absolute func symbol signature key.
pub const AbsFuncSigKey = KeyU96;

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

    enableFileModules: bool = false,

    /// Whether url imports and cached assets should be reloaded.
    reload: bool = false,

    /// By default, debug syms are only generated for insts that can potentially fail.
    genAllDebugSyms: bool = false,
};

fn opMatch(vm: *const VM, pc: [*]const cy.InstDatum, framePtr: [*]const Value) u16 {
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

fn releaseFuncSymDep(vm: *VM, symId: SymbolId) void {
    const entry = vm.funcSyms.buf[symId];
    switch (@intToEnum(FuncSymbolEntryType, entry.entryT)) {
        .closure => {
            cy.arc.releaseObject(vm, @ptrCast(*cy.HeapObject, entry.inner.closure));
        },
        else => {
            // Check to cleanup previous dep.
            if (vm.funcSymDeps.get(symId)) |dep| {
                release(vm, dep);
                _ = vm.funcSymDeps.remove(symId);
            }
        },
    }
}

fn reportAssignFuncSigMismatch(vm: *VM, srcFuncSigId: u32, dstFuncSigId: u32) error{OutOfMemory, Panic} {
    const dstSig = cy.sema.allocResolvedFuncSigStr(&vm.compiler, dstFuncSigId) catch fatal();
    const srcSig = cy.sema.allocResolvedFuncSigStr(&vm.compiler, srcFuncSigId) catch fatal();
    defer {
        vm.alloc.free(dstSig);
        vm.alloc.free(srcSig);
    }
    return vm.panicFmt("Assigning to static function `func {}` with a different function signature `func {}`.",
        &.{v(dstSig), v(srcSig)}
    );
}

fn setStaticFunc(vm: *VM, symId: SymbolId, val: Value) linksection(cy.Section) !void {
    errdefer {
        // TODO: This should be taken care of by panic stack unwinding.
        cy.arc.release(vm, val);
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        switch (obj.head.typeId) {
            rt.NativeFuncT => {
                const dstRFuncSigId = getResolvedFuncSigIdOfSym(vm, symId);
                if (dstRFuncSigId != obj.nativeFunc1.rFuncSigId) {
                    return @call(.never_inline, reportAssignFuncSigMismatch, .{vm, obj.nativeFunc1.rFuncSigId, dstRFuncSigId});
                }
                releaseFuncSymDep(vm, symId);

                const rFuncSig = vm.compiler.sema.resolvedFuncSigs.items[dstRFuncSigId];
                vm.funcSyms.buf[symId] = .{
                    .entryT = @enumToInt(FuncSymbolEntryType.nativeFunc1),
                    .innerExtra = .{
                        .nativeFunc1 = .{
                            .typedFlagNumParams = rFuncSig.numParams(),
                            .rFuncSigId = @intCast(u16, dstRFuncSigId),
                        }
                    },
                    .inner = .{
                        .nativeFunc1 = obj.nativeFunc1.func,
                    },
                };
                vm.funcSymDeps.put(vm.alloc, symId, val) catch stdx.fatal();
            },
            rt.LambdaT => {
                const dstRFuncSigId = getResolvedFuncSigIdOfSym(vm, symId);
                if (dstRFuncSigId != obj.lambda.rFuncSigId) {
                    return @call(.never_inline, reportAssignFuncSigMismatch, .{vm, @intCast(u32, obj.lambda.rFuncSigId), dstRFuncSigId});
                }
                releaseFuncSymDep(vm, symId);
                vm.funcSyms.buf[symId] = .{
                    .entryT = @enumToInt(FuncSymbolEntryType.func),
                    .inner = .{
                        .func = .{
                            .pc = obj.lambda.funcPc,
                            .numLocals = obj.lambda.numLocals,
                            .numParams = obj.lambda.numParams,
                        },
                    },
                };
                vm.funcSymDeps.put(vm.alloc, symId, val) catch stdx.fatal();
            },
            rt.ClosureT => {
                const dstRFuncSigId = getResolvedFuncSigIdOfSym(vm, symId);
                if (dstRFuncSigId != obj.closure.rFuncSigId) {
                    return @call(.never_inline, reportAssignFuncSigMismatch, .{vm, @intCast(u32, obj.closure.rFuncSigId), dstRFuncSigId});
                }
                releaseFuncSymDep(vm, symId);
                vm.funcSyms.buf[symId] = .{
                    .entryT = @enumToInt(FuncSymbolEntryType.closure),
                    .inner = .{
                        .closure = val.asPointer(*cy.heap.Closure),
                    },
                };
                // Don't set func sym dep since the closure is assigned into the func sym entry.
            },
            else => {
                return vm.panicFmt("Assigning to static function with unsupported type {}.", &.{v(obj.head.typeId)});
            }
        }
    } else {
        return vm.panic("Assigning to static function with a non-function value.");
    }
}

fn getResolvedFuncSigIdOfSym(vm: *const VM, symId: SymbolId) sema.ResolvedFuncSigId {
    switch (@intToEnum(FuncSymbolEntryType, vm.funcSyms.buf[symId].entryT)) {
        .nativeFunc1 => {
            return vm.funcSyms.buf[symId].innerExtra.nativeFunc1.rFuncSigId;
        },
        .func => {
            return vm.funcSyms.buf[symId].innerExtra.func.rFuncSigId;
        },
        .closure => {
            return @intCast(u32, vm.funcSyms.buf[symId].inner.closure.rFuncSigId);
        },
        .none => {
            return vm.funcSyms.buf[symId].innerExtra.none.rFuncSigId;
        },
    }
}

const MethodSymExtra = struct {
    namePtr: [*]const u8,
    nameLen: u32,
    nameIsOwned: bool,

    pub fn getName(self: *const MethodSymExtra) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

pub const ValidateResult = struct {
    err: ?cy.CompileErrorType,
};

export fn zFatal() void {
    stdx.fatal();
}

export fn zOpCodeName(code: vmc.OpCode) [*:0]const u8 {
    return @tagName(@intToEnum(cy.OpCode, code));
}

export fn zCallSym(vm: *VM, pc: [*]cy.InstDatum, framePtr: [*]Value, symId: SymbolId, startLocal: u8, numArgs: u8, reqNumRetVals: u8) linksection(cy.HotSection) vmc.PcSp {
    const res = @call(.always_inline, vm.callSym, .{pc, framePtr, symId, startLocal, numArgs, @intCast(u2, reqNumRetVals)}) catch {
        stdx.fatal();
    };
    return .{
        .pc = @ptrCast([*c]u8, res.pc),
        .sp = @ptrCast([*c]u64, res.sp),
    };
}

export fn zDumpEvalOp(vm: *const VM, pc: [*]const cy.InstDatum) void {
    dumpEvalOp(vm, pc) catch stdx.fatal();
}

export fn zFreeObject(vm: *cy.VM, obj: *HeapObject) linksection(cy.HotSection) void {
    cy.heap.freeObject(vm, obj);
} 

export fn zEnd(vm: *cy.VM, pc: [*]const cy.InstDatum) void {
    vm.endLocal = pc[1].arg;
    vm.curFiber.pcOffset = @intCast(u32, getInstOffset(vm, pc + 2));
}

export fn zAllocList(vm: *cy.VM, elemStart: [*]const Value, nElems: u8) vmc.ValueResult {
    const list = cy.heap.allocList(vm, elemStart[0..nElems]) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(vmc.Value, list),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zOtherToF64(val: Value) f64 {
    return val.otherToF64() catch fatal();
}

export fn zCallObjSym(vm: *cy.VM, pc: [*]cy.InstDatum, stack: [*]Value, recv: Value, typeId: rt.TypeId, symId: u8, rFuncSigId: u16, startLocal: u8, numArgs: u8, numRet: u8) vmc.CallObjSymResult {
    if (vm.getCallObjSym(typeId, symId, rFuncSigId)) |sym| {
        const res = @call(.never_inline, vm.callSymEntry, .{pc, stack, sym, recv, typeId, startLocal, numArgs, numRet }) catch {
            return .{
                .pc = undefined,
                .stack = undefined,
                .code = vmc.RES_CODE_UNKNOWN,
            };
        };
        return .{
            .pc = @ptrCast([*c]u8, res.pc),
            .stack = @ptrCast([*c]u64, res.sp),
            .code = vmc.RES_CODE_SUCCESS,
        };
    } else {
        const res = @call(.never_inline, callObjSymFallback, .{vm, pc, stack, recv, typeId, symId, rFuncSigId, startLocal, numArgs, numRet}) catch {
            return .{
                .pc = undefined,
                .stack = undefined,
                .code = vmc.RES_CODE_UNKNOWN,
            };
        };
        return .{
            .pc = @ptrCast([*c]u8, res.pc),
            .stack = @ptrCast([*c]u64, res.sp),
            .code = vmc.RES_CODE_SUCCESS,
        };
    }
}

export fn zAllocFiber(vm: *cy.VM, pc: u32, args: [*]const Value, nargs: u8, initialStackSize: u8) vmc.ValueResult {
    const fiber = cy.fiber.allocFiber(vm, pc, args[0..nargs], initialStackSize) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(vmc.Value, fiber),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zPushFiber(vm: *cy.VM, curFiberEndPc: usize, curStack: [*]Value, fiber: *cy.Fiber, parentDstLocal: u8) vmc.PcSp {
    const res = cy.fiber.pushFiber(vm, curFiberEndPc, curStack, fiber, parentDstLocal);
    return .{
        .pc = @ptrCast([*c]u8, res.pc),
        .sp = @ptrCast([*c]u64, res.sp),
    };
}

export fn zPopFiber(vm: *cy.VM, curFiberEndPc: usize, curStack: [*]Value, retValue: Value) vmc.PcSp {
    const res = cy.fiber.popFiber(vm, curFiberEndPc, curStack, retValue);
    return .{
        .pc = @ptrCast([*c]u8, res.pc),
        .sp = @ptrCast([*c]u64, res.sp),
    };
}

export fn zAllocObjectSmall(vm: *cy.VM, typeId: rt.TypeId, fields: [*]const Value, nfields: u8) vmc.ValueResult {
    const res = cy.heap.allocObjectSmall(vm, typeId, fields[0..nfields]) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(vmc.Value, res),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zGetFieldOffsetFromTable(vm: *VM, typeId: rt.TypeId, symId: SymbolId) u8 {
    return vm.getFieldOffsetFromTable(typeId, symId);
}

export fn zEvalCompare(vm: *VM, left: Value, right: Value) vmc.Value {
    return @bitCast(vmc.Value, evalCompare(vm, left, right));
}

export fn zEvalCompareNot(vm: *VM, left: Value, right: Value) vmc.Value {
    return @bitCast(vmc.Value, evalCompareNot(vm, left, right));
}
