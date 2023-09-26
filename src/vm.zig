const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const aarch64 = builtin.cpu.arch == .aarch64;
const stdx = @import("stdx");
const fatal = cy.fatal;
const t = stdx.testing;
const tcc = @import("tcc");

const vmc = @import("vm_c.zig");
const fmt = @import("fmt.zig");
const v = fmt.v;
const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypeSymIds;
const rt = cy.rt;
const sema = cy.sema;
const types = cy.types;
const bindings = @import("builtins/bindings.zig");
const math_mod = @import("builtins/math.zig");
const Value = cy.Value;
const debug = @import("debug.zig");
const http = @import("http.zig");
const HeapObject = cy.HeapObject;
const release = cy.arc.release;
const retain = cy.arc.retain;
const retainObject = cy.arc.retainObject;
const UserVM = cy.UserVM;

const log = cy.log.scoped(.vm);

const UseGlobalVM = true;
const StdSection = cy.StdSection;

/// Temp buf for toString conversions when the len is known to be small.
var tempU8Buf: [256]u8 = undefined;
var tempU8BufIdx: u32 = undefined;
var tempU8Writer = SliceWriter{ .buf = &tempU8Buf, .idx = &tempU8BufIdx };

pub const VM = struct {
    alloc: std.mem.Allocator,

    /// Program counter. Pointer to the current instruction data in `ops`.
    pc: [*]cy.Inst,
    /// Current stack frame ptr.
    framePtr: [*]Value,

    /// Value stack.
    stack: []Value,
    stackEndPtr: [*]const Value,

    ops: []cy.Inst,
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
    /// Tail is only used in trace mode.
    heapFreeTail: if (cy.Trace) ?*HeapObject else void,

    /// GC: Contains the head to the first cyclable object.
    /// Always contains one dummy node to avoid null checking.
    cyclableHead: if (cy.hasGC) *cy.heap.DListNode else void,

    tryStack: cy.List(vmc.TryFrame),

    refCounts: if (cy.TrackGlobalRC) usize else void,

    /// Cached buckets used to lookup object methods.
    /// If the `MethodGroup.mruTypeId` matches, the mru func data stored in MethodGroup is used first.
    /// Otherwise, `typeMethodGroups` is queried.
    /// `Method`s, and `TypeMethodGroup`s are not created until `MethodGroup` has more than one method.
    methods: cy.List(rt.Method),
    methodGroups: cy.List(rt.MethodGroup),
    methodGroupKeys: std.AutoHashMapUnmanaged(rt.MethodGroupKey, vmc.MethodGroupId),
    typeMethodGroups: cy.List(rt.TypeMethodGroup),
    typeMethodGroupKeys: std.HashMapUnmanaged(rt.TypeMethodGroupKey, vmc.TypeMethodGroupId, cy.hash.KeyU64Context, 80),

    /// Regular function symbol table.
    funcSyms: cy.List(rt.FuncSymbol),
    funcSymSigs: std.HashMapUnmanaged(AbsFuncSigKey, SymbolId, KeyU96Context, 80),
    funcSymDetails: cy.List(rt.FuncSymDetail),

    /// Static vars. 
    varSyms: cy.List(rt.VarSym),
    varSymSigs: std.HashMapUnmanaged(rt.VarKey, SymbolId, cy.hash.KeyU64Context, 80),

    /// Struct fields symbol table.
    fieldSyms: cy.List(rt.FieldSymbolMap),
    fieldTable: std.HashMapUnmanaged(rt.FieldTableKey, FieldEntry, cy.hash.KeyU64Context, 80),
    fieldSymSignatures: std.StringHashMapUnmanaged(rt.FieldId),

    /// Types.
    types: cy.List(vmc.Type),
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
    methodGroupExts: cy.List(rt.MethodGroupExt),
    methodExts: cy.List(rt.MethodExt),
    debugTable: []const cy.DebugSym,
    debugTempIndexTable: []const u32,
    unwindTempRegs: []const u8,
    unwindTempPrevIndexes: []const u32,

    curFiber: *cy.Fiber,
    mainFiber: cy.Fiber,

    /// Records a minimal trace during stack unwinding.
    /// Only includes frames visited during stack unwinding.
    /// Further frames would need to be queried when building a full stack trace.
    throwTrace: cy.List(vmc.CompactFrame),

    trace: if (cy.Trace) *vmc.TraceInfo else void,

    compiler: *cy.VMcompiler,

    /// User data ptr. Useful for embedders.
    userData: ?*anyopaque,

    /// Host print callback.
    print: cy.PrintFn,

    /// Object to pc of instruction that allocated it.
    objectTraceMap: if (cy.Trace) std.AutoHashMapUnmanaged(*HeapObject, debug.ObjectTrace) else void,

    /// In debug mode, always save the current pc so tracing can be obtained.
    /// debugPc == NullId indicates execution has not started.
    debugPc: if (cy.Trace) u32 else void,

    /// Interface used for imports and fetch.
    httpClient: http.HttpClient,
    stdHttpClient: if (cy.hasCLI) *http.StdHttpClient else *anyopaque,

    iteratorMGID: vmc.MethodGroupId = cy.NullId,
    pairIteratorMGID: vmc.MethodGroupId = cy.NullId,
    nextMGID: vmc.MethodGroupId = cy.NullId,
    nextPairMGID: vmc.MethodGroupId = cy.NullId,
    indexMGID: vmc.MethodGroupId = cy.NullId,
    setIndexMGID: vmc.MethodGroupId = cy.NullId,
    sliceMGID: vmc.MethodGroupId = cy.NullId,
    @"prefix~MGID": vmc.MethodGroupId = cy.NullId,
    @"prefix-MGID": vmc.MethodGroupId = cy.NullId,
    @"infix>MGID": vmc.MethodGroupId = cy.NullId,
    @"infix>=MGID": vmc.MethodGroupId = cy.NullId,
    @"infix<MGID": vmc.MethodGroupId = cy.NullId,
    @"infix<=MGID": vmc.MethodGroupId = cy.NullId,
    @"infix+MGID": vmc.MethodGroupId = cy.NullId,
    @"infix-MGID": vmc.MethodGroupId = cy.NullId,
    @"infix*MGID": vmc.MethodGroupId = cy.NullId,
    @"infix/MGID": vmc.MethodGroupId = cy.NullId,
    @"infix%MGID": vmc.MethodGroupId = cy.NullId,
    @"infix^MGID": vmc.MethodGroupId = cy.NullId,
    @"infix&MGID": vmc.MethodGroupId = cy.NullId,
    @"infix|MGID": vmc.MethodGroupId = cy.NullId,
    @"infix||MGID": vmc.MethodGroupId = cy.NullId,
    @"infix<<MGID": vmc.MethodGroupId = cy.NullId,
    @"infix>>MGID": vmc.MethodGroupId = cy.NullId,

    expGlobalRC: usize,

    varSymExtras: std.AutoHashMapUnmanaged(SymbolId, sema.NameSymId),

    config: EvalConfig,

    /// Local to be returned back to eval caller.
    /// 255 indicates no return value.
    endLocal: u8,

    lastError: ?error{TokenError, ParseError, CompileError, Panic},

    /// Whether this VM is already deinited. Used to skip the next deinit to avoid using undefined memory.
    deinited: bool,
    deinitedRtObjects: bool,

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
            .heapFreeTail = if (cy.Trace) null else undefined,
            .cyclableHead = if (cy.hasGC) @ptrCast(&dummyCyclableHead) else {},
            .pc = undefined,
            .framePtr = undefined,
            .tryStack = .{},
            .methods = .{},
            .methodGroupExts = .{},
            .methodExts = .{},
            .methodGroups = .{},
            .methodGroupKeys = .{},
            .typeMethodGroups = .{},
            .typeMethodGroupKeys = .{},
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
            .trace = undefined,
            .u8Buf = .{},
            .u8Buf2 = .{},
            .stackTrace = .{},
            .debugTable = undefined,
            .debugTempIndexTable = undefined,
            .unwindTempRegs = undefined,
            .unwindTempPrevIndexes = undefined,
            .refCounts = if (cy.TrackGlobalRC) 0 else undefined,
            .throwTrace = .{},
            .mainFiber = undefined,
            .curFiber = undefined,
            .endLocal = undefined,
            .objectTraceMap = if (cy.Trace) .{} else undefined,
            // Initialize to NullId to indicate vm is still in initing.
            .debugPc = if (cy.Trace) cy.NullId else undefined,
            .deinited = false,
            .deinitedRtObjects = false,
            .funcSymDeps = .{},
            .config = undefined,
            .httpClient = undefined,
            .stdHttpClient = undefined,
            .lastError = null,
            .userData = null,
            .expGlobalRC = 0,
            .varSymExtras = .{},
            .print = defaultPrint,
        };
        self.mainFiber.panicType = vmc.PANIC_NONE;
        self.curFiber = &self.mainFiber;
        self.compiler = try self.alloc.create(cy.VMcompiler);
        try self.compiler.init(self);

        if (cy.hasCLI) {
            self.stdHttpClient = try alloc.create(http.StdHttpClient);
            self.stdHttpClient.* = http.StdHttpClient.init(self.alloc);
            self.httpClient = self.stdHttpClient.iface();
        }

        if (cy.Trace) {
            self.trace = try alloc.create(vmc.TraceInfo);
        }

        // Perform decently sized allocation for hot data paths since the allocator
        // will likely use a more consistent allocation.
        // Also try to allocate them in the same bucket.
        try cy.fiber.stackEnsureTotalCapacityPrecise(self, 511);

        try self.funcSyms.ensureTotalCapacityPrecise(self.alloc, 255);
        try self.methodGroups.ensureTotalCapacityPrecise(self.alloc, 170);
        try self.methodGroupKeys.ensureTotalCapacity(self.alloc, 200);
        std.debug.assert(cy.utils.getHashMapMemSize(rt.MethodGroupKey, vmc.MethodGroupId, self.methodGroupKeys.capacity()) < 4096);

        try self.types.ensureTotalCapacityPrecise(alloc, 170);
        try self.fieldSyms.ensureTotalCapacityPrecise(alloc, 170);

        // Initialize heap.
        const list = try cy.heap.growHeapPages(self, 1);
        self.heapFreeHead = list.head;
        if (cy.Trace) {
            self.heapFreeTail = list.tail;
        }
    }

    pub fn deinitRtObjects(self: *VM) void {
        if (self.deinitedRtObjects) {
            return;
        }
        for (self.funcSyms.items()) |sym| {
            if (sym.entryT == @intFromEnum(rt.FuncSymbolType.closure)) {
                cy.arc.releaseObject(self, @ptrCast(sym.inner.closure));
            }
        }

        for (self.varSyms.items()) |vsym| {
            release(self, vsym.value);
        }
        self.varSyms.clearRetainingCapacity();

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
            self.alloc.destroy(self.compiler);
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

        for (self.methodGroupExts.items()) |extra| {
            if (extra.nameIsOwned) {
                self.alloc.free(extra.getName());
            }
        }
        if (reset) {
            self.methods.clearRetainingCapacity();
            self.methodGroups.clearRetainingCapacity();
            self.methodGroupExts.clearRetainingCapacity();
            self.methodExts.clearRetainingCapacity();
            self.methodGroupKeys.clearRetainingCapacity();
            self.typeMethodGroups.clearRetainingCapacity();
            self.typeMethodGroupKeys.clearRetainingCapacity();
        } else {
            self.methods.deinit(self.alloc);
            self.methodGroups.deinit(self.alloc);
            self.methodGroupExts.deinit(self.alloc);
            self.methodExts.deinit(self.alloc);
            self.methodGroupKeys.deinit(self.alloc);
            self.typeMethodGroups.deinit(self.alloc);
            self.typeMethodGroupKeys.deinit(self.alloc);
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

        if (cy.Trace) {
            if (reset) {
                self.objectTraceMap.clearRetainingCapacity();
            } else {
                self.objectTraceMap.deinit(self.alloc);
                self.alloc.destroy(self.trace);
            }
        }

        if (reset) {
            self.varSymExtras.clearRetainingCapacity();
        } else {
            self.varSymExtras.deinit(self.alloc);
        }

        if (!reset) {
            if (cy.hasCLI) {
                self.httpClient.deinit();
                self.alloc.destroy(self.stdHttpClient);
            }
        }

        if (!reset) {
            self.deinited = true;
        }
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
        var tt = cy.debug.timer();
        const res = try self.compiler.compile(srcUri, src, config);
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
        var tt = cy.debug.timer();
        const res = try self.compiler.compile(srcUri, src, .{
            .enableFileModules = config.enableFileModules,
            .genDebugFuncMarkers = cy.Trace,
        });
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

        if (cy.verbose) {
            try debug.dumpBytecode(self, null);
        }

        if (cy.Trace) {
            var i: u32 = 0;
            while (i < self.trace.opCounts.len) : (i += 1) {
                self.trace.opCounts[i] = .{
                    .code = i,
                    .count = 0,
                };
            }
            self.trace.totalOpCounts = 0;
            self.trace.numReleases = 0;
            self.trace.numReleaseAttempts = 0;
            self.trace.numRetains = 0;
            self.trace.numRetainAttempts = 0;
            self.trace.numCycFrees = 0;
        }

        tt = cy.debug.timer();
        defer {
            tt.endPrint("eval");
            if (cy.Trace) {
                if (cy.verbose) {
                    self.dumpInfo() catch fatal();
                }
            }
        }

        return self.evalByteCode(res.buf);
    }

    pub fn dumpStats(self: *const VM) void {
        const S = struct {
            fn opCountLess(_: void, a: vmc.OpCount, b: vmc.OpCount) bool {
                return a.count > b.count;
            }
        };
        std.debug.print("total ops evaled: {}\n", .{self.trace.totalOpCounts});
        std.sort.pdq(vmc.OpCount, &self.trace.opCounts, {}, S.opCountLess);
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

        // Dump func symbols.
        {
            fmt.printStderr("func syms:\n", &.{});
            var iter = self.funcSymSigs.iterator();
            while (iter.next()) |it| {
                const key = it.key_ptr.*;
                const name = sema.getName(self.compiler, key.rtFuncSymKey.nameId);
                if (key.rtFuncSymKey.funcSigId == cy.NullId) {
                    fmt.printStderr("\t{}: {}\n", &.{v(name), v(it.value_ptr.*)});
                } else {
                    const sigStr = try sema.getFuncSigTempStr(self.compiler, key.rtFuncSymKey.funcSigId);
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

    pub fn getTypeName(vm: *const cy.VM, typeId: rt.TypeId) []const u8 {
        if (typeId == cy.NullId >> 2) {
            return "danglingObject";
        }
        const vmType = vm.types.buf[typeId];
        return vmType.namePtr[0..vmType.nameLen];
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

    pub fn evalByteCode(self: *VM, buf: cy.ByteCodeBuffer) !Value {
        if (buf.ops.items.len == 0) {
            return error.NoEndOp;
        }

        debug.freePanicPayload(self);
        self.curFiber.panicType = vmc.PANIC_NONE;
        self.debugTable = buf.debugTable.items;
        self.debugTempIndexTable = buf.debugTempIndexTable.items;
        self.unwindTempRegs = buf.unwindTempRegs.items;
        self.unwindTempPrevIndexes = buf.unwindTempPrevIndexes.items;

        // Set these last to hint location to cache before eval.
        self.pc = @ptrCast(buf.ops.items.ptr);
        try cy.fiber.stackEnsureTotalCapacity(self, buf.mainStackSize);
        self.framePtr = @ptrCast(self.stack.ptr);

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
        if (cy.Trace) {
            log.info("main stack size: {}", .{buf.mainStackSize});
        }

        if (self.endLocal == 255) {
            return Value.None;
        } else {
            return self.stack[self.endLocal];
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
            const nameId = try cy.sema.ensureNameSymExt(self.compiler, name, true);
            const key = rt.TypeKey.initTypeKey(cy.NullId, nameId);
            if (!self.typeSignatures.contains(key)) {
                return try self.addObjectTypeExt(cy.NullId, nameId, baseName, cy.NullId);
            }
        }
        return error.TooManyAttempts;
    }

    pub fn ensureObjectType(
        self: *VM, parentSymId: sema.SymbolId, nameId: sema.NameSymId, symId: sema.SymbolId
    ) !rt.TypeId {
        const key = rt.TypeKey.initTypeKey(parentSymId, nameId);
        const res = try @call(.never_inline, @TypeOf(self.typeSignatures).getOrPut, .{&self.typeSignatures, self.alloc, key });
        if (!res.found_existing) {
            const name = sema.getName(self.compiler, nameId);
            return self.addObjectTypeExt(parentSymId, nameId, name, symId);
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
        const id: u32 = @intCast(self.enums.len);
        try self.enums.append(self.alloc, s);
        try self.enumSignatures.put(self.alloc, name, id);
        return id;
    }

    pub inline fn getObjectTypeId(
        self: *const VM, parentSymId: sema.SymbolId, nameId: sema.NameSymId
    ) ?rt.TypeId {
        const key = rt.TypeKey.initTypeKey(parentSymId, nameId);
        return self.typeSignatures.get(key);
    }

    /// Can provide a different display name.
    pub fn addObjectTypeExt(
        self: *VM, parentSymId: sema.SymbolId,
        nameId: sema.NameSymId, name: []const u8, rTypeSymId: sema.SymbolId
    ) !rt.TypeId {
        const s = vmc.Type{
            .namePtr = name.ptr,
            .nameLen = name.len,
            .numFields = 0,
            .semaTypeId = rTypeSymId,
        };
        const id: u32 = @intCast(self.types.len);
        try self.types.append(self.alloc, s);
        const key = rt.TypeKey.initTypeKey(parentSymId, nameId);
        try self.typeSignatures.put(self.alloc, key, id);
        return id;
    }

    pub fn addBuiltinType(self: *VM, name: []const u8, semaTypeId: types.TypeId) !rt.TypeId {
        const nameId = try sema.ensureNameSym(self.compiler, name);
        return self.addObjectTypeExt(cy.NullId, nameId, name, semaTypeId);
    }

    pub inline fn getFuncSym(self: *const VM, parentSymId: u32, nameId: u32, funcSigId: sema.FuncSigId) ?SymbolId {
        const key = AbsFuncSigKey{
            .rtFuncSymKey = .{
                .parentSymId = parentSymId,
                .nameId = nameId,
                .funcSigId = funcSigId,
            },
        };
        return self.funcSymSigs.get(key);
    }

    pub inline fn getVarSym(self: *const VM, parentSymId: u32, nameId: u32) ?SymbolId {
        const key = rt.VarKey.initVarKey(parentSymId, nameId);
        return self.varSymSigs.get(key);
    }

    pub fn ensureVarSym(self: *VM, parentSymId: sema.SymbolId, nameId: u32) !SymbolId {
        const key = rt.VarKey.initVarKey(parentSymId, nameId);
        const res = try self.varSymSigs.getOrPut(self.alloc, key);
        if (!res.found_existing) {
            const id: u32 = @intCast(self.varSyms.len);
            try self.varSyms.append(self.alloc, rt.VarSym.init(Value.None));
            res.value_ptr.* = id;
            if (cy.Trace) {
                try self.varSymExtras.put(self.alloc, id, nameId);
            }
            return id;
        } else {
            return res.value_ptr.*;
        }
    }
    
    pub fn ensureFuncSym(self: *VM, parentSymId: sema.SymbolId, nameId: u32, funcSigId: sema.FuncSigId) !SymbolId {
        const key = RtFuncSymKey{
            .rtFuncSymKey = .{
                .parentSymId = parentSymId,
                .nameId = nameId,
                .funcSigId = funcSigId,
            },
        };
        const res = try self.funcSymSigs.getOrPut(self.alloc, key);
        if (!res.found_existing) {
            const id: u32 = @intCast(self.funcSyms.len);

            // const rFuncSig = self.compiler.sema.resolvedFuncSigs.items[funcSigId];
            // const typedFlag: u16 = if (rFuncSig.isTyped) 1 << 15 else 0;
            try self.funcSyms.append(self.alloc, .{
                .entryT = @intFromEnum(rt.FuncSymbolType.none),
                .innerExtra = .{
                    .none = .{
                        // .typedFlagNumParams = typedFlag | rFuncSig.numParams(),
                        // .funcSigId = @intCast(u16, funcSigId),
                        .funcSigId = funcSigId,
                    },
                },
                .inner = undefined,
            });
            try self.funcSymDetails.append(self.alloc, .{
                .nameId = nameId,
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
            const id: u32 = @intCast(self.syms.len);
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
        const nameId = try sema.ensureNameSym(self.compiler, name);
        const res = try self.fieldSymSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            const id: u32 = @intCast(self.fieldSyms.len);
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

    pub fn ensureMethodGroup(self: *VM, name: []const u8) !vmc.MethodGroupId {
        return self.ensureMethodGroup2(name, false);
    }

    pub fn ensureMethodGroup2(self: *VM, name: []const u8, dupeName: bool) !vmc.MethodGroupId {
        const nameId = try sema.ensureNameSym(self.compiler, name);
        const res = try @call(.never_inline, @TypeOf(self.methodGroupKeys).getOrPut, .{&self.methodGroupKeys, self.alloc, nameId});
        if (!res.found_existing) {
            const id: u32 = @intCast(self.methodGroups.len);

            try self.methodGroups.append(self.alloc, .{
                .mruTypeId = cy.NullId,
                .mruMethodType = undefined,
                .mruMethodData = undefined,
                .mruTypeMethodOverloaded = false,
            });
            var newName = name;
            if (dupeName) {
                newName = try self.alloc.dupe(u8, name);
            }
            try self.methodGroupExts.append(self.alloc, .{
                .mruTypeMethodGroupId = cy.NullId,
                .mruMethodId = cy.NullId,
                .initialFuncSigId = cy.NullId,
                .namePtr = newName.ptr,
                .nameLen = @intCast(newName.len),
                .nameIsOwned = dupeName,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn addFieldSym(self: *VM, sid: rt.TypeId, symId: SymbolId, offset: u16, typeSymId: sema.SymbolId) !void {
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

    pub inline fn setVarSym(self: *VM, symId: SymbolId, sym: rt.VarSym) void {
        self.varSyms.buf[symId] = sym;
    }

    pub inline fn setFuncSym(self: *VM, symId: SymbolId, sym: rt.FuncSymbol) void {
        self.funcSyms.buf[symId] = sym;
    }

    pub fn ensureTypeMethodGroup(self: *VM, typeId: vmc.TypeId, mgId: vmc.MethodGroupId) !vmc.TypeMethodGroupId {
        const key = rt.TypeMethodGroupKey.initTypeMethodGroupKey(typeId, mgId);
        const res = try self.typeMethodGroupKeys.getOrPut(self.alloc, key);
        if (!res.found_existing) {
            const id: vmc.TypeMethodGroupId = @intCast(self.typeMethodGroups.len);
            try self.typeMethodGroups.append(self.alloc, .{
                .mruMethodId = cy.NullId,
                .head = cy.NullId,
                .tail = cy.NullId,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    fn storeMethod(self: *VM, tmgId: vmc.TypeMethodGroupId, method: rt.Method, funcSigId: sema.FuncSigId) !vmc.MethodId {
        const methodId: vmc.MethodId = @intCast(self.methods.len);
        try self.methods.append(self.alloc, method);
        try self.methodExts.append(self.alloc, .{ .funcSigId = funcSigId });

        const tmg = &self.typeMethodGroups.buf[tmgId];
        if (tmg.head == cy.NullId) {
            tmg.head = methodId;
            tmg.tail = methodId;
            tmg.mruMethodId = methodId;
        } else {
            self.methods.buf[tmg.tail].next = methodId;
            tmg.tail = methodId;
        }
        return methodId;
    }

    pub fn addMethod(self: *VM, id: rt.TypeId, mgId: vmc.MethodGroupId, method: rt.MethodInit) !void {
        const mg = &self.methodGroups.buf[mgId];
        const mgExt = &self.methodGroupExts.buf[mgId];
        if (mg.mruTypeId != cy.NullId) {
            if (mgExt.mruTypeMethodGroupId == cy.NullId) {
                // Store existing method.
                const tmgId = try self.ensureTypeMethodGroup(mg.mruTypeId, mgId);
                const methodId = try self.storeMethod(tmgId, rt.Method{
                    .type = mg.mruMethodType,
                    .data = mg.mruMethodData,
                    .next = cy.NullId,
                }, method.funcSigId);
                mgExt.mruTypeMethodGroupId = tmgId;
                mgExt.mruMethodId = methodId;
            }

            const tmgId = if (mg.mruTypeId == id) mgExt.mruTypeMethodGroupId
                else try self.ensureTypeMethodGroup(id, mgId);
            _ = try self.storeMethod(tmgId, rt.Method{
                .type = method.type,
                .data = method.data,
                .next = cy.NullId,
            }, method.funcSigId);

            if (mg.mruTypeId == id and !mg.mruTypeMethodOverloaded) {
                const tmg = self.typeMethodGroups.buf[tmgId];
                mg.mruTypeMethodOverloaded = tmg.head != tmg.tail;
            }
        } else {
            // Empty method group.
            mg.* = .{
                .mruTypeId = id,
                .mruMethodType = method.type,
                .mruMethodData = method.data,
                .mruTypeMethodOverloaded = false,
            };
            mgExt.initialFuncSigId = method.funcSigId;
        }
    }

    fn panicFmt(self: *VM, format: []const u8, args: []const fmt.FmtValue) error{Panic} {
        @setCold(true);
        const msg = fmt.allocFormat(self.alloc, format, args) catch |err| {
            if (err == error.OutOfMemory) {
                self.curFiber.panicType = vmc.PANIC_INFLIGHT_OOM;
                return error.Panic;
            } else {
                cy.panic("unexpected");
            }
        };
        self.curFiber.panicPayload = @as(u64, @intFromPtr(msg.ptr)) | (@as(u64, msg.len) << 48);
        self.curFiber.panicType = vmc.PANIC_MSG;
        log.debug("{s}", .{msg});
        return error.Panic;
    }

    pub fn interruptThrowSymbol(self: *VM, sym: bindings.Symbol) error{Panic} {
        self.curFiber.panicPayload = Value.initErrorSymbol(@intFromEnum(sym)).val;
        self.curFiber.panicType = vmc.PANIC_NATIVE_THROW;
        return error.Panic;
    }

    pub fn panicWithUncaughtError(self: *VM, err: Value) error{Panic} {
        @setCold(true);
        self.curFiber.panicPayload = err.val;
        self.curFiber.panicType = vmc.PANIC_UNCAUGHT_ERROR;
        return error.Panic;
    }

    fn panic(self: *VM, comptime msg: []const u8) error{Panic, OutOfMemory} {
        @setCold(true);
        const dupe = try self.alloc.dupe(u8, msg);
        self.curFiber.panicPayload = @as(u64, @intFromPtr(dupe.ptr)) | (@as(u64, dupe.len) << 48);
        self.curFiber.panicType = vmc.PANIC_MSG;
        log.debug("{s}", .{dupe});
        return error.Panic;
    }

    fn setField(self: *VM, recv: Value, fieldId: SymbolId, val: Value) linksection(cy.HotSection) !void {
        if (recv.isPointer()) {
            const obj = recv.asHeapObject();
            const symMap = &self.fieldSyms.buf[fieldId];

            if (obj.getTypeId() == symMap.mruTypeId) {
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
            return @intCast(res.offset);
        } else {
            return cy.NullU8;
        }
    }

    pub fn getFieldOffset(self: *VM, obj: *HeapObject, symId: SymbolId) linksection(cy.HotSection) u8 {
        const symMap = self.fieldSyms.buf[symId];
        if (obj.getTypeId() == symMap.mruTypeId) {
            return @intCast(symMap.mruOffset);
        } else {
            return @call(.never_inline, VM.getFieldOffsetFromTable, .{self, obj.getTypeId(), symId});
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
        const name = sema.getName(self.compiler, nameId);
        if (obj.getTypeId() == rt.MapT) {
            const map = cy.ptrAlignCast(*const cy.MapInner, &obj.map.inner);
            if (map.getByString(self, name)) |val| {
                return val;
            } else return Value.None;
        } else {
            log.debug("Missing symbol for object: {}", .{obj.getTypeId()});
            return Value.None;
        }
    }

    /// startLocal points to the first arg in the current stack frame.
    fn callSym(
        self: *VM, pc: [*]cy.Inst, framePtr: [*]Value, symId: SymbolId, startLocal: u8,
        numArgs: u8, reqNumRetVals: u2
    ) linksection(cy.HotSection) !cy.fiber.PcSp {
        const sym = self.funcSyms.buf[symId];
        switch (@as(rt.FuncSymbolType, @enumFromInt(sym.entryT))) {
            .hostFunc => {
                const newFramePtr = framePtr + startLocal;

                // Optimize.
                pc[0] = cy.Inst.initOpCode(.callNativeFuncIC);
                @as(*align(1) u48, @ptrCast(pc + 6)).* = @intCast(@intFromPtr(sym.inner.hostFunc));

                self.pc = pc;
                self.framePtr = framePtr;
                const res: Value = @bitCast(sym.inner.hostFunc.?(@ptrCast(self), @ptrCast(newFramePtr + 4), numArgs));
                if (res.isInterrupt()) {
                    return error.Panic;
                }
                if (reqNumRetVals == 1) {
                    newFramePtr[0] = @bitCast(res);
                } else {
                    switch (reqNumRetVals) {
                        0 => {
                            // Nop.
                        },
                        1 => cy.panic("not possible"),
                        else => cy.panic("unsupported"),
                    }
                }
                return cy.fiber.PcSp{
                    .pc = pc + cy.bytecode.CallSymInstLen,
                    .sp = framePtr,
                };
            },
            .hostQuickenFunc => {
                const newFramePtr = framePtr + startLocal;
                sym.inner.hostQuickenFunc(@ptrCast(self), pc, @ptrCast(newFramePtr + 4), numArgs);
                return cy.fiber.PcSp{
                    .pc = pc,
                    .sp = framePtr,
                };
            },
            .func => {
                if (@intFromPtr(framePtr + startLocal + sym.inner.func.stackSize) >= @intFromPtr(self.stackEndPtr)) {
                    return error.StackOverflow;
                }

                // Optimize.
                pc[0] = cy.Inst.initOpCode(.callFuncIC);
                pc[4] = cy.Inst{ .val = @intCast(sym.inner.func.stackSize) };
                @as(*align(1) u48, @ptrCast(pc + 6)).* = @intCast(@intFromPtr(cy.fiber.toVmPc(self, sym.inner.func.pc)));

                const newFramePtr = framePtr + startLocal;
                newFramePtr[1] = buildReturnInfo(reqNumRetVals, true, cy.bytecode.CallSymInstLen);
                newFramePtr[2] = Value{ .retPcPtr = pc + cy.bytecode.CallSymInstLen };
                newFramePtr[3] = Value{ .retFramePtr = framePtr };
                return cy.fiber.PcSp{
                    .pc = cy.fiber.toVmPc(self, sym.inner.func.pc),
                    .sp = newFramePtr,
                };
            },
            .closure => {
                if (@intFromPtr(framePtr + startLocal + sym.inner.closure.stackSize) >= @intFromPtr(self.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const newFp = framePtr + startLocal;
                newFp[1] = buildReturnInfo(reqNumRetVals, true, cy.bytecode.CallSymInstLen);
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

    fn reportMissingFuncSym(vm: *VM, pc: [*]const cy.Inst, rtSymId: SymbolId) error{Panic, OutOfMemory} {
        const relPc = getInstOffset(vm, pc);
        if (debug.getDebugSymByPc(vm, relPc)) |sym| {
            const chunk = vm.compiler.chunks.items[sym.file];
            const callExpr = chunk.nodes[sym.loc];
            const callee = chunk.nodes[callExpr.head.callExpr.callee];
            var csymId: sema.CompactSymbolId = undefined;
            if (callee.node_t == .accessExpr) {
                csymId = @bitCast(callee.head.accessExpr.sema_csymId);
            } else if (callee.node_t == .ident) {
                csymId = @bitCast(callee.head.ident.sema_csymId);
            } else {
                return vm.panic("Failed to generate error report.");
            }
            const funcSymId = csymId.id;

            const rFuncSym = chunk.compiler.sema.resolvedFuncSyms.items[funcSymId];
            const funcChunk = &vm.compiler.chunks.items[rFuncSym.chunkId];
            const func = funcChunk.semaFuncDecls.items[rFuncSym.declId];

            const name = func.getName(funcChunk);
            const rSym = chunk.compiler.sema.resolvedSyms.items[func.inner.staticFunc.semaSymId];

            if (rSym.symT == .func) {
                const rtSym = vm.funcSyms.buf[rtSymId];
                if (rSym.inner.func.funcSymId == cy.NullId) {
                    const sigStr = try sema.getFuncSigTempStr(vm.compiler, rtSym.innerExtra.none.funcSigId);
                    return vm.panicFmt("Unsupported call signature: `{}{}`.\nThere are multiple overloaded functions named `{}`", &.{
                        v(name), v(sigStr), v(name),
                    });
                } else {
                    const rFuncSym_ = vm.compiler.sema.getFuncSym(rSym.inner.func.funcSymId);

                    vm.u8Buf.clearRetainingCapacity();
                    const w = vm.u8Buf.writer(vm.alloc);
                    cy.sema.writeFuncSigStr(vm.compiler, w, rtSym.innerExtra.none.funcSigId) catch fatal();
                    const rtSigStr = vm.u8Buf.items();
                    const start = vm.u8Buf.len;
                    cy.sema.writeFuncSigStr(vm.compiler, w, rFuncSym_.getFuncSigId()) catch fatal();
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

    fn getTypeMethodGroup(
        self: *VM, typeId: rt.TypeId, mgId: vmc.MethodGroupId,
    ) ?vmc.TypeMethodGroupId {
        const key = rt.TypeMethodGroupKey.initTypeMethodGroupKey(typeId, mgId);
        return self.typeMethodGroupKeys.get(key);
    }

    fn getCachedMethodGroupForType(self: *VM, typeId: vmc.TypeId, mgId: vmc.MethodGroupId) linksection(cy.HotSection) !?rt.MethodGroup {
        const mg = self.methodGroups.buf[mgId];
        if (mg.mruTypeId == typeId) {
            return mg;
        } else {
            if (@call(.never_inline, getTypeMethodGroup, .{self, typeId, mgId})) |tmgId| {
                const mgExt = &self.methodGroupExts.buf[mgId];

                if (mg.mruTypeId != cy.NullId) {
                    // Persist most recent TypeMethodGroup data.
                    if (mgExt.mruMethodId == cy.NullId) {
                        const curTmgId = try self.ensureTypeMethodGroup(mg.mruTypeId, mgId);
                        _ = try self.storeMethod(curTmgId, rt.Method{
                            .type = mg.mruMethodType,
                            .data = mg.mruMethodData,
                            .next = cy.NullId,
                        }, mgExt.initialFuncSigId);
                    } else {
                        self.typeMethodGroups.buf[mgExt.mruTypeMethodGroupId].mruMethodId = mgExt.mruMethodId;
                    }
                }

                const newTmg = self.typeMethodGroups.buf[tmgId];
                const newMethodId = newTmg.mruMethodId;
                const newMethod = self.methods.buf[newMethodId];
                const new = rt.MethodGroup{
                    .mruTypeId = typeId,
                    .mruMethodType = newMethod.type,
                    .mruMethodData = newMethod.data,
                    .mruTypeMethodOverloaded = newTmg.head != newTmg.tail,
                };
                self.methodGroups.buf[mgId] = new;
                mgExt.mruMethodId = newMethodId;
                mgExt.mruTypeMethodGroupId = tmgId;
                return new;
            } else {
                return null;
            }
        }
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
            if (obj.getTypeId() == rt.AstringT) {
                return obj.astring.getConstSlice();
            } else if (obj.getTypeId() == rt.UstringT) {
                return obj.ustring.getConstSlice();
            } else if (obj.getTypeId() == rt.StringSliceT) {
                return cy.heap.StringSlice.getConstSlice(obj.stringSlice);
            } else if (obj.getTypeId() == rt.RawstringT) {
                return obj.rawstring.getConstSlice();
            } else if (obj.getTypeId() == rt.RawstringSliceT) {
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
                return cy.heap.StringSlice.getConstSlice(obj.stringSlice);
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
            if (obj.getTypeId() == rt.AstringT) {
                return obj.astring.getConstSlice();
            } else if (obj.getTypeId() == rt.UstringT) {
                return obj.ustring.getConstSlice();
            } else if (obj.getTypeId() == rt.StringSliceT) {
                return cy.heap.StringSlice.getConstSlice(obj.stringSlice);
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
        const typeId = val.getTypeId();
        switch (typeId) {
            rt.FloatT => {
                const f = val.asF64();
                const start = writer.pos();
                if (Value.floatIsSpecial(f)) {
                    std.fmt.format(writer, "{}", .{f}) catch cy.fatal();
                } else {
                    if (Value.floatCanBeInteger(f)) {
                        std.fmt.format(writer, "{d:.0}", .{f}) catch cy.fatal();
                    } else {
                        std.fmt.format(writer, "{d}", .{f}) catch cy.fatal();
                    }
                }
                const slice = writer.sliceFrom(start);
                if (getCharLen) {
                    outCharLen.* = @intCast(slice.len);
                }
                return slice;
            },
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
                std.fmt.format(writer, "error.{s}", .{self.getSymbolName(symId)}) catch cy.fatal();
                const slice = writer.sliceFrom(start);
                if (getCharLen) {
                    outCharLen.* = @intCast(slice.len);
                }
                return slice;
            },
            rt.StaticAstringT => {
                const slice = val.asStaticStringSlice();
                const buf = self.strBuf[slice.start..slice.end];
                if (getCharLen) {
                    outCharLen.* = @intCast(buf.len);
                }
                return buf;
            },
            rt.StaticUstringT => {
                const slice = val.asStaticStringSlice();
                if (getCharLen) {
                    outCharLen.* = @as(*align (1) cy.StaticUstringHeader, @ptrCast(self.strBuf.ptr + slice.start - 12)).charLen;
                }
                return self.strBuf[slice.start..slice.end];
            },
            rt.EnumT => {
                const start = writer.pos();
                const enumv = val.asEnum();
                const enumSym = self.enums.buf[enumv.enumId];
                const memberName = sema.getName(self.compiler, enumSym.members[enumv.memberId]);
                std.fmt.format(writer, "{s}.{s}", .{enumSym.name, memberName}) catch cy.fatal();
                const slice = writer.sliceFrom(start);
                if (getCharLen) {
                    outCharLen.* = @intCast(slice.len);
                }
                return slice;
            },
            rt.SymbolT => {
                const start = writer.pos();
                const litId = val.asSymbolId();
                std.fmt.format(writer, ".{s}", .{self.getSymbolName(litId)}) catch cy.fatal();
                const slice = writer.sliceFrom(start);
                if (getCharLen) {
                    outCharLen.* = @intCast(slice.len);
                }
                return slice;
            },
            rt.IntegerT => {
                const start = writer.pos();
                std.fmt.format(writer, "{}", .{val.asInteger()}) catch cy.fatal();
                const slice = writer.sliceFrom(start);
                if (getCharLen) {
                    outCharLen.* = @intCast(slice.len);
                }
                return slice;
            },
            else => {
                const obj = val.asHeapObject();
                switch (typeId) {
                    rt.AstringT => {
                        const res = obj.astring.getConstSlice();
                        if (getCharLen) {
                            outCharLen.* = @intCast(res.len);
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
                            if (cy.heap.StringSlice.isAstring(obj.stringSlice)) {
                                outCharLen.* = obj.stringSlice.len;
                            } else {
                                outCharLen.* = obj.stringSlice.uCharLen;
                            }
                        }
                        return cy.heap.StringSlice.getConstSlice(obj.stringSlice);
                    },
                    rt.RawstringT => {
                        const start = writer.pos();
                        std.fmt.format(writer, "rawstring ({})", .{obj.rawstring.len}) catch cy.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(slice.len);
                        }
                        return slice;
                    },
                    rt.RawstringSliceT => {
                        const start = writer.pos();
                        std.fmt.format(writer, "rawstring ({})", .{obj.rawstringSlice.len}) catch cy.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(slice.len);
                        }
                        return slice;
                    },
                    rt.ListT => {
                        const start = writer.pos();
                        std.fmt.format(writer, "List ({})", .{obj.list.list.len}) catch cy.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(slice.len);
                        }
                        return slice;
                    },
                    rt.MapT => {
                        const start = writer.pos();
                        std.fmt.format(writer, "Map ({})", .{obj.map.inner.size}) catch cy.fatal();
                        const slice = writer.sliceFrom(start);
                        if (getCharLen) {
                            outCharLen.* = @intCast(slice.len);
                        }
                        return slice;
                    },
                    rt.MetaTypeT => {
                        const start = writer.pos();
                        const symType: cy.heap.MetaTypeKind = @enumFromInt(obj.metatype.type);
                        var slice: []const u8 = undefined;
                        if (symType == .object) {
                            const vmType = self.types.buf[obj.metatype.symId];
                            const name = vmType.namePtr[0..vmType.nameLen];
                            std.fmt.format(writer, "type: {s}", .{name}) catch cy.fatal();
                            slice = writer.sliceFrom(start);
                        } else {
                            slice = "Unknown Symbol";
                        }
                        if (getCharLen) {
                            outCharLen.* = @intCast(slice.len);
                        }
                        return slice;
                    },
                    else => {
                        const vmType = self.types.buf[obj.getTypeId()];
                        const buf = vmType.namePtr[0..vmType.nameLen];
                        if (getCharLen) {
                            outCharLen.* = @intCast(buf.len);
                        }
                        return buf;
                    }
                }
            }
        }
    }

    pub fn writeValueToString(self: *const VM, writer: anytype, val: Value) void {
        const str = self.valueToTempString(val);
        _ = writer.write(str) catch cy.fatal();
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
        if (obj.getTypeId() == rt.AstringT) {
            return .astring;
        } else if (obj.getTypeId() == rt.UstringT) {
            return .ustring;
        } else if (obj.getTypeId() == rt.StringSliceT) {
            return .slice;
        } else if (obj.getTypeId() == rt.RawstringT) {
            return .rawstring;
        } else if (obj.getTypeId() == rt.RawstringSliceT) {
            return .rawSlice;
        }
        return null;
    } else {
        if (val.isFloat()) {
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
    if (val.isFloat()) {
        return val.asF64();
    } else if (val.isInteger()) {
        return @floatFromInt(val.asInteger());
    } else {
        return @call(.never_inline, panicConvertFloatError, .{vm, val});
    }
}

pub fn panicDivisionByZero(vm: *cy.VM) error{Panic, OutOfMemory} {
    @setCold(true);
    return vm.panic("Division by zero.");
}

pub fn panicExpectedInteger(vm: *cy.VM) error{Panic, OutOfMemory} {
    @setCold(true);
    return vm.panic("Expected integer operand.");
}

pub fn panicExpectedFloat(vm: *cy.VM) error{Panic, OutOfMemory} {
    @setCold(true);
    return vm.panic("Expected float operand.");
}

fn panicConvertFloatError(vm: *cy.VM, val: Value) error{Panic, OutOfMemory} {
    @setCold(true);
    const typeId = val.getTypeId();
    return vm.panicFmt("Cannot convert `{}` to float.", &.{v(vm.types.buf[typeId].name)});
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

test "vm internals." {
    try t.eq(@alignOf(VM), 8);

    try t.eq(@sizeOf(AbsFuncSigKey), 16);

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
    if (cy.hasGC) {
        try t.eq(@offsetOf(VM, "cyclableHead"), @offsetOf(vmc.VM, "cyclableHead"));
    }
    try t.eq(@offsetOf(VM, "tryStack"), @offsetOf(vmc.VM, "tryStack"));
    if (cy.TrackGlobalRC) {
        try t.eq(@offsetOf(VM, "refCounts"), @offsetOf(vmc.VM, "refCounts"));
    }
    try t.eq(@offsetOf(VM, "methods"), @offsetOf(vmc.VM, "methods"));
    try t.eq(@offsetOf(VM, "methodGroups"), @offsetOf(vmc.VM, "methodGroups"));
    try t.eq(@offsetOf(VM, "methodGroupKeys"), @offsetOf(vmc.VM, "methodGroupKeys"));
    try t.eq(@offsetOf(VM, "typeMethodGroups"), @offsetOf(vmc.VM, "typeMethodGroups"));
    try t.eq(@offsetOf(VM, "typeMethodGroupKeys"), @offsetOf(vmc.VM, "typeMethodGroupKeys"));
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
    try t.eq(@offsetOf(VM, "methodGroupExts"), @offsetOf(vmc.VM, "methodGroupExts"));
    try t.eq(@offsetOf(VM, "methodExts"), @offsetOf(vmc.VM, "methodExts"));
    try t.eq(@offsetOf(VM, "debugTable"), @offsetOf(vmc.VM, "debugTablePtr"));
    try t.eq(@offsetOf(VM, "debugTempIndexTable"), @offsetOf(vmc.VM, "debugTempIndexTablePtr"));
    try t.eq(@offsetOf(VM, "unwindTempRegs"), @offsetOf(vmc.VM, "unwindTempRegsPtr"));
    try t.eq(@offsetOf(VM, "unwindTempPrevIndexes"), @offsetOf(vmc.VM, "unwindTempPrevIndexesPtr"));
    try t.eq(@offsetOf(VM, "curFiber"), @offsetOf(vmc.VM, "curFiber"));
    try t.eq(@offsetOf(VM, "mainFiber"), @offsetOf(vmc.VM, "mainFiber"));
    try t.eq(@offsetOf(VM, "throwTrace"), @offsetOf(vmc.VM, "throwTrace"));
    try t.eq(@offsetOf(VM, "compiler"), @offsetOf(vmc.VM, "compiler"));
    try t.eq(@offsetOf(VM, "userData"), @offsetOf(vmc.VM, "userData"));
    try t.eq(@offsetOf(VM, "print"), @offsetOf(vmc.VM, "print"));
    try t.eq(@offsetOf(VM, "iteratorMGID"), @offsetOf(vmc.VM, "padding"));
    try t.eq(@offsetOf(VM, "httpClient"), @offsetOf(vmc.VM, "httpClient"));
    try t.eq(@offsetOf(VM, "stdHttpClient"), @offsetOf(vmc.VM, "stdHttpClient"));
    try t.eq(@offsetOf(VM, "varSymExtras"), @offsetOf(vmc.VM, "varSymExtras"));
    try t.eq(@offsetOf(VM, "expGlobalRC"), @offsetOf(vmc.VM, "expGlobalRC"));

    if (cy.Trace) {
        try t.eq(@offsetOf(VM, "trace"), @offsetOf(vmc.VM, "trace"));
        try t.eq(@offsetOf(VM, "objectTraceMap"), @offsetOf(vmc.VM, "objectTraceMap"));
        try t.eq(@offsetOf(VM, "debugPc"), @offsetOf(vmc.VM, "debugPc"));
    }
}

const EnumId = u32;
const Enum = struct {
    name: []const u8,
    members: []const sema.NameSymId,
};

pub const SymbolId = u32;

const Root = @This();

/// To reduce the amount of code inlined in the hot loop, handle StackOverflow at the top and resume execution.
/// This is also the entry way for native code to call into the VM, assuming pc, framePtr, and virtual registers are already set.
pub fn evalLoopGrowStack(vm: *VM) linksection(cy.HotSection) error{StackOverflow, OutOfMemory, Panic, NoDebugSym, End}!void {
    if (comptime build_options.vmEngine == .zig) {
        while (true) {
            @call(.always_inline, evalLoop, .{vm}) catch |err| {
                if (err == error.StackOverflow) {
                    log.debug("grow stack", .{});
                    try @call(.never_inline, cy.fiber.growStackAuto, .{vm});
                    continue;
                } else if (err == error.End) {
                    return;
                } else if (err == error.Panic) {
                    if (vm.curFiber.panicType == vmc.PANIC_NATIVE_THROW) {
                        if (try @call(.never_inline, cy.fiber.throw, .{vm, vm.framePtr, vm.pc, Value.initRaw(vm.curFiber.panicPayload) })) |res| {
                            vm.pc = res.pc;
                            vm.framePtr = res.sp;
                            continue;
                        } else {
                            vm.curFiber.panicType = vmc.PANIC_UNCAUGHT_ERROR;
                        }
                    }
                    try @call(.never_inline, debug.buildStackTrace, .{vm});
                    return error.Panic;
                } else return err;
            };
            return;
        }
    } else if (comptime build_options.vmEngine == .c) {
        while (true) {
            const res = vmc.execBytecode(@ptrCast(vm));
            if (res == vmc.RES_CODE_PANIC) {
                if (vm.curFiber.panicType == vmc.PANIC_NATIVE_THROW) {
                    if (try @call(.never_inline, cy.fiber.throw, .{vm, vm.framePtr, vm.pc, Value.initRaw(vm.curFiber.panicPayload) })) |throwRes| {
                        vm.pc = throwRes.pc;
                        vm.framePtr = throwRes.sp;
                        continue;
                    } else {
                        vm.curFiber.panicType = vmc.PANIC_UNCAUGHT_ERROR;
                    }
                }
                try @call(.never_inline, debug.buildStackTrace, .{vm});
                return error.Panic;
            } else if (res == vmc.RES_CODE_STACK_OVERFLOW) {
                log.debug("grow stack", .{});
                try @call(.never_inline, cy.fiber.growStackAuto, .{vm});
                continue;
            } else if (res == vmc.RES_CODE_UNKNOWN) {
                log.debug("Unknown error.", .{});
                return vm.panic("Unknown error code.");
            }
            return;
        }
    } else {
        @compileError("Unsupported engine.");
    }
}

/// Generate assembly labels to find sections easier.
const GenLabels = builtin.mode != .Debug and !builtin.cpu.arch.isWasm() and false;

const DebugTraceStopAtNumOps: ?u32 = null;

fn evalLoop(vm: *VM) linksection(cy.HotSection) error{StackOverflow, OutOfMemory, Panic, NoDebugSym, End}!void {
    if (GenLabels) {
        _ = asm volatile ("LEvalLoop:"::);
    }

    var pc = vm.pc;
    var framePtr = vm.framePtr;
    defer {
        vm.pc = pc;
        vm.framePtr = framePtr;
        if (cy.Trace) {
            vm.debugPc = getInstOffset(vm, pc);
        }
    } 

    while (true) {
        if (cy.Trace) {
            const op = pc[0].opcode();
            vm.trace.opCounts[@intFromEnum(op)].count += 1;
            vm.trace.totalOpCounts += 1;
        }
        if (cy.Trace) {
            if (cy.verbose) {
                dumpEvalOp(vm, pc) catch cy.fatal();
            }
            vm.debugPc = getInstOffset(vm, pc);
            if (DebugTraceStopAtNumOps) |target| {
                if (vm.trace.totalOpCounts == target) {
                    return vm.panic("DebugTraceStop reached.");
                }
            }
        }
        if (GenLabels) {
            // _ = asm volatile ("LOpBase:"::);
        }
        switch (pc[0].opcode()) {
            .none => {
                if (GenLabels) {
                    _ = asm volatile ("LOpNone:"::);
                }
                framePtr[pc[1].val] = Value.None;
                pc += 2;
                continue;
            },
            .constOp => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstOp:"::);
                }
                const idx = @as(*align (1) u16, @ptrCast(pc + 1)).*;
                framePtr[pc[3].val] = Value.initRaw(vm.consts[idx].val);
                pc += 4;
                continue;
            },
            .constI8 => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstI8:"::);
                }
                framePtr[pc[2].val] = Value.initInt(@intCast(@as(i8, @bitCast(pc[1].val))));
                pc += 3;
                continue;
            },
            .release => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRelease:"::);
                }
                release(vm, framePtr[pc[1].val]);
                pc += 2;
                continue;
            },
            .releaseN => {
                if (GenLabels) {
                    _ = asm volatile ("LOpReleaseN:"::);
                }
                const numLocals = pc[1].val;
                for (pc[2..2+numLocals]) |local| {
                    release(vm, framePtr[local.val]);
                }
                pc += 2 + numLocals;
                continue;
            },
            .fieldIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFieldIC:"::);
                }
                const recv = framePtr[pc[1].val];
                const dst = pc[2].val;
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    if (obj.getTypeId() == @as(*align (1) u16, @ptrCast(pc + 5)).*) {
                        framePtr[dst] = obj.object.getValue(pc[7].val);
                        pc += 8;
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.Inst.initOpCode(.field);
                continue;
            },
            .copyRetainSrc => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCopyRetainSrc:"::);
                }
                const val = framePtr[pc[1].val];
                framePtr[pc[2].val] = val;
                retain(vm, val);
                pc += 3;
                continue;
            },
            .jumpNotCond => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJumpNotCond:"::);
                }
                const cond = framePtr[pc[1].val];
                const condVal = if (cond.isBool()) b: {
                    break :b cond.asBool();
                } else b: {
                    break :b cond.assumeNotBoolToBool();
                };
                if (!condVal) {
                    pc += @as(*const align(1) u16, @ptrCast(pc + 2)).*;
                } else {
                    pc += 4;
                }
                continue;
            },
            .neg => {
                if (GenLabels) {
                    _ = asm volatile ("LOpNeg:"::);
                }
                const dst = &framePtr[pc[1].val];
                const val = dst.*;
                if (val.isFloat()) {
                    dst.* = Value.initF64(-val.asF64());
                    pc += 2;
                    continue;
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
            },
            .compare => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCompare:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                // Can immediately match numbers, objects, primitives.
                framePtr[pc[3].val] = if (left.val == right.val) Value.True else 
                    @call(.never_inline, evalCompare, .{vm, left, right});
                pc += 4;
                continue;
            },
            .compareNot => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCompareNot:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                // Can immediately match numbers, objects, primitives.
                framePtr[pc[3].val] = if (left.val == right.val) Value.False else 
                    @call(.never_inline, evalCompareNot, .{vm, left, right});
                pc += 4;
                continue;
            },
            // .lessNumber => {
            //     @setRuntimeSafety(debug);
            //     const left = vm.stack[vm.framePtr + pc[1].val];
            //     const right = vm.stack[vm.framePtr + pc[2].val];
            //     const dst = pc[3].val;
            //     pc += 4;
            //     vm.stack[vm.framePtr + dst] = Value.initBool(left.asF64() < right.asF64());
            //     continue;
            // },
            .addFloat => {
                if (GenLabels) {
                    _ = asm volatile ("LOpAddFloat:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    framePtr[pc[3].val] = Value.initF64(left.asF64() + right.asF64());
                    pc += cy.bytecode.CallObjSymInstLen;
                    continue;
                } else {
                    if (pc[4].val == 1) {
                        deoptimizeBinOp(pc);
                        continue;
                    } else {
                        return @call(.never_inline, panicExpectedFloat, .{vm});
                    }
                }
            },
            .addInt => {
                if (GenLabels) {
                    _ = asm volatile ("LOpAddInt:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothIntegers(left, right)) {
                    framePtr[pc[3].val] = Value.initI32(left.asInteger() + right.asInteger());
                    pc += cy.bytecode.CallObjSymInstLen;
                    continue;
                } else {
                    if (pc[4].val == 1) {
                        deoptimizeBinOp(pc);
                        continue;
                    } else {
                        return @call(.never_inline, panicExpectedInteger, .{vm});
                    }
                }
            },
            .subFloat => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSubFloat:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    framePtr[pc[3].val] = Value.initF64(left.asF64() - right.asF64());
                    pc += cy.bytecode.CallObjSymInstLen;
                    continue;
                } else {
                    if (pc[4].val == 1) {
                        deoptimizeBinOp(pc);
                        continue;
                    } else {
                        return @call(.never_inline, panicExpectedFloat, .{vm});
                    }
                }
            },
            .subInt => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSubInt:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothIntegers(left, right)) {
                    framePtr[pc[3].val] = Value.initI32(left.asInteger() - right.asInteger());
                    pc += cy.bytecode.CallObjSymInstLen;
                    continue;
                } else {
                    if (pc[4].val == 1) {
                        deoptimizeBinOp(pc);
                        continue;
                    } else {
                        return @call(.never_inline, panicExpectedInteger, .{vm});
                    }
                }
            },
            .less => {
                if (GenLabels) {
                    _ = asm volatile ("LOpLess:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    framePtr[pc[3].val] = Value.initBool(left.asF64() < right.asF64());
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .lessInt => {
                if (GenLabels) {
                    _ = asm volatile ("LOpLessInt:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                framePtr[pc[3].val] = Value.initBool(left.asInteger() < right.asInteger());
                pc += 4;
                continue;
            },
            .greater => {
                if (GenLabels) {
                    _ = asm volatile ("LOpGreater:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    framePtr[pc[3].val] = Value.initBool(left.asF64() > right.asF64());
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .lessEqual => {
                if (GenLabels) {
                    _ = asm volatile ("LOpLessEqual:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    framePtr[pc[3].val] = Value.initBool(left.asF64() <= right.asF64());
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .greaterEqual => {
                if (GenLabels) {
                    _ = asm volatile ("LOpGreaterEqual:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    framePtr[pc[3].val] = Value.initBool(left.asF64() >= right.asF64());
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .true => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTrue:"::);
                }
                framePtr[pc[1].val] = Value.True;
                pc += 2;
                continue;
            },
            .false => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFalse:"::);
                }
                framePtr[pc[1].val] = Value.False;
                pc += 2;
                continue;
            },
            .not => {
                if (GenLabels) {
                    _ = asm volatile ("LOpNot:"::);
                }
                const dst = &framePtr[pc[1].val];
                const val = dst.*;

                const bval = if (val.isBool()) b: {
                    break :b val.asBool();
                } else b: {
                    break :b val.assumeNotBoolToBool();
                };
                dst.* = Value.initBool(!bval);
                pc += 2;
                continue;
            },
            .stringTemplate => {
                if (GenLabels) {
                    _ = asm volatile ("LOpStringTemplate:"::);
                }
                const startLocal = pc[1].val;
                const exprCount = pc[2].val;
                const dst = pc[3].val;
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
                const startLocal = pc[1].val;
                const numElems = pc[2].val;
                const dst = pc[3].val;
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
                const dst = pc[1].val;
                pc += 2;
                framePtr[dst] = try cy.heap.allocEmptyMap(vm);
                continue;
            },
            .objectSmall => {
                if (GenLabels) {
                    _ = asm volatile ("LOpObjectSmall:"::);
                }
                const sid = pc[1].val;
                const startLocal = pc[2].val;
                const numFields = pc[3].val;
                const fields = framePtr[startLocal .. startLocal + numFields];
                framePtr[pc[4].val] = try cy.heap.allocObjectSmall(vm, sid, fields);
                pc += 5;
                continue;
            },
            .object => {
                if (GenLabels) {
                    _ = asm volatile ("LOpObject:"::);
                }
                const sid = pc[1].val;
                const startLocal = pc[2].val;
                const numFields = pc[3].val;
                const fields = framePtr[startLocal .. startLocal + numFields];
                framePtr[pc[4].val] = try cy.heap.allocObject(vm, sid, fields);
                pc += 5;
                continue;
            },
            .map => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMap:"::);
                }
                const startLocal = pc[1].val;
                const numEntries = pc[2].val;
                const dst = pc[3].val;
                const keyIdxes = @as([*]const align(1) u16, @ptrCast(pc + 4))[0..numEntries];
                const vals = framePtr[startLocal .. startLocal + numEntries];
                framePtr[dst] = try cy.heap.allocMap(vm, keyIdxes, vals);
                pc += 4 + numEntries * 2;
                continue;
            },
            .init => {
                if (GenLabels) {
                    _ = asm volatile ("LOpInit:"::);
                }
                const start = pc[1].val;
                const numLocals = pc[2].val;
                for (start..start+numLocals) |i| {
                    framePtr[i] = Value.None;
                }
                pc += 3;
                continue;
            },
            .copy => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCopy:"::);
                }
                framePtr[pc[2].val] = framePtr[pc[1].val];
                pc += 3;
                continue;
            },
            .copyRetainRelease => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCopyRetainRelease:"::);
                }
                const src = pc[1].val;
                const dst = pc[2].val;
                retain(vm, framePtr[src]);
                release(vm, framePtr[dst]);
                framePtr[dst] = framePtr[src];
                pc += 3;
                continue;
            },
            .copyReleaseDst => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCopyReleaseDst:"::);
                }
                const dst = pc[2].val;
                release(vm, framePtr[dst]);
                framePtr[dst] = framePtr[pc[1].val];
                pc += 3;
                continue;
            },
            .retain => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRetain:"::);
                }
                retain(vm, framePtr[pc[1].val]);
                pc += 2;
                continue;
            },
            .index => {
                if (GenLabels) {
                    _ = asm volatile ("LOpIndex:"::);
                }
                const recv = &framePtr[pc[1].val];
                const indexv = framePtr[pc[2].val];
                framePtr[pc[3].val] = try @call(.never_inline, VM.getIndex, .{vm, recv, indexv});
                pc += 4;
                continue;
            },
            .reverseIndex => {
                if (GenLabels) {
                    _ = asm volatile ("LOpReverseIndex:"::);
                }
                const recv = &framePtr[pc[1].val];
                const indexv = framePtr[pc[2].val];
                framePtr[pc[3].val] = try @call(.never_inline, VM.getReverseIndex, .{vm, recv, indexv});
                pc += 4;
                continue;
            },
            .jump => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJump:"::);
                }
                @setRuntimeSafety(false);
                pc += @as(usize, @intCast(@as(*const align(1) i16, @ptrCast(&pc[1])).*));
                continue;
            },
            .jumpCond => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJumpCond:"::);
                }
                const jump = @as(*const align(1) i16, @ptrCast(pc + 1)).*;
                const cond = framePtr[pc[3].val];
                const condVal = if (cond.isBool()) b: {
                    break :b cond.asBool();
                } else b: {
                    break :b cond.assumeNotBoolToBool();
                };
                if (condVal) {
                    @setRuntimeSafety(false);
                    pc += @as(usize, @intCast(jump));
                } else {
                    pc += 4;
                }
                continue;
            },
            .call => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCall:"::);
                }
                const startLocal = pc[1].val;
                const numArgs = pc[2].val;
                const numRet = pc[3].val;

                const callee = framePtr[startLocal + numArgs + 4];
                const retInfo = buildReturnInfo(numRet, true, cy.bytecode.CallInstLen);
                const res = try @call(.always_inline, call, .{vm, pc, framePtr, callee, startLocal, numArgs, retInfo});
                pc = res.pc;
                framePtr = res.sp;
                continue;
            },
            .callObjFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallObjFuncIC:"::);
                }
                const startLocal = pc[1].val;
                const recv = framePtr[startLocal + 4];
                const typeId = recv.getTypeId();

                const cachedStruct = @as(*align (1) u16, @ptrCast(pc + 14)).*;
                if (typeId == cachedStruct) {
                    const stackSize = pc[7].val;
                    if (@intFromPtr(framePtr + startLocal + stackSize) >= @intFromPtr(vm.stackEndPtr)) {
                        return error.StackOverflow;
                    }
                    const retFramePtr = Value{ .retFramePtr = framePtr };
                    framePtr += startLocal;
                    framePtr[1] = buildReturnInfo(pc[3].val, true, cy.bytecode.CallObjSymInstLen);
                    framePtr[2] = Value{ .retPcPtr = pc + cy.bytecode.CallObjSymInstLen };
                    framePtr[3] = retFramePtr;
                    pc = vm.ops.ptr + @as(*align(1) u32, @ptrCast(pc + 8)).*;
                    continue;
                }

                // Deoptimize.
                pc[0] = cy.Inst.initOpCode(.callObjSym);
                continue;
            },
            .callObjNativeFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallObjNativeFuncIC:"::);
                }
                const startLocal = pc[1].val;
                const numArgs = pc[2].val;
                const recv = framePtr[startLocal + 4];
                const typeId = recv.getTypeId();

                const cachedStruct = @as(*align (1) u16, @ptrCast(pc + 14)).*;
                if (typeId == cachedStruct) {
                    // const newFramePtr = framePtr + startLocal;
                    vm.framePtr = framePtr;
                    const func: cy.NativeObjFuncPtr = @ptrFromInt(@as(usize, @intCast(@as(*align (1) u48, @ptrCast(pc + 8)).*)));
                    const res = func(@ptrCast(vm), recv, @ptrCast(framePtr + startLocal + 5), numArgs);
                    if (res.isInterrupt()) {
                        return error.Panic;
                    }
                    const numRet = pc[3].val;
                    if (numRet == 1) {
                        framePtr[startLocal] = res;
                    } else {
                        switch (numRet) {
                            0 => {
                                // Nop.
                            },
                            1 => cy.panic("not possible"),
                            else => {
                                cy.panic("unsupported numret");
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
                pc[0] = cy.Inst.initOpCode(.callObjSym);
                continue;
            },
            .callFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallFuncIC:"::);
                }
                const startLocal = pc[1].val;
                const stackSize = pc[4].val;
                if (@intFromPtr(framePtr + startLocal + stackSize) >= @intFromPtr(vm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr };
                framePtr += startLocal;
                framePtr[1] = buildReturnInfo(pc[3].val, true, cy.bytecode.CallSymInstLen);
                framePtr[2] = Value{ .retPcPtr = pc + cy.bytecode.CallSymInstLen };
                framePtr[3] = retFramePtr;

                pc = @ptrFromInt(@as(usize, @intCast(@as(*align(1) u48, @ptrCast(pc + 6)).*)));
                continue;
            },
            .callNativeFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallNativeFuncIC:"::);
                }
                const startLocal = pc[1].val;
                const numArgs = pc[2].val;

                const newFramePtr = framePtr + startLocal;
                vm.pc = pc;
                vm.framePtr = newFramePtr;
                const func: cy.NativeFuncPtr = @ptrFromInt(@as(usize, @intCast(@as(*align (1) u48, @ptrCast(pc + 6)).*)));
                const res = func(@ptrCast(vm), @ptrCast(newFramePtr + 4), numArgs);
                if (res.isInterrupt()) {
                    return error.Panic;
                }
                const numRet = pc[3].val;
                if (numRet == 1) {
                    newFramePtr[0] = res;
                } else {
                    switch (numRet) {
                        0 => {
                            // Nop.
                        },
                        1 => cy.panic("not possible"),
                        else => cy.panic("unsupported"),
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
                const errDst = pc[1].val;
                const catchPcOffset = @as(*align (1) u16, @ptrCast(pc + 2)).*;
                if (vm.tryStack.len == vm.tryStack.buf.len) {
                    try @call(.never_inline, @TypeOf(vm.tryStack).growTotalCapacity, .{&vm.tryStack, vm.alloc, vm.tryStack.len + 1});
                }
                vm.tryStack.appendAssumeCapacity(.{
                    .fp = @ptrCast(framePtr),
                    .catchPc = getInstOffset(vm, pc) + catchPcOffset,
                    .catchErrDst = errDst,
                });
                pc += 4;
                continue;
            },
            .popTry => {
                vm.tryStack.len -= 1;
                pc += @as(*align (1) u16, @ptrCast(pc + 1)).*;
                continue;
            },
            .throw => {
                const err = framePtr[pc[1].val];
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
                    return @call(.never_inline, VM.panic, .{vm, "Not an error."});
                }
            },
            .setFieldReleaseIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetFieldReleaseIC:"::);
                }
                const recv = framePtr[pc[1].val];
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    if (obj.getTypeId() == @as(*align (1) u16, @ptrCast(pc + 4)).*) {
                        const lastValue = obj.object.getValuePtr(pc[6].val);
                        release(vm, lastValue.*);
                        lastValue.* = framePtr[pc[2].val];
                        pc += 7;
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.Inst.initOpCode(.setFieldRelease);
                try @call(.never_inline, VM.setFieldRelease, .{ vm, recv, pc[3].val, framePtr[pc[2].val] });
                pc += 7;
                continue;
            },
            .fieldRetainIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFieldRetainIC:"::);
                }
                const recv = framePtr[pc[1].val];
                const dst = pc[2].val;
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    if (obj.getTypeId() == @as(*align (1) u16, @ptrCast(pc + 5)).*) {
                        framePtr[dst] = obj.object.getValue(pc[7].val);
                        retain(vm, framePtr[dst]);
                        pc += 8;
                        continue;
                    }
                } else {
                    return vm.getFieldMissingSymbolError();
                }
                // Deoptimize.
                pc[0] = cy.Inst.initOpCode(.fieldRetain);
                continue;
            },
            .forRangeInit => {
                if (GenLabels) {
                    _ = asm volatile ("LOpForRangeInit:"::);
                }
                const start = framePtr[pc[1].val].toF64();
                const end = framePtr[pc[2].val].toF64();
                framePtr[pc[2].val] = Value.initF64(end);
                var step = framePtr[pc[3].val].toF64();
                if (step < 0) {
                    step = -step;
                }
                framePtr[pc[3].val] = Value.initF64(step);
                if (start == end) {
                    pc += @as(*const align(1) u16, @ptrCast(pc + 6)).* + 7;
                } else {
                    framePtr[pc[4].val] = Value.initF64(start);
                    framePtr[pc[5].val] = Value.initF64(start);
                    const offset = @as(*const align(1) u16, @ptrCast(pc + 6)).*;
                    pc[offset] = if (start < end)
                        cy.Inst.initOpCode(.forRange)
                    else
                        cy.Inst.initOpCode(.forRangeReverse);
                    pc += 8;
                }
                continue;
            },
            .forRange => {
                if (GenLabels) {
                    _ = asm volatile ("LOpForRange:"::);
                }
                const counter = framePtr[pc[1].val].asF64() + framePtr[pc[2].val].asF64();
                if (counter < framePtr[pc[3].val].asF64()) {
                    framePtr[pc[1].val] = Value.initF64(counter);
                    framePtr[pc[4].val] = Value.initF64(counter);
                    pc -= @as(*const align(1) u16, @ptrCast(pc + 5)).*;
                } else {
                    pc += 7;
                }
                continue;
            },
            .forRangeReverse => {
                if (GenLabels) {
                    _ = asm volatile ("LOpForRangeReverse:"::);
                }
                const counter = framePtr[pc[1].val].asF64() - framePtr[pc[2].val].asF64();
                if (counter > framePtr[pc[3].val].asF64()) {
                    framePtr[pc[1].val] = Value.initF64(counter);
                    framePtr[pc[4].val] = Value.initF64(counter);
                    pc -= @as(*const align(1) u16, @ptrCast(pc + 5)).*;
                } else {
                    pc += 7;
                }
                continue;
            },
            .jumpNotNone => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJumpNotNone:"::);
                }
                const offset = @as(*const align(1) i16, @ptrCast(pc + 1)).*;
                if (!framePtr[pc[3].val].isNone()) {
                    @setRuntimeSafety(false);
                    pc += @as(usize, @intCast(offset));
                } else {
                    pc += 4;
                }
                continue;
            },
            .setField => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetField:"::);
                }
                const fieldId = pc[1].val;
                const left = pc[2].val;
                const right = pc[3].val;

                const recv = framePtr[left];
                const val = framePtr[right];
                try vm.setField(recv, fieldId, val);
                // try @call(.never_inline, vm.setField, .{recv, fieldId, val});
                pc += 4;
                continue;
            },
            .field => {
                if (GenLabels) {
                    _ = asm volatile ("LOpField:"::);
                }
                const left = pc[1].val;
                const dst = pc[2].val;
                const symId = @as(*align (1) u16, @ptrCast(pc + 3)).*;
                const recv = framePtr[left];
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    // const offset = @call(.never_inline, vm.getFieldOffset, .{obj, symId });
                    const offset = vm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        framePtr[dst] = obj.object.getValue(offset);
                        // Inline cache.
                        pc[0] = cy.Inst.initOpCode(.fieldIC);
                        @as(*align (1) u16, @ptrCast(pc + 5)).* = @intCast(obj.getTypeId());
                        pc[7] = cy.Inst{ .val = offset };
                    } else {
                        const sym = vm.fieldSyms.buf[symId];
                        framePtr[dst] = @call(.never_inline, VM.getFieldFallback, .{vm, obj, sym.nameId});
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
                const recv = framePtr[pc[1].val];
                const dst = pc[2].val;
                const symId = @as(*align (1) u16, @ptrCast(pc + 3)).*;
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    // const offset = @call(.never_inline, vm.getFieldOffset, .{obj, symId });
                    const offset = vm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        framePtr[dst] = obj.object.getValue(offset);
                        // Inline cache.
                        pc[0] = cy.Inst.initOpCode(.fieldRetainIC);
                        @as(*align (1) u16, @ptrCast(pc + 5)).* = @intCast(obj.getTypeId());
                        pc[7] = cy.Inst { .val = offset };
                    } else {
                        const sym = vm.fieldSyms.buf[symId];
                        framePtr[dst] = @call(.never_inline, VM.getFieldFallback, .{vm, obj, sym.nameId});
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
                const recv = framePtr[pc[1].val];
                const val = framePtr[pc[2].val];
                const symId = pc[3].val;
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    const offset = vm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        const lastValue = obj.object.getValuePtr(offset);
                        release(vm, lastValue.*);
                        lastValue.* = val;

                        // Inline cache.
                        pc[0] = cy.Inst.initOpCode(.setFieldReleaseIC);
                        @as(*align (1) u16, @ptrCast(pc + 4)).* = @intCast(obj.getTypeId());
                        pc[6] = cy.Inst { .val = offset };
                        pc += 7;
                        continue;
                    } else {
                        return vm.getFieldMissingSymbolError();
                    }
                } else {
                    return vm.setFieldNotObjectError();
                }
            },
            .setCheckFieldRelease => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetFieldRelease:"::);
                }
                const recv = framePtr[pc[1].val];
                const val = framePtr[pc[2].val];
                const symId = pc[3].val;
                if (recv.isPointer()) {
                    const obj = recv.asHeapObject();
                    const offset = vm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        const fieldSemaTypeId = vm.fieldSyms.buf[symId].mruFieldTypeSymId;
                        const rightTypeId = val.getTypeId();
                        const rightSemaTypeId = vm.types.buf[rightTypeId].rTypeSymId;
                        if (!types.isTypeSymCompat(&vm.compiler, rightSemaTypeId, fieldSemaTypeId)) {
                            return panicIncompatibleFieldType(vm, fieldSemaTypeId, val);
                        }

                        const lastValue = obj.object.getValuePtr(offset);
                        release(vm, lastValue.*);
                        lastValue.* = val;

                        // TODO: Inline cache.
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
                const funcPc = getInstOffset(vm, pc) - pc[1].val;
                const numParams = pc[2].val;
                const stackSize = pc[3].val;
                const funcSigId = @as(*const align(1) u16, @ptrCast(pc + 4)).*;
                const dst = pc[6].val;
                pc += 7;
                framePtr[dst] = try @call(.never_inline, cy.heap.allocLambda, .{vm, funcPc, numParams, stackSize, funcSigId });
                continue;
            },
            .closure => {
                if (GenLabels) {
                    _ = asm volatile ("LOpClosure:"::);
                }
                const funcPc = getInstOffset(vm, pc) - pc[1].val;
                const numParams = pc[2].val;
                const numCaptured = pc[3].val;
                const stackSize = pc[4].val;
                const funcSigId = @as(*const align(1) u16, @ptrCast(pc + 5)).*;
                const local = pc[7].val;
                const dst = pc[8].val;
                const capturedVals = pc[9..9+numCaptured];
                pc += 9 + numCaptured;

                framePtr[dst] = try @call(.never_inline, cy.heap.allocClosure, .{vm, framePtr, funcPc, numParams, stackSize, funcSigId, capturedVals, local});
                continue;
            },
            .staticVar => {
                if (GenLabels) {
                    _ = asm volatile ("LOpStaticVar:"::);
                }
                const symId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
                const sym = vm.varSyms.buf[symId];
                retain(vm, sym.value);
                framePtr[pc[3].val] = sym.value;
                pc += 4;
                continue;
            },
            .setStaticVar => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetStaticVar:"::);
                }
                const symId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
                const prev = vm.varSyms.buf[symId].value;
                vm.varSyms.buf[symId].value = framePtr[pc[3].val];
                release(vm, prev);
                pc += 4;
                continue;
            },
            .staticFunc => {
                if (GenLabels) {
                    _ = asm volatile ("LOpStaticFunc:"::);
                }
                const symId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
                framePtr[pc[3].val] = try cy.heap.allocFuncFromSym(vm, symId);
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
                const val = framePtr[pc[1].val];
                if (val.isPointer()) {
                    const fiber = val.asPointer(*vmc.Fiber);
                    if (fiber.typeId == rt.FiberT) {
                        if (fiber != vm.curFiber) {
                            // Only resume fiber if it's not done.
                            if (fiber.pcOffset != cy.NullId) {
                                const res = cy.fiber.pushFiber(vm, getInstOffset(vm, pc + 3), framePtr, fiber, pc[2].val);
                                pc = res.pc;
                                framePtr = res.sp;
                                continue;
                            }
                        }
                    }
                    cy.arc.releaseObject(vm, cy.ptrAlignCast(*HeapObject, fiber));
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
                const startArgsLocal = pc[1].val;
                const numArgs = pc[2].val;
                const jump = pc[3].val;
                const initialStackSize = pc[4].val;
                const dst = pc[5].val;

                const val = framePtr[startArgsLocal..startArgsLocal + numArgs];
                const fiber = try @call(.never_inline, cy.fiber.allocFiber, .{vm, getInstOffset(vm, pc + 6), val, initialStackSize});
                framePtr[dst] = fiber;
                pc += jump;
                continue;
            },
            .mul => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMul:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    framePtr[pc[3].val] = Value.initF64(left.asF64() * right.asF64());
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .div => {
                if (GenLabels) {
                    _ = asm volatile ("LOpDiv:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    framePtr[pc[3].val] = Value.initF64(left.asF64() / right.asF64());
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .mod => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMod:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    framePtr[pc[3].val] = Value.initF64(std.math.mod(f64, left.asF64(), right.asF64()) catch std.math.nan_f64);
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .pow => {
                if (GenLabels) {
                    _ = asm volatile ("LOpPow:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    framePtr[pc[3].val] = Value.initF64(std.math.pow(f64, left.asF64(), right.asF64()));
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .box => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBox:"::);
                }
                const value = framePtr[pc[1].val];
                framePtr[pc[2].val] = try cy.heap.allocBox(vm, value);
                pc += 3;
                continue;
            },
            .setBoxValue => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetBoxValue:"::);
                }
                const box = framePtr[pc[1].val];
                const rval = framePtr[pc[2].val];
                if (builtin.mode == .Debug) {
                    std.debug.assert(box.isPointer());
                }
                const obj = box.asHeapObject();
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.getTypeId() == rt.BoxT);
                }
                obj.box.val = rval;
                pc += 3;
                continue;
            },
            .setBoxValueRelease => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBoxValueRelease:"::);
                }
                const box = framePtr[pc[1].val];
                const rval = framePtr[pc[2].val];
                if (builtin.mode == .Debug) {
                    std.debug.assert(box.isPointer());
                }
                const obj = box.asHeapObject();
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.getTypeId() == rt.BoxT);
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
                const box = framePtr[pc[1].val];
                if (builtin.mode == .Debug) {
                    if (!box.isBox()) {
                        cy.panic("Expected box value.");
                    }
                }
                framePtr[pc[2].val] = box.asHeapObject().box.val;
                pc += 3;
                continue;
            },
            .boxValueRetain => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBoxValueRetain:"::);
                }
                const box = framePtr[pc[1].val];
                if (builtin.mode == .Debug) {
                    if (!box.isBox()) {
                        cy.panic("Expected box value.");
                    }
                }
                const val = box.asHeapObject().box.val;
                framePtr[pc[2].val] = val;
                retain(vm, val);
                pc += 3;
                continue;
            },
            .captured => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCaptured:"::);
                }
                const closure = framePtr[pc[1].val];
                if (builtin.mode == .Debug) {
                    if (!closure.isClosure()) {
                        cy.panic("Expected closure value.");
                    }
                }
                framePtr[pc[3].val] = closure.asHeapObject().closure.getCapturedValuesPtr()[pc[2].val];
                pc += 4;
                continue;
            },
            .tag => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTag:"::);
                }
                const tagId = pc[1].val;
                const val = pc[2].val;
                framePtr[pc[3].val] = Value.initEnum(tagId, val);
                pc += 4;
                continue;
            },
            .tagLiteral => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTagLiteral:"::);
                }
                const symId = pc[1].val;
                framePtr[pc[2].val] = Value.initSymbol(symId);
                pc += 3;
                continue;
            },
            .cast => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCast:"::);
                }
                const val = framePtr[pc[1].val];
                const expTypeId = @as(*const align(1) u16, @ptrCast(pc + 2)).*;
                if (val.getTypeId() == expTypeId) {
                    pc += 4;
                    continue;
                } else {
                    return @call(.never_inline, panicCastError, .{ vm, val, expTypeId });
                }
            },
            .castAbstract => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCastAbstract:"::);
                }
                const val = framePtr[pc[1].val];
                const expTypeSymId = @as(*const align(1) u16, @ptrCast(pc + 2)).*;
                if (expTypeSymId == bt.Any) {
                    pc += 4;
                    continue;
                } else if (expTypeSymId == bt.String) {
                    if (val.isString()) {
                        pc += 4;
                        continue;
                    }
                } else if (expTypeSymId == bt.Rawstring) {
                    if (val.isRawString()) {
                        pc += 4;
                        continue;
                    }
                }
                return @call(.never_inline, panicCastAbstractError, .{ vm, val, expTypeSymId });
            },
            .bitwiseAnd => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseAnd:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    const f: f64 = @floatFromInt(left.asF64toI32() & right.asF64toI32());
                    framePtr[pc[3].val] = Value.initF64(f);
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .bitwiseOr => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseOr:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    const f: f64 = @floatFromInt(left.asF64toI32() | right.asF64toI32());
                    framePtr[pc[3].val] = Value.initF64(f);
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .bitwiseXor => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseXor:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    const f: f64 = @floatFromInt(left.asF64toI32() ^ right.asF64toI32());
                    framePtr[pc[3].val] = Value.initF64(f);
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .bitwiseNot => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseNot:"::);
                }
                const dst = &framePtr[pc[1].val];
                const val = dst.*;
                if (val.isFloat()) {
                    const f: f64 = @floatFromInt(~val.asF64toI32());
                    dst.* = Value.initF64(f);
                    pc += 2;
                    continue;
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
            },
            .bitwiseLeftShift => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseLeftShift:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    const mod: u64 = @bitCast(@mod(right.asF64toI64(), 32));
                    const amt: u5 = @intCast(mod);
                    const res: f64 = @floatFromInt(left.asF64toI32() << amt);
                    framePtr[pc[3].val] = Value.initF64(res);
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .bitwiseRightShift => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseRightShift:"::);
                }
                const left = framePtr[pc[1].val];
                const right = framePtr[pc[2].val];
                if (Value.bothFloats(left, right)) {
                    const mod: u64 = @bitCast(@mod(right.asF64toI64(), 32));
                    const amt: u5 = @intCast(mod);
                    const res: f64 = @floatFromInt(left.asF64toI32() >> amt);
                    framePtr[pc[3].val] = Value.initF64(res);
                } else {
                    return @call(.never_inline, panicExpectedFloat, .{vm});
                }
                pc += 4;
                continue;
            },
            .callObjSym => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallObjSym:"::);
                }
                const startLocal = pc[1].val;
                const numArgs = pc[2].val;

                const numRet = pc[3].val;
                const mgId = pc[4].val;
                const anySelfFuncSigId = @as(*const align(1) u16, @ptrCast(pc + 5)).*;

                const recv = framePtr[startLocal + 4];
                const typeId = recv.getTypeId();

                if (try vm.getCachedMethodGroupForType(typeId, mgId)) |mg| {
                    if (try @call(.never_inline, callMethodGroup, .{
                        vm, pc, framePtr, mgId, mg, typeId, startLocal, numArgs, numRet, anySelfFuncSigId,
                    })) |res| {
                        pc = res.pc;
                        framePtr = res.sp;
                        continue;
                    }
                }
                const res = try @call(.never_inline, callObjSymFallback, .{
                    vm, pc, framePtr, recv, typeId, mgId, startLocal, numArgs, numRet
                });
                pc = res.pc;
                framePtr = res.sp;
                continue;
            },
            .callTypeCheck => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallTypeCheck:"::);
                }
                const argStartReg = pc[1].val;
                const numArgs = pc[2].val;
                const funcSigId = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
                const funcSig = vm.compiler.sema.getFuncSig(funcSigId);
                const args = framePtr[argStartReg .. argStartReg + funcSig.numParams()];

                // TODO: numArgs and this check can be removed if overloaded symbols are grouped by numParams.
                if (numArgs != funcSig.numParams()) {
                    const funcId = @as(*const align(1) u16, @ptrCast(pc + 5 + 4)).*;
                    return panicIncompatibleFuncSig(vm, funcId, args, funcSigId);
                }

                // Perform type check on args.
                for (funcSig.params(), 0..) |cstrTypeId, i| {
                    const argTypeId = args[i].getTypeId();
                    const argSemaTypeId = vm.types.buf[argTypeId].rTypeSymId;
                    if (!types.isTypeSymCompat(&vm.compiler, argSemaTypeId, cstrTypeId)) {
                        // Assumes next inst is callSym/callNativeFuncIC/callFuncIC.
                        const funcId = @as(*const align(1) u16, @ptrCast(pc + 5 + 4)).*;
                        return panicIncompatibleFuncSig(vm, funcId, args, funcSigId);
                    }
                }
                pc += 5;
                continue;
            },
            .callSym => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallSym:"::);
                }
                const startLocal = pc[1].val;
                const numArgs = pc[2].val;
                const numRet: u2 = @intCast(pc[3].val);
                const symId = @as(*const align(1) u16, @ptrCast(pc + 4)).*;
                const res = try @call(.never_inline, VM.callSym, .{vm, pc, framePtr, symId, startLocal, numArgs, numRet});
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
                const symType = pc[1].val;
                const symId = @as(*const align(1) u32, @ptrCast(pc + 2)).*;
                framePtr[pc[6].val] = try cy.heap.allocMetaType(vm, symType, symId);
                pc += 7;
                continue;
            },
            .setStaticFunc => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetStaticFunc:"::);
                }
                const symId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
                try @call(.never_inline, setStaticFunc, .{vm, symId, framePtr[pc[3].val]});
                pc += 4;
                continue;
            },
            .end => {
                if (GenLabels) {
                    _ = asm volatile ("LOpEnd:"::);
                }
                vm.endLocal = pc[1].val;
                pc += 2;
                vm.curFiber.pcOffset = @intCast(getInstOffset(vm, pc));
                return error.End;
            },
        }
    }
}

fn popStackFrameLocal0(pc: *[*]const cy.Inst, framePtr: *[*]Value) linksection(cy.HotSection) bool {
    const retFlag = framePtr.*[1].retInfoRetFlag();
    const reqNumArgs = framePtr.*[1].retInfoNumRet();
    if (reqNumArgs == 0) {
        pc.* = framePtr.*[2].retPcPtr;
        framePtr.* = framePtr.*[3].retFramePtr;
        return retFlag == 0;
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
        return retFlag == 0;
    }
}

fn popStackFrameLocal1(vm: *VM, pc: *[*]const cy.Inst, framePtr: *[*]Value) linksection(cy.HotSection) bool {
    const retFlag = framePtr.*[1].retInfoRetFlag();
    const reqNumArgs = framePtr.*[1].retInfoNumRet();
    if (reqNumArgs == 1) {
        pc.* = framePtr.*[2].retPcPtr;
        framePtr.* = framePtr.*[3].retFramePtr;
        return retFlag == 0;
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
        return retFlag == 0;
    }
}

fn dumpEvalOp(vm: *const VM, pc: [*]const cy.Inst) !void {
    const offset = getInstOffset(vm, pc);
    const len = cy.getInstLenAt(pc);
    try cy.debug.dumpInst(vm, offset, pc[0].opcode(), pc, len);
    switch (pc[0].opcode()) {
        .constOp => {
            const idx = @as(*const align (1) u16, @ptrCast(pc + 1)).*;
            const dst = pc[3].val;
            _ = dst;
            const val = Value{ .val = vm.consts[idx].val };
            fmt.printStderr("rt: [constVal={}]\n", &.{v(vm.valueToTempString(val))});
        },
        else => {},
    }
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
    typeSymId: sema.SymbolId,
};

/// See `reserveFuncParams` for stack layout.
/// numArgs does not include the callee.
pub fn call(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !cy.fiber.PcSp {
    if (callee.isPointer()) {
        const obj = callee.asHeapObject();
        switch (obj.getTypeId()) {
            rt.ClosureT => {
                if (numArgs != obj.closure.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                if (@intFromPtr(framePtr + startLocal + obj.closure.stackSize) >= @intFromPtr(vm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr };
                framePtr[startLocal + 1] = retInfo;
                framePtr[startLocal + 2] = Value{ .retPcPtr = pc + cy.bytecode.CallInstLen };
                framePtr[startLocal + 3] = retFramePtr;

                // Copy closure to local.
                framePtr[startLocal + obj.closure.local] = callee;
                return cy.fiber.PcSp{
                    .pc = cy.fiber.toVmPc(vm, obj.closure.funcPc),
                    .sp = framePtr + startLocal,
                };
            },
            rt.LambdaT => {
                if (numArgs != obj.lambda.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                if (@intFromPtr(framePtr + startLocal + obj.lambda.stackSize) >= @intFromPtr(vm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr };
                framePtr[startLocal + 1] = retInfo;
                framePtr[startLocal + 2] = Value{ .retPcPtr = pc + cy.bytecode.CallInstLen };
                framePtr[startLocal + 3] = retFramePtr;
                return cy.fiber.PcSp{
                    .pc = cy.fiber.toVmPc(vm, obj.lambda.funcPc),
                    .sp = framePtr + startLocal,
                };
            },
            rt.NativeFuncT => {
                if (numArgs != obj.nativeFunc1.numParams) {
                    log.debug("params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                vm.pc = pc;
                const newFramePtr = framePtr + startLocal;
                vm.framePtr = newFramePtr;
                const res = obj.nativeFunc1.func.?(@ptrCast(vm), @ptrCast(newFramePtr + 4), numArgs);
                newFramePtr[0] = @bitCast(res);
                return cy.fiber.PcSp{
                    .pc = pc + cy.bytecode.CallInstLen,
                    .sp = framePtr,
                };
            },
            else => {
                // TODO: Throw error.
                cy.panic("not a function");
            },
        }
    } else {
        // TODO: Throw error.
        cy.panic("not a function");
    }
}

pub fn callNoInline(vm: *VM, pc: *[*]cy.Inst, framePtr: *[*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !void {
    if (callee.isPointer()) {
        const obj = callee.asHeapObject();
        switch (obj.getTypeId()) {
            rt.ClosureT => {
                if (numArgs != obj.closure.numParams) {
                    cy.panic("params/args mismatch");
                }

                if (@intFromPtr(framePtr.* + startLocal + obj.closure.stackSize) >= @intFromPtr(vm.stack.ptr) + (vm.stack.len << 3)) {
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
                    cy.fatal();
                }

                if (@intFromPtr(framePtr.* + startLocal + obj.lambda.stackSize) >= @intFromPtr(vm.stack.ptr) + (vm.stack.len << 3)) {
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
                const res = obj.nativeFunc1.func.?(@ptrCast(vm), @ptrCast(newFramePtr + 4), numArgs);
                newFramePtr[0] = @bitCast(res);
                cy.arc.releaseObject(vm, obj);
                pc.* += 14;
            },
            else => {},
        }
    } else {
        cy.panic("not a function");
    }
}

fn panicCastAbstractError(vm: *cy.VM, val: Value, expTypeSymId: cy.sema.SymbolId) !void {
    const sym = vm.compiler.sema.getSymbol(expTypeSymId);
    const name = cy.sema.getName(&vm.compiler, sym.key.resolvedSymKey.nameId);
    return vm.panicFmt("Can not cast `{}` to `{}`.", &.{
         v(vm.types.buf[val.getTypeId()].name), v(name), 
    });
}

fn panicCastError(vm: *cy.VM, val: Value, expTypeId: rt.TypeId) !void {
    return vm.panicFmt("Can not cast `{}` to `{}`.", &.{
         v(vm.types.buf[val.getTypeId()].name), v(vm.types.buf[expTypeId].name), 
    });
}

fn allocFuncCallSemaTypeIds(vm: *cy.VM, vals: []const Value) ![]const cy.types.TypeId {
    const semaTypeIds = try vm.alloc.alloc(cy.types.TypeId, vals.len);
    for (vals, 0..) |val, i| {
        const typeId = val.getTypeId();
        semaTypeIds[i] = vm.types.buf[typeId].semaTypeId;
    }
    return semaTypeIds;
}

fn allocMethodCallSemaTypeIds(vm: *cy.VM, vals: []const Value) ![]const cy.types.TypeId {
    const semaTypeIds = try vm.alloc.alloc(cy.types.TypeId, vals.len + 1);
    semaTypeIds[0] = bt.Any;
    for (vals, 1..) |val, i| {
        const typeId = val.getTypeId();
        semaTypeIds[i] = vm.types.buf[typeId].semaTypeId;
    }
    return semaTypeIds;
}

fn panicIncompatibleFieldType(vm: *cy.VM, fieldSemaTypeId: types.TypeId, rightv: Value) error{Panic, OutOfMemory} {
    defer release(vm, rightv);

    const fieldTypeName = sema.getSymName(&vm.compiler, fieldSemaTypeId);
    const rightTypeId = rightv.getTypeId();
    const rightSemaTypeId = vm.types.buf[rightTypeId].rTypeSymId;
    const rightTypeName = sema.getSymName(&vm.compiler, rightSemaTypeId);
    return vm.panicFmt(
        \\Assigning to `{}` member with incompatible type `{}`.
        , &.{
            v(fieldTypeName), v(rightTypeName),
        },
    );
}

fn panicIncompatibleFuncSig(vm: *cy.VM, funcId: rt.FuncId, args: []const Value, targetFuncSigId: sema.FuncSigId) error{Panic} {
    for (args) |arg| {
        release(vm, arg);
    }

    const semaTypeIds = allocFuncCallSemaTypeIds(vm, args) catch {
        vm.curFiber.panicType = vmc.PANIC_INFLIGHT_OOM;
        return error.Panic;
    };
    defer vm.alloc.free(semaTypeIds);

    const nameId = vm.funcSymDetails.buf[funcId].nameId;
    const name = sema.getName(vm.compiler, nameId);
    const sigStr = sema.allocFuncSigTypesStr(vm.compiler, semaTypeIds, bt.Any) catch {
        vm.curFiber.panicType = vmc.PANIC_INFLIGHT_OOM;
        return error.Panic;
    };
    const existingSigStr = sema.allocFuncSigStr(vm.compiler, targetFuncSigId) catch {
        vm.curFiber.panicType = vmc.PANIC_INFLIGHT_OOM;
        return error.Panic;
    };
    defer {
        vm.alloc.free(sigStr);
        vm.alloc.free(existingSigStr);
    }
    return vm.panicFmt(
        \\Can not find compatible function for `{}{}`.
        \\Only `func {}{}` exists.
        , &.{
            v(name), v(sigStr),
            v(name), v(existingSigStr),
        },
    );
}

pub fn getMruFuncSigId(vm: *cy.VM, mgExt: rt.MethodGroupExt) sema.FuncSigId {
    if (mgExt.mruTypeMethodGroupId == cy.NullId) {
        return mgExt.initialFuncSigId;
    } else {
        return vm.methodExts.buf[mgExt.mruMethodId].funcSigId;
    }
}

/// TODO: Once methods are recorded in the object/builtin type's module, this should look there instead of the rt table.
fn panicIncompatibleMethodSig(
    vm: *cy.VM, mgId: vmc.MethodGroupId, recv: Value, args: []const Value
) error{Panic, OutOfMemory} {
    const typeId = recv.getTypeId();

    const semaTypeIds = try allocMethodCallSemaTypeIds(vm, args);
    defer vm.alloc.free(semaTypeIds);

    const mgExt = vm.methodGroupExts.buf[mgId];
    const name = mgExt.getName();
    const methodSym = vm.methodGroups.buf[mgId];
    if (methodSym.mruTypeId == typeId) {
        var singleMethod = false;
        if (mgExt.mruTypeMethodGroupId == cy.NullId) {
            singleMethod = true;
        } else {
            const tmg = vm.typeMethodGroups.buf[mgExt.mruTypeMethodGroupId];
            if (tmg.head == tmg.tail) {
                singleMethod = true;
            }
        }
        const sigStr = try sema.allocFuncSigTypesStr(vm.compiler, semaTypeIds, bt.Any);
        defer vm.alloc.free(sigStr);
        if (singleMethod) {
            const funcSigId = getMruFuncSigId(vm, mgExt);
            const existingSigStr = try sema.allocFuncSigStr(vm.compiler, funcSigId);
            defer vm.alloc.free(existingSigStr);

            const vmType = vm.types.buf[typeId];
            return vm.panicFmt(
                \\Can not find compatible function for `{}{}` in `{}`.
                \\Only `func {}{}` exists for the symbol `{}`.
                , &.{
                    v(name), v(sigStr), v(vmType.namePtr[0..vmType.nameLen]),
                    v(name), v(existingSigStr), v(name),
                },
            );
        } else {
            const vmType = vm.types.buf[typeId];
            return vm.panicFmt(
                \\Can not find compatible function for `{}{}` in `{}`.
                \\Multiple signatures exist for `func {}`.
                , &.{
                    v(name), v(sigStr), v(vmType.namePtr[0..vmType.nameLen]),
                    v(name),
                },
            );
        }
    } else {
    //     const key = rt.MethodTableKey.initMethodTableKey(typeId, mgId);
    //     if (vm.methodTable.get(key)) |_| {
    //         const sigStr = try sema.allocFuncSigTypesStr(&vm.compiler, semaTypeIds, bt.Any);
    //         defer {
    //             vm.alloc.free(sigStr);
    //         }
    //         return vm.panicFmt(
    //             \\Can not find compatible function for `{}{}` in `{}`.
    //             , &.{
    //                 v(name), v(sigStr), v(vm.types.buf[typeId].name),
    //             },
    //         );
    //     } else {
            const sigStr = try sema.allocFuncSigTypesStr(vm.compiler, semaTypeIds, bt.Any);
            defer vm.alloc.free(sigStr);
            const vmType = vm.types.buf[typeId];
            return vm.panicFmt("`func {}{}` can not be found in `{}`.", &.{
                v(name), v(sigStr), v(vmType.namePtr[0..vmType.nameLen]),
            });
    //     }
    }
}

fn getObjectFunctionFallback(
    vm: *VM, pc: [*]cy.Inst, recv: Value, typeId: u32, rtSymId: SymbolId, vals: []const Value,
) !Value {
    @setCold(true);
    _ = typeId;
    _ = pc;
    // Map fallback is no longer supported since cleanup of recv is not auto generated by the compiler.
    // In the future, this may invoke the exact method signature or call a custom overloaded function.
    // if (typeId == MapS) {
    //     const name = vm.methodGroupExtras.buf[symId];
    //     const heapMap = cy.ptrAlignCast(*const MapInner, &obj.map.inner);
    //     if (heapMap.getByString(vm, name)) |val| {
    //         return val;
    //     }
    // }

    // Once methods are added to resolved func syms,
    // this error reporting can be more descriptive depending on these scenarios:
    // - a method with the name is missing.
    // - a method exists with the name but the call signature doesn't match up
    // - multiple methods exist with the name but the call signature doesn't match up
    // const relPc = getInstOffset(vm, pc);
    // if (debug.getDebugSym(vm, relPc)) |sym| {
        // const chunk = vm.compiler.chunks.items[sym.file];
        // const node = chunk.nodes[sym.loc];
        // if (node.node_t == .callExpr) {
            return panicIncompatibleMethodSig(vm, rtSymId, recv, vals);
        // } else {
        //     release(vm, recv);
        //     // Debug node is from:
        //     // `for [iterable]:`
        //     const name = vm.methodGroupExts.buf[rtSymId].getName();
        //     return vm.panicFmt("`{}` is either missing in `{}` or the call signature: {}(self, 0 args) is unsupported.", &.{
        //          v(name), v(vm.types.buf[typeId].name), v(name),
        //     });
        // }
    // } else {
    //     return vm.panicFmt("Missing debug sym at {}", &.{v(getInstOffset(vm, pc))});
    // }
}

/// Use new pc local to avoid deoptimization.
fn callObjSymFallback(
    vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, recv: Value, typeId: u32, mgId: vmc.MethodGroupId, 
    startLocal: u8, numArgs: u8, reqNumRetVals: u8
) linksection(cy.Section) !cy.fiber.PcSp {
    @setCold(true);
    // const func = try @call(.never_inline, getObjectFunctionFallback, .{obj, symId});
    const vals = framePtr[startLocal+5..startLocal+5+numArgs-1];
    const func = try getObjectFunctionFallback(vm, pc, recv, typeId, mgId, vals);
    _ = reqNumRetVals;
    _ = func;
    return vm.panic("Missing method.");
}

pub inline fn buildReturnInfo(numRetVals: u8, comptime cont: bool, comptime callInstOffset: u8) Value {
    return .{
        .val = numRetVals | (@as(u32, @intFromBool(!cont)) << 8) | (@as(u32, callInstOffset) << 16),
    };
    // return .{
    //     .retInfo = .{
    //         .numRetVals = numRetVals,
    //         // .retFlag = if (cont) 0 else 1,
    //         .retFlag = @intFromBool(!cont),
    //         .callInstOffset = callInstOffset,
    //     },
    // };
}

pub inline fn getInstOffset(vm: *const VM, to: [*]const cy.Inst) u32 {
    return @intCast(cy.fiber.getInstOffset(vm.ops.ptr, to));
}

pub inline fn getStackOffset(vm: *const VM, to: [*]const Value) u32 {
    return @intCast(cy.fiber.getStackOffset(vm.stack.ptr, to));
}

/// Like Value.dump but shows heap values.
pub fn dumpValue(vm: *const VM, val: Value) void {
    if (val.isFloat()) {
        fmt.printStdout("Float {}\n", &.{ v(val.asF64()) });
    } else {
        if (val.isPointer()) {
            const obj = val.asHeapObject();
            switch (obj.getTypeId()) {
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
                    const vmType = vm.types.buf[obj.getTypeId()];
                    fmt.printStdout("HeapObject {} {} {}\n", &.{v(obj), v(obj.getTypeId()), v(vmType.namePtr[0..vmType.nameLen])});
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
        // One use for splitting up parentSymId/nameId is to look for $call() magic function.
        parentSymId: sema.SymbolId,
        nameId: sema.NameSymId,
        funcSigId: sema.FuncSigId,
    },
};

pub const KeyU96Context = struct {
    pub fn hash(_: @This(), key: KeyU96) u64 {
        var hasher = std.hash.Wyhash.init(0);
        @call(.always_inline, std.hash.Wyhash.update, .{&hasher, std.mem.asBytes(&key.val.a)});
        @call(.always_inline, std.hash.Wyhash.update, .{&hasher, std.mem.asBytes(&key.val.b)});
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
        self.idx.* += @intCast(data.len);
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
        @memset(self.buf[self.idx.*..self.idx.*+n], byte);
        self.idx.* += @intCast(n);
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

fn opMatch(vm: *const VM, pc: [*]const cy.Inst, framePtr: [*]const Value) u16 {
    const expr = framePtr[pc[1].val];
    const numCases = pc[2].val;
    var i: u32 = 0;
    while (i < numCases) : (i += 1) {
        const right = framePtr[pc[3 + i * 3].val];
        // Can immediately match numbers, objects, primitives.
        const cond = if (expr.val == right.val) true else 
            @call(.never_inline, evalCompareBool, .{vm, expr, right});
        if (cond) {
            // Jump.
            return @as(*const align (1) u16, @ptrCast(pc + 4 + i * 3)).*;
        }
    }
    // else case
    return @as(*const align (1) u16, @ptrCast(pc + 4 + i * 3 - 1)).*;
}

fn releaseFuncSymDep(vm: *VM, symId: SymbolId) void {
    const entry = vm.funcSyms.buf[symId];
    switch (@as(rt.FuncSymbolType, @enumFromInt(entry.entryT))) {
        .closure => {
            cy.arc.releaseObject(vm, @ptrCast(entry.inner.closure));
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
    const dstSig = cy.sema.allocFuncSigStr(vm.compiler, dstFuncSigId) catch fatal();
    const srcSig = cy.sema.allocFuncSigStr(vm.compiler, srcFuncSigId) catch fatal();
    defer {
        vm.alloc.free(dstSig);
        vm.alloc.free(srcSig);
    }
    return vm.panicFmt("Assigning to static function `func {}` with a different function signature `func {}`.",
        &.{v(dstSig), v(srcSig)}
    );
}

fn isAssignFuncSigCompat(vm: *VM, srcFuncSigId: sema.FuncSigId, dstFuncSigId: sema.FuncSigId) bool {
    if (srcFuncSigId == dstFuncSigId) {
        return true;
    }
    const srcFuncSig = vm.compiler.sema.getFuncSig(srcFuncSigId);
    const dstFuncSig = vm.compiler.sema.getFuncSig(dstFuncSigId);
    if (srcFuncSig.numParams() != dstFuncSig.numParams()) {
        return false;
    }
    const dstParams = dstFuncSig.params();
    for (srcFuncSig.params(), 0..) |srcParam, i| {
        const dstParam = dstParams[i];
        if (srcParam == dstParam) {
            continue;
        }
        if (srcParam == bt.Any and dstParam == bt.Dynamic) {
            continue;
        }
        if (srcParam == bt.Dynamic and dstParam == bt.Any) {
            continue;
        }
        return false;
    }
    return true;
}

fn setStaticFunc(vm: *VM, symId: SymbolId, val: Value) linksection(cy.Section) !void {
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        switch (obj.getTypeId()) {
            rt.NativeFuncT => {
                const dstRFuncSigId = getFuncSigIdOfSym(vm, symId);
                if (!isAssignFuncSigCompat(vm, obj.nativeFunc1.funcSigId, dstRFuncSigId)) {
                    return @call(.never_inline, reportAssignFuncSigMismatch, .{vm, obj.nativeFunc1.funcSigId, dstRFuncSigId});
                }
                releaseFuncSymDep(vm, symId);

                const rFuncSig = vm.compiler.sema.resolvedFuncSigs.items[dstRFuncSigId];
                vm.funcSyms.buf[symId] = .{
                    .entryT = @intFromEnum(rt.FuncSymbolType.hostFunc),
                    .innerExtra = .{
                        .hostFunc = .{
                            .typedFlagNumParams = rFuncSig.numParams(),
                            .funcSigId = @intCast(dstRFuncSigId),
                        }
                    },
                    .inner = .{
                        .hostFunc = obj.nativeFunc1.func,
                    },
                };
                vm.funcSymDeps.put(vm.alloc, symId, val) catch cy.fatal();
            },
            rt.LambdaT => {
                const dstRFuncSigId = getFuncSigIdOfSym(vm, symId);
                if (!isAssignFuncSigCompat(vm, @intCast(obj.lambda.funcSigId), dstRFuncSigId)) {
                    const sigId: u32 = @intCast(obj.lambda.funcSigId);
                    return @call(.never_inline, reportAssignFuncSigMismatch, .{vm, sigId, dstRFuncSigId});
                }
                releaseFuncSymDep(vm, symId);
                vm.funcSyms.buf[symId] = .{
                    .entryT = @intFromEnum(rt.FuncSymbolType.func),
                    .inner = .{
                        .func = .{
                            .pc = obj.lambda.funcPc,
                            .stackSize = obj.lambda.stackSize,
                            .numParams = obj.lambda.numParams,
                        },
                    },
                };
                vm.funcSymDeps.put(vm.alloc, symId, val) catch cy.fatal();
            },
            rt.ClosureT => {
                const dstRFuncSigId = getFuncSigIdOfSym(vm, symId);
                if (!isAssignFuncSigCompat(vm, @intCast(obj.closure.funcSigId), dstRFuncSigId)) {
                    const sigId: u32 = @intCast(obj.closure.funcSigId);
                    return @call(.never_inline, reportAssignFuncSigMismatch, .{vm, sigId, dstRFuncSigId});
                }
                releaseFuncSymDep(vm, symId);
                vm.funcSyms.buf[symId] = .{
                    .entryT = @intFromEnum(rt.FuncSymbolType.closure),
                    .inner = .{
                        .closure = val.asPointer(*cy.heap.Closure),
                    },
                };
                // Don't set func sym dep since the closure is assigned into the func sym entry.
            },
            else => {
                return vm.panicFmt("Assigning to static function with unsupported type {}.", &.{v(obj.getTypeId())});
            }
        }
    } else {
        return vm.panic("Assigning to static function with a non-function value.");
    }
}

fn getFuncSigIdOfSym(vm: *const VM, symId: SymbolId) sema.FuncSigId {
    switch (@as(rt.FuncSymbolType, @enumFromInt(vm.funcSyms.buf[symId].entryT))) {
        .hostFunc => {
            return vm.funcSyms.buf[symId].innerExtra.hostFunc.funcSigId;
        },
        .hostQuickenFunc => {
            return vm.funcSyms.buf[symId].innerExtra.hostFunc.funcSigId;
        },
        .func => {
            return vm.funcSyms.buf[symId].innerExtra.func.funcSigId;
        },
        .closure => {
            return @intCast(vm.funcSyms.buf[symId].inner.closure.funcSigId);
        },
        .none => {
            return vm.funcSyms.buf[symId].innerExtra.none.funcSigId;
        },
    }
}

fn callMethodNoInline(
    vm: *VM, pc: [*]cy.Inst, sp: [*]cy.Value, methodType: rt.MethodType, data: rt.MethodData,
    typeId: vmc.TypeId, startLocal: u8, numArgs: u8, reqNumRetVals: u8, anySelfFuncSigId: sema.FuncSigId,
) !?cy.fiber.PcSp {
    return @call(.always_inline, callMethod, .{vm, pc, sp, methodType, data, typeId, startLocal, numArgs, reqNumRetVals, anySelfFuncSigId});
}

fn callMethod(
    vm: *VM, pc: [*]cy.Inst, sp: [*]cy.Value, methodType: rt.MethodType, data: rt.MethodData,
    typeId: vmc.TypeId, startLocal: u8, numArgs: u8, reqNumRetVals: u8, anySelfFuncSigId: sema.FuncSigId,
) !?cy.fiber.PcSp {
    switch (methodType) {
        .untyped => {
            if (numArgs != data.untyped.numParams) {
                return null;
            }
            if (@intFromPtr(sp + startLocal + data.untyped.stackSize) >= @intFromPtr(vm.stackEndPtr)) {
                return error.StackOverflow;
            }

            // Optimize.
            pc[0] = cy.Inst.initOpCode(.callObjFuncIC);
            pc[7] = cy.Inst{ .val = @intCast(data.untyped.stackSize) };
            @as(*align(1) u32, @ptrCast(pc + 8)).* = data.untyped.pc;
            @as(*align(1) u16, @ptrCast(pc + 14)).* = @intCast(typeId);

            const newFp = sp + startLocal;
            newFp[1] = buildReturnInfo(reqNumRetVals, true, cy.bytecode.CallObjSymInstLen);
            newFp[2] = Value{ .retPcPtr = pc + cy.bytecode.CallObjSymInstLen };
            newFp[3] = Value{ .retFramePtr = sp };
            return cy.fiber.PcSp{
                .pc = cy.fiber.toVmPc(vm, data.untyped.pc),
                .sp = newFp,
            };
        },
        .untypedNative1 => {
            if (numArgs != data.untypedNative1.numParams) {
                return null;
            }
            // Optimize.
            pc[0] = cy.Inst.initOpCode(.callObjNativeFuncIC);
            @as(*align(1) u48, @ptrCast(pc + 8)).* = @intCast(@intFromPtr(data.untypedNative1.ptr));
            @as(*align(1) u16, @ptrCast(pc + 14)).* = @intCast(typeId);

            vm.framePtr = sp;
            const res: Value = @bitCast(data.untypedNative1.ptr.?(@ptrCast(vm), @ptrCast(sp + startLocal + 4), numArgs));
            if (res.isInterrupt()) {
                return error.Panic;
            }
            if (reqNumRetVals == 1) {
                sp[startLocal] = res;
            } else {
                switch (reqNumRetVals) {
                    0 => {
                        // Nop.
                    },
                    1 => cy.panic("not possible"),
                    else => {
                        cy.panic("unsupported");
                    },
                }
            }
            return cy.fiber.PcSp{
                .pc = pc + cy.bytecode.CallObjSymInstLen,
                .sp = sp,
            };
        },
        .untypedNative2 => {
            if (numArgs != data.untypedNative2.numParams) {
                return null;
            }
            vm.framePtr = sp;
            const res = data.untypedNative2.ptr(@ptrCast(vm), @ptrCast(sp + startLocal + 4), numArgs);
            if (res.left.isInterrupt()) {
                return error.Panic;
            }
            if (reqNumRetVals == 2) {
                sp[startLocal] = res.left;
                sp[startLocal+1] = res.right;
            } else {
                switch (reqNumRetVals) {
                    0 => {
                        release(vm, res.left);
                        release(vm, res.right);
                    },
                    1 => {
                        sp[startLocal] = res.left;
                        release(vm, res.right);
                    },
                    else => {
                        cy.panic("unsupported");
                    },
                }
            }
            return cy.fiber.PcSp{
                .pc = pc + cy.bytecode.CallObjSymInstLen,
                .sp = sp,
            };
        },
        .optimizing => {
            if (numArgs != data.optimizing.numParams) {
                return null;
            }

            // Optimize.
            pc[0] = cy.Inst.initOpCode(.callObjNativeFuncIC);
            @as(*align(1) u48, @ptrCast(pc + 8)).* = @intCast(@intFromPtr(data.optimizing.ptr));
            @as(*align(1) u16, @ptrCast(pc + 14)).* = @intCast(typeId);

            data.optimizing.ptr(@ptrCast(vm), pc, @ptrCast(sp + startLocal + 4), numArgs);
            return cy.fiber.PcSp{
                .pc = pc,
                .sp = sp,
            };
        },
        .typed => {
            if (numArgs != data.typed.numParams) {
                return null;
            }
            // Perform type check on args.
            const vals = sp[startLocal+5..startLocal+5+numArgs-1];
            const targetFuncSig = vm.compiler.sema.getFuncSig(data.typed.funcSigId);
            for (vals, targetFuncSig.params()[1..]) |val, cstrTypeId| {
                const valTypeId = val.getTypeId();
                const semaTypeId = vm.types.buf[valTypeId].semaTypeId;
                if (!types.isTypeSymCompat(vm.compiler, semaTypeId, cstrTypeId)) {
                    return null;
                }
            }

            if (@intFromPtr(sp + startLocal + data.typed.stackSize) >= @intFromPtr(vm.stackEndPtr)) {
                return error.StackOverflow;
            }

            // Optimize only if anySelfFuncSigId matches.
            if (anySelfFuncSigId == data.typed.funcSigId) {
                pc[0] = cy.Inst.initOpCode(.callObjFuncIC);
                pc[7] = cy.Inst{ .val = @intCast(data.typed.stackSize) };
                @as(*align(1) u32, @ptrCast(pc + 8)).* = data.typed.pc;
                @as(*align(1) u16, @ptrCast(pc + 14)).* = @intCast(typeId);
            }

            const newFp = sp + startLocal;
            newFp[1] = buildReturnInfo(reqNumRetVals, true, cy.bytecode.CallObjSymInstLen);
            newFp[2] = Value{ .retPcPtr = pc + cy.bytecode.CallObjSymInstLen };
            newFp[3] = Value{ .retFramePtr = sp };
            return cy.fiber.PcSp{
                .pc = cy.fiber.toVmPc(vm, data.typed.pc),
                .sp = newFp,
            };
        },
        .typedNative => {
            if (numArgs != data.typedNative.numParams) {
                return null;
            }
            // Perform type check on args.
            const vals = sp[startLocal+5..startLocal+5+numArgs-1];
            const targetFuncSig = vm.compiler.sema.getFuncSig(data.typedNative.funcSigId);
            for (vals, targetFuncSig.params()[1..]) |val, cstrTypeId| {
                const valTypeId = val.getTypeId();
                const semaTypeId = vm.types.buf[valTypeId].semaTypeId;
                if (!types.isTypeSymCompat(vm.compiler, semaTypeId, cstrTypeId)) {
                    return null;
                }
            }

            // Optimize only if anySelfFuncSigId matches.
            if (anySelfFuncSigId == data.typedNative.funcSigId) {
                pc[0] = cy.Inst.initOpCode(.callObjNativeFuncIC);
                @as(*align(1) u48, @ptrCast(pc + 8)).* = @intCast(@intFromPtr(data.typedNative.ptr));
                @as(*align(1) u16, @ptrCast(pc + 14)).* = @intCast(typeId);
            }

            vm.framePtr = sp;
            const res: Value = @bitCast(data.typedNative.ptr.?(@ptrCast(vm), @ptrCast(sp + startLocal + 4), numArgs));
            if (res.isInterrupt()) {
                return error.Panic;
            }
            if (reqNumRetVals == 1) {
                sp[startLocal] = res;
            } else {
                switch (reqNumRetVals) {
                    0 => {
                        // Nop.
                    },
                    1 => cy.panic("not possible"),
                    else => {
                        cy.panic("unsupported");
                    },
                }
            }
            return cy.fiber.PcSp{
                .pc = pc + cy.bytecode.CallObjSymInstLen,
                .sp = sp,
            };
        },
    }
}

/// Assumes there are overloaded methods.
fn callFirstOverloadedMethod(
    vm: *VM, pc: [*]cy.Inst, sp: [*]Value, mgId: vmc.MethodGroupId,
    typeId: vmc.TypeId, startLocal: u8, numArgs: u8, reqNumRetVals: u8, anySelfFuncSigId: sema.FuncSigId,
) !?cy.fiber.PcSp {
    const mgExt = &vm.methodGroupExts.buf[mgId];
    std.debug.assert(mgExt.mruTypeMethodGroupId != cy.NullId);

    const tmg = vm.typeMethodGroups.buf[mgExt.mruTypeMethodGroupId];
    std.debug.assert(tmg.head != tmg.tail);

    var methodId = tmg.head;
    while (methodId != cy.NullId) {
        const method = vm.methods.buf[methodId];
        if (methodId == mgExt.mruMethodId) {
            // Skip mruMethod.
            methodId = method.next;
            continue;
        }
        if (try @call(.never_inline, callMethodNoInline, .{vm, pc, sp, method.type, method.data, typeId, startLocal, numArgs, reqNumRetVals, anySelfFuncSigId})) |res| {
            // Update MethodGroup cache.
            // TypeMethodGroup is updated on MethodGroup cache miss.
            const mgPtr = &vm.methodGroups.buf[mgId];
            mgPtr.mruMethodType = method.type;
            mgPtr.mruMethodData = method.data;
            mgExt.mruMethodId = methodId;

            return res;
        }
        methodId = method.next;
    }
    return null;
}

/// Assumes cache hit on TypeMethodGroup.
fn callMethodGroup(
    vm: *VM, pc: [*]cy.Inst, sp: [*]Value, mgId: vmc.MethodGroupId, mg: rt.MethodGroup,
    typeId: vmc.TypeId, startLocal: u8, numArgs: u8, reqNumRetVals: u8, anySelfFuncSigId: sema.FuncSigId,
) linksection(cy.HotSection) !?cy.fiber.PcSp {
    if (try @call(.always_inline, callMethod, .{vm, pc, sp, mg.mruMethodType, mg.mruMethodData, typeId, startLocal, numArgs, reqNumRetVals, anySelfFuncSigId})) |res| {
        return res;
    }
    if (mg.mruTypeMethodOverloaded) {
        return @call(.never_inline, callFirstOverloadedMethod, .{vm, pc, sp, mgId, typeId, startLocal, numArgs, reqNumRetVals, anySelfFuncSigId});
    }
    return null;
}

inline fn deoptimizeBinOp(pc: [*]cy.Inst) void {
    pc[0] = cy.Inst.initOpCode(.callObjSym);
    pc[1] = pc[8];
    pc[2] = pc[9];
    pc[3] = pc[10];
    pc[4] = pc[11];
}

pub const ValidateResult = struct {
    err: ?cy.CompileErrorType,
};

export fn zFatal() void {
    cy.fatal();
}

export fn zOpCodeName(code: vmc.OpCode) [*:0]const u8 {
    const ecode: cy.OpCode = @enumFromInt(code);
    return @tagName(ecode);
}

export fn zCallSym(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, symId: u16, startLocal: u8, numArgs: u8, reqNumRetVals: u8) callconv(.C) vmc.PcSpResult {
    const numRetVals: u2 = @intCast(reqNumRetVals);
    const res = @call(.always_inline, VM.callSym, .{vm, pc, framePtr, @as(u32, @intCast(symId)), startLocal, numArgs, numRetVals}) catch |err| {
        if (err == error.Panic) {
            return .{
                .pc = undefined,
                .sp = undefined,
                .code = vmc.RES_CODE_PANIC,
            };
        } else if (err == error.StackOverflow) {
            return .{
                .pc = undefined,
                .sp = undefined,
                .code = vmc.RES_CODE_STACK_OVERFLOW,
            };
        } else {
            return .{
                .pc = undefined,
                .sp = undefined,
                .code = vmc.RES_CODE_UNKNOWN,
            };
        }
    };
    return .{
        .pc = @ptrCast(res.pc),
        .sp = @ptrCast(res.sp),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zDumpEvalOp(vm: *const VM, pc: [*]const cy.Inst) void {
    dumpEvalOp(vm, pc) catch cy.fatal();
}

export fn zFreeObject(vm: *cy.VM, obj: *HeapObject) linksection(cy.HotSection) void {
    cy.heap.freeObject(vm, obj, true, false, true, false);
} 

export fn zEnd(vm: *cy.VM, pc: [*]const cy.Inst) void {
    vm.endLocal = pc[1].val;
    vm.curFiber.pcOffset = @intCast(getInstOffset(vm, pc + 2));
}

export fn zAllocList(vm: *cy.VM, elemStart: [*]const Value, nElems: u8) vmc.ValueResult {
    const list = cy.heap.allocList(vm, elemStart[0..nElems]) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(list),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zOtherToF64(val: Value) f64 {
    return val.otherToF64() catch fatal();
}

export fn zCallObjSym(
    vm: *cy.VM, pc: [*]cy.Inst, stack: [*]Value, recv: Value,
    typeId: rt.TypeId, mgId: u8, startLocal: u8, numArgs: u8, numRet: u8, anySelfFuncSigId: u16,
) vmc.CallObjSymResult {
    const mbMethodGroup = vm.getCachedMethodGroupForType(typeId, mgId) catch {
        return .{
            .pc = undefined,
            .stack = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    if (mbMethodGroup) |sym| {
        const mb_res = @call(.always_inline, callMethodGroup, .{
            vm, pc, stack, mgId, sym, typeId, startLocal, numArgs, numRet, anySelfFuncSigId,
        }) catch |err| {
            if (err == error.Panic) {
                return .{
                    .pc = undefined,
                    .stack = undefined,
                    .code = vmc.RES_CODE_PANIC,
                };
            } else {
                return .{
                    .pc = undefined,
                    .stack = undefined,
                    .code = vmc.RES_CODE_UNKNOWN,
                };
            }
        };
        if (mb_res) |res| {
            return .{
                .pc = @ptrCast(res.pc),
                .stack = @ptrCast(res.sp),
                .code = vmc.RES_CODE_SUCCESS,
            };
        }
    }
    const res = @call(.never_inline, callObjSymFallback, .{vm, pc, stack, recv, typeId, mgId, startLocal, numArgs, numRet}) catch |err| {
        if (err == error.Panic) {
            return .{
                .pc = undefined,
                .stack = undefined,
                .code = vmc.RES_CODE_PANIC,
            };
        } else {
            return .{
                .pc = undefined,
                .stack = undefined,
                .code = vmc.RES_CODE_UNKNOWN,
            };
        }
    };
    return .{
        .pc = @ptrCast(res.pc),
        .stack = @ptrCast(res.sp),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zAllocFiber(vm: *cy.VM, pc: u32, args: [*]const Value, nargs: u8, initialStackSize: u8) vmc.ValueResult {
    const fiber = cy.fiber.allocFiber(vm, pc, args[0..nargs], initialStackSize) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(fiber),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zPushFiber(vm: *cy.VM, curFiberEndPc: usize, curStack: [*]Value, fiber: *cy.Fiber, parentDstLocal: u8) vmc.PcSp {
    const res = cy.fiber.pushFiber(vm, curFiberEndPc, curStack, fiber, parentDstLocal);
    return .{
        .pc = @ptrCast(res.pc),
        .sp = @ptrCast(res.sp),
    };
}

export fn zPopFiber(vm: *cy.VM, curFiberEndPc: usize, curStack: [*]Value, retValue: Value) vmc.PcSp {
    const res = cy.fiber.popFiber(vm, curFiberEndPc, curStack, retValue);
    return .{
        .pc = @ptrCast(res.pc),
        .sp = @ptrCast(res.sp),
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
        .val = @bitCast(res),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zGetFieldOffsetFromTable(vm: *VM, typeId: rt.TypeId, symId: SymbolId) u8 {
    return vm.getFieldOffsetFromTable(typeId, symId);
}

export fn zEvalCompare(vm: *VM, left: Value, right: Value) vmc.Value {
    return @bitCast(evalCompare(vm, left, right));
}

export fn zEvalCompareNot(vm: *VM, left: Value, right: Value) vmc.Value {
    return @bitCast(evalCompareNot(vm, left, right));
}

export fn zCall(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) vmc.PcSpResult {
    const res = call(vm, pc, framePtr, callee, startLocal, numArgs, retInfo) catch |err| {
        if (err == error.Panic) {
            return .{
                .pc = undefined,
                .sp = undefined,
                .code = vmc.RES_CODE_PANIC,
            };
        } else {
            return .{
                .pc = undefined,
                .sp = undefined,
                .code = vmc.RES_CODE_UNKNOWN,
            };
        }
    };
    return .{
        .pc = @ptrCast(res.pc),
        .sp = @ptrCast(res.sp),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zAllocPoolObject(vm: *cy.VM) vmc.HeapObjectResult {
    const obj = cy.heap.allocPoolObject(vm) catch {
        return .{
            .obj = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .obj = @ptrCast(obj),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zAllocExternalCycObject(vm: *cy.VM, size: usize) vmc.HeapObjectResult {
    const obj = cy.heap.allocExternalObject(vm, size, true) catch {
        return .{
            .obj = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .obj = @ptrCast(obj),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zAllocExternalObject(vm: *cy.VM, size: usize) vmc.HeapObjectResult {
    const obj = cy.heap.allocExternalObject(vm, size, false) catch {
        return .{
            .obj = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .obj = @ptrCast(obj),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zAllocStringTemplate(vm: *cy.VM, strs: [*]cy.Inst, strCount: u8, vals: [*]Value, valCount: u8) vmc.ValueResult {
    const val = cy.heap.allocStringTemplate(vm, strs[0..strCount], vals[0..valCount]) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(val),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zDumpValue(val: Value) void {
    val.dump();
}

export fn zAllocMap(vm: *VM, keyIdxes: [*] align(1) u16, vals: [*]Value, numEntries: u32) vmc.ValueResult {
    const val = cy.heap.allocMap(vm, keyIdxes[0..numEntries], vals[0..numEntries]) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(val),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zGetFieldFallback(vm: *const VM, obj: *const HeapObject, nameId: sema.NameSymId) vmc.Value {
    return @bitCast(vm.getFieldFallback(obj, nameId));
}

export fn zAlloc(alloc: vmc.ZAllocator, n: usize) vmc.BufferResult {
    const zalloc = std.mem.Allocator{
        .ptr = @ptrCast(alloc.ptr),
        .vtable = @ptrCast(@alignCast(alloc.vtable)),
    };
    const buf = zalloc.alloc(u8, n) catch {
        return .{
            .buf = undefined,
            .len = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .buf = @ptrCast(buf.ptr),
        .len = buf.len,
        .code = vmc.RES_CODE_SUCCESS,
    };
}

export fn zPanicIncompatibleFuncSig(vm: *cy.VM, funcId: rt.FuncId, args: [*]const Value, numArgs: usize, targetFuncSigId: sema.FuncSigId) void {
    panicIncompatibleFuncSig(vm, funcId, args[0..numArgs], targetFuncSigId) catch {};
}

export fn zSetStaticFunc(vm: *VM, funcId: SymbolId, val: Value) vmc.ResultCode {
    setStaticFunc(vm, funcId, val) catch |err| {
        if (err == error.Panic) {
            return vmc.RES_CODE_PANIC;
        } else {
            return vmc.RES_CODE_UNKNOWN;
        }
    };
    return vmc.RES_CODE_SUCCESS;
}

export fn zGrowTryStackTotalCapacity(list: *cy.List(vmc.TryFrame), alloc: vmc.ZAllocator, minCap: usize) vmc.ResultCode {
    const zalloc = std.mem.Allocator{
        .ptr = @ptrCast(alloc.ptr),
        .vtable = @ptrCast(@alignCast(alloc.vtable)),
    };
    list.growTotalCapacity(zalloc, minCap) catch {
        return vmc.RES_CODE_UNKNOWN;
    };
    return vmc.RES_CODE_SUCCESS;
}

export fn zThrow(vm: *cy.VM, startFp: [*]Value, pc: [*]const cy.Inst, err: Value) vmc.PcSpResult {
    const mbRes = cy.fiber.throw(vm, startFp, pc, err) catch {
        return .{
            .pc = undefined,
            .sp = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    if (mbRes) |res| {
        return .{
            .pc = @ptrCast(res.pc),
            .sp = @ptrCast(res.sp),
            .code = vmc.RES_CODE_SUCCESS,
        };
    } else {
        vm.panicWithUncaughtError(err) catch {};
        return .{
            .pc = undefined,
            .sp = undefined,
            .code = vmc.RES_CODE_PANIC,
        };
    }
}

export fn zOpMatch(vm: *const VM, pc: [*]const cy.Inst, framePtr: [*]const Value) u16 {
    return opMatch(vm, pc, framePtr);
}

export fn zPrintStderr(fmtz: [*:0]const u8, valsPtr: [*]const fmt.FmtValue, len: usize) void {
    const format = std.mem.sliceTo(fmtz, 0);
    const vals = valsPtr[0..len];
    fmt.printStderr(format, vals);
}

export fn zCheckDoubleFree(vm: *cy.VM, obj: *cy.HeapObject) void {
    if (cy.Trace) {
        cy.arc.checkDoubleFree(vm, obj);
    }
}

export fn zCheckRetainDanglingPointer(vm: *cy.VM, obj: *cy.HeapObject) void {
    if (cy.Trace) {
        cy.arc.checkRetainDanglingPointer(vm, obj);
    }
}

export fn zPanicFmt(vm: *VM, formatz: [*:0]const u8, argsPtr: [*]const fmt.FmtValue, numArgs: usize) void {
    const format = formatz[0..std.mem.sliceTo(formatz, 0).len];
    const args = argsPtr[0..numArgs];

    const msg = fmt.allocFormat(vm.alloc, format, args) catch |err| {
        if (err == error.OutOfMemory) {
            vm.curFiber.panicType = vmc.PANIC_INFLIGHT_OOM;
            return;
        } else {
            cy.panic("unexpected");
        }
    };
    vm.curFiber.panicPayload = @as(u64, @intFromPtr(msg.ptr)) | (@as(u64, msg.len) << 48);
    vm.curFiber.panicType = vmc.PANIC_MSG;
    log.tracev("{s}", .{msg});
}

export fn zMapSet(vm: *VM, map: *cy.heap.Map, key: Value, val: Value) vmc.ResultCode {
    map.set(vm, key, val) catch {
        return vmc.RES_CODE_UNKNOWN;
    };
    return vmc.RES_CODE_SUCCESS;
}

export fn zValueMapGet(vm: *VM, map: *cy.ValueMap, key: Value, found: *bool) Value {
    if (map.get(vm, key)) |val| {
        found.* = true;
        return val;
    } else {
        found.* = false;
        return undefined;
    }
}

fn c_strlen(s: [*:0]const u8) callconv(.C) usize {
    return std.mem.sliceTo(s, 0).len;
}

fn c_pow(b: f64, e: f64) callconv(.C) f64 {
    return std.math.pow(f64, b, e);
}

comptime {
    if (cy.isWasmFreestanding and build_options.vmEngine == .c) {
        @export(c_strlen, .{ .name = "strlen", .linkage = .Strong });
        @export(c_pow, .{ .name = "pow", .linkage = .Strong });
    }
}

const DummyCyclableNode = extern struct {
    prev: ?*cy.heap.DListNode,
    next: ?*cy.heap.DListNode,
    typeId: u32,
};
pub var dummyCyclableHead = DummyCyclableNode{
    .prev = null,
    .next = null,
    // This will be marked automatically before sweep, so it's never considered as a cyc object.
    .typeId = vmc.GC_MARK_MASK | rt.NoneT,
};

pub fn defaultPrint(_: *UserVM, _: cy.Str) callconv(.C) void {
    // Default print is a nop.
}