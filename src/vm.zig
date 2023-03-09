const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const aarch64 = builtin.cpu.arch == .aarch64;
const stdx = @import("stdx");
const fatal = stdx.fatal;
const t = stdx.testing;
const tcc = @import("tcc");

const fmt = @import("fmt.zig");
const v = fmt.v;
const cy = @import("cyber.zig");
const sema = @import("sema.zig");
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

    stackTrace: cy.StackTrace,

    /// Since func syms can be reassigned, track any RC dependencies.
    funcSymDeps: std.AutoHashMapUnmanaged(SymbolId, Value),
    methodSymExtras: cy.List(MethodSymExtra),
    debugTable: []const cy.DebugSym,

    curFiber: *cy.Fiber,
    mainFiber: cy.Fiber,

    /// Local to be returned back to eval caller.
    /// 255 indicates no return value.
    endLocal: u8,

    panicType: debug.PanicType,
    panicPayload: debug.PanicPayload,

    /// Interface used for imports and fetch.
    httpClient: http.HttpClient,
    stdHttpClient: http.StdHttpClient,

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

    lastError: ?error{TokenError, ParseError, CompileError, Panic},

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
            self.structs.clearRetainingCapacity();
            self.structSignatures.clearRetainingCapacity();
        } else {
            self.structs.deinit(self.alloc);
            self.structSignatures.deinit(self.alloc);
        }

        for (self.tagLitSyms.items()) |sym| {
            if (sym.nameOwned) {
                self.alloc.free(sym.name);
            }
        }
        if (reset) {
            self.tagTypes.clearRetainingCapacity();
            self.tagTypeSignatures.clearRetainingCapacity();
            self.tagLitSyms.clearRetainingCapacity();
            self.tagLitSymSignatures.clearRetainingCapacity();
        } else {
            self.tagTypes.deinit(self.alloc);
            self.tagTypeSignatures.deinit(self.alloc);
            self.tagLitSyms.deinit(self.alloc);
            self.tagLitSymSignatures.deinit(self.alloc);
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
            self.stdHttpClient.deinit();
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
        fmt.printStderr("stack framePtr: {}\n", &.{v(framePtrOffset(self, self.framePtr))});
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
        self.pc = @ptrCast([*]cy.OpData, buf.ops.items.ptr);
        try self.stackEnsureTotalCapacity(buf.mainStackSize);
        self.framePtr = @ptrCast([*]Value, self.stack.ptr);

        self.ops = buf.ops.items;
        self.consts = buf.mconsts;
        self.strBuf = buf.strBuf.items;

        @call(.never_inline, evalLoopGrowStack, .{self}) catch |err| {
            if (err == error.Panic) {
                self.lastError = error.Panic;
                unwindReleaseStack(self, self.stack, self.framePtr, self.pc);
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
                    return bindings.stringSlice(.astring)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2);
                },
                cy.UstringT => {
                    retainObject(self, obj);
                    return bindings.stringSlice(.ustring)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2);
                },
                cy.StringSliceT => {
                    retainObject(self, obj);
                    return bindings.stringSlice(.slice)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2);
                },
                cy.RawStringT => {
                    retainObject(self, obj);
                    return bindings.stringSlice(.rawstring)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2);
                },
                cy.RawStringSliceT => {
                    retainObject(self, obj);
                    return bindings.stringSlice(.rawSlice)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2);
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
                    cy.StaticAstringT => return bindings.stringSlice(.staticAstring)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2),
                    cy.StaticUstringT => return bindings.stringSlice(.staticUstring)(@ptrCast(*UserVM, self), recv.*, &[_]Value{startV, endV}, 2),
                    else => {
                        return self.panicFmt("Unsupported slice operation on type `{}`.", &.{v(@intCast(u8, recv.getTag()))});
                    },
                }
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

    pub fn ensureTagType(self: *VM, name: []const u8) !TagTypeId {
        const res = try self.tagTypeSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            return self.addTagType(name);
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn addAnonymousStruct(self: *VM, baseName: []const u8) !TypeId {
        const name = try self.alloc.alloc(u8, baseName.len + 16);
        defer self.alloc.free(name);

        var attempts: u32 = 0;
        while (attempts < 100): (attempts += 1) {
            const next = math_mod.rand.next();
            std.mem.copy(u8, name[0..baseName.len], baseName);
            _ = std.fmt.formatIntBuf(name[baseName.len..], next, 16, .lower, .{ .width = 16, .fill = '0'});
            if (!self.compiler.semaNameSymMap.contains(name)) {
                const nameId = try cy.sema.ensureNameSymExt(&self.compiler, name, true);
                return try self.ensureObjectType(cy.NullId, nameId);
            }
        }
        return error.TooManyAttempts;
    }

    pub fn ensureObjectType(self: *VM, rParentSymId: sema.ResolvedSymId, nameId: sema.NameSymId) !TypeId {
        const res = try @call(.never_inline, self.structSignatures.getOrPut, .{self.alloc, .{
            .structKey = .{
                .rParentSymId = rParentSymId,
                .nameId = nameId,
            },
        }});
        if (!res.found_existing) {
            return self.addObjectTypeExt(rParentSymId, nameId);
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

    pub inline fn getObjectTypeId(self: *const VM, rParentSymId: sema.ResolvedSymId, nameId: sema.NameSymId) ?TypeId {
        return self.structSignatures.get(.{
            .structKey = .{
                .rParentSymId = rParentSymId,
                .nameId = nameId,
            },
        });
    }

    pub fn addObjectTypeExt(self: *VM, rParentSymId: sema.ResolvedSymId, nameId: sema.NameSymId) !TypeId {
        const name = sema.getName(&self.compiler, nameId);
        const s = Struct{
            .name = name,
            .numFields = 0,
        };
        const id = @intCast(u32, self.structs.len);
        try self.structs.append(self.alloc, s);
        try self.structSignatures.put(self.alloc, .{
            .structKey = .{
                .rParentSymId = rParentSymId,
                .nameId = nameId,
            },
        }, id);
        return id;
    }

    pub fn addObjectType(self: *VM, name: []const u8) !TypeId {
        const nameId = try sema.ensureNameSym(&self.compiler, name);
        return self.addObjectTypeExt(cy.NullId, nameId);
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
        const key = RtVarSymKey{
            .rtVarSymKey = .{
                .rParentSymId = rParentSymId,
                .nameId = nameId,
            }
        };
        return self.varSymSigs.get(key);
    }

    pub fn ensureVarSym(self: *VM, rParentSymId: sema.ResolvedSymId, nameId: u32) !SymbolId {
        const key = RtVarSymKey{
            .rtVarSymKey = .{
                .rParentSymId = rParentSymId,
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

            // const rFuncSig = self.compiler.semaResolvedFuncSigs.items[rFuncSigId];
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

    pub fn getTagLitName(self: *const VM, id: u32) []const u8 {
        return self.tagLitSyms.buf[id].name;
    }

    pub fn ensureTagLitSym(self: *VM, name: []const u8) !SymbolId {
        return self.ensureTagLitSymExt(name, false);
    }

    pub fn ensureTagLitSymExt(self: *VM, name: []const u8, owned: bool) !SymbolId {
        const res = try self.tagLitSymSignatures.getOrPut(self.alloc, name);
        if (!res.found_existing) {
            const id = @intCast(u32, self.tagLitSyms.len);
            var effName = name;
            if (owned) {
                effName = try self.alloc.dupe(u8, name);
            }
            try self.tagLitSyms.append(self.alloc, .{
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
        return self.ensureMethodSymKey2(name, numParams, false);
    }

    pub fn ensureMethodSymKey2(self: *VM, name: []const u8, numParams: u32, dupeName: bool) !SymbolId {
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
            const obj = left.asHeapObject();
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
            const obj = left.asHeapObject();
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
            const obj = left.asHeapObject();
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
                    return bindings.stringCharAt(.astring)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                },
                cy.UstringT => {
                    const idx = @intToFloat(f64, @intCast(i32, obj.ustring.charLen) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    return bindings.stringCharAt(.ustring)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                },
                cy.StringSliceT => {
                    if (obj.stringSlice.isAstring()) {
                        const idx = @intToFloat(f64, @intCast(i32, obj.stringSlice.len) + @floatToInt(i32, index.toF64()));
                        retainObject(self, obj);
                        return bindings.stringCharAt(.slice)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                    } else {
                        const idx = @intToFloat(f64, @intCast(i32, obj.stringSlice.uCharLen) + @floatToInt(i32, index.toF64()));
                        retainObject(self, obj);
                        return bindings.stringCharAt(.slice)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                    }
                },
                cy.RawStringT => {
                    const idx = @intToFloat(f64, @intCast(i32, obj.rawstring.len) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    return bindings.stringCharAt(.rawstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                },
                cy.RawStringSliceT => {
                    const idx = @intToFloat(f64, @intCast(i32, obj.rawstringSlice.len) + @floatToInt(i32, index.toF64()));
                    retainObject(self, obj);
                    return bindings.stringCharAt(.rawSlice)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
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
                        return bindings.stringCharAt(.staticAstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
                    },
                    cy.StaticUstringT => {
                        const start = left.asStaticStringSlice().start;
                        const idx = @intToFloat(f64, @intCast(i32, cy.string.getStaticUstringHeader(self, start).charLen) + @floatToInt(i32, index.toF64()));
                        return bindings.stringCharAt(.staticUstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{Value.initF64(idx)}, 1);
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
            const obj = left.asHeapObject();
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
                    return bindings.stringCharAt(.astring)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1);
                },
                cy.UstringT => {
                    retainObject(self, obj);
                    return bindings.stringCharAt(.ustring)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1);
                },
                cy.StringSliceT => {
                    retainObject(self, obj);
                    return bindings.stringCharAt(.slice)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1);
                },
                cy.RawStringT => {
                    retainObject(self, obj);
                    return bindings.stringCharAt(.rawstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1);
                },
                cy.RawStringSliceT => {
                    retainObject(self, obj);
                    return bindings.stringCharAt(.rawSlice)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1);
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
                    cy.StaticAstringT => return bindings.stringCharAt(.staticAstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1),
                    cy.StaticUstringT => return bindings.stringCharAt(.staticUstring)(@ptrCast(*UserVM, self), left.*, &[_]Value{index}, 1),
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
            const obj = recv.asHeapObject();
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
                return self.getFieldFallback(obj, self.fieldSyms.buf[symId].name);
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

                self.framePtr = newFramePtr;
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
                newFramePtr[1] = buildReturnInfo2(reqNumRetVals, true);
                newFramePtr[2] = Value{ .retPcPtr = pc + 11 };
                newFramePtr[3] = Value{ .retFramePtr = framePtr };
                return PcFramePtr{
                    .pc = self.toPc(sym.inner.func.pc),
                    .framePtr = newFramePtr,
                };
            },
            .closure => {
                if (@ptrToInt(framePtr + startLocal + sym.inner.closure.numLocals) >= @ptrToInt(self.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const newFramePtr = framePtr + startLocal;
                newFramePtr[1] = buildReturnInfo2(reqNumRetVals, true);
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
                return @call(.never_inline, reportMissingFuncSym, .{self, pc, symId});
            },
            // else => {
            //     return self.panic("unsupported callsym");
            // },
        }
    }

    fn reportMissingFuncSym(vm: *VM, pc: [*]const cy.OpData, rtSymId: SymbolId) error{Panic, OutOfMemory} {
        const relPc = pcOffset(vm, pc);
        if (debug.getDebugSym(vm, relPc)) |sym| {
            const chunk = vm.compiler.chunks.items[sym.file];
            const callExpr = chunk.nodes[sym.loc];
            const callee = chunk.nodes[callExpr.head.callExpr.callee];
            var symId: cy.sema.SymId = undefined;
            if (callee.node_t == .accessExpr) {
                symId = callee.head.accessExpr.semaSymId;
            } else if (callee.node_t == .ident) {
                symId = callee.head.ident.semaSymId;
            } else {
                return vm.panic("Failed to generate error report.");
            }
            const key = chunk.semaSyms.items[symId].key.absLocalSymKey;
            const name = cy.sema.getName(&vm.compiler, key.nameId);
            var rpSymId: sema.ResolvedSymId = undefined;
            if (key.parentSymId == cy.NullId) {
                rpSymId = chunk.semaResolvedRootSymId;
            } else {
                rpSymId = chunk.semaSyms.items[key.parentSymId].rSymId;
            }
            const rkey = sema.AbsResolvedSymKey{
                .absResolvedSymKey = .{
                    .rParentSymId = rpSymId,
                    .nameId = key.nameId,
                },
            };
            if (vm.compiler.semaResolvedSymMap.get(rkey)) |rsymId| {
                const rsym = vm.compiler.semaResolvedSyms.items[rsymId];
                if (rsym.symT == .func) {
                    const rtSym = vm.funcSyms.buf[rtSymId];
                    if (rsym.inner.func.rFuncSymId == cy.NullId) {
                        const sigStr = try sema.getResolvedFuncSigTempStr(&vm.compiler, rtSym.innerExtra.none.rFuncSigId);
                        return vm.panicFmt("Unsupported call signature: `{}{}`.\nThere are multiple overloaded functions named `{}`", &.{
                            v(name), v(sigStr), v(name),
                        });
                    } else {
                        const rfSym = vm.compiler.semaResolvedFuncSyms.items[rsym.inner.func.rFuncSymId];

                        vm.u8Buf.clearRetainingCapacity();
                        const w = vm.u8Buf.writer(vm.alloc);
                        cy.sema.writeResolvedFuncSigStr(&vm.compiler, w, rtSym.innerExtra.none.rFuncSigId) catch fatal();
                        const rtSigStr = vm.u8Buf.items();
                        const start = vm.u8Buf.len;
                        cy.sema.writeResolvedFuncSigStr(&vm.compiler, w, rfSym.rFuncSigId) catch fatal();
                        const existingSigStr = vm.u8Buf.buf[start..vm.u8Buf.len];

                        return vm.panicFmt("Unsupported call signature: `{}{}`.\nA function with signature `{}{}` exists.", &.{
                            v(name), v(rtSigStr), v(name), v(existingSigStr),
                        });
                    }
                }
            } else {
                return vm.panicFmt("Missing function symbol `{}`.", &.{
                    v(name),
                });
            }
            return vm.panic("Failed to generate error report.");
        } else {
            return vm.panicFmt("Missing debug sym at {}", &.{v(pcOffset(vm, pc))});
        }
    }

    fn callSymEntry(self: *VM, pc: [*]cy.OpData, framePtr: [*]Value, sym: MethodSym, recv: Value, typeId: u32, startLocal: u8, numArgs: u8, reqNumRetVals: u8) linksection(cy.HotSection) !PcFramePtr {
        switch (sym.entryT) {
            .func => {
                if (@ptrToInt(framePtr + startLocal + sym.inner.func.numLocals) >= @ptrToInt(self.stackEndPtr)) {
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
                const res = sym.inner.nativeFunc1(@ptrCast(*UserVM, self), recv, @ptrCast([*]const Value, framePtr + startLocal + 4), numArgs);
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
                const res = sym.inner.nativeFunc2(@ptrCast(*UserVM, self), recv, @ptrCast([*]const Value, framePtr + startLocal + 4), numArgs);
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
                        std.fmt.format(writer, "{}", .{val.asInteger()}) catch stdx.fatal();
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
        const obj = val.asHeapObject();
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

fn evalMinusFallback(vm: *VM, left: Value, right: Value) linksection(cy.HotSection) !Value {
    @setCold(true);
    return Value.initF64(try toF64OrPanic(vm, left) - try toF64OrPanic(vm, right));
}

fn evalPower(vm: *VM, left: cy.Value, right: cy.Value) !cy.Value {
    if (Value.bothNumbers(left, right)) {
        return Value.initF64(std.math.pow(f64, left.asF64(), right.asF64()));
    } else {
        return Value.initF64(std.math.pow(f64, try toF64OrPanic(vm, left), try toF64OrPanic(vm, right)));
    }
}

fn evalDivideFallback(vm: *cy.VM, left: cy.Value, right: cy.Value) !cy.Value {
    @setCold(true);
    return Value.initF64(try toF64OrPanic(vm, left) / try toF64OrPanic(vm, right));
}

fn evalModFallback(vm: *cy.VM, left: cy.Value, right: cy.Value) !cy.Value {
    @setCold(true);
    return Value.initF64(std.math.mod(f64, try toF64OrPanic(vm, left), try toF64OrPanic(vm, right)) catch std.math.nan_f64);
}

fn evalMultiplyFallback(vm: *cy.VM, left: cy.Value, right: cy.Value) !cy.Value {
    @setCold(true);
    return Value.initF64(try toF64OrPanic(vm, left) * try toF64OrPanic(vm, right));
}

fn evalAddFallback(vm: *cy.VM, left: cy.Value, right: cy.Value) linksection(cy.HotSection) !cy.Value {
    @setCold(true);
    return Value.initF64(try toF64OrPanic(vm, left) + try toF64OrPanic(vm, right));
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

fn panicConvertNumberError(vm: *cy.VM, val: Value) error{Panic, OutOfMemory} {
    @setCold(true);
    const typeId = val.getTypeId();
    return vm.panicFmt("Cannot convert `{}` to number.", &.{v(vm.structs.buf[typeId].name)});
}

fn convToF64OrPanic(val: Value) linksection(cy.HotSection) !f64 {
    if (val.isPointer()) {
        const obj = val.asHeapObject();
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
    nameOwned: bool,
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

/// Keeping this small is better for function calls.
/// Secondary symbol data should be moved to `methodSymExtras`.
pub const MethodSym = struct {
    entryT: MethodSymType,
    /// Most recent sym used is cached avoid hashmap lookup. 
    mruStructId: TypeId,
    inner: packed union {
        nativeFunc1: cy.NativeObjFuncPtr,
        nativeFunc2: cy.NativeObjFunc2Ptr,
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

    pub fn initNativeFunc1(func: cy.NativeObjFuncPtr) MethodSym {
        return .{
            .entryT = .nativeFunc1,
            .mruStructId = undefined,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    pub fn initNativeFunc2(func: cy.NativeObjFunc2Ptr) MethodSym {
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
                try @call(.never_inline, debug.buildStackTrace, .{vm, true});
                return error.Panic;
            } else return err;
        };
        return;
    }
}

inline fn gotoNext(pc: [*]cy.OpData, jumpTablePtr: u64) void {
    if (EnableAarch64ComputedGoto) {
        const asmPc = asm (
            \\ldrb w8, [%[pc]]
            \\ldrsw  x10, [x28, x8, lsl #2]
            \\adr x9, LOpBase
            // TODO: Remove extra offset addition somehow.
            \\add x9, x9, #4
            \\add x9, x9, x10
            : [ret] "={x9}" (-> [*]const u32)
            : [pc] "r" (pc),
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

/// This is experimental and does not work in all branch cases.
/// The current issue is that some frequently used registers (eg. TrueMask)
/// are restored after the inserted computed gotos which leads to undefined behavior.
const EnableAarch64ComputedGoto = build_options.fastArm64;
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
        if (builtin.mode == .Debug) {
            vm.debugPc = pcOffset(vm, pc);
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
            vm.debugPc = pcOffset(vm, pc);
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .constOp => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstOp:"::);
                }
                framePtr[pc[2].arg] = Value.initRaw(vm.consts[pc[1].arg].val);
                pc += 3;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .constI8 => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstI8:"::);
                }
                framePtr[pc[2].arg] = Value.initF64(@intToFloat(f64, @bitCast(i8, pc[1].arg)));
                pc += 3;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .constI8Int => {
                if (GenLabels) {
                    _ = asm volatile ("LOpConstI8Int:"::);
                }
                framePtr[pc[2].arg] = Value.initI32(@intCast(i32, @bitCast(i8, pc[1].arg)));
                pc += 3;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .release => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRelease:"::);
                }
                release(vm, framePtr[pc[1].arg]);
                pc += 2;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    if (obj.common.structId == @ptrCast(*align (1) u16, pc + 4).*) {
                        framePtr[dst] = obj.object.getValue(pc[6].arg);
                        pc += 7;
                        if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    break :b cond.assumeNotBoolToBool();
                };
                if (!condVal) {
                    pc += jump;
                } else {
                    pc += 4;
                }
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    framePtr[pc[3].arg] = try @call(.never_inline, evalAddFallback, .{ vm, left, right });
                }
                pc += 4;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .sub => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSub:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = if (Value.bothNumbers(left, right))
                    Value.initF64(left.asF64() - right.asF64())
                else try @call(.never_inline, evalMinusFallback, .{vm, left, right});
                pc += 4;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .true => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTrue:"::);
                }
                framePtr[pc[1].arg] = Value.True;
                pc += 2;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .false => {
                if (GenLabels) {
                    _ = asm volatile ("LOpFalse:"::);
                }
                framePtr[pc[1].arg] = Value.False;
                pc += 2;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .not => {
                if (GenLabels) {
                    _ = asm volatile ("LOpNot:"::);
                }
                const val = framePtr[pc[1].arg];
                framePtr[pc[2].arg] = evalNot(val);
                pc += 3;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .mapEmpty => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMapEmpty:"::);
                }
                const dst = pc[1].arg;
                pc += 2;
                framePtr[dst] = try cy.heap.allocEmptyMap(vm);
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .copy => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCopy:"::);
                }
                framePtr[pc[2].arg] = framePtr[pc[1].arg];
                pc += 3;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .retain => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRetain:"::);
                }
                retain(vm, framePtr[pc[1].arg]);
                pc += 2;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .jump => {
                if (GenLabels) {
                    _ = asm volatile ("LOpJump:"::);
                }
                @setRuntimeSafety(false);
                pc += @intCast(usize, @ptrCast(*const align(1) i16, &pc[1]).*);
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    recv.asHeapObject().common.structId
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
                    if (useGoto) { gotoNext(pc, jumpTablePtr); }
                    continue;
                }

                // Deoptimize.
                pc[0] = cy.OpData{ .code = .callObjSym };
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .callObjNativeFuncIC => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCallObjNativeFuncIC:"::);
                }
                const startLocal = pc[1].arg;
                const numArgs = pc[2].arg;
                const recv = framePtr[startLocal + numArgs + 4 - 1];
                const typeId = if (recv.isPointer()) 
                    recv.asHeapObject().common.structId
                else recv.getPrimitiveTypeId();

                const cachedStruct = @ptrCast(*align (1) u16, pc + 12).*;
                if (typeId == cachedStruct) {
                    // const newFramePtr = framePtr + startLocal;
                    vm.framePtr = framePtr;
                    const func = @intToPtr(cy.NativeObjFuncPtr, @intCast(usize, @ptrCast(*align (1) u48, pc + 6).*));
                    const res = func(@ptrCast(*UserVM, vm), recv, @ptrCast([*]const Value, framePtr + startLocal + 4), numArgs);
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
                    if (useGoto) { gotoNext(pc, jumpTablePtr); }
                    continue;
                }

                // Deoptimize.
                pc[0] = cy.OpData{ .code = .callObjSym };
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                const func = @intToPtr(cy.NativeFuncPtr, @intCast(usize, @ptrCast(*align (1) u48, pc + 5).*));
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .ret1 => {
                if (GenLabels) {
                    _ = asm volatile ("LOpRet1:"::);
                }
                if (@call(.always_inline, popStackFrameLocal1, .{vm, &pc, &framePtr})) {
                    if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    const obj = recv.asHeapObject();
                    if (obj.common.structId == @ptrCast(*align (1) u16, pc + 4).*) {
                        const lastValue = obj.object.getValuePtr(pc[6].arg);
                        release(vm, lastValue.*);
                        lastValue.* = framePtr[pc[2].arg];
                        pc += 7;
                        if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    if (obj.common.structId == @ptrCast(*align (1) u16, pc + 4).*) {
                        framePtr[dst] = obj.object.getValue(pc[6].arg);
                        retain(vm, framePtr[dst]);
                        pc += 7;
                        if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    const obj = recv.asHeapObject();
                    // const offset = @call(.never_inline, gvm.getFieldOffset, .{obj, symId });
                    const offset = gvm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        framePtr[dst] = obj.object.getValue(offset);
                        // Inline cache.
                        pc[0] = cy.OpData{ .code = .fieldIC };
                        @ptrCast(*align (1) u16, pc + 4).* = @intCast(u16, obj.common.structId);
                        pc[6] = cy.OpData{ .arg = offset };
                    } else {
                        framePtr[dst] = @call(.never_inline, gvm.getFieldFallback, .{obj, gvm.fieldSyms.buf[symId].name});
                    }
                    pc += 7;
                    if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    const obj = recv.asHeapObject();
                    // const offset = @call(.never_inline, gvm.getFieldOffset, .{obj, symId });
                    const offset = vm.getFieldOffset(obj, symId);
                    if (offset != cy.NullU8) {
                        framePtr[dst] = obj.object.getValue(offset);
                        // Inline cache.
                        pc[0] = cy.OpData{ .code = .fieldRetainIC };
                        @ptrCast(*align (1) u16, pc + 4).* = @intCast(u16, obj.common.structId);
                        pc[6] = cy.OpData { .arg = offset };
                    } else {
                        framePtr[dst] = @call(.never_inline, vm.getFieldFallback, .{obj, vm.fieldSyms.buf[symId].name});
                    }
                    retain(vm, framePtr[dst]);
                    pc += 7;
                    if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                        pc[0] = cy.OpData{ .code = .setFieldReleaseIC };
                        @ptrCast(*align (1) u16, pc + 4).* = @intCast(u16, obj.common.structId);
                        pc[6] = cy.OpData { .arg = offset };
                        pc += 7;
                        if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                const rFuncSigId = @ptrCast(*const align(1) u16, pc + 4).*;
                const dst = pc[6].arg;
                pc += 7;
                framePtr[dst] = try @call(.never_inline, cy.heap.allocLambda, .{vm, funcPc, numParams, numLocals, rFuncSigId });
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                const rFuncSigId = @ptrCast(*const align(1) u16, pc + 5).*;
                const dst = pc[7].arg;
                const capturedVals = pc[8..8+numCaptured];
                pc += 8 + numCaptured;

                framePtr[dst] = try @call(.never_inline, cy.heap.allocClosure, .{vm, framePtr, funcPc, numParams, numLocals, rFuncSigId, capturedVals});
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .staticFunc => {
                if (GenLabels) {
                    _ = asm volatile ("LOpStaticFunc:"::);
                }
                const symId = pc[1].arg;
                framePtr[pc[2].arg] = try cy.heap.allocFuncFromSym(vm, symId);
                pc += 3;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .coresume => {
                if (GenLabels) {
                    _ = asm volatile ("LOpCoresume:"::);
                }
                const fiber = framePtr[pc[1].arg];
                if (fiber.isPointer()) {
                    const obj = fiber.asHeapObject();
                    if (obj.common.structId == cy.FiberS) {
                        if (&obj.fiber != vm.curFiber) {
                            // Only resume fiber if it's not done.
                            if (obj.fiber.pc != cy.NullId) {
                                const res = cy.fiber.pushFiber(vm, pcOffset(vm, pc + 3), framePtr, &obj.fiber, pc[2].arg);
                                pc = res.pc;
                                framePtr = res.framePtr;
                                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                                continue;
                            }
                        }
                    }
                    cy.arc.releaseObject(vm, obj);
                }
                pc += 3;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    framePtr[pc[3].arg] = try @call(.never_inline, evalMultiplyFallback, .{ vm, left, right });
                }
                pc += 4;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    framePtr[pc[3].arg] = try @call(.never_inline, evalDivideFallback, .{ vm, left, right });
                }
                pc += 4;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    framePtr[pc[3].arg] = try @call(.never_inline, evalModFallback, .{ vm, left, right });
                }
                pc += 4;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .pow => {
                if (GenLabels) {
                    _ = asm volatile ("LOpPow:"::);
                }
                const left = framePtr[pc[1].arg];
                const right = framePtr[pc[2].arg];
                framePtr[pc[3].arg] = try @call(.never_inline, evalPower, .{ vm, left, right });
                pc += 4;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                const obj = box.asHeapObject();
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.common.structId == cy.BoxS);
                }
                obj.box.val = rval;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                const obj = box.asHeapObject();
                if (builtin.mode == .Debug) {
                    std.debug.assert(obj.common.structId == cy.BoxS);
                }
                @call(.never_inline, release, .{vm, obj.box.val});
                obj.box.val = rval;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .boxValue => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBoxValue:"::);
                }
                const box = framePtr[pc[1].arg];
                if (box.isPointer()) {
                    const obj = box.asHeapObject();
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .tagLiteral => {
                if (GenLabels) {
                    _ = asm volatile ("LOpTagLiteral:"::);
                }
                const symId = pc[1].arg;
                framePtr[pc[2].arg] = Value.initTagLiteral(symId);
                pc += 3;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                    if (useGoto) { gotoNext(pc, jumpTablePtr); }
                    continue;
                } else {
                    if (framePtr != vm.stack.ptr) {
                        framePtr[0] = val;
                        pc += @ptrCast(*const align(1) u16, pc + 3).*;
                        if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .bitwiseNot => {
                if (GenLabels) {
                    _ = asm volatile ("LOpBitwiseNot:"::);
                }
                const val = framePtr[pc[1].arg];
                framePtr[pc[2].arg] = @call(.never_inline, evalBitwiseNot, .{val});
                pc += 3;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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

                const recv = framePtr[startLocal + numArgs + 4 - 1];
                const typeId = recv.getTypeId();

                if (vm.getCallObjSym(typeId, symId)) |sym| {
                    const res = try @call(.never_inline, vm.callSymEntry, .{pc, framePtr, sym, recv, typeId, startLocal, numArgs, numRet });
                    pc = res.pc;
                    framePtr = res.framePtr;
                } else {
                    const res = try @call(.never_inline, callObjSymFallback, .{vm, pc, framePtr, recv, typeId, symId, startLocal, numArgs, numRet});
                    pc = res.pc;
                    framePtr = res.framePtr;
                }

                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .match => {
                if (GenLabels) {
                    _ = asm volatile ("LOpMatch:"::);
                }
                pc += @call(.never_inline, opMatch, .{vm, pc, framePtr});
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
                continue;
            },
            .setStaticFunc => {
                if (GenLabels) {
                    _ = asm volatile ("LOpSetStaticFunc:"::);
                }
                const symId = pc[1].arg;
                try @call(.never_inline, setStaticFunc, .{vm, symId, framePtr[pc[2].arg]});
                pc += 3;
                if (useGoto) { gotoNext(pc, jumpTablePtr); }
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

fn dumpEvalOp(vm: *const VM, pc: [*]const cy.OpData) !void {
    if (builtin.is_test and !debug.atLeastTestDebugLevel()) {
        return;
    }
    var buf: [1024]u8 = undefined;
    var extra: []const u8 = "";

    const offset = pcOffset(vm, pc);
    switch (pc[0].code) {
        .constOp => {
            const idx = pc[1].arg;
            const dst = pc[2].arg;
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
    OutOfBounds,
    StackOverflow,
    NoDebugSym,
};

const ObjectSymKey = struct {
    structId: TypeId,
    symId: SymbolId,
};

/// See `reserveFuncParams` for stack layout.
/// numArgs does not include the callee.
pub fn call(vm: *VM, pc: *[*]cy.OpData, framePtr: *[*]Value, callee: Value, startLocal: u8, numArgs: u8, retInfo: Value) !void {
    if (callee.isPointer()) {
        const obj = callee.asHeapObject();
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
        const obj = callee.asHeapObject();
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

fn getObjectFunctionFallback(vm: *VM, pc: [*]cy.OpData, recv: Value, typeId: u32, rtSymId: SymbolId) !Value {
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
    const relPc = pcOffset(vm, pc);
    if (debug.getDebugSym(vm, relPc)) |sym| {
        const chunk = vm.compiler.chunks.items[sym.file];
        const node = chunk.nodes[sym.loc];
        if (node.node_t == .callExpr) {
            const numParams = node.head.callExpr.getNumArgs(chunk.nodes);
            const name = vm.methodSymExtras.buf[rtSymId].getName();
            return vm.panicFmt("`{}` is either missing in `{}` or the call signature: {}(self, {} args) is unsupported.", &.{
                 v(name), v(vm.structs.buf[typeId].name), v(name), v(numParams),
            });
        } else {
            // Debug node is from:
            // `for [iterable]:`
            const name = vm.methodSymExtras.buf[rtSymId].getName();
            return vm.panicFmt("`{}` is either missing in `{}` or the call signature: {}(self, 0 args) is unsupported.", &.{
                 v(name), v(vm.structs.buf[typeId].name), v(name),
            });
        }
    } else {
        return vm.panicFmt("Missing debug sym at {}", &.{v(pcOffset(vm, pc))});
    }
}

/// Use new pc local to avoid deoptimization.
fn callObjSymFallback(vm: *VM, pc: [*]cy.OpData, framePtr: [*]Value, recv: Value, typeId: u32, symId: SymbolId, startLocal: u8, numArgs: u8, reqNumRetVals: u8) linksection(cy.Section) !PcFramePtr {
    @setCold(true);
    // const func = try @call(.never_inline, getObjectFunctionFallback, .{obj, symId});
    const func = try getObjectFunctionFallback(vm, pc, recv, typeId, symId);

    retain(vm, func);
    cy.arc.releaseObject(vm, recv.asHeapObject());

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
            if (@ptrToInt(framePtr + startLocal + sym.inner.func.numLocals) >= @ptrToInt(vm.stack.ptr) + 8 * vm.stack.len) {
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

pub inline fn framePtrOffset(vm: *const VM, framePtr: [*]const Value) usize {
    // Divide by eight.
    return (@ptrToInt(framePtr) - @ptrToInt(vm.stack.ptr)) >> 3;
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
        const obj = box.asHeapObject();
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

/// Like Value.dump but shows heap values.
pub fn dumpValue(vm: *const VM, val: Value) void {
    if (val.isNumber()) {
        fmt.printStdout("Number {}\n", &.{ v(val.asF64()) });
    } else {
        if (val.isPointer()) {
            const obj = val.asHeapObject();
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
const RtFuncSymKey = KeyU96;
const RtVarSymKey = KeyU64;

pub const KeyU96 = extern union {
    val: extern struct {
        a: u64,
        b: u32,
    },
    absLocalSymKey: extern struct {
        parentSymId: sema.ResolvedSymId,
        nameId: sema.NameSymId,
        numParams: u32,
    },
    rtFuncSymKey: extern struct {
        // TODO: Is it enough to just use the final resolved func sym id?
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

pub const KeyU128 = extern union {
    val: extern struct {
        left: u64,
        right: u64,
    },
    absLocalSymKey: extern struct {
        parentSymId: u32,
        nameId: u32,
        funcSigId: u32, 
        dummy: u32 = 0, 
    },
};

pub const KeyU128Context = struct {
    pub fn hash(_: @This(), key: KeyU128) u64 {
        var hasher = std.hash.Wyhash.init(0);
        @call(.always_inline, hasher.update, .{std.mem.asBytes(&key.val)});
        return hasher.final();
    }
    pub fn eql(_: @This(), a: KeyU128, b: KeyU128) bool {
        return a.val.left == b.val.left and a.val.right == b.val.right;
    }
};

pub const KeyU64 = extern union {
    val: u64,
    absResolvedSymKey: extern struct {
        rParentSymId: u32,
        nameId: u32,
    },
    absResolvedFuncSymKey: extern struct {
        rSymId: sema.ResolvedSymId,
        rFuncSigId: sema.ResolvedFuncSigId,
    },
    relModuleSymKey: extern struct {
        nameId: sema.NameSymId,
        rFuncSigId: sema.ResolvedFuncSigId,
    },
    rtVarSymKey: extern struct {
        rParentSymId: sema.ResolvedSymId,
        nameId: sema.NameSymId,
    },
    relFuncSigKey: extern struct {
        nameId: sema.NameSymId,
        numParams: u32,
    },
    structKey: extern struct {
        rParentSymId: sema.ResolvedSymId,
        nameId: sema.NameSymId,
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

    enableFileModules: bool = false,

    /// Whether url imports and cached assets should be reloaded.
    reload: bool = false,

    /// By default, debug syms are only generated for insts that can potentially fail.
    genAllDebugSyms: bool = false,
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

fn setStaticFunc(vm: *VM, symId: SymbolId, val: Value) linksection(cy.Section) !void {
    errdefer {
        // TODO: This should be taken care of by panic stack unwinding.
        cy.arc.release(vm, val);
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        switch (obj.common.structId) {
            cy.NativeFunc1S => {
                const dstRFuncSigId = getResolvedFuncSigIdOfSym(vm, symId);
                if (dstRFuncSigId != obj.nativeFunc1.rFuncSigId) {
                    return vm.panic("Assigning to static function with a different function signature.");
                }
                releaseFuncSymDep(vm, symId);

                const rFuncSig = vm.compiler.semaResolvedFuncSigs.items[dstRFuncSigId];
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
            cy.LambdaS => {
                const dstRFuncSigId = getResolvedFuncSigIdOfSym(vm, symId);
                if (dstRFuncSigId != obj.lambda.rFuncSigId) {
                    vm.u8Buf.clearRetainingCapacity();
                    const w = vm.u8Buf.writer(vm.alloc);
                    cy.sema.writeResolvedFuncSigStr(&vm.compiler, w, dstRFuncSigId) catch fatal();
                    const dstSig = vm.u8Buf.items();
                    const start = vm.u8Buf.len;
                    cy.sema.writeResolvedFuncSigStr(&vm.compiler, w, @intCast(u32, obj.lambda.rFuncSigId)) catch fatal();
                    const srcSig = vm.u8Buf.buf[start..vm.u8Buf.len];
                    return vm.panicFmt("Assigning to static function `sig: {}` with a different function signature `sig: {}`.", &.{v(dstSig), v(srcSig)});
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
            cy.ClosureS => {
                const dstRFuncSigId = getResolvedFuncSigIdOfSym(vm, symId);
                if (dstRFuncSigId != obj.closure.rFuncSigId) {
                    return vm.panic("Assigning to static function with a different function signature.");
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
                return vm.panicFmt("Assigning to static function with unsupported type {}.", &.{v(obj.common.structId)});
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

/// Unwind given stack starting at a pc, framePtr and release all locals.
/// TODO: See if fiber.releaseFiberStack can resuse the same code.
fn unwindReleaseStack(vm: *cy.VM, stack: []const Value, startFramePtr: [*]const Value, startPc: [*]const cy.OpData) void {
    var pc = pcOffset(vm, startPc);
    var framePtr = framePtrOffset(vm, startFramePtr);

    // Top of the frame indexes directly into the debug table.
    log.debug("release frame at {}", .{pc});
    var endLocalsPc = debug.pcToEndLocalsPc(vm, pc);
    if (endLocalsPc != cy.NullId) {
        cy.arc.runReleaseOps(vm, stack, framePtr, endLocalsPc);
    }
    if (framePtr == 0) {
        return;
    } else {
        pc = pcOffset(vm, stack[framePtr + 2].retPcPtr);
        framePtr = (@ptrToInt(stack[framePtr + 3].retFramePtr) - @ptrToInt(stack.ptr)) >> 3;
    }

    while (true) {
        log.debug("release frame at {}", .{pc});
        const sym = debug.getDebugSymBefore(vm, pc);
        endLocalsPc = debug.debugSymToEndLocalsPc(vm, sym);
        if (endLocalsPc != cy.NullId) {
            cy.arc.runReleaseOps(vm, stack, framePtr, endLocalsPc);
        }
        if (framePtr == 0) {
            // Done, at main block.
            return;
        } else {
            // Unwind.
            pc = pcOffset(vm, stack[framePtr + 2].retPcPtr);
            framePtr = (@ptrToInt(stack[framePtr + 3].retFramePtr) - @ptrToInt(stack.ptr)) >> 3;
        }
    }
}

pub const ValidateResult = struct {
    err: ?cy.CompileErrorType,
};