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
const cc = @import("capi.zig");
const bt = cy.types.BuiltinTypes;
const rt = cy.rt;
const sema = cy.sema;
const types = cy.types;
const builtins = @import("builtins/builtins.zig");
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

const logger = cy.log.scoped(.vm);

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
    consts: []const Value,

    /// Holds unique heap string interns (*Astring, *Ustring).
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
    /// If the `MethodEntry.mru_type` matches, `mru_id` stored in MethodEntry is used first.
    /// Otherwise, `type_method_map` is queried.
    /// `TypeMethod`s are not created until at least 2 types share the same method name.
    methods: cy.List(rt.Method),
    method_map: std.AutoHashMapUnmanaged(rt.MethodKey, vmc.MethodId),
    type_methods: cy.List(rt.TypeMethod),
    type_method_map: std.HashMapUnmanaged(rt.TypeMethodKey, vmc.TypeMethodId, cy.hash.KeyU64Context, 80),

    /// Regular function symbol table.
    /// TODO: The only reason this exists right now is for FFI to dynamically set binded functions to empty static functions.
    ///       Once func types, and var func pointers are done this can be removed.
    funcSyms: cy.List(rt.FuncSymbol),
    funcSymDetails: cy.List(rt.FuncSymDetail),

    /// Contiguous func ids. The first element in a segment contains the num of overloaded funcs for the func sym.
    overloaded_funcs: cy.List(u32),

    /// Static vars. 
    varSyms: cy.List(rt.VarSym),

    /// Struct fields symbol table.
    fieldSyms: cy.List(rt.FieldSymbolMap),
    fieldTable: std.HashMapUnmanaged(rt.FieldTableKey, FieldEntry, cy.hash.KeyU64Context, 80),
    fieldSymSignatures: std.StringHashMapUnmanaged(rt.FieldId),

    /// Types.
    types: []const types.Type,

    /// Stores insts that were modified to restore them later.
    inlineSaves: std.AutoHashMapUnmanaged(u32, [*]const u8),

    /// Symbols.
    syms: cy.List(Symbol),
    symSignatures: std.StringHashMapUnmanaged(SymbolId),

    names: cy.List(vmc.Name),
    nameMap: std.StringHashMapUnmanaged(vmc.NameId),

    /// Static heap values are retained until the end of execution.
    /// Retains each value by +1.
    staticObjects: cy.List(*cy.heap.HeapObject),

    u8Buf: cy.ListAligned(u8, 8),

    stackTrace: cy.StackTrace,

    /// This is needed for reporting since a method entry can be empty.
    method_names: cy.List(vmc.NameId),
    debugTable: []const cy.DebugSym,
    debugTempIndexTable: []const u32,
    unwindTempRegs: []const u8,
    unwindTempPrevIndexes: []const u32,

    curFiber: *cy.Fiber,
    mainFiber: cy.Fiber,

    /// Records a minimal trace when walking the stack.
    /// Stack frames are then constructed from them.
    compactTrace: cy.List(vmc.CompactFrame),

    trace: if (cy.Trace) *vmc.TraceInfo else void,

    compiler: *cy.Compiler,
    sema: *cy.sema.Sema,

    /// User data ptr. Useful for embedders.
    userData: ?*anyopaque,

    /// Host write hook.
    print: cc.PrintFn,
    print_err: cc.PrintErrorFn,

    /// Object to pc of instruction that allocated it.
    objectTraceMap: if (cy.Trace) std.AutoHashMapUnmanaged(*HeapObject, debug.ObjectTrace) else void,

    /// In debug mode, always save the current pc so tracing can be obtained.
    /// debugPc == NullId indicates execution has not started.
    debugPc: if (cy.Trace) u32 else void,

    /// Interface used for imports and fetch.
    httpClient: http.HttpClient,
    stdHttpClient: if (cy.hasCLI) *http.StdHttpClient else *anyopaque,

    emptyString: Value,
    emptyArray: Value,

    varSymExtras: cy.List(*cy.Sym),

    /// Local to be returned back to eval caller.
    /// 255 indicates no return value.
    endLocal: usize,

    config: cc.EvalConfig,

    countFrees: if (cy.Trace) bool else void,
    numFreed: if (cy.Trace) u32 else void,

    /// Whether this VM is already deinited. Used to skip the next deinit to avoid using undefined memory.
    deinited: bool,
    deinitedRtObjects: bool, 

    tempBuf: [128]u8 align(4),

    lastExeError: []const u8,
    last_res: cc.ResultCode,

    pub fn init(self: *VM, alloc: std.mem.Allocator) !void {
        self.* = .{
            .alloc = alloc,
            .compiler = undefined,
            .sema = undefined,
            .ops = undefined,
            .consts = undefined,
            .emptyString = undefined,
            .emptyArray = undefined,
            .strInterns = .{},
            .staticObjects = .{},
            .stack = &.{},
            .stackEndPtr = undefined,
            .names = .{},
            .nameMap = .{},
            .heapPages = .{},
            .heapFreeHead = null,
            .heapFreeTail = if (cy.Trace) null else undefined,
            .cyclableHead = if (cy.hasGC) @ptrCast(&dummyCyclableHead) else {},
            .pc = undefined,
            .framePtr = undefined,
            .tryStack = .{},
            .methods = .{},
            .method_map = .{},
            .method_names = .{},
            .type_methods = .{},
            .type_method_map = .{},
            .funcSyms = .{},
            .funcSymDetails = .{},
            .overloaded_funcs = .{},
            .varSyms = .{},
            .fieldSyms = .{},
            .fieldTable = .{},
            .fieldSymSignatures = .{},
            .types = &.{},
            .syms = .{},
            .symSignatures = .{},
            .inlineSaves = .{},
            .trace = undefined,
            .u8Buf = .{},
            .stackTrace = .{},
            .debugTable = undefined,
            .debugTempIndexTable = undefined,
            .unwindTempRegs = undefined,
            .unwindTempPrevIndexes = undefined,
            .refCounts = if (cy.TrackGlobalRC) 0 else undefined,
            .compactTrace = .{},
            .mainFiber = undefined,
            .curFiber = undefined,
            .endLocal = undefined,
            .objectTraceMap = if (cy.Trace) .{} else undefined,
            // Initialize to NullId to indicate vm is still in initing.
            .debugPc = if (cy.Trace) cy.NullId else undefined,
            .deinited = false,
            .deinitedRtObjects = false,
            .config = undefined,
            .httpClient = undefined,
            .stdHttpClient = undefined,
            .userData = null,
            .varSymExtras = .{},
            .print = defaultPrint,
            .print_err = defaultPrintError,
            .countFrees = if (cy.Trace) false else {},
            .numFreed = if (cy.Trace) 0 else {},
            .tempBuf = undefined,
            .lastExeError = "",
            .last_res = cc.Success,
        };
        self.mainFiber.panicType = vmc.PANIC_NONE;
        self.curFiber = &self.mainFiber;
        self.compiler = try self.alloc.create(cy.Compiler);
        self.sema = &self.compiler.sema;
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
        try self.methods.ensureTotalCapacityPrecise(self.alloc, 340);
        try self.method_map.ensureTotalCapacity(self.alloc, 200);
        std.debug.assert(cy.utils.getHashMapMemSize(rt.MethodKey, vmc.MethodId, self.method_map.capacity()) < 4096);

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

        logger.tracev("release varSyms", .{});
        for (self.varSyms.items(), 0..) |vsym, i| {
            logger.tracevIf(cy.logMemory, "release varSym: {s}", .{self.varSymExtras.buf[i].name()});
            release(self, vsym.value);
        }
        self.varSyms.clearRetainingCapacity();

        // Release static strings.
        logger.tracev("release static objects {}", .{self.staticObjects.len});
        for (self.staticObjects.items()) |obj| {
            logger.tracevIf(cy.logMemory, "release static object", .{});
            cy.arc.releaseObject(self, obj);
        }
        logger.tracev("release static objects end", .{});

        // No need to release `emptyString` since it is owned by `staticObjects`.

        self.deinitedRtObjects = true;
    }

    pub fn deinit(self: *VM, reset: bool) void {
        if (self.deinited) {
            return;
        }

        cy.fiber.freeFiberPanic(self, &self.mainFiber);

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
            self.compactTrace.clearRetainingCapacity();
        } else {
            self.tryStack.deinit(self.alloc);
            self.compactTrace.deinit(self.alloc);
        }

        if (reset) {
            self.methods.clearRetainingCapacity();
            self.method_map.clearRetainingCapacity();
            self.method_names.clearRetainingCapacity();
            self.type_methods.clearRetainingCapacity();
            self.type_method_map.clearRetainingCapacity();
        } else {
            self.methods.deinit(self.alloc);
            self.method_map.deinit(self.alloc);
            self.method_names.deinit(self.alloc);
            self.type_methods.deinit(self.alloc);
            self.type_method_map.deinit(self.alloc);
        }

        if (reset) {
            self.funcSyms.clearRetainingCapacity();
            self.funcSymDetails.clearRetainingCapacity();
            self.overloaded_funcs.clearRetainingCapacity();
        } else {
            self.funcSyms.deinit(self.alloc);
            self.funcSymDetails.deinit(self.alloc);
            self.overloaded_funcs.deinit(self.alloc);
        }

        if (reset) {
            self.varSyms.clearRetainingCapacity();
        } else {
            self.varSyms.deinit(self.alloc);
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

        self.types = &.{};

        for (self.syms.items()) |sym| {
            if (sym.nameOwned) {
                self.alloc.free(sym.name);
            }
        }
        if (reset) {
            self.syms.clearRetainingCapacity();
            self.symSignatures.clearRetainingCapacity();
        } else {
            self.syms.deinit(self.alloc);
            self.symSignatures.deinit(self.alloc);
        }

        self.stackTrace.deinit(self.alloc);
        if (reset) {
            self.u8Buf.clearRetainingCapacity();
            self.strInterns.clearRetainingCapacity();
        } else {
            self.u8Buf.deinit(self.alloc);
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

        var iter = self.inlineSaves.valueIterator();
        while (iter.next()) |it| {
            switch (@as(cy.bindings.QuickenType, @enumFromInt(it.*[0]))) {
                // .binOp => {
                //     self.alloc.free(it.*[0..vmc.CALL_OBJ_SYM_INST_LEN + 1]);
                // },
            }
        }
        if (reset) {
            self.inlineSaves.clearRetainingCapacity();
        } else {
            self.inlineSaves.deinit(self.alloc);
        }

        if (reset) {
            self.varSymExtras.clearRetainingCapacity();
            self.names.clearRetainingCapacity();
            self.nameMap.clearRetainingCapacity();
            self.staticObjects.clearRetainingCapacity();
        } else {
            self.varSymExtras.deinit(self.alloc);
            self.names.deinit(self.alloc);
            self.nameMap.deinit(self.alloc);
            self.staticObjects.deinit(self.alloc);
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

        self.alloc.free(self.lastExeError);
        self.lastExeError = "";
    }

    pub fn validate(self: *VM, srcUri: []const u8, src: []const u8, config: cc.ValidateConfig) !void {
        var compile_c = cc.defaultCompileConfig();
        compile_c.file_modules = config.file_modules;
        compile_c.skip_codegen = true;
        _ = try self.compile(srcUri, src, compile_c);
    }

    pub fn compile(self: *VM, srcUri: []const u8, src: []const u8, config: cc.CompileConfig) !cy.CompileResult {
        try self.resetVM();
        self.config = cc.defaultEvalConfig();
        self.config.single_run = config.single_run;
        self.config.file_modules = config.file_modules;
        self.config.gen_all_debug_syms = true;

        var tt = cy.debug.timer();
        const res = try self.compiler.compile(srcUri, src, config);
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
    }

    pub fn eval(self: *VM, srcUri: []const u8, src: []const u8, config: cc.EvalConfig) !Value {
        try self.resetVM();
        self.config = config;
        var tt = cy.debug.timer();

        var compile_c = cc.defaultCompileConfig();
        compile_c.single_run = config.single_run;
        compile_c.file_modules = config.file_modules;
        compile_c.gen_all_debug_syms = cy.Trace;
        compile_c.backend = config.backend;
        const res = try self.compiler.compile(srcUri, src, compile_c);
        tt.endPrint("compile");

        if (config.backend == cc.BackendJIT) {
            if (cy.isFreestanding or cy.isWasm) {
                return error.Unsupported;
            }

            const jitRes = res.jit.buf;
            try cy.fiber.stackEnsureTotalCapacity(self, res.jit.mainStackSize);
            self.framePtr = @ptrCast(self.stack.ptr);

            // Mark code executable.
            const PROT_READ = 1;
            const PROT_WRITE = 2;
            const PROT_EXEC = 4;
            try std.os.mprotect(jitRes.buf.items.ptr[0..jitRes.buf.capacity], PROT_READ | PROT_EXEC);
            
            // Memory must be reset to original setting in order to be freed.
            defer std.os.mprotect(jitRes.buf.items.ptr[0..jitRes.buf.capacity], PROT_WRITE) catch cy.fatal();

            if (jitRes.buf.items.len > 500*4) {
                logger.tracev("jit code (size: {}) {}...", .{jitRes.buf.items.len, std.fmt.fmtSliceHexLower(jitRes.buf.items[0..100*4])});
            } else {
                logger.tracev("jit code (size: {}) {}", .{jitRes.buf.items.len, std.fmt.fmtSliceHexLower(jitRes.buf.items)});
            }

            const bytes = jitRes.buf.items[jitRes.mainPc..jitRes.mainPc+12*4];
            logger.tracev("main start {}: {}", .{jitRes.mainPc, std.fmt.fmtSliceHexLower(bytes)});

            const main: *const fn(*VM, [*]Value) callconv(.C) void = @ptrCast(@alignCast(jitRes.buf.items.ptr + jitRes.mainPc));
            // @breakpoint();
            main(self, self.framePtr);
            return Value.initInt(0);
        } else if (config.backend == cc.BackendVM) {
            if (cc.verbose()) {
                try debug.dumpBytecode(self, .{});
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
            defer tt.endPrint("eval");

            return self.evalByteCode(res.vm);
        } else {
            defer res.aot.deinit(self.alloc);
            if (cy.isFreestanding or cy.isWasm) {
                return error.Unsupported;
            }

            if (self.config.spawn_exe) {
                const term = try spawn(.{
                    .allocator = self.alloc,
                    .argv = &.{res.aot.exePath},
                });
                if (term != .Exited or term.Exited != 0) {
                    self.alloc.free(self.lastExeError);
                    self.lastExeError = "";
                    return error.Panic;
                }
            } else {
                const exeRes = try std.ChildProcess.exec(.{
                    .allocator = self.alloc,
                    .argv = &.{res.aot.exePath},
                });
                self.alloc.free(exeRes.stdout);
                if (exeRes.term != .Exited or exeRes.term.Exited != 0) {
                    self.alloc.free(self.lastExeError);
                    self.lastExeError = exeRes.stderr;
                    return error.Panic;
                }
            }
            return Value.initInt(0);
        }
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

        while (i < vmc.NumCodes) : (i += 1) {
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
            for (self.funcSyms.items(), 0..) |_, i| {
                const details = self.funcSymDetails.buf[i];
                const name = details.namePtr[0..details.nameLen];
                const sigStr = try self.sema.formatFuncSig(details.funcSigId, &cy.tempBuf);
                fmt.printStderr("\t{}{}: {}\n", &.{v(name), v(sigStr), v(i)});
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

    pub fn getFiberContext(vm: *const cy.VM) cy.fiber.PcSpOff {
        return .{
            .pc = cy.fiber.getInstOffset(vm.ops.ptr, vm.pc),
            .sp = cy.fiber.getStackOffset(vm.stack.ptr, vm.framePtr),
        };
    }

    pub fn getTypeName(vm: *const cy.VM, typeId: cy.TypeId) []const u8 {
        if (typeId == cy.NullId >> 3) {
            return "danglingObject";
        }
        return vm.compiler.sema.types.items[typeId].sym.name();
    }

    pub fn popStackFrameCold(self: *VM, comptime numRetVals: u2) void {
        _ = self;
        @setRuntimeSafety(debug);
        switch (numRetVals) {
            2 => {
                logger.err("unsupported", .{});
            },
            3 => {
                // unreachable;
            },
            else => @compileError("Unsupported num return values."),
        }
    }

    fn popStackFrameLocal(self: *VM, pc: *usize, retLocal: u8, comptime numRetVals: u2) bool {
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

    pub fn fillUndefinedStackSpace(self: *VM, val: Value) void {
        @memset(self.stack, val);
    }

    pub fn evalByteCode(self: *VM, buf: cy.ByteCodeBuffer) !Value {
        if (buf.ops.items.len == 0) {
            return error.NoEndOp;
        }

        cy.fiber.freeFiberPanic(self, &self.mainFiber);
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
        self.types = self.compiler.sema.types.items;

        try @call(.never_inline, evalLoopGrowStack, .{self, true});
        logger.tracev("main stack size: {}", .{buf.mainStackSize});

        if (self.endLocal == 255) {
            return Value.initInt(0);
        } else {
            return self.stack[self.endLocal];
        }
    }

    /// Assumes `vm.pc` is at a call inst before entering host code.
    fn getNewCallFuncRet(vm: *VM) u8 {
        const fp = cy.fiber.getStackOffset(vm.stack.ptr, vm.framePtr);
        if (fp == 0 or cy.fiber.isVmFrame(vm, vm.stack, fp)) {
            // Vm frame. Obtain current stack top from looking at the current inst.
            switch (vm.pc[0].opcode()) {
                .callSym,
                .callNativeFuncIC,
                .call,
                .callObjNativeFuncIC,
                .callObjSym => {
                    const ret = vm.pc[1];
                    const numArgs = vm.pc[2];
                    return ret.val + CallArgStart + numArgs.val;
                },
                .fieldDyn => {
                    return vm.pc[8].val;
                },
                .setFieldDyn => {
                    return vm.pc[10].val;
                },
                else => {
                    cy.panicFmt("TODO: getNewCallFuncFp {}", .{vm.pc[0].opcode()});
                }
            }
        } else {
            // Host frame.
            cy.panic("TODO getNewCallFuncFp");
        }
    }

    const CallFuncConfig = struct {
        from_external: bool = true,
    };

    pub fn callFunc(vm: *VM, func: Value, args: []const Value, config: CallFuncConfig) !Value {
        const fpOff = cy.fiber.getStackOffset(vm.stack.ptr, vm.framePtr);
        const ret = vm.getNewCallFuncRet();

        const pc = vm.pc;

        // Since the compiler isn't aware of a re-entry call, it can not predetermine the stack size required
        // for the arguments.
        try cy.fiber.ensureTotalStackCapacity(vm, fpOff + ret + CallArgStart + args.len);
        // Should the stack grow, update pointer.
        vm.framePtr = vm.stack.ptr + fpOff;

        var call_ret: u8 = undefined;
        if (config.from_external) {
            // Create a new host frame so that it can be reported.
            const retInfo = buildReturnInfoForRoot(false);
            const retFramePtr = Value{ .retFramePtr = vm.framePtr };
            vm.framePtr += ret;

            // Reserve extra slot for info about the call.
            vm.framePtr[0].val = func.getTypeId();
            vm.framePtr += 1;

            vm.framePtr[1] = retInfo;
            // Return pc points to the call inst that entered host.
            vm.framePtr[2] = Value{ .retPcPtr = pc };
            vm.framePtr[3] = retFramePtr;
            call_ret = 4;
        } else {
            call_ret = ret;
        }

        // Copy callee + args to a new blank frame.
        vm.framePtr[call_ret + CalleeStart] = func;
        @memcpy(vm.framePtr[call_ret+CallArgStart..call_ret+CallArgStart+args.len], args);

        // Pass in an arbitrary `pc` to reuse the same `call` used by the VM.
        const pcsp = try call(vm, vm.pc, vm.framePtr, func, call_ret, @intCast(args.len), false);
        vm.framePtr = pcsp.sp;
        vm.pc = pcsp.pc;

        // Only user funcs start eval loop.
        if (!func.isObjectType(bt.HostFunc)) {
            if (config.from_external) {
                @call(.never_inline, evalLoopGrowStack, .{vm, true}) catch |err| {
                    if (err == error.Panic) {
                        // Dump for now.
                        const frames = try cy.debug.allocStackTrace(vm, vm.stack, vm.compactTrace.items());
                        defer vm.alloc.free(frames);

                        const w = cy.fmt.lockStderrWriter();
                        defer cy.fmt.unlockPrint();
                        const msg = try cy.debug.allocPanicMsg(vm);
                        defer vm.alloc.free(msg);
                        try fmt.format(w, "{}\n\n", &.{v(msg)});
                        try cy.debug.writeStackFrames(vm, w, frames);
                    }
                    logger.tracev("{}", .{err});
                    return error.Panic;
                    // return builtins.prepThrowZError(@ptrCast(vm), err, @errorReturnTrace());
                };
            } else {
                @call(.never_inline, evalLoopGrowStack, .{vm, false}) catch |err| {
                    if (err == error.Panic) {
                        return err;
                    } else {
                        return error.Unexpected;
                    }
                };
            }
        }

        // Restore pc/sp.
        vm.framePtr = vm.stack.ptr + fpOff;
        vm.pc = pc;
        if (config.from_external) {
            return vm.framePtr[ret + 1 + 4];
        } else {
            return vm.framePtr[ret];
        }
    }

    pub fn addAnonymousStruct(self: *VM, parent: *cy.Sym, baseName: []const u8, uniqId: u32, fields: []const []const u8) !cy.TypeId {
        const mod = parent.getMod().?;
        const c = mod.chunk;

        const name = try std.fmt.allocPrint(self.alloc, "{s}{}", .{baseName, uniqId});
        errdefer self.alloc.free(name);

        if (mod.getSym(name)) |_| {
            return error.DuplicateSym;
        }
        const sym = c.declareObjectType(parent, name, cy.NullId, null) catch return error.Unexpected;
        sym.head.setNameOwned(true);

        const infos = try c.alloc.alloc(cy.sym.FieldInfo, fields.len);
        for (fields, 0..) |field, i| {
            const field_sym = c.declareField(@ptrCast(sym), field, @intCast(i), bt.Any, cy.NullId) catch return error.Unexpected;
            infos[i] = .{
                .sym = @ptrCast(field_sym),
                .type = bt.Any,
            };
        }
        sym.fields = infos.ptr;
        sym.numFields = @intCast(infos.len);
        c.sema.types.items[sym.type].data.object = .{
            .numFields = @intCast(sym.numFields),
        };

        // Update vm types view.
        self.types = c.sema.types.items;
        return sym.type;
    }

    pub fn getStructFieldIdx(self: *const VM, typeId: cy.TypeId, propName: []const u8) ?u32 {
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
        const nameId = try rt.ensureNameSym(self, name);
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

    pub fn ensureMethod(self: *VM, name: []const u8) !rt.MethodId {
        const nameId = try rt.ensureNameSym(self, name);
        const res = try @call(.never_inline, @TypeOf(self.method_map).getOrPut, .{&self.method_map, self.alloc, nameId});
        if (!res.found_existing) {
            const id: u32 = @intCast(self.methods.len);
            try self.methods.append(self.alloc, .{
                .mru_type = cy.NullId,
                .mru_id = cy.NullId,
                .mru_overloaded = false,
                .has_multiple_types = false,
            });
            try self.method_names.append(self.alloc, nameId);
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn addFieldSym(self: *VM, sid: cy.TypeId, fieldId: u32, offset: u16, typeSymId: types.TypeId) !void {
        const sym = &self.fieldSyms.buf[fieldId];
        if (sym.mruTypeId != cy.NullId) {
            // Add prev mru if it doesn't exist in hashmap.
            const prev = rt.FieldTableKey.initFieldTableKey(sym.mruTypeId, fieldId);
            if (!self.fieldTable.contains(prev)) {
                try self.fieldTable.putNoClobber(self.alloc, prev, .{
                    .offset = sym.mruOffset,
                    .typeId = sym.mruFieldTypeSymId,
                });
            }
            const key = rt.FieldTableKey.initFieldTableKey(sid, fieldId);
            try self.fieldTable.putNoClobber(self.alloc, key, .{
                .offset = offset,
                .typeId = typeSymId,
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

    pub fn addTypeMethod(self: *VM, typeId: cy.TypeId, mgId: rt.MethodId, func: u32, is_overloaded: bool) !rt.TypeMethodId {
        const key = rt.TypeMethodKey.initTypeMethodKey(typeId, mgId);
        const res = try self.type_method_map.getOrPut(self.alloc, key);
        if (res.found_existing) {
            return error.Unexpected;
        }
        const id: rt.TypeMethodId = @intCast(self.type_methods.len);
        try self.type_methods.append(self.alloc, .{
            .id = func,
            .overloaded = is_overloaded,
        });
        res.value_ptr.* = id;
        return id;
    }

    pub fn addMethod(self: *VM, type_id: cy.TypeId, method_id: rt.MethodId, func: u32, is_overloaded_func: bool) !void {
        _ = try self.addTypeMethod(type_id, method_id, func, is_overloaded_func);
        const method = &self.methods.buf[method_id];
        if (method.mru_type != cy.NullId) {
            if (method.mru_type == type_id) {
                return error.Unexpected;
            }
            method.has_multiple_types = true;
        } else {
            // Empty method entry.
            method.* = .{
                .mru_type = type_id,
                .mru_id = func,
                .mru_overloaded = is_overloaded_func,
                .has_multiple_types = false,
            };
        }
    }

    pub fn addFunc(self: *VM, name: []const u8, sig: cy.sema.FuncSigId, rtFunc: rt.FuncSymbol) !u32 {
        const id = self.funcSyms.len;
        try self.funcSyms.append(self.alloc, rtFunc);

        try self.funcSymDetails.append(self.alloc, .{
            .namePtr = name.ptr, .nameLen = @intCast(name.len), .funcSigId = sig,
        });
        return @intCast(id);
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

    fn panic(self: *VM, msg: []const u8) error{Panic, OutOfMemory} {
        @setCold(true);
        const dupe = try self.alloc.dupe(u8, msg);
        self.curFiber.panicPayload = @as(u64, @intFromPtr(dupe.ptr)) | (@as(u64, dupe.len) << 48);
        self.curFiber.panicType = vmc.PANIC_MSG;
        logger.tracev("{s}", .{dupe});
        return error.Panic;
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
        logger.tracev("{s}", .{msg});
        return error.Panic;
    }

    fn setField(self: *VM, recv: Value, fieldId: SymbolId, val: Value) !void {
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

    pub fn getFieldOffsetFromTable(self: *VM, typeId: cy.TypeId, symId: SymbolId) u8 {
        const key = rt.FieldTableKey.initFieldTableKey(typeId, symId);
        if (self.fieldTable.get(key)) |res| {
            const sym = &self.fieldSyms.buf[symId];
            sym.mruTypeId = typeId;
            sym.mruOffset = res.offset;
            sym.mruFieldTypeSymId = res.typeId;
            return @intCast(res.offset);
        } else {
            return cy.NullU8;
        }
    }

    pub fn getFieldOffset(self: *VM, obj: *HeapObject, symId: SymbolId) u8 {
        const symMap = self.fieldSyms.buf[symId];
        if (obj.getTypeId() == symMap.mruTypeId) {
            return @intCast(symMap.mruOffset);
        } else {
            return @call(.never_inline, VM.getFieldOffsetFromTable, .{self, obj.getTypeId(), symId});
        }
    }

    pub fn setFieldRelease(self: *VM, recv: Value, symId: SymbolId, val: Value) !void {
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

    pub fn getField2(self: *VM, recv: Value, symId: SymbolId) !Value {
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

    pub fn getField(self: *VM, recv: Value, symId: SymbolId) !Value {
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

    fn setFieldFallback(self: *VM, obj: *HeapObject, nameId: vmc.NameId, val: cy.Value) !void {
        const name = rt.getName(self, nameId);
        const rec_t = obj.getTypeId();
        if (!self.types[rec_t].has_set_method) {
            _ = self.prepPanic("Missing field in object.");
            return error.Panic;
        }
        const rec_arg = Value.initPtr(obj);
        const name_arg = try self.allocString(name);
        defer self.release(name_arg);

        const args: []const Value = &.{rec_arg, name_arg, val};
        const func = self.getCompatMethodFunc(rec_t, self.compiler.setMID, args[1..]) orelse {
            return error.Unexpected;
        };
        const func_val = try cy.heap.allocFuncFromSym(self, func);
        defer self.release(func_val);
        const result = try self.callFunc(func_val, args, .{ .from_external = false });
        if (result.isInterrupt()) {
            return error.Panic;
        }
    }

    fn getFieldFallback(self: *VM, obj: *HeapObject, nameId: vmc.NameId) !Value {
        const name = rt.getName(self, nameId);
        const rec_t = obj.getTypeId();
        if (!self.types[rec_t].has_get_method) {
            return self.prepPanic("Missing field in object.");
        }

        const rec_arg = Value.initPtr(obj);
        const name_arg = try self.allocString(name);
        defer self.release(name_arg);

        const args: []const Value = &.{rec_arg, name_arg};
        const func = self.getCompatMethodFunc(rec_t, self.compiler.getMID, args[1..]) orelse {
            return error.Unexpected;
        };

        const func_val = try cy.heap.allocFuncFromSym(self, func);
        defer self.release(func_val);
        const result = try self.callFunc(func_val, args, .{ .from_external = false });
        return result;
    }

    /// Assumes overloaded function. Finds first matching function at runtime.
    fn callSymDyn(
        self: *VM, pc: [*]cy.Inst, fp: [*]Value, entry: u32, ret: u8, nargs: u8,
    ) !cy.fiber.PcSp {
        const num_funcs = self.overloaded_funcs.buf[entry];
        const funcs = self.overloaded_funcs.buf[entry+1..entry+1+num_funcs];
        const vals = fp[ret+CallArgStart..ret+CallArgStart+nargs];
        for (funcs) |func_id| {
            const func = self.funcSyms.buf[func_id];
            if (!self.isFuncCompat(func, vals)) {
                continue;
            }

            // Invoke function.
            // TODO: Extract common code between this and `callSym`
            switch (func.type) {
                .host_func => {
                    const newFramePtr = fp + ret;

                    self.pc = pc;
                    self.framePtr = fp;
                    const res: Value = @bitCast(func.data.host_func.?(@ptrCast(self), @ptrCast(newFramePtr + CallArgStart), nargs));
                    if (res.isInterrupt()) {
                        return error.Panic;
                    }
                    newFramePtr[0] = @bitCast(res);
                    return cy.fiber.PcSp{
                        .pc = pc + cy.bytecode.CallSymInstLen,
                        .sp = fp,
                    };
                },
                .func => {
                    if (@intFromPtr(fp + ret + func.data.func.stackSize) >= @intFromPtr(self.stackEndPtr)) {
                        return error.StackOverflow;
                    }

                    const newFramePtr = fp + ret;
                    newFramePtr[1] = buildReturnInfo(true, cy.bytecode.CallSymInstLen);
                    newFramePtr[2] = Value{ .retPcPtr = pc + cy.bytecode.CallSymInstLen };
                    newFramePtr[3] = Value{ .retFramePtr = fp };
                    return cy.fiber.PcSp{
                        .pc = cy.fiber.toVmPc(self, func.data.func.pc),
                        .sp = newFramePtr,
                    };
                },
                else => {
                    return self.panic("Missing func");
                },
            }
        }
        return panicIncompatibleCallSymSig(self, entry, vals);
    }

    /// startLocal points to the first arg in the current stack frame.
    fn callSym(
        self: *VM, pc: [*]cy.Inst, framePtr: [*]Value, func_id: rt.FuncId, ret: u8, numArgs: u8,
    ) !cy.fiber.PcSp {
        const func = self.funcSyms.buf[func_id];
        switch (func.type) {
            .host_func => {
                // Optimize.
                pc[0] = cy.Inst.initOpCode(.callNativeFuncIC);
                @as(*align(1) u48, @ptrCast(pc + 6)).* = @intCast(@intFromPtr(func.data.host_func));

                self.pc = pc;
                self.framePtr = framePtr;

                const newFramePtr = framePtr + ret;
                const res: Value = @bitCast(func.data.host_func.?(@ptrCast(self), @ptrCast(newFramePtr + CallArgStart), numArgs));
                if (res.isInterrupt()) {
                    return error.Panic;
                }
                newFramePtr[0] = @bitCast(res);
                return cy.fiber.PcSp{
                    .pc = pc + cy.bytecode.CallSymInstLen,
                    .sp = framePtr,
                };
            },
            .func => {
                if (@intFromPtr(framePtr + ret + func.data.func.stackSize) >= @intFromPtr(self.stackEndPtr)) {
                    return error.StackOverflow;
                }

                // Optimize.
                pc[0] = cy.Inst.initOpCode(.callFuncIC);
                pc[4] = cy.Inst{ .val = @intCast(func.data.func.stackSize) };
                @as(*align(1) u48, @ptrCast(pc + 6)).* = @intCast(@intFromPtr(cy.fiber.toVmPc(self, func.data.func.pc)));

                const newFramePtr = framePtr + ret;
                newFramePtr[1] = buildReturnInfo(true, cy.bytecode.CallSymInstLen);
                newFramePtr[2] = Value{ .retPcPtr = pc + cy.bytecode.CallSymInstLen };
                newFramePtr[3] = Value{ .retFramePtr = framePtr };
                return cy.fiber.PcSp{
                    .pc = cy.fiber.toVmPc(self, func.data.func.pc),
                    .sp = newFramePtr,
                };
            },
            .null => {
                return self.panic("Missing func");
            },
        }
    }

    /// Like `isFuncCompat` but does not check rec.
    fn isMethodFuncCompat(self: *VM, func: rt.FuncSymbol, args: []const Value) bool {
        // Perform type check on args.
        const target = self.compiler.sema.getFuncSig(func.sig);
        const target_params = target.params()[1..];

        if (target_params.len != args.len) {
            return false;
        }

        for (args, target_params) |val, target_t| {
            const valTypeId = val.getTypeId();
            if (!types.isTypeSymCompat(self.compiler, valTypeId, target_t)) {
                return false;
            }
        }
        return true;
    }

    fn isFuncCompat(self: *VM, func: rt.FuncSymbol, args: []const Value) bool {
        // Perform type check on args.
        const target = self.compiler.sema.getFuncSig(func.sig);
        const target_params = target.params();

        if (target_params.len != args.len) {
            return false;
        }

        for (args, target_params) |val, target_t| {
            const valTypeId = val.getTypeId();
            if (!types.isTypeSymCompat(self.compiler, valTypeId, target_t)) {
                return false;
            }
        }
        return true;
    }

    fn getTypeMethod(self: *VM, rec_t: cy.TypeId, method: rt.MethodId) ?rt.TypeMethodId {
        const key = rt.TypeMethodKey.initTypeMethodKey(rec_t, method);
        return self.type_method_map.get(key);
    }

    /// Assumes args does not include rec.
    fn getCompatMethodFunc(self: *VM, rec_t: cy.TypeId, method_id: rt.MethodId, args: []const cy.Value) ?rt.FuncSymbol {
        var method = self.methods.buf[method_id];
        if (method.mru_type != rec_t) {
            if (!method.has_multiple_types) {
                return null;
            }
            if (@call(.never_inline, getTypeMethod, .{self, rec_t, method_id})) |tm_id| {
                const type_method = self.type_methods.buf[tm_id];
                method.mru_type = rec_t;
                method.mru_id = type_method.id;
                method.mru_overloaded = type_method.overloaded;
                self.methods.buf[method_id] = method;
            } else {
                return null;
            }
        }

        // TODO: Skip type check for untyped params.

        if (!method.mru_overloaded) {
            // Check one function.
            const func = self.funcSyms.buf[method.mru_id];
            if (self.isMethodFuncCompat(func, args)) {
                return func;
            } else {
                return null;
            }
        } else {
            // Overloaded functions.
            const num_funcs = self.overloaded_funcs.buf[method.mru_id];
            const funcs = self.overloaded_funcs.buf[method.mru_id+1..method.mru_id+1+num_funcs];

            for (funcs) |func_id| {
                const func = self.funcSyms.buf[func_id];

                if (!func.is_method) {
                    continue;
                }

                if (self.isMethodFuncCompat(func, args)) {
                    return func;
                }
            }
            return null;
        }
    }

    pub fn getStackTrace(self: *const VM) *const cy.StackTrace {
        return &self.stackTrace;
    }

    pub fn allocValueStr(self: *const VM, val: Value) ![]const u8 {
        const str = try self.getOrBufPrintValueStr(&cy.tempBuf, val);
        return try self.alloc.dupe(u8, str);
    }

    /// Not guaranteed to be valid UTF-8. Returns `array` bytes.
    pub fn getOrBufPrintValueRawStr(self: *const VM, buf: []u8, val: Value) ![]const u8 {
        if (val.isArray()) {
            return val.asArray();
        } else {
            return self.getOrBufPrintValueStr(buf, val);
        }
    }

    pub fn bufPrintValueShortStr(self: *const VM, buf: []u8, val: Value) ![]const u8 {
        var fbuf = std.io.fixedBufferStream(buf);
        var w = fbuf.writer();

        if (val.isString()) {
            const str = val.asString();
            if (str.len > 20) {
                try w.print("String({}) {s}...", .{str.len, str[0..20]});
            } else {
                try w.print("String({}) {s}", .{str.len, str});
            }
        } else {
            _ = try self.writeValue(w, val);
        }
        return fbuf.getWritten();
    }

    /// String is guaranteed to be valid UTF-8.
    pub fn getOrBufPrintValueStr(self: *const VM, buf: []u8, val: Value) ![]const u8 {
        var fbuf = std.io.fixedBufferStream(buf);
        var w = fbuf.writer();

        if (val.isString()) {
            return val.asString();
        } else {
            _ = try self.writeValue(w, val);
        }
        return fbuf.getWritten();
    }

    pub fn getOrBufPrintValueStr2(self: *const VM, buf: []u8, val: Value, out_ascii: *bool) ![]const u8 {
        var fbuf = std.io.fixedBufferStream(buf);
        var w = fbuf.writer();

        if (val.isString()) {
            out_ascii.* = val.asHeapObject().string.getType().isAstring();
            return val.asString();
        } else {
            _ = try self.writeValue(w, val);
        }
        const res = fbuf.getWritten();
        out_ascii.* = true;
        return res;
    }

    pub fn writeValue(self: *const VM, w: anytype, val: Value) !void {
        const typeId = val.getTypeId();
        
        switch (typeId) {
            bt.Float => {
                const f = val.asF64();
                if (Value.floatIsSpecial(f)) {
                    try std.fmt.format(w, "{}", .{f});
                } else {
                    if (Value.floatCanBeInteger(f)) {
                        try std.fmt.format(w, "{d:.0}", .{f});
                    } else {
                        try std.fmt.format(w, "{d}", .{f});
                    }
                }
                return;
            },
            bt.Boolean => {
                if (val.asBool()) {
                    try w.writeAll("true");
                } else {
                    try w.writeAll("false");
                }
                return;
            },
            bt.Error => {
                const symId = val.asErrorSymbol();
                try std.fmt.format(w, "error.{s}", .{self.getSymbolName(symId)});
                return;
            },
            bt.Symbol => {
                const litId = val.asSymbolId();
                try std.fmt.format(w, ".{s}", .{self.getSymbolName(litId)});
                return;
            },
            bt.Integer => {
                try std.fmt.format(w, "{}", .{val.asInteger()});
                return;
            },
            else => {}, // Fall-through.
        }

        if (!val.isPointer()) {
            if (val.isEnum()) {
                const sym = self.types[typeId].sym;
                const enumv = val.getEnumValue();
                const name = sym.cast(.enum_t).getValueSym(enumv).name();
                try std.fmt.format(w, "{s}.{s}", .{sym.name(), name});
            } else {
                try w.writeAll("Unknown");
            }
            return;
        }

        const obj = val.asHeapObject();
        switch (typeId) {
            bt.String => {
                try w.writeAll(obj.string.getSlice());
            },
            bt.Array => {
                if (obj.array.isSlice()) {
                    try std.fmt.format(w, "Array ({})", .{obj.array.len()});
                } else {
                    try std.fmt.format(w, "Array ({})", .{obj.array.len()});
                }
            },
            bt.List => {
                try std.fmt.format(w, "List ({})", .{obj.list.list.len});
            },
            bt.Map => {
                try std.fmt.format(w, "Map ({})", .{obj.map.inner.size});
            },
            bt.MetaType => {
                const symType: cy.heap.MetaTypeKind = @enumFromInt(obj.metatype.type);
                if (symType == .object) {
                    const name = self.compiler.sema.getTypeBaseName(obj.metatype.type);
                    try std.fmt.format(w, "type: {s}", .{name});
                } else {
                    try w.writeAll("Unknown Symbol");
                }
            },
            else => {
                if (typeId == cy.NullId >> 3) {
                    try w.writeAll("danglingObject");
                    return;
                }
                const name = self.compiler.sema.getTypeBaseName(typeId);
                try w.writeAll(name);
            }
        }
    }

    pub fn setApiError(self: *const VM, str: []const u8) !void {
        self.compiler.hasApiError = true;
        self.alloc.free(self.compiler.apiError);
        self.compiler.apiError = try self.alloc.dupe(u8, str);
    }

    pub fn prepPanic(vm: *VM, msg: []const u8) Value {
        @setCold(true);
        const dupe = vm.alloc.dupe(u8, msg) catch cy.fatal();
        vm.curFiber.panicPayload = @as(u64, @intCast(@intFromPtr(dupe.ptr))) | (@as(u64, dupe.len) << 48);
        vm.curFiber.panicType = vmc.PANIC_MSG;
        return Value.Interrupt;
    }

    pub fn clearTempString(vm: *VM) cy.ListAligned(u8, 8).Writer {
        vm.u8Buf.clearRetainingCapacity();
        return vm.u8Buf.writer(vm.alloc);
    }

    pub fn getTempString(vm: *VM) []const u8 {
        return vm.u8Buf.items();
    }

    pub fn getNewFramePtrOffset(self: *VM, args: [*]const Value, nargs: u8) u32 {
        return @intCast(cy.fiber.getStackOffset(self.stack.ptr, args + nargs));
    }

    pub usingnamespace cy.heap.VmExt;
    pub usingnamespace cy.arc.VmExt;
};

fn evalCompareBool(left: Value, right: Value) bool {
    switch (left.getTypeId()) {
        bt.String => {
            if (right.getTypeId() == bt.String) {
                const lstr = left.asString();
                const rstr = right.asString();
                return std.mem.eql(u8, lstr, rstr);
            }
        },
        bt.Array => {
            if (right.getTypeId() == bt.Array) {
                const lslice = left.asArray();
                const rslice = right.asArray();
                return std.mem.eql(u8, lslice, rslice);
            }
        },
        bt.MetaType => {
            if (right.getTypeId() == bt.MetaType) {
                const l = left.asHeapObject().metatype;
                const r = right.asHeapObject().metatype;
                return l.typeKind == r.typeKind and l.type == r.type;
            }
        },
        else => {},
    }
    return false;
}

fn evalCompare(left: Value, right: Value) Value {
    switch (left.getTypeId()) {
        bt.String => {
            if (right.getTypeId() == bt.String) {
                const lstr = left.asString();
                const rstr = right.asString();
                return Value.initBool(std.mem.eql(u8, lstr, rstr));
            }
        },
        bt.Array => {
            if (right.getTypeId() == bt.Array) {
                const lslice = left.asArray();
                const rslice = right.asArray();
                return Value.initBool(std.mem.eql(u8, lslice, rslice));
            }
        },
        bt.MetaType => {
            if (right.getTypeId() == bt.MetaType) {
                const l = left.asHeapObject().metatype;
                const r = right.asHeapObject().metatype;
                return Value.initBool(l.typeKind == r.typeKind and l.type == r.type);
            }
        },
        else => {},
    }
    return Value.False;
}

fn evalCompareNot(left: cy.Value, right: cy.Value) cy.Value {
    switch (left.getTypeId()) {
        bt.String => {
            if (right.getTypeId() == bt.String) {
                const lstr = left.asString();
                const rstr = right.asString();
                return Value.initBool(!std.mem.eql(u8, lstr, rstr));
            }
        },
        bt.Array => {
            if (right.getTypeId() == bt.Array) {
                const lslice = left.asArray();
                const rslice = right.asArray();
                return Value.initBool(!std.mem.eql(u8, lslice, rslice));
            }
        },
        bt.MetaType => {
            if (right.getTypeId() == bt.MetaType) {
                const l = left.asHeapObject().metatype;
                const r = right.asHeapObject().metatype;
                return Value.initBool(l.typeKind != r.typeKind or l.type != r.type);
            }
        },
        else => {},
    }
    return Value.True;
}

fn toF64OrPanic(vm: *cy.VM, val: Value) !f64 {
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
            id: u32,
            val: u32,
        },
    },
    name: []const u8,
    nameOwned: bool,
};

test "vm internals." {
    try t.eq(@alignOf(VM), 8);

    // Check Zig/C structs.

    // cy.verbose = true;
    // inline for (std.meta.fields(VM)) |field| {
    //     logger.tracev("{s} {}", .{field.name, @offsetOf(VM, field.name)});
    // }

    try t.eq(@offsetOf(VM, "alloc"), @offsetOf(vmc.VM, "alloc"));
    try t.eq(@offsetOf(VM, "pc"), @offsetOf(vmc.VM, "curPc"));
    try t.eq(@offsetOf(VM, "framePtr"), @offsetOf(vmc.VM, "curStack"));
    try t.eq(@offsetOf(VM, "stack"), @offsetOf(vmc.VM, "stackPtr"));
    try t.eq(@offsetOf(VM, "stackEndPtr"), @offsetOf(vmc.VM, "stackEndPtr"));
    try t.eq(@offsetOf(VM, "ops"), @offsetOf(vmc.VM, "instPtr"));
    try t.eq(@offsetOf(VM, "consts"), @offsetOf(vmc.VM, "constPtr"));
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
    try t.eq(@offsetOf(VM, "method_map"), @offsetOf(vmc.VM, "method_map"));
    try t.eq(@offsetOf(VM, "type_methods"), @offsetOf(vmc.VM, "type_methods"));
    try t.eq(@offsetOf(VM, "type_method_map"), @offsetOf(vmc.VM, "type_method_map"));
    try t.eq(@offsetOf(VM, "funcSyms"), @offsetOf(vmc.VM, "funcSyms"));
    try t.eq(@offsetOf(VM, "funcSymDetails"), @offsetOf(vmc.VM, "funcSymDetails"));
    try t.eq(@offsetOf(VM, "overloaded_funcs"), @offsetOf(vmc.VM, "overloaded_funcs"));
    try t.eq(@offsetOf(VM, "varSyms"), @offsetOf(vmc.VM, "varSyms"));
    try t.eq(@offsetOf(VM, "fieldSyms"), @offsetOf(vmc.VM, "fieldSyms"));
    try t.eq(@offsetOf(VM, "fieldTable"), @offsetOf(vmc.VM, "fieldTable"));
    try t.eq(@offsetOf(VM, "fieldSymSignatures"), @offsetOf(vmc.VM, "fieldSymSignatures"));
    try t.eq(@offsetOf(VM, "inlineSaves"), @offsetOf(vmc.VM, "inlineSaves"));
    try t.eq(@offsetOf(VM, "syms"), @offsetOf(vmc.VM, "syms"));
    try t.eq(@offsetOf(VM, "symSignatures"), @offsetOf(vmc.VM, "symSignatures"));
    try t.eq(@offsetOf(VM, "names"), @offsetOf(vmc.VM, "names"));
    try t.eq(@offsetOf(VM, "nameMap"), @offsetOf(vmc.VM, "nameMap"));
    try t.eq(@offsetOf(VM, "u8Buf"), @offsetOf(vmc.VM, "u8Buf"));
    try t.eq(@offsetOf(VM, "stackTrace"), @offsetOf(vmc.VM, "stackTrace"));
    try t.eq(@offsetOf(VM, "method_names"), @offsetOf(vmc.VM, "method_names"));
    try t.eq(@offsetOf(VM, "debugTable"), @offsetOf(vmc.VM, "debugTablePtr"));
    try t.eq(@offsetOf(VM, "debugTempIndexTable"), @offsetOf(vmc.VM, "debugTempIndexTablePtr"));
    try t.eq(@offsetOf(VM, "unwindTempRegs"), @offsetOf(vmc.VM, "unwindTempRegsPtr"));
    try t.eq(@offsetOf(VM, "unwindTempPrevIndexes"), @offsetOf(vmc.VM, "unwindTempPrevIndexesPtr"));
    try t.eq(@offsetOf(VM, "curFiber"), @offsetOf(vmc.VM, "curFiber"));
    try t.eq(@offsetOf(VM, "mainFiber"), @offsetOf(vmc.VM, "mainFiber"));
    try t.eq(@offsetOf(VM, "compactTrace"), @offsetOf(vmc.VM, "compactTrace"));
    try t.eq(@offsetOf(VM, "compiler"), @offsetOf(vmc.VM, "compiler"));
    try t.eq(@offsetOf(VM, "userData"), @offsetOf(vmc.VM, "userData"));
    try t.eq(@offsetOf(VM, "print"), @offsetOf(vmc.VM, "print"));
    try t.eq(@offsetOf(VM, "print_err"), @offsetOf(vmc.VM, "print_err"));
    try t.eq(@offsetOf(VM, "httpClient"), @offsetOf(vmc.VM, "httpClient"));
    try t.eq(@offsetOf(VM, "stdHttpClient"), @offsetOf(vmc.VM, "stdHttpClient"));
    try t.eq(@offsetOf(VM, "emptyString"), @offsetOf(vmc.VM, "emptyString"));
    try t.eq(@offsetOf(VM, "emptyArray"), @offsetOf(vmc.VM, "emptyArray"));

    try t.eq(@offsetOf(VM, "varSymExtras"), @offsetOf(vmc.VM, "varSymExtras"));
    try t.eq(@offsetOf(VM, "endLocal"), @offsetOf(vmc.VM, "endLocal"));

    if (cy.Trace) {
        try t.eq(@offsetOf(VM, "trace"), @offsetOf(vmc.VM, "trace"));
        try t.eq(@offsetOf(VM, "objectTraceMap"), @offsetOf(vmc.VM, "objectTraceMap"));
    }

    try t.eq(@offsetOf(VM, "config"), @offsetOf(vmc.VM, "config"));
    try t.eq(@offsetOf(VM, "lastExeError"), @offsetOf(vmc.VM, "lastExeError"));

    if (cy.Trace) {
        try t.eq(@offsetOf(VM, "debugPc"), @offsetOf(vmc.VM, "debugPc"));
    }
}

pub const SymbolId = u32;

const Root = @This();

/// If successful, execution should continue.
pub fn handleInterrupt(vm: *VM, rootFp: u32) !void {
    if (vm.curFiber.panicType == vmc.PANIC_NATIVE_THROW) {
        const res = try @call(.never_inline, cy.fiber.throw, .{
            vm, rootFp, vm.getFiberContext(), Value.initRaw(vm.curFiber.panicPayload) });
        vm.pc = vm.ops.ptr + res.pc;
        vm.framePtr = vm.stack.ptr + res.sp;
    } else {
        const res = try @call(.never_inline, panicCurFiber, .{ vm });
        vm.pc = vm.ops.ptr + res.pc;
        vm.framePtr = vm.stack.ptr + res.sp;
    }
}

/// To reduce the amount of code inlined in the hot loop, handle StackOverflow at the top and resume execution.
/// This is also the entry way for native code to call into the VM, assuming pc, framePtr, and virtual registers are already set.
pub fn evalLoopGrowStack(vm: *VM, handle_panic: bool) error{StackOverflow, OutOfMemory, Panic, NoDebugSym, Unexpected, End}!void {
    logger.tracev("begin eval loop", .{});

    // Record the start fp offset, so that stack unwinding knows when to stop.
    // This is useful for having nested eval stacks.
    const startFp = cy.fiber.getStackOffset(vm.stack.ptr, vm.framePtr);

    if (comptime build_options.vmEngine == .zig) {
        while (true) {
            @call(.always_inline, evalLoop, .{vm}) catch |err| {
                if (err == error.StackOverflow) {
                    logger.debug("grow stack", .{});
                    try @call(.never_inline, cy.fiber.growStackAuto, .{vm});
                    continue;
                } else if (err == error.End) {
                    return;
                } else if (err == error.Panic) {
                    try @call(.never_inline, handleInterrupt, .{vm});
                    continue;
                } else return err;
            };
            return;
        }
    } else if (comptime build_options.vmEngine == .c) {
        while (true) {
            const res = vmc.execBytecode(@ptrCast(vm));
            if (res == vmc.RES_CODE_SUCCESS) {
                break;
            }
            if (handle_panic) {
                try @call(.never_inline, handleExecResult, .{vm, res, startFp});
            } else {
                if (res == vmc.RES_CODE_PANIC or res == vmc.RES_CODE_UNKNOWN) {
                    return error.Panic;
                } else {
                    try @call(.never_inline, handleExecResult, .{vm, res, startFp});
                }
            }
        }
    } else {
        @compileError("Unsupported engine.");
    }
}

fn handleExecResult(vm: *VM, res: vmc.ResultCode, fpStart: u32) !void {
    logger.tracev("handle exec error: {}", .{res});
    if (res == vmc.RES_CODE_PANIC) {
        try handleInterrupt(vm, fpStart);
    } else if (res == vmc.RES_CODE_STACK_OVERFLOW) {
        logger.tracev("grow stack", .{});
        try @call(.never_inline, cy.fiber.growStackAuto, .{vm});
    } else if (res == vmc.RES_CODE_UNKNOWN) {
        logger.tracev("Unknown error code.", .{});
        const cont = try @call(.never_inline, panicCurFiber, .{ vm });
        vm.pc = vm.ops.ptr + cont.pc;
        vm.framePtr = vm.stack.ptr + cont.sp;
    }
}

fn panicCurFiber(vm: *VM) !cy.fiber.PcSpOff {
    var ctx = vm.getFiberContext();
    ctx = try cy.fiber.unwindStack(vm, vm.stack, ctx);

    const frames = try debug.allocStackTrace(vm, vm.stack, vm.compactTrace.items());
    vm.stackTrace.deinit(vm.alloc);
    vm.stackTrace.frames = frames;

    return cy.fiber.fiberEnd(vm, ctx) orelse {
        return error.Panic;
    };
}

/// Generate assembly labels to find sections easier.
const GenLabels = builtin.mode != .Debug and !builtin.cpu.arch.isWasm() and false;

const DebugTraceStopAtNumOps: ?u32 = null;

fn evalLoop(vm: *VM) error{StackOverflow, OutOfMemory, Panic, NoDebugSym, End}!void {
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
                    if (fiber.typeId == bt.Fiber) {
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
                    std.debug.assert(obj.getTypeId() == bt.Box);
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
                    std.debug.assert(obj.getTypeId() == bt.Box);
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
                    if (val.isArray()) {
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

fn popStackFrameLocal0(pc: *[*]const cy.Inst, framePtr: *[*]Value) bool {
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

fn popStackFrameLocal1(vm: *VM, pc: *[*]const cy.Inst, framePtr: *[*]Value) bool {
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

fn dumpEvalOp(vm: *VM, pc: [*]const cy.Inst) !void {
    const S = struct {
        var buf: [1024]u8 = undefined;
    };
    const offset = getInstOffset(vm, pc);
    var extra: []const u8 = "";
    switch (pc[0].opcode()) {
        .callObjSym => {
            const symId = pc[4].val;
            const name_id = vm.method_names.buf[symId];
            const name = rt.getName(vm, name_id);
            extra = try std.fmt.bufPrint(&S.buf, "rt: sym={s}", .{name});
        },
        .constOp => {
            const idx = @as(*const align (1) u16, @ptrCast(pc + 1)).*;
            const dst = pc[3].val;
            _ = dst;
            const val = Value{ .val = vm.consts[idx].val };
            extra = try std.fmt.bufPrint(&S.buf, "rt: constVal={s}", .{
                try vm.getOrBufPrintValueStr(&cy.tempBuf, val)
            });
        },
        .callObjNativeFuncIC => {
            const symId = pc[4].val;
            const name_id = vm.method_names.buf[symId];
            const name = rt.getName(vm, name_id);
            extra = try std.fmt.bufPrint(&S.buf, "rt: sym={s}", .{name});
        },
        .callSym => {
            const symId = @as(*const align(1) u16, @ptrCast(pc + 4)).*;
            const details = vm.funcSymDetails.buf[symId];
            const name = details.namePtr[0..details.nameLen];
            extra = try std.fmt.bufPrint(&S.buf, "rt: sym={s}", .{name});
        },
        .fieldDyn => {
            const symId = pc[3].val;
            const sym = vm.fieldSyms.buf[symId];
            const name = rt.getName(vm, sym.nameId);
            extra = try std.fmt.bufPrint(&S.buf, "rt: sym={s}", .{name});
        },
        else => {},
    }
    try cy.bytecode.dumpInst(vm, offset, pc[0].opcode(), pc, .{ .extra = extra });
}

const FieldEntry = struct {
    offset: u32,
    typeId: types.TypeId,
};

/// See `reserveFuncParams` for stack layout.
/// numArgs does not include the callee.
pub fn call(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, callee: Value, ret: u8, numArgs: u8, cont: bool) !cy.fiber.PcSp {
    if (callee.isPointer()) {
        const obj = callee.asHeapObject();
        switch (obj.getTypeId()) {
            bt.Closure => {
                if (numArgs != obj.closure.numParams) {
                    logger.tracev("closure params/args mismatch {} {}", .{numArgs, obj.closure.numParams});
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                if (obj.closure.reqCallTypeCheck) {
                    // Perform type check on args.
                    const args = framePtr[ret+CallArgStart..ret+CallArgStart+numArgs];
                    const cstrFuncSig = vm.compiler.sema.getFuncSig(obj.closure.funcSigId);
                    for (args, 0..) |arg, i| {
                        const cstrType = cstrFuncSig.paramPtr[i];
                        const argType = arg.getTypeId();
                        if (!types.isTypeSymCompat(vm.compiler, argType, cstrType)) {
                            return panicIncompatibleLambdaSig(vm, args, obj.closure.funcSigId);
                        }
                    }
                }

                if (@intFromPtr(framePtr + ret + obj.closure.stackSize) >= @intFromPtr(vm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr };
                framePtr[ret + 1] = buildReturnInfo2(cont, cy.bytecode.CallInstLen);
                framePtr[ret + 2] = Value{ .retPcPtr = pc + cy.bytecode.CallInstLen };
                framePtr[ret + 3] = retFramePtr;

                // Copy closure to local.
                framePtr[ret + obj.closure.local] = callee;
                return cy.fiber.PcSp{
                    .pc = cy.fiber.toVmPc(vm, obj.closure.funcPc),
                    .sp = framePtr + ret,
                };
            },
            bt.Lambda => {
                if (numArgs != obj.lambda.numParams) {
                    logger.tracev("lambda params/args mismatch {} {}", .{numArgs, obj.lambda.numParams});
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                if (obj.lambda.reqCallTypeCheck) {
                    // Perform type check on args.
                    const args = framePtr[ret+CallArgStart..ret+CallArgStart+numArgs];
                    const cstrFuncSig = vm.compiler.sema.getFuncSig(obj.lambda.funcSigId);
                    for (args, 0..) |arg, i| {
                        const cstrType = cstrFuncSig.paramPtr[i];
                        const argType = arg.getTypeId();
                        if (!types.isTypeSymCompat(vm.compiler, argType, cstrType)) {
                            return panicIncompatibleLambdaSig(vm, args, obj.lambda.funcSigId);
                        }
                    }
                }

                if (@intFromPtr(framePtr + ret + obj.lambda.stackSize) >= @intFromPtr(vm.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const retFramePtr = Value{ .retFramePtr = framePtr };
                framePtr[ret + 1] = buildReturnInfo2(cont, cy.bytecode.CallInstLen);
                framePtr[ret + 2] = Value{ .retPcPtr = pc + cy.bytecode.CallInstLen };
                framePtr[ret + 3] = retFramePtr;
                return cy.fiber.PcSp{
                    .pc = cy.fiber.toVmPc(vm, obj.lambda.funcPc),
                    .sp = framePtr + ret,
                };
            },
            bt.HostFunc => {
                if (numArgs != obj.hostFunc.numParams) {
                    logger.tracev("hostfunc params/args mismatch {} {}", .{numArgs, obj.hostFunc.numParams});
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                if (obj.hostFunc.reqCallTypeCheck) {
                    // Perform type check on args.
                    const args = framePtr[ret+CallArgStart..ret+CallArgStart+numArgs];
                    const cstrFuncSig = vm.compiler.sema.getFuncSig(obj.hostFunc.funcSigId);
                    for (args, 0..) |arg, i| {
                        const cstrType = cstrFuncSig.paramPtr[i];
                        const argType = arg.getTypeId();
                        if (!types.isTypeSymCompat(vm.compiler, argType, cstrType)) {
                            return panicIncompatibleLambdaSig(vm, args, obj.hostFunc.funcSigId);
                        }
                    }
                }

                vm.pc = pc;
                vm.framePtr = framePtr;
                const newFramePtr = framePtr + ret;
                const res = obj.hostFunc.func.?(@ptrCast(vm), @ptrCast(newFramePtr + CallArgStart), numArgs);
                newFramePtr[0] = @bitCast(res);
                return cy.fiber.PcSp{
                    .pc = pc + cy.bytecode.CallInstLen,
                    .sp = framePtr,
                };
            },
            else => {
                return vm.interruptThrowSymbol(.InvalidArgument);
            },
        }
    } else {
        return vm.interruptThrowSymbol(.InvalidArgument);
    }
}

fn panicCastAbstractError(vm: *cy.VM, val: Value, expTypeSymId: cy.sema.SymbolId) !void {
    const sym = vm.compiler.sema.getSymbol(expTypeSymId);
    const name = cy.sema.getName(&vm.compiler, sym.key.resolvedSymKey.nameId);
    return vm.panicFmt("Can not cast `{}` to `{}`.", &.{
         v(vm.types.buf[val.getTypeId()].name), v(name), 
    });
}

fn panicCastError(vm: *cy.VM, val: Value, expTypeId: cy.TypeId) !void {
    return vm.panicFmt("Can not cast `{}` to `{}`.", &.{
         v(vm.types.buf[val.getTypeId()].name), v(vm.types.buf[expTypeId].name), 
    });
}

fn allocValueTypeIds(vm: *cy.VM, vals: []const Value) ![]const cy.TypeId {
    const typeIds = try vm.alloc.alloc(cy.TypeId, vals.len);
    for (vals, 0..) |val, i| {
        typeIds[i] = val.getTypeId();
    }
    return typeIds;
}

fn allocMethodCallTypeIds(vm: *cy.VM, rec_t: cy.TypeId, vals: []const Value) ![]const cy.TypeId {
    const typeIds = try vm.alloc.alloc(cy.TypeId, vals.len + 1);
    typeIds[0] = rec_t;
    for (vals, 1..) |val, i| {
        typeIds[i] = val.getTypeId();
    }
    return typeIds;
}

fn panicIncompatibleFieldType(vm: *cy.VM, fieldSemaTypeId: types.TypeId, rightv: Value) error{Panic, OutOfMemory} {
    defer release(vm, rightv);

    const fieldTypeName = sema.getSymName(&vm.compiler, fieldSemaTypeId);
    const rightTypeId = rightv.getTypeId();
    const rightSemaTypeId = vm.types.buf[rightTypeId].rTypeSymId;
    const rightTypeName = sema.getSymName(&vm.compiler, rightSemaTypeId);
    return vm.panicFmt(
        \\Assigning to `{}` field with incompatible type `{}`.
        , &.{
            v(fieldTypeName), v(rightTypeName),
        },
    );
}

fn panicIncompatibleLambdaSig(vm: *cy.VM, args: []const Value, cstrFuncSigId: sema.FuncSigId) error{Panic, OutOfMemory} {
    const cstrFuncSigStr = try vm.compiler.sema.allocFuncSigStr(cstrFuncSigId, true);
    defer vm.alloc.free(cstrFuncSigStr);
    const argTypes = try allocValueTypeIds(vm, args);
    defer vm.alloc.free(argTypes);
    const argsSigStr = try vm.compiler.sema.allocFuncSigTypesStr(argTypes, bt.Any);
    defer vm.alloc.free(argsSigStr);

    return vm.panicFmt(
        \\Incompatible call arguments `{}`
        \\to the lambda `func {}`.
        , &.{
            v(argsSigStr), v(cstrFuncSigStr),
        },
    );
}

/// Runtime version of `sema.reportIncompatibleCallSig`.
fn panicIncompatibleCallSymSig(vm: *cy.VM, overload_entry: u32, args: []const Value) error{Panic, OutOfMemory} {
    const num_funcs = vm.overloaded_funcs.buf[overload_entry];
    const funcs = vm.overloaded_funcs.buf[overload_entry+1..overload_entry+1+num_funcs];

    // Use first func to determine name.
    const first_func = funcs[0];
    const first_func_details = vm.funcSymDetails.buf[first_func];
    const name = first_func_details.namePtr[0..first_func_details.nameLen];

        // vm.curFiber.panicType = vmc.PANIC_INFLIGHT_OOM;
        // return error.Panic;

    const arg_types = try allocValueTypeIds(vm, args);
    defer vm.alloc.free(arg_types);

    const call_args_str = try vm.compiler.sema.allocTypesStr(arg_types);
    defer vm.alloc.free(call_args_str);

    var msg: std.ArrayListUnmanaged(u8) = .{}; 
    defer msg.deinit(vm.alloc);
    const w = msg.writer(vm.alloc);
    try w.print("Can not find compatible function for call: `{s}{s}`.", .{name, call_args_str});
    // if (num_ret == 1) {
    //     try w.writeAll(" Expects non-void return.");
    // }
    try w.writeAll("\n");
    try w.print("Functions named `{s}`:\n", .{name});

    var funcStr = vm.compiler.sema.formatFuncSig(first_func_details.funcSigId, &cy.tempBuf) catch {
        return error.OutOfMemory;
    };
    try w.print("    func {s}{s}", .{name, funcStr});
    for (funcs[1..]) |func| {
        try w.writeByte('\n');
        const func_details = vm.funcSymDetails.buf[func];
        funcStr = vm.sema.formatFuncSig(func_details.funcSigId, &cy.tempBuf) catch {
            return error.OutOfMemory;
        };
        try w.print("    func {s}{s}", .{name, funcStr});
    }
    return vm.panic(msg.items);
}

fn panicIncompatibleMethodSig(
    vm: *cy.VM, method_id: rt.MethodId, recv: Value, args: []const Value
) error{Panic, OutOfMemory} {
    const typeId = recv.getTypeId();

    const typeIds = try allocMethodCallTypeIds(vm, typeId, args);
    defer vm.alloc.free(typeIds);

    // Lookup `func_entry`.
    const method = vm.methods.buf[method_id];
    const name_id = vm.method_names.buf[method_id];
    const name = rt.getName(vm, name_id);
    const typeName = vm.types[typeId].sym.name();
    var entry: u32 = undefined;
    var overloaded: bool = undefined;
    if (method.mru_type == typeId) {
        entry = method.mru_id;
        overloaded = method.mru_overloaded;
    } else {
        if (method.has_multiple_types) {
            const tm_id = vm.getTypeMethod(typeId, method_id) orelse {
                return vm.panicFmt("The method `{}` can not be found in `{}`.", &.{
                    v(name), v(typeName),
                });
            };
            const tmethod = vm.type_methods.buf[tm_id];
            entry = tmethod.id;
            overloaded = tmethod.overloaded;
        } else {
            return vm.panicFmt("The method `{}` can not be found in `{}`.", &.{
                v(name), v(typeName),
            });
        }
    }

    var msg: std.ArrayListUnmanaged(u8) = .{}; 
    defer msg.deinit(vm.alloc);
    const w = msg.writer(vm.alloc);
    const call_args_str = try vm.compiler.sema.allocTypesStr(typeIds[1..]);
    defer vm.alloc.free(call_args_str);

    try w.print("Can not find compatible method for call: `({s}) {s}{s}`.", .{typeName, name, call_args_str});
    try w.writeAll("\n");
    try w.print("Methods named `{s}`:\n", .{name});

    var funcs: []const rt.FuncId = undefined;
    if (overloaded) {
        const num_funcs = vm.overloaded_funcs.buf[entry];
        funcs = vm.overloaded_funcs.buf[entry+1..entry+1+num_funcs];
    } else {
        funcs = &.{entry};
    }
    for (funcs, 0..) |func_id, i| {
        const func = vm.funcSyms.buf[func_id];
        if (!func.is_method) {
            continue;
        }
        const funcStr = vm.sema.formatFuncSig(func.sig, &cy.tempBuf) catch {
            return error.OutOfMemory;
        };
        try w.print("    func {s}{s}", .{name, funcStr});
        if (i < funcs.len-1) {
            try w.writeByte('\n');
        }
    }    
    return vm.panic(msg.items);
}

fn getObjectFunctionFallback(
    vm: *VM, pc: [*]cy.Inst, recv: Value, typeId: u32, method: rt.MethodId, vals: []const Value,
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
            return panicIncompatibleMethodSig(vm, method, recv, vals);
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
    vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, recv: Value, typeId: u32, method: rt.MethodId, 
    ret: u8, numArgs: u8
) !cy.fiber.PcSp {
    @setCold(true);
    // const func = try @call(.never_inline, getObjectFunctionFallback, .{obj, symId});
    const vals = framePtr[ret+CallArgStart+1..ret+CallArgStart+1+numArgs-1];
    const func = try getObjectFunctionFallback(vm, pc, recv, typeId, method, vals);
    _ = func;
    return vm.panic("Missing method.");
}

pub inline fn buildReturnInfo2(cont: bool, comptime callInstOffset: u8) Value {
    return .{
        .val = 0 | (@as(u32, @intFromBool(!cont)) << 8) | (@as(u32, callInstOffset) << 16),
    };
}

pub inline fn buildReturnInfo(comptime cont: bool, comptime callInstOffset: u8) Value {
    return .{
        .val = 0 | (@as(u32, @intFromBool(!cont)) << 8) | (@as(u32, callInstOffset) << 16),
    };
}

pub inline fn buildReturnInfoForRoot(comptime cont: bool) Value {
    return .{
        .val = 0 | (@as(u32, @intFromBool(!cont)) << 8),
    };
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
                bt.List => fmt.printStdout("List {} len={}\n", &.{v(obj), v(obj.list.list.len)}),
                bt.Map => fmt.printStdout("Map {} size={}\n", &.{v(obj), v(obj.map.inner.size)}),
                bt.String => {
                    const str = obj.string.getSlice();
                    if (str.len > 20) {
                        fmt.printStdout("String {} len={} str=\"{}\"...\n", &.{v(obj), v(str.len), v(str[0..20])});
                    } else {
                        fmt.printStdout("String {} len={} str=\"{}\"\n", &.{v(obj), v(str.len), v(str)});
                    }
                },
                bt.Lambda => fmt.printStdout("Lambda {}\n", &.{v(obj)}),
                bt.Closure => fmt.printStdout("Closure {}\n", &.{v(obj)}),
                bt.Fiber => fmt.printStdout("Fiber {}\n", &.{v(obj)}),
                bt.HostFunc => fmt.printStdout("NativeFunc {}\n", &.{v(obj)}),
                else => {
                    const name = vm.compiler.sema.types.items[obj.getTypeId()].sym.name();
                    fmt.printStdout("HeapObject {} {} {}\n", &.{v(obj), v(obj.getTypeId()), v(name)});
                },
            }
        } else {
            fmt.printStdout("{}\n", &.{v(val.val)});
        }
    }
}

fn opMatch(pc: [*]const cy.Inst, framePtr: [*]const Value) u16 {
    const expr = framePtr[pc[1].val];
    const numCases = pc[2].val;
    var i: u32 = 0;
    while (i < numCases) : (i += 1) {
        const right = framePtr[pc[3 + i * 3].val];
        // Can immediately match numbers, objects, primitives.
        const cond = if (expr.val == right.val) true else 
            @call(.never_inline, evalCompareBool, .{expr, right});
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
    const dstSig = vm.compiler.sema.allocFuncSigStr(dstFuncSigId, true) catch fatal();
    const srcSig = vm.compiler.sema.allocFuncSigStr(srcFuncSigId, true) catch fatal();
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

fn getFuncSigIdOfSym(vm: *const VM, symId: SymbolId) sema.FuncSigId {
    switch (@as(rt.FuncSymbolType, @enumFromInt(vm.funcSyms.buf[symId].entryT))) {
        .hostFunc => {
            return vm.funcSyms.buf[symId].innerExtra.hostFunc.funcSigId;
        },
        .hostInlineFunc => {
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

pub const CallArgStart: u8 = vmc.CALL_ARG_START;
pub const CalleeStart: u8 = vmc.CALLEE_START;

/// Assumes args are compatible.
/// Like `callSym` except inlines different op codes.
fn callMethod(
    vm: *VM, pc: [*]cy.Inst, sp: [*]cy.Value, func: rt.FuncSymbol,
    typeId: cy.TypeId, ret: u8, nargs: u8,
) !?cy.fiber.PcSp {
    switch (func.type) {
        .func => {
            if (@intFromPtr(sp + ret + func.data.func.stackSize) >= @intFromPtr(vm.stackEndPtr)) {
                return error.StackOverflow;
            }

            // Optimize.
            // TODO: callObjFuncIC (rt args typecheck) or callObjFuncNoCheckIC.
            if (false) {
                pc[0] = cy.Inst.initOpCode(.callObjFuncIC);
                pc[7] = cy.Inst{ .val = @intCast(func.data.func.stackSize) };
                @as(*align(1) u32, @ptrCast(pc + 8)).* = func.data.func.pc;
                @as(*align(1) u16, @ptrCast(pc + 14)).* = @intCast(typeId);
            }

            const newFp = sp + ret;
            newFp[1] = buildReturnInfo(true, cy.bytecode.CallObjSymInstLen);
            newFp[2] = Value{ .retPcPtr = pc + cy.bytecode.CallObjSymInstLen };
            newFp[3] = Value{ .retFramePtr = sp };
            return cy.fiber.PcSp{
                .pc = cy.fiber.toVmPc(vm, func.data.func.pc),
                .sp = newFp,
            };
        },
        .host_func => {
            // Optimize.
            // TODO: callObjHostFuncIC (rt args typecheck) or callObjHostFuncNoCheckIC
            if (false) {
                pc[0] = cy.Inst.initOpCode(.callObjNativeFuncIC);
                @as(*align(1) u48, @ptrCast(pc + 8)).* = @intCast(@intFromPtr(func.data.hostFunc));
                @as(*align(1) u16, @ptrCast(pc + 14)).* = @intCast(typeId);
            }

            vm.pc = pc;
            vm.framePtr = sp;
            const res: Value = @bitCast(func.data.host_func.?(@ptrCast(vm), @ptrCast(sp + ret + CallArgStart), nargs));
            if (res.isInterrupt()) {
                return error.Panic;
            }
            sp[ret] = res;
            return cy.fiber.PcSp{
                .pc = pc + cy.bytecode.CallObjSymInstLen,
                .sp = sp,
            };
        },
        .null => {
            return vm.panic("Missing func");
        },
    }
}

inline fn deoptimizeBinOp(pc: [*]cy.Inst) void {
    pc[0] = cy.Inst.initOpCode(.callObjSym);
    pc[1] = pc[8];
    pc[2] = pc[9];
    pc[3] = pc[10];
    pc[4] = pc[11];
}

export fn zFatal() void {
    cy.fatal();
}

export fn zOpCodeName(code: vmc.OpCode) [*:0]const u8 {
    const ecode: cy.OpCode = @enumFromInt(code);
    return @tagName(ecode);
}

export fn zCallSym(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, symId: u16, ret: u8, numArgs: u8) callconv(.C) vmc.PcSpResult {
    const res = @call(.always_inline, VM.callSym, .{vm, pc, framePtr, @as(u32, @intCast(symId)), ret, numArgs}) catch |err| {
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

export fn zCallSymDyn(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, symId: u16, ret: u8, numArgs: u8) callconv(.C) vmc.PcSpResult {
    const res = @call(.always_inline, VM.callSymDyn, .{vm, pc, framePtr, @as(u32, @intCast(symId)), ret, numArgs}) catch |err| {
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

export fn zDumpEvalOp(vm: *VM, pc: [*]const cy.Inst) void {
    dumpEvalOp(vm, pc) catch cy.fatal();
}

pub export fn zFreeObject(vm: *cy.VM, obj: *HeapObject) void {
    cy.heap.freeObject(vm, obj, true, false, true);
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
    typeId: cy.TypeId, method: u16, ret: u8, numArgs: u8,
) vmc.CallObjSymResult {
    const args = stack[ret+CallArgStart+1..ret+CallArgStart+1+numArgs-1];
    if (vm.getCompatMethodFunc(typeId, method, args)) |func| {
        const mb_res = callMethod(vm, pc, stack, func, typeId, ret, numArgs) catch |err| {
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
    const res = @call(.never_inline, callObjSymFallback, .{vm, pc, stack, recv, typeId, method, ret, numArgs}) catch |err| {
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

export fn zAllocFiber(vm: *cy.VM, pc: u32, args: [*]const Value, nargs: u8, argDst: u8, initialStackSize: u8) vmc.ValueResult {
    const fiber = cy.fiber.allocFiber(vm, pc, args[0..nargs], argDst, initialStackSize) catch {
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

export fn zPopFiber(vm: *cy.VM, curFiberEndPc: usize, curStack: [*]Value, retValue: Value) vmc.PcSpOff {
    const fp = cy.fiber.getStackOffset(vm.stack.ptr, curStack);
    const res = cy.fiber.popFiber(vm, .{ .pc = @intCast(curFiberEndPc), .sp = fp }, retValue);
    return .{
        .pc = res.pc,
        .sp = res.sp,
    };
}

export fn zAllocObjectSmall(vm: *cy.VM, typeId: cy.TypeId, fields: [*]const Value, nfields: u8) vmc.ValueResult {
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

export fn zGetFieldOffsetFromTable(vm: *VM, typeId: cy.TypeId, symId: SymbolId) u8 {
    return vm.getFieldOffsetFromTable(typeId, symId);
}

export fn zEvalCompare(left: Value, right: Value) vmc.Value {
    return @bitCast(evalCompare(left, right));
}

export fn zEvalCompareNot(left: Value, right: Value) vmc.Value {
    return @bitCast(evalCompareNot(left, right));
}

export fn zCall(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, callee: Value, ret: u8, numArgs: u8) vmc.PcSpResult {
    const res = call(vm, pc, framePtr, callee, ret, numArgs, true) catch |err| {
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

pub export fn zAllocStringTemplate2(vm: *cy.VM, strs: [*]cy.Value, strCount: u8, vals: [*]Value, valCount: u8) vmc.ValueResult {
    const val = cy.heap.allocStringTemplate2(vm, strs[0..strCount], vals[0..valCount]) catch {
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
    var buf: [1024]u8 = undefined;
    var fbuf = std.io.fixedBufferStream(&buf);
    val.writeDump(fbuf.writer()) catch cy.fatal();
    cy.rt.log(fbuf.getWritten());
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

export fn zGetFieldFallback(vm: *VM, obj: *HeapObject, nameId: vmc.NameId) Value {
    return vm.getFieldFallback(obj, nameId) catch {
        return Value.Interrupt;
    };
}

export fn zSetFieldFallback(vm: *VM, obj: *HeapObject, nameId: vmc.NameId, val: cy.Value) vmc.ResultCode {
    vm.setFieldFallback(obj, nameId, val) catch |err| {
        if (err == error.Panic) {
            return vmc.RES_CODE_PANIC;
        } else {
            return vmc.RES_CODE_UNKNOWN;
        }
    };
    return vmc.RES_CODE_SUCCESS;
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

export fn zOpMatch(pc: [*]const cy.Inst, framePtr: [*]const Value) u16 {
    return opMatch(pc, framePtr);
}

export fn zLog(fmtz: [*:0]const u8, valsPtr: [*]const fmt.FmtValue, len: usize) void {
    const format = std.mem.sliceTo(fmtz, 0);
    const vals = valsPtr[0..len];
    rt.logFmt(format, vals);
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
    logger.tracev("{s}", .{msg});
}

export fn zMapSet(vm: *VM, map: *cy.heap.Map, key: Value, val: Value) vmc.ResultCode {
    map.set(vm, key, val) catch {
        return vmc.RES_CODE_UNKNOWN;
    };
    return vmc.RES_CODE_SUCCESS;
}

export fn zValueMapGet(map: *cy.ValueMap, key: Value, found: *bool) Value {
    if (map.get(key)) |val| {
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
    prev: ?*cy.heap.DListNode align(@alignOf(HeapObject)), // Ensure same alignment as a heap object.
    next: ?*cy.heap.DListNode,
    len: if (cy.Malloc == .zig) u64 else void,
    typeId: u32,
};
pub var dummyCyclableHead = DummyCyclableNode{
    .prev = null,
    .next = null,
    // This will be marked automatically before sweep, so it's never considered as a cyc object.
    .len = if (cy.Malloc == .zig) 0 else {},
    .typeId = vmc.GC_MARK_MASK | bt.Void,
};

pub fn defaultPrint(_: ?*cc.VM, _: cc.Str) callconv(.C) void {
    // Default is a nop.
}

pub fn defaultPrintError(_: ?*cc.VM, _: cc.Str) callconv(.C) void {
    // Default is a nop.
}

export fn zGetTypeName(vm: *VM, id: cy.TypeId) vmc.Str {
    if (cy.Trace) {
        if ((id & vmc.TYPE_MASK) == vmc.TYPE_MASK) {
            return vmc.Str{ .ptr = "DanglingObject", .len = "DanglingObject".len };
        }
        if (vm.types[id].kind == .null) {
            cy.panicFmt("Type `{}` is uninited.", .{ id });
        }
    }
    const name = vm.types[id].sym.name();
    return vmc.Str{
        .ptr = name.ptr,
        .len = name.len,
    };
}

export fn zEnsureListCap(vm: *VM, list: *cy.List(Value), cap: usize) vmc.ResultCode {
    list.ensureTotalCapacity(vm.alloc, cap) catch {
        return vmc.RES_CODE_UNKNOWN;
    };
    return vmc.RES_CODE_SUCCESS;
}

export fn zTraceRetain(vm: *VM, val: Value) void {
    _ = vm;
    _ = val;

    // if (val.isString()) {
    //     if (std.mem.eql(u8, val.asString(), "Color_S")) {
    //         const res = traceObjRetains.getOrPut(vm.alloc, vm.debugPc) catch cy.fatal();
    //         if (res.found_existing) {
    //             res.value_ptr.* += 1;
    //         } else {
    //             res.value_ptr.* = 0;
    //         }
    //     }
    // }
}

/// pc -> number of retains.
pub var traceObjRetains: std.AutoHashMapUnmanaged(u32, u32) = .{};

fn spawn(args: struct {
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    cwd: ?[]const u8 = null,
    cwd_dir: ?std.fs.Dir = null,
    env_map: ?*const std.process.EnvMap = null,
    max_output_bytes: usize = 50 * 1024,
    expand_arg0: std.os.Arg0Expand = .no_expand,
}) !std.ChildProcess.Term {
    var child = std.ChildProcess.init(args.argv, args.allocator);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;
    child.cwd = args.cwd;
    child.cwd_dir = args.cwd_dir;
    child.env_map = args.env_map;
    child.expand_arg0 = args.expand_arg0;

    try child.spawn();
    return try child.wait();
}