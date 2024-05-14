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

const VMC = extern struct {
    /// Program counter. Pointer to the current instruction data in `ops`.
    pc: [*]cy.Inst,

    /// Current stack frame ptr.
    framePtr: [*]Value,

    /// Value stack.
    stack: [*]Value,
    stack_len: usize,
    stackEndPtr: [*]const Value,

    ops: [*]cy.Inst,
    ops_len: usize,

    consts: [*]const Value,
    consts_len: usize,

    curFiber: *cy.Fiber,
    mainFiber: cy.Fiber,

    fieldSyms: [*]rt.FieldSymbolMap,
    fieldSyms_cap: usize,
    fieldSyms_len: usize,

    /// Static vars. 
    varSyms: [*]rt.VarSym,
    varSyms_cap: usize,
    varSyms_len: usize,

    tryStack: [*]vmc.TryFrame,
    tryStack_cap: usize,
    tryStack_len: usize,

    /// Types.
    types: [*]const types.Type,
    types_len: usize,

    trace: *vmc.TraceInfo,

    /// In debug mode, always save the current pc so tracing can be obtained.
    /// debugPc == NullId indicates execution has not started.
    debugPc: u32,

    refCounts: if (cy.TrackGlobalRC) usize else void,

    pub fn getTryStack(self: *VMC) *cy.List(vmc.TryFrame) {
        return @ptrCast(&self.tryStack);
    }

    pub fn getVarSyms(self: *VMC) *cy.List(rt.VarSym) {
        return @ptrCast(&self.varSyms);
    }

    pub fn getFieldSyms(self: *VMC) *cy.List(rt.FieldSymbolMap) {
        return @ptrCast(&self.fieldSyms);
    }

    pub fn getStack(self: *VMC) []Value {
        return @as(*[]Value, @ptrCast(&self.stack)).*;
    }
};

pub const VM = struct {
    alloc: std.mem.Allocator,

    c: VMC,

    /// Holds unique heap string interns (*Astring, *Ustring).
    /// By default, small strings (at most 64 bytes) are interned.
    strInterns: std.StringHashMapUnmanaged(*HeapObject),

    /// Object heap pages.
    heapPages: cy.List(*cy.heap.HeapPage),
    heapFreeHead: ?*HeapObject,
    /// Trace mode.
    heapFreeTail: ?*HeapObject,

    /// GC: Contains the head to the first cyclable object.
    /// Always contains one dummy node to avoid null checking.
    cyclableHead: if (cy.hasGC) *cy.heap.DListNode else void,

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

    /// Struct fields symbol table.
    fieldTable: std.HashMapUnmanaged(rt.FieldTableKey, FieldEntry, cy.hash.KeyU64Context, 80),
    fieldSymSignatures: std.StringHashMapUnmanaged(rt.FieldId),

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

    /// Records a minimal trace when walking the stack.
    /// Stack frames are then constructed from them.
    compactTrace: cy.List(vmc.CompactFrame),

    compiler: *cy.Compiler,
    sema: *cy.sema.Sema,

    /// User data ptr. Useful for embedders.
    userData: ?*anyopaque,

    /// Host write hook.
    print: cc.PrintFn,
    print_err: cc.PrintErrorFn,

    /// Object to pc of instruction that allocated it. Trace mode.
    objectTraceMap: std.AutoHashMapUnmanaged(*HeapObject, debug.ObjectTrace),

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

    // Trace mode.
    countFrees: bool,
    numFreed: u32,

    /// Whether this VM is already deinited. Used to skip the next deinit to avoid using undefined memory.
    deinited: bool,
    deinitedRtObjects: bool, 

    tempBuf: [128]u8 align(4),

    lastExeError: []const u8,
    last_res: cc.ResultCode,

    num_evals: u32,

    /// Save vm state at the end of execution so that a subsequent eval knows where it left off
    /// from a the previous bytecode buffer.
    num_cont_evals: u32,
    last_bc_len: u32,

    pub fn init(self: *VM, alloc: std.mem.Allocator) !void {
        self.* = .{
            .alloc = alloc,
            .compiler = undefined,
            .sema = undefined,
            .emptyString = undefined,
            .emptyArray = undefined,
            .strInterns = .{},
            .staticObjects = .{},
            .names = .{},
            .nameMap = .{},
            .heapPages = .{},
            .heapFreeHead = null,
            .heapFreeTail = if (cy.Trace) null else undefined,
            .cyclableHead = if (cy.hasGC) @ptrCast(&dummyCyclableHead) else {},
            .c = .{
                .pc = undefined,
                .framePtr = undefined,
                .stack = undefined,
                .stack_len = 0,
                .ops = undefined,
                .ops_len = 0,
                .consts = undefined,
                .consts_len = 0,
                .stackEndPtr = undefined,
                .varSyms = undefined,
                .varSyms_cap = 0,
                .varSyms_len = 0,
                .fieldSyms = undefined,
                .fieldSyms_cap = 0,
                .fieldSyms_len = 0,
                .tryStack = undefined,
                .tryStack_len = 0,
                .tryStack_cap = 0,
                .types = undefined,
                .types_len = 0,
                .trace = undefined,
                .refCounts = if (cy.TrackGlobalRC) 0 else undefined,
                .mainFiber = undefined,
                .curFiber = undefined,
                .debugPc = cy.NullId,
            },
            .methods = .{},
            .method_map = .{},
            .method_names = .{},
            .type_methods = .{},
            .type_method_map = .{},
            .funcSyms = .{},
            .funcSymDetails = .{},
            .overloaded_funcs = .{},
            .fieldTable = .{},
            .fieldSymSignatures = .{},
            .syms = .{},
            .symSignatures = .{},
            .inlineSaves = .{},
            .u8Buf = .{},
            .stackTrace = .{},
            .debugTable = undefined,
            .debugTempIndexTable = undefined,
            .unwindTempRegs = undefined,
            .unwindTempPrevIndexes = undefined,
            .compactTrace = .{},
            .endLocal = undefined,
            .objectTraceMap = .{},
            // Initialize to NullId to indicate vm is still in initing.
            .deinited = false,
            .deinitedRtObjects = false,
            .config = undefined,
            .httpClient = undefined,
            .stdHttpClient = undefined,
            .userData = null,
            .varSymExtras = .{},
            .print = defaultPrint,
            .print_err = defaultPrintError,
            .countFrees = false,
            .numFreed = 0,
            .tempBuf = undefined,
            .lastExeError = "",
            .last_res = cc.Success,
            .num_evals = 0,
            .num_cont_evals = 0,
            .last_bc_len = 0,
        };
        self.c.mainFiber.panicType = vmc.PANIC_NONE;
        self.c.curFiber = &self.c.mainFiber;
        self.compiler = try self.alloc.create(cy.Compiler);
        self.sema = &self.compiler.sema;
        try self.compiler.init(self);

        if (cy.hasCLI) {
            self.stdHttpClient = try alloc.create(http.StdHttpClient);
            self.stdHttpClient.* = http.StdHttpClient.init(self.alloc);
            self.httpClient = self.stdHttpClient.iface();
        }

        if (cy.Trace) {
            self.c.trace = try alloc.create(vmc.TraceInfo);
        }

        // Perform decently sized allocation for hot data paths since the allocator
        // will likely use a more consistent allocation.
        // Also try to allocate them in the same bucket.
        try cy.fiber.stackEnsureTotalCapacityPrecise(self, 511);

        try self.funcSyms.ensureTotalCapacityPrecise(self.alloc, 255);
        try self.methods.ensureTotalCapacityPrecise(self.alloc, 340);
        try self.method_map.ensureTotalCapacity(self.alloc, 200);
        std.debug.assert(cy.utils.getHashMapMemSize(rt.MethodKey, vmc.MethodId, self.method_map.capacity()) < 4096);

        try self.c.getFieldSyms().ensureTotalCapacityPrecise(alloc, 170);

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
        for (self.c.getVarSyms().items(), 0..) |vsym, i| {
            logger.tracevIf(cy.logMemory, "release varSym: {s}", .{self.varSymExtras.buf[i].name()});
            release(self, vsym.value);
        }
        self.c.getVarSyms().clearRetainingCapacity();

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

        cy.fiber.freeFiberPanic(self, &self.c.mainFiber);

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
            self.alloc.free(self.c.getStack());
            self.c.stack_len = 0;
        }

        if (reset) {
            self.c.getTryStack().clearRetainingCapacity();
            self.compactTrace.clearRetainingCapacity();
        } else {
            self.c.getTryStack().deinit(self.alloc);
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
            self.c.getVarSyms().clearRetainingCapacity();
        } else {
            self.c.getVarSyms().deinit(self.alloc);
        }

        if (reset) {
            self.c.getFieldSyms().clearRetainingCapacity();
            self.fieldTable.clearRetainingCapacity();
            self.fieldSymSignatures.clearRetainingCapacity();
        } else {
            self.c.getFieldSyms().deinit(self.alloc);
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

        self.c.types_len = 0;

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
                self.alloc.destroy(self.c.trace);
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

        for (self.names.items()) |name| {
            if (name.owned) {
                self.alloc.free(name.ptr[0..name.len]);
            }
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

    pub fn validate(self: *VM, srcUri: []const u8, src: ?[]const u8, config: cc.ValidateConfig) !void {
        var compile_c = cc.defaultCompileConfig();
        compile_c.file_modules = config.file_modules;
        compile_c.skip_codegen = true;
        _ = try self.compile(srcUri, src, compile_c);
    }

    pub fn compile(self: *VM, srcUri: []const u8, src: ?[]const u8, config: cc.CompileConfig) !cy.CompileResult {
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

    pub fn resetVM(self: *VM) !void {
        self.deinit(true);
        self.num_cont_evals = 0;

        // Reset flags for next reset/deinit.
        self.deinitedRtObjects = false;
        self.deinited = false;

        // Before reinit, everything in VM, VMcompiler should be cleared.

        try self.compiler.reinitPerRun();
        self.compiler.cont = false;
    }

    pub fn eval(self: *VM, src_uri: []const u8, src: ?[]const u8, config: cc.EvalConfig) !Value {
        var tt = cy.debug.timer();

        self.config = config;
        try self.compiler.reinitPerRun();

        var compile_c = cc.defaultCompileConfig();
        compile_c.single_run = config.single_run;
        compile_c.file_modules = config.file_modules;
        compile_c.gen_all_debug_syms = cy.Trace;
        compile_c.backend = config.backend;
        const res = try self.compiler.compile(src_uri, src, compile_c);
        tt.endPrint("compile");

        if (config.backend == cc.BackendJIT) {
            if (cy.isFreestanding or cy.isWasm) {
                return error.Unsupported;
            }

            const jitRes = res.jit.buf;
            try cy.fiber.stackEnsureTotalCapacity(self, res.jit.mainStackSize);
            self.c.framePtr = @ptrCast(self.c.stack);

            // Mark code executable.
            const PROT_READ = 1;
            const PROT_WRITE = 2;
            const PROT_EXEC = 4;
            try std.posix.mprotect(jitRes.buf.items.ptr[0..jitRes.buf.capacity], PROT_READ | PROT_EXEC);
            
            // Memory must be reset to original setting in order to be freed.
            defer std.posix.mprotect(jitRes.buf.items.ptr[0..jitRes.buf.capacity], PROT_WRITE) catch cy.fatal();

            if (jitRes.buf.items.len > 500*4) {
                logger.tracev("jit code (size: {}) {}...", .{jitRes.buf.items.len, std.fmt.fmtSliceHexLower(jitRes.buf.items[0..100*4])});
            } else {
                logger.tracev("jit code (size: {}) {}", .{jitRes.buf.items.len, std.fmt.fmtSliceHexLower(jitRes.buf.items)});
            }

            const bytes = jitRes.buf.items[jitRes.mainPc..jitRes.mainPc+12*4];
            logger.tracev("main start {}: {}", .{jitRes.mainPc, std.fmt.fmtSliceHexLower(bytes)});

            const main: *const fn(*VM, [*]Value) callconv(.C) void = @ptrCast(@alignCast(jitRes.buf.items.ptr + jitRes.mainPc));
            // @breakpoint();
            main(self, self.c.framePtr);
            return Value.initInt(0);
        } else if (config.backend == cc.BackendVM) {
            if (cc.verbose()) {
                try debug.dumpBytecode(self, .{});
            }

            if (cy.Trace) {
                var i: u32 = 0;
                while (i < self.c.trace.opCounts.len) : (i += 1) {
                    self.c.trace.opCounts[i] = .{
                        .code = i,
                        .count = 0,
                    };
                }
                self.c.trace.totalOpCounts = 0;
                self.c.trace.numReleases = 0;
                self.c.trace.numReleaseAttempts = 0;
                self.c.trace.numRetains = 0;
                self.c.trace.numRetainAttempts = 0;
                self.c.trace.numCycFrees = 0;
            }

            tt = cy.debug.timer();
            defer tt.endPrint("eval");

            return self.evalByteCode(res.vm);
        } else {
            defer res.aot.deinit(self.alloc);
            if (cy.isFreestanding or cy.isWasm or !cy.hasCLI) {
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
                const exeRes = try std.ChildProcess.run(.{
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
            return Value.Void;
        }
    }

    pub fn dumpStats(self: *const VM) void {
        const S = struct {
            fn opCountLess(_: void, a: vmc.OpCount, b: vmc.OpCount) bool {
                return a.count > b.count;
            }
        };
        std.debug.print("total ops evaled: {}\n", .{self.c.trace.totalOpCounts});
        std.sort.pdq(vmc.OpCount, &self.c.trace.opCounts, {}, S.opCountLess);
        var i: u32 = 0;

        while (i < vmc.NumCodes) : (i += 1) {
            if (self.c.trace.opCounts[i].count > 0) {
                const op = std.meta.intToEnum(cy.OpCode, self.c.trace.opCounts[i].code) catch continue;
                std.debug.print("\t{s} {}\n", .{@tagName(op), self.c.trace.opCounts[i].count});
            }
        }
    }

    pub fn dumpInfo(self: *VM) !void {
        fmt.printStderr("stack size: {}\n", &.{v(self.c.stack_len)});
        fmt.printStderr("stack framePtr: {}\n", &.{v(getStackOffset(self, self.c.framePtr))});
        fmt.printStderr("heap pages: {}\n", &.{v(self.heapPages.len)});
        if (cy.TrackGlobalRC) {
            fmt.printStderr("global rc: {}\n", &.{v(self.c.refCounts)});
        }

        // Dump func symbols.
        {
            fmt.printStderr("func syms:\n", &.{});
            for (self.funcSyms.items(), 0..) |_, i| {
                const details = self.funcSymDetails.buf[i];
                const name = details.namePtr[0..details.nameLen];
                const sigStr = try self.sema.formatFuncSig(details.funcSigId, &cy.tempBuf, null);
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
            .pc = cy.fiber.getInstOffset(vm.c.ops, vm.c.pc),
            .sp = cy.fiber.getStackOffset(vm.c.stack, vm.c.framePtr),
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

        cy.fiber.freeFiberPanic(self, &self.c.mainFiber);
        self.c.curFiber.panicType = vmc.PANIC_NONE;
        self.debugTable = buf.debugTable.items;
        self.debugTempIndexTable = buf.debugTempIndexTable.items;
        self.unwindTempRegs = buf.unwindTempRegs.items;
        self.unwindTempPrevIndexes = buf.unwindTempPrevIndexes.items;

        // Set these last to hint location to cache before eval.
        if (self.num_cont_evals > 0) {
            self.c.pc = @ptrCast(&buf.ops.items[self.last_bc_len]);
        } else {
            self.c.pc = @ptrCast(buf.ops.items.ptr);
        }
        try cy.fiber.stackEnsureTotalCapacity(self, buf.mainStackSize);
        self.c.framePtr = @ptrCast(self.c.stack);

        self.c.ops = buf.ops.items.ptr;
        self.c.ops_len = buf.ops.items.len;
        self.c.consts = buf.mconsts.ptr;
        self.c.consts_len = buf.mconsts.len;
        self.c.types = self.compiler.sema.types.items.ptr;
        self.c.types_len = self.compiler.sema.types.items.len;

        defer {
            self.num_cont_evals += 1;
            self.num_evals += 1;
            self.last_bc_len = @intCast(self.c.ops_len);
        }
        try @call(.never_inline, evalLoopGrowStack, .{self, true});
        logger.tracev("main stack size: {}", .{buf.mainStackSize});

        if (self.endLocal == 255) {
            return Value.Void;
        } else {
            return self.c.stack[self.endLocal];
        }
    }

    /// Assumes `vm.pc` is at a call inst before entering host code.
    fn getNewCallFuncRet(vm: *VM) u8 {
        const fp = cy.fiber.getStackOffset(vm.c.stack, vm.c.framePtr);
        if (fp == 0 or cy.fiber.isVmFrame(vm, vm.c.getStack(), fp)) {
            // Vm frame. Obtain current stack top from looking at the current inst.
            switch (vm.c.pc[0].opcode()) {
                .callSym,
                .callNativeFuncIC,
                .call,
                .callObjNativeFuncIC,
                .callObjSym => {
                    const ret = vm.c.pc[1];
                    const numArgs = vm.c.pc[2];
                    return ret.val + CallArgStart + numArgs.val;
                },
                .fieldDyn => {
                    return vm.c.pc[8].val;
                },
                .setFieldDyn => {
                    return vm.c.pc[10].val;
                },
                else => {
                    cy.panicFmt("TODO: getNewCallFuncFp {}", .{vm.c.pc[0].opcode()});
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
        const fpOff = cy.fiber.getStackOffset(vm.c.stack, vm.c.framePtr);
        const ret = vm.getNewCallFuncRet();

        const pc = vm.c.pc;

        // Since the compiler isn't aware of a re-entry call, it can not predetermine the stack size required
        // for the arguments.
        try cy.fiber.ensureTotalStackCapacity(vm, fpOff + ret + CallArgStart + args.len);
        // Should the stack grow, update pointer.
        vm.c.framePtr = vm.c.stack + fpOff;

        var call_ret: u8 = undefined;
        if (config.from_external) {
            // Create a new host frame so that it can be reported.
            const retInfo = buildReturnInfoForRoot(false);
            const retFramePtr = Value{ .retFramePtr = vm.c.framePtr };
            vm.c.framePtr += ret;

            // Reserve extra slot for info about the call.
            vm.c.framePtr[0].val = func.getTypeId();
            vm.c.framePtr += 1;

            vm.c.framePtr[1] = retInfo;
            // Return pc points to the call inst that entered host.
            vm.c.framePtr[2] = Value{ .retPcPtr = pc };
            vm.c.framePtr[3] = retFramePtr;
            call_ret = 4;
        } else {
            call_ret = ret;
        }

        // Copy callee + args to a new blank frame.
        vm.c.framePtr[call_ret + CalleeStart] = func;
        @memcpy(vm.c.framePtr[call_ret+CallArgStart..call_ret+CallArgStart+args.len], args);

        // Pass in an arbitrary `pc` to reuse the same `call` used by the VM.
        const pcsp = try call(vm, vm.c.pc, vm.c.framePtr, func, call_ret, @intCast(args.len), false);
        vm.c.framePtr = pcsp.sp;
        vm.c.pc = pcsp.pc;

        // Only user funcs start eval loop.
        if (!func.isObjectType(bt.HostFunc)) {
            if (config.from_external) {
                @call(.never_inline, evalLoopGrowStack, .{vm, true}) catch |err| {
                    if (err == error.Panic) {
                        // Dump for now.
                        const frames = try cy.debug.allocStackTrace(vm, vm.c.getStack(), vm.compactTrace.items());
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
        vm.c.framePtr = vm.c.stack + fpOff;
        vm.c.pc = pc;
        if (config.from_external) {
            return vm.c.framePtr[ret + 1 + 4];
        } else {
            return vm.c.framePtr[ret];
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
        const sym = c.createObjectType(parent, name, null) catch return error.Unexpected;
        try sema.resolveObjectTypeId2(c, sym, null);
        sym.head.setNameOwned(true);

        const infos = try c.alloc.alloc(cy.sym.FieldInfo, fields.len);
        for (fields, 0..) |field, i| {
            const field_sym = c.declareField(@ptrCast(sym), field, @intCast(i), bt.Any, null) catch return error.Unexpected;
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
        self.c.types = c.sema.types.items.ptr;
        self.c.types_len = c.sema.types.items.len;
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
            const id: u32 = @intCast(self.c.fieldSyms_len);
            try self.c.getFieldSyms().append(self.alloc, .{
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
        const sym = &self.c.fieldSyms[fieldId];
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
        self.c.curFiber.panicPayload = Value.initErrorSymbol(@intFromEnum(sym)).val;
        self.c.curFiber.panicType = vmc.PANIC_NATIVE_THROW;
        return error.Panic;
    }

    pub fn panicWithUncaughtError(self: *VM, err: Value) error{Panic} {
        @setCold(true);
        self.c.curFiber.panicPayload = err.val;
        self.c.curFiber.panicType = vmc.PANIC_UNCAUGHT_ERROR;
        return error.Panic;
    }

    fn panic(self: *VM, msg: []const u8) error{Panic, OutOfMemory} {
        @setCold(true);
        const dupe = try self.alloc.dupe(u8, msg);
        self.c.curFiber.panicPayload = @as(u64, @intFromPtr(dupe.ptr)) | (@as(u64, dupe.len) << 48);
        self.c.curFiber.panicType = vmc.PANIC_MSG;
        logger.tracev("{s}", .{dupe});
        return error.Panic;
    }

    fn panicFmt(self: *VM, format: []const u8, args: []const fmt.FmtValue) error{Panic} {
        @setCold(true);
        const msg = fmt.allocFormat(self.alloc, format, args) catch |err| {
            if (err == error.OutOfMemory) {
                self.c.curFiber.panicType = vmc.PANIC_INFLIGHT_OOM;
                return error.Panic;
            } else {
                cy.panic("unexpected");
            }
        };
        self.c.curFiber.panicPayload = @as(u64, @intFromPtr(msg.ptr)) | (@as(u64, msg.len) << 48);
        self.c.curFiber.panicType = vmc.PANIC_MSG;
        logger.tracev("{s}", .{msg});
        return error.Panic;
    }

    fn setField(self: *VM, recv: Value, fieldId: SymbolId, val: Value) !void {
        if (recv.isPointer()) {
            const obj = recv.asHeapObject();
            const symMap = &self.c.fieldSyms.buf[fieldId];

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
            const sym = &self.c.fieldSyms[symId];
            sym.mruTypeId = typeId;
            sym.mruOffset = res.offset;
            sym.mruFieldTypeSymId = res.typeId;
            return @intCast(res.offset);
        } else {
            return cy.NullU8;
        }
    }

    pub fn getFieldOffset(self: *VM, obj: *HeapObject, symId: SymbolId) u8 {
        const symMap = self.c.fieldSyms[symId];
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
                const sym = self.c.fieldSyms.buf[symId];
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
                const sym = self.c.fieldSyms[symId];
                return self.getFieldFallback(obj, sym.nameId);
            }
        } else {
            return self.getFieldMissingSymbolError();
        }
    }

    fn setFieldFallback(self: *VM, obj: *HeapObject, nameId: vmc.NameId, val: cy.Value) !void {
        const name = rt.getName(self, nameId);
        const rec_t = obj.getTypeId();
        if (!self.c.types[rec_t].has_set_method) {
            _ = self.prepPanic("Missing field in object.");
            return error.Panic;
        }
        const rec_arg = Value.initPtr(obj);
        const name_arg = try self.allocString(name);
        defer self.release(name_arg);

        var args: [3]Value = .{rec_arg, name_arg, val};
        const func = self.getCompatMethodFunc(rec_t, self.compiler.setMID, args[1..]) orelse {
            return error.Unexpected;
        };
        const func_val = try cy.heap.allocFuncFromSym(self, func);
        defer self.release(func_val);
        const result = try self.callFunc(func_val, &args, .{ .from_external = false });
        if (result.isInterrupt()) {
            return error.Panic;
        }
    }

    fn getFieldFallback(self: *VM, obj: *HeapObject, nameId: vmc.NameId) !Value {
        const name = rt.getName(self, nameId);
        const rec_t = obj.getTypeId();
        if (!self.c.types[rec_t].has_get_method) {
            return self.prepPanic("Missing field in object.");
        }

        const rec_arg = Value.initPtr(obj);
        const name_arg = try self.allocString(name);
        defer self.release(name_arg);

        var args: [2]Value = .{rec_arg, name_arg};
        const func = self.getCompatMethodFunc(rec_t, self.compiler.getMID, args[1..]) orelse {
            return error.Unexpected;
        };

        const func_val = try cy.heap.allocFuncFromSym(self, func);
        defer self.release(func_val);
        const result = try self.callFunc(func_val, &args, .{ .from_external = false });
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

                    self.c.pc = pc;
                    self.c.framePtr = fp;
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
                    if (@intFromPtr(fp + ret + func.data.func.stackSize) >= @intFromPtr(self.c.stackEndPtr)) {
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

                self.c.pc = pc;
                self.c.framePtr = framePtr;

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
                if (@intFromPtr(framePtr + ret + func.data.func.stackSize) >= @intFromPtr(self.c.stackEndPtr)) {
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
    fn isMethodFuncCompat(self: *VM, func: rt.FuncSymbol, args: []Value) bool {
        // Perform type check on args.
        const target = self.compiler.sema.getFuncSig(func.sig);
        const target_params = target.params()[1..];

        if (target_params.len != args.len) {
            return false;
        }

        for (args, 0..) |arg, i| {
            const target_t = target_params[i];
            const arg_t = arg.getTypeId();
            if (!types.isTypeSymCompat(self.compiler, arg_t, target_t)) {
                if (!@call(.never_inline, inferArg, .{self, args, i, arg, target_t})) {
                    return false;
                }
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
    fn getCompatMethodFunc(self: *VM, rec_t: cy.TypeId, method_id: rt.MethodId, args: []cy.Value) ?rt.FuncSymbol {
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
        const w = fbuf.writer();

        if (val.isString()) {
            return val.asString();
        } else {
            _ = try self.writeValue(w, val);
        }
        return fbuf.getWritten();
    }

    pub fn getOrBufPrintValueStr2(self: *const VM, buf: []u8, val: Value, out_ascii: *bool) ![]const u8 {
        var fbuf = std.io.fixedBufferStream(buf);
        const w = fbuf.writer();

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
                        try std.fmt.format(w, "{d:.1}", .{f});
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
                try std.fmt.format(w, "symbol.{s}", .{self.getSymbolName(litId)});
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
                const sym = self.c.types[typeId].sym;
                const enumv = val.getEnumValue();
                const name = sym.cast(.enum_t).getValueSym(enumv).head.name();
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
            bt.ListDyn => {
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
        vm.c.curFiber.panicPayload = @as(u64, @intCast(@intFromPtr(dupe.ptr))) | (@as(u64, dupe.len) << 48);
        vm.c.curFiber.panicType = vmc.PANIC_MSG;
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
        return @intCast(cy.fiber.getStackOffset(self.c.stack, args + nargs));
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
    return vm.panicFmt("Cannot convert `{}` to float.", &.{v(vm.c.types[typeId].name)});
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
    // inline for (std.meta.fields(VM)) |field| {
    //     std.debug.print("{s} {}\n", .{field.name, @offsetOf(VM, field.name)});
    // }

    try t.eq(@offsetOf(VMC, "pc"), @offsetOf(vmc.VMC, "curPc"));
    try t.eq(@offsetOf(VMC, "framePtr"), @offsetOf(vmc.VMC, "curStack"));
    try t.eq(@offsetOf(VMC, "stack"), @offsetOf(vmc.VMC, "stackPtr"));
    try t.eq(@offsetOf(VMC, "stackEndPtr"), @offsetOf(vmc.VMC, "stackEndPtr"));
    try t.eq(@offsetOf(VMC, "ops"), @offsetOf(vmc.VMC, "instPtr"));
    try t.eq(@offsetOf(VMC, "consts"), @offsetOf(vmc.VMC, "constPtr"));
    try t.eq(@offsetOf(VMC, "tryStack"), @offsetOf(vmc.VMC, "tryStack"));
    try t.eq(@offsetOf(VMC, "varSyms"), @offsetOf(vmc.VMC, "varSyms"));
    try t.eq(@offsetOf(VMC, "fieldSyms"), @offsetOf(vmc.VMC, "fieldSyms"));
    try t.eq(@offsetOf(VMC, "curFiber"), @offsetOf(vmc.VMC, "curFiber"));
    try t.eq(@offsetOf(VMC, "mainFiber"), @offsetOf(vmc.VMC, "mainFiber"));
    try t.eq(@offsetOf(VMC, "types"), @offsetOf(vmc.VMC, "typesPtr"));
    if (cy.TrackGlobalRC) {
        try t.eq(@offsetOf(VMC, "refCounts"), @offsetOf(vmc.VMC, "refCounts"));
    }
    try t.eq(@offsetOf(VMC, "trace"), @offsetOf(vmc.VMC, "trace"));
    try t.eq(@offsetOf(VMC, "debugPc"), @offsetOf(vmc.VMC, "debugPc"));
    try t.eq(@offsetOf(VM, "c"), @offsetOf(vmc.VM, "c"));
}

pub const SymbolId = u32;

const Root = @This();

/// If successful, execution should continue.
pub fn handleInterrupt(vm: *VM, rootFp: u32) !void {
    if (vm.c.curFiber.panicType == vmc.PANIC_NATIVE_THROW) {
        const res = try @call(.never_inline, cy.fiber.throw, .{
            vm, rootFp, vm.getFiberContext(), Value.initRaw(vm.c.curFiber.panicPayload) });
        vm.c.pc = vm.c.ops + res.pc;
        vm.c.framePtr = vm.c.stack + res.sp;
    } else {
        const res = try @call(.never_inline, panicCurFiber, .{ vm });
        vm.c.pc = vm.c.ops + res.pc;
        vm.c.framePtr = vm.c.stack + res.sp;
    }
}

/// To reduce the amount of code inlined in the hot loop, handle StackOverflow at the top and resume execution.
/// This is also the entry way for native code to call into the VM, assuming pc, framePtr, and virtual registers are already set.
pub fn evalLoopGrowStack(vm: *VM, handle_panic: bool) error{StackOverflow, OutOfMemory, Panic, NoDebugSym, Unexpected, End}!void {
    logger.tracev("begin eval loop", .{});

    // Record the start fp offset, so that stack unwinding knows when to stop.
    // This is useful for having nested eval stacks.
    const startFp = cy.fiber.getStackOffset(vm.c.stack, vm.c.framePtr);

    if (comptime build_options.vmEngine == .zig) {
        return error.Unsupported;
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
        vm.c.pc = vm.c.ops + cont.pc;
        vm.c.framePtr = vm.c.stack + cont.sp;
    }
}

fn panicCurFiber(vm: *VM) !cy.fiber.PcSpOff {
    var ctx = vm.getFiberContext();
    ctx = try cy.fiber.unwindStack(vm, vm.c.getStack(), ctx);

    const frames = try debug.allocStackTrace(vm, vm.c.getStack(), vm.compactTrace.items());
    vm.stackTrace.deinit(vm.alloc);
    vm.stackTrace.frames = frames;

    return cy.fiber.fiberEnd(vm, ctx) orelse {
        return error.Panic;
    };
}

/// Generate assembly labels to find sections easier.
const GenLabels = builtin.mode != .Debug and !builtin.cpu.arch.isWasm() and false;

const DebugTraceStopAtNumOps: ?u32 = null;

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
            const val = Value{ .val = vm.c.consts[idx].val };
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
            const sym = vm.c.fieldSyms[symId];
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

fn inferArg(vm: *VM, args: []Value, arg_idx: usize, arg: Value, target_t: cy.TypeId) bool {
    if (arg.getTypeId() == bt.TagLit) {
        const type_e = vm.c.types[target_t];
        if (type_e.kind == .@"enum") {
            const name = vm.syms.buf[arg.asSymbolId()].name;
            if (type_e.sym.cast(.enum_t).getMemberTag(name)) |value| {
                args[arg_idx] = Value.initEnum(target_t, @intCast(value));
                return true;
            }
        } else if (target_t == bt.Symbol) {
            args[arg_idx] = Value.initSymbol(arg.asSymbolId());
            return true;
        }
    }
    return false;
}

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
                            if (!@call(.never_inline, inferArg, .{vm, args, i, arg, cstrType})) {
                                return panicIncompatibleLambdaSig(vm, args, obj.closure.funcSigId);
                            }
                        }
                    }
                }

                if (@intFromPtr(framePtr + ret + obj.closure.stackSize) >= @intFromPtr(vm.c.stackEndPtr)) {
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
                            if (!@call(.never_inline, inferArg, .{vm, args, i, arg, cstrType})) {
                                return panicIncompatibleLambdaSig(vm, args, obj.lambda.funcSigId);
                            }
                        }
                    }
                }

                if (@intFromPtr(framePtr + ret + obj.lambda.stackSize) >= @intFromPtr(vm.c.stackEndPtr)) {
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
                            if (!@call(.never_inline, inferArg, .{vm, args, i, arg, cstrType})) {
                                return panicIncompatibleLambdaSig(vm, args, obj.hostFunc.funcSigId);
                            }
                        }
                    }
                }

                vm.c.pc = pc;
                vm.c.framePtr = framePtr;
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
         v(vm.c.types.buf[val.getTypeId()].name), v(name), 
    });
}

fn panicCastError(vm: *cy.VM, val: Value, expTypeId: cy.TypeId) !void {
    return vm.panicFmt("Can not cast `{}` to `{}`.", &.{
         v(vm.c.types.buf[val.getTypeId()].name), v(vm.c.types.buf[expTypeId].name), 
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
    const rightSemaTypeId = vm.c.types[rightTypeId].rTypeSymId;
    const rightTypeName = sema.getSymName(&vm.compiler, rightSemaTypeId);
    return vm.panicFmt(
        \\Assigning to `{}` field with incompatible type `{}`.
        , &.{
            v(fieldTypeName), v(rightTypeName),
        },
    );
}

fn panicIncompatibleLambdaSig(vm: *cy.VM, args: []const Value, cstrFuncSigId: sema.FuncSigId) error{Panic, OutOfMemory} {
    const cstrFuncSigStr = vm.compiler.sema.allocFuncSigStr(cstrFuncSigId, true, null) catch return error.OutOfMemory;
    defer vm.alloc.free(cstrFuncSigStr);
    const argTypes = try allocValueTypeIds(vm, args);
    defer vm.alloc.free(argTypes);
    const argsSigStr = vm.compiler.sema.allocFuncSigTypesStr(argTypes, bt.Any, null) catch return error.OutOfMemory;
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

    const call_args_str = vm.compiler.sema.allocTypesStr(arg_types, null) catch return error.OutOfMemory;
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

    var funcStr = vm.compiler.sema.formatFuncSig(first_func_details.funcSigId, &cy.tempBuf, null) catch {
        return error.OutOfMemory;
    };
    try w.print("    func {s}{s}", .{name, funcStr});
    for (funcs[1..]) |func| {
        try w.writeByte('\n');
        const func_details = vm.funcSymDetails.buf[func];
        funcStr = vm.sema.formatFuncSig(func_details.funcSigId, &cy.tempBuf, null) catch return error.OutOfMemory;
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
    const typeName = vm.c.types[typeId].sym.name();
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
    const call_args_str = vm.compiler.sema.allocTypesStr(typeIds[1..], null) catch return error.OutOfMemory;
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
        const funcStr = vm.sema.formatFuncSig(func.sig, &cy.tempBuf, null) catch {
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
    return @intCast(cy.fiber.getInstOffset(vm.c.ops, to));
}

pub inline fn getStackOffset(vm: *const VM, to: [*]const Value) u32 {
    return @intCast(cy.fiber.getStackOffset(vm.c.stack, to));
}

/// Like Value.dump but shows heap values.
pub fn dumpValue(vm: *const VM, val: Value) void {
    if (val.isFloat()) {
        fmt.printStdout("Float {}\n", &.{ v(val.asF64()) });
    } else {
        if (val.isPointer()) {
            const obj = val.asHeapObject();
            switch (obj.getTypeId()) {
                bt.ListDyn => fmt.printStdout("List {} len={}\n", &.{v(obj), v(obj.list.list.len)}),
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
        if (srcParam == bt.Any and dstParam == bt.Dyn) {
            continue;
        }
        if (srcParam == bt.Dyn and dstParam == bt.Any) {
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
            if (@intFromPtr(sp + ret + func.data.func.stackSize) >= @intFromPtr(vm.c.stackEndPtr)) {
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

            vm.c.pc = pc;
            vm.c.framePtr = sp;
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

fn zFatal() callconv(.C) void {
    cy.fatal();
}

fn zOpCodeName(code: vmc.OpCode) callconv(.C) [*:0]const u8 {
    const ecode: cy.OpCode = @enumFromInt(code);
    return @tagName(ecode);
}

fn zCallSym(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, symId: u16, ret: u8, numArgs: u8) callconv(.C) vmc.PcSpResult {
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

fn zCallSymDyn(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, symId: u16, ret: u8, numArgs: u8) callconv(.C) vmc.PcSpResult {
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

fn zDumpEvalOp(vm: *VM, pc: [*]const cy.Inst) callconv(.C) void {
    dumpEvalOp(vm, pc) catch cy.fatal();
}

pub fn zFreeObject(vm: *cy.VM, obj: *HeapObject) callconv(.C) void {
    cy.heap.freeObject(vm, obj, true, false, true);
} 

fn zEnd(vm: *cy.VM, pc: [*]const cy.Inst) callconv(.C) void {
    vm.endLocal = pc[1].val;
    vm.c.curFiber.pcOffset = @intCast(getInstOffset(vm, pc + 2));
}

fn zAllocFuncFromSym(vm: *cy.VM, id: u16) callconv(.C) vmc.ValueResult {
    const func = cy.heap.allocFuncFromSym(vm, vm.funcSyms.buf[id]) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(func),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

fn zAllocList(vm: *cy.VM, type_id: cy.TypeId, elemStart: [*]const Value, nElems: u8) callconv(.C) vmc.ValueResult {
    const list = cy.heap.allocList(vm, type_id, elemStart[0..nElems]) catch {
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

fn zAllocListDyn(vm: *cy.VM, elemStart: [*]const Value, nElems: u8) callconv(.C) vmc.ValueResult {
    const list = cy.heap.allocListDyn(vm, elemStart[0..nElems]) catch {
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

fn zOtherToF64(val: Value) callconv(.C) f64 {
    return val.otherToF64() catch fatal();
}

fn zCallObjSym(
    vm: *cy.VM, pc: [*]cy.Inst, stack: [*]Value, recv: Value,
    typeId: cy.TypeId, method: u16, ret: u8, numArgs: u8,
) callconv(.C) vmc.CallObjSymResult {
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

fn zAllocFiber(vm: *cy.VM, pc: u32, args: [*]const Value, nargs: u8, argDst: u8, initialStackSize: u8) callconv(.C) vmc.ValueResult {
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

fn zPushFiber(vm: *cy.VM, curFiberEndPc: usize, curStack: [*]Value, fiber: *cy.Fiber, parentDstLocal: u8) callconv(.C) vmc.PcSp {
    const res = cy.fiber.pushFiber(vm, curFiberEndPc, curStack, fiber, parentDstLocal);
    return .{
        .pc = @ptrCast(res.pc),
        .sp = @ptrCast(res.sp),
    };
}

fn zPopFiber(vm: *cy.VM, curFiberEndPc: usize, curStack: [*]Value, retValue: Value) callconv(.C) vmc.PcSpOff {
    const fp = cy.fiber.getStackOffset(vm.c.stack, curStack);
    const res = cy.fiber.popFiber(vm, .{ .pc = @intCast(curFiberEndPc), .sp = fp }, retValue);
    return .{
        .pc = res.pc,
        .sp = res.sp,
    };
}

fn zAllocObjectSmall(vm: *cy.VM, typeId: cy.TypeId, fields: [*]const Value, nfields: u8) callconv(.C) vmc.ValueResult {
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

fn zGetFieldOffsetFromTable(vm: *VM, typeId: cy.TypeId, symId: SymbolId) callconv(.C) u8 {
    return vm.getFieldOffsetFromTable(typeId, symId);
}

fn zEvalCompare(left: Value, right: Value) callconv(.C) vmc.Value {
    return @bitCast(evalCompare(left, right));
}

fn zEvalCompareNot(left: Value, right: Value) callconv(.C) vmc.Value {
    return @bitCast(evalCompareNot(left, right));
}

fn zCall(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, callee: Value, ret: u8, numArgs: u8) callconv(.C) vmc.PcSpResult {
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

fn zAllocPoolObject(vm: *cy.VM) callconv(.C) vmc.HeapObjectResult {
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

fn zAllocExternalCycObject(vm: *cy.VM, size: usize) callconv(.C) vmc.HeapObjectResult {
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

fn zAllocExternalObject(vm: *cy.VM, size: usize) callconv(.C) vmc.HeapObjectResult {
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

fn zAllocStringTemplate(vm: *cy.VM, strs: [*]cy.Inst, strCount: u8, vals: [*]Value, valCount: u8) callconv(.C) vmc.ValueResult {
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

pub fn zAllocStringTemplate2(vm: *cy.VM, strs: [*]cy.Value, strCount: u8, vals: [*]Value, valCount: u8) callconv(.C) vmc.ValueResult {
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

fn zDumpValue(vm: *VM, val: Value) callconv(.C) void {
    var buf: [1024]u8 = undefined;
    var fbuf = std.io.fixedBufferStream(&buf);
    cy.debug.dumpValue(vm, fbuf.writer(), val, .{}) catch cy.fatal();
    cy.rt.log(fbuf.getWritten());
}

fn zGetFieldFallback(vm: *VM, obj: *HeapObject, nameId: vmc.NameId) callconv(.C) Value {
    return vm.getFieldFallback(obj, nameId) catch {
        return Value.Interrupt;
    };
}

fn zSetFieldFallback(vm: *VM, obj: *HeapObject, nameId: vmc.NameId, val: cy.Value) callconv(.C) vmc.ResultCode {
    vm.setFieldFallback(obj, nameId, val) catch |err| {
        if (err == error.Panic) {
            return vmc.RES_CODE_PANIC;
        } else {
            return vmc.RES_CODE_UNKNOWN;
        }
    };
    return vmc.RES_CODE_SUCCESS;
}

fn zAlloc(alloc: vmc.ZAllocator, n: usize) callconv(.C) vmc.BufferResult {
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

fn zGrowTryStackTotalCapacity(vm: *cy.VM) callconv(.C) vmc.ResultCode {
    vm.c.getTryStack().growTotalCapacity(vm.alloc, vm.c.tryStack_len + 1) catch {
        return vmc.RES_CODE_UNKNOWN;
    };
    return vmc.RES_CODE_SUCCESS;
}

fn zOpMatch(pc: [*]const cy.Inst, framePtr: [*]const Value) callconv(.C) u16 {
    return opMatch(pc, framePtr);
}

fn zLog(fmtz: [*:0]const u8, valsPtr: [*]const fmt.FmtValue, len: usize) callconv(.C) void {
    const format = std.mem.sliceTo(fmtz, 0);
    const vals = valsPtr[0..len];
    rt.logFmt(format, vals);
}

fn zCheckDoubleFree(vm: *cy.VM, obj: *cy.HeapObject) callconv(.C) void {
    if (cy.Trace) {
        cy.arc.checkDoubleFree(vm, obj);
    }
}

fn zCheckRetainDanglingPointer(vm: *cy.VM, obj: *cy.HeapObject) callconv(.C) void {
    if (cy.Trace) {
        cy.arc.checkRetainDanglingPointer(vm, obj);
    }
}

fn zPanicFmt(vm: *VM, formatz: [*:0]const u8, argsPtr: [*]const fmt.FmtValue, numArgs: usize) callconv(.C) void {
    const format = formatz[0..std.mem.sliceTo(formatz, 0).len];
    const args = argsPtr[0..numArgs];

    const msg = fmt.allocFormat(vm.alloc, format, args) catch |err| {
        if (err == error.OutOfMemory) {
            vm.c.curFiber.panicType = vmc.PANIC_INFLIGHT_OOM;
            return;
        } else {
            cy.panic("unexpected");
        }
    };
    vm.c.curFiber.panicPayload = @as(u64, @intFromPtr(msg.ptr)) | (@as(u64, msg.len) << 48);
    vm.c.curFiber.panicType = vmc.PANIC_MSG;
    logger.tracev("{s}", .{msg});
}

fn zMapSet(vm: *VM, map: *cy.heap.Map, key: Value, val: Value) callconv(.C) vmc.ResultCode {
    map.set(vm, key, val) catch {
        return vmc.RES_CODE_UNKNOWN;
    };
    return vmc.RES_CODE_SUCCESS;
}

fn zValueMapGet(map: *cy.ValueMap, key: Value, found: *bool) callconv(.C) Value {
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
        @export(c_strlen, .{ .name = "strlen", .linkage = .strong });
        @export(c_pow, .{ .name = "pow", .linkage = .strong });
    }

    if (build_options.export_vmz) {
        @export(zPopFiber, .{ .name = "zPopFiber", .linkage = .strong });
        @export(zPushFiber, .{ .name = "zPushFiber", .linkage = .strong });
        @export(zGetFieldFallback, .{ .name = "zGetFieldFallback", .linkage = .strong });
        @export(zSetFieldFallback, .{ .name = "zSetFieldFallback", .linkage = .strong });
        @export(zMapSet, .{ .name = "zMapSet", .linkage = .strong });
        @export(zValueMapGet, .{ .name = "zValueMapGet", .linkage = .strong });
        @export(zPanicFmt, .{ .name = "zPanicFmt", .linkage = .strong });
        @export(zOtherToF64, .{ .name = "zOtherToF64", .linkage = .strong });
        @export(zAllocExternalObject, .{ .name = "zAllocExternalObject", .linkage = .strong });
        @export(zAllocExternalCycObject, .{ .name = "zAllocExternalCycObject", .linkage = .strong });
        @export(zAllocStringTemplate, .{ .name = "zAllocStringTemplate", .linkage = .strong });
        @export(zAllocStringTemplate2, .{ .name = "zAllocStringTemplate2", .linkage = .strong });
        @export(zAllocPoolObject, .{ .name = "zAllocPoolObject", .linkage = .strong });
        @export(zAllocObjectSmall, .{ .name = "zAllocObjectSmall", .linkage = .strong });
        @export(zAllocFiber, .{ .name = "zAllocFiber", .linkage = .strong });
        @export(zAllocFuncFromSym, .{ .name = "zAllocFuncFromSym", .linkage = .strong });
        @export(zAlloc, .{ .name = "zAlloc", .linkage = .strong });
        @export(zAllocList, .{ .name = "zAllocList", .linkage = .strong });
        @export(zAllocListDyn, .{ .name = "zAllocListDyn", .linkage = .strong });
        @export(zCall, .{ .name = "zCall", .linkage = .strong });
        @export(zCallSym, .{ .name = "zCallSym", .linkage = .strong });
        @export(zCallSymDyn, .{ .name = "zCallSymDyn", .linkage = .strong });
        @export(zCallObjSym, .{ .name = "zCallObjSym", .linkage = .strong });
        @export(zOpMatch, .{ .name = "zOpMatch", .linkage = .strong });
        @export(zOpCodeName, .{ .name = "zOpCodeName", .linkage = .strong });
        @export(zLog, .{ .name = "zLog", .linkage = .strong });
        @export(zGrowTryStackTotalCapacity, .{ .name = "zGrowTryStackTotalCapacity", .linkage = .strong });
        @export(zGetTypeName, .{ .name = "zGetTypeName", .linkage = .strong });
        @export(zFreeObject, .{ .name = "zFreeObject", .linkage = .strong });
        @export(zDumpValue, .{ .name = "zDumpValue", .linkage = .strong });
        @export(zDumpEvalOp, .{ .name = "zDumpEvalOp", .linkage = .strong });
        @export(zCheckDoubleFree, .{ .name = "zCheckDoubleFree", .linkage = .strong });
        @export(zCheckRetainDanglingPointer, .{ .name = "zCheckRetainDanglingPointer", .linkage = .strong });
        @export(zFatal, .{ .name = "zFatal", .linkage = .strong });
        @export(zEvalCompareNot, .{ .name = "zEvalCompareNot", .linkage = .strong });
        @export(zEvalCompare, .{ .name = "zEvalCompare", .linkage = .strong });
        @export(zEnsureListCap, .{ .name = "zEnsureListCap", .linkage = .strong });
        @export(zEnd, .{ .name = "zEnd", .linkage = .strong });
        @export(zGetFieldOffsetFromTable, .{ .name = "zGetFieldOffsetFromTable", .linkage = .strong });
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

fn zGetTypeName(vm: *VM, id: cy.TypeId) callconv(.C) vmc.Str {
    if (cy.Trace) {
        if ((id & vmc.TYPE_MASK) == vmc.TYPE_MASK) {
            return vmc.Str{ .ptr = "DanglingObject", .len = "DanglingObject".len };
        }
        if (vm.c.types[id].kind == .null) {
            cy.panicFmt("Type `{}` is uninited.", .{ id });
        }
    }
    const name = vm.c.types[id].sym.name();
    return vmc.Str{
        .ptr = name.ptr,
        .len = name.len,
    };
}

fn zEnsureListCap(vm: *VM, list: *cy.List(Value), cap: usize) callconv(.C) vmc.ResultCode {
    list.ensureTotalCapacity(vm.alloc, cap) catch {
        return vmc.RES_CODE_UNKNOWN;
    };
    return vmc.RES_CODE_SUCCESS;
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
    expand_arg0: std.posix.Arg0Expand = .no_expand,
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