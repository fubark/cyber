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
const bc = @import("bc_gen.zig");
const Value = cy.Value;
const debug = @import("debug.zig");
const http = @import("http.zig");
const HeapObject = cy.HeapObject;
const release = cy.arc.release;
const retain = cy.arc.retain;
const retainObject = cy.arc.retainObject;
const UserVM = cy.UserVM;

const logger = cy.log.scoped(.vm);

/// Duplicate from vm.h to add methods.
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

    fields: [*]vmc.Field,
    fields_cap: usize,
    fields_len: usize,

    /// Static vars. 
    varSyms: [*]rt.VarSym,
    varSyms_cap: usize,
    varSyms_len: usize,

    /// Context vars.
    context_vars: [*]rt.ContextVar,
    context_vars_cap: usize,
    context_vars_len: usize,

    /// Types.
    types: [*]const types.Type,
    types_len: usize,

    trace: *vmc.TraceInfo,

    /// In debug mode, always save the current pc so tracing can be obtained.
    /// debugPc == NullId indicates execution has not started.
    debugPc: u32,

    trace_indent: u32,

    refCounts: if (cy.TrackGlobalRC) usize else void,

    pub fn getVarSyms(self: *VMC) *cy.List(rt.VarSym) {
        return @ptrCast(&self.varSyms);
    }

    pub fn getContextVars(self: *VMC) *cy.List(rt.ContextVar) {
        return @ptrCast(&self.context_vars);
    }

    pub fn getFields(self: *VMC) *cy.List(vmc.Field) {
        return @ptrCast(&self.fields);
    }

    pub fn getStack(self: *VMC) []Value {
        return @as(*[]Value, @ptrCast(&self.stack)).*;
    }
};

const OverloadedFuncEntry = struct {
    id: u32,
    next: u32,
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

    /// `type_method_map` is queried at runtime by dynamic method calls using the receiver's type and method id.
    method_map: std.AutoHashMapUnmanaged(rt.MethodKey, vmc.MethodId),
    type_method_map: std.HashMapUnmanaged(rt.TypeMethodKey, rt.FuncGroupId, cy.hash.KeyU64Context, 80),

    /// Regular function symbol table.
    /// During codegen function calls only depend on a reserved runtime func id.
    /// The callee can then be generated later or by a separate chunk worker.
    funcSyms: cy.List(rt.FuncSymbol),
    funcSymDetails: cy.List(rt.FuncSymDetail),

    /// Contiguous func ids. The first element in a segment contains the num of overloaded funcs for the func sym.
    overloaded_funcs: cy.List(OverloadedFuncEntry),

    func_groups: cy.List(rt.FuncGroup),

    /// Struct fields symbol table.
    type_field_map: std.HashMapUnmanaged(rt.FieldTableKey, vmc.TypeField, cy.hash.KeyU64Context, 80),
    field_map: std.AutoHashMapUnmanaged(vmc.NameId, rt.FieldId),

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

    /// Tasks in this queue are ready to be executed (not in a waiting state).
    /// `queueTask` appends tasks here.
    ready_tasks: std.fifo.LinearFifo(cy.heap.AsyncTask, .Dynamic),

    /// vtables for trait impls, each vtable contains func ids.
    vtables: cy.List([]const u32),

    /// This is needed for reporting since a method entry can be empty.
    methods: cy.List(rt.Method),
    debugTable: []const cy.DebugSym,
    unwind_table: []const cy.fiber.UnwindKey,
    unwind_slots: []const u8,
    unwind_slot_prevs: []const cy.fiber.UnwindKey,
    unwind_trys: []const cy.fiber.UnwindTry,

    /// Records a minimal trace when walking the stack.
    /// Stack frames are then constructed from them.
    compactTrace: cy.List(vmc.CompactFrame),

    compiler: *cy.Compiler,
    sema: *cy.sema.Sema,

    /// User data ptr. Useful for embedders.
    userData: ?*anyopaque,

    /// Map of arbitrary data pointers.
    data: std.StringHashMapUnmanaged(?*anyopaque),

    /// Host write hook.
    print: cc.PrintFn,
    print_err: cc.PrintErrorFn,

    /// Object to pc of instruction that allocated it. Trace mode.
    objectTraceMap: std.AutoHashMapUnmanaged(*HeapObject, debug.ObjectTrace),

    /// Interface used for imports and fetch.
    httpClient: http.HttpClient,
    stdHttpClient: if (cy.hasCLI) *http.StdHttpClient else *anyopaque,

    emptyString: Value,
    placeholder: Value,

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
            .placeholder = undefined,
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
                .context_vars = undefined,
                .context_vars_cap = 0,
                .context_vars_len = 0,
                .fields = undefined,
                .fields_cap = 0,
                .fields_len = 0,
                .types = undefined,
                .types_len = 0,
                .trace = undefined,
                .refCounts = if (cy.TrackGlobalRC) 0 else undefined,
                .mainFiber = undefined,
                .curFiber = undefined,
                .debugPc = cy.NullId,
                .trace_indent = 0,
            },
            .method_map = .{},
            .methods = .{},
            .type_method_map = .{},
            .funcSyms = .{},
            .funcSymDetails = .{},
            .overloaded_funcs = .{},
            .func_groups = .{},
            .type_field_map = .{},
            .field_map = .{},
            .syms = .{},
            .symSignatures = .{},
            .inlineSaves = .{},
            .u8Buf = .{},
            .stackTrace = .{},
            .debugTable = undefined,
            .unwind_table = undefined,
            .unwind_slots = undefined,
            .unwind_slot_prevs = undefined,
            .unwind_trys = undefined,
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
            .data = .{},
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
            .ready_tasks = std.fifo.LinearFifo(cy.heap.AsyncTask, .Dynamic).init(alloc),
            .vtables = .{},
        };
        self.c.mainFiber.typeId = bt.Fiber | vmc.CYC_TYPE_MASK;
        self.c.mainFiber.rc = 1;
        self.c.mainFiber.panicType = vmc.PANIC_NONE;
        self.c.curFiber = &self.c.mainFiber;
        self.compiler = try self.alloc.create(cy.Compiler);
        self.sema = &self.compiler.sema;
        try self.compiler.init(self);

        const core_data = try self.alloc.create(builtins.CoreData);
        try self.data.put(self.alloc, "core", core_data);

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
        self.c.mainFiber.stackPtr = @ptrCast(self.c.stack);
        self.c.mainFiber.stackLen = @intCast(self.c.stack_len);

        try self.funcSyms.ensureTotalCapacityPrecise(self.alloc, 255);
        try self.method_map.ensureTotalCapacity(self.alloc, 200);
        std.debug.assert(cy.utils.getHashMapMemSize(rt.MethodKey, vmc.MethodId, self.method_map.capacity()) < 4096);

        try self.c.getFields().ensureTotalCapacityPrecise(alloc, 170);

        // Initialize heap.
        const list = try cy.heap.growHeapPages(self, 1);
        self.heapFreeHead = list.head;
        if (cy.Trace) {
            self.heapFreeTail = list.tail;
        }

        const data = try self.alloc.create(cy.builtins.BuiltinsData);
        try self.data.put(self.alloc, "builtins", data);
        
        try @call(.never_inline, cy.bindings.bindCore, .{self});
    }

    pub fn deinitRtObjects(self: *VM) void {
        if (self.deinitedRtObjects) {
            return;
        }

        for (self.c.getContextVars().items()) |context_var| {
            self.release(context_var.value);
        }

        logger.tracev("release varSyms", .{});
        for (self.c.getVarSyms().items(), 0..) |vsym, i| {
            var do_release = false;
            const sym = self.varSymExtras.buf[i];
            switch (sym.type) {
                .userVar => {
                    const user_var = sym.cast(.userVar);
                    if (!self.sema.isUnboxedType(user_var.type)) {
                        do_release = true;
                    }
                },
                .hostVar => {
                    const host_var = sym.cast(.hostVar);
                    if (!self.sema.isUnboxedType(host_var.type)) {
                        do_release = true;
                    }
                },
                else => std.debug.panic("Unexpected: {}", .{sym.type}),
            }
            if (do_release) {
                logger.tracevIf(build_options.log_mem, "release varSym: {s}", .{self.varSymExtras.buf[i].name()});
                release(self, vsym.value);
            }
        }
        self.c.getVarSyms().clearRetainingCapacity();

        // Release static strings.
        logger.tracev("release static objects {}", .{self.staticObjects.len});
        for (self.staticObjects.items()) |obj| {
            logger.tracevIf(build_options.log_mem, "release static object", .{});
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

        if (!reset) {
            const data = self.getData(*cy.builtins.BuiltinsData, "builtins");
            self.alloc.destroy(data);
            _ = self.data.remove("builtins");
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
            self.alloc.free(self.c.mainFiber.stackPtr[0..self.c.mainFiber.stackLen]);
            self.c.mainFiber.stackLen = 0;
            self.c.stack_len = 0;
        }

        if (reset) {
            self.compactTrace.clearRetainingCapacity();
        } else {
            self.compactTrace.deinit(self.alloc);
        }

        if (reset) {
            self.ready_tasks.deinit();
            self.ready_tasks = std.fifo.LinearFifo(cy.heap.AsyncTask, .Dynamic).init(self.alloc);
        } else {
            self.ready_tasks.deinit();
        }

        if (reset) {
            self.method_map.clearRetainingCapacity();
            self.methods.clearRetainingCapacity();
            self.type_method_map.clearRetainingCapacity();
        } else {
            self.method_map.deinit(self.alloc);
            self.methods.deinit(self.alloc);
            self.type_method_map.deinit(self.alloc);
        }

        if (reset) {
            self.funcSyms.clearRetainingCapacity();
            self.funcSymDetails.clearRetainingCapacity();
            self.overloaded_funcs.clearRetainingCapacity();
            self.func_groups.clearRetainingCapacity();
        } else {
            self.funcSyms.deinit(self.alloc);
            self.funcSymDetails.deinit(self.alloc);
            self.overloaded_funcs.deinit(self.alloc);
            self.func_groups.deinit(self.alloc);
        }

        if (reset) {
            self.c.getVarSyms().clearRetainingCapacity();
            self.c.getContextVars().clearRetainingCapacity();
        } else {
            self.c.getVarSyms().deinit(self.alloc);
            self.c.getContextVars().deinit(self.alloc);
        }

        if (reset) {
            self.c.getFields().clearRetainingCapacity();
            self.type_field_map.clearRetainingCapacity();
            self.field_map.clearRetainingCapacity();
        } else {
            self.c.getFields().deinit(self.alloc);
            self.type_field_map.deinit(self.alloc);
            self.field_map.deinit(self.alloc);
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

        for (self.vtables.items()) |vtable| {
            self.alloc.free(vtable);
        }
        if (reset) {
            self.vtables.clearRetainingCapacity();
        } else {
            self.vtables.deinit(self.alloc);
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
            const core_data = self.getData(*builtins.CoreData, "core");
            self.alloc.destroy(core_data);
            self.data.deinit(self.alloc);
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

        // Sync debug table for debug output.
        if (!config.skip_codegen and config.backend == cc.BackendVM) {
            self.debugTable = res.vm.debugTable.items;
        }
        tt.endPrint("compile");
        return res;
    }

    pub fn resetVM(self: *VM) !void {
        self.deinit(true);
        try @call(.never_inline, cy.bindings.bindCore, .{self});

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
            var iter = self.field_map.iterator();
            while (iter.next()) |it| {
                const name = rt.getName(self, it.key_ptr.*);
                fmt.printStderr("\t{}: {}\n", &.{v(name), v(it.value_ptr.*)});
            }
        }
    }

    pub fn getFiberContext(vm: *const cy.VM) cy.fiber.PcFpOff {
        return .{
            .pc = cy.fiber.getInstOffset(vm.c.ops, vm.c.pc),
            .fp = cy.fiber.getStackOffset(vm.c.stack, vm.c.framePtr),
        };
    }

    pub fn getData(self: *cy.VM, comptime T: type, key: []const u8) T {
        return @ptrCast(@alignCast(self.data.get(key).?));
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

    pub fn prepCtEval(self: *VM, buf: *cy.ByteCodeBuffer) !void {
        buf.mconsts = buf.consts.items;
        try self.prepEval(buf);
    }

    pub fn prepEval(self: *VM, buf: *cy.ByteCodeBuffer) !void {
        cy.fiber.freeFiberPanic(self, &self.c.mainFiber);
        self.c.curFiber.panicType = vmc.PANIC_NONE;
        self.debugTable = buf.debugTable.items;
        self.unwind_table = buf.unwind_table.items;
        self.unwind_slots = buf.unwind_slots.items;
        self.unwind_slot_prevs = buf.unwind_slot_prevs.items;
        self.unwind_trys = buf.unwind_trys.items;
    
        self.c.pc = @ptrCast(&buf.ops.items[buf.main_pc]);

        try cy.fiber.stackEnsureTotalCapacity(self, buf.mainStackSize);
        self.c.mainFiber.stack_size = @intCast(buf.mainStackSize);
        self.c.framePtr = @ptrCast(self.c.stack);

        self.c.ops = buf.ops.items.ptr;
        self.c.ops_len = buf.ops.items.len;
        self.c.consts = buf.mconsts.ptr;
        self.c.consts_len = buf.mconsts.len;
        self.c.types = self.compiler.sema.types.items.ptr;
        self.c.types_len = self.compiler.sema.types.items.len;
    }

    pub fn evalByteCode(self: *VM, buf: *cy.ByteCodeBuffer) !Value {
        if (buf.ops.items.len == 0) {
            return error.NoEndOp;
        }

        try self.prepEval(buf);
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
        const frame_t = cy.fiber.getFrameType(vm, vm.c.getStack(), fp);
        if (fp == 0) {
            return vm.c.curFiber.stack_size;
        } else if (frame_t == .vm) {
            // Vm frame.
            return vm.c.framePtr[1].call_info.stack_size;
        } else if (frame_t == .host) {
            // Host frame.
            return vm.c.framePtr[1].call_info.stack_size;
        } else {
            @panic("Unexpected");
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

        // Copy callee + args to a new blank frame.
        vm.c.framePtr[ret + CalleeStart] = func;
        @memcpy(vm.c.framePtr[ret+CallArgStart..ret+CallArgStart+args.len], args);

        if (cy.Trace) {
            vm.c.trace_indent += 1;
        }

        // Pass in an arbitrary `pc` to reuse the same `call` used by the VM.
        const pcsp = try call(vm, vm.c.pc, vm.c.framePtr, func, ret, @intCast(args.len), false);
        vm.c.framePtr = pcsp.fp;
        vm.c.pc = pcsp.pc;

        // Only user funcs start eval loop.
        if (!cy.value.isHostFunc(vm, func)) {
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
            // Return to dyn frame since ret inst does not unwind for cont=false.
            vm.c.pc = vm.c.framePtr[2].retPcPtr;
            vm.c.framePtr = vm.c.framePtr[3].retFramePtr;
        } else {
            if (cy.Trace) {
                vm.c.trace_indent -= 1;
            }
        }

        // Perform ret_dyn.
        const final_ret = 5 + args.len;
        const ret_v = vm.c.framePtr[final_ret];
        const ret_t = vm.c.framePtr[1].call_info.payload & 0x7fffffff;
        var res: Value = undefined;
        if (vm.sema.isUnboxedType(ret_t)) {
            res = zBox(vm, ret_v, ret_t);
        } else {
            res = ret_v;
        }

        // Restore pc/sp.
        vm.c.framePtr = vm.c.stack + fpOff;
        vm.c.pc = pc;
        return res;
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
        const type_id = try self.sema.pushType();
        try sema.resolveObjectTypeId(c, sym, false, type_id);
        sym.head.setNameOwned(true);

        const infos = try c.alloc.alloc(cy.sym.FieldInfo, fields.len);
        for (fields, 0..) |field, i| {
            const field_sym = c.declareField(@ptrCast(sym), field, @intCast(i), bt.Any, null) catch return error.Unexpected;
            infos[i] = .{
                .sym = @ptrCast(field_sym),
                .type = bt.Any,
                .offset = 0,
            };
        }
        sym.fields = infos.ptr;
        sym.numFields = @intCast(infos.len);
        const rt_fields = try c.alloc.alloc(bool, infos.len);
        @memset(rt_fields, true);
        c.sema.types.items[sym.type].data.object = .{
            .numFields = @intCast(sym.numFields),
            .has_boxed_fields = true,
            .tuple = false,
            .fields = rt_fields.ptr,
        };

        // Update vm types view.
        self.c.types = c.sema.types.items.ptr;
        self.c.types_len = c.sema.types.items.len;
        return sym.type;
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

    pub fn ensureField(self: *VM, name: []const u8) !rt.FieldId {
        const name_id = try rt.ensureNameSym(self, name);
        const res = try self.field_map.getOrPut(self.alloc, name_id);
        if (!res.found_existing) {
            const id: u32 = @intCast(self.c.fields_len);
            try self.c.getFields().append(self.alloc, .{
                .name_id = name_id,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn addFuncGroup(self: *VM) !u32 {
        const id = self.func_groups.len;
        try self.func_groups.append(self.alloc, .{
            .id = undefined,
            .empty = true,
            .overloaded = false,
        });
        return @intCast(id);
    }

    pub fn ensureMethod(self: *VM, name: []const u8) !rt.MethodId {
        const nameId = try rt.ensureNameSym(self, name);
        const res = try @call(.never_inline, @TypeOf(self.method_map).getOrPut, .{&self.method_map, self.alloc, nameId});
        if (!res.found_existing) {
            const id: u32 = @intCast(self.methods.len);
            try self.methods.append(self.alloc, .{ .name = nameId });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    pub fn addTypeField(self: *VM, type_id: cy.TypeId, field_id: u32, offset: u16, field_t: types.TypeId) !void {
        const key = rt.FieldTableKey.initFieldTableKey(type_id, field_id);
        try self.type_field_map.putNoClobber(self.alloc, key, .{
            .boxed = !self.sema.isUnboxedType(field_t),
            .offset = offset,
            .type_id = field_t,
        });
    }

    /// Walking the linked list is ok since overloaded functions are less common
    /// and eventually insertion will need to match the same order as the sema order.
    fn getLastOverloadedFunc(self: *VM, head: u32) u32 {
        var cur_id = head;
        var cur = self.overloaded_funcs.buf[head];
        while (cur.next != cy.NullId) {
            cur_id = cur.next;
            cur = self.overloaded_funcs.buf[cur_id];
        }
        return cur_id;
    }

    pub fn setMethodGroup(self: *VM, type_id: cy.TypeId, name: []const u8, group: rt.FuncGroupId) !void {   
        const method = try self.ensureMethod(name);
        const key = rt.TypeMethodKey.initTypeMethodKey(type_id, method);
        try self.type_method_map.put(self.alloc, key, group);
    }

    pub fn addFunc(self: *VM, name: []const u8, sig: cy.sema.FuncSigId, func: rt.FuncSymbol) !u32 {
        const id = self.funcSyms.len;
        try self.funcSyms.append(self.alloc, func);

        try self.funcSymDetails.append(self.alloc, .{
            .namePtr = name.ptr, .nameLen = @intCast(name.len), .funcSigId = sig,
        });
        return @intCast(id);
    }

    pub fn addGroupFunc(self: *VM, group_id: rt.FuncGroupId, name: []const u8, sig: cy.sema.FuncSigId, rtFunc: rt.FuncSymbol) !u32 {
        const id = try self.addFunc(name, sig, rtFunc);

        const group = &self.func_groups.buf[group_id];
        if (group.empty) {
            group.id = @intCast(id);
            group.empty = false;
            return id;
        }

        if (!group.overloaded) {
            const func_head = self.overloaded_funcs.len;
            try self.overloaded_funcs.append(self.alloc, .{
                .id = group.id,
                .next = @intCast(func_head + 1),
            });
            try self.overloaded_funcs.append(self.alloc, .{
                .id = @intCast(id),
                .next = cy.NullId,
            });
            group.id = @intCast(func_head);
            group.overloaded = true;
        } else {
            const last = self.getLastOverloadedFunc(group.id);
            const next = self.overloaded_funcs.len;
            try self.overloaded_funcs.append(self.alloc, .{
                .id = @intCast(id),
                .next = cy.NullId,
            });
            self.overloaded_funcs.buf[last].next = @intCast(next);
        }
        return id;
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

    fn getFieldMissingSymbolError(self: *VM) error{Panic, OutOfMemory} {
        @setCold(true);
        return self.panic("Field not found in value.");
    }

    fn setFieldNotObjectError(self: *VM) !void {
        @setCold(true);
        return self.panic("Can't assign to value's field since the value is not an object.");
    }

    pub fn getTypeField(self: *VM, type_id: cy.TypeId, field_id: rt.FieldId) vmc.TypeField {
        const key = rt.FieldTableKey.initFieldTableKey(type_id, field_id);
        return self.type_field_map.get(key) orelse {
            return .{ .offset = cy.NullU16, .boxed = false, .type_id = cy.NullId };
        };
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

    pub fn getField(self: *VM, recv: Value, field_id: rt.FieldId) !Value {
        if (recv.isPointer()) {
            const obj = recv.asHeapObject();
            const res = self.getTypeField(obj.getTypeId(), field_id);
            if (res.offset != cy.NullU16) {
                return obj.object.getValue(res.offset);
            } else {
                const field = self.c.fields[field_id];
                return self.getFieldFallback(obj, field.name_id);
            }
        } else {
            return self.getFieldMissingSymbolError();
        }
    }

    /// Does not invoke `getFieldFallback`.
    pub fn getObjectField(self: *VM, obj: *cy.heap.Object, field: []const u8) !Value {
        const field_id = try self.ensureField(field);
        const base: *cy.heap.HeapObject = @ptrCast(obj);
        const res = self.getTypeField(base.getTypeId(), field_id);
        if (res.offset == cy.NullU16) {
            return error.MissingField;
        }
        return obj.getValue(res.offset);
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
        const func_val = try cy.heap.allocFunc(self, func);
        defer self.release(func_val);
        const result = try self.callFunc(func_val, &args, .{ .from_external = false });
        if (result.isInterrupt()) {
            return error.Panic;
        }
    }

    fn getFieldFallback(self: *VM, obj: *HeapObject, nameId: vmc.NameId) !Value {
        const name = rt.getName(self, nameId);
        const rec_t = obj.getTypeId();
        const type_e = self.c.types[rec_t];
        if (type_e.kind == .option) {
            const enum_t = type_e.sym.cast(.enum_t);
            const member = enum_t.getMember(name) orelse {
                return self.prepPanic("Missing member from choice type.");
            };
            const active_tag: u32 = @intCast(obj.object.getValue(0).asInt());
            if (active_tag != member.val) {
                return self.prepPanic("Tag is not active.");
            }
            const value = obj.object.getValue(1);
            if (self.sema.isUnboxedType(member.payloadType)) {
                return try box(self, value, member.payloadType);
            } else {
                self.retain(value);
                return value;
            }
        }
        if (!type_e.has_get_method) {
            return self.prepPanic("Missing field in object.");
        }

        const rec_arg = Value.initPtr(obj);
        const name_arg = try self.allocString(name);
        defer self.release(name_arg);

        var args: [2]Value = .{rec_arg, name_arg};
        const func = self.getCompatMethodFunc(rec_t, self.compiler.getMID, args[1..]) orelse {
            return error.Unexpected;
        };

        const func_val = try cy.heap.allocFunc(self, func);
        defer self.release(func_val);
        const result = try self.callFunc(func_val, &args, .{ .from_external = false });

        return result;
    }

    /// Assumes overloaded function. Finds first matching function at runtime.
    fn callSymDyn(
        self: *VM, pc: [*]cy.Inst, fp: [*]Value, entry: u32, ret: u8, nargs: u8,
    ) !cy.fiber.PcFp {
        const vals = fp[ret+CallArgStart..ret+CallArgStart+nargs];

        var cur = entry;
        while (cur != cy.NullId) {
            const ofunc = self.overloaded_funcs.buf[cur];
            const func = self.funcSyms.buf[ofunc.id];
            if (!self.isFuncCompat(func, vals)) {
                cur = ofunc.next;
                continue;
            }

            // Invoke function.
            // TODO: Extract common code between this and `callSym`
            switch (func.type) {
                .host_func => {
                    self.c.pc = pc;
                    self.c.framePtr = fp + ret;
                    fp[ret + 1] = buildRootCallInfo(false);
                    fp[ret + 2] = Value{ .retPcPtr = pc };
                    fp[ret + 3] = Value{ .retFramePtr = fp };
                    const res: Value = @bitCast(func.data.host_func.?(@ptrCast(self)));
                    if (res.isInterrupt()) {
                        return error.Panic;
                    }
                    fp[ret] = @bitCast(res);
                    return cy.fiber.PcFp{
                        .pc = pc + cy.bytecode.CallSymInstLen,
                        .fp = fp,
                    };
                },
                .func => {
                    if (@intFromPtr(fp + ret + func.data.func.stackSize) >= @intFromPtr(self.c.stackEndPtr)) {
                        return error.StackOverflow;
                    }

                    const newFramePtr = fp + ret;
                    newFramePtr[1] = buildCallInfo(true, cy.bytecode.CallSymInstLen, @intCast(func.data.func.stackSize));
                    newFramePtr[2] = Value{ .retPcPtr = pc + cy.bytecode.CallSymInstLen };
                    newFramePtr[3] = Value{ .retFramePtr = fp };
                    return cy.fiber.PcFp{
                        .pc = cy.fiber.toVmPc(self, func.data.func.pc),
                        .fp = newFramePtr,
                    };
                },
                else => {
                    logger.tracev("{}", .{func.type});
                    return self.panic("Missing func");
                },
            }
            cur = ofunc.next;
        }
        return panicIncompatibleCallSymSig(self, entry, vals);
    }

    fn callSym(
        self: *VM, pc: [*]cy.Inst, framePtr: [*]Value, func: rt.FuncSymbol, ret: u8,
    ) !cy.fiber.PcFp {
        switch (func.type) {
            .host_func => {
                defer if (cy.Trace) {
                    self.c.trace_indent -= 1;
                };
                // Optimize.
                pc[0] = cy.Inst.initOpCode(.callNativeFuncIC);
                @as(*align(1) u48, @ptrCast(pc + 6)).* = @intCast(@intFromPtr(func.data.host_func));

                self.c.pc = pc;
                self.c.framePtr = framePtr + ret;
                framePtr[ret + 1] = buildRootCallInfo(false);
                framePtr[ret + 2] = Value{ .retPcPtr = pc };
                framePtr[ret + 3] = Value{ .retFramePtr = framePtr };
                const res: Value = @bitCast(func.data.host_func.?(@ptrCast(self)));
                if (res.isInterrupt()) {
                    return error.Panic;
                }
                framePtr[ret] = @bitCast(res);
                return cy.fiber.PcFp{
                    .pc = pc + cy.bytecode.CallSymInstLen,
                    .fp = framePtr,
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
                newFramePtr[1] = buildCallInfo(true, cy.bytecode.CallSymInstLen, @intCast(func.data.func.stackSize));
                newFramePtr[2] = Value{ .retPcPtr = pc + cy.bytecode.CallSymInstLen };
                newFramePtr[3] = Value{ .retFramePtr = framePtr };
                return cy.fiber.PcFp{
                    .pc = cy.fiber.toVmPc(self, func.data.func.pc),
                    .fp = newFramePtr,
                };
            },
            .null => {
                return self.panic("Missing func null");
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
            if (!types.isTypeSymCompat(self.compiler, arg_t, target_t.type)) {
                if (!@call(.never_inline, canInferArg, .{self, arg, target_t.type})) {
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
            if (!types.isTypeSymCompat(self.compiler, valTypeId, target_t.type)) {
                return false;
            }
        }
        return true;
    }

    fn getTypeMethod(self: *VM, rec_t: cy.TypeId, method: rt.MethodId) ?rt.FuncGroupId {
        const key = rt.TypeMethodKey.initTypeMethodKey(rec_t, method);
        return self.type_method_map.get(key);
    }

    /// Assumes args does not include rec.
    fn getCompatMethodFunc(self: *VM, rec_t: cy.TypeId, method_id: rt.MethodId, args: []cy.Value) ?rt.FuncSymbol {
        const group_id = @call(.never_inline, getTypeMethod, .{self, rec_t, method_id}) orelse {
            return null;
        };

        // TODO: Skip type check for untyped params.
        const group = self.func_groups.buf[group_id];
        if (!group.overloaded) {
            // Check one function.
            const func = self.funcSyms.buf[group.id];
            if (self.isMethodFuncCompat(func, args)) {
                return func;
            } else {
                return null;
            }
        } else {
            // Overloaded functions.
            var cur: u32 = group.id;
            while (cur != cy.NullId) {
                const entry = self.overloaded_funcs.buf[cur];
                const func = self.funcSyms.buf[entry.id];

                if (!func.is_method) {
                    cur = entry.next;
                    continue;
                }

                if (self.isMethodFuncCompat(func, args)) {
                    return func;
                }
                cur = entry.next;
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
                try std.fmt.format(w, "{}", .{val.asBoxInt()});
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
            bt.ListDyn => {
                try std.fmt.format(w, "List ({})", .{obj.list.list.len});
            },
            bt.Map => {
                try std.fmt.format(w, "Map ({})", .{obj.map.inner.size});
            },
            bt.Type => {
                const name = try self.sema.allocTypeName(obj.type.type);
                defer self.alloc.free(name);
                try std.fmt.format(w, "type: {s}", .{name});
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
    pub usingnamespace VMGetArgExt;
};

fn evalCompareBool(left: Value, right: Value) bool {
    switch (left.getTypeId()) {
        bt.Integer => {
            if (right.getTypeId() == bt.Integer) {
                return left.castHeapObject(*cy.heap.Int).val == right.castHeapObject(*cy.heap.Int).val;
            }
        },
        bt.String => {
            if (right.getTypeId() == bt.String) {
                const lstr = left.asString();
                const rstr = right.asString();
                return std.mem.eql(u8, lstr, rstr);
            }
        },
        bt.Type => {
            if (right.getTypeId() == bt.Type) {
                const l = left.asHeapObject().type;
                const r = right.asHeapObject().type;
                return l.type == r.type;
            }
        },
        else => {},
    }
    return false;
}

fn evalCompare(left: Value, right: Value) Value {
    switch (left.getTypeId()) {
        bt.Integer => {
            if (right.getTypeId() == bt.Integer) {
                return Value.initBool(left.castHeapObject(*cy.heap.Int).val == right.castHeapObject(*cy.heap.Int).val);
            }
        },
        bt.String => {
            if (right.getTypeId() == bt.String) {
                const lstr = left.asString();
                const rstr = right.asString();
                return Value.initBool(std.mem.eql(u8, lstr, rstr));
            }
        },
        bt.Type => {
            if (right.getTypeId() == bt.Type) {
                const l = left.asHeapObject().type;
                const r = right.asHeapObject().type;
                return Value.initBool(l.type == r.type);
            }
        },
        else => {},
    }
    return Value.False;
}

fn evalCompareNot(left: cy.Value, right: cy.Value) cy.Value {
    switch (left.getTypeId()) {
        bt.Integer => {
            if (right.getTypeId() == bt.Integer) {
                return Value.initBool(left.castHeapObject(*cy.heap.Int).val != right.castHeapObject(*cy.heap.Int).val);
            }
        },
        bt.String => {
            if (right.getTypeId() == bt.String) {
                const lstr = left.asString();
                const rstr = right.asString();
                return Value.initBool(!std.mem.eql(u8, lstr, rstr));
            }
        },
        bt.Type => {
            if (right.getTypeId() == bt.Type) {
                const l = left.asHeapObject().type;
                const r = right.asHeapObject().type;
                return Value.initBool(l.type != r.type);
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
    try t.eq(@offsetOf(VMC, "varSyms"), @offsetOf(vmc.VMC, "varSyms"));
    try t.eq(@offsetOf(VMC, "context_vars"), @offsetOf(vmc.VMC, "context_vars"));
    try t.eq(@offsetOf(VMC, "fields"), @offsetOf(vmc.VMC, "fields"));
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
        vm.c.framePtr = vm.c.stack + res.fp;
    } else {
        const res = try @call(.never_inline, panicCurFiber, .{ vm });
        vm.c.pc = vm.c.ops + res.pc;
        vm.c.framePtr = vm.c.stack + res.fp;
    }
}

/// To reduce the amount of code inlined in the hot loop, handle StackOverflow at the top and resume execution.
/// This is also the entry way for native code to call into the VM, assuming pc, framePtr, and virtual registers are already set.
pub fn evalLoopGrowStack(vm: *VM, handle_panic: bool) error{StackOverflow, OutOfMemory, Panic, NoDebugSym, Unexpected, Await, End}!void {
    logger.tracev("begin eval loop", .{});

    // Record the start fp offset, so that stack unwinding knows when to stop.
    // This is useful for having nested eval stacks.
    const startFp = cy.fiber.getStackOffset(vm.c.stack, vm.c.framePtr);

    if (comptime build_options.vmEngine == .zig) {
        return error.Unsupported;
    } else if (comptime build_options.vmEngine == .c) {
        while (true) {
            const res = vmc.execBytecode(@ptrCast(vm));
            cy.fiber.saveCurFiber(vm);

            if (res == vmc.RES_CODE_SUCCESS) {
                break;
            }

            if (res == vmc.RES_CODE_AWAIT) {
                return error.Await;
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
        try @call(.never_inline, cy.fiber.growStackAuto, .{vm});
    } else if (res == vmc.RES_CODE_UNKNOWN) {
        logger.tracev("Unknown error code.", .{});
        const cont = try @call(.never_inline, panicCurFiber, .{ vm });
        vm.c.pc = vm.c.ops + cont.pc;
        vm.c.framePtr = vm.c.stack + cont.fp;
    }
}

fn panicCurFiber(vm: *VM) !cy.fiber.PcFpOff {
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

fn dumpEvalOp(vm: *VM, pc: [*]const cy.Inst, fp: [*]const cy.Value) !void {
    const S = struct {
        var buf: [1024]u8 = undefined;
    };
    const offset = getInstOffset(vm, pc);
    var extra: []const u8 = "";
    switch (pc[0].opcode()) {
        .callObjSym => {
            const symId = pc[4].val;
            const name_id = vm.methods.buf[symId].name;
            const name = rt.getName(vm, name_id);
            extra = try std.fmt.bufPrint(&S.buf, "rt: sym={s}", .{name});
        },
        .constOp => {
            const idx = @as(*const align (1) u16, @ptrCast(pc + 1)).*;
            _ = idx;
            const dst = pc[3].val;
            _ = dst;
            // TODO: Requires const type.
            // const val = Value{ .val = vm.c.consts[idx].val };
            // extra = try std.fmt.bufPrint(&S.buf, "rt: constVal={s}", .{
            //     try vm.getOrBufPrintValueStr(&cy.tempBuf, val)
            // });
        },
        .callObjNativeFuncIC => {
            const symId = pc[4].val;
            const name_id = vm.methods.buf[symId].name;
            const name = rt.getName(vm, name_id);
            extra = try std.fmt.bufPrint(&S.buf, "rt: sym={s}", .{name});
        },
        .callSym => {
            const ret = pc[1].val;
            const symId = @as(*const align(1) u16, @ptrCast(pc + 4)).*;
            const details = vm.funcSymDetails.buf[symId];
            const name = details.namePtr[0..details.nameLen];
            extra = try std.fmt.bufPrint(&S.buf, "rt: sym={s}, fp={}", .{name, cy.fiber.getStackOffset(vm.c.stack, fp) + ret});
        },
        .call => {
            const ret = pc[1].val;
            extra = try std.fmt.bufPrint(&S.buf, "rt: fp={}", .{cy.fiber.getStackOffset(vm.c.stack, fp) + ret});
        },
        .fieldDyn => {
            const field_id = pc[3].val;
            const field = vm.c.fields[field_id];
            const name = rt.getName(vm, field.name_id);
            extra = try std.fmt.bufPrint(&S.buf, "rt: sym={s}", .{name});
        },
        else => {},
    }

    fmt.printStderr("{}", &.{fmt.repeat(' ', vm.c.trace_indent * 2)});
    try cy.bytecode.dumpInst(vm, offset, pc[0].opcode(), pc, .{ .extra = extra });
}

fn box(vm: *VM, val: Value, type_id: cy.TypeId) !cy.Value {
    if (cy.Trace) {
        if (!vm.sema.isUnboxedType(type_id)) {
            @panic("Unsupported.");
        }
    }
    return vm.allocBoxValue(type_id, val.val);
}

pub fn unbox(vm: *VM, val: Value, type_id: cy.TypeId) cy.Value {
    if (cy.Trace) {
        if (!vm.sema.isUnboxedType(type_id)) {
            @panic("Unsupported.");
        }
    }
    return val.asHeapObject().object.firstValue;
}

fn canInferArg(vm: *VM, arg: Value, target_t: cy.TypeId) bool {
    if (arg.getTypeId() == bt.TagLit) {
        const type_e = vm.c.types[target_t];
        if (type_e.kind == .@"enum") {
            const name = vm.syms.buf[arg.asSymbolId()].name;
            if (type_e.sym.cast(.enum_t).getMemberTag(name)) |value| {
                _ = value;
            
                return true;
            }
        } else if (target_t == bt.Symbol) {
            return true;
        }
    }
    return false;
}

fn inferArg(vm: *VM, arg: Value, target_t: cy.TypeId) ?Value {
    if (arg.getTypeId() == bt.TagLit) {
        const type_e = vm.c.types[target_t];
        if (type_e.kind == .@"enum") {
            const name = vm.syms.buf[arg.asSymbolId()].name;
            if (type_e.sym.cast(.enum_t).getMemberTag(name)) |value| {
                return Value.initEnum(target_t, @intCast(value));
            }
        } else if (target_t == bt.Symbol) {
            return Value.initSymbol(arg.asSymbolId());
        }
    }
    return null;
}

fn preCallDyn(vm: *VM, pc: [*]cy.Inst, fp: [*]Value, nargs: u8, ret: u8, final_ret: u8, func_stack_size: u16, req_type_check: bool, sig_id: sema.FuncSigId, cont: bool) !cy.TypeId {
    const sig = vm.compiler.sema.getFuncSig(sig_id);

    // Setup call frame to return to the next inst.
    fp[ret + 1] = buildDynFrameInfo(cont, cy.bytecode.CallInstLen + cy.bytecode.RetDynLen, final_ret - ret);
    // Save ret box type.
    var ret_type_mask = sig.ret;
    if (vm.sema.isUnboxedType(sig.ret)) {
        ret_type_mask = ret_type_mask | 0x80000000;
    }
    fp[ret + 1].call_info.payload = ret_type_mask;
    fp[ret + 2] = Value{ .retPcPtr = pc + cy.bytecode.CallInstLen + cy.bytecode.RetDynLen };
    fp[ret + 3] = Value{ .retFramePtr = fp };

    // Setup the final call frame with args copied.
    // This allows arg unboxing so that post call release still operates on boxed args.
    fp[final_ret + 1] = buildCallInfo2(cont, cy.bytecode.CallInstLen, @intCast(func_stack_size));
    fp[final_ret + 2] = Value{ .retPcPtr = pc + cy.bytecode.CallInstLen};
    fp[final_ret + 3] = Value{ .retFramePtr = fp + ret };

    const args = fp[ret+CallArgStart..ret+CallArgStart+nargs];
    const final_args = fp[final_ret+CallArgStart..final_ret+CallArgStart+nargs];
    if (req_type_check) {
        // Perform type check on args.
        for (args, 0..) |arg, i| {
            const cstrType = sig.params_ptr[i];
            const argType = arg.getTypeId();
            if (!types.isTypeSymCompat(vm.compiler, argType, cstrType.type)) {
                const final_arg = @call(.never_inline, inferArg, .{vm, arg, cstrType.type}) orelse {
                    return panicIncompatibleLambdaSig(vm, args, sig_id);
                };
                args[i] = final_arg;
                final_args[i] = final_arg;
                continue;
            }
            if (vm.sema.isUnboxedType(cstrType.type)) {
                const final_arg = @call(.never_inline, unbox, .{vm, arg, argType});
                final_args[i] = final_arg;
                continue;
            }
            final_args[i] = args[i];
        }
    } else {
        // Just copy args.
        @memcpy(final_args, args);
    }
    return sig.ret;
}

/// See `reserveFuncParams` for stack layout.
/// numArgs does not include the callee.
/// Arguments are always boxed values, so they need to be unboxed when calling a typed function
/// and reboxed so that the follow up release doesn't operate on an unboxed value.
pub fn call(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, callee: Value, ret: u8, numArgs: u8, cont: bool) !cy.fiber.PcFp {
    if (!callee.isPointer()) {
        return vm.interruptThrowSymbol(.InvalidArgument);
    }
    const obj = callee.asHeapObject();
    if (obj.getTypeId() == bt.Func) {
        return callFuncUnion(vm, pc, framePtr, callee, ret, numArgs, cont, &obj.func);
    }
    const type_e = vm.c.types[obj.getTypeId()];
    if (type_e.kind == .func_ptr) {
        switch (obj.func_ptr.kind) {
            .bc => {
                if (numArgs != obj.func_ptr.numParams) {
                    logger.tracev("lambda params/args mismatch {} {}", .{numArgs, obj.func_ptr.numParams});
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                const req_stack_size = obj.func_ptr.data.bc.stack_size + CallArgStart + obj.func_ptr.numParams;
                if (@intFromPtr(framePtr + ret + req_stack_size) >= @intFromPtr(vm.c.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const final_ret: u8 = @intCast(ret + 5 + obj.func_ptr.numParams);
                _ = try preCallDyn(vm, pc, framePtr, numArgs, ret, final_ret, obj.func_ptr.data.bc.stack_size, obj.func_ptr.reqCallTypeCheck, obj.func_ptr.sig, cont);
                return cy.fiber.PcFp{
                    .pc = cy.fiber.toVmPc(vm, obj.func_ptr.data.bc.pc),
                    .fp = framePtr + final_ret,
                };
            },
            .host => {
                if (numArgs != obj.func_ptr.numParams) {
                    logger.tracev("hostfunc params/args mismatch {} {}", .{numArgs, obj.func_ptr.numParams});
                    return vm.interruptThrowSymbol(.InvalidSignature);
                }

                const final_ret: u8 = @intCast(ret + 5 + obj.func_ptr.numParams);
                _ = try preCallDyn(vm, pc, framePtr, numArgs, ret, final_ret, @intCast(5 + obj.func_ptr.numParams), obj.func_ptr.reqCallTypeCheck, obj.func_ptr.sig, cont);

                vm.c.pc = pc;
                vm.c.framePtr = framePtr + final_ret;

                // Overwrite last frame as a host frame.
                framePtr[final_ret + 1] = buildRootCallInfo(false);
                framePtr[final_ret + 2] = Value{ .retPcPtr = pc };
                // Keep retFramePtr.

                const res: cy.Value = @bitCast(obj.func_ptr.data.host.ptr.?(@ptrCast(vm)));
                if (res.isInterrupt()) {
                    return error.Panic;
                }
                framePtr[final_ret] = res;

                return cy.fiber.PcFp{
                    .pc = pc + cy.bytecode.CallInstLen,
                    .fp = framePtr + ret,
                };
            },
        }
    } else if (type_e.kind == .func_union) {
        return callFuncUnion(vm, pc, framePtr, callee, ret, numArgs, cont, &obj.func_union);
    } else {
        return vm.interruptThrowSymbol(.InvalidArgument);
    }
}

fn callFuncUnion(vm: *VM, pc: [*]cy.Inst, framePtr: [*]cy.Value, callee: cy.Value, ret: u8, numArgs: u8, cont: bool, func: *cy.heap.FuncUnion) !cy.fiber.PcFp {
    switch (func.kind) {
        .closure => {
            if (numArgs != func.numParams) {
                logger.tracev("closure params/args mismatch {} {}", .{numArgs, func.numParams});
                return vm.interruptThrowSymbol(.InvalidSignature);
            }

            const req_stack_size = func.data.closure.stack_size + CallArgStart + func.numParams;
            if (@intFromPtr(framePtr + ret + req_stack_size) >= @intFromPtr(vm.c.stackEndPtr)) {
                return error.StackOverflow;
            }

            const final_ret: u8 = @intCast(ret + 5 + func.numParams);
            _ = try preCallDyn(vm, pc, framePtr, numArgs, ret, final_ret, func.data.closure.stack_size, func.reqCallTypeCheck, func.sig, cont);

            // Copy closure to local.
            framePtr[final_ret + func.data.closure.local] = callee;

            return cy.fiber.PcFp{
                .pc = cy.fiber.toVmPc(vm, func.data.closure.pc),
                .fp = framePtr + final_ret,
            };
        },
        .bc => {
            if (numArgs != func.numParams) {
                logger.tracev("lambda params/args mismatch {} {}", .{numArgs, func.numParams});
                return vm.interruptThrowSymbol(.InvalidSignature);
            }

            const req_stack_size = func.data.bc.stack_size + CallArgStart + func.numParams;
            if (@intFromPtr(framePtr + ret + req_stack_size) >= @intFromPtr(vm.c.stackEndPtr)) {
                return error.StackOverflow;
            }

            const final_ret: u8 = @intCast(ret + 5 + func.numParams);
            _ = try preCallDyn(vm, pc, framePtr, numArgs, ret, final_ret, func.data.bc.stack_size, func.reqCallTypeCheck, func.sig, cont);
            return cy.fiber.PcFp{
                .pc = cy.fiber.toVmPc(vm, func.data.bc.pc),
                .fp = framePtr + final_ret,
            };
        },
        .host => {
            if (numArgs != func.numParams) {
                logger.tracev("hostfunc params/args mismatch {} {}", .{numArgs, func.numParams});
                return vm.interruptThrowSymbol(.InvalidSignature);
            }

            const final_ret: u8 = @intCast(ret + 5 + func.numParams);
            _ = try preCallDyn(vm, pc, framePtr, numArgs, ret, final_ret, @intCast(5 + func.numParams), func.reqCallTypeCheck, func.sig, cont);

            vm.c.pc = pc;
            vm.c.framePtr = framePtr + final_ret;

            // Overwrite last frame as a host frame.
            framePtr[final_ret + 1] = buildRootCallInfo(false);
            framePtr[final_ret + 2] = Value{ .retPcPtr = pc };
            // Keep retFramePtr.

            const res: cy.Value = @bitCast(func.data.host.ptr.?(@ptrCast(vm)));
            if (res.isInterrupt()) {
                return error.Panic;
            }
            framePtr[final_ret] = res;

            return cy.fiber.PcFp{
                .pc = pc + cy.bytecode.CallInstLen,
                .fp = framePtr + ret,
            };
        },
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
    const argsSigStr = vm.compiler.sema.allocTypesStr(argTypes, null) catch return error.OutOfMemory;
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
    // Use first func to determine name.
    const first_ofunc = vm.overloaded_funcs.buf[overload_entry];
    const first_func_details = vm.funcSymDetails.buf[first_ofunc.id];
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

    var cur = first_ofunc.next;
    while (cur != cy.NullId) {
        const ofunc = vm.overloaded_funcs.buf[cur];
        try w.writeByte('\n');
        const func_details = vm.funcSymDetails.buf[ofunc.id];
        funcStr = vm.sema.formatFuncSig(func_details.funcSigId, &cy.tempBuf, null) catch return error.OutOfMemory;
        try w.print("    func {s}{s}", .{name, funcStr});
        cur = ofunc.next;
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
    const name = rt.getName(vm, method.name);
    const typeName = vm.c.types[typeId].sym.name();
    const group_id = vm.getTypeMethod(typeId, method_id) orelse {
        return vm.panicFmt("The method `{}` can not be found in `{}`.", &.{
            v(name), v(typeName),
        });
    };

    var msg: std.ArrayListUnmanaged(u8) = .{}; 
    defer msg.deinit(vm.alloc);
    const w = msg.writer(vm.alloc);
    const call_args_str = vm.compiler.sema.allocTypesStr(typeIds[1..], null) catch return error.OutOfMemory;
    defer vm.alloc.free(call_args_str);

    try w.print("Can not find compatible method for call: `({s}) {s}{s}`.", .{typeName, name, call_args_str});
    try w.writeAll("\n");
    try w.print("Methods named `{s}`:\n", .{name});

    const group = vm.func_groups.buf[group_id];
    if (!group.overloaded) {
        const func = vm.funcSyms.buf[group.id];
        if (func.is_method) {
            const funcStr = vm.sema.formatFuncSig(func.sig, &cy.tempBuf, null) catch {
                return error.OutOfMemory;
            };
            try w.print("    func {s}{s}", .{name, funcStr});
        }
    } else {
        var cur: u32 = group.id;
        while (cur != cy.NullId) {
            const ofunc = vm.overloaded_funcs.buf[cur];
            const func = vm.funcSyms.buf[ofunc.id];
            if (!func.is_method) {
                cur = ofunc.next;
                continue;
            }

            const funcStr = vm.sema.formatFuncSig(func.sig, &cy.tempBuf, null) catch {
                return error.OutOfMemory;
            };
            try w.print("    func {s}{s}", .{name, funcStr});
            if (ofunc.next != cy.NullId) {
                try w.writeByte('\n');
            }
            cur = ofunc.next;
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
) !cy.fiber.PcFp {
    @setCold(true);
    // const func = try @call(.never_inline, getObjectFunctionFallback, .{obj, symId});
    const vals = framePtr[ret+CallArgStart+1..ret+CallArgStart+1+numArgs-1];
    const func = try getObjectFunctionFallback(vm, pc, recv, typeId, method, vals);
    _ = func;
    return vm.panic("Missing method.");
}

pub inline fn buildDynFrameInfo(cont: bool, comptime call_inst_off: u8, stack_size: u8) Value {
    return .{ .call_info = .{
        .ret_flag = !cont,
        .call_inst_off = call_inst_off,
        .stack_size = stack_size,
        .type = .dyn,
    }};
}

pub inline fn buildCallInfo2(cont: bool, comptime callInstOffset: u8, stack_size: u8) Value {
    return .{ .call_info = .{
        .ret_flag = !cont,
        .call_inst_off = callInstOffset,
        .stack_size = stack_size,
        .type = .vm,
    }};
}

pub inline fn buildCallInfo(comptime cont: bool, comptime callInstOffset: u8, stack_size: u8) Value {
    return .{ .call_info = .{
        .ret_flag = !cont,
        .call_inst_off = callInstOffset,
        .stack_size = stack_size,
        .type = .vm,
    }};
}

pub inline fn buildRootCallInfo(comptime cont: bool) Value {
    return .{ .call_info = .{
        .ret_flag = !cont,
        .call_inst_off = 0,
        .stack_size = 4,
        .type = .host,
    }};
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

fn preCallObjSym(vm: *VM, pc: [*]cy.Inst, fp: [*]Value, nargs: u8, ret: u8, final_ret: u8, func_stack_size: u8, req_type_check: bool, sig_id: sema.FuncSigId) !cy.TypeId {
    const sig = vm.compiler.sema.getFuncSig(sig_id);
    // Setup call frame to return to the next inst.
    fp[ret + 1] = buildDynFrameInfo(true, cy.bytecode.CallObjSymInstLen + cy.bytecode.RetDynLen, final_ret - ret);
    // Save ret box type.
    var ret_type_mask = sig.ret;
    if (vm.sema.isUnboxedType(sig.ret)) {
        ret_type_mask = ret_type_mask | 0x80000000;
    }
    fp[ret + 1].call_info.payload = ret_type_mask;
    fp[ret + 2] = Value{ .retPcPtr = pc + cy.bytecode.CallObjSymInstLen + cy.bytecode.RetDynLen };
    fp[ret + 3] = Value{ .retFramePtr = fp };

    // Setup the final call frame with args copied.
    // This allows arg unboxing so that post call release still operates on boxed args.
    fp[final_ret + 1] = buildCallInfo2(true, cy.bytecode.CallObjSymInstLen, func_stack_size);
    fp[final_ret + 2] = Value{ .retPcPtr = pc + cy.bytecode.CallObjSymInstLen};
    fp[final_ret + 3] = Value{ .retFramePtr = fp + ret };

    const args = fp[ret+CallArgStart..ret+CallArgStart+nargs];
    const final_args = fp[final_ret+CallArgStart..final_ret+CallArgStart+nargs];
    if (req_type_check) {
        // Perform type check on args.
        for (args, 0..) |arg, i| {
            const cstrType = sig.params_ptr[i];
            const argType = arg.getTypeId();

            // Arguments are already type checked when matching methods.
            if (arg.getTypeId() == bt.TagLit) {
                const final_arg = @call(.never_inline, inferArg, .{vm, arg, cstrType.type}) orelse {
                    return panicIncompatibleLambdaSig(vm, args, sig_id);
                };
                args[i] = final_arg;
                final_args[i] = final_arg;
                continue;
            }
            if (vm.sema.isUnboxedType(cstrType.type)) {
                const final_arg = @call(.never_inline, unbox, .{vm, arg, argType});
                final_args[i] = final_arg;
                continue;
            }
            final_args[i] = args[i];
        }
    } else {
        // Just copy args.
        @memcpy(final_args, args);
    }
    return sig.ret;
}

/// Assumes args are compatible.
/// Like `callSym` except inlines different op codes.
fn callMethod(
    vm: *VM, pc: [*]cy.Inst, fp: [*]cy.Value, func: rt.FuncSymbol,
    typeId: cy.TypeId, nargs: u8, ret: u8,
) !?cy.fiber.PcFp {
    switch (func.type) {
        .func => {
            const req_stack_size = func.data.func.stackSize + CallArgStart + func.nparams;
            if (@intFromPtr(fp + ret + req_stack_size) >= @intFromPtr(vm.c.stackEndPtr)) {
                return error.StackOverflow;
            }

            const final_ret = ret + 5 + func.nparams;
            _ = try preCallObjSym(vm, pc, fp, nargs, ret, final_ret, @intCast(func.data.func.stackSize), func.req_type_check, func.sig);

            // Optimize.
            // TODO: callObjFuncIC (rt args typecheck) or callObjFuncNoCheckIC.
            if (false) {
                pc[0] = cy.Inst.initOpCode(.callObjFuncIC);
                pc[7] = cy.Inst{ .val = @intCast(func.data.func.stackSize) };
                @as(*align(1) u32, @ptrCast(pc + 8)).* = func.data.func.pc;
                @as(*align(1) u16, @ptrCast(pc + 14)).* = @intCast(typeId);
            }

            return cy.fiber.PcFp{
                .pc = cy.fiber.toVmPc(vm, func.data.func.pc),
                .fp = fp + final_ret,
            };
        },
        .host_func => {
            defer if (cy.Trace) {
                vm.c.trace_indent -= 1;
            };
            const final_ret = ret + 5 + func.nparams;
            _ = try preCallObjSym(vm, pc, fp, nargs, ret, final_ret, 5 + func.nparams, func.req_type_check, func.sig);

            // Optimize.
            // TODO: callObjHostFuncIC (rt args typecheck) or callObjHostFuncNoCheckIC
            if (false) {
                pc[0] = cy.Inst.initOpCode(.callObjNativeFuncIC);
                @as(*align(1) u48, @ptrCast(pc + 8)).* = @intCast(@intFromPtr(func.data.hostFunc));
                @as(*align(1) u16, @ptrCast(pc + 14)).* = @intCast(typeId);
            }

            vm.c.pc = pc;
            vm.c.framePtr = fp + final_ret;
            // Overwrite last frame as a host frame.
            fp[final_ret + 1] = buildRootCallInfo(false);
            fp[final_ret + 2] = Value{ .retPcPtr = pc };
            // Keep retFramePtr.
            const res: Value = @bitCast(func.data.host_func.?(@ptrCast(vm)));
            if (res.isInterrupt()) {
                return error.Panic;
            }
            fp[final_ret] = res;

            return cy.fiber.PcFp{
                .pc = pc + cy.bytecode.CallObjSymInstLen,
                .fp = fp + ret,
            };
        },
        .null => {
            return vm.panic("Missing func null");
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

fn zCopyStruct(vm: *VM, obj: *HeapObject) callconv(.C) vmc.ValueResult {
    const val = copyStruct(vm, obj) catch {
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

pub fn copyStruct(vm: *VM, obj: *HeapObject) !cy.Value {
    const type_id = obj.getTypeId();
    const type_e = vm.c.types[type_id];

    const values = obj.object.getValuesPtr();
    const nfields = type_e.data.struct_t.nfields;
    var res: cy.Value = undefined;
    if (nfields <= 4) {
        res = try vm.allocEmptyObjectSmall(type_id);
    } else {
        res = try vm.allocEmptyObject(type_id, nfields);
    }

    const dst = res.castHeapObject(*cy.heap.Object).getValuesPtr();
    if (type_e.data.struct_t.has_boxed_fields) {
        for (type_e.data.struct_t.fields[0..nfields], 0..) |boxed, i| {
            dst[i] = values[i];
            if (boxed) {
                retain(vm, dst[i]);
            }
        }
    } else {
        @memcpy(dst[0..nfields], values[0..nfields]);
    }
    return res;
}

fn zBox(vm: *VM, val: cy.Value, type_id: cy.TypeId) callconv(.C) cy.Value {
    return box(vm, val, type_id) catch @panic("Unexpected.");
}

fn zUnbox(vm: *VM, val: cy.Value, type_id: cy.TypeId) callconv(.C) cy.Value {
    return unbox(vm, val, type_id);
}

fn zCallTrait(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, vtable_idx: u16, ret: u8) callconv(.C) vmc.PcFpResult {
    // Get func from vtable.
    const trait = framePtr[ret+4].asHeapObject();
    const vtable = vm.vtables.buf[trait.trait.vtable];
    const func_id = vtable[vtable_idx];
    const func = vm.funcSyms.buf[func_id];

    // Unwrap impl to first arg slot.
    framePtr[ret+5] = trait.trait.impl;

    const res = @call(.always_inline, VM.callSym, .{vm, pc, framePtr, func, ret}) catch |err| {
        if (err == error.Panic) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_PANIC,
            };
        } else if (err == error.StackOverflow) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_STACK_OVERFLOW,
            };
        } else {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_UNKNOWN,
            };
        }
    };
    return .{
        .pc = @ptrCast(res.pc),
        .fp = @ptrCast(res.fp),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

fn zCallSym(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, symId: u16, ret: u8) callconv(.C) vmc.PcFpResult {
    const func = vm.funcSyms.buf[symId];
    const res = @call(.always_inline, VM.callSym, .{vm, pc, framePtr, func, ret}) catch |err| {
        if (err == error.Panic) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_PANIC,
            };
        } else if (err == error.StackOverflow) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_STACK_OVERFLOW,
            };
        } else {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_UNKNOWN,
            };
        }
    };
    return .{
        .pc = @ptrCast(res.pc),
        .fp = @ptrCast(res.fp),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

fn zCallSymDyn(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, symId: u16, ret: u8, numArgs: u8) callconv(.C) vmc.PcFpResult {
    const res = @call(.always_inline, VM.callSymDyn, .{vm, pc, framePtr, @as(u32, @intCast(symId)), ret, numArgs}) catch |err| {
        if (err == error.Panic) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_PANIC,
            };
        } else if (err == error.StackOverflow) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_STACK_OVERFLOW,
            };
        } else {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_UNKNOWN,
            };
        }
    };
    return .{
        .pc = @ptrCast(res.pc),
        .fp = @ptrCast(res.fp),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

fn zDumpEvalOp(vm: *VM, pc: [*]const cy.Inst, fp: [*]const cy.Value) callconv(.C) void {
    dumpEvalOp(vm, pc, fp) catch cy.fatal();
}

pub fn zFreeObject(vm: *cy.VM, obj: *HeapObject) callconv(.C) void {
    cy.heap.freeObject(vm, obj, false);
} 

fn zEnd(vm: *cy.VM, pc: [*]const cy.Inst) callconv(.C) void {
    vm.endLocal = pc[1].val;
    vm.c.curFiber.pcOffset = @intCast(getInstOffset(vm, pc + 2));
}

fn zAllocLambda(vm: *cy.VM, rt_id: u32, ptr_t: cy.TypeId) callconv(.C) vmc.ValueResult {
    const func = vm.funcSyms.buf[rt_id];
    const func_ptr = cy.heap.allocBcFuncPtr(vm, ptr_t, func.data.func.pc,
        @intCast(func.nparams),
        @intCast(func.data.func.stackSize),
        @intCast(func.sig),
        func.req_type_check
    ) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(func_ptr),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

fn zAllocClosure(vm: *cy.VM, fp: [*]cy.Value, rt_id: u32, union_t: cy.TypeId, captures: [*]cy.Inst, ncaptures: u8, closure_local: u8) callconv(.C) vmc.ValueResult {
    const func_union = cy.heap.allocClosure(vm, fp, rt_id, union_t, captures[0..ncaptures], closure_local) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(func_union),
        .code = vmc.RES_CODE_SUCCESS,
    };
}

fn zAllocFuncPtr(vm: *cy.VM, ptr_t: cy.TypeId, rt_func: u16) callconv(.C) vmc.ValueResult {
    const func = cy.heap.allocFuncPtr(vm, ptr_t, vm.funcSyms.buf[rt_func]) catch {
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

fn zAllocArray(vm: *cy.VM, type_id: cy.TypeId, elemStart: [*]const Value, nElems: u8) callconv(.C) vmc.ValueResult {
    const arr = cy.heap.allocArray(vm, type_id, elemStart[0..nElems]) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_CODE_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(arr),
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
        const mb_res = callMethod(vm, pc, stack, func, typeId, numArgs, ret) catch |err| {
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
                .stack = @ptrCast(res.fp),
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
        .stack = @ptrCast(res.fp),
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

fn zPushFiber(vm: *cy.VM, curFiberEndPc: usize, curStack: [*]Value, fiber: *cy.Fiber, parentDstLocal: u8) callconv(.C) vmc.PcFp {
    const res = cy.fiber.pushFiber(vm, curFiberEndPc, curStack, fiber, parentDstLocal);
    return .{
        .pc = @ptrCast(res.pc),
        .fp = @ptrCast(res.fp),
    };
}

fn zPopFiber(vm: *cy.VM, curFiberEndPc: usize, curStack: [*]Value, retValue: Value) callconv(.C) vmc.PcFpOff {
    const fp = cy.fiber.getStackOffset(vm.c.stack, curStack);
    const res = cy.fiber.popFiber(vm, .{ .pc = @intCast(curFiberEndPc), .fp = fp }, retValue);
    return .{
        .pc = res.pc,
        .fp = res.fp,
    };
}

fn zFutureValue(vm: *cy.VM, mb_future: Value) callconv(.C) vmc.Value {
    const type_s = vm.c.types[mb_future.getTypeId()].sym;
    if (type_s.getVariant()) |variant| {
        if (variant.getSymTemplate() == vm.sema.future_tmpl) {
            const future = mb_future.castHostObject(*cy.heap.Future);
            vm.retain(future.val);
            return @bitCast(future.val);
        }
    }
    vm.retain(mb_future);
    return @bitCast(mb_future);
}

fn zAwait(vm: *cy.VM, value: Value) callconv(.C) vmc.ResultCode {
    const type_s = vm.c.types[value.getTypeId()].sym;
    if (type_s.getVariant()) |variant| {
        if (variant.getSymTemplate() == vm.sema.future_tmpl) {
            const future = value.castHostObject(*cy.heap.Future);
            if (future.completed) {
                // Continue if completed Future.
                return vmc.RES_CODE_SUCCESS;
            }

            // Persist fiber state.
            cy.fiber.saveCurFiber(vm);

            // Add task to future's continuation list.
            vm.retainObject(@ptrCast(vm.c.curFiber));
            const task_n = vm.alloc.create(cy.heap.AsyncTaskNode) catch @panic("error");
            task_n.* = .{
                .next = null,
                .task = .{
                    .type = .cont,
                    .data = .{ .cont = vm.c.curFiber },
                },
            };
            future.appendCont(task_n);

            return vmc.RES_CODE_AWAIT;
        }
    }
    return vmc.RES_CODE_SUCCESS;
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

fn zGetTypeField(vm: *VM, type_id: cy.TypeId, field_id: rt.FieldId) callconv(.C) vmc.TypeField {
    return vm.getTypeField(type_id, field_id);
}

fn zEvalCompare(left: Value, right: Value) callconv(.C) vmc.Value {
    return @bitCast(evalCompare(left, right));
}

fn zEvalCompareNot(left: Value, right: Value) callconv(.C) vmc.Value {
    return @bitCast(evalCompareNot(left, right));
}

fn zCall(vm: *VM, pc: [*]cy.Inst, framePtr: [*]Value, callee: Value, ret: u8, numArgs: u8) callconv(.C) vmc.PcFpResult {
    const res = call(vm, pc, framePtr, callee, ret, numArgs, true) catch |err| {
        if (err == error.Panic) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_PANIC,
            };
        } else if (err == error.StackOverflow) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_STACK_OVERFLOW,
            };
        } else {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_CODE_UNKNOWN,
            };
        }
    };
    return .{
        .pc = @ptrCast(res.pc),
        .fp = @ptrCast(res.fp),
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

pub fn zDumpValue(vm: *VM, val: Value) callconv(.C) void {
    var buf: [1024]u8 = undefined;
    var fbuf = std.io.fixedBufferStream(&buf);
    cy.debug.dumpValue(vm, fbuf.writer(), val, .{}) catch cy.fatal();
    cy.rt.log(fbuf.getWritten());
}

fn zGetFieldFallback(vm: *VM, obj: *HeapObject, field_id: rt.FieldId) callconv(.C) Value {
    const name_id = vm.c.fields[field_id].name_id;
    return vm.getFieldFallback(obj, name_id) catch {
        return Value.Interrupt;
    };
}

fn zSetFieldFallback(vm: *VM, obj: *HeapObject, field_id: rt.FieldId, val: cy.Value) callconv(.C) vmc.ResultCode {
    const name_id = vm.c.fields[field_id].name_id;
    vm.setFieldFallback(obj, name_id, val) catch |err| {
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
        @export(zAwait, .{ .name = "zAwait", .linkage = .strong });
        @export(zFutureValue, .{ .name = "zFutureValue", .linkage = .strong });
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
        @export(zAllocFuncPtr, .{ .name = "zAllocFuncPtr", .linkage = .strong });
        @export(zAllocLambda, .{ .name = "zAllocLambda", .linkage = .strong });
        @export(zAllocClosure, .{ .name = "zAllocClosure", .linkage = .strong });
        @export(zAlloc, .{ .name = "zAlloc", .linkage = .strong });
        @export(zAllocArray, .{ .name = "zAllocArray", .linkage = .strong });
        @export(zAllocList, .{ .name = "zAllocList", .linkage = .strong });
        @export(zAllocListDyn, .{ .name = "zAllocListDyn", .linkage = .strong });
        @export(zCopyStruct, .{ .name = "zCopyStruct", .linkage = .strong });
        @export(zBox, .{ .name = "zBox", .linkage = .strong });
        @export(zUnbox, .{ .name = "zUnbox", .linkage = .strong });
        @export(zCall, .{ .name = "zCall", .linkage = .strong });
        @export(zCallSym, .{ .name = "zCallSym", .linkage = .strong });
        @export(zCallTrait, .{ .name = "zCallTrait", .linkage = .strong });
        @export(zCallSymDyn, .{ .name = "zCallSymDyn", .linkage = .strong });
        @export(zCallObjSym, .{ .name = "zCallObjSym", .linkage = .strong });
        @export(zOpMatch, .{ .name = "zOpMatch", .linkage = .strong });
        @export(zOpCodeName, .{ .name = "zOpCodeName", .linkage = .strong });
        @export(zLog, .{ .name = "zLog", .linkage = .strong });
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
        @export(zGetTypeField, .{ .name = "zGetTypeField", .linkage = .strong });
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

pub const VMGetArgExt = struct {

    pub fn getByte(vm: *VM, idx: u32) u8 {
        return vm.c.framePtr[CallArgStart + idx].asByte();
    }

    pub fn getInt(vm: *VM, idx: u32) i64 {
        return vm.c.framePtr[CallArgStart + idx].asInt();
    }

    pub fn setInt(vm: *VM, idx: u32, i: i64) void {
        vm.c.framePtr[CallArgStart + idx] = Value.initInt(i);
    }    

    pub fn getPointer(vm: *VM, idx: u32) ?*anyopaque {
        return vm.c.framePtr[CallArgStart + idx].asPointer();
    }

    pub fn getFloat(vm: *VM, idx: u32) f64 {
        return vm.c.framePtr[CallArgStart + idx].asF64();
    }

    pub fn getString(vm: *VM, idx: u32) []const u8 {
        return vm.c.framePtr[CallArgStart + idx].asString();
    }

    pub fn getBool(vm: *VM, idx: u32) bool {
        return vm.c.framePtr[CallArgStart + idx].asBool();
    }

    pub fn setBool(vm: *VM, idx: u32, b: bool) void {
        vm.c.framePtr[CallArgStart + idx] = Value.initBool(b);
    }    

    pub fn getEnumValue(vm: *VM, idx: u32) u32 {
        return vm.c.framePtr[CallArgStart + idx].getEnumValue();
    }

    pub fn getSymbol(vm: *VM, idx: u32) u32 {
        return vm.c.framePtr[CallArgStart + idx].asSymbolId();
    }

    pub fn setSymbol(vm: *VM, idx: u32, id: u32) void {
        vm.c.framePtr[CallArgStart + idx] = Value.initSymbol(id);
    }    

    pub fn getObject(vm: *VM, comptime Ptr: type, idx: u32) Ptr {
        return vm.c.framePtr[CallArgStart + idx].castHeapObject(Ptr);
    }

    pub fn getHostObject(vm: *VM, comptime Ptr: type, idx: u32) Ptr {
        return vm.c.framePtr[CallArgStart + idx].castHostObject(Ptr);
    }

    pub fn getValue(vm: *VM, idx: u32) Value {
        return vm.c.framePtr[CallArgStart + idx];
    }
};

pub fn getFuncPtrType(vm: *cy.VM, sig: cy.sema.FuncSigId) !cy.TypeId {
    const chunk = vm.sema.func_ptr_tmpl.chunk();
    return cy.sema.getFuncPtrType(chunk, sig);
}