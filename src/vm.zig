const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const aarch64 = builtin.cpu.arch == .aarch64;
const stdx = @import("stdx");
const fatal = cy.fatal;
const zt = stdx.testing;
const tcc = @import("tcc");

const vmc = @import("vmc");
const fmt = @import("fmt.zig");
const v = fmt.v;
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const bt = cy.types.BuiltinTypes;
const sema = cy.sema;
const types = cy.types;
const worker = cy.worker;
const core = @import("builtins/core.zig");
const bindings = @import("builtins/bindings.zig");
const bc = @import("bc_gen.zig");
const Value = cy.Value;
const debug = @import("debug.zig");
const http = @import("http.zig");
const HeapObject = cy.HeapObject;
const UserVM = cy.UserVM;

const logger = cy.log.scoped(.vm);

const trace_panic_at_main_heap_event: u64 = 0;

pub export threadlocal var cur_thread: ?*cy.Thread = null;

const VMC = extern struct {
    consts_ptr: [*]Value,
    consts_len: usize,

    rw_lock: cy.sync.RWLock,

    types_ptr: [*]vmc.TypeInfo,
    types_len: usize,
};

pub const VM = struct {
    c: VMC,

    alloc: std.mem.Allocator,

    main_thread: *cy.Thread,

    // Not protected for incremental compilation.
    globals: std.ArrayList(*GlobalSym),

    shared_states: std.ArrayList(*u8),

    // Protects `shared_states`.
    // TODO: Make lock-free.
    shared_state_mutex: std.Thread.Mutex = .{},

    /// Functions are relocated at compile-time except extern functions which are generated at runtime.
    funcSyms: std.ArrayList(FuncSymbol),

    /// Host func ptr to rt func id.
    host_funcs: std.AutoHashMapUnmanaged(*anyopaque, usize),

    /// `TRACE` mode only.
    types: std.ArrayList(TypeInfo),

    funcSymDetails: std.ArrayList(FuncSymDetail),

    /// Stores insts that were modified to restore them later.
    inlineSaves: std.AutoHashMapUnmanaged(u32, [*]const u8),

    /// For REPL to persist locals from previous run.
    env_local_saves: std.ArrayList(LocalSave),
    env_local_saves_map: std.StringHashMapUnmanaged(usize),

    dyn_libs: if (cy.hasFFI) std.ArrayList(*std.DynLib) else std.ArrayList(*anyopaque),

    next_thread_id: usize,

    u8Buf: std.ArrayListAligned(u8, .@"8"),

    /// Tasks in this queue are ready to be executed (not in a waiting state).
    /// `queueTask` appends tasks here.
    ready_tasks: cy.fifo.LinearFifo(cy.heap.AsyncTask, .Dynamic),
    task_nodes: std.ArrayList(cy.heap.AsyncTaskNode),

    /// vtables for trait impls, each vtable contains func ids.
    vtables: std.ArrayList([]u64),

    /// Sorted by buffer pointer.
    debug_tables: []const cy.DebugTableEntry, 

    worker_pool: worker.Pool,

    compiler: *cy.Compiler,
    sema: *cy.sema.Sema,

    /// User data ptr. Useful for embedders.
    userData: ?*anyopaque,

    /// Map of arbitrary data pointers.
    data: std.StringHashMapUnmanaged(?*anyopaque),

    /// Host write hook.
    print: C.PrintFn,
    print_err: C.PrintErrorFn,
    log: C.LogFn,

    /// Interface used for imports and fetch.
    httpClient: http.HttpClient,
    stdHttpClient: if (cy.hasCLI) *http.StdHttpClient else *anyopaque,

    /// One TCC state for `bindLib`.
    tcc_state: if (cy.hasFFI) ?*tcc.TCCState else ?*anyopaque,
    placeholder: Value = undefined,

    config: C.EvalConfig,

    /// Whether this VM is already deinited. Used to skip the next deinit to avoid using undefined memory.
    deinited: bool,
    padding7: bool = undefined,

    tempBuf: [128]u8 align(4),

    last_exe_error: []const u8,
    last_res: C.ResultCode,

    num_evals: u32,

    /// Save vm state at the end of execution so that a subsequent eval knows where it left off
    /// from a the previous bytecode buffer.
    num_cont_evals: u32,

    pub fn init(self: *VM, alloc: std.mem.Allocator) !void {
        const num_cpus = try std.Thread.getCpuCount();
        self.* = .{
            .alloc = alloc,
            .compiler = undefined,
            .sema = undefined,
            .worker_pool = try worker.Pool.init(self, num_cpus-1),
            .globals = .{},
            .shared_states = .{},
            .c = .{
                .consts_ptr = undefined,
                .consts_len = 0,
                .types_ptr = undefined,
                .types_len = 0,
                .rw_lock = .{},
            },
            .next_thread_id = 1,
            .main_thread = undefined,
            .funcSyms = .{},
            .host_funcs = .{},
            .types = .{},
            .funcSymDetails = .{},
            .inlineSaves = .{},
            .u8Buf = .{},
            .debug_tables = &.{},
            // Initialize to NullId to indicate vm is still in initing.
            .deinited = false,
            .config = undefined,
            .httpClient = undefined,
            .stdHttpClient = undefined,
            .userData = null,
            .data = .{},
            .print = default_print,
            .print_err = default_print_err,
            .log = default_log,
            .tempBuf = undefined,
            .last_exe_error = "",
            .last_res = C.Success,
            .num_evals = 0,
            .num_cont_evals = 0,
            .ready_tasks = cy.fifo.LinearFifo(cy.heap.AsyncTask, .Dynamic).init(alloc),
            .task_nodes = .{},
            .vtables = .{},
            .env_local_saves = .{},
            .env_local_saves_map = .{},
            .dyn_libs = .{},
            .tcc_state = null,
        };
        self.compiler = try self.alloc.create(cy.Compiler);
        self.sema = &self.compiler.sema;
        try self.compiler.init(self);

        self.main_thread = try self.alloc.create(cy.Thread);
        try self.main_thread.init(0, alloc, self);
        if (cy.Trace) {
            self.main_thread.heap.c.trace_panic_at_event = trace_panic_at_main_heap_event;
        }

        if (cy.hasCLI) {
            self.stdHttpClient = try alloc.create(http.StdHttpClient);
            self.stdHttpClient.* = http.StdHttpClient.init(self.alloc);
            self.httpClient = self.stdHttpClient.iface();
        }

        try self.funcSyms.ensureTotalCapacityPrecise(self.alloc, 255);

        const data = try self.alloc.create(cy.core.BuiltinsData);
        try self.data.put(self.alloc, "builtins", data);

        try @call(.never_inline, cy.bindings.bindCore, .{self});
    }

    pub fn deinit(self: *VM, reset: bool) void {
        if (self.deinited) {
            return;
        }

        if (!reset) {
            const data = self.getData(*cy.core.BuiltinsData, "builtins");
            self.alloc.destroy(data);
            _ = self.data.remove("builtins");
        }

        if (!reset) {
            if (cy.hasFFI) {
                if (self.tcc_state) |state| {
                    tcc.tcc_delete(state);
                }
                for (self.dyn_libs.items) |lib| {
                    lib.close();
                    self.alloc.destroy(lib);
                }
                self.dyn_libs.deinit(self.alloc);
            }
        }

        for (self.env_local_saves.items) |save| {
            save.deinit(self.alloc);
        }
        if (reset) {
            self.env_local_saves.clearRetainingCapacity();
            self.env_local_saves_map.clearRetainingCapacity();
        } else {
            self.env_local_saves.deinit(self.alloc);
            self.env_local_saves_map.deinit(self.alloc);
        }

        // Deinit compiler first since it depends on buffers from parser.
        if (reset) {
            self.compiler.deinit(true);
        } else {
            self.compiler.deinit(false);
            self.alloc.destroy(self.compiler);
        }

        if (reset) {
            self.ready_tasks.deinit();
            self.ready_tasks = cy.fifo.LinearFifo(cy.heap.AsyncTask, .Dynamic).init(self.alloc);
            self.task_nodes.clearRetainingCapacity();
        } else {
            self.ready_tasks.deinit();
            self.task_nodes.deinit(self.alloc);
        }

        for (self.types.items) |*info| {
            info.deinit(self.alloc);
        }

        if (reset) {
            self.funcSyms.clearRetainingCapacity();
            self.host_funcs.clearRetainingCapacity();
            self.types.clearRetainingCapacity();
            self.funcSymDetails.clearRetainingCapacity();
        } else {
            self.funcSyms.deinit(self.alloc);
            self.host_funcs.deinit(self.alloc);
            self.types.deinit(self.alloc);
            self.funcSymDetails.deinit(self.alloc);
        }

        for (self.shared_states.items) |state| {
            self.alloc.destroy(state);
        }
        if (reset) {
            self.shared_states.clearRetainingCapacity();
        } else {
            self.shared_states.deinit(self.alloc);
        }

        for (self.globals.items) |global| {
            global.deinit(self.alloc);
            self.alloc.destroy(global);
        }
        if (reset) {
            self.globals.clearRetainingCapacity();
        } else {
            self.globals.deinit(self.alloc);
        }

        self.c.types_len = 0;

        if (reset) {
            self.u8Buf.clearRetainingCapacity();
        } else {
            self.u8Buf.deinit(self.alloc);
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

        for (self.vtables.items) |vtable| {
            self.alloc.free(vtable);
        }
        if (reset) {
            self.vtables.clearRetainingCapacity();
        } else {
            self.vtables.deinit(self.alloc);
        }

        if (!reset) {
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

        if (!reset) {
            self.worker_pool.deinit(self.alloc);
        }

        self.main_thread.deinit(reset);
        if (!reset) {
            self.alloc.destroy(self.main_thread);
        }

        self.alloc.free(self.last_exe_error);
        self.last_exe_error = "";
    }

    pub fn getConstBytes(self: *VM) []const u8 {
        return self.c.const_bytes[0..self.c.const_bytes_len];
    }

    pub fn validate(self: *VM, srcUri: []const u8, src: ?[]const u8, config: C.ValidateConfig) !void {
        var compile_c = C.defaultCompileConfig();
        compile_c.file_modules = config.file_modules;
        compile_c.skip_codegen = true;
        _ = try self.compile(srcUri, src, compile_c);
    }

    pub fn compile(self: *VM, srcUri: []const u8, src: ?[]const u8, config: C.CompileConfig) !cy.CompileResult {
        try self.resetVM();
        self.config = C.defaultEvalConfig();
        self.config.single_run = config.single_run;
        self.config.file_modules = config.file_modules;
        self.config.gen_all_debug_syms = true;

        var tt = cy.debug.timer();
        const res = try self.compiler.compile(srcUri, src, config);

        // Sync debug table for debug output.
        if (!config.skip_codegen and config.backend == C.BackendVM) {
            self.debug_tables = self.compiler.debug_tables.items;
        }
        tt.endPrintVerbose("compile");
        return res;
    }

    pub fn resetVM(self: *VM) !void {
        self.deinit(true);
        try @call(.never_inline, cy.bindings.bindCore, .{self});

        self.num_cont_evals = 0;

        // Reset flags for next reset/deinit.
        self.deinited = false;

        // Before reinit, everything in VM, VMcompiler should be cleared.

        try self.compiler.reinitPerRun();
        self.compiler.cont = false;
    }

    pub fn api(self: *VM) *C.ZVM {
        return @ptrCast(self);
    }

    pub fn eval(self: *VM, src_uri: []const u8, src: ?[]const u8, config: C.EvalConfig) !C.EvalResult {
        var tt = cy.debug.timer();

        self.config = config;
        try self.compiler.reinitPerRun();

        var compile_c = C.defaultCompileConfig();
        compile_c.single_run = config.single_run;
        compile_c.file_modules = config.file_modules;
        compile_c.gen_all_debug_syms = cy.Trace;
        compile_c.backend = config.backend;
        compile_c.persist_main_locals = config.persist_main_locals;
        const res = try self.compiler.compile(src_uri, src, compile_c);
        tt.endPrintVerbose("compile");

        if (config.backend == C.BackendJIT) {
            if (cy.isFreestanding or cy.isWasm) {
                return error.Unsupported;
            }

            const jitRes = res.jit.buf;
            try self.main_thread.stackEnsureTotalCapacity(res.jit.mainStackSize);
            self.main_thread.c.fp = @ptrCast(self.main_thread.c.stack_ptr);

            // Mark code executable.
            const PROT_READ = 1;
            const PROT_WRITE = 2;
            const PROT_EXEC = 4;
            try std.posix.mprotect(jitRes.buf.items.ptr[0..jitRes.buf.capacity], PROT_READ | PROT_EXEC);

            // Memory must be reset to original setting in order to be freed.
            defer std.posix.mprotect(jitRes.buf.items.ptr[0..jitRes.buf.capacity], PROT_WRITE) catch cy.fatal();

            // if (jitRes.buf.items.len > 500*4) {
            //     logger.tracev("jit code (size: {}) {}...", .{jitRes.buf.items.len, std.fmt.fmtSliceHexLower(jitRes.buf.items[0..100*4])});
            // } else {
            //     logger.tracev("jit code (size: {}) {}", .{jitRes.buf.items.len, std.fmt.fmtSliceHexLower(jitRes.buf.items)});
            // }

            // const bytes = jitRes.buf.items[jitRes.mainPc..jitRes.mainPc+12*4];
            // logger.tracev("main start {}: {}", .{jitRes.mainPc, std.fmt.fmtSliceHexLower(bytes)});

            const main: *const fn(*VM, [*]Value) callconv(.c) void = @ptrCast(@alignCast(jitRes.buf.items.ptr + jitRes.mainPc));
            // @breakpoint();
            main(self, @ptrCast(self.main_thread.c.fp));
            return .{
                .res = null,
                .res_t = bt.Void,
            };
        } else if (config.backend == C.BackendVM) {
            if (C.verbose()) {
                try debug.dumpBytecode(self, .{});
            }

            if (cy.Trace) {
                var i: u32 = 0;
                while (i < self.main_thread.c.trace.opCounts.len) : (i += 1) {
                    self.main_thread.c.trace.opCounts[i] = .{
                        .code = i,
                        .count = 0,
                    };
                }
                self.main_thread.c.trace.totalOpCounts = 0;
                self.main_thread.heap.c.numReleases = 0;
                self.main_thread.heap.c.numRetains = 0;
            }

            tt = cy.debug.timer();
            defer tt.endPrintVerbose("eval");

            const val = try self.exec_bytecode(res.vm);
            return .{
                .res = @ptrCast(val),
                .res_t = if (self.compiler.main_func) |main_func| main_func.sig.ret.id() else bt.Void,
            };
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
                    self.alloc.free(self.last_exe_error);
                    self.last_exe_error = "";
                    return error.Panic;
                }
            } else {
                const exeRes = try std.process.Child.run(.{
                    .allocator = self.alloc,
                    .argv = &.{res.aot.exePath},
                });
                self.alloc.free(exeRes.stdout);
                if (exeRes.term != .Exited or exeRes.term.Exited != 0) {
                    self.alloc.free(self.last_exe_error);
                    self.last_exe_error = exeRes.stderr;
                    return error.Panic;
                }
            }
            return .{
                .res = null,
                .res_t = bt.Void,
            };
        }
    }

    pub fn dumpStats(self: *const VM) void {
        const S = struct {
            fn opCountLess(_: void, a: vmc.OpCount, b: vmc.OpCount) bool {
                return a.count > b.count;
            }
        };
        std.debug.print("main ops evaled: {}\n", .{self.main_thread.c.trace.totalOpCounts});
        std.sort.pdq(vmc.OpCount, &self.main_thread.c.trace.opCounts, {}, S.opCountLess);
        var i: u32 = 0;

        while (i < vmc.NumCodes) : (i += 1) {
            if (self.main_thread.c.trace.opCounts[i].count > 0) {
                const op = std.meta.intToEnum(cy.OpCode, self.main_thread.c.trace.opCounts[i].code) catch continue;
                std.debug.print("\t{s} {}\n", .{@tagName(op), self.main_thread.c.trace.opCounts[i].count});
            }
        }
    }

    pub fn dumpInfo(self: *VM) !void {
        const w = std.debug.lockStderrWriter(&cy.debug.print_buf);
        defer std.debug.unlockStderrWriter();

        try w.print("main stack size: {}\n", .{self.main_thread.c.stack_len});
        try w.print("main stack fp: {}\n", .{cy.thread.getStackOffset(self.main_thread.c.stack_ptr, self.main_thread.c.fp)});
        try w.print("main heap pages: {}\n", .{self.main_thread.heap.heapPages.items.len});
        if (cy.TrackGlobalRC) {
            try w.print("main global rc: {}\n", .{self.main_thread.heap.c.refCounts});
        }

        // Dump func symbols.
        {
            try w.print("func syms:\n", .{});
            for (self.funcSyms.items, 0..) |_, i| {
                const details = self.funcSymDetails.items[i];
                const name = details.name_ptr[0..details.name_len];
                const sig = self.sema.getFuncSig(details.sig);
                const sigStr = try self.sema.formatFuncSig(sig, &cy.tempBuf, null);
                try w.print("\t{s}{s}: {}\n", .{name, sigStr, i});
            }
        }
    }

    pub fn getData(self: *cy.VM, comptime T: type, key: []const u8) T {
        return @ptrCast(@alignCast(self.data.get(key).?));
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
        try self.prepEval(buf);
    }

    pub fn prepEval(self: *VM, buf: *cy.ByteCodeBuffer) !void {
        self.main_thread.free_panic();
        self.main_thread.c.panic_type = vmc.PANIC_NONE;
        self.debug_tables = self.compiler.debug_tables.items;
    
        self.main_thread.c.pc = @ptrCast(&buf.ops.items[buf.main_pc]);

        try self.main_thread.stackEnsureTotalCapacity(buf.mainStackSize);
        self.main_thread.c.fp = @ptrCast(self.main_thread.c.stack_ptr);

        self.c.consts_ptr = @ptrCast(buf.consts.items.ptr);
        self.c.consts_len = buf.consts.items.len;
    }

    pub fn exec_bytecode(self: *VM, buf: *cy.ByteCodeBuffer) !*Value {
        if (buf.ops.items.len == 0) {
            return error.NoEndOp;
        }

        try self.prepEval(buf);
        defer {
            self.num_cont_evals += 1;
            self.num_evals += 1;
        }

        try @call(.never_inline, cy.Thread.exec_auto, .{self.main_thread, true});
        logger.tracev("main stack size: {}", .{buf.mainStackSize});

        // NOTE: %1 happens to be reserved for _main_res which might not be true if the program bootstrap changes.
        return &self.main_thread.c.stack_ptr[1];
    }

    pub fn addFunc(self: *VM, sema_func: ?*cy.Func, name: []const u8, sig: cy.sema.FuncSigId, func: FuncSymbol) !u32 {
        const id = self.funcSyms.items.len;
        try self.funcSyms.append(self.alloc, func);

        try self.funcSymDetails.append(self.alloc, .{
            .func = sema_func,
            .name_ptr = name.ptr, .name_len = @intCast(name.len), .sig = sig,
        });
        return @intCast(id);
    }

    pub fn getType(self: *const VM, id: cy.TypeId) vmc.TypeInfo {
        return self.c.types_ptr[id];
    }

    pub fn setApiError(self: *const VM, str: []const u8) !void {
        self.compiler.hasApiError = true;
        self.alloc.free(self.compiler.apiError);
        self.compiler.apiError = try self.alloc.dupe(u8, str);
    }

    pub fn clearTempString(vm: *VM) std.ArrayListAlignedUnmanaged(u8, .@"8").Writer {
        vm.u8Buf.clearRetainingCapacity();
        return vm.u8Buf.writer(vm.alloc);
    }

    pub fn getTempString(vm: *VM) []const u8 {
        return vm.u8Buf.items;
    }

    pub fn getNewFramePtrOffset(self: *VM, args: [*]const Value, nargs: u8) u32 {
        return @intCast(cy.fiber.getStackOffset(self.c.stack, args + nargs));
    }

    pub fn expandTypeTemplate(vm: *cy.VM, template: *cy.sym.Template, arg_types: []const *cy.Type, args: []const cy.Value) !*cy.Type {
        const chunk = vm.compiler.chunks.items[template.head.declNode().src()];
        const sym = try cy.template.expand_template(chunk, template, arg_types, args);
        return sym.getStaticType().?;
    }

    pub fn findType(vm: *cy.VM, spec: []const u8) !?*cy.Type {
        if (vm.compiler.chunks.items.len == 0) {
            return null;
        }

        // First check the cache.
        if (vm.compiler.find_type_cache.get(spec)) |type_| {
            return type_;
        }

        // Look in main first.
        const main_mod = vm.compiler.main_chunk.sym.getMod();
        if (main_mod.getSym(spec)) |sym| {
            if (sym.getStaticType()) |type_| {
                const spec_dup = try vm.alloc.dupe(u8, spec);
                try vm.compiler.find_type_cache.put(vm.alloc, spec_dup, type_);
                return type_;
            }
        }

        // Look in builtins.
        const b_mod = vm.compiler.builtins_chunk.sym.getMod();
        if (b_mod.getSym(spec)) |sym| {
            if (sym.getStaticType()) |type_| {
                const spec_dup = try vm.alloc.dupe(u8, spec);
                try vm.compiler.find_type_cache.put(vm.alloc, spec_dup, type_);
                return type_;
            }
        }

        // Evaluate compile-time value.
        // Assumes that there is no dependency on api_chunk's source for subsequent use.
        const c = vm.compiler.api_chunk;
        vm.alloc.free(c.src);
        c.src = try vm.alloc.dupe(u8, spec);
        try parseTypeExpr(vm.compiler, c);

        try cy.sema.pushChunkResolveContext(c, null);
        defer cy.sema.popResolveContext(c);

        const expr_stmt = c.ast.root.?.stmts.ptr[0].cast(.exprStmt);
        const ct_expr = cy.cte.evalExprInfer(c, expr_stmt.child) catch return null;
        const res = cy.cte.deriveCtExprType2(c, ct_expr) catch return null;
        const spec_dup = try vm.alloc.dupe(u8, spec);
        try vm.compiler.find_type_cache.put(vm.alloc, spec_dup, res);
        return res;
    } 

    pub fn parseTypeExpr(self: *cy.Compiler, chunk: *cy.Chunk) !void {
        _ = self;
        var tt = cy.debug.timer();

        const S = struct {
            fn parserReport(ctx: *anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror {
                const c: *cy.Chunk = @ptrCast(@alignCast(ctx));
                _ = try c.compiler.addReportFmt(.parse_err, format, args, c.id, pos);
                return error.ParseError;
            }

            fn tokenizerReport(ctx: *anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror!void {
                const c: *cy.Chunk = @ptrCast(@alignCast(ctx));
                _ = try c.compiler.addReportFmt(.token_err, format, args, c.id, pos);
                return error.TokenError;
            }
        };

        chunk.parser.reportFn = S.parserReport;
        chunk.parser.tokenizerReportFn = S.tokenizerReport;
        chunk.parser.ctx = chunk;

        const res = try chunk.parser.parse(chunk.src, chunk.id, .{ .parse_func_types = true });
        tt.endPrintVerbose("parse");
        // Update buffer pointers so success/error paths can access them.
        chunk.updateAstView(res.ast);
        if (res.has_error) {
            return error.CompileError;
        }
    }
};

test "vm internals." {
    try zt.eq(@alignOf(VM), 8);

    // Check Zig/C structs.
    inline for (std.meta.fields(vmc.VM)) |field| {
        try zt.eq(@offsetOf(vmc.VM, field.name), @offsetOf(VMC, field.name));
    }
    // inline for (std.meta.fields(vmc.GlobalSym)) |field| {
    //     try zt.eq(@offsetOf(vmc.GlobalSym, field.name), @offsetOf(GlobalSym, field.name));
    // }
    inline for (std.meta.fields(vmc.TypeInfo)) |field| {
        try zt.eq(@offsetOf(vmc.TypeInfo, field.name), @offsetOf(TypeInfo, field.name));
    }

    try zt.eq(@offsetOf(VM, "c"), @offsetOf(vmc.ZVM, "c"));

    try zt.eq(16, @sizeOf(FuncSymbol));
    try zt.eq(@offsetOf(FuncSymbol, "type"), @offsetOf(vmc.FuncSymbol, "type"));
    try zt.eq(@offsetOf(FuncSymbol, "data"), @offsetOf(vmc.FuncSymbol, "data"));
}

pub const SymbolId = u64;

const Root = @This();

/// Generate assembly labels to find sections easier.
const GenLabels = builtin.mode != .Debug and !builtin.cpu.arch.isWasm() and false;

const DebugTraceStopAtNumOps: ?u32 = null;

fn panicCastAbstractError(vm: *cy.VM, val: Value, expTypeSymId: cy.sema.SymbolId) !void {
    const sym = vm.compiler.sema.getSymbol(expTypeSymId);
    const name = cy.sema.getName(&vm.compiler, sym.key.resolvedSymKey.nameId);
    return vm.panicFmt("Can not cast `{}` to `{}`.", &.{
         v(vm.c.types_ptr.buf[val.getBoxType()].name), v(name), 
    });
}

fn panicCastError(vm: *cy.VM, val: Value, expTypeId: cy.TypeId) !void {
    return vm.panicFmt("Can not cast `{}` to `{}`.", &.{
         v(vm.c.types_ptr.buf[val.getBoxType()].name), v(vm.c.types_ptr.buf[expTypeId].name), 
    });
}

fn allocValueTypes(vm: *cy.VM, vals: []const Value) ![]const *cy.Type {
    const typeIds = try vm.alloc.alloc(*cy.Type, vals.len);
    for (vals, 0..) |val, i| {
        typeIds[i] = vm.sema.getType(val.getRefeeType());
    }
    return typeIds;
}

fn allocValueTypeIds(vm: *cy.VM, vals: []const Value) ![]const cy.TypeId {
    const typeIds = try vm.alloc.alloc(cy.TypeId, vals.len);
    for (vals, 0..) |val, i| {
        typeIds[i] = val.getBoxType();
    }
    return typeIds;
}

/// Like Value.dump but shows heap values.
pub fn dumpValue(vm: *const VM, val: Value) void {
    if (val.isFloat()) {
        fmt.printStdout("Float {}\n", &.{ v(val.asF64()) });
    } else {
        if (val.isBoxPtr()) {
            const obj = val.asHeapObject();
            switch (obj.getTypeId()) {
                bt.Str => {
                    const str = obj.string.slice();
                    if (str.len > 20) {
                        fmt.printStdout("String {} len={} str=\"{}\"...\n", &.{v(obj), v(str.len), v(str[0..20])});
                    } else {
                        fmt.printStdout("String {} len={} str=\"{}\"\n", &.{v(obj), v(str.len), v(str)});
                    }
                },
                bt.Fiber => fmt.printStdout("Fiber {}\n", &.{v(obj)}),
                else => {
                    const name = vm.compiler.sema.types.items[obj.getTypeId()].name();
                    fmt.printStdout("HeapObject {} {} {}\n", &.{v(obj), v(obj.getTypeId()), v(name)});
                },
            }
        } else {
            fmt.printStdout("{}\n", &.{v(val.val)});
        }
    }
}

inline fn deoptimizeBinOp(pc: [*]cy.Inst) void {
    pc[0] = cy.Inst.initOpCode(.callObjSym);
    pc[1] = pc[8];
    pc[2] = pc[9];
    pc[3] = pc[10];
    pc[4] = pc[11];
}

fn zFatal() callconv(.c) void {
    cy.fatal();
}

fn zOpCodeName(code: vmc.OpCode) callconv(.c) [*:0]const u8 {
    const ecode: cy.OpCode = @enumFromInt(code);
    return @tagName(ecode);
}

fn zCallTrait(t: *cy.Thread, pc: [*]cy.Inst, fp: [*]Value, vtable_idx: u16, base: u16) callconv(.c) vmc.PcFpResult {
    // Get func from vtable.
    const trait_vtable: usize = @intCast(fp[base+3].val);
    const vtable = t.c.vm.vtables.items[trait_vtable];
    const ptr_info = vtable[vtable_idx];
    const ptr: *anyopaque = @ptrFromInt(ptr_info & ~(@as(u64, 1) << 63));
    const host_func = (ptr_info >> 63) != 0;

    const res = @call(.always_inline, cy.Thread.callTraitSymInst, .{t, pc, fp, ptr, host_func, base}) catch |err| {
        if (err == error.Panic) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_PANIC,
            };
        } else if (err == error.StackOverflow) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_STACK_OVERFLOW,
            };
        } else {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_UNKNOWN,
            };
        }
    };
    return .{
        .pc = @ptrCast(res.pc),
        .fp = @ptrCast(res.fp),
        .code = vmc.RES_SUCCESS,
    };
}

fn z_dump_thread_inst(t: *cy.Thread, pc: [*]const cy.Inst) callconv(.c) void {
    cy.thread.print_thread_inst(t, pc) catch cy.fatal();
}

pub fn zDestroyObject(t: *cy.Thread, obj: *HeapObject) callconv(.c) void {
    t.heap.destroyObject(obj, false, {});
} 

pub fn zFreePoolObject(t: *cy.Thread, obj: *HeapObject) callconv(.c) void {
    t.heap.freePoolObject(obj);
} 

pub fn zFreeBigObject(t: *cy.Thread, obj: *HeapObject, size: usize) callconv(.c) void {
    t.heap.freeBigObject(obj, size);
} 

pub fn zPrintTraceAtPc(vm: *cy.VM, debug_pc: ?*anyopaque, title: C.Bytes, msg: C.Bytes) callconv(.c) void {
    const w = std.debug.lockStderrWriter(&cy.debug.print_buf);
    defer std.debug.unlockStderrWriter();
    cy.debug.write_trace_at_pc(vm, w, debug_pc, C.from_bytes(title), C.from_bytes(msg)) catch cy.fatal();
}

pub fn zGetExternFunc(vm: *cy.VM, func_id: u32) callconv(.c) *const anyopaque {
    vm.c.rw_lock.read_lock();
    defer vm.c.rw_lock.read_unlock();

    const res = vm.funcSyms.items.ptr[func_id].data.host_func;
    return @ptrCast(res);
}

pub fn zDumpObjectTrace(t: *cy.Thread, obj: *HeapObject) callconv(.c) void {
    const w = std.debug.lockStderrWriter(&cy.debug.print_buf);
    defer std.debug.unlockStderrWriter();
    cy.debug.write_object_trace(t, w, obj) catch cy.fatal();
}

fn zAllocFuncUnion(t: *cy.Thread, type_id: cy.TypeId, func_ptr: Value) callconv(.c) vmc.ValueResult {
    const func_union = t.heap.allocFunc(type_id, func_ptr) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(func_union),
        .code = vmc.RES_SUCCESS,
    };
}

fn zAllocClosure(t: *cy.Thread, fp: [*]cy.Value, func_pc: [*]cy.Inst, union_t: cy.TypeId, captures: [*]cy.Inst, ncaptures: u8, pinned_closure: bool) callconv(.c) vmc.ValueResult {
    const func_union = t.heap.allocClosure(fp, func_pc, union_t, captures, ncaptures, pinned_closure) catch {
        return .{
            .val = undefined,
            .code = vmc.RES_UNKNOWN,
        };
    };
    return .{
        .val = @bitCast(func_union),
        .code = vmc.RES_SUCCESS,
    };
}

fn z_ret_generator(t: *cy.Thread, type_id: cy.TypeId, ret_size: u16, fp: [*]Value, reg_end: usize, resume_pc: ?*cy.Inst, deinit_pc: ?*cy.Inst) callconv(.c) vmc.ResultCode {
    cy.thread.ret_generator(&t.heap, type_id, ret_size, fp[0..reg_end], resume_pc, deinit_pc) catch {
        return vmc.RES_UNKNOWN;
    };
    return vmc.RES_SUCCESS;
}

fn zAwait(t: *cy.Thread, fut_value_ptr: Value) callconv(.c) vmc.ResultCode {
    const future = fut_value_ptr.asPtr(*cy.heap.FutureValue);
    if (future.shared_state) |shared_state| {
        var state = @atomicLoad(u8, shared_state, .acquire);
        if (state == 2) {
            return vmc.RES_SUCCESS;
        }

        if (t == t.c.vm.main_thread) {
            // Block until future is done.
            while (true) {
                state = @atomicLoad(u8, shared_state, .acquire);
                if (state == 2) {
                    break;
                }
                std.atomic.spinLoopHint();
            }
            return vmc.RES_SUCCESS;
        } else {
            // // Add task to future's continuation list.
            // const task_id = vm.task_nodes.items.len;
            // const task = cy.heap.ContinuationTask{
            //     .thread = t,
            // };
            // vm.task_nodes.append(vm.alloc, .{
            //     .next = cy.NullId,
            //     .task = .{
            //         .type = .cont,
            //         .data = .{ .cont = task },
            //     },
            // }) catch @panic("error");
            // appendCont(vm, future, task_id);
            // return vmc.RES_AWAIT;
            @panic("TODO");
        }
    } else {
        if (!future.completed) {
            _ = t.ret_panic("Local future will never complete.");
            return vmc.RES_PANIC;
        }
        // Continue if completed Future.
        return vmc.RES_SUCCESS;
    }
}

pub fn appendCont(vm: *cy.VM, fut: *cy.heap.Future, node_id: usize) void {
    if (fut.inner.cont_head == cy.NullId) {
        fut.inner.cont_head = node_id;
        fut.inner.cont_tail = node_id;
        return;
    }
    vm.task_nodes.items[fut.inner.cont_tail].next = node_id;
    fut.inner.cont_tail = node_id;
}

fn zCallPtr(t: *cy.Thread, pc: [*]cy.Inst, framePtr: [*]Value, base: u16) callconv(.c) vmc.PcFpResult {
    const res = t.callPtrInst(pc, framePtr, base) catch |err| {
        if (err == error.Panic) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_PANIC,
            };
        } else if (err == error.StackOverflow) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_STACK_OVERFLOW,
            };
        } else {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_UNKNOWN,
            };
        }
    };
    return .{
        .pc = @ptrCast(res.pc),
        .fp = @ptrCast(res.fp),
        .code = vmc.RES_SUCCESS,
    };
}

fn zCallUnion(t: *cy.Thread, pc: [*]cy.Inst, framePtr: [*]Value, base: u16) callconv(.c) vmc.PcFpResult {
    const res = t.callUnionInst(pc, framePtr, base) catch |err| {
        if (err == error.Panic) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_PANIC,
            };
        } else if (err == error.StackOverflow) {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_STACK_OVERFLOW,
            };
        } else {
            return .{
                .pc = undefined,
                .fp = undefined,
                .code = vmc.RES_UNKNOWN,
            };
        }
    };
    return .{
        .pc = @ptrCast(res.pc),
        .fp = @ptrCast(res.fp),
        .code = vmc.RES_SUCCESS,
    };
}

fn zAllocPoolObject(t: *cy.Thread, type_id: cy.TypeId) callconv(.c) vmc.HeapObjectResult {
    const obj = t.heap.allocPoolObject(type_id) catch {
        return .{
            .obj = undefined,
            .code = vmc.RES_UNKNOWN,
        };
    };
    return .{
        .obj = @ptrCast(obj),
        .code = vmc.RES_SUCCESS,
    };
}

fn zAllocBigObject(t: *cy.Thread, type_id: cy.TypeId, size: usize) callconv(.c) vmc.HeapObjectResult {
    const obj = t.heap.allocBigObject(type_id, size) catch {
        return .{
            .obj = undefined,
            .code = vmc.RES_UNKNOWN,
        };
    };
    return .{
        .obj = @ptrCast(obj),
        .code = vmc.RES_SUCCESS,
    };
}

fn zAlloc(vm: *VM, n: usize) callconv(.c) vmc.BufferResult {
    const buf = vm.alloc.alloc(u8, n) catch {
        return .{
            .buf = undefined,
            .len = undefined,
            .code = vmc.RES_UNKNOWN,
        };
    };
    return .{
        .buf = @ptrCast(buf.ptr),
        .len = buf.len,
        .code = vmc.RES_SUCCESS,
    };
}

fn zLog(fmtz: [*:0]const u8, valsPtr: [*]const fmt.FmtValue, len: usize) callconv(.c) void {
    const format = std.mem.sliceTo(fmtz, 0);
    const vals = valsPtr[0..len];
    cy.debug.log2(format, vals);
}

fn zCheckDoubleFree(t: *cy.Thread, obj: *cy.HeapObject) callconv(.c) bool {
    if (cy.Trace) {
        return t.heap.checkDoubleFree(obj);
    } else {
        return false;
    }
}

fn zCheckRetainDanglingPointer(t: *cy.Thread, obj: *cy.HeapObject) callconv(.c) bool {
    if (cy.Trace) {
        return t.heap.checkRetainDanglingPointer(obj);
    } else {
        return false;
    }
}

fn zPanicFmt(t: *cy.Thread, formatz: [*:0]const u8, argsPtr: [*]const fmt.FmtValue, numArgs: usize) callconv(.c) void {
    const format = formatz[0..std.mem.sliceTo(formatz, 0).len];
    const args = argsPtr[0..numArgs];

    const msg = fmt.allocFormat(t.alloc, format, args) catch |err| {
        if (err == error.OutOfMemory) {
            t.c.panic_type = vmc.PANIC_INFLIGHT_OOM;
            return;
        } else {
            cy.panic("unexpected");
        }
    };
    t.c.panic_payload = @as(u64, @intFromPtr(msg.ptr)) | (@as(u64, msg.len) << 48);
    t.c.panic_type = vmc.PANIC_MSG;
    logger.tracev("{s}", .{msg});
}

fn c_strlen(s: [*:0]const u8) callconv(.c) usize {
    return std.mem.sliceTo(s, 0).len;
}

fn c_pow(b: f64, e: f64) callconv(.c) f64 {
    return std.math.pow(f64, b, e);
}

comptime {
    if (cy.isWasmFreestanding and build_options.vmEngine == .c) {
        @export(c_strlen, .{ .name = "strlen", .linkage = .strong });
        @export(c_pow, .{ .name = "pow", .linkage = .strong });
    }

    if (build_options.export_vmz) {
        @export(&zAwait, .{ .name = "zAwait", .linkage = .strong });
        @export(&zPanicFmt, .{ .name = "zPanicFmt", .linkage = .strong });
        @export(&zAllocBigObject, .{ .name = "zAllocBigObject", .linkage = .strong });
        @export(&zAllocPoolObject, .{ .name = "zAllocPoolObject", .linkage = .strong });
        @export(&z_ret_generator, .{ .name = "z_ret_generator", .linkage = .strong });
        @export(&zAllocClosure, .{ .name = "zAllocClosure", .linkage = .strong });
        @export(&zAllocFuncUnion, .{ .name = "zAllocFuncUnion", .linkage = .strong });
        @export(&zAlloc, .{ .name = "zAlloc", .linkage = .strong });
        @export(&zCallPtr, .{ .name = "zCallPtr", .linkage = .strong });
        @export(&zCallUnion, .{ .name = "zCallUnion", .linkage = .strong });
        @export(&zCallTrait, .{ .name = "zCallTrait", .linkage = .strong });
        @export(&zOpCodeName, .{ .name = "zOpCodeName", .linkage = .strong });
        @export(&zLog, .{ .name = "zLog", .linkage = .strong });
        @export(&zGetTypeName, .{ .name = "zGetTypeName", .linkage = .strong });
        @export(&zFree, .{ .name = "zFree", .linkage = .strong });
        @export(&zDestroyObject, .{ .name = "zDestroyObject", .linkage = .strong });
        @export(&zFreePoolObject, .{ .name = "zFreePoolObject", .linkage = .strong });
        @export(&zFreeBigObject, .{ .name = "zFreeBigObject", .linkage = .strong });
        @export(&z_dump_thread_inst, .{ .name = "z_dump_thread_inst", .linkage = .strong });
        @export(&zCheckDoubleFree, .{ .name = "zCheckDoubleFree", .linkage = .strong });
        @export(&zCheckRetainDanglingPointer, .{ .name = "zCheckRetainDanglingPointer", .linkage = .strong });
        @export(&zFatal, .{ .name = "zFatal", .linkage = .strong });
        @export(&zEnsureListCap, .{ .name = "zEnsureListCap", .linkage = .strong });
        @export(&zDumpObjectTrace, .{ .name = "zDumpObjectTrace", .linkage = .strong });
        @export(&zPrintTraceAtPc, .{ .name = "zPrintTraceAtPc", .linkage = .strong });
        @export(&zGetExternFunc, .{ .name = "zGetExternFunc", .linkage = .strong });
    }
}

pub fn default_print(_: ?*C.Thread, _: C.Bytes) callconv(.c) void {
    // Nop.
}

pub fn default_print_err(_: ?*C.Thread, _: C.Bytes) callconv(.c) void {
    // Nop.
}

pub fn default_log(_: ?*C.VM, _: C.Bytes) callconv(.c) void {
    // Nop.
}

fn zFree(t: *cy.Thread, bytes: vmc.Str) callconv(.c) void {
    t.alloc.free(bytes.ptr[0..bytes.len]);
}

pub fn zGetTypeName(vm: *VM, id: cy.TypeId) callconv(.c) vmc.Str {
    // Only builds rt type names in trace mode.
    std.debug.assert(cy.Trace);

    vm.c.rw_lock.read_lock();
    defer vm.c.rw_lock.read_unlock();

    const info = vm.c.types_ptr[id];
    const res = info.name_ptr[0..info.name_len];
    const name = C.to_bytes(res);

    return vmc.Str{
        .ptr = name.ptr,
        .len = name.len,
    };
}

fn zEnsureListCap(vm: *VM, list: *std.ArrayList(Value), cap: usize) callconv(.c) vmc.ResultCode {
    list.ensureTotalCapacity(vm.alloc, cap) catch {
        return vmc.RES_UNKNOWN;
    };
    return vmc.RES_SUCCESS;
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
}) !std.process.Child.Term {
    var child = std.process.Child.init(args.argv, args.allocator);
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

pub fn getFuncPtrType(vm: *cy.VM, sig: cy.sema.FuncSigId) !*cy.Type {
    const chunk = vm.sema.func_ptr_tmpl.chunk();
    return cy.sema.getFuncPtrType(chunk, sig);
}

const LocalSave = struct {
    name: []const u8,
    val_t: *cy.Type,
    value: []const cy.Value,

    pub fn deinit(self: *const LocalSave, alloc: std.mem.Allocator) void {
        alloc.free(self.name);
    }
};

pub const FuncId = u32;

pub const FuncSymbolType = enum(u8) {
    func,
    host_func,
    null,
};

pub const FuncSymDetail = struct {
    func: ?*cy.Func,

    // Some functions don't map to a sema func, so the details are provided.
    name_ptr: [*]const u8,
    name_len: u32,
    sig: sema.FuncSigId,

    pub fn name(self: FuncSymDetail) []const u8 {
        return self.name_ptr[0..self.name_len];
    }
};

pub const FuncSymbol = extern struct {
    type: FuncSymbolType,

    data: extern union {
        host_func: vmc.HostFn,
        func: extern struct {
            pc: [*]cy.Inst,
        },
    },

    pub fn to_vm_ptr(self: *FuncSymbol) cy.Value {
        if (self.type == .host_func) {
            return Value.initRaw(@intFromPtr(self.data.host_func) | (@as(u64, 1) << 63));
        } else {
            return Value.initPtr(self.data.func.pc);
        }
    }

    pub fn initNull() FuncSymbol {
        return .{
            .type = .null,
            .data = undefined,
        };
    }

    pub fn initHostFunc(func: vmc.HostFn) FuncSymbol {
        return .{
            .type = .host_func,
            .data = .{
                .host_func = func,
            },
        };
    }

    pub fn initFunc(pc: [*]cy.Inst) FuncSymbol {
        return .{
            .type = .func,
            .data = .{
                .func = .{
                    .pc = pc,
                },
            },
        };
    }
};

pub const TypeInfo = extern struct {
    name_ptr: [*]const u8,
    name_len: usize,

    pub fn deinit(self: *TypeInfo, alloc: std.mem.Allocator) void {
        alloc.free(self.name_ptr[0..self.name_len]);
    }
};

pub const GlobalSym = struct {
    value: []align(8) u8,
    sym: *cy.Sym,
    extern_: bool,

    pub fn init(value: []align(8) u8, sym: *cy.Sym) GlobalSym {
        return .{
            .value = value,
            .sym = sym,
            .extern_ = sym.type == .extern_var,
        };
    }

    pub fn deinit(self: *GlobalSym, alloc: std.mem.Allocator) void {
        if (!self.extern_) {
            alloc.free(self.value);
        }
    }
};

pub fn getTypeName(vm: *cy.VM, type_h: cy.TypeId) []const u8 {
    return vm.getType(type_h).name();
}

pub fn getSymName(vm: *cy.VM, id: u32) []const u8 {
    return vm.syms.buf[id].name;
}

// pub fn print_err(vm: *cy.VM, str: []const u8) void {
//     vm.print_err.?(@ptrCast(vm), C.toStr(str));
// }

// pub fn print_err_fmt(vm: *cy.VM, format: []const u8, args: []const cy.fmt.FmtValue) void {
//     var adapter = vm.clearTempString().adaptToNewApi(&.{});
//     cy.fmt.print(&adapter.new_interface, format, args);
//     vm.print_err.?(@ptrCast(vm), C.toStr(vm.getTempString()));
// }

// pub fn print_err_zfmt(vm: *cy.VM, comptime format: []const u8, args: anytype) void {
//     const w = vm.clearTempString();
//     std.fmt.format(w, format, args) catch cy.fatal();
//     vm.print_err.?(@ptrCast(vm), C.toStr(vm.getTempString()));
// }

// pub fn print(vm: *cy.VM, str: []const u8) void {
//     vm.print.?(@ptrCast(vm), C.toStr(str));
// }

// pub fn print_fmt(vm: *cy.VM, format: []const u8, args: []const cy.fmt.FmtValue) void {
//     const w = vm.clearTempString();
//     cy.fmt.print(w, format, args);
//     vm.print.?(@ptrCast(vm), C.toStr(vm.getTempString()));
// }

// pub fn print_zfmt(vm: *cy.VM, comptime format: []const u8, args: anytype) void {
//     const w = vm.clearTempString();
//     std.fmt.format(w, format, args) catch cy.fatal();
//     vm.print.?(@ptrCast(vm), C.toStr(vm.getTempString()));
// }

pub fn writeStderr(s: []const u8) void {
    @branchHint(.cold);
    var w = cy.fmt.lockStderrWriter();
    defer cy.fmt.unlockPrint();
    w.interface.writeAll(s) catch |e| {
        logger.tracev("{}", .{e});
        cy.fatal();
    };
}
