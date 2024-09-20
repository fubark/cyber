const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const rt = cy.rt;
const fmt = @import("fmt.zig");
const v = fmt.v;
const vmc = @import("vm_c.zig");
const sema = cy.sema;
const bt = cy.types.BuiltinTypes;
const core_mod = @import("builtins/builtins.zig");
const cy_mod = @import("builtins/cy.zig");
const math_mod = @import("builtins/math.zig");
const llvm_gen = @import("llvm_gen.zig");
const cgen = @import("cgen.zig");
const bcgen = @import("bc_gen.zig");
const jitgen = @import("jit/gen.zig");
const assm = @import("jit/assembler.zig");
const A64 = @import("jit/a64.zig");
const bindings = cy.bindings;
const module = cy.module;
const ast = cy.ast;

const log = cy.log.scoped(.compiler);

const f64NegOne = cy.Value.initF64(-1);
const f64One = cy.Value.initF64(1);

const dumpCompileErrorStackTrace = !cy.isFreestanding and builtin.mode == .Debug and !cy.isWasm and true;

const Root = @This();

pub const Compiler = struct {
    alloc: std.mem.Allocator,
    vm: *cy.VM,
    buf: cy.ByteCodeBuffer,
    jitBuf: jitgen.CodeBuffer,

    reports: std.ArrayListUnmanaged(Report),
    
    /// Sema model resulting from the sema pass.
    sema: sema.Sema,

    /// Determines how modules are loaded.
    moduleLoader: C.ModuleLoaderFn,

    /// Determines how module uris are resolved.
    moduleResolver: C.ResolverFn,

    /// Compilation units for iteration.
    chunks: std.ArrayListUnmanaged(*cy.Chunk),

    /// Special chunks managed separately.
    ct_builtins_chunk: ?*cy.Chunk,

    /// Resolved URI to chunk.
    chunk_map: std.StringHashMapUnmanaged(*cy.Chunk),

    /// Context vars.
    context_vars: std.StringHashMapUnmanaged(ContextVar),

    /// Key is either a *Sym or *Func.
    genSymMap: std.AutoHashMapUnmanaged(*anyopaque, bcgen.Sym),

    /// Key to VM.vtables index.
    gen_vtables: std.AutoHashMapUnmanaged(bcgen.VtableKey, u32),

    /// Imports are queued.
    import_tasks: std.ArrayListUnmanaged(ImportTask),

    config: C.CompileConfig,

    /// Tracks whether an error was set from the API.
    hasApiError: bool,
    apiError: []const u8, // Duped so Cyber owns the msg.

    /// Whether core should be imported.
    importCore: bool = true,

    iteratorMID: vmc.MethodId = cy.NullId,
    nextMID: vmc.MethodId = cy.NullId,
    indexMID: vmc.MethodId = cy.NullId,
    setIndexMID: vmc.MethodId = cy.NullId,
    sliceMID: vmc.MethodId = cy.NullId,
    getMID: vmc.MethodId = cy.NullId,
    setMID: vmc.MethodId = cy.NullId,

    main_chunk: *cy.Chunk,

    global_sym: ?*cy.sym.UserVar,
    get_global: ?*cy.Func,

    /// Whether this is a subsequent compilation reusing the same state.
    cont: bool,

    chunk_start: u32,
    type_start: u32,

    /// TODO: Move the following into the `Worker`.
    svar_init_stack: std.ArrayListUnmanaged(StaticVarInit),

    pub fn init(self: *Compiler, vm: *cy.VM) !void {
        self.* = .{
            .alloc = vm.alloc,
            .vm = vm,
            .buf = try cy.ByteCodeBuffer.init(vm.alloc, vm),
            .jitBuf = jitgen.CodeBuffer.init(),
            .reports = .{},
            .sema = try sema.Sema.init(vm.alloc, self),
            .moduleLoader = defaultModuleLoader,
            .moduleResolver = defaultModuleResolver,
            .chunks = .{},
            .ct_builtins_chunk = null,
            .chunk_map = .{},
            .genSymMap = .{},
            .gen_vtables = .{},
            .import_tasks = .{},
            .config = C.defaultCompileConfig(), 
            .hasApiError = false,
            .apiError = "",
            .main_chunk = undefined,
            .global_sym = null,
            .get_global = null,
            .cont = false,
            .chunk_start = 0,
            .type_start = 0,
            .context_vars = .{},
            .svar_init_stack = .{},
        };
        try self.reinitPerRun();    
    }

    pub fn deinitValues(self: *Compiler) void {
        for (self.chunks.items) |chunk| {
            for (chunk.syms.items) |sym| {
                sym.deinitValues(self.vm);
            }
        }
    }

    pub fn deinit(self: *Compiler, comptime reset: bool) void {
        self.clearReports();
        if (!reset) {
            self.reports.deinit(self.alloc);
        }

        if (reset) {
            self.buf.clear();
            self.jitBuf.clear();
        } else {
            self.buf.deinit();
            self.jitBuf.deinit(self.alloc);
        }

        // Retained vars are deinited first since they can depend on types/syms.
        self.deinitValues();

        // Free any remaining import tasks.
        for (self.import_tasks.items) |task| {
            task.deinit(self.alloc);
        }

        for (self.chunks.items) |chunk| {
            log.tracev("Deinit chunk `{s}`", .{chunk.srcUri});
            if (chunk.onDestroy) |onDestroy| {
                onDestroy(@ptrCast(self.vm), cy.Sym.toC(@ptrCast(chunk.sym)));
            }
            chunk.deinit();
            self.alloc.destroy(chunk);
        }
        if (self.ct_builtins_chunk) |chunk| {
            chunk.deinit();
            self.alloc.destroy(chunk);
        }
        for (self.svar_init_stack.items) |*svar_init| {
            svar_init.deps.deinit(self.alloc);
        }
        if (reset) {
            self.chunks.clearRetainingCapacity();
            self.chunk_map.clearRetainingCapacity();
            self.genSymMap.clearRetainingCapacity();
            self.gen_vtables.clearRetainingCapacity();
            self.import_tasks.clearRetainingCapacity();
            self.context_vars.clearRetainingCapacity();
            self.svar_init_stack.clearRetainingCapacity();
        } else {
            self.chunks.deinit(self.alloc);
            self.chunk_map.deinit(self.alloc);
            self.genSymMap.deinit(self.alloc);
            self.gen_vtables.deinit(self.alloc);
            self.import_tasks.deinit(self.alloc);
            self.context_vars.deinit(self.alloc);
            self.svar_init_stack.deinit(self.alloc);
        }

        // Chunks depends on modules.
        self.sema.deinit(self.alloc, reset);

        self.alloc.free(self.apiError);
        self.apiError = "";
    }

    pub fn reinitPerRun(self: *Compiler) !void {
        self.clearReports();
    }

    pub fn clearReports(self: *Compiler) void {
        for (self.reports.items) |report| {
            report.deinit(self.alloc);
        }
        self.reports.clearRetainingCapacity();
    }

    pub fn newTypes(self: *Compiler) []cy.types.Type {
        return self.sema.types.items[self.type_start..];
    }

    pub fn newChunks(self: *Compiler) []*cy.Chunk {
        return self.chunks.items[self.chunk_start..];
    }

    pub fn compile(self: *Compiler, uri: []const u8, src: ?[]const u8, config: C.CompileConfig) !CompileResult {
        self.chunk_start = @intCast(self.chunks.items.len);
        self.type_start = @intCast(self.sema.types.items.len);
        const res = self.compileInner(uri, src, config) catch |err| {
            if (dumpCompileErrorStackTrace and !C.silent()) {
                std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            }
            if (err == error.CompileError) {
                return err;
            }
            if (self.chunks.items.len > 0) {
                // Report other errors using the main chunk.
                return self.main_chunk.reportErrorFmt("Error: {}", &.{v(err)}, null);
            } else {
                try self.addReportFmt(.compile_err, "Error: {}", &.{v(err)}, null, null);
                return error.CompileError;
            }
        };

        // Update VM types view.
        self.vm.c.types = self.sema.types.items.ptr;
        self.vm.c.types_len = self.sema.types.items.len;

        // Successful.
        self.cont = true;
        return res;
    }

    /// Wrap compile so all errors can be handled in one place.
    fn compileInner(self: *Compiler, uri: []const u8, src_opt: ?[]const u8, config: C.CompileConfig) !CompileResult {
        self.config = config;

        if (!self.cont) {
            try reserveCoreTypes(self);
            try loadCtBuiltins(self);
            try self.context_vars.put(self.alloc, "mem", .{
                .type = bt.Memory,
                .idx = 0,
            });
            try self.context_vars.put(self.alloc, "test_int", .{
                .type = bt.Integer,
                .idx = 1,
            });
        }

        // TODO: Types and symbols should be loaded recursively for single-threaded.
        //       Separate into two passes one for types and function signatures, and
        //       another for function bodies.

        // Load core module first since the members are imported into each user module.
        var core_sym: *cy.sym.Chunk = undefined;
        if (self.importCore) {
            if (!self.cont) {
                const importCore = ImportTask{
                    .type = .nop,
                    .from = null,
                    .node = null,
                    .resolved_spec = try self.alloc.dupe(u8, "core"),
                    .data = undefined,
                };
                try self.import_tasks.append(self.alloc, importCore);
                const core_chunk = performImportTask(self, importCore) catch |err| {
                    return err;
                };
                core_sym = core_chunk.sym;
                importCore.deinit(self.alloc);
                _ = self.import_tasks.orderedRemove(0);
                try createDynMethodIds(self);
            } else {
                core_sym = self.chunk_map.get("core").?.sym;
            }
        }

        const prev_main = self.main_chunk;
        var mainChunk: *cy.Chunk = undefined;
        if (src_opt) |src_temp| {
            mainChunk = try createModule(self, uri, src_temp);
            try self.chunk_map.put(self.alloc, mainChunk.srcUri, mainChunk);
        } else {
            var buf: [4096]u8 = undefined;
            const r_uri = try resolveModuleUri(self, &buf, uri);
            mainChunk = (try loadModule(self, r_uri)) orelse {
                try self.addReportFmt(.compile_err, "Failed to load module: {}", &.{v(r_uri)}, null, null);
                return error.CompileError;
            };
        }
        self.main_chunk = mainChunk;

        if (self.cont) {
            // Use all *resolved* top level syms from previous main.
            // If a symbol wasn't resolved, it either failed during compilation or didn't end up getting used.
            var iter = prev_main.sym.getMod().symMap.iterator();
            while (iter.next()) |e| {
                const name = e.key_ptr.*;
                const sym = e.value_ptr.*;

                const resolved = switch (sym.type) {
                    .object_t => sym.cast(.object_t).isResolved(),
                    .struct_t => sym.cast(.struct_t).isResolved(),
                    .func => sym.cast(.func).isResolved(),
                    else => true,
                };
                if (!resolved) {
                    continue;
                }

                const alias = try mainChunk.reserveUseAlias(@ptrCast(mainChunk.sym), name, null);
                if (sym.type == .use_alias) {
                    alias.sym = sym.cast(.use_alias).sym;
                } else {
                    alias.sym = sym;
                }
                alias.resolved = true;
            }

            if (prev_main.use_global) {
                mainChunk.use_global = true;
            }
        }

        // All symbols are reserved by loading all modules and looking at the declarations.
        try reserveSyms(self, core_sym);

        // Resolve symbols:
        // - Variable types are resolved.
        // - Function signatures are resolved.
        // - Type fields are resolved.
        try resolveSyms(self);

        // Pass through type syms.
        for (self.newTypes()) |*type_e| {
            if (type_e.sym.getMod()) |mod| {
                if (mod.getSym("$get") != null) {
                    type_e.has_get_method = true;
                }
                if (mod.getSym("$set") != null) {
                    type_e.has_set_method = true;
                }
                if (mod.getSym("$initPair") != null) {
                    type_e.has_init_pair_method = true;
                }
            }
            switch (type_e.sym.type) {
                .object_t => {
                    const object_t = type_e.sym.cast(.object_t);
                    const impls = object_t.impls();
                    if (impls.len > 0) {
                        const decl = object_t.decl.?.cast(.objectDecl);
                        const mod = object_t.getMod();

                        for (impls) |*impl| {
                            const trait_members = impl.trait.members();
                            const funcs = try self.alloc.alloc(*cy.Func, trait_members.len);
                            errdefer self.alloc.free(funcs);

                            for (trait_members, 0..) |member, i| {
                                const func = mod.getTraitMethodImpl(self, member) orelse {
                                    const sig_str = try self.sema.allocFuncSigStr(member.func.funcSigId, true, mod.chunk);
                                    defer self.alloc.free(sig_str);
                                    return mod.chunk.reportErrorFmt("`{}` does not implement `func {}{}` from `{}`.", &.{v(object_t.head.name()), v(member.func.name()), v(sig_str), v(impl.trait.head.name()) }, @ptrCast(decl.impl_withs[i].trait));
                                };
                                funcs[i] = func;
                            }

                            impl.funcs = funcs;
                        }
                    }
                },
                else => {},
            }
        }

        // Compute type sizes after type fields have been resolved.
        // try computeTypeSizesRec(self);

        // Perform sema on static initializers.
        log.tracev("Perform init sema.", .{});
        for (self.newChunks()) |chunk| {
            // First stmt is root at index 0.
            if (chunk.hasStaticInit) {
                try performChunkInitSema(self, chunk);
            }
        }

        // Perform sema on all chunks.
        log.tracev("Perform sema.", .{});
        for (self.newChunks()) |chunk| {
            performChunkSema(self, chunk) catch |err| {
                if (err == error.CompileError) {
                    return err;
                } else {
                    // Wrap all other errors as a CompileError.
                    return chunk.reportErrorFmt("error.{}", &.{v(err)}, chunk.curNode);
                }
                return err;
            };
        }

        // Perform deferred sema.
        for (self.newChunks()) |chunk| {
            var i: usize = 0; // Explicit iterator in case more variants are added during body sema.
            while (i < chunk.deferred_funcs.items.len) : (i += 1) {
                const func = chunk.deferred_funcs.items[i];
                if (func.type != .userFunc) {
                    continue;
                }
                if (func.isMethod()) {
                    try sema.methodDecl(chunk, func);
                } else {
                    try sema.funcDecl(chunk, func);
                }
            }
        }

        if (!config.skip_codegen) {
            log.tracev("Perform codegen.", .{});

            switch (self.config.backend) {
                C.BackendJIT => {
                    if (cy.isWasm) return error.Unsupported;
                    {
                        return error.Unsupported;
                    }
                    try jitgen.gen(self);
                    return .{ .jit = .{
                        .mainStackSize = self.buf.mainStackSize,
                        .buf = self.jitBuf,
                    }};
                },
                C.BackendTCC, C.BackendCC => {
                    if (cy.isWasm or !cy.hasCLI) return error.Unsupported;
                    const res = try cgen.gen(self);
                    return .{ .aot = res };
                },
                C.BackendLLVM => {
                    // try llvm_gen.genNativeBinary(self);
                    return error.TODO;
                },
                C.BackendVM => {
                    try bcgen.genAll(self);
                    return .{
                        .vm = &self.buf,
                    };
                },
                else => return error.Unsupported,
            }
            log.tracev("Done. Perform codegen.", .{});
        }

        return CompileResult{ .vm = undefined };
    }

    pub fn addReportFmt(self: *Compiler, report_t: ReportType, format: []const u8, args: []const fmt.FmtValue, chunk: ?cy.ChunkId, loc: ?u32) !void {
        const msg = try fmt.allocFormat(self.alloc, format, args);
        try self.addReportConsume(report_t, msg, chunk, loc);
    }

    /// Assumes `msg` is heap allocated.
    pub fn addReportConsume(self: *Compiler, report_t: ReportType, msg: []const u8, chunk: ?cy.ChunkId, loc: ?u32) !void {
        try self.reports.append(self.alloc, .{
            .type = report_t,
            .chunk = chunk orelse cy.NullId,
            .loc = loc orelse cy.NullId,
            .msg = msg,
        });
    }

    pub fn addReport(self: *Compiler, report_t: ReportType, msg: []const u8, chunk: ?cy.ChunkId, loc: ?u32) !void {
        const dupe = try self.alloc.dupe(u8, msg);
        try self.addReportConsume(report_t, dupe, chunk, loc);
    }
};

const ReportType = enum(u8) {
    token_err,
    parse_err,
    compile_err,
};

pub const Report = struct {
    type: ReportType,
    msg: []const u8,

    /// If NullId, then the report comes from an aggregate step.
    chunk: cy.ChunkId,

    /// srcPos if token_err or parse_err
    //  nodeId if compile_err
    /// If NullId, then the report does not have a location.
    loc: u32,

    pub fn deinit(self: Report, alloc: std.mem.Allocator) void {
        alloc.free(self.msg);
    }
};

pub const AotCompileResult = struct {
    exePath: [:0]const u8,

    pub fn deinit(self: AotCompileResult, alloc: std.mem.Allocator) void {
        alloc.free(self.exePath);
    }
};

/// Tokenize and parse.
/// Parser pass collects static declaration info.
fn performChunkParse(self: *Compiler, chunk: *cy.Chunk) !void {
    _ = self;
    var tt = cy.debug.timer();

    const S = struct {
        fn parserReport(ctx: *anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror {
            const c: *cy.Chunk = @ptrCast(@alignCast(ctx));
            try c.compiler.addReportFmt(.parse_err, format, args, c.id, pos);
            return error.ParseError;
        }

        fn tokenizerReport(ctx: *anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror!void {
            const c: *cy.Chunk = @ptrCast(@alignCast(ctx));
            try c.compiler.addReportFmt(.token_err, format, args, c.id, pos);
            return error.TokenError;
        }
    };

    chunk.parser.reportFn = S.parserReport;
    chunk.parser.tokenizerReportFn = S.tokenizerReport;
    chunk.parser.ctx = chunk;

    const res = try chunk.parser.parse(chunk.src, .{});
    tt.endPrint("parse");
    // Update buffer pointers so success/error paths can access them.
    chunk.updateAstView(res.ast);
    if (res.has_error) {
        return error.CompileError;
    }
}

/// Sema pass.
/// Symbol resolving, type checking, and builds the model for codegen.
fn performChunkSema(self: *Compiler, chunk: *cy.Chunk) !void {
    if (chunk == self.main_chunk) {
        try sema.pushResolveContext(chunk, @ptrCast(chunk.ast.root));
        defer sema.popResolveContext(chunk);

        _ = try sema.semaMainBlock(self, chunk);
    }
    // Top level declarations only.
    try performChunkSemaDecls(chunk);
}

fn performChunkSemaDecls(c: *cy.Chunk) !void {
    // Iterate funcs with explicit index since lambdas could be appended.
    var i: u32 = 0;
    const num_funcs: u32 = @intCast(c.funcs.items.len);
    while (i < num_funcs) : (i += 1) {
        const func = c.funcs.items[i];
        switch (func.type) {
            .userFunc => {
                // Skip already emitted functions such as `$init`.
                if (func.emitted) {
                    continue;
                }
                log.tracev("sema func: {s}", .{func.name()});
                if (func.isMethod()) {
                    try sema.methodDecl(c, func);
                } else {
                    try sema.funcDecl(c, func);
                }
            },
            else => {},
        }
    }
}

/// Sema on static initializers.
fn performChunkInitSema(self: *Compiler, c: *cy.Chunk) !void {
    log.tracev("Perform init sema. {} {s}", .{c.id, c.srcUri});

    const funcSigId = try c.sema.ensureFuncSig(&.{}, bt.Void);

    const name = try c.parser.ast.genSpanNode(self.alloc, .ident, "$init");
    const decl = try c.parser.ast.newNode(.funcDecl, .{
        .name = @ptrCast(name),
        .attrs = &.{},
        .params = &.{},
        .ret = null,
        .stmts = &.{},
        .pos = cy.NullId,
        .sig_t = .func,
        .hidden = true,
    });
    c.updateAstView(c.parser.ast.view());

    const func = try c.reserveUserFunc(@ptrCast(c.sym), "$init", decl, false, false);
    try c.resolveUserFunc(func, funcSigId);

    _ = try sema.pushFuncProc(c, func);

    // Emit in correct dependency order.
    for (c.syms.items) |sym| {
        switch (sym.type) {
            .userVar => {
                const user_var = sym.cast(.userVar);
                try sema.semaUserVarInitDeep(c, user_var);
            },
            else => {},
        }
    }

    try sema.popFuncBlock(c);
    func.emitted = true;
}

fn completeImportTask(self: *Compiler, task: ImportTask, res: *cy.Chunk) !void {
    switch (task.type) {
        .nop => {},
        .module_alias => {
            task.data.module_alias.sym.sym = @ptrCast(res.sym);
        },
        .use_alias => {
            const c = task.from.?;

            const node = task.node.?;
            if (node.name.type() == .all) {
                try c.use_alls.append(self.alloc, @ptrCast(res.sym));
            } else {
                task.data.use_alias.sym.sym = @ptrCast(res.sym);
                task.data.use_alias.sym.resolved = true;
            }
        },
    }
}

fn loadModule(self: *Compiler, r_uri: []const u8) !?*cy.Chunk {
    // Check cache first.
    const cache = try self.chunk_map.getOrPut(self.alloc, r_uri);
    if (cache.found_existing) {
        return cache.value_ptr.*;
    }

    // Initialize defaults.
    var res: C.Module = undefined;

    self.hasApiError = false;
    log.tracev("Invoke module loader: {s}", .{r_uri});

    if (!self.moduleLoader.?(@ptrCast(self.vm), C.toStr(r_uri), &res)) {
        return null;
    }
    const chunk: *cy.Chunk = @ptrCast(@alignCast(res.ptr));
    cache.key_ptr.* = chunk.srcUri;
    cache.value_ptr.* = chunk;
    return chunk;
}

pub fn createModule(self: *Compiler, r_uri: []const u8, src: []const u8) !*cy.Chunk {
    const src_dupe = try self.alloc.dupe(u8, src);
    const r_uri_dupe = try self.alloc.dupe(u8, r_uri);

    const chunk_id: u32 = @intCast(self.chunks.items.len);
    const chunk = try self.alloc.create(cy.Chunk);
    try chunk.init(self, chunk_id, r_uri_dupe, src_dupe);
    chunk.sym = try chunk.createChunkSym(r_uri_dupe);
    try self.chunks.append(self.alloc, chunk);
    return chunk;
}

fn performImportTask(self: *Compiler, task: ImportTask) !*cy.Chunk {
    const chunk = (try loadModule(self, task.resolved_spec)) orelse {
        if (task.from) |from| {
            if (task.node) |node| {
                if (self.hasApiError) {
                    return from.reportErrorFmt(self.apiError, &.{}, node.spec);
                } else {
                    return from.reportErrorFmt("Failed to load module: {}", &.{v(task.resolved_spec)}, node.spec);
                }
            } else {
                if (self.hasApiError) {
                    return from.reportError(self.apiError, null);
                } else {
                    return from.reportErrorFmt("Failed to load module: {}", &.{v(task.resolved_spec)}, null);
                }
            }
        } else {
            try self.addReportFmt(.compile_err, "Failed to load module: {}", &.{v(task.resolved_spec)}, null, null);
            return error.CompileError;
        }
    };
    try completeImportTask(self, task, chunk);
    return chunk;
}

fn reserveSyms(self: *Compiler, core_sym: *cy.sym.Chunk) !void{
    log.tracev("Reserve symbols.", .{});

    var id: u32 = self.chunk_start;
    while (true) {
        while (id < self.chunks.items.len) : (id += 1) {
            const chunk = self.chunks.items[id];
            log.tracev("chunk parse: {}", .{chunk.id});
            try performChunkParse(self, chunk);

            if (self.importCore) {
                // Import all from core module into local namespace.
                try chunk.use_alls.append(self.alloc, @ptrCast(core_sym));
            }

            // Process static declarations.
            for (chunk.parser.staticDecls.items) |node| {
                log.tracev("reserve: {s}", .{try chunk.ast.declNamePath(node)});
                switch (node.type()) {
                    .import_stmt => {
                        try sema.declareUseImport(chunk, node.cast(.import_stmt));
                    },
                    .use_alias => {
                        _ = try sema.reserveUseAlias(chunk, node.cast(.use_alias));
                    },
                    .cstruct_decl => {
                        const decl = node.cast(.cstruct_decl);
                        const sym = try sema.reserveStruct(chunk, decl, true);
                        for (decl.funcs) |func| {
                            _ = try sema.reserveNestedFunc(chunk, @ptrCast(sym), func, false);
                        }
                    },
                    .struct_decl => {
                        const decl = node.cast(.struct_decl);
                        const sym = try sema.reserveStruct(chunk, decl, false);
                        for (decl.funcs) |func| {
                            _ = try sema.reserveNestedFunc(chunk, @ptrCast(sym), func, false);
                        }
                    },
                    .trait_decl => {
                        const decl = node.cast(.trait_decl);
                        const sym = try sema.reserveTraitType(chunk, decl);

                        const members = try self.alloc.alloc(cy.sym.TraitMember, decl.funcs.len);
                        errdefer self.alloc.free(members);
                        for (decl.funcs, 0..) |func_decl, i| {
                            const func = try sema.reserveImplicitTraitMethod(chunk, @ptrCast(sym), func_decl, i, false);
                            members[i] = .{
                                .func = func,
                            };
                        }

                        sym.members_ptr = members.ptr;
                        sym.members_len = @intCast(members.len);
                    },
                    .enumDecl => {
                        _ = try sema.reserveEnum(chunk, node.cast(.enumDecl));
                    },
                    .typeAliasDecl => {
                        _ = try sema.reserveTypeAlias(chunk, node.cast(.typeAliasDecl));
                    },
                    .custom_decl => {
                        const decl = node.cast(.custom_decl);
                        const sym = try sema.resolveCustomType(chunk, decl);
                        _ = try cy.module.addUniqueSym(chunk, sym.parent.?.getMod().?, sym.name(), sym, @ptrCast(decl));

                        for (decl.funcs) |func| {
                            _ = try sema.reserveNestedFunc(chunk, @ptrCast(sym), func, false);
                        }
                    },
                    .distinct_decl => {
                        const decl = node.cast(.distinct_decl);
                        const sym = try sema.reserveDistinctType(chunk, decl);

                        for (decl.funcs) |func| {
                            _ = try sema.reserveNestedFunc(chunk, @ptrCast(sym), func, false);
                        }
                    },
                    .template => {
                        const decl = node.cast(.template);
                        if (decl.child_decl.type() == .funcDecl and decl.child_decl.cast(.funcDecl).sig_t == .template) {
                            _ = try sema.reserveFuncTemplate(chunk, decl);
                        } else {
                            _ = try sema.reserveTemplate(chunk, decl);
                        }
                    },
                    .staticDecl => {
                        const sym = try sema.reserveVar(chunk, node.cast(.staticDecl));
                        if (sym.type == .userVar) {
                            chunk.hasStaticInit = true;
                        }
                    },
                    .context_decl => {
                        _ = try sema.reserveContextVar(chunk, node.cast(.context_decl));
                    },
                    .funcDecl => {
                        const decl = node.cast(.funcDecl);
                        if (decl.stmts.len == 0) {
                            _ = try sema.reserveHostFunc(chunk, decl);
                        } else {
                            _ = try sema.reserveUserFunc(chunk, decl);
                        }
                    },
                    else => return error.Unsupported,
                }
            }

            if (chunk.onTypeLoad) |onTypeLoad| {
                onTypeLoad(@ptrCast(self.vm), chunk.sym.sym().toC());
            }

            if (id == 0) {
                // Extract special syms. Assumes chunks[0] is the builtins chunk.
                const core = self.chunks.items[0].sym.getMod();
                self.sema.future_tmpl = core.getSym("Future").?.cast(.template);
                self.sema.option_tmpl = core.getSym("Option").?.cast(.template);
                self.sema.array_tmpl = core.getSym("Array").?.cast(.template);
                self.sema.pointer_tmpl = core.getSym("pointer").?.cast(.template);
                self.sema.ref_tmpl = core.getSym("ref").?.cast(.template);
                self.sema.list_tmpl = core.getSym("List").?.cast(.template);
                self.sema.table_type = core.getSym("Table").?.cast(.object_t);
                self.sema.ptr_slice_tmpl = core.getSym("PtrSlice").?.cast(.template);
                self.sema.func_sym_tmpl = core.getSym("funcsym_t").?.cast(.template);
                self.sema.ref_slice_tmpl = core.getSym("Slice").?.cast(.template);
                self.sema.func_ptr_tmpl = core.getSym("funcptr_t").?.cast(.template);
                self.sema.func_union_tmpl = core.getSym("funcunion_t").?.cast(.template);
            }
            if (chunk != self.main_chunk) {
                // Check for illegal top level statements.
                if (chunk.parser.staticDecls.items.len != chunk.ast.root.?.stmts.len) {
                    for (chunk.ast.root.?.stmts) |stmt| {
                        switch (stmt.type()) {
                            .context_decl,
                            .custom_decl,
                            .distinct_decl,
                            .enumDecl,
                            .funcDecl,
                            .import_stmt,
                            .staticDecl,
                            .struct_decl,
                            .typeAliasDecl,
                            .template => {},
                            else => {
                                return chunk.reportError("Top level statement is not allowed from imported module.", @ptrCast(stmt));
                            }
                        }
                    }
                }
            }
        }

        // Check for import tasks.
        for (self.import_tasks.items) |task| {
            _ = performImportTask(self, task) catch |err| {
                return err;
            };
        }
        for (self.import_tasks.items) |task| {
            task.deinit(self.alloc);
        }
        self.import_tasks.clearRetainingCapacity();

        if (id == self.chunks.items.len) {
            // No more chunks were added from import tasks.
            break;
        }
    }
}

fn loadCtBuiltins(self: *Compiler) !void {
    const bc = try self.alloc.create(cy.Chunk);
    const ct_builtins_uri = try self.alloc.dupe(u8, "ct");
    try bc.init(self, cy.NullId, ct_builtins_uri, "");
    bc.sym = try bc.createChunkSym(ct_builtins_uri);
    self.ct_builtins_chunk = bc;

    _ = try bc.declareBoolType(@ptrCast(bc.sym), "bool_t", null, null);
    _ = try bc.declareIntType(@ptrCast(bc.sym), "int64_t", 64, null, null);
    _ = try bc.declareIntType(@ptrCast(bc.sym), "int8_t", 8, null, null);
    _ = try bc.declareFloatType(@ptrCast(bc.sym), "float64_t", 64, null, null);
}

fn reserveCoreTypes(self: *Compiler) !void {
    log.tracev("Reserve core types", .{});

    const type_ids = &[_]cy.TypeId{
        // Incomplete type.
        bt.Void,

        // Primitives.
        bt.Boolean,
        bt.Error,
        bt.Byte,
        bt.TagLit,
        bt.Symbol,
        bt.Integer,
        bt.Float,

        // Unions.
        bt.Dyn,
        bt.Any,

        // VM specific types.
        bt.Type,

        // Object types.
        bt.Tuple,
        bt.Placeholder4,
        bt.Placeholder5,
        bt.Map,
        bt.MapIter,
        bt.Func,
        bt.FuncSig,
        bt.Placeholder2,
        bt.ExternFunc,
        bt.String,
        bt.ExprType,
        bt.Fiber,
        bt.UpValue,
        bt.TccState,
        bt.Placeholder3,
        bt.Range,
        bt.Table,
        bt.Memory,
    };

    for (type_ids) |type_id| {
        const id = try self.sema.pushType();
        std.debug.assert(id == type_id);
    }

    std.debug.assert(self.sema.types.items.len == cy.types.BuiltinEnd);

    self.sema.types.items[bt.UpValue].kind = .bare;
    self.sema.types.items[bt.UpValue].sym = @ptrCast(&UpValueType);
    self.sema.types.items[bt.UpValue].info = .{};
}

pub var UpValueType = cy.sym.DummyType{
    .head = cy.Sym.init(.dummy_t, null, "UpValue"),
    .type = bt.UpValue,
};

fn createDynMethodIds(self: *Compiler) !void {
    self.indexMID = try self.vm.ensureMethod("$index");
    self.setIndexMID = try self.vm.ensureMethod("$setIndex");
    self.sliceMID = try self.vm.ensureMethod("$slice");
    self.iteratorMID = try self.vm.ensureMethod("iterator");
    self.nextMID = try self.vm.ensureMethod("next");
    self.getMID = try self.vm.ensureMethod("$get");
    self.setMID = try self.vm.ensureMethod("$set");
}

// fn computeTypeSizesRec(self: *VMcompiler) !void {
//     log.tracev("Compute type sizes.", .{});
//     for (self.chunks.items) |chunk| {
//         // Process static declarations.
//         for (chunk.parser.staticDecls.items) |decl| {
//             switch (decl.declT) {
//                 // .struct_t => {
//                 // },
//                 .object => {
//                     const object_t = decl.data.sym.cast(.object_t);
//                     if (object_t.rt_size == cy.NullId) {
//                         try computeTypeSize(object_t);
//                     }
//                 },
//                 // .enum_t => {
//                 // },
//                 else => {},
//             }
//         }

//         if (chunk.onLoad) |onLoad| {
//             onLoad(@ptrCast(self.vm), cc.ApiModule{ .sym = @ptrCast(chunk.sym) });
//         }
//     }
// }

// fn computeTypeSize(object_t: *cy.sym.ObjectType) !void {
//     var size: u32 = 0;
//     for (object_t.getFields()) |field| {
//         if (field.type == )
//         _ = field;
//     }
//     object_t.rt_size = size;
// }

fn resolveSyms(self: *Compiler) !void {
    log.tracev("Resolve syms.", .{});
    defer log.tracev("Resolve syms. Done.", .{});
    for (self.newChunks()) |chunk| {
        // Iterate with explicit index since unnamed types could be appended.
        var i: u32 = 0;
        while (i < chunk.syms.items.len) : (i += 1) {
            const sym = chunk.syms.items[i];
            log.tracev("resolve: {s}", .{sym.name()});
            switch (sym.type) {
                .context_var => {
                    try sema.resolveContextVar(chunk, @ptrCast(sym));
                },
                .userVar => {
                    // Delay resolving user vars since they are typically only
                    // referenced when resolving runtime code.
                },
                .hostVar => {
                    try sema.resolveHostVar(chunk, @ptrCast(sym));
                },
                .func => {},
                .struct_t => {
                    const struct_t = sym.cast(.struct_t);
                    try sema.resolveObjectLikeType(chunk, sym, @ptrCast(struct_t.decl.?));
                },
                .object_t => {
                    const object_t = sym.cast(.object_t);
                    const decl = object_t.decl.?;
                    try sema.resolveObjectLikeType(chunk, sym, decl.cast(.objectDecl));
                },
                .enum_t => {
                    const enum_t = sym.cast(.enum_t);
                    try sema.resolveEnumType(chunk, @ptrCast(sym), enum_t.decl);
                },
                .distinct_t => {
                    _ = try sema.resolveDistinctType(chunk, @ptrCast(sym));
                },
                .use_alias => {
                    const use_alias = sym.cast(.use_alias);
                    if (!use_alias.resolved) {
                        if (use_alias.decl.?.type() == .use_alias) {
                            try sema.resolveUseAlias(chunk, @ptrCast(sym));
                        }
                    }
                },
                .typeAlias => {
                    try sema.resolveTypeAlias(chunk, @ptrCast(sym));
                },
                .template => {
                    try sema.ensureResolvedTemplate(chunk, @ptrCast(sym));
                },
                .func_template => {
                    try sema.ensureResolvedFuncTemplate(chunk, @ptrCast(sym));
                },
                .trait_t => {},
                else => {},
            }
        }

        for (chunk.funcs.items) |func| {
            log.tracev("resolve: {s}", .{func.name()});
            try sema.resolveFunc(chunk, func);
        }

        if (chunk.onLoad) |onLoad| {
            onLoad(@ptrCast(self.vm), chunk.sym.sym().toC());
        }
    }
}

pub const CompileResult = union {
    vm: *cy.ByteCodeBuffer,
    jit: struct {
        mainStackSize: u32,
        buf: jitgen.CodeBuffer,
    },
    aot: AotCompileResult,
};

const VarInfo = struct {
    hasStaticType: bool,
};

const Load = struct {
    pc: u32,
    tempOffset: u8,
};

// Same as std.mem.replace except writes to an ArrayList. Final result is also known to be at most the size of the original.
pub fn replaceIntoShorterList(comptime T: type, input: []const T, needle: []const T, replacement: []const T, output: *std.ArrayListUnmanaged(T), alloc: std.mem.Allocator) !usize {
    // Known upper bound.
    try output.resize(alloc, input.len);
    var i: usize = 0;
    var slide: usize = 0;
    var replacements: usize = 0;
    while (slide < input.len) {
        if (std.mem.indexOf(T, input[slide..], needle) == @as(usize, 0)) {
            std.mem.copy(u8, output.items[i..i+replacement.len], replacement);
            i += replacement.len;
            slide += needle.len;
            replacements += 1;
        } else {
            output.items[i] = input[slide];
            i += 1;
            slide += 1;
        }
    }
    output.items.len = i;
    return replacements;
}

const unexpected = cy.fatal;

const ImportTaskType = enum(u8) {
    nop,
    use_alias,
    module_alias,
};

pub const ImportTask = struct {
    type: ImportTaskType,
    from: ?*cy.Chunk,
    node: ?*ast.ImportStmt,
    resolved_spec: []const u8,
    data: union {
        module_alias: struct {
            sym: *cy.sym.ModuleAlias,
        },
        use_alias: struct {
            sym: *cy.sym.UseAlias,
        },
    },

    fn deinit(self: ImportTask, alloc: std.mem.Allocator) void {
        alloc.free(self.resolved_spec);
    }
};

pub fn initModuleCompat(comptime name: []const u8, comptime initFn: fn (vm: *Compiler, modId: cy.ModuleId) anyerror!void) cy.ModuleLoaderFunc {
    return struct {
        fn initCompat(vm: *cy.UserVM, modId: cy.ModuleId) bool {
            initFn(vm.internal().compiler, modId) catch |err| {
                log.tracev("Init module `{s}` failed: {}", .{name, err});
                return false;
            };
            return true;
        }
    }.initCompat;
}

pub fn defaultModuleResolver(_: ?*C.VM, params: C.ResolverParams) callconv(.C) bool {
    params.resUri.* = params.uri.ptr;
    params.resUriLen.* = params.uri.len;
    return true;
}

pub fn defaultModuleLoader(vm_: ?*C.VM, spec: C.Str, res: ?*C.Module) callconv(.C) bool {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const name = C.fromStr(spec);
    if (std.mem.eql(u8, name, "core")) {
        res.?.* = core_mod.create(vm, name);
        return true;
    } else if (std.mem.eql(u8, name, "math")) {
        res.?.* = math_mod.create(vm, name);
        return true;
    } else if (std.mem.eql(u8, name, "cy")) {
        res.?.* = cy_mod.create(vm, name);
        return true;
    }
    return false;
}

pub fn resolveModuleUriFrom(self: *cy.Chunk, buf: []u8, uri: []const u8, node: ?*ast.Node) ![]const u8 {
    self.compiler.hasApiError = false;

    var r_uri: [*]const u8 = undefined;
    var r_uri_len: usize = undefined;
    const params: C.ResolverParams = .{
        .chunkId = self.id,
        .curUri = C.toStr(self.srcUri),
        .uri = C.toStr(uri),
        .buf = buf.ptr,
        .bufLen = buf.len,
        .resUri = @ptrCast(&r_uri),
        .resUriLen = &r_uri_len,
    };
    if (!self.compiler.moduleResolver.?(@ptrCast(self.compiler.vm), params)) {
        if (self.compiler.hasApiError) {
            return self.reportErrorFmt(self.compiler.apiError, &.{}, node);
        } else {
            return self.reportErrorFmt("Failed to resolve module.", &.{}, node);
        }
    }
    return r_uri[0..r_uri_len];
}

pub fn resolveModuleUri(self: *cy.Compiler, buf: []u8, uri: []const u8) ![]const u8 {
    self.hasApiError = false;

    var r_uri: [*]const u8 = undefined;
    var r_uri_len: usize = undefined;
    const params: C.ResolverParams = .{
        .chunkId = cy.NullId,
        .curUri = C.NullStr,
        .uri = C.toStr(uri),
        .buf = buf.ptr,
        .bufLen = buf.len,
        .resUri = @ptrCast(&r_uri),
        .resUriLen = &r_uri_len,
    };
    if (!self.moduleResolver.?(@ptrCast(self.vm), params)) {
        if (self.hasApiError) {
            try self.addReport(.compile_err, self.apiError, null, null);
            return error.CompileError;
        } else {
            try self.addReport(.compile_err, "Failed to resolve module.", null, null);
            return error.CompileError;
        }
    }
    return r_uri[0..r_uri_len];
}

const ContextVar = struct {
    type: cy.TypeId,
    idx: u8,
};

test "vm compiler internals." {
}

const StaticVarInit = struct {
    deps: std.ArrayListUnmanaged(*cy.sym.UserVar) = .{},
};
