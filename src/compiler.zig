const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const cc = @import("capi.zig");
const rt = cy.rt;
const fmt = @import("fmt.zig");
const v = fmt.v;
const vmc = @import("vm_c.zig");
const sema = cy.sema;
const bt = cy.types.BuiltinTypes;
const cy_mod = @import("builtins/builtins.zig");
const math_mod = @import("builtins/math.zig");
const llvm_gen = @import("llvm_gen.zig");
const cgen = @import("cgen.zig");
const bcgen = @import("bc_gen.zig");
const jitgen = @import("jit/gen.zig");
const assm = @import("jit/assembler.zig");
const A64 = @import("jit/a64.zig");
const bindings = cy.bindings;
const module = cy.module;

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
    moduleLoader: cc.ModuleLoaderFn,

    /// Determines how module uris are resolved.
    moduleResolver: cc.ResolverFn,

    /// Compilation units for iteration.
    chunks: std.ArrayListUnmanaged(*cy.Chunk),

    /// Chunk srcUri to chunk.
    chunkMap: std.StringHashMapUnmanaged(*cy.Chunk),

    /// Key is either a *Sym or *Func.
    genSymMap: std.AutoHashMapUnmanaged(*anyopaque, bcgen.Sym),

    /// Imports are queued.
    importTasks: std.ArrayListUnmanaged(ImportTask),

    config: cc.CompileConfig,

    /// Tracks whether an error was set from the API.
    hasApiError: bool,
    apiError: []const u8, // Duped so Cyber owns the msg.

    /// Whether builtins should be imported.
    importBuiltins: bool = true,

    iteratorMGID: vmc.MethodGroupId = cy.NullId,
    seqIteratorMGID: vmc.MethodGroupId = cy.NullId,
    nextMGID: vmc.MethodGroupId = cy.NullId,
    nextSeqMGID: vmc.MethodGroupId = cy.NullId,
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

    main_chunk: *cy.Chunk,

    pub fn init(self: *Compiler, vm: *cy.VM) !void {
        self.* = .{
            .alloc = vm.alloc,
            .vm = vm,
            .buf = try cy.ByteCodeBuffer.init(vm.alloc, vm),
            .jitBuf = jitgen.CodeBuffer.init(),
            .reports = .{},
            .sema = sema.Sema.init(vm.alloc, self),
            .moduleLoader = defaultModuleLoader,
            .moduleResolver = defaultModuleResolver,
            .chunks = .{},
            .chunkMap = .{},
            .genSymMap = .{},
            .importTasks = .{},
            .config = cc.defaultCompileConfig(), 
            .hasApiError = false,
            .apiError = "",
            .main_chunk = undefined,
        };
        try self.reinit();    
    }

    pub fn deinitModRetained(self: *Compiler) void {
        for (self.chunks.items) |chunk| {
            for (chunk.syms.items) |sym| {
                sym.deinitRetained(self.vm);
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
        self.deinitModRetained();

        // Free any remaining import tasks.
        for (self.importTasks.items) |task| {
            self.alloc.free(task.absSpec);
        }

        for (self.chunks.items) |chunk| {
            log.tracev("Deinit chunk `{s}`", .{chunk.srcUri});
            if (chunk.onDestroy) |onDestroy| {
                onDestroy(@ptrCast(self.vm), cy.Sym.toC(@ptrCast(chunk.sym)));
            }
            chunk.deinit();
            self.alloc.destroy(chunk);
        }
        if (reset) {
            self.chunks.clearRetainingCapacity();
            self.chunkMap.clearRetainingCapacity();
            self.genSymMap.clearRetainingCapacity();
            self.importTasks.clearRetainingCapacity();
        } else {
            self.chunks.deinit(self.alloc);
            self.chunkMap.deinit(self.alloc);
            self.genSymMap.deinit(self.alloc);
            self.importTasks.deinit(self.alloc);
        }

        // Chunks depends on modules.
        self.sema.deinit(self.alloc, reset);

        self.alloc.free(self.apiError);
        self.apiError = "";
    }

    pub fn reinit(self: *Compiler) !void {
        self.clearReports();
    }

    pub fn clearReports(self: *Compiler) void {
        for (self.reports.items) |report| {
            report.deinit(self.alloc);
        }
        self.reports.clearRetainingCapacity();
    }

    pub fn compile(self: *Compiler, srcUri: []const u8, src: []const u8, config: cc.CompileConfig) !CompileResult {
        defer {
            // Update VM types view.
            self.vm.types = self.sema.types.items;
        }
        const res = self.compileInner(srcUri, src, config) catch |err| {
            if (dumpCompileErrorStackTrace and !cc.silent()) {
                std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            }
            if (err == error.CompileError) {
                return err;
            }
            if (self.chunks.items.len > 0) {
                // Report other errors using the main chunk.
                return self.main_chunk.reportErrorFmt("Error: {}", &.{v(err)}, null);
            } else {
                try self.addReportFmt(.compile_err, null, null, "Error: {}", &.{v(err)});
                return error.CompileError;
            }
        };
        return res;
    }

    /// Wrap compile so all errors can be handled in one place.
    fn compileInner(self: *Compiler, srcUri: []const u8, src: []const u8, config: cc.CompileConfig) !CompileResult {
        self.config = config;

        var finalSrcUri: []const u8 = undefined;
        if (!cy.isWasm and builtin.os.tag != .freestanding and config.file_modules) {
            // Ensure that `srcUri` is resolved.
            finalSrcUri = std.fs.cwd().realpathAlloc(self.alloc, srcUri) catch |err| {
                log.tracev("Could not resolve main src uri: {s}", .{srcUri});
                return err;
            };
        } else {
            finalSrcUri = try self.alloc.dupe(u8, srcUri);
        }

        const srcDup = try self.alloc.dupe(u8, src);

        try loadBuiltins(self);

        // TODO: Types and symbols should be loaded recursively for single-threaded.
        //       Separate into two passes one for types and function signatures, and
        //       another for function bodies.

        // Load core module first since the members are imported into each user module.
        var core_sym: *cy.sym.Chunk = undefined;
        if (self.importBuiltins) {
            const importCore = ImportTask{
                .fromChunk = null,
                .nodeId = cy.NullNode,
                .absSpec = try self.alloc.dupe(u8, "builtins"),
                .import = null,
            };
            try self.importTasks.append(self.alloc, importCore);
            const core_chunk = performImportTask(self, importCore) catch |err| {
                return err;
            };
            core_sym = core_chunk.sym;
            _ = self.importTasks.orderedRemove(0);
            try reserveCoreTypes(self);
            try createDynMethodIds(self);
        }

        // Main chunk.
        const nextId: u32 = @intCast(self.chunks.items.len);
        var mainChunk = try self.alloc.create(cy.Chunk);
        mainChunk.* = try cy.Chunk.init(self, nextId, finalSrcUri, srcDup);
        mainChunk.sym = try mainChunk.createChunkSym(finalSrcUri);
        self.main_chunk = mainChunk;
        try self.chunks.append(self.alloc, mainChunk);
        try self.chunkMap.put(self.alloc, finalSrcUri, mainChunk);

        // All modules and data types are loaded first.
        try declareImportsAndTypes(self, core_sym);

        // Declare static vars and funcs after types have been resolved.
        try declareSymbols(self);

        // Compute type sizes after type fields have been resolved.
        // try computeTypeSizesRec(self);

        // Perform sema on static initializers.
        log.tracev("Perform init sema.", .{});
        for (self.chunks.items) |chunk| {
            // First stmt is root at index 0.
            _ = try chunk.ir.pushEmptyStmt2(chunk.alloc, .root, chunk.parserAstRootId, false);
            try chunk.ir.pushStmtBlock2(chunk.alloc, chunk.rootStmtBlock);

            chunk.initializerVisited = false;
            chunk.initializerVisiting = false;
            if (chunk.hasStaticInit) {
                try performChunkInitSema(self, chunk);
            }

            chunk.rootStmtBlock = chunk.ir.popStmtBlock();
        }

        // Perform sema on all chunks.
        log.tracev("Perform sema.", .{});
        for (self.chunks.items) |chunk| {
            performChunkSema(self, chunk) catch |err| {
                if (err == error.CompileError) {
                    return err;
                } else {
                    // Wrap all other errors as a CompileError.
                    return chunk.reportErrorFmt("error.{}", &.{v(err)}, chunk.curNodeId);
                }
                return err;
            };
        }

        // Perform deferred sema.
        for (self.chunks.items) |chunk| {
            try chunk.ir.pushStmtBlock2(chunk.alloc, chunk.rootStmtBlock);
            for (chunk.variantFuncSyms.items) |func| {
                if (func.isMethod) {
                    try sema.methodDecl(chunk, func);
                } else {
                    try sema.funcDecl(chunk, func);
                }
            }
            chunk.rootStmtBlock = chunk.ir.popStmtBlock();
            // No more statements are added to the chunks root, so update bodyHead.
            chunk.ir.setStmtData(0, .root, .{ .bodyHead = chunk.rootStmtBlock.first });
        }

        if (!config.skip_codegen) {
            log.tracev("Perform codegen.", .{});

            switch (self.config.backend) {
                cc.BackendJIT => {
                    if (cy.isWasm) return error.Unsupported;
                    try jitgen.gen(self);
                    return .{ .jit = .{
                        .mainStackSize = self.buf.mainStackSize,
                        .buf = self.jitBuf,
                    }};
                },
                cc.BackendTCC, cc.BackendCC => {
                    if (cy.isWasm or !cy.hasCLI) return error.Unsupported;
                    const res = try cgen.gen(self);
                    return .{ .aot = res };
                },
                cc.BackendLLVM => {
                    // try llvm_gen.genNativeBinary(self);
                    return error.TODO;
                },
                cc.BackendVM => {
                    try bcgen.genAll(self);
                    return .{
                        .vm = self.buf,
                    };
                },
                else => return error.Unsupported,
            }
            log.tracev("Done. Perform codegen.", .{});
        }

        return CompileResult{ .vm = undefined };
    }

    pub fn addReportFmt(self: *Compiler, report_t: ReportType, chunk: ?cy.ChunkId, loc: ?u32, format: []const u8, args: []const fmt.FmtValue) !void {
        const msg = try fmt.allocFormat(self.alloc, format, args);
        try self.addReportConsume(report_t, chunk, loc, msg);
    }

    /// Assumes `msg` is heap allocated.
    pub fn addReportConsume(self: *Compiler, report_t: ReportType, chunk: ?cy.ChunkId, loc: ?u32, msg: []const u8) !void {
        try self.reports.append(self.alloc, .{
            .type = report_t,
            .chunk = chunk orelse cy.NullId,
            .loc = loc orelse cy.NullId,
            .msg = msg,
        });
    }

    pub fn addReport(self: *Compiler, report_t: ReportType, chunk: ?cy.ChunkId, loc: ?u32, msg: []const u8) !void {
        const dupe = try self.alloc.dupe(u8, msg);
        try self.addReportConsume(report_t, chunk, loc, dupe);
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
            try c.compiler.addReportFmt(.parse_err, c.id, pos, format, args);
            return error.ParseError;
        }

        fn tokenizerReport(ctx: *anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror!void {
            const c: *cy.Chunk = @ptrCast(@alignCast(ctx));
            try c.compiler.addReportFmt(.token_err, c.id, pos, format, args);
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
    chunk.parserAstRootId = res.root_id;
}

/// Sema pass.
/// Symbol resolving, type checking, and builds the model for codegen.
fn performChunkSema(self: *Compiler, chunk: *cy.Chunk) !void {
    try chunk.ir.pushStmtBlock2(chunk.alloc, chunk.rootStmtBlock);

    if (chunk == self.main_chunk) {
        _ = try sema.semaMainBlock(self, chunk);
    }
    // Top level declarations only.
    try performChunkSemaDecls(chunk);

    chunk.rootStmtBlock = chunk.ir.popStmtBlock();
}

fn performChunkSemaDecls(c: *cy.Chunk) !void {
    for (c.parser.staticDecls.items) |decl| {
        switch (decl.declT) {
            .func => {
                if (decl.data.func.isMethod) {
                    try sema.methodDecl(c, decl.data.func);
                } else {
                    try sema.funcDecl(c, decl.data.func);
                }
            },
            .implicit_method => {
                if (decl.data.implicit_method.type == .userFunc) {
                    try sema.methodDecl(c, decl.data.implicit_method);
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

    const decl = try c.parser.ast.pushNode(self.alloc, .funcDecl, cy.NullId);
    const header = try c.parser.ast.pushNode(self.alloc, .funcHeader, cy.NullNode);
    const name = try c.parser.ast.genSpanNode(self.alloc, .ident, "$init", null);
    c.parser.ast.setNodeData(header, .{ .funcHeader = .{
        .name = name,
        .paramHead = cy.NullNode,
    }});
    c.parser.ast.setNodeData(decl, .{ .func = .{
        .header = header,
        .bodyHead = cy.NullNode,
        .sig_t = .func,
    }});
    c.updateAstView(c.parser.ast.view());

    const func = try c.declareUserFunc(@ptrCast(c.sym), "$init", funcSigId, decl, false);

    _ = try sema.pushFuncProc(c, func);

    for (c.parser.staticDecls.items) |sdecl| {
        switch (sdecl.declT) {
            .variable => {
                const node = c.ast.node(sdecl.nodeId);
                if (node.type() == .staticDecl) {
                    try sema.staticDecl(c, sdecl.data.sym, sdecl.nodeId);
                }
            },
            else => {},
        }
    }

    // Pop unordered stmts list.
    _ = c.ir.popStmtBlock();
    // Create a new stmt list.
    try c.ir.pushStmtBlock(c.alloc);

    // Reorder local declarations in DFS order by patching next stmt IR.
    for (c.parser.staticDecls.items) |sdecl| {
        switch (sdecl.declT) {
            .variable => {
                const node = c.ast.node(sdecl.nodeId);
                if (node.type() == .staticDecl) {
                    const info = c.symInitInfos.getPtr(sdecl.data.sym).?;
                    try appendSymInitIrDFS(c, sdecl.data.sym, info, cy.NullNode);
                }
            },
            else => {},
        }
    }

    try sema.popFuncBlock(c);
}

fn appendSymInitIrDFS(c: *cy.Chunk, sym: *cy.Sym, info: *cy.chunk.SymInitInfo, refNodeId: cy.NodeId) !void {
    if (info.visited) {
        return;
    }
    if (info.visiting) {
        return c.reportErrorFmt("Referencing `{}` creates a circular dependency in the module.", &.{v(sym.name())}, refNodeId);
    }
    info.visiting = true;

    const deps = c.symInitDeps.items[info.depStart..info.depEnd];
    if (deps.len > 0) {
        for (deps) |dep| {
            if (c.symInitInfos.getPtr(dep.sym)) |depInfo| {
                try appendSymInitIrDFS(c, dep.sym, depInfo, dep.refNodeId);
            }
        }
    }

    // Append stmt in the correct order.
    c.ir.appendToParent(info.irStart);
    // Reset this stmt's next or it can end up creating a cycle list.
    c.ir.setStmtNext(info.irStart, cy.NullId);

    info.visited = true;
}

fn performImportTask(self: *Compiler, task: ImportTask) !*cy.Chunk {
    // Initialize defaults.
    var res: cc.ModuleLoaderResult = .{
        .src = "",
        .srcLen = 0,
        .funcLoader = null,
        .varLoader = null,
        .typeLoader = null,
        .onTypeLoad = null,
        .onLoad = null,
        .onDestroy = null,
        .onReceipt = null,
    };

    self.hasApiError = false;
    log.tracev("Invoke module loader: {s}", .{task.absSpec});

    if (!self.moduleLoader.?(@ptrCast(self.vm), cc.toStr(task.absSpec), &res)) {
        if (task.fromChunk) |from| {
            if (task.nodeId == cy.NullNode) {
                if (self.hasApiError) {
                    return from.reportError(self.apiError, null);
                } else {
                    return from.reportErrorFmt("Failed to load module: {}", &.{v(task.absSpec)}, null);
                }
            } else {
                const stmt = from.ast.node(task.nodeId);
                if (self.hasApiError) {
                    return from.reportErrorFmt(self.apiError, &.{}, stmt.data.importStmt.spec);
                } else {
                    return from.reportErrorFmt("Failed to load module: {}", &.{v(task.absSpec)}, stmt.data.importStmt.spec);
                }
            }
        } else {
            try self.addReportFmt(.compile_err, null, null, "Failed to load module: {}", &.{v(task.absSpec)});
            return error.CompileError;
        }
    }

    var src: []const u8 = undefined;
    if (res.srcLen > 0) {
        src = res.src[0..res.srcLen];
    } else {
        src = std.mem.sliceTo(@as([*:0]const u8, @ptrCast(res.src)), 0);
    }

    // Push another chunk.
    const newChunkId: u32 = @intCast(self.chunks.items.len);

    // Dupe src.
    const srcDup = try self.alloc.dupe(u8, src);
    if (res.onReceipt) |onReceipt| {
        onReceipt(@ptrCast(self.vm), &res);
    }
    
    // uri is already duped.
    var newChunk = try self.alloc.create(cy.Chunk);

    newChunk.* = try cy.Chunk.init(self, newChunkId, task.absSpec, srcDup);
    newChunk.sym = try newChunk.createChunkSym(task.absSpec);
    newChunk.funcLoader = res.funcLoader;
    newChunk.varLoader = res.varLoader;
    newChunk.typeLoader = res.typeLoader;
    newChunk.onTypeLoad = res.onTypeLoad;
    newChunk.onLoad = res.onLoad;
    newChunk.srcOwned = true;
    newChunk.onDestroy = res.onDestroy;

    if (task.import) |import| {
        import.sym = @ptrCast(newChunk.sym);
    }

    try self.chunks.append(self.alloc, newChunk);
    try self.chunkMap.put(self.alloc, task.absSpec, newChunk);

    return newChunk;
}

fn declareImportsAndTypes(self: *Compiler, core_sym: *cy.sym.Chunk) !void {
    log.tracev("Load imports and types.", .{});

    var id: u32 = 0;
    while (true) {
        while (id < self.chunks.items.len) : (id += 1) {
            const chunk = self.chunks.items[id];
            log.tracev("chunk parse: {}", .{chunk.id});
            try performChunkParse(self, chunk);

            if (self.importBuiltins) {
                // Import builtin module into local namespace.
                try sema.declareUsingModule(chunk, @ptrCast(core_sym));
            }

            // Process static declarations.
            for (chunk.parser.staticDecls.items) |*decl| {
                switch (decl.declT) {
                    .import => {
                        try sema.declareImport(chunk, decl.nodeId);
                    },
                    .struct_t => {
                        const sym = try sema.declareStruct(chunk, decl.nodeId);
                        decl.data = .{ .sym = @ptrCast(sym) };
                    },
                    .dynobject_t => {
                        const sym = try sema.declareDynObject(chunk, decl.nodeId);
                        decl.data = .{ .sym = @ptrCast(sym) };
                    },
                    .object => {
                        const sym = try sema.declareObject(chunk, decl.nodeId);
                        // Persist for declareObjectMembers.
                        decl.data = .{ .sym = @ptrCast(sym) };
                    },
                    .enum_t => {
                        const sym = try sema.declareEnum(chunk, decl.nodeId);
                        decl.data = .{ .sym = @ptrCast(sym) };
                    },
                    .typeAlias => {
                        try sema.declareTypeAlias(chunk, decl.nodeId);
                    },
                    .distinct_t => {
                        const sym = try sema.declareDistinctType(chunk, decl.nodeId);
                        decl.data = .{ .sym = @ptrCast(sym) };
                    },
                    .typeTemplate => {
                        const ctNodes = chunk.parser.ast.templateCtNodes.items[decl.data.typeTemplate.ctNodeStart..decl.data.typeTemplate.ctNodeEnd];
                        try sema.declareTypeTemplate(chunk, decl.nodeId, ctNodes);
                    },
                    .variable,
                    .implicit_method,
                    .func,
                    .funcInit => {},
                }
            }

            if (chunk.onTypeLoad) |onTypeLoad| {
                onTypeLoad(@ptrCast(self.vm), chunk.sym.sym().toC());
            }
        }

        // Check for import tasks.
        for (self.importTasks.items, 0..) |task, i| {
            _ = performImportTask(self, task) catch |err| {
                try self.importTasks.replaceRange(self.alloc, 0, i, &.{});
                return err;
            };
        }
        self.importTasks.clearRetainingCapacity();

        if (id == self.chunks.items.len) {
            // No more chunks were added from import tasks.
            break;
        }
    }

    // Extract special syms. Assumes chunks[0] is the builtins chunk.
    const builtins = self.chunks.items[0].sym.getMod();
    self.sema.option_tmpl = builtins.getSym("Option").?.cast(.typeTemplate);
}

fn loadBuiltins(self: *Compiler) !void {
    const builtins = &self.sema.builtins;
    try builtins.putNoClobber(self.alloc, "int64_t", .{ .type = .int_t, .data = .{ .int_t = .{
        .bits = 64,
    }}});
    try builtins.putNoClobber(self.alloc, "float64_t", .{ .type = .int_t, .data = .{ .int_t = .{
        .bits = 64,
    }}});
    try builtins.putNoClobber(self.alloc, "bool_t", .{ .type = .bool_t, .data = undefined });
}

fn reserveCoreTypes(self: *Compiler) !void {
    log.tracev("Reserve core types", .{});

    const type_ids = &[_]cy.TypeId{
        // Incomplete type.
        bt.Void,

        // Primitives.
        bt.Boolean,
        bt.Error,
        bt.Placeholder1,
        bt.Placeholder2,
        bt.Placeholder3,
        bt.Symbol,
        bt.Integer,
        bt.Float,

        // Unions.
        bt.Dynamic,
        bt.Any,

        // VM specific types.
        bt.Type,

        // Object types.
        bt.Tuple,
        bt.List,
        bt.ListIter,
        bt.Map,
        bt.MapIter,
        bt.Closure,
        bt.Lambda,
        bt.HostFunc,
        bt.ExternFunc,
        bt.String,
        bt.Array,
        bt.Fiber,
        bt.Box,
        bt.TccState,
        bt.Pointer,
        bt.MetaType,
        bt.Range,
    };

    for (type_ids) |type_id| {
        const id = try self.sema.pushType();
        std.debug.assert(id == type_id);
    }

    std.debug.assert(self.sema.types.items.len == cy.types.BuiltinEnd);
}

fn createDynMethodIds(self: *Compiler) !void {
    self.indexMGID = try self.vm.ensureMethodGroup("$index");
    self.setIndexMGID = try self.vm.ensureMethodGroup("$setIndex");
    self.sliceMGID = try self.vm.ensureMethodGroup("$slice");
    self.iteratorMGID = try self.vm.ensureMethodGroup("iterator");
    self.nextMGID = try self.vm.ensureMethodGroup("next");
    self.nextSeqMGID = try self.vm.ensureMethodGroup("nextSeq");
    self.seqIteratorMGID = try self.vm.ensureMethodGroup("seqIterator");
    self.@"infix<MGID" = try self.vm.ensureMethodGroup("$infix<");
    self.@"infix<=MGID" = try self.vm.ensureMethodGroup("$infix<=");
    self.@"infix>MGID" = try self.vm.ensureMethodGroup("$infix>");
    self.@"infix>=MGID" = try self.vm.ensureMethodGroup("$infix>=");
    self.@"infix+MGID" = try self.vm.ensureMethodGroup("$infix+");
    self.@"infix-MGID" = try self.vm.ensureMethodGroup("$infix-");
    self.@"infix*MGID" = try self.vm.ensureMethodGroup("$infix*");
    self.@"infix/MGID" = try self.vm.ensureMethodGroup("$infix/");
    self.@"infix%MGID" = try self.vm.ensureMethodGroup("$infix%");
    self.@"infix^MGID" = try self.vm.ensureMethodGroup("$infix^");
    self.@"infix&MGID" = try self.vm.ensureMethodGroup("$infix&");
    self.@"infix|MGID" = try self.vm.ensureMethodGroup("$infix|");
    self.@"infix||MGID" = try self.vm.ensureMethodGroup("$infix||");
    self.@"infix<<MGID" = try self.vm.ensureMethodGroup("$infix<<");
    self.@"infix>>MGID" = try self.vm.ensureMethodGroup("$infix>>");
    self.@"prefix-MGID" = try self.vm.ensureMethodGroup("$prefix-");
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

fn declareSymbols(self: *Compiler) !void {
    log.tracev("Load module symbols.", .{});
    for (self.chunks.items) |chunk| {
        // Process static declarations.
        var last_type_sym: *cy.Sym = undefined;
        for (chunk.parser.staticDecls.items) |*decl| {
            log.tracev("Load {s}", .{@tagName(decl.declT)});
            switch (decl.declT) {
                .variable => {
                    const sym = try sema.declareVar(chunk, decl.nodeId);
                    decl.data = .{ .sym = sym };
                    if (sym.type == .userVar) {
                        chunk.hasStaticInit = true;
                    }
                },
                .implicit_method => {
                    const method = try sema.resolveImplicitMethodDecl(chunk, last_type_sym, decl.nodeId);
                    const func = try sema.declareMethod(chunk, last_type_sym, decl.nodeId, method);
                    decl.data = .{ .implicit_method = func };
                },
                .func => {
                    const func = try sema.declareFunc(chunk, @ptrCast(chunk.sym), decl.nodeId);
                    decl.data = .{ .func = func };
                },
                .funcInit => {
                    try sema.declareFuncInit(chunk, @ptrCast(chunk.sym), decl.nodeId);
                },
                .dynobject_t,
                .struct_t,
                .object => {
                    try sema.declareObjectFields(chunk, decl.data.sym, decl.nodeId);
                    last_type_sym = decl.data.sym;
                },
                .enum_t => {
                    try sema.declareEnumMembers(chunk, @ptrCast(decl.data.sym), decl.nodeId);
                },
                .distinct_t => {
                    if (decl.data.sym.type == .distinct_t) {
                        const sym = try sema.resolveDistinctType(chunk, @ptrCast(decl.data.sym));
                        last_type_sym = sym;
                    } else {
                        last_type_sym = decl.data.sym;
                    }
                },
                .import,
                .typeTemplate,
                .typeAlias => {},
            }
        }

        if (chunk.onLoad) |onLoad| {
            onLoad(@ptrCast(self.vm), chunk.sym.sym().toC());
        }
    }
}

pub const CompileResult = union {
    vm: cy.ByteCodeBuffer,
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

const ImportTask = struct {
    fromChunk: ?*cy.Chunk,
    nodeId: cy.NodeId,
    absSpec: []const u8,
    import: ?*cy.sym.Import,
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

pub fn defaultModuleResolver(_: ?*cc.VM, params: cc.ResolverParams) callconv(.C) bool {
    params.resUri.* = params.spec.buf;
    params.resUriLen.* = params.spec.len;
    return true;
}

pub fn defaultModuleLoader(vm_: ?*cc.VM, spec: cc.Str, out_: [*c]cc.ModuleLoaderResult) callconv(.C) bool {
    const out: *cc.ModuleLoaderResult = out_;
    const name = cc.fromStr(spec);
    if (std.mem.eql(u8, name, "builtins")) {
        const vm: *cy.VM = @ptrCast(@alignCast(vm_));
        const aot = cy.isAot(vm.compiler.config.backend);
        out.* = .{
            .src = if (aot) cy_mod.Src else cy_mod.VmSrc,
            .srcLen = if (aot) cy_mod.Src.len else cy_mod.VmSrc.len,
            .funcLoader = cy_mod.funcLoader,
            .typeLoader = if (aot) cy_mod.typeLoader else cy_mod.vmTypeLoader,
            .onLoad = cy_mod.onLoad,
            .onReceipt = null,
            .varLoader = null,
            .onTypeLoad = null,
            .onDestroy = null,
        };
        return true;
    } else if (std.mem.eql(u8, name, "math")) {
        out.* = .{
            .src = math_mod.Src,
            .srcLen = math_mod.Src.len,
            .funcLoader = math_mod.funcLoader,
            .varLoader = math_mod.varLoader,
            .typeLoader = null,
            .onLoad = null,
            .onReceipt = null,
            .onTypeLoad = null,
            .onDestroy = null,
        };
        return true;
    }
    return false;
}

test "vm compiler internals." {
    try t.eq(@offsetOf(Compiler, "buf"), @offsetOf(vmc.Compiler, "buf"));
    try t.eq(@offsetOf(Compiler, "reports"), @offsetOf(vmc.Compiler, "reports"));
    try t.eq(@offsetOf(Compiler, "sema"), @offsetOf(vmc.Compiler, "sema"));
}