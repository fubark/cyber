const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const cc = @import("clib.zig");
const rt = cy.rt;
const fmt = @import("fmt.zig");
const v = fmt.v;
const vm_ = @import("vm.zig");
const vmc = @import("vm_c.zig");
const sema = cy.sema;
const types = cy.types;
const bt = types.BuiltinTypes;
const cy_mod = @import("builtins/builtins.zig");
const math_mod = @import("builtins/math.zig");
const llvm_gen = @import("llvm_gen.zig");
const bcgen = @import("bc_gen.zig");
const jitgen = @import("jit/gen.zig");
const assembler = @import("jit/assembler.zig");
const bindings = cy.bindings;
const module = cy.module;

const log = cy.log.scoped(.vm_compiler);

const f64NegOne = cy.Value.initF64(-1);
const f64One = cy.Value.initF64(1);

const dumpCompileErrorStackTrace = builtin.mode == .Debug and !cy.isWasm and true;

const Root = @This();

pub const VMcompiler = struct {
    alloc: std.mem.Allocator,
    vm: *cy.VM,
    buf: cy.ByteCodeBuffer,
    jitBuf: jitgen.CodeBuffer,

    lastErr: []const u8,
    
    /// Sema model resulting from the sema pass.
    sema: sema.Sema,

    lastErrNode: cy.NodeId,
    lastErrChunk: cy.ChunkId,

    /// Used to return additional info for an error.
    errorPayload: cy.NodeId,

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

    config: CompileConfig,

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

    pub fn init(self: *VMcompiler, vm: *cy.VM) !void {
        self.* = .{
            .alloc = vm.alloc,
            .vm = vm,
            .buf = try cy.ByteCodeBuffer.init(vm.alloc, vm),
            .jitBuf = jitgen.CodeBuffer.init(),
            .lastErr = "",
            .lastErrNode = undefined,
            .lastErrChunk = undefined,
            .errorPayload = undefined,
            .sema = sema.Sema.init(vm.alloc, self),
            .moduleLoader = defaultModuleLoader,
            .moduleResolver = defaultModuleResolver,
            .chunks = .{},
            .chunkMap = .{},
            .genSymMap = .{},
            .importTasks = .{},
            .config = .{}, 
            .hasApiError = false,
            .apiError = "",
        };
        try self.reinit();    
    }

    pub fn deinitModRetained(self: *VMcompiler) void {
        for (self.chunks.items) |chunk| {
            const chunkMod = chunk.sym.getMod();
            chunkMod.deinitRetained(self.vm);

            for (chunk.modSyms.items) |modSym| {
                const mod = modSym.getMod().?;
                mod.deinitRetained(self.vm);
            }
        }
    }

    pub fn deinit(self: *VMcompiler, comptime reset: bool) void {
        self.alloc.free(self.lastErr);
        self.lastErr = "";

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
            self.alloc.destroy(task.sym);
        }

        for (self.chunks.items) |chunk| {
            log.tracev("Deinit chunk `{s}`", .{chunk.srcUri});
            if (chunk.onDestroy) |onDestroy| {
                onDestroy(@ptrCast(self.vm), cc.ApiModule{ .sym = @ptrCast(chunk.sym) });
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

    pub fn reinit(self: *VMcompiler) !void {
        self.lastErrNode = cy.NullId;
        self.lastErrChunk = cy.NullId;
    }

    pub fn compile(self: *VMcompiler, srcUri: []const u8, src: []const u8, config: CompileConfig) !CompileResultView {
        defer {
            // Update VM types view.
            self.vm.types = self.sema.types.items;
        }
        self.compileInner(srcUri, src, config) catch |err| {

            if (err == error.TokenError) {
                return CompileResultView{
                    .buf = self.buf,
                    .jitBuf = self.jitBuf,
                    .err = .tokenize,
                };
            } else if (err == error.ParseError) {
                return CompileResultView{
                    .buf = self.buf,
                    .jitBuf = self.jitBuf,
                    .err = .parse,
                };
            } else {
                if (dumpCompileErrorStackTrace and !cy.silentError) {
                    std.debug.dumpStackTrace(@errorReturnTrace().?.*);
                }
                if (err != error.CompileError) {
                    if (self.chunks.items.len > 0) {
                        // Report other errors using the main chunk.
                        const chunk = self.chunks.items[0];
                        try chunk.setErrorFmtAt("Error: {}", &.{v(err)}, cy.NullId);
                    } else {
                        return err;
                    }
                }
                return CompileResultView{
                    .buf = self.buf,
                    .jitBuf = self.jitBuf,
                    .err = .compile,
                };
            }
        };
        return CompileResultView{
            .buf = self.buf,
            .jitBuf = self.jitBuf,
            .err = null,
        };
    }

    /// Wrap compile so all errors can be handled in one place.
    fn compileInner(self: *VMcompiler, srcUri: []const u8, src: []const u8, config: CompileConfig) !void {
        self.config = config;

        var finalSrcUri: []const u8 = undefined;
        if (!cy.isWasm and config.enableFileModules) {
            // Ensure that `srcUri` is resolved.
            finalSrcUri = std.fs.cwd().realpathAlloc(self.alloc, srcUri) catch |err| {
                log.debug("Could not resolve main src uri: {s}", .{srcUri});
                return err;
            };
        } else {
            finalSrcUri = try self.alloc.dupe(u8, srcUri);
        }

        const srcDup = try self.alloc.dupe(u8, src);

        // Main chunk.
        const mainSym = try self.sema.createChunkSym();
        const nextId: u32 = @intCast(self.chunks.items.len);
        var mainChunk = try self.alloc.create(cy.Chunk);
        mainChunk.* = try cy.Chunk.init(self, nextId, finalSrcUri, srcDup, mainSym);
        mainSym.mod.chunk = mainChunk;
        mainSym.head.namePtr = finalSrcUri.ptr;
        mainSym.head.nameLen = @intCast(finalSrcUri.len);
        try self.chunks.append(self.alloc, mainChunk);
        try self.chunkMap.put(self.alloc, finalSrcUri, mainChunk);

        // All modules and data types are loaded first.
        try declareImportsAndTypes(self, mainChunk);

        // Declare static vars and funcs after types have been resolved.
        try declareSymbols(self);

        // Perform sema on static initializers.
        log.tracev("Perform init sema.", .{});
        for (self.chunks.items) |chunk| {
            // First stmt is root at index 0.
            _ = try chunk.irPushEmptyStmt2(.root, chunk.parserAstRootId, false);
            try chunk.irPushStmtBlock();

            chunk.initializerVisited = false;
            chunk.initializerVisiting = false;
            if (chunk.hasStaticInit) {
                try performChunkInitSema(self, chunk);
            }
        }

        // Perform sema on all chunks.
        log.tracev("Perform sema.", .{});
        for (self.chunks.items) |chunk| {
            performChunkSema(self, chunk) catch |err| {
                if (err == error.CompileError) {
                    return err;
                } else {
                    // Wrap all other errors as a CompileError.
                    try chunk.setErrorFmtAt("error.{}", &.{v(err)}, chunk.curNodeId);
                    return error.CompileError;
                }
                return err;
            };
        }

        if (!config.skipCodegen) {
            try performCodegen(self);
        }
    }

    /// If `chunkId` is NullId, then the error comes from an aggregate step.
    /// If `nodeId` is NullId, then the error does not have a location.
    pub fn setErrorFmtAt(self: *VMcompiler, chunkId: cy.ChunkId, nodeId: cy.NodeId, format: []const u8, args: []const fmt.FmtValue) !void {
        self.alloc.free(self.lastErr);
        self.lastErr = try fmt.allocFormat(self.alloc, format, args);
        self.lastErrChunk = chunkId;
        self.lastErrNode = nodeId;
    }

    /// Assumes `msg` is heap allocated.
    pub fn setErrorAt(self: *VMcompiler, chunkId: cy.ChunkId, nodeId: cy.NodeId, msg: []const u8) !void {
        self.alloc.free(self.lastErr);
        self.lastErr = msg;
        self.lastErrChunk = chunkId;
        self.lastErrNode = nodeId;
    }
};

/// Tokenize and parse.
/// Parser pass collects static declaration info.
fn performChunkParse(self: *VMcompiler, chunk: *cy.Chunk) !void {
    var tt = cy.debug.timer();
    const ast = try chunk.parser.parse(chunk.src);
    tt.endPrint("parse");
    // Update buffer pointers so success/error paths can access them.
    chunk.nodes = ast.nodes.items;
    chunk.tokens = ast.tokens;
    chunk.ast = .{
        .nodes = ast.nodes.items,
        .tokens = ast.tokens,
        .src = ast.src,
    };
    chunk.encoder.src = chunk.ast;
    if (ast.has_error) {
        self.lastErrChunk = chunk.id;
        if (ast.isTokenError) {
            return error.TokenError;
        } else {
            return error.ParseError;
        }
    }
    chunk.parserAstRootId = ast.root_id;
}

/// Sema pass.
/// Symbol resolving, type checking, and builds the model for codegen.
fn performChunkSema(self: *VMcompiler, chunk: *cy.Chunk) !void {
    if (chunk.id == 0) {
        _ = try sema.semaMainBlock(self, chunk);
    }
    // Top level declarations only.
    try performChunkSemaDecls(chunk);

    // End root.
    const stmtBlock = chunk.irPopStmtBlock();
    chunk.irSetStmtData(0, .root, .{ .bodyHead = stmtBlock.first });
}

fn performChunkSemaDecls(c: *cy.Chunk) !void {
    for (c.parser.staticDecls.items) |decl| {
        switch (decl.declT) {
            .func => {
                // Skip flat method decls since they are processed in objectDecl.
                if (decl.data.func.isMethod) continue;
                try sema.funcDecl(c, decl.data.func, decl.nodeId);
            },
            .object => {
                try sema.objectDecl(c, @ptrCast(decl.data.sym), decl.nodeId);
            },
            else => {},
        }
    }
}

/// Sema on static initializers.
fn performChunkInitSema(_: *VMcompiler, c: *cy.Chunk) !void {
    log.tracev("Perform init sema. {} {s}", .{c.id, c.srcUri});

    const funcSigId = try c.sema.ensureFuncSig(&.{}, bt.None);
    const func = try c.declareUserFunc(@ptrCast(c.sym), "$init", funcSigId, cy.NullId, false);

    _ = try sema.pushFuncBlock(c, func);

    for (c.parser.staticDecls.items) |sdecl| {
        switch (sdecl.declT) {
            .variable => {
                const node = c.nodes[sdecl.nodeId];
                if (node.node_t == .staticDecl) {
                    try sema.staticDecl(c, sdecl.data.sym, sdecl.nodeId);
                }
            },
            else => {},
        }
    }

    // Pop unordered stmts list.
    _ = c.irPopStmtBlock();
    // Create a new stmt list.
    try c.irPushStmtBlock();

    // Reorder local declarations in DFS order by patching next stmt IR.
    for (c.parser.staticDecls.items) |sdecl| {
        switch (sdecl.declT) {
            .variable => {
                const node = c.nodes[sdecl.nodeId];
                if (node.node_t == .staticDecl) {
                    const info = c.symInitInfos.getPtr(sdecl.data.sym).?;
                    try appendSymInitIrDFS(c, sdecl.data.sym, info, cy.NullId);
                }
            },
            else => {},
        }
    }

    try sema.popStaticFuncBlock(c);
}

fn appendSymInitIrDFS(c: *cy.Chunk, sym: *cy.Sym, info: *cy.chunk.SymInitInfo, refNodeId: cy.NodeId) !void {
    if (info.visited) {
        return;
    }
    if (info.visiting) {
        return c.reportErrorAt("Referencing `{}` creates a circular dependency in the module.", &.{v(sym.name())}, refNodeId);
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
    c.irAppendToParent(info.irStart);
    // Reset this stmt's next or it can end up creating a cycle list.
    c.irSetNextStmt(info.irStart, cy.NullId);

    info.visited = true;
}

fn performChunkCodegen(self: *VMcompiler, chunk: *cy.Chunk) !void {
    chunk.buf = &self.buf;
    try bcgen.genChunk(chunk);
}

fn performImportTask(self: *VMcompiler, task: ImportTask) !void {
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

    if (!self.moduleLoader.?(@ptrCast(self.vm), cc.initStr(task.absSpec), &res)) {
        const chunk = task.fromChunk;
        if (task.nodeId == cy.NullId) {
            if (self.hasApiError) {
                return chunk.reportError(self.apiError, &.{});
            } else {
                return chunk.reportError("Failed to load module: {}", &.{v(task.absSpec)});
            }
        } else {
            const stmt = chunk.nodes[task.nodeId];
            if (self.hasApiError) {
                return chunk.reportErrorAt(self.apiError, &.{}, stmt.head.left_right.right);
            } else {
                return chunk.reportErrorAt("Failed to load module: {}", &.{v(task.absSpec)}, stmt.head.left_right.right);
            }
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
    newChunk.* = try cy.Chunk.init(self, newChunkId, task.absSpec, srcDup, task.sym);
    newChunk.funcLoader = res.funcLoader;
    newChunk.varLoader = res.varLoader;
    newChunk.typeLoader = res.typeLoader;
    newChunk.onTypeLoad = res.onTypeLoad;
    newChunk.onLoad = res.onLoad;
    newChunk.srcOwned = true;
    newChunk.onDestroy = res.onDestroy;

    try self.chunks.append(self.alloc, newChunk);
    try self.chunkMap.put(self.alloc, task.absSpec, newChunk);
    task.sym.mod.chunk = newChunk;
    task.sym.head.namePtr = task.absSpec.ptr;
    task.sym.head.nameLen = @intCast(task.absSpec.len);
}

fn declareImportsAndTypes(self: *VMcompiler, mainChunk: *cy.Chunk) !void {
    log.tracev("Load imports and types.", .{});

    // Load core module first since the members are imported into each user module.
    var builtinSym: *cy.sym.Chunk = undefined;
    if (self.importBuiltins) {
        builtinSym = try self.sema.createChunkSym();
        const importCore = ImportTask{
            .fromChunk = mainChunk,
            .nodeId = cy.NullId,
            .absSpec = try self.alloc.dupe(u8, "builtins"),
            .sym = builtinSym,
        };
        try self.importTasks.append(self.alloc, importCore);
        performImportTask(self, importCore) catch |err| {
            return err;
        };
        _ = self.importTasks.orderedRemove(0);
        try loadPredefinedTypes(self, @ptrCast(builtinSym));
    }

    var id: u32 = 0;
    while (true) {
        while (id < self.chunks.items.len) : (id += 1) {
            const chunk = self.chunks.items[id];
            log.debug("chunk parse: {}", .{chunk.id});
            try performChunkParse(self, chunk);

            if (self.importBuiltins) {
                // Import builtin module into local namespace.
                try sema.declareUsingModule(chunk, @ptrCast(builtinSym));
            }

            // Process static declarations.
            for (chunk.parser.staticDecls.items) |*decl| {
                switch (decl.declT) {
                    .import => {
                        try sema.declareImport(chunk, decl.nodeId);
                    },
                    .object => {
                        const sym = try sema.declareObject(chunk, decl.nodeId);
                        // Persist for declareObjectMembers.
                        decl.data = .{ .sym = @ptrCast(sym) };
                    },
                    .enumT => {
                        try sema.declareEnum(chunk, decl.nodeId);
                    },
                    .typeAlias => {
                        try sema.declareTypeAlias(chunk, decl.nodeId);
                    },
                    .variable,
                    .func,
                    .funcInit => {},
                }
            }

            if (chunk.onTypeLoad) |onTypeLoad| {
                onTypeLoad(@ptrCast(self.vm), cc.ApiModule{ .sym = @ptrCast(chunk.sym) });
            }
        }

        // Check for import tasks.
        for (self.importTasks.items, 0..) |task, i| {
            performImportTask(self, task) catch |err| {
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
}

fn loadPredefinedTypes(self: *VMcompiler, parent: *cy.Sym) !void {
    log.tracev("Load predefined types", .{});
    const Entry = struct { []const u8, cy.TypeId };
    const entries = &[_]Entry{
        .{"none", bt.None},
        .{"bool", bt.Boolean},
        .{"error", bt.Error},
        .{"placeholder1", bt.Placeholder1},
        .{"placeholder2", bt.Placeholder2},
        .{"placeholder3", bt.Placeholder3},
        .{"symbol", bt.Symbol},
        .{"int", bt.Integer},
        .{"float", bt.Float},

        // Begin object types.
        .{"tuple", bt.Tuple},
        .{"List", bt.List},
        .{"ListIterator", bt.ListIter},
        .{"Map", bt.Map},
        .{"MapIterator", bt.MapIter},
        .{"Closure", bt.Closure},
        .{"Lambda", bt.Lambda},
        .{"string", bt.String},
        .{"array", bt.Array},
        .{"Fiber", bt.Fiber},
        .{"Box", bt.Box},
        .{"HostFunc", bt.HostFunc},
        .{"TccState", bt.TccState},
        .{"pointer", bt.Pointer},
        .{"metatype", bt.MetaType},
        .{"ExternFunc", bt.ExternFunc},

        .{"dynamic", bt.Dynamic},
        .{"any", bt.Any},
    };

    const mod = parent.getMod().?;
    const c = mod.chunk;

    for (entries) |entry| {
        const id = try self.sema.pushType();
        std.debug.assert(id == entry.@"1");
        _ = try c.declarePredefinedType(parent, entry.@"0", id);
    }

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

    // id = try sema.addResolvedInternalSym(self, "undefined");
    // std.debug.assert(id == bt.Undefined);
    // id = try sema.addResolvedInternalSym(self, "dynamic");
    // std.debug.assert(id == bt.Dynamic);
}

fn declareSymbols(self: *VMcompiler) !void {
    log.tracev("Load module symbols.", .{});
    for (self.chunks.items) |chunk| {
        // Process static declarations.
        for (chunk.parser.staticDecls.items) |*decl| {
            log.tracev("Load {s}", .{@tagName(decl.declT)});
            switch (decl.declT) {
                .variable => {
                    const sym = try sema.declareVar(chunk, decl.nodeId);
                    decl.data = .{ .sym = sym };
                    const node = chunk.nodes[decl.nodeId];
                    if (node.node_t == .staticDecl) {
                        chunk.hasStaticInit = true;
                    }
                },
                .func => {
                    const func = try sema.declareFunc(chunk, @ptrCast(chunk.sym), decl.nodeId);
                    decl.data = .{ .func = func };
                },
                .funcInit => {
                    try sema.declareFuncInit(chunk, @ptrCast(chunk.sym), decl.nodeId);
                    const node = chunk.nodes[decl.nodeId];
                    if (node.node_t == .funcDeclInit) {
                        chunk.hasStaticInit = true;
                    }
                },
                .object => {
                    try sema.declareObjectMembers(chunk, decl.data.sym, decl.nodeId);
                },
                .enumT,
                .import,
                .typeAlias => {},
            }
        }

        if (chunk.onLoad) |onLoad| {
            onLoad(@ptrCast(self.vm), cc.ApiModule{ .sym = @ptrCast(chunk.sym) });
        }
    }
}

fn performCodegen(self: *VMcompiler) !void {
    log.tracev("Perform codegen.", .{});

    // if (cy.hasAOT) {
    //     if (self.config.aot) {
    //         try llvm_gen.genNativeBinary(self);
    //         return;
    //     }
    // }

    if (self.config.preJit) {
        // Prepare host funcs.
        for (self.chunks.items) |chunk| {
            const mod = chunk.sym.getMod();
            for (mod.funcs.items) |func| {
                try jitgen.prepareFunc(self, func);
            }

            for (chunk.modSyms.items) |modSym| {
                const mod2 = modSym.getMod().?;
                for (mod2.funcs.items) |func| {
                    try jitgen.prepareFunc(self, func);
                }
            }
        }

        for (self.chunks.items) |chunk| {
            if (chunk.id != 0) {
                // Skip other chunks for now.
                continue;
            }

            log.tracev("Perform codegen for chunk{}: {s}", .{chunk.id, chunk.srcUri});
            chunk.buf = &self.buf;
            chunk.jitBuf = &self.jitBuf;
            try jitgen.genChunk(chunk);
            log.tracev("Done. performChunkCodegen {s}", .{chunk.srcUri});
        }

        // Perform relocation.
        for (self.jitBuf.relocs.items) |reloc| {
            switch (reloc.type) {
                .jumpToFunc => {
                    const jumpPc = reloc.data.jumpToFunc.pc;
                    const func = reloc.data.jumpToFunc.func;
                    if (func.type == .hostFunc) {
                        return error.Unexpected;
                    } else {
                        const targetPc = self.genSymMap.get(func).?.funcSym.pc;
                        const offset: i28 = @intCast(@as(i32, @bitCast(targetPc -% jumpPc)));
                        var inst: *assembler.A64.BrImm = @ptrCast(@alignCast(&self.jitBuf.buf.items[jumpPc]));
                        inst.setOffset(offset);
                    }
                }
            }
        }

    } else {
        try genBytecode(self);
    }
    log.tracev("Done. Perform codegen.", .{});
}

fn genBytecode(c: *VMcompiler) !void {
    // Constants.
    c.vm.emptyString = try c.buf.getOrPushStaticAstring("");
    c.vm.emptyArray = try cy.heap.allocArray(c.vm, "");
    try c.vm.staticObjects.append(c.alloc, c.vm.emptyArray.asHeapObject());

    // Prepare types.
    for (c.sema.types.items, 0..) |stype, typeId| {
        log.tracev("bc prepare type: {s}", .{stype.sym.name()});
        const sym = stype.sym;

        if (sym.type == .object) {
            const obj = sym.cast(.object);
            const mod = sym.getMod().?;
            for (obj.fields[0..obj.numFields], 0..) |field, i| {
                const fieldSym = mod.getSymById(field.symId);
                const fieldSymId = try c.vm.ensureFieldSym(fieldSym.name());
                try c.vm.addFieldSym(@intCast(typeId), fieldSymId, @intCast(i), field.type);
            }
        } else if (sym.type == .predefinedType) {
            // Nop.
        } else if (sym.type == .hostObjectType) {
            // Nop.
        } else if (sym.type == .enumType) {
            // Nop.
        } else {
            log.tracev("{}", .{sym.type});
            return error.Unsupported;
        }
    }

    // Prepare funcs.
    for (c.chunks.items) |chunk| {
        const mod = chunk.sym.getMod();
        for (mod.syms.items) |sym| {
            try prepareSym(c, sym);
        }
        for (mod.funcs.items) |func| {
            try prepareFunc(c, func);
        }

        for (chunk.modSyms.items) |modSym| {
            const mod2 = modSym.getMod().?;
            for (mod2.syms.items) |sym| {
                try prepareSym(c, sym);
            }
            for (mod2.funcs.items) |func| {
                try prepareFunc(c, func);
            }
        }
    }

    // Bind the rest that aren't in sema.
    try @call(.never_inline, bindings.bindCore, .{c.vm});

    for (c.chunks.items) |chunk| {
        log.tracev("Perform codegen for chunk{}: {s}", .{chunk.id, chunk.srcUri});
        try performChunkCodegen(c, chunk);
        log.tracev("Done. performChunkCodegen {s}", .{chunk.srcUri});
    }

    // Merge inst and const buffers.
    var reqLen = c.buf.ops.items.len + c.buf.consts.items.len * @sizeOf(cy.Value) + @alignOf(cy.Value) - 1;
    if (c.buf.ops.capacity < reqLen) {
        try c.buf.ops.ensureTotalCapacityPrecise(c.alloc, reqLen);
    }
    const constAddr = std.mem.alignForward(usize, @intFromPtr(c.buf.ops.items.ptr) + c.buf.ops.items.len, @alignOf(cy.Value));
    const constDst = @as([*]cy.Value, @ptrFromInt(constAddr))[0..c.buf.consts.items.len];
    const constSrc = try c.buf.consts.toOwnedSlice(c.alloc);
    std.mem.copy(cy.Value, constDst, constSrc);
    c.alloc.free(constSrc);
    c.buf.mconsts = constDst;

    // Final op address is known. Patch pc offsets.
    // for (c.vm.funcSyms.items()) |*sym| {
    //     if (sym.entryT == .func) {
    //         sym.inner.func.pc = .{ .ptr = c.buf.ops.items.ptr + sym.inner.func.pc.offset};
    //     }
    // }
    // for (c.vm.methodGroups.items()) |*sym| {
    //     if (sym.mapT == .one) {
    //         if (sym.inner.one.sym.entryT == .func) {
    //             sym.inner.one.sym.inner.func.pc = .{ .ptr = c.buf.ops.items.ptr + sym.inner.one.sym.inner.func.pc.offset };
    //         }
    //     } else if (sym.mapT == .many) {
    //         if (sym.inner.many.mruSym.entryT == .func) {
    //             sym.inner.many.mruSym.inner.func.pc = .{ .ptr = c.buf.ops.items.ptr + sym.inner.many.mruSym.inner.func.pc.offset };
    //         }
    //     }
    // }
    // var iter = c.vm.methodTable.iterator();
    // while (iter.next()) |entry| {
    //     const sym = entry.value_ptr;
    //     if (sym.entryT == .func) {
    //         sym.inner.func.pc = .{ .ptr = c.buf.ops.items.ptr + sym.inner.func.pc.offset };
    //     }
    // }
}

fn addVmFunc(c: *VMcompiler, func: *cy.Func, rtFunc: rt.FuncSymbol) !u32 {
    const id = c.vm.funcSyms.len;
    try c.vm.funcSyms.append(c.alloc, rtFunc);

    const name = func.name();
    try c.vm.funcSymDetails.append(c.alloc, .{
        .namePtr = name.ptr, .nameLen = @intCast(name.len), .funcSigId = func.funcSigId,
    });

    try c.genSymMap.putNoClobber(c.alloc, func, .{ .funcSym = .{ .id = @intCast(id), .pc = 0 }});
    return @intCast(id);
}

fn prepareSym(c: *VMcompiler, sym: *cy.Sym) !void {
    switch (sym.type) {
        .hostVar => {
            const id = c.vm.varSyms.len;
            const rtVar = rt.VarSym.init(sym.cast(.hostVar).val);
            cy.arc.retain(c.vm, rtVar.value);
            try c.vm.varSyms.append(c.alloc, rtVar);
            try c.vm.varSymExtras.append(c.alloc, sym);
            try c.genSymMap.putNoClobber(c.alloc, sym, .{ .varSym = .{ .id = @intCast(id) }});
        },
        .userVar => {
            const id = c.vm.varSyms.len;
            const rtVar = rt.VarSym.init(cy.Value.None);
            try c.vm.varSyms.append(c.alloc, rtVar);
            try c.vm.varSymExtras.append(c.alloc, sym);
            try c.genSymMap.putNoClobber(c.alloc, sym, .{ .varSym = .{ .id = @intCast(id) }});
        },
        .hostObjectType,
        .predefinedType,
        .field,
        .object,
        .func,
        .typeAlias,
        .enumType,
        .enumMember,
        .import => {},
        else => {
            log.tracev("{}", .{sym.type});
            return error.Unsupported;
        }
    }
}

fn prepareFunc(c: *VMcompiler, func: *cy.Func) !void {
    if (func.type == .userLambda) {
        return;
    }
    if (cy.Trace) {
        const symPath = try func.sym.?.head.allocAbsPath(c.alloc);
        defer c.alloc.free(symPath);
        log.tracev("bc prepare func: {s}", .{symPath});
    }
    if (func.type == .hostFunc) {
        const funcSig = c.sema.getFuncSig(func.funcSigId);
        const rtFunc = rt.FuncSymbol.initHostFunc(@ptrCast(func.data.hostFunc.ptr), funcSig.reqCallTypeCheck, funcSig.numParams(), func.funcSigId);
        _ = try addVmFunc(c, func, rtFunc);
        if (func.isMethod) {
            const parentT = func.sym.?.head.parent.?.getStaticType().?;
            const name = func.name();
            const mgId = try c.vm.ensureMethodGroup(name);
            if (funcSig.reqCallTypeCheck) {
                const m = rt.MethodInit.initHostTyped(func.funcSigId, @ptrCast(func.data.hostFunc.ptr), func.numParams);
                try c.vm.addMethod(parentT, mgId, m);
            } else {
                const m = rt.MethodInit.initHostUntyped(func.funcSigId, @ptrCast(func.data.hostFunc.ptr), func.numParams);
                try c.vm.addMethod(parentT, mgId, m);
            }
        }
    } else if (func.type == .hostInlineFunc) {
        const funcSig = c.sema.getFuncSig(func.funcSigId);
        const rtFunc = rt.FuncSymbol.initHostInlineFunc(@ptrCast(func.data.hostInlineFunc.ptr), funcSig.reqCallTypeCheck, funcSig.numParams(), func.funcSigId);
        _ = try addVmFunc(c, func, rtFunc);
        if (func.isMethod) {
            log.tracev("ismethod", .{});
            const name = func.name();
            const mgId = try c.vm.ensureMethodGroup(name);
            const parentT = func.sym.?.head.parent.?.getStaticType().?;
            log.tracev("host inline method: {s}.{s} {} {}", .{c.sema.getTypeName(parentT), name, parentT, mgId});
            const m = rt.MethodInit.initHostInline(func.funcSigId, func.data.hostInlineFunc.ptr, func.numParams);
            try c.vm.addMethod(parentT, mgId, m);
        }
    } else if (func.type == .userFunc) {
        _ = try addVmFunc(c, func, rt.FuncSymbol.initNone());
        // Func is patched later once funcPc and stackSize is obtained.
        // Method entry is also added later.
    } else {
        log.tracev("{}", .{func.type});
        return error.Unsupported;
    }
}

pub const CompileErrorType = enum {
    tokenize,
    parse,
    compile,
};

pub const CompileResultView = struct {
    buf: cy.ByteCodeBuffer,
    jitBuf: jitgen.CodeBuffer,
    err: ?CompileErrorType,
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
    fromChunk: *cy.Chunk,
    nodeId: cy.NodeId,
    absSpec: []const u8,
    sym: *cy.sym.Chunk,
};

pub fn initModuleCompat(comptime name: []const u8, comptime initFn: fn (vm: *VMcompiler, modId: cy.ModuleId) anyerror!void) cy.ModuleLoaderFunc {
    return struct {
        fn initCompat(vm: *cy.UserVM, modId: cy.ModuleId) bool {
            initFn(vm.internal().compiler, modId) catch |err| {
                log.debug("Init module `{s}` failed: {}", .{name, err});
                return false;
            };
            return true;
        }
    }.initCompat;
}

pub const CompileConfig = struct {
    singleRun: bool = false,
    skipCodegen: bool = false,
    enableFileModules: bool = false,
    genDebugFuncMarkers: bool = false,
    preJit: bool = false,
};

pub const ValidateConfig = struct {
    enableFileModules: bool = false,
};

pub fn defaultModuleResolver(_: ?*cc.VM, _: cy.ChunkId, _: cc.Str, spec_: cc.Str, res_: [*c]cc.ResolverResult) callconv(.C) bool {
    const res: *cc.ResolverResult = res_;
    res.uri = spec_.buf;
    res.uriLen = spec_.len;
    return true;
}

pub fn defaultModuleLoader(_: ?*cc.VM, spec: cc.Str, out_: [*c]cc.ModuleLoaderResult) callconv(.C) bool {
    const out: *cc.ModuleLoaderResult = out_;
    const name = cc.strSlice(spec);
    if (std.mem.eql(u8, name, "builtins")) {
        out.* = .{
            .src = cy_mod.Src,
            .srcLen = cy_mod.Src.len,
            .funcLoader = cy_mod.funcLoader,
            .typeLoader = cy_mod.typeLoader,
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

comptime {
    @export(defaultModuleResolver, .{ .name = "csDefaultModuleResolver", .linkage = .Strong });
    @export(defaultModuleLoader, .{ .name = "csDefaultModuleLoader", .linkage = .Strong });
}

test "vm compiler internals." {
    try t.eq(@offsetOf(VMcompiler, "buf"), @offsetOf(vmc.Compiler, "buf"));
    try t.eq(@offsetOf(VMcompiler, "sema"), @offsetOf(vmc.Compiler, "sema"));
}