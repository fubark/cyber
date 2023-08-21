const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const rt = cy.rt;
const fmt = @import("fmt.zig");
const v = fmt.v;
const vm_ = @import("vm.zig");
const sema = cy.sema;
const types = cy.types;
const bt = types.BuiltinTypeSymIds;
const gen = cy.codegen;
const cache = @import("cache.zig");
const core_mod = @import("builtins/core.zig");
const math_mod = @import("builtins/math.zig");
const os_mod = @import("builtins/os.zig");
const test_mod = @import("builtins/test.zig");

const log = stdx.log.scoped(.vm_compiler);

const f64NegOne = cy.Value.initF64(-1);
const f64One = cy.Value.initF64(1);

const dumpCompileErrorStackTrace = builtin.mode == .Debug and !cy.isWasm and true;

const Root = @This();

pub const VMcompiler = struct {
    alloc: std.mem.Allocator,
    vm: *cy.VM,
    buf: cy.ByteCodeBuffer,
    lastErr: []const u8,
    lastErrNode: cy.NodeId,
    lastErrChunk: cy.ChunkId,

    /// Used to return additional info for an error.
    errorPayload: cy.NodeId,

    /// Sema model resulting from the sema pass.
    sema: sema.Model,

    /// Absolute specifier to additional loaders.
    moduleLoaders: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(cy.ModuleLoaderFunc)),

    /// Compilation units indexed by their id.
    chunks: std.ArrayListUnmanaged(cy.Chunk),

    /// Imports are queued.
    importTasks: std.ArrayListUnmanaged(ImportTask),

    /// Stack for building func signatures. (eg. for nested func calls)
    typeStack: std.ArrayListUnmanaged(types.TypeId),

    /// Reused for SymIds.
    tempSyms: std.ArrayListUnmanaged(sema.ResolvedSymId),

    deinitedRtObjects: bool,

    pub fn init(self: *VMcompiler, vm: *cy.VM) !void {
        self.* = .{
            .alloc = vm.alloc,
            .vm = vm,
            .buf = try cy.ByteCodeBuffer.init(vm.alloc),
            .lastErr = "",
            .lastErrNode = undefined,
            .lastErrChunk = undefined,
            .errorPayload = undefined,
            .sema = sema.Model.init(vm.alloc),
            .chunks = .{},
            .importTasks = .{},
            .moduleLoaders = .{},
            .deinitedRtObjects = false,
            .tempSyms = .{},
            .typeStack = .{},
        };
        try self.addModuleLoader("core", initModuleCompat("core", core_mod.initModule));
        try self.addModuleLoader("math", initModuleCompat("math", math_mod.initModule));
        try self.addModuleLoader("os", initModuleCompat("os", os_mod.initModule));
        try self.addModuleLoader("test", initModuleCompat("test", test_mod.initModule));

        try self.reinit();    
    }

    pub fn deinitRtObjects(self: *VMcompiler) void {
        if (self.deinitedRtObjects) {
            return;
        }
        if (self.sema.moduleMap.get("os")) |id| {
            os_mod.deinitModule(self, self.sema.modules.items[id]) catch stdx.fatal();
        }
        self.deinitedRtObjects = true;
    }

    pub fn deinit(self: *VMcompiler, comptime reset: bool) void {
        self.alloc.free(self.lastErr);

        if (reset) {
            self.buf.clear();
        } else {
            self.buf.deinit();
        }

        self.deinitRtObjects();
        self.sema.deinit(self.alloc, reset);

        for (self.chunks.items) |*chunk| {
            chunk.deinit();
        }
        if (reset) {
            self.chunks.clearRetainingCapacity();
            self.importTasks.clearRetainingCapacity();
        } else {
            self.chunks.deinit(self.alloc);
            self.importTasks.deinit(self.alloc);
        }

        if (reset) {
            // `moduleLoaders` persists.
        } else {
            var iter = self.moduleLoaders.iterator();
            while (iter.next()) |e| {
                self.alloc.free(e.key_ptr.*);
                e.value_ptr.deinit(self.alloc);
            }
            self.moduleLoaders.deinit(self.alloc);
        }

        if (reset) {
            self.typeStack.clearRetainingCapacity();
            self.tempSyms.clearRetainingCapacity();
        } else {
            self.typeStack.deinit(self.alloc);
            self.tempSyms.deinit(self.alloc);
        }
    }

    pub fn reinit(self: *VMcompiler) !void {
        self.deinitedRtObjects = false;
        self.lastErrNode = cy.NullId;
        self.lastErrChunk = cy.NullId;

        var id = try sema.ensureNameSym(self, "any");
        std.debug.assert(id == sema.NameAny);
        id = try sema.ensureNameSym(self, "boolean");
        std.debug.assert(id == sema.NameBoolean);
        id = try sema.ensureNameSym(self, "number");
        std.debug.assert(id == sema.NameNumber);
        id = try sema.ensureNameSym(self, "int");
        std.debug.assert(id == sema.NameInt);
        id = try sema.ensureNameSym(self, "string");
        std.debug.assert(id == sema.NameString);
        id = try sema.ensureNameSym(self, "rawstring");
        std.debug.assert(id == sema.NameRawstring);
        id = try sema.ensureNameSym(self, "symbol");
        std.debug.assert(id == sema.NameSymbol);
        id = try sema.ensureNameSym(self, "List");
        std.debug.assert(id == sema.NameList);
        id = try sema.ensureNameSym(self, "Map");
        std.debug.assert(id == sema.NameMap);
        id = try sema.ensureNameSym(self, "pointer");
        std.debug.assert(id == sema.NamePointer);
        id = try sema.ensureNameSym(self, "none");
        std.debug.assert(id == sema.NameNone);
        id = try sema.ensureNameSym(self, "error");
        std.debug.assert(id == sema.NameError);
        id = try sema.ensureNameSym(self, "fiber");
        std.debug.assert(id == sema.NameFiber);
        id = try sema.ensureNameSym(self, "metatype");
        std.debug.assert(id == sema.NameMetatype);

        // Add builtins types as resolved syms.
        id = try sema.addResolvedBuiltinSym(self, "any", rt.AnyT);
        std.debug.assert(id == bt.Any);
        id = try sema.addResolvedBuiltinSym(self, "boolean", rt.BooleanT);
        std.debug.assert(id == bt.Boolean);
        id = try sema.addResolvedBuiltinSym(self, "number", rt.NumberT);
        std.debug.assert(id == bt.Number);
        id = try sema.addResolvedBuiltinSym(self, "int", rt.IntegerT);
        std.debug.assert(id == bt.Integer);
        id = try sema.addResolvedBuiltinSym(self, "string", rt.StringUnionT);
        std.debug.assert(id == bt.String);
        id = try sema.addResolvedBuiltinSym(self, "rawstring", rt.RawstringUnionT);
        std.debug.assert(id == bt.Rawstring);
        id = try sema.addResolvedBuiltinSym(self, "symbol", rt.SymbolT);
        std.debug.assert(id == bt.Symbol);
        id = try sema.addResolvedBuiltinSym(self, "List", rt.ListT);
        std.debug.assert(id == bt.List);
        id = try sema.addResolvedBuiltinSym(self, "Map", rt.MapT);
        std.debug.assert(id == bt.Map);
        id = try sema.addResolvedBuiltinSym(self, "pointer", rt.PointerT);
        std.debug.assert(id == bt.Pointer);
        id = try sema.addResolvedBuiltinSym(self, "none", rt.NoneT);
        std.debug.assert(id == bt.None);
        id = try sema.addResolvedBuiltinSym(self, "error", rt.ErrorT);
        std.debug.assert(id == bt.Error);
        id = try sema.addResolvedBuiltinSym(self, "fiber", rt.FiberT);
        std.debug.assert(id == bt.Fiber);
        id = try sema.addResolvedBuiltinSym(self, "metatype", rt.MetaTypeT);
        std.debug.assert(id == bt.MetaType);

        id = try sema.addResolvedInternalSym(self, "number");
        std.debug.assert(id == bt.NumberLit);
        id = try sema.addResolvedInternalSym(self, "undefined");
        std.debug.assert(id == bt.Undefined);
        id = try sema.addResolvedInternalSym(self, "string");
        std.debug.assert(id == bt.StaticString);
        id = try sema.addResolvedInternalSym(self, "File");
        std.debug.assert(id == bt.File);
        id = try sema.addResolvedInternalSym(self, "any");
        std.debug.assert(id == bt.Dynamic);
    }

    pub fn compile(self: *VMcompiler, srcUri: []const u8, src: []const u8, config: CompileConfig) !CompileResultView {
        self.compileInner(srcUri, src, config) catch |err| {
            if (err == error.TokenError) {
                return CompileResultView{
                    .buf = self.buf,
                    .err = .tokenize,
                };
            } else if (err == error.ParseError) {
                return CompileResultView{
                    .buf = self.buf,
                    .err = .parse,
                };
            } else {
                if (dumpCompileErrorStackTrace and !cy.silentError) {
                    std.debug.dumpStackTrace(@errorReturnTrace().?.*);
                }
                if (err != error.CompileError) {
                    if (self.chunks.items.len > 0) {
                        // Report other errors using the main chunk.
                        const chunk = &self.chunks.items[0];
                        try chunk.setErrorAt("Error: {}", &.{v(err)}, cy.NullId);
                    } else {
                        return err;
                    }
                }
                return CompileResultView{
                    .buf = self.buf,
                    .err = .compile,
                };
            }
        };
        return CompileResultView{
            .buf = self.buf,
            .err = null,
        };
    }

    /// Wrap compile so all errors can be handled in one place.
    fn compileInner(self: *VMcompiler, srcUri: []const u8, src: []const u8, config: CompileConfig) !void {
        var finalSrcUri: []const u8 = undefined;
        if (!cy.isWasm and self.vm.config.enableFileModules) {
            // Ensure that `srcUri` is resolved.
            finalSrcUri = std.fs.cwd().realpathAlloc(self.alloc, srcUri) catch |err| {
                log.debug("Could not resolve main src uri: {s}", .{srcUri});
                return err;
            };
        } else {
            finalSrcUri = try self.alloc.dupe(u8, srcUri);
        }
        defer {
            self.alloc.free(finalSrcUri);
        }

        // Main chunk.
        const mainModId = try sema.appendResolvedRootModule(self, finalSrcUri);
        const mainMod = self.sema.getModulePtr(mainModId);
        const nextId: u32 = @intCast(self.chunks.items.len);
        var mainChunk = try cy.Chunk.init(self, nextId, mainMod.absSpec, src);
        mainChunk.modId = mainModId;
        mainMod.chunkId = nextId;
        try self.chunks.append(self.alloc, mainChunk);

        // Load core module first since the members are imported into each user module.
        const coreModId = try sema.appendResolvedRootModule(self, "core");
        const importCore = ImportTask{
            .chunkId = nextId,
            .nodeId = cy.NullId,
            .absSpec = "core",
            .modId = coreModId,
            .builtin = true,
        };
        try performImportTask(self, importCore);

        // All modules and data types are loaded first.
        var id: u32 = 0;
        while (true) {
            while (id < self.chunks.items.len) : (id += 1) {
                log.debug("chunk parse: {}", .{id});
                try self.performChunkParse(id);

                const chunk = &self.chunks.items[id];
                const mod = self.sema.getModule(chunk.modId);
                chunk.semaResolvedRootSymId = mod.resolvedRootSymId;

                // Process static declarations.
                for (chunk.parser.staticDecls.items) |decl| {
                    switch (decl.declT) {
                        .import => {
                            try sema.declareImport(chunk, decl.inner.import);
                        },
                        .object => {
                            try sema.declareObject(chunk, decl.inner.object);
                        },
                        .enumT => {
                            try sema.declareEnum(chunk, decl.inner.enumT);
                        },
                        .typeAlias => {
                            try sema.declareTypeAlias(chunk, decl.inner.typeAlias);
                        },
                        .variable,
                        .func,
                        .funcInit => {},
                    }
                }
            }
            // Check for import tasks.
            for (self.importTasks.items) |task| {
                try self.performImportTask(task);
            }
            self.importTasks.clearRetainingCapacity();
            if (id == self.chunks.items.len) {
                // No more chunks were added from import tasks.
                break;
            }
        }

        // Declare static vars and funcs after types have been resolved.
        id = 0;
        while (id < self.chunks.items.len) : (id += 1) {
            const chunk = &self.chunks.items[id];

            // Import core module into local namespace.
            const modId = try sema.getOrLoadModule(chunk, "core", cy.NullId);
            try sema.importAllFromModule(chunk, modId);

            // Process static declarations.
            for (chunk.parser.staticDecls.items) |decl| {
                switch (decl.declT) {
                    .variable => {
                        try sema.declareVar(chunk, decl.inner.variable);
                    },
                    .func => {
                        try sema.declareFunc(chunk, decl.inner.func);
                    },
                    .funcInit => {
                        try sema.declareFuncInit(chunk, decl.inner.funcInit);
                    },
                    .object => {
                        try sema.declareObjectMembers(chunk, decl.inner.object);
                    },
                    .enumT,
                    .import,
                    .typeAlias => {},
                }
            }
        }

        // Perform sema on all chunks.
        // TODO: Single pass sema/codegen.
        id = 0;
        while (id < self.chunks.items.len) : (id += 1) {
            try self.performChunkSema(id);
        }
        
        // Set up for genVarDecls.
        // All main blocks should be initialized since genVarDecls can alternate chunks.
        for (self.chunks.items) |*chunk| {
            try chunk.pushSemaBlock(chunk.mainSemaBlockId);
            chunk.buf = &self.buf;
            // Temp locals can start at 0 for initializers codegen.
            chunk.curBlock.numLocals = 0;
        }

        if (!config.skipCodegen) {
            // Once all symbols have been resolved, the static initializers are generated in DFS order.
            for (self.chunks.items) |*chunk| {
                log.debug("gen static initializer for chunk: {}", .{chunk.id});

                for (chunk.parser.staticDecls.items) |decl| {
                    if (decl.declT == .variable) {
                        const node = chunk.nodes[decl.inner.variable];
                        const rSymId = node.head.staticDecl.sema_rSymId;
                        const crSymId = sema.CompactResolvedSymId.initSymId(rSymId);
                        try gen.genStaticInitializerDFS(chunk, crSymId);
                    } else if (decl.declT == .funcInit) {
                        const node = chunk.nodes[decl.inner.funcInit];
                        const declId = node.head.func.semaDeclId;
                        const func = chunk.semaFuncDecls.items[declId];
                        const crSymId = sema.CompactResolvedSymId.initFuncSymId(func.inner.staticFunc.semaFuncSymId);
                        try gen.genStaticInitializerDFS(chunk, crSymId);
                    }
                }
            }

            for (self.chunks.items, 0..) |*chunk, i| {
                log.debug("perform codegen for chunk: {}", .{i});
                try self.performChunkCodegen(chunk.id);
            }

            // Merge inst and const buffers.
            var reqLen = self.buf.ops.items.len + self.buf.consts.items.len * @sizeOf(cy.Const) + @alignOf(cy.Const) - 1;
            if (self.buf.ops.capacity < reqLen) {
                try self.buf.ops.ensureTotalCapacityPrecise(self.alloc, reqLen);
            }
            const constAddr = std.mem.alignForward(usize, @intFromPtr(self.buf.ops.items.ptr) + self.buf.ops.items.len, @alignOf(cy.Const));
            const constDst = @as([*]cy.Const, @ptrFromInt(constAddr))[0..self.buf.consts.items.len];
            const constSrc = try self.buf.consts.toOwnedSlice(self.alloc);
            std.mem.copy(cy.Const, constDst, constSrc);
            self.alloc.free(constSrc);
            self.buf.mconsts = constDst;
        }

        // Final op address is known. Patch pc offsets.
        // for (self.vm.funcSyms.items()) |*sym| {
        //     if (sym.entryT == .func) {
        //         sym.inner.func.pc = .{ .ptr = self.buf.ops.items.ptr + sym.inner.func.pc.offset};
        //     }
        // }
        // for (self.vm.methodSyms.items()) |*sym| {
        //     if (sym.mapT == .one) {
        //         if (sym.inner.one.sym.entryT == .func) {
        //             sym.inner.one.sym.inner.func.pc = .{ .ptr = self.buf.ops.items.ptr + sym.inner.one.sym.inner.func.pc.offset };
        //         }
        //     } else if (sym.mapT == .many) {
        //         if (sym.inner.many.mruSym.entryT == .func) {
        //             sym.inner.many.mruSym.inner.func.pc = .{ .ptr = self.buf.ops.items.ptr + sym.inner.many.mruSym.inner.func.pc.offset };
        //         }
        //     }
        // }
        // var iter = self.vm.methodTable.iterator();
        // while (iter.next()) |entry| {
        //     const sym = entry.value_ptr;
        //     if (sym.entryT == .func) {
        //         sym.inner.func.pc = .{ .ptr = self.buf.ops.items.ptr + sym.inner.func.pc.offset };
        //     }
        // }
    }

    /// Sema pass.
    /// Symbol resolving, type checking, and builds the model for codegen.
    fn performChunkSema(self: *VMcompiler, id: cy.ChunkId) !void {
        const chunk = &self.chunks.items[id];

        // Dummy first element to avoid len > 0 check during pop.
        try chunk.semaSubBlocks.append(self.alloc, sema.SubBlock.init(0, 0, 0));
        try chunk.semaBlockStack.append(self.alloc, 0);

        const root = chunk.nodes[chunk.parserAstRootId];

        chunk.mainSemaBlockId = try sema.pushBlock(chunk, cy.NullId);
        chunk.nodeTypes = try self.alloc.alloc(sema.ResolvedSymId, chunk.nodes.len);
        sema.semaStmts(chunk, root.head.root.headStmt) catch |err| {
            try sema.endBlock(chunk);
            return err;
        };
        try sema.endBlock(chunk);
    }

    /// Tokenize and parse.
    /// Parser pass collects static declaration info.
    fn performChunkParse(self: *VMcompiler, id: cy.ChunkId) !void {
        const chunk = &self.chunks.items[id];

        var tt = stdx.debug.trace();
        const ast = try chunk.parser.parse(chunk.src);
        tt.endPrint("parse");
        // Update buffer pointers so success/error paths can access them.
        chunk.nodes = ast.nodes.items;
        chunk.tokens = ast.tokens;
        if (ast.has_error) {
            self.lastErrChunk = id;
            if (ast.isTokenError) {
                return error.TokenError;
            } else {
                return error.ParseError;
            }
        }
        chunk.parserAstRootId = ast.root_id;
    }

    fn performChunkCodegen(self: *VMcompiler, id: cy.ChunkId) !void {
        const chunk = &self.chunks.items[id];

        if (id == 0) {
            // Main script performs gen for decls and the main block.
            try gen.initVarLocals(chunk);
            const jumpStackStart = chunk.blockJumpStack.items.len;
            const root = chunk.nodes[0];
            try gen.genStatements(chunk, root.head.root.headStmt, true);
            chunk.patchBlockJumps(jumpStackStart);
            chunk.blockJumpStack.items.len = jumpStackStart;
            self.buf.mainStackSize = chunk.getMaxUsedRegisters();
            chunk.popBlock();
        } else {
            // Modules perform gen for only the top level declarations.
            const root = chunk.nodes[0];
            try gen.topDeclStatements(chunk, root.head.root.headStmt);
            chunk.popBlock();
        }
    }

    fn performImportTask(self: *VMcompiler, task: ImportTask) !void {
        if (task.builtin) {
            if (self.moduleLoaders.get(task.absSpec)) |loaders| {
                for (loaders.items) |loader| {
                    if (!loader(@ptrCast(self.vm), task.modId)) {
                        return error.LoadModuleError;
                    }
                }
            } else {
                const chunk = &self.chunks.items[task.chunkId];
                return chunk.reportErrorAt("Unsupported builtin. {}", &.{fmt.v(task.absSpec)}, task.nodeId);
            }
        } else {
            // Default loader.

            if (cy.isWasm) {
                return error.Unsupported;
            }

            var src: []const u8 = undefined;
            if (std.mem.startsWith(u8, task.absSpec, "http://") or std.mem.startsWith(u8, task.absSpec, "https://")) {
                src = try self.importUrl(task);
            } else {
                src = try std.fs.cwd().readFileAlloc(self.alloc, task.absSpec, 1e10);
            }

            // Push another chunk.
            const newChunkId: u32 = @intCast(self.chunks.items.len);
            var newChunk = try cy.Chunk.init(self, newChunkId, task.absSpec, src);
            newChunk.srcOwned = true;
            newChunk.modId = task.modId;

            try self.chunks.append(self.alloc, newChunk);
            self.sema.modules.items[task.modId].chunkId = newChunkId;
        }
    }

    pub fn addModuleLoader(self: *VMcompiler, absSpec: []const u8, func: cy.ModuleLoaderFunc) !void {
        const res = try self.moduleLoaders.getOrPut(self.alloc, absSpec);
        if (res.found_existing) {
            const list = res.value_ptr;
            try list.append(self.alloc, func);
        } else {
            const keyDupe = try self.alloc.dupe(u8, absSpec);
            // Start with initial cap = 1.
            res.value_ptr.* = try std.ArrayListUnmanaged(cy.ModuleLoaderFunc).initCapacity(self.alloc, 1);
            res.key_ptr.* = keyDupe;
            const list = res.value_ptr;
            list.items.len = 1;
            list.items[0] = func;
        }
    }

    fn importUrl(self: *VMcompiler, task: ImportTask) ![]const u8 {
        const specGroup = try cache.getSpecHashGroup(self.alloc, task.absSpec);
        defer specGroup.deinit(self.alloc);

        if (self.vm.config.reload) {
            // Remove cache entry.
            try specGroup.markEntryBySpecForRemoval(task.absSpec);
        } else {
            // First check local cache.
            if (try specGroup.findEntryBySpec(task.absSpec)) |entry| {
                var found = true;
                const src = cache.allocSpecFileContents(self.alloc, entry) catch |err| b: {
                    if (err == error.FileNotFound) {
                        // Fallthrough.
                        found = false;
                        break :b "";
                    } else {
                        return err;
                    }
                };
                if (found) {
                    log.debug("Using cached {s}", .{task.absSpec});
                    return src;
                }
            }
        }

        const client = self.vm.httpClient;

        const uri = try std.Uri.parse(task.absSpec);
        var req = client.request(.GET, uri, .{ .allocator = self.alloc }) catch |err| {
            if (err == error.UnknownHostName) {
                const chunk = &self.chunks.items[task.chunkId];
                const stmt = chunk.nodes[task.nodeId];
                return chunk.reportErrorAt("Can not connect to `{}`.", &.{v(uri.host.?)}, stmt.head.left_right.right);
            } else {
                return err;
            }
        };
        defer client.deinitRequest(&req);

        try client.startRequest(&req);
        try client.waitRequest(&req);

        switch (req.response.status) {
            .ok => {
                // Whitelisted status codes.
            },
            else => {
                // Stop immediately.
                const chunk = &self.chunks.items[task.chunkId];
                const stmt = chunk.nodes[task.nodeId];
                return chunk.reportErrorAt("Can not load `{}`. Response code: {}", &.{v(task.absSpec), v(req.response.status)}, stmt.head.left_right.right);
            },
        }

        var buf: std.ArrayListUnmanaged(u8) = .{};
        errdefer buf.deinit(self.alloc);
        var readBuf: [4096]u8 = undefined;
        var read: usize = readBuf.len;

        while (read == readBuf.len) {
            read = try client.readAll(&req, &readBuf);
            try buf.appendSlice(self.alloc, readBuf[0..read]);
        }

        // Cache to local.
        const entry = try cache.saveNewSpecFile(self.alloc, specGroup, task.absSpec, buf.items);
        entry.deinit(self.alloc);

        return try buf.toOwnedSlice(self.alloc);
    }
};

pub const CompileErrorType = enum {
    tokenize,
    parse,
    compile,
};

pub const CompileResultView = struct {
    buf: cy.ByteCodeBuffer,
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

const unexpected = stdx.fatal;

const ImportTask = struct {
    chunkId: cy.ChunkId,
    nodeId: cy.NodeId,
    absSpec: []const u8,
    modId: cy.ModuleId,
    builtin: bool,
};

pub fn initModuleCompat(comptime name: []const u8, comptime initFn: fn (vm: *VMcompiler, modId: cy.ModuleId) anyerror!void) cy.ModuleLoaderFunc {
    return struct {
        fn initCompat(vm: *cy.UserVM, modId: cy.ModuleId) bool {
            initFn(&vm.internal().compiler, modId) catch |err| {
                log.debug("Init module `{s}` failed: {}", .{name, err});
                return false;
            };
            return true;
        }
    }.initCompat;
}

pub const CompileConfig = struct {
    skipCodegen: bool = false,
    enableFileModules: bool = false,
};

pub const ValidateConfig = struct {
    enableFileModules: bool = false,
};