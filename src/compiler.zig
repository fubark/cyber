const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const rt = cy.rt;
const ir = cy.ir;
const fmt = @import("fmt.zig");
const v = fmt.v;
const vmc = @import("vmc");
const sema = cy.sema;
const sema_type = cy.sema_type;
const bt = cy.types.BuiltinTypes;
const core_mod = @import("builtins/core.zig");
const meta_mod = @import("builtins/meta.zig");
const cy_mod = @import("builtins/cy.zig");
const math_mod = @import("std/math.zig");
const c_mod = @import("builtins/c.zig");
const io_mod = @import("std/io.zig");
const test_mod = @import("std/test.zig");
const llvm_gen = @import("llvm_gen.zig");
const cgen = cy.cgen;
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

pub const dumpCompileErrorStackTrace = !cy.isFreestanding and builtin.mode == .Debug and !cy.isWasm and true;

const Root = @This();

pub var debug = false;

pub const Compiler = struct {
    alloc: std.mem.Allocator,
    vm: *cy.VM,

    // For BC codegen.
    debug_tables: std.ArrayListUnmanaged(cy.DebugTableEntry),

    jitBuf: jitgen.CodeBuffer,

    // Compile-time heap.
    heap: cy.Heap,

    reports: std.ArrayListUnmanaged(Report),
    
    /// Sema model resulting from the sema pass.
    sema: sema.Sema,

    /// Determines how modules are loaded.
    loader: C.ModuleLoaderFn,

    /// Determines how module uris are resolved.
    resolver: C.ResolverFn,

    dl_resolver: C.DlResolverFn,

    /// Compilation units for iteration.
    chunks: std.ArrayListUnmanaged(*cy.Chunk),

    /// Unowned reference to the builtins chunk.
    builtins_chunk: *cy.Chunk,

    /// Resolved URI to chunk.
    chunk_map: std.StringHashMapUnmanaged(*cy.Chunk),

    /// Key is either a *Sym, *Func, or *cy.types.FuncPtr (for extern func ptrs).
    genSymMap: std.AutoHashMapUnmanaged(*anyopaque, bcgen.Sym),

    /// Key to VM.vtables index.
    gen_vtables: std.AutoHashMapUnmanaged(bcgen.VtableKey, u32),

    config: C.CompileConfig,

    /// Tracks whether an error was set from the API.
    hasApiError: bool,
    apiError: []const u8, // Duped so Cyber owns the msg.

    /// Whether core should be imported.
    importCore: bool = true,

    main_chunk: *cy.Chunk,
    main_func: ?*cy.Func,

    program_ir: *ir.Stmt,

    /// A chunk is reserved for the libcyber API.
    api_chunk: *cy.Chunk,

    find_type_cache: std.StringHashMapUnmanaged(*cy.Type),

    /// Whether this is a subsequent compilation reusing the same state.
    cont: bool,

    chunk_start: u32,
    type_start: u32,

    c_includes: std.ArrayListUnmanaged([]const u8),
    c_flags: std.ArrayListUnmanaged([]const u8),

    build_flags: std.StringHashMapUnmanaged(void),
    build_options: std.StringHashMapUnmanaged([]const u8),

    /// Useful for custom panic handler to indicate whether it failed at compile-time or runtime.
    compiling: bool,

    pub fn init(self: *Compiler, vm: *cy.VM) !void {
        self.* = .{
            .alloc = vm.alloc,
            .vm = vm,
            .heap = try cy.Heap.init(vm, null, vm.alloc, undefined),
            .jitBuf = jitgen.CodeBuffer.init(),
            .reports = .{},
            .sema = try sema.Sema.init(vm.alloc, self),
            .loader = default_loader,
            .resolver = default_resolver,
            .dl_resolver = default_dl_resolver,
            .chunks = .{},
            .builtins_chunk = undefined,
            .chunk_map = .{},
            .genSymMap = .{},
            .gen_vtables = .{},
            .config = C.defaultCompileConfig(), 
            .hasApiError = false,
            .apiError = "",
            .main_func = null,
            .main_chunk = undefined,
            .program_ir = undefined,
            .api_chunk = undefined,
            .cont = false,
            .chunk_start = 0,
            .type_start = 0,
            .find_type_cache = .{},
            .c_includes = .{},
            .c_flags = .{},
            .compiling = false,
            .debug_tables = .{},
            .build_flags = .{},
            .build_options = .{},
        };
        self.heap.sema = &self.sema;

        self.api_chunk = try createModuleUnmanaged(self, "api", "");
        self.api_chunk.resolve_name_fn = sema.apiResolveName;

        try self.reinitPerRun();    
    }

    pub fn deinit(self: *Compiler, comptime reset: bool) void {
        self.vm.log_tracev("Deinit compiler reset={}", .{reset});
        self.clearReports();
        if (!reset) {
            self.reports.deinit(self.alloc);
        }

        if (reset) {
            self.jitBuf.clear();
        } else {
            self.jitBuf.deinit(self.alloc);
        }

        if (reset) {
            self.debug_tables.clearRetainingCapacity();
        } else {
            self.debug_tables.deinit(self.alloc);
        }

        for (self.chunks.items) |chunk| {
            chunk.deinitCtValues();
        }
        for (self.chunks.items) |chunk| {
            log.tracev("Deinit chunk `{s}`", .{chunk.srcUri});
            if (chunk.on_destroy) |on_destroy| {
                on_destroy(@ptrCast(self.vm), @ptrCast(chunk.sym));
            }
            chunk.deinit();
        }
        if (!reset) {
            self.api_chunk.deinit();
            self.alloc.free(self.api_chunk.src);
            self.alloc.destroy(self.api_chunk);
        } else {
            self.api_chunk.fallback_modules.clearRetainingCapacity();
        }

        // Free chunk sources afterwards since syms may depend on them for debugging (type names).
        for (self.chunks.items) |chunk| {
            if (chunk.manage_src) {
                chunk.alloc.free(chunk.src);
            }
            self.alloc.destroy(chunk);
        }

        {
            var iter = self.find_type_cache.keyIterator();
            while (iter.next()) |spec| {
                self.alloc.free(spec.*);
            }
        }
        for (self.c_includes.items) |import| {
            self.alloc.free(import);
        }
        for (self.c_flags.items) |flag| {
            self.alloc.free(flag);
        }
        {
            var iter = self.build_flags.keyIterator();
            while (iter.next()) |key| {
                self.alloc.free(key.*);
            }
        }
        {
            var iter = self.build_options.iterator();
            while (iter.next()) |entry| {
                self.alloc.free(entry.key_ptr.*);
                self.alloc.free(entry.value_ptr.*);
            }
        }
        if (reset) {
            self.chunks.clearRetainingCapacity();
            self.chunk_map.clearRetainingCapacity();
            self.genSymMap.clearRetainingCapacity();
            self.gen_vtables.clearRetainingCapacity();
            self.find_type_cache.clearRetainingCapacity();
            self.c_includes.clearRetainingCapacity();
            self.c_flags.clearRetainingCapacity();
            self.build_options.clearRetainingCapacity();
        } else {
            self.chunks.deinit(self.alloc);
            self.chunk_map.deinit(self.alloc);
            self.genSymMap.deinit(self.alloc);
            self.gen_vtables.deinit(self.alloc);
            self.find_type_cache.deinit(self.alloc);
            self.c_includes.deinit(self.alloc);
            self.c_flags.deinit(self.alloc);
            self.build_flags.clearRetainingCapacity();
            self.build_options.clearRetainingCapacity();
        }

        // Chunks depends on modules.
        self.sema.deinit(self.alloc, reset);

        self.heap.deinit(reset);

        self.alloc.free(self.apiError);
        self.apiError = "";
    }

    pub fn reinitPerRun(self: *Compiler) !void {
        self.clearReports();
    }

    pub fn clearReports(self: *Compiler) void {
        for (self.reports.items) |*report| {
            report.deinit(self.alloc);
        }
        self.reports.clearRetainingCapacity();
    }

    pub fn newTypes(self: *Compiler) []*cy.Type {
        return self.sema.types.items[self.type_start..];
    }

    pub fn newChunks(self: *Compiler) []*cy.Chunk {
        return self.chunks.items[self.chunk_start..];
    }

    pub fn compile(self: *Compiler, uri: []const u8, src: ?[]const u8, config: C.CompileConfig) !CompileResult {
        self.compiling = true;
        defer self.compiling = false;
        
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
                _ = try self.main_chunk.addReportFmt("Error: {}", &.{v(err)}, null);
                return error.CompileError;
            } else {
                _ = try self.addReportFmt(.compile_err, "Error: {}", &.{v(err)}, null, 0);
                return error.CompileError;
            }
        };

        // Successful.
        self.cont = true;
        return res;
    }

    /// Wrap compile so all errors can be handled in one place.
    fn compileInner(self: *Compiler, uri: []const u8, src_opt: ?[]const u8, config: C.CompileConfig) !CompileResult {
        self.config = config;

        if (!self.cont) {
            try reserveCoreTypes(self);
        }

        // TODO: Types and symbols should be loaded recursively for single-threaded.
        //       Separate into two passes one for types and function signatures, and
        //       another for function bodies.

        const prev_main = self.main_chunk;
        if (self.cont) {
            // Evict the main chunk from cache so a new one can be created.
            _ = self.chunk_map.remove(prev_main.srcUri);
        }

        var mainChunk: *cy.Chunk = undefined;
        var buf: [4096]u8 = undefined;
        const r_uri = try resolveModuleUri(self, &buf, uri);
        if (src_opt) |src_temp| {
            const src_dup = try self.alloc.dupe(u8, src_temp);
            mainChunk = try ensureModule2(self, r_uri, src_dup);
        } else {
            mainChunk = try ensureModule(self, r_uri);
        }
        self.main_chunk = mainChunk;

        // Load core module. Added as fallback module for each user module.
        if (self.importCore) {
            if (!self.cont) {
                const core_uri = try cy.compiler.resolveModuleUriFrom(mainChunk, &buf, "core", null);
                self.builtins_chunk = try ensureModule(self, core_uri);
            }
            try self.api_chunk.fallback_modules.append(self.alloc, self.builtins_chunk.sym.sym());
        }

        if (self.cont) {
            if (config.persist_main) {
                // Use all *resolved* top level syms from previous main.
                // If a symbol wasn't resolved, it either failed during compilation or didn't end up getting used.
                var iter = prev_main.sym.getMod().symMap.iterator();
                while (iter.next()) |e| {
                    const name = e.key_ptr.*;
                    const sym = e.value_ptr.*;

                    const resolved = switch (sym.type) {
                        .type => b: {
                            const type_sym = sym.cast(.type);
                            break :b type_sym.type.isResolved();
                        },
                        .func => b: {
                            if (std.mem.eql(u8, sym.name(), "@main_init")) {
                                continue;
                            }
                            if (std.mem.eql(u8, sym.name(), "@main_deinit")) {
                                continue;
                            }
                            break :b sym.cast(.func).isResolved();
                        },
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
            }
        }

        // Resolve core first.
        if (!self.cont) {
            try loadBuiltinChunk(self.builtins_chunk);
            try resolveChunkSyms(self.builtins_chunk);
        }

        var id: u32 = self.chunk_start;
        while (id < self.chunks.items.len) : (id += 1) {
            const chunk = self.chunks.items[id];
            if (chunk == self.builtins_chunk) {
                continue;
            }
            try loadChunk(chunk);
            try resolveChunkSyms(chunk);
        }

        if (self.main_chunk.sym.getMod().getSym("main")) |main_func| {
            const sym = main_func.cast(.func);
            if (sym.numFuncs > 1) {
                return self.main_chunk.reportError("Expected only one `main` function.", sym.first.decl);
            }
            if (sym.first.sig.params().len > 0) {
                return self.main_chunk.reportError("Expected no parameters for `main` function.", sym.first.decl);
            }
            self.main_func = sym.first;
        }

        const program_init = try reserve_hidden_func(mainChunk, "@program_init");
        const program_deinit = try reserve_hidden_func(mainChunk, "@program_deinit");

        log.tracev("emit @program_init.", .{});
        try emit_program_init_func(mainChunk, program_init);
        log.tracev("emit @program_deinit.", .{});
        try emit_program_deinit_func(mainChunk, program_deinit);

        // Generate main first to expand any templates.
        log.tracev("emit program.", .{});
        self.program_ir = try sema.sema_program(mainChunk, self.main_func);

        // Perform sema on all chunks.
        log.tracev("Perform sema.", .{});
        for (self.newChunks()) |chunk| {
            performChunkSema(chunk) catch |err| {
                if (err == error.CompileError) {
                    return err;
                } else {
                    // Wrap all other errors as a CompileError.
                    _ = try chunk.addReportFmt("error.{}", &.{v(err)}, chunk.curNode);
                    return error.CompileError;
                }
                return err;
            };
        }

        // Pass through type syms.
        for (self.newTypes()) |type_| {
            const chunk = self.getSymChunk(&type_.sym().head);
            switch (type_.kind()) {
                .struct_t => {
                    const struct_t = type_.cast(.struct_t);
                    const impls = struct_t.impls();
                    if (impls.len > 0) { 
                        const sym = type_.sym();

                        try sema.pushSymResolveContext(chunk, &sym.head, sym.decl.?);
                        defer sema.popResolveContext(chunk);

                        for (impls, 0..) |*impl, j| {
                            const trait_members = impl.trait.members();
                            const funcs = try self.alloc.alloc(*cy.Func, trait_members.len);
                            errdefer self.alloc.free(funcs);

                            for (trait_members, 0..) |member, i| {
                                const func = type_.get_trait_method_impl(member) orelse {
                                    const sig_str = try self.sema.allocFuncSigStr(member.func.sig, true, chunk);
                                    defer self.alloc.free(sig_str);
                                    const type_name = try self.sema.allocTypeName(type_);
                                    defer self.alloc.free(type_name);
                                    return chunk.reportErrorFmt("`{}` does not implement `fn {}{}` from `{}`.", &.{
                                        v(type_name), v(member.func.name()), v(sig_str), 
                                        v(impl.trait.base.name()) }, @ptrCast(sym.decl.?.cast(.struct_decl).impls.ptr[j].trait));
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
                    const enable_dce = false;
                    if (enable_dce) {
                        try cy.dce.performDCE(self);
                    }
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
                        .vm = &self.main_chunk.buf,
                    };
                },
                else => return error.Unsupported,
            }
            log.tracev("Done. Perform codegen.", .{});
        }

        return CompileResult{ .vm = undefined };
    }

    pub fn addReportFmt(self: *Compiler, report_t: ReportType, format: []const u8, args: []const fmt.FmtValue, chunk: ?cy.ChunkId, loc: u64) !usize {
        const msg = try fmt.allocFormat(self.alloc, format, args);
        return self.addReportConsume(report_t, msg, chunk, loc);
    }

    /// Assumes `msg` is heap allocated.
    pub fn addReportConsume(self: *Compiler, report_t: ReportType, msg: []const u8, chunk: ?cy.ChunkId, loc: u64) !usize {
        const id = self.reports.items.len;
        try self.reports.append(self.alloc, .{
            .type = report_t,
            .chunk = chunk orelse cy.NullId,
            .loc = loc,
            .msg = msg,
        });
        return id;
    }

    pub fn addReport(self: *Compiler, report_t: ReportType, msg: []const u8, chunk: ?cy.ChunkId, loc: u64) !usize {
        const dupe = try self.alloc.dupe(u8, msg);
        return self.addReportConsume(report_t, dupe, chunk, loc);
    }

    pub fn getSymChunk(self: *Compiler, sym: *cy.Sym) *cy.Chunk {
        return self.chunks.items[sym.declNode().src()];
    }
};

fn resolveCopyCtor(c: *Compiler, type_: *cy.Type, tmpl: *cy.sym.FuncTemplate) !void {
    const func = try resolveFuncTemplate(c, type_, tmpl);
    type_.copy_ctor = func;
}

fn resolveFuncTemplate(c: *Compiler, type_: *cy.Type, tmpl: *cy.sym.FuncTemplate) !*cy.Func {
    const param = cy.Value.initType(type_);
    const func = try cy.generics.expandFuncTemplate(c.builtins_chunk, tmpl, &.{c.sema.type_t}, &.{param});
    try sema.resolveFunc(c.builtins_chunk, func);
    try sema.semaFuncBody(c.builtins_chunk, func);
    return func;
}

const ReportType = enum(u8) {
    token_err,
    parse_err,
    compile_err,

    /// Additional context trace.
    context,
};

pub const Report = struct {
    type: ReportType,
    msg: []const u8,

    /// If NullId, then the report comes from an aggregate step.
    chunk: cy.ChunkId,

    /// srcPos if token_err or parse_err
    /// *ast.Node if compile_err
    /// If NullId, then the report does not have a location.
    loc: u64,

    /// Backtrace.
    next: ?*Report = null,

    pub fn deinit(self: *Report, alloc: std.mem.Allocator) void {
        if (self.next) |next| {
            next.deinit(alloc);
            alloc.destroy(next);
        }
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
pub fn performChunkParse(self: *Compiler, chunk: *cy.Chunk) !void {
    _ = self;
    var tt = cy.debug.timer();

    const S = struct {
        fn parserReport(p: *cy.Parser, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror {
            const c: *cy.Chunk = @ptrCast(@alignCast(p.ctx));
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

    const res = try chunk.parser.parse(chunk.src, chunk.id, .{});
    tt.endPrintVerbose("parse");
    // Update buffer pointers so success/error paths can access them.
    chunk.updateAstView(res.ast);
    if (res.has_error) {
        return error.CompileError;
    }
}

/// Sema pass.
/// Symbol resolving, type checking, and builds the model for codegen.
fn performChunkSema(c: *cy.Chunk) !void {
    // try sema.pushChunkResolveContext(chunk, null);
    // defer sema.popResolveContext(chunk);

    if (c.has_static_init) {
        const init = c.sym.getMod().getSym("@init").?.cast(.func).first;
        try emit_chunk_init_func(c, init);

        const deinit = c.sym.getMod().getSym("@deinit").?.cast(.func).first;
        try emit_chunk_deinit_func(c, deinit);
    }

    try semaChunkFuncs(c);
}

fn semaChunkFuncs(c: *cy.Chunk) !void {
    // Iterate funcs with explicit index since lambdas could be appended.
    var i: u32 = 0;
    while (i < c.funcs.items.len) : (i += 1) {
        const func = c.funcs.items[i];
        switch (func.type) {
            .userFunc => {
                // Skip strictly const eval functions.
                if (func.info.const_eval_only) {
                    continue;
                }

                // Skip already emitted functions such as `@init`.
                if (func.info.emitted) {
                    continue;
                }
                log.tracev("sema func: {s}", .{func.name()});
                if (func.isMethod()) {
                    try sema.semaMethodBody(c, func);
                } else {
                    try sema.semaFuncBody(c, func);
                }
            },
            .extern_ => {
                if (!func.data.extern_.has_impl) {
                    continue;
                }
                if (func.info.emitted) {
                    continue;
                }
                try sema.semaFuncBody(c, func);
            },
            else => {},
        }
    }
}

fn emit_chunk_init_func(c: *cy.Chunk, init: *cy.Func) !void {
    log.tracev("emit @init.", .{});

    try sema.pushChunkResolveContext(c, null);
    defer sema.popResolveContext(c);

    _ = try sema.pushFuncProc(c, init);

    // Emit in correct dependency order.
    for (0..c.syms.items.len) |i| {
        const sym = c.syms.items[i];
        switch (sym.type) {
            .hostVar => {
                try sema.semaTraceNopFmt(c, "init {s}", .{sym.name()}, init.decl.?);
            },
            .userVar => {
                try sema.semaTraceNopFmt(c, "init {s}", .{sym.name()}, init.decl.?);
                const user_var = sym.cast(.userVar);
                try sema.semaUserVarInit(c, user_var, null);
            },
            else => {},
        }
    }

    try sema.procPrologue(c);
    try sema.popFuncBlock(c);
}

/// Invoke any chunk init functions.
/// Invoke @initBindLib if necessary.
fn emit_program_init_func(c: *cy.Chunk, start: *cy.Func) !void {
    try sema.pushChunkResolveContext(c, null);
    defer sema.popResolveContext(c);

    _ = try sema.pushFuncProc(c, start);

    var has_extern_func = false;
    for (c.compiler.newChunks()) |chunk| {
        if (chunk.has_extern_func) {
            has_extern_func = true;
        }
        if (!chunk.has_static_init) {
            continue;
        }

        const init = try reserve_hidden_func(chunk, "@init");

        const call = try sema.sema_call_ir(c, init, &.{}, c.ast.null_node);
        try sema.sema_discard(c, call, c.ast.null_node);
    }

    if (has_extern_func) {
        const init_bind_lib = (try c.get_chunk_by_uri("c")).?.sym.getMod().getSym("@initBindLib").?.cast(.func).first;
        const expr = try sema.sema_call_ir(c, init_bind_lib, &.{}, c.ast.null_node);
        try sema.sema_discard(c, expr, c.ast.null_node);
    }

    try sema.procPrologue(c);
    try sema.popFuncBlock(c);
}

fn reserve_hidden_func(c: *cy.Chunk, name: []const u8) !*cy.Func {
    const name_n = try c.parser.ast.genIdentNode(c.alloc, name);
    const decl = try c.parser.ast.newNode(.funcDecl, .{
        .name = @ptrCast(name_n),
        .parent = null,
        .attrs = .{ .ptr = undefined, .len = 0 },
        .with = null,
        .params = .{ .ptr = undefined, .len = 0 },
        .ret = null,
        .stmts = .{ .ptr = undefined, .len = 0 },
        .pos = cy.NullId,
        .sig_t = .func,
        .hidden = true,
        .scope_ret = false,
    });
    c.updateAstView(c.parser.ast.view());

    const config = cy.sym.FuncConfig{
        .is_method = false,
        .extern_ = false,
    };
    const func = try c.reserveUserFunc(@ptrCast(c.sym), name, config, decl);

    const sig_id = try c.sema.ensureFuncSig(&.{}, c.sema.void_t);
    try c.resolveUserFunc(func, sig_id);

    // Mark as emitted so `performChunkSema` doesn't try to emit the body.
    func.info.emitted = true;
    
    return func;
}

// Chunk destructor.
fn emit_chunk_deinit_func(c: *cy.Chunk, deinit_fn: *cy.Func) !void {
    try sema.pushChunkResolveContext(c, null);
    defer sema.popResolveContext(c);

    _ = try sema.pushFuncProc(c, deinit_fn);

    for (0..c.syms.items.len) |i| {
        const sym = c.syms.items[i];
        switch (sym.type) {
            .hostVar => {
                // Binded globals are managed by the host. No destruction needed.
                const host_var = sym.cast(.hostVar);
                const node: *ast.Node = @ptrCast(host_var.decl.?);
                try sema.semaTraceNopFmt(c, "deinit global {s} -- skip", .{host_var.head.name()}, node);
            },
            .userVar => {
                const user_var = sym.cast(.userVar);
                if (!user_var.type.isManaged()) {
                    continue;
                }

                const node: *ast.Node = @ptrCast(user_var.decl.?);
                try sema.semaNopFmt(c, "deinit global {s}", .{user_var.head.name()}, node);
                const static = try sema.symbol(c, sym, false, node);
                const ptr_t = try sema.getPtrType(c, user_var.type);
                const addr = try sema.semaAddressOf(c, static, ptr_t, node);
                const move_ir = try c.ir.newExpr(.deref, user_var.type, node, .{
                    .expr = addr.ir,
                });
                const move = sema.ExprResult.init2(move_ir);
                const temp_local = try sema.declareHiddenLocal(c, "_temp", user_var.type, move, node);
                const temp = &c.varStack.items[temp_local.id];
                try sema.semaDestructLocal(c, temp, node);
                c.varStack.items[temp_local.id].type = .dead;
            },
            else => {},
        }
    }

    try sema.procPrologue(c);
    try sema.popFuncBlock(c);
}

fn emit_program_deinit_func(c: *cy.Chunk, end_fn: *cy.Func) !void {
    try sema.pushChunkResolveContext(c, null);
    defer sema.popResolveContext(c);

    _ = try sema.pushFuncProc(c, end_fn);
    for (c.compiler.newChunks()) |chunk| {
        if (!chunk.has_static_init) {
            continue;
        }

        const deinit = try reserve_hidden_func(chunk, "@deinit");

        // Invoke chunk destructor.
        const call = try sema.sema_call_ir(c, deinit, &.{}, c.ast.null_node);
        try sema.sema_discard(c, call, c.ast.null_node);
    }
    
    try sema.procPrologue(c);
    try sema.popFuncBlock(c);
}

fn resolveTypeSym(c: *cy.Chunk, mod: *cy.Module, name: []const u8) !*cy.Type {
    const sym = mod.getSym(name).?;
    try resolveSym(c, sym);
    return sym.cast(.type).type;
}

fn resolveFirstFunc(c: *cy.Chunk, mod: *cy.Module, name: []const u8) !*cy.Func {
    var sym = mod.getSym(name).?;
    if (sym.type == .use_alias) {
        const alias = sym.cast(.use_alias);
        if (!alias.resolved) {
            try sema.resolveUseAlias(c, alias);
        }
        sym = alias.sym;
    }
    const func = sym.cast(.func).first;
    try sema.ensure_resolved_func(c, func, false, null);
    return func;
}

fn loadBuiltinChunk(c: *cy.Chunk) !void {
    const core_sym = c.sym;

    try loadChunk(c);

    // Extract special syms. Assumes chunks[0] is the builtins chunk.
    const core = c.sym.getMod();
    c.sema.generator_tmpl = core.getSym("Generator").?.cast(.template);
    c.sema.future_tmpl = core.getSym("Future").?.cast(.template);
    c.sema.va_list_tmpl = core.getSym("VaList").?.cast(.template);
    c.sema.option_tmpl = core.getSym("Option").?.cast(.template);
    c.sema.int_tmpl = core.getSym("Int").?.cast(.template);
    c.sema.float_tmpl = core.getSym("Float").?.cast(.template);
    c.sema.fn_tuple_tmpl = core.getSym("FnTuple").?.cast(.template);
    c.sema.result_tmpl = core.getSym("Result").?.cast(.template);
    c.sema.vector_tmpl = core.getSym("Vector").?.cast(.template);
    c.sema.generic_vector_tmpl = core.getSym("GenericVector").?.cast(.template);
    c.sema.partial_vector_tmpl = core.getSym("PartialVector").?.cast(.template);
    c.sema.ptr_tmpl = core.getSym("Ptr").?.cast(.template);
    c.sema.ref_tmpl = core.getSym("Ref").?.cast(.template);
    c.sema.borrow_tmpl = core.getSym("Borrow").?.cast(.template);
    c.sema.ex_borrow_tmpl = core.getSym("ExBorrow").?.cast(.template);
    c.sema.slice_tmpl = core.getSym("Slice").?.cast(.template);
    c.sema.span_tmpl = core.getSym("Span").?.cast(.template);
    c.sema.as_span_tmpl = core.getSym("AsSpan").?.cast(.template);
    c.sema.array_tmpl = core.getSym("Array").?.cast(.template);
    c.sema.map_tmpl = core.getSym("Map").?.cast(.template);
    c.sema.hash_map_tmpl = core.getSym("HashMap").?.cast(.template);
    c.sema.ptr_span_tmpl = core.getSym("PtrSpan").?.cast(.template);
    c.sema.ref_child_tmpl = core.getSym("RefChild").?.cast(.template);
    c.sema.func_sym_tmpl = core.getSym("funcsym_t").?.cast(.template);
    c.sema.func_ptr_tmpl = core.getSym("funcptr_t").?.cast(.template);
    c.sema.func_tmpl = core.getSym("Func").?.cast(.template);
    c.sema.opaque_func_tmpl = core.getSym("OpaqueFunc").?.cast(.template);
    c.sema.raw_buffer_tmpl = core.getSym("RawBuffer").?.cast(.template);
    c.sema.buffer_tmpl = core.getSym("Buffer").?.cast(.template);
    c.sema.eval_buffer_tmpl = core.getSym("EvalBuffer").?.cast(.template);
    c.sema.panic_fn = core.getSym("panic").?.cast(.func).first;
    c.sema.panic_unwrap_error_fn = core.getSym("panic_unwrap_error").?.cast(.func).first;
    c.sema.track_main_local_fn = core.getSym("@trackMainLocal").?.cast(.func).first;

    const float_tmpl = c.sema.float_tmpl;
    const raw_tmpl = core.getSym("Raw").?.cast(.template);

    // Resolve core builtin types first, since they are needed by sema.
    const mod = core_sym.getMod();
    var sym = core.getSym("type").?;
    try resolveSym(c, sym);
    c.sema.type_t = sym.cast(.type_alias).sym.type;
    sym = core.getSym("PartialStructLayout").?;
    try resolveSym(c, sym);
    c.sema.partial_struct_layout_t = sym.cast(.type_alias).sym.type;
    sym = core.getSym("Code").?;
    try resolveSym(c, sym);
    c.sema.code_t = sym.cast(.type_alias).sym.type;
    _ = try resolveTypeSym(c, mod, "NoCopy");
    c.sema.any_t = try resolveTypeSym(c, mod, "Any");
    c.sema.dependent_t = try resolveTypeSym(c, mod, "dependent");
    c.sema.infer_t = try resolveTypeSym(c, mod, "Infer");
    c.sema.eval_int_t = try resolveTypeSym(c, mod, "EvalInt");
    c.sema.eval_str_t = try resolveTypeSym(c, mod, "EvalStr");
    c.sema.str_t = try resolveTypeSym(c, mod, "str");
    c.sema.str_buffer_t = try resolveTypeSym(c, mod, "StrBuffer");
    c.sema.mut_str_t = try resolveTypeSym(c, mod, "Str");
    c.sema.error_t = try resolveTypeSym(c, mod, "error");
    c.sema.object_t = try resolveTypeSym(c, mod, "Object");
    c.sema.table_t = try resolveTypeSym(c, mod, "Table");
    c.sema.range_t = try resolveTypeSym(c, mod, "Range");
    try resolveSym(c, mod.getSym("bool").?);
    c.sema.symbol_t = try resolveTypeSym(c, mod, "symbol");
    try resolveSym(c, mod.getSym("StrBuffer").?);
    // try resolveSym(c, mod.getSym("placeholder1").?);
    try resolveSym(c, mod.getSym("Table").?);
    try resolveSym(c, mod.getSym("Any").?);
    try resolveSym(c, mod.getSym("Code").?);
    c.sema.funcsig_t = try resolveTypeSym(c, mod, "FuncSig");
    // try resolveSym(c, mod.getSym("Table").?);
    try resolveSym(c, @ptrCast(c.sema.option_tmpl));
    try resolveSym(c, @ptrCast(c.sema.ptr_tmpl));
    try resolveSym(c, @ptrCast(c.sema.ptr_span_tmpl));

    c.sema.copy_fn = try resolveFirstFunc(c, mod, "@copy");
    c.sema.dtor_fn = try resolveFirstFunc(c, mod, "@destruct");
    c.sema.partial_dtor_fn = try resolveFirstFunc(c, mod, "@partial_destruct");
    c.sema.deinit_obj_tmpl = core.getSym("@deinitObject").?.cast(.func_template);
    c.sema.to_print_string_fn = try resolveFirstFunc(c, mod, "to_print_string");
    c.sema.eq_fn = try resolveFirstFunc(c, mod, "@eq");

    const meta_c = (try c.get_chunk_by_uri("meta")).?;
    try reserveChunkSyms(meta_c);
    const meta_mod2 = meta_c.sym.getMod();
    c.sema.access_fn = try resolveFirstFunc(c, meta_mod2, "access");

    // int, i32, i16, i8
    var bits = cy.Value.initGenericInt(64);
    const int_tmpl = c.sema.int_tmpl;
    c.sema.i64_t = try c.vm.expandTypeTemplate(int_tmpl, &.{c.sema.eval_int_t}, &.{ bits });
    bits = cy.Value.initGenericInt(32);
    c.sema.i32_t = try c.vm.expandTypeTemplate(int_tmpl, &.{c.sema.eval_int_t}, &.{ bits });
    bits = cy.Value.initGenericInt(16);
    c.sema.i16_t = try c.vm.expandTypeTemplate(int_tmpl, &.{c.sema.eval_int_t}, &.{ bits });
    bits = cy.Value.initGenericInt(8);
    c.sema.i8_t = try c.vm.expandTypeTemplate(int_tmpl, &.{c.sema.eval_int_t}, &.{ bits });

    // r8, r16, r32, r64, rsize
    bits = cy.Value.initGenericInt(8);
    c.sema.r8_t = try c.vm.expandTypeTemplate(raw_tmpl, &.{c.sema.eval_int_t}, &.{ bits });
    bits = cy.Value.initGenericInt(16);
    c.sema.r16_t = try c.vm.expandTypeTemplate(raw_tmpl, &.{c.sema.eval_int_t}, &.{ bits });
    bits = cy.Value.initGenericInt(32);
    c.sema.r32_t = try c.vm.expandTypeTemplate(raw_tmpl, &.{c.sema.eval_int_t}, &.{ bits });
    bits = cy.Value.initGenericInt(64);
    c.sema.r64_t = try c.vm.expandTypeTemplate(raw_tmpl, &.{c.sema.eval_int_t}, &.{ bits });
    sym = core.getSym("rsize").?;
    try resolveSym(c, sym);
    c.sema.rsize_t = sym.cast(.type_alias).sym.type;

    // float, f32
    bits = cy.Value.initGenericInt(64);
    c.sema.f64_t = try c.vm.expandTypeTemplate(float_tmpl, &.{c.sema.eval_int_t}, &.{ bits });
    bits = cy.Value.initGenericInt(32);
    c.sema.f32_t = try c.vm.expandTypeTemplate(float_tmpl, &.{c.sema.eval_int_t}, &.{ bits });

    const int_t = cy.Value.initType(c.sema.i64_t);
    c.sema.option_int_t = try c.vm.expandTypeTemplate(c.sema.option_tmpl, &.{c.sema.type_t}, &.{ int_t });
    c.sema.ptr_int_t = try c.vm.expandTypeTemplate(c.sema.ptr_tmpl, &.{c.sema.type_t}, &.{ int_t });

    const r8_t = cy.Value.initType(c.sema.r8_t);
    c.sema.ptr_span_byte_t = try c.vm.expandTypeTemplate(c.sema.ptr_span_tmpl, &.{c.sema.type_t}, &.{ r8_t });

    const void_t = cy.Value.initType(c.sema.void_t);
    c.sema.ptr_void_t = (try c.vm.expandTypeTemplate(c.sema.ptr_tmpl, &.{c.sema.type_t}, &.{ void_t }));

    c.sema.raw_buffer_byte_t = (try c.vm.expandTypeTemplate(c.sema.raw_buffer_tmpl, &.{c.sema.type_t}, &.{r8_t}));

    // // Verify all builtin types have loaded.
    for (1..cy.types.BuiltinEnd) |i| {
        if (c.sema.types.items[i].kind() == .null) {
            std.debug.panic("Builtin type {} was not initialized.", .{i});
        }
    }

    // Verify builtin types were binded to the state.
    std.debug.assert(c.sema.object_t.id() == bt.Object);
    // std.debug.assert(self.sema.table_t.id() == bt.Table);
    std.debug.assert(c.sema.bool_t.id() == bt.Bool);
    std.debug.assert(c.sema.i8_t.id() == bt.I8);
    std.debug.assert(c.sema.i16_t.id() == bt.I16);
    std.debug.assert(c.sema.i32_t.id() == bt.I32);
    std.debug.assert(c.sema.i64_t.id() == bt.I64);
    std.debug.assert(c.sema.r8_t.id() == bt.R8);
    std.debug.assert(c.sema.r16_t.id() == bt.R16);
    std.debug.assert(c.sema.r32_t.id() == bt.R32);
    std.debug.assert(c.sema.r64_t.id() == bt.R64);
    std.debug.assert(c.sema.f64_t.id() == bt.F64);
    std.debug.assert(c.sema.f32_t.id() == bt.F32);
    std.debug.assert(c.sema.str_t.id() == bt.Str);
    std.debug.assert(c.sema.eval_str_t.id() == bt.EvalStr);
    std.debug.assert(c.sema.str_buffer_t.id() == bt.StrBuffer);
    std.debug.assert(c.sema.void_t.id() == bt.Void);
    std.debug.assert(c.sema.never_t.id() == bt.Never);
    std.debug.assert(c.sema.type_t.id() == bt.Type);
    std.debug.assert(c.sema.error_t.id() == bt.Error);
    std.debug.assert(c.sema.symbol_t.id() == bt.Symbol);
    std.debug.assert(c.sema.range_t.id() == bt.Range);
    std.debug.assert(c.sema.funcsig_t.id() == bt.FuncSig);
    std.debug.assert(c.sema.dependent_t.id() == bt.Dependent);
    std.debug.assert(c.sema.infer_t.id() == bt.Infer);
    std.debug.assert(c.sema.any_t.id() == bt.Any);
    std.debug.assert(c.sema.code_t.id() == bt.Code);
    std.debug.assert(c.sema.mut_str_t.id() == bt.MutStr);
}

pub fn loadChunk(c: *cy.Chunk) !void {
    if (c.loaded) {
        return;
    }
    if (c.pending_load_src) {
        // Initialize defaults.
        c.compiler.hasApiError = false;
        log.tracev("Invoke module loader: {s}", .{c.srcUri});

        var res = C.LoaderResult{
            .src = C.to_bytes(""),
            .manage_src = false,
        };
        if (!c.compiler.loader.?(@ptrCast(c.vm), @ptrCast(c.sym), C.to_bytes(c.srcUri), &res)) {
            if (c.importer) |node| {
                const from = c.compiler.chunks.items[node.src()];
                if (c.compiler.hasApiError) {
                    return from.reportErrorFmt(c.compiler.apiError, &.{}, node);
                } else {
                    return from.reportErrorFmt("Failed to load module: {}", &.{v(c.srcUri)}, node);
                }
            } else {
                if (c.compiler.hasApiError) {
                    return c.reportError(c.compiler.apiError, null);
                } else {
                    return c.reportErrorFmt("Failed to load module: {}", &.{v(c.srcUri)}, null);
                }
            }
        }
        c.src = C.from_bytes(res.src);
        c.manage_src = res.manage_src;
        c.pending_load_src = false;
    }

    log.tracev("chunk parse: {}", .{c.id});
    try performChunkParse(c.compiler, c);
    c.has_pending_ct_stmts = c.parser.has_top_ct_stmt;

    try reserveChunkSyms(c);

    if (c.compiler.importCore) {
        // Add builtins as fallback search module.
        const core_sym = c.compiler.builtins_chunk.sym;
        try c.fallback_modules.append(c.alloc, core_sym.sym());
    }
    c.loaded = true;
}

fn createModuleUnmanaged(self: *Compiler, r_uri: []const u8, src: []const u8) !*cy.Chunk {
    const src_dupe = try self.alloc.dupe(u8, src);
    const r_uri_dupe = try self.alloc.dupe(u8, r_uri);

    const chunk = try self.alloc.create(cy.Chunk);
    try chunk.init(self, cy.NullId, r_uri_dupe, src_dupe);
    chunk.sym = try chunk.createChunkSym(r_uri_dupe);
    return chunk;
}

pub fn ensureModule(self: *Compiler, r_uri: []const u8) !*cy.Chunk {
    return ensureModule2(self, r_uri, null);
}

pub fn ensureModule2(self: *Compiler, r_uri: []const u8, src: ?[]const u8) !*cy.Chunk {
    // Check cache first.
    const cache = try self.chunk_map.getOrPut(self.alloc, r_uri);
    if (cache.found_existing) {
        return cache.value_ptr.*;
    }

    const r_uri_dupe = try self.alloc.dupe(u8, r_uri);
    const chunk_id: u32 = @intCast(self.chunks.items.len);
    const chunk = try self.alloc.create(cy.Chunk);
    try chunk.init(self, chunk_id, r_uri_dupe, src);

    var name = std.fs.path.basename(r_uri_dupe);
    if (std.mem.lastIndexOfScalar(u8, name, '.')) |idx| {
        name = name[0..idx];
    }

    chunk.sym = try chunk.createChunkSym(name);
    try self.chunks.append(self.alloc, chunk);
    cache.key_ptr.* = r_uri_dupe;
    cache.value_ptr.* = chunk;
    return chunk;
}

// /// `src` is consumed.
// pub fn createModule(self: *Compiler, r_uri: []const u8, src: ?[]const u8) !*cy.Chunk {
//     const chunk_id: u32 = @intCast(self.chunks.items.len);
//     const chunk = try self.alloc.create(cy.Chunk);
//     try chunk.init(self, chunk_id, r_uri_dupe, src);
//     chunk.sym = try chunk.createChunkSym(r_uri_dupe);
//     try self.chunks.append(self.alloc, chunk);
//     return chunk;
// }

fn reserveChunkSyms(c: *cy.Chunk) anyerror!void {
    if (c.reserved_syms) {
        return;
    }

    try sema.pushChunkResolveContext(c, null);
    defer sema.popResolveContext(c);

    // Process static declarations.
    for (c.ast.root.?.stmts.slice()) |stmt| {
        switch (stmt.type()) {
            .import_stmt => {
                try sema.declareUseImport(c, stmt.cast(.import_stmt));
            },
            .type_alias_decl => {
                _ = try sema_type.reserve_type_alias(c, stmt.cast(.type_alias_decl));
            },
            .type_const_decl => {
                _ = try sema_type.reserve_type_const(c, stmt.cast(.type_const_decl));
            },
            .use_alias => {
                _ = try sema.reserveUseAlias(c, stmt.cast(.use_alias));
            },
            .cstruct_decl => {
                const decl = stmt.cast(.cstruct_decl);
                _ = try sema_type.reserveStruct(c, decl, true);
            },
            .cunion_decl => {
                const decl = stmt.cast(.cunion_decl);
                _ = try sema_type.reserveCUnion(c, decl);
            },
            .struct_decl => {
                const decl = stmt.cast(.struct_decl);
                _ = try sema_type.reserveStruct(c, decl, false);
            },
            .trait_decl => {
                const decl = stmt.cast(.trait_decl);
                _ = try sema_type.reserve_trait_type(c, decl);
            },
            .enumDecl => {
                const decl = stmt.cast(.enumDecl);
                if (decl.isChoiceType) {
                    _ = try sema_type.reserve_choice(c, stmt.cast(.enumDecl));
                } else {
                    _ = try sema_type.reserve_enum(c, stmt.cast(.enumDecl));
                }
            },
            .custom_type_decl => {
                const decl = stmt.cast(.custom_type_decl);
                const sym = try sema_type.resolve_custom_type(c, decl, null);
                _ = try cy.module.addUniqueSym(c, sym.parent.?.getMod().?, sym.name(), sym, @ptrCast(decl));
            },
            .template => {
                const decl = stmt.cast(.template);
                if (decl.child_decl.type() == .funcDecl) {
                    const func_decl = decl.child_decl.cast(.funcDecl);
                    const decl_path = try sema.ensureDeclNamePath(c, @ptrCast(c.sym), func_decl.parent, func_decl.name);
                    _ = try sema.reserveFuncTemplateSym(c, decl_path.parent, decl_path.name.name, decl);
                } else {
                    _ = try sema.reserveTemplate(c, decl);
                }
            },
            .const_decl => {
                const decl = stmt.cast(.const_decl);
                const decl_path = try sema.ensureDeclNamePath(c, @ptrCast(c.sym), decl.parent, decl.name);
                if (decl_path.variant) {
                    _ = try c.reserveVariantMember(decl_path.parent, decl_path.name.name, @ptrCast(decl));
                    continue;
                }

                _ = try sema.reserveConst(c, decl_path.parent, decl_path.name.name, stmt.cast(.const_decl));
            },
            .global_decl => {
                _ = try sema.reserveGlobal(c, stmt.cast(.global_decl));
                c.has_static_init = true;
            },
            .funcDecl => {
                const decl = stmt.cast(.funcDecl);
                var parent = decl.parent;
                if (parent == null and decl.sig_t == .method) {
                    parent = decl.params.ptr[0].type;
                }
                const decl_path = try sema.ensureDeclNamePath(c, @ptrCast(c.sym), parent, decl.name);
                if (decl_path.variant) {
                    _ = try c.reserveVariantFunc(decl_path.parent, decl_path.name.name, decl);
                    continue;
                } else {
                    _ = try c.reserveFunc(decl_path.parent, decl_path.name.name, decl);
                }
            },
            .ct_stmt => {},
            else => {
                if (c != c.compiler.main_chunk) {
                    return c.reportError("Top level statement is not allowed from imported module.", @ptrCast(stmt));
                }
            },
        }
    }

    c.reserved_syms = true;
}

/// Since core types are declared in the same builtins.cy,
/// Reserve them upfront so that other types do not occupy them first.
fn reserveCoreTypes(self: *Compiler) !void {
    self.vm.log_tracev("Reserve core types", .{});

    const type_ids = &[_]cy.TypeId{
        bt.Void,
        bt.Bool,
        bt.I8,
        bt.I16,
        bt.I32,
        bt.I64,
        bt.EvalInt,
        bt.R8,
        bt.R16,
        bt.R32,
        bt.R64,
        bt.F32,
        bt.F64,
        bt.Error,
        bt.Symbol,
        bt.Object,
        bt.Any,
        bt.Type,
        bt.Thread,
        bt.Code,
        bt.FuncSig,
        bt.PartialStructLayout,
        bt.Str,
        bt.StrBuffer,
        bt.EvalStr,
        bt.Never,
        bt.Infer,
        bt.Dependent,
        bt.TccState,
        bt.Range,
        bt.Table,
        bt.NoCopy,
        bt.MutStr,
    };

    for (type_ids) |type_id| {
        const id = try self.sema.reserveType();
        std.debug.assert(id == type_id);
    }

    std.debug.assert(self.sema.types.items.len == cy.types.BuiltinEnd);
}

fn resolveSym(c: *cy.Chunk, sym: *cy.Sym) !void {
    log.tracev("resolve: {s} {}", .{sym.name(), sym.type});
    switch (sym.type) {
        .extern_var => {
            try sema.resolveExternVar(c, sym.cast(.extern_var));
        },
        .hostVar => {
            try sema.resolveHostVar(c, sym.cast(.hostVar));
        },
        .userVar => {
            try sema.resolveUserVar(c, sym.cast(.userVar));
        },
        .const_ => {
            try sema.resolveConst(c, sym.cast(.const_));
        },
        .func => {},
        .type => {
            const type_sym = sym.cast(.type);
            try sema_type.ensure_resolved_type(c, type_sym.type, type_sym.decl.?);
        },
        .use_alias => {
            try sema.resolveUseAlias(c, @ptrCast(sym));
        },
        .type_alias => {
            try sema_type.resolve_type_alias(c, @ptrCast(sym));
        },
        .type_const => {
            try sema_type.resolve_type_const(c, @ptrCast(sym));
        },
        .template => {
            try sema.ensureResolvedTemplate(c, @ptrCast(sym));
        },
        .func_template => {
            try sema.ensureResolvedFuncTemplate(c, @ptrCast(sym));
        },
        else => {},
    }
}

// Resolves symbol signatures:
// - Variable types are resolved.
// - Function signatures are resolved.
// - Type fields are resolved.
fn resolveChunkSyms(c: *cy.Chunk) !void {
    log.tracev("resolve chunk syms: {s}", .{c.srcUri});

    // Evaluate any top level ct stmts.
    try sema.semaChunkTopCtStmts(c, null);

    var cur_sym: usize = 0;
    var cur_func: usize = 0;
    while (true) {
        // _ = try sema.pushProc(chunk, null);

        // Iterate with explicit index since unnamed types could be appended.
        while (cur_sym < c.syms.items.len) : (cur_sym += 1) {
            const sym = c.syms.items[cur_sym];
            try resolveSym(c, sym);
        }

        // _ = try sema.popProc(chunk);

        while (cur_func < c.funcs.items.len) : (cur_func += 1) {
            const func = c.funcs.items[cur_func];
            log.tracev("resolve: {s}", .{func.name()});
            try sema.ensure_resolved_func(c, func, false, null);
        }

        if (cur_sym == c.syms.items.len) {
            break;
        }
    }

    if (c.on_load) |onLoad| {
        onLoad(@ptrCast(c.vm), @ptrCast(c.sym));
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

pub fn default_dl_resolver(vm: ?*C.VM, uri: C.Bytes, out: [*c]C.Bytes) callconv(.c) bool {
    const final_uri = C.vm_allocb(vm.?, uri.len);
    @memcpy(final_uri, C.from_bytes(uri));
    out[0] = C.to_bytes(final_uri);
    return true;
}

pub fn default_resolver(_: ?*C.VM, params: C.ResolverParams, res_uri_len: ?*usize) callconv(.c) bool {
    @memcpy(params.buf[0..params.uri.len], params.uri.ptr[0..params.uri.len]);
    res_uri_len.?.* = params.uri.len;
    return true;
}

const ModuleEntry = struct {
    src: []const u8,
    bind: *const fn(*C.VM, *C.Sym) callconv(.c) void,
};

const mods = std.StaticStringMap(ModuleEntry).initComptime(.{
    .{"core", ModuleEntry{.src = core_mod.Src, .bind = &core_mod.bind}},
    .{"meta", ModuleEntry{.src = meta_mod.Src, .bind = &meta_mod.bind}},
    .{"math", ModuleEntry{.src = math_mod.Src, .bind = &math_mod.bind}},
    .{"cy",   ModuleEntry{.src = cy_mod.Src, .bind = &cy_mod.bind}},
    .{"c",    ModuleEntry{.src = c_mod.Src, .bind = &c_mod.bind}},
    .{"io",   ModuleEntry{.src = io_mod.Src, .bind = &io_mod.bind}},
    .{"test", ModuleEntry{.src = test_mod.Src, .bind = &test_mod.bind}},
});

pub fn default_loader(vm_: ?*C.VM, mod_: ?*C.Sym, spec: C.Bytes, res: [*c]C.LoaderResult) callconv(.c) bool {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const name = C.from_bytes(spec);
    var src: []const u8 = undefined;
    if (mods.get(name)) |entry| {
        src = vm.alloc.dupe(u8, entry.src) catch @panic("error");
        entry.bind(vm_.?, mod_.?);
    } else {
        return false;
    }
    res[0] = .{
        .src = C.to_bytes(src),
        .manage_src = true,
    };
    return true;
}

pub fn resolveModuleUriFrom(self: *cy.Chunk, buf: []u8, uri: []const u8, node: ?*ast.Node) ![]const u8 {
    self.compiler.hasApiError = false;

    const params: C.ResolverParams = .{
        .chunkId = self.id,
        .curUri = C.to_bytes(self.srcUri),
        .uri = C.to_bytes(uri),
        .buf = buf.ptr,
        .bufLen = buf.len,
    };
    var res_uri_len: usize = undefined;
    if (!self.compiler.resolver.?(@ptrCast(self.compiler.vm), params, &res_uri_len)) {
        if (self.compiler.hasApiError) {
            return self.reportErrorFmt(self.compiler.apiError, &.{}, node);
        } else {
            return self.reportErrorFmt("Failed to resolve module.", &.{}, node);
        }
    }
    return buf[0..res_uri_len];
}

pub fn resolveModuleUri(self: *cy.Compiler, buf: []u8, uri: []const u8) ![]const u8 {
    self.hasApiError = false;

    const params: C.ResolverParams = .{
        .chunkId = cy.NullId,
        .curUri = C.NullStr,
        .uri = C.to_bytes(uri),
        .buf = buf.ptr,
        .bufLen = buf.len,
    };
    var res_uri_len: usize = undefined;
    if (!self.resolver.?(@ptrCast(self.vm), params, &res_uri_len)) {
        if (self.hasApiError) {
            _ = try self.addReport(.compile_err, self.apiError, null, 0);
            return error.CompileError;
        } else {
            _ = try self.addReport(.compile_err, "Failed to resolve module.", null, 0);
            return error.CompileError;
        }
    }
    return params.buf[0..res_uri_len];
}

test "vm compiler internals." {
}