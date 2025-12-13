const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const builtin = @import("builtin");
const cy = @import("cyber.zig");
const rt = cy.rt;
const C = @import("capi.zig");
const fmt = cy.fmt;
const v = fmt.v;
const sema = cy.sema;
const types = cy.types;
const log = cy.log.scoped(.chunk);
const llvm = @import("llvm.zig");
const llvm_gen = @import("llvm_gen.zig");
const bc = @import("bc_gen.zig");
const jitgen = @import("jit/gen.zig");
const X64 = @import("jit/x64.zig");
const ast = cy.ast;
const bt = cy.types.BuiltinTypes;

pub const ChunkId = u32;
pub const SymId = u32;
pub const FuncId = u32;

const VarAccessPath = struct {
    // Maybe redundant with recParentLocal()
    var_id: sema.LocalId = undefined,

    struct_field_offset: usize = 0,
};

/// A compilation unit.
/// It contains data to compile from source into a module with exported symbols.
pub const Chunk = struct {
    id: ChunkId,
    alloc: std.mem.Allocator,
    compiler: *cy.Compiler,
    sema: *cy.Sema,
    vm: *cy.VM,

    /// Heap for compile-time values.
    heap: *cy.Heap,

    /// Source code.
    src: []const u8,
    manage_src: bool,

    /// Whether `src` needs to be loaded from `srcUri`.
    pending_load_src: bool,
    loaded: bool,

    importer: ?*ast.Node,

    /// Read-only view of the AST.
    ast: cy.ast.AstView,

    /// Owned, absolute path to source.
    srcUri: []const u8,

    parser: cy.Parser,

    /// Used for temp string building.
    tempBufU8: std.ArrayListUnmanaged(u8),
    u16_stack: std.ArrayListUnmanaged(u16),

    /// Since nodes are currently processed recursively,
    /// set the current node so that error reporting has a better
    /// location context for helper methods that simply return no context errors.
    curNode: ?*ast.Node,

    /// The current expr that has a continuation body, otherwise null.
    cur_cont_expr: ?*ast.LambdaContExpr,

    ///
    /// Sema pass.
    ///
    semaProcs: std.ArrayListUnmanaged(sema.Proc),
    semaBlocks: std.ArrayListUnmanaged(sema.Block),

    /// Generic stacks.
    listDataStack: std.ArrayListUnmanaged(ListData),

    /// Stack for building func signatures. (e.g. for nested func calls)
    typeStack: std.ArrayListUnmanaged(*types.Type),

    valueStack: std.ArrayListUnmanaged(cy.Value),
    func_template_param_stack: std.ArrayListUnmanaged(cy.sym.FuncTemplateParam),

    expr_stack: std.ArrayListUnmanaged(sema.ExprResult),

    func_param_stack: std.ArrayListUnmanaged(sema.FuncParam),

    active_field_stack: std.ArrayListUnmanaged(bool),

    ir: cy.ir.Buffer,

    /// VM locals. From a block's `localStart` offset, this is indexed by the virtual reg 
    /// which includes function prelude regs and params for simplicity.

    /// Local vars.
    varStack: std.ArrayListUnmanaged(sema.Local),
    varShadowStack: std.ArrayListUnmanaged(cy.sema.VarShadow),

    /// Main sema block id.
    mainSemaProcId: sema.ProcId,

    /// Builtins is always a fallback module for symbol lookups.
    fallback_modules: std.ArrayListUnmanaged(*cy.Sym),

    /// Syms owned by this chunk. Does not include field syms.
    syms: std.ArrayListUnmanaged(*cy.Sym),

    /// Functions owned by this chunk.
    /// Includes lambdas which are not linked from a named sym.
    funcs: std.ArrayListUnmanaged(*cy.Func),

    host_funcs: std.StringHashMapUnmanaged(C.BindFunc),
    host_types: std.StringHashMapUnmanaged(C.BindType),
    host_globals: std.StringHashMapUnmanaged(C.BindGlobal),

    /// Run before chunk is destroyed.
    on_destroy: C.ModuleOnDestroyFn = null,

    /// Run after declarations have been loaded.
    on_load: C.ModuleOnLoadFn = null,

    /// Reference the current resolve context.
    resolve_stack: std.ArrayListUnmanaged(sema.ResolveContext),

    has_pending_ct_stmts: bool,
    resolving_ct_stmts: bool,

    ///
    /// Codegen pass
    ///
    proc_stack: std.ArrayListUnmanaged(bc.Proc), // TODO: This isn't needed anymore. `cur_proc` should be enough.
    funcs_debug: std.ArrayListUnmanaged(FuncDebugInfo),
    blocks: std.ArrayListUnmanaged(bc.Block),
    blockJumpStack: std.ArrayListUnmanaged(BlockJump),

    /// Maps ir local to bc reg.
    bc_local_map: std.ArrayList(bc.Reg),

    relocations: std.ArrayList(bc.Relocation),
    rt_relocations: std.ArrayList(bc.Relocation),

    cur_proc: *bc.Proc,

    // TODO: inline the active `Proc`.
    cur_sema_proc: *sema.Proc,

    temp_access_path: VarAccessPath,

    buf: cy.ByteCodeBuffer,
    jitBuf: *jitgen.CodeBuffer,
    x64Enc: X64.Encoder,

    /// This chunk's sym.
    sym: *cy.sym.Chunk,

    has_bind_lib: bool,
    bind_lib: ?[]const u8,
    has_extern_func: bool,

    has_static_init: bool,

    encoder: cy.ast.Encoder,

    indent: u32,

    /// Fallback for name resolving. Used by libcyber's `clFindType`.
    resolve_name_fn: ?*const fn(c: *Chunk, name: []const u8) ?sema.NameResult = null,

    reserved_syms: bool = false,

    /// LLVM
    tempTypeRefs: if (cy.hasJIT) std.ArrayListUnmanaged(llvm.TypeRef) else void,
    tempValueRefs: if (cy.hasJIT) std.ArrayListUnmanaged(llvm.ValueRef) else void,
    // mod: if (cy.hasJIT) llvm.ModuleRef else void,
    builder: if (cy.hasJIT) llvm.BuilderRef else void,
    ctx: if (cy.hasJIT) llvm.ContextRef else void,
    llvmFuncs: if (cy.hasJIT) []LLVM_Func else void, // One-to-one with `semaFuncDecls`
    
    user_data: ?*anyopaque,

    trace_eval_expr_depth: usize = 0,

    pub fn init(self: *Chunk, c: *cy.Compiler, id: ChunkId, srcUri: []const u8, src: ?[]const u8) !void {
        self.* = .{
            .id = id,
            .alloc = c.alloc,
            .compiler = c,
            .heap = &c.heap,
            .sema = &c.sema,
            .cur_sema_proc = undefined,
            .vm = c.vm,
            .src = src orelse "",
            .manage_src = true,
            .ast = undefined,
            .srcUri = srcUri,
            .sym = undefined,
            .parser = undefined,
            .semaProcs = .{},
            .semaBlocks = .{},
            .proc_stack = .{},
            .funcs_debug = .{},
            .blocks = .{},
            .blockJumpStack = .{},
            .varShadowStack = .{},
            .varStack = .{},
            .typeStack = .{},
            .valueStack = .{},
            .expr_stack = .{},
            .func_param_stack = .{},
            .active_field_stack = .{},
            .temp_access_path = .{},
            .func_template_param_stack = .{},
            .ir = undefined,
            .listDataStack = .{},
            .cur_proc = undefined,
            .buf = cy.ByteCodeBuffer.init(c.alloc, c.vm),
            .jitBuf = undefined,
            .x64Enc = undefined,
            .curNode = null,
            .cur_cont_expr = null,
            .tempBufU8 = .{},
            .u16_stack = .{},
            .mainSemaProcId = cy.NullId,
            .fallback_modules = .{},
            .tempTypeRefs = undefined,
            .tempValueRefs = undefined,
            .builder = undefined,
            .ctx = undefined,
            .llvmFuncs = undefined,
            .has_static_init = false,
            .encoder = undefined,
            .indent = 0,
            .syms = .{},
            .funcs = .{},
            .host_funcs = .{},
            .host_types = .{},
            .host_globals = .{},
            .resolve_stack = .{},
            .user_data = null,
            .has_bind_lib = false,
            .bind_lib = null,
            .has_pending_ct_stmts = false,
            .resolving_ct_stmts = false,
            .pending_load_src = src == null,
            .loaded = false,
            .importer = null,
            .has_extern_func = false,
            .bc_local_map = .{},
            .relocations = .{},
            .rt_relocations = .{},
        };
        self.encoder.c = c;
        self.ir.init(c.alloc);
        try self.parser.init(c.alloc);

        if (cy.hasJIT) {
            self.tempTypeRefs = .{};
            self.tempValueRefs = .{};
            // self.exprResStack = .{};
            // self.exprStack = .{};
            self.llvmFuncs = &.{}; 
        }
    }

    // Compile-time values are destructed first since they depend on type syms.
    pub fn deinitCtValues(self: *Chunk) void {
        for (self.syms.items) |sym| {
            cy.sym.deinit_sym_ct_values(self.heap, sym);
        }
        for (self.funcs.items) |func| {
            cy.sym.deinit_func_ct_values(self.heap, func);
        }
    }

    pub fn deinit(self: *Chunk) void {
        self.tempBufU8.deinit(self.alloc);
        self.u16_stack.deinit(self.alloc);

        self.semaBlocks.deinit(self.alloc);

        for (self.semaProcs.items) |*sproc| {
            sproc.deinit(self.alloc);
        }
        self.semaProcs.deinit(self.alloc);

        self.proc_stack.deinit(self.alloc);
        self.funcs_debug.deinit(self.alloc);
        self.blocks.deinit(self.alloc); 

        self.blockJumpStack.deinit(self.alloc);
        self.varShadowStack.deinit(self.alloc);
        self.varStack.deinit(self.alloc);

        self.typeStack.deinit(self.alloc);
        self.valueStack.deinit(self.alloc);
        self.expr_stack.deinit(self.alloc);
        self.func_param_stack.deinit(self.alloc);
        self.active_field_stack.deinit(self.alloc);
        self.func_template_param_stack.deinit(self.alloc);
        self.ir.deinit(self.alloc);
        self.listDataStack.deinit(self.alloc);
        self.resolve_stack.deinit(self.alloc);

        if (cy.hasJIT) {
            self.tempTypeRefs.deinit(self.alloc);
            self.tempValueRefs.deinit(self.alloc);
            // self.exprResStack.deinit(self.alloc);
            // self.exprStack.deinit(self.alloc);
            self.alloc.free(self.llvmFuncs);
        }

        self.fallback_modules.deinit(self.alloc);

        for (self.funcs.items) |func| {
            func.destroy(self.alloc);
        }
        self.funcs.deinit(self.alloc);

        self.host_funcs.deinit(self.alloc);
        self.host_types.deinit(self.alloc);
        self.host_globals.deinit(self.alloc);

        // Deinit chunk syms. Any retained values must already be freed in case they need to reference syms.
        for (self.syms.items) |sym| {
            sym.destroy(self.heap, self.alloc);
        }
        self.syms.deinit(self.alloc);

        self.buf.deinit();

        if (self.bind_lib) |dl_bind| {
            self.alloc.free(dl_bind);
            self.bind_lib = null;
        }

        self.bc_local_map.deinit(self.alloc);
        self.relocations.deinit(self.alloc);
        self.rt_relocations.deinit(self.alloc);

        self.alloc.free(self.srcUri);
        self.parser.deinit();
    }

    pub fn fromC(mod: C.Module) *cy.Chunk {
        return @ptrCast(@alignCast(mod.ptr));
    }

    pub fn updateAstView(self: *cy.Chunk, view: cy.ast.AstView) void {
        self.ast = view;
    }

    pub fn genBlock(self: *cy.Chunk) *bc.Block {
        return &self.blocks.items[self.blocks.items.len-1];
    }

    pub fn loopBlock(self: *cy.Chunk, start: usize) ?usize {
        var i: usize = self.semaBlocks.items.len;
        while (i > start) {
            i -= 1;
            if (self.semaBlocks.items[i].loop) {
                return i;
            }
        }
        return null;
    }

    pub fn block(self: *cy.Chunk) *sema.Block {
        return &self.semaBlocks.items[self.semaBlocks.items.len-1];
    }

    pub fn proc(self: *cy.Chunk) *sema.Proc {
        return &self.semaProcs.items[self.semaProcs.items.len-1];
    }

    pub fn procBlock(self: *cy.Chunk) *sema.Block {
        return &self.semaBlocks.items[self.proc().block_start];
    }

    pub fn getProcParams(c: *const cy.Chunk, p: *sema.Proc) []const sema.Local {
        return c.varStack.items[p.varStart..p.varStart+p.numParams];
    }

    /// Includes aliases.
    pub fn getProcVars(c: *const cy.Chunk, p: *sema.Proc) []const sema.Local {
        return c.varStack.items[p.varStart+p.numParams..];
    }

    pub inline fn semaBlockDepth(self: *Chunk) u32 {
        return @intCast(self.semaProcs.items.len);
    }

    pub fn reserveIfTempLocal(self: *Chunk, local: LocalId) !void {
        if (self.isTempLocal(local)) {
            try self.setReservedTempLocal(local);
        }
    }

    pub fn initGenValue(self: *const Chunk, local: LocalId, vtype: types.TypeId, retained: bool) bc.GenValue {
        if (self.isTempLocal(local)) {
            return bc.GenValue.initTempValue(local, vtype, retained);
        } else {
            return bc.GenValue.initLocalValue(local, vtype, retained);
        }
    }

    /// Given two local values, determine the next destination temp local.
    /// The type of the dest value is left undefined to be set by caller.
    fn nextTempDestValue(self: *cy.Compiler, src1: bc.GenValue, src2: bc.GenValue) !bc.GenValue {
        if (src1.isTempLocal == src2.isTempLocal) {
            if (src1.isTempLocal) {
                const minTempLocal = std.math.min(src1.local, src2.local);
                self.setFirstFreeTempLocal(minTempLocal + 1);
                return bc.GenValue.initTempValue(minTempLocal, undefined);
            } else {
                return bc.GenValue.initTempValue(try self.nextFreeTempLocal(), undefined);
            }
        } else {
            if (src1.isTempLocal) {
                return bc.GenValue.initTempValue(src1.local, undefined);
            } else {
                return bc.GenValue.initTempValue(src2.local, undefined);
            }
        }
    }

    pub fn get_chunk_by_uri(self: *Chunk, uri: []const u8) !?*cy.Chunk {
        var buf: [1024]u8 = undefined;
        const ruri = try cy.compiler.resolveModuleUriFrom(self, &buf, uri, null);
        return self.compiler.chunk_map.get(ruri);
    }

    pub fn pushEmptyJumpNotCond(self: *Chunk, reg: bc.Reg, node: *ast.Node) !u32 {
        const start: u32 = @intCast(self.buf.ops.items.len);
        try self.pushCode(.jump_f, &.{reg, 0}, node);
        return start;
    }

    pub fn pushJumpBackTo(self: *Chunk, toPc: usize, node: *ast.Node) !void {
        const pc = self.buf.ops.items.len;
        try self.pushCode(.jump, &.{@bitCast(-@as(i16, @intCast(pc - toPc)))}, node);
    }

    pub fn pushEmptyJump(self: *Chunk, node: *ast.Node) !u32 {
        const start: u32 = @intCast(self.buf.ops.items.len);
        try self.pushCode(.jump, &.{0}, node);
        return start;
    }

    pub fn pushEmptyJumpCond(self: *Chunk, reg: bc.Reg, node: *ast.Node) !u32 {
        const start: u32 = @intCast(self.buf.ops.items.len);
        try self.buf.pushCode(.jump_t, &.{reg, 0}, node);
        return start;
    }

    pub fn patchJumpToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.ops.items[jumpPc + 1].val = @intCast(self.buf.ops.items.len - jumpPc);
    }

    pub fn patchJumpCondToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.ops.items[jumpPc + 2].val = @intCast(self.buf.ops.items.len - jumpPc);
    }

    pub fn patchJumpNotCondToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.ops.items[jumpPc + 2].val = @intCast(self.buf.ops.items.len - jumpPc);
    }

    /// Patches block breaks. For `if` and `match` blocks.
    /// All other jumps are propagated up the stack by copying to the front.
    /// Returns the adjusted jumpStackStart for this block.
    pub fn patchSubBlockBreakJumps(self: *Chunk, jumpStackStart: usize, breakPc: usize) usize {
        var keepIdx = jumpStackStart;
        for (self.blockJumpStack.items[jumpStackStart..]) |jump| {
            if (jump.jumpT == .subBlockBreak) {
                self.buf.setOpArgU16(jump.pc + 1, @intCast(breakPc - jump.pc));
            } else {
                self.blockJumpStack.items[keepIdx] = jump;
                keepIdx += 1;
            }
        }
        return keepIdx;
    }

    pub fn patchBreaks(self: *Chunk, jumpStackStart: usize, block_offset: usize, breakPc: usize) usize {
        var jump_stack_end = jumpStackStart;
        for (self.blockJumpStack.items[jumpStackStart..]) |jump| {
            switch (jump.jumpT) {
                .brk => {
                    if (jump.block_offset == block_offset) {
                        if (breakPc > jump.pc) {
                            self.buf.ops.items[jump.pc + 1].val = @intCast(breakPc - jump.pc);
                        } else {
                            self.buf.ops.items[jump.pc + 1].val = @bitCast(-@as(i16, @intCast(jump.pc - breakPc)));
                        }
                    } else {
                        self.blockJumpStack.items[jump_stack_end] = jump;
                        jump_stack_end += 1;
                    }
                },
                else => {
                    self.blockJumpStack.items[jump_stack_end] = jump;
                    jump_stack_end += 1;
                },
            }
        }
        return jump_stack_end;
    }

    pub fn patchForBlockJumps(self: *Chunk, jumpStackStart: usize, block_offset: usize, breakPc: usize, contPc: usize) usize {
        var jump_stack_end = jumpStackStart;
        for (self.blockJumpStack.items[jumpStackStart..]) |jump| {
            switch (jump.jumpT) {
                .brk => {
                    if (jump.block_offset == block_offset) {
                        if (breakPc > jump.pc) {
                            self.buf.ops.items[jump.pc + 1].val = @intCast(breakPc - jump.pc);
                        } else {
                            self.buf.ops.items[jump.pc + 1].val = @bitCast(-@as(i16, @intCast(jump.pc - breakPc)));
                        }
                    } else {
                        self.blockJumpStack.items[jump_stack_end] = jump;
                        jump_stack_end += 1;
                    }
                },
                .cont => {
                    if (jump.block_offset == block_offset) {
                        if (contPc > jump.pc) {
                            self.buf.ops.items[jump.pc + 1].val = @intCast(contPc - jump.pc);
                        } else {
                            self.buf.ops.items[jump.pc + 1].val = @bitCast(-@as(i16, @intCast(jump.pc - contPc)));
                        }
                    } else {
                        self.blockJumpStack.items[jump_stack_end] = jump;
                        jump_stack_end += 1;
                    }
                },
            }
        }
        return jump_stack_end;
    }

    pub fn getMaxUsedRegisters(self: *Chunk) bc.Reg {
        return self.cur_proc.max_regs;
    }

    pub fn blockNumLocals(self: *Chunk) usize {
        return sema.curBlock(self).locals.items.len + sema.curBlock(self).params.items.len;
    }

    pub fn genGetVarPtr(self: *const Chunk, id: sema.LocalId) ?*sema.Local {
        if (id != cy.NullId) {
            return &self.vars.items[id];
        } else {
            return null;
        }
    }

    pub fn genGetVar(self: *const Chunk, id: sema.LocalId) ?sema.Local {
        if (id != cy.NullId) {
            return self.vars.items[id];
        } else {
            return null;
        }
    }

    pub fn unescapeString(self: *Chunk, literal: []const u8) ![]const u8 {
        try self.tempBufU8.resize(self.alloc, literal.len);
        return cy.sema.unescapeString(self.tempBufU8.items, literal, false);
    }

    pub fn dumpLocals(self: *const Chunk, sproc: *sema.Proc) !void {
        if (cy.Trace) {
            rt.print(self.vm, "Locals:\n");
            const params = self.getProcParams(sproc);
            for (params) |svar| {
                rt.printFmt(self.vm, "{} (param), local: {}, dyn: {}, type: {}\n", &.{
                    v(svar.name()), v(svar.local), v(svar.decl_t.id() == bt.Dyn), v(svar.decl_t),
                });
            }
            const vars = self.getProcVars(sproc);
            for (vars) |svar| {
                rt.printFmt(self.vm, "{}, local: {}, dyn: {}, type: {}\n", &.{
                    v(svar.name()), v(svar.local), v(svar.decl_t.id() == bt.Dyn), v(svar.decl_t),
                });
            }
        }
    }

    pub fn appendContextTrace(self: *Chunk, report: *cy.compiler.Report, msg: []const u8, ctx_n: *ast.Node) !*cy.compiler.Report {
        const trace = try self.alloc.create(cy.compiler.Report);
        trace.* = .{
            .type = .context,
            .msg = try self.alloc.dupe(u8, msg),
            .chunk = ctx_n.src(),
            .loc = @intFromPtr(ctx_n),
        };
        report.next = trace;
        return trace;
    }

    pub fn appendErrorTrace(self: *Chunk, report: *cy.compiler.Report) !void {
        var last = report;
        if (self.resolve_stack.items.len > 0) {
            var i: usize = self.resolve_stack.items.len;
            while (i > 0) {
                i -= 1;
                const ctx = self.resolve_stack.items[i];
                const caller = ctx.caller orelse continue;
                const trace = try self.alloc.create(cy.compiler.Report);
                trace.* = .{
                    .type = .compile_err,
                    .msg = "",
                    .chunk = caller.src(),
                    .loc = @intFromPtr(caller),
                };
                last.next = trace;
                last = trace;
            }
        }
    }

    pub fn reportError(self: *Chunk, msg: []const u8, opt_node: ?*ast.Node) error{OutOfMemory, CompileError} {
        const id = try self.addReport(msg, opt_node);
        try self.appendErrorTrace(&self.compiler.reports.items[id]);
        return error.CompileError;
    }

    pub fn addReport(self: *Chunk, msg: []const u8, opt_node: ?*ast.Node) !usize {
        var chunk_id: u32 = undefined;
        if (opt_node) |node| {
            chunk_id = node.src();
        } else {
            const ctx = sema.getResolveContext(self);
            chunk_id = ctx.chunk.id;
        }
        return self.compiler.addReport(.compile_err, msg, chunk_id, @intFromPtr(opt_node));
    }

    pub fn reportErrorFmt(self: *Chunk, format: []const u8, args: []const fmt.FmtValue, opt_node: ?*ast.Node) error{CompileError, OutOfMemory, FormatError, WriteFailed} {
        const id = try self.addReportFmt(format, args, opt_node);
        try self.appendErrorTrace(&self.compiler.reports.items[id]);
        return error.CompileError;
    }

    pub fn addReportFmt(self: *Chunk, format: []const u8, args: []const fmt.FmtValue, opt_node: ?*ast.Node) !usize {
        var chunk_id: u32 = undefined;
        if (opt_node) |node| {
            chunk_id = node.src();
        } else {
            if (self.resolve_stack.items.len > 0) {
                const ctx = sema.getResolveContext(self);
                chunk_id = ctx.chunk.id;
            } else {
                chunk_id = self.id;
            }
        }
        return self.compiler.addReportFmt(.compile_err, format, args, chunk_id, @intFromPtr(opt_node));
    }

    /// An optional debug sym is only included in Trace builds.
    pub fn pushOptionalDebugSym(c: *Chunk, node: *ast.Node) !void {
        if (cy.Trace or c.compiler.vm.config.gen_all_debug_syms) {
            try c.buf.pushFailableDebugSym(
                c.buf.ops.items.len, c.cur_proc.chunk_id, node.pos(), c.cur_proc.id,
            );
        }
    }

    pub fn pushFailableDebugSym(self: *Chunk, node: *ast.Node) !void {
        try self.buf.pushFailableDebugSym(
            self.buf.ops.items.len, self.cur_proc.chunk_id, node.pos(), self.cur_proc.id,
        );
    }

    pub fn fmtExtraDesc(self: *Chunk, comptime format: []const u8, vals: anytype) !u32 {
        // Behind a comptime flag since it's doing allocation.
        if (cy.Trace) {
            const idx = self.buf.instDescExtras.items.len;
            const text = try std.fmt.allocPrint(self.alloc, format, vals);
            try self.buf.instDescExtras.append(self.alloc, .{
                .text = text,
            });
            return @intCast(idx);
        } else {
            return cy.NullId;
        }
    }

    /// An instruction that can fail (can throw or panic).
    pub fn pushFCode(c: *Chunk, code: cy.OpCode, args: []const u16, node: *ast.Node) !void {
        log.tracev("pushFCode: {s} {}", .{@tagName(code), c.buf.ops.items.len});
        try c.pushFailableDebugSym(node);
        try c.buf.pushOpSliceExt(code, args, null);
        if (cy.Trace) {
            const exp = cy.bytecode.getInstLenAt(c.buf.ops.items.ptr + c.buf.ops.items.len - 1 - args.len) - 1;
            if (args.len != exp) {
                std.debug.panic("Expected {} operands for {}, found {}.", .{exp, code, args.len});
            }
        }
    }

    pub fn pushCode(c: *Chunk, code: cy.OpCode, args: []const u16, node: *ast.Node) !void {
        log.tracev("pushCode: {s} {}", .{@tagName(code), c.buf.ops.items.len});
        try c.pushOptionalDebugSym(node);
        try c.buf.pushOpSliceExt(code, args, null);
        if (cy.Trace) {
            const exp = cy.bytecode.getInstLenAt(c.buf.ops.items.ptr + c.buf.ops.items.len - 1 - args.len) - 1;
            if (args.len != exp) {
                std.debug.panic("Expected {} operands for {}, found {}.", .{exp, code, args.len});
            }
        }
    }

    pub fn pushCodeExt(c: *Chunk, code: cy.OpCode, args: []const u16, operands: usize, node: *ast.Node) !void {
        log.tracev("pushCode: {s} {}", .{@tagName(code), c.buf.ops.items.len});
        try c.pushOptionalDebugSym(node);
        try c.buf.pushOpSliceExt(code, args, null);
        _ = try c.buf.reserveData(operands);
        if (cy.Trace) {
            const exp = cy.bytecode.getInstLenAt(c.buf.ops.items.ptr + c.buf.ops.items.len - 1 - args.len - operands) - 1;
            if (args.len + operands != exp) {
                std.debug.panic("Expected {} operands for {}, found {}.", .{exp, code, args.len + operands});
            }
        }
    }

    pub fn pushCodeBytes(c: *Chunk, bytes: []const u8) !void {
        try c.buf.pushOperands(bytes);
    }

    pub fn findResolvedType(c: *cy.Chunk, spec: []const u8, node: *ast.Node) !?*cy.Type {
        const type_ = (try c.vm.findType(spec)) orelse {
            return null;
        };
        try cy.sema_type.ensure_resolved_type(c, type_, node);
        return type_;
    }

    pub const createTypeSym = cy.sym.createTypeSym;
    pub const createTypeAlias = cy.sym.createTypeAlias;
    pub const createTypeConst = cy.sym.createTypeConst;
    pub const createFuncSym = cy.sym.createFuncSym;
    pub const createFunc = cy.sym.createFunc;
    pub const createConst = cy.sym.createConst;
    pub const createField = cy.sym.createField;
    pub const createHostVar = cy.sym.createHostVar;
    pub const createUserVar = cy.sym.createUserVar;
    pub const createEnumCase = cy.sym.createEnumCase;
    pub const createChoiceCase = cy.sym.createChoiceCase;
    pub const createUnionCase = cy.sym.createUnionCase;
    pub const createFuncTemplate = cy.sym.createFuncTemplate;
    pub const createChunkSym = cy.sym.createChunkSym;
    pub const createUseAlias = cy.sym.createUseAlias;
    pub const createTemplate = cy.sym.createTemplate;
    pub const createVariantMember = cy.sym.createVariantMember;
    pub const createVariantFunc = cy.sym.createVariantFunc;
    pub const createExternVar = cy.sym.createExternVar;
    pub const getSymValueType = cy.sym.getSymValueType;

    pub const declareEnumCase = cy.module.declareEnumCase;
    pub const declareChoiceCase = cy.module.declareChoiceCase;
    pub const declareUnionCase = cy.module.declareUnionCase;
    pub const declareField = cy.module.declareField;
    pub const accessResolvedSym = cy.module.accessResolvedSym;
    pub const accessResolvedSymOrFail = cy.module.accessResolvedSymOrFail;
    pub const appendFunc = cy.module.appendFunc;
    pub const addUserLambda = cy.module.addUserLambda;
    pub const reserveTypeSym = cy.module.reserveTypeSym;
    pub const reserveDistinctType = cy.module.reserveDistinctType;
    pub const reserveTypeAlias = cy.module.reserveTypeAlias;
    pub const reserve_type_const = cy.module.reserve_type_const;
    pub const reserveFunc = cy.module.reserveFunc;
    pub const reserveUserFunc = cy.module.reserveUserFunc;
    pub const reserveTemplate = cy.module.reserveTemplate;
    pub const reserveTraitFunc = cy.module.reserveTraitFunc;
    pub const reserveConst = cy.module.reserveConst;
    pub const reserveFuncTemplateSym = cy.module.reserveFuncTemplateSym;
    pub const reserveUseAlias = cy.module.reserveUseAlias;
    pub const reserveVariantMember = cy.module.reserveVariantMember;
    pub const reserveVariantFunc = cy.module.reserveVariantFunc;
    pub const reserveUserVar = cy.module.reserveUserVar;
    pub const reserveHostVar = cy.module.reserveHostVar;
    pub const reserveExternVar = cy.module.reserveExternVar;
    pub const resolveFunc = cy.module.resolveFunc;
    pub const resolveHostFunc = cy.module.resolveHostFunc;
    pub const resolveUserFunc = cy.module.resolveUserFunc;
    pub const resolveUserLambda = cy.module.resolveUserLambda;
    pub const resolveFuncInfo = cy.module.resolveFuncInfo;
    pub const resolveHostConstEvalFunc = cy.module.resolveHostConstEvalFunc;
    pub const resolveHostBuiltinFunc = cy.module.resolveHostBuiltinFunc;
    pub const resolveHostVar = cy.module.resolveHostVar;
    pub const resolveUserVarType = cy.module.resolveUserVarType;
    pub const checkResolvedSymIsDistinct = cy.module.checkResolvedSymIsDistinct;
    pub const getResolvedSym = cy.module.getResolvedSym;
    pub const getResolvedSymOrFail = cy.module.getResolvedSymOrFail;

    pub const checkOption = sema.checkOption;
    pub const checkResult = sema.checkResult;
    pub const semaInitExpr = sema.semaInitExpr;
    pub const semaObjectInit2 = sema.semaObjectInit2;
    pub const semaInitStruct = sema.semaInitStruct;
    pub const semaInitFixedArray = sema.semaInitVector;
    pub const semaTupleInit = sema.semaTupleInit;
    pub const semaPtrOrType = sema.semaPtrOrType;
    pub const semaRefOrType = sema.semaRefOrType;
    pub const semaBorrowOrType = sema.semaBorrowOrType;
    pub const semaBinExpr = sema.semaBinExpr;
    pub const semaUnExpr = sema.semaUnExpr;
    pub const semaIsError = sema.semaIsError;
    pub const semaIsNone = sema.semaIsNone;
    pub const semaIsZero = sema.semaIsZero;
    pub const semaConst = sema.semaConst;
    pub const semaConst8 = sema.semaConst8;
    pub const semaConst16 = sema.semaConst16;
    pub const semaConst32 = sema.semaConst32;
    pub const semaNone = sema.semaNone;
    pub const semaFalse = sema.semaFalse;
    pub const semaString = sema.semaString;
    pub const semaStringTarget = sema.semaStringTarget;
    pub const semaRawString = sema.semaRawString;
    pub const semaRawStringAs = sema.semaRawStringAs;
    pub const semaStringAs = sema.semaStringAs;
    pub const semaTrue = sema.semaTrue;
    pub const semaVoid = sema.semaVoid;
    pub const semaExprOrOpAssignBinExpr = sema.semaExprOrOpAssignBinExpr;
    pub const semaExprSkipSym = sema.semaExprSkipSym;
    pub const semaExpr = sema.semaExpr;
    pub const semaExprHint = sema.semaExprHint;
    pub const semaExprCstr = sema.semaExprCstr;
    pub const semaExprNoCheck = sema.semaExprNoCheck;
    pub const semaExprTarget = sema.semaExprTarget;
    pub const semaCallExpr = sema.semaCallExpr;
    pub const semaCallValue = sema.semaCallValue;
    pub const semaCallUnion = sema.semaCallUnion;
    pub const semaCallPtr = sema.semaCallPtr;
    pub const semaCallFunc = sema.semaCallFunc;
    pub const semaCallFunc2 = sema.semaCallFunc2;
    pub const semaCallFuncResult = sema.semaCallFuncResult;
    pub const semaCallFuncSym = sema.semaCallFuncSym;
    pub const semaCallFuncSym1 = sema.semaCallFuncSym1;
    pub const semaCallFuncSym2 = sema.semaCallFuncSym2;
    pub const semaCallFuncSymRec = sema.semaCallFuncSymRec;
    pub const semaCallFuncSymRec2 = sema.semaCallFuncSymRec2;
    // pub const semaCallGenericFunc = sema.semaCallGenericFunc;
    // pub const semaCallGenericFuncRec = sema.semaCallGenericFuncRec;

    pub const sema_expr_cstr_template = sema.sema_expr_cstr_template;

    // pub usingnamespace jitgen.ChunkExt;
};

const FuncDebugInfo = struct {
    func: ?*cy.Func,

    name: []const u8,
    src_chunk: u32,
};

test "chunk internals." {
}

const BlockJumpType = enum {
    /// Breaks out of a for loop, while loop, or switch stmt.
    brk,
    /// Continues a for loop or while loop.
    cont,
};

const BlockJump = struct {
    jumpT: BlockJumpType,
    pc: u32,
    block_offset: u32,
};

const ReservedTempLocal = struct {
    local: LocalId,
};

const LocalId = u8;

pub const LLVM_Func = struct {
    typeRef: llvm.TypeRef,
    funcRef: llvm.ValueRef,
};

pub const TypeDepNode = struct {
    visited: bool,
    hasCircularDep: bool,
    hasUnsupported: bool,
};

pub const ListData = union {
    pc: u32,
    node: ?*ast.Node,
    constIdx: u16,
    jumpToEndPc: u32,
};