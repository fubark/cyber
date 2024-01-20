const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const builtin = @import("builtin");
const cy = @import("cyber.zig");
const cc = @import("clib.zig");
const fmt = cy.fmt;
const v = fmt.v;
const sema = cy.sema;
const types = cy.types;
const log = cy.log.scoped(.chunk);
const llvm = @import("llvm.zig");
const llvm_gen = @import("llvm_gen.zig");
const bc_gen = @import("bc_gen.zig");
const jitgen = @import("jit/gen.zig");
const X64 = @import("jit/x64.zig");

pub const ChunkId = u32;

/// A compilation unit.
/// It contains data to compile from source into a module with exported symbols.
pub const Chunk = struct {
    id: ChunkId,
    alloc: std.mem.Allocator,
    compiler: *cy.VMcompiler,
    sema: *cy.Sema,

    /// Source code.
    src: []const u8,

    ast: cy.ast.Source,

    /// Owned, absolute path to source.
    srcUri: []const u8,

    parser: cy.Parser,
    parserAstRootId: cy.NodeId,

    /// Used for temp string building.
    tempBufU8: std.ArrayListUnmanaged(u8),

    /// Since nodes are currently processed recursively,
    /// set the current node so that error reporting has a better
    /// location context for helper methods that simply return no context errors.
    curNodeId: cy.NodeId,

    ///
    /// Sema pass
    ///
    semaBlocks: std.ArrayListUnmanaged(sema.Block),
    semaSubBlocks: std.ArrayListUnmanaged(sema.SubBlock),
    capVarDescs: std.AutoHashMapUnmanaged(sema.LocalVarId, sema.CapVarDesc),

    genValueStack: std.ArrayListUnmanaged(bc_gen.GenValue),

    /// Generic stacks.
    dataStack: std.ArrayListUnmanaged(Data),
    dataU8Stack: std.ArrayListUnmanaged(DataU8),
    listDataStack: std.ArrayListUnmanaged(ListData),

    /// Stack for building func signatures. (eg. for nested func calls)
    typeStack: std.ArrayListUnmanaged(types.TypeId),

    ir: cy.ir.Buffer,

    /// Maps a IR local to a VM local.
    genIrLocalMapStack: std.ArrayListUnmanaged(u8),

    /// VM locals. From a block's `localStart` offset, this is indexed by the virtual reg 
    /// which includes function prelude regs and params for simplicity.
    genLocalStack: std.ArrayListUnmanaged(bc_gen.Local),

    curObjectSym: ?*cy.sym.ObjectType,

    /// Symbols declared in this chunk that have modules and which need to be deinited.
    modSyms: std.ArrayListUnmanaged(*cy.Sym),

    /// Record other chunks that this chunk's static initializer depends on.
    symInitChunkDeps: std.AutoHashMapUnmanaged(*cy.Chunk, void),

    /// Local vars.
    varStack: std.ArrayListUnmanaged(sema.LocalVar),
    varShadowStack: std.ArrayListUnmanaged(cy.sema.VarShadow),
    preLoopVarSaveStack: std.ArrayListUnmanaged(cy.sema.PreLoopVarSave),
    assignedVarStack: std.ArrayListUnmanaged(sema.LocalVarId),

    /// Which sema sym var is currently being analyzed for an assignment initializer.
    curInitingSym: ?*cy.Sym,
    curInitingSymDeps: std.AutoHashMapUnmanaged(*cy.Sym, void),

    /// Currently used to store lists of static var dependencies.
    symInitDeps: std.ArrayListUnmanaged(SymInitDep),
    symInitInfos: std.AutoHashMapUnmanaged(*cy.Sym, SymInitInfo),

    /// Main sema block id.
    mainSemaBlockId: sema.BlockId,

    /// Local sym names to mod syms. eg. imports, type aliases.
    /// Most syms are cached while syms like `Root` are always included.
    localSymMap: std.StringHashMapUnmanaged(*cy.Sym),

    /// Successful module func signature matches are cached.
    funcCheckCache: std.HashMapUnmanaged(sema.ModFuncSigKey, *cy.Func, cy.hash.KeyU96Context, 80),

    /// Using modules.
    usingModules: std.ArrayListUnmanaged(*cy.Sym),

    /// Object type dependency graph.
    /// This is only needed for default initializers so it is created on demand per chunk.
    typeDeps: std.ArrayListUnmanaged(TypeDepNode),
    typeDepsMap: std.AutoHashMapUnmanaged(*cy.Sym, u32),

    ///
    /// Codegen pass
    ///
    rega: cy.register.Allocator,
    blocks: std.ArrayListUnmanaged(bc_gen.GenBlock),
    subBlocks: std.ArrayListUnmanaged(bc_gen.GenSubBlock),
    subBlockJumpStack: std.ArrayListUnmanaged(SubBlockJump),

    regStack: std.ArrayListUnmanaged(u8),
    operandStack: std.ArrayListUnmanaged(u8),

    unwindTempIndexStack: std.ArrayListUnmanaged(UnwindTempIndex),
    unwindTempRegStack: std.ArrayListUnmanaged(u8),

    curBlock: *bc_gen.GenBlock,

    /// Shared final code buffer.
    buf: *cy.ByteCodeBuffer,
    jitBuf: *jitgen.CodeBuffer,
    x64Enc: X64.Encoder,

    nodes: []cy.Node,
    tokens: []const cy.Token,

    /// Whether the src is owned by the chunk.
    srcOwned: bool,

    /// Points to this chunk's `Module`.
    /// Its exported members will be populated in the Module as sema encounters them.
    mod: *cy.Module,
    sym: *cy.sym.Chunk,

    /// For binding #host func declarations.
    funcLoader: cc.FuncLoaderFn = null,
    /// For binding #host var declarations.
    varLoader: cc.VarLoaderFn = null,
    /// For binding #host type declarations.
    typeLoader: cc.TypeLoaderFn = null,
    /// Run after type declarations are loaded.
    onTypeLoad: cc.ModuleOnTypeLoadFn = null,
    /// Run after declarations have been loaded.
    onLoad: cc.ModuleOnLoadFn = null,
    /// Run before chunk is destroyed.
    onDestroy: cc.ModuleOnDestroyFn = null,
    /// Counter for loading #host funcs.
    curHostFuncIdx: u32,
    /// Counter for loading #host vars.
    curHostVarIdx: u32,
    /// Counter for loading #host types.
    curHostTypeIdx: u32,

    hasStaticInit: bool,
    initializerVisiting: bool,
    initializerVisited: bool,

    encoder: cy.ast.Encoder,

    /// LLVM
    tempTypeRefs: if (cy.hasJIT) std.ArrayListUnmanaged(llvm.TypeRef) else void,
    tempValueRefs: if (cy.hasJIT) std.ArrayListUnmanaged(llvm.ValueRef) else void,
    // mod: if (cy.hasJIT) llvm.ModuleRef else void,
    builder: if (cy.hasJIT) llvm.BuilderRef else void,
    ctx: if (cy.hasJIT) llvm.ContextRef else void,
    llvmFuncs: if (cy.hasJIT) []LLVM_Func else void, // One-to-one with `semaFuncDecls`

    /// Chunk owns `srcUri` and `src`.
    pub fn init(c: *cy.VMcompiler, id: ChunkId, srcUri: []const u8, src: []const u8, sym: *cy.sym.Chunk) !Chunk {
        var new = Chunk{
            .id = id,
            .alloc = c.alloc,
            .compiler = c,
            .sema = &c.sema,
            .src = src,
            .ast = undefined,
            .srcUri = srcUri,
            .sym = sym,
            .parser = cy.Parser.init(c.alloc),
            .parserAstRootId = cy.NullId,
            .nodes = undefined,
            .tokens = undefined,
            .semaBlocks = .{},
            .semaSubBlocks = .{},
            .capVarDescs = .{},
            .blocks = .{},
            .subBlocks = .{},
            .subBlockJumpStack = .{},
            .assignedVarStack = .{},
            .varShadowStack = .{},
            .varStack = .{},
            .preLoopVarSaveStack = .{},
            .typeStack = .{},
            .ir = cy.ir.Buffer.init(),
            .genValueStack = .{},
            .genLocalStack = .{},
            .genIrLocalMapStack = .{},
            .dataStack = .{},
            .dataU8Stack = .{},
            .listDataStack = .{},
            .regStack = .{},
            .operandStack = .{},
            .unwindTempIndexStack = .{},
            .unwindTempRegStack = .{},
            .curBlock = undefined,
            .curObjectSym = null,
            .buf = undefined,
            .jitBuf = undefined,
            .x64Enc = undefined,
            .curNodeId = cy.NullId,
            .symInitDeps = .{},
            .symInitInfos = .{},
            .curInitingSym = null,
            .curInitingSymDeps = .{},
            .symInitChunkDeps = .{},
            .modSyms = .{},
            .tempBufU8 = .{},
            .srcOwned = true,
            .mainSemaBlockId = cy.NullId,
            .localSymMap = .{},
            .funcCheckCache = .{},
            .rega = cy.register.Allocator.init(c, id),
            .curHostFuncIdx = 0,
            .curHostVarIdx = 0,
            .curHostTypeIdx = 0,
            .usingModules = .{},
            .tempTypeRefs = undefined,
            .tempValueRefs = undefined,
            .mod = undefined,
            .builder = undefined,
            .ctx = undefined,
            .llvmFuncs = undefined,
            .typeDeps = .{},
            .typeDepsMap = .{},
            .hasStaticInit = false,
            .initializerVisited = false,
            .initializerVisiting = false,
            .encoder = undefined,
        };

        if (cy.hasJIT) {
            new.tempTypeRefs = .{};
            new.tempValueRefs = .{};
            // new.exprResStack = .{};
            // new.exprStack = .{};
            new.llvmFuncs = &.{};
        }
        try new.parser.tokens.ensureTotalCapacityPrecise(c.alloc, 511);
        try new.parser.nodes.ensureTotalCapacityPrecise(c.alloc, 127);
        return new;
    }

    pub fn deinit(self: *Chunk) void {
        self.tempBufU8.deinit(self.alloc);

        for (self.semaSubBlocks.items) |*block| {
            block.deinit(self.alloc);
        }
        self.semaSubBlocks.deinit(self.alloc);

        for (self.semaBlocks.items) |*sblock| {
            sblock.deinit(self.alloc);
        }
        self.semaBlocks.deinit(self.alloc);

        self.blocks.deinit(self.alloc);
        self.subBlocks.deinit(self.alloc);

        self.subBlockJumpStack.deinit(self.alloc);
        self.assignedVarStack.deinit(self.alloc);
        self.varShadowStack.deinit(self.alloc);
        self.varStack.deinit(self.alloc);
        self.preLoopVarSaveStack.deinit(self.alloc);
        self.regStack.deinit(self.alloc);
        self.operandStack.deinit(self.alloc);
        self.unwindTempIndexStack.deinit(self.alloc);
        self.unwindTempRegStack.deinit(self.alloc);
        self.capVarDescs.deinit(self.alloc);

        self.symInitDeps.deinit(self.alloc);
        self.symInitInfos.deinit(self.alloc);
        self.curInitingSymDeps.deinit(self.alloc);

        self.typeStack.deinit(self.alloc);
        self.ir.deinit(self.alloc);
        self.genValueStack.deinit(self.alloc);
        self.dataStack.deinit(self.alloc);
        self.dataU8Stack.deinit(self.alloc);
        self.listDataStack.deinit(self.alloc);
        self.genIrLocalMapStack.deinit(self.alloc);
        self.genLocalStack.deinit(self.alloc);

        if (cy.hasJIT) {
            self.tempTypeRefs.deinit(self.alloc);
            self.tempValueRefs.deinit(self.alloc);
            // self.exprResStack.deinit(self.alloc);
            // self.exprStack.deinit(self.alloc);
            self.alloc.free(self.llvmFuncs);
        }

        self.typeDeps.deinit(self.alloc);
        self.typeDepsMap.deinit(self.alloc);

        self.localSymMap.deinit(self.alloc);
        self.usingModules.deinit(self.alloc);

        // Deinit modSyms declared in this chunk.
        for (self.modSyms.items) |modSym| {
            cy.sym.deinitModSym(self.compiler.vm, modSym);
        }
        self.modSyms.deinit(self.alloc);

        // Deinit the Chunk modSym.
        cy.sym.deinitModSym(self.compiler.vm, @ptrCast(self.sym));
        self.alloc.destroy(self.sym);

        self.alloc.free(self.srcUri);
        self.parser.deinit();
        if (self.srcOwned) {
            self.alloc.free(self.src);
        }
    }

    pub inline fn isInStaticInitializer(self: *Chunk) bool {
        return self.curInitingSym != null;
    }

    pub inline fn semaBlockDepth(self: *Chunk) u32 {
        return @intCast(self.semaBlocks.items.len);
    }

    pub fn reserveIfTempLocal(self: *Chunk, local: LocalId) !void {
        if (self.isTempLocal(local)) {
            try self.setReservedTempLocal(local);
        }
    }

    pub inline fn isTempLocal(self: *const Chunk, local: LocalId) bool {
        return local >= self.rega.tempStart;
    }

    pub inline fn isParamOrLocalVar(self: *const Chunk, reg: u8) bool {
        if (self.blocks.items.len > 1) {
            return reg != 0 and reg < self.rega.tempStart;
        } else {
            return reg < self.rega.tempStart;
        }
    }

    /// TODO: This can be extended to check whether the operands use the dst.
    pub inline fn canUseDstAsTempForBinOp(self: *const Chunk, dst: LocalId) bool {
        return !self.isParamOrLocalVar(dst);
    }

    pub fn initGenValue(self: *const Chunk, local: LocalId, vtype: types.TypeId, retained: bool) bc_gen.GenValue {
        if (self.isTempLocal(local)) {
            return bc_gen.GenValue.initTempValue(local, vtype, retained);
        } else {
            return bc_gen.GenValue.initLocalValue(local, vtype, retained);
        }
    }

    /// Given two local values, determine the next destination temp local.
    /// The type of the dest value is left undefined to be set by caller.
    fn nextTempDestValue(self: *cy.VMcompiler, src1: bc_gen.GenValue, src2: bc_gen.GenValue) !bc_gen.GenValue {
        if (src1.isTempLocal == src2.isTempLocal) {
            if (src1.isTempLocal) {
                const minTempLocal = std.math.min(src1.local, src2.local);
                self.setFirstFreeTempLocal(minTempLocal + 1);
                return bc_gen.GenValue.initTempValue(minTempLocal, undefined);
            } else {
                return bc_gen.GenValue.initTempValue(try self.nextFreeTempLocal(), undefined);
            }
        } else {
            if (src1.isTempLocal) {
                return bc_gen.GenValue.initTempValue(src1.local, undefined);
            } else {
                return bc_gen.GenValue.initTempValue(src2.local, undefined);
            }
        }
    }

    fn genEnsureRequiredType(self: *Chunk, genv: bc_gen.GenValue, requiredType: types.Type) !void {
        if (requiredType.typeT != .any) {
            if (genv.vtype.typeT == requiredType.typeT) {
                return;
            }

            const reqTypeSymId = types.typeToSymbol(requiredType);
            const typeSymId = types.typeToSymbol(genv.vtype);
            if (typeSymId != reqTypeSymId) {
                return self.reportError("Type {} can not be casted to required type {}", &.{v(genv.vtype.typeT), fmt.v(requiredType.typeT)});
            }
        }
    }

    fn canUseVarAsDst(svar: sema.LocalVar) bool {
        // If boxed, the var needs to be copied out of the box.
        // If static selected, the var needs to be copied to a local.
        return !svar.isBoxed and !svar.isStaticAlias;
    }

    /// Checks to see if the ident references a local to avoid a copy to dst.
    fn userLocalOrDst(self: *Chunk, nodeId: cy.NodeId, dst: LocalId, usedDst: *bool) LocalId {
        if (self.nodes[nodeId].node_t == .ident) {
            if (self.genGetVar(self.nodes[nodeId].head.ident.semaVarId)) |svar| {
                if (canUseVarAsDst(svar)) {
                    return svar.local;
                }
            }
        }
        usedDst.* = true;
        return dst;
    }

    pub fn pushTempOperand(self: *Chunk, operand: u8) !void {
        try self.operandStack.append(self.alloc, operand);
    }

    pub fn pushReg(self: *Chunk, reg: u8) !void {
        try self.regStack.append(self.alloc, reg);
    }

    pub fn pushJumpBackNotNone(self: *Chunk, toPc: usize, condLocal: LocalId) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp3(.jumpNotNone, 0, 0, condLocal);
        self.buf.setOpArgU16(pc + 1, @bitCast(-@as(i16, @intCast(pc - toPc))));
    }

    pub fn pushEmptyJumpNone(self: *Chunk, condLocal: LocalId) !u32 {
        const start: u32 = @intCast(self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpNone, 0, 0, condLocal);
        return start;
    }

    pub fn pushEmptyJumpNotNone(self: *Chunk, condLocal: LocalId) !u32 {
        const start: u32 = @intCast(self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpNotNone, 0, 0, condLocal);
        return start;
    }

    pub fn pushEmptyJumpNotCond(self: *Chunk, condLocal: LocalId) !u32 {
        const start: u32 = @intCast(self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpNotCond, condLocal, 0, 0);
        return start;
    }

    pub fn pushJumpBackCond(self: *Chunk, toPc: usize, condLocal: LocalId) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp3(.jumpCond, condLocal, 0, 0);
        self.buf.setOpArgU16(pc + 2, @bitCast(-@as(i16, @intCast(pc - toPc))));
    }

    pub fn pushJumpBackTo(self: *Chunk, toPc: usize) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp2(.jump, 0, 0);
        self.buf.setOpArgU16(pc + 1, @bitCast(-@as(i16, @intCast(pc - toPc))));
    }

    pub fn pushEmptyJump(self: *Chunk) !u32 {
        const start: u32 = @intCast(self.buf.ops.items.len);
        try self.buf.pushOp2(.jump, 0, 0);
        return start;
    }

    pub fn pushEmptyJumpExt(self: *Chunk, desc_: cy.bytecode.InstDesc) !u32 {
        const start: u32 = @intCast(self.buf.ops.items.len);
        try self.buf.pushOp2Ext(.jump, 0, 0, desc_);
        return start;
    }

    pub fn pushEmptyJumpCond(self: *Chunk, condLocal: LocalId) !u32 {
        const start: u32 = @intCast(self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpCond, condLocal, 0, 0);
        return start;
    }

    pub fn patchJumpToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 1, @intCast(self.buf.ops.items.len - jumpPc));
    }

    pub fn patchJumpCondToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 2, @intCast(self.buf.ops.items.len - jumpPc));
    }

    pub fn patchJumpNotCondToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 2, @intCast(self.buf.ops.items.len - jumpPc));
    }

    pub fn patchJumpNoneToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 1, @intCast(self.buf.ops.items.len - jumpPc));
    }

    pub fn patchJumpNotNoneToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 1, @intCast(self.buf.ops.items.len - jumpPc));
    }

    /// Patches sub block breaks. For `if` and `match` blocks.
    /// All other jumps are propagated up the stack by copying to the front.
    /// Returns the adjusted jumpStackStart for this block.
    pub fn patchSubBlockBreakJumps(self: *Chunk, jumpStackStart: usize, breakPc: usize) usize {
        var keepIdx = jumpStackStart;
        for (self.subBlockJumpStack.items[jumpStackStart..]) |jump| {
            if (jump.jumpT == .subBlockBreak) {
                self.buf.setOpArgU16(jump.pc + 1, @intCast(breakPc - jump.pc));
            } else {
                self.subBlockJumpStack.items[keepIdx] = jump;
                keepIdx += 1;
            }
        }
        return keepIdx;
    }

    pub fn patchBreaks(self: *Chunk, jumpStackStart: usize, breakPc: usize) usize {
        var keepIdx = jumpStackStart;
        for (self.subBlockJumpStack.items[jumpStackStart..]) |jump| {
            switch (jump.jumpT) {
                .brk => {
                    if (breakPc > jump.pc) {
                        self.buf.setOpArgU16(jump.pc + 1, @intCast(breakPc - jump.pc));
                    } else {
                        self.buf.setOpArgU16(jump.pc + 1, @bitCast(-@as(i16, @intCast(jump.pc - breakPc))));
                    }
                },
                else => {
                    self.subBlockJumpStack.items[keepIdx] = jump;
                    keepIdx += 1;
                },
            }
        }
        return keepIdx;
    }

    pub fn patchForBlockJumps(self: *Chunk, jumpStackStart: usize, breakPc: usize, contPc: usize) void {
        for (self.subBlockJumpStack.items[jumpStackStart..]) |jump| {
            switch (jump.jumpT) {
                .brk => {
                    if (breakPc > jump.pc) {
                        self.buf.setOpArgU16(jump.pc + 1, @intCast(breakPc - jump.pc));
                    } else {
                        self.buf.setOpArgU16(jump.pc + 1, @bitCast(-@as(i16, @intCast(jump.pc - breakPc))));
                    }
                },
                .cont => {
                    if (contPc > jump.pc) {
                        self.buf.setOpArgU16(jump.pc + 1, @intCast(contPc - jump.pc));
                    } else {
                        self.buf.setOpArgU16(jump.pc + 1, @bitCast(-@as(i16, @intCast(jump.pc - contPc))));
                    }
                },
            }
        }
    }

    pub fn getMaxUsedRegisters(self: *Chunk) u8 {
        return self.rega.maxTemp;
    }

    pub fn blockNumLocals(self: *Chunk) usize {
        return sema.curBlock(self).locals.items.len + sema.curBlock(self).params.items.len;
    }

    pub fn genGetVarPtr(self: *const Chunk, id: sema.LocalVarId) ?*sema.LocalVar {
        if (id != cy.NullId) {
            return &self.vars.items[id];
        } else {
            return null;
        }
    }

    pub fn genGetVar(self: *const Chunk, id: sema.LocalVarId) ?sema.LocalVar {
        if (id != cy.NullId) {
            return self.vars.items[id];
        } else {
            return null;
        }
    }

    pub fn unescapeString(self: *Chunk, literal: []const u8) ![]const u8 {
        try self.tempBufU8.resize(self.alloc, literal.len);
        return cy.sema.unescapeString(self.tempBufU8.items, literal);
    }

    pub fn dumpLocals(self: *const Chunk, block: *sema.Block) !void {
        if (cy.Trace) {
            if (!cy.silentInternal) {
                fmt.printStderr("Locals:\n", &.{});
                const params = sema.getBlockParams(self, block);
                for (params) |svar| {
                    const typeId: types.TypeId = svar.vtype.id;
                    fmt.printStderr("{} (param), local: {}, dyn: {}, rtype: {}, boxed: {}\n", &.{
                        v(svar.name()), v(svar.local), v(svar.vtype.dynamic), v(typeId),
                        v(svar.inner.local.isBoxed),
                    });
                }
                const locals = sema.getBlockLocals(self, block);
                for (locals) |svar| {
                    const typeId: types.TypeId = svar.vtype.id;
                    fmt.printStderr("{}, local: {}, dyn: {}, rtype: {}, boxed: {}\n", &.{
                        v(svar.name()), v(svar.local), v(svar.vtype.dynamic), v(typeId),
                        v(svar.inner.local.isBoxed),
                    });
                }
            }
        }
    }

    pub fn reportErrorMsgAt(self: *Chunk, msg: []const u8, nodeId: cy.NodeId) !void {
        try self.compiler.setErrorAt(self.id, nodeId, msg);
        return error.CompileError;
    }

    pub fn setErrorFmtAt(self: *Chunk, format: []const u8, args: []const fmt.FmtValue, nodeId: cy.NodeId) !void {
        try self.compiler.setErrorFmtAt(self.id, nodeId, format, args);
    }

    // TODO: Rename to reportErrorFmt
    pub fn reportError(self: *Chunk, format: []const u8, args: []const fmt.FmtValue) error{CompileError, OutOfMemory, FormatError} {
        return self.reportErrorAt(format, args, cy.NullId);
    }

    // TODO: Rename to reportErrorFmtAt
    pub fn reportErrorAt(self: *Chunk, format: []const u8, args: []const fmt.FmtValue, nodeId: cy.NodeId) error{CompileError, OutOfMemory, FormatError} {
        try self.setErrorFmtAt(format, args, nodeId);
        return error.CompileError;
    }

    pub fn getNodeStringById(self: *const Chunk, nodeId: cy.NodeId) []const u8 {
        const node = self.nodes[nodeId];
        const token = self.tokens[node.start_token];
        return self.src[token.pos()..token.data.end_pos];
    }

    pub fn getNodeString(self: *const Chunk, node: cy.Node) []const u8 {
        const token = self.tokens[node.start_token];
        return self.src[token.pos()..token.data.end_pos];
    }

    /// An optional debug sym is only included in Trace builds.
    pub fn pushOptionalDebugSym(c: *Chunk, nodeId: cy.NodeId) !void {
        if (cy.Trace or c.compiler.vm.config.genAllDebugSyms) {
            try c.buf.pushFailableDebugSym(
                c.buf.ops.items.len, c.id, nodeId, c.curBlock.frameLoc,
                cy.NullId, 0, 0,
            );
        }
    }

    pub fn getUnwindTempsLen(c: *Chunk) usize {
        return c.unwindTempIndexStack.items.len;
    }

    pub fn pushUnwindTempBoundary(c: *Chunk) !void {
        try c.unwindTempIndexStack.append(c.alloc, @bitCast(@as(u32, cy.NullId)));
        try c.unwindTempRegStack.append(c.alloc, cy.NullU8);
    }

    pub fn pushRetainedTemp(c: *Chunk, reg: u8) !void {
        log.tracev("push unwind temp {} +1 ({})", .{c.unwindTempIndexStack.items.len, reg});
        try c.unwindTempIndexStack.append(c.alloc, .{
            .created = false,
        });
        try c.unwindTempRegStack.append(c.alloc, reg);
    }

    pub fn pushIfRetainedTemp(c: *Chunk, val: bc_gen.GenValue) !bool {
        if (val.isRetainedTemp()) {
            try c.unwindTempIndexStack.append(c.alloc, .{
                .created = false,
            });
            try c.unwindTempRegStack.append(c.alloc, val.local);
            return true;
        }
        return false;
    }

    pub fn popRetainedTemps(self: *Chunk, n: usize) []const cy.register.RegisterId {
        log.tracev("pop unwind temps: {} -{}", .{self.unwindTempIndexStack.items.len, n});
        self.unwindTempIndexStack.items.len -= n;
        defer self.unwindTempRegStack.items.len -= n;
        return self.unwindTempRegStack.items[self.unwindTempRegStack.items.len-n..];
    }

    pub fn popRetainedTemp(self: *Chunk) cy.register.RegisterId {
        log.tracev("pop unwind temp: {} -1", .{self.unwindTempIndexStack.items.len});
        _ = self.unwindTempIndexStack.pop();
        return self.unwindTempRegStack.pop();
    }

    pub fn getLastUnwindTempIndex(self: *Chunk) !u32 {
        const tempIdx = self.unwindTempIndexStack.items[self.unwindTempIndexStack.items.len-1];
        if (@as(u32, @bitCast(tempIdx)) == cy.NullId) {
            return cy.NullId;
        }
        if (!tempIdx.created) {
            var prev: u32 = cy.NullId;
            if (self.unwindTempIndexStack.items.len > 1) {
                const prevIdx = self.unwindTempIndexStack.items[self.unwindTempIndexStack.items.len-2];
                if (@as(u32, @bitCast(prevIdx)) != cy.NullId) {
                    if (!prevIdx.created) {
                        // Multiple uncreated temp indexes. 

                        // Find first uncreated.
                        var first = self.unwindTempIndexStack.items.len-2;
                        while (first > 0) {
                            first -= 1;
                            if (@as(u32, @bitCast(self.unwindTempIndexStack.items[first])) == cy.NullId) {
                                // Block boundary.
                                prev = cy.NullId;
                                first += 1;
                                break;
                            }
                            if (self.unwindTempIndexStack.items[first].created) {
                                // Created.
                                prev = self.unwindTempIndexStack.items[first].idx;
                                first += 1;
                                break;
                            }
                        }

                        for (first..self.unwindTempIndexStack.items.len-1) |i| {
                            const idx = self.buf.unwindTempRegs.items.len;
                            try self.buf.unwindTempRegs.append(self.alloc, self.unwindTempRegStack.items[i]);
                            try self.buf.unwindTempPrevIndexes.append(self.alloc, prev);
                            prev = @intCast(idx);
                        }
                    } else {
                        // Previous index is already created.
                        prev = prevIdx.idx;
                    }
                }
            }

            // Insert unwind temp now that it's needed by a failable inst.
            const idx = self.buf.unwindTempRegs.items.len;
            const reg = self.unwindTempRegStack.items[self.unwindTempRegStack.items.len-1];
            try self.buf.unwindTempRegs.append(self.alloc, reg);
            try self.buf.unwindTempPrevIndexes.append(self.alloc, prev);

            // Update temp index record.
            self.unwindTempIndexStack.items[self.unwindTempIndexStack.items.len-1] = .{
                .created = true,
                .idx = @intCast(idx),
            };
            return @intCast(idx);
        } else {
            return tempIdx.idx;
        }
    }

    pub fn pushFailableDebugSym(self: *Chunk, nodeId: cy.NodeId) !void {
        const unwindTempIdx = try self.getLastUnwindTempIndex();
        try self.buf.pushFailableDebugSym(
            self.buf.ops.items.len, self.id, nodeId, self.curBlock.frameLoc,
            unwindTempIdx, self.curBlock.startLocalReg, self.curBlock.nextLocalReg,
        );
    }

    fn pushFailableDebugSymAt(self: *Chunk, pc: usize, nodeId: cy.NodeId, unwindTempIdx: u32) !void {
        try self.buf.pushFailableDebugSym(pc, self.id, nodeId, self.curBlock.frameLoc, unwindTempIdx);
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

    pub fn desc(self: *Chunk, nodeId: cy.NodeId) cy.bytecode.InstDesc {
        return .{
            .chunkId = self.id,
            .nodeId = nodeId,
            .extraIdx = cy.NullId,
        };
    }

    pub fn descExtra(self: *Chunk, nodeId: cy.NodeId, extraIdx: u32) cy.bytecode.InstDesc {
        return .{
            .chunkId = self.id,
            .nodeId = nodeId,
            .extraIdx = extraIdx,
        };
    }

    pub usingnamespace cy.module.ChunkExt;
    pub usingnamespace cy.types.ChunkExt;
    pub usingnamespace cy.sema.ChunkExt;
    pub usingnamespace jitgen.ChunkExt;
};

test "chunk internals." {
    if (builtin.mode == .ReleaseFast) {
        try t.eq(@sizeOf(Data), 4);
    } else {
        try t.eq(@sizeOf(Data), 4);
    }
}

const SubBlockJumpType = enum {
    /// Breaks out of a for loop, while loop, or switch stmt.
    brk,
    /// Continues a for loop or while loop.
    cont,
};

const SubBlockJump = struct {
    jumpT: SubBlockJumpType,
    pc: u32,
};

const ReservedTempLocal = struct {
    local: LocalId,
};

const LocalId = u8;

const UnwindTempIndex = packed struct {
    idx: u31 = undefined,
    created: bool,
};

pub const SymInitInfo = struct {
    depStart: u32,
    depEnd: u32,
    irStart: u32,
    irEnd: u32,
    visiting: bool = false,
    visited: bool = false,
};

const SymInitDep = struct {
    sym: *cy.Sym,
    refNodeId: cy.NodeId,
};

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
    nodeId: cy.NodeId,
    constIdx: u16,
    jumpToEndPc: u32,
};

pub const DataU8 = union {
    irLocal: u8,
};

pub const Data = union {
    placeholder: u32,
};

pub fn pushData(c: *cy.Chunk, data: Data) !void {
    try c.dataStack.append(c.alloc, data);
}

pub fn popData(c: *cy.Chunk) Data {
    return c.dataStack.pop();
}