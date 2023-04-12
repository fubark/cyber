const std = @import("std");
const stdx = @import("stdx");
const builtin = @import("builtin");
const cy = @import("cyber.zig");
const fmt = cy.fmt;
const v = fmt.v;
const sema = cy.sema;
const types = cy.types;
const gen = cy.codegen;

pub const ChunkId = u32;

/// A compilation unit.
/// It contains data to compile from source into a module with exported symbols.
pub const Chunk = struct {
    id: ChunkId,
    alloc: std.mem.Allocator,
    compiler: *cy.VMcompiler,

    /// Source code.
    src: []const u8,

    /// Absolute path to source.
    srcUri: []const u8,

    parser: cy.Parser,
    parserAstRootId: cy.NodeId,

    /// Generic linked list buffer.
    dataNodes: std.ArrayListUnmanaged(DataNode),

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
    vars: std.ArrayListUnmanaged(sema.LocalVar),
    capVarDescs: std.AutoHashMapUnmanaged(sema.LocalVarId, sema.CapVarDesc),

    /// List of func decls.
    /// They are resolved after data types.
    semaFuncDecls: std.ArrayListUnmanaged(sema.FuncDecl),

    /// Additional info for initializer symbols.
    semaInitializerSyms: std.AutoArrayHashMapUnmanaged(sema.CompactResolvedSymId, sema.InitializerSym),

    assignedVarStack: std.ArrayListUnmanaged(sema.LocalVarId),
    curSemaBlockId: sema.BlockId,
    curSemaSubBlockId: sema.SubBlockId,

    /// Which sema sym var is currently being analyzed for an assignment initializer.
    curSemaInitingSym: sema.CompactResolvedSymId,

    /// When looking at a var declaration, keep track of which symbols are already recorded as dependencies.
    semaVarDeclDeps: std.AutoHashMapUnmanaged(sema.CompactResolvedSymId, void),

    /// Currently used to store lists of static var dependencies.
    bufU32: std.ArrayListUnmanaged(u32),

    /// The resolved sym id of this chunk.
    semaResolvedRootSymId: sema.ResolvedSymId,

    /// Current block stack.
    semaBlockStack: std.ArrayListUnmanaged(sema.BlockId),

    /// Main sema block id.
    mainSemaBlockId: sema.BlockId,

    /// Local syms is used as a cache to sema.resolvedSyms.
    /// It's useful to store imports, importAlls that are only visible to the module.
    localSyms: std.HashMapUnmanaged(sema.RelLocalSymKey, sema.LocalSym, cy.hash.KeyU64Context, 80),

    ///
    /// Codegen pass
    ///
    blocks: std.ArrayListUnmanaged(GenBlock),
    blockJumpStack: std.ArrayListUnmanaged(BlockJump),
    subBlockJumpStack: std.ArrayListUnmanaged(SubBlockJump),

    /// Tracks which temp locals are reserved. They are skipped for temp local allocations.
    reservedTempLocalStack: std.ArrayListUnmanaged(ReservedTempLocal),

    operandStack: std.ArrayListUnmanaged(cy.InstDatum),

    /// Used to advance to the next saved sema sub block.
    nextSemaSubBlockId: u32,
    curBlock: *GenBlock,

    /// Shared final code buffer.
    buf: *cy.ByteCodeBuffer,

    nodes: []cy.Node,
    tokens: []const cy.Token,

    /// Whether the src is owned by the chunk.
    srcOwned: bool,

    /// Points to this chunk's `Module`.
    /// Its exported members will be populated in the Module as sema encounters them.
    modId: sema.ModuleId,

    pub fn init(c: *cy.VMcompiler, id: ChunkId, srcUri: []const u8, src: []const u8) !Chunk {
        var new = Chunk{
            .id = id,
            .alloc = c.alloc,
            .compiler = c,
            .src = src,
            .srcUri = srcUri,
            .parser = cy.Parser.init(c.alloc),
            .parserAstRootId = cy.NullId,
            .nodes = undefined,
            .tokens = undefined,
            .semaBlocks = .{},
            .semaSubBlocks = .{},
            .semaInitializerSyms = .{},
            .vars = .{},
            .capVarDescs = .{},
            .blocks = .{},
            .blockJumpStack = .{},
            .subBlockJumpStack = .{},
            .reservedTempLocalStack = .{},
            .assignedVarStack = .{},
            .operandStack = .{},
            .curBlock = undefined,
            .curSemaBlockId = undefined,
            .curSemaSubBlockId = undefined,
            .nextSemaSubBlockId = undefined,
            .buf = undefined,
            .curNodeId = cy.NullId,
            .curSemaInitingSym = @bitCast(sema.CompactResolvedSymId, @as(u32, cy.NullId)),
            .semaVarDeclDeps = .{},
            .bufU32 = .{},
            .dataNodes = .{},
            .tempBufU8 = .{},
            .srcOwned = false,
            .modId = cy.NullId,
            .semaResolvedRootSymId = cy.NullId,
            .semaBlockStack = .{},
            .mainSemaBlockId = cy.NullId,
            .semaFuncDecls = .{},
            .localSyms = .{},
        };
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
        self.semaBlockStack.deinit(self.alloc);

        self.blocks.deinit(self.alloc);

        self.bufU32.deinit(self.alloc);
        self.semaVarDeclDeps.deinit(self.alloc);
        self.dataNodes.deinit(self.alloc);

        self.blockJumpStack.deinit(self.alloc);
        self.subBlockJumpStack.deinit(self.alloc);
        self.assignedVarStack.deinit(self.alloc);
        self.operandStack.deinit(self.alloc);
        self.reservedTempLocalStack.deinit(self.alloc);
        self.vars.deinit(self.alloc);
        self.capVarDescs.deinit(self.alloc);

        self.semaInitializerSyms.deinit(self.alloc);

        self.parser.deinit();
        if (self.srcOwned) {
            self.alloc.free(self.src);
        }

        self.semaFuncDecls.deinit(self.alloc);
        self.localSyms.deinit(self.alloc);
    }

    pub inline fn isInStaticInitializer(self: *Chunk) bool {
        return self.curSemaInitingSym.isPresent();
    }

    /// Assumes `semaBlockStack` has a dummy head element. Main block starts at 1.
    pub inline fn semaBlockDepth(self: *Chunk) u32 {
        return @intCast(u32, self.semaBlockStack.items.len-1);
    }

    pub fn pushSemaBlock(self: *Chunk, id: sema.BlockId) !void {
        // Codegen block should be pushed first so nextSemaSubBlock can use it.
        try self.pushBlock();

        try self.semaBlockStack.append(self.alloc, id);
        self.curSemaBlockId = id;
        self.nextSemaSubBlockId = self.semaBlocks.items[id].firstSubBlockId;
        self.nextSemaSubBlock();
    }

    pub fn popSemaBlock(self: *Chunk) void {
        self.semaBlockStack.items.len -= 1;
        self.curSemaBlockId = self.semaBlockStack.items[self.semaBlockStack.items.len-1];
        self.prevSemaSubBlock();

        self.popBlock();
    }

    pub fn reserveIfTempLocal(self: *Chunk, local: LocalId) !void {
        if (self.isTempLocal(local)) {
            try self.setReservedTempLocal(local);
        }
    }

    pub fn setReservedTempLocal(self: *Chunk, local: LocalId) !void {
        // log.debug("set reserved {}", .{self.reservedTempLocalStack.items.len});
        try self.reservedTempLocalStack.append(self.alloc, .{
            .local = local,
        });
    }

    pub fn canUseLocalAsTemp(self: *const Chunk, local: LocalId) bool {
        if (self.blocks.items.len > 1) {
            // Temp or return slot.
            return local == 0 or local >= self.curBlock.numLocals;
        } else {
            // For main block, it can only use local as a temporary if it's in fact a temp.
            return local >= self.curBlock.numLocals;
        }
    }

    pub inline fn isTempLocal(self: *const Chunk, local: LocalId) bool {
        return local >= self.curBlock.numLocals;
    }

    pub fn initGenValue(self: *const Chunk, local: LocalId, vtype: types.Type, retained: bool) gen.GenValue {
        if (self.isTempLocal(local)) {
            return gen.GenValue.initTempValue(local, vtype, retained);
        } else {
            return gen.GenValue.initLocalValue(local, vtype, retained);
        }
    }

    /// TODO: Rename to reserveNextTempLocal.
    /// Assumes that if `firstFreeTempLocal` is in bounds, it is free.
    pub fn nextFreeTempLocal(self: *Chunk) !LocalId {
        if (self.curBlock.firstFreeTempLocal < 256) {
            if (self.curBlock.firstFreeTempLocal == self.curBlock.numLocals + self.curBlock.numTempLocals) {
                self.curBlock.numTempLocals += 1;
            }
            defer {
                // Advance to the next free temp considering reserved arc temps.
                self.computeNextTempLocalFrom(self.curBlock.firstFreeTempLocal + 1);
            }
            return @intCast(u8, self.curBlock.firstFreeTempLocal);
        } else {
            return self.reportError("Exceeded max locals.", &.{});
        }
    }

    pub fn computeNextTempLocalFrom(self: *Chunk, local: LocalId) void {
        self.curBlock.firstFreeTempLocal = local;
        if (self.curBlock.reservedTempLocalStart < self.reservedTempLocalStack.items.len) {
            while (self.isReservedTempLocal(self.curBlock.firstFreeTempLocal)) {
                self.curBlock.firstFreeTempLocal += 1;
            }
        }
    }

    /// Find first available temp starting from the beginning.
    pub fn resetNextFreeTemp(self: *Chunk) void {
        self.computeNextTempLocalFrom(@intCast(u8, self.curBlock.numLocals));
    }

    pub fn setFirstFreeTempLocal(self: *Chunk, local: LocalId) void {
        self.curBlock.firstFreeTempLocal = local;
    }

    /// Given two local values, determine the next destination temp local.
    /// The type of the dest value is left undefined to be set by caller.
    fn nextTempDestValue(self: *cy.VMcompiler, src1: gen.GenValue, src2: gen.GenValue) !gen.GenValue {
        if (src1.isTempLocal == src2.isTempLocal) {
            if (src1.isTempLocal) {
                const minTempLocal = std.math.min(src1.local, src2.local);
                self.setFirstFreeTempLocal(minTempLocal + 1);
                return gen.GenValue.initTempValue(minTempLocal, undefined);
            } else {
                return gen.GenValue.initTempValue(try self.nextFreeTempLocal(), undefined);
            }
        } else {
            if (src1.isTempLocal) {
                return gen.GenValue.initTempValue(src1.local, undefined);
            } else {
                return gen.GenValue.initTempValue(src2.local, undefined);
            }
        }
    }

    pub fn genExpr(self: *Chunk, nodeId: cy.NodeId, comptime discardTopExprReg: bool) anyerror!gen.GenValue {
        self.curNodeId = nodeId;
        return self.genExpr2(nodeId, types.AnyType, discardTopExprReg);
    }

    /// If the expression is a user local, the local is returned.
    /// Otherwise, the expression is allocated a temp local on the stack.
    fn genExpr2(self: *Chunk, nodeId: cy.NodeId, requiredType: types.Type, comptime discardTopExprReg: bool) anyerror!gen.GenValue {
        const dst = try self.userLocalOrNextTempLocal(nodeId);
        const res = try gen.genExprTo2(self, nodeId, dst, requiredType, false, !discardTopExprReg);
        try self.genEnsureRequiredType(res, requiredType);
        return res;
    }

    pub fn genExprTo(self: *Chunk, nodeId: cy.NodeId, dst: LocalId, retainEscapeTop: bool, comptime discardTopExprReg: bool) anyerror!gen.GenValue {
        return gen.genExprTo2(self, nodeId, dst, types.AnyType, retainEscapeTop, !discardTopExprReg);
    }

    fn genEnsureRequiredType(self: *Chunk, genValue: gen.GenValue, requiredType: types.Type) !void {
        if (requiredType.typeT != .any) {
            if (genValue.vtype.typeT == requiredType.typeT) {
                return;
            }

            const reqTypeSymId = types.typeToResolvedSym(requiredType);
            const typeSymId = types.typeToResolvedSym(genValue.vtype);
            if (typeSymId != reqTypeSymId) {
                return self.reportError("Type {} can not be casted to required type {}", &.{fmt.v(genValue.vtype.typeT), fmt.v(requiredType.typeT)});
            }
        }
    }

    pub fn genExprPreferLocalOrReplaceableDest(self: *Chunk, nodeId: cy.NodeId, dst: LocalId, usedDst: *bool, comptime discardTopExprReg: bool) !gen.GenValue {
        return self.genExprPreferLocalOrReplaceableDest2(types.AnyType, nodeId, dst, usedDst, discardTopExprReg);
    }

    /// Prefers a local.
    /// Then prefers unused dest if the dest can be replaced without ARC release.
    /// Otherwise, a temp is used.
    pub fn genExprPreferLocalOrReplaceableDest2(self: *Chunk, requestedType: types.Type, nodeId: cy.NodeId, dst: LocalId, usedDst: *bool, comptime discardTopExprReg: bool) !gen.GenValue {
        const node = self.nodes[nodeId];
        if (node.node_t == .ident) {
            if (self.genGetVar(node.head.ident.semaVarId)) |svar| {
                if (canUseVarAsDst(svar)) {
                    return gen.genExprTo2(self, nodeId, svar.local, requestedType, false, !discardTopExprReg);
                }
            }
        }
        if (usedDst.*) {
            const finalDst = try self.nextFreeTempLocal();
            return gen.genExprTo2(self, nodeId, finalDst, requestedType, false, !discardTopExprReg);
        } else {
            // TODO: Handle other expressions that can save to dst.
            var useDst = false;
            switch (node.node_t) {
                .number,
                .false_literal,
                .true_literal => useDst = true,
                else => {}
            }
            if (useDst) {
                usedDst.* = true;
                return gen.genExprTo2(self, nodeId, dst, requestedType, false, !discardTopExprReg);
            } else {
                const finalDst = try self.nextFreeTempLocal();
                return gen.genExprTo2(self, nodeId, finalDst, requestedType, false, !discardTopExprReg);
            }
        }
    }

    pub fn genExprToDestOrTempLocal(self: *Chunk, nodeId: cy.NodeId, dst: LocalId, usedDst: *bool, comptime discardTopExprReg: bool) !gen.GenValue {
        return self.genExprToDestOrTempLocal2(types.AnyType, nodeId, dst, usedDst, discardTopExprReg);
    }

    /// Attempts to gen expression to the destination if it can avoid a retain op.
    /// Otherwise, it is copied to a temp.
    pub fn genExprToDestOrTempLocal2(self: *Chunk, requestedType: types.Type, nodeId: cy.NodeId, dst: LocalId, usedDst: *bool, comptime discardTopExprReg: bool) !gen.GenValue {
        const node = self.nodes[nodeId];
        if (genWillAlwaysRetainNode(self, node) or usedDst.*) {
            const finalDst = try self.userLocalOrNextTempLocal(nodeId);
            return gen.genExprTo2(self, nodeId, finalDst, requestedType, false, !discardTopExprReg);
        } else {
            const finalDst = self.userLocalOrDst(nodeId, dst, usedDst);
            return gen.genExprTo2(self, nodeId, finalDst, requestedType, false, !discardTopExprReg);
        }
    }

    /// Generates an instruction to copy the root expression to a specific destination local.
    /// Also ensures that the expression is retained.
    pub fn genRetainedExprTo(self: *Chunk, nodeId: cy.NodeId, dst: LocalId, comptime discardTopExprReg: bool) anyerror!gen.GenValue {
        return try self.genExprTo(nodeId, dst, true, discardTopExprReg);
    }

    pub fn genRetainedTempExpr(self: *Chunk, nodeId: cy.NodeId, comptime discardTopExprReg: bool) !gen.GenValue {
        return self.genRetainedTempExpr2(nodeId, types.AnyType, discardTopExprReg);
    }

    /// Ensures that the expr value is retained and ends up in the next temp local.
    pub fn genRetainedTempExpr2(self: *Chunk, nodeId: cy.NodeId, requiredType: types.Type, comptime discardTopExprReg: bool) anyerror!gen.GenValue {
        const dst = try self.nextFreeTempLocal();
        // ARC temps released at the end of this expr,
        // so the next free temp is guaranteed to be after dst.
        defer self.setFirstFreeTempLocal(dst + 1);

        const val = try gen.genExprTo2(self, nodeId, dst, requiredType, true, !discardTopExprReg);
        try self.genEnsureRequiredType(val, requiredType);
        return val;
    }

    fn isReservedTempLocal(self: *const Chunk, local: LocalId) bool {
        for (self.reservedTempLocalStack.items[self.curBlock.reservedTempLocalStart..]) |temp| {
            if (temp.local == local) {
                return true;
            }
        }
        return false;
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

    fn userLocalOrNextTempLocal(self: *Chunk, nodeId: cy.NodeId) !LocalId {
        const node = self.nodes[nodeId];
        if (node.node_t == .ident) {
            if (self.genGetVar(self.nodes[nodeId].head.ident.semaVarId)) |svar| {
                if (canUseVarAsDst(svar)) {
                    return svar.local;
                }
            }
        } else if (node.node_t == .callExpr) {
            // Since call expr args allocate arg locals past the arc temps,
            // select the call dst to be past the arc temps to skip generating an extra copy op.
            _ = self.advanceNextTempLocalPastReservedTemps();
            return self.nextFreeTempLocal();
        }
        return self.nextFreeTempLocal();
    }

    pub fn advanceNextTempLocalPastReservedTemps(self: *Chunk) LocalId {
        if (self.curBlock.reservedTempLocalStart < self.reservedTempLocalStack.items.len) {
            for (self.reservedTempLocalStack.items[self.curBlock.reservedTempLocalStart..]) |temp| {
                if (self.curBlock.firstFreeTempLocal <= temp.local) {
                    self.curBlock.firstFreeTempLocal = temp.local + 1;
                }
            }
        }
        return self.curBlock.firstFreeTempLocal;
    }

    pub fn pushTempOperand(self: *Chunk, operand: u8) !void {
        try self.operandStack.append(self.alloc, cy.InstDatum.initArg(operand));
    }

    pub fn reserveLocal(self: *Chunk, block: *GenBlock) !u8 {
        const idx = block.numLocals;
        block.numLocals += 1;
        if (idx <= std.math.maxInt(u8)) {
            return @intCast(u8, idx);
        } else {
            return self.reportError("Exceeded max local count: {}", &.{v(@as(u8, std.math.maxInt(u8)))});
        }
    }

    /// Reserve params and captured vars.
    /// Function stack layout:
    /// [startLocal/retLocal] [retInfo] [retAddress] [prevFramePtr] [params...] [callee] [var locals...] [temp locals...]
    /// `callee` is reserved so that function values can call static functions with the same call convention.
    /// For this reason, `callee` isn't freed in the function body and a separate release inst is required for lambda calls.
    /// A closure can also occupy the callee and is used to do captured var lookup.
    pub fn reserveFuncParams(self: *Chunk, numParams: u32) !void {
        // First local is reserved for a single return value.
        _ = try self.reserveLocal(self.curBlock);

        // Second local is reserved for the return info.
        _ = try self.reserveLocal(self.curBlock);

        // Third local is reserved for the return address.
        _ = try self.reserveLocal(self.curBlock);

        // Fourth local is reserved for the previous frame pointer.
        _ = try self.reserveLocal(self.curBlock);

        const sblock = sema.curBlock(self);

        // Reserve func params.
        for (sblock.params.items[0..numParams]) |varId| {
            _ = try self.reserveLocalVar(varId);

            // Params are already defined.
            self.vars.items[varId].genIsDefined = true;
        }

        // An extra callee slot is reserved so that function values
        // can call static functions with the same call convention.
        _ = try self.reserveLocal(self.curBlock);

        if (sblock.params.items.len > numParams) {
            for (sblock.params.items[numParams..]) |varId| {
                _ = try self.reserveLocalVar(varId);

                // Params are already defined.
                self.vars.items[varId].genIsDefined = true;
            }
        }
    }

    pub fn genEnsureRtFuncSym(self: *Chunk, rFuncSymId: sema.ResolvedFuncSymId) !u32 {
        const rFuncSym = self.compiler.sema.getResolvedFuncSym(rFuncSymId);
        const rSym = self.compiler.sema.getResolvedSym(rFuncSym.getResolvedSymId());
        const key = rSym.key.absResolvedSymKey;
        const rFuncSigId = rFuncSym.getResolvedFuncSigId();
        return self.compiler.vm.ensureFuncSym(key.rParentSymId, key.nameId, rFuncSigId);
    }

    pub fn genGetResolvedFuncSym(self: *const Chunk, rSymId: sema.ResolvedSymId, rFuncSigId: sema.ResolvedFuncSigId) ?sema.ResolvedFuncSym {
        const key = sema.AbsResolvedSymKey{
            .absResolvedFuncSymKey = .{
                .rSymId = rSymId,
                .rFuncSigId = rFuncSigId,
            },
        };
        if (self.compiler.semaResolvedFuncSymMap.get(key)) |id| {
            return self.compiler.sema.resolvedFuncSyms.items[id];
        } else {
            return null;
        }
    }

    pub fn genGetResolvedSymId(self: *const Chunk, semaSymId: sema.SymId) ?sema.ResolvedSymId {
        const sym = self.semaSyms.items[semaSymId];
        if (sym.rSymId != cy.NullId) {
            return sym.rSymId;
        } else {
            return null;
        }
    }

    pub fn genGetResolvedSym(self: *const Chunk, semaSymId: sema.SymId) ?sema.ResolvedSym {
        if (semaSymId != cy.NullId) {
            const sym = self.semaSyms.items[semaSymId];
            if (sym.rSymId != cy.NullId) {
                return self.compiler.sema.resolvedSyms.items[sym.rSymId];
            }
        }
        return null;
    }

    pub fn genBlockEnding(self: *Chunk) !void {
        self.curBlock.endLocalsPc = @intCast(u32, self.buf.ops.items.len);
        try self.endLocals();
        if (self.curBlock.requiresEndingRet1) {
            try self.buf.pushOp(.ret1);
        } else {
            try self.buf.pushOp(.ret0);
        }
    }

    pub fn endLocals(self: *Chunk) !void {
        const sblock = sema.curBlock(self);

        const start = self.operandStack.items.len;
        defer self.operandStack.items.len = start;

        for (sblock.params.items) |varId| {
            const svar = self.vars.items[varId];
            if (svar.lifetimeRcCandidate and !svar.isCaptured()) {
                try self.operandStack.append(self.alloc, cy.InstDatum.initArg(svar.local));
            }
        }
        for (sblock.locals.items) |varId| {
            const svar = self.vars.items[varId];
            if (svar.lifetimeRcCandidate and svar.genIsDefined) {
                try self.operandStack.append(self.alloc, cy.InstDatum.initArg(svar.local));
            }
        }
        
        const locals = self.operandStack.items[start..];
        if (locals.len > 0) {
            const nodeId = sema.getBlockNodeId(self, sblock);
            try self.pushOptionalDebugSym(nodeId);
            if (locals.len == 1) {
                try self.buf.pushOp1(.release, locals[0].arg);
            } else {
                try self.buf.pushOp1(.releaseN, @intCast(u8, locals.len));
                try self.buf.pushOperands(locals);
            }
        }
    }

    pub fn pushJumpBackNotNone(self: *Chunk, toPc: usize, condLocal: LocalId) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp3(.jumpNotNone, 0, 0, condLocal);
        self.buf.setOpArgU16(pc + 1, @bitCast(u16, -@intCast(i16, pc - toPc)));
    }

    pub fn pushEmptyJumpNotNone(self: *Chunk, condLocal: LocalId) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpNotNone, 0, 0, condLocal);
        return start;
    }

    pub fn pushEmptyJumpNotCond(self: *Chunk, condLocal: LocalId) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpNotCond, condLocal, 0, 0);
        return start;
    }

    pub fn pushJumpBackCond(self: *Chunk, toPc: usize, condLocal: LocalId) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp3(.jumpCond, 0, 0, condLocal);
        self.buf.setOpArgU16(pc + 1, @bitCast(u16, -@intCast(i16, pc - toPc)));
    }

    pub fn pushJumpBackTo(self: *Chunk, toPc: usize) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp2(.jump, 0, 0);
        self.buf.setOpArgU16(pc + 1, @bitCast(u16, -@intCast(i16, pc - toPc)));
    }

    pub fn pushEmptyJump(self: *Chunk) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp2(.jump, 0, 0);
        return start;
    }

    pub fn pushEmptyJumpCond(self: *Chunk, condLocal: LocalId) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpCond, 0, 0, condLocal);
        return start;
    }

    pub fn patchJumpToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 1, @intCast(u16, self.buf.ops.items.len - jumpPc));
    }

    pub fn patchJumpCondToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 1, @intCast(u16, self.buf.ops.items.len - jumpPc));
    }

    pub fn patchJumpNotCondToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 2, @intCast(u16, self.buf.ops.items.len - jumpPc));
    }

    pub fn patchJumpNotNoneToCurPc(self: *Chunk, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 1, @intCast(u16, self.buf.ops.items.len - jumpPc));
    }

    /// Patches sub block breaks. For `if` and `match` blocks.
    /// All other jumps are propagated up the stack by copying to the front.
    /// Returns the adjusted jumpStackStart for this block.
    pub fn patchSubBlockBreakJumps(self: *Chunk, jumpStackStart: usize, breakPc: usize) usize {
        var propagateIdx = jumpStackStart;
        for (self.subBlockJumpStack.items[jumpStackStart..]) |jump| {
            if (jump.jumpT == .subBlockBreak) {
                self.buf.setOpArgU16(jump.pc + 1, @intCast(u16, breakPc - jump.pc));
            } else {
                self.subBlockJumpStack.items[propagateIdx] = jump;
                propagateIdx += 1;
            }
        }
        return propagateIdx;
    }

    pub fn patchForBlockJumps(self: *Chunk, jumpStackStart: usize, breakPc: usize, contPc: usize) void {
        for (self.subBlockJumpStack.items[jumpStackStart..]) |jump| {
            switch (jump.jumpT) {
                .subBlockBreak => {
                    stdx.panicFmt("Unexpected jump.", .{});
                },
                .brk => {
                    if (breakPc > jump.pc) {
                        self.buf.setOpArgU16(jump.pc + 1, @intCast(u16, breakPc - jump.pc));
                    } else {
                        self.buf.setOpArgU16(jump.pc + 1, @bitCast(u16, -@intCast(i16, jump.pc - breakPc)));
                    }
                },
                .cont => {
                    if (contPc > jump.pc) {
                        self.buf.setOpArgU16(jump.pc + 1, @intCast(u16, contPc - jump.pc));
                    } else {
                        self.buf.setOpArgU16(jump.pc + 1, @bitCast(u16, -@intCast(i16, jump.pc - contPc)));
                    }
                },
            }
        }
    }

    pub fn patchBlockJumps(self: *Chunk, jumpStackStart: usize) void {
        for (self.blockJumpStack.items[jumpStackStart..]) |jump| {
            switch (jump.jumpT) {
                .jumpToEndLocals => {
                    self.buf.setOpArgU16(jump.pc + jump.pcOffset, @intCast(u16, self.curBlock.endLocalsPc - jump.pc));
                }
            }
        }
    }

    pub fn pushBlock(self: *Chunk) !void {
        try self.blocks.append(self.alloc, GenBlock.init());
        self.curBlock = &self.blocks.items[self.blocks.items.len-1];
        self.curBlock.reservedTempLocalStart = @intCast(u32, self.reservedTempLocalStack.items.len);
    }

    pub fn popBlock(self: *Chunk) void {
        var last = self.blocks.pop();
        self.reservedTempLocalStack.items.len = last.reservedTempLocalStart;
        last.deinit(self.alloc);
        if (self.blocks.items.len > 0) {
            self.curBlock = &self.blocks.items[self.blocks.items.len-1];
        }
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

    pub fn reserveLocalVar(self: *Chunk, varId: sema.LocalVarId) !LocalId {
        const local = try self.reserveLocal(self.curBlock);
        self.vars.items[varId].local = local;
        return local;
    }

    pub fn nextSemaSubBlock(self: *Chunk) void {
        self.curSemaSubBlockId = self.nextSemaSubBlockId;
        self.nextSemaSubBlockId += 1;

        const ssblock = sema.curSubBlock(self);
        for (ssblock.iterVarBeginTypes.items) |varAndType| {
            const svar = &self.vars.items[varAndType.id];
            // log.debug("{s} iter var", .{self.getVarName(varAndType.id)});
            svar.vtype = varAndType.vtype;
            svar.genIsDefined = true;
        }
    }

    pub fn prevSemaSubBlock(self: *Chunk) void {
        const ssblock = sema.curSubBlock(self);
        self.curSemaSubBlockId = ssblock.prevSubBlockId;

        // Update narrow types.
        for (ssblock.endMergeTypes.items) |it| {
            self.vars.items[it.id].vtype = it.vtype;
        }
    }

    pub fn unescapeString(self: *Chunk, literal: []const u8) ![]const u8 {
        try self.tempBufU8.resize(self.alloc, literal.len);
        return cy.sema.unescapeString(self.tempBufU8.items, literal);
    }

    pub fn dumpLocals(self: *const Chunk, sblock: *sema.Block) !void {
        if (builtin.mode == .Debug and !cy.silentInternal) {
            fmt.printStderr("Locals:\n", &.{});
            for (sblock.params.items) |varId| {
                const svar = self.vars.items[varId];
                fmt.printStderr("{} (param), local: {}, curType: {}, rc: {}, lrc: {}, boxed: {}, capIdx: {}\n", &.{
                    v(svar.name), v(svar.local), v(svar.vtype.typeT),
                    v(svar.vtype.rcCandidate), v(svar.lifetimeRcCandidate), v(svar.isBoxed), v(svar.capturedIdx),
                });
            }
            for (sblock.locals.items) |varId| {
                const svar = self.vars.items[varId];
                fmt.printStderr("{}, local: {}, curType: {}, rc: {}, lrc: {}, boxed: {}, capIdx: {}\n", &.{
                    v(svar.name), v(svar.local), v(svar.vtype.typeT),
                    v(svar.vtype.rcCandidate), v(svar.lifetimeRcCandidate), v(svar.isBoxed), v(svar.capturedIdx),
                });
            }
        }
    }

    pub fn setErrorAt(self: *Chunk, format: []const u8, args: []const fmt.FmtValue, nodeId: cy.NodeId) !void {
        self.alloc.free(self.compiler.lastErr);
        self.compiler.lastErr = try fmt.allocFormat(self.alloc, format, args);
        self.compiler.lastErrNode = nodeId;
        self.compiler.lastErrChunk = self.id;
    }

    pub fn reportError(self: *Chunk, format: []const u8, args: []const fmt.FmtValue) error{CompileError, OutOfMemory, FormatError} {
        return self.reportErrorAt(format, args, self.curNodeId);
    }

    pub fn reportErrorAt(self: *Chunk, format: []const u8, args: []const fmt.FmtValue, nodeId: cy.NodeId) error{CompileError, OutOfMemory, FormatError} {
        try self.setErrorAt(format, args, nodeId);
        return error.CompileError;
    }

    pub fn getNodeTokenString(self: *const Chunk, node: cy.Node) []const u8 {
        const token = self.tokens[node.start_token];
        return self.src[token.pos()..token.data.end_pos];
    }

    /// An optional debug sym is only included in Debug builds.
    pub fn pushOptionalDebugSym(self: *Chunk, nodeId: cy.NodeId) !void {
        if (builtin.mode == .Debug or self.compiler.vm.config.genAllDebugSyms) {
            try self.buf.pushDebugSym(self.buf.ops.items.len, self.id, nodeId, self.curBlock.frameLoc);
        }
    }

    pub fn pushDebugSym(self: *Chunk, nodeId: cy.NodeId) !void {
        try self.buf.pushDebugSym(self.buf.ops.items.len, self.id, nodeId, self.curBlock.frameLoc);
    }

    fn pushDebugSymAt(self: *Chunk, pc: usize, nodeId: cy.NodeId) !void {
        try self.buf.pushDebugSym(pc, self.id, nodeId, self.curBlock.frameLoc);
    }

    pub fn getModule(self: *Chunk) *sema.Module {
        return &self.compiler.sema.modules.items[self.modId];
    }
};

/// Some nodes will always retain in order to behave correctly under ARC.
/// The temp register allocator needs to know this ahead of time to determine the dst register.
fn genWillAlwaysRetainNode(c: *cy.Chunk, node: cy.Node) bool {
    switch (node.node_t) {
        .callExpr,
        .arr_literal,
        .map_literal,
        .stringTemplate,
        .arr_access_expr,
        .coinit,
        .objectInit => return true,
        .accessExpr => {
            if (node.head.accessExpr.sema_crSymId.isPresent()) {
                if (willAlwaysRetainResolvedSym(c, node.head.accessExpr.sema_crSymId)) {
                    return true;
                }
            }
            return false;
        },
        .ident => {
            if (node.head.ident.semaVarId != cy.NullId) {
                const svar = c.genGetVar(node.head.ident.semaVarId).?;
                if (svar.isStaticAlias) {
                    if (willAlwaysRetainResolvedSym(c, svar.inner.staticAlias.crSymId)) {
                        return true;
                    }
                }
            }
            if (node.head.ident.sema_crSymId.isPresent()) {
                if (willAlwaysRetainResolvedSym(c, node.head.ident.sema_crSymId)) {
                    return true;
                }
            }
            return false;
        },
        else => return false,
    }
}

fn willAlwaysRetainResolvedSym(c: *cy.Chunk, crSymId: sema.CompactResolvedSymId) bool {
    if (crSymId.isFuncSymId) {
        // `staticFunc` op is always retained.
        return true;
    } else {
        const rSym = c.compiler.sema.getResolvedSym(crSymId.id);
        switch (rSym.symT) {
            .variable => {
                // Since `staticVar` op is always retained atm.
                return true;
            },
            .object => {
                // `sym` op is always retained.
                return true;
            },
            .builtinType => {
                return true;
            },
            else => return false,
        }
    }
}

const DataNode = packed struct {
    inner: packed union {
        funcSym: packed struct {
            symId: u32,
        },
    },
    next: u32,
};

const GenBlock = struct {
    /// This includes the return info, function params, captured params, and local vars.
    /// Does not include temp locals.
    numLocals: u32,
    frameLoc: cy.NodeId = cy.NullId,
    rFuncSymId: sema.ResolvedFuncSymId = cy.NullId,
    endLocalsPc: u32,

    /// These are used for rvalues and function args.
    /// At the end of the block, the total stack size needed for the function body is known.
    numTempLocals: u8,

    /// Starts at `numLocals`.
    /// Temp locals are allocated from the end of the user locals towards the right.
    firstFreeTempLocal: u8,

    /// Start of the first reserved temp local.
    reservedTempLocalStart: u32,

    /// Whether codegen should create an ending that returns 1 arg.
    /// Otherwise `ret0` is generated.
    requiresEndingRet1: bool,

    /// If the function body belongs to a closure, this local
    /// contains the closure's value which is then used to perform captured var lookup.
    closureLocal: u8,

    fn init() GenBlock {
        return .{
            .numLocals = 0,
            .endLocalsPc = 0,
            .numTempLocals = 0,
            .firstFreeTempLocal = 0,
            .reservedTempLocalStart = 0,
            .requiresEndingRet1 = false,
            .closureLocal = cy.NullU8,
        };
    }

    fn deinit(self: *GenBlock, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }

    pub fn getRequiredStackSize(self: *const GenBlock) u8 {
        return @intCast(u8, self.numLocals + self.numTempLocals);
    }
};

const BlockJumpType = enum {
    jumpToEndLocals,
};

const BlockJump = struct {
    jumpT: BlockJumpType,
    pc: u32,

    /// Offset from `pc` to where the jump value should be encoded.
    pcOffset: u16,
};

const SubBlockJumpType = enum {
    /// Each if/else body contains a break at the end to jump out of the if block.
    /// Each match case block jumps to the end of the match block.
    subBlockBreak,
    /// Breaks out of a for loop.
    brk,
    /// Continues a for loop.
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