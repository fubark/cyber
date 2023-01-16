const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const fmt = @import("fmt.zig");
const v = fmt.v;
const vm_ = @import("vm.zig");
const sema = @import("sema.zig");
const os_mod = @import("builtins/os.zig");

const log = stdx.log.scoped(.vm_compiler);

const NullId = std.math.maxInt(u32);
const NullIdU16 = std.math.maxInt(u16);
const NullIdU8 = std.math.maxInt(u8);
const f64NegOne = cy.Value.initF64(-1);
const f64One = cy.Value.initF64(1);

const dumpCompileErrorStackTrace = builtin.mode == .Debug and true;

const Root = @This();

pub const VMcompiler = struct {
    alloc: std.mem.Allocator,
    vm: *cy.VM,
    buf: cy.ByteCodeBuffer,
    lastErr: []const u8,
    lastErrNode: cy.NodeId,

    /// Used to return additional info for an error.
    errorPayload: cy.NodeId,

    /// -- Context vars

    /// Since nodes are currently processed recursively,
    /// set the current node so that error reporting has a better
    /// location context for helper methods that simply return no context errors.
    curNodeId: cy.NodeId,
    src: []const u8,
    nodes: []cy.Node,
    tokens: []const cy.Token,
    funcDecls: []cy.FuncDecl,
    funcParams: []const cy.FunctionParam,
    semaBlocks: std.ArrayListUnmanaged(sema.Block),
    semaSubBlocks: std.ArrayListUnmanaged(sema.SubBlock),
    vars: std.ArrayListUnmanaged(sema.LocalVar),
    capVarDescs: std.AutoHashMapUnmanaged(sema.LocalVarId, sema.CapVarDesc),
    blocks: std.ArrayListUnmanaged(Block),
    blockJumpStack: std.ArrayListUnmanaged(BlockJump),
    subBlockJumpStack: std.ArrayListUnmanaged(SubBlockJump),

    /// Tracks which temp locals are reserved. They are skipped for temp local allocations.
    reservedTempLocalStack: std.ArrayListUnmanaged(ReservedTempLocal),

    assignedVarStack: std.ArrayListUnmanaged(sema.LocalVarId),
    operandStack: std.ArrayListUnmanaged(cy.OpData),
    
    /// Generic linked list buffer.
    dataNodes: std.ArrayListUnmanaged(DataNode),

    /// Which sema sym var is currently being analyzed for an assignment initializer.
    curSemaSymVar: u32,

    /// When looking at a var declaration, keep track of which symbols are already recorded as dependencies.
    semaVarDeclDeps: std.AutoHashMapUnmanaged(u32, void),

    curBlock: *Block,
    curSemaBlockId: sema.BlockId,
    semaBlockDepth: u32,
    curSemaSubBlockId: sema.SubBlockId,

    /// Used during codegen to advance to the next saved sema block.
    nextSemaBlockId: u32,
    nextSemaSubBlockId: u32,

    /// Currently used to store lists of static var dependencies.
    bufU32: std.ArrayListUnmanaged(u32),

    /// Used for temp string building.
    tempBufU8: std.ArrayListUnmanaged(u8),

    /// Local paths to syms.
    semaSyms: std.ArrayListUnmanaged(sema.Sym),
    semaSymMap: std.HashMapUnmanaged(sema.AbsSymSigKey, sema.ResolvedSymId, sema.AbsSymSigKeyContext, 80),
    
    /// Absolute path to syms and shared among modules.
    semaResolvedSyms: std.ArrayListUnmanaged(sema.ResolvedSym),
    semaResolvedSymMap: std.HashMapUnmanaged(sema.AbsSymSigKey, sema.ResolvedSymId, sema.AbsSymSigKeyContext, 80),

    semaSymToRef: std.AutoArrayHashMapUnmanaged(sema.SymId, sema.SymRef),

    /// Modules.
    modules: std.ArrayListUnmanaged(sema.Module),
    /// Specifier to module.
    moduleMap: std.StringHashMapUnmanaged(sema.ModuleId),

    typeNames: std.StringHashMapUnmanaged(sema.Type),

    pub fn init(self: *VMcompiler, vm: *cy.VM) !void {
        self.* = .{
            .alloc = vm.alloc,
            .vm = vm,
            .buf = try cy.ByteCodeBuffer.init(vm.alloc),
            .lastErr = "",
            .lastErrNode = undefined,
            .errorPayload = undefined,
            .curSemaSymVar = NullId,
            .curNodeId = NullId,
            .nodes = undefined,
            .tokens = undefined,
            .funcDecls = undefined,
            .funcParams = undefined,
            .semaBlocks = .{},
            .semaSubBlocks = .{},
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
            .nextSemaBlockId = undefined,
            .nextSemaSubBlockId = undefined,
            .semaBlockDepth = undefined,
            .src = undefined,
            .tempBufU8 = .{},
            .bufU32 = .{},
            .semaSyms = .{},
            .semaSymMap = .{},
            .semaResolvedSyms = .{},
            .semaResolvedSymMap = .{},
            .modules = .{},
            .moduleMap = .{},
            .semaSymToRef = .{},
            .typeNames = .{},
            .dataNodes = .{},
            .semaVarDeclDeps = .{},
        };
        try self.typeNames.put(self.alloc, "int", sema.IntegerType);
    }

    pub fn deinit(self: *VMcompiler) void {
        self.alloc.free(self.lastErr);

        for (self.semaSubBlocks.items) |*block| {
            block.deinit(self.alloc);
        }
        self.semaSubBlocks.deinit(self.alloc);

        for (self.semaBlocks.items) |*sblock| {
            sblock.deinit(self.alloc);
        }
        self.semaBlocks.deinit(self.alloc);

        self.blocks.deinit(self.alloc);
        self.buf.deinit();
        self.blockJumpStack.deinit(self.alloc);
        self.subBlockJumpStack.deinit(self.alloc);
        self.assignedVarStack.deinit(self.alloc);
        self.operandStack.deinit(self.alloc);
        self.tempBufU8.deinit(self.alloc);
        self.bufU32.deinit(self.alloc);
        self.reservedTempLocalStack.deinit(self.alloc);
        self.vars.deinit(self.alloc);
        self.capVarDescs.deinit(self.alloc);

        self.semaSyms.deinit(self.alloc);
        self.semaSymMap.deinit(self.alloc);

        self.semaResolvedSyms.deinit(self.alloc);
        self.semaResolvedSymMap.deinit(self.alloc);

        if (self.moduleMap.get("os")) |id| {
            os_mod.deinitModule(self, self.modules.items[id]);
        }
        for (self.modules.items) |*mod| {
            mod.deinit(self.alloc);
        }
        self.modules.deinit(self.alloc);
        self.moduleMap.deinit(self.alloc);
        self.semaSymToRef.deinit(self.alloc);
        self.typeNames.deinit(self.alloc);
        self.dataNodes.deinit(self.alloc);
        self.semaVarDeclDeps.deinit(self.alloc);
    }

    fn nextSemaSubBlock(self: *VMcompiler) void {
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

    fn prevSemaSubBlock(self: *VMcompiler) void {
        self.curSemaSubBlockId = sema.curSubBlock(self).prevSubBlockId;
    }

    fn nextSemaBlock(self: *VMcompiler) void {
        self.curSemaBlockId = self.nextSemaBlockId;
        self.nextSemaBlockId += 1;
        self.nextSemaSubBlock();
    }

    fn prevSemaBlock(self: *VMcompiler) void {
        self.curSemaBlockId = sema.curBlock(self).prevBlockId;
        self.prevSemaSubBlock();
    }

    /// Reserve locals upfront and gen var initializer if necessary.
    fn genInitLocals(self: *VMcompiler) !void {
        const sblock = sema.curBlock(self);

        // Reserve the locals.
        var numInitializers: u32 = 0;
        for (sblock.locals.items) |varId| {
            const svar = self.genGetVarPtr(varId).?;
            _ = try self.reserveLocalVar(varId);
            if (svar.genInitializer) {
                numInitializers += 1;
            }
            // log.debug("reserve {} {s}", .{local, self.getVarName(varId)});
        }

        if (numInitializers > 0) {
            try self.buf.pushOp1(.setInitN, @intCast(u8, numInitializers));
            for (sblock.locals.items) |varId| {
                const svar = self.genGetVar(varId).?;
                if (svar.genInitializer) {
                    // log.debug("init {} {s}", .{svar.local, self.getVarName(varId)});
                    try self.buf.pushOperand(svar.local);
                }
            }
        }

        self.resetNextFreeTemp();
    }

    /// Generates var decl initializer.
    /// If the declaration contains dependencies those are generated first in DFS order.
    fn genVarDeclInitDFS(self: *VMcompiler, symId: u32) !void {
        const sym = &self.semaSyms.items[symId];
        sym.visited = true;

        if (self.semaSymToRef.get(symId)) |ref| {
            // Contains dependencies. Generate initializers for them first.
            const deps = self.bufU32.items[ref.inner.initDeps.start..ref.inner.initDeps.end];
            for (deps) |dep| {
                if (!self.semaSyms.items[dep].visited) {
                    try self.genVarDeclInitDFS(dep);
                }
            }
        }

        // log.debug("generate init var: {s}", .{sym.path});
        const rsym = self.semaResolvedSyms.items[sym.resolvedSymId];
        const declId = rsym.inner.variable.declId;
        const decl = self.nodes[declId];
        const exprv = try self.genRetainedTempExpr(decl.head.varDecl.right, false);
        const rtSymId = try self.vm.ensureVarSym(NullId, sym.getName());
        try self.buf.pushOp2(.setStatic, @intCast(u8, rtSymId), exprv.local);
    }

    /// Wrap compile so all errors can be handled in one place.
    fn compileInner(self: *VMcompiler, ast: cy.ParseResultView) !void {
        // Import core module into local namespace.
        const modId = try sema.getOrLoadModule(self, "core", NullId);
        try sema.importAllFromModule(self, modId);

        const root = self.nodes[ast.root_id];

        // Ensure static var syms.
        for (self.vm.parser.varDecls.items) |declId| {
            const decl = self.nodes[declId];
            const ident = self.nodes[decl.head.varDecl.left];
            const str = self.getNodeTokenString(ident);
            const symId = try sema.ensureSym(self, null, str, null);
            self.semaSyms.items[symId].symT = .variable;
        }

        try sema.pushBlock(self);
        sema.semaStmts(self, root.head.child_head, true) catch |err| {
            try sema.endBlock(self);
            return err;
        };
        try sema.endBlock(self);

        // After sema pass, resolve used syms.
        for (self.semaSyms.items) |sym, symId| {
            // Only full symbol paths that are unresolved.
            if (sym.used and sym.resolvedSymId == NullId) {
                try sema.resolveSym(self, @intCast(u32, symId));
            }
        }

        self.nextSemaBlockId = 1;
        self.nextSemaSubBlockId = 1;
        _ = self.nextSemaBlock();
        try self.pushBlock();

        // Once all symbols have been resolved, the static variables are initialized in DFS order.
        // Uses temp locals from the main block.
        for (self.semaSyms.items) |sym, i| {
            const symId = @intCast(u32, i);
            if (sym.used and sym.symT == .variable and !sym.visited) {
                try self.genVarDeclInitDFS(symId);
            }
        }
        self.resetNextFreeTemp();

        try self.genInitLocals();
        try self.genStatements(root.head.child_head, true);
        self.popBlock();
        self.buf.mainStackSize = @intCast(u32, self.curBlock.getRequiredStackSize());

        // Merge inst and const buffers.
        var reqLen = self.buf.ops.items.len + self.buf.consts.items.len * @sizeOf(cy.Const) + @alignOf(cy.Const) - 1;
        if (self.buf.ops.capacity < reqLen) {
            try self.buf.ops.ensureTotalCapacityPrecise(self.alloc, reqLen);
        }
        const constAddr = std.mem.alignForward(@ptrToInt(self.buf.ops.items.ptr) + self.buf.ops.items.len, @alignOf(cy.Const));
        const constDst = @intToPtr([*]cy.Const, constAddr)[0..self.buf.consts.items.len];
        const constSrc = try self.buf.consts.toOwnedSlice(self.alloc);
        std.mem.copy(cy.Const, constDst, constSrc);
        self.alloc.free(constSrc);
        self.buf.mconsts = constDst;

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

    pub fn compile(self: *VMcompiler, ast: cy.ParseResultView) !ResultView {
        self.buf.clear();
        self.blocks.clearRetainingCapacity();

        for (self.semaBlocks.items) |*block| {
            block.deinit(self.alloc);
        }
        self.semaBlocks.clearRetainingCapacity();

        for (self.semaSubBlocks.items) |*block| {
            block.deinit(self.alloc);
        }
        self.semaSubBlocks.clearRetainingCapacity();

        // Dummy first element to avoid len > 0 check during pop.
        try self.semaSubBlocks.append(self.alloc, sema.SubBlock.init(0, 0));
        try self.semaBlocks.append(self.alloc, sema.Block.init(0));

        self.nodes = ast.nodes.items;
        self.funcDecls = ast.func_decls.items;
        self.funcParams = ast.func_params;
        self.src = ast.src;
        self.tokens = ast.tokens;
        self.semaBlockDepth = 0;

        self.compileInner(ast) catch |err| {
            if (dumpCompileErrorStackTrace and !cy.silentError) {
                std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            }
            if (err != error.CompileError) {
                // Report other errors with curNodeId.
                try self.setErrorAt("Error: {}", &.{v(err)}, self.curNodeId);
            }
            return ResultView{
                .buf = self.buf,
                .hasError = true,
            };
        };
        return ResultView{
            .buf = self.buf,
            .hasError = false,
        };
    }

    fn endLocals(self: *VMcompiler) !void {
        const sblock = sema.curBlock(self);

        const start = self.operandStack.items.len;
        defer self.operandStack.items.len = start;

        for (sblock.params.items) |varId| {
            const svar = self.vars.items[varId];
            if (svar.lifetimeRcCandidate and !svar.isCaptured) {
                try self.operandStack.append(self.alloc, cy.OpData.initArg(svar.local));
            }
        }
        for (sblock.locals.items) |varId| {
            const svar = self.vars.items[varId];
            if (svar.lifetimeRcCandidate and svar.genIsDefined) {
                try self.operandStack.append(self.alloc, cy.OpData.initArg(svar.local));
            }
        }
        
        const locals = self.operandStack.items[start..];
        if (locals.len > 0) {
            if (locals.len == 1) {
                try self.buf.pushOp1(.release, locals[0].arg);
            } else {
                try self.buf.pushOp1(.releaseN, @intCast(u8, locals.len));
                try self.buf.pushOperands(locals);
            }
        }
    }

    fn pushJumpBackNotNone(self: *VMcompiler, toPc: usize, condLocal: LocalId) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp3(.jumpNotNone, 0, 0, condLocal);
        self.buf.setOpArgU16(pc + 1, @bitCast(u16, -@intCast(i16, pc - toPc)));
    }

    fn pushEmptyJumpNotNone(self: *VMcompiler, condLocal: LocalId) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpNotNone, 0, 0, condLocal);
        return start;
    }

    fn pushEmptyJumpNotCond(self: *VMcompiler, condLocal: LocalId) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpNotCond, 0, 0, condLocal);
        return start;
    }

    fn pushJumpBackCond(self: *VMcompiler, toPc: usize, condLocal: LocalId) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp3(.jumpCond, 0, 0, condLocal);
        self.buf.setOpArgU16(pc + 1, @bitCast(u16, -@intCast(i16, pc - toPc)));
    }

    fn pushJumpBackTo(self: *VMcompiler, toPc: usize) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp2(.jump, 0, 0);
        self.buf.setOpArgU16(pc + 1, @bitCast(u16, -@intCast(i16, pc - toPc)));
    }

    fn pushEmptyJump(self: *VMcompiler) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp2(.jump, 0, 0);
        return start;
    }

    fn pushEmptyJumpCond(self: *VMcompiler, condLocal: LocalId) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpCond, 0, 0, condLocal);
        return start;
    }

    fn patchJumpToCurrent(self: *VMcompiler, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 1, @intCast(u16, self.buf.ops.items.len - jumpPc));
    }

    /// Patches jumps related to this `if` block.
    /// All other jumps are propagated up the stack by copying to the front.
    /// Returns the adjusted jumpStackStart for this block.
    fn patchIfBlockJumps(self: *VMcompiler, jumpStackStart: usize, breakPc: usize) usize {
        var propagateIdx = jumpStackStart;
        for (self.subBlockJumpStack.items[jumpStackStart..]) |jump| {
            if (jump.jumpT == .ifBreak) {
                self.buf.setOpArgU16(jump.pc + 1, @intCast(u16, breakPc - jump.pc));
            } else {
                self.subBlockJumpStack.items[propagateIdx] = jump;
                propagateIdx += 1;
            }
        }
        return propagateIdx;
    }

    fn patchForBlockJumps(self: *VMcompiler, jumpStackStart: usize, breakPc: usize, contPc: usize) void {
        for (self.subBlockJumpStack.items[jumpStackStart..]) |jump| {
            switch (jump.jumpT) {
                .ifBreak => {
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

    fn patchBlockJumps(self: *VMcompiler, jumpStackStart: usize) void {
        for (self.blockJumpStack.items[jumpStackStart..]) |jump| {
            switch (jump.jumpT) {
                .jumpToEndLocals => {
                    self.buf.setOpArgU16(jump.pc + 1, @intCast(u16, self.curBlock.endLocalsPc - jump.pc));
                }
            }
        }
    }

    fn pushBlock(self: *VMcompiler) !void {
        try self.blocks.append(self.alloc, Block.init());
        self.curBlock = &self.blocks.items[self.blocks.items.len-1];
        self.curBlock.reservedTempLocalStart = @intCast(u32, self.reservedTempLocalStack.items.len);
    }

    fn popBlock(self: *VMcompiler) void {
        var last = self.blocks.pop();
        self.reservedTempLocalStack.items.len = last.reservedTempLocalStart;
        last.deinit(self.alloc);
        if (self.blocks.items.len > 0) {
            self.curBlock = &self.blocks.items[self.blocks.items.len-1];
        }
    }

    fn blockNumLocals(self: *VMcompiler) usize {
        return sema.curBlock(self).locals.items.len + sema.curBlock(self).params.items.len;
    }

    fn genStatements(self: *VMcompiler, head: cy.NodeId, comptime attachEnd: bool) anyerror!void {
        var cur_id = head;
        var node = self.nodes[cur_id];

        while (node.next != NullId) {
            try self.genStatement(cur_id, true);
            cur_id = node.next;
            node = self.nodes[cur_id];
        }

        // Check for last expression statement.
        if (node.node_t == .expr_stmt) {
            if (attachEnd) {
                const local = try self.genExprStmt(cur_id, true, false);
                try self.endLocals();
                try self.buf.pushOp1(.end, local);
            } else {
                _ = try self.genStatement(cur_id, true);
            }
        } else {
            if (attachEnd) {
                try self.genStatement(cur_id, false);
                try self.endLocals();
                try self.buf.pushOp1(.end, 255);
            } else {
                try self.genStatement(cur_id, true);
            }
        }
    }

    fn genGetVarPtr(self: *const VMcompiler, id: sema.LocalVarId) ?*sema.LocalVar {
        if (id != NullId) {
            return &self.vars.items[id];
        } else {
            return null;
        }
    }

    fn genGetVar(self: *const VMcompiler, id: sema.LocalVarId) ?sema.LocalVar {
        if (id != NullId) {
            return self.vars.items[id];
        } else {
            return null;
        }
    }

    fn reserveLocalVar(self: *VMcompiler, varId: sema.LocalVarId) !LocalId {
        const local = try self.reserveLocal(self.curBlock);
        self.vars.items[varId].local = local;
        return local;
    }

    fn genSetBoxedVarToExpr(self: *VMcompiler, svar: *sema.LocalVar, exprId: cy.NodeId) !void {
        // Retain rval.
        const exprv = try self.genRetainedTempExpr(exprId, false);
        svar.vtype = exprv.vtype;
        svar.genIsDefined = true;
        if (!svar.vtype.rcCandidate) {
            try self.buf.pushOp2(.setBoxValue, svar.local, exprv.local);
        } else {
            try self.buf.pushOp2(.setBoxValueRelease, svar.local, exprv.local);
        }
    }

    fn genSetVarToExpr(self: *VMcompiler, leftId: cy.NodeId, exprId: cy.NodeId, comptime discardTopExprReg: bool) !void {
        _ = discardTopExprReg;
        const varId = self.nodes[leftId].head.ident.semaVarId;
        const expr = self.nodes[exprId];
        if (self.genGetVarPtr(varId)) |svar| {
            if (svar.isBoxed) {
                if (!svar.genIsDefined) {
                    const exprv = try self.genExpr(exprId, false);
                    try self.buf.pushOp2(.box, @intCast(u8, exprv.local), svar.local);
                    svar.vtype = exprv.vtype;

                    if (self.capVarDescs.get(varId)) |desc| {
                        // Update dependent func syms.
                        const start = self.operandStack.items.len;
                        defer self.operandStack.items.len = start;
                        var cur = desc.owner;
                        var numFuncSyms: u8 = 0;
                        while (cur != NullId) {
                            const dep = self.dataNodes.items[cur];
                            try self.pushTempOperand(@intCast(u8, dep.inner.funcSym.symId));
                            try self.pushTempOperand(dep.inner.funcSym.capVarIdx);
                            numFuncSyms += 1;
                            cur = dep.next;
                        }
                        try self.buf.pushOp2(.setCapValToFuncSyms, svar.local, numFuncSyms);
                        try self.buf.pushOperands(self.operandStack.items[start..]);
                    }

                    svar.genIsDefined = true;
                    return;
                } else {
                    try self.genSetBoxedVarToExpr(svar, exprId);
                    return;
                }
            }

            if (expr.node_t == .ident) {
                const exprv = try self.genExpr(exprId, false);
                if (svar.genIsDefined) {
                    if (svar.vtype.rcCandidate) {
                        // log.debug("releaseSet {} {}", .{varId, svar.vtype.typeT});
                        if (exprv.vtype.rcCandidate) {
                            try self.buf.pushOp2(.copyRetainRelease, exprv.local, svar.local);
                        } else {
                            try self.buf.pushOp2(.copyReleaseDst, exprv.local, svar.local);
                        }
                    } else {
                        // log.debug("set {} {}", .{varId, svar.vtype.typeT});
                        if (exprv.vtype.rcCandidate) {
                            try self.buf.pushOp2(.copyRetainSrc, exprv.local, svar.local);
                        } else {
                            try self.buf.pushOp2(.copy, exprv.local, svar.local);
                        }
                    }
                    if (svar.vtype.typeT != exprv.vtype.typeT) {
                        svar.vtype = exprv.vtype;
                    }
                } else {
                    if (exprv.vtype.rcCandidate) {
                        if (exprv.vtype.typeT == .box) {
                            // TODO: Becomes boxValue if child is known to not be an rcCandidate.
                            try self.buf.pushOp2(.boxValueRetain, exprv.local, svar.local);
                        } else {
                            try self.buf.pushOp2(.copyRetainSrc, exprv.local, svar.local);
                        }
                    } else {
                        try self.buf.pushOp2(.copy, exprv.local, svar.local);
                    }
                    svar.genIsDefined = true;
                    svar.vtype = exprv.vtype;
                }
                return;
            } else if (expr.node_t == .tagLiteral) {
                if (svar.vtype.typeT == .tag) {
                    const name = self.getNodeTokenString(expr);
                    const symId = try self.vm.ensureTagLitSym(name);
                    const sym = self.vm.tagLitSyms.buf[symId];
                    if (sym.symT == .one and sym.inner.one.id == svar.vtype.inner.tag.tagId) {
                        try self.buf.pushOp3(.tag, svar.vtype.inner.tag.tagId, @intCast(u8, sym.inner.one.val), svar.local);
                        return;
                    }
                }
            }

            // Retain rval.
            if (!svar.genIsDefined or !svar.vtype.rcCandidate) {
                const exprv = try self.genRetainedExprTo(exprId, svar.local, false);
                svar.vtype = exprv.vtype;
                if (!svar.genIsDefined) {
                    svar.genIsDefined = true;
                }
            } else {
                const exprv = try self.genRetainedTempExpr(exprId, false);
                try self.buf.pushOp2(.copyReleaseDst, exprv.local, svar.local);
                svar.vtype = exprv.vtype;
            }
        } else {
            const left = self.nodes[leftId];
            return self.reportErrorAt("Undefined var: `{}`", &.{v(self.getNodeTokenString(left))}, leftId);
        }
    }

    fn reserveLocal(self: *VMcompiler, block: *Block) !u8 {
        const idx = block.numLocals;
        block.numLocals += 1;
        if (idx <= std.math.maxInt(u8)) {
            return @intCast(u8, idx);
        } else {
            return self.reportError("Exceeded max local count: {}", &.{v(@as(u8, std.math.maxInt(u8)))});
        }
    }

    // Reserve params and captured vars.
    fn reserveFuncParams(self: *VMcompiler) !void {
        // First local is reserved for a single return value.
        _ = try self.reserveLocal(self.curBlock);

        // Second local is reserved for the return info.
        _ = try self.reserveLocal(self.curBlock);

        // Third local is reserved for the return address.
        _ = try self.reserveLocal(self.curBlock);

        // Fourth local is reserved for the previous frame pointer.
        _ = try self.reserveLocal(self.curBlock);

        const sblock = sema.curBlock(self);
        for (sblock.params.items) |varId| {
            _ = try self.reserveLocalVar(varId);

            // Params are already defined.
            self.vars.items[varId].genIsDefined = true;
        }
    }

    /// ARC managed temps need to be released at the end of the current ARC expression.
    fn beginArcExpr(self: *VMcompiler) u32 {
        self.curBlock.arcTempLocalStart = @intCast(u32, self.reservedTempLocalStack.items.len);
        return self.curBlock.arcTempLocalStart;
    }

    fn endArcExpr(self: *VMcompiler, arcTempLocalStart: u32) !void {
        // Gen release ops.
        for (self.reservedTempLocalStack.items[self.curBlock.arcTempLocalStart..]) |temp| {
            try self.buf.pushOp1(.release, temp.local);
        }
        self.reservedTempLocalStack.items.len = self.curBlock.arcTempLocalStart;

        // Restore current local start.
        self.curBlock.arcTempLocalStart = arcTempLocalStart;
    }

    fn genExprStmt(self: *VMcompiler, stmtId: cy.NodeId, retainEscapeTop: bool, comptime discardTopExprReg: bool) !LocalId {
        const arcLocalStart = self.beginArcExpr();

        const stmt = self.nodes[stmtId];
        var val: GenValue = undefined;
        if (retainEscapeTop) {
            const node = self.nodes[stmt.head.child_head];
            var retLocal = false;
            if (node.node_t == .ident) {
                if (self.genGetVar(node.head.ident.semaVarId)) |svar| {
                    if (!svar.isBoxed) {
                        if (svar.vtype.rcCandidate) {
                            try self.buf.pushOp1(.retain, svar.local);
                        }
                        val = GenValue.initLocalValue(svar.local, svar.vtype);
                        retLocal = true;
                    }
                }
            }
            if (!retLocal) {
                val = try self.genRetainedTempExpr(stmt.head.child_head, discardTopExprReg);
            }
        } else {
            val = try self.genExpr(stmt.head.child_head, discardTopExprReg);
        }

        try self.endArcExpr(arcLocalStart);
        return val.local;
    }

    fn genBinOpAssignToField(self: *VMcompiler, code: cy.OpCode, leftId: cy.NodeId, rightId: cy.NodeId) !void {
        const left = self.nodes[leftId];

        const startTempLocal = self.curBlock.firstFreeTempLocal;
        defer self.computeNextTempLocalFrom(startTempLocal);

        const accessRight = self.nodes[left.head.accessExpr.right];
        if (accessRight.node_t != .ident) {
            return self.reportErrorAt("Expected ident. Got {}.", &.{v(accessRight.node_t)}, left.head.accessExpr.right);
        }
        const fieldName = self.getNodeTokenString(accessRight);
        const fieldId = try self.vm.ensureFieldSym(fieldName);

        const accessLeftv = try self.genExpr(left.head.accessExpr.left, false);
        const accessLocal = try self.nextFreeTempLocal();
        try self.buf.pushOpSlice(.field, &.{ accessLeftv.local, accessLocal, @intCast(u8, fieldId), 0, 0, 0 });

        const rightv = try self.genExpr(rightId, false);
        try self.buf.pushOp3(code, accessLocal, rightv.local, accessLocal);

        try self.buf.pushOp3(.setField, @intCast(u8, fieldId), accessLeftv.local, accessLocal);
        try self.pushDebugSym(leftId);
    }

    /// discardTopExprReg is usually true since statements aren't expressions and evaluating child expressions
    /// would just grow the register stack unnecessarily. However, the last main statement requires the
    /// resulting expr to persist to return from `eval`.
    fn genStatement(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) !void {
        // log.debug("gen stmt {}", .{node.node_t});
        self.curNodeId = nodeId;

        self.resetNextFreeTemp();

        const node = self.nodes[nodeId];
        switch (node.node_t) {
            .pass_stmt => {
                return;
            },
            .expr_stmt => {
                _ = try self.genExprStmt(nodeId, false, discardTopExprReg);
            },
            .breakStmt => {
                const pc = try self.pushEmptyJump();
                try self.subBlockJumpStack.append(self.alloc, .{ .jumpT = .brk, .pc = pc });
            },
            .continueStmt => {
                const pc = try self.pushEmptyJump();
                try self.subBlockJumpStack.append(self.alloc, .{ .jumpT = .cont, .pc = pc });
            },
            .opAssignStmt => {
                const left = self.nodes[node.head.opAssignStmt.left];
                const genOp: cy.OpCode = switch (node.head.opAssignStmt.op) {
                    .plus => .add,
                    .minus => .minus,
                    .star => .mul,
                    .slash => .div,
                    else => fmt.panic("Unexpected operator assignment.", &.{}),
                };
                if (left.node_t == .ident) {
                    if (left.head.ident.semaVarId != NullId) {
                        const svar = self.genGetVarPtr(left.head.ident.semaVarId).?;
                        const right = try self.genExpr(node.head.opAssignStmt.right, false);
                        if (svar.isBoxed) {
                            const tempLocal = try self.nextFreeTempLocal();
                            try self.buf.pushOp2(.boxValue, svar.local, tempLocal);
                            try self.buf.pushOp3(genOp, tempLocal, right.local, tempLocal);
                            try self.buf.pushOp2(.setBoxValue, svar.local, tempLocal);
                            return;
                        } else {
                            try self.buf.pushOp3(genOp, svar.local, right.local, svar.local);
                        }
                    } else {
                        const sym = self.semaSyms.items[left.head.ident.semaSymId];
                        const rightv = try self.genExpr(node.head.opAssignStmt.right, false);
                        const rtSymId = try self.vm.ensureVarSym(NullId, sym.getName());

                        const tempLocal = try self.nextFreeTempLocal();
                        try self.buf.pushOp2(.static, @intCast(u8, rtSymId), tempLocal);
                        try self.buf.pushOp3(genOp, tempLocal, rightv.local, tempLocal);
                        try self.buf.pushOp2(.setStatic, @intCast(u8, rtSymId), tempLocal);
                    }
                } else if (left.node_t == .accessExpr) {
                    try self.genBinOpAssignToField(genOp, node.head.opAssignStmt.left, node.head.opAssignStmt.right);
                } else {
                    unexpectedFmt("unsupported assignment to left {}", &.{fmt.v(left.node_t)});
                }
            },
            .assign_stmt => {
                const left = self.nodes[node.head.left_right.left];
                if (left.node_t == .ident) {
                    if (left.head.ident.semaVarId != NullId) {
                        try self.genSetVarToExpr(node.head.left_right.left, node.head.left_right.right, false);
                    } else {
                        const sym = self.semaSyms.items[left.head.ident.semaSymId];
                        const rightv = try self.genRetainedTempExpr(node.head.left_right.right, false);
                        const rtSymId = try self.vm.ensureVarSym(NullId, sym.getName());
                        try self.buf.pushOp2(.setStatic, @intCast(u8, rtSymId), rightv.local);
                    }
                } else if (left.node_t == .arr_access_expr) {
                    const startTempLocal = self.curBlock.firstFreeTempLocal;
                    defer self.setFirstFreeTempLocal(startTempLocal);

                    const leftv = try self.genExpr(left.head.left_right.left, false);
                    const indexv = try self.genExpr(left.head.left_right.right, false);
                    const rightv = try self.genRetainedTempExpr(node.head.left_right.right, false);
                    try self.buf.pushOp3(.setIndexRelease, leftv.local, indexv.local, rightv.local);
                } else if (left.node_t == .accessExpr) {
                    const startTempLocal = self.curBlock.firstFreeTempLocal;
                    defer self.setFirstFreeTempLocal(startTempLocal);

                    const leftv = try self.genExpr(left.head.accessExpr.left, false);

                    const accessRight = self.nodes[left.head.accessExpr.right];
                    if (accessRight.node_t != .ident) {
                        return self.reportErrorAt("Expected ident. Got {}.", &.{v(accessRight.node_t)}, left.head.accessExpr.right);
                    }

                    const rightv = try self.genRetainedTempExpr(node.head.left_right.right, false);

                    const fieldName = self.getNodeTokenString(accessRight);
                    const fieldId = try self.vm.ensureFieldSym(fieldName);
                    try self.buf.pushOpSlice(.setFieldRelease, &.{leftv.local, rightv.local, @intCast(u8, fieldId), 0, 0, 0 });
                    try self.pushDebugSym(nodeId);
                } else {
                    unexpectedFmt("unsupported assignment to left {}", &.{fmt.v(left.node_t)});
                }
            },
            .varDecl => {
                // Nop. Static variables are hoisted and initialized at the start of the program.
            },
            .localDecl => {
                // Can assume left is .ident from sema.
                try self.genSetVarToExpr(node.head.left_right.left, node.head.left_right.right, false);
            },
            .tagDecl => {
                // Nop.
            },
            .structDecl => {
                const nameN = self.nodes[node.head.structDecl.name];
                const name = self.getNodeTokenString(nameN);

                const sid = try self.vm.ensureStruct(name);

                var i: u32 = 0;
                var fieldId = node.head.structDecl.fieldsHead;
                while (fieldId != NullId) : (i += 1) {
                    const field = self.nodes[fieldId];
                    fieldId = field.next;
                }
                const numFields = i;

                i = 0;
                fieldId = node.head.structDecl.fieldsHead;
                while (fieldId != NullId) : (i += 1) {
                    const field = self.nodes[fieldId];
                    const fieldName = self.getNodeTokenString(field);
                    const fieldSymId = try self.vm.ensureFieldSym(fieldName);
                    try self.vm.addFieldSym(sid, fieldSymId, @intCast(u16, i));
                    fieldId = field.next;
                }

                self.vm.structs.buf[sid].numFields = numFields;

                var funcId = node.head.structDecl.funcsHead;
                var func: cy.Node = undefined;
                while (funcId != NullId) : (funcId = func.next) {
                    func = self.nodes[funcId];
                    const decl = self.funcDecls[func.head.func.decl_id];

                    const funcName = self.src[decl.name.start..decl.name.end];
                    if (decl.params.end > decl.params.start) {
                        const param = self.funcParams[decl.params.start];
                        const paramName = self.src[param.name.start..param.name.end];
                        if (std.mem.eql(u8, paramName, "self")) {
                            // Struct method.
                            try self.genMethodDecl(sid, func, decl, funcName);
                            continue;
                        }
                    }

                    const detail = cy.FuncSymDetail{
                        .name = try self.alloc.dupe(u8, funcName),
                    };
                    try self.vm.funcSymDetails.append(self.alloc, detail);

                    try self.genFuncDecl(funcId);
                }
            },
            .func_decl => {
                try self.genFuncDecl(nodeId);
            },
            .forCondStmt => {
                self.nextSemaSubBlock();

                const topPc = @intCast(u32, self.buf.ops.items.len);
                const jumpStackSave = @intCast(u32, self.subBlockJumpStack.items.len);
                defer self.subBlockJumpStack.items.len = jumpStackSave;

                var condLocal: LocalId = undefined;
                if (node.head.forCondStmt.as != NullId) {
                    const as = self.nodes[node.head.forCondStmt.as];
                    condLocal = self.genGetVar(as.head.ident.semaVarId).?.local;
                    // Since this variable is used in the loop, it is considered defined before codegen.
                    self.vars.items[as.head.ident.semaVarId].genIsDefined = true;
                    try self.genSetVarToExpr(node.head.forCondStmt.as, node.head.forCondStmt.cond, false);
                } else {
                    const condv = try self.genExpr(node.head.forCondStmt.cond, false);
                    condLocal = condv.local;
                }

                var jumpPc = try self.pushEmptyJumpNotCond(condLocal);

                try self.genStatements(node.head.forCondStmt.bodyHead, false);
                try self.pushJumpBackTo(topPc);

                self.patchJumpToCurrent(jumpPc);

                self.patchForBlockJumps(jumpStackSave, self.buf.ops.items.len, topPc);
                self.prevSemaSubBlock();
            },
            .for_inf_stmt => {
                self.nextSemaSubBlock();

                const pcSave = @intCast(u32, self.buf.ops.items.len);
                const jumpStackSave = @intCast(u32, self.subBlockJumpStack.items.len);
                defer self.subBlockJumpStack.items.len = jumpStackSave;

                // TODO: generate gas meter checks.
                // if (self.opts.gas_meter != .none) {
                //     try self.indent();
                //     if (self.opts.gas_meter == .error_interrupt) {
                //         _ = try self.writer.write("__interrupt_count += 1; if (__interrupt_count > __interrupt_max) throw globalThis._internal.interruptSym;\n");
                //     } else if (self.opts.gas_meter == .yield_interrupt) {
                //         _ = try self.writer.write("__interrupt_count += 1; if (__interrupt_count > __interrupt_max) yield globalThis._internal.interruptSym;\n");
                //     }
                // }

                try self.genStatements(node.head.child_head, false);
                try self.pushJumpBackTo(pcSave);

                self.patchForBlockJumps(jumpStackSave, self.buf.ops.items.len, pcSave);
                self.prevSemaSubBlock();
            },
            .for_iter_stmt => {
                self.nextSemaSubBlock();
                defer self.prevSemaSubBlock();

                const iterable = try self.genExpr(node.head.for_iter_stmt.iterable, false);

                const eachClause = self.nodes[node.head.for_iter_stmt.eachClause];

                var keyVar: sema.LocalVar = undefined;
                var keyIdent: cy.Node = undefined;
                var pairIter = false;
                if (eachClause.head.eachClause.key != NullId) {
                    keyIdent = self.nodes[eachClause.head.eachClause.key];
                    keyVar = self.genGetVar(keyIdent.head.ident.semaVarId).?;
                    pairIter = true;
                }
                const valIdent = self.nodes[eachClause.head.eachClause.value];
                const valVar = self.genGetVar(valIdent.head.ident.semaVarId).?;

                // At this point the temp var is loosely defined.
                self.vars.items[valIdent.head.ident.semaVarId].genIsDefined = true;
                if (pairIter) {
                    self.vars.items[keyIdent.head.ident.semaVarId].genIsDefined = true;
                }

                // Loop needs to reserve temp locals.
                const reservedStart = self.reservedTempLocalStack.items.len;
                defer self.reservedTempLocalStack.items.len = reservedStart;

                // Reserve temp local for iterator.
                const iterLocal = try self.nextFreeTempLocal();
                try self.setReservedTempLocal(iterLocal);
                try self.buf.pushOp2(.copyRetainSrc, iterable.local, iterLocal + 4);
                if (pairIter) {
                    try self.buf.pushOpSlice(.callObjSym, &.{ iterLocal, 1, 1, @intCast(u8, self.vm.pairIteratorObjSym), 0, 0, 0, 0, 0, 0, 0, 0, 0 });
                } else {
                    try self.buf.pushOpSlice(.callObjSym, &.{ iterLocal, 1, 1, @intCast(u8, self.vm.iteratorObjSym), 0, 0, 0, 0, 0, 0, 0, 0, 0 });
                }

                try self.buf.pushOp2(.copyRetainSrc, iterLocal, iterLocal + 5);
                if (pairIter) {
                    try self.buf.pushOpSlice(.callObjSym, &.{ iterLocal + 1, 1, 2, @intCast(u8, self.vm.nextPairObjSym), 0, 0, 0, 0, 0, 0, 0, 0, 0 });
                    try self.buf.pushOp2(.copyReleaseDst, iterLocal + 1, keyVar.local);
                    try self.buf.pushOp2(.copyReleaseDst, iterLocal + 2, valVar.local);
                } else {
                    try self.buf.pushOpSlice(.callObjSym, &.{ iterLocal + 1, 1, 1, @intCast(u8, self.vm.nextObjSym), 0, 0, 0, 0, 0, 0, 0, 0, 0 });
                    try self.buf.pushOp2(.copyReleaseDst, iterLocal + 1, valVar.local);
                }

                const skipSkipJump = try self.pushEmptyJumpNotNone(if (pairIter) keyVar.local else valVar.local);
                const skipBodyJump = try self.pushEmptyJump();
                self.patchJumpToCurrent(skipSkipJump);

                const bodyPc = self.buf.ops.items.len;
                const jumpStackSave = @intCast(u32, self.subBlockJumpStack.items.len);
                defer self.subBlockJumpStack.items.len = jumpStackSave;
                try self.genStatements(node.head.for_iter_stmt.body_head, false);

                const contPc = self.buf.ops.items.len;
                try self.buf.pushOp2(.copyRetainSrc, iterLocal, iterLocal + 5);
                if (pairIter) {
                    try self.buf.pushOpSlice(.callObjSym, &.{ iterLocal + 1, 1, 2, @intCast(u8, self.vm.nextPairObjSym), 0, 0, 0, 0, 0, 0, 0, 0, 0 });
                    try self.buf.pushOp2(.copyReleaseDst, iterLocal + 1, keyVar.local);
                    try self.buf.pushOp2(.copyReleaseDst, iterLocal + 2, valVar.local);
                } else {
                    try self.buf.pushOpSlice(.callObjSym, &.{ iterLocal + 1, 1, 1, @intCast(u8, self.vm.nextObjSym), 0, 0, 0, 0, 0, 0, 0, 0, 0 });
                    try self.buf.pushOp2(.copyReleaseDst, iterLocal + 1, valVar.local);
                }

                try self.pushJumpBackNotNone(bodyPc, if (pairIter) keyVar.local else valVar.local);

                self.patchForBlockJumps(jumpStackSave, self.buf.ops.items.len, contPc);

                try self.buf.pushOp1(.release, iterLocal);
                self.patchJumpToCurrent(skipBodyJump);
            },
            .for_range_stmt => {
                self.nextSemaSubBlock();
                defer self.prevSemaSubBlock();

                var local: u8 = NullIdU8;
                if (node.head.for_range_stmt.eachClause != NullId) {
                    const eachClause = self.nodes[node.head.for_range_stmt.eachClause];
                    const ident = self.nodes[eachClause.head.eachClause.value];
                    local = self.genGetVar(ident.head.ident.semaVarId).?.local;

                    // inc = as_clause.head.as_range_clause.inc;
                    // if (as_clause.head.as_range_clause.step != NullId) {
                    //     step = self.nodes[as_clause.head.as_range_clause.step];
                    // }
                }

                // Loop needs to reserve temp locals.
                const reservedStart = self.reservedTempLocalStack.items.len;
                defer self.reservedTempLocalStack.items.len = reservedStart;

                // Set range start/end.
                const range_clause = self.nodes[node.head.for_range_stmt.range_clause];
                const rangeStartN = self.nodes[range_clause.head.left_right.left];
                const rangeEndN = self.nodes[range_clause.head.left_right.right];
                var lessThanCond = true;
                if (rangeStartN.node_t == .number and rangeEndN.node_t == .number) {
                    const startLit = self.getNodeTokenString(rangeStartN);
                    const endLit = self.getNodeTokenString(rangeEndN);
                    const start = try std.fmt.parseFloat(f64, startLit);
                    const end = try std.fmt.parseFloat(f64, endLit);
                    if (start > end) {
                        lessThanCond = false;
                    }
                }

                // Keep counter hidden from user. (User can't change it's value.)
                const counter = try self.nextFreeTempLocal();
                try self.setReservedTempLocal(counter);

                const rangeStart = try self.genExpr(range_clause.head.left_right.left, false);

                const rangeEnd = try self.nextFreeTempLocal();
                _ = try self.genExprTo(range_clause.head.left_right.right, rangeEnd, false, false);
                try self.setReservedTempLocal(rangeEnd);

                // Set custom step.
                const rangeStep = try self.nextFreeTempLocal();
                try self.setReservedTempLocal(rangeStep);
                _ = try self.genConst(1, rangeStep);

                const initPc = self.buf.ops.items.len;
                try self.buf.pushOpSlice(.forRangeInit, &.{ rangeStart.local, rangeEnd, rangeStep, counter, local, 0, 0 });

                const bodyPc = self.buf.ops.items.len;
                const jumpStackSave = @intCast(u32, self.subBlockJumpStack.items.len);
                defer self.subBlockJumpStack.items.len = jumpStackSave;
                try self.genStatements(node.head.for_range_stmt.body_head, false);

                // Perform counter update and perform check against end range.
                const jumpBackOffset = @intCast(u16, self.buf.ops.items.len - bodyPc);
                const forRangeOp = self.buf.ops.items.len;
                // The forRange op is patched by forRangeInit at runtime.
                self.buf.setOpArgU16(initPc + 6, @intCast(u16, self.buf.ops.items.len - initPc));
                try self.buf.pushOpSlice(.forRange, &.{ counter, rangeStep, rangeEnd, local, 0, 0 });
                self.buf.setOpArgU16(forRangeOp + 5, jumpBackOffset);

                self.patchForBlockJumps(jumpStackSave, self.buf.ops.items.len, forRangeOp);
            },
            .if_stmt => {
                const startTempLocal = self.curBlock.firstFreeTempLocal;

                const condv = try self.genExpr(node.head.left_right.left, false);
                var lastCondJump = try self.pushEmptyJumpNotCond(condv.local);
                self.setFirstFreeTempLocal(startTempLocal);

                self.nextSemaSubBlock();
                try self.genStatements(node.head.left_right.right, false);
                self.prevSemaSubBlock();

                var elseClauseId = node.head.left_right.extra;
                if (elseClauseId != NullId) {
                    var jumpsStart = self.subBlockJumpStack.items.len;
                    defer self.subBlockJumpStack.items.len = jumpsStart;

                    var endsWithElse = false;
                    while (elseClauseId != NullId) {
                        const pc = try self.pushEmptyJump();
                        try self.subBlockJumpStack.append(self.alloc, .{ .jumpT = .ifBreak, .pc = pc });

                        self.patchJumpToCurrent(lastCondJump);

                        const elseClause = self.nodes[elseClauseId];
                        if (elseClause.head.else_clause.cond == NullId) {
                            self.nextSemaSubBlock();
                            try self.genStatements(elseClause.head.else_clause.body_head, false);
                            self.prevSemaSubBlock();
                            endsWithElse = true;
                            self.setFirstFreeTempLocal(startTempLocal);
                            break;
                        } else {
                            const elifCondv = try self.genExpr(elseClause.head.else_clause.cond, false);
                            lastCondJump = try self.pushEmptyJumpNotCond(elifCondv.local);

                            self.nextSemaSubBlock();
                            try self.genStatements(elseClause.head.else_clause.body_head, false);
                            self.prevSemaSubBlock();
                            elseClauseId = elseClause.head.else_clause.else_clause;

                            self.setFirstFreeTempLocal(startTempLocal);
                        }
                    }

                    if (!endsWithElse) {
                        self.patchJumpToCurrent(lastCondJump);
                    }
                    jumpsStart = self.patchIfBlockJumps(jumpsStart, self.buf.ops.items.len);
                } else {
                    self.patchJumpToCurrent(lastCondJump);
                }
            },
            .importStmt => {
                const ident = self.nodes[node.head.left_right.left];
                const name = self.getNodeTokenString(ident);

                const spec = self.nodes[node.head.left_right.right];
                const specPath = self.getNodeTokenString(spec);

                _ = name;
                _ = specPath;

                // const modId = try self.getOrLoadModule(specPath);
                // const leadSym = try self.ensureSemaSym(name);
                // try self.semaSymToMod.put(self.alloc, leadSym, modId);
            },
            .return_stmt => {
                if (self.blocks.items.len == 1) {
                    try self.endLocals();
                    try self.buf.pushOp1(.end, 255);
                } else {
                    try self.endLocals();
                    try self.buf.pushOp(.ret0);
                }
            },
            .return_expr_stmt => {
                self.setFirstFreeTempLocal(@intCast(u8, self.curBlock.numLocals));

                if (self.blocks.items.len == 1) {
                    var val: GenValue = undefined;
                    if (self.curBlock.resolvedSymId != NullId) {
                        const retType = self.semaResolvedSyms.items[self.curBlock.resolvedSymId].inner.func.retType;
                        val = try self.genRetainedTempExpr2(node.head.child_head, retType, false);
                    } else {
                        val = try self.genRetainedTempExpr(node.head.child_head, false);
                    }
                    try self.endLocals();
                    try self.buf.pushOp1(.end, @intCast(u8, val.local));
                } else {
                    _ = try self.genRetainedExprTo(node.head.child_head, 0, false);
                    try self.endLocals();
                    try self.buf.pushOp(.ret1);
                }
            },
            else => {
                return self.reportErrorAt("Unsupported statement: {}", &.{v(node.node_t)}, nodeId);
            }
        }
    }

    fn genExprToDestOrTempLocal(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, usedDst: *bool, comptime discardTopExprReg: bool) !GenValue {
        return self.genExprToDestOrTempLocal2(sema.AnyType, nodeId, dst, usedDst, discardTopExprReg);
    }

    fn genExprToDestOrTempLocal2(self: *VMcompiler, requestedType: sema.Type, nodeId: cy.NodeId, dst: LocalId, usedDst: *bool, comptime discardTopExprReg: bool) !GenValue {
        const node = self.nodes[nodeId];
        if (isArcTempNode(node.node_t) or usedDst.*) {
            const finalDst = try self.userLocalOrNextTempLocal(nodeId);
            return self.genExprTo2(nodeId, finalDst, requestedType, false, discardTopExprReg);
        } else {
            const finalDst = self.userLocalOrDst(nodeId, dst, usedDst);
            return self.genExprTo2(nodeId, finalDst, requestedType, false, discardTopExprReg);
        }
    }

    /// Generates an instruction to put the root expression to a specific destination local.
    /// Also ensures that the expression is retained.
    fn genRetainedExprTo(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, comptime discardTopExprReg: bool) anyerror!GenValue {
        return try self.genExprTo(nodeId, dst, true, discardTopExprReg);
    }

    fn genRetainedTempExpr(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) !GenValue {
        return self.genRetainedTempExpr2(nodeId, sema.AnyType, discardTopExprReg);
    }

    /// Ensures that the expr value is retained and ends up in the next temp local.
    fn genRetainedTempExpr2(self: *VMcompiler, nodeId: cy.NodeId, requiredType: sema.Type, comptime discardTopExprReg: bool) anyerror!GenValue {
        const arcLocalStart = self.beginArcExpr();

        const dst = try self.nextFreeTempLocal();
        // ARC temps released at the end of this expr,
        // so the next free temp is guaranteed to be after dst.
        defer self.setFirstFreeTempLocal(dst + 1);

        const val = try self.genExprTo2(nodeId, dst, requiredType, true, discardTopExprReg);
        try self.genEnsureRequiredType(val, requiredType);
        try self.endArcExpr(arcLocalStart);
        return val;
    }

    fn genMethodDecl(self: *VMcompiler, structId: cy.StructId, node: cy.Node, func: cy.FuncDecl, name: []const u8) !void {
        // log.debug("gen method {s}", .{name});
        const numParams = func.params.end - func.params.start;
        const methodId = try self.vm.ensureMethodSymKey(name, numParams - 1);

        const jumpPc = try self.pushEmptyJump();

        try self.pushBlock();
        self.nextSemaBlock();

        const opStart = @intCast(u32, self.buf.ops.items.len);
        try self.reserveFuncParams();
        try self.genInitLocals();
        try self.genStatements(node.head.func.body_head, false);
        // TODO: Check last statement to skip adding ret.
        try self.endLocals();
        try self.buf.pushOp(.ret0);

        // Reserve another local for the call return info.
        const numLocals = @intCast(u32, self.blockNumLocals() + 1 - numParams);

        self.prevSemaBlock();
        self.popBlock();

        self.patchJumpToCurrent(jumpPc);

        const sym = cy.MethodSym.initFuncOffset(opStart, numLocals);
        try self.vm.addMethodSym(structId, methodId, sym);
    }

    fn genFuncDecl(self: *VMcompiler, nodeId: cy.NodeId) !void {
        const node = self.nodes[nodeId];
        const func = self.funcDecls[node.head.func.decl_id];
        const sym = self.semaSyms.items[func.semaSymId];
        const numParams = @intCast(u8, sym.key.numParams);

        const resolvedParentId = if (sym.key.parentId == NullId) NullId else self.semaSyms.items[sym.key.parentId].resolvedSymId;
        const symId = try self.vm.ensureFuncSym(resolvedParentId, sym.getName(), sym.key.numParams);

        const jumpPc = try self.pushEmptyJump();

        try self.pushBlock();
        self.nextSemaBlock();
        self.curBlock.frameLoc = nodeId;
        self.curBlock.resolvedSymId = sym.resolvedSymId;

        const jumpStackStart = self.blockJumpStack.items.len;

        const opStart = @intCast(u32, self.buf.ops.items.len);
        try self.reserveFuncParams();
        try self.genInitLocals();
        try self.genStatements(node.head.func.body_head, false);
        // TODO: Check last statement to skip adding ret.
        self.curBlock.endLocalsPc = @intCast(u32, self.buf.ops.items.len);
        self.nodes[nodeId].head.func.genEndLocalsPc = self.curBlock.endLocalsPc;
        
        try self.endLocals();
        try self.buf.pushOp(.ret0);

        // Reserve another local for the call return info.
        const sblock = sema.curBlock(self);
        const numLocals = @intCast(u32, self.curBlock.numLocals + self.curBlock.numTempLocals);
        const numCaptured = @intCast(u8, sblock.params.items.len - numParams);

        self.patchJumpToCurrent(jumpPc);

        self.patchBlockJumps(jumpStackStart);
        self.blockJumpStack.items.len = jumpStackStart;

        self.prevSemaBlock();
        self.popBlock();

        if (numCaptured > 0) {
            const operandStart = self.operandStack.items.len;
            defer self.operandStack.items.len = operandStart;

            // Push register operands to op.
            for (sblock.params.items) |varId| {
                const svar = self.vars.items[varId];
                if (svar.isCaptured) {
                    const pId = self.capVarDescs.get(varId).?.user;
                    const pvar = &self.vars.items[pId];

                    if (svar.isBoxed and !pvar.isBoxed) {
                        pvar.isBoxed = true;
                        pvar.lifetimeRcCandidate = true;
                    }
                }
            }

            const closure = try self.vm.allocEmptyClosure(opStart, numParams, @intCast(u8, numLocals), numCaptured);
            const rtSym = cy.FuncSymbolEntry.initClosure(closure.asHeapObject(*cy.Closure));
            self.vm.setFuncSym(symId, rtSym);
        } else {
            const rtSym = cy.FuncSymbolEntry.initFuncOffset(opStart, numLocals);
            self.vm.setFuncSym(symId, rtSym);
        }
    }

    fn genCallArgs2(self: *VMcompiler, func: cy.FuncDecl, first: cy.NodeId) !u32 {
        const params = self.funcParams[func.params.start .. func.params.end];
        var numArgs: u32 = 0;
        var argId = first;
        while (argId != NullId) : (numArgs += 1) {
            const arg = self.nodes[argId];
            var reqType = sema.AnyType;
            const param = params[numArgs];
            if (param.typeName.len() > 0) {
                const typeName = self.src[param.typeName.start..param.typeName.end];
                if (self.typeNames.get(typeName)) |paramT| {
                    reqType = paramT;
                }
            }
            _ = try self.genRetainedTempExpr2(argId, reqType, false);
            argId = arg.next;
        }
        return numArgs;
    }

    fn genCallArgs(self: *VMcompiler, first: cy.NodeId) !u32 {
        var numArgs: u32 = 0;
        var argId = first;
        while (argId != NullId) : (numArgs += 1) {
            const arg = self.nodes[argId];
            _ = try self.genRetainedTempExpr(argId, false);
            argId = arg.next;
        }
        return numArgs;
    }

    fn genEnsureRtFuncSym(self: *VMcompiler, symId: sema.SymId) !u32 {
        const sym = self.semaSyms.items[symId];
        if (sym.resolvedSymId != NullId) {
            const resolvedParentId = if (sym.key.parentId == NullId) NullId else self.semaSyms.items[sym.key.parentId].resolvedSymId;
            return self.vm.ensureFuncSym(resolvedParentId, sym.getName(), sym.key.numParams);
        } else {
            // Undefined namespace is NullId-1 so it doesn't conflict with the root.
            return self.vm.ensureFuncSym(NullId-1, sym.getName(), sym.key.numParams);
        }
    }

    fn genGetResolvedSym(self: *const VMcompiler, semaSymId: sema.SymId) ?sema.ResolvedSym {
        const sym = self.semaSyms.items[semaSymId];
        if (sym.resolvedSymId != NullId) {
            return self.semaResolvedSyms.items[sym.resolvedSymId];
        } else {
            return null;
        }
    }

    fn genCallExpr(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, comptime discardTopExprReg: bool, comptime startFiber: bool) !GenValue {
        const startTempLocal = self.curBlock.firstFreeTempLocal;
        // Can assume temps after startTempLocal are not arc temps.
        defer self.setFirstFreeTempLocal(startTempLocal);

        var callStartLocal = self.advanceNextTempLocalPastArcTemps();
        if (self.blocks.items.len == 1) {
            // Main block.
            // Ensure call start register is at least 1 so the runtime can easily check
            // if framePtr is at main or a function.
            if (callStartLocal == 0) {
                callStartLocal = 1;
            }
        }

        // Reserve registers for return value and return info.
        if (dst + 1 == self.curBlock.firstFreeTempLocal) {
            callStartLocal -= 1;
        } else {
            _ = try self.nextFreeTempLocal();
        }
        _ = try self.nextFreeTempLocal();
        _ = try self.nextFreeTempLocal();
        _ = try self.nextFreeTempLocal();

        const genCallStartLocal = if (startFiber) 1 else callStartLocal;

        const node = self.nodes[nodeId];
        const callee = self.nodes[node.head.func_call.callee];
        if (!node.head.func_call.has_named_arg) {
            if (callee.node_t == .accessExpr) {
                if (callee.head.accessExpr.semaSymId != NullId) {
                    if (self.genGetResolvedSym(callee.head.accessExpr.semaSymId)) |rsym| {
                        const sym = self.semaSyms.items[callee.head.accessExpr.semaSymId];
                        if (rsym.symT == .func) {
                            // Symbol func call.

                            var numArgs: u32 = undefined;
                            if (rsym.inner.func.declId != NullId) {
                                const func = self.funcDecls[rsym.inner.func.declId];
                                numArgs = try self.genCallArgs2(func, node.head.func_call.arg_head);
                            } else {
                                numArgs = try self.genCallArgs(node.head.func_call.arg_head);
                            }

                            // var isStdCall = false;
                            const resolvedParentId = if (sym.key.parentId == NullId) NullId else self.semaSyms.items[sym.key.parentId].resolvedSymId;
                            if (self.vm.getFuncSym(resolvedParentId, sym.getName(), sym.key.numParams)) |symId| {
                                if (discardTopExprReg) {
                                    try self.buf.pushOpSlice(.callSym, &.{ callStartLocal, @intCast(u8, numArgs), 0, @intCast(u8, symId), 0, 0, 0, 0, 0, 0 });
                                    try self.pushDebugSym(nodeId);
                                } else {
                                    try self.buf.pushOpSlice(.callSym, &.{ callStartLocal, @intCast(u8, numArgs), 1, @intCast(u8, symId), 0, 0, 0, 0, 0, 0 });
                                    try self.pushDebugSym(nodeId);
                                }
                                return GenValue.initTempValue(callStartLocal, rsym.inner.func.retType);
                            } else {
                                return self.reportErrorAt("Unsupported callee", &.{}, nodeId);
                            }

                            // if (try self.readScopedVar(leftName)) |info| {
                            //     if (info.vtype.typeT == ListType.typeT) {
                            //         if (self.vm.hasMethodSym(cy.ListS, methodId)) {
                            //             isStdCall = true;
                            //         } 
                            //     }
                            // }
                            
                            // if (isStdCall) {
                            //     // Avoid retain/release for std call.
                            //     _ = try self.genExpr(left, false);
                            // } else {
                            //     _ = try self.genMaybeRetainExpr(left, false);
                            // }
                        } else {
                            return self.reportErrorAt("Unsupported callee", &.{}, nodeId);
                        }
                    }
                }

                // accessExpr is not a symbol.

                const right = self.nodes[callee.head.accessExpr.right];
                if (right.node_t == .ident) {
                    // One more arg for receiver.
                    const numArgs = 1 + try self.genCallArgs(node.head.func_call.arg_head);

                    const rightName = self.getNodeTokenString(right);
                    const methodId = try self.vm.ensureMethodSymKey(rightName, numArgs - 1);

                    _ = try self.genRetainedTempExpr(callee.head.accessExpr.left, false);

                    if (discardTopExprReg) {
                        try self.buf.pushOpSlice(.callObjSym, &.{ callStartLocal, @intCast(u8, numArgs), 0, @intCast(u8, methodId), 0, 0, 0, 0, 0, 0, 0, 0, 0 });
                        try self.pushDebugSym(nodeId);
                    } else {
                        try self.buf.pushOpSlice(.callObjSym, &.{ callStartLocal, @intCast(u8, numArgs), 1, @intCast(u8, methodId), 0, 0, 0, 0, 0, 0, 0, 0, 0 });
                        try self.pushDebugSym(nodeId);
                    }
                    return GenValue.initTempValue(callStartLocal, sema.AnyType);
                } else return self.reportErrorAt("Unsupported callee", &.{}, nodeId);
            } else if (callee.node_t == .ident) {
                if (self.genGetVar(callee.head.ident.semaVarId)) |_| {
                    var numArgs: u32 = 1;
                    var argId = node.head.func_call.arg_head;
                    while (argId != NullId) : (numArgs += 1) {
                        const arg = self.nodes[argId];
                        _ = try self.genRetainedTempExpr(argId, false);
                        argId = arg.next;
                    }

                    _ = try self.genRetainedTempExpr(node.head.func_call.callee, false);
                    self.setFirstFreeTempLocal(startTempLocal + 1);

                    if (discardTopExprReg) {
                        try self.buf.pushOp2(.call0, callStartLocal, @intCast(u8, numArgs));
                        return GenValue.initNoValue();
                    } else {
                        try self.buf.pushOp2(.call1, callStartLocal, @intCast(u8, numArgs));
                        return GenValue.initTempValue(callStartLocal, sema.AnyType);
                    }
                } else {
                    var genArgs = false;
                    var numArgs: u32 = undefined;
                    if (self.genGetResolvedSym(callee.head.ident.semaSymId)) |semaSym| {
                        if (semaSym.symT == .func) {
                            if (semaSym.inner.func.declId != NullId) {
                                const func = self.funcDecls[semaSym.inner.func.declId];
                                numArgs = try self.genCallArgs2(func, node.head.func_call.arg_head);
                                genArgs = true;
                            }
                        } else {
                            return self.reportErrorAt("Unsupported callee", &.{}, nodeId);
                        }
                    // } else {
                    //     const symPath = self.semaSyms.items[callee.head.ident.semaSymId].path;
                    //     log.debug("{} {s}", .{callee.head.ident.semaSymId, symPath});
                    //     return self.reportErrorAt("Missing symbol: {s}", .{symPath}, node);
                    // }
                    }

                    if (!genArgs) {
                        numArgs = try self.genCallArgs(node.head.func_call.arg_head);
                    }

                    const coinitPc = self.buf.ops.items.len;
                    if (startFiber) {
                        // Precompute first arg local since coinit doesn't need the startLocal.
                        // numArgs + 4 (ret slots) + 1 (min call start local for main block)
                        var initialStackSize = numArgs + 4 + 1;
                        if (initialStackSize < 16) {
                            initialStackSize = 16;
                        }
                        try self.buf.pushOpSlice(.coinit, &.{ callStartLocal + 4, @intCast(u8, numArgs), 0, @intCast(u8, initialStackSize), dst });
                    }

                    const rtSymId = try self.genEnsureRtFuncSym(callee.head.ident.semaSymId);
                    if (discardTopExprReg) {
                        try self.buf.pushOpSlice(.callSym, &.{ genCallStartLocal, @intCast(u8, numArgs), 0, @intCast(u8, rtSymId), 0, 0, 0, 0, 0, 0 });
                        try self.pushDebugSym(nodeId);
                    } else {
                        try self.buf.pushOpSlice(.callSym, &.{ genCallStartLocal, @intCast(u8, numArgs), 1, @intCast(u8, rtSymId), 0, 0, 0, 0, 0, 0 });
                        try self.pushDebugSym(nodeId);
                    }

                    if (startFiber) {
                        try self.buf.pushOp(.coreturn);
                        self.buf.setOpArgs1(coinitPc + 3, @intCast(u8, self.buf.ops.items.len - coinitPc));
                    }

                    if (self.genGetResolvedSym(callee.head.ident.semaSymId)) |semaSym| {
                        return GenValue.initTempValue(callStartLocal, semaSym.inner.func.retType);
                    } else {
                        return GenValue.initTempValue(callStartLocal, sema.AnyType);
                    }
                }
            } else return self.reportError("Unsupported callee {}", &.{fmt.v(callee.node_t)});
        } else return self.reportError("Unsupported named args", &.{});
    }

    fn genIfExpr(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, retainEscapeTop: bool, comptime discardTopExprReg: bool) !GenValue {
        const node = self.nodes[nodeId];
        const startTempLocal = self.curBlock.firstFreeTempLocal;
        defer self.computeNextTempLocalFrom(startTempLocal);

        const condv = try self.genExpr(node.head.if_expr.cond, false);
        var jumpNotPc = try self.pushEmptyJumpNotCond(condv.local);

        self.computeNextTempLocalFrom(startTempLocal);
    
        var truev = try self.genExprTo(node.head.if_expr.body_expr, dst, retainEscapeTop, false);
        const jumpPc = try self.pushEmptyJump();
        self.patchJumpToCurrent(jumpNotPc);

        self.computeNextTempLocalFrom(startTempLocal);

        var falsev: GenValue = undefined;
        if (node.head.if_expr.else_clause != NullId) {
            const else_clause = self.nodes[node.head.if_expr.else_clause];
            falsev = try self.genExprTo(else_clause.head.child_head, dst, retainEscapeTop, discardTopExprReg);
        } else {
            if (!discardTopExprReg) {
                falsev = try self.genNone(dst);
            }
        }

        self.patchJumpToCurrent(jumpPc);

        if (truev.vtype.typeT != falsev.vtype.typeT) {
            return self.initGenValue(dst, sema.AnyType);
        } else {
            return self.initGenValue(dst, truev.vtype);
        }
    }

    fn genString(self: *VMcompiler, str: []const u8, dst: LocalId) !GenValue {
        const idx = try self.buf.getOrPushStringConst(str);
        try self.buf.pushOp2(.constOp, @intCast(u8, idx), dst);
        return self.initGenValue(dst, sema.StaticStringType);
    }

    fn genConstInt(self: *VMcompiler, val: f64, dst: LocalId) !GenValue {
        if (cy.Value.floatCanBeInteger(val)) {
            const i = @floatToInt(i64, val);
            if (i >= std.math.minInt(i8) and i <= std.math.maxInt(i8)) {
                try self.buf.pushOp2(.constI8Int, @bitCast(u8, @intCast(i8, i)), dst);
                return self.initGenValue(dst, sema.IntegerType);
            }
        } else {
            return self.reportError("TODO: coerce", &.{});
        }
        const int = @floatToInt(i32, val);
        const idx = try self.buf.pushConst(cy.Const.init(cy.Value.initI32(int).val));
        try self.buf.pushOp2(.constOp, @intCast(u8, idx), dst);
        return self.initGenValue(dst, sema.IntegerType);
    }

    fn genConst(self: *VMcompiler, val: f64, dst: LocalId) !GenValue {
        if (cy.Value.floatCanBeInteger(val)) {
            const i = @floatToInt(i64, val);
            if (i >= std.math.minInt(i8) and i <= std.math.maxInt(i8)) {
                try self.buf.pushOp2(.constI8, @bitCast(u8, @intCast(i8, i)), dst);
                return self.initGenValue(dst, sema.NumberType);
            }
        }
        const idx = try self.buf.pushConst(cy.Const.init(@bitCast(u64, val)));
        try self.buf.pushOp2(.constOp, @intCast(u8, idx), dst);
        return self.initGenValue(dst, sema.NumberType);
    }

    fn genNone(self: *VMcompiler, dst: LocalId) !GenValue {
        try self.buf.pushOp1(.none, dst);
        return self.initGenValue(dst, sema.AnyType);
    }

    fn isReservedTempLocal(self: *const VMcompiler, local: LocalId) bool {
        for (self.reservedTempLocalStack.items[self.curBlock.reservedTempLocalStart..]) |temp| {
            if (temp.local == local) {
                return true;
            }
        }
        return false;
    }

    fn userLocalOrDst(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, usedDst: *bool) LocalId {
        if (self.nodes[nodeId].node_t == .ident) {
            if (self.genGetVar(self.nodes[nodeId].head.ident.semaVarId)) |svar| {
                if (!svar.isBoxed) {
                    // If boxed, the value needs to be copied outside of the box.
                    return svar.local;
                }
            }
        }
        usedDst.* = true;
        return dst;
    }

    fn userLocalOrNextTempLocal(self: *VMcompiler, nodeId: cy.NodeId) !LocalId {
        const node = self.nodes[nodeId];
        if (node.node_t == .ident) {
            if (self.genGetVar(self.nodes[nodeId].head.ident.semaVarId)) |svar| {
                if (!svar.isBoxed) {
                    return svar.local;
                }
            }
        } else if (node.node_t == .call_expr) {
            // Since call expr args allocate arg locals past the arc temps,
            // select the call dst to be past the arc temps to skip generating an extra copy op.
            _ = self.advanceNextTempLocalPastArcTemps();
            return self.nextFreeTempLocal();
        }
        return self.nextFreeTempLocal();
    }

    fn advanceNextTempLocalPastArcTemps(self: *VMcompiler) LocalId {
        if (self.curBlock.reservedTempLocalStart < self.reservedTempLocalStack.items.len) {
            for (self.reservedTempLocalStack.items[self.curBlock.reservedTempLocalStart..]) |temp| {
                if (self.curBlock.firstFreeTempLocal <= temp.local) {
                    self.curBlock.firstFreeTempLocal = temp.local + 1;
                }
            }
        }
        return self.curBlock.firstFreeTempLocal;
    }

    /// TODO: Rename to reserveNextTempLocal.
    fn nextFreeTempLocal(self: *VMcompiler) !LocalId {
        if (self.curBlock.firstFreeTempLocal < 256) {
            if (self.curBlock.firstFreeTempLocal == self.curBlock.numLocals + self.curBlock.numTempLocals) {
                self.curBlock.numTempLocals += 1;
            }
            defer {
                // Advance to the next free temp considering reserved arc temps.
                self.curBlock.firstFreeTempLocal += 1;
                if (self.curBlock.reservedTempLocalStart < self.reservedTempLocalStack.items.len) {
                    while (self.isReservedTempLocal(self.curBlock.firstFreeTempLocal)) {
                        self.curBlock.firstFreeTempLocal += 1;
                    }
                }
            }
            return @intCast(u8, self.curBlock.firstFreeTempLocal);
        } else {
            return self.reportError("Exceeded max locals.", &.{});
        }
    }

    fn computeNextTempLocalFrom(self: *VMcompiler, local: LocalId) void {
        self.curBlock.firstFreeTempLocal = local;
        if (self.curBlock.reservedTempLocalStart < self.reservedTempLocalStack.items.len) {
            while (self.isReservedTempLocal(self.curBlock.firstFreeTempLocal)) {
                self.curBlock.firstFreeTempLocal += 1;
            }
        }
    }

    /// Find first available temp starting from the beginning.
    fn resetNextFreeTemp(self: *VMcompiler) void {
        self.curBlock.firstFreeTempLocal = @intCast(u8, self.curBlock.numLocals);
        if (self.curBlock.reservedTempLocalStart < self.reservedTempLocalStack.items.len) {
            while (self.isReservedTempLocal(self.curBlock.firstFreeTempLocal)) {
                self.curBlock.firstFreeTempLocal += 1;
            }
        }
    }

    fn setFirstFreeTempLocal(self: *VMcompiler, local: LocalId) void {
        self.curBlock.firstFreeTempLocal = local;
    }

    /// Given two local values, determine the next destination temp local.
    /// The type of the dest value is left undefined to be set by caller.
    fn nextTempDestValue(self: *VMcompiler, src1: GenValue, src2: GenValue) !GenValue {
        if (src1.isTempLocal == src2.isTempLocal) {
            if (src1.isTempLocal) {
                const minTempLocal = std.math.min(src1.local, src2.local);
                self.setFirstFreeTempLocal(minTempLocal + 1);
                return GenValue.initTempValue(minTempLocal, undefined);
            } else {
                return GenValue.initTempValue(try self.nextFreeTempLocal(), undefined);
            }
        } else {
            if (src1.isTempLocal) {
                return GenValue.initTempValue(src1.local, undefined);
            } else {
                return GenValue.initTempValue(src2.local, undefined);
            }
        }
    }

    fn genEnsureRequiredType(self: *VMcompiler, genValue: GenValue, requiredType: sema.Type) !void {
        if (requiredType.typeT != .any) {
            if (genValue.vtype.typeT != requiredType.typeT) {
                return self.reportError("Type {} can not be auto converted to required type {}", &.{fmt.v(genValue.vtype.typeT), fmt.v(requiredType.typeT)});
            }
        }
    }

    fn genExpr(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) anyerror!GenValue {
        self.curNodeId = nodeId;
        return self.genExpr2(nodeId, sema.AnyType, discardTopExprReg);
    }

    /// If the expression is a user local, the local is returned.
    /// Otherwise, the expression is allocated a temp local on the stack.
    fn genExpr2(self: *VMcompiler, nodeId: cy.NodeId, requiredType: sema.Type, comptime discardTopExprReg: bool) anyerror!GenValue {
        const dst = try self.userLocalOrNextTempLocal(nodeId);
        const res = try self.genExprTo2(nodeId, dst, requiredType, false, discardTopExprReg);
        try self.genEnsureRequiredType(res, requiredType);
        return res;
    }

    fn canUseLocalAsTemp(self: *const VMcompiler, local: LocalId) bool {
        return local == 0 or local >= self.curBlock.numLocals;
    }

    fn isTempLocal(self: *const VMcompiler, local: LocalId) bool {
        return local >= self.curBlock.numLocals;
    }

    fn initGenValue(self: *const VMcompiler, local: LocalId, vtype: sema.Type) GenValue {
        if (local >= self.curBlock.numLocals) {
            return GenValue.initTempValue(local, vtype);
        } else {
            return GenValue.initLocalValue(local, vtype);
        }
    }

    fn genPushBinOp(self: *VMcompiler, code: cy.OpCode, left: cy.NodeId, right: cy.NodeId, vtype: sema.Type, dst: LocalId, comptime discardTopExprReg: bool) !GenValue {
        const startTempLocal = self.curBlock.firstFreeTempLocal;
        defer self.computeNextTempLocalFrom(startTempLocal);

        var usedDstAsTemp = !self.canUseLocalAsTemp(dst);
        const leftv = try self.genExprToDestOrTempLocal(left, dst, &usedDstAsTemp, discardTopExprReg);
        const rightv = try self.genExprToDestOrTempLocal(right, dst, &usedDstAsTemp, discardTopExprReg);
        if (!discardTopExprReg) {
            try self.buf.pushOp3(code, leftv.local, rightv.local, dst);
            return self.initGenValue(dst, vtype);
        } else return GenValue.initNoValue();
    }

    fn pushTempOperand(self: *VMcompiler, operand: u8) !void {
        try self.operandStack.append(self.alloc, cy.OpData.initArg(operand));
    }

    fn unescapeString(self: *VMcompiler, literal: []const u8) ![]const u8 {
        try self.tempBufU8.resize(self.alloc, literal.len);
        return Root.unescapeString(self.tempBufU8.items, literal);
    }

    fn setReservedTempLocal(self: *VMcompiler, local: LocalId) !void {
        try self.reservedTempLocalStack.append(self.alloc, .{
            .local = local,
        });
    }

    fn genExprTo(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, retainEscapeTop: bool, comptime discardTopExprReg: bool) anyerror!GenValue {
        return self.genExprTo2(nodeId, dst, sema.AnyType, retainEscapeTop, discardTopExprReg);
    }

    fn dumpLocals(self: *VMcompiler) !void {
        if (builtin.mode == .Debug) {
            const sblock = sema.curBlock(self);
            fmt.printStdout("Compiler (dump locals):\n", &.{});
            for (sblock.params.items) |varId| {
                const svar = self.vars.items[varId];
                fmt.printStdout("{} (param), local: {}, curType: {}, rc: {}, lrc: {}, boxed: {}, cap: {}\n", &.{
                    v(svar.name), v(svar.local), v(svar.vtype.typeT),
                    v(svar.vtype.rcCandidate), v(svar.lifetimeRcCandidate), v(svar.isBoxed), v(svar.isCaptured),
                });
            }
            for (sblock.locals.items) |varId| {
                const svar = self.vars.items[varId];
                fmt.printStdout("{}, local: {}, curType: {}, rc: {}, lrc: {}, boxed: {}, cap: {}\n", &.{
                    v(svar.name), v(svar.local), v(svar.vtype.typeT),
                    v(svar.vtype.rcCandidate), v(svar.lifetimeRcCandidate), v(svar.isBoxed), v(svar.isCaptured),
                });
            }
        }
    }

    /// `dst` indicates the local of the resulting value.
    /// `retainEscapeTop` indicates that the resulting val is meant to escape the current scope. (eg. call args)
    /// If `retainEscapeTop` is false, the dst is a temp local, and the expr requires a retain (eg. call expr), it is added as an arcTempLocal.
    fn genExprTo2(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, requestedType: sema.Type, retainEscapeTop: bool, comptime discardTopExprReg: bool) anyerror!GenValue {
        const node = self.nodes[nodeId];
        // log.debug("gen reg expr {}", .{node.node_t});
        switch (node.node_t) {
            .ident => {
                if (self.genGetVar(node.head.ident.semaVarId)) |svar| {
                    if (dst == svar.local) {
                        if (retainEscapeTop) {
                            stdx.panic("Unexpected retainEscapeTop.");
                        }
                        return GenValue.initSemaVar(svar);
                    } else {
                        if (retainEscapeTop and svar.vtype.rcCandidate) {
                            if (svar.isBoxed) {
                                try self.buf.pushOp2(.boxValueRetain, svar.local, dst);
                            } else {
                                try self.buf.pushOp2(.copyRetainSrc, svar.local, dst);
                            }
                        } else {
                            if (svar.isBoxed) {
                                try self.buf.pushOp2(.boxValue, svar.local, dst);
                            } else {
                                try self.buf.pushOp2(.copy, svar.local, dst);
                            }
                        }
                        return self.initGenValue(dst, svar.vtype);
                    }
                } else {
                    if (builtin.mode == .Debug and node.head.ident.semaSymId == NullId) {
                        unexpectedFmt("Missing semaSymId.", &.{});
                    }
                    if (self.genGetResolvedSym(node.head.ident.semaSymId)) |_| {
                        const name = self.semaSyms.items[node.head.ident.semaSymId].getName();
                        const varId = try self.vm.ensureVarSym(NullId, name);
                        try self.buf.pushOp2(.static, @intCast(u8, varId), dst);
                        return self.initGenValue(dst, sema.AnyType);
                    } else {
                        return self.genNone(dst);
                    }
                }
            },
            .true_literal => {
                if (!discardTopExprReg) {
                    try self.buf.pushOp1(.true, dst);
                    return self.initGenValue(dst, sema.BoolType);
                } else return GenValue.initNoValue();
            },
            .false_literal => {
                if (!discardTopExprReg) {
                    try self.buf.pushOp1(.false, dst);
                    return self.initGenValue(dst, sema.BoolType);
                } else return GenValue.initNoValue();
            },
            .number => {
                if (!discardTopExprReg) {
                    const literal = self.getNodeTokenString(node);
                    const val = try std.fmt.parseFloat(f64, literal);
                    if (requestedType.typeT == .int) {
                        return try self.genConstInt(val, dst);
                    } else {
                        return try self.genConst(val, dst);
                    }
                } else {
                    return GenValue.initNoValue();
                }
            },
            .nonDecInt => {
                if (!discardTopExprReg) {
                    const literal = self.getNodeTokenString(node);
                    var val: u64 = undefined;
                    if (literal[1] == 'x') {
                        val = try std.fmt.parseInt(u64, literal[2..], 16);
                    } else if (literal[1] == 'o') {
                        val = try std.fmt.parseInt(u64, literal[2..], 8);
                    } else if (literal[1] == 'b') {
                        val = try std.fmt.parseInt(u64, literal[2..], 2);
                    }
                    const fval = @intToFloat(f64, val);
                    if (requestedType.typeT == .int) {
                        return try self.genConstInt(fval, dst);
                    } else {
                        return try self.genConst(fval, dst);
                    }
                } else {
                    return GenValue.initNoValue();
                }
            },
            .tagLiteral => {
                const name = self.getNodeTokenString(node);
                const symId = try self.vm.ensureTagLitSym(name);
                try self.buf.pushOp2(.tagLiteral, @intCast(u8, symId), dst);
                return self.initGenValue(dst, sema.TagLiteralType);
            },
            .string => {
                if (!discardTopExprReg) {
                    const literal = self.getNodeTokenString(node);
                    const str = try self.unescapeString(literal);
                    return self.genString(str, dst);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .stringTemplate => {
                const operandStart = self.operandStack.items.len;
                defer self.operandStack.items.len = operandStart;

                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                const argStartLocal = self.advanceNextTempLocalPastArcTemps();

                var expStringPart = true;
                var curId = node.head.stringTemplate.partsHead;
                var numExprs: u32 = 0;
                while (curId != NullId) {
                    const cur = self.nodes[curId];
                    if (expStringPart) {
                        if (!discardTopExprReg) {
                            const raw = self.getNodeTokenString(cur);
                            const str = try self.unescapeString(raw);
                            const idx = try self.buf.getOrPushStringConst(str);
                            try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                        }
                    } else {
                        _ = try self.genRetainedTempExpr(curId, discardTopExprReg);
                        numExprs += 1;
                    }
                    curId = cur.next;
                    expStringPart = !expStringPart;
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOp3(.stringTemplate, argStartLocal, @intCast(u8, numExprs), dst);
                    try self.buf.pushOperands(self.operandStack.items[operandStart..]);

                    if (!retainEscapeTop and self.isTempLocal(dst)) {
                        try self.setReservedTempLocal(dst);
                    }
                    return self.initGenValue(dst, sema.StringType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .none => {
                if (!discardTopExprReg) {
                    return self.genNone(dst);
                } else return GenValue.initNoValue();
            },
            .tagInit => {
                const name = self.nodes[node.head.left_right.left];
                const tname = self.getNodeTokenString(name);
                const tid = self.vm.tagTypeSignatures.get(tname) orelse {
                    return self.reportErrorAt("Missing tag type: `{}`", &.{v(tname)}, nodeId);
                };

                const tagLit = self.nodes[node.head.left_right.right];
                const lname = self.getNodeTokenString(tagLit);
                const symId = self.vm.tagLitSymSignatures.get(lname) orelse {
                    return self.reportErrorAt("Missing tag literal: `{}`", &.{v(lname)}, nodeId);
                };
                const sym = self.vm.tagLitSyms.buf[symId];
                if (sym.symT == .one) {
                    if (sym.inner.one.id == tid) {
                        try self.buf.pushOp3(.tag, @intCast(u8, tid), @intCast(u8, sym.inner.one.val), dst);
                        const vtype = sema.initTagType(tid);
                        return self.initGenValue(dst, vtype);
                    }
                }
                return self.reportErrorAt("Tag `{}` does not have member `{}`", &.{v(tname), v(lname)}, nodeId); 
            },
            .structInit => {
                const stype = self.nodes[node.head.structInit.name];
                const sname = self.getNodeTokenString(stype);
                const sid = self.vm.getStruct(sname) orelse {
                    return self.reportErrorAt("Missing object type: `{}`", &.{v(sname)}, nodeId);
                };

                const initializer = self.nodes[node.head.structInit.initializer];
                
                // TODO: Would it be faster/efficient to copy the fields into contiguous registers 
                //       and copy all at once to heap or pass locals into the operands and iterate each and copy to heap?
                //       The current implementation is the former.
                // TODO: Have sema sort the fields so eval can handle default values easier.

                // Push props onto stack.

                const numFields = self.vm.structs.buf[sid].numFields;
                // Repurpose stack for sorting fields.
                const sortedFieldsStart = self.assignedVarStack.items.len;
                try self.assignedVarStack.resize(self.alloc, self.assignedVarStack.items.len + numFields);
                defer self.assignedVarStack.items.len = sortedFieldsStart;

                // Initially set to NullId so leftovers are defaulted to `none`.
                const initFields = self.assignedVarStack.items[sortedFieldsStart..];
                std.mem.set(u32, initFields, NullId);

                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                const argStartLocal = self.advanceNextTempLocalPastArcTemps();

                var i: u32 = 0;
                var entryId = initializer.head.child_head;
                // First iteration to sort the initializer fields.
                while (entryId != NullId) : (i += 1) {
                    const entry = self.nodes[entryId];
                    const prop = self.nodes[entry.head.mapEntry.left];
                    const fieldName = self.getNodeTokenString(prop);
                    const fieldIdx = self.vm.getStructFieldIdx(sid, fieldName) orelse {
                        return self.reportErrorAt("Missing field {}", &.{fmt.v(fieldName)}, entry.head.mapEntry.left);
                    };
                    initFields[fieldIdx] = entryId;
                    entryId = entry.next;
                }

                i = 0;
                while (i < numFields) : (i += 1) {
                    entryId = self.assignedVarStack.items[sortedFieldsStart + i];
                    if (entryId == NullId) {
                        if (!discardTopExprReg) {
                            // Push none.
                            const local = try self.nextFreeTempLocal();
                            try self.buf.pushOp1(.none, local);
                        }
                    } else {
                        const entry = self.nodes[entryId];
                        _ = try self.genRetainedTempExpr(entry.head.mapEntry.right, discardTopExprReg);
                    }
                }

                if (!discardTopExprReg) {
                    if (self.vm.structs.buf[sid].numFields <= 4) {
                        try self.pushOptionalDebugSym(nodeId);
                        try self.buf.pushOpSlice(.objectSmall, &.{ @intCast(u8, sid), argStartLocal, @intCast(u8, numFields), dst });
                    } else {
                        try self.buf.pushOpSlice(.object, &.{ @intCast(u8, sid), argStartLocal, @intCast(u8, numFields), dst });
                    }
                    if (!retainEscapeTop and self.isTempLocal(dst)) {
                        try self.setReservedTempLocal(dst);
                    }
                    return GenValue.initTempValue(dst, sema.AnyType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .if_expr => {
                return self.genIfExpr(nodeId, dst, retainEscapeTop, discardTopExprReg);
            },
            .map_literal => {
                const operandStart = self.operandStack.items.len;
                defer self.operandStack.items.len = operandStart;

                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                const argStartLocal = self.advanceNextTempLocalPastArcTemps();

                var i: u32 = 0;
                var entry_id = node.head.child_head;
                while (entry_id != NullId) : (i += 1) {
                    var entry = self.nodes[entry_id];
                    const key = self.nodes[entry.head.mapEntry.left];

                    if (!discardTopExprReg) {
                        switch (key.node_t) {
                            .ident => {
                                const name = self.getNodeTokenString(key);
                                const idx = try self.buf.getOrPushStringConst(name);
                                try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                            },
                            .string => {
                                const name = self.getNodeTokenString(key);
                                const idx = try self.buf.getOrPushStringConst(name);
                                try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                            },
                            else => stdx.panicFmt("unsupported key {}", .{key.node_t}),
                        }
                    }

                    _ = try self.genRetainedTempExpr(entry.head.mapEntry.right, discardTopExprReg);
                    entry_id = entry.next;
                }

                if (!discardTopExprReg) {
                    if (i == 0) {
                        try self.buf.pushOp1(.mapEmpty, dst);
                    } else {
                        try self.buf.pushOp3(.map, argStartLocal, @intCast(u8, i), dst);
                        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
                    }

                    if (!retainEscapeTop and self.isTempLocal(dst)) {
                        try self.setReservedTempLocal(dst);
                    }
                    return self.initGenValue(dst, sema.MapType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .arr_literal => {
                const startLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startLocal);

                const argStartLocal = self.advanceNextTempLocalPastArcTemps();

                var exprId = node.head.child_head;
                var i: u32 = 0;
                while (exprId != NullId) : (i += 1) {
                    const expr = self.nodes[exprId];
                    _ = try self.genRetainedTempExpr(exprId, discardTopExprReg);
                    exprId = expr.next;
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOp3(.list, argStartLocal, @intCast(u8, i), dst);
                    if (!retainEscapeTop and self.isTempLocal(dst)) {
                        try self.setReservedTempLocal(dst);
                    }
                    return self.initGenValue(dst, sema.ListType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .arr_range_expr => {
                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                var usedDstAsTemp = !self.canUseLocalAsTemp(dst);

                // Parent value.
                const parentv = try self.genExprToDestOrTempLocal(node.head.arr_range_expr.arr, dst, &usedDstAsTemp, discardTopExprReg);

                // Range left value.
                var leftv: GenValue = undefined;
                if (node.head.arr_range_expr.left == NullId) {
                    if (!discardTopExprReg) {
                        if (usedDstAsTemp) {
                            const leftDst = try self.nextFreeTempLocal();
                            leftv = try self.genConst(0, leftDst);
                        } else {
                            leftv = try self.genConst(0, dst);
                            usedDstAsTemp = true;
                        }
                    }
                } else {
                    leftv = try self.genExprToDestOrTempLocal(node.head.arr_range_expr.left, dst, &usedDstAsTemp, discardTopExprReg);
                }

                // Range right value.
                var rightv: GenValue = undefined;
                if (node.head.arr_range_expr.right == NullId) {
                    if (!discardTopExprReg) {
                        if (usedDstAsTemp) {
                            const rightDst = try self.nextFreeTempLocal();
                            rightv = try self.genConst(-1, rightDst);
                        } else {
                            rightv = try self.genConst(-1, dst);
                            usedDstAsTemp = true;
                        }
                    }
                } else {
                    rightv = try self.genExprToDestOrTempLocal(node.head.arr_range_expr.right, dst, &usedDstAsTemp, discardTopExprReg);
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOpSlice(.slice, &.{ parentv.local, leftv.local, rightv.local, dst });
                    if (!retainEscapeTop and self.isTempLocal(dst)) {
                        try self.setReservedTempLocal(dst);
                    }
                    return self.initGenValue(dst, sema.ListType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .arr_access_expr => {
                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                var usedDstAsTemp = !self.canUseLocalAsTemp(dst);

                // Gen left.
                const leftv = try self.genExprToDestOrTempLocal(node.head.left_right.left, dst, &usedDstAsTemp, discardTopExprReg);

                // Gen index.
                const index = self.nodes[node.head.left_right.right];
                const isReverseIndex = index.node_t == .unary_expr and index.head.unary.op == .minus;
                const indexv = try self.genExprToDestOrTempLocal(node.head.left_right.right, dst, &usedDstAsTemp, discardTopExprReg);

                if (!discardTopExprReg) {
                    if (isReverseIndex) {
                        try self.buf.pushOp3(.reverseIndex, leftv.local, indexv.local, dst);
                        if (!retainEscapeTop and self.isTempLocal(dst)) {
                            try self.setReservedTempLocal(dst);
                        }
                    } else {
                        try self.buf.pushOp3(.index, leftv.local, indexv.local, dst);
                        if (!retainEscapeTop and self.isTempLocal(dst)) {
                            try self.setReservedTempLocal(dst);
                        }
                    }
                    return self.initGenValue(dst, sema.AnyType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .accessExpr => {
                if (node.head.accessExpr.semaSymId != NullId) {
                    const sym = self.semaSyms.items[node.head.accessExpr.semaSymId];
                    if (self.genGetResolvedSym(node.head.accessExpr.semaSymId)) |rsym| {
                        const resolvedParentId = if (sym.key.parentId == NullId) NullId else self.semaSyms.items[sym.key.parentId].resolvedSymId;
                        if (rsym.symT == .variable) {
                            if (self.vm.getVarSym(resolvedParentId, sym.getName())) |symId| {
                                if (!discardTopExprReg) {
                                    // Static variable.
                                    try self.buf.pushOp2(.static, @intCast(u8, symId), dst);
                                    return self.initGenValue(dst, sema.AnyType);
                                } else {
                                    return GenValue.initNoValue();
                                }
                            }
                        }
                    }
                    return self.reportErrorAt("Unsupported sym: {}", &.{v(sym.getName())}, nodeId);
                }

                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                var usedDstAsTemp = !self.canUseLocalAsTemp(dst);

                // Left side.
                const leftv = try self.genExprToDestOrTempLocal(node.head.accessExpr.left, dst, &usedDstAsTemp, discardTopExprReg);

                // Right should be an ident.
                const right = self.nodes[node.head.accessExpr.right];

                const name = self.getNodeTokenString(right);
                const fieldId = try self.vm.ensureFieldSym(name);

                if (!discardTopExprReg) {
                    if (retainEscapeTop) {
                        try self.buf.pushOpSlice(.fieldRetain, &.{ leftv.local, dst, @intCast(u8, fieldId), 0, 0, 0 });
                    } else {
                        try self.buf.pushOpSlice(.field, &.{ leftv.local, dst, @intCast(u8, fieldId), 0, 0, 0 });
                    }
                    try self.pushDebugSym(nodeId);
                    return self.initGenValue(dst, sema.AnyType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .comptExpr => {
                const child = self.nodes[node.head.child_head];
                if (child.node_t == .call_expr) {
                    const callee = self.nodes[child.head.func_call.callee];
                    const name = self.getNodeTokenString(callee);
                    if (std.mem.eql(u8, name, "compilerDumpLocals")) {
                        try self.dumpLocals();
                    } else if (std.mem.eql(u8, name, "compilerDumpBytecode")) {
                        self.buf.dump();
                    }
                    try self.buf.pushOp1(.none, dst);
                    return self.initGenValue(dst, sema.AnyType);
                } else {
                    return self.reportError("Unsupported compt expr {}", &.{fmt.v(child.node_t)});
                }
            },
            .tryExpr => {
                const child = try self.genExpr(node.head.child_head, false);
                try self.buf.pushOp2(.tryValue, child.local, dst);
                try self.pushDebugSym(nodeId);
                return self.initGenValue(dst, sema.AnyType);
            },
            .unary_expr => {
                const op = node.head.unary.op;
                switch (op) {
                    .minus => {
                        const child = try self.genExpr(node.head.unary.child, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp2(.neg, child.local, dst);
                            return self.initGenValue(dst, sema.NumberType);
                        } else return GenValue.initNoValue();
                    },
                    .not => {
                        const child = try self.genExpr(node.head.unary.child, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp2(.not, child.local, dst);
                            return self.initGenValue(dst, sema.BoolType);
                        } else return GenValue.initNoValue();
                    },
                    .bitwiseNot => {
                        const child = try self.genExpr(node.head.unary.child, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp2(.bitwiseNot, child.local, dst);
                            return self.initGenValue(dst, sema.NumberType);
                        } else return GenValue.initNoValue();
                    },
                    // else => return self.reportErrorAt("Unsupported unary op: {}", .{op}, node),
                }
            },
            .binExpr => {
                const left = node.head.binExpr.left;
                const right = node.head.binExpr.right;

                const op = node.head.binExpr.op;
                switch (op) {
                    .slash => {
                        return self.genPushBinOp(.div, left, right, sema.NumberType, dst, discardTopExprReg);
                    },
                    .percent => {
                        return self.genPushBinOp(.mod, left, right, sema.NumberType, dst, discardTopExprReg);
                    },
                    .caret => {
                        return self.genPushBinOp(.pow, left, right, sema.NumberType, dst, discardTopExprReg);
                    },
                    .star => {
                        return self.genPushBinOp(.mul, left, right, sema.NumberType, dst, discardTopExprReg);
                    },
                    .plus => {
                        const startTempLocal = self.curBlock.firstFreeTempLocal;
                        defer self.computeNextTempLocalFrom(startTempLocal);

                        var usedDstAsTemp = !self.canUseLocalAsTemp(dst);
                        const leftv = try self.genExprToDestOrTempLocal2(requestedType, left, dst, &usedDstAsTemp, discardTopExprReg);
                        const rightv = try self.genExprToDestOrTempLocal2(requestedType, right, dst, &usedDstAsTemp, discardTopExprReg);
                        if (!discardTopExprReg) {
                            if (leftv.vtype.typeT == .int and rightv.vtype.typeT == .int) {
                                try self.buf.pushOp3(.addInt, leftv.local, rightv.local, dst);
                                return self.initGenValue(dst, sema.IntegerType);
                            }
                            try self.buf.pushOp3(.add, leftv.local, rightv.local, dst);
                            try self.pushDebugSym(nodeId);
                            return self.initGenValue(dst, sema.NumberType);
                        } else {
                            return GenValue.initNoValue();
                        }
                    },
                    .minus => {
                        const startTempLocal = self.curBlock.firstFreeTempLocal;
                        defer self.computeNextTempLocalFrom(startTempLocal);

                        var usedDstAsTemp = !self.canUseLocalAsTemp(dst);
                        const leftv = try self.genExprToDestOrTempLocal2(requestedType, left, dst, &usedDstAsTemp, discardTopExprReg);
                        const rightv = try self.genExprToDestOrTempLocal2(requestedType, right, dst, &usedDstAsTemp, discardTopExprReg);
                        if (!discardTopExprReg) {
                            if (leftv.vtype.typeT == .int and rightv.vtype.typeT == .int) {
                                try self.buf.pushOp3(.minusInt, leftv.local, rightv.local, dst);
                                return self.initGenValue(dst, sema.IntegerType);
                            }
                            try self.buf.pushOp3(.minus, leftv.local, rightv.local, dst);
                            return self.initGenValue(dst, sema.NumberType);
                        } else return GenValue.initNoValue();
                    },
                    .equal_equal => {
                        return self.genPushBinOp(.compare, left, right, sema.BoolType, dst, discardTopExprReg);
                    },
                    .bang_equal => {
                        return self.genPushBinOp(.compareNot, left, right, sema.BoolType, dst, discardTopExprReg);
                    },
                    .less => {
                        const startTempLocal = self.curBlock.firstFreeTempLocal;
                        defer self.computeNextTempLocalFrom(startTempLocal);

                        var usedDstAsTemp = !self.canUseLocalAsTemp(dst);
                        var leftv: GenValue = undefined;
                        var rightv: GenValue = undefined;
                        if (node.head.binExpr.semaCanRequestIntegerOperands) {
                            leftv = try self.genExprToDestOrTempLocal2(sema.IntegerType, left, dst, &usedDstAsTemp, discardTopExprReg);
                            rightv = try self.genExprToDestOrTempLocal2(sema.IntegerType, right, dst, &usedDstAsTemp, discardTopExprReg);
                        } else {
                            leftv = try self.genExprToDestOrTempLocal(left, dst, &usedDstAsTemp, discardTopExprReg);
                            rightv = try self.genExprToDestOrTempLocal(right, dst, &usedDstAsTemp, discardTopExprReg);
                        }
                        if (!discardTopExprReg) {
                            if (node.head.binExpr.semaCanRequestIntegerOperands) {
                                try self.buf.pushOp3(.lessInt, leftv.local, rightv.local, dst);
                                return self.initGenValue(dst, sema.BoolType);
                            }
                            try self.buf.pushOp3(.less, leftv.local, rightv.local, dst);
                            return self.initGenValue(dst, sema.BoolType);
                        } else return GenValue.initNoValue();
                    },
                    .less_equal => {
                        return self.genPushBinOp(.lessEqual, left, right, sema.BoolType, dst, discardTopExprReg);
                    },
                    .greater => {
                        return self.genPushBinOp(.greater, left, right, sema.BoolType, dst, discardTopExprReg);
                    },
                    .greater_equal => {
                        return self.genPushBinOp(.greaterEqual, left, right, sema.BoolType, dst, discardTopExprReg);
                    },
                    .bitwiseAnd => {
                        return self.genPushBinOp(.bitwiseAnd, left, right, sema.NumberType, dst, discardTopExprReg);
                    },
                    .bitwiseOr => {
                        return self.genPushBinOp(.bitwiseOr, left, right, sema.NumberType, dst, discardTopExprReg);
                    },
                    .bitwiseXor => {
                        return self.genPushBinOp(.bitwiseXor, left, right, sema.NumberType, dst, discardTopExprReg);
                    },
                    .bitwiseLeftShift => {
                        return self.genPushBinOp(.bitwiseLeftShift, left, right, sema.NumberType, dst, discardTopExprReg);
                    },
                    .bitwiseRightShift => {
                        return self.genPushBinOp(.bitwiseRightShift, left, right, sema.NumberType, dst, discardTopExprReg);
                    },
                    .and_op => {
                        const startTempLocal = self.curBlock.firstFreeTempLocal;
                        defer self.computeNextTempLocalFrom(startTempLocal);

                        const leftv = try self.genExprTo(left, dst, false, discardTopExprReg);
                        const jumpPc = try self.pushEmptyJumpNotCond(leftv.local);
                        const rightv = try self.genExprTo(right, dst, false, discardTopExprReg);
                        self.patchJumpToCurrent(jumpPc);

                        if (leftv.vtype.typeT == rightv.vtype.typeT) {
                            if (retainEscapeTop and leftv.vtype.rcCandidate) {
                                try self.buf.pushOp1(.retain, dst);
                            }
                            return self.initGenValue(dst, leftv.vtype);
                        } else {
                            if (retainEscapeTop and (leftv.vtype.rcCandidate or rightv.vtype.rcCandidate)) {
                                try self.buf.pushOp1(.retain, dst);
                            }
                            return self.initGenValue(dst, sema.AnyType);
                        }
                    },
                    .or_op => {
                        const startTempLocal = self.curBlock.firstFreeTempLocal;
                        defer self.computeNextTempLocalFrom(startTempLocal);

                        const leftv = try self.genExprTo(left, dst, retainEscapeTop, discardTopExprReg);
                        const jumpPc = try self.pushEmptyJumpCond(leftv.local);
                        const rightv = try self.genExprTo(right, dst, retainEscapeTop, discardTopExprReg);
                        self.patchJumpToCurrent(jumpPc);

                        if (leftv.vtype.typeT == rightv.vtype.typeT) {
                            return self.initGenValue(dst, leftv.vtype);
                        } else return self.initGenValue(dst, sema.AnyType);
                    },
                    else => return self.reportErrorAt("Unsupported binary op: {}", &.{fmt.v(op)}, nodeId),
                }
            },
            .call_expr => {
                const val = try self.genCallExpr(nodeId, dst, discardTopExprReg, false);
                if (dst != val.local) {
                    try self.buf.pushOp2(.copy, val.local, dst);
                }
                if (!discardTopExprReg and !retainEscapeTop and self.isTempLocal(dst)) {
                    try self.setReservedTempLocal(dst);
                }
                return self.initGenValue(dst, val.vtype);
            },
            .lambda_multi => {
                if (!discardTopExprReg) {
                    const jumpPc = try self.pushEmptyJump();

                    self.nextSemaBlock();
                    try self.pushBlock();
                    self.curBlock.frameLoc = nodeId;

                    const jumpStackStart = self.blockJumpStack.items.len;
                    const opStart = @intCast(u32, self.buf.ops.items.len);

                    // Generate function body.
                    const func = self.funcDecls[node.head.func.decl_id];
                    try self.reserveFuncParams();
                    try self.genInitLocals();
                    const numParams = @intCast(u8, func.params.end - func.params.start);

                    try self.genStatements(node.head.func.body_head, false);

                    try self.endLocals();
                    try self.buf.pushOp(.ret0);
                    self.patchJumpToCurrent(jumpPc);

                    const sblock = sema.curBlock(self);
                    const numLocals = @intCast(u8, self.curBlock.numLocals + self.curBlock.numTempLocals);
                    const numCaptured = @intCast(u8, sblock.params.items.len - numParams - 1);

                    self.patchBlockJumps(jumpStackStart);
                    self.blockJumpStack.items.len = jumpStackStart;

                    self.popBlock();
                    self.prevSemaBlock();

                    if (numCaptured == 0) {
                        const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
                        try self.buf.pushOpSlice(.lambda, &.{ funcPcOffset, numParams, numLocals, dst });
                        if (!retainEscapeTop and self.isTempLocal(dst)) {
                            try self.setReservedTempLocal(dst);
                        }
                        return self.initGenValue(dst, sema.AnyType);
                    } else {
                        const operandStart = self.operandStack.items.len;
                        defer self.operandStack.items.len = operandStart;

                        // Retain captured vars.
                        for (sblock.params.items) |varId| {
                            const svar = self.vars.items[varId];
                            if (svar.isCaptured) {
                                const pId = self.capVarDescs.get(varId).?.user;
                                const pvar = &self.vars.items[pId];

                                if (svar.isBoxed) {
                                    try self.buf.pushOp1(.retain, pvar.local);
                                    try self.pushTempOperand(pvar.local);
                                }
                            }
                        }

                        const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
                        try self.buf.pushOpSlice(.closure, &.{ funcPcOffset, numParams, numCaptured, numLocals, dst });
                        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
                        if (!retainEscapeTop and self.isTempLocal(dst)) {
                            try self.setReservedTempLocal(dst);
                        }
                        return self.initGenValue(dst, sema.AnyType);
                    }
                } else {
                    return GenValue.initNoValue();
                }
            },
            .lambda_expr => {
                if (!discardTopExprReg) {
                    const jumpPc = try self.pushEmptyJump();

                    self.nextSemaBlock();
                    try self.pushBlock();
                    const opStart = @intCast(u32, self.buf.ops.items.len);

                    // Generate function body.
                    const func = self.funcDecls[node.head.func.decl_id];
                    try self.reserveFuncParams();
                    try self.genInitLocals();
                    const numParams = @intCast(u8, func.params.end - func.params.start);
                    _ = try self.genRetainedExprTo(node.head.func.body_head, 0, false);
                    try self.endLocals();
                    try self.buf.pushOp(.ret1);
                    self.patchJumpToCurrent(jumpPc);

                    const sblock = sema.curBlock(self);
                    const numLocals = @intCast(u8, self.curBlock.numLocals + self.curBlock.numTempLocals);
                    const numCaptured = @intCast(u8, sblock.params.items.len - numParams - 1);

                    self.popBlock();
                    self.prevSemaBlock();

                    if (numCaptured == 0) {
                        const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
                        try self.buf.pushOpSlice(.lambda, &.{ funcPcOffset, numParams, numLocals, dst });
                        if (!retainEscapeTop and self.isTempLocal(dst)) {
                            try self.setReservedTempLocal(dst);
                        }
                        return self.initGenValue(dst, sema.AnyType);
                    } else {
                        const operandStart = self.operandStack.items.len;
                        defer self.operandStack.items.len = operandStart;

                        // Retain captured vars.
                        for (sblock.params.items) |varId| {
                            const svar = self.vars.items[varId];
                            if (svar.isCaptured) {
                                const pId = self.capVarDescs.get(varId).?.user;
                                const pvar = &self.vars.items[pId];

                                if (svar.isBoxed) {
                                    try self.buf.pushOp1(.retain, pvar.local);
                                    try self.pushTempOperand(pvar.local);
                                }
                            }
                        }

                        const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
                        try self.buf.pushOpSlice(.closure, &.{ funcPcOffset, numParams, numCaptured, numLocals, dst });
                        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
                        if (!retainEscapeTop and self.isTempLocal(dst)) {
                            try self.setReservedTempLocal(dst);
                        }
                        return self.initGenValue(dst, sema.AnyType);
                    }
                } else {
                    return GenValue.initNoValue();
                }
            },
            .coresume => {
                const fiber = try self.genRetainedTempExpr(node.head.child_head, false);
                if (!discardTopExprReg) {
                    try self.buf.pushOp2(.coresume, fiber.local, dst);
                } else {
                    try self.buf.pushOp2(.coresume, fiber.local, NullIdU8);
                }
                return self.initGenValue(dst, sema.AnyType);
            },
            .coyield => {
                const pc = self.buf.ops.items.len;
                try self.buf.pushOp2(.coyield, 0, 0);
                try self.blockJumpStack.append(self.alloc, .{ .jumpT = .jumpToEndLocals, .pc = @intCast(u32, pc) });

                // TODO: return coyield expression.
                if (!discardTopExprReg) {
                    return try self.genNone(dst);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .coinit => {
                _ = try self.genCallExpr(node.head.child_head, dst, discardTopExprReg, true);
                return self.initGenValue(dst, sema.FiberType);
            },
            else => {
                return self.reportError("Unsupported {}", &.{fmt.v(node.node_t)});
            }
        }
    }

    fn setErrorAt(self: *VMcompiler, format: []const u8, args: []const fmt.FmtValue, nodeId: cy.NodeId) !void {
        self.alloc.free(self.lastErr);
        self.lastErr = try fmt.allocFormat(self.alloc, format, args);
        self.lastErrNode = nodeId;
    }

    pub fn reportError(self: *VMcompiler, format: []const u8, args: []const fmt.FmtValue) error{CompileError, OutOfMemory, FormatError} {
        return self.reportErrorAt(format, args, self.curNodeId);
    }

    pub fn reportErrorAt(self: *VMcompiler, format: []const u8, args: []const fmt.FmtValue, nodeId: cy.NodeId) error{CompileError, OutOfMemory, FormatError} {
        try self.setErrorAt(format, args, nodeId);
        return error.CompileError;
    }

    pub fn getNodeTokenString(self: *const VMcompiler, node: cy.Node) []const u8 {
        const token = self.tokens[node.start_token];
        return self.src[token.pos()..token.data.end_pos];
    }

    /// An optional debug sym is only included in Debug builds.
    fn pushOptionalDebugSym(self: *VMcompiler, nodeId: cy.NodeId) !void {
        if (builtin.mode == .Debug) {
            try self.buf.pushDebugSym(self.buf.ops.items.len, 0, nodeId, self.curBlock.frameLoc);
        }
    }

    fn pushDebugSym(self: *VMcompiler, nodeId: cy.NodeId) !void {
        try self.buf.pushDebugSym(self.buf.ops.items.len, 0, nodeId, self.curBlock.frameLoc);
    }

    fn pushDebugSymAt(self: *VMcompiler, pc: usize, nodeId: cy.NodeId) !void {
        try self.buf.pushDebugSym(pc, 0, nodeId, self.curBlock.frameLoc);
    }
};

pub const ResultView = struct {
    buf: cy.ByteCodeBuffer,
    hasError: bool,
};

const LocalId = u8;

// TODO: Rename to GenBlock
const Block = struct {
    /// This includes the return info, function params, captured params, and local vars.
    /// Does not include temp locals.
    numLocals: u32,
    frameLoc: cy.NodeId = NullId,
    resolvedSymId: sema.ResolvedSymId = NullId,
    endLocalsPc: u32,

    /// These are used for rvalues and function args.
    /// At the end of the block, the total stack size needed for the function body is known.
    numTempLocals: u8,

    /// Starts at `numLocals`.
    /// Temp locals are allocated from the end of the user locals towards the right.
    firstFreeTempLocal: u8,

    /// Start of the first reserved temp local.
    reservedTempLocalStart: u32,

    /// Start of the first retained temp local of the current ARC expr.
    /// This must be kept updated as codegen walks the ast so that sub expressions
    /// knows which ARC expr they belong to.
    arcTempLocalStart: u32,

    fn init() Block {
        return .{
            .numLocals = 0,
            .endLocalsPc = 0,
            .numTempLocals = 0,
            .firstFreeTempLocal = 0,
            .reservedTempLocalStart = 0,
            .arcTempLocalStart = 0,
        };
    }

    fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }

    fn getRequiredStackSize(self: *const Block) u8 {
        return @intCast(u8, self.numLocals + self.numTempLocals);
    }
};

const VarInfo = struct {
    hasStaticType: bool,
};

const BlockJumpType = enum {
    jumpToEndLocals,
};

const BlockJump = struct {
    jumpT: BlockJumpType,
    pc: u32,
};

const SubBlockJumpType = enum {
    /// Each if/else body contains a break at the end to jump out of the if block.
    ifBreak,
    /// Breaks out of a for loop.
    brk,
    /// Continues a for loop.
    cont,
};

const SubBlockJump = struct {
    jumpT: SubBlockJumpType,
    pc: u32,
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

fn isArcTempNode(nodeT: cy.NodeType) bool {
    switch (nodeT) {
        .call_expr,
        .arr_literal,
        .map_literal,
        .stringTemplate,
        .arr_access_expr,
        .coinit,
        .structInit => return true,
        else => return false,
    }
}

const GenValue = struct {
    vtype: sema.Type,
    local: LocalId,
    isTempLocal: bool,

    fn initNoValue() GenValue {
        return .{
            .vtype = undefined,
            .local = 0,
            .isTempLocal = false,
        };
    }

    fn initLocalValue(local: LocalId, vtype: sema.Type) GenValue {
        return .{
            .vtype = vtype,
            .local = local,
            .isTempLocal = false,
        };
    }

    fn initTempValue(local: LocalId, vtype: sema.Type) GenValue {
        return .{
            .vtype = vtype,
            .local = local,
            .isTempLocal = true,
        };
    }

    fn initSemaVar(svar: sema.LocalVar) GenValue {
        return .{
            .vtype = svar.vtype,
            .local = svar.local,
            .isTempLocal = false,
        };
    }
};

/// `buf` is assumed to be big enough.
pub fn unescapeString(buf: []u8, literal: []const u8) []const u8 {
    var newIdx: u32 = 0; 
    var i: u32 = 0;
    while (i < literal.len) : (newIdx += 1) {
        if (literal[i] == '\\') {
            switch (literal[i + 1]) {
                'n' => {
                    buf[newIdx] = '\n';
                },
                'r' => {
                    buf[newIdx] = '\r';
                },
                't' => {
                    buf[newIdx] = '\t';
                },
                else => {
                    buf[newIdx] = literal[i + 1];
                }
            }
            i += 2;
        } else {
            buf[newIdx] = literal[i];
            i += 1;
        }
    }
    return buf[0..newIdx];
}

const ReservedTempLocal = struct {
    local: LocalId,
};


const unexpected = stdx.fatal;

fn unexpectedFmt(format: []const u8, vals: []const fmt.FmtValue) noreturn {
    if (builtin.mode == .Debug) {
        fmt.printStderr(format, vals);
    }
    stdx.fatal();
}

const DataNode = packed struct {
    inner: packed union {
        funcSym: packed struct {
            symId: u24,
            capVarIdx: u8,
        },
    },
    next: u32,
};