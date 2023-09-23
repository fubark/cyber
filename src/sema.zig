const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const vmc = @import("vm_c.zig");
const rt = cy.rt;
const types = cy.types;
const bt = types.BuiltinTypeSymIds;
const Nullable = cy.Nullable;
const fmt = cy.fmt;
const v = fmt.v;

const TypeId = types.TypeId;

const vm_ = @import("vm.zig");

const log = cy.log.scoped(.sema);

const ValueAddrType = enum {
    frameOffset,
};

const ValueAddr = struct {
    addrT: ValueAddrType,
    inner: union {
        frameOffset: u32,
    },
};

const RegisterId = cy.register.RegisterId;

pub const LocalVarId = u32;

const LocalVarType = enum(u8) {
    local, 

    param,

    /// If a param is written to or turned into a Box, this local would be a copy of the param
    /// and `inner.paramCopy` would be the param idx.
    paramCopy,

    /// Whether this var references a static variable.
    staticAlias,

    parentLocalAlias,

    /// Whether this var references a parent object member.
    objectMemberAlias,

    parentObjectMemberAlias,
};

/// Represents a variable or alias in a block.
/// Local variables are given reserved registers on the stack frame.
/// Captured variables have box values at runtime.
pub const LocalVar = struct {
    type: LocalVarType,

    /// The current type of the var as the ast is traversed.
    /// This is updated when there is a variable assignment or a child block returns.
    vtype: TypeId,

    /// If non-null, points to the captured var idx in the closure.
    capturedIdx: u8 = cy.NullU8,


    /// Currently a captured var always needs to be boxed.
    /// In the future, the concept of a const variable could change this.
    isBoxed: bool = false,

    /// Indicates that at some point during the vars lifetime it was an rcCandidate.
    /// Since all exit paths jump to the same release inst, this flag is used to determine
    /// which vars need a release.
    lifetimeRcCandidate: bool,

    /// Local register offset assigned to this var.
    /// Locals are relative to the stack frame's start position.
    local: RegisterId = undefined,

    /// Since the same sema var is used later by codegen,
    /// use a flag to indicate whether the var has been loosely defined in the block. (eg. assigned to lvalue)
    /// Note that assigning inside a branch counts as defined.
    /// Entering an iter block will auto mark those as defined since the var could have been assigned by a previous iteration.
    isDefinedOnce: bool = false,

    /// Whether the var is currently declared (according to the language rules) at the current position in the current block. 
    /// When a sub block ends, this is set back to false.
    /// If this variable is an alias, this is always false.
    isDeclared: bool = false,

    inner: extern union {
        staticAlias: extern struct {
            csymId: CompactSymbolId,
        },
        paramIdx: u8,
        paramCopy: u8,
    } = undefined,

    name: if (builtin.mode == .Debug) []const u8 else void,

    pub inline fn isParentLocalAlias(self: LocalVar) bool {
        return self.capturedIdx != cy.NullU8;
    }
};

pub const CapVarDesc = extern union {
    /// The user of a captured var contains the SemaVarId back to the owner's var.
    user: LocalVarId,
};

const VarAndType = struct {
    id: LocalVarId,
    vtype: TypeId,
};

pub const SubBlockId = u32;

pub const SubBlock = struct {
    /// Save var start types for entering a codegen iter block.
    /// This can only be determined after the sema pass.
    /// This is used to initialize the var type when entering the codegen iter block so
    /// that the first `genSetVar` produces the correct `set` op.
    iterVarBeginTypes: std.ArrayListUnmanaged(VarAndType),

    /// Record any merged narrow types at the end of the subblock for codegen.
    endMergeTypes: std.ArrayListUnmanaged(VarAndType),

    /// Track which vars were assigned to in the current sub block.
    /// If the var was first assigned in a parent sub block, the type is saved in the map to
    /// be merged later with the ending var type.
    /// Can be freed after the end of block.
    prevVarTypes: std.AutoHashMapUnmanaged(LocalVarId, TypeId),

    /// Start of vars assigned in this block in `assignedVarStack`.
    /// When leaving this block, all assigned var types in this block are merged
    /// back to the parent scope.
    assignedVarStart: u32,

    /// Start of declared vars in this subblock in `declaredVarStack`.
    declaredVarStart: u32,

    /// Previous sema sub block.
    /// When this sub block ends, the previous sub block id is set as the current.
    prevSubBlockId: SubBlockId,

    pub fn init(prevSubBlockId: SubBlockId, assignedVarStart: usize, declaredVarStart: usize) SubBlock {
        return .{
            .assignedVarStart = @intCast(assignedVarStart),
            .declaredVarStart = @intCast(declaredVarStart),
            .iterVarBeginTypes = .{},
            .endMergeTypes = .{},
            .prevVarTypes = .{},
            .prevSubBlockId = prevSubBlockId,
        };
    }

    pub fn deinit(self: *SubBlock, alloc: std.mem.Allocator) void {
        self.iterVarBeginTypes.deinit(alloc);
        self.endMergeTypes.deinit(alloc);
    }
};

pub const BlockId = u32;

pub const Block = struct {
    /// Local vars defined in this block. Does not include function params.
    locals: std.ArrayListUnmanaged(LocalVarId),

    /// Param vars for function blocks.
    /// Codegen will reserve these first for the calling convention layout.
    params: std.ArrayListUnmanaged(LocalVarId),

    /// Captured vars. 
    captures: std.ArrayListUnmanaged(LocalVarId),

    /// Name to var.
    /// This can be deinited after ending the sema block.
    nameToVar: std.StringHashMapUnmanaged(LocalVarId),

    /// First sub block id is recorded so the rest can be obtained by advancing
    /// the id in the same order it was traversed in the sema pass.
    firstSubBlockId: SubBlockId,

    /// Current sub block depth.
    subBlockDepth: u32,

    /// Index into `Chunk.funcDecls`. Main block if `NullId`.
    funcDeclId: u32,

    /// Whether this block belongs to a static function.
    isStaticFuncBlock: bool,

    /// Whether this block belongs to an object method.
    isMethodBlock: bool = false,

    /// Whether temporaries (nameToVar) was deinit.
    deinitedTemps: bool,

    pub fn init(funcDeclId: cy.NodeId, firstSubBlockId: SubBlockId, isStaticFuncBlock: bool) Block {
        return .{
            .nameToVar = .{},
            .locals = .{},
            .params = .{},
            .subBlockDepth = 0,
            .funcDeclId = funcDeclId,
            .firstSubBlockId = firstSubBlockId,
            .isStaticFuncBlock = isStaticFuncBlock,
            .deinitedTemps = false,
            .captures = .{},
        };
    }

    pub fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        self.locals.deinit(alloc);
        self.params.deinit(alloc);

        // Deinit for CompileError during sema.
        self.deinitTemps(alloc);

        self.captures.deinit(alloc);
    }

    fn deinitTemps(self: *Block, alloc: std.mem.Allocator) void {
        if (!self.deinitedTemps) {
            self.nameToVar.deinit(alloc);
            self.deinitedTemps = true;
        }
    }

    fn getReturnType(self: *const Block, c: *cy.Chunk) !TypeId {
        if (self.funcDeclId != cy.NullId) {
            return c.semaFuncDecls.items[self.funcDeclId].getReturnType();
        } else {
            return bt.Any;
        }
    }
};

pub fn getBlockNodeId(c: *cy.Chunk, block: *Block) cy.NodeId {
    if (block.funcDeclId == cy.NullId) {
        return c.parserAstRootId;
    } else {
        const decl = c.semaFuncDecls.items[block.funcDeclId];
        return decl.nodeId;
    }
}

pub const NameSymId = u32;

pub const Name = struct {
    ptr: [*]const u8,
    len: u32,
    owned: bool,

    pub fn getName(self: Name) []const u8 {
        return self.ptr[0..self.len];
    }
};

pub fn getName(c: *const cy.VMcompiler, nameId: NameSymId) []const u8 {
    const name = c.sema.nameSyms.items[nameId];
    return name.ptr[0..name.len];
}

/// This is only called after symbol resolving.
pub fn symHasStaticInitializer(c: *const cy.Chunk, csymId: CompactSymbolId) bool {
    if (csymId.isFuncSymId) {
        return c.compiler.sema.getFuncSym(csymId.id).hasStaticInitializer;
    } else {
        const rsym = c.compiler.sema.getSymbol(csymId.id);
        if (rsym.symT == .variable) {
            return rsym.inner.variable.declId != cy.NullId;
        }
    }
    return false;
}

pub const FuncSymId = u32;
pub const FuncSym = struct {
    chunkId: ChunkId,
    
    /// Can be the cy.NullId for native functions.
    declId: FuncDeclId,

    /// Access to symId, funcSigId.
    key: ResolvedFuncSymKey,

    /// Return type.
    retType: TypeId,

    /// Whether this func has a static initializer.
    hasStaticInitializer: bool,
    
    /// Whether the symbol has been or is in the process of generating it's static initializer.
    genStaticInitVisited: bool = false,

    pub fn getSymbolId(self: FuncSym) SymbolId {
        return self.key.resolvedFuncSymKey.symId;
    }

    pub fn getFuncSigId(self: FuncSym) FuncSigId {
        return self.key.resolvedFuncSymKey.funcSigId;
    }
};

const SymbolType = enum(u8) {
    func,
    variable,
    object,
    enumType,
    enumMember,
    module,
    builtinType,
    internal,
};

pub const SymbolId = u32;

const SymbolData = extern union {
    func: extern struct {
        /// Refers to exactly one resolved func sym.
        /// funcSymId == cy.NullId indicates this sym is overloaded;
        /// more than one func shares the same symbol. To disambiguate,
        /// `resolvedFuncSymMap` must be queried with a resolvedFuncSymKey.
        funcSymId: FuncSymId,
    },
    variable: extern struct {
        chunkId: ChunkId,
        declId: cy.NodeId,
        rTypeSymId: types.TypeId,
    },
    object: extern struct {
        modId: cy.ModuleId,
        typeId: rt.TypeId,
    },
    enumType: extern struct {
        modId: cy.ModuleId,
        enumId: u32,
    },
    enumMember: extern struct {
        enumId: u32,
        memberId: u32,
    },
    module: extern struct {
        id: cy.ModuleId,
    },
    builtinType: extern struct {
        modId: cy.ModuleId,
        typeId: rt.TypeId,
    },
};

/// Only module members that are used are included as Symbols.
pub const Symbol = struct {
    symT: SymbolType,
    /// Used to backtrack and build the full sym name.
    key: ResolvedSymKey,
    inner: SymbolData,
    /// Whether the symbol is exported.
    exported: bool,
    /// Whether the symbol has been or is in the process of generating it's static initializer.
    genStaticInitVisited: bool = false,

    pub fn getObjectTypeId(self: Symbol, vm: *cy.VM) ?rt.TypeId {
        return vm.getObjectTypeId(self.key.resolvedSymKey.parentSymId, self.key.resolvedSymKey.nameId);
    }

    pub fn getModuleId(self: Symbol) ?cy.ModuleId {
        switch (self.symT) {
            .module => {
                return self.inner.module.id;
            },
            .enumType => {
                return self.inner.enumType.modId;
            },
            .object => {
                return self.inner.object.modId;
            },
            .builtinType => {
                return self.inner.builtinType.modId;
            },
            .internal,
            .enumMember,
            .func,
            .variable => {
                return null;
            },
        }
    }
};

/// Additional info attached to a initializer symbol.
pub const InitializerSym = struct {
    /// This points to a list of sema sym ids in `bufU32` that it depends on for initialization.
    depsStart: u32,
    depsEnd: u32,
};

const ModuleSymKey = cy.hash.KeyU64;
const ChunkId = u32;

pub const LocalSymKey = cy.hash.KeyU64;
pub const LocalSym = struct {
    /// Can be NullId since some syms are not resolved until they are used. eg. ImportAll syms
    symId: Nullable(SymbolId),
    funcSymId: Nullable(FuncSymId),

    /// Which module to find the sym in.
    parentSymId: SymbolId,
};

pub const ResolvedSymKey = cy.hash.KeyU64;
pub const ResolvedFuncSymKey = cy.hash.KeyU64;

const ObjectMemberKey = cy.hash.KeyU64;

pub fn semaStmts(self: *cy.Chunk, head: cy.NodeId) anyerror!void {
    var cur_id = head;
    while (cur_id != cy.NullId) {
        const node = self.nodes[cur_id];
        try semaStmt(self, cur_id);
        cur_id = node.next;
    }
}

pub fn semaStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    // log.debug("sema stmt {}", .{node.node_t});
    c.curNodeId = nodeId;
    const node = c.nodes[nodeId];
    switch (node.node_t) {
        .pass_stmt => {
            return;
        },
        .expr_stmt => {
            _ = try semaExpr(c, node.head.child_head);
        },
        .breakStmt => {
            return;
        },
        .continueStmt => {
            return;
        },
        .localDecl => {
            _ = try localDecl(c, nodeId);
        },
        .opAssignStmt => {
            const left = c.nodes[node.head.opAssignStmt.left];
            if (left.node_t == .ident) {
                const rtype = try semaExpr(c, node.head.opAssignStmt.right);
                c.nodes[nodeId].head.opAssignStmt.semaGenStrat = .generic;
                _ = try assignVar(c, node.head.opAssignStmt.left, rtype);
            } else if (left.node_t == .accessExpr) {
                _ = try accessExpr(c, node.head.opAssignStmt.left);
                _ = try semaExpr(c, node.head.opAssignStmt.right);
                c.nodes[nodeId].head.opAssignStmt.semaGenStrat = .generic;
            } else if (left.node_t == .indexExpr) {
                _ = try semaExpr(c, node.head.opAssignStmt.left);
                _ = try semaExpr(c, node.head.opAssignStmt.right);
                c.nodes[nodeId].head.opAssignStmt.semaGenStrat = .generic;
            } else {
                return c.reportErrorAt("Assignment to the left {} is not allowed.", &.{fmt.v(left.node_t)}, nodeId);
            }
        },
        .assign_stmt => {
            const left = c.nodes[node.head.left_right.left];
            if (left.node_t == .ident) {
                const right = c.nodes[node.head.left_right.right];
                if (right.node_t == .matchBlock) {
                    const rtype = try matchBlock(c, node.head.left_right.right, true);
                    _ = try assignVar(c, node.head.left_right.left, rtype);
                } else {
                    const rtype = try semaExpr(c, node.head.left_right.right);
                    _ = try assignVar(c, node.head.left_right.left, rtype);
                }
            } else if (left.node_t == .indexExpr) {
                const leftId = node.head.left_right.left;
                const leftT = try semaExpr(c, left.head.indexExpr.left);
                if (leftT == bt.List) {
                    // Specialized.
                    _ = try semaExprType(c, left.head.indexExpr.right, bt.Integer);
                    _ = try semaExpr(c, node.head.left_right.right);
                    c.nodes[leftId].head.indexExpr.semaGenStrat = .specialized;
                } else if (leftT == bt.Map) {
                    // Specialized.
                    _ = try semaExprType(c, left.head.indexExpr.right, bt.Any);
                    _ = try semaExpr(c, node.head.left_right.right);
                    c.nodes[leftId].head.indexExpr.semaGenStrat = .specialized;
                } else {
                    _ = try semaExpr(c, left.head.indexExpr.right);
                    _ = try semaExpr(c, node.head.left_right.right);
                    c.nodes[leftId].head.indexExpr.semaGenStrat = .generic;
                }
            } else if (left.node_t == .accessExpr) {
                const res = try accessExpr(c, node.head.left_right.left);
                const rightT = try semaExpr(c, node.head.left_right.right);
                if (rightT != bt.Dynamic and res.recvT != bt.Any) {
                    // Compile-time type check on the field.
                    if (!types.isTypeSymCompat(c.compiler, rightT, res.exprT)) {
                        const fieldTypeName = getSymName(c.compiler, res.exprT);
                        const rightTypeName = getSymName(c.compiler, rightT);
                        return c.reportError("Assigning to `{}` member with incompatible type `{}`.", &.{v(fieldTypeName), v(rightTypeName)});
                    }
                }
            } else {
                return c.reportErrorAt("Assignment to the left {} is not allowed.", &.{fmt.v(left.node_t)}, nodeId);
            }
        },
        .staticDecl => {
            try staticDecl(c, nodeId, true);
        },
        .typeAliasDecl => {
            const nameN = c.nodes[node.head.typeAliasDecl.name];
            const name = c.getNodeTokenString(nameN);
            const nameId = try ensureNameSym(c.compiler, name);

            const symId = try getOrResolveTypeSymFromSpecNode(c, node.head.typeAliasDecl.typeSpecHead);
            const rSym = c.compiler.sema.getSymbol(symId);
            try setLocalSym(c, nameId, .{
                .symId = symId,
                .funcSymId = cy.NullId,
                .parentSymId = rSym.key.resolvedSymKey.parentSymId,
            });
        },
        .enumDecl => {
            return;
        },
        .objectDecl => {
            try objectDecl(c, nodeId);
        },
        .funcDeclInit => {
            try funcDeclInit(c, nodeId);
        },
        .funcDecl => {
            try funcDecl(c, nodeId);
        },
        .hostFuncDecl => {
            return;
        },
        .whileCondStmt => {
            try pushIterSubBlock(c);

            _ = try semaExpr(c, node.head.whileCondStmt.cond);
            try semaStmts(c, node.head.whileCondStmt.bodyHead);

            try endIterSubBlock(c);
        },
        .whileOptStmt => {
            try pushIterSubBlock(c);

            const optt = try semaExpr(c, node.head.whileOptStmt.opt);
            if (node.head.whileOptStmt.some != cy.NullId) {
                _ = try getOrDeclareLocal(c, node.head.whileOptStmt.some, bt.Any);
                _ = try assignVar(c, node.head.whileOptStmt.some, optt);
            }

            try semaStmts(c, node.head.whileOptStmt.bodyHead);

            try endIterSubBlock(c);
        },
        .whileInfStmt => {
            try pushIterSubBlock(c);
            try semaStmts(c, node.head.child_head);
            try endIterSubBlock(c);
        },
        .for_iter_stmt => {
            try pushIterSubBlock(c);

            _ = try semaExpr(c, node.head.for_iter_stmt.iterable);

            if (node.head.for_iter_stmt.eachClause != cy.NullId) {
                const eachClause = c.nodes[node.head.for_iter_stmt.eachClause];
                if (eachClause.head.eachClause.key != cy.NullId) {
                    _ = try getOrDeclareLocal(c, eachClause.head.eachClause.key, bt.Any);
                }
                _ = try getOrDeclareLocal(c, eachClause.head.eachClause.value, bt.Any);
            }

            try semaStmts(c, node.head.for_iter_stmt.body_head);
            try endIterSubBlock(c);
        },
        .for_range_stmt => {
            try pushIterSubBlock(c);

            if (node.head.for_range_stmt.eachClause != cy.NullId) {
                const eachClause = c.nodes[node.head.for_range_stmt.eachClause];
                _ = try getOrDeclareLocal(c, eachClause.head.eachClause.value, bt.Integer);
            }

            const range_clause = c.nodes[node.head.for_range_stmt.range_clause];
            _ = try semaExpr(c, range_clause.head.left_right.left);
            _ = try semaExpr(c, range_clause.head.left_right.right);

            try semaStmts(c, node.head.for_range_stmt.body_head);
            try endIterSubBlock(c);
        },
        .matchBlock => {
            _ = try matchBlock(c, nodeId, false);
        },
        .if_stmt => {
            _ = try semaExpr(c, node.head.left_right.left);

            try pushSubBlock(c);
            try semaStmts(c, node.head.left_right.right);
            try endSubBlock(c);

            var elseClauseId = node.head.left_right.extra;
            while (elseClauseId != cy.NullId) {
                const elseClause = c.nodes[elseClauseId];
                if (elseClause.head.else_clause.cond == cy.NullId) {
                    try pushSubBlock(c);
                    try semaStmts(c, elseClause.head.else_clause.body_head);
                    try endSubBlock(c);
                    break;
                } else {
                    _ = try semaExpr(c, elseClause.head.else_clause.cond);

                    try pushSubBlock(c);
                    try semaStmts(c, elseClause.head.else_clause.body_head);
                    try endSubBlock(c);
                    elseClauseId = elseClause.head.else_clause.else_clause;
                }
            }
        },
        .tryStmt => {
            try pushSubBlock(c);
            try semaStmts(c, node.head.tryStmt.tryFirstStmt);
            try endSubBlock(c);

            try pushSubBlock(c);
            if (node.head.tryStmt.errorVar != cy.NullId) {
                _ = try getOrDeclareLocal(c, node.head.tryStmt.errorVar, bt.Error);
            }
            try semaStmts(c, node.head.tryStmt.catchFirstStmt);
            try endSubBlock(c);
        },
        .importStmt => {
            return;
        },
        .return_stmt => {
            return;
        },
        .return_expr_stmt => {
            const block = curBlock(c);
            const retType = try block.getReturnType(c);
            _ = try semaExprType(c, node.head.child_head, retType);
        },
        .comptimeStmt => {
            return;
        },
        else => return c.reportErrorAt("Unsupported node: {}", &.{v(node.node_t)}, nodeId),
    }
}

pub fn declareTypeAlias(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const nameN = c.nodes[node.head.typeAliasDecl.name];
    const name = c.getNodeTokenString(nameN);
    const nameId = try ensureNameSym(c.compiler, name);

    // Check for local sym.
    const key = LocalSymKey.initLocalSymKey(nameId, null);
    if (c.localSyms.contains(key)) {
        return c.reportErrorAt("The symbol `{}` was already declared.", &.{v(getName(c.compiler, nameId))}, node.head.typeAliasDecl.name);
    }

    const mod = c.getModule();
    try mod.setTypeAlias(c.compiler, name, nodeId);

    try setLocalSym(c, nameId, .{
        .symId = cy.NullId,
        .funcSymId = cy.NullId,
        .parentSymId = c.semaRootSymId,
    });
}

pub fn getOrInitModule(self: *cy.Chunk, spec: []const u8, nodeId: cy.NodeId) !cy.ModuleId {
    var resUri: cy.Str = undefined;
    self.compiler.hasApiError = false;
    if (!self.compiler.moduleResolver(@ptrCast(self.compiler.vm), self.id, cy.Str.initSlice(self.srcUri), cy.Str.initSlice(spec), &resUri)) {
        if (self.compiler.hasApiError) {
            return self.reportError(self.compiler.apiError, &.{});
        } else {
            return self.reportError("Failed to resolve module.", &.{});
        }
    }

    if (self.compiler.sema.moduleMap.get(resUri.slice())) |modId| {
        return modId;
    } else {
        // resUri is duped.
        const modId = try appendResolvedRootModule(self.compiler, resUri.slice());
        const dupedAbsSpec = self.compiler.sema.getModule(modId).absSpec;

        // Queue import task.
        try self.compiler.importTasks.append(self.alloc, .{
            .chunkId = self.id,
            .nodeId = nodeId,
            .absSpec = dupedAbsSpec,
            .modId = modId,
        });
        return modId;
    }
}

pub fn declareImport(chunk: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = chunk.nodes[nodeId];
    const ident = chunk.nodes[node.head.left_right.left];
    const name = chunk.getNodeTokenString(ident);
    const nameId = try ensureNameSym(chunk.compiler, name);

    const spec = chunk.nodes[node.head.left_right.right];
    const specPath = chunk.getNodeTokenString(spec);

    const modId = try getOrInitModule(chunk, specPath, nodeId);

    const mod = chunk.compiler.sema.getModule(modId);
    try setLocalSym(chunk, nameId, .{
        .symId = mod.resolvedRootSymId,
        .funcSymId = cy.NullId,
        .parentSymId = cy.NullId,
    });
}

fn setLocalSym(chunk: *cy.Chunk, nameId: NameSymId, sym: LocalSym) !void {
    const key = LocalSymKey.initLocalSymKey(nameId, null);
    try chunk.localSyms.put(chunk.alloc, key, sym);
}

fn matchBlock(c: *cy.Chunk, nodeId: cy.NodeId, canBreak: bool) !TypeId {
    const node = c.nodes[nodeId];
    _ = try semaExpr(c, node.head.matchBlock.expr);

    var curCase = node.head.matchBlock.firstCase;
    while (curCase != cy.NullId) {
        const case = c.nodes[curCase];
        var curCond = case.head.caseBlock.firstCond;
        while (curCond != cy.NullId) {
            const cond = c.nodes[curCond];
            if (cond.node_t != .elseCase) {
                _ = try semaExpr(c, curCond);
            }
            curCond = cond.next;
        }
        curCase = case.next;
    }

    curCase = node.head.matchBlock.firstCase;
    while (curCase != cy.NullId) {
        const case = c.nodes[curCase];
        try pushSubBlock(c);
        try semaStmts(c, case.head.caseBlock.firstChild);
        try endSubBlock(c);
        curCase = case.next;
    }

    if (canBreak) {
        return bt.Any;
    } else {
        return bt.Undefined;
    }
}

pub fn declareEnum(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const nameN = c.nodes[node.head.enumDecl.name];
    const name = c.getNodeTokenString(nameN);
    // const nameId = try ensureNameSym(c.compiler, name);

    const spec = try std.fmt.allocPrint(c.alloc, "{s}.{s}", .{c.getModule().absSpec, name});
    defer c.alloc.free(spec);
    const modId = try cy.module.appendModule(c.compiler, spec);
    const mod = c.compiler.sema.getModulePtr(modId);

    const eid = try c.compiler.vm.ensureEnum(name);

    var i: u32 = 0;
    var memberId = node.head.enumDecl.memberHead;
    var buf: std.ArrayListUnmanaged(NameSymId) = .{};
    defer buf.deinit(c.alloc);

    while (memberId != cy.NullId) : (i += 1) {
        const member = c.nodes[memberId];
        const mName = c.getNodeTokenString(member);
        const mNameId = try ensureNameSym(c.compiler, mName);
        try buf.append(c.alloc, mNameId);
        // const mSymId = try c.compiler.vm.ensureSymbol(mName);
        mod.declareEnumMember(c.compiler, mName, eid, i) catch |err| {
            if (err == error.DuplicateSymName) {
                return c.reportErrorAt("The enum symbol `{}` was already declared.", &.{v(mName)}, memberId);
            } else {
                return err;
            }
        };
        memberId = member.next;
    }
    c.compiler.vm.enums.buf[eid].members = try buf.toOwnedSlice(c.alloc);

    const chunkMod = c.compiler.sema.getModulePtr(c.modId);
    chunkMod.declareEnumType(c.compiler, name, eid, modId) catch |err| {
        if (err == error.DuplicateSymName) {
            return c.reportErrorAt("The symbol `{}` was already declared.", &.{v(name)}, nodeId);
        } else {
            return err;
        }
    };
}

pub fn declareObject(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const nameN = c.nodes[node.head.objectDecl.name];
    const name = c.getNodeTokenString(nameN);
    const nameId = try ensureNameSym(c.compiler, name);

    try checkForDuplicateUsingSym(c, c.getModule().resolvedRootSymId, nameId, nodeId);
    const objModId = cy.module.declareTypeObject(c.compiler, c.modId, name, c.id, nodeId) catch |err| {
        if (err == error.DuplicateSymName) {
            return c.reportErrorAt("Object type `{}` already exists", &.{v(name)}, nodeId);
        } else return err;
    };
    const key = ResolvedSymKey.initResolvedSymKey(c.getModule().resolvedRootSymId, nameId);
    const symId = try resolveObjectSym(c.compiler, key, objModId);

    // Persist for declareObjectMembers.
    c.nodes[node.head.objectDecl.name].head.ident.sema_csymId = CompactSymbolId.initSymId(symId);
}

pub fn declareObjectMembers(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const objSymId = c.nodes[node.head.objectDecl.name].head.ident.sema_csymId.id;
    const objSym = c.compiler.sema.getSymbol(objSymId);
    const objModId = objSym.inner.object.modId;
    const rtTypeId = objSym.inner.object.typeId;

    // Load fields.
    var i: u32 = 0;

    var fieldId = node.head.objectDecl.fieldsHead;
    while (fieldId != cy.NullId) : (i += 1) {
        const field = c.nodes[fieldId];
        const fieldName = c.getNodeTokenString(field);
        const fieldNameId = try ensureNameSym(c.compiler, fieldName);
        const fieldSymId = try c.compiler.vm.ensureFieldSym(fieldName);

        var fieldType: SymbolId = undefined;
        if (field.head.objectField.typeSpecHead != cy.NullId) {
            fieldType = try getOrResolveTypeSymFromSpecNode(c, field.head.objectField.typeSpecHead);
        } else {
            fieldType = bt.Any;
        }

        try c.compiler.vm.addFieldSym(rtTypeId, fieldSymId, @intCast(i), fieldType);

        try c.compiler.sema.objectMembers.put(c.alloc, .{
            .objectMemberKey = .{
                .objSymId = objSymId,
                .memberNameId = fieldNameId,
            },
        }, {});
        fieldId = field.next;
    }
    c.compiler.vm.types.buf[rtTypeId].numFields = i;

    var funcId = node.head.objectDecl.funcsHead;
    while (funcId != cy.NullId) {
        const declId = try appendFuncDecl(c, funcId, true);
        c.nodes[funcId].head.func.semaDeclId = declId;

        const func = &c.semaFuncDecls.items[declId];
        const funcN = c.nodes[funcId];

        if (func.numParams > 0) {
            const param = c.nodes[func.paramHead];
            const paramName = c.getNodeTokenString(c.nodes[param.head.funcParam.name]);
            if (std.mem.eql(u8, paramName, "self")) {
                // Skip methods for now.

                funcId = funcN.next;
                continue;
            }
        }

        // Object function.
        const funcName = func.getName(c);
        const funcNameId = try ensureNameSym(c.compiler, funcName);

        cy.module.declareUserFunc(c.compiler, objModId, funcName, func.funcSigId, declId, false) catch |err| {
            if (err == error.DuplicateSymName) {
                return c.reportErrorAt("The symbol `{}` already exists.", &.{v(funcName)}, declId);
            } else if (err == error.DuplicateFuncSig) {
                return c.reportErrorAt("The function `{}` with the same signature already exists.", &.{v(funcName)}, declId);
            } else return err;
        };
        const key = ResolvedSymKey.initResolvedSymKey(objSymId, funcNameId);
        _ = try resolveUserFunc(c, key, func.funcSigId, declId, false);

        funcId = funcN.next;
    }
}

fn objectDecl(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    // const nameN = c.nodes[node.head.objectDecl.name];
    // const name = c.getNodeTokenString(nameN);
    // const nameId = try ensureNameSym(c.compiler, name);
    // const symId = nameN.head.ident.sema_csymId.id;

    if (c.curObjectSymId != cy.NullId) {
        return c.reportErrorAt("Nested types are not supported.", &.{}, nodeId);
    }
    c.curObjectSymId = c.nodes[node.head.objectDecl.name].head.ident.sema_csymId.id;
    defer c.curObjectSymId = cy.NullId;

    var funcId = node.head.objectDecl.funcsHead;
    while (funcId != cy.NullId) {
        const declId = c.nodes[funcId].head.func.semaDeclId;

        const func = &c.semaFuncDecls.items[declId];
        const funcN = c.nodes[funcId];

        if (func.numParams > 0) {
            const param = c.nodes[func.paramHead];
            const paramName = c.getNodeTokenString(c.nodes[param.head.funcParam.name]);
            if (std.mem.eql(u8, paramName, "self")) {
                // Struct method.
                const blockId = try pushBlock(c, funcN.head.func.semaDeclId);
                c.semaBlocks.items[blockId].isMethodBlock = true;

                func.semaBlockId = blockId;
                errdefer endBlock(c) catch cy.fatal();
                try pushMethodParamVars(c, func);
                try semaStmts(c, funcN.head.func.bodyHead);
                try endBlock(c);

                funcId = funcN.next;
                continue;
            }
        }

        // Object function.
        const blockId = try pushBlock(c, funcN.head.func.semaDeclId);
        func.semaBlockId = blockId;
        errdefer endBlock(c) catch cy.fatal();
        try appendFuncParamVars(c, func);
        try semaStmts(c, funcN.head.func.bodyHead);
        try endFuncSymBlock(c, func.numParams);

        funcId = funcN.next;
    }
}

fn checkForDuplicateUsingSym(c: *cy.Chunk, parentSymId: SymbolId, nameId: NameSymId, nodeId: cy.NodeId) !void {
    if (parentSymId == c.semaRootSymId) {
        // Root symbol, check that it's not a local alias.
        const key = LocalSymKey.initLocalSymKey(nameId, null);
        if (c.localSyms.contains(key)) {
            return c.reportErrorAt("The symbol `{}` was already declared.",
                &.{v(getName(c.compiler, nameId))}, nodeId);
        }
    }
}

pub fn declareFuncInit(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    if (node.head.func.bodyHead == cy.NullId) {
        // No initializer. Check if @host func.
        const modifierId = c.nodes[node.head.func.header].next;
        if (modifierId != cy.NullId) {
            const modifier = c.nodes[modifierId];
            if (modifier.head.annotation.type == .host) {
                try declareHostFunc(c, nodeId);
                return;
            }
        }
        const name = c.getNodeTokenString(c.nodes[c.nodes[node.head.func.header].head.funcHeader.name]);
        return c.reportErrorAt("`{}` does not have an initializer.", &.{v(name)}, nodeId);
    } else {
        const declId = try appendFuncDecl(c, nodeId, true);
        c.nodes[nodeId].head.func.semaDeclId = declId;

        // Determine if it's a host func.
        const func = &c.semaFuncDecls.items[declId];
        const name = func.getName(c);
        const nameId = try ensureNameSym(c.compiler, name);

        const mod = c.getModule();
        try checkForDuplicateUsingSym(c, mod.resolvedRootSymId, nameId, func.getNameNode(c));
        cy.module.declareUserFunc(c.compiler, c.modId, name, func.funcSigId, declId, true) catch |err| {
            if (err == error.DuplicateSymName) {
                return c.reportErrorAt("The symbol `{}` already exists.", &.{v(name)}, nodeId);
            } else if (err == error.DuplicateFuncSig) {
                return c.reportErrorAt("The function `{}` with the same signature already exists.", &.{v(name)}, nodeId);
            } else return err;
        };
        const key = ResolvedSymKey.initResolvedSymKey(mod.resolvedRootSymId, nameId);
        _ = try resolveUserFunc(c, key, func.funcSigId, declId, true);
    }
}

fn funcDeclInit(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const declId = c.nodes[nodeId].head.func.semaDeclId;

    const node = c.nodes[nodeId];
    const func = &c.semaFuncDecls.items[declId];

    const name = func.getName(c);

    c.curSemaInitingSym = CompactSymbolId.initFuncSymId(func.inner.staticFunc.semaFuncSymId);
    c.semaVarDeclDeps.clearRetainingCapacity();
    defer c.curSemaInitingSym = CompactSymbolId.initNull();

    _ = semaExpr(c, node.head.func.bodyHead) catch |err| {
        if (err == error.CanNotUseLocal) {
            const local = c.nodes[c.compiler.errorPayload];
            const localName = c.getNodeTokenString(local);
            return c.reportErrorAt("The declaration initializer of static function `{}` can not reference the local variable `{}`.", &.{v(name), v(localName)}, nodeId);
        } else {
            return err;
        }
    };
}

pub fn declareHostFunc(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    c.nodes[nodeId].node_t = .hostFuncDecl;
    const declId = try appendFuncDecl(c, nodeId, true);
    const func = &c.semaFuncDecls.items[declId];
    const name = func.getName(c);
    const nameId = try ensureNameSym(c.compiler, name);
    c.nodes[nodeId].head.func.semaDeclId = declId;

    const mod = c.getModule();
    try checkForDuplicateUsingSym(c, mod.resolvedRootSymId, nameId, func.getNameNode(c));

    const info = cy.HostFuncInfo{
        .modId = c.modId,
        .name = cy.Str.initSlice(name),
        .funcSigId = func.funcSigId,
        .idx = c.curHostFuncIdx,
    };
    c.curHostFuncIdx += 1;
    if (c.funcLoader.?(@ptrCast(c.compiler.vm), info)) |funcPtr| {
        cy.module.declareHostFunc(c.compiler, c.modId, name, func.funcSigId, declId, funcPtr) catch |err| {
            if (err == error.DuplicateSymName) {
                return c.reportErrorAt("The symbol `{}` already exists.", &.{v(name)}, nodeId);
            } else if (err == error.DuplicateFuncSig) {
                return c.reportErrorAt("The function `{}` with the same signature already exists.", &.{v(name)}, nodeId);
            } else return err;
        };
        const key = ResolvedSymKey.initResolvedSymKey(mod.resolvedRootSymId, nameId);
        _ = try resolveHostFunc(c, key, func.funcSigId, funcPtr);
    } else {
        return c.reportErrorAt("Host func `{}` failed to load.", &.{v(name)}, nodeId);
    }
}

pub fn declareFunc(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const declId = try appendFuncDecl(c, nodeId, true);
    const func = &c.semaFuncDecls.items[declId];
    const name = func.getName(c);
    const nameId = try ensureNameSym(c.compiler, name);
    c.setNodeFuncDecl(nodeId, declId);

    const mod = c.getModule();
    try checkForDuplicateUsingSym(c, mod.resolvedRootSymId, nameId, func.getNameNode(c));
    cy.module.declareUserFunc(c.compiler, c.modId, name, func.funcSigId, declId, false) catch |err| {
        if (err == error.DuplicateSymName) {
            return c.reportErrorAt("The symbol `{}` already exists.", &.{v(name)}, nodeId);
        } else if (err == error.DuplicateFuncSig) {
            return c.reportErrorAt("The function `{}` with the same signature already exists.", &.{v(name)}, nodeId);
        } else return err;
    };
    const key = ResolvedSymKey.initResolvedSymKey(mod.resolvedRootSymId, nameId);
    _ = try resolveUserFunc(c, key, func.funcSigId, declId, false);
}

fn funcDecl(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const declId = c.nodes[nodeId].head.func.semaDeclId;

    const node = c.nodes[nodeId];
    const func = &c.semaFuncDecls.items[declId];

    const blockId = try pushBlock(c, declId);
    try appendFuncParamVars(c, func);
    try semaStmts(c, node.head.func.bodyHead);

    try endFuncSymBlock(c, func.numParams);

    func.semaBlockId = blockId;
}

pub fn declareVar(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const varSpec = c.nodes[node.head.staticDecl.varSpec];
    const nameN = c.nodes[varSpec.head.varSpec.name];
    const name = c.getNodeTokenString(nameN);
    const nameId = try ensureNameSym(c.compiler, name);
    try c.compiler.sema.modules.items[c.modId].setUserVar(c.compiler, name, nodeId);

    // var type.
    var typeSymId: SymbolId = undefined;
    if (varSpec.head.varSpec.typeSpecHead != cy.NullId) {
        typeSymId = try getOrResolveTypeSymFromSpecNode(c, varSpec.head.varSpec.typeSpecHead);
    } else {
        typeSymId = bt.Any;
    }

    const symId = try resolveLocalVarSym(c, c.semaRootSymId, nameId, typeSymId, nodeId, true);
    c.nodes[nodeId].head.staticDecl.sema_symId = symId;
}

fn localDecl(self: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = self.nodes[nodeId];
    const rtype = try semaExpr(self, node.head.localDecl.right);
    const varSpec = self.nodes[node.head.localDecl.varSpec];
    const nameNid = varSpec.head.varSpec.name;
    const nameN = self.nodes[nameNid];
    const name = self.getNodeTokenString(nameN);

    const sblock = curBlock(self);
    if (sblock.nameToVar.get(name)) |varId| {
        const svar = &self.vars.items[varId];
        if (svar.isParentLocalAlias()) {
            return self.reportErrorAt("`{}` already references a parent local variable.", &.{v(name)}, nodeId);
        } else if (svar.type == .staticAlias) {
            return self.reportErrorAt("`{}` already references a static variable.", &.{v(name)}, nodeId);
        } else {
            if (!svar.isDeclared) {
                svar.isDeclared = true;
                try self.assignedVarStack.append(self.alloc, varId);
                try self.declaredVarStack.append(self.alloc, varId);
                self.nodes[nameNid].head.ident.semaVarId = varId;
            } else {
                if (self.semaIsMainBlock()) {
                    return self.reportErrorAt("Variable `{}` is already declared in the main block.", &.{v(name)}, nodeId);
                } else {
                    return self.reportErrorAt("Variable `{}` is already declared in the current function block.", &.{v(name)}, nodeId);
                }
            }
        }
    } else {
        const id = try pushLocalBodyVar(self, name, rtype);
        self.vars.items[id].isDeclared = true;
        try self.assignedVarStack.append(self.alloc, id);
        try self.declaredVarStack.append(self.alloc, id);
        self.nodes[nameNid].head.ident.semaVarId = id;
    }
}

fn staticDecl(c: *cy.Chunk, nodeId: cy.NodeId, exported: bool) !void {
    _ = exported;
    const node = c.nodes[nodeId];
    const varSpec = c.nodes[node.head.staticDecl.varSpec];
    const nameN = c.nodes[varSpec.head.varSpec.name];
    if (nameN.node_t == .ident) {
        const name = c.getNodeTokenString(nameN);

        const symId = node.head.staticDecl.sema_symId;
        const csymId = CompactSymbolId.initSymId(symId);

        c.curSemaInitingSym = csymId;
        c.semaVarDeclDeps.clearRetainingCapacity();
        defer c.curSemaInitingSym = CompactSymbolId.initNull();

        const right = c.nodes[node.head.staticDecl.right];
        if (right.node_t == .matchBlock) {
            _ = try matchBlock(c, node.head.staticDecl.right, true);
        } else {
            _ = semaExpr(c, node.head.staticDecl.right) catch |err| {
                if (err == error.CanNotUseLocal) {
                    const local = c.nodes[c.compiler.errorPayload];
                    const localName = c.getNodeTokenString(local);
                    return c.reportErrorAt("The declaration of static variable `{}` can not reference the local variable `{}`.", &.{v(name), v(localName)}, nodeId);
                } else {
                    return err;
                } 
            };
        }
    } else {
        return c.reportErrorAt("Static variable declarations can only have an identifier as the name. Parsed {} instead.", &.{fmt.v(nameN.node_t)}, nodeId);
    }
}

fn semaExpr(c: *cy.Chunk, nodeId: cy.NodeId) anyerror!TypeId {
    const res = try semaExprInner(c, nodeId, bt.Any);
    c.nodeTypes[nodeId] = res;
    return res;
}

/// No preferred type when `preferType == bt.Any`.
fn semaExprType(c: *cy.Chunk, nodeId: cy.NodeId, preferType: TypeId) anyerror!TypeId {
    const res = try semaExprInner(c, nodeId, preferType);
    c.nodeTypes[nodeId] = res;
    return res;
}

fn semaExprInner(c: *cy.Chunk, nodeId: cy.NodeId, preferType: TypeId) anyerror!TypeId {
    c.curNodeId = nodeId;
    const node = c.nodes[nodeId];
    // log.debug("sema expr {}", .{node.node_t});
    switch (node.node_t) {
        .true_literal => {
            return bt.Boolean;
        },
        .false_literal => {
            return bt.Boolean;
        },
        .none => {
            return bt.None;
        },
        .arr_literal => {
            var expr_id = node.head.child_head;
            var i: u32 = 0;
            while (expr_id != cy.NullId) : (i += 1) {
                var expr = c.nodes[expr_id];
                _ = try semaExpr(c, expr_id);
                expr_id = expr.next;
            }
            return bt.List;
        },
        .symbolLit => {
            return bt.Symbol;
        },
        .errorSymLit => {
            return bt.Error;
        },
        .objectInit => {
            _ = try semaExpr(c, node.head.objectInit.name);
            const nameN = c.nodes[node.head.objectInit.name];

            var csymId = CompactSymbolId.initNull();
            if (nameN.node_t == .ident) {
                csymId = nameN.head.ident.sema_csymId;
            } else if (nameN.node_t == .accessExpr) {
                csymId = nameN.head.accessExpr.sema_csymId;
            }

            if (csymId.isPresent()) {
                if (!csymId.isFuncSymId) {
                    c.nodes[nodeId].head.objectInit.sema_symId = csymId.id;
                    const initializer = c.nodes[node.head.objectInit.initializer];
                    var i: u32 = 0;
                    var entry_id = initializer.head.child_head;
                    while (entry_id != cy.NullId) : (i += 1) {
                        var entry = c.nodes[entry_id];
                        _ = try semaExpr(c, entry.head.mapEntry.right);
                        entry_id = entry.next;
                    }
                    return csymId.id;
                }
            }

            const name = c.getNodeTokenString(nameN);
            return c.reportError("Object type `{}` does not exist.", &.{v(name)});
        },
        .map_literal => {
            var i: u32 = 0;
            var entry_id = node.head.child_head;
            while (entry_id != cy.NullId) : (i += 1) {
                var entry = c.nodes[entry_id];

                _ = try semaExpr(c, entry.head.mapEntry.right);
                entry_id = entry.next;
            }
            return bt.Map;
        },
        .nonDecInt => {
            const literal = c.getNodeTokenString(node);
            var val: u64 = undefined;
            if (literal[1] == 'x') {
                val = try std.fmt.parseInt(u64, literal[2..], 16);
            } else if (literal[1] == 'o') {
                val = try std.fmt.parseInt(u64, literal[2..], 8);
            } else if (literal[1] == 'b') {
                val = try std.fmt.parseInt(u64, literal[2..], 2);
            } else if (literal[1] == 'u') {
                if (literal[3] == '\\') {
                    if (unescapeAsciiChar(literal[4])) |ch| {
                        val = ch;
                    } else {
                        val = literal[4];
                        if (val > 128) {
                            return c.reportError("Invalid UTF-8 Rune.", &.{});
                        }
                    }
                    if (literal.len != 6) {
                        return c.reportError("Invalid UTF-8 Rune.", &.{});
                    }
                } else {
                    const len = std.unicode.utf8ByteSequenceLength(literal[3]) catch {
                        return c.reportError("Invalid UTF-8 Rune.", &.{});
                    };
                    if (literal.len != @as(usize, 4) + len) {
                        return c.reportError("Invalid UTF-8 Rune.", &.{});
                    }
                    val = std.unicode.utf8Decode(literal[3..3+len]) catch {
                        return c.reportError("Invalid UTF-8 Rune.", &.{});
                    };
                }
            } else {
                const char: []const u8 = &[_]u8{literal[1]};
                return c.reportError("Unsupported integer notation: {}", &.{v(char)});
            }
            c.nodes[nodeId].head = .{
                .nonDecInt = .{
                    .semaNumberVal = val,
                },
            };
            const canBeInt = std.math.cast(i48, val) != null;
            if (canBeInt) {
                return bt.Integer;
            } else {
                return c.reportError("Number literal can not be an integer.", &.{});
            }
        },
        .number => {
            if (preferType == bt.Float) {
                return bt.Float;
            }
            const literal = c.getNodeTokenString(node);
            _ = try std.fmt.parseInt(i48, literal, 10);
            return bt.Integer;
        },
        .float => {
            return bt.Float;
        },
        .string => {
            return bt.StaticString;
        },
        .stringTemplate => {
            var expStringPart = true;
            var curId = node.head.stringTemplate.partsHead;
            while (curId != cy.NullId) {
                const cur = c.nodes[curId];
                if (!expStringPart) {
                    _ = try semaExpr(c, curId);
                }
                curId = cur.next;
                expStringPart = !expStringPart;
            }
            return bt.String;
        },
        .ident => {
            return identifier(c, nodeId);
        },
        .if_expr => {
            _ = try semaExpr(c, node.head.if_expr.cond);

            _ = try semaExpr(c, node.head.if_expr.body_expr);

            if (node.head.if_expr.else_clause != cy.NullId) {
                const else_clause = c.nodes[node.head.if_expr.else_clause];
                _ = try semaExpr(c, else_clause.head.child_head);
            }
            return bt.Any;
        },
        .sliceExpr => {
            const arrT = try semaExpr(c, node.head.sliceExpr.arr);

            if (node.head.sliceExpr.left == cy.NullId) {
                // nop
            } else {
                _ = try semaExpr(c, node.head.sliceExpr.left);
            }
            if (node.head.sliceExpr.right == cy.NullId) {
                // nop
            } else {
                _ = try semaExpr(c, node.head.sliceExpr.right);
            }

            if (arrT == bt.List) {
                return bt.List;
            } else {
                return bt.Dynamic;
            }
        },
        .accessExpr => {
            const res = try accessExpr(c, nodeId);
            return res.exprT;
        },
        .indexExpr => {
            const leftT = try semaExpr(c, node.head.indexExpr.left);
            if (leftT == bt.List) {
                // Specialized.
                _ = try semaExprType(c, node.head.indexExpr.right, bt.Integer);
                c.nodes[nodeId].head.indexExpr.semaGenStrat = .specialized;
                return bt.Dynamic;
            } else if (leftT == bt.Map) {
                // Specialized.
                _ = try semaExprType(c, node.head.indexExpr.right, bt.Any);
                c.nodes[nodeId].head.indexExpr.semaGenStrat = .specialized;
                return bt.Dynamic;
            } else {
                _ = try semaExpr(c, node.head.indexExpr.right);
                c.nodes[nodeId].head.indexExpr.semaGenStrat = .generic;

                // TODO: Check func syms.
                return bt.Dynamic;
            }
        },
        .tryExpr => {
            _ = try semaExpr(c, node.head.tryExpr.expr);
            if (node.head.tryExpr.elseExpr != cy.NullId) {
                _ = try semaExpr(c, node.head.tryExpr.elseExpr);
            }
            return bt.Any;
        },
        .throwExpr => {
            _ = try semaExpr(c, node.head.child_head);
            return bt.Any;
        },
        .unary_expr => {
            const op = node.head.unary.op;
            switch (op) {
                .minus => {
                    const childPreferT = if (preferType == bt.Integer or preferType == bt.Float) preferType else bt.Any;
                    const childT = try semaExprType(c, node.head.unary.child, childPreferT);
                    if (childT == bt.Integer or childT == bt.Float) {
                        c.nodes[nodeId].head.unary.semaGenStrat = .specialized;
                        return childT;
                    } else {
                        c.nodes[nodeId].head.unary.semaGenStrat = .generic;
                        return bt.Dynamic; 
                    }
                },
                .not => {
                    _ = try semaExpr(c, node.head.unary.child);
                    return bt.Boolean;
                },
                .bitwiseNot => {
                    const childPreferT = if (preferType == bt.Integer) preferType else bt.Any;
                    const childT = try semaExprType(c, node.head.unary.child, childPreferT);
                    if (childT == bt.Integer) {
                        c.nodes[nodeId].head.unary.semaGenStrat = .specialized;
                        return bt.Float;
                    } else {
                        c.nodes[nodeId].head.unary.semaGenStrat = .generic;
                        return bt.Dynamic; 
                    }
                },
                // else => return self.reportErrorAt("Unsupported unary op: {}", .{op}, node),
            }
        },
        .group => {
            return semaExpr(c, node.head.child_head);
        },
        .castExpr => {
            _ = try semaExpr(c, node.head.castExpr.expr);
            const rTypeSymId = try getOrResolveTypeSymFromSpecNode(c, node.head.castExpr.typeSpecHead);
            c.nodes[nodeId].head.castExpr.semaTypeSymId = rTypeSymId;
            return rTypeSymId;
        },
        .binExpr => {
            const left = node.head.binExpr.left;
            const right = node.head.binExpr.right;

            const op = node.head.binExpr.op;
            switch (op) {
                .star,
                .slash,
                .percent,
                .caret,
                .plus,
                .minus => {
                    const leftPreferT = if (preferType == bt.Float or preferType == bt.Integer) preferType else bt.Any;
                    const leftT = try semaExprType(c, left, leftPreferT);

                    if (leftT == bt.Integer or leftT == bt.Float) {
                        // Specialized.
                        _ = try semaExprType(c, right, leftT);
                        c.nodes[nodeId].head.binExpr.semaGenStrat = .specialized;
                        return leftT;
                    } else {
                        // Generic callObjSym.
                        _ = try semaExprType(c, right, bt.Any);
                        c.nodes[nodeId].head.binExpr.semaGenStrat = .generic;

                        // TODO: Check func syms.
                        return bt.Dynamic;
                    }
                },
                .bitwiseAnd,
                .bitwiseOr,
                .bitwiseXor,
                .bitwiseLeftShift,
                .bitwiseRightShift => {
                    const leftPreferT = if (preferType == bt.Integer) preferType else bt.Any;
                    const leftT = try semaExprType(c, left, leftPreferT);

                    if (leftT == bt.Integer) {
                        _ = try semaExprType(c, right, leftT);
                        c.nodes[nodeId].head.binExpr.semaGenStrat = .specialized;
                        return leftT;
                    } else {
                        _ = try semaExprType(c, right, bt.Any);
                        c.nodes[nodeId].head.binExpr.semaGenStrat = .generic;

                        // TODO: Check func syms.
                        return bt.Dynamic;
                    }
                },
                .and_op => {
                    const ltype = try semaExpr(c, left);
                    const rtype = try semaExpr(c, right);
                    if (ltype == rtype) {
                        return ltype;
                    } else return bt.Any;
                },
                .or_op => {
                    const ltype = try semaExpr(c, left);
                    const rtype = try semaExpr(c, right);
                    if (ltype == rtype) {
                        return ltype;
                    } else return bt.Any;
                },
                .bang_equal,
                .equal_equal => {
                    const leftPreferT = if (preferType == bt.Float or preferType == bt.Integer) preferType else bt.Any;
                    const leftT = try semaExprType(c, left, leftPreferT);
                    if (leftT == bt.Integer or leftT == bt.Float) {
                        _ = try semaExprType(c, right, leftT);
                    } else {
                        _ = try semaExpr(c, right);
                    }
                    return bt.Boolean;
                },
                .less_equal,
                .greater,
                .greater_equal,
                .less => {
                    const leftPreferT = if (preferType == bt.Float or preferType == bt.Integer) preferType else bt.Any;
                    const leftT = try semaExprType(c, left, leftPreferT);

                    if (leftT == bt.Integer or leftT == bt.Float) {
                        // Specialized.
                        _ = try semaExprType(c, right, leftT);
                        c.nodes[nodeId].head.binExpr.semaGenStrat = .specialized;
                        return bt.Boolean;
                    } else {
                        // Generic callObjSym.
                        _ = try semaExprType(c, right, bt.Any);
                        c.nodes[nodeId].head.binExpr.semaGenStrat = .generic;

                        // TODO: Check func syms.
                        return bt.Dynamic;
                    }
                },
                else => return c.reportErrorAt("Unsupported binary op: {}", &.{fmt.v(op)}, nodeId),
            }
        },
        .coyield => {
            return bt.Any;
        },
        .coresume => {
            _ = try semaExpr(c, node.head.child_head);
            return bt.Any;
        },
        .coinit => {
            _ = try semaExpr(c, node.head.child_head);
            return bt.Fiber;
        },
        .callExpr => {
            return callExpr(c, nodeId);
        },
        .lambda_multi => {
            const declId = try appendFuncDecl(c, nodeId, false);
            c.nodes[nodeId].head.func.semaDeclId = declId;

            const blockId = try pushBlock(c, declId);

            // Generate function body.
            const func = &c.semaFuncDecls.items[declId];
            func.semaBlockId = blockId;
            try appendFuncParamVars(c, func);
            try semaStmts(c, node.head.func.bodyHead);

            try endFuncBlock(c);

            const funcSigId = try ensureResolvedUntypedFuncSig(c.compiler, func.numParams);
            func.inner.lambda.funcSigId = funcSigId;
            return bt.Any;
        },
        .lambda_expr => {
            const declId = try appendFuncDecl(c, nodeId, false);
            c.nodes[nodeId].head.func.semaDeclId = declId;

            const blockId = try pushBlock(c, declId);

            // Generate function body.
            const func = &c.semaFuncDecls.items[declId];
            func.semaBlockId = blockId;
            try appendFuncParamVars(c, func);
            _ = try semaExpr(c, node.head.func.bodyHead);

            try endFuncBlock(c);

            const funcSigId = try ensureResolvedUntypedFuncSig(c.compiler, func.numParams);
            func.inner.lambda.funcSigId = funcSigId;
            return bt.Any;
        },
        else => return c.reportErrorAt("Unsupported node", &.{}, nodeId),
    }
}

fn callExpr(c: *cy.Chunk, nodeId: cy.NodeId) !TypeId {
    const node = c.nodes[nodeId];
    const callee = c.nodes[node.head.callExpr.callee];
    if (!node.head.callExpr.has_named_arg) {
        if (callee.node_t == .accessExpr) {
            _ = try semaExpr(c, callee.head.accessExpr.left);

            const left = c.nodes[callee.head.accessExpr.left];
            var crLeftSym = CompactSymbolId.initNull();
            if (left.node_t == .ident) {
                crLeftSym = left.head.ident.sema_csymId;
            } else if (left.node_t == .accessExpr) {
                crLeftSym = left.head.accessExpr.sema_csymId;
            }
            if (crLeftSym.isPresent()) {
                // Calling a symbol.
                if (!crLeftSym.isFuncSymId) {
                    const right = c.nodes[callee.head.accessExpr.right];
                    const name = c.getNodeTokenString(right);
                    const nameId = try ensureNameSym(c.compiler, name);

                    const sym = c.compiler.sema.getSymbol(crLeftSym.id);
                    if (sym.symT == .module) {
                        if (getFirstFuncSigInModule(c, sym.inner.module.id, nameId)) |funcSigId| {
                            const funcSig = c.compiler.sema.getFuncSig(funcSigId);
                            const preferredParamTypes = funcSig.params();

                            const callArgStart = c.compiler.typeStack.items.len;
                            defer c.compiler.typeStack.items.len = callArgStart;

                            const callArgs = try pushCallArgsWithPreferredTypes(c, node.head.callExpr.arg_head, preferredParamTypes);
                            const reqRet = bt.Any;

                            if (try getOrResolveSymForFuncCall(c, crLeftSym.id, nameId, callArgs.argTypes, reqRet, callArgs.hasDynamicArg)) |callRes| {
                                try referenceSym(c, callRes.csymId, true);
                                c.nodes[node.head.callExpr.callee].head.accessExpr.sema_csymId = callRes.csymId;
                                return callRes.retType;
                            }
                        }
                    }
                    const callArgStart = c.compiler.typeStack.items.len;
                    defer c.compiler.typeStack.items.len = callArgStart;

                    // Push Any for self arg.
                    try c.compiler.typeStack.append(c.alloc, bt.Any);

                    const callArgs = try pushCallArgs(c, node.head.callExpr.arg_head);
                    const reqRet = bt.Any;

                    if (try getOrResolveSymForFuncCall(c, crLeftSym.id, nameId, callArgs.argTypes, reqRet, callArgs.hasDynamicArg)) |callRes| {
                        try referenceSym(c, callRes.csymId, true);
                        c.nodes[node.head.callExpr.callee].head.accessExpr.sema_csymId = callRes.csymId;
                        return callRes.retType;
                    }
                }
            }
            const callArgStart = c.compiler.typeStack.items.len;
            defer c.compiler.typeStack.items.len = callArgStart;

            // Push Any for self arg.
            try c.compiler.typeStack.append(c.alloc, bt.Any);

            const callArgs = try pushCallArgs(c, node.head.callExpr.arg_head);
            _ = callArgs;
            const reqRet = bt.Any;

            // Dynamic method call.
            const selfCallArgs = c.compiler.typeStack.items[callArgStart..];
            const funcSigId = try ensureFuncSig(c.compiler, selfCallArgs, reqRet);
            c.nodes[callee.head.accessExpr.right].head.ident.semaMethodSigId = funcSigId;

            return bt.Dynamic;
        } else if (callee.node_t == .ident) {
            const name = c.getNodeTokenString(callee);

            // Perform custom lookup for static vars.
            const res = try getOrLookupVar(c, name, false);
            switch (res) {
                .local => |id| {
                    c.nodes[node.head.callExpr.callee].head.ident.semaVarId = id;

                    var numArgs: u32 = 1;
                    var arg_id = node.head.callExpr.arg_head;
                    while (arg_id != cy.NullId) : (numArgs += 1) {
                        const arg = c.nodes[arg_id];
                        _ = try semaExpr(c, arg_id);
                        arg_id = arg.next;
                    }
                    return bt.Any;
                },
                .static, // There was a previous usage of a static var alias.
                .not_found => {
                    const nameId = try ensureNameSym(c.compiler, name);

                    if (getFirstFuncSigForSym(c, c.semaRootSymId, nameId)) |funcSigId| {
                        const funcSig = c.compiler.sema.getFuncSig(funcSigId);
                        const preferredParamTypes = funcSig.params();

                        const callArgStart = c.compiler.typeStack.items.len;
                        defer c.compiler.typeStack.items.len = callArgStart;

                        const callArgs = try pushCallArgsWithPreferredTypes(c, node.head.callExpr.arg_head, preferredParamTypes);
                        const reqRet = bt.Any;

                        c.curNodeId = node.head.callExpr.callee;

                        if (try getOrResolveSymForFuncCall(c, c.semaRootSymId, nameId, callArgs.argTypes, reqRet, callArgs.hasDynamicArg)) |callRes| {
                            try referenceSym(c, callRes.csymId, true);
                            c.nodes[node.head.callExpr.callee].head.ident.sema_csymId = callRes.csymId;
                            return callRes.retType;
                        }
                        return c.reportErrorAt("Undeclared func `{}`.", &.{v(name)}, node.head.callExpr.callee);
                    } else {
                        const callArgStart = c.compiler.typeStack.items.len;
                        defer c.compiler.typeStack.items.len = callArgStart;

                        // const funcCandStart = c.funcCandidateStack.items.len;
                        // defer c.funcCandidateStack.items.len = funcCandStart;
                        // const funcCandidates = try pushFuncCandidates(c, c.semaRootSymId, nameId, node.head.callExpr.numArgs);
                        const callArgs = try pushCallArgs(c, node.head.callExpr.arg_head);
                        const reqRet = bt.Any;

                        c.curNodeId = node.head.callExpr.callee;
                        if (try getOrResolveSymForFuncCall(c, c.semaRootSymId, nameId, callArgs.argTypes, reqRet, callArgs.hasDynamicArg)) |callRes| {
                            try referenceSym(c, callRes.csymId, true);
                            c.nodes[node.head.callExpr.callee].head.ident.sema_csymId = callRes.csymId;
                            return callRes.retType;
                        }
                        return c.reportErrorAt("Undeclared func `{}`.", &.{v(name)}, node.head.callExpr.callee);
                    }
                },
            }
        } else {
            // All other callees are treated as function value calls.
            var numArgs: u32 = 0;
            var arg_id = node.head.callExpr.arg_head;
            while (arg_id != cy.NullId) : (numArgs += 1) {
                const arg = c.nodes[arg_id];
                _ = try semaExpr(c, arg_id);
                arg_id = arg.next;
            }

            _ = try semaExpr(c, node.head.callExpr.callee);
            return bt.Any;
        }
    } else return c.reportErrorAt("Unsupported named args", &.{}, nodeId);
}

fn identifier(c: *cy.Chunk, nodeId: cy.NodeId) !TypeId {
    const node = c.nodes[nodeId];
    const name = c.getNodeTokenString(node);
    const res = try getOrLookupVar(c, name, true);
    switch (res) {
        .local => |id| {
            c.nodes[nodeId].head.ident.semaVarId = id;
            return c.vars.items[id].vtype;
        },
        .static => {
            const nameId = try ensureNameSym(c.compiler, name);

            const symRes = try mustGetOrResolveDistinctSym(c, c.semaRootSymId, nameId);
            const csymId = symRes.toCompactId();
            try referenceSym(c, csymId, true);
            c.nodes[nodeId].head.ident.sema_csymId = csymId;

            const rSym = c.compiler.sema.getSymbol(symRes.symId);
            switch (rSym.symT) {
                .variable => {
                    return rSym.inner.variable.rTypeSymId;
                },
                .builtinType => {
                    return bt.MetaType;
                },
                else => {
                    return bt.Any;
                },
            }
        },
        .not_found => {
            return c.reportErrorAt("Undeclared variable `{}`.", &.{v(name)}, nodeId);
        },
    }
}

/// Recursively walks a type spec head node and returns the final resolved type sym.
/// Assumes head is non null.
fn getOrResolveTypeSymFromSpecNode(chunk: *cy.Chunk, head: cy.NodeId) !SymbolId {
    var nodeId = head;
    var parentSymId = chunk.semaRootSymId;
    // log.debug("getOrResolveTypeSymFromSpecNode from {} ", .{parentSymId});
    while (true) {
        const node = chunk.nodes[nodeId];
        const name = chunk.getNodeTokenString(node);
        const nameId = try ensureNameSym(chunk.compiler, name);
        // log.debug("looking for {s}", .{name});

        if (node.next == cy.NullId) {
            chunk.curNodeId = nodeId;
            return getOrResolveTypeSym(chunk, parentSymId, nameId);
        } else {
            const res = try mustGetOrResolveDistinctSym(chunk, parentSymId, nameId);
            parentSymId = res.symId;
        }
        nodeId = node.next;
    }
}

fn appendFuncDecl(chunk: *cy.Chunk, nodeId: cy.NodeId, isStatic: bool) !FuncDeclId {
    const func = chunk.nodes[nodeId];
    const header = chunk.nodes[func.head.func.header];

    var decl = FuncDecl{
        .nodeId = nodeId,
        .paramHead = header.head.funcHeader.paramHead,
        .rRetTypeSymId = cy.NullId,
        .inner = undefined,
        .isStatic = isStatic,
        .numParams = undefined,
        .funcSigId = undefined,
    };
    if (isStatic) {
        decl.inner = .{
            .lambda = .{},
        };
    } else {
        decl.inner = .{
            .staticFunc = .{},
        };
    }

    // Get params, build func signature.
    chunk.compiler.tempSyms.clearRetainingCapacity();
    var curParamId = header.head.funcHeader.paramHead;
    var numParams: u8 = 0;
    while (curParamId != cy.NullId) {
        const param = chunk.nodes[curParamId];
        if (param.head.funcParam.typeSpecHead == cy.NullId) {
            try chunk.compiler.tempSyms.append(chunk.alloc, bt.Any);
        } else {
            const rTypeSymId = try getOrResolveTypeSymFromSpecNode(chunk, param.head.funcParam.typeSpecHead);
            try chunk.compiler.tempSyms.append(chunk.alloc, rTypeSymId);
        }
        numParams += 1;
        curParamId = param.next;
    }
    decl.numParams = numParams;

    // Get return type.
    var retType: TypeId = undefined;
    if (header.head.funcHeader.ret != cy.NullId) {
        retType = try getOrResolveTypeSymFromSpecNode(chunk, header.head.funcHeader.ret);
    } else {
        retType = bt.Dynamic;
    }

    // Resolve func signature.
    decl.funcSigId = try ensureFuncSig(chunk.compiler, chunk.compiler.tempSyms.items, retType);
    decl.rRetTypeSymId = retType;

    const declId: u32 = @intCast(chunk.semaFuncDecls.items.len);
    try chunk.semaFuncDecls.append(chunk.alloc, decl);
    return declId;
}

pub fn pushBlock(self: *cy.Chunk, funcDeclId: u32) !BlockId {
    self.curSemaBlockId = @intCast(self.semaBlocks.items.len);
    const nextSubBlockId: u32 = @intCast(self.semaSubBlocks.items.len);
    var isStaticFuncBlock = false;
    if (funcDeclId != cy.NullId) {
        isStaticFuncBlock = self.semaFuncDecls.items[funcDeclId].isStatic;
    }
    try self.semaBlocks.append(self.alloc, Block.init(funcDeclId, nextSubBlockId, isStaticFuncBlock));
    try self.semaBlockStack.append(self.alloc, self.curSemaBlockId);
    try pushSubBlock(self);
    return self.curSemaBlockId;
}

fn pushSubBlock(self: *cy.Chunk) !void {
    curBlock(self).subBlockDepth += 1;
    const prev = self.curSemaSubBlockId;
    self.curSemaSubBlockId = @intCast(self.semaSubBlocks.items.len);
    try self.semaSubBlocks.append(self.alloc, SubBlock.init(prev, self.assignedVarStack.items.len, self.declaredVarStack.items.len));
}

fn pushMethodParamVars(c: *cy.Chunk, func: *const FuncDecl) !void {
    const curNodeId = c.curNodeId;
    defer c.curNodeId = curNodeId;

    const sblock = curBlock(c);

    // Add self receiver param.
    var id = try pushLocalVar(c, .param, "self", bt.Any);
    c.vars.items[id].inner.paramIdx = 0;
    c.vars.items[id].isDeclared = true;
    try sblock.params.append(c.alloc, id);

    if (func.numParams > 1) {
        const rFuncSig = c.compiler.sema.resolvedFuncSigs.items[func.funcSigId];
        const params = rFuncSig.params()[1..];

        // Skip the first param node.
        var curNode = func.paramHead;
        curNode = c.nodes[curNode].next;

        for (params, 0..) |rParamSymId, idx| {
            const param = c.nodes[curNode];
            const name = c.getNodeTokenString(c.nodes[param.head.funcParam.name]);

            try types.assertTypeSym(c, rParamSymId);

            c.curNodeId = curNode;
            id = try pushLocalVar(c, .param, name, rParamSymId);
            c.vars.items[id].inner.paramIdx = @intCast(idx + 1);
            c.vars.items[id].isDeclared = true;
            try sblock.params.append(c.alloc, id);

            curNode = param.next;
        }
    }
}

fn appendFuncParamVars(chunk: *cy.Chunk, func: *const FuncDecl) !void {
    const sblock = curBlock(chunk);

    if (func.numParams > 0) {
        const rFuncSig = chunk.compiler.sema.resolvedFuncSigs.items[func.funcSigId];
        const params = rFuncSig.params();
        var curNode = func.paramHead;
        for (params, 0..) |rParamSymId, idx| {
            const param = chunk.nodes[curNode];
            const name = chunk.getNodeTokenString(chunk.nodes[param.head.funcParam.name]);

            try types.assertTypeSym(chunk, rParamSymId);
            const id = try pushLocalVar(chunk, .param, name, rParamSymId);
            chunk.vars.items[id].inner.paramIdx = @intCast(idx);
            chunk.vars.items[id].isDeclared = true;
            try sblock.params.append(chunk.alloc, id);

            curNode = param.next;
        }
    }
}

fn pushLocalVar(c: *cy.Chunk, _type: LocalVarType, name: []const u8, vtype: TypeId) !LocalVarId {
    const sblock = curBlock(c);
    const id: u32 = @intCast(c.vars.items.len);
    const res = try sblock.nameToVar.getOrPut(c.alloc, name);
    if (res.found_existing) {
        return c.reportError("Var `{}` already exists", &.{v(name)});
    } else {
        res.value_ptr.* = id;
        try c.vars.append(c.alloc, .{
            .type = _type,
            .name = if (builtin.mode == .Debug) name else {},
            .vtype = toLocalType(vtype),
            .lifetimeRcCandidate = types.isRcCandidateType(c.compiler, vtype),
        });
        return id;
    }
}

fn getVarPtr(self: *cy.Chunk, name: []const u8) ?*LocalVar {
    if (curBlock(self).nameToVar.get(name)) |varId| {
        return &self.vars.items[varId];
    } else return null;
}

fn pushStaticVarAlias(c: *cy.Chunk, name: []const u8, csymId: CompactSymbolId) !LocalVarId {
    const id = try pushLocalVar(c, .staticAlias, name, bt.Any);
    c.vars.items[id].inner.staticAlias = .{
        .csymId = csymId,
    };
    return id;
}

fn pushObjectMemberAlias(c: *cy.Chunk, name: []const u8) !LocalVarId {
    const id = try pushLocalVar(c, .objectMemberAlias, name, bt.Any);
    return id;
}

fn pushCapturedObjectMemberAlias(self: *cy.Chunk, name: []const u8, parentVarId: LocalVarId, vtype: TypeId) !LocalVarId {
    const block = curBlock(self);
    const id = try pushLocalVar(self, .parentObjectMemberAlias, name, vtype);
    const capturedIdx: u8 = @intCast(block.captures.items.len);
    self.vars.items[id].capturedIdx = capturedIdx;
    self.vars.items[id].isBoxed = true;
    self.vars.items[id].isDefinedOnce = true;

    try self.capVarDescs.put(self.alloc, id, .{
        .user = parentVarId,
    });

    try block.captures.append(self.alloc, id);
    return id;
}

fn pushCapturedVar(self: *cy.Chunk, name: []const u8, parentVarId: LocalVarId, vtype: TypeId) !LocalVarId {
    const block = curBlock(self);
    const id = try pushLocalVar(self, .parentLocalAlias, name, vtype);
    const capturedIdx: u8 = @intCast(block.captures.items.len);
    self.vars.items[id].capturedIdx = capturedIdx;
    self.vars.items[id].isBoxed = true;
    self.vars.items[id].isDefinedOnce = true;

    try self.capVarDescs.put(self.alloc, id, .{
        .user = parentVarId,
    });

    try block.captures.append(self.alloc, id);
    return id;
}

fn pushLocalBodyVar(self: *cy.Chunk, name: []const u8, vtype: TypeId) !LocalVarId {
    const id = try pushLocalVar(self, .local, name, vtype);
    try curBlock(self).locals.append(self.alloc, id);
    return id;
}

fn getOrDeclareLocal(self: *cy.Chunk, ident: cy.NodeId, vtype: TypeId) !LocalVarId {
    const node = self.nodes[ident];
    const name = self.getNodeTokenString(node);
    if (curBlock(self).nameToVar.get(name)) |varId| {
        self.nodes[ident].head.ident.semaVarId = varId;
        if (!self.vars.items[varId].isDeclared) {
            self.vars.items[varId].isDeclared = true;
            try self.declaredVarStack.append(self.alloc, varId);
        }
        return varId;
    } else {
        // Declare var.
        const id = try pushLocalBodyVar(self, name, vtype);
        self.vars.items[id].isDeclared = true;
        self.nodes[ident].head.ident.semaVarId = id;
        try self.declaredVarStack.append(self.alloc, id);
        return id;
    }
}

fn referenceSym(c: *cy.Chunk, symId: CompactSymbolId, trackDep: bool) !void {
    if (trackDep) {
        if (c.isInStaticInitializer()) {
            // Record this symbol as a dependency.
            const res = try c.semaInitializerSyms.getOrPut(c.alloc, c.curSemaInitingSym);
            if (res.found_existing) {
                const depRes = try c.semaVarDeclDeps.getOrPut(c.alloc, symId);
                if (!depRes.found_existing) {
                    try c.bufU32.append(c.alloc, @bitCast(symId));
                    res.value_ptr.*.depsEnd = @intCast(c.bufU32.items.len);
                    depRes.value_ptr.* = {};
                }
            } else {
                const start: u32 = @intCast(c.bufU32.items.len);
                try c.bufU32.append(c.alloc, @bitCast(symId));
                res.value_ptr.* = .{
                    .depsStart = start,
                    .depsEnd = @intCast(c.bufU32.items.len),
                };
            }
        }
    }
}

const VarLookupResult = union(enum) {
    static: CompactSymbolId,

    /// Local, parent local alias, or parent object member alias.
    local: LocalVarId,

    not_found: void,
};

/// Static var lookup is skipped for callExpr since there is a chance it can fail on a
/// symbol with overloaded signatures.
fn getOrLookupVar(self: *cy.Chunk, name: []const u8, staticLookup: bool) !VarLookupResult {
    const sblock = curBlock(self);
    if (sblock.nameToVar.get(name)) |varId| {
        const svar = self.vars.items[varId];
        if (svar.type == .staticAlias) {
            return VarLookupResult{
                .static = @bitCast(svar.inner.staticAlias.csymId),
            };
        } else if (svar.type == .objectMemberAlias) {
            return VarLookupResult{
                .local = varId,
            };
        } else if (svar.isParentLocalAlias()) {
            // Can not reference local var in a static var decl unless it's in a nested block.
            // eg. var a = 0
            //     var b: a
            if (self.isInStaticInitializer() and self.semaBlockDepth() == 1) {
                self.compiler.errorPayload = self.curNodeId;
                return error.CanNotUseLocal;
            }
            return VarLookupResult{
                .local = varId,
            };
        } else {
            if (self.isInStaticInitializer() and self.semaBlockDepth() == 1) {
                self.compiler.errorPayload = self.curNodeId;
                return error.CanNotUseLocal;
            }
            if (!svar.isDeclared) {
                return VarLookupResult{
                    .not_found = {},
                };
            } else {
                return VarLookupResult{
                    .local = varId,
                };
            }
        }
    }

    // Look for object member if inside method.
    if (sblock.isMethodBlock) {
        const nameId = try ensureNameSym(self.compiler, name);
        const key = ObjectMemberKey{
            .objectMemberKey = .{
                .objSymId = self.curObjectSymId,
                .memberNameId = nameId,
            },
        };
        if (self.compiler.sema.objectMembers.contains(key)) {
            const id = try pushObjectMemberAlias(self, name);
            return VarLookupResult{
                .local = id,
            };
        }
    }

    if (try lookupParentLocal(self, name)) |res| {
        if (self.isInStaticInitializer()) {
            // Can not capture local before this block.
            if (res.blockDepth == 1) {
                self.compiler.errorPayload = self.curNodeId;
                return error.CanNotUseLocal;
            }
        } else if (sblock.isStaticFuncBlock) {
            // Can not capture local before static function block.
            const func = self.semaFuncDecls.items[sblock.funcDeclId];
            const funcName = func.getName(self);
            return self.reportErrorAt("Can not capture the local variable `{}` from static function `{}`.\nOnly lambdas (anonymous functions) can capture local variables.", &.{v(name), v(funcName)}, self.curNodeId);
        }

        // Create a local captured variable.
        const parentVar = self.vars.items[res.varId];
        if (res.isObjectMember) {
            const parentBlockId = self.semaBlockStack.items[res.blockDepth - 1];
            const parentBlock = &self.semaBlocks.items[parentBlockId];
            const selfId = parentBlock.nameToVar.get("self").?;
            var resVarId = res.varId;
            if (self.vars.items[selfId].type == .param) {
                resVarId = try replaceParamWithCopy(self, parentBlock, "self", selfId);
            }

            const id = try pushCapturedObjectMemberAlias(self, name, resVarId, parentVar.vtype);

            return VarLookupResult{
                .local = id,
            };
        } else {
            var resVarId = res.varId;
            if (self.vars.items[res.varId].type == .param) {
                const parentBlockId = self.semaBlockStack.items[res.blockDepth - 1];
                const parentBlock = &self.semaBlocks.items[parentBlockId];
                resVarId = try replaceParamWithCopy(self, parentBlock, name, res.varId);
            }

            const id = try pushCapturedVar(self, name, resVarId, parentVar.vtype);
            return VarLookupResult{
                .local = id,
            };
        }
    } else {
        if (staticLookup) {
            const nameId = try ensureNameSym(self.compiler, name);
            const res = (try getOrResolveDistinctSym(self, self.semaRootSymId, nameId)) orelse {
                return VarLookupResult{
                    .not_found = {},
                };
            };
            _ = try pushStaticVarAlias(self, name, res.toCompactId());
            return VarLookupResult{
                .static = @bitCast(res.toCompactId()),
            };
        } else {
            return VarLookupResult{
                .not_found = {},
            };
        }
    }
}

const LookupParentLocalResult = struct {
    varId: LocalVarId,

    // Main block starts at 1.
    blockDepth: u32,

    isObjectMember: bool,
};

fn lookupParentLocal(c: *cy.Chunk, name: []const u8) !?LookupParentLocalResult {
    // Only check one block above.
    if (c.semaBlockDepth() > 1) {
        const prevId = c.semaBlockStack.items[c.semaBlockDepth() - 1];
        const prev = c.semaBlocks.items[prevId];
        if (prev.nameToVar.get(name)) |varId| {
            if (c.vars.items[varId].type != .staticAlias) {
                return .{
                    .varId = varId,
                    .blockDepth = c.semaBlockDepth(),
                    .isObjectMember = false,
                };
            }
        }

        // Look for object member if inside method.
        if (prev.isMethodBlock) {
            const nameId = try ensureNameSym(c.compiler, name);
            const key = ObjectMemberKey{
                .objectMemberKey = .{
                    .objSymId = c.curObjectSymId,
                    .memberNameId = nameId,
                },
            };
            if (c.compiler.sema.objectMembers.contains(key)) {
                return .{
                    .varId = prev.nameToVar.get("self").?,
                    .blockDepth = c.semaBlockDepth(),
                    .isObjectMember = true,
                };
            }
        }
    }
    return null;
}

fn getTypeForResolvedValueSym(chunk: *cy.Chunk, csymId: CompactSymbolId) !TypeId {
    if (csymId.isFuncSymId) {
        return bt.Any;
    } else {
        const rSym = chunk.compiler.sema.getSymbol(csymId.id);
        switch (rSym.symT) {
            .variable => {
                return rSym.inner.variable.rTypeSymId;
            },
            .enumMember => {
                return rSym.key.resolvedSymKey.parentSymId;
            },
            else => {
                return bt.Any;
            },
        }
    }
}

/// Give a name to the internal sym for formatting.
pub fn addResolvedInternalSym(c: *cy.VMcompiler, name: []const u8) !SymbolId {
    const nameId = try ensureNameSym(c, name);
    const id: u32 = @intCast(c.sema.resolvedSyms.items.len);
    try c.sema.resolvedSyms.append(c.alloc, .{
        .key = ResolvedSymKey{
            .resolvedSymKey = .{
                .parentSymId = cy.NullId,
                .nameId = nameId,
            },
        },
        .symT = .internal,
        .inner = undefined,
        .exported = true,
    });
    return id;
}

pub fn addBuiltinSym(c: *cy.VMcompiler, name: []const u8, typeId: rt.TypeId) !SymbolId {
    const nameId = try ensureNameSym(c, name);
    const key = ResolvedSymKey{
        .resolvedSymKey = .{
            .parentSymId = cy.NullId,
            .nameId = nameId,
        },
    };

    const spec = try std.fmt.allocPrint(c.alloc, "builtin.{s}", .{name});
    defer c.alloc.free(spec);
    const modId = try cy.module.appendModule(c, spec);
    const mod = c.sema.getModulePtr(modId);

    const id: u32 = @intCast(c.sema.resolvedSyms.items.len);
    try c.sema.resolvedSyms.append(c.alloc, .{
        .key = key,
        .symT = .builtinType,
        .inner = .{
            .builtinType = .{
                .modId = modId,
                .typeId = typeId,
            },
        },
        .exported = true,
    });
    try c.sema.resolvedSymMap.put(c.alloc, key, id);
    mod.resolvedRootSymId = id;
    return id;
}

fn addResolvedUntypedFuncSig(c: *cy.VMcompiler, numParams: u32) !FuncSigId {
    // AnyType for params and return.
    try c.tempSyms.resize(c.alloc, numParams);
    for (c.tempSyms.items) |*stype| {
        stype.* = bt.Any;
    }
    return ensureFuncSig(c, c.tempSyms.items, bt.Any);
}

pub fn ensureResolvedUntypedFuncSig(c: *cy.VMcompiler, numParams: u32) !FuncSigId {
    if (numParams < c.sema.resolvedUntypedFuncSigs.items.len) {
        var funcSigId = c.sema.resolvedUntypedFuncSigs.items[numParams];
        if (funcSigId == cy.NullId) {
            funcSigId = try addResolvedUntypedFuncSig(c, numParams);
            c.sema.resolvedUntypedFuncSigs.items[numParams] = funcSigId;
        }
        return funcSigId;
    }
    const end = c.sema.resolvedUntypedFuncSigs.items.len;
    try c.sema.resolvedUntypedFuncSigs.resize(c.alloc, numParams + 1);
    for (end..c.sema.resolvedUntypedFuncSigs.items.len) |i| {
        c.sema.resolvedUntypedFuncSigs.items[i] = cy.NullId;
    }
    const funcSigId = try addResolvedUntypedFuncSig(c, numParams);
    c.sema.resolvedUntypedFuncSigs.items[numParams] = funcSigId;
    return funcSigId;
}

pub fn ensureFuncSig(c: *cy.VMcompiler, params: []const TypeId, ret: TypeId) !FuncSigId {
    const res = try c.sema.resolvedFuncSigMap.getOrPut(c.alloc, .{
        .paramPtr = params.ptr,
        .paramLen = @intCast(params.len),
        .retSymId = ret,
    });
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        const id: u32 = @intCast(c.sema.resolvedFuncSigs.items.len);
        const new = try c.alloc.dupe(SymbolId, params);
        var isParamsTyped = false;
        for (params) |symId| {
            if (symId != bt.Any) {
                isParamsTyped = true;
                break;
            }
        }
        var isTyped = isParamsTyped;
        if (ret != bt.Any) {
            isTyped = true;
        }
        try c.sema.resolvedFuncSigs.append(c.alloc, .{
            .paramPtr = new.ptr,
            .paramLen = @intCast(new.len),
            .retSymId = ret,
            .isTyped = isTyped,
            .isParamsTyped = isParamsTyped,
        });
        res.value_ptr.* = id;
        res.key_ptr.* = .{
            .paramPtr = new.ptr,
            .paramLen = @intCast(new.len),
            .retSymId = ret,
        };
        return id;
    }
}

/// Format: (Type, ...) RetType
pub fn getFuncSigTempStr(c: *cy.VMcompiler, funcSigId: FuncSigId) ![]const u8 {
    c.vm.u8Buf.clearRetainingCapacity();
    const w = c.vm.u8Buf.writer(c.alloc);
    try writeFuncSigStr(c, w, funcSigId);
    return c.vm.u8Buf.items();
}
pub fn allocFuncSigStr(c: *cy.VMcompiler, funcSigId: FuncSigId) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(c.alloc);

    const w = buf.writer(c.alloc);
    try writeFuncSigStr(c, w, funcSigId);
    return buf.toOwnedSlice(c.alloc);
}
pub fn writeFuncSigStr(c: *cy.VMcompiler, w: anytype, funcSigId: FuncSigId) !void {
    const rFuncSig = c.sema.resolvedFuncSigs.items[funcSigId];
    try writeFuncSigTypesStr(c, w, rFuncSig.params(), rFuncSig.retSymId);
}

pub fn allocFuncSigTypesStr(c: *cy.VMcompiler, params: []const TypeId, ret: TypeId) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(c.alloc);

    const w = buf.writer(c.alloc);
    try writeFuncSigTypesStr(c, w, params, ret);
    return buf.toOwnedSlice(c.alloc);
}
pub fn writeFuncSigTypesStr(c: *cy.VMcompiler, w: anytype, params: []const TypeId, ret: TypeId) !void {
    try w.writeAll("(");

    if (params.len > 0) {
        var rParamSym = c.sema.getSymbol(params[0]);
        var name = getName(c, rParamSym.key.resolvedSymKey.nameId);
        try w.writeAll(name);

        if (params.len > 1) {
            for (params[1..]) |rParamSymId| {
                try w.writeAll(", ");
                rParamSym = c.sema.getSymbol(rParamSymId);
                name = getName(c, rParamSym.key.resolvedSymKey.nameId);
                try w.writeAll(name);
            }
        }
    }
    try w.writeAll(") ");

    var rRetSym = c.sema.getSymbol(ret);
    var name = getName(c, rRetSym.key.resolvedSymKey.nameId);
    try w.writeAll(name);
}

pub const CompactSymbolId = packed struct {
    id: u31,
    isFuncSymId: bool,

    pub fn initNull() CompactSymbolId {
        return @bitCast(@as(u32, cy.NullId));
    }

    pub fn initSymId(id: SymbolId) CompactSymbolId {
        return .{
            .id = @intCast(id),
            .isFuncSymId = false,
        };
    }

    pub fn initFuncSymId(id: FuncSymId) CompactSymbolId {
        return .{
            .id = @intCast(id),
            .isFuncSymId = true,
        };
    }

    fn isNull(self: CompactSymbolId) bool {
        return cy.NullId == @as(u32, @bitCast(self));
    }

    pub fn isPresent(self: CompactSymbolId) bool {
        return cy.NullId != @as(u32, @bitCast(self));
    }
};

fn checkTypeSym(c: *cy.Chunk, symId: SymbolId, nameId: NameSymId) !void {
    const rSym = c.compiler.sema.getSymbol(symId);
    if (rSym.symT == .object) {
        return;
    } else if (rSym.symT == .builtinType) {
        return;
    } else {
        const name = getName(c.compiler, nameId);
        return c.reportError("`{}` is not a type symbol.", &.{v(name)});
    }
}

fn getOrResolveTypeSym(chunk: *cy.Chunk, parentSymId: SymbolId, nameId: NameSymId) !SymbolId {
    // Check builtin types.
    if (parentSymId == chunk.semaRootSymId) {
        if (nameId < NameBuiltinTypeEnd) {
            return @intCast(nameId);
        }
    }

    var key: cy.hash.KeyU64 = undefined;
    if (parentSymId == chunk.semaRootSymId) {
        // Faster check against local syms.
        key = LocalSymKey{
            .localSymKey = .{
                .nameId = nameId,
                .funcSigId = cy.NullId,
            },
        };
        if (chunk.localSyms.get(key)) |sym| {
            if (sym.symId != cy.NullId) {
                try checkTypeSym(chunk, sym.symId, nameId);
                return sym.symId;
            } else {
                // Unresolved.
                const res = try resolveDistinctLocalSym(chunk, key);
                try checkTypeSym(chunk, res.symId, nameId);
                return res.symId;
            }
        }
    }

    key = ResolvedSymKey{
        .resolvedSymKey = .{
            .parentSymId = parentSymId,
            .nameId = nameId,
        },
    };

    var symId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;
    if (symId != cy.NullId) {
        try checkTypeSym(chunk, symId, nameId);
        return symId;
    }

    const parentSym = chunk.compiler.sema.resolvedSyms.items[parentSymId];
    if (parentSym.symT == .module) {
        const modId = parentSym.inner.module.id;
        if (try resolveTypeSymFromModule(chunk, modId, nameId)) |resSymId| {
            return resSymId;
        }
    }
    const name = getName(chunk.compiler, nameId);
    return chunk.reportError("Could not find type symbol `{}`.", &.{v(name)});
}

fn mustGetOrResolveDistinctSym(chunk: *cy.Chunk, parentSymId: SymbolId, nameId: NameSymId) !SymbolResult {
    return try getOrResolveDistinctSym(chunk, parentSymId, nameId) orelse {
        const name = getName(chunk.compiler, nameId);
        return chunk.reportError("Can not find the symbol `{}`.", &.{v(name)});
    };
}

fn resolveDistinctLocalSym(chunk: *cy.Chunk, lkey: LocalSymKey) !SymbolResult {
    const sym = chunk.localSyms.getPtr(lkey).?;

    // First check resolved syms.
    const key = ResolvedSymKey{
        .resolvedSymKey = .{
            .parentSymId = sym.parentSymId,
            .nameId = lkey.localSymKey.nameId,
        },
    };
    if (chunk.compiler.sema.resolvedSymMap.get(key)) |symId| {
        const rSym = chunk.compiler.sema.getSymbol(symId);
        if (rSym.symT == .func) {
            if (rSym.inner.func.funcSymId == cy.NullId) {
                const name = getName(chunk.compiler, lkey.localSymKey.nameId);
                return chunk.reportError("Can not disambiguate the symbol `{}`.", &.{v(name)});
            } else {
                sym.symId = symId;
                sym.funcSymId = rSym.inner.func.funcSymId;
                return SymbolResult{
                    .symId = sym.symId,
                    .funcSymId = sym.funcSymId,
                };
            }
        } else {
            sym.symId = symId;
            sym.funcSymId = cy.NullId;
        }
    } else {
        const rParentSym = chunk.compiler.sema.getSymbol(sym.parentSymId);
        if (rParentSym.symT == .module) {
            const csymId = (try resolveSymFromModule(chunk, rParentSym.inner.module.id, lkey.localSymKey.nameId, cy.NullId)).?;
            if (csymId.isFuncSymId) {
                const rFuncSym = chunk.compiler.sema.getFuncSym(csymId.id);
                sym.symId = rFuncSym.key.resolvedFuncSymKey.symId;
                sym.funcSymId = csymId.id;
            } else {
                sym.symId = csymId.id;
                sym.funcSymId = cy.NullId;
            }
        } else {
            cy.fatal();
        }
    }
    return SymbolResult{
        .symId = sym.symId,
        .funcSymId = sym.funcSymId,
    };
}

/// TODO: This should perform type checking.
fn getOrResolveDistinctSym(chunk: *cy.Chunk, parentSymId: SymbolId, nameId: NameSymId) !?SymbolResult {
    log.debug("getDistinctSym {}.{s}", .{parentSymId, getName(chunk.compiler, nameId)} );
    var key: cy.hash.KeyU64 = undefined;
    if (parentSymId == chunk.semaRootSymId) {
        // Faster check against local syms.
        key = LocalSymKey.initLocalSymKey(nameId, null);
        if (chunk.localSyms.get(key)) |sym| {
            if (sym.symId != cy.NullId) {
                return SymbolResult{
                    .symId = sym.symId,
                    .funcSymId = sym.funcSymId,
                };
            } else {
                // Unresolved.
                const res = try resolveDistinctLocalSym(chunk, key);
                return SymbolResult{
                    .symId = res.symId,
                    .funcSymId = res.funcSymId,
                };
            }
        }
    }

    key = ResolvedSymKey{
        .resolvedSymKey = .{
            .parentSymId = parentSymId,
            .nameId = nameId,
        },
    };

    var symId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;
    if (symId != cy.NullId) {
        const rSym = chunk.compiler.sema.resolvedSyms.items[symId];
        if (rSym.symT == .func) {
            if (rSym.inner.func.funcSymId == cy.NullId) {
                const name = getName(chunk.compiler, nameId);
                return chunk.reportError("Can not disambiguate the symbol `{}`.", &.{v(name)});
            } else {
                return SymbolResult{
                    .symId = symId,
                    .funcSymId = rSym.inner.func.funcSymId,
                };
            }
        } else {
            return SymbolResult{
                .symId = symId,
                .funcSymId = cy.NullId,
            };
        }
    }
            
    const parentSym = chunk.compiler.sema.getSymbol(parentSymId);
    if (parentSym.getModuleId()) |modId| {
        if (try cy.module.findDistinctModuleSym(chunk, modId, nameId)) {
            const csymId = (try resolveSymFromModule(chunk, modId, nameId, cy.NullId)).?;
            if (csymId.isFuncSymId) {
                const rFuncSym = chunk.compiler.sema.getFuncSym(csymId.id);
                return SymbolResult{
                    .symId = rFuncSym.key.resolvedFuncSymKey.symId,
                    .funcSymId = csymId.id,
                };
            } else {
                return SymbolResult{
                    .symId = csymId.id,
                    .funcSymId = cy.NullId,
                };
            }
        } else {
            // Check builtin types.
            if (parentSymId == chunk.semaRootSymId) {
                if (nameId < NameBuiltinTypeEnd) {
                    return SymbolResult{
                        .symId = @intCast(nameId),
                        .funcSymId = cy.NullId,
                    };
                }
            }
        }
    }

    // Look in using modules.
    for (chunk.usingModules.items) |modId| {
        if (try cy.module.findDistinctModuleSym(chunk, modId, nameId)) {
            const mod = chunk.compiler.sema.getModule(modId);
            // Cache to local syms.
            try setLocalSym(chunk, nameId, .{
                .symId = cy.NullId,
                .funcSymId = cy.NullId,
                .parentSymId = mod.resolvedRootSymId,
            });

            const csymId = (try resolveSymFromModule(chunk, modId, nameId, cy.NullId)).?;
            if (csymId.isFuncSymId) {
                const rFuncSym = chunk.compiler.sema.getFuncSym(csymId.id);
                return SymbolResult{
                    .symId = rFuncSym.key.resolvedFuncSymKey.symId,
                    .funcSymId = csymId.id,
                };
            } else {
                return SymbolResult{
                    .symId = csymId.id,
                    .funcSymId = cy.NullId,
                };
            }
        }
    }

    return null;
}

// pub const FuncCandidate = struct {
//     parentSymId: SymbolId,
//     funcSigId: FuncSigId,
// };

// /// Query for a list of potential func sigs by sym path and number of params.
// fn pushFuncCandidates(
//     c: *cy.Chunk, parentSymId: SymbolId, nameId: NameSymId, numParams: u32
// ) !cy.IndexSlice(u32) {
//     var key: cy.hash.KeyU64 = undefined;

//     var effParentSymId = parentSymId;

//     // Check for a using symbol or builtin sym.
//     if (parentSymId == c.semaRootSymId) {
//         key = RelLocalSymKey{
//             .localSymKey = .{
//                 .nameId = nameId,
//                 .funcSigId = cy.NullId,
//             },
//         };
//         if (c.localSyms.get(key)) |sym| {
//             effParentSymId = sym.parentSymId;
//         } else {
//             if (nameId < types.BuiltinTypeTags.len) {
//                 effParentSymId = cy.NullId;
//             }
//         }
//     }

//     const start = c.funcCandidateStack.items.len;

//     const parentSym = c.compiler.sema.getSymbol(effParentSymId);
//     if (parentSym.getModuleId()) |modId| {
//         try pushModuleFuncCandidates(c, modId, nameId, numParams);
//     }

//     // Look for <call> magic function.
//     key = ResolvedSymKey{
//         .resolvedSymKey = .{
//             .parentSymId = effParentSymId,
//             .nameId = nameId,
//         },
//     };
//     if (c.compiler.sema.resolvedSymMap.get(key)) |symId| {
//         const sym = c.compiler.sema.getSymbol(symId);
//         if (sym.getModuleId()) |modId| {
//             const callNameId = try ensureNameSym(c.compiler, "<call>");
//             try pushModuleFuncCandidates(c, modId, callNameId, numParams);
//         }
//     }

//     return cy.IndexSlice(u32).init(
//         @intCast(start),
//         @intCast(c.funcCandidateStack.items.len)
//     );
// }

fn getResolvedParentSymId(chunk: *cy.Chunk, parentSymId: SymbolId, nameId: NameSymId) SymbolId {
    var rParentSymId = parentSymId;
    if (parentSymId == chunk.semaRootSymId) {
        // Check for local sym.
        const key = LocalSymKey.initLocalSymKey(nameId, null);
        if (chunk.localSyms.get(key)) |sym| {
            rParentSymId = sym.parentSymId;
        } else {
            if (nameId < NameBuiltinTypeEnd) {
                rParentSymId = cy.NullId;
            }
        }
    }
    return rParentSymId;
}

fn getFuncSigForSym(chunk: *cy.Chunk, symId: SymbolId) ?FuncSigId {
    const sym = chunk.compiler.sema.getSymbol(symId);
    if (sym.symT == .func) {
        const funcSymId = sym.inner.func.funcSymId;
        if (funcSymId == cy.NullId) {
            // TODO: Get first overloaded func.
            return null;
        } else {
            const funcSym = chunk.compiler.sema.getFuncSym(funcSymId);
            return funcSym.getFuncSigId();
        }
    } else return null;
}

fn getFirstFuncSigForSym(chunk: *cy.Chunk, parentSymId: SymbolId, nameId: NameSymId) ?FuncSigId {
    const rParentSymId = getResolvedParentSymId(chunk, parentSymId, nameId);

    const key = ResolvedSymKey.initResolvedSymKey(rParentSymId, nameId);
    const symId = chunk.compiler.sema.resolvedSymMap.get(key) orelse return null;
    return getFuncSigForSym(chunk, symId);
}

fn getFirstFuncSigInModule(chunk: *cy.Chunk, modId: cy.ModuleId, nameId: NameSymId) ?FuncSigId {
    const mod = chunk.compiler.sema.getModule(modId);
    const key = ModuleSymKey.initModuleSymKey(nameId, null);
    const sym = mod.syms.get(key) orelse return null;
    switch (sym.symT) {
        .symToOneFunc => return sym.inner.symToOneFunc.funcSigId,
        .symToManyFuncs => {
            return sym.inner.symToManyFuncs.head.funcSigId;
        },
        else => {
            return null;
        }
    }
}

const FuncCallSymResult = struct {
    csymId: CompactSymbolId,
    funcSigId: FuncSigId,
    retType: TypeId,
    typeChecked: bool,
};

/// Assumes parentSymId is not null.
/// Returns CompileError if the symbol exists but can't be used for a function call.
/// Returns null if the symbol is missing but can still be used as an accessor.
fn getOrResolveSymForFuncCall(
    chunk: *cy.Chunk, parentSymId: SymbolId, nameId: NameSymId, args: []const TypeId, ret: TypeId, hasDynamicArg: bool,
) !?FuncCallSymResult {
    const funcSigId = try ensureFuncSig(chunk.compiler, args, ret);
    log.debug("getFuncCallSym {}.{s}, sig: {s}", .{parentSymId, getName(chunk.compiler, nameId), try getFuncSigTempStr(chunk.compiler, funcSigId)} );

    // TODO: Cache lookup by symId and funcSigId.

    const rParentSymId = getResolvedParentSymId(chunk, parentSymId, nameId);

    var key = ResolvedSymKey.initResolvedSymKey(rParentSymId, nameId);
    var symId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;
    if (symId != cy.NullId) {
        const sym = chunk.compiler.sema.getSymbol(symId);
        switch (sym.symT) {
            .variable => {
                // TODO: Check var type.
                return FuncCallSymResult{
                    .csymId = CompactSymbolId.initSymId(symId),
                    .funcSigId = funcSigId,
                    .retType = bt.Any,
                    .typeChecked = false,
                };
            },
            .builtinType,
            .func => {
                // Match against exact signature.
                key = ResolvedFuncSymKey{
                    .resolvedFuncSymKey = .{
                        .symId = symId,
                        .funcSigId = funcSigId,
                    },
                };
                if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |funcSymId| {
                    const rFuncSym = chunk.compiler.sema.getFuncSym(funcSymId);
                    return FuncCallSymResult{
                        .csymId = CompactSymbolId.initFuncSymId(funcSymId),
                        .retType = rFuncSym.retType,
                        .funcSigId = funcSigId,
                        .typeChecked = true,
                    };
                }

                // Fallthrough. Still need to check the module that contains the func 
                // in case it's an overloaded function not yet resolved.
            },
            else => {
                const name = getName(chunk.compiler, nameId);
                return chunk.reportError("`{}` is not a callable symbol.", &.{v(name)});
            }
        }
    }

    if (rParentSymId != cy.NullId) {
        const parentSym = chunk.compiler.sema.getSymbol(rParentSymId);
        const modId = parentSym.getModuleId() orelse return null;

        if (try cy.module.findModuleSymForFuncCall(chunk, modId, nameId, args, ret, hasDynamicArg)) |modRes| {
            if (symId != cy.NullId) {
                key = ResolvedFuncSymKey{
                    .resolvedFuncSymKey = .{
                        .symId = symId,
                        .funcSigId = modRes.funcSigId,
                    },
                };
                if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |funcSymId| {
                    const rFuncSym = chunk.compiler.sema.getFuncSym(funcSymId);
                    return FuncCallSymResult{
                        .csymId = CompactSymbolId.initFuncSymId(funcSymId),
                        .retType = rFuncSym.retType,
                        .funcSigId = modRes.funcSigId,
                        .typeChecked = modRes.typeChecked,
                    };
                }
            }

            if (try resolveSymFromModule(chunk, modId, nameId, modRes.funcSigId)) |csymId| {
                if (csymId.isFuncSymId) {
                    const rFuncSym = chunk.compiler.sema.getFuncSym(csymId.id);
                    return FuncCallSymResult{
                        .csymId = csymId,
                        .retType = rFuncSym.retType,
                        .funcSigId = modRes.funcSigId,
                        .typeChecked = modRes.typeChecked,
                    };
                } else {
                    return FuncCallSymResult{
                        .csymId = csymId,
                        .retType = bt.Any,
                        .funcSigId = modRes.funcSigId,
                        .typeChecked = modRes.typeChecked,
                    };
                }
            } else {
                cy.panic("unexpected");
            }
        } else {
            if (rParentSymId != chunk.semaRootSymId) {
                try reportIncompatibleFuncSig(chunk, nameId, funcSigId, modId);
            }
        }
    }

    // Look for $call magic function.
    if (symId != cy.NullId) {
        const sym = chunk.compiler.sema.getSymbol(symId);
        if (sym.getModuleId()) |symModId| {
            const callNameId = try ensureNameSym(chunk.compiler, "$call");
            if (try cy.module.findModuleSymForFuncCall(chunk, symModId, callNameId, args, ret, hasDynamicArg)) |modRes| {
                // Check if already resolved.
                key = ResolvedSymKey{
                    .resolvedSymKey = .{
                        .parentSymId = symId,
                        .nameId = callNameId,
                    },
                };
                if (chunk.compiler.sema.resolvedSymMap.get(key)) |callSymId| {
                    key = ResolvedFuncSymKey{
                        .resolvedFuncSymKey = .{
                            .symId = callSymId,
                            .funcSigId = modRes.funcSigId,
                        },
                    };
                    if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |funcSymId| {
                        const rFuncSym = chunk.compiler.sema.getFuncSym(funcSymId);
                        return FuncCallSymResult{
                            .csymId = CompactSymbolId.initFuncSymId(funcSymId),
                            .retType = rFuncSym.retType,
                            .funcSigId = modRes.funcSigId,
                            .typeChecked = modRes.typeChecked,
                        };
                    }
                }

                if (try resolveSymFromModule(chunk, symModId, callNameId, modRes.funcSigId)) |csymId| {
                    std.debug.assert(csymId.isFuncSymId);

                    const rFuncSym = chunk.compiler.sema.getFuncSym(csymId.id);
                    return FuncCallSymResult{
                        .csymId = csymId,
                        .retType = rFuncSym.retType,
                        .funcSigId = modRes.funcSigId,
                        .typeChecked = modRes.typeChecked,
                    };
                } else {
                    cy.panic("unexpected");
                }
            }
        }
    }

    // Look in using modules.
    for (chunk.usingModules.items) |modId| {
        if (try cy.module.findModuleSymForFuncCall(chunk, modId, nameId, args, ret, hasDynamicArg)) |modRes| {
            const mod = chunk.compiler.sema.getModule(modId);
            // Cache to local syms.
            try setLocalSym(chunk, nameId, .{
                .symId = cy.NullId,
                .funcSymId = cy.NullId,
                .parentSymId = mod.resolvedRootSymId,
            });

            key = ResolvedSymKey.initResolvedSymKey(mod.resolvedRootSymId, nameId);
            symId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;
            if (symId != cy.NullId) {
                key = ResolvedFuncSymKey{
                    .resolvedFuncSymKey = .{
                        .symId = symId,
                        .funcSigId = modRes.funcSigId,
                    },
                };
                if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |funcSymId| {
                    const rFuncSym = chunk.compiler.sema.getFuncSym(funcSymId);
                    return FuncCallSymResult{
                        .csymId = CompactSymbolId.initFuncSymId(funcSymId),
                        .retType = rFuncSym.retType,
                        .funcSigId = modRes.funcSigId,
                        .typeChecked = modRes.typeChecked,
                    };
                }
            }

            if (try resolveSymFromModule(chunk, modId, nameId, modRes.funcSigId)) |csymId| {
                if (csymId.isFuncSymId) {
                    const rFuncSym = chunk.compiler.sema.getFuncSym(csymId.id);
                    return FuncCallSymResult{
                        .csymId = csymId,
                        .retType = rFuncSym.retType,
                        .funcSigId = modRes.funcSigId,
                        .typeChecked = modRes.typeChecked,
                    };
                } else {
                    return FuncCallSymResult{
                        .csymId = csymId,
                        .retType = bt.Any,
                        .funcSigId = modRes.funcSigId,
                        .typeChecked = modRes.typeChecked,
                    };
                }
            } else {
                cy.panic("unexpected");
            }
        }
    }

    if (rParentSymId != cy.NullId) {
        const parentSym = chunk.compiler.sema.getSymbol(rParentSymId);
        const modId = parentSym.getModuleId().?;
        try reportIncompatibleFuncSig(chunk, nameId, funcSigId, modId);
    }

    return null;
}

fn reportIncompatibleFuncSig(c: *cy.Chunk, nameId: NameSymId, funcSigId: FuncSigId, searchModId: cy.ModuleId) !void {
    const name = getName(c.compiler, nameId);
    const sigStr = try getFuncSigTempStr(c.compiler, funcSigId);

    const modKey = ModuleSymKey{
        .moduleSymKey = .{
            .nameId = nameId,
            .funcSigId = cy.NullId,
        },
    };
    const mod = c.compiler.sema.getModule(searchModId);
    if (mod.syms.get(modKey)) |modSym| {
        if (modSym.symT == .symToOneFunc) {
            const existingSigStr = try allocFuncSigStr(c.compiler, modSym.inner.symToOneFunc.funcSigId);
            defer c.alloc.free(existingSigStr);
            return c.reportError(
                \\Can not find compatible function signature for `{}{}`.
                \\Only `func {}{}` exists for the symbol `{}`.
            , &.{v(name), v(sigStr), v(name), v(existingSigStr), v(name)});
        } else if (modSym.symT == .symToManyFuncs) {
            return c.reportError(
                \\Can not find compatible function signature for `{}{}`.
                \\There are multiple overloaded functions named `{}`.
            , &.{v(name), v(sigStr), v(name)});
        }
    }
    return c.reportError(
        \\Can not find compatible function signature for `{}{}`.
        \\`{}` does not exist.
    , &.{v(name), v(sigStr), v(name)});
}

fn getSymbolRootMod(c: *cy.VMcompiler, id: SymbolId) cy.ModuleId {
    const rsym = c.sema.resolvedSyms.items[id];
    if (rsym.key.resolvedSymKey.parentSymId == cy.NullId) {
        return rsym.inner.module.id;
    } else {
        return getSymbolRootMod(c, rsym.key.resolvedSymKey.parentSymId);
    }
}

fn getAndCheckResolvedTypeSym(c: *cy.Chunk, key: ResolvedSymKey, nodeId: cy.NodeId) !?SymbolId {
    if (c.compiler.semaSymbolMap.get(key)) |id| {
        const rsym = c.compiler.sema.resolvedSyms.items[id];
        if (rsym.symT == .object) {
            return id;
        } else {
            return c.reportErrorAt("`{}` does not refer to a type.", &.{v(getName(c.compiler, key.resolvedSymKey.nameId))}, nodeId);
        }
    } else return null;
}

/// Get the resolved sym that matches a signature.
fn getAndCheckSymbolBySig(c: *cy.Chunk, key: ResolvedSymKey, funcSigId: FuncSigId, nodeId: cy.NodeId) !?SymbolId {
    if (c.compiler.semaSymbolMap.get(key)) |id| {
        const rsym = c.compiler.sema.resolvedSyms.items[id];
        if (funcSigId == cy.NullId) {
            // Searching for a non-func reference.
            if (rsym.symT == .func) {
                if (rsym.inner.func.funcSymId != cy.NullId) {
                    // When the signature is for a non-func reference,
                    // a non overloaded function symbol can be used.
                    return id;
                } else {
                    return c.reportErrorAt("Can not disambiguate the symbol `{}`.", &.{v(getName(c.compiler, key.resolvedSymKey.nameId))}, nodeId);
                }
            } else {
                return id;
            }
        } else {
            // Searching for function reference.
            if (rsym.symT == .variable) {
                // When the signature is for a func reference,
                // a variable symbol can be used.
                return id;
            } else if (rsym.symT == .func) {
                // Function signature must match exactly.
                const funcKey = ResolvedFuncSymKey{
                    .resolvedFuncSymKey = .{
                        .symId = id,
                        .funcSigId = funcSigId,
                    },
                };
                if (c.compiler.semaFuncSymMap.contains(funcKey)) {
                    return id;
                } else {
                    return null;
                }
            } else {
                return c.reportErrorAt("Can not use `{}` as a function reference.", &.{v(getName(c.compiler, key.resolvedSymKey.nameId))}, nodeId);
            }
        }
    } else return null;
}

fn resolveTypeSymFromModule(chunk: *cy.Chunk, modId: cy.ModuleId, nameId: NameSymId) anyerror!?SymbolId {
    const self = chunk.compiler;
    const relKey = ModuleSymKey{
        .moduleSymKey = .{
            .nameId = nameId,
            .funcSigId = cy.NullId,
        },
    };

    const mod = self.sema.modules.items[modId];
    if (mod.syms.get(relKey)) |modSym| {
        const key = ResolvedSymKey{
            .resolvedSymKey = .{
                .parentSymId = mod.resolvedRootSymId,
                .nameId = nameId,
            },
        };

        switch (modSym.symT) {
            .object => {
                const id = try resolveObjectSym(chunk.compiler, key, modSym.inner.object.modId);
                return id;
            },
            .userObject => {
                return chunk.reportError("Unsupported module sym: userObject", &.{});
            },
            .typeAlias => {
                const srcChunk = &chunk.compiler.chunks.items[mod.chunkId];
                const node = srcChunk.nodes[modSym.inner.typeAlias.declId];
                const symId = try getOrResolveTypeSymFromSpecNode(srcChunk, node.head.typeAliasDecl.typeSpecHead);
                return symId;
            },
            else => {},
        }
    }
    return null;
}

/// If the name symbol points to only one function, the function sym is returned.
fn resolveSymFromModule(chunk: *cy.Chunk, modId: cy.ModuleId, nameId: NameSymId, funcSigId: FuncSigId) anyerror!?CompactSymbolId {
    const self = chunk.compiler;
    const relKey = ModuleSymKey{
        .moduleSymKey = .{
            .nameId = nameId,
            .funcSigId = funcSigId,
        },
    };

    const mod = self.sema.modules.items[modId];
    if (mod.syms.get(relKey)) |modSym| {
        const key = ResolvedSymKey{
            .resolvedSymKey = .{
                .parentSymId = mod.resolvedRootSymId,
                .nameId = nameId,
            },
        };

        switch (modSym.symT) {
            .hostFunc => {
                const res = try resolveHostFunc(chunk, key, funcSigId, modSym.inner.hostFunc.func);
                return CompactSymbolId.initFuncSymId(res.funcSymId);
            },
            .variable => {
                const rtSymId = try self.vm.ensureVarSym(mod.resolvedRootSymId, nameId);
                const rtSym = rt.VarSym.init(modSym.inner.variable.val);
                cy.arc.retain(self.vm, rtSym.value);
                self.vm.setVarSym(rtSymId, rtSym);
                const id = try self.sema.addSymbol(key, .variable, .{
                    .variable = .{
                        .chunkId = cy.NullId,
                        .declId = cy.NullId,
                        .rTypeSymId = modSym.extra.variable.rTypeSymId,
                    },
                });
                return CompactSymbolId.initSymId(id);
            },
            .userVar => {
                _ = try self.vm.ensureVarSym(mod.resolvedRootSymId, nameId);

                const id = try self.sema.addSymbol(key, .variable, .{
                    .variable = .{
                        .chunkId = self.sema.modules.items[modId].chunkId,
                        .declId = modSym.inner.userVar.declId,
                        .rTypeSymId = bt.Any,
                    },
                });
                return CompactSymbolId.initSymId(id);
            },
            .userFunc => {
                const res = try resolveUserFunc(chunk, key, funcSigId,
                    modSym.inner.userFunc.declId, modSym.inner.userFunc.hasStaticInitializer);
                return CompactSymbolId.initFuncSymId(res.funcSymId);
            },
            .object => {
                const id = try resolveObjectSym(chunk.compiler, key, modSym.inner.object.modId);
                return CompactSymbolId.initSymId(id);
            },
            .enumType => {
                const id = try self.sema.addSymbol(key, .enumType, .{
                    .enumType = .{
                        .modId = modSym.inner.enumType.modId,
                        .enumId = modSym.inner.enumType.rtEnumId,
                    },
                });
                const enumMod = self.sema.getModulePtr(modSym.inner.enumType.modId);
                enumMod.resolvedRootSymId = id;
                return CompactSymbolId.initSymId(id);
            },
            .enumMember => {
                const id = try self.sema.addSymbol(key, .enumMember, .{
                    .enumMember = .{
                        .enumId = modSym.inner.enumMember.rtEnumId,
                        .memberId = modSym.inner.enumMember.memberId,
                    },
                });
                return CompactSymbolId.initSymId(id);
            },
            .typeAlias => {
                const srcChunk = &chunk.compiler.chunks.items[mod.chunkId];
                const node = srcChunk.nodes[modSym.inner.typeAlias.declId];
                const symId = try getOrResolveTypeSymFromSpecNode(srcChunk, node.head.typeAliasDecl.typeSpecHead);
                return CompactSymbolId.initSymId(symId);
            },
            .symToManyFuncs => {
                // More than one func for sym.
                const name = getName(chunk.compiler, nameId);
                return chunk.reportError("Symbol `{}` is ambiguous. There are multiple functions with the same name.", &.{v(name)});
            },
            .symToOneFunc => {
                const sigId = modSym.inner.symToOneFunc.funcSigId;
                return resolveSymFromModule(chunk, modId, nameId, sigId);
            },
            .userObject => {
                return chunk.reportError("Unsupported module sym: userObject", &.{});
            },
        }
    }
    return null;
}

pub fn ensureNameSym(c: *cy.VMcompiler, name: []const u8) !NameSymId {
    return ensureNameSymExt(c, name, false);
}

pub fn ensureNameSymExt(c: *cy.VMcompiler, name: []const u8, dupe: bool) !NameSymId {
    const res = try @call(.never_inline, std.StringHashMapUnmanaged(NameSymId).getOrPut, .{ &c.sema.nameSymMap, c.alloc, name});
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        const id: u32 = @intCast(c.sema.nameSyms.items.len);
        if (dupe) {
            const new = try c.alloc.dupe(u8, name);
            try c.sema.nameSyms.append(c.alloc, .{
                .ptr = new.ptr,
                .len = @intCast(new.len),
                .owned = true,
            });
            res.key_ptr.* = new;
        } else {
            try c.sema.nameSyms.append(c.alloc, .{
                .ptr = name.ptr,
                .len = @intCast(name.len),
                .owned = false,
            });
        }
        res.value_ptr.* = id;
        return id;
    }
}

/// TODO: This should also return true for local function symbols.
fn hasSymbol(self: *const cy.Chunk, parentSymId: SymbolId, nameId: NameSymId) bool {
    const key = ResolvedSymKey{
        .resolvedSymKey = .{
            .parentSymId = parentSymId,
            .nameId = nameId,
        },
    };
    return self.compiler.sema.resolvedSymMap.contains(key);
}

pub fn getVarName(c: *cy.Chunk, varId: LocalVarId) []const u8 {
    if (builtin.mode == .Debug) {
        return c.vars.items[varId].name;
    } else {
        return "";
    }
}

pub fn curSubBlock(self: *cy.Chunk) *SubBlock {
    return &self.semaSubBlocks.items[self.curSemaSubBlockId];
}

pub fn curBlock(self: *cy.Chunk) *Block {
    return &self.semaBlocks.items[self.curSemaBlockId];
}

pub fn endBlock(self: *cy.Chunk) !void {
    try endSubBlock(self);
    const sblock = curBlock(self);
    sblock.deinitTemps(self.alloc);
    self.semaBlockStack.items.len -= 1;
    self.curSemaBlockId = self.semaBlockStack.items[self.semaBlockStack.items.len-1];
}

pub fn getAccessExprResult(c: *cy.Chunk, ltype: TypeId, rightName: []const u8) !AccessExprResult {
    const sym = c.compiler.sema.getSymbol(ltype);
    if (sym.symT == .object) {
        const typeId = sym.inner.object.typeId;
        const rtFieldId = try c.compiler.vm.ensureFieldSym(rightName);
        var offset: u8 = undefined;
        const symMap = c.compiler.vm.fieldSyms.buf[rtFieldId];
        if (typeId == symMap.mruTypeId) {
            offset = @intCast(symMap.mruOffset);
        } else {
            offset = @call(.never_inline, cy.VM.getFieldOffsetFromTable, .{c.compiler.vm, typeId, rtFieldId});
        }
        if (offset == cy.NullU8) {
            const vmType = c.compiler.vm.types.buf[typeId];
            const name = vmType.namePtr[0..vmType.nameLen];
            return c.reportError("Missing field `{}` for type: {}", &.{v(rightName), v(name)});
        }
        return AccessExprResult{
            .recvT = ltype,
            .exprT = c.compiler.vm.fieldSyms.buf[rtFieldId].mruFieldTypeSymId,
        };
    }
    return AccessExprResult{
        .recvT = ltype,
        .exprT = bt.Any,
    };
}

const AccessExprResult = struct {
    recvT: TypeId,
    exprT: TypeId,
};

fn accessExpr(self: *cy.Chunk, nodeId: cy.NodeId) !AccessExprResult {
    const node = self.nodes[nodeId];
    const right = self.nodes[node.head.accessExpr.right];

    if (right.node_t == .ident) {
        var left = self.nodes[node.head.accessExpr.left];
        if (left.node_t == .ident) {
            const name = self.getNodeTokenString(left);
            const nameId = try ensureNameSym(self.compiler, name);
            const res = try getOrLookupVar(self, name, true);

            const rightName = self.getNodeTokenString(right);
            const rightNameId = try ensureNameSym(self.compiler, rightName);
            switch (res) {
                .local => |id| {
                    self.nodes[node.head.accessExpr.left].head.ident.semaVarId = id;
                    const svar = self.vars.items[id];
                    self.nodeTypes[node.head.accessExpr.left] = svar.vtype;
                    return getAccessExprResult(self, svar.vtype, rightName);
                },
                .static => {
                    // Static symbol.
                    const leftSymRes = try mustGetOrResolveDistinctSym(self, self.semaRootSymId, nameId);
                    const crLeftSym = leftSymRes.toCompactId();
                    try referenceSym(self, crLeftSym, true);
                    self.nodes[node.head.accessExpr.left].head.ident.sema_csymId = crLeftSym;

                    self.curNodeId = node.head.accessExpr.right;
                    if (try getOrResolveDistinctSym(self, leftSymRes.symId, rightNameId)) |symRes| {
                        const crRightSym = symRes.toCompactId();
                        try referenceSym(self, crRightSym, true);
                        self.nodes[nodeId].head.accessExpr.sema_csymId = crRightSym;

                        const exprT = try getTypeForResolvedValueSym(self, crRightSym);
                        return AccessExprResult{
                            .recvT = getSymType(self.compiler, crLeftSym.id),
                            .exprT = exprT,
                        };
                    } else {
                        const leftSym = self.compiler.sema.getSymbol(leftSymRes.symId);
                        if (leftSym.getModuleId() != null) {
                            // Report missing symbol when looking in a module.
                            return self.reportError("Missing symbol: `{}`", &.{v(rightName)});
                        }
                        if (leftSym.symT == .variable) {
                            const vtype = leftSym.inner.variable.rTypeSymId;
                            return getAccessExprResult(self, vtype, rightName);
                        }
                    }
                },
                .not_found => {
                    return self.reportErrorAt("Undeclared variable `{}`.", &.{v(name)}, node.head.accessExpr.left);
                },
            }
        } else if (left.node_t == .accessExpr) {
            const res = try accessExpr(self, node.head.accessExpr.left);

            left = self.nodes[node.head.accessExpr.left];
            if (left.head.accessExpr.sema_csymId.isPresent()) {
                // Static var.
                const crLeftSym = left.head.accessExpr.sema_csymId;
                if (!crLeftSym.isFuncSymId) {
                    const rightName = self.getNodeTokenString(right);
                    const rightNameId = try ensureNameSym(self.compiler, rightName);
                    if (try getOrResolveDistinctSym(self, crLeftSym.id, rightNameId)) |rightSym| {
                        const crRightSym = rightSym.toCompactId();
                        try referenceSym(self, crRightSym, true);
                        self.nodes[nodeId].head.accessExpr.sema_csymId = crRightSym;
                    }
                }
            } else {
                const rightName = self.getNodeTokenString(right);
                return getAccessExprResult(self, res.exprT, rightName);
            }
        } else {
            const recvT = try semaExpr(self, node.head.accessExpr.left);
            return AccessExprResult{
                .recvT = recvT,
                .exprT = bt.Any,
            };
        }
    } else {
        return self.reportError("Unsupported access expression with: {}", &.{v(right.node_t)});
    }
    return AccessExprResult{
        .recvT = bt.Any,
        .exprT = bt.Any,
    };
}

const VarResult = struct {
    id: LocalVarId,
    fromParentBlock: bool,
};

/// To a local type before assigning to a local variable.
fn toLocalType(vtype: TypeId) TypeId {
    return vtype;
}

fn replaceParamWithCopy(self: *cy.Chunk, sblock: *Block, name: []const u8, id: LocalVarId) !LocalVarId {
    _ = sblock.nameToVar.remove(name);
    const svar = self.vars.items[id];
    const newId: u32 = @intCast(self.vars.items.len);
    try self.vars.append(self.alloc, .{
        .type = .paramCopy,
        .name = if (builtin.mode == .Debug) name else {},
        .vtype = toLocalType(svar.vtype),
        .lifetimeRcCandidate = types.isRcCandidateType(self.compiler, svar.vtype),
        .isDefinedOnce = true,
        .inner = .{
            .paramCopy = svar.inner.paramIdx,
        },
    });
    try sblock.locals.append(self.alloc, newId);
    try sblock.nameToVar.put(self.alloc, name, newId);
    return newId;
}

fn assignVar(self: *cy.Chunk, ident: cy.NodeId, vtype: TypeId) !void {
    // log.debug("set var {s}", .{name});
    const node = self.nodes[ident];
    const name = self.getNodeTokenString(node);

    const res = try getOrLookupVar(self, name, true);
    switch (res) {
        .local => |id| {
            var svar = &self.vars.items[id];
            if (svar.isParentLocalAlias()) {
                if (!svar.isBoxed) {
                    // Becomes boxed so codegen knows ahead of time.
                    svar.isBoxed = true;
                }
            }

            if (svar.type == .param) {
                const sblock = curBlock(self);
                const newId = try replaceParamWithCopy(self, sblock, name, id);
                svar = &self.vars.items[newId];
            }

            const ssblock = curSubBlock(self);
            if (!ssblock.prevVarTypes.contains(id)) {
                // Same variable but branched to sub block.
                try ssblock.prevVarTypes.put(self.alloc, id, svar.vtype);
            }

            // Update current type after checking for branched assignment.
            if (!types.isSameType(svar.vtype, vtype)) {
                svar.vtype = toLocalType(vtype);
                if (!svar.lifetimeRcCandidate and types.isRcCandidateType(self.compiler, vtype)) {
                    svar.lifetimeRcCandidate = true;
                }
            }

            try self.assignedVarStack.append(self.alloc, id);
            self.nodes[ident].head.ident.semaVarId = id;
        },
        .static => {
            const nameId = try ensureNameSym(self.compiler, name);
            const symRes = try mustGetOrResolveDistinctSym(self, self.semaRootSymId, nameId);
            const csymId = symRes.toCompactId();
            try referenceSym(self, csymId, true);
            self.nodes[ident].head.ident.sema_csymId = csymId;
        },
        .not_found => {
            return self.reportErrorAt("Undeclared variable `{}`.", &.{v(name)}, ident);
        },
    }
}

fn endSubBlock(self: *cy.Chunk) !void {
    const sblock = curBlock(self);
    const ssblock = curSubBlock(self);

    const curAssignedVars = self.assignedVarStack.items[ssblock.assignedVarStart..];
    self.assignedVarStack.items.len = ssblock.assignedVarStart;

    if (sblock.subBlockDepth > 1) {
        const pssblock = self.semaSubBlocks.items[ssblock.prevSubBlockId];

        // Merge types to parent sub block.
        for (curAssignedVars) |varId| {
            const svar = &self.vars.items[varId];
            // log.debug("merging {s}", .{self.getVarName(varId)});
            if (ssblock.prevVarTypes.get(varId)) |prevt| {
                // Update current var type by merging.
                if (svar.vtype != prevt) {
                    svar.vtype = bt.Any;

                    // Previous sub block hasn't recorded the var assignment.
                    if (!pssblock.prevVarTypes.contains(varId)) {
                        try self.assignedVarStack.append(self.alloc, varId);
                    }

                    // Record merged type for codegen.
                    try ssblock.endMergeTypes.append(self.alloc, .{
                        .id = varId,
                        .vtype = svar.vtype,
                    });
                }
            } else {
                // New variable assignment, propagate to parent block.
                try self.assignedVarStack.append(self.alloc, varId);
            }
        }
    }
    ssblock.prevVarTypes.deinit(self.alloc);

    // Mark vars as undeclared.
    const curDeclaredVars = self.declaredVarStack.items[ssblock.declaredVarStart..];
    self.declaredVarStack.items.len = ssblock.declaredVarStart;
    for (curDeclaredVars) |varId| {
        const svar = &self.vars.items[varId];
        svar.isDeclared = false;
    }

    self.curSemaSubBlockId = ssblock.prevSubBlockId;
    sblock.subBlockDepth -= 1;
}

fn pushIterSubBlock(self: *cy.Chunk) !void {
    try pushSubBlock(self);
}

fn endIterSubBlock(self: *cy.Chunk) !void {
    const ssblock = curSubBlock(self);
    for (self.assignedVarStack.items[ssblock.assignedVarStart..]) |varId| {
        const svar = self.vars.items[varId];
        if (ssblock.prevVarTypes.get(varId)) |prevt| {
            if (svar.vtype != prevt) {
                // Type differs from prev scope type. Record change for iter block codegen.
                try ssblock.iterVarBeginTypes.append(self.alloc, .{
                    .id = varId,
                    .vtype = bt.Any,
                });
            }
        } else {
            // First assigned in iter block. Record change for iter block codegen.
            try ssblock.iterVarBeginTypes.append(self.alloc, .{
                .id = varId,
                .vtype = svar.vtype,
            });
        }
    }
    try endSubBlock(self);
}

pub fn declareUsingModule(chunk: *cy.Chunk, modId: cy.ModuleId) !void {
    try chunk.usingModules.append(chunk.alloc, modId);
}

pub fn importAllFromModule(self: *cy.Chunk, modId: cy.ModuleId) !void {
    const mod = self.compiler.sema.modules.items[modId];
    var iter = mod.syms.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*.moduleSymKey;
        const modSym = entry.value_ptr.*;
        switch (modSym.symT) {
            .variable,
            .symToManyFuncs,
            .symToOneFunc => {
                try setLocalSym(self, key.nameId, .{
                    .symId = cy.NullId,
                    .funcSymId = cy.NullId,
                    .parentSymId = mod.resolvedRootSymId,
                });
            },
            .hostFunc => {
                // Skip exact func syms.
            },
            else => {
                cy.panicFmt("Unsupported {}", .{modSym.symT});
            },
        }
    }
}

pub fn resolveEnumSym(c: *cy.VMcompiler, parentSymId: SymbolId, name: []const u8, modId: cy.ModuleId) !SymbolId {
    const nameId = try ensureNameSym(c, name);
    const key = ResolvedSymKey{
        .resolvedSymKey = .{
            .parentSymId = parentSymId,
            .nameId = nameId,
        },
    };
    if (c.sema.resolvedSymMap.contains(key)) {
        return error.DuplicateSymName;
    }

    // Resolve the symbol.
    const symId: u32 = @intCast(c.sema.resolvedSyms.items.len);
    try c.sema.resolvedSyms.append(c.alloc, .{
        .symT = .enumT,
        .key = key,
        .inner = .{
            .enumT = .{
                .modId = modId,
            },
        },
        .exported = true,
    });
    try @call(.never_inline, c.sema.resolvedSymMap.put, .{c.alloc, key, symId});

    return symId;
}

pub fn resolveObjectSym(c: *cy.VMcompiler, key: ResolvedSymKey, modId: cy.ModuleId) !SymbolId {
    const rtTypeId = try c.vm.ensureObjectType(key.resolvedSymKey.parentSymId, key.resolvedSymKey.nameId, cy.NullId);
    const symId = try c.sema.addSymbol(key, .object, .{
        .object = .{
            .modId = modId,
            .typeId = rtTypeId,
        },
    });
    try @call(.never_inline, @TypeOf(c.sema.resolvedSymMap).put, .{&c.sema.resolvedSymMap, c.alloc, key, symId});

    c.vm.types.buf[rtTypeId].semaTypeId = symId;
    const mod = c.sema.getModulePtr(modId);
    mod.resolvedRootSymId = symId;
    return symId;
}

/// A root module symbol is used as the parent for it's members.
pub fn resolveRootModuleSym(self: *cy.VMcompiler, name: []const u8, modId: cy.ModuleId) !SymbolId {
    const nameId = try ensureNameSym(self, name);
    const key = ResolvedSymKey{
        .resolvedSymKey = .{
            .parentSymId = cy.NullId,
            .nameId = nameId,
        },
    };
    if (self.sema.resolvedSymMap.contains(key)) {
        // Assume no existing symbol, since each module has a unique srcUri.
        log.debug("Root symbol {s} already exists.", .{name});
        cy.fatal();
    }

    // Resolve the symbol.
    const resolvedId: u32 = @intCast(self.sema.resolvedSyms.items.len);
    try self.sema.resolvedSyms.append(self.alloc, .{
        .symT = .module,
        .key = key,
        .inner = .{
            .module = .{
                .id = modId,
            },
        },
        .exported = true,
    });
    try @call(.never_inline, @TypeOf(self.sema.resolvedSymMap).put, .{&self.sema.resolvedSymMap, self.alloc, key, resolvedId});

    return resolvedId;
}

/// Given the local sym path, add a resolved var sym entry.
/// Fail if there is already a symbol in this path with the same name.
fn resolveLocalVarSym(self: *cy.Chunk, parentSymId: SymbolId, nameId: NameSymId, typeSymId: SymbolId, declId: cy.NodeId, exported: bool) !SymbolId {
    if (parentSymId == self.semaRootSymId) {
        // Check for local sym.
        const key = LocalSymKey.initLocalSymKey(nameId, null);
        if (self.localSyms.contains(key)) {
            const node = self.nodes[declId];
            const varSpec = self.nodes[node.head.staticDecl.varSpec];
            return self.reportErrorAt("The symbol `{}` was already declared.", &.{v(getName(self.compiler, nameId))}, varSpec.head.varSpec.name);
        }
    }

    const key = ResolvedSymKey{
        .resolvedSymKey = .{
            .parentSymId = parentSymId,
            .nameId = nameId,
        },
    };

    if (self.compiler.sema.resolvedSymMap.contains(key)) {
        return self.reportErrorAt("The symbol `{}` was already declared.", &.{v(getName(self.compiler, nameId))}, declId);
    }

    // Resolve the symbol.
    const resolvedId: u32 = @intCast(self.compiler.sema.resolvedSyms.items.len);
    try self.compiler.sema.resolvedSyms.append(self.alloc, .{
        .symT = .variable,
        .key = key,
        .inner = .{
            .variable = .{
                .chunkId = self.id,
                .declId = declId,
                .rTypeSymId = typeSymId,
            },
        },
        .exported = exported,
    });

    try @call(.never_inline, @TypeOf(self.compiler.sema.resolvedSymMap).put, .{&self.compiler.sema.resolvedSymMap, self.alloc, key, resolvedId});

    return resolvedId;
}

pub fn getSymType(c: *cy.VMcompiler, id: SymbolId) TypeId {
    const sym = c.sema.getSymbol(id);
    switch (sym.symT) {
        .variable => {
            return sym.inner.variable.rTypeSymId;
        },
        .enumType,
        .module => {
            return bt.Any;
        },
        else => {
            cy.panicFmt("Unsupported sym: {}", .{sym.symT});
        }
    }
}

pub fn getSymName(c: *cy.VMcompiler, id: SymbolId) []const u8 {
    const sym = c.sema.getSymbol(id);
    return getName(c, sym.key.resolvedSymKey.nameId);
}

/// Dump the full path of a resolved sym.
fn allocAbsSymbolName(self: *cy.VMcompiler, id: SymbolId) ![]const u8 {
    const sym = self.sema.resolvedSyms.items[id];
    if (sym.key.resolvedSymKey.parentSymId != cy.NullId) {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(self.alloc);
        try allocAbsSymbolNameR(self, &buf, sym.key.resolvedSymKey.parentSymId);
        try buf.append(self.alloc, '.');
        try buf.appendSlice(self.alloc, getName(self, sym.key.resolvedSymKey.nameId));
        return buf.toOwnedSlice(self.alloc);
    } else {
        const name = getName(self, sym.key.resolvedSymKey.nameId);
        return self.alloc.dupe(u8, name);
    }
}

fn allocAbsSymbolNameR(self: *cy.VMcompiler, buf: *std.ArrayListUnmanaged(u8), id: SymbolId) !void {
    const sym = self.sema.resolvedSyms.items[id];
    if (sym.key.resolvedSymKey.parentSymId == cy.NullId) {
        try buf.appendSlice(self.alloc, getName(self, sym.key.resolvedSymKey.nameId));
    } else {
        try allocAbsSymbolNameR(self, buf, sym.key.resolvedSymKey.parentSymId);
        try buf.append(self.alloc, '.');
        try buf.appendSlice(self.alloc, getName(self, sym.key.resolvedSymKey.nameId));
    }
}

const ResolveFuncSymResult = struct {
    symId: SymbolId,
    funcSymId: FuncSymId,
};

const SymbolResult = struct {
    symId: SymbolId,
    funcSymId: Nullable(FuncSymId),

    fn toCompactId(self: SymbolResult) CompactSymbolId {
        if (self.funcSymId != cy.NullId) {
            return CompactSymbolId.initFuncSymId(self.funcSymId);
        } else {
            return CompactSymbolId.initSymId(self.symId);
        }
    }
};

fn resolveHostFunc(
    c: *cy.Chunk, key: ResolvedSymKey, funcSigId: FuncSigId, func: vmc.HostFuncFn,
) !SymbolResult {
    const parentSymId = key.resolvedSymKey.parentSymId;
    const nameId = key.resolvedSymKey.nameId;
    const rtSymId = try c.compiler.vm.ensureFuncSym(parentSymId, nameId, funcSigId);

    const funcSig = c.compiler.sema.getFuncSig(funcSigId);
    const rtSym = rt.FuncSymbolEntry.initNativeFunc1(func, funcSig.isTyped, funcSig.numParams(), funcSigId);
    c.compiler.vm.setFuncSym(rtSymId, rtSym);
    return try resolveFunc(c, key, funcSigId, cy.NullId);
}

fn resolveUserFunc(
    c: *cy.Chunk, key: ResolvedSymKey, funcSigId: FuncSigId, declId: FuncDeclId, hasStaticInitializer: bool,
) !SymbolResult {
    // Func sym entry will be updated when the func is generated later.
    const parentSymId = key.resolvedSymKey.parentSymId;
    const nameId = key.resolvedSymKey.nameId;
    _ = try c.compiler.vm.ensureFuncSym(parentSymId, nameId, funcSigId);

    const res = try resolveFunc(c, key, funcSigId, declId);
    if (hasStaticInitializer) {
        c.compiler.sema.resolvedFuncSyms.items[res.funcSymId].hasStaticInitializer = true;
    }
    const nodeId = c.semaFuncDecls.items[declId].nodeId;
    const func = c.getNodeFuncDeclPtr(nodeId);
    func.inner.staticFunc = .{
        .semaFuncSymId = res.funcSymId,
        .semaSymId = res.symId,
    };
    return res;
}

fn resolveFunc(
    self: *cy.Chunk, key: ResolvedSymKey, funcSigId: FuncSigId, declId: FuncDeclId,
) !SymbolResult {
    const c = self.compiler;
    var rsymId: SymbolId = undefined;
    var createdSym = false;
    if (c.sema.resolvedSymMap.get(key)) |id| {
        const rsym = c.sema.resolvedSyms.items[id];
        if (rsym.symT != .func) {
            // Only fail if the symbol already exists and isn't a function.
            const name = getName(c, key.resolvedSymKey.nameId);
            return self.reportError("The symbol `{}` was already declared.", &.{v(name)});
        }
        rsymId = id;
    } else {
        rsymId = @intCast(c.sema.resolvedSyms.items.len);
        try c.sema.resolvedSyms.append(c.alloc, .{
            .symT = .func,
            .key = key,
            .inner = .{
                .func = .{
                    .funcSymId = undefined,
                },
            },
            .exported = true,
        });
        try @call(.never_inline, @TypeOf(c.sema.resolvedSymMap).put, .{&c.sema.resolvedSymMap, c.alloc, key, rsymId});
        createdSym = true;
    }

    // Now check resolved function syms.
    const funcKey = ResolvedFuncSymKey{
        .resolvedFuncSymKey = .{
            .symId = rsymId,
            .funcSigId = funcSigId,
        },
    };
    if (c.sema.resolvedFuncSymMap.contains(funcKey)) {
        const name = getName(c, key.resolvedSymKey.nameId);
        return self.reportError("The function symbol `{}` with the same signature was already declared.", &.{v(name)});
    }

    const rFuncSig = c.sema.getFuncSig(funcSigId);
    const retSymId = rFuncSig.getRetTypeSymId();

    const rfsymId: u32 = @intCast(c.sema.resolvedFuncSyms.items.len);
    try types.assertTypeSym(self, retSymId);
    try c.sema.resolvedFuncSyms.append(c.alloc, .{
        .chunkId = self.id,
        .declId = declId,
        .key = funcKey,
        .retType = retSymId,
        .hasStaticInitializer = false,
    });
    try @call(.never_inline, @TypeOf(c.sema.resolvedFuncSymMap).put, .{&c.sema.resolvedFuncSymMap, c.alloc, funcKey, rfsymId});

    if (createdSym) {
        c.sema.resolvedSyms.items[rsymId].inner.func.funcSymId = rfsymId;
    } else {
        // Mark sym as overloaded.
        c.sema.resolvedSyms.items[rsymId].inner.func.funcSymId = cy.NullId;
    }

    if (builtin.mode == .Debug and cy.verbose) {
        const name = try allocAbsSymbolName(c, rsymId);
        defer self.alloc.free(name);
        const sigStr = try getFuncSigTempStr(self.compiler, funcSigId);
        log.debug("resolved static func: func {s}{s}", .{name, sigStr});
    }

    return SymbolResult{
        .symId = rsymId,
        .funcSymId = rfsymId,
    };
}

fn endFuncBlock(self: *cy.Chunk) !void {
    const sblock = curBlock(self);
    if (sblock.captures.items.len > 0) {
        for (sblock.captures.items) |varId| {
            const pId = self.capVarDescs.get(varId).?.user;
            const pvar = &self.vars.items[pId];
            if (!pvar.isBoxed) {
                pvar.isBoxed = true;
                pvar.lifetimeRcCandidate = true;
            }
        }
    }
    try endBlock(self);
}

fn endFuncSymBlock(self: *cy.Chunk, numParams: u32) !void {
    const sblock = curBlock(self);
    const numCaptured: u8 = @intCast(sblock.params.items.len - numParams);
    if (builtin.mode == .Debug and numCaptured > 0) {
        cy.panicFmt("Captured var in static func.", .{});
    }
    try endBlock(self);
}

const PushCallArgsResult = struct {
    argTypes: []const TypeId,
    hasDynamicArg: bool,
};

fn pushCallArgsWithPreferredTypes(c: *cy.Chunk, argHead: cy.NodeId, preferredTypes: []const types.TypeId) !PushCallArgsResult {
    const start = c.compiler.typeStack.items.len;
    var nodeId = argHead;
    var hasDynamicArg = false;
    var i: u32 = 0;
    while (nodeId != cy.NullId) {
        const arg = c.nodes[nodeId];
        var preferredT = bt.Any;
        if (i < preferredTypes.len) {
            preferredT = preferredTypes[i];
            i += 1;
        }
        const argT = try semaExprType(c, nodeId, preferredT);
        hasDynamicArg = hasDynamicArg or (argT == bt.Dynamic);
        try c.compiler.typeStack.append(c.alloc, argT);
        nodeId = arg.next;
    }
    return PushCallArgsResult{
        .argTypes = c.compiler.typeStack.items[start..],
        .hasDynamicArg = hasDynamicArg,
    };
}

fn pushCallArgs(c: *cy.Chunk, argHead: cy.NodeId) !PushCallArgsResult {
    const start = c.compiler.typeStack.items.len;
    var nodeId = argHead;
    var hasDynamicArg = false;
    while (nodeId != cy.NullId) {
        const arg = c.nodes[nodeId];
        const argT = try semaExpr(c, nodeId);
        hasDynamicArg = hasDynamicArg or (argT == bt.Dynamic);
        try c.compiler.typeStack.append(c.alloc, argT);
        nodeId = arg.next;
    }
    return PushCallArgsResult{
        .argTypes = c.compiler.typeStack.items[start..],
        .hasDynamicArg = hasDynamicArg,
    };
}

pub const FuncSigId = u32;
pub const FuncSig = struct {
    /// Last elem is the return type sym.
    paramPtr: [*]const types.TypeId,
    retSymId: types.TypeId,
    paramLen: u16,

    /// If a param or the return type is not the any type.
    isTyped: bool,

    /// If a param is not the any type.
    isParamsTyped: bool,

    pub inline fn params(self: FuncSig) []const types.TypeId {
        return self.paramPtr[0..self.paramLen];
    }

    pub inline fn numParams(self: FuncSig) u8 {
        return @intCast(self.paramLen);
    }

    pub inline fn getRetTypeSymId(self: FuncSig) types.TypeId {
        return self.retSymId;
    }

    pub fn deinit(self: *FuncSig, alloc: std.mem.Allocator) void {
        alloc.free(self.params());
    }
};

pub const NameAny = 0;
pub const NameBoolean = 1;
pub const NameFloat = 2;
pub const NameInt = 3;
pub const NameString = 4;
pub const NameRawstring = 5;
pub const NameSymbol = 6;
pub const NameList = 7;
pub const NameMap = 8;
pub const NamePointer = 9;
pub const NameNone = 10;
pub const NameError = 11;
pub const NameFiber = 12;
pub const NameMetatype = 13;
pub const NameBuiltinTypeEnd = 14;

pub const FuncDeclId = u32;

pub const FuncDecl = struct {
    nodeId: cy.NodeId,

    paramHead: Nullable(cy.NodeId),

    /// Resolved func signature.
    funcSigId: FuncSigId,

    /// Resolved return type sym. NullId indicates no declaration.
    rRetTypeSymId: Nullable(SymbolId),

    /// Sema block is attached to func decl so it can be accessed from anywhere with the node id.
    /// If the func decl has an initializer then this is repurposed to point to the decl's node id.
    semaBlockId: u32 = cy.NullId,

    inner: extern union {
        staticFunc: extern struct {
            semaSymId: u32 = cy.NullId,

            /// Used by funcDeclInit to generate static initializer dependencies.
            semaFuncSymId: u32 = cy.NullId,
        },
        lambda: extern struct {
            funcSigId: u32 = cy.NullId,
        },
    },

    /// pc to start of end locals procedure.
    genEndLocalsPc: u32 = cy.NullId,

    /// Number of params in the function signature.
    numParams: u8,

    /// Whether this is a static function.
    isStatic: bool,

    pub fn hasReturnTypeSpec(self: *const FuncDecl) bool {
        return self.rRetTypeSymId != cy.NullId;
    }

    fn getReturnType(self: *const FuncDecl) !TypeId {
        if (!self.hasReturnTypeSpec()) {
            return bt.Any;
        } else {
            return self.rRetTypeSymId;
        }
    }

    pub fn getReturnNode(self: *const FuncDecl, chunk: *const cy.Chunk) cy.NodeId {
        const node = chunk.nodes[self.nodeId];
        const header = chunk.nodes[node.head.func.header];
        return header.head.funcHeader.ret;
    }

    fn getNameNode(self: *const FuncDecl, chunk: *const cy.Chunk) cy.NodeId {
        const node = chunk.nodes[self.nodeId];
        const header = chunk.nodes[node.head.func.header];
        return header.head.funcHeader.name;
    }

    pub fn getName(self: *const FuncDecl, chunk: *const cy.Chunk) []const u8 {
        const node = chunk.nodes[self.nodeId];
        const header = chunk.nodes[node.head.func.header];
        const name = header.head.funcHeader.name;
        if (name == cy.NullId) {
            return "";
        } else {
            return chunk.getNodeTokenString(chunk.nodes[name]);
        }
    }

    pub fn getNameFromParser(self: *const FuncDecl, parser: *const cy.Parser) []const u8 {
        const node = parser.nodes.items[self.nodeId];
        const header = parser.nodes.items[node.head.func.header];
        const name = header.head.funcHeader.name;
        if (name == cy.NullId) {
            return "";
        } else {
            const nameN = parser.nodes.items[name];
            const token = parser.tokens.items[nameN.start_token];
            return parser.src[token.pos()..token.data.end_pos];
        }
    }
};

const FuncSigKey = struct {
    paramPtr: [*]const SymbolId,
    paramLen: u32,
    retSymId: SymbolId,
};

pub const Model = struct {
    alloc: std.mem.Allocator,

    /// Unique name syms.
    nameSyms: std.ArrayListUnmanaged(Name),
    nameSymMap: std.StringHashMapUnmanaged(NameSymId),

    /// Resolved symbols are shared among all modules.
    /// Only resolved symbols are included in the execution runtime.
    /// When a symbol is missing, sema will attempt to find it within the resolved parent module.
    /// Each symbol is keyed by the absolute path to the symbol.
    resolvedSyms: std.ArrayListUnmanaged(Symbol),
    resolvedSymMap: std.HashMapUnmanaged(ResolvedSymKey, SymbolId, cy.hash.KeyU64Context, 80),

    /// Resolved function symbols that are included in the runtime.
    /// Each func symbol is keyed by the resolved sym and function signature.
    resolvedFuncSyms: std.ArrayListUnmanaged(FuncSym),
    resolvedFuncSymMap: std.HashMapUnmanaged(ResolvedFuncSymKey, FuncSymId, cy.hash.KeyU64Context, 80),

    /// Resolved signatures for functions.
    resolvedFuncSigs: std.ArrayListUnmanaged(FuncSig),
    resolvedFuncSigMap: std.HashMapUnmanaged(FuncSigKey, FuncSigId, FuncSigKeyContext, 80),

    /// Fast index to untyped func sig id by num params.
    /// `NullId` indicates a missing func sig id.
    /// TODO: If Cyber implements multiple return values, this would need to be a map.
    resolvedUntypedFuncSigs: std.ArrayListUnmanaged(FuncSigId),

    /// Modules.
    modules: std.ArrayListUnmanaged(cy.Module),
    /// Owned absolute specifier path to module.
    moduleMap: std.StringHashMapUnmanaged(cy.ModuleId),

    objectMembers: std.HashMapUnmanaged(ObjectMemberKey, void, cy.hash.KeyU64Context, 80),

    pub fn init(alloc: std.mem.Allocator) Model {
        return .{
            .alloc = alloc,
            .nameSyms = .{},
            .nameSymMap = .{},
            .resolvedSyms = .{},
            .resolvedSymMap = .{},
            .resolvedFuncSyms = .{},
            .resolvedFuncSymMap = .{},
            .resolvedFuncSigs = .{},
            .resolvedFuncSigMap = .{},
            .resolvedUntypedFuncSigs = .{},
            .modules = .{},
            .moduleMap = .{},
            .objectMembers = .{},
        };
    }

    pub fn deinit(self: *Model, alloc: std.mem.Allocator, comptime reset: bool) void {
        if (reset) {
            self.resolvedSyms.clearRetainingCapacity();
            self.resolvedSymMap.clearRetainingCapacity();
            self.resolvedFuncSyms.clearRetainingCapacity();
            self.resolvedFuncSymMap.clearRetainingCapacity();
            self.objectMembers.clearRetainingCapacity();
        } else {
            self.resolvedSyms.deinit(alloc);
            self.resolvedSymMap.deinit(alloc);
            self.resolvedFuncSyms.deinit(alloc);
            self.resolvedFuncSymMap.deinit(alloc);
            self.objectMembers.deinit(alloc);
        }

        for (self.modules.items) |*mod| {
            mod.deinit(alloc);
        }
        if (reset) {
            self.modules.clearRetainingCapacity();
            self.moduleMap.clearRetainingCapacity();
        } else {
            self.modules.deinit(alloc);
            self.moduleMap.deinit(alloc);
        }

        for (self.nameSyms.items) |name| {
            if (name.owned) {
                alloc.free(name.getName());
            }
        }
        if (reset) {
            self.nameSyms.clearRetainingCapacity();
            self.nameSymMap.clearRetainingCapacity();
        } else {
            self.nameSyms.deinit(alloc);
            self.nameSymMap.deinit(alloc);
        }

        for (self.resolvedFuncSigs.items) |*it| {
            it.deinit(alloc);
        }
        if (reset) {
            self.resolvedFuncSigs.clearRetainingCapacity();
            self.resolvedFuncSigMap.clearRetainingCapacity();
            self.resolvedUntypedFuncSigs.clearRetainingCapacity();
        } else {
            self.resolvedFuncSigs.deinit(alloc);
            self.resolvedFuncSigMap.deinit(alloc);
            self.resolvedUntypedFuncSigs.deinit(alloc);
        }
    }

    pub inline fn getSymbol(self: *Model, id: SymbolId) Symbol {
        return self.resolvedSyms.items[id];
    }

    pub fn addSymbol(self: *Model, key: ResolvedSymKey, symT: SymbolType, data: SymbolData) !SymbolId {
        const id: u32 = @intCast(self.resolvedSyms.items.len);
        try self.resolvedSyms.append(self.alloc, .{
            .symT = symT,
            .key = key,
            .inner = data,
            .exported = true,
        });
        try self.resolvedSymMap.put(self.alloc, key, id);
        return id;
    }

    pub inline fn getSymbolPtr(self: *Model, id: SymbolId) *Symbol {
        return &self.resolvedSyms.items[id];
    }

    pub inline fn getFuncSym(self: *Model, id: FuncSymId) FuncSym {
        return self.resolvedFuncSyms.items[id];
    }

    pub inline fn getFuncSymPtr(self: *Model, id: FuncSymId) *FuncSym {
        return &self.resolvedFuncSyms.items[id];
    }

    pub inline fn getFuncSig(self: *Model, id: FuncSigId) FuncSig {
        return self.resolvedFuncSigs.items[id];
    }

    pub inline fn getModule(self: *Model, id: cy.ModuleId) cy.Module {
        return self.modules.items[id];
    }

    pub inline fn getModulePtr(self: *Model, id: cy.ModuleId) *cy.Module {
        return &self.modules.items[id];
    }
};

pub const FuncSigKeyContext = struct {
    pub fn hash(_: @This(), key: FuncSigKey) u64 {
        var c = std.hash.Wyhash.init(0);
        const bytes: [*]const u8 = @ptrCast(key.paramPtr);
        c.update(bytes[0..key.paramLen*4]);
        c.update(std.mem.asBytes(&key.retSymId));
        return c.final();
    }
    pub fn eql(_: @This(), a: FuncSigKey, b: FuncSigKey) bool {
        return std.mem.eql(u32, a.paramPtr[0..a.paramLen], b.paramPtr[0..b.paramLen]);
    }
};

pub const U32SliceContext = struct {
    pub fn hash(_: @This(), key: []const u32) u64 {
        var c = std.hash.Wyhash.init(0);
        const bytes: [*]const u8 = @ptrCast(key.ptr);
        c.update(bytes[0..key.len*4]);
        return c.final();
    }
    pub fn eql(_: @This(), a: []const u32, b: []const u32) bool {
        return std.mem.eql(u32, a, b);
    }
};

/// `buf` is assumed to be big enough.
pub fn unescapeString(buf: []u8, literal: []const u8) []const u8 {
    var newIdx: u32 = 0; 
    var i: u32 = 0;
    while (i < literal.len) : (newIdx += 1) {
        if (literal[i] == '\\') {
            if (unescapeAsciiChar(literal[i + 1])) |ch| {
                buf[newIdx] = ch;
            } else {
                buf[newIdx] = literal[i + 1];
            }
            i += 2;
        } else {
            buf[newIdx] = literal[i];
            i += 1;
        }
    }
    return buf[0..newIdx];
}

pub fn unescapeAsciiChar(ch: u8) ?u8 {
    switch (ch) {
        'a' => {
            return 0x07;
        },
        'b' => {
            return 0x08;
        },
        'e' => {
            return 0x1b;
        },
        'n' => {
            return '\n';
        },
        'r' => {
            return '\r';
        },
        't' => {
            return '\t';
        },
        else => {
            return null;
        }
    }
}

pub fn appendResolvedRootModule(c: *cy.VMcompiler, absSpec: []const u8) !cy.ModuleId {
    const modId = try cy.module.appendModule(c, absSpec);
    const mod = c.sema.getModulePtr(modId);
    const rModSymId = try resolveRootModuleSym(c, mod.absSpec, modId);
    mod.resolvedRootSymId = rModSymId;
    return modId;
}

test "sema internals." {
    if (builtin.mode == .Debug) {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(LocalVar), 24);
        } else {
            try t.eq(@sizeOf(LocalVar), 32);
        }
    } else {
        try t.eq(@sizeOf(LocalVar), 16);
    }
    try t.eq(@sizeOf(FuncSym), 24);

    if (cy.is32Bit) {
        try t.eq(@sizeOf(FuncSig), 12);
    } else {
        try t.eq(@sizeOf(FuncSig), 16);
    }

    try t.eq(@sizeOf(Symbol), 24);
    try t.eq(@offsetOf(Symbol, "symT"), @offsetOf(vmc.Symbol, "symT"));
    try t.eq(@offsetOf(Symbol, "key"), @offsetOf(vmc.Symbol, "key"));
    try t.eq(@offsetOf(Symbol, "inner"), @offsetOf(vmc.Symbol, "data"));
    try t.eq(@offsetOf(Symbol, "exported"), @offsetOf(vmc.Symbol, "exported"));
    try t.eq(@offsetOf(Symbol, "genStaticInitVisited"), @offsetOf(vmc.Symbol, "genStaticInitVisited"));

    try t.eq(@sizeOf(SymbolData), 12);

    if (cy.is32Bit) {
        try t.eq(@sizeOf(Name), 12);
    } else {
        try t.eq(@sizeOf(Name), 16);
    }
    try t.eq(@offsetOf(Name, "ptr"), @offsetOf(vmc.Name, "ptr"));
    try t.eq(@offsetOf(Name, "len"), @offsetOf(vmc.Name, "len"));
    try t.eq(@offsetOf(Name, "owned"), @offsetOf(vmc.Name, "owned"));

    try t.eq(@sizeOf(CompactSymbolId), 4);

    try t.eq(@sizeOf(CapVarDesc), 4);

    try t.eq(@offsetOf(Model, "alloc"), @offsetOf(vmc.SemaModel, "alloc"));
    try t.eq(@offsetOf(Model, "nameSyms"), @offsetOf(vmc.SemaModel, "nameSyms"));
    try t.eq(@offsetOf(Model, "nameSymMap"), @offsetOf(vmc.SemaModel, "nameSymMap"));
    try t.eq(@offsetOf(Model, "resolvedSyms"), @offsetOf(vmc.SemaModel, "resolvedSyms"));
    try t.eq(@offsetOf(Model, "resolvedSymMap"), @offsetOf(vmc.SemaModel, "resolvedSymMap"));
    try t.eq(@offsetOf(Model, "resolvedFuncSyms"), @offsetOf(vmc.SemaModel, "resolvedFuncSyms"));
    try t.eq(@offsetOf(Model, "resolvedFuncSymMap"), @offsetOf(vmc.SemaModel, "resolvedFuncSymMap"));
    try t.eq(@offsetOf(Model, "resolvedFuncSigs"), @offsetOf(vmc.SemaModel, "resolvedFuncSigs"));
    try t.eq(@offsetOf(Model, "resolvedFuncSigMap"), @offsetOf(vmc.SemaModel, "resolvedFuncSigMap"));
    try t.eq(@offsetOf(Model, "resolvedUntypedFuncSigs"), @offsetOf(vmc.SemaModel, "resolvedUntypedFuncSigs"));
    try t.eq(@offsetOf(Model, "modules"), @offsetOf(vmc.SemaModel, "modules"));
    try t.eq(@offsetOf(Model, "moduleMap"), @offsetOf(vmc.SemaModel, "moduleMap"));
    try t.eq(@offsetOf(Model, "objectMembers"), @offsetOf(vmc.SemaModel, "objectMembers"));
}
