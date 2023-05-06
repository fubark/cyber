const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const rt = cy.rt;
const types = cy.types;
const bt = types.BuiltinTypeSymIds;
const Nullable = cy.Nullable;
const fmt = cy.fmt;
const v = fmt.v;

const TypeId = types.TypeId;

const vm_ = @import("vm.zig");

const log = stdx.log.scoped(.sema);

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

/// Represents a variable in a block.
/// If the variable was declared `static`, references to it will use a static symbol instead.
/// Other variables are given reserved registers on the stack frame.
/// Captured variables have box values at runtime.
pub const LocalVar = struct {
    /// The current type of the var as the ast is traversed.
    /// This is updated when there is a variable assignment or a child block returns.
    vtype: TypeId,

    /// If non-null, points to the captured var idx in the closure.
    capturedIdx: u8 = cy.NullU8,

    /// Whether this var references a static variable.
    isStaticAlias: bool = false,

    /// Whether the variable references a captured/static var from a modifier or implicity from a read reference.
    hasCaptureOrStaticModifier: bool = false,

    /// Currently a captured var always needs to be boxed.
    /// In the future, the concept of a const variable could change this.
    isBoxed: bool = false,

    /// Whether this is a function param. (Does not include captured vars.)
    isParam: bool = false,

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
    genIsDefined: bool = false,

    inner: extern union {
        staticAlias: extern struct {
            crSymId: CompactResolvedSymId,
        }
    } = undefined,

    name: if (builtin.mode == .Debug) []const u8 else void,

    pub inline fn isCaptured(self: LocalVar) bool {
        return self.capturedIdx != cy.NullU8;
    }
};

pub const CapVarDesc = packed union {
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

    /// Previous sema sub block.
    /// When this sub block ends, the previous sub block id is set as the current.
    prevSubBlockId: SubBlockId,

    pub fn init(prevSubBlockId: SubBlockId, assignedVarStart: usize) SubBlock {
        return .{
            .assignedVarStart = @intCast(u32, assignedVarStart),
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
pub fn symHasStaticInitializer(c: *const cy.Chunk, crSymId: CompactResolvedSymId) bool {
    if (crSymId.isFuncSymId) {
        return c.compiler.sema.getResolvedFuncSym(crSymId.id).hasStaticInitializer;
    } else {
        const rsym = c.compiler.sema.getResolvedSym(crSymId.id);
        if (rsym.symT == .variable) {
            return rsym.inner.variable.declId != cy.NullId;
        }
    }
    return false;
}

pub const ResolvedFuncSymId = u32;
pub const ResolvedFuncSym = struct {
    chunkId: ChunkId,
    
    /// Can be the cy.NullId for native functions.
    declId: FuncDeclId,

    /// Access to rSymId, rFuncSigId.
    key: AbsResolvedFuncSymKey,

    /// Return type.
    retType: TypeId,

    /// Whether this func has a static initializer.
    hasStaticInitializer: bool,
    
    /// Whether the symbol has been or is in the process of generating it's static initializer.
    genStaticInitVisited: bool = false,

    pub fn getResolvedSymId(self: ResolvedFuncSym) ResolvedSymId {
        return self.key.absResolvedFuncSymKey.rSymId;
    }

    pub fn getResolvedFuncSigId(self: ResolvedFuncSym) ResolvedFuncSigId {
        return self.key.absResolvedFuncSymKey.rFuncSigId;
    }
};

const ResolvedSymType = enum {
    func,
    variable,
    object,
    enumType,
    enumMember,
    module,
    builtinType,
    internal,
};

pub const ResolvedSymId = u32;

const ResolvedSymData = extern union {
    func: extern struct {
        /// Refers to exactly one resolved func sym.
        /// rFuncSymId == cy.NullId indicates this sym is overloaded;
        /// more than one func shares the same symbol. To disambiguate,
        /// `resolvedFuncSymMap` must be queried with a absResolvedFuncSymKey.
        rFuncSymId: ResolvedFuncSymId,
    },
    variable: extern struct {
        chunkId: ChunkId,
        declId: cy.NodeId,
        rTypeSymId: ResolvedSymId,
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

/// Local symbols are resolved during and after the sema pass.
/// Not all module members from builtins are resolved, only ones that are used.
pub const ResolvedSym = struct {
    symT: ResolvedSymType,
    /// Used to backtrack and build the full sym name.
    key: AbsResolvedSymKey,
    inner: ResolvedSymData,
    /// Whether the symbol is exported.
    exported: bool,
    /// Whether the symbol has been or is in the process of generating it's static initializer.
    genStaticInitVisited: bool = false,

    pub fn getObjectTypeId(self: ResolvedSym, vm: *cy.VM) ?rt.TypeId {
        return vm.getObjectTypeId(self.key.absResolvedSymKey.rParentSymId, self.key.absResolvedSymKey.nameId);
    }

    pub fn getModuleId(self: ResolvedSym) ?cy.ModuleId {
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

const RelModuleSymKey = cy.hash.KeyU64;
const ChunkId = u32;

pub const LocalSym = struct {
    /// Can be NullId since some syms are not resolved until they are used. eg. ImportAll syms
    rSymId: Nullable(ResolvedSymId),
    rFuncSymId: Nullable(ResolvedFuncSymId),

    /// Which module to find the sym in.
    rParentSymId: ResolvedSymId,
};

/// Relative symbol signature key. RelFuncSigKey is repurposed for variable syms when `numParams` == cy.NullId.
const RelSymSigKey = cy.RelFuncSigKey;
const RelSymSigKeyContext = cy.RelFuncSigKeyContext;
pub const RelLocalSymKey = cy.hash.KeyU64;

/// Absolute symbol signature key. AbsFuncSigKey is repurposed for variable syms when `numParams` == cy.NullId.
pub const AbsSymSigKey = cy.hash.KeyU64;
pub const AbsResolvedSymKey = cy.hash.KeyU64;
pub const AbsResolvedFuncSymKey = cy.hash.KeyU64;
pub const AbsSymSigKeyContext = cy.AbsFuncSigKeyContext;

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
        .opAssignStmt => {
            const left = c.nodes[node.head.opAssignStmt.left];
            if (left.node_t == .ident) {
                const rtype = try semaExpr(c, node.head.opAssignStmt.right);
                _ = try assignVar(c, node.head.opAssignStmt.left, rtype, .assign);
            } else if (left.node_t == .accessExpr) {
                _ = try accessExpr(c, node.head.opAssignStmt.left);
                _ = try semaExpr(c, node.head.opAssignStmt.right);
            } else if (left.node_t == .indexExpr) {
                _ = try semaExpr(c, node.head.opAssignStmt.left);
                _ = try semaExpr(c, node.head.opAssignStmt.right);
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
                    _ = try assignVar(c, node.head.left_right.left, rtype, .assign);
                } else {
                    const rtype = try semaExpr(c, node.head.left_right.right);
                    _ = try assignVar(c, node.head.left_right.left, rtype, .assign);
                }
            } else if (left.node_t == .indexExpr) {
                _ = try semaExpr(c, left.head.left_right.left);
                _ = try semaExpr(c, left.head.left_right.right);
                _ = try semaExpr(c, node.head.left_right.right);
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
        .varDecl => {
            try varDecl(c, nodeId, true);
        },
        .captureDecl => {
            const left = c.nodes[node.head.left_right.left];
            std.debug.assert(left.node_t == .ident);
            if (node.head.left_right.right != cy.NullId) {
                const rtype = try semaExpr(c, node.head.left_right.right);
                _ = try assignVar(c, node.head.left_right.left, rtype, .captureAssign);
            } else {
                _ = try assignVar(c, node.head.left_right.left, bt.Undefined, .captureAssign);
            }
        },
        .staticDecl => {
            const left = c.nodes[node.head.left_right.left];
            std.debug.assert(left.node_t == .ident);
            if (node.head.left_right.right != cy.NullId) {
                const rtype = try semaExpr(c, node.head.left_right.right);
                _ = try assignVar(c, node.head.left_right.left, rtype, .staticAssign);
            } else {
                _ = try assignVar(c, node.head.left_right.left, bt.Undefined, .staticAssign);
            }
        },
        .typeAliasDecl => {
            const nameN = c.nodes[node.head.typeAliasDecl.name];
            const name = c.getNodeTokenString(nameN);
            const nameId = try ensureNameSym(c.compiler, name);

            const rSymId = try getOrResolveTypeSymFromSpecNode(c, node.head.typeAliasDecl.typeSpecHead);
            const rSym = c.compiler.sema.getResolvedSym(rSymId);
            try setLocalSym(c, nameId, .{
                .rSymId = rSymId,
                .rFuncSymId = cy.NullId,
                .rParentSymId = rSym.key.absResolvedSymKey.rParentSymId,
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
                _ = try ensureLocalBodyVar(c, node.head.whileOptStmt.some, bt.Any);
                _ = try assignVar(c, node.head.whileOptStmt.some, optt, .assign);
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
                    _ = try ensureLocalBodyVar(c, eachClause.head.eachClause.key, bt.Any);
                }
                _ = try ensureLocalBodyVar(c, eachClause.head.eachClause.value, bt.Any);
            }

            try semaStmts(c, node.head.for_iter_stmt.body_head);
            try endIterSubBlock(c);
        },
        .for_range_stmt => {
            try pushIterSubBlock(c);

            if (node.head.for_range_stmt.eachClause != cy.NullId) {
                const eachClause = c.nodes[node.head.for_range_stmt.eachClause];
                _ = try ensureLocalBodyVar(c, eachClause.head.eachClause.value, bt.Number);
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
                _ = try ensureLocalBodyVar(c, node.head.tryStmt.errorVar, bt.Error);
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
        .atStmt => {
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
    const key = RelLocalSymKey{
        .relLocalSymKey = .{
            .nameId = nameId,
            .rFuncSigId = cy.NullId,
        },
    };
    if (c.localSyms.contains(key)) {
        return c.reportErrorAt("The symbol `{}` was already declared.", &.{v(getName(c.compiler, nameId))}, node.head.typeAliasDecl.name);
    }

    const mod = c.getModule();
    try mod.setTypeAlias(c.compiler, name, nodeId);

    try setLocalSym(c, nameId, .{
        .rSymId = cy.NullId,
        .rFuncSymId = cy.NullId,
        .rParentSymId = c.semaResolvedRootSymId,
    });
}

pub fn declareImport(chunk: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = chunk.nodes[nodeId];
    const ident = chunk.nodes[node.head.left_right.left];
    const name = chunk.getNodeTokenString(ident);
    const nameId = try ensureNameSym(chunk.compiler, name);

    const spec = chunk.nodes[node.head.left_right.right];
    const specPath = chunk.getNodeTokenString(spec);

    const modId = try getOrLoadModule(chunk, specPath, nodeId);
    const mod = chunk.compiler.sema.getModule(modId);
    try setLocalSym(chunk, nameId, .{
        .rSymId = mod.resolvedRootSymId,
        .rFuncSymId = cy.NullId,
        .rParentSymId = cy.NullId,
    });
}

fn setLocalSym(chunk: *cy.Chunk, nameId: NameSymId, sym: LocalSym) !void {
    const key = RelLocalSymKey{
        .relLocalSymKey = .{
            .nameId = nameId,
            .rFuncSigId = cy.NullId,
        },
    };
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
    const key = AbsResolvedSymKey.initAbsResolvedSymKey(c.getModule().resolvedRootSymId, nameId);
    const symId = try resolveObjectSym(c.compiler, key, objModId);

    // Persist for declareObjectMembers.
    c.nodes[node.head.objectDecl.name].head.ident.sema_crSymId = CompactResolvedSymId.initSymId(symId);
}

pub fn declareObjectMembers(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const objSymId = c.nodes[node.head.objectDecl.name].head.ident.sema_crSymId.id;
    const objSym = c.compiler.sema.getResolvedSym(objSymId);
    const objModId = objSym.inner.object.modId;
    const rtTypeId = objSym.inner.object.typeId;

    // Load fields.
    var i: u32 = 0;

    var fieldId = node.head.objectDecl.fieldsHead;
    while (fieldId != cy.NullId) : (i += 1) {
        const field = c.nodes[fieldId];
        const fieldName = c.getNodeTokenString(field);
        const fieldSymId = try c.compiler.vm.ensureFieldSym(fieldName);

        var fieldType: ResolvedSymId = undefined;
        if (field.head.objectField.typeSpecHead != cy.NullId) {
            fieldType = try getOrResolveTypeSymFromSpecNode(c, field.head.objectField.typeSpecHead);
        } else {
            fieldType = bt.Any;
        }

        try c.compiler.vm.addFieldSym(rtTypeId, fieldSymId, @intCast(u16, i), fieldType);
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

        cy.module.declareUserFunc(c.compiler, objModId, funcName, func.rFuncSigId, declId, false) catch |err| {
            if (err == error.DuplicateSymName) {
                return c.reportErrorAt("The symbol `{}` already exists.", &.{v(funcName)}, declId);
            } else if (err == error.DuplicateFuncSig) {
                return c.reportErrorAt("The function `{}` with the same signature already exists.", &.{v(funcName)}, declId);
            } else return err;
        };
        const key = AbsResolvedSymKey.initAbsResolvedSymKey(objSymId, funcNameId);
        _ = try resolveUserFunc(c, key, func.rFuncSigId, declId, false);

        funcId = funcN.next;
    }
}

fn objectDecl(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    // const nameN = c.nodes[node.head.objectDecl.name];
    // const name = c.getNodeTokenString(nameN);
    // const nameId = try ensureNameSym(c.compiler, name);
    // const rSymId = nameN.head.ident.sema_crSymId.id;

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
                func.semaBlockId = blockId;
                errdefer endBlock(c) catch stdx.fatal();
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
        errdefer endBlock(c) catch stdx.fatal();
        try appendFuncParamVars(c, func);
        try semaStmts(c, funcN.head.func.bodyHead);
        try endFuncSymBlock(c, func.numParams);

        funcId = funcN.next;
    }
}

fn checkForDuplicateUsingSym(c: *cy.Chunk, parentSymId: ResolvedSymId, nameId: NameSymId, nodeId: cy.NodeId) !void {
    if (parentSymId == c.semaResolvedRootSymId) {
        // Root symbol, check that it's not a local alias.
        const key = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (c.localSyms.contains(key)) {
            return c.reportErrorAt("The symbol `{}` was already declared.",
                &.{v(getName(c.compiler, nameId))}, nodeId);
        }
    }
}

pub fn declareFuncInit(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const declId = try appendFuncDecl(c, nodeId, true);
    c.nodes[nodeId].head.func.semaDeclId = declId;

    const func = &c.semaFuncDecls.items[declId];
    const name = func.getName(c);
    const nameId = try ensureNameSym(c.compiler, name);

    const mod = c.getModule();
    try checkForDuplicateUsingSym(c, mod.resolvedRootSymId, nameId, func.getNameNode(c));
    cy.module.declareUserFunc(c.compiler, c.modId, name, func.rFuncSigId, declId, true) catch |err| {
        if (err == error.DuplicateSymName) {
            return c.reportErrorAt("The symbol `{}` already exists.", &.{v(name)}, nodeId);
        } else if (err == error.DuplicateFuncSig) {
            return c.reportErrorAt("The function `{}` with the same signature already exists.", &.{v(name)}, nodeId);
        } else return err;
    };
    const key = AbsResolvedSymKey.initAbsResolvedSymKey(mod.resolvedRootSymId, nameId);
    _ = try resolveUserFunc(c, key, func.rFuncSigId, declId, true);
}

fn funcDeclInit(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const declId = c.nodes[nodeId].head.func.semaDeclId;

    const node = c.nodes[nodeId];
    const func = &c.semaFuncDecls.items[declId];

    const name = func.getName(c);

    c.curSemaInitingSym = CompactResolvedSymId.initFuncSymId(func.inner.staticFunc.semaFuncSymId);
    c.semaVarDeclDeps.clearRetainingCapacity();
    defer c.curSemaInitingSym = CompactResolvedSymId.initNull();

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

pub fn declareFunc(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const declId = try appendFuncDecl(c, nodeId, true);
    const func = &c.semaFuncDecls.items[declId];
    const name = func.getName(c);
    const nameId = try ensureNameSym(c.compiler, name);
    c.nodes[nodeId].head.func.semaDeclId = declId;

    const mod = c.getModule();
    try checkForDuplicateUsingSym(c, mod.resolvedRootSymId, nameId, func.getNameNode(c));
    cy.module.declareUserFunc(c.compiler, c.modId, name, func.rFuncSigId, declId, false) catch |err| {
        if (err == error.DuplicateSymName) {
            return c.reportErrorAt("The symbol `{}` already exists.", &.{v(name)}, nodeId);
        } else if (err == error.DuplicateFuncSig) {
            return c.reportErrorAt("The function `{}` with the same signature already exists.", &.{v(name)}, nodeId);
        } else return err;
    };
    const key = AbsResolvedSymKey.initAbsResolvedSymKey(mod.resolvedRootSymId, nameId);
    _ = try resolveUserFunc(c, key, func.rFuncSigId, declId, false);
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
    const varSpec = c.nodes[node.head.varDecl.varSpec];
    const nameN = c.nodes[varSpec.head.varSpec.name];
    const name = c.getNodeTokenString(nameN);
    const nameId = try ensureNameSym(c.compiler, name);
    try c.compiler.sema.modules.items[c.modId].setUserVar(c.compiler, name, nodeId);

    // var type.
    var typeSymId: ResolvedSymId = undefined;
    if (varSpec.head.varSpec.typeSpecHead != cy.NullId) {
        typeSymId = try getOrResolveTypeSymFromSpecNode(c, varSpec.head.varSpec.typeSpecHead);
    } else {
        typeSymId = bt.Any;
    }

    const rSymId = try resolveLocalVarSym(c, c.semaResolvedRootSymId, nameId, typeSymId, nodeId, true);
    c.nodes[nodeId].head.varDecl.sema_rSymId = rSymId;
}

fn varDecl(c: *cy.Chunk, nodeId: cy.NodeId, exported: bool) !void {
    _ = exported;
    const node = c.nodes[nodeId];
    const varSpec = c.nodes[node.head.varDecl.varSpec];
    const nameN = c.nodes[varSpec.head.varSpec.name];
    if (nameN.node_t == .ident) {
        const name = c.getNodeTokenString(nameN);

        const rSymId = node.head.varDecl.sema_rSymId;
        const crSymId = CompactResolvedSymId.initSymId(rSymId);

        c.curSemaInitingSym = crSymId;
        c.semaVarDeclDeps.clearRetainingCapacity();
        defer c.curSemaInitingSym = CompactResolvedSymId.initNull();

        const right = c.nodes[node.head.varDecl.right];
        if (right.node_t == .matchBlock) {
            _ = try matchBlock(c, node.head.varDecl.right, true);
        } else {
            _ = semaExpr(c, node.head.varDecl.right) catch |err| {
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

fn flexExpr(c: *cy.Chunk, nodeId: cy.NodeId) anyerror!TypeId {
    const node = c.nodes[nodeId];
    const reqType = switch (node.node_t) {
        .number,
        .nonDecInt => bt.NumberLit,
        else => bt.Any,
    };
    return semaExprType(c, nodeId, reqType);
}

fn semaExpr(c: *cy.Chunk, nodeId: cy.NodeId) anyerror!TypeId {
    const res = try semaExprInner(c, nodeId, bt.Any);
    c.nodeTypes[nodeId] = res;
    return res;
}

fn semaExprType(c: *cy.Chunk, nodeId: cy.NodeId, reqType: TypeId) anyerror!TypeId {
    const res = try semaExprInner(c, nodeId, reqType);
    c.nodeTypes[nodeId] = res;
    return res;
}

fn semaExprInner(c: *cy.Chunk, nodeId: cy.NodeId, reqType: TypeId) anyerror!TypeId {
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

            var crSymId = CompactResolvedSymId.initNull();
            if (nameN.node_t == .ident) {
                crSymId = nameN.head.ident.sema_crSymId;
            } else if (nameN.node_t == .accessExpr) {
                crSymId = nameN.head.accessExpr.sema_crSymId;
            }

            if (crSymId.isPresent()) {
                if (!crSymId.isFuncSymId) {
                    c.nodes[nodeId].head.objectInit.sema_rSymId = crSymId.id;
                    const initializer = c.nodes[node.head.objectInit.initializer];
                    var i: u32 = 0;
                    var entry_id = initializer.head.child_head;
                    while (entry_id != cy.NullId) : (i += 1) {
                        var entry = c.nodes[entry_id];
                        _ = try semaExpr(c, entry.head.mapEntry.right);
                        entry_id = entry.next;
                    }
                    return crSymId.id;
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
                    .semaNumberVal = @intToFloat(f64, val),
                },
            };
            const canBeInt = std.math.cast(i32, val) != null;
            if (reqType == bt.NumberLit) {
                return bt.NumberLit;
            } else if (reqType == bt.Integer) {
                if (canBeInt) {
                    return bt.Integer;
                } else {
                    return c.reportError("Number literal can not be an integer.", &.{});
                }
            } else {
                return bt.Number;
            }
        },
        .number => {
            if (reqType == bt.NumberLit) {
                return bt.NumberLit;
            } else if (reqType == bt.Integer) {
                const literal = c.getNodeTokenString(node);
                const val = try std.fmt.parseFloat(f64, literal);
                if (cy.Value.floatCanBeInteger(val)) {
                    const int = @floatToInt(i64, val);
                    if (std.math.cast(i32, int) != null) {
                        return bt.Integer;
                    }
                }
                return c.reportError("Number literal can not be an integer.", &.{});
            } else {
                return bt.Number;
            }
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
            _ = try semaExpr(c, node.head.sliceExpr.arr);

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

            return bt.List;
        },
        .accessExpr => {
            const res = try accessExpr(c, nodeId);
            return res.exprT;
        },
        .indexExpr => {
            _ = try semaExpr(c, node.head.left_right.left);

            const index = c.nodes[node.head.left_right.right];
            if (index.node_t == .unary_expr and index.head.unary.op == .minus) {
                _ = try semaExpr(c, index.head.unary.child);
            } else {
                _ = try semaExpr(c, node.head.left_right.right);
            }
            return bt.Any;
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
                    _ = try semaExpr(c, node.head.unary.child);
                    return bt.Number;
                },
                .not => {
                    _ = try semaExpr(c, node.head.unary.child);
                    return bt.Boolean;
                },
                .bitwiseNot => {
                    _ = try semaExpr(c, node.head.unary.child);
                    return bt.Number;
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
                .percent => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Number;
                },
                .caret => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Number;
                },
                .plus,
                .minus => {
                    const leftT = try semaExprType(c, left, reqType);
                    const rightT = try semaExprType(c, right, leftT);

                    if (leftT == bt.Integer and rightT == bt.Integer) {
                        return bt.Integer;
                    } else {
                        return bt.Number;
                    }
                },
                .bitwiseAnd => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Number;
                },
                .bitwiseOr => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Number;
                },
                .bitwiseXor => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Number;
                },
                .bitwiseLeftShift => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Number;
                },
                .bitwiseRightShift => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Number;
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
                .bang_equal => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Boolean;
                },
                .equal_equal => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Boolean;
                },
                .less => {
                    const leftT = try semaExpr(c, left);
                    _ = try semaExprType(c, right, leftT);
                    return bt.Boolean;
                },
                .less_equal => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Boolean;
                },
                .greater => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Boolean;
                },
                .greater_equal => {
                    _ = try semaExpr(c, left);
                    _ = try semaExpr(c, right);
                    return bt.Boolean;
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

            const rFuncSigId = try ensureResolvedUntypedFuncSig(c.compiler, func.numParams);
            func.inner.lambda.rFuncSigId = rFuncSigId;
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

            const rFuncSigId = try ensureResolvedUntypedFuncSig(c.compiler, func.numParams);
            func.inner.lambda.rFuncSigId = rFuncSigId;
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

            const callArgStart = c.compiler.typeStack.items.len;
            defer c.compiler.typeStack.items.len = callArgStart;

            // Push Any in case it's a method sym. pushCallArgs will ignore the self param.
            try c.compiler.typeStack.append(c.alloc, bt.Any);

            const callArgs = try pushCallArgs(c, node.head.callExpr.arg_head);
            const reqRet = bt.Any;

            const left = c.nodes[callee.head.accessExpr.left];
            var crLeftSym = CompactResolvedSymId.initNull();
            if (left.node_t == .ident) {
                crLeftSym = left.head.ident.sema_crSymId;
            } else if (left.node_t == .accessExpr) {
                crLeftSym = left.head.accessExpr.sema_crSymId;
            }
            if (crLeftSym.isPresent()) {
                // Calling a symbol.
                if (!crLeftSym.isFuncSymId) {
                    const right = c.nodes[callee.head.accessExpr.right];
                    const name = c.getNodeTokenString(right);
                    const nameId = try ensureNameSym(c.compiler, name);

                    if (callArgs.hasDynamicArg) {
                        // Runtime type check.
                        if (try getOrResolveSymForDynamicFuncCall(c, crLeftSym.id, nameId, callArgs.argTypes)) |callRes| {
                            if (callArgs.hasFlexArg) {
                                updateFlexArgTypes(c, node.head.callExpr.arg_head, callArgs.argTypes, callRes.funcSigId);
                            }
                            try referenceSym(c, callRes.crSymId, true);
                            c.nodes[node.head.callExpr.callee].head.accessExpr.sema_crSymId = callRes.crSymId;
                            return callRes.retType;
                        }
                    } else {
                        // Compile-time type check.
                        if (try getOrResolveSymForFuncCall(c, crLeftSym.id, nameId, callArgs.argTypes, reqRet)) |callRes| {
                            if (callArgs.hasFlexArg) {
                                updateFlexArgTypes(c, node.head.callExpr.arg_head, callArgs.argTypes, callRes.funcSigId);
                            }
                            try referenceSym(c, callRes.crSymId, true);
                            c.nodes[node.head.callExpr.callee].head.accessExpr.sema_crSymId = callRes.crSymId;
                            return callRes.retType;
                        }
                    }
                }
            }

            // Dynamic method call.
            const selfCallArgs = c.compiler.typeStack.items[callArgStart..];
            const rFuncSigId = try ensureResolvedFuncSigNonFlex(c.compiler, selfCallArgs, reqRet);
            if (callArgs.hasFlexArg) {
                updateFlexSelfArgTypes(c, node.head.callExpr.arg_head, selfCallArgs, rFuncSigId);
            }
            c.nodes[callee.head.accessExpr.right].head.ident.semaMethodSigId = rFuncSigId;

            return bt.Dynamic;
        } else if (callee.node_t == .ident) {
            const name = c.getNodeTokenString(callee);
            const res = try getOrLookupVar(c, name, .readSkipStaticVar);
            if (res.isLocal) {
                c.nodes[node.head.callExpr.callee].head.ident.semaVarId = res.id;

                var numArgs: u32 = 1;
                var arg_id = node.head.callExpr.arg_head;
                while (arg_id != cy.NullId) : (numArgs += 1) {
                    const arg = c.nodes[arg_id];
                    _ = try semaExpr(c, arg_id);
                    arg_id = arg.next;
                }
                return bt.Any;
            } else {
                const callArgStart = c.compiler.typeStack.items.len;
                defer c.compiler.typeStack.items.len = callArgStart;

                const nameId = try ensureNameSym(c.compiler, name);

                // const funcCandStart = c.funcCandidateStack.items.len;
                // defer c.funcCandidateStack.items.len = funcCandStart;
                // const funcCandidates = try pushFuncCandidates(c, c.semaResolvedRootSymId, nameId, node.head.callExpr.numArgs);
                const callArgs = try pushCallArgs(c, node.head.callExpr.arg_head);
                const reqRet = bt.Any;

                c.curNodeId = node.head.callExpr.callee;
                if (callArgs.hasDynamicArg) {
                    // Runtime type check.
                    if (try getOrResolveSymForDynamicFuncCall(c, c.semaResolvedRootSymId, nameId, callArgs.argTypes)) |callRes| {
                        if (callArgs.hasFlexArg) {
                            updateFlexArgTypes(c, node.head.callExpr.arg_head, callArgs.argTypes, callRes.funcSigId);
                        }
                        try referenceSym(c, callRes.crSymId, true);
                        c.nodes[node.head.callExpr.callee].head.ident.sema_crSymId = callRes.crSymId;
                        return callRes.retType;
                    }
                } else {
                    // Compile-time type check.
                    if (try getOrResolveSymForFuncCall(c, c.semaResolvedRootSymId, nameId, callArgs.argTypes, reqRet)) |callRes| {
                        if (callArgs.hasFlexArg) {
                            updateFlexArgTypes(c, node.head.callExpr.arg_head, callArgs.argTypes, callRes.funcSigId);
                        }
                        try referenceSym(c, callRes.crSymId, true);
                        c.nodes[node.head.callExpr.callee].head.ident.sema_crSymId = callRes.crSymId;
                        return callRes.retType;
                    }
                }

                return c.reportError("No such symbol: `{}`", &.{v(name)});
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
    const res = try getOrLookupVar(c, name, .read);
    if (res.isLocal) {
        c.nodes[nodeId].head.ident.semaVarId = res.id;
        return c.vars.items[res.id].vtype;
    } else {
        const nameId = try ensureNameSym(c.compiler, name);

        const symRes = try mustGetOrResolveDistinctSym(c, c.semaResolvedRootSymId, nameId);
        const crSymId = symRes.toCompactId();
        try referenceSym(c, crSymId, true);
        c.nodes[nodeId].head.ident.sema_crSymId = crSymId;

        const rSym = c.compiler.sema.getResolvedSym(symRes.rSymId);
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
    }
}

/// Recursively walks a type spec head node and returns the final resolved type sym.
/// Assumes head is non null.
fn getOrResolveTypeSymFromSpecNode(chunk: *cy.Chunk, head: cy.NodeId) !ResolvedSymId {
    var nodeId = head;
    var rParentSymId = chunk.semaResolvedRootSymId;
    // log.debug("getOrResolveTypeSymFromSpecNode from {} ", .{rParentSymId});
    while (true) {
        const node = chunk.nodes[nodeId];
        const name = chunk.getNodeTokenString(node);
        const nameId = try ensureNameSym(chunk.compiler, name);
        // log.debug("looking for {s}", .{name});

        if (node.next == cy.NullId) {
            chunk.curNodeId = nodeId;
            return getOrResolveTypeSym(chunk, rParentSymId, nameId);
        } else {
            const res = try mustGetOrResolveDistinctSym(chunk, rParentSymId, nameId);
            rParentSymId = res.rSymId;
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
        .rFuncSigId = undefined,
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
    decl.rFuncSigId = try ensureResolvedFuncSig(chunk.compiler, chunk.compiler.tempSyms.items, retType);
    decl.rRetTypeSymId = retType;

    const declId = @intCast(u32, chunk.semaFuncDecls.items.len);
    try chunk.semaFuncDecls.append(chunk.alloc, decl);
    return declId;
}

pub fn pushBlock(self: *cy.Chunk, funcDeclId: u32) !BlockId {
    self.curSemaBlockId = @intCast(u32, self.semaBlocks.items.len);
    const nextSubBlockId = @intCast(u32, self.semaSubBlocks.items.len);
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
    self.curSemaSubBlockId = @intCast(u32, self.semaSubBlocks.items.len);
    try self.semaSubBlocks.append(self.alloc, SubBlock.init(prev, self.assignedVarStack.items.len));
}

fn pushMethodParamVars(c: *cy.Chunk, func: *const FuncDecl) !void {
    const curNodeId = c.curNodeId;
    defer c.curNodeId = curNodeId;

    const sblock = curBlock(c);

    if (func.numParams > 1) {
        const rFuncSig = c.compiler.sema.resolvedFuncSigs.items[func.rFuncSigId];
        const params = rFuncSig.params()[1..];

        // Skip the first param node.
        var curNode = func.paramHead;
        curNode = c.nodes[curNode].next;

        for (params) |rParamSymId| {
            const param = c.nodes[curNode];
            const name = c.getNodeTokenString(c.nodes[param.head.funcParam.name]);

            try types.assertTypeSym(c, rParamSymId);

            c.curNodeId = curNode;
            const id = try pushLocalVar(c, name, rParamSymId);
            try sblock.params.append(c.alloc, id);

            curNode = param.next;
        }
    }

    // Add self receiver param.
    var id = try pushLocalVar(c, "self", bt.Any);
    try sblock.params.append(c.alloc, id);
}

fn appendFuncParamVars(chunk: *cy.Chunk, func: *const FuncDecl) !void {
    const sblock = curBlock(chunk);

    if (func.numParams > 0) {
        const rFuncSig = chunk.compiler.sema.resolvedFuncSigs.items[func.rFuncSigId];
        const params = rFuncSig.params();
        var curNode = func.paramHead;
        for (params) |rParamSymId| {
            const param = chunk.nodes[curNode];
            const name = chunk.getNodeTokenString(chunk.nodes[param.head.funcParam.name]);

            try types.assertTypeSym(chunk, rParamSymId);
            const id = try pushLocalVar(chunk, name, rParamSymId);
            try sblock.params.append(chunk.alloc, id);

            curNode = param.next;
        }
    }
}

fn pushLocalVar(c: *cy.Chunk, name: []const u8, vtype: TypeId) !LocalVarId {
    const sblock = curBlock(c);
    const id = @intCast(u32, c.vars.items.len);
    const res = try sblock.nameToVar.getOrPut(c.alloc, name);
    if (res.found_existing) {
        return c.reportError("Var `{}` already exists", &.{v(name)});
    } else {
        res.value_ptr.* = id;
        try c.vars.append(c.alloc, .{
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

fn pushStaticVarAlias(c: *cy.Chunk, name: []const u8, crSymId: CompactResolvedSymId) !LocalVarId {
    const id = try pushLocalVar(c, name, bt.Any);
    c.vars.items[id].isStaticAlias = true;
    c.vars.items[id].inner.staticAlias = .{
        .crSymId = crSymId,
    };
    return id;
}

fn pushCapturedVar(self: *cy.Chunk, name: []const u8, parentVarId: LocalVarId, vtype: TypeId) !LocalVarId {
    const block = curBlock(self);
    const id = try pushLocalVar(self, name, vtype);
    const capturedIdx = @intCast(u8, block.captures.items.len);
    self.vars.items[id].capturedIdx = capturedIdx;
    self.vars.items[id].isBoxed = true;
    self.vars.items[id].genIsDefined = true;

    try self.capVarDescs.put(self.alloc, id, .{
        .user = parentVarId,
    });

    try block.captures.append(self.alloc, id);
    return id;
}

fn pushLocalBodyVar(self: *cy.Chunk, name: []const u8, vtype: TypeId) !LocalVarId {
    const id = try pushLocalVar(self, name, vtype);
    try curBlock(self).locals.append(self.alloc, id);
    return id;
}

fn ensureLocalBodyVar(self: *cy.Chunk, ident: cy.NodeId, vtype: TypeId) !LocalVarId {
    const node = self.nodes[ident];
    const name = self.getNodeTokenString(node);
    if (curBlock(self).nameToVar.get(name)) |varId| {
        self.nodes[ident].head.ident.semaVarId = varId;
        return varId;
    } else {
        const id = try pushLocalBodyVar(self, name, vtype);
        self.nodes[ident].head.ident.semaVarId = id;
        return id;
    }
}

fn referenceSym(c: *cy.Chunk, rSymId: CompactResolvedSymId, trackDep: bool) !void {
    if (trackDep) {
        if (c.isInStaticInitializer()) {
            // Record this symbol as a dependency.
            const res = try c.semaInitializerSyms.getOrPut(c.alloc, c.curSemaInitingSym);
            if (res.found_existing) {
                const depRes = try c.semaVarDeclDeps.getOrPut(c.alloc, rSymId);
                if (!depRes.found_existing) {
                    try c.bufU32.append(c.alloc, @bitCast(u32, rSymId));
                    res.value_ptr.*.depsEnd = @intCast(u32, c.bufU32.items.len);
                    depRes.value_ptr.* = {};
                }
            } else {
                const start = @intCast(u32, c.bufU32.items.len);
                try c.bufU32.append(c.alloc, @bitCast(u32, rSymId));
                res.value_ptr.* = .{
                    .depsStart = start,
                    .depsEnd = @intCast(u32, c.bufU32.items.len),
                };
            }
        }
    }
}

const VarLookupStrategy = enum {
    /// Look upwards for a parent local. If no such local exists, assume a static var.
    read,
    /// Same as read but does not try to find a static var symbol.
    readSkipStaticVar,
    /// Assume a static var.
    staticAssign,
    /// Look upwards for a parent local. If no such local exists, a compile error is returned.
    captureAssign,
    /// If missing in the current block, a new local is created.
    assign,
};

const VarLookupResult = struct {
    /// If `isLocal` is true, id is a LocalVarId.
    id: u32,
    /// If `isLocal` is false, id is a CompactResolvedSymId.
    isLocal: bool,
    /// Whether the local var was created.
    created: bool,
};

fn getOrLookupVar(self: *cy.Chunk, name: []const u8, strat: VarLookupStrategy) !VarLookupResult {
    const sblock = curBlock(self);
    if (sblock.nameToVar.get(name)) |varId| {
        const svar = self.vars.items[varId];
        switch (strat) {
            .readSkipStaticVar,
            .read => {
                if (!svar.isStaticAlias) {
                    // Can not reference local var in a static var decl unless it's in a nested block.
                    // eg. a = 0
                    //     var b = a
                    if (self.isInStaticInitializer() and self.semaBlockDepth() == 1) {
                        self.compiler.errorPayload = self.curNodeId;
                        return error.CanNotUseLocal;
                    }
                    return VarLookupResult{
                        .id = varId,
                        .isLocal = true,
                        .created = false,
                    };
                } else {
                    return VarLookupResult{
                        .id = @bitCast(u32, svar.inner.staticAlias.crSymId),
                        .isLocal = false,
                        .created = false,
                    };
                }
            },
            .assign => {
                if (svar.isStaticAlias) {
                    // Assumes static variables can only exist in the main block.
                    if (svar.hasCaptureOrStaticModifier or self.semaBlockDepth() == 1) {
                        return VarLookupResult{
                            .id = @bitCast(u32, svar.inner.staticAlias.crSymId),
                            .isLocal = false,
                            .created = false,
                        };
                    } else {
                        return self.reportError("`{}` already references a static variable. The variable must be declared with `static` before assigning to it.", &.{v(name)});
                    }
                } else if (svar.isCaptured()) {
                    if (svar.hasCaptureOrStaticModifier) {
                        return VarLookupResult{
                            .id = varId,
                            .isLocal = true,
                            .created = false,
                        };
                    } else {
                        return self.reportError("`{}` already references a captured variable. The variable must be declared with `capture` before assigning to it.", &.{v(name)});
                    }
                } else {
                    return VarLookupResult{
                        .id = varId,
                        .isLocal = true,
                        .created = false,
                    };
                }
            },
            .captureAssign => {
                if (!svar.isCaptured()) {
                    // Previously not captured, update to captured.
                    return self.reportError("TODO: update to captured variable", &.{});
                } else {
                    return VarLookupResult{
                        .id = varId,
                        .isLocal = true,
                        .created = false,
                    };
                }
            },
            .staticAssign => {
                if (!svar.isStaticAlias) {
                    // Previously not static alias, update to static alias.
                    return self.reportError("TODO: update to static alias", &.{});
                } else {
                    return VarLookupResult{
                        .id = cy.NullId,
                        .isLocal = false,
                        .created = false,
                    };
                }
            },
            // When typed declaration is implemented, that can create a new local if the variable was previously implicity captured.
            // // Create a new local var and update mapping so any references after will refer to the local var.
            // const sblock = curBlock(self);
            // _ = sblock.nameToVar.remove(name);
            // const id = try pushLocalBodyVar(self, name, vtype);
            // if (sblock.subBlockDepth > 1) {
            //     self.vars.items[id].genInitializer = true;
            // }
        }
    }

    // Perform lookup based on the strategy. See `VarLookupStrategy`.
    switch (strat) {
        .readSkipStaticVar,
        .read => {
            if (lookupParentLocal(self, name)) |res| {
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
                    return self.reportErrorAt("Can not capture the local variable `{}` from static function `{}`.\nOnly lambdas (function values) can capture local variables.", &.{v(name), v(funcName)}, self.curNodeId);
                }

                // Create a local captured variable.
                const parentVar = self.vars.items[res.varId];
                const id = try pushCapturedVar(self, name, res.varId, parentVar.vtype);
                return VarLookupResult{
                    .id = id,
                    .isLocal = true,
                    .created = true,
                };
            } else {
                if (strat == .read) {
                    const nameId = try ensureNameSym(self.compiler, name);
                    const res = try mustGetOrResolveDistinctSym(self, self.semaResolvedRootSymId, nameId);
                    _ = try pushStaticVarAlias(self, name, res.toCompactId());
                    return VarLookupResult{
                        .id = @bitCast(u32, res.toCompactId()),
                        .isLocal = false,
                        .created = false,
                    };
                } else {
                    return VarLookupResult{
                        .id = cy.NullId,
                        .isLocal = false,
                        .created = false,
                    };
                }
            }
        },
        .staticAssign => {
            const nameId = try ensureNameSym(self.compiler, name);
            const res = try mustGetOrResolveDistinctSym(self, self.semaResolvedRootSymId, nameId);
            const id = try pushStaticVarAlias(self, name, res.toCompactId());
            self.vars.items[id].hasCaptureOrStaticModifier = true;
            return VarLookupResult{
                .id = @bitCast(u32, res.toCompactId()),
                .isLocal = false,
                .created = true,
            };
        },
        .captureAssign => {
            if (lookupParentLocal(self, name)) |res| {
                if (self.isInStaticInitializer()) {
                    if (res.blockDepth == 1) {
                        return self.reportError("Can not use local in static variable initializer.", &.{});
                    }
                } else if (sblock.isStaticFuncBlock) {
                    // Can not capture local before static function block.
                    const func = self.semaFuncDecls.items[sblock.funcDeclId];
                    const funcName = func.getName(self);
                    return self.reportErrorAt("Can not capture the local variable `{}` from static function `{}`.\nOnly lambdas (function values) can capture local variables.", &.{v(name), v(funcName)}, self.curNodeId);
                }
                // Create a local captured variable.
                const parentVar = self.vars.items[res.varId];
                const id = try pushCapturedVar(self, name, res.varId, parentVar.vtype);
                self.vars.items[id].hasCaptureOrStaticModifier = true;
                return VarLookupResult{
                    .id = id,
                    .isLocal = true,
                    .created = true,
                };
            } else {
                return self.reportError("Could not find a parent local named `{}`.", &.{v(name)});
            }
        },
        .assign => {
            // Prefer static variable in the same block.
            // For now, only do this for main block.
            if (self.semaBlockDepth() == 1) {
                const nameId = try ensureNameSym(self.compiler, name);
                if (hasResolvedSym(self, self.semaResolvedRootSymId, nameId)) {
                    return VarLookupResult{
                        .id = cy.NullId,
                        .isLocal = false,
                        .created = false,
                    };
                }
            }
            const id = try pushLocalBodyVar(self, name, bt.Undefined);
            return VarLookupResult{
                .id = id,
                .isLocal = true,
                .created = true,
            };
        },
    }
}

const LookupParentLocalResult = struct {
    varId: LocalVarId,

    // Main block starts at 1.
    blockDepth: u32,
};

fn lookupParentLocal(c: *cy.Chunk, name: []const u8) ?LookupParentLocalResult {
    // Only check one block above.
    if (c.semaBlockDepth() > 1) {
        const prevId = c.semaBlockStack.items[c.semaBlockDepth() - 1];
        const prev = c.semaBlocks.items[prevId];
        if (prev.nameToVar.get(name)) |varId| {
            if (!c.vars.items[varId].isStaticAlias) {
                return .{
                    .varId = varId,
                    .blockDepth = c.semaBlockDepth(),
                };
            }
        }
    }
    return null;
}

fn getTypeForResolvedValueSym(chunk: *cy.Chunk, crSymId: CompactResolvedSymId) !TypeId {
    if (crSymId.isFuncSymId) {
        return bt.Any;
    } else {
        const rSym = chunk.compiler.sema.getResolvedSym(crSymId.id);
        switch (rSym.symT) {
            .variable => {
                return rSym.inner.variable.rTypeSymId;
            },
            .enumMember => {
                return rSym.key.absResolvedSymKey.rParentSymId;
            },
            else => {
                return bt.Any;
            },
        }
    }
}

/// Give a name to the internal sym for formatting.
pub fn addResolvedInternalSym(c: *cy.VMcompiler, name: []const u8) !ResolvedSymId {
    const nameId = try ensureNameSym(c, name);
    const id = @intCast(u32, c.sema.resolvedSyms.items.len);
    try c.sema.resolvedSyms.append(c.alloc, .{
        .key = AbsResolvedSymKey{
            .absResolvedSymKey = .{
                .rParentSymId = cy.NullId,
                .nameId = nameId,
            },
        },
        .symT = .internal,
        .inner = undefined,
        .exported = true,
    });
    return id;
}

pub fn addResolvedBuiltinSym(c: *cy.VMcompiler, name: []const u8, typeId: rt.TypeId) !ResolvedSymId {
    const nameId = try ensureNameSym(c, name);
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = cy.NullId,
            .nameId = nameId,
        },
    };

    const spec = try std.fmt.allocPrint(c.alloc, "builtin.{s}", .{name});
    defer c.alloc.free(spec);
    const modId = try cy.module.appendModule(c, spec);
    const mod = c.sema.getModulePtr(modId);

    const id = @intCast(u32, c.sema.resolvedSyms.items.len);
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

fn addResolvedUntypedFuncSig(c: *cy.VMcompiler, numParams: u32) !ResolvedFuncSigId {
    // AnyType for params and return.
    try c.tempSyms.resize(c.alloc, numParams);
    for (c.tempSyms.items) |*stype| {
        stype.* = bt.Any;
    }
    return ensureResolvedFuncSig(c, c.tempSyms.items, bt.Any);
}

pub fn ensureResolvedUntypedFuncSig(c: *cy.VMcompiler, numParams: u32) !ResolvedFuncSigId {
    if (numParams < c.sema.resolvedUntypedFuncSigs.items.len) {
        var rFuncSigId = c.sema.resolvedUntypedFuncSigs.items[numParams];
        if (rFuncSigId == cy.NullId) {
            rFuncSigId = try addResolvedUntypedFuncSig(c, numParams);
            c.sema.resolvedUntypedFuncSigs.items[numParams] = rFuncSigId;
        }
        return rFuncSigId;
    }
    const end = c.sema.resolvedUntypedFuncSigs.items.len;
    try c.sema.resolvedUntypedFuncSigs.resize(c.alloc, numParams + 1);
    for (end..c.sema.resolvedUntypedFuncSigs.items.len) |i| {
        c.sema.resolvedUntypedFuncSigs.items[i] = cy.NullId;
    }
    const rFuncSigId = try addResolvedUntypedFuncSig(c, numParams);
    c.sema.resolvedUntypedFuncSigs.items[numParams] = rFuncSigId;
    return rFuncSigId;
}

fn ensureResolvedFuncSigNonFlex(c: *cy.VMcompiler, params: []const TypeId, ret: TypeId) !ResolvedFuncSigId {
    try c.tempSyms.resize(c.alloc, params.len);
    for (params, 0..) |param, i| {
        const rSymId = types.toNonFlexType(param);
        c.tempSyms.items[i] = rSymId;
    }
    return ensureResolvedFuncSig(c, c.tempSyms.items, ret);
}

pub fn ensureResolvedFuncSig(c: *cy.VMcompiler, params: []const TypeId, ret: TypeId) !ResolvedFuncSigId {
    const res = try c.sema.resolvedFuncSigMap.getOrPut(c.alloc, .{
        .paramPtr = params.ptr,
        .paramLen = @intCast(u32, params.len),
        .retSymId = ret,
    });
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        const id = @intCast(u32, c.sema.resolvedFuncSigs.items.len);
        const new = try c.alloc.dupe(ResolvedSymId, params);
        var isTyped = false;
        for (params) |rSymId| {
            if (rSymId != bt.Any) {
                isTyped = true;
                break;
            }
        }
        if (ret != bt.Any) {
            isTyped = true;
        }
        try c.sema.resolvedFuncSigs.append(c.alloc, .{
            .paramPtr = new.ptr,
            .paramLen = @intCast(u16, new.len),
            .retSymId = ret,
            .isTyped = isTyped,
        });
        res.value_ptr.* = id;
        res.key_ptr.* = .{
            .paramPtr = new.ptr,
            .paramLen = @intCast(u32, new.len),
            .retSymId = ret,
        };
        return id;
    }
}

/// Format: (Type, ...) RetType
pub fn getResolvedFuncSigTempStr(c: *cy.VMcompiler, rFuncSigId: ResolvedFuncSigId) ![]const u8 {
    c.vm.u8Buf.clearRetainingCapacity();
    const w = c.vm.u8Buf.writer(c.alloc);
    try writeResolvedFuncSigStr(c, w, rFuncSigId);
    return c.vm.u8Buf.items();
}
pub fn allocResolvedFuncSigStr(c: *cy.VMcompiler, rFuncSigId: ResolvedFuncSigId) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(c.alloc);

    const w = buf.writer(c.alloc);
    try writeResolvedFuncSigStr(c, w, rFuncSigId);
    return buf.toOwnedSlice(c.alloc);
}
pub fn writeResolvedFuncSigStr(c: *cy.VMcompiler, w: anytype, rFuncSigId: ResolvedFuncSigId) !void {
    const rFuncSig = c.sema.resolvedFuncSigs.items[rFuncSigId];
    try writeResolvedFuncSigTypesStr(c, w, rFuncSig.params(), rFuncSig.retSymId);
}

pub fn allocResolvedFuncSigTypesStr(c: *cy.VMcompiler, params: []const TypeId, ret: TypeId) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(c.alloc);

    const w = buf.writer(c.alloc);
    try writeResolvedFuncSigTypesStr(c, w, params, ret);
    return buf.toOwnedSlice(c.alloc);
}
pub fn writeResolvedFuncSigTypesStr(c: *cy.VMcompiler, w: anytype, params: []const TypeId, ret: TypeId) !void {
    try w.writeAll("(");

    if (params.len > 0) {
        var rParamSym = c.sema.getResolvedSym(params[0]);
        var name = getName(c, rParamSym.key.absResolvedSymKey.nameId);
        try w.writeAll(name);

        if (params.len > 1) {
            for (params[1..]) |rParamSymId| {
                try w.writeAll(", ");
                rParamSym = c.sema.getResolvedSym(rParamSymId);
                name = getName(c, rParamSym.key.absResolvedSymKey.nameId);
                try w.writeAll(name);
            }
        }
    }
    try w.writeAll(") ");

    var rRetSym = c.sema.getResolvedSym(ret);
    var name = getName(c, rRetSym.key.absResolvedSymKey.nameId);
    try w.writeAll(name);
}

pub const CompactResolvedSymId = packed struct {
    id: u31,
    isFuncSymId: bool,

    pub fn initNull() CompactResolvedSymId {
        return @bitCast(CompactResolvedSymId, @as(u32, cy.NullId));
    }

    pub fn initSymId(id: ResolvedSymId) CompactResolvedSymId {
        return .{
            .id = @intCast(u31, id),
            .isFuncSymId = false,
        };
    }

    pub fn initFuncSymId(id: ResolvedFuncSymId) CompactResolvedSymId {
        return .{
            .id = @intCast(u31, id),
            .isFuncSymId = true,
        };
    }

    fn isNull(self: CompactResolvedSymId) bool {
        return @bitCast(u32, self) == cy.NullId;
    }

    pub fn isPresent(self: CompactResolvedSymId) bool {
        return @bitCast(u32, self) != cy.NullId;
    }
};

fn checkTypeSym(c: *cy.Chunk, rSymId: ResolvedSymId, nameId: NameSymId) !void {
    const rSym = c.compiler.sema.getResolvedSym(rSymId);
    if (rSym.symT == .object) {
        return;
    } else if (rSym.symT == .builtinType) {
        return;
    } else {
        const name = getName(c.compiler, nameId);
        return c.reportError("`{}` is not a type symbol.", &.{v(name)});
    }
}

fn getOrResolveTypeSym(chunk: *cy.Chunk, rParentSymId: ResolvedSymId, nameId: NameSymId) !ResolvedSymId {
    // Check builtin types.
    if (rParentSymId == chunk.semaResolvedRootSymId) {
        if (nameId < NameBuiltinTypeEnd) {
            return @intCast(ResolvedSymId, nameId);
        }
    }

    var key: cy.hash.KeyU64 = undefined;
    if (rParentSymId == chunk.semaResolvedRootSymId) {
        // Faster check against local syms.
        key = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (chunk.localSyms.get(key)) |sym| {
            if (sym.rSymId != cy.NullId) {
                try checkTypeSym(chunk, sym.rSymId, nameId);
                return sym.rSymId;
            } else {
                // Unresolved.
                const res = try resolveDistinctLocalSym(chunk, key);
                try checkTypeSym(chunk, res.rSymId, nameId);
                return res.rSymId;
            }
        }
    }

    key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
            .nameId = nameId,
        },
    };

    var rSymId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;
    if (rSymId != cy.NullId) {
        try checkTypeSym(chunk, rSymId, nameId);
        return rSymId;
    }

    const parentSym = chunk.compiler.sema.resolvedSyms.items[rParentSymId];
    if (parentSym.symT == .module) {
        const modId = parentSym.inner.module.id;
        if (try resolveTypeSymFromModule(chunk, modId, nameId)) |resSymId| {
            return resSymId;
        }
    }
    const name = getName(chunk.compiler, nameId);
    return chunk.reportError("Could not find type symbol `{}`.", &.{v(name)});
}

fn mustGetOrResolveDistinctSym(chunk: *cy.Chunk, rParentSymId: ResolvedSymId, nameId: NameSymId) !ResolvedSymResult {
    return try getOrResolveDistinctSym(chunk, rParentSymId, nameId) orelse {
        const name = getName(chunk.compiler, nameId);
        return chunk.reportError("Can not find the symbol `{}`.", &.{v(name)});
    };
}

fn resolveDistinctLocalSym(chunk: *cy.Chunk, lkey: RelLocalSymKey) !ResolvedSymResult {
    const sym = chunk.localSyms.getPtr(lkey).?;

    // First check resolved syms.
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = sym.rParentSymId,
            .nameId = lkey.relLocalSymKey.nameId,
        },
    };
    if (chunk.compiler.sema.resolvedSymMap.get(key)) |rSymId| {
        const rSym = chunk.compiler.sema.getResolvedSym(rSymId);
        if (rSym.symT == .func) {
            if (rSym.inner.func.rFuncSymId == cy.NullId) {
                const name = getName(chunk.compiler, lkey.relLocalSymKey.nameId);
                return chunk.reportError("Can not disambiguate the symbol `{}`.", &.{v(name)});
            } else {
                sym.rSymId = rSymId;
                sym.rFuncSymId = rSym.inner.func.rFuncSymId;
                return ResolvedSymResult{
                    .rSymId = sym.rSymId,
                    .rFuncSymId = sym.rFuncSymId,
                };
            }
        } else {
            sym.rSymId = rSymId;
            sym.rFuncSymId = cy.NullId;
        }
    } else {
        const rParentSym = chunk.compiler.sema.getResolvedSym(sym.rParentSymId);
        if (rParentSym.symT == .module) {
            const crSymId = (try resolveSymFromModule(chunk, rParentSym.inner.module.id, lkey.relLocalSymKey.nameId, cy.NullId)).?;
            if (crSymId.isFuncSymId) {
                const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(crSymId.id);
                sym.rSymId = rFuncSym.key.absResolvedFuncSymKey.rSymId;
                sym.rFuncSymId = crSymId.id;
            } else {
                sym.rSymId = crSymId.id;
                sym.rFuncSymId = cy.NullId;
            }
        } else {
            stdx.fatal();
        }
    }
    return ResolvedSymResult{
        .rSymId = sym.rSymId,
        .rFuncSymId = sym.rFuncSymId,
    };
}

/// TODO: This should perform type checking.
fn getOrResolveDistinctSym(chunk: *cy.Chunk, rParentSymId: ResolvedSymId, nameId: NameSymId) !?ResolvedSymResult {
    log.debug("getDistinctSym {}.{s}", .{rParentSymId, getName(chunk.compiler, nameId)} );
    var key: cy.hash.KeyU64 = undefined;
    if (rParentSymId == chunk.semaResolvedRootSymId) {
        // Faster check against local syms.
        key = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (chunk.localSyms.get(key)) |sym| {
            if (sym.rSymId != cy.NullId) {
                return ResolvedSymResult{
                    .rSymId = sym.rSymId,
                    .rFuncSymId = sym.rFuncSymId,
                };
            } else {
                // Unresolved.
                const res = try resolveDistinctLocalSym(chunk, key);
                return ResolvedSymResult{
                    .rSymId = res.rSymId,
                    .rFuncSymId = res.rFuncSymId,
                };
            }
        }
    }

    key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
            .nameId = nameId,
        },
    };

    var rSymId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;
    if (rSymId != cy.NullId) {
        const rSym = chunk.compiler.sema.resolvedSyms.items[rSymId];
        if (rSym.symT == .func) {
            if (rSym.inner.func.rFuncSymId == cy.NullId) {
                const name = getName(chunk.compiler, nameId);
                return chunk.reportError("Can not disambiguate the symbol `{}`.", &.{v(name)});
            } else {
                return ResolvedSymResult{
                    .rSymId = rSymId,
                    .rFuncSymId = rSym.inner.func.rFuncSymId,
                };
            }
        } else {
            return ResolvedSymResult{
                .rSymId = rSymId,
                .rFuncSymId = cy.NullId,
            };
        }
    }
            
    const parentSym = chunk.compiler.sema.getResolvedSym(rParentSymId);
    if (parentSym.getModuleId()) |modId| {
        if (try cy.module.findDistinctModuleSym(chunk, modId, nameId)) {
            const crSymId = (try resolveSymFromModule(chunk, modId, nameId, cy.NullId)).?;
            if (crSymId.isFuncSymId) {
                const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(crSymId.id);
                return ResolvedSymResult{
                    .rSymId = rFuncSym.key.absResolvedFuncSymKey.rSymId,
                    .rFuncSymId = crSymId.id,
                };
            } else {
                return ResolvedSymResult{
                    .rSymId = crSymId.id,
                    .rFuncSymId = cy.NullId,
                };
            }
        } else {
            // Check builtin types.
            if (rParentSymId == chunk.semaResolvedRootSymId) {
                if (nameId < NameBuiltinTypeEnd) {
                    return ResolvedSymResult{
                        .rSymId = @intCast(ResolvedSymId, nameId),
                        .rFuncSymId = cy.NullId,
                    };
                }
            }

            // Report missing symbol when looking in a module.
            const name = getName(chunk.compiler, nameId);
            return chunk.reportError("Missing symbol: `{}`", &.{v(name)});
        }
    }

    return null;
}

// pub const FuncCandidate = struct {
//     parentSymId: ResolvedSymId,
//     funcSigId: ResolvedFuncSigId,
// };

// /// Query for a list of potential func sigs by sym path and number of params.
// fn pushFuncCandidates(
//     c: *cy.Chunk, rParentSymId: ResolvedSymId, nameId: NameSymId, numParams: u32
// ) !stdx.IndexSlice(u32) {
//     var key: cy.hash.KeyU64 = undefined;

//     var effParentSymId = rParentSymId;

//     // Check for a using symbol or builtin sym.
//     if (rParentSymId == c.semaResolvedRootSymId) {
//         key = RelLocalSymKey{
//             .relLocalSymKey = .{
//                 .nameId = nameId,
//                 .rFuncSigId = cy.NullId,
//             },
//         };
//         if (c.localSyms.get(key)) |sym| {
//             effParentSymId = sym.rParentSymId;
//         } else {
//             if (nameId < types.BuiltinTypeTags.len) {
//                 effParentSymId = cy.NullId;
//             }
//         }
//     }

//     const start = c.funcCandidateStack.items.len;

//     const parentSym = c.compiler.sema.getResolvedSym(effParentSymId);
//     if (parentSym.getModuleId()) |modId| {
//         try pushModuleFuncCandidates(c, modId, nameId, numParams);
//     }

//     // Look for <call> magic function.
//     key = AbsResolvedSymKey{
//         .absResolvedSymKey = .{
//             .rParentSymId = effParentSymId,
//             .nameId = nameId,
//         },
//     };
//     if (c.compiler.sema.resolvedSymMap.get(key)) |rSymId| {
//         const sym = c.compiler.sema.getResolvedSym(rSymId);
//         if (sym.getModuleId()) |modId| {
//             const callNameId = try ensureNameSym(c.compiler, "<call>");
//             try pushModuleFuncCandidates(c, modId, callNameId, numParams);
//         }
//     }

//     return stdx.IndexSlice(u32).init(
//         @intCast(u32, start),
//         @intCast(u32, c.funcCandidateStack.items.len)
//     );
// }

const DynamicFuncCallSymResult = struct {
    crSymId: CompactResolvedSymId,
    funcSigId: ResolvedFuncSigId,
    retType: TypeId,
    overloaded: bool,
};

fn getOrResolveSymForDynamicFuncCall(
    chunk: *cy.Chunk, rParentSymId: ResolvedSymId, nameId: NameSymId, args: []const TypeId,
) !?DynamicFuncCallSymResult {
    var rParentSymIdFinal = rParentSymId;

    var key: cy.hash.KeyU64 = undefined;
    if (rParentSymId == chunk.semaResolvedRootSymId) {
        key = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (chunk.localSyms.get(key)) |sym| {
            rParentSymIdFinal = sym.rParentSymId;
        } else {
            if (nameId < NameBuiltinTypeEnd) {
                rParentSymIdFinal = cy.NullId;
            }
        }
    }

    key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymIdFinal,
            .nameId = nameId,
        },
    };
    const rSymId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;

    if (rParentSymIdFinal != cy.NullId) {
        const parentSym = chunk.compiler.sema.getResolvedSym(rParentSymIdFinal);
        const modId = parentSym.getModuleId() orelse return null;

        if (try cy.module.findModuleSymForDynamicFuncCall(chunk, modId, nameId)) |mod_rFuncSigId| {
            if (rSymId != cy.NullId) {
                key = AbsResolvedFuncSymKey{
                    .absResolvedFuncSymKey = .{
                        .rSymId = rSymId,
                        .rFuncSigId = mod_rFuncSigId,
                    },
                };
                if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |rFuncSymId| {
                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(rFuncSymId);
                    return DynamicFuncCallSymResult{
                        .crSymId = CompactResolvedSymId.initFuncSymId(rFuncSymId),
                        .funcSigId = mod_rFuncSigId,
                        .retType = rFuncSym.retType,
                        .overloaded = false,
                    };
                }
            }

            if (try resolveSymFromModule(chunk, modId, nameId, mod_rFuncSigId)) |crSymId| {
                if (crSymId.isFuncSymId) {
                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(crSymId.id);
                    return DynamicFuncCallSymResult{
                        .crSymId = crSymId,
                        .funcSigId = mod_rFuncSigId,
                        .retType = rFuncSym.retType,
                        .overloaded = false,
                    };
                } else {
                    return DynamicFuncCallSymResult{
                        .crSymId = crSymId,
                        .funcSigId = mod_rFuncSigId,
                        .retType = bt.Any,
                        .overloaded = false,
                    };
                }
            } else {
                stdx.panic("unexpected");
            }
        } else {
            const rFuncSigId = try ensureResolvedFuncSig(chunk.compiler, args, bt.Any);
            try reportIncompatibleFuncSig(chunk, nameId, rFuncSigId, modId);
        }
    }

    // Look for <call> magic function.
    if (rSymId != cy.NullId) {
        const sym = chunk.compiler.sema.getResolvedSym(rSymId);
        if (sym.getModuleId()) |symModId| {
            const callNameId = try ensureNameSym(chunk.compiler, "<call>");
            if (try cy.module.findModuleSymForDynamicFuncCall(chunk, symModId, callNameId)) |funcSigId| {
                // Check if already resolved.
                key = AbsResolvedSymKey{
                    .absResolvedSymKey = .{
                        .rParentSymId = rSymId,
                        .nameId = callNameId,
                    },
                };
                if (chunk.compiler.sema.resolvedSymMap.get(key)) |callSymId| {
                    key = AbsResolvedFuncSymKey{
                        .absResolvedFuncSymKey = .{
                            .rSymId = callSymId,
                            .rFuncSigId = funcSigId,
                        },
                    };
                    if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |rFuncSymId| {
                        const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(rFuncSymId);
                        return DynamicFuncCallSymResult{
                            .crSymId = CompactResolvedSymId.initFuncSymId(rFuncSymId),
                            .retType = rFuncSym.retType,
                            .funcSigId = funcSigId,
                            .overloaded = false,
                        };
                    }
                }

                if (try resolveSymFromModule(chunk, symModId, callNameId, funcSigId)) |crSymId| {
                    std.debug.assert(crSymId.isFuncSymId);

                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(crSymId.id);
                    return DynamicFuncCallSymResult{
                        .crSymId = crSymId,
                        .retType = rFuncSym.retType,
                        .funcSigId = funcSigId,
                        .overloaded = false,
                    };
                } else {
                    stdx.panic("unexpected");
                }
            }
        }
    }

    return null;
}

const FuncCallSymResult = struct {
    crSymId: CompactResolvedSymId,
    funcSigId: ResolvedFuncSigId,
    retType: TypeId,
};

/// Assumes rParentSymId is not null.
/// Returns CompileError if the symbol exists but can't be used for a function call.
/// Returns null if the symbol is missing but can still be used as an accessor.
fn getOrResolveSymForFuncCall(
    chunk: *cy.Chunk, rParentSymId: ResolvedSymId, nameId: NameSymId, args: []const TypeId, ret: TypeId
) !?FuncCallSymResult {
    const rFuncSigId = try ensureResolvedFuncSig(chunk.compiler, args, ret);
    log.debug("getFuncCallSym {}.{s}, sig: {s}", .{rParentSymId, getName(chunk.compiler, nameId), try getResolvedFuncSigTempStr(chunk.compiler, rFuncSigId)} );

    // TODO: Cache lookup by rSymId and rFuncSigId.

    var rParentSymIdFinal = rParentSymId;

    var key: cy.hash.KeyU64 = undefined;
    if (rParentSymId == chunk.semaResolvedRootSymId) {
        key = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (chunk.localSyms.get(key)) |sym| {
            rParentSymIdFinal = sym.rParentSymId;
        } else {
            if (nameId < NameBuiltinTypeEnd) {
                rParentSymIdFinal = cy.NullId;
            }
        }
    }

    key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymIdFinal,
            .nameId = nameId,
        },
    };

    var rSymId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;
    if (rSymId != cy.NullId) {
        const sym = chunk.compiler.sema.getResolvedSym(rSymId);
        switch (sym.symT) {
            .variable => {
                // TODO: Check var type.
                return FuncCallSymResult{
                    .crSymId = CompactResolvedSymId.initSymId(rSymId),
                    .funcSigId = rFuncSigId,
                    .retType = bt.Any,
                };
            },
            .builtinType,
            .func => {
                // Match against exact signature.
                key = AbsResolvedFuncSymKey{
                    .absResolvedFuncSymKey = .{
                        .rSymId = rSymId,
                        .rFuncSigId = rFuncSigId,
                    },
                };
                if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |rFuncSymId| {
                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(rFuncSymId);
                    return FuncCallSymResult{
                        .crSymId = CompactResolvedSymId.initFuncSymId(rFuncSymId),
                        .retType = rFuncSym.retType,
                        .funcSigId = rFuncSigId,
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

    if (rParentSymIdFinal != cy.NullId) {
        const parentSym = chunk.compiler.sema.getResolvedSym(rParentSymIdFinal);
        const modId = parentSym.getModuleId() orelse return null;

        if (try cy.module.findModuleSymForFuncCall(chunk, modId, nameId, args, ret)) |mod_rFuncSigId| {
            if (rSymId != cy.NullId) {
                key = AbsResolvedFuncSymKey{
                    .absResolvedFuncSymKey = .{
                        .rSymId = rSymId,
                        .rFuncSigId = mod_rFuncSigId,
                    },
                };
                if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |rFuncSymId| {
                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(rFuncSymId);
                    return FuncCallSymResult{
                        .crSymId = CompactResolvedSymId.initFuncSymId(rFuncSymId),
                        .retType = rFuncSym.retType,
                        .funcSigId = mod_rFuncSigId,
                    };
                }
            }

            if (try resolveSymFromModule(chunk, modId, nameId, mod_rFuncSigId)) |crSymId| {
                if (crSymId.isFuncSymId) {
                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(crSymId.id);
                    return FuncCallSymResult{
                        .crSymId = crSymId,
                        .retType = rFuncSym.retType,
                        .funcSigId = mod_rFuncSigId,
                    };
                } else {
                    return FuncCallSymResult{
                        .crSymId = crSymId,
                        .retType = bt.Any,
                        .funcSigId = mod_rFuncSigId,
                    };
                }
            } else {
                stdx.panic("unexpected");
            }
        } else {
            try reportIncompatibleFuncSig(chunk, nameId, rFuncSigId, modId);
        }
    }

    // Look for <call> magic function.
    if (rSymId != cy.NullId) {
        const sym = chunk.compiler.sema.getResolvedSym(rSymId);
        if (sym.getModuleId()) |symModId| {
            const callNameId = try ensureNameSym(chunk.compiler, "<call>");
            if (try cy.module.findModuleSymForFuncCall(chunk, symModId, callNameId, args, ret)) |funcSigId| {
                // Check if already resolved.
                key = AbsResolvedSymKey{
                    .absResolvedSymKey = .{
                        .rParentSymId = rSymId,
                        .nameId = callNameId,
                    },
                };
                if (chunk.compiler.sema.resolvedSymMap.get(key)) |callSymId| {
                    key = AbsResolvedFuncSymKey{
                        .absResolvedFuncSymKey = .{
                            .rSymId = callSymId,
                            .rFuncSigId = funcSigId,
                        },
                    };
                    if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |rFuncSymId| {
                        const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(rFuncSymId);
                        return FuncCallSymResult{
                            .crSymId = CompactResolvedSymId.initFuncSymId(rFuncSymId),
                            .retType = rFuncSym.retType,
                            .funcSigId = funcSigId,
                        };
                    }
                }

                if (try resolveSymFromModule(chunk, symModId, callNameId, funcSigId)) |crSymId| {
                    std.debug.assert(crSymId.isFuncSymId);

                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(crSymId.id);
                    return FuncCallSymResult{
                        .crSymId = crSymId,
                        .retType = rFuncSym.retType,
                        .funcSigId = funcSigId,
                    };
                } else {
                    stdx.panic("unexpected");
                }
            }
        }
    }
    return null;
}

fn reportIncompatibleFuncSig(c: *cy.Chunk, nameId: NameSymId, rFuncSigId: ResolvedFuncSigId, searchModId: cy.ModuleId) !void {
    const name = getName(c.compiler, nameId);
    const sigStr = try getResolvedFuncSigTempStr(c.compiler, rFuncSigId);

    const modKey = RelModuleSymKey{
        .relModuleSymKey = .{
            .nameId = nameId,
            .rFuncSigId = cy.NullId,
        },
    };
    const mod = c.compiler.sema.getModule(searchModId);
    if (mod.syms.get(modKey)) |modSym| {
        if (modSym.symT == .symToOneFunc) {
            const existingSigStr = try allocResolvedFuncSigStr(c.compiler, modSym.inner.symToOneFunc.rFuncSigId);
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

fn getResolvedSymRootMod(c: *cy.VMcompiler, id: ResolvedSymId) cy.ModuleId {
    const rsym = c.sema.resolvedSyms.items[id];
    if (rsym.key.absResolvedSymKey.rParentSymId == cy.NullId) {
        return rsym.inner.module.id;
    } else {
        return getResolvedSymRootMod(c, rsym.key.absResolvedSymKey.rParentSymId);
    }
}

fn getAndCheckResolvedTypeSym(c: *cy.Chunk, key: AbsResolvedSymKey, nodeId: cy.NodeId) !?ResolvedSymId {
    if (c.compiler.semaResolvedSymMap.get(key)) |id| {
        const rsym = c.compiler.sema.resolvedSyms.items[id];
        if (rsym.symT == .object) {
            return id;
        } else {
            return c.reportErrorAt("`{}` does not refer to a type.", &.{v(getName(c.compiler, key.absResolvedSymKey.nameId))}, nodeId);
        }
    } else return null;
}

/// Get the resolved sym that matches a signature.
fn getAndCheckResolvedSymBySig(c: *cy.Chunk, key: AbsResolvedSymKey, rFuncSigId: ResolvedFuncSigId, nodeId: cy.NodeId) !?ResolvedSymId {
    if (c.compiler.semaResolvedSymMap.get(key)) |id| {
        const rsym = c.compiler.sema.resolvedSyms.items[id];
        if (rFuncSigId == cy.NullId) {
            // Searching for a non-func reference.
            if (rsym.symT == .func) {
                if (rsym.inner.func.rFuncSymId != cy.NullId) {
                    // When the signature is for a non-func reference,
                    // a non overloaded function symbol can be used.
                    return id;
                } else {
                    return c.reportErrorAt("Can not disambiguate the symbol `{}`.", &.{v(getName(c.compiler, key.absResolvedSymKey.nameId))}, nodeId);
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
                const funcKey = AbsResolvedFuncSymKey{
                    .absResolvedFuncSymKey = .{
                        .rSymId = id,
                        .rFuncSigId = rFuncSigId,
                    },
                };
                if (c.compiler.semaResolvedFuncSymMap.contains(funcKey)) {
                    return id;
                } else {
                    return null;
                }
            } else {
                return c.reportErrorAt("Can not use `{}` as a function reference.", &.{v(getName(c.compiler, key.absResolvedSymKey.nameId))}, nodeId);
            }
        }
    } else return null;
}

fn resolveTypeSymFromModule(chunk: *cy.Chunk, modId: cy.ModuleId, nameId: NameSymId) anyerror!?ResolvedSymId {
    const self = chunk.compiler;
    const relKey = RelModuleSymKey{
        .relModuleSymKey = .{
            .nameId = nameId,
            .rFuncSigId = cy.NullId,
        },
    };

    const mod = self.sema.modules.items[modId];
    if (mod.syms.get(relKey)) |modSym| {
        const key = AbsResolvedSymKey{
            .absResolvedSymKey = .{
                .rParentSymId = mod.resolvedRootSymId,
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
                const rSymId = try getOrResolveTypeSymFromSpecNode(srcChunk, node.head.typeAliasDecl.typeSpecHead);
                return rSymId;
            },
            else => {},
        }
    }
    return null;
}

/// If the name symbol points to only one function, the function sym is returned.
fn resolveSymFromModule(chunk: *cy.Chunk, modId: cy.ModuleId, nameId: NameSymId, rFuncSigId: ResolvedFuncSigId) anyerror!?CompactResolvedSymId {
    const self = chunk.compiler;
    const relKey = RelModuleSymKey{
        .relModuleSymKey = .{
            .nameId = nameId,
            .rFuncSigId = rFuncSigId,
        },
    };

    const mod = self.sema.modules.items[modId];
    if (mod.syms.get(relKey)) |modSym| {
        const key = AbsResolvedSymKey{
            .absResolvedSymKey = .{
                .rParentSymId = mod.resolvedRootSymId,
                .nameId = nameId,
            },
        };

        switch (modSym.symT) {
            .nativeFunc1 => {
                const res = try resolveNativeFunc(chunk, key, rFuncSigId, modSym.inner.nativeFunc1.func);
                return CompactResolvedSymId.initFuncSymId(res.rFuncSymId);
            },
            .variable => {
                const rtSymId = try self.vm.ensureVarSym(mod.resolvedRootSymId, nameId);
                const rtSym = rt.VarSym.init(modSym.inner.variable.val);
                cy.arc.retain(self.vm, rtSym.value);
                self.vm.setVarSym(rtSymId, rtSym);
                const id = try self.sema.addResolvedSym(key, .variable, .{
                    .variable = .{
                        .chunkId = cy.NullId,
                        .declId = cy.NullId,
                        .rTypeSymId = modSym.extra.variable.rTypeSymId,
                    },
                });
                return CompactResolvedSymId.initSymId(id);
            },
            .userVar => {
                _ = try self.vm.ensureVarSym(mod.resolvedRootSymId, nameId);

                const id = try self.sema.addResolvedSym(key, .variable, .{
                    .variable = .{
                        .chunkId = self.sema.modules.items[modId].chunkId,
                        .declId = modSym.inner.userVar.declId,
                        .rTypeSymId = bt.Any,
                    },
                });
                return CompactResolvedSymId.initSymId(id);
            },
            .userFunc => {
                const res = try resolveUserFunc(chunk, key, rFuncSigId,
                    modSym.inner.userFunc.declId, modSym.inner.userFunc.hasStaticInitializer);
                return CompactResolvedSymId.initFuncSymId(res.rFuncSymId);
            },
            .object => {
                const id = try resolveObjectSym(chunk.compiler, key, modSym.inner.object.modId);
                return CompactResolvedSymId.initSymId(id);
            },
            .enumType => {
                const id = try self.sema.addResolvedSym(key, .enumType, .{
                    .enumType = .{
                        .modId = modSym.inner.enumType.modId,
                        .enumId = modSym.inner.enumType.rtEnumId,
                    },
                });
                const enumMod = self.sema.getModulePtr(modSym.inner.enumType.modId);
                enumMod.resolvedRootSymId = id;
                return CompactResolvedSymId.initSymId(id);
            },
            .enumMember => {
                const id = try self.sema.addResolvedSym(key, .enumMember, .{
                    .enumMember = .{
                        .enumId = modSym.inner.enumMember.rtEnumId,
                        .memberId = modSym.inner.enumMember.memberId,
                    },
                });
                return CompactResolvedSymId.initSymId(id);
            },
            .typeAlias => {
                const srcChunk = &chunk.compiler.chunks.items[mod.chunkId];
                const node = srcChunk.nodes[modSym.inner.typeAlias.declId];
                const rSymId = try getOrResolveTypeSymFromSpecNode(srcChunk, node.head.typeAliasDecl.typeSpecHead);
                return CompactResolvedSymId.initSymId(rSymId);
            },
            .symToManyFuncs => {
                // More than one func for sym.
                const name = getName(chunk.compiler, nameId);
                return chunk.reportError("Symbol `{}` is ambiguous. There are multiple functions with the same name.", &.{v(name)});
            },
            .symToOneFunc => {
                const sigId = modSym.inner.symToOneFunc.rFuncSigId;
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
    const res = try @call(.never_inline, c.sema.nameSymMap.getOrPut, .{c.alloc, name});
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        const id = @intCast(u32, c.sema.nameSyms.items.len);
        if (dupe) {
            const new = try c.alloc.dupe(u8, name);
            try c.sema.nameSyms.append(c.alloc, .{
                .ptr = new.ptr,
                .len = @intCast(u32, new.len),
                .owned = true,
            });
            res.key_ptr.* = new;
        } else {
            try c.sema.nameSyms.append(c.alloc, .{
                .ptr = name.ptr,
                .len = @intCast(u32, name.len),
                .owned = false,
            });
        }
        res.value_ptr.* = id;
        return id;
    }
}

/// TODO: This should also return true for local function symbols.
fn hasResolvedSym(self: *const cy.Chunk, rParentSymId: ResolvedSymId, nameId: NameSymId) bool {
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
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
    const sym = c.compiler.sema.getResolvedSym(ltype);
    if (sym.symT == .object) {
        const typeId = sym.inner.object.typeId;
        const rtFieldId = try c.compiler.vm.ensureFieldSym(rightName);
        var offset: u8 = undefined;
        const symMap = c.compiler.vm.fieldSyms.buf[rtFieldId];
        if (typeId == symMap.mruTypeId) {
            offset = @intCast(u8, symMap.mruOffset);
        } else {
            offset = @call(.never_inline, c.compiler.vm.getFieldOffsetFromTable, .{typeId, rtFieldId});
        }
        if (offset == cy.NullU8) {
            const name = c.compiler.vm.types.buf[typeId].name;
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
            const res = try getOrLookupVar(self, name, .read);

            const rightName = self.getNodeTokenString(right);
            const rightNameId = try ensureNameSym(self.compiler, rightName);
            if (!res.isLocal) {
                // Static symbol.
                const leftSymRes = try mustGetOrResolveDistinctSym(self, self.semaResolvedRootSymId, nameId);
                const crLeftSym = leftSymRes.toCompactId();
                try referenceSym(self, crLeftSym, true);
                self.nodes[node.head.accessExpr.left].head.ident.sema_crSymId = crLeftSym;

                self.curNodeId = node.head.accessExpr.right;
                if (try getOrResolveDistinctSym(self, leftSymRes.rSymId, rightNameId)) |symRes| {
                    const crRightSym = symRes.toCompactId();
                    try referenceSym(self, crRightSym, true);
                    self.nodes[nodeId].head.accessExpr.sema_crSymId = crRightSym;

                    const exprT = try getTypeForResolvedValueSym(self, crRightSym);
                    return AccessExprResult{
                        .recvT = getSymType(self.compiler, crLeftSym.id),
                        .exprT = exprT,
                    };
                } else {
                    const leftSym = self.compiler.sema.getResolvedSym(leftSymRes.rSymId);
                    if (leftSym.symT == .variable) {
                        const vtype = leftSym.inner.variable.rTypeSymId;
                        return getAccessExprResult(self, vtype, rightName);
                    }
                }
            } else {
                self.nodes[node.head.accessExpr.left].head.ident.semaVarId = res.id;
                const svar = self.vars.items[res.id];
                self.nodeTypes[node.head.accessExpr.left] = svar.vtype;
                return getAccessExprResult(self, svar.vtype, rightName);
            }
        } else if (left.node_t == .accessExpr) {
            const res = try accessExpr(self, node.head.accessExpr.left);

            left = self.nodes[node.head.accessExpr.left];
            if (left.head.accessExpr.sema_crSymId.isPresent()) {
                // Static var.
                const crLeftSym = left.head.accessExpr.sema_crSymId;
                if (!crLeftSym.isFuncSymId) {
                    const rightName = self.getNodeTokenString(right);
                    const rightNameId = try ensureNameSym(self.compiler, rightName);
                    if (try getOrResolveDistinctSym(self, crLeftSym.id, rightNameId)) |rightSym| {
                        const crRightSym = rightSym.toCompactId();
                        try referenceSym(self, crRightSym, true);
                        self.nodes[nodeId].head.accessExpr.sema_crSymId = crRightSym;
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
    stdx.debug.dassert(vtype != bt.NumberLit);
    return vtype;
}

fn assignVar(self: *cy.Chunk, ident: cy.NodeId, vtype: TypeId, strat: VarLookupStrategy) !void {
    // log.debug("set var {s}", .{name});
    const node = self.nodes[ident];
    const name = self.getNodeTokenString(node);

    const res = try getOrLookupVar(self, name, strat);
    if (res.isLocal) {
        const svar = &self.vars.items[res.id];
        if (svar.isCaptured()) {
            if (!svar.isBoxed) {
                // Becomes boxed so codegen knows ahead of time.
                svar.isBoxed = true;
            }
        }

        if (!res.created) {
            const ssblock = curSubBlock(self);
            if (!ssblock.prevVarTypes.contains(res.id)) {
                // Same variable but branched to sub block.
                try ssblock.prevVarTypes.put(self.alloc, res.id, svar.vtype);
            }
        }

        // Update current type after checking for branched assignment.
        if (!types.isSameType(svar.vtype, vtype)) {
            svar.vtype = toLocalType(vtype);
            if (!svar.lifetimeRcCandidate and types.isRcCandidateType(self.compiler, vtype)) {
                svar.lifetimeRcCandidate = true;
            }
        }

        try self.assignedVarStack.append(self.alloc, res.id);
        self.nodes[ident].head.ident.semaVarId = res.id;
    } else {
        const nameId = try ensureNameSym(self.compiler, name);
        const symRes = try mustGetOrResolveDistinctSym(self, self.semaResolvedRootSymId, nameId);
        const crSymId = symRes.toCompactId();
        try referenceSym(self, crSymId, true);
        self.nodes[ident].head.ident.sema_crSymId = crSymId;
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

pub fn importAllFromModule(self: *cy.Chunk, modId: cy.ModuleId) !void {
    const mod = self.compiler.sema.modules.items[modId];
    var iter = mod.syms.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*.relModuleSymKey;
        const modSym = entry.value_ptr.*;
        switch (modSym.symT) {
            .variable,
            .symToManyFuncs,
            .symToOneFunc => {
                try setLocalSym(self, key.nameId, .{
                    .rSymId = cy.NullId,
                    .rFuncSymId = cy.NullId,
                    .rParentSymId = mod.resolvedRootSymId,
                });
            },
            .nativeFunc1 => {
                // Skip exact func syms.
            },
            else => {
                stdx.panicFmt("Unsupported {}", .{modSym.symT});
            },
        }
    }
}

/// Writes resolved spec to temp buf.
fn resolveSpecTemp(self: *cy.Chunk, spec: []const u8, outBuiltin: *bool) ![]const u8 {
    if (self.compiler.moduleLoaders.contains(spec)) {
        outBuiltin.* = true;
        return spec;
    }

    if (cy.isWasm) {
        return error.NotSupported;
    }

    if (std.mem.startsWith(u8, spec, "http://") or std.mem.startsWith(u8, spec, "https://")) {
        outBuiltin.* = false;
        const uri = try std.Uri.parse(spec);
        if (std.mem.endsWith(u8, uri.host.?, "github.com")) {
            if (std.mem.count(u8, uri.path, "/") == 2 and uri.path[uri.path.len-1] != '/') {
                self.tempBufU8.clearRetainingCapacity();
                try self.tempBufU8.appendSlice(self.alloc, uri.scheme);
                try self.tempBufU8.appendSlice(self.alloc, "://raw.githubusercontent.com");
                try self.tempBufU8.appendSlice(self.alloc, uri.path);
                try self.tempBufU8.appendSlice(self.alloc, "/master/mod.cy");
                std.debug.print("{s}\n", .{self.tempBufU8.items});
                return self.tempBufU8.items;
            }
        }
        return spec;
    }

    self.tempBufU8.clearRetainingCapacity();

    // Create path from the current script.
    // There should always be a parent directory since `srcUri` should be absolute when dealing with file modules.
    const dir = std.fs.path.dirname(self.srcUri) orelse return error.NoParentDir;
    try self.tempBufU8.ensureTotalCapacity(self.alloc, dir.len + 1 + spec.len + std.fs.MAX_PATH_BYTES);
    try self.tempBufU8.appendSlice(self.alloc, dir);
    try self.tempBufU8.append(self.alloc, '/');
    try self.tempBufU8.appendSlice(self.alloc, spec);
    const path = self.tempBufU8.items;

    // Get canonical path.
    self.tempBufU8.items.len += std.fs.MAX_PATH_BYTES;
    outBuiltin.* = false;
    return std.fs.cwd().realpath(path, self.tempBufU8.items[path.len..]) catch |err| {
        if (err == error.FileNotFound) {
            return self.reportError("Import path does not exist: `{}`", &.{v(path)});
        } else {
            return err;
        }
    };
}

pub fn resolveEnumSym(c: *cy.VMcompiler, rParentSymId: ResolvedSymId, name: []const u8, modId: cy.ModuleId) !ResolvedSymId {
    const nameId = try ensureNameSym(c, name);
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
            .nameId = nameId,
        },
    };
    if (c.sema.resolvedSymMap.contains(key)) {
        return error.DuplicateSymName;
    }

    // Resolve the symbol.
    const rSymId = @intCast(u32, c.sema.resolvedSyms.items.len);
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
    try @call(.never_inline, c.sema.resolvedSymMap.put, .{c.alloc, key, rSymId});

    return rSymId;
}

pub fn resolveObjectSym(c: *cy.VMcompiler, key: AbsResolvedSymKey, modId: cy.ModuleId) !ResolvedSymId {
    const rtTypeId = try c.vm.ensureObjectType(key.absResolvedSymKey.rParentSymId, key.absResolvedSymKey.nameId, cy.NullId);
    const symId = try c.sema.addResolvedSym(key, .object, .{
        .object = .{
            .modId = modId,
            .typeId = rtTypeId,
        },
    });
    try @call(.never_inline, c.sema.resolvedSymMap.put, .{c.alloc, key, symId});

    c.vm.types.buf[rtTypeId].rTypeSymId = symId;
    const mod = c.sema.getModulePtr(modId);
    mod.resolvedRootSymId = symId;
    return symId;
}

/// A root module symbol is used as the parent for it's members.
pub fn resolveRootModuleSym(self: *cy.VMcompiler, name: []const u8, modId: cy.ModuleId) !ResolvedSymId {
    const nameId = try ensureNameSym(self, name);
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = cy.NullId,
            .nameId = nameId,
        },
    };
    if (self.sema.resolvedSymMap.contains(key)) {
        // Assume no existing symbol, since each module has a unique srcUri.
        log.debug("Root symbol {s} already exists.", .{name});
        stdx.fatal();
    }

    // Resolve the symbol.
    const resolvedId = @intCast(u32, self.sema.resolvedSyms.items.len);
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
    try @call(.never_inline, self.sema.resolvedSymMap.put, .{self.alloc, key, resolvedId});

    return resolvedId;
}

/// Given the local sym path, add a resolved var sym entry.
/// Fail if there is already a symbol in this path with the same name.
fn resolveLocalVarSym(self: *cy.Chunk, rParentSymId: ResolvedSymId, nameId: NameSymId, typeSymId: ResolvedSymId, declId: cy.NodeId, exported: bool) !ResolvedSymId {
    if (rParentSymId == self.semaResolvedRootSymId) {
        // Check for local sym.
        const key = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (self.localSyms.contains(key)) {
            const node = self.nodes[declId];
            const varSpec = self.nodes[node.head.varDecl.varSpec];
            return self.reportErrorAt("The symbol `{}` was already declared.", &.{v(getName(self.compiler, nameId))}, varSpec.head.varSpec.name);
        }
    }

    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
            .nameId = nameId,
        },
    };

    if (self.compiler.sema.resolvedSymMap.contains(key)) {
        return self.reportErrorAt("The symbol `{}` was already declared.", &.{v(getName(self.compiler, nameId))}, declId);
    }

    // Resolve the symbol.
    const resolvedId = @intCast(u32, self.compiler.sema.resolvedSyms.items.len);
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

    try @call(.never_inline, self.compiler.sema.resolvedSymMap.put, .{self.alloc, key, resolvedId});

    return resolvedId;
}

pub fn getSymType(c: *cy.VMcompiler, id: ResolvedSymId) TypeId {
    const sym = c.sema.getResolvedSym(id);
    switch (sym.symT) {
        .variable => {
            return sym.inner.variable.rTypeSymId;
        },
        .enumType,
        .module => {
            return bt.Any;
        },
        else => {
            stdx.panicFmt("Unsupported sym: {}", .{sym.symT});
        }
    }
}

pub fn getSymName(c: *cy.VMcompiler, id: ResolvedSymId) []const u8 {
    const sym = c.sema.getResolvedSym(id);
    return getName(c, sym.key.absResolvedSymKey.nameId);
}

/// Dump the full path of a resolved sym.
fn allocAbsResolvedSymName(self: *cy.VMcompiler, id: ResolvedSymId) ![]const u8 {
    const sym = self.sema.resolvedSyms.items[id];
    if (sym.key.absResolvedSymKey.rParentSymId != cy.NullId) {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(self.alloc);
        try allocAbsResolvedSymNameR(self, &buf, sym.key.absResolvedSymKey.rParentSymId);
        try buf.append(self.alloc, '.');
        try buf.appendSlice(self.alloc, getName(self, sym.key.absResolvedSymKey.nameId));
        return buf.toOwnedSlice(self.alloc);
    } else {
        const name = getName(self, sym.key.absResolvedSymKey.nameId);
        return self.alloc.dupe(u8, name);
    }
}

fn allocAbsResolvedSymNameR(self: *cy.VMcompiler, buf: *std.ArrayListUnmanaged(u8), id: ResolvedSymId) !void {
    const sym = self.sema.resolvedSyms.items[id];
    if (sym.key.absResolvedSymKey.rParentSymId == cy.NullId) {
        try buf.appendSlice(self.alloc, getName(self, sym.key.absResolvedSymKey.nameId));
    } else {
        try allocAbsResolvedSymNameR(self, buf, sym.key.absResolvedSymKey.rParentSymId);
        try buf.append(self.alloc, '.');
        try buf.appendSlice(self.alloc, getName(self, sym.key.absResolvedSymKey.nameId));
    }
}

const ResolveFuncSymResult = struct {
    rSymId: ResolvedSymId,
    rFuncSymId: ResolvedFuncSymId,
};

const ResolvedSymResult = struct {
    rSymId: ResolvedSymId,
    rFuncSymId: Nullable(ResolvedFuncSymId),

    fn toCompactId(self: ResolvedSymResult) CompactResolvedSymId {
        if (self.rFuncSymId != cy.NullId) {
            return CompactResolvedSymId.initFuncSymId(self.rFuncSymId);
        } else {
            return CompactResolvedSymId.initSymId(self.rSymId);
        }
    }
};

fn resolveNativeFunc(
    c: *cy.Chunk, key: AbsResolvedSymKey, funcSigId: ResolvedFuncSigId, func: cy.NativeFuncPtr,
) !ResolvedSymResult {
    const parentSymId = key.absResolvedSymKey.rParentSymId;
    const nameId = key.absResolvedSymKey.nameId;
    const rtSymId = try c.compiler.vm.ensureFuncSym(parentSymId, nameId, funcSigId);

    const funcSig = c.compiler.sema.getResolvedFuncSig(funcSigId);
    const rtSym = rt.FuncSymbolEntry.initNativeFunc1(func, funcSig.isTyped, funcSig.numParams(), funcSigId);
    c.compiler.vm.setFuncSym(rtSymId, rtSym);
    return try resolveFunc(c, key, funcSigId, cy.NullId);
}

fn resolveUserFunc(
    c: *cy.Chunk, key: AbsResolvedSymKey, funcSigId: ResolvedFuncSigId, declId: FuncDeclId, hasStaticInitializer: bool,
) !ResolvedSymResult {
    // Func sym entry will be updated when the func is generated later.
    const parentSymId = key.absResolvedSymKey.rParentSymId;
    const nameId = key.absResolvedSymKey.nameId;
    _ = try c.compiler.vm.ensureFuncSym(parentSymId, nameId, funcSigId);

    const res = try resolveFunc(c, key, funcSigId, declId);
    if (hasStaticInitializer) {
        c.compiler.sema.resolvedFuncSyms.items[res.rFuncSymId].hasStaticInitializer = true;
    }
    const nodeId = c.semaFuncDecls.items[declId].nodeId;
    const node = c.nodes[nodeId];
    const func = &c.semaFuncDecls.items[node.head.func.semaDeclId];
    func.inner.staticFunc = .{
        .semaFuncSymId = res.rFuncSymId,
        .semaSymId = res.rSymId,
    };
    return res;
}

fn resolveFunc(
    self: *cy.Chunk, key: AbsResolvedSymKey, rFuncSigId: ResolvedFuncSigId, declId: FuncDeclId,
) !ResolvedSymResult {
    const c = self.compiler;
    var rsymId: ResolvedSymId = undefined;
    var createdSym = false;
    if (c.sema.resolvedSymMap.get(key)) |id| {
        const rsym = c.sema.resolvedSyms.items[id];
        if (rsym.symT != .func) {
            // Only fail if the symbol already exists and isn't a function.
            const name = getName(c, key.absResolvedSymKey.nameId);
            return self.reportError("The symbol `{}` was already declared.", &.{v(name)});
        }
        rsymId = id;
    } else {
        rsymId = @intCast(u32, c.sema.resolvedSyms.items.len);
        try c.sema.resolvedSyms.append(c.alloc, .{
            .symT = .func,
            .key = key,
            .inner = .{
                .func = .{
                    .rFuncSymId = undefined,
                },
            },
            .exported = true,
        });
        try @call(.never_inline, c.sema.resolvedSymMap.put, .{c.alloc, key, rsymId});
        createdSym = true;
    }

    // Now check resolved function syms.
    const funcKey = AbsResolvedFuncSymKey{
        .absResolvedFuncSymKey = .{
            .rSymId = rsymId,
            .rFuncSigId = rFuncSigId,
        },
    };
    if (c.sema.resolvedFuncSymMap.contains(funcKey)) {
        const name = getName(c, key.absResolvedSymKey.nameId);
        return self.reportError("The function symbol `{}` with the same signature was already declared.", &.{v(name)});
    }

    const rFuncSig = c.sema.getResolvedFuncSig(rFuncSigId);
    const retSymId = rFuncSig.getRetTypeSymId();

    const rfsymId = @intCast(u32, c.sema.resolvedFuncSyms.items.len);
    try types.assertTypeSym(self, retSymId);
    try c.sema.resolvedFuncSyms.append(c.alloc, .{
        .chunkId = self.id,
        .declId = declId,
        .key = funcKey,
        .retType = retSymId,
        .hasStaticInitializer = false,
    });
    try @call(.never_inline, c.sema.resolvedFuncSymMap.put, .{c.alloc, funcKey, rfsymId});

    if (createdSym) {
        c.sema.resolvedSyms.items[rsymId].inner.func.rFuncSymId = rfsymId;
    } else {
        // Mark sym as overloaded.
        c.sema.resolvedSyms.items[rsymId].inner.func.rFuncSymId = cy.NullId;
    }

    if (builtin.mode == .Debug and cy.verbose) {
        const name = try allocAbsResolvedSymName(c, rsymId);
        defer self.alloc.free(name);
        const sigStr = try getResolvedFuncSigTempStr(self.compiler, rFuncSigId);
        log.debug("resolved static func: func {s}{s}", .{name, sigStr});
    }

    return ResolvedSymResult{
        .rSymId = rsymId,
        .rFuncSymId = rfsymId,
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
    const numCaptured = @intCast(u8, sblock.params.items.len - numParams);
    if (builtin.mode == .Debug and numCaptured > 0) {
        stdx.panicFmt("Captured var in static func.", .{});
    }
    try endBlock(self);
}

const PushCallArgsResult = struct {
    argTypes: []const TypeId,
    hasFlexArg: bool,
    hasDynamicArg: bool,
};

fn updateFlexSelfArgTypes(c: *cy.Chunk, argHead: cy.NodeId, argTypes: []const TypeId, funcSigId: ResolvedFuncSigId) void {
    const funcSig = c.compiler.sema.getResolvedFuncSig(funcSigId);
    updateFlexArgTypes2(c, argHead, argTypes[1..], funcSig.params()[1..]);
}

fn updateFlexArgTypes(c: *cy.Chunk, argHead: cy.NodeId, argTypes: []const TypeId, funcSigId: ResolvedFuncSigId) void {
    const funcSig = c.compiler.sema.getResolvedFuncSig(funcSigId);
    updateFlexArgTypes2(c, argHead, argTypes, funcSig.params());
}

fn updateFlexArgTypes2(c: *cy.Chunk, argHead: cy.NodeId, argTypes: []const TypeId, paramTypes: []const TypeId) void {
    var cur = argHead;
    for (paramTypes, 0..) |param, i| {
        const arg = c.nodes[cur];
        if (types.isFlexibleType(argTypes[i])) {
            switch (argTypes[i]) {
                bt.NumberLit => {
                    if (param == bt.Integer) {
                        c.nodeTypes[cur] = bt.Integer;
                    } else {
                        c.nodeTypes[cur] = bt.Number;
                    }
                },
                else => unreachable,
            }
        }
        cur = arg.next;
    }
}

fn defaultFlexArgTypes(c: *cy.Chunk, argHead: cy.NodeId, argTypes: []const TypeId) void {
    var cur = argHead;
    for (argTypes, 0..) |argT, i| {
        const arg = c.nodes[cur];
        if (types.isFlexibleType(argT)) {
            switch (argTypes[i]) {
                bt.NumberLit => {
                    c.nodeTypes[cur] = bt.Number;
                },
                else => unreachable,
            }
        }
        cur = arg.next;
    }
}

fn pushCallArgs(c: *cy.Chunk, argHead: cy.NodeId) !PushCallArgsResult {
    const start = c.compiler.typeStack.items.len;
    var nodeId = argHead;
    var hasFlexArg = false;
    var hasDynamicArg = false;
    while (nodeId != cy.NullId) {
        const arg = c.nodes[nodeId];
        const argT = try flexExpr(c, nodeId);
        hasFlexArg = hasFlexArg or types.isFlexibleType(argT);
        hasDynamicArg = hasDynamicArg or (argT == bt.Dynamic);
        try c.compiler.typeStack.append(c.alloc, argT);
        nodeId = arg.next;
    }
    return PushCallArgsResult{
        .argTypes = c.compiler.typeStack.items[start..],
        .hasFlexArg = hasFlexArg,
        .hasDynamicArg = hasDynamicArg,
    };
}

pub const ResolvedFuncSigId = u32;
pub const ResolvedFuncSig = struct {
    /// Last elem is the return type sym.
    paramPtr: [*]const ResolvedSymId,
    retSymId: ResolvedSymId,
    paramLen: u16,
    isTyped: bool,

    pub inline fn params(self: ResolvedFuncSig) []const ResolvedSymId {
        return self.paramPtr[0..self.paramLen];
    }

    pub inline fn numParams(self: ResolvedFuncSig) u8 {
        return @intCast(u8, self.paramLen);
    }

    pub inline fn getRetTypeSymId(self: ResolvedFuncSig) ResolvedSymId {
        return self.retSymId;
    }

    pub fn deinit(self: *ResolvedFuncSig, alloc: std.mem.Allocator) void {
        alloc.free(self.params());
    }
};

pub const NameAny = 0;
pub const NameBoolean = 1;
pub const NameNumber = 2;
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
    rFuncSigId: ResolvedFuncSigId,

    /// Resolved return type sym. NullId indicates no declaration.
    rRetTypeSymId: Nullable(ResolvedSymId),

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
            rFuncSigId: u32 = cy.NullId,
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

const ResolvedFuncSigKey = struct {
    paramPtr: [*]const ResolvedSymId,
    paramLen: u32,
    retSymId: ResolvedSymId,
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
    resolvedSyms: std.ArrayListUnmanaged(ResolvedSym),
    resolvedSymMap: std.HashMapUnmanaged(AbsResolvedSymKey, ResolvedSymId, cy.hash.KeyU64Context, 80),

    /// Resolved function symbols that are included in the runtime.
    /// Each func symbol is keyed by the resolved sym and function signature.
    resolvedFuncSyms: std.ArrayListUnmanaged(ResolvedFuncSym),
    resolvedFuncSymMap: std.HashMapUnmanaged(AbsResolvedFuncSymKey, ResolvedFuncSymId, cy.hash.KeyU64Context, 80),

    /// Resolved signatures for functions.
    resolvedFuncSigs: std.ArrayListUnmanaged(ResolvedFuncSig),
    resolvedFuncSigMap: std.HashMapUnmanaged(ResolvedFuncSigKey, ResolvedFuncSigId, ResolvedFuncSigKeyContext, 80),

    /// Fast index to untyped func sig id by num params.
    /// `NullId` indicates a missing func sig id.
    /// TODO: If Cyber implements multiple return values, this would need to be a map.
    resolvedUntypedFuncSigs: std.ArrayListUnmanaged(ResolvedFuncSigId),

    /// Modules.
    modules: std.ArrayListUnmanaged(cy.Module),
    /// Owned absolute specifier path to module.
    moduleMap: std.StringHashMapUnmanaged(cy.ModuleId),

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
        };
    }

    pub fn deinit(self: *Model, alloc: std.mem.Allocator, comptime reset: bool) void {
        if (reset) {
            self.resolvedSyms.clearRetainingCapacity();
            self.resolvedSymMap.clearRetainingCapacity();
            self.resolvedFuncSyms.clearRetainingCapacity();
            self.resolvedFuncSymMap.clearRetainingCapacity();
        } else {
            self.resolvedSyms.deinit(alloc);
            self.resolvedSymMap.deinit(alloc);
            self.resolvedFuncSyms.deinit(alloc);
            self.resolvedFuncSymMap.deinit(alloc);
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

    pub inline fn getResolvedSym(self: *Model, id: ResolvedSymId) ResolvedSym {
        return self.resolvedSyms.items[id];
    }

    pub fn addResolvedSym(self: *Model, key: AbsResolvedSymKey, symT: ResolvedSymType, data: ResolvedSymData) !ResolvedSymId {
        const id = @intCast(u32, self.resolvedSyms.items.len);
        try self.resolvedSyms.append(self.alloc, .{
            .symT = symT,
            .key = key,
            .inner = data,
            .exported = true,
        });
        try self.resolvedSymMap.put(self.alloc, key, id);
        return id;
    }

    pub inline fn getResolvedSymPtr(self: *Model, id: ResolvedSymId) *ResolvedSym {
        return &self.resolvedSyms.items[id];
    }

    pub inline fn getResolvedFuncSym(self: *Model, id: ResolvedFuncSymId) ResolvedFuncSym {
        return self.resolvedFuncSyms.items[id];
    }

    pub inline fn getResolvedFuncSymPtr(self: *Model, id: ResolvedFuncSymId) *ResolvedFuncSym {
        return &self.resolvedFuncSyms.items[id];
    }

    pub inline fn getResolvedFuncSig(self: *Model, id: ResolvedFuncSigId) ResolvedFuncSig {
        return self.resolvedFuncSigs.items[id];
    }

    pub inline fn getModule(self: *Model, id: cy.ModuleId) cy.Module {
        return self.modules.items[id];
    }

    pub inline fn getModulePtr(self: *Model, id: cy.ModuleId) *cy.Module {
        return &self.modules.items[id];
    }
};

pub const ResolvedFuncSigKeyContext = struct {
    pub fn hash(_: @This(), key: ResolvedFuncSigKey) u64 {
        var c = std.hash.Wyhash.init(0);
        c.update(@ptrCast([*]const u8, key.paramPtr)[0..key.paramLen*4]);
        c.update(std.mem.asBytes(&key.retSymId));
        return c.final();
    }
    pub fn eql(_: @This(), a: ResolvedFuncSigKey, b: ResolvedFuncSigKey) bool {
        return std.mem.eql(u32, a.paramPtr[0..a.paramLen], b.paramPtr[0..b.paramLen]);
    }
};

pub const U32SliceContext = struct {
    pub fn hash(_: @This(), key: []const u32) u64 {
        var c = std.hash.Wyhash.init(0);
        c.update(@ptrCast([*]const u8, key.ptr)[0..key.len*4]);
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

pub fn getOrLoadModule(self: *cy.Chunk, spec: []const u8, nodeId: cy.NodeId) !cy.ModuleId {
    var isBuiltin: bool = undefined;
    const absSpec = try resolveSpecTemp(self, spec, &isBuiltin);

    if (self.compiler.sema.moduleMap.get(absSpec)) |modId| {
        return modId;
    } else {
        const modId = try appendResolvedRootModule(self.compiler, absSpec);
        const dupedAbsSpec = self.compiler.sema.getModule(modId).absSpec;

        // Queue import task.
        try self.compiler.importTasks.append(self.alloc, .{
            .chunkId = self.id,
            .nodeId = nodeId,
            .absSpec = dupedAbsSpec,
            .modId = modId,
            .builtin = isBuiltin,
        });
        return modId;
    }
}

test "Internals." {
    try t.eq(@sizeOf(LocalVar), 32);
    try t.eq(@sizeOf(ResolvedFuncSym), 24);
    try t.eq(@sizeOf(ResolvedFuncSig), 16);
    try t.eq(@sizeOf(ResolvedSym), 24);
    try t.eq(@sizeOf(Name), 16);
    try t.eq(@sizeOf(CompactResolvedSymId), 4);
}
