const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const fmt = @import("fmt.zig");
const v = fmt.v;

const core_mod = @import("builtins/core.zig");
const math_mod = @import("builtins/math.zig");
const os_mod = @import("builtins/os.zig");
const test_mod = @import("builtins/test.zig");
const vm_ = @import("vm.zig");

const log = stdx.log.scoped(.sema);

const NullId = std.math.maxInt(u32);
const NullIdU16 = std.math.maxInt(u16);

const TypeTag = enum {
    any,
    boolean,
    number,
    int,
    list,
    map,
    fiber,
    string,
    staticString,
    box,
    tag,
    tagLiteral,
};

pub const Type = struct {
    typeT: TypeTag,
    rcCandidate: bool,
    inner: packed union {
        tag: packed struct {
            tagId: u8,
        },
        number: packed struct {
            canRequestInteger: bool,
        },
    } = undefined,
};

pub const AnyType = Type{
    .typeT = .any,
    .rcCandidate = true,
};

pub const BoolType = Type{
    .typeT = .boolean,
    .rcCandidate = false,
};

pub const IntegerType = Type{
    .typeT = .int,
    .rcCandidate = false,
};

pub const NumberType = Type{
    .typeT = .number,
    .rcCandidate = false,
    .inner = .{
        .number = .{
            .canRequestInteger = false,
        },
    },
};

/// Number constants are numbers by default, but some constants can be requested as an integer during codegen.
/// Once a constant has been assigned to a variable, it becomes a `NumberType`.
const NumberOrRequestIntegerType = Type{
    .typeT = .number,
    .rcCandidate = false,
    .inner = .{
        .number = .{
            .canRequestInteger = true,
        },
    },
};

pub const StaticStringType = Type{
    .typeT = .staticString,
    .rcCandidate = false,
};

pub const StringType = Type{
    .typeT = .string,
    .rcCandidate = true,
};

pub const FiberType = Type{
    .typeT = .fiber,
    .rcCandidate = true,
};

pub const ListType = Type{
    .typeT = .list,
    .rcCandidate = true,
};

pub const TagLiteralType = Type{
    .typeT = .tagLiteral,
    .rcCandidate = false,
};

pub fn initTagType(tagId: u32) Type {
    return .{
        .typeT = .tag,
        .rcCandidate = false,
        .inner = .{
            .tag = .{
                .tagId = @intCast(u8, tagId),
            },
        },
    };
}

pub const MapType = Type{
    .typeT = .map,
    .rcCandidate = true,
};

const ValueAddrType = enum {
    frameOffset,
};

const ValueAddr = struct {
    addrT: ValueAddrType,
    inner: union {
        frameOffset: u32,
    },
};

const RegisterId = u8;

pub const LocalVarId = u32;

/// Local vars exist on the stack.
pub const LocalVar = struct {
    /// The current type of the var as the ast is traversed.
    /// This is updated when there is a variable assignment or a child block returns.
    vtype: Type,

    /// Whether this var is a captured function param.
    isCaptured: bool = false,

    /// Currently a captured var always needs to be boxed.
    /// In the future, the concept of a const variable could change this.
    isBoxed: bool = false,

    /// Whether this is a function param. (Does not include captured vars.)
    isParam: bool = false,

    /// Indicates that at some point during the vars lifetime it was an rcCandidate.
    /// Since all exit paths jump to the same release inst, this flag is used to determine
    /// which vars need a release.
    lifetimeRcCandidate: bool,

    /// There are two cases where the compiler needs to implicitly generate var initializers.
    /// 1. Var is first assigned in a branched block. eg. Assigned inside if block.
    ///    Since the var needs to be released at the end of the root block,
    ///    it needs to have a defined value.
    /// 2. Var is first assigned in an iteration block.
    /// At the beginning of codegen for this block, these vars will be inited to the `none` value.
    genInitializer: bool = false,

    /// Local register offset assigned to this var.
    /// Locals are relative to the stack frame's start position.
    local: RegisterId = undefined,

    /// Since the same sema var is used later by codegen,
    /// use a flag to indicate whether the var has been loosely defined in the block. (eg. assigned to lvalue)
    /// Note that assigning inside a branch counts as defined.
    /// Entering an iter block will auto mark those as defined since the var could have been assigned by a previous iteration.
    genIsDefined: bool = false,

    name: if (builtin.mode == .Debug) []const u8 else void,
};

pub const CapVarDesc = packed union {
    /// The owner of a captured var contains the `DataNode` head to a list of func sym that depend on it.
    owner: u32,
    /// The user of a captured var contains the SemaVarId back to the owner's var.
    user: LocalVarId,
};

const VarAndType = struct {
    id: LocalVarId,
    vtype: Type,
};

pub const SubBlockId = u32;

pub const SubBlock = struct {
    /// Save var start types for entering a codegen iter block.
    /// This can only be determined after the sema pass.
    /// This is used to initialize the var type when entering the codegen iter block so
    /// that the first `genSetVar` produces the correct `set` op.
    iterVarBeginTypes: std.ArrayListUnmanaged(VarAndType),

    /// Track which vars were assigned to in the current sub block.
    /// If the var was first assigned in a parent sub block, the type is saved in the map to
    /// be merged later with the ending var type.
    /// Can be freed after the end of block.
    prevVarTypes: std.AutoHashMapUnmanaged(LocalVarId, Type),

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
            .prevVarTypes = .{},
            .prevSubBlockId = prevSubBlockId,
        };
    }

    pub fn deinit(self: *SubBlock, alloc: std.mem.Allocator) void {
        self.iterVarBeginTypes.deinit(alloc);
    }
};

pub const BlockId = u32;

pub const Block = struct {
    /// Local vars defined in this block. Does not include function params.
    locals: std.ArrayListUnmanaged(LocalVarId),

    /// Param vars for function blocks. Includes captured vars for closures.
    /// Captured vars are always at the end since the function params are known from the start.
    /// Codegen will reserve these first for the calling convention layout.
    params: std.ArrayListUnmanaged(LocalVarId),

    /// Name to var.
    /// This can be deinited after ending the sema block.
    nameToVar: std.StringHashMapUnmanaged(LocalVarId),

    /// Current sub block depth.
    subBlockDepth: u32,

    /// Previous sema block.
    /// When this block ends, the previous block id is set as the current.
    prevBlockId: BlockId,

    /// If the return type is not provided, sema tries to infer it.
    /// It won't try to infer non-trivial cases.
    /// Return type is updated only if `inferRetType` is true while iterating the body statements.
    retType: Type,
    hasRetType: bool,
    inferRetType: bool,

    pub fn init(prevBlockId: BlockId) Block {
        return .{
            .nameToVar = .{},
            .locals = .{},
            .params = .{},
            .subBlockDepth = 0,
            .prevBlockId = prevBlockId,
            .hasRetType = false,
            .inferRetType = false,
            .retType = undefined,
        };
    }

    pub fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        self.locals.deinit(alloc);
        self.params.deinit(alloc);
    }
    
    fn getReturnType(self: *const Block) Type {
        if (self.hasRetType) {
            return self.retType;
        } else {
            return AnyType;
        }
    }
};

pub const NameSymId = u32;

pub const SymId = u32;
const SymType = enum {
    func,
    variable,
    object,
    undefined,
};

const AbsLocalSymKey = vm_.KeyU96;

/// Represents a sema symbol in the current module. It can be an intermediate sym in a sym path or a sym leaf.
/// Since module namespaces use the same accessor operator as local variables, a symbol doesn't always get resolved post sema.
pub const Sym = struct {
    /// key.parentId points to the parent sym in the current module.
    /// If key.parentId == NullId, then this sym is at the root of the module.
    key: AbsLocalSymKey,

    /// After the sema pass, the resolvedSymId will be updated.
    /// If `resolvedSymId` is NullId at codegen pass, this sym is undefined.
    resolvedSymId: ResolvedSymId = NullId,

    // symT: SymType,

    /// Whether this sym is used in the script.
    /// TODO: This might not be necessary once resolving syms ends up using the parent path symbol.
    used: bool,

    /// Used for static vars to track whether it has already generated code for the initializer
    /// in a DFS traversal of its dependencies.
    visited: bool,

    inline fn isFuncSym(self: *const Sym) bool {
        return self.key.absLocalSymKey.numParams != NullId;
    }
};

pub fn getName(c: *const cy.VMcompiler, nameId: NameSymId) []const u8 {
    return c.semaNameSyms.items[nameId];
}

pub fn getSymName(c: *const cy.VMcompiler, sym: *const Sym) []const u8 {
    return c.semaNameSyms.items[sym.key.absLocalSymKey.nameId];
}

pub fn isResolvedUserVarSym(c: *const cy.VMcompiler, sym: *const Sym) bool {
    if (sym.resolvedSymId != NullId) {
        if (c.semaResolvedSyms.items[sym.resolvedSymId].symT == .variable) {
            return c.semaResolvedSyms.items[sym.resolvedSymId].inner.variable.declId != NullId;
        }
    }
    return false;
}

pub fn isResolvedVarSym(c: *const cy.VMcompiler, sym: *const Sym) bool {
    if (sym.resolvedSymId != NullId) {
        return c.semaResolvedSyms.items[sym.resolvedSymId].symT == .variable;
    }
    return false;
}

pub const ResolvedFuncSymId = u32;
pub const ResolvedFuncSym = struct {
    /// DeclId can be the NullId for native functions.
    declId: u32,

    /// Can be used to update a local sym to point to this func sym.
    numParams: u8,

    /// Return type.
    retType: Type,
};

const ResolvedSymType = enum {
    func,
    variable,
    object,
    module,
};

pub const ResolvedSymId = u32;
pub const ResolvedSym = struct {
    symT: ResolvedSymType,
    inner: extern union {
        func: extern struct {
            /// Refers to exactly one resolved func sym.
            /// resolvedFuncSymId == NullId indicates this sym is overloaded;
            /// more than one func shares the same symbol. To disambiguate,
            /// `resolvedFuncSymMap` must be queried with a absResolvedFuncSymKey.
            resolvedFuncSymId: ResolvedFuncSymId,
        },
        variable: extern struct {
            declId: cy.NodeId,
        },
        object: extern struct {
            declId: cy.NodeId,
        },
        module: extern struct {
            id: ModuleId,
        },
    },
};

const SymRefType = enum {
    module,
    moduleMember,
    moduleFuncMember,
    initDeps,
};

pub const SymRef = struct {
    refT: SymRefType,
    inner: union {
        module: ModuleId,
        moduleMember: struct {
            modId: ModuleId,
        },
        /// Points to a module with a specific function signature.
        moduleFuncMember: struct {
            modId: ModuleId,
            numParams: u32,
        },
        /// For variable symbols, this points to a list of sema sym ids in `bufU32` that it depends on for initialization.
        initDeps: struct {
            start: u32 = 0,
            end: u32 = 0,
        },
    },
};

const RelModuleSymKey = vm_.KeyU64;

pub const ModuleId = u32;
pub const Module = struct {
    syms: std.HashMapUnmanaged(RelModuleSymKey, ModuleSym, vm_.KeyU64Context, 80),
    prefix: []const u8,

    pub fn setNativeFunc(self: *Module, c: *cy.VMcompiler, name: []const u8, numParams: u32, func: *const fn (*cy.UserVM, [*]const cy.Value, u8) cy.Value) !void {
        const nameId = try ensureNameSym(c, name);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .numParams = numParams,
            },
        };
        try self.syms.put(c.alloc, key, .{
            .symT = .nativeFunc1,
            .inner = .{
                .nativeFunc1 = .{
                    .func = func,
                },
            },
        });
    }

    pub fn getVarVal(self: *const Module, c: *cy.VMcompiler, name: []const u8) !?cy.Value {
        const nameId = try ensureNameSym(c, name);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .numParams = NullId,
            },
        };
        if (self.syms.get(key)) |sym| {
            return sym.inner.variable.val;
        } else return null;
    }

    pub fn setVar(self: *Module, c: *cy.VMcompiler, name: []const u8, val: cy.Value) !void {
        const nameId = try ensureNameSym(c, name);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .numParams = NullId,
            },
        };
        try self.syms.put(c.alloc, key, .{
            .symT = .variable,
            .inner = .{
                .variable = .{
                    .val = val,
                },
            },
        });
    }

    pub fn deinit(self: *Module, alloc: std.mem.Allocator) void {
        self.syms.deinit(alloc);
    }
};

const ModuleSymType = enum {
    variable,
    nativeFunc1,
};

const ModuleSym = struct {
    symT: ModuleSymType,
    inner: union {
        nativeFunc1: struct {
            func: *const fn (*cy.UserVM, [*]const cy.Value, u8) cy.Value,
        },
        variable: struct {
            val: cy.Value,
        },
    },
};

/// Relative symbol signature key. RelFuncSigKey is repurposed for variable syms when `numParams` == NullId.
const RelSymSigKey = cy.RelFuncSigKey;
const RelSymSigKeyContext = cy.RelFuncSigKeyContext;

/// Absolute symbol signature key. AbsFuncSigKey is repurposed for variable syms when `numParams` == NullId.
pub const AbsSymSigKey = vm_.KeyU64;
pub const AbsResolvedSymKey = vm_.KeyU64;
pub const AbsSymSigKeyContext = cy.AbsFuncSigKeyContext;

pub fn semaStmts(self: *cy.VMcompiler, head: cy.NodeId, comptime attachEnd: bool) anyerror!void {
    var cur_id = head;
    while (cur_id != NullId) {
        const node = self.nodes[cur_id];
        if (attachEnd) {
            if (node.next == NullId) {
                try semaStmt(self, cur_id, false);
            } else {
                try semaStmt(self, cur_id, true);
            }
        } else {
            try semaStmt(self, cur_id, true);
        }
        cur_id = node.next;
    }
}

pub fn semaStmt(c: *cy.VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) !void {
    // log.debug("sema stmt {}", .{node.node_t});
    c.curNodeId = nodeId;
    const node = c.nodes[nodeId];
    switch (node.node_t) {
        .pass_stmt => {
            return;
        },
        .expr_stmt => {
            _ = try semaExpr(c, node.head.child_head, discardTopExprReg);
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
                const rtype = try semaExpr(c, node.head.opAssignStmt.right, false);
                _ = try opAssignVar(c, node.head.opAssignStmt.left, rtype);
            } else if (left.node_t == .accessExpr) {
                const accessLeft = try semaExpr(c, left.head.accessExpr.left, false);
                const accessRight = try semaExpr(c, left.head.accessExpr.right, false);
                const right = try semaExpr(c, node.head.opAssignStmt.right, false);
                _ = accessLeft;
                _ = accessRight;
                _ = right;
            } else {
                return c.reportErrorAt("Assignment to the left {} is not allowed.", &.{fmt.v(left.node_t)}, nodeId);
            }
        },
        .assign_stmt => {
            const left = c.nodes[node.head.left_right.left];
            if (left.node_t == .ident) {
                const rtype = try semaExpr(c, node.head.left_right.right, false);
                _ = try assignVar(c, node.head.left_right.left, rtype, false);
            } else if (left.node_t == .arr_access_expr) {
                _ = try semaExpr(c, left.head.left_right.left, false);
                _ = try semaExpr(c, left.head.left_right.right, false);
                _ = try semaExpr(c, node.head.left_right.right, false);
            } else if (left.node_t == .accessExpr) {
                _ = try semaExpr(c, left.head.accessExpr.left, false);
                _ = try semaExpr(c, left.head.accessExpr.right, false);
                _ = try semaExpr(c, node.head.left_right.right, false);
            } else {
                return c.reportErrorAt("Assignment to the left {} is not allowed.", &.{fmt.v(left.node_t)}, nodeId);
            }
        },
        .varDecl => {
            const left = c.nodes[node.head.varDecl.left];
            if (left.node_t == .ident) {
                const name = c.getNodeTokenString(left);
                const symId = try resolveLocalVarSym(c, null, name, nodeId);
                c.curSemaSymVar = symId;
                c.semaVarDeclDeps.clearRetainingCapacity();
                defer c.curSemaSymVar = NullId;

                _ = semaExpr(c, node.head.varDecl.right, false) catch |err| {
                    if (err == error.CanNotUseLocal) {
                        const local = c.nodes[c.errorPayload];
                        const localName = c.getNodeTokenString(local);
                        return c.reportErrorAt("The declaration of static variable `{}` can not reference the local variable `{}`.", &.{v(name), v(localName)}, nodeId);
                    } else {
                        return err;
                    } 
                };
            } else {
                return c.reportErrorAt("Static variable declarations can only have an identifier as the name. Parsed {} instead.", &.{fmt.v(left.node_t)}, nodeId);
            }
        },
        .localDecl => {
            const left = c.nodes[node.head.left_right.left];
            if (left.node_t == .ident) {
                const rtype = try semaExpr(c, node.head.left_right.right, false);
                _ = try assignVar(c, node.head.left_right.left, rtype, true);
            } else {
                return c.reportErrorAt("Local variable declarations can only have an identifier as the name. Parsed {} instead.", &.{fmt.v(left.node_t)}, nodeId);
            }
        },
        .tagDecl => {
            const nameN = c.nodes[node.head.tagDecl.name];
            const name = c.getNodeTokenString(nameN);

            const tid = try c.vm.ensureTagType(name);

            var i: u32 = 0;
            var memberId = node.head.tagDecl.memberHead;
            while (memberId != NullId) : (i += 1) {
                const member = c.nodes[memberId];
                memberId = member.next;
            }
            const numMembers = i;
            c.vm.tagTypes.buf[tid].numMembers = numMembers;

            i = 0;
            memberId = node.head.tagDecl.memberHead;
            while (memberId != NullId) : (i += 1) {
                const member = c.nodes[memberId];
                const mName = c.getNodeTokenString(member);
                const symId = try c.vm.ensureTagLitSym(mName);
                c.vm.setTagLitSym(tid, symId, i);
                memberId = member.next;
            }
        },
        .structDecl => {
            const nameN = c.nodes[node.head.structDecl.name];
            const name = c.getNodeTokenString(nameN);
            const nameId = try ensureNameSym(c, name);

            if (c.vm.getStruct(nameId, 0) != null) {
                return c.reportErrorAt("Object type `{}` already exists", &.{v(name)}, nodeId);
            }

            if (getSym(c, null, nameId, null) != null) {
                return c.reportErrorAt("Object type `{}` already exists", &.{v(name)}, nodeId);
            }
            const objSymId = try resolveLocalObjectSym(c, null, name, nodeId);

            var funcId = node.head.structDecl.funcsHead;
            while (funcId != NullId) {
                const func = c.nodes[funcId];
                const decl = c.funcDecls[func.head.func.decl_id];

                if (decl.params.end > decl.params.start) {
                    const param = c.funcParams[decl.params.start];
                    const paramName = c.src[param.name.start..param.name.end];
                    if (std.mem.eql(u8, paramName, "self")) {
                        // Struct method.
                        try pushBlock(c);
                        errdefer endBlock(c) catch stdx.fatal();
                        try pushMethodParamVars(c, decl);
                        try semaStmts(c, func.head.func.body_head, false);
                        try endBlock(c);
                        funcId = func.next;
                        continue;
                    }
                }

                // Struct function.
                const funcName = c.src[decl.name.start..decl.name.end];
                const funcNameId = try ensureNameSym(c, funcName);
                const numParams = @intCast(u16, decl.params.end - decl.params.start);
                if (getSym(c, objSymId, funcNameId, numParams) == null) {
                    _ = try ensureSym(c, objSymId, funcNameId, numParams);
                } else {
                    return c.reportErrorAt("Symbol `{}` is already declared.", &.{v(funcName)}, nodeId);
                }

                try pushBlock(c);
                errdefer endBlock(c) catch stdx.fatal();
                try pushFuncParamVars(c, decl);
                try semaStmts(c, func.head.func.body_head, false);
                const retType = curBlock(c).getReturnType();
                try endBlock(c);

                const symId = try ensureSym(c, objSymId, funcNameId, numParams);
                const resolvedParentSymId = c.semaSyms.items[objSymId].resolvedSymId;
                const res = try resolveLocalFuncSym(c, resolvedParentSymId, funcNameId, func.head.func.decl_id, retType);
                c.funcDecls[func.head.func.decl_id].semaSymId = symId;
                c.funcDecls[func.head.func.decl_id].semaResolvedFuncSymId = res.resolvedFuncSymId;
                c.semaSyms.items[symId].resolvedSymId = res.resolvedSymId;

                funcId = func.next;
            }
        },
        .func_decl => {
            const func = c.funcDecls[node.head.func.decl_id];
            var retType: ?Type = null;
            if (func.return_type) |slice| {
                const retTypeName = c.src[slice.start..slice.end];
                if (c.typeNames.get(retTypeName)) |vtype| {
                    retType = vtype;
                }
            }

            try pushBlock(c);
            if (retType == null) {
                curBlock(c).inferRetType = true;
            }
            try pushFuncParamVars(c, func);
            try semaStmts(c, node.head.func.body_head, false);
            const sblock = curBlock(c);
            if (retType == null) {
                retType = sblock.getReturnType();
            }
            const name = c.src[func.name.start..func.name.end];
            const nameId = try ensureNameSym(c, name);
            const numParams = @intCast(u16, func.params.end - func.params.start);

            const rtSymId = try c.vm.ensureFuncSym(NullId, nameId, numParams);
            try endFuncSymBlock(c, rtSymId, numParams);

            const symId = try ensureSym(c, NullId, nameId, numParams);
            const res = try resolveLocalFuncSym(c, null, nameId, node.head.func.decl_id, retType.?);
            c.funcDecls[node.head.func.decl_id].semaSymId = symId;
            c.funcDecls[node.head.func.decl_id].semaResolvedFuncSymId = res.resolvedFuncSymId;
            c.semaSyms.items[symId].resolvedSymId = res.resolvedSymId;
        },
        .whileCondStmt => {
            try pushIterSubBlock(c);

            _ = try semaExpr(c, node.head.whileCondStmt.cond, false);
            try semaStmts(c, node.head.whileCondStmt.bodyHead, false);

            try endIterSubBlock(c);
        },
        .forOptStmt => {
            try pushIterSubBlock(c);

            const optt = try semaExpr(c, node.head.forOptStmt.opt, false);
            if (node.head.forOptStmt.as != NullId) {
                _ = try ensureLocalBodyVar(c, node.head.forOptStmt.as, AnyType);
                _ = try assignVar(c, node.head.forOptStmt.as, optt, false);
            }

            try semaStmts(c, node.head.forOptStmt.bodyHead, false);

            try endIterSubBlock(c);
        },
        .whileInfStmt => {
            try pushIterSubBlock(c);
            try semaStmts(c, node.head.child_head, false);
            try endIterSubBlock(c);
        },
        .for_iter_stmt => {
            try pushIterSubBlock(c);

            _ = try semaExpr(c, node.head.for_iter_stmt.iterable, false);

            const eachClause = c.nodes[node.head.for_iter_stmt.eachClause];
            if (eachClause.head.eachClause.key != NullId) {
                _ = try ensureLocalBodyVar(c, eachClause.head.eachClause.key, AnyType);
            }
            _ = try ensureLocalBodyVar(c, eachClause.head.eachClause.value, AnyType);

            try semaStmts(c, node.head.for_iter_stmt.body_head, false);
            try endIterSubBlock(c);
        },
        .for_range_stmt => {
            try pushIterSubBlock(c);

            if (node.head.for_range_stmt.eachClause != NullId) {
                const eachClause = c.nodes[node.head.for_range_stmt.eachClause];
                _ = try ensureLocalBodyVar(c, eachClause.head.eachClause.value, NumberType);
            }

            const range_clause = c.nodes[node.head.for_range_stmt.range_clause];
            _ = try semaExpr(c, range_clause.head.left_right.left, false);
            _ = try semaExpr(c, range_clause.head.left_right.right, false);

            try semaStmts(c, node.head.for_range_stmt.body_head, false);
            try endIterSubBlock(c);
        },
        .matchBlock => {
            _ = try semaExpr(c, node.head.matchBlock.expr, false);

            var curCase = node.head.matchBlock.firstCase;
            while (curCase != NullId) {
                const case = c.nodes[curCase];
                var curCond = case.head.caseBlock.firstCond;
                while (curCond != NullId) {
                    const cond = c.nodes[curCond];
                    if (cond.node_t != .elseCase) {
                        _ = try semaExpr(c, curCond, false);
                    }
                    curCond = cond.next;
                }
                curCase = case.next;
            }

            curCase = node.head.matchBlock.firstCase;
            while (curCase != NullId) {
                const case = c.nodes[curCase];
                try pushSubBlock(c);
                try semaStmts(c, case.head.caseBlock.firstChild, false);
                try endSubBlock(c);
                curCase = case.next;
            }
        },
        .if_stmt => {
            _ = try semaExpr(c, node.head.left_right.left, false);

            try pushSubBlock(c);
            try semaStmts(c, node.head.left_right.right, false);
            try endSubBlock(c);

            var elseClauseId = node.head.left_right.extra;
            while (elseClauseId != NullId) {
                const elseClause = c.nodes[elseClauseId];
                if (elseClause.head.else_clause.cond == NullId) {
                    try pushSubBlock(c);
                    try semaStmts(c, elseClause.head.else_clause.body_head, false);
                    try endSubBlock(c);
                    break;
                } else {
                    _ = try semaExpr(c, elseClause.head.else_clause.cond, false);

                    try pushSubBlock(c);
                    try semaStmts(c, elseClause.head.else_clause.body_head, false);
                    try endSubBlock(c);
                    elseClauseId = elseClause.head.else_clause.else_clause;
                }
            }
        },
        .importStmt => {
            const ident = c.nodes[node.head.left_right.left];
            const name = c.getNodeTokenString(ident);
            const nameId = try ensureNameSym(c, name);
            const symId = try ensureSym(c, null, nameId, null);

            const spec = c.nodes[node.head.left_right.right];
            const specPath = c.getNodeTokenString(spec);

            const modId = try getOrLoadModule(c, specPath, nodeId);
            try c.semaSymToRef.put(c.alloc, symId, .{
                .refT = .module,
                .inner = .{
                    .module = modId,
                },
            });
        },
        .return_stmt => {
            return;
        },
        .return_expr_stmt => {
            const childT = try semaExpr(c, node.head.child_head, false);
            const block = curBlock(c);
            if (block.inferRetType) {
                if (block.hasRetType) {
                    block.retType = childT;
                    block.hasRetType = true;
                } else {
                    if (block.retType.typeT != .any) {
                        if (block.retType.typeT != childT.typeT) {
                            block.retType = AnyType;
                        }
                    }
                }
            }
        },
        else => return c.reportErrorAt("Unsupported node", &.{}, nodeId),
    }
}

fn semaExpr(c: *cy.VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) anyerror!Type {
    c.curNodeId = nodeId;
    const node = c.nodes[nodeId];
    // log.debug("sema expr {}", .{node.node_t});
    switch (node.node_t) {
        .true_literal => {
            return BoolType;
        },
        .false_literal => {
            return BoolType;
        },
        .none => {
            return AnyType;
        },
        .arr_literal => {
            var expr_id = node.head.child_head;
            var i: u32 = 0;
            while (expr_id != NullId) : (i += 1) {
                var expr = c.nodes[expr_id];
                _ = try semaExpr(c, expr_id, discardTopExprReg);
                expr_id = expr.next;
            }

            return ListType;
        },
        .tagLiteral => {
            return TagLiteralType;
        },
        .tagInit => {
            const nameN = c.nodes[node.head.left_right.left];
            const name = c.getNodeTokenString(nameN);
            const tid = try c.vm.ensureTagType(name);
            return initTagType(tid);
        },
        .structInit => {
            const initializer = c.nodes[node.head.structInit.initializer];

            var i: u32 = 0;
            var entry_id = initializer.head.child_head;
            while (entry_id != NullId) : (i += 1) {
                var entry = c.nodes[entry_id];
                _ = try semaExpr(c, entry.head.mapEntry.right, discardTopExprReg);
                entry_id = entry.next;
            }
            return AnyType;
        },
        .map_literal => {
            var i: u32 = 0;
            var entry_id = node.head.child_head;
            while (entry_id != NullId) : (i += 1) {
                var entry = c.nodes[entry_id];

                _ = try semaExpr(c, entry.head.mapEntry.right, discardTopExprReg);
                entry_id = entry.next;
            }
            return MapType;
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
            }
            if (std.math.cast(i32, val) != null) {
                return NumberOrRequestIntegerType;
            }
            return NumberType;
        },
        .number => {
            const literal = c.getNodeTokenString(node);
            const val = try std.fmt.parseFloat(f64, literal);
            if (cy.Value.floatCanBeInteger(val)) {
                const int = @floatToInt(i64, val);
                if (std.math.cast(i32, int) != null) {
                    return NumberOrRequestIntegerType;
                }
            }
            return NumberType;
        },
        .string => {
            return StaticStringType;
        },
        .stringTemplate => {
            var expStringPart = true;
            var curId = node.head.stringTemplate.partsHead;
            while (curId != NullId) {
                const cur = c.nodes[curId];
                if (!expStringPart) {
                    _ = try semaExpr(c, curId, discardTopExprReg);
                }
                curId = cur.next;
                expStringPart = !expStringPart;
            }
            return StringType;
        },
        .ident => {
            if (try identLocalVarOrNull(c, nodeId, true)) |varId| {
                return c.vars.items[varId].vtype;
            } else {
                try setIdentAsVarSym(c, nodeId);
                return AnyType;
            }
        },
        .if_expr => {
            _ = try semaExpr(c, node.head.if_expr.cond, false);

            _ = try semaExpr(c, node.head.if_expr.body_expr, discardTopExprReg);

            if (node.head.if_expr.else_clause != NullId) {
                const else_clause = c.nodes[node.head.if_expr.else_clause];
                _ = try semaExpr(c, else_clause.head.child_head, discardTopExprReg);
            }
            return AnyType;
        },
        .arr_range_expr => {
            _ = try semaExpr(c, node.head.arr_range_expr.arr, discardTopExprReg);

            if (node.head.arr_range_expr.left == NullId) {
                // nop
            } else {
                _ = try semaExpr(c, node.head.arr_range_expr.left, discardTopExprReg);
            }
            if (node.head.arr_range_expr.right == NullId) {
                // nop
            } else {
                _ = try semaExpr(c, node.head.arr_range_expr.right, discardTopExprReg);
            }

            return ListType;
        },
        .accessExpr => {
            return semaAccessExpr(c, nodeId, discardTopExprReg);
        },
        .arr_access_expr => {
            _ = try semaExpr(c, node.head.left_right.left, discardTopExprReg);

            const index = c.nodes[node.head.left_right.right];
            if (index.node_t == .unary_expr and index.head.unary.op == .minus) {
                _ = try semaExpr(c, index.head.unary.child, discardTopExprReg);
            } else {
                _ = try semaExpr(c, node.head.left_right.right, discardTopExprReg);
            }
            return AnyType;
        },
        .comptExpr => {
            _ = try semaExpr(c, node.head.child_head, discardTopExprReg);
            return AnyType;
        },
        .tryExpr => {
            _ = try semaExpr(c, node.head.child_head, discardTopExprReg);
            return AnyType;
        },
        .unary_expr => {
            const op = node.head.unary.op;
            switch (op) {
                .minus => {
                    _ = try semaExpr(c, node.head.unary.child, discardTopExprReg);
                    return NumberType;
                },
                .not => {
                    _ = try semaExpr(c, node.head.unary.child, discardTopExprReg);
                    return BoolType;
                },
                .bitwiseNot => {
                    _ = try semaExpr(c, node.head.unary.child, discardTopExprReg);
                    return NumberType;
                },
                // else => return self.reportErrorAt("Unsupported unary op: {}", .{op}, node),
            }
        },
        .group => {
            return semaExpr(c, node.head.child_head, discardTopExprReg);
        },
        .binExpr => {
            const left = node.head.binExpr.left;
            const right = node.head.binExpr.right;

            const op = node.head.binExpr.op;
            switch (op) {
                .plus => {
                    const ltype = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    if (ltype.typeT == .string) {
                        return StringType;
                    } else {
                        return NumberType;
                    }
                },
                .star => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return NumberType;
                },
                .slash => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return NumberType;
                },
                .percent => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return NumberType;
                },
                .caret => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return NumberType;
                },
                .minus => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return NumberType;
                },
                .bitwiseAnd => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return NumberType;
                },
                .bitwiseOr => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return NumberType;
                },
                .bitwiseXor => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return NumberType;
                },
                .bitwiseLeftShift => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return NumberType;
                },
                .bitwiseRightShift => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return NumberType;
                },
                .and_op => {
                    const ltype = try semaExpr(c, left, discardTopExprReg);
                    const rtype = try semaExpr(c, right, discardTopExprReg);
                    if (ltype.typeT == rtype.typeT) {
                        return ltype;
                    } else return AnyType;
                },
                .or_op => {
                    const ltype = try semaExpr(c, left, discardTopExprReg);
                    const rtype = try semaExpr(c, right, discardTopExprReg);
                    if (ltype.typeT == rtype.typeT) {
                        return ltype;
                    } else return AnyType;
                },
                .bang_equal => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return BoolType;
                },
                .equal_equal => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return BoolType;
                },
                .less => {
                    const leftT = try semaExpr(c, left, discardTopExprReg);
                    const rightT = try semaExpr(c, right, discardTopExprReg);
                    const canRequestLeftInt = leftT.typeT == .int or (leftT.typeT == .number and leftT.inner.number.canRequestInteger);
                    const canRequestRightInt = rightT.typeT == .int or (rightT.typeT == .number and rightT.inner.number.canRequestInteger);
                    if (canRequestLeftInt and canRequestRightInt) {
                        c.nodes[nodeId].head.binExpr.semaCanRequestIntegerOperands = true;
                    }
                    return BoolType;
                },
                .less_equal => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return BoolType;
                },
                .greater => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return BoolType;
                },
                .greater_equal => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return BoolType;
                },
                else => return c.reportErrorAt("Unsupported binary op: {}", &.{fmt.v(op)}, nodeId),
            }
        },
        .coyield => {
            return AnyType;
        },
        .coresume => {
            _ = try semaExpr(c, node.head.child_head, false);
            return AnyType;
        },
        .coinit => {
            _ = try semaExpr(c, node.head.child_head, false);
            return FiberType;
        },
        .call_expr => {
            const callee = c.nodes[node.head.func_call.callee];
            if (!node.head.func_call.has_named_arg) {
                if (callee.node_t == .accessExpr) {
                    _ = try semaExpr(c, callee.head.accessExpr.left, false);

                    var numArgs: u32 = 0;
                    var arg_id = node.head.func_call.arg_head;
                    while (arg_id != NullId) : (numArgs += 1) {
                        const arg = c.nodes[arg_id];
                        _ = try semaExpr(c, arg_id, false);
                        arg_id = arg.next;
                    }

                    const left = c.nodes[callee.head.accessExpr.left];
                    var leftSymId: u32 = NullId;
                    if (left.node_t == .ident) {
                        leftSymId = left.head.ident.semaSymId;
                    } else if (left.node_t == .accessExpr) {
                        leftSymId = left.head.accessExpr.semaSymId;
                    }
                    if (leftSymId != NullId) {
                        // Left is a sym candidate.
                        // Ensure func sym.
                        const right = c.nodes[callee.head.accessExpr.right];
                        const name = c.getNodeTokenString(right);
                        const nameId = try ensureNameSym(c, name);
                        const symId = try ensureSym(c, leftSymId, nameId, @intCast(u16, numArgs));
                        c.semaSyms.items[symId].used = true;
                        c.nodes[node.head.func_call.callee].head.accessExpr.semaSymId = symId;
                    }

                    return AnyType;
                } else if (callee.node_t == .ident) {
                    if (try identLocalVarOrNull(c, node.head.func_call.callee, true)) |varId| {
                        _ = varId;
                        var numArgs: u32 = 1;
                        var arg_id = node.head.func_call.arg_head;
                        while (arg_id != NullId) : (numArgs += 1) {
                            const arg = c.nodes[arg_id];
                            _ = try semaExpr(c, arg_id, false);
                            arg_id = arg.next;
                        }

        //                 // Load callee after args so it can be easily discarded.
        //                 try self.genLoadLocal(info);
                        return AnyType;
                    } else {
                        var numArgs: u32 = 0;
                        var arg_id = node.head.func_call.arg_head;
                        while (arg_id != NullId) : (numArgs += 1) {
                            const arg = c.nodes[arg_id];
                            _ = try semaExpr(c, arg_id, false);
                            arg_id = arg.next;
                        }

                        // Ensure func sym.
                        const name = c.getNodeTokenString(callee);
                        const nameId = try ensureNameSym(c, name);
                        const symId = try ensureSym(c, null, nameId, @intCast(u16, numArgs));
                        c.semaSyms.items[symId].used = true;
                        c.nodes[node.head.func_call.callee].head.ident.semaSymId = symId;

                        return AnyType;
                    }
                } else {
                    // All other callees are treated as function value calls.
                    var numArgs: u32 = 0;
                    var arg_id = node.head.func_call.arg_head;
                    while (arg_id != NullId) : (numArgs += 1) {
                        const arg = c.nodes[arg_id];
                        _ = try semaExpr(c, arg_id, false);
                        arg_id = arg.next;
                    }

                    _ = try semaExpr(c, node.head.func_call.callee, false);
                    return AnyType;
                }
            } else return c.reportErrorAt("Unsupported named args", &.{}, nodeId);
        },
        .lambda_multi => {
            if (!discardTopExprReg) {
                try pushBlock(c);

                // Generate function body.
                const func = c.funcDecls[node.head.func.decl_id];
                try pushFuncParamVars(c, func);
                try semaStmts(c, node.head.func.body_head, false);

                const numParams = func.params.len();
                try endFuncBlock(c, numParams);
            }
            return AnyType;
        },
        .lambda_expr => {
            if (!discardTopExprReg) {
                try pushBlock(c);

                // Generate function body.
                const func = c.funcDecls[node.head.func.decl_id];
                try pushFuncParamVars(c, func);
                _ = try semaExpr(c, node.head.func.body_head, false);

                const numParams = func.params.len();
                try endFuncBlock(c, numParams);
            }
            return AnyType;
        },
        else => return c.reportErrorAt("Unsupported node", &.{}, nodeId),
    }
}

pub fn pushBlock(self: *cy.VMcompiler) !void {
    const prevId = self.curSemaBlockId;
    self.curSemaBlockId = @intCast(u32, self.semaBlocks.items.len);
    try self.semaBlocks.append(self.alloc, Block.init(prevId));
    self.semaBlockDepth += 1;
    try pushSubBlock(self);
}

fn pushSubBlock(self: *cy.VMcompiler) !void {
    curBlock(self).subBlockDepth += 1;
    const prev = self.curSemaSubBlockId;
    self.curSemaSubBlockId = @intCast(u32, self.semaSubBlocks.items.len);
    try self.semaSubBlocks.append(self.alloc, SubBlock.init(prev, self.assignedVarStack.items.len));
}

fn pushMethodParamVars(c: *cy.VMcompiler, func: cy.FuncDecl) !void {
    const sblock = curBlock(c);

    if (func.params.end > func.params.start) {
        for (c.funcParams[func.params.start + 1..func.params.end]) |param| {
            const paramName = c.src[param.name.start..param.name.end];
            const paramT = AnyType;
            const id = try pushLocalVar(c, paramName, paramT);
            try sblock.params.append(c.alloc, id);
        }
    }

    // Add self receiver param.
    var id = try pushLocalVar(c, "self", AnyType);
    try sblock.params.append(c.alloc, id);
}

fn pushFuncParamVars(c: *cy.VMcompiler, func: cy.FuncDecl) !void {
    const sblock = curBlock(c);

    if (func.params.end > func.params.start) {
        for (c.funcParams[func.params.start..func.params.end]) |param| {
            const paramName = c.src[param.name.start..param.name.end];
            var paramT = AnyType;
            if (param.typeName.len() > 0) {
                const typeName = c.src[param.typeName.start..param.typeName.end];
                if (c.typeNames.get(typeName)) |vtype| {
                    paramT = vtype;
                }
            }
            const id = try pushLocalVar(c, paramName, paramT);
            try sblock.params.append(c.alloc, id);
        }
    }
}

fn ensureCapVarOwner(c: *cy.VMcompiler, varId: LocalVarId) !void {
    const res = try c.capVarDescs.getOrPut(c.alloc, varId);
    if (!res.found_existing) {
        res.value_ptr.* = .{
            .owner = NullId,
        };
    }
}

fn pushLocalVar(c: *cy.VMcompiler, name: []const u8, vtype: Type) !LocalVarId {
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
            .lifetimeRcCandidate = vtype.rcCandidate,
        });
        return id;
    }
}

fn getVarPtr(self: *cy.VMcompiler, name: []const u8) ?*LocalVar {
    if (curBlock(self).nameToVar.get(name)) |varId| {
        return &self.vars.items[varId];
    } else return null;
}

fn pushCapturedVar(self: *cy.VMcompiler, name: []const u8, parentVarId: LocalVarId, vtype: Type) !LocalVarId {
    const id = try pushLocalVar(self, name, vtype);
    self.vars.items[id].isCaptured = true;
    self.vars.items[id].isBoxed = true;
    try self.capVarDescs.put(self.alloc, id, .{
        .user = parentVarId,
    });
    try curBlock(self).params.append(self.alloc, id);
    return id;
}

fn pushLocalBodyVar(self: *cy.VMcompiler, name: []const u8, vtype: Type) !LocalVarId {
    const id = try pushLocalVar(self, name, vtype);
    try curBlock(self).locals.append(self.alloc, id);
    return id;
}

fn ensureLocalBodyVar(self: *cy.VMcompiler, ident: cy.NodeId, vtype: Type) !LocalVarId {
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

fn setIdentAsVarSym(self: *cy.VMcompiler, ident: cy.NodeId) !void {
    const node = self.nodes[ident];
    const name = self.getNodeTokenString(node);
    const nameId = try ensureNameSym(self, name);

    // Assume reference to global sym.
    const symId = try ensureSym(self, null, nameId, null);
    self.semaSyms.items[symId].used = true;

    if (self.curSemaSymVar != NullId) {
        // Record this symbol as a dependency.
        const res = try self.semaSymToRef.getOrPut(self.alloc, self.curSemaSymVar);
        if (res.found_existing) {
            const depRes = try self.semaVarDeclDeps.getOrPut(self.alloc, symId);
            if (!depRes.found_existing) {
                try self.bufU32.append(self.alloc, symId);
                res.value_ptr.*.inner.initDeps.end = @intCast(u32, self.bufU32.items.len);
                depRes.value_ptr.* = {};
            }
        } else {
            const start = @intCast(u32, self.bufU32.items.len);
            try self.bufU32.append(self.alloc, symId);
            res.value_ptr.* = .{
                .refT = .initDeps,
                .inner = .{
                    .initDeps = .{
                        .start = start,
                        .end = @intCast(u32, self.bufU32.items.len),
                    },
                }
            };
        }
    }
    self.nodes[ident].head.ident.semaSymId = symId;
}

/// Retrieve the SemaVarId that is local to the current block.
/// If the var comes from a parent block, a local captured var is created and returned.
/// Sets the resulting id onto the node for codegen.
fn identLocalVarOrNull(self: *cy.VMcompiler, ident: cy.NodeId, searchParentScope: bool) !?LocalVarId {
    const node = self.nodes[ident];
    const name = self.getNodeTokenString(node);

    if (lookupVar(self, name, searchParentScope)) |res| {
        if (self.curSemaSymVar != NullId) {
            self.errorPayload = ident;
            return error.CanNotUseLocal;
        }
        if (res.fromParentBlock) {
            // Create a local captured variable.
            const svar = self.vars.items[res.id];
            const id = try pushCapturedVar(self, name, res.id, svar.vtype);
            self.nodes[ident].head.ident.semaVarId = id;
            return id;
        } else {
            self.nodes[ident].head.ident.semaVarId = res.id;
            return res.id;
        }
    } else {
        self.nodes[ident].head.ident.semaVarId = NullId;
        return null;
    }
}

pub fn resolveSym(self: *cy.VMcompiler, symId: SymId) !void {
    const sym = &self.semaSyms.items[symId];
    log.debug("resolving {} {s} {}", .{symId, getSymName(self, sym), sym.key.absLocalSymKey.numParams});
    // defer {
    //     if (sym.resolvedSymId != NullId) {
    //         log.debug("resolved", .{});
    //     }
    // }
    if (sym.key.absLocalSymKey.localParentSymId == NullId) {
        // Direct alias lookup.
        if (self.semaSymToRef.get(symId)) |ref| {
            if (ref.refT == .moduleMember) {
                const modId = ref.inner.moduleMember.modId;
                const key = vm_.KeyU64{
                    .absResolvedSymKey = .{
                        .resolvedParentSymId = NullId,
                        .nameId = sym.key.absLocalSymKey.nameId,
                    },
                };
                if (try getOrTryResolveSym(self, key, sym.key.absLocalSymKey.numParams, modId, NullId)) |resolvedId| {
                    sym.resolvedSymId = resolvedId;
                }
            } else if (ref.refT == .moduleFuncMember) {
                const modId = ref.inner.moduleFuncMember.modId;
                const key = vm_.KeyU64{
                    .absResolvedSymKey = .{
                        .resolvedParentSymId = NullId,
                        .nameId = sym.key.absLocalSymKey.nameId,
                    },
                };
                if (try getOrTryResolveSym(self, key, ref.inner.moduleFuncMember.numParams, modId, NullId)) |resolvedId| {
                    sym.resolvedSymId = resolvedId;
                }
            } else if (ref.refT == .module) {
                const key = AbsResolvedSymKey{
                    .absResolvedSymKey = .{
                        .resolvedParentSymId = NullId,
                        .nameId = sym.key.absLocalSymKey.nameId,
                    },
                };
                if (self.semaResolvedSymMap.get(key)) |id| {
                    sym.resolvedSymId = id;
                } else {
                    const id = @intCast(u32, self.semaResolvedSyms.items.len);
                    try self.semaResolvedSyms.append(self.alloc, .{
                        .symT = .module,
                        .inner = .{
                            .module = .{
                                .id = ref.inner.module,
                            },
                        },
                    });
                    try self.semaResolvedSymMap.put(self.alloc, key, id);
                    sym.resolvedSymId = id;
                }
                // log.debug("resolved", .{});
            } else {
                return self.reportError("Unsupported {}", &.{fmt.v(ref.refT)});
            }
        }

        // Fallback logic, look for resolved sym.
        const key = vm_.KeyU64{
            .absResolvedSymKey = .{
                .resolvedParentSymId = NullId,
                .nameId = sym.key.absLocalSymKey.nameId,
            },
        };
        if (self.semaResolvedSymMap.get(key)) |rsymId| {
            const rsym = self.semaResolvedSyms.items[rsymId];
            if (rsym.symT == .func) {
                if (rsym.inner.func.resolvedFuncSymId == NullId) {
                    return self.reportError("Can not disambiguate the symbol `{}`.", &.{v(getName(self, sym.key.absLocalSymKey.nameId))});
                } else {
                    // Unique function sym. Update key to func signature.
                    const funcSym = self.semaResolvedFuncSyms.items[rsym.inner.func.resolvedFuncSymId];
                    if (sym.key.absLocalSymKey.numParams != NullId) {
                        if (funcSym.numParams == sym.key.absLocalSymKey.numParams) {
                            // Only if signatures match.
                            sym.resolvedSymId = rsymId;
                        }
                    } else {
                        sym.resolvedSymId = rsymId;
                    }
                }
            } else {
                sym.resolvedSymId = rsymId;
            }
        }
    } else {
        // Has parent symbol.
        
        const psym = self.semaSyms.items[sym.key.absLocalSymKey.localParentSymId];
        if (psym.resolvedSymId == NullId) {
            // If the parent isn't resolved, this sym won't be resolved either.
            return;
        }

        const rpsym = self.semaResolvedSyms.items[psym.resolvedSymId];
        if (rpsym.symT == .module) {
            const modId = rpsym.inner.module.id;
            const key = vm_.KeyU64{
                .absResolvedSymKey = .{
                    .resolvedParentSymId = psym.resolvedSymId,
                    .nameId = sym.key.absLocalSymKey.nameId,
                },
            };
            if (try getOrTryResolveSym(self, key, sym.key.absLocalSymKey.numParams, modId, NullId)) |resolvedId| {
                sym.resolvedSymId = resolvedId;
                // log.debug("resolve {s} {} {}", .{sym.getName(), sym.key.parentId, modId});
            }
        }
    }
}

fn getOrTryResolveSym(self: *cy.VMcompiler, key: AbsResolvedSymKey, numParams: u32, modId: ModuleId, nodeId: cy.NodeId) !?ResolvedSymId {
    if (numParams == NullId) {
        // Not a function. Check sym map.
        if (self.semaResolvedSymMap.get(key)) |id| {
            return id;
        }
    } else {
        if (self.semaResolvedSymMap.get(key)) |id| {
            const funcKey = vm_.KeyU64{
                .absResolvedFuncSymKey = .{
                    .resolvedSymId = id,
                    .numParams = numParams,
                },
            };
            if (self.semaResolvedFuncSymMap.get(funcKey)) |_| {
                return id;
            }
        }
    }

    const relKey = vm_.KeyU64{
        .relModuleSymKey = .{
            .nameId = key.absResolvedSymKey.nameId,
            .numParams = numParams,
        },
    };
    if (self.modules.items[modId].syms.get(relKey)) |modSym| {
        if (modSym.symT == .nativeFunc1) {
            const resolvedParentId = key.absResolvedSymKey.resolvedParentSymId;
            const rtSymId = try self.vm.ensureFuncSym(resolvedParentId, key.absResolvedSymKey.nameId, numParams);
            const rtSym = cy.FuncSymbolEntry.initNativeFunc1(modSym.inner.nativeFunc1.func, numParams);
            self.vm.setFuncSym(rtSymId, rtSym);

            const res = try resolveSymAsFunc(self, key, numParams, NullId, AnyType);
            return res.resolvedSymId;
        } else if (modSym.symT == .variable) {
            const id = @intCast(u32, self.semaResolvedSyms.items.len);
            const resolvedParentId = key.absResolvedSymKey.resolvedParentSymId;
            const rtSymId = try self.vm.ensureVarSym(resolvedParentId, key.absResolvedSymKey.nameId);
            const rtSym = cy.VarSym.init(modSym.inner.variable.val);
            self.vm.retain(rtSym.value);
            self.vm.setVarSym(rtSymId, rtSym);
            try self.semaResolvedSyms.append(self.alloc, .{
                .symT = .variable,
                .inner = .{
                    .variable = .{
                        .declId = NullId,
                    },
                },
            });
            try self.semaResolvedSymMap.put(self.alloc, key, id);
            return id;
        } else {
            return self.reportErrorAt("Unsupported module sym {}", &.{fmt.v(modSym.symT)}, nodeId);
        }
    }
    return null;
}

fn getSym(self: *const cy.VMcompiler, parentId: ?u32, nameId: NameSymId, numParams: ?u16) ?SymId {
    const key = vm_.KeyU96{
        .absLocalSymKey = .{
            .localParentSymId = parentId orelse NullId,
            .nameId = nameId,
            .numParams = numParams orelse NullId,
        },
    };
    return self.semaSymMap.get(key);
}

pub fn ensureNameSym(c: *cy.VMcompiler, name: []const u8) !NameSymId {
    const res = try @call(.never_inline, c.semaNameSymMap.getOrPut, .{c.alloc, name});
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        const id = @intCast(u32, c.semaNameSyms.items.len);
        try c.semaNameSyms.append(c.alloc, name);
        res.value_ptr.* = id;
        return id;
    }
}

pub fn ensureSym(c: *cy.VMcompiler, parentId: ?SymId, nameId: NameSymId, numParams: ?u32) !SymId {
    const key = vm_.KeyU96{
        .absLocalSymKey = .{
            .localParentSymId = parentId orelse NullId,
            .nameId = nameId,
            .numParams = numParams orelse NullId,
        },
    };
    const res = try c.semaSymMap.getOrPut(c.alloc, key);
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        const id = @intCast(u32, c.semaSyms.items.len);
        try c.semaSyms.append(c.alloc, .{
            .key = key,
            .used = false,
            .visited = false,
        });
        res.value_ptr.* = id;
        return id;
    }
}

pub fn getVarName(self: *cy.VMcompiler, varId: LocalVarId) []const u8 {
    if (builtin.mode == .Debug) {
        return self.vars.items[varId].name;
    } else {
        return "";
    }
}

pub fn curSubBlock(self: *cy.VMcompiler) *SubBlock {
    return &self.semaSubBlocks.items[self.curSemaSubBlockId];
}

pub fn curBlock(self: *cy.VMcompiler) *Block {
    return &self.semaBlocks.items[self.curSemaBlockId];
}

pub fn endBlock(self: *cy.VMcompiler) !void {
    try endSubBlock(self);
    const sblock = curBlock(self);
    sblock.nameToVar.deinit(self.alloc);
    self.curSemaBlockId = sblock.prevBlockId;
    self.semaBlockDepth -= 1;
}

fn semaAccessExpr(self: *cy.VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) !Type {
    const node = self.nodes[nodeId];
    const right = self.nodes[node.head.accessExpr.right];
    if (right.node_t == .ident) {
        var left = self.nodes[node.head.accessExpr.left];
        if (left.node_t == .ident) {
            if ((try identLocalVarOrNull(self, node.head.accessExpr.left, true)) == null) {
                try setIdentAsVarSym(self, node.head.accessExpr.left);
            }

            left = self.nodes[node.head.accessExpr.left];
            if (left.head.ident.semaSymId != NullId) {
                const rightName = self.getNodeTokenString(right);
                const rightNameId = try ensureNameSym(self, rightName);
                const symId = try ensureSym(self, left.head.ident.semaSymId, rightNameId, null);
                self.semaSyms.items[symId].used = true;
                self.nodes[nodeId].head.accessExpr.semaSymId = symId;
            }
        } else if (left.node_t == .accessExpr) {
            _ = try semaAccessExpr(self, node.head.accessExpr.left, discardTopExprReg);

            left = self.nodes[node.head.accessExpr.left];
            if (left.head.accessExpr.semaSymId != NullId) {
                const rightName = self.getNodeTokenString(right);
                const rightNameId = try ensureNameSym(self, rightName);
                const symId = try ensureSym(self, left.head.accessExpr.semaSymId, rightNameId, null);
                self.semaSyms.items[symId].used = true;
                self.nodes[nodeId].head.accessExpr.semaSymId = symId;
            }
        } else {
            _ = try semaExpr(self, node.head.accessExpr.left, discardTopExprReg);
        }
    }
    return AnyType;
}

const VarResult = struct {
    id: LocalVarId,
    fromParentBlock: bool,
};

/// First checks current block and then the immediate parent block.
fn lookupVar(self: *cy.VMcompiler, name: []const u8, searchParentScope: bool) ?VarResult {
    const sblock = curBlock(self);
    if (sblock.nameToVar.get(name)) |varId| {
        return VarResult{
            .id = varId,
            .fromParentBlock = false,
        };
    }

    if (searchParentScope) {
        // Only check one block above.
        if (self.semaBlockDepth > 1) {
            const prev = self.semaBlocks.items[sblock.prevBlockId];
            if (prev.nameToVar.get(name)) |varId| {
                return VarResult{
                    .id = varId,
                    .fromParentBlock = true,
                };
            }
        }
    }

    // Undefined var.
    return null;
}

/// To a local type before assigning to a local variable.
fn toLocalType(vtype: Type) Type {
    if (vtype.typeT == .number and vtype.inner.number.canRequestInteger) {
        return NumberType;
    } else {
        return vtype;
    }
}

fn assignVar(self: *cy.VMcompiler, ident: cy.NodeId, vtype: Type, isLocalDecl: bool) !void {
    // log.debug("set var {s}", .{name});
    const node = self.nodes[ident];
    const name = self.getNodeTokenString(node);

    // canBeSymRoot=false and check if the sym is declared in else clause.
    if (try identLocalVarOrNull(self, ident, !isLocalDecl)) |varId| {
        const svar = &self.vars.items[varId];

        if (svar.isCaptured) {
            if (isLocalDecl) {
                // Create a new local var and update mapping so any references after will refer to the local var.
                const sblock = curBlock(self);
                _ = sblock.nameToVar.remove(name);
                const id = try pushLocalBodyVar(self, name, vtype);
                if (sblock.subBlockDepth > 1) {
                    self.vars.items[id].genInitializer = true;
                }
                self.nodes[ident].head.ident.semaVarId = id;

                try self.assignedVarStack.append(self.alloc, id);
                return;
            } else {
                if (!svar.isBoxed) {
                    // Becomes boxed so codegen knows ahead of time.
                    svar.isBoxed = true;
                }
            }
        }

        const ssblock = curSubBlock(self);
        if (!ssblock.prevVarTypes.contains(varId)) {
            // Same variable but branched to sub block.
            try ssblock.prevVarTypes.put(self.alloc, varId, svar.vtype);
            try self.assignedVarStack.append(self.alloc, varId);
        }

        // Update current type after checking for branched assignment.
        if (svar.vtype.typeT != vtype.typeT) {
            svar.vtype = toLocalType(vtype);
            if (!svar.lifetimeRcCandidate and vtype.rcCandidate) {
                svar.lifetimeRcCandidate = true;
            }
        }

        self.nodes[ident].head.ident.semaVarId = varId;
    } else {
        // If the ident already points to a sym, then skip creating a new local.
        const nameId = try ensureNameSym(self, name);
        if (getSym(self, null, nameId, null)) |symId| {
            self.semaSyms.items[symId].used = true;
            self.nodes[ident].head.ident.semaSymId = symId;
            return;
        }

        // Create a new local.
        const id = try pushLocalBodyVar(self, name, vtype);
        const sblock = curBlock(self);
        if (sblock.subBlockDepth > 1) {
            self.vars.items[id].genInitializer = true;
        }
        self.nodes[ident].head.ident.semaVarId = id;

        try self.assignedVarStack.append(self.alloc, id);
    }
}

fn opAssignVar(self: *cy.VMcompiler, ident: cy.NodeId, vtype: Type) !void {
    // log.debug("set var {s}", .{name});
    if (try identLocalVarOrNull(self, ident, true)) |varId| {
        const svar = &self.vars.items[varId];

        if (svar.isCaptured) {
            if (!svar.isBoxed) {
                // Becomes boxed so codegen knows ahead of time.
                svar.isBoxed = true;
            }
        }

        const ssblock = curSubBlock(self);
        if (!ssblock.prevVarTypes.contains(varId)) {
            // Same variable but branched to sub block.
            try ssblock.prevVarTypes.put(self.alloc, varId, svar.vtype);
            try self.assignedVarStack.append(self.alloc, varId);
        }

        // Update current type after checking for branched assignment.
        if (svar.vtype.typeT != vtype.typeT) {
            svar.vtype = toLocalType(vtype);
            if (!svar.lifetimeRcCandidate and vtype.rcCandidate) {
                svar.lifetimeRcCandidate = true;
            }
        }

        self.nodes[ident].head.ident.semaVarId = varId;
    } else {
        try setIdentAsVarSym(self, ident);
    }
}

fn endSubBlock(self: *cy.VMcompiler) !void {
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
                if (svar.vtype.typeT != prevt.typeT) {
                    svar.vtype = AnyType;

                    // Previous sub block hasn't recorded the var assignment.
                    if (!pssblock.prevVarTypes.contains(varId)) {
                        try self.assignedVarStack.append(self.alloc, varId);
                    }
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

fn pushIterSubBlock(self: *cy.VMcompiler) !void {
    try pushSubBlock(self);
}

fn endIterSubBlock(self: *cy.VMcompiler) !void {
    const ssblock = curSubBlock(self);
    for (self.assignedVarStack.items[ssblock.assignedVarStart..]) |varId| {
        const svar = self.vars.items[varId];
        if (ssblock.prevVarTypes.get(varId)) |prevt| {
            if (svar.vtype.typeT != prevt.typeT) {
                // Type differs from prev scope type. Record change for iter block codegen.
                try ssblock.iterVarBeginTypes.append(self.alloc, .{
                    .id = varId,
                    .vtype = AnyType,
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

pub fn importAllFromModule(self: *cy.VMcompiler, modId: ModuleId) !void {
    const mod = self.modules.items[modId];
    var iter = mod.syms.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*.relModuleSymKey;
        
        const symId = try ensureSym(self, null, key.nameId, if (key.numParams == NullId) null else key.numParams);
        try self.semaSymToRef.put(self.alloc, symId, .{
            .refT = .moduleMember,
            .inner = .{
                .moduleMember = .{
                    .modId = modId,
                }
            }
        });

        // For functions, also create a sym mapping if it doesn't exist already (no function overloading),
        // so any local sym with the same name can match it.
        if (key.numParams != NullId) {
            const nameSymId = try ensureSym(self, null, key.nameId, NullId);
            if (!self.semaSymToRef.contains(nameSymId)) {
                try self.semaSymToRef.put(self.alloc, nameSymId, .{
                    .refT = .moduleFuncMember,
                    .inner = .{
                        .moduleFuncMember = .{
                            .modId = modId,
                            .numParams = key.numParams,
                        },
                    },
                });
            }
        }
    }
}

pub fn getOrLoadModule(self: *cy.VMcompiler, spec: []const u8, nodeId: cy.NodeId) !ModuleId {
    const res = try self.moduleMap.getOrPut(self.alloc, spec);
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        const mod = try loadModule(self, spec, nodeId);
        const id = @intCast(u32, self.modules.items.len);
        try self.modules.append(self.alloc, mod);
        res.key_ptr.* = spec;
        res.value_ptr.* = id;
        return id;
    }
}

fn loadModule(self: *cy.VMcompiler, spec: []const u8, nodeId: cy.NodeId) !Module {
    // Builtin modules.
    if (std.mem.eql(u8, "test", spec)) {
        return test_mod.initModule(self, spec);
    } else if (std.mem.eql(u8, "math", spec)) {
        return math_mod.initModule(self, spec);
    } else if (std.mem.eql(u8, "core", spec)) {
        return core_mod.initModule(self, spec);
    } else if (std.mem.eql(u8, "os", spec)) {
        return os_mod.initModule(self, spec);
    } else {
        return self.reportErrorAt("Unsupported import. {}", &.{fmt.v(spec)}, nodeId);
    }
}

/// Given the local sym path, add a resolved object sym entry.
/// Assumes parent is resolved.
fn resolveLocalObjectSym(self: *cy.VMcompiler, parentId: ?u32, name: []const u8, declId: cy.NodeId) !u32 {
    const nameId = try ensureNameSym(self, name);
    const key = vm_.KeyU64{
        .absResolvedSymKey = .{
            .resolvedParentSymId = if (parentId == null) NullId else self.semaSyms.items[parentId.?].resolvedSymId,
            .nameId = nameId,
        },
    };
    if (self.semaResolvedSymMap.contains(key)) {
        return self.reportErrorAt("The symbol `{}` was already declared.", &.{v(name)}, declId);
    }

    // Resolve the symbol.
    const resolvedId = @intCast(u32, self.semaResolvedSyms.items.len);
    try self.semaResolvedSyms.append(self.alloc, .{
        .symT = .object,
        .inner = .{
            .object = .{
                .declId = declId,
            },
        },
    });
    try @call(.never_inline, self.semaResolvedSymMap.put, .{self.alloc, key, resolvedId});

    const symId = try ensureSym(self, parentId, nameId, null);
    self.semaSyms.items[symId].resolvedSymId = resolvedId;
    return symId;
}

/// Given the local sym path, add a resolved var sym entry.
/// Fail if there is already a symbol in this path.with the same name.
fn resolveLocalVarSym(self: *cy.VMcompiler, parentId: ?u32, name: []const u8, declId: cy.NodeId) !u32 {
    const nameId = try ensureNameSym(self, name);
    const key = vm_.KeyU64{
        .absResolvedSymKey = .{
            .resolvedParentSymId = if (parentId == null) NullId else self.semaSyms.items[parentId.?].resolvedSymId,
            .nameId = nameId,
        },
    };
    if (self.semaResolvedSymMap.contains(key)) {
        return self.reportErrorAt("The symbol `{}` was already declared.", &.{v(name)}, declId);
    }

    // Resolve the symbol.
    const resolvedId = @intCast(u32, self.semaResolvedSyms.items.len);
    try self.semaResolvedSyms.append(self.alloc, .{
        .symT = .variable,
        .inner = .{
            .variable = .{
                .declId = declId,
            },
        },
    });
    try @call(.never_inline, self.semaResolvedSymMap.put, .{self.alloc, key, resolvedId});

    // Link to local symbol.
    const symId = try ensureSym(self, parentId, nameId, null);
    self.semaSyms.items[symId].resolvedSymId = resolvedId;
    return symId;
}

const ResolveFuncSymResult = struct {
    resolvedSymId: ResolvedSymId,
    resolvedFuncSymId: ResolvedFuncSymId,
};

fn resolveSymAsFunc(c: *cy.VMcompiler, key: AbsResolvedSymKey, numParams: u32, declId: u32, retType: Type) !ResolveFuncSymResult {
    var rsymId: ResolvedSymId = undefined;
    var createdSym = false;
    if (c.semaResolvedSymMap.get(key)) |id| {
        const rsym = c.semaResolvedSyms.items[id];
        if (rsym.symT != .func) {
            // Only fail if the symbol already exists and isn't a function.
            return c.reportError("The symbol `{}` was already declared.", &.{v(getName(c, key.absResolvedSymKey.nameId))});
        }
        rsymId = id;
    } else {
        rsymId = @intCast(u32, c.semaResolvedSyms.items.len);
        try c.semaResolvedSyms.append(c.alloc, .{
            .symT = .func,
            .inner = .{
                .func = .{
                    .resolvedFuncSymId = undefined,
                },
            },
        });
        try @call(.never_inline, c.semaResolvedSymMap.put, .{c.alloc, key, rsymId});
        createdSym = true;
    }

    // Now check resolved function syms.
    const funcKey = vm_.KeyU64{
        .absResolvedFuncSymKey = .{
            .resolvedSymId = rsymId,
            .numParams = numParams,
        },
    };
    if (c.semaResolvedFuncSymMap.contains(funcKey)) {
        return c.reportError("The function symbol `{}` with the same signature was already declared.", &.{v(key.absResolvedSymKey.nameId)});
    }

    // Resolve the func symbol.
    const rfsymId = @intCast(u32, c.semaResolvedFuncSyms.items.len);
    try c.semaResolvedFuncSyms.append(c.alloc, .{
        .declId = declId,
        .numParams = @intCast(u8, numParams),
        .retType = retType,
    });
    try @call(.never_inline, c.semaResolvedFuncSymMap.put, .{c.alloc, funcKey, rfsymId});

    if (createdSym) {
        c.semaResolvedSyms.items[rsymId].inner.func.resolvedFuncSymId = rfsymId;
    } else {
        // Mark sym as overloaded.
        c.semaResolvedSyms.items[rsymId].inner.func.resolvedFuncSymId = NullId;
    }

    return ResolveFuncSymResult{
        .resolvedSymId = rsymId,
        .resolvedFuncSymId = rfsymId,
    };
}

/// Given the local sym path, add a resolved func sym entry.
/// Assumes parent local sym is resolved.
fn resolveLocalFuncSym(self: *cy.VMcompiler, resolvedParentSymId: ?ResolvedSymId, nameId: NameSymId, declId: u32, retType: Type) !ResolveFuncSymResult {
    const func = self.funcDecls[declId];
    const numParams = func.params.len();

    const key = vm_.KeyU64{
        .absResolvedSymKey = .{
            .resolvedParentSymId = resolvedParentSymId orelse NullId,
            .nameId = nameId,
        },
    };
    return try resolveSymAsFunc(self, key, numParams, declId, retType);
}

fn endFuncBlock(self: *cy.VMcompiler, numParams: u32) !void {
    const sblock = curBlock(self);
    const numCaptured = @intCast(u8, sblock.params.items.len - numParams);
    if (numCaptured > 0) {
        for (sblock.params.items) |varId| {
            const svar = self.vars.items[varId];
            if (svar.isCaptured) {
                const pId = self.capVarDescs.get(varId).?.user;
                const pvar = &self.vars.items[pId];

                if (!pvar.isBoxed) {
                    pvar.isBoxed = true;
                    pvar.lifetimeRcCandidate = true;
                }
            }
        }
    }
    try endBlock(self);
}

fn endFuncSymBlock(self: *cy.VMcompiler, symId: u32, numParams: u32) !void {
    const sblock = curBlock(self);
    const numCaptured = @intCast(u8, sblock.params.items.len - numParams);
    // Update original var to boxed.
    if (numCaptured > 0) {
        for (sblock.params.items) |varId, i| {
            const svar = self.vars.items[varId];
            if (svar.isCaptured) {
                const pId = self.capVarDescs.get(varId).?.user;
                const pvar = &self.vars.items[pId];

                if (!pvar.isBoxed) {
                    pvar.isBoxed = true;
                    pvar.lifetimeRcCandidate = true;
                }

                try ensureCapVarOwner(self, pId);
                const owner = self.capVarDescs.getPtr(pId).?;

                // Add func sym as dependent.
                const dataId = @intCast(u32, self.dataNodes.items.len);
                try self.dataNodes.append(self.alloc, .{
                    .inner = .{
                        .funcSym = .{
                            .symId = @intCast(u24, symId),
                            .capVarIdx = @intCast(u8, i - numParams),
                        },
                    },
                    .next = owner.owner,
                });
                owner.owner = dataId;
            }
        }
    }
    try endBlock(self);
}

test "Internals." {
    try t.eq(@sizeOf(LocalVar), 32);
    try t.eq(@sizeOf(Sym), 24);
    try t.eq(@sizeOf(ResolvedFuncSym), 8);
    try t.eq(@sizeOf(ResolvedSym), 8);
    try t.eq(@sizeOf(Type), 3);
}