const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const cc = @import("clib.zig");
const ir = cy.ir;
const vmc = @import("vm_c.zig");
const rt = cy.rt;
const types = cy.types;
const bt = types.BuiltinTypes;
const Nullable = cy.Nullable;
const fmt = cy.fmt;
const v = fmt.v;
const module = cy.module;

const ChunkId = cy.chunk.ChunkId;
const TypeId = types.TypeId;
const CompactType = types.CompactType;
const Sym = cy.Sym;

const vm_ = @import("vm.zig");

const log = cy.log.scoped(.sema);

const RegisterId = cy.register.RegisterId;

pub const LocalVarId = u32;

const LocalVarType = enum(u8) {
    /// Local var or param.
    local, 

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
/// TODO: This should be SemaVar since it includes all vars not just locals.
pub const LocalVar = struct {
    type: LocalVarType,

    declT: TypeId,

    /// This tracks the most recent type as the ast is traversed.
    /// This is updated when there is a variable assignment or a child block returns.
    vtype: CompactType,

    /// Last sub-block that mutated the dynamic var.
    dynamicLastMutSubBlockId: SubBlockId,

    /// Local register offset assigned to this var.
    /// Locals are relative to the stack frame's start position.
    local: RegisterId = undefined,

    inner: union {
        staticAlias: *Sym,
        local: struct {
            /// Id emitted in IR code. Starts from 0 after params in the current block.
            /// Aliases do not have an id.
            id: u8,

            /// If `isParam` is true, `id` refers to the param idx.
            isParam: bool,
            /// If a param is written to or turned into a Box, `copied` becomes true.
            isParamCopied: bool,

            /// Currently a captured var always needs to be boxed.
            /// In the future, the concept of a immutable variable could change this.
            isBoxed: bool,

            /// For locals this points to a ir.DeclareLocal.
            /// This is NullId for params.
            declIrStart: u32,
        },
        objectMemberAlias: struct {
            fieldIdx: u8,
        },
        parentObjectMemberAlias: struct {
            selfCapturedIdx: u8,
            fieldIdx: u8,
        },
        parentLocalAlias: struct {
            capturedIdx: u8,
        },
        unDefined: void,
    } = .{ .unDefined = {} },

    nameLen: u16,
    namePtr: [*]const u8,

    /// Whether the variable is dynamic or statically typed.
    /// This should provide the same result as `vtype.dynamic`.
    pub fn isDynamic(self: LocalVar) bool {
        return self.declT == bt.Dynamic;
    }

    pub fn name(self: LocalVar) []const u8 {
        return self.namePtr[0..self.nameLen];
    }

    pub inline fn isParentLocalAlias(self: LocalVar) bool {
        return self.type == .parentLocalAlias;
    }

    pub inline fn isCapturable(self: LocalVar) bool {
        return self.type == .local;
    }
};

const VarSubBlock = extern struct {
    varId: LocalVarId,
    subBlockId: SubBlockId,
};

pub const VarShadow = extern struct {
    namePtr: [*]const u8,
    nameLen: u32,
    varId: LocalVarId,
    subBlockId: SubBlockId,
};

pub const NameVar = extern struct {
    namePtr: [*]const u8,
    nameLen: u32,
    varId: LocalVarId,
};

pub const CapVarDesc = extern union {
    /// The user of a captured var contains the SemaVarId back to the owner's var.
    user: LocalVarId,
};

pub const PreLoopVarSave = packed struct {
    vtype: CompactType,
    varId: LocalVarId,
};

pub const VarAndType = struct {
    id: LocalVarId,
    vtype: TypeId,
};

pub const SubBlockId = u32;

pub const SubBlock = struct {
    /// Track which vars were assigned to in the current sub block.
    /// If the var was first assigned in a parent sub block, the type is saved in the map to
    /// be merged later with the ending var type.
    /// Can be freed after the end of block.
    prevVarTypes: std.AutoHashMapUnmanaged(LocalVarId, CompactType),

    /// Start of vars assigned in this block in `assignedVarStack`.
    /// When leaving this block, all assigned var types in this block are merged
    /// back to the parent scope.
    assignedVarStart: u32,

    /// Start of local vars in this sub-block in `varStack`.
    varStart: u32,

    /// Start of shadowed vars from the previous sub-block in `varShadowStack`.
    varShadowStart: u32,

    preLoopVarSaveStart: u32, 

    /// Node that began the sub-block.
    nodeId: cy.NodeId,

    /// Whether execution can reach the end.
    /// If a return statement was generated, this would be set to false.
    endReachable: bool = true,

    /// Tracks how many locals are owned by this sub-block.
    /// When the sub-block is popped, this is subtracted from the block's `curNumLocals`.
    numLocals: u8,

    pub fn init(nodeId: cy.NodeId, assignedVarStart: usize, varStart: usize, varShadowStart: usize) SubBlock {
        return .{
            .nodeId = nodeId,
            .assignedVarStart = @intCast(assignedVarStart),
            .varStart = @intCast(varStart),
            .varShadowStart = @intCast(varShadowStart),
            .preLoopVarSaveStart = 0,
            .prevVarTypes = .{},
            .numLocals = 0,
        };
    }

    pub fn deinit(self: *SubBlock, alloc: std.mem.Allocator) void {
        self.prevVarTypes.deinit(alloc);
    }
};

pub const BlockId = u32;

pub const Block = struct {
    /// `varStack[varStart]` is the start of params.
    numParams: u8,

    /// Captured vars. 
    captures: std.ArrayListUnmanaged(LocalVarId),

    /// Maps a name to a var.
    /// This is updated as sub-blocks declare their own locals and restored
    /// when sub-blocks end.
    /// This can be deinited after ending the sema block.
    /// TODO: Move to chunk level.
    nameToVar: std.StringHashMapUnmanaged(VarSubBlock),

    /// First sub block id is recorded so the rest can be obtained by advancing
    /// the id in the same order it was traversed in the sema pass.
    firstSubBlockId: SubBlockId,

    /// Current sub block depth.
    subBlockDepth: u32,

    /// Main block if `NullId`.
    func: ?*cy.Func,

    /// Whether this block belongs to a static function.
    isStaticFuncBlock: bool,

    /// Whether this is a method block.
    isMethodBlock: bool = false,

    /// Track max locals so that codegen knows where stack registers begin.
    /// Includes function params.
    maxLocals: u8,

    /// Everytime a new local is encountered in a sub-block, this is incremented by one.
    /// At the end of the sub-block `maxLocals` is updated and this unwinds by subtracting its numLocals.
    /// Includes function params.
    curNumLocals: u8,

    /// All locals currently alive in this block.
    /// Every local above the current sub-block is included.
    varStart: u32,

    /// Where the IR starts for this block.
    irStart: u32,

    nodeId: cy.NodeId,

    resetVerboseOnBlockEnd: bool,

    pub fn init(nodeId: cy.NodeId, func: ?*cy.Func, firstSubBlockId: SubBlockId, isStaticFuncBlock: bool, varStart: u32) Block {
        return .{
            .nameToVar = .{},
            .numParams = 0,
            .subBlockDepth = 0,
            .func = func,
            .firstSubBlockId = firstSubBlockId,
            .isStaticFuncBlock = isStaticFuncBlock,
            .nodeId = nodeId,
            .captures = .{},
            .maxLocals = 0,
            .curNumLocals = 0,
            .varStart = varStart,
            .irStart = cy.NullId,
            .resetVerboseOnBlockEnd = false,
        };
    }

    pub fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        self.nameToVar.deinit(alloc);
        self.captures.deinit(alloc);
    }

    fn getReturnType(self: *const Block) !TypeId {
        if (self.func) |func| {
            return func.retType;
        } else {
            return bt.Any;
        }
    }
};

pub const ModFuncSigKey = cy.hash.KeyU96;
const ObjectMemberKey = cy.hash.KeyU64;

pub fn semaStmts(c: *cy.Chunk, head: cy.NodeId) anyerror!void {
    var curId = head;
    while (curId != cy.NullId) {
        try semaStmt(c, curId);
        const node = c.nodes[curId];
        curId = node.next;
    }
}

pub fn semaStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    c.curNodeId = nodeId;
    const node = c.nodes[nodeId];
    if (cy.Trace) {
        var buf: [1024]u8 = undefined;
        var fbuf = std.io.fixedBufferStream(&buf);
        try c.encoder.writeNode(fbuf.writer(), nodeId);
        log.tracev("stmt.{s}: \"{s}\"", .{@tagName(c.nodes[nodeId].node_t), fbuf.getWritten()});
    }
    switch (node.node_t) {
        .exprStmt => {
            const returnMain = node.head.exprStmt.isLastRootStmt;
            _ = try c.irPushStmt(.exprStmt, nodeId, .{ .returnMain = returnMain });
            _ = try c.semaExpr(node.head.exprStmt.child, .{});
        },
        .breakStmt => {
            _ = try c.irPushStmt(.breakStmt, nodeId, {});
        },
        .continueStmt => {
            _ = try c.irPushStmt(.contStmt, nodeId, {});
        },
        .localDecl => {
            _ = try localDecl(c, nodeId);
        },
        .opAssignStmt => {
            const op = node.head.opAssignStmt.op;
            switch (op) {
                .star,
                .slash,
                .percent,
                .caret,
                .plus,
                .minus => {},
                else => {
                    return c.reportErrorAt("Unsupported op assign statement for {}.", &.{v(op)}, nodeId);
                }
            }

            const irIdx = try c.irPushStmt(.opSet, nodeId, .{ .op = op });

            const leftId = node.head.opAssignStmt.left;
            try assignStmt(c, nodeId, leftId, nodeId, .{ .rhsOpAssignBinExpr = true });

            // Reset `opSet`'s next since pushed statements are appended to the top StmtBlock.
            c.irSetNextStmt(irIdx, cy.NullId);
            c.irStmtBlockStack.items[c.irStmtBlockStack.items.len-1].last = irIdx;
        },
        .assign_stmt => {
            try assignStmt(c, nodeId, node.head.left_right.left, node.head.left_right.right, .{});
        },
        .hostObjectDecl,
        .objectDecl,
        .pass_stmt,
        .hostVarDecl,
        .hostFuncDecl,
        .staticDecl,
        .typeAliasDecl,
        .enumDecl,
        .funcDecl,
        .funcDeclInit => {
            // Nop.
        },
        .forIterStmt => {
            const header = c.nodes[node.head.forIterStmt.header];

            try preLoop(c, nodeId);
            const irIdx = try c.irPushEmptyStmt(.forIterStmt, nodeId);
            _ = try c.semaExpr(header.head.forIterHeader.iterable, .{});
            try pushSubBlock(c, nodeId);

            var eachLocal: ?u8 = null;
            var countLocal: ?u8 = null;
            var hasSeqDestructure = false;
            var seqIrVarStart: u32 = undefined;
            if (header.head.forIterHeader.eachClause != cy.NullId) {
                const eachClause = c.nodes[header.head.forIterHeader.eachClause];
                if (eachClause.node_t == .ident) {
                    const varId = try declareLocal(c, header.head.forIterHeader.eachClause, bt.Dynamic, false);
                    eachLocal = c.varStack.items[varId].inner.local.id;

                    if (header.head.forIterHeader.count != cy.NullId) {
                        const countVarId = try declareLocal(c, header.head.forIterHeader.count, bt.Integer, false);
                        countLocal = c.varStack.items[countVarId].inner.local.id;
                    }
                } else if (eachClause.node_t == .seqDestructure) {
                    const varId = try declareLocalName(c, "$elem", bt.Dynamic, false, header.head.forIterHeader.eachClause);
                    eachLocal = c.varStack.items[varId].inner.local.id;

                    if (header.head.forIterHeader.count != cy.NullId) {
                        const countVarId = try declareLocal(c, header.head.forIterHeader.count, bt.Integer, false);
                        countLocal = c.varStack.items[countVarId].inner.local.id;
                    }

                    var curId = eachClause.head.seqDestructure.head;

                    seqIrVarStart = @intCast(c.dataU8Stack.items.len);
                    while (curId != cy.NullId) {
                        const varId2 = try declareLocal(c, curId, bt.Dynamic, false);
                        const irVarId = c.varStack.items[varId2].inner.local.id;
                        try c.dataU8Stack.append(c.alloc, .{ .irLocal = irVarId });
                        const cur = c.nodes[curId];
                        curId = cur.next;
                    }
                    hasSeqDestructure = true;
                } else {
                    return c.reportErrorAt("Unsupported each clause: {}", &.{v(eachClause.node_t)}, header.head.forIterHeader.eachClause);
                }
            }

            // End decl statements and begin collecting body statements.
            const declHead = c.irStmtBlockStack.getLast().first;
            c.irStmtBlockStack.items[c.irStmtBlockStack.items.len-1].first = cy.NullId;
            c.irStmtBlockStack.items[c.irStmtBlockStack.items.len-1].last = cy.NullId;

            // Begin body code.
            if (hasSeqDestructure) {
                const eachClause = c.nodes[header.head.forIterHeader.eachClause];
                const destrIdx = try c.irPushEmptyStmt(.destrElemsStmt, header.head.forIterHeader.eachClause);
                const locals = c.dataU8Stack.items[seqIrVarStart..];
                const irStart = c.irBuf.items.len;
                try c.irBuf.resize(c.alloc, c.irBuf.items.len + locals.len);
                for (locals, 0..) |local, i| {
                    c.irBuf.items[irStart + i] = local.irLocal;
                }
                const right = try c.irPushExpr(.local, header.head.forIterHeader.eachClause, .{ .id = eachLocal.? });
                c.irSetStmtData(destrIdx, .destrElemsStmt, .{
                    .numLocals = eachClause.head.seqDestructure.numArgs,
                    .right = right,
                });
                c.dataU8Stack.items.len = seqIrVarStart;
            }

            try semaStmts(c, node.head.forIterStmt.bodyHead);
            const stmtBlock = try popLoopSubBlock(c);

            c.irSetStmtData(irIdx, .forIterStmt, .{
                .eachLocal = eachLocal, .countLocal = countLocal, .declHead = declHead, .bodyHead = stmtBlock.first,
            });
        },
        .forRangeStmt => {
            try preLoop(c, nodeId);
            const irIdx = try c.irPushEmptyStmt(.forRangeStmt, nodeId);

            const rangeClause = c.nodes[node.head.forRangeStmt.range_clause];
            _ = try c.semaExpr(rangeClause.head.forRange.left, .{});
            const rangeEnd = try c.semaExpr(rangeClause.head.forRange.right, .{});

            try pushSubBlock(c, nodeId);

            const hasEach = node.head.forRangeStmt.eachClause != cy.NullId;
            var eachLocal: ?u8 = null;
            if (hasEach) {
                const eachClause = c.nodes[node.head.forRangeStmt.eachClause];
                if (eachClause.node_t == .ident) {
                    const varId = try declareLocal(c, node.head.forRangeStmt.eachClause, bt.Integer, false);
                    eachLocal = c.varStack.items[varId].inner.local.id;
                } else {
                    return c.reportErrorAt("Unsupported each clause: {}", &.{v(eachClause.node_t)}, node.head.forRangeStmt.eachClause);
                }
            }

            const declHead = c.irStmtBlockStack.getLast().first;
            c.irStmtBlockStack.items[c.irStmtBlockStack.items.len-1].first = cy.NullId;
            c.irStmtBlockStack.items[c.irStmtBlockStack.items.len-1].last = cy.NullId;

            try semaStmts(c, node.head.forRangeStmt.body_head);
            const stmtBlock = try popLoopSubBlock(c);
            c.irSetStmtData(irIdx, .forRangeStmt, .{
                .eachLocal = eachLocal,
                .rangeEnd = rangeEnd.irIdx,
                .bodyHead = stmtBlock.first,
                .increment = rangeClause.head.forRange.increment,
                .declHead = declHead,
            });
        },
        .whileInfStmt => {
            try preLoop(c, node.head.child_head);
            const irIdx = try c.irPushEmptyStmt(.whileInfStmt, nodeId);
            try pushSubBlock(c, nodeId);
            try semaStmts(c, node.head.child_head);
            const stmtBlock = try popLoopSubBlock(c);
            c.irSetStmtData(irIdx, .whileInfStmt, .{
                .bodyHead = stmtBlock.first,
            });
        },
        .whileCondStmt => {
            try preLoop(c, nodeId);
            const irIdx = try c.irPushEmptyStmt(.whileCondStmt, nodeId);

            _ = try c.semaExpr(node.head.whileCondStmt.cond, .{});
            try pushSubBlock(c, nodeId);

            try semaStmts(c, node.head.whileCondStmt.bodyHead);

            const stmtBlock = try popLoopSubBlock(c);
            c.irSetStmtData(irIdx, .whileCondStmt, .{
                .bodyHead = stmtBlock.first,
            });
        },
        .whileOptStmt => {
            try preLoop(c, nodeId);
            const irIdx = try c.irPushEmptyStmt(.whileOptStmt, nodeId);
            const opt = try c.semaExpr(node.head.whileOptStmt.opt, .{});

            try pushSubBlock(c, nodeId);
            if (node.head.whileOptStmt.some == cy.NullId) {
                return c.reportErrorAt("Missing variable declaration for `some`.", &.{}, nodeId);
            }
            const declT = if (opt.type.dynamic) bt.Dynamic else bt.Any;
            const id = try declareLocal(c, node.head.whileOptStmt.some, declT, false);
            const someLocal: u8 = @intCast(c.varStack.items[id].inner.local.id);

            const declHead = c.irStmtBlockStack.getLast().first;
            c.irStmtBlockStack.items[c.irStmtBlockStack.items.len-1].first = cy.NullId;
            c.irStmtBlockStack.items[c.irStmtBlockStack.items.len-1].last = cy.NullId;

            try semaStmts(c, node.head.whileOptStmt.bodyHead);

            const stmtBlock = try popLoopSubBlock(c);
            c.irSetStmtData(irIdx, .whileOptStmt, .{
                .someLocal = someLocal,
                .capIdx = declHead,
                .bodyHead = stmtBlock.first,
            });
        },
        .switchBlock => {
            _ = try c.irPushStmt(.switchStmt, nodeId, {});
            _ = try c.semaSwitch(nodeId, false);
        },
        .ifStmt => {
            const irIdx = try c.irPushEmptyStmt(.ifStmt, nodeId);
            _ = try c.semaExpr(node.head.ifStmt.cond, .{});

            const irElsesIdx = try c.irPushEmptyArray(u32, node.head.ifStmt.numElseBlocks);

            try pushSubBlock(c, nodeId);
            try semaStmts(c, node.head.ifStmt.bodyHead);
            const ifStmtBlock = try popSubBlock(c);

            var elseBlockId = node.head.ifStmt.elseHead;
            var i: u32 = 0;
            // var hasElse = false;
            while (elseBlockId != cy.NullId) {
                const elseBlock = c.nodes[elseBlockId];
                const isElse = elseBlock.head.elseBlock.cond == cy.NullId;
                if (isElse) {
                    // hasElse = true;
                    const irElseIdx = try c.irPushEmptyExpr(.elseBlock, elseBlockId);
                    try pushSubBlock(c, elseBlockId);
                    try semaStmts(c, elseBlock.head.elseBlock.bodyHead);
                    const stmtBlock = try popSubBlock(c);

                    c.irSetArrayItem(irElsesIdx, u32, i, irElseIdx);
                    c.irSetExprData(irElseIdx, .elseBlock, .{ .isElse = true, .bodyHead = stmtBlock.first });

                    elseBlockId = elseBlock.next;
                    if (elseBlockId != cy.NullId) {
                        return c.reportErrorAt("Can not have additional blocks after `else`.", &.{}, elseBlockId);
                    }
                    break;
                } else {
                    const irElseIdx = try c.irPushEmptyExpr(.elseBlock, elseBlockId);
                    _ = try c.semaExpr(elseBlock.head.elseBlock.cond, .{});

                    try pushSubBlock(c, elseBlockId);
                    try semaStmts(c, elseBlock.head.elseBlock.bodyHead);
                    const stmtBlock = try popSubBlock(c);

                    c.irSetArrayItem(irElsesIdx, u32, i, irElseIdx);
                    c.irSetExprData(irElseIdx, .elseBlock, .{ .isElse = false, .bodyHead = stmtBlock.first });

                    elseBlockId = elseBlock.next;
                    i += 1;
                    continue;
                }
            }

            c.irSetStmtData(irIdx, .ifStmt, .{
                .bodyHead = ifStmtBlock.first,
                .numElseBlocks = node.head.ifStmt.numElseBlocks,
                .elseBlocks = irElsesIdx,
            });
        },
        .tryStmt => {
            var data: ir.TryStmt = undefined;
            const irIdx = try c.irPushEmptyStmt(.tryStmt, nodeId);
            try pushSubBlock(c, nodeId);
            try semaStmts(c, node.head.tryStmt.tryFirstStmt);
            var stmtBlock = try popSubBlock(c);
            data.bodyHead = stmtBlock.first;

            try pushSubBlockCommon(c, nodeId);
            if (node.head.tryStmt.errorVar != cy.NullId) {
                const id = try declareLocal(c, node.head.tryStmt.errorVar, bt.Error, false);
                data.hasErrLocal = true;
                data.errLocal = c.varStack.items[id].inner.local.id;
            } else {
                data.hasErrLocal = false;
            }
            try semaStmts(c, node.head.tryStmt.catchFirstStmt);
            stmtBlock = try popSubBlockCommon(c);
            data.catchBodyHead = stmtBlock.first;

            c.irSetStmtData(irIdx, .tryStmt, data);
        },
        .importStmt => {
            return;
        },
        .return_stmt => {
            _ = try c.irPushStmt(.retStmt, nodeId, {});
        },
        .return_expr_stmt => {
            const block = curBlock(c);
            const retType = try block.getReturnType();

            _ = try c.irPushStmt(.retExprStmt, nodeId, {});
            _ = try c.semaExprCstr(node.head.child_head, retType);
        },
        .comptimeStmt => {
            const expr = c.nodes[node.head.comptimeStmt.expr];
            if (expr.node_t == .callExpr) {
                const callee = c.nodes[expr.head.callExpr.callee];
                const name = c.getNodeString(callee);

                if (std.mem.eql(u8, "genLabel", name)) {
                    if (expr.head.callExpr.numArgs != 1) {
                        return c.reportErrorAt("genLabel expected 1 arg", &.{}, nodeId);
                    }

                    const arg = c.nodes[expr.head.callExpr.arg_head];
                    if (arg.node_t != .string) {
                        return c.reportErrorAt("genLabel expected string arg", &.{}, nodeId);
                    }

                    const label = c.getNodeString(arg);
                    _ = try c.irPushStmt(.pushDebugLabel, nodeId, .{ .name = label });
                } else if (std.mem.eql(u8, "dumpLocals", name)) {
                    const block = curBlock(c);
                    try c.dumpLocals(block);
                } else if (std.mem.eql(u8, "dumpBytecode", name)) {
                    _ = try c.irPushStmt(.dumpBytecode, nodeId, {});
                } else if (std.mem.eql(u8, "verbose", name)) {
                    if (cy.Trace and !cy.verbose) {
                        cy.verbose = true;
                        curBlock(c).resetVerboseOnBlockEnd = true;
                    }
                    _ = try c.irPushStmt(.verbose, nodeId, {});
                } else {
                    return c.reportErrorAt("Unsupported annotation: {}", &.{v(name)}, nodeId);
                }
            } else {
                return c.reportErrorAt("Unsupported expr: {}", &.{v(expr.node_t)}, nodeId);
            }
        },
        else => return c.reportErrorAt("Unsupported statement: {}", &.{v(node.node_t)}, nodeId),
    }
}

const AssignOptions = struct {
    rhsOpAssignBinExpr: bool = false,
};

/// Pass rightId explicitly to perform custom sema on op assign rhs.
fn assignStmt(c: *cy.Chunk, nodeId: cy.NodeId, leftId: cy.NodeId, rightId: cy.NodeId, opts: AssignOptions) !void {
    const left = c.nodes[leftId];
    switch (left.node_t) {
        .indexExpr => {
            const irStart = try c.irPushEmptyStmt(.set, nodeId);

            const recv = try c.semaExpr(left.head.indexExpr.left, .{});
            var specialized = false;
            var index: ExprResult = undefined;
            if (recv.type.id == bt.List) {
                index = try c.semaExprPrefer(left.head.indexExpr.right, bt.Integer);
                specialized = true;
            } else if (recv.type.id == bt.Map) {
                index = try c.semaExpr(left.head.indexExpr.right, .{});
                specialized = true;
            } else {
                index = try c.semaExpr(left.head.indexExpr.right, .{});
            }

            // RHS.
            const rightExpr = Expr.init(rightId, bt.Any);
            const right = try c.semaExprOrOpAssignBinExpr(rightExpr, opts.rhsOpAssignBinExpr);

            if (specialized) {
                c.irSetStmtCode(irStart, .setIndex);
                c.irSetStmtData(irStart, .setIndex, .{
                    .index = .{
                        .recvT = recv.type.id,
                        .index = index.irIdx,
                        .right = right.irIdx,
                    },
                });
            } else {
                const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any, recv.type.id, right.type.id }, bt.Any);
                c.irSetStmtCode(irStart, .setCallObjSymTern);
                c.irSetStmtData(irStart, .setCallObjSymTern, .{
                    .callObjSymTern = .{
                        // .recvT = recvT.id,
                        .name = "$setIndex",
                        .funcSigId = funcSigId,
                        .index = index.irIdx,
                        .right = right.irIdx,
                    },
                });
            }
        },
        .ident,
        .accessExpr => {
            const irStart = try c.irPushEmptyStmt(.set, nodeId);

            var right: ExprResult = undefined;
            const leftRes = try c.semaExpr(leftId, .{});
            const leftT = leftRes.type;
            if (leftRes.resType == .local) {
                right = try assignToLocalVar(c, leftRes, rightId, opts);
            } else {
                const rightExpr = Expr{ .nodeId = rightId, .hasTypeCstr = !leftT.dynamic, .preferType = leftT.id };
                right = try c.semaExprOrOpAssignBinExpr(rightExpr, opts.rhsOpAssignBinExpr);
            }

            c.irSetStmtData(irStart, .set, .{ .generic = .{
                .leftT = leftT,
                .rightT = right.type,
                .right = right.irIdx,
            }});
            switch (leftRes.resType) {
                .field          => c.irSetStmtCode(irStart, .setField),
                .objectField    => c.irSetStmtCode(irStart, .setObjectField),
                .varSym         => c.irSetStmtCode(irStart, .setVarSym),
                .func           => c.irSetStmtCode(irStart, .setFuncSym),
                .local          => c.irSetStmtCode(irStart, .setLocal),
                .capturedLocal  => c.irSetStmtCode(irStart, .setCaptured),
                else => {
                    log.tracev("leftRes {s} {}", .{@tagName(leftRes.resType), leftRes.type});
                    return c.reportErrorAt("Assignment to the left `{}` is unsupported.", &.{v(left.node_t)}, nodeId);
                }
            }
        },
        else => {
            return c.reportErrorAt("Assignment to the left `{}` is unsupported.", &.{v(left.node_t)}, nodeId);
        }
    }
}

fn checkGetFieldFromObjMod(c: *cy.Chunk, obj: *cy.sym.ObjectType, fieldName: []const u8, nodeId: cy.NodeId) !FieldResult {
    if (obj.head.getMod().?.getSym(fieldName)) |sym| {
        if (sym.type != .field) {
            const objectName = obj.head.name();
            return c.reportErrorAt("`{}` is not a field in `{}`.", &.{v(fieldName), v(objectName)}, nodeId);
        }
        const field = sym.cast(.field);
        return .{
            .idx = @intCast(field.idx),
            .typeId = field.type,
        };
    } else {
        const objectName = obj.head.name();
        return c.reportErrorAt("Field `{}` does not exist in `{}`.", &.{v(fieldName), v(objectName)}, nodeId);
    }
}

fn checkGetField(c: *cy.Chunk, recvT: TypeId, fieldName: []const u8, nodeId: cy.NodeId) !FieldResult {
    const sym = c.sema.getTypeSym(recvT);
    if (sym.type != .object) {
        const name = c.sema.getTypeName(recvT);
        return c.reportErrorAt("Type `{}` does not have a field named `{}`.", &.{v(name), v(fieldName)}, nodeId);
    }
    return checkGetFieldFromObjMod(c, sym.cast(.object), fieldName, nodeId);
}

const FieldResult = struct {
    typeId: TypeId,
    idx: u8,
};

pub fn declareTypeAlias(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const nameN = c.nodes[node.head.typeAliasDecl.name];
    const name = c.getNodeString(nameN);
    try c.declareTypeAlias(@ptrCast(c.sym), name, nodeId);
}

pub fn getOrInitModule(self: *cy.Chunk, spec: []const u8, nodeId: cy.NodeId) !*cy.sym.Chunk {
    self.compiler.hasApiError = false;

    var res: cc.ResolverResult = .{
        .uri = undefined,
        .uriLen = 0,
        .onReceipt = null,
    };
    if (!self.compiler.moduleResolver.?(@ptrCast(self.compiler.vm), self.id, cc.initStr(self.srcUri), cc.initStr(spec), @ptrCast(&res))) {
        if (self.compiler.hasApiError) {
            return self.reportError(self.compiler.apiError, &.{});
        } else {
            return self.reportError("Failed to resolve module.", &.{});
        }
    }

    var uri: []const u8 = undefined;
    if (res.uriLen > 0) {
        uri = res.uri[0..res.uriLen];
    } else {
        uri = std.mem.sliceTo(@as([*:0]const u8, @ptrCast(res.uri)), 0);
    }

    if (self.compiler.chunkMap.get(uri)) |chunk| {
        if (res.onReceipt) |onReceipt| {
            onReceipt(@ptrCast(self.compiler.vm), &res);
        }
        return chunk.sym;
    } else {
        // resUri is duped. Moves to chunk afterwards.
        const dupedResUri = try self.alloc.dupe(u8, uri);
        if (res.onReceipt) |onReceipt| {
            onReceipt(@ptrCast(self.compiler.vm), &res);
        }

        const sym = try self.sema.createChunkSym();

        // Queue import task.
        try self.compiler.importTasks.append(self.alloc, .{
            .fromChunk = self,
            .nodeId = nodeId,
            .absSpec = dupedResUri,
            .sym = sym,
        });
        return sym;
    }
}

pub fn declareImport(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const ident = c.nodes[node.head.left_right.left];
    const name = c.getNodeString(ident);

    var specPath: []const u8 = undefined;
    if (node.head.left_right.right != cy.NullId) {
        const spec = c.nodes[node.head.left_right.right];
        specPath = c.getNodeString(spec);
    } else {
        specPath = name;
    }

    const sym = try getOrInitModule(c, specPath, nodeId);
    _ = try c.declareImport(@ptrCast(c.sym), name, @ptrCast(sym), nodeId);
}

pub fn declareEnum(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const nameN = c.nodes[node.head.enumDecl.name];
    const name = c.getNodeString(nameN);

    const sym = try c.declareEnumType(@ptrCast(c.sym), name, nodeId);
    var i: u32 = 0;
    var cur = node.head.enumDecl.memberHead;

    const members = try c.alloc.alloc(cy.module.ModuleSymId, node.head.enumDecl.numMembers);
    while (cur != cy.NullId) : (i += 1) {
        const member = c.nodes[cur];
        const memberNameN = c.nodes[member.head.enumMember.name];
        const mName = c.getNodeString(memberNameN);
        const modSymId = try c.declareEnumMember(@ptrCast(sym), mName, sym.type, i, cur);
        members[i] = modSymId;
        cur = member.next;
    }
    sym.members = members.ptr;
    sym.numMembers = @intCast(members.len);
}

pub fn declareHostObject(c: *cy.Chunk, nodeId: cy.NodeId) !*cy.sym.HostObjectType {
    c.nodes[nodeId].node_t = .hostObjectDecl;
    const node = c.nodes[nodeId];
    const nameN = c.nodes[node.head.objectDecl.name];
    const name = c.getNodeString(nameN);

    const typeLoader = c.typeLoader orelse {
        return c.reportErrorAt("No object type loader set for `{}`.", &.{v(name)}, nodeId);
    };

    const info = cc.TypeInfo{
        .mod = cc.ApiModule{ .sym = @ptrCast(c.sym) },
        .name = cc.initStr(name),
        .idx = c.curHostTypeIdx,
    };
    c.curHostTypeIdx += 1;
    var res: cc.TypeResult = .{
        .data = .{
            .object = .{
                .outTypeId = null,
                .getChildren = null,
                .finalizer = null,
            },
        },
        .type = cc.TypeKindObject,
    };
    log.tracev("Invoke type loader for: {s}", .{name});
    if (!typeLoader(@ptrCast(c.compiler.vm), info, &res)) {
        return c.reportErrorAt("@host type `{}` object failed to load.", &.{v(name)}, nodeId);
    }

    switch (@as(cc.TypeEnumKind, @enumFromInt(res.type))) {
        .object => {
            const sym = try c.declareHostObjectType(@ptrCast(c.sym), name, nodeId,
                res.data.object.getChildren, res.data.object.finalizer);
            if (res.data.object.outTypeId) |outTypeId| {
                log.tracev("output typeId: {}", .{sym.type});
                outTypeId.* = sym.type;
            }
            return sym;
        },
        .coreObject => {
            const sym = c.compiler.sema.types.items[res.data.coreObject.typeId].sym;
            return @ptrCast(sym);
        },
    }
}

pub fn declareObject(c: *cy.Chunk, nodeId: cy.NodeId) !*cy.Sym{
    const node = c.nodes[nodeId];
    // Check for @host modifier.
    if (node.head.objectDecl.modifierHead != cy.NullId) {
        const modifier = c.nodes[node.head.objectDecl.modifierHead];
        if (modifier.head.annotation.type == .host) {
            return @ptrCast(try declareHostObject(c, nodeId));
        }
    }
    const nameN = c.nodes[node.head.objectDecl.name];
    const name = c.getNodeString(nameN);

    const sym = try c.declareObjectType(@ptrCast(c.sym), name, nodeId);
    return @ptrCast(sym);
}

/// modSym can be an objectType or predefinedType.
pub fn declareObjectMembers(c: *cy.Chunk, modSym: *cy.Sym, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];

    // Load fields.
    var i: u32 = 0;

    const body = c.nodes[node.head.objectDecl.body];

    if (modSym.type == .object) {
        // Only object types can have fields.
        const obj = modSym.cast(.object);

        const fields = try c.alloc.alloc(cy.sym.FieldInfo, body.head.objectDeclBody.numFields);
        var fieldId = body.head.objectDeclBody.fieldsHead;
        while (fieldId != cy.NullId) : (i += 1) {
            const field = c.nodes[fieldId];
            const fieldName = c.getNodeStringById(field.head.objectField.name);
            var fieldType: cy.NodeId = cy.NullId;
            if (field.head.objectField.typed) {
                if (field.head.objectField.typeSpecHead == cy.NullId) {
                    fieldType = bt.Any;
                } else {
                    fieldType = try resolveTypeFromSpecNode(c, field.head.objectField.typeSpecHead);
                }
            } else {
                fieldType = bt.Dynamic;
            }

            const symId = try c.declareField(@ptrCast(obj), fieldName, i, fieldType, fieldId);
            fields[i] = .{
                .symId = symId,
                .type = fieldType,
            };

            fieldId = field.next;
        }
        obj.fields = fields.ptr;
        obj.numFields = @intCast(fields.len);
        c.sema.types.items[obj.type].data = .{
            .numFields = @intCast(obj.numFields),
        };
    }

    var funcId = body.head.objectDeclBody.funcsHead;
    while (funcId != cy.NullId) {
        try declareMethod(c, modSym, funcId);
        const funcN = c.nodes[funcId];
        funcId = funcN.next;
    }
}

pub fn objectDecl(c: *cy.Chunk, obj: *cy.sym.ObjectType, nodeId: cy.NodeId) !void {
    if (c.curObjectSym != null) {
        return c.reportErrorAt("Nested types are not supported.", &.{}, nodeId);
    }
    c.curObjectSym = obj;
    defer c.curObjectSym = null;

    const mod = obj.head.getMod().?;

    // Iterate with index since lambdas can be pushed and reallocate the buffer.
    var i: u32 = 0;
    const numFuncs = mod.funcs.items.len;
    while (i < numFuncs) {
        const func = mod.funcs.items[i];
        const funcN = c.nodes[func.declId];
        if (func.isMethod and func.type == .userFunc) {
            // Object method.
            const blockId = try pushFuncBlock(c, func);
            c.semaBlocks.items[blockId].isMethodBlock = true;

            try pushMethodParamVars(c, obj.type, func);
            try semaStmts(c, funcN.head.func.bodyHead);
            try popStaticFuncBlock(c);
        }
        i += 1;
    }
}

pub fn declareFuncInit(c: *cy.Chunk, parent: *cy.Sym, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    if (node.head.func.bodyHead != cy.NullId) {
        return error.Unexpected;
    }
    // Check if @host func.
    const modifierId = c.nodes[node.head.func.header].next;
    if (modifierId != cy.NullId) {
        const modifier = c.nodes[modifierId];
        if (modifier.head.annotation.type == .host) {
            _ = try declareHostFunc(c, parent, nodeId, false);
            return;
        }
    }
    const name = c.getNodeString(c.nodes[c.nodes[node.head.func.header].head.funcHeader.name]);
    return c.reportErrorAt("`{}` is not a host function.", &.{v(name)}, nodeId);
}

fn declareGenericFunc(c: *cy.Chunk, parent: *cy.Sym, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];

    // Object function.
    if (node.node_t == .funcDecl) {
        try declareFunc(c, parent, nodeId);
    } else if (node.node_t == .funcDeclInit) {
        try declareFuncInit(c, parent, nodeId);
    } else {
        return error.Unexpected;
    }
}

fn declareMethod(c: *cy.Chunk, parent: *cy.Sym, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    if (node.head.func.bodyHead != cy.NullId) {
        const parentT = parent.getStaticType().?;
        const decl = try resolveMethodDecl(c, parentT, nodeId);
        _ = try c.declareUserFunc(parent, decl.name, decl.funcSigId, nodeId, true);
        return;
    }

    // No initializer. Check if @host func.
    const modifierId = c.nodes[node.head.func.header].next;
    if (modifierId != cy.NullId) {
        const modifier = c.nodes[modifierId];
        if (modifier.head.annotation.type == .host) {
            _ = try declareHostFunc(c, parent, nodeId, true);
            return;
        }
    }
    const name = c.getNodeString(c.nodes[c.nodes[node.head.func.header].head.funcHeader.name]);
    return c.reportErrorAt("`{}` does not have an initializer.", &.{v(name)}, nodeId);
}

pub fn declareHostFunc(c: *cy.Chunk, parent: *cy.Sym, nodeId: cy.NodeId, isMethod: bool) !*cy.Func {
    c.nodes[nodeId].node_t = .hostFuncDecl;

    var decl: FuncDecl = undefined;
    if (isMethod) {
        const parentT = parent.getStaticType().?;
        decl = try resolveMethodDecl(c, parentT, nodeId);
    } else {
        decl = try resolveFuncDecl(c, parent, nodeId);
    }

    const info = cc.FuncInfo{
        .mod = cc.ApiModule{ .sym = parent },
        .name = cc.initStr(decl.namePath),
        .funcSigId = decl.funcSigId,
        .idx = c.curHostFuncIdx,
    };
    c.curHostFuncIdx += 1;
    const funcLoader = c.funcLoader orelse {
        return c.reportErrorAt("No function loader set for `{}`.", &.{v(decl.name)}, nodeId);
    };

    log.tracev("Invoke func loader for: {s}", .{decl.name});
    var res: cc.FuncResult = .{
        .ptr = null,
        .type = cc.FuncTypeStandard,
    };
    if (!funcLoader(@ptrCast(c.compiler.vm), info, @ptrCast(&res))) {
        return c.reportErrorAt("Host func `{}` failed to load.", &.{v(decl.name)}, nodeId);
    }

    if (res.type == cc.FuncTypeStandard) {
        return try c.declareHostFunc(decl.parent, decl.name, decl.funcSigId, nodeId, @ptrCast(@alignCast(res.ptr)), isMethod);
    } else if (res.type == cc.FuncTypeInline) {
        // const funcSig = c.compiler.sema.getFuncSig(func.funcSigId);
        // if (funcSig.reqCallTypeCheck) {
        //     return c.reportErrorAt("Failed to load: {}, Only untyped quicken func is supported.", &.{v(name)}, nodeId);
        // }
        return try c.declareHostInlineFunc(decl.parent, decl.name, decl.funcSigId, nodeId, @ptrCast(@alignCast(res.ptr)), isMethod);
    } else {
        return error.Unexpected;
    }
}

/// Declares a bytecode function in a given module.
pub fn declareFunc(c: *cy.Chunk, parent: *Sym, nodeId: cy.NodeId) !*cy.Func {
    const decl = try resolveFuncDecl(c, parent, nodeId);
    return try c.declareUserFunc(decl.parent, decl.name, decl.funcSigId, nodeId, false);
}

pub fn funcDecl(c: *cy.Chunk, func: *cy.Func, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];

    _ = try pushFuncBlock(c, func);
    try appendFuncParamVars(c, func);
    try semaStmts(c, node.head.func.bodyHead);

    try popStaticFuncBlock(c);
}

pub fn declareVar(c: *cy.Chunk, nodeId: cy.NodeId) !*Sym {
    const node = c.nodes[nodeId];

    const varSpec = c.nodes[node.head.staticDecl.varSpec];
    if (node.head.staticDecl.right == cy.NullId) {
        // No initializer. Check if @host var.
        const modifierId = varSpec.head.varSpec.modifierHead;
        if (modifierId != cy.NullId) {
            const modifier = c.nodes[modifierId];
            if (modifier.head.annotation.type == .host) {
                return try declareHostVar(c, nodeId);
            }
        }
        const name = c.ast.getNamePathStr(varSpec.head.varSpec.name);
        return c.reportErrorAt("`{}` does not have an initializer.", &.{v(name)}, nodeId);
    } else {
        // var type.
        const typeId = try resolveTypeFromSpecNode(c, varSpec.head.varSpec.typeSpecHead);

        const decl = try resolveLocalDeclNamePath(c, varSpec.head.varSpec.name);
        return @ptrCast(try c.declareUserVar(decl.parent, decl.name, nodeId, typeId));
    }
}

fn declareHostVar(c: *cy.Chunk, nodeId: cy.NodeId) !*Sym {
    c.nodes[nodeId].node_t = .hostVarDecl;
    const node = c.nodes[nodeId];
    const varSpec = c.nodes[node.head.staticDecl.varSpec];
    const decl = try resolveLocalDeclNamePath(c, varSpec.head.varSpec.name);

    const info = cc.VarInfo{
        .mod = cc.ApiModule{ .sym = @ptrCast(c.sym) },
        .name = cc.initStr(decl.namePath),
        .idx = c.curHostVarIdx,
    };
    c.curHostVarIdx += 1;
    const varLoader = c.varLoader orelse {
        return c.reportErrorAt("No var loader set for `{}`.", &.{v(decl.namePath)}, nodeId);
    };
    log.tracev("Invoke var loader for: {s}", .{decl.namePath});
    var out: cy.Value = cy.Value.None;
    if (!varLoader(@ptrCast(c.compiler.vm), info, @ptrCast(&out))) {
        return c.reportErrorAt("Host var `{}` failed to load.", &.{v(decl.namePath)}, nodeId);
    }
    // var type.
    const typeId = try resolveTypeFromSpecNode(c, varSpec.head.varSpec.typeSpecHead);
    c.nodes[node.head.staticDecl.varSpec].next = typeId;

    const outTypeId = out.getTypeId();
    if (!types.isTypeSymCompat(c.compiler, outTypeId, typeId)) {
        const expTypeName = c.sema.getTypeName(typeId);
        const actTypeName = c.sema.getTypeName(outTypeId);
        return c.reportErrorAt("Host var `{}` expects type {}, got: {}.", &.{v(decl.namePath), v(expTypeName), v(actTypeName)}, nodeId);
    }

    return @ptrCast(try c.declareHostVar(decl.parent, decl.name, nodeId, typeId, out));
}

fn declareParam(c: *cy.Chunk, paramId: cy.NodeId, isSelf: bool, paramIdx: u32, declT: TypeId) !void {
    var name: []const u8 = undefined;
    if (isSelf) {
        name = "self";
    } else {
        const param = c.nodes[paramId];
        name = c.getNodeStringById(param.head.funcParam.name);
    }

    const block = curBlock(c);
    if (!isSelf) {
        if (block.nameToVar.get(name)) |varInfo| {
            if (varInfo.subBlockId == c.semaSubBlocks.items.len - 1) {
                return c.reportErrorAt("Function param `{}` is already declared.", &.{v(name)}, paramId);
            }
        }
    }

    const id = try pushLocalVar(c, .local, name, declT);
    var svar = &c.varStack.items[id];
    svar.inner = .{
        .local = .{
            .id = @intCast(paramIdx),
            .isParam = true,
            .isParamCopied = false,
            .isBoxed = false,
            .declIrStart = cy.NullId,
        },
    };
    block.numParams += 1;

    block.curNumLocals += 1;
}

fn declareLocalName(c: *cy.Chunk, name: []const u8, declType: TypeId, assign: bool, nodeId: cy.NodeId) !LocalVarId {
    const block = curBlock(c);
    if (block.nameToVar.get(name)) |varInfo| {
        if (varInfo.subBlockId == c.semaSubBlocks.items.len - 1) {
            const svar = &c.varStack.items[varInfo.varId];
            if (svar.isParentLocalAlias()) {
                return c.reportErrorAt("`{}` already references a parent local variable.", &.{v(name)}, nodeId);
            } else if (svar.type == .staticAlias) {
                return c.reportErrorAt("`{}` already references a static variable.", &.{v(name)}, nodeId);
            } else {
                return c.reportErrorAt("Variable `{}` is already declared in the block.", &.{v(name)}, nodeId);
            }
        } else {
            // Create shadow entry for restoring the prev var.
            try c.varShadowStack.append(c.alloc, .{
                .namePtr = name.ptr,
                .nameLen = @intCast(name.len),
                .varId = varInfo.varId,
                .subBlockId = varInfo.subBlockId,
            });
        }
    }
    const id = try pushLocalVar(c, .local, name, declType);
    var svar = &c.varStack.items[id];

    const irId = block.curNumLocals;
    const irIdx = try c.irPushStmt(.declareLocal, nodeId, .{
        .declType = declType,
        .id = irId,
        .assign = assign,
        .isBoxed = false,
    });

    svar.inner = .{ .local = .{
        .id = irId,
        .isParam = false,
        .isParamCopied = false,
        .isBoxed = false,
        .declIrStart = @intCast(irIdx),
    }};

    const sblock = curSubBlock(c);
    sblock.numLocals += 1;

    block.curNumLocals += 1;
    return id;
}

fn declareLocal(c: *cy.Chunk, identId: cy.NodeId, declType: TypeId, assign: bool) !LocalVarId {
    const ident = c.nodes[identId];
    const name = c.getNodeString(ident);
    return declareLocalName(c, name, declType, assign, identId);
}

fn localDecl(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const varSpec = c.nodes[node.head.localDecl.varSpec];

    var typeId: cy.TypeId = undefined;
    var inferType = false;
    if (node.head.localDecl.typed) {
        if (varSpec.head.varSpec.typeSpecHead == cy.NullId) {
            inferType = true;
            typeId = bt.Any;
        } else {
            typeId = try resolveTypeFromSpecNode(c, varSpec.head.varSpec.typeSpecHead);
        }
    } else {
        typeId = bt.Dynamic;
    }

    const varId = try declareLocal(c, varSpec.head.varSpec.name, typeId, true);

    // Infer rhs type and enforce constraint.
    const right = try c.semaExprCstr(node.head.localDecl.right, typeId);

    if (inferType) {
        const rightT = right.type.toStaticDeclType();
        c.varStack.items[varId].declT = rightT;
        c.varStack.items[varId].vtype.id = right.type.id;
        // Patch IR.
        const irIdx = c.varStack.items[varId].inner.local.declIrStart;
        var data = c.irGetStmtData(irIdx, .declareLocal);
        data.declType = rightT;
        c.irSetStmtData(irIdx, .declareLocal, data);
    } else if (typeId == bt.Dynamic) {
        // Update recent static type.
        c.varStack.items[varId].vtype.id = right.type.id;
    }

    try c.assignedVarStack.append(c.alloc, varId);
}

pub fn staticDecl(c: *cy.Chunk, sym: *Sym, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];

    c.curInitingSym = sym;
    c.curInitingSymDeps.clearRetainingCapacity();
    defer c.curInitingSym = null;
    const depStart = c.symInitDeps.items.len;

    const irIdx = try c.irPushEmptyStmt(.setVarSym, nodeId);
    _ = try c.irPushExpr(.varSym, nodeId, .{ .sym = sym });
    const symType = CompactType.init(try sym.getValueType());
    const right = try semaExprType(c, node.head.staticDecl.right, symType);
    c.irSetStmtData(irIdx, .setVarSym, .{ .generic = .{
        .leftT = symType,
        .rightT = right.type,
        .right = right.irIdx,
    }});

    try c.symInitInfos.put(c.alloc, sym, .{
        .depStart = @intCast(depStart),
        .depEnd = @intCast(c.symInitDeps.items.len),
        .irStart = @intCast(irIdx),
        .irEnd = @intCast(c.irBuf.items.len),
    });
}

const SemaExprOptions = struct {
    preferType: TypeId = bt.Any,
    hasTypeCstr: bool = false,
};

fn semaExprType(c: *cy.Chunk, nodeId: cy.NodeId, ctype: CompactType) !ExprResult {
    return try c.semaExpr(nodeId, .{
        .preferType = ctype.id,
        .hasTypeCstr = !ctype.dynamic,
    });
}

const ExprResultType = enum(u8) {
    value,
    sym,
    varSym,
    func,
    local,
    field,
    objectField,
    capturedLocal,
};

const ExprResultData = union {
    sym: *Sym,
    varSym: *Sym,
    func: *cy.sym.Func,
    local: LocalVarId,
};

pub const ExprResult = struct {
    resType: ExprResultType,
    type: CompactType,
    data: ExprResultData,
    irIdx: u32,

    fn init(irIdx: u32, ctype: CompactType) ExprResult {
        return .{
            .resType = .value,
            .type = ctype,
            .data = undefined,
            .irIdx = irIdx,
        };
    }

    fn initDynamic(irIdx: u32, typeId: TypeId) ExprResult {
        return .{
            .resType = .value,
            .type = CompactType.initDynamic(typeId),
            .data = undefined,
            .irIdx = irIdx,
        };
    }

    fn initStatic(irIdx: u32, typeId: TypeId) ExprResult {
        return .{
            .resType = .value,
            .type = CompactType.initStatic(typeId),
            .data = undefined,
            .irIdx = irIdx,
        };
    }

    fn initCustom(irIdx: u32, resType: ExprResultType, ctype: CompactType, data: ExprResultData) ExprResult {
        return .{
            .resType = resType,
            .type = ctype,
            .data = data,
            .irIdx = irIdx,
        };
    }
};

pub const Expr = struct {
    hasTypeCstr: bool,
    nodeId: cy.NodeId,
    preferType: TypeId,

    fn init(nodeId: cy.NodeId, preferTypeId: TypeId) Expr {
        return .{
            .hasTypeCstr = false,
            .nodeId = nodeId,
            .preferType = preferTypeId,
        };
    }

    fn initCstr(nodeId: cy.NodeId, typeId: TypeId) Expr {
        return .{
            .hasTypeCstr = true,
            .nodeId = nodeId,
            .preferType = typeId,
        };
    }
};

fn toTypes(ctypes: []CompactType) []const TypeId {
    for (ctypes) |*ctype| {
        if (ctype.*.dynamic) {
            ctype.* = @bitCast(bt.Any);
        }
    }
    return @ptrCast(ctypes);
}

fn callSym(c: *cy.Chunk, preIdx: u32, sym: *Sym, numArgs: u8, symNodeId: cy.NodeId, argHead: cy.NodeId) !ExprResult {
    try referenceSym(c, sym, symNodeId);
    switch (sym.type) {
        .func => {
            const funcSym = sym.cast(.func);
            const funcSigId = getFuncSigHint(funcSym, numArgs);
            const funcSig = c.compiler.sema.getFuncSig(funcSigId);
            const preferParamTypes = funcSig.params();

            const res = try c.semaPushCallArgsInfer(argHead, numArgs, preferParamTypes);
            defer c.typeStack.items.len = res.start;
            return c.semaCallFuncSym(preIdx, symNodeId, funcSym, res);
        },
        .predefinedType => {
            const typeCallSym = try c.findDistinctSym(sym, "$call", cy.NullId, true);
            if (typeCallSym.type != .func) {
                const typeName = c.sema.getTypeName(sym.cast(.predefinedType).type);
                return c.reportErrorAt("Can not find `$call` function for `{}`.", &.{v(typeName)}, symNodeId);
            }
            const funcSym = typeCallSym.cast(.func);
            const funcSigId = getFuncSigHint(funcSym, numArgs);
            const funcSig = c.compiler.sema.getFuncSig(funcSigId);
            const preferParamTypes = funcSig.params();
            const res = try c.semaPushCallArgsInfer(argHead, numArgs, preferParamTypes);
            defer c.typeStack.items.len = res.start;
            return c.semaCallFuncSym(preIdx, symNodeId, funcSym, res);
        }, 
        .userVar,
        .hostVar => {
            // preCall.
            _ = try semaSym(c, sym, symNodeId, true);
            const res = try c.semaPushCallArgs(argHead, numArgs);
            defer c.typeStack.items.len = res.start;
            return c.semaCallValue(preIdx, numArgs, res.irArgsIdx);
        },
        else => {
            // try pushCallArgs(c, node.head.callExpr.arg_head, numArgs, true);
            return error.TODO;
        },
    }
}

fn semaSym(c: *cy.Chunk, sym: *Sym, nodeId: cy.NodeId, comptime symAsValue: bool) !ExprResult {
    try referenceSym(c, sym, nodeId);
    const typeId = try sym.getValueType();
    const ctype = CompactType.init(typeId);
    if (symAsValue) {
        switch (sym.type) {
            .userVar,
            .hostVar => {
                const irIdx = try c.irPushExpr(.varSym, nodeId, .{ .sym = sym });
                return ExprResult.initCustom(irIdx, .varSym, ctype, .{ .varSym = sym });
            },
            .func => {
                // semaSym being invoked suggests the func sym is not ambiguous.
                const funcSym = sym.cast(.func);
                const irIdx = try c.irPushExpr(.funcSym, nodeId, .{ .func = funcSym.first });
                return ExprResult.initCustom(irIdx, .func, ctype, .{ .func = funcSym.first });
            },
            .object => {
                const objTypeId = sym.cast(.object).type;
                const irIdx = try c.irPushExpr(.typeSym, nodeId, .{ .typeId = objTypeId });
                return ExprResult.init(irIdx, ctype);
            },
            .predefinedType => {
                const predefinedType = sym.cast(.predefinedType).type;
                const irIdx = try c.irPushExpr(.typeSym, nodeId, .{ .typeId = predefinedType });
                return ExprResult.init(irIdx, ctype);
            },
            .enumMember => {
                const mem = sym.cast(.enumMember);
                const irIdx = try c.irPushExpr(.enumMemberSym, nodeId, .{
                    .type = mem.type,
                    .val = @as(u8, @intCast(mem.val)),
                });
                return ExprResult.init(irIdx, ctype);
            },
            else => {
                return c.reportErrorAt("Can't use symbol `{}` as a value.", &.{v(sym.name())}, nodeId);
            }
        }
    } else {
        return ExprResult.initCustom(cy.NullId, .sym, ctype, .{ .sym = sym });
    }
}

fn semaLocal(c: *cy.Chunk, id: LocalVarId, nodeId: cy.NodeId) !ExprResult {
    const svar = c.varStack.items[id];
    switch (svar.type) {
        .local => {
            const irIdx = try c.irPushExpr(.local, nodeId, .{ .id = svar.inner.local.id });
            return ExprResult.initCustom(irIdx, .local, svar.vtype, .{ .local = id });
        },
        .objectMemberAlias => {
            const irIdx = try c.irPushExpr(.fieldStatic, nodeId, .{ .static = .{
                .idx = svar.inner.objectMemberAlias.fieldIdx, .typeId = svar.declT,
            }});
            _ = try c.irPushExpr(.local, nodeId, .{ .id = 0 });
            return ExprResult.initCustom(irIdx, .objectField, svar.vtype, undefined);
        },
        .parentObjectMemberAlias => {
            const irIdx = try c.irPushExpr(.fieldStatic, nodeId, .{ .static = .{
                .idx = svar.inner.parentObjectMemberAlias.fieldIdx, .typeId = svar.declT,
            }});
            _ = try c.irPushExpr(.captured, nodeId, .{ .idx = svar.inner.parentObjectMemberAlias.selfCapturedIdx });
            return ExprResult.init(irIdx, svar.vtype);
        },
        .parentLocalAlias => {
            const irIdx = try c.irPushExpr(.captured, nodeId, .{ .idx = svar.inner.parentLocalAlias.capturedIdx });
            return ExprResult.initCustom(irIdx, .capturedLocal, svar.vtype, undefined);
        },
        else => {
            return c.reportError("Unsupported: {}", &.{v(svar.type)});
        },
    }
}

fn semaIdent(c: *cy.Chunk, nodeId: cy.NodeId, comptime symAsValue: bool) !ExprResult {
    const node = c.nodes[nodeId];
    const name = c.getNodeString(node);
    const res = try getOrLookupVar(c, name, true, nodeId);
    switch (res) {
        .local => |id| {
            return semaLocal(c, id, nodeId);
        },
        .static => |csymId| {
            return try semaSym(c, csymId, nodeId, symAsValue);
        },
    }
}

fn resolveLocalRootSym(c: *cy.Chunk, name: []const u8, nodeId: cy.NodeId, distinct: bool) !?*Sym {
    if (c.localSymMap.get(name)) |sym| {
        if (!distinct or sym.isDistinct()) {
            return sym;
        } else {
            return c.reportErrorAt("`{}` is not a unique symbol.", &.{v(name)}, nodeId);
        }
    }

    // Look in the current chunk module.
    if (distinct) {
        if (try c.findDistinctSym(@ptrCast(c.sym), name, nodeId, false)) |res| {
            // Cache to local syms.
            try c.localSymMap.putNoClobber(c.alloc, name, res);
            return res;
        }
    } else {
        if (try c.getOptResolvedSym(@ptrCast(c.sym), name)) |sym| {
            // Cache to local syms.
            try c.localSymMap.putNoClobber(c.alloc, name, sym);
            return sym;
        }
    }

    // Look in using modules.
    for (c.usingModules.items) |using| {
        if (distinct) {
            if (try c.findDistinctSym(using, name, nodeId, false)) |res| {
                // Cache to local syms.
                try c.localSymMap.putNoClobber(c.alloc, name, res);
                return res;
            }
        } else {
            const mod = using.getMod().?;
            if (mod.getSym(name)) |sym| {
                // Cache to local syms.
                try c.localSymMap.putNoClobber(c.alloc, name, sym);
                return sym;
            }
        }
    }
    return null;
}

/// If no type spec, default to `dynamic` type.
/// Recursively walks a type spec head node and returns the final resolved type sym.
pub fn resolveTypeFromSpecNode(c: *cy.Chunk, head: cy.NodeId) !types.TypeId {
    if (head == cy.NullId) {
        return bt.Dynamic;
    }
    const sym = try resolveLocalNamePathSym(c, head, cy.NullId);
    if (sym.getStaticType()) |typeId| {
        return typeId;
    } else {
        return c.reportErrorAt("`{}` is not a type symbol.", &.{v(sym.name())}, head);
    }
}

pub fn resolveLocalNamePathSym(c: *cy.Chunk, head: cy.NodeId, end: cy.NodeId) anyerror!*Sym {
    var nodeId = head;
    var node = c.nodes[nodeId];
    var name = c.getNodeString(node);

    var sym = (try resolveLocalRootSym(c, name, nodeId, true)) orelse {
        return c.reportErrorAt("Could not find the symbol `{}`.", &.{v(name)}, nodeId);
    };

    nodeId = node.next;
    while (nodeId != end) {
        node = c.nodes[nodeId];
        name = c.getNodeString(node);
        sym = try c.findDistinctSym(sym, name, nodeId, true);
        nodeId = node.next;
    }
    return sym;
}

const FuncDecl = struct {
    namePath: []const u8,
    name: []const u8,
    parent: *Sym,
    funcSigId: FuncSigId,
};

fn resolveMethodDecl(c: *cy.Chunk, objectT: TypeId, nodeId: cy.NodeId) !FuncDecl {
    const func = c.nodes[nodeId];
    const header = c.nodes[func.head.func.header];

    // Get params, build func signature.
    const start = c.typeStack.items.len;
    defer c.typeStack.items.len = start;

    // First param is always `self`.
    try c.typeStack.append(c.alloc, objectT);

    var curParamId = header.head.funcHeader.paramHead;
    while (curParamId != cy.NullId) {
        const param = c.nodes[curParamId];
        const typeId = try resolveTypeFromSpecNode(c, param.head.funcParam.typeSpecHead);
        try c.typeStack.append(c.alloc, typeId);
        curParamId = param.next;
    }

    // Get return type.
    const retType = try resolveTypeFromSpecNode(c, header.head.funcHeader.ret);

    var res = FuncDecl{
        .namePath = undefined,
        .name = undefined,
        .funcSigId = try c.sema.ensureFuncSig(c.typeStack.items[start..], retType),
        .parent = undefined,
    };
    const parent = c.sema.types.items[objectT].sym;
    try updateFuncDeclNamePath(c, &res, parent, header.head.funcHeader.name);
    return res;
}

fn resolveFuncDecl(c: *cy.Chunk, parent: *Sym, nodeId: cy.NodeId) !FuncDecl {
    const func = c.nodes[nodeId];
    const header = c.nodes[func.head.func.header];

    // Get params, build func signature.
    const start = c.typeStack.items.len;
    defer c.typeStack.items.len = start;

    var curParamId = header.head.funcHeader.paramHead;
    while (curParamId != cy.NullId) {
        const param = c.nodes[curParamId];
        const typeId = try resolveTypeFromSpecNode(c, param.head.funcParam.typeSpecHead);
        try c.typeStack.append(c.alloc, typeId);
        curParamId = param.next;
    }

    // Get return type.
    const retType = try resolveTypeFromSpecNode(c, header.head.funcHeader.ret);

    var res = FuncDecl{
        .namePath = undefined,
        .name = undefined,
        .funcSigId = try c.sema.ensureFuncSig(c.typeStack.items[start..], retType),
        .parent = undefined,
    };
    try updateFuncDeclNamePath(c, &res, parent, header.head.funcHeader.name);
    return res;
}

const DeclNamePathResult = struct {
    name: []const u8,
    namePath: []const u8,
    parent: *cy.Sym,
};

fn resolveLocalDeclNamePath(c: *cy.Chunk, nameId: cy.NodeId) !DeclNamePathResult {
    const nameN = c.nodes[nameId];
    if (nameN.next == cy.NullId) {
        const name = c.getNodeString(nameN);
        return .{
            .name = name,
            .namePath = name,
            .parent = @ptrCast(c.sym),
        };
    } else {
        const lastId = c.ast.getLastNameNode(nameN.next);
        const last = c.nodes[lastId];
        const startToken = c.tokens[nameN.start_token];
        const lastToken = c.tokens[last.start_token];
        const name = c.src[lastToken.pos()..lastToken.data.end_pos];

        var end = lastToken.data.end_pos;
        if (lastToken.tag() == .string) {
            end += 1;
        }
        const namePath = c.src[startToken.pos()..end];
        const parent = try resolveLocalNamePathSym(c, nameId, lastId);
        return .{
            .name = name,
            .namePath = namePath,
            .parent = parent,
        };
    }
}

fn updateFuncDeclNamePath(c: *cy.Chunk, decl: *FuncDecl, parent: *Sym, nameId: cy.NodeId) !void {
    if (nameId != cy.NullId) {
        const nameN = c.nodes[nameId];
        if (nameN.next == cy.NullId) {
            decl.name = c.getNodeString(nameN);
            decl.namePath = decl.name;
            decl.parent = parent;
        } else {
            const lastId = c.ast.getLastNameNode(nameN.next);
            const last = c.nodes[lastId];
            const startToken = c.tokens[nameN.start_token];
            const lastToken = c.tokens[last.start_token];
            decl.name = c.src[lastToken.pos()..lastToken.data.end_pos];

            var end = lastToken.data.end_pos;
            if (lastToken.tag() == .string) {
                end += 1;
            }
            decl.namePath = c.src[startToken.pos()..end];

            const explicitParent = try resolveLocalNamePathSym(c, nameId, lastId);
            decl.parent = explicitParent;
        }
    } else {
        decl.name = "";
        decl.namePath = "";
        decl.parent = parent;
    }
}

pub fn popBlock(self: *cy.Chunk) !cy.ir.StmtBlock {
    const stmtBlock = try popSubBlock(self);
    const block = curBlock(self);
    if (cy.Trace and block.resetVerboseOnBlockEnd) {
        cy.verbose = false;
    }
    block.deinit(self.alloc);
    self.semaBlocks.items.len -= 1;
    self.varStack.items.len = block.varStart;
    return stmtBlock;
}

pub fn semaMainBlock(compiler: *cy.VMcompiler, mainc: *cy.Chunk) !u32 {
    const irIdx = try mainc.irPushEmptyStmt(.mainBlock, mainc.parserAstRootId);

    const id = try pushBlock(mainc, null);
    mainc.mainSemaBlockId = id;

    // Emit IR for initializers. DFS order. Stop at circular dependency.
    // TODO: Allow circular dependency between chunks but not symbols.
    for (compiler.chunks.items) |c| {
        if (c.hasStaticInit and !c.initializerVisited) {
            try visitChunkInit(compiler, c, );
        }
    }

    // Main.
    const root = mainc.nodes[mainc.parserAstRootId];
    try semaStmts(mainc, root.head.root.headStmt);

    const block = curBlock(mainc);

    // Pop block first to obtain the max locals.
    const stmtBlock = try popBlock(mainc);
    log.tracev("pop main block: {}", .{block.maxLocals});

    mainc.irSetStmtData(irIdx, .mainBlock, .{
        .maxLocals = block.maxLocals,
        .bodyHead = stmtBlock.first,
    });
    return irIdx;
}

fn visitChunkInit(self: *cy.VMcompiler, c: *cy.Chunk) !void {
    c.initializerVisiting = true;
    var iter = c.symInitChunkDeps.keyIterator();

    while (iter.next()) |key| {
        const depChunk = key.*;
        if (depChunk.initializerVisited) {
            continue;
        }
        if (depChunk.initializerVisiting) {
            return c.reportErrorAt("Referencing `{}` created a circular module dependency.", &.{v(c.srcUri)}, cy.NullId);
        }
        try visitChunkInit(self, depChunk);
    }
    // Once all deps are visited, emit IR.
    const mainChunk = self.chunks.items[0];

    const func = c.sym.getMod().getSym("$init").?.cast(.func).first;
    _ = try mainChunk.irPushStmt(.exprStmt, cy.NullId, .{ .returnMain = false });
    _ = try mainChunk.irPushExpr(.preCallFuncSym, cy.NullId, .{ .callFuncSym = .{
        .func = func, .hasDynamicArg = false, .numArgs = 0,
    }});

    c.initializerVisiting = false;
    c.initializerVisited = true;
}

pub fn pushBlock(self: *cy.Chunk, func: ?*cy.Func) !BlockId {
    var isStaticFuncBlock = false;
    var nodeId: cy.NodeId = undefined;
    if (func != null) {
        isStaticFuncBlock = func.?.isStatic();
        nodeId = func.?.declId;
    } else {
        nodeId = self.parserAstRootId;
    }

    var new = Block.init(nodeId, func, @intCast(self.semaSubBlocks.items.len), isStaticFuncBlock, @intCast(self.varStack.items.len));
    const idx = self.semaBlocks.items.len;
    try self.semaBlocks.append(self.alloc, new);
    try pushSubBlock(self, nodeId);
    return @intCast(idx);
}

fn preLoop(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const block = curBlock(c);
    const sblock = curSubBlock(c);

    // Scan for dynamic vars and prepare them for entering loop block.
    const start = c.preLoopVarSaveStack.items.len;
    const vars = c.varStack.items[block.varStart..];
    for (vars, 0..) |*svar, i| {
        if (svar.type == .local) {
            if (svar.isDynamic()) {
                if (svar.vtype.id != bt.Any) {
                    // Dynamic vars enter the loop with a recent type of `any`
                    // since the rest of the loop hasn't been seen.
                    try c.preLoopVarSaveStack.append(c.alloc, .{
                        .vtype = svar.vtype,
                        .varId = @intCast(block.varStart + i),
                    });
                    svar.vtype.id = bt.Any;
                    _ = try c.irPushStmt(.setLocalType, nodeId, .{ .local = svar.inner.local.id, .type = svar.vtype });
                }
            }
        }
    }
    sblock.preLoopVarSaveStart = @intCast(start);
}

fn popSubBlock(c: *cy.Chunk) !cy.ir.StmtBlock {
    return popSubBlockCommon(c);
}

fn popSubBlockCommon(c: *cy.Chunk) !cy.ir.StmtBlock {
    const block = curBlock(c);
    const sblock = curSubBlock(c);

    // Update max locals and unwind.
    if (block.curNumLocals > block.maxLocals) {
        block.maxLocals = block.curNumLocals;
    }
    block.curNumLocals -= sblock.numLocals;

    const curAssignedVars = c.assignedVarStack.items[sblock.assignedVarStart..];
    c.assignedVarStack.items.len = sblock.assignedVarStart;

    if (block.subBlockDepth > 1) {
        const psblock = c.semaSubBlocks.items[c.semaSubBlocks.items.len-2];

        // Merge types to parent sub block.
        for (curAssignedVars) |varId| {
            const svar = &c.varStack.items[varId];
            // log.debug("merging {s}", .{self.getVarName(varId)});
            if (sblock.prevVarTypes.get(varId)) |prevt| {
                // Merge recent static type.
                if (svar.vtype.id != prevt.id) {
                    svar.vtype.id = bt.Any;
                    if (svar.type == .local) {
                        _ = try c.irPushStmt(.setLocalType, sblock.nodeId, .{
                            .local = svar.inner.local.id, .type = svar.vtype,
                        });
                    }

                    // Previous sub block hasn't recorded the var assignment.
                    if (!psblock.prevVarTypes.contains(varId)) {
                        try c.assignedVarStack.append(c.alloc, varId);
                    }
                }
            }
        }
    }
    sblock.prevVarTypes.deinit(c.alloc);

    // Restore `nameToVar` to previous sub-block state.
    if (block.subBlockDepth > 1) {
        // Remove dead vars.
        const varDecls = c.varStack.items[sblock.varStart..];
        for (varDecls) |decl| {
            const name = decl.namePtr[0..decl.nameLen];
            _ = block.nameToVar.remove(name);
        }
        c.varStack.items.len = sblock.varStart;

        // Restore shadowed vars.
        const varShadows = c.varShadowStack.items[sblock.varShadowStart..];
        for (varShadows) |shadow| {
            const name = shadow.namePtr[0..shadow.nameLen];
            try block.nameToVar.putNoClobber(c.alloc, name, .{
                .varId = shadow.varId,
                .subBlockId = shadow.subBlockId,
            });
        }
        c.varShadowStack.items.len = sblock.varShadowStart;
    }

    block.subBlockDepth -= 1;
    c.semaSubBlocks.items.len -= 1;

    return c.irPopStmtBlock();
}

fn popLoopSubBlock(c: *cy.Chunk) !cy.ir.StmtBlock {
    const psblock = curSubBlock(c);
    const stmtBlock = try popSubBlockCommon(c);

    const sblock = curSubBlock(c);
    const varSaves = c.preLoopVarSaveStack.items[sblock.preLoopVarSaveStart..];
    for (varSaves) |save| {
        var svar = &c.varStack.items[save.varId];
        if (svar.dynamicLastMutSubBlockId <= c.semaSubBlocks.items.len - 1) {
            // Unused inside loop block. Restore type.
            svar.vtype = save.vtype;
            _ = try c.irPushStmt(.setLocalType, psblock.nodeId, .{ .local = svar.inner.local.id, .type = svar.vtype });
        }
    }
    c.preLoopVarSaveStack.items.len = sblock.preLoopVarSaveStart;
    return stmtBlock;
}

fn pushSubBlock(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    try pushSubBlockCommon(c, nodeId);
}

fn pushSubBlockCommon(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    try c.irPushStmtBlock();
    curBlock(c).subBlockDepth += 1;
    const new = SubBlock.init(
        nodeId,
        c.assignedVarStack.items.len,
        c.varStack.items.len,
        c.varShadowStack.items.len,
    );
    try c.semaSubBlocks.append(c.alloc, new);
}

fn pushMethodParamVars(c: *cy.Chunk, objectT: TypeId, func: *const cy.Func) !void {
    const curNodeId = c.curNodeId;
    defer c.curNodeId = curNodeId;

    const rFuncSig = c.compiler.sema.funcSigs.items[func.funcSigId];
    const params = rFuncSig.params();

    // Add self receiver param.
    try declareParam(c, cy.NullId, true, 0, objectT);

    if (func.numParams > 1) {
        const funcN = c.nodes[func.declId];
        const header = c.nodes[funcN.head.func.header];
        var curNode = header.head.funcHeader.paramHead;
        for (params[1..], 1..) |paramT, idx| {
            try declareParam(c, curNode, false, @intCast(idx), paramT);

            const param = c.nodes[curNode];
            curNode = param.next;
        }
    }
}

fn appendFuncParamVars(c: *cy.Chunk, func: *const cy.Func) !void {
    if (func.numParams > 0) {
        const rFuncSig = c.compiler.sema.funcSigs.items[func.funcSigId];
        const params = rFuncSig.params();
        const funcN = c.nodes[func.declId];
        const header = c.nodes[funcN.head.func.header];
        var curNode = header.head.funcHeader.paramHead;
        for (params, 0..) |paramT, idx| {
            const param = c.nodes[curNode];
            try declareParam(c, curNode, false, @intCast(idx), paramT);
            curNode = param.next;
        }
    }
}

fn pushLocalVar(c: *cy.Chunk, _type: LocalVarType, name: []const u8, declType: TypeId) !LocalVarId {
    const sblock = curBlock(c);
    const id: u32 = @intCast(c.varStack.items.len);
    _ = try sblock.nameToVar.put(c.alloc, name, .{
        .varId = id,
        .subBlockId = @intCast(c.semaSubBlocks.items.len-1),
    });
    try c.varStack.append(c.alloc, .{
        .declT = declType,
        .type = _type,
        .dynamicLastMutSubBlockId = 0,
        .namePtr = name.ptr,
        .nameLen = @intCast(name.len),
        .vtype = CompactType.init(declType),
    });
    return id;
}

fn getVarPtr(self: *cy.Chunk, name: []const u8) ?*LocalVar {
    if (curBlock(self).nameToVar.get(name)) |varId| {
        return &self.vars.items[varId];
    } else return null;
}

fn pushStaticVarAlias(c: *cy.Chunk, name: []const u8, sym: *Sym) !LocalVarId {
    const id = try pushLocalVar(c, .staticAlias, name, bt.Any);
    c.varStack.items[id].inner = .{ .staticAlias = sym };
    return id;
}

fn pushObjectMemberAlias(c: *cy.Chunk, name: []const u8, idx: u8, typeId: TypeId) !LocalVarId {
    const id = try pushLocalVar(c, .objectMemberAlias, name, typeId);
    c.varStack.items[id].inner = .{ .objectMemberAlias = .{
        .fieldIdx = idx,
    }};
    return id;
}

fn pushCapturedObjectMemberAlias(self: *cy.Chunk, name: []const u8, parentVarId: LocalVarId, idx: u8, vtype: TypeId) !LocalVarId {
    const block = curBlock(self);
    const id = try pushLocalVar(self, .parentObjectMemberAlias, name, vtype);
    const capturedIdx: u8 = @intCast(block.captures.items.len);
    self.varStack.items[id].inner = .{ .parentObjectMemberAlias = .{
        .selfCapturedIdx = capturedIdx,
        .fieldIdx = idx,
    }};

    const pvar = &self.varStack.items[parentVarId];
    pvar.inner.local.isBoxed = true;

    try self.capVarDescs.put(self.alloc, id, .{
        .user = parentVarId,
    });

    try block.captures.append(self.alloc, id);
    return id;
}

fn pushCapturedVar(c: *cy.Chunk, name: []const u8, parentVarId: LocalVarId, vtype: CompactType) !LocalVarId {
    const block = curBlock(c);
    const id = try pushLocalVar(c, .parentLocalAlias, name, vtype.id);
    const capturedIdx: u8 = @intCast(block.captures.items.len);
    c.varStack.items[id].inner = .{
        .parentLocalAlias = .{
            .capturedIdx = capturedIdx,
        },
    };
    c.varStack.items[id].vtype = vtype;

    const pvar = &c.varStack.items[parentVarId];
    pvar.inner.local.isBoxed = true;

    // Patch local IR.
    if (!pvar.inner.local.isParam) {
        const declIrStart = pvar.inner.local.declIrStart;
        var data = c.irGetStmtData(declIrStart, .declareLocal);
        data.isBoxed = true;
        c.irSetStmtData(declIrStart, .declareLocal, data);
    }

    try c.capVarDescs.put(c.alloc, id, .{
        .user = parentVarId,
    });

    try block.captures.append(c.alloc, id);
    return id;
}

fn referenceSym(c: *cy.Chunk, sym: *Sym, nodeId: cy.NodeId) !void {
    if (!c.isInStaticInitializer()) {
        return;
    }

    if (c.curInitingSym == sym) {
        const node = c.nodes[nodeId];
        const name = c.getNodeString(node);
        return c.reportErrorAt("Reference to `{}` creates a circular dependency.", &.{v(name)}, nodeId);
    }

    // Determine the chunk the symbol belongs to.
    // Skip host symbols.
    var chunk: *cy.Chunk = undefined;
    if (sym.type != .userVar) {
        return;
    }
    chunk = sym.parent.?.getMod().?.chunk;

    if (chunk == c) {
        // Internal sym dep.
        if (c.curInitingSymDeps.contains(sym)) {
            return;
        }
        try c.curInitingSymDeps.put(c.alloc, sym, {});
        try c.symInitDeps.append(c.alloc, .{ .sym = sym, .refNodeId = nodeId });
    } else {
        // Module dep.
        // Record this symbol's chunk as a dependency.
        try c.symInitChunkDeps.put(c.alloc, chunk, {});
    }
}

const VarLookupResult = union(enum) {
    static: *Sym,

    /// Local, parent local alias, or parent object member alias.
    local: LocalVarId,
};

/// Static var lookup is skipped for callExpr since there is a chance it can fail on a
/// symbol with overloaded signatures.
fn getOrLookupVar(self: *cy.Chunk, name: []const u8, staticLookup: bool, nodeId: cy.NodeId) !VarLookupResult {
    if (self.semaBlocks.items.len == 1 and self.isInStaticInitializer()) {
        return (try lookupStaticVar(self, name, nodeId, staticLookup)) orelse {
            return self.reportErrorAt("Could not find the symbol `{}`.", &.{v(name)}, nodeId);
        };
    }

    const block = curBlock(self);
    if (block.nameToVar.get(name)) |varInfo| {
        const svar = self.varStack.items[varInfo.varId];
        if (svar.type == .staticAlias) {
            return VarLookupResult{
                .static = svar.inner.staticAlias,
            };
        } else if (svar.type == .objectMemberAlias) {
            return VarLookupResult{
                .local = varInfo.varId,
            };
        } else if (svar.isParentLocalAlias()) {
            // Can not reference local var in a static var decl unless it's in a nested block.
            // eg. var a = 0
            //     var b: a
            if (self.isInStaticInitializer() and self.semaBlockDepth() == 0) {
                return self.reportErrorAt("Can not reference local `{}` in a static initializer.", &.{v(name)}, nodeId);
            }
            return VarLookupResult{
                .local = varInfo.varId,
            };
        } else {
            if (self.isInStaticInitializer() and self.semaBlockDepth() == 0) {
                return self.reportErrorAt("Can not reference local `{}` in a static initializer.", &.{v(name)}, nodeId);
            }
            return VarLookupResult{
                .local = varInfo.varId,
            };
        }
    }

    // Look for object member if inside method.
    if (block.isMethodBlock) {
        if (self.curObjectSym.?.head.getMod().?.getSym(name)) |sym| {
            if (sym.type == .field) {
                const field = sym.cast(.field);
                const id = try pushObjectMemberAlias(self, name, @intCast(field.idx), field.type);
                return VarLookupResult{
                    .local = id,
                };
            }
        }
    }

    if (try lookupParentLocal(self, name)) |res| {
        if (self.isInStaticInitializer()) {
            // Nop. Since static initializers are analyzed separately from the main block, it can capture any parent block local.
        } else if (block.isStaticFuncBlock) {
            // Can not capture local before static function block.
            const funcName = block.func.?.name();
            return self.reportErrorAt("Can not capture the local variable `{}` from static function `{}`.\nOnly lambdas (anonymous functions) can capture local variables.", &.{v(name), v(funcName)}, self.curNodeId);
        }

        // Create a local captured variable.
        const parentVar = self.varStack.items[res.varId];
        if (res.isObjectField) {
            const parentBlock = &self.semaBlocks.items[res.blockIdx];
            const selfId = parentBlock.nameToVar.get("self").?.varId;
            var resVarId = res.varId;
            const selfVar = &self.varStack.items[selfId];
            if (!selfVar.inner.local.isParamCopied) {
                selfVar.inner.local.isParamCopied = true;
            }

            const id = try pushCapturedObjectMemberAlias(self, name, resVarId, res.fieldIdx, res.fieldT);

            return VarLookupResult{
                .local = id,
            };
        } else {
            const resVar = &self.varStack.items[res.varId];
            if (!resVar.inner.local.isParamCopied) {
                resVar.inner.local.isParamCopied = true;
            }
            const id = try pushCapturedVar(self, name, res.varId, parentVar.vtype);
            return VarLookupResult{
                .local = id,
            };
        }
    } else {
        const res = (try lookupStaticVar(self, name, nodeId, staticLookup)) orelse {
            return self.reportErrorAt("Undeclared variable `{}`.", &.{v(name)}, nodeId);
        };
        _ = try pushStaticVarAlias(self, name, res.static);
        return res;
    }
}

fn lookupStaticVar(c: *cy.Chunk, name: []const u8, nodeId: cy.NodeId, staticLookup: bool) !?VarLookupResult {
    if (!staticLookup) {
        return null;
    }
    const res = (try resolveLocalRootSym(c, name, nodeId, false)) orelse {
        return null;
    };
    return VarLookupResult{ .static = res };
}

const LookupParentLocalResult = struct {
    varId: LocalVarId,
    isObjectField: bool,
    fieldIdx: u8,
    fieldT: TypeId,
    blockIdx: u32,
};

fn lookupParentLocal(c: *cy.Chunk, name: []const u8) !?LookupParentLocalResult {
    // Only check one block above.
    if (c.semaBlockDepth() > 1) {
        const prev = c.semaBlocks.items[c.semaBlocks.items.len - 2];
        if (prev.nameToVar.get(name)) |varInfo| {
            const svar = c.varStack.items[varInfo.varId];
            if (svar.isCapturable()) {
                return .{
                    .isObjectField = false,
                    .varId = varInfo.varId,
                    .blockIdx = @intCast(c.semaBlocks.items.len - 2),
                    .fieldIdx = undefined,
                    .fieldT = undefined,
                };
            }
        }

        // Look for object member if inside method.
        if (prev.isMethodBlock) {
            if (c.curObjectSym.?.head.getMod().?.getSym(name)) |sym| {
                if (sym.type == .field) {
                    const field = sym.cast(.field);
                    return .{
                        .varId = prev.nameToVar.get("self").?.varId,
                        .blockIdx = @intCast(c.semaBlocks.items.len - 2),
                        .fieldIdx = @intCast(field.idx),
                        .fieldT = field.type,
                        .isObjectField = true,
                    };
                }
            }
        }
    }
    return null;
}

/// Finds the first function that matches the constrained signature.
fn findCompatFuncForSym(
    c: *cy.Chunk, sym: *Sym, 
    args: []const CompactType, ret: types.TypeId,
) ?*cy.Func {
    switch (sym.type) {
        .func => {
            const func = sym.cast(.func);
            if (cy.types.isTypeFuncSigCompat(c.compiler, args, ret, func.firstFuncSig)) {
                return func.first;
            }
            if (func.numFuncs > 1) {
                var cur = func.first.next;
                while (cur) |entry| {
                    if (cy.types.isTypeFuncSigCompat(c.compiler, args, ret, entry.funcSigId)) {
                        return entry;
                    }
                    cur = entry.next;
                }
            }
            return null;
        },
        else => {
            return null;
        },
    }
}

fn getFuncSigHint(funcSym: *cy.sym.FuncSym, numArgs: u32) FuncSigId {
    _ = numArgs;
    // Just return the first func for now.
    return funcSym.firstFuncSig;
}

fn mustFindCompatFuncForSym(
    c: *cy.Chunk, sym: *cy.sym.FuncSym, 
    args: []const CompactType, ret: types.TypeId, nodeId: cy.NodeId,
) !*cy.Func {
    if (cy.types.isTypeFuncSigCompat(c.compiler, args, ret, sym.firstFuncSig)) {
        return sym.first;
    }
    if (sym.numFuncs > 1) {
        var cur = sym.first.next;
        while (cur) |entry| {
            if (cy.types.isTypeFuncSigCompat(c.compiler, args, ret, entry.funcSigId)) {
                return entry;
            }
            cur = entry.next;
        }
    }
    try reportIncompatibleCallSig(c, sym, args, ret, nodeId);
    unreachable;
}

const FuncCallResult = struct {
    sym: *Sym,
    func: ?*cy.Func,
};

fn resolveSymFuncCall(
    c: *cy.Chunk, sym: *Sym, args: []const CompactType, ret: TypeId,
) !?FuncCallResult {
    log.tracev("resolveSymFuncCall {s}, arg sig: {s}", .{
        sym.name(), try c.sema.formatArgsRet(@ptrCast(args), ret, &cy.tempBuf, true)} );

    // TODO: Use func check cache.

    // Check call signature against sym.
    if (findCompatFuncForSym(c, sym, args, ret)) |func| {
        return .{
            .sym = sym,
            .func = func,
        };
    }

    // Look for $call magic function on sym's module.
    if (sym.getMod()) |_| {
        if (try c.findDistinctSym(sym, "$call", cy.NullId, false)) |childSym| {
            if (findCompatFuncForSym(c, childSym, args, ret)) |func| {
                return .{
                    .sym = childSym,
                    .func = func,
                };
            }
        }
    }

    return null;
}

fn reportIncompatibleCallSig(c: *cy.Chunk, sym: *cy.sym.FuncSym, args: []const CompactType, retType: cy.TypeId, nodeId: cy.NodeId) !void {
    const name = sym.head.name();
    var msg: std.ArrayListUnmanaged(u8) = .{};
    const w = msg.writer(c.alloc);
    const callSigStr = try c.sema.formatArgsRet(args, retType, &cy.tempBuf, false);
    try w.print("Can not find compatible function for call signature: `{s}{s}`.\n", .{name, callSigStr});
    try w.print("Functions named `{s}` in `{s}`:\n", .{name, sym.head.parent.?.name() });

    var funcStr = try c.sema.formatFuncSig(sym.firstFuncSig, &cy.tempBuf);
    try w.print("    func {s}{s}", .{name, funcStr});
    if (sym.numFuncs > 1) {
        var cur: ?*cy.Func = sym.first.next;
        while (cur) |curFunc| {
            try w.writeByte('\n');
            funcStr = try c.sema.formatFuncSig(curFunc.funcSigId, &cy.tempBuf);
            try w.print("    func {s}{s}", .{name, funcStr});
            cur = curFunc.next;
        }
    }
    return c.reportErrorMsgAt(try msg.toOwnedSlice(c.alloc), nodeId);
}

fn reportIncompatibleFuncSig(c: *cy.Chunk, name: []const u8, funcSigId: FuncSigId, searchSym: *Sym) !void {
    const sigStr = try c.sema.getFuncSigTempStr(&c.tempBufU8, funcSigId);
    if (searchSym.getMod().?.getSym(name)) |sym| {
        if (sym.type == .func) {
            const func = sym.cast(.func);
            if (func.numFuncs == 1) {
                const existingSigStr = try c.sema.allocFuncSigStr(func.firstFuncSig);
                defer c.alloc.free(existingSigStr);
                return c.reportError(
                    \\Can not find compatible function signature for `{}{}`.
                    \\Only `func {}{}` exists for the symbol `{}`.
                , &.{v(name), v(sigStr), v(name), v(existingSigStr), v(name)});
            } else {
                return c.reportError(
                    \\Can not find compatible function signature for `{}{}`.
                    \\There are multiple overloaded functions named `{}`.
                , &.{v(name), v(sigStr), v(name)});
            }
        }
    }
    return c.reportError(
        \\Can not find compatible function signature for `{}{}`.
        \\`{}` does not exist.
    , &.{v(name), v(sigStr), v(name)});
}

pub fn curSubBlock(self: *cy.Chunk) *SubBlock {
    return &self.semaSubBlocks.items[self.semaSubBlocks.items.len-1];
}

pub fn curBlock(self: *cy.Chunk) *Block {
    return &self.semaBlocks.items[self.semaBlocks.items.len-1];
}

fn checkTypeCstr(c: *cy.Chunk, ctype: CompactType, cstrTypeId: TypeId, nodeId: cy.NodeId) !void {
    // Dynamic is allowed.
    if (!ctype.dynamic) {
        if (!types.isTypeSymCompat(c.compiler, ctype.id, cstrTypeId)) {
            const cstrName = c.sema.getTypeName(cstrTypeId);
            const typeName = c.sema.getTypeName(ctype.id);
            return c.reportErrorAt("Expected type `{}`, got `{}`.", &.{v(cstrName), v(typeName)}, nodeId);
        }
    }
}

fn pushExprRes(c: *cy.Chunk, res: ExprResult) !void {
    try c.exprResStack.append(c.alloc, res);
}

const SymAccessType = enum {
    sym,
    fieldDynamic,
    fieldStatic,
};

const SymAccessResult = struct {
    type: SymAccessType,
    data: union {
        sym: *Sym,
        fieldDynamic: struct {
            name: []const u8,
        },
        fieldStatic: struct {
            typeId: TypeId,
            idx: u8,
        },
    },
};

fn resolveSymAccess(c: *cy.Chunk, sym: *Sym, rightId: cy.NodeId) !SymAccessResult {
    const right = c.nodes[rightId];
    if (right.node_t != .ident) {
        return error.Unexpected;
    }
    const rightName = c.getNodeString(right);

    if (sym.isVariable()) {
        const leftT = try sym.getValueType();
        if (leftT == bt.Dynamic or leftT == bt.Map) {
            return .{ .type = .fieldDynamic, .data = .{ .fieldDynamic = .{
                .name = rightName,
            }}};
        } else {
            const res = try checkGetField(c, leftT, rightName, rightId);
            return .{ .type = .fieldStatic, .data = .{ .fieldStatic = .{
                .idx = res.idx,
                .typeId = res.typeId,
            }}};
        }
    } else {
        const rightSym = try c.findDistinctSym(sym, rightName, rightId, true);
        try referenceSym(c, rightSym, rightId);
        return .{ .type = .sym, .data = .{ .sym = rightSym }};
    }
}

pub const ChunkExt = struct {

    pub fn semaZeroInit(c: *cy.Chunk, typeId: cy.TypeId, nodeId: cy.NodeId) !ExprResult {
        switch (typeId) {
            bt.Any,
            bt.Dynamic  => return c.semaNone(nodeId),
            bt.Boolean  => return c.semaFalse(nodeId),
            bt.Integer  => return c.semaInt(0, nodeId),
            bt.Float    => return c.semaFloat(0, nodeId),
            bt.List     => return c.semaEmptyList(nodeId),
            bt.Map      => return c.semaEmptyMap(nodeId),
            bt.Array    => return c.semaArray("", nodeId),
            bt.String   => return c.semaString("", nodeId),
            else => {
                const sym = c.sema.getTypeSym(typeId);
                if (sym.type != .object) {
                    return error.Unsupported;
                }

                const obj = sym.cast(.object);
                const irIdx = try c.irPushExpr(.objectInit, nodeId, .{
                    .typeId = obj.type, .numArgs = @as(u8, @intCast(obj.numFields)),
                    .numFieldsToCheck = 0, .fieldsToCheck = 0,
                });
                const irArgsIdx = try c.irPushEmptyArray(u32, obj.numFields);
                for (obj.fields[0..obj.numFields], 0..) |field, i| {
                    const arg = try semaZeroInit(c, field.type, nodeId);
                    c.irSetArrayItem(irArgsIdx, u32, i, arg.irIdx);
                }
                return ExprResult.initStatic(irIdx, typeId);
            },
        }
    }

    pub fn semaSwitch(c: *cy.Chunk, nodeId: cy.NodeId, leftAssign: bool) !ExprResult {
        const node = c.nodes[nodeId];

        const irIdx = try c.irPushExpr(.switchBlock, nodeId, .{ .leftAssign = leftAssign, .numCases = node.head.switchBlock.numCases });
        const irCasesIdx = try c.irPushEmptyArray(u32, node.head.switchBlock.numCases);
        _ = try c.semaExpr(node.head.switchBlock.expr, .{});

        var caseId = node.head.switchBlock.caseHead;
        var i: u32 = 0;
        while (caseId != cy.NullId) {
            const irCaseIdx = try semaSwitchCase(c, caseId, leftAssign);
            c.irSetArrayItem(irCasesIdx, u32, i, irCaseIdx);
            caseId = c.nodes[caseId].next;
            i += 1;
        }

        return ExprResult.initStatic(irIdx, bt.Any);
    }

    fn semaSwitchCase(c: *cy.Chunk, nodeId: cy.NodeId, leftAssign: bool) !u32 {
        const case = c.nodes[nodeId];
        const numConds = case.head.caseBlock.numConds;
        const irIdx = try c.irPushEmptyExpr(.switchCase, nodeId);

        if (numConds > 0) {
            var irCondsIdx = try c.irPushEmptyArray(u32, numConds);
            var condId = case.head.caseBlock.condHead;
            var i: u32 = 0;
            while (condId != cy.NullId) {
                const cond = c.nodes[condId];
                const condRes = try c.semaExpr(condId, .{});
                c.irSetArrayItem(irCondsIdx, u32, i, condRes.irIdx);
                condId = cond.next;
                i += 1;
            }
        }
        var bodyHead: u32 = undefined;
        if (case.head.caseBlock.bodyIsExpr) {
            const body = try c.semaExpr(case.head.caseBlock.bodyHead, .{});
            bodyHead = body.irIdx;
        } else {
            if (leftAssign) {
                if (numConds > 0) {
                    return c.reportErrorAt("Assign switch statement requires a return case: `case {cond} => {expr}`", &.{}, nodeId);
                } else {
                    return c.reportErrorAt("Assign switch statement requires a return case: `else => {expr}`", &.{}, nodeId);
                }
            }

            try pushSubBlock(c, nodeId);
            try semaStmts(c, case.head.caseBlock.bodyHead);
            const stmtBlock = try popSubBlock(c);
            bodyHead = stmtBlock.first;
        }

        c.irSetExprData(irIdx, .switchCase, .{
            .numConds = numConds,
            .bodyIsExpr = case.head.caseBlock.bodyIsExpr,
            .bodyHead = bodyHead,
        });
        return irIdx;
    }

    pub fn semaObjectInit(c: *cy.Chunk, expr: Expr) !ExprResult {
        const node = c.nodes[expr.nodeId];

        const left = try c.semaExprSkipSym(node.head.objectInit.name);
        if (left.resType != .sym) {
            const name = c.getNodeStringById(node.head.objectInit.name);
            return c.reportErrorAt("Object type `{}` does not exist.", &.{v(name)}, node.head.objectInit.name);
        }

        const sym = left.data.sym.resolved();
        if (sym.type != .object) {
            const name = c.getNodeStringById(node.head.objectInit.name);
            return c.reportErrorAt("`{}` is not an object type.", &.{v(name)}, node.head.objectInit.name);
        }

        const obj = sym.cast(.object);

        // Set up a temp buffer to map initializer entries to type fields.
        const fieldsDataStart = c.listDataStack.items.len;
        try c.listDataStack.resize(c.alloc, c.listDataStack.items.len + obj.numFields);
        defer c.listDataStack.items.len = fieldsDataStart;

        // Initially set to NullId so missed mappings are known from a linear scan.
        const fieldNodes = c.listDataStack.items[fieldsDataStart..];
        @memset(fieldNodes, .{ .nodeId = cy.NullId });

        const initializer = c.nodes[node.head.objectInit.initializer];

        var entryId = initializer.head.recordLiteral.argHead;
        while (entryId != cy.NullId) {
            var entry = c.nodes[entryId];

            const fieldN = c.nodes[entry.head.keyValue.left];
            const fieldName = c.getNodeString(fieldN);

            const field = try checkGetFieldFromObjMod(c, obj, fieldName, entry.head.keyValue.left);
            fieldNodes[field.idx] = .{ .nodeId = entry.head.keyValue.right };
            entryId = entry.next;
        }

        const irIdx = try c.irPushEmptyExpr(.objectInit, expr.nodeId);
        const irArgsIdx = try c.irPushEmptyArray(u32, obj.numFields);

        const argStart = c.typeStack.items.len;
        defer c.typeStack.items.len = argStart;

        var i: u32 = 0;
        for (fieldNodes, 0..) |item, fIdx| {
            const fieldT = obj.fields[fIdx].type;
            if (item.nodeId == cy.NullId) {
                // Check that unset fields can be zero initialized.
                try c.checkForZeroInit(fieldT, expr.nodeId);

                const arg = try c.semaZeroInit(fieldT, expr.nodeId);
                c.irSetArrayItem(irArgsIdx, u32, i, arg.irIdx);
                try c.typeStack.append(c.alloc, @bitCast(arg.type));
            } else {
                var preferT = if (fieldT == bt.Dynamic) bt.Any else fieldT;

                const arg = try c.semaExprPrefer(item.nodeId, preferT);
                c.irSetArrayItem(irArgsIdx, u32, i, arg.irIdx);
                try c.typeStack.append(c.alloc, @bitCast(arg.type));
            }
            i += 1;
        }

        const args: []const CompactType = @ptrCast(c.typeStack.items[argStart..]);
        const irExtraIdx = try c.irPushEmptyArray(u8, args.len);

        var numFieldsToCheck: u32 = 0;
        for (args, 0..) |arg, fidx| {
            if (obj.fields[fidx].type != bt.Dynamic and arg.dynamic) {
                c.irSetArrayItem(irExtraIdx, u8, numFieldsToCheck, @as(u8, @intCast(fidx)));
                numFieldsToCheck += 1;
            }
        }
        c.irSetExprData(irIdx, .objectInit, .{
            .typeId = obj.type, .numArgs = @as(u8, @intCast(obj.numFields)),
            .numFieldsToCheck = @as(u8, @intCast(numFieldsToCheck)),
            .fieldsToCheck = irExtraIdx,
        });
        c.irBuf.items.len = irExtraIdx + numFieldsToCheck;

        return ExprResult.initStatic(irIdx, obj.type);
    }

    pub fn semaPushCallArgsInfer(c: *cy.Chunk, argHead: cy.NodeId, numArgs: u8, preferTypes: []const types.TypeId) !StackTypesResult {
        const start = c.typeStack.items.len;
        const irIdx = try c.irPushEmptyArray(u32, numArgs);

        var nodeId = argHead;
        var i: u32 = 0;
        var hasDynamicArg: bool = false;
        while (nodeId != cy.NullId) {
            const arg = c.nodes[nodeId];
            var preferT = bt.Any;
            if (i < preferTypes.len) {
                preferT = preferTypes[i];
            }
            const argRes = try c.semaExprPrefer(nodeId, preferT);
            c.irSetArrayItem(irIdx, u32, i, argRes.irIdx);
            try c.typeStack.append(c.alloc, @bitCast(argRes.type));
            hasDynamicArg = hasDynamicArg or argRes.type.dynamic;
            i += 1;
            nodeId = arg.next;
        }
        return StackTypesResult{
            .hasDynamicArg = hasDynamicArg,
            .types = @ptrCast(c.typeStack.items[start..start+numArgs]),
            .start = @intCast(start),
            .irArgsIdx = irIdx,
        };
    }

    pub fn semaPushCalleeAndArgs(c: *cy.Chunk, callee: ExprResult, argHead: cy.NodeId, numArgs: u8) !StackTypesResult {
        const start = c.typeStack.items.len;
        try c.typeStack.append(c.alloc, @bitCast(callee.type));
        const irIdx = try c.irPushEmptyArray(u32, numArgs);

        var hasDynamicArg: bool = false;
        var nodeId = argHead;
        var i: u32 = 0;
        while (nodeId != cy.NullId) {
            const arg = c.nodes[nodeId];
            const argRes = try c.semaExpr(nodeId, .{});
            c.irSetArrayItem(irIdx, u32, i, argRes.irIdx);
            hasDynamicArg = hasDynamicArg or argRes.type.dynamic;
            try c.typeStack.append(c.alloc, @bitCast(argRes.type));
            i += 1;
            nodeId = arg.next;
        }
        return StackTypesResult{
            .types = @ptrCast(c.typeStack.items[start..start+numArgs+1]),
            .start = @intCast(start),
            .hasDynamicArg = hasDynamicArg,
            .irArgsIdx = irIdx,
        };
    }

    pub fn semaPushCallArgs(c: *cy.Chunk, argHead: cy.NodeId, numArgs: u8) !StackTypesResult {
        const start = c.typeStack.items.len;
        const irIdx = try c.irPushEmptyArray(u32, numArgs);

        var nodeId = argHead;
        var hasDynamicArg: bool = false;
        var i: u32 = 0;
        while (nodeId != cy.NullId) {
            const arg = c.nodes[nodeId];
            const argRes = try c.semaExpr(nodeId, .{});
            c.irSetArrayItem(irIdx, u32, i, argRes.irIdx);
            try c.typeStack.append(c.alloc, @bitCast(argRes.type));
            hasDynamicArg = hasDynamicArg or argRes.type.dynamic;
            i += 1;
            nodeId = arg.next;
        }
        return StackTypesResult{
            .hasDynamicArg = hasDynamicArg,
            .types = @ptrCast(c.typeStack.items[start..start+numArgs]),
            .start = @intCast(start),
            .irArgsIdx = irIdx,
        };
    }

    /// Skips emitting IR for a sym.
    pub fn semaExprSkipSym(c: *cy.Chunk, nodeId: cy.NodeId) !ExprResult {
        const expr = Expr.init(nodeId, bt.Any);
        const node = c.nodes[nodeId];
        if (node.node_t == .ident) {
            return try semaIdent(c, nodeId, false);
        } else if (node.node_t == .accessExpr) {
            const left = c.nodes[node.head.accessExpr.left];
            if (left.node_t == .ident or left.node_t == .accessExpr) {
                const right = c.nodes[node.head.accessExpr.right]; 
                if (right.node_t != .ident) return error.Unexpected;

                // TODO: Check if ident is sym to reduce work.
                return try c.semaAccessExpr(expr, false);
            } else {
                return try c.semaExprNoCheck(expr);
            }
        } else {
            // No chance it's a symbol path.
            return try c.semaExprNoCheck(expr);
        }
    }

    pub fn semaExprPrefer(c: *cy.Chunk, nodeId: cy.NodeId, preferType: TypeId) !ExprResult {
        return try semaExpr(c, nodeId, .{
            .preferType = preferType,
            .hasTypeCstr = false,
        });
    }

    pub fn semaExprCstr(c: *cy.Chunk, nodeId: cy.NodeId, typeId: TypeId) !ExprResult {
        return try semaExpr(c, nodeId, .{
            .preferType = typeId,
            .hasTypeCstr = true,
        });
    }

    pub fn semaExpr(c: *cy.Chunk, nodeId: cy.NodeId, opts: SemaExprOptions) !ExprResult {
        // Set current node for unexpected errors.
        c.curNodeId = nodeId;

        var expr: Expr = undefined;
        if (opts.hasTypeCstr) {
            expr = Expr.initCstr(nodeId, opts.preferType);
        } else {
            expr = Expr.init(nodeId, opts.preferType);
        }
        return c.semaExpr2(expr);
    }

    pub fn semaExpr2(c: *cy.Chunk, expr: Expr) !ExprResult {
        const res = try c.semaExprNoCheck(expr);
        if (expr.hasTypeCstr) {
            try checkTypeCstr(c, res.type, expr.preferType, expr.nodeId);
        }
        return res;
    }

    /// No preferred type when `preferType == bt.Any`.
    pub fn semaExprNoCheck(c: *cy.Chunk, expr: Expr) anyerror!ExprResult {
        if (cy.Trace) {
            const nodeId = expr.nodeId;
            const nodeStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
            log.tracev("expr.{s}: \"{s}\"", .{@tagName(c.nodes[nodeId].node_t), nodeStr});
        }

        const nodeId = expr.nodeId;
        const node = c.nodes[nodeId];
        c.curNodeId = nodeId;
        switch (node.node_t) {
            .none => return c.semaNone(nodeId),
            .errorSymLit => {
                const sym = c.nodes[node.head.errorSymLit.symbol];
                const name = c.getNodeString(sym);
                const irIdx = try c.irPushExpr(.errorv, nodeId, .{ .name = name });
                return ExprResult.initStatic(irIdx, bt.Error);
            },
            .symbolLit => {
                const name = c.getNodeString(node);
                if (c.sema.isEnumType(expr.preferType)) {
                    const sym = c.sema.getTypeSym(expr.preferType).cast(.enumType);
                    if (sym.head.getMod().?.getSym(name)) |res| {
                        if (res.type == .enumMember) {
                            const val = res.cast(.enumMember).val;
                            const irIdx = try c.irPushExpr(.enumMemberSym, nodeId, .{
                                .type = expr.preferType,
                                .val = @as(u8, @intCast(val)),
                            });
                            return ExprResult.initStatic(irIdx, expr.preferType);
                        }
                    }
                }
                const irIdx = try c.irPushExpr(.tagSym, nodeId, .{ .name = name });
                return ExprResult.initStatic(irIdx, bt.Symbol);
            },
            .true_literal => {
                const irIdx = try c.irPushExpr(.truev, nodeId, {});
                return ExprResult.initStatic(irIdx, bt.Boolean);
            },
            .false_literal => return c.semaFalse(nodeId),
            .float => {
                const literal = c.getNodeString(node);
                const val = try std.fmt.parseFloat(f64, literal);
                const irIdx = try c.irPushExpr(.float, nodeId, .{ .val = val });
                return ExprResult.initStatic(irIdx, bt.Float);
            },
            .number => {
                if (expr.preferType == bt.Float) {
                    const literal = c.getNodeString(node);
                    const val = try std.fmt.parseFloat(f64, literal);
                    return c.semaFloat(val, nodeId);
                } else {
                    const literal = c.getNodeString(node);
                    const val = try std.fmt.parseInt(u48, literal, 10);
                    return c.semaInt(val, nodeId);
                }
            },
            .nonDecInt => {
                const literal = c.getNodeString(node);
                var val: u48 = undefined;
                if (literal[1] == 'x') {
                    val = try std.fmt.parseInt(u48, literal[2..], 16);
                } else if (literal[1] == 'o') {
                    val = try std.fmt.parseInt(u48, literal[2..], 8);
                } else if (literal[1] == 'b') {
                    val = try std.fmt.parseInt(u48, literal[2..], 2);
                } else if (literal[1] == 'u') {
                    if (literal[3] == '\\') {
                        if (unescapeAsciiChar(literal[4])) |ch| {
                            val = ch;
                        } else {
                            val = literal[4];
                            if (val > 128) {
                                return c.reportErrorAt("Invalid UTF-8 Rune.", &.{}, nodeId);
                            }
                        }
                        if (literal.len != 6) {
                            return c.reportErrorAt("Invalid UTF-8 Rune.", &.{}, nodeId);
                        }
                    } else {
                        const len = std.unicode.utf8ByteSequenceLength(literal[3]) catch {
                            return c.reportErrorAt("Invalid UTF-8 Rune.", &.{}, nodeId);
                        };
                        if (literal.len != @as(usize, 4) + len) {
                            return c.reportErrorAt("Invalid UTF-8 Rune.", &.{}, nodeId);
                        }
                        val = std.unicode.utf8Decode(literal[3..3+len]) catch {
                            return c.reportErrorAt("Invalid UTF-8 Rune.", &.{}, nodeId);
                        };
                    }
                } else {
                    const char: []const u8 = &[_]u8{literal[1]};
                    return c.reportErrorAt("Unsupported integer notation: {}", &.{v(char)}, nodeId);
                }

                const irIdx = try c.irPushExpr(.int, nodeId, .{ .val = val });
                return ExprResult.initStatic(irIdx, bt.Integer);
            },
            .ident => {
                return try semaIdent(c, nodeId, true);
            },
            .string => return c.semaString(c.getNodeString(node), nodeId),
            .condExpr => {
                const irIdx = try c.irPushEmptyExpr(.condExpr, nodeId);

                _ = try c.semaExpr(node.head.condExpr.cond, .{});
                const body = try c.semaExpr(node.head.condExpr.bodyExpr, .{});
                var elseBody: ExprResult = undefined;
                if (node.head.condExpr.elseExpr != cy.NullId) {
                    elseBody = try c.semaExpr(node.head.condExpr.elseExpr, .{});
                } else {
                    elseBody = try c.semaNone(nodeId);
                }
                c.irSetExprData(irIdx, .condExpr, .{ .body = body.irIdx, .elseBody = elseBody.irIdx });

                const dynamic = body.type.dynamic or elseBody.type.dynamic;
                if (body.type.id == elseBody.type.id) {
                    return ExprResult.init(irIdx, CompactType.init2(body.type.id, dynamic));
                } else {
                    return ExprResult.init(irIdx, CompactType.init2(bt.Any, dynamic));
                }
            },
            .castExpr => {
                const typeId = try resolveTypeFromSpecNode(c, node.head.castExpr.typeSpecHead);
                const irIdx = try c.irPushExpr(.cast, nodeId, .{ .typeId = typeId });
                _ = try c.semaExpr(node.head.castExpr.expr, .{});
                return ExprResult.init(irIdx, CompactType.init(typeId));
            },
            .callExpr => {
                return try c.semaCallExpr(expr);
            },
            .accessExpr => {
                return try c.semaAccessExpr(expr, true);
            },
            .indexExpr => {
                const preIdx = try c.irPushEmptyExpr(.pre, nodeId);

                const left = try c.semaExpr(node.head.indexExpr.left, .{});
                const leftT = left.type.id;
                const preferT = if (leftT == bt.List or leftT == bt.Tuple) bt.Integer else bt.Any;
                const index = try c.semaExprPrefer(node.head.indexExpr.right, preferT);

                if (leftT == bt.List or leftT == bt.Tuple or leftT == bt.Map) {
                    // Specialized.
                    c.irSetExprCode(preIdx, .preBinOp);
                    c.irSetExprData(preIdx, .preBinOp, .{
                        .binOp = .{
                            .leftT = leftT,
                            .op = .index,
                            .right = index.irIdx,
                        },
                    });
                } else {
                    const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any, index.type.id }, bt.Any);
                    c.irSetExprCode(preIdx, .preCallObjSymBinOp);
                    c.irSetExprData(preIdx, .preCallObjSymBinOp, .{ .callObjSymBinOp = .{
                        .op = .index, .funcSigId = funcSigId, .right = index.irIdx,
                    }});
                }
                return ExprResult.initDynamic(preIdx, bt.Any);
            },
            .sliceExpr => {
                const preIdx = try c.irPushEmptyExpr(.pre, nodeId);

                const recv = try c.semaExpr(node.head.sliceExpr.arr, .{});
                const preferT = if (recv.type.id == bt.List) bt.Integer else bt.Any;

                var left: ExprResult = undefined;
                if (node.head.sliceExpr.left != cy.NullId) {
                    left = try c.semaExprPrefer(node.head.sliceExpr.left, preferT);
                } else {
                    left = try c.semaNone(nodeId);
                }
                var right: ExprResult = undefined;
                if (node.head.sliceExpr.right != cy.NullId) {
                    right = try c.semaExprPrefer(node.head.sliceExpr.right, preferT);
                } else {
                    right = try c.semaNone(nodeId);
                }

                if (recv.type.id == bt.List) {
                    // Specialized.
                    c.irSetExprCode(preIdx, .preSlice);
                    c.irSetExprData(preIdx, .preSlice, .{
                        .slice = .{
                            .recvT = recv.type.id,
                            .left = left.irIdx,
                            .right = right.irIdx,
                        },
                    });
                } else {
                    const irExtraIdx = try c.irPushEmptyArray(u32, 2);
                    c.irSetArrayItem(irExtraIdx, u32, 0, left.irIdx);
                    c.irSetArrayItem(irExtraIdx, u32, 1, right.irIdx);
                    const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any, left.type.id, right.type.id }, bt.Any);
                    c.irSetExprCode(preIdx, .preCallObjSym);
                    c.irSetExprData(preIdx, .preCallObjSym, .{ .callObjSym = .{
                        .name = "$slice",
                        .funcSigId = funcSigId,
                        .numArgs = 2,
                        .args = irExtraIdx,
                    }});
                }
                return ExprResult.initDynamic(preIdx, bt.Any);
            },
            .binExpr => {
                const left = node.head.binExpr.left;
                const right = node.head.binExpr.right;
                return try c.semaBinExpr(expr, left, node.head.binExpr.op, right);
            },
            .unary_expr => {
                return try c.semaUnExpr(expr);
            },
            .arrayLiteral => {
                const numArgs = node.head.arrayLiteral.numArgs;
                const irIdx = try c.irPushEmptyExpr(.list, nodeId);
                const irArgsIdx = try c.irPushEmptyArray(u32, numArgs);

                var argId = node.head.arrayLiteral.argHead;
                var i: u32 = 0;
                while (argId != cy.NullId) {
                    var arg = c.nodes[argId];
                    const argRes = try c.semaExpr(argId, .{});
                    c.irSetArrayItem(irArgsIdx, u32, i, argRes.irIdx);
                    argId = arg.next;
                    i += 1;
                }

                c.irSetExprData(irIdx, .list, .{ .numArgs = numArgs });
                return ExprResult.initStatic(irIdx, bt.List);
            },
            .recordLiteral => {
                const numArgs = node.head.recordLiteral.numArgs;
                const irIdx = try c.irPushEmptyExpr(.map, nodeId);
                const irKeysIdx = try c.irPushEmptyArray([]const u8, numArgs);
                const irArgsIdx = try c.irPushEmptyArray(u32, numArgs);

                var i: u32 = 0;
                var argId = node.head.recordLiteral.argHead;
                while (argId != cy.NullId) {
                    const arg = c.nodes[argId];
                    const key = c.nodes[arg.head.keyValue.left];
                    switch (key.node_t) {
                        .ident => {
                            const name = c.getNodeString(key);
                            c.irSetArrayItem(irKeysIdx, []const u8, i, name);
                        },
                        .string => {
                            const name = c.getNodeString(key);
                            c.irSetArrayItem(irKeysIdx, []const u8, i, name);
                        },
                        else => cy.panicFmt("Unsupported key {}", .{key.node_t}),
                    }
                    const argRes = try c.semaExpr(arg.head.keyValue.right, .{});
                    c.irSetArrayItem(irArgsIdx, u32, i, argRes.irIdx);
                    i += 1;
                    argId = arg.next;
                }

                c.irSetExprData(irIdx, .map, .{ .numArgs = numArgs, .args = irArgsIdx });
                return ExprResult.initStatic(irIdx, bt.Map);
            },
            .stringTemplate => {
                const numExprs = node.head.stringTemplate.numExprs;
                const irIdx = try c.irPushEmptyExpr(.stringTemplate, nodeId);
                const irStrsIdx = try c.irPushEmptyArray([]const u8, numExprs+1);
                const irArgsIdx = try c.irPushEmptyArray(u32, numExprs);

                var i: u32 = 0;
                var curId = node.head.stringTemplate.strHead;
                while (curId != cy.NullId) {
                    const str = c.nodes[curId];
                    c.irSetArrayItem(irStrsIdx, []const u8, i, c.getNodeString(str));
                    curId = str.next;
                    i += 1;
                }

                i = 0;
                curId = node.head.stringTemplate.exprHead;
                while (curId != cy.NullId) {
                    var exprN = c.nodes[curId];
                    const argRes = try c.semaExpr(curId, .{});
                    c.irSetArrayItem(irArgsIdx, u32, i, argRes.irIdx);
                    curId = exprN.next;
                    i += 1;
                }

                c.irSetExprData(irIdx, .stringTemplate, .{ .numExprs = numExprs, .args = irArgsIdx });
                return ExprResult.initStatic(irIdx, bt.String);
            },
            .group => {
                return c.semaExpr(node.head.child_head, .{});
            },
            .lambda_expr => {
                const decl = try resolveFuncDecl(c, @ptrCast(c.sym), nodeId);
                const func = try c.addUserLambda(@ptrCast(c.sym), decl.funcSigId, nodeId);
                const irIdx = try pushLambdaBlock(c, func);

                // Generate function body.
                try appendFuncParamVars(c, func);

                _ = try c.irPushStmt(.retExprStmt, nodeId, {});
                _ = try c.semaExpr(node.head.func.bodyHead, .{});

                try popLambdaBlock(c);

                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .lambda_multi => {
                const decl = try resolveFuncDecl(c, @ptrCast(c.sym), nodeId);
                const func = try c.addUserLambda(@ptrCast(c.sym), decl.funcSigId, nodeId);
                const irIdx = try pushLambdaBlock(c, func);

                // Generate function body.
                try appendFuncParamVars(c, func);
                try semaStmts(c, node.head.func.bodyHead);

                try popLambdaBlock(c);

                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .objectInit => {
                return c.semaObjectInit(expr);
            },
            .throwExpr => {
                const irIdx = try c.irPushExpr(.throw, nodeId, {});
                _ = try c.semaExpr(node.head.child_head, .{});
                return ExprResult.initDynamic(irIdx, bt.Any);
            },
            .tryExpr => {
                var catchError = false;
                if (node.head.tryExpr.catchExpr != cy.NullId) {
                    const catchExpr = c.nodes[node.head.tryExpr.catchExpr];
                    if (catchExpr.node_t == .ident) {
                        const name = c.getNodeString(catchExpr);
                        if (std.mem.eql(u8, "error", name)) {
                            catchError = true;
                        }
                    }
                } else {
                    catchError = true;
                }
                const irIdx = try c.irPushEmptyExpr(.tryExpr, nodeId);

                if (catchError) {
                    const child = try c.semaExpr(node.head.tryExpr.expr, .{});
                    c.irSetExprData(irIdx, .tryExpr, .{ .catchBody = cy.NullId });
                    const unionT = types.unionOf(c.compiler, child.type.id, bt.Error);
                    return ExprResult.init(irIdx, CompactType.init2(unionT, child.type.dynamic));
                } else {
                    const child = try c.semaExpr(node.head.tryExpr.expr, .{});
                    const catchExpr = try c.semaExpr(node.head.tryExpr.catchExpr, .{});
                    c.irSetExprData(irIdx, .tryExpr, .{ .catchBody = catchExpr.irIdx });
                    const dynamic = catchExpr.type.dynamic or child.type.dynamic;
                    const unionT = types.unionOf(c.compiler, child.type.id, catchExpr.type.id);
                    return ExprResult.init(irIdx, CompactType.init2(unionT, dynamic));
                }
            },
            .comptimeExpr => {
                const child = c.nodes[node.head.comptimeExpr.child];
                if (child.node_t == .ident) {
                    const name = c.getNodeString(child);
                    if (std.mem.eql(u8, name, "ModUri")) {
                        const irIdx = try c.irPushExpr(.string, nodeId, .{ .literal = c.srcUri });
                        return ExprResult.initStatic(irIdx, bt.String);
                    } else {
                        return c.reportErrorAt("Compile-time symbol does not exist: {}", &.{v(name)}, node.head.comptimeExpr.child);
                    }
                } else {
                    return c.reportErrorAt("Unsupported compile-time expr: {}", &.{v(child.node_t)}, node.head.comptimeExpr.child);
                }
            },
            .coinit => {
                const irIdx = try c.irPushExpr(.coinitCall, nodeId, {});

                const irCallIdx = try c.irPushEmptyExpr(.pre, nodeId);
                const callExpr = c.nodes[node.head.child_head];

                const callee = try c.semaExprSkipSym(callExpr.head.callExpr.callee);

                // Callee is already pushed as a value or is a symbol.
                if (callee.resType == .sym) {
                    const sym = callee.data.sym;
                    _ = try callSym(c, irCallIdx, sym, callExpr.head.callExpr.numArgs,
                        callExpr.head.callExpr.callee, callExpr.head.callExpr.arg_head);
                } else {
                    // preCall.
                    const numArgs = callExpr.head.callExpr.numArgs;
                    const res = try c.semaPushCallArgs(callExpr.head.callExpr.arg_head, numArgs);
                    defer c.typeStack.items.len = res.start;
                    _ = try c.semaCallValue(irCallIdx, numArgs, res.irArgsIdx);
                }

                return ExprResult.initStatic(irIdx, bt.Fiber);
            },
            .coyield => {
                const irIdx = try c.irPushExpr(.coyield, nodeId, {});
                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .coresume => {
                const irIdx = try c.irPushExpr(.coresume, nodeId, {});
                _ = try c.semaExpr(node.head.child_head, .{});
                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .switchBlock => {
                return try c.semaSwitch(nodeId, true);
            },
            else => {
                return c.reportErrorAt("Unsupported node: {}", &.{v(node.node_t)}, nodeId);
            },
        }
    }

    pub fn semaCallExpr(c: *cy.Chunk, expr: Expr) !ExprResult {
        const node = c.nodes[expr.nodeId];
        const callee = c.nodes[node.head.callExpr.callee];
        const numArgs = node.head.callExpr.numArgs;

        if (node.head.callExpr.has_named_arg) {
            return c.reportErrorAt("Unsupported named args.", &.{}, expr.nodeId);
        }

        // pre is later patched with the type of call.
        const preIdx = try c.irPushEmptyExpr(.pre, expr.nodeId);

        if (callee.node_t == .accessExpr) {
            const leftRes = try c.semaExprSkipSym(callee.head.accessExpr.left);
            const rightId = callee.head.accessExpr.right;
            const accessRight = c.nodes[rightId];
            if (accessRight.node_t != .ident) {
                return error.Unexpected;
            }

            if (leftRes.resType == .sym) {
                const leftSym = leftRes.data.sym;
                if (leftSym.type == .func) {
                    return c.reportErrorAt("Can not access function symbol `{}`.", &.{
                        v(c.getNodeStringById(callee.head.accessExpr.left))}, callee.head.accessExpr.right);
                }
                const right = c.nodes[rightId];
                const rightName = c.getNodeString(right);

                if (leftRes.type.dynamic) {
                    // Runtime method call.
                    const calleeRes = try semaSym(c, leftSym, callee.head.accessExpr.left, true);
                    const res = try c.semaPushCalleeAndArgs(calleeRes, node.head.callExpr.arg_head, numArgs);
                    defer c.typeStack.items.len = res.start;
                    return c.semaCallObjSym(preIdx, rightId, res.types, res.irArgsIdx);
                }

                const rightSym = try c.mustFindSym(leftSym, rightName, rightId);
                return try callSym(c, preIdx, rightSym, numArgs, rightId, node.head.callExpr.arg_head);
            } else {
                // preCallObjSym.
                const res = try c.semaPushCalleeAndArgs(leftRes, node.head.callExpr.arg_head, numArgs);
                defer c.typeStack.items.len = res.start;
                return c.semaCallObjSym(preIdx, rightId, res.types, res.irArgsIdx);
            }
        } else if (callee.node_t == .ident) {
            const name = c.getNodeString(callee);

            const varRes = try getOrLookupVar(c, name, true, node.head.callExpr.callee);
            switch (varRes) {
                .local => |_| {
                    // preCall.
                    _ = try c.semaExpr(node.head.callExpr.callee, .{});
                    const res = try c.semaPushCallArgs(node.head.callExpr.arg_head, numArgs);
                    defer c.typeStack.items.len = res.start;
                    return c.semaCallValue(preIdx, numArgs, res.irArgsIdx);
                },
                .static => |sym| {
                    return callSym(c, preIdx, sym, numArgs, node.head.callExpr.callee, node.head.callExpr.arg_head);
                },
            }
        } else {
            // preCall.
            _ = try c.semaExpr(node.head.callExpr.callee, .{});
            const res = try c.semaPushCallArgs(node.head.callExpr.arg_head, numArgs);
            defer c.typeStack.items.len = res.start;
            return c.semaCallValue(preIdx, numArgs, res.irArgsIdx);
        }
    }

    /// Expr or construct binExpr from opAssignStmt.
    pub fn semaExprOrOpAssignBinExpr(c: *cy.Chunk, expr: Expr, opAssignBinExpr: bool) !ExprResult {
        if (!opAssignBinExpr) {
            return try c.semaExpr2(expr);
        } else {
            const opAssign = c.nodes[expr.nodeId];
            const left = opAssign.head.opAssignStmt.left;
            const op = opAssign.head.opAssignStmt.op;
            const right = opAssign.head.opAssignStmt.right;
            return try c.semaBinExpr(expr, left, op, right);
        }
    }

    pub fn semaString(c: *cy.Chunk, str: []const u8, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.irPushExpr(.string, nodeId, .{ .literal = str });
        return ExprResult.initStatic(irIdx, bt.String);
    }

    pub fn semaArray(c: *cy.Chunk, arr: []const u8, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.irPushExpr(.array, nodeId, .{ .buffer = arr });
        return ExprResult.initStatic(irIdx, bt.Array);
    }

    pub fn semaFloat(c: *cy.Chunk, val: f64, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.irPushExpr(.float, nodeId, .{ .val = val });
        return ExprResult.initStatic(irIdx, bt.Float);
    }

    pub fn semaInt(c: *cy.Chunk, val: u48, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.irPushExpr(.int, nodeId, .{ .val = val });
        return ExprResult.initStatic(irIdx, bt.Integer);
    }

    pub fn semaFalse(c: *cy.Chunk, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.irPushExpr(.falsev, nodeId, {});
        return ExprResult.initStatic(irIdx, bt.Boolean);
    }

    pub fn semaNone(c: *cy.Chunk, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.irPushExpr(.none, nodeId, {});
        return ExprResult.initStatic(irIdx, bt.None);
    }

    pub fn semaEmptyList(c: *cy.Chunk, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.irPushExpr(.list, nodeId, .{ .numArgs = 0 });
        return ExprResult.initStatic(irIdx, bt.List);
    }

    pub fn semaEmptyMap(c: *cy.Chunk, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.irPushExpr(.map, nodeId, .{ .numArgs = 0, .args = 0 });
        return ExprResult.initStatic(irIdx, bt.Map);
    }

    pub fn semaCallFuncSym(c: *cy.Chunk, preIdx: u32, calleeId: cy.NodeId, funcSym: *cy.sym.FuncSym, args: StackTypesResult) !ExprResult {
        const reqRet = bt.Any;

        const func = try mustFindCompatFuncForSym(c, funcSym, args.types, reqRet, calleeId);
        try referenceSym(c, @ptrCast(funcSym), calleeId);

        c.irSetExprCode(preIdx, .preCallFuncSym);
        c.irSetExprData(preIdx, .preCallFuncSym, .{ .callFuncSym = .{
            .func = func, .hasDynamicArg = args.hasDynamicArg, .numArgs = @as(u8, @intCast(args.types.len)),
        }});
        return ExprResult.init(preIdx, CompactType.init(func.retType));
    }

    pub fn semaCallValue(c: *cy.Chunk, preIdx: u32, numArgs: u32, irArgsIdx: u32) !ExprResult {
        // Dynamic call.
        c.irSetExprCode(preIdx, .preCall);
        c.irSetExprData(preIdx, .preCall, .{ .call = .{ .numArgs = @as(u8, @intCast(numArgs)), .args = irArgsIdx }});
        return ExprResult.initDynamic(preIdx, bt.Any);
    }

    pub fn semaCallObjSym(c: *cy.Chunk, preIdx: u32, right: cy.NodeId, calleeAndArgs: []CompactType, irArgsIdx: u32) !ExprResult {
        // Dynamic method call.
        const name = c.getNodeStringById(right);

        // Overwrite first arg as any to match prev implementation.
        calleeAndArgs[0] = CompactType.initDynamic(bt.Any);

        const reqRet = bt.Any;
        const sigTypes = toTypes(calleeAndArgs);
        const funcSigId = try c.sema.ensureFuncSig(sigTypes, reqRet);

        c.irSetExprCode(preIdx, .preCallObjSym);
        c.irSetExprData(preIdx, .preCallObjSym, .{ .callObjSym = .{
            .funcSigId = funcSigId, .name = name, .numArgs = @as(u8, @intCast(calleeAndArgs.len-1)),
            .args = irArgsIdx,
        }});
        return ExprResult.initDynamic(preIdx, bt.Any);
    }

    pub fn semaUnExpr(c: *cy.Chunk, expr: Expr) !ExprResult {
        const nodeId = expr.nodeId;
        const node = c.nodes[nodeId];

        const op = node.head.unary.op;
        switch (op) {
            .minus => {
                const preferT = if (expr.preferType == bt.Integer or expr.preferType == bt.Float) expr.preferType else bt.Any;

                const irIdx = try c.irPushEmptyExpr(.pre, nodeId);

                const child = try c.semaExprPrefer(node.head.unary.child, preferT);
                if (child.type.id == bt.Integer or child.type.id == bt.Float) {
                    // Specialized.
                    c.irSetExprCode(irIdx, .preUnOp);
                    c.irSetExprData(irIdx, .preUnOp, .{ .unOp = .{
                        .childT = child.type.id, .op = op,
                    }});
                    return ExprResult.initStatic(irIdx, child.type.id);
                } else {
                    // Generic callObjSym.
                    const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any }, bt.Any);
                    c.irSetExprCode(irIdx, .preCallObjSymUnOp);
                    c.irSetExprData(irIdx, .preCallObjSymUnOp, .{ .callObjSymUnOp = .{
                        .op = op, .funcSigId = funcSigId,
                    }});
                    return ExprResult.initDynamic(irIdx, bt.Any);
                }
            },
            .not => {
                const irIdx = try c.irPushEmptyExpr(.preUnOp, nodeId);

                const child = try c.semaExpr(node.head.unary.child, .{});
                c.irSetExprData(irIdx, .preUnOp, .{ .unOp = .{
                    .childT = child.type.id, .op = op,
                }});
                return ExprResult.initStatic(irIdx, bt.Boolean);
            },
            .bitwiseNot => {
                const preferT = if (expr.preferType == bt.Integer) expr.preferType else bt.Any;

                const irIdx = try c.irPushEmptyExpr(.pre, nodeId);

                const child = try c.semaExprPrefer(node.head.unary.child, preferT);
                if (child.type.id == bt.Integer) {
                    c.irSetExprCode(irIdx, .preUnOp);
                    c.irSetExprData(irIdx, .preUnOp, .{ .unOp = .{
                        .childT = child.type.id, .op = op,
                    }});
                    return ExprResult.initStatic(irIdx, bt.Float);
                } else {
                    const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any }, bt.Any);
                    c.irSetExprCode(irIdx, .preCallObjSymUnOp);
                    c.irSetExprData(irIdx, .preCallObjSymUnOp, .{ .callObjSymUnOp = .{
                        .op = op, .funcSigId = funcSigId,
                    }});
                    return ExprResult.initDynamic(irIdx, bt.Any);
                }
            },
            else => return c.reportErrorAt("Unsupported unary op: {}", &.{v(op)}, nodeId),
        }
    }

    pub fn semaBinExpr(c: *cy.Chunk, expr: Expr, leftId: cy.NodeId, op: cy.BinaryExprOp, rightId: cy.NodeId) !ExprResult {
        const nodeId = expr.nodeId;

        switch (op) {
            .and_op,
            .or_op => {
                const preIdx = try c.irPushEmptyExpr(.preBinOp, nodeId);
                const left = try c.semaExpr(leftId, .{});
                const right = try c.semaExpr(rightId, .{});
                c.irSetExprData(preIdx, .preBinOp, .{ .binOp = .{
                    .leftT = left.type.id,
                    .op = op,
                    .right = right.irIdx,
                }});

                const dynamic = right.type.dynamic or left.type.dynamic;
                const unionT = types.unionOf(c.compiler, left.type.id, right.type.id);                    
                return ExprResult.init(preIdx, CompactType.init2(unionT, dynamic));
            },
            .bitwiseAnd,
            .bitwiseOr,
            .bitwiseXor,
            .bitwiseLeftShift,
            .bitwiseRightShift => {
                const preIdx = try c.irPushEmptyExpr(.pre, nodeId);

                const leftPreferT = if (expr.preferType == bt.Integer) expr.preferType else bt.Any;
                const left = try c.semaExprPrefer(leftId, leftPreferT);
                const rightPreferT = if (left.type.id == bt.Integer) left.type.id else bt.Any;
                const right = try c.semaExprPrefer(rightId, rightPreferT);

                if (left.type.id == bt.Integer) {
                    // Specialized.
                    c.irSetExprCode(preIdx, .preBinOp);
                    c.irSetExprData(preIdx, .preBinOp, .{ .binOp = .{
                        .leftT = bt.Integer,
                        .op = op,
                        .right = right.irIdx,
                    }});
                    return ExprResult.initStatic(preIdx, bt.Integer);
                } else {
                    // Generic callObjSym.
                    const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any, right.type.id }, bt.Any);
                    c.irSetExprCode(preIdx, .preCallObjSymBinOp);
                    c.irSetExprData(preIdx, .preCallObjSymBinOp, .{ .callObjSymBinOp = .{
                        .op = op, .funcSigId = funcSigId, .right = right.irIdx,
                    }});

                    // TODO: Check func syms.
                    return ExprResult.initDynamic(preIdx, bt.Any);
                }
            },
            .greater,
            .greater_equal,
            .less,
            .less_equal => {
                const leftPreferT = if (expr.preferType == bt.Float or expr.preferType == bt.Integer) expr.preferType else bt.Any;

                const preIdx = try c.irPushEmptyExpr(.pre, nodeId);

                const left = try c.semaExprPrefer(leftId, leftPreferT);
                const rightPreferT = if (left.type.id == bt.Float or left.type.id == bt.Integer) left.type.id else bt.Any;
                const right = try c.semaExprPrefer(rightId, rightPreferT);

                if (left.type.id == bt.Integer or left.type.id == bt.Float) {
                    // Specialized.
                    c.irSetExprCode(preIdx, .preBinOp);
                    c.irSetExprData(preIdx, .preBinOp, .{ .binOp = .{
                        .leftT = left.type.id,
                        .op = op,
                        .right = right.irIdx,
                    }});
                    return ExprResult.initStatic(preIdx, bt.Boolean);
                } else {
                    // Generic callObjSym.
                    const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any, right.type.id }, bt.Any);
                    c.irSetExprCode(preIdx, .preCallObjSymBinOp);
                    c.irSetExprData(preIdx, .preCallObjSymBinOp, .{ .callObjSymBinOp = .{
                        .op = op, .funcSigId = funcSigId, .right = right.irIdx,
                    }});

                    // TODO: Check func syms.
                    return ExprResult.initDynamic(preIdx, bt.Any);
                }
            },
            .star,
            .slash,
            .percent,
            .caret,
            .plus,
            .minus => {
                const leftPreferT = if (expr.preferType == bt.Float or expr.preferType == bt.Integer) expr.preferType else bt.Any;

                const preIdx = try c.irPushEmptyExpr(.pre, nodeId);

                const left = try c.semaExprPrefer(leftId, leftPreferT);
                const rightPreferT = if (left.type.id == bt.Float or left.type.id == bt.Integer) left.type.id else bt.Any;
                const right = try c.semaExprPrefer(rightId, rightPreferT);
                if (left.type.id == bt.Integer or left.type.id == bt.Float) {
                    // Specialized.
                    c.irSetExprCode(preIdx, .preBinOp);
                    c.irSetExprData(preIdx, .preBinOp, .{ .binOp = .{
                        .leftT = left.type.id,
                        .op = op,
                        .right = right.irIdx,
                    }});
                    return ExprResult.initStatic(preIdx, left.type.id);
                } else {
                    // Generic callObjSym.
                    const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any, right.type.id }, bt.Any);
                    c.irSetExprCode(preIdx, .preCallObjSymBinOp);
                    c.irSetExprData(preIdx, .preCallObjSymBinOp, .{ .callObjSymBinOp = .{
                        .op = op, .funcSigId = funcSigId, .right = right.irIdx,
                    }});

                    // TODO: Check func syms.
                    return ExprResult.initDynamic(preIdx, bt.Any);
                }
            },
            .bang_equal,
            .equal_equal => {
                const leftPreferT = if (expr.preferType == bt.Float or expr.preferType == bt.Integer) expr.preferType else bt.Any;

                const preIdx = try c.irPushEmptyExpr(.pre, nodeId);

                const left = try c.semaExprPrefer(leftId, leftPreferT);
                const rightPreferT = if (left.type.id == bt.Float or left.type.id == bt.Integer) left.type.id else bt.Any;
                const right = try c.semaExprPrefer(rightId, rightPreferT);

                c.irSetExprCode(preIdx, .preBinOp);
                c.irSetExprData(preIdx, .preBinOp, .{
                    .binOp = .{
                        .leftT = left.type.id,
                        .op = op,
                        .right = right.irIdx,
                    },
                });
                return ExprResult.initStatic(preIdx, bt.Boolean);
            },
            else => return c.reportErrorAt("Unsupported binary op: {}", &.{v(op)}, nodeId),
        }
    }

    pub fn semaAccessExpr(c: *cy.Chunk, expr: Expr, symAsValue: bool) !ExprResult {
        const node = c.nodes[expr.nodeId];
        const right = c.nodes[node.head.accessExpr.right];

        if (right.node_t != .ident) {
            return error.Unexpected;
        }

        var left = c.nodes[node.head.accessExpr.left];
        if (left.node_t == .ident) {
            const name = c.getNodeString(left);
            const vres = try getOrLookupVar(c, name, true, node.head.accessExpr.left);

            switch (vres) {
                .local => |id| {
                    // Local: [local].[right]
                    const irStart = try c.irPushEmptyExpr(.field, node.head.accessExpr.right);
                    const leftRes = try semaLocal(c, id, node.head.accessExpr.left);
                    return c.semaAccessExprLeftValue(irStart, expr, leftRes.type);
                },
                .static => |crLeftSym| {
                    // Static symbol: [sym].[right]
                    try referenceSym(c, crLeftSym, node.head.accessExpr.left);

                    const res = try resolveSymAccess(c, crLeftSym, node.head.accessExpr.right);
                    switch (res.type) {
                        .sym => {
                            if (!symAsValue) {
                                const sym = res.data.sym;
                                const typeId = CompactType.init(try sym.getValueType());
                                return ExprResult.initCustom(cy.NullId, .sym, typeId, .{ .sym = res.data.sym });
                            } else {
                                return try semaSym(c, res.data.sym, node.head.accessExpr.right, true);
                            }
                        },
                        .fieldDynamic => {
                            const irIdx = try c.irPushExpr(.fieldDynamic, node.head.accessExpr.right, .{ .dynamic = .{
                                .name = res.data.fieldDynamic.name,
                            }});
                            _ = try semaSym(c, crLeftSym, node.head.accessExpr.left, true);
                            return ExprResult.initCustom(irIdx, .field, CompactType.initDynamic(bt.Any), undefined);
                        },
                        .fieldStatic => {
                            const irIdx = try c.irPushExpr(.fieldStatic, node.head.accessExpr.right, .{ .static = .{
                                .idx = res.data.fieldStatic.idx,
                                .typeId = res.data.fieldStatic.typeId,
                            }});
                            _ = try semaSym(c, crLeftSym, node.head.accessExpr.left, true);
                            return ExprResult.initCustom(irIdx, .objectField, CompactType.init(res.data.fieldStatic.typeId), undefined);
                        },
                    }
                },
            }
    //     } else if (left.node_t == .accessExpr) {
    //         const res = try accessExpr(self, node.head.accessExpr.left);

    //         left = self.nodes[node.head.accessExpr.left];
    //         if (left.head.accessExpr.sema_csymId.isPresent()) {
    //             // Static var.
    //             const crLeftSym = left.head.accessExpr.sema_csymId;
    //             if (!crLeftSym.isFuncSymId) {
    //                 const rightName = self.getNodeString(right);
    //                 const rightNameId = try ensureNameSym(self.compiler, rightName);
    //                 if (try getOrResolveDistinctSym(self, crLeftSym.id, rightNameId)) |rightSym| {
    //                     const crRightSym = rightSym.toCompactId();
    //                     try referenceSym(self, crRightSym, true);
    //                     self.nodes[nodeId].head.accessExpr.sema_csymId = crRightSym;
    //                 }
    //             }
    //         } else {
    //             const rightName = self.getNodeString(right);
    //             return getAccessExprResult(self, res.exprT, rightName);
    //         }
    //     } else {
        } else {
            const irIdx = try c.irPushEmptyExpr(.field, node.head.accessExpr.right);
            const leftRes = try c.semaExpr(node.head.accessExpr.left, .{});
            return c.semaAccessExprLeftValue(irIdx, expr, leftRes.type);
        }
    }

    pub fn semaAccessExprLeftValue(c: *cy.Chunk, irIdx: u32, expr: Expr, left: CompactType) !ExprResult {
        const node = c.nodes[expr.nodeId];
        const right = c.nodes[node.head.accessExpr.right];

        if ((left.dynamic and left.id == bt.Any) or left.id == bt.Map) {
            // Dynamic field access.
            const name = c.getNodeString(right);
            c.irSetExprCode(irIdx, .fieldDynamic);
            c.irSetExprData(irIdx, .fieldDynamic, .{ .dynamic = .{
                .name = name,
            }});

            return ExprResult.initCustom(irIdx, .field, CompactType.initDynamic(bt.Any), undefined);
        }

        const fieldName = c.getNodeString(right);
        const field = try checkGetField(c, left.id, fieldName, node.head.accessExpr.right);
        c.irSetExprCode(irIdx, .fieldStatic);
        c.irSetExprData(irIdx, .fieldStatic, .{ .static = .{
            .idx = field.idx, .typeId = field.typeId,
        }});

        return ExprResult.initCustom(irIdx, .objectField, CompactType.init(field.typeId), undefined);
    }
};

const VarResult = struct {
    id: LocalVarId,
    fromParentBlock: bool,
};

fn assignToLocalVar(c: *cy.Chunk, localRes: ExprResult, rhs: cy.NodeId, opts: AssignOptions) !ExprResult {
    const id = localRes.data.local;
    var svar = &c.varStack.items[id];

    const rightExpr = Expr{ .preferType = svar.vtype.id, .nodeId = rhs, .hasTypeCstr = !svar.vtype.dynamic };
    const right = try c.semaExprOrOpAssignBinExpr(rightExpr, opts.rhsOpAssignBinExpr);
    // Refresh pointer after rhs.
    svar = &c.varStack.items[id];

    if (svar.inner.local.isParam) {
        if (!svar.inner.local.isParamCopied) {
            svar.inner.local.isParamCopied = true;
        }
    }

    const sblock = curSubBlock(c);
    if (!sblock.prevVarTypes.contains(id)) {
        // Same variable but branched to sub block.
        try sblock.prevVarTypes.put(c.alloc, id, svar.vtype);
    }

    if (svar.isDynamic()) {
        svar.dynamicLastMutSubBlockId = @intCast(c.semaSubBlocks.items.len-1);

        // Update recent static type after checking for branched assignment.
        if (svar.vtype.id != right.type.id) {
            svar.vtype.id = right.type.id;
        }
    }

    try c.assignedVarStack.append(c.alloc, id);
    return right;
}

pub fn declareUsingModule(c: *cy.Chunk, sym: *cy.Sym) !void {
    try c.usingModules.append(c.alloc, sym);
}

pub fn pushLambdaBlock(c: *cy.Chunk, func: *cy.Func) !u32 {
    const idx = try c.irPushEmptyExpr(.lambda, func.declId);
    // Reserve param info.
    _ = try c.irPushEmptyArray(ir.FuncParam, func.numParams);

    _ = try pushBlock(c, func);
    curBlock(c).irStart = idx;
    return idx;
}

fn popLambdaBlock(c: *cy.Chunk) !void {
    const block = curBlock(c);
    const params = getBlockParams(c, block);

    const captures = try block.captures.toOwnedSlice(c.alloc);
    const stmtBlock = try popBlock(c);

    var numParamCopies: u8 = 0;
    const paramIrStart = c.irAdvanceExpr(block.irStart, .lambda);
    const paramData = c.irGetArray(paramIrStart, ir.FuncParam, params.len);
    for (params, 0..) |param, i| {
        paramData[i] = .{
            .declType = param.declT,
            .isCopy = param.inner.local.isParamCopied,
            .isBoxed = param.inner.local.isBoxed,
        };
        if (param.inner.local.isParamCopied) {
            numParamCopies += 1;
        }
    }

    var irCapturesIdx: u32 = cy.NullId;
    if (captures.len > 0) {
        irCapturesIdx = try c.irPushEmptyArray(u8, captures.len);
        for (captures, 0..) |varId, i| {
            const pId = c.capVarDescs.get(varId).?.user;
            const pvar = c.varStack.items[pId];
            c.irSetArrayItem(irCapturesIdx, u8, i, pvar.inner.local.id);
        }
        c.alloc.free(captures);
    }

    // Patch `pushFuncBlock` with maxLocals and param copies.
    c.irSetExprData(block.irStart, .lambda, .{
        .func = block.func.?,
        .numCaptures = @as(u8, @intCast(captures.len)),
        .maxLocals = block.maxLocals,
        .numParamCopies = numParamCopies,
        .bodyHead = stmtBlock.first,
        .captures = irCapturesIdx,
    });
}

pub fn pushFuncBlock(c: *cy.Chunk, func: *cy.Func) !BlockId {
    const idx = try c.irPushEmptyStmt(.funcDecl, func.declId);
    // Reserve param info.
    _ = try c.irPushEmptyArray(ir.FuncParam, func.numParams);

    const id = try pushBlock(c, func);
    curBlock(c).irStart = idx;
    return id;
}

pub fn popStaticFuncBlock(c: *cy.Chunk) !void {
    const block = curBlock(c);
    const params = getBlockParams(c, block);

    const stmtBlock = try popBlock(c);

    var numParamCopies: u8 = 0;
    const paramIrStart = c.irAdvanceStmt(block.irStart, .funcDecl);
    const paramData = c.irGetArray(paramIrStart, ir.FuncParam, params.len);
    for (params, 0..) |param, i| {
        paramData[i] = .{
            .declType = param.declT,
            .isCopy = param.inner.local.isParamCopied,
            .isBoxed = param.inner.local.isBoxed,
        };
        if (param.inner.local.isParamCopied) {
            numParamCopies += 1;
        }
    }

    const func = block.func.?;
    const parentType = if (func.isMethod) params[0].declT else cy.NullId;

    // Patch `pushFuncBlock` with maxLocals and param copies.
    c.irSetStmtData(block.irStart, .funcDecl, .{
        .maxLocals = block.maxLocals,
        .numParamCopies = numParamCopies,
        .func = block.func.?,
        .bodyHead = stmtBlock.first,
        .parentType = parentType,
    });
}

const PushCallArgsResult = struct {
    argTypes: []const TypeId,
    hasDynamicArg: bool,
};

const StackTypesResult = struct {
    types: []CompactType,
    start: u32,
    hasDynamicArg: bool,
    irArgsIdx: u32,
};

pub const FuncSigId = u32;
pub const FuncSig = struct {
    /// Last elem is the return type sym.
    paramPtr: [*]const types.TypeId,
    ret: types.TypeId,
    paramLen: u16,

    /// If a param or the return type is not the any type.
    // isTyped: bool,

    /// If a param is not the any type.
    // isParamsTyped: bool,

    /// Requires type checking if any param is not `dynamic` or `any`.
    reqCallTypeCheck: bool,

    pub inline fn params(self: FuncSig) []const types.TypeId {
        return self.paramPtr[0..self.paramLen];
    }

    pub inline fn numParams(self: FuncSig) u8 {
        return @intCast(self.paramLen);
    }

    pub inline fn getRetType(self: FuncSig) types.TypeId {
        return self.ret;
    }

    pub fn deinit(self: *FuncSig, alloc: std.mem.Allocator) void {
        alloc.free(self.params());
    }
};

const FuncSigKey = struct {
    paramPtr: [*]const TypeId,
    paramLen: u32,
    ret: TypeId,
};

pub const Sema = struct {
    alloc: std.mem.Allocator,
    compiler: *cy.VMcompiler,

    types: std.ArrayListUnmanaged(types.Type),

    /// Resolved signatures for functions.
    funcSigs: std.ArrayListUnmanaged(FuncSig),
    funcSigMap: std.HashMapUnmanaged(FuncSigKey, FuncSigId, FuncSigKeyContext, 80),

    pub fn init(alloc: std.mem.Allocator, compiler: *cy.VMcompiler) Sema {
        return .{
            .alloc = alloc,
            .compiler = compiler,
            .funcSigs = .{},
            .funcSigMap = .{},
            .types = .{},
        };
    }

    pub fn deinit(self: *Sema, alloc: std.mem.Allocator, comptime reset: bool) void {
        for (self.funcSigs.items) |*it| {
            it.deinit(alloc);
        }
        if (reset) {
            self.types.clearRetainingCapacity();
            self.funcSigs.clearRetainingCapacity();
            self.funcSigMap.clearRetainingCapacity();
        } else {
            self.types.deinit(alloc);
            self.funcSigs.deinit(alloc);
            self.funcSigMap.deinit(alloc);
        }
    }

    pub fn ensureUntypedFuncSig(s: *Sema, numParams: u32) !FuncSigId {
        const buf = std.mem.bytesAsSlice(cy.TypeId, &cy.tempBuf);
        if (buf.len < numParams) return error.TooBig;
        @memset(buf[0..numParams], bt.Dynamic);
        return try s.ensureFuncSig(buf[0..numParams], bt.Dynamic);
    }

    pub fn ensureFuncSig(s: *Sema, params: []const TypeId, ret: TypeId) !FuncSigId {
        const res = try s.funcSigMap.getOrPut(s.alloc, .{
            .paramPtr = params.ptr,
            .paramLen = @intCast(params.len),
            .ret = ret,
        });
        if (res.found_existing) {
            return res.value_ptr.*;
        } else {
            const id: u32 = @intCast(s.funcSigs.items.len);
            const new = try s.alloc.dupe(TypeId, params);
            var reqCallTypeCheck = false;
            for (params) |symId| {
                if (symId != bt.Dynamic and symId != bt.Any) {
                    reqCallTypeCheck = true;
                    break;
                }
            }
            try s.funcSigs.append(s.alloc, .{
                .paramPtr = new.ptr,
                .paramLen = @intCast(new.len),
                .ret = ret,
                .reqCallTypeCheck = reqCallTypeCheck,
            });
            res.value_ptr.* = id;
            res.key_ptr.* = .{
                .paramPtr = new.ptr,
                .paramLen = @intCast(new.len),
                .ret = ret,
            };
            return id;
        }
    }

    pub fn formatFuncSig(s: *Sema, funcSigId: FuncSigId, buf: []u8) ![]const u8 {
        var fbuf = std.io.fixedBufferStream(buf);
        try s.writeFuncSigStr(fbuf.writer(), funcSigId);
        return fbuf.getWritten();
    }

    fn writeCompactType(s: *Sema, w: anytype, ctype: CompactType, comptime showRecentType: bool) !void {
        if (showRecentType) {
            if (ctype.dynamic) {
                try w.writeAll("dyn ");
            }
            var name = s.getTypeName(ctype.id);
            try w.writeAll(name);
        } else {
            if (ctype.dynamic) {
                try w.writeAll("dynamic");
            } else {
                var name = s.getTypeName(ctype.id);
                try w.writeAll(name);
            }
        }
    }

    pub fn formatArgsRet(s: *Sema, args: []const CompactType, ret: TypeId, buf: []u8, comptime showRecentType: bool) ![]const u8 {
        var fbuf = std.io.fixedBufferStream(buf);
        const w = fbuf.writer();
        try w.writeAll("(");

        if (args.len > 0) {
            try s.writeCompactType(w, args[0], showRecentType);

            if (args.len > 1) {
                for (args[1..]) |arg| {
                    try w.writeAll(", ");
                    try s.writeCompactType(w, arg, showRecentType);
                }
            }
        }
        try w.writeAll(") ");

        var name = s.getTypeName(ret);
        try w.writeAll(name);

        return fbuf.getWritten();
    }

    /// Format: (Type, ...) RetType
    pub fn getFuncSigTempStr(s: *Sema, buf: *std.ArrayListUnmanaged(u8), funcSigId: FuncSigId) ![]const u8 {
        buf.clearRetainingCapacity();
        const w = buf.writer(s.alloc);
        try writeFuncSigStr(s, w, funcSigId);
        return buf.items;
    }

    pub fn allocFuncSigTypesStr(s: *Sema, params: []const TypeId, ret: TypeId) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        try writeFuncSigTypesStr(s, w, params, ret);
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn allocFuncSigStr(s: *Sema, funcSigId: FuncSigId) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        try writeFuncSigStr(s, w, funcSigId);
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn writeFuncSigStr(s: *Sema, w: anytype, funcSigId: FuncSigId) !void {
        const funcSig = s.funcSigs.items[funcSigId];
        try writeFuncSigTypesStr(s, w, funcSig.params(), funcSig.ret);
    }

    pub fn writeFuncSigTypesStr(m: *Sema, w: anytype, params: []const TypeId, ret: TypeId) !void {
        try w.writeAll("(");

        if (params.len > 0) {
            var name = m.getTypeName(params[0]);
            try w.writeAll(name);

            if (params.len > 1) {
                for (params[1..]) |paramT| {
                    try w.writeAll(", ");
                    name = m.getTypeName(paramT);
                    try w.writeAll(name);
                }
            }
        }
        try w.writeAll(") ");

        var name = m.getTypeName(ret);
        try w.writeAll(name);
    }

    pub inline fn getFuncSig(self: *Sema, id: FuncSigId) FuncSig {
        return self.funcSigs.items[id];
    }

    pub usingnamespace cy.types.SemaExt;
    pub usingnamespace cy.module.SemaExt;
};

pub const FuncSigKeyContext = struct {
    pub fn hash(_: @This(), key: FuncSigKey) u64 {
        var c = std.hash.Wyhash.init(0);
        const bytes: [*]const u8 = @ptrCast(key.paramPtr);
        c.update(bytes[0..key.paramLen*4]);
        c.update(std.mem.asBytes(&key.ret));
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

test "sema internals." {
    if (builtin.mode == .ReleaseFast) {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(LocalVar), 28);
        } else {
            try t.eq(@sizeOf(LocalVar), 32);
        }
    } else {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(LocalVar), 28);
        } else {
            try t.eq(@sizeOf(LocalVar), 40);
        }
    }

    if (cy.is32Bit) {
        try t.eq(@sizeOf(FuncSig), 12);
    } else {
        try t.eq(@sizeOf(FuncSig), 16);
    }

    try t.eq(@sizeOf(CapVarDesc), 4);

    try t.eq(@offsetOf(Sema, "alloc"), @offsetOf(vmc.Sema, "alloc"));
    try t.eq(@offsetOf(Sema, "compiler"), @offsetOf(vmc.Sema, "compiler"));
    try t.eq(@offsetOf(Sema, "funcSigs"), @offsetOf(vmc.Sema, "funcSigs"));
    try t.eq(@offsetOf(Sema, "funcSigMap"), @offsetOf(vmc.Sema, "funcSigMap"));
}

pub fn getBlockParams(c: *const cy.Chunk, block: *Block) []const LocalVar {
    return c.varStack.items[block.varStart..block.varStart+block.numParams];
}

/// Includes aliases.
pub fn getBlockLocals(c: *const cy.Chunk, block: *Block) []const LocalVar {
    return c.varStack.items[block.varStart+block.numParams..];
}
