const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const cc = @import("capi.zig");
const ir = cy.ir;
const vmc = @import("vm_c.zig");
const rt = cy.rt;
const bt = cy.types.BuiltinTypes;
const Nullable = cy.Nullable;
const sema = @This();
const cte = cy.cte;
const fmt = cy.fmt;
const v = fmt.v;
const module = cy.module;

const ChunkId = cy.chunk.ChunkId;
const TypeId = cy.types.TypeId;
const CompactType = cy.types.CompactType;
const Sym = cy.Sym;
const ReturnCstr = cy.types.ReturnCstr;

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
    dynamicLastMutBlockId: BlockId,

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

            /// If declaration has an initializer.
            hasInit: bool,

            /// Currently a captured var always needs to be lifted.
            /// In the future, the concept of a immutable variable could change this.
            lifted: bool,

            /// If var is hidden, user code can not reference it.
            hidden: bool,

            /// For locals this points to a ir.DeclareLocal.
            /// This is NullId for params.
            declIrStart: u32,
        },
        objectMemberAlias: struct {
            fieldIdx: u8,
        },
        parentObjectMemberAlias: struct {
            parentVarId: u32,
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

const VarBlock = extern struct {
    varId: LocalVarId,
    blockId: BlockId,
};

pub const VarShadow = extern struct {
    namePtr: [*]const u8,
    nameLen: u32,
    varId: LocalVarId,
    blockId: BlockId,
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

pub const BlockId = u32;

pub const Block = struct {
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

    pub fn init(nodeId: cy.NodeId, assignedVarStart: usize, varStart: usize, varShadowStart: usize) Block {
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

    pub fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        self.prevVarTypes.deinit(alloc);
    }
};

pub const ProcId = u32;

/// Container for main, fiber, function, and lambda blocks.
pub const Proc = struct {
    /// `varStack[varStart]` is the start of params.
    numParams: u8,

    /// Captured vars. 
    captures: std.ArrayListUnmanaged(LocalVarId),

    /// Maps a name to a var.
    /// This is updated as blocks declare their own locals and restored
    /// when blocks end.
    /// This can be deinited after ending the sema block.
    /// TODO: Move to chunk level.
    nameToVar: std.StringHashMapUnmanaged(VarBlock),

    /// First sub block id is recorded so the rest can be obtained by advancing
    /// the id in the same order it was traversed in the sema pass.
    firstBlockId: BlockId,

    /// Current sub block depth.
    blockDepth: u32,

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

    pub fn init(nodeId: cy.NodeId, func: ?*cy.Func, firstBlockId: BlockId, isStaticFuncBlock: bool, varStart: u32) Proc {
        return .{
            .nameToVar = .{},
            .numParams = 0,
            .blockDepth = 0,
            .func = func,
            .firstBlockId = firstBlockId,
            .isStaticFuncBlock = isStaticFuncBlock,
            .nodeId = nodeId,
            .captures = .{},
            .maxLocals = 0,
            .curNumLocals = 0,
            .varStart = varStart,
            .irStart = cy.NullId,
        };
    }

    pub fn deinit(self: *Proc, alloc: std.mem.Allocator) void {
        self.nameToVar.deinit(alloc);
        self.captures.deinit(alloc);
    }

    fn getReturnType(self: *const Proc) !TypeId {
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
    var stmt = head;
    while (stmt != cy.NullNode) {
        stmt = try semaStmt(c, stmt);
    }
}

/// Returns the next stmt node or NullNode.
pub fn semaStmt(c: *cy.Chunk, nodeId: cy.NodeId) !cy.NodeId {
    c.curNodeId = nodeId;
    const node = c.ast.node(nodeId);
    if (cy.Trace) {
        var buf: [1024]u8 = undefined;
        var fbuf = std.io.fixedBufferStream(&buf);
        try c.encoder.write(fbuf.writer(), nodeId);
        log.tracev("stmt.{s}: \"{s}\"", .{@tagName(c.ast.node(nodeId).type()), fbuf.getWritten()});
    }
    switch (node.type()) {
        .exprStmt => {
            const returnMain = node.data.exprStmt.isLastRootStmt;
            const expr = try c.semaExprTarget(node.data.exprStmt.child, bt.Void);
            _ = try c.ir.pushStmt(c.alloc, .exprStmt, nodeId, .{
                .expr = expr.irIdx,
                .isBlockResult = returnMain,
            });
        },
        .breakStmt => {
            _ = try c.ir.pushStmt(c.alloc, .breakStmt, nodeId, {});
        },
        .continueStmt => {
            _ = try c.ir.pushStmt(c.alloc, .contStmt, nodeId, {});
        },
        .localDecl => {
            try localDecl(c, nodeId);
        },
        .opAssignStmt => {
            const op = node.data.opAssignStmt.op;
            switch (op) {
                .star,
                .slash,
                .percent,
                .caret,
                .plus,
                .minus => {},
                else => {
                    return c.reportErrorFmt("Unsupported op assign statement for {}.", &.{v(op)}, nodeId);
                }
            }

            const irIdx = try c.ir.pushStmt(c.alloc, .opSet, nodeId, .{ .op = op, .set_stmt = undefined });

            const leftId = node.data.opAssignStmt.left;
            const set_stmt = try assignStmt(c, nodeId, leftId, nodeId, .{ .rhsOpAssignBinExpr = true });
            c.ir.getStmtDataPtr(irIdx, .opSet).set_stmt = set_stmt;

            // Reset `opSet`'s next since pushed statements are appended to the top StmtBlock.
            c.ir.setStmtNext(irIdx, cy.NullId);
            c.ir.stmtBlockStack.items[c.ir.stmtBlockStack.items.len-1].last = irIdx;
        },
        .assignStmt => {
            _ = try assignStmt(c, nodeId, node.data.assignStmt.left, node.data.assignStmt.right, .{});
        },
        .dynobject_decl,
        .objectDecl,
        .structDecl,
        .passStmt,
        .staticDecl,
        .typeAliasDecl,
        .distinct_decl,
        .typeTemplate,
        .enumDecl,
        .funcDecl => {
            // Nop.
        },
        .forIterStmt => {
            const header = c.ast.node(node.data.forIterStmt.header);

            try preLoop(c, nodeId);
            const irIdx = try c.ir.pushEmptyStmt(c.alloc, .forIterStmt, nodeId);
            const iter = try c.semaExpr(header.data.forIterHeader.iterable, .{});
            try pushBlock(c, nodeId);

            var eachLocal: ?u8 = null;
            var countLocal: ?u8 = null;
            var hasSeqDestructure = false;
            var seqIrVarStart: u32 = undefined;
            if (header.data.forIterHeader.eachClause != cy.NullNode) {
                const eachClause = c.ast.node(header.data.forIterHeader.eachClause);
                if (eachClause.type() == .ident) {
                    const varId = try declareLocal(c, header.data.forIterHeader.eachClause, bt.Dynamic, false);
                    eachLocal = c.varStack.items[varId].inner.local.id;

                    if (header.forIterHeader_count() != cy.NullNode) {
                        const countVarId = try declareLocal(c, header.forIterHeader_count(), bt.Integer, false);
                        countLocal = c.varStack.items[countVarId].inner.local.id;
                    }
                } else if (eachClause.type() == .seqDestructure) {
                    const varId = try declareLocalName(c, "$elem", bt.Dynamic, false, header.data.forIterHeader.eachClause);
                    eachLocal = c.varStack.items[varId].inner.local.id;

                    if (header.forIterHeader_count() != cy.NullNode) {
                        const countVarId = try declareLocal(c, header.forIterHeader_count(), bt.Integer, false);
                        countLocal = c.varStack.items[countVarId].inner.local.id;
                    }

                    var curId = eachClause.data.seqDestructure.head;

                    seqIrVarStart = @intCast(c.dataU8Stack.items.len);
                    while (curId != cy.NullNode) {
                        const varId2 = try declareLocal(c, curId, bt.Dynamic, false);
                        const irVarId = c.varStack.items[varId2].inner.local.id;
                        try c.dataU8Stack.append(c.alloc, .{ .irLocal = irVarId });
                        const cur = c.ast.node(curId);
                        curId = cur.next();
                    }
                    hasSeqDestructure = true;
                } else {
                    return c.reportErrorFmt("Unsupported each clause: {}", &.{v(eachClause.type())}, header.data.forIterHeader.eachClause);
                }
            }

            const declHead = c.ir.getAndClearStmtBlock();

            // Begin body code.
            if (hasSeqDestructure) {
                const eachClause = c.ast.node(header.data.forIterHeader.eachClause);
                const destrIdx = try c.ir.pushEmptyStmt(c.alloc, .destrElemsStmt, header.data.forIterHeader.eachClause);
                const locals = c.dataU8Stack.items[seqIrVarStart..];
                const irStart = c.ir.buf.items.len;
                try c.ir.buf.resize(c.alloc, c.ir.buf.items.len + locals.len);
                for (locals, 0..) |local, i| {
                    c.ir.buf.items[irStart + i] = local.irLocal;
                }
                const right = try c.ir.pushExpr(.local, c.alloc, bt.Any, header.data.forIterHeader.eachClause, .{ .id = eachLocal.? });
                c.ir.setStmtData(destrIdx, .destrElemsStmt, .{
                    .numLocals = eachClause.data.seqDestructure.numArgs,
                    .right = right,
                });
                c.dataU8Stack.items.len = seqIrVarStart;
            }

            try semaStmts(c, node.data.forIterStmt.bodyHead);
            const stmtBlock = try popLoopBlock(c);

            c.ir.setStmtData(irIdx, .forIterStmt, .{
                .iter = iter.irIdx, .eachLocal = eachLocal,
                .countLocal = countLocal, .declHead = declHead,
                .bodyHead = stmtBlock.first,
            });
        },
        .forRangeStmt => {
            try preLoop(c, nodeId);
            const irIdx = try c.ir.pushEmptyStmt(c.alloc, .forRangeStmt, nodeId);

            const header = c.ast.node(node.data.forRangeStmt.header);
            const range_start = try c.semaExpr(header.data.forRangeHeader.start, .{});
            const range_end = try c.semaExpr(header.data.forRangeHeader.end, .{});

            try pushBlock(c, nodeId);

            const hasEach = header.head.data.forRangeHeader.eachClause != cy.NullNode;
            var eachLocal: ?u8 = null;
            if (hasEach) {
                const eachClause = c.ast.node(header.head.data.forRangeHeader.eachClause);
                if (eachClause.type() == .ident) {
                    const varId = try declareLocal(c, header.head.data.forRangeHeader.eachClause, bt.Integer, false);
                    eachLocal = c.varStack.items[varId].inner.local.id;
                } else {
                    return c.reportErrorFmt("Unsupported each clause: {}", &.{v(eachClause.type())}, header.head.data.forRangeHeader.eachClause);
                }
            }

            const declHead = c.ir.getAndClearStmtBlock();

            try semaStmts(c, node.data.forRangeStmt.bodyHead);
            const stmtBlock = try popLoopBlock(c);
            c.ir.setStmtData(irIdx, .forRangeStmt, .{
                .eachLocal = eachLocal,
                .start = range_start.irIdx,
                .end = range_end.irIdx,
                .bodyHead = stmtBlock.first,
                .increment = header.data.forRangeHeader.increment,
                .declHead = declHead,
            });
        },
        .whileInfStmt => {
            try preLoop(c, node.data.whileInfStmt.bodyHead);
            const loc = try c.ir.pushEmptyStmt(c.alloc, .loopStmt, nodeId);
            try pushBlock(c, nodeId);
            {
                try semaStmts(c, node.data.whileInfStmt.bodyHead);
                _ = try c.ir.pushStmt(c.alloc, .contStmt, nodeId, {});
            }
            const stmtBlock = try popLoopBlock(c);
            c.ir.setStmtData(loc, .loopStmt, .{
                .body_head = stmtBlock.first,
            });
        },
        .whileCondStmt => {
            try preLoop(c, nodeId);
            const loc = try c.ir.pushEmptyStmt(c.alloc, .loopStmt, nodeId);

            try pushBlock(c, nodeId);
            {
                const if_stmt = try c.ir.pushEmptyStmt(c.alloc, .ifStmt, node.data.whileCondStmt.cond);
                const cond = try c.semaExprCstr(node.data.whileCondStmt.cond, bt.Boolean);
                try pushBlock(c, node.data.whileCondStmt.cond);
                {
                    try semaStmts(c, node.data.whileCondStmt.bodyHead);
                    _ = try c.ir.pushStmt(c.alloc, .contStmt, nodeId, {});
                }
                const if_block = try popBlock(c);
                c.ir.setStmtData(if_stmt, .ifStmt, .{
                    .cond = cond.irIdx,
                    .body_head = if_block.first,
                    .else_block = cy.NullId,
                });
            }
            const stmtBlock = try popLoopBlock(c);
            c.ir.setStmtData(loc, .loopStmt, .{
                .body_head = stmtBlock.first,
            });
        },
        .whileOptStmt => {
            try preLoop(c, nodeId);
            const loc = try c.ir.pushEmptyStmt(c.alloc, .loopStmt, nodeId);

            const header = c.ast.node(node.data.whileOptStmt.header);
            if (header.data.whileOptHeader.capture == cy.NullNode) {
                return c.reportErrorFmt("Expected unwrapped variable declaration.", &.{}, nodeId);
            }

            try pushBlock(c, nodeId);
            {
                const if_stmt = try c.ir.pushEmptyStmt(c.alloc, .ifUnwrapStmt, header.data.whileOptHeader.opt);
                const opt = try c.semaOptionExpr(header.data.whileOptHeader.opt);
                try pushBlock(c, header.data.whileOptHeader.opt);
                var decl_head: u32 = undefined;
                var unwrap_local: u8 = undefined;
                {
                    const unwrap_t = if (opt.type.dynamic) bt.Dynamic else b: {
                        break :b c.sema.getTypeSym(opt.type.id).cast(.enum_t).getMemberByIdx(1).payloadType;
                    };
                    const unwrap_var = try declareLocal(c, header.data.whileOptHeader.capture, unwrap_t, false);
                    unwrap_local = @intCast(c.varStack.items[unwrap_var].inner.local.id);

                    decl_head = c.ir.getAndClearStmtBlock();
                    try semaStmts(c, node.data.whileOptStmt.bodyHead);
                    _ = try c.ir.pushStmt(c.alloc, .contStmt, nodeId, {});
                }
                const if_block = try popBlock(c);
                c.ir.setStmtData(if_stmt, .ifUnwrapStmt, .{
                    .opt = opt.irIdx,
                    .unwrap_local = unwrap_local,
                    .decl_head = decl_head,
                    .body_head = if_block.first,
                    .else_block = cy.NullId,
                });
            }
            const stmtBlock = try popLoopBlock(c);
            c.ir.setStmtData(loc, .loopStmt, .{
                .body_head = stmtBlock.first,
            });
        },
        .switchStmt => {
            try semaSwitchStmt(c, nodeId);
        },
        .if_stmt => {
            try semaIfStmt(c, nodeId, node);
        },
        .if_unwrap_stmt => {
            try semaIfUnwrapStmt(c, nodeId, node);
        },
        .tryStmt => {
            var data: ir.TryStmt = undefined;
            const irIdx = try c.ir.pushEmptyStmt(c.alloc, .tryStmt, nodeId);
            try pushBlock(c, nodeId);
            try semaStmts(c, node.data.tryStmt.bodyHead);
            var stmtBlock = try popBlock(c);
            data.bodyHead = stmtBlock.first;

            try pushBlock(c, nodeId);
            const catchStmt = c.ast.node(node.data.tryStmt.catchStmt);
            if (catchStmt.data.catchStmt.errorVar != cy.NullNode) {
                const id = try declareLocal(c, catchStmt.data.catchStmt.errorVar, bt.Error, false);
                data.hasErrLocal = true;
                data.errLocal = c.varStack.items[id].inner.local.id;
            } else {
                data.hasErrLocal = false;
            }
            try semaStmts(c, catchStmt.data.catchStmt.bodyHead);
            stmtBlock = try popBlock(c);
            data.catchBodyHead = stmtBlock.first;

            c.ir.setStmtData(irIdx, .tryStmt, data);
        },
        .importStmt => {},
        .returnStmt => {
            _ = try c.ir.pushStmt(c.alloc, .retStmt, nodeId, {});
        },
        .returnExprStmt => {
            const proc = c.proc();
            const retType = try proc.getReturnType();

            const expr = try c.semaExprCstr(node.data.returnExprStmt.child, retType);
            _ = try c.ir.pushStmt(c.alloc, .retExprStmt, nodeId, .{
                .expr = expr.irIdx,
            });
        },
        .comptimeStmt => {
            const expr = c.ast.node(node.data.comptimeStmt.expr);
            if (expr.type() == .callExpr) {
                const callee = c.ast.node(expr.data.callExpr.callee);
                const name = c.ast.nodeString(callee);

                if (std.mem.eql(u8, "genLabel", name)) {
                    if (expr.data.callExpr.numArgs != 1) {
                        return c.reportErrorFmt("genLabel expected 1 arg", &.{}, nodeId);
                    }

                    const arg = c.ast.node(expr.data.callExpr.argHead);
                    if (arg.type() != .stringLit and arg.type() != .raw_string_lit) {
                        return c.reportErrorFmt("genLabel expected string arg", &.{}, nodeId);
                    }

                    const label = c.ast.nodeString(arg);
                    _ = try c.ir.pushStmt(c.alloc, .pushDebugLabel, nodeId, .{ .name = label });
                } else if (std.mem.eql(u8, "dumpLocals", name)) {
                    const proc = c.proc();
                    try c.dumpLocals(proc);
                } else if (std.mem.eql(u8, "dumpBytecode", name)) {
                    _ = try c.ir.pushStmt(c.alloc, .dumpBytecode, nodeId, {});
                } else if (std.mem.eql(u8, "verbose", name)) {
                    cc.setVerbose(true);
                    _ = try c.ir.pushStmt(c.alloc, .verbose, nodeId, .{ .verbose = true });
                } else if (std.mem.eql(u8, "verboseOff", name)) {
                    cc.setVerbose(false);
                    _ = try c.ir.pushStmt(c.alloc, .verbose, nodeId, .{ .verbose = false });
                } else {
                    return c.reportErrorFmt("Unsupported annotation: {}", &.{v(name)}, nodeId);
                }
            } else {
                return c.reportErrorFmt("Unsupported expr: {}", &.{v(expr.type())}, nodeId);
            }
        },
        else => return c.reportErrorFmt("Unsupported statement: {}", &.{v(node.type())}, nodeId),
    }
    return node.next();
}

fn semaElseStmt(c: *cy.Chunk, node_id: cy.NodeId, node: cy.Node) !u32 {
    if (node.type() == .else_block) {
        if (node.data.else_block.cond == cy.NullNode) {
            // else.
            const irElseIdx = try c.ir.pushEmptyExpr(.else_block, c.alloc, undefined, node_id);
            try pushBlock(c, node_id);
            try semaStmts(c, node.data.else_block.body_head);
            const stmtBlock = try popBlock(c);

            c.ir.setExprData(irElseIdx, .else_block, .{
                .cond = cy.NullId, .body_head = stmtBlock.first, .else_block = cy.NullId,
            });

            if (node.next() != cy.NullNode) {
                return c.reportErrorFmt("Can not have additional blocks after `else`.", &.{}, node.next());
            }
            return irElseIdx;
        } else {
            const irElseIdx = try c.ir.pushEmptyExpr(.else_block, c.alloc, undefined, node_id);
            const elseCond = try c.semaExprCstr(node.data.else_block.cond, bt.Boolean);

            try pushBlock(c, node_id);
            try semaStmts(c, node.data.else_block.body_head);
            const stmtBlock = try popBlock(c);

            c.ir.setExprData(irElseIdx, .else_block, .{
                .cond = elseCond.irIdx, .body_head = stmtBlock.first, .else_block = cy.NullId,
            });
            return irElseIdx;
        }
    } else return error.TODO;
}

fn semaIfUnwrapStmt(c: *cy.Chunk, nodeId: cy.NodeId, node: cy.Node) !void {
    const loc = try c.ir.pushEmptyStmt(c.alloc, .ifUnwrapStmt, nodeId);

    const if_unwrap = c.ast.node(node.data.if_unwrap_stmt.if_unwrap);
    const opt = try c.semaOptionExpr(if_unwrap.data.if_unwrap.opt);

    try pushBlock(c, nodeId);
    const body_head = if_unwrap.head.data.if_unwrap.body_head;
    var decl_head: u32 = undefined;
    var unwrap_local: u8 = undefined;
    {
        const unwrap_t = if (opt.type.dynamic) bt.Dynamic else b: {
            break :b c.sema.getTypeSym(opt.type.id).cast(.enum_t).getMemberByIdx(1).payloadType;
        };
        const unwrap_var = try declareLocal(c, if_unwrap.data.if_unwrap.unwrap, unwrap_t, false);
        unwrap_local = @intCast(c.varStack.items[unwrap_var].inner.local.id);

        decl_head = c.ir.getAndClearStmtBlock();
        try semaStmts(c, body_head);
    }
    const if_block = try popBlock(c);

    if (node.data.if_unwrap_stmt.else_block == cy.NullNode) {
        c.ir.setStmtData(loc, .ifUnwrapStmt, .{
            .opt = opt.irIdx,
            .unwrap_local = unwrap_local,
            .decl_head = decl_head,
            .body_head = if_block.first,
            .else_block = cy.NullId,
        });
        return;
    }

    var else_id = node.data.if_unwrap_stmt.else_block;
    var else_n = c.ast.node(else_id);
    var else_loc = try semaElseStmt(c, else_id, else_n);
    c.ir.setStmtData(loc, .ifUnwrapStmt, .{
        .opt = opt.irIdx,
        .unwrap_local = unwrap_local,
        .decl_head = decl_head,
        .body_head = if_block.first,
        .else_block = else_loc,
    });

    else_id = else_n.next();
    while (else_id != cy.NullNode) {
        else_n = c.ast.node(else_id);
        const next_else_loc = try semaElseStmt(c, else_id, else_n);

        c.ir.getExprDataPtr(else_loc, .else_block).else_block = next_else_loc;
        else_loc = next_else_loc;
        else_id = else_n.next();
    }
}

fn semaIfStmt(c: *cy.Chunk, nodeId: cy.NodeId, node: cy.Node) !void {
    const irIdx = try c.ir.pushEmptyStmt(c.alloc, .ifStmt, nodeId);

    const branch = c.ast.node(node.data.if_stmt.if_branch);
    const cond = try c.semaExprCstr(branch.data.if_branch.cond, bt.Boolean);

    try pushBlock(c, nodeId);
    try semaStmts(c, branch.data.if_branch.body_head);
    const ifStmtBlock = try popBlock(c);

    if (node.data.if_stmt.else_block == cy.NullNode) {
        c.ir.setStmtData(irIdx, .ifStmt, .{
            .cond = cond.irIdx,
            .body_head = ifStmtBlock.first,
            .else_block = cy.NullId,
        });
        return;
    }

    var else_id = node.data.if_stmt.else_block;
    var else_n = c.ast.node(else_id);
    var else_loc = try semaElseStmt(c, else_id, else_n);
    c.ir.setStmtData(irIdx, .ifStmt, .{
        .cond = cond.irIdx,
        .body_head = ifStmtBlock.first,
        .else_block = else_loc,
    });

    else_id = else_n.next();
    while (else_id != cy.NullNode) {
        else_n = c.ast.node(else_id);
        const next_else_loc = try semaElseStmt(c, else_id, else_n);

        c.ir.getExprDataPtr(else_loc, .else_block).else_block = next_else_loc;
        else_loc = next_else_loc;
        else_id = else_n.next();
    }
}

const AssignOptions = struct {
    rhsOpAssignBinExpr: bool = false,
};

/// Pass rightId explicitly to perform custom sema on op assign rhs.
fn assignStmt(c: *cy.Chunk, nodeId: cy.NodeId, leftId: cy.NodeId, rightId: cy.NodeId, opts: AssignOptions) !u32 {
    const left = c.ast.node(leftId);
    switch (left.type()) {
        .array_expr => {
            const irStart = try c.ir.pushEmptyStmt(c.alloc, .set, nodeId);

            const array = c.ast.node(left.data.array_expr.array);
            if (array.data.arrayLit.numArgs != 1) {
                return c.reportErrorFmt("Unsupported array expr.", &.{}, left.data.array_expr.array);
            }

            const recv = try c.semaExpr(left.data.array_expr.left, .{});
            var specialized = false;
            var index: ExprResult = undefined;
            if (recv.type.id == bt.List) {
                index = try c.semaExprTarget(array.data.arrayLit.argHead, bt.Integer);
                specialized = true;
            } else if (recv.type.id == bt.Map) {
                index = try c.semaExpr(array.data.arrayLit.argHead, .{});
                specialized = true;
            } else {
                index = try c.semaExpr(array.data.arrayLit.argHead, .{});
            }

            // RHS.
            const rightExpr = Expr.init(rightId);
            const right = try c.semaExprOrOpAssignBinExpr(rightExpr, opts.rhsOpAssignBinExpr);

            if (specialized) {
                c.ir.setStmtCode(irStart, .setIndex);
                c.ir.setStmtData(irStart, .setIndex, .{
                    .index = .{
                        .recvT = recv.type.id,
                        .rec = recv.irIdx,
                        .index = index.irIdx,
                        .right = right.irIdx,
                    },
                });
            } else {
                c.ir.setStmtCode(irStart, .setCallObjSymTern);
                c.ir.setStmtData(irStart, .setCallObjSymTern, .{
                    .callObjSymTern = .{
                        // .recvT = recvT.id,
                        .name = "$setIndex",
                        .rec = recv.irIdx,
                        .index = index.irIdx,
                        .right = right.irIdx,
                    },
                });
            }
            return irStart;
        },
        .accessExpr => {
            const rec = try c.semaExpr(left.data.accessExpr.left, .{});
            const name = c.ast.nodeStringById(left.data.accessExpr.right);
            const type_sym = c.sema.getTypeSym(rec.type.id);
            const debug_node = left.data.accessExpr.right;
            if (rec.type.isDynAny()) {
                const expr = Expr{ .nodeId = rightId, .reqTypeCstr = false, .target_t = bt.Any };
                const right = try c.semaExprOrOpAssignBinExpr(expr, opts.rhsOpAssignBinExpr);
                return try c.ir.pushStmt(c.alloc, .set_field_dyn, debug_node, .{ .set_field_dyn = .{
                    .name = name,
                    .rec = rec.irIdx,
                    .right = right.irIdx,
                }});
            }

            const set_info = try checkSymSetField(c, type_sym, name, debug_node);
            if (set_info.use_set_method) {
                const expr = Expr{ .nodeId = rightId, .reqTypeCstr = false, .target_t = bt.Any };
                const right = try c.semaExprOrOpAssignBinExpr(expr, opts.rhsOpAssignBinExpr);
                return try c.ir.pushStmt(c.alloc, .set_field_dyn, debug_node, .{ .set_field_dyn = .{
                    .name = name,
                    .rec = rec.irIdx,
                    .right = right.irIdx,
                }});
            }

            const recIsStructFromFieldAccess = rec.resType == .field and c.sema.getTypeKind(rec.type.id) == .@"struct";
            if (recIsStructFromFieldAccess) {
                // Continue the field chain.
                const fidx = try c.ir.reserveData(c.alloc, u8);
                fidx.* = set_info.idx;
                const existing = c.ir.getExprDataPtr(rec.irIdx, .field);
                c.ir.setExprType(rec.irIdx, set_info.typeId);
                existing.numNestedFields += 1;
                c.ir.setNode(rec.irIdx, debug_node);

                const expr = Expr{ .nodeId = rightId, .reqTypeCstr = true, .target_t = set_info.typeId };
                const right = try c.semaExprOrOpAssignBinExpr(expr, opts.rhsOpAssignBinExpr);
                return try c.ir.pushStmt(c.alloc, .set_field, debug_node, .{ .set_field = .{
                    .field = rec.irIdx,
                    .right = right.irIdx,
                }});
            } else {
                const loc = try c.ir.pushExpr(.field, c.alloc, set_info.typeId, debug_node, .{
                    .idx = set_info.idx,
                    .rec = rec.irIdx,
                    .numNestedFields = 0,
                });

                const expr = Expr{ .nodeId = rightId, .reqTypeCstr = true, .target_t = set_info.typeId };
                const right = try c.semaExprOrOpAssignBinExpr(expr, opts.rhsOpAssignBinExpr);
                return try c.ir.pushStmt(c.alloc, .set_field, debug_node, .{ .set_field = .{
                    .field = loc,
                    .right = right.irIdx,
                }});
            }
        },
        .ident => {
            var right: ExprResult = undefined;
            const leftRes = try c.semaExpr(leftId, .{});
            const leftT = leftRes.type;
            if (leftRes.resType == .local) {
                right = try assignToLocalVar(c, leftRes, rightId, opts);
            } else if (leftRes.resType == .field) {
                const expr = Expr{ .nodeId = rightId, .reqTypeCstr = true, .target_t = leftT.id };
                right = try c.semaExprOrOpAssignBinExpr(expr, opts.rhsOpAssignBinExpr);
                return try c.ir.pushStmt(c.alloc, .set_field, leftId, .{ .set_field = .{
                    .field = leftRes.irIdx,
                    .right = right.irIdx,
                }});
            } else {
                const rightExpr = Expr{ .nodeId = rightId, .reqTypeCstr = !leftT.dynamic, .target_t = leftT.id };
                right = try c.semaExprOrOpAssignBinExpr(rightExpr, opts.rhsOpAssignBinExpr);
            }

            const irStart = try c.ir.pushEmptyStmt(c.alloc, .set, nodeId);
            c.ir.setStmtData(irStart, .set, .{ .generic = .{
                .left_t = leftT,
                .right_t = right.type,
                .left = leftRes.irIdx,
                .right = right.irIdx,
            }});
            switch (leftRes.resType) {
                .varSym         => c.ir.setStmtCode(irStart, .setVarSym),
                .func           => {
                    return c.reportErrorFmt("Can not reassign to a namespace function.", &.{}, nodeId);
                },
                .local          => c.ir.setStmtCode(irStart, .setLocal),
                .capturedLocal  => c.ir.setStmtCode(irStart, .setCaptured),
                else => {
                    log.tracev("leftRes {s} {}", .{@tagName(leftRes.resType), leftRes.type});
                    return c.reportErrorFmt("Assignment to the left `{}` is unsupported.", &.{v(left.type())}, nodeId);
                }
            }
            return irStart;
        },
        else => {
            return c.reportErrorFmt("Assignment to the left `{}` is unsupported.", &.{v(left.type())}, nodeId);
        }
    }
}

fn semaAccessField(c: *cy.Chunk, rec: ExprResult, field: cy.NodeId) !ExprResult {
    const field_n = c.ast.node(field);
    if (field_n.type() != .ident) {
        return error.Unexpected;
    }
    const name = c.ast.nodeString(field_n);
    return semaAccessFieldName(c, rec, name, field);
}

fn semaAccessFieldName(c: *cy.Chunk, rec: ExprResult, name: []const u8, field: cy.NodeId) !ExprResult {
    if (rec.type.isDynAny()) {
        const loc = try c.ir.pushExpr(.fieldDyn, c.alloc, bt.Any, field, .{
            .name = name,
            .rec = rec.irIdx,
        });
        return ExprResult.initCustom(loc, .fieldDyn, CompactType.initDynamic(bt.Any), undefined);
    }

    const type_sym = c.sema.getTypeSym(rec.type.id);
    const info = try checkSymGetField(c, type_sym, name, field);
    if (info.use_get_method) {
        const loc = try c.ir.pushExpr(.fieldDyn, c.alloc, bt.Any, field, .{
            .name = name,
            .rec = rec.irIdx,
        });
        return ExprResult.initCustom(loc, .fieldDyn, CompactType.initDynamic(bt.Any), undefined);
    }

    const field_t = CompactType.init2(info.typeId, rec.type.dynamic);

    const recIsStructFromFieldAccess = rec.resType == .field and c.sema.getTypeKind(rec.type.id) == .@"struct";
    if (recIsStructFromFieldAccess) {
        // Continue the field chain.
        const fidx = try c.ir.reserveData(c.alloc, u8);
        fidx.* = info.idx;
        const existing = c.ir.getExprDataPtr(rec.irIdx, .field);
        c.ir.setExprType(rec.irIdx, field_t.id);
        existing.numNestedFields += 1;
        c.ir.setNode(rec.irIdx, field);
        return ExprResult.initCustom(rec.irIdx, .field, field_t, undefined);
    } else {
        const loc = try c.ir.pushExpr(.field, c.alloc, info.typeId, field, .{
            .idx = info.idx,
            .rec = rec.irIdx,
            .numNestedFields = 0,
        });
        return ExprResult.initCustom(loc, .field, field_t, undefined);
    }
}

fn checkGetField(c: *cy.Chunk, rec_t: TypeId, fieldName: []const u8, nodeId: cy.NodeId) !FieldResult {
    const sym = c.sema.getTypeSym(rec_t);
    switch (sym.type) {
        .enum_t,
        .dynobject_t,
        .object_t,
        .struct_t => {
            return checkSymGetField(c, sym, fieldName, nodeId);
        },
        else => {
            const name = c.sema.getTypeBaseName(rec_t);
            return c.reportErrorFmt("Field access unsupported for `{}`.", &.{v(name)}, nodeId);
        },
    }
}

fn checkSymSetField(c: *cy.Chunk, type_sym: *cy.Sym, name: []const u8, nodeId: cy.NodeId) !SetFieldResult {
    const sym = type_sym.getMod().?.getSym(name) orelse {
        // TODO: This depends on $get being known, make sure $get is a nested declaration.
        const type_e = c.sema.types.items[type_sym.getStaticType().?];
        if (type_e.has_set_method) {
            return .{ .idx = undefined, .typeId = undefined, .use_set_method = true };
        } else {
            const type_name = type_sym.name();
            return c.reportErrorFmt("Field `{}` does not exist in `{}`.", &.{v(name), v(type_name)}, nodeId);
        }
    };
    if (sym.type != .field) {
        const type_name = type_sym.name();
        return c.reportErrorFmt("Type `{}` does not have a field named `{}`.", &.{v(type_name), v(name)}, nodeId);
    }
    const field = sym.cast(.field);
    return .{
        .idx = @intCast(field.idx),
        .typeId = field.type,
        .use_set_method = false,
    };
}

fn checkSymGetField(c: *cy.Chunk, type_sym: *cy.Sym, name: []const u8, nodeId: cy.NodeId) !FieldResult {
    const sym = type_sym.getMod().?.getSym(name) orelse {
        // TODO: This depends on $get being known, make sure $get is a nested declaration.
        const type_e = c.sema.types.items[type_sym.getStaticType().?];
        if (type_e.has_get_method) {
            return .{ .idx = undefined, .typeId = undefined, .use_get_method = true };
        } else {
            const type_name = type_sym.name();
            return c.reportErrorFmt("Field `{}` does not exist in `{}`.", &.{v(name), v(type_name)}, nodeId);
        }
    };
    if (sym.type != .field) {
        const type_name = type_sym.name();
        return c.reportErrorFmt("Type `{}` does not have a field named `{}`.", &.{v(type_name), v(name)}, nodeId);
    }
    const field = sym.cast(.field);
    return .{
        .idx = @intCast(field.idx),
        .typeId = field.type,
        .use_get_method = false,
    };
}

fn symGetField(type_sym: *cy.Sym, name: []const u8) ?FieldResult {
    const sym = type_sym.getMod().?.getSym(name) orelse return null;
    if (sym.type != .field) return null;
    const field = sym.cast(.field);
    return .{
        .idx = @intCast(field.idx),
        .typeId = field.type,
    };
}

const FieldResult = struct {
    typeId: TypeId,
    idx: u8,
    use_get_method: bool,
};

const SetFieldResult = struct {
    typeId: TypeId,
    idx: u8,
    use_set_method: bool,
};

pub fn declareTypeTemplate(c: *cy.Chunk, nodeId: cy.NodeId, ctNodes: []const cy.NodeId) !void {
    const node = c.ast.node(nodeId);
    const typeDecl = c.ast.node(node.data.typeTemplate.typeDecl);
    switch (typeDecl.type()) {
        .objectDecl => {
            const header = c.ast.node(typeDecl.data.objectDecl.header);
            const name = c.ast.nodeStringById(header.data.objectHeader.name);
            var sigId: FuncSigId = undefined;
            const params = try resolveTemplateSig(c, node.data.typeTemplate.paramHead, node.data.typeTemplate.numParams, &sigId);
            try c.declareTypeTemplate(@ptrCast(c.sym), name, sigId, params, .object_t, ctNodes, nodeId);
        },
        .enumDecl => {
            const name = c.ast.nodeStringById(typeDecl.data.enumDecl.name);
            var sigId: FuncSigId = undefined;
            const params = try resolveTemplateSig(c, node.data.typeTemplate.paramHead, node.data.typeTemplate.numParams, &sigId);
            try c.declareTypeTemplate(@ptrCast(c.sym), name, sigId, params, .enum_t, ctNodes, nodeId);
        },
        else => {
            return c.reportErrorFmt("Unsupported type template.", &.{}, nodeId);
        }
    }
}

pub fn declareDistinctType(c: *cy.Chunk, nodeId: cy.NodeId) !*cy.sym.Sym {
    const node = c.ast.node(nodeId);
    const header = c.ast.node(node.data.distinct_decl.header);
    const name = c.ast.nodeStringById(header.data.distinct_header.name);

    // Check for #host modifier.
    var type_id: ?cy.TypeId = null;
    if (header.head.data.objectHeader.modHead != cy.NullNode) {
        const modifier = c.ast.node(header.head.data.objectHeader.modHead);
        if (modifier.data.dirModifier.type == .host) {
            type_id = try getHostTypeId(c, name, nodeId);
        }
    }

    // Check if target is a builtin type.
    const target = c.ast.node(header.data.distinct_header.target);
    if (target.type() == .comptimeExpr) {
        const expr = c.ast.node(target.data.comptimeExpr.child);
        if (expr.type() == .ident) {
            const builtin_n = c.ast.nodeString(expr);
            if (c.sema.builtins.get(builtin_n)) |builtin_sym| {
                switch (builtin_sym.type) {
                    .bool_t => {
                        const sym = try c.declareBoolType(@ptrCast(c.sym), name, type_id, nodeId);
                        return @ptrCast(sym);
                    },
                    .int_t => {
                        const bits = builtin_sym.data.int_t.bits;
                        const sym = try c.declareIntType(@ptrCast(c.sym), name, bits, type_id, nodeId);
                        return @ptrCast(sym);
                    },
                    .float_t => {
                        const bits = builtin_sym.data.float_t.bits;
                        const sym = try c.declareFloatType(@ptrCast(c.sym), name, bits, type_id, nodeId);
                        return @ptrCast(sym);
                    },
                    // else => {
                    //     return error.Unsupported;
                    // },
                }
            }
        }
    }

    const sym = try c.declareDistinctType(@ptrCast(c.sym), name, nodeId, type_id);
    return @ptrCast(sym);
}

pub fn declareTypeAlias(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.ast.node(nodeId);
    const name = c.ast.nodeStringById(node.data.typeAliasDecl.name);
    try c.declareTypeAlias(@ptrCast(c.sym), name, nodeId);
}

pub fn resolveModuleSpec(self: *cy.Chunk, buf: []u8, spec: []const u8, nodeId: cy.NodeId) ![]const u8 {
    self.compiler.hasApiError = false;

    var resUri: [*]const u8 = undefined;
    var resUriLen: usize = undefined;
    const params: cc.ResolverParams = .{
        .chunkId = self.id,
        .curUri = cc.toStr(self.srcUri),
        .spec = cc.toStr(spec),
        .buf = buf.ptr,
        .bufLen = buf.len,
        .resUri = @ptrCast(&resUri),
        .resUriLen = &resUriLen,
    };
    if (!self.compiler.moduleResolver.?(@ptrCast(self.compiler.vm), params)) {
        if (self.compiler.hasApiError) {
            return self.reportErrorFmt(self.compiler.apiError, &.{}, nodeId);
        } else {
            return self.reportErrorFmt("Failed to resolve module.", &.{}, nodeId);
        }
    }
    return resUri[0..resUriLen];
}

pub fn declareImport(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.ast.node(nodeId);
    const ident = c.ast.node(node.data.importStmt.name);
    const name = c.ast.nodeString(ident);

    var specPath: []const u8 = undefined;
    if (node.data.importStmt.spec != cy.NullNode) {
        const spec = c.ast.node(node.data.importStmt.spec);
        specPath = c.ast.nodeString(spec);
    } else {
        specPath = name;
    }

    var buf: [4096]u8 = undefined;
    const uri = try resolveModuleSpec(c, &buf, specPath, node.data.importStmt.spec);

    if (c.compiler.chunkMap.get(uri)) |chunk| {
        _ = try c.declareImport(@ptrCast(c.sym), name, @ptrCast(chunk.sym), nodeId);
    } else {
        const import = try c.declareImport(@ptrCast(c.sym), name, undefined, nodeId);

        // resUri is duped. Moves to chunk afterwards.
        const dupedResUri = try c.alloc.dupe(u8, uri);

        // Queue import task.
        try c.compiler.importTasks.append(c.alloc, .{
            .fromChunk = c,
            .nodeId = nodeId,
            .absSpec = dupedResUri,
            .import = import,
        });
    }
}

pub fn declareEnum(c: *cy.Chunk, nodeId: cy.NodeId) !*cy.sym.EnumType {
    const node = c.ast.node(nodeId);
    const name = c.ast.nodeStringById(node.data.enumDecl.name);

    const isChoiceType = node.data.enumDecl.isChoiceType;

    const enumt = try c.declareEnumType(@ptrCast(c.sym), name, isChoiceType, nodeId);
    return enumt;
}

pub fn declareEnumMembers(c: *cy.Chunk, sym: *cy.sym.EnumType, nodeId: cy.NodeId) !void {
    const node = c.ast.node(nodeId);

    var i: u32 = 0;
    var cur: cy.NodeId = node.data.enumDecl.memberHead;

    const members = try c.alloc.alloc(cy.SymId, node.data.enumDecl.numMembers);
    while (cur != cy.NullNode) : (i += 1) {
        const member = c.ast.node(cur);
        const memberNameN = c.ast.node(member.data.enumMember.name);
        const mName = c.ast.nodeString(memberNameN);
        var payloadType: cy.TypeId = cy.NullId;
        if (member.data.enumMember.typeSpec != cy.NullNode) {
            payloadType = try resolveTypeSpecNode(c, member.data.enumMember.typeSpec);
        }
        const modSymId = try c.declareEnumMember(@ptrCast(sym), mName, sym.type, i, payloadType, cur);
        members[i] = modSymId;
        cur = member.next();
    }
    sym.members = members.ptr;
    sym.numMembers = @intCast(members.len);
}

/// Returns an object or custom object type sym.
pub fn declareHostObject(c: *cy.Chunk, nodeId: cy.NodeId) !*cy.sym.Sym {
    const node = c.ast.node(nodeId);

    const header = c.ast.node(node.data.objectDecl.header); 
    const name = c.ast.nodeStringById(header.data.objectHeader.name);

    const typeLoader = c.typeLoader orelse {
        return c.reportErrorFmt("No type loader set for `{}`.", &.{v(name)}, nodeId);
    };

    const info = cc.TypeInfo{
        .mod = c.sym.sym().toC(),
        .name = cc.toStr(name),
        .idx = c.curHostTypeIdx,
    };
    c.curHostTypeIdx += 1;
    var res: cc.TypeResult = .{
        .data = .{
            .custom = .{
                .out_type_id = null,
                .type_id = cy.NullId,
                .get_children = null,
                .finalizer = null,
            },
        },
        .type = cc.BindTypeCustom,
    };
    log.tracev("Invoke type loader for: {s}", .{name});
    if (!typeLoader(@ptrCast(c.compiler.vm), info, &res)) {
        return c.reportErrorFmt("Failed to load #host type `{}` object.", &.{v(name)}, nodeId);
    }

    switch (res.type) {
        cc.BindTypeCustom => {
            const type_id: ?cy.TypeId = if (res.data.custom.type_id != cy.NullId) res.data.custom.type_id else null;
            const sym = try c.declareCustomObjectType(@ptrCast(c.sym), name, nodeId,
                res.data.custom.get_children, res.data.custom.finalizer, type_id);
            if (res.data.custom.out_type_id) |out_type_id| {
                log.tracev("output typeId: {}", .{sym.type});
                out_type_id.* = sym.type;
            }
            return @ptrCast(sym);
        },
        cc.BindTypeDecl => {
            const sym = try c.declareObjectType(@ptrCast(c.sym), name, nodeId, res.data.decl.type_id);
            return @ptrCast(sym);
        },
        else => {
            return error.Unsupported;
        },
    }
}

/// Only allows binding a predefined host type id (BIND_TYPE_DECL).
pub fn getHostTypeId(c: *cy.Chunk, name: []const u8, nodeId: cy.NodeId) !cy.TypeId {
    const typeLoader = c.typeLoader orelse {
        return c.reportErrorFmt("No type loader set for `{}`.", &.{v(name)}, nodeId);
    };

    const info = cc.TypeInfo{
        .mod = c.sym.sym().toC(),
        .name = cc.toStr(name),
        .idx = c.curHostTypeIdx,
    };
    c.curHostTypeIdx += 1;
    var res: cc.TypeResult = .{
        .data = .{
            .custom = .{
                .out_type_id = null,
                .type_id = cy.NullId,
                .get_children = null,
                .finalizer = null,
            },
        },
        .type = cc.BindTypeCustom,
    };
    log.tracev("Invoke type loader for: {s}", .{name});
    if (!typeLoader(@ptrCast(c.compiler.vm), info, &res)) {
        return c.reportErrorFmt("Failed to load #host type `{}`.", &.{v(name)}, nodeId);
    }

    switch (res.type) {
        cc.BindTypeCustom => {
            return error.Unsupported;
        },
        cc.BindTypeDecl => {
            return res.data.decl.type_id;
        },
        else => {
            return error.Unsupported;
        },
    }
}

pub fn declareTemplateVariant(c: *cy.Chunk, template: *cy.sym.TypeTemplate, variantId: u32) !*cy.sym.Sym {
    const tchunk = template.chunk();
    const template_n = tchunk.ast.node(template.declId);
    switch (template.kind) {
        .object_t => {
            const sym = try c.declareObjectVariantType(template, variantId);

            // Set variant in context chunk so sema ops know to swap nodes.
            const variant = template.variants.items[variantId];
            tchunk.patchTemplateNodes = variant.patchNodes;

            const mod = sym.getMod();
            const start = mod.chunk.funcs.items.len;

            try declareObjectFields(tchunk, @ptrCast(sym), template_n.data.typeTemplate.typeDecl);

            const object_n = tchunk.ast.node(template_n.data.typeTemplate.typeDecl);
            var func_id: cy.NodeId = object_n.data.objectDecl.funcHead;
            while (func_id != cy.NullNode) {
                const decl = try resolveImplicitMethodDecl(c, @ptrCast(sym), func_id);
                _ = try declareMethod(c, @ptrCast(sym), func_id, decl);
                func_id = c.ast.node(func_id).next();
            }

            // Defer method sema.
            try mod.chunk.variantFuncSyms.appendSlice(c.alloc, mod.chunk.funcs.items[start..]);
            return @ptrCast(sym);
        },
        .enum_t => {
            const enum_n = tchunk.ast.node(template_n.data.typeTemplate.typeDecl);
            const isChoiceType = enum_n.data.enumDecl.isChoiceType;
            const sym = try c.declareEnumVariantType(template, isChoiceType, variantId);

            // Set variant in context chunk so sema ops know to swap nodes.
            const variant = template.variants.items[variantId];
            tchunk.patchTemplateNodes = variant.patchNodes;

            try declareEnumMembers(tchunk, sym, template_n.data.typeTemplate.typeDecl);
            return @ptrCast(sym);
        },
    }
}

pub fn declareObject(c: *cy.Chunk, nodeId: cy.NodeId) !*cy.Sym {
    const node = c.ast.node(nodeId);

    const header = c.ast.node(node.data.objectDecl.header);

    // Check for #host modifier.
    if (header.head.data.objectHeader.modHead != cy.NullNode) {
        const modifier = c.ast.node(header.head.data.objectHeader.modHead);
        if (modifier.data.dirModifier.type == .host) {
            return @ptrCast(try declareHostObject(c, nodeId));
        }
    }

    const nameId = header.data.objectHeader.name;
    if (nameId != cy.NullNode) {
        const name = c.ast.nodeStringById(nameId);
        const sym = try c.declareObjectType(@ptrCast(c.sym), name, nodeId, null);
        return @ptrCast(sym);
    } else {
        // Unnamed object.
        const sym = try c.declareUnnamedObjectType(@ptrCast(c.sym), nodeId);
        return @ptrCast(sym);
    }
}

pub fn declareStruct(c: *cy.Chunk, nodeId: cy.NodeId) !*cy.Sym {
    const node = c.ast.node(nodeId);

    const header = c.ast.node(node.data.objectDecl.header);
    const nameId = header.data.objectHeader.name;
    if (nameId == cy.NullNode) {
        // Unnamed.
        const sym = try c.declareUnnamedStructType(@ptrCast(c.sym), nodeId);
        return @ptrCast(sym);
    }

    const name = c.ast.nodeStringById(nameId);

    // Check for #host modifier.
    var type_id: ?cy.TypeId = null;
    if (header.head.data.objectHeader.modHead != cy.NullNode) {
        const modifier = c.ast.node(header.head.data.objectHeader.modHead);
        if (modifier.data.dirModifier.type == .host) {
            type_id = try getHostTypeId(c, name, nodeId);
        }
    }

    const sym = try c.declareStructType(@ptrCast(c.sym), name, nodeId, type_id);
    return @ptrCast(sym);
}

pub fn resolveDistinctType(c: *cy.Chunk, distinct_t: *cy.sym.DistinctType) !*cy.Sym {
    const decl = c.ast.node(distinct_t.decl_id);

    const header = c.ast.node(decl.data.distinct_decl.header);
    const target_t = try resolveTypeSpecNode(c, header.data.distinct_header.target);
    const target_sym = c.sema.getTypeSym(target_t);
    if (target_sym.type == .object_t) {
        const object_t: *cy.sym.ObjectType = @ptrCast(distinct_t);
        object_t.* = cy.sym.ObjectType.init(distinct_t.head.parent.?, c, distinct_t.head.name(), distinct_t.decl_id, distinct_t.type);
        c.compiler.sema.types.items[object_t.type] = .{
            .sym = @ptrCast(object_t),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = 0,
            }},
        };
        try sema.declareObjectFields(c, @ptrCast(object_t), target_sym.cast(.object_t).declId);
        object_t.head.setDistinctType(true);
        return @ptrCast(object_t);
    } else {
        return error.Unsupported;
    }
}

/// modSym can be an object or core type.
pub fn declareObjectFields(c: *cy.Chunk, modSym: *cy.Sym, nodeId: cy.NodeId) !void {
    const node = c.ast.node(nodeId);

    // Load fields.
    var i: u32 = 0;

    if (modSym.type == .object_t or modSym.type == .struct_t) {
        const header = c.ast.node(node.data.objectDecl.header);

        // Only object types can have fields.
        const obj: *cy.sym.ObjectType = @ptrCast(modSym);

        const fields = try c.alloc.alloc(cy.sym.FieldInfo, header.data.objectHeader.numFields);
        errdefer c.alloc.free(fields);

        var fieldId: cy.NodeId = header.data.objectHeader.fieldHead;
        while (fieldId != cy.NullNode) : (i += 1) {
            const field = c.ast.node(fieldId);
            const fieldName = c.ast.nodeStringById(field.data.objectField.name);
            const fieldType = try resolveTypeSpecNode(c, field.data.objectField.typeSpec);

            const sym = try c.declareField(@ptrCast(obj), fieldName, i, fieldType, fieldId);
            fields[i] = .{
                .sym = @ptrCast(sym),
                .type = fieldType,
            };

            fieldId = field.next();
        }
        obj.fields = fields.ptr;
        obj.numFields = @intCast(fields.len);
        switch (modSym.type) {
            .object_t => {
                c.sema.types.items[obj.type].data.object = .{
                    .numFields = @intCast(obj.numFields),
                };
            },
            .struct_t => {
                c.sema.types.items[obj.type].data.@"struct" = .{
                    .numFields = @intCast(obj.numFields),
                };
            },
            else => return error.Unexpected,
        }
    } 
}

pub fn declareFuncInit(c: *cy.Chunk, parent: *cy.Sym, nodeId: cy.NodeId) !void {
    const node = c.ast.node(nodeId);
    if (node.data.func.bodyHead != cy.NullNode) {
        return error.Unexpected;
    }
    // Check if #host func.
    const modifierId = c.ast.node(node.data.func.header).funcHeader_modHead();
    if (modifierId != cy.NullNode) {
        const modifier = c.ast.node(modifierId);
        if (modifier.data.dirModifier.type == .host) {
            const decl = try resolveFuncDecl(c, parent, nodeId);
            _ = try declareHostFunc(c, parent, nodeId, decl);
            return;
        }
    }
    const header = c.ast.node(node.funcDecl_header());
    const name = c.ast.nodeStringById(header.funcHeader_name());
    return c.reportErrorFmt("`{}` is not a host function.", &.{v(name)}, nodeId);
}

fn declareGenericFunc(c: *cy.Chunk, parent: *cy.Sym, nodeId: cy.NodeId) !void {
    const node = c.ast.node(nodeId);

    // Object function.
    if (node.type() == .funcDecl) {
        try declareFunc(c, parent, nodeId);
    } else if (node.type() == .funcDeclInit) {
        try declareFuncInit(c, parent, nodeId);
    } else {
        return error.Unexpected;
    }
}

pub fn declareMethod(c: *cy.Chunk, parent: *cy.Sym, nodeId: cy.NodeId, decl: FuncDecl) !*cy.Func {
    const node = c.ast.node(nodeId);
    if (node.data.func.bodyHead != cy.NullNode) {
        return c.declareUserFunc(parent, decl.name, decl.funcSigId, nodeId, true);
    }

    // No initializer. Check if #host func.
    const modifierId = c.ast.node(node.data.func.header).head.data.funcHeader.modHead;
    if (modifierId != cy.NullNode) {
        const modifier = c.ast.node(modifierId);
        if (modifier.data.dirModifier.type == .host) {
            return declareHostFunc(c, parent, nodeId, decl);
        }
    }
    return c.reportErrorFmt("`{}` does not have an initializer.", &.{v(decl.name)}, nodeId);
}

pub fn declareHostFunc(c: *cy.Chunk, parent: *cy.Sym, nodeId: cy.NodeId, decl: FuncDecl) !*cy.Func {
    const info = cc.FuncInfo{
        .mod = parent.toC(),
        .name = cc.toStr(decl.namePath),
        .funcSigId = decl.funcSigId,
        .idx = c.curHostFuncIdx,
    };
    c.curHostFuncIdx += 1;
    const funcLoader = c.funcLoader orelse {
        return c.reportErrorFmt("No function loader set for `{}`.", &.{v(decl.name)}, nodeId);
    };

    log.tracev("Invoke func loader for: {s}", .{decl.namePath});
    var res: cc.FuncResult = .{
        .ptr = null,
    };
    if (!funcLoader(@ptrCast(c.compiler.vm), info, @ptrCast(&res))) {
        return c.reportErrorFmt("Host func `{}` failed to load.", &.{v(decl.name)}, nodeId);
    }

    return try c.declareHostFunc(decl.parent, decl.name, decl.funcSigId, nodeId, @ptrCast(@alignCast(res.ptr)), decl.isMethod);
}

/// Declares a bytecode function in a given module.
pub fn declareFunc(c: *cy.Chunk, parent: *Sym, nodeId: cy.NodeId) !*cy.Func {
    const decl = try resolveFuncDecl(c, parent, nodeId);
    if (!decl.isMethod) {
        return try c.declareUserFunc(decl.parent, decl.name, decl.funcSigId, nodeId, false);
    } else {
        return try declareMethod(c, decl.parent, nodeId, decl);
    }
}

pub fn methodDecl(c: *cy.Chunk, func: *cy.Func) !void {
    const parent = func.parent;

    c.curSelfSym = parent;
    defer c.curSelfSym = null;

    // Object method.
    const blockId = try pushFuncProc(c, func);
    c.semaProcs.items[blockId].isMethodBlock = true;

    try pushMethodParamVars(c, parent.getStaticType().?, func);
    const funcN = c.ast.node(func.declId);
    try semaStmts(c, funcN.data.func.bodyHead);
    try popFuncBlock(c);
}

pub fn funcDecl(c: *cy.Chunk, func: *cy.Func) !void {
    const node = c.ast.node(func.declId);

    _ = try pushFuncProc(c, func);
    try appendFuncParamVars(c, func);
    try semaStmts(c, node.data.func.bodyHead);

    try popFuncBlock(c);
}

pub fn declareVar(c: *cy.Chunk, nodeId: cy.NodeId) !*Sym {
    const node = c.ast.node(nodeId);

    const varSpec = c.ast.node(node.data.staticDecl.varSpec);
    if (node.data.staticDecl.right == cy.NullNode) {
        // No initializer. Check if #host var.
        const modifierId = varSpec.head.data.varSpec.modHead;
        if (modifierId != cy.NullNode) {
            const modifier = c.ast.node(modifierId);
            if (modifier.data.dirModifier.type == .host) {
                return try declareHostVar(c, nodeId);
            }
        }
        const name = c.ast.getNamePathInfoById(varSpec.data.varSpec.name).namePath;
        return c.reportErrorFmt("`{}` does not have an initializer.", &.{v(name)}, nodeId);
    } else {
        // var type.
        const typeId = try resolveTypeSpecNode(c, varSpec.data.varSpec.typeSpec);

        const decl = try resolveLocalDeclNamePath(c, varSpec.data.varSpec.name);
        return @ptrCast(try c.declareUserVar(decl.parent, decl.name, nodeId, typeId));
    }
}

fn declareHostVar(c: *cy.Chunk, nodeId: cy.NodeId) !*Sym {
    const node = c.ast.node(nodeId);
    const varSpec = c.ast.node(node.data.staticDecl.varSpec);
    const decl = try resolveLocalDeclNamePath(c, varSpec.data.varSpec.name);

    const info = cc.VarInfo{
        .mod = c.sym.sym().toC(),
        .name = cc.toStr(decl.namePath),
        .idx = c.curHostVarIdx,
    };
    c.curHostVarIdx += 1;
    const varLoader = c.varLoader orelse {
        return c.reportErrorFmt("No var loader set for `{}`.", &.{v(decl.namePath)}, nodeId);
    };
    log.tracev("Invoke var loader for: {s}", .{decl.namePath});
    var out: cy.Value = cy.Value.initInt(0);
    if (!varLoader(@ptrCast(c.compiler.vm), info, @ptrCast(&out))) {
        return c.reportErrorFmt("Host var `{}` failed to load.", &.{v(decl.namePath)}, nodeId);
    }
    // var type.
    const typeId = try resolveTypeSpecNode(c, varSpec.data.varSpec.typeSpec);
    // c.ast.node(node.data.staticDecl.varSpec).next = typeId;

    const outTypeId = out.getTypeId();
    if (!cy.types.isTypeSymCompat(c.compiler, outTypeId, typeId)) {
        const expTypeName = c.sema.getTypeBaseName(typeId);
        const actTypeName = c.sema.getTypeBaseName(outTypeId);
        return c.reportErrorFmt("Host var `{}` expects type {}, got: {}.", &.{v(decl.namePath), v(expTypeName), v(actTypeName)}, nodeId);
    }

    return @ptrCast(try c.declareHostVar(decl.parent, decl.name, nodeId, typeId, out));
}

fn declareParam(c: *cy.Chunk, paramId: cy.NodeId, isSelf: bool, paramIdx: u32, declT: TypeId) !void {
    var name: []const u8 = undefined;
    if (isSelf) {
        name = "self";
    } else {
        const param = c.ast.node(paramId);
        name = c.ast.nodeStringById(param.data.funcParam.name);
    }

    const proc = c.proc();
    if (!isSelf) {
        if (proc.nameToVar.get(name)) |varInfo| {
            if (varInfo.blockId == c.semaBlocks.items.len - 1) {
                return c.reportErrorFmt("Function param `{}` is already declared.", &.{v(name)}, paramId);
            }
        }
    }

    const id = try pushLocalVar(c, .local, name, declT, false);
    var svar = &c.varStack.items[id];
    svar.inner = .{
        .local = .{
            .id = @intCast(paramIdx),
            .isParam = true,
            .isParamCopied = false,
            .hasInit = false,
            .lifted = false,
            .declIrStart = cy.NullId,
            .hidden = false,
        },
    };
    proc.numParams += 1;

    proc.curNumLocals += 1;
}

fn declareLocalName(c: *cy.Chunk, name: []const u8, declType: TypeId, hasInit: bool, nodeId: cy.NodeId) !LocalVarId {
    const proc = c.proc();
    if (proc.nameToVar.get(name)) |varInfo| {
        if (varInfo.blockId == c.semaBlocks.items.len - 1) {
            const svar = &c.varStack.items[varInfo.varId];
            if (svar.isParentLocalAlias()) {
                return c.reportErrorFmt("`{}` already references a parent local variable.", &.{v(name)}, nodeId);
            } else if (svar.type == .staticAlias) {
                return c.reportErrorFmt("`{}` already references a static variable.", &.{v(name)}, nodeId);
            } else {
                return c.reportErrorFmt("Variable `{}` is already declared in the block.", &.{v(name)}, nodeId);
            }
        } else {
            // Create shadow entry for restoring the prev var.
            try c.varShadowStack.append(c.alloc, .{
                .namePtr = name.ptr,
                .nameLen = @intCast(name.len),
                .varId = varInfo.varId,
                .blockId = varInfo.blockId,
            });
        }
    }

    return try declareLocalName2(c, name, declType, false, hasInit, nodeId);
}

fn declareLocalName2(c: *cy.Chunk, name: []const u8, declType: TypeId, hidden: bool, hasInit: bool, nodeId: cy.NodeId) !LocalVarId {
    const id = try pushLocalVar(c, .local, name, declType, hidden);
    var svar = &c.varStack.items[id];

    const proc = c.proc();
    const irId = proc.curNumLocals;

    var irIdx: u32 = undefined;
    if (hasInit) {
        irIdx = try c.ir.pushStmt(c.alloc, .declareLocalInit, nodeId, .{
            .namePtr = name.ptr,
            .nameLen = @as(u16, @intCast(name.len)),
            .declType = declType,
            .id = irId,
            .lifted = false,
            .init = cy.NullId,
            .initType = undefined,
            .zeroMem = false,
        });
    } else {
        irIdx = try c.ir.pushStmt(c.alloc, .declareLocal, nodeId, .{
            .namePtr = name.ptr,
            .nameLen = @as(u16, @intCast(name.len)),
            .declType = declType,
            .id = irId,
            .lifted = false,
        });
    }

    svar.inner = .{ .local = .{
        .id = irId,
        .isParam = false,
        .isParamCopied = false,
        .hasInit = hasInit,
        .lifted = false,
        .hidden = hidden,
        .declIrStart = @intCast(irIdx),
    }};

    const b = c.block();
    b.numLocals += 1;

    proc.curNumLocals += 1;
    return id;
}

fn declareLocal(c: *cy.Chunk, identId: cy.NodeId, declType: TypeId, hasInit: bool) !LocalVarId {
    const ident = c.ast.node(identId);
    const name = c.ast.nodeString(ident);
    return declareLocalName(c, name, declType, hasInit, identId);
}

fn localDecl(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.ast.node(nodeId);
    const varSpec = c.ast.node(node.data.localDecl.varSpec);

    var typeId: cy.TypeId = undefined;
    var inferType = false;
    if (node.data.localDecl.typed) {
        if (varSpec.data.varSpec.typeSpec == cy.NullNode) {
            inferType = true;
            typeId = bt.Any;
        } else {
            typeId = try resolveTypeSpecNode(c, varSpec.data.varSpec.typeSpec);
        }
    } else {
        typeId = bt.Dynamic;
    }

    const varId = try declareLocal(c, varSpec.data.varSpec.name, typeId, true);
    var svar = &c.varStack.items[varId];
    const irIdx = svar.inner.local.declIrStart;

    const maxLocalsBeforeInit = c.proc().curNumLocals;

    // Infer rhs type and enforce constraint.
    const right = try c.semaExpr(node.data.localDecl.right, .{
        .target_t = typeId,
        .req_target_t = !inferType,
    });

    var data = c.ir.getStmtDataPtr(irIdx, .declareLocalInit);
    data.init = right.irIdx;
    data.initType = right.type;
    if (maxLocalsBeforeInit < c.proc().maxLocals) {
        // Initializer must have a declaration (in a block expr)
        // since the number of locals increased.
        // Local's memory must be zeroed.
        data.zeroMem = true;
    }

    svar = &c.varStack.items[varId];
    if (inferType) {
        var declType = right.type.toDeclType();
        svar.declT = declType;
        svar.vtype = right.type;
        // Patch IR.
        data.declType = declType;
    } else if (typeId == bt.Dynamic) {
        // Update recent static type.
        svar.vtype.id = right.type.id;
    }

    try c.assignedVarStack.append(c.alloc, varId);
}

pub fn staticDecl(c: *cy.Chunk, sym: *Sym, nodeId: cy.NodeId) !void {
    const node = c.ast.node(nodeId);

    c.curInitingSym = sym;
    c.curInitingSymDeps.clearRetainingCapacity();
    defer c.curInitingSym = null;
    const depStart = c.symInitDeps.items.len;

    const irIdx = try c.ir.pushEmptyStmt(c.alloc, .setVarSym, nodeId);
    const recLoc = try c.ir.pushExpr(.varSym, c.alloc, bt.Void, nodeId, .{ .sym = sym });
    const symType = CompactType.init((try sym.getValueType()).?);
    const right = try semaExprCType(c, node.data.staticDecl.right, symType);
    c.ir.setStmtData(irIdx, .setVarSym, .{ .generic = .{
        .left_t = symType,
        .right_t = right.type,
        .left = recLoc,
        .right = right.irIdx,
    }});

    try c.symInitInfos.put(c.alloc, sym, .{
        .depStart = @intCast(depStart),
        .depEnd = @intCast(c.symInitDeps.items.len),
        .irStart = @intCast(irIdx),
        .irEnd = @intCast(c.ir.buf.items.len),
    });
}

const SemaExprOptions = struct {
    target_t: TypeId = undefined,
    req_target_t: bool = false,
};

fn semaExprCType(c: *cy.Chunk, nodeId: cy.NodeId, ctype: CompactType) !ExprResult {
    return try c.semaExpr(nodeId, .{
        .target_t = ctype.id,
        .req_target_t = !ctype.dynamic,
    });
}

const ExprResultType = enum(u8) {
    value,
    sym,
    varSym,
    func,
    local,
    fieldDyn,
    field,
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

    fn initInheritDyn(loc: u32, ctype: CompactType, type_id: TypeId) ExprResult {
        var new_ctype = ctype;
        new_ctype.id = @intCast(type_id);
        return .{
            .resType = .value,
            .type = new_ctype,
            .data = undefined,
            .irIdx = loc,
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
    /// Whether to fail if incompatible with type cstr.
    reqTypeCstr: bool,

    nodeId: cy.NodeId,
    target_t: TypeId,

    fn init(nodeId: cy.NodeId) Expr {
        return .{
            .nodeId = nodeId,
            .target_t = bt.Any,
            .reqTypeCstr = false,
        };
    }

    fn hasTargetType(self: Expr) bool {
        return self.target_t != bt.Any;
    }

    fn getRetCstr(self: Expr) ReturnCstr {
        if (self.target_t == bt.Void) {
            return .any;
        } else {
            return .not_void;
        }
    }
};

fn requireFuncSym(c: *cy.Chunk, sym: *Sym, node: cy.NodeId) !*cy.sym.FuncSym {
    if (sym.type != .func) {
        return c.reportErrorFmt("Expected `{}` to be a function symbol.", &.{v(sym.name())}, node);
    }
    return sym.cast(.func);
}

// Invoke a type's sym as the callee.
fn callTypeSym(c: *cy.Chunk, preIdx: u32, sym: *Sym, numArgs: u8, symNodeId: cy.NodeId, argHead: cy.NodeId, typeId: cy.TypeId, ret_cstr: ReturnCstr) !ExprResult {
    _ = typeId;
    const typeCallSym = try c.findDistinctSym(sym, "$call", cy.NullNode, true);
    const funcSym = try requireFuncSym(c, typeCallSym, symNodeId);
    return c.semaCallFuncSym(preIdx, funcSym, argHead, numArgs, ret_cstr, symNodeId);
}

fn callSym(c: *cy.Chunk, preIdx: u32, sym: *Sym, numArgs: u8, symNodeId: cy.NodeId, argHead: cy.NodeId, ret_cstr: ReturnCstr) !ExprResult {
    try referenceSym(c, sym, symNodeId);
    switch (sym.type) {
        .func => {
            const funcSym = sym.cast(.func);
            return c.semaCallFuncSym(preIdx, funcSym, argHead, numArgs, ret_cstr, symNodeId);
        },
        .bool_t => {
            const type_id = sym.cast(.bool_t).type;
            return callTypeSym(c, preIdx, sym, numArgs, symNodeId, argHead, type_id, ret_cstr);
        }, 
        .int_t => {
            const type_id = sym.cast(.int_t).type;
            return callTypeSym(c, preIdx, sym, numArgs, symNodeId, argHead, type_id, ret_cstr);
        }, 
        .float_t => {
            const type_id = sym.cast(.float_t).type;
            return callTypeSym(c, preIdx, sym, numArgs, symNodeId, argHead, type_id, ret_cstr);
        }, 
        .custom_object_t => {
            const type_id = sym.cast(.custom_object_t).type;
            return callTypeSym(c, preIdx, sym, numArgs, symNodeId, argHead, type_id, ret_cstr);
        },
        .struct_t => {
            const type_id = sym.cast(.struct_t).type;
            return callTypeSym(c, preIdx, sym, numArgs, symNodeId, argHead, type_id, ret_cstr);
        },
        .object_t => {
            const type_id = sym.cast(.object_t).type;
            return callTypeSym(c, preIdx, sym, numArgs, symNodeId, argHead, type_id, ret_cstr);
        },
        .userVar,
        .hostVar => {
            // preCall.
            const callee = try sema.symbol(c, sym, symNodeId, true);
            const args = try c.semaPushCallArgs(argHead, numArgs);
            return c.semaCallValue(preIdx, callee.irIdx, numArgs, args);
        },
        else => {
            // try pushCallArgs(c, node.data.callExpr.argHead, numArgs, true);
            log.tracev("{}", .{sym.type});
            return error.TODO;
        },
    }
}

pub fn symbol(c: *cy.Chunk, sym: *Sym, nodeId: cy.NodeId, comptime symAsValue: bool) !ExprResult {
    try referenceSym(c, sym, nodeId);
    if (symAsValue) {
        const typeId = (try sym.getValueType()) orelse {
            return c.reportErrorFmt("Can't use symbol `{}` as a value.", &.{v(sym.name())}, nodeId);
        };
        const ctype = CompactType.init(typeId);
        switch (sym.type) {
            .userVar,
            .hostVar => {
                const loc = try c.ir.pushExpr(.varSym, c.alloc, typeId, nodeId, .{ .sym = sym });
                return ExprResult.initCustom(loc, .varSym, ctype, .{ .varSym = sym });
            },
            .func => {
                // `symbol` being invoked suggests the func sym is not ambiguous.
                const funcSym = sym.cast(.func);
                const loc = try c.ir.pushExpr(.funcSym, c.alloc, typeId, nodeId, .{ .func = funcSym.first });
                return ExprResult.initCustom(loc, .func, ctype, .{ .func = funcSym.first });
            },
            .struct_t => {
                const objTypeId = sym.cast(.struct_t).type;
                const loc = try c.ir.pushExpr(.typeSym, c.alloc, typeId, nodeId, .{ .typeId = objTypeId });
                return ExprResult.init(loc, ctype);
            },
            .object_t => {
                const objTypeId = sym.cast(.object_t).type;
                const irIdx = try c.ir.pushExpr(.typeSym,c.alloc, typeId, nodeId, .{ .typeId = objTypeId });
                return ExprResult.init(irIdx, ctype);
            },
            .custom_object_t => {
                const type_id = sym.cast(.custom_object_t).type;
                const irIdx = try c.ir.pushExpr(.typeSym, c.alloc, typeId, nodeId, .{ .typeId = type_id });
                return ExprResult.init(irIdx, ctype);
            },
            .bool_t => {
                const type_id = sym.cast(.bool_t).type;
                const irIdx = try c.ir.pushExpr(.typeSym, c.alloc, typeId, nodeId, .{ .typeId = type_id });
                return ExprResult.init(irIdx, ctype);
            },
            .int_t => {
                const type_id = sym.cast(.int_t).type;
                const irIdx = try c.ir.pushExpr(.typeSym, c.alloc, typeId, nodeId, .{ .typeId = type_id });
                return ExprResult.init(irIdx, ctype);
            },
            .float_t => {
                const type_id = sym.cast(.float_t).type;
                const irIdx = try c.ir.pushExpr(.typeSym, c.alloc, typeId, nodeId, .{ .typeId = type_id });
                return ExprResult.init(irIdx, ctype);
            },
            .enumMember => {
                const mem = sym.cast(.enumMember);
                const irIdx = try c.ir.pushExpr(.enumMemberSym, c.alloc, typeId, nodeId, .{
                    .type = mem.type,
                    .val = @as(u8, @intCast(mem.val)),
                });
                return ExprResult.init(irIdx, ctype);
            },
            else => {
                return error.Unsupported;
            }
        }
    } else {
        var typeId: CompactType = undefined;
        if (sym.isType()) {
            typeId = CompactType.init(sym.getStaticType().?);
        } else {
            typeId = CompactType.init((try sym.getValueType()) orelse bt.Void);
        }
        return ExprResult.initCustom(cy.NullId, .sym, typeId, .{ .sym = sym });
    }
}

fn semaLocal(c: *cy.Chunk, id: LocalVarId, nodeId: cy.NodeId) !ExprResult {
    const svar = c.varStack.items[id];
    switch (svar.type) {
        .local => {
            const loc = try c.ir.pushExpr(.local, c.alloc, svar.vtype.id, nodeId, .{ .id = svar.inner.local.id });
            return ExprResult.initCustom(loc, .local, svar.vtype, .{ .local = id });
        },
        .objectMemberAlias => {
            const rec_t = c.varStack.items[c.proc().varStart].declT;
            const rec_loc = try c.ir.pushExpr(.local, c.alloc, rec_t, nodeId, .{ .id = 0 });
            const loc = try c.ir.pushExpr(.field, c.alloc, svar.vtype.id, nodeId, .{
                .idx = svar.inner.objectMemberAlias.fieldIdx,
                .rec = rec_loc,
                .numNestedFields = 0,
            });
            return ExprResult.initCustom(loc, .field, svar.vtype, undefined);
        },
        .parentObjectMemberAlias => {
            const rec_t = c.varStack.items[svar.inner.parentObjectMemberAlias.parentVarId].declT;
            const rec_loc = try c.ir.pushExpr(.captured, c.alloc, rec_t, nodeId, .{ .idx = svar.inner.parentObjectMemberAlias.selfCapturedIdx });
            const loc = try c.ir.pushExpr(.field, c.alloc, svar.vtype.id, nodeId, .{
                .idx = svar.inner.parentObjectMemberAlias.fieldIdx,
                .rec = rec_loc,
                .numNestedFields = 0,
            });
            return ExprResult.init(loc, svar.vtype);
        },
        .parentLocalAlias => {
            const loc = try c.ir.pushExpr(.captured, c.alloc, svar.vtype.id, nodeId, .{ .idx = svar.inner.parentLocalAlias.capturedIdx });
            return ExprResult.initCustom(loc, .capturedLocal, svar.vtype, undefined);
        },
        else => {
            return c.reportErrorFmt("Unsupported: {}", &.{v(svar.type)}, null);
        },
    }
}

fn semaIdent(c: *cy.Chunk, nodeId: cy.NodeId, comptime symAsValue: bool) !ExprResult {
    const node = c.ast.node(nodeId);
    const name = c.ast.nodeString(node);
    const res = try getOrLookupVar(c, name, true, nodeId);
    switch (res) {
        .local => |id| {
            return semaLocal(c, id, nodeId);
        },
        .static => |csymId| {
            return try sema.symbol(c, csymId, nodeId, symAsValue);
        },
    }
}

pub fn resolveLocalRootSym(c: *cy.Chunk, name: []const u8, nodeId: cy.NodeId, distinct: bool) !?*Sym {
    if (c.localSymMap.get(name)) |sym| {
        if (!distinct or sym.isDistinct()) {
            return sym;
        } else {
            return c.reportErrorFmt("`{}` is not a unique symbol.", &.{v(name)}, nodeId);
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
/// Returns the final resolved type sym.
pub fn resolveTypeSpecNode(c: *cy.Chunk, nodeId: cy.NodeId) anyerror!cy.TypeId {
    if (nodeId == cy.NullNode) {
        return bt.Dynamic;
    }
    const type_id = try resolveTypeExpr(c, nodeId);
    if (type_id == bt.Void) {
        return c.reportErrorFmt("`void` can not be used as common type specifier.", &.{}, nodeId);
    }
    return type_id;
}

pub fn resolveReturnTypeSpecNode(c: *cy.Chunk, nodeId: cy.NodeId) anyerror!cy.TypeId {
    if (nodeId == cy.NullNode) {
        return bt.Dynamic;
    }
    return resolveTypeExpr(c, nodeId);
}

pub fn resolveLocalNamePathSym(c: *cy.Chunk, head: cy.NodeId, end: cy.NodeId) anyerror!*Sym {
    var nodeId = head;
    var node = c.ast.node(nodeId);
    var name = c.ast.nodeString(node);

    var sym = (try resolveLocalRootSym(c, name, nodeId, true)) orelse {
        return c.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, nodeId);
    };

    nodeId = node.next();
    while (nodeId != end) {
        node = c.ast.node(nodeId);
        name = c.ast.nodeString(node);
        sym = try c.findDistinctSym(sym, name, nodeId, true);
        nodeId = node.next();
    }
    return sym;
}

fn resolveSymType(c: *cy.Chunk, sym: *cy.Sym, node_id: cy.NodeId) !cy.TypeId {
    return sym.getStaticType() orelse {
        switch (sym.type) {
            .typeTemplate => {
                return c.reportErrorFmt("Expected a type symbol. `{}` is a type template and must be expanded to a type first.", &.{v(sym.name())}, node_id);
            },
            else => {
                return c.reportErrorFmt("`{}` is not a type symbol.", &.{v(sym.name())}, node_id);
            }
        }
    };
}

fn resolveSym(c: *cy.Chunk, node_id: cy.NodeId) !*cy.Sym {
    const node = c.ast.node(node_id);
    switch (node.type()) {
        .ident => {
            const name = c.ast.nodeString(node);
            return (try resolveLocalRootSym(c, name, node_id, true)) orelse {
                return c.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, node_id);
            };
        },
        else => {
            return c.reportErrorFmt("Expected symbol. Found `{}`.", &.{v(node.type())}, node_id);
        },
    }
}

fn resolveTypeExpr(c: *cy.Chunk, exprId: cy.NodeId) !cy.TypeId {
    const expr = c.ast.node(exprId);
    switch (expr.type()) {
        .objectDecl => {
            // Unnamed object.
            const header = c.ast.node(expr.data.objectDecl.header);
            const symId = header.data.objectHeader.name;
            const sym = c.sym.getMod().chunk.syms.items[symId];
            return resolveSymType(c, sym, exprId);
        },
        .void => {
            return bt.Void;
        },
        .ident => {
            const name = c.ast.nodeString(expr);
            const sym = (try resolveLocalRootSym(c, name, exprId, true)) orelse {
                return c.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, exprId);
            };
            return resolveSymType(c, sym, exprId);
        },
        .accessExpr => {
            const parent = try resolveSym(c, expr.data.accessExpr.left);
            const right = c.ast.node(expr.data.accessExpr.right);
            if (right.type() != .ident) {
                return c.reportErrorFmt("Expected identifier.", &.{}, expr.data.accessExpr.right);
            }
            const name = c.ast.nodeString(right);
            const sym = try c.findDistinctSym(parent, name, expr.data.accessExpr.right, true);
            return resolveSymType(c, sym, exprId);
        },
        .comptimeExpr => {
            if (expr.data.comptimeExpr.patchIdx != cy.NullId) {
                const patchNode = c.patchTemplateNodes[expr.data.comptimeExpr.patchIdx];
                return resolveTypeExpr(c, patchNode);
            } else {
                return c.reportErrorFmt("Unexpected compile-time expression.", &.{}, exprId);
            }
        },
        .semaSym => {
            return resolveSymType(c, expr.data.semaSym.sym, exprId);
        },
        .callExpr => {
            const sym = try cte.expandTemplateOnCallExpr(c, exprId);
            return resolveSymType(c, sym, exprId);
        },
        .expandOpt => {
            const sym = try cte.expandTemplateOnCallArgs(c, c.sema.option_tmpl, expr.data.expandOpt.param, exprId);
            return resolveSymType(c, sym, exprId);
        },
        else => {
            return c.reportErrorFmt("Unsupported type expr: `{}`", &.{v(expr.type())}, exprId);
        }
    }
}

const FuncDecl = struct {
    namePath: []const u8,
    name: []const u8,
    parent: *Sym,
    funcSigId: FuncSigId,
    isMethod: bool,
};

pub fn resolveImplicitMethodDecl(c: *cy.Chunk, parent: *Sym, nodeId: cy.NodeId) !FuncDecl {
    const func = c.ast.node(nodeId);
    const header = c.ast.node(func.data.func.header);
    const name = c.ast.nodeStringById(header.data.funcHeader.name);

    // Get params, build func signature.
    const start = c.typeStack.items.len;
    defer c.typeStack.items.len = start;

    // First param is always `self`.
    try c.typeStack.append(c.alloc, parent.getStaticType().?);

    var curParamId = header.data.funcHeader.paramHead;
    while (curParamId != cy.NullNode) {
        const param = c.ast.node(curParamId);
        const paramName = c.ast.nodeStringById(param.data.funcParam.name);
        if (std.mem.eql(u8, "self", paramName)) {
            return c.reportErrorFmt("`self` param is not allowed in an implicit method declaration.", &.{}, curParamId);
        }
        const typeId = try resolveTypeSpecNode(c, param.data.funcParam.typeSpec);
        try c.typeStack.append(c.alloc, typeId);
        curParamId = param.next();
    }

    // Get return type.
    const retType = try resolveReturnTypeSpecNode(c, header.funcHeader_ret());

    const funcSigId = try c.sema.ensureFuncSig(c.typeStack.items[start..], retType);
    return FuncDecl{
        .namePath = name,
        .name = name,
        .parent = parent,
        .funcSigId = funcSigId,
        .isMethod = true,
    };
}

fn resolveTemplateSig(c: *cy.Chunk, paramHead: cy.NodeId, numParams: u32, outSigId: *FuncSigId) ![]const cy.sym.TemplateParam {
    const typeStart = c.typeStack.items.len;
    defer c.typeStack.items.len = typeStart;

    const params = try c.alloc.alloc(cy.sym.TemplateParam, numParams);

    var param = paramHead;
    var i: u32 = 0;
    while (param != cy.NullNode) {
        const param_n = c.ast.node(param);
        if (param_n.data.funcParam.typeSpec == cy.NullNode) {
            return c.reportErrorFmt("Expected param type specifier.", &.{}, param);
        }
        const typeId = try resolveTypeSpecNode(c, param_n.data.funcParam.typeSpec);
        try c.typeStack.append(c.alloc, typeId);
        params[i] = .{
            .name = c.ast.nodeStringById(param_n.data.funcParam.name),
            .type = typeId,
        };
        param = param_n.next();
        i += 1;
    }

    const retType = bt.Type;
    outSigId.* = try c.sema.ensureFuncSig(c.typeStack.items[typeStart..], retType);
    return params;
}

fn resolveFuncDecl(c: *cy.Chunk, parent: *Sym, nodeId: cy.NodeId) !FuncDecl {
    const func = c.ast.node(nodeId);
    const header = c.ast.node(func.data.func.header);

    // Get params, build func signature.
    const start = c.typeStack.items.len;
    defer c.typeStack.items.len = start;

    var res: FuncDecl = undefined;
    try updateFuncDeclNamePath(c, &res, parent, header.data.funcHeader.name);

    const sig_t = func.data.func.sig_t;
    if (sig_t == .infer) {
        return error.Unexpected;
    }
    var isMethod = false;
    var curParamId = header.data.funcHeader.paramHead;
    while (curParamId != cy.NullNode) {
        const param = c.ast.node(curParamId);
        const paramName = c.ast.nodeStringById(param.data.funcParam.name);
        if (std.mem.eql(u8, paramName, "self")) {
            isMethod = true;
            try c.typeStack.append(c.alloc, res.parent.getStaticType().?);
        } else {
            if (sig_t == .func) {
                if (param.data.funcParam.typeSpec == cy.NullNode) {
                    return c.reportError("Expected type specifier.", curParamId);
                }
            } else {
                if (param.data.funcParam.typeSpec != cy.NullNode) {
                    return c.reportError("Type specifier not allowed in `let` declaration. Declare typed functions with `func`.", curParamId);
                }
            }
            const typeId = try resolveTypeSpecNode(c, param.data.funcParam.typeSpec);
            try c.typeStack.append(c.alloc, typeId);
        }
        curParamId = param.next();
    }

    // Get return type.
    const retType = try resolveReturnTypeSpecNode(c, header.funcHeader_ret());

    res.funcSigId = try c.sema.ensureFuncSig(c.typeStack.items[start..], retType);
    res.isMethod = isMethod;
    return res;
}

fn resolveLambdaDecl(c: *cy.Chunk, parent: *Sym, nodeId: cy.NodeId) !FuncDecl {
    const func = c.ast.node(nodeId);
    const header = c.ast.node(func.data.func.header);

    // Get params, build func signature.
    const start = c.typeStack.items.len;
    defer c.typeStack.items.len = start;

    var res: FuncDecl = undefined;
    try updateFuncDeclNamePath(c, &res, parent, header.data.funcHeader.name);

    const sig_t = func.data.func.sig_t;
    var curParamId = header.data.funcHeader.paramHead;
    while (curParamId != cy.NullNode) {
        const param = c.ast.node(curParamId);
        const paramName = c.ast.nodeStringById(param.data.funcParam.name);
        if (std.mem.eql(u8, paramName, "self")) {
            return c.reportError("`self` is a reserved parameter for methods.", curParamId);
        }
        if (sig_t == .func) {
            if (param.data.funcParam.typeSpec == cy.NullNode) {
                return c.reportError("Expected type specifier.", curParamId);
            }
        } else {
            if (param.data.funcParam.typeSpec != cy.NullNode) {
                return c.reportError("Type specifier not allowed. Declare typed lambdas with `func`.", curParamId);
            }
        }
        const typeId = try resolveTypeSpecNode(c, param.data.funcParam.typeSpec);
        try c.typeStack.append(c.alloc, typeId);

        curParamId = param.next();
    }

    // Get return type.
    const retType = try resolveReturnTypeSpecNode(c, header.funcHeader_ret());

    res.funcSigId = try c.sema.ensureFuncSig(c.typeStack.items[start..], retType);
    res.isMethod = false;
    return res;
}

const DeclNamePathResult = struct {
    name: []const u8,
    namePath: []const u8,
    parent: *cy.Sym,
};

fn resolveLocalDeclNamePath(c: *cy.Chunk, nameId: cy.NodeId) !DeclNamePathResult {
    const nameN = c.ast.node(nameId);
    if (nameN.next() == cy.NullNode) {
        const name = c.ast.nodeString(nameN);
        return .{
            .name = name,
            .namePath = name,
            .parent = @ptrCast(c.sym),
        };
    } else {
        const lastId = c.ast.getLastNameNode(nameN.next());
        const last = c.ast.node(lastId);
        const name = c.ast.nodeString(last);

        var end = last.srcPos + name.len;
        if (last.type() == .raw_string_lit) {
            end += 1;
        }
        const namePath = c.ast.src[nameN.srcPos..end];
        const parent = try resolveLocalNamePathSym(c, nameId, lastId);
        return .{
            .name = name,
            .namePath = namePath,
            .parent = parent,
        };
    }
}

fn updateFuncDeclNamePath(c: *cy.Chunk, decl: *FuncDecl, parent: *Sym, nameId: cy.NodeId) !void {
    if (nameId != cy.NullNode) {
        const nameN = c.ast.node(nameId);
        if (nameN.next() == cy.NullNode) {
            decl.name = c.ast.nodeString(nameN);
            decl.namePath = decl.name;
            decl.parent = parent;
        } else {
            const info = c.ast.getNamePathInfo(nameN, nameId);
            decl.name = info.lastName;
            decl.namePath = info.namePath;
            const explicitParent = try resolveLocalNamePathSym(c, nameId, info.lastId);
            decl.parent = explicitParent;
        }
    } else {
        decl.name = "";
        decl.namePath = "";
        decl.parent = parent;
    }
}

pub fn popProc(self: *cy.Chunk) !cy.ir.StmtBlock {
    const stmtBlock = try popBlock(self);
    const proc = self.proc();
    proc.deinit(self.alloc);
    self.semaProcs.items.len -= 1;
    self.varStack.items.len = proc.varStart;
    return stmtBlock;
}

pub fn pushLambdaProc(c: *cy.Chunk, func: *cy.Func) !ProcId {
    const idx = try c.ir.pushEmptyExpr(.lambda, c.alloc, ir.ExprType.init(bt.Lambda), func.declId);
    // Reserve param info.
    _ = try c.ir.pushEmptyArray(c.alloc, ir.FuncParam, func.numParams);

    const id = try pushProc(c, func);
    c.proc().irStart = idx;
    return id;
}

pub fn pushFuncProc(c: *cy.Chunk, func: *cy.Func) !ProcId {
    const idx = try c.ir.pushEmptyStmt(c.alloc, .funcBlock, func.declId);
    // Reserve param info.
    _ = try c.ir.pushEmptyArray(c.alloc, ir.FuncParam, func.numParams);

    const id = try pushProc(c, func);
    c.proc().irStart = idx;
    return id;
}

pub fn semaMainBlock(compiler: *cy.Compiler, mainc: *cy.Chunk) !u32 {
    const irIdx = try mainc.ir.pushEmptyStmt(compiler.alloc, .mainBlock, mainc.parserAstRootId);

    const id = try pushProc(mainc, null);
    mainc.mainSemaProcId = id;

    // Emit IR for initializers. DFS order. Stop at circular dependency.
    // TODO: Allow circular dependency between chunks but not symbols.
    for (compiler.chunks.items) |c| {
        if (c.hasStaticInit and !c.initializerVisited) {
            try visitChunkInit(compiler, c, );
        }
    }

    // Main.
    const root = mainc.ast.node(mainc.parserAstRootId);
    try semaStmts(mainc, root.data.root.bodyHead);

    const proc = mainc.proc();

    // Pop block first to obtain the max locals.
    const stmtBlock = try popProc(mainc);
    log.tracev("pop main block: {}", .{proc.maxLocals});

    mainc.ir.setStmtData(irIdx, .mainBlock, .{
        .maxLocals = proc.maxLocals,
        .bodyHead = stmtBlock.first,
    });
    return irIdx;
}

fn visitChunkInit(self: *cy.Compiler, c: *cy.Chunk) !void {
    c.initializerVisiting = true;
    var iter = c.symInitChunkDeps.keyIterator();

    while (iter.next()) |key| {
        const depChunk = key.*;
        if (depChunk.initializerVisited) {
            continue;
        }
        if (depChunk.initializerVisiting) {
            return c.reportErrorFmt("Referencing `{}` created a circular module dependency.", &.{v(c.srcUri)}, cy.NullNode);
        }
        try visitChunkInit(self, depChunk);
    }

    // Once all deps are visited, emit IR.
    const func = c.sym.getMod().getSym("$init").?.cast(.func).first;
    const exprLoc = try self.main_chunk.ir.pushExpr(.preCallFuncSym, c.alloc, bt.Void, cy.NullNode, .{ .callFuncSym = .{
        .func = func, .numArgs = 0, .args = 0,
    }});
    _ = try self.main_chunk.ir.pushStmt(c.alloc, .exprStmt, cy.NullNode, .{
        .expr = exprLoc,
        .isBlockResult = false,
    });

    c.initializerVisiting = false;
    c.initializerVisited = true;
}

pub fn pushProc(self: *cy.Chunk, func: ?*cy.Func) !ProcId {
    var isStaticFuncBlock = false;
    var nodeId: cy.NodeId = undefined;
    if (func != null) {
        isStaticFuncBlock = func.?.isStatic();
        nodeId = func.?.declId;
    } else {
        nodeId = self.parserAstRootId;
    }

    var new = Proc.init(nodeId, func, @intCast(self.semaBlocks.items.len), isStaticFuncBlock, @intCast(self.varStack.items.len));
    const idx = self.semaProcs.items.len;
    try self.semaProcs.append(self.alloc, new);

    try pushBlock(self, nodeId);
    return @intCast(idx);
}

fn preLoop(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const proc = c.proc();
    const b = c.block();

    // Scan for dynamic vars and prepare them for entering loop block.
    const start = c.preLoopVarSaveStack.items.len;
    const vars = c.varStack.items[proc.varStart..];
    for (vars, 0..) |*svar, i| {
        if (svar.type == .local) {
            if (svar.isDynamic()) {
                if (svar.vtype.id != bt.Any) {
                    // Dynamic vars enter the loop with a recent type of `any`
                    // since the rest of the loop hasn't been seen.
                    try c.preLoopVarSaveStack.append(c.alloc, .{
                        .vtype = svar.vtype,
                        .varId = @intCast(proc.varStart + i),
                    });
                    svar.vtype.id = bt.Any;
                    _ = try c.ir.pushStmt(c.alloc, .setLocalType, nodeId, .{ .local = svar.inner.local.id, .type = svar.vtype });
                }
            }
        }
    }
    b.preLoopVarSaveStart = @intCast(start);
}

fn popBlock(c: *cy.Chunk) !cy.ir.StmtBlock {
    const proc = c.proc();
    const b = c.block();

    // Update max locals and unwind.
    if (proc.curNumLocals > proc.maxLocals) {
        proc.maxLocals = proc.curNumLocals;
    }
    proc.curNumLocals -= b.numLocals;

    const curAssignedVars = c.assignedVarStack.items[b.assignedVarStart..];
    c.assignedVarStack.items.len = b.assignedVarStart;

    if (proc.blockDepth > 1) {
        const pblock = c.semaBlocks.items[c.semaBlocks.items.len-2];

        // Merge types to parent sub block.
        for (curAssignedVars) |varId| {
            const svar = &c.varStack.items[varId];
            // log.tracev("merging {s}", .{self.getVarName(varId)});
            if (b.prevVarTypes.get(varId)) |prevt| {
                // Merge recent static type.
                if (svar.vtype.id != prevt.id) {
                    svar.vtype.id = bt.Any;
                    if (svar.type == .local) {
                        _ = try c.ir.pushStmt(c.alloc, .setLocalType, b.nodeId, .{
                            .local = svar.inner.local.id, .type = svar.vtype,
                        });
                    }

                    // Previous sub block hasn't recorded the var assignment.
                    if (!pblock.prevVarTypes.contains(varId)) {
                        try c.assignedVarStack.append(c.alloc, varId);
                    }
                }
            }
        }
    }
    b.prevVarTypes.deinit(c.alloc);

    // Restore `nameToVar` to previous sub-block state.
    if (proc.blockDepth > 1) {
        // Remove dead vars.
        const varDecls = c.varStack.items[b.varStart..];
        for (varDecls) |decl| {
            if (decl.type == .local and decl.inner.local.hidden) {
                continue;
            }
            const name = decl.namePtr[0..decl.nameLen];
            _ = proc.nameToVar.remove(name);
        }
        c.varStack.items.len = b.varStart;

        // Restore shadowed vars.
        const varShadows = c.varShadowStack.items[b.varShadowStart..];
        for (varShadows) |shadow| {
            const name = shadow.namePtr[0..shadow.nameLen];
            try proc.nameToVar.putNoClobber(c.alloc, name, .{
                .varId = shadow.varId,
                .blockId = shadow.blockId,
            });
        }
        c.varShadowStack.items.len = b.varShadowStart;
    }

    proc.blockDepth -= 1;
    c.semaBlocks.items.len -= 1;

    return c.ir.popStmtBlock();
}

fn popLoopBlock(c: *cy.Chunk) !cy.ir.StmtBlock {
    const pblock = c.block();
    const stmtBlock = try popBlock(c);

    const b = c.block();
    const varSaves = c.preLoopVarSaveStack.items[b.preLoopVarSaveStart..];
    for (varSaves) |save| {
        var svar = &c.varStack.items[save.varId];
        if (svar.dynamicLastMutBlockId <= c.semaBlocks.items.len - 1) {
            // Unused inside loop block. Restore type.
            svar.vtype = save.vtype;
            _ = try c.ir.pushStmt(c.alloc, .setLocalType, pblock.nodeId, .{ .local = svar.inner.local.id, .type = svar.vtype });
        }
    }
    c.preLoopVarSaveStack.items.len = b.preLoopVarSaveStart;
    return stmtBlock;
}

fn pushBlock(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    try c.ir.pushStmtBlock(c.alloc);
    c.proc().blockDepth += 1;
    const new = Block.init(
        nodeId,
        c.assignedVarStack.items.len,
        c.varStack.items.len,
        c.varShadowStack.items.len,
    );
    try c.semaBlocks.append(c.alloc, new);
}

fn pushMethodParamVars(c: *cy.Chunk, objectT: TypeId, func: *const cy.Func) !void {
    const curNodeId = c.curNodeId;
    defer c.curNodeId = curNodeId;

    const rFuncSig = c.compiler.sema.funcSigs.items[func.funcSigId];
    const params = rFuncSig.params();

    const funcN = c.ast.node(func.declId);
    const header = c.ast.node(funcN.data.func.header);
    var curNode = header.data.funcHeader.paramHead;
    if (curNode != cy.NullNode) {
        var param = c.ast.node(curNode);
        const name = c.ast.nodeStringById(param.data.funcParam.name);
        if (std.mem.eql(u8, name, "self")) {
            try declareParam(c, curNode, false, 0, objectT);

            if (param.next() != cy.NullNode) {
                curNode = param.next();
                for (params[1..], 1..) |paramT, idx| {
                    try declareParam(c, curNode, false, @intCast(idx), paramT);

                    param = c.ast.node(curNode);
                    curNode = param.next();
                }
            }
        } else {
            // Implicit `self` param.
            try declareParam(c, cy.NullNode, true, 0, objectT);
            // First param.
            try declareParam(c, curNode, false, 1, params[1]);

            if (param.next() != cy.NullNode) {
                curNode = param.next();
                for (params[2..], 2..) |paramT, idx| {
                    try declareParam(c, curNode, false, @intCast(idx), paramT);

                    param = c.ast.node(curNode);
                    curNode = param.next();
                }
            }
        }
    } else {
        // Implicit `self` param.
        try declareParam(c, cy.NullNode, true, 0, objectT);
    }
}

fn appendFuncParamVars(c: *cy.Chunk, func: *const cy.Func) !void {
    if (func.numParams > 0) {
        const rFuncSig = c.compiler.sema.funcSigs.items[func.funcSigId];
        const params = rFuncSig.params();
        const funcN = c.ast.node(func.declId);
        const header = c.ast.node(funcN.data.func.header);
        var curNode = header.data.funcHeader.paramHead;
        for (params, 0..) |paramT, idx| {
            const param = c.ast.node(curNode);
            try declareParam(c, curNode, false, @intCast(idx), paramT);
            curNode = param.next();
        }
    }
}

fn pushLocalVar(c: *cy.Chunk, _type: LocalVarType, name: []const u8, declType: TypeId, hidden: bool) !LocalVarId {
    const proc = c.proc();
    const id: u32 = @intCast(c.varStack.items.len);

    if (!hidden) {
        _ = try proc.nameToVar.put(c.alloc, name, .{
            .varId = id,
            .blockId = @intCast(c.semaBlocks.items.len-1),
        });
    }
    try c.varStack.append(c.alloc, .{
        .declT = declType,
        .type = _type,
        .dynamicLastMutBlockId = 0,
        .namePtr = name.ptr,
        .nameLen = @intCast(name.len),
        .vtype = CompactType.init(declType),
    });
    return id;
}

fn getVarPtr(self: *cy.Chunk, name: []const u8) ?*LocalVar {
    if (self.proc().nameToVar.get(name)) |varId| {
        return &self.vars.items[varId];
    } else return null;
}

fn pushStaticVarAlias(c: *cy.Chunk, name: []const u8, sym: *Sym) !LocalVarId {
    const id = try pushLocalVar(c, .staticAlias, name, bt.Any, false);
    c.varStack.items[id].inner = .{ .staticAlias = sym };
    return id;
}

fn pushObjectMemberAlias(c: *cy.Chunk, name: []const u8, idx: u8, typeId: TypeId) !LocalVarId {
    const id = try pushLocalVar(c, .objectMemberAlias, name, typeId, false);
    c.varStack.items[id].inner = .{ .objectMemberAlias = .{
        .fieldIdx = idx,
    }};
    return id;
}

fn pushCapturedObjectMemberAlias(self: *cy.Chunk, name: []const u8, parentVarId: LocalVarId, idx: u8, vtype: TypeId) !LocalVarId {
    const proc = self.proc();
    const id = try pushLocalVar(self, .parentObjectMemberAlias, name, vtype, false);
    const capturedIdx: u8 = @intCast(proc.captures.items.len);
    self.varStack.items[id].inner = .{ .parentObjectMemberAlias = .{
        .parentVarId = parentVarId,
        .selfCapturedIdx = capturedIdx,
        .fieldIdx = idx,
    }};

    const pvar = &self.varStack.items[parentVarId];
    pvar.inner.local.lifted = true;

    try self.capVarDescs.put(self.alloc, id, .{
        .user = parentVarId,
    });

    try proc.captures.append(self.alloc, id);
    return id;
}

fn pushCapturedVar(c: *cy.Chunk, name: []const u8, parentVarId: LocalVarId, vtype: CompactType) !LocalVarId {
    const proc = c.proc();
    const id = try pushLocalVar(c, .parentLocalAlias, name, vtype.id, false);
    const capturedIdx: u8 = @intCast(proc.captures.items.len);
    c.varStack.items[id].inner = .{
        .parentLocalAlias = .{
            .capturedIdx = capturedIdx,
        },
    };
    c.varStack.items[id].vtype = vtype;

    const pvar = &c.varStack.items[parentVarId];
    pvar.inner.local.lifted = true;

    // Patch local IR.
    if (!pvar.inner.local.isParam) {
        const declIrStart = pvar.inner.local.declIrStart;

        if (pvar.inner.local.hasInit) {
            const data = c.ir.getStmtDataPtr(declIrStart, .declareLocalInit);
            data.lifted = true;
        } else {
            const data = c.ir.getStmtDataPtr(declIrStart, .declareLocal);
            data.lifted = true;
        }
    }

    try c.capVarDescs.put(c.alloc, id, .{
        .user = parentVarId,
    });

    try proc.captures.append(c.alloc, id);
    return id;
}

fn referenceSym(c: *cy.Chunk, sym: *Sym, nodeId: cy.NodeId) !void {
    if (!c.isInStaticInitializer()) {
        return;
    }

    if (c.curInitingSym == sym) {
        const node = c.ast.node(nodeId);
        const name = c.ast.nodeString(node);
        return c.reportErrorFmt("Reference to `{}` creates a circular dependency.", &.{v(name)}, nodeId);
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
pub fn getOrLookupVar(self: *cy.Chunk, name: []const u8, staticLookup: bool, nodeId: cy.NodeId) !VarLookupResult {
    if (self.semaProcs.items.len == 1 and self.isInStaticInitializer()) {
        return (try lookupStaticVar(self, name, nodeId, staticLookup)) orelse {
            return self.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, nodeId);
        };
    }

    const proc = self.proc();
    if (proc.nameToVar.get(name)) |varInfo| {
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
                return self.reportErrorFmt("Can not reference local `{}` in a static initializer.", &.{v(name)}, nodeId);
            }
            return VarLookupResult{
                .local = varInfo.varId,
            };
        } else {
            if (self.isInStaticInitializer() and self.semaBlockDepth() == 0) {
                return self.reportErrorFmt("Can not reference local `{}` in a static initializer.", &.{v(name)}, nodeId);
            }
            return VarLookupResult{
                .local = varInfo.varId,
            };
        }
    }

    // Look for object member if inside method.
    if (proc.isMethodBlock) {
        if (self.curSelfSym.?.getMod().?.getSym(name)) |sym| {
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
        } else if (proc.isStaticFuncBlock) {
            // Can not capture local before static function block.
            const funcName = proc.func.?.name();
            return self.reportErrorFmt("Can not capture the local variable `{}` from static function `{}`.\nOnly lambdas (anonymous functions) can capture local variables.", &.{v(name), v(funcName)}, self.curNodeId);
        }

        // Create a local captured variable.
        const parentVar = self.varStack.items[res.varId];
        if (res.isObjectField) {
            const parentBlock = &self.semaProcs.items[res.blockIdx];
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
            return self.reportErrorFmt("Undeclared variable `{}`.", &.{v(name)}, nodeId);
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
        const prev = c.semaProcs.items[c.semaProcs.items.len - 2];
        if (prev.nameToVar.get(name)) |varInfo| {
            const svar = c.varStack.items[varInfo.varId];
            if (svar.isCapturable()) {
                return .{
                    .isObjectField = false,
                    .varId = varInfo.varId,
                    .blockIdx = @intCast(c.semaProcs.items.len - 2),
                    .fieldIdx = undefined,
                    .fieldT = undefined,
                };
            }
        }

        // Look for object member if inside method.
        if (prev.isMethodBlock) {
            if (c.curSelfSym.?.getMod().?.getSym(name)) |sym| {
                if (sym.type == .field) {
                    const field = sym.cast(.field);
                    return .{
                        .varId = prev.nameToVar.get("self").?.varId,
                        .blockIdx = @intCast(c.semaProcs.items.len - 2),
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

fn reportIncompatibleCallSig(c: *cy.Chunk, sym: *cy.sym.FuncSym, args: []const cy.TypeId, ret_cstr: ReturnCstr, nodeId: cy.NodeId) anyerror {
    const name = sym.head.name();
    var msg: std.ArrayListUnmanaged(u8) = .{};
    const w = msg.writer(c.alloc);
    const callSigStr = try c.sema.allocTypesStr(args);
    defer c.alloc.free(callSigStr);

    try w.print("Can not find compatible function for call: `{s}{s}`.", .{name, callSigStr});
    if (ret_cstr == .not_void) {
        try w.writeAll(" Expects non-void return.");
    }
    try w.writeAll("\n");
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
    try c.compiler.addReportConsume(.compile_err, c.id, nodeId, try msg.toOwnedSlice(c.alloc));
    return error.CompileError;
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

fn checkTypeCstr(c: *cy.Chunk, ctype: CompactType, cstrTypeId: TypeId, nodeId: cy.NodeId) !void {
    return checkTypeCstr2(c, ctype, cstrTypeId, cstrTypeId, nodeId);
}

fn checkTypeCstr2(c: *cy.Chunk, ctype: CompactType, cstrTypeId: TypeId, reportCstrTypeId: TypeId, nodeId: cy.NodeId) !void {
    // Dynamic is allowed.
    if (!ctype.dynamic) {
        if (!cy.types.isTypeSymCompat(c.compiler, ctype.id, cstrTypeId)) {
            const cstrName = try c.sema.allocTypeName(reportCstrTypeId);
            defer c.alloc.free(cstrName);
            const typeName = try c.sema.allocTypeName(ctype.id);
            defer c.alloc.free(typeName);
            return c.reportErrorFmt("Expected type `{}`, got `{}`.", &.{v(cstrName), v(typeName)}, nodeId);
        }
    }
}

fn pushExprRes(c: *cy.Chunk, res: ExprResult) !void {
    try c.exprResStack.append(c.alloc, res);
}

const SymAccessType = enum {
    sym,
    field,
};

const SymAccessResult = struct {
    type: SymAccessType,
    data: union {
        sym: *Sym,
        field: ExprResult,
    },
};

fn semaSymAccessRight(c: *cy.Chunk, sym: *Sym, sym_n: cy.NodeId, rightId: cy.NodeId) !SymAccessResult {
    if (sym.isVariable()) {
        const rec = try sema.symbol(c, sym, sym_n, true);
        const res = try semaAccessField(c, rec, rightId);
        return .{ .type = .field, .data = .{ .field = res }};
    } else {
        const right = c.ast.node(rightId);
        if (right.type() != .ident) {
            return error.Unexpected;
        }
        const rightName = c.ast.nodeString(right);
        const rightSym = try c.findDistinctSym(sym, rightName, rightId, true);
        try referenceSym(c, rightSym, rightId);
        return .{ .type = .sym, .data = .{ .sym = rightSym }};
    }
}

const SwitchInfo = struct {
    exprType: CompactType,
    exprTypeSym: *cy.Sym,
    exprIsChoiceType: bool,
    choiceIrVarId: u8,
    isStmt: bool,

    fn init(c: *cy.Chunk, expr: ExprResult, isStmt: bool) SwitchInfo {
        var info = SwitchInfo{
            .exprType = expr.type,
            .exprTypeSym = c.sema.getTypeSym(expr.type.id),
            .exprIsChoiceType = false,
            .choiceIrVarId = undefined,
            .isStmt = isStmt,
        };

        if (info.exprTypeSym.type == .enum_t) {
            if (info.exprTypeSym.cast(.enum_t).isChoiceType) {
                info.exprIsChoiceType = true;
            }
        }
        return info;
    }
};

fn semaSwitchStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.ast.node(nodeId);

    // Perform sema on expr first so it knows how to construct the rest of the switch.
    const exprId = node.data.switchBlock.expr;
    const expr = try c.semaExpr(exprId, .{});
    var info = SwitchInfo.init(c, expr, true);

    var blockLoc: u32 = undefined;
    var exprLoc: u32 = undefined;
    if (info.exprIsChoiceType) {
        blockLoc = try c.ir.pushStmt(c.alloc, .block, nodeId, .{ .bodyHead = cy.NullId });
        try pushBlock(c, nodeId);
        exprLoc = try semaSwitchChoicePrologue(c, &info, expr, exprId);
    } else {
        exprLoc = expr.irIdx;
    }

    _ = try c.ir.pushStmt(c.alloc, .switchStmt, nodeId, {});
    _ = try semaSwitchBody(c, info, nodeId, exprLoc);

    if (info.exprIsChoiceType) {
        const stmtBlock = try popBlock(c);
        const block = c.ir.getStmtDataPtr(blockLoc, .block);
        block.bodyHead = stmtBlock.first;
    }
}

fn semaSwitchExpr(c: *cy.Chunk, nodeId: cy.NodeId) !ExprResult {
    const node = c.ast.node(nodeId);

    // Perform sema on expr first so it knows how to construct the rest of the switch.
    const exprId = node.data.switchBlock.expr;
    const expr = try c.semaExpr(exprId, .{});
    var info = SwitchInfo.init(c, expr, false);

    var blockExprLoc: u32 = undefined;
    var exprLoc: u32 = undefined;
    if (info.exprIsChoiceType) {
        blockExprLoc = try c.ir.pushExpr(.blockExpr, c.alloc, bt.Void, nodeId, .{ .bodyHead = cy.NullId });
        try pushBlock(c, nodeId);

        exprLoc = try semaSwitchChoicePrologue(c, &info, expr, exprId);
    } else {
        exprLoc = expr.irIdx;
    }

    if (info.exprIsChoiceType) {
    }
    const irIdx = try semaSwitchBody(c, info, nodeId, exprLoc);

    if (info.exprIsChoiceType) {
        _ = try c.ir.pushStmt(c.alloc, .exprStmt, nodeId, .{
            .expr = irIdx,
            .isBlockResult = true,
        });

        const stmtBlock = try popBlock(c);
        const blockExpr = c.ir.getExprDataPtr(blockExprLoc, .blockExpr);
        blockExpr.bodyHead = stmtBlock.first;
        return ExprResult.initStatic(blockExprLoc, bt.Any);
    } else {
        return ExprResult.initStatic(irIdx, bt.Any);
    }
}

/// Choice expr is assigned to a hidden local.
/// The choice tag is then used for the switch expr.
/// Choice payload is copied to case blocks that have a capture param.
fn semaSwitchChoicePrologue(c: *cy.Chunk, info: *SwitchInfo, expr: ExprResult, exprId: cy.NodeId) !u32 {
    const choiceVarId = try declareLocalName2(c, "choice", expr.type.id, true, true, exprId);
    const choiceVar = &c.varStack.items[choiceVarId];
    info.choiceIrVarId = choiceVar.inner.local.id;
    const declareLoc = choiceVar.inner.local.declIrStart;
    const declare = c.ir.getStmtDataPtr(declareLoc, .declareLocalInit);
    declare.init = expr.irIdx;
    declare.initType = expr.type;

    // Get choice tag for switch expr.
    const recLoc = try c.ir.pushExpr(.local, c.alloc, expr.type.id, exprId, .{ .id = choiceVar.inner.local.id });
    const exprLoc = try c.ir.pushExpr(.field, c.alloc, bt.Integer, exprId, .{
        .idx = 0,
        .rec = recLoc,
        .numNestedFields = 0,
    });
    return exprLoc;
}

fn semaSwitchBody(c: *cy.Chunk, info: SwitchInfo, nodeId: cy.NodeId, exprLoc: u32) !u32 {
    const node = c.ast.node(nodeId);
    const irIdx = try c.ir.pushExpr(.switchExpr, c.alloc, bt.Any, nodeId, .{
        .numCases = node.data.switchBlock.numCases,
        .expr = exprLoc,
    });
    const irCasesIdx = try c.ir.pushEmptyArray(c.alloc, u32, node.data.switchBlock.numCases);

    var caseId: cy.NodeId = node.data.switchBlock.caseHead;
    var i: u32 = 0;
    while (caseId != cy.NullNode) {
        const irCaseIdx = try semaSwitchCase(c, info, caseId);
        c.ir.setArrayItem(irCasesIdx, u32, i, irCaseIdx);
        caseId = c.ast.node(caseId).next();
        i += 1;
    }

    return irIdx;
}

const CaseState = struct {
    irCondsIdx: u32,
    condId: cy.NodeId,
    captureType: cy.TypeId = cy.NullId,
    condIdx: u32,
};

fn semaSwitchElseCase(c: *cy.Chunk, info: SwitchInfo, nodeId: cy.NodeId) !u32 {
    const case = c.ast.node(nodeId);

    const irIdx = try c.ir.pushEmptyExpr(.switchCase, c.alloc, undefined, nodeId);

    var bodyHead: u32 = undefined;
    if (case.data.caseBlock.bodyIsExpr) {
        // Wrap in block expr.
        bodyHead = try c.ir.pushExpr(.blockExpr, c.alloc, bt.Any, case.data.caseBlock.bodyHead, .{ .bodyHead = cy.NullId });
        try pushBlock(c, nodeId);

        const expr = try c.semaExpr(case.data.caseBlock.bodyHead, .{});
        _ = try c.ir.pushStmt(c.alloc, .exprStmt, case.data.caseBlock.bodyHead, .{
            .expr = expr.irIdx,
            .isBlockResult = true,
        });
        const stmtBlock = try popBlock(c);
        const blockExpr = c.ir.getExprDataPtr(bodyHead, .blockExpr);
        blockExpr.bodyHead = stmtBlock.first;
    } else {
        try pushBlock(c, nodeId);

        if (!info.isStmt) {
            return c.reportErrorFmt("Assign switch statement requires a return case: `else => {expr}`", &.{}, nodeId);
        }

        try semaStmts(c, case.data.caseBlock.bodyHead);
        const stmtBlock = try popBlock(c);
        bodyHead = stmtBlock.first;
    }

    c.ir.setExprData(irIdx, .switchCase, .{
        .numConds = 0,
        .bodyIsExpr = case.data.caseBlock.bodyIsExpr,
        .bodyHead = bodyHead,
    });
    return irIdx;
}

fn semaSwitchCase(c: *cy.Chunk, info: SwitchInfo, nodeId: cy.NodeId) !u32 {
    const case = c.ast.node(nodeId);
    if (case.data.caseBlock.header == cy.NullNode) {
        return semaSwitchElseCase(c, info, nodeId);
    }

    const header = c.ast.node(case.data.caseBlock.header);
    const numConds = header.data.caseHeader.numConds;
    const irIdx = try c.ir.pushEmptyExpr(.switchCase, c.alloc, undefined, nodeId);

    const hasCapture = header.data.caseHeader.capture != cy.NullNode;
    var state = CaseState{
        .irCondsIdx = undefined,
        .condId = header.data.caseHeader.condHead,
        .condIdx = 0,
        .captureType = cy.NullId,
    };

    if (state.condId != cy.NullNode) {
        state.irCondsIdx = try c.ir.pushEmptyArray(c.alloc, u32, numConds);
        try semaCaseCond(c, info, &state);
        if (state.condId != cy.NullNode) {
            while (state.condId != cy.NullNode) {
                try semaCaseCond(c, info, &state);
            }
        }
    }

    var bodyHead: u32 = undefined;
    if (case.data.caseBlock.bodyIsExpr) {
        // Wrap in block expr.
        bodyHead = try c.ir.pushExpr(.blockExpr, c.alloc, bt.Any, case.data.caseBlock.bodyHead, .{ .bodyHead = cy.NullId });
        try pushBlock(c, nodeId);
    } else {
        try pushBlock(c, nodeId);
    }

    if (hasCapture) {
        const declT = if (info.exprType.dynamic) bt.Dynamic else state.captureType;
        const capVarId = try declareLocal(c, header.data.caseHeader.capture, declT, true);
        const declareLoc = c.varStack.items[capVarId].inner.local.declIrStart;

        // Copy payload to captured var.
        const recLoc = try c.ir.pushExpr(.local, c.alloc, info.exprType.id, header.data.caseHeader.capture, .{ .id = info.choiceIrVarId });
        const fieldLoc = try c.ir.pushExpr(.field, c.alloc, declT, header.data.caseHeader.capture, .{
            .idx = 1,
            .rec = recLoc,
            .numNestedFields = 0,
        });

        const declare = c.ir.getStmtDataPtr(declareLoc, .declareLocalInit);
        declare.init = fieldLoc;
        declare.initType = CompactType.init(declT);
    }

    if (case.data.caseBlock.bodyIsExpr) {
        const expr = try c.semaExpr(case.data.caseBlock.bodyHead, .{});
        _ = try c.ir.pushStmt(c.alloc, .exprStmt, case.data.caseBlock.bodyHead, .{
            .expr = expr.irIdx,
            .isBlockResult = true,
        });
        const stmtBlock = try popBlock(c);
        const blockExpr = c.ir.getExprDataPtr(bodyHead, .blockExpr);
        blockExpr.bodyHead = stmtBlock.first;
    } else {
        if (!info.isStmt) {
            return c.reportErrorFmt("Assign switch statement requires a return case: `case {cond} => {expr}`", &.{}, nodeId);
        }

        try semaStmts(c, case.data.caseBlock.bodyHead);
        const stmtBlock = try popBlock(c);
        bodyHead = stmtBlock.first;
    }

    c.ir.setExprData(irIdx, .switchCase, .{
        .numConds = numConds,
        .bodyIsExpr = case.data.caseBlock.bodyIsExpr,
        .bodyHead = bodyHead,
    });
    return irIdx;
}

fn semaCaseCond(c: *cy.Chunk, info: SwitchInfo, state: *CaseState) !void {
    const cond = c.ast.node(state.condId);

    if (info.exprIsChoiceType) {
        if (cond.type() == .symbolLit) {
            const name = c.ast.nodeString(cond);
            if (info.exprTypeSym.cast(.enum_t).getMember(name)) |member| {
                const condRes = try c.semaInt(member.val, state.condId);
                c.ir.setArrayItem(state.irCondsIdx, u32, state.condIdx, condRes.irIdx);
                state.condId = cond.next();
                state.condIdx += 1;
                if (state.captureType == cy.NullId) {
                    state.captureType = member.payloadType;
                } else {
                    // TODO: Merge into compile-time union.
                    state.captureType = bt.Any;
                }
                return;
            } else {
                const targetTypeName = info.exprTypeSym.name();
                return c.reportErrorFmt("`{}` is not a member of `{}`", &.{v(name), v(targetTypeName)}, state.condId);
            }
        } else {
            const targetTypeName = info.exprTypeSym.name();
            return c.reportErrorFmt("Expected to match a member of `{}`", &.{v(targetTypeName)}, state.condId);
        }
    }

    // General case.
    const condRes = try c.semaExpr(state.condId, .{});
    c.ir.setArrayItem(state.irCondsIdx, u32, state.condIdx, condRes.irIdx);
    state.condId = cond.next();
    state.condIdx += 1;
}


pub const ChunkExt = struct {

    pub fn semaZeroInit(c: *cy.Chunk, typeId: cy.TypeId, nodeId: cy.NodeId) !ExprResult {
        switch (typeId) {
            bt.Any,
            bt.Dynamic  => return c.semaInt(0, nodeId),
            bt.Boolean  => return c.semaFalse(nodeId),
            bt.Integer  => return c.semaInt(0, nodeId),
            bt.Float    => return c.semaFloat(0, nodeId),
            bt.List     => return c.semaEmptyList(nodeId),
            bt.Map      => return c.semaEmptyMap(nodeId),
            bt.Array    => return c.semaArray("", nodeId),
            bt.String   => return c.semaString("", nodeId),
            else => {
                const sym = c.sema.getTypeSym(typeId);
                if (sym.type != .object_t) {
                    return error.Unsupported;
                }

                const obj = sym.cast(.object_t);
                const irArgsIdx = try c.ir.pushEmptyArray(c.alloc, u32, obj.numFields);
                const irIdx = try c.ir.pushExpr(.object_init, c.alloc, typeId, nodeId, .{
                    .typeId = obj.type, .numArgs = @as(u8, @intCast(obj.numFields)), .args = irArgsIdx,
                });
                for (obj.fields[0..obj.numFields], 0..) |field, i| {
                    const arg = try semaZeroInit(c, field.type, nodeId);
                    c.ir.setArrayItem(irArgsIdx, u32, i, arg.irIdx);
                }
                return ExprResult.initStatic(irIdx, typeId);
            },
        }
    }

    /// `initializerId` is a record literal node.
    pub fn semaObjectInit2(c: *cy.Chunk, obj: *cy.sym.ObjectType, initializerId: cy.NodeId) !ExprResult {
        const type_e = c.sema.types.items[obj.type];
        if (type_e.has_init_pair_method) {
            if (obj.numFields == 0) {
                const init = try c.ir.pushExpr(.object_init, c.alloc, obj.type, initializerId, .{
                    .typeId = obj.type, .numArgs = 0, .args = 0,
                });
                return semaWithInitPairs(c, @ptrCast(obj), obj.type, initializerId, init);
            } else {
                const type_name = try c.sema.allocTypeName(obj.type);
                defer c.alloc.free(type_name);
                return c.reportErrorFmt("Type `{}` can not initialize with `$initPair` since it does not have a default record initializer.", &.{v(type_name)}, initializerId);
            }
        }

        // Set up a temp buffer to map initializer entries to type fields.
        const fieldsDataStart = c.listDataStack.items.len;
        try c.listDataStack.resize(c.alloc, c.listDataStack.items.len + obj.numFields);
        defer c.listDataStack.items.len = fieldsDataStart;

        const undecl_start = c.listDataStack.items.len;

        // Initially set to NullId so missed mappings are known from a linear scan.
        const fieldNodes = c.listDataStack.items[fieldsDataStart..];
        @memset(fieldNodes, .{ .nodeId = cy.NullNode });

        const initializer = c.ast.node(initializerId);
        var entryId = initializer.data.recordLit.argHead;
        while (entryId != cy.NullNode) {
            var entry = c.ast.node(entryId);

            const fieldN = c.ast.node(entry.data.keyValue.key);
            const fieldName = c.ast.nodeString(fieldN);

            const info = try checkSymGetField(c, @ptrCast(obj), fieldName, entry.data.keyValue.key);
            if (info.use_get_method) {
                try c.listDataStack.append(c.alloc, .{ .nodeId = entryId });
                entryId = entry.next();
                continue;
            }

            fieldNodes[info.idx] = .{ .nodeId = entry.data.keyValue.value };
            entryId = entry.next();
        }

        const irIdx = try c.ir.pushEmptyExpr(.object_init, c.alloc, ir.ExprType.init(obj.type), initializerId);
        const irArgsIdx = try c.ir.pushEmptyArray(c.alloc, u32, obj.numFields);

        var i: u32 = 0;
        for (fieldNodes, 0..) |item, fIdx| {
            const fieldT = obj.fields[fIdx].type;
            if (item.nodeId == cy.NullNode) {
                // Check that unset fields can be zero initialized.
                try c.checkForZeroInit(fieldT, initializerId);

                const arg = try c.semaZeroInit(fieldT, initializerId);
                c.ir.setArrayItem(irArgsIdx, u32, i, arg.irIdx);
            } else {
                const arg = try c.semaExprCstr(item.nodeId, fieldT);
                c.ir.setArrayItem(irArgsIdx, u32, i, arg.irIdx);
            }
            i += 1;
        }

        c.ir.setExprData(irIdx, .object_init, .{
            .typeId = obj.type, .numArgs = @as(u8, @intCast(obj.numFields)), .args = irArgsIdx,
        });

        const undecls = c.listDataStack.items[undecl_start..];
        if (undecls.len > 0) {
            // Initialize object, then invoke $set on the undeclared fields.
            const expr = try c.ir.pushExpr(.blockExpr, c.alloc, obj.type, initializerId, .{ .bodyHead = cy.NullId });
            try pushBlock(c, initializerId);
            {
                // create temp with object.
                const var_id = try declareLocalName(c, "$temp", obj.type, true, initializerId);
                const temp_ir_id = c.varStack.items[var_id].inner.local.id;
                const temp_ir = c.varStack.items[var_id].inner.local.declIrStart;
                const decl_stmt = c.ir.getStmtDataPtr(temp_ir, .declareLocalInit);
                decl_stmt.init = irIdx;
                decl_stmt.initType = CompactType.initStatic(obj.type);

                // setFieldDyn.
                const temp_expr = try c.ir.pushExpr(.local, c.alloc, obj.type, initializerId, .{ .id = temp_ir_id });
                for (undecls) |undecl| {
                    const entry = c.ast.node(undecl.nodeId);
                    const field_name = c.ast.nodeStringById(entry.data.keyValue.key);
                    const field_value = try c.semaExpr(entry.data.keyValue.value, .{});
                    _ = try c.ir.pushStmt(c.alloc, .set_field_dyn, initializerId, .{ .set_field_dyn = .{
                        .name = field_name,
                        .rec = temp_expr,
                        .right = field_value.irIdx,
                    }});
                }

                // return temp.
                _ = try c.ir.pushStmt(c.alloc, .exprStmt, initializerId, .{
                    .expr = temp_expr,
                    .isBlockResult = true,
                });
            }
            const stmtBlock = try popBlock(c);
            c.ir.getExprDataPtr(expr, .blockExpr).bodyHead = stmtBlock.first;
            return ExprResult.initStatic(expr, obj.type);
        } else {
            return ExprResult.initStatic(irIdx, obj.type);
        }
    }

    pub fn semaObjectInit(c: *cy.Chunk, expr: Expr) !ExprResult {
        const node = c.ast.node(expr.nodeId);

        const left = try c.semaExprSkipSym(node.data.record_expr.left);
        if (left.resType != .sym) {
            const desc = try c.encoder.allocFmt(c.alloc, node.data.record_expr.left);
            defer c.alloc.free(desc);
            return c.reportErrorFmt("Type `{}` does not exist.", &.{v(desc)}, node.data.record_expr.left);
        }

        const sym = left.data.sym.resolved();
        switch (sym.type) {
            .struct_t => {
                const obj = sym.cast(.struct_t);
                return c.semaObjectInit2(obj, node.data.record_expr.record);
            },
            .object_t => {
                const obj = sym.cast(.object_t);
                return c.semaObjectInit2(obj, node.data.record_expr.record);
            },
            .enum_t => {
                const enumType = sym.cast(.enum_t);

                const initializer = c.ast.node(node.data.record_expr.record);
                if (initializer.data.recordLit.numArgs != 1) {
                    return c.reportErrorFmt("Expected only one member to payload entry.", &.{}, node.data.record_expr.record);
                }

                const entryId = initializer.data.recordLit.argHead;
                const entry = c.ast.node(entryId);

                const field = c.ast.node(entry.data.keyValue.key);
                const fieldName = c.ast.nodeString(field);

                const member = enumType.getMember(fieldName) orelse {
                    return c.reportErrorFmt("`{}` is not a member of `{}`", &.{v(fieldName), v(enumType.head.name())}, entry.data.keyValue.key);
                };

                var b: ObjectBuilder = .{ .c = c };
                try b.begin(member.type, 2, expr.nodeId);
                const tag = try c.semaInt(member.val, expr.nodeId);
                b.pushArg(tag);
                const payload = try c.semaExprCstr(entry.data.keyValue.value, member.payloadType);
                b.pushArg(payload);
                const irIdx = b.end();
                return ExprResult.initStatic(irIdx, member.type);
            },
            .enumMember => {
                const member = sym.cast(.enumMember);
                const enumSym = member.head.parent.?.cast(.enum_t);
                // Check if enum is choice type.
                if (!enumSym.isChoiceType) {
                    const desc = try c.encoder.allocFmt(c.alloc, node.data.record_expr.left);
                    defer c.alloc.free(desc);
                    return c.reportErrorFmt("Can not initialize `{}`. It is not a choice type.", &.{v(desc)}, node.data.record_expr.left);
                }

                if (member.payloadType == cy.NullId) {
                    // No payload type. Ensure empty record literal.
                    const initializer = c.ast.node(node.data.record_expr.record);
                    if (initializer.data.recordLit.numArgs != 0) {
                        return c.reportErrorFmt("Expected empty record literal.", &.{}, node.data.record_expr.left);
                    }

                    var b: ObjectBuilder = .{ .c = c };
                    try b.begin(member.type, 2, expr.nodeId);
                    const tag = try c.semaInt(member.val, expr.nodeId);
                    b.pushArg(tag);
                    const payload = try c.semaZeroInit(bt.Any, expr.nodeId);
                    b.pushArg(payload);
                    const irIdx = b.end();
                    return ExprResult.initStatic(irIdx, member.type);
                } else {
                    if (!c.sema.isUserObjectType(member.payloadType)) {
                        const payloadTypeName = c.sema.getTypeBaseName(member.payloadType);
                        return c.reportErrorFmt("The payload type `{}` can not be initialized with key value pairs.", &.{v(payloadTypeName)}, node.data.record_expr.left);
                    }

                    const obj = c.sema.getTypeSym(member.payloadType).cast(.object_t);

                    var b: ObjectBuilder = .{ .c = c };
                    try b.begin(member.type, 2, expr.nodeId);
                    const tag = try c.semaInt(member.val, expr.nodeId);
                    b.pushArg(tag);
                    const payload = try c.semaObjectInit2(obj, node.data.record_expr.record);
                    b.pushArg(payload);
                    const irIdx = b.end();
                    return ExprResult.initStatic(irIdx, member.type);
                }
            },
            .typeTemplate => {
                const desc = try c.encoder.allocFmt(c.alloc, node.data.record_expr.left);
                defer c.alloc.free(desc);
                return c.reportErrorFmt("Expected a type symbol. `{}` is a type template and must be expanded to a type first.", &.{v(desc)}, node.data.record_expr.left);
            },
            else => {
                const desc = try c.encoder.allocFmt(c.alloc, node.data.record_expr.left);
                defer c.alloc.free(desc);
                return c.reportErrorFmt("Can not initialize `{}`.", &.{v(desc)}, node.data.record_expr.left);
            }
        }
    }

    pub fn semaCallFuncSymRec(c: *cy.Chunk, loc: u32, sym: *cy.sym.FuncSym, rec: cy.NodeId, rec_res: ExprResult,
        argHead: cy.NodeId, numArgs: u8, ret_cstr: ReturnCstr, node: cy.NodeId) !ExprResult {

        var matcher = try FuncMatcher.init(c, sym, numArgs + 1, ret_cstr);
        defer matcher.deinit(c);

        // Receiver.
        try matcher.matchArg(c, rec, 0, rec_res);

        var arg = argHead;
        var arg_idx: u32 = 1;

        while (arg_idx < numArgs + 1) {
            const arg_n = c.ast.node(arg);
            const arg_res = try c.semaExprTarget(arg, matcher.getArgTypeHint(arg_idx));
            try matcher.matchArg(c, arg, arg_idx, arg_res);
            arg_idx += 1;
            arg = arg_n.next();
        }

        try matcher.matchEnd(c, node);

        return c.semaCallFuncSymResult(loc, &matcher, node);
    }

    pub fn semaCallFuncSymResult(c: *cy.Chunk, loc: u32, matcher: *FuncMatcher, node: cy.NodeId) !ExprResult {
        try referenceSym(c, @ptrCast(matcher.sym), node);
        if (matcher.dyn_call) {
            // Dynamic call.
            c.ir.setExprCode(loc, .pre_call_sym_dyn);
            c.ir.setExprType(loc, bt.Any);
            c.ir.setExprData(loc, .pre_call_sym_dyn, .{ .call_sym_dyn = .{
                .sym = matcher.sym, .nargs = @as(u8, @intCast(matcher.nargs)), .args = matcher.args_loc,
            }});
            return ExprResult.initDynamic(loc, bt.Any);
        } else {
            const func = matcher.func;
            c.ir.setExprCode(loc, .preCallFuncSym);
            c.ir.setExprType2(loc, .{ .id = @intCast(func.retType), .throws = func.throws });
            c.ir.setExprData(loc, .preCallFuncSym, .{ .callFuncSym = .{
                .func = func, .numArgs = @as(u8, @intCast(matcher.nargs)), .args = matcher.args_loc,
            }});
            return ExprResult.init(loc, CompactType.init(func.retType));
        }
    }

    pub fn semaCallFuncSym1(c: *cy.Chunk, loc: u32, sym: *cy.sym.FuncSym, arg1_n: cy.NodeId, arg1: ExprResult,
        ret_cstr: ReturnCstr, node: cy.NodeId) !ExprResult {

        var matcher = try FuncMatcher.init(c, sym, 1, ret_cstr);
        defer matcher.deinit(c);

        try matcher.matchArg(c, arg1_n, 0, arg1);
        try matcher.matchEnd(c, node);

        return c.semaCallFuncSymResult(loc, &matcher, node);
    }

    pub fn semaCallFuncSym2(c: *cy.Chunk, loc: u32, sym: *cy.sym.FuncSym, arg1_n: cy.NodeId, arg1: ExprResult,
        arg2_n: cy.NodeId, arg2: ExprResult, ret_cstr: ReturnCstr, node: cy.NodeId) !ExprResult {

        var matcher = try FuncMatcher.init(c, sym, 2, ret_cstr);
        defer matcher.deinit(c);

        try matcher.matchArg(c, arg1_n, 0, arg1);
        try matcher.matchArg(c, arg2_n, 1, arg2);
        try matcher.matchEnd(c, node);

        return c.semaCallFuncSymResult(loc, &matcher, node);
    }

    pub fn semaCallFuncSymN(c: *cy.Chunk, sym: *cy.sym.FuncSym, arg_nodes: []const cy.NodeId, args: []const ExprResult, ret_cstr: ReturnCstr, node: cy.NodeId) !ExprResult {
        var matcher = try FuncMatcher.init(c, sym, @intCast(args.len), ret_cstr);
        defer matcher.deinit(c);

        for (args, 0..) |arg, i| {
            try matcher.matchArg(c, arg_nodes[i], @intCast(i), arg);
        }
        try matcher.matchEnd(c, node);

        const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, node);
        return c.semaCallFuncSymResult(loc, &matcher, node);
    }

    /// Match first overloaded function.
    pub fn semaCallFuncSym(c: *cy.Chunk, loc: u32, sym: *cy.sym.FuncSym, argHead: cy.NodeId, numArgs: u8, ret_cstr: ReturnCstr, node: cy.NodeId) !ExprResult {
        var matcher = try FuncMatcher.init(c, sym, numArgs, ret_cstr);
        defer matcher.deinit(c);

        var arg = argHead;
        var arg_idx: u32 = 0;

        while (arg_idx < numArgs) {
            const arg_n = c.ast.node(arg);
            const arg_res = try c.semaExprTarget(arg, matcher.getArgTypeHint(arg_idx));
            try matcher.matchArg(c, arg, arg_idx, arg_res);
            arg_idx += 1;
            arg = arg_n.next();
        }

        try matcher.matchEnd(c, node);

        return c.semaCallFuncSymResult(loc, &matcher, node);
    }

    pub fn semaPushCallArgs(c: *cy.Chunk, argHead: cy.NodeId, numArgs: u8) !u32 {
        const loc = try c.ir.pushEmptyArray(c.alloc, u32, numArgs);
        var nodeId = argHead;
        var i: u32 = 0;
        while (nodeId != cy.NullNode) {
            const arg = c.ast.node(nodeId);
            const argRes = try c.semaExpr(nodeId, .{});
            c.ir.setArrayItem(loc, u32, i, argRes.irIdx);
            i += 1;
            nodeId = arg.next();
        }
        return loc;
    }

    /// Skips emitting IR for a sym.
    pub fn semaExprSkipSym(c: *cy.Chunk, nodeId: cy.NodeId) !ExprResult {
        const node = c.ast.node(nodeId);
        if (node.type() == .ident) {
            return try semaIdent(c, nodeId, false);
        } else if (node.type() == .accessExpr) {
            const left = c.ast.node(node.data.accessExpr.left);
            if (left.type() == .ident or left.type() == .accessExpr) {
                const right = c.ast.node(node.data.accessExpr.right); 
                if (right.type() != .ident) return error.Unexpected;

                // TODO: Check if ident is sym to reduce work.
                const expr = Expr.init(nodeId);
                return try c.semaAccessExpr(expr, false);
            } else {
                return try c.semaExpr(nodeId, .{});
            }
        } else {
            // No chance it's a symbol path.
            return try c.semaExpr(nodeId, .{});
        }
    }

    pub fn semaExprTarget(c: *cy.Chunk, nodeId: cy.NodeId, target_t: TypeId) !ExprResult {
        return try semaExpr(c, nodeId, .{
            .target_t = target_t,
            .req_target_t = false,
        });
    }

    pub fn semaExprCstr(c: *cy.Chunk, nodeId: cy.NodeId, typeId: TypeId) !ExprResult {
        return try semaExpr(c, nodeId, .{
            .target_t = typeId,
            .req_target_t = true,
        });
    }

    pub fn semaOptionExpr(c: *cy.Chunk, node_id: cy.NodeId) !ExprResult {
        const res = try semaExpr(c, node_id, .{});
        if (res.type.dynamic and res.type.id == bt.Any) {
            // Runtime check.
            var new_res = res;
            new_res.irIdx = try c.ir.pushExpr(.typeCheckOption, c.alloc, bt.Any, node_id, .{
                .expr = res.irIdx,
            });
            return new_res;
        } else {
            const type_sym = c.sema.getTypeSym(res.type.id);
            if (type_sym.parent != @as(*cy.Sym, @ptrCast(c.sema.option_tmpl))) {
                const name = c.sema.getTypeBaseName(res.type.id);
                return c.reportErrorFmt("Expected `Option` type, found `{}`.", &.{v(name)}, node_id);
            }
            return res;
        }
    }

    pub fn semaExpr(c: *cy.Chunk, nodeId: cy.NodeId, opts: SemaExprOptions) !ExprResult {
        // Set current node for unexpected errors.
        c.curNodeId = nodeId;

        const expr = Expr{
            .target_t = opts.target_t,
            .reqTypeCstr = opts.req_target_t,
            .nodeId = nodeId,
        };
        return c.semaExpr2(expr);
    }

    pub fn semaExpr2(c: *cy.Chunk, expr: Expr) !ExprResult {
        const res = try c.semaExprNoCheck(expr);
        if (cy.Trace) {
            const nodeId = expr.nodeId;
            const type_name = c.sema.getTypeBaseName(res.type.id);
            log.tracev("expr.{s}: end {s}", .{@tagName(c.ast.node(nodeId).type()), type_name});
        }

        if (expr.hasTargetType()) {
            // TODO: Check for exact match first since it's the common case.
            const type_e = c.sema.types.items[expr.target_t];
            if (type_e.kind == .option) {
                // Already the same optional type.
                if (res.type.id == expr.target_t) {
                    return res;
                }
                // Check if type is compatible with Optional's some payload.
                const someMember = type_e.sym.cast(.enum_t).getMemberByIdx(1);
                if (cy.types.isTypeSymCompat(c.compiler, res.type.id, someMember.payloadType)) {
                    // Generate IR to wrap value into optional.
                    var b: ObjectBuilder = .{ .c = c };
                    try b.begin(expr.target_t, 2, expr.nodeId);
                    const tag = try c.semaInt(1, expr.nodeId);
                    b.pushArg(tag);
                    b.pushArg(res);
                    const irIdx = b.end();

                    return ExprResult.initStatic(irIdx, expr.target_t);
                }
            } else {
                if (expr.target_t == bt.Any and res.type.id != bt.Any) {
                    // Box value.
                    var newRes = res;
                    newRes.irIdx = try c.ir.pushExpr(.box, c.alloc, bt.Any, expr.nodeId, .{
                        .expr = res.irIdx,
                    });
                    return newRes;
                }
            }

            if (expr.reqTypeCstr) {
                if (!cy.types.isTypeSymCompat(c.compiler, res.type.id, expr.target_t)) {
                    if (res.type.dynamic) {
                        var new_res = res;
                        new_res.irIdx = try c.ir.pushExpr(.type_check, c.alloc, expr.target_t, expr.nodeId, .{
                            .expr = res.irIdx,
                            .exp_type = expr.target_t,
                        });
                        return new_res;
                    }
                    const cstrName = try c.sema.allocTypeName(expr.target_t);
                    defer c.alloc.free(cstrName);
                    const typeName = try c.sema.allocTypeName(res.type.id);
                    defer c.alloc.free(typeName);
                    return c.reportErrorFmt("Expected type `{}`, got `{}`.", &.{v(cstrName), v(typeName)}, expr.nodeId);
                }
            }
        }
        return res;
    }

    pub fn semaExprNoCheck(c: *cy.Chunk, expr: Expr) anyerror!ExprResult {
        if (cy.Trace) {
            const nodeId = expr.nodeId;
            const nodeStr = try c.encoder.format(nodeId, &cy.tempBuf);
            log.tracev("expr.{s}: \"{s}\"", .{@tagName(c.ast.node(nodeId).type()), nodeStr});
        }

        const nodeId = expr.nodeId;
        const node = c.ast.node(nodeId);
        c.curNodeId = nodeId;
        switch (node.type()) {
            .noneLit => {
                if (expr.hasTargetType()) {
                    return c.semaNone(expr.target_t, nodeId);
                } else {
                    return c.reportErrorFmt("Could not determine optional type for `none`.", &.{}, nodeId);
                }
            },
            .errorSymLit => {
                const sym = c.ast.node(node.data.errorSymLit.symbol);
                const name = c.ast.nodeString(sym);
                const loc = try c.ir.pushExpr(.errorv, c.alloc, bt.Error, nodeId, .{ .name = name });
                return ExprResult.initStatic(loc, bt.Error);
            },
            .symbolLit => {
                const name = c.ast.nodeString(node);
                if (expr.hasTargetType() and c.sema.isEnumType(expr.target_t)) {
                    const sym = c.sema.getTypeSym(expr.target_t).cast(.enum_t);
                    if (sym.getMemberTag(name)) |tag| {
                        const irIdx = try c.ir.pushExpr(.enumMemberSym, c.alloc, expr.target_t, nodeId, .{
                            .type = expr.target_t,
                            .val = @as(u8, @intCast(tag)),
                        });
                        return ExprResult.initStatic(irIdx, expr.target_t);
                    }
                }
                const irIdx = try c.ir.pushExpr(.symbol, c.alloc, bt.Symbol, nodeId, .{ .name = name });
                return ExprResult.initStatic(irIdx, bt.Symbol);
            },
            .trueLit => {
                const irIdx = try c.ir.pushExpr(.truev, c.alloc, bt.Boolean, nodeId, {});
                return ExprResult.initStatic(irIdx, bt.Boolean);
            },
            .falseLit => return c.semaFalse(nodeId),
            .floatLit => {
                const literal = c.ast.nodeString(node);
                const val = try std.fmt.parseFloat(f64, literal);
                const irIdx = try c.ir.pushExpr(.float, c.alloc, bt.Float, nodeId, .{ .val = val });
                return ExprResult.initStatic(irIdx, bt.Float);
            },
            .decLit => {
                if (expr.target_t == bt.Float) {
                    const literal = c.ast.nodeString(node);
                    const val = try std.fmt.parseFloat(f64, literal);
                    return c.semaFloat(val, nodeId);
                } else {
                    const literal = c.ast.nodeString(node);
                    const val = try std.fmt.parseInt(u48, literal, 10);
                    return c.semaInt(val, nodeId);
                }
            },
            .binLit => {
                const literal = c.ast.nodeString(node);
                const val = try std.fmt.parseInt(u48, literal[2..], 2);
                const loc = try c.ir.pushExpr(.int, c.alloc, bt.Integer, nodeId, .{ .val = val });
                return ExprResult.initStatic(loc, bt.Integer);
            },
            .octLit => {
                const literal = c.ast.nodeString(node);
                const val = try std.fmt.parseInt(u48, literal[2..], 8);
                const loc = try c.ir.pushExpr(.int, c.alloc, bt.Integer, nodeId, .{ .val = val });
                return ExprResult.initStatic(loc, bt.Integer);
            },
            .hexLit => {
                const literal = c.ast.nodeString(node);
                const val = try std.fmt.parseInt(u48, literal[2..], 16);
                const loc = try c.ir.pushExpr(.int, c.alloc, bt.Integer, nodeId, .{ .val = val });
                return ExprResult.initStatic(loc, bt.Integer);
            },
            .ident => {
                return try semaIdent(c, nodeId, true);
            },
            .stringLit => return c.semaString(c.ast.nodeString(node), nodeId),
            .raw_string_lit => return c.semaRawString(c.ast.nodeString(node), nodeId),
            .runeLit => {
                const literal = c.ast.nodeString(node);
                if (literal.len == 0) {
                    return c.reportErrorFmt("Invalid UTF-8 Rune.", &.{}, nodeId);
                }
                var val: u48 = undefined;
                if (literal[0] == '\\') {
                    const res = try unescapeSeq(literal[1..]);
                    val = res.char;
                    if (literal.len > res.advance + 1) {
                        return c.reportErrorFmt("Invalid escape sequence.", &.{}, nodeId);
                    }
                } else {
                    const len = std.unicode.utf8ByteSequenceLength(literal[0]) catch {
                        return c.reportErrorFmt("Invalid UTF-8 Rune.", &.{}, nodeId);
                    };
                    if (literal.len != len) {
                        return c.reportErrorFmt("Invalid UTF-8 Rune.", &.{}, nodeId);
                    }
                    val = std.unicode.utf8Decode(literal[0..0+len]) catch {
                        return c.reportErrorFmt("Invalid UTF-8 Rune.", &.{}, nodeId);
                    };
                }
                const loc = try c.ir.pushExpr(.int, c.alloc, bt.Integer, nodeId, .{ .val = val });
                return ExprResult.initStatic(loc, bt.Integer);
            },
            .if_expr => {
                const ifBranch = c.ast.node(node.data.if_expr.if_branch);

                const cond = try c.semaExprCstr(ifBranch.data.if_branch.cond, bt.Boolean);
                const body = try c.semaExprCstr(ifBranch.data.if_branch.body_head, expr.target_t);
                const else_body = try c.semaExprCstr(node.data.if_expr.else_expr, expr.target_t);

                const loc = try c.ir.pushExpr(.if_expr, c.alloc, body.type.id, nodeId, .{
                    .cond = cond.irIdx,
                    .body = body.irIdx,
                    .elseBody = else_body.irIdx,
                });
                const dynamic = body.type.dynamic or else_body.type.dynamic;
                return ExprResult.init(loc, CompactType.init2(bt.Any, dynamic));
            },
            .castExpr => {
                const typeId = try resolveTypeSpecNode(c, node.data.castExpr.typeSpec);
                const child = try c.semaExpr(node.data.castExpr.expr, .{});
                const irIdx = try c.ir.pushExpr(.cast, c.alloc, typeId, nodeId, .{
                    .typeId = typeId, .isRtCast = true, .expr = child.irIdx,
                });
                if (!child.type.dynamic) {
                    // Compile-time cast.
                    if (cy.types.isTypeSymCompat(c.compiler, child.type.id, typeId)) {
                        c.ir.getExprDataPtr(irIdx, .cast).isRtCast = false;
                    } else {
                        // Check if it's a narrowing cast (deferred to runtime).
                        if (!cy.types.isTypeSymCompat(c.compiler, typeId, child.type.id)) {
                            const actTypeName = c.sema.getTypeBaseName(child.type.id);
                            const expTypeName = c.sema.getTypeBaseName(typeId);
                            return c.reportErrorFmt("Cast expects `{}`, got `{}`.", &.{v(expTypeName), v(actTypeName)}, nodeId);
                        }
                    }
                }
                return ExprResult.init(irIdx, CompactType.init(typeId));
            },
            .callExpr => {
                return try c.semaCallExpr(expr);
            },
            .expandOpt => {
                const sym = try cte.expandTemplateOnCallArgs(c, c.sema.option_tmpl, node.data.expandOpt.param, nodeId);
                const ctype = CompactType.initStatic(sym.getStaticType().?);
                return ExprResult.initCustom(cy.NullId, .sym, ctype, .{ .sym = sym });
            },
            .accessExpr => {
                return try c.semaAccessExpr(expr, true);
            },
            .unwrap => {
                const opt = try c.semaOptionExpr(node.data.unwrap.opt);
                const payload_t = if (opt.type.id == bt.Any) bt.Any else b: {
                    const type_sym = c.sema.getTypeSym(opt.type.id).cast(.enum_t);
                    const some = type_sym.getMemberByIdx(1);
                    break :b some.payloadType;
                };

                const loc = try c.ir.pushExpr(.unwrapChoice, c.alloc, payload_t, nodeId, .{
                    .choice = opt.irIdx,
                    .tag = 1,
                    .payload_t = payload_t,
                    .fieldIdx = 1,
                });
                return ExprResult.initInheritDyn(loc, opt.type, payload_t);
            },
            .unwrap_or => {
                const opt = try c.semaOptionExpr(node.data.unwrap_or.opt);
                const payload_t = if (opt.type.id == bt.Any) bt.Any else b: {
                    const type_sym = c.sema.getTypeSym(opt.type.id).cast(.enum_t);
                    const some = type_sym.getMemberByIdx(1);
                    break :b some.payloadType;
                };

                const default = try c.semaExprCstr(node.data.unwrap_or.default, payload_t);
                const loc = try c.ir.pushExpr(.unwrap_or, c.alloc, payload_t, nodeId, .{
                    .opt = opt.irIdx,
                    .default = default.irIdx,
                });
                return ExprResult.initInheritDyn(loc, opt.type, payload_t);
            },
            .array_expr => {
                const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, nodeId);

                const array = c.ast.node(node.data.array_expr.array);
                if (array.data.arrayLit.numArgs != 1) {
                    return c.reportErrorFmt("Unsupported array expr.", &.{}, node.data.array_expr.array);
                }

                const left = try c.semaExpr(node.data.array_expr.left, .{});
                const leftT = left.type.id;
                const preferT = if (leftT == bt.List or leftT == bt.Tuple) bt.Integer else bt.Any;
                const index = try c.semaExprTarget(array.data.arrayLit.argHead, preferT);

                if (left.type.isDynAny()) {
                    return c.semaCallObjSym2(loc, left.irIdx, getBinOpName(.index), &.{index});
                }

                // TODO: codegen should determine whether it is specialized, not here.
                var specialized = false;
                if ((leftT == bt.List or leftT == bt.Tuple) and (index.type.id == bt.Integer or index.type.id == bt.Range)) {
                    specialized = true;
                } else if (leftT == bt.Map) {
                    specialized = true;
                }
                if (specialized) {
                    // Specialized.
                    var res_t = CompactType.initDynamic(bt.Any);
                    if (leftT == bt.List and index.type.id == bt.Range) {
                        res_t = CompactType.initStatic(bt.List);
                    }

                    c.ir.setExprCode(loc, .preBinOp);
                    c.ir.setExprType(loc, res_t.id);
                    c.ir.setExprData(loc, .preBinOp, .{ .binOp = .{
                        .leftT = leftT,
                        .rightT = index.type.id,
                        .op = .index,
                        .left = left.irIdx,
                        .right = index.irIdx,
                    }});

                    return ExprResult.init(loc, res_t);
                } else {
                    // Look for sym under left type's module.
                    const recTypeSym = c.sema.getTypeSym(leftT);
                    const sym = try c.mustFindSym(recTypeSym, "$index", nodeId);
                    const func_sym = try requireFuncSym(c, sym, nodeId);

                    return c.semaCallFuncSym2(loc, func_sym, node.data.array_expr.left, left,
                        array.data.arrayLit.argHead, index, expr.getRetCstr(), nodeId);
                }
            },
            .range => {
                var start: u32 = cy.NullId;
                if (node.data.range.start != cy.NullNode) {
                    const start_res = try c.semaExprCstr(node.data.range.start, bt.Integer);
                    start = start_res.irIdx;
                }
                var end: u32 = cy.NullId;
                if (node.data.range.end != cy.NullNode) {
                    const end_res = try c.semaExprCstr(node.data.range.end, bt.Integer);
                    end = end_res.irIdx;
                }
                const loc = try c.ir.pushExpr(.range, c.alloc, bt.Range, nodeId, .{
                    .start = start,
                    .end = end,
                    .inc = node.data.range.inc,
                });
                return ExprResult.initStatic(loc, bt.Range);
            },
            .binExpr => {
                const left = node.data.binExpr.left;
                const right = node.data.binExpr.right;
                return try c.semaBinExpr(expr, left, node.data.binExpr.op, right);
            },
            .unary_expr => {
                return try c.semaUnExpr(expr);
            },
            .arrayLit => {
                const numArgs = node.data.arrayLit.numArgs;
                const irIdx = try c.ir.pushEmptyExpr(.list, c.alloc, ir.ExprType.init(bt.List), nodeId);
                const irArgsIdx = try c.ir.pushEmptyArray(c.alloc, u32, numArgs);

                var argId = node.data.arrayLit.argHead;
                var i: u32 = 0;
                while (argId != cy.NullNode) {
                    var arg = c.ast.node(argId);
                    const argRes = try c.semaExpr(argId, .{});
                    c.ir.setArrayItem(irArgsIdx, u32, i, argRes.irIdx);
                    argId = arg.next();
                    i += 1;
                }

                c.ir.setExprData(irIdx, .list, .{ .numArgs = numArgs });
                return ExprResult.initStatic(irIdx, bt.List);
            },
            .recordLit => {
                if (expr.hasTargetType()) {
                    if (c.sema.isUserObjectType(expr.target_t)) {
                        // Infer user object type.
                        const obj = c.sema.getTypeSym(expr.target_t).cast(.object_t);
                        return c.semaObjectInit2(obj, nodeId);
                    } else if (c.sema.isStructType(expr.target_t)) {
                        const obj = c.sema.getTypeSym(expr.target_t).cast(.struct_t);
                        return c.semaObjectInit2(obj, nodeId);
                    }
                }

                const numArgs = node.data.recordLit.numArgs;
                const irIdx = try c.ir.pushEmptyExpr(.map, c.alloc, ir.ExprType.init(bt.Map), nodeId);
                const irKeysIdx = try c.ir.pushEmptyArray(c.alloc, []const u8, numArgs);
                const irArgsIdx = try c.ir.pushEmptyArray(c.alloc, u32, numArgs);

                var i: u32 = 0;
                var argId = node.data.recordLit.argHead;
                while (argId != cy.NullNode) {
                    const arg = c.ast.node(argId);
                    const key = c.ast.node(arg.data.keyValue.key);
                    switch (key.type()) {
                        .ident => {
                            const name = c.ast.nodeString(key);
                            c.ir.setArrayItem(irKeysIdx, []const u8, i, name);
                        },
                        .raw_string_lit => {
                            const name = c.ast.nodeString(key);
                            c.ir.setArrayItem(irKeysIdx, []const u8, i, name);
                        },
                        else => cy.panicFmt("Unsupported key {}", .{key.type()}),
                    }
                    const argRes = try c.semaExpr(arg.data.keyValue.value, .{});
                    c.ir.setArrayItem(irArgsIdx, u32, i, argRes.irIdx);
                    i += 1;
                    argId = arg.next();
                }

                c.ir.setExprData(irIdx, .map, .{ .numArgs = numArgs, .args = irArgsIdx });
                return ExprResult.initStatic(irIdx, bt.Map);
            },
            .stringTemplate => {
                const numExprs = node.data.stringTemplate.numExprs;
                const irIdx = try c.ir.pushEmptyExpr(.stringTemplate, c.alloc, ir.ExprType.init(bt.String), nodeId);
                const irStrsIdx = try c.ir.pushEmptyArray(c.alloc, []const u8, numExprs+1);
                const irArgsIdx = try c.ir.pushEmptyArray(c.alloc, u32, numExprs);

                var i: u32 = 0;
                var curId: cy.NodeId = node.data.stringTemplate.strHead;
                while (curId != cy.NullNode) {
                    const str = c.ast.node(curId);
                    c.ir.setArrayItem(irStrsIdx, []const u8, i, c.ast.nodeString(str));
                    curId = str.next();
                    i += 1;
                }

                i = 0;
                curId = node.data.stringTemplate.exprHead;
                while (curId != cy.NullNode) {
                    var exprN = c.ast.node(curId);
                    const argRes = try c.semaExpr(curId, .{});
                    c.ir.setArrayItem(irArgsIdx, u32, i, argRes.irIdx);
                    curId = exprN.next();
                    i += 1;
                }

                c.ir.setExprData(irIdx, .stringTemplate, .{ .numExprs = numExprs, .args = irArgsIdx });
                return ExprResult.initStatic(irIdx, bt.String);
            },
            .group => {
                return c.semaExpr(node.data.group.child, .{});
            },
            .lambda_expr => {
                const decl = try resolveLambdaDecl(c, @ptrCast(c.sym), nodeId);
                const func = try c.addUserLambda(@ptrCast(c.sym), decl.funcSigId, nodeId);
                _ = try pushLambdaProc(c, func);
                const irIdx = c.proc().irStart;

                // Generate function body.
                try appendFuncParamVars(c, func);

                const exprRes = try c.semaExpr(node.data.func.bodyHead, .{});
                _ = try c.ir.pushStmt(c.alloc, .retExprStmt, nodeId, .{
                    .expr = exprRes.irIdx,
                });

                try popLambdaProc(c);

                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .lambda_multi => {
                const decl = try resolveLambdaDecl(c, @ptrCast(c.sym), nodeId);
                const func = try c.addUserLambda(@ptrCast(c.sym), decl.funcSigId, nodeId);
                _ = try pushLambdaProc(c, func);
                const irIdx = c.proc().irStart;

                // Generate function body.
                try appendFuncParamVars(c, func);
                try semaStmts(c, node.data.func.bodyHead);

                try popLambdaProc(c);

                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .record_expr => {
                return c.semaObjectInit(expr);
            },
            .throwExpr => {
                const child = try c.semaExpr(node.data.throwExpr.child, .{});
                const irIdx = try c.ir.pushExpr(.throw, c.alloc, bt.Any, nodeId, .{ .expr = child.irIdx });
                return ExprResult.initDynamic(irIdx, bt.Any);
            },
            .tryExpr => {
                var catchError = false;
                if (node.data.tryExpr.catchExpr != cy.NullNode) {
                    const catchExpr = c.ast.node(node.data.tryExpr.catchExpr);
                    if (catchExpr.type() == .ident) {
                        const name = c.ast.nodeString(catchExpr);
                        if (std.mem.eql(u8, "error", name)) {
                            catchError = true;
                        }
                    }
                } else {
                    catchError = true;
                }
                const irIdx = try c.ir.pushEmptyExpr(.tryExpr, c.alloc, ir.ExprType.init(bt.Any), nodeId);

                if (catchError) {
                    const child = try c.semaExpr(node.data.tryExpr.expr, .{});
                    c.ir.setExprData(irIdx, .tryExpr, .{ .expr = child.irIdx, .catchBody = cy.NullId });
                    const unionT = cy.types.unionOf(c.compiler, child.type.id, bt.Error);
                    return ExprResult.init(irIdx, CompactType.init2(unionT, child.type.dynamic));
                } else {
                    const child = try c.semaExpr(node.data.tryExpr.expr, .{});
                    const catchExpr = try c.semaExpr(node.data.tryExpr.catchExpr, .{});
                    c.ir.setExprData(irIdx, .tryExpr, .{ .expr = child.irIdx, .catchBody = catchExpr.irIdx });
                    const dynamic = catchExpr.type.dynamic or child.type.dynamic;
                    const unionT = cy.types.unionOf(c.compiler, child.type.id, catchExpr.type.id);
                    return ExprResult.init(irIdx, CompactType.init2(unionT, dynamic));
                }
            },
            .comptimeExpr => {
                const child = c.ast.node(node.data.comptimeExpr.child);
                if (child.type() == .ident) {
                    const name = c.ast.nodeString(child);
                    if (std.mem.eql(u8, name, "modUri")) {
                        return c.semaRawString(c.srcUri, nodeId);
                    } else {
                        return c.reportErrorFmt("Compile-time symbol does not exist: {}", &.{v(name)}, node.data.comptimeExpr.child);
                    }
                } else {
                    return c.reportErrorFmt("Unsupported compile-time expr: {}", &.{v(child.type())}, node.data.comptimeExpr.child);
                }
            },
            .coinit => {
                const irIdx = try c.ir.pushExpr(.coinitCall, c.alloc, bt.Fiber, nodeId, {});

                const irCallIdx = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, nodeId);
                const callExpr = c.ast.node(node.data.coinit.child);

                const callee = try c.semaExprSkipSym(callExpr.data.callExpr.callee);

                // Callee is already pushed as a value or is a symbol.
                if (callee.resType == .sym) {
                    const sym = callee.data.sym;
                    _ = try callSym(c, irCallIdx, sym, callExpr.data.callExpr.numArgs,
                        callExpr.data.callExpr.callee, callExpr.data.callExpr.argHead, expr.getRetCstr());
                } else {
                    // preCall.
                    const numArgs = callExpr.data.callExpr.numArgs;
                    const args = try c.semaPushCallArgs(callExpr.data.callExpr.argHead, numArgs);
                    _ = try c.semaCallValue(irCallIdx, callee.irIdx, numArgs, args);
                }

                return ExprResult.initStatic(irIdx, bt.Fiber);
            },
            .coyield => {
                const irIdx = try c.ir.pushExpr(.coyield, c.alloc, bt.Any, nodeId, {});
                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .coresume => {
                const child = try c.semaExpr(node.data.coresume.child, .{});
                const irIdx = try c.ir.pushExpr(.coresume, c.alloc, bt.Any, nodeId, .{
                    .expr = child.irIdx,
                });
                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .switchExpr => {
                return semaSwitchExpr(c, nodeId);
            },
            else => {
                return c.reportErrorFmt("Unsupported node: {}", &.{v(node.type())}, nodeId);
            },
        }
    }

    pub fn semaCallExpr(c: *cy.Chunk, expr: Expr) !ExprResult {
        const node = c.ast.node(expr.nodeId);
        const callee = c.ast.node(node.data.callExpr.callee);
        const numArgs = node.data.callExpr.numArgs;

        if (node.data.callExpr.hasNamedArg) {
            return c.reportErrorFmt("Unsupported named args.", &.{}, expr.nodeId);
        }

        // pre is later patched with the type of call.
        const preIdx = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, expr.nodeId);

        if (callee.type() == .accessExpr) {
            const leftRes = try c.semaExprSkipSym(callee.data.accessExpr.left);
            const rightId = callee.data.accessExpr.right;
            const accessRight = c.ast.node(rightId);
            if (accessRight.type() != .ident) {
                return error.Unexpected;
            }

            if (leftRes.resType == .sym) {
                const leftSym = leftRes.data.sym;

                if (leftSym.type == .typeTemplate) {
                    const final_sym = try cte.expandTemplateOnCallArgs(c, leftSym.cast(.typeTemplate), node.data.callExpr.argHead, expr.nodeId);
                    const ctype = CompactType.initStatic(final_sym.getStaticType().?);
                    return ExprResult.initCustom(cy.NullId, .sym, ctype, .{ .sym = final_sym });
                }

                if (leftSym.type == .func) {
                    return c.reportErrorFmt("Can not access function symbol `{}`.", &.{
                        v(c.ast.nodeStringById(callee.data.accessExpr.left))}, callee.data.accessExpr.right);
                }
                const right = c.ast.node(rightId);
                const rightName = c.ast.nodeString(right);

                if (leftRes.type.dynamic) {
                    // Runtime method call.
                    const recv = try sema.symbol(c, leftSym, callee.data.accessExpr.left, true);
                    const args = try c.semaPushCallArgs(node.data.callExpr.argHead, numArgs);
                    return c.semaCallObjSym(preIdx, recv.irIdx, rightId, numArgs, args);
                }

                if (leftSym.isVariable()) {
                    // Look for sym under left type's module.
                    const leftTypeSym = c.sema.getTypeSym(leftRes.type.id);
                    const rightSym = try c.mustFindSym(leftTypeSym, rightName, rightId);
                    const func_sym = try requireFuncSym(c, rightSym, rightId);
                    const recv = try sema.symbol(c, leftSym, callee.data.accessExpr.left, true);
                    return c.semaCallFuncSymRec(preIdx, func_sym, callee.data.accessExpr.left, recv,
                        node.data.callExpr.argHead, numArgs, expr.getRetCstr(), expr.nodeId);
                } else {
                    // Look for sym under left module.
                    const rightSym = try c.mustFindSym(leftSym, rightName, rightId);
                    return try callSym(c, preIdx, rightSym, numArgs, rightId, node.data.callExpr.argHead, expr.getRetCstr());
                }
            } else {
                if (leftRes.type.dynamic) {
                    // preCallObjSym.
                    const args = try c.semaPushCallArgs(node.data.callExpr.argHead, numArgs);
                    return c.semaCallObjSym(preIdx, leftRes.irIdx, rightId, numArgs, args);
                } else {
                    // Look for sym under left type's module.
                    const rightName = c.ast.nodeStringById(rightId);
                    const leftTypeSym = c.sema.getTypeSym(leftRes.type.id);
                    const rightSym = try c.mustFindSym(leftTypeSym, rightName, rightId);
                    const func_sym = try requireFuncSym(c, rightSym, rightId);

                    return c.semaCallFuncSymRec(preIdx, func_sym, callee.data.accessExpr.left, leftRes,
                        node.data.callExpr.argHead, numArgs, expr.getRetCstr(), expr.nodeId);
                }
            }
        } else if (callee.type() == .ident) {
            const name = c.ast.nodeString(callee);

            const varRes = try getOrLookupVar(c, name, true, node.data.callExpr.callee);
            switch (varRes) {
                .local => |_| {
                    // preCall.
                    const calleeRes = try c.semaExpr(node.data.callExpr.callee, .{});
                    const args = try c.semaPushCallArgs(node.data.callExpr.argHead, numArgs);
                    return c.semaCallValue(preIdx, calleeRes.irIdx, numArgs, args);
                },
                .static => |sym| {
                    if (sym.type == .typeTemplate) {
                        const final_sym = try cte.expandTemplateOnCallArgs(c, sym.cast(.typeTemplate), node.data.callExpr.argHead, expr.nodeId);
                        const ctype = CompactType.initStatic(final_sym.getStaticType().?);
                        return ExprResult.initCustom(cy.NullId, .sym, ctype, .{ .sym = final_sym });
                    }

                    return callSym(c, preIdx, sym, numArgs, node.data.callExpr.callee, node.data.callExpr.argHead, expr.getRetCstr());
                },
            }
        } else {
            // preCall.
            const calleeRes = try c.semaExpr(node.data.callExpr.callee, .{});
            const args = try c.semaPushCallArgs(node.data.callExpr.argHead, numArgs);
            return c.semaCallValue(preIdx, calleeRes.irIdx, numArgs, args);
        }
    }

    /// Expr or construct binExpr from opAssignStmt.
    pub fn semaExprOrOpAssignBinExpr(c: *cy.Chunk, expr: Expr, opAssignBinExpr: bool) !ExprResult {
        if (!opAssignBinExpr) {
            return try c.semaExpr2(expr);
        } else {
            const opAssign = c.ast.node(expr.nodeId);
            const left = opAssign.data.opAssignStmt.left;
            const op = opAssign.data.opAssignStmt.op;
            const right = opAssign.data.opAssignStmt.right;
            return try c.semaBinExpr(expr, left, op, right);
        }
    }

    pub fn semaString(c: *cy.Chunk, lit: []const u8, nodeId: cy.NodeId) !ExprResult {
        const raw = try c.unescapeString(lit);
        if (raw.ptr != lit.ptr) {
            // Dupe and track in ast.strs.
            const dupe = try c.alloc.dupe(u8, raw);
            try c.parser.ast.strs.append(c.alloc, dupe);
            return c.semaRawString(dupe, nodeId);
        } else {
            return c.semaRawString(raw, nodeId);
        }
    }

    pub fn semaRawString(c: *cy.Chunk, raw: []const u8, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.ir.pushExpr(.string, c.alloc, bt.String, nodeId, .{ .raw = raw });
        return ExprResult.initStatic(irIdx, bt.String);
    }

    pub fn semaArray(c: *cy.Chunk, arr: []const u8, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.ir.pushExpr(.array, c.alloc, bt.Array, nodeId, .{ .buffer = arr });
        return ExprResult.initStatic(irIdx, bt.Array);
    }

    pub fn semaFloat(c: *cy.Chunk, val: f64, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.ir.pushExpr(.float, c.alloc, bt.Float, nodeId, .{ .val = val });
        return ExprResult.initStatic(irIdx, bt.Float);
    }

    pub fn semaInt(c: *cy.Chunk, val: u48, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.ir.pushExpr(.int, c.alloc, bt.Integer, nodeId, .{ .val = val });
        return ExprResult.initStatic(irIdx, bt.Integer);
    }

    pub fn semaFalse(c: *cy.Chunk, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.ir.pushExpr(.falsev, c.alloc, bt.Boolean, nodeId, {});
        return ExprResult.initStatic(irIdx, bt.Boolean);
    }

    pub fn semaNone(c: *cy.Chunk, preferType: cy.TypeId, nodeId: cy.NodeId) !ExprResult {
        const type_e = c.sema.types.items[preferType];
        if (type_e.kind == .option) {
            // Generate IR to wrap value into optional.
            var b: ObjectBuilder = .{ .c = c };
            try b.begin(preferType, 2, nodeId);
            const tag = try c.semaInt(0, nodeId);
            b.pushArg(tag);
            const payload = try c.semaInt(0, nodeId);
            b.pushArg(payload);
            const loc = b.end();

            return ExprResult.initStatic(loc, preferType);
        } else {
            const name = type_e.sym.name();
            return c.reportErrorFmt("Expected `Option(T)` to infer `none` value, found `{}`.", &.{v(name)}, nodeId);
        }
    }

    pub fn semaEmptyList(c: *cy.Chunk, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.ir.pushExpr(.list, c.alloc, bt.List, nodeId, .{ .numArgs = 0 });
        return ExprResult.initStatic(irIdx, bt.List);
    }

    pub fn semaEmptyMap(c: *cy.Chunk, nodeId: cy.NodeId) !ExprResult {
        const irIdx = try c.ir.pushExpr(.map, c.alloc, bt.Map, nodeId, .{ .numArgs = 0, .args = 0 });
        return ExprResult.initStatic(irIdx, bt.Map);
    }

    pub fn semaCallValue(c: *cy.Chunk, preIdx: u32, calleeLoc: u32, numArgs: u32, argsLoc: u32) !ExprResult {
        // Dynamic call.
        c.ir.setExprCode(preIdx, .preCallDyn);
        c.ir.setExprType(preIdx, bt.Any);
        c.ir.setExprData(preIdx, .preCallDyn, .{ .callDyn = .{
            .callee = calleeLoc,
            .numArgs = @as(u8, @intCast(numArgs)),
            .args = argsLoc,
        }});
        return ExprResult.initDynamic(preIdx, bt.Any);
    }

    pub fn semaCallObjSym(c: *cy.Chunk, preIdx: u32, recLoc: u32, right: cy.NodeId, num_args: u8, irArgsIdx: u32) !ExprResult {
        // Dynamic method call.
        const name = c.ast.nodeStringById(right);

        c.ir.setExprCode(preIdx, .preCallObjSym);
        c.ir.setExprData(preIdx, .preCallObjSym, .{ .callObjSym = .{
            .name = name,
            .numArgs = num_args,
            .rec = recLoc,
            .args = irArgsIdx,
        }});
        return ExprResult.initDynamic(preIdx, bt.Any);
    }

    pub fn semaCallObjSym2(c: *cy.Chunk, preIdx: u32, recLoc: u32, name: []const u8, arg_exprs: []const ExprResult) !ExprResult {
        const args_loc = try c.ir.pushEmptyArray(c.alloc, u32, arg_exprs.len);
        for (arg_exprs, 0..) |expr, i| {
            c.ir.setArrayItem(args_loc, u32, i, expr.irIdx);
        }

        c.ir.setExprCode(preIdx, .preCallObjSym);
        c.ir.setExprData(preIdx, .preCallObjSym, .{ .callObjSym = .{
            .name = name,
            .numArgs = @as(u8, @intCast(arg_exprs.len)),
            .rec = recLoc,
            .args = args_loc,
        }});
        return ExprResult.initDynamic(preIdx, bt.Any);
    }

    pub fn semaUnExpr(c: *cy.Chunk, expr: Expr) !ExprResult {
        const nodeId = expr.nodeId;
        const node = c.ast.node(nodeId);

        const op = node.data.unary.op;
        switch (op) {
            .minus => {
                const irIdx = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, nodeId);

                const child = try c.semaExprTarget(node.data.unary.child, expr.target_t);
                if (child.type.isDynAny()) {
                    return c.semaCallObjSym2(irIdx, child.irIdx, getUnOpName(.minus), &.{});
                }

                if (child.type.id == bt.Integer or child.type.id == bt.Float) {
                    // Specialized.
                    c.ir.setExprCode(irIdx, .preUnOp);
                    c.ir.setExprData(irIdx, .preUnOp, .{ .unOp = .{
                        .childT = child.type.id, .op = op, .expr = child.irIdx,
                    }});
                    return ExprResult.initStatic(irIdx, child.type.id);
                } else {
                    // Look for sym under child type's module.
                    const childTypeSym = c.sema.getTypeSym(child.type.id);
                    const sym = try c.mustFindSym(childTypeSym, op.name(), nodeId);
                    const func_sym = try requireFuncSym(c, sym, nodeId);
                    return c.semaCallFuncSym1(irIdx, func_sym, node.data.unary.child, child, expr.getRetCstr(), nodeId);
                }
            },
            .not => {
                const irIdx = try c.ir.pushEmptyExpr(.preUnOp, c.alloc, undefined, nodeId);

                const child = try c.semaExprCstr(node.data.unary.child, bt.Boolean);
                c.ir.setExprData(irIdx, .preUnOp, .{ .unOp = .{
                    .childT = child.type.id, .op = op, .expr = child.irIdx,
                }});
                return ExprResult.initStatic(irIdx, bt.Boolean);
            },
            .bitwiseNot => {
                const irIdx = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, nodeId);

                const child = try c.semaExprTarget(node.data.unary.child, expr.target_t);
                if (child.type.isDynAny()) {
                    return c.semaCallObjSym2(irIdx, child.irIdx, getUnOpName(.bitwiseNot), &.{});
                }

                if (child.type.id == bt.Integer) {
                    c.ir.setExprCode(irIdx, .preUnOp);
                    c.ir.setExprData(irIdx, .preUnOp, .{ .unOp = .{
                        .childT = child.type.id, .op = op, .expr = child.irIdx,
                    }});
                    return ExprResult.initStatic(irIdx, bt.Float);
                } else {
                    // Look for sym under child type's module.
                    const childTypeSym = c.sema.getTypeSym(child.type.id);
                    const sym = try c.mustFindSym(childTypeSym, op.name(), nodeId);
                    const func_sym = try requireFuncSym(c, sym, nodeId);
                    return c.semaCallFuncSym1(irIdx, func_sym, node.data.unary.child, child, expr.getRetCstr(), nodeId);
                }
            },
            else => return c.reportErrorFmt("Unsupported unary op: {}", &.{v(op)}, nodeId),
        }
    }

    pub fn semaBinExpr(c: *cy.Chunk, expr: Expr, leftId: cy.NodeId, op: cy.BinaryExprOp, rightId: cy.NodeId) !ExprResult {
        const nodeId = expr.nodeId;

        switch (op) {
            .and_op,
            .or_op => {
                const loc = try c.ir.pushEmptyExpr(.preBinOp, c.alloc, ir.ExprType.init(bt.Boolean), nodeId);
                const left = try c.semaExprCstr(leftId, bt.Boolean);
                const right = try c.semaExprCstr(rightId, bt.Boolean);
                c.ir.setExprData(loc, .preBinOp, .{ .binOp = .{
                    .leftT = left.type.id,
                    .rightT = right.type.id,
                    .op = op,
                    .left = left.irIdx,
                    .right = right.irIdx,
                }});

                const dynamic = right.type.dynamic or left.type.dynamic;
                return ExprResult.init(loc, CompactType.init2(bt.Boolean, dynamic));
            },
            .bitwiseAnd,
            .bitwiseOr,
            .bitwiseXor,
            .bitwiseLeftShift,
            .bitwiseRightShift => {
                const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, nodeId);

                const left = try c.semaExprTarget(leftId, expr.target_t);
                const right = try c.semaExprTarget(rightId, left.type.id);

                if (left.type.isDynAny()) {
                    return c.semaCallObjSym2(loc, left.irIdx, getBinOpName(op), &.{right});
                }

                if (left.type.id == right.type.id and left.type.id == bt.Integer) {
                    // Specialized.
                    c.ir.setExprCode(loc, .preBinOp);
                    c.ir.setExprType(loc, bt.Integer);
                    c.ir.setExprData(loc, .preBinOp, .{ .binOp = .{
                        .leftT = bt.Integer,
                        .rightT = right.type.id,
                        .op = op,
                        .left = left.irIdx,
                        .right = right.irIdx,
                    }});
                    return ExprResult.initStatic(loc, bt.Integer);
                } else {
                    // Look for sym under left type's module.
                    const leftTypeSym = c.sema.getTypeSym(left.type.id);
                    const sym = try c.mustFindSym(leftTypeSym, op.name(), nodeId);
                    const funcSym = try requireFuncSym(c, sym, nodeId);
                    return c.semaCallFuncSym2(loc, funcSym, leftId, left, rightId, right, expr.getRetCstr(), nodeId);
                }
            },
            .greater,
            .greater_equal,
            .less,
            .less_equal => {
                const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, nodeId);

                const left = try c.semaExprTarget(leftId, expr.target_t);
                const right = try c.semaExprTarget(rightId, left.type.id);

                if (left.type.isDynAny()) {
                    return c.semaCallObjSym2(loc, left.irIdx, getBinOpName(op), &.{right});
                }

                if (left.type.id == right.type.id and (left.type.id == bt.Float or left.type.id == bt.Integer)) {
                    // Specialized.
                    c.ir.setExprCode(loc, .preBinOp);
                    c.ir.setExprType(loc, bt.Boolean);
                    c.ir.setExprData(loc, .preBinOp, .{ .binOp = .{
                        .leftT = left.type.id,
                        .rightT = right.type.id,
                        .op = op,
                        .left = left.irIdx,
                        .right = right.irIdx,
                    }});
                    return ExprResult.initStatic(loc, bt.Boolean);
                } else {
                    // Look for sym under left type's module.
                    const leftTypeSym = c.sema.getTypeSym(left.type.id);
                    const sym = try c.mustFindSym(leftTypeSym, op.name(), nodeId);
                    const funcSym = try requireFuncSym(c, sym, nodeId);
                    return c.semaCallFuncSym2(loc, funcSym, leftId, left, rightId, right, expr.getRetCstr(), nodeId);
                }
            },
            .star,
            .slash,
            .percent,
            .caret,
            .plus,
            .minus => {
                const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, nodeId);

                const left = try c.semaExprTarget(leftId, expr.target_t);
                const right = try c.semaExprTarget(rightId, left.type.id);

                if (left.type.isDynAny()) {
                    return c.semaCallObjSym2(loc, left.irIdx, getBinOpName(op), &.{right});
                }

                if (left.type.id == right.type.id and (left.type.id == bt.Float or left.type.id == bt.Integer)) {
                    // Specialized.
                    c.ir.setExprCode(loc, .preBinOp);
                    c.ir.setExprType(loc, left.type.id);
                    c.ir.setExprData(loc, .preBinOp, .{ .binOp = .{
                        .leftT = left.type.id,
                        .rightT = right.type.id,
                        .op = op,
                        .left = left.irIdx,
                        .right = right.irIdx,
                    }});
                    return ExprResult.initStatic(loc, left.type.id);
                } else {
                    // Look for sym under left type's module.
                    const leftTypeSym = c.sema.getTypeSym(left.type.id);
                    const sym = try c.mustFindSym(leftTypeSym, op.name(), nodeId);
                    const func_sym = try requireFuncSym(c, sym, nodeId);
                    return c.semaCallFuncSym2(loc, func_sym, leftId, left, rightId, right, expr.getRetCstr(), nodeId);
                }
            },
            .bang_equal,
            .equal_equal => {
                const leftPreferT = if (expr.target_t == bt.Float or expr.target_t == bt.Integer) expr.target_t else bt.Any;

                const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, nodeId);

                const left = try c.semaExprTarget(leftId, leftPreferT);
                const right = try c.semaExprTarget(rightId, left.type.id);

                if (left.type.id == right.type.id) {
                    const left_te = c.sema.types.items[left.type.id];
                    if (left_te.kind == .option or left_te.kind == .@"struct") {
                        return semaStructCompare(c, left, leftId, op, right, rightId, left_te, nodeId);
                    }
                }

                c.ir.setExprCode(loc, .preBinOp);
                c.ir.setExprType(loc, bt.Boolean);
                c.ir.setExprData(loc, .preBinOp, .{ .binOp = .{
                    .leftT = left.type.id,
                    .rightT = right.type.id,
                    .op = op,
                    .left = left.irIdx,
                    .right = right.irIdx,
                }});
                return ExprResult.initStatic(loc, bt.Boolean);
            },
            else => return c.reportErrorFmt("Unsupported binary op: {}", &.{v(op)}, nodeId),
        }
    }

    pub fn semaAccessExpr(c: *cy.Chunk, expr: Expr, symAsValue: bool) !ExprResult {
        const node = c.ast.node(expr.nodeId);
        const right = c.ast.node(node.data.accessExpr.right);

        if (right.type() != .ident) {
            return error.Unexpected;
        }

        var left = c.ast.node(node.data.accessExpr.left);
        if (left.type() == .ident) {
            const name = c.ast.nodeString(left);
            const vres = try getOrLookupVar(c, name, true, node.data.accessExpr.left);

            switch (vres) {
                .local => |id| {
                    // Local: [local].[right]
                    const rec = try semaLocal(c, id, node.data.accessExpr.left);
                    return semaAccessField(c, rec, node.data.accessExpr.right);
                },
                .static => |crLeftSym| {
                    // Static symbol: [sym].[right]
                    try referenceSym(c, crLeftSym, node.data.accessExpr.left);

                    const res = try semaSymAccessRight(c, crLeftSym, node.data.accessExpr.left, node.data.accessExpr.right);
                    switch (res.type) {
                        .sym => {
                            if (!symAsValue) {
                                const sym = res.data.sym;
                                var typeId: CompactType = undefined;
                                if (sym.isType()) {
                                    typeId = CompactType.init(sym.getStaticType().?);
                                } else {
                                    typeId = CompactType.init((try sym.getValueType()) orelse bt.Void);
                                }
                                return ExprResult.initCustom(cy.NullId, .sym, typeId, .{ .sym = res.data.sym });
                            } else {
                                return try sema.symbol(c, res.data.sym, node.data.accessExpr.right, true);
                            }
                        },
                        .field => {
                            return res.data.field;
                        },
                    }
                },
            }
    //     } else if (left.type() == .accessExpr) {
    //         const res = try accessExpr(self, node.data.accessExpr.left);

    //         left = self.nodes[node.data.accessExpr.left];
    //         if (left.data.accessExpr.sema_csymId.isPresent()) {
    //             // Static var.
    //             const crLeftSym = left.data.accessExpr.sema_csymId;
    //             if (!crLeftSym.isFuncSymId) {
    //                 const rightName = self.getNodeString(right);
    //                 const rightNameId = try ensureNameSym(self.compiler, rightName);
    //                 if (try getOrResolveDistinctSym(self, crLeftSym.id, rightNameId)) |rightSym| {
    //                     const crRightSym = rightSym.toCompactId();
    //                     try referenceSym(self, crRightSym, true);
    //                     self.nodes[nodeId].data.accessExpr.sema_csymId = crRightSym;
    //                 }
    //             }
    //         } else {
    //             const rightName = self.getNodeString(right);
    //             return getAccessExprResult(self, res.exprT, rightName);
    //         }
    //     } else {
        } else {
            const rec = try c.semaExpr(node.data.accessExpr.left, .{});
            return semaAccessField(c, rec, node.data.accessExpr.right);
        }
    }
};

fn semaWithInitPairs(c: *cy.Chunk, type_sym: *cy.Sym, type_id: cy.TypeId, record_id: cy.NodeId, init: u32) !ExprResult {
    const init_pair = type_sym.getMod().?.getSym("$initPair").?;
    const record = c.ast.node(record_id);

    const expr = try c.ir.pushExpr(.blockExpr, c.alloc, type_id, record_id, .{ .bodyHead = cy.NullId });
    try pushBlock(c, record_id);
    {
        // create temp with object.
        const var_id = try declareLocalName(c, "$temp", type_id, true, record_id);
        const temp_ir_id = c.varStack.items[var_id].inner.local.id;
        const temp_ir = c.varStack.items[var_id].inner.local.declIrStart;
        const decl_stmt = c.ir.getStmtDataPtr(temp_ir, .declareLocalInit);
        decl_stmt.init = init;
        decl_stmt.initType = CompactType.initStatic(type_id);

        // call $initPair for each record pair.
        const temp_loc = try c.ir.pushExpr(.local, c.alloc, type_id, record_id, .{ .id = temp_ir_id });
        const temp_expr = ExprResult.init(temp_loc, CompactType.initStatic(type_id));
        var entry_id = record.data.recordLit.argHead;
        while (entry_id != cy.NullNode) {
            const entry = c.ast.node(entry_id);

            const key = c.ast.node(entry.data.keyValue.key);
            const key_name = c.ast.nodeString(key);
            const key_expr = try c.semaString(key_name, entry.data.keyValue.key);
            const val_expr = try c.semaExpr(entry.data.keyValue.value, .{});

            const call = try c.semaCallFuncSymN(init_pair.cast(.func),
                &.{record_id, entry.data.keyValue.key, entry.data.keyValue.value},
                &.{temp_expr, key_expr, val_expr},
                .any, entry_id);

            _ = try c.ir.pushStmt(c.alloc, .exprStmt, entry_id, .{
                .expr = call.irIdx,
                .isBlockResult = false,
            });

            entry_id = entry.next();
        }

        // return temp.
        _ = try c.ir.pushStmt(c.alloc, .exprStmt, record_id, .{
            .expr = temp_loc,
            .isBlockResult = true,
        });
    }
    const stmtBlock = try popBlock(c);
    c.ir.getExprDataPtr(expr, .blockExpr).bodyHead = stmtBlock.first;
    return ExprResult.initStatic(expr, type_id);
}

fn semaStructCompare(c: *cy.Chunk, left: ExprResult, left_id: cy.NodeId, op: cy.BinaryExprOp,
    right: ExprResult, right_id: cy.NodeId, left_te: cy.types.Type, node_id: cy.NodeId) !ExprResult {

    // Struct memberwise comparison.
    const fields = left_te.sym.getFields().?;

    var field_t = c.sema.getTypeSym(fields[0].type);
    var it: u32 = undefined;
    if (field_t.type != .int_t and field_t.type != .object_t) {
        return error.Unsupported;
    } else {
        const left_f = try c.ir.pushExpr(.field, c.alloc, fields[0].type, left_id, .{
            .idx = 0,
            .rec = left.irIdx,
            .numNestedFields = 0,
        });
        const right_f = try c.ir.pushExpr(.field, c.alloc, fields[0].type, right_id, .{
            .idx = 0,
            .rec = right.irIdx,
            .numNestedFields = 0,
        });
        it = try c.ir.pushExpr(.preBinOp, c.alloc, bt.Boolean, node_id, .{ .binOp = .{
            .leftT = fields[0].type,
            .rightT = fields[0].type,
            .op = op,
            .left = left_f,
            .right = right_f,
        }});
    }
    if (fields.len > 1) {
        for (fields[1..], 1..) |field, fidx| {
            field_t = c.sema.getTypeSym(fields[0].type);
            if (field_t.type != .int_t and field_t.type != .object_t) {
                return error.Unsupported;
            }
            const left_f = try c.ir.pushExpr(.field, c.alloc, field.type, left_id, .{
                .idx = @as(u8, @intCast(fidx)),
                .rec = left.irIdx,
                .numNestedFields = 0,
            });
            const right_f = try c.ir.pushExpr(.field, c.alloc, field.type, right_id, .{
                .idx = @as(u8, @intCast(fidx)),
                .rec = right.irIdx,
                .numNestedFields = 0,
            });
            const compare = try c.ir.pushExpr(.preBinOp, c.alloc, bt.Boolean, node_id, .{ .binOp = .{
                .leftT = field.type,
                .rightT = field.type,
                .op = op,
                .left = left_f,
                .right = right_f,
            }});
            const logic_op: cy.BinaryExprOp = if (op == .equal_equal) .and_op else .or_op;
            it = try c.ir.pushExpr(.preBinOp, c.alloc, bt.Boolean, node_id, .{ .binOp = .{
                .leftT = bt.Boolean,
                .rightT = bt.Boolean,
                .op = logic_op,
                .left = it,
                .right = compare,
            }});
        }
    }
    const dynamic = right.type.dynamic or left.type.dynamic;
    return ExprResult.init(it, CompactType.init2(bt.Boolean, dynamic));
}

const VarResult = struct {
    id: LocalVarId,
    fromParentBlock: bool,
};

fn assignToLocalVar(c: *cy.Chunk, localRes: ExprResult, rhs: cy.NodeId, opts: AssignOptions) !ExprResult {
    const id = localRes.data.local;
    var svar = &c.varStack.items[id];

    const rightExpr = Expr{ .target_t = svar.vtype.id, .nodeId = rhs, .reqTypeCstr = !svar.vtype.dynamic };
    const right = try c.semaExprOrOpAssignBinExpr(rightExpr, opts.rhsOpAssignBinExpr);
    // Refresh pointer after rhs.
    svar = &c.varStack.items[id];

    if (svar.inner.local.isParam) {
        if (!svar.inner.local.isParamCopied) {
            svar.inner.local.isParamCopied = true;
        }
    }

    const b = c.block();
    if (!b.prevVarTypes.contains(id)) {
        // Same variable but branched to sub block.
        try b.prevVarTypes.put(c.alloc, id, svar.vtype);
    }

    if (svar.isDynamic()) {
        svar.dynamicLastMutBlockId = @intCast(c.semaBlocks.items.len-1);

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

fn popLambdaProc(c: *cy.Chunk) !void {
    const proc = c.proc();
    const params = c.getProcParams(proc);

    const captures = try proc.captures.toOwnedSlice(c.alloc);
    const stmtBlock = try popProc(c);

    var numParamCopies: u8 = 0;
    const paramIrStart = c.ir.advanceExpr(proc.irStart, .lambda);
    const paramData = c.ir.getArray(paramIrStart, ir.FuncParam, params.len);
    for (params, 0..) |param, i| {
        paramData[i] = .{
            .namePtr = param.namePtr,
            .nameLen = param.nameLen,
            .declType = param.declT,
            .isCopy = param.inner.local.isParamCopied,
            .lifted = param.inner.local.lifted,
        };
        if (param.inner.local.isParamCopied) {
            numParamCopies += 1;
        }
    }

    var irCapturesIdx: u32 = cy.NullId;
    if (captures.len > 0) {
        irCapturesIdx = try c.ir.pushEmptyArray(c.alloc, u8, captures.len);
        for (captures, 0..) |varId, i| {
            const pId = c.capVarDescs.get(varId).?.user;
            const pvar = c.varStack.items[pId];
            c.ir.setArrayItem(irCapturesIdx, u8, i, pvar.inner.local.id);
        }
        c.alloc.free(captures);
    }

    // Patch `pushFuncBlock` with maxLocals and param copies.
    c.ir.setExprData(proc.irStart, .lambda, .{
        .func = proc.func.?,
        .numCaptures = @as(u8, @intCast(captures.len)),
        .maxLocals = proc.maxLocals,
        .numParamCopies = numParamCopies,
        .bodyHead = stmtBlock.first,
        .captures = irCapturesIdx,
    });
}

pub fn popFuncBlock(c: *cy.Chunk) !void {
    const proc = c.proc();
    const params = c.getProcParams(proc);

    const stmtBlock = try popProc(c);

    var numParamCopies: u8 = 0;
    const paramIrStart = c.ir.advanceStmt(proc.irStart, .funcBlock);
    const paramData = c.ir.getArray(paramIrStart, ir.FuncParam, params.len);
    for (params, 0..) |param, i| {
        paramData[i] = .{
            .namePtr = param.namePtr,
            .nameLen = param.nameLen,
            .declType = param.declT,
            .isCopy = param.inner.local.isParamCopied,
            .lifted = param.inner.local.lifted,
        };
        if (param.inner.local.isParamCopied) {
            numParamCopies += 1;
        }
    }

    const func = proc.func.?;
    const parentType = if (func.isMethod) params[0].declT else cy.NullId;

    // Patch `pushFuncBlock` with maxLocals and param copies.
    c.ir.setStmtData(proc.irStart, .funcBlock, .{
        .maxLocals = proc.maxLocals,
        .numParamCopies = numParamCopies,
        .func = proc.func.?,
        .bodyHead = stmtBlock.first,
        .parentType = parentType,
    });
}

pub const FuncSigId = u32;
pub const FuncSig = struct {
    /// Last elem is the return type sym.
    paramPtr: [*]const cy.TypeId,
    ret: cy.TypeId,
    paramLen: u16,

    /// If a param or the return type is not the any type.
    // isTyped: bool,

    /// If a param is not the any type.
    // isParamsTyped: bool,

    /// Requires type checking if any param is not `dynamic` or `any`.
    reqCallTypeCheck: bool,

    pub inline fn params(self: FuncSig) []const cy.TypeId {
        return self.paramPtr[0..self.paramLen];
    }

    pub inline fn numParams(self: FuncSig) u8 {
        return @intCast(self.paramLen);
    }

    pub inline fn getRetType(self: FuncSig) cy.TypeId {
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

const BuiltinSymType = enum(u8) {
    bool_t,
    int_t,
    float_t,
};

const BuiltinSym = struct {
    type: BuiltinSymType,
    data: union {
        int_t: struct {
            bits: u8,
        },
        float_t: struct {
            bits: u8,
        },
    },
};

pub const Sema = struct {
    alloc: std.mem.Allocator,
    compiler: *cy.Compiler,

    types: std.ArrayListUnmanaged(cy.types.Type),

    /// Resolved signatures for functions.
    funcSigs: std.ArrayListUnmanaged(FuncSig),
    funcSigMap: std.HashMapUnmanaged(FuncSigKey, FuncSigId, FuncSigKeyContext, 80),

    option_tmpl: *cy.sym.TypeTemplate,
    builtins: std.StringHashMapUnmanaged(BuiltinSym),

    pub fn init(alloc: std.mem.Allocator, compiler: *cy.Compiler) Sema {
        return .{
            .alloc = alloc,
            .compiler = compiler,
            .option_tmpl = undefined,
            .funcSigs = .{},
            .funcSigMap = .{},
            .types = .{},
            .builtins = .{},
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
            self.builtins.clearRetainingCapacity();
        } else {
            self.types.deinit(alloc);
            self.funcSigs.deinit(alloc);
            self.funcSigMap.deinit(alloc);
            self.builtins.deinit(alloc);
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

    pub fn allocArgsStr(s: *Sema, args: []const CompactType, comptime showRecentType: bool) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
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
        try w.writeAll(")");
        return buf.toOwnedSlice(s.alloc);
    }

    /// Format: (Type, ...) RetType
    pub fn getFuncSigTempStr(s: *Sema, buf: *std.ArrayListUnmanaged(u8), funcSigId: FuncSigId) ![]const u8 {
        buf.clearRetainingCapacity();
        const w = buf.writer(s.alloc);
        try writeFuncSigStr(s, w, funcSigId);
        return buf.items;
    }

    pub fn allocTypesStr(s: *Sema, types: []const TypeId) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        try w.writeAll("(");
        try writeFuncParams(s, w, types);
        try w.writeAll(")");
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn allocFuncSigTypesStr(s: *Sema, params: []const TypeId, ret: TypeId) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        try writeFuncSigTypesStr(s, w, params, ret);
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn allocFuncSigStr(s: *Sema, funcSigId: FuncSigId, show_ret: bool) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        const funcSig = s.funcSigs.items[funcSigId];
        try w.writeAll("(");
        try writeFuncParams(s, w, funcSig.params());
        try w.writeAll(")");
        if (show_ret) {
            try w.writeAll(" ");
            try s.writeTypeName(w, funcSig.ret);
        }
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn writeFuncSigStr(s: *Sema, w: anytype, funcSigId: FuncSigId) !void {
        const funcSig = s.funcSigs.items[funcSigId];
        try writeFuncSigTypesStr(s, w, funcSig.params(), funcSig.ret);
    }

    pub fn writeFuncSigTypesStr(s: *Sema, w: anytype, params: []const TypeId, ret: TypeId) !void {
        try w.writeAll("(");
        try writeFuncParams(s, w, params);
        try w.writeAll(") ");
        try s.writeTypeName(w, ret);
    }

    pub fn writeFuncParams(s: *Sema, w: anytype, params: []const TypeId) !void {
        if (params.len > 0) {
            try s.writeTypeName(w, params[0]);

            if (params.len > 1) {
                for (params[1..]) |paramT| {
                    try w.writeAll(", ");
                    try s.writeTypeName(w, paramT);
                }
            }
        }
    }

    pub inline fn getFuncSig(self: *Sema, id: FuncSigId) FuncSig {
        return self.funcSigs.items[id];
    }

    pub usingnamespace cy.types.SemaExt;
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
pub fn unescapeString(buf: []u8, literal: []const u8, always_copy: bool) ![]const u8 {
    var i = std.mem.indexOfScalar(u8, literal, '\\') orelse {
        if (always_copy) {
            @memcpy(buf[0..literal.len], literal);
            return buf[0..literal.len];
        } else {
            return literal;
        }
    };
    @memcpy(buf[0..i], literal[0..i]);
    var len = i;
    var res = try unescapeSeq(literal[i+1..]);
    buf[len] = res.char;
    len += 1;
    var rest = literal[i+1+res.advance..];

    while (true) {
        i = std.mem.indexOfScalar(u8, rest, '\\') orelse break;
        @memcpy(buf[len..len+i], rest[0..i]);
        len += i;
        res = try unescapeSeq(rest[i+1..]);
        buf[len] = res.char;
        len += 1;
        rest = rest[i+1+res.advance..];
    }

    @memcpy(buf[len..len+rest.len], rest);
    return buf[0..len+rest.len];
}

const CharAdvance = struct {
    char: u8,
    advance: u8,

    fn init(char: u8, advance: u8) CharAdvance {
        return CharAdvance{ .char = char, .advance = advance };
    }
};

pub fn unescapeSeq(seq: []const u8) !CharAdvance {
    if (seq.len == 0) {
        return error.MissingEscapeSeq;
    }
    switch (seq[0]) {
        'a' => {
            return CharAdvance.init(0x07, 1);
        },
        'b' => {
            return CharAdvance.init(0x08, 1);
        },
        'e' => {
            return CharAdvance.init(0x1b, 1);
        },
        'n' => {
            return CharAdvance.init('\n', 1);
        },
        'r' => {
            return CharAdvance.init('\r', 1);
        },
        't' => {
            return CharAdvance.init('\t', 1);
        },
        '\\' => {
            return CharAdvance.init('\\', 1);
        },
        '"' => {
            return CharAdvance.init('"', 1);
        },
        '0' => {
            return CharAdvance.init('"', 1);
        },
        'x' => {
            if (seq.len < 3) {
                return error.InvalidEscapeSeq;
            }
            const ch = try std.fmt.parseInt(u8, seq[1..3], 16);
            return CharAdvance.init(ch, 3);
        },
        else => {
            return error.InvalidEscapeSeq;
        }
    }
}

test "sema internals." {
    if (builtin.mode == .ReleaseFast) {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(LocalVar), 32);
        } else {
            try t.eq(@sizeOf(LocalVar), 40);
        }
    } else {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(LocalVar), 32);
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

pub const ObjectBuilder = struct {
    irIdx: u32 = undefined,
    irArgsIdx: u32 = undefined,

    /// Generic typeId so choice types can also be instantiated.
    typeId: cy.TypeId = undefined,

    c: *cy.Chunk,
    argIdx: u32 = undefined,

    pub fn begin(b: *ObjectBuilder, typeId: cy.TypeId, numFields: u8, nodeId: cy.NodeId) !void {
        b.typeId = typeId;
        b.irArgsIdx = try b.c.ir.pushEmptyArray(b.c.alloc, u32, numFields);
        b.irIdx = try b.c.ir.pushExpr(.object_init, b.c.alloc, typeId, nodeId, .{
            .typeId = typeId, .numArgs = @as(u8, @intCast(numFields)), .args = b.irArgsIdx,
        });
        b.argIdx = 0;
    }

    pub fn pushArg(b: *ObjectBuilder, expr: ExprResult) void {
        b.c.ir.setArrayItem(b.irArgsIdx, u32, b.argIdx, expr.irIdx);
        b.argIdx += 1;
    }

    pub fn end(b: *ObjectBuilder) u32 {
        return b.irIdx;
    }
};

/// Currently this matches arguments one at a time. 
/// If an argument can't be matched, this advances to the next function.
/// TODO: A simpler way is to visit all the args first. (literals that have an inferred type can be adjusted afterwards).
const FuncMatcher = struct {
    sym: *cy.sym.FuncSym,
    func: *cy.Func,
    func_params: []const cy.TypeId,
    args_loc: u32,
    type_start: usize,
    ret_cstr: ReturnCstr,
    nargs: u32,

    // Computed.
    has_dyn_arg: bool,
    dyn_call: bool,

    fn init(c: *cy.Chunk, func_sym: *cy.sym.FuncSym, nargs: u32, ret_cstr: ReturnCstr) !FuncMatcher {
        const args_loc = try c.ir.pushEmptyArray(c.alloc, u32, nargs);
        const start = c.typeStack.items.len;

        const func_sig = c.sema.getFuncSig(func_sym.firstFuncSig);
        return FuncMatcher{
            .sym = func_sym,
            .args_loc = args_loc,
            .type_start = start,
            .func = func_sym.first,
            .func_params = func_sig.params(),
            .ret_cstr = ret_cstr,
            .nargs = nargs,
            .has_dyn_arg = false,
            .dyn_call = false,
        };
    }

    fn deinit(self: *FuncMatcher, c: *cy.Chunk) void {
        c.typeStack.items.len = self.type_start;
    }

    fn getArgTypeHint(self: *FuncMatcher, arg_idx: u32) cy.TypeId {
        if (arg_idx < self.func_params.len) {
            return self.func_params[arg_idx];
        } else {
            return bt.Any;
        }
    }

    fn matchEnd(self: *FuncMatcher, c: *cy.Chunk, node: cy.NodeId) !void {
        // Check for unconsumed func params.
        if (self.nargs < self.func_params.len) {
            const arg_types = c.typeStack.items[self.type_start..];
            while (self.nargs < self.func_params.len) {
                if (!self.matchNextFunc(c, arg_types)) {
                    return reportIncompatibleCallSig(c, self.sym, arg_types, self.ret_cstr, node);
                }
            }
        }

        // Check return type.
        if (!cy.types.isValidReturnType(c.compiler, self.func.retType, self.ret_cstr)) {
            const arg_types = c.typeStack.items[self.type_start..];
            return reportIncompatibleCallSig(c, self.sym, arg_types, self.ret_cstr, node);
        }

        // Becomes a dynamic call if this is an overloaded function with a dynamic arg.
        self.dyn_call = self.has_dyn_arg and self.sym.numFuncs > 1;
    }

    fn matchNextFunc(self: *FuncMatcher, c: *cy.Chunk, arg_types: []const cy.TypeId) bool {
        while (true) {
            // First get next function that matches previous arg types.
            self.func = self.func.next orelse {
                return false;
            };
            const func_sig = c.sema.getFuncSig(self.func.funcSigId);
            self.func_params = func_sig.params();

            if (self.func_params.len < arg_types.len) {
                continue;
            }
            for (arg_types, 0..) |arg_t, i| {
                if (i >= self.func_params.len) {
                    continue;
                }
                if (!cy.types.isTypeSymCompat(c.compiler, arg_t, self.func_params[i])) {
                    // Ignore `arg_t.dynamic` since dynamic args already have inserted type checks.
                    continue;
                }
            }
            return true;
        }
    }

    fn reportIncompatibleArg(self: *FuncMatcher, c: *cy.Chunk, arg: cy.NodeId, arg_res: ExprResult) anyerror {
        try c.typeStack.append(c.alloc, arg_res.type.id);
        const types = c.typeStack.items[self.type_start..];
        return reportIncompatibleCallSig(c, self.sym, types, self.ret_cstr, arg);
    }

    fn matchArg(self: *FuncMatcher, c: *cy.Chunk, arg: cy.NodeId, arg_idx: u32, arg_res: ExprResult) !void {
        if (arg_idx >= self.func_params.len) {
            while (arg_idx >= self.func_params.len) {
                const arg_types = c.typeStack.items[self.type_start..];
                if (!self.matchNextFunc(c, arg_types)) {
                    return self.reportIncompatibleArg(c, arg, arg_res);
                }
            }
        }
        var ct_compat = cy.types.isTypeSymCompat(c.compiler, arg_res.type.id, self.func_params[arg_idx]);
        const rt_compat = arg_res.type.isDynAny();
        if (!ct_compat and !rt_compat) {
            const arg_types = c.typeStack.items[self.type_start..];
            while (!ct_compat) {
                // Can not be used as argument.
                if (!self.matchNextFunc(c, arg_types)) {
                    return self.reportIncompatibleArg(c, arg, arg_res);
                }

                // Check if next func has an unconsumed param.
                if (arg_idx >= self.func_params.len) {
                    continue;
                }

                ct_compat = cy.types.isTypeSymCompat(c.compiler, arg_res.type.id, self.func_params[arg_idx]);
            }
        }

        self.has_dyn_arg = self.has_dyn_arg or rt_compat;

        if (ct_compat) {
            try c.typeStack.append(c.alloc, arg_res.type.id);
            c.ir.setArrayItem(self.args_loc, u32, arg_idx, arg_res.irIdx);
        } else {
            if (self.sym.numFuncs > 1) {
                try c.typeStack.append(c.alloc, arg_res.type.id);
                c.ir.setArrayItem(self.args_loc, u32, arg_idx, arg_res.irIdx);
            } else {
                // Insert rt arg type check if this is not an overloaded function.
                const loc = try c.ir.pushExpr(.type_check, c.alloc, self.func_params[arg_idx], arg, .{
                    .expr = arg_res.irIdx,
                    .exp_type = self.func_params[arg_idx],
                });
                try c.typeStack.append(c.alloc, self.func_params[arg_idx]);
                c.ir.setArrayItem(self.args_loc, u32, arg_idx, loc);
            }
        }
    }
};

fn getUnOpName(op: cy.UnaryOp) []const u8 {
    return switch (op) {
        .minus => "$prefix-",
        .bitwiseNot => "$prefix~",
        else => @panic("unsupported"),
    };
}

fn getBinOpName(op: cy.BinaryExprOp) []const u8 {
    return switch (op) {
        .index => "$index",
        .less => "$infix<",
        .greater => "$infix>",
        .less_equal => "$infix<=",
        .greater_equal => "$infix>=",
        .minus => "$infix-",
        .plus => "$infix+",
        .star => "$infix*",
        .slash => "$infix/",
        .percent => "$infix%",
        .caret => "$infix^",
        .bitwiseAnd => "$infix&",
        .bitwiseOr => "$infix|",
        .bitwiseXor => "$infix||",
        .bitwiseLeftShift => "$infix<<",
        .bitwiseRightShift => "$infix>>",
        else => @panic("unsupported"),
    };
}
