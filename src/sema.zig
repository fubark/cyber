const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const C = @import("capi.zig");
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
const ast = cy.ast;

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
        return self.declT == bt.Dyn;
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
    node: *ast.Node,

    /// Whether execution can reach the end.
    /// If a return statement was generated, this would be set to false.
    endReachable: bool = true,

    /// Tracks how many locals are owned by this sub-block.
    /// When the sub-block is popped, this is subtracted from the block's `curNumLocals`.
    numLocals: u8,

    pub fn init(node: *ast.Node, assignedVarStart: usize, varStart: usize, varShadowStart: usize) Block {
        return .{
            .node = node,
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

    node: *ast.Node,

    pub fn init(node: *ast.Node, func: ?*cy.Func, firstBlockId: BlockId, isStaticFuncBlock: bool, varStart: u32) Proc {
        return .{
            .nameToVar = .{},
            .numParams = 0,
            .blockDepth = 0,
            .func = func,
            .firstBlockId = firstBlockId,
            .isStaticFuncBlock = isStaticFuncBlock,
            .node = node,
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

pub fn semaStmts(c: *cy.Chunk, stmts: []const *ast.Node) anyerror!void {
    for (stmts) |stmt| {
        try semaStmt(c, stmt);
    }
}

pub fn semaStmt(c: *cy.Chunk, node: *ast.Node) !void {
    c.curNode = node;
    if (cy.Trace) {
        var buf: [1024]u8 = undefined;
        var fbuf = std.io.fixedBufferStream(&buf);
        try c.encoder.write(fbuf.writer(), node);
        log.tracev("stmt.{s}: \"{s}\"", .{@tagName(node.type()), fbuf.getWritten()});
    }
    switch (node.type()) {
        .exprStmt => {
            const stmt = node.cast(.exprStmt);
            const returnMain = stmt.isLastRootStmt;
            const expr = try c.semaExprTarget(stmt.child, bt.Void);
            _ = try c.ir.pushStmt(c.alloc, .exprStmt, node, .{
                .expr = expr.irIdx,
                .isBlockResult = returnMain,
            });
        },
        .breakStmt => {
            _ = try c.ir.pushStmt(c.alloc, .breakStmt, node, {});
        },
        .continueStmt => {
            _ = try c.ir.pushStmt(c.alloc, .contStmt, node, {});
        },
        .localDecl => {
            try localDecl(c, node.cast(.localDecl));
        },
        .opAssignStmt => {
            const stmt = node.cast(.opAssignStmt);
            switch (stmt.op) {
                .star,
                .slash,
                .percent,
                .caret,
                .plus,
                .minus => {},
                else => {
                    return c.reportErrorFmt("Unsupported op assign statement for {}.", &.{v(stmt.op)}, node);
                }
            }

            const irIdx = try c.ir.pushStmt(c.alloc, .opSet, node, .{ .op = stmt.op, .set_stmt = undefined });

            const set_stmt = try assignStmt(c, node, stmt.left, node, .{ .rhsOpAssignBinExpr = true });
            c.ir.getStmtDataPtr(irIdx, .opSet).set_stmt = set_stmt;

            // Reset `opSet`'s next since pushed statements are appended to the top StmtBlock.
            c.ir.setStmtNext(irIdx, cy.NullId);
            c.ir.stmtBlockStack.items[c.ir.stmtBlockStack.items.len-1].last = irIdx;
        },
        .assignStmt => {
            const stmt = node.cast(.assignStmt);
            _ = try assignStmt(c, node, stmt.left, stmt.right, .{});
        },
        .table_decl,
        .objectDecl,
        .structDecl,
        .passStmt,
        .staticDecl,
        .typeAliasDecl,
        .distinct_decl,
        .template,
        .enumDecl,
        .funcDecl => {
            // Nop.
        },
        .forIterStmt => {
            const stmt = node.cast(.forIterStmt);

            try preLoop(c, node);
            const irIdx = try c.ir.pushEmptyStmt(c.alloc, .forIterStmt, node);
            const iter = try c.semaExpr(stmt.iterable, .{});
            try pushBlock(c, node);

            var eachLocal: ?u8 = null;
            var countLocal: ?u8 = null;
            var hasSeqDestructure = false;
            var seqIrVarStart: u32 = undefined;
            if (stmt.each) |each| {
                if (each.type() == .ident) {
                    const varId = try declareLocal(c, each, bt.Dyn, false);
                    eachLocal = c.varStack.items[varId].inner.local.id;

                    if (stmt.count) |count| {
                        const countVarId = try declareLocal(c, count, bt.Integer, false);
                        countLocal = c.varStack.items[countVarId].inner.local.id;
                    }
                } else if (each.type() == .seqDestructure) {
                    const varId = try declareLocalName(c, "$elem", bt.Dyn, false, each);
                    eachLocal = c.varStack.items[varId].inner.local.id;

                    if (stmt.count) |count| {
                        const countVarId = try declareLocal(c, count, bt.Integer, false);
                        countLocal = c.varStack.items[countVarId].inner.local.id;
                    }

                    const decls = each.cast(.seqDestructure).args;

                    seqIrVarStart = @intCast(c.dataU8Stack.items.len);
                    for (decls) |decl| {
                        const varId2 = try declareLocal(c, decl, bt.Dyn, false);
                        const irVarId = c.varStack.items[varId2].inner.local.id;
                        try c.dataU8Stack.append(c.alloc, .{ .irLocal = irVarId });
                    }
                    hasSeqDestructure = true;
                } else {
                    return c.reportErrorFmt("Unsupported each clause: {}", &.{v(each.type())}, each);
                }
            }

            const declHead = c.ir.getAndClearStmtBlock();

            // Begin body code.
            if (hasSeqDestructure) {
                const destrIdx = try c.ir.pushEmptyStmt(c.alloc, .destrElemsStmt, stmt.each.?);
                const locals = c.dataU8Stack.items[seqIrVarStart..];
                const irStart = c.ir.buf.items.len;
                try c.ir.buf.resize(c.alloc, c.ir.buf.items.len + locals.len);
                for (locals, 0..) |local, i| {
                    c.ir.buf.items[irStart + i] = local.irLocal;
                }
                const right = try c.ir.pushExpr(.local, c.alloc, bt.Any, stmt.each.?, .{ .id = eachLocal.? });
                c.ir.setStmtData(destrIdx, .destrElemsStmt, .{
                    .numLocals = @intCast(stmt.each.?.cast(.seqDestructure).args.len),
                    .right = right,
                });
                c.dataU8Stack.items.len = seqIrVarStart;
            }

            try semaStmts(c, stmt.stmts);
            const stmtBlock = try popLoopBlock(c);

            c.ir.setStmtData(irIdx, .forIterStmt, .{
                .iter = iter.irIdx, .eachLocal = eachLocal,
                .countLocal = countLocal, .declHead = declHead,
                .bodyHead = stmtBlock.first,
            });
        },
        .forRangeStmt => {
            const stmt = node.cast(.forRangeStmt);
            try preLoop(c, node);
            const irIdx = try c.ir.pushEmptyStmt(c.alloc, .forRangeStmt, node);

            const range_start = try c.semaExpr(stmt.start, .{});
            const range_end = try c.semaExpr(stmt.end, .{});

            try pushBlock(c, node);

            var eachLocal: ?u8 = null;
            if (stmt.each) |each| {
                if (each.type() == .ident) {
                    const varId = try declareLocal(c, each, bt.Integer, false);
                    eachLocal = c.varStack.items[varId].inner.local.id;
                } else {
                    return c.reportErrorFmt("Unsupported each clause: {}", &.{v(each.type())}, each);
                }
            }

            const declHead = c.ir.getAndClearStmtBlock();

            try semaStmts(c, stmt.stmts);
            const stmtBlock = try popLoopBlock(c);
            c.ir.setStmtData(irIdx, .forRangeStmt, .{
                .eachLocal = eachLocal,
                .start = range_start.irIdx,
                .end = range_end.irIdx,
                .bodyHead = stmtBlock.first,
                .increment = stmt.increment,
                .declHead = declHead,
            });
        },
        .whileInfStmt => {
            const stmt = node.cast(.whileInfStmt);
            try preLoop(c, node);
            const loc = try c.ir.pushEmptyStmt(c.alloc, .loopStmt, node);
            try pushBlock(c, node);
            {
                try semaStmts(c, stmt.stmts);
                _ = try c.ir.pushStmt(c.alloc, .contStmt, node, {});
            }
            const stmtBlock = try popLoopBlock(c);
            c.ir.setStmtData(loc, .loopStmt, .{
                .body_head = stmtBlock.first,
            });
        },
        .whileCondStmt => {
            const stmt = node.cast(.whileCondStmt);
            try preLoop(c, node);
            const loc = try c.ir.pushEmptyStmt(c.alloc, .loopStmt, node);

            try pushBlock(c, node);
            {
                const if_stmt = try c.ir.pushEmptyStmt(c.alloc, .ifStmt, stmt.cond);
                const cond = try c.semaExprCstr(stmt.cond, bt.Boolean);
                try pushBlock(c, stmt.cond);
                {
                    try semaStmts(c, stmt.stmts);
                    _ = try c.ir.pushStmt(c.alloc, .contStmt, node, {});
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
            const stmt = node.cast(.whileOptStmt);
            try preLoop(c, node);
            const loc = try c.ir.pushEmptyStmt(c.alloc, .loopStmt, node);

            try pushBlock(c, node);
            {
                const if_stmt = try c.ir.pushEmptyStmt(c.alloc, .ifUnwrapStmt, stmt.opt);
                const opt = try c.semaOptionExpr(stmt.opt);
                try pushBlock(c, stmt.opt);
                var decl_head: u32 = undefined;
                var unwrap_local: u8 = undefined;
                {
                    const unwrap_t = if (opt.type.dynamic) bt.Dyn else b: {
                        break :b c.sema.getTypeSym(opt.type.id).cast(.enum_t).getMemberByIdx(1).payloadType;
                    };
                    const unwrap_var = try declareLocal(c, stmt.capture, unwrap_t, false);
                    unwrap_local = @intCast(c.varStack.items[unwrap_var].inner.local.id);

                    decl_head = c.ir.getAndClearStmtBlock();
                    try semaStmts(c, stmt.stmts);
                    _ = try c.ir.pushStmt(c.alloc, .contStmt, node, {});
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
            try semaSwitchStmt(c, node.cast(.switchStmt));
        },
        .if_stmt => {
            try semaIfStmt(c, node.cast(.if_stmt));
        },
        .if_unwrap_stmt => {
            try semaIfUnwrapStmt(c, node.cast(.if_unwrap_stmt));
        },
        .tryStmt => {
            const block = node.cast(.tryStmt);
            var data: ir.TryStmt = undefined;
            const irIdx = try c.ir.pushEmptyStmt(c.alloc, .tryStmt, node);
            try pushBlock(c, node);
            try semaStmts(c, block.stmts);
            var stmtBlock = try popBlock(c);
            data.bodyHead = stmtBlock.first;

            try pushBlock(c, node);
            if (block.catchStmt.errorVar) |err_var| {
                const id = try declareLocal(c, err_var, bt.Error, false);
                data.hasErrLocal = true;
                data.errLocal = c.varStack.items[id].inner.local.id;
            } else {
                data.hasErrLocal = false;
            }
            try semaStmts(c, block.catchStmt.stmts);
            stmtBlock = try popBlock(c);
            data.catchBodyHead = stmtBlock.first;

            c.ir.setStmtData(irIdx, .tryStmt, data);
        },
        .use_alias,
        .import_stmt => {},
        .returnStmt => {
            _ = try c.ir.pushStmt(c.alloc, .retStmt, node, {});
        },
        .returnExprStmt => {
            const proc = c.proc();
            const retType = try proc.getReturnType();

            const expr = try c.semaExprCstr(node.cast(.returnExprStmt).child, retType);
            _ = try c.ir.pushStmt(c.alloc, .retExprStmt, node, .{
                .expr = expr.irIdx,
            });
        },
        .comptimeStmt => {
            const expr = node.cast(.comptimeStmt).expr;
            if (expr.type() == .callExpr) {
                const call = expr.cast(.callExpr);
                const name = c.ast.nodeString(call.callee);

                if (std.mem.eql(u8, "genLabel", name)) {
                    if (call.args.len != 1) {
                        return c.reportErrorFmt("genLabel expected 1 arg", &.{}, node);
                    }

                    if (call.args[0].type() != .stringLit and call.args[0].type() != .raw_string_lit) {
                        return c.reportErrorFmt("genLabel expected string arg", &.{}, node);
                    }

                    const label = c.ast.nodeString(call.args[0]);
                    _ = try c.ir.pushStmt(c.alloc, .pushDebugLabel, node, .{ .name = label });
                } else if (std.mem.eql(u8, "dumpLocals", name)) {
                    const proc = c.proc();
                    try c.dumpLocals(proc);
                } else if (std.mem.eql(u8, "dumpBytecode", name)) {
                    _ = try c.ir.pushStmt(c.alloc, .dumpBytecode, node, {});
                } else if (std.mem.eql(u8, "verbose", name)) {
                    C.setVerbose(true);
                    _ = try c.ir.pushStmt(c.alloc, .verbose, node, .{ .verbose = true });
                } else if (std.mem.eql(u8, "verboseOff", name)) {
                    C.setVerbose(false);
                    _ = try c.ir.pushStmt(c.alloc, .verbose, node, .{ .verbose = false });
                } else {
                    return c.reportErrorFmt("Unsupported annotation: {}", &.{v(name)}, node);
                }
            } else {
                return c.reportErrorFmt("Unsupported expr: {}", &.{v(expr.type())}, node);
            }
        },
        else => return c.reportErrorFmt("Unsupported statement: {}", &.{v(node.type())}, node),
    }
}

fn semaElseStmt(c: *cy.Chunk, node: *ast.Node) !u32 {
    if (node.type() == .else_block) {
        const block = node.cast(.else_block);
        if (block.cond) |cond| {
            const irElseIdx = try c.ir.pushEmptyExpr(.else_block, c.alloc, undefined, node);
            const elseCond = try c.semaExprCstr(cond, bt.Boolean);

            try pushBlock(c, node);
            try semaStmts(c, block.stmts);
            const stmtBlock = try popBlock(c);

            c.ir.setExprData(irElseIdx, .else_block, .{
                .cond = elseCond.irIdx, .body_head = stmtBlock.first, .else_block = cy.NullId,
            });
            return irElseIdx;
        } else {
            // else.
            const irElseIdx = try c.ir.pushEmptyExpr(.else_block, c.alloc, undefined, node);
            try pushBlock(c, node);
            try semaStmts(c, block.stmts);
            const stmtBlock = try popBlock(c);

            c.ir.setExprData(irElseIdx, .else_block, .{
                .cond = cy.NullId, .body_head = stmtBlock.first, .else_block = cy.NullId,
            });

            return irElseIdx;
        }
    } else return error.TODO;
}

fn semaIfUnwrapStmt(c: *cy.Chunk, block: *ast.IfUnwrapStmt) !void {
    const loc = try c.ir.pushEmptyStmt(c.alloc, .ifUnwrapStmt, @ptrCast(block));

    const opt = try c.semaOptionExpr(block.opt);

    try pushBlock(c, @ptrCast(block));
    var decl_head: u32 = undefined;
    var unwrap_local: u8 = undefined;
    {
        const unwrap_t = if (opt.type.dynamic) bt.Dyn else b: {
            break :b c.sema.getTypeSym(opt.type.id).cast(.enum_t).getMemberByIdx(1).payloadType;
        };
        const unwrap_var = try declareLocal(c, block.unwrap, unwrap_t, false);
        unwrap_local = @intCast(c.varStack.items[unwrap_var].inner.local.id);

        decl_head = c.ir.getAndClearStmtBlock();
        try semaStmts(c, block.stmts);
    }
    const if_block = try popBlock(c);

    if (block.else_blocks.len == 0) {
        c.ir.setStmtData(loc, .ifUnwrapStmt, .{
            .opt = opt.irIdx,
            .unwrap_local = unwrap_local,
            .decl_head = decl_head,
            .body_head = if_block.first,
            .else_block = cy.NullId,
        });
        return;
    }

    var else_loc = try semaElseStmt(c, @ptrCast(block.else_blocks[0]));
    c.ir.setStmtData(loc, .ifUnwrapStmt, .{
        .opt = opt.irIdx,
        .unwrap_local = unwrap_local,
        .decl_head = decl_head,
        .body_head = if_block.first,
        .else_block = else_loc,
    });

    for (block.else_blocks[1..]) |else_block| {
        const next_else_loc = try semaElseStmt(c, @ptrCast(else_block));
        c.ir.getExprDataPtr(else_loc, .else_block).else_block = next_else_loc;
        else_loc = next_else_loc;
    }
}

fn semaIfStmt(c: *cy.Chunk, block: *ast.IfStmt) !void {
    const irIdx = try c.ir.pushEmptyStmt(c.alloc, .ifStmt, @ptrCast(block));

    const cond = try c.semaExprCstr(block.cond, bt.Boolean);

    try pushBlock(c, @ptrCast(block));
    try semaStmts(c, block.stmts);
    const ifStmtBlock = try popBlock(c);

    if (block.else_blocks.len == 0) {
        c.ir.setStmtData(irIdx, .ifStmt, .{
            .cond = cond.irIdx,
            .body_head = ifStmtBlock.first,
            .else_block = cy.NullId,
        });
        return;
    }

    var else_loc = try semaElseStmt(c, @ptrCast(block.else_blocks[0]));
    c.ir.setStmtData(irIdx, .ifStmt, .{
        .cond = cond.irIdx,
        .body_head = ifStmtBlock.first,
        .else_block = else_loc,
    });

    for (block.else_blocks[1..]) |else_block| {
        const next_else_loc = try semaElseStmt(c, @ptrCast(else_block));
        c.ir.getExprDataPtr(else_loc, .else_block).else_block = next_else_loc;
        else_loc = next_else_loc;
    }
}

const AssignOptions = struct {
    rhsOpAssignBinExpr: bool = false,
};

/// Pass rightId explicitly to perform custom sema on op assign rhs.
fn assignStmt(c: *cy.Chunk, node: *ast.Node, left_n: *ast.Node, right: *ast.Node, opts: AssignOptions) !u32 {
    switch (left_n.type()) {
        .array_expr => {
            const left = left_n.cast(.array_expr);
            if (left.args.len != 1) {
                return c.reportErrorFmt("Unsupported array expr.", &.{}, left_n);
            }

            const rec = try c.semaExpr(left.left, .{});

            var final_right = right;
            if (opts.rhsOpAssignBinExpr) {
                // Push bin expr.
                const op_assign = right.cast(.opAssignStmt);
                final_right = try c.parser.ast.newNodeErase(.binExpr, .{
                    .left = left_n,
                    .right = op_assign.right,
                    .op = op_assign.op,
                    .op_pos = op_assign.assign_pos,
                });
            }

            if (rec.type.isDynAny()) {
                const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, node);
                const index = try c.semaExpr(left.args[0], .{});
                const right_res = try c.semaExpr(final_right, .{});
                const res = try c.semaCallObjSym2(loc, rec.irIdx, "$setIndex", &.{index, right_res});
                return c.ir.pushStmt(c.alloc, .exprStmt, node, .{
                    .expr = res.irIdx,
                    .isBlockResult = false,
                });
            }

            const rec_type_sym = c.sema.getTypeSym(rec.type.id);
            const set_index_sym = try c.mustFindSym(rec_type_sym, "$setIndex", left_n);
            const func_sym = try requireFuncSym(c, set_index_sym, left_n);

            const res = try c.semaCallFuncSymRecArgs(func_sym, left.left, rec,
                &.{ left.args[0], final_right }, .any, node);
            return c.ir.pushStmt(c.alloc, .exprStmt, node, .{
                .expr = res.irIdx,
                .isBlockResult = false,
            });
        },
        .accessExpr => {
            const left = left_n.cast(.accessExpr);
            const rec = try c.semaExpr(left.left, .{});
            const name = c.ast.nodeString(left.right);
            const type_sym = c.sema.getTypeSym(rec.type.id);
            const debug_node = left.right;
            if (rec.type.isDynAny()) {
                const expr = Expr{ .node = right, .reqTypeCstr = false, .target_t = bt.Any };
                const right_res = try c.semaExprOrOpAssignBinExpr(expr, opts.rhsOpAssignBinExpr);
                return try c.ir.pushStmt(c.alloc, .set_field_dyn, debug_node, .{ .set_field_dyn = .{
                    .name = name,
                    .rec = rec.irIdx,
                    .right = right_res.irIdx,
                }});
            }

            const set_info = try checkSymSetField(c, type_sym, name, debug_node);
            if (set_info.use_set_method) {
                const expr = Expr{ .node = right, .reqTypeCstr = false, .target_t = bt.Any };
                const right_res = try c.semaExprOrOpAssignBinExpr(expr, opts.rhsOpAssignBinExpr);
                return try c.ir.pushStmt(c.alloc, .set_field_dyn, debug_node, .{ .set_field_dyn = .{
                    .name = name,
                    .rec = rec.irIdx,
                    .right = right_res.irIdx,
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

                const expr = Expr{ .node = right, .reqTypeCstr = true, .target_t = set_info.typeId };
                const right_res = try c.semaExprOrOpAssignBinExpr(expr, opts.rhsOpAssignBinExpr);
                return try c.ir.pushStmt(c.alloc, .set_field, debug_node, .{ .set_field = .{
                    .field = rec.irIdx,
                    .right = right_res.irIdx,
                }});
            } else {
                const loc = try c.ir.pushExpr(.field, c.alloc, set_info.typeId, debug_node, .{
                    .idx = set_info.idx,
                    .rec = rec.irIdx,
                    .numNestedFields = 0,
                });

                const expr = Expr{ .node = right, .reqTypeCstr = true, .target_t = set_info.typeId };
                const right_res = try c.semaExprOrOpAssignBinExpr(expr, opts.rhsOpAssignBinExpr);
                return try c.ir.pushStmt(c.alloc, .set_field, debug_node, .{ .set_field = .{
                    .field = loc,
                    .right = right_res.irIdx,
                }});
            }
        },
        .ident => {
            var right_res: ExprResult = undefined;
            const leftRes = try c.semaExpr(left_n, .{});
            const leftT = leftRes.type;
            if (leftRes.resType == .local) {
                right_res = try assignToLocalVar(c, leftRes, right, opts);
            } else if (leftRes.resType == .field) {
                const expr = Expr{ .node = right, .reqTypeCstr = true, .target_t = leftT.id };
                right_res = try c.semaExprOrOpAssignBinExpr(expr, opts.rhsOpAssignBinExpr);
                return try c.ir.pushStmt(c.alloc, .set_field, left_n, .{ .set_field = .{
                    .field = leftRes.irIdx,
                    .right = right_res.irIdx,
                }});
            } else if (leftRes.resType == .global) {
                const rightExpr = Expr{ .node = right, .reqTypeCstr = !leftT.dynamic, .target_t = leftT.id };
                right_res = try c.semaExprOrOpAssignBinExpr(rightExpr, opts.rhsOpAssignBinExpr);

                const name = c.ast.nodeString(left_n);
                const key = try c.semaString(name, left_n);

                const map = try symbol(c, @ptrCast(c.compiler.global_sym.?), left_n, true);
                const map_sym = c.sema.getTypeSym(bt.Map);
                const set_index = map_sym.getMod().?.getSym("$setIndex").?;
                const call = try c.semaCallFuncSymN(set_index.cast(.func),
                    &.{left_n, left_n, right},
                    &.{map, key, right_res},
                    .any, node);

                return c.ir.pushStmt(c.alloc, .exprStmt, node, .{
                    .expr = call.irIdx,
                    .isBlockResult = false,
                });
            } else {
                const rightExpr = Expr{ .node = right, .reqTypeCstr = !leftT.dynamic, .target_t = leftT.id };
                right_res = try c.semaExprOrOpAssignBinExpr(rightExpr, opts.rhsOpAssignBinExpr);
            }

            const irStart = try c.ir.pushEmptyStmt(c.alloc, .set, node);
            c.ir.setStmtData(irStart, .set, .{ .generic = .{
                .left_t = leftT,
                .right_t = right_res.type,
                .left = leftRes.irIdx,
                .right = right_res.irIdx,
            }});
            switch (leftRes.resType) {
                .varSym         => c.ir.setStmtCode(irStart, .setVarSym),
                .func           => {
                    return c.reportErrorFmt("Can not reassign to a namespace function.", &.{}, node);
                },
                .local          => c.ir.setStmtCode(irStart, .setLocal),
                .capturedLocal  => c.ir.setStmtCode(irStart, .setCaptured),
                else => {
                    log.tracev("leftRes {s} {}", .{@tagName(leftRes.resType), leftRes.type});
                    return c.reportErrorFmt("Assignment to the left `{}` is unsupported.", &.{v(left_n.type())}, node);
                }
            }
            return irStart;
        },
        else => {
            return c.reportErrorFmt("Assignment to the left `{}` is unsupported.", &.{v(left_n.type())}, node);
        }
    }
}

fn semaIndexExpr(c: *cy.Chunk, left_id: *ast.Node, left: ExprResult, expr: Expr) !ExprResult {
    const array = expr.node.cast(.array_expr);
    if (array.args.len != 1) {
        return c.reportErrorFmt("Unsupported array expr.", &.{}, expr.node);
    }

    const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, expr.node);

    const leftT = left.type.id;
    if (left.type.isDynAny()) {
        const index = try c.semaExpr(array.args[0], .{});
        return c.semaCallObjSym2(loc, left.irIdx, getBinOpName(.index), &.{index});
    }

    const recTypeSym = c.sema.getTypeSym(leftT);
    const sym = try c.mustFindSym(recTypeSym, "$index", expr.node);
    const func_sym = try requireFuncSym(c, sym, expr.node);

    return c.semaCallFuncSymRec(loc, func_sym,
        left_id, left,
        array.args, expr.getRetCstr(), expr.node);
}

fn semaAccessField(c: *cy.Chunk, rec: ExprResult, field: *ast.Node) !ExprResult {
    if (field.type() != .ident) {
        return error.Unexpected;
    }
    const name = c.ast.nodeString(field);
    return semaAccessFieldName(c, rec, name, field);
}

fn semaAccessFieldName(c: *cy.Chunk, rec: ExprResult, name: []const u8, field: *ast.Node) !ExprResult {
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

fn checkGetField(c: *cy.Chunk, rec_t: TypeId, fieldName: []const u8, node: *ast.Node) !FieldResult {
    const sym = c.sema.getTypeSym(rec_t);
    switch (sym.type) {
        .enum_t,
        .dynobject_t,
        .object_t,
        .struct_t => {
            return checkSymGetField(c, sym, fieldName, node);
        },
        else => {
            const name = c.sema.getTypeBaseName(rec_t);
            return c.reportErrorFmt("Field access unsupported for `{}`.", &.{v(name)}, node);
        },
    }
}

fn checkSymSetField(c: *cy.Chunk, type_sym: *cy.Sym, name: []const u8, node: *ast.Node) !SetFieldResult {
    const sym = type_sym.getMod().?.getSym(name) orelse {
        // TODO: This depends on $get being known, make sure $get is a nested declaration.
        const type_e = c.sema.types.items[type_sym.getStaticType().?];
        if (type_e.has_set_method) {
            return .{ .idx = undefined, .typeId = undefined, .use_set_method = true };
        } else {
            const type_name = type_sym.name();
            return c.reportErrorFmt("Field `{}` does not exist in `{}`.", &.{v(name), v(type_name)}, node);
        }
    };
    if (sym.type != .field) {
        const type_name = type_sym.name();
        return c.reportErrorFmt("Type `{}` does not have a field named `{}`.", &.{v(type_name), v(name)}, node);
    }
    const field = sym.cast(.field);
    return .{
        .idx = @intCast(field.idx),
        .typeId = field.type,
        .use_set_method = false,
    };
}

fn checkSymGetField(c: *cy.Chunk, type_sym: *cy.Sym, name: []const u8, node: *ast.Node) !FieldResult {
    const sym = type_sym.getMod().?.getSym(name) orelse {
        // TODO: This depends on $get being known, make sure $get is a nested declaration.
        const type_e = c.sema.types.items[type_sym.getStaticType().?];
        if (type_e.has_get_method) {
            return .{ .idx = undefined, .typeId = undefined, .use_get_method = true };
        } else {
            const type_name = type_sym.name();
            return c.reportErrorFmt("Field `{}` does not exist in `{}`.", &.{v(name), v(type_name)}, node);
        }
    };
    if (sym.type != .field) {
        const type_name = type_sym.name();
        return c.reportErrorFmt("Type `{}` does not have a field named `{}`.", &.{v(type_name), v(name)}, node);
    }
    const field = sym.cast(.field);
    return .{
        .idx = @intCast(field.idx),
        .typeId = field.type,
        .use_get_method = false,
    };
}

fn checkSymInitField(c: *cy.Chunk, type_sym: *cy.Sym, name: []const u8, node: *ast.Node) !InitFieldResult {
    const sym = type_sym.getMod().?.getSym(name) orelse {
        // // TODO: This depends on $get being known, make sure $get is a nested declaration.
        // const type_e = c.sema.types.items[type_sym.getStaticType().?];
        // if (type_e.has_get_method) {
        //     return .{ .idx = undefined, .typeId = undefined, .use_get_method = true };
        // } else {
            const type_name = type_sym.name();
            return c.reportErrorFmt("Field `{}` does not exist in `{}`.", &.{v(name), v(type_name)}, node);
        // }
    };
    if (sym.type != .field) {
        const type_name = type_sym.name();
        return c.reportErrorFmt("Type `{}` does not have a field named `{}`.", &.{v(type_name), v(name)}, node);
    }
    const field = sym.cast(.field);
    return .{
        .idx = @intCast(field.idx),
        .typeId = field.type,
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

const InitFieldResult = struct {
    typeId: TypeId,
    idx: u8,
};

pub fn declareTemplate(c: *cy.Chunk, node: *ast.TemplateDecl) !*cy.sym.Template {
    var name_n: *ast.Node = undefined;
    var decl_t: cy.sym.SymType = undefined;
    switch (node.decl.type()) {
        .objectDecl => {
            name_n = node.decl.cast(.objectDecl).name.?;
            decl_t = .object_t;
        },
        .enumDecl => {
            name_n = node.decl.cast(.enumDecl).name;
            decl_t = .enum_t;
        },
        .custom_decl => {
            name_n = node.decl.cast(.custom_decl).name;
            decl_t = .custom_t;
        },
        .funcDecl => {
            name_n = node.decl.cast(.funcDecl).name;
            decl_t = .func;
        },
        else => {
            return c.reportErrorFmt("Unsupported type template.", &.{}, @ptrCast(node));
        }
    }

    const decl_path = try ensureDeclNamePath(c, @ptrCast(c.sym), name_n);

    var sigId: FuncSigId = undefined;
    const params = try resolveTemplateSig(c, node.params, &sigId);
    const sym = try c.declareTemplate(decl_path.parent, decl_path.name.base_name, sigId, params, decl_t, node.decl, node);
    if (decl_t == .func) {
        const child_decl = node.decl.cast(.funcDecl);

        for (child_decl.params, 0..) |param, i| {
            if (param.typeSpec == null) {
                break;
            }
            if (param.typeSpec.?.type() != .ident) {
                continue;
            }
            const pname = c.ast.nodeString(@ptrCast(param.typeSpec));
            const tidx = sym.indexOfParam(pname) orelse {
                continue;
            };
            if (sym.params[tidx].infer_from_func_param != cy.NullU8) {
                // Already a mapping.
                continue;
            }
            sym.params[tidx].infer_from_func_param = @intCast(i);
        }
        // Verify each template param has a mapping.
        var infer_min_params: u8 = 0;
        for (sym.params) |param| {
            if (param.infer_from_func_param == cy.NullU8) {
                infer_min_params = cy.NullU8;
                break;
            }
            if (param.infer_from_func_param + 1 > infer_min_params) {
                infer_min_params = param.infer_from_func_param + 1;
            }
        }
        sym.infer_min_params = infer_min_params;
    }
    return sym;
}

/// Explicit `decl` for specialization declarations.
fn resolveCustomType(c: *cy.Chunk, sym: *cy.sym.CustomType, decl: *ast.CustomDecl) !void {
    var name: ?[]const u8 = null;
    var has_host_attr = false;
    if (decl.attrs.len > 0) {
        if (decl.attrs[0].type == .host) {
            has_host_attr = true;
            name = try getHostAttrName(c, decl.attrs[0]);
        }
    }
    log.tracev("host name: {any}", .{name});
    if (!has_host_attr) {
        return c.reportErrorFmt("Custom type requires a `@host` attribute.", &.{}, @ptrCast(decl));
    }

    const bind_name = name orelse sym.head.name();
    if (c.host_types.get(bind_name)) |host_t| {
        try resolveCustomTypeResult(c, sym, host_t);
        return;
    }

    const loader = c.type_loader orelse {
        return c.reportErrorFmt("Failed to load custom type `{}`.", &.{v(bind_name)}, @ptrCast(decl));
    };
    const info = C.TypeInfo{
        .mod = c.sym.sym().toC(),
        .name = C.toStr(bind_name),
    };
    var host_t: C.HostType = .{
        .data = .{
            .custom = .{
                .out_type_id = null,
                .get_children = null,
                .finalizer = null,
            },
        },
        .type = C.BindTypeCustom,
    };
    log.tracev("Invoke type loader for: {s}", .{bind_name});
    if (!loader(@ptrCast(c.compiler.vm), info, &host_t)) {
        return c.reportErrorFmt("Failed to load custom type `{}`.", &.{v(bind_name)}, @ptrCast(decl));
    }

    try resolveCustomTypeResult(c, sym, host_t);
}

fn resolveCustomType2(c: *cy.Chunk, custom_t: *cy.sym.CustomType,
    get_children: C.GetChildrenFn, finalizer: C.FinalizerFn, opt_type: ?cy.TypeId) !void {
    const typeid = opt_type orelse try c.sema.pushType();
    custom_t.type = typeid;
    c.compiler.sema.types.items[typeid] = .{
        .sym = @ptrCast(custom_t),
        .kind = .custom,
        .data = .{ .custom = .{
            .getChildrenFn = get_children,
            .finalizerFn = finalizer,
        }},
    };
}

fn resolveCustomTypeResult(c: *cy.Chunk, custom_t: *cy.sym.CustomType, host_t: C.HostType) !void {
    switch (host_t.type) {
        C.BindTypeCoreCustom => {
            try resolveCustomType2(c, custom_t,
                host_t.data.core_custom.get_children,
                host_t.data.core_custom.finalizer,
                host_t.data.core_custom.type_id,
            );
        },
        C.BindTypeCustom => {
            try resolveCustomType2(c, custom_t,
                host_t.data.custom.get_children,
                host_t.data.custom.finalizer,
                null,
            );
            if (host_t.data.custom.out_type_id) |out_type_id| {
                out_type_id.* = custom_t.type;
            }
        },
        else => return error.Unsupported,
    }
}

pub fn declareCustomType(c: *cy.Chunk, node: *ast.CustomDecl) !*cy.sym.CustomType {
    const name = c.ast.nodeString(node.name);
    const sym = try c.reserveCustomType(@ptrCast(c.sym), name, node);

    try resolveCustomType(c, sym, node);
    return sym;
}

pub fn reserveDistinctType(c: *cy.Chunk, decl: *ast.DistinctDecl) !*cy.sym.DistinctType {
    const name = c.ast.nodeString(decl.name);

    const sym = try c.reserveDistinctType(@ptrCast(c.sym), name, decl);
    var opt_type: ?cy.TypeId = null;

    // Check for @host modifier.
    if (decl.attrs.len > 0) {
        if (decl.attrs[0].type == .host) {
            const host_name = try getHostAttrName(c, decl.attrs[0]);
            opt_type = try getHostTypeId(c, @ptrCast(sym), host_name, @ptrCast(decl));
        }
    }

    try resolveDistinctTypeId(c, sym, opt_type);
    return sym;
}

pub fn reserveTypeAlias(c: *cy.Chunk, node: *ast.TypeAliasDecl) !*cy.sym.TypeAlias {
    const name = c.ast.nodeString(node.name);
    return c.reserveTypeAlias(@ptrCast(c.sym), name, node);
}

pub fn resolveTypeAlias(c: *cy.Chunk, sym: *cy.sym.TypeAlias) !void {
    sym.type = try cy.sema.resolveTypeSpecNode(c, sym.decl.typeSpec);
    sym.sym = c.sema.types.items[sym.type].sym;
    // Merge modules.
    const mod = sym.getMod();
    const src_sym = sym.sym;
    const src_mod = src_sym.getMod().?;
    var iter = mod.symMap.iterator();
    while (iter.next()) |e| {
        const name = e.key_ptr.*;
        const child = e.value_ptr.*;
        if (src_mod.symMap.contains(name)) {
            try cy.module.reportDupSym(c, child, name, @ptrCast(sym.decl));
        }
        child.parent = src_sym;
        try src_mod.symMap.putNoClobber(c.alloc, name, child);
    }
    mod.retainedVars.clearAndFree(c.alloc);
    mod.symMap.clearAndFree(c.alloc);
    mod.overloadedFuncMap.clearAndFree(c.alloc);
    sym.resolved = true;
}

pub fn reserveUseAlias(c: *cy.Chunk, node: *ast.UseAlias) !*cy.sym.UseAlias {
    const name = c.ast.nodeString(node.name);
    return c.reserveUseAlias(@ptrCast(c.sym), name, @ptrCast(node));
}

pub fn resolveUseAlias(c: *cy.Chunk, sym: *cy.sym.UseAlias) anyerror!void {
    const r_sym = try resolveSym(c, sym.decl.?.cast(.use_alias).target);
    sym.sym = r_sym;
    sym.resolved = true;
}

pub fn declareUseImport(c: *cy.Chunk, node: *ast.ImportStmt) !void {
    var specPath: []const u8 = undefined;
    if (node.spec) |spec| {
        if (spec.type() != .raw_string_lit) {
            // use alias.
            return error.TODO;
        }
        specPath = c.ast.nodeString(spec);
    } else {
        if (node.name.type() == .ident) {
            // Single ident import.
            specPath = c.ast.nodeString(node.name);
        } else {
            // use path.
            return error.TODO;
        }
    }

    if (std.mem.eql(u8, "$global", specPath)) {
        if (c.use_global) {
            return c.reportErrorFmt("`use $global` is already declared.", &.{}, @ptrCast(node));
        }
        c.use_global = true;
        if (c.compiler.global_sym == null) {
            const global = try c.reserveUserVar(@ptrCast(c.compiler.main_chunk.sym), "$global", null);
            c.resolveUserVar(global, bt.Map);
            c.compiler.global_sym = global;

            const func = try c.reserveHostFunc(@ptrCast(c.compiler.main_chunk.sym), "$getGlobal", null, false);
            const func_sig = try c.sema.ensureFuncSig(&.{ bt.Map, bt.Any }, bt.Dyn);
            try c.resolveHostFunc(func, func_sig, cy.bindings.getGlobal);
            c.compiler.get_global = func;
        }
        return;
    }

    var buf: [4096]u8 = undefined;
    const uri = try cy.compiler.resolveModuleUriFrom(c, &buf, specPath, node.spec);

    // resUri is duped. Moves to chunk afterwards.
    const dupedResUri = try c.alloc.dupe(u8, uri);

    // Queue import task.
    var task = cy.compiler.ImportTask{
        .type = .use_alias,
        .from = c,
        .node = node,
        .resolved_spec = dupedResUri,
        .data = undefined,
    };
    if (node.name.type() != .all) {
        const name = c.ast.nodeString(node.name);
        const sym = try c.reserveUseAlias(@ptrCast(c.sym), name, @ptrCast(node));
        task.data = .{ .use_alias = .{
            .sym = sym,
        }};
    }
    try c.compiler.import_tasks.append(c.alloc, task);
}

pub fn declareModuleAlias(c: *cy.Chunk, node: *ast.ImportStmt) !void {
    const name = c.ast.nodeString(node.name);

    var specPath: []const u8 = undefined;
    if (node.spec) |spec| {
        specPath = c.ast.nodeString(spec);
    } else {
        specPath = name;
    }

    var buf: [4096]u8 = undefined;
    const uri = try cy.compiler.resolveModuleUri(c, &buf, specPath, node.spec);

    const import = try c.declareModuleAlias(@ptrCast(c.sym), name, undefined, node);

    // resUri is duped. Moves to chunk afterwards.
    const dupedResUri = try c.alloc.dupe(u8, uri);

    // Queue import task.
    try c.compiler.import_tasks.append(c.alloc, .{
        .type = .module_alias,
        .from = c,
        .node = node,
        .absSpec = dupedResUri,
        .data = .{ .module_alias = .{
            .sym = import,
        }},
    });
}

pub fn reserveEnum(c: *cy.Chunk, node: *ast.EnumDecl) !*cy.sym.EnumType {
    const name = c.ast.nodeString(node.name);
    return c.reserveEnumType(@ptrCast(c.sym), name, node.isChoiceType, node);
}

/// Explicit `decl` node for distinct type declarations. Must belong to `c`.
pub fn declareEnumMembers(c: *cy.Chunk, sym: *cy.sym.EnumType, decl: *ast.EnumDecl) !void {
    const members = try c.alloc.alloc(*cy.sym.EnumMember, decl.members.len);
    for (decl.members, 0..) |member, i| {
        const mName = c.ast.nodeString(member.name);
        var payloadType: cy.TypeId = cy.NullId;
        if (member.typeSpec != null) {
            payloadType = try resolveTypeSpecNode(c, member.typeSpec);
        }
        const modSymId = try c.declareEnumMember(@ptrCast(sym), mName, sym.type, sym.isChoiceType, @intCast(i), payloadType, member);
        members[i] = modSymId;
    }
    sym.members = members.ptr;
    sym.numMembers = @intCast(members.len);
}

/// Only allows binding a predefined host type id (BIND_TYPE_DECL).
pub fn getHostTypeId(c: *cy.Chunk, type_sym: *cy.Sym, opt_name: ?[]const u8, node: ?*ast.Node) !cy.TypeId {
    const bind_name = opt_name orelse type_sym.name();
    if (c.host_types.get(bind_name)) |host_t| {
        if (host_t.type != C.BindTypeCoreDecl) {
            return error.Unsupported;
        }
        return host_t.data.core_decl.type_id;
    }

    const loader = c.type_loader orelse {
        return c.reportErrorFmt("Failed to load @host type `{}`.", &.{v(bind_name)}, node);
    };

    const info = C.TypeInfo{
        .mod = c.sym.sym().toC(),
        .name = C.toStr(bind_name),
    };
    var host_t: C.HostType = .{
        .data = .{
            .custom = .{
                .out_type_id = null,
                .get_children = null,
                .finalizer = null,
            },
        },
        .type = C.BindTypeCustom,
    };
    log.tracev("Invoke type loader for: {s}", .{bind_name});
    if (!loader(@ptrCast(c.compiler.vm), info, &host_t)) {
        return c.reportErrorFmt("Failed to load @host type `{}`.", &.{v(bind_name)}, node);
    }

    if (host_t.type != C.BindTypeCoreDecl) {
        return error.Unsupported;
    }
    return host_t.data.core_decl.type_id;
}

pub const TemplateContext = struct {
    template: *cy.sym.Template,
    variant: *cy.sym.Variant,
};

fn pushTemplateContext(c: *cy.Chunk, template: *cy.sym.Template, variant: *cy.sym.Variant) !?TemplateContext {
    for (template.params, 0..) |param, i| {
        const arg = variant.args[i];
        try c.cur_template_params.put(c.alloc, param.name, arg);
    }
    defer c.cur_template_ctx = .{ .template = template, .variant = variant };
    return c.cur_template_ctx;
}

fn popTemplateContext(c: *cy.Chunk, opt_ctx: ?TemplateContext) !void {
    if (std.meta.eql(c.cur_template_ctx, opt_ctx)) {
        return;
    }
    c.cur_template_params.clearRetainingCapacity();
    if (opt_ctx) |ctx| {
        for (ctx.template.params, 0..) |param, i| {
            const arg = ctx.variant.args[i];
            try c.cur_template_params.put(c.alloc, param.name, arg);
        }
    }
    c.cur_template_ctx = opt_ctx;
}

/// Allow an explicit `opt_header_decl` so that template specialization can use it to override @host attributes.
pub fn reserveTemplateVariant(c: *cy.Chunk, template: *cy.sym.Template, opt_header_decl: ?*ast.Node, variant: *cy.sym.Variant) !*cy.sym.Sym {
    const tchunk = template.chunk();
    const name = template.head.name();
    switch (template.kind) {
        .object_t => {
            const object_t = try c.createObjectType(@ptrCast(c.sym), name, @ptrCast(template.child_decl));
            object_t.variant = variant;

            const header_decl = opt_header_decl orelse template.child_decl;
            try resolveObjectTypeId(c, object_t, header_decl.cast(.objectDecl).attrs);

            return @ptrCast(object_t);
        },
        .custom_t => {
            const custom_t = try c.createCustomType(@ptrCast(c.sym), name, @ptrCast(template.child_decl));
            custom_t.variant = variant;

            const header_decl = opt_header_decl orelse template.child_decl;
            try resolveCustomType(tchunk, custom_t, header_decl.cast(.custom_decl));

            return @ptrCast(custom_t);
        },
        .enum_t => {
            const decl = template.child_decl.cast(.enumDecl);
            const sym = try c.createEnumTypeVariant(@ptrCast(c.sym), template, decl.isChoiceType, variant);
            return @ptrCast(sym);
        },
        .func => {
            const sym = try c.createFuncSym(@ptrCast(c.sym), name);
            sym.variant = variant;

            var func: *cy.Func = undefined;
            const decl = template.child_decl.cast(.funcDecl);
            if (decl.stmts.len == 0) {
                func = try c.createFunc(.hostFunc, @ptrCast(c.sym), sym, @ptrCast(decl), false);
                func.data = .{ .hostFunc = .{
                    .ptr = undefined,
                }};
            } else {
                func = try c.createFunc(.userFunc, @ptrCast(c.sym), sym, @ptrCast(decl), false);
            }
            sym.addFunc(func);
            return @ptrCast(sym);
        },
        else => {
            log.tracev("{}", .{template.kind});
            return error.Unsupported;
        },
    }
}

pub fn resolveTemplateVariant(c: *cy.Chunk, template: *cy.sym.Template, sym: *cy.sym.Sym) !void {
    const tchunk = template.chunk();
    switch (sym.type) {
        .object_t => {
            const object_t = sym.cast(.object_t);
            const object_decl = template.child_decl.cast(.objectDecl);

            const prev = try pushTemplateContext(tchunk, template, object_t.variant.?);
            defer popTemplateContext(tchunk, prev) catch @panic("error");

            try resolveObjectFields(tchunk, @ptrCast(sym), template.child_decl);

            const mod = object_t.getMod();
            const start = mod.chunk.funcs.items.len;
            for (object_decl.funcs) |func_n| {
                const func = try reserveImplicitMethod(c, @ptrCast(object_t), func_n);
                try resolveImplicitMethod(c, func);
            }

            // Defer method sema.
            try mod.chunk.variantFuncSyms.appendSlice(c.alloc, mod.chunk.funcs.items[start..]);
        },
        .custom_t => {
            const custom_t = sym.cast(.custom_t);
            const prev = try pushTemplateContext(tchunk, template, custom_t.variant.?);
            defer popTemplateContext(tchunk, prev) catch @panic("error");

            const mod = custom_t.getMod();
            const start = mod.chunk.funcs.items.len;
            const custom_decl = template.child_decl.cast(.custom_decl);

            for (custom_decl.funcs) |func_n| {
                const func = try reserveImplicitMethod(tchunk, @ptrCast(custom_t), func_n);
                try resolveImplicitMethod(tchunk, func);
            }

            // Resolve explicit functions.
            var iter = template.getMod().symMap.iterator();
            while (iter.next()) |e| {
                if (e.value_ptr.*.type != .template) {
                    return error.Unsupported;
                }
                const child_template = e.value_ptr.*.cast(.template);
                if (child_template.kind != .func) {
                    return error.Unsupported;
                }

                const func_decl = child_template.child_decl.cast(.funcDecl);
                if (func_decl.stmts.len == 0) {
                    const func = try sema.reserveHostFunc2(tchunk, @ptrCast(custom_t), child_template.head.name(), @ptrCast(child_template.child_decl));
                    try sema.resolveFunc(tchunk, func);
                } else {
                    return error.Unsupported;
                }
            }

            // Defer method sema.
            try mod.chunk.variantFuncSyms.appendSlice(c.alloc, mod.chunk.funcs.items[start..]);
        },
        .enum_t => {
            const enum_t = sym.cast(.enum_t);

            const prev = try pushTemplateContext(tchunk, template, enum_t.variant.?);
            defer popTemplateContext(tchunk, prev) catch @panic("error");

            try declareEnumMembers(tchunk, enum_t, @ptrCast(template.child_decl));
        },
        .func => {
            const func = sym.cast(.func);

            const prev = try pushTemplateContext(tchunk, template, func.variant.?);
            defer popTemplateContext(tchunk, prev) catch @panic("error");

            try resolveFunc(tchunk, func.first);
            try c.variantFuncSyms.append(c.alloc, func.first);
        },
        else => {
            return error.Unsupported;
        },
    }
}

pub fn reserveTableMethods(c: *cy.Chunk, obj: *cy.sym.ObjectType) !void {
    const table_mod = c.sema.table_type.getMod();
    const table_c = table_mod.chunk;

    const start = table_c.funcs.items.len;

    _ = try c.reserveHostFunc(@ptrCast(obj), "$get", null, true);
    _ = try c.reserveHostFunc(@ptrCast(obj), "$set", null, true);
    _ = try c.reserveHostFunc(@ptrCast(obj), "$index", null, true);
    _ = try c.reserveHostFunc(@ptrCast(obj), "$setIndex", null, true);

    try table_c.variantFuncSyms.appendSlice(c.alloc, table_c.funcs.items[start..]);
}

pub fn resolveTableMethods(c: *cy.Chunk, obj: *cy.sym.ObjectType) !void {
    const mod = obj.getMod();

    const get = mod.getSym("$get").?.cast(.func).first;
    var sig = try c.sema.ensureFuncSig(&.{ obj.type, bt.String }, bt.Dyn);
    try c.resolveHostFunc(get, sig, cy.bindings.tableGet);

    const set = mod.getSym("$set").?.cast(.func).first;
    sig = try c.sema.ensureFuncSig(&.{ obj.type, bt.String, bt.Any }, bt.Void);
    try c.resolveHostFunc(set, sig, cy.builtins.zErrFunc(cy.bindings.tableSet));

    const index = mod.getSym("$index").?.cast(.func).first;
    sig = try c.sema.ensureFuncSig(&.{ obj.type, bt.Any }, bt.Dyn);
    try c.resolveHostFunc(index, sig, cy.bindings.tableIndex);

    const set_index = mod.getSym("$setIndex").?.cast(.func).first;
    sig = try c.sema.ensureFuncSig(&.{ obj.type, bt.Any, bt.Any }, bt.Void);
    try c.resolveHostFunc(set_index, sig, cy.builtins.zErrFunc(cy.bindings.tableSet));
}

pub fn reserveTableType(c: *cy.Chunk, decl: *ast.TableDecl) !*cy.sym.ObjectType {
    const name = c.ast.nodeString(decl.name);
    const sym = try c.reserveObjectType(@ptrCast(c.sym), name, @ptrCast(decl));
    try resolveObjectTypeId(c, sym, decl.attrs);
    return sym;
}

pub fn reserveObjectType(c: *cy.Chunk, decl: *ast.ObjectDecl) !*cy.sym.ObjectType {
    if (decl.name == null) {
        // Unnamed object.
        var buf: [16]u8 = undefined;
        const name = c.getNextUniqUnnamedIdent(&buf);
        const nameDup = try c.alloc.dupe(u8, name);
        try c.parser.ast.strs.append(c.alloc, nameDup);
        return c.createObjectTypeUnnamed(@ptrCast(c.sym), nameDup, decl);
    }

    const name = c.ast.nodeString(decl.name.?);
    const sym = try c.reserveObjectType(@ptrCast(c.sym), name, @ptrCast(decl));
    try resolveObjectTypeId(c, sym, decl.attrs);
    return sym;
}

fn resolveObjectTypeId(c: *cy.Chunk, sym: *cy.sym.ObjectType, attrs: []const *ast.Attribute) !void {
    var opt_type: ?cy.TypeId = null;

    // Check for @host modifier.
    if (attrs.len > 0) {
        if (attrs[0].type == .host) {
            const name = try getHostAttrName(c, attrs[0]);
            opt_type = try getHostTypeId(c, @ptrCast(sym), name, sym.decl);
        }
    }

    try resolveObjectTypeId2(c, sym, opt_type);
}

pub fn resolveObjectTypeId2(c: *cy.Chunk, object_t: *cy.sym.ObjectType, opt_type: ?cy.TypeId) !void {
    const typeid = opt_type orelse try c.sema.pushType();
    object_t.type = typeid;
    c.compiler.sema.types.items[typeid] = .{
        .sym = @ptrCast(object_t),
        .kind = .object,
        .data = .{ .object = .{
            .numFields = cy.NullU16,
        }},
    };
}

pub fn reserveStruct(c: *cy.Chunk, node: *ast.ObjectDecl) !*cy.sym.ObjectType {
    if (node.name == null) {
        // Unnamed.
        var buf: [16]u8 = undefined;
        const name = c.getNextUniqUnnamedIdent(&buf);
        const nameDup = try c.alloc.dupe(u8, name);
        try c.parser.ast.strs.append(c.alloc, nameDup);
        return c.createStructTypeUnnamed(@ptrCast(c.sym), nameDup, node);
    }

    const name = c.ast.nodeString(node.name.?);
    const sym = try c.reserveStructType(@ptrCast(c.sym), name, node);

    // Check for @host modifier.
    var opt_type: ?cy.TypeId = null;
    if (node.attrs.len > 0) {
        if (node.attrs[0].type == .host) {
            const host_name = try getHostAttrName(c, node.attrs[0]);
            opt_type = try getHostTypeId(c, @ptrCast(sym), host_name, @ptrCast(node));
        }
    }

    try resolveStructTypeId(c, sym, opt_type);
    return @ptrCast(sym);
}

pub fn resolveDistinctType(c: *cy.Chunk, distinct_t: *cy.sym.DistinctType) !*cy.Sym {
    const decl = distinct_t.decl;

    const target_t = try resolveTypeSpecNode(c, decl.target);
    const target_sym = c.sema.getTypeSym(target_t);
    const name = distinct_t.head.name();
    var new_sym: *cy.Sym = undefined;
    switch (target_sym.type) {
        .object_t => {
            const object_t = target_sym.cast(.object_t);
            const new = try c.createObjectType(distinct_t.head.parent.?, name, object_t.decl);
            try resolveObjectTypeId2(c, new, distinct_t.type);
            new.getMod().* = distinct_t.getMod().*;
            new.getMod().updateParentRefs(@ptrCast(new));

            try sema.resolveObjectFields(c, @ptrCast(new), object_t.decl.?);
            new_sym = @ptrCast(new);
        },
        .bool_t => {
            const new = try c.createBoolType(distinct_t.head.parent.?, name, distinct_t.type);
            new.getMod().* = distinct_t.getMod().*;
            new.getMod().updateParentRefs(@ptrCast(new));

            new_sym = @ptrCast(new);
        },
        .int_t => {
            const int_t = target_sym.cast(.int_t);

            const new = try c.createIntType(distinct_t.head.parent.?, name, int_t.bits, distinct_t.type);
            new.getMod().* = distinct_t.getMod().*;
            new.getMod().updateParentRefs(@ptrCast(new));
            new_sym = @ptrCast(new);
        },
        .float_t => {
            const float_t = target_sym.cast(.float_t);
            const new = try c.createFloatType(distinct_t.head.parent.?, name, float_t.bits, distinct_t.type);
            new.getMod().* = distinct_t.getMod().*;
            new.getMod().updateParentRefs(@ptrCast(new));

            new_sym = @ptrCast(new);
        },
        else => {
            return error.Unsupported;
        },
    }
    try distinct_t.head.parent.?.getMod().?.symMap.put(c.alloc, name, new_sym);
    distinct_t.resolved = true;
    return new_sym;
}

pub fn resolveTableFields(c: *cy.Chunk, obj: *cy.sym.ObjectType) !void {
    const node = obj.decl.?.cast(.table_decl);

    const fields = try c.alloc.alloc(cy.sym.FieldInfo, node.fields.len + 1);
    errdefer c.alloc.free(fields);

    // First field is reserved as `table_data Map`.
    var field_sym = try c.declareField(@ptrCast(obj), "table_data", 0, bt.Map, null);
    fields[0] = .{
        .sym = @ptrCast(field_sym),
        .type = bt.Map,
    };

    // Load custom fields.
    for (node.fields, 1..) |field, i| {
        const field_name = c.ast.nodeString(field);
        field_sym = try c.declareField(@ptrCast(obj), field_name, i, bt.Dyn, @ptrCast(field));
        fields[i] = .{
            .sym = @ptrCast(field_sym),
            .type = bt.Dyn,
        };
    }
    obj.fields = fields.ptr;
    obj.numFields = @intCast(fields.len);
    c.sema.types.items[obj.type].data.object = .{
        .numFields = @intCast(obj.numFields),
    };
}

pub fn resolveDistinctTypeId(c: *cy.Chunk, distinct_t: *cy.sym.DistinctType, opt_type: ?cy.TypeId) !void {
    const typeid = opt_type orelse try c.sema.pushType();
    distinct_t.type = typeid;
    c.compiler.sema.types.items[typeid] = .{
        .sym = @ptrCast(distinct_t),
        .kind = .null,
        .data = undefined,
    };
}

pub fn resolveStructTypeId(c: *cy.Chunk, struct_t: *cy.sym.ObjectType, opt_type: ?cy.TypeId) !void {
    const typeid = opt_type orelse try c.sema.pushType();
    struct_t.type = typeid;
    c.compiler.sema.types.items[typeid] = .{
        .sym = @ptrCast(struct_t),
        .kind = .@"struct",
        .data = .{ .@"struct" = .{
            .numFields = cy.NullU16,
        }},
    };
}

/// Explicit `decl` node for distinct type declarations. Must belong to `c`.
pub fn resolveObjectFields(c: *cy.Chunk, object_like: *cy.Sym, decl: *ast.Node) !void {
    var obj: *cy.sym.ObjectType = undefined;
    var object_decl: *ast.ObjectDecl = undefined;
    switch (object_like.type) {
        .object_t => {
            obj = object_like.cast(.object_t);
            object_decl = decl.cast(.objectDecl);
        },
        .struct_t => {
            obj = object_like.cast(.struct_t);
            object_decl = decl.cast(.structDecl);
        },
        else => {
            return error.Unsupported;
        },
    }
    if (obj.isResolved()) {
        return;
    }

    // Load fields.
    const fields = try c.alloc.alloc(cy.sym.FieldInfo, object_decl.fields.len);
    errdefer c.alloc.free(fields);

    for (object_decl.fields, 0..) |field, i| {
        const fieldName = c.ast.nodeString(field.name);
        const fieldType = try resolveTypeSpecNode(c, field.typeSpec);

        const sym = try c.declareField(@ptrCast(obj), fieldName, i, fieldType, @ptrCast(field));
        fields[i] = .{
            .sym = @ptrCast(sym),
            .type = fieldType,
        };
    }
    obj.fields = fields.ptr;
    obj.numFields = @intCast(fields.len);
    switch (object_like.type) {
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

pub fn reserveHostFunc(c: *cy.Chunk, node: *ast.FuncDecl) !*cy.Func {
    if (node.stmts.len > 0) {
        return error.Unexpected;
    }
    // Check if @host func.
    const decl_path = try ensureDeclNamePath(c, @ptrCast(c.sym), node.name);
    if (node.attrs.len > 0) {
        if (node.attrs[0].type == .host) {
            const is_method = c.ast.isMethodDecl(node);
            return c.reserveHostFunc(decl_path.parent, decl_path.name.base_name, node, is_method);
        }
    }
    return c.reportErrorFmt("`{}` is not a host function.", &.{v(decl_path.name.name_path)}, @ptrCast(node));
}

pub fn reserveHostFunc2(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, node: *ast.FuncDecl) !*cy.Func {
    if (node.stmts.len > 0) {
        return error.Unexpected;
    }
    // Check if @host func.
    if (node.attrs.len > 0) {
        if (node.attrs[0].type == .host) {
            const is_method = c.ast.isMethodDecl(node);
            return c.reserveHostFunc(parent, name, node, is_method);
        }
    }
    return c.reportErrorFmt("`{}` is not a host function.", &.{v(name)}, @ptrCast(node));
}

pub fn resolveFunc(c: *cy.Chunk, func: *cy.Func) !void {
    switch (func.type) {
        .hostFunc => {
            if (func.is_implicit_method) {
                try sema.resolveImplicitMethod(c, func);
            } else {
                try sema.resolveHostFunc(c, func);
            }
        },
        .userFunc => {
            if (func.is_implicit_method) {
                try sema.resolveImplicitMethod(c, func);
            } else {
                try sema.resolveUserFunc(c, func);
            }
        },
        .userLambda => {},
    }
}

pub fn getHostAttrName(c: *cy.Chunk, attr: *ast.Attribute) !?[]const u8 {
    const value = attr.value orelse {
        return null;
    };
    if (value.type() != .raw_string_lit) {
        return error.Unsupported;
    }
    return c.ast.nodeString(value);
}

pub fn resolveHostFunc(c: *cy.Chunk, func: *cy.Func) !void {
    if (func.isResolved()) {
        return;
    }
    const func_sig = try resolveFuncSig(c, func);
    try resolveHostFunc2(c, func, func_sig);
}

fn resolveHostFunc2(c: *cy.Chunk, func: *cy.Func, func_sig: FuncSigId) !void {
    const decl = func.decl.?.cast(.funcDecl);
    const attr = decl.attrs[0];
    const host_name = try getHostAttrName(c, attr);
    var name_buf: [128]u8 = undefined;
    const bind_name = host_name orelse b: {
        var fbs = std.io.fixedBufferStream(&name_buf);
        try cy.sym.writeFuncName(c.sema, fbs.writer(), func, .{ .from = c, .emit_template_args = false });
        break :b fbs.getWritten();
    };

    if (c.host_funcs.get(bind_name)) |ptr| {
        try c.resolveHostFunc(func, func_sig, @ptrCast(@alignCast(ptr)));
        return;
    }

    const loader = c.func_loader orelse {
        return c.reportErrorFmt("Host func `{}` failed to load.", &.{v(bind_name)}, @ptrCast(func.decl));
    };
    const parent = func.parent;
    const info = C.FuncInfo{
        .mod = parent.toC(),
        .name = C.toStr(bind_name),
        .funcSigId = func_sig,
    };

    log.tracev("Invoke func loader for: {s}", .{bind_name});
    var res: C.FuncFn = null;
    if (!loader(@ptrCast(c.compiler.vm), info, @ptrCast(&res))) {
        return c.reportErrorFmt("Host func `{}` failed to load.", &.{v(bind_name)}, @ptrCast(func.decl));
    }
    try c.resolveHostFunc(func, func_sig, @ptrCast(@alignCast(res)));
}

/// Declares a bytecode function in a given module.
pub fn reserveUserFunc(c: *cy.Chunk, decl: *ast.FuncDecl) !*cy.Func {
    const decl_path = try ensureDeclNamePath(c, @ptrCast(c.sym), decl.name);
    const is_method = c.ast.isMethodDecl(decl);
    return c.reserveUserFunc(decl_path.parent, decl_path.name.base_name, decl, is_method);
}

pub fn resolveUserFunc(c: *cy.Chunk, func: *cy.Func) !void {
    const func_sig = try resolveFuncSig(c, func);
    try c.resolveUserFunc(func, func_sig);
}

pub fn reserveImplicitMethod(c: *cy.Chunk, parent: *cy.Sym, decl: *ast.FuncDecl) !*cy.Func {
    const decl_path = try ensureDeclNamePath(c, parent, decl.name);
    if (decl.stmts.len > 0) {
        const func = try c.reserveUserFunc(decl_path.parent, decl_path.name.base_name, decl, true);
        func.is_implicit_method = true;
        return func;
    }

    // No initializer. Check if @host func.
    if (decl.attrs.len > 0) {
        if (decl.attrs[0].type == .host) {
            const func = try c.reserveHostFunc(decl_path.parent, decl_path.name.base_name, decl, true);
            func.is_implicit_method = true;
            return func;
        }
    }
    return c.reportErrorFmt("`{}` does not have an initializer.", &.{v(decl_path.name.name_path)}, @ptrCast(decl));
}

pub fn resolveImplicitMethod(c: *cy.Chunk, func: *cy.Func) !void {
    const func_sig = try resolveImplicitMethodSig(c, func);
    if (func.type == .hostFunc) {
        try resolveHostFunc2(c, func, func_sig);
    } else {
        try c.resolveUserFunc(func, func_sig);
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
    try semaStmts(c, func.decl.?.cast(.funcDecl).stmts);
    try popFuncBlock(c);
    func.emitted = true;
}

pub fn funcDecl(c: *cy.Chunk, func: *cy.Func) !void {
    const node = func.decl.?.cast(.funcDecl);
    _ = try pushFuncProc(c, func);
    try appendFuncParamVars(c, func, node.params);
    try semaStmts(c, node.stmts);

    try popFuncBlock(c);
    func.emitted = true;
}

pub fn reserveVar(c: *cy.Chunk, decl: *ast.StaticVarDecl) !*Sym {
    const decl_path = try ensureDeclNamePath(c, @ptrCast(c.sym), decl.name);
    if (decl.right == null) {
        // No initializer. Check if @host var.
        if (decl.attrs.len > 0) {
            if (decl.attrs[0].type == .host) {
                return @ptrCast(try c.reserveHostVar(decl_path.parent, decl_path.name.base_name, decl));
            }
        }
        return c.reportErrorFmt("`{}` does not have an initializer.", &.{v(decl_path.name.name_path)}, @ptrCast(decl));
    } else {
        return @ptrCast(try c.reserveUserVar(decl_path.parent, decl_path.name.base_name, decl));
    }
}

pub fn resolveUserVar(c: *cy.Chunk, sym: *cy.sym.UserVar) !void {
    if (sym.isResolved()) {
        return;
    }
    const typeId = try resolveTypeSpecNode(c, sym.decl.?.typeSpec);
    c.resolveUserVar(sym, typeId);
}

pub fn resolveHostVar(c: *cy.Chunk, sym: *cy.sym.HostVar) !void {
    const decl = sym.decl.?;
    const name = c.ast.getNamePathInfo(decl.name);

    const info = C.VarInfo{
        .mod = c.sym.sym().toC(),
        .name = C.toStr(name.name_path),
        .idx = c.curHostVarIdx,
    };
    c.curHostVarIdx += 1;
    const varLoader = c.varLoader orelse {
        return c.reportErrorFmt("No var loader set for `{}`.", &.{v(name.name_path)}, @ptrCast(decl));
    };
    log.tracev("Invoke var loader for: {s}", .{name.name_path});
    var out: cy.Value = cy.Value.initInt(0);
    if (!varLoader(@ptrCast(c.compiler.vm), info, @ptrCast(&out))) {
        return c.reportErrorFmt("Host var `{}` failed to load.", &.{v(name.name_path)}, @ptrCast(decl));
    }
    // var type.
    const typeId = try resolveTypeSpecNode(c, decl.typeSpec);
    // c.ast.node(node.data.staticDecl.varSpec).next = typeId;

    const outTypeId = out.getTypeId();
    if (!cy.types.isTypeSymCompat(c.compiler, outTypeId, typeId)) {
        const expTypeName = c.sema.getTypeBaseName(typeId);
        const actTypeName = c.sema.getTypeBaseName(outTypeId);
        return c.reportErrorFmt("Host var `{}` expects type {}, got: {}.", &.{v(name.name_path), v(expTypeName), v(actTypeName)}, @ptrCast(decl));
    }

    try c.resolveHostVar(sym, typeId, out);
}

fn declareParam(c: *cy.Chunk, param: ?*ast.FuncParam, isSelf: bool, paramIdx: u32, declT: TypeId) !void {
    var name: []const u8 = undefined;
    if (isSelf) {
        name = "self";
    } else {
        name = c.ast.funcParamName(param.?);
    }

    const proc = c.proc();
    if (!isSelf) {
        if (proc.nameToVar.get(name)) |varInfo| {
            if (varInfo.blockId == c.semaBlocks.items.len - 1) {
                return c.reportErrorFmt("Function param `{}` is already declared.", &.{v(name)}, @ptrCast(param));
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

fn declareLocalName(c: *cy.Chunk, name: []const u8, declType: TypeId, hasInit: bool, node: *ast.Node) !LocalVarId {
    const proc = c.proc();
    if (proc.nameToVar.get(name)) |varInfo| {
        if (varInfo.blockId == c.semaBlocks.items.len - 1) {
            const svar = &c.varStack.items[varInfo.varId];
            if (svar.isParentLocalAlias()) {
                return c.reportErrorFmt("`{}` already references a parent local variable.", &.{v(name)}, node);
            } else if (svar.type == .staticAlias) {
                return c.reportErrorFmt("`{}` already references a static variable.", &.{v(name)}, node);
            } else {
                return c.reportErrorFmt("Variable `{}` is already declared in the block.", &.{v(name)}, node);
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

    return try declareLocalName2(c, name, declType, false, hasInit, node);
}

fn declareLocalName2(c: *cy.Chunk, name: []const u8, declType: TypeId, hidden: bool, hasInit: bool, node: *ast.Node) !LocalVarId {
    const id = try pushLocalVar(c, .local, name, declType, hidden);
    var svar = &c.varStack.items[id];

    const proc = c.proc();
    const irId = proc.curNumLocals;

    var irIdx: u32 = undefined;
    if (hasInit) {
        irIdx = try c.ir.pushStmt(c.alloc, .declareLocalInit, node, .{
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
        irIdx = try c.ir.pushStmt(c.alloc, .declareLocal, node, .{
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

fn declareLocal(c: *cy.Chunk, ident: *ast.Node, declType: TypeId, hasInit: bool) !LocalVarId {
    const name = c.ast.nodeString(ident);
    return declareLocalName(c, name, declType, hasInit, ident);
}

fn localDecl(c: *cy.Chunk, node: *ast.VarDecl) !void {
    var typeId: cy.TypeId = undefined;
    var inferType = false;
    if (node.typed) {
        if (node.typeSpec == null) {
            inferType = true;
            typeId = bt.Any;
        } else {
            typeId = try resolveTypeSpecNode(c, node.typeSpec);
        }
    } else {
        typeId = bt.Dyn;
    }

    const varId = try declareLocal(c, node.name, typeId, true);
    var svar = &c.varStack.items[varId];
    const irIdx = svar.inner.local.declIrStart;

    const maxLocalsBeforeInit = c.proc().curNumLocals;

    // Infer rhs type and enforce constraint.
    const right = try c.semaExpr(node.right, .{
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
        const declType = right.type.toDeclType();
        svar.declT = declType;
        svar.vtype = right.type;
        // Patch IR.
        data.declType = declType;
    } else if (typeId == bt.Dyn) {
        // Update recent static type.
        svar.vtype.id = right.type.id;
    }

    try c.assignedVarStack.append(c.alloc, varId);
}

pub fn staticDecl(c: *cy.Chunk, sym: *Sym, node: *ast.StaticVarDecl) !void {
    c.curInitingSym = sym;
    c.curInitingSymDeps.clearRetainingCapacity();
    defer c.curInitingSym = null;
    const depStart = c.symInitDeps.items.len;

    const irIdx = try c.ir.pushEmptyStmt(c.alloc, .setVarSym, @ptrCast(node));
    const recLoc = try c.ir.pushExpr(.varSym, c.alloc, bt.Void, @ptrCast(node), .{ .sym = sym });
    const symType = CompactType.init((try sym.getValueType()).?);
    const right = try semaExprCType(c, node.right.?, symType);
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
    target_t: TypeId = cy.NullId,
    req_target_t: bool = false,
};

fn semaExprCType(c: *cy.Chunk, node: *ast.Node, ctype: CompactType) !ExprResult {
    return try c.semaExpr(node, .{
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
    global,
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

    node: *ast.Node,
    target_t: TypeId,

    fn init(node: *ast.Node) Expr {
        return .{
            .node = node,
            .target_t = bt.Any,
            .reqTypeCstr = false,
        };
    }

    fn hasTargetType(self: Expr) bool {
        return self.target_t != cy.NullId;
    }

    fn getRetCstr(self: Expr) ReturnCstr {
        if (self.target_t == bt.Void) {
            return .any;
        } else {
            return .not_void;
        }
    }
};

fn requireFuncSym(c: *cy.Chunk, sym: *Sym, node: *ast.Node) !*cy.sym.FuncSym {
    if (sym.type != .func) {
        return c.reportErrorFmt("Expected `{}` to be a function symbol.", &.{v(sym.name())}, node);
    }
    return sym.cast(.func);
}

// Invoke a type's sym as the callee.
fn callTypeSym(c: *cy.Chunk, preIdx: u32, sym: *Sym, symNodeId: *ast.Node, args: []*ast.Node, typeId: cy.TypeId, ret_cstr: ReturnCstr) !ExprResult {
    _ = typeId;
    const typeCallSym = try c.getResolvedDistinctSym(sym, "$call", null, true);
    const funcSym = try requireFuncSym(c, typeCallSym, symNodeId);
    return c.semaCallFuncSym(preIdx, funcSym, args, ret_cstr, symNodeId);
}

fn callTemplateSym(c: *cy.Chunk, loc: u32, template: *cy.sym.Template, args: []*ast.Node, ret_cstr: ReturnCstr, node: *ast.Node) !ExprResult {
    if (template.kind != .func) {
        return error.Unsupported;
    }

    if (!template.canInferFuncTemplateArgs()) {
        return c.reportError("Can not infer template params.", node);
    }
    if (template.infer_min_params > args.len) {
        return c.reportError("Can not infer template params.", node);
    }

    // First resolve args up to `min_infer_params` without a target arg type.
    const start = c.typeStack.items.len;
    defer c.typeStack.items.len = start;
    const args_loc = try c.ir.pushEmptyArray(c.alloc, u32, args.len);
    var arg_idx: u32 = 0;
    while (arg_idx < template.infer_min_params) {
        const arg_res = try c.semaExpr(args[arg_idx], .{});
        c.ir.setArrayItem(args_loc, u32, arg_idx, arg_res.irIdx);
        try c.typeStack.append(c.alloc, arg_res.type.id);
        arg_idx += 1;
    }

    // Infer template params.
    const targ_start = c.valueStack.items.len;
    try c.valueStack.resize(c.alloc, targ_start + template.params.len);
    const targs = c.valueStack.items[targ_start..];
    @memset(targs, cy.Value.Void);
    defer {
        c.valueStack.items.len = targ_start;
        for (targs) |targ| {
            c.vm.release(targ);
        }
    }
    for (template.params, 0..) |param, i| {
        c.valueStack.items[targ_start + i] = try c.vm.allocType(c.typeStack.items[start+param.infer_from_func_param]);
    }
    const targ_types_start = c.typeStack.items.len;
    try c.typeStack.resize(c.alloc, targ_types_start + template.params.len);
    const targ_types = c.typeStack.items[targ_types_start..];
    @memset(targ_types, bt.Type);
    const func_sym = (try cte.expandTemplate(c, template, targs, targ_types)).cast(.func);
    const func = func_sym.first;
    c.typeStack.items.len = targ_types_start;

    // The rest of the args are resolved with target arg type.
    const sig = c.sema.getFuncSig(func.funcSigId);
    const params = sig.params();
    while (arg_idx < args.len) {
        var arg_res: ExprResult = undefined;
        if (arg_idx < params.len) {
            arg_res = try c.semaExprTarget(args[arg_idx], params[arg_idx]);
        } else {
            arg_res = try c.semaExpr(args[arg_idx], .{});
        }
        c.ir.setArrayItem(args_loc, u32, arg_idx, arg_res.irIdx);
        try c.typeStack.append(c.alloc, arg_res.type.id);
        arg_idx += 1;
    }
    const arg_types = c.typeStack.items[start..];

    // Check func sig against args.
    const func_params = c.sema.getFuncSig(func.funcSigId).params();
    if (func_params.len != arg_types.len) {
        return reportIncompatibleCallSig(c, func_sym, arg_types, ret_cstr, node);
    }
    for (arg_types, 0..) |arg_t, i| {
        if (!cy.types.isTypeSymCompat(c.compiler, arg_t, func_params[i])) {
            return reportIncompatibleCallSig(c, func_sym, arg_types, ret_cstr, node);
        }
    }

    c.ir.setExprCode(loc, .preCallFuncSym);
    c.ir.setExprType2(loc, .{ .id = @intCast(func.retType), .throws = func.throws });
    c.ir.setExprData(loc, .preCallFuncSym, .{ .callFuncSym = .{
        .func = func, .numArgs = @as(u8, @intCast(args.len)), .args = args_loc,
    }});
    return ExprResult.init(loc, CompactType.init(func.retType));
}

fn callSym(c: *cy.Chunk, preIdx: u32, sym: *Sym, symNode: *ast.Node, args: []*ast.Node, ret_cstr: ReturnCstr) !ExprResult {
    try referenceSym(c, sym, symNode);
    switch (sym.type) {
        .func => {
            const funcSym = sym.cast(.func);
            return c.semaCallFuncSym(preIdx, funcSym, args, ret_cstr, symNode);
        },
        .bool_t => {
            const type_id = sym.cast(.bool_t).type;
            return callTypeSym(c, preIdx, sym, symNode, args, type_id, ret_cstr);
        }, 
        .int_t => {
            const type_id = sym.cast(.int_t).type;
            return callTypeSym(c, preIdx, sym, symNode, args, type_id, ret_cstr);
        }, 
        .float_t => {
            const type_id = sym.cast(.float_t).type;
            return callTypeSym(c, preIdx, sym, symNode, args, type_id, ret_cstr);
        }, 
        .custom_t => {
            const type_id = sym.cast(.custom_t).type;
            return callTypeSym(c, preIdx, sym, symNode, args, type_id, ret_cstr);
        },
        .struct_t => {
            const type_id = sym.cast(.struct_t).type;
            return callTypeSym(c, preIdx, sym, symNode, args, type_id, ret_cstr);
        },
        .object_t => {
            const type_id = sym.cast(.object_t).type;
            return callTypeSym(c, preIdx, sym, symNode, args, type_id, ret_cstr);
        },
        .enumMember => {
            const member = sym.cast(.enumMember);
            if (member.payloadType == cy.NullId) {
                return c.reportErrorFmt("Can not initialize choice member without a payload type.", &.{}, symNode);
            }
            if (args.len != 1) {
                return c.reportErrorFmt("Expected only one payload argument for choice initializer. Found `{}`.", &.{v(args.len)}, symNode);
            }
            const payload = try c.semaExprCstr(args[0], member.payloadType);
            return semaInitChoice(c, member, payload, symNode);
        },
        .userVar,
        .hostVar => {
            // preCall.
            const callee = try sema.symbol(c, sym, symNode, true);
            const args_loc = try c.semaPushDynCallArgs(args);
            return c.semaCallValue(preIdx, callee.irIdx, args.len, args_loc);
        },
        .template => {
            return callTemplateSym(c, preIdx, sym.cast(.template), args, ret_cstr, symNode);
        },
        else => {
            // try pushCallArgs(c, node.data.callExpr.argHead, numArgs, true);
            log.tracev("{}", .{sym.type});
            return error.TODO;
        },
    }
}

pub fn symbol(c: *cy.Chunk, sym: *Sym, node: *ast.Node, symAsValue: bool) !ExprResult {
    try referenceSym(c, sym, node);
    if (symAsValue) {
        const typeId = (try sym.getValueType()) orelse {
            return c.reportErrorFmt("Can't use symbol `{}` as a value.", &.{v(sym.name())}, node);
        };
        const ctype = CompactType.init(typeId);
        switch (sym.type) {
            .userVar,
            .hostVar => {
                const loc = try c.ir.pushExpr(.varSym, c.alloc, typeId, node, .{ .sym = sym });
                return ExprResult.initCustom(loc, .varSym, ctype, .{ .varSym = sym });
            },
            .func => {
                // `symbol` being invoked suggests the func sym is not ambiguous.
                const funcSym = sym.cast(.func);
                const loc = try c.ir.pushExpr(.funcSym, c.alloc, typeId, node, .{ .func = funcSym.first });
                return ExprResult.initCustom(loc, .func, ctype, .{ .func = funcSym.first });
            },
            .struct_t => {
                const objTypeId = sym.cast(.struct_t).type;
                const loc = try c.ir.pushExpr(.typeSym, c.alloc, typeId, node, .{ .typeId = objTypeId });
                return ExprResult.init(loc, ctype);
            },
            .object_t => {
                const objTypeId = sym.cast(.object_t).type;
                const irIdx = try c.ir.pushExpr(.typeSym,c.alloc, typeId, node, .{ .typeId = objTypeId });
                return ExprResult.init(irIdx, ctype);
            },
            .custom_t => {
                const type_id = sym.cast(.custom_t).type;
                const irIdx = try c.ir.pushExpr(.typeSym, c.alloc, typeId, node, .{ .typeId = type_id });
                return ExprResult.init(irIdx, ctype);
            },
            .bool_t => {
                const type_id = sym.cast(.bool_t).type;
                const irIdx = try c.ir.pushExpr(.typeSym, c.alloc, typeId, node, .{ .typeId = type_id });
                return ExprResult.init(irIdx, ctype);
            },
            .int_t => {
                const type_id = sym.cast(.int_t).type;
                const irIdx = try c.ir.pushExpr(.typeSym, c.alloc, typeId, node, .{ .typeId = type_id });
                return ExprResult.init(irIdx, ctype);
            },
            .float_t => {
                const type_id = sym.cast(.float_t).type;
                const irIdx = try c.ir.pushExpr(.typeSym, c.alloc, typeId, node, .{ .typeId = type_id });
                return ExprResult.init(irIdx, ctype);
            },
            .enumMember => {
                const member = sym.cast(.enumMember);
                if (member.is_choice_type) {
                    return semaInitChoiceNoPayload(c, member, node);
                } else {
                    const irIdx = try c.ir.pushExpr(.enumMemberSym, c.alloc, typeId, node, .{
                        .type = member.type,
                        .val = @as(u8, @intCast(member.val)),
                    });
                    return ExprResult.init(irIdx, ctype);
                }
            },
            else => {
                return error.Unsupported;
            }
        }
    } else {
        if (sym.isVariable()) {
            const typeId = (try sym.getValueType()) orelse return error.Unexpected;
            const ctype = CompactType.init(typeId);
            const loc = try c.ir.pushExpr(.varSym, c.alloc, typeId, node, .{ .sym = sym });
            return ExprResult.initCustom(loc, .varSym, ctype, .{ .varSym = sym });
        }
        var typeId: CompactType = undefined;
        if (sym.isType()) {
            typeId = CompactType.init(sym.getStaticType().?);
        } else {
            typeId = CompactType.init((try sym.getValueType()) orelse bt.Void);
        }
        return ExprResult.initCustom(cy.NullId, .sym, typeId, .{ .sym = sym });
    }
}

fn semaLocal(c: *cy.Chunk, id: LocalVarId, node: *ast.Node) !ExprResult {
    const svar = c.varStack.items[id];
    switch (svar.type) {
        .local => {
            const loc = try c.ir.pushExpr(.local, c.alloc, svar.vtype.id, node, .{ .id = svar.inner.local.id });
            return ExprResult.initCustom(loc, .local, svar.vtype, .{ .local = id });
        },
        .objectMemberAlias => {
            const rec_t = c.varStack.items[c.proc().varStart].declT;
            const rec_loc = try c.ir.pushExpr(.local, c.alloc, rec_t, node, .{ .id = 0 });
            const loc = try c.ir.pushExpr(.field, c.alloc, svar.vtype.id, node, .{
                .idx = svar.inner.objectMemberAlias.fieldIdx,
                .rec = rec_loc,
                .numNestedFields = 0,
            });
            return ExprResult.initCustom(loc, .field, svar.vtype, undefined);
        },
        .parentObjectMemberAlias => {
            const rec_t = c.varStack.items[svar.inner.parentObjectMemberAlias.parentVarId].declT;
            const rec_loc = try c.ir.pushExpr(.captured, c.alloc, rec_t, node, .{ .idx = svar.inner.parentObjectMemberAlias.selfCapturedIdx });
            const loc = try c.ir.pushExpr(.field, c.alloc, svar.vtype.id, node, .{
                .idx = svar.inner.parentObjectMemberAlias.fieldIdx,
                .rec = rec_loc,
                .numNestedFields = 0,
            });
            return ExprResult.init(loc, svar.vtype);
        },
        .parentLocalAlias => {
            const loc = try c.ir.pushExpr(.captured, c.alloc, svar.vtype.id, node, .{ .idx = svar.inner.parentLocalAlias.capturedIdx });
            return ExprResult.initCustom(loc, .capturedLocal, svar.vtype, undefined);
        },
        else => {
            return c.reportErrorFmt("Unsupported: {}", &.{v(svar.type)}, null);
        },
    }
}

fn semaIdent(c: *cy.Chunk, node: *ast.Node, symAsValue: bool) !ExprResult {
    const name = c.ast.nodeString(node);
    const res = try getOrLookupVar(c, name, node);
    switch (res) {
        .global => |sym| {
            const map = try symbol(c, sym, node, true);
            const key = try c.semaString(name, node);

            const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, node);
            const get_global = c.compiler.get_global.?.sym.?;
            var expr_res = try c.semaCallFuncSym2(loc, get_global, node, map, node, key, .any, node);
            expr_res.resType = .global;
            return expr_res;
        },
        .local => |id| {
            return semaLocal(c, id, node);
        },
        .static => |sym| {
            return sema.symbol(c, sym, node, symAsValue);
        },
    }
}

pub fn getLocalDistinctSym(c: *cy.Chunk, name: []const u8, node: *ast.Node) !?*Sym {
    // if (c.sym_cache.get(name)) |sym| {
    //     if (!sym.isDistinct()) {
    //         return c.reportErrorFmt("`{}` is not a unique symbol.", &.{v(name)}, node);
    //     }
    //     return sym;
    // }

    if (try c.getDistinctSym(@ptrCast(c.sym), name, node, false)) |res| {
        // try c.sym_cache.putNoClobber(c.alloc, name, res);
        return res;
    }
    return null;
}

pub fn getResolvedLocalSym(c: *cy.Chunk, name: []const u8, node: *ast.Node, distinct: bool) !?*Sym {
    if (c.sym_cache.get(name)) |sym| {
        if (distinct and !sym.isDistinct()) {
            return c.reportErrorFmt("`{}` is not a unique symbol.", &.{v(name)}, node);
        }
        return sym;
    }

    // Look in the current chunk module.
    if (distinct) {
        if (try c.getResolvedDistinctSym(@ptrCast(c.sym), name, node, false)) |res| {
            try c.sym_cache.putNoClobber(c.alloc, name, res);
            return res;
        }
    } else {
        if (try c.getOptResolvedSym(@ptrCast(c.sym), name)) |sym| {
            try c.sym_cache.putNoClobber(c.alloc, name, sym);
            return sym;
        }
    }

    // Look in template context.
    if (c.cur_template_params.size > 0) {
        if (c.cur_template_params.get(name)) |param| {
            if (param.getTypeId() != bt.Type) {
                const param_type_name = c.sema.getTypeBaseName(param.getTypeId());
                return c.reportErrorFmt("Can not use a `{}` template param here.", &.{v(param_type_name)}, node);
            }
            const sym = c.sema.getTypeSym(param.asHeapObject().type.type);
            if (!distinct or sym.isDistinct()) {
                return sym;
            }
        }
    }

    // Look in use alls.
    var i = c.use_alls.items.len;
    while (i > 0) {
        i -= 1;
        const mod_sym = c.use_alls.items[i];
        if (distinct) {
            if (try c.getResolvedDistinctSym(@ptrCast(mod_sym), name, node, false)) |res| {
                try c.sym_cache.putNoClobber(c.alloc, name, res);
                return res;
            }
        } else {
            if (try c.getOptResolvedSym(@ptrCast(mod_sym), name)) |sym| {
                try c.sym_cache.putNoClobber(c.alloc, name, sym);
                return sym;
            }
        }
    }
    return null;
}

/// If no type spec, default to `dynamic` type.
/// Returns the final resolved type sym.
pub fn resolveTypeSpecNode(c: *cy.Chunk, node: ?*ast.Node) anyerror!cy.TypeId {
    const n = node orelse {
        return bt.Dyn;
    };
    const type_id = try resolveSymType(c, n);
    if (type_id == bt.Void) {
        return c.reportErrorFmt("`void` can not be used as common type specifier.", &.{}, n);
    }
    return type_id;
}

pub fn resolveReturnTypeSpecNode(c: *cy.Chunk, node: ?*ast.Node) anyerror!cy.TypeId {
    const n = node orelse {
        return bt.Dyn;
    };
    return resolveSymType(c, n);
}

/// Creates `Placeholder`s for missing parent symbols.
fn ensureLocalNamePathSym(c: *cy.Chunk, path: []*ast.Node) !*Sym {
    var name = c.ast.nodeString(path[0]);
    var sym = (try getLocalDistinctSym(c, name, path[0])) orelse b: {
        const placeholder = try c.addPlaceholder(@ptrCast(c.sym), name);
        break :b @as(*cy.Sym, @ptrCast(placeholder));
    };
    for (path[1..]) |n| {
        name = c.ast.nodeString(n);
        sym = try c.getDistinctSym(sym, name, n, true);
    }
    return sym;
}

fn resolveSymType(c: *cy.Chunk, expr_id: *ast.Node) !cy.TypeId {
    const sym = try resolveSym(c, expr_id);
    return sym.getStaticType() orelse {
        switch (sym.type) {
            .template => {
                return c.reportErrorFmt("Expected a type symbol. `{}` is a type template and must be expanded to a type first.", &.{v(sym.name())}, expr_id);
            },
            else => {
                return c.reportErrorFmt("`{}` is not a type symbol. Found `{}`.", &.{v(sym.name()), v(sym.type)}, expr_id);
            }
        }
    };
}

pub fn resolveSym(c: *cy.Chunk, expr: *ast.Node) !*cy.Sym {
    switch (expr.type()) {
        .objectDecl => {
            // Unnamed object.
            return @ptrCast(expr.cast(.objectDecl).name);
        },
        .void => {
            return c.sema.getTypeSym(bt.Void);
        },
        .ident => {
            const name = c.ast.nodeString(expr);

            if (c.in_ct_expr) {
                // Check for ct builtins.
                const mod = c.compiler.ct_builtins_chunk.?.sym.getMod();
                if (mod.getSym(name)) |sym| {
                    return sym;
                }
            }

            return (try getResolvedLocalSym(c, name, expr, true)) orelse {
                return c.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, expr);
            };
        },
        .accessExpr => {
            const access_expr = expr.cast(.accessExpr);
            const parent = try resolveSym(c, access_expr.left);
            if (access_expr.right.type() != .ident) {
                return c.reportErrorFmt("Expected identifier.", &.{}, access_expr.right);
            }
            const name = c.ast.nodeString(access_expr.right);
            return c.getResolvedDistinctSym(parent, name, access_expr.right, true);
        },
        .comptimeExpr => {
            if (c.in_ct_expr) {
                return c.reportErrorFmt("Unexpected compile-time expression.", &.{}, expr);
            }
            c.in_ct_expr = true;
            defer c.in_ct_expr = false;
            // TODO: Check for ct builtins.
            return resolveSym(c, expr.cast(.comptimeExpr).child);
        },
        .array_expr => {
            const array_expr = expr.cast(.array_expr);
            var left = try resolveSym(c, array_expr.left);
            if (left.type == .template) {
                return cte.expandTemplateOnCallArgs(c, left.cast(.template), array_expr.args, expr);
            }
            return c.reportErrorFmt("Unsupported array expression.", &.{}, expr);
        },
        .semaSym => {
            return expr.cast(.semaSym).sym;
        },
        .callExpr => {
            return try cte.expandTemplateOnCallExpr(c, expr.cast(.callExpr));
        },
        .expandOpt => {
            return try cte.expandTemplateOnCallArgs(c, c.sema.option_tmpl, &.{expr.cast(.expandOpt).param}, expr);
        },
        else => {
            return c.reportErrorFmt("Unsupported symbol expr: `{}`", &.{v(expr.type())}, expr);
        }
    }
}

pub fn resolveImplicitMethodSig(c: *cy.Chunk, func: *cy.Func) !FuncSigId {
    const func_n = func.decl.?.cast(.funcDecl);

    // Get params, build func signature.
    const start = c.typeStack.items.len;
    defer c.typeStack.items.len = start;

    // First param is always `self`.
    try c.typeStack.append(c.alloc, func.parent.getStaticType().?);

    for (func_n.params) |param| {
        const paramName = c.ast.funcParamName(param);
        if (std.mem.eql(u8, "self", paramName)) {
            return c.reportErrorFmt("`self` param is not allowed in an implicit method declaration.", &.{}, @ptrCast(param));
        }
        const typeId = try resolveTypeSpecNode(c, param.typeSpec);
        try c.typeStack.append(c.alloc, typeId);
    }

    // Get return type.
    const retType = try resolveReturnTypeSpecNode(c, func_n.ret);
    return c.sema.ensureFuncSig(c.typeStack.items[start..], retType);
}

fn resolveTemplateSig(c: *cy.Chunk, params: []*ast.FuncParam, outSigId: *FuncSigId) ![]cy.sym.TemplateParam {
    const typeStart = c.typeStack.items.len;
    defer c.typeStack.items.len = typeStart;

    const tparams = try c.alloc.alloc(cy.sym.TemplateParam, params.len);
    errdefer c.alloc.free(tparams);

    for (params, 0..) |param, i| {
        if (param.typeSpec == null) {
            return c.reportErrorFmt("Expected param type specifier.", &.{}, @ptrCast(param));
        }
        const typeId = try resolveTypeSpecNode(c, param.typeSpec);
        try c.typeStack.append(c.alloc, typeId);
        tparams[i] = .{
            .name = c.ast.funcParamName(param),
            .type = typeId,
            .infer_from_func_param = cy.NullU8,
        };
    }

    const retType = bt.Type;
    outSigId.* = try c.sema.ensureFuncSig(c.typeStack.items[typeStart..], retType);
    return tparams;
}

fn resolveFuncSig(c: *cy.Chunk, func: *cy.Func) !FuncSigId {
    const func_n = func.decl.?.cast(.funcDecl);

    // Get params, build func signature.
    const start = c.typeStack.items.len;
    defer c.typeStack.items.len = start;

    const sig_t = func_n.sig_t;
    if (sig_t == .infer) {
        return error.Unexpected;
    }

    for (func_n.params) |param| {
        const paramName = c.ast.funcParamName(param);
        if (std.mem.eql(u8, paramName, "self")) {
            try c.typeStack.append(c.alloc, func.parent.getStaticType().?);
        } else {
            if (sig_t == .func) {
                if (param.typeSpec == null) {
                    return c.reportError("Expected type specifier.", @ptrCast(param));
                }
            } else {
                if (param.typeSpec != null) {
                    return c.reportError("Type specifier not allowed in `let` declaration. Declare typed functions with `func`.", @ptrCast(param));
                }
            }
            const typeId = try resolveTypeSpecNode(c, param.typeSpec);
            try c.typeStack.append(c.alloc, typeId);
        }
    }

    // Get return type.
    const retType = try resolveReturnTypeSpecNode(c, func_n.ret);
    return c.sema.ensureFuncSig(c.typeStack.items[start..], retType);
}

fn resolveLambdaFuncSig(c: *cy.Chunk, n: *ast.LambdaExpr) !FuncSigId {
    // Get params, build func signature.
    const start = c.typeStack.items.len;
    defer c.typeStack.items.len = start;

    for (n.params) |param| {
        const paramName = c.ast.funcParamName(param);
        if (std.mem.eql(u8, paramName, "self")) {
            return c.reportError("`self` is a reserved parameter for methods.", @ptrCast(param));
        }
        if (n.sig_t == .func) {
            if (param.typeSpec == null) {
                return c.reportError("Expected type specifier.", @ptrCast(param));
            }
        } else {
            if (param.typeSpec != null) {
                return c.reportError("Type specifier not allowed. Declare typed lambdas with `func`.", @ptrCast(param));
            }
        }
        const typeId = try resolveTypeSpecNode(c, param.typeSpec);
        try c.typeStack.append(c.alloc, typeId);
    }

    // Get return type.
    const retType = try resolveReturnTypeSpecNode(c, n.ret);
    return c.sema.ensureFuncSig(c.typeStack.items[start..], retType);
}

const DeclNamePathResult = struct {
    name: cy.ast.NamePathInfo,
    parent: *cy.Sym,
};

fn ensureDeclNamePath(c: *cy.Chunk, parent: *cy.Sym, name_n: *ast.Node) !DeclNamePathResult {
    const name = c.ast.getNamePathInfo(name_n);
    if (name_n.type() != .name_path) {
        return .{
            .name = name,
            .parent = parent,
        };
    } else {
        const name_path = name_n.cast(.name_path).path;
        const explicit_parent = try ensureLocalNamePathSym(c, name_path[0..name_path.len-1]);
        return .{
            .name = name,
            .parent = explicit_parent,
        };
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
    const idx = try c.ir.pushEmptyExpr(.lambda, c.alloc, ir.ExprType.init(bt.Lambda), @ptrCast(func.decl));
    // Reserve param info.
    _ = try c.ir.pushEmptyArray(c.alloc, ir.FuncParam, func.numParams);

    const id = try pushProc(c, func);
    c.proc().irStart = idx;
    return id;
}

pub fn pushFuncProc(c: *cy.Chunk, func: *cy.Func) !ProcId {
    const idx = try c.ir.pushEmptyStmt(c.alloc, .funcBlock, @ptrCast(func.decl));
    // Reserve param info.
    _ = try c.ir.pushEmptyArray(c.alloc, ir.FuncParam, func.numParams);

    const id = try pushProc(c, func);
    c.proc().irStart = idx;
    return id;
}

pub fn semaMainBlock(compiler: *cy.Compiler, mainc: *cy.Chunk) !u32 {
    const irIdx = try mainc.ir.pushEmptyStmt(compiler.alloc, .mainBlock, @ptrCast(mainc.ast.root));

    const id = try pushProc(mainc, null);
    mainc.mainSemaProcId = id;

    if (!compiler.cont) {
        if (compiler.global_sym) |global| {
            const set = try mainc.ir.pushEmptyStmt(compiler.alloc, .setVarSym, mainc.ast.null_node);
            const sym = try mainc.ir.pushExpr(.varSym, compiler.alloc, bt.Map, mainc.ast.null_node, .{ .sym = @as(*cy.Sym, @ptrCast(global)) });
            const map = try mainc.semaMap(mainc.ast.null_node);
            mainc.ir.setStmtData(set, .setVarSym, .{ .generic = .{
                .left_t = CompactType.initStatic(bt.Map),
                .right_t = CompactType.initStatic(bt.Map),
                .left = sym,
                .right = map.irIdx,
            }});
        }
    }

    // Emit IR for initializers. DFS order. Stop at circular dependency.
    // TODO: Allow circular dependency between chunks but not symbols.
    for (compiler.newChunks()) |c| {
        if (c.hasStaticInit and !c.initializerVisited) {
            try visitChunkInit(compiler, c);
        }
    }

    // Main.
    try semaStmts(mainc, mainc.ast.root.?.stmts);

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
            return c.reportErrorFmt("Referencing `{}` created a circular module dependency.", &.{v(c.srcUri)}, null);
        }
        try visitChunkInit(self, depChunk);
    }

    // Once all deps are visited, emit IR.
    const func = c.sym.getMod().getSym("$init").?.cast(.func).first;
    const exprLoc = try self.main_chunk.ir.pushExpr(.preCallFuncSym, c.alloc, bt.Void, c.ast.null_node, .{ .callFuncSym = .{
        .func = func, .numArgs = 0, .args = 0,
    }});
    _ = try self.main_chunk.ir.pushStmt(c.alloc, .exprStmt, c.ast.null_node, .{
        .expr = exprLoc,
        .isBlockResult = false,
    });

    c.initializerVisiting = false;
    c.initializerVisited = true;
}

pub fn pushProc(self: *cy.Chunk, func: ?*cy.Func) !ProcId {
    var isStaticFuncBlock = false;
    var node: *ast.Node = undefined;
    if (func != null) {
        isStaticFuncBlock = func.?.isStatic();
        node = @ptrCast(func.?.decl);
    } else {
        node = @ptrCast(self.ast.root);
    }

    const new = Proc.init(node, func, @intCast(self.semaBlocks.items.len), isStaticFuncBlock, @intCast(self.varStack.items.len));
    const idx = self.semaProcs.items.len;
    try self.semaProcs.append(self.alloc, new);

    try pushBlock(self, node);
    return @intCast(idx);
}

fn preLoop(c: *cy.Chunk, node: *ast.Node) !void {
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
                    _ = try c.ir.pushStmt(c.alloc, .setLocalType, node, .{ .local = svar.inner.local.id, .type = svar.vtype });
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
                        _ = try c.ir.pushStmt(c.alloc, .setLocalType, b.node, .{
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
            _ = try c.ir.pushStmt(c.alloc, .setLocalType, pblock.node, .{ .local = svar.inner.local.id, .type = svar.vtype });
        }
    }
    c.preLoopVarSaveStack.items.len = b.preLoopVarSaveStart;
    return stmtBlock;
}

fn pushBlock(c: *cy.Chunk, node: *ast.Node) !void {
    try c.ir.pushStmtBlock(c.alloc);
    c.proc().blockDepth += 1;
    const new = Block.init(
        node,
        c.assignedVarStack.items.len,
        c.varStack.items.len,
        c.varShadowStack.items.len,
    );
    try c.semaBlocks.append(c.alloc, new);
}

fn pushMethodParamVars(c: *cy.Chunk, objectT: TypeId, func: *const cy.Func) !void {
    const curNode = c.curNode;
    defer c.curNode = curNode;

    const rFuncSig = c.compiler.sema.funcSigs.items[func.funcSigId];
    const params = rFuncSig.params();

    const param_decls = func.decl.?.cast(.funcDecl).params;
    if (param_decls.len > 0) {
        const name = c.ast.funcParamName(param_decls[0]);
        if (std.mem.eql(u8, name, "self")) {
            try declareParam(c, param_decls[0], false, 0, objectT);
            for (params[1..], 1..) |paramT, idx| {
                try declareParam(c, param_decls[idx], false, @intCast(idx), paramT);
            }
        } else {
            // Implicit `self` param.
            try declareParam(c, null, true, 0, objectT);
            // First param.
            try declareParam(c, param_decls[0], false, 1, params[1]);

            for (params[2..], 2..) |paramT, idx| {
                try declareParam(c, param_decls[idx-1], false, @intCast(idx), paramT);
            }
        }
    } else {
        // Implicit `self` param.
        try declareParam(c, null, true, 0, objectT);
    }
}

fn appendFuncParamVars(c: *cy.Chunk, func: *const cy.Func, params: []const *ast.FuncParam) !void {
    if (func.numParams > 0) {
        const rFuncSig = c.compiler.sema.funcSigs.items[func.funcSigId];
        for (rFuncSig.params(), 0..) |paramT, idx| {
            try declareParam(c, params[idx], false, @intCast(idx), paramT);
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

fn referenceSym(c: *cy.Chunk, sym: *Sym, node: *ast.Node) !void {
    if (!c.isInStaticInitializer()) {
        return;
    }

    if (c.curInitingSym == sym) {
        const name = c.ast.nodeString(node);
        return c.reportErrorFmt("Reference to `{}` creates a circular dependency.", &.{v(name)}, node);
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
        try c.symInitDeps.append(c.alloc, .{ .sym = sym, .refNodeId = node });
    } else {
        // Module dep.
        // Record this symbol's chunk as a dependency.
        try c.symInitChunkDeps.put(c.alloc, chunk, {});
    }
}

const VarLookupResult = union(enum) {
    global: *Sym,
    static: *Sym,

    /// Local, parent local alias, or parent object member alias.
    local: LocalVarId,
};

/// Static var lookup is skipped for callExpr since there is a chance it can fail on a
/// symbol with overloaded signatures.
pub fn getOrLookupVar(self: *cy.Chunk, name: []const u8, node: *ast.Node) !VarLookupResult {
    if (self.semaProcs.items.len == 1 and self.isInStaticInitializer()) {
        return (try lookupStaticVar(self, name, node)) orelse {
            if (self.use_global) {
                return VarLookupResult{ .global = @ptrCast(self.compiler.global_sym.?) };
            }
            return self.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, node);
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
                return self.reportErrorFmt("Can not reference local `{}` in a static initializer.", &.{v(name)}, node);
            }
            return VarLookupResult{
                .local = varInfo.varId,
            };
        } else {
            if (self.isInStaticInitializer() and self.semaBlockDepth() == 0) {
                return self.reportErrorFmt("Can not reference local `{}` in a static initializer.", &.{v(name)}, node);
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
            return self.reportErrorFmt("Can not capture the local variable `{}` from static function `{}`.\nOnly lambdas (anonymous functions) can capture local variables.", &.{v(name), v(funcName)}, self.curNode);
        }

        // Create a local captured variable.
        const parentVar = self.varStack.items[res.varId];
        if (res.isObjectField) {
            const parentBlock = &self.semaProcs.items[res.blockIdx];
            const selfId = parentBlock.nameToVar.get("self").?.varId;
            const resVarId = res.varId;
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
        const res = (try lookupStaticVar(self, name, node)) orelse {
            if (self.use_global) {
                return VarLookupResult{ .global = @ptrCast(self.compiler.global_sym.?) };
            }
            return self.reportErrorFmt("Undeclared variable `{}`.", &.{v(name)}, node);
        };
        _ = try pushStaticVarAlias(self, name, res.static);
        return res;
    }
}

fn lookupStaticVar(c: *cy.Chunk, name: []const u8, node: *ast.Node) !?VarLookupResult {
    const res = (try getResolvedLocalSym(c, name, node, false)) orelse {
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

fn reportIncompatibleCallSig(c: *cy.Chunk, sym: *cy.sym.FuncSym, args: []const cy.TypeId, ret_cstr: ReturnCstr, node: *ast.Node) anyerror {
    const name = sym.head.name();
    var msg: std.ArrayListUnmanaged(u8) = .{};
    const w = msg.writer(c.alloc);
    const callSigStr = try c.sema.allocTypesStr(args, c);
    defer c.alloc.free(callSigStr);

    try w.print("Can not find compatible function for call: `{s}{s}`.", .{name, callSigStr});
    if (ret_cstr == .not_void) {
        try w.writeAll(" Expects non-void return.");
    }
    try w.writeAll("\n");
    const parent_name = try cy.sym.allocSymName(c.sema, c.alloc, sym.head.parent.?, .{ .from = c });
    defer c.alloc.free(parent_name);
    try w.print("Functions named `{s}` in `{s}`:\n", .{name, parent_name });

    var funcStr = try c.sema.formatFuncSig(sym.firstFuncSig, &cy.tempBuf, c);
    try w.print("    func {s}{s}", .{name, funcStr});
    if (sym.numFuncs > 1) {
        var cur: ?*cy.Func = sym.first.next;
        while (cur) |curFunc| {
            try w.writeByte('\n');
            funcStr = try c.sema.formatFuncSig(curFunc.funcSigId, &cy.tempBuf, c);
            try w.print("    func {s}{s}", .{name, funcStr});
            cur = curFunc.next;
        }
    }
    try c.compiler.addReportConsume(.compile_err, try msg.toOwnedSlice(c.alloc), c.id, node.pos());
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

fn checkTypeCstr(c: *cy.Chunk, ctype: CompactType, cstrTypeId: TypeId, node: *ast.Node) !void {
    return checkTypeCstr2(c, ctype, cstrTypeId, cstrTypeId, node);
}

fn checkTypeCstr2(c: *cy.Chunk, ctype: CompactType, cstrTypeId: TypeId, reportCstrTypeId: TypeId, node: *ast.Node) !void {
    // Dynamic is allowed.
    if (!ctype.dynamic) {
        if (!cy.types.isTypeSymCompat(c.compiler, ctype.id, cstrTypeId)) {
            const cstrName = try c.sema.allocTypeName(reportCstrTypeId);
            defer c.alloc.free(cstrName);
            const typeName = try c.sema.allocTypeName(ctype.id);
            defer c.alloc.free(typeName);
            return c.reportErrorFmt("Expected type `{}`, got `{}`.", &.{v(cstrName), v(typeName)}, node);
        }
    }
}

fn pushExprRes(c: *cy.Chunk, res: ExprResult) !void {
    try c.exprResStack.append(c.alloc, res);
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

fn semaSwitchStmt(c: *cy.Chunk, block: *ast.SwitchBlock) !void {
    // Perform sema on expr first so it knows how to construct the rest of the switch.
    const expr = try c.semaExpr(block.expr, .{});
    var info = SwitchInfo.init(c, expr, true);

    var blockLoc: u32 = undefined;
    var exprLoc: u32 = undefined;
    if (info.exprIsChoiceType) {
        blockLoc = try c.ir.pushStmt(c.alloc, .block, @ptrCast(block), .{ .bodyHead = cy.NullId });
        try pushBlock(c, @ptrCast(block));
        exprLoc = try semaSwitchChoicePrologue(c, &info, expr, block.expr);
    } else {
        exprLoc = expr.irIdx;
    }

    _ = try c.ir.pushStmt(c.alloc, .switchStmt, @ptrCast(block), {});
    _ = try semaSwitchBody(c, info, block, exprLoc);

    if (info.exprIsChoiceType) {
        const stmtBlock = try popBlock(c);
        const block_ = c.ir.getStmtDataPtr(blockLoc, .block);
        block_.bodyHead = stmtBlock.first;
    }
}

fn semaSwitchExpr(c: *cy.Chunk, block: *ast.SwitchBlock) !ExprResult {
    // Perform sema on expr first so it knows how to construct the rest of the switch.
    const expr = block.expr;
    const expr_res = try c.semaExpr(expr, .{});
    var info = SwitchInfo.init(c, expr_res, false);

    var blockExprLoc: u32 = undefined;
    var exprLoc: u32 = undefined;
    if (info.exprIsChoiceType) {
        blockExprLoc = try c.ir.pushExpr(.blockExpr, c.alloc, bt.Void, @ptrCast(block), .{ .bodyHead = cy.NullId });
        try pushBlock(c, @ptrCast(block));

        exprLoc = try semaSwitchChoicePrologue(c, &info, expr_res, expr);
    } else {
        exprLoc = expr_res.irIdx;
    }

    if (info.exprIsChoiceType) {
    }
    const irIdx = try semaSwitchBody(c, info, block, exprLoc);

    if (info.exprIsChoiceType) {
        _ = try c.ir.pushStmt(c.alloc, .exprStmt, @ptrCast(block), .{
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
fn semaSwitchChoicePrologue(c: *cy.Chunk, info: *SwitchInfo, expr: ExprResult, exprId: *ast.Node) !u32 {
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

fn semaSwitchBody(c: *cy.Chunk, info: SwitchInfo, block: *ast.SwitchBlock, exprLoc: u32) !u32 {
    const irIdx = try c.ir.pushExpr(.switchExpr, c.alloc, bt.Any, @ptrCast(block), .{
        .numCases = @intCast(block.cases.len),
        .expr = exprLoc,
    });
    const irCasesIdx = try c.ir.pushEmptyArray(c.alloc, u32, block.cases.len);

    for (block.cases, 0..) |case, i| {
        const irCaseIdx = try semaSwitchCase(c, info, @ptrCast(case));
        c.ir.setArrayItem(irCasesIdx, u32, i, irCaseIdx);

    }
    return irIdx;
}

fn semaSwitchElseCase(c: *cy.Chunk, info: SwitchInfo, case: *ast.CaseBlock) !u32 {
    const irIdx = try c.ir.pushEmptyExpr(.switchCase, c.alloc, undefined, @ptrCast(case));

    var bodyHead: u32 = undefined;
    if (case.bodyIsExpr) {
        // Wrap in block expr.
        const body: *ast.Node = @ptrCast(@alignCast(case.stmts.ptr));
        bodyHead = try c.ir.pushExpr(.blockExpr, c.alloc, bt.Any, body, .{ .bodyHead = cy.NullId });
        try pushBlock(c, @ptrCast(case));

        const expr = try c.semaExpr(body, .{});
        _ = try c.ir.pushStmt(c.alloc, .exprStmt, body, .{
            .expr = expr.irIdx,
            .isBlockResult = true,
        });
        const stmtBlock = try popBlock(c);
        const blockExpr = c.ir.getExprDataPtr(bodyHead, .blockExpr);
        blockExpr.bodyHead = stmtBlock.first;
    } else {
        try pushBlock(c, @ptrCast(case));

        if (!info.isStmt) {
            return c.reportErrorFmt("Assign switch statement requires a return case: `else => {expr}`", &.{}, @ptrCast(case));
        }

        try semaStmts(c, case.stmts);
        const stmtBlock = try popBlock(c);
        bodyHead = stmtBlock.first;
    }

    c.ir.setExprData(irIdx, .switchCase, .{
        .numConds = 0,
        .bodyIsExpr = case.bodyIsExpr,
        .bodyHead = bodyHead,
    });
    return irIdx;
}

fn semaSwitchCase(c: *cy.Chunk, info: SwitchInfo, case: *ast.CaseBlock) !u32 {
    if (case.conds.len == 0) {
        return semaSwitchElseCase(c, info, case);
    }

    const irIdx = try c.ir.pushEmptyExpr(.switchCase, c.alloc, undefined, @ptrCast(case));

    const hasCapture = case.capture != null;

    var case_capture_type: cy.TypeId = cy.NullId;
    if (case.conds.len > 0) {
        const conds_loc = try c.ir.pushEmptyArray(c.alloc, u32, case.conds.len);
        for (case.conds, 0..) |cond, i| {
            if (try semaCaseCond(c, info, conds_loc, cond, i)) |capture_type| {
                if (case_capture_type == cy.NullId) {
                    case_capture_type = capture_type;
                } else {
                    // TODO: Handle multiple cond capture types.
                }
            }
        }
    }

    var bodyHead: u32 = undefined;
    if (case.bodyIsExpr) {
        // Wrap in block expr.
        bodyHead = try c.ir.pushExpr(.blockExpr, c.alloc, bt.Any, @ptrCast(@alignCast(case.stmts.ptr)), .{ .bodyHead = cy.NullId });
        try pushBlock(c, @ptrCast(case));
    } else {
        try pushBlock(c, @ptrCast(case));
    }

    if (hasCapture) {
        const declT = if (info.exprType.dynamic) bt.Dyn else case_capture_type;
        const capVarId = try declareLocal(c, case.capture.?, declT, true);
        const declareLoc = c.varStack.items[capVarId].inner.local.declIrStart;

        // Copy payload to captured var.
        const recLoc = try c.ir.pushExpr(.local, c.alloc, info.exprType.id, case.capture.?, .{ .id = info.choiceIrVarId });
        const fieldLoc = try c.ir.pushExpr(.field, c.alloc, declT, case.capture.?, .{
            .idx = 1,
            .rec = recLoc,
            .numNestedFields = 0,
        });

        const declare = c.ir.getStmtDataPtr(declareLoc, .declareLocalInit);
        declare.init = fieldLoc;
        declare.initType = CompactType.init(declT);
    }

    if (case.bodyIsExpr) {
        const body: *ast.Node = @ptrCast(@alignCast(case.stmts.ptr));
        const expr = try c.semaExpr(body, .{});
        _ = try c.ir.pushStmt(c.alloc, .exprStmt, body, .{
            .expr = expr.irIdx,
            .isBlockResult = true,
        });
        const stmtBlock = try popBlock(c);
        const blockExpr = c.ir.getExprDataPtr(bodyHead, .blockExpr);
        blockExpr.bodyHead = stmtBlock.first;
    } else {
        if (!info.isStmt) {
            return c.reportErrorFmt("Assign switch statement requires a return case: `case {cond} => {expr}`", &.{}, @ptrCast(case));
        }

        try semaStmts(c, case.stmts);
        const stmtBlock = try popBlock(c);
        bodyHead = stmtBlock.first;
    }

    c.ir.setExprData(irIdx, .switchCase, .{
        .numConds = @intCast(case.conds.len),
        .bodyIsExpr = case.bodyIsExpr,
        .bodyHead = bodyHead,
    });
    return irIdx;
}

fn semaCaseCond(c: *cy.Chunk, info: SwitchInfo, conds_loc: u32, cond: *ast.Node, cond_idx: usize) !?cy.TypeId {
    if (info.exprIsChoiceType) {
        if (cond.type() == .dot_lit) {
            if (info.exprTypeSym.type != .enum_t) {
                const type_name = info.exprTypeSym.name();
                return c.reportErrorFmt("Can only match symbol literal for an enum type. Found `{}`.", &.{v(type_name)}, cond);
            }
            const name = c.ast.nodeString(cond);
            if (info.exprTypeSym.cast(.enum_t).getMember(name)) |member| {
                const condRes = try c.semaInt(member.val, cond);
                c.ir.setArrayItem(conds_loc, u32, cond_idx, condRes.irIdx);
                return member.payloadType;
            } else {
                const targetTypeName = info.exprTypeSym.name();
                return c.reportErrorFmt("`{}` is not a member of `{}`", &.{v(name), v(targetTypeName)}, cond);
            }
        } else {
            const targetTypeName = info.exprTypeSym.name();
            return c.reportErrorFmt("Expected to match a member of `{}`", &.{v(targetTypeName)}, cond);
        }
    }

    // General case.
    const condRes = try c.semaExpr(cond, .{});
    c.ir.setArrayItem(conds_loc, u32, cond_idx, condRes.irIdx);
    return null;
}

pub const ChunkExt = struct {

    pub fn semaZeroInit(c: *cy.Chunk, typeId: cy.TypeId, node: *ast.Node) !ExprResult {
        switch (typeId) {
            bt.Any,
            bt.Dyn  => return c.semaInt(0, node),
            bt.Boolean  => return c.semaFalse(node),
            bt.Integer  => return c.semaInt(0, node),
            bt.Float    => return c.semaFloat(0, node),
            bt.ListDyn  => return c.semaEmptyList(node),
            bt.Map      => return c.semaMap(node),
            bt.Array    => return c.semaArray("", node),
            bt.String   => return c.semaString("", node),
            else => {
                const sym = c.sema.getTypeSym(typeId);
                if (sym.type != .object_t) {
                    return error.Unsupported;
                }

                const obj = sym.cast(.object_t);
                const irArgsIdx = try c.ir.pushEmptyArray(c.alloc, u32, obj.numFields);
                const irIdx = try c.ir.pushExpr(.object_init, c.alloc, typeId, node, .{
                    .typeId = obj.type, .numArgs = @as(u8, @intCast(obj.numFields)), .args = irArgsIdx,
                });
                for (obj.fields[0..obj.numFields], 0..) |field, i| {
                    const arg = try semaZeroInit(c, field.type, node);
                    c.ir.setArrayItem(irArgsIdx, u32, i, arg.irIdx);
                }
                return ExprResult.initStatic(irIdx, typeId);
            },
        }
    }

    /// `initializerId` is a record literal node.
    pub fn semaObjectInit2(c: *cy.Chunk, obj: *cy.sym.ObjectType, initializer: *ast.RecordLit) !ExprResult {
        const node: *ast.Node = @ptrCast(initializer);
        const type_e = c.sema.types.items[obj.type];
        if (type_e.has_init_pair_method) {
            if (obj.numFields == 0) {
                const init = try c.ir.pushExpr(.object_init, c.alloc, obj.type, node, .{
                    .typeId = obj.type, .numArgs = 0, .args = 0,
                });
                return semaWithInitPairs(c, @ptrCast(obj), obj.type, initializer, init);
            } else {
                // Attempt to initialize fields with zero values.
                const fields = obj.getFields();
                const args_loc = try c.ir.pushEmptyArray(c.alloc, u32, fields.len);
                for (fields, 0..) |field, i| {
                    try c.checkForZeroInit(field.type, node);
                    // const type_name = try c.sema.allocTypeName(obj.type);
                    // defer c.alloc.free(type_name);
                    // return c.reportErrorFmt("Type `{}` can not initialize with `$initPair` since it does not have a default record initializer.", &.{v(type_name)}, initializerId);
                    const arg = try c.semaZeroInit(field.type, node);
                    c.ir.setArrayItem(args_loc, u32, i, arg.irIdx);
                }
                const init = try c.ir.pushExpr(.object_init, c.alloc, obj.type, node, .{
                    .typeId = obj.type, .numArgs = @as(u8, @intCast(fields.len)), .args = args_loc,
                });
                return semaWithInitPairs(c, @ptrCast(obj), obj.type, initializer, init);
            }
        }

        // Set up a temp buffer to map initializer entries to type fields.
        const fieldsDataStart = c.listDataStack.items.len;
        try c.listDataStack.resize(c.alloc, c.listDataStack.items.len + obj.numFields);
        defer c.listDataStack.items.len = fieldsDataStart;

        const undecl_start = c.listDataStack.items.len;
        _ = undecl_start;

        // Initially set to NullId so missed mappings are known from a linear scan.
        const fieldNodes = c.listDataStack.items[fieldsDataStart..];
        @memset(fieldNodes, .{ .node = null });

        for (initializer.args) |arg| {
            const fieldName = c.ast.nodeString(@ptrCast(arg.key));
            const info = try checkSymInitField(c, @ptrCast(obj), fieldName, arg.key);
            // if (info.use_get_method) {
            //     try c.listDataStack.append(c.alloc, .{ .node = entryId });
            //     entryId = entry.next();
            //     continue;
            // }

            fieldNodes[info.idx] = .{ .node = arg.value };
        }

        const irIdx = try c.ir.pushEmptyExpr(.object_init, c.alloc, ir.ExprType.init(obj.type), node);
        const irArgsIdx = try c.ir.pushEmptyArray(c.alloc, u32, obj.numFields);

        for (0..fieldNodes.len) |i| {
            const item = c.listDataStack.items[fieldsDataStart+i];
            const fieldT = obj.fields[i].type;
            if (item.node == null) {
                // Check that unset fields can be zero initialized.
                try c.checkForZeroInit(fieldT, node);

                const arg = try c.semaZeroInit(fieldT, node);
                c.ir.setArrayItem(irArgsIdx, u32, i, arg.irIdx);
            } else {
                const arg = try c.semaExprCstr(item.node.?, fieldT);
                c.ir.setArrayItem(irArgsIdx, u32, i, arg.irIdx);
            }
        }

        c.ir.setExprData(irIdx, .object_init, .{
            .typeId = obj.type, .numArgs = @as(u8, @intCast(obj.numFields)), .args = irArgsIdx,
        });

        // TODO: Implement with $initPairMissing.
        // const undecls = c.listDataStack.items[undecl_start..];
        // if (undecls.len > 0) {
        //     // Initialize object, then invoke $set on the undeclared fields.
        //     const expr = try c.ir.pushExpr(.blockExpr, c.alloc, obj.type, initializerId, .{ .bodyHead = cy.NullId });
        //     try pushBlock(c, initializerId);
        //     {
        //         // create temp with object.
        //         const var_id = try declareLocalName(c, "$temp", obj.type, true, initializerId);
        //         const temp_ir_id = c.varStack.items[var_id].inner.local.id;
        //         const temp_ir = c.varStack.items[var_id].inner.local.declIrStart;
        //         const decl_stmt = c.ir.getStmtDataPtr(temp_ir, .declareLocalInit);
        //         decl_stmt.init = irIdx;
        //         decl_stmt.initType = CompactType.initStatic(obj.type);

        //         // setFieldDyn.
        //         const temp_expr = try c.ir.pushExpr(.local, c.alloc, obj.type, initializerId, .{ .id = temp_ir_id });
        //         for (undecls) |undecl| {
        //             const entry = c.ast.node(undecl.node);
        //             const field_name = c.ast.nodeString(entry.data.keyValue.key);
        //             const field_value = try c.semaExpr(entry.data.keyValue.value, .{});
        //             _ = try c.ir.pushStmt(c.alloc, .set_field_dyn, initializerId, .{ .set_field_dyn = .{
        //                 .name = field_name,
        //                 .rec = temp_expr,
        //                 .right = field_value.irIdx,
        //             }});
        //         }

        //         // return temp.
        //         _ = try c.ir.pushStmt(c.alloc, .exprStmt, initializerId, .{
        //             .expr = temp_expr,
        //             .isBlockResult = true,
        //         });
        //     }
        //     const stmtBlock = try popBlock(c);
        //     c.ir.getExprDataPtr(expr, .blockExpr).bodyHead = stmtBlock.first;
        //     return ExprResult.initStatic(expr, obj.type);
        // } else {
            return ExprResult.initStatic(irIdx, obj.type);
        // }
    }

    pub fn semaObjectInit(c: *cy.Chunk, expr: Expr) !ExprResult {
        const node = expr.node.cast(.record_expr);

        const left = try c.semaExprSkipSym(node.left);
        if (left.resType != .sym) {
            const desc = try c.encoder.allocFmt(c.alloc, node.left);
            defer c.alloc.free(desc);
            return c.reportErrorFmt("Type `{}` does not exist.", &.{v(desc)}, node.left);
        }

        const sym = left.data.sym.resolved();
        switch (sym.type) {
            .struct_t => {
                const obj = sym.cast(.struct_t);
                return c.semaObjectInit2(obj, node.record);
            },
            .object_t => {
                const obj = sym.cast(.object_t);
                return c.semaObjectInit2(obj, node.record);
            },
            .enum_t => {
                return c.reportErrorFmt("Only enum members can be used as initializers.", &.{}, node.left);
            },
            .enumMember => {
                const member = sym.cast(.enumMember);
                const enumSym = member.head.parent.?.cast(.enum_t);
                // Check if enum is choice type.
                if (!enumSym.isChoiceType) {
                    const desc = try c.encoder.allocFmt(c.alloc, node.left);
                    defer c.alloc.free(desc);
                    return c.reportErrorFmt("Can not initialize `{}`. It is not a choice type.", &.{v(desc)}, node.left);
                }

                if (member.payloadType == cy.NullId) {
                    return c.reportErrorFmt("Expected enum member with a payload type.", &.{}, node.left);
                } else {
                    if (!c.sema.isUserObjectType(member.payloadType)) {
                        const payloadTypeName = c.sema.getTypeBaseName(member.payloadType);
                        return c.reportErrorFmt("The payload type `{}` can not be initialized with key value pairs.", &.{v(payloadTypeName)}, node.left);
                    }

                    const obj = c.sema.getTypeSym(member.payloadType).cast(.object_t);
                    const payload = try c.semaObjectInit2(obj, node.record);
                    return semaInitChoice(c, member, payload, expr.node);
                }
            },
            .template => {
                const desc = try c.encoder.allocFmt(c.alloc, node.left);
                defer c.alloc.free(desc);
                return c.reportErrorFmt("Expected a type symbol. `{}` is a type template and must be expanded to a type first.", &.{v(desc)}, node.left);
            },
            .custom_t => {
                // TODO: Implement `$initRecord` instead of hardcoding which custom types are allowed.
                const object_t = sym.cast(.custom_t);
                switch (object_t.type) {
                    bt.Map => {
                        const init = try c.ir.pushExpr(.map, c.alloc, object_t.type, @ptrCast(node.record), .{ .placeholder = undefined });
                        const type_e = c.sema.types.items[object_t.type];
                        if (!type_e.has_init_pair_method) {
                            return error.Unexpected;
                        }
                        return semaWithInitPairs(c, sym, object_t.type, node.record, init);
                    },
                    else => {
                        const desc = try c.encoder.allocFmt(c.alloc, node.left);
                        defer c.alloc.free(desc);
                        return c.reportErrorFmt("Can not initialize `{}`.", &.{v(desc)}, node.left);
                    },
                }
            },
            else => {
                const desc = try c.encoder.allocFmt(c.alloc, node.left);
                defer c.alloc.free(desc);
                return c.reportErrorFmt("Can not initialize `{}`.", &.{v(desc)}, node.left);
            }
        }
    }

    pub fn semaCallFuncSymRec(c: *cy.Chunk, loc: u32, sym: *cy.sym.FuncSym, rec: *ast.Node, rec_res: ExprResult,
        args: []*ast.Node, ret_cstr: ReturnCstr, node: *ast.Node) !ExprResult {

        var matcher = try FuncMatcher.init(c, sym, args.len + 1, ret_cstr);
        defer matcher.deinit(c);

        // Receiver.
        try matcher.matchArg(c, rec, 0, rec_res);

        for (args, 1..) |arg, i| {
            const arg_res = try c.semaExprTarget(arg, matcher.getArgTypeHint(i));
            try matcher.matchArg(c, arg, i, arg_res);
        }

        try matcher.matchEnd(c, node);

        return c.semaCallFuncSymResult(loc, &matcher, node);
    }

    pub fn semaCallFuncSymRecArgs(c: *cy.Chunk, sym: *cy.sym.FuncSym,
        rec_id: *ast.Node, rec: ExprResult,
        args: []const *ast.Node, ret_cstr: ReturnCstr, node: *ast.Node) !ExprResult {
        var matcher = try FuncMatcher.init(c, sym, args.len + 1, ret_cstr);
        defer matcher.deinit(c);

        // Receiver.
        try matcher.matchArg(c, rec_id, 0, rec);

        var i: u32 = 0;
        while (i < args.len) : (i += 1) {
            const arg_id = args[i];
            const arg_res = try c.semaExprTarget(arg_id, matcher.getArgTypeHint(i + 1));
            try matcher.matchArg(c, arg_id, i + 1, arg_res);
        }
        try matcher.matchEnd(c, node);

        const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, node);
        return c.semaCallFuncSymResult(loc, &matcher, node);
    }

    pub fn semaCallFuncSymResult(c: *cy.Chunk, loc: u32, matcher: *FuncMatcher, node: *ast.Node) !ExprResult {
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

    pub fn semaCallFuncSym1(c: *cy.Chunk, loc: u32, sym: *cy.sym.FuncSym, arg1_n: *ast.Node, arg1: ExprResult,
        ret_cstr: ReturnCstr, node: *ast.Node) !ExprResult {

        var matcher = try FuncMatcher.init(c, sym, 1, ret_cstr);
        defer matcher.deinit(c);

        try matcher.matchArg(c, arg1_n, 0, arg1);
        try matcher.matchEnd(c, node);

        return c.semaCallFuncSymResult(loc, &matcher, node);
    }

    pub fn semaCallFuncSym2(c: *cy.Chunk, loc: u32, sym: *cy.sym.FuncSym, arg1_n: *ast.Node, arg1: ExprResult,
        arg2_n: *ast.Node, arg2: ExprResult, ret_cstr: ReturnCstr, node: *ast.Node) !ExprResult {

        var matcher = try FuncMatcher.init(c, sym, 2, ret_cstr);
        defer matcher.deinit(c);

        try matcher.matchArg(c, arg1_n, 0, arg1);
        try matcher.matchArg(c, arg2_n, 1, arg2);
        try matcher.matchEnd(c, node);

        return c.semaCallFuncSymResult(loc, &matcher, node);
    }

    pub fn semaCallFuncSymN(c: *cy.Chunk, sym: *cy.sym.FuncSym, arg_nodes: []const *ast.Node, args: []const ExprResult, ret_cstr: ReturnCstr, node: *ast.Node) !ExprResult {
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
    pub fn semaCallFuncSym(c: *cy.Chunk, loc: u32, sym: *cy.sym.FuncSym, args: []*ast.Node, ret_cstr: ReturnCstr, node: *ast.Node) !ExprResult {
        var matcher = try FuncMatcher.init(c, sym, args.len, ret_cstr);
        defer matcher.deinit(c);

        for (args, 0..) |arg, i| {
            const arg_res = try c.semaExprTarget(arg, matcher.getArgTypeHint(i));
            try matcher.matchArg(c, arg, i, arg_res);
        }

        try matcher.matchEnd(c, node);

        return c.semaCallFuncSymResult(loc, &matcher, node);
    }

    pub fn semaPushDynCallArgs(c: *cy.Chunk, args: []*ast.Node) !u32 {
        const loc = try c.ir.pushEmptyArray(c.alloc, u32, args.len);
        for (args, 0..) |arg, i| {
            const argRes = try c.semaExprTarget(arg, bt.Dyn);
            c.ir.setArrayItem(loc, u32, i, argRes.irIdx);
        }
        return loc;
    }

    /// Skips emitting IR for a sym.
    pub fn semaExprSkipSym(c: *cy.Chunk, node: *ast.Node) !ExprResult {
        switch (node.type()) {
            .ident => {
                return semaIdent(c, node, false);
            },
            .array_expr => {
                const array_expr = node.cast(.array_expr);
                var left = try semaExprSkipSym(c, array_expr.left);
                if (left.resType == .sym) {
                    if (left.data.sym.type == .template) {
                        const final_sym = try cte.expandTemplateOnCallArgs(c, left.data.sym.cast(.template), array_expr.args, node);
                        return sema.symbol(c, final_sym, node, false);
                    }
                    left = try sema.symbol(c, left.data.sym, node, true);
                }
                const expr = Expr.init(node);
                return semaIndexExpr(c, array_expr.left, left, expr);
            },
            .accessExpr => {
                const expr = node.cast(.accessExpr);
                if (expr.left.type() == .ident or expr.left.type() == .accessExpr) {
                    if (expr.right.type() != .ident) return error.Unexpected;

                    // TODO: Check if ident is sym to reduce work.
                    const cstr = Expr.init(node);
                    return c.semaAccessExpr(cstr, false);
                } else {
                    return c.semaExpr(node, .{});
                }
            },
            else => {
                // No chance it's a symbol path.
                return c.semaExpr(node, .{});
            },
        }
    }

    pub fn semaExprTarget(c: *cy.Chunk, node: *ast.Node, target_t: TypeId) !ExprResult {
        return try semaExpr(c, node, .{
            .target_t = target_t,
            .req_target_t = false,
        });
    }

    pub fn semaExprCstr(c: *cy.Chunk, node: *ast.Node, typeId: TypeId) !ExprResult {
        return try semaExpr(c, node, .{
            .target_t = typeId,
            .req_target_t = true,
        });
    }

    pub fn semaOptionExpr(c: *cy.Chunk, node: *ast.Node) !ExprResult {
        const res = try semaExpr(c, node, .{});
        if (res.type.dynamic and res.type.id == bt.Any) {
            // Runtime check.
            var new_res = res;
            new_res.irIdx = try c.ir.pushExpr(.typeCheckOption, c.alloc, bt.Any, node, .{
                .expr = res.irIdx,
            });
            return new_res;
        } else {
            const type_e = c.sema.types.items[res.type.id];
            if (type_e.kind != .option) {
                const name = try c.sema.allocTypeName(res.type.id);
                defer c.alloc.free(name);
                return c.reportErrorFmt("Expected `Option` type, found `{}`.", &.{v(name)}, node);
            }
            return res;
        }
    }

    pub fn semaExpr(c: *cy.Chunk, node: *ast.Node, opts: SemaExprOptions) !ExprResult {
        // Set current node for unexpected errors.
        c.curNode = node;

        const expr = Expr{
            .target_t = opts.target_t,
            .reqTypeCstr = opts.req_target_t,
            .node = node,
        };
        return c.semaExpr2(expr);
    }

    pub fn semaExpr2(c: *cy.Chunk, expr: Expr) !ExprResult {
        const res = try c.semaExprNoCheck(expr);
        if (cy.Trace) {
            const node = expr.node;
            const type_name = c.sema.getTypeBaseName(res.type.id);
            log.tracev("expr.{s}: end {s}", .{@tagName(node.type()), type_name});
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
                    try b.begin(expr.target_t, 2, expr.node);
                    const tag = try c.semaInt(1, expr.node);
                    b.pushArg(tag);
                    b.pushArg(res);
                    const irIdx = b.end();

                    return ExprResult.initStatic(irIdx, expr.target_t);
                }
            } else {
                if (expr.target_t == bt.Any and res.type.id != bt.Any) {
                    // Box value.
                    var newRes = res;
                    newRes.irIdx = try c.ir.pushExpr(.box, c.alloc, bt.Any, expr.node, .{
                        .expr = res.irIdx,
                    });
                    return newRes;
                }
            }

            if (expr.reqTypeCstr) {
                if (!cy.types.isTypeSymCompat(c.compiler, res.type.id, expr.target_t)) {
                    if (res.type.dynamic) {
                        var new_res = res;
                        new_res.irIdx = try c.ir.pushExpr(.type_check, c.alloc, expr.target_t, expr.node, .{
                            .expr = res.irIdx,
                            .exp_type = expr.target_t,
                        });
                        return new_res;
                    }
                    const cstrName = try c.sema.allocTypeName(expr.target_t);
                    defer c.alloc.free(cstrName);
                    const typeName = try c.sema.allocTypeName(res.type.id);
                    defer c.alloc.free(typeName);
                    return c.reportErrorFmt("Expected type `{}`, got `{}`.", &.{v(cstrName), v(typeName)}, expr.node);
                }
            }
        }
        return res;
    }

    pub fn semaExprNoCheck(c: *cy.Chunk, expr: Expr) anyerror!ExprResult {
        if (cy.Trace) {
            const node = expr.node;
            const nodeStr = try c.encoder.format(node, &cy.tempBuf);
            log.tracev("expr.{s}: \"{s}\"", .{@tagName(node.type()), nodeStr});
        }

        const node = expr.node;
        c.curNode = node;
        switch (node.type()) {
            .noneLit => {
                if (expr.hasTargetType()) {
                    return c.semaNone(expr.target_t, node);
                } else {
                    return c.reportErrorFmt("Could not determine optional type for `none`.", &.{}, node);
                }
            },
            .error_lit => {
                const name = c.ast.nodeString(node);
                const loc = try c.ir.pushExpr(.errorv, c.alloc, bt.Error, node, .{ .name = name });
                return ExprResult.initStatic(loc, bt.Error);
            },
            .symbol_lit => {
                const name = c.ast.nodeString(node);
                const irIdx = try c.ir.pushExpr(.symbol, c.alloc, bt.Symbol, node, .{ .name = name });
                return ExprResult.initStatic(irIdx, bt.Symbol);
            },
            .dot_lit => {
                if (!expr.hasTargetType()) {
                    return c.reportErrorFmt("Can not infer dot literal.", &.{}, node);
                }
                const name = c.ast.nodeString(node);
                switch (expr.target_t) {
                    bt.Dyn => {
                        const irIdx = try c.ir.pushExpr(.tag_lit, c.alloc, bt.TagLit, node, .{ .name = name });
                        return ExprResult.initStatic(irIdx, bt.TagLit);
                    },
                    bt.Symbol => {
                        const irIdx = try c.ir.pushExpr(.symbol, c.alloc, bt.Symbol, node, .{ .name = name });
                        return ExprResult.initStatic(irIdx, bt.Symbol);
                    },
                    else => {
                        if (c.sema.isEnumType(expr.target_t)) {
                            const sym = c.sema.getTypeSym(expr.target_t).cast(.enum_t);
                            if (sym.getMemberTag(name)) |tag| {
                                const irIdx = try c.ir.pushExpr(.enumMemberSym, c.alloc, expr.target_t, node, .{
                                    .type = expr.target_t,
                                    .val = @as(u8, @intCast(tag)),
                                });
                                return ExprResult.initStatic(irIdx, expr.target_t);
                            }
                        }
                    }
                }
                return c.reportErrorFmt("Can not infer dot literal.", &.{}, node);
            },
            .trueLit => {
                const irIdx = try c.ir.pushExpr(.truev, c.alloc, bt.Boolean, node, {});
                return ExprResult.initStatic(irIdx, bt.Boolean);
            },
            .falseLit => return c.semaFalse(node),
            .floatLit => {
                const literal = c.ast.nodeString(node);
                const val = try std.fmt.parseFloat(f64, literal);
                const irIdx = try c.ir.pushExpr(.float, c.alloc, bt.Float, node, .{ .val = val });
                return ExprResult.initStatic(irIdx, bt.Float);
            },
            .decLit => {
                if (expr.target_t == bt.Float) {
                    const literal = c.ast.nodeString(node);
                    const val = try std.fmt.parseFloat(f64, literal);
                    return c.semaFloat(val, node);
                } else {
                    const literal = c.ast.nodeString(node);
                    const val = try std.fmt.parseInt(u48, literal, 10);
                    return c.semaInt(val, node);
                }
            },
            .binLit => {
                const literal = c.ast.nodeString(node);
                const val = try std.fmt.parseInt(u48, literal[2..], 2);
                const loc = try c.ir.pushExpr(.int, c.alloc, bt.Integer, node, .{ .val = val });
                return ExprResult.initStatic(loc, bt.Integer);
            },
            .octLit => {
                const literal = c.ast.nodeString(node);
                const val = try std.fmt.parseInt(u48, literal[2..], 8);
                const loc = try c.ir.pushExpr(.int, c.alloc, bt.Integer, node, .{ .val = val });
                return ExprResult.initStatic(loc, bt.Integer);
            },
            .hexLit => {
                const literal = c.ast.nodeString(node);
                const val = try std.fmt.parseInt(u48, literal[2..], 16);
                const loc = try c.ir.pushExpr(.int, c.alloc, bt.Integer, node, .{ .val = val });
                return ExprResult.initStatic(loc, bt.Integer);
            },
            .ident => {
                return try semaIdent(c, node, true);
            },
            .stringLit => return c.semaString(c.ast.nodeString(node), node),
            .raw_string_lit => return c.semaRawString(c.ast.nodeString(node), node),
            .runeLit => {
                const literal = c.ast.nodeString(node);
                if (literal.len == 0) {
                    return c.reportErrorFmt("Invalid UTF-8 Rune.", &.{}, node);
                }
                var val: u48 = undefined;
                if (literal[0] == '\\') {
                    const res = try unescapeSeq(literal[1..]);
                    val = res.char;
                    if (literal.len > res.advance + 1) {
                        return c.reportErrorFmt("Invalid escape sequence.", &.{}, node);
                    }
                } else {
                    const len = std.unicode.utf8ByteSequenceLength(literal[0]) catch {
                        return c.reportErrorFmt("Invalid UTF-8 Rune.", &.{}, node);
                    };
                    if (literal.len != len) {
                        return c.reportErrorFmt("Invalid UTF-8 Rune.", &.{}, node);
                    }
                    val = std.unicode.utf8Decode(literal[0..0+len]) catch {
                        return c.reportErrorFmt("Invalid UTF-8 Rune.", &.{}, node);
                    };
                }
                const loc = try c.ir.pushExpr(.int, c.alloc, bt.Integer, node, .{ .val = val });
                return ExprResult.initStatic(loc, bt.Integer);
            },
            .if_expr => {
                const if_expr = node.cast(.if_expr);

                const cond = try c.semaExprCstr(if_expr.cond, bt.Boolean);
                const body = try c.semaExprCstr(if_expr.body, expr.target_t);
                const else_body = try c.semaExprCstr(if_expr.else_expr, expr.target_t);

                const loc = try c.ir.pushExpr(.if_expr, c.alloc, body.type.id, node, .{
                    .cond = cond.irIdx,
                    .body = body.irIdx,
                    .elseBody = else_body.irIdx,
                });
                const dynamic = body.type.dynamic or else_body.type.dynamic;
                return ExprResult.init(loc, CompactType.init2(bt.Any, dynamic));
            },
            .castExpr => {
                const cast_expr = node.cast(.castExpr);
                const typeId = try resolveTypeSpecNode(c, cast_expr.typeSpec);
                const child = try c.semaExpr(cast_expr.expr, .{});
                const irIdx = try c.ir.pushExpr(.cast, c.alloc, typeId, node, .{
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
                            return c.reportErrorFmt("Cast expects `{}`, got `{}`.", &.{v(expTypeName), v(actTypeName)}, cast_expr.typeSpec);
                        }
                    }
                }
                return ExprResult.init(irIdx, CompactType.init(typeId));
            },
            .callExpr => {
                return c.semaCallExpr(expr);
            },
            .expandOpt => {
                const sym = try cte.expandTemplateOnCallArgs(c, c.sema.option_tmpl, &.{node.cast(.expandOpt).param}, node);
                const ctype = CompactType.initStatic(sym.getStaticType().?);
                return ExprResult.initCustom(cy.NullId, .sym, ctype, .{ .sym = sym });
            },
            .accessExpr => {
                return try c.semaAccessExpr(expr, true);
            },
            .unwrap => {
                const opt = try c.semaOptionExpr(node.cast(.unwrap).opt);
                const payload_t = if (opt.type.id == bt.Any) bt.Any else b: {
                    const type_sym = c.sema.getTypeSym(opt.type.id).cast(.enum_t);
                    const some = type_sym.getMemberByIdx(1);
                    break :b some.payloadType;
                };

                const loc = try c.ir.pushExpr(.unwrapChoice, c.alloc, payload_t, node, .{
                    .choice = opt.irIdx,
                    .tag = 1,
                    .payload_t = payload_t,
                    .fieldIdx = 1,
                });
                return ExprResult.initInheritDyn(loc, opt.type, payload_t);
            },
            .unwrap_or => {
                const unwrap = node.cast(.unwrap_or);
                const opt = try c.semaOptionExpr(unwrap.opt);
                const payload_t = if (opt.type.id == bt.Any) bt.Any else b: {
                    const type_sym = c.sema.getTypeSym(opt.type.id).cast(.enum_t);
                    const some = type_sym.getMemberByIdx(1);
                    break :b some.payloadType;
                };

                const default = try c.semaExprCstr(unwrap.default, payload_t);
                const loc = try c.ir.pushExpr(.unwrap_or, c.alloc, payload_t, node, .{
                    .opt = opt.irIdx,
                    .default = default.irIdx,
                });
                return ExprResult.initInheritDyn(loc, opt.type, payload_t);
            },
            .array_init => {
                const array_init = node.cast(.array_init);
                var left = try c.semaExprSkipSym(array_init.left);
                if (left.resType == .sym) {
                    if (left.data.sym.getStaticType()) |type_id| {
                        if (left.data.sym.getVariant()) |variant| {
                            if (variant.template == c.sema.list_tmpl) {
                                const nargs = array_init.args.len;
                                const loc = try c.ir.pushEmptyExpr(.list, c.alloc, ir.ExprType.init(type_id), node);
                                const args_loc = try c.ir.pushEmptyArray(c.alloc, u32, nargs);

                                const elem_t = variant.args[0].asHeapObject().type.type;

                                for (array_init.args, 0..) |arg, i| {
                                    const res = try c.semaExprTarget(arg, elem_t);
                                    c.ir.setArrayItem(args_loc, u32, i, res.irIdx);
                                }

                                c.ir.setExprData(loc, .list, .{ .type = type_id, .numArgs = @intCast(nargs) });
                                return ExprResult.initStatic(loc, type_id);
                            }
                        }
                    }
                }
                return c.reportError("Unsupported array initializer", expr.node);
            },
            .array_expr => {
                const array_expr = node.cast(.array_expr);
                var left = try c.semaExprSkipSym(array_expr.left);
                if (left.resType == .sym) {
                    if (left.data.sym.type == .template) {
                        const final_sym = try cte.expandTemplateOnCallArgs(c, left.data.sym.cast(.template), array_expr.args, node);
                        return sema.symbol(c, final_sym, expr.node, true);
                    } else {
                        left = try sema.symbol(c, left.data.sym, expr.node, true);
                    }
                }
                return semaIndexExpr(c, array_expr.left, left, expr);
            },
            .range => {
                const range = node.cast(.range);
                var start: u32 = cy.NullId;
                if (range.start != null) {
                    const start_res = try c.semaExprCstr(range.start.?, bt.Integer);
                    start = start_res.irIdx;
                }
                var end: u32 = cy.NullId;
                if (range.end != null) {
                    const end_res = try c.semaExprCstr(range.end.?, bt.Integer);
                    end = end_res.irIdx;
                }
                const loc = try c.ir.pushExpr(.range, c.alloc, bt.Range, node, .{
                    .start = start,
                    .end = end,
                    .inc = range.inc,
                });
                return ExprResult.initStatic(loc, bt.Range);
            },
            .binExpr => {
                const bin_expr = node.cast(.binExpr);
                return try c.semaBinExpr(expr, bin_expr.left, bin_expr.op, bin_expr.right);
            },
            .unary_expr => {
                return try c.semaUnExpr(expr);
            },
            .arrayLit => {
                const array_lit = node.cast(.arrayLit);
                const irIdx = try c.ir.pushEmptyExpr(.list, c.alloc, ir.ExprType.init(bt.ListDyn), node);
                const irArgsIdx = try c.ir.pushEmptyArray(c.alloc, u32, array_lit.args.len);

                for (array_lit.args, 0..) |arg, i| {
                    const argRes = try c.semaExpr(arg, .{});
                    c.ir.setArrayItem(irArgsIdx, u32, i, argRes.irIdx);
                }

                c.ir.setExprData(irIdx, .list, .{ .type = bt.ListDyn, .numArgs = @intCast(array_lit.args.len) });
                return ExprResult.initStatic(irIdx, bt.ListDyn);
            },
            .recordLit => {
                const record_lit = node.cast(.recordLit);

                if (expr.hasTargetType()) {
                    if (c.sema.isUserObjectType(expr.target_t)) {
                        // Infer user object type.
                        const obj = c.sema.getTypeSym(expr.target_t).cast(.object_t);
                        return c.semaObjectInit2(obj, record_lit);
                    } else if (c.sema.isStructType(expr.target_t)) {
                        const obj = c.sema.getTypeSym(expr.target_t).cast(.struct_t);
                        return c.semaObjectInit2(obj, record_lit);
                    }
                }

                const obj_t = c.sema.getTypeSym(bt.Table).cast(.object_t);
                return c.semaObjectInit2(obj_t, record_lit);
            },
            .stringTemplate => {
                const template = node.cast(.stringTemplate);
                const numExprs = template.parts.len / 2;
                const irIdx = try c.ir.pushEmptyExpr(.stringTemplate, c.alloc, ir.ExprType.init(bt.String), node);
                const irStrsIdx = try c.ir.pushEmptyArray(c.alloc, []const u8, numExprs+1);
                const irArgsIdx = try c.ir.pushEmptyArray(c.alloc, u32, numExprs);

                for (0..numExprs+1) |i| {
                    const str = template.parts[i*2];
                    c.ir.setArrayItem(irStrsIdx, []const u8, i, c.ast.nodeString(str));
                }

                for (0..numExprs) |i| {
                    const expr_ = template.parts[1 + i*2];
                    const argRes = try c.semaExpr(expr_, .{});
                    c.ir.setArrayItem(irArgsIdx, u32, i, argRes.irIdx);
                }

                c.ir.setExprData(irIdx, .stringTemplate, .{ .numExprs = @intCast(numExprs), .args = irArgsIdx });
                return ExprResult.initStatic(irIdx, bt.String);
            },
            .group => {
                return c.semaExpr(node.cast(.group).child, .{});
            },
            .lambda_expr => {
                const lambda = node.cast(.lambda_expr);
                const func_sig = try resolveLambdaFuncSig(c, lambda);
                const func = try c.addUserLambda(@ptrCast(c.sym), func_sig, lambda);
                _ = try pushLambdaProc(c, func);
                const irIdx = c.proc().irStart;

                // Generate function body.
                try appendFuncParamVars(c, func, lambda.params);

                const exprRes = try c.semaExpr(@ptrCast(@constCast(@alignCast(lambda.stmts.ptr))), .{});
                _ = try c.ir.pushStmt(c.alloc, .retExprStmt, node, .{
                    .expr = exprRes.irIdx,
                });

                try popLambdaProc(c);

                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .lambda_multi => {
                const lambda = node.cast(.lambda_multi);
                const func_sig = try resolveLambdaFuncSig(c, lambda);
                const func = try c.addUserLambda(@ptrCast(c.sym), func_sig, lambda);
                _ = try pushLambdaProc(c, func);
                const irIdx = c.proc().irStart;

                // Generate function body.
                try appendFuncParamVars(c, func, lambda.params);
                try semaStmts(c, lambda.stmts);

                try popLambdaProc(c);

                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .record_expr => {
                return c.semaObjectInit(expr);
            },
            .throwExpr => {
                const child = try c.semaExpr(node.cast(.throwExpr).child, .{});
                const irIdx = try c.ir.pushExpr(.throw, c.alloc, bt.Any, node, .{ .expr = child.irIdx });
                return ExprResult.initDynamic(irIdx, bt.Any);
            },
            .tryExpr => {
                const try_expr = node.cast(.tryExpr);
                var catchError = false;
                if (try_expr.catchExpr) |catch_expr| {
                    if (catch_expr.type() == .ident) {
                        const name = c.ast.nodeString(catch_expr);
                        if (std.mem.eql(u8, "error", name)) {
                            catchError = true;
                        }
                    }
                } else {
                    catchError = true;
                }
                const irIdx = try c.ir.pushEmptyExpr(.tryExpr, c.alloc, ir.ExprType.init(bt.Any), node);

                if (catchError) {
                    const child = try c.semaExpr(try_expr.expr, .{});
                    c.ir.setExprData(irIdx, .tryExpr, .{ .expr = child.irIdx, .catchBody = cy.NullId });
                    const unionT = cy.types.unionOf(c.compiler, child.type.id, bt.Error);
                    return ExprResult.init(irIdx, CompactType.init2(unionT, child.type.dynamic));
                } else {
                    const child = try c.semaExpr(try_expr.expr, .{});
                    const catchExpr = try c.semaExpr(try_expr.catchExpr.?, .{});
                    c.ir.setExprData(irIdx, .tryExpr, .{ .expr = child.irIdx, .catchBody = catchExpr.irIdx });
                    const dynamic = catchExpr.type.dynamic or child.type.dynamic;
                    const unionT = cy.types.unionOf(c.compiler, child.type.id, catchExpr.type.id);
                    return ExprResult.init(irIdx, CompactType.init2(unionT, dynamic));
                }
            },
            .comptimeExpr => {
                const child = node.cast(.comptimeExpr).child;
                if (child.type() == .ident) {
                    const name = c.ast.nodeString(child);
                    if (std.mem.eql(u8, name, "modUri")) {
                        return c.semaRawString(c.srcUri, node);
                    } else {
                        return c.reportErrorFmt("Compile-time symbol does not exist: {}", &.{v(name)}, child);
                    }
                } else {
                    return c.reportErrorFmt("Unsupported compile-time expr: {}", &.{v(child.type())}, child);
                }
            },
            .coinit => {
                const coinit = node.cast(.coinit);
                const irIdx = try c.ir.pushExpr(.coinitCall, c.alloc, bt.Fiber, node, {});

                const irCallIdx = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, node);
                const callExpr = coinit.child;

                const callee = try c.semaExprSkipSym(callExpr.callee);

                // Callee is already pushed as a value or is a symbol.
                if (callee.resType == .sym) {
                    const sym = callee.data.sym;
                    _ = try callSym(c, irCallIdx, sym, 
                        callExpr.callee, callExpr.args, expr.getRetCstr());
                } else {
                    // preCall.
                    const args = try c.semaPushDynCallArgs(callExpr.args);
                    _ = try c.semaCallValue(irCallIdx, callee.irIdx, callExpr.args.len, args);
                }

                return ExprResult.initStatic(irIdx, bt.Fiber);
            },
            .coyield => {
                const irIdx = try c.ir.pushExpr(.coyield, c.alloc, bt.Any, node, {});
                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .coresume => {
                const child = try c.semaExpr(node.cast(.coresume).child, .{});
                const irIdx = try c.ir.pushExpr(.coresume, c.alloc, bt.Any, node, .{
                    .expr = child.irIdx,
                });
                return ExprResult.initStatic(irIdx, bt.Any);
            },
            .switchExpr => {
                return semaSwitchExpr(c, node.cast(.switchExpr));
            },
            else => {
                return c.reportErrorFmt("Unsupported node: {}", &.{v(node.type())}, node);
            },
        }
    }

    pub fn semaCallExpr(c: *cy.Chunk, expr: Expr) !ExprResult {
        const node = expr.node.cast(.callExpr);

        if (node.hasNamedArg) {
            return c.reportErrorFmt("Unsupported named args.", &.{}, expr.node);
        }

        // pre is later patched with the type of call.
        const preIdx = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, expr.node);

        if (node.callee.type() == .accessExpr) {
            const callee = node.callee.cast(.accessExpr);
            const leftRes = try c.semaExprSkipSym(callee.left);
            if (callee.right.type() != .ident) {
                return error.Unexpected;
            }

            if (leftRes.resType == .sym) {
                const leftSym = leftRes.data.sym;

                if (leftSym.type == .template) {
                    return c.reportErrorFmt("Can not access template symbol `{}`.", &.{
                        v(c.ast.nodeString(callee.left))}, callee.right);
                }
                if (leftSym.type == .func) {
                    return c.reportErrorFmt("Can not access function symbol `{}`.", &.{
                        v(c.ast.nodeString(callee.left))}, callee.right);
                }
                const rightName = c.ast.nodeString(callee.right);

                if (leftRes.type.isDynAny()) {
                    // Runtime method call.
                    const recv = try sema.symbol(c, leftSym, callee.left, true);
                    const args = try c.semaPushDynCallArgs(node.args);
                    return c.semaCallObjSym(preIdx, recv.irIdx, callee.right, node.args.len, args);
                }

                if (leftSym.isVariable()) {
                    // Look for sym under left type's module.
                    const leftTypeSym = c.sema.getTypeSym(leftRes.type.id);
                    const rightSym = try c.mustFindSym(leftTypeSym, rightName, callee.right);
                    const func_sym = try requireFuncSym(c, rightSym, callee.right);
                    const recv = try sema.symbol(c, leftSym, callee.left, true);
                    return c.semaCallFuncSymRec(preIdx, func_sym, callee.left, recv,
                        node.args, expr.getRetCstr(), expr.node);
                } else {
                    // Look for sym under left module.
                    const rightSym = try c.mustFindSym(leftSym, rightName, callee.right);
                    return try callSym(c, preIdx, rightSym, callee.right, node.args, expr.getRetCstr());
                }
            } else {
                if (leftRes.type.isDynAny()) {
                    // preCallObjSym.
                    const args = try c.semaPushDynCallArgs(node.args);
                    return c.semaCallObjSym(preIdx, leftRes.irIdx, callee.right, node.args.len, args);
                } else {
                    // Look for sym under left type's module.
                    const rightName = c.ast.nodeString(callee.right);
                    const leftTypeSym = c.sema.getTypeSym(leftRes.type.id);
                    const rightSym = try c.mustFindSym(leftTypeSym, rightName, callee.right);
                    const func_sym = try requireFuncSym(c, rightSym, callee.right);

                    return c.semaCallFuncSymRec(preIdx, func_sym, callee.left, leftRes,
                        node.args, expr.getRetCstr(), expr.node);
                }
            }
        } else if (node.callee.type() == .ident) {
            const name = c.ast.nodeString(node.callee);

            const varRes = try getOrLookupVar(c, name, node.callee);
            switch (varRes) {
                .global,
                .local => {
                    // preCall.
                    const calleeRes = try c.semaExpr(node.callee, .{});
                    const args = try c.semaPushDynCallArgs(node.args);
                    return c.semaCallValue(preIdx, calleeRes.irIdx, node.args.len, args);
                },
                .static => |sym| {
                    return callSym(c, preIdx, sym, node.callee, node.args, expr.getRetCstr());
                },
            }
        } else {
            // preCall.
            const calleeRes = try c.semaExprSkipSym(node.callee);
            if (calleeRes.resType == .sym) {
                return callSym(c, preIdx, calleeRes.data.sym, node.callee, node.args, expr.getRetCstr());
            } else {
                const args = try c.semaPushDynCallArgs(node.args);
                return c.semaCallValue(preIdx, calleeRes.irIdx, node.args.len, args);
            }
        }
    }

    /// Expr or construct binExpr from opAssignStmt.
    pub fn semaExprOrOpAssignBinExpr(c: *cy.Chunk, expr: Expr, opAssignBinExpr: bool) !ExprResult {
        if (!opAssignBinExpr) {
            return try c.semaExpr2(expr);
        } else {
            const stmt = expr.node.cast(.opAssignStmt);
            return try c.semaBinExpr(expr, stmt.left, stmt.op, stmt.right);
        }
    }

    pub fn semaString(c: *cy.Chunk, lit: []const u8, node: *ast.Node) !ExprResult {
        const raw = try c.unescapeString(lit);
        if (raw.ptr != lit.ptr) {
            // Dupe and track in ast.strs.
            const dupe = try c.alloc.dupe(u8, raw);
            try c.parser.ast.strs.append(c.alloc, dupe);
            return c.semaRawString(dupe, node);
        } else {
            return c.semaRawString(raw, node);
        }
    }

    pub fn semaRawString(c: *cy.Chunk, raw: []const u8, node: *ast.Node) !ExprResult {
        const irIdx = try c.ir.pushExpr(.string, c.alloc, bt.String, node, .{ .raw = raw });
        return ExprResult.initStatic(irIdx, bt.String);
    }

    pub fn semaArray(c: *cy.Chunk, arr: []const u8, node: *ast.Node) !ExprResult {
        const irIdx = try c.ir.pushExpr(.array, c.alloc, bt.Array, node, .{ .buffer = arr });
        return ExprResult.initStatic(irIdx, bt.Array);
    }

    pub fn semaFloat(c: *cy.Chunk, val: f64, node: *ast.Node) !ExprResult {
        const irIdx = try c.ir.pushExpr(.float, c.alloc, bt.Float, node, .{ .val = val });
        return ExprResult.initStatic(irIdx, bt.Float);
    }

    pub fn semaInt(c: *cy.Chunk, val: u48, node: *ast.Node) !ExprResult {
        const irIdx = try c.ir.pushExpr(.int, c.alloc, bt.Integer, node, .{ .val = val });
        return ExprResult.initStatic(irIdx, bt.Integer);
    }

    pub fn semaFalse(c: *cy.Chunk, node: *ast.Node) !ExprResult {
        const irIdx = try c.ir.pushExpr(.falsev, c.alloc, bt.Boolean, node, {});
        return ExprResult.initStatic(irIdx, bt.Boolean);
    }

    pub fn semaNone(c: *cy.Chunk, preferType: cy.TypeId, node: *ast.Node) !ExprResult {
        const type_e = c.sema.types.items[preferType];
        if (type_e.kind == .option) {
            // Generate IR to wrap value into optional.
            var b: ObjectBuilder = .{ .c = c };
            try b.begin(preferType, 2, node);
            const tag = try c.semaInt(0, node);
            b.pushArg(tag);
            const payload = try c.semaInt(0, node);
            b.pushArg(payload);
            const loc = b.end();

            return ExprResult.initStatic(loc, preferType);
        } else {
            const name = type_e.sym.name();
            return c.reportErrorFmt("Expected `Option(T)` to infer `none` value, found `{}`.", &.{v(name)}, node);
        }
    }

    pub fn semaEmptyList(c: *cy.Chunk, node: *ast.Node) !ExprResult {
        const irIdx = try c.ir.pushExpr(.list, c.alloc, bt.ListDyn, node, .{ .type = bt.ListDyn, .numArgs = 0 });
        return ExprResult.initStatic(irIdx, bt.ListDyn);
    }

    pub fn semaMap(c: *cy.Chunk, node: *ast.Node) !ExprResult {
        const irIdx = try c.ir.pushExpr(.map, c.alloc, bt.Map, node, .{ .placeholder = undefined });
        return ExprResult.initStatic(irIdx, bt.Map);
    }

    pub fn semaCallValue(c: *cy.Chunk, preIdx: u32, calleeLoc: u32, numArgs: usize, argsLoc: u32) !ExprResult {
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

    pub fn semaCallObjSym(c: *cy.Chunk, preIdx: u32, recLoc: u32, right: *ast.Node, num_args: usize, irArgsIdx: u32) !ExprResult {
        // Dynamic method call.
        const name = c.ast.nodeString(right);

        c.ir.setExprCode(preIdx, .preCallObjSym);
        c.ir.setExprData(preIdx, .preCallObjSym, .{ .callObjSym = .{
            .name = name,
            .numArgs = @intCast(num_args),
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
        const node = expr.node.cast(.unary_expr);

        switch (node.op) {
            .minus => {
                const irIdx = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, expr.node);

                const child = try c.semaExprTarget(node.child, expr.target_t);
                if (child.type.isDynAny()) {
                    return c.semaCallObjSym2(irIdx, child.irIdx, getUnOpName(.minus), &.{});
                }

                if (child.type.id == bt.Integer or child.type.id == bt.Float) {
                    // Specialized.
                    c.ir.setExprCode(irIdx, .preUnOp);
                    c.ir.setExprData(irIdx, .preUnOp, .{ .unOp = .{
                        .childT = child.type.id, .op = node.op, .expr = child.irIdx,
                    }});
                    return ExprResult.initStatic(irIdx, child.type.id);
                } else {
                    // Look for sym under child type's module.
                    const childTypeSym = c.sema.getTypeSym(child.type.id);
                    const sym = try c.mustFindSym(childTypeSym, node.op.name(), expr.node);
                    const func_sym = try requireFuncSym(c, sym, expr.node);
                    return c.semaCallFuncSym1(irIdx, func_sym, node.child, child, expr.getRetCstr(), expr.node);
                }
            },
            .not => {
                const irIdx = try c.ir.pushEmptyExpr(.preUnOp, c.alloc, undefined, expr.node);

                const child = try c.semaExprCstr(node.child, bt.Boolean);
                c.ir.setExprData(irIdx, .preUnOp, .{ .unOp = .{
                    .childT = child.type.id, .op = node.op, .expr = child.irIdx,
                }});
                return ExprResult.initStatic(irIdx, bt.Boolean);
            },
            .bitwiseNot => {
                const irIdx = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, expr.node);

                const child = try c.semaExprTarget(node.child, expr.target_t);
                if (child.type.isDynAny()) {
                    return c.semaCallObjSym2(irIdx, child.irIdx, getUnOpName(.bitwiseNot), &.{});
                }

                if (child.type.id == bt.Integer) {
                    c.ir.setExprCode(irIdx, .preUnOp);
                    c.ir.setExprData(irIdx, .preUnOp, .{ .unOp = .{
                        .childT = child.type.id, .op = node.op, .expr = child.irIdx,
                    }});
                    return ExprResult.initStatic(irIdx, bt.Integer);
                } else {
                    // Look for sym under child type's module.
                    const childTypeSym = c.sema.getTypeSym(child.type.id);
                    const sym = try c.mustFindSym(childTypeSym, node.op.name(), expr.node);
                    const func_sym = try requireFuncSym(c, sym, expr.node);
                    return c.semaCallFuncSym1(irIdx, func_sym, node.child, child, expr.getRetCstr(), expr.node);
                }
            },
            else => return c.reportErrorFmt("Unsupported unary op: {}", &.{v(node.op)}, expr.node),
        }
    }

    pub fn semaBinExpr(c: *cy.Chunk, expr: Expr, leftId: *ast.Node, op: cy.BinaryExprOp, rightId: *ast.Node) !ExprResult {
        const node = expr.node;

        switch (op) {
            .and_op,
            .or_op => {
                const loc = try c.ir.pushEmptyExpr(.preBinOp, c.alloc, ir.ExprType.init(bt.Boolean), node);
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
                const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, node);

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
                    const sym = try c.mustFindSym(leftTypeSym, op.name(), node);
                    const funcSym = try requireFuncSym(c, sym, node);
                    return c.semaCallFuncSym2(loc, funcSym, leftId, left, rightId, right, expr.getRetCstr(), node);
                }
            },
            .greater,
            .greater_equal,
            .less,
            .less_equal => {
                const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, node);

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
                    const sym = try c.mustFindSym(leftTypeSym, op.name(), node);
                    const funcSym = try requireFuncSym(c, sym, node);
                    return c.semaCallFuncSym2(loc, funcSym, leftId, left, rightId, right, expr.getRetCstr(), node);
                }
            },
            .star,
            .slash,
            .percent,
            .caret,
            .plus,
            .minus => {
                const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, node);

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
                    const sym = try c.mustFindSym(leftTypeSym, op.name(), node);
                    const func_sym = try requireFuncSym(c, sym, node);
                    return c.semaCallFuncSym2(loc, func_sym, leftId, left, rightId, right, expr.getRetCstr(), node);
                }
            },
            .bang_equal,
            .equal_equal => {
                const leftPreferT = if (expr.target_t == bt.Float or expr.target_t == bt.Integer) expr.target_t else bt.Any;

                const loc = try c.ir.pushEmptyExpr(.pre, c.alloc, undefined, node);

                const left = try c.semaExprTarget(leftId, leftPreferT);
                const right = try c.semaExprTarget(rightId, left.type.id);

                if (left.type.id == right.type.id) {
                    const left_te = c.sema.types.items[left.type.id];
                    if (left_te.kind == .option or left_te.kind == .@"struct") {
                        return semaStructCompare(c, left, leftId, op, right, rightId, left_te, node);
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
            else => return c.reportErrorFmt("Unsupported binary op: {}", &.{v(op)}, node),
        }
    }

    pub fn semaAccessExpr(c: *cy.Chunk, expr: Expr, symAsValue: bool) !ExprResult {
        const node = expr.node.cast(.accessExpr);

        if (node.right.type() != .ident) {
            return error.Unexpected;
        }

        const rec = try c.semaExprSkipSym(node.left);
        if (rec.resType == .sym) {
            const sym = rec.data.sym;
            const rightName = c.ast.nodeString(node.right);
            const rightSym = try c.getResolvedDistinctSym(sym, rightName, node.right, true);
            try referenceSym(c, rightSym, node.right);

            if (!symAsValue) {
                var typeId: CompactType = undefined;
                if (rightSym.isType()) {
                    typeId = CompactType.init(rightSym.getStaticType().?);
                } else {
                    typeId = CompactType.init((try rightSym.getValueType()) orelse bt.Void);
                }
                return ExprResult.initCustom(cy.NullId, .sym, typeId, .{ .sym = rightSym });
            } else {
                return try sema.symbol(c, rightSym, node.right, true);
            }
        } else {
            return semaAccessField(c, rec, node.right);
        }
    }
};

fn semaWithInitPairs(c: *cy.Chunk, type_sym: *cy.Sym, type_id: cy.TypeId, record: *ast.RecordLit, init: u32) !ExprResult {
    const node: *ast.Node = @ptrCast(record);
    if (record.args.len == 0) {
        // Just return default initializer for no record pairs.
        return ExprResult.init(init, CompactType.initStatic(type_id));
    }
    const init_pair = type_sym.getMod().?.getSym("$initPair").?;

    const expr = try c.ir.pushExpr(.blockExpr, c.alloc, type_id, node, .{ .bodyHead = cy.NullId });
    try pushBlock(c, node);
    {
        // create temp with object.
        const var_id = try declareLocalName(c, "$temp", type_id, true, node);
        const temp_ir_id = c.varStack.items[var_id].inner.local.id;
        const temp_ir = c.varStack.items[var_id].inner.local.declIrStart;
        const decl_stmt = c.ir.getStmtDataPtr(temp_ir, .declareLocalInit);
        decl_stmt.init = init;
        decl_stmt.initType = CompactType.initStatic(type_id);

        // call $initPair for each record pair.
        const temp_loc = try c.ir.pushExpr(.local, c.alloc, type_id, node, .{ .id = temp_ir_id });
        const temp_expr = ExprResult.init(temp_loc, CompactType.initStatic(type_id));

        for (record.args) |arg| {
            const key_name = c.ast.nodeString(arg.key);
            const key_expr = try c.semaString(key_name, arg.key);
            const val_expr = try c.semaExpr(arg.value, .{});

            const call = try c.semaCallFuncSymN(init_pair.cast(.func),
                &.{node, arg.key, arg.value},
                &.{temp_expr, key_expr, val_expr},
                .any, @ptrCast(arg));

            _ = try c.ir.pushStmt(c.alloc, .exprStmt, @ptrCast(arg), .{
                .expr = call.irIdx,
                .isBlockResult = false,
            });
        }

        // return temp.
        _ = try c.ir.pushStmt(c.alloc, .exprStmt, node, .{
            .expr = temp_loc,
            .isBlockResult = true,
        });
    }
    const stmtBlock = try popBlock(c);
    c.ir.getExprDataPtr(expr, .blockExpr).bodyHead = stmtBlock.first;
    return ExprResult.initStatic(expr, type_id);
}

fn semaInitChoice(c: *cy.Chunk, member: *cy.sym.EnumMember, payload: ExprResult, node: *ast.Node) !ExprResult {
    var b: ObjectBuilder = .{ .c = c };
    try b.begin(member.type, 2, node);
    const tag = try c.semaInt(member.val, node);
    b.pushArg(tag);
    b.pushArg(payload);
    const irIdx = b.end();
    return ExprResult.initStatic(irIdx, member.type);
}

fn semaInitChoiceNoPayload(c: *cy.Chunk, member: *cy.sym.EnumMember, node: *ast.Node) !ExprResult {
    // No payload type.
    var b: ObjectBuilder = .{ .c = c };
    try b.begin(member.type, 2, node);
    const tag = try c.semaInt(member.val, node);
    b.pushArg(tag);
    const payload = try c.semaZeroInit(bt.Any, node);
    b.pushArg(payload);
    const irIdx = b.end();
    return ExprResult.initStatic(irIdx, member.type);
}

fn semaStructCompare(c: *cy.Chunk, left: ExprResult, left_id: *ast.Node, op: cy.BinaryExprOp,
    right: ExprResult, right_id: *ast.Node, left_te: cy.types.Type, node_id: *ast.Node) !ExprResult {

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

fn assignToLocalVar(c: *cy.Chunk, localRes: ExprResult, rhs: *ast.Node, opts: AssignOptions) !ExprResult {
    const id = localRes.data.local;
    var svar = &c.varStack.items[id];

    const rightExpr = Expr{ .target_t = svar.vtype.id, .node = rhs, .reqTypeCstr = !svar.vtype.dynamic };
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

pub const Sema = struct {
    alloc: std.mem.Allocator,
    compiler: *cy.Compiler,

    types: std.ArrayListUnmanaged(cy.types.Type),

    /// Resolved signatures for functions.
    funcSigs: std.ArrayListUnmanaged(FuncSig),
    funcSigMap: std.HashMapUnmanaged(FuncSigKey, FuncSigId, FuncSigKeyContext, 80),

    option_tmpl: *cy.sym.Template,
    list_tmpl: *cy.sym.Template,
    table_type: *cy.sym.ObjectType,

    pub fn init(alloc: std.mem.Allocator, compiler: *cy.Compiler) Sema {
        return .{
            .alloc = alloc,
            .compiler = compiler,
            .option_tmpl = undefined,
            .list_tmpl = undefined,
            .table_type = undefined,
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
        @memset(buf[0..numParams], bt.Dyn);
        return try s.ensureFuncSig(buf[0..numParams], bt.Dyn);
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
                if (symId != bt.Dyn and symId != bt.Any) {
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

    pub fn formatFuncSig(s: *Sema, funcSigId: FuncSigId, buf: []u8, from: ?*cy.Chunk) ![]const u8 {
        var fbuf = std.io.fixedBufferStream(buf);
        try s.writeFuncSigStr(fbuf.writer(), funcSigId, from);
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

    pub fn allocTypesStr(s: *Sema, types: []const TypeId, from: ?*cy.Chunk) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        try w.writeAll("(");
        try writeFuncParams(s, w, types, from);
        try w.writeAll(")");
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn allocFuncSigTypesStr(s: *Sema, params: []const TypeId, ret: TypeId, from: ?*cy.Chunk) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        try writeFuncSigTypesStr(s, w, params, ret, from);
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn allocFuncParamsStr(s: *Sema, params: []const TypeId, from: ?*cy.Chunk) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        try writeFuncParams(s, w, params, from);
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn allocFuncSigStr(s: *Sema, funcSigId: FuncSigId, show_ret: bool, from: ?*cy.Chunk) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        const funcSig = s.funcSigs.items[funcSigId];
        try w.writeAll("(");
        try writeFuncParams(s, w, funcSig.params(), from);
        try w.writeAll(")");
        if (show_ret) {
            try w.writeAll(" ");
            try s.writeTypeName(w, funcSig.ret, from);
        }
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn writeFuncSigStr(s: *Sema, w: anytype, funcSigId: FuncSigId, from: ?*cy.Chunk) !void {
        const funcSig = s.funcSigs.items[funcSigId];
        try writeFuncSigTypesStr(s, w, funcSig.params(), funcSig.ret, from);
    }

    pub fn writeFuncSigTypesStr(s: *Sema, w: anytype, params: []const TypeId, ret: TypeId, from: ?*cy.Chunk) !void {
        try w.writeAll("(");
        try writeFuncParams(s, w, params, from);
        try w.writeAll(") ");
        try s.writeTypeName(w, ret, from);
    }

    pub fn writeFuncParams(s: *Sema, w: anytype, params: []const TypeId, from: ?*cy.Chunk) !void {
        if (params.len > 0) {
            try s.writeTypeName(w, params[0], from);

            if (params.len > 1) {
                for (params[1..]) |paramT| {
                    try w.writeAll(", ");
                    try s.writeTypeName(w, paramT, from);
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
            try t.eq(@sizeOf(LocalVar), 36);
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
}

pub const ObjectBuilder = struct {
    irIdx: u32 = undefined,
    irArgsIdx: u32 = undefined,

    /// Generic typeId so choice types can also be instantiated.
    typeId: cy.TypeId = undefined,

    c: *cy.Chunk,
    argIdx: u32 = undefined,

    pub fn begin(b: *ObjectBuilder, typeId: cy.TypeId, numFields: u8, node: *ast.Node) !void {
        b.typeId = typeId;
        b.irArgsIdx = try b.c.ir.pushEmptyArray(b.c.alloc, u32, numFields);
        b.irIdx = try b.c.ir.pushExpr(.object_init, b.c.alloc, typeId, node, .{
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

    fn init(c: *cy.Chunk, func_sym: *cy.sym.FuncSym, nargs: usize, ret_cstr: ReturnCstr) !FuncMatcher {
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
            .nargs = @intCast(nargs),
            .has_dyn_arg = false,
            .dyn_call = false,
        };
    }

    fn deinit(self: *FuncMatcher, c: *cy.Chunk) void {
        c.typeStack.items.len = self.type_start;
    }

    fn getArgTypeHint(self: *FuncMatcher, arg_idx: usize) cy.TypeId {
        if (arg_idx < self.func_params.len) {
            return self.func_params[arg_idx];
        } else {
            return bt.Any;
        }
    }

    fn matchEnd(self: *FuncMatcher, c: *cy.Chunk, node: *ast.Node) !void {
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

    fn reportIncompatibleArg(self: *FuncMatcher, c: *cy.Chunk, arg: *ast.Node, arg_res: ExprResult) anyerror {
        try c.typeStack.append(c.alloc, arg_res.type.id);
        const types = c.typeStack.items[self.type_start..];
        return reportIncompatibleCallSig(c, self.sym, types, self.ret_cstr, arg);
    }

    fn matchArg(self: *FuncMatcher, c: *cy.Chunk, arg: *ast.Node, arg_idx: usize, arg_res: ExprResult) !void {
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