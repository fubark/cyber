const std = @import("std");
const cy = @import("cyber.zig");
const TypeId = cy.TypeId;
const sema = cy.sema;
const log = cy.log.scoped(.ir);
const ast = cy.ast;

/// An IR is useful because the AST isn't suited to represent the result of sema.
/// The goal for the IR is to handle most of the heavy lifting such as type checking, lifetimes, arc, and  destructors.
/// This should allow codegen to perform a simple translation from the IR.
/// The IR is generated during the sema pass and thrown away after it is consumed by codegen.
///
/// A couple of reasons for an IR:
/// 1. Since there are static types and symbols, some statements and expressions become irrelevant.
/// 2. Backpatching for param copies and capturing variables.
/// 3. Additional IR can be added such as extracting managed temps into locals, perform value copying,
///    ARC retain/release, static variable initializers, and zero values.
/// 4. Should make bytecode and C backends simpler. Creating additional backends should also be simpler.
///
/// The IR is a tree structure but all the nodes are packed into a linear array.
/// IR nodes are indexed by their position in the array.
/// Stmt and expr nodes contain metadata which starts with their node type followed by a pointer to their AST node.
/// Stmt nodes additionally have a next index that points to the next stmt node.
/// Expr nodes additionally have a type id that also contains a throws bit.
/// Data specific to a node's type follows the metadata.

pub const StmtCode = enum(u8) {
    /// Main block. Only one chunk has IR for the main block.
    mainBlock,

    /// A [numParams]u8 array follows.
    /// Each elem is a boolean indicating whether the respective param
    /// will be written to at some point in the function.
    funcBlock,

    declare_local,
    discard,

    // For recovering local lifetimes and break control flow.
    block,

    // For recovering local lifetimes.
    pop_locals,

    if_block,
    switch_stmt,
    tryStmt,
    loop_block,
    forRangeStmt,
    yield,
    await_,
    ret,
    ret_expr,
    ret_gen,
    break_,
    continue_,
    trap,

    set,
    set_local,
    set_ret,
    setCaptured,
    set_field,
    set_global,
    set_deref,

    release,

    /// Nop with a label.
    /// This can be useful for debugging without introducing any value management. e.g. In $destruct.
    nop_label,

    dumpBytecode,
    verbose,
};

pub const Continue = struct {
    base: Stmt = undefined,

    // Offset from the function's block.
    block_offset: u32,
};

pub const Break = struct {
    base: Stmt = undefined,

    // Offset from the function's block.
    block_offset: u32,
};

pub const SimpleStmt = struct {
    base: Stmt = undefined,
};

pub const Stmt = extern struct {
    code: StmtCode,
    node: *ast.Node,
    next: ?*Stmt,

    pub fn cast(self: *Stmt, comptime code: StmtCode) *StmtImpl(code) {
        if (cy.Trace) {
            if (self.code != code) {
                std.debug.panic("Expected {}, found {}.", .{code, self.code});
            }
        }
        return @ptrCast(@alignCast(self));
    }
};

pub const ExprCode = enum(u8) {
    and_op,
    bitcast,
    captured,
    case_cond,
    cast,
    closure,
    const8,
    const16,
    const32,
    const64,
    falsev,
    field,
    func,
    func_ptr,
    init,
    init_case,
    init_zero,
    is_zero,
    lambda,
    lift,
    local,
    spawn,
    global,
    string,
    truev,
    undef,
    voidv,
    switch_case,
    or_op,
    neg,
    not,
    xor,
    lsl,
    asr,
    lsr,
    add,
    sub,
    imul,
    mul,
    idiv,
    div,
    mod,
    imod,
    fadd,
    fsub,
    fmod,
    fmul,
    fneg,
    fdiv,
    fabs,
    cmp,
    f2i,
    i2f,
    zext,
    sext,
    binary_op,
    unary_op,
    call_ptr,
    call_union,
    call,
    call_trait,
    retain,

    gen_next,
    gen_end,

    mainEnd,
    unwrap_addr,
    unwrap_nz,
    trait,
    address_of,
    deref,
    trunc,
    case,
    vector,
    partial_vector,
};

pub const SimpleExpr = struct {
    base: Expr = undefined,
};

pub const Expr = extern struct {
    code: ExprCode,
    node: *ast.Node,
    type: *cy.Type,

    pub fn cast(self: *Expr, comptime code: ExprCode) *ExprImpl(code) {
        if (cy.Trace) {
            if (self.code != code) {
                std.debug.panic("Expected {}, found {}.", .{code, self.code});
            }
        }
        return @ptrCast(@alignCast(self));
    }
};

pub const Condition = enum {
    eq,
    ne,
    gt,
    ge,
    lt,
    le,
    ult,
    ule,
    ugt,
    uge,

    pub fn isUnsigned(self: *Condition) bool {
        switch (self.*) {
            .ult, .ule, .ugt, .uge => return true,
            else => return false,
        }
    }
};

pub const GenNext = struct {
    base: Expr = undefined,
    gen: *Expr,
};

pub const GenEnd = struct {
    base: Expr = undefined,
    gen: *Expr,
};

pub const Compare = struct {
    base: Expr = undefined,
    left: *Expr,
    right: *Expr,
    cond: Condition,
};

pub const Case = struct {
    base: Expr = undefined,
    child: *Expr,
    union_t: *cy.Type,
    case: u32,
};

pub const Yield = struct {
    base: Stmt = undefined,
    deinit_head: ?*Stmt,
    ret_opt_t: *cy.Type,
    end: bool,
};

pub const ReturnExpr = struct {
    base: Stmt = undefined,
    expr: *Expr,
};

pub const ReturnGen = struct {
    base: Stmt = undefined,
    deinit_head: ?*Stmt,
    gen_t: *cy.Type,
    next_t: *cy.Type,
};

pub const Return = struct {
    base: Stmt = undefined,
    return_value: bool,
};

pub const Release = struct {
    base: Stmt = undefined,
    reg: u32,
    optional: bool,
    deinit_obj: *cy.Func,
};

pub const Retain = struct {
    base: Expr = undefined,
    expr: *Expr,
};

pub const Lift = struct {
    base: Expr = undefined,
    child: *Expr,
};

pub const Trunc = struct {
    base: Expr = undefined,
    expr: *Expr,
    from_bits: u8,
    to_bits: u8,
};

pub const Await = struct {
    base: Stmt = undefined,
    expr: *Expr,
};

pub const AddressOf = struct {
    base: Expr = undefined,
    expr: *Expr,
};

pub const Deref = struct {
    base: Expr = undefined,
    expr: *Expr,
};

pub const Trait = struct {
    base: Expr = undefined,
    ref: *Expr,
    impl_t: *cy.Type,
    generic_t: *cy.types.GenericTrait,
    trait_t: *cy.Type,
};

pub const UnwrapAddr = struct {
    base: Expr = undefined,
    choice: *Expr,
    tag: u8,
};

pub const UnwrapNZ = struct {
    base: Expr = undefined,
    option: *Expr,
};

pub const PopLocals = struct {
    base: Stmt = undefined,
    local_end: usize,
};

pub const Block = struct {
    base: Stmt = undefined,
    bodyHead: ?*Stmt,
};

pub const SwitchStmt = struct {
    base: Stmt = undefined,
    cases: [*]*Expr,
    numCases: u8,
};

const Loc = u32;
const Reg = packed struct {
    id: u31,
    temp: bool,
};

pub const SwitchCase = struct {
    base: Expr = undefined,
    // else case if `numConds` == 0.
    numConds: u8,
    body_head: ?*Stmt,
    conds: [*]*Expr,
    fallthrough: bool,
};

pub const CaseCond = struct {
    base: Expr = undefined,
    body_head: ?*Stmt,
    expr: *Expr,
};

pub const LoopBlock = struct {
    base: Stmt = undefined,
    body_head: ?*Stmt,
};

pub const Captured = struct {
    base: Expr = undefined,
    idx: u8,
};

pub const Bitcast = struct {
    base: Expr = undefined,
    expr: *Expr,
};

pub const Cast = struct {
    base: Expr = undefined,
    expr: *Expr,
    type: *cy.Type,
    isRtCast: bool,
};

/// Can have a chain of nested struct field indexes.
/// The array of nested indexes are located after this struct.
pub const Field = struct {
    base: Expr = undefined,
    /// Receiver.
    rec: *Expr,

    /// Field index of receiver.
    idx: u8,

    /// Whether there is a followup member access.
    member_cont: bool = false,
};

pub const SetField = struct {
    base: Stmt = undefined,
    field: *Expr,
    right: *Expr,
};

pub const Spawn = struct {
    base: Expr = undefined,
    callee: *Expr,
    args: [*]*Expr,
    nargs: u8,
};

pub const Init = struct {
    base: Expr = undefined,
    args: [*]*Expr,
    nargs: u8,
};

pub const InitZero = struct {
    base: Expr = undefined,
    size: usize,
};

pub const InitCase = struct {
    base: Expr = undefined,
    child: *Expr,
    case: u32,
};

pub const ForRangeStmt = struct {
    base: Stmt = undefined,
    start: *Expr,
    end: *Expr,
    eachLocal: ?u8,
    increment: bool,
    end_inclusive: bool,
    declHead: ?*Stmt,
    bodyHead: ?*Stmt,
};

pub const TryStmt = struct {
    base: Stmt = undefined,
    hasErrLocal: bool,
    errLocal: u8,
    bodyHead: ?*Stmt,
    catchBodyHead: *Stmt,
};

pub const Local = struct {
    base: Expr = undefined,
    id: u8,
};

pub const Lambda = struct {
    base: Expr = undefined,
    func: *cy.Func,

    // If `numCaptures` > 0, this is a closure.
    numCaptures: u8,
    captures: [*]u8,

    // Captures locals and must be pinned. (Initialized as an `OpaqueFunc`)
    pinned_closure: bool,
};

pub const FuncBlock = struct {
    base: Stmt = undefined,
    func: *cy.Func,
    bodyHead: ?*Stmt,
    params: [*]FuncParam,

    // For methods only.
    parentType: ?*cy.Type,

    /// Mark a func block to skip generation.
    /// Useful for removing temporary compile-time functions.
    skip: bool = false,
};

pub const PushBlock = struct {
    maxLocals: u8,
};

pub const MainBlock = struct {
    base: Stmt = undefined,
    bodyHead: ?*Stmt,
};

pub const FuncParam = struct {
    reg: u8,
    namePtr: [*]const u8,
    nameLen: u16,
    declType: *cy.Type,

    pub fn name(self: FuncParam) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

pub const Discard = struct {
    base: Stmt = undefined,
    expr: *Expr,
};

pub const DeclareLocal = struct {
    base: Stmt = undefined,
    name_ptr: [*]const u8,
    name_len: u16,
    decl_t: *cy.Type,
    id: u8,

    // If null, only reserve the local.
    init: ?*Expr,

    pub fn name(self: DeclareLocal) []const u8 {
        return self.name_ptr[0..self.name_len];
    }
};

pub const Slice = struct {
    base: Expr = undefined,
    recvT: *cy.Type,
    rec: *Expr,
    left: *Expr,
    right: *Expr,
};

pub const BinOp2 = struct {
    base: Expr = undefined,
    left: *Expr,
    right: *Expr,
};

pub const BinOp = struct {
    base: Expr = undefined,
    leftT: *cy.Type,
    rightT: *cy.Type,
    op: cy.BinaryExprOp,
    left: *Expr,
    right: *Expr,
};

pub const SetDeref = struct {
    base: Stmt = undefined,
    ptr: *Expr,
    right: *Expr,
};

pub const SetGeneric = struct {
    base: Stmt = undefined,
    left_t: *cy.Type,
    right_t: *cy.Type,
    left: *Expr,
    right: *Expr,
};

pub const SetReturn = struct {
    base: Stmt = undefined,
    right: *Expr,
};

pub const SetLocal = struct {
    base: Stmt = undefined,
    id: u8,
    right: *Expr,
};

pub const SetGlobal = struct {
    base: Stmt = undefined,
    sym: *cy.Sym,
    expr: *Expr,
};

pub const Global = struct {
    base: Expr = undefined,
    sym: *cy.Sym,
};

pub const FuncPtr = struct {
    base: Expr = undefined,
    func: *cy.Func,
};

pub const Func = struct {
    base: Expr = undefined,
    expr: *Expr,
};

pub const Call = struct {
    base: Expr = undefined,
    func: *cy.Func,
    numArgs: u8,
    args: [*]*Expr,
};

pub const CallTrait = struct {
    base: Expr = undefined,
    trait: *Expr,
    args: [*]*Expr,
    nargs: u8,
    vtable_idx: u8,
};

pub const CallPtr = struct {
    base: Expr = undefined,
    callee: *Expr,
    args: [*]*Expr,
    nargs: u8,
};

pub const CallUnion = struct {
    base: Expr = undefined,
    callee: *Expr,
    args: [*]*Expr,
    nargs: u8,
};

pub const Const64 = struct {
    base: Expr = undefined,
    val: u64,
};

pub const Const16 = struct {
    base: Expr = undefined,
    val: u16,
};

pub const Const32 = struct {
    base: Expr = undefined,
    val: u32,
};

pub const Const8 = struct {
    base: Expr = undefined,
    val: u8,
};

pub const IsZero = struct {
    base: Expr = undefined,
    child: *Expr,
};

pub const NopLabel = struct {
    base: Stmt = undefined,
    label_idx: usize,
    label_len: usize,
};

pub const String = struct {
    base: Expr = undefined,
    raw: []const u8,
};

pub const UnOp = struct {
    base: Expr = undefined,
    expr: *Expr,
    childT: *cy.Type,
    op: cy.UnaryOp,
};

pub const Widen = struct {
    base: Expr = undefined,
    expr: *Expr,
    src_bits: u32,
};

pub const UnOp2 = struct {
    base: Expr = undefined,
    expr: *Expr,
};

pub const StmtBlock = struct {
    first: ?*Stmt,
    last: ?*Stmt,
};

pub const IfBlock = struct {
    base: Stmt = undefined,
    cond_expr: *Expr,
    body_head: ?*Stmt,
};

pub const Verbose = struct {
    verbose: bool,
};

pub fn StmtImpl(comptime code: StmtCode) type {
    return comptime switch (code) {
        .mainBlock => MainBlock,
        .funcBlock => FuncBlock,
        .declare_local => DeclareLocal,
        .discard => Discard,
        .if_block => IfBlock,
        .tryStmt => TryStmt,
        .forRangeStmt => ForRangeStmt,
        .set_local => SetLocal,
        .set_ret => SetReturn,
        .setCaptured => SetGeneric,
        .set_field => SetField,
        .set_global => SetGlobal,
        .set_deref => SetDeref,
        .verbose => Verbose,
        .block => Block,
        .pop_locals => PopLocals,
        .loop_block => LoopBlock,
        .switch_stmt => SwitchStmt,
        .release => Release,
        .nop_label => NopLabel,
        .yield => Yield,
        .await_ => Await,
        .ret_expr => ReturnExpr,
        .ret_gen => ReturnGen,
        .ret => Return,
        .break_ => Break,
        .continue_ => Continue,
        else => SimpleStmt,
    };
}

pub fn ExprImpl(comptime code: ExprCode) type {
    return switch (code) {
        .lambda => Lambda,
        .switch_case => SwitchCase,
        .case_cond => CaseCond,
        .call_ptr => CallPtr,
        .call_union => CallUnion,
        .call => Call,
        .call_trait => CallTrait,
        .sext,
        .zext => Widen,
        .f2i,
        .i2f,
        .fabs,
        .fneg,
        .neg,
        .not => UnOp2,
        .unary_op => UnOp,
        .binary_op => BinOp,
        .cmp => Compare,
        .fadd,
        .fsub,
        .fmod,
        .fmul,
        .fdiv,
        .asr,
        .lsr,
        .lsl,
        .xor,
        .or_op,
        .and_op,
        .add,
        .sub,
        .imul,
        .mul,
        .idiv,
        .div,
        .imod,
        .mod => BinOp2,
        .global => Global,
        .func_ptr => FuncPtr,
        .func => Func,
        .const64 => Const64,
        .const32 => Const32,
        .const16 => Const16,
        .const8 => Const8,
        .is_zero => IsZero,
        .local => Local,
        .string => String,
        .init => Init,
        .init_case => InitCase,
        .init_zero => InitZero,
        .lift => Lift,
        .retain => Retain,
        .spawn => Spawn,
        .field => Field,
        .cast => Cast,
        .bitcast => Bitcast,
        .captured => Captured,
        .unwrap_addr => UnwrapAddr,
        .unwrap_nz => UnwrapNZ,
        .case => Case,
        .trait => Trait,
        .address_of => AddressOf,
        .deref => Deref,
        .trunc => Trunc,
        .gen_next => GenNext,
        .gen_end => GenEnd,
        else => SimpleExpr,
    };
}

/// IR ops use an explicit index since the underlying buffer can grow.
pub const Buffer = struct {
    arena: std.heap.ArenaAllocator,
    alloc: std.mem.Allocator,

    /// Used to store strings.
    buf: std.ArrayListUnmanaged(u8),

    stmtBlockStack: std.ArrayListUnmanaged(StmtBlock),

    /// Main and func blocks.
    func_blocks: std.ArrayListUnmanaged(*Stmt),

    pub fn init(self: *Buffer, alloc: std.mem.Allocator) void {
        self.* = .{
            .arena = .{ .child_allocator = alloc, .state = .{} },
            .alloc = undefined,
            .stmtBlockStack = .{},
            .func_blocks = .{},
            .buf = .{},
        };
        self.alloc = self.arena.allocator();
    }

    pub fn deinit(self: *Buffer, alloc: std.mem.Allocator) void {
        self.stmtBlockStack.deinit(alloc);
        self.func_blocks.deinit(alloc);
        self.buf.deinit(alloc);
        self.arena.deinit();
    }

    pub fn curBlock(self: *Buffer) *StmtBlock {
        return &self.stmtBlockStack.items[self.stmtBlockStack.items.len-1];
    }

    pub fn pushStmtBlock(self: *Buffer, alloc: std.mem.Allocator) !void {
        try self.stmtBlockStack.append(alloc, .{
            .first = null,
            .last = null,
        });
    }

    pub fn pushStmtBlock2(self: *Buffer, alloc: std.mem.Allocator, block: StmtBlock) !void {
        try self.stmtBlockStack.append(alloc, block);
    }

    pub fn popStmtBlock(self: *Buffer) StmtBlock {
        return self.stmtBlockStack.pop().?;
    }

    pub fn newData(self: *Buffer, comptime T: type) !*T {
        return self.alloc.new(T);
    }

    pub fn allocArray(self: *Buffer, comptime T: type, len: usize) ![]T {
        return self.alloc.alloc(T, len);
    }

    pub fn newExpr(self: *Buffer, comptime code: ExprCode, type_: *cy.Type, node: *ast.Node, data: ExprImpl(code)) !*Expr {
        log.tracev("ir expr: {} {s}", .{code, type_.name()});
        const new = try self.alloc.create(ExprImpl(code));
        new.* = data;
        new.base = .{
            .code = code,
            .type = type_,
            .node = node,
        };
        return @ptrCast(new);
    }

    pub fn newStmt(self: *Buffer, comptime code: StmtCode, node: *ast.Node, data: StmtImpl(code)) !*Stmt {
        const new = try self.alloc.create(StmtImpl(code));
        log.tracev("ir stmt: {} {*}", .{code, new});
        new.* = data;
        new.base = .{
            .code = code,
            .node = node,
            .next = null,
        };
        return @ptrCast(new);
    }

    pub fn pushStmt(self: *Buffer, comptime code: StmtCode, node: *ast.Node, data: StmtImpl(code)) !*Stmt {
        const stmt = try self.newStmt(code, node, data);
        self.appendStmt(stmt);
        return stmt;
    }

    pub fn getAndClearStmtBlock(self: *Buffer) ?*Stmt {
        const b = &self.stmtBlockStack.items[self.stmtBlockStack.items.len-1];
        defer {
            b.first = null;
            b.last = null;
        }
        return b.first;
    }

    pub fn appendStmt(self: *Buffer, stmt: *Stmt) void {
        const list = &self.stmtBlockStack.items[self.stmtBlockStack.items.len-1];
        if (list.last == null) {
            // Set head stmt.
            list.first = stmt;
            list.last = stmt;
        } else {
            // Attach to last.
            list.last.?.next = stmt;
            list.last = stmt;
        }
    }
};

pub const VisitStmtFn = *const fn(ctx: *VisitContext, stmt: *Stmt) anyerror!bool;
pub const VisitExprFn = *const fn(ctx: *VisitContext, expr: *Expr) anyerror!bool;

pub const VisitContext = struct {
    ctx: ?*anyopaque,
    visit_stmt: VisitStmtFn,
    visit_expr: VisitExprFn,
};

pub fn visitStmts(ctx: *VisitContext, head: ?*Stmt) !bool {
    var stmt_opt = head;
    while (stmt_opt) |stmt| {
        if (!try ctx.visit_stmt(ctx, stmt)) return false;
        switch (stmt.code) {
            .mainBlock => {
                const data = stmt.cast(.mainBlock);
                if (!try visitStmts(ctx, data.bodyHead)) return false;
            },
            .funcBlock => {
                const data = stmt.cast(.funcBlock);
                if (!try visitStmts(ctx, data.bodyHead)) return false;
            },
            .declare_local => {
                const data = stmt.cast(.declare_local);
                if (data.init) |init| {
                    if (!try visitExpr(ctx, init)) return false;
                }
            },
            .set_ret => {
                const data = stmt.cast(.set_ret);
                if (!try visitExpr(ctx, data.right)) return false;
            },
            .block => {
                const data = stmt.cast(.block);
                if (!try visitStmts(ctx, data.bodyHead)) return false;
            },
            .if_block => {
                const data = stmt.cast(.if_block);

                if (!try visitExpr(ctx, data.cond_expr)) return false;
                if (!try visitStmts(ctx, data.body_head)) return false;
            },
            .loop_block => {
                const data = stmt.cast(.loop_block);
                if (!try visitStmts(ctx, data.body_head)) return false;
            },
            .set_local => {
                const data = stmt.cast(.set_local);
                if (!try visitExpr(ctx, data.right)) return false;
            },
            .set_field => {
                const data = stmt.cast(.set_field);
                if (!try visitExpr(ctx, data.right)) return false;
                if (!try visitExpr(ctx, data.field)) return false;
            },
            .forRangeStmt => {
                const data = stmt.cast(.forRangeStmt);
                if (!try visitExpr(ctx, data.start)) return false;
                if (!try visitExpr(ctx, data.end)) return false;
                if (!try visitStmts(ctx, data.bodyHead)) return false;
            },
            .switch_stmt => {
                const data = stmt.cast(.switch_stmt);
                const cases = data.cases[0..data.numCases];
                for (cases) |case| {
                    const case_data = case.cast(.switch_case);
                    if (!try visitExpr(ctx, case)) return false;
                    for (case_data.conds[0..case_data.numConds]) |cond| {
                        if (!try visitExpr(ctx, cond)) return false;
                    }
                    if (!try visitStmts(ctx, case_data.body_head)) return false;
                }
            },
            .set_deref => {
                const data = stmt.cast(.set_deref);
                if (!try visitExpr(ctx, data.right)) return false;
                if (!try visitExpr(ctx, data.ptr)) return false;
            },
            .set_global => {
                const data = stmt.cast(.set_global);
                if (!try visitExpr(ctx, data.expr)) return false;
            },
            .ret,
            .release,
            .break_,
            .continue_,
            .nop_label => {},
            else => {
                std.debug.panic("TODO: {}", .{stmt.code});
            }
        }
        stmt_opt = stmt.next;
    }
    return true;
}

fn visitExpr(ctx: *VisitContext, expr: *Expr) !bool {
    switch (expr.code) {
        .vector => {
            const data = expr.cast(.vector);
            for (data.args[0..data.nargs]) |arg| {
                if (!try visitExpr(ctx, arg)) return false;
            }
        },
        .call => {
            const data = expr.cast(.call);
            for (data.args[0..data.numArgs]) |arg| {
                if (!try visitExpr(ctx, arg)) return false;
            }
        },
        .call_ptr => {
            const data = expr.cast(.call_ptr);
            if (!try visitExpr(ctx, data.callee)) return false;
            for (data.args[0..data.nargs]) |arg| {
                if (!try visitExpr(ctx, arg)) return false;
            }
        },
        .func => {
            const data = expr.cast(.func);
            if (!try visitExpr(ctx, data.expr)) return false;
        },
        .field => {
            const data = expr.cast(.field);
            if (!try visitExpr(ctx, data.rec)) return false;
        },
        .binary_op => {
            const data = expr.cast(.binary_op);
            if (!try visitExpr(ctx, data.left)) return false;
            if (!try visitExpr(ctx, data.right)) return false;
        },
        // .switch_case => {
        //     const data = expr.cast(.switch_case);
        //     self.clearTemps();
        //     try self.pushStmts(data.body_head);
        //     try self.prependTemps();
        // },
        .bitcast => {
            const data = expr.cast(.bitcast);
            if (!try visitExpr(ctx, data.expr)) return false;
        },
        .address_of => {
            const data = expr.cast(.address_of);
            if (!try visitExpr(ctx, data.expr)) return false;
        },
        .deref => {
            const data = expr.cast(.deref);
            if (!try visitExpr(ctx, data.expr)) return false;
        },
        .init => {
            const data = expr.cast(.init);
            for (data.args[0..data.numArgs]) |arg| {
                if (!try visitExpr(ctx, arg)) return false;
            }
        },
        .cmp => {
            const data = expr.cast(.cmp);
            if (!try visitExpr(ctx, data.left)) return false;
            if (!try visitExpr(ctx, data.right)) return false;
        },
        .and_op => {
            const data = expr.cast(.and_op);
            if (!try visitExpr(ctx, data.left)) return false;
            if (!try visitExpr(ctx, data.right)) return false;
        },
        .or_op => {
            const data = expr.cast(.or_op);
            if (!try visitExpr(ctx, data.left)) return false;
            if (!try visitExpr(ctx, data.right)) return false;
        },
        .lsr => {
            const data = expr.cast(.lsr);
            if (!try visitExpr(ctx, data.left)) return false;
            if (!try visitExpr(ctx, data.right)) return false;
        },
        .lsl => {
            const data = expr.cast(.lsl);
            if (!try visitExpr(ctx, data.left)) return false;
            if (!try visitExpr(ctx, data.right)) return false;
        },
        .add => {
            const data = expr.cast(.add);
            if (!try visitExpr(ctx, data.left)) return false;
            if (!try visitExpr(ctx, data.right)) return false;
        },
        .mul => {
            const data = expr.cast(.mul);
            if (!try visitExpr(ctx, data.left)) return false;
            if (!try visitExpr(ctx, data.right)) return false;
        },
        .unary_op => {
            const data = expr.cast(.unary_op);
            if (!try visitExpr(ctx, data.expr)) return false;
        },
        .is_zero => {
            const data = expr.cast(.is_zero);
            if (!try visitExpr(ctx, data.child)) return false;
        },
        .case => {
            const data = expr.cast(.case);
            if (!try visitExpr(ctx, data.child)) return false;
        },
        .retain => {
            const data = expr.cast(.retain);
            if (!try visitExpr(ctx, data.expr)) return false;
        },
        .lift => {
            const data = expr.cast(.lift);
            if (!try visitExpr(ctx, data.src_ptr)) return false;
        },
        .global,
        .byte,
        .undef,
        .voidv,
        .func_ptr,
        .lambda,
        .local,
        .truev,
        .falsev,
        .float,
        .int,
        .string => {},
        else => {
            // std.debug.panic("TODO: {}", .{expr.code});
        }
    }
    if (!try ctx.visit_expr(ctx, expr)) return false;
    return true;
}
