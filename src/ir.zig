const std = @import("std");
const cy = @import("cyber.zig");
const TypeId = cy.TypeId;
const CompactType = cy.types.CompactType;
const sema = cy.sema;
const log = cy.log.scoped(.ir);
const ast = cy.ast;

/// An IR is useful because the AST isn't suited to represent the result of sema.
/// The IR is generated during the sema pass and thrown away after it is consumed by codegen.
/// A couple of reasons for an IR:
/// 1. Since there is static symbol resolution, some statements and expressions become irrelevant.
/// 2. Type inference, an operation's invocation may be patched:
///    Bin-exprs could be inlined rather than invoking an operator function.
/// 3. Lifted vars/param copies can be patched as sema discovers them.
/// 4. Additional IR can be added that have no source attribution such as
///    static variable initializers and zero values.
/// 5. Makes bc codegen simpler, creating additional backends should also be simpler.
///
/// The IR is a tree structure but all the nodes are packed into a linear array.
/// IR nodes are indexed by their position in the array.
/// Stmt and expr nodes contain metadata which starts with their node type followed by a pointer to their AST node.
/// Stmt nodes additionally have a next index that points to the next stmt node.
/// Expr nodes additionally have a type id that also contains a throws bit.
/// Data specific to a node's type follows the metadata.

pub const StmtCode = enum(u8) {
    root,

    /// Main block. Only one chunk has IR for the main block.
    mainBlock,

    /// A [numParams]u8 array follows.
    /// Each elem is a boolean indicating whether the respective param
    /// will be written to at some point in the function.
    funcBlock,

    declareLocal,
    declareLocalInit,

    block,

    exprStmt,
    ifStmt,
    switchStmt,
    tryStmt,
    loopStmt,
    forRangeStmt,
    retStmt,
    retExprStmt,
    breakStmt,
    contStmt,

    /// precedes a set* stmt.
    opSet,

    set,
    setLocal,
    setCaptured,
    set_field_dyn,
    set_field,
    setIndex,
    setVarSym,

    pushDebugLabel,
    dumpBytecode,
    verbose,
};

pub const ExprCode = enum(u8) {
    cast,

    coinitCall,
    coyield,
    coresume,

    await_expr,

    local,
    object_init,

    fieldDyn,
    field,

    list,
    map,

    truev,
    falsev,
    errorv,
    symbol,
    tag_lit,
    float,
    int,
    none,
    unOpEnd,
    funcSym,
    varSym,
    context,
    typeSym,
    enumMemberSym,
    string,
    stringTemplate,
    array,
    lambda,
    closure,
    if_expr,
    captured,
    throw,
    switchExpr,
    switchCase,

    /// Placeholder that is patched later to be `preCall`, `preBinOp`, `preUnOp`, etc.
    pre,
    preBinOp,
    preUnOp,
    call_dyn,
    call_obj_sym,
    call_sym,
    call_sym_dyn,
    call_trait,

    andOp,
    orOp,

    tryExpr,
    type_check,
    typeCheckOption,

    blockExpr,
    mainEnd,
    else_block,
    unwrapChoice,
    unwrap_or,
    box,
    unbox,
    trait,
};

pub const ExprType = packed struct {
    id: u31,
    throws: bool,

    pub fn init(id: cy.TypeId) ExprType {
        return .{ .id = @intCast(id), .throws = false };
    }

    pub fn initThrows(id: cy.TypeId) ExprType {
        return .{ .id = @intCast(id), .throws = true };
    }
};

pub const Await = struct {
    expr: Loc,
};

pub const Trait = struct {
    expr: Loc,
    expr_t: cy.TypeId,
    trait_t: cy.TypeId,
};

pub const Box = struct {
    expr: Loc,
};

pub const Unbox = struct {
    expr: Loc,
};

pub const TypeCheck = struct {
    expr: Loc,
    exp_type: cy.TypeId,
};

pub const TypeCheckOption = struct {
    expr: Loc,
};

pub const UnwrapChoice = struct {
    choice: Loc,
    tag: u8,
    fieldIdx: u8,
};

pub const UnwrapOr = struct {
    opt: Loc,
    default: Loc,
};

pub const Coresume = struct {
    expr: Loc,
};

pub const Block = struct {
    bodyHead: Loc,
};

pub const BlockExpr = struct {
    bodyHead: Loc,
};

pub const Switch = struct {
    expr: Loc,
    numCases: u8,
    is_expr: bool,
};

const Loc = u32;

pub const SwitchCase = struct {
    // else case if `numConds` == 0.
    numConds: u8,
    bodyIsExpr: bool,
    bodyHead: Loc,
};

pub const LoopStmt = struct {
    body_head: Loc,
};

pub const PushDebugLabel = struct {
    name: []const u8,
};

pub const Captured = struct {
    idx: u8,
};

pub const IfExpr = struct {
    cond: Loc,
    body: u32,
    elseBody: u32,
};

pub const ThrowExpr = struct {
    expr: Loc,
};

pub const TryExpr = struct {
    expr: Loc,
    catchBody: u32,
};

pub const TagLit = struct {
    name: []const u8,
};

pub const Symbol = struct {
    name: []const u8,
};

pub const Error = struct {
    name: []const u8,
};

pub const Cast = struct {
    expr: Loc,
    typeId: TypeId,
    isRtCast: bool,
};

pub const FieldDyn = struct {
    name: []const u8,
    rec: Loc,
};

pub const SetFieldDyn = struct {
    name: []const u8,
    rec: Loc,
    right: Loc,
};

/// Can have a chain of nested struct field indexes.
/// The array of nested indexes are located after this struct.
pub const Field = struct {
    /// Receiver.
    rec: Loc,

    /// Field index of receiver.
    idx: u8,

    /// Number of nested field indexes.
    numNestedFields: u8,
};

pub const SetField = struct {
    field: Loc,
    right: Loc,
};

pub const ObjectInit = struct {
    typeId: TypeId,
    args: Loc,
    numArgs: u8,
};

pub const ForRangeStmt = struct {
    start: Loc,
    end: Loc,
    eachLocal: ?u8,
    increment: bool,
    declHead: u32,
    bodyHead: u32,
};

pub const TryStmt = struct {
    hasErrLocal: bool,
    errLocal: u8,
    bodyHead: u32,
    catchBodyHead: u32,
};

pub const Local = struct {
    id: u8,
};

pub const Lambda = struct {
    func: *cy.Func,
    maxLocals: u8,

    // If `numCaptures` > 0, this is a closure.
    numCaptures: u8,
    numParamCopies: u8,
    bodyHead: u32,
    captures: u32,
};

pub const FuncBlock = struct {
    func: *cy.Func,
    maxLocals: u8,
    numParamCopies: u8,
    bodyHead: u32,

    // For methods only.
    parentType: cy.TypeId,
};

pub const PushBlock = struct {
    maxLocals: u8,
};

pub const Root = struct {
    bodyHead: u32,
};

pub const MainBlock = struct {
    maxLocals: u8,
    bodyHead: u32,
};

pub const FuncParam = struct {
    namePtr: [*]const u8,
    nameLen: u16,
    declType: TypeId,
    isCopy: bool,
    lifted: bool,

    pub fn name(self: FuncParam) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

pub const DeclareLocalInit = struct {
    namePtr: [*]const u8,
    nameLen: u16,
    declType: TypeId,
    id: u8,
    lifted: bool,

    /// If the local depends on a child local (declared in a block expr),
    /// the memory must be zeroed so unwinding doesn't end up using an undefined value.
    zeroMem: bool,
    init: Loc,
    initType: CompactType,

    pub fn name(self: DeclareLocalInit) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

pub const DeclareLocal = struct {
    namePtr: [*]const u8,
    nameLen: u16,
    declType: TypeId,
    id: u8,
    lifted: bool,

    pub fn name(self: DeclareLocal) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

/// Several pre codes share a union so that sema
/// can generate the IR in one pass by back-patching.
pub const Prepare = union {
    binOp: BinOp,
    slice: Slice,
    unOp: UnOp,

    pub fn initCall(numArgs: u8) Prepare {
        return .{
            .call = .{
                .numArgs = numArgs,
            }
        };
    }
};

pub const Slice = struct {
    recvT: TypeId,
    rec: Loc,
    left: Loc,
    right: Loc,
};

pub const BinOp = struct {
    leftT: TypeId,
    rightT: TypeId,
    op: cy.BinaryExprOp,
    left: Loc,
    right: Loc,
};

pub const Set = union {
    local: SetLocal,
    index: SetIndex,
    generic: SetGeneric,
    callObjSymTern: SetCallObjSymTern,
    set_field_dyn: SetFieldDyn,
    set_field: SetField,
};

pub const SetCallObjSymTern = struct {
    name: []const u8,
    rec: Loc,
    index: Loc,
    right: Loc,
};

pub const SetGeneric = struct {
    left_t: CompactType,
    right_t: CompactType,
    left: Loc,
    right: Loc,
};

pub const SetLocal = struct {
    id: u8,
    right: Loc,
};

pub const SetIndex = struct {
    recvT: TypeId,
    rec: Loc,
    index: Loc,
    right: Loc,
};

pub const Context = struct {
    sym: *cy.sym.ContextVar,
};

pub const VarSym = struct {
    sym: *cy.Sym,
};

pub const EnumMemberSym = struct {
    type: TypeId,
    val: u8,
};

pub const FuncSym = struct {
    func: *cy.Func,
};

pub const TypeSym = struct {
    typeId: TypeId,
};

pub const CoinitCall = struct {
    call: Loc,
};

pub const CallObjSym = struct {
    name: []const u8,
    rec: Loc,
    args: Loc,

    // Does not include rec.
    numArgs: u8,
};

pub const CallFuncSym = struct {
    func: *cy.Func,
    numArgs: u8,
    args: Loc,
};

pub const CallSymDyn = struct {
    sym: *cy.sym.FuncSym,
    nargs: u8,
    args: Loc,
};

pub const CallTrait = struct {
    trait: Loc,
    args: Loc,
    nargs: u8,
    vtable_idx: u8,
};

pub const CallDyn = struct {
    callee: Loc,
    args: Loc,
    numArgs: u8,
};

pub const RetExprStmt = struct {
    expr: Loc,
};

pub const ExprStmt = struct {
    expr: Loc,
    /// If in a block expression, returns as the result of the expression.
    /// If in the main block, can be used to return from an `eval`.
    isBlockResult: bool,
};

pub const Map = struct {
    placeholder: u8,
};

pub const List = struct {
    numArgs: u8,
};

pub const Float = struct {
    val: f64,
};

pub const Int = struct {
    val: i64,
};

pub const Array = struct {
    buffer: []const u8,
};

pub const None = struct {
    child: Loc,
};

pub const String = struct {
    raw: []const u8,
};

pub const StringTemplate = struct {
    numExprs: u8,
    args: u32,
};

pub const UnOp = struct {
    expr: Loc,
    childT: TypeId,
    op: cy.UnaryOp,
};

pub const StmtBlock = struct {
    first: u32,
    last: u32,
};

pub const ElseBlock = struct {
    cond: cy.Nullable(Loc),
    body_head: Loc,
    else_block: cy.Nullable(Loc),
};

pub const IfStmt = struct {
    cond: Loc,
    body_head: Loc,
    else_block: cy.Nullable(Loc),
};

pub const Verbose = struct {
    verbose: bool,
};

pub const OpSet = struct {
    op: cy.BinaryExprOp,
    set_stmt: Loc,
};

pub fn StmtData(comptime code: StmtCode) type {
    return comptime switch (code) {
        .root => Root,
        .mainBlock => MainBlock,
        .funcBlock => FuncBlock,
        .declareLocal => DeclareLocal,
        .declareLocalInit => DeclareLocalInit,
        .ifStmt => IfStmt,
        .tryStmt => TryStmt,
        .forRangeStmt => ForRangeStmt,
        .setIndex,
        .setLocal,
        .set_field_dyn,
        .setCaptured,
        .set_field,
        .setVarSym,
        .set => Set,
        .opSet => OpSet,
        .pushDebugLabel => PushDebugLabel,
        .verbose => Verbose,
        .exprStmt => ExprStmt,
        .block => Block,
        .retExprStmt => RetExprStmt,
        .loopStmt => LoopStmt,
        else => void,
    };
}

pub fn ExprData(comptime code: ExprCode) type {
    return switch (code) {
        .lambda => Lambda,
        .switchExpr => Switch,
        .switchCase => SwitchCase,
        .else_block => ElseBlock,
        .call_dyn => CallDyn,
        .call_sym => CallFuncSym,
        .call_trait => CallTrait,
        .call_sym_dyn => CallSymDyn,
        .call_obj_sym => CallObjSym,
        .coinitCall => CoinitCall,
        .preBinOp,
        .preUnOp,
        .pre => Prepare,
        .varSym => VarSym,
        .context => Context,
        .enumMemberSym => EnumMemberSym,
        .funcSym => FuncSym,
        .typeSym => TypeSym,
        .float => Float,
        .int => Int,
        .none => None,
        .local => Local,
        .if_expr => IfExpr,
        .tryExpr => TryExpr,
        .throw => ThrowExpr,
        .list => List,
        .map => Map,
        .array => Array,
        .string => String,
        .stringTemplate => StringTemplate,
        .object_init => ObjectInit,
        .fieldDyn => FieldDyn,
        .field => Field,
        .cast => Cast,
        .errorv => Error,
        .captured => Captured,
        .symbol => Symbol,
        .tag_lit => TagLit,
        .blockExpr => BlockExpr,
        .coresume => Coresume,
        .unwrapChoice => UnwrapChoice,
        .unwrap_or => UnwrapOr,
        .type_check => TypeCheck,
        .typeCheckOption => TypeCheckOption,
        .box => Box,
        .unbox => Unbox,
        .await_expr => Await,
        .trait => Trait,
        else => void,
    };
}

/// IR ops use an explicit index since the underlying buffer can grow.
pub const Buffer = struct {
    buf: std.ArrayListUnmanaged(u8),
    stmtBlockStack: std.ArrayListUnmanaged(StmtBlock),

    pub fn init() Buffer {
        return .{
            .buf = .{},
            .stmtBlockStack = .{},
        };
    }

    pub fn deinit(self: *Buffer, alloc: std.mem.Allocator) void {
        self.buf.deinit(alloc);
        self.stmtBlockStack.deinit(alloc);
    }

    pub fn setExprCode(self: *Buffer, idx: usize, comptime code: ExprCode) void {
        self.buf.items[idx] = @intFromEnum(code);
    }

    pub fn setExprData(self: *Buffer, idx: usize, comptime code: ExprCode, data: ExprData(code)) void {
        const bytes = std.mem.toBytes(data);
        @memcpy(self.buf.items[idx+1+8+4..idx+1+8+4+bytes.len], &bytes);
    }

    pub fn getExprData(self: *Buffer, idx: usize, comptime code: ExprCode) ExprData(code) {
        const data = self.buf.items[idx+1+8+4..][0..@sizeOf(ExprData(code))];
        return std.mem.bytesToValue(ExprData(code), data);
    }

    pub fn getExprDataPtr(self: *Buffer, idx: usize, comptime code: ExprCode) *align(1) ExprData(code) {
        const data = self.buf.items[idx+1+8+4..][0..@sizeOf(ExprData(code))];
        return std.mem.bytesAsValue(ExprData(code), data);
    }

    pub fn setStmtCode(self: *Buffer, idx: usize, comptime code: StmtCode) void {
        self.buf.items[idx] = @intFromEnum(code);
    }

    pub fn setStmtData(self: *Buffer, idx: usize, comptime code: StmtCode, data: StmtData(code)) void {
        const bytes = std.mem.toBytes(data);
        @memcpy(self.buf.items[idx+1+8+4..idx+1+8+4+bytes.len], &bytes);
    }

    pub fn getStmtData(self: *Buffer, idx: usize, comptime code: StmtCode) StmtData(code) {
        const data = self.buf.items[idx+1+8+4..][0..@sizeOf(StmtData(code))];
        return std.mem.bytesToValue(StmtData(code), data);
    }

    pub fn getStmtDataPtr(self: *Buffer, idx: usize, comptime code: StmtCode) *align(1) StmtData(code) {
        const data = self.buf.items[idx+1+8+4..][0..@sizeOf(StmtData(code))];
        return std.mem.bytesAsValue(StmtData(code), data);
    }

    pub fn advanceArray(_: *Buffer, idx: usize, comptime T: type, arr: []align(1) const T) usize {
        return idx + arr.len * @sizeOf(T);
    }

    pub fn advanceExpr(_: *Buffer, idx: usize, comptime code: ExprCode) usize {
        return idx + 1 + 8 + 4 + @sizeOf(ExprData(code));
    }

    pub fn advanceStmt(_: *Buffer, idx: usize, comptime code: StmtCode) usize {
        return idx + 1 + 8 + 4 + @sizeOf(StmtData(code));
    }

    pub fn pushStmtBlock(self: *Buffer, alloc: std.mem.Allocator) !void {
        try self.stmtBlockStack.append(alloc, .{
            .first = cy.NullId,
            .last = cy.NullId,
        });
    }

    pub fn pushStmtBlock2(self: *Buffer, alloc: std.mem.Allocator, block: StmtBlock) !void {
        try self.stmtBlockStack.append(alloc, block);
    }

    pub fn popStmtBlock(self: *Buffer) StmtBlock {
        return self.stmtBlockStack.pop();
    }

    pub fn pushEmptyExpr(self: *Buffer, comptime code: ExprCode, alloc: std.mem.Allocator, expr_t: ExprType, node_id: *ast.Node) !u32 {
        log.tracev("irPushExpr: {}", .{code});
        const start = self.buf.items.len;
        try self.buf.resize(alloc, self.buf.items.len + 1 + 8 + 4 + @sizeOf(ExprData(code)));
        self.buf.items[start] = @intFromEnum(code);
        self.setNode(start, node_id);
        self.setExprType2(start, expr_t);
        return @intCast(start);
    }

    pub fn reserveData(self: *Buffer, alloc: std.mem.Allocator, comptime T: type) !*align(1) T {
        const start = self.buf.items.len;
        try self.buf.resize(alloc, self.buf.items.len + @sizeOf(T));
        return @ptrCast(&self.buf.items[start]);
    }

    pub fn pushEmptyArray(self: *Buffer, alloc: std.mem.Allocator, comptime T: type, len: usize) !u32 {
        const start = self.buf.items.len;
        try self.buf.resize(alloc, self.buf.items.len + @sizeOf(T) * len);
        return @intCast(start);
    }

    pub fn setArrayItem(self: *Buffer, idx: usize, comptime T: type, elemIdx: usize, elem: T) void {
        @as(*align(1) T, @ptrCast(&self.buf.items[idx+@sizeOf(T)*elemIdx])).* = elem;
    }

    pub fn getArray(self: *Buffer, idx: usize, comptime T: type, len: usize) []align(1) T {
        const data = self.buf.items[idx..idx + @sizeOf(T) * len];
        return std.mem.bytesAsSlice(T, data);
    }

    pub fn pushExpr(self: *Buffer, comptime code: ExprCode, alloc: std.mem.Allocator, type_id: cy.TypeId, node: *ast.Node, data: ExprData(code)) !u32 {
        const expr_t = ExprType.init(type_id);
        const loc = try self.pushEmptyExpr(code, alloc, expr_t, node);
        self.setExprData(loc, code, data);
        return loc;
    }

    pub fn pushExprThrows(self: *Buffer, comptime code: ExprCode, alloc: std.mem.Allocator, type_id: cy.TypeId, node_id: *ast.Node, data: ExprData(code)) !u32 {
        const expr_t = ExprType.initThrows(type_id);
        const loc = try self.pushEmptyExpr(code, alloc, expr_t, node_id);
        self.setExprData(loc, code, data);
        return loc;
    }

    pub fn getExprCode(self: *Buffer, idx: usize) ExprCode {
        return @enumFromInt(self.buf.items[idx]);
    }

    pub fn getStmtCode(self: *Buffer, idx: usize) StmtCode {
        return @enumFromInt(self.buf.items[idx]);
    }

    pub fn getNode(self: *Buffer, idx: usize) *ast.Node {
        return @as(*align(1) *ast.Node, @ptrCast(self.buf.items.ptr + idx + 1)).*;
    }

    pub fn setNode(self: *Buffer, idx: usize, nodeId: *ast.Node) void {
        @as(*align(1) *ast.Node, @ptrCast(self.buf.items.ptr + idx + 1)).* = nodeId;
    }

    pub fn setExprType2(self: *Buffer, loc: usize, expr_t: ExprType) void {
        @as(*align(1) ExprType, @ptrCast(self.buf.items.ptr + loc + 1 + 8)).* = expr_t;
    }

    pub fn setExprType(self: *Buffer, loc: usize, type_id: cy.TypeId) void {
        const expr_t = ExprType.init(type_id);
        @as(*align(1) ExprType, @ptrCast(self.buf.items.ptr + loc + 1 + 8)).* = expr_t;
    }

    pub fn setExprTypeThrows(self: *Buffer, loc: usize, type_id: cy.TypeId) void {
        const expr_t = ExprType.initThrows(type_id);
        @as(*align(1) ExprType, @ptrCast(self.buf.items.ptr + loc + 1 + 8)).* = expr_t;
    }

    pub fn getExprType(self: *Buffer, loc: usize) ExprType {
        return @as(*align(1) ExprType, @ptrCast(self.buf.items.ptr + loc + 1 + 8)).*;
    }

    pub fn setStmtNext(self: *Buffer, idx: usize, nextIdx: u32) void {
        @as(*align(1) u32, @ptrCast(self.buf.items.ptr + idx + 1 + 8)).* = nextIdx;
    }

    pub fn getStmtNext(self: *Buffer, idx: usize) u32 {
        return @as(*align(1) u32, @ptrCast(self.buf.items.ptr + idx + 1 + 8)).*;
    }

    pub fn pushEmptyStmt(self: *Buffer, alloc: std.mem.Allocator, comptime code: StmtCode, node: *ast.Node) !u32 {
        return self.pushEmptyStmt2(alloc, code, node, true);
    }

    pub fn getAndClearStmtBlock(self: *Buffer) u32 {
        const b = &self.stmtBlockStack.items[self.stmtBlockStack.items.len-1];
        defer {
            b.first = cy.NullId;
            b.last = cy.NullId;
        }
        return b.first;
    }

    pub fn appendToParent(self: *Buffer, idx: u32) void {
        const list = &self.stmtBlockStack.items[self.stmtBlockStack.items.len-1];
        if (list.last == cy.NullId) {
            // Set head stmt.
            list.first = idx;
            list.last = idx;
        } else {
            // Attach to last.
            self.setStmtNext(list.last, idx);
            list.last = idx;
        }
    }

    pub fn pushEmptyStmt2(self: *Buffer, alloc: std.mem.Allocator, comptime code: StmtCode, node: *ast.Node, comptime appendToParent_: bool) !u32 {
        log.tracev("irPushStmt: {}", .{code});
        const start: u32 = @intCast(self.buf.items.len);
        try self.buf.resize(alloc, self.buf.items.len + 1 + 8 + 4 + @sizeOf(StmtData(code)));
        self.buf.items[start] = @intFromEnum(code);
        self.setNode(start, node);
        self.setStmtNext(start, cy.NullId);

        if (appendToParent_) {
            self.appendToParent(start);
        }
        return @intCast(start);
    }

    pub fn pushStmt(self: *Buffer, alloc: std.mem.Allocator, comptime code: StmtCode, nodeId: *ast.Node, data: StmtData(code)) !u32 {
        const idx = try self.pushEmptyStmt(alloc, code, nodeId);
        self.setStmtData(idx, code, data);
        return idx;
    }
};