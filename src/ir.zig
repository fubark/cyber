const std = @import("std");
const cy = @import("cyber.zig");
const TypeId = cy.TypeId;
const CompactType = cy.types.CompactType;
const sema = cy.sema;
const log = cy.log.scoped(.ir);

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
/// Stmt and expr nodes contain metadata which starts with their node type followed by their AST node id.
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
    ifUnwrapStmt,
    switchStmt,
    tryStmt,
    loopStmt,
    forRangeStmt,
    forIterStmt,
    destrElemsStmt,
    retStmt,
    retExprStmt,
    breakStmt,
    contStmt,

    /// precedes a set* stmt.
    opSet,

    set,
    setLocal,
    setCaptured,
    setFieldDyn,
    setField,
    setIndex,
    setCallObjSymTern,
    setVarSym,
    setFuncSym,

    /// Upon merging branches back, a dynamic local may have a new recent type.
    setLocalType,

    pushDebugLabel,
    dumpBytecode,
    verbose,
};

pub const ExprCode = enum(u8) {
    cast,

    coinitCall,
    coyield,
    coresume,

    local,
    objectInit,

    fieldDyn,
    field,

    list,
    map,

    truev,
    falsev,
    errorv,
    symbol,
    float,
    int,
    unOpEnd,
    funcSym,
    varSym,
    typeSym,
    enumMemberSym,
    string,
    stringTemplate,
    array,
    lambda,
    closure,
    condExpr,
    captured,
    throw,
    switchExpr,
    switchCase,

    /// Placeholder that is patched later to be `preCall`, `preBinOp`, `preUnOp`, etc.
    pre,
    preBinOp,
    preUnOp,
    preCallDyn,
    preCallObjSym,
    preCallObjSymUnOp,
    preCallObjSymBinOp,
    preCallFuncSym,

    andOp,
    orOp,

    tryExpr,
    typeCheckOption,

    blockExpr,
    mainEnd,
    else_block,
    unwrapChoice,
    box,
    range,
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

pub const Range = struct {
    start: cy.Nullable(Loc),
    end: cy.Nullable(Loc),
    inc: bool,
};

pub const Box = struct {
    expr: Loc,
};

pub const TypeCheckOption = struct {
    expr: Loc,
};

pub const UnwrapChoice = struct {
    choice: Loc,
    payload_t: cy.TypeId,
    tag: u8,
    fieldIdx: u8,
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
};

const Loc = u32;

pub const SwitchCase = struct {
    // else case if `numConds` == 0.
    numConds: u8,
    bodyIsExpr: bool,
    bodyHead: Loc,
};

pub const DestructureElems = struct {
    numLocals: u8,
    right: u32,
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

pub const CondExpr = struct {
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

pub const ObjectInit = struct {
    typeId: TypeId,
    numArgs: u8,
    numFieldsToCheck: u8,

    /// Idx into ir data.
    fieldsToCheck: u32,
};

pub const SetLocalType = struct {
    local: u8,
    type: CompactType,
};

pub const ForIterStmt = struct {
    eachLocal: ?u8,
    countLocal: ?u8,
    iter: Loc,
    declHead: u32,
    bodyHead: u32,
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
    callDyn: CallDyn,
    unOp: UnOp,
    callFuncSym: CallFuncSym,
    callObjSym: CallObjSym,
    callObjSymBinOp: CallObjSymBinOp,
    callObjSymUnOp: CallObjSymUnOp,

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
    index: SetIndex,
    generic: SetGeneric,
    callObjSymTern: SetCallObjSymTern,
};

pub const SetCallObjSymTern = struct {
    name: []const u8,
    funcSigId: sema.FuncSigId,
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

pub const SetIndex = struct {
    recvT: TypeId,
    rec: Loc,
    index: Loc,
    right: Loc,
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

pub const CallObjSym = struct {
    name: []const u8,
    funcSigId: sema.FuncSigId,
    rec: Loc,
    args: Loc,
    numArgs: u8,
};

pub const CallObjSymBinOp = struct {
    op: cy.BinaryExprOp,
    funcSigId: sema.FuncSigId,
    left: Loc,
    right: Loc,
};

pub const CallObjSymUnOp = struct {
    expr: Loc,
    op: cy.UnaryOp,
    funcSigId: sema.FuncSigId,
};

pub const CallFuncSym = struct {
    func: *cy.Func,
    hasDynamicArg: bool,
    numArgs: u8,
    args: u32,
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
    numArgs: u8,
    args: u32,
};

pub const List = struct {
    numArgs: u8,
};

pub const Float = struct {
    val: f64,
};

pub const Int = struct {
    val: u48,
};

pub const Array = struct {
    buffer: []const u8,
};

pub const String = struct {
    literal: []const u8,
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

pub const IfUnwrapStmt = struct {
    opt: Loc,
    unwrap_local: u8,
    decl_head: Loc,
    body_head: Loc,
    else_block: cy.Nullable(Loc),
};

pub const Verbose = struct {
    verbose: bool,
};

pub fn StmtData(comptime code: StmtCode) type {
    return comptime switch (code) {
        .root => Root,
        .mainBlock => MainBlock,
        .funcBlock => FuncBlock,
        .declareLocal => DeclareLocal,
        .declareLocalInit => DeclareLocalInit,
        .ifStmt => IfStmt,
        .ifUnwrapStmt => IfUnwrapStmt,
        .tryStmt => TryStmt,
        .forIterStmt => ForIterStmt,
        .forRangeStmt => ForRangeStmt,
        .setLocalType => SetLocalType,
        .setIndex,
        .setCallObjSymTern,
        .setLocal,
        .setFieldDyn,
        .setCaptured,
        .setField,
        .setFuncSym,
        .setVarSym,
        .set => Set,
        .pushDebugLabel => PushDebugLabel,
        .verbose => Verbose,
        .destrElemsStmt => DestructureElems,
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
        .preCallDyn,
        .preCallFuncSym,
        .preCallObjSym,
        .preCallObjSymBinOp,
        .preBinOp,
        .preUnOp,
        .preCallObjSymUnOp,
        .pre => Prepare,
        .varSym => VarSym,
        .enumMemberSym => EnumMemberSym,
        .funcSym => FuncSym,
        .typeSym => TypeSym,
        .float => Float,
        .int => Int,
        .local => Local,
        .condExpr => CondExpr,
        .tryExpr => TryExpr,
        .throw => ThrowExpr,
        .list => List,
        .map => Map,
        .array => Array,
        .string => String,
        .stringTemplate => StringTemplate,
        .objectInit => ObjectInit,
        .fieldDyn => FieldDyn,
        .field => Field,
        .cast => Cast,
        .errorv => Error,
        .captured => Captured,
        .symbol => Symbol,
        .blockExpr => BlockExpr,
        .coresume => Coresume,
        .unwrapChoice => UnwrapChoice,
        .typeCheckOption => TypeCheckOption,
        .box => Box,
        .range => Range,
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
        @memcpy(self.buf.items[idx+1+4+4..idx+1+4+4+bytes.len], &bytes);
    }

    pub fn getExprData(self: *Buffer, idx: usize, comptime code: ExprCode) ExprData(code) {
        const data = self.buf.items[idx+1+4+4..][0..@sizeOf(ExprData(code))];
        return std.mem.bytesToValue(ExprData(code), data);
    }

    pub fn getExprDataPtr(self: *Buffer, idx: usize, comptime code: ExprCode) *align(1) ExprData(code) {
        const data = self.buf.items[idx+1+4+4..][0..@sizeOf(ExprData(code))];
        return std.mem.bytesAsValue(ExprData(code), data);
    }

    pub fn setStmtCode(self: *Buffer, idx: usize, comptime code: StmtCode) void {
        self.buf.items[idx] = @intFromEnum(code);
    }

    pub fn setStmtData(self: *Buffer, idx: usize, comptime code: StmtCode, data: StmtData(code)) void {
        const bytes = std.mem.toBytes(data);
        @memcpy(self.buf.items[idx+1+4+4..idx+1+4+4+bytes.len], &bytes);
    }

    pub fn getStmtData(self: *Buffer, idx: usize, comptime code: StmtCode) StmtData(code) {
        const data = self.buf.items[idx+1+4+4..][0..@sizeOf(StmtData(code))];
        return std.mem.bytesToValue(StmtData(code), data);
    }

    pub fn getStmtDataPtr(self: *Buffer, idx: usize, comptime code: StmtCode) *align(1) StmtData(code) {
        const data = self.buf.items[idx+1+4+4..][0..@sizeOf(StmtData(code))];
        return std.mem.bytesAsValue(StmtData(code), data);
    }

    pub fn advanceArray(_: *Buffer, idx: usize, comptime T: type, arr: []align(1) const T) usize {
        return idx + arr.len * @sizeOf(T);
    }

    pub fn advanceExpr(_: *Buffer, idx: usize, comptime code: ExprCode) usize {
        return idx + 1 + 4 + 4 + @sizeOf(ExprData(code));
    }

    pub fn advanceStmt(_: *Buffer, idx: usize, comptime code: StmtCode) usize {
        return idx + 1 + 4 + 4 + @sizeOf(StmtData(code));
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

    pub fn pushEmptyExpr(self: *Buffer, comptime code: ExprCode, alloc: std.mem.Allocator, expr_t: ExprType, node_id: cy.NodeId) !u32 {
        log.tracev("irPushExpr: {}", .{code});
        const start = self.buf.items.len;
        try self.buf.resize(alloc, self.buf.items.len + 1 + 4 + 4 + @sizeOf(ExprData(code)));
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

    pub fn pushExpr(self: *Buffer, comptime code: ExprCode, alloc: std.mem.Allocator, type_id: cy.TypeId, node_id: cy.NodeId, data: ExprData(code)) !u32 {
        const expr_t = ExprType.init(type_id);
        const loc = try self.pushEmptyExpr(code, alloc, expr_t, node_id);
        self.setExprData(loc, code, data);
        return loc;
    }

    pub fn pushExprThrows(self: *Buffer, comptime code: ExprCode, alloc: std.mem.Allocator, type_id: cy.TypeId, node_id: cy.NodeId, data: ExprData(code)) !u32 {
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

    pub fn getNode(self: *Buffer, idx: usize) cy.NodeId {
        return @as(*align(1) cy.NodeId, @ptrCast(self.buf.items.ptr + idx + 1)).*;
    }

    pub fn setNode(self: *Buffer, idx: usize, nodeId: cy.NodeId) void {
        @as(*align(1) cy.NodeId, @ptrCast(self.buf.items.ptr + idx + 1)).* = nodeId;
    }

    pub fn setExprType2(self: *Buffer, loc: usize, expr_t: ExprType) void {
        @as(*align(1) ExprType, @ptrCast(self.buf.items.ptr + loc + 1 + 4)).* = expr_t;
    }

    pub fn setExprType(self: *Buffer, loc: usize, type_id: cy.TypeId) void {
        const expr_t = ExprType.init(type_id);
        @as(*align(1) ExprType, @ptrCast(self.buf.items.ptr + loc + 1 + 4)).* = expr_t;
    }

    pub fn setExprTypeThrows(self: *Buffer, loc: usize, type_id: cy.TypeId) void {
        const expr_t = ExprType.initThrows(type_id);
        @as(*align(1) ExprType, @ptrCast(self.buf.items.ptr + loc + 1 + 4)).* = expr_t;
    }

    pub fn getExprType(self: *Buffer, loc: usize) ExprType {
        return @as(*align(1) ExprType, @ptrCast(self.buf.items.ptr + loc + 1 + 4)).*;
    }

    pub fn setStmtNext(self: *Buffer, idx: usize, nextIdx: u32) void {
        @as(*align(1) u32, @ptrCast(self.buf.items.ptr + idx + 1 + 4)).* = nextIdx;
    }

    pub fn getStmtNext(self: *Buffer, idx: usize) u32 {
        return @as(*align(1) u32, @ptrCast(self.buf.items.ptr + idx + 1 + 4)).*;
    }

    pub fn pushEmptyStmt(self: *Buffer, alloc: std.mem.Allocator, comptime code: StmtCode, nodeId: cy.NodeId) !u32 {
        return self.pushEmptyStmt2(alloc, code, nodeId, true);
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

    pub fn pushEmptyStmt2(self: *Buffer, alloc: std.mem.Allocator, comptime code: StmtCode, nodeId: cy.NodeId, comptime appendToParent_: bool) !u32 {
        log.tracev("irPushStmt: {}", .{code});
        const start: u32 = @intCast(self.buf.items.len);
        try self.buf.resize(alloc, self.buf.items.len + 1 + 4 + 4 + @sizeOf(StmtData(code)));
        self.buf.items[start] = @intFromEnum(code);
        self.setNode(start, nodeId);
        self.setStmtNext(start, cy.NullId);

        if (appendToParent_) {
            self.appendToParent(start);
        }
        return @intCast(start);
    }

    pub fn pushStmt(self: *Buffer, alloc: std.mem.Allocator, comptime code: StmtCode, nodeId: cy.NodeId, data: StmtData(code)) !u32 {
        const idx = try self.pushEmptyStmt(alloc, code, nodeId);
        self.setStmtData(idx, code, data);
        return idx;
    }
};