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
/// The IR is tree structure but all the nodes are packed into a linear array.
/// IR nodes are indexed by their position in the array.
/// Stmt and expr nodes contain metadata which starts with their node type followed by their AST nodeId.
/// Stmt nodes additionally have a next index that points to the next stmt node.
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

    block,

    exprStmt,
    ifStmt,
    switchStmt,
    tryStmt,
    whileOptStmt,
    whileInfStmt,
    whileCondStmt,
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
    setField,
    setObjectField,
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

    field,
    fieldDynamic,
    fieldStatic,

    list,
    map,

    none,
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
    switchBlock,
    switchCase,

    /// Placeholder that is patched later to be `preCall`, `preBinOp`, `preUnOp`, etc.
    pre,
    preBinOp,
    preUnOp,
    preSlice,
    preCall,
    preCallObjSym,
    preCallObjSymUnOp,
    preCallObjSymBinOp,
    preCallFuncSym,

    andOp,
    orOp,

    tryExpr,

    blockExpr,
    mainEnd,
    elseBlock,
};

pub const Block = struct {
    bodyHead: Loc,
};

pub const BlockExpr = struct {
    bodyHead: Loc,
};

pub const Switch = struct {
    leftAssign: bool, 
    numCases: u8,
};

pub const SwitchCase = struct {
    // else case if `numConds` == 0.
    numConds: u8,
    bodyIsExpr: bool,
    bodyHead: u32,
};

pub const DestructureElems = struct {
    numLocals: u8,
    right: u32,
};

pub const WhileOptStmt = struct {
    someLocal: u8,
    capIdx: u32,
    bodyHead: u32,
};

pub const WhileCondStmt = struct {
    bodyHead: u32,
};

pub const WhileInfStmt = struct {
    bodyHead: u32,
};

pub const PushDebugLabel = struct {
    name: []const u8,
};

pub const Captured = struct {
    idx: u8,
};

pub const CondExpr = struct {
    body: u32,
    elseBody: u32,
};

pub const TryExpr = struct {
    catchBody: u32,
};

pub const Symbol = struct {
    name: []const u8,
};

pub const Error = struct {
    name: []const u8,
};

pub const Cast = struct {
    typeId: TypeId,
    isRtCast: bool,
};

pub const Field = union {
    dynamic: FieldDynamic,
    static: FieldStatic,
};

pub const FieldDynamic = struct {
    name: []const u8,
};

pub const FieldStatic = struct {
    typeId: TypeId,
    idx: u8,
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
    declHead: u32,
    bodyHead: u32,
};

pub const ForRangeStmt = struct {
    rangeEnd: u32,
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

pub const ElseBlock = struct {
    isElse: bool,
    bodyHead: u32,
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

pub const DeclareLocal = struct {
    namePtr: [*]const u8,
    nameLen: u16,
    declType: TypeId,
    id: u8,
    lifted: bool,
    assign: bool,

    pub fn name(self: DeclareLocal) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

/// Several pre codes share a union so that sema
/// can generate the IR in one pass by back-patching.
pub const Prepare = union {
    binOp: BinOp,
    slice: Slice,
    call: Call,
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
    left: u32,
    right: u32,
};

pub const BinOp = struct {
    leftT: TypeId,
    rightT: TypeId,
    op: cy.BinaryExprOp,
    right: u32,
};

pub const Set = union {
    index: SetIndex,
    generic: SetGeneric,
    callObjSymTern: SetCallObjSymTern,
};

pub const SetCallObjSymTern = struct {
    name: []const u8,
    funcSigId: sema.FuncSigId,
    index: u32,
    right: u32,
};

pub const SetGeneric = struct {
    leftT: CompactType,
    rightT: CompactType,
    right: u32,
};

pub const SetIndex = struct {
    recvT: TypeId,
    index: u32,
    right: u32,
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
    numArgs: u8,
    args: u32,
};

pub const CallObjSymBinOp = struct {
    op: cy.BinaryExprOp,
    funcSigId: sema.FuncSigId,
    right: u32,
};

pub const CallObjSymUnOp = struct {
    op: cy.UnaryOp,
    funcSigId: sema.FuncSigId,
};

pub const CallFuncSym = struct {
    func: *cy.Func,
    hasDynamicArg: bool,
    numArgs: u8,
    args: u32,
};

pub const Call = struct {
    numArgs: u8,
    args: u32,
};

pub const ExprStmt = struct {
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
    childT: TypeId,
    op: cy.UnaryOp,
};

pub const StmtBlock = struct {
    first: u32,
    last: u32,
};

pub const IfStmt = struct {
    numElseBlocks: u8,
    bodyHead: u32,
    elseBlocks: u32,
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
        .ifStmt => IfStmt,
        .tryStmt => TryStmt,
        .forIterStmt => ForIterStmt,
        .forRangeStmt => ForRangeStmt,
        .setLocalType => SetLocalType,
        .setIndex,
        .setCallObjSymTern,
        .setLocal,
        .setField,
        .setCaptured,
        .setObjectField,
        .setFuncSym,
        .setVarSym,
        .set => Set,
        .pushDebugLabel => PushDebugLabel,
        .whileOptStmt => WhileOptStmt,
        .whileCondStmt => WhileCondStmt,
        .verbose => Verbose,
        .whileInfStmt => WhileInfStmt,
        .destrElemsStmt => DestructureElems,
        .exprStmt => ExprStmt,
        .block => Block,
        else => void,
    };
}

pub fn ExprData(comptime code: ExprCode) type {
    return switch (code) {
        .lambda => Lambda,
        .switchBlock => Switch,
        .switchCase => SwitchCase,
        .elseBlock => ElseBlock,
        .preCall,
        .preCallFuncSym,
        .preCallObjSym,
        .preCallObjSymBinOp,
        .preSlice,
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
        .list => List,
        .map => Map,
        .array => Array,
        .string => String,
        .stringTemplate => StringTemplate,
        .objectInit => ObjectInit,
        .field,
        .fieldDynamic,
        .fieldStatic => Field,
        .cast => Cast,
        .errorv => Error,
        .captured => Captured,
        .symbol => Symbol,
        .blockExpr => BlockExpr,
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
        @memcpy(self.buf.items[idx+1+4..idx+1+4+bytes.len], &bytes);
    }

    pub fn getExprData(self: *Buffer, idx: usize, comptime code: ExprCode) ExprData(code) {
        const data = self.buf.items[idx+1+4..][0..@sizeOf(ExprData(code))];
        return std.mem.bytesToValue(ExprData(code), data);
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

    pub fn advanceArray(_: *Buffer, idx: usize, comptime T: type, arr: []align(1) const T) usize {
        return idx + arr.len * @sizeOf(T);
    }

    pub fn advanceExpr(_: *Buffer, idx: usize, comptime code: ExprCode) usize {
        return idx + 1 + 4 + @sizeOf(ExprData(code));
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

    pub fn popStmtBlock(self: *Buffer) StmtBlock {
        return self.stmtBlockStack.pop();
    }

    pub fn pushEmptyExpr(self: *Buffer, alloc: std.mem.Allocator, comptime code: ExprCode, nodeId: cy.NodeId) !u32 {
        log.tracev("irPushExpr: {}", .{code});
        const start = self.buf.items.len;
        try self.buf.resize(alloc, self.buf.items.len + 1 + 4 + @sizeOf(ExprData(code)));
        self.buf.items[start] = @intFromEnum(code);
        self.setNode(start, nodeId);
        return @intCast(start);
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

    pub fn pushExpr(self: *Buffer, alloc: std.mem.Allocator, comptime code: ExprCode, nodeId: cy.NodeId, data: ExprData(code)) !u32 {
        const idx = try self.pushEmptyExpr(alloc, code, nodeId);
        self.setExprData(idx, code, data);
        return idx;
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

    fn setNode(self: *Buffer, idx: usize, nodeId: cy.NodeId) void {
        @as(*align(1) cy.NodeId, @ptrCast(self.buf.items.ptr + idx + 1)).* = nodeId;
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