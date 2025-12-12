const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const C = cy.C;
const log = cy.log.scoped(.ast);

pub const NodeType = enum(u8) {
    // To allow non optional nodes.
    // Can be used to simplify code by accepting *Node only instead of ?*Node.
    null,

    accessExpr,
    all,
    as_expr,
    assign_stmt,
    at_lit,
    attribute,
    begin_stmt,
    binExpr,
    binLit,
    borrow,
    breakStmt,
    case_stmt,
    callExpr,
    catchStmt,
    const_decl,
    continueStmt,
    cstruct_decl,
    ct_stmt,
    cunion_decl,
    custom_type_decl,
    decLit,
    dec_u,
    deref,
    dollar,
    dollar_lit,
    dot,
    dot_lit,
    else_block,
    elseif_block,
    enumDecl,
    enumMember,
    error_lit,
    ex_borrow,
    expand_lit,
    exprStmt,
    falseLit,
    for_iter_stmt,
    for_range_stmt,
    floatLit,
    funcDecl,
    func_param,
    fn_type,
    fnsym_type,
    generic_expand,
    generic_vector_type,
    global_decl,
    group,
    hexLit,
    ident,
    if_expr,
    if_stmt,
    if_unwrap_stmt,
    impl_decl,
    import_stmt,
    index_expr,
    infer_param,
    init_expr,
    init_lit,
    keyValue,
    label_decl,
    lambda_cont_expr,
    lambda_cont,
    lambda_expr, 
    lambda_multi,
    move_expr,
    namedArg,
    noneLit,
    octLit,
    op_assign_stmt,
    option_type,
    partial_vector_type,
    passStmt,
    ptr,
    range,
    raw_string_lit,
    raw_string_multi_lit,
    ref,
    returnExprStmt,
    returnStmt,
    root,
    seqDestructure,
    slice_type,
    span_type,
    special_string_lit,
    sq_string_lit,
    sq_string_multi_lit,
    string_lit,
    string_multi_lit,
    stringt,
    stringt_multi,
    stringt_part,
    stringt_expr,
    struct_decl,
    struct_field,
    switchExpr,
    switch_stmt,
    template,
    trait_decl,
    trueLit,
    tryStmt,
    type_alias_decl,
    type_const_decl,
    unary_expr,
    undef_lit,
    union_case,
    unwrap,
    unwrap_choice,
    unwrap_or,
    unwrap_or_block,
    unwrap_res,
    unwrap_res_or,
    unwrap_res_or_block,
    use_alias,
    var_decl,
    vector_type,
    void_lit,
    whileCondStmt,
    whileInfStmt,
    while_unwrap_stmt,
    with,
    yield_stmt,
};

pub fn ConstSlice(T: type) type {
    return extern struct {
        ptr: [*]const T,
        len: usize,

        pub fn slice(self: *const @This()) []const T {
            return self.ptr[0..self.len];
        }
    };
}

pub fn Slice(T: type) type {
    return extern struct {
        ptr: [*]T,
        len: usize,

        pub fn slice(self: *@This()) []T {
            return self.ptr[0..self.len];
        }
    };
}

pub const AttributeType = enum(u8) {
    bind,
    extern_,
    call,
    generator,
    cond,
    unsafe,
    consteval,
    reserve,
};

const OptionType = extern struct {
    child: *Node,
    pos: u32,
};

const Borrow = extern struct {
    child: *Node,
    pos: u32,
};

const ExBorrow = extern struct {
    child: *Node,
    pos: u32,
};

const Ref = extern struct {
    child: *Node,
    pos: u32,
};

const Ptr = extern struct {
    child: *Node,
    pos: u32,
};

const ExprStmt = struct {
    child: *Node align(8),
};

const ReturnExprStmt = extern struct {
    child: *Node,
    pos: u32,
};

const MoveExpr = struct {
    expr: *Node align(8),
    pos: u32,
};

pub const ImportStmt = extern struct {
    attrs: Slice(*Attribute),
    name: *Node,
    spec: ?*Node,
    pos: u32,
};

pub const Ident = extern struct {
    src: u32 align(8),
    pos: u32,
    name: ConstSlice(u8),

    fn end(self: *Ident) u32 {
        return @intCast(self.pos + self.name.len);
    }
};

pub const PrefixLit = extern struct {
    name: *Node,
    pos: u32,
};

pub const SpecialStringKind = enum(u8) {
    c,
    ascii_cp,
    unicode_cp,
};

pub const SpecialStringLiteral = extern struct {
    kind: SpecialStringKind,
    lit: *Literal,
    pos: u32,

    pub fn asString(self: *@This()) []const u8 {
        var node: *Node = @ptrCast(self.lit);
        if (node.type() == .sq_string_multi_lit or node.type() == .string_multi_lit) {
            return self.lit.asStringMulti();
        } else {
            return self.lit.asString();
        }
    }
};

pub const Literal = extern struct {
    src: u32 align(8),
    pos: u32,

    // Contains the content including the prefix or delimiters.
    value: ConstSlice(u8),

    fn end(self: *Literal) u32 {
        return @intCast(self.pos + self.value.len);
    }

    pub fn as_dot_infer_name(self: *Literal) []const u8 {
        return self.value.ptr[1..self.value.len];
    }

    pub fn as_at_infer_name(self: *Literal) []const u8 {
        return self.value.ptr[1..self.value.len];
    }

    pub fn asRawString(self: *Literal) []const u8 {
        return self.value.ptr[1..self.value.len - 1];
    }

    pub fn asRawStringMulti(self: *Literal) []const u8 {
        return self.value.ptr[3..self.value.len - 3];
    }

    pub fn asString(self: *Literal) []const u8 {
        return self.value.ptr[1..self.value.len - 1];
    }

    pub fn asStringMulti(self: *Literal) []const u8 {
        return self.value.ptr[3..self.value.len - 3];
    }

    pub fn asSymbol(self: *Literal) []const u8 {
        return self.value.ptr[1..self.value.len];
    }

    pub fn asHex(self: *Literal) []const u8 {
        return self.value.ptr[2..self.value.len];
    }

    pub fn asBin(self: *Literal) []const u8 {
        return self.value.ptr[2..self.value.len];
    }

    pub fn asOct(self: *Literal) []const u8 {
        return self.value.ptr[2..self.value.len];
    }

    pub fn asDecU(self: *Literal) []const u8 {
        return self.value.ptr[0..self.value.len-1];
    }
};

const NamedArg = struct {
    name_pos: u32 align(8),
    name_len: u32,
    arg: *Node,
};

pub const TryStmt = struct {
    stmts: []*Node align(8),
    catchStmt: *CatchStmt,
    pos: u32,
};

const CatchStmt = struct {
    errorVar: ?*Node align(8),
    stmts: []*Node,
    src: u32,
    pos: u32,
};

const AsExpr = extern struct {
    expr: *Node,
    target: ?*Node,
    pos: u32,
};

const AssignIndexStmt = struct {
    left: *IndexExpr align(8),
    right: *Node,
};

const AssignStmt = struct {
    left: *Node align(8),
    right: *Node,
};

pub const BinExpr = extern struct {
    left: *Node,
    right: *Node,
    op: BinaryExprOp,
    op_pos: u32,
};

const OpAssignStmt = struct {
    left: *Node align(8),
    right: *Node,
    op: BinaryExprOp,
    assign_pos: u32,
};

pub const CaseKind = enum(u8) {
    case,
    else_,
};

pub const CaseBodyKind = enum(u8) {
    block,
    expr,
    fallthrough,
};

pub const CaseStmt = extern struct {
    kind: CaseKind,
    data: extern union {
        case: extern struct {
            conds: Slice(*Node),
            capture: ?*Node,
        },
        else_: extern struct {
            src: u32,
        },
    },
    body_data: extern union {
        block: Slice(*Node),
        expr: *Node,
    },
    body_kind: CaseBodyKind,
    pos: u32,
};

pub const SwitchBlock = extern struct {
    expr: *Node,

    // CaseStmt, CtStmt/CaseStmt, or CtStmt/ForStmt
    cases: Slice(*Node),
    pos: u32,
};

pub const Attribute = extern struct {
    type: AttributeType align(8),
    value: ?*Node,
    src: u32,
    pos: u32,

    pub fn getString(self: *Attribute) !?[]const u8 {
        const value = self.value orelse {
            return null;
        };
        if (value.type() != .sq_string_lit) {
            return error.Unsupported;
        }
        return value.cast(.sq_string_lit).asString();
    }
};

const Group = extern struct {
    child: *Node,
    pos: u32,
    end: u32,
};

pub const BeginStmt = struct {
    stmts: []const *Node,
    pos: u32,
};

pub const IfStmt = struct {
    cond: *Node align(8),
    stmts: []const *Node,
    else_blocks: []*Node,
    pos: u32,
};

pub const ElseIfBlock = struct {
    cond: *Node align(8),
    stmts: []const *Node,
    pos: u32,
};

pub const ElseBlock = struct {
    stmts: []const *Node,
    src: u32,
    pos: u32,
};

pub const IfUnwrapStmt = struct {
    opt: *Node align(8),
    unwrap: *Node,
    stmts: []const *Node,
    else_blocks: []*Node,
    pos: u32,
};

pub const AccessExpr = extern struct {
    left: *Node,
    right: *Node,
};

const UnwrapChoice = struct {
    left: *Node align(8),
    right: *Node,
};

const DerefExpr = struct {
    left: *Node align(8),
    end: u32,
};

const Unwrap = struct {
    opt: *Node align(8),
    end: u32,
};

pub const UnwrapOr = struct {
    opt: *Node align(8),
    default: *Node,
};

pub const UnwrapOrBlock = struct {
    opt: *Node align(8),
    else_stmts: []const *Node,
};

const UnwrapRes = struct {
    res: *Node align(8),
    end: u32,
};

pub const UnwrapResOr = struct {
    res: *Node align(8),
    default: *Node,
};

pub const UnwrapResOrBlock = struct {
    res: *Node align(8),
    capture: ?*Node,
    else_stmts: []const *Node,
};

pub const CallExpr = extern struct {
    callee: *Node,
    args: Slice(*Node),
    hasNamedArg: bool,
    end: u32,
};

const GenericExpand = extern struct {
    left: *Node,
    end: u32,
};

const ExpandLit = struct {
    child: *Node align(8),
    pos: u32,
    end: u32,
};

pub const InitExpr = struct {
    left: *Node align(8),
    lit: *InitLit,
};

pub const InitLit = extern struct {
    // KeyValue or Expr.
    args: Slice(*Node),
    src: u32,
    pos: u32,
    end: u32,

    /// This only reflects the literal by itself.
    /// true when args is empty.
    array_like: bool,
};

pub const IndexExpr = extern struct {
    left: *Node,
    args: Slice(*Node),
    end: u32,
};

pub const GenericVectorType = extern struct {
    elem_t: *Node,
    pos: u32,
};

pub const VectorType = extern struct {
    child: *Node,
    n: *Node,
    pos: u32,
};

pub const SpanType = extern struct {
    child: *Node,
    pos: u32,
};

pub const SliceType = extern struct {
    child: *Node,
    pos: u32,
};

pub const Unary = extern struct {
    child: *Node,
    op: UnaryOp,
    pos: u32,
};

pub const Root = extern struct {
    stmts: Slice(*Node),
    src: u32,
};

pub const KeyValue = struct {
    key: *Node align(8),
    value: *Node,
};

pub const ComptimeStmt = struct {
    child: *Node align(8),
    pos: u32,
};

pub const ComptimeExpr = struct {
    child: *Node align(8),
};

pub const FuncType = extern struct {
    params: ConstSlice(*FuncParam),
    ret: ?*Node,
    src: u32,
    pos: u32,
    extern_: ?*Attribute = null,
};

pub const LambdaContExpr = struct {
    // Child expr.
    child: *Node align(8),

    stmts: []*Node,
};

pub const LambdaCont = struct {
    params: []const *FuncParam align(8),

    stmts: []const *Node,
    sig_t: FuncSigType,
    ret: ?*Node,
    src: u32,
    pos: u32,
    end: u32,
};

pub const LambdaExpr = struct {
    params: []const *FuncParam align(8),
    expr: *Node,
    sig_t: FuncSigType,
    ret: ?*Node,
    src: u32,
    pos: u32,
};

pub const LambdaMulti = struct {
    params: []const *FuncParam align(8),

    // Initially empty for `lambda_cont`, but set during sema from a LambdaContExpr.
    stmts: []const *Node,
    
    sig_t: FuncSigType,
    ret: ?*Node,
    src: u32,
    pos: u32,
    sig_end: u32,
};

pub const With = extern struct {
    params: ConstSlice(*FuncParam),
    src: u32,
    pos: u32,
    end: u32,
};

pub const FuncDecl = extern struct {
    name: *Node,

    // If method, `parent` refers the receiver type.
    parent: ?*Node,

    attrs: Slice(*Attribute),
    with: ?*With,
    params: ConstSlice(*FuncParam),
    ret: ?*Node,
    scope_ret: bool,
    hidden: bool,
    stmts: Slice(*Node),
    sig_t: FuncSigType,
    pos: u32,
};

// TODO: Consider splitting into FuncParam and TemplateParam.
pub const FuncParam = extern struct {
    name_type: *Node,
    type: ?*Node,

    template_param: bool,
    scope_param: bool,
    sink_param: bool,

    // For template param only.
    const_param: bool,
    pos: u32,
};

pub const YieldStmt = struct {
    child: ?*Node align(8),
    src: u32,
    pos: u32,
};

pub const TypeConstDecl = extern struct {
    name: *Node,

    hidden: bool,
    attrs: Slice(*Attribute),

    stmts: Slice(*Node),
    pos: u32,
};

pub const TypeAliasDecl = extern struct {
    name: *Node,
    parent: ?*Node,

    hidden: bool,
    attrs: Slice(*Attribute),

    /// Host defined target if `underscore`.
    target: *Node,

    pos: u32,
};

pub const UseAlias = struct {
    name: *Node align(8),

    hidden: bool,
    attrs: []*Attribute,
    target: *Node,

    pos: u32,
};

pub const CustomTypeDecl = extern struct {
    name: *Node,
    attrs: Slice(*Attribute),
    hidden: bool,
    pos: u32,
};

pub const Field = struct {
    name: *Node align(8),
    typeSpec: ?*Node,
    init: ?*Node,
    hidden: bool,
    embedded: bool,
};

pub const TraitDecl = extern struct {
    name: *Node,
    attrs: Slice(*Attribute),
    funcs: Slice(*FuncDecl),
    pos: u32,
};

pub const ImplDecl = extern struct {
    trait: *Node,
    pos: u32,
};

pub const UnionCase = extern struct {
    name: *Node,
    payload_t: *Node,
    pos: u32,
};

pub const CUnionDecl = extern struct {
    name: *Node,
    attrs: Slice(*Attribute),
    impls: Slice(*ImplDecl),
    cases: Slice(*UnionCase),
    pos: u32,
};

pub const StructDecl = extern struct {
    name: *Node,
    attrs: Slice(*Attribute),
    impls: Slice(*ImplDecl),
    fields: Slice(*Field),
    num_embedded_fields: u32,
    is_tuple: bool,
    pos: u32,
};

pub const ConstDecl = extern struct {
    name: *Node,
    parent: ?*Node,
    attrs: Slice(*Attribute),
    type: ?*Node,
    right: ?*Node,
    hidden: bool,
    pos: u32,
};

pub const GlobalDecl = extern struct {
    name: *Node align(8),
    parent: ?*Node,
    attrs: Slice(*Attribute),
    typeSpec: *Node,
    right: ?*Node,
    hidden: bool,
    pos: u32,
};

pub const VaList = struct {
    elem: ?*Node align(8),
    src: u32,
    pos: u32,
};

pub const VarDecl = extern struct {
    name: *Node,
    typeSpec: ?*Node,
    right: *Node,
    pos: u32,
};

pub const EnumMember = extern struct {
    name: *Node,
    typeSpec: ?*Node,
    pos: u32,
};

pub const EnumDecl = extern struct {
    name: *Node,
    members: Slice(*EnumMember),
    isChoiceType: bool,
    hidden: bool,
    pos: u32,
};

const WhileInfStmt = struct {
    stmts: []*Node align(8),
    src: u32,
    pos: u32,
};

const WhileCondStmt = struct {
    cond: *Node align(8),
    stmts: []const *Node,
    pos: u32,
};

pub const WhileUnwrapStmt = struct {
    opt: *Node align(8),
    capture: *Node,
    stmts: []const *Node,
    pos: u32,
};

pub const ForRangeStmt = struct {
    start: *Node align(8),
    end: *Node,
    each: ?*Node,
    increment: bool,
    end_inclusive: bool,
    stmts: []*Node,
    pos: u32,
};

pub const ForIterStmt = struct {
    iterable: *Node align(8),
    each: ?*Node,
    count: ?*Node,
    stmts: []const *Node,
    pos: u32,
};

pub const SeqDestructure = struct {
    args: []*Node align(8),
    pos: u32,
    end: u32,
};

pub const TemplateDecl = struct {
    params: []*FuncParam,
    child_decl: *Node,

    pub fn getAttrs(self: *TemplateDecl) []const *Attribute {
        switch (self.child_decl.type()) {
            .funcDecl => {
                return self.child_decl.cast(.funcDecl).attrs;
            },
            else => {
                std.debug.panic("TODO: {}", .{self.child_decl.type()});
            }
        }
    }
};

pub const Range = extern struct {
    start: ?*Node,
    end: ?*Node,
    inc: bool,
    end_inclusive: bool,
    src: u32,
    pos: u32,
    op_end: u32,
};

const IfExpr = extern struct {
    cond: *Node,
    body: *Node,
    else_expr: *Node,
    pos: u32,
};

pub const StringTemplate = struct {
    // Begins and ends with stringt_part and alternates between stringt_expr.
    parts: []*Node align(8),

    pos: u32,
};

const StringTemplateExpr = struct {
    child: *Node align(8),
    pos: u32,
};

pub const FuncSigType = enum(u8) {
    func,
    infer,
    method,
};

pub const Token = struct {
    src: u32 align(8),
    pos: u32,
};

fn NodeData(comptime node_t: NodeType) type {
    return switch (node_t) {
        .null           => Node,
        .accessExpr     => AccessExpr,
        .all            => Token,
        .as_expr        => AsExpr,
        .assign_stmt    => AssignStmt,
        .at_lit         => Literal,
        .attribute      => Attribute,
        .begin_stmt    => BeginStmt,
        .binExpr        => BinExpr,
        .binLit         => Literal,
        .borrow         => Borrow,
        .breakStmt      => Token,
        .case_stmt      => CaseStmt,
        .callExpr       => CallExpr,
        .catchStmt      => CatchStmt,
        .const_decl     => ConstDecl,
        .continueStmt   => Token,
        .cstruct_decl   => StructDecl,
        .ct_stmt        => ComptimeStmt,
        .cunion_decl    => CUnionDecl,
        .custom_type_decl => CustomTypeDecl,
        .decLit         => Literal,
        .dec_u          => Literal,
        .deref          => DerefExpr,
        .dollar         => Token,
        .dollar_lit     => PrefixLit,
        .dot            => Token,
        .dot_lit        => Literal,
        .else_block     => ElseBlock,
        .elseif_block   => ElseIfBlock,
        .enumDecl       => EnumDecl,
        .enumMember     => EnumMember,
        .error_lit      => PrefixLit,
        .ex_borrow      => ExBorrow,
        .expand_lit     => ExpandLit,
        .exprStmt       => ExprStmt,
        .falseLit       => Token,
        .for_iter_stmt  => ForIterStmt,
        .for_range_stmt => ForRangeStmt,
        .floatLit       => Literal,
        .funcDecl       => FuncDecl,
        .func_param     => FuncParam,
        .fn_type        => FuncType,
        .fnsym_type     => FuncType,
        .generic_expand => GenericExpand,
        .generic_vector_type => GenericVectorType,
        .global_decl    => GlobalDecl,
        .group          => Group,
        .hexLit         => Literal,
        .ident          => Ident,
        .if_expr        => IfExpr,
        .if_stmt        => IfStmt,
        .if_unwrap_stmt => IfUnwrapStmt,
        .impl_decl      => ImplDecl,
        .import_stmt    => ImportStmt,
        .index_expr     => IndexExpr,
        .infer_param    => PrefixLit,
        .init_expr      => InitExpr,
        .init_lit       => InitLit,
        .keyValue       => KeyValue,
        .label_decl     => void,
        .lambda_cont_expr => LambdaContExpr,
        .lambda_cont    => LambdaMulti,
        .lambda_expr    => LambdaExpr,
        .lambda_multi   => LambdaMulti,
        .move_expr      => MoveExpr,
        .namedArg       => NamedArg,
        .noneLit        => Token,
        .octLit         => Literal,
        .op_assign_stmt => OpAssignStmt,
        .option_type    => OptionType,
        .partial_vector_type => VectorType,
        .passStmt       => Token,
        .ptr            => Ptr,
        .raw_string_lit => Literal,
        .raw_string_multi_lit => Literal,
        .range          => Range,
        .slice_type     => SliceType,
        .span_type      => SpanType,
        .sq_string_lit  => Literal,
        .sq_string_multi_lit => Literal,
        .ref            => Ref,
        .returnExprStmt => ReturnExprStmt,
        .returnStmt     => Token,
        .root           => Root,
        .seqDestructure => SeqDestructure,
        .special_string_lit => SpecialStringLiteral,
        .string_lit     => Literal,
        .string_multi_lit => Literal,
        .stringt        => StringTemplate,
        .stringt_multi  => StringTemplate,
        .stringt_part   => Literal,
        .stringt_expr   => StringTemplateExpr,
        .struct_decl    => StructDecl,
        .struct_field   => Field,
        .switchExpr     => SwitchBlock,
        .switch_stmt    => SwitchBlock,
        .template       => TemplateDecl,
        .trait_decl     => TraitDecl,
        .trueLit        => Token,
        .tryStmt        => TryStmt,
        .type_alias_decl => TypeAliasDecl,
        .type_const_decl => TypeConstDecl,
        .unary_expr     => Unary,
        .undef_lit      => Token,
        .union_case     => UnionCase,
        .unwrap         => Unwrap,
        .unwrap_choice  => UnwrapChoice,
        .unwrap_or      => UnwrapOr,
        .unwrap_or_block => UnwrapOrBlock,
        .unwrap_res     => UnwrapRes,
        .unwrap_res_or  => UnwrapResOr,
        .unwrap_res_or_block => UnwrapResOrBlock,
        .use_alias      => UseAlias,
        .var_decl       => VarDecl,
        .vector_type    => VectorType,
        .void_lit       => Token,
        .whileCondStmt  => WhileCondStmt,
        .whileInfStmt   => WhileInfStmt,
        .while_unwrap_stmt => WhileUnwrapStmt,
        .with           => With,
        .yield_stmt     => YieldStmt,
    };
}

const NodeHeader = extern struct {
    type: NodeType,
};

/// Each Node's position includes is between the source's start position and end position.
pub const Node = struct {
    dummy: u8 = undefined,

    pub fn @"type"(self: *Node) NodeType {
        return @as(*NodeHeader, @ptrFromInt(@intFromPtr(self) - 1)).*.type;
    }

    pub fn setType(self: *Node, node_t: NodeType) void {
        @as(*NodeHeader, @ptrFromInt(@intFromPtr(self) - 1)).*.type = node_t;
    }

    pub fn from_c(node: *C.Node) *Node {
        return @ptrCast(node);
    }

    pub fn to_c(self: *Node) *C.Node {
        return @ptrCast(self);
    }

    pub fn cast(self: *Node, comptime node_t: NodeType) *NodeData(node_t) {
        if (cy.Trace) {
            if (self.type() != node_t) {
                std.debug.panic("Expected {}, found {}.", .{node_t, self.type()});
            }
        }
        return @ptrCast(@alignCast(self));
    }

    pub fn declName(self: *Node) []const u8 {
        switch (self.type()) {
            .raw_string_lit => return self.cast(.raw_string_lit).asRawString(),
            .at_lit => return self.cast(.at_lit).value.slice(),
            .ident => return self.cast(.ident).name.slice(),
            else => std.debug.panic("Expected declaration name. {}", .{self.type()}),
        }
    }

    pub fn getNamePathBase(self: *Node) NamePathBase {
        if (self.type() != .accessExpr) {
            return .{
                .name = self.declName(),
                .node = self,
            };
        } else {
            const base = self.cast(.accessExpr).right;
            return .{
                .name = base.declName(),
                .node = base,
            };
        }
    }

    pub fn name(self: *Node) []const u8 {
        return self.nameOrNull() orelse {
            std.debug.panic("Expected name. {}", .{self.type()});
        };
    }

    pub fn nameOrNull(self: *Node) ?[]const u8 {
        switch (self.type()) {
            .sq_string_lit => return self.cast(.sq_string_lit).asString(),
            .at_lit => return self.cast(.at_lit).value.slice(),
            .ident => return self.cast(.ident).name.slice(),
            else => return null,
        }
    }

    /// Source id.
    pub fn src(self: *Node) u32 {
        return switch (self.type()) {
            .null           => cy.NullId,
            .all            => self.cast(.all).src,
            .accessExpr     => self.cast(.accessExpr).left.src(),
            .as_expr        => self.cast(.as_expr).expr.src(),
            .assign_stmt    => self.cast(.assign_stmt).left.src(),
            .at_lit         => self.cast(.at_lit).src,
            .attribute      => self.cast(.attribute).src,
            .begin_stmt    => self.cast(.begin_stmt).stmts[0].src(),
            .binExpr        => self.cast(.binExpr).left.src(),
            .binLit         => self.cast(.binLit).src,
            .borrow         => self.cast(.borrow).child.src(),
            .breakStmt      => self.cast(.breakStmt).src,
            .callExpr       => self.cast(.callExpr).callee.src(),
            .case_stmt      => {
                const case_stmt = self.cast(.case_stmt);
                switch (case_stmt.kind) {
                    .case => return case_stmt.data.case.conds.ptr[0].src(),
                    .else_ => return case_stmt.data.else_.src,
                }
            },
            .catchStmt      => self.cast(.catchStmt).src,
            .const_decl     => self.cast(.const_decl).name.src(),
            .continueStmt   => self.cast(.continueStmt).src,
            .cstruct_decl   => self.cast(.cstruct_decl).name.src(),
            .ct_stmt        => self.cast(.ct_stmt).child.src(),
            .cunion_decl    => self.cast(.cunion_decl).name.src(),
            .custom_type_decl => self.cast(.custom_type_decl).name.src(),
            .decLit         => self.cast(.decLit).src,
            .dec_u          => self.cast(.dec_u).src,
            .deref          => self.cast(.deref).left.src(),
            .dollar         => self.cast(.dollar).src,
            .dollar_lit     => self.cast(.dollar_lit).name.src(),
            .dot            => self.cast(.dot).src,
            .dot_lit        => self.cast(.dot_lit).src,
            .else_block     => self.cast(.else_block).src,
            .elseif_block   => self.cast(.elseif_block).cond.src(),
            .enumDecl       => self.cast(.enumDecl).name.src(),
            .enumMember     => self.cast(.enumMember).name.src(),
            .error_lit      => self.cast(.error_lit).name.src(),
            .ex_borrow      => self.cast(.ex_borrow).child.src(),
            .expand_lit     => self.cast(.expand_lit).child.src(),
            .exprStmt       => self.cast(.exprStmt).child.src(),
            .falseLit       => self.cast(.falseLit).src,
            .floatLit       => self.cast(.floatLit).src,
            .for_iter_stmt  => self.cast(.for_iter_stmt).iterable.src(),
            .for_range_stmt => self.cast(.for_range_stmt).start.src(),
            .funcDecl       => self.cast(.funcDecl).name.src(),
            .func_param     => self.cast(.func_param).name_type.src(),
            .fn_type        => self.cast(.fn_type).src,
            .fnsym_type     => self.cast(.fnsym_type).src,
            .generic_expand => self.cast(.generic_expand).left.src(),
            .generic_vector_type => self.cast(.generic_vector_type).elem_t.src(),
            .global_decl    => self.cast(.global_decl).name.src(),
            .group          => self.cast(.group).child.src(),
            .hexLit         => self.cast(.hexLit).src,
            .ident          => self.cast(.ident).src,
            .if_expr        => self.cast(.if_expr).cond.src(),
            .if_stmt        => self.cast(.if_stmt).cond.src(),
            .if_unwrap_stmt => self.cast(.if_unwrap_stmt).opt.src(),
            .init_expr      => self.cast(.init_expr).left.src(),
            .init_lit       => self.cast(.init_lit).src,
            .keyValue       => self.cast(.keyValue).key.src(),
            .impl_decl      => self.cast(.impl_decl).trait.src(),
            .import_stmt    => self.cast(.import_stmt).name.src(),
            .index_expr     => self.cast(.index_expr).left.src(),
            .infer_param    => self.cast(.infer_param).name.src(),
            .label_decl     => cy.NullId,
            .lambda_cont_expr => self.cast(.lambda_cont_expr).child.src(),
            .lambda_cont    => self.cast(.lambda_cont).src,
            .lambda_expr    => self.cast(.lambda_expr).src,
            .lambda_multi   => self.cast(.lambda_multi).src,
            .move_expr      => self.cast(.move_expr).expr.src(),
            .namedArg       => self.cast(.namedArg).arg.src(),
            .noneLit        => self.cast(.noneLit).src,
            .octLit         => self.cast(.octLit).src,
            .op_assign_stmt => self.cast(.op_assign_stmt).left.src(),
            .option_type    => self.cast(.option_type).child.src(),
            .partial_vector_type => self.cast(.partial_vector_type).n.src(),
            .passStmt       => self.cast(.passStmt).src,
            .ptr            => self.cast(.ptr).child.src(),
            .range          => self.cast(.range).src,
            .raw_string_lit => self.cast(.raw_string_lit).src,
            .raw_string_multi_lit => self.cast(.raw_string_multi_lit).src,
            .ref            => self.cast(.ref).child.src(),
            .returnExprStmt => self.cast(.returnExprStmt).child.src(),
            .returnStmt     => self.cast(.returnStmt).src,
            .root           => self.cast(.root).src,
            .seqDestructure => self.cast(.seqDestructure).args[0].src(),
            .slice_type     => self.cast(.slice_type).child.src(),
            .span_type      => self.cast(.span_type).child.src(),
            .special_string_lit => @as(*Node, @ptrCast(self.cast(.special_string_lit).lit)).src(),
            .sq_string_lit => self.cast(.sq_string_lit).src,
            .sq_string_multi_lit => self.cast(.sq_string_multi_lit).src,
            .string_lit     => self.cast(.string_lit).src,
            .string_multi_lit => self.cast(.string_multi_lit).src,
            .stringt        => self.cast(.stringt).parts[0].src(),
            .stringt_multi  => self.cast(.stringt_multi).parts[0].src(),
            .stringt_part   => self.cast(.stringt_part).src,
            .stringt_expr   => self.cast(.stringt_expr).child.src(),
            .struct_decl    => self.cast(.struct_decl).name.src(),
            .struct_field   => self.cast(.struct_field).name.src(),
            .switchExpr     => self.cast(.switchExpr).expr.src(),
            .switch_stmt    => self.cast(.switch_stmt).expr.src(),
            .template       => self.cast(.template).child_decl.src(),
            .trait_decl     => self.cast(.trait_decl).name.src(),
            .trueLit        => self.cast(.trueLit).src,
            .tryStmt        => self.cast(.tryStmt).catchStmt.src,
            .type_alias_decl => self.cast(.type_alias_decl).name.src(),
            .type_const_decl => self.cast(.type_const_decl).name.src(),
            .unary_expr     => self.cast(.unary_expr).child.src(),
            .undef_lit      => self.cast(.undef_lit).src,
            .union_case     => self.cast(.union_case).name.src(),
            .unwrap         => self.cast(.unwrap).opt.src(),
            .unwrap_choice  => self.cast(.unwrap_choice).left.src(),
            .unwrap_or      => self.cast(.unwrap_or).opt.src(),
            .unwrap_or_block => self.cast(.unwrap_or_block).opt.src(),
            .unwrap_res     => self.cast(.unwrap_res).res.src(),
            .unwrap_res_or  => self.cast(.unwrap_res_or).res.src(),
            .unwrap_res_or_block => self.cast(.unwrap_res_or_block).res.src(),
            .use_alias      => self.cast(.use_alias).name.src(),
            .var_decl       => self.cast(.var_decl).name.src(),
            .vector_type    => self.cast(.vector_type).n.src(),
            .void_lit       => self.cast(.void_lit).src,
            .whileInfStmt   => self.cast(.whileInfStmt).src,
            .whileCondStmt  => self.cast(.whileCondStmt).cond.src(),
            .while_unwrap_stmt => self.cast(.while_unwrap_stmt).opt.src(),
            .with           => self.cast(.with).src,
            .yield_stmt     => self.cast(.yield_stmt).src,
        };
    }

    pub fn pos(self: *Node) u32 {
        return switch (self.type()) {
            .null           => cy.NullId,
            .all            => self.cast(.all).pos,
            .accessExpr     => self.cast(.accessExpr).left.pos(),
            .as_expr        => self.cast(.as_expr).pos,
            .assign_stmt    => self.cast(.assign_stmt).left.pos(),
            .at_lit         => self.cast(.at_lit).pos,
            .attribute      => self.cast(.attribute).pos,
            .begin_stmt    => self.cast(.begin_stmt).pos,
            .binExpr        => self.cast(.binExpr).left.pos(),
            .binLit         => self.cast(.binLit).pos,
            .borrow         => self.cast(.borrow).pos,
            .breakStmt      => self.cast(.breakStmt).pos,
            .callExpr       => self.cast(.callExpr).callee.pos(),
            .case_stmt      => self.cast(.case_stmt).pos,
            .catchStmt      => self.cast(.catchStmt).pos,
            .const_decl     => self.cast(.const_decl).pos,
            .continueStmt   => self.cast(.continueStmt).pos,
            .cstruct_decl   => self.cast(.cstruct_decl).pos,
            .ct_stmt        => self.cast(.ct_stmt).pos,
            .cunion_decl    => self.cast(.cunion_decl).pos,
            .custom_type_decl => self.cast(.custom_type_decl).pos,
            .decLit         => self.cast(.decLit).pos,
            .dec_u          => self.cast(.dec_u).pos,
            .deref          => self.cast(.deref).left.pos(),
            .dollar         => self.cast(.dollar).pos,
            .dollar_lit     => self.cast(.dollar_lit).pos,
            .dot            => self.cast(.dot).pos,
            .dot_lit        => self.cast(.dot_lit).pos,
            .else_block     => self.cast(.else_block).pos,
            .elseif_block   => self.cast(.elseif_block).pos,
            .enumDecl       => self.cast(.enumDecl).pos,
            .enumMember     => self.cast(.enumMember).name.pos(),
            .error_lit      => self.cast(.error_lit).pos,
            .ex_borrow      => self.cast(.ex_borrow).pos,
            .expand_lit     => self.cast(.expand_lit).pos,
            .exprStmt       => self.cast(.exprStmt).child.pos(),
            .falseLit       => self.cast(.falseLit).pos,
            .floatLit       => self.cast(.floatLit).pos,
            .for_iter_stmt  => self.cast(.for_iter_stmt).pos,
            .for_range_stmt => self.cast(.for_range_stmt).pos,
            .funcDecl       => self.cast(.funcDecl).pos,
            .func_param     => self.cast(.func_param).pos,
            .fn_type        => self.cast(.fn_type).pos,
            .fnsym_type     => self.cast(.fnsym_type).pos,
            .generic_expand => self.cast(.generic_expand).left.pos(),
            .generic_vector_type => self.cast(.generic_vector_type).pos,
            .global_decl    => self.cast(.global_decl).pos,
            .group          => self.cast(.group).pos,
            .hexLit         => self.cast(.hexLit).pos,
            .ident          => self.cast(.ident).pos,
            .if_expr        => self.cast(.if_expr).pos,
            .if_stmt        => self.cast(.if_stmt).pos,
            .if_unwrap_stmt => self.cast(.if_unwrap_stmt).pos,
            .index_expr     => self.cast(.index_expr).left.pos(),
            .init_expr      => self.cast(.init_expr).left.pos(),
            .init_lit       => self.cast(.init_lit).pos,
            .keyValue       => self.cast(.keyValue).key.pos(),
            .impl_decl      => self.cast(.impl_decl).pos,
            .import_stmt    => self.cast(.import_stmt).pos,
            .infer_param    => self.cast(.infer_param).pos,
            .label_decl     => cy.NullId,
            .lambda_cont_expr => self.cast(.lambda_cont_expr).child.pos(),
            .lambda_cont    => self.cast(.lambda_cont).pos,
            .lambda_expr    => self.cast(.lambda_expr).pos,
            .lambda_multi   => self.cast(.lambda_multi).pos,
            .move_expr      => self.cast(.move_expr).pos,
            .namedArg       => self.cast(.namedArg).name_pos,
            .noneLit        => self.cast(.noneLit).pos,
            .octLit         => self.cast(.octLit).pos,
            .op_assign_stmt => self.cast(.op_assign_stmt).left.pos(),
            .option_type    => self.cast(.option_type).pos,
            .partial_vector_type => self.cast(.partial_vector_type).pos,
            .passStmt       => self.cast(.passStmt).pos,
            .ptr            => self.cast(.ptr).pos,
            .range          => self.cast(.range).pos,
            .raw_string_lit => self.cast(.raw_string_lit).pos,
            .raw_string_multi_lit => self.cast(.raw_string_multi_lit).pos,
            .ref            => self.cast(.ref).pos,
            .returnExprStmt => self.cast(.returnExprStmt).pos,
            .returnStmt     => self.cast(.returnStmt).pos,
            .root           => 0,
            .seqDestructure => self.cast(.seqDestructure).pos,
            .slice_type     => self.cast(.slice_type).pos,
            .span_type      => self.cast(.span_type).pos,
            .special_string_lit => self.cast(.special_string_lit).pos,
            .sq_string_lit => self.cast(.sq_string_lit).pos,
            .sq_string_multi_lit => self.cast(.sq_string_multi_lit).pos,
            .string_lit     => self.cast(.string_lit).pos,
            .string_multi_lit => self.cast(.string_multi_lit).pos,
            .stringt        => self.cast(.stringt).pos,
            .stringt_multi  => self.cast(.stringt_multi).pos,
            .stringt_part   => self.cast(.stringt_part).pos,
            .stringt_expr   => self.cast(.stringt_expr).pos,
            .struct_decl    => self.cast(.struct_decl).pos,
            .struct_field   => self.cast(.struct_field).name.pos(),
            .switchExpr     => self.cast(.switchExpr).pos,
            .switch_stmt    => self.cast(.switch_stmt).pos,
            .template       => self.cast(.template).child_decl.pos(),
            .trait_decl     => self.cast(.trait_decl).pos,
            .trueLit        => self.cast(.trueLit).pos,
            .tryStmt        => self.cast(.tryStmt).pos,
            .type_alias_decl => self.cast(.type_alias_decl).pos,
            .type_const_decl => self.cast(.type_const_decl).pos,
            .unary_expr     => self.cast(.unary_expr).pos,
            .undef_lit      => self.cast(.undef_lit).pos,
            .union_case     => self.cast(.union_case).pos,
            .unwrap         => self.cast(.unwrap).opt.pos(),
            .unwrap_choice  => self.cast(.unwrap_choice).left.pos(),
            .unwrap_or      => self.cast(.unwrap_or).opt.pos(),
            .unwrap_or_block => self.cast(.unwrap_or_block).opt.pos(),
            .unwrap_res     => self.cast(.unwrap_res).res.pos(),
            .unwrap_res_or  => self.cast(.unwrap_res_or).res.pos(),
            .unwrap_res_or_block => self.cast(.unwrap_res_or_block).res.pos(),
            .use_alias      => self.cast(.use_alias).pos,
            .var_decl       => self.cast(.var_decl).pos,
            .vector_type    => self.cast(.vector_type).pos,
            .void_lit       => self.cast(.void_lit).pos,
            .whileInfStmt   => self.cast(.whileInfStmt).pos,
            .whileCondStmt  => self.cast(.whileCondStmt).pos,
            .while_unwrap_stmt => self.cast(.while_unwrap_stmt).pos,
            .with           => self.cast(.with).pos,
            .yield_stmt     => self.cast(.yield_stmt).pos,
        };
    }

    pub fn end(self: *Node) u32 {
        return switch (self.type()) {
            .null           => cy.NullId,
            .all            => self.cast(.all).pos + 1,
            .accessExpr     => self.cast(.accessExpr).right.end(),
            .as_expr        => self.cast(.as_expr).expr.end(),
            .assign_stmt    => self.cast(.assign_stmt).right.end(),
            .at_lit         => self.cast(.at_lit).end(),
            .attribute      => {
                const attr = self.cast(.attribute);
                if (attr.value) |value| {
                    return value.end();
                }
                return attr.pos + 1;
            },
            .begin_stmt    => {
                const stmts = self.cast(.begin_stmt).stmts;
                return stmts[stmts.len-1].end();
            },
            .binExpr        => self.cast(.binExpr).right.end(),
            .binLit         => self.cast(.binLit).end(),
            .borrow         => self.cast(.borrow).child.end(),
            .ex_borrow      => self.cast(.ex_borrow).child.end(),
            .breakStmt      => self.cast(.breakStmt).pos + 5,
            .callExpr       => self.cast(.callExpr).end,
            .case_stmt      => {
                const case = self.cast(.case_stmt);
                switch (case.body_kind) {
                    .expr => {
                        return case.body_data.expr.end();
                    },
                    .block => {
                        return case.body_data.block.ptr[case.body_data.block.len-1].end();
                    },
                    .fallthrough => {
                        switch (case.kind) {
                            else => { 
                                return self.pos() + 4;
                            }
                        }
                    },
                }
            },
            .catchStmt      => {
                const catch_stmt = self.cast(.catchStmt);
                return catch_stmt.stmts[catch_stmt.stmts.len-1].end();
            },
            .const_decl     => {
                const decl = self.cast(.const_decl);
                if (decl.right) |right| {
                    return right.end();
                }
                return decl.type.?.end();
            },
            .continueStmt   => self.cast(.continueStmt).pos + 8,
            .cstruct_decl   => {
                const decl = self.cast(.cstruct_decl);
                const field: *Node = @ptrCast(decl.fields.ptr[decl.fields.len-1]);
                return field.end();
            },
            .ct_stmt        => self.cast(.ct_stmt).child.end(),
            .cunion_decl    => {
                const decl = self.cast(.cunion_decl);
                if (decl.cases.len > 0) {
                    const case: *Node = @ptrCast(decl.cases.ptr[decl.cases.len-1]);
                    return case.end();
                } else {
                    return decl.name.end();
                }
            },
            .custom_type_decl => self.cast(.custom_type_decl).name.end(),
            .decLit         => self.cast(.decLit).end(),
            .dec_u          => self.cast(.dec_u).end(),
            .deref          => self.cast(.deref).end,
            // .dot_array_lit  => self.cast(.dot_array_lit).pos,
            .dollar         => self.cast(.dollar).pos + 1,
            .dollar_lit     => self.cast(.dollar_lit).name.end(),
            .dot            => self.cast(.dot).pos + 1,
            .dot_lit        => self.cast(.dot_lit).end(),
            .else_block     => {
                const block = self.cast(.else_block);
                return block.stmts[block.stmts.len-1].end();
            },
            .elseif_block     => {
                const block = self.cast(.elseif_block);
                return block.stmts[block.stmts.len-1].end();
            },
            .enumDecl       => {
                const decl = self.cast(.enumDecl);
                const member: *Node = @ptrCast(decl.members.ptr[decl.members.len-1]);
                return member.end();
            },
            .enumMember     => {
                const member = self.cast(.enumMember);
                if (member.typeSpec) |type_| {
                    return type_.end();
                }
                return member.name.end();
            },
            .error_lit      => self.cast(.error_lit).name.end(),
            .expand_lit     => self.cast(.expand_lit).end,
            .exprStmt       => self.cast(.exprStmt).child.end(),
            .falseLit       => self.cast(.falseLit).pos + 5,
            .floatLit       => self.cast(.floatLit).end(),
            .for_iter_stmt  => {
                const stmt = self.cast(.for_iter_stmt);
                return stmt.stmts[stmt.stmts.len-1].end();
            },
            .for_range_stmt => {
                const stmt = self.cast(.for_range_stmt);
                return stmt.stmts[stmt.stmts.len-1].end();
            },
            .funcDecl       => {
                const decl = self.cast(.funcDecl);
                if (decl.stmts.len > 0) {
                    return decl.stmts.ptr[decl.stmts.len-1].end();
                }
                if (decl.ret) |ret| {
                    return ret.end();
                }
                return decl.name.end();
            },
            .func_param     => {
                const node = self.cast(.func_param);
                if (node.type) |type_spec| {
                    return type_spec.end();
                }
                return node.name_type.end();
            },
            .fn_type      => {
                const node = self.cast(.fn_type);
                if (node.ret) |ret| {
                    return ret.end();
                }
                return node.pos;
            },
            .fnsym_type      => {
                const node = self.cast(.fnsym_type);
                if (node.ret) |ret| {
                    return ret.end();
                }
                return node.pos;
            },
            .generic_vector_type => self.cast(.generic_vector_type).elem_t.end(),
            .generic_expand => self.cast(.generic_expand).end,
            .global_decl    => {
                const decl = self.cast(.global_decl);
                if (decl.right) |right| {
                    return right.end();
                }
                return decl.typeSpec.end();
            },
            .group          => self.cast(.group).end,
            .hexLit         => self.cast(.hexLit).end(),
            .ident          => self.cast(.ident).end(),
            .if_expr        => self.cast(.if_expr).else_expr.end(),
            .if_stmt        => {
                const stmt = self.cast(.if_stmt);
                if (stmt.else_blocks.len > 0) {
                    return stmt.else_blocks[stmt.else_blocks.len-1].end();
                }
                return stmt.stmts[stmt.stmts.len-1].end();
            },
            .if_unwrap_stmt => {
                const stmt = self.cast(.if_unwrap_stmt);
                if (stmt.else_blocks.len > 0) {
                    return stmt.else_blocks[stmt.else_blocks.len-1].end();
                }
                return stmt.stmts[stmt.stmts.len-1].end();
            },
            // .impl_with      => self.cast(.impl_with).pos,
            .impl_decl      => self.cast(.impl_decl).trait.end(),
            .import_stmt    => {
                const import_stmt = self.cast(.import_stmt);
                if (import_stmt.spec) |spec| {
                    return spec.end();
                }
                return import_stmt.name.end();
            },
            .infer_param    => self.cast(.infer_param).name.end(),
            .index_expr     => self.cast(.index_expr).end,
            .init_expr      => self.cast(.init_expr).lit.end,
            .init_lit       => self.cast(.init_lit).end,
            .keyValue       => self.cast(.keyValue).value.end(),
            .label_decl     => @panic("unexpected"),
            .lambda_cont_expr => {
                const stmt = self.cast(.lambda_cont_expr);
                return stmt.stmts[stmt.stmts.len-1].end();
            },
            .lambda_cont    => {
                const lambda = self.cast(.lambda_cont);
                return lambda.sig_end;
            },
            .lambda_expr    => self.cast(.lambda_expr).expr.end(),
            .lambda_multi   => {
                const lambda = self.cast(.lambda_multi);
                return lambda.stmts[lambda.stmts.len-1].end();
            },
            .move_expr      => self.cast(.move_expr).expr.end(),
            .namedArg       => self.cast(.namedArg).arg.end(),
            .noneLit        => self.cast(.noneLit).pos + 4,
            .octLit         => self.cast(.octLit).end(),
            .op_assign_stmt => self.cast(.op_assign_stmt).right.end(),
            .option_type    => self.cast(.option_type).child.end(),
            .partial_vector_type => self.cast(.partial_vector_type).child.end(),
            .passStmt       => self.cast(.passStmt).pos + 4,
            .ptr            => self.cast(.ptr).child.end(),
            .range          => {
                const range = self.cast(.range);
                if (range.end) |end_| {
                    return end_.end();
                }
                return range.op_end;
            },
            .sq_string_lit => self.cast(.sq_string_lit).end(),
            .sq_string_multi_lit => self.cast(.sq_string_multi_lit).end(),
            .raw_string_lit => self.cast(.raw_string_lit).end(),
            .raw_string_multi_lit => self.cast(.raw_string_multi_lit).end(),
            .ref            => self.cast(.ref).child.end(),
            .returnExprStmt => self.cast(.returnExprStmt).child.end(),
            .returnStmt     => self.cast(.returnStmt).pos + 6,
            .root           => {
                const root = self.cast(.root);
                if (root.stmts.len > 0) {
                    return root.stmts.ptr[root.stmts.len-1].end();
                } else {
                    return 0;
                }
            },
            .seqDestructure => self.cast(.seqDestructure).end,
            .slice_type     => self.cast(.slice_type).child.end(),
            .span_type      => self.cast(.span_type).child.end(),
            .special_string_lit => self.cast(.special_string_lit).lit.end(),
            .string_multi_lit => {
                return self.cast(.string_multi_lit).end();
            },
            .stringt        => {
                const str = self.cast(.stringt);
                return str.parts[str.parts.len-1].end();
            },
            .stringt_multi  => {
                const str = self.cast(.stringt_multi);
                return str.parts[str.parts.len-1].end();
            },
            .stringt_part   => {
                const part = self.cast(.stringt_part);
                return part.end();
            },
            .stringt_expr   => self.cast(.stringt_expr).child.end(),
            .string_lit     => self.cast(.string_lit).end(),
            .struct_decl    => {
                const decl = self.cast(.struct_decl);
                if (decl.fields.len > 0) {
                    const field: *Node = @ptrCast(decl.fields.ptr[decl.fields.len-1]);
                    return field.end();
                } else {
                    return decl.name.end();
                }
            },
            .struct_field   => {
                const field = self.cast(.struct_field);
                if (field.init) |init| {
                    return init.end();
                }
                if (field.typeSpec) |type_| {
                    return type_.end();
                }
                return field.name.end();
            },
            .switchExpr     => {
                const block = self.cast(.switchExpr);
                return block.cases.ptr[block.cases.len-1].end();
            },
            .switch_stmt    => {
                const block = self.cast(.switch_stmt);
                return block.cases.ptr[block.cases.len-1].end();
            },
            .template       => self.cast(.template).child_decl.end(),
            .trait_decl     => {
                const decl = self.cast(.trait_decl);
                const func: *Node = @ptrCast(decl.funcs.ptr[decl.funcs.len-1]);
                return func.end();
            },
            .trueLit        => self.cast(.trueLit).pos + 4,
            .tryStmt        => {
                const stmt = self.cast(.tryStmt);
                const catch_: *Node = @ptrCast(stmt.catchStmt);
                return catch_.end();
            },
            .type_alias_decl => self.cast(.type_alias_decl).target.end(),
            .type_const_decl => {
                const decl = self.cast(.type_const_decl);
                return decl.stmts.ptr[decl.stmts.len-1].end();
            },
            .unary_expr     => self.cast(.unary_expr).child.end(),
            .undef_lit      => self.cast(.undef_lit).pos + 5,
            .union_case     => {
                const case = self.cast(.union_case);
                return case.payload_t.end();
            },
            .unwrap         => self.cast(.unwrap).end,
            .unwrap_choice  => self.cast(.unwrap_choice).right.end(),
            // .unwrap_or      => self.cast(.unwrap_or).opt.pos(),
            .unwrap_or      => self.cast(.unwrap_or).default.end(),
            .unwrap_or_block => {
                const unwrap = self.cast(.unwrap_or_block);
                return unwrap.else_stmts[unwrap.else_stmts.len-1].end();
            },
            .unwrap_res     => self.cast(.unwrap_res).end,
            .unwrap_res_or  => self.cast(.unwrap_res_or).default.end(),
            .unwrap_res_or_block => {
                const unwrap = self.cast(.unwrap_res_or_block);
                return unwrap.else_stmts[unwrap.else_stmts.len-1].end();
            },
            .use_alias      => {
                const node = self.cast(.use_alias);
                return node.target.end();
            },
            .var_decl       => {
                const decl = self.cast(.var_decl);
                return decl.right.end();
            },
            .vector_type    => self.cast(.vector_type).child.end(),
            .void_lit       => self.cast(.void_lit).pos + 1,
            .whileInfStmt   => {
                const stmt = self.cast(.whileInfStmt);
                return stmt.stmts[stmt.stmts.len-1].end();
            },
            .whileCondStmt  => {
                const while_stmt = self.cast(.whileCondStmt);
                return while_stmt.stmts[while_stmt.stmts.len-1].end();
            },
            // .whileOptStmt   => self.cast(.whileOptStmt).pos,
            .while_unwrap_stmt => {
                const while_stmt = self.cast(.while_unwrap_stmt);
                return while_stmt.stmts[while_stmt.stmts.len-1].end();
            },
            .with => self.cast(.with).end,
            .yield_stmt     => {
                const yield = self.cast(.yield_stmt);
                if (yield.child) |child| {
                    return child.end();
                } else {
                    return yield.pos + 5;
                }
            },
        };
    }
};

pub const BinaryExprOp = enum(u8) {
    index,
    plus,
    minus,
    star,
    pow,
    slash,
    percent,
    bitwiseAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseLeftShift,
    bitwiseRightShift,
    bang_equal,
    less,
    less_equal,
    greater,
    greater_equal,
    equal_equal,
    and_op,
    or_op,
    cast,
    range,
    reverse_range,
    dummy,

    pub fn name(self: BinaryExprOp) []const u8 {
        return switch (self) {
            .index => "$index",
            .less => "<",
            .greater => ">",
            .less_equal => "<=",
            .greater_equal => ">=",
            .minus => "-",
            .plus => "+",
            .star => "*",
            .slash => "/",
            .percent => "%",
            .pow => "**",
            .bitwiseAnd => "&&",
            .bitwiseOr => "||",
            .bitwiseXor => "~",
            .bitwiseLeftShift => "<<",
            .bitwiseRightShift => ">>",
            else => "unknown",
        };
    }
};

pub const UnaryOp = enum(u8) {
    minus,
    lnot,
    bitwiseNot,
    address_of,
    dummy,

    pub fn name(self: UnaryOp) []const u8 {
        return switch (self) {
            .minus => "-",
            .lnot => "!",
            .bitwiseNot => "~",
            .address_of => "&",
            else => "unknown",
        };
    }
};

test "ast internals." {
    try t.eq(122, std.enums.values(NodeType).len);
    try t.eq(1, @sizeOf(NodeHeader));
}

pub const Ast = struct {
    node_alloc_handle: std.heap.ArenaAllocator,
    node_alloc: std.mem.Allocator,
    root: ?*Root,
    null_node: *Node,
    src: []const u8,
    src_id: u32,

    /// Generated source literals from templates or CTE.
    srcGen: std.ArrayListUnmanaged(u8),

    /// Heap generated strings, stable pointers unlike `srcGen`.
    /// Used for:
    /// - Unnamed struct identifiers.
    /// - Unescaped strings.
    strs: std.ArrayListUnmanaged([]const u8),

    /// Optionally parsed by tokenizer.
    comments: std.ArrayListUnmanaged(cy.IndexSlice(u32)),

    pub fn init(self: *Ast, alloc: std.mem.Allocator, src: []const u8, src_id: u32) !void {
        self.* = .{
            .node_alloc_handle = std.heap.ArenaAllocator.init(alloc),
            .node_alloc = undefined,
            .root = null,
            .null_node = undefined,
            .src = src,
            .src_id = src_id,
            .srcGen = .{},
            .strs = .{},
            .comments = .{},
        };
        self.node_alloc = self.node_alloc_handle.allocator();
        try self.clearNodes();
    }

    pub fn deinit(self: *Ast, alloc: std.mem.Allocator) void {
        self.node_alloc_handle.deinit();
        self.srcGen.deinit(alloc);
        for (self.strs.items) |str| {
            alloc.free(str);
        }
        self.strs.deinit(alloc);
        self.comments.deinit(alloc);
    }

    pub fn clearNodes(self: *Ast) !void {
        _ = self.node_alloc_handle.reset(.retain_capacity);
        self.null_node = try self.newEmptyNode(.null);
    }

    pub fn view(self: *const Ast) AstView {
        return .{
            .root = self.root,
            .null_node = self.null_node,
            .src = self.src,
            .srcGen = self.srcGen.items,
        };
    }

    pub fn dupeNodes(self: *Ast, nodes: []const *Node) ![]*Node {
        return self.node_alloc.dupe(*Node, nodes);
    }

    pub fn newEmptyNode(self: *Ast, comptime node_t: NodeType) !*NodeData(node_t) {
        const slice = try self.node_alloc.alignedAlloc(u8, .@"8", @sizeOf(NodeData(node_t)) + 8);
        @as(*NodeType, @ptrFromInt(@intFromPtr(slice.ptr) + 8 - 1)).* = node_t;
        return @ptrFromInt(@intFromPtr(slice.ptr) + 8);
    }

    pub fn newNode(self: *Ast, comptime node_t: NodeType, data: NodeData(node_t)) !*NodeData(node_t) {
        const n = try self.newEmptyNode(node_t);
        n.* = data;
        return n;
    }

    pub fn newNodeErase(self: *Ast, comptime node_t: NodeType, data: NodeData(node_t)) !*Node {
        const n = try self.newEmptyNode(node_t);
        n.* = data;
        return @ptrCast(n);
    }

    pub fn genIdentNode(self: *Ast, alloc: std.mem.Allocator, name: []const u8) !*Ident {
        const dup = try alloc.dupe(u8, name);
        try self.strs.append(alloc, dup);
        const span = try self.newNode(.ident, .{
            .src = self.src_id,
            .pos = cy.NullId,
            .name = .{ .ptr = dup.ptr, .len = dup.len },
        });
        return span;
    }

    pub fn newLitNode(self: *Ast, comptime node_t: NodeType, src_pos: usize, str_end: usize) !*Literal {
        const span = try self.newEmptyNode(node_t);
        const slice = self.src[src_pos..str_end];
        span.* = .{
            .src = self.src_id,
            .pos = @intCast(src_pos),
            .value = .{ .ptr = slice.ptr, .len = slice.len },
        };
        return span;
    }

    pub fn text(self: Ast, n: *Node) []const u8 {
        return self.src[n.pos()..n.end()];
    }
};

pub const AstView = struct {
    root: ?*Root,
    null_node: *Node,
    src: []const u8,
    srcGen: []const u8,

    /// Find the line/col in `src` at `pos`.
    /// Iterating tokens could be faster but it would still require counting new lines for skipped segments like comments, multiline strings.
    pub fn computeLinePos(self: AstView, pos: u32, outLine: *u32, outCol: *u32, outLineStart: *u32) void {
        var line: u32 = 0;
        var lineStart: u32 = 0;
        for (self.src, 0..) |ch, i| {
            if (i == pos) {
                break;
            }
            if (ch == '\n') {
                line += 1;
                lineStart = @intCast(i + 1);
            }
        }
        // This also handles the case where target pos is at the end of source.
        outLine.* = line;
        outCol.* = pos - lineStart;
        outLineStart.* = lineStart;
    }

    pub fn declNamePath(self: AstView, n: *Node) ![]const u8 {
        return switch (n.type()) {
            .cstruct_decl => b: {
                break :b n.cast(.cstruct_decl).name.declName();
            },
            .struct_decl => b: {
                break :b n.cast(.struct_decl).name.declName();
            },
            .custom_type_decl => n.cast(.custom_type_decl).name.declName(),
            .trait_decl => n.cast(.trait_decl).name.declName(),
            .enumDecl => n.cast(.enumDecl).name.declName(),
            .import_stmt => b: {
                const import_stmt = n.cast(.import_stmt);
                if (import_stmt.name.type() == .all) {
                    break :b "*";
                }
                break :b import_stmt.name.declName();
            },
            .use_alias => n.cast(.use_alias).name.declName(),
            .type_alias_decl => n.cast(.type_alias_decl).name.declName(),
            .template => try self.declNamePath(n.cast(.template).child_decl),
            .global_decl => self.getNamePathBase(n.cast(.global_decl).name).name_path,
            .const_decl => self.getNamePathBase(n.cast(.const_decl).name).name_path,
            .funcDecl => self.getNamePathBase(n.cast(.funcDecl).name).name_path,
            else => {
                log.tracev("{}", .{n.type()});
                return error.Unsupported;
            }
        };
    }

    pub fn fieldNameString(self: AstView, n: *Node) []const u8 {
        return self.declNameString(n);
    }

    pub fn declNameString(self: AstView, n: *Node) []const u8 {
        switch (n.type()) {
            .ident => return n.cast(.ident).name,
            .raw_string_lit => return n.cast(.raw_string_lit).value,
            else => {
                return self.nodeString(n);
            }
        }
    }

    pub fn nodeString(self: AstView, n: *Node) []const u8 {
        return self.src[n.pos()..n.end()];
    }
};

pub const NamePathBase = struct {
    name: []const u8,
    node: *Node,
};

const EncodeEvent = enum {
    preNode,
    postNode,
};

/// Simple encoder. Emits nodes from source pos.
/// TODO: Remove and just use ast.nodeString().
pub const Encoder = struct {
    c: *cy.Compiler,
    eventHandler: ?*const fn (Encoder, EncodeEvent, *Node) void = null,

    pub fn format(self: Encoder, node: ?*Node) []const u8 {
        if (node == null) {
            return "";
        }
        if (node.?.src() == cy.NullId or node.?.pos() == cy.NullId) {
            // Generated node.
            return "<generated>";
        }
        const chunk = self.c.chunks.items[node.?.src()];
        return chunk.ast.nodeString(node.?);
    }

    pub fn formatTrunc(self: Encoder, node: ?*Node) []const u8 {
        const res = self.format(node);
        if (res.len > 32) {
            return res[0..32];
        }
        return res;
    }
};

// const VisitNode = packed struct {
//     nodeId: u31,
//     visited: bool,
// };

// pub const Visitor = struct {
//     alloc: std.mem.Allocator,
//     ast: AstView,
//     stack: std.ArrayListUnmanaged(VisitNode),

//     pub fn deinit(self: *Visitor) void {
//         self.stack.deinit(self.alloc);
//     }

//     pub fn visit(self: *Visitor, rootId: *Node,
//         comptime C: type, ctx: C, visitFn: *const fn(ctx: C, nodeId: NodeId, enter: bool) bool) !void {

//         self.stack.clearRetainingCapacity();
//         try self.pushNode(rootId);
//         while (self.stack.items.len > 0) {
//             const vnode = &self.stack.items[self.stack.items.len-1];
//             if (!vnode.visited) {
//                 if (visitFn(ctx, vnode.nodeId, true)) {
//                     vnode.visited = true;
//                     const node = self.ast.node(vnode.nodeId);
//                     switch (node.type()) {
//                         .objectField => {},
//                         .objectDecl => {
//                             try self.pushNodeList(node.data.objectDecl.funcHead, node.data.objectDecl.numFuncs);
//                             const header = self.ast.node(node.data.objectDecl.header);
//                             try self.pushNodeList(header.data.objectHeader.fieldHead, header.data.objectHeader.numFields);
//                         },
//                         else => {
//                             cy.rt.logZFmt("TODO: {}", .{node.type()});
//                             return error.TODO;
//                         }
//                     }
//                 } else {
//                     self.stack.items.len -= 1;
//                 }
//             } else {
//                 _ = visitFn(ctx, vnode.nodeId, false);
//                 self.stack.items.len -= 1;
//             }
//         }
//     }

//     fn pushNode(self: *Visitor, nodeId: NodeId) !void {
//         try self.stack.append(self.alloc, .{
//             .nodeId = @intCast(nodeId),
//             .visited = false,
//         });
//     }

//     fn pushNodeList(self: *Visitor, head: NodeId, size: u32) !void {
//         try self.stack.ensureUnusedCapacity(self.alloc, size);
//         self.stack.items.len += size;

//         var i: u32 = 0;
//         var cur = head;
//         while (cur != cy.NullNode) {
//             self.stack.items[self.stack.items.len-1-i] = .{
//                 .nodeId = @intCast(cur),
//                 .visited = false,
//             };
//             i += 1;
//             cur = self.ast.node(cur).next();
//         }
//     }
// };

pub fn findAttr(attrs: []const *Attribute, attr_t: AttributeType) ?*Attribute {
    if (attrs.len == 0) {
        return null;
    }
    if (attrs.len == 1) {
        if (attrs[0].type == attr_t) {
            return attrs[0];
        }
        return null;
    }
    for (attrs) |attr| {
        if (attr.type == attr_t) {
            return attr;
        }
    }
    return null;
}
