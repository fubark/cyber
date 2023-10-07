const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");

pub const NodeId = u32;

pub const NodeType = enum {
    root,
    expr_stmt,
    assign_stmt,
    opAssignStmt,
    varSpec,
    staticDecl,
    localDecl,
    pass_stmt,
    breakStmt,
    continueStmt,
    return_stmt,
    return_expr_stmt,
    comptimeExpr,
    comptimeStmt,
    annotation,
    ident,
    true_literal,
    false_literal,
    none,
    string,
    stringTemplate,
    await_expr,
    accessExpr,
    indexExpr,
    sliceExpr,
    callExpr,
    named_arg,
    binExpr,
    unary_expr,
    number,
    float,
    nonDecInt,
    if_expr,
    if_stmt,
    else_clause,
    whileInfStmt,
    whileCondStmt,
    whileOptStmt,
    for_range_stmt,
    for_iter_stmt,
    range_clause,
    eachClause,
    label_decl,
    hostVarDecl,
    hostFuncDecl,
    methDecl,
    methDeclInit,
    funcDecl,
    funcDeclInit,
    funcHeader,
    funcParam,
    hostObjectDecl,
    seqDestructure,
    objectDecl,
    objectDeclBody,
    objectField,
    objectInit,
    typeAliasDecl,
    enumDecl,
    tagMember,
    tagInit,
    symbolLit,
    errorSymLit,
    lambda_assign_decl,
    lambda_expr, 
    lambda_multi,
    map_literal,
    mapEntry,
    arr_literal,
    coinit,
    coyield,
    coresume,
    importStmt,
    tryExpr,
    tryStmt,
    throwExpr,
    group,
    caseBlock,
    matchBlock,
    elseCase,
    castExpr,
};

pub const Node = struct {
    node_t: NodeType,

    /// TODO: Once tokenizer is merged into AST parser, this would become src start pos.
    start_token: u32,

    next: NodeId,
    /// Fixed size. TODO: Rename to `data`.
    head: union {
        tryStmt: struct {
            tryFirstStmt: NodeId,
            errorVar: cy.Nullable(NodeId),
            catchFirstStmt: NodeId,
        },
        tryExpr: struct {
            expr: NodeId,
            elseExpr: cy.Nullable(NodeId),
        },
        errorSymLit: struct {
            symbol: NodeId,
        },
        castExpr: struct {
            expr: NodeId,
            typeSpecHead: NodeId,
            semaTypeSymId: cy.sema.SymbolId = cy.NullId,
        },
        indexExpr: struct {
            left: NodeId,
            right: NodeId,
            semaGenStrat: GenBinExprStrategy = .none,
        },
        binExpr: struct {
            left: NodeId,
            right: NodeId,
            op: BinaryExprOp,
            semaGenStrat: GenBinExprStrategy = .none,
        },
        opAssignStmt: struct {
            left: NodeId,
            right: NodeId,
            op: BinaryExprOp,
            semaGenStrat: GenBinExprStrategy = .none,
        },
        mapEntry: struct {
            left: NodeId,
            right: NodeId,

            // Used for object initializers to map an entry to an object field.
            semaFieldIdx: u32 = cy.NullId,
        },
        caseBlock: struct {
            firstCond: NodeId,
            firstChild: NodeId,
        },
        matchBlock: struct {
            expr: NodeId,
            firstCase: NodeId,
        },
        annotation: struct {
            type: AnnotationType,
        },
        left_right: struct {
            left: NodeId,
            right: NodeId,
            extra: u32 = cy.NullId,
        },
        accessExpr: struct {
            left: NodeId,
            right: NodeId,
            /// Symbol id of a var or func. NullId if it does not point to a symbol.
            sema_csymId: cy.sema.CompactSymbolId = cy.sema.CompactSymbolId.initNull(),
        },
        callExpr: struct {
            callee: NodeId,
            arg_head: NodeId,
            numArgs: u8,
            has_named_arg: bool,
        },
        ident: struct {
            semaVarId: u32 = cy.NullId,
            sema_csymId: cy.sema.CompactSymbolId = cy.sema.CompactSymbolId.initNull(),
            semaMethodSigId: cy.sema.FuncSigId = cy.NullId,
        },
        unary: struct {
            child: NodeId,
            op: UnaryOp,
            semaGenStrat: GenUnaryExprStrategy = .none,
        },
        root: struct {
            headStmt: NodeId,
        },
        child_head: NodeId,
        comptimeExpr: struct {
            child: NodeId,
        },
        comptimeStmt: struct {
            expr: NodeId,
        },
        func: struct {
            header: NodeId,
            bodyHead: NodeId,
            semaDeclId: cy.sema.FuncDeclId = cy.NullId,
        },
        funcHeader: struct {
            /// Can be NullId for lambdas.
            name: cy.Nullable(NodeId),
            paramHead: cy.Nullable(NodeId),
            ret: cy.Nullable(NodeId),
            // modifierHead is stored in `next`.
        },
        funcParam: struct {
            name: NodeId,
            /// Type spec consists of ident nodes linked by `next`.
            typeSpecHead: cy.Nullable(NodeId),
        },
        lambda_assign_decl: struct {
            decl_id: cy.sema.FuncDeclId,
            body_head: NodeId,
            assign_expr: NodeId,
        },
        typeAliasDecl: struct {
            name: NodeId,
            typeSpecHead: NodeId,
        },
        objectInit: struct {
            name: NodeId,
            initializer: NodeId,
            sema_symId: cy.Nullable(cy.sema.SymbolId) = cy.NullId,
        },
        objectField: struct {
            name: NodeId,
            /// Type spec consists of ident nodes linked by `next`.
            typeSpecHead: cy.Nullable(NodeId),
        },
        objectDecl: struct {
            // `name` is an ident token with a semaSymId.
            name: NodeId,
            modifierHead: cy.Nullable(NodeId),
            body: NodeId,
        },
        objectDeclBody: struct {
            fieldsHead: NodeId,
            funcsHead: NodeId,
            numFields: u32,
        },
        varSpec: struct {
            name: NodeId,
            typeSpecHead: cy.Nullable(NodeId),
            modifierHead: cy.Nullable(NodeId),
            // `next` contains TypeId for @host var
        },
        staticDecl: struct {
            varSpec: NodeId,
            right: NodeId,
            sema_symId: cy.sema.SymbolId = cy.NullId,
        },
        localDecl: struct {
            varSpec: NodeId,
            right: NodeId,
        },
        tagMember: struct {
            name: NodeId,
        },
        enumDecl: struct {
            name: NodeId,
            memberHead: NodeId,
        },
        whileCondStmt: struct {
            cond: NodeId,
            bodyHead: NodeId,
        },
        whileOptStmt: struct {
            opt: NodeId,
            bodyHead: NodeId,
            some: NodeId,
        },
        for_range_stmt: struct {
            range_clause: NodeId,
            body_head: NodeId,
            eachClause: NodeId,
        },
        for_iter_stmt: struct {
            iterable: NodeId,
            body_head: NodeId,
            eachClause: NodeId,
        },
        seqDestructure: struct {
            head: NodeId,
        },
        sliceExpr: struct {
            arr: NodeId,
            left: NodeId,
            right: NodeId,
        },
        if_expr: struct {
            cond: NodeId,
            body_expr: NodeId,
            else_clause: NodeId,
        },
        else_clause: struct {
            body_head: NodeId,
            // for else ifs only.
            cond: NodeId,
            else_clause: NodeId,
        },
        stringTemplate: struct {
            partsHead: NodeId,
        },
        nonDecInt: struct {
            semaVal: u64,
        } align (4) ,
    },
};

pub const BinaryExprOp = enum {
    plus,
    minus,
    star,
    caret,
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
    dummy,
};

pub const AnnotationType = enum {
    host,
    custom,
};

pub const UnaryOp = enum {
    minus,
    not,
    bitwiseNot,
};

pub const GenUnaryExprStrategy = enum {
    none,
    specialized,
    generic,
};

pub const GenBinExprStrategy = enum {
    none,
    specialized,
    generic,
};

test "ast internals." {
    if (builtin.mode == .ReleaseFast) {
        try t.eq(@sizeOf(Node), 24);
    } else {
        try t.eq(@sizeOf(Node), 28);
    }
}

pub const Source = struct {
    src: []const u8,
    nodes: []const Node,
    tokens: []const cy.Token,

    pub fn getNodeStringById(src: Source, nodeId: cy.NodeId) []const u8 {
        return getNodeString(src, src.nodes[nodeId]);
    }

    pub fn getNodeString(src: Source, node: cy.Node) []const u8 {
        const token = src.tokens[node.start_token];
        return src.src[token.pos()..token.data.end_pos];
    }
};

const EncodeEvent = enum {
    preNode,
    postNode,
};

fn getBinOpStr(op: BinaryExprOp) []const u8 {
    return switch (op) {
        .plus => "+",
        .minus => "-",
        .star => "*",
        .caret => "^",
        .slash => "/",
        .percent => "%",
        .bitwiseAnd => "&",
        .bitwiseOr => "|",
        .bitwiseXor => "||",
        .bitwiseLeftShift => "<<",
        .bitwiseRightShift => ">>",
        .bang_equal => "!=",
        .less => "<",
        .less_equal => "<=",
        .greater => ">",
        .greater_equal => ">=",
        .equal_equal => "==",
        .and_op => "and",
        .or_op => "or",
        .cast => "as",
        .dummy => cy.unexpected(),
    };
}

/// The default encoder doesn't insert any formatting and is used to
/// provide a quick context summary next to generated code.
pub const Encoder = struct {
    src: Source,
    eventHandler: ?*const fn (Encoder, EncodeEvent, cy.NodeId) void = null,

    pub fn writeNode(self: Encoder, w: anytype, nodeId: cy.NodeId) !void {
        const node = self.src.nodes[nodeId];
        switch (node.node_t) {
            .opAssignStmt => {
                try self.writeNode(w, node.head.opAssignStmt.left);
                try w.writeAll(getBinOpStr(node.head.opAssignStmt.op));
                try w.writeByte('=');
                try self.writeNode(w, node.head.opAssignStmt.right);
            },
            .binExpr => {
                try self.writeNode(w, node.head.binExpr.left);
                try w.writeAll(getBinOpStr(node.head.binExpr.op));
                try self.writeNode(w, node.head.binExpr.right);
            },
            .expr_stmt => {
                try self.writeNode(w, node.head.child_head);
            },
            .none => {
                try w.writeAll("none");
            },
            .number,
            .ident => {
                try w.writeAll(self.src.getNodeString(node));
            },
            .accessExpr => {
                try self.writeNode(w, node.head.accessExpr.left);
                try w.writeByte('.');
                try self.writeNode(w, node.head.accessExpr.right);
            },
            .callExpr => {
                try self.writeNode(w, node.head.callExpr.callee);

                try w.writeByte('(');
                if (node.head.callExpr.numArgs > 0) {
                    var argId = node.head.callExpr.arg_head;
                    try self.writeNode(w, argId);
                    argId = self.src.nodes[argId].next;

                    while (argId != cy.NullId) {
                        try w.writeAll(", ");
                        try self.writeNode(w, argId);
                        argId = self.src.nodes[argId].next;
                    }
                }
                try w.writeByte(')');
            },
            else => {
                try w.writeByte('<');
                try w.writeAll(@tagName(node.node_t));
                try w.writeByte('>');
            },
        }
    }
};


