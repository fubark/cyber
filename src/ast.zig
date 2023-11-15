const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const log = cy.log.scoped(.ast);

pub const NodeId = u32;

pub const NodeType = enum {
    root,
    exprStmt,
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
    condExpr,
    ifStmt,
    elseBlock,
    whileInfStmt,
    whileCondStmt,
    whileOptStmt,
    forRange,
    forRangeStmt,
    forIterStmt,
    forIterHeader,
    range_clause,
    eachClause,
    label_decl,
    hostVarDecl,
    hostFuncDecl,
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
    enumMember,
    tagInit,
    symbolLit,
    errorSymLit,
    lambda_expr, 
    lambda_multi,
    arrayLiteral,
    recordLiteral,
    keyValue,
    coinit,
    coyield,
    coresume,
    importStmt,
    tryExpr,
    tryStmt,
    throwExpr,
    group,
    caseBlock,
    switchBlock,
    castExpr,
};

pub const Node = struct {
    /// TODO: Since type is often accessed before visiting a node, it should go into a separate array.
    node_t: NodeType,
    hasParentAssignStmt: bool = false,

    /// TODO: Once tokenizer is merged into AST parser, this would become src start pos.
    start_token: u32,

    next: NodeId,
    /// Fixed size. TODO: Rename to `data`.
    head: union {
        exprStmt: struct {
            child: NodeId,
            isLastRootStmt: bool = false, 
        },
        tryStmt: struct {
            tryFirstStmt: NodeId,
            errorVar: cy.Nullable(NodeId),
            catchFirstStmt: NodeId,
        },
        tryExpr: struct {
            expr: NodeId,
            catchExpr: cy.Nullable(NodeId),
        },
        errorSymLit: struct {
            symbol: NodeId,
        },
        castExpr: struct {
            expr: NodeId,
            typeSpecHead: NodeId,
        },
        indexExpr: struct {
            left: NodeId,
            right: NodeId,
        },
        binExpr: struct {
            left: NodeId,
            right: NodeId,
            op: BinaryExprOp,
        },
        opAssignStmt: struct {
            left: NodeId,
            right: NodeId,
            op: BinaryExprOp,
        },
        caseBlock: struct {
            // Null when `isElseCase` is true.
            condHead: cy.Nullable(NodeId),
            bodyHead: NodeId,
            numConds: u8,
            isElseCase: bool,
            bodyIsExpr: bool,
        },
        switchBlock: struct {
            expr: NodeId,
            caseHead: NodeId,
            numCases: u8,
        },
        annotation: struct {
            type: AnnotationType,
        },
        ifStmt: struct {
            cond: NodeId,
            bodyHead: cy.Nullable(cy.NodeId),
            elseHead: cy.Nullable(cy.NodeId),
            numElseBlocks: u8,
        },
        left_right: struct {
            left: NodeId,
            right: NodeId,
            extra: u32 = cy.NullId,
        },
        accessExpr: struct {
            left: NodeId,
            right: NodeId,
        },
        callExpr: struct {
            callee: NodeId,
            arg_head: NodeId,
            numArgs: u8,
            has_named_arg: bool,
        },
        ident: struct {
            semaVarId: u32 = cy.NullId,
            semaModSymId: cy.module.ModuleSymId = cy.NullId,
        },
        unary: struct {
            child: NodeId,
            op: UnaryOp,
        },
        root: struct {
            headStmt: NodeId,
        },
        child_head: NodeId,
        arrayLiteral: struct {
            argHead: NodeId,
            numArgs: u8,
        },
        recordLiteral: struct {
            argHead: NodeId,
            numArgs: u8,
        },
        keyValue: struct {
            left: NodeId,
            right: NodeId,
        },
        comptimeExpr: struct {
            child: NodeId,
        },
        comptimeStmt: struct {
            expr: NodeId,
        },
        func: struct {
            header: NodeId,
            bodyHead: NodeId,
            semaModFuncId: cy.module.ModuleFuncId = cy.NullId,
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
        typeAliasDecl: struct {
            name: NodeId,
            typeSpecHead: NodeId,
        },
        objectInit: struct {
            name: NodeId,
            initializer: NodeId,
        },
        objectField: struct {
            name: NodeId,
            /// Type spec consists of ident nodes linked by `next`.
            typeSpecHead: cy.Nullable(NodeId),
            typed: bool,
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
            typed: bool,
        },
        localDecl: struct {
            varSpec: NodeId,
            right: NodeId,
            typed: bool,
        },
        enumMember: struct {
            name: NodeId,
        },
        enumDecl: struct {
            name: NodeId,
            memberHead: NodeId,
            numMembers: u8,
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
        forRange: struct {
            left: NodeId,
            right: NodeId,
            increment: bool,
        },
        forRangeStmt: struct {
            range_clause: NodeId,
            body_head: NodeId,
            eachClause: NodeId,
        },
        forIterStmt: struct {
            header: NodeId,
            bodyHead: NodeId,
        },
        forIterHeader: struct {
            iterable: NodeId,
            eachClause: NodeId,
            count: cy.Nullable(NodeId),
        },
        seqDestructure: struct {
            head: NodeId,
            numArgs: u8,
        },
        sliceExpr: struct {
            arr: NodeId,
            left: NodeId,
            right: NodeId,
        },
        condExpr: struct {
            cond: NodeId,
            bodyExpr: NodeId,
            elseExpr: NodeId,
        },
        elseBlock: struct {
            bodyHead: NodeId,
            // for else ifs only.
            cond: NodeId,
        },
        stringTemplate: struct {
            exprHead: NodeId,
            strHead: NodeId,
            numExprs: u8,
        },
        nonDecInt: struct {
        },
    },
};

pub const BinaryExprOp = enum(u8) {
    index,
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

pub const UnaryOp = enum(u8) {
    minus,
    not,
    bitwiseNot,
    dummy,
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
        try t.eq(@sizeOf(Node), 28);
    } else {
        try t.eq(@sizeOf(Node), 32);
    }
}

pub const Source = struct {
    src: []const u8,
    nodes: []const Node,
    tokens: []const cy.Token,

    pub fn getParentAssignStmt(self: Source, nodeId: cy.NodeId) cy.NodeId {
        var cur = nodeId;
        while (true) {
            cur -= 1;
            const nodeT = self.nodes[cur].node_t;
            if (nodeT == .localDecl or nodeT == .assign_stmt or nodeT == .staticDecl) {
                return cur;
            }
        }
    }

    pub fn getNodeStringById(self: Source, nodeId: cy.NodeId) []const u8 {
        return getNodeString(self, self.nodes[nodeId]);
    }

    pub fn getNodeString(self: Source, node: cy.Node) []const u8 {
        const token = self.tokens[node.start_token];
        return self.src[token.pos()..token.data.end_pos];
    }

    pub fn getNodeStringWithDelim(self: Source, node: cy.Node) []const u8 {
        const token = self.tokens[node.start_token];
        return self.src[token.pos()-1..token.data.end_pos+1];
    }

    pub fn getNamePathStr(self: Source, nameId: cy.NodeId) []const u8 {
        const nameN = self.nodes[nameId];
        if (nameN.next == cy.NullId) {
            return self.getNodeString(nameN);
        } else {
            const lastId = self.getLastNameNode(nameN.next);
            const last = self.nodes[lastId];
            const startToken = self.tokens[nameN.start_token];
            const lastToken = self.tokens[last.start_token];
            var end = lastToken.data.end_pos;
            if (lastToken.tag() == .string) {
                end += 1;
            }
            return self.src[startToken.pos()..end];
        }
    }

    pub fn getLastNameNode(self: Source, nameId: cy.NodeId) cy.NodeId {
        var name = self.nodes[nameId];
        var curId = nameId;
        while (name.next != cy.NullId) {
            name = self.nodes[name.next];
            curId = name.next;
        }
        return curId;
    }
};

const EncodeEvent = enum {
    preNode,
    postNode,
};

fn getUnOpStr(op: UnaryOp) []const u8 {
    return switch (op) {
        .minus => "-",
        .not => "!",
        .bitwiseNot => "~",
        .dummy => cy.unexpected(),
    };
}

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
        .and_op => " and ",
        .or_op => " or ",
        .cast => "as",
        .index,
        .dummy => cy.unexpected(),
    };
}

/// The default encoder doesn't insert any formatting and is used to
/// provide a quick context summary next to generated code.
pub const Encoder = struct {
    src: Source,
    eventHandler: ?*const fn (Encoder, EncodeEvent, cy.NodeId) void = null,

    pub fn formatNode(self: Encoder, nodeId: cy.NodeId, buf: []u8) ![]const u8 {
        if (nodeId == cy.NullId) {
            return "";
        }
        var fbuf = std.io.fixedBufferStream(buf);
        try self.writeNode(fbuf.writer(), nodeId);
        return fbuf.getWritten();
    }

    pub fn writeNode(self: Encoder, w: anytype, nodeId: cy.NodeId) !void {
        const node = self.src.nodes[nodeId];
        switch (node.node_t) {
            .assign_stmt => {
                try self.writeNode(w, node.head.left_right.left);
                try w.writeByte('=');
                try self.writeNode(w, node.head.left_right.right);
            },
            .opAssignStmt => {
                try self.writeNode(w, node.head.opAssignStmt.left);
                try w.writeAll(getBinOpStr(node.head.opAssignStmt.op));
                try w.writeByte('=');
                try self.writeNode(w, node.head.opAssignStmt.right);
            },
            .unary_expr => {
                const op = node.head.unary.op;
                try w.writeAll(getUnOpStr(op));
                try self.writeNode(w, node.head.unary.child);
            },
            .binExpr => {
                try self.writeNode(w, node.head.binExpr.left);
                try w.writeAll(getBinOpStr(node.head.binExpr.op));
                try self.writeNode(w, node.head.binExpr.right);
            },
            .exprStmt => {
                try self.writeNode(w, node.head.exprStmt.child);
            },
            .condExpr => {
                try self.writeNode(w, node.head.condExpr.cond);
                try w.writeAll("?");
                try self.writeNode(w, node.head.condExpr.bodyExpr);
                try w.writeAll(" else ");
                try self.writeNode(w, node.head.condExpr.elseExpr);
            },
            .caseBlock => {
                if (node.head.caseBlock.isElseCase) {
                    try w.writeAll("else");
                } else {
                    var cond = node.head.caseBlock.condHead;
                    try self.writeNode(w, cond);
                    cond = self.src.nodes[cond].next;
                    while (cond != cy.NullId) {
                        try w.writeByte(',');
                        try self.writeNode(w, cond);
                        cond = self.src.nodes[cond].next;
                    }
                }
                if (node.head.caseBlock.bodyIsExpr) {
                    try w.writeAll("=>");
                    try self.writeNode(w, node.head.caseBlock.bodyHead);
                } else {
                    try w.writeAll(": ...");
                }
            },
            .none => {
                try w.writeAll("none");
            },
            .false_literal => {
                try w.writeAll("false");
            },
            .true_literal => {
                try w.writeAll("true");
            },
            .errorSymLit => {
                try w.writeAll("error.");
                try self.writeNode(w, node.head.errorSymLit.symbol);
            },
            .number,
            .ident => {
                try w.writeAll(self.src.getNodeString(node));
            },
            .string => {
                try w.writeAll(self.src.getNodeStringWithDelim(node));
            },
            .accessExpr => {
                try self.writeNode(w, node.head.accessExpr.left);
                try w.writeByte('.');
                try self.writeNode(w, node.head.accessExpr.right);
            },
            .group => {
                try w.writeByte('(');
                try self.writeNode(w, node.head.child_head);
                try w.writeByte(')');
            },
            .sliceExpr => {
                try self.writeNode(w, node.head.sliceExpr.arr);
                try w.writeByte('[');
                if (node.head.sliceExpr.left != cy.NullId) {
                    try self.writeNode(w, node.head.sliceExpr.left);
                }
                try w.writeAll("..");
                if (node.head.sliceExpr.right != cy.NullId) {
                    try self.writeNode(w, node.head.sliceExpr.right);
                }
                try w.writeByte(']');
            },
            .indexExpr => {
                try self.writeNode(w, node.head.indexExpr.left);
                try w.writeByte('[');
                try self.writeNode(w, node.head.indexExpr.right);
                try w.writeByte(']');
            },
            .throwExpr => {
                try w.writeAll("throw ");
                try self.writeNode(w, node.head.child_head);
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
            .tryExpr => {
                try w.writeAll("try ");
                try self.writeNode(w, node.head.tryExpr.expr);
                if (node.head.tryExpr.catchExpr != cy.NullId) {
                    try w.writeAll(" catch ");
                    try self.writeNode(w, node.head.tryExpr.catchExpr);
                }
            },
            .localDecl => {
                try w.writeAll("var ");
                try self.writeNode(w, node.head.localDecl.varSpec);
                try w.writeByte('=');
                try self.writeNode(w, node.head.localDecl.right);
            },
            .arrayLiteral => {
                try w.writeByte('[');
                try w.writeAll("...");
                try w.writeByte(']');
            },
            .recordLiteral => {
                try w.writeByte('[');
                try w.writeAll("...");
                try w.writeByte(']');
            },
            .varSpec => {
                try self.writeNode(w, node.head.varSpec.name);
                if (node.head.varSpec.typeSpecHead != cy.NullId) {
                    var cur = node.head.varSpec.typeSpecHead;
                    try self.writeNode(w, cur);
                    cur = self.src.nodes[cur].next;
                    while (cur != cy.NullId) {
                        try w.writeByte('.');
                        try self.writeNode(w, cur);
                        cur = self.src.nodes[cur].next;
                    }
                }
            },
            else => {
                try w.writeByte('<');
                try w.writeAll(@tagName(node.node_t));
                try w.writeByte('>');
            },
        }
    }
};