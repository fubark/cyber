const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const log = cy.log.scoped(.ast);

pub const NodeId = u32;

pub const NodeType = enum(u8) {
    @"null",
    root,
    exprStmt,
    assignStmt,
    opAssignStmt,
    varSpec,
    staticDecl,
    localDecl,
    passStmt,
    breakStmt,
    continueStmt,
    returnStmt,
    returnExprStmt,
    comptimeExpr,
    comptimeStmt,
    dirModifier,
    ident,
    trueLit,
    falseLit,
    noneLit,
    stringLit,
    runeLit,
    stringTemplate,
    await_expr,
    accessExpr,
    indexExpr,
    sliceExpr,
    range,
    callExpr,
    namedArg,
    binExpr,
    unary_expr,
    binLit,
    decLit,
    octLit,
    hexLit,
    floatLit,
    condExpr,
    ifStmt,
    ifBranch,
    elseBlock,
    whileInfStmt,
    whileCondStmt,
    whileOptStmt,
    whileOptHeader,
    forRangeHeader,
    forRangeStmt,
    forIterStmt,
    forIterHeader,
    eachClause,
    label_decl,
    hostVarDecl,
    hostFuncDecl,
    funcDecl,
    funcHeader,
    funcParam,
    hostObjectDecl,
    seqDestructure,
    objectDecl,
    objectHeader,
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
    arrayLit,
    recordLit,
    keyValue,
    coinit,
    coyield,
    coresume,
    importStmt,
    tryExpr,
    tryStmt,
    catchStmt,
    throwExpr,
    group,
    caseBlock,
    caseHeader,
    switchStmt,
    switchExpr,
    castExpr,
};

pub const DirModifierType = enum(u8) {
    host,
};

const NodeHead = packed struct {
    type: NodeType,
    data: packed union {
        // Statements use next to advance to the next statement.
        // Expressions also use this for sequential arguments.
        next: u24,
        funcHeader: packed struct {
            modHead: cy.Nullable(u24),
        },
        objectHeader: packed struct {
            modHead: cy.Nullable(u24),
        },
        varSpec: packed struct {
            modHead: cy.Nullable(u24),
        },
        forRangeHeader: packed struct {
            eachClause: u24,
        },
        forIterHeader: packed struct {
            count: cy.Nullable(u24),
        },
    },
};

/// At most 16 bytes in release mode.
const NodeData = union {
    uninit: void,
    exprStmt: struct {
        child: NodeId,
        isLastRootStmt: bool = false, 
    },
    returnExprStmt: struct {
        child: NodeId,
    },
    importStmt: struct {
        name: cy.NodeId,
        spec: cy.Nullable(cy.NodeId),
    },
    // idents and literals.
    span: struct {
        // This can be different from Node.srcPos if the literal was generated.
        pos: u32,
        len: u16,
        srcGen: bool,
    },
    namedArg: struct {
        name: NodeId,
        arg: NodeId,
    },
    tryStmt: struct {
        bodyHead: NodeId,
        catchStmt: NodeId,
    },
    catchStmt: struct {
        errorVar: cy.Nullable(NodeId),
        bodyHead: NodeId,
    },
    whileInfStmt: struct {
        bodyHead: NodeId,
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
    assignStmt: struct {
        left: NodeId,
        right: NodeId,
    },
    binExpr: packed struct {
        left: NodeId,
        right: u24,
        op: BinaryExprOp,
    },
    opAssignStmt: packed struct {
        left: NodeId,
        right: u24,
        op: BinaryExprOp,
    },
    caseBlock: packed struct {
        // Null if `else` case.
        header: NodeId,
        bodyHead: u24,
        bodyIsExpr: bool,
    },
    caseHeader: packed struct {
        condHead: cy.Nullable(NodeId),
        capture: cy.Nullable(u24),
        numConds: u8,
    },
    switchBlock: packed struct {
        expr: NodeId,
        caseHead: u24,
        numCases: u8,
    },
    dirModifier: struct {
        type: DirModifierType,
    },
    throwExpr: struct {
        child: NodeId,
    },
    group: struct {
        child: NodeId,
    },
    coresume: struct {
        child: NodeId,
    },
    coinit: struct {
        child: NodeId,
    },
    ifStmt: packed struct {
        ifBranch: NodeId,
        elseHead: cy.Nullable(u24),
        // if numElseBlocks > 0, `elseHead` points to first else block.
        numElseBlocks: u8,
    },
    ifBranch: struct {
        cond: NodeId,
        bodyHead: NodeId,
    },
    accessExpr: struct {
        left: NodeId,
        right: NodeId,
    },
    callExpr: packed struct {
        callee: u24,
        numArgs: u8,
        argHead: u24,
        hasNamedArg: bool,
    },
    unary: struct {
        child: NodeId,
        op: UnaryOp,
    },
    root: struct {
        bodyHead: NodeId,
    },
    arrayLit: struct {
        argHead: NodeId,
        numArgs: u8,
    },
    recordLit: packed struct {
        argHead: NodeId,
        argTail: u24, // For appending additional args from block syntax.
        numArgs: u8,
    },
    keyValue: struct {
        key: NodeId,
        value: NodeId,
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
    },
    funcHeader: struct {
        /// Can be NullNode for lambdas.
        name: cy.Nullable(NodeId),
        /// Params.
        paramHead: cy.Nullable(NodeId),
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
        initializer: NodeId, // Record literal.
    },
    objectField: packed struct {
        name: NodeId,
        /// Type spec path head (linked by `next`) or unnamed type decl.
        typeSpec: cy.Nullable(u24),
        typed: bool,
    },
    objectDecl: struct {
        header: NodeId,
        funcHead: NodeId,
    },
    objectHeader: packed struct {
        name: u24,
        /// When true, a non null `name` references an unnamed `ModuleSymId`.
        unnamed: bool,
        fieldHead: u24,
        numFields: u8,
    },
    varSpec: struct {
        name: NodeId,
        typeSpecHead: cy.Nullable(NodeId),
    },
    staticDecl: packed struct {
        varSpec: NodeId,
        right: u24,
        typed: bool,
        // Declared with `.` prefix.
        root: bool,
    },
    localDecl: packed struct {
        varSpec: NodeId,
        right: u24,
        typed: bool,
    },
    enumMember: struct {
        name: NodeId,
        /// Type spec path head (linked by `next`) or unnamed type decl.
        typeSpec: cy.Nullable(NodeId),
    },
    enumDecl: packed struct {
        name: u24,
        numMembers: u8,
        memberHead: u24,
        isChoiceType: bool,
    },
    whileCondStmt: struct {
        cond: NodeId,
        bodyHead: NodeId,
    },
    whileOptStmt: struct {
        header: NodeId,
        bodyHead: NodeId,
    },
    whileOptHeader: struct {
        opt: NodeId,
        capture: NodeId,
    },
    forRangeStmt: struct {
        header: NodeId,
        bodyHead: NodeId,
    },
    forRangeHeader: packed struct {
        start: NodeId,
        end: u24,
        increment: bool,
    },
    forIterStmt: struct {
        header: NodeId,
        bodyHead: NodeId,
    },
    forIterHeader: struct {
        iterable: NodeId,
        eachClause: NodeId,
    },
    seqDestructure: struct {
        head: NodeId,
        numArgs: u8,
    },
    sliceExpr: struct {
        arr: NodeId,
        range: NodeId,
    },
    range: struct {
        start: NodeId,
        end: NodeId,
    },
    condExpr: struct {
        ifBranch: NodeId,
        elseExpr: NodeId,
    },
    elseBlock: struct {
        bodyHead: NodeId,
        // for else ifs only.
        cond: NodeId,
    },
    stringTemplate: packed struct {
        exprHead: NodeId,
        strHead: u24,
        numExprs: u8,
    },
};

/// TODO: See if separating `head`, `srcPos`, and `data` improves perf for a large project.
pub const Node = struct {
    head: NodeHead,

    // Can be repurposed for secondary node data:
    // funcHeader.ret NodeId
    srcPos: u32,

    data: NodeData,

    pub fn @"type"(self: Node) NodeType {
        return self.head.type;
    }

    pub fn next(self: Node) NodeId {
        return self.head.data.next;
    }

    pub fn forIterHeader_count(self: Node) NodeId {
        return self.head.data.forIterHeader.count;
    }

    pub fn funcDecl_header(self: Node) NodeId {
        return self.data.func.header;
    }

    pub fn funcHeader_name(self: Node) NodeId {
        return self.data.funcHeader.name;
    }

    pub fn funcHeader_ret(self: Node) NodeId {
        return self.srcPos;
    }

    pub fn funcHeader_modHead(self: Node) NodeId {
        return self.head.data.funcHeader.modHead;
    }

    pub fn root_bodyHead(self: Node) NodeId {
        return self.data.root.bodyHead;
    }

    pub fn exprStmt_child(self: Node) NodeId {
        return self.data.exprStmt.child; 
    }

    pub fn recordLit_argHead(self: Node) NodeId {
        return self.data.recordLit.argHead;
    }

    pub fn keyValue_key(self: Node) NodeId {
        return self.data.keyValue.key;
    }

    pub fn keyValue_value(self: Node) NodeId {
        return self.data.keyValue.value;
    }
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

    pub fn name(self: BinaryExprOp) []const u8 {
        return switch (self) {
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
            else => "unknown",
        };
    }
};

pub const UnaryOp = enum(u8) {
    minus,
    not,
    bitwiseNot,
    dummy,

    pub fn name(self: UnaryOp) []const u8 {
        return switch (self) {
            .minus => "$prefix-",
            .not => "$prefix!",
            .bitwiseNot => "$prefix~",
            else => "unknown",
        };
    }
};

test "ast internals." {
    if (builtin.mode == .ReleaseFast) {
        try t.eq(@sizeOf(NodeHead), 4);
        try t.eq(@sizeOf(NodeData), 8);
        try t.eq(@sizeOf(Node), 16);
    } else {
        try t.eq(@sizeOf(NodeHead), 4);
        try t.eq(@sizeOf(NodeData), 16);
        try t.eq(@sizeOf(Node), 24);
    }
}

pub const Ast = struct {
    src: []const u8,
    nodes: std.ArrayListUnmanaged(Node),

    /// Generated source literals from templates or CTE.
    srcGen: std.ArrayListUnmanaged(u8),

    /// Heap generated strings, stable pointers unlike `srcGen`.
    strs: std.ArrayListUnmanaged([]const u8),

    /// Optionally parsed by tokenizer.
    comments: std.ArrayListUnmanaged(cy.IndexSlice(u32)),

    pub fn init(alloc: std.mem.Allocator, src: []const u8) !Ast {
        var ast = Ast{
            .src = src,
            .nodes = .{},
            .srcGen = .{},
            .strs = .{},
            .comments = .{},
        };
        try ast.clearNodes(alloc);
        return ast;
    }

    pub fn deinit(self: *Ast, alloc: std.mem.Allocator) void {
        self.nodes.deinit(alloc);
        self.srcGen.deinit(alloc);
        for (self.strs.items) |str| {
            alloc.free(str);
        }
        self.strs.deinit(alloc);
        self.comments.deinit(alloc);
    }

    pub fn clearNodes(self: *Ast, alloc: std.mem.Allocator) !void {
        self.nodes.clearRetainingCapacity();
        // Insert dummy for cy.NullNode.
        try self.nodes.append(alloc, .{
            .head = .{ .type = .null, .data = undefined },
            .data = undefined,
            .srcPos = 0,
        });
    }

    pub fn view(self: *const Ast) AstView {
        return .{
            .src = self.src,
            .srcGen = self.srcGen.items,
            .nodes = self.nodes.items,
        };
    }

    pub fn pushNode(self: *Ast, alloc: std.mem.Allocator, node_t: cy.NodeType, srcPos: u32) !NodeId {
        const id = self.nodes.items.len;
        try self.nodes.append(alloc, .{
            .head = .{
                .type = node_t,
                .data = .{ .next = cy.NullNode },
            }, 
            .data = .{ .uninit = {} },
            .srcPos = srcPos,
        });
        return @intCast(id);
    }

    pub fn genSpanNode(self: *Ast, alloc: std.mem.Allocator, node_t: cy.NodeType, str: []const u8, srcPos: ?u32) !NodeId {
        const pos = self.srcGen.items.len;
        try self.srcGen.appendSlice(alloc, str);
        const id = self.nodes.items.len;
        try self.nodes.append(alloc, .{
            .head = .{
                .type = node_t,
                .data = .{ .next = cy.NullNode },
            }, 
            .data = .{ .span = .{
                .pos = @intCast(pos),
                .len = @intCast(str.len),
                .srcGen = true,
            }},
            .srcPos = srcPos orelse cy.NullId,
        });
        return @intCast(id);
    }

    pub fn pushSpanNode(self: *Ast, alloc: std.mem.Allocator, node_t: cy.NodeType, srcPos: u32, srcEnd: u32) !NodeId {
        const id = self.nodes.items.len;
        try self.nodes.append(alloc, .{
            .head = .{
                .type = node_t,
                .data = .{ .next = cy.NullNode },
            }, 
            .data = .{ .span = .{
                .pos = srcPos,
                .len = @intCast(srcEnd-srcPos),
                .srcGen = false,
            }},
            .srcPos = srcPos,
        });
        return @intCast(id);
    }

    pub fn setNodeData(self: *Ast, id: NodeId, data: NodeData) void {
        self.nodes.items[id].data = data;
    }

    pub fn setNextNode(self: *Ast, id: NodeId, next: NodeId) void {
        self.nodes.items[id].head.data = .{ .next = @intCast(next) };
    }
    
    pub fn node(self: Ast, nodeId: NodeId) Node {
        return self.nodes.items[nodeId];
    }

    pub fn nodePtr(self: Ast, nodeId: NodeId) *Node {
        return &self.nodes.items[nodeId];
    }

    pub fn nodeType(self: Ast, nodeId: NodeId) NodeType {
        return self.nodes.items[nodeId].head.type;
    }

    pub fn nodePos(self: Ast, nodeId: NodeId) u32 {
        return self.nodes.items[nodeId].srcPos;
    }
    
    pub fn nodeString(self: Ast, n: Node) []const u8 {
        if (n.data.span.srcGen) {
            return self.srcGen.items[n.data.span.pos..n.data.span.pos+n.data.span.len];
        } else {
            return self.src[n.data.span.pos..n.data.span.pos+n.data.span.len];
        }
    }
};

pub const AstView = struct {
    src: []const u8,
    srcGen: []const u8,
    nodes: []const Node,

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

    pub fn node(self: AstView, nodeId: NodeId) Node {
        return self.nodes[nodeId];
    }

    pub fn nodeType(self: AstView, nodeId: NodeId) NodeType {
        return self.nodes[nodeId].head.type;
    }

    pub fn nodePos(self: AstView, nodeId: NodeId) u32 {
        return self.nodes[nodeId].srcPos;
    }

    pub fn nodeStringById(self: AstView, nodeId: cy.NodeId) []const u8 {
        return self.nodeString(self.nodes[nodeId]);
    }

    pub fn nodeString(self: AstView, n: cy.Node) []const u8 {
        if (n.data.span.srcGen) {
            return self.srcGen[n.data.span.pos..n.data.span.pos+n.data.span.len];
        } else {
            return self.src[n.data.span.pos..n.data.span.pos+n.data.span.len];
        }
    }

    pub fn nodeStringAndDelim(self: AstView, n: cy.Node) []const u8 {
        if (n.data.span.srcGen) {
            return self.srcGen[n.data.span.pos-1..n.data.span.pos+n.data.span.len+1];
        } else {
            return self.src[n.data.span.pos-1..n.data.span.pos+n.data.span.len+1];
        }
    }

    const NamePathInfo = struct {
        namePath: []const u8,
        lastName: []const u8,
        lastId: cy.NodeId,
    };

    pub fn getNamePathInfo(self: AstView, n: cy.Node, nameId: cy.NodeId) NamePathInfo {
        if (n.next() == cy.NullNode) {
            const name = self.nodeString(n);
            return .{
                .namePath = name,
                .lastName = name,
                .lastId = nameId,
            };
        } else {
            const lastId = self.getLastNameNode(n.next());
            const last = self.nodes[lastId];
            const lastName = self.nodeString(last);

            var end = last.srcPos + last.data.span.len;
            log.tracev("{}", .{last.type()});
            if (last.type() == .stringLit) {
                end += 1;
            }
            return .{
                .namePath = self.src[n.srcPos..end],
                .lastName = lastName,
                .lastId = lastId,
            };
        }
    }

    pub fn getNamePathInfoById(self: AstView, nameId: cy.NodeId) NamePathInfo {
        const n = self.nodes[nameId];
        return self.getNamePathInfo(n, nameId);
    }

    pub fn getLastNameNode(self: AstView, nameId: cy.NodeId) cy.NodeId {
        var name = self.nodes[nameId];
        var curId = nameId;
        while (name.next() != cy.NullNode) {
            name = self.nodes[name.next()];
            curId = name.next();
        }
        return curId;
    }

    // Returns whether two lines are connected by a new line and optional indentation.
    pub fn isAdjacentLine(self: AstView, aEnd: u32, bStart: u32) bool {
        var i = aEnd;
        if (self.src[i] == '\r') {
            i += 1;
            if (self.src[i] != '\n') {
                return false;
            }
            i += 1;
        } else if (self.src[i] == '\n') {
            i += 1;
        } else {
            return false;
        }
        while (i < bStart) {
            if (self.src[i] != ' ' and self.src[i] != '\t') {
                return false;
            }
            i += 1;
        }
        return true;
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
    ast: AstView,
    eventHandler: ?*const fn (Encoder, EncodeEvent, cy.NodeId) void = null,

    pub fn formatNode(self: Encoder, nodeId: cy.NodeId, buf: []u8) ![]const u8 {
        if (nodeId == cy.NullNode) {
            return "";
        }
        var fbuf = std.io.fixedBufferStream(buf);
        try self.writeNode(fbuf.writer(), nodeId);
        return fbuf.getWritten();
    }

    pub fn writeNode(self: Encoder, w: anytype, nodeId: cy.NodeId) !void {
        const node = self.ast.node(nodeId);
        switch (node.type()) {
            .funcDecl => {
                const header = self.ast.node(node.data.func.header);
                try w.writeAll("func ");
                try self.writeNode(w, header.data.funcHeader.name);
                try w.writeAll("(");
                var paramId = header.data.funcHeader.paramHead;
                if (paramId != cy.NullNode) {
                    try self.writeNode(w, paramId);
                    paramId = self.ast.node(paramId).next();

                    while (paramId != cy.NullNode) {
                        try w.writeAll(", ");
                        try self.writeNode(w, paramId);
                        paramId = self.ast.node(paramId).next();
                    }
                }
                try w.writeAll(")");
                if (header.funcHeader_ret() != cy.NullNode) {
                    try w.writeAll(" ");
                    try self.writeNode(w, header.funcHeader_ret());
                }
                // node.data.func.bodyHead
            },
            .funcParam => {
                const param = self.ast.node(nodeId);
                try self.writeNode(w, param.data.funcParam.name);
                if (param.data.funcParam.typeSpecHead != cy.NullNode) {
                    try w.writeAll(" ");
                    try self.writeNode(w, param.data.funcParam.typeSpecHead);
                }
            },
            .assignStmt => {
                try self.writeNode(w, node.data.assignStmt.left);
                try w.writeByte('=');
                try self.writeNode(w, node.data.assignStmt.right);
            },
            .opAssignStmt => {
                try self.writeNode(w, node.data.opAssignStmt.left);
                try w.writeAll(getBinOpStr(node.data.opAssignStmt.op));
                try w.writeByte('=');
                try self.writeNode(w, node.data.opAssignStmt.right);
            },
            .unary_expr => {
                const op = node.data.unary.op;
                try w.writeAll(getUnOpStr(op));
                try self.writeNode(w, node.data.unary.child);
            },
            .binExpr => {
                try self.writeNode(w, node.data.binExpr.left);
                try w.writeAll(getBinOpStr(node.data.binExpr.op));
                try self.writeNode(w, node.data.binExpr.right);
            },
            .exprStmt => {
                try self.writeNode(w, node.data.exprStmt.child);
            },
            .condExpr => {
                const ifBranch = self.ast.node(node.data.condExpr.ifBranch);
                try self.writeNode(w, ifBranch.data.ifBranch.cond);
                try w.writeAll("?");
                try self.writeNode(w, ifBranch.data.ifBranch.bodyHead);
                try w.writeAll(" else ");
                try self.writeNode(w, node.data.condExpr.elseExpr);
            },
            .caseBlock => {
                if (node.data.caseBlock.header == cy.NullNode) {
                    try w.writeAll("else");
                } else {
                    const header = self.ast.node(node.data.caseBlock.header);
                    var cond = header.data.caseHeader.condHead;
                    try self.writeNode(w, cond);
                    cond = self.ast.node(cond).next();
                    while (cond != cy.NullNode) {
                        try w.writeByte(',');
                        try self.writeNode(w, cond);
                        cond = self.ast.node(cond).next();
                    }
                }
                if (node.data.caseBlock.bodyIsExpr) {
                    try w.writeAll("=>");
                    try self.writeNode(w, node.data.caseBlock.bodyHead);
                } else {
                    try w.writeAll(": ...");
                }
            },
            .noneLit => {
                try w.writeAll("none");
            },
            .falseLit => {
                try w.writeAll("false");
            },
            .trueLit => {
                try w.writeAll("true");
            },
            .errorSymLit => {
                try w.writeAll("error.");
                try self.writeNode(w, node.data.errorSymLit.symbol);
            },
            .symbolLit => {
                try w.writeAll(".");
                try w.writeAll(self.ast.nodeString(node));
            },
            .hexLit,
            .binLit,
            .octLit,
            .decLit,
            .ident => {
                try w.writeAll(self.ast.nodeString(node));
            },
            .stringLit => {
                try w.writeAll(self.ast.nodeStringAndDelim(node));
            },
            .accessExpr => {
                try self.writeNode(w, node.data.accessExpr.left);
                try w.writeByte('.');
                try self.writeNode(w, node.data.accessExpr.right);
            },
            .group => {
                try w.writeByte('(');
                try self.writeNode(w, node.data.group.child);
                try w.writeByte(')');
            },
            .sliceExpr => {
                try self.writeNode(w, node.data.sliceExpr.arr);

                const range = self.ast.node(node.data.sliceExpr.range);
                try w.writeByte('[');
                if (range.data.range.start != cy.NullNode) {
                    try self.writeNode(w, range.data.range.start);
                }
                try w.writeAll("..");
                if (range.data.range.end != cy.NullNode) {
                    try self.writeNode(w, range.data.range.end);
                }
                try w.writeByte(']');
            },
            .indexExpr => {
                try self.writeNode(w, node.data.indexExpr.left);
                try w.writeByte('[');
                try self.writeNode(w, node.data.indexExpr.right);
                try w.writeByte(']');
            },
            .throwExpr => {
                try w.writeAll("throw ");
                try self.writeNode(w, node.data.throwExpr.child);
            },
            .callExpr => {
                try self.writeNode(w, node.data.callExpr.callee);

                try w.writeByte('(');
                if (node.data.callExpr.numArgs > 0) {
                    var argId: cy.NodeId = node.data.callExpr.argHead;
                    try self.writeNode(w, argId);
                    argId = self.ast.node(argId).next();

                    while (argId != cy.NullNode) {
                        try w.writeAll(", ");
                        try self.writeNode(w, argId);
                        argId = self.ast.node(argId).next();
                    }
                }
                try w.writeByte(')');
            },
            .tryExpr => {
                try w.writeAll("try ");
                try self.writeNode(w, node.data.tryExpr.expr);
                if (node.data.tryExpr.catchExpr != cy.NullNode) {
                    try w.writeAll(" catch ");
                    try self.writeNode(w, node.data.tryExpr.catchExpr);
                }
            },
            .localDecl => {
                if (node.data.localDecl.typed) {
                    try w.writeAll("var ");
                } else {
                    try w.writeAll("my ");
                }
                try self.writeNode(w, node.data.localDecl.varSpec);
                try w.writeByte('=');
                try self.writeNode(w, node.data.localDecl.right);
            },
            .arrayLit => {
                try w.writeByte('[');
                try w.writeAll("...");
                try w.writeByte(']');
            },
            .recordLit => {
                try w.writeByte('[');
                try w.writeAll("...");
                try w.writeByte(']');
            },
            .objectInit => {
                try w.writeByte('[');
                try self.writeNode(w, node.data.objectInit.name);
                try w.writeAll(" ...]");
            },
            .varSpec => {
                try self.writeNode(w, node.data.varSpec.name);
                if (node.data.varSpec.typeSpecHead != cy.NullNode) {
                    var cur = node.data.varSpec.typeSpecHead;
                    try self.writeNode(w, cur);
                    cur = self.ast.node(cur).next();
                    while (cur != cy.NullNode) {
                        try w.writeByte('.');
                        try self.writeNode(w, cur);
                        cur = self.ast.node(cur).next();
                    }
                }
            },
            else => {
                try w.writeByte('<');
                try w.writeAll(@tagName(node.type()));
                try w.writeByte('>');
            },
        }
    }
};