const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const fatal = cy.fatal;
const cy = @import("../cyber.zig");
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const bindings = @import("bindings.zig");
const Symbol = bindings.Symbol;
const prepareThrowSymbol = bindings.prepareThrowSymbol;
const fmt = @import("../fmt.zig");
const rt = cy.rt;
const bt = cy.types.BuiltinTypeSymIds;
const vmc = cy.vmc;

const log = cy.log.scoped(.core);

pub const Src = @embedFile("builtins.cy");
pub fn funcLoader(_: *cy.UserVM, func: cy.HostFuncInfo, out: *cy.HostFuncResult) callconv(.C) bool {
    if (std.mem.eql(u8, funcs[func.idx].@"0", func.name.slice())) {
        out.ptr = @ptrCast(@alignCast(funcs[func.idx].@"1"));
        out.type = funcs[func.idx].@"2";
        return true;
    }
    return false;
}

const NameFunc = struct { []const u8, ?*const anyopaque, cy.HostFuncType };
const funcs = [_]NameFunc{
    // Utils.
    .{"arrayFill", arrayFill, .standard},
    .{"copy", copy, .standard},
    .{"dump", dump, .standard},
    .{"errorReport", errorReport, .standard},
    .{"isAlpha", isAlpha, .standard},
    .{"isDigit", isDigit, .standard},
    .{"must", must, .standard},
    .{"panic", panic, .standard},
    .{"parseCyber", parseCyber, .standard},
    .{"parseCyon", parseCyon, .standard},
    .{"performGC", performGC, .standard},
    .{"print", print, .standard},
    .{"runestr", runestr, .standard},
    .{"toCyon", toCyon, .standard},
    .{"typeof", typeof, .standard},
    .{"typesym", typesym, .standard},

    // boolean
    .{"$call", bindings.booleanCall, .standard},

    // error
    .{"$call", bindings.errorCall, .standard},
    .{"value", bindings.errorValue, .standard},

    // int
    .{"$call", bindings.integerCall, .standard},
    .{"$prefix~", bindings.intBitwiseNot, .quicken},
    .{"$prefix-", bindings.intNeg, .quicken},
    // Inlined opcodes allow the right arg to be dynamic so the compiler can gen more of those.
    // So for now, the runtime signature reflects that.
    .{"$infix<", bindings.inlineBinOp(.lessInt), .quicken},
    .{"$infix<=", bindings.inlineBinOp(.lessEqualInt), .quicken},
    .{"$infix>", bindings.inlineBinOp(.greaterInt), .quicken},
    .{"$infix>=", bindings.inlineBinOp(.greaterEqualInt), .quicken},
    .{"$infix+", bindings.inlineBinOp(.addInt), .quicken},
    .{"$infix-", bindings.inlineBinOp(.subInt), .quicken},
    .{"$infix*", bindings.inlineBinOp(.mulInt), .quicken},
    .{"$infix/", bindings.inlineBinOp(.divInt), .quicken},
    .{"$infix%", bindings.inlineBinOp(.modInt), .quicken},
    .{"$infix^", bindings.inlineBinOp(.powInt), .quicken},
    .{"$infix&", bindings.inlineBinOp(.bitwiseAnd), .quicken},
    .{"$infix|", bindings.inlineBinOp(.bitwiseOr), .quicken},
    .{"$infix||", bindings.inlineBinOp(.bitwiseXor), .quicken},
    .{"$infix<<", bindings.inlineBinOp(.bitwiseLeftShift), .quicken},
    .{"$infix>>", bindings.inlineBinOp(.bitwiseRightShift), .quicken},

    // float
    .{"$call", bindings.floatCall, .standard},
    .{"$prefix-", bindings.floatNeg, .quicken},
    .{"$infix<", bindings.inlineBinOp(.lessFloat), .quicken},
    .{"$infix<=", bindings.inlineBinOp(.lessEqualFloat), .quicken},
    .{"$infix>", bindings.inlineBinOp(.greaterFloat), .quicken},
    .{"$infix>=", bindings.inlineBinOp(.greaterEqualFloat), .quicken},
    .{"$infix+", bindings.inlineBinOp(.addFloat), .quicken},
    .{"$infix-", bindings.inlineBinOp(.subFloat), .quicken},
    .{"$infix*", bindings.inlineBinOp(.mulFloat), .quicken},
    .{"$infix/", bindings.inlineBinOp(.divFloat), .quicken},
    .{"$infix%", bindings.inlineBinOp(.modFloat), .quicken},
    .{"$infix^", bindings.inlineBinOp(.powFloat), .quicken},

    // List
    .{"$index", bindings.inlineBinOp(.indexList), .quicken},
    .{"$setIndex", bindings.inlineTernNoRetOp(.setIndexList), .quicken},
    .{"add", bindings.listAdd, .standard},
    .{"append", bindings.listAppend, .standard},
    .{"concat", bindings.listConcat, .standard},
    .{"insert", bindings.listInsert, .standard},
    .{"iterator", bindings.listIterator, .standard},
    .{"joinString", bindings.listJoinString, .standard},
    .{"len", bindings.listLen, .standard},
    .{"seqIterator", bindings.listIterator, .standard},
    .{"remove", bindings.listRemove, .standard},
    .{"resize", bindings.listResize, .standard},
    .{"sort", bindings.listSort, .standard},

    // ListIterator
    .{"next", bindings.listIteratorNext, .standard},
    .{"nextSeq", bindings.listIteratorNextSeq, .standard},

    // Map
    .{"$index", bindings.inlineBinOp(.indexMap), .quicken},
    .{"$setIndex", bindings.inlineTernNoRetOp(.setIndexMap), .quicken},
    .{"remove", bindings.mapRemove, .standard},
    .{"size", bindings.mapSize, .standard},
    .{"iterator", bindings.mapIterator, .standard},
    .{"seqIterator", bindings.mapIterator, .standard},

    // MapIterator
    .{"next", bindings.mapIteratorNext, .standard},
    .{"nextSeq", bindings.mapIteratorNextSeq, .standard},

    // pointer
    .{"$call", bindings.pointerCall, .standard},
    .{"value", bindings.pointerValue, .standard},
};

const NameType = struct { []const u8, cy.rt.TypeId, cy.types.TypeId };
const types = [_]NameType{
    .{"boolean", rt.BooleanT, bt.Boolean },
    .{"error", rt.ErrorT, bt.Error },
    .{"int", rt.IntegerT, bt.Integer },
    .{"float", rt.FloatT, bt.Float },
    .{"List", rt.ListT, bt.List },
    .{"ListIterator", rt.ListIteratorT, bt.ListIter },
    .{"Map", rt.MapT, bt.Map },
    .{"MapIterator", rt.MapIteratorT, bt.MapIter },
    .{"pointer", rt.PointerT, bt.Pointer },
};

pub fn typeLoader(_: *cy.UserVM, info: cy.HostTypeInfo, out: *cy.HostTypeResult) callconv(.C) bool {
    if (std.mem.eql(u8, types[info.idx].@"0", info.name.slice())) {
        out.type = .coreObject;
        out.data.coreObject = .{
            .typeId = types[info.idx].@"1",
            .semaTypeId = types[info.idx].@"2",
        };
        return true;
    }
    return false;
}

pub fn arrayFill(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return vm.allocListFill(args[0], @intCast(args[1].asInteger())) catch cy.fatal();
}

pub fn copy(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = args[0];
    return cy.value.shallowCopy(@ptrCast(@alignCast(vm)), val);
}

pub fn errorReport(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const ivm = vm.internal();

    cy.debug.buildStackTrace(ivm) catch |err| {
        log.debug("unexpected {}", .{err});
        fatal();
    };

    var frames = ivm.alloc.alloc(cy.StackFrame, ivm.stackTrace.frames.len-1 + ivm.throwTrace.len) catch fatal();
    defer ivm.alloc.free(frames);
    for (ivm.throwTrace.items(), 0..) |tframe, i| {
        const frame = cy.debug.compactToStackFrame(ivm, tframe) catch fatal();
        frames[i] = frame;
    }
    // Skip the current callsite frame.
    for (ivm.stackTrace.frames[1..], 0..) |frame, i| {
        frames[ivm.throwTrace.len + i] = frame;
    }

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(ivm.alloc);

    const w = buf.writer(ivm.alloc);
    cy.debug.writeStackFrames(ivm, w, frames) catch fatal();

    return vm.allocStringInfer(buf.items) catch fatal();
}

pub fn must(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    if (!args[0].isError()) {
        return args[0];
    } else {
        return panic(vm, args, nargs);
    }
}

pub fn panic(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueToTempString(args[0]);
    return vm.returnPanic(str);
}

pub fn isAlpha(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const num = args[0].asInteger();
    if (num < 0 or num >= 2 << 21) {
        return prepareThrowSymbol(vm, .InvalidRune);
    }
    if (num > 255) {
        return Value.False;
    } else {
        return Value.initBool(std.ascii.isAlphabetic(@intCast(num)));
    }
}

pub fn isDigit(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const num = args[0].asInteger();
    if (num < 0 or num >= 2 << 21) {
        return prepareThrowSymbol(vm, .InvalidRune);
    }
    if (num > 255) {
        return Value.False;
    } else {
        return Value.initBool(std.ascii.isDigit(@intCast(num)));
    }
}

pub fn runestr(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const num = args[0].asInteger();
    if (num < 0 or num >= 2 << 21) {
        return prepareThrowSymbol(vm, .InvalidRune);
    }
    const rune: u21 = @intCast(num);
    if (std.unicode.utf8ValidCodepoint(rune)) {
        var buf: [4]u8 = undefined;
        const len = std.unicode.utf8Encode(rune, &buf) catch fatal();
        return vm.allocStringInfer(buf[0..len]) catch fatal();
    } else {
        return prepareThrowSymbol(vm, .InvalidRune);
    }
}

pub fn dump(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const alloc = vm.allocator();
    const res = allocToCyon(vm, alloc, args[0]) catch cy.fatal();
    defer alloc.free(res);
    vm.internal().print(vm, cy.Str.initSlice(res));
    return Value.None;
}

pub fn toCyon(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const alloc = vm.allocator();
    const res = allocToCyon(vm, alloc, args[0]) catch cy.fatal();
    defer alloc.free(res);
    return vm.allocStringInfer(res) catch fatal();
}

fn allocToCyon(vm: *cy.UserVM, alloc: std.mem.Allocator, root: Value) ![]const u8 {
    const S = struct {
        fn encodeMap(ctx: *cy.EncodeMapContext, val: cy.Value) anyerror!void {
            const uservm = cy.ptrAlignCast(*cy.UserVM, ctx.user_ctx);
            var iter = val.asHeapObject().map.map().iterator();
            while (iter.next()) |e| {
                const key = uservm.valueToTempString(e.key);
                switch (e.value.getUserTag()) {
                    .float => {
                        try ctx.encodeFloat(key, e.value.asF64());
                    },
                    .int => {
                        try ctx.encodeInt(key, e.value.asInteger());
                    },
                    .string => {
                        const keyDupe = try uservm.allocator().dupe(u8, key);
                        defer uservm.allocator().free(keyDupe);
                        const str = uservm.valueToTempString(e.value);
                        try ctx.encodeString(keyDupe, str);
                    },
                    .boolean => {
                        try ctx.encodeBool(key, e.value.asBool());
                    },
                    .map => {
                        try ctx.encodeMap(key, e.value, encodeMap);
                    },
                    .list => {
                        try ctx.encodeList(key, e.value, encodeList);
                    },
                    else => {},
                }
            }
        }
        fn encodeList(ctx: *cy.EncodeListContext, val: cy.Value) anyerror!void {
            const items = val.asHeapObject().list.items();
            for (items) |it| {
                switch (it.getUserTag()) {
                    .float => {
                        try ctx.encodeFloat(it.asF64());
                    },
                    .int => {
                        try ctx.encodeInt(it.asInteger());
                    },
                    .string => {
                        const uservm = cy.ptrAlignCast(*cy.UserVM, ctx.user_ctx);
                        const str = uservm.valueToTempString(it);
                        try ctx.encodeString(str);
                    },
                    .boolean => {
                        try ctx.encodeBool(it.asBool());
                    },
                    .map => {
                        try ctx.encodeMap(it, encodeMap);
                    },
                    .list => {
                        try ctx.encodeList(it, encodeList);
                    },
                    else => {},
                }
            }
        }
        fn encodeRoot(ctx: *cy.EncodeValueContext, val: anytype) !void {
            const T = @TypeOf(val);
            if (T == Value) {
                switch (val.getUserTag()) {
                    .float => {
                        try ctx.encodeFloat(val.asF64());
                    },
                    .int => {
                        try ctx.encodeInt(val.asInteger());
                    },
                    .string => {
                        const uservm = cy.ptrAlignCast(*cy.UserVM, ctx.user_ctx);
                        const str = uservm.valueToTempString(val);
                        try ctx.encodeString(str);
                    },
                    .boolean => {
                        try ctx.encodeBool(val.asBool());
                    },
                    .list => {
                        if (val.asHeapObject().list.items().len == 0) {
                            _ = try ctx.writer.write("[]");
                        } else {
                            try ctx.encodeList(val, encodeList);
                        }
                    },
                    .map => {
                        if (val.asHeapObject().map.inner.size == 0) {
                            _ = try ctx.writer.write("{}");
                        } else {
                            try ctx.encodeMap(val, encodeMap);
                        }
                    },
                    else => {},
                }
            } else {
                cy.panicFmt("unsupported: {s}", .{@typeName(T)});
            }
        }
    };
    return try cy.encodeCyon(alloc, vm, root, S.encodeRoot);
}

pub fn parseCyber(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const src = vm.valueToTempRawString(args[0]);

    const alloc = vm.allocator();
    var parser = cy.Parser.init(alloc);
    parser.parseComments = true;
    defer parser.deinit();
    _ = parser.parse(src) catch |err| {
        log.debug("parseCyber: {}", .{err});
        return prepareThrowSymbol(vm, .UnknownError);
    };

    return parseCyberGenResult(vm, &parser) catch |err| {
        log.debug("parseCyber: {}", .{err});
        return prepareThrowSymbol(vm, .UnknownError);
    };
}

const ParseCyberState = struct {
    sb: std.ArrayListUnmanaged(u8),
    commentIdx: u32,
    pos: u32,
    node: cy.Node,
};

fn genTypeSpecString(vm: *cy.UserVM, parser: *const cy.Parser, headId: cy.NodeId) !cy.Value {
    const alloc = vm.allocator();
    if (headId != cy.NullId) {
        var sb: std.ArrayListUnmanaged(u8) = .{};
        defer sb.deinit(alloc);

        var name = cy.parser.getNodeTokenString(parser, headId);
        try sb.appendSlice(alloc, name);

        var curId = parser.nodes.items[headId].next;
        while (curId != cy.NullId) {
            try sb.append(alloc, '.');
            name = cy.parser.getNodeTokenString(parser, curId);
            try sb.appendSlice(alloc, name);
            curId = parser.nodes.items[curId].next;
        }

        return try vm.allocAstring(sb.items);
    } else {
        return try vm.allocAstring("");
    }
}

fn genNodeValue(vm: *cy.UserVM, parser: *const cy.Parser, nodeId: cy.NodeId) !cy.Value {
    const node = parser.nodes.items[nodeId];
    const res = try vm.allocEmptyMap();
    switch (node.node_t) {
        .funcHeader => {
            const name = cy.parser.getNodeTokenString(parser, node.head.funcHeader.name);
            try vm.mapRawSet(res, try vm.allocAstring("name"), try vm.allocStringInfer(name));

            const params = try vm.allocEmptyList();
            var paramId = node.head.funcHeader.paramHead;
            while (paramId != cy.NullId) {
                const param = try genNodeValue(vm, parser, paramId);
                try vm.listAppend(params, param);
                paramId = parser.nodes.items[paramId].next;
            }
            try vm.mapRawSet(res, try vm.allocAstring("params"), params);
        },
        .funcParam => {
            var name = cy.parser.getNodeTokenString(parser, node.head.funcParam.name);
            try vm.mapRawSet(res, try vm.allocAstring("name"), try vm.allocStringInfer(name));

            const typeSpec = try genTypeSpecString(vm, parser, node.head.funcParam.typeSpecHead);
            try vm.mapRawSet(res, try vm.allocAstring("typeSpec"), typeSpec);
        },
        else => {},
    }
    return res;
}

fn genDeclEntry(vm: *cy.UserVM, parser: *const cy.Parser, decl: cy.parser.StaticDecl, state: *ParseCyberState) !Value {
    const alloc = vm.allocator();
    const nodes = parser.nodes.items;
    const tokens = parser.tokens.items;
    const entry = try vm.allocEmptyMap();
    try vm.mapRawSet(entry, try vm.allocAstring("type"), try vm.allocAstring(@tagName(decl.declT)));
    var name: []const u8 = undefined;
    var node: cy.Node = undefined;
    switch (decl.declT) {
        .variable => {
            node = nodes[decl.inner.variable];
            const varSpec = nodes[node.head.staticDecl.varSpec];
            name = cy.parser.getNodeTokenString(parser, varSpec.head.varSpec.name);

            const typeSpec = try genTypeSpecString(vm, parser, varSpec.head.varSpec.typeSpecHead);
            try vm.mapRawSet(entry, try vm.allocAstring("typeSpec"), typeSpec);
        },
        .typeAlias => {
            node = nodes[decl.inner.typeAlias];
            name = cy.parser.getNodeTokenString(parser, node.head.typeAliasDecl.name);
        },
        .func => {
            node = nodes[decl.inner.func];
            const header = nodes[node.head.func.header];
            name = cy.parser.getNodeTokenString(parser, header.head.funcHeader.name);
        },
        .funcInit => {
            node = nodes[decl.inner.funcInit];
            const header = nodes[node.head.func.header];
            name = cy.parser.getNodeTokenString(parser, header.head.funcHeader.name);

            const headerv = try genNodeValue(vm, parser, node.head.func.header);
            try vm.mapRawSet(entry, try vm.allocAstring("header"), headerv);
        },
        .import => {
            node = nodes[decl.inner.import];
            name = cy.parser.getNodeTokenString(parser, node.head.left_right.left);
        },
        .object => {
            node = nodes[decl.inner.object];
            name = cy.parser.getNodeTokenString(parser, node.head.objectDecl.name);

            const childrenv = try vm.allocEmptyList();
            const children = childrenv.asHeapObject().list.getList();

            const body = nodes[node.head.objectDecl.body];
            var funcId = body.head.objectDeclBody.funcsHead;
            while (funcId != cy.NullId) {
                const funcN = nodes[funcId];
                var childDecl: cy.parser.StaticDecl = undefined;
                switch (funcN.node_t) {
                    .funcDecl => childDecl = .{ .declT = .func, .inner = .{ .func = funcId }},
                    .funcDeclInit => childDecl = .{ .declT = .funcInit, .inner = .{ .funcInit = funcId }},
                    else => return error.Unsupported,
                }

                try children.append(alloc, try genDeclEntry(vm, parser, childDecl, state));
                funcId = funcN.next;
            }

            try vm.mapRawSet(entry, try vm.allocAstring("children"), childrenv);
        },
        .enumT => {
            node = nodes[decl.inner.object];
            name = cy.parser.getNodeTokenString(parser, node.head.enumDecl.name);
        }
    }
    const pos = tokens[node.start_token].pos();
    state.pos = pos;
    state.node = node;

    // Find doc comments.
    if (try genDocComment(vm, parser, decl, state)) |docStr| {
        try vm.mapRawSet(entry, try vm.allocAstring("docs"), docStr);
    }

    try vm.mapRawSet(entry, try vm.allocAstring("name"), try vm.allocAstring(name));
    try vm.mapRawSet(entry, try vm.allocAstring("pos"), Value.initF64(@floatFromInt(pos)));
    return entry;
}

fn genDocComment(vm: *cy.UserVM, parser: *const cy.Parser, decl: cy.parser.StaticDecl, state: *ParseCyberState) !?cy.Value {
    const alloc = vm.allocator();
    const nodes = parser.nodes.items;
    const tokens = parser.tokens.items;
    if (state.commentIdx < parser.comments.items.len) {
        var docStartIdx = state.commentIdx;
        var docEndIdx = state.commentIdx;
        while (state.commentIdx < parser.comments.items.len) {
            var commentPos = parser.comments.items[state.commentIdx];
            if (commentPos.start > state.pos) {
                break;
            }
            state.commentIdx += 1;
            docEndIdx = state.commentIdx;
            if (commentPos.len() < 3 or !std.mem.eql(u8, "--|", parser.src[commentPos.start..commentPos.start+3])) {
                // Not a doc comment, reset.
                docStartIdx = state.commentIdx;
                continue;
            }
            // Check it is connected to last comment.
            if (docEndIdx > docStartIdx + 1) {
                const last = parser.comments.items[docEndIdx - 2];
                if (!linesConnected(parser.src, last.end, commentPos.start)) {
                    // Reset.
                    docStartIdx = state.commentIdx;
                    continue;
                }
            }
        }
        if (docEndIdx > docStartIdx) {
            // Check it is connected to last comment.
            const last = parser.comments.items[docEndIdx - 1];

            var posWithModifiers = state.pos;
            switch (decl.declT) {
                .variable => {
                    const varSpec = nodes[state.node.head.staticDecl.varSpec];
                    if (varSpec.head.varSpec.modifierHead != cy.NullId) {
                        const modifier = nodes[varSpec.head.varSpec.modifierHead];
                        posWithModifiers = tokens[modifier.start_token].pos() - 1;
                    }
                },
                .funcInit,
                .func => {
                    const header = nodes[state.node.head.func.header];
                    if (header.next != cy.NullId) {
                        const modifier = nodes[header.next];
                        posWithModifiers = tokens[modifier.start_token].pos() - 1;
                    }
                },
                .object => {
                    if (state.node.head.objectDecl.modifierHead != cy.NullId) {
                        const modifier = nodes[state.node.head.objectDecl.modifierHead];
                        posWithModifiers = tokens[modifier.start_token].pos() - 1;
                    }
                },
                else => {},
            }

            if (linesConnected(parser.src, last.end, posWithModifiers)) {
                for (parser.comments.items[docStartIdx..docEndIdx]) |docPos| {
                    try state.sb.appendSlice(alloc, parser.src[docPos.start+3..docPos.end]);
                    try state.sb.append(alloc, ' ');
                }
                const finalStr = std.mem.trim(u8, state.sb.items, " ");
                defer state.sb.clearRetainingCapacity();
                return try vm.allocStringInfer(finalStr);
            }
        }
    }
    return null;
}

// Returns whether two lines are connected by a new line and indentation.
fn linesConnected(src: []const u8, aEnd: u32, bStart: u32) bool {
    var i = aEnd;
    if (src[i] == '\r') {
        i += 1;
        if (src[i] != '\n') {
            return false;
        }
        i += 1;
    } else if (src[i] == '\n') {
        i += 1;
    } else {
        return false;
    }
    while (i < bStart) {
        if (src[i] != ' ' and src[i] != '\t') {
            return false;
        }
        i += 1;
    }
    return true;
}

fn parseCyberGenResult(vm: *cy.UserVM, parser: *const cy.Parser) !Value {
    const alloc = vm.allocator();
    const root = try vm.allocEmptyMap();
    const map = root.asHeapObject().map.map();

    const decls = try vm.allocEmptyList();
    const declsList = decls.asHeapObject().list.getList();

    var state = ParseCyberState{
        .commentIdx = 0,
        .sb = .{},
        .pos = undefined,
        .node = undefined,
    };
    defer state.sb.deinit(alloc);

    for (parser.staticDecls.items) |decl| {
        const entry = try genDeclEntry(vm, parser, decl, &state);
        try declsList.append(alloc, entry);
    }
    try map.put(alloc, vm.internal(), try vm.allocAstring("decls"), decls);

    return root;
}

pub fn parseCyon(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const src = vm.valueToTempRawString(args[0]);
    
    const alloc = vm.allocator();
    var parser = cy.Parser.init(alloc);
    defer parser.deinit();
    const val = cy.decodeCyon(alloc, &parser, src) catch cy.fatal();
    return fromCyonValue(vm, val) catch cy.fatal();
}

fn fromCyonValue(self: *cy.UserVM, val: cy.DecodeValueIR) !Value {
    const ivm = self.internal();
    switch (val.getValueType()) {
        .list => {
            var dlist = val.getList() catch cy.fatal();
            defer dlist.deinit();
            const elems = try ivm.alloc.alloc(Value, dlist.arr.len);
            for (elems, 0..) |*elem, i| {
                elem.* = try fromCyonValue(self, dlist.getIndex(i));
            }
            return try cy.heap.allocOwnedList(ivm, elems);
        },
        .map => {
            var dmap = val.getMap() catch cy.fatal();
            defer dmap.deinit();
            var iter = dmap.iterator();

            const mapVal = try self.allocEmptyMap();
            const map = mapVal.asHeapObject();
            while (iter.next()) |entry| {
                const child = try fromCyonValue(self, dmap.getValue(entry.key_ptr.*));
                const key = try self.allocStringInfer(entry.key_ptr.*);
                stdMapPut(self, map, key, child);
            }
            return mapVal;
        },
        .string => {
            const str = try val.allocString();
            defer val.alloc.free(str);
            // TODO: Use allocOwnedString
            return try self.allocStringInfer(str);
        },
        .integer => {
            return Value.initInt(try val.getInt());
        },
        .float => {
            return Value.initF64(try val.getF64());
        },
        .boolean => {
            return Value.initBool(val.getBool());
        },
    }
}

fn stdMapPut(vm: *cy.UserVM, obj: *cy.HeapObject, key: Value, value: Value) void {
    const ivm = vm.internal();
    const map = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner); 
    map.put(vm.allocator(), ivm, key, value) catch cy.fatal();
}

pub fn performGC(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const res = cy.arc.performGC(vm.internal()) catch cy.fatal();
    const map = vm.allocEmptyMap() catch cy.fatal();
    const cycKey = vm.allocAstring("numCycFreed") catch cy.fatal();
    const objKey = vm.allocAstring("numObjFreed") catch cy.fatal();
    defer {
        vm.release(cycKey);
        vm.release(objKey);
    }
    map.asHeapObject().map.set(vm.internal(), cycKey, Value.initInt(@intCast(res.numCycFreed))) catch cy.fatal();
    map.asHeapObject().map.set(vm.internal(), objKey, Value.initInt(@intCast(res.numObjFreed))) catch cy.fatal();
    return map;
}

pub fn print(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueToTempRawString(args[0]);
    vm.internal().print(vm, cy.Str.initSlice(str));
    return Value.None;
}

pub fn typeof(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    const typeId = val.getTypeId();
    return cy.heap.allocMetaType(vm.internal(), @intFromEnum(cy.heap.MetaTypeKind.object), typeId) catch fatal();
}

pub fn typesym(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getUserTag()) {
        .float => return Value.initSymbol(@intFromEnum(Symbol.float)),
        .object => return Value.initSymbol(@intFromEnum(Symbol.object)),
        .err => return Value.initSymbol(@intFromEnum(Symbol.err)),
        .boolean => return Value.initSymbol(@intFromEnum(Symbol.boolean)),
        .map => return Value.initSymbol(@intFromEnum(Symbol.map)),
        .int => return Value.initSymbol(@intFromEnum(Symbol.int)),
        .list => return Value.initSymbol(@intFromEnum(Symbol.list)),
        .string => return Value.initSymbol(@intFromEnum(Symbol.string)),
        .rawstring => return Value.initSymbol(@intFromEnum(Symbol.rawstring)),
        .fiber => return Value.initSymbol(@intFromEnum(Symbol.fiber)),
        .nativeFunc,
        .closure,
        .lambda => return Value.initSymbol(@intFromEnum(Symbol.function)),
        .none => return Value.initSymbol(@intFromEnum(Symbol.none)),
        .symbol => return Value.initSymbol(@intFromEnum(Symbol.symbol)),
        .metatype => return Value.initSymbol(@intFromEnum(Symbol.metatype)),
        .pointer => return Value.initSymbol(@intFromEnum(Symbol.pointer)),
        else => fmt.panic("Unsupported {}", &.{fmt.v(val.getUserTag())}),
    }
}