const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const fatal = cy.fatal;
const cy = @import("../cyber.zig");
const cc = @import("../clib.zig");
const Value = cy.Value;
const bindings = @import("bindings.zig");
const Symbol = bindings.Symbol;
const prepareThrowSymbol = bindings.prepareThrowSymbol;
const fromUnsupportedError = bindings.fromUnsupportedError;
const fmt = @import("../fmt.zig");
const rt = cy.rt;
const bt = cy.types.BuiltinTypes;
const vmc = cy.vmc;
const string = @import("string.zig");

const log = cy.log.scoped(.core);

pub const Src = @embedFile("builtins.cy");
pub fn funcLoader(_: ?*cc.VM, func: cc.FuncInfo, out_: [*c]cc.FuncResult) callconv(.C) bool {
    const out: *cc.FuncResult = out_;
    const name = cc.strSlice(func.name);
    if (std.mem.eql(u8, funcs[func.idx].@"0", name)) {
        out.ptr = @ptrCast(@alignCast(funcs[func.idx].@"1"));
        out.type = @intFromEnum(funcs[func.idx].@"2");
        return true;
    }
    return false;
}

const NameFunc = struct { []const u8, ?*const anyopaque, cc.FuncEnumType };
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

    // bool
    .{"bool.'$call'", boolCall, .standard},

    // error
    .{"sym", errorSym, .standard},
    .{"error.'$call'", errorCall, .standard},

    // int
    .{"$prefix~", bindings.intBitwiseNot, .inlinec},
    .{"$prefix-", bindings.intNeg, .inlinec},
    // Inlined opcodes allow the right arg to be dynamic so the compiler can gen more of those.
    // So for now, the runtime signature reflects that.
    .{"$infix<", bindings.inlineBinOp(.lessInt), .inlinec},
    .{"$infix<=", bindings.inlineBinOp(.lessEqualInt), .inlinec},
    .{"$infix>", bindings.inlineBinOp(.greaterInt), .inlinec},
    .{"$infix>=", bindings.inlineBinOp(.greaterEqualInt), .inlinec},
    .{"$infix+", bindings.inlineBinOp(.addInt), .inlinec},
    .{"$infix-", bindings.inlineBinOp(.subInt), .inlinec},
    .{"$infix*", bindings.inlineBinOp(.mulInt), .inlinec},
    .{"$infix/", bindings.inlineBinOp(.divInt), .inlinec},
    .{"$infix%", bindings.inlineBinOp(.modInt), .inlinec},
    .{"$infix^", bindings.inlineBinOp(.powInt), .inlinec},
    .{"$infix&", bindings.inlineBinOp(.bitwiseAnd), .inlinec},
    .{"$infix|", bindings.inlineBinOp(.bitwiseOr), .inlinec},
    .{"$infix||", bindings.inlineBinOp(.bitwiseXor), .inlinec},
    .{"$infix<<", bindings.inlineBinOp(.bitwiseLeftShift), .inlinec},
    .{"$infix>>", bindings.inlineBinOp(.bitwiseRightShift), .inlinec},
    .{"int.'$call'", intCall, .standard},

    // float
    .{"$prefix-", bindings.floatNeg, .inlinec},
    .{"$infix<", bindings.inlineBinOp(.lessFloat), .inlinec},
    .{"$infix<=", bindings.inlineBinOp(.lessEqualFloat), .inlinec},
    .{"$infix>", bindings.inlineBinOp(.greaterFloat), .inlinec},
    .{"$infix>=", bindings.inlineBinOp(.greaterEqualFloat), .inlinec},
    .{"$infix+", bindings.inlineBinOp(.addFloat), .inlinec},
    .{"$infix-", bindings.inlineBinOp(.subFloat), .inlinec},
    .{"$infix*", bindings.inlineBinOp(.mulFloat), .inlinec},
    .{"$infix/", bindings.inlineBinOp(.divFloat), .inlinec},
    .{"$infix%", bindings.inlineBinOp(.modFloat), .inlinec},
    .{"$infix^", bindings.inlineBinOp(.powFloat), .inlinec},
    .{"float.'$call'", floatCall, .standard},

    // List
    .{"$index", bindings.inlineBinOp(.indexList), .inlinec},
    .{"$setIndex", bindings.inlineTernOp(.setIndexList), .inlinec},
    .{"append", bindings.listAppend, .standard},
    .{"concat", bindings.listConcat, .standard},
    .{"insert", bindings.listInsert, .standard},
    .{"iterator", bindings.listIterator, .standard},
    .{"joinString", bindings.listJoinString, .standard},
    .{"len", bindings.listLen, .standard},
    .{"remove", bindings.listRemove, .standard},
    .{"resize", bindings.listResize, .standard},
    // .{"sort", bindings.listSort, .standard},

    // ListIterator
    .{"next", bindings.listIteratorNext, .standard},

    // tuple
    .{"$index", bindings.inlineBinOp(.indexTuple), .inlinec},

    // Map
    .{"$index", bindings.inlineBinOp(.indexMap), .inlinec},
    .{"$setIndex", bindings.inlineTernOp(.setIndexMap), .inlinec},
    .{"remove", bindings.mapRemove, .standard},
    .{"size", bindings.mapSize, .standard},
    .{"iterator", bindings.mapIterator, .standard},

    // MapIterator
    .{"next", bindings.mapIteratorNext, .standard},

    // string
    .{"$infix+", string.concat, .standard},
    .{"concat", string.concat, .standard},
    .{"endsWith", string.endsWith, .standard},
    .{"find", string.find, .standard},
    .{"findAnyRune", string.findAnyRune, .standard},
    .{"findRune", string.findRune, .standard},
    .{"insert", string.insertFn, .standard},
    .{"isAscii", string.isAscii, .standard},
    .{"len", string.lenFn, .standard},
    .{"less", string.less, .standard},
    .{"lower", string.lower, .standard},
    .{"replace", string.replace, .standard},
    .{"repeat", string.repeat, .standard},
    .{"runeAt", string.runeAt, .standard},
    .{"slice", string.sliceFn, .standard},
    .{"$slice", string.sliceFn, .standard},
    .{"sliceAt", string.sliceAt, .standard},
    .{"$index", string.sliceAt, .standard},
    .{"split", string.split, .standard},
    .{"startsWith", string.startsWith, .standard},
    .{"trim", string.trim, .standard},
    .{"upper", string.upper, .standard},
    .{"string.'$call'", string.stringCall, .standard},

    // array
    .{"$infix+", arrayConcat, .standard},
    .{"byteAt", arrayByteAt, .standard},
    .{"concat", arrayConcat, .standard},
    .{"decode", arrayDecode, .standard},
    .{"decode", arrayDecode1, .standard},
    .{"endsWith", arrayEndsWith, .standard},
    .{"find", arrayFind, .standard},
    .{"findAnyByte", arrayFindAnyByte, .standard},
    .{"findByte", arrayFindByte, .standard},
    .{"insert", arrayInsert, .standard},
    .{"insertByte", arrayInsertByte, .standard},
    .{"len", arrayLen, .standard},
    .{"repeat", arrayRepeat, .standard},
    .{"replace", arrayReplace, .standard},
    .{"slice", arraySlice, .standard},
    .{"$slice", arraySlice, .standard},
    .{"$index", arrayByteAt, .standard},
    .{"split", arraySplit, .standard},
    .{"startsWith", arrayStartsWith, .standard},
    .{"trim", arrayTrim, .standard},
    .{"array.'$call'", arrayCall, .standard},

    // pointer
    .{"value", pointerValue, .standard},
    .{"writeAt", pointerWriteAt, .standard},
    .{"pointer.'$call'", pointerCall, .standard},

    // Fiber
    .{"status", fiberStatus, .standard},

    // metatype
    .{"id", metatypeId, .standard},
};

const NameType = struct { []const u8, cy.TypeId };
const types = [_]NameType{
    .{"bool", bt.Boolean },
    .{"error", bt.Error },
    .{"int", bt.Integer },
    .{"float", bt.Float },
    .{"List", bt.List },
    .{"ListIterator", bt.ListIter },
    .{"tuple", bt.Tuple },
    .{"Map", bt.Map },
    .{"MapIterator", bt.MapIter },
    .{"string", bt.String },
    .{"array", bt.Array },
    .{"pointer", bt.Pointer },
    .{"Fiber", bt.Fiber },
    .{"metatype", bt.MetaType },
};

pub fn typeLoader(_: ?*cc.VM, info: cc.TypeInfo, out_: [*c]cc.TypeResult) callconv(.C) bool {
    const out: *cc.TypeResult = out_;
    const name = cc.strSlice(info.name);
    if (std.mem.eql(u8, types[info.idx].@"0", name)) {
        out.type = cc.TypeKindCoreObject;
        out.data.coreObject = .{
            .typeId = types[info.idx].@"1",
        };
        return true;
    }
    return false;
}

pub fn onLoad(vm_: ?*cc.VM, mod: cc.ApiModule) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const b = bindings.ModuleBuilder.init(vm.compiler, @ptrCast(@alignCast(mod.sym)));
    if (cy.Trace) {
        b.declareFuncSig("traceRetains", &.{}, bt.Integer, traceRetains) catch cy.fatal();
        b.declareFuncSig("traceReleases", &.{}, bt.Integer, traceRetains) catch cy.fatal();
    }
}

pub fn zErrFunc(comptime func: fn (vm: *cy.UserVM, args: [*]const Value, nargs: u8) anyerror!Value) cy.ZHostFuncFn {
    const S = struct {
        pub fn genFunc(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
            return func(vm, args, nargs) catch |err| {
                return throwZError(vm, err, @errorReturnTrace());
            };
        }
    };
    return S.genFunc;
}

pub fn throwZError(vm: *cy.UserVM, err: anyerror, trace: ?*std.builtin.StackTrace) Value {
    if (builtin.mode == .Debug) {
        // std.debug.dumpStackTrace(trace.?.*);
    }
    switch (err) {
        error.InvalidArgument   => return prepareThrowSymbol(vm, .InvalidArgument),
        error.InvalidEnumTag    => return prepareThrowSymbol(vm, .InvalidArgument),
        error.FileNotFound      => return prepareThrowSymbol(vm, .FileNotFound),
        error.PermissionDenied  => return prepareThrowSymbol(vm, .PermissionDenied),
        else                    => return fromUnsupportedError(vm, "", err, trace),
    }
}

fn traceRetains(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return Value.initInt(vm.internal().trace.numRetains);
}

fn traceReleases(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return Value.initInt(vm.internal().trace.numReleases);
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

    return vm.retainOrAllocString(buf.items) catch fatal();
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
        return vm.retainOrAllocString(buf[0..len]) catch fatal();
    } else {
        return prepareThrowSymbol(vm, .InvalidRune);
    }
}

pub fn dump(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const alloc = vm.allocator();
    const res = allocToCyon(vm, alloc, args[0]) catch cy.fatal();
    defer alloc.free(res);
    vm.internal().print.?(@ptrCast(vm), cc.initStr(res));
    return Value.None;
}

pub fn toCyon(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const alloc = vm.allocator();
    const res = allocToCyon(vm, alloc, args[0]) catch cy.fatal();
    defer alloc.free(res);
    return vm.retainOrAllocString(res) catch fatal();
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
                    .bool => {
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
                try ctx.indent();
                switch (it.getUserTag()) {
                    .float => {
                        try ctx.encodeFloat(it.asF64());
                        _ = try ctx.writer.write(",\n");
                    },
                    .int => {
                        try ctx.encodeInt(it.asInteger());
                        _ = try ctx.writer.write(",\n");
                    },
                    .string => {
                        const uservm = cy.ptrAlignCast(*cy.UserVM, ctx.user_ctx);
                        const str = uservm.valueToTempString(it);
                        try ctx.encodeString(str);
                        _ = try ctx.writer.write(",\n");
                    },
                    .bool => {
                        try ctx.encodeBool(it.asBool());
                        _ = try ctx.writer.write(",\n");
                    },
                    .map => {
                        try ctx.encodeMap(it, encodeMap);
                        _ = try ctx.writer.write(",\n");
                    },
                    .list => {
                        try ctx.encodeList(it, encodeList);
                        _ = try ctx.writer.write(",\n");
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
                    .bool => {
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
                            _ = try ctx.writer.write("[:]");
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
    const src = args[0].asString();

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

        var name = cy.parser.getNodeString(parser, headId);
        try sb.appendSlice(alloc, name);

        var curId = parser.nodes.items[headId].next;
        while (curId != cy.NullId) {
            try sb.append(alloc, '.');
            name = cy.parser.getNodeString(parser, curId);
            try sb.appendSlice(alloc, name);
            curId = parser.nodes.items[curId].next;
        }

        return try vm.retainOrAllocAstring(sb.items);
    } else {
        return try vm.retainOrAllocAstring("");
    }
}

fn genNodeValue(vm: *cy.UserVM, parser: *const cy.Parser, ast: cy.ast.Source, nodeId: cy.NodeId) !cy.Value {
    const node = parser.nodes.items[nodeId];
    const res = try vm.allocEmptyMap();
    switch (node.node_t) {
        .funcHeader => {
            const name = ast.getNamePathStr(node.head.funcHeader.name);
            try vm.mapRawSet(res, try vm.retainOrAllocAstring("name"), try vm.retainOrAllocString(name));

            const params = try vm.allocEmptyList();
            var paramId = node.head.funcHeader.paramHead;
            while (paramId != cy.NullId) {
                const param = try genNodeValue(vm, parser, ast, paramId);
                try vm.listAppend(params, param);
                paramId = parser.nodes.items[paramId].next;
            }
            try vm.mapRawSet(res, try vm.retainOrAllocAstring("params"), params);

            const ret = try genTypeSpecString(vm, parser, node.head.funcHeader.ret);
            try vm.mapRawSet(res, try vm.retainOrAllocAstring("ret"), ret);
        },
        .funcParam => {
            var name = cy.parser.getNodeString(parser, node.head.funcParam.name);
            try vm.mapRawSet(res, try vm.retainOrAllocAstring("name"), try vm.retainOrAllocString(name));

            const typeSpec = try genTypeSpecString(vm, parser, node.head.funcParam.typeSpecHead);
            try vm.mapRawSet(res, try vm.retainOrAllocAstring("typeSpec"), typeSpec);
        },
        else => {},
    }
    return res;
}

fn genDeclEntry(vm: *cy.UserVM, parser: *const cy.Parser, ast: cy.ast.Source, decl: cy.parser.StaticDecl, state: *ParseCyberState) !Value {
    const alloc = vm.allocator();
    const nodes = parser.nodes.items;
    const tokens = parser.tokens.items;
    const entry = try vm.allocEmptyMap();
    try vm.mapRawSet(entry, try vm.retainOrAllocAstring("type"), try vm.retainOrAllocAstring(@tagName(decl.declT)));
    var name: []const u8 = undefined;
    var node = nodes[decl.nodeId];
    switch (decl.declT) {
        .variable => {
            const varSpec = nodes[node.head.staticDecl.varSpec];
            name = ast.getNamePathStr(varSpec.head.varSpec.name);

            const typeSpec = try genTypeSpecString(vm, parser, varSpec.head.varSpec.typeSpecHead);
            try vm.mapRawSet(entry, try vm.retainOrAllocAstring("typeSpec"), typeSpec);
        },
        .typeAlias => {
            name = cy.parser.getNodeString(parser, node.head.typeAliasDecl.name);
        },
        .func => {
            const header = nodes[node.head.func.header];
            name = cy.parser.getNodeString(parser, header.head.funcHeader.name);
        },
        .funcInit => {
            const header = nodes[node.head.func.header];
            name = ast.getNamePathStr(header.head.funcHeader.name);

            const headerv = try genNodeValue(vm, parser, ast, node.head.func.header);
            try vm.mapRawSet(entry, try vm.retainOrAllocAstring("header"), headerv);
        },
        .import => {
            name = cy.parser.getNodeString(parser, node.head.left_right.left);
        },
        .object => {
            name = cy.parser.getNodeString(parser, node.head.objectDecl.name);

            const childrenv = try vm.allocEmptyList();
            const children = childrenv.asHeapObject().list.getList();

            const body = nodes[node.head.objectDecl.body];
            var funcId = body.head.objectDeclBody.funcsHead;
            while (funcId != cy.NullId) {
                const funcN = nodes[funcId];
                var childDecl: cy.parser.StaticDecl = undefined;
                switch (funcN.node_t) {
                    .funcDecl => childDecl = .{ .declT = .func, .nodeId = funcId, .data = undefined },
                    .funcDeclInit => childDecl = .{ .declT = .funcInit, .nodeId = funcId, .data = undefined },
                    else => return error.Unsupported,
                }

                try children.append(alloc, try genDeclEntry(vm, parser, ast, childDecl, state));
                funcId = funcN.next;
            }

            try vm.mapRawSet(entry, try vm.retainOrAllocAstring("children"), childrenv);
        },
        .enumT => {
            name = cy.parser.getNodeString(parser, node.head.enumDecl.name);
        }
    }
    const pos = tokens[node.start_token].pos();
    state.pos = pos;
    state.node = node;

    // Find doc comments.
    if (try genDocComment(vm, parser, decl, state)) |docStr| {
        try vm.mapRawSet(entry, try vm.retainOrAllocAstring("docs"), docStr);
    }

    try vm.mapRawSet(entry, try vm.retainOrAllocAstring("name"), try vm.retainOrAllocAstring(name));
    try vm.mapRawSet(entry, try vm.retainOrAllocAstring("pos"), Value.initInt(@intCast(pos)));
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
                return try vm.retainOrAllocString(finalStr);
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

    const ast = cy.ast.Source{
        .src = parser.src,
        .nodes = parser.nodes.items,
        .tokens = parser.tokens.items,
    };

    for (parser.staticDecls.items) |decl| {
        const entry = try genDeclEntry(vm, parser, ast, decl, &state);
        try declsList.append(alloc, entry);
    }
    try map.put(alloc, try vm.retainOrAllocAstring("decls"), decls);

    return root;
}

pub fn parseCyon(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const src = args[0].asString();

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
                const key = try self.retainOrAllocString(entry.key_ptr.*);
                stdMapPut(self, map, key, child);
            }
            return mapVal;
        },
        .string => {
            const str = try val.allocString();
            defer val.alloc.free(str);
            // TODO: Use allocOwnedString
            return try self.retainOrAllocString(str);
        },
        .integer => {
            return Value.initInt(try val.getInt());
        },
        .float => {
            return Value.initF64(try val.getF64());
        },
        .bool => {
            return Value.initBool(val.getBool());
        },
    }
}

fn stdMapPut(vm: *cy.UserVM, obj: *cy.HeapObject, key: Value, value: Value) void {
    const map = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner); 
    map.put(vm.allocator(), key, value) catch cy.fatal();
}

pub fn performGC(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const res = cy.arc.performGC(vm.internal()) catch cy.fatal();
    const map = vm.allocEmptyMap() catch cy.fatal();
    const cycKey = vm.retainOrAllocAstring("numCycFreed") catch cy.fatal();
    const objKey = vm.retainOrAllocAstring("numObjFreed") catch cy.fatal();
    defer {
        vm.release(cycKey);
        vm.release(objKey);
    }
    map.asHeapObject().map.set(vm.internal(), cycKey, Value.initInt(@intCast(res.numCycFreed))) catch cy.fatal();
    map.asHeapObject().map.set(vm.internal(), objKey, Value.initInt(@intCast(res.numObjFreed))) catch cy.fatal();
    return map;
}

pub fn print(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueToTempString(args[0]);
    vm.internal().print.?(@ptrCast(vm), cc.initStr(str));
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
        .bool => return Value.initSymbol(@intFromEnum(Symbol.bool)),
        .map => return Value.initSymbol(@intFromEnum(Symbol.map)),
        .int => return Value.initSymbol(@intFromEnum(Symbol.int)),
        .list => return Value.initSymbol(@intFromEnum(Symbol.list)),
        .string => return Value.initSymbol(@intFromEnum(Symbol.string)),
        .array => return Value.initSymbol(@intFromEnum(Symbol.array)),
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

fn arrayConcat(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const rslice = args[1].asArray();
    return vm.allocArrayConcat(slice, rslice) catch fatal();
}

fn arraySlice(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();

    var start: i48 = undefined;
    if (args[1].isNone()) {
        start = 0;
    } else if (args[1].isInteger()) {
        start = args[1].asInteger();
    } else {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }
    if (start < 0) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    }

    var end: i48 = undefined;
    if (args[2].isNone()) {
        end = @intCast(slice.len);
    } else if (args[2].isInteger()) {
        end = args[2].asInteger();
    } else {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }
    if (end > slice.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    }
    if (end < start) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    }

    const parent = obj.array.getParent();
    vm.retainObject(parent);
    return vm.allocArraySlice(slice[@intCast(start)..@intCast(end)], parent) catch fatal();
}

fn arrayInsert(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const idx = args[1].asInteger();
    if (idx < 0 or idx > slice.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    } 
    const insert = args[2].asArray();
    const new = vm.allocUnsetArrayObject(slice.len + insert.len) catch cy.fatal();
    const buf = new.array.getMutSlice();
    const uidx: u32 = @intCast(idx);
    std.mem.copy(u8, buf[0..uidx], slice[0..uidx]);
    std.mem.copy(u8, buf[uidx..uidx+insert.len], insert);
    std.mem.copy(u8, buf[uidx+insert.len..], slice[uidx..]);
    return Value.initNoCycPtr(new);
}

fn arrayFind(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const needle = args[1].asArray();
    if (needle.len > 0 and needle.len <= slice.len) {
        if (needle.len == 1) {
            // One byte special case. Perform indexOfChar.
            if (cy.string.indexOfChar(slice, needle[0])) |idx| {
                return Value.initInt(@intCast(idx));
            }
        }
        if (cy.string.indexOf(slice, needle)) |idx| {
            return Value.initInt(@intCast(idx));
        }
    }
    return Value.None;
}

fn arrayStartsWith(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const needle = args[1].asArray();
    return Value.initBool(std.mem.startsWith(u8, slice, needle));
}

fn arrayEndsWith(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = args[0].asHeapObject().array.getSlice();
    const needle = args[1].asArray();
    return Value.initBool(std.mem.endsWith(u8, str, needle));
}

fn arrayDecode(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    const encoding = Value.initSymbol(@intFromEnum(Symbol.utf8));
    return arrayDecode1(vm, &[_]Value{args[0], encoding}, nargs);
}

fn arrayDecode1(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();

    const encoding = bindings.getBuiltinSymbol(args[1].asSymbolId()) orelse {
        return prepareThrowSymbol(vm, .InvalidArgument);
    };
    if (encoding != Symbol.utf8) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }

    const parent = obj.array.getParent();
    const slice = obj.array.getSlice();
    if (cy.validateUtf8(slice)) |size| {
        // Since the bytes are validated, just return a slice view of the bytes.
        if (size == slice.len) {
            vm.retainObject(parent);
            return vm.allocAstringSlice(slice, parent) catch fatal();
        } else {
            vm.retainObject(parent);
            return vm.allocUstringSlice(slice, @intCast(size), parent) catch fatal();
        }
    } else {
        return prepareThrowSymbol(vm, .Unicode);
    }
}

fn arrayByteAt(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    var idx = args[1].asInteger();

    if (idx < 0) {
        idx = @as(i48, @intCast(slice.len)) + idx;
    }
    if (idx < 0 or idx >= slice.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    }
    return Value.initInt(@intCast(slice[@intCast(idx)]));
}

fn arrayFindAnyByte(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const set = args[1].asArray();
    const setIsAscii = cy.isAstring(set);
    if (setIsAscii) {
        if (cy.indexOfAsciiSet(slice, set)) |idx| {
            return Value.initInt(@intCast(idx));
        }
    } else {
        // Slow check against every byte.
        var minIndex: u32 = cy.NullId;
        for (set) |byte| {
            if (cy.string.indexOfChar(slice, byte)) |idx| {
                if (idx < minIndex) {
                    minIndex = @intCast(idx);
                }
            }
        }
        if (minIndex != cy.NullId) {
            return Value.initInt(@intCast(minIndex));
        }
    }
    return Value.None;
}

fn arrayFindByte(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const byte = args[1].asInteger();

    if (cy.indexOfChar(slice, @intCast(byte))) |idx| {
        return Value.initInt(@intCast(idx));
    }
    return Value.None;
}

fn arrayLen(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(@intCast(obj.array.getSlice().len));
}

fn arrayTrim(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();

    const trimRunes = args[2].asArray();

    var res: []const u8 = undefined;
    const mode = bindings.getBuiltinSymbol(args[1].asSymbolId()) orelse {
        return prepareThrowSymbol(vm, .InvalidArgument);
    };
    switch (mode) {
        .left => res = std.mem.trimLeft(u8, slice, trimRunes),
        .right => res = std.mem.trimRight(u8, slice, trimRunes),
        .ends => res = std.mem.trim(u8, slice, trimRunes),
        else => {
            return prepareThrowSymbol(vm, .InvalidArgument);
        }
    }

    return vm.allocArray(res) catch fatal();
}

fn arrayReplace(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const needle = args[1].asArray();
    const replacement = args[2].asArray();

    const idxBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
    idxBuf.clearRetainingCapacity();
    defer idxBuf.ensureMaxCapOrClear(vm.allocator(), 4096) catch fatal();
    const newLen = cy.prepReplacement(slice, needle, replacement, idxBuf.writer(vm.allocator())) catch fatal();
    const numIdxes = @divExact(idxBuf.len, 4);
    if (numIdxes > 0) {
        const new = vm.allocUnsetArrayObject(newLen) catch fatal();
        const newBuf = new.array.getMutSlice();
        const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
        cy.replaceAtIdxes(newBuf, slice, @intCast(needle.len), replacement, idxes);
        return Value.initNoCycPtr(new);
    } else {
        vm.retainObject(obj);
        return Value.initNoCycPtr(obj);
    }
}

fn arrayInsertByte(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.array.getSlice();

    const index: i48 = args[1].asInteger();
    if (index < 0 or index > str.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    } 
    const byte: u8 = @intCast(args[2].asInteger());
    const new = vm.allocUnsetArrayObject(str.len + 1) catch cy.fatal();
    const buf = new.array.getMutSlice();
    const uidx: usize = @intCast(index);
    std.mem.copy(u8, buf[0..uidx], str[0..uidx]);
    buf[uidx] = byte;
    std.mem.copy(u8, buf[uidx+1..], str[uidx..]);
    return Value.initNoCycPtr(new);
}

fn arrayRepeat(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();

    const n = args[1].asInteger();
    if (n < 0) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }

    var un: u32 = @intCast(n);
    const len = un * slice.len;
    if (un > 1 and len > 0) {
        const new = vm.allocUnsetArrayObject(len) catch fatal();
        const buf = new.array.getMutSlice();

        // This is already quite fast since it has good cache locality.
        // Might be faster if the front of the buffer up to a certain size was used to memcpy instead of just 1 `str`.
        var i: u32 = 0;
        var dst: u32 = 0;
        while (i < un) : (i += 1) {
            std.mem.copy(u8, buf[dst..dst + slice.len], slice);
            dst += @intCast(slice.len);
        }

        return Value.initNoCycPtr(new);
    } else {
        if (un == 0) {
            return vm.allocArray("") catch fatal();
        } else {
            vm.retainObject(obj);
            return Value.initNoCycPtr(obj);
        }
    }
}

fn arraySplit(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const delim = args[1].asArray();

    const res = vm.allocEmptyList() catch fatal();
    if (delim.len == 0) {
        return res;
    }
    const list = res.asHeapObject();

    const parent = obj.array.getParent();
    var iter = std.mem.split(u8, slice, delim);
    while (iter.next()) |part| {
        vm.retainObject(parent);
        const new = vm.allocArraySlice(part, parent) catch fatal();
        list.list.append(vm.allocator(), new);
    }
    return res;
}

fn arrayCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const str = vm.valueToTempByteArray(args[0]);
    return vm.allocArray(str) catch cy.fatal();
}

fn fiberStatus(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const fiber = args[0].castHeapObject(*vmc.Fiber);

    if (vm.internal().curFiber == fiber) {
        return Value.initSymbol(@intFromEnum(Symbol.running));
    } else {
        // Check if done.
        if (fiber.pcOffset == cy.NullId) {
            return Value.initSymbol(@intFromEnum(Symbol.done));
        } else {
            return Value.initSymbol(@intFromEnum(Symbol.paused));
        }
    }
}

fn metatypeId(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(obj.metatype.type);
}

fn pointerValue(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(@bitCast(@as(u48, (@intCast(@intFromPtr(obj.pointer.ptr))))));
}

fn pointerWriteAt(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const idx = args[1].asInteger();
    const val = args[2];
    const rawPtr = obj.pointer.ptr;
    const valT = val.getTypeId();
    const uidx: u48 = @bitCast(idx);
    switch (valT) {
        bt.Pointer => {
            @as(*?*anyopaque, @ptrFromInt(@intFromPtr(rawPtr) + uidx)).* = val.asHeapObject().pointer.ptr;
        },
        else => {
            return prepareThrowSymbol(vm, .InvalidArgument);
        }
    }
    return Value.initInt(@bitCast(@as(u48, (@intCast(@intFromPtr(obj.pointer.ptr))))));
}

fn pointerCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    if (val.isPointerT()) {
        vm.retain(val);
        return val;
    } else if (val.isInteger()) {
        const i: usize = @intCast(val.asInteger());
        return cy.heap.allocPointer(vm.internal(), @ptrFromInt(i)) catch fatal();
    } else {
        return vm.returnPanic("Not a `pointer`.");
    }
}

fn errorSym(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const recv = args[0];
    return Value.initSymbol(recv.asErrorSymbol());
}

fn errorCall(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = args[0];
    if (val.isPointer()) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    } else {
        if (val.isSymbol()) {
            return Value.initErrorSymbol(@intCast(val.asSymbolId()));
        } else if (val.isEnum()) {
            const enumT = val.getEnumType();
            const enumv = val.getEnumValue();
            const name = vm.internal().types[enumT].sym.cast(.enumType).getValueSym(enumv).name();
            const symId = vm.internal().ensureSymbol(name) catch cy.unexpected();
            return Value.initErrorSymbol(symId);
        } else {
            return prepareThrowSymbol(vm, .InvalidArgument);
        }
    }
}

fn intCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getUserTag()) {
        .float => {
            return Value.initInt(@intFromFloat(@trunc(val.asF64())));
        },
        .string => {
            var str = vm.valueToTempString(val);
            if (std.mem.indexOfScalar(u8, str, '.')) |idx| {
                str = str[0..idx];
            }
            const res = std.fmt.parseInt(i32, str, 10) catch {
                return Value.initInt(0);
            };
            return Value.initInt(res);
        },
        .enumT => return Value.initInt(val.getEnumValue()),
        .symbol => return Value.initInt(@intCast(val.val & @as(u64, 0xFF))),
        .int => {
            return val;
        },
        else => {
            return Value.initInt(0);
        }
    }
}

fn boolCall(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].toBool());
}

fn floatCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getUserTag()) {
        .float => return val,
        .string => {
            const res = std.fmt.parseFloat(f64, vm.valueToTempString(val)) catch {
                return Value.initF64(0);
            };
            return Value.initF64(res);
        },
        .enumT => return Value.initF64(@floatFromInt(val.getEnumValue())),
        .symbol => return Value.initF64(@floatFromInt(val.val & @as(u64, 0xFF))),
        .int => return Value.initF64(@floatFromInt(val.asInteger())),
        .none => return Value.initF64(0),
        .bool => return Value.initF64(if (val.asBool()) 1 else 0),
        else => {
            vm.release(val);
            return vm.returnPanic("Not a type that can be converted to `float`.");
        }
    }
}