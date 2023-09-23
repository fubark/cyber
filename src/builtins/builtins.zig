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
const bt = cy.types.BuiltinTypeSymIds;
const vmc = cy.vmc;

const log = cy.log.scoped(.core);

pub const Src = @embedFile("builtins.cy");
pub fn defaultFuncLoader(_: *cy.UserVM, func: cy.HostFuncInfo) callconv(.C) vmc.HostFuncFn {
    if (std.mem.eql(u8, funcs[func.idx].@"0", func.name.slice())) {
        return @ptrCast(funcs[func.idx].@"1");
    }
    return null;
}

pub fn postLoad(vm: *cy.UserVM, modId: cy.ModuleId) callconv(.C) void {
    const b = bindings.ModuleBuilder.init(vm.internal().compiler, modId);
    if (cy.isWasm) {
        b.setFunc("evalJS", &.{bt.Any}, bt.None, evalJS) catch cy.fatal();
    }
}

const NameHostFunc = struct { []const u8, cy.ZHostFuncFn };
const funcs = [_]NameHostFunc{
    .{"arrayFill", arrayFill},
    .{"asciiCode", asciiCode},
    .{"bool", btBool},
    .{"char", char},
    .{"copy", copy},
    .{"errorReport", errorReport},
    .{"isAlpha", isAlpha},
    .{"isDigit", isDigit},
    .{"must", must},
    .{"opaque", btOpaque},
    .{"panic", panic},
    .{"parseCyber", parseCyber},
    .{"parseCyon", parseCyon},
    .{"performGC", performGC},
    .{"print", print},
    .{"prints", prints},
    .{"runestr", runestr},
    .{"toCyon", toCyon},
    .{"typeid", typeid},
    .{"valtag", valtag},
    .{"typesym", typesym},
    .{"typeof", typeof},
};

pub fn arrayFill(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return vm.allocListFill(args[0], @intCast(args[1].asInteger())) catch cy.fatal();
}

pub fn asciiCode(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("asciiCode", "0.2", "Use UTF-8 rune notation or string.runeAt(0) instead.", &.{});
    const str = vm.valueToTempString(args[0]);
    if (str.len > 0) {
        return Value.initInt(str[0]);
    } else {
        return Value.None;
    }
}

pub fn btBool(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    fmt.printDeprecated("bool", "0.2", "Use boolean() instead.", &.{});
    return bindings.booleanCall(vm, args, nargs);
}

pub fn char(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("char", "0.1", "Use asciiCode() instead.", &.{});
    return asciiCode(vm, args, nargs);
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

pub fn btOpaque(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    fmt.printDeprecated("opaque", "0.1", "Use pointer() instead.", &.{});
    return bindings.pointerCall(vm, args, nargs);
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

pub fn toCyon(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
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
    const root = args[0];
    const alloc = vm.allocator();

    const cyon = cy.encodeCyon(alloc, vm, root, S.encodeRoot) catch fatal();
    defer alloc.free(cyon);
    return vm.allocStringInfer(cyon) catch fatal();
}

pub fn parseCyber(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const src = vm.valueToTempRawString(args[0]);

    const alloc = vm.allocator();
    var parser = cy.Parser.init(alloc);
    defer parser.deinit();
    const res = parser.parse(src) catch |err| {
        log.debug("parseCyber: {}", .{err});
        return prepareThrowSymbol(vm, .UnknownError);
    };

    return parseCyberGenResult(vm, &parser, res) catch |err| {
        log.debug("parseCyber: {}", .{err});
        return prepareThrowSymbol(vm, .UnknownError);
    };
}

fn parseCyberGenResult(vm: *cy.UserVM, parser: *const cy.Parser, res: cy.ParseResultView) !Value {
    const alloc = vm.allocator();
    const nodes = parser.nodes.items;
    const root = try vm.allocEmptyMap();
    const map = root.asHeapObject().map.map();

    const decls = try vm.allocEmptyList();
    const declsList = decls.asHeapObject().list.getList();
    for (parser.staticDecls.items) |decl| {
        const entry = try vm.allocEmptyMap();
        const entryMap = entry.asHeapObject().map.map();
        try entryMap.put(alloc, vm.internal(), try vm.allocAstring("type"), try vm.allocAstring(@tagName(decl.declT)));
        var name: []const u8 = undefined;
        var pos: u32 = undefined;
        switch (decl.declT) {
            .variable => {
                const node = nodes[decl.inner.variable];
                const varSpec = nodes[node.head.staticDecl.varSpec];
                name = res.getFirstNodeString(varSpec.head.varSpec.name);
                pos = res.tokens[node.start_token].pos();
            },
            .typeAlias => {
                const node = nodes[decl.inner.typeAlias];
                name = res.getFirstNodeString(node.head.typeAliasDecl.name);
                pos = res.tokens[node.start_token].pos();
            },
            .func => {
                const node = nodes[decl.inner.func];
                const header = nodes[node.head.func.header];
                name = res.getFirstNodeString(header.head.funcHeader.name);
                pos = res.tokens[node.start_token].pos();
            },
            .funcInit => {
                const node = nodes[decl.inner.funcInit];
                const header = nodes[node.head.func.header];
                name = res.getFirstNodeString(header.head.funcHeader.name);
                pos = res.tokens[node.start_token].pos();
            },
            .import => {
                const node = nodes[decl.inner.import];
                name = res.getFirstNodeString(node.head.left_right.left);
                pos = res.tokens[node.start_token].pos();
            },
            .object => {
                const node = nodes[decl.inner.object];
                name = res.getFirstNodeString(node.head.objectDecl.name);
                pos = res.tokens[node.start_token].pos();
            },
            .enumT => {
                const node = nodes[decl.inner.object];
                name = res.getFirstNodeString(node.head.enumDecl.name);
                pos = res.tokens[node.start_token].pos();
            }
        }
        try entryMap.put(alloc, vm.internal(), try vm.allocAstring("name"), try vm.allocAstring(name));
        try entryMap.put(alloc, vm.internal(), try vm.allocAstring("pos"), Value.initF64(@floatFromInt(pos)));
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
    vm.internal().print(vm, cy.Str.initSlice("\n"));
    return Value.None;
}

pub fn prints(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueToTempRawString(args[0]);
    vm.internal().print(vm, cy.Str.initSlice(str));
    return Value.None;
}

pub fn typeid(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    fmt.printDeprecated("typeid", "0.2", "Use metatype.id() instead.", &.{});
    return Value.initInt(@intCast(args[0].getTypeId()));
}

pub fn valtag(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("valtag", "0.2", "Use typesym() instead.", &.{});
    return typesym(vm, args, nargs);
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

pub fn evalJS(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueToTempString(args[0]);
    hostEvalJS(str.ptr, str.len);
    return Value.None;
}

extern fn hostEvalJS(ptr: [*]const u8, len: usize) void;