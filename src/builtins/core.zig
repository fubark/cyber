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
const os_mod = @import("os.zig");
const http = @import("../http.zig");
const cache = @import("../cache.zig");
const bt = cy.types.BuiltinTypeSymIds;

const log = cy.log.scoped(.core);

pub fn initModule(self: *cy.VMcompiler, modId: cy.ModuleId) anyerror!void {
    const b = bindings.ModuleBuilder.init(self, modId);
    try b.mod().syms.ensureTotalCapacity(self.alloc, 13);

    // Funcs.
    try b.setFunc("arrayFill", &.{bt.Any, bt.Integer}, bt.List, arrayFill);
    try b.setFunc("asciiCode", &.{bt.Any}, bt.Any, asciiCode);
    if (cy.hasJit) {
        try b.setFunc("bindLib", &.{bt.Any, bt.List}, bt.Any, bindLib);
        try b.setFunc("bindLib", &.{bt.Any, bt.List, bt.Map}, bt.Any, bindLibExt);
    } else {
        try b.setFunc("bindLib", &.{bt.Any, bt.List}, bt.Any, bindings.nop2);
        try b.setFunc("bindLib", &.{bt.Any, bt.List, bt.Map}, bt.Any, bindings.nop3);
    }
    try b.setFunc("bool", &.{ bt.Any }, bt.Boolean, coreBool);
    if (cy.isWasm) {
        try b.setFunc("cacheUrl", &.{ bt.Any }, bt.Any, bindings.nop1);
    } else {
        try b.setFunc("cacheUrl", &.{ bt.Any }, bt.Any, cacheUrl);
    }
    try b.setFunc("char", &.{bt.Any}, bt.Any, char);
    try b.setFunc("copy", &.{bt.Any}, bt.Any, copy);
    try b.setFunc("errorReport", &.{}, bt.String, errorReport);
    if (cy.isWasm) {
        try b.setFunc("evalJS", &.{bt.Any}, bt.None, evalJS);
        try b.setFunc("execCmd", &.{bt.List}, bt.Any, bindings.nop1);
    } else {
        try b.setFunc("execCmd", &.{bt.List}, bt.Any, execCmd);
    }
    try b.setFunc("exit", &.{bt.Integer}, bt.None, exit);
    try b.setFunc("fetchUrl", &.{bt.Any}, bt.Any, fetchUrl);
    if (cy.hasStdFiles) {
        try b.setFunc("getInput", &.{}, bt.Any, getInput);
    } else {
        try b.setFunc("getInput", &.{}, bt.Any, bindings.nop0);
    }
    try b.setFunc("isAlpha", &.{ bt.Integer }, bt.Boolean, isAlpha);
    try b.setFunc("isDigit", &.{ bt.Integer }, bt.Boolean, isDigit);
    // try mod.setNativeFunc(alloc, "dump", 1, dump);
    try b.setFunc("must", &.{ bt.Any }, bt.Any, must);
    try b.setFunc("opaque", &.{ bt.Any }, bt.Pointer, coreOpaque);
    try b.setFunc("panic", &.{ bt.Any }, bt.None, panic);
    try b.setFunc("parseCyber", &.{ bt.Any }, bt.Map, parseCyber);
    try b.setFunc("parseCyon", &.{ bt.Any }, bt.Any, parseCyon);
    try b.setFunc("performGC", &.{}, bt.Map, performGC);
    try b.setFunc("print", &.{bt.Any}, bt.None, print);
    try b.setFunc("prints", &.{bt.Any}, bt.None, prints);
    if (cy.hasStdFiles) {
        try b.setFunc("readAll", &.{}, bt.Any, readAll);
        try b.setFunc("readFile", &.{ bt.Any }, bt.Any, readFile);
        try b.setFunc("readLine", &.{}, bt.Any, readLine);
    } else {
        try b.setFunc("readAll", &.{}, bt.Any, bindings.nop0);
        try b.setFunc("readFile", &.{ bt.Any }, bt.Any, bindings.nop1);
        try b.setFunc("readLine", &.{}, bt.Any, bindings.nop0);
    }
    try b.setFunc("runestr", &.{ bt.Integer }, bt.String, runestr);
    try b.setFunc("toCyon", &.{ bt.Any }, bt.String, toCyon);
    try b.setFunc("typeid", &.{ bt.Any }, bt.Integer, typeid);
    try b.setFunc("valtag", &.{ bt.Any }, bt.Symbol, valtag);
    try b.setFunc("typesym", &.{ bt.Any }, bt.Symbol, typesym);
    try b.setFunc("typeof", &.{ bt.Any }, bt.MetaType, typeof);
    if (cy.hasStdFiles) {
        try b.setFunc("writeFile", &.{ bt.Any, bt.Any }, bt.Any, writeFile);
    } else {
        try b.setFunc("writeFile", &.{ bt.Any, bt.Any }, bt.Any, bindings.nop2);
    }
}

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

fn cacheUrl(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const alloc = vm.allocator();
    const url = vm.valueToTempString(args[0]);

    const specGroup = cache.getSpecHashGroup(alloc, url) catch cy.fatal();
    defer specGroup.deinit(alloc);

    if (vm.internal().config.reload) {
        specGroup.markEntryBySpecForRemoval(url) catch cy.fatal();
    } else {
        // First check local cache.
        if (specGroup.findEntryBySpec(url) catch cy.fatal()) |entry| {
            const path = cache.allocSpecFilePath(alloc, entry) catch cy.fatal();
            defer alloc.free(path);
            return vm.allocStringInfer(path) catch cy.fatal();
        }
    }

    const resp = http.get(alloc, vm.internal().httpClient, url) catch |err| {
        log.debug("cacheUrl error: {}", .{err});
        return prepareThrowSymbol(vm, .UnknownError);
    };
    defer alloc.free(resp.body);
    if (resp.status != .ok) {
        log.debug("cacheUrl response status: {}", .{resp.status});
        return prepareThrowSymbol(vm, .UnknownError);
    } else {
        const entry = cache.saveNewSpecFile(alloc, specGroup, url, resp.body) catch cy.fatal();
        defer entry.deinit(alloc);
        const path = cache.allocSpecFilePath(alloc, entry) catch cy.fatal();
        defer alloc.free(path);
        return vm.allocStringInfer(path) catch cy.fatal();
    }
}

pub fn bindLib(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("bindLib", "0.1", "Use os.bindLib() instead.", &.{});
    return os_mod.bindLib(vm, args, nargs);
}

pub fn bindLibExt(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("bindLib", "0.1", "Use os.bindLib() instead.", &.{});
    return os_mod.bindLibExt(vm, args, nargs);
}

pub fn coreBool(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    fmt.printDeprecated("bool", "0.2", "Use boolean() instead.", &.{});
    return bindings.booleanCall(vm, args, nargs);
}

pub fn char(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("char", "0.1", "Use asciiCode() instead.", &.{});
    return asciiCode(vm, args, nargs);
}

pub fn copy(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = args[0];
    return cy.value.shallowCopy(@ptrCast(vm), val);
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

pub fn execCmd(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const alloc = vm.allocator();
    const ivm = vm.internal();

    const obj = args[0].asHeapObject();
    var buf: std.ArrayListUnmanaged([]const u8) = .{};
    defer {
        for (buf.items) |arg| {
            alloc.free(arg);
        }
        buf.deinit(alloc);
    }
    for (obj.list.items()) |arg| {
        buf.append(alloc, vm.valueToString(arg) catch cy.fatal()) catch cy.fatal();
    }

    const res = std.ChildProcess.exec(.{
        .allocator = alloc,
        .argv = buf.items,
        .max_output_bytes = 1024 * 1024 * 10,
    }) catch |err| {
        switch (err) {
            error.FileNotFound => 
                return prepareThrowSymbol(vm, .FileNotFound),
            error.StdoutStreamTooLong =>
                return prepareThrowSymbol(vm, .StreamTooLong),
            error.StderrStreamTooLong =>
                return prepareThrowSymbol(vm, .StreamTooLong),
            else => cy.panicFmt("exec err {}\n", .{err}),
        }
    };

    const map = vm.allocEmptyMap() catch cy.fatal();
    const outKey = vm.allocAstring("out") catch cy.fatal();
    const errKey = vm.allocAstring("err") catch cy.fatal();
    defer {
        vm.release(outKey);
        vm.release(errKey);
    }

    // TODO: Use allocOwnedString
    defer alloc.free(res.stdout);
    const out = vm.allocStringInfer(res.stdout) catch cy.fatal();
    defer vm.release(out);
    map.asHeapObject().map.set(ivm, outKey, out) catch cy.fatal();
    // TODO: Use allocOwnedString
    defer alloc.free(res.stderr);
    const err = vm.allocStringInfer(res.stderr) catch cy.fatal();
    defer vm.release(err);
    map.asHeapObject().map.set(ivm, errKey, err) catch cy.fatal();
    if (res.term == .Exited) {
        const exitedKey = vm.allocAstring("exited") catch cy.fatal();
        defer vm.release(exitedKey);
        map.asHeapObject().map.set(ivm, exitedKey, Value.initF64(@floatFromInt(res.term.Exited))) catch cy.fatal();
    }
    return map;
}

pub fn exit(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const status: u8 = @intCast(args[0].asInteger());
    std.os.exit(status);
}

pub fn fetchUrl(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const alloc = vm.allocator();
    const url = vm.valueToTempString(args[0]);
    if (cy.isWasm) {
        hostFetchUrl(url.ptr, url.len);
        return Value.None;
    } else {
        const resp = http.get(alloc, vm.internal().httpClient, url) catch |err| {
            log.debug("fetchUrl error: {}", .{err});
            return prepareThrowSymbol(vm, .UnknownError);
        };
        defer alloc.free(resp.body);
        // TODO: Use allocOwnedString
        return vm.allocRawString(resp.body) catch cy.fatal();
    }
}

extern fn hostFetchUrl(url: [*]const u8, urlLen: usize) void;

pub fn getInput(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const input = std.io.getStdIn().reader().readUntilDelimiterAlloc(vm.allocator(), '\n', 10e8) catch |err| {
        if (err == error.EndOfStream) {
            return prepareThrowSymbol(vm, .EndOfStream);
        } else cy.fatal();
    };
    defer vm.allocator().free(input);
    // TODO: Use allocOwnedString
    return vm.allocRawString(input) catch cy.fatal();
}

pub fn must(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    if (!args[0].isError()) {
        return args[0];
    } else {
        return panic(vm, args, nargs);
    }
}

pub fn coreOpaque(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    fmt.printDeprecated("opaque", "0.1", "Use pointer() instead.", &.{});
    return bindings.pointerCall(vm, args, nargs);
}

pub fn panic(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueToTempString(args[0]);
    return vm.returnPanic(str);
}

fn isAlpha(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
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

fn isDigit(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
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

fn runestr(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
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

fn parseCyber(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
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
    if (cy.isWasmFreestanding) {
        hostFileWrite(1, str.ptr, str.len);
        hostFileWrite(1, "\n", 1);
    } else {
        const w = std.io.getStdOut().writer();
        w.writeAll(str) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
    return Value.None;
}

pub extern fn hostFileWrite(fid: u32, str: [*]const u8, strLen: usize) void;

pub fn prints(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueToTempRawString(args[0]);
    if (cy.isWasm) {
        hostFileWrite(1, str.ptr, str.len);
    } else {
        const w = std.io.getStdOut().writer();
        w.writeAll(str) catch cy.fatal();
    }
    return Value.None;
}

pub fn readAll(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    const input = std.io.getStdIn().readToEndAlloc(vm.allocator(), 10e8) catch cy.fatal();
    defer vm.allocator().free(input);
    // TODO: Use allocOwnString.
    return vm.allocRawString(input) catch cy.fatal();
}

pub fn readFile(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const path = vm.valueToTempRawString(args[0]);
    const content = std.fs.cwd().readFileAlloc(vm.allocator(), path, 10e8) catch |err| {
        if (err == error.FileNotFound) {
            return prepareThrowSymbol(vm, .FileNotFound);
        }
        log.debug("readFile {}", .{err});
        return prepareThrowSymbol(vm, .UnknownError);
    };
    defer vm.allocator().free(content);
    // TODO: Use allocOwnedString.
    return vm.allocRawString(content) catch cy.fatal();
}

pub fn readLine(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("readLine", "0.1", "Use getInput() instead.", &.{});
    return getInput(vm, args, nargs);
}

pub fn typeid(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    fmt.printDeprecated("typeid", "0.2", "Use metatype.id() instead.", &.{});
    return Value.initInt(@intCast(args[0].getTypeId()));
}

fn valtag(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("valtag", "0.2", "Use typesym() instead.", &.{});
    return typesym(vm, args, nargs);
}

fn typeof(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
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

pub fn writeFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const path = vm.valueToTempRawString(args[0]);
    const pathDupe = vm.allocator().dupe(u8, path) catch fatal();
    defer vm.allocator().free(pathDupe);
    const content = vm.valueToTempRawString(args[1]);
    std.fs.cwd().writeFile(path, content) catch cy.fatal();
    return Value.None;
}

pub fn evalJS(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueToTempString(args[0]);
    hostEvalJS(str.ptr, str.len);
    return Value.None;
}

extern fn hostEvalJS(ptr: [*]const u8, len: usize) void;