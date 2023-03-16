const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const cy = @import("../cyber.zig");
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const bindings = @import("bindings.zig");
const TagLit = bindings.TagLit;
const fmt = @import("../fmt.zig");
const os_mod = @import("os.zig");
const http = @import("../http.zig");
const cache = @import("../cache.zig");
const bt = cy.sema.BuiltinTypeSymIds;

const log = stdx.log.scoped(.core);

pub fn initModule(self: *cy.VMcompiler, mod: *cy.Module) !void {
    try mod.syms.ensureTotalCapacity(self.alloc, 13);
    try mod.setNativeFunc(self, "arrayFill", 2, arrayFill);
    try mod.setNativeFunc(self, "asciiCode", 1, asciiCode);
    if (cy.isWasm) {
        try mod.setNativeFunc(self, "bindLib", 2, bindings.nop2);
        try mod.setNativeFunc(self, "bindLib", 3, bindings.nop3);
    } else {
        try mod.setNativeFunc(self, "bindLib", 2, bindLib);
        try mod.setNativeFunc(self, "bindLib", 3, bindLibExt);
    }
    try mod.setNativeFunc(self, "bool", 1, coreBool);
    if (cy.isWasm) {
        try mod.setNativeFunc(self, "cacheUrl", 1, bindings.nop1);
    } else {
        try mod.setNativeFunc(self, "cacheUrl", 1, cacheUrl);
    }
    try mod.setNativeFunc(self, "char", 1, char);
    try mod.setNativeFunc(self, "copy", 1, copy);
    try mod.setNativeFunc(self, "error", 1, coreError);
    if (cy.isWasm) {
        try mod.setNativeFunc(self, "evalJS", 1, evalJS);
        try mod.setNativeFunc(self, "execCmd", 1, bindings.nop1);
    } else {
        try mod.setNativeFunc(self, "execCmd", 1, execCmd);
    }
    try mod.setNativeFunc(self, "exit", 1, exit);
    try mod.setNativeFunc(self, "fetchUrl", 1, fetchUrl);
    if (cy.hasStdFiles) {
        try mod.setNativeFunc(self, "getInput", 0, getInput);
    } else {
        try mod.setNativeFunc(self, "getInput", 0, bindings.nop0);
    }
    try mod.setNativeFunc(self, "int", 1, int);
    // try mod.setNativeFunc(alloc, "dump", 1, dump);
    try mod.setNativeTypedFunc(self, "List", &.{ bt.Any, bt.List }, List);
    try mod.setNativeFunc(self, "must", 1, must);
    try mod.setNativeFunc(self, "number", 1, number);
    try mod.setNativeFunc(self, "opaque", 1, coreOpaque);
    try mod.setNativeFunc(self, "panic", 1, panic);
    try mod.setNativeFunc(self, "parseCyon", 1, parseCyon);
    try mod.setNativeFunc(self, "print", 1, print);
    try mod.setNativeFunc(self, "prints", 1, prints);
    try mod.setNativeFunc(self, "rawstring", 1, rawstring);
    if (cy.hasStdFiles) {
        try mod.setNativeFunc(self, "readAll", 0, readAll);
        try mod.setNativeFunc(self, "readFile", 1, readFile);
        try mod.setNativeFunc(self, "readLine", 0, readLine);
    } else {
        try mod.setNativeFunc(self, "readAll", 0, bindings.nop0);
        try mod.setNativeFunc(self, "readFile", 1, bindings.nop1);
        try mod.setNativeFunc(self, "readLine", 0, bindings.nop0);
    }
    try mod.setNativeFunc(self, "string", 1, string);
    try mod.setNativeTypedFunc(self, "taglit", &.{ bt.Any, bt.TagLiteral }, taglit);
    try mod.setNativeFunc(self, "toCyon", 1, toCyon);
    try mod.setNativeFunc(self, "typeid", 1, typeid);
    try mod.setNativeFunc(self, "valtag", 1, valtag);
    if (cy.hasStdFiles) {
        try mod.setNativeFunc(self, "writeFile", 2, writeFile);
    } else {
        try mod.setNativeFunc(self, "writeFile", 2, bindings.nop2);
    }
}

pub fn arrayFill(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    return vm.allocListFill(args[0], @floatToInt(u32, args[1].toF64())) catch stdx.fatal();
}

pub fn asciiCode(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("asciiCode", "0.2", "Use UTF-8 rune notation or string.runeAt(0) instead.", &.{});
    defer vm.release(args[0]);
    const str = vm.valueToTempString(args[0]);
    if (str.len > 0) {
        return Value.initF64(@intToFloat(f64, str[0]));
    } else {
        return Value.None;
    }
}

fn cacheUrl(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const alloc = vm.allocator();
    const url = vm.valueToTempString(args[0]);
    defer vm.release(args[0]);

    const specGroup = cache.getSpecHashGroup(alloc, url) catch stdx.fatal();
    defer specGroup.deinit(alloc);

    if (vm.internal().config.reload) {
        specGroup.markEntryBySpecForRemoval(url) catch stdx.fatal();
    } else {
        // First check local cache.
        if (specGroup.findEntryBySpec(url) catch stdx.fatal()) |entry| {
            const path = cache.allocSpecFilePath(alloc, entry) catch stdx.fatal();
            defer alloc.free(path);
            return vm.allocStringInfer(path) catch stdx.fatal();
        }
    }

    const resp = http.get(alloc, vm.internal().httpClient, url) catch |err| {
        log.debug("cacheUrl error: {}", .{err});
        return Value.initErrorTagLit(@enumToInt(TagLit.UnknownError));
    };
    defer alloc.free(resp.body);
    if (resp.status != .ok) {
        log.debug("cacheUrl response status: {}", .{resp.status});
        return Value.initErrorTagLit(@enumToInt(TagLit.UnknownError));
    } else {
        const entry = cache.saveNewSpecFile(alloc, specGroup, url, resp.body) catch stdx.fatal();
        defer entry.deinit(alloc);
        const path = cache.allocSpecFilePath(alloc, entry) catch stdx.fatal();
        defer alloc.free(path);
        return vm.allocStringInfer(path) catch stdx.fatal();
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

pub fn coreBool(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    defer vm.release(args[0]);
    return Value.initBool(args[0].toBool());
}

pub fn char(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("char", "0.1", "Use asciiCode() instead.", &.{});
    return asciiCode(vm, args, nargs);
}

pub fn copy(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = args[0];
    defer vm.release(val);
    return cy.value.shallowCopy(@ptrCast(*cy.VM, vm), val);
}

pub fn coreError(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = args[0];
    if (val.isPointer()) {
        stdx.fatal();
    } else {
        if (val.assumeNotPtrIsTagLiteral()) {
            return Value.initErrorTagLit(@intCast(u8, val.asTagLiteralId()));
        } else {
            stdx.fatal();
        }
    }
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

        vm.releaseObject(obj);
    }
    for (obj.list.items()) |arg| {
        buf.append(alloc, vm.valueToString(arg) catch stdx.fatal()) catch stdx.fatal();
    }

    const res = std.ChildProcess.exec(.{
        .allocator = alloc,
        .argv = buf.items,
        .max_output_bytes = 1024 * 1024 * 10,
    }) catch |err| {
        switch (err) {
            error.FileNotFound => 
                return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound)),
            error.StdoutStreamTooLong =>
                return Value.initErrorTagLit(@enumToInt(TagLit.StreamTooLong)),
            error.StderrStreamTooLong =>
                return Value.initErrorTagLit(@enumToInt(TagLit.StreamTooLong)),
            else => stdx.panicFmt("exec err {}\n", .{err}),
        }
    };

    const map = vm.allocEmptyMap() catch stdx.fatal();
    const outKey = vm.allocAstring("out") catch stdx.fatal();
    // TODO: Use allocOwnedString
    defer alloc.free(res.stdout);
    const out = vm.allocStringInfer(res.stdout) catch stdx.fatal();
    ivm.setIndex(map, outKey, out) catch stdx.fatal();
    const errKey = vm.allocAstring("err") catch stdx.fatal();
    // TODO: Use allocOwnedString
    defer alloc.free(res.stderr);
    const err = vm.allocStringInfer(res.stderr) catch stdx.fatal();
    ivm.setIndex(map, errKey, err) catch stdx.fatal();
    if (res.term == .Exited) {
        const exitedKey = vm.allocAstring("exited") catch stdx.fatal();
        ivm.setIndex(map, exitedKey, Value.initF64(@intToFloat(f64, res.term.Exited))) catch stdx.fatal();
    }
    return map;
}

pub fn exit(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const status = @floatToInt(u8, args[0].toF64());
    std.os.exit(status);
}

pub fn fetchUrl(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const alloc = vm.allocator();
    const url = vm.valueToTempString(args[0]);
    defer vm.release(args[0]);
    if (cy.isWasm) {
        hostFetchUrl(url.ptr, url.len);
        return Value.None;
    } else {
        const resp = http.get(alloc, vm.internal().httpClient, url) catch |err| {
            log.debug("fetchUrl error: {}", .{err});
            return Value.initErrorTagLit(@enumToInt(TagLit.UnknownError));
        };
        defer alloc.free(resp.body);
        // TODO: Use allocOwnedString
        return vm.allocRawString(resp.body) catch stdx.fatal();
    }
}

extern fn hostFetchUrl(url: [*]const u8, urlLen: usize) void;

pub fn getInput(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const input = std.io.getStdIn().reader().readUntilDelimiterAlloc(vm.allocator(), '\n', 10e8) catch |err| {
        if (err == error.EndOfStream) {
            return Value.initErrorTagLit(@enumToInt(TagLit.EndOfStream));
        } else stdx.fatal();
    };
    defer vm.allocator().free(input);
    // TODO: Use allocOwnedString
    return vm.allocRawString(input) catch stdx.fatal();
}

pub fn int(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getUserTag()) {
        .number => {
            return Value.initI32(@floatToInt(i32, @trunc(val.asF64())));
        },
        .string => {
            var str = vm.valueToTempString(val);
            if (std.mem.indexOfScalar(u8, str, '.')) |idx| {
                str = str[0..idx];
            }
            const res = std.fmt.parseInt(i32, str, 10) catch {
                return Value.initI32(0);
            };
            return Value.initI32(res);
        },
        else => {
            return Value.initI32(0);
        }
    }
}

pub fn must(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    if (!args[0].isError()) {
        return args[0];
    } else {
        return panic(vm, args, nargs);
    }
}

pub fn number(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getUserTag()) {
        .number => return val,
        .string => {
            const res = std.fmt.parseFloat(f64, vm.valueToTempString(val)) catch {
                return Value.initI32(0);
            };
            return Value.initF64(res);
        },
        .tag => return Value.initF64(@intToFloat(f64, val.val & @as(u64, 0xFF))),
        .tagLiteral => return Value.initF64(@intToFloat(f64, val.val & @as(u64, 0xFF))),
        .int => return Value.initF64(@intToFloat(f64, val.asInteger())),
        else => return Value.initF64(0),
    }
}

pub fn coreOpaque(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const val = args[0];
    if (val.isNumber()) {
        return cy.heap.allocOpaquePtr(@ptrCast(*cy.VM, vm), @intToPtr(?*anyopaque, @floatToInt(usize, val.asF64()))) catch stdx.fatal();
    } else {
        stdx.panicFmt("Unsupported conversion", .{});
    }
}

pub fn panic(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueToTempString(args[0]);
    return vm.returnPanic(str);
}

pub fn toCyon(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const S = struct {
        fn encodeMap(ctx: *cy.EncodeMapContext, val: cy.Value) !void {
            const uservm = stdx.ptrAlignCast(*cy.UserVM, ctx.user_ctx);
            var iter = val.asHeapObject().map.map().iterator();
            while (iter.next()) |e| {
                const key = uservm.valueToTempString(e.key);
                switch (e.value.getUserTag()) {
                    .number => {
                        try ctx.encodeNumber(key, e.value.asF64());
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
        fn encodeList(ctx: *cy.EncodeListContext, val: cy.Value) !void {
            const items = val.asHeapObject().list.items();
            for (items) |it| {
                switch (it.getUserTag()) {
                    .number => {
                        try ctx.encodeNumber(it.asF64());
                    },
                    .string => {
                        const uservm = stdx.ptrAlignCast(*cy.UserVM, ctx.user_ctx);
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
                    .number => {
                        try ctx.encodeNumber(val.asF64());
                    },
                    .string => {
                        const uservm = stdx.ptrAlignCast(*cy.UserVM, ctx.user_ctx);
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
                stdx.panicFmt("unsupported: {s}", .{@typeName(T)});
            }
        }
    };
    const root = args[0];
    defer vm.release(root);

    const alloc = vm.allocator();

    const cyon = cy.encodeCyon(alloc, vm, root, S.encodeRoot) catch fatal();
    defer alloc.free(cyon);
    return vm.allocStringInfer(cyon) catch fatal();
}

pub fn parseCyon(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueAsString(args[0]);
    defer vm.release(args[0]);
    
    const alloc = vm.allocator();
    var parser = cy.Parser.init(alloc);
    defer parser.deinit();
    const val = cy.decodeCyon(alloc, &parser, str) catch stdx.fatal();
    return fromCyonValue(vm, val) catch stdx.fatal();
}

fn fromCyonValue(self: *cy.UserVM, val: cy.DecodeValueIR) !Value {
    const ivm = self.internal();
    switch (val.getValueType()) {
        .list => {
            var dlist = val.asList() catch stdx.fatal();
            defer dlist.deinit();
            const elems = try ivm.alloc.alloc(Value, dlist.arr.len);
            for (elems, 0..) |*elem, i| {
                elem.* = try fromCyonValue(self, dlist.getIndex(i));
            }
            return try cy.heap.allocOwnedList(ivm, elems);
        },
        .map => {
            var dmap = val.asMap() catch stdx.fatal();
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
        .number => {
            return Value.initF64(try val.asF64());
        },
        .boolean => {
            return Value.initBool(val.asBool());
        },
    }
}

fn stdMapPut(vm: *cy.UserVM, obj: *cy.HeapObject, key: Value, value: Value) void {
    const ivm = vm.internal();
    const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner); 
    map.put(vm.allocator(), ivm, key, value) catch stdx.fatal();
}

pub fn print(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const str = vm.valueToTempRawString(args[0]);
    if (cy.isWasm) {
        hostFileWrite(1, str.ptr, str.len);
        hostFileWrite(1, "\n", 1);
    } else {
        const w = std.io.getStdOut().writer();
        w.writeAll(str) catch stdx.fatal();
        w.writeByte('\n') catch stdx.fatal();
    }
    return Value.None;
}

pub extern fn hostFileWrite(fid: u32, str: [*]const u8, strLen: usize) void;

pub fn prints(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = vm.valueToTempRawString(args[0]);
    defer vm.release(args[0]);
    if (cy.isWasm) {
        hostFileWrite(1, str.ptr, str.len);
    } else {
        const w = std.io.getStdOut().writer();
        w.writeAll(str) catch stdx.fatal();
    }
    return Value.None;
}

pub fn readAll(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    const input = std.io.getStdIn().readToEndAlloc(vm.allocator(), 10e8) catch stdx.fatal();
    defer vm.allocator().free(input);
    // TODO: Use allocOwnString.
    return vm.allocRawString(input) catch stdx.fatal();
}

pub fn readFile(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    defer vm.release(args[0]);
    const path = vm.valueToTempString(args[0]);
    const content = std.fs.cwd().readFileAlloc(vm.allocator(), path, 10e8) catch stdx.fatal();
    defer vm.allocator().free(content);
    // TODO: Use allocOwnedString.
    return vm.allocRawString(content) catch stdx.fatal();
}

pub fn readLine(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("readLine", "0.1", "Use getInput() instead.", &.{});
    return getInput(vm, args, nargs);
}

fn List(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (args[0].isList()) {
        return args[0];
    } else {
        vm.release(args[0]);
        return vm.returnPanic("Not a List.");
    }
}

fn taglit(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (args[0].isTagLiteral()) {
        return args[0];
    } else {
        vm.release(args[0]);
        return vm.returnPanic("Not a tag literal.");
    }
}

pub fn string(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    defer vm.release(args[0]);
    if (val.isString()) {
        return val;
    } else {
        const str = vm.valueToTempString(val);
        return vm.allocStringInfer(str) catch stdx.fatal();
    }
}

pub fn typeid(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    defer vm.release(args[0]);
    return Value.initF64(@intToFloat(f64, args[0].getTypeId()));
}

pub fn valtag(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    defer vm.release(val);
    switch (val.getUserTag()) {
        .number => return Value.initTagLiteral(@enumToInt(TagLit.number)),
        .object => return Value.initTagLiteral(@enumToInt(TagLit.object)),
        .errorVal => return Value.initTagLiteral(@enumToInt(TagLit.err)),
        .boolean => return Value.initTagLiteral(@enumToInt(TagLit.bool)),
        .map => return Value.initTagLiteral(@enumToInt(TagLit.map)),
        .int => return Value.initTagLiteral(@enumToInt(TagLit.int)),
        .list => return Value.initTagLiteral(@enumToInt(TagLit.list)),
        .string => return Value.initTagLiteral(@enumToInt(TagLit.string)),
        .rawstring => return Value.initTagLiteral(@enumToInt(TagLit.rawstring)),
        .fiber => return Value.initTagLiteral(@enumToInt(TagLit.fiber)),
        .nativeFunc,
        .closure,
        .lambda => return Value.initTagLiteral(@enumToInt(TagLit.function)),
        .none => return Value.initTagLiteral(@enumToInt(TagLit.none)),
        .symbol => return Value.initTagLiteral(@enumToInt(TagLit.symbol)),
        .opaquePtr => return Value.initTagLiteral(@enumToInt(TagLit.pointer)),
        else => fmt.panic("Unsupported {}", &.{fmt.v(val.getUserTag())}),
    }
}

pub fn writeFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const path = vm.valueToString(args[0]) catch stdx.fatal();
    defer vm.allocator().free(path);
    var content: []const u8 = undefined;
    if (args[1].isRawString()) {
        content = args[1].asRawString();
    } else {
        content = vm.valueToTempString(args[1]);
    }
    std.fs.cwd().writeFile(path, content) catch stdx.fatal();
    return Value.None;
}

pub fn rawstring(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const str = vm.valueToTempString(args[0]);
    defer vm.release(args[0]);
    return vm.allocRawString(str) catch stdx.fatal();
}

pub fn evalJS(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const str = vm.valueToTempString(args[0]);
    hostEvalJS(str.ptr, str.len);
    return Value.None;
}

extern fn hostEvalJS(ptr: [*]const u8, len: usize) void;