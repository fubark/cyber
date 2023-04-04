const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const fatal = stdx.fatal;
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

const log = stdx.log.scoped(.core);

pub fn initModule(self: *cy.VMcompiler, mod: *cy.Module) !void {
    try mod.syms.ensureTotalCapacity(self.alloc, 13);

    const b = bindings.ModuleBuilder.init(self, mod);

    // Funcs.
    try b.setFunc("arrayFill", &.{bt.Any, bt.Number}, bt.List, arrayFill);
    try b.setFunc("asciiCode", &.{bt.Any}, bt.Any, asciiCode);
    if (cy.isWasm) {
        try b.setFunc("bindLib", &.{bt.Any, bt.List}, bt.Any, bindings.nop2);
        try b.setFunc("bindLib", &.{bt.Any, bt.List, bt.Map}, bt.Any, bindings.nop3);
    } else {
        try b.setFunc("bindLib", &.{bt.Any, bt.List}, bt.Any, bindLib);
        try b.setFunc("bindLib", &.{bt.Any, bt.List, bt.Map}, bt.Any, bindLibExt);
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
    try b.setFunc("exit", &.{bt.Number}, bt.None, exit);
    try b.setFunc("fetchUrl", &.{bt.Any}, bt.Any, fetchUrl);
    if (cy.hasStdFiles) {
        try b.setFunc("getInput", &.{}, bt.Any, getInput);
    } else {
        try b.setFunc("getInput", &.{}, bt.Any, bindings.nop0);
    }
    // try mod.setNativeFunc(alloc, "dump", 1, dump);
    try b.setFunc("must", &.{ bt.Any }, bt.Any, must);
    try b.setFunc("opaque", &.{ bt.Any }, bt.Pointer, coreOpaque);
    try b.setFunc("panic", &.{ bt.Any }, bt.None, panic);
    try b.setFunc("parseCyon", &.{ bt.Any }, bt.Any, parseCyon);
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
    try b.setFunc("toCyon", &.{ bt.Any }, bt.String, toCyon);
    try b.setFunc("typeid", &.{ bt.Any }, bt.Number, typeid);
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
    defer vm.release(args[0]);
    return vm.allocListFill(args[0], @floatToInt(u32, args[1].asF64())) catch stdx.fatal();
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
        return prepareThrowSymbol(vm, .UnknownError);
    };
    defer alloc.free(resp.body);
    if (resp.status != .ok) {
        log.debug("cacheUrl response status: {}", .{resp.status});
        return prepareThrowSymbol(vm, .UnknownError);
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
    defer vm.release(val);
    return cy.value.shallowCopy(@ptrCast(*cy.VM, vm), val);
}

pub fn errorReport(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const ivm = vm.internal();

    // Unwind the fp to before the function call.
    ivm.framePtr -= 4;

    cy.debug.buildStackTrace(ivm, true) catch |err| {
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
                return prepareThrowSymbol(vm, .FileNotFound),
            error.StdoutStreamTooLong =>
                return prepareThrowSymbol(vm, .StreamTooLong),
            error.StderrStreamTooLong =>
                return prepareThrowSymbol(vm, .StreamTooLong),
            else => stdx.panicFmt("exec err {}\n", .{err}),
        }
    };

    const map = vm.allocEmptyMap() catch stdx.fatal();
    const outKey = vm.allocAstring("out") catch stdx.fatal();
    const errKey = vm.allocAstring("err") catch stdx.fatal();
    defer {
        vm.release(outKey);
        vm.release(errKey);
    }

    // TODO: Use allocOwnedString
    defer alloc.free(res.stdout);
    const out = vm.allocStringInfer(res.stdout) catch stdx.fatal();
    ivm.setIndex(map, outKey, out) catch stdx.fatal();
    // TODO: Use allocOwnedString
    defer alloc.free(res.stderr);
    const err = vm.allocStringInfer(res.stderr) catch stdx.fatal();
    ivm.setIndex(map, errKey, err) catch stdx.fatal();
    if (res.term == .Exited) {
        const exitedKey = vm.allocAstring("exited") catch stdx.fatal();
        defer vm.release(exitedKey);
        ivm.setIndex(map, exitedKey, Value.initF64(@intToFloat(f64, res.term.Exited))) catch stdx.fatal();
    }
    return map;
}

pub fn exit(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const status = @floatToInt(u8, args[0].asF64());
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
            return prepareThrowSymbol(vm, .UnknownError);
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
            return prepareThrowSymbol(vm, .EndOfStream);
        } else stdx.fatal();
    };
    defer vm.allocator().free(input);
    // TODO: Use allocOwnedString
    return vm.allocRawString(input) catch stdx.fatal();
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
    const path = vm.valueToTempRawString(args[0]);
    const content = std.fs.cwd().readFileAlloc(vm.allocator(), path, 10e8) catch stdx.fatal();
    defer vm.allocator().free(content);
    // TODO: Use allocOwnedString.
    return vm.allocRawString(content) catch stdx.fatal();
}

pub fn readLine(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("readLine", "0.1", "Use getInput() instead.", &.{});
    return getInput(vm, args, nargs);
}

pub fn typeid(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    defer vm.release(args[0]);
    fmt.printDeprecated("typeid", "0.2", "Use metatype.id() instead.", &.{});
    return Value.initF64(@intToFloat(f64, args[0].getTypeId()));
}

fn valtag(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("valtag", "0.2", "Use typesym() instead.", &.{});
    return typesym(vm, args, nargs);
}

fn typeof(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    defer vm.release(val);

    const typeId = val.getTypeId();
    return cy.heap.allocMetaType(vm.internal(), @enumToInt(cy.heap.MetaTypeKind.object), typeId) catch fatal();
}

pub fn typesym(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    defer vm.release(val);
    switch (val.getUserTag()) {
        .number => return Value.initSymbol(@enumToInt(Symbol.number)),
        .object => return Value.initSymbol(@enumToInt(Symbol.object)),
        .err => return Value.initSymbol(@enumToInt(Symbol.err)),
        .boolean => return Value.initSymbol(@enumToInt(Symbol.boolean)),
        .map => return Value.initSymbol(@enumToInt(Symbol.map)),
        .int => return Value.initSymbol(@enumToInt(Symbol.int)),
        .list => return Value.initSymbol(@enumToInt(Symbol.list)),
        .string => return Value.initSymbol(@enumToInt(Symbol.string)),
        .rawstring => return Value.initSymbol(@enumToInt(Symbol.rawstring)),
        .fiber => return Value.initSymbol(@enumToInt(Symbol.fiber)),
        .nativeFunc,
        .closure,
        .lambda => return Value.initSymbol(@enumToInt(Symbol.function)),
        .none => return Value.initSymbol(@enumToInt(Symbol.none)),
        .symbol => return Value.initSymbol(@enumToInt(Symbol.symbol)),
        .metatype => return Value.initSymbol(@enumToInt(Symbol.metatype)),
        .pointer => return Value.initSymbol(@enumToInt(Symbol.pointer)),
        else => fmt.panic("Unsupported {}", &.{fmt.v(val.getUserTag())}),
    }
}

pub fn writeFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const path = vm.valueToTempRawString(args[0]);
    const pathDupe = vm.allocator().dupe(u8, path) catch fatal();
    defer vm.allocator().free(pathDupe);
    const content = vm.valueToTempRawString(args[1]);
    std.fs.cwd().writeFile(path, content) catch stdx.fatal();
    return Value.None;
}

pub fn evalJS(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const str = vm.valueToTempString(args[0]);
    hostEvalJS(str.ptr, str.len);
    return Value.None;
}

extern fn hostEvalJS(ptr: [*]const u8, len: usize) void;