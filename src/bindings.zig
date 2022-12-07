const std = @import("std");
const stdx = @import("stdx");
const builtin = @import("builtin");
const tcc = @import("tcc");

const cy = @import("cyber.zig");
const Value = cy.Value;
const vm_ = @import("vm.zig");
const TrackGlobalRC = vm_.TrackGlobalRC;
const gvm = &vm_.gvm;

const debug = builtin.mode == .Debug;
const log = stdx.log.scoped(.bindings);

const TagLitInt = 0;

pub fn bindCore(self: *cy.VM) !void {
    // Init compile time builtins.
    const resize = try self.ensureMethodSymKey("resize");
    var id = try self.addStruct("List");
    std.debug.assert(id == cy.ListS);
    try self.addMethodSym(cy.ListS, resize, cy.SymbolEntry.initNativeFunc1(listResize));
    self.iteratorObjSym = try self.ensureMethodSymKey("iterator");
    try self.addMethodSym(cy.ListS, self.iteratorObjSym, cy.SymbolEntry.initNativeFunc1(listIterator));
    self.nextObjSym = try self.ensureMethodSymKey("next");
    try self.addMethodSym(cy.ListS, self.nextObjSym, cy.SymbolEntry.initNativeFunc1(listNext));
    const add = try self.ensureMethodSymKey("add");
    try self.addMethodSym(cy.ListS, add, cy.SymbolEntry.initNativeFunc1(listAdd));
    const sort = try self.ensureMethodSymKey("sort");
    try self.addMethodSym(cy.ListS, sort, cy.SymbolEntry.initNativeFunc1(listSort));
    const size = try self.ensureMethodSymKey("size");
    try self.addMethodSym(cy.ListS, size, cy.SymbolEntry.initNativeFunc1(listSize));

    id = try self.addStruct("Map");
    std.debug.assert(id == cy.MapS);
    const remove = try self.ensureMethodSymKey("remove");
    try self.addMethodSym(cy.MapS, remove, cy.SymbolEntry.initNativeFunc1(mapRemove));

    id = try self.addStruct("Closure");
    std.debug.assert(id == cy.ClosureS);

    id = try self.addStruct("Lambda");
    std.debug.assert(id == cy.LambdaS);

    id = try self.addStruct("String");
    std.debug.assert(id == cy.StringS);

    id = try self.addStruct("Fiber");
    std.debug.assert(id == cy.FiberS);

    id = try self.addStruct("Box");
    std.debug.assert(id == cy.BoxS);

    id = try self.addStruct("NativeFunc1");
    std.debug.assert(id == cy.NativeFunc1S);

    id = try self.addStruct("TccState");
    std.debug.assert(id == cy.TccStateS);

    id = try self.ensureFuncSym("std.readInput");
    self.setFuncSym(id, cy.FuncSymbolEntry.initNativeFunc1(stdReadInput));
    id = try self.ensureFuncSym("std.parseCyon");
    self.setFuncSym(id, cy.FuncSymbolEntry.initNativeFunc1(stdParseCyon));
    id = try self.ensureFuncSym("std.print");
    self.setFuncSym(id, cy.FuncSymbolEntry.initNativeFunc1(stdPrint));
    id = try self.ensureFuncSym("std.toString");
    self.setFuncSym(id, cy.FuncSymbolEntry.initNativeFunc1(stdToString));
    // id = try self.ensureFuncSym("std.dump");
    // self.setFuncSym(id, cy.FuncSymbolEntry.initNativeFunc1(stdDump));
    id = try self.ensureFuncSym("std.bindLib");
    self.setFuncSym(id, cy.FuncSymbolEntry.initNativeFunc1(stdBindLib));
    id = try self.ensureFuncSym("number");
    self.setFuncSym(id, cy.FuncSymbolEntry.initNativeFunc1(castNumber));

    try self.ensureGlobalFuncSym("readInput", "std.readInput");
    try self.ensureGlobalFuncSym("parseCyon", "std.parseCyon");
    try self.ensureGlobalFuncSym("print", "std.print");
    try self.ensureGlobalFuncSym("toString", "std.toString");
    try self.ensureGlobalFuncSym("bindLib", "std.bindLib");
    // try self.ensureGlobalFuncSym("dump", "std.dump");

    const sid = try self.ensureStruct("CFunc");
    self.structs.buf[sid].numFields = 3;
    id = try self.ensureFieldSym("sym");
    self.setFieldSym(sid, id, 0, true);
    id = try self.ensureFieldSym("args");
    self.setFieldSym(sid, id, 1, true);
    id = try self.ensureFieldSym("ret");
    self.setFieldSym(sid, id, 2, true);

    id = try self.ensureTagLitSym("int");
    std.debug.assert(id == TagLitInt);
}

export fn printInt(n: i32) void {
    std.debug.print("zig print int: {}\n", .{n});
}

export fn printU64(n: u64) void {
    std.debug.print("zig print u64: {}\n", .{n});
}

export fn printF64(n: f64) void {
    std.debug.print("zig print f64: {}\n", .{n});
}

fn stdBindLib(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const path = args[0];
    const alloc = vm.allocator();

    var lib: std.DynLib = undefined;
    if (path.isNone()) {
        lib = std.DynLib.openZ("") catch stdx.fatal();
        log.debug("loaded main exe", .{});
    } else {
        lib = std.DynLib.open(gvm.valueToTempString(path)) catch stdx.fatal();
    }
    defer lib.close();

    // Check that symbols exist.
    const cfuncs = stdx.ptrAlignCast(*cy.CyList, args[1].asPointer().?);
    var cfuncPtrs = alloc.alloc(*anyopaque, cfuncs.items().len) catch stdx.fatal();
    defer alloc.free(cfuncPtrs);
    const symf = gvm.ensureFieldSym("sym") catch stdx.fatal();
    for (cfuncs.items()) |cfunc, i| {
        const sym = gvm.valueToTempString(gvm.getField(symf, cfunc) catch stdx.fatal());
        const symz = std.cstr.addNullByte(alloc, sym) catch stdx.fatal();
        defer alloc.free(symz);
        if (lib.lookup(*anyopaque, symz)) |ptr| {
            cfuncPtrs[i] = ptr;
        } else stdx.panicFmt("Missing sym: {s}", .{sym});
    }

    // Generate c code.
    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(alloc);
    const w = csrc.writer(alloc);

    w.print("#define uint64_t unsigned long long\n", .{}) catch stdx.fatal();

    const argsf = gvm.ensureFieldSym("args") catch stdx.fatal();
    const retf = gvm.ensureFieldSym("ret") catch stdx.fatal();
    for (cfuncs.items()) |cfunc| {
        const sym = gvm.valueToTempString(gvm.getField(symf, cfunc) catch stdx.fatal());
        const cargsv = gvm.getField(argsf, cfunc) catch stdx.fatal();
        const ret = gvm.getField(retf, cfunc) catch stdx.fatal();

        const cargs = stdx.ptrAlignCast(*cy.CyList, cargsv.asPointer().?);
        const lastArg = cargs.items().len - 1;
        w.print("extern uint64_t {s}(", .{sym}) catch stdx.fatal();
        for (cargs.items()) |carg, i| {
            const argTag = carg.asTagLiteralId();
            switch (argTag) {
                TagLitInt => {
                    w.print("int", .{}) catch stdx.fatal();
                },
                else => stdx.panicFmt("Unsupported arg type: {s}", .{ gvm.getTagLitName(argTag) }),
            }
            if (i != lastArg) {
                w.print(", ", .{}) catch stdx.fatal();
            }
        }
        w.print(");\n", .{}) catch stdx.fatal();

        w.print("uint64_t cy{s}(void* vm, uint64_t** args, char numArgs) {{\n", .{sym}) catch stdx.fatal();
        // w.print("  printF64(*(double*)&args[0]);\n", .{}) catch stdx.fatal();
        const retTag = ret.asTagLiteralId();
        switch (retTag) {
            TagLitInt => {
                w.print("  int res = {s}(", .{sym}) catch stdx.fatal();
            },
            else => stdx.panicFmt("Unsupported return type: {s}", .{ gvm.getTagLitName(retTag) }),
        }

        // Gen args.
        for (cargs.items()) |carg, i| {
            const argTag = carg.asTagLiteralId();
            switch (argTag) {
                TagLitInt => {
                    w.print("(int)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                },
                else => stdx.panicFmt("Unsupported arg type: {s}", .{ gvm.getTagLitName(argTag) }),
            }
            if (i != lastArg) {
                w.print(", ", .{}) catch stdx.fatal();
            }
        }

        // End of args.
        w.print(");\n", .{}) catch stdx.fatal();

        // Gen return.
        switch (retTag) {
            TagLitInt => {
                w.print("  double dres = (double)res;\n", .{}) catch stdx.fatal();
                w.print("  return *(uint64_t*)&dres;\n", .{}) catch stdx.fatal();
            },
            else => stdx.fatal(),
        }
        w.print("}}\n", .{}) catch stdx.fatal();
    }

    w.writeByte(0) catch stdx.fatal();
    // log.debug("{s}", .{csrc.items});

    const state = tcc.tcc_new();
    // Don't include libtcc1.a.
    tcc.tcc_set_options(state, "-nostdlib");
    _ = tcc.tcc_set_output_type(state, tcc.TCC_OUTPUT_MEMORY);

    if (tcc.tcc_compile_string(state, csrc.items.ptr) == -1) {
        stdx.panic("Failed to compile c source.");
    }

    // const __fixunsdfdi = @extern(*anyopaque, .{ .name = "__fixunsdfdi", .linkage = .Strong });
    // _ = tcc.tcc_add_symbol(state, "__fixunsdfdi", __fixunsdfdi);
    _ = tcc.tcc_add_symbol(state, "printU64", printU64);
    _ = tcc.tcc_add_symbol(state, "printF64", printF64);
    _ = tcc.tcc_add_symbol(state, "printInt", printInt);

    // Add binded symbols.
    for (cfuncs.items()) |cfunc, i| {
        const sym = gvm.valueToTempString(gvm.getField(symf, cfunc) catch stdx.fatal());
        const symz = std.cstr.addNullByte(alloc, sym) catch stdx.fatal();
        defer alloc.free(symz);
        _ = tcc.tcc_add_symbol(state, symz.ptr, cfuncPtrs[i]);
    }

    if (tcc.tcc_relocate(state, tcc.TCC_RELOCATE_AUTO) < 0) {
        stdx.panic("Failed to relocate compiled code.");
    }

    // Create vm function pointers and put in map.
    const map = gvm.allocEmptyMap() catch stdx.fatal();
    const cyState = gvm.allocTccState(state.?) catch stdx.fatal();
    gvm.retainInc(cyState, @intCast(u32, cfuncs.items().len - 1));
    for (cfuncs.items()) |cfunc| {
        const sym = gvm.valueToTempString(gvm.getField(symf, cfunc) catch stdx.fatal());
        const cySym = std.fmt.allocPrint(alloc, "cy{s}{u}", .{sym, 0}) catch stdx.fatal();
        defer alloc.free(cySym);
        const funcPtr = tcc.tcc_get_symbol(state, cySym.ptr) orelse {
            stdx.panic("Failed to get symbol.");
        };

        const func = @ptrCast(*const fn (*cy.UserVM, [*]Value, u8) Value, funcPtr);
        const key = gvm.allocString(sym) catch stdx.fatal();
        const val = gvm.allocNativeFunc1(func, cyState) catch stdx.fatal();
        gvm.setIndex(map, key, val) catch stdx.fatal();
    }

    vm_.release(args[0]);
    vm_.release(args[1]);
    return map;
}

fn castNumber(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = vm;
    _ = nargs;
    const val = args[0];
    if (val.isNumber()) {
        return val;
    } else {
        if (val.isPointer()) {
            return Value.initF64(1);
        } else {
            switch (val.getTag()) {
                cy.TagUserTag => return Value.initF64(@intToFloat(f64, val.val & @as(u64, 0xFF))),
                cy.TagUserTagLiteral => return Value.initF64(@intToFloat(f64, val.val & @as(u64, 0xFF))),
                else => return Value.initF64(1),
            }
        }
    }
}

fn stdToString(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const val = args[0];
    defer vm_.release(args[0]);
    if (val.isString()) {
        return val;
    } else {
        const str = gvm.valueToTempString(val);
        return vm.allocString(str) catch stdx.fatal();
    }
}

fn stdPrint(_: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const str = gvm.valueToTempString(args[0]);
    std.io.getStdOut().writer().print("{s}\n", .{str}) catch stdx.fatal();
    vm_.release(args[0]);
    return Value.initNone();
}

fn stdReadInput(_: *cy.UserVM, _: [*]const Value, _: u8) Value {
    const input = std.io.getStdIn().readToEndAlloc(gvm.alloc, 10e8) catch stdx.fatal();
    return gvm.allocOwnedString(input) catch stdx.fatal();
}

fn stdParseCyon(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const str = gvm.valueAsString(args[0]);
    defer vm_.release(args[0]);

    var parser = cy.Parser.init(gvm.alloc);
    defer parser.deinit();
    const val = cy.decodeCyon(gvm.alloc, &parser, str) catch stdx.fatal();
    return fromCyonValue(vm, val) catch stdx.fatal();
}

fn fromCyonValue(self: *cy.UserVM, val: cy.DecodeValueIR) !Value {
    switch (val.getValueType()) {
        .list => {
            var dlist = val.asList() catch stdx.fatal();
            defer dlist.deinit();
            const elems = try gvm.alloc.alloc(Value, dlist.arr.len);
            for (elems) |*elem, i| {
                elem.* = try fromCyonValue(self, dlist.getIndex(i));
            }
            return try gvm.allocOwnedList(elems);
        },
        .map => {
            var dmap = val.asMap() catch stdx.fatal();
            defer dmap.deinit();
            var iter = dmap.iterator();

            const mapVal = try gvm.allocEmptyMap();
            const map = stdx.ptrCastAlign(*cy.HeapObject, mapVal.asPointer().?);
            while (iter.next()) |entry| {
                const child = try fromCyonValue(self, dmap.getValue(entry.key_ptr.*));
                const key = try self.allocString(entry.key_ptr.*);
                stdMapPut(self, map, key, child);
            }
            return mapVal;
        },
        .string => {
            const str = try val.allocString();
            return try gvm.allocOwnedString(str);
        },
        .number => {
            return Value.initF64(try val.asF64());
        },
    }
}

fn stdMapPut(_: *cy.UserVM, obj: *cy.HeapObject, key: Value, value: Value) void {
    const map = stdx.ptrCastAlign(*cy.MapInner, &obj.map.inner); 
    map.put(gvm.alloc, gvm, key, value) catch stdx.fatal();
}

fn listSort(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    @setRuntimeSafety(debug);
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }

    const obj = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const list = stdx.ptrCastAlign(*cy.List(Value), &obj.list.list);
    const LessContext = struct {
        lessFn: Value,
    };
    var lessCtx = LessContext{
        .lessFn = args[0],
    };
    gvm.stackEnsureUnusedCapacity(5) catch stdx.fatal();
    const S = struct {
        fn less(ctx_: *LessContext, a: Value, b: Value) bool {
            gvm.retain(a);
            gvm.retain(b);
            gvm.retain(ctx_.lessFn);
            gvm.framePtr[2] = a;
            gvm.framePtr[3] = b;
            gvm.framePtr[4] = ctx_.lessFn;
            const retInfo = cy.buildReturnInfo(@ptrToInt(gvm.pc) - @ptrToInt(gvm.ops.ptr), (@ptrToInt(gvm.framePtr) - @ptrToInt(gvm.stack.ptr))/8, 1, false);
            var framePtr = gvm.framePtr;
            vm_.callNoInline(&gvm.pc, &framePtr, ctx_.lessFn, 0, 3, retInfo) catch stdx.fatal();
            gvm.framePtr = framePtr;
            @call(.{ .modifier = .never_inline }, vm_.evalLoopGrowStack, .{}) catch unreachable;
            const res = gvm.framePtr[0];
            return res.toBool();
        }
    };
    std.sort.sort(Value, list.items(), &lessCtx, S.less);
    vm_.releaseObject(obj);
    return Value.initNone();
}

fn listAdd(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    @setRuntimeSafety(debug);
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.List(Value), &list.list.list);
    if (inner.len == inner.buf.len) {
        inner.growTotalCapacity(gvm.alloc, inner.len + 1) catch stdx.fatal();
    }
    inner.appendAssumeCapacity(args[0]);
    vm_.releaseObject(list);
    return Value.initNone();
}

fn listNext(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    @setRuntimeSafety(debug);
    _ = args;
    _ = nargs;
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    if (list.list.nextIterIdx < list.list.list.len) {
        defer list.list.nextIterIdx += 1;
        const val = list.list.list.ptr[list.list.nextIterIdx];
        gvm.retain(val);
        return val;
    } else return Value.initNone();
}

fn listIterator(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    _ = args;
    _ = nargs;
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    gvm.retainObject(list);
    list.list.nextIterIdx = 0;
    return Value.initPtr(ptr);
}

fn listResize(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.List(Value), &list.list.list);
    const size = @floatToInt(u32, args[0].toF64());
    inner.resize(gvm.alloc, size) catch stdx.fatal();
    vm_.releaseObject(list);
    return Value.initNone();
}

fn mapRemove(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    @setRuntimeSafety(debug);
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const obj = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.MapInner, &obj.map.inner);
    _ = inner.remove(gvm, args[0]);
    return Value.initNone();
}

fn listSize(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    @setRuntimeSafety(debug);
    _ = nargs;
    _ = args;
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.List(Value), &list.list.list);
    vm_.releaseObject(list);
    return Value.initF64(@intToFloat(f64, inner.len));
}