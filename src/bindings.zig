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

const TagLit_int = 0;
const TagLit_i8 = 1;
const TagLit_u8 = 2;
const TagLit_i16 = 3;
const TagLit_u16 = 4;
const TagLit_i32 = 5;
const TagLit_u32 = 6;
const TagLit_f32 = 7;
const TagLit_f64 = 8;
const TagLit_float = 9;
const TagLit_double = 10;
const TagLit_charPtrZ = 11;
const TagLit_ptr = 12;
const TagLit_AssertError = 13;

const StdSection = ".std";

pub fn bindCore(self: *cy.VM) !void {
    @setCold(true);

    // Init compile time builtins.
    const resize = try self.ensureMethodSymKey("resize");
    var id = try self.addStruct("List");
    std.debug.assert(id == cy.ListS);
    try self.addMethodSym(cy.ListS, resize, cy.SymbolEntry.initNativeFunc1(listResize));
    self.iteratorObjSym = try self.ensureMethodSymKey("iterator");
    try self.addMethodSym(cy.ListS, self.iteratorObjSym, cy.SymbolEntry.initNativeFunc1(listIterator));
    self.nextObjSym = try self.ensureMethodSymKey("next");
    try self.addMethodSym(cy.ListS, self.nextObjSym, cy.SymbolEntry.initNativeFunc1(listNext));
    self.pairIteratorObjSym = try self.ensureMethodSymKey("pairIterator");
    try self.addMethodSym(cy.ListS, self.pairIteratorObjSym, cy.SymbolEntry.initNativeFunc1(listIterator));
    self.nextPairObjSym = try self.ensureMethodSymKey("nextPair");
    try self.addMethodSym(cy.ListS, self.nextPairObjSym, cy.SymbolEntry.initNativeFunc2(listNextPair));
    const add = try self.ensureMethodSymKey("add");
    try self.addMethodSym(cy.ListS, add, cy.SymbolEntry.initNativeFunc1(listAdd));
    const insert = try self.ensureMethodSymKey("insert");
    try self.addMethodSym(cy.ListS, insert, cy.SymbolEntry.initNativeFunc1(listInsert));
    const remove = try self.ensureMethodSymKey("remove");
    try self.addMethodSym(cy.ListS, remove, cy.SymbolEntry.initNativeFunc1(listRemove));
    const sort = try self.ensureMethodSymKey("sort");
    try self.addMethodSym(cy.ListS, sort, cy.SymbolEntry.initNativeFunc1(listSort));
    const size = try self.ensureMethodSymKey("size");
    try self.addMethodSym(cy.ListS, size, cy.SymbolEntry.initNativeFunc1(listSize));

    id = try self.addStruct("Map");
    std.debug.assert(id == cy.MapS);
    try self.addMethodSym(cy.MapS, remove, cy.SymbolEntry.initNativeFunc1(mapRemove));
    try self.addMethodSym(cy.MapS, size, cy.SymbolEntry.initNativeFunc1(mapSize));

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

    id = try self.addStruct("OpaquePtr");
    std.debug.assert(id == cy.OpaquePtrS);

    const sid = try self.ensureStruct("CFunc");
    self.structs.buf[sid].numFields = 3;
    id = try self.ensureFieldSym("sym");
    try self.addFieldSym(sid, id, 0);
    id = try self.ensureFieldSym("args");
    try self.addFieldSym(sid, id, 1);
    id = try self.ensureFieldSym("ret");
    try self.addFieldSym(sid, id, 2);

    id = try self.ensureTagLitSym("int");
    std.debug.assert(id == TagLit_int);
    id = try self.ensureTagLitSym("i8");
    std.debug.assert(id == TagLit_i8);
    id = try self.ensureTagLitSym("u8");
    std.debug.assert(id == TagLit_u8);
    id = try self.ensureTagLitSym("i16");
    std.debug.assert(id == TagLit_i16);
    id = try self.ensureTagLitSym("u16");
    std.debug.assert(id == TagLit_u16);
    id = try self.ensureTagLitSym("i32");
    std.debug.assert(id == TagLit_i32);
    id = try self.ensureTagLitSym("u32");
    std.debug.assert(id == TagLit_u32);
    id = try self.ensureTagLitSym("f32");
    std.debug.assert(id == TagLit_f32);
    id = try self.ensureTagLitSym("f64");
    std.debug.assert(id == TagLit_f64);
    id = try self.ensureTagLitSym("float");
    std.debug.assert(id == TagLit_float);
    id = try self.ensureTagLitSym("double");
    std.debug.assert(id == TagLit_double);
    id = try self.ensureTagLitSym("charPtrZ");
    std.debug.assert(id == TagLit_charPtrZ);
    id = try self.ensureTagLitSym("ptr");
    std.debug.assert(id == TagLit_ptr);

    id = try self.ensureTagLitSym("AssertError");
    std.debug.assert(id == TagLit_AssertError);
}

pub fn testEqNear(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = vm;
    _ = nargs;
    const act = args[0];
    const exp = args[1];

    const actType = act.getUserTag();
    const expType = exp.getUserTag();
    if (actType == expType) {
        if (actType == .number) {
            if (std.math.approxEqAbs(f64, act.asF64(), exp.asF64(), 1e-5)) {
                return Value.True;
            } else {
                println("actual: {} != {}", .{act.asF64(), exp.asF64()});
                return Value.initErrorTagLit(TagLit_AssertError);
            }
        } else {
            println("Expected number, actual: {}", .{actType});
            return Value.initErrorTagLit(TagLit_AssertError);
        }
    } else {
        println("Types do not match:", .{});
        println("actual: {} != {}", .{actType, expType});
        return Value.initErrorTagLit(TagLit_AssertError);
    }
}

pub fn testEq(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const act = args[0];
    const exp = args[1];
    defer {
        vm.release(act);
        vm.release(exp);
    }

    const actType = act.getUserTag();
    const expType = exp.getUserTag();
    if (actType == expType) {
        switch (actType) {
            .number => {
                if (act.asF64() == exp.asF64()) {
                    return Value.True;
                } else {
                    println("actual: {} != {}", .{act.asF64(), exp.asF64()});
                    return Value.initErrorTagLit(TagLit_AssertError);
                }
            },
            .string => {
                if (std.mem.eql(u8, gvm.valueAsString(act), gvm.valueAsString(exp))) {
                    return Value.True;
                } else {
                    println("actual: '{s}' != '{s}'", .{gvm.valueAsString(act), gvm.valueAsString(exp)});
                    return Value.initErrorTagLit(TagLit_AssertError);
                }
            },
            .opaquePtr => {
                const actPtr = stdx.ptrAlignCast(*cy.OpaquePtr, act.asPointer().?).ptr;
                const expPtr = stdx.ptrAlignCast(*cy.OpaquePtr, exp.asPointer().?).ptr;
                if (actPtr == expPtr) {
                    return Value.True;
                } else {
                    println("actual: {*} != {*}", .{actPtr, expPtr});
                    return Value.initErrorTagLit(TagLit_AssertError);
                }
            },
            .boolean => {
                const actv = act.asBool();
                const expv = exp.asBool();
                if (actv == expv) {
                    return Value.True;
                } else {
                    println("actual: {} != {}", .{actv, expv});
                    return Value.initErrorTagLit(TagLit_AssertError);
                }
            },
            .none => {
                return Value.True;
            },
            else => {
                stdx.panicFmt("Unsupported type {}", .{actType});
            }
        }
    } else {
        println("Types do not match:", .{});
        println("actual: {} != {}", .{actType, expType});
        return Value.initErrorTagLit(TagLit_AssertError);
    }
}

const testStdOutLog = stdx.log.scoped(.stdout);
fn println(comptime fmt: []const u8, args: anytype) void {
    if (builtin.is_test) {
        testStdOutLog.debug(fmt, args);
    } else {
        const stdout = std.io.getStdOut().writer();
        stdout.print(fmt ++ "\n", args) catch stdx.fatal();
    }
}

export fn printInt(n: i32) void {
    std.debug.print("print int: {}\n", .{n});
}

export fn printU64(n: u64) void {
    std.debug.print("print u64: {}\n", .{n});
}

export fn printF64(n: f64) void {
    std.debug.print("print f64: {}\n", .{n});
}

export fn printF32(n: f32) void {
    std.debug.print("print f32: {}\n", .{n});
}

export fn freeCStr(ptr: [*:0]const u8, len: u32) void {
    gvm.alloc.free(ptr[0..len+1]);
}

export fn fromCStr(ptr: [*:0]const u8) Value {
    const slice = std.mem.span(ptr);
    return gvm.allocString(slice) catch stdx.fatal();
}

export fn cGetPtr(val: Value) ?*anyopaque {
    return stdx.ptrAlignCast(*cy.OpaquePtr, val.asPointer().?).ptr;
}

export fn cAllocOpaquePtr(ptr: ?*anyopaque) Value {
    return gvm.allocOpaquePtr(ptr) catch stdx.fatal();
}

export fn cRelease(val: Value) void {
    vm_.release(gvm, val);
}

export fn toCStr(val: Value, len: *u32) [*:0]const u8 {
    if (val.isPointer()) {
        const obj = stdx.ptrCastAlign(*cy.HeapObject, val.asPointer().?);
        const dupe = std.cstr.addNullByte(gvm.alloc, obj.string.ptr[0..obj.string.len]) catch stdx.fatal();
        len.* = @intCast(u32, obj.string.len);
        return dupe.ptr;
    } else {
        const slice = val.asConstStr();
        const dupe = std.cstr.addNullByte(gvm.alloc, gvm.strBuf[slice.start..slice.end]) catch stdx.fatal();
        len.* = slice.len();
        return dupe.ptr;
    }
}

pub fn coreExecCmd(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const alloc = vm.allocator();

    const list = args[0].asHeapObject(*cy.CyList);
    var buf: std.ArrayListUnmanaged([]const u8) = .{};
    defer buf.deinit(alloc);
    for (list.items()) |arg| {
        buf.append(alloc, vm.valueToString(arg) catch stdx.fatal()) catch stdx.fatal();
    }

    const res = std.ChildProcess.exec(.{
        .allocator = alloc,
        .argv = buf.items,
    }) catch |err| {
        std.debug.print("exec err {}\n", .{err});
        stdx.fatal();
    };

    const map = gvm.allocEmptyMap() catch stdx.fatal();
    const outKey = gvm.allocString("out") catch stdx.fatal();
    const out = vm.allocOwnedString(res.stdout) catch stdx.fatal();
    gvm.setIndex(map, outKey, out) catch stdx.fatal();
    const errKey = gvm.allocString("err") catch stdx.fatal();
    const err = vm.allocOwnedString(res.stderr) catch stdx.fatal();
    gvm.setIndex(map, errKey, err) catch stdx.fatal();
    return map;
}

pub fn coreFetchUrl(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const alloc = vm.allocator();
    const url = vm.valueToTempString(args[0]);
    const res = std.ChildProcess.exec(.{
        .allocator = alloc,
        .argv = &.{ "curl", url },
    }) catch stdx.fatal();
    alloc.free(res.stderr);
    return vm.allocOwnedString(res.stdout) catch stdx.fatal();
}

pub fn coreBindLib(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
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
        const sym = gvm.valueToTempString(gvm.getField(cfunc, symf) catch stdx.fatal());
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

    w.print(
        \\#define uint64_t unsigned long long
        \\#define int8_t signed char
        \\#define uint8_t unsigned char
        \\#define int16_t short
        \\#define uint16_t unsigned short
        \\#define uint32_t unsigned int
        // \\float printF32(float);
        \\extern char* icyToCStr(uint64_t, uint32_t*);
        \\extern void icyFreeCStr(char*, uint32_t);
        \\extern uint64_t icyFromCStr(char*);
        \\extern void icyRelease(uint64_t);
        \\extern void* icyGetPtr(uint64_t);
        \\extern uint64_t icyAllocOpaquePtr(void*);
        \\
    , .{}) catch stdx.fatal();

    const argsf = gvm.ensureFieldSym("args") catch stdx.fatal();
    const retf = gvm.ensureFieldSym("ret") catch stdx.fatal();
    for (cfuncs.items()) |cfunc| {
        const sym = gvm.valueToTempString(gvm.getField(cfunc, symf) catch stdx.fatal());
        const cargsv = gvm.getField(cfunc, argsf) catch stdx.fatal();
        const ret = gvm.getField(cfunc, retf) catch stdx.fatal();

        const cargs = stdx.ptrAlignCast(*cy.CyList, cargsv.asPointer().?);
        const lastArg = cargs.items().len - 1;

        // Emit extern declaration.
        w.print("extern ", .{}) catch stdx.fatal();
        const retTag = ret.asTagLiteralId();
        switch (retTag) {
            TagLit_i32,
            TagLit_int => {
                w.print("int", .{}) catch stdx.fatal();
            },
            TagLit_i8 => {
                w.print("int8_t", .{}) catch stdx.fatal();
            },
            TagLit_u8 => {
                w.print("uint8_t", .{}) catch stdx.fatal();
            },
            TagLit_i16 => {
                w.print("int16_t", .{}) catch stdx.fatal();
            },
            TagLit_u16 => {
                w.print("uint16_t", .{}) catch stdx.fatal();
            },
            TagLit_u32 => {
                w.print("uint32_t", .{}) catch stdx.fatal();
            },
            TagLit_float,
            TagLit_f32 => {
                w.print("float", .{}) catch stdx.fatal();
            },
            TagLit_double,
            TagLit_f64 => {
                w.print("double", .{}) catch stdx.fatal();
            },
            TagLit_charPtrZ => {
                w.print("char*", .{}) catch stdx.fatal();
            },
            TagLit_ptr => {
                w.print("void*", .{}) catch stdx.fatal();
            },
            else => stdx.panicFmt("Unsupported return type: {s}", .{ gvm.getTagLitName(retTag) }),
        }
        w.print(" {s}(", .{sym}) catch stdx.fatal();
        for (cargs.items()) |carg, i| {
            const argTag = carg.asTagLiteralId();
            switch (argTag) {
                TagLit_i32,
                TagLit_int => {
                    w.print("int", .{}) catch stdx.fatal();
                },
                TagLit_i8 => {
                    w.print("int8_t", .{}) catch stdx.fatal();
                },
                TagLit_u8 => {
                    w.print("uint8_t", .{}) catch stdx.fatal();
                },
                TagLit_i16 => {
                    w.print("int16_t", .{}) catch stdx.fatal();
                },
                TagLit_u16 => {
                    w.print("uint16_t", .{}) catch stdx.fatal();
                },
                TagLit_u32 => {
                    w.print("uint32_t", .{}) catch stdx.fatal();
                },
                TagLit_float,
                TagLit_f32 => {
                    w.print("float", .{}) catch stdx.fatal();
                },
                TagLit_double,
                TagLit_f64 => {
                    w.print("double", .{}) catch stdx.fatal();
                },
                TagLit_charPtrZ => {
                    w.print("char*", .{}) catch stdx.fatal();
                },
                TagLit_ptr => {
                    w.print("void*", .{}) catch stdx.fatal();
                },
                else => stdx.panicFmt("Unsupported arg type: {s}", .{ gvm.getTagLitName(argTag) }),
            }
            if (i != lastArg) {
                w.print(", ", .{}) catch stdx.fatal();
            }
        }
        w.print(");\n", .{}) catch stdx.fatal();

        w.print("uint64_t cy{s}(void* vm, uint64_t* args, char numArgs) {{\n", .{sym}) catch stdx.fatal();
        // w.print("  printF64(*(double*)&args[0]);\n", .{}) catch stdx.fatal();
        for (cargs.items()) |carg, i| {
            const argTag = carg.asTagLiteralId();
            switch (argTag) {
                TagLit_charPtrZ => {
                    w.print("  uint32_t strLen{};\n", .{i}) catch stdx.fatal();
                    w.print("  char* str{} = icyToCStr(args[{}], &strLen{});\n", .{i, i, i}) catch stdx.fatal();
                },
                else => {},
            }
        }

        switch (retTag) {
            TagLit_i8,
            TagLit_u8,
            TagLit_i16,
            TagLit_u16,
            TagLit_i32,
            TagLit_u32,
            TagLit_f32,
            TagLit_float,
            TagLit_int => {
                w.print("  double res = (double){s}(", .{sym}) catch stdx.fatal();
            },
            TagLit_f64,
            TagLit_double => {
                w.print("  double res = {s}(", .{sym}) catch stdx.fatal();
            },
            TagLit_charPtrZ => {
                w.print("  char* res = {s}(", .{sym}) catch stdx.fatal();
            },
            TagLit_ptr => {
                w.print("  void* res = {s}(", .{sym}) catch stdx.fatal();
            },
            else => stdx.panicFmt("Unsupported return type: {s}", .{ gvm.getTagLitName(retTag) }),
        }

        // Gen args.
        for (cargs.items()) |carg, i| {
            const argTag = carg.asTagLiteralId();
            switch (argTag) {
                TagLit_i32,
                TagLit_int => {
                    w.print("(int)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                },
                TagLit_i8 => {
                    w.print("(int8_t)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                },
                TagLit_u8 => {
                    w.print("(uint8_t)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                },
                TagLit_i16 => {
                    w.print("(int16_t)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                },
                TagLit_u16 => {
                    w.print("(uint16_t)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                },
                TagLit_u32 => {
                    w.print("(uint32_t)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                },
                TagLit_float,
                TagLit_f32 => {
                    w.print("(float)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                },
                TagLit_double,
                TagLit_f64 => {
                    w.print("*(double*)&args[{}]", .{i}) catch stdx.fatal();
                },
                TagLit_charPtrZ => {
                    w.print("str{}", .{i}) catch stdx.fatal();
                },
                TagLit_ptr => {
                    w.print("icyGetPtr(args[{}])", .{i}) catch stdx.fatal();
                },
                else => stdx.panicFmt("Unsupported arg type: {s}", .{ gvm.getTagLitName(argTag) }),
            }
            if (i != lastArg) {
                w.print(", ", .{}) catch stdx.fatal();
            }
        }

        // End of args.
        w.print(");\n", .{}) catch stdx.fatal();

        for (cargs.items()) |carg, i| {
            const argTag = carg.asTagLiteralId();
            switch (argTag) {
                TagLit_charPtrZ => {
                    w.print("  icyFreeCStr(str{}, strLen{});\n", .{i, i}) catch stdx.fatal();
                    w.print("  icyRelease(args[{}]);\n", .{i}) catch stdx.fatal();
                },
                TagLit_ptr => {
                    w.print("  icyRelease(args[{}]);\n", .{i}) catch stdx.fatal();
                },
                else => {},
            }
        }

        // Gen return.
        switch (retTag) {
            TagLit_i8,
            TagLit_u8,
            TagLit_i16,
            TagLit_u16,
            TagLit_i32,
            TagLit_u32,
            TagLit_f32,
            TagLit_float,
            TagLit_f64,
            TagLit_double,
            TagLit_int => {
                w.print("  return *(uint64_t*)&res;\n", .{}) catch stdx.fatal();
            },
            TagLit_charPtrZ => {
                w.print("  return icyFromCStr(res);\n", .{}) catch stdx.fatal();
            },
            TagLit_ptr => {
                w.print("  return icyAllocOpaquePtr(res);\n", .{}) catch stdx.fatal();
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

    // const __floatundisf = @extern(*anyopaque, .{ .name = "__floatundisf", .linkage = .Strong });
    // _ = tcc.tcc_add_symbol(state, "__floatundisf", __floatundisf);
    // _ = tcc.tcc_add_symbol(state, "printU64", printU64);
    // _ = tcc.tcc_add_symbol(state, "printF64", printF64);
    // _ = tcc.tcc_add_symbol(state, "printF32", printF32);
    // _ = tcc.tcc_add_symbol(state, "printInt", printInt);
    _ = tcc.tcc_add_symbol(state, "icyFromCStr", fromCStr);
    _ = tcc.tcc_add_symbol(state, "icyToCStr", toCStr);
    _ = tcc.tcc_add_symbol(state, "icyFreeCStr", freeCStr);
    _ = tcc.tcc_add_symbol(state, "icyRelease", cRelease);
    _ = tcc.tcc_add_symbol(state, "icyGetPtr", cGetPtr);
    _ = tcc.tcc_add_symbol(state, "icyAllocOpaquePtr", cAllocOpaquePtr);

    // Add binded symbols.
    for (cfuncs.items()) |cfunc, i| {
        const sym = gvm.valueToTempString(gvm.getField(cfunc, symf) catch stdx.fatal());
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
        const sym = gvm.valueToTempString(gvm.getField(cfunc, symf) catch stdx.fatal());
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

    vm.release(args[0]);
    vm.release(args[1]);
    return map;
}

pub fn coreOpaque(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = vm;
    _ = nargs;
    const val = args[0];
    if (val.isNumber()) {
        return gvm.allocOpaquePtr(@intToPtr(?*anyopaque, @floatToInt(u64, val.asF64()))) catch stdx.fatal();
    } else {
        stdx.panicFmt("Unsupported conversion", .{});
    }
}

pub fn coreNumber(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
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

pub fn coreString(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const val = args[0];
    defer vm.release(args[0]);
    if (val.isString()) {
        return val;
    } else {
        const str = gvm.valueToTempString(val);
        return vm.allocString(str) catch stdx.fatal();
    }
}

pub fn corePrint(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const str = gvm.valueToTempString(args[0]);
    const w = std.io.getStdOut().writer();
    w.writeAll(str) catch stdx.fatal();
    w.writeByte('\n') catch stdx.fatal();
    vm.release(args[0]);
    return Value.None;
}

pub fn corePrints(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const str = gvm.valueToTempString(args[0]);
    const w = std.io.getStdOut().writer();
    w.writeAll(str) catch stdx.fatal();
    vm.release(args[0]);
    return Value.None;
}

pub fn coreWriteFile(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const path = vm.valueToString(args[0]) catch stdx.fatal();
    defer vm.allocator().free(path);
    const content = vm.valueToTempString(args[1]);
    std.fs.cwd().writeFile(path, content) catch stdx.fatal();
    return Value.None;
}

pub fn coreReadFile(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const path = vm.valueToTempString(args[0]);
    const content = std.fs.cwd().readFileAlloc(vm.allocator(), path, 10e8) catch stdx.fatal();
    return vm.allocOwnedString(content) catch stdx.fatal();
}

pub fn coreReadAll(_: *cy.UserVM, _: [*]const Value, _: u8) Value {
    const input = std.io.getStdIn().readToEndAlloc(gvm.alloc, 10e8) catch stdx.fatal();
    return gvm.allocOwnedString(input) catch stdx.fatal();
}

pub fn coreReadLine(_: *cy.UserVM, _: [*]const Value, _: u8) Value {
    const input = std.io.getStdIn().reader().readUntilDelimiterAlloc(gvm.alloc, '\n', 10e8) catch stdx.fatal();
    return gvm.allocOwnedString(input) catch stdx.fatal();
}

pub fn coreParseCyon(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(StdSection) Value {
    _ = nargs;
    const str = gvm.valueAsString(args[0]);
    defer vm.release(args[0]);

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
            gvm.framePtr[4] = a;
            gvm.framePtr[5] = b;
            gvm.framePtr[6] = ctx_.lessFn;
            const retInfo = cy.buildReturnInfo(1, false);
            vm_.callNoInline(&gvm.pc, &gvm.framePtr, ctx_.lessFn, 0, 3, retInfo) catch stdx.fatal();
            @call(.{ .modifier = .never_inline }, vm_.evalLoopGrowStack, .{gvm}) catch unreachable;
            const res = gvm.framePtr[0];
            return res.toBool();
        }
    };
    std.sort.sort(Value, list.items(), &lessCtx, S.less);
    vm_.releaseObject(gvm, obj);
    return Value.None;
}

fn listRemove(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const index = @floatToInt(usize, args[0].toF64());
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.List(Value), &list.list.list);
    inner.remove(index);
    vm_.releaseObject(gvm, list);
    return Value.None;
}

fn listInsert(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const index = @floatToInt(usize, args[0].toF64());
    const value = args[1];
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.List(Value), &list.list.list);
    if (inner.len == inner.buf.len) {
        inner.growTotalCapacity(gvm.alloc, inner.len + 1) catch stdx.fatal();
    }
    inner.insertAssumeCapacity(index, value);
    vm_.releaseObject(gvm, list);
    return Value.None;
}

fn listAdd(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.List(Value), &list.list.list);
    if (inner.len == inner.buf.len) {
        inner.growTotalCapacity(gvm.alloc, inner.len + 1) catch stdx.fatal();
    }
    inner.appendAssumeCapacity(args[0]);
    // vm_.releaseObject(list);
    vm.releaseObject(list);
    return Value.None;
}

fn listNextPair(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) cy.ValuePair {
    _ = args;
    _ = nargs;
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    if (list.list.nextIterIdx < list.list.list.len) {
        defer list.list.nextIterIdx += 1;
        const val = list.list.list.ptr[list.list.nextIterIdx];
        gvm.retain(val);
        return .{
            .left = Value.initF64(@intToFloat(f64, list.list.nextIterIdx)),
            .right = val,
        };
    } else return .{
        .left = Value.None,
        .right = Value.None,
    };
}

fn listNext(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    _ = args;
    _ = nargs;
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    if (list.list.nextIterIdx < list.list.list.len) {
        defer list.list.nextIterIdx += 1;
        const val = list.list.list.ptr[list.list.nextIterIdx];
        gvm.retain(val);
        return val;
    } else return Value.None;
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
    vm_.releaseObject(gvm, list);
    return Value.None;
}

fn mapSize(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    _ = args;
    const obj = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.MapInner, &obj.map.inner);
    vm_.releaseObject(gvm, obj);
    return Value.initF64(@intToFloat(f64, inner.size));
}

fn mapRemove(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const obj = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.MapInner, &obj.map.inner);
    _ = inner.remove(gvm, args[0]);
    return Value.None;
}

fn listSize(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    @setRuntimeSafety(debug);
    _ = nargs;
    _ = args;
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.List(Value), &list.list.list);
    vm_.releaseObject(gvm, list);
    return Value.initF64(@intToFloat(f64, inner.len));
}

pub fn mathAbs(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@fabs(args[0].toF64()));
}

pub fn mathCeil(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.ceil(args[0].toF64()));
}

pub fn mathFloor(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.floor(args[0].toF64()));
}

pub fn mathRound(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.round(args[0].toF64()));
}

pub fn mathTrunc(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.trunc(args[0].toF64()));
}

pub fn mathMax(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.max(args[0].toF64(), args[1].toF64()));
}

pub fn mathMin(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.min(args[0].toF64(), args[1].toF64()));
}

pub fn mathSign(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sign(args[0].toF64()));
}

pub fn mathClz32(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@intToFloat(f64, @clz(@floatToInt(i32, args[0].toF64()))));
}

pub fn mathMul32(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@intToFloat(f64, @floatToInt(i32, args[0].toF64()) *% @floatToInt(i32, args[1].toF64())));
}

pub fn mathExp(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.exp(args[0].toF64()));
}

pub fn mathExpm1(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.expm1(args[0].toF64()));
}

pub fn mathLog(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log(f64, args[0].toF64(), args[1].toF64()));
}

pub fn mathLog1p(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log1p(args[0].toF64()));
}

pub fn mathLog10(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log10(args[0].toF64()));
}

pub fn mathLog2(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.log2(args[0].toF64()));
}

pub fn mathLn(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.ln(args[0].toF64()));
}

pub fn mathIsNaN(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initBool(std.math.isNan(args[0].toF64()));
}

pub fn mathPow(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.pow(f64, args[0].toF64(), args[1].toF64()));
}

pub fn mathHypot(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.hypot(f64, args[0].toF64(), args[1].toF64()));
}

pub fn mathSqrt(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sqrt(args[0].toF64()));
}

pub fn mathCbrt(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.cbrt(args[0].toF64()));
}

var rand = std.rand.DefaultPrng.init(0);
pub fn mathRandom(_: *cy.UserVM, _: [*]const Value, _: u8) Value {
    return Value.initF64(rand.random().float(f64));
}

pub fn mathCos(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.cos(args[0].toF64()));
}

pub fn mathSin(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sin(args[0].toF64()));
}

pub fn mathTan(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.tan(args[0].toF64()));
}

pub fn mathCosh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.cosh(args[0].toF64()));
}

pub fn mathSinh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.sinh(args[0].toF64()));
}

pub fn mathTanh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.tanh(args[0].toF64()));
}

pub fn mathAcos(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.acos(args[0].toF64()));
}

pub fn mathAsin(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.asin(args[0].toF64()));
}

pub fn mathAtan(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.atan(args[0].toF64()));
}

pub fn mathAtan2(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.atan2(f64, args[0].toF64(), args[1].toF64()));
}

pub fn mathAcosh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.acosh(args[0].toF64()));
}

pub fn mathAsinh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.asinh(args[0].toF64()));
}

pub fn mathAtanh(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.atanh(args[0].toF64()));
}

pub fn osCwd(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    const cwd = std.process.getCwdAlloc(vm.allocator()) catch stdx.fatal();
    return vm.allocOwnedString(cwd) catch stdx.fatal();
}

pub fn osRealPath(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const path = vm.valueToTempString(args[0]);
    const res = std.fs.cwd().realpathAlloc(vm.allocator(), path) catch stdx.fatal();
    return vm.allocOwnedString(res) catch stdx.fatal();
}

pub fn osGetEnvAll(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    var env = std.process.getEnvMap(vm.allocator()) catch stdx.fatal();
    defer env.deinit();

    const map = gvm.allocEmptyMap() catch stdx.fatal();
    var iter = env.iterator();
    while (iter.next()) |entry| {
        const key = gvm.allocString(entry.key_ptr.*) catch stdx.fatal();
        const val = gvm.allocString(entry.value_ptr.*) catch stdx.fatal();
        gvm.setIndex(map, key, val) catch stdx.fatal();
    }
    return map;
}

pub fn osGetEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const key = vm.valueToTempString(args[0]);
    const res = std.os.getenv(key) orelse return Value.None;
    return vm.allocString(res) catch stdx.fatal();
}

pub fn osSetEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const key = vm.valueToString(args[0]) catch stdx.fatal();
    defer vm.allocator().free(key);
    const keyz = std.cstr.addNullByte(vm.allocator(), key) catch stdx.fatal();
    defer vm.allocator().free(keyz);

    const value = vm.valueToTempString(args[1]);
    const valuez = std.cstr.addNullByte(vm.allocator(), value) catch stdx.fatal();
    defer vm.allocator().free(valuez);
    _ = setenv(keyz, valuez, 1);
    return Value.None;
}

pub fn osUnsetEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const key = vm.valueToTempString(args[0]);
    const keyz = std.cstr.addNullByte(vm.allocator(), key) catch stdx.fatal();
    defer vm.allocator().free(keyz);
    _ = unsetenv(keyz);
    return Value.None;
}

pub extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;
pub extern "c" fn unsetenv(name: [*:0]const u8) c_int;