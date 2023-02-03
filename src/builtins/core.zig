const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const tcc = @import("tcc");
const cy = @import("../cyber.zig");
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const gvm = &vm_.gvm;
const bindings = @import("bindings.zig");
const TagLit = bindings.TagLit;
const fmt = @import("../fmt.zig");

const log = stdx.log.scoped(.core);

pub fn initModule(self: *cy.VMcompiler) !cy.Module {
    var mod = cy.Module{
        .syms = .{},
        .chunkId = cy.NullId,
        .resolvedRootSymId = cy.NullId,
    };
    try mod.syms.ensureTotalCapacity(self.alloc, 13);
    try mod.setNativeFunc(self, "arrayFill", 2, arrayFill);
    try mod.setNativeFunc(self, "asciiCode", 1, asciiCode);
    if (cy.isWasm) {
        try mod.setNativeFunc(self, "bindLib", 2, bindings.nop2);
    } else {
        try mod.setNativeFunc(self, "bindLib", 2, bindLib);
    }
    try mod.setNativeFunc(self, "bool", 1, coreBool);
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
    try mod.setNativeFunc(self, "valtag", 1, valtag);
    if (cy.hasStdFiles) {
        try mod.setNativeFunc(self, "writeFile", 2, writeFile);
    } else {
        try mod.setNativeFunc(self, "writeFile", 2, bindings.nop2);
    }
    return mod;
}

pub fn arrayFill(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    return vm.allocListFill(args[0], @floatToInt(u32, args[1].toF64())) catch stdx.fatal();
}

pub fn asciiCode(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const str = vm.valueToTempString(args[0]);
    if (str.len > 0) {
        return Value.initF64(@intToFloat(f64, str[0]));
    } else {
        return Value.None;
    }
}

pub fn bindLib(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    _ = nargs;
    const path = args[0];
    const alloc = vm.allocator();

    var success = false;

    defer {
        vm.release(args[0]);
        vm.release(args[1]);
    }

    var lib = alloc.create(std.DynLib) catch stdx.fatal();
    defer {
        if (!success) {
            alloc.destroy(lib);
        }
    }

    if (path.isNone()) {
        if (builtin.os.tag == .macos) {
            const exe = std.fs.selfExePathAlloc(alloc) catch stdx.fatal();
            defer alloc.free(exe);
            lib.* = std.DynLib.open(exe) catch |err| {
                log.debug("{}", .{err});
                stdx.fatal();
            };
        } else {
            lib.* = std.DynLib.openZ("") catch |err| {
                log.debug("{}", .{err});
                stdx.fatal();
            };
        }
    } else {
        lib.* = std.DynLib.open(vm.valueToTempString(path)) catch |err| {
            log.debug("{}", .{err});
            return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
        };
    }

    // Check that symbols exist.
    const cfuncs = stdx.ptrAlignCast(*cy.CyList, args[1].asPointer().?);
    var cfuncPtrs = alloc.alloc(*anyopaque, cfuncs.items().len) catch stdx.fatal();
    defer alloc.free(cfuncPtrs);
    const symf = gvm.ensureFieldSym("sym") catch stdx.fatal();
    for (cfuncs.items()) |cfunc, i| {
        const sym = vm.valueToTempString(gvm.getField(cfunc, symf) catch stdx.fatal());
        const symz = std.cstr.addNullByte(alloc, sym) catch stdx.fatal();
        defer alloc.free(symz);
        if (lib.lookup(*anyopaque, symz)) |ptr| {
            cfuncPtrs[i] = ptr;
        } else {
            log.debug("Missing sym: '{s}'", .{sym});
            return Value.initErrorTagLit(@enumToInt(TagLit.MissingSymbol));
        }
    }

    // Generate c code.
    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(alloc);
    const w = csrc.writer(alloc);

    w.print(
        \\#define bool _Bool
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
        const sym = vm.valueToTempString(gvm.getField(cfunc, symf) catch stdx.fatal());
        const cargsv = gvm.getField(cfunc, argsf) catch stdx.fatal();
        const ret = gvm.getField(cfunc, retf) catch stdx.fatal();

        const cargs = stdx.ptrAlignCast(*cy.CyList, cargsv.asPointer().?).items();

        // Emit extern declaration.
        w.print("extern ", .{}) catch stdx.fatal();
        const retTag = ret.asTagLiteralId();
        switch (@intToEnum(TagLit, retTag)) {
            .i32,
            .int => {
                w.print("int", .{}) catch stdx.fatal();
            },
            .i8 => {
                w.print("int8_t", .{}) catch stdx.fatal();
            },
            .u8 => {
                w.print("uint8_t", .{}) catch stdx.fatal();
            },
            .i16 => {
                w.print("int16_t", .{}) catch stdx.fatal();
            },
            .u16 => {
                w.print("uint16_t", .{}) catch stdx.fatal();
            },
            .u32 => {
                w.print("uint32_t", .{}) catch stdx.fatal();
            },
            .float,
            .f32 => {
                w.print("float", .{}) catch stdx.fatal();
            },
            .double,
            .f64 => {
                w.print("double", .{}) catch stdx.fatal();
            },
            .charPtrZ => {
                w.print("char*", .{}) catch stdx.fatal();
            },
            .ptr => {
                w.print("void*", .{}) catch stdx.fatal();
            },
            .void => {
                w.print("void", .{}) catch stdx.fatal();
            },
            .bool => {
                w.print("bool", .{}) catch stdx.fatal();
            },
            else => stdx.panicFmt("Unsupported return type: {s}", .{ gvm.getTagLitName(retTag) }),
        }
        w.print(" {s}(", .{sym}) catch stdx.fatal();
        if (cargs.len > 0) {
            const lastArg = cargs.len-1;
            for (cargs) |carg, i| {
                const argTag = carg.asTagLiteralId();
                switch (@intToEnum(TagLit, argTag)) {
                    .i32,
                    .int => {
                        w.print("int", .{}) catch stdx.fatal();
                    },
                    .bool => {
                        w.print("bool", .{}) catch stdx.fatal();
                    },
                    .i8 => {
                        w.print("int8_t", .{}) catch stdx.fatal();
                    },
                    .u8 => {
                        w.print("uint8_t", .{}) catch stdx.fatal();
                    },
                    .i16 => {
                        w.print("int16_t", .{}) catch stdx.fatal();
                    },
                    .u16 => {
                        w.print("uint16_t", .{}) catch stdx.fatal();
                    },
                    .u32 => {
                        w.print("uint32_t", .{}) catch stdx.fatal();
                    },
                    .float,
                    .f32 => {
                        w.print("float", .{}) catch stdx.fatal();
                    },
                    .double,
                    .f64 => {
                        w.print("double", .{}) catch stdx.fatal();
                    },
                    .charPtrZ => {
                        w.print("char*", .{}) catch stdx.fatal();
                    },
                    .ptr => {
                        w.print("void*", .{}) catch stdx.fatal();
                    },
                    else => stdx.panicFmt("Unsupported arg type: {s}", .{ gvm.getTagLitName(argTag) }),
                }
                if (i != lastArg) {
                    w.print(", ", .{}) catch stdx.fatal();
                }
            }
        }
        w.print(");\n", .{}) catch stdx.fatal();

        w.print("uint64_t cy{s}(void* vm, void* obj, uint64_t* args, char numArgs) {{\n", .{sym}) catch stdx.fatal();
        // w.print("  printF64(*(double*)&args[0]);\n", .{}) catch stdx.fatal();
        for (cargs) |carg, i| {
            const argTag = carg.asTagLiteralId();
            switch (@intToEnum(TagLit, argTag)) {
                .charPtrZ => {
                    w.print("  uint32_t strLen{};\n", .{i}) catch stdx.fatal();
                    w.print("  char* str{} = icyToCStr(args[{}], &strLen{});\n", .{i, i, i}) catch stdx.fatal();
                },
                else => {},
            }
        }

        switch (@intToEnum(TagLit, retTag)) {
            .i8,
            .u8,
            .i16,
            .u16,
            .i32,
            .u32,
            .f32,
            .float,
            .int => {
                w.print("  double res = (double){s}(", .{sym}) catch stdx.fatal();
            },
            .f64,
            .double => {
                w.print("  double res = {s}(", .{sym}) catch stdx.fatal();
            },
            .charPtrZ => {
                w.print("  char* res = {s}(", .{sym}) catch stdx.fatal();
            },
            .ptr => {
                w.print("  void* res = {s}(", .{sym}) catch stdx.fatal();
            },
            .void => {
                w.print("  {s}(", .{sym}) catch stdx.fatal();
            },
            .bool => {
                w.print("  bool res = {s}(", .{sym}) catch stdx.fatal();
            },
            else => stdx.panicFmt("Unsupported return type: {s}", .{ gvm.getTagLitName(retTag) }),
        }

        // Gen args.
        if (cargs.len > 0) {
            const lastArg = cargs.len-1;
            for (cargs) |carg, i| {
                const argTag = carg.asTagLiteralId();
                switch (@intToEnum(TagLit, argTag)) {
                    .i32,
                    .int => {
                        w.print("(int)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                    },
                    .bool => {
                        w.print("(args[{}] == 0x7FFC000100000001)?1:0", .{i}) catch stdx.fatal();
                    },
                    .i8 => {
                        w.print("(int8_t)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                    },
                    .u8 => {
                        w.print("(uint8_t)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                    },
                    .i16 => {
                        w.print("(int16_t)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                    },
                    .u16 => {
                        w.print("(uint16_t)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                    },
                    .u32 => {
                        w.print("(uint32_t)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                    },
                    .float,
                    .f32 => {
                        w.print("(float)*(double*)&args[{}]", .{i}) catch stdx.fatal();
                    },
                    .double,
                    .f64 => {
                        w.print("*(double*)&args[{}]", .{i}) catch stdx.fatal();
                    },
                    .charPtrZ => {
                        w.print("str{}", .{i}) catch stdx.fatal();
                    },
                    .ptr => {
                        w.print("icyGetPtr(args[{}])", .{i}) catch stdx.fatal();
                    },
                    else => stdx.panicFmt("Unsupported arg type: {s}", .{ gvm.getTagLitName(argTag) }),
                }
                if (i != lastArg) {
                    w.print(", ", .{}) catch stdx.fatal();
                }
            }
        }

        // End of args.
        w.print(");\n", .{}) catch stdx.fatal();

        for (cargs) |carg, i| {
            const argTag = carg.asTagLiteralId();
            switch (@intToEnum(TagLit, argTag)) {
                .charPtrZ => {
                    w.print("  icyFreeCStr(str{}, strLen{});\n", .{i, i}) catch stdx.fatal();
                    w.print("  icyRelease(args[{}]);\n", .{i}) catch stdx.fatal();
                },
                .ptr => {
                    w.print("  icyRelease(args[{}]);\n", .{i}) catch stdx.fatal();
                },
                else => {},
            }
        }
        // Release obj.
        w.print("  icyRelease(((uint64_t)obj) | 0xFFFC000000000000);\n", .{}) catch stdx.fatal();

        // Gen return.
        switch (@intToEnum(TagLit, retTag)) {
            .i8,
            .u8,
            .i16,
            .u16,
            .i32,
            .u32,
            .f32,
            .float,
            .f64,
            .double,
            .int => {
                w.print("  return *(uint64_t*)&res;\n", .{}) catch stdx.fatal();
            },
            .charPtrZ => {
                w.print("  return icyFromCStr(res);\n", .{}) catch stdx.fatal();
            },
            .ptr => {
                w.print("  return icyAllocOpaquePtr(res);\n", .{}) catch stdx.fatal();
            },
            .void => {
                w.print("  return 0x7FFC000000000000;\n", .{}) catch stdx.fatal();
            },
            .bool => {
                w.print("  return (res == 1) ? 0x7FFC000100000001 : 0x7FFC000100000000;\n", .{}) catch stdx.fatal();
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
        const sym = vm.valueToTempString(gvm.getField(cfunc, symf) catch stdx.fatal());
        const symz = std.cstr.addNullByte(alloc, sym) catch stdx.fatal();
        defer alloc.free(symz);
        _ = tcc.tcc_add_symbol(state, symz.ptr, cfuncPtrs[i]);
    }

    if (tcc.tcc_relocate(state, tcc.TCC_RELOCATE_AUTO) < 0) {
        stdx.panic("Failed to relocate compiled code.");
    }

    // Create vm function pointers and put in anonymous struct.
    const ivm = @ptrCast(*cy.VM, vm);
    const nameId = cy.sema.ensureNameSym(&ivm.compiler, "BindLib") catch stdx.fatal();
    const sid = ivm.ensureStruct(nameId, @intCast(u32, ivm.structs.len)) catch stdx.fatal();
    const tccField = ivm.ensureFieldSym("tcc") catch stdx.fatal();
    ivm.structs.buf[sid].numFields = 1;
    ivm.addFieldSym(sid, tccField, 0) catch stdx.fatal();

    const cyState = cy.heap.allocTccState(ivm, state.?, lib) catch stdx.fatal();
    // ivm.retainInc(cyState, @intCast(u32, cfuncs.items().len - 1));
    for (cfuncs.items()) |cfunc| {
        const sym = vm.valueToTempString(ivm.getField2(cfunc, symf) catch stdx.fatal());
        const cySym = std.fmt.allocPrint(alloc, "cy{s}{u}", .{sym, 0}) catch stdx.fatal();
        defer alloc.free(cySym);
        const funcPtr = tcc.tcc_get_symbol(state, cySym.ptr) orelse {
            stdx.panic("Failed to get symbol.");
        };

        const cargsv = ivm.getField2(cfunc, argsf) catch stdx.fatal();
        const cargs = stdx.ptrAlignCast(*cy.CyList, cargsv.asPointer().?).items();

        const func = stdx.ptrAlignCast(*const fn (*cy.UserVM, *anyopaque, [*]const Value, u8) Value, funcPtr);

        const methodSym = ivm.ensureMethodSymKey(sym, @intCast(u32, cargs.len)) catch stdx.fatal();
        // const val = ivm.allocNativeFunc1(func, @intCast(u32, cargs.len), cyState) catch stdx.fatal();
        @call(.never_inline, ivm.addMethodSym, .{sid, methodSym, cy.MethodSym.initNativeFunc1(func) }) catch stdx.fatal();
    }

    success = true;
    return vm.allocObjectSmall(sid, &.{cyState}) catch stdx.fatal();
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

    const obj = args[0].asHeapObject(*cy.HeapObject);
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
    gvm.setIndex(map, outKey, out) catch stdx.fatal();
    const errKey = vm.allocAstring("err") catch stdx.fatal();
    // TODO: Use allocOwnedString
    defer alloc.free(res.stderr);
    const err = vm.allocStringInfer(res.stderr) catch stdx.fatal();
    gvm.setIndex(map, errKey, err) catch stdx.fatal();
    if (res.term == .Exited) {
        const exitedKey = vm.allocAstring("exited") catch stdx.fatal();
        gvm.setIndex(map, exitedKey, Value.initF64(@intToFloat(f64, res.term.Exited))) catch stdx.fatal();
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
    if (cy.isWasm) {
        hostFetchUrl(url.ptr, url.len);
        return Value.None;
    } else {
        const res = std.ChildProcess.exec(.{
            .allocator = alloc,
            // Use curl, follow redirects.
            .argv = &.{ "curl", "-L", url },
            .max_output_bytes = 1024 * 1024 * 10,
        }) catch |err| {
            switch (err) {
                error.FileNotFound => 
                    return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound)),
                error.StdoutStreamTooLong =>
                    return Value.initErrorTagLit(@enumToInt(TagLit.StreamTooLong)),
                error.StderrStreamTooLong =>
                    return Value.initErrorTagLit(@enumToInt(TagLit.StreamTooLong)),
                else => stdx.panicFmt("curl err {}\n", .{err}),
            }
        };
        alloc.free(res.stderr);
        defer vm.allocator().free(res.stdout);
        // TODO: Use allocOwnedString
        return vm.allocRawString(res.stdout) catch stdx.fatal();
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
        .int => return Value.initF64(@intToFloat(f64, val.asI32())),
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

pub fn parseCyon(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    _ = nargs;
    const str = vm.valueAsString(args[0]);
    defer vm.release(args[0]);
    
    const alloc = vm.allocator();
    var parser = cy.Parser.init(alloc);
    defer parser.deinit();
    const val = cy.decodeCyon(alloc, &parser, str) catch stdx.fatal();
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
            return try cy.heap.allocOwnedList(gvm, elems);
        },
        .map => {
            var dmap = val.asMap() catch stdx.fatal();
            defer dmap.deinit();
            var iter = dmap.iterator();

            const mapVal = try cy.heap.allocEmptyMap(gvm);
            const map = stdx.ptrAlignCast(*cy.HeapObject, mapVal.asPointer().?);
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
    }
}

fn stdMapPut(_: *cy.UserVM, obj: *cy.HeapObject, key: Value, value: Value) void {
    const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner); 
    map.put(gvm.alloc, gvm, key, value) catch stdx.fatal();
}

pub fn print(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const str = vm.valueToTempString(args[0]);
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
    const str = vm.valueToTempString(args[0]);
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

pub fn string(vm: *cy.UserVM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const val = args[0];
    defer vm.release(args[0]);
    if (val.isString()) {
        return val;
    } else {
        const str = vm.valueToTempString(val);
        return vm.allocStringInfer(str) catch stdx.fatal();
    }
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
        content = args[1].asHeapObject(*cy.HeapObject).rawstring.getConstSlice();
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

export fn fromCStr(ptr: [*:0]const u8) Value {
    const slice = std.mem.span(ptr);
    return cy.heap.allocRawString(gvm, slice) catch stdx.fatal();
}

export fn toCStr(val: Value, len: *u32) [*:0]const u8 {
    if (val.isPointer()) {
        const obj = stdx.ptrAlignCast(*cy.HeapObject, val.asPointer().?);
        if (obj.common.structId == cy.AstringT) {
            const dupe = std.cstr.addNullByte(gvm.alloc, obj.astring.getConstSlice()) catch stdx.fatal();
            len.* = @intCast(u32, obj.astring.len);
            return dupe.ptr;
        } else {
            const dupe = std.cstr.addNullByte(gvm.alloc, obj.ustring.getConstSlice()) catch stdx.fatal();
            len.* = @intCast(u32, obj.ustring.len);
            return dupe.ptr;
        }
    } else {
        const slice = val.asStaticStringSlice();
        const dupe = std.cstr.addNullByte(gvm.alloc, gvm.strBuf[slice.start..slice.end]) catch stdx.fatal();
        len.* = slice.len();
        return dupe.ptr;
    }
}

export fn freeCStr(ptr: [*:0]const u8, len: u32) void {
    gvm.alloc.free(ptr[0..len+1]);
}

export fn cRelease(val: Value) void {
    cy.arc.release(gvm, val);
}

export fn cGetPtr(val: Value) ?*anyopaque {
    return stdx.ptrAlignCast(*cy.OpaquePtr, val.asPointer().?).ptr;
}

export fn cAllocOpaquePtr(ptr: ?*anyopaque) Value {
    return cy.heap.allocOpaquePtr(gvm, ptr) catch stdx.fatal();
}

// export fn printInt(n: i32) void {
//     std.debug.print("print int: {}\n", .{n});
// }

// export fn printU64(n: u64) void {
//     std.debug.print("print u64: {}\n", .{n});
// }

// export fn printF64(n: f64) void {
//     std.debug.print("print f64: {}\n", .{n});
// }

// export fn printF32(n: f32) void {
//     std.debug.print("print f32: {}\n", .{n});
// }

pub fn evalJS(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const str = vm.valueToTempString(args[0]);
    hostEvalJS(str.ptr, str.len);
    return Value.None;
}

extern fn hostEvalJS(ptr: [*]const u8, len: usize) void;