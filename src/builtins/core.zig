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

pub fn bindLib(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return doBindLib(vm, args) catch |err| {
        log.debug("{}", .{err});
        stdx.fatal();
    };
}

fn doBindLib(vm: *cy.UserVM, args: [*]const Value) !Value {
    const CFuncData = struct {
        decl: Value,
        symPtr: *anyopaque,
    };
    const S = struct {
        fn toCType(ivm: *cy.VM, val: Value) []const u8 {
            const tag = val.asTagLiteralId();
            switch (@intToEnum(TagLit, tag)) {
                .i32,
                .int => return "int",
                .bool => return "bool",
                .i8 => return "int8_t",
                .u8 => return "uint8_t",
                .i16 => return "int16_t",
                .u16 => return "uint16_t",
                .u32 => return "uint32_t",
                .i64 => return "int64_t",
                .u64 => return "uint64_t",
                .usize => return "size_t",
                .float,
                .f32 => return "float",
                .double,
                .f64 => return "double",
                .charPtrZ => return "char*",
                .ptr => return "void*",
                .void => return "void",
                else => stdx.panicFmt("Unsupported arg type: {s}", .{ ivm.getTagLitName(tag) }),
            }
        }

        /// Assumes `uint64_t* args` is available in the scope.
        fn printToCValueFromArg(ivm: *cy.VM, w: anytype, argType: Value, i: usize) !void {
            const tag = argType.asTagLiteralId();
            switch (@intToEnum(TagLit, tag)) {
                .i32,
                .int => {
                    try w.print("(int)*(double*)&args[{}]", .{i});
                },
                .bool => {
                    try w.print("(args[{}] == 0x7FFC000100000001)?1:0", .{i});
                },
                .i8 => {
                    try w.print("(int8_t)*(double*)&args[{}]", .{i});
                },
                .u8 => {
                    try w.print("(uint8_t)*(double*)&args[{}]", .{i});
                },
                .i16 => {
                    try w.print("(int16_t)*(double*)&args[{}]", .{i});
                },
                .u16 => {
                    try w.print("(uint16_t)*(double*)&args[{}]", .{i});
                },
                .u32 => {
                    try w.print("(uint32_t)*(double*)&args[{}]", .{i});
                },
                .i64 => {
                    try w.print("(int64_t)*(double*)&args[{}]", .{i});
                },
                .u64 => {
                    try w.print("(uint64_t)*(double*)&args[{}]", .{i});
                },
                .usize => {
                    try w.print("(size_t)*(double*)&args[{}]", .{i});
                },
                .float,
                .f32 => {
                    try w.print("(float)*(double*)&args[{}]", .{i});
                },
                .double,
                .f64 => {
                    try w.print("*(double*)&args[{}]", .{i});
                },
                .charPtrZ => {
                    try w.print("icyToCStr(args[{}])", .{i});
                },
                .ptr => {
                    try w.print("icyGetPtr(args[{}])", .{i});
                },
                else => stdx.panicFmt("Unsupported arg type: {s}", .{ ivm.getTagLitName(tag) }),
            }
        }

        fn printCyValue(ivm: *cy.VM, w: anytype, argType: Value, cval: []const u8) !void {
            const tag = argType.asTagLiteralId();
            switch (@intToEnum(TagLit, tag)) {
                .i8,
                .u8,
                .i16,
                .u16,
                .i32,
                .u32,
                .i64,
                .u64,
                .usize,
                .f32,
                .float,
                .f64,
                .double,
                .int => {
                    try w.print("*(uint64_t*)&{s}", .{cval});
                },
                .charPtrZ => {
                    try w.print("icyFromCStr({s})", .{cval});
                },
                .ptr => {
                    try w.print("icyAllocOpaquePtr({s})", .{cval});
                },
                .void => {
                    try w.print("0x7FFC000000000000", .{});
                },
                .bool => {
                    try w.print("({s} == 1) ? 0x7FFC000100000001 : 0x7FFC000100000000", .{cval});
                },
                else => stdx.panicFmt("Unsupported arg type: {s}", .{ ivm.getTagLitName(tag) }),
            }
        }
    };

    const path = args[0];
    const alloc = vm.allocator();
    const ivm = @ptrCast(*cy.VM, vm);

    var success = false;

    defer {
        vm.release(args[0]);
        vm.release(args[1]);
    }

    var lib = try alloc.create(std.DynLib);
    defer {
        if (!success) {
            alloc.destroy(lib);
        }
    }

    if (path.isNone()) {
        if (builtin.os.tag == .macos) {
            const exe = try std.fs.selfExePathAlloc(alloc);
            defer alloc.free(exe);
            lib.* = try std.DynLib.open(exe);
        } else {
            lib.* = try std.DynLib.openZ("");
        }
    } else {
        lib.* = std.DynLib.open(vm.valueToTempString(path)) catch |err| {
            if (err == error.FileNotFound) {
                return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
            } else {
                return err;
            }
        };
    }

    var cfuncs: std.ArrayListUnmanaged(CFuncData) = .{};
    defer cfuncs.deinit(alloc);

    var symToCStructFields: std.AutoHashMapUnmanaged(u32, *cy.CyList) = .{};
    defer symToCStructFields.deinit(alloc);

    const decls = stdx.ptrAlignCast(*cy.CyList, args[1].asPointer().?);

    // Check that symbols exist and build the model.
    const symF = try ivm.ensureFieldSym("sym");
    const fieldsF = try ivm.ensureFieldSym("fields");
    const typeF = try ivm.ensureFieldSym("type");
    for (decls.items()) |decl| {
        if (decl.isObjectType(bindings.CFuncT)) {
            const sym = vm.valueToTempString(try ivm.getField(decl, symF));
            const symz = try std.cstr.addNullByte(alloc, sym);
            defer alloc.free(symz);
            if (lib.lookup(*anyopaque, symz)) |ptr| {
                try cfuncs.append(alloc, .{
                    .decl = decl,
                    .symPtr = ptr,
                });
            } else {
                log.debug("Missing sym: '{s}'", .{sym});
                return Value.initErrorTagLit(@enumToInt(TagLit.MissingSymbol));
            }
        } else if (decl.isObjectType(bindings.CStructT)) {
            const val = try ivm.getField(decl, typeF);
            if (val.isObjectType(cy.SymbolT)) {
                const objType = val.asHeapObject(*cy.heap.Symbol);
                if (objType.symType == @enumToInt(cy.heap.SymbolType.object)) {
                    if (!symToCStructFields.contains(objType.symId)) {
                        const fields = try ivm.getField(decl, fieldsF);
                        try symToCStructFields.put(alloc, objType.symId, fields.asHeapObject(*cy.CyList));
                    } else {
                        log.debug("Object type already declared.", .{});
                        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
                    }
                } else {
                    log.debug("Not an object Symbol", .{});
                    return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
                }
            } else {
                log.debug("Not a Symbol", .{});
                return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
            }
        } else {
            log.debug("Not a CFunc or CStruct", .{});
            return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
        }
    }

    // Generate c code.
    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(alloc);
    const w = csrc.writer(alloc);

    try w.print(
        \\#define bool _Bool
        \\#define int64_t long long
        \\#define uint64_t unsigned long long
        // Check for 32bit addressing.
        \\#define size_t uint64_t
        \\#define int8_t signed char
        \\#define uint8_t unsigned char
        \\#define int16_t short
        \\#define uint16_t unsigned short
        \\#define uint32_t unsigned int
        \\#define PointerMask 0xFFFC000000000000
        // \\float printF32(float);
        \\extern char* icyToCStr(uint64_t);
        \\extern uint64_t icyFromCStr(char*);
        \\extern void icyRelease(uint64_t);
        \\extern void* icyGetPtr(uint64_t);
        \\extern uint64_t icyAllocObject(uint32_t);
        \\extern uint64_t icyAllocOpaquePtr(void*);
        \\
    , .{});

    var iter = symToCStructFields.iterator();
    while (iter.next()) |e| {
        const objSymId = e.key_ptr.*;
        const fields = e.value_ptr.*;

        // Generate C struct.
        try w.print("typedef struct Struct{} {{\n", .{objSymId});
        for (fields.items()) |field, i| {
            try w.print("  {s} f{};\n", .{S.toCType(ivm, field), i});
        }
        try w.print("}} Struct{};\n", .{objSymId});

        // Generate to C struct conv.
        try w.print("Struct{} toStruct{}(uint64_t val) {{\n", .{objSymId, objSymId});
        // Get pointer to first cyber object field.
        try w.print("  uint64_t* args = (uint64_t*)(val & ~PointerMask) + 1;\n", .{});
        try w.print("  Struct{} res;\n", .{objSymId, });
        for (fields.items()) |field, i| {
            try w.print("  res.f{} = ", .{i});
            try S.printToCValueFromArg(ivm, w, field, i);
            try w.print(";\n", .{});
        }
        try w.print("  return res;\n", .{});
        try w.print("}}\n", .{});

        // Generate from C struct conv.
        try w.print("uint64_t fromStruct{}(Struct{} val) {{\n", .{objSymId, objSymId});
        try w.print("  uint64_t obj = icyAllocObject({});\n", .{objSymId});
        try w.print("  uint64_t* args = (uint64_t*)(obj & ~PointerMask) + 1;\n", .{});
        var buf: [8]u8 = undefined;
        for (fields.items()) |field, i| {
            const argStr = try std.fmt.bufPrint(&buf, "val.f{}", .{i});
            try w.print("  args[{}] = ", .{i});
            try S.printCyValue(ivm, w, field, argStr);
            try w.print(";\n", .{});
        }
        try w.print("  return obj;\n", .{});
        try w.print("}}\n", .{});
    }

    const argsf = try ivm.ensureFieldSym("args");
    const retf = try ivm.ensureFieldSym("ret");
    for (cfuncs.items) |cfunc| {
        const sym = vm.valueToTempString(try ivm.getField(cfunc.decl, symF));
        const cargsv = try ivm.getField(cfunc.decl, argsf);
        const ret = try ivm.getField(cfunc.decl, retf);

        const cargs = stdx.ptrAlignCast(*cy.CyList, cargsv.asPointer().?).items();

        // Emit extern declaration.
        if (ret.isObjectType(cy.SymbolT)) {
            const objType = ret.asHeapObject(*cy.heap.Symbol);
            if (symToCStructFields.contains(objType.symId)) {
                try w.print("extern Struct{} {s}(", .{objType.symId, sym});
            } else {
                log.debug("CStruct not declared.", .{});
                return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
            }
        } else {
            try w.print("extern {s} {s}(", .{S.toCType(ivm, ret), sym});
        }
        if (cargs.len > 0) {
            const lastArg = cargs.len-1;
            for (cargs) |carg, i| {
                if (carg.isObjectType(cy.SymbolT)) {
                    const objType = carg.asHeapObject(*cy.heap.Symbol);
                    if (symToCStructFields.contains(objType.symId)) {
                        try w.print("Struct{}", .{objType.symId});
                    } else {
                        log.debug("CStruct not declared.", .{});
                        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
                    }
                } else {
                    try w.print("{s}", .{S.toCType(ivm, carg)});
                }
                if (i != lastArg) {
                    try w.print(", ", .{});
                }
            }
        }
        try w.print(");\n", .{});

        try w.print("uint64_t cy{s}(void* vm, void* obj, uint64_t* args, char numArgs) {{\n", .{sym});
        // w.print("  printF64(*(double*)&args[0]);\n", .{}) catch stdx.fatal();

        // Gen call.
        if (ret.isObjectType(cy.SymbolT)) {
            const objType = ret.asHeapObject(*cy.heap.Symbol);
            if (symToCStructFields.contains(objType.symId)) {
                try w.print("  Struct{} res = {s}(", .{objType.symId, sym});
            } else {
                log.debug("CStruct not declared.", .{});
                return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
            }
        } else {
            const retTag = ret.asTagLiteralId();
            switch (@intToEnum(TagLit, retTag)) {
                .i8,
                .u8,
                .i16,
                .u16,
                .i32,
                .u32,
                .i64,
                .u64,
                .usize,
                .f32,
                .float,
                .int => {
                    try w.print("  double res = (double){s}(", .{sym});
                },
                .f64,
                .double => {
                    try w.print("  double res = {s}(", .{sym});
                },
                .charPtrZ => {
                    try w.print("  char* res = {s}(", .{sym});
                },
                .ptr => {
                    try w.print("  void* res = {s}(", .{sym});
                },
                .void => {
                    try w.print("  {s}(", .{sym});
                },
                .bool => {
                    try w.print("  bool res = {s}(", .{sym});
                },
                else => stdx.panicFmt("Unsupported return type: {s}", .{ ivm.getTagLitName(retTag) }),
            }
        }

        // Gen args.
        if (cargs.len > 0) {
            const lastArg = cargs.len-1;
            for (cargs) |carg, i| {
                if (carg.isObjectType(cy.SymbolT)) {
                    const objType = ret.asHeapObject(*cy.heap.Symbol);
                    try w.print("toStruct{}(args[{}])", .{objType.symId, i});
                } else {
                    try S.printToCValueFromArg(ivm, w, carg, i);
                }
                if (i != lastArg) {
                    try w.print(", ", .{});
                }
            }
        }

        // End of args.
        try w.print(");\n", .{});

        for (cargs) |carg, i| {
            if (carg.isObjectType(cy.SymbolT)) {
                try w.print("  icyRelease(args[{}]);\n", .{i});
            } else {
                const argTag = carg.asTagLiteralId();
                switch (@intToEnum(TagLit, argTag)) {
                    .charPtrZ => {
                        try w.print("  icyRelease(args[{}]);\n", .{i});
                    },
                    .ptr => {
                        try w.print("  icyRelease(args[{}]);\n", .{i});
                    },
                    else => {},
                }
            }
        }
        // Release obj.
        try w.print("  icyRelease(((uint64_t)obj) | 0xFFFC000000000000);\n", .{});

        // Gen return.
        try w.print("  return ", .{});
        if (ret.isObjectType(cy.SymbolT)) {
            const objType = ret.asHeapObject(*cy.heap.Symbol);
            try w.print("fromStruct{}(res)", .{ objType.symId });
        } else {
            try S.printCyValue(ivm, w, ret, "res");
        }
        try w.print(";\n", .{});

        try w.print("}}\n", .{});
    }

    try w.writeByte(0);
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
    _ = tcc.tcc_add_symbol(state, "icyRelease", cRelease);
    _ = tcc.tcc_add_symbol(state, "icyGetPtr", cGetPtr);
    _ = tcc.tcc_add_symbol(state, "icyAllocOpaquePtr", cAllocOpaquePtr);
    _ = tcc.tcc_add_symbol(state, "icyAllocObject", cAllocObject);
    if (builtin.cpu.arch == .aarch64) {
        _ = tcc.tcc_add_symbol(state, "memmove", memmove);
    }

    // Add binded symbols.
    for (cfuncs.items) |cfunc| {
        const sym = vm.valueToTempString(try ivm.getField(cfunc.decl, symF));
        const symz = try std.cstr.addNullByte(alloc, sym);
        defer alloc.free(symz);
        _ = tcc.tcc_add_symbol(state, symz.ptr, cfunc.symPtr);
    }

    if (tcc.tcc_relocate(state, tcc.TCC_RELOCATE_AUTO) < 0) {
        stdx.panic("Failed to relocate compiled code.");
    }

    // Create vm function pointers and put in anonymous struct.
    const nameId = try cy.sema.ensureNameSym(&ivm.compiler, "BindLib");
    const sid = try ivm.ensureStruct(nameId, @intCast(u32, ivm.structs.len));
    const tccField = try ivm.ensureFieldSym("tcc");
    ivm.structs.buf[sid].numFields = 1;
    try ivm.addFieldSym(sid, tccField, 0);

    const cyState = try cy.heap.allocTccState(ivm, state.?, lib);
    // ivm.retainInc(cyState, @intCast(u32, cfuncs.items().len - 1));
    for (cfuncs.items) |cfunc| {
        const sym = vm.valueToTempString(try ivm.getField2(cfunc.decl, symF));
        const cySym = try std.fmt.allocPrint(alloc, "cy{s}{u}", .{sym, 0});
        defer alloc.free(cySym);
        const funcPtr = tcc.tcc_get_symbol(state, cySym.ptr) orelse {
            stdx.panic("Failed to get symbol.");
        };

        const cargsv = try ivm.getField2(cfunc.decl, argsf);
        const cargs = stdx.ptrAlignCast(*cy.CyList, cargsv.asPointer().?).items();

        const func = stdx.ptrAlignCast(*const fn (*cy.UserVM, *anyopaque, [*]const Value, u8) Value, funcPtr);

        const methodSym = try ivm.ensureMethodSymKey(sym, @intCast(u32, cargs.len));
        // const val = ivm.allocNativeFunc1(func, @intCast(u32, cargs.len), cyState) catch stdx.fatal();
        try @call(.never_inline, ivm.addMethodSym, .{sid, methodSym, cy.MethodSym.initNativeFunc1(func) });
    }

    success = true;
    return try vm.allocObjectSmall(sid, &.{cyState});
}

extern fn memmove(dst: *anyopaque, src: *anyopaque, num: usize) *anyopaque;

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
        .symbol => return Value.initTagLiteral(@enumToInt(TagLit.symbol)),
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

export fn toCStr(val: Value) [*]const u8 {
    const str = gvm.valueAsString(val);
    const dupe = @ptrCast([*]u8, std.c.malloc(str.len + 1));
    std.mem.copy(u8, dupe[0..str.len], str);
    dupe[str.len] = 0;
    return dupe;
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

export fn cAllocObject(id: u32) Value {
    if (gvm.structs.buf[id].numFields <= 4) {
        return cy.heap.allocEmptyObjectSmall(gvm, id) catch stdx.fatal();
    } else {
        return cy.heap.allocEmptyObject(gvm, id, gvm.structs.buf[id].numFields) catch stdx.fatal();
    }
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