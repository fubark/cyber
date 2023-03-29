const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const tcc = @import("tcc");
const cy = @import("../cyber.zig");
const Value = cy.Value;
const bindings = @import("bindings.zig");
const prepareThrowSymbol = bindings.prepareThrowSymbol;
const Symbol = bindings.Symbol;
const os = @import("os.zig");
const sema = cy.sema;
const bt = cy.types.BuiltinTypeSymIds;

const log = stdx.log.scoped(.ffi);

const DumpCGen = builtin.mode == .Debug and false;

pub const BindLibConfig = struct {
    /// Whether bindLib generates the binding to an anonymous object type as methods
    /// or a map with functions.
    genMap: bool = false,
};

const Context = struct {
    ivm: *cy.VM,
    alloc: std.mem.Allocator,
    symToCStructFields: std.AutoHashMapUnmanaged(u32, *cy.CyList) = .{},
    config: BindLibConfig,

    fn deinit(self: *@This()) void {
        self.symToCStructFields.deinit(self.alloc);
    }

    fn genFuncReleaseOps(self: *@This(), w: anytype, cargs: []const Value) !void {
        for (cargs, 0..) |carg, i| {
            if (carg.isObjectType(cy.MetaTypeT)) {
                try w.print("  icyRelease(vm, args[{}]);\n", .{i});
                // Free any child temps.
                const objType = carg.asPointer(*cy.heap.MetaType);
                const fields = self.symToCStructFields.get(objType.symId).?.items();
                for (fields, 0..) |field, fidx| {
                    if (!field.isObjectType(cy.MetaTypeT)) {
                        const fieldType = field.asSymbolId();
                        switch (@intToEnum(Symbol, fieldType)) {
                            .charPtrZ => {
                                try w.print("  icyFree(s{}.f{});\n", .{i, fidx});
                            },
                            else => {},
                        } 
                    }
                }
            } else {
                const argTag = carg.asSymbolId();
                switch (@intToEnum(Symbol, argTag)) {
                    .charPtrZ => {
                        try w.print("  icyRelease(vm, args[{}]);\n", .{i});
                        try w.print("  icyFree(str{});\n", .{i});
                    },
                    .dupeCharPtrZ => {
                        try w.print("  icyRelease(vm, args[{}]);\n", .{i});
                    },
                    .voidPtr => {
                        try w.print("  icyRelease(vm, args[{}]);\n", .{i});
                    },
                    else => {},
                }
            }
        }
    }
};

fn toResolvedParamTypeSymId(ivm: *cy.VM, val: Value) cy.sema.ResolvedSymId {
    const tag = val.asSymbolId();
    switch (@intToEnum(Symbol, tag)) {
        .bool => return bt.Boolean,
        .char => return bt.Number,
        .uchar => return bt.Number,
        .short => return bt.Number,
        .ushort => return bt.Number,
        .int => return bt.Number,
        .uint => return bt.Number,
        .long => return bt.Number,
        .ulong => return bt.Number,
        .usize => return bt.Number,
        .float => return bt.Number,
        .double => return bt.Number,
        .charPtrZ => return bt.Any,
        .dupeCharPtrZ => return bt.Any,
        .voidPtr => return bt.Pointer,
        .void => return bt.None,
        else => stdx.panicFmt("Unsupported param type: {s}", .{ ivm.getSymbolName(tag) }),
    }
}

fn toResolvedReturnTypeSymId(ivm: *cy.VM, val: Value) cy.sema.ResolvedSymId {
    const tag = val.asSymbolId();
    switch (@intToEnum(Symbol, tag)) {
        .bool => return bt.Boolean,
        .char => return bt.Number,
        .uchar => return bt.Number,
        .short => return bt.Number,
        .ushort => return bt.Number,
        .int => return bt.Number,
        .uint => return bt.Number,
        .long => return bt.Number,
        .ulong => return bt.Number,
        .usize => return bt.Number,
        .float => return bt.Number,
        .double => return bt.Number,
        .charPtrZ => return bt.Rawstring,
        .voidPtr => return bt.Pointer,
        .void => return bt.None,
        else => stdx.panicFmt("Unsupported return type: {s}", .{ ivm.getSymbolName(tag) }),
    }
}

fn toCType(ivm: *cy.VM, val: Value) []const u8 {
    const tag = val.asSymbolId();
    switch (@intToEnum(Symbol, tag)) {
        .bool => return "bool",
        .char => return "int8_t",
        .uchar => return "uint8_t",
        .short => return "int16_t",
        .ushort => return "uint16_t",
        .int => return "int",
        .uint => return "uint32_t",
        .long => return "int64_t",
        .ulong => return "uint64_t",
        .usize => return "size_t",
        .float => return "float",
        .double => return "double",
        .charPtrZ => return "char*",
        .dupeCharPtrZ => return "char*",
        .voidPtr => return "void*",
        .void => return "void",
        else => stdx.panicFmt("Unsupported arg type: {s}", .{ ivm.getSymbolName(tag) }),
    }
}

/// Assumes `uint64_t* args` is available in the scope.
fn printToCValueFromArg(ivm: *cy.VM, w: anytype, argType: Value, i: usize) !void {
    const tag = argType.asSymbolId();
    switch (@intToEnum(Symbol, tag)) {
        .bool => {
            try w.print("(args[{}] == 0x7FFC000100000001)?1:0", .{i});
        },
        .char => {
            try w.print("(int8_t)*(double*)&args[{}]", .{i});
        },
        .uchar => {
            try w.print("(uint8_t)*(double*)&args[{}]", .{i});
        },
        .short => {
            try w.print("(int16_t)*(double*)&args[{}]", .{i});
        },
        .ushort => {
            try w.print("(uint16_t)*(double*)&args[{}]", .{i});
        },
        .int => {
            try w.print("(int)*(double*)&args[{}]", .{i});
        },
        .uint => {
            try w.print("(uint32_t)*(double*)&args[{}]", .{i});
        },
        .long => {
            try w.print("(int64_t)*(double*)&args[{}]", .{i});
        },
        .ulong => {
            try w.print("(uint64_t)*(double*)&args[{}]", .{i});
        },
        .usize => {
            try w.print("(size_t)*(double*)&args[{}]", .{i});
        },
        .float => {
            try w.print("(float)*(double*)&args[{}]", .{i});
        },
        .double => {
            try w.print("*(double*)&args[{}]", .{i});
        },
        .dupeCharPtrZ => {
            try w.print("icyToCStr(vm, args[{}])", .{i});
        },
        .charPtrZ => {
            try w.print("str{}", .{i});
        },
        .voidPtr => {
            try w.print("icyGetPtr(args[{}])", .{i});
        },
        else => stdx.panicFmt("Unsupported arg type: {s}", .{ ivm.getSymbolName(tag) }),
    }
}

/// If the c value is converted to a number, this assumes `cval` is already a double.
fn printCyValue(ivm: *cy.VM, w: anytype, argType: Value, cval: []const u8) !void {
    const tag = argType.asSymbolId();
    switch (@intToEnum(Symbol, tag)) {
        .char,
        .uchar,
        .short,
        .ushort,
        .int,
        .uint,
        .long,
        .ulong,
        .usize,
        .float,
        .double => {
            // Assumes cval is already converted to double.
            try w.print("*(uint64_t*)&{s}", .{cval});
        },
        .charPtrZ => {
            try w.print("icyFromCStr(vm, {s})", .{cval});
        },
        .voidPtr => {
            try w.print("icyAllocCyPointer(vm, {s})", .{cval});
        },
        .void => {
            try w.print("0x7FFC000000000000", .{});
        },
        .bool => {
            try w.print("({s} == 1) ? 0x7FFC000100000001 : 0x7FFC000100000000", .{cval});
        },
        else => stdx.panicFmt("Unsupported arg type: {s}", .{ ivm.getSymbolName(tag) }),
    }
}

pub fn bindLib(vm: *cy.UserVM, args: [*]const Value, config: BindLibConfig) !Value {
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
            lib.* = try dlopen(exe);
        } else {
            lib.* = try dlopen("");
        }
    } else {
        lib.* = dlopen(vm.valueToTempRawString(path)) catch |err| {
            if (err == error.FileNotFound) {
                return prepareThrowSymbol(vm, .FileNotFound);
            } else {
                return err;
            }
        };
    }

    var ctx = Context{
        .ivm = ivm,
        .alloc = alloc,
        .symToCStructFields = .{},
        .config = config,
    };
    defer ctx.deinit();

    var cfuncs: std.ArrayListUnmanaged(CFuncData) = .{};
    defer cfuncs.deinit(alloc);

    const decls = args[1].asPointer(*cy.CyList);

    const argsf = try ivm.ensureFieldSym("args");
    const retf = try ivm.ensureFieldSym("ret");

    // Check that symbols exist and build the model.
    const symF = try ivm.ensureFieldSym("sym");
    const fieldsF = try ivm.ensureFieldSym("fields");
    const typeF = try ivm.ensureFieldSym("type");
    for (decls.items()) |decl| {
        if (decl.isObjectType(os.CFuncT)) {
            const sym = vm.valueToTempString(try ivm.getField(decl, symF));
            const symz = try std.cstr.addNullByte(alloc, sym);
            defer alloc.free(symz);
            if (lib.lookup(*anyopaque, symz)) |ptr| {
                // Build function signature.
                ivm.compiler.tempSyms.clearRetainingCapacity();
                if (!ctx.config.genMap) {
                    // Self param.
                    try ivm.compiler.tempSyms.append(ctx.alloc, bt.Any);
                }

                const ret = try ivm.getField(decl, retf);
                var retTypeSymId: sema.ResolvedSymId = undefined;
                if (ret.isObjectType(cy.MetaTypeT)) {
                    const objType = ret.asPointer(*cy.heap.MetaType);
                    const rtType = ivm.structs.buf[objType.symId];
                    retTypeSymId = rtType.rTypeSymId;
                } else {
                    retTypeSymId = toResolvedReturnTypeSymId(ivm, ret);
                }

                const cargsv = try ivm.getField(decl, argsf);
                const cargs = cargsv.asPointer(*cy.CyList).items();

                if (cargs.len > 0) {
                    for (cargs) |carg| {
                        if (carg.isObjectType(cy.MetaTypeT)) {
                            const objType = carg.asPointer(*cy.heap.MetaType);

                            const rtType = ivm.structs.buf[objType.symId];
                            try ivm.compiler.tempSyms.append(ctx.alloc, rtType.rTypeSymId);
                        } else {
                            try ivm.compiler.tempSyms.append(ctx.alloc, toResolvedParamTypeSymId(ivm, carg));
                        }
                    }
                }

                const rFuncSigId = try sema.ensureResolvedFuncSig(&ivm.compiler, ivm.compiler.tempSyms.items, retTypeSymId);
                try cfuncs.append(alloc, .{
                    .decl = decl,
                    .symPtr = ptr,
                    .rFuncSigId = rFuncSigId,
                });
            } else {
                log.debug("Missing sym: '{s}'", .{sym});
                return prepareThrowSymbol(vm, .MissingSymbol);
            }
        } else if (decl.isObjectType(os.CStructT)) {
            const val = try ivm.getField(decl, typeF);
            if (val.isObjectType(cy.MetaTypeT)) {
                const objType = val.asPointer(*cy.heap.MetaType);
                if (objType.type == @enumToInt(cy.heap.MetaTypeKind.object)) {
                    if (!ctx.symToCStructFields.contains(objType.symId)) {
                        const fields = try ivm.getField(decl, fieldsF);
                        try ctx.symToCStructFields.put(alloc, objType.symId, fields.asPointer(*cy.CyList));
                    } else {
                        log.debug("Object type already declared.", .{});
                        return prepareThrowSymbol(vm, .InvalidArgument);
                    }
                } else {
                    log.debug("Not an object Symbol", .{});
                    return prepareThrowSymbol(vm, .InvalidArgument);
                }
            } else {
                log.debug("Not a Symbol", .{});
                return prepareThrowSymbol(vm, .InvalidArgument);
            }
        } else {
            log.debug("Not a CFunc or CStruct", .{});
            return prepareThrowSymbol(vm, .InvalidArgument);
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
        // TODO: Check for 32bit addressing.
        \\#define size_t uint64_t
        \\#define int8_t signed char
        \\#define uint8_t unsigned char
        \\#define int16_t short
        \\#define uint16_t unsigned short
        \\#define uint32_t unsigned int
        \\#define PointerMask 0xFFFC000000000000
        \\typedef struct UserVM *UserVM;
        \\extern char* icyToCStr(UserVM*, uint64_t);
        \\extern uint64_t icyFromCStr(UserVM*, char*);
        \\extern void icyFree(void*);
        \\extern void icyRelease(UserVM*, uint64_t);
        \\extern void* icyGetPtr(uint64_t);
        \\extern uint64_t icyAllocObject(UserVM*, uint32_t);
        \\extern uint64_t icyAllocCyPointer(UserVM*, void*);
        // \\extern int printf(char* fmt, ...);
        // \\extern void exit(int code);
        \\
    , .{});

    var iter = ctx.symToCStructFields.iterator();
    while (iter.next()) |e| {
        const objSymId = e.key_ptr.*;
        const fields = e.value_ptr.*;

        // Generate C struct.
        try w.print("typedef struct Struct{} {{\n", .{objSymId});
        for (fields.items(), 0..) |field, i| {
            try w.print("  {s} f{};\n", .{toCType(ivm, field), i});
        }
        try w.print("}} Struct{};\n", .{objSymId});

        // Generate to C struct conv.
        try w.print("Struct{} toStruct{}(UserVM* vm, uint64_t val) {{\n", .{objSymId, objSymId});
        // Get pointer to first cyber object field.
        try w.print("  uint64_t* args = (uint64_t*)(val & ~PointerMask) + 1;\n", .{});
        try w.print("  Struct{} res;\n", .{objSymId, });
        for (fields.items(), 0..) |field, i| {
            try w.print("  res.f{} = ", .{i});
            if (field.isObjectType(cy.MetaTypeT)) {
                const objType = field.asPointer(*cy.heap.MetaType);
                try w.print("toStruct{}(vm, args[{}])", .{objType.symId, i});
            } else {
                const tag = field.asSymbolId();
                switch (@intToEnum(Symbol, tag)) {
                    .charPtrZ => {
                        try w.print("icyToCStr(vm, args[{}])", .{i});
                    },
                    else => {
                        try printToCValueFromArg(ivm, w, field, i);
                    }
                }
            }
            try w.print(";\n", .{});
        }
        try w.print("  return res;\n", .{});
        try w.print("}}\n", .{});

        // Generate from C struct conv.
        try w.print("uint64_t fromStruct{}(UserVM* vm, Struct{} val) {{\n", .{objSymId, objSymId});
        try w.print("  uint64_t obj = icyAllocObject(vm, {});\n", .{objSymId});
        try w.print("  uint64_t* args = (uint64_t*)(obj & ~PointerMask) + 1;\n", .{});
        var buf: [8]u8 = undefined;
        for (fields.items(), 0..) |field, i| {
            if (!field.isObjectType(cy.SymbolT)) {
                const fieldTag = field.asSymbolId();
                switch (@intToEnum(Symbol, fieldTag)) {
                    .char,
                    .uchar,
                    .short,
                    .ushort,
                    .int,
                    .uint,
                    .long,
                    .ulong,
                    .usize,
                    .float => {
                        try w.print("  double arg{} = (double)val.f{};\n", .{i, i});
                        const argStr = try std.fmt.bufPrint(&buf, "arg{}", .{i});
                        try w.print("  args[{}] = ", .{i});
                        try printCyValue(ivm, w, field, argStr);
                        try w.print(";\n", .{});
                        continue;
                    },
                    else => {},
                }
            }
            const argStr = try std.fmt.bufPrint(&buf, "val.f{}", .{i});
            try w.print("  args[{}] = ", .{i});
            try printCyValue(ivm, w, field, argStr);
            try w.print(";\n", .{});
        }
        try w.print("  return obj;\n", .{});
        try w.print("}}\n", .{});

        // Generate ptrTo[Object].
        if (config.genMap) {
            try w.print("uint64_t cyPtrTo{s}(UserVM* vm, uint64_t* args, char numArgs) {{\n", .{ivm.structs.buf[objSymId].name});
        } else {
            try w.print("uint64_t cyPtrTo{s}(UserVM* vm, uint64_t recv, uint64_t* args, char numArgs) {{\n", .{ivm.structs.buf[objSymId].name});
        }
        try w.print("  uint64_t ptr = *((uint64_t*)(args[0] & ~PointerMask) + 1);\n", .{});
        try w.print("  uint64_t res = fromStruct{}(vm, *(Struct{}*)ptr);\n", .{objSymId, objSymId});
        try w.print("  icyRelease(vm, args[0]);\n", .{});
        if (!config.genMap) {
            try w.print("  icyRelease(vm, recv);\n", .{});
        }
        try w.print("  return res;\n", .{});
        try w.print("}}\n", .{});
    }

    // Begin func binding generation.

    for (cfuncs.items) |cfunc| {
        try genCFunc(&ctx, vm, w, cfunc);
    }

    try w.writeByte(0);
    if (DumpCGen) {
        log.debug("{s}", .{csrc.items});
    }

    const state = tcc.tcc_new();
    // Don't include libtcc1.a.
    tcc.tcc_set_options(state, "-nostdlib");
    _ = tcc.tcc_set_output_type(state, tcc.TCC_OUTPUT_MEMORY);

    if (tcc.tcc_compile_string(state, csrc.items.ptr) == -1) {
        stdx.panic("Failed to compile c source.");
    }

    // const __floatundisf = @extern(*anyopaque, .{ .name = "__floatundisf", .linkage = .Strong });
    if (builtin.cpu.arch != .aarch64) {
        _ = tcc.tcc_add_symbol(state, "__fixunsdfdi", __fixunsdfdi);
        _ = tcc.tcc_add_symbol(state, "__floatundidf", __floatundidf);
    }
    // _ = tcc.tcc_add_symbol(state, "__floatundisf", __floatundisf);
    // _ = tcc.tcc_add_symbol(state, "printf", std.c.printf);
    // _ = tcc.tcc_add_symbol(state, "exit", std.c.exit);
    // _ = tcc.tcc_add_symbol(state, "breakpoint", breakpoint);
    _ = tcc.tcc_add_symbol(state, "icyFromCStr", fromCStr);
    _ = tcc.tcc_add_symbol(state, "icyToCStr", toCStr);
    _ = tcc.tcc_add_symbol(state, "icyFree", cFree);
    _ = tcc.tcc_add_symbol(state, "icyRelease", cRelease);
    _ = tcc.tcc_add_symbol(state, "icyGetPtr", cGetPtr);
    _ = tcc.tcc_add_symbol(state, "icyAllocCyPointer", cAllocCyPointer);
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

    if (config.genMap) {
        // Create map with binded C-functions as functions.
        const map = vm.allocEmptyMap() catch stdx.fatal();

        const cyState = try cy.heap.allocTccState(ivm, state.?, lib);
        cy.arc.retainInc(ivm, cyState, @intCast(u32, cfuncs.items.len + ctx.symToCStructFields.size - 1));

        for (cfuncs.items) |cfunc| {
            const sym = vm.valueToTempString(try ivm.getField2(cfunc.decl, symF));
            const symGen = try std.fmt.allocPrint(alloc, "cy{s}{u}", .{sym, 0});
            defer alloc.free(symGen);
            const funcPtr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
                stdx.panic("Failed to get symbol.");
            };

            const symKey = vm.allocAstring(sym) catch stdx.fatal();
            const cargsv = try ivm.getField2(cfunc.decl, argsf);
            const cargs = cargsv.asPointer(*cy.CyList).items();
            const func = stdx.ptrAlignCast(*const fn (*cy.UserVM, [*]const Value, u8) Value, funcPtr);

            const rFuncSigId = try vm.ensureUntypedFuncSig(@intCast(u32, cargs.len));
            const funcVal = cy.heap.allocNativeFunc1(ivm, func, @intCast(u32, cargs.len), rFuncSigId, cyState) catch stdx.fatal();
            ivm.setIndex(map, symKey, funcVal) catch stdx.fatal();
        }
        iter = ctx.symToCStructFields.iterator();
        while (iter.next()) |e| {
            const objSymId = e.key_ptr.*;
            const typeName = ivm.structs.buf[objSymId].name;
            const symGen = try std.fmt.allocPrint(alloc, "cyPtrTo{s}{u}", .{typeName, 0});
            defer alloc.free(symGen);
            const funcPtr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
                stdx.panic("Failed to get symbol.");
            };

            const symKey = vm.allocAstringConcat("ptrTo", typeName) catch stdx.fatal();
            const func = stdx.ptrAlignCast(*const fn (*cy.UserVM, [*]const Value, u8) Value, funcPtr);

            const rFuncSigId = try vm.ensureUntypedFuncSig(1);
            const funcVal = cy.heap.allocNativeFunc1(ivm, func, 1, rFuncSigId, cyState) catch stdx.fatal();
            ivm.setIndex(map, symKey, funcVal) catch stdx.fatal();
        }
        success = true;
        return map;
    } else {
        // Create anonymous struct with binded C-functions as methods.
        const sid = try ivm.addAnonymousStruct("BindLib");
        const tccField = try ivm.ensureFieldSym("tcc");
        ivm.structs.buf[sid].numFields = 1;
        try ivm.addFieldSym(sid, tccField, 0);

        const cyState = try cy.heap.allocTccState(ivm, state.?, lib);
        for (cfuncs.items) |cfunc| {
            const sym = vm.valueToTempString(try ivm.getField2(cfunc.decl, symF));
            const cySym = try std.fmt.allocPrint(alloc, "cy{s}{u}", .{sym, 0});
            defer alloc.free(cySym);
            const funcPtr = tcc.tcc_get_symbol(state, cySym.ptr) orelse {
                stdx.panic("Failed to get symbol.");
            };

            const cargsv = try ivm.getField2(cfunc.decl, argsf);
            const cargs = cargsv.asPointer(*cy.CyList).items();

            const func = stdx.ptrAlignCast(cy.NativeObjFuncPtr, funcPtr);

            const methodSym = try ivm.ensureMethodSym(sym, @intCast(u32, cargs.len));
            try @call(.never_inline, ivm.addMethodSym, .{sid, methodSym, cy.MethodSym.initNativeFunc1(cfunc.rFuncSigId, func) });
        }
        iter = ctx.symToCStructFields.iterator();
        while (iter.next()) |e| {
            const objSymId = e.key_ptr.*;
            const rtType = ivm.structs.buf[objSymId];
            const symGen = try std.fmt.allocPrint(alloc, "cyPtrTo{s}{u}", .{rtType.name, 0});
            defer alloc.free(symGen);
            const funcPtr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
                stdx.panic("Failed to get symbol.");
            };
            const func = stdx.ptrAlignCast(cy.NativeObjFuncPtr, funcPtr);

            const methodName = try std.fmt.allocPrint(alloc, "ptrTo{s}", .{rtType.name});
            defer alloc.free(methodName);
            const methodSym = try ivm.ensureMethodSym2(methodName, 1, true);
            const rFuncSigId = try sema.ensureResolvedFuncSig(&ivm.compiler, &.{ bt.Any, bt.Pointer }, rtType.rTypeSymId);
            try @call(.never_inline, ivm.addMethodSym, .{sid, methodSym, cy.MethodSym.initNativeFunc1(rFuncSigId, func) });
        }
        success = true;
        return try vm.allocObjectSmall(sid, &.{cyState});
    }
}

const CFuncData = struct {
    decl: Value,
    symPtr: *anyopaque,
    rFuncSigId: cy.sema.ResolvedFuncSigId,
};

fn genCFunc(ctx: *Context, vm: *cy.UserVM, w: anytype, cfunc: CFuncData) !void {
    const ivm = vm.internal();
    const symF = try ivm.ensureFieldSym("sym");
    const argsf = try ivm.ensureFieldSym("args");
    const retf = try ivm.ensureFieldSym("ret");
    const sym = vm.valueToTempString(try ivm.getField(cfunc.decl, symF));
    const cargsv = try ivm.getField(cfunc.decl, argsf);
    const ret = try ivm.getField(cfunc.decl, retf);

    const cargs = cargsv.asPointer(*cy.CyList).items();

    // Emit extern declaration.
    if (ret.isObjectType(cy.MetaTypeT)) {
        const objType = ret.asPointer(*cy.heap.MetaType);
        if (ctx.symToCStructFields.contains(objType.symId)) {
            try w.print("extern Struct{} {s}(", .{objType.symId, sym});
        } else {
            log.debug("CStruct not declared.", .{});
            return error.InvalidArgument;
        }
    } else {
        try w.print("extern {s} {s}(", .{toCType(ivm, ret), sym});
    }
    if (cargs.len > 0) {
        const lastArg = cargs.len-1;
        for (cargs, 0..) |carg, i| {
            if (carg.isObjectType(cy.MetaTypeT)) {
                const objType = carg.asPointer(*cy.heap.MetaType);
                if (ctx.symToCStructFields.contains(objType.symId)) {
                    try w.print("Struct{}", .{objType.symId});
                } else {
                    log.debug("CStruct not declared.", .{});
                    return error.InvalidArgument;
                }
            } else {
                try w.print("{s}", .{toCType(ivm, carg)});
            }
            if (i != lastArg) {
                try w.print(", ", .{});
            }
        }
    }
    try w.print(");\n", .{});

    if (ctx.config.genMap) {
        try w.print("uint64_t cy{s}(UserVM* vm, uint64_t* args, char numArgs) {{\n", .{sym});
    } else {
        try w.print("uint64_t cy{s}(UserVM* vm, uint64_t recv, uint64_t* args, char numArgs) {{\n", .{sym});
    }
    // w.print("  printF64(*(double*)&args[0]);\n", .{}) catch stdx.fatal();

    // Gen temp args.
    if (cargs.len > 0) {
        for (cargs, 0..) |carg, i| {
            if (carg.isObjectType(cy.MetaTypeT)) {
                const objType = carg.asPointer(*cy.heap.MetaType);
                try w.print("Struct{} s{} = toStruct{}(vm, args[{}]);\n", .{objType.symId, i, objType.symId, i});
            } else {
                const tag = carg.asSymbolId();
                switch (@intToEnum(Symbol, tag)) {
                    .charPtrZ => {
                        try w.print("  char* str{} = icyToCStr(vm, args[{}]);\n", .{i, i});
                    },
                    else => {},
                }
            }
        }
    }

    // Gen call.
    if (ret.isObjectType(cy.MetaTypeT)) {
        const objType = ret.asPointer(*cy.heap.MetaType);
        if (ctx.symToCStructFields.contains(objType.symId)) {
            try w.print("  Struct{} res = {s}(", .{objType.symId, sym});
        } else {
            log.debug("CStruct not declared.", .{});
            return error.InvalidArgument;
        }
    } else {
        const retTag = ret.asSymbolId();
        switch (@intToEnum(Symbol, retTag)) {
            .char,
            .uchar,
            .short,
            .ushort,
            .int,
            .uint,
            .long,
            .ulong,
            .usize,
            .float => {
                try w.print("  double res = (double){s}(", .{sym});
            },
            .double => {
                try w.print("  double res = {s}(", .{sym});
            },
            .charPtrZ => {
                try w.print("  char* res = {s}(", .{sym});
            },
            .voidPtr => {
                try w.print("  void* res = {s}(", .{sym});
            },
            .void => {
                try w.print("  {s}(", .{sym});
            },
            .bool => {
                try w.print("  bool res = {s}(", .{sym});
            },
            else => stdx.panicFmt("Unsupported return type: {s}", .{ ivm.getSymbolName(retTag) }),
        }
    }

    // Gen args.
    if (cargs.len > 0) {
        const lastArg = cargs.len-1;
        for (cargs, 0..) |carg, i| {
            if (carg.isObjectType(cy.MetaTypeT)) {
                try w.print("s{}", .{i});
            } else {
                try printToCValueFromArg(ivm, w, carg, i);
            }
            if (i != lastArg) {
                try w.print(", ", .{});
            }
        }
    }

    // End of args.
    try w.print(");\n", .{});

    try ctx.genFuncReleaseOps(w, cargs);
    if (!ctx.config.genMap) {
        // Release obj.
        try w.print("  icyRelease(vm, recv);\n", .{});
    }

    // Gen return.
    try w.print("  return ", .{});
    if (ret.isObjectType(cy.MetaTypeT)) {
        const objType = ret.asPointer(*cy.heap.MetaType);
        try w.print("fromStruct{}(vm, res)", .{ objType.symId });
    } else {
        try printCyValue(ivm, w, ret, "res");
    }
    try w.print(";\n", .{});

    try w.print("}}\n", .{});
}

fn dlopen(path: []const u8) !std.DynLib {
    if (builtin.os.tag == .linux and builtin.link_libc) {
        const path_c = try std.os.toPosixPath(path);
        // Place the lookup scope of the symbols in this library ahead of the global scope.
        const RTLD_DEEPBIND = 0x00008;
        return std.DynLib{
            .handle = std.os.system.dlopen(&path_c, std.os.system.RTLD.LAZY | RTLD_DEEPBIND) orelse {
                return error.FileNotFound;
            }
        };
    } else {
        return std.DynLib.open(path);
    }
}

extern fn __floatundidf(u64) f64;
extern fn __fixunsdfdi(f64) u64;
extern fn memmove(dst: *anyopaque, src: *anyopaque, num: usize) *anyopaque;

fn fromCStr(vm: *cy.UserVM, ptr: [*:0]const u8) callconv(.C) Value {
    const slice = std.mem.span(ptr);
    return vm.allocRawString(slice) catch stdx.fatal();
}

fn toCStr(vm: *cy.UserVM, val: Value) callconv(.C) [*]const u8 {
    var str: []const u8 = undefined;
    if (val.isRawString()) {
        str = val.asRawString();
    } else {
        str = vm.valueToTempString(val);
    }
    const dupe = @ptrCast([*]u8, std.c.malloc(str.len + 1));
    std.mem.copy(u8, dupe[0..str.len], str);
    dupe[str.len] = 0;
    return dupe;
}

fn cFree(ptr: ?*anyopaque) callconv(.C) void {
    std.c.free(ptr);
}

fn cRelease(vm: *cy.UserVM, val: Value) callconv(.C) void {
    vm.release(val);
}

fn cGetPtr(val: Value) callconv(.C) ?*anyopaque {
    return val.asPointer(*cy.Pointer).ptr;
}

fn cAllocCyPointer(vm: *cy.UserVM, ptr: ?*anyopaque) callconv(.C) Value {
    return cy.heap.allocPointer(vm.internal(), ptr) catch stdx.fatal();
}

fn cAllocObject(vm: *cy.UserVM, id: u32) callconv(.C) Value {
    const ivm = vm.internal();
    if (ivm.structs.buf[id].numFields <= 4) {
        return cy.heap.allocEmptyObjectSmall(ivm, id) catch stdx.fatal();
    } else {
        return cy.heap.allocEmptyObject(ivm, id, ivm.structs.buf[id].numFields) catch stdx.fatal();
    }
}

fn breakpoint() callconv(.C) void {
    @breakpoint();
}
