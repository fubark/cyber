const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const tcc = @import("tcc");
const app = @import("../app.zig");
const c = app.C;
const cli = app.cli;
const bindings = @import("../builtins/bindings.zig");
const prepareThrowSymbol = bindings.prepareThrowSymbol;
const Symbol = bindings.Symbol;
const os = @import("os.zig");

const log = cy.log.scoped(.ffi);

const CType = union(enum) {
    sym: Symbol,
    struct_t: *cy.Type,
    arr: struct {
        child: *CType,
        n: u32,
    },

    fn size(self: CType) usize {
        switch (self) {
            .sym => return 8,
            .struct_t => |struct_t| return struct_t.size(),
            .arr => |arr| return arr.child.size() * arr.n,
        }
    }

    fn wordSize(self: CType) usize {
        switch (self) {
            .sym => return 1,
            .struct_t => |struct_t| return struct_t.size() / 8,
            .arr => |arr| return arr.child.wordSize() * arr.n,
        }
    }

    fn deinit(self: CType, alloc: std.mem.Allocator) void {
        if (self == .arr) {
            self.arr.child.deinit(alloc);
            alloc.destroy(self.arr.child);
        }
    }
};

const CStructInfo = struct {
    type: *cy.Type,
    fields: []const CType,
};

pub const FFI = struct {
    /// Ordered to handle C decl rules.
    cstructs: std.ArrayListUnmanaged(CStructInfo),

    typeToCStruct: std.AutoHashMapUnmanaged(*cy.Type, u32),

    /// Elem type name to CType. Entry indicates that `writeToArray`/`writeFromArray` should be generated.
    carrayMap: std.StringHashMapUnmanaged(CType),

    cfuncs: std.ArrayListUnmanaged(CFuncData),

    /// Managed ExternFunc or Objects.
    managed: std.ArrayListUnmanaged(Value),

    fn toCType(self: *FFI, vm: *cy.VM, spec: Value) !CType {
        return self.toElemCType(vm, spec);
    }

    fn toElemCType(self: *FFI, vm: *cy.VM, spec: Value) !CType {
        if (spec.isObjectType(bt.MetaType)) {
            const type_ = vm.getType(@intCast(spec.asHeapObject().integer.val));
            if (self.typeToCStruct.contains(type_)) {
                return CType{ .struct_t = type_ };
            } else {
                const name = try vm.heap.getOrBufPrintValueStr(&cy.tempBuf, spec);
                log.tracev("CStruct not declared for: {s}", .{name});
                return error.InvalidArgument;
            }
            // } else if (spec.isSymbol()) {
            //     const sym = try std.meta.intToEnum(Symbol, spec.asSymbol());
            //     return CType{ .sym = sym };
        }
        return error.InvalidArgument;
    }

    fn ensureArray(self: *FFI, alloc: std.mem.Allocator, arrT: CType) !void {
        const elemName = try fmtBaseTypeName(&cy.tempBuf, arrT.arr.child.*);
        if (!self.carrayMap.contains(elemName)) {
            const elemNameDup = try alloc.dupe(u8, elemName);
            try self.carrayMap.put(alloc, elemNameDup, arrT.arr.child.*);
        }
    }
};

pub fn allocFFI(vm: *cy.VM, ffi_t: *cy.Type) !Value {
    const ffi: *FFI = @ptrCast(try vm.heap.allocHostObject(ffi_t.id(), @sizeOf(FFI)));
    ffi.* = .{
        .cstructs = .{},
        .typeToCStruct = .{},
        .carrayMap = .{},
        .cfuncs = .{},
        .managed = .{},
    };
    return Value.initPtr(ffi);
}

pub fn FFI_deinit(heap_: ?*c.Heap, obj: ?*anyopaque, finalizer: bool) callconv(.c) usize {
    const heap: *cy.Heap = @ptrCast(@alignCast(heap_));

    const alloc = heap.alloc;
    var ffi: *FFI = @ptrCast(@alignCast(obj));

    if (!finalizer) {
        for (ffi.managed.items) |val| {
            heap.release(val);
        }
    }

    for (ffi.cstructs.items) |cstruct| {
        for (cstruct.fields) |field| {
            field.deinit(alloc);
        }
        alloc.free(cstruct.fields);
    }
    ffi.cstructs.deinit(alloc);
    ffi.typeToCStruct.deinit(alloc);

    var iter = ffi.carrayMap.keyIterator();
    while (iter.next()) |key| {
        alloc.free(key.*);
    }
    ffi.carrayMap.deinit(alloc);

    for (ffi.cfuncs.items) |func| {
        alloc.free(func.namez);
    }
    ffi.cfuncs.deinit(alloc);

    ffi.managed.deinit(alloc);
    return @sizeOf(FFI);
}

pub const CGen = struct {

    // Generate C structs.
    pub fn genStructDecls(self: CGen, w: anytype) !void {
        for (self.ffi.cstructs.items) |cstruct| {
            try w.print("typedef struct Struct{} {{\n", .{cstruct.type.id()});
            for (cstruct.fields, 0..) |field, i| {
                try w.writeAll("  ");
                const name = try std.fmt.bufPrint(&cy.tempBuf, "f{}", .{i});
                try writeNamedCType(w, field, name);
                try w.writeAll(";\n");
            }
            try w.print("}} Struct{};\n", .{cstruct.type.id()});
        }
    }

    // Generate array conversions declarations.
    pub fn genArrayConvDecls(self: CGen, w: anytype) !void {
        var iter = self.ffi.carrayMap.valueIterator();
        while (iter.next()) |elem_type_ptr| {
            const elem_type = elem_type_ptr.*;
            const elemName = try fmtBaseTypeName(&cy.tempBuf, elem_type);

            // writeToArray{elem}
            try w.print("void writeToArray{s}(VM* vm, ", .{elemName});
            try writeCType(w, elem_type);
            try w.print("* dst, Value* src, int n);\n", .{});

            // writeFromArray{elem}
            try w.print("void writeFromArray{s}(VM* vm, Value* dst, ", .{elemName});
            try writeCType(w, elem_type);
            try w.print("* src, int n);\n", .{});

            // newFromArray{elem}
            try w.print("Value newFromArray{s}(VM* vm, ", .{elemName});
            try writeCType(w, elem_type);
            try w.print("* src, int n);\n", .{});
        }
    }

    // Generate struct conversions.
    pub fn genStructConvs(self: CGen, w: anytype) !void {
        const ffi = self.ffi;
        for (ffi.cstructs.items) |cstruct| {
            // Generate to C struct conv.
            try w.print("Struct{} toStruct{}(VM* vm, u8* src) {{\n", .{ cstruct.type.id(), cstruct.type.id() });
            // Get pointer to first cyber object field.
            try w.print("  Struct{} res;\n", .{cstruct.type.id()});
            var offset: usize = 0;
            for (cstruct.fields, 0..) |field, i| {
                switch (field) {
                    .struct_t => |struct_t| {
                        try w.print("  res.f{} = toStruct{}(vm, src + {});\n", .{ i, struct_t.id(), offset });
                        offset += struct_t.size();
                    },
                    .arr => |arr| {
                        const elemName = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
                        try w.print("  writeToArray{s}(vm, &res.f{}[0], src + {}, {});", .{ elemName, i, offset, arr.n });
                        offset += field.size();
                    },
                    .sym => |sym| {
                        _ = sym;
                        try w.print("  res.f{} = ", .{i});
                        const val = try std.fmt.bufPrint(&cy.tempBuf, "src[{}]", .{i});
                        _ = val;
                        // try writeToCValue(w, val, field, false);
                        try w.print(";\n", .{});
                        offset += 1;
                    },
                }
            }
            try w.print("  return res;\n", .{});
            try w.print("}}\n", .{});

            // Generate from C struct conv.
            try w.print("void writeFromStruct{}(VM* vm, u8* dst, Struct{} val) {{\n", .{ cstruct.type.id(), cstruct.type.id() });
            var buf: [8]u8 = undefined;
            offset = 0;
            for (cstruct.fields, 0..) |field, i| {
                const from_field = try std.fmt.bufPrint(&buf, "val.f{}", .{i});
                switch (field) {
                    .struct_t => |struct_t| {
                        try w.print("  writeFromStruct{}(vm, dst + {}, {s});\n", .{ struct_t.id(), offset, from_field });
                        offset += struct_t.size();
                    },
                    .arr => |arr| {
                        const elemName = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
                        try w.print("  writeFromArray{s}(vm, dst + {}, &{s}[0], {});\n", .{ elemName, offset, from_field, arr.n });
                        offset += field.size();
                    },
                    .sym => |sym| {
                        // Int and other types.
                        try w.print("  dst[{}] = ", .{offset});
                        try writeToCyValueForSym(w, from_field, sym, false);
                        try w.print(";\n", .{});
                        offset += 1;
                    },
                }
            }
            try w.print("}}\n", .{});
            try w.print("Value newFromStruct{}(VM* vm, Struct{} val) {{\n", .{ cstruct.type.id(), cstruct.type.id() });
            try w.print("  Value obj = icyAllocObject(vm, {});\n", .{cstruct.type.id()});
            try w.print("  Value* dst = (Value*)(obj & ~PointerMask) + 1;\n", .{});
            try w.print("  writeFromStruct{}(vm, dst, val);\n", .{cstruct.type.id()});
            try w.print("  return obj;\n", .{});
            try w.print("}}\n", .{});
        }
    }

    // Generate array conversions.
    pub fn genArrayConvs(self: CGen, vm: *cy.VM, w: anytype) !void {
        var iter = self.ffi.carrayMap.valueIterator();
        while (iter.next()) |elem_type_ptr| {
            const elem_type = elem_type_ptr.*;
            const elemName = try fmtBaseTypeName(&cy.tempBuf, elem_type);

            // Generate writeToArray{elem}. Writes to ptr since arrays can't be returned in C.
            try w.print("void writeToArray{s}(VM* vm, ", .{elemName});
            try writeCType(w, elem_type);
            try w.print("* dst, Value* src, int n) {{\n", .{});
            try w.print("  for (int i = 0; i < n; i += 1) {{\n", .{});
            switch (elem_type) {
                .struct_t => |struct_t| {
                    try w.print("    dst[i] = toStruct{}(vm, src + (i * {}));\n", .{ struct_t.id(), elem_type.wordSize() });
                },
                .arr => |arr| {
                    const child_name = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
                    try w.print("    writeToArray{s}(vm, ({s}*)(dst + i), src + (i * {}), {});\n", .{ child_name, child_name, elem_type.wordSize(), arr.n });
                },
                .sym => |sym| {
                    try w.print("    dst[i] = ", .{});
                    try writeToCValueForSym(w, "src[i]", sym, false);
                    try w.writeAll(";\n");
                },
            }
            try w.print("  }}\n", .{});
            try w.print("}}\n", .{});

            // Generate writeFromArray{elem}.
            try w.print("void writeFromArray{s}(VM* vm, Value* dst, ", .{elemName});
            try writeCType(w, elem_type);
            try w.print("* src, int n) {{\n", .{});
            try w.print("  for (int i = 0; i < n; i += 1) {{\n", .{});
            switch (elem_type) {
                .struct_t => |struct_t| {
                    try w.print("    writeFromStruct{}(vm, dst + (i * {}), *(src + i));\n", .{ struct_t.id(), elem_type.wordSize() });
                },
                .arr => |arr| {
                    const child_name = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
                    try w.print("    writeFromArray{s}(vm, dst + (i * {}), ({s}*)(src + i));\n", .{ child_name, elem_type.wordSize(), child_name });
                },
                .sym => |sym| {
                    try w.print("    dst[i] = ", .{});
                    try writeToCyValueForSym(w, "src[i]", sym, false);
                    try w.writeAll(";\n");
                },
            }
            try w.print("  }}\n", .{});
            try w.print("}}\n", .{});
            try w.print("Value newFromArray{s}(VM* vm, ", .{elemName});
            try writeCType(w, elem_type);
            try w.print("* src, int n) {{\n", .{});

            const cy_type = try toCyType(vm, elem_type);
            try w.print("  Value obj = icyAllocArrayEmpty(vm, {}, n, n * {});\n", .{ cy_type.id(), elem_type.wordSize() });
            try w.print("  Value* dst = (Value*)(obj & ~PointerMask) + 1;\n", .{});
            try w.print("  writeFromArray{s}(vm, dst, src, n);\n", .{elemName});
            try w.print("  return obj;\n", .{});
            try w.print("}}\n", .{});
        }
    }
};

/// Returns the name of a base type. No arrays.
fn fmtBaseTypeName(buf: []u8, ctype: CType) ![]const u8 {
    switch (ctype) {
        .struct_t => |struct_t| {
            return std.fmt.bufPrint(buf, "Struct{}", .{struct_t.id()});
        },
        .arr => return error.Unexpected,
        .sym => |sym| {
            return switch (sym) {
                .bool => "Bool",
                .char => "I8",
                .uchar => "U8",
                .short => "I16",
                .ushort => "U16",
                .int => "I32",
                .uint => "U32",
                .long => "I64",
                .ulong => "U64",
                .usize => "Usize",
                .float => "Float",
                .double => "Double",
                .charPtr => "CharPtr",
                .voidPtr => "VoidPtr",
                else => {
                    std.debug.print("Unsupported arg type: {s}\n", .{@tagName(sym)});
                    return error.InvalidArgument;
                },
            };
        },
    }
}

fn writeCType(w: anytype, type_: *cy.Type) !void {
    switch (type_.id()) {
        bt.Byte => {
            try w.writeAll("u8");
        },
        bt.I8 => {
            try w.writeAll("i8");
        },
        bt.U16 => {
            try w.writeAll("u16");
        },
        bt.I16 => {
            try w.writeAll("i16");
        },
        bt.I32 => {
            try w.writeAll("i32");
        },
        bt.U32 => {
            try w.writeAll("u32");
        },
        bt.Int => {
            try w.writeAll("i64");
        },
        bt.Uint => {
            try w.writeAll("u64");
        },
        bt.Never => {
            try w.writeAll("void");
        },
        else => {
            switch (type_.kind()) {
                .pointer => {
                    const pointer = type_.cast(.pointer);
                    std.debug.assert(!pointer.ref);
                    try w.writeAll("void*");
                },
                .c_variadic => {
                    try w.print("...", .{});
                },
                else => {
                    try w.print("{s}", .{type_.name()});
                },
            }
        },
    }
}

/// e.g. int a; int a[4];
fn writeNamedCType(w: anytype, ctype: CType, name: []const u8) !void {
    if (ctype == .arr) {
        try writeCType(w, ctype.arr.child.*);
        try w.print(" {s}[{}]", .{ name, ctype.arr.n });
    } else {
        try writeCType(w, ctype);
        try w.print(" {s}", .{name});
    }
}

fn writeToCValueForSym(w: anytype, val: []const u8, sym: Symbol, unbox: bool) !void {
    _ = unbox;
    switch (sym) {
        .char => {
            try w.print("(i8)({s} & 0xFFFFFFFFFFFF)", .{val});
        },
        .uchar => {
            try w.print("(u8)({s} & 0xFFFFFFFFFFFF)", .{val});
        },
        .short => {
            try w.print("(i16)({s} & 0xFFFFFFFFFFFF)", .{val});
        },
        .ushort => {
            try w.print("(u16)({s} & 0xFFFFFFFFFFFF)", .{val});
        },
        .usize => {
            try w.print("(size_t)({s} & 0xFFFFFFFFFFFF)", .{val});
        },
        .float => {
            try w.print("(float)*(double*)&{s}", .{val});
        },
        .double => {
            try w.print("*(double*)&{s}", .{val});
        },
        .charPtr => {
            try w.print("(char*){s}", .{val});
        },
        .voidPtr => {
            try w.print("(void*){s}", .{val});
        },
        .funcPtr => {
            try w.print("(void*){s}", .{val});
        },
        else => {
            std.debug.print("Unsupported arg type: {s}\n", .{@tagName(sym)});
            return error.InvalidArgument;
        },
    }
}

fn writeToCyValueForSym(w: anytype, cval: []const u8, ctype: Symbol) !void {
    switch (ctype) {
        .char, .short, .uchar, .ushort, .usize => {
            try w.print("((uint64_t){s})", .{cval});
        },
        .float, .double => {
            // Assumes cval is already converted to double.
            try w.print("*(uint64_t*)&{s}", .{cval});
        },
        .charPtr => {
            try w.print("((uint64_t){s})", .{cval});
        },
        .voidPtr => {
            try w.print("((uint64_t){s})", .{cval});
        },
        .void => {
            try w.print("0", .{});
        },
        .bool => {
            try w.print("{s}", .{cval});
        },
        else => {
            cy.panicFmt("Unsupported arg type: {s}", .{@tagName(ctype)});
        },
    }
}

fn writeCTypeForCSym(w: anytype, ctype: Symbol) !void {
    const str = switch (ctype) {
        .bool => "bool",
        .char => "int8_t",
        .uchar => "uint8_t",
        .short => "int16_t",
        .ushort => "uint16_t",
        .int => "int",
        .uint => "uint32_t",
        .long => "int64_t",
        .ulong => "uint64_t",
        .usize => "size_t",
        .float => "float",
        .double => "double",
        .charPtr => "char*",
        .voidPtr => "void*",
        .funcPtr => "void*",
        .void => "void",
        else => {
            std.debug.print("Unsupported arg type: {s}\n", .{@tagName(ctype)});
            return error.InvalidArgument;
        },
    };
    try w.writeAll(str);
}

fn sizeOf(ctype: Symbol) !usize {
    return switch (ctype) {
        .bool => 1,
        .char => 1,
        .uchar => 1,
        .short => 2,
        .ushort => 2,
        .int => 4,
        .uint => 4,
        .long => 8,
        .ulong => 8,
        .usize => 8,
        .float => 4,
        .double => 8,
        .charPtr => 8,
        .voidPtr => 8,
        .funcPtr => 8,
        else => {
            std.debug.print("Unsupported type: {s}\n", .{@tagName(ctype)});
            return error.InvalidArgument;
        },
    };
}

fn toCyType(vm: *cy.VM, ctype: CType) !*cy.Type {
    switch (ctype) {
        .struct_t => |struct_t| return struct_t,
        .arr => |arr| {
            const child_t = try toCyType(vm, arr.child.*);
            return sema.getFixedArrayType(vm.compiler.api_chunk, child_t, arr.n);
        },
        .sym => |sym| {
            switch (sym) {
                .bool => return vm.sema.bool_t,
                .char => return vm.sema.int_t,
                .uchar => return vm.sema.int_t,
                .short => return vm.sema.int_t,
                .ushort => return vm.sema.int_t,
                .int => return vm.sema.int_t,
                .uint => return vm.sema.int_t,
                .long => return vm.sema.int_t,
                .ulong => return vm.sema.int_t,
                .usize => return vm.sema.int_t,
                .float => return vm.sema.float_t,
                .double => return vm.sema.float_t,
                .funcPtr, .charPtr, .voidPtr => {
                    const data = vm.getData(*cy.builtins.BuiltinsData, "builtins");
                    return data.PtrVoid;
                },
                .void => return vm.sema.void_t,
                else => {
                    std.debug.print("Unsupported val type: {s}\n", .{@tagName(sym)});
                    return error.InvalidArgument;
                },
            }
        },
    }
}

pub const BindLibConfig = struct {};

pub const DynLib = extern struct {};

pub fn ffiBindLib(t: *cy.Thread, config: BindLibConfig, _: *DynLib) !c.Ret {
    _ = config;

    const ffi = t.getPtr(*FFI, 0);

    var success = false;
    var lib = try t.alloc.create(std.DynLib);
    defer {
        if (!success) {
            t.alloc.destroy(lib);
        }
    }

    const path = t.getPtr(*cy.heap.Object, 1);
    if (path.getValue(0).asInt() == 0) {
        if (builtin.os.tag == .macos) {
            const exe = try std.fs.selfExePathAlloc(t.alloc);
            defer t.alloc.free(exe);
            lib.* = try dlopen(exe);
        } else if (builtin.os.tag == .windows) {
            // On Windows, load the C runtime library
            if (dlopen("ucrtbase.dll")) |ucrt_lib| {
                lib.* = ucrt_lib;
            } else |_| {
                lib.* = try dlopen("msvcrt.dll");
            }
        } else {
            lib.* = try dlopen("");
        }
    } else {
        const path_s = path.getValue(1).asString();
        log.tracev("bindLib {s}", .{path_s});
        lib.* = dlopen(path_s) catch |err| {
            if (err == error.FileNotFound) {
                return t.ret_panic("FileNotFound");
            } else {
                return err;
            }
        };
    }

    // Generate c code.
    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(t.alloc);
    const w = csrc.writer(t.alloc);

    const cgen = CGen{};
    _ = cgen;

    // try cgen.genHeaders(w);

    // try cgen.enStructDecls(w);
    // try cgen.genArrayConvDecls(w);

    // try cgen.genStructConvs(w);
    // try cgen.genArrayConvs(vm, w);

    // Begin func binding generation.

    const tempTypes: cy.BoundedArray(*cy.Type, 16) = .{};
    _ = tempTypes;
    for (ffi.cfuncs.items) |*cfunc| {
        // Check that symbol exists.
        const ptr = lib.lookup(*anyopaque, cfunc.namez) orelse {
            std.debug.panic("Missing symbol `{s}`.", .{cfunc.namez});
        };
        _ = ptr;

        // // Build function signature.
        // tempTypes.len = 0;
        // for (cfunc.params) |param| {
        //     const param_t = try toCyType(vm, param);
        //     try tempTypes.append(param_t);
        // }

        // const retType = try toCyType(vm, cfunc.ret);
        // const sig = try vm.sema.ensureFuncSig(tempTypes.slice(), retType);

        // cfunc.ptr = ptr;
        // cfunc.sig = sig;
        // try cgen.genFunc(vm, w, cfunc.*, config);
    }

    try w.writeByte(0);
    if (cy.cgen.DumpCGen) {
        std.debug.print("{s}\n", .{csrc.items});
    }

    const state = tcc.tcc_new();
    // Don't include libtcc1.a.
    _ = tcc.tcc_set_options(state, "-nostdlib");
    _ = tcc.tcc_set_output_type(state, tcc.TCC_OUTPUT_MEMORY);

    if (tcc.tcc_compile_string(state, csrc.items.ptr) == -1) {
        cy.panic("Failed to compile c source.");
    }

    // const __floatundisf = @extern(*anyopaque, .{ .name = "__floatundisf", .linkage = .Strong });
    if (builtin.cpu.arch != .aarch64) {
        _ = tcc.tcc_add_symbol(state, "__fixunsdfdi", __fixunsdfdi);
        _ = tcc.tcc_add_symbol(state, "__floatundidf", __floatundidf);
    }
    // _ = tcc.tcc_add_symbol(state, "__floatundisf", __floatundisf);
    _ = tcc.tcc_add_symbol(state, "printf", std.c.printf);
    // _ = tcc.tcc_add_symbol(state, "exit", std.c.exit);
    // _ = tcc.tcc_add_symbol(state, "breakpoint", breakpoint);
    _ = tcc.tcc_add_symbol(state, "_cyRelease", cyRelease);
    _ = tcc.tcc_add_symbol(state, "icyAllocObject", cAllocObject);
    _ = tcc.tcc_add_symbol(state, "icyAllocVectorEmpty", cAllocVectorEmpty);
    // _ = tcc.tcc_add_symbol(state, "printValue", cPrintValue);

    // Add binded symbols.
    for (ffi.cfuncs.items) |cfunc| {
        _ = tcc.tcc_add_symbol(state, cfunc.namez.ptr, cfunc.ptr);
    }

    if (tcc.tcc_relocate(state, tcc.TCC_RELOCATE_AUTO) < 0) {
        cy.panic("Failed to relocate compiled code.");
    }

    success = true;
    // return t.heap.allocTccState(state.?, lib);
    return c.RetOk;
}

pub const CFuncData = struct {
    namez: [:0]const u8,
    ptr: *anyopaque,
    sig: *cy.FuncSig,
};

fn cyRelease(t: *cy.Thread, val: Value) callconv(.c) void {
    t.heap.release(val);
}

fn cAllocObject(t: *cy.Thread, id: cy.TypeId) callconv(.c) Value {
    const size = t.c.vm.sema.getType(id).cast(.struct_t).size;
    const obj = t.heap.new_object_undef(id, size) catch cy.fatal();
    return Value.initPtr(obj);
}

/// TODO: Once objects become unboxed, this won't require fetching the array type.
fn cAllocVectorEmpty(t: *cy.Thread, elem_tid: cy.TypeId, n: usize, size: usize) callconv(.c) Value {
    const elem_t = t.c.vm.sema.getType(elem_tid);
    const arr_t = sema.getVectorType(t.c.vm.compiler.api_chunk, elem_t, @intCast(n)) catch cy.fatal();
    const obj = t.heap.new_object_undef(arr_t.id(), size) catch cy.fatal();
    return Value.initPtr(obj);
}

fn cPrintValue(val: u64) void {
    const v: Value = @bitCast(val);
    v.dump();
}

fn breakpoint() callconv(.c) void {
    @breakpoint();
}

pub fn ffiBindCallback(vm: *cy.VM) anyerror!Value {
    if (!cy.hasFFI) return vm.prepPanic("Unsupported.");

    const ffi = vm.getPtr(*FFI, 0);
    _ = ffi;
    const func = vm.getValue(1);
    const params = vm.getPtr(*cy.heap.List, 2).items();
    _ = params;
    // const ret = try ffi.toCType(vm, vm.getValue(3));

    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(vm.alloc);
    const w = csrc.writer(vm.alloc);
    _ = w;

    const cgen = CGen{};
    _ = cgen;
    // try cgen.genHeaders(w);
    // try cgen.genStructDecls(w);
    // try cgen.genArrayConvDecls(w);
    // try cgen.genStructConvs(w);
    // try cgen.genArrayConvs(vm, w);

    // Emit C func declaration.

    const state = tcc.tcc_new();
    // Don't include libtcc1.a.
    _ = tcc.tcc_set_options(state, "-nostdlib");
    _ = tcc.tcc_set_output_type(state, tcc.TCC_OUTPUT_MEMORY);

    if (tcc.tcc_compile_string(state, csrc.items.ptr) == -1) {
        cy.panic("Failed to compile c source.");
    }

    // const __floatundisf = @extern(*anyopaque, .{ .name = "__floatundisf", .linkage = .Strong });
    if (builtin.cpu.arch != .aarch64) {
        _ = tcc.tcc_add_symbol(state, "__fixunsdfdi", __fixunsdfdi);
        _ = tcc.tcc_add_symbol(state, "__floatundidf", __floatundidf);
    }
    // _ = tcc.tcc_add_symbol(state, "__floatundisf", __floatundisf);
    _ = tcc.tcc_add_symbol(state, "printf", std.c.printf);
    // _ = tcc.tcc_add_symbol(state, "exit", std.c.exit);
    // _ = tcc.tcc_add_symbol(state, "breakpoint", breakpoint);
    _ = tcc.tcc_add_symbol(state, "icyAllocObject", cAllocObject);
    _ = tcc.tcc_add_symbol(state, "icyAllocVectorEmpty", cAllocVectorEmpty);
    _ = tcc.tcc_add_symbol(state, "_cyRelease", cyRelease);
    // _ = tcc.tcc_add_symbol(state, "printValue", cPrintValue);

    if (tcc.tcc_relocate(state, tcc.TCC_RELOCATE_AUTO) < 0) {
        cy.panic("Failed to relocate compiled code.");
    }

    const tccState = try vm.heap.allocTccState(state.?, null);
    _ = tccState;

    const funcPtr = tcc.tcc_get_symbol(state, "cyExternFunc") orelse {
        cy.panic("Failed to get symbol.");
    };
    _ = funcPtr;

    // Extern func consumes tcc state and retains func.
    vm.heap.retain(func);

    // // Create extern func.
    // const res = try vm.heap.allocExternFunc(func, funcPtr, tccState);

    // // ffi manages the extern func. (So it does not get freed).
    // vm.heap.retain(res);
    // try ffi.managed.append(vm.alloc, res);

    // return res;
    return undefined;
}
