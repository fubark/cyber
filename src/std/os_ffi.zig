const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const tcc = @import("tcc");
const cy = @import("../cyber.zig");
const c = @import("../capi.zig");
const cli = @import("../cli.zig");
const rt = cy.rt;
const Value = cy.Value;
const builtins = @import("../builtins/builtins.zig");
const bindings = @import("../builtins/bindings.zig");
const prepareThrowSymbol = bindings.prepareThrowSymbol;
const Symbol = bindings.Symbol;
const os = @import("os.zig");
const sema = cy.sema;
const bt = cy.types.BuiltinTypes;
const types = cy.types;

const log = cy.log.scoped(.ffi);

const DumpCGen = builtin.mode == .Debug and false;

const CType = union(enum) {
    sym: Symbol,
    struct_t: *cy.Type,
    arr: struct {
        child: *CType,
        n: u32,
    },

    fn size(self: CType) usize {
        switch (self) {
            .sym => return 1,
            .struct_t => |struct_t| return struct_t.size(),
            .arr => |arr| return arr.child.size() * arr.n,
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

    CArrayT: *cy.Type,

    fn toCType(self: *FFI, vm: *cy.VM, spec: Value) !CType {
        if (spec.getTypeId() == self.CArrayT.id()) {
            const nField = try vm.ensureField("n");
            const n: u32 = @intCast((try vm.getField(spec, nField)).asBoxInt());
            const elemField = try vm.ensureField("elem");
            const elem = try vm.getField(spec, elemField);
            const child = try vm.alloc.create(CType);
            child.* = try self.toElemCType(vm, elem);
            const res = CType{ .arr = .{ .child = child, .n = n }};
            try self.ensureArray(vm.alloc, res);
            return res;
        } else {
            return self.toElemCType(vm, spec);
        }
    }

    fn toElemCType(self: *FFI, vm: *cy.VM, spec: Value) !CType {
        if (spec.isObjectType(bt.Type)) {
            const type_ = vm.getType(spec.asHeapObject().type.type);
            if (self.typeToCStruct.contains(type_)) {
                return CType{ .struct_t = type_ };
            } else {
                const name = try vm.getOrBufPrintValueStr(&cy.tempBuf, spec);
                log.tracev("CStruct not declared for: {s}", .{name});
                return error.InvalidArgument;
            }
        } else if (spec.isSymbol()) {
            const sym = try std.meta.intToEnum(Symbol, spec.asSymbolId());
            return CType{ .sym = sym };
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

pub fn allocFFI(vm: *cy.VM) !Value {
    const cli_data = vm.getData(*cli.CliData, "cli");
    const ffi: *FFI = @ptrCast(try cy.heap.allocHostObject(vm, cli_data.FFIT.id(), @sizeOf(FFI)));
    ffi.* = .{
        .cstructs = .{},
        .typeToCStruct = .{},
        .carrayMap = .{},
        .cfuncs = .{},
        .managed = .{},
        .CArrayT = cli_data.CArrayT,
    };
    return Value.initHostPtr(ffi);
}

pub fn ffiGetChildren(_: ?*c.VM, obj: ?*anyopaque) callconv(.C) c.ValueSlice {
    const ffi: *FFI = @ptrCast(@alignCast(obj));
    return Value.toSliceC(ffi.managed.items);
}

pub fn ffiFinalizer(vm_: ?*c.VM, obj: ?*anyopaque) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));

    const alloc = vm.alloc;
    var ffi: *FFI = @ptrCast(@alignCast(obj));

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
        for (func.params) |param| {
            param.deinit(alloc);
        }
        alloc.free(func.params);
    }
    ffi.cfuncs.deinit(alloc);

    ffi.managed.deinit(alloc);
}

const CGen = struct {
    ffi: *FFI,

    pub fn genHeaders(_: CGen, w: anytype) !void {
        try w.print(
            \\#define bool _Bool
            \\#define int64_t long long
            \\#define uint64_t unsigned long long
            \\#define Value uint64_t
            // TODO: Check for 32bit addressing.
            \\#define size_t uint64_t
            \\#define int8_t signed char
            \\#define uint8_t unsigned char
            \\#define int16_t short
            \\#define uint16_t unsigned short
            \\#define int32_t int
            \\#define uint32_t unsigned int
            \\#define PointerMask 0xFFFE000000000000
            \\typedef struct ZAllocator {{
            \\    void* ptr;
            \\    void* vtable;
            \\}} ZAllocator;
            \\typedef struct VMC {{
            \\    uint8_t* pc;
            \\    uint64_t* fp;
            \\}} VMC;
            \\typedef struct VM {{
            \\    ZAllocator alloc;
            \\    VMC c;
            \\}} VM;
            \\extern void _cyRelease(VM*, uint64_t);
            \\extern void* icyGetPtr(VM*, uint64_t);
            \\extern void* _cyGetFuncPtr(VM*, uint64_t);
            \\extern uint64_t icyAllocObject(VM*, uint32_t);
            \\extern uint64_t icyAllocArrayEmpty(VM*, uint32_t, size_t, size_t);
            \\extern uint64_t cy_alloc_int(VM*, int64_t);
            \\extern int64_t cy_as_boxint(uint64_t);
            \\extern uint64_t icyAllocCyPointer(VM*, void*);
            \\extern uint64_t _cyCallFunc(VM*, uint64_t, uint64_t*, uint8_t);
            \\extern int printf(char* fmt, ...);
            \\#define CALL_ARG_START 5
            \\uint64_t cy_get_value(VM* vm, size_t idx) {{
            \\    return vm->c.fp[CALL_ARG_START + idx];
            \\}}
            // \\extern void exit(int code);
            \\
        , .{});
    }

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
            try w.print("Struct{} toStruct{}(VM* vm, Value* src) {{\n", .{cstruct.type.id(), cstruct.type.id()});
            // Get pointer to first cyber object field.
            try w.print("  Struct{} res;\n", .{cstruct.type.id()});
            var offset: usize = 0;
            for (cstruct.fields, 0..) |field, i| {
                switch (field) {
                    .struct_t => |struct_t| {
                        try w.print("  res.f{} = toStruct{}(vm, src + {});\n", .{i, struct_t.id(), offset});
                        offset += struct_t.size();
                    },
                    .arr => |arr| {
                        const elemName = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
                        try w.print("  writeToArray{s}(vm, &res.f{}[0], src + {}, {});", .{elemName, i, offset, arr.n});
                        offset += field.size();
                    },
                    .sym => |sym| {
                        _ = sym;
                        try w.print("  res.f{} = ", .{i});
                        const val = try std.fmt.bufPrint(&cy.tempBuf, "src[{}]", .{i});
                        try writeToCValue(w, val, field, false);
                        try w.print(";\n", .{});
                        offset += 1;
                    }
                }
            }
            try w.print("  return res;\n", .{});
            try w.print("}}\n", .{});

            // Generate from C struct conv.
            try w.print("void writeFromStruct{}(VM* vm, Value* dst, Struct{} val) {{\n", .{cstruct.type.id(), cstruct.type.id()});
            var buf: [8]u8 = undefined;
            offset = 0;
            for (cstruct.fields, 0..) |field, i| {
                const from_field = try std.fmt.bufPrint(&buf, "val.f{}", .{i});
                switch (field) {
                    .struct_t => |struct_t| {
                        try w.print("  writeFromStruct{}(vm, dst + {}, {s});\n", .{struct_t.id(), offset, from_field});
                        offset += struct_t.size();
                    },
                    .arr => |arr| {
                        const elemName = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
                        try w.print("  writeFromArray{s}(vm, dst + {}, &{s}[0], {});\n", .{elemName, offset, from_field, arr.n});
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
            try w.print("Value newFromStruct{}(VM* vm, Struct{} val) {{\n", .{cstruct.type.id(), cstruct.type.id()});
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
                    try w.print("    dst[i] = toStruct{}(vm, src + (i * {}));\n", .{struct_t.id(), elem_type.size()});
                },
                .arr => |arr| {
                    const child_name = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
                    try w.print("    writeToArray{s}(vm, ({s}*)(dst + i), src + (i * {}), {});\n", .{child_name, child_name, elem_type.size(), arr.n});
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
                    try w.print("    writeFromStruct{}(vm, dst + (i * {}), *(src + i));\n", .{struct_t.id(), elem_type.size()});
                },
                .arr => |arr| {
                    const child_name = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
                    try w.print("    writeFromArray{s}(vm, dst + (i * {}), ({s}*)(src + i));\n", .{child_name, elem_type.size(), child_name});
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
            try w.print("  Value obj = icyAllocArrayEmpty(vm, {}, n, n * {});\n", .{cy_type.id(), elem_type.size()});
            try w.print("  Value* dst = (Value*)(obj & ~PointerMask) + 1;\n", .{});
            try w.print("  writeFromArray{s}(vm, dst, src, n);\n", .{elemName});
            try w.print("  return obj;\n", .{});
            try w.print("}}\n", .{});
        }
    }

    fn genFunc(self: *CGen, vm: *cy.VM, w: anytype, funcInfo: CFuncData, config: BindLibConfig) !void {
        _ = config;
    
        _ = vm;
        var buf: [32]u8 = undefined;
        _ = self;
        const params = funcInfo.params;
        const sym = funcInfo.namez;
        const ret = funcInfo.ret;

        // Emit extern declaration.
        try w.writeAll("extern ");
        try writeCType(w, funcInfo.ret);
        try w.print(" {s}(", .{ sym });
        if (params.len > 0) {
            try writeCType(w, params[0]);
            if (params.len > 1) {
                for (params[1..]) |param| {
                    try w.print(", ", .{});
                    try writeCType(w, param);
                }
            }
        }
        try w.print(");\n", .{});

        try w.print("uint64_t cy{s}(VM* vm) {{\n", .{sym});

        // Temps.
        if (params.len > 0) {
            for (params, 0..) |param, i| {
                switch (param) {
                    .struct_t => |struct_t| {
                        try w.print("  ", .{});
                        const arg_name = try std.fmt.bufPrint(&buf, "arg_{}", .{i});
                        try writeNamedCType(w, param, arg_name);
                        try w.print(";\n", .{});
                        try w.print("  Value* src_{} = (Value*)(cy_get_value(vm, {}) & ~PointerMask) + 1;\n", .{i, i});
                        try w.print("  arg_{} = toStruct{}(vm, src_{});\n", .{i, struct_t.id(), i});
                    },
                    .arr => |arr| {
                        const elem = arr.child.*;
                        const elemName = try fmtBaseTypeName(&cy.tempBuf, elem);

                        try w.print("  ", .{});
                        const arg_name = try std.fmt.bufPrint(&buf, "arg_{}", .{i});
                        try writeNamedCType(w, param, arg_name);
                        try w.print(";\n", .{});

                        try w.print("  Value* src_{} = (Value*)(cy_get_value(vm, {}) & ~PointerMask) + 1;\n", .{i, i});
                        try w.print("  writeToArray{s}(vm, &arg_{}[0], src_{}, {});\n", .{elemName, i, i, arr.n});
                    },
                    .sym => {
                        try w.print("  Value param_{} = cy_get_value(vm, {});\n", .{i, i});
                        try w.print("  ", .{});
                        try writeCType(w, param);
                        try w.print(" arg_{} = ", .{i});
                        const param_val = try std.fmt.bufPrint(&buf, "param_{}", .{i});
                        try writeToCValue(w, param_val, param, false);
                        try w.print(";\n", .{});
                    },
                }
            }
        }

        // Gen call.
        if (ret == .struct_t) {
            try w.print("  Struct{} res = {s}(", .{ret.struct_t.id(), sym});
        } else {
            switch (ret.sym) {
                .char => {
                    try w.print("  int8_t res = {s}(", .{sym});
                },
                .uchar => {
                    try w.print("  uint8_t res = {s}(", .{sym});
                },
                .short => {
                    try w.print("  int16_t res = {s}(", .{sym});
                },
                .ushort => {
                    try w.print("  uint16_t res = {s}(", .{sym});
                },
                .int => {
                    try w.print("  int32_t res = {s}(", .{sym});
                },
                .uint => {
                    try w.print("  uint32_t res = {s}(", .{sym});
                },
                .long => {
                    try w.print("  int64_t res = {s}(", .{sym});
                },
                .ulong => {
                    try w.print("  uint64_t res = {s}(", .{sym});
                },
                .usize => {
                    try w.print("  size_t res = {s}(", .{sym});
                },
                .float => {
                    try w.print("  double res = (double){s}(", .{sym});
                },
                .double => {
                    try w.print("  double res = {s}(", .{sym});
                },
                .charPtr => {
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
                else => cy.panicFmt("Unsupported return type: {s}", .{ @tagName(ret.sym) }),
            }
        }

        // Gen args.
        if (params.len > 0) {
            try w.print("arg_0", .{});
            for (params[1..], 1..) |param, i| {
                _ = param;
                try w.print(", arg_{}", .{i});
            }
        }

        // End of args.
        try w.print(");\n", .{});

        // Gen return.
        try w.print("  return ", .{});
        try writeToCyValue(w, "res", ret, false);
        try w.print(";\n", .{});

        try w.print("}}\n", .{});
    }
};

pub fn ffiCbind(vm: *cy.VM) anyerror!Value {
    const alloc = vm.alloc;
    const ffi = vm.getHostObject(*FFI, 0);
    const mt = vm.getValue(1).asHeapObject();
    const type_ = vm.getType(mt.type.type);
    const fieldsList = &vm.getValue(2).asHeapObject().list;

    if (ffi.typeToCStruct.contains(type_)) {
        log.tracev("Object type already declared.", .{});
        return error.InvalidArgument;
    }
    const id = ffi.cstructs.items.len;
    try ffi.typeToCStruct.put(alloc, type_, @intCast(id));

    var fieldsBuilder: std.ArrayListUnmanaged(CType) = .{};
    for (fieldsList.items()) |field| {
        try fieldsBuilder.append(alloc, try ffi.toCType(vm, field));
    }

    const fields = try fieldsBuilder.toOwnedSlice(alloc);
    try ffi.cstructs.append(alloc, .{ .type = type_, .fields = fields });

    return Value.Void;
}

pub fn ffiCfunc(vm: *cy.VM) anyerror!Value {
    const ffi = vm.getHostObject(*FFI, 0);

    const funcArgs = vm.getObject(*cy.heap.List, 2).items();

    const ctypes = try vm.alloc.alloc(CType, funcArgs.len);
    if (funcArgs.len > 0) {
        for (funcArgs, 0..) |arg, i| {
            ctypes[i] = try ffi.toCType(vm, arg);
        }
    }

    const retType = try ffi.toCType(vm, vm.getValue(3));

    const symName = try vm.getOrBufPrintValueStr(&cy.tempBuf, vm.getValue(1));
    try ffi.cfuncs.append(vm.alloc, .{
        .namez = try vm.alloc.dupeZ(u8, symName),
        .params = ctypes,
        .ret = retType,
        .ptr = undefined,
        .funcSigId = undefined,
        .skip = false,
    });

    return Value.Void;
}

pub fn ffiNew(vm: *cy.VM) anyerror!Value {
    // const ffi = args[0].castHostObject(*FFI);

    const csym = try std.meta.intToEnum(Symbol, vm.getSymbol(1));
    const size = try sizeOf(csym);

    const ptr = std.c.malloc(size);
    return Value.initRaw(@intFromPtr(ptr));
}

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
                    std.debug.print("Unsupported arg type: {s}\n", .{ @tagName(sym) });
                    return error.InvalidArgument;
                },
            };
        },
    }
}

pub const BindLibConfig = struct {
};

fn writeCType(w: anytype, ctype: CType) !void {
    switch (ctype) {
        .struct_t => |struct_t| {
            try w.print("Struct{}", .{struct_t.id()});
        },
        .arr => |arr| {
            try writeCType(w, arr.child.*);
            try w.print("[{}]", .{ arr.n });
        },
        .sym => |sym| {
            try writeCTypeForCSym(w, sym);
        }
    }
}

/// eg. int a; int a[4];
fn writeNamedCType(w: anytype, ctype: CType, name: []const u8) !void {
    if (ctype == .arr) {
        try writeCType(w, ctype.arr.child.*);
        try w.print(" {s}[{}]", .{ name, ctype.arr.n });
    } else {
        try writeCType(w, ctype);
        try w.print(" {s}", .{name});
    }
}

fn writeToCValue(w: anytype, val: []const u8, ctype: CType, unbox: bool) !void {
    switch (ctype) {
        .struct_t => |struct_t| {
            try w.print("toStruct{}(vm, {s})", .{struct_t.id(), val});
        },
        .sym => |sym| {
            try writeToCValueForSym(w, val, sym, unbox);
        },
        else => {
            std.debug.print("Unsupported arg type: {s}\n", .{ @tagName(ctype) });
            return error.InvalidArgument;
        }
    }
}

fn writeToCValueForSym(w: anytype, val: []const u8, sym: Symbol, unbox: bool) !void {
    switch (sym) {
        .bool => {
            if (unbox) {
                try w.print("({s} == 0x7FFC000200000001)?1:0", .{val});
            } else {
                try w.print("(bool)({s})", .{val});
            }
        },
        .char => {
            try w.print("(int8_t)({s} & 0xFFFFFFFFFFFF)", .{val});
        },
        .uchar => {
            try w.print("(uint8_t)({s} & 0xFFFFFFFFFFFF)", .{val});
        },
        .short => {
            try w.print("(int16_t)({s} & 0xFFFFFFFFFFFF)", .{val});
        },
        .ushort => {
            try w.print("(uint16_t)({s} & 0xFFFFFFFFFFFF)", .{val});
        },
        .int => {
            if (unbox) {
                try w.print("(int)cy_as_boxint({s})", .{val});
            } else {
                try w.print("(int){s}", .{val});
            }
        },
        .uint => {
            try w.print("(uint32_t)({s} & 0xFFFFFFFFFFFF)", .{val});
        },
        .long => {
            // 48-bit int to 64-bit.
            try w.print("(((int64_t)({s} & 0xFFFFFFFFFFFF) << 16) >> 16)", .{val});
        },
        .ulong => {
            try w.print("(uint64_t)({s} & 0xFFFFFFFFFFFF)", .{val});
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
            if (unbox) {
                try w.print("(char*)icyGetPtr(vm, {s})", .{val});
            } else {
                try w.print("(char*){s}", .{val});
            }
        },
        .voidPtr => {
            if (unbox) {
                try w.print("icyGetPtr(vm, {s})", .{val});
            } else {
                try w.print("(void*){s}", .{val});
            }
        },
        .funcPtr => {
            if (unbox) {
                try w.print("_cyGetFuncPtr(vm, {s})", .{val});
            } else {
                try w.print("(void*){s}", .{val});
            }
        },
        else => {
            std.debug.print("Unsupported arg type: {s}\n", .{ @tagName(sym) });
            return error.InvalidArgument;
        }
    }
}

fn writeToCyValue(w: anytype, cval: []const u8, ctype: CType, must_box: bool) !void {
    switch (ctype) {
        .struct_t => |struct_t| {
            try w.print("newFromStruct{}(vm, {s})", .{ struct_t.id(), cval });
        },
        .arr => |arr| {
            const elemName = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
            try w.print("newFromArray{s}(vm, {s}, {})", .{elemName, cval, arr.n});
        },
        .sym => |sym| {
            try writeToCyValueForSym(w, cval, sym, must_box);
        },
    }
}

fn writeToCyValueForSym(w: anytype, cval: []const u8, ctype: Symbol, must_box: bool) !void {
    switch (ctype) {
        .char,
        .short,
        .int,
        .ulong,
        .uchar,
        .ushort,
        .uint,
        .usize => {
            if (must_box) {
                try w.print("cy_alloc_int(vm, ((int64_t){s}))", .{cval});
            } else {
                try w.print("((uint64_t){s})", .{cval});
            }
        },
        .long => {
            if (must_box) {
                try w.print("cy_alloc_int(vm, {s})", .{cval});
            } else {
                try w.print("{s}", .{cval});
            }
        },
        .float,
        .double => {
            // Assumes cval is already converted to double.
            try w.print("*(uint64_t*)&{s}", .{cval});
        },
        .charPtr => {
            if (must_box) {
                try w.print("icyAllocCyPointer(vm, {s})", .{cval});
            } else {
                try w.print("((uint64_t){s})", .{cval});
            }
        },
        .voidPtr => {
            if (must_box) {
                try w.print("icyAllocCyPointer(vm, {s})", .{cval});
            } else {
                try w.print("((uint64_t){s})", .{cval});
            }
        },
        .void => {
            try w.print("0x7FFC000100000000", .{});
        },
        .bool => {
            if (must_box) {
                try w.print("({s} == 1) ? 0x7FFC000200000001 : 0x7FFC000200000000", .{cval});
            } else {
                try w.print("{s}", .{cval});
            }
        },
        else => {
            cy.panicFmt("Unsupported arg type: {s}", .{ @tagName(ctype) });
        }
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
            std.debug.print("Unsupported arg type: {s}\n", .{ @tagName(ctype) });
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
            std.debug.print("Unsupported type: {s}\n", .{ @tagName(ctype) });
            return error.InvalidArgument;
        }
    };
}

fn toCyType(vm: *cy.VM, ctype: CType) !*cy.Type {
    switch (ctype) {
        .struct_t => |struct_t| return struct_t,
        .arr => |arr| {
            const child_t = try toCyType(vm, arr.child.*);
            return sema.getArrayType(vm.compiler.api_chunk, arr.n, child_t);
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
                .funcPtr,
                .charPtr,
                .voidPtr => {
                    const data = vm.getData(*cy.builtins.BuiltinsData, "builtins");
                    return data.PtrVoid;
                },
                .void => return vm.sema.void_t,
                else => {
                    std.debug.print("Unsupported val type: {s}\n", .{ @tagName(sym) });
                    return error.InvalidArgument;
                }
            }
        }
    }
}

pub fn ffiBindLib(vm: *cy.VM, config: BindLibConfig) !Value {
    const ffi = vm.getHostObject(*FFI, 0);

    const bt_data = vm.getData(*cy.builtins.BuiltinsData, "builtins");

    var success = false;

    var lib = try vm.alloc.create(std.DynLib);
    defer {
        if (!success) {
            vm.alloc.destroy(lib);
        }
    }

    const path = vm.getObject(*cy.heap.Object, 1);
    if (path.getValue(0).asInt() == 0) {
        if (builtin.os.tag == .macos) {
            const exe = try std.fs.selfExePathAlloc(vm.alloc);
            defer vm.alloc.free(exe);
            lib.* = try dlopen(exe);
        } else {
            lib.* = try dlopen("");
        }
    } else {
        const path_s = path.getValue(1).asString();
        log.tracev("bindLib {s}", .{path_s});
        lib.* = dlopen(path_s) catch |err| {
            if (err == error.FileNotFound) {
                return rt.prepThrowError(vm, .FileNotFound);
            } else {
                return err;
            }
        };
    }

    // Generate c code.
    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(vm.alloc);
    const w = csrc.writer(vm.alloc);

    var cgen = CGen{ .ffi = ffi };

    try cgen.genHeaders(w);

    try cgen.genStructDecls(w);
    try cgen.genArrayConvDecls(w);

    try cgen.genStructConvs(w);
    try cgen.genArrayConvs(vm, w);

    // Begin func binding generation.

    var tempTypes: std.BoundedArray(*cy.Type, 16) = .{};
    for (ffi.cfuncs.items) |*cfunc| {
        // Check that symbol exists.
        cfunc.skip = false;
        const ptr = lib.lookup(*anyopaque, cfunc.namez) orelse {
            log.tracev("Missing sym: '{s}'", .{cfunc.namez});
            // Don't generate function if it is missing from the lib.
            cfunc.skip = true;
            continue;
        };

        // Build function signature.
        tempTypes.len = 0;
        for (cfunc.params) |param| {
            const param_t = try toCyType(vm, param);
            try tempTypes.append(param_t);
        }

        const retType = try toCyType(vm, cfunc.ret);
        const funcSigId = try vm.sema.ensureFuncSig(tempTypes.slice(), retType);

        cfunc.ptr = ptr;
        cfunc.funcSigId = funcSigId;
        try cgen.genFunc(vm, w, cfunc.*, config);
    }

    for (ffi.cstructs.items) |cstruct| {
        // Generate ptrTo[Object].
        try w.print("uint64_t cyPtrTo{s}(VM* vm) {{\n", .{cstruct.type.name()});
        try w.print("  Value ptr = cy_get_value(vm, 0);\n", .{});
        try w.print("  Value res = newFromStruct{}(vm, *(Struct{}*)ptr);\n", .{cstruct.type.id(), cstruct.type.id()});
        try w.print("  return res;\n", .{});
        try w.print("}}\n", .{});
    }

    try w.writeByte(0);
    if (DumpCGen) {
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
    _ = tcc.tcc_add_symbol(state, "icyGetPtr", cGetPtr);
    _ = tcc.tcc_add_symbol(state, "cy_as_boxint", cAsBoxInt);
    _ = tcc.tcc_add_symbol(state, "_cyGetFuncPtr", cGetFuncPtr);
    _ = tcc.tcc_add_symbol(state, "icyAllocCyPointer", cAllocCyPointer);
    _ = tcc.tcc_add_symbol(state, "icyAllocObject", cAllocObject);
    _ = tcc.tcc_add_symbol(state, "icyAllocArrayEmpty", cAllocArrayEmpty);
    _ = tcc.tcc_add_symbol(state, "cy_alloc_int", cAllocInt);
    // _ = tcc.tcc_add_symbol(state, "printValue", cPrintValue);
    if (builtin.cpu.arch == .aarch64) {
        _ = tcc.tcc_add_symbol(state, "memmove", memmove);
    }

    // Add binded symbols.
    for (ffi.cfuncs.items) |cfunc| {
        if (cfunc.skip) continue;
        _ = tcc.tcc_add_symbol(state, cfunc.namez.ptr, cfunc.ptr);
    }

    if (tcc.tcc_relocate(state, tcc.TCC_RELOCATE_AUTO) < 0) {
        cy.panic("Failed to relocate compiled code.");
    }

    // Create map with binded C-functions as functions.
    const map = try vm.allocEmptyMap();

    const cyState = try cy.heap.allocTccState(vm, state.?, lib);

    var numGenFuncs: u32 = 0;
    for (ffi.cfuncs.items) |cfunc| {
        if (cfunc.skip) continue;
        const symGen = try std.fmt.allocPrint(vm.alloc, "cy{s}\x00", .{cfunc.namez});
        defer vm.alloc.free(symGen);
        const funcPtr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
            cy.panic("Failed to get symbol.");
        };

        const symKey = try vm.retainOrAllocAstring(cfunc.namez);
        const func = cy.ptrAlignCast(cy.ZHostFuncFn, funcPtr);
        const funcSig = vm.compiler.sema.getFuncSig(cfunc.funcSigId);

        const ptr_t = try cy.vm.getFuncPtrType(vm, cfunc.funcSigId);
        const funcVal = try cy.heap.allocHostFuncPtr(vm, ptr_t.id(), func, @intCast(cfunc.params.len),
            cfunc.funcSigId, cyState, funcSig.info.reqCallTypeCheck);
        try map.asHeapObject().map.setConsume(vm, symKey, funcVal);
        numGenFuncs += 1;
    }
    cy.arc.retainInc(vm, cyState, @intCast(numGenFuncs + ffi.cstructs.items.len - 1));
    for (ffi.cstructs.items) |cstruct| {
        const typeName = cstruct.type.name();
        const symGen = try std.fmt.allocPrint(vm.alloc, "cyPtrTo{s}{u}", .{typeName, 0});
        defer vm.alloc.free(symGen);
        const cfunc_ptr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
            cy.panic("Failed to get symbol.");
        };

        const symKey = try vm.allocAstringConcat("ptrTo", typeName);
        const func_ptr = cy.ptrAlignCast(cy.ZHostFuncFn, cfunc_ptr);

        const sig_id = try vm.sema.ensureFuncSigRt(&.{ bt_data.PtrVoid }, cstruct.type);
        const ptr_t = try cy.vm.getFuncPtrType(vm, sig_id);
        const funcVal = try cy.heap.allocHostFuncPtr(vm, ptr_t.id(), func_ptr, 1, sig_id, cyState, true);
        try map.asHeapObject().map.setConsume(vm, symKey, funcVal);
    }
    success = true;
    return map;
}

const CFuncData = struct {
    namez: [:0]const u8,
    params: []const CType,
    ret: CType,
    ptr: *anyopaque,
    funcSigId: cy.sema.FuncSigId,
    skip: bool,
};

fn dlopen(path: []const u8) !std.DynLib {
    if (builtin.os.tag == .linux and builtin.link_libc) {
        const path_c = try std.posix.toPosixPath(path);
        // Place the lookup scope of the symbols in this library ahead of the global scope.
        const RTLD_DEEPBIND = 0x00008;
        return std.DynLib{
            .inner = .{
                .handle = std.c.dlopen(&path_c, std.c.RTLD.LAZY | RTLD_DEEPBIND) orelse {
                    return error.FileNotFound;
                },
            },
        };
    } else {
        return std.DynLib.open(path);
    }
}

extern fn __floatundidf(u64) f64;
extern fn __fixunsdfdi(f64) u64;
extern fn memmove(dst: *anyopaque, src: *anyopaque, num: usize) *anyopaque;

fn cyRelease(vm: *cy.VM, val: Value) callconv(.C) void {
    vm.release(val);
}

fn cAsBoxInt(val: Value) callconv(.C) i64 {
    return val.asBoxInt();
}

fn cGetPtr(vm: *cy.VM, val: Value) callconv(.C) ?*anyopaque {
    const valT = val.getTypeId();
    switch (valT) {
        else => {
            const type_ = vm.getType(valT);
            if (type_.kind() == .int) {
                const addr: usize = @intCast(val.asBoxInt());
                return @ptrFromInt(addr);
            }
            // TODO: Since union types aren't supported yet, check for type miss.
            cy.panicFmt("Expected `pointer`, got type id: `{}`", .{valT});
        },
    }
}

fn cGetFuncPtr(vm: *cy.VM, val: Value) callconv(.C) ?*anyopaque {
    const valT = val.getTypeId();
    switch (valT) {
        else => {
            const type_ = vm.getType(valT);
            if (type_.kind() == .int) {
                const addr: usize = @intCast(val.asBoxInt());
                return @ptrFromInt(addr);
            }
            // TODO: Since union types aren't supported yet, check for type miss.
            cy.panicFmt("Expected `pointer`, got type id: `{}`", .{valT});
        },
    }
}

fn cAllocCyPointer(vm: *cy.VM, ptr: ?*anyopaque) callconv(.C) Value {
    const bt_data = vm.getData(*cy.builtins.BuiltinsData, "builtins");
    return cy.heap.allocPointer(vm, bt_data.PtrVoid.id(), ptr) catch cy.fatal();
}

fn cAllocInt(vm: *cy.VM, val: i64) callconv(.C) Value {
    return vm.allocInt(val) catch cy.fatal();
}

fn cAllocObject(vm: *cy.VM, id: cy.TypeId) callconv(.C) Value {
    const size = vm.compiler.sema.getType(id).cast(.struct_t).size;
    if (size <= 4) {
        return cy.heap.allocEmptyObjectSmall(vm, id) catch cy.fatal();
    } else {
        return cy.heap.allocEmptyObject(vm, id, size) catch cy.fatal();
    }
}

/// TODO: Once objects become unboxed, this won't require fetching the array type.
fn cAllocArrayEmpty(vm: *cy.VM, elem_tid: cy.TypeId, n: usize, size: usize) callconv(.C) Value {
    const elem_t = vm.getType(elem_tid);
    const arr_t = sema.getArrayType(vm.compiler.api_chunk, n, elem_t) catch cy.fatal();
    return cy.heap.allocEmptyObject2(vm, arr_t.id(), size) catch cy.fatal();
}

fn cyCallFunc(vm: *cy.VM, func: Value, args: [*]const Value, nargs: u8) callconv(.C) Value {
    // TODO: Since the host may decide to ignore an error, force an exit here.
    //       A better approach is to return the error but disable the VM until the error has been acknowledged by the host.
    const res = vm.callFunc(func, args[0..nargs], .{}) catch {
        cy.panic("cyCallFunc error.");
    };
    return res;
}

fn cPrintValue(val: u64) void {
    const v: Value = @bitCast(val);
    v.dump();
}

fn breakpoint() callconv(.C) void {
    @breakpoint();
}

pub fn ffiBindCallback(vm: *cy.VM) anyerror!Value {
    if (!cy.hasFFI) return vm.prepPanic("Unsupported.");

    const ffi = vm.getHostObject(*FFI, 0);
    const func = vm.getValue(1);
    const params = vm.getObject(*cy.heap.List, 2).items();
    const ret = try ffi.toCType(vm, vm.getValue(3));

    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(vm.alloc);
    const w = csrc.writer(vm.alloc);

    const cgen = CGen{ .ffi = ffi };
    try cgen.genHeaders(w);
    try cgen.genStructDecls(w);
    try cgen.genArrayConvDecls(w);
    try cgen.genStructConvs(w);
    try cgen.genArrayConvs(vm, w);

    // Emit C func declaration.
    try writeCType(w, ret);
    try w.print(" cyExternFunc(", .{});
    if (params.len > 0) {
        var ctype = try ffi.toCType(vm, params[0]);
        try writeCType(w, ctype);
        try w.print(" p0", .{});

        if (params.len > 1) {
            for (params[1..], 1..) |param, i| {
                try w.print(", ", .{});
                ctype = try ffi.toCType(vm, param);
                try writeCType(w, ctype);
                try w.print(" p{}", .{i});
            }
        }
    }
    try w.print(") {{\n", .{});

    try w.print("  VM* vm = (VM*)0x{X};\n", .{@intFromPtr(vm)});
    try w.print("  uint64_t args[{}];\n", .{params.len});
    for (params, 0..) |param, i| {
        const ctype = try ffi.toCType(vm, param);
        try w.print("  args[{}] = ", .{i});
        const cval = try std.fmt.bufPrint(&cy.tempBuf, "p{}", .{i});
        try writeToCyValue(w, cval, ctype, true);
        try w.print(";\n", .{});
    }

    try w.print("  int64_t res = _cyCallFunc(vm, 0x{X}, &args[0], {});\n", .{
        func.val,
        params.len,
    });

    // Release temp args.
    for (params, 0..) |_, i| {
        try w.print("  _cyRelease(vm, args[{}]);\n", .{i});
    }

    try w.print("  ", .{});
    try writeCType(w, ret);
    try w.print(" ret = ", .{});
    try writeToCValue(w, "res", ret, true);
    try w.print(";\n", .{});
    try w.print("  _cyRelease(vm, res);\n", .{});
    try w.print("  return ret;\n", .{});
    try w.print("}}\n", .{});

    try w.writeByte(0);
    if (DumpCGen) {
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
    _ = tcc.tcc_add_symbol(state, "icyGetPtr", cGetPtr);
    _ = tcc.tcc_add_symbol(state, "cy_as_boxint", cAsBoxInt);
    _ = tcc.tcc_add_symbol(state, "icyAllocCyPointer", cAllocCyPointer);
    _ = tcc.tcc_add_symbol(state, "icyAllocObject", cAllocObject);
    _ = tcc.tcc_add_symbol(state, "icyAllocArrayEmpty", cAllocArrayEmpty);
    _ = tcc.tcc_add_symbol(state, "cy_alloc_int", cAllocInt);
    _ = tcc.tcc_add_symbol(state, "_cyCallFunc", cyCallFunc);
    _ = tcc.tcc_add_symbol(state, "_cyRelease", cyRelease);
    // _ = tcc.tcc_add_symbol(state, "printValue", cPrintValue);
    if (builtin.cpu.arch == .aarch64) {
        _ = tcc.tcc_add_symbol(state, "memmove", memmove);
    }

    if (tcc.tcc_relocate(state, tcc.TCC_RELOCATE_AUTO) < 0) {
        cy.panic("Failed to relocate compiled code.");
    }

    const tccState = try cy.heap.allocTccState(vm, state.?, null);

    const funcPtr = tcc.tcc_get_symbol(state, "cyExternFunc") orelse {
        cy.panic("Failed to get symbol.");
    };

    // Extern func consumes tcc state and retains func.
    vm.retain(func);

    // Create extern func.
    const res = try cy.heap.allocExternFunc(vm, func, funcPtr, tccState);

    // ffi manages the extern func. (So it does not get freed).
    vm.retain(res);
    try ffi.managed.append(vm.alloc, res);

    return res;
}

pub fn ffiBindObjPtr(vm: *cy.VM) anyerror!Value {
    if (!vm.getValue(1).isPointer()) return error.InvalidArgument;

    const ffi = vm.getHostObject(*FFI, 0);
    const obj = vm.getValue(1).asHeapObject();

    // Retain and managed by FFI context.
    vm.retainObject(obj);
    try ffi.managed.append(vm.alloc, vm.getValue(1));
    
    return Value.initRaw(@intFromPtr(obj));
}

pub fn ffiUnbindObjPtr(vm: *cy.VM) anyerror!Value {
    if (!vm.getValue(1).isPointer()) return error.InvalidArgument;

    const ffi = vm.getHostObject(*FFI, 0);
    const val = vm.getValue(1);

    // Linear scan for now.
    for (ffi.managed.items, 0..) |ffiObj, i| {
        if (val.val == ffiObj.val) {
            vm.release(val);
            _ = ffi.managed.swapRemove(i);
            break;
        }
    }

    return Value.Void;
}
