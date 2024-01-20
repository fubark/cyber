const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const tcc = @import("tcc");
const cy = @import("../cyber.zig");
const c = @import("../clib.zig");
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
    object: cy.TypeId,
    arr: struct {
        child: *CType,
        n: u32,
    },

    fn deinit(self: CType, alloc: std.mem.Allocator) void {
        if (self == .arr) {
            self.arr.child.deinit(alloc);
            alloc.destroy(self.arr.child);
        }
    }
};

const CStructInfo = struct {
    type: cy.TypeId,
    fields: []const CType,
};

pub var FFIT: cy.TypeId = undefined;

pub const FFI = struct {
    /// Ordered to handle C decl rules.
    cstructs: std.ArrayListUnmanaged(CStructInfo),

    typeToCStruct: std.AutoHashMapUnmanaged(cy.TypeId, u32),

    carrays: std.ArrayListUnmanaged(CType),
    carrayMap: std.StringHashMapUnmanaged(u32),

    cfuncs: std.ArrayListUnmanaged(CFuncData),

    /// Managed ExternFunc or Objects.
    managed: std.ArrayListUnmanaged(Value),

    fn toCType(self: *FFI, vm: *cy.VM, spec: Value) !CType {
        if (spec.isObjectType(os.CArrayT)) {
            const nField = try vm.ensureFieldSym("n");
            const n: u32 = @intCast((try vm.getField(spec, nField)).asInteger());
            const elemField = try vm.ensureFieldSym("elem");
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
        if (spec.isObjectType(bt.MetaType)) {
            const typeId = spec.asHeapObject().metatype.type;
            if (self.typeToCStruct.contains(typeId)) {
                return CType{ .object = typeId };
            } else {
                const name = try vm.getOrBufPrintValueStr(&cy.tempBuf, spec);
                log.debug("CStruct not declared for: {s}", .{name});
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
        const key = try std.fmt.allocPrint(alloc, "{s}[{}]", .{elemName, arrT.arr.n});
        if (!self.carrayMap.contains(key)) {
            const id = self.carrays.items.len;
            try self.carrays.append(alloc, arrT);
            try self.carrayMap.put(alloc, key, @intCast(id));
        } else {
            alloc.free(key);
        }
    }
};

pub fn allocFFI(vm: *cy.VM) linksection(cy.StdSection) !Value {
    const ffi: *FFI = @ptrCast(try cy.heap.allocHostNoCycObject(vm, FFIT, @sizeOf(FFI)));
    ffi.* = .{
        .cstructs = .{},
        .typeToCStruct = .{},
        .carrays = .{},
        .carrayMap = .{},
        .cfuncs = .{},
        .managed = .{},
    };
    return Value.initHostNoCycPtr(ffi);
}

pub fn ffiGetChildren(_: ?*c.VM, obj: ?*anyopaque) callconv(.C) c.ValueSlice {
    var ffi: *FFI = @ptrCast(@alignCast(obj));
    return c.initValueSlice(ffi.managed.items);
}

pub fn ffiFinalizer(vm_: ?*c.VM, obj: ?*anyopaque) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    log.tracev("ffi finalize", .{});

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

    ffi.carrays.deinit(alloc);

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
            // TODO: Check for 32bit addressing.
            \\#define size_t uint64_t
            \\#define int8_t signed char
            \\#define uint8_t unsigned char
            \\#define int16_t short
            \\#define uint16_t unsigned short
            \\#define int32_t int
            \\#define uint32_t unsigned int
            \\#define PointerMask 0xFFFE000000000000
            \\typedef struct UserVM *UserVM;
            \\extern void _cyRelease(UserVM*, uint64_t);
            \\extern void* icyGetPtr(uint64_t);
            \\extern void* _cyGetFuncPtr(uint64_t);
            \\extern uint64_t icyAllocObject(UserVM*, uint32_t);
            \\extern uint64_t icyAllocList(UserVM*, uint64_t*, uint32_t);
            \\extern uint64_t icyAllocCyPointer(UserVM*, void*);
            \\extern uint64_t _cyCallFunc(UserVM*, uint64_t, uint64_t*, uint8_t);
            \\extern int printf(char* fmt, ...);
            // \\extern void exit(int code);
            \\
        , .{});
    }

    // Generate C structs.
    pub fn genStructDecls(self: CGen, w: anytype) !void {
        for (self.ffi.cstructs.items) |cstruct| {
            try w.print("typedef struct Struct{} {{\n", .{cstruct.type});
            for (cstruct.fields, 0..) |field, i| {
                try w.writeAll("  ");
                const name = try std.fmt.bufPrint(&cy.tempBuf, "f{}", .{i});
                try writeNamedCType(w, field, name);
                try w.writeAll(";\n");
            }
            try w.print("}} Struct{};\n", .{cstruct.type});
        }
    }

    // Generate array conversions declarations.
    pub fn genArrayConvDecls(self: CGen, w: anytype) !void {
        for (self.ffi.carrays.items) |carr| {
            const elemName = try fmtBaseTypeName(&cy.tempBuf, carr.arr.child.*);

            // to{elem}Array{n}
            try w.print("void to{s}Array{}(UserVM* vm, uint64_t val, ", .{elemName, carr.arr.n});
            try writeCType(w, carr.arr.child.*);
            try w.print("* out);\n", .{});

            // from{elem}Array{n}.
            try w.print("uint64_t from{s}Array{}(UserVM* vm, ", .{elemName, carr.arr.n});
            try writeCType(w, carr.arr.child.*);
            try w.print(" arr[{}]);\n", .{carr.arr.n});
        }
    }

    // Generate struct conversions.
    pub fn genStructConvs(self: CGen, w: anytype) !void {
        const ffi = self.ffi;
        for (ffi.cstructs.items) |cstruct| {
            // Generate to C struct conv.
            try w.print("Struct{} toStruct{}(UserVM* vm, uint64_t val) {{\n", .{cstruct.type, cstruct.type});
            // Get pointer to first cyber object field.
            try w.print("  uint64_t* args = (uint64_t*)(val & ~PointerMask) + 1;\n", .{});
            try w.print("  Struct{} res;\n", .{cstruct.type, });
            for (cstruct.fields, 0..) |field, i| {
                if (field == .arr) {
                    const elemName = try fmtBaseTypeName(&cy.tempBuf, field.arr.child.*);
                    try w.print("  to{s}Array{}(vm, args[{}], &res.f{}[0]);", .{elemName, field.arr.n, i, i});
                } else {
                    try w.print("  res.f{} = ", .{i});
                    const val = try std.fmt.bufPrint(&cy.tempBuf, "args[{}]", .{i});
                    try writeToCValue(w, val, field);
                    try w.print(";\n", .{});
                }
            }
            try w.print("  return res;\n", .{});
            try w.print("}}\n", .{});

            // Generate from C struct conv.
            try w.print("uint64_t fromStruct{}(UserVM* vm, Struct{} val) {{\n", .{cstruct.type, cstruct.type});
            try w.print("  uint64_t obj = icyAllocObject(vm, {});\n", .{cstruct.type});
            try w.print("  uint64_t* args = (uint64_t*)(obj & ~PointerMask) + 1;\n", .{});
            var buf: [8]u8 = undefined;
            for (cstruct.fields, 0..) |field, i| {
                // Int and other types.
                try w.print("  args[{}] = ", .{i});
                const argStr = try std.fmt.bufPrint(&buf, "val.f{}", .{i});
                try writeToCyValue(w, argStr, field);
                try w.print(";\n", .{});
            }
            try w.print("  return obj;\n", .{});
            try w.print("}}\n", .{});
        }
    }

    // Generate array conversions.
    pub fn genArrayConvs(self: CGen, w: anytype) !void {
        var buf: [16]u8 = undefined;
        for (self.ffi.carrays.items) |carr| {
            const elem = carr.arr.child.*;
            const n = carr.arr.n;
            const elemName = try fmtBaseTypeName(&cy.tempBuf, elem);

            // Generate to{elem}Array{n}. Writes to ptr since arrays can't be returned in C.
            try w.print("void to{s}Array{}(UserVM* vm, uint64_t val, ", .{elemName, n});
            try writeCType(w, elem);
            try w.print("* out) {{\n", .{});
            try w.print("  uint64_t* args = (uint64_t*)*((uint64_t*)(val & ~PointerMask) + 1);\n", .{});
            for (0..n) |i| {
                try w.print("  out[{}] = ", .{i});
                const val = try std.fmt.bufPrint(&buf, "args[{}]", .{i});
                try writeToCValue(w, val, elem);
                try w.writeAll(";\n");
            }
            try w.print("}}\n", .{});

            // Generate from{elem}Array{n}.
            try w.print("uint64_t from{s}Array{}(UserVM* vm, ", .{elemName, n});
            try writeCType(w, elem);
            try w.print(" arr[{}]) {{\n", .{n});

            try w.print("  uint64_t vals[{}];\n", .{n});
            for (0..n) |i| {
                try w.print("  vals[{}] = ", .{i});
                const cval = try std.fmt.bufPrint(&buf, "arr[{}]", .{i});
                try writeToCyValue(w, cval, elem);
                try w.writeAll(";\n");
            }
            try w.print("  return icyAllocList(vm, &vals[0], {});\n", .{n});
            try w.print("}}\n", .{});
        }
    }

    fn genFunc(self: *CGen, vm: *cy.UserVM, w: anytype, funcInfo: CFuncData, config: BindLibConfig) !void {
        var buf: [16]u8 = undefined;
        _ = self;
        const ivm = vm.internal();
        _ = ivm;
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

        const isMethod = !config.genMap;
        try w.print("uint64_t cy{s}(UserVM* vm, uint64_t* args, char numArgs) {{\n", .{sym});
        if (isMethod) {
            try w.print("  uint64_t recv = args[0];\n", .{});
        }

        // Temps.
        if (params.len > 0) {
            for (params, 0..) |param, i| {
                const argIdx = if (isMethod) i + 1 else i;
                if (param == .arr) {
                    const elem = param.arr.child.*;
                    const elemName = try fmtBaseTypeName(&cy.tempBuf, elem);

                    try w.print("  ", .{});
                    const cval = try std.fmt.bufPrint(&buf, "arr{}", .{argIdx});
                    try writeNamedCType(w, param, cval);
                    try w.print(";\n", .{});

                    try w.print("  to{s}Array{}(vm, args[{}], &arr{}[0]);", .{elemName, param.arr.n, argIdx, argIdx});
                } else {
                    continue;
                }
            }
        }

        // Gen call.
        if (ret == .object) {
            try w.print("  Struct{} res = {s}(", .{ret.object, sym});
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
            var argIdx: usize = if (isMethod) 1 else 0;
            if (params[0] == .arr) {
                try w.print("arr{}", .{argIdx});
            } else {
                const val = try std.fmt.bufPrint(&buf, "args[{}]", .{argIdx});
                try writeToCValue(w, val, params[0]);
            }

            if (params.len > 1) {
                for (params[1..], 1..) |param, i| {
                    try w.print(", ", .{});
                    argIdx = if (isMethod) i + 1 else i;
                    if (param == .arr) {
                        try w.print("arr{}", .{argIdx});
                    } else {
                        const val = try std.fmt.bufPrint(&buf, "args[{}]", .{argIdx});
                        try writeToCValue(w, val, param);
                    }
                }
            }
        }

        // End of args.
        try w.print(");\n", .{});

        // Gen return.
        try w.print("  return ", .{});
        try writeToCyValue(w, "res", ret);
        try w.print(";\n", .{});

        try w.print("}}\n", .{});
    }
};

pub fn ffiCbind(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    const ivm = vm.internal();
    const alloc = vm.allocator();
    const ffi = args[0].castHostObject(*FFI);
    const mt = args[1].asHeapObject();
    if (mt.metatype.typeKind != @intFromEnum(cy.heap.MetaTypeKind.object)) {
        log.debug("Not an object Symbol", .{});
        return error.InvalidArgument;
    }
    const typeId = mt.metatype.type;
    const fieldsList = &args[2].asHeapObject().list;

    if (ffi.typeToCStruct.contains(typeId)) {
        log.debug("Object type already declared.", .{});
        return error.InvalidArgument;
    }
    const id = ffi.cstructs.items.len;
    try ffi.typeToCStruct.put(alloc, typeId, @intCast(id));

    var fieldsBuilder: std.ArrayListUnmanaged(CType) = .{};
    for (fieldsList.items()) |field| {
        try fieldsBuilder.append(alloc, try ffi.toCType(ivm, field));
    }

    const fields = try fieldsBuilder.toOwnedSlice(alloc);
    try ffi.cstructs.append(alloc, .{ .type = typeId, .fields = fields });

    return Value.None;
}

pub fn ffiCfunc(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    const ffi = args[0].castHostObject(*FFI);

    const funcArgs = args[2].asHeapObject().list.items();

    const ctypes = try vm.alloc.alloc(CType, funcArgs.len);
    if (funcArgs.len > 0) {
        for (funcArgs, 0..) |arg, i| {
            ctypes[i] = try ffi.toCType(vm, arg);
        }
    }

    const retType = try ffi.toCType(vm, args[3]);

    const symName = try vm.getOrBufPrintValueStr(&cy.tempBuf, args[1]);
    try ffi.cfuncs.append(vm.alloc, .{
        .namez = try vm.alloc.dupeZ(u8, symName),
        .params = ctypes,
        .ret = retType,
        .ptr = undefined,
        .funcSigId = undefined,
        .skip = false,
    });

    return Value.None;
}

pub fn ffiNew(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    // const ffi = args[0].castHostObject(*FFI);

    const csym = try std.meta.intToEnum(Symbol, args[1].asSymbolId());
    const size = try sizeOf(csym);

    const ptr = std.c.malloc(size);
    return cy.heap.allocPointer(vm, ptr);
}

/// Returns the name of a base type. No arrays.
fn fmtBaseTypeName(buf: []u8, ctype: CType) ![]const u8 {
    switch (ctype) {
        .object => |objectT| {
            return std.fmt.bufPrint(buf, "Struct{}", .{objectT});
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
    /// Whether bindLib generates the binding to an anonymous object type as methods
    /// or a map with functions.
    genMap: bool = false,
};

fn writeCType(w: anytype, ctype: CType) !void {
    switch (ctype) {
        .object => |objectT| {
            try w.print("Struct{}", .{objectT});
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

fn writeToCValue(w: anytype, val: []const u8, ctype: CType) !void {
    switch (ctype) {
        .object => |objectT| {
            try w.print("toStruct{}(vm, {s})", .{objectT, val});
        },
        .sym => |sym| {
            try writeToCValueForSym(w, val, sym);
        },
        else => {
            std.debug.print("Unsupported arg type: {s}\n", .{ @tagName(ctype) });
            return error.InvalidArgument;
        }
    }
}

fn writeToCValueForSym(w: anytype, val: []const u8, sym: Symbol) !void {
    switch (sym) {
        .bool => {
            try w.print("({s} == 0x7FFC000100000001)?1:0", .{val});
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
            try w.print("(int)({s} & 0xFFFFFFFFFFFF)", .{val});
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
            try w.print("(char*)icyGetPtr({s})", .{val});
        },
        .voidPtr => {
            try w.print("icyGetPtr({s})", .{val});
        },
        .funcPtr => {
            try w.print("_cyGetFuncPtr({s})", .{val});
        },
        else => {
            std.debug.print("Unsupported arg type: {s}\n", .{ @tagName(sym) });
            return error.InvalidArgument;
        }
    }
}

fn writeToCyValue(w: anytype, cval: []const u8, ctype: CType) !void {
    switch (ctype) {
        .object => |objectT| {
            try w.print("fromStruct{}(vm, {s})", .{ objectT, cval });
        },
        .arr => |arr| {
            const elemName = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
            try w.print("from{s}Array{}(vm, {s})", .{elemName, arr.n, cval});
        },
        .sym => |sym| {
            try writeToCyValueForSym(w, cval, sym);
        },
    }
}

fn writeToCyValueForSym(w: anytype, cval: []const u8, ctype: Symbol) !void {
    switch (ctype) {
        .char,
        .short,
        .int,
        .long,
        .ulong,
        .usize => {
            try w.print("(0x7FFE000000000000 | ({s} & 0xFFFFFFFFFFFF))", .{cval});
        },
        .uchar,
        .ushort,
        .uint => {
            try w.print("(0x7FFE000000000000 | {s})", .{cval});
        },
        .float,
        .double => {
            // Assumes cval is already converted to double.
            try w.print("*(uint64_t*)&{s}", .{cval});
        },
        .charPtr => {
            try w.print("icyAllocCyPointer(vm, {s})", .{cval});
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

fn toCyType(ctype: CType, forRet: bool) !types.TypeId {
    switch (ctype) {
        .object => |objectT| return objectT,
        .arr => return bt.List,
        .sym => |sym| {
            switch (sym) {
                .bool => return bt.Boolean,
                .char => return bt.Integer,
                .uchar => return bt.Integer,
                .short => return bt.Integer,
                .ushort => return bt.Integer,
                .int => return bt.Integer,
                .uint => return bt.Integer,
                .long => return bt.Integer,
                .ulong => return bt.Integer,
                .usize => return bt.Integer,
                .float => return bt.Float,
                .double => return bt.Float,
                .funcPtr, // pointer? or ExternFunc.
                .charPtr,
                .voidPtr => {
                    // TODO: Once optional types are implemented, this would be an optional pointer.
                    if (!forRet) {
                        return bt.Any;
                    } else {
                        return bt.Pointer;
                    }
                },
                .void => return bt.None,
                else => {
                    std.debug.print("Unsupported val type: {s}\n", .{ @tagName(sym) });
                    return error.InvalidArgument;
                }
            }
        }
    }
}

pub fn ffiBindLib(vm: *cy.UserVM, args: [*]const Value, config: BindLibConfig) !Value {
    const ffi = args[0].castHostObject(*FFI);
    const path = args[1];
    const alloc = vm.allocator();
    const ivm: *cy.VM = @ptrCast(vm);

    var success = false;

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
        const pathStr = try ivm.getOrBufPrintValueStr(&cy.tempBuf, path);
        log.tracev("bindLib {s}", .{pathStr});
        lib.* = dlopen(pathStr) catch |err| {
            if (err == error.FileNotFound) {
                return prepareThrowSymbol(vm, .FileNotFound);
            } else {
                return err;
            }
        };
    }

    // Generate c code.
    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(alloc);
    const w = csrc.writer(alloc);

    var cgen = CGen{ .ffi = ffi };

    try cgen.genHeaders(w);

    try cgen.genStructDecls(w);

    try cgen.genArrayConvDecls(w);

    try cgen.genStructConvs(w);

    try cgen.genArrayConvs(w);

    // Begin func binding generation.

    var tempTypes: std.BoundedArray(cy.TypeId, 16) = .{};
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
        if (!config.genMap) {
            // Self param.
            try tempTypes.append(bt.Any);
        }
        for (cfunc.params) |param| {
            const typeId = try toCyType(param, false);
            try tempTypes.append(typeId);
        }

        const retType = try toCyType(cfunc.ret, true);
        const funcSigId = try ivm.sema.ensureFuncSig(tempTypes.slice(), retType);

        cfunc.ptr = ptr;
        cfunc.funcSigId = funcSigId;
        try cgen.genFunc(vm, w, cfunc.*, config);
    }

    for (ffi.cstructs.items) |cstruct| {
        // Generate ptrTo[Object].
        const isMethod = !config.genMap;
        try w.print("uint64_t cyPtrTo{s}(UserVM* vm, uint64_t* args, char numArgs) {{\n", .{ivm.getTypeName(cstruct.type)});
        if (isMethod) {
            try w.print("  uint64_t ptr = *((uint64_t*)(args[1] & ~PointerMask) + 1);\n", .{});
        } else {
            try w.print("  uint64_t ptr = *((uint64_t*)(args[0] & ~PointerMask) + 1);\n", .{});
        }
        try w.print("  uint64_t res = fromStruct{}(vm, *(Struct{}*)ptr);\n", .{cstruct.type, cstruct.type});
        try w.print("  return res;\n", .{});
        try w.print("}}\n", .{});
    }

    try w.writeByte(0);
    if (DumpCGen) {
        log.debug("{s}", .{csrc.items});
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
    _ = tcc.tcc_add_symbol(state, "_cyGetFuncPtr", cGetFuncPtr);
    _ = tcc.tcc_add_symbol(state, "icyAllocCyPointer", cAllocCyPointer);
    _ = tcc.tcc_add_symbol(state, "icyAllocObject", cAllocObject);
    _ = tcc.tcc_add_symbol(state, "icyAllocList", cAllocList);
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

    if (config.genMap) {
        // Create map with binded C-functions as functions.
        const map = try ivm.allocEmptyMap();

        const cyState = try cy.heap.allocTccState(ivm, state.?, lib);

        var numGenFuncs: u32 = 0;
        for (ffi.cfuncs.items) |cfunc| {
            if (cfunc.skip) continue;
            const symGen = try std.fmt.allocPrint(alloc, "cy{s}\x00", .{cfunc.namez});
            defer alloc.free(symGen);
            const funcPtr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
                cy.panic("Failed to get symbol.");
            };

            const symKey = try ivm.retainOrAllocAstring(cfunc.namez);
            const func = cy.ptrAlignCast(cy.ZHostFuncFn, funcPtr);
            const funcSig = ivm.compiler.sema.getFuncSig(cfunc.funcSigId);
            const funcVal = try cy.heap.allocHostFunc(ivm, func, @intCast(cfunc.params.len),
                cfunc.funcSigId, cyState, funcSig.reqCallTypeCheck);
            try map.asHeapObject().map.set(ivm, symKey, funcVal);
            vm.release(symKey);
            vm.release(funcVal);
            numGenFuncs += 1;
        }
        cy.arc.retainInc(ivm, cyState, @intCast(numGenFuncs + ffi.cstructs.items.len - 1));
        for (ffi.cstructs.items) |cstruct| {
            const typeName = ivm.getTypeName(cstruct.type);
            const symGen = try std.fmt.allocPrint(alloc, "cyPtrTo{s}{u}", .{typeName, 0});
            defer alloc.free(symGen);
            const funcPtr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
                cy.panic("Failed to get symbol.");
            };

            const symKey = try ivm.allocAstringConcat("ptrTo", typeName);
            const func = cy.ptrAlignCast(cy.ZHostFuncFn, funcPtr);

            const funcSigId = try ivm.sema.ensureFuncSig(&.{bt.Dynamic}, bt.Dynamic);
            const funcVal = try cy.heap.allocHostFunc(ivm, func, 1, funcSigId, cyState, false);
            try map.asHeapObject().map.set(ivm, symKey, funcVal);
            vm.release(symKey);
            vm.release(funcVal);
        }
        success = true;
        return map;
    } else {
        // Create anonymous struct with binded C-functions as methods.
        const osSym = ivm.compiler.chunkMap.get("os").?.sym;
        const sid = try ivm.addAnonymousStruct(@ptrCast(osSym), "BindLib", os.nextUniqId, &.{ "tcc" });
        os.nextUniqId += 1;
        const tccField = try ivm.ensureFieldSym("tcc");
        try ivm.addFieldSym(sid, tccField, 0, bt.Any);

        const cyState = try cy.heap.allocTccState(ivm, state.?, lib);
        for (ffi.cfuncs.items) |cfunc| {
            if (cfunc.skip) continue;
            const cySym = try std.fmt.allocPrint(alloc, "cy{s}\x00", .{cfunc.namez});
            defer alloc.free(cySym);
            const funcPtr = tcc.tcc_get_symbol(state, cySym.ptr) orelse {
                cy.panic("Failed to get symbol.");
            };

            const func = cy.ptrAlignCast(cy.ZHostFuncFn, funcPtr);
            const mgId = try ivm.ensureMethodGroup(cfunc.namez);
            try @call(.never_inline, cy.VM.addMethod, .{ivm, sid, mgId, rt.MethodInit.initHostTyped(cfunc.funcSigId, func, @as(u8, @intCast(cfunc.params.len)) + 1) });
        }
        for (ffi.cstructs.items) |cstruct| {
            const typeName = ivm.getTypeName(cstruct.type);
            const symGen = try std.fmt.allocPrint(alloc, "cyPtrTo{s}{u}", .{typeName, 0});
            defer alloc.free(symGen);
            const funcPtr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
                cy.panic("Failed to get symbol.");
            };
            const func = cy.ptrAlignCast(cy.ZHostFuncFn, funcPtr);

            const methodName = try std.fmt.allocPrint(alloc, "ptrTo{s}", .{typeName});
            defer alloc.free(methodName);
            const mgId = try ivm.ensureMethodGroup2(methodName, true);
            const funcSigId = try ivm.sema.ensureFuncSig(&.{ bt.Any, bt.Pointer }, cstruct.type);
            try @call(.never_inline, cy.VM.addMethod, .{ivm, sid, mgId, rt.MethodInit.initHostTyped(funcSigId, func, 2) });
        }
        success = true;
        return try vm.allocObjectSmall(sid, &.{cyState});
    }
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

fn cyRelease(vm: *cy.UserVM, val: Value) callconv(.C) void {
    vm.release(val);
}

fn cGetPtr(val: Value) callconv(.C) ?*anyopaque {
    const valT = val.getTypeId();
    switch (valT) {
        bt.None => return null,
        bt.Pointer => {
            return val.asHeapObject().pointer.ptr;
        },
        else => {
            // TODO: Since union types aren't supported yet, check for type miss.
            cy.panicFmt("Expected `pointer`, got type id: `{}`", .{valT});
        },
    }
}

fn cGetFuncPtr(val: Value) callconv(.C) ?*anyopaque {
    const valT = val.getTypeId();
    switch (valT) {
        bt.None => return null,
        bt.Pointer => {
            return val.asHeapObject().pointer.ptr;
        },
        bt.ExternFunc => {
            return val.asHeapObject().externFunc.ptr;
        },
        else => {
            // TODO: Since union types aren't supported yet, check for type miss.
            cy.panicFmt("Expected `pointer`, got type id: `{}`", .{valT});
        },
    }
}

fn cAllocCyPointer(vm: *cy.UserVM, ptr: ?*anyopaque) callconv(.C) Value {
    return cy.heap.allocPointer(vm.internal(), ptr) catch cy.fatal();
}

fn cAllocObject(vm: *cy.UserVM, id: u32) callconv(.C) Value {
    const ivm = vm.internal();

    const numFields = ivm.compiler.sema.types.items[id].data.numFields;
    if (numFields <= 4) {
        return cy.heap.allocEmptyObjectSmall(ivm, id) catch cy.fatal();
    } else {
        return cy.heap.allocEmptyObject(ivm, id, numFields) catch cy.fatal();
    }
}

fn cAllocList(vm: *cy.UserVM, elems: [*]Value, n: u32) callconv(.C) Value {
    const ivm = vm.internal();
    return cy.heap.allocList(ivm, elems[0..n]) catch cy.fatal();
}

fn cyCallFunc(vm: *cy.VM, func: Value, args: [*]const Value, nargs: u8) callconv(.C) Value {
    return vm.callFunc(func, args[0..nargs]) catch cy.fatal();
}

fn cPrintValue(val: u64) void {
    const v: Value = @bitCast(val);
    v.dump();
}

fn breakpoint() callconv(.C) void {
    @breakpoint();
}

pub fn ffiBindCallback(vm: *cy.UserVM, args: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasFFI) return vm.returnPanic("Unsupported.");

    const ivm = vm.internal();

    const ffi = args[0].castHostObject(*FFI);
    const func = args[1];
    const params = args[2].asHeapObject().list.items();
    const ret = try ffi.toCType(ivm, args[3]);

    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(ivm.alloc);
    const w = csrc.writer(ivm.alloc);

    const cgen = CGen{ .ffi = ffi };
    try cgen.genHeaders(w);
    try cgen.genStructDecls(w);
    try cgen.genArrayConvDecls(w);
    try cgen.genStructConvs(w);
    try cgen.genArrayConvs(w);

    // Emit C func declaration.
    try writeCType(w, ret);
    try w.print(" cyExternFunc(", .{});
    if (params.len > 0) {
        var ctype = try ffi.toCType(ivm, params[0]);
        try writeCType(w, ctype);
        try w.print(" p0", .{});

        if (params.len > 1) {
            for (params[1..], 1..) |param, i| {
                try w.print(", ", .{});
                ctype = try ffi.toCType(ivm, param);
                try writeCType(w, ctype);
                try w.print(" p{}", .{i});
            }
        }
    }
    try w.print(") {{\n", .{});

    try w.print("  UserVM* vm = (UserVM*)0x{X};\n", .{@intFromPtr(vm)});
    try w.print("  int64_t args[{}];\n", .{params.len});
    for (params, 0..) |param, i| {
        var ctype = try ffi.toCType(ivm, param);
        try w.print("  args[{}] = ", .{i});
        const cval = try std.fmt.bufPrint(&cy.tempBuf, "p{}", .{i});
        try writeToCyValue(w, cval, ctype);
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

    try w.print("  return ", .{});
    try writeToCValue(w, "res", ret);
    try w.print(";\n", .{});

    try w.print("}}\n", .{});

    try w.writeByte(0);
    if (DumpCGen) {
        log.debug("{s}", .{csrc.items});
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
    _ = tcc.tcc_add_symbol(state, "icyAllocCyPointer", cAllocCyPointer);
    _ = tcc.tcc_add_symbol(state, "icyAllocObject", cAllocObject);
    _ = tcc.tcc_add_symbol(state, "icyAllocList", cAllocList);
    _ = tcc.tcc_add_symbol(state, "_cyCallFunc", cyCallFunc);
    _ = tcc.tcc_add_symbol(state, "_cyRelease", cyRelease);
    // _ = tcc.tcc_add_symbol(state, "printValue", cPrintValue);
    if (builtin.cpu.arch == .aarch64) {
        _ = tcc.tcc_add_symbol(state, "memmove", memmove);
    }

    if (tcc.tcc_relocate(state, tcc.TCC_RELOCATE_AUTO) < 0) {
        cy.panic("Failed to relocate compiled code.");
    }

    const tccState = try cy.heap.allocTccState(ivm, state.?, null);

    const funcPtr = tcc.tcc_get_symbol(state, "cyExternFunc") orelse {
        cy.panic("Failed to get symbol.");
    };

    // Extern func consumes tcc state and retains func.
    vm.retain(func);

    // Create extern func.
    const res = try cy.heap.allocExternFunc(ivm, func, funcPtr, tccState);

    // ffi manages the extern func. (So it does not get freed).
    vm.retain(res);
    try ffi.managed.append(ivm.alloc, res);

    return res;
}

pub fn ffiBindObjPtr(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (!args[1].isPointer()) return error.InvalidArgument;

    const ffi = args[0].castHostObject(*FFI);
    const obj = args[1].asHeapObject();
    const res = vm.allocPointer(@ptrCast(obj));

    // Retain and managed by FFI context.
    vm.retainObject(obj);
    try ffi.managed.append(vm.alloc, args[1]);
    
    return res;
}

pub fn ffiUnbindObjPtr(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (!args[1].isPointer()) return error.InvalidArgument;

    const ffi = args[0].castHostObject(*FFI);
    const obj = args[1].asHeapObject();

    // Linear scan for now.
    for (ffi.managed.items, 0..) |ffiObj, i| {
        if (args[1].val == ffiObj.val) {
            vm.releaseObject(obj);
            _ = ffi.managed.swapRemove(i);
            break;
        }
    }

    return Value.None;
}
