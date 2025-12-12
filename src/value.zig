const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const debug = builtin.mode == .Debug;
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const bt = cy.types.BuiltinTypes;
const vmc = @import("vmc");

const SignMask: u64 = 1 << 63;
const TaggedValueMask: u64 = 0x7ffc000000000000;

const ErrorMask: u64 = TaggedValueMask | (@as(u64, TagError) << 32);

const TagMask: u32 = vmc.TAG_MASK;
const TaggedPrimitiveMask: u64 = vmc.TAGGED_PRIMITIVE_MASK;
const TaggedUpperMask: u64 = vmc.TAGGED_UPPER_MASK;
const MinPtrMask: u64 = vmc.MIN_PTR_MASK;
const MinRefMask: u64 = vmc.MIN_REF_MASK;
const PtrPayloadMask: u64 = vmc.PTR_PAYLOAD_MASK;

/// The tag id is also the primitive type id.
const TagId = u3;
pub const TagVoid: TagId = 1;
pub const TagBoolean: TagId = 2;
pub const TagError: TagId = 3;
pub const TagSymbol: TagId = 6;
pub const TagInteger: TagId = 7;

pub const TypeValue = extern struct {
    type: *cy.Type,
    value: Value,

    pub fn init(type_: *cy.Type, value: Value) TypeValue {
        return .{ .type = type_, .value = value };
    }
};

/// NaN tagging over a f64 value.
/// Represents a f64 value if not a quiet nan.
/// Otherwise, the sign bit represents either a pointer value or a special value (true, false, none, etc).
/// Pointer values can be at most 50 bits since the sign bit (1), quiet nan (12),
/// and one more bit (so that QNANs can also be a number value) take up 13 bits.
pub const Value = packed union {
    val: u64,
    ptr: ?*anyopaque,
    u32: u32,
    u16: u16,
    byte: u8,

    call_info: cy.thread.CallInfo,
    retPcPtr: [*]cy.Inst,
    retFramePtr: [*]Value,

    /// This is only used to return something from binded functions that have void return.
    /// It should never be encountered by user code.
    /// TODO: Remove once binded functions no longer have a Value return.
    pub const Void = Value{ .val = 0 };
    
    pub const True = Value{ .val = 1 };
    pub const False = Value{ .val = 0 };

    /// Interrupt value. Represented as an error tag literal with a null tag id.
    /// Returned from native funcs.
    pub const Interrupt = Value{ .val = ErrorMask | (@as(u32, 0xFF) << 8) | std.math.maxInt(u8) };

    pub inline fn as_eval_int(self: *const Value) i64 {
        return @bitCast(self.val);
    }

    pub inline fn asRefInt(self: *const Value) i64 {
        return self.asHeapObject().integer.val;
    }

    pub inline fn asInt(self: *const Value) i64 {
        return @bitCast(self.val);
    }

    pub inline fn asUint(self: *const Value) u64 {
        return self.val;
    }

    pub inline fn as_raw_lit(self: *const Value, bits: usize) u64 {
        switch (bits) {
            8 => return self.asByte(),
            16 => return self.asU16(),
            32 => return self.asU32(),
            64 => return self.asUint(),
            else => @panic("unexpected"),
        }
    }

    pub inline fn asRefByte(self: *const Value) u8 {
        return self.asHeapObject().byte.val;
    }

    pub inline fn asByte(self: *const Value) u8 {
        return self.byte;
    }

    pub inline fn asI8(self: *const Value) i8 {
        return @bitCast(self.byte);
    }

    pub inline fn asU8(self: *const Value) u8 {
        return self.byte;
    }

    pub inline fn asI16(self: *const Value) i16 {
        return @bitCast(self.u16);
    }

    pub inline fn asU16(self: *const Value) u16 {
        return self.u16;
    }

    pub inline fn asRefI32(self: *const Value) i32 {
        return @as(*i32, @ptrCast(&self.asHeapObject().integer.val)).*;
    }

    pub inline fn asI32(self: *const Value) i32 {
        return @bitCast(self.u32);
    }

    pub inline fn asU32(self: *const Value) u32 {
        return self.u32;
    }

    pub inline fn asPtr(self: *const Value, comptime Ptr: type) Ptr {
        // NOTE: Access `ptr` since the upper bits may be undefined for 32-bit pointers.
        return @ptrCast(@alignCast(self.ptr));
    }

    pub inline fn asAnyPtr(self: *const Value) ?*anyopaque {
        return @ptrCast(self.ptr);
    }

    pub inline fn asF64toI32(self: *const Value) i32 {
        const f = self.asF64();
        if (self.val & 0x7ff0000000000000 != 0x7ff0000000000000) {
            return @truncate(@as(i64, @intFromFloat(f)));
        } else {
            return 0;
        }
    }

    pub inline fn asF64toI64(self: *const Value) i64 {
        const f = self.asF64();
        if (self.val & 0x7ff0000000000000 != 0x7ff0000000000000) {
            // Not nan or inf.
            return @intFromFloat(f);
        } else {
            return 0;
        }
    }

    pub inline fn asF64toU32(self: *const Value) u32 {
        @setRuntimeSafety(debug);
        return @intFromFloat(self.asF64());
    }

    pub inline fn asF32(self: *const Value) f32 {
        return @bitCast(self.u32);
    }

    pub inline fn asF64(self: *const Value) f64 {
        return @bitCast(self.val);
    }

    pub inline fn asRefF64(self: *const Value) f64 {
        return @bitCast(self.asHeapObject().object.firstValue.val);
    }

    pub inline fn asSymbol(self: *const Value) u64 {
        return self.val;
    }

    pub inline fn asRefSymbol(self: *const Value) u64 {
        return @bitCast(self.asHeapObject().integer.val);
    }

    pub inline fn asError(self: *const Value) u64 {
        return self.val;
    }

    pub inline fn asRefError(self: *const Value) u64 {
        return @bitCast(self.asHeapObject().integer.val);
    }

    pub fn as_eval_str(val: Value) []const u8 {
        return val.asPtr(*cy.heap.EvalStr).slice();
    }

    pub fn asString(val: Value) []const u8 {
        return val.asPtr(*cy.heap.Str).slice();
    }

    pub fn asStrZ(val: Value) []const u8 {
        return val.asPtr(*cy.heap.StrZ).slice();
    }

    pub inline fn assumeNotBoolToBool(self: *const Value) bool {
        return !self.isNone();
    }

    pub inline fn isRefArray(self: *const Value) bool {
        return self.asHeapObject().getTypeId() == bt.Array;
    }

    pub inline fn isRefString(self: *const Value) bool {
        return self.asHeapObject().getTypeId() == bt.Str;
    }

    pub inline fn bothFloats(a: Value, b: Value) bool {
        return a.isFloat() and b.isFloat();
    }

    pub inline fn bothBoxInts(a: Value, b: Value) bool {
        return a.isBoxInt() and b.isBoxInt();
    }

    pub inline fn getPrimitiveTypeId(self: *const Value) u32 {
        if (self.isFloat()) {
            return bt.F64;
        } else {
            return self.getTag();
        }
    }

    pub inline fn getRefeeType(self: *const Value) cy.TypeId {
        return self.asHeapObject().getTypeId();
    }

    pub inline fn isObjectType(self: *const Value, typeId: cy.TypeId) bool {
        return self.asHeapObject().getTypeId() == typeId;
    }
    
    pub inline fn isFloatOrPointer(self: *const Value) bool {
        // This could be faster if the 3 bits past the 48 pointer bits represents a non primitive number value.
        return self.isFloat() or self.isBoxPtr();
    }

    pub inline fn isRefFloat(self: *const Value) bool {
        return self.asHeapObject().getTypeId() == bt.F64;
    }

    pub inline fn isRefInt(self: *const Value) bool {
        return self.asHeapObject().getTypeId() == bt.I64;
    }

    pub inline fn isPointerT(self: *const Value) bool {
        return self.isBoxPtr() and self.asBoxObject().getTypeId() == bt.Pointer;
    }

    pub inline fn isClosure(self: *const Value) bool {
        return self.isBoxPtr() and self.asBoxObject().getTypeId() == bt.Closure;
    }

    pub inline fn asHeapObject(self: *const Value) *cy.HeapObject {
        // NOTE: Access `ptr` since the upper bits may be undefined for 32-bit pointers.
        return @ptrCast(@alignCast(self.ptr));
    }

    pub inline fn asBytes(self: *const Value) [*]u8 {
        return @ptrCast(self.ptr);
    }

    pub inline fn asBool(self: *const Value) bool {
        return self.val == 1;
    }

    pub inline fn isTrue(self: *const Value) bool {
        return self.val == 1;
    }

    pub inline fn isInterrupt(self: *const Value) bool {
        return self.val == Interrupt.val;
    }

    pub inline fn isRefBool(self: *const Value) bool {
        return self.asHeapObject().getTypeId() == bt.Bool;
    }

    pub inline fn getTag(self: *const Value) u4 {
        return @intCast(@as(u32, @intCast(self.val >> 32)) & TagMask);
    }

    pub inline fn initFuncSig(ptr: *cy.FuncSig) Value {
        return .{ .ptr = ptr };
    }

    pub inline fn initType(ptr: *cy.Type) Value {
        return .{ .ptr = ptr };
    }

    pub inline fn initSymbol(symId: u64) Value {
        return .{ .val = symId };
    }

    pub inline fn initFloat32(val: f32) Value {
        return .{ .u32 = @bitCast(val) };
    }

    pub inline fn initFloat64(val: f64) Value {
        return .{ .val = @as(u64, @bitCast(val)) };
    }

    pub inline fn initInt(val: i64) Value {
        return .{ .val = @bitCast(val) };
    }

    pub inline fn initGenericInt(val: i64) Value {
        return .{ .val = @bitCast(val) };
    }

    pub inline fn initR8(val: u8) Value {
        return .{ .byte = val };
    }

    pub inline fn initInt8(val: i8) Value {
        return .{ .byte = @bitCast(val) };
    }

    pub inline fn initR32(val: u32) Value {
        return .{ .u32 = val };
    }

    pub inline fn initInt32(val: i32) Value {
        return .{ .u32 = @bitCast(val) };
    }

    pub inline fn initR16(val: u16) Value {
        return .{ .u16 = val };
    }

    pub inline fn initR64(val: u64) Value {
        return .{ .val = val };
    }

    pub inline fn initInt16(val: i16) Value {
        return .{ .u16 = @bitCast(val) };
    }

    pub inline fn initRaw(val: u64) Value {
        return .{ .val = val };
    }

    pub inline fn initBool(b: bool) Value {
        if (b) {
            return True;
        } else {
            return False;
        }
    }

    pub inline fn initPtr(ptr: ?*anyopaque) Value {
        return .{ .val = @intFromPtr(ptr) };
    }

    pub inline fn floatIsSpecial(val: f64) bool {
        @setRuntimeSafety(debug);
        if (std.math.isInf(val)) return true;
        return false;
    }

    pub inline fn floatCanBeInteger(val: f64) bool {
        @setRuntimeSafety(debug);
        // return @fabs(std.math.floor(val) - val) < std.math.f64_epsilon;
        
        // This seems to be the faster check so far.
        return std.math.floor(val) == val;
    }

    pub inline fn initError(err: u64) Value {
        return .{ .val = err };
    }

    pub fn fromSliceC(self: C.ValueSlice) []const Value {
        if (self.len == 0) {
            return &.{};
        }
        return @ptrCast(self.ptr[0..self.len]);
    }

    pub fn toSliceC(slice: []const cy.Value) C.ValueSlice {
        return .{
            .ptr = @ptrCast(slice.ptr),
            .len = slice.len,
        };
    }

    pub fn toC(self: Value) C.Value {
        return @bitCast(self);
    }
};

pub fn isHostFunc(vm: *cy.VM, val: Value) bool {
    const obj = val.asHeapObject();
    const type_id = obj.getTypeId();
    if (type_id == bt.Func) {
        return obj.func.kind == .host;
    }
    const type_e = vm.getType(type_id);
    if (type_e.kind() == .func_ptr) {
        return vm.funcSyms.buf[@intCast(obj.integer.val)].type == .host_func;
    } else if (type_e.kind() == .func_union) {
        return obj.func_union.kind == .host;
    } else {
        return false;
    }
}

test "floatCanBeInteger" {
    var f: f64 = -100000000000;
    while (f < 100000000000) : (f += 10000) {
        if (std.math.floor(f) == f) {
            continue;
        } else try t.fail();
    }
}

test "asF64" {
    // +Inf.
    var val = Value{ .val = 0x7ff0000000000000 };
    try t.eq(val.asF64(), std.math.inf(f64));

    // -Inf.
    val = Value{ .val = 0xfff0000000000000 };
    try t.eq(val.asF64(), -std.math.inf(f64));
}

test "value internals." {
    try t.eq(@sizeOf(Value), 8);
    try t.eq(@alignOf(Value), 8);
    try t.eq(Value.Interrupt.val, 0x7ffc00030000ffff);
    try t.eq(vmc.MIN_PTR_MASK, 0xFFFC000000000000);
    try t.eq(Value.initInt(0).val, 0);

    // Check Zig/C struct compat.
    try t.eq(@sizeOf(Value), @sizeOf(vmc.Value));
    // const retInfoT = std.meta.fieldInfo(Value, .retInfo).type;
    // try t.eq(@offsetOf(retInfoT, "numRetVals"), 0);
    // try t.eq(@offsetOf(retInfoT, "retFlag"), 1);
}

pub fn PtrSpan(T: type) type {
    return extern struct {
        ptr: [*]T,
        len: usize,
    };
}

pub fn Option(T: type) type {
    return extern struct {
        tag: u64,
        inner: T,

        pub fn some(value: T) @This() {
            return .{
                .tag = 1,
                .inner = value,
            };
        }

        pub fn none() @This() {
            return .{
                .tag = 0,
                .inner = undefined,
            };
        }
    };
}

pub fn ones(bits: usize) u64 {
    return if (bits == 64) std.math.maxInt(u64) else (@as(u64, 1) << @intCast(bits)) - 1;
}