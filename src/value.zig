const std = @import("std");
const builtin = @import("builtin");
const endian = builtin.target.cpu.arch.endian();
const stdx = @import("stdx");
const t = stdx.testing;
const debug = builtin.mode == .Debug;
const log = cy.log.scoped(.value);
const cy = @import("cyber.zig");
const rt = cy.rt;
const bt = cy.types.BuiltinTypes;
const fmt = @import("fmt.zig");
const vmc = @import("vm_c.zig");

const SignMask: u64 = 1 << 63;
const TaggedValueMask: u64 = 0x7ffc000000000000;
const IntegerMask: u64 = vmc.INTEGER_MASK;
const TaggedIntegerMask: u64 = vmc.TAGGED_INTEGER_MASK;
const TaggedUpperValueMask: u64 = vmc.TAGGED_UPPER_VALUE_MASK;
const EnumMask: u64 = vmc.ENUM_MASK;
const TaggedEnumMask: u64 = vmc.TAGGED_ENUM_MASK;

const BooleanMask: u64 = TaggedValueMask | (@as(u64, TagBoolean) << 32);
const FalseMask: u64 = BooleanMask;
const TrueMask: u64 = BooleanMask | TrueBitMask;
const TrueBitMask: u64 = 1;
const NoneMask: u64 = TaggedValueMask | (@as(u64, TagNone) << 32);
const ErrorMask: u64 = TaggedValueMask | (@as(u64, TagError) << 32);
const SymbolMask: u64 = TaggedValueMask | (@as(u64, TagSymbol) << 32);

const TagMask: u32 = (1 << 3) - 1;
const PrimitiveMask: u64 = TaggedValueMask | (@as(u64, TagMask) << 32) | IntegerMask | EnumMask;
const TaggedPrimitiveMask = TaggedValueMask | PrimitiveMask;
const BeforeTagMask: u32 = 0x7fff << 3;

/// The tag id is also the primitive type id.
const TagId = u3;
pub const TagNone: TagId = 0;
pub const TagBoolean: TagId = 1;
pub const TagError: TagId = 2;
// pub const TagEnum: TagId = 5;
pub const TagSymbol: TagId = 6;
pub const TagInteger: TagId = 7;

/// NaN tagging over a f64 value.
/// Represents a f64 value if not a quiet nan.
/// Otherwise, the sign bit represents either a pointer value or a special value (true, false, none, etc).
/// Pointer values can be at most 50 bits since the sign bit (1), quiet nan (12),
/// and one more bit (so that QNANs can also be a number value) take up 13 bits.
pub const Value = packed union {
    val: u64,

    /// Call frame return info.
    // retInfo: packed struct {
    //     numRetVals: u8,
    //     retFlag: u8,

    //     /// Since there are different call insts with varying lengths,
    //     /// the call convention prefers to advance the pc before saving it so
    //     /// stepping over the call will already have the correct pc.
    //     /// An offset is stored to the original call inst for stack unwinding.
    //     callInstOffset: u8,
    // },

    retPcPtr: [*]const cy.Inst,
    retFramePtr: [*]Value,
    // two: packed struct {
    //     low: u32,
    //     high: u32,
    // },

    pub const None = Value{ .val = NoneMask };
    pub const True = Value{ .val = TrueMask };
    pub const False = Value{ .val = FalseMask };

    /// Interrupt value. Represented as an error tag literal with a null tag id.
    /// Returned from native funcs.
    pub const Interrupt = Value{ .val = ErrorMask | (@as(u32, 0xFF) << 8) | std.math.maxInt(u8) };

    pub inline fn retInfoCallInstOffset(self: *const Value) u8 {
        return @intCast((self.val & 0xff0000) >> 16);
    }

    pub inline fn retInfoRetFlag(self: *const Value) u8 {
        return @intCast((self.val & 0xff00) >> 8);
    }

    pub inline fn retInfoNumRet(self: *const Value) u8 {
        return @intCast(self.val & 0xff);
    }

    pub inline fn asInteger(self: *const Value) i48 {
        return @bitCast(@as(u48, @intCast(self.val & 0xffffffffffff)));
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

    pub inline fn asF64(self: *const Value) f64 {
        @setRuntimeSafety(debug);
        return @bitCast(self.val);
    }

    pub inline fn asSymbolId(self: *const Value) u32 {
        return @intCast(self.val & @as(u64, 0xFFFFFFFF));
    }

    pub inline fn asArray(self: *const Value) []const u8 {
        if (cy.Trace) {
            if (!self.isArray()) cy.panic("Not an array.");
        }
        const obj = self.asHeapObject();
        return obj.array.getSlice();
    }

    pub fn asString(val: Value) []const u8 {
        if (cy.Trace) {
            if (!val.isString()) cy.panic("Not a string.");
        }
        const obj = val.asHeapObject();
        return obj.string.getSlice();
    }

    pub inline fn toF64(self: *const Value) f64 {
        @setRuntimeSafety(debug);
        if (self.isFloat()) {
            return self.asF64();
        } else {
            return @call(.never_inline, otherToF64, .{self}) catch cy.fatal();
        }
    }

    pub fn otherToF64(self: *const Value) !f64 {
        if (self.isPointer()) {
            const obj = self.asHeapObject();
            if (obj.getTypeId() == bt.String) {
                const str = obj.string.getSlice();
                return std.fmt.parseFloat(f64, str) catch 0;
            } else {
                log.tracev("unsupported conv to number: {}", .{obj.getTypeId()});
                return error.Unsupported;
            }
        } else {
            switch (self.getTag()) {
                TagNone => return 0,
                TagBoolean => return if (self.asBool()) 1 else 0,
                TagInteger => return @floatFromInt(self.asInteger()),
                else => {
                    log.tracev("unsupported conv to number: {}", .{self.getTag()});
                    return error.Unsupported;
                }
            }
        }
    }

    pub inline fn assumeNotBoolToBool(self: *const Value) bool {
        return !self.isNone();
    }

    pub inline fn toBool(self: *const Value) bool {
        if (self.isBool()) {
            return self.asBool();
        }
        return !self.isNone();
    }

    pub inline fn isArray(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().getTypeId() == bt.Array;
    }

    pub inline fn isString(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().getTypeId() == bt.String;
    }

    pub inline fn bothFloats(a: Value, b: Value) bool {
        return a.isFloat() and b.isFloat();
    }

    pub inline fn bothIntegers(a: Value, b: Value) bool {
        return a.val & b.val & TaggedUpperValueMask == IntegerMask;
    }

    pub inline fn isError(self: *const Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == ErrorMask;
    }

    pub inline fn getPrimitiveTypeId(self: *const Value) u32 {
        if (self.isFloat()) {
            return bt.Float;
        } else {
            return self.getTag();
        }
    }

    pub inline fn getTypeId(self: *const Value) u32 {
        const bits = self.val & TaggedPrimitiveMask;
        if (bits >= TaggedValueMask) {
            if (self.isPointer()) {
                return self.asHeapObject().getTypeId();
            } else {
                if (bits >= TaggedIntegerMask) {
                    return bt.Integer;
                } else if (bits >= TaggedEnumMask) {
                    return @intCast(self.val & 0xffffffff);
                } else {
                    return self.getTag();
                }
            }
        } else {
            return bt.Float;
        }
    }

    pub inline fn isFloatOrPointer(self: *const Value) bool {
        // This could be faster if the 3 bits past the 48 pointer bits represents a non primitive number value.
        return self.isFloat() or self.isPointer();
    }

    pub inline fn isFloat(self: *const Value) bool {
        // Only a number(f64) if not all tagged bits are set.
        return self.val & TaggedValueMask != TaggedValueMask;
    }

    pub inline fn isInteger(self: *const Value) bool {
        return self.val & TaggedUpperValueMask == TaggedIntegerMask;
    }

    pub inline fn isEnum(self: *const Value) bool {
        return self.val & TaggedUpperValueMask == TaggedEnumMask;
    }

    pub inline fn isPointer(self: *const Value) bool {
        return self.val >= vmc.NOCYC_POINTER_MASK;
    }

    pub inline fn isCycPointer(self: *const Value) bool {
        return self.val >= vmc.CYC_POINTER_MASK;
    }

    pub inline fn isObjectType(self: *const Value, typeId: cy.TypeId) bool {
        return isPointer(self) and self.asHeapObject().getTypeId() == typeId;
    }

    pub inline fn isPointerT(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().getTypeId() == bt.Pointer;
    }

    pub inline fn isMap(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().getTypeId() == bt.Map;
    }

    pub inline fn isList(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().getTypeId() == bt.List;
    }

    pub inline fn isBox(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().getTypeId() == bt.Box;
    }

    pub inline fn isClosure(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().getTypeId() == bt.Closure;
    }

    pub fn isGcConfirmedCyc(self: *const Value) bool {
        if (self.isCycPointer()) {
            return self.asHeapObject().isGcConfirmedCyc();
        }
        return false;
    }

    pub inline fn castHostObject(self: *const Value, comptime T: type) T {
        return @ptrFromInt(@as(usize, @intCast(self.val & ~vmc.POINTER_MASK)) + 8);
    }

    pub inline fn asHeapObject(self: *const Value) *cy.HeapObject {
        return @ptrFromInt(@as(usize, @intCast(self.val & ~vmc.POINTER_MASK)));
    }

    pub inline fn castHeapObject(self: *const Value, comptime Ptr: type) Ptr {
        return @ptrFromInt(@as(usize, @intCast(self.val & ~vmc.POINTER_MASK)));
    }

    pub inline fn asAnyOpaque(self: *const Value) ?*anyopaque {
        return @ptrFromInt(@as(usize, @intCast(self.val & ~vmc.POINTER_MASK)));
    }

    pub inline fn asBool(self: *const Value) bool {
        return self.val == TrueMask;
    }

    pub inline fn isNone(self: *const Value) bool {
        return self.val == NoneMask;
    }

    pub inline fn isTrue(self: *const Value) bool {
        return self.val == TrueMask;
    }

    pub inline fn isInterrupt(self: *const Value) bool {
        return self.val == Interrupt.val;
    }

    pub inline fn isBool(self: *const Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == BooleanMask;
    }

    pub inline fn assumeNotPtrIsBool(self: *const Value) bool {
        return self.val & TaggedPrimitiveMask == BooleanMask;
    }

    pub inline fn getTag(self: *const Value) u3 {
        return @intCast(@as(u32, @intCast(self.val >> 32)) & TagMask);
    }

    pub inline fn initEnum(typeId: cy.TypeId, val: u16) Value {
        return .{ .val = TaggedEnumMask | (@as(u64, val) << 32) | typeId };
    }

    pub inline fn getEnumType(self: *const Value) cy.TypeId {
        return @intCast(self.val & 0xffffffff);
    }

    pub inline fn getEnumValue(self: *const Value) u16 {
        return @intCast((self.val >> 32) & 0xff);
    }

    pub inline fn isSymbol(self: *const Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == SymbolMask;
    }

    pub inline fn initSymbol(symId: u32) Value {
        return .{ .val = SymbolMask | symId };
    }

    pub inline fn initF64(val: f64) Value {
        return .{ .val = @as(u64, @bitCast(val)) };
    }

    pub inline fn initInt(val: i48) Value {
        return .{ .val = TaggedIntegerMask | @as(u48, @bitCast(val)) };
    }

    pub inline fn initI32(val: i32) Value {
        return initInt(@intCast(val));
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

    pub inline fn initHostPtr(ptr: ?*anyopaque) Value {
        const obj: *cy.HeapObject = @ptrFromInt(@intFromPtr(ptr) - 8);
        if (obj.isCyclable()) {
            return .{ .val = vmc.CYC_POINTER_MASK | (@intFromPtr(obj) & vmc.POINTER_PAYLOAD_MASK) };
        } else {
            return .{ .val = vmc.NOCYC_POINTER_MASK | (@intFromPtr(obj) & vmc.POINTER_PAYLOAD_MASK) };
        }
    }

    pub inline fn initHostNoCycPtr(ptr: ?*anyopaque) Value {
        return .{ .val = vmc.NOCYC_POINTER_MASK | ((@intFromPtr(ptr) & vmc.POINTER_PAYLOAD_MASK) - 8) };
    }

    pub inline fn initHostCycPtr(ptr: ?*anyopaque) Value {
        return .{ .val = vmc.CYC_POINTER_MASK | ((@intFromPtr(ptr) & vmc.POINTER_PAYLOAD_MASK) - 8) };
    }

    pub inline fn initPtr(ptr: ?*anyopaque) Value {
        const obj: *cy.HeapObject = @ptrCast(@alignCast(ptr));
        if (obj.isCyclable()) {
            return .{ .val = vmc.CYC_POINTER_MASK | (@intFromPtr(obj) & vmc.POINTER_PAYLOAD_MASK) };
        } else {
            return .{ .val = vmc.NOCYC_POINTER_MASK | (@intFromPtr(obj) & vmc.POINTER_PAYLOAD_MASK) };
        }
    }

    pub inline fn initNoCycPtr(ptr: ?*anyopaque) Value {
        return .{ .val = vmc.NOCYC_POINTER_MASK | (@intFromPtr(ptr) & vmc.POINTER_PAYLOAD_MASK) };
    }

    pub inline fn initCycPtr(ptr: ?*anyopaque) Value {
        return .{ .val = vmc.CYC_POINTER_MASK | (@intFromPtr(ptr) & vmc.POINTER_PAYLOAD_MASK) };
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

    pub inline fn initErrorSymbol(id: u32) Value {
        return .{ .val = ErrorMask | id };
    }

    pub inline fn asErrorSymbol(self: *const Value) u32 {
        return @intCast(self.val & 0xffffffff);
    }

    pub fn dump(self: *const Value) void {
        switch (self.getTypeId()) {
            bt.Float => {
                log.tracev("Float {}", .{self.asF64()});
            },
            bt.None => {
                log.tracev("None", .{});
            },
            bt.Integer => {
                log.tracev("Integer {}", .{self.asInteger()});
            },
            bt.Error => {
                log.tracev("Error {}", .{self.asErrorSymbol()});
            },
            bt.Symbol => {
                log.tracev("Symbol {}", .{self.asSymbolId()});
            },
            else => |typeId| {
                if (self.isPointer()) {
                    const obj = self.asHeapObject();
                    switch (obj.getTypeId()) {
                        bt.List => log.tracev("List {*} rc={} len={}", .{obj, obj.head.rc, obj.list.list.len}),
                        bt.Map => log.tracev("Map {*} rc={} size={}", .{obj, obj.head.rc, obj.map.inner.size}),
                        bt.String => {
                            const str = obj.string.getSlice();
                            if (str.len > 20) {
                                log.tracev("String {*} rc={} len={} str=\"{s}\"...", .{obj, obj.head.rc, str.len, str[0..20]});
                            } else {
                                log.tracev("String {*} rc={} len={} str={s}", .{obj, obj.head.rc, str.len, str});
                            }
                        },
                        bt.Lambda => log.tracev("Lambda {*} rc={}", .{obj, obj.head.rc}),
                        bt.Closure => log.tracev("Closure {*} rc={}", .{obj, obj.head.rc}),
                        bt.Fiber => log.tracev("Fiber {*} rc={}", .{obj, obj.head.rc}),
                        bt.HostFunc => return log.tracev("NativeFunc {*} rc={}", .{obj, obj.head.rc}),
                        bt.Pointer => return log.tracev("Pointer {*} rc={} ptr={*}", .{obj, obj.head.rc, obj.pointer.ptr}),
                        else => {
                            log.tracev("HeapObject {*} type={} rc={}", .{obj, obj.getTypeId(), obj.head.rc});
                        },
                    }
                } else {
                    if (self.isEnum()) {
                        log.tracev("Enum {}", .{typeId});
                    } else {
                        log.tracev("Unknown {}", .{self.getTag()});
                    }
                }
            }
        }
    }

    pub fn getUserTag(self: *const Value) ValueUserTag {
        const typeId = self.getTypeId();
        switch (typeId) {
            bt.Float => return .float,
            bt.Boolean => return .bool,
            bt.None => return .none,
            bt.Symbol => return .symbol,
            bt.Error => return .err,
            bt.Integer => return .int,
            else => {} // Fall-through.
        }

        if (!self.isPointer()) {
            if (self.isEnum()) {
                return .enumT;
            } else {
                return .unknown;
            }
        }

        switch (typeId) {
            bt.List => return .list,
            bt.Map => return .map,
            bt.String => return .string,
            bt.Array => return .array,
            bt.Closure => return .closure,
            bt.Lambda => return .lambda,
            bt.Fiber => return .fiber,
            bt.Box => return .box,
            bt.HostFunc => return .nativeFunc,
            bt.TccState => return .tccState,
            bt.Pointer => return .pointer,
            bt.MetaType => return .metatype,
            cy.NullId => return .danglingObject,
            else => {
                return .object;
            },
        }
    }
};

pub const ValueUserTag = enum {
    float,
    int,
    bool,
    object,
    list,
    map,
    string,
    array,
    closure,
    lambda,
    fiber,
    box,
    nativeFunc,
    tccState,
    pointer,
    enumT,
    symbol,
    err,
    metatype,
    danglingObject,
    unknown,
    none,
};

pub fn shallowCopy(vm: *cy.VM, val: Value) Value {
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        switch (obj.getTypeId()) {
            bt.List => {
                const list = cy.ptrAlignCast(*cy.List(Value), &obj.list.list);
                const new = cy.heap.allocList(vm, list.items()) catch cy.fatal();
                for (list.items()) |item| {
                    cy.arc.retain(vm, item);
                }
                return new;
            },
            bt.Map => {
                const new = cy.heap.allocEmptyMap(vm) catch cy.fatal();
                const newMap = cy.ptrAlignCast(*cy.MapInner, &(new.asHeapObject()).map.inner);

                const map = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    cy.arc.retain(vm, entry.key);
                    cy.arc.retain(vm, entry.value);
                    newMap.put(vm.alloc, entry.key, entry.value) catch cy.fatal();
                }
                return new;
            },
            bt.Closure => {
                fmt.panic("Unsupported copy closure.", &.{});
            },
            bt.Lambda => {
                fmt.panic("Unsupported copy closure.", &.{});
            },
            bt.String => {
                cy.arc.retainObject(vm, obj);
                return val;
            },
            bt.Array => {
                cy.arc.retainObject(vm, obj);
                return val;
            },
            bt.Fiber => {
                fmt.panic("Unsupported copy fiber.", &.{});
            },
            bt.Box => {
                fmt.panic("Unsupported copy box.", &.{});
            },
            bt.HostFunc => {
                fmt.panic("Unsupported copy native func.", &.{});
            },
            bt.TccState => {
                fmt.panic("Unsupported copy tcc state.", &.{});
            },
            bt.Pointer => {
                fmt.panic("Unsupported copy pointer.", &.{});
            },
            else => {
                const entry = &@as(*const cy.VM, @ptrCast(vm)).types[obj.getTypeId()];
                if (entry.kind == .object) {
                    const numFields = entry.data.object.numFields;
                    const fields = obj.object.getValuesConstPtr()[0..numFields];
                    var new: Value = undefined;
                    if (numFields <= 4) {
                        new = cy.heap.allocObjectSmall(vm, obj.getTypeId(), fields) catch cy.fatal();
                    } else {
                        new = cy.heap.allocObject(vm, obj.getTypeId(), fields) catch cy.fatal();
                    }
                    for (fields) |field| {
                        cy.arc.retain(vm, field);
                    }
                    return new;
                } else {
                    fmt.panic("Unsupported copy host object.", &.{});
                }
            },
        }
    } else {
        return val;
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
    try t.eq(NoneMask, 0x7FFC000000000000);
    try t.eq(TrueMask, 0x7FFC000100000001);
    try t.eq(FalseMask, 0x7FFC000100000000);
    try t.eq(vmc.POINTER_MASK, 0xFFFE000000000000);
    try t.eq(Value.initInt(0).val, 0x7ffe000000000000);

    // Check Zig/C struct compat.
    try t.eq(@sizeOf(Value), @sizeOf(vmc.Value));
    // const retInfoT = std.meta.fieldInfo(Value, .retInfo).type;
    // try t.eq(@offsetOf(retInfoT, "numRetVals"), 0);
    // try t.eq(@offsetOf(retInfoT, "retFlag"), 1);
}