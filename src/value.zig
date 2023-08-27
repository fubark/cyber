const std = @import("std");
const builtin = @import("builtin");
const endian = builtin.target.cpu.arch.endian();
const stdx = @import("stdx");
const t = stdx.testing;
const debug = builtin.mode == .Debug;
const log = stdx.log.scoped(.value);
const cy = @import("cyber.zig");
const rt = cy.rt;
const fmt = @import("fmt.zig");
const vmc = @import("vm_c.zig");

/// Most significant bit.
const SignMask: u64 = 1 << 63;

/// QNAN and one extra bit to the right.
const TaggedValueMask: u64 = 0x7ffc000000000000;

/// TaggedMask + Sign bit indicates a pointer value.
const PointerMask: u64 = TaggedValueMask | SignMask;

const BooleanMask: u64 = TaggedValueMask | (@as(u64, TagBoolean) << 32);
const FalseMask: u64 = BooleanMask;
const TrueMask: u64 = BooleanMask | TrueBitMask;
const TrueBitMask: u64 = 1;
const NoneMask: u64 = TaggedValueMask | (@as(u64, TagNone) << 32);
const ErrorMask: u64 = TaggedValueMask | (@as(u64, TagError) << 32);
const StaticAstringMask: u64 = TaggedValueMask | (@as(u64, TagStaticAstring) << 32);
const StaticUstringMask: u64 = TaggedValueMask | (@as(u64, TagStaticUstring) << 32);
const EnumMask: u64 = TaggedValueMask | (@as(u64, TagEnum) << 32);
const SymbolMask: u64 = TaggedValueMask | (@as(u64, TagSymbol) << 32);
const IntegerMask: u64 = TaggedValueMask | (@as(u64, TagInteger) << 32);

const TagMask: u32 = (1 << 3) - 1;
const TaggedPrimitiveMask = TaggedValueMask | (@as(u64, TagMask) << 32);
const BeforeTagMask: u32 = 0x7fff << 3;

/// The tag id is also the primitive type id.
const TagId = u3;
pub const TagNone: TagId = 0;
pub const TagBoolean: TagId = 1;
pub const TagError: TagId = 2;
pub const TagStaticAstring: TagId = 3;
pub const TagStaticUstring: TagId = 4;
pub const TagEnum: TagId = 5;
pub const TagSymbol: TagId = 6;
pub const TagInteger: TagId = 7;

pub const StaticUstringHeader = struct {
    charLen: u32,
    mruIdx: u32,
    mruCharIdx: u32,
};

pub const ValuePair = struct {
    left: Value,
    right: Value,
};

/// NaN tagging over a f64 value.
/// Represents a f64 value if not a quiet nan.
/// Otherwise, the sign bit represents either a pointer value or a special value (true, false, none, etc).
/// Pointer values can be at most 50 bits since the sign bit (1), quiet nan (12),
/// and one more bit (so that QNANs can also be a number value) take up 13 bits.
pub const Value = packed union {
    val: u64,

    /// Call frame return info.
    retInfo: packed struct {
        numRetVals: u8,
        retFlag: u8,

        /// Since there are different call insts with varying lengths,
        /// the call convention prefers to advance the pc before saving it so
        /// stepping over the call will already have the correct pc.
        /// An offset is stored to the original call inst for stack unwinding.
        callInstOffset: u8,
    },
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

    pub inline fn asInteger(self: *const Value) i32 {
        return @bitCast(@as(u32, @intCast(self.val & 0xffffffff)));
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

    pub inline fn toF64(self: *const Value) linksection(cy.HotSection) f64 {
        @setRuntimeSafety(debug);
        if (self.isNumber()) {
            return self.asF64();
        } else {
            return @call(.never_inline, otherToF64, .{self}) catch stdx.fatal();
        }
    }

    pub fn otherToF64(self: *const Value) linksection(cy.HotSection) !f64 {
        if (self.isPointer()) {
            const obj = self.asHeapObject();
            if (obj.head.typeId == rt.AstringT) {
                const str = obj.astring.getConstSlice();
                return std.fmt.parseFloat(f64, str) catch 0;
            } else if (obj.head.typeId == rt.UstringT) {
                const str = obj.ustring.getConstSlice();
                return std.fmt.parseFloat(f64, str) catch 0;
            } else if (obj.head.typeId == rt.StringSliceT) {
                const str = obj.stringSlice.getConstSlice();
                return std.fmt.parseFloat(f64, str) catch 0;
            } else {
                log.debug("unsupported conv to number: {}", .{obj.head.typeId});
                return error.Unsupported;
            }
        } else {
            switch (self.getTag()) {
                TagNone => return 0,
                TagBoolean => return if (self.asBool()) 1 else 0,
                TagInteger => return @floatFromInt(self.asInteger()),
                else => {
                    log.debug("unsupported conv to number: {}", .{self.getTag()});
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

    pub inline fn isRawString(self: *const Value) bool {
        if (!self.isPointer()) {
            return false;
        }
        const typeId = self.asHeapObject().head.typeId;
        return typeId == rt.RawstringT or typeId == rt.RawstringSliceT;
    }

    pub fn isString(self: *const Value) linksection(cy.HotSection) bool {
        if (self.isPointer()) {
            const obj = self.asHeapObject();
            return obj.head.typeId == rt.AstringT or obj.head.typeId == rt.UstringT or obj.head.typeId == rt.StringSliceT;
        } else {
            return self.assumeNotPtrIsStaticString();
        }
    }

    pub inline fn bothNumbers(a: Value, b: Value) bool {
        return a.isNumber() and b.isNumber();
    }

    pub inline fn isStaticString(self: *const Value) bool {
        const mask = self.val & (TaggedPrimitiveMask | SignMask);
        return mask == StaticAstringMask or mask == StaticUstringMask;
    }

    pub inline fn isStaticAstring(self: *const Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == StaticAstringMask;
    }

    pub inline fn isStaticUstring(self: *const Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == StaticUstringMask;
    }

    pub inline fn assumeNotPtrIsStaticString(self: *const Value) bool {
        const mask = self.val & TaggedPrimitiveMask;
        return mask == StaticAstringMask or mask == StaticUstringMask;
    }

    pub inline fn isError(self: *const Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == ErrorMask;
    }

    pub inline fn assumeNotPtrIsError(self: *const Value) bool {
        return self.val & TaggedPrimitiveMask == ErrorMask;
    }

    pub inline fn assumeNotPtrIsSymbol(self: *const Value) bool {
        return self.val & TaggedPrimitiveMask == SymbolMask;
    }

    pub inline fn assumeNotPtrIsEnum(self: *const Value) bool {
        return self.val & TaggedPrimitiveMask == EnumMask;
    }

    pub inline fn getPrimitiveTypeId(self: *const Value) u32 {
        if (self.isNumber()) {
            return rt.NumberT;
        } else {
            return self.getTag();
        }
    }

    pub inline fn getTypeId(self: *const Value) u32 {
        if (self.isPointer()) {
            return self.asHeapObject().head.typeId;
        } else {
            return self.getPrimitiveTypeId();
        }
    }

    pub inline fn isNumberOrPointer(self: *const Value) bool {
        // This could be faster if the 3 bits past the 48 pointer bits represents a non primitive number value.
        return self.isNumber() or self.isPointer();
    }

    pub inline fn isNumber(self: *const Value) bool {
        // Only a number(f64) if not all tagged bits are set.
        return self.val & TaggedValueMask != TaggedValueMask;
    }

    pub inline fn isInteger(self: *const Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == IntegerMask;
    }

    pub inline fn isPointer(self: *const Value) bool {
        // Only a pointer if nan bits and sign bit are set.
        return self.val & PointerMask == PointerMask;
    }

    pub inline fn isObjectType(self: *const Value, typeId: rt.TypeId) bool {
        return isPointer(self) and self.asHeapObject().head.typeId == typeId;
    }

    pub inline fn isPointerT(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().head.typeId == rt.PointerT;
    }

    pub inline fn isMap(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().head.typeId == rt.MapT;
    }

    pub inline fn isList(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().head.typeId == rt.ListT;
    }

    pub inline fn isBox(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().head.typeId == rt.BoxT;
    }

    pub inline fn isClosure(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().head.typeId == rt.ClosureT;
    }

    pub inline fn asRawString(self: *const Value) []const u8 {
        const obj = self.asHeapObject();
        if (obj.head.typeId == rt.RawstringT) {
            return obj.rawstring.getConstSlice();
        } else if (obj.head.typeId == rt.RawstringSliceT) {
            return obj.rawstringSlice.getConstSlice();
        } else unreachable;
    }

    pub inline fn asHeapObject(self: *const Value) *cy.HeapObject {
        return @ptrFromInt(@as(usize, @intCast(self.val & ~PointerMask)));
    }

    pub inline fn asPointer(self: *const Value, comptime Ptr: type) Ptr {
        return @ptrFromInt(@as(usize, @intCast(self.val & ~PointerMask)));
    }

    pub inline fn asAnyOpaque(self: *const Value) ?*anyopaque {
        return @ptrFromInt(@as(usize, @intCast(self.val & ~PointerMask)));
    }

    pub inline fn asBool(self: *const Value) linksection(cy.HotSection) bool {
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

    pub inline fn initEnum(tag: u8, val: u8) Value {
        return .{ .val = EnumMask | (@as(u32, tag) << 8) | val };
    }

    pub inline fn asEnum(self: *const Value) EnumValue {
        return .{
            .enumId = @intCast((0xff00 & self.val) >> 8),
            .memberId = @intCast(0xff & self.val),
        };
    }

    pub inline fn isSymbol(self: *const Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == SymbolMask;
    }

    pub inline fn initSymbol(symId: u8) Value {
        return .{ .val = SymbolMask | symId };
    }

    pub inline fn initF64(val: f64) Value {
        return .{ .val = @as(u64, @bitCast(val)) };
    }

    pub inline fn initI32(val: i32) Value {
        return .{ .val = IntegerMask | @as(u32, @bitCast(val)) };
    }

    pub inline fn initRaw(val: u64) Value {
        @setRuntimeSafety(debug);
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
        @setRuntimeSafety(debug);
        return .{ .val = PointerMask | @intFromPtr(ptr) };
    }

    pub inline fn initStaticAstring(start: u32, len: u15) Value {
        return .{ .val = StaticAstringMask | (@as(u64, len) << 35) | start };
    }

    pub inline fn initStaticUstring(start: u32, len: u15) Value {
        return .{ .val = StaticUstringMask | (@as(u64, len) << 35) | start };
    }

    pub inline fn asStaticStringSlice(self: *const Value) stdx.IndexSlice(u32) {
        const len = (@as(u32, @intCast(self.val >> 32)) & BeforeTagMask) >> 3;
        const start: u32 = @intCast(self.val & 0xffffffff);
        return stdx.IndexSlice(u32).init(start, start + len);
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

    pub inline fn initErrorSymbol(id: u8) Value {
        return .{ .val = ErrorMask | (@as(u32, 0xFF) << 8) | id };
    }

    pub inline fn initErrorEnum(enumId: u8, id: u8) Value {
        return .{ .val = ErrorMask | (@as(u32, enumId) << 8) | id };
    }

    pub inline fn asErrorSymbol(self: *const Value) u8 {
        return @intCast(self.val & 0xff);
    }

    pub fn dump(self: *const Value) void {
        if (self.isNumber()) {
            log.info("Number {}", .{self.asF64()});
        } else {
            if (self.isPointer()) {
                const obj = self.asHeapObject();
                switch (obj.head.typeId) {
                    rt.ListT => log.info("List {*} len={}", .{obj, obj.list.list.len}),
                    rt.MapT => log.info("Map {*} size={}", .{obj, obj.map.inner.size}),
                    rt.AstringT => {
                        const str = obj.astring.getConstSlice();
                        if (str.len > 20) {
                            log.info("String {*} len={} str=\"{s}\"...", .{obj, str.len, str[0..20]});
                        } else {
                            log.info("String {*} len={} str={s}", .{obj, str.len, str});
                        }
                    },
                    rt.UstringT => {
                        const str = obj.ustring.getConstSlice();
                        if (str.len > 20) {
                            log.info("String {*} len={} str=\"{s}\"...", .{obj, str.len, str[0..20]});
                        } else {
                            log.info("String {*} len={} str={s}", .{obj, str.len, str});
                        }
                    },
                    rt.LambdaT => log.info("Lambda {*}", .{obj}),
                    rt.ClosureT => log.info("Closure {*}", .{obj}),
                    rt.FiberT => log.info("Fiber {*}", .{obj}),
                    rt.NativeFuncT => return log.info("NativeFunc {*}", .{obj}),
                    else => {
                        log.info("HeapObject {*} {}", .{obj, obj.head.typeId});
                    },
                }
            } else {
                switch (self.getTag()) {
                    TagNone => {
                        log.info("None", .{});
                    },
                    TagInteger => {
                        log.info("Integer {}", .{self.asInteger()});
                    },
                    TagEnum => {
                        const enumv = self.asEnum();
                        log.info("Enum {} {}", .{enumv.enumId, enumv.memberId});
                    },
                    TagError => {
                        log.info("Error {}", .{self.asErrorSymbol()});
                    },
                    TagSymbol => {
                        log.info("Symbol {}", .{self.asSymbolId()});
                    },
                    TagStaticUstring,
                    TagStaticAstring => {
                        const slice = self.asStaticStringSlice();
                        log.info("Const String len={}", .{slice.len()});
                    },
                    else => {
                        log.info("Unknown {}", .{self.getTag()});
                    },
                }
            }
        }
    }

    pub fn getUserTag(self: *const Value) linksection(cy.Section) ValueUserTag {
        if (self.isNumber()) {
            return .number;
        } else {
            if (self.isPointer()) {
                const obj = self.asHeapObject();
                switch (obj.head.typeId) {
                    rt.ListT => return .list,
                    rt.MapT => return .map,
                    rt.AstringT => return .string,
                    rt.UstringT => return .string,
                    rt.StringSliceT => return .string,
                    rt.RawstringT => return .rawstring,
                    rt.RawstringSliceT => return .rawstring,
                    rt.ClosureT => return .closure,
                    rt.LambdaT => return .lambda,
                    rt.FiberT => return .fiber,
                    rt.BoxT => return .box,
                    rt.NativeFuncT => return .nativeFunc,
                    rt.TccStateT => return .tccState,
                    rt.PointerT => return .pointer,
                    rt.FileT => return .file,
                    rt.DirT => return .dir,
                    rt.DirIteratorT => return .dirIter,
                    rt.MetaTypeT => return .metatype,
                    else => {
                        return .object;
                    },
                }
            } else {
                switch (self.getTag()) {
                    TagBoolean => return .boolean,
                    TagStaticAstring => return .string,
                    TagStaticUstring => return .string,
                    TagNone => return .none,
                    TagEnum => return .enumT,
                    TagSymbol => return .symbol,
                    TagError => return .err,
                    TagInteger => return .int,
                    // else => unreachable,
                }
            }
        }
    }
};

const EnumValue = struct {
    enumId: u16,
    memberId: u16,
};

pub const ValueUserTag = enum {
    number,
    int,
    boolean,
    object,
    list,
    map,
    string,
    rawstring,
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
    file,
    dir,
    dirIter,
    metatype,
    none,
};

pub fn shallowCopy(vm: *cy.VM, val: Value) linksection(cy.StdSection) Value {
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        switch (obj.head.typeId) {
            rt.ListT => {
                const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                const new = cy.heap.allocList(vm, list.items()) catch stdx.fatal();
                for (list.items()) |item| {
                    cy.arc.retain(vm, item);
                }
                return new;
            },
            rt.MapT => {
                const new = cy.heap.allocEmptyMap(vm) catch stdx.fatal();
                const newMap = stdx.ptrAlignCast(*cy.MapInner, &(new.asHeapObject()).map.inner);

                const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    cy.arc.retain(vm, entry.key);
                    cy.arc.retain(vm, entry.value);
                    newMap.put(vm.alloc, @ptrCast(vm), entry.key, entry.value) catch stdx.fatal();
                }
                return new;
            },
            rt.ClosureT => {
                fmt.panic("Unsupported copy closure.", &.{});
            },
            rt.LambdaT => {
                fmt.panic("Unsupported copy closure.", &.{});
            },
            rt.AstringT => {
                cy.arc.retainObject(vm, obj);
                return val;
            },
            rt.UstringT => {
                cy.arc.retainObject(vm, obj);
                return val;
            },
            rt.RawstringT => {
                cy.arc.retainObject(vm, obj);
                return val;
            },
            rt.FiberT => {
                fmt.panic("Unsupported copy fiber.", &.{});
            },
            rt.BoxT => {
                fmt.panic("Unsupported copy box.", &.{});
            },
            rt.NativeFuncT => {
                fmt.panic("Unsupported copy native func.", &.{});
            },
            rt.TccStateT => {
                fmt.panic("Unsupported copy tcc state.", &.{});
            },
            rt.PointerT => {
                fmt.panic("Unsupported copy pointer.", &.{});
            },
            else => {
                const numFields = @as(*const cy.VM, @ptrCast(vm)).types.buf[obj.head.typeId].numFields;
                const fields = obj.object.getValuesConstPtr()[0..numFields];
                var new: Value = undefined;
                if (numFields <= 4) {
                    new = cy.heap.allocObjectSmall(vm, obj.head.typeId, fields) catch stdx.fatal();
                } else {
                    new = cy.heap.allocObject(vm, obj.head.typeId, fields) catch stdx.fatal();
                }
                for (fields) |field| {
                    cy.arc.retain(vm, field);
                }
                return new;
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

test "Internals." {
    try t.eq(@sizeOf(Value), 8);
    try t.eq(@alignOf(Value), 8);
    try t.eq(StaticAstringMask, 0x7FFC000300000000);
    try t.eq(StaticUstringMask, 0x7FFC000400000000);
    try t.eq(NoneMask, 0x7FFC000000000000);
    try t.eq(TrueMask, 0x7FFC000100000001);
    try t.eq(PointerMask, 0xFFFC000000000000);

    // Check Zig/C struct compat.
    try t.eq(@sizeOf(Value), @sizeOf(vmc.Value));
    const retInfoT = std.meta.fieldInfo(Value, .retInfo).type;
    try t.eq(@offsetOf(retInfoT, "numRetVals"), 0);
    try t.eq(@offsetOf(retInfoT, "retFlag"), 1);
}