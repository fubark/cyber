const std = @import("std");
const builtin = @import("builtin");
const endian = builtin.target.cpu.arch.endian();
const stdx = @import("stdx");
const t = stdx.testing;
const debug = builtin.mode == .Debug;
const log = stdx.log.scoped(.value);
const cy = @import("cyber.zig");
const fmt = @import("fmt.zig");

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
const UserTagMask: u64 = TaggedValueMask | (@as(u64, TagUserTag) << 32);
const UserTagLiteralMask: u64 = TaggedValueMask | (@as(u64, TagUserTagLiteral) << 32);
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
pub const TagUserTag: TagId = 5;
pub const TagUserTagLiteral: TagId = 6;
pub const TagInteger: TagId = 7;
pub const NoneT: u32 = TagNone;
pub const BooleanT: u32 = TagBoolean;
pub const ErrorT: u32 = TagError;
pub const StaticAstringT: u32 = TagStaticAstring; // ASCII string.
pub const StaticUstringT: u32 = TagStaticUstring; // UTF8 string.
pub const UserTagT: u32 = TagUserTag;
pub const UserTagLiteralT: u32 = TagUserTagLiteral;
pub const IntegerT: u32 = TagInteger;
pub const NumberT: u32 = 8;

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
        retFlag: bool,
    },
    retPcPtr: [*]const cy.OpData,
    retFramePtr: [*]Value,
    // two: packed struct {
    //     low: u32,
    //     high: u32,
    // },

    pub const None = Value{ .val = NoneMask };
    pub const True = Value{ .val = TrueMask };
    pub const False = Value{ .val = FalseMask };

    /// Panic value. Represented as an error tag literal with a null tag id.
    /// Can be returned from native functions.
    pub const Panic = Value{ .val = ErrorMask | (@as(u32, 0xFF) << 8) | std.math.maxInt(u8) };

    pub inline fn asInteger(self: *const Value) i32 {
        return @bitCast(i32, @intCast(u32, self.val & 0xffffffff));
    }

    pub inline fn asF64toI32(self: *const Value) i32 {
        return @floatToInt(i32, self.asF64());
    }

    pub inline fn asF64toU32(self: *const Value) u32 {
        @setRuntimeSafety(debug);
        return @floatToInt(u32, self.asF64());
    }

    pub inline fn asF64(self: *const Value) f64 {
        @setRuntimeSafety(debug);
        return @bitCast(f64, self.val);
    }

    pub inline fn asTagLiteralId(self: *const Value) u32 {
        return @intCast(u32, self.val & @as(u64, 0xFFFFFFFF));
    }

    pub inline fn toF64(self: *const Value) linksection(cy.HotSection) f64 {
        @setRuntimeSafety(debug);
        if (self.isNumber()) {
            return self.asF64();
        } else {
            return @call(.never_inline, otherToF64, .{self});
        }
    }

    fn otherToF64(self: *const Value) linksection(cy.HotSection) f64 {
        if (self.isPointer()) {
            const obj = self.asHeapObject();
            if (obj.common.structId == cy.AstringT) {
                const str = obj.astring.getConstSlice();
                return std.fmt.parseFloat(f64, str) catch 0;
            } else if (obj.common.structId == cy.UstringT) {
                const str = obj.ustring.getConstSlice();
                return std.fmt.parseFloat(f64, str) catch 0;
            } else stdx.panicFmt("unexpected struct {}", .{obj.common.structId});
        } else {
            switch (self.getTag()) {
                TagNone => return 0,
                TagBoolean => return if (self.asBool()) 1 else 0,
                TagInteger => return @intToFloat(f64, self.asInteger()),
                else => stdx.panicFmt("unexpected tag {}", .{self.getTag()}),
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

    pub inline fn isMap(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().common.structId == cy.MapS;
    }

    pub inline fn isList(self: *const Value) bool {
        return self.isPointer() and self.asHeapObject().common.structId == cy.ListS;
    }

    pub inline fn isRawString(self: *const Value) bool {
        if (!self.isPointer()) {
            return false;
        }
        const typeId = self.asHeapObject().common.structId;
        return typeId == cy.RawStringT or typeId == cy.RawStringSliceT;
    }

    pub fn isString(self: *const Value) linksection(cy.HotSection) bool {
        if (self.isPointer()) {
            const obj = self.asHeapObject();
            return obj.common.structId == cy.AstringT or obj.common.structId == cy.UstringT;
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

    pub inline fn assumeNotPtrIsTagLiteral(self: *const Value) bool {
        return self.val & TaggedPrimitiveMask == UserTagLiteralMask;
    }

    pub inline fn getPrimitiveTypeId(self: *const Value) u32 {
        if (self.isNumber()) {
            return NumberT;
        } else {
            return self.getTag();
        }
    }

    pub inline fn getTypeId(self: *const Value) u32 {
        if (self.isPointer()) {
            return self.asHeapObject().common.structId;
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

    pub inline fn isObjectType(self: *const Value, typeId: cy.TypeId) bool {
        if (isPointer(self)) {
            return self.asHeapObject().common.structId == typeId;
        }
        return false;
    }

    pub inline fn asRawString(self: *const Value) []const u8 {
        const obj = self.asHeapObject();
        if (obj.common.structId == cy.RawStringT) {
            return obj.rawstring.getConstSlice();
        } else if (obj.common.structId == cy.RawStringSliceT) {
            return obj.rawstringSlice.getConstSlice();
        } else unreachable;
    }

    pub inline fn asHeapObject(self: *const Value) *cy.HeapObject {
        return @intToPtr(*cy.HeapObject, @intCast(usize, self.val & ~PointerMask));
    }

    pub inline fn asPointer(self: *const Value, comptime Ptr: type) Ptr {
        return @intToPtr(Ptr, @intCast(usize, self.val & ~PointerMask));
    }

    pub inline fn asAnyOpaque(self: *const Value) ?*anyopaque {
        return @intToPtr(?*anyopaque, @intCast(usize, self.val & ~PointerMask));
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

    pub inline fn isPanic(self: *const Value) bool {
        return self.val == Panic.val;
    }

    pub inline fn isBool(self: *const Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == BooleanMask;
    }

    pub inline fn assumeNotPtrIsBool(self: *const Value) bool {
        return self.val & TaggedPrimitiveMask == BooleanMask;
    }

    pub inline fn getTag(self: *const Value) u3 {
        return @intCast(u3, @intCast(u32, self.val >> 32) & TagMask);
    }

    pub inline fn initTag(tag: u8, val: u8) Value {
        return .{ .val = UserTagMask | (@as(u32, tag) << 8) | val };
    }

    pub inline fn isTagLiteral(self: *const Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == UserTagLiteralMask;
    }

    pub inline fn initTagLiteral(symId: u8) Value {
        return .{ .val = UserTagLiteralMask | symId };
    }

    pub inline fn initF64(val: f64) Value {
        return .{ .val = @bitCast(u64, val) };
    }

    pub inline fn initI32(val: i32) Value {
        return .{ .val = IntegerMask | @bitCast(u32, val) };
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
        return .{ .val = PointerMask | @ptrToInt(ptr) };
    }

    pub inline fn initStaticAstring(start: u32, len: u15) Value {
        return .{ .val = StaticAstringMask | (@as(u64, len) << 35) | start };
    }

    pub inline fn initStaticUstring(start: u32, len: u15) Value {
        return .{ .val = StaticUstringMask | (@as(u64, len) << 35) | start };
    }

    pub inline fn asStaticStringSlice(self: *const Value) stdx.IndexSlice(u32) {
        const len = (@intCast(u32, self.val >> 32) & BeforeTagMask) >> 3;
        const start = @intCast(u32, self.val & 0xffffffff);
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

    pub inline fn initErrorTagLit(id: u8) Value {
        return .{ .val = ErrorMask | (@as(u32, 0xFF) << 8) | id };
    }

    pub inline fn asErrorTagLit(self: *const Value) u8 {
        return @intCast(u8, self.val & 0xff);
    }

    pub fn dump(self: *const Value) void {
        if (self.isNumber()) {
            log.info("Number {}", .{self.asF64()});
        } else {
            if (self.isPointer()) {
                const obj = self.asHeapObject();
                switch (obj.common.structId) {
                    cy.ListS => log.info("List {*} len={}", .{obj, obj.list.list.len}),
                    cy.MapS => log.info("Map {*} size={}", .{obj, obj.map.inner.size}),
                    cy.AstringT => {
                        const str = obj.astring.getConstSlice();
                        if (str.len > 20) {
                            log.info("String {*} len={} str=\"{s}\"...", .{obj, str.len, str[0..20]});
                        } else {
                            log.info("String {*} len={} str={s}", .{obj, str.len, str});
                        }
                    },
                    cy.UstringT => {
                        const str = obj.ustring.getConstSlice();
                        if (str.len > 20) {
                            log.info("String {*} len={} str=\"{s}\"...", .{obj, str.len, str[0..20]});
                        } else {
                            log.info("String {*} len={} str={s}", .{obj, str.len, str});
                        }
                    },
                    cy.LambdaS => log.info("Lambda {*}", .{obj}),
                    cy.ClosureS => log.info("Closure {*}", .{obj}),
                    cy.FiberS => log.info("Fiber {*}", .{obj}),
                    cy.NativeFunc1S => return log.info("NativeFunc {*}", .{obj}),
                    else => {
                        log.info("HeapObject {*} {}", .{obj, obj.common.structId});
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
                    TagStaticUstring,
                    TagStaticAstring => {
                        const slice = self.asStaticStringSlice();
                        // log.info("Const String {*} len={} str=\"{s}\"", .{&gvm.strBuf[slice.start], slice.len(), gvm.strBuf[slice.start..slice.end]});
                        log.info("Const String len={}", .{slice.len()});
                    },
                    else => {
                        log.info("Unknown {}", .{self.val});
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
                switch (obj.common.structId) {
                    cy.ListS => return .list,
                    cy.MapS => return .map,
                    cy.AstringT => return .string,
                    cy.UstringT => return .string,
                    cy.StringSliceT => return .string,
                    cy.RawStringT => return .rawstring,
                    cy.RawStringSliceT => return .rawstring,
                    cy.ClosureS => return .closure,
                    cy.LambdaS => return .lambda,
                    cy.FiberS => return .fiber,
                    cy.BoxS => return .box,
                    cy.NativeFunc1S => return .nativeFunc,
                    cy.TccStateS => return .tccState,
                    cy.OpaquePtrS => return .opaquePtr,
                    cy.FileT => return .file,
                    cy.DirT => return .dir,
                    cy.DirIteratorT => return .dirIter,
                    cy.SymbolT => return .symbol,
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
                    TagUserTag => return .tag,
                    TagUserTagLiteral => return .tagLiteral,
                    TagError => return .errorVal,
                    TagInteger => return .int,
                    // else => unreachable,
                }
            }
        }
    }
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
    opaquePtr,
    tag,
    tagLiteral,
    errorVal,
    file,
    dir,
    dirIter,
    symbol,
    none,
};

pub fn shallowCopy(vm: *cy.VM, val: Value) linksection(cy.StdSection) Value {
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        switch (obj.common.structId) {
            cy.ListS => {
                const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
                const new = cy.heap.allocList(vm, list.items()) catch stdx.fatal();
                for (list.items()) |item| {
                    cy.arc.retain(vm, item);
                }
                return new;
            },
            cy.MapS => {
                const new = cy.heap.allocEmptyMap(vm) catch stdx.fatal();
                const newMap = stdx.ptrAlignCast(*cy.MapInner, &(new.asHeapObject()).map.inner);

                const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    cy.arc.retain(vm, entry.key);
                    cy.arc.retain(vm, entry.value);
                    newMap.put(vm.alloc, @ptrCast(*const cy.VM, vm), entry.key, entry.value) catch stdx.fatal();
                }
                return new;
            },
            cy.ClosureS => {
                fmt.panic("Unsupported copy closure.", &.{});
            },
            cy.LambdaS => {
                fmt.panic("Unsupported copy closure.", &.{});
            },
            cy.AstringT => {
                cy.arc.retainObject(vm, obj);
                return val;
            },
            cy.UstringT => {
                cy.arc.retainObject(vm, obj);
                return val;
            },
            cy.RawStringT => {
                cy.arc.retainObject(vm, obj);
                return val;
            },
            cy.FiberS => {
                fmt.panic("Unsupported copy fiber.", &.{});
            },
            cy.BoxS => {
                fmt.panic("Unsupported copy box.", &.{});
            },
            cy.NativeFunc1S => {
                fmt.panic("Unsupported copy native func.", &.{});
            },
            cy.TccStateS => {
                fmt.panic("Unsupported copy tcc state.", &.{});
            },
            cy.OpaquePtrS => {
                fmt.panic("Unsupported copy opaque ptr.", &.{});
            },
            else => {
                const numFields = @ptrCast(*const cy.VM, vm).structs.buf[obj.common.structId].numFields;
                const fields = obj.object.getValuesConstPtr()[0..numFields];
                var new: Value = undefined;
                if (numFields <= 4) {
                    new = cy.heap.allocObjectSmall(vm, obj.common.structId, fields) catch stdx.fatal();
                } else {
                    new = cy.heap.allocObject(vm, obj.common.structId, fields) catch stdx.fatal();
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
    try t.eq(val.asF64(), std.math.inf_f64);

    // -Inf.
    val = Value{ .val = 0xfff0000000000000 };
    try t.eq(val.asF64(), -std.math.inf_f64);
}

test "Internals." {
    try t.eq(@sizeOf(Value), 8);
    try t.eq(@alignOf(Value), 8);
    try t.eq(StaticAstringMask, 0x7FFC000300000000);
    try t.eq(StaticUstringMask, 0x7FFC000400000000);
    try t.eq(NoneMask, 0x7FFC000000000000);
    try t.eq(TrueMask, 0x7FFC000100000001);
    try t.eq(PointerMask, 0xFFFC000000000000);
}