const std = @import("std");
const builtin = @import("builtin");
const endian = builtin.target.cpu.arch.endian();
const stdx = @import("stdx");
const t = stdx.testing;
const debug = builtin.mode == .Debug;
const log = stdx.log.scoped(.value);
const cy = @import("cyber.zig");

/// Most significant bit.
const SignMask: u64 = 1 << 63;

/// QNAN and one extra bit to the right.
const TaggedValueMask: u64 = 0x7ffc000000000000;

/// TaggedMask + Sign bit indicates a pointer value.
const PointerMask: u64 = TaggedValueMask | SignMask;

const BooleanMask: u64 = TaggedValueMask | (TagBoolean << 32);
const FalseMask: u64 = BooleanMask;
const TrueMask: u64 = BooleanMask | TrueBitMask;
const TrueBitMask: u64 = 1;
const NoneMask: u64 = TaggedValueMask | (TagNone << 32);
const ErrorMask: u64 = TaggedValueMask | (TagError << 32);
const ConstStringMask: u64 = TaggedValueMask | (TagConstString << 32);

const TagMask: u32 = (1 << 3) - 1;
const BeforeTagMask: u32 = 0x7fff << 3;
pub const TagNone = 0;
pub const TagBoolean = 1;
pub const TagError = 2;
pub const TagConstString = 3;

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
        pc: u32,
        framePtr: u29,
        numRetVals: u2,
        retFlag: u1,
    },
    // two: packed struct {
    //     low: u32,
    //     high: u32,
    // },

    pub const None = Value.initNone();
    pub const True = Value.initTrue();
    pub const False = Value.initFalse();

    pub inline fn asI32(self: *const Value) i32 {
        @setRuntimeSafety(debug);
        return @floatToInt(i32, self.asF64());
    }

    pub inline fn asU32(self: *const Value) u32 {
        @setRuntimeSafety(debug);
        return @floatToInt(u32, self.asF64());
    }

    pub inline fn asF64(self: *const Value) linksection(".eval") f64 {
        @setRuntimeSafety(debug);
        return @bitCast(f64, self.val);
    }

    pub inline fn asError(self: *const Value) u32 {
        if (endian == .Little) {
            return self.two[0];
        } else {
            return self.two[1];
        } 
    }

    pub inline fn toF64(self: *const Value) linksection(".eval") f64 {
        @setRuntimeSafety(debug);
        if (self.isNumber()) {
            return self.asF64();
        } else {
            return @call(.{ .modifier = .never_inline }, otherToF64, .{self});
        }
    }

    fn otherToF64(self: *const Value) linksection(".eval") f64 {
        if (self.isPointer()) {
            const obj = stdx.ptrCastAlign(*cy.HeapObject, self.asPointer().?);
            if (obj.common.structId == cy.StringS) {
                const str = obj.string.ptr[0..obj.string.len];
                return std.fmt.parseFloat(f64, str) catch 0;
            } else stdx.panicFmt("unexpected struct {}", .{obj.common.structId});
        } else {
            switch (self.getTag()) {
                TagNone => return 0,
                TagBoolean => return if (self.asBool()) 1 else 0,
                else => stdx.panicFmt("unexpected tag {}", .{self.getTag()}),
            }
        }
    }

    pub fn toBool(self: *const Value) linksection(".eval") bool {
        @setRuntimeSafety(debug);
        if (self.isNumber()) {
            return self.asF64() != 0;
        } else {
            switch (self.getTag()) {
                TagNone => return false,
                TagBoolean => return self.asBool(),
                else => {
                    log.debug("tag {}", .{self.getTag()});
                    stdx.panic("unexpected tag");
                },
            }
        }
    }

    pub fn isString(self: *const Value) linksection(".eval") bool {
        @setRuntimeSafety(debug);
        if (self.isPointer()) {
            const obj = stdx.ptrCastAlign(*cy.HeapObject, self.asPointer().?);
            return obj.common.structId == cy.StringS;
        } else {
            return self.getTag() == TagConstString;
        }
    }

    pub inline fn isNumber(self: *const Value) linksection(".eval") bool {
        @setRuntimeSafety(debug);
        // Only a number(f64) if not all tagged bits are set.
        return self.val & TaggedValueMask != TaggedValueMask;
    }

    pub inline fn isPointer(self: *const Value) linksection(".eval") bool {
        @setRuntimeSafety(debug);
        // Only a pointer if nan bits and sign bit are set.
        return self.val & PointerMask == PointerMask;
    }

    pub inline fn asPointer(self: *const Value) linksection(".eval") ?*anyopaque {
        @setRuntimeSafety(debug);
        return @intToPtr(?*anyopaque, self.val & ~PointerMask);
    }

    pub inline fn asBool(self: *const Value) linksection(".eval") bool {
        @setRuntimeSafety(debug);
        return self.val == TrueMask;
    }

    pub inline fn isNone(self: *const Value) linksection(".eval") bool {
        return self.val == NoneMask;
    }

    pub inline fn isFalse(self: *const Value) bool {
        return self.val == FalseMask;
    }

    pub inline fn isTrue(self: *const Value) bool {
        return self.val == TrueMask;
    }

    pub inline fn isBool(self: *const Value) linksection(".eval") bool {
        @setRuntimeSafety(debug);
        return self.val & BooleanMask == BooleanMask;
    }

    pub inline fn getTag(self: *const Value) linksection(".eval") u3 {
        @setRuntimeSafety(debug);
        return @intCast(u3, @intCast(u32, self.val >> 32) & TagMask);
        // if (endian == .Little) {
        //     return @intCast(u3, self.two[1] & TagMask);
        // } else {
        //     return @intCast(u3, self.two[0] & TagMask);
        // }
    }

    pub inline fn initFalse() Value {
        return .{ .val = FalseMask };
    }

    pub inline fn initTrue() Value {
        return .{ .val = TrueMask };
    }

    pub inline fn initF64(val: f64) Value {
        @setRuntimeSafety(debug);
        return .{ .val = @bitCast(u64, val) };
    }

    pub inline fn initRaw(val: u64) Value {
        @setRuntimeSafety(debug);
        return .{ .val = val };
    }

    pub inline fn initNone() linksection(".eval") Value {
        @setRuntimeSafety(debug);
        return .{ .val = NoneMask };
    }

    pub inline fn initBool(b: bool) linksection(".eval") Value {
        if (b) {
            return .{ .val = TrueMask };
        } else {
            return .{ .val = FalseMask };
        }
    }

    pub inline fn initPtr(ptr: ?*anyopaque) Value {
        @setRuntimeSafety(debug);
        return .{ .val = PointerMask | @ptrToInt(ptr) };
    }

    pub inline fn initConstStr(start: u32, len: u15) Value {
        return .{ .val = ConstStringMask | (@as(u64, len) << 35) | start };
    }

    pub inline fn asConstStr(self: *const Value) stdx.IndexSlice(u32) {
        @setRuntimeSafety(debug);
        const len = (@intCast(u32, self.val >> 32) & BeforeTagMask) >> 3;
        const start = @intCast(u32, self.val & 0xffffffff);
        return stdx.IndexSlice(u32).init(start, start + len);

        // if (endian == .Little) {
        //     const len = (self.two[1] & BeforeTagMask) >> 3;
        //     const start = self.two[0];
        //     return stdx.IndexSlice(u32).init(start, start + len);
        // } else {
        //     const len = self.two[0] & BeforeTagMask >> 3;
        //     const start = self.two[1];
        //     return stdx.IndexSlice(u32).init(start, start + len);
        // }
    }

    pub inline fn floatIsSpecial(val: f64) bool {
        @setRuntimeSafety(debug);
        if (std.math.isInf(val)) return true;
        return false;
    }

    pub inline fn floatCanBeInteger(val: f64) linksection(".eval") bool {
        @setRuntimeSafety(debug);
        // return @fabs(std.math.floor(val) - val) < std.math.f64_epsilon;
        
        // This seems to be the faster check so far.
        return std.math.floor(val) == val;
    }

    pub inline fn initError(id: u32) Value {
        return .{ .val = ErrorMask | id };
    }

    pub fn dump(self: *const Value) void {
        if (self.isNumber()) {
            log.info("Number {}", .{self.asF64()});
        } else {
            if (self.isPointer()) {
                const obj = stdx.ptrCastAlign(*cy.HeapObject, self.asPointer().?);
                switch (obj.common.structId) {
                    cy.ListS => log.info("List {*} len={}", .{obj, obj.list.list.len}),
                    cy.MapS => log.info("Map {*} size={}", .{obj, obj.map.inner.size}),
                    cy.StringS => {
                        if (obj.string.len > 20) {
                            log.info("String {*} len={} str=\"{s}\"...", .{obj, obj.string.len, obj.string.ptr[0..20]});
                        } else {
                            log.info("String {*} len={} str={s}", .{obj, obj.string.len, obj.string.ptr[0..obj.string.len]});
                        }
                    },
                    cy.LambdaS => log.info("Lambda {*}", .{obj}),
                    cy.ClosureS => log.info("Closure {*}", .{obj}),
                    cy.FiberS => log.info("Fiber {*}", .{obj}),
                    else => {
                        log.info("HeapObject {*} {}", .{obj, obj.common.structId});
                    },
                }
            } else {
                switch (self.getTag()) {
                    TagNone => {
                        log.info("None", .{});
                    },
                    else => {
                        log.info("{}", .{self.val});
                    },
                }
            }
        }
    }

    pub fn getUserTag(self: *const Value) ValueUserTag {
        if (self.isNumber()) {
            return .number;
        } else {
            if (self.isPointer()) {
                const obj = stdx.ptrCastAlign(*cy.HeapObject, self.asPointer().?);
                switch (obj.common.structId) {
                    cy.ListS => return .list,
                    cy.MapS => return .map,
                    cy.StringS => return .string,
                    cy.ClosureS => return .closure,
                    cy.LambdaS => return .lambda,
                    cy.FiberS => return .fiber,
                    else => {
                        return .object;
                    },
                }
            } else {
                switch (self.getTag()) {
                    TagBoolean => return .boolean,
                    else => unreachable,
                }
            }
        }
    }
};

pub const ValueUserTag = enum {
    number,
    boolean,
    object,
    list,
    map,
    string,
    constString,
    closure,
    lambda,
    fiber,
};

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