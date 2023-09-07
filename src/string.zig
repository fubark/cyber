// Copyright (c) 2023 Cyber (See LICENSE)

/// Strings and string operations.

const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const rt = cy.rt;

/// Like `ArrayList` except the buffer is allocated as a `Astring` or `Ustring`.
/// TODO: Might not need the Astring -> Ustring upgrade logic now that there is HeapRawStringBuilder.
pub const HeapStringBuilder = struct {
    buf: []u8,
    len: u32,
    isAstring: bool,
    hasObject: bool,
    vm: *cy.VM,

    /// Starts as an Astring heap pool object.
    pub fn init(vm: *cy.VM) !HeapStringBuilder {
        const obj = try vm.allocPoolObject();
        obj.astring = .{
            .structId = rt.AstringT,
            .rc = 1,
            .len = cy.MaxPoolObjectStringByteLen,
            .bufStart = undefined,
        };
        return .{
            .buf = obj.astring.getSlice(),
            .len = 0,
            .isAstring = true,
            .hasObject = true,
            .vm = vm,
        };
    }

    fn getHeapObject(self: *const HeapStringBuilder) * align(@alignOf(cy.HeapObject)) cy.HeapObject {
        return @ptrCast(self.buf.ptr - @offsetOf(cy.Astring, "bufStart"));
    }

    pub fn deinit(self: *HeapStringBuilder) void {
        if (self.hasObject) {
            const obj = self.getHeapObject();
            obj.astring.len = self.len;
            self.vm.freeObject(obj);
            self.hasObject = false;
        }
    }

    pub fn ownObject(self: *HeapStringBuilder, alloc: std.mem.Allocator) *cy.HeapObject {
        const obj = self.getHeapObject();
        obj.astring.len = self.len;

        // Shrink.
        const objBuf = (self.buf.ptr - 16)[0..self.buf.len + 16];
        _ = alloc.resize(objBuf, self.len + 16);

        self.hasObject = false;
        return obj;
    }

    pub fn appendString(self: *HeapStringBuilder, alloc: std.mem.Allocator, str: []const u8, utf8: bool) !void {
        try self.ensureTotalCapacity(alloc, self.len + str.len);
        const oldLen = self.len;
        self.len += @intCast(str.len);
        std.mem.copy(u8, self.buf[oldLen..self.len], str);
        if (self.isAstring and utf8) {
            // Upgrade to Ustring.
            const obj = self.getHeapObject();
            obj.head.typeId = rt.UstringT;
            self.isAstring = false;
        }
    }

    pub inline fn ensureTotalCapacity(self: *HeapStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        if (newCap > self.buf.len) {
            try self.growTotalCapacity(alloc, newCap);
        }
    }

    pub fn growTotalCapacityPrecise(self: *HeapStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        if (self.buf.len > cy.MaxPoolObjectStringByteLen) {
            const oldHead = (self.buf.ptr - 16)[0..self.buf.len + 16];
            if (alloc.resize(oldHead, 16 + newCap)) {
                self.buf.len = newCap;
            } else {
                const old = self.buf;
                const objSlice = try alloc.alignedAlloc(u8, @alignOf(cy.HeapObject), 16 + newCap);
                const obj: *cy.HeapObject = @ptrCast(objSlice.ptr);
                obj.astring = .{
                    .structId = if (self.isAstring) rt.AstringT else rt.UstringT,
                    .rc = 1,
                    .len = 0,
                    .bufStart = undefined,
                };
                self.buf = objSlice[16..newCap];
                std.mem.copy(u8, self.buf[0..self.len], old[0..self.len]);
                alloc.free(oldHead);
            }
        } else {
            const oldObj = self.getHeapObject();
            const old = self.buf;
            const objSlice = try alloc.alignedAlloc(u8, @alignOf(cy.HeapObject), 16 + newCap);
            const obj: *cy.HeapObject = @ptrCast(objSlice.ptr);
            obj.astring = .{
                .structId = if (self.isAstring) rt.AstringT else rt.UstringT,
                .rc = 1,
                .len = 0,
                .bufStart = undefined,
            };
            self.buf = objSlice[16..newCap];
            std.mem.copy(u8, self.buf[0..self.len], old[0..self.len]);

            // Free pool object.
            oldObj.astring.len = self.len;
            self.vm.freeObject(oldObj);
        }
    }

    pub fn growTotalCapacity(self: *HeapStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        var betterCap = self.buf.len;
        while (true) {
            betterCap +|= betterCap / 2 + 8;
            if (betterCap >= newCap) {
                break;
            }
        }
        try self.growTotalCapacityPrecise(alloc, betterCap);
    }
};

pub const HeapRawStringBuilder = struct {
    buf: []u8,
    len: u32,
    hasObject: bool,
    vm: *cy.VM,

    pub fn init(vm: *cy.VM) !HeapRawStringBuilder {
        const obj = try cy.heap.allocPoolObject(vm);
        obj.rawstring = .{
            .structId = rt.RawstringT,
            .rc = 1,
            .len = cy.MaxPoolObjectRawStringByteLen,
            .bufStart = undefined,
        };
        return .{
            .buf = obj.rawstring.getSlice(),
            .len = 0,
            .hasObject = true,
            .vm = vm,
        };
    }

    fn getHeapObject(self: *const HeapRawStringBuilder) *cy.HeapObject {
        return @ptrCast(@alignCast(self.buf.ptr - cy.RawString.BufOffset));
    }

    pub fn deinit(self: *HeapRawStringBuilder) void {
        if (self.hasObject) {
            const obj = self.getHeapObject();
            obj.rawstring.len = self.len;
            cy.heap.freeObject(self.vm, obj);
            self.hasObject = false;
        }
    }

    pub fn ownObject(self: *HeapRawStringBuilder, alloc: std.mem.Allocator) *cy.HeapObject {
        const obj = self.getHeapObject();
        obj.rawstring.len = self.len;

        // Shrink.
        const objBuf = (self.buf.ptr - 12)[0..self.buf.len + 12];
        _ = alloc.resize(objBuf, self.len + 12);

        self.hasObject = false;
        return obj;
    }

    pub fn appendString(self: *HeapRawStringBuilder, alloc: std.mem.Allocator, str: []const u8) !void {
        try self.ensureTotalCapacity(alloc, self.len + str.len);
        const oldLen = self.len;
        self.len += @intCast(str.len);
        std.mem.copy(u8, self.buf[oldLen..self.len], str);
    }

    pub inline fn ensureTotalCapacity(self: *HeapRawStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        if (newCap > self.buf.len) {
            try self.growTotalCapacity(alloc, newCap);
        }
    }

    pub fn growTotalCapacityPrecise(self: *HeapRawStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        if (self.buf.len < cy.MaxPoolObjectRawStringByteLen) {
            const oldHead = @as([*] align (@alignOf(cy.HeapObject)) u8, @alignCast(self.buf.ptr - cy.RawString.BufOffset))[0..self.buf.len + cy.RawString.BufOffset];
            if (alloc.resize(oldHead, cy.RawString.BufOffset + newCap)) {
                self.buf.len = newCap;
            } else {
                const old = self.buf;
                const objSlice = try alloc.alignedAlloc(u8, @alignOf(cy.HeapObject), cy.RawString.BufOffset + newCap);
                const obj: *cy.HeapObject = @ptrCast(objSlice.ptr);
                obj.rawstring = .{
                    .structId = rt.RawstringT,
                    .rc = 1,
                    .len = 0,
                    .bufStart = undefined,
                };
                self.buf = objSlice[cy.RawString.BufOffset..cy.RawString.BufOffset+newCap];
                std.mem.copy(u8, self.buf[0..self.len], old[0..self.len]);
                alloc.free(oldHead);
            }
        } else {
            const oldObj = self.getHeapObject();
            const old = self.buf;
            const objSlice = try alloc.alignedAlloc(u8, @alignOf(cy.HeapObject), cy.RawString.BufOffset + newCap);
            const obj: *cy.HeapObject = @ptrCast(objSlice.ptr);
            obj.rawstring = .{
                .structId = rt.RawstringT,
                .rc = 1,
                .len = 0,
                .bufStart = undefined,
            };
            self.buf = objSlice[cy.RawString.BufOffset..cy.RawString.BufOffset+newCap];
            std.mem.copy(u8, self.buf[0..self.len], old[0..self.len]);

            // Free pool object.
            oldObj.rawstring.len = self.len;
            cy.heap.freeObject(self.vm, oldObj);
        }
    }

    pub fn growTotalCapacity(self: *HeapRawStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        var betterCap = self.buf.len;
        while (true) {
            betterCap +|= betterCap / 2 + 8;
            if (betterCap >= newCap) {
                break;
            }
        }
        try self.growTotalCapacityPrecise(alloc, betterCap);
    }
};

fn isAstringScalar(str: []const u8) linksection(cy.Section) bool {
    for (str) |ch| {
        if (ch & 0x80 > 0) {
            return false;
        }
    }
    return true;
}

pub fn isAstring(str: []const u8) linksection(cy.Section) bool {
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        var vbuf: @Vector(VecSize, u8) = undefined;
        var i: usize = 0;
        while (i + VecSize <= str.len) : (i += VecSize) {
            vbuf = str[i..i+VecSize][0..VecSize].*;
            const res = @reduce(.Or, vbuf);
            if (res & 0x80 > 0) {
                // Found non ascii char.
                return false;
            }
        }
        if (i < str.len) {
            // Remaining use cpu.
            return isAstringScalar(str[i..]);
        }
        return true;
    } else {
        return isAstringScalar(str);
    }
}

/// Validates a utf8 string and returns the char length.
/// If the char length returned is the same as the byte len, it's also a valid ascii string.
/// TODO: Implement SIMD version.
pub fn validateUtf8(s: []const u8) linksection(cy.Section) ?usize {
    var charLen: usize = 0;
    var i: usize = 0;
    while (i < s.len) {
        const cp_len = std.unicode.utf8ByteSequenceLength(s[i]) catch return null;
        if (i + cp_len > s.len) {
            return null;
        }
        _ = std.unicode.utf8Decode(s[i .. i + cp_len]) catch {
            return null;
        };
        i += cp_len;
        charLen += 1;
    }
    return charLen;
}

/// Assumes valid UTF-8 sequence.
pub fn utf8Len(s: []const u8) linksection(cy.Section) usize {
    var len: usize = 0;
    var i: usize = 0;
    while (i < s.len) {
        const cp_len = std.unicode.utf8ByteSequenceLength(s[i]) catch return cy.fatal();
        if (i + cp_len == s.len) {
            return len;
        }
        i += cp_len;
        len += 1;
    }
    return len;
}

pub fn utf8CharSliceAt(str: []const u8, idx: usize) linksection(cy.Section) ?[]const u8 {
    const cp_len = std.unicode.utf8ByteSequenceLength(str[idx]) catch return null;
    if (idx + cp_len > str.len) {
        return null;
    }
    const slice = str[idx .. idx + cp_len];
    _ = std.unicode.utf8Decode(slice) catch {
        return null;
    };
    return slice;
}

/// Assumes str is valid UTF-8 and charIdx is at most charLen.
pub fn ustringSeekByCharIndex(str: []const u8, seekIdx: u32, seekCharIdx: u32, charIdx: u32) linksection(cy.Section) usize {
    var i: usize = 0;
    var curCharIdx: u32 = 0;
    if (charIdx >= seekCharIdx) {
        i = seekIdx;
        curCharIdx = seekCharIdx;
    }
    while (true) {
        if (curCharIdx == charIdx) {
            return i;
        } else {
            const len = std.unicode.utf8ByteSequenceLength(str[i]) catch cy.fatal();
            i += len;
            curCharIdx += 1;
        }
    }
}

fn indexOfCharScalar(buf: []const u8, needle: u8) linksection(cy.Section) ?usize {
    for (buf, 0..) |ch, i| {
        if (ch == needle) {
            return i;
        }
    }
    return null;
}

fn indexOfCharSimdRemain(comptime VecSize: usize, buf: []const u8, needle: u8) ?usize {
    const MaskInt = std.meta.Int(.unsigned, VecSize);
    const vbuf = cy.simd.load(VecSize, u8, buf);
    const mask: MaskInt = (@as(MaskInt, 1) << @intCast(buf.len)) - 1;
    const hitMask = @as(MaskInt, @bitCast(vbuf == @as(@Vector(VecSize, u8), @splat(needle)))) & mask;
    if (hitMask > 0) {
        return @ctz(hitMask);
    }
    return null;
}

fn indexOfCharSimdFixed4(comptime VecSize: usize, buf: []const u8, needle: u8) ?usize {
    if (VecSize == 32 and hasAvx2 and builtin.cpu.arch == .x86_64) {
        // const MaskInt = std.meta.Int(.unsigned, VecSize);
        const vneedle: @Vector(VecSize, u8) = @splat(needle);
        var i: usize = 0;
        while (i + VecSize * 4 <= buf.len) : (i += VecSize * 4) {
            // Inline asm to avoid extra vpmovmskb.
            // TODO: There is a slight perf gain to be had when using 32-byte aligned memory.
            const hitMask = asm (
                \\vpcmpeqb (%rsi, %r8), %%ymm0, %%ymm1
                \\vpcmpeqb 32(%rsi, %r8), %%ymm0, %%ymm2
                \\vpcmpeqb 64(%rsi, %r8), %%ymm0, %%ymm3
                \\vpcmpeqb 96(%rsi, %r8), %%ymm0, %%ymm4
                \\vpor %%ymm1, %%ymm2, %%ymm5
                \\vpor %%ymm3, %%ymm4, %%ymm6
                \\vpor %%ymm5, %%ymm6, %%ymm5
                \\vpmovmskb %%ymm5, %eax 
                : [ret] "={eax}" (-> u32)
                : [needle] "{ymm0}" (vneedle),
                  [buf] "{rsi}" (buf.ptr),
                  [i] "{r8}" (i),
            );
            if (hitMask > 0) {
                // Find out which one contains the result.
                var mask = asm (
                    \\vpmovmskb %%ymm1, %eax
                    : [ret] "={eax}" (-> u32)
                );
                if (mask > 0) {
                    const bitIdx = @ctz(mask);
                    return i + bitIdx;
                }
                mask = asm (
                    \\vpmovmskb %%ymm2, %eax
                    : [ret] "={eax}" (-> u32)
                );
                if (mask > 0) {
                    const bitIdx = @ctz(mask);
                    return i + bitIdx + 32;
                }
                mask = asm (
                    \\vpmovmskb %%ymm3, %eax
                    : [ret] "={eax}" (-> u32)
                );
                if (mask > 0) {
                    const bitIdx = @ctz(mask);
                    return i + bitIdx + 64;
                } else {
                    const bitIdx = @ctz(mask);
                    return i + bitIdx + 96;
                }
            }
        }
        return null;
    } else {
        return indexOfCharSimdFixed(VecSize, buf, needle);
    }
}

fn indexOfCharSimdFixed(comptime VecSize: usize, buf: []const u8, needle: u8) ?usize {
    const MaskInt = std.meta.Int(.unsigned, VecSize);
    var vbuf: @Vector(VecSize, u8) = undefined;
    const vneedle: @Vector(VecSize, u8) = @splat(needle);
    var i: usize = 0;
    while (i + VecSize <= buf.len) : (i += VecSize) {
        vbuf = buf[i..i+VecSize][0..VecSize].*;
        const hitMask: MaskInt = @bitCast(vbuf == vneedle);
        if (hitMask > 0) {
            const bitIdx = @ctz(hitMask);
            return i + bitIdx;
        }
    }
    return null;
}

const hasAvx2 = std.Target.x86.featureSetHasAny(builtin.cpu.features, .{ .avx2 });
const NullId = std.math.maxInt(u32);

fn indexOfScalar(str: []const u8, needle: []const u8) linksection(cy.StdSection) ?usize {
    return std.mem.indexOf(u8, str, needle);
}

/// `needle` can be any UTF-8 string.
/// `str` is assumed to have `needle.len` additional bytes so that an offset buffer can loaded without bounds check.
fn indexOfSimdFixed(comptime VecSize: usize, str: []const u8, needle: []const u8) linksection(cy.StdSection) ?usize {
    const MaskInt = std.meta.Int(.unsigned, VecSize);
    // Algorithm based on: http://0x80.pl/articles/simd-strfind.html.

    // Set up hashing. Hash end is currently always the last char position in needle.
    const offset = needle.len - 1;
    const first: @Vector(VecSize, u8) = @splat(needle[0]);
    const last: @Vector(VecSize, u8) = @splat(needle[needle.len-1]);

    var i: usize = 0;
    while (i + VecSize <= str.len) : (i += VecSize) {
        const buf1: @Vector(VecSize, u8) = str[i..i + VecSize][0..VecSize].*;
        const firstMask: MaskInt = @bitCast(buf1 == first);
        if (firstMask > 0) {
            const buf2: @Vector(VecSize, u8) = @as([*]const u8, @ptrCast(str.ptr + i + offset))[0..VecSize].*;
            const lastMask: MaskInt = @bitCast(buf2 == last);
            if (lastMask > 0) {
                var hitMask = firstMask & lastMask;
                // Visit each potential substring match until a match is found.
                while (hitMask > 0) {
                    const idx = @ctz(hitMask);
                    if (std.mem.eql(u8, @as([*]const u8, @ptrCast(str.ptr + i + idx))[0..needle.len], needle)) {
                        return i + idx;
                    }
                    hitMask = unsetLowestBit(hitMask, idx);
                }
            }
        }
    }
    return null;
}

/// `str.len` can be assumed to be >= 1 and < VecSize + needle.len.
fn indexOfSimdRemain(comptime VecSize: usize, str_: []const u8, needle: []const u8) linksection(cy.StdSection) ?usize {
    const MaskInt = std.meta.Int(.unsigned, VecSize);

    // This will check up to `VecSize` bytes and any extras after that
    // can be discarded since the length will be < `needle.len`.
    const str = if (str_.len > VecSize) str_[0..VecSize] else str_;

    // Algorithm based on: http://0x80.pl/articles/simd-strfind.html.

    // Set up hashing. Hash end is currently always the last char position in needle.
    const offset = needle.len - 1;
    const first: @Vector(VecSize, u8) = @splat(needle[0]);
    const last: @Vector(VecSize, u8) = @splat(needle[needle.len-1]);

    // Don't need a remainMask since the eq op on `first` and `last` will be false.
    const buf1 = cy.simd.load(VecSize, u8, str);
    const firstMask: MaskInt = @bitCast(buf1 == first);
    if (firstMask > 0) {
        const buf2 = cy.simd.load(VecSize, u8, str_[offset..]);
        const lastMask: MaskInt = @bitCast(buf2 == last);
        if (lastMask > 0) {
            var hitMask = firstMask & lastMask;
            // Visit each potential substring match until a match is found.
            while (hitMask > 0) {
                const idx = @ctz(hitMask);
                if (std.mem.eql(u8, @as([*]const u8, @ptrCast(str.ptr + idx))[0..needle.len], needle)) {
                    return idx;
                }
                hitMask = unsetLowestBit(hitMask, idx);
            }
        }
    }
    return null;
}

inline fn unsetLowestBit(mask: anytype, idx: anytype) @TypeOf(mask) {
    const Mask = @TypeOf(mask);
    // const MaskShiftInt = std.meta.Int(.unsigned, std.math.log2(@bitSizeOf(Mask)));
    return mask & ~(@as(Mask, 1) << @intCast(idx));
}

/// `needle.len` is assumed to be at most `str.len`.
pub fn indexOf(str: []const u8, needle: []const u8) linksection(cy.StdSection) ?usize {
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        var i: usize = 0;
        // Ensure that there is `needle.len` additional bytes after the fixed width.
        const iters = @divTrunc(str.len - needle.len, VecSize);
        if (iters > 0) {
            if (indexOfSimdFixed(VecSize, str[0..iters * VecSize], needle)) |idx| {
                return idx;
            }
            i += iters * VecSize;
        }
        if (i < str.len) {
            if (indexOfSimdRemain(VecSize, str[i..], needle)) |idx| {
                return i + idx;
            }
        }
        return null;
    } else {
        return indexOfScalar(str, needle);
    }
}

fn indexOfAsciiSetScalar(str: []const u8, set: []const u8) linksection(cy.StdSection) ?usize {
    for (str, 0..) |code, i| {
        if (indexOfChar(set, code) != null) {
            return i;
        }
    }
    return null;
}

pub fn indexOfAsciiSetSimdRemain(comptime VecSize: usize, str: []const u8, set: []const u8) linksection(cy.StdSection) ?usize {
    const MaskInt = std.meta.Int(.unsigned, VecSize);
    // const MaskShiftInt = std.meta.Int(.unsigned, std.math.log2(@bitSizeOf(MaskInt)));

    if (VecSize == 32 and hasAvx2) {
        // Same as SimdFixed version with a mask at the end.

        // Construct the LUT.
        // Since it's ASCII, the upper needle mask can have at most 8 bits.
        var lut: @Vector(VecSize, u8) = @splat(@as(u8, 0));
        for (set) |code| {
            const lower = code & 0xF;
            const upper = code >> 4; // At most 7.
            lut[lower] = @as(u8, 1) << @intCast(upper);
            // Dupe to upper offset 128 for 256bit vpshufb.
            lut[16 + lower] = @as(u8, 1) << @intCast(upper);
        }

        var upperMaskLut: @Vector(VecSize, u8) = @splat(@as(u8, 0));
        comptime var j = 0;
        inline while (j < 8) : (j += 1) {
            upperMaskLut[j] = 1 << j;
            upperMaskLut[8 + j] = 255;
            // Dupe to upper offset 128 for 256bit vpshufb.
            upperMaskLut[16 + j] = 1 << j;
            upperMaskLut[24 + j] = 255;
        }

        const buf = cy.simd.load(VecSize, u8, str);
        const lower: @Vector(VecSize, u8) = buf & @as(@Vector(VecSize, u8), @splat(@as(u8, 0xF)));
        const upper: @Vector(VecSize, u8) = buf >> @splat(@as(u8, 4));

        const needle = asm (
            \\vpshufb %%ymm3, %%ymm0, %%ymm5
            : [ret] "={ymm5}" (-> @Vector(VecSize, u8))
            : [lut] "{ymm0}" (lut),
              [lower] "{ymm3}" (lower),
        );

        const upperMask = asm (
            \\vpshufb %%ymm4, %%ymm1, %%ymm6
            : [ret] "={ymm6}" (-> @Vector(VecSize, u8))
            : [upperMaskLut] "{ymm1}" (upperMaskLut),
              [upper] "{ymm4}" (upper),
        );

        const remainMask: MaskInt = (@as(MaskInt, 1) << @intCast(str.len)) - 1;
        var hitMask = @as(MaskInt, @bitCast((needle & upperMask) == upperMask)) & remainMask;
        if (hitMask > 0) {
            var res = @ctz(hitMask);
            if (str[res] & 0x80 == 0) {
                return res;
            } else {
                // Non-ascii result. Move to next bit in `hitMask`.
                hitMask = unsetLowestBit(hitMask, res);
                while (hitMask > 0) {
                    res = @ctz(hitMask);
                    if (str[res] & 0x80 == 0) {
                        return res;
                    }
                    hitMask = unsetLowestBit(hitMask, res);
                }
            }
        }
        return null;
    } else {
        return indexOfAsciiSetScalar(str, set);
    }
}

pub fn indexOfAsciiSetSimdFixed(comptime VecSize: usize, str: []const u8, set: []const u8) linksection(cy.StdSection) ?usize {
    const MaskInt = std.meta.Int(.unsigned, VecSize);

    if (VecSize == 32 and hasAvx2) {
        // Based on the algorithm: http://0x80.pl/articles/simd-byte-lookup.html

        // Construct the LUT.
        // Since it's ASCII, the upper needle mask can have at most 8 bits.
        var lut: @Vector(VecSize, u8) = @splat(@as(u8, 0));
        for (set) |code| {
            const lower = code & 0xF;
            const upper = code >> 4; // At most 7.
            lut[lower] = @as(u8, 1) << @intCast(upper);
            // Dupe to upper offset 128 for 256bit vpshufb.
            lut[16 + lower] = @as(u8, 1) << @intCast(upper);
        }

        var upperMaskLut: @Vector(VecSize, u8) = @splat(@as(u8, 0));
        comptime var j = 0;
        inline while (j < 8) : (j += 1) {
            upperMaskLut[j] = 1 << j;
            // Non-ascii maps to full mask.
            // This is unlikely to match the `lut` but if it does, the final resulting char is validated. 
            upperMaskLut[8 + j] = 255;
            // Dupe to upper offset 128 for 256bit vpshufb.
            upperMaskLut[16 + j] = 1 << j;
            upperMaskLut[24 + j] = 255;
        }

        var i: usize = 0;
        while (i + VecSize <= str.len) : (i += VecSize) {
            const buf: @Vector(VecSize, u8) = str[i..i+VecSize][0..VecSize].*;
            const lower: @Vector(VecSize, u8) = buf & @as(@Vector(VecSize, u8), @splat(@as(u8, 0xF)));
            const upper: @Vector(VecSize, u8) = buf >> @splat(@as(u8, 4));

            const needle = asm (
                \\vpshufb %%ymm3, %%ymm0, %%ymm5
                : [ret] "={ymm5}" (-> @Vector(VecSize, u8))
                : [lut] "{ymm0}" (lut),
                  [lower] "{ymm3}" (lower),
            );

            const upperMask = asm (
                \\vpshufb %%ymm4, %%ymm1, %%ymm6
                : [ret] "={ymm6}" (-> @Vector(VecSize, u8))
                : [upperMaskLut] "{ymm1}" (upperMaskLut),
                  [upper] "{ymm4}" (upper),
            );

            var hitMask: MaskInt = @bitCast((needle & upperMask) == upperMask);
            if (hitMask > 0) {
                var res = i + @ctz(hitMask);
                if (str[res] & 0x80 == 0) {
                    return res;
                } else {
                    // Non-ascii result. Move to next bit in `hitMask`.
                    hitMask = unsetLowestBit(hitMask, res);
                    while (hitMask > 0) {
                        res = i + @ctz(hitMask);
                        if (str[res] & 0x80 == 0) {
                            return res;
                        }
                        hitMask = unsetLowestBit(hitMask, res);
                    }
                }
            }
        }
        return null;
    } else {
        return indexOfAsciiSetScalar(str, set);
    }
}

pub fn indexOfAsciiSet(str: []const u8, set: []const u8) linksection(cy.StdSection) ?usize {
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        var i: usize = 0;
        const iters = @divTrunc(str.len, VecSize);
        if (iters > 0) {
            if (indexOfAsciiSetSimdFixed(VecSize, str[i..i + iters * VecSize], set)) |idx| {
                return i + idx;
            }
            i += iters * VecSize;
        }
        if (i < str.len) {
            if (indexOfAsciiSetSimdRemain(VecSize, str[i..], set)) |idx| {
                return i + idx;
            }
        }
        return null;
    } else {
        return indexOfAsciiSetScalar(str, set);
    }
}

/// For Ascii needle.
pub fn indexOfChar(buf: []const u8, needle: u8) linksection(cy.StdSection) ?usize {
    // SIMD is approx 5x faster than scalar.
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        var i: usize = 0;
        var iters = @divTrunc(buf.len, VecSize * 4);
        if (iters > 0) {
            if (indexOfCharSimdFixed4(VecSize, buf[0..iters * VecSize * 4], needle)) |idx| {
                return idx;
            }
            i += iters * VecSize * 4;
        }
        iters = @divTrunc(buf.len - i, VecSize);
        if (iters > 0) {
            if (indexOfCharSimdFixed(VecSize, buf[i..i + iters * VecSize], needle)) |idx| {
                return i + idx;
            }
            i += iters * VecSize;
        }
        if (i < buf.len) {
            if (indexOfCharSimdRemain(VecSize, buf[i..], needle)) |idx| {
                return i + idx;
            }
        }
        return null;
    } else {
        return indexOfCharScalar(buf, needle);
    }
}

pub fn toUtf8CharIdx(str: []const u8, idx: usize) linksection(cy.Section) usize {
    var charIdx: usize = 0;
    var i: usize = 0;
    while (i < idx) {
        const cpLen = std.unicode.utf8ByteSequenceLength(str[i]) catch cy.fatal();
        i += cpLen;
        charIdx += 1;
    }
    return charIdx;
}

pub fn charIndexOfCodepoint(str: []const u8, needle: u21) linksection(cy.Section) ?usize {
    var charIdx: usize = 0;
    var i: usize = 0;
    while (i < str.len) {
        const cpLen = std.unicode.utf8ByteSequenceLength(str[i]) catch cy.fatal();
        const cp = std.unicode.utf8Decode(str[i..i+cpLen]) catch cy.fatal();
        if (cp == needle) {
            return charIdx;
        }
        i += cpLen;
        charIdx += 1;
    }
    return null;
}

fn getLineEndCpu(buf: []const u8) linksection(cy.StdSection) ?usize {
    for (buf, 0..) |ch, i| {
        if (ch == '\n') {
            return i + 1;
        } else if (ch == '\r') {
            if (i + 1 < buf.len) {
                if (buf[i+1] == '\n') {
                    return i + 2;
                }
            }
            return i + 1;
        }
    }
    return null;
}

test "getLineEndCpu()" {
    const str = "abcxyz\nfoobar\rdeadbeef\r\nzzz";
    try t.eq(getLineEndCpu(str).?, 7);
    try t.eq(getLineEndCpu(str[7..]).?, 7);
    try t.eq(getLineEndCpu(str[14..]).?, 10);
    try t.eq(getLineEndCpu(str[24..]), null);
}

pub fn getLineEnd(buf: []const u8) linksection(cy.StdSection) ?usize {
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        const MaskInt = std.meta.Int(.unsigned, VecSize);
        var vbuf: @Vector(VecSize, u8) = undefined;
        const lfNeedle: @Vector(VecSize, u8) = @splat(@as(u8, '\n'));
        const crNeedle: @Vector(VecSize, u8) = @splat(@as(u8, '\r'));
        var i: usize = 0;
        while (i + VecSize <= buf.len) : (i += VecSize) {
            vbuf = buf[i..i+VecSize][0..VecSize].*;
            const lfHits: MaskInt = @bitCast(vbuf == lfNeedle);
            const crHits: MaskInt = @bitCast(vbuf == crNeedle);
            const bitIdx = @ctz(lfHits | crHits);
            if (bitIdx > 0) {
                // Found.
                const res = i + bitIdx;
                if (buf[res] == '\n') {
                    return res + 1;
                } else {
                    if (res + 1 < buf.len and buf[res+1] == '\n') {
                        return res + 2;
                    } else {
                        return res + 1;
                    }
                }
            }
        }
        if (i < buf.len) {
            // Remaining use cpu.
            if (getLineEndCpu(buf[i..])) |res| {
                return i + res;
            }
        }
        return null;
    } else {
        return getLineEndCpu(buf);
    }
}

/// Like std.mem.replacementSize but also records the idxes.
pub fn prepReplacement(str: []const u8, needle: []const u8, replacement: []const u8, idxesWriter: anytype) linksection(cy.StdSection) !usize {
    if (needle.len == 0) {
        return str.len;
    }
    var i: usize = 0;
    var size: usize = str.len;
    while (i < str.len) {
        if (std.mem.startsWith(u8, str[i..], needle)) {
            size = size - needle.len + replacement.len;
            _ = try idxesWriter.write(std.mem.asBytes(&@as(u32, @intCast(i))));
            i += needle.len;
        } else {
            i += 1;
        }
    }
    return size;
}

pub fn replaceAtIdxes(dst: []u8, src: []const u8, needleLen: u32, replacement: []const u8, idxes: []const u32) linksection(cy.StdSection) void {
    var dstIdx: usize = 0;
    var srcIdx: usize = 0;
    for (idxes) |idx| {
        // First copy segment between last `idx` and this `idx`.
        const preSegmentLen = idx - srcIdx;
        std.mem.copy(u8, dst[dstIdx..dstIdx + preSegmentLen], src[srcIdx..idx]);
        dstIdx += preSegmentLen;

        // Copy replacement.
        std.mem.copy(u8, dst[dstIdx..dstIdx + replacement.len], replacement);
        dstIdx += replacement.len;
        srcIdx += idx + needleLen;
    }
    // Copy end segment.
    std.mem.copy(u8, dst[dstIdx..], src[srcIdx..]);
}

pub fn utf8CodeAtNoCheck(str: []const u8, idx: usize) u21 {
    const len = std.unicode.utf8ByteSequenceLength(str[idx]) catch cy.fatal();
    return std.unicode.utf8Decode(str[0..len]) catch cy.fatal();
}

pub fn utf8CharSliceAtNoCheck(str: []const u8, idx: usize) []const u8 {
    const len = std.unicode.utf8ByteSequenceLength(str[idx]) catch cy.fatal();
    return str[idx..idx+len];
}

pub const StringConcat = struct {
    left: []const u8,
    right: []const u8,
};

pub const StringConcatContext = struct {
    pub fn hash(_: StringConcatContext, concat: StringConcat) u64 {
        return @call(.always_inline, computeStringConcatHash, .{concat.left, concat.right});
    }

    pub fn eql(_: StringConcatContext, a: StringConcat, b: []const u8) bool {
        if (a.left.len + a.right.len != b.len) {
            return false;
        }
        return std.mem.eql(u8, a.left, b[0..a.left.len]) and 
            std.mem.eql(u8, a.right, b[a.left.len..]);
    }
};

pub const StringConcat3 = struct {
    str1: []const u8,
    str2: []const u8,
    str3: []const u8,
};

pub const StringConcat3Context = struct {
    pub fn hash(_: StringConcat3Context, concat: StringConcat3) u64 {
        return @call(.always_inline, computeStringConcat3Hash, .{concat.str1, concat.str2, concat.str3});
    }

    pub fn eql(_: StringConcat3Context, a: StringConcat3, b: []const u8) bool {
        if (a.str1.len + a.str2.len + a.str3.len != b.len) {
            return false;
        }
        return std.mem.eql(u8, a.str1, b[0..a.str1.len]) and 
            std.mem.eql(u8, a.str2, b[a.str1.len..a.str1.len+a.str2.len]) and
            std.mem.eql(u8, a.str3, b[a.str1.len+a.str2.len..]);
    }
};

fn computeStringConcat3Hash(str1: []const u8, str2: []const u8, str3: []const u8) u64 {
    var c = std.hash.Wyhash.init(0);
    @call(.always_inline, std.hash.Wyhash.update, .{&c, str1});
    @call(.always_inline, std.hash.Wyhash.update, .{&c, str2});
    @call(.always_inline, std.hash.Wyhash.update, .{&c, str3});
    return @call(.always_inline, std.hash.Wyhash.final, .{&c});
}

fn computeStringConcatHash(left: []const u8, right: []const u8) u64 {
    var c = std.hash.Wyhash.init(0);
    @call(.always_inline, std.hash.Wyhash.update, .{&c, left});
    @call(.always_inline, std.hash.Wyhash.update, .{&c, right});
    return @call(.always_inline, std.hash.Wyhash.final, .{&c});
}

test "computeStringConcatHash() matches the concated string hash." {
    const exp = std.hash.Wyhash.hash(0, "foobar");
    try t.eq(computeStringConcatHash("foo", "bar"), exp);
    try t.eq(computeStringConcat3Hash("fo", "ob", "ar"), exp);
}

pub fn getStaticUstringHeader(vm: *cy.VM, start: usize) *align(1) cy.StaticUstringHeader {
    return @ptrCast(vm.strBuf.ptr + start - 12);
}