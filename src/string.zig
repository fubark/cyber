const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const rt = cy.rt;
const bt = cy.types.BuiltinTypes;
const log = cy.log.scoped(.string);

/// Like `ArrayList` except the buffer is allocated as a `Astring` or `Ustring`.
/// A final slice is returned from the backing string to avoid an extra copy.
pub const HeapStringBuilder = struct {
    buf_obj: *cy.heap.StrBuffer,
    buf: []u8,
    len: u32,
    ascii: bool,
    hasObject: bool,
    detect_encoding: bool = true,
    vm: *cy.VM,

    /// Starts as an ascii string buffer.
    pub fn init(vm: *cy.VM) !HeapStringBuilder {
        const obj = try vm.newStringBufferUndef(cy.heap.MaxPoolObjectUserBytes-16);
        return .{
            .buf_obj = @ptrCast(obj),
            .buf = obj.buffer.slice(u8),
            .len = 0,
            .ascii = true,
            .hasObject = true,
            .vm = vm,
        };
    }

    pub fn deinit(self: *HeapStringBuilder) void {
        if (self.hasObject) {
            self.vm.releaseObject(@ptrCast(self.buf_obj));
            self.hasObject = false;
        }
    }

    pub const Writer = struct {
        buf: *HeapStringBuilder,

        pub fn print(self: Writer, comptime format: []const u8, args: anytype) !void {
            try std.fmt.format(self, format, args);
        }

        pub fn writeByte(self: Writer, b: u8) !void {
            try self.writeAll(&.{ b });
        }

        pub fn writeAll(self: Writer, data: []const u8) !void {
            _ = try self.buf.appendString(data);
        }

        pub fn writeBytesNTimes(self: Writer, bytes: []const u8, n: usize) anyerror!void {
            var i: usize = 0;
            while (i < n) : (i += 1) {
                try self.writeAll(bytes);
            }
        }

        pub const Error = anyerror;
    };

    pub fn writer(self: *HeapStringBuilder) Writer {
        return Writer{
            .buf = self,
        };
    }

    pub fn buildWithEncoding(self: *HeapStringBuilder, ascii: bool) !cy.Value {
        self.hasObject = false;
        if (ascii) {
            return self.vm.newAstrSlice(self.buf[0..self.len], self.buf_obj);
        } else {
            return self.vm.newUstrSlice(self.buf[0..self.len], self.buf_obj);
        }
    }

    pub fn build(self: *HeapStringBuilder) !*cy.HeapObject {
        var slice: cy.Value = undefined;
        if (self.ascii) {
            slice = try self.vm.newAstrSlice(self.buf[0..self.len], self.buf_obj);
        } else {
            slice = try self.vm.newUstrSlice(self.buf[0..self.len], self.buf_obj);
        }

        // Don't need to retain for slice since we are moving.
        self.hasObject = false;
        return slice.asHeapObject();
    }

    pub fn appendString(self: *HeapStringBuilder, str: []const u8) !void {
        try self.ensureTotalCapacity(self.len + str.len);
        const oldLen = self.len;
        self.len += @intCast(str.len);
        @memcpy(self.buf[oldLen..self.len], str);
        if (self.detect_encoding) {
            const append_ascii = cy.string.isAstring(str);
            if (self.ascii and !append_ascii) {
                // Upgrade to Ustring.
                self.ascii = false;
            }
        }
    }

    pub inline fn ensureTotalCapacity(self: *HeapStringBuilder, newCap: usize) !void {
        if (newCap > self.buf.len) {
            try self.growTotalCapacity(newCap);
        }
    }

    pub fn growTotalCapacityPrecise(self: *HeapStringBuilder, newCap: usize) !void {
        const new_obj = try self.vm.newStringBufferUndef(newCap);
        const new_buf = new_obj.buffer.slice(u8);
        @memcpy(new_buf[0..self.len], self.buf[0..self.len]);

        self.vm.releaseObject(@ptrCast(self.buf_obj));
        self.buf_obj = @ptrCast(new_obj);
        self.buf = new_buf;
    }

    pub fn growTotalCapacity(self: *HeapStringBuilder, newCap: usize) !void {
        var betterCap = self.buf.len;
        while (true) {
            betterCap +|= betterCap / 2 + 8;
            if (betterCap >= newCap) {
                break;
            }
        }
        try self.growTotalCapacityPrecise(betterCap);
    }
};

fn isAstringScalar(str: []const u8) bool {
    for (str) |ch| {
        if (ch & 0x80 > 0) {
            return false;
        }
    }
    return true;
}

pub fn isAstring(str: []const u8) bool {
    if (comptime std.simd.suggestVectorLength(u8)) |VecSize| {
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

/// Validates a UTF-8 string and returns the char length.
/// If the char length returned is the same as the byte len, it's also a valid ascii string.
/// TODO: Implement SIMD version.
pub fn validateUtf8(s: []const u8) ?usize {
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

pub fn countRunes(s: []const u8) usize {
    var len: usize = 0;
    var i: usize = 0;
    while (i < s.len) {
        const cp_len = std.unicode.utf8ByteSequenceLength(s[i]) catch {
            i += 1;
            len += 1;
            continue;
        };
        i += cp_len;
        len += 1;
    }
    return len;
}

pub fn utf8CharSliceAt(str: []const u8, idx: usize) ?[]const u8 {
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

/// `out_bad_byte` can be used to determine if it's an ascii string.
pub fn ustringSeekByRuneIndex(str: []const u8, byte_idx: u32, rune_idx: u32, target_rune_idx: u32, out_bad_byte: *bool) !usize {
    var i: usize = 0;
    var curCharIdx: u32 = 0;
    var bad_byte = false;
    if (target_rune_idx >= rune_idx) {
        i = byte_idx;
        curCharIdx = rune_idx;
    }
    while (true) {
        if (curCharIdx == target_rune_idx) {
            out_bad_byte.* = bad_byte;
            return i;
        } else {
            if (i >= str.len) {
                return error.OutOfBounds;
            }
            const len = std.unicode.utf8ByteSequenceLength(str[i]) catch {
                i += 1;
                curCharIdx += 1;
                bad_byte = true;
                continue;
            };
            i += len;
            curCharIdx += 1;
        }
    }
}

fn indexOfCharScalar(buf: []const u8, needle: u8) ?usize {
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
            const hitMask = asm volatile (
                \\vpcmpeqb %[v0], %[needle], %%ymm1
                \\vpcmpeqb %[v1], %[needle], %%ymm2
                \\vpcmpeqb %[v2], %[needle], %%ymm3
                \\vpcmpeqb %[v3], %[needle], %%ymm4
                \\vpor %%ymm1, %%ymm2, %%ymm5
                \\vpor %%ymm3, %%ymm4, %%ymm6
                \\vpor %%ymm5, %%ymm6, %%ymm5
                \\vpmovmskb %%ymm5, %[ret]
                : [ret] "={eax}" (-> u32),
                : [needle] "x" (vneedle),
                  [v0] "x" (@as(@Vector(VecSize, u8), @as(*align(1) const @Vector(VecSize, u8), @ptrCast(&buf[0])).*)),
                  [v1] "x" (@as(@Vector(VecSize, u8), @as(*align(1) const @Vector(VecSize, u8), @ptrCast(&buf[VecSize])).*)),
                  [v2] "x" (@as(@Vector(VecSize, u8), @as(*align(1) const @Vector(VecSize, u8), @ptrCast(&buf[VecSize * 2])).*)),
                  [v3] "x" (@as(@Vector(VecSize, u8), @as(*align(1) const @Vector(VecSize, u8), @ptrCast(&buf[VecSize * 3])).*)),
                : .{ .ymm1 = true, .ymm2 = true, .ymm3 = true, .ymm4 = true, .ymm5 = true, .ymm6 = true });
            if (hitMask > 0) {
                // Find out which one contains the result.
                var mask = asm (
                    \\vpmovmskb %%ymm1, %[ret]
                    : [ret] "={eax}" (-> u32),
                );
                if (mask > 0) {
                    const bitIdx = @ctz(mask);
                    return i + bitIdx;
                }
                mask = asm (
                    \\vpmovmskb %%ymm2, %[ret]
                    : [ret] "={eax}" (-> u32),
                );
                if (mask > 0) {
                    const bitIdx = @ctz(mask);
                    return i + bitIdx + 32;
                }
                mask = asm (
                    \\vpmovmskb %%ymm3, %[ret]
                    : [ret] "={eax}" (-> u32),
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

fn indexOfScalar(str: []const u8, needle: []const u8) ?usize {
    return std.mem.indexOf(u8, str, needle);
}

/// `needle` can be any UTF-8 string.
/// `str` is assumed to have `needle.len` additional bytes so that an offset buffer can loaded without bounds check.
fn indexOfSimdFixed(comptime VecSize: usize, str: []const u8, needle: []const u8) ?usize {
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
fn indexOfSimdRemain(comptime VecSize: usize, str_: []const u8, needle: []const u8) ?usize {
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
pub fn indexOf(str: []const u8, needle: []const u8) ?usize {
    if (comptime std.simd.suggestVectorLength(u8)) |VecSize| {
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

fn indexOfAsciiSetScalar(str: []const u8, set: []const u8) ?usize {
    for (str, 0..) |code, i| {
        if (indexOfChar(set, code) != null) {
            return i;
        }
    }
    return null;
}

pub fn indexOfAsciiSetSimdRemain(comptime VecSize: usize, str: []const u8, set: []const u8) ?usize {
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

pub fn indexOfAsciiSetSimdFixed(comptime VecSize: usize, str: []const u8, set: []const u8) ?usize {
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

pub fn indexOfAsciiSet(str: []const u8, set: []const u8) ?usize {
    if (comptime std.simd.suggestVectorLength(u8)) |VecSize| {
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
pub fn indexOfChar(buf: []const u8, needle: u8) ?usize {
    // SIMD is approx 5x faster than scalar.
    if (comptime std.simd.suggestVectorLength(u8)) |VecSize| {
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

pub fn toUtf8CharIdx(str: []const u8, idx: usize) usize {
    var charIdx: usize = 0;
    var i: usize = 0;
    while (i < idx) {
        const cpLen = std.unicode.utf8ByteSequenceLength(str[i]) catch cy.fatal();
        i += cpLen;
        charIdx += 1;
    }
    return charIdx;
}

pub fn charIndexOfCodepoint(str: []const u8, needle: u21) ?usize {
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

fn indexOfNewLineCpu(buf: []const u8) ?usize {
    for (buf, 0..) |ch, i| {
        if (ch == '\n' or ch == '\r') {
            return i;
        }
    }
    return null;
}

test "indexOfNewLineCpu()" {
    const str = "abcxyz\nfoobar\rdeadbeef\r\nzzz";
    try t.eq(indexOfNewLineCpu(str).?, 6);
    try t.eq(indexOfNewLineCpu(str[7..]).?, 6);
    try t.eq(indexOfNewLineCpu(str[14..]).?, 8);
    try t.eq(indexOfNewLineCpu(str[24..]), null);
}

pub fn indexOfNewLine(buf: []const u8) ?usize {
    if (comptime std.simd.suggestVectorLength(u8)) |VecSize| {
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
            if (bitIdx < VecSize) {
                // Found.
                return i + bitIdx;
            }
        }
        if (i < buf.len) {
            // Remaining use cpu.
            if (indexOfNewLineCpu(buf[i..])) |res| {
                return i + res;
            }
        }
        return null;
    } else {
        return indexOfNewLineCpu(buf);
    }
}

/// Like std.mem.replacementSize but also records the idxes.
pub fn prepReplacement(str: []const u8, needle: []const u8, replacement: []const u8, idxesWriter: anytype) !usize {
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

pub fn replaceAtIdxes(dst: []u8, src: []const u8, needleLen: u32, replacement: []const u8, idxes: []const u32) void {
    var dstIdx: usize = 0;
    var srcIdx: usize = 0;
    for (idxes) |idx| {
        // First copy segment between last `idx` and this `idx`.
        const preSegmentLen = idx - srcIdx;
        @memcpy(dst[dstIdx..dstIdx + preSegmentLen], src[srcIdx..idx]);
        dstIdx += preSegmentLen;

        // Copy replacement.
        @memcpy(dst[dstIdx..dstIdx + replacement.len], replacement);
        dstIdx += replacement.len;
        srcIdx = idx + needleLen;
    }
    // Copy end segment.
    @memcpy(dst[dstIdx..], src[srcIdx..]);
}

pub fn utf8CodeAtNoCheck(str: []const u8, idx: usize) u21 {
    const len = std.unicode.utf8ByteSequenceLength(str[idx]) catch cy.fatal();
    return std.unicode.utf8Decode(str[0..len]) catch cy.fatal();
}

pub fn runeSliceAt(str: []const u8, idx: usize, out_bad_byte: *bool) []const u8 {
    const len = std.unicode.utf8ByteSequenceLength(str[idx]) catch {
        out_bad_byte.* = true;
        return str[idx..idx+1];
    };
    out_bad_byte.* = false;
    return str[idx..idx+len];
}

const ReplacementCharSlice = &.{0xEF, 0xBF, 0xBD};

pub fn getStaticUstringHeader(vm: *cy.VM, start: usize) *align(1) cy.StaticUstringHeader {
    return @ptrCast(vm.strBuf.ptr + start - 12);
}