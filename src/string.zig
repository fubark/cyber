const std = @import("std");
const stdx = @import("stdx");
const cy = @import("cyber.zig");

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
            .structId = cy.AstringT,
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

    fn getHeapObject(self: *const HeapStringBuilder) *cy.HeapObject {
        return @ptrCast(*cy.HeapObject, @alignCast(@alignOf(cy.HeapObject), self.buf.ptr - @offsetOf(cy.Astring, "bufStart")));
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
        self.len += @intCast(u32, str.len);
        std.mem.copy(u8, self.buf[oldLen..self.len], str);
        if (self.isAstring and utf8) {
            // Upgrade to Ustring.
            const obj = self.getHeapObject();
            obj.common.structId = cy.UstringT;
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
                const obj = @ptrCast(*cy.HeapObject, objSlice.ptr);
                obj.astring = .{
                    .structId = if (self.isAstring) cy.AstringT else cy.UstringT,
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
            const obj = @ptrCast(*cy.HeapObject, objSlice.ptr);
            obj.astring = .{
                .structId = if (self.isAstring) cy.AstringT else cy.UstringT,
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
        const obj = try vm.allocPoolObject();
        obj.rawstring = .{
            .structId = cy.RawStringT,
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
        return @ptrCast(*cy.HeapObject, @alignCast(@alignOf(cy.HeapObject), self.buf.ptr - cy.RawString.BufOffset));
    }

    pub fn deinit(self: *HeapRawStringBuilder) void {
        if (self.hasObject) {
            const obj = self.getHeapObject();
            obj.rawstring.len = self.len;
            self.vm.freeObject(obj);
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
        self.len += @intCast(u32, str.len);
        std.mem.copy(u8, self.buf[oldLen..self.len], str);
    }

    pub inline fn ensureTotalCapacity(self: *HeapRawStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        if (newCap > self.buf.len) {
            try self.growTotalCapacity(alloc, newCap);
        }
    }

    pub fn growTotalCapacityPrecise(self: *HeapRawStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        if (self.buf.len < cy.MaxPoolObjectRawStringByteLen) {
            const oldHead = @alignCast(@alignOf(cy.HeapObject), (self.buf.ptr - cy.RawString.BufOffset)[0..self.buf.len + cy.RawString.BufOffset]);
            if (alloc.resize(oldHead, cy.RawString.BufOffset + newCap)) {
                self.buf.len = newCap;
            } else {
                const old = self.buf;
                const objSlice = try alloc.alignedAlloc(u8, @alignOf(cy.HeapObject), cy.RawString.BufOffset + newCap);
                const obj = @ptrCast(*cy.HeapObject, objSlice.ptr);
                obj.rawstring = .{
                    .structId = cy.RawStringT,
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
            const obj = @ptrCast(*cy.HeapObject, objSlice.ptr);
            obj.rawstring = .{
                .structId = cy.RawStringT,
                .rc = 1,
                .len = 0,
                .bufStart = undefined,
            };
            self.buf = objSlice[cy.RawString.BufOffset..cy.RawString.BufOffset+newCap];
            std.mem.copy(u8, self.buf[0..self.len], old[0..self.len]);

            // Free pool object.
            oldObj.rawstring.len = self.len;
            self.vm.freeObject(oldObj);
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

pub fn ustringSeekCharIndexSliceAt(str: []const u8, seekIdx: u32, seekCharIdx: u32, charIdx: u32) linksection(cy.Section) stdx.IndexSlice(u32) {
    var iter = std.unicode.Utf8Iterator{
        .bytes = str,
        .i = 0,
    };
    var curCharIdx: u32 = 0;
    if (charIdx >= seekCharIdx) {
        iter.i = seekIdx;
        curCharIdx = seekCharIdx;
    }
    while (true) {
        const start = @intCast(u32, iter.i);
        _ = iter.nextCodepointSlice();
        if (curCharIdx == charIdx) {
            return stdx.IndexSlice(u32).init(start, @intCast(u32, iter.i));
        } else {
            curCharIdx += 1;
        }
    }
    stdx.fatal();
}

fn indexOfCharScalar(buf: []const u8, needle: u8) linksection(cy.Section) ?usize {
    for (buf) |ch, i| {
        if (ch == needle) {
            return i;
        }
    }
    return null;
}

/// For Ascii needle.
pub fn indexOfChar(buf: []const u8, needle: u8) linksection(cy.Section) ?usize {
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        const MaskInt = std.meta.Int(.unsigned, VecSize);
        var vbuf: @Vector(VecSize, u8) = undefined;
        const vneedle: @Vector(VecSize, u8) = @splat(VecSize, needle);
        var i: usize = 0;
        while (i + VecSize <= buf.len) : (i += VecSize) {
            vbuf = buf[i..i+VecSize][0..VecSize].*;
            const hitMask = @bitCast(MaskInt, vbuf == vneedle);
            const bitIdx = @ctz(hitMask);
            if (bitIdx < VecSize) {
                // Found.
                return i + bitIdx;
            }
        }
        if (i < buf.len) {
            // Remaining use cpu.
            if (indexOfCharScalar(buf[i..], needle)) |res| {
                return i + res;
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
        const cpLen = std.unicode.utf8ByteSequenceLength(str[i]) catch stdx.fatal();
        i += cpLen;
        charIdx += 1;
    }
    return charIdx;
}

pub fn charIndexOfCodepoint(str: []const u8, needle: u21) linksection(cy.Section) ?usize {
    var charIdx: usize = 0;
    var i: usize = 0;
    while (i < str.len) {
        const cpLen = std.unicode.utf8ByteSequenceLength(str[i]) catch stdx.fatal();
        const cp = std.unicode.utf8Decode(str[i..i+cpLen]) catch stdx.fatal();
        if (cp == needle) {
            return charIdx;
        }
        i += cpLen;
        charIdx += 1;
    }
    return null;
}

fn getLineEndCpu(buf: []const u8) linksection(cy.StdSection) ?usize {
    for (buf) |ch, i| {
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

pub fn getLineEnd(buf: []const u8) linksection(cy.StdSection) ?usize {
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        const MaskInt = std.meta.Int(.unsigned, VecSize);
        var vbuf: @Vector(VecSize, u8) = undefined;
        const lfNeedle: @Vector(VecSize, u8) = @splat(VecSize, @as(u8, '\n'));
        const crNeedle: @Vector(VecSize, u8) = @splat(VecSize, @as(u8, '\r'));
        var i: usize = 0;
        while (i + VecSize <= buf.len) : (i += VecSize) {
            vbuf = buf[i..i+VecSize][0..VecSize].*;
            const lfHits = @bitCast(MaskInt, vbuf == lfNeedle);
            const crHits = @bitCast(MaskInt, vbuf == crNeedle);
            const bitIdx = @ctz(lfHits | crHits);
            if (bitIdx < VecSize) {
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
            _ = try idxesWriter.write(std.mem.asBytes(&@intCast(u32, i)));
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