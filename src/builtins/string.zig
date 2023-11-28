const std = @import("std");
const cy = @import("../cyber.zig");
const Value = cy.Value;
const fatal = cy.fatal;
const bindings = @import("bindings.zig");
const prepareThrowSymbol = bindings.prepareThrowSymbol;

pub fn concat(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const stype = obj.string.getType();
    const str = obj.string.getSlice();
    if (stype.isAstring()) {
        var rcharLen: u32 = undefined;
        const rstr = args[1].asString2(&rcharLen);
        if (rcharLen == rstr.len) {
            return vm.allocAstringConcat(str, rstr) catch fatal();
        } else {
            return vm.allocUstringConcat(str, rstr, @intCast(str.len + rcharLen)) catch fatal();
        }
    } else {
        var rcharLen: u32 = undefined;
        const rstr = args[1].asString2(&rcharLen);
        const charLen = obj.string.getUstringCharLen();
        return vm.allocUstringConcat(str, rstr, charLen + rcharLen) catch fatal();
    }
}

pub fn sliceFn(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const stype = obj.string.getType();
    const str = obj.string.getSlice();
    const parent = obj.string.getParentByType(stype);

    var start: i48 = undefined;
    if (args[1].isNone()) {
        start = 0;
    } else if (args[1].isInteger()) {
        start = args[1].asInteger();
    } else {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }
    if (start < 0) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    }

    if (stype.isAstring()) {
        var end: i48 = undefined;
        if (args[2].isNone()) {
            end = @intCast(str.len);
        } else if (args[2].isInteger()) {
            end = args[2].asInteger();
        } else {
            return prepareThrowSymbol(vm, .InvalidArgument);
        }
        if (end > str.len) {
            return prepareThrowSymbol(vm, .OutOfBounds);
        }
        if (end < start) {
            return prepareThrowSymbol(vm, .OutOfBounds);
        }
        const ustart: u32 = @intCast(start);
        const uend: u32 = @intCast(end);

        vm.retainObject(parent);
        return vm.allocAstringSlice(str[ustart..uend], parent) catch fatal();
    } else {
        const charLen = obj.string.getUstringCharLen();
        var end: i48 = undefined;
        if (args[2].isNone()) {
            end = @intCast(charLen);
        } else if (args[2].isInteger()) {
            end = args[2].asInteger();
        } else {
            return prepareThrowSymbol(vm, .InvalidArgument);
        }
        if (end > charLen) {
            return prepareThrowSymbol(vm, .OutOfBounds);
        }
        if (end < start) {
            return prepareThrowSymbol(vm, .OutOfBounds);
        }
        const ustart: u32 = @intCast(start);
        const uend: u32 = @intCast(end);

        const mru = obj.string.getUstringMruChar();
        const startByteIdx: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, ustart));
        obj.string.setUstringMruChar(ustart, startByteIdx);
        const endByteIdx: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, startByteIdx, ustart, uend));

        vm.retainObject(parent);
        return vm.allocUstringSlice(str[startByteIdx..endByteIdx], uend - ustart, parent) catch fatal();
    }
}

pub fn insertFn(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const idx = args[1].asInteger();
    const stype = obj.string.getType();
    if (stype.isAstring()) {
        if (idx < 0 or idx > str.len) {
            return prepareThrowSymbol(vm, .OutOfBounds);
        }
        var insertCharLen: u32 = undefined;
        const insert = args[2].asString2(&insertCharLen);
        const uidx: u32 = @intCast(idx);
        if (insertCharLen == insert.len) {
            return vm.allocAstringConcat3(str[0..uidx], insert, str[uidx..]) catch fatal();
        } else {
            return vm.allocUstringConcat3(str[0..uidx], insert, str[uidx..], @intCast(str.len + insertCharLen)) catch fatal();
        }
    } else {
        const charLen = obj.string.getCharLen();
        if (idx < 0 or idx > charLen) {
            return prepareThrowSymbol(vm, .OutOfBounds);
        }
        var insertCharLen: u32 = undefined;
        const insert = args[2].asString2(&insertCharLen);
        const uidx: u32 = @intCast(idx);
        const mru = obj.string.getUstringMruChar();
        const start: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));

        obj.string.setUstringMruChar(uidx, start);
        return vm.allocUstringConcat3(str[0..start], insert, str[start..], @intCast(charLen + insertCharLen)) catch fatal();
    }
}

pub fn find(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const needle = args[1].asString();
    if (needle.len > 0 and needle.len <= str.len) {
        if (needle.len == 1) {
            // One ascii char special case. Perform indexOfChar.
            if (cy.string.indexOfChar(str, needle[0])) |idx| {
                const stype = obj.string.getType();
                if (stype.isUstring()) {
                    const charIdx = cy.toUtf8CharIdx(str, idx);
                    return Value.initInt(@intCast(charIdx));
                } else {
                    return Value.initInt(@intCast(idx));
                }
            }
        }
        if (cy.string.indexOf(str, needle)) |idx| {
            const stype = obj.string.getType();
            if (stype.isUstring()) {
                const charIdx = cy.toUtf8CharIdx(str, idx);
                return Value.initInt(@intCast(charIdx));
            } else {
                return Value.initInt(@intCast(idx));
            }
        }
    }
    return Value.None;
}

pub fn startsWith(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const needle = args[1].asString();
    return Value.initBool(std.mem.startsWith(u8, str, needle));
}


pub fn endsWith(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const str = args[0].asHeapObject().string.getSlice();
    const needle = args[1].asString();
    return Value.initBool(std.mem.endsWith(u8, str, needle));
}


pub fn isAscii(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const stype = obj.string.getType();
    return Value.initBool(stype.isAstring());
}

pub fn findAnyRune(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    var setCharLen: u32 = undefined;
    const set = args[1].asString2(&setCharLen);
    const setIsAscii = setCharLen == set.len;
    const stype = obj.string.getType();

    if (set.len > 0) {
        if (stype.isAstring()) {
            if (setIsAscii) {
                if (@call(.never_inline, cy.indexOfAsciiSet, .{str, set})) |idx| {
                    return Value.initInt(@intCast(idx));
                }
            } else {
                // Reduce search set to just ASCII codepoints.
                const alloc = vm.allocator();
                const tempBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
                tempBuf.clearRetainingCapacity();
                defer tempBuf.ensureMaxCapOrClear(alloc, 4096) catch fatal();
                var iter = std.unicode.Utf8Iterator{
                    .bytes = set,
                    .i = 0,
                };
                while (iter.nextCodepoint()) |cp| {
                    if (cp < 128) {
                        tempBuf.append(alloc, @intCast(cp)) catch fatal();
                    }
                }
                if (tempBuf.len > 0) {
                    if (cy.indexOfAsciiSet(str, tempBuf.items())) |idx| {
                        return Value.initInt(@intCast(idx));
                    }
                }
            }
        } else {
            if (setIsAscii) {
                if (cy.indexOfAsciiSet(str, set)) |idx| {
                    const charIdx = cy.toUtf8CharIdx(str, idx);
                    return Value.initInt(@intCast(charIdx));
                }
            } else {
                // Slow. Checks every utf8 code and applies min.
                var minIndex: u32 = cy.NullId;
                var iter = std.unicode.Utf8Iterator{
                    .bytes = set,
                    .i = 0,
                };
                while (iter.nextCodepoint()) |cp| {
                    if (cy.charIndexOfCodepoint(str, cp)) |idx| {
                        if (idx < minIndex) {
                            minIndex = @intCast(idx);
                        }
                    }
                }
                if (minIndex != cy.NullId) {
                    return Value.initInt(@intCast(minIndex));
                }
            }
        }
    }
    return Value.None;
}

pub fn findRune(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const needle = args[1].asInteger();
    const stype = obj.string.getType(); 

    if (needle > 0) {
        const code: u21 = @intCast(needle);
        const needleIsAscii = code < 128;
        if (stype.isAstring()) {
            if (needleIsAscii) {
                if (cy.string.indexOfChar(str, @intCast(code))) |idx| {
                    return Value.initInt(@intCast(idx));
                }
            }
        } else {
            if (needleIsAscii) {
                if (cy.string.indexOfChar(str, @intCast(code))) |idx| {
                    const charIdx = cy.toUtf8CharIdx(str, idx);
                    return Value.initInt(@intCast(charIdx));
                }
            } else {
                var slice: [4]u8 = undefined;
                _ = std.unicode.utf8Encode(code, &slice) catch fatal();
                if (cy.string.indexOf(str, &slice)) |idx| {
                    const charIdx = cy.toUtf8CharIdx(str, idx);
                    return Value.initInt(@intCast(charIdx));
                }
            }
        }
    }
    return Value.None;
}

pub fn upper(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();
    if (stype.isAstring()) {
        const new = vm.allocUnsetAstringObject(str.len) catch fatal();
        const newBuf = new.astring.getMutSlice();
        _ = std.ascii.upperString(newBuf, str);
        return vm.allocOwnedAstring(new) catch fatal();
    } else {
        const new = vm.allocUnsetUstringObject(str.len, obj.string.getUstringCharLen()) catch fatal();
        const newBuf = new.ustring.getMutSlice();
        _ = std.ascii.upperString(newBuf, str);
        return vm.allocOwnedUstring(new) catch fatal();
    }
}

pub fn lower(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();
    if (stype.isAstring()) {
        const new = vm.allocUnsetAstringObject(str.len) catch fatal();
        const newBuf = new.astring.getMutSlice();
        _ = std.ascii.lowerString(newBuf, str);
        return vm.allocOwnedAstring(new) catch fatal();
    } else {
        const new = vm.allocUnsetUstringObject(str.len, obj.string.getUstringCharLen()) catch fatal();
        const newBuf = new.ustring.getMutSlice();
        _ = std.ascii.lowerString(newBuf, str);
        return vm.allocOwnedUstring(new) catch fatal();
    }
}

pub fn less(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const right = args[1].asString();
    return Value.initBool(std.mem.lessThan(u8, str, right));
}

pub fn lenFn(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(obj.string.getCharLen());
}

pub fn sliceAt(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();
    var idx = args[1].asInteger();

    if (stype.isAstring()) {
        if (idx < 0) {
            idx = @as(i48, @intCast(str.len)) + idx;
        }
        if (idx < 0 or idx >= str.len) {
            return prepareThrowSymbol(vm, .OutOfBounds);
        }
        const uidx: u32 = @intCast(idx);
        // TODO: return slice.
        return vm.retainOrAllocAstring(str[uidx..uidx + 1]) catch fatal();
    } else {
        const charLen = obj.string.getUstringCharLen();
        if (idx < 0) {
            idx = @as(i48, @intCast(charLen)) + idx;
        }
        if (idx < 0 or idx >= charLen) {
            return prepareThrowSymbol(vm, .OutOfBounds);
        }
        const uidx: u32 = @intCast(idx);
        const mru = obj.string.getUstringMruChar();
        const start: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));
        const slice = cy.string.utf8CharSliceAtNoCheck(str, start);

        obj.string.setUstringMruChar(uidx, start);
        if (slice.len == 1) {
            return vm.retainOrAllocAstring(slice) catch fatal();
        } else {
            return vm.retainOrAllocUstring(slice, 1) catch fatal();
        }
    }
}

pub fn runeAt(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const idx = args[1].asInteger();
    const stype = obj.string.getType();
    if (stype.isAstring()) {
        if (idx < 0 or idx >= str.len) {
            return prepareThrowSymbol(vm, .OutOfBounds);
        }
        return Value.initInt(@intCast(str[@intCast(idx)]));
    } else {
        if (idx < 0 or idx >= obj.string.getUstringCharLen()) {
            return prepareThrowSymbol(vm, .OutOfBounds);
        }
        const uidx: u32 = @intCast(idx);
        const mru = obj.string.getUstringMruChar();
        const start: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));
        const slice = cy.string.utf8CharSliceAtNoCheck(str, start);

        const cp = std.unicode.utf8Decode(slice) catch cy.fatal();
        obj.string.setUstringMruChar(uidx, start);
        return Value.initInt(@intCast(cp));
    }
}

pub fn trim(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();

    const trimRunes = args[2].asString();

    var res: []const u8 = undefined;
    const mode = bindings.getBuiltinSymbol(args[1].asSymbolId()) orelse {
        return prepareThrowSymbol(vm, .InvalidArgument);
    };
    switch (mode) {
        .left => res = std.mem.trimLeft(u8, str, trimRunes),
        .right => res = std.mem.trimRight(u8, str, trimRunes),
        .ends => res = std.mem.trim(u8, str, trimRunes),
        else => {
            return prepareThrowSymbol(vm, .InvalidArgument);
        }
    }

    if (stype.isAstring()) {
        return vm.retainOrAllocAstring(res) catch fatal();
    } else {
        const runeLen: u32 = @intCast(cy.string.utf8Len(res));
        return vm.retainOrAllocUstring(res, runeLen) catch fatal();
    }
}

pub fn stringReplace(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();

    const needle = args[1].castHeapObject(*cy.heap.String);
    const replace = args[2].castHeapObject(*cy.heap.String);

    if (stype.isAstring()) {
        if (astringReplace(vm, str, needle, replace)) |val| {
            return val;
        } else {
            vm.retainObject(obj);
            return Value.initNoCycPtr(obj);
        }
    } else {
        if (ustringReplace(vm, str, needle, replace)) |val| {
            return val;
        } else {
            vm.retainObject(obj);
            return Value.initNoCycPtr(obj);
        }
    }
}

fn astringReplace(vm: *cy.UserVM, str: []const u8, needlev: *cy.heap.String, replacev: *cy.heap.String) linksection(cy.StdSection) ?Value {
    const needle = needlev.getSlice();
    const replacement = replacev.getSlice();
    const rcharLen = replacev.getCharLen();

    const idxBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
    idxBuf.clearRetainingCapacity();
    defer idxBuf.ensureMaxCapOrClear(vm.allocator(), 4096) catch fatal();
    const newLen = cy.prepReplacement(str, needle, replacement, idxBuf.writer(vm.allocator())) catch fatal();
    const numIdxes = @divExact(idxBuf.len, 4);
    if (numIdxes > 0) {
        if (rcharLen == replacement.len) {
            const new = vm.allocUnsetAstringObject(newLen) catch fatal();
            const newBuf = new.astring.getMutSlice();
            const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
            cy.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
            return vm.allocOwnedAstring(new) catch fatal();
        } else {
            const new = vm.allocUnsetUstringObject(newLen, @intCast(str.len + idxBuf.len * rcharLen - idxBuf.len * needle.len)) catch fatal();
            const newBuf = new.ustring.getMutSlice();
            const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
            cy.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
            return vm.allocOwnedUstring(new) catch fatal();
        }
    } else {
        return null;
    }
}

fn ustringReplace(vm: *cy.UserVM, str: []const u8, needlev: *cy.heap.String, replacev: *cy.heap.String) linksection(cy.StdSection) ?Value {
    var ncharLen: u32 = undefined;
    const needle = needlev.getSlice2(&ncharLen);
    var rcharLen: u32 = undefined;
    const replacement = replacev.getSlice2(&rcharLen);

    const idxBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
    idxBuf.clearRetainingCapacity();
    defer idxBuf.ensureMaxCapOrClear(vm.allocator(), 4096) catch fatal();
    const newLen = cy.prepReplacement(str, needle, replacement, idxBuf.writer(vm.allocator())) catch fatal();
    const numIdxes = @divExact(idxBuf.len, 4);
    if (numIdxes > 0) {
        const new = vm.allocUnsetUstringObject(newLen, @intCast(str.len + idxBuf.len * rcharLen - idxBuf.len * ncharLen)) catch fatal();
        const newBuf = new.ustring.getMutSlice();
        const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
        cy.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
        return vm.allocOwnedUstring(new) catch fatal();
    } else {
        return null;
    }
}

pub fn repeat(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();

    const n = args[1].asInteger();
    if (n < 0) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }

    const un: u32 = @intCast(n);
    const len = un * str.len;
    if (un > 1 and len > 0) {
        var new: *cy.HeapObject = undefined;
        var buf: []u8 = undefined;
        const stype = obj.string.getType();
        if (stype.isAstring()) {
            new = vm.allocUnsetAstringObject(len) catch fatal();
            buf = new.astring.getMutSlice();
        } else {
            const charLen = obj.string.getUstringCharLen();
            new = vm.allocUnsetUstringObject(len, charLen) catch fatal();
            buf = new.ustring.getMutSlice();
        }
        // This is already quite fast since it has good cache locality.
        // Might be faster if the front of the buffer up to a certain size was used to memcpy instead of just 1 `str`.
        var i: u32 = 0;
        var dst: u32 = 0;
        while (i < un) : (i += 1) {
            std.mem.copy(u8, buf[dst..dst + str.len], str);
            dst += @intCast(str.len);
        }

        return Value.initNoCycPtr(new);
    } else {
        if (un == 0) {
            const empty = vm.internal().emptyString;
            vm.retain(empty);
            return empty;
        } else {
            vm.retainObject(obj);
            return Value.initNoCycPtr(obj);
        }
    }
}

pub fn split(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();
    const parent = obj.string.getParentByType(stype);
    const delim = args[1].asString();

    const res = try vm.allocEmptyList();
    if (delim.len == 0) {
        return res;
    }
    const list = res.asHeapObject();

    var iter = std.mem.split(u8, str, delim);
    while (iter.next()) |part| {
        if (stype.isAstring()) {
            vm.retainObject(parent);
            const partv = try vm.allocAstringSlice(part, parent);
            try list.list.append(vm.alloc, partv);
        } else {
            vm.retainObject(parent);
            const runeLen: u32 = @intCast(cy.string.utf8Len(part));
            const partv = try vm.allocUstringSlice(part, runeLen, parent);
            try list.list.append(vm.alloc, partv);
        }
    }
    return res;
}

pub fn stringCall(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const val = args[0];
    if (val.isString()) {
        vm.retain(val);
        return val; 
    } else {
        var charLen: u32 = undefined;
        const str = try vm.getOrBufPrintValueStr2(&cy.tempBuf, val, &charLen);
        if (charLen == str.len) {
            return vm.retainOrAllocAstring(str);
        } else {
            return vm.retainOrAllocUstring(str, charLen);
        }
    }
}