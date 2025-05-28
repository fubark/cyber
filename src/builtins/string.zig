const std = @import("std");
const cy = @import("../cyber.zig");
const rt = cy.rt;
const Value = cy.Value;
const fatal = cy.fatal;
const bindings = @import("bindings.zig");
const string = cy.string;

pub fn concat(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const stype = obj.getType();
    const str = obj.getSlice();
    if (stype.isAstring()) {
        const obj2 = vm.getObject(*cy.heap.String, 1);
        const rstr = obj2.getSlice();
        if (obj2.getType().isAstring()) {
            return vm.allocAstringConcat(str, rstr) catch fatal();
        } else {
            return vm.allocUstringConcat(str, rstr) catch fatal();
        }
    } else {
        const obj2 = vm.getObject(*cy.heap.String, 1);
        const rstr = obj2.getSlice();
        return vm.allocUstringConcat(str, rstr) catch fatal();
    }
}

pub fn sliceFn(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const stype = obj.getType();
    const str = obj.getSlice();
    const parent = obj.getParentByType(stype);

    const range = vm.getObject(*cy.heap.Range, 1);
    if (range.start < 0) {
        return rt.prepThrowError(vm, .OutOfBounds);
    }
    if (range.end > str.len) {
        return rt.prepThrowError(vm, .OutOfBounds);
    }
    if (range.end < range.start) {
        return rt.prepThrowError(vm, .OutOfBounds);
    }

    const ustart: u32 = @intCast(range.start);
    const uend: u32 = @intCast(range.end);
    if (stype.isAstring()) {
        vm.retainObject(parent);
        return vm.allocAstringSlice(str[ustart..uend], parent) catch fatal();
    } else {
        vm.retainObject(parent);
        if (cy.string.isAstring(str[ustart..uend])) {
            return vm.allocAstringSlice(str[ustart..uend], parent) catch fatal();
        } else {
            return vm.allocUstringSlice(str[ustart..uend], parent) catch fatal();
        }
    }
}

pub fn insertByte(vm: *cy.VM) anyerror!Value {
    const str = vm.getObject(*cy.heap.String, 0);
    const slice = str.getSlice();
    const idx = try cy.builtins.intAsIndex(vm.getInt(1), slice.len + 1);
    const byte = vm.getByte(2);
    if (str.getType().isAstring()) {
        if (byte < 128) {
            return vm.allocAstringConcat3(slice[0..idx], &.{ byte }, slice[idx..]);
        } else {
            return vm.allocUstringConcat3(slice[0..idx], &.{ byte }, slice[idx..]);
        }
    } else {
        return vm.allocUstringConcat3(slice[0..idx], &.{byte}, slice[idx..]);
    }
}

pub fn insertFn(vm: *cy.VM) anyerror!Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const idx = vm.getInt(1);
    if (idx < 0 or idx > str.len) {
        return error.OutOfBounds;
    }
    const uidx: u32 = @intCast(idx);
    const stype = obj.getType();
    if (stype.isAstring()) {
        const obj2 = vm.getObject(*cy.heap.String, 2);
        const insert = obj2.getSlice();
        if (obj2.getType().isAstring()) {
            return vm.allocAstringConcat3(str[0..uidx], insert, str[uidx..]) catch fatal();
        } else {
            return vm.allocUstringConcat3(str[0..uidx], insert, str[uidx..]) catch fatal();
        }
    } else {
        const insert = vm.getString(2);
        return vm.allocUstringConcat3(str[0..uidx], insert, str[uidx..]) catch fatal();
    }
}

pub fn find(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const needle = vm.getString(1);
    if (needle.len > 0 and needle.len <= str.len) {
        if (needle.len == 1) {
            // One ascii char special case. Perform indexOfChar.
            if (string.indexOfChar(str, needle[0])) |idx| {
                return intSome(vm, @intCast(idx)) catch cy.fatal();
            }
        }
        if (string.indexOf(str, needle)) |idx| {
            return intSome(vm, @intCast(idx)) catch cy.fatal();
        }
    }
    return intNone(vm) catch cy.fatal();
}

pub fn startsWith(vm: *cy.VM) Value {
    const str = vm.getString(0);
    const needle = vm.getString(1);
    return Value.initBool(std.mem.startsWith(u8, str, needle));
}


pub fn endsWith(vm: *cy.VM) Value {
    const str = vm.getString(0);
    const needle = vm.getString(1);
    return Value.initBool(std.mem.endsWith(u8, str, needle));
}


pub fn isAscii(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const stype = obj.getType();
    return Value.initBool(stype.isAstring());
}

const intSome = cy.builtins.intSome;
const intNone = cy.builtins.intNone;

pub fn findAnyRune(vm: *cy.VM) anyerror!Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const set_list = vm.getObject(*cy.heap.List, 1).items();
    const stype = obj.getType();

    const tempBuf = &vm.u8Buf;
    tempBuf.clearRetainingCapacity();
    defer tempBuf.ensureMaxCapOrClear(vm.alloc, 4096) catch fatal();

    var set_is_ascii = true;
    for (set_list) |rune| {
        if (rune.val >= 0 and rune.val < 128) {
            try tempBuf.append(vm.alloc, @intCast(rune.val));
        } else {
            set_is_ascii = false;
        }
    }
    const set_str = tempBuf.items();

    if (set_list.len > 0) {
        if (stype.isAstring()) {
            if (set_is_ascii) {
                if (@call(.never_inline, string.indexOfAsciiSet, .{str, set_str})) |idx| {
                    return intSome(vm, @intCast(idx));
                }
            } else {
                // Reduce search set to just ASCII codepoints.
                if (set_str.len > 0) {
                    if (string.indexOfAsciiSet(str, set_str)) |idx| {
                        return intSome(vm, @intCast(idx));
                    }
                }
            }
        } else {
            if (set_is_ascii) {
                if (string.indexOfAsciiSet(str, set_str)) |idx| {
                    return intSome(vm, @intCast(idx));
                }
            } else {
                // Scalar.
                var idx: usize = 0;
                var iter = std.unicode.Utf8Iterator{
                    .bytes = str,
                    .i = 0,
                };
                while (true) {
                    idx = iter.i;
                    const cp = iter.nextCodepoint() orelse {
                        break;
                    };
                    for (set_list) |set_rune| {
                        if (set_rune.val == cp) {
                            return intSome(vm, @intCast(idx));
                        }
                    }
                }
            }
        }
    }
    return intNone(vm);
}

pub fn findRune(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const needle = vm.getInt(1);
    const stype = obj.getType(); 

    if (needle > 0) {
        const code: u21 = @intCast(needle);
        const needleIsAscii = code < 128;
        if (stype.isAstring()) {
            if (needleIsAscii) {
                if (string.indexOfChar(str, @intCast(code))) |idx| {
                    return intSome(vm, @intCast(idx)) catch cy.fatal();
                }
            }
        } else {
            if (needleIsAscii) {
                if (string.indexOfChar(str, @intCast(code))) |idx| {
                    return intSome(vm, @intCast(idx)) catch cy.fatal();
                }
            } else {
                var slice: [4]u8 = undefined;
                _ = std.unicode.utf8Encode(code, &slice) catch fatal();
                if (string.indexOf(str, &slice)) |idx| {
                    return intSome(vm, @intCast(idx)) catch cy.fatal();
                }
            }
        }
    }
    return intNone(vm) catch cy.fatal();
}

pub fn upper(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const stype = obj.getType();
    if (stype.isAstring()) {
        const new = vm.allocUnsetAstringObject(str.len) catch fatal();
        const newBuf = new.astring.getMutSlice();
        _ = std.ascii.upperString(newBuf, str);
        return vm.allocOwnedAstring(new) catch fatal();
    } else {
        const new = vm.allocUnsetUstringObject(str.len) catch fatal();
        const newBuf = new.ustring.getMutSlice();
        _ = std.ascii.upperString(newBuf, str);
        return vm.allocOwnedUstring(new) catch fatal();
    }
}

pub fn lower(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const stype = obj.getType();
    if (stype.isAstring()) {
        const new = vm.allocUnsetAstringObject(str.len) catch fatal();
        const newBuf = new.astring.getMutSlice();
        _ = std.ascii.lowerString(newBuf, str);
        return vm.allocOwnedAstring(new) catch fatal();
    } else {
        const new = vm.allocUnsetUstringObject(str.len) catch fatal();
        const newBuf = new.ustring.getMutSlice();
        _ = std.ascii.lowerString(newBuf, str);
        return vm.allocOwnedUstring(new) catch fatal();
    }
}

pub fn less(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const right = vm.getString(1);
    return Value.initBool(std.mem.lessThan(u8, str, right));
}

pub fn lenFn(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    return Value.initInt(obj.len());
}

pub fn count(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const stype = obj.getType();
    if (stype.isAstring()) {
        return Value.initInt(obj.len());
    } else {
        return Value.initInt(@intCast(cy.string.countRunes(obj.getSlice())));
    }
}

pub fn seek(vm: *cy.VM) anyerror!Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const idx = vm.getInt(1);
    const stype = obj.getType();
    if (stype.isAstring()) {
        if (idx < 0 or idx >= str.len) {
            return error.OutOfBounds;
        }
        return Value.initInt(@intCast(idx));
    } else {
        const uidx: u32 = @intCast(idx);
        var bad_byte: bool = undefined;
        const start: u32 = @intCast(try string.ustringSeekByRuneIndex(str, 0, 0, uidx, &bad_byte));
        return Value.initInt(@intCast(start));
    }
}

pub fn runeAt(vm: *cy.VM) anyerror!Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const idx = vm.getInt(1);
    if (idx < 0 or idx >= str.len) {
        return error.OutOfBounds;
    }
    const stype = obj.getType();
    if (stype.isAstring()) {
        return Value.initInt(@intCast(str[@intCast(idx)]));
    } else {
        const uidx: u32 = @intCast(idx);
        const rune_len = std.unicode.utf8ByteSequenceLength(str[uidx]) catch {
            return Value.initInt(@intCast(std.unicode.replacement_character));
        };
        if (uidx + rune_len > str.len) {
            return Value.initInt(@intCast(std.unicode.replacement_character));
        }
        const rune = std.unicode.utf8Decode(str[uidx..uidx+rune_len]) catch {
            return Value.initInt(@intCast(std.unicode.replacement_character));
        };
        return Value.initInt(@intCast(rune));
    }
}

pub fn sliceAt(vm: *cy.VM) anyerror!Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const stype = obj.getType();
    const idx = vm.getInt(1);

    if (idx < 0 or idx >= str.len) {
        return error.OutOfBounds;
    }
    const uidx: u32 = @intCast(idx);
    if (stype.isAstring()) {
        // TODO: return slice.
        return vm.retainOrAllocAstring(str[uidx..uidx + 1]) catch fatal();
    } else {
        var bad_byte: bool = undefined;
        const slice = string.runeSliceAt(str, uidx, &bad_byte);

        if (slice.len == 1 and !bad_byte) {
            // TODO: If single ascii runes are interned by default,
            //       it's better to fetch that instead to reduce dependency on parent.
            vm.retainObject(@ptrCast(obj));
            return vm.allocAstringSlice(slice, @ptrCast(obj)) catch fatal();
        } else {
            vm.retainObject(@ptrCast(obj));
            return vm.allocUstringSlice(slice, @ptrCast(obj)) catch fatal();
        }
    }
}

pub fn trim(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const stype = obj.getType();

    const trimRunes = vm.getString(2);

    var res: []const u8 = undefined;
    const mode = bindings.getBuiltinSymbol(vm.getSymbol(1)) orelse {
        return rt.prepThrowError(vm, .InvalidArgument);
    };
    switch (mode) {
        .left => res = std.mem.trimLeft(u8, str, trimRunes),
        .right => res = std.mem.trimRight(u8, str, trimRunes),
        .ends => res = std.mem.trim(u8, str, trimRunes),
        else => {
            return rt.prepThrowError(vm, .InvalidArgument);
        }
    }

    vm.retainObject(@ptrCast(obj));
    if (stype.isAstring()) {
        return vm.allocAstringSlice(res, @ptrCast(obj)) catch fatal();
    } else {
        return vm.allocUstringSlice(res, @ptrCast(obj)) catch fatal();
    }
}

pub fn stringReplace(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const stype = obj.getType();

    const needle = vm.getObject(*cy.heap.String, 1);
    const replace = vm.getObject(*cy.heap.String, 2);

    if (stype.isAstring()) {
        if (astringReplace(vm, str, needle, replace)) |val| {
            return val;
        } else {
            vm.retainObject(@ptrCast(obj));
            return Value.initNoCycPtr(obj);
        }
    } else {
        if (ustringReplace(vm, str, needle, replace)) |val| {
            return val;
        } else {
            vm.retainObject(@ptrCast(obj));
            return Value.initNoCycPtr(obj);
        }
    }
}

fn astringReplace(vm: *cy.VM, str: []const u8, needlev: *cy.heap.String, replacev: *cy.heap.String) ?Value {
    const needle = needlev.getSlice();
    const replacement = replacev.getSlice();

    const idxBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
    idxBuf.clearRetainingCapacity();
    defer idxBuf.ensureMaxCapOrClear(vm.alloc, 4096) catch fatal();
    const newLen = string.prepReplacement(str, needle, replacement, idxBuf.writer(vm.alloc)) catch fatal();
    const numIdxes = @divExact(idxBuf.len, 4);
    if (numIdxes > 0) {
        if (replacev.getType().isAstring()) {
            const new = vm.allocUnsetAstringObject(newLen) catch fatal();
            const newBuf = new.astring.getMutSlice();
            const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
            string.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
            return vm.allocOwnedAstring(new) catch fatal();
        } else {
            const new = vm.allocUnsetUstringObject(newLen) catch fatal();
            const newBuf = new.ustring.getMutSlice();
            const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
            string.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
            return vm.allocOwnedUstring(new) catch fatal();
        }
    } else {
        return null;
    }
}

fn ustringReplace(vm: *cy.VM, str: []const u8, needlev: *cy.heap.String, replacev: *cy.heap.String) ?Value {
    const needle = needlev.getSlice();
    const replacement = replacev.getSlice();

    const idxBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
    idxBuf.clearRetainingCapacity();
    defer idxBuf.ensureMaxCapOrClear(vm.alloc, 4096) catch fatal();
    const newLen = string.prepReplacement(str, needle, replacement, idxBuf.writer(vm.alloc)) catch fatal();
    const numIdxes = @divExact(idxBuf.len, 4);
    if (numIdxes > 0) {
        const new = vm.allocUnsetUstringObject(newLen) catch fatal();
        const newBuf = new.ustring.getMutSlice();
        const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
        cy.string.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
        return vm.allocOwnedUstring(new) catch fatal();
    } else {
        return null;
    }
}

pub fn repeat(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();

    const n = vm.getInt(1);
    if (n < 0) {
        return rt.prepThrowError(vm, .InvalidArgument);
    }

    const un: u32 = @intCast(n);
    const len = un * str.len;
    if (un > 1 and len > 0) {
        var new: *cy.HeapObject = undefined;
        var buf: []u8 = undefined;
        const stype = obj.getType();
        if (stype.isAstring()) {
            new = vm.allocUnsetAstringObject(len) catch fatal();
            buf = new.astring.getMutSlice();
        } else {
            new = vm.allocUnsetUstringObject(len) catch fatal();
            buf = new.ustring.getMutSlice();
        }
        // This is already quite fast since it has good cache locality.
        // Might be faster if the front of the buffer up to a certain size was used to memcpy instead of just 1 `str`.
        var i: u32 = 0;
        var dst: u32 = 0;
        while (i < un) : (i += 1) {
            @memcpy(buf[dst..dst + str.len], str);
            dst += @intCast(str.len);
        }

        return Value.initNoCycPtr(new);
    } else {
        if (un == 0) {
            vm.retain(vm.emptyString);
            return vm.emptyString;
        } else {
            vm.retainObject(@ptrCast(obj));
            return Value.initNoCycPtr(obj);
        }
    }
}

pub fn split(vm: *cy.VM) anyerror!Value {
    const obj = vm.getObject(*cy.heap.String, 0);
    const str = obj.getSlice();
    const stype = obj.getType();
    const parent = obj.getParentByType(stype);
    const delim = vm.getString(1);

    const res = try vm.allocEmptyListDyn();
    if (delim.len == 0) {
        return res;
    }
    const list = res.asHeapObject();

    var iter = std.mem.splitSequence(u8, str, delim);
    while (iter.next()) |part| {
        if (stype.isAstring()) {
            vm.retainObject(parent);
            const partv = try vm.allocAstringSlice(part, parent);
            try list.list.append(vm.alloc, partv);
        } else {
            vm.retainObject(parent);
            const partv = try vm.allocUstringSlice(part, parent);
            try list.list.append(vm.alloc, partv);
        }
    }
    return res;
}

pub fn stringCall(vm: *cy.VM) anyerror!Value {
    const val = vm.getValue(0);
    if (val.isString()) {
        vm.retain(val);
        return val; 
    } else {
        const str = try vm.getOrBufPrintValueStr(&cy.tempBuf, val);
        return vm.retainOrAllocAstring(str);
    }
}