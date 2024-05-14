const std = @import("std");
const cy = @import("../cyber.zig");
const rt = cy.rt;
const Value = cy.Value;
const fatal = cy.fatal;
const bindings = @import("bindings.zig");
const string = cy.string;

pub fn concat(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const stype = obj.string.getType();
    const str = obj.string.getSlice();
    if (stype.isAstring()) {
        const obj2 = args[1].asHeapObject();
        const rstr = obj2.string.getSlice();
        if (obj2.string.getType().isAstring()) {
            return vm.allocAstringConcat(str, rstr) catch fatal();
        } else {
            return vm.allocUstringConcat(str, rstr) catch fatal();
        }
    } else {
        const obj2 = args[1].asHeapObject();
        const rstr = obj2.string.getSlice();
        return vm.allocUstringConcat(str, rstr) catch fatal();
    }
}

pub fn sliceFn(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const stype = obj.string.getType();
    const str = obj.string.getSlice();
    const parent = obj.string.getParentByType(stype);

    const range = args[1].asHeapObject();

    var start: i48 = undefined;
    if (!range.range.has_start) {
        start = 0;
    } else {
        start = @intCast(range.range.start);
    }
    if (start < 0) {
        return rt.prepThrowError(vm, .OutOfBounds);
    }

    var end: i48 = undefined;
    if (!range.range.has_end) {
        end = @intCast(str.len);
    } else {
        end = @intCast(range.range.end);
    }
    if (end > str.len) {
        return rt.prepThrowError(vm, .OutOfBounds);
    }
    if (end < start) {
        return rt.prepThrowError(vm, .OutOfBounds);
    }

    const ustart: u32 = @intCast(start);
    const uend: u32 = @intCast(end);
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

pub fn insertFn(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const idx = args[1].asInteger();
    if (idx < 0 or idx > str.len) {
        return error.OutOfBounds;
    }
    const uidx: u32 = @intCast(idx);
    const stype = obj.string.getType();
    if (stype.isAstring()) {
        const obj2 = args[2].asHeapObject();
        const insert = obj2.string.getSlice();
        if (obj2.string.getType().isAstring()) {
            return vm.allocAstringConcat3(str[0..uidx], insert, str[uidx..]) catch fatal();
        } else {
            return vm.allocUstringConcat3(str[0..uidx], insert, str[uidx..]) catch fatal();
        }
    } else {
        const obj2 = args[2].asHeapObject();
        const insert = obj2.string.getSlice();
        return vm.allocUstringConcat3(str[0..uidx], insert, str[uidx..]) catch fatal();
    }
}

pub fn find(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const needle = args[1].asString();
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

pub fn startsWith(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const needle = args[1].asString();
    return Value.initBool(std.mem.startsWith(u8, str, needle));
}


pub fn endsWith(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const str = args[0].asHeapObject().string.getSlice();
    const needle = args[1].asString();
    return Value.initBool(std.mem.endsWith(u8, str, needle));
}


pub fn isAscii(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const stype = obj.string.getType();
    return Value.initBool(stype.isAstring());
}

const intSome = cy.builtins.intSome;
const intNone = cy.builtins.intNone;

pub fn findAnyRune(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const set_obj = args[1].asHeapObject();
    const set = set_obj.string.getSlice();
    const setIsAscii = set_obj.string.getType().isAstring();
    const stype = obj.string.getType();

    if (set.len > 0) {
        if (stype.isAstring()) {
            if (setIsAscii) {
                if (@call(.never_inline, string.indexOfAsciiSet, .{str, set})) |idx| {
                    return intSome(vm, @intCast(idx)) catch cy.fatal();
                }
            } else {
                // Reduce search set to just ASCII codepoints.
                const alloc = vm.alloc;
                const tempBuf = &vm.u8Buf;
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
                    if (string.indexOfAsciiSet(str, tempBuf.items())) |idx| {
                        return intSome(vm, @intCast(idx)) catch cy.fatal();
                    }
                }
            }
        } else {
            if (setIsAscii) {
                if (string.indexOfAsciiSet(str, set)) |idx| {
                    return intSome(vm, @intCast(idx)) catch cy.fatal();
                }
            } else {
                // Scalar.
                var iter = std.unicode.Utf8Iterator{
                    .bytes = str,
                    .i = 0,
                };
                while (iter.nextCodepoint()) |cp| {
                    var set_iter = std.unicode.Utf8Iterator{
                        .bytes = set,
                        .i = 0,
                    };
                    while (set_iter.nextCodepoint()) |set_cp| {
                        if (set_cp == cp) {
                            return intSome(vm, @intCast(iter.i)) catch cy.fatal();
                        }
                    }
                }
            }
        }
    }
    return intNone(vm) catch cy.fatal();
}

pub fn findRune(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const needle = args[1].asInteger();
    const stype = obj.string.getType(); 

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

pub fn upper(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();
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

pub fn lower(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();
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

pub fn less(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const right = args[1].asString();
    return Value.initBool(std.mem.lessThan(u8, str, right));
}

pub fn lenFn(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(obj.string.len());
}

pub fn count(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const stype = obj.string.getType();
    if (stype.isAstring()) {
        return Value.initInt(obj.string.len());
    } else {
        return Value.initInt(@intCast(cy.string.countRunes(obj.string.getSlice())));
    }
}

pub fn seek(_: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const idx = args[1].asInteger();
    const stype = obj.string.getType();
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

pub fn runeAt(_: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const idx = args[1].asInteger();
    if (idx < 0 or idx >= str.len) {
        return error.OutOfBounds;
    }
    const stype = obj.string.getType();
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

pub fn sliceAt(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();
    var idx = args[1].asInteger();

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
            vm.retainObject(obj);
            return vm.allocAstringSlice(slice, obj) catch fatal();
        } else {
            vm.retainObject(obj);
            return vm.allocUstringSlice(slice, obj) catch fatal();
        }
    }
}

pub fn trim(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();

    const trimRunes = args[2].asString();

    var res: []const u8 = undefined;
    const mode = bindings.getBuiltinSymbol(args[1].asSymbolId()) orelse {
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

    vm.retainObject(obj);
    if (stype.isAstring()) {
        return vm.allocAstringSlice(res, obj) catch fatal();
    } else {
        return vm.allocUstringSlice(res, obj) catch fatal();
    }
}

pub fn stringReplace(vm: *cy.VM, args: [*]const Value, _: u8) Value {
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

pub fn repeat(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();

    const n = args[1].asInteger();
    if (n < 0) {
        return rt.prepThrowError(vm, .InvalidArgument);
    }

    var un: u32 = @intCast(n);
    const len = un * str.len;
    if (un > 1 and len > 0) {
        var new: *cy.HeapObject = undefined;
        var buf: []u8 = undefined;
        const stype = obj.string.getType();
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
            std.mem.copy(u8, buf[dst..dst + str.len], str);
            dst += @intCast(str.len);
        }

        return Value.initNoCycPtr(new);
    } else {
        if (un == 0) {
            vm.retain(vm.emptyString);
            return vm.emptyString;
        } else {
            vm.retainObject(obj);
            return Value.initNoCycPtr(obj);
        }
    }
}

pub fn split(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const str = obj.string.getSlice();
    const stype = obj.string.getType();
    const parent = obj.string.getParentByType(stype);
    const delim = args[1].asString();

    const res = try vm.allocEmptyListDyn();
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
            const partv = try vm.allocUstringSlice(part, parent);
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
        const str = try vm.getOrBufPrintValueStr(&cy.tempBuf, val);
        return vm.retainOrAllocAstring(str);
    }
}