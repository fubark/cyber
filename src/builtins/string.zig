const std = @import("std");
const cy = @import("../cyber.zig");
const Ret = cy.C.Ret;
const RetOk = cy.C.RetOk;
const rt = cy.rt;
const Value = cy.Value;
const fatal = cy.fatal;
const bindings = @import("bindings.zig");
const string = cy.string;
const bt = cy.types.BuiltinTypes;

pub fn initPtrSpan(t: *cy.Thread) !Ret {
    const ret = t.ret(cy.heap.Str);
    const span = t.param(cy.heap.PtrSpan);
    if (span.len == 0) {
        ret.* = try t.heap.init_str("");
        return RetOk;
    } else {
        const ptr: [*]const u8 = @ptrFromInt(span.ptr);
        ret.* = try t.heap.init_str(ptr[0..@intCast(span.len)]);
        return RetOk;
    }
}

pub fn concat(t: *cy.Thread) !Ret {
    const ret = t.ret(cy.heap.Str);
    const str = t.param(*cy.heap.Str);
    const str2 = t.param(cy.heap.Str);
    defer t.heap.destructStr(&str2);
    ret.* = try concat2(&t.heap, str, &str2);
    return RetOk;
}

fn concat2(heap: *cy.Heap, left: *cy.heap.Str, right: *const cy.heap.Str) !cy.heap.Str {
    if (left.ascii() and right.ascii()) {
        return heap.init_astr_concat(left.slice(), right.slice());
    } else {
        return heap.init_ustr_concat(left.slice(), right.slice());
    }
}

pub fn concat_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const left = ctx.args[0].asPtr(*cy.heap.Str);
    const right = ctx.args[1].asPtr(*cy.heap.Str);
    defer c.heap.releaseObject(@ptrCast(right));

    const res = try concat2(c.heap, left, right);
    const boxed = try c.heap.lift(bt.Str, res);
    return cy.TypeValue.init(c.sema.str_t, boxed);
}

pub fn sliceFn(t: *cy.Thread) callconv(.c) Ret {
    const ret = t.ret(cy.heap.Str);
    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();
    const buf = obj.buf();

    const start: usize = @intCast(t.param(u64));
    const end = str.len;
    if (start > end) {
        return t.ret_panic("OutOfBounds");
    }
    if (end < start) {
        return t.ret_panic("OutOfBounds");
    }

    const ustart: u32 = @intCast(start);
    const uend: u32 = @intCast(end);
    if (obj.ascii()) {
        t.heap.retainObjectOpt(@ptrCast(buf));
        ret.* = try t.heap.init_astr_slice(str[ustart..uend], buf);
        return RetOk;
    } else {
        t.heap.retainObjectOpt(@ptrCast(buf));
        if (cy.string.isAstring(str[ustart..uend])) {
            ret.* = try t.heap.init_astr_slice(str[ustart..uend], buf);
            return RetOk;
        } else {
            ret.* = try t.heap.init_ustr_slice(str[ustart..uend], buf);
            return RetOk;
        }
    }
}

pub fn slice2(t: *cy.Thread) callconv(.c) Ret {
    const ret = t.ret(cy.heap.Str);
    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();
    const buf = obj.buf();

    const start: usize = @intCast(t.param(u64));
    const end: usize = @intCast(t.param(u64));
    if (start > str.len) {
        return t.ret_panic("OutOfBounds");
    }
    if (end > str.len) {
        return t.ret_panic("OutOfBounds");
    }
    if (end < start) {
        return t.ret_panic("OutOfBounds");
    }

    const ustart: u32 = @intCast(start);
    const uend: u32 = @intCast(end);
    if (obj.ascii()) {
        t.heap.retainObjectOpt(@ptrCast(buf));
        ret.* = try t.heap.init_astr_slice(str[ustart..uend], buf);
        return RetOk;
    } else {
        t.heap.retainObjectOpt(@ptrCast(buf));
        if (cy.string.isAstring(str[ustart..uend])) {
            ret.* = try t.heap.init_astr_slice(str[ustart..uend], buf);
            return RetOk;
        } else {
            ret.* = try t.heap.init_ustr_slice(str[ustart..uend], buf);
            return RetOk;
        }
    }
}

pub fn insertByte(t: *cy.Thread) anyerror!Ret {
    const ret = t.ret(cy.heap.Str);
    const str = t.param(*cy.heap.Str);
    const slice = str.slice();
    const idx = try cy.core.intAsIndex(t.param(i64), slice.len + 1);
    const byte = t.param(u8);
    if (str.ascii() and byte < 128) {
        ret.* = try t.heap.init_astr_concat3(slice[0..idx], &.{ byte }, slice[idx..]);
        return RetOk;
    } else {
        ret.* = try t.heap.init_ustr_concat3(slice[0..idx], &.{byte}, slice[idx..]);
        return RetOk;
    }
}

pub fn insertFn(t: *cy.Thread) anyerror!Ret {
    const ret = t.ret(cy.heap.Str);

    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();
    const idx = t.param(i64);
    const insert = t.param(cy.heap.Str);
    if (idx < 0 or idx > str.len) {
        return error.OutOfBounds;
    }
    const uidx: u32 = @intCast(idx);
    if (obj.ascii()) {
        if (insert.ascii()) {
            ret.* = try t.heap.init_astr_concat3(str[0..uidx], insert.slice(), str[uidx..]);
            return RetOk;
        } else {
            ret.* = try t.heap.init_ustr_concat3(str[0..uidx], insert.slice(), str[uidx..]);
            return RetOk;
        }
    } else {
        ret.* = try t.heap.init_ustr_concat3(str[0..uidx], insert.slice(), str[uidx..]);
        return RetOk;
    }
}

pub fn index(t: *cy.Thread) !Ret {
    const ret = t.ret(cy.value.Option(i64));
    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();
    const needle = t.param(cy.heap.Str).slice();
    if (needle.len > 0 and needle.len <= str.len) {
        if (needle.len == 1) {
            // One ascii char special case. Perform indexOfChar.
            if (string.indexOfChar(str, needle[0])) |idx| {
                ret.* = cy.value.Option(i64).some(@intCast(idx));
                return RetOk;
            }
        }
        if (string.indexOf(str, needle)) |idx| {
            ret.* = cy.value.Option(i64).some(@intCast(idx));
            return RetOk;
        }
    }
    ret.* = cy.value.Option(i64).none();
    return RetOk;
}

pub fn starts_with(t: *cy.Thread) callconv(.c) Ret {
    const ret = t.ret(bool);
    const str = t.param(*cy.heap.Str).slice();
    const needle = t.param(cy.heap.Str);
    defer t.heap.destructStr(&needle);

    ret.* = std.mem.startsWith(u8, str, needle.slice());
    return RetOk;
}

pub fn ends_with(t: *cy.Thread) callconv(.c) Ret {
    const ret = t.ret(bool);
    const str = t.param(*cy.heap.Str).slice();
    const needle = t.param(cy.heap.Str);
    defer t.heap.destructStr(&needle);

    ret.* = std.mem.endsWith(u8, str, needle.slice());
    return RetOk;
}

pub fn isAscii(t: *cy.Thread) callconv(.c) Ret {
    const ret = t.ret(bool);
    const str = t.param(*cy.heap.Str).slice();
    const ascii = cy.string.isAstring(str);
    ret.* = ascii;
    return RetOk;
}

pub fn index_any_rune(t: *cy.Thread) anyerror!Ret {
    const ret = t.ret(cy.value.Option(i64));
    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();
    const set_list = t.param(cy.heap.Span).items(u64);

    const tempBuf = &t.c.vm.u8Buf;
    tempBuf.clearRetainingCapacity();
    defer if (tempBuf.capacity > 4096) tempBuf.clearAndFree(t.alloc);

    var set_is_ascii = true;
    for (set_list) |rune| {
        if (rune >= 0 and rune < 128) {
            try tempBuf.append(t.alloc, @intCast(rune));
        } else {
            set_is_ascii = false;
        }
    }
    const set_str = tempBuf.items;

    if (set_list.len > 0) {
        if (obj.ascii()) {
            if (set_is_ascii) {
                if (@call(.never_inline, string.indexOfAsciiSet, .{str, set_str})) |idx| {
                    ret.* = cy.value.Option(i64).some(@intCast(idx));
                    return RetOk;
                }
            } else {
                // Reduce search set to just ASCII codepoints.
                if (set_str.len > 0) {
                    if (string.indexOfAsciiSet(str, set_str)) |idx| {
                        ret.* = cy.value.Option(i64).some(@intCast(idx));
                        return RetOk;
                    }
                }
            }
        } else {
            if (set_is_ascii) {
                if (string.indexOfAsciiSet(str, set_str)) |idx| {
                    ret.* = cy.value.Option(i64).some(@intCast(idx));
                    return RetOk;
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
                        if (set_rune == cp) {
                            ret.* = cy.value.Option(i64).some(@intCast(idx));
                            return RetOk;
                        }
                    }
                }
            }
        }
    }
    ret.* = cy.value.Option(i64).none();
    return RetOk;
}

pub fn index_rune(t: *cy.Thread) !Ret {
    const ret = t.ret(cy.value.Option(i64));
    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();
    const needle = t.param(i64);

    if (needle > 0) {
        const code: u21 = @intCast(needle);
        const needleIsAscii = code < 128;
        if (obj.ascii()) {
            if (needleIsAscii) {
                if (string.indexOfChar(str, @intCast(code))) |idx| {
                    ret.* = cy.value.Option(i64).some(@intCast(idx));
                    return RetOk;
                }
            }
        } else {
            if (needleIsAscii) {
                if (string.indexOfChar(str, @intCast(code))) |idx| {
                    ret.* = cy.value.Option(i64).some(@intCast(idx));
                    return RetOk;
                }
            } else {
                var slice: [4]u8 = undefined;
                _ = std.unicode.utf8Encode(code, &slice) catch fatal();
                if (string.indexOf(str, &slice)) |idx| {
                    ret.* = cy.value.Option(i64).some(@intCast(idx));
                    return RetOk;
                }
            }
        }
    }
    ret.* = cy.value.Option(i64).none();
    return RetOk;
}

pub fn index_newline(t: *cy.Thread) !Ret {
    const ret = t.ret(cy.value.Option(i64));
    const str = t.param(*cy.heap.Str).slice();
    if (cy.string.indexOfNewLine(str)) |idx| {
        ret.* = cy.value.Option(i64).some(@intCast(idx));
        return RetOk;
    } else {
        ret.* = cy.value.Option(i64).none();
        return RetOk;
    }
}

pub fn upper(t: *cy.Thread) !Ret {
    const ret = t.ret(cy.heap.Str);
    const str_v = t.param(*cy.heap.Str);
    const str = str_v.slice();
    if (str_v.ascii()) {
        const new = try t.heap.init_astr_undef(str.len);
        const newBuf = new.mutSlice();
        _ = std.ascii.upperString(newBuf, str);
        ret.* = new;
    } else {
        const new = try t.heap.init_ustr_undef(str.len);
        const newBuf = new.mutSlice();
        _ = std.ascii.upperString(newBuf, str);
        ret.* = new;
    }
    return RetOk;
}

pub fn lower(t: *cy.Thread) !Ret {
    const ret = t.ret(cy.heap.Str);
    const str_v = t.param(*cy.heap.Str);
    const str = str_v.slice();
    if (str_v.ascii()) {
        const new = try t.heap.init_astr_undef(str.len);
        const newBuf = new.mutSlice();
        _ = std.ascii.lowerString(newBuf, str);
        ret.* = new;
    } else {
        const new = try t.heap.init_ustr_undef(str.len);
        const newBuf = new.mutSlice();
        _ = std.ascii.lowerString(newBuf, str);
        ret.* = new;
    }
    return RetOk;
}

pub fn less(t: *cy.Thread) callconv(.c) Ret {
    const ret = t.ret(bool);
    const str = t.param(cy.heap.Str);
    defer t.heap.destructStr(&str);
    const right = t.param(cy.heap.Str);
    defer t.heap.destructStr(&right);
    ret.* = std.mem.lessThan(u8, str.slice(), right.slice());
    return RetOk;
}

pub fn lenFn(t: *cy.Thread) callconv(.c) Ret {
    const ret = t.ret(i64);
    const str = t.param(*cy.heap.Str);
    ret.* = str.len();
    return RetOk;
}

pub fn count(t: *cy.Thread) callconv(.c) Ret {
    const ret = t.ret(i64);
    const str = t.param(*cy.heap.Str);
    if (str.ascii()) {
        ret.* = str.len();
    } else {
        ret.* = @intCast(cy.string.countRunes(str.slice()));
    }
    return RetOk;
}

pub fn seek_pos(t: *cy.Thread) anyerror!Ret {
    const ret = t.ret(i64);
    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();
    const idx = t.param(i64);
    if (obj.ascii()) {
        if (idx < 0 or idx >= str.len) {
            return t.ret_panic("OutOfBounds");
        }
        ret.* = @intCast(idx);
        return RetOk;
    } else {
        const uidx: u32 = @intCast(idx);
        var bad_byte: bool = undefined;
        const start: u32 = @intCast(try string.ustringSeekByRuneIndex(str, 0, 0, uidx, &bad_byte));
        ret.* = @intCast(start);
        return RetOk;
    }
}

pub fn rune_at(t: *cy.Thread) anyerror!Ret {
    const ret = t.ret(u32);

    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();
    const idx = t.param(i64);
    if (idx < 0 or idx >= str.len) {
        return t.ret_panic("OutOfBounds");
    }
    if (obj.ascii()) {
        ret.* = @intCast(str[@intCast(idx)]);
        return RetOk;
    } else {
        const uidx: u32 = @intCast(idx);
        const rune_len = std.unicode.utf8ByteSequenceLength(str[uidx]) catch {
            ret.* = @intCast(std.unicode.replacement_character);
            return RetOk;
        };
        if (uidx + rune_len > str.len) {
            ret.* = @intCast(std.unicode.replacement_character);
            return RetOk;
        }
        const rune = std.unicode.utf8Decode(str[uidx..uidx+rune_len]) catch {
            ret.* = @intCast(std.unicode.replacement_character);
            return RetOk;
        };
        ret.* = @intCast(rune);
        return RetOk;
    }
}

pub fn runeStrAt(t: *cy.Thread) anyerror!Ret {
    const ret = t.ret(cy.heap.Str);
    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();
    const idx = t.param(i64);

    if (idx < 0 or idx >= str.len) {
        return t.ret_panic("OutOfBounds");
    }
    const uidx: u32 = @intCast(idx);
    if (obj.ascii()) {
        // TODO: return slice.
        ret.* = try t.heap.init_astr(str[uidx..uidx + 1]);
        return RetOk;
    } else {
        var bad_byte: bool = undefined;
        const slice = string.runeSliceAt(str, uidx, &bad_byte);

        if (slice.len == 1 and !bad_byte) {
            // TODO: If single ascii runes are interned by default,
            //       it's better to fetch that instead to reduce dependency on parent.
            _ = try t.heap.copy_str(obj.*);
            ret.* = try t.heap.init_astr_slice(slice, obj.buf());
            return RetOk;
        } else {
            _ = try t.heap.copy_str(obj.*);
            ret.* = try t.heap.init_ustr_slice(slice, obj.buf());
            return RetOk;
        }
    }
}

pub fn stringReplace(t: *cy.Thread) !Ret {
    const ret = t.ret(cy.heap.Str);
    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();

    const needle = t.param(cy.heap.Str);
    defer t.heap.destructStr(&needle);

    const replace = t.param(cy.heap.Str);
    defer t.heap.destructStr(&replace);

    if (obj.ascii()) {
        if (try astringReplace(t, str, needle, replace)) |val| {
            ret.* = val;
            return RetOk;
        } else {
            ret.* = try t.heap.copy_str(obj.*);
            return RetOk;
        }
    } else {
        if (try ustringReplace(t, str, needle, replace)) |val| {
            ret.* = val;
            return RetOk;
        } else {
            ret.* = try t.heap.copy_str(obj.*);
            return RetOk;
        }
    }
}

fn astringReplace(t: *cy.Thread, str: []const u8, needlev: cy.heap.Str, replacev: cy.heap.Str) !?cy.heap.Str {
    const needle = needlev.slice();
    const replacement = replacev.slice();

    var buf: std.ArrayListAligned(u8, .@"4") = .{};
    defer buf.deinit(t.alloc);
    const newLen = try string.prepReplacement(str, needle, replacement, buf.writer(t.alloc));
    const numIdxes = buf.items.len / 4;
    if (numIdxes > 0) {
        if (replacev.ascii()) {
            const new = try t.heap.init_astr_undef(newLen);
            const newBuf = new.mutSlice();
            const idxes = @as([*]const u32, @ptrCast(buf.items.ptr))[0..numIdxes];
            string.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
            return new;
        } else {
            const new = try t.heap.init_ustr_undef(newLen);
            const newBuf = new.mutSlice();
            const idxes = @as([*]const u32, @ptrCast(buf.items.ptr))[0..numIdxes];
            string.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
            return new;
        }
    } else {
        return null;
    }
}

fn ustringReplace(t: *cy.Thread, str: []const u8, needlev: cy.heap.Str, replacev: cy.heap.Str) !?cy.heap.Str {
    const needle = needlev.slice();
    const replacement = replacev.slice();

    var buf: std.ArrayListAligned(u8, .@"4") = .{};
    defer buf.deinit(t.alloc);
    const newLen = try string.prepReplacement(str, needle, replacement, buf.writer(t.alloc));
    const numIdxes = buf.items.len / 4;
    if (numIdxes > 0) {
        const new = try t.heap.init_ustr_undef(newLen);
        const newBuf = new.mutSlice();
        const idxes = @as([*]const u32, @ptrCast(buf.items.ptr))[0..numIdxes];
        cy.string.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
        return new;
    } else {
        return null;
    }
}

pub fn repeat(t: *cy.Thread) !Ret {
    const ret = t.ret(cy.heap.Str);
    const obj = t.param(*cy.heap.Str);
    const str = obj.slice();

    const n = t.param(i64);
    if (n < 0) {
        return t.ret_panic("InvalidArgument");
    }

    const un: u32 = @intCast(n);
    const len = un * str.len;
    if (un > 1 and len > 0) {
        var new: cy.heap.Str = undefined;
        var buf: []u8 = undefined;
        if (obj.ascii()) {
            new = try t.heap.init_astr_undef(len);
            buf = new.mutSlice();
        } else {
            new = try t.heap.init_ustr_undef(len);
            buf = new.mutSlice();
        }
        // This is already quite fast since it has good cache locality.
        // Might be faster if the front of the buffer up to a certain size was used to memcpy instead of just 1 `str`.
        var i: u32 = 0;
        var dst: u32 = 0;
        while (i < un) : (i += 1) {
            @memcpy(buf[dst..dst + str.len], str);
            dst += @intCast(str.len);
        }

        ret.* = new;
        return RetOk;
    } else {
        if (un == 0) {
            ret.* = try t.heap.init_empty_str();
            return RetOk;
        } else {
            ret.* = try t.heap.copy_str(obj.*);
            return RetOk;
        }
    }
}
