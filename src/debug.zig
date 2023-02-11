const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const fmt = @import("fmt.zig");
const log = stdx.log.scoped(.debug);

const NullId = std.math.maxInt(u32);

pub fn computeLinePos(src: []const u8, loc: u32, outLine: *u32, outCol: *u32, outLineStart: *u32) linksection(cy.Section) void {
    var line: u32 = 0;
    var lineStart: u32 = 0;
    for (src) |ch, i| {
        if (i == loc) {
            break;
        }
        if (ch == '\n') {
            line += 1;
            lineStart = @intCast(u32, i + 1);
        }
    }
    // This also handles the case where target pos is at the end of source.
    outLine.* = line;
    outCol.* = loc - lineStart;
    outLineStart.* = lineStart;
}

pub fn computeLinePosWithTokens(tokens: []const cy.Token, src: []const u8, pos: u32, outLine: *u32, outCol: *u32, outLineStart: *u32) linksection(cy.Section) void {
    var line: u32 = 0;
    var lineStart: u32 = 0;
    for (tokens) |token| {
        if (token.pos() == pos) {
            break;
        }
        if (token.tag() == .new_line) {
            line += 1;
            lineStart = token.pos() + 1;
        } else if (token.tag() == .string or token.tag() == .templateString) {
            const str = src[token.start_pos..token.data.end_pos];
            var lastIdx: u32 = undefined;
            const newLines = countNewLines(str, &lastIdx);
            if (newLines > 0) {
                line += newLines;
                lineStart = token.start_pos + lastIdx + 1;
            }
        }
    }
    // This also handles the case where target pos is at the end of source.
    outLine.* = line;
    outCol.* = pos - lineStart;
    outLineStart.* = lineStart;
}

pub fn countNewLines(str: []const u8, outLastIdx: *u32) u32 {
    var count: u32 = 0;
    var i: u32 = 0;
    while (i < str.len) {
        if (str[i] == '\n') {
            count += 1;
            outLastIdx.* = i;
            i += 1;
        } else if (str[i] == '\r') {
            count += 1;
            if (i + 1 < str.len and str[i+1] == '\n') {
                outLastIdx.* = i + 1;
                i += 2;
            } else {
                outLastIdx.* = i;
                i += 1;
            }
        } else {
            i += 1;
        }
    }
    return count;
}

test "computeLinePosFromTokens" {
    var parser = cy.Parser.init(t.alloc);
    defer parser.deinit();

    // Basic.
    _ = try parser.parse(
        \\a = 1
    );
    var line: u32 = undefined;
    var col: u32 = undefined;
    var lineStart: u32 = undefined;
    computeLinePosWithTokens(parser.tokens.items, parser.src, 4, &line, &col, &lineStart);
    try t.eq(line, 0);
    try t.eq(col, 4);
    try t.eq(lineStart, 0);

    // String token with new lines.
    _ = try parser.parse(
        \\a = "abc
        \\xyz
        \\123"
        \\a
    );
    computeLinePosWithTokens(parser.tokens.items, parser.src, @intCast(u32, parser.src.len-1), &line, &col, &lineStart);
    try t.eq(line, 3);
    try t.eq(col, 0);
    try t.eq(lineStart, 18);

    // Error at end.
    _ = try parser.parse(
        \\a = 1
    );
    computeLinePosWithTokens(parser.tokens.items, parser.src, @intCast(u32, parser.src.len), &line, &col, &lineStart);
    try t.eq(line, 0);
    try t.eq(col, 5);
    try t.eq(lineStart, 0);
}

pub fn indexOfDebugSym(vm: *const cy.VM, pc: usize) ?usize {
    for (vm.debugTable) |sym, i| {
        if (sym.pc == pc) {
            return i;
        }
    }
    return null;
}

pub fn dumpObjectTrace(vm: *const cy.VM, obj: *cy.HeapObject) void {
    if (vm.objectTraceMap.get(obj)) |pc| {
        if (indexOfDebugSym(vm, pc)) |idx| {
            const sym = vm.debugTable[idx];
            const chunk = &vm.compiler.chunks.items[sym.file];
            const node = chunk.nodes[sym.loc];
            var line: u32 = undefined;
            var col: u32 = undefined;
            var lineStart: u32 = undefined;
            const pos = chunk.tokens[node.start_token].pos();
            computeLinePosWithTokens(chunk.parser.tokens.items, chunk.src, pos, &line, &col, &lineStart);
            log.debug("{*} was allocated at {}:{}", .{obj, line + 1, col + 1});
            return;
        } 
    }
    log.debug("No trace for {*}.", .{obj});
}

pub inline fn atLeastTestDebugLevel() bool {
    return @enumToInt(std.testing.log_level) >= @enumToInt(std.log.Level.debug);
}

pub fn allocLastUserParseError(vm: *const cy.VM) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    const w = buf.writer(vm.alloc);
    try writeLastUserParseError(vm, w);
    return buf.toOwnedSlice(vm.alloc);
}

pub fn writeLastUserTokenError(vm: *const cy.VM, w: anytype) !void {
    const chunk = vm.compiler.chunks.items[vm.compiler.lastErrChunk];
    try writeUserError(vm, w, "TokenError", chunk.parser.last_err, chunk.id, chunk.parser.last_err_pos, true);
}

pub fn printLastUserTokenError(vm: *const cy.VM) !void {
    if (cy.silentError) {
        return;
    }
    const w = fmt.lockStderrWriter();
    defer fmt.unlockPrint();
    try writeLastUserTokenError(vm, w);
}

pub fn writeLastUserParseError(vm: *const cy.VM, w: anytype) !void {
    const chunk = vm.compiler.chunks.items[vm.compiler.lastErrChunk];
    try writeUserError(vm, w, "ParseError", chunk.parser.last_err, chunk.id, chunk.parser.last_err_pos, true);
}

pub fn printLastUserParseError(vm: *const cy.VM) !void {
    if (cy.silentError) {
        return;
    }
    const w = fmt.lockStderrWriter();
    defer fmt.unlockPrint();
    try writeLastUserParseError(vm, w);
}

pub fn printUserError(vm: *const cy.VM, title: []const u8, msg: []const u8, chunkId: u32, pos: u32, isCharPos: bool) linksection(cy.Section) !void {
    if (cy.silentError) {
        return;
    }
    const w = fmt.lockStderrWriter();
    defer fmt.unlockPrint();
    try writeUserError(vm, w, title, msg, chunkId, pos, isCharPos);
}

/// Reduced to using writer so printed errors can be tested.
pub fn writeUserError(vm: *const cy.VM, w: anytype, title: []const u8, msg: []const u8, chunkId: u32, pos: u32, isCharPos: bool) linksection(cy.Section) !void {
    const chunk = vm.compiler.chunks.items[chunkId];
    if (pos != NullId) {
        var line: u32 = undefined;
        var col: u32 = undefined;
        var lineStart: u32 = undefined;
        if (isCharPos) {
            computeLinePos(chunk.parser.src, pos, &line, &col, &lineStart);
        } else {
            computeLinePosWithTokens(chunk.tokens, chunk.src, pos, &line, &col, &lineStart);
        }
        const lineEnd = std.mem.indexOfScalarPos(u8, chunk.src, lineStart, '\n') orelse chunk.src.len;
        try fmt.format(w,
            \\{}: {}
            \\
            \\{}:{}:{}:
            \\{}
            \\
        , &.{
            fmt.v(title), fmt.v(msg), fmt.v(chunk.srcUri),
            fmt.v(line+1), fmt.v(col+1),
            fmt.v(chunk.src[lineStart..lineEnd]),
        });
        try w.writeByteNTimes(' ', col);
        _ = try w.write("^\n");
    } else {
        try fmt.format(w,
            \\{}: {}
            \\
            \\in {}
            \\
        , &.{
            fmt.v(title), fmt.v(msg), fmt.v(chunk.srcUri),
        });
    }
}

pub const PanicPayload = u64;

pub const PanicType = enum {
    /// User error. Error value is in `panicPayload`.
    err,

    /// Msg string is in `panicPayload`. Lower u48 is the pointer, and upper u16 is the length.
    msg,

    none,
};

pub fn allocPanicMsg(vm: *const cy.VM) ![]const u8 {
    switch (vm.panicType) {
        .err => {
            const str = vm.valueToTempString(cy.Value{ .val = vm.panicPayload });
            return try fmt.allocFormat(vm.alloc, "{}", &.{fmt.v(str)});
        },
        .msg => {
            const ptr = @intCast(usize, vm.panicPayload & ((1 << 48) - 1));
            const len = @intCast(usize, vm.panicPayload >> 48);
            return vm.alloc.dupe(u8, @intToPtr([*]const u8, ptr)[0..len]);
        },
        .none => {
            stdx.panic("Unexpected panic type.");
        },
    }
}

pub fn freePanicPayload(vm: *const cy.VM) void {
    switch (vm.panicType) {
        .err => {},
        .msg => {
            const ptr = @intCast(usize, vm.panicPayload & ((1 << 48) - 1));
            const len = @intCast(usize, vm.panicPayload >> 48);
            vm.alloc.free(@intToPtr([*]const u8, ptr)[0..len]);
        },
        .none => {},
    }
}