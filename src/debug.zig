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
            outLine.* = line;
            outCol.* = loc - lineStart;
            outLineStart.* = lineStart;
            return;
        }
        if (ch == '\n') {
            line += 1;
            lineStart = @intCast(u32, i + 1);
        }
    }
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
    computeLinePosWithTokens(parser.tokens.items, parser.src.items, 4, &line, &col, &lineStart);
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
    computeLinePosWithTokens(parser.tokens.items, parser.src.items, @intCast(u32, parser.src.items.len-1), &line, &col, &lineStart);
    try t.eq(line, 3);
    try t.eq(col, 0);
    try t.eq(lineStart, 18);

    // Error at end.
    _ = try parser.parse(
        \\a = 1
    );
    computeLinePosWithTokens(parser.tokens.items, parser.src.items, @intCast(u32, parser.src.items.len), &line, &col, &lineStart);
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
            const node = vm.compiler.nodes[sym.loc];
            var line: u32 = undefined;
            var col: u32 = undefined;
            var lineStart: u32 = undefined;
            const pos = vm.compiler.tokens[node.start_token].pos();
            computeLinePosWithTokens(vm.parser.tokens.items, vm.parser.src.items, pos, &line, &col, &lineStart);
            log.debug("{*} was allocated at {}:{}", .{obj, line + 1, col + 1});
            return;
        } 
    }
    log.debug("No trace for {*}.", .{obj});
}

pub inline fn atLeastTestDebugLevel() bool {
    return @enumToInt(std.testing.log_level) >= @enumToInt(std.log.Level.debug);
}

pub fn printUserError(vm: *const cy.VM, title: []const u8, msg: []const u8, srcUri: []const u8, pos: u32, isTokenError: bool) linksection(cy.Section) !void {
    if (cy.silentError) {
        return;
    }
    if (pos != NullId) {
        var line: u32 = undefined;
        var col: u32 = undefined;
        var lineStart: u32 = undefined;
        if (isTokenError) {
            computeLinePos(vm.parser.src.items, pos, &line, &col, &lineStart);
        } else {
            computeLinePosWithTokens(vm.parser.tokens.items, vm.parser.src.items, pos, &line, &col, &lineStart);
        }
        const lineEnd = std.mem.indexOfScalarPos(u8, vm.parser.src.items, lineStart, '\n') orelse vm.parser.src.items.len;
        var arrowBuf: std.ArrayListUnmanaged(u8) = .{};
        defer arrowBuf.deinit(vm.alloc);
        var w = arrowBuf.writer(vm.alloc);
        try w.writeByteNTimes(' ', col);
        try w.writeByte('^');
        fmt.printStderr(
            \\{}: {}
            \\
            \\{}:{}:{}:
            \\{}
            \\{}
            \\
        , &.{
            fmt.v(title), fmt.v(msg), fmt.v(srcUri),
            fmt.v(line+1), fmt.v(col+1), fmt.v(vm.parser.src.items[lineStart..lineEnd]),
            fmt.v(arrowBuf.items)
        });
    } else {
        fmt.printStderr(
            \\{}: {}
            \\
            \\in {}
            \\
        , &.{
            fmt.v(title), fmt.v(msg), fmt.v(srcUri),
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
            const ptr = vm.panicPayload & ((1 << 48) - 1);
            const len = vm.panicPayload >> 48;
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
            const ptr = vm.panicPayload & ((1 << 48) - 1);
            const len = vm.panicPayload >> 48;
            vm.alloc.free(@intToPtr([*]const u8, ptr)[0..len]);
        },
        .none => {},
    }
}