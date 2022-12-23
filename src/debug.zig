const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.debug);

const Section = ".eval2";

pub fn computeLinePos(src: []const u8, loc: u32, outLine: *u32, outCol: *u32, outLineStart: *u32) linksection(Section) void {
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

pub fn computeLinePosWithTokens(tokens: []const cy.Token, src: []const u8, pos: u32, outLine: *u32, outCol: *u32, outLineStart: *u32) linksection(Section) void {
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