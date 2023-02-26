const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const fmt = @import("fmt.zig");
const bytecode = @import("bytecode.zig");
const v = fmt.v;
const log = stdx.log.scoped(.debug);

const NullId = std.math.maxInt(u32);

pub fn computeLinePos(src: []const u8, loc: u32, outLine: *u32, outCol: *u32, outLineStart: *u32) linksection(cy.Section) void {
    var line: u32 = 0;
    var lineStart: u32 = 0;
    for (src, 0..) |ch, i| {
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

/// `pos` is the char pos in `src`.
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

/// Assumes symbol exists.
/// Since there are different call insts with varying lengths,
/// the call convention prefers to advance the pc before saving it so
/// stepping over the call will already have the correct pc.
/// The saved pc would point to the end of the inst so the lookup
/// returns first entry before the saved pc.
pub fn getDebugSymBefore(vm: *const cy.VM, pc: usize) cy.DebugSym {
    for (vm.debugTable, 0..) |sym, i| {
        if (sym.pc >= pc) {
            return vm.debugTable[i - 1];
        }
    }
    return vm.debugTable[vm.debugTable.len-1];
}

pub fn getDebugSym(vm: *const cy.VM, pc: usize) ?cy.DebugSym {
    return getDebugSymFromTable(vm.debugTable, pc);
}

pub fn getDebugSymFromTable(table: []const cy.DebugSym, pc: usize) ?cy.DebugSym {
    const idx = indexOfDebugSymFromTable(table, pc) orelse return null;
    return table[idx];
}

pub fn indexOfDebugSym(vm: *const cy.VM, pc: usize) ?usize {
    return indexOfDebugSymFromTable(vm.debugTable, pc);
}

pub fn indexOfDebugSymFromTable(table: []const cy.DebugSym, pc: usize) ?usize {
    for (table, 0..) |sym, i| {
        if (sym.pc == pc) {
            return i;
        }
    }
    return null;
}

pub fn dumpObjectTrace(vm: *const cy.VM, obj: *cy.HeapObject) !void {
    if (vm.objectTraceMap.get(obj)) |trace| {
        if (indexOfDebugSym(vm, trace.allocPc)) |idx| {
            const sym = vm.debugTable[idx];
            const chunk = &vm.compiler.chunks.items[sym.file];
            const node = chunk.nodes[sym.loc];
            const token = chunk.tokens[node.start_token];
            const msg = try std.fmt.allocPrint(vm.alloc, "Allocated object: {*} at pc: {}", .{obj, trace.allocPc});
            defer vm.alloc.free(msg);
            try printUserError(vm, "DebugTrace", msg, sym.file, token.pos(), false);
        } else {
            log.debug("Missing debug sym for {}, pc: {}.", .{vm.ops[trace.allocPc].code, trace.allocPc});
        }
        if (trace.freePc != cy.NullId) {
            const msg = try std.fmt.allocPrint(vm.alloc, "Freed object: {*} at pc: {}", .{obj, trace.freePc});
            defer vm.alloc.free(msg);
            try printTraceAtPc(vm, trace.freePc, msg);
        }
    } else {
        log.debug("No trace for {*}.", .{obj});
    }
}

pub fn printTraceAtPc(vm: *const cy.VM, pc: u32, msg: []const u8) !void {
    if (indexOfDebugSym(vm, pc)) |idx| {
        const sym = vm.debugTable[idx];
        const chunk = &vm.compiler.chunks.items[sym.file];
        const node = chunk.nodes[sym.loc];
        const token = chunk.tokens[node.start_token];
        try printUserError(vm, "Trace", msg, sym.file, token.pos(), false);
    } else {
        if (pc == cy.NullId) {
            std.debug.print("Trace: {s} (vm global allocation)\n", .{msg});
        } else {
            log.debug("Missing debug sym for {}, pc: {}.", .{vm.ops[pc].code, pc});
        }
    }
}

pub inline fn atLeastTestDebugLevel() bool {
    return @enumToInt(std.testing.log_level) >= @enumToInt(std.log.Level.debug);
}

pub fn allocLastUserPanicError(vm: *const cy.VM) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    const w = buf.writer(vm.alloc);
    try writeLastUserPanicError(vm, w);
    return buf.toOwnedSlice(vm.alloc);
}

pub fn printLastUserPanicError(vm: *const cy.VM) !void {
    if (cy.silentError) {
        return;
    }
    const w = fmt.lockStderrWriter();
    defer fmt.unlockPrint();
    try writeLastUserPanicError(vm, w);
}

fn writeLastUserPanicError(vm: *const cy.VM, w: anytype) !void {
    const msg = try allocPanicMsg(vm);
    defer vm.alloc.free(msg);

    try fmt.format(w, "panic: {}\n\n", &.{v(msg)});
    const trace = vm.getStackTrace();
    try trace.write(vm, w);
}

pub fn allocLastUserCompileError(vm: *const cy.VM) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    const w = buf.writer(vm.alloc);
    try writeLastUserCompileError(vm, w);
    return buf.toOwnedSlice(vm.alloc);
}

pub fn printLastUserCompileError(vm: *const cy.VM) !void {
    if (cy.silentError) {
        return;
    }
    const w = fmt.lockStderrWriter();
    defer fmt.unlockPrint();
    try writeLastUserCompileError(vm, w);
}

fn writeLastUserCompileError(vm: *const cy.VM, w: anytype) !void {
    const chunk = vm.compiler.chunks.items[vm.compiler.lastErrChunk];
    if (vm.compiler.lastErrNode != cy.NullId) {
        const token = chunk.nodes[vm.compiler.lastErrNode].start_token;
        const pos = chunk.tokens[token].pos();
        try writeUserError(vm, w, "CompileError", vm.compiler.lastErr, chunk.id, pos, false);
    } else {
        try writeUserError(vm, w, "CompileError", vm.compiler.lastErr, chunk.id, cy.NullId, false);
    }
}

pub fn allocLastUserParseError(vm: *const cy.VM) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    const w = buf.writer(vm.alloc);
    try writeLastUserParseError(vm, w);
    return buf.toOwnedSlice(vm.alloc);
}

fn writeLastUserTokenError(vm: *const cy.VM, w: anytype) !void {
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

fn writeLastUserParseError(vm: *const cy.VM, w: anytype) !void {
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

pub fn printUserError(vm: *const cy.VM, title: []const u8, msg: []const u8, chunkId: u32, pos: u32, queryPosFromSrc: bool) linksection(cy.Section) !void {
    if (cy.silentError) {
        return;
    }
    const w = fmt.lockStderrWriter();
    defer fmt.unlockPrint();
    try writeUserError(vm, w, title, msg, chunkId, pos, queryPosFromSrc);
}

/// Reduced to using writer so printed errors can be tested.
pub fn writeUserError(vm: *const cy.VM, w: anytype, title: []const u8, msg: []const u8, chunkId: u32, pos: u32, queryPosFromSrc: bool) linksection(cy.Section) !void {
    const chunk = vm.compiler.chunks.items[chunkId];
    if (pos != NullId) {
        var line: u32 = undefined;
        var col: u32 = undefined;
        var lineStart: u32 = undefined;
        if (queryPosFromSrc) {
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
            v(title), v(msg), v(chunk.srcUri),
            v(line+1), v(col+1),
            v(chunk.src[lineStart..lineEnd]),
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
            v(title), v(msg), v(chunk.srcUri),
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
            return try fmt.allocFormat(vm.alloc, "{}", &.{v(str)});
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

pub const StackTrace = struct {
    frames: []const StackFrame = &.{},

    pub fn deinit(self: *StackTrace, alloc: std.mem.Allocator) void {
        alloc.free(self.frames);
    }

    pub fn dump(self: *const StackTrace, vm: *const cy.VM) !void {
        const w = fmt.lockStderrWriter();
        defer fmt.unlockPrint();
        try self.write(vm, w);
    }

    pub fn write(self: *const StackTrace, vm: *const cy.VM, w: anytype) !void {
        for (self.frames) |frame| {
            const chunk = vm.compiler.chunks.items[frame.chunkId];
            const lineEnd = std.mem.indexOfScalarPos(u8, chunk.src, frame.lineStartPos, '\n') orelse chunk.src.len;
            try fmt.format(w,
                \\{}:{}:{} {}:
                \\{}
                \\
            , &.{
                v(chunk.srcUri), v(frame.line+1), v(frame.col+1), v(frame.name),
                v(chunk.src[frame.lineStartPos..lineEnd]),
            });
            try w.writeByteNTimes(' ', frame.col);
            try w.writeAll("^\n");
        }
    }
};

pub const StackFrame = struct {
    /// Name identifier (eg. function name, or "main" for the main block)
    name: []const u8,
    /// Starts at 0.
    line: u32,
    /// Starts at 0.
    col: u32,
    /// Where the line starts in the source file.
    lineStartPos: u32,
    chunkId: u32,
};

pub fn buildStackTrace(self: *cy.VM, fromPanic: bool) !void {
    @setCold(true);
    self.stackTrace.deinit(self.alloc);
    var frames: std.ArrayListUnmanaged(StackFrame) = .{};

    var framePtr = cy.framePtrOffset(self, self.framePtr);
    var pc = cy.pcOffset(self, self.pc);
    var isTopFrame = true;
    while (true) {
        const sym = b: {
            if (isTopFrame) {
                isTopFrame = false;
                if (fromPanic) {
                    break :b getDebugSym(self, pc) orelse return error.NoDebugSym;
                }
            }
            break :b getDebugSymBefore(self, pc);
        };

        if (sym.frameLoc == cy.NullId) {
            const chunk = self.compiler.chunks.items[sym.file];
            const node = chunk.nodes[sym.loc];
            var line: u32 = undefined;
            var col: u32 = undefined;
            var lineStart: u32 = undefined;
            const pos = chunk.tokens[node.start_token].pos();
            computeLinePosWithTokens(chunk.tokens, chunk.src, pos, &line, &col, &lineStart);
            try frames.append(self.alloc, .{
                .name = "main",
                .chunkId = sym.file,
                .line = line,
                .col = col,
                .lineStartPos = lineStart,
            });
            break;
        } else {
            const chunk = self.compiler.chunks.items[sym.file];
            const frameNode = chunk.nodes[sym.frameLoc];
            const func = chunk.funcDecls[frameNode.head.func.decl_id];
            const name = func.getName(&chunk);

            const node = chunk.nodes[sym.loc];
            var line: u32 = undefined;
            var col: u32 = undefined;
            var lineStart: u32 = undefined;
            const pos = chunk.tokens[node.start_token].pos();
            computeLinePosWithTokens(chunk.tokens, chunk.src, pos, &line, &col, &lineStart);
            try frames.append(self.alloc, .{
                .name = name,
                .chunkId = sym.file,
                .line = line,
                .col = col,
                .lineStartPos = lineStart,
            });
            pc = cy.pcOffset(self, self.stack[framePtr + 2].retPcPtr);
            framePtr = cy.framePtrOffset(self, self.stack[framePtr + 3].retFramePtr);
        }
    }

    self.stackTrace.frames = try frames.toOwnedSlice(self.alloc);
}

/// Given pc position, return the end locals pc in the same frame.
/// TODO: Memoize this function.
pub fn pcToEndLocalsPc(vm: *const cy.VM, pc: usize) u32 {
    const idx = indexOfDebugSym(vm, pc) orelse {
        stdx.panic("Missing debug symbol.");
    };
    const sym = vm.debugTable[idx];
    if (sym.frameLoc != cy.NullId) {
        const chunk = vm.compiler.chunks.items[sym.file];
        const node = chunk.nodes[sym.frameLoc];
        return node.head.func.genEndLocalsPc;
    } else {
        // Located in the main block.
        const chunk = vm.compiler.chunks.items[0];
        const node = chunk.nodes[0];
        // Can be NullId if `shouldGenMainScopeReleaseOps` is false.
        return node.head.root.genEndLocalsPc;
    }
}

pub fn debugSymToEndLocalsPc(vm: *const cy.VM, sym: cy.DebugSym) u32 {
    if (sym.frameLoc != cy.NullId) {
        const chunk = vm.compiler.chunks.items[sym.file];
        const node = chunk.nodes[sym.frameLoc];
        return node.head.func.genEndLocalsPc;
    } else {
        // Located in the main block.
        const chunk = vm.compiler.chunks.items[0];
        const node = chunk.nodes[0];
        // Can be NullId if `shouldGenMainScopeReleaseOps` is false.
        return node.head.root.genEndLocalsPc;
    }
}

pub const ObjectTrace = struct {
    /// Points to inst that allocated the object.
    allocPc: u32,

    /// Points to inst that freed the object.
    freePc: u32,
};

/// When `optPcContext` is null, all the bytecode is dumped along with constants.
/// When `optPcContext` is non null, it will dump a trace at `optPcContext` and the surrounding bytecode with extra details.
pub fn dumpBytecode(vm: *const cy.VM, optPcContext: ?u32) !void {
    var pcOffset: u32 = 0;
    var opsLen = vm.compiler.buf.ops.items.len;
    var pc = vm.compiler.buf.ops.items.ptr;
    const debugTable = vm.compiler.buf.debugTable.items;

    if (optPcContext) |pcContext| {
        const idx = indexOfDebugSymFromTable(debugTable, pcContext) orelse {
            return stdx.panicFmt("Missing debug sym at {} {}", .{pcContext, pc[pcContext].code});
        };
        const sym = debugTable[idx];
        const chunk = vm.compiler.chunks.items[sym.file];

        if (sym.frameLoc == cy.NullId) {
            fmt.printStderr("Block: main\n", &.{});
            const sblock = &chunk.semaBlocks.items[chunk.mainSemaBlockId];
            try chunk.dumpLocals(sblock);
        } else {
            const funcNode = chunk.nodes[sym.frameLoc];
            const funcDecl = chunk.funcDecls[funcNode.head.func.decl_id];
            const funcName = funcDecl.getName(&chunk);
            if (funcName.len == 0) {
                fmt.printStderr("Block: lambda()\n", &.{});
            } else {
                fmt.printStderr("Block: {}()\n", &.{ v(funcName) });
            }
            const sblock = &chunk.semaBlocks.items[funcDecl.semaBlockId];
            try chunk.dumpLocals(sblock);
        }
        fmt.printStderr("\n", &.{});

        var dumpCtx = DumpContext{
            .vm = vm,
            .symIdToVar = .{},
        };
        defer dumpCtx.deinit();
        var iter = vm.varSymSigs.iterator();
        while (iter.next()) |e| {
            try dumpCtx.symIdToVar.put(vm.alloc, e.value_ptr.*, e.key_ptr.*);
        }

        const node = chunk.nodes[sym.loc];
        const token = chunk.tokens[node.start_token];
        const msg = try std.fmt.allocPrint(vm.alloc, "pc={} op={s} node={s}", .{ pcContext, @tagName(pc[pcContext].code), @tagName(node.node_t) });
        defer vm.alloc.free(msg);
        try printUserError(vm, "Trace", msg, sym.file, token.pos(), false);

        fmt.printStderr("Bytecode:\n", &.{});
        const ContextSize = 40;
        const startSymIdx = if (idx >= ContextSize) idx - ContextSize else 0;
        pcOffset = debugTable[startSymIdx].pc;

        // Print until requested inst.
        pc += pcOffset;
        while (pcOffset < pcContext) {
            const code = pc[0].code;
            const len = bytecode.getInstLenAt(pc);
            try dumpCtx.dumpInst(pcOffset, code, pc, len);
            pcOffset += len;
            pc += len;
        }

        // Special marker for requested inst.
        fmt.printStderr("--", &.{});
        var code = pc[0].code;
        var len = bytecode.getInstLenAt(pc);
        try dumpCtx.dumpInst(pcOffset, code, pc, len);
        pcOffset += len;
        pc += len;

        // Keep printing instructions until ContextSize or end is reached.
        var i: usize = 0;
        while (pcOffset < opsLen) {
            code = pc[0].code;
            len = bytecode.getInstLenAt(pc);
            try dumpCtx.dumpInst(pcOffset, code, pc, len);
            pcOffset += len;
            pc += len;
            i += 1;
            if (i >= ContextSize) {
                break;
            }
        }
    } else {
        fmt.printStderr("Bytecode:\n", &.{});
        while (pcOffset < opsLen) {
            const code = pc[0].code;
            const len = bytecode.getInstLenAt(pc);
            bytecode.dumpInst(pcOffset, code, pc, len, "");
            pcOffset += len;
            pc += len;
        }

        fmt.printStderr("\nConstants:\n", &.{});
        for (vm.compiler.buf.mconsts) |extra| {
            const val = cy.Value{ .val = extra.val };
            if (val.isNumber()) {
                fmt.printStderr("{}\n", &.{v(val.asF64())});
            } else {
                fmt.printStderr("{}\n", &.{v(extra.val)});
            }
        }
    }
}

const DumpContext = struct {
    vm: *const cy.VM,
    symIdToVar: std.AutoHashMapUnmanaged(u32, cy.KeyU64),

    fn deinit(self: *DumpContext) void {
        self.symIdToVar.deinit(self.vm.alloc);
    }

    fn dumpInst(self: *const DumpContext, pcOffset: u32, code: cy.OpCode, pc: [*]cy.OpData, len: u32) !void {
        var buf: [1024]u8 = undefined;
        var extra: []const u8 = "";
        switch (code) {
            .staticVar => {
                const symId = pc[1].arg;
                const nameId = self.symIdToVar.get(symId).?.rtVarSymKey.nameId;
                const name = cy.sema.getName(&self.vm.compiler, nameId);
                extra = try std.fmt.bufPrint(&buf, "[sym={s}]", .{name});
            },
            .fieldRetain => {
                const symId = pc[3].arg;
                const name = self.vm.fieldSyms.buf[symId].name;
                extra = try std.fmt.bufPrint(&buf, "[sym={s}]", .{name});
            },
            else => {},
        }
        bytecode.dumpInst(pcOffset, code, pc, len, extra);
    }
};
