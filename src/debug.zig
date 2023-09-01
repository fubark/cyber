const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const rt = cy.rt;
const fmt = @import("fmt.zig");
const bytecode = @import("bytecode.zig");
const v = fmt.v;
const log = stdx.log.scoped(.debug);
const vmc = @import("vm_c.zig");

const NullId = std.math.maxInt(u32);

/// Find the line/col in `src` at `pos`.
/// Iterating tokens could be faster but it would still require counting new lines for skipped segments like comments, multiline strings.
pub fn computeLinePos(src: []const u8, pos: u32, outLine: *u32, outCol: *u32, outLineStart: *u32) linksection(cy.Section) void {
    var line: u32 = 0;
    var lineStart: u32 = 0;
    for (src, 0..) |ch, i| {
        if (i == pos) {
            break;
        }
        if (ch == '\n') {
            line += 1;
            lineStart = @intCast(i + 1);
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
        const msg = try std.fmt.allocPrint(vm.alloc, "Allocated object: {*} at pc: {}({s})", .{
            obj, trace.allocPc, @tagName(vm.ops[trace.allocPc].opcode()),
        });
        defer vm.alloc.free(msg);
        try printTraceAtPc(vm, trace.allocPc, msg);

        if (trace.freePc != cy.NullId) {
            const msg2 = try std.fmt.allocPrint(vm.alloc, "Last freed at pc: {}({s}) with type: {}({s})", .{
                trace.freePc, @tagName(vm.ops[trace.freePc].opcode()), trace.freeTypeId, cy.heap.getTypeName(vm, trace.freeTypeId),
            });
            defer vm.alloc.free(msg2);
            try printTraceAtPc(vm, trace.freePc, msg2);
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
        try printUserError(vm, "Trace", msg, sym.file, token.pos());
    } else {
        if (pc == cy.NullId) {
            fmt.printStderr("Trace: {} (vm global allocation)\n", &.{v(msg)});
        } else {
            fmt.printStderr("{}\nMissing debug sym for {}, pc: {}.\n", &.{v(msg), v(vm.ops[pc].opcode()), v(pc)});
        }
    }
}

pub inline fn atLeastTestDebugLevel() bool {
    return @intFromEnum(std.testing.log_level) >= @intFromEnum(std.log.Level.debug);
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
    try writeStackFrames(vm, w, trace.frames);
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
        try writeUserError(vm, w, "CompileError", vm.compiler.lastErr, chunk.id, pos);
    } else {
        try writeUserError(vm, w, "CompileError", vm.compiler.lastErr, chunk.id, cy.NullId);
    }
}

pub fn allocLastUserParseError(vm: *const cy.VM) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    const w = buf.writer(vm.alloc);
    try writeLastUserParseError(vm, w);
    return buf.toOwnedSlice(vm.alloc);
}

pub fn allocLastUserTokenError(vm: *const cy.VM) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    const w = buf.writer(vm.alloc);
    try writeLastUserTokenError(vm, w);
    return buf.toOwnedSlice(vm.alloc);
}

fn writeLastUserTokenError(vm: *const cy.VM, w: anytype) !void {
    const chunk = vm.compiler.chunks.items[vm.compiler.lastErrChunk];
    try writeUserError(vm, w, "TokenError", chunk.parser.last_err, chunk.id, chunk.parser.last_err_pos);
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
    try writeUserError(vm, w, "ParseError", chunk.parser.last_err, chunk.id, chunk.parser.last_err_pos);
}

pub fn printLastUserParseError(vm: *const cy.VM) !void {
    if (cy.silentError) {
        return;
    }
    const w = fmt.lockStderrWriter();
    defer fmt.unlockPrint();
    try writeLastUserParseError(vm, w);
}

pub fn printUserError(vm: *const cy.VM, title: []const u8, msg: []const u8, chunkId: u32, pos: u32) linksection(cy.Section) !void {
    if (cy.silentError) {
        return;
    }
    const w = fmt.lockStderrWriter();
    defer fmt.unlockPrint();
    try writeUserError(vm, w, title, msg, chunkId, pos);
}

/// Reduced to using writer so printed errors can be tested.
pub fn writeUserError(vm: *const cy.VM, w: anytype, title: []const u8, msg: []const u8, chunkId: u32, pos: u32) linksection(cy.Section) !void {
    const chunk = vm.compiler.chunks.items[chunkId];
    if (pos != NullId) {
        var line: u32 = undefined;
        var col: u32 = undefined;
        var lineStart: u32 = undefined;
        computeLinePos(chunk.parser.src, pos, &line, &col, &lineStart);
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

pub fn allocPanicMsg(vm: *const cy.VM) ![]const u8 {
    switch (@as(cy.fiber.PanicType, @enumFromInt(vm.curFiber.panicType))) {
        .uncaughtError => {
            const str = vm.valueToTempString(cy.Value{ .val = vm.curFiber.panicPayload });
            return try fmt.allocFormat(vm.alloc, "{}", &.{v(str)});
        },
        .msg => {
            const ptr: usize = @intCast(vm.curFiber.panicPayload & ((1 << 48) - 1));
            const len: usize = @intCast(vm.curFiber.panicPayload >> 48);
            // Check for zero delimited.
            const str = @as([*]const u8, @ptrFromInt(ptr))[0..len];
            if (str[len-1] == 0) {
                return vm.alloc.dupe(u8, str[0..len-1]);
            } else {
                return vm.alloc.dupe(u8, str);
            }
        },
        .staticMsg => {
            const ptr: usize = @intCast(vm.curFiber.panicPayload & ((1 << 48) - 1));
            const len: usize = @intCast(vm.curFiber.panicPayload >> 48);
            return vm.alloc.dupe(u8, @as([*]const u8, @ptrFromInt(ptr))[0..len]);
        },
        .inflightOom,
        .nativeThrow,
        .none => {
            stdx.panicFmt("Unexpected panic type. {}", .{vm.curFiber.panicType});
        },
    }
}

pub fn freePanicPayload(vm: *const cy.VM) void {
    switch (@as(cy.fiber.PanicType, @enumFromInt(vm.curFiber.panicType))) {
        .uncaughtError => {},
        .msg => {
            const ptr: usize = @intCast(vm.curFiber.panicPayload & ((1 << 48) - 1));
            const len: usize = @intCast(vm.curFiber.panicPayload >> 48);
            vm.alloc.free(@as([*]const u8, @ptrFromInt(ptr))[0..len]);
        },
        .staticMsg,
        .inflightOom,
        .nativeThrow,
        .none => {},
    }
}

pub const StackTrace = struct {
    frames: []const StackFrame = &.{},

    pub fn deinit(self: *StackTrace, alloc: std.mem.Allocator) void {
        alloc.free(self.frames);
        self.frames = &.{};
    }

    pub fn dump(self: *const StackTrace, vm: *const cy.VM) !void {
        const w = fmt.lockStderrWriter();
        defer fmt.unlockPrint();
        try writeStackFrames(vm, w, self.frames);
    }
};

pub fn writeStackFrames(vm: *const cy.VM, w: anytype, frames: []const StackFrame) !void {
    for (frames) |frame| {
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

test "debug internals." {
    try t.eq(@sizeOf(vmc.CompactFrame), 8);
}

pub fn compactToStackFrame(vm: *cy.VM, cframe: vmc.CompactFrame) !StackFrame {
    const sym = getDebugSym(vm, cframe.pcOffset) orelse return error.NoDebugSym;
    return getStackFrame(vm, sym);
}

fn getStackFrame(vm: *cy.VM, sym: cy.DebugSym) StackFrame {
    if (sym.frameLoc == cy.NullId) {
        const chunk = vm.compiler.chunks.items[sym.file];
        const node = chunk.nodes[sym.loc];
        var line: u32 = undefined;
        var col: u32 = undefined;
        var lineStart: u32 = undefined;
        const pos = chunk.tokens[node.start_token].pos();
        computeLinePos(chunk.parser.src, pos, &line, &col, &lineStart);
        return StackFrame{
            .name = "main",
            .chunkId = sym.file,
            .line = line,
            .col = col,
            .lineStartPos = lineStart,
        };
    } else {
        const chunk = vm.compiler.chunks.items[sym.file];
        const frameNode = chunk.nodes[sym.frameLoc];
        const func = chunk.semaFuncDecls.items[frameNode.head.func.semaDeclId];
        const name = func.getName(&chunk);

        const node = chunk.nodes[sym.loc];
        var line: u32 = undefined;
        var col: u32 = undefined;
        var lineStart: u32 = undefined;
        const pos = chunk.tokens[node.start_token].pos();
        computeLinePos(chunk.parser.src, pos, &line, &col, &lineStart);
        return StackFrame{
            .name = name,
            .chunkId = sym.file,
            .line = line,
            .col = col,
            .lineStartPos = lineStart,
        };
    }
}

pub fn buildStackTrace(self: *cy.VM) !void {
    @setCold(true);
    self.stackTrace.deinit(self.alloc);
    var frames: std.ArrayListUnmanaged(StackFrame) = .{};

    var fpOffset = cy.getStackOffset(self, self.framePtr);
    var pcOffset = cy.getInstOffset(self, self.pc);
    while (true) {
        const sym = getDebugSym(self, pcOffset) orelse return error.NoDebugSym;

        const frame = getStackFrame(self, sym);
        try frames.append(self.alloc, frame);
        if (sym.frameLoc == cy.NullId) {
            break;
        } else {
            pcOffset = cy.getInstOffset(self, self.stack[fpOffset + 2].retPcPtr) - self.stack[fpOffset + 1].retInfoCallInstOffset();
            fpOffset = cy.getStackOffset(self, self.stack[fpOffset + 3].retFramePtr);
        }
    }

    self.stackTrace.frames = try frames.toOwnedSlice(self.alloc);
}

/// Given pc position, return the end locals pc in the same frame.
/// TODO: Memoize this function.
pub fn pcToEndLocalsPc(vm: *const cy.VM, pc: usize) u32 {
    const idx = indexOfDebugSym(vm, pc) orelse {
        stdx.panicFmt("Missing debug symbol: {}", .{vm.ops[pc].opcode()});
    };
    const sym = vm.debugTable[idx];
    if (sym.frameLoc != cy.NullId) {
        const chunk = vm.compiler.chunks.items[sym.file];
        const node = chunk.nodes[sym.frameLoc];
        return chunk.semaFuncDecls.items[node.head.func.semaDeclId].genEndLocalsPc;
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
        return chunk.semaFuncDecls.items[node.head.func.semaDeclId].genEndLocalsPc;
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

    /// Type when freed.
    freeTypeId: rt.TypeId,
};

fn getOpCodeAtPc(ops: []const cy.Inst, atPc: u32) ?cy.OpCode {
    var i: usize = 0;
    while (i < ops.len) {
        if (i == atPc) {
            return ops[i].opcode();
        }
        i += bytecode.getInstLenAt(ops.ptr + i);
    }
    return null;
}

/// When `optPcContext` is null, all the bytecode is dumped along with constants.
/// When `optPcContext` is non null, it will dump a trace at `optPcContext` and the surrounding bytecode with extra details.
pub fn dumpBytecode(vm: *const cy.VM, optPcContext: ?u32) !void {
    var pcOffset: u32 = 0;
    var opsLen = vm.compiler.buf.ops.items.len;
    var pc = vm.compiler.buf.ops.items.ptr;
    const debugTable = vm.compiler.buf.debugTable.items;

    if (optPcContext) |pcContext| {
        const idx = indexOfDebugSymFromTable(debugTable, pcContext) orelse {
            if (getOpCodeAtPc(vm.compiler.buf.ops.items, pcContext)) |code| {
                return stdx.panicFmt("Missing debug sym at {}, code={}", .{pcContext, code});
            } else {
                return stdx.panicFmt("Missing debug sym at {}, Invalid pc", .{pcContext});
            }
        };
        const sym = debugTable[idx];
        const chunk = vm.compiler.chunks.items[sym.file];

        if (sym.frameLoc == cy.NullId) {
            fmt.printStderr("Block: main\n", &.{});
            const sblock = &chunk.semaBlocks.items[chunk.mainSemaBlockId];
            try chunk.dumpLocals(sblock);
        } else {
            const funcNode = chunk.nodes[sym.frameLoc];
            const funcDecl = chunk.semaFuncDecls.items[funcNode.head.func.semaDeclId];
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

        const node = chunk.nodes[sym.loc];
        const token = chunk.tokens[node.start_token];
        const msg = try std.fmt.allocPrint(vm.alloc, "pc={} op={s} node={s}", .{ pcContext, @tagName(pc[pcContext].opcode()), @tagName(node.node_t) });
        defer vm.alloc.free(msg);
        try printUserError(vm, "Trace", msg, sym.file, token.pos());

        fmt.printStderr("Bytecode:\n", &.{});
        const ContextSize = 40;
        const startSymIdx = if (idx >= ContextSize) idx - ContextSize else 0;
        pcOffset = debugTable[startSymIdx].pc;

        var curLabelIdx: u32 = if (vm.compiler.buf.debugLabels.items.len > 0) 0 else cy.NullId;
        var nextLabelPc: u32 = if (curLabelIdx == 0) vm.compiler.buf.debugLabels.items[curLabelIdx].pc else cy.NullId;

        // Print until requested inst.
        pc += pcOffset;
        while (pcOffset < pcContext) {
            if (pcOffset == nextLabelPc) {
                dumpLabelAdvance(vm, &curLabelIdx, &nextLabelPc);
            }

            const code = pc[0].opcode();
            const len = bytecode.getInstLenAt(pc);
            try dumpInst(vm, pcOffset, code, pc, len);
            pcOffset += len;
            pc += len;
        }

        if (pcOffset == nextLabelPc) {
            dumpLabelAdvance(vm, &curLabelIdx, &nextLabelPc);
        }
        // Special marker for requested inst.
        fmt.printStderr("--", &.{});
        var code = pc[0].opcode();
        var len = bytecode.getInstLenAt(pc);
        try dumpInst(vm, pcOffset, code, pc, len);
        pcOffset += len;
        pc += len;

        // Keep printing instructions until ContextSize or end is reached.
        var i: usize = 0;
        while (pcOffset < opsLen) {
            if (pcOffset == nextLabelPc) {
                dumpLabelAdvance(vm, &curLabelIdx, &nextLabelPc);
            }
            code = pc[0].opcode();
            len = bytecode.getInstLenAt(pc);
            try dumpInst(vm, pcOffset, code, pc, len);
            pcOffset += len;
            pc += len;
            i += 1;
            if (i >= ContextSize) {
                break;
            }
        }
    } else {
        fmt.printStderr("Bytecode:\n", &.{});

        var curLabelIdx: u32 = if (vm.compiler.buf.debugLabels.items.len > 0) 0 else cy.NullId;
        var nextLabelPc: u32 = if (curLabelIdx == 0) vm.compiler.buf.debugLabels.items[curLabelIdx].pc else cy.NullId;

        while (pcOffset < opsLen) {
            if (pcOffset == nextLabelPc) {
                dumpLabelAdvance(vm, &curLabelIdx, &nextLabelPc);
            }

            const code = pc[0].opcode();
            const len = bytecode.getInstLenAt(pc);
            try dumpInst(vm, pcOffset, code, pc, len);
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

fn dumpLabelAdvance(vm: *const cy.VM, curLabelIdx: *u32, nextLabelPc: *u32) void {
    fmt.printStderr("{}:\n", &.{v(vm.compiler.buf.debugLabels.items[curLabelIdx.*].getName())});
    curLabelIdx.* += 1;
    if (curLabelIdx.* == vm.compiler.buf.debugLabels.items.len) {
        nextLabelPc.* = cy.NullId;
    } else {
        nextLabelPc.* = vm.compiler.buf.debugLabels.items[curLabelIdx.*].pc;
    }
}

pub fn dumpInst(vm: *const cy.VM, pcOffset: u32, code: cy.OpCode, pc: [*]const cy.Inst, len: u32) !void {
    var buf: [1024]u8 = undefined;
    var extra: []const u8 = "";
    switch (code) {
        .staticVar => {
            const symId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const nameId = vm.varSymExtras.get(symId).?;
            const name = cy.sema.getName(&vm.compiler, nameId);
            extra = try std.fmt.bufPrint(&buf, "[sym={s}]", .{name});
        },
        .fieldRetain => {
            const symId = pc[3].val;
            const sym = vm.fieldSyms.buf[symId];
            const name = cy.sema.getName(&vm.compiler, sym.nameId);
            extra = try std.fmt.bufPrint(&buf, "[sym={s}]", .{name});
        },
        .callObjSym => {
            const symId = pc[4].val;
            const symName = vm.methodGroupExts.buf[symId].getName();
            extra = try std.fmt.bufPrint(&buf, "[sym={s}]", .{symName});
        },
        else => {},
    }
    bytecode.dumpInst(pcOffset, code, pc, len, extra);
}
