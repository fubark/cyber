const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypes;
const rt = cy.rt;
const fmt = @import("fmt.zig");
const bytecode = @import("bytecode.zig");
const v = fmt.v;
const log = cy.log.scoped(.debug);
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

/// TODO: Memoize this function.
pub fn getDebugSymByPc(vm: *const cy.VM, pc: usize) ?cy.DebugSym {
    return getDebugSymFromTable(vm.debugTable, pc);
}

pub fn getDebugSymByIndex(vm: *const cy.VM, idx: usize) cy.DebugSym {
    return vm.debugTable[idx];
}

pub fn getDebugTempIndex(vm: *const cy.VM, idx: usize) u32 {
    return vm.debugTempIndexTable[idx];
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
        if (trace.allocPc != cy.NullId) {
            const msg = try std.fmt.allocPrint(vm.alloc, "Allocated object: {*} at pc: {}({s})", .{
                obj, trace.allocPc, @tagName(vm.ops[trace.allocPc].opcode()),
            });
            defer vm.alloc.free(msg);
            try printTraceAtPc(vm, trace.allocPc, msg);
        } else {
            try printTraceAtPc(vm, trace.allocPc, "Allocated object");
        }

        if (trace.freePc != cy.NullId) {
            const msg = try std.fmt.allocPrint(vm.alloc, "Freed object: {}({s}) at pc: {}({s})", .{
                trace.freeTypeId, vm.getTypeName(trace.freeTypeId),
                trace.freePc, @tagName(vm.ops[trace.freePc].opcode()),
            });
            defer vm.alloc.free(msg);
            try printTraceAtPc(vm, trace.freePc, msg);
        } else {
            try printTraceAtPc(vm, trace.freePc, "Freed object");
        }
    } else {
        log.debug("No trace for {*}.", .{obj});
    }
}

pub fn printTraceAtPc(vm: *const cy.VM, pc: u32, msg: []const u8) !void {
    if (pc == cy.NullId) {
        fmt.printStderr("Trace: {} (external)\n", &.{v(msg)});
        return;
    }
    if (indexOfDebugSym(vm, pc)) |idx| {
        const sym = vm.debugTable[idx];
        const chunk = vm.compiler.chunks.items[sym.file];
        const node = chunk.nodes[sym.loc];
        const token = chunk.tokens[node.start_token];
        try printUserError(vm, "Trace", msg, sym.file, token.pos());
    } else {
        fmt.printStderr("{}\nMissing debug sym for {}, pc: {}.\n", &.{v(msg), v(vm.ops[pc].opcode()), v(pc)});
    }
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
    if (vm.compiler.lastErrChunk != cy.NullId) {
        const chunk = vm.compiler.chunks.items[vm.compiler.lastErrChunk];
        if (vm.compiler.lastErrNode != cy.NullId) {
            const token = chunk.nodes[vm.compiler.lastErrNode].start_token;
            const pos = chunk.tokens[token].pos();
            try writeUserError(vm, w, "CompileError", vm.compiler.lastErr, chunk.id, pos);
        } else {
            try writeUserError(vm, w, "CompileError", vm.compiler.lastErr, chunk.id, cy.NullId);
        }
    } else {
        try writeUserError(vm, w, "CompileError", vm.compiler.lastErr, cy.NullId, cy.NullId);
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
    if (chunkId != cy.NullId) {
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
    } else {
        try fmt.format(w,
            \\{}: {}
            \\
        , &.{
            v(title), v(msg),
        });
    }
}

pub fn allocPanicMsg(vm: *const cy.VM) ![]const u8 {
    switch (@as(cy.fiber.PanicType, @enumFromInt(vm.curFiber.panicType))) {
        .uncaughtError => {
            const str = try vm.getOrBufPrintValueStr(&cy.tempBuf, cy.Value{ .val = vm.curFiber.panicPayload });
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
            cy.panicFmt("Unexpected panic type. {}", .{vm.curFiber.panicType});
        },
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
        if (frame.chunkId != cy.NullId) {
            const chunk = vm.compiler.chunks.items[frame.chunkId];
            if (frame.lineStartPos != cy.NullId) {
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
            } else {
                // No source code attribution.
                try fmt.format(w,
                    \\{}: {}
                    \\
                , &.{
                    v(chunk.srcUri), v(frame.name),
                });
            }
        } else {
            // Host frame.
            try fmt.format(w,
                \\<host>: {}
                \\
            , &.{
                v(frame.name),
            });
        }
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

pub fn compactToStackFrame(vm: *cy.VM, stack: []const cy.Value, frame: vmc.CompactFrame) !StackFrame {
    if (frame.fpOffset == 0 or cy.fiber.isVmFrame(vm, stack, frame.fpOffset)) {
        const sym = getDebugSymByPc(vm, frame.pcOffset) orelse return error.NoDebugSym;
        return getStackFrame(vm, sym);
    } else {
        const func = stack[frame.fpOffset+2];
        if (func.isObjectType(bt.HostFunc)) {
            return StackFrame{
                .name = "call host func",
                .chunkId = cy.NullId,
                .line = 0,
                .col = 0,
                .lineStartPos = cy.NullId,
            };
        } else {
            var funcPc: u32 = undefined; 
            if (func.isObjectType(bt.Lambda)) {
                funcPc = func.asHeapObject().lambda.funcPc;
            } else if (func.isObjectType(bt.Closure)) {
                funcPc = func.asHeapObject().closure.funcPc;
            } else return error.Unexpected;

            // TODO: Determine lambda source from inspecting callee value.
            return StackFrame{
                .name = "call lambda",
                .chunkId = cy.NullId,
                .line = 0,
                .col = 0,
                .lineStartPos = cy.NullId,
            };
        }
    }
}

fn getStackFrame(vm: *cy.VM, sym: cy.DebugSym) StackFrame {
    if (sym.frameLoc == 0) {
        const chunk = vm.compiler.chunks.items[sym.file];
        if (sym.loc != cy.NullId) {
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
            // Invoking $init.
            return StackFrame{
                .name = "main",
                .chunkId = sym.file,
                .line = 0,
                .col = 0,
                .lineStartPos = cy.NullId,
            };
        }
    } else {
        const chunk = vm.compiler.chunks.items[sym.file]; 
        var name: []const u8 = undefined;
        if (sym.frameLoc != cy.NullId) {
            const node = chunk.nodes[sym.frameLoc];
            if (node.node_t == .coinit) {
                name = "coroutine";
            } else {
                const header = chunk.nodes[node.head.func.header];
                if (header.head.funcHeader.name == cy.NullId) {
                    name = "lambda";
                } else {
                    name = chunk.getNodeStringById(header.head.funcHeader.name);
                }
            }
        } else {
            name = "init";
        }

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

pub fn allocStackTrace(vm: *cy.VM, stack: []const cy.Value, cframes: []const vmc.CompactFrame) ![]const cy.StackFrame {
    @setCold(true);
    log.tracev("build stacktrace", .{});
    var frames = try vm.alloc.alloc(cy.StackFrame, cframes.len);
    for (cframes, 0..) |cframe, i| {
        frames[i] = try cy.debug.compactToStackFrame(vm, stack, cframe);
    }
    return frames;
}

pub const ObjectTrace = struct {
    /// Points to inst that allocated the object.
    allocPc: u32,

    /// Points to inst that freed the object.
    freePc: u32,

    /// Type when freed.
    freeTypeId: cy.TypeId,
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
    var instIdx: u32 = 0;
    const debugTable = vm.compiler.buf.debugTable.items;

    if (optPcContext) |pcContext| {
        const idx = indexOfDebugSymFromTable(debugTable, pcContext) orelse {
            if (getOpCodeAtPc(vm.compiler.buf.ops.items, pcContext)) |code| {
                return cy.panicFmt("Missing debug sym at {}, code={}", .{pcContext, code});
            } else {
                return cy.panicFmt("Missing debug sym at {}, Invalid pc", .{pcContext});
            }
        };
        const sym = debugTable[idx];
        const chunk = vm.compiler.chunks.items[sym.file];

        if (sym.frameLoc == 0) {
            fmt.printStderr("Block: main\n", &.{});
            const sblock = &chunk.semaBlocks.items[chunk.mainSemaBlockId];
            try chunk.dumpLocals(sblock);
        } else {
            // const funcNode = chunk.nodes[sym.frameLoc];
            return error.TODO;
            // const funcDecl = chunk.semaFuncDecls.items[funcNode.head.func.semaDeclId];
            // const funcName = funcDecl.getName(&chunk);
            // if (funcName.len == 0) {
            //     fmt.printStderr("Block: lambda()\n", &.{});
            // } else {
            //     fmt.printStderr("Block: {}()\n", &.{ v(funcName) });
            // }
            // const sblock = &chunk.semaBlocks.items[funcDecl.semaBlockId];
            // try chunk.dumpLocals(sblock);
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

        var curMarkerIdx: u32 = if (vm.compiler.buf.debugMarkers.items.len > 0) 0 else cy.NullId;
        var nextMarkerPc: u32 = if (curMarkerIdx == 0) vm.compiler.buf.debugMarkers.items[curMarkerIdx].pc else cy.NullId;

        // Print until requested inst.
        pc += pcOffset;
        while (pcOffset < pcContext) {
            if (pcOffset == nextMarkerPc) {
                dumpMarkerAdvance(vm, &curMarkerIdx, &nextMarkerPc);
            }

            const code = pc[0].opcode();
            const len = bytecode.getInstLenAt(pc);
            try dumpInst(vm, pcOffset, code, pc, instIdx);
            pcOffset += len;
            pc += len;
            instIdx += 1;
        }

        if (pcOffset == nextMarkerPc) {
            dumpMarkerAdvance(vm, &curMarkerIdx, &nextMarkerPc);
        }
        // Special marker for requested inst.
        fmt.printStderr("--", &.{});
        var code = pc[0].opcode();
        var len = bytecode.getInstLenAt(pc);
        try dumpInst(vm, pcOffset, code, pc, instIdx);
        pcOffset += len;
        pc += len;
        instIdx += 1;

        // Keep printing instructions until ContextSize or end is reached.
        var i: usize = 0;
        while (pcOffset < opsLen) {
            if (pcOffset == nextMarkerPc) {
                dumpMarkerAdvance(vm, &curMarkerIdx, &nextMarkerPc);
            }
            code = pc[0].opcode();
            len = bytecode.getInstLenAt(pc);
            try dumpInst(vm, pcOffset, code, pc, instIdx);
            pcOffset += len;
            pc += len;
            instIdx += 1;
            i += 1;
            if (i >= ContextSize) {
                break;
            }
        }
    } else {
        fmt.printStderr("Bytecode:\n", &.{});

        var curMarkerIdx: u32 = if (vm.compiler.buf.debugMarkers.items.len > 0) 0 else cy.NullId;
        var nextMarkerPc: u32 = if (curMarkerIdx == 0) vm.compiler.buf.debugMarkers.items[curMarkerIdx].pc else cy.NullId;

        while (pcOffset < opsLen) {
            if (pcOffset == nextMarkerPc) {
                dumpMarkerAdvance(vm, &curMarkerIdx, &nextMarkerPc);
            }

            const code = pc[0].opcode();
            const len = bytecode.getInstLenAt(pc);
            try dumpInst(vm, pcOffset, code, pc, instIdx);
            pcOffset += len;
            pc += len;
            instIdx += 1;
        }

        fmt.printStderr("\nConstants:\n", &.{});
        for (vm.compiler.buf.mconsts) |extra| {
            const val = cy.Value{ .val = extra.val };
            if (val.isFloat()) {
                fmt.printStderr("{}\n", &.{v(val.asF64())});
            } else {
                fmt.printStderr("{}\n", &.{v(extra.val)});
            }
        }
    }
}

fn dumpMarkerAdvance(vm: *const cy.VM, curMarkerIdx: *u32, nextMarkerPc: *u32) void {
    const marker = vm.compiler.buf.debugMarkers.items[curMarkerIdx.*];
    switch (marker.etype()) {
        .label => {
            fmt.printStderr("{}:\n", &.{v(marker.getLabelName())});
        },
        .funcStart => {
            fmt.printStderr("---- func begin: {}\n", &.{v(marker.data.funcStart.func.name())});
        },
        .funcEnd => {
            fmt.printStderr("---- func end: {}\n", &.{v(marker.data.funcEnd.func.name())});
        },
    }
    curMarkerIdx.* += 1;
    if (curMarkerIdx.* == vm.compiler.buf.debugMarkers.items.len) {
        nextMarkerPc.* = cy.NullId;
    } else {
        nextMarkerPc.* = vm.compiler.buf.debugMarkers.items[curMarkerIdx.*].pc;
    }
}

pub fn dumpInst(vm: *const cy.VM, pcOffset: u32, code: cy.OpCode, pc: [*]const cy.Inst, instIdx: u32) !void {
    var buf: [1024]u8 = undefined;
    var extra: []const u8 = "";
    switch (code) {
        .staticVar => {
            if (cy.Trace) {
                const symId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
                const name = vm.varSymExtras.buf[symId].name();
                extra = try std.fmt.bufPrint(&buf, "[sym={s}]", .{name});
            }
        },
        else => {
            if (cy.Trace) {
                const desc = vm.compiler.buf.instDescs.items[instIdx];
                if (desc.nodeId != cy.NullId) {
                    var fbuf = std.io.fixedBufferStream(&buf);
                    var w = fbuf.writer();

                    if (desc.extraIdx != cy.NullId) {
                        const descExtra = vm.compiler.buf.instDescExtras.items[desc.extraIdx];
                        try w.writeByte('"');
                        try w.writeAll(descExtra.text);
                        try w.writeAll("\" ");
                    }

                    const chunk = vm.compiler.chunks.items[desc.chunkId];

                    const enc = cy.ast.Encoder{
                        .src = .{
                            .src = chunk.src,
                            .nodes = chunk.nodes,
                            .tokens = chunk.tokens,
                        },
                    };

                    var nodeId = desc.nodeId;
                    if (chunk.nodes[nodeId].hasParentAssignStmt) {
                        // Print the entire statement instead.
                        nodeId = enc.src.getParentAssignStmt(nodeId);
                    }
                    try enc.writeNode(w, nodeId);
                    extra = fbuf.getWritten();
                }
            }
        },
    }
    try bytecode.dumpInst(pcOffset, code, pc, extra);
}

const EnableTimerTrace = builtin.mode == .Debug;

const TimerTrace = struct {
    timer: if (EnableTimerTrace) stdx.time.Timer else void,

    pub fn end(self: *TimerTrace) void {
        if (EnableTimerTrace) {
            const now = self.timer.read();
            log.info("time: {d:.3}ms", .{ @as(f32, @floatFromInt(now)) / 1e6 });
        }
    }

    pub fn endPrint(self: *TimerTrace, msg: []const u8) void {
        if (EnableTimerTrace) {
            const now = self.timer.read();
            if (builtin.mode == .ReleaseFast) {
                std.debug.print("{s}: {d:.3}ms\n", .{ msg, @as(f32, @floatFromInt(now)) / 1e6 });
            } else {
                log.info("{s}: {d:.3}ms", .{ msg, @as(f32, @floatFromInt(now)) / 1e6 });
            }
        }
    }
};

// Simple trace with std Timer.
pub fn timer() TimerTrace {
    if (EnableTimerTrace) {
        return .{
            .timer = stdx.time.Timer.start() catch unreachable,
        };
    } else {
        return .{
            .timer = {},
        };
    }
}
