const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const cc = @import("capi.zig");
const bt = cy.types.BuiltinTypes;
const rt = cy.rt;
const fmt = @import("fmt.zig");
const bytecode = @import("bytecode.zig");
const v = fmt.v;
const log = cy.log.scoped(.debug);
const vmc = @import("vm_c.zig");

const NullId = std.math.maxInt(u32);

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

pub fn indexOfDebugSym(vm: *const cy.VM, pc: usize) !usize {
    return getIndexOfDebugSym(vm, pc) orelse {
        log.trace("at pc: {}", .{pc});
        return error.NoDebugSym;
    };
}

pub fn getIndexOfDebugSym(vm: *const cy.VM, pc: usize) ?usize {
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

pub fn dumpObjectTrace(vm: *cy.VM, obj: *cy.HeapObject) !void {
    if (vm.objectTraceMap.get(obj)) |trace| {
        if (trace.allocPc != cy.NullId) {
            const msg = try std.fmt.allocPrint(vm.alloc, "{*} at pc: {}({s})", .{
                obj, trace.allocPc, @tagName(vm.ops[trace.allocPc].opcode()),
            });
            defer vm.alloc.free(msg);
            try printTraceAtPc(vm, trace.allocPc, "alloced", msg);
        } else {
            try printTraceAtPc(vm, trace.allocPc, "alloced", "");
        }

        if (trace.freePc != cy.NullId) {
            const msg = try std.fmt.allocPrint(vm.alloc, "{}({s}) at pc: {}({s})", .{
                trace.freeTypeId, vm.getTypeName(trace.freeTypeId),
                trace.freePc, @tagName(vm.ops[trace.freePc].opcode()),
            });
            defer vm.alloc.free(msg);
            try printTraceAtPc(vm, trace.freePc, "freed", msg);
        } else {
            try printTraceAtPc(vm, trace.freePc, "freed", "");
        }
    } else {
        log.tracev("No trace for {*}.", .{obj});
    }
}

pub fn printTraceAtNode(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const token = c.nodes[nodeId].start_token;
    const pos = c.tokens[token].pos();
    const w = fmt.lockStderrWriter();
    defer fmt.unlockPrint();
    try writeUserError(c.compiler.vm, w, "Trace", "", c.id, pos);
}

pub fn printTraceAtPc(vm: *cy.VM, pc: u32, title: []const u8, msg: []const u8) !void {
    if (pc == cy.NullId) {
        rt.printErrorFmt(vm, "{}: {} (external)\n", &.{v(title), v(msg)});
        return;
    }
    if (getIndexOfDebugSym(vm, pc)) |idx| {
        const sym = vm.debugTable[idx];
        const chunk = vm.compiler.chunks.items[sym.file];
        const node = chunk.ast.node(sym.loc);
        try printUserError(vm, title, msg, sym.file, node.srcPos);
    } else {
        rt.printErrorFmt(vm, "{}: {}\nMissing debug sym for {}, pc: {}.\n", &.{
            v(title), v(msg), v(vm.ops[pc].opcode()), v(pc)});
    }
}

pub fn allocLastUserPanicError(vm: *const cy.VM) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    const w = buf.writer(vm.alloc);
    try writeLastUserPanicError(vm, w);
    return buf.toOwnedSlice(vm.alloc);
}

pub fn printLastUserPanicError(vm: *cy.VM) !void {
    if (cc.silent()) {
        return;
    }
    var w = rt.ErrorWriter{ .c = vm };
    try writeLastUserPanicError(vm, w);
}

fn writeLastUserPanicError(vm: *const cy.VM, w: anytype) !void {
    const msg = try allocPanicMsg(vm);
    defer vm.alloc.free(msg);

    try fmt.format(w, "panic: {}\n\n", &.{v(msg)});
    const trace = vm.getStackTrace();
    try writeStackFrames(vm, w, trace.frames);
}

pub fn allocReportSummary(c: *const cy.Compiler, report: cy.Report) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    const w = buf.writer(c.alloc);
    switch (report.type) {
        .token_err => {
            try writeUserError(c, w, "TokenError", report.msg, report.chunk, report.loc);
        },
        .parse_err => {
            try writeUserError(c, w, "ParseError", report.msg, report.chunk, report.loc);
        },
        .compile_err => {
            try writeCompileErrorSummary(c, w, report);
        },
    }
    return buf.toOwnedSlice(c.alloc);
}

fn writeCompileErrorSummary(c: *const cy.Compiler, w: anytype, report: cy.Report) !void {
    if (report.chunk != cy.NullId) {
        const chunk = c.chunks.items[report.chunk];
        if (report.loc != cy.NullId) {
            const node = chunk.ast.node(report.loc);
            try writeUserError(c, w, "CompileError", report.msg, report.chunk, node.srcPos);
        } else {
            try writeUserError(c, w, "CompileError", report.msg, report.chunk, cy.NullId);
        }
    } else {
        try writeUserError(c, w, "CompileError", report.msg, cy.NullId, cy.NullId);
    }
}

pub fn printUserError(vm: *cy.VM, title: []const u8, msg: []const u8, chunkId: u32, pos: u32) !void {
    if (cc.silent()) {
        return;
    }
    var w = rt.ErrorWriter{ .c = vm };
    try writeUserError(vm.compiler, w, title, msg, chunkId, pos);
}

/// Reduced to using writer so printed errors can be tested.
pub fn writeUserError(c: *const cy.Compiler, w: anytype, title: []const u8, msg: []const u8, chunkId: u32, pos: u32) !void {
    if (chunkId != cy.NullId) {
        const chunk = c.chunks.items[chunkId];
        if (pos != NullId) {
            var line: u32 = undefined;
            var col: u32 = undefined;
            var lineStart: u32 = undefined;
            chunk.ast.computeLinePos(pos, &line, &col, &lineStart);
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

/// Can only rely on pc and other non-reference values to build the stack frame since
/// unwinding could have already freed reference values.
pub fn compactToStackFrame(vm: *cy.VM, stack: []const cy.Value, frame: vmc.CompactFrame) !StackFrame {
    if (frame.pcOffset != cy.NullId) {
        const sym = getDebugSymByPc(vm, frame.pcOffset) orelse {
            log.trace("at pc: {}", .{frame.pcOffset});
            return error.NoDebugSym;
        };
        return getStackFrame(vm, sym);
    } else {
        // External frame.
        const func_type = stack[frame.fpOffset-1].val;
        if (func_type == bt.HostFunc) {
            return StackFrame{
                .name = "call host func",
                .chunkId = cy.NullId,
                .line = 0,
                .col = 0,
                .lineStartPos = cy.NullId,
            };
        } else {
            // TODO: If there is a function debug table, the function name could be 
            //       found with a pc value.
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
    if (sym.frameLoc == cy.NullNode) {
        const chunk = vm.compiler.chunks.items[sym.file];
        if (sym.loc != cy.NullNode) {
            const node = chunk.ast.node(sym.loc);
            var line: u32 = undefined;
            var col: u32 = undefined;
            var lineStart: u32 = undefined;
            chunk.ast.computeLinePos(node.srcPos, &line, &col, &lineStart);
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
        const proc = chunk.ast.node(sym.frameLoc);
        if (proc.type() == .coinit) {
            name = "coroutine";
        } else {
            const header = chunk.ast.node(proc.data.func.header);
            if (header.data.funcHeader.name == cy.NullNode) {
                name = "lambda";
            } else {
                name = chunk.ast.nodeStringById(header.data.funcHeader.name);
            }
        }

        const node = chunk.ast.node(sym.loc);
        var line: u32 = undefined;
        var col: u32 = undefined;
        var lineStart: u32 = undefined;
        chunk.ast.computeLinePos(node.srcPos, &line, &col, &lineStart);
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
    log.tracev("build stacktrace {}", .{cframes.len});
    var frames = try vm.alloc.alloc(cy.StackFrame, cframes.len);
    for (cframes, 0..) |cframe, i| {
        log.tracev("build stackframe pc={}, fp={}", .{cframe.pcOffset, cframe.fpOffset});
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

fn getPcInstIdx(ops: []const cy.Inst, atPc: u32) ?usize {
    var idx: usize = 0;
    var i: usize = 0;
    while (i < ops.len) {
        if (i == atPc) {
            return idx;
        }
        i += bytecode.getInstLenAt(ops.ptr + i);
        idx += 1;
    }
    return null;
}

const DumpBytecodeOptions = struct {
    pcContext: ?u32 = null,
};

/// When `optPcContext` is null, all the bytecode is dumped along with constants.
/// When `optPcContext` is non null, it will dump a trace at `optPcContext` and the surrounding bytecode with extra details.
pub fn dumpBytecode(vm: *cy.VM, opts: DumpBytecodeOptions) !void {
    var pcOffset: u32 = 0;
    var opsLen = vm.compiler.buf.ops.items.len;
    var pc = vm.compiler.buf.ops.items.ptr;
    var instIdx: u32 = 0;
    const debugTable = vm.compiler.buf.debugTable.items;

    if (opts.pcContext) |pcContext| {
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
            // rt.print(vm, "Block: main");
            // const proc = &chunk.semaProcs.items[chunk.mainSemaProcId];
            // try chunk.dumpLocals(proc);
        } else {
            // const funcNode = chunk.nodes[sym.frameLoc];
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
        rt.print(vm, "\n");

        const node = chunk.ast.node(sym.loc);
        const msg = try std.fmt.allocPrint(vm.alloc, "pc={} op={s} node={s}", .{ pcContext, @tagName(pc[pcContext].opcode()), @tagName(node.type()) });
        defer vm.alloc.free(msg);
        try printUserError(vm, "Trace", msg, sym.file, node.srcPos);

        rt.print(vm, "Bytecode:\n");
        const ContextSize = 40;
        const startSymIdx = if (idx >= ContextSize) idx - ContextSize else 0;
        pcOffset = debugTable[startSymIdx].pc;

        var curMarkerIdx: u32 = if (vm.compiler.buf.debugMarkers.items.len > 0) 0 else cy.NullId;
        var nextMarkerPc: u32 = if (curMarkerIdx == 0) vm.compiler.buf.debugMarkers.items[curMarkerIdx].pc else cy.NullId;

        // Print until requested inst.
        pc += pcOffset;
        instIdx = @intCast(getPcInstIdx(vm.compiler.buf.ops.items, pcOffset).?);
        while (pcOffset < pcContext) {
            if (pcOffset == nextMarkerPc) {
                dumpMarkerAdvance(vm, &curMarkerIdx, &nextMarkerPc);
            }

            const code = pc[0].opcode();
            const len = bytecode.getInstLenAt(pc);
            try dumpInst(vm, pcOffset, code, pc, instIdx, null);
            pcOffset += len;
            pc += len;
            instIdx += 1;
        }

        if (pcOffset == nextMarkerPc) {
            dumpMarkerAdvance(vm, &curMarkerIdx, &nextMarkerPc);
        }
        // Special marker for requested inst.
        var code = pc[0].opcode();
        var len = bytecode.getInstLenAt(pc);
        try dumpInst(vm, pcOffset, code, pc, instIdx, "--");
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
            try dumpInst(vm, pcOffset, code, pc, instIdx, null);
            pcOffset += len;
            pc += len;
            instIdx += 1;
            i += 1;
            if (i >= ContextSize) {
                break;
            }
        }
    } else {
        rt.print(vm, "Bytecode:\n");

        var curMarkerIdx: u32 = if (vm.compiler.buf.debugMarkers.items.len > 0) 0 else cy.NullId;
        var nextMarkerPc: u32 = if (curMarkerIdx == 0) vm.compiler.buf.debugMarkers.items[curMarkerIdx].pc else cy.NullId;

        while (pcOffset < opsLen) {
            while (pcOffset == nextMarkerPc) {
                dumpMarkerAdvance(vm, &curMarkerIdx, &nextMarkerPc);
            }

            const code = pc[0].opcode();
            const len = bytecode.getInstLenAt(pc);
            try dumpInst(vm, pcOffset, code, pc, instIdx, null);
            pcOffset += len;
            pc += len;
            instIdx += 1;
        }

        rt.printFmt(vm, "\nConstants ({}):\n", &.{v(vm.compiler.buf.mconsts.len)});
        for (vm.compiler.buf.mconsts) |extra| {
            const val = cy.Value{ .val = extra.val };
            const str = try vm.bufPrintValueShortStr(&vm.tempBuf, val);
            rt.printFmt(vm, "{}\n", &.{v(str)});
        }
    }
}

fn dumpMarkerAdvance(vm: *cy.VM, curMarkerIdx: *u32, nextMarkerPc: *u32) void {
    const marker = vm.compiler.buf.debugMarkers.items[curMarkerIdx.*];
    switch (marker.etype()) {
        .label => {
            rt.printFmt(vm, "{}:\n", &.{v(marker.getLabelName())});
        },
        .funcStart => {
            rt.printFmt(vm, "---- func begin: {}\n", &.{v(marker.data.funcStart.func.name())});
        },
        .funcEnd => {
            rt.printFmt(vm, "---- func end: {}\n", &.{v(marker.data.funcEnd.func.name())});
        },
    }
    curMarkerIdx.* += 1;
    if (curMarkerIdx.* == vm.compiler.buf.debugMarkers.items.len) {
        nextMarkerPc.* = cy.NullId;
    } else {
        nextMarkerPc.* = vm.compiler.buf.debugMarkers.items[curMarkerIdx.*].pc;
    }
}

pub fn dumpInst(vm: *cy.VM, pcOffset: u32, code: cy.OpCode, pc: [*]const cy.Inst, instIdx: u32, prefix: ?[]const u8) !void {
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
                        .ast = chunk.ast,
                    };

                    try enc.write(w, desc.nodeId);
                    extra = fbuf.getWritten();
                }
            }
        },
    }
    try bytecode.dumpInst(vm, pcOffset, code, pc, .{ .extra = extra, .prefix = prefix });
}

const EnableTimerTrace = builtin.os.tag != .freestanding and builtin.mode == .Debug;

const TimerTrace = struct {
    timer: if (EnableTimerTrace) stdx.time.Timer else void,

    pub fn end(self: *TimerTrace) void {
        if (EnableTimerTrace and cy.verbose) {
            const now = self.timer.read();
            cy.log.zfmt("time: {d:.3}ms", .{ @as(f32, @floatFromInt(now)) / 1e6 });
        }
    }

    pub fn endPrint(self: *TimerTrace, msg: []const u8) void {
        if (EnableTimerTrace and cc.verbose()) {
            const now = self.timer.read();
            cy.log.zfmt("{s}: {d:.3}ms", .{ msg, @as(f32, @floatFromInt(now)) / 1e6 });
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
