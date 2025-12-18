const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const zt = stdx.testing;
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const bt = cy.types.BuiltinTypes;
const fmt = @import("fmt.zig");
const bytecode = @import("bytecode.zig");
const v = fmt.v;
const logger = cy.log.scoped(.debug);
const vmc = @import("vmc");
const ast = cy.ast;

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

const FuncMetadata = struct {
    ret_size: u16,
    stack_size: u16,
};

pub fn get_pc_func_metadata(vm: *const cy.VM, pc: [*]cy.Inst) FuncMetadata {
    const table_idx = indexOfTableEntry(vm.debug_tables, pc) orelse @panic("unexpected");
    const entry = vm.debug_tables[table_idx];
    const S = struct {
        fn compare(cx: usize, o: usize) std.math.Order {
            return std.math.order(cx, o);
        }
    };
    const func_table = entry.chunk.buf.func_table.items;
    const idx = std.sort.upperBound(usize, func_table, pc - entry.chunk.buf.ops.items.ptr, S.compare);
    const metadata_pc = entry.chunk.buf.ops.items.ptr + func_table[idx-1];

    const ret_size = @as(*const align(1) u16, @ptrCast(metadata_pc + 1)).*;
    const stack_size = @as(*const align(1) u16, @ptrCast(metadata_pc + 3)).*;
    return .{
        .ret_size = ret_size,
        .stack_size = stack_size,
    };
}

pub fn getDebugSymByPc(vm: *const cy.VM, pc: *anyopaque) ?cy.DebugSym {
    const res = getIndexOfDebugSym(vm, pc) orelse return null;
    return res.chunk.buf.debugTable.items[res.idx];
}

pub fn getDebugSymFromTable(table: []const cy.DebugSym, pc: *anyopaque) ?cy.DebugSym {
    const idx = indexOfDebugSymFromTable(table, pc) orelse return null;
    return table[idx];
}

pub fn indexOfDebugSym(vm: *const cy.VM, pc: usize) ?DebugSymIndex {
    return getIndexOfDebugSym(vm, pc) orelse {
        logger.trace("at pc: {}", .{pc});
        return error.NoDebugSym;
    };
}

pub fn getIndexOfDebugSym(vm: *const cy.VM, pc: *anyopaque) ?DebugSymIndex {
    const table_idx = indexOfTableEntry(vm.debug_tables, pc) orelse return null;
    const entry = vm.debug_tables[table_idx];
    const syms = entry.chunk.buf.debugTable.items;
    const idx = indexOfDebugSymFromTable(entry.ptr, syms, pc) orelse return null;
    return DebugSymIndex{ .chunk = entry.chunk, .idx = idx };
}

const DebugSymIndex = struct {
    chunk: *cy.Chunk,
    idx: usize,
};

pub fn indexOfTableEntry(entries: []const cy.DebugTableEntry, pc: *anyopaque) ?usize {
    if (entries.len == 0) {
        return null;
    }
    const S = struct {
        fn compare(context: *anyopaque, item: cy.DebugTableEntry) std.math.Order {
            return std.math.order(@intFromPtr(context), @intFromPtr(item.ptr));
        }
    };
    const upper = std.sort.upperBound(cy.DebugTableEntry, entries, pc, S.compare);
    return upper - 1;
}

/// TODO: Binary search.
pub fn indexOfDebugSymFromTable(buf_ptr: [*]cy.Inst, table: []const cy.DebugSym, pc: *anyopaque) ?usize {
    for (table, 0..) |sym, i| {
        if (@intFromPtr(buf_ptr + sym.pc) == @intFromPtr(pc)) {
            return i;
        }
    }
    return null;
}

pub fn write_object_alloc_trace(t: *cy.Thread, w: *std.Io.Writer, obj: *cy.HeapObject, trace: cy.heap.ObjectTrace) !void {
    var buf: [256]u8 = undefined;
    const typeId = obj.getTypeId();
    var type_name: []const u8 = "undefined";
    if (typeId < t.heap.sema.types.items.len) {
        type_name = t.heap.getType(typeId).name();
    }
    const valStr = try t.heap.bufPrintValueShortStr(&buf, typeId, cy.Value.initPtr(obj), true);
    const msg = try std.fmt.bufPrint(buf[valStr.len..], "{*}, type: {s}, rc: {} at t{}@{x} ev={}\nval={s}", .{
        obj, type_name, obj.rc(), t.c.id, trace.alloc_ctx, trace.alloc_event, valStr,
    });
    const addr: usize = @intCast(trace.alloc_ctx);
    try cy.debug.write_trace_at_pc(t.c.vm, w, @ptrFromInt(addr), "alloced", msg);
}

pub fn write_object_trace(t: *cy.Thread, w: *std.Io.Writer, obj: *cy.HeapObject) !void {
    if (t.heap.objectTraceMap.get(obj)) |trace| {
        if (trace.alloc_ctx != 0) {
            const addr: usize = @intCast(trace.alloc_ctx);
            const alloc_pc: *cy.Inst = @ptrFromInt(addr);
            const msg = try std.fmt.allocPrint(t.alloc, "{*} at pc: {*}({s}), ev={}", .{
                obj, alloc_pc, @tagName(alloc_pc.*.opcode()), trace.alloc_event,
            });
            defer t.alloc.free(msg);
            try write_trace_at_pc(t.c.vm, w, alloc_pc, "alloced", msg);
        } else {
            try write_trace_at_pc(t.c.vm, w, null, "alloced", "");
        }

        if (trace.free_ctx) |free_pc| {
            if (free_pc != 0) {
                const addr: usize = @intCast(free_pc);
                const free_pc_: *cy.Inst = @ptrFromInt(addr);
                const msg = try std.fmt.allocPrint(t.alloc, "{}({s}) at pc: {}({s}), ev={}", .{
                    trace.free_type.?, t.heap.getType(trace.free_type.?).name(),
                    free_pc, @tagName(free_pc_.*.opcode()), trace.free_event,
                });
                defer t.alloc.free(msg);
                try write_trace_at_pc(t.c.vm, w, free_pc_, "freed", msg);
            } else {
                try write_trace_at_pc(t.c.vm, w, null, "freed", "");
            }
        } else {
            _ = try w.writeAll("not freed");
        }
    } else {
        try w.print("No trace for {*}.", .{obj});
    }
}

pub fn printTraceAtNode(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const token = c.nodes[nodeId].start_token;
    const pos = c.tokens[token].pos();
    const w = fmt.lockStderrWriter();
    defer fmt.unlockPrint();
    try write_user_error(c.compiler.vm, w, "Trace", "", c.id, pos);
}

fn getSrcChunk(vm: *cy.VM, chunk: u32, func: u32) u32 {
    return vm.compiler.chunks.items[chunk].funcs_debug.items[func].src_chunk;
}

pub fn thread_signal_host_panic(t: *cy.Thread) !void {
    var lw = std.Io.Writer.Allocating.init(t.alloc);
    defer lw.deinit();
    var w = &lw.writer;

    const cx = t.getTraceFiberContext();
    if (cx.pc) |pc| {
        const sym = cy.debug.getDebugSymByPc(t.c.vm, pc) orelse {
            try w.print("No debug sym at pc: {*}", .{pc});
            try cy.thread.write_thread_inst(t, w, pc);
            return;
        };
        try w.print("panic at t{}@{}:{}\n", .{t.c.id, sym.file, sym.pc});
        try cy.thread.write_thread_inst(t, w, pc);
        try w.writeByte('\n');
    } else {
        try w.print("panic at t{}@host_call:\n", .{t.c.id});
    }

    // NOTE: During TRACE, the vm will save the context before each inst, which allows a full stack trace dump.
    _ = try t.unwindStack(t.c.stack(), cx);
    const frames = try cy.debug.allocStackTrace(t.c.vm, t.c.stack(), t.compact_trace.items);

    try cy.debug.writeStackFrames(t.c.vm, w, frames);

    t.c.vm.log(lw.written());
}

pub fn thread_signal_host_segfault(t: *cy.Thread) !void {
    var lw = std.Io.Writer.Allocating.init(t.alloc);
    defer lw.deinit();
    var w = &lw.writer;

    const cx = t.getTraceFiberContext();
    if (cx.pc) |pc| {
        const sym = cy.debug.getDebugSymByPc(t.c.vm, pc) orelse {
            try w.print("No debug sym at pc: {*}", .{pc});
            try cy.thread.write_thread_inst(t, w, pc);
            return;
        };
        try w.print("segfault at t{}@{}:{}\n", .{t.c.id, sym.file, sym.pc});
        try cy.thread.write_thread_inst(t, w, pc);
        try w.writeByte('\n');
    } else {
        try w.print("segfault at t{}@host_call:\n", .{t.c.id});
    }

    // NOTE: During TRACE, the vm will save the context before each inst, which allows a full stack trace dump.
    _ = try t.unwindStack(t.c.stack(), cx);
    const frames = try cy.debug.allocStackTrace(t.c.vm, t.c.stack(), t.compact_trace.items);

    try cy.debug.writeStackFrames(t.c.vm, w, frames);

    t.c.vm.log(lw.written());
}

pub fn write_trace_at_pc(vm: *cy.VM, w: *std.Io.Writer, pc_opt: ?*anyopaque, title: []const u8, msg: []const u8) !void {
    const pc = pc_opt orelse {
        try w.print("{s}: {s} (external)\n", .{title, msg});
        return;
    };
    if (getIndexOfDebugSym(vm, pc)) |res| {
        const sym = res.chunk.buf.debugTable.items[res.idx];
        const src_chunk = getSrcChunk(vm, sym.file, sym.frameLoc);
        try write_user_error(vm.compiler, w, title, msg, src_chunk, sym.loc);
    } else {
        try w.print("{s}: {s}\nMissing debug sym for {}, pc: {*}.\n", .{
            title, msg, @as([*]cy.Inst, @ptrCast(@alignCast(pc)))[0].opcode(), pc});
    }
}

pub fn allocLastUserPanicError(t: *cy.Thread) ![]const u8 {
    var buf = std.Io.Writer.Allocating.init(t.alloc);
    defer buf.deinit();
    try writeLastUserPanicError(t, &buf.writer);
    return buf.toOwnedSlice();
}

fn writeLastUserPanicError(t: *cy.Thread, w: *std.Io.Writer) !void {
    const msg = try allocPanicMsg(t);
    defer t.alloc.free(msg);

    try w.print("panic: {s}\n\n", .{msg});
    try writeStackFrames(t.c.vm, w, t.stack_trace.frames);
}

pub fn allocReportSummary(c: *const cy.Compiler, report: cy.Report) ![]const u8 {
    var buf = std.Io.Writer.Allocating.init(c.alloc);
    defer buf.deinit();

    switch (report.type) {
        .token_err => {
            try write_user_error(c, &buf.writer, "TokenError", report.msg, report.chunk, @intCast(report.loc));
        },
        .parse_err => {
            try write_user_error(c, &buf.writer, "ParseError", report.msg, report.chunk, @intCast(report.loc));
        },
        .compile_err => {
            try writeCompileErrorSummary(c, &buf.writer, report);
        },
        .context => {
            @panic("Unexpected.");
        },
    }
    return buf.toOwnedSlice();
}

fn writeCompileErrorSummary(c: *const cy.Compiler, w: *std.Io.Writer, report: cy.Report) !void {
    try w.print("CompileError: {s}\n\n", .{report.msg});
    var addr: usize = @intCast(report.loc);
    try writeUserErrorTrace2(c, w, report.chunk, @ptrFromInt(addr));

    // Append back trace.
    var opt_next: ?*cy.Report = report.next;
    while (opt_next) |next| {
        const chunk = c.chunks.items[next.chunk];
        addr = @intCast(next.loc);
        switch (next.type) {
            .context => {
                try w.print("{s}\n", .{next.msg});
                try writeUserErrorTrace2(c, w, chunk.id, @ptrFromInt(addr));
            },
            .compile_err => {
                try writeUserErrorTrace2(c, w, chunk.id, @ptrFromInt(addr));
            },
            else => {},
        }
        opt_next = next.next;
    }
}

/// Reduced to using writer so printed errors can be tested.
pub fn write_user_error(c: *const cy.Compiler, w: *std.Io.Writer, title: []const u8, msg: []const u8, chunk_id: u32, pos: u32) !void {
    try w.print("{s}: {s}\n\n", .{title, msg});
    try writeUserErrorTrace(c, w, chunk_id, pos);
}

pub fn writeUserErrorTrace2(c: *const cy.Compiler, w: *std.Io.Writer, chunk_id: u32, opt_node: ?*ast.Node) !void {
    if (chunk_id != cy.NullId) {
        const chunk = c.chunks.items[chunk_id];
        if (opt_node) |node| {
            var line: u32 = undefined;
            var col: u32 = undefined;
            var line_pos: u32 = undefined;
            const start_pos = node.pos();
            chunk.ast.computeLinePos(start_pos, &line, &col, &line_pos);

            const line_end = std.mem.indexOfScalarPos(u8, chunk.src, line_pos, '\n') orelse chunk.src.len;
            var end_pos = node.end();
            if (end_pos > line_end) {
                end_pos = @intCast(line_end);
            }

            try fmt.format(w,
                \\{}:{}:{}:
                \\{}
                \\
            , &.{
                v(chunk.srcUri),
                v(line+1), v(col+1),
                v(chunk.src[line_pos..line_end]),
            });
            try w.splatByteAll(' ', col);
            // TODO: Should not emit escape codes. Instead, expose stack trace as data for consumer.
            try w.writeAll("\x1b[31m");
            try w.writeAll("^");
            // NOTE: This does not consider grapheme clusters.
            const cp_len = std.unicode.utf8CountCodepoints(chunk.src[start_pos..end_pos]) catch end_pos - start_pos;
            try w.splatByteAll('~', cp_len - 1);
            try w.writeAll("\x1b[0m");
            try w.writeAll("\n");
        } else {
            try fmt.format(w,
                \\in {}
                \\
            , &.{
                v(chunk.srcUri),
            });
        }
    }
}

pub fn write_user_error_trace2(w: *std.Io.Writer, ast_view: ast.AstView, src_uri: []const u8, pos: u32) !void {
    if (pos != cy.NullId) {
        var line: u32 = undefined;
        var col: u32 = undefined;
        var line_pos: u32 = undefined;
        ast_view.computeLinePos(pos, &line, &col, &line_pos);

        const line_end = std.mem.indexOfScalarPos(u8, ast_view.src, line_pos, '\n') orelse ast_view.src.len;

        try fmt.format(w,
            \\{}:{}:{}:
            \\{}
            \\
        , &.{
            v(src_uri),
            v(line+1), v(col+1),
            v(ast_view.src[line_pos..line_end]),
        });
        try w.splatByteAll(' ', col);
        try w.writeAll("^\n");
    } else {
        try fmt.format(w,
            \\in {}
            \\
        , &.{
            v(src_uri),
        });
    }
}

/// For token or parse errors.
pub fn writeUserErrorTrace(c: *const cy.Compiler, w: *std.Io.Writer, chunk_id: u32, pos: u32) !void {
    if (chunk_id != cy.NullId) {
        const chunk = c.chunks.items[chunk_id];
        try write_user_error_trace2(w, chunk.ast, chunk.srcUri, pos);
    }
}

pub fn allocPanicMsg(t: *cy.Thread) ![]const u8 {
    switch (@as(cy.thread.PanicType, @enumFromInt(t.c.panic_type))) {
        .err => {
            var w = std.Io.Writer.fixed(@as([]u8, @ptrCast(&cy.tempBuf)));
            _ = try t.heap.writeValue(&w, bt.Error, @bitCast(t.c.panic_payload), false);
            const str = w.buffered();
            return try fmt.allocFormat(t.alloc, "{}", &.{v(str)});
        },
        .msg => {
            const ptr: usize = @intCast(t.c.panic_payload & ((1 << 48) - 1));
            const len: usize = @intCast(t.c.panic_payload >> 48);
            // Check for zero delimited.
            const str = @as([*]const u8, @ptrFromInt(ptr))[0..len];
            if (len > 0) {
                if (str[len-1] == 0) {
                    return t.alloc.dupe(u8, str[0..len-1]);
                } else {
                    return t.alloc.dupe(u8, str);
                }
            } else {
                return "";
            }
        },
        .staticMsg => {
            const ptr: usize = @intCast(t.c.panic_payload & ((1 << 48) - 1));
            const len: usize = @intCast(t.c.panic_payload >> 48);
            return t.alloc.dupe(u8, @as([*]const u8, @ptrFromInt(ptr))[0..len]);
        },
        .inflightOom,
        .none => {
            cy.panicFmt("Unexpected panic type. {}", .{t.c.panic_type});
        },
    }
}

pub fn deinit_frame(frame: *C.StackFrame, alloc: std.mem.Allocator) void {
    alloc.free(frame.name.ptr[0..frame.name.len]);
    frame.name.len = 0;
}

pub const StackTrace = struct {
    frames: []C.StackFrame = &.{},

    pub fn deinit(self: *StackTrace, alloc: std.mem.Allocator) void {
        for (self.frames) |*frame| {
            deinit_frame(frame, alloc);
        }
        alloc.free(self.frames);
        self.frames = &.{};
    }

    pub fn dump(self: *const StackTrace, vm: *const cy.VM) !void {
        const w = fmt.lockStderrWriter();
        defer fmt.unlockPrint();
        try writeStackFrames(vm, w, self.frames);
    }
};

pub fn writeStackFrames(vm: *const cy.VM, w: *std.Io.Writer, frames: []const C.StackFrame) !void {
    for (frames) |frame| {
        if (frame.chunk != cy.NullId) {
            const chunk = vm.compiler.chunks.items[frame.chunk];
            if (frame.line_pos != cy.NullId) {
                const lineEnd = std.mem.indexOfScalarPos(u8, chunk.src, frame.line_pos, '\n') orelse chunk.src.len;
                try fmt.format(w,
                    \\{}:{}:{} {}:
                    \\{}
                    \\
                , &.{
                    v(chunk.srcUri), v(frame.line+1), v(frame.col+1), v(C.from_bytes(frame.name)),
                    v(chunk.src[frame.line_pos..lineEnd]),
                });
                try w.splatByteAll(' ', frame.col);
                try w.writeAll("^\n");
            } else {
                // No source code attribution.
                try fmt.format(w,
                    \\{}: {}
                    \\
                , &.{
                    v(chunk.srcUri), v(C.from_bytes(frame.name)),
                });
            }
        } else {
            // Host frame.
            try fmt.format(w,
                \\<host>: {}
                \\
            , &.{
                v(C.from_bytes(frame.name)),
            });
        }
    }
}

test "debug internals." {
    if (cy.is32Bit) {
        try zt.eq(8, @sizeOf(vmc.CompactFrame));
    } else {
        try zt.eq(16, @sizeOf(vmc.CompactFrame));
    }
}

/// Can only rely on pc and other non-reference values to build the stack frame since
/// unwinding could have already freed reference values.
pub fn compactToStackFrame(vm: *cy.VM, stack: []const cy.Value, frame: vmc.CompactFrame) !C.StackFrame {
    if (frame.pc) |pc| {
        const sym = getDebugSymByPc(vm, pc) orelse {
            logger.trace("at pc: {*}", .{pc});
            return error.NoDebugSym;
        };
        return getStackFrame(vm, sym);
    } else {
        // Host frame.
        // Assume fp is at frame base.
        // Recover caller inst to lookup name about the host function.
        const call_pc = stack[frame.fpOffset + 1].retPcPtr - stack[frame.fpOffset].call_info.call_inst_off;
        const sym = getDebugSymByPc(vm, call_pc) orelse {
            logger.trace("at pc: {*}", .{frame.pc});
            return error.NoDebugSym;
        };
        const chunk = vm.compiler.chunks.items[sym.file];
        const ops = chunk.buf.ops.items;
        var name: []const u8 = "host function";
        switch (ops[sym.pc].opcode()) {
            .call_host => {
                const addr: usize = @intCast(@as(*const align(1) u48, @ptrCast(ops.ptr + sym.pc + 2)).*);
                const func_ptr: *anyopaque = @ptrFromInt(addr);
                const func_id = vm.host_funcs.get(func_ptr).?;
                name = vm.funcSymDetails.items[func_id].name();
            },
            else => {
            },
        }

        return C.StackFrame{
            .name = C.to_bytes(try vm.alloc.dupe(u8, name)),
            .chunk = cy.NullId,
            .line = 0,
            .col = 0,
            .line_pos = cy.NullId,
        };
    }
}

pub const CoinitFramePos = cy.NullId - 1;
pub const FramePos = cy.NullId - 1;

fn getStackFrame(vm: *cy.VM, sym: cy.DebugSym) !C.StackFrame {
    const chunk = vm.compiler.chunks.items[sym.file];
    if (sym.loc != cy.NullId) {
        const func = chunk.funcs_debug.items[sym.frameLoc];
        var line: u32 = undefined;
        var col: u32 = undefined;
        var lineStart: u32 = undefined;
        const src_chunk = vm.compiler.chunks.items[func.src_chunk];
        src_chunk.ast.computeLinePos(sym.loc, &line, &col, &lineStart);
        var name: []const u8 = undefined;
        if (func.func) |sema_func| {
            if (sema_func.type != .userLambda and sema_func.parent.parent.?.type != .chunk) {
                const parent = sema_func.parent.parent.?;
                name = try std.fmt.allocPrint(vm.alloc, "{s}.{s}", .{parent.name(), sema_func.name()});
            } else {
                var buf: [256]u8 = undefined;
                var w = std.Io.Writer.fixed(&buf);
                cy.sym.write_func_base_name(vm.sema, &w, sema_func, .{}) catch @panic("error");
                name = try vm.alloc.dupe(u8, w.buffered());
            }
        } else {
            name = try vm.alloc.dupe(u8, func.name);
        }
        return C.StackFrame{
            .name = C.to_bytes(name),
            .chunk = func.src_chunk,
            .line = line,
            .col = col,
            .line_pos = lineStart,
        };
    } else {
        // Hidden function.
        const func = chunk.funcs_debug.items[sym.frameLoc];
        return C.StackFrame{
            .name = C.to_bytes(try vm.alloc.dupe(u8, func.name)),
            .chunk = sym.file,
            .line = 0,
            .col = 0,
            .line_pos = cy.NullId,
        };
    }
}

pub fn allocStackTrace(vm: *cy.VM, stack: []const cy.Value, cframes: []const vmc.CompactFrame) ![]C.StackFrame {
    @branchHint(.cold);
    logger.tracev("build stacktrace {}", .{cframes.len});
    var frames = try vm.alloc.alloc(C.StackFrame, cframes.len);
    for (cframes, 0..) |cframe, i| {
        logger.tracev("build stackframe pc={*}, fp={}", .{cframe.pc, cframe.fpOffset});
        frames[i] = try cy.debug.compactToStackFrame(vm, stack, cframe);
    }
    return frames;
}

pub fn nextEventId() u64 {
    defer cy.event_id += 1;
    return cy.event_id;
}

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

fn checkBytecode(c: *cy.Chunk) void {
    var pc = c.buf.ops.items.ptr;
    var pc_off: u32 = 0;
    var inst: u32 = 0;
    while (pc_off < c.buf.ops.items.len) {
        if (pc[0].opcode() >= vmc.NumCodes) {
            std.debug.panic("Bad instruction at {}.", .{pc_off});
        }
        const code = pc[0].opcode();
        if (c.buf.opcodes.items[inst] != code) {
            std.debug.panic("Expected instruction {}, found {}.", .{c.buf.opcodes.items[inst], code});
        }
        // std.debug.print("DEBUG: {}\n", .{pc[0].opcode()});
        const len = bytecode.getInstLenAt(pc);
        pc_off += len;
        pc += len;
        inst += 1;
    }
}

const DumpBytecodeOptions = struct {
};

/// When `optPcContext` is null, all the bytecode is dumped along with constants.
/// When `optPcContext` is non null, it will dump a trace at `optPcContext` and the surrounding bytecode with extra details.
pub fn dumpBytecode(vm: *cy.VM, opts: DumpBytecodeOptions) !void {
    _ = opts;

    var lw = std.Io.Writer.Allocating.init(vm.alloc);
    defer lw.deinit();
    var w = &lw.writer;

    const start_pc = vm.compiler.main_chunk.buf.ops.items.ptr + vm.compiler.main_chunk.buf.main_pc;

    try w.writeAll("Bytecode:\n");
    for (vm.compiler.chunks.items, 0..) |c, chunk_id| {
        const opsLen = c.buf.ops.items.len;
        var pc = c.buf.ops.items.ptr;
        var pc_off: usize = 0;
        var instIdx: u32 = 0;

        // Build func pc map.
        var funcs: std.AutoHashMapUnmanaged(*anyopaque, *cy.Func) = .{};
        defer funcs.deinit(vm.alloc);
        var iter = c.buf.markers.iterator();
        while (iter.next()) |e| {
            try funcs.put(vm.alloc, pc + e.key_ptr.*, e.value_ptr.*);
        }

        try w.print("--chunk{} {s}:\n", .{c.id, c.srcUri});

        // checkBytecode(c);

        while (@intFromPtr(pc) < @intFromPtr(c.buf.ops.items.ptr + opsLen)) {
            if (funcs.get(pc)) |func| {
                var id: usize = undefined;
                if (func.type == .extern_) {
                    id = vm.compiler.genSymMap.get(func).?.extern_func.id;
                } else {
                    id = vm.compiler.genSymMap.get(func).?.func.id;
                }
                const name = try c.sema.newFuncName(c.alloc, func, .{});
                defer c.alloc.free(name);
                try w.print("--{} `fn {s}`\n", .{id, name});
            } else if (pc == start_pc) {
                try w.print("--start\n", .{});
            }
            const code = pc[0].opcode();
            const len = bytecode.getInstLenAt(pc);
            // std.debug.print("{}\n", .{code});
            // try dumpInst(vm, code, chunk_id, pc_off, pc, c, 0, null);
            try bytecode.write_inst(vm, w, code, chunk_id, pc_off, pc, .{});
            try w.writeByte('\n');
            pc += len;
            pc_off += len;
            instIdx += 1;
        }

        // TODO: Record unboxed constant types.
        // rt.printFmt(vm, "\nConstants ({}):\n", &.{v(vm.compiler.buf.mconsts.len)});
        // for (vm.compiler.buf.mconsts) |extra| {
        //     const val = cy.Value{ .val = extra.val };
        //     const str = try vm.bufPrintValueShortStr(&vm.tempBuf, val);
        //     rt.printFmt(vm, "{}\n", &.{v(str)});
        // }
    }

    // Dump aggregate info per chunk.
    for (vm.compiler.chunks.items) |c| {
        try w.print("chunk{} {s}:\n", .{c.id, c.srcUri});
        try w.print("  syms: {}\n", .{c.syms.items.len});
        try w.print("  funcs: {}\n", .{c.funcs.items.len});
        try w.print("  debug_table: {}\n", .{c.buf.debugTable.items.len});
        try w.print("  ptr_layouts: {}\n", .{c.buf.ptr_layouts.items.len});
        try w.print("  ptr_table: {}\n", .{c.buf.ptr_table.items.len});
    }

    vm.log(lw.written());
}

pub fn dumpInst(vm: *cy.VM, code: cy.OpCode, chunk_id: ?usize, pc_off: ?usize, pc: [*]const cy.Inst, c: *cy.Chunk, instIdx: u32, prefix: ?[]const u8) !void {
    var buf: [1024]u8 = undefined;
    if (cy.Trace) {
        const desc = c.buf.instDescs.items[instIdx];
        if (desc != cy.NullId) {
            var fbuf = std.io.fixedBufferStream(&buf);
            var w = fbuf.writer();

            const descExtra = c.buf.instDescExtras.items[desc];
            try w.writeByte('"');
            try w.writeAll(descExtra.text);
            try w.writeAll("\" ");
        }

        // TODO: Dump src code from optional debug entry
    }

    var w = std.Io.Writer.Allocating.init(vm.alloc);
    defer w.deinit();
    try bytecode.write_inst(vm, &w.writer, code, chunk_id, pc_off, pc, .{ .extra = "", .prefix = prefix });
    vm.log(w.written());
}

pub fn alloc_value_desc(vm: *cy.VM, val_t: cy.TypeId, val: *cy.Value) ![]const u8 {
    var buf = std.Io.Writer.Allocating.init(vm.alloc);
    defer buf.deinit();
    try cy.debug.write_value_desc(vm, &buf.writer, val_t, val);
    return buf.toOwnedSlice();
}

pub fn write_value_desc(vm: *cy.VM, w: *std.Io.Writer, val_t: cy.TypeId, val: *cy.Value) !void {
    switch (val_t) {
        bt.Bool => try w.print("{}", .{val.asBool()}),
        bt.I64 => try w.print("{}", .{val.asInt()}),
        bt.F64 => {
            const f = val.asF64();
            if (cy.Value.floatCanBeInteger(f)) {
                try w.print("{d:.1}", .{f});
            } else {
                try w.print("{d}", .{f});
            }
        },
        bt.Void => {},
        bt.Error => {
            try w.print("error.{}", .{val.asError()});
        },
        bt.Symbol => {
            try w.print("@{}", .{val.asSymbol()});
        },
        bt.Type => {
            try w.print("type=", .{});
            const type_ptr = val.asPtr(*cy.Type);
            try vm.sema.writeTypeName(w, type_ptr, null);
        },
        bt.Str => {
            const MaxStrLen = 30;
            const str: *cy.heap.Str = @ptrCast(val);
            const slice = str.slice();
            if (slice.len > MaxStrLen) {
                try w.print("'{s}'...", .{slice[0..MaxStrLen]});
            } else {
                try w.print("'{s}'", .{slice});
            }
        },
        else => {
            try w.print("(type={}){}", .{val_t, val.val});
        },
    }
}

const EnableTimerTrace = builtin.os.tag != .freestanding and builtin.mode == .Debug;

const TimerTrace = struct {
    timer: if (EnableTimerTrace) stdx.time.Timer else void,

    pub fn end(self: *TimerTrace) void {
        if (EnableTimerTrace and cy.verbose) {
            const now = self.timer.read();
            log("time: {d:.3}ms", .{ @as(f32, @floatFromInt(now)) / 1e6 });
        }
    }
    pub fn endPrintVerbose(self: *TimerTrace, msg: []const u8) void {
        if (EnableTimerTrace and C.verbose()) {
            const now = self.timer.read();
            log("{s}: {d:.3}ms", .{ msg, @as(f32, @floatFromInt(now)) / 1e6 });
        }
    }
    pub fn endPrint(self: *TimerTrace, msg: []const u8) void {
        if (EnableTimerTrace) {
            const now = self.timer.read();
            log("{s}: {d:.3}ms", .{ msg, @as(f32, @floatFromInt(now)) / 1e6 });
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

// pub fn print(comptime format: []const u8, args: anytype) void {
//     const w = std.debug.lockStderrWriter(&print_buf);
//     defer std.debug.unlockStderrWriter();
//     w.print(format, args) catch @panic("error");
// }

// pub fn print2(format: []const u8, args: []const cy.fmt.FmtValue) void {
//     const w = std.debug.lockStderrWriter(&print_buf);
//     defer std.debug.unlockStderrWriter();
//     cy.fmt.print(w, format, args);
// }

// pub fn prints(str: []const u8) void {
//     const w = std.debug.lockStderrWriter(&.{});
//     defer std.debug.unlockStderrWriter();
//     w.writeAll(str) catch @panic("error");
// }

pub fn log(comptime format: []const u8, args: anytype) void {
    var buf: [1024]u8 = undefined;
    var w = std.Io.Writer.fixed(&buf);
    w.print(format, args) catch @panic("error");
    C.c.cl_logger.?(C.to_bytes(w.buffered()));
}

// pub fn log2(format: []const u8, args: []const cy.fmt.FmtValue) void {
//     const w = std.debug.lockStderrWriter(&print_buf);
//     defer std.debug.unlockStderrWriter();
//     cy.fmt.print(w, format, args);
//     w.writeByte('\n') catch @panic("error");
// }

// pub fn logs(str: []const u8) void {
//     const w = std.debug.lockStderrWriter(&print_buf);
//     defer std.debug.unlockStderrWriter();
//     w.writeAll(str) catch @panic("error");
//     w.writeByte('\n') catch @panic("error");
// }
