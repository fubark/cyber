const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const zt = stdx.testing;
const cy = @import("cyber.zig");
const cc = @import("capi.zig");
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
    try cy.debug.write_trace_at_pc(t.c.vm, w, @ptrFromInt(trace.alloc_ctx), "alloced", msg);
}

pub fn write_object_trace(t: *cy.Thread, w: *std.Io.Writer, obj: *cy.HeapObject) !void {
    if (t.heap.objectTraceMap.get(obj)) |trace| {
        if (trace.alloc_ctx != 0) {
            const alloc_pc: *cy.Inst = @ptrFromInt(trace.alloc_ctx);
            const msg = try std.fmt.allocPrint(t.alloc, "{*} at pc: {*}({s}), ev={}", .{
                obj, @as(*cy.Inst, @ptrFromInt(trace.alloc_ctx)), @tagName(alloc_pc.*.opcode()), trace.alloc_event,
            });
            defer t.alloc.free(msg);
            try write_trace_at_pc(t.c.vm, w, @ptrFromInt(trace.alloc_ctx), "alloced", msg);
        } else {
            try write_trace_at_pc(t.c.vm, w, @ptrFromInt(trace.alloc_ctx), "alloced", "");
        }

        if (trace.free_ctx) |free_pc| {
            if (free_pc != 0) {
                const free_pc_: *cy.Inst = @ptrFromInt(free_pc);
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
            cy.debug.prints("not freed\n");
        }
    } else {
        logger.tracev("No trace for {*}.", .{obj});
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
            title, msg, @as([*]cy.Inst, @ptrCast(pc))[0].opcode(), pc});
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
    try writeUserErrorTrace2(c, w, report.chunk, @ptrFromInt(report.loc));

    // Append back trace.
    var opt_next: ?*cy.Report = report.next;
    while (opt_next) |next| {
        const chunk = c.chunks.items[next.chunk];
        switch (next.type) {
            .context => {
                try w.print("{s}\n", .{next.msg});
                try writeUserErrorTrace2(c, w, chunk.id, @ptrFromInt(next.loc));
            },
            .compile_err => {
                try writeUserErrorTrace2(c, w, chunk.id, @ptrFromInt(next.loc));
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
            if (!builtin.is_test) {
                try w.writeAll("\x1b[31m");
            }
            try w.writeAll("^");
            // NOTE: This does not consider grapheme clusters.
            const cp_len = std.unicode.utf8CountCodepoints(chunk.src[start_pos..end_pos]) catch end_pos - start_pos;
            try w.splatByteAll('~', cp_len - 1);
            if (!builtin.is_test) {
                try w.writeAll("\x1b[0m");
            }
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

/// For token or parse errors.
pub fn writeUserErrorTrace(c: *const cy.Compiler, w: *std.Io.Writer, chunk_id: u32, pos: u32) !void {
    if (chunk_id != cy.NullId) {
        const chunk = c.chunks.items[chunk_id];
        if (pos != cy.NullId) {
            var line: u32 = undefined;
            var col: u32 = undefined;
            var line_pos: u32 = undefined;
            chunk.ast.computeLinePos(pos, &line, &col, &line_pos);

            const line_end = std.mem.indexOfScalarPos(u8, chunk.src, line_pos, '\n') orelse chunk.src.len;

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
            try w.writeAll("^\n");
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

pub const StackTrace = struct {
    frames: []StackFrame = &.{},

    pub fn deinit(self: *StackTrace, alloc: std.mem.Allocator) void {
        for (self.frames) |*frame| {
            frame.deinit(alloc);
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

pub fn writeStackFrames(vm: *const cy.VM, w: *std.Io.Writer, frames: []const StackFrame) !void {
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
                try w.splatByteAll(' ', frame.col);
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
    /// Name identifier (e.g. function name, or "main" for the main block)
    name: []const u8,
    /// Starts at 0.
    line: u32,
    /// Starts at 0.
    col: u32,
    /// Where the line starts in the source file.
    lineStartPos: u32,
    chunkId: u32,

    pub fn deinit(self: *StackFrame, alloc: std.mem.Allocator) void {
        alloc.free(self.name);
        self.name = "";
    }
};

test "debug internals." {
    try zt.eq(16, @sizeOf(vmc.CompactFrame));
}

/// Can only rely on pc and other non-reference values to build the stack frame since
/// unwinding could have already freed reference values.
pub fn compactToStackFrame(vm: *cy.VM, stack: []const cy.Value, frame: vmc.CompactFrame) !StackFrame {
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
                const func_ptr: *anyopaque = @ptrFromInt(@as(*const align(1) u48, @ptrCast(ops.ptr + sym.pc + 3)).*);
                const func_id = vm.host_funcs.get(func_ptr).?;
                name = vm.funcSymDetails.items[func_id].name();
            },
            else => {
            },
        }
        return StackFrame{
            .name = try vm.alloc.dupe(u8, name),
            .chunkId = cy.NullId,
            .line = 0,
            .col = 0,
            .lineStartPos = cy.NullId,
        };
    }
}

pub const CoinitFramePos = cy.NullId - 1;
pub const FramePos = cy.NullId - 1;

fn getStackFrame(vm: *cy.VM, sym: cy.DebugSym) !StackFrame {
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
        return StackFrame{
            .name = name,
            .chunkId = func.src_chunk,
            .line = line,
            .col = col,
            .lineStartPos = lineStart,
        };
    } else {
        // Invoking $init.
        return StackFrame{
            .name = try vm.alloc.dupe(u8, "$init"),
            .chunkId = sym.file,
            .line = 0,
            .col = 0,
            .lineStartPos = cy.NullId,
        };
    }
}

pub fn allocStackTrace(vm: *cy.VM, stack: []const cy.Value, cframes: []const vmc.CompactFrame) ![]cy.StackFrame {
    @branchHint(.cold);
    logger.tracev("build stacktrace {}", .{cframes.len});
    var frames = try vm.alloc.alloc(cy.StackFrame, cframes.len);
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
        if (pc[0].val >= vmc.NumCodes) {
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

    const w = std.debug.lockStderrWriter(&print_buf);
    defer std.debug.unlockStderrWriter();

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
                try w.print("--fn {s} {}\n", .{func.name(), id});
            }
            const code = pc[0].opcode();
            const len = bytecode.getInstLenAt(pc);
            try dumpInst(vm, code, chunk_id, pc_off, pc, c, instIdx, null);
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

    const w = std.debug.lockStderrWriter(&print_buf);
    defer std.debug.unlockStderrWriter();

    try bytecode.write_inst(vm, w, code, chunk_id, pc_off, pc, .{ .extra = "", .prefix = prefix });
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
        if (EnableTimerTrace and cc.verbose()) {
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

/// NOTE: Dupe Zig segfault handler to wrap a custom user handler.

var windows_segfault_handle: ?std.windows.HANDLE = null;

/// Attaches a global SIGSEGV handler which calls `@panic("segmentation fault");`
const SigactionFn = std.posix.Sigaction.sigaction_fn;
pub fn attachSegfaultHandler(handler: SigactionFn) void {
    if (!std.debug.have_segfault_handling_support) {
        @compileError("segfault handler not supported for this target");
    }
    if (builtin.os.tag == .windows) {
        windows_segfault_handle = std.windows.kernel32.AddVectoredExceptionHandler(0, handleSegfaultWindows);
        return;
    }
    const act = std.posix.Sigaction{
        .handler = .{ .sigaction = handler },
        .mask = std.posix.sigemptyset(),
        .flags = (std.posix.SA.SIGINFO | std.posix.SA.RESTART | std.posix.SA.RESETHAND),
    };
    std.debug.updateSegfaultHandler(&act);
}

fn resetSegfaultHandler() void {
    if (builtin.os.tag == .windows) {
        if (windows_segfault_handle) |handle| {
            std.debug.assert(std.windows.kernel32.RemoveVectoredExceptionHandler(handle) != 0);
            windows_segfault_handle = null;
        }
        return;
    }
    const act = std.posix.Sigaction{
        .handler = .{ .handler = std.posix.SIG.DFL },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.debug.updateSegfaultHandler(&act);
}

/// Non-zero whenever the program triggered a panic.
/// The counter is incremented/decremented atomically.
var panicking = std.atomic.Value(u8).init(0);

/// Counts how many times the panic handler is invoked by this thread.
/// This is used to catch and handle panics triggered by the panic handler.
pub threadlocal var panic_stage: usize = 0;

/// Modified from `std.debug.defaultPanic` to wrap a custom `handler`.
/// Dumps a stack trace to standard error, then aborts.
pub fn defaultPanic(
    msg: []const u8,
    first_trace_addr: ?usize,
    handler: *const fn() anyerror!void,
) noreturn {
    @branchHint(.cold);

    // For backends that cannot handle the language features depended on by the
    // default panic handler, we have a simpler panic handler:
    switch (builtin.zig_backend) {
        .stage2_aarch64,
        .stage2_arm,
        .stage2_powerpc,
        .stage2_riscv64,
        .stage2_spirv,
        .stage2_wasm,
        .stage2_x86,
        => @trap(),
        .stage2_x86_64 => switch (builtin.target.ofmt) {
            .elf, .macho => {},
            else => @trap(),
        },
        else => {},
    }

    switch (builtin.os.tag) {
        .freestanding, .other => {
            @trap();
        },
        .uefi => {
            const uefi = std.os.uefi;

            var utf16_buffer: [1000]u16 = undefined;
            const len_minus_3 = std.unicode.utf8ToUtf16Le(&utf16_buffer, msg) catch 0;
            utf16_buffer[len_minus_3..][0..3].* = .{ '\r', '\n', 0 };
            const len = len_minus_3 + 3;
            const exit_msg = utf16_buffer[0 .. len - 1 :0];

            // Output to both std_err and con_out, as std_err is easier
            // to read in stuff like QEMU at times, but, unlike con_out,
            // isn't visible on actual hardware if directly booted into
            inline for ([_]?*uefi.protocol.SimpleTextOutput{ uefi.system_table.std_err, uefi.system_table.con_out }) |o| {
                if (o) |out| {
                    out.setAttribute(.{ .foreground = .red }) catch {};
                    _ = out.outputString(exit_msg) catch {};
                    out.setAttribute(.{ .foreground = .white }) catch {};
                }
            }

            if (uefi.system_table.boot_services) |bs| {
                // ExitData buffer must be allocated using boot_services.allocatePool (spec: page 220)
                const exit_data = uefi.raw_pool_allocator.dupeZ(u16, exit_msg) catch @trap();
                bs.exit(uefi.handle, .aborted, exit_data) catch {};
            }
            @trap();
        },
        .cuda, .amdhsa => std.posix.abort(),
        .plan9 => {
            var status: [std.os.plan9.ERRMAX]u8 = undefined;
            const len = @min(msg.len, status.len - 1);
            @memcpy(status[0..len], msg[0..len]);
            status[len] = 0;
            std.os.plan9.exits(status[0..len :0]);
        },
        else => {},
    }

    if (std.options.enable_segfault_handler) {
        // If a segfault happens while panicking, we want it to actually segfault, not trigger
        // the handler.
        resetSegfaultHandler();
    }

    // Note there is similar logic in handleSegfaultPosix and handleSegfaultWindowsExtra.
    nosuspend switch (panic_stage) {
        0 => {
            panic_stage = 1;

            _ = panicking.fetchAdd(1, .seq_cst);

            {
                const stderr = std.debug.lockStderrWriter(&.{});
                defer std.debug.unlockStderrWriter();

                if (builtin.single_threaded) {
                    stderr.print("panic: ", .{}) catch std.posix.abort();
                } else {
                    const current_thread_id = std.Thread.getCurrentId();
                    stderr.print("thread {} panic: ", .{current_thread_id}) catch std.posix.abort();
                }
                stderr.print("{s}\n", .{msg}) catch std.posix.abort();

                if (@errorReturnTrace()) |t| std.debug.dumpStackTrace(t.*);
                std.debug.dumpCurrentStackTraceToWriter(first_trace_addr orelse @returnAddress(), stderr) catch {};
            }

            handler() catch |err| {
                std.debug.print("error during panic: {}", .{err});
                std.posix.abort();
            };

            waitForOtherThreadToFinishPanicking();
        },
        1 => {
            panic_stage = 2;

            // A panic happened while trying to print a previous panic message.
            // We're still holding the mutex but that's fine as we're going to
            // call abort().
            std.fs.File.stderr().writeAll("aborting due to recursive panic\n") catch {};
        },
        else => {}, // Panicked while printing the recursive panic message.
    };

    std.posix.abort();
}

pub fn vm_segv_handler(vm: *cy.VM) !void {
    const w = std.debug.lockStderrWriter(&print_buf);
    defer std.debug.unlockStderrWriter();

    const t = cy.vm.cur_thread orelse {
        try w.print("segfault without thread context\n", .{});
        return;
    };

    const cx = t.getTraceFiberContext();
    if (cx.pc) |pc| {
        const sym = cy.debug.getDebugSymByPc(vm, pc) orelse {
            try w.print("No debug sym at pc: {*}", .{pc});
            try cy.thread.write_thread_inst(t, w, pc);
            return;
        };
        try w.print("segfault at t{}@{}:{}\n", .{t.c.id, sym.file, sym.pc});
        try cy.thread.write_thread_inst(t, w, pc);
    } else {
        try w.print("segfault at t{}@host_call:\n", .{t.c.id});
    }

    // NOTE: During TRACE, the vm will save the context before each inst, which allows a full stack trace dump.
    _ = try t.unwindStack(t.c.stack(), cx);
    const frames = try cy.debug.allocStackTrace(vm, t.c.stack(), t.compact_trace.items);

    try cy.debug.writeStackFrames(vm, w, frames);
}

pub fn vm_panic_handler(vm: *cy.VM) !void {
    const w = std.debug.lockStderrWriter(&print_buf);
    defer std.debug.unlockStderrWriter();

    const t = cy.vm.cur_thread orelse {
        try w.print("panic without thread context\n", .{});
        return;
    };

    const cx = t.getTraceFiberContext();
    if (cx.pc) |pc| {
        const sym = getDebugSymByPc(vm, pc) orelse {
            try w.print("No debug sym at pc: {*}", .{pc});
            try cy.thread.write_thread_inst(t, w, pc);
            return;
        };
        try w.print("panic at t{}@{}:{}\n", .{t.c.id, sym.file, sym.pc});
        try cy.thread.write_thread_inst(t, w, pc);
    } else {
        try w.print("panic at t{}@host_call:\n", .{t.c.id});
    }

    // NOTE: During TRACE, the vm will save the context before each inst, which allows a full stack trace dump.
    _ = try t.unwindStack(t.c.stack(), cx);
    const frames = try cy.debug.allocStackTrace(vm, t.c.stack(), t.compact_trace.items);

    try writeStackFrames(vm, w, frames);
}

pub fn handleSegfaultPosix(sig: i32, info: *const std.posix.siginfo_t, ctx_ptr: ?*anyopaque) callconv(.c) noreturn {
    // Reset to the default handler so that if a segfault happens in this handler it will crash
    // the process. Also when this handler returns, the original instruction will be repeated
    // and the resulting segfault will crash the process rather than continually dump stack traces.
    resetSegfaultHandler();

    const addr = switch (builtin.os.tag) {
        .linux => @intFromPtr(info.fields.sigfault.addr),
        .freebsd, .macos => @intFromPtr(info.addr),
        .netbsd => @intFromPtr(info.info.reason.fault.addr),
        .openbsd => @intFromPtr(info.data.fault.addr),
        .solaris, .illumos => @intFromPtr(info.reason.fault.addr),
        else => unreachable,
    };

    const code = if (builtin.os.tag == .netbsd) info.info.code else info.code;
    nosuspend switch (panic_stage) {
        0 => {
            panic_stage = 1;
            _ = panicking.fetchAdd(1, .seq_cst);

            {
                std.debug.lockStdErr();
                defer std.debug.unlockStdErr();

                dumpSegfaultInfoPosix(sig, code, addr, ctx_ptr);
            }

            waitForOtherThreadToFinishPanicking();
        },
        else => {
            // panic mutex already locked
            dumpSegfaultInfoPosix(sig, code, addr, ctx_ptr);
        },
    };

    // We cannot allow the signal handler to return because when it runs the original instruction
    // again, the memory may be mapped and undefined behavior would occur rather than repeating
    // the segfault. So we simply abort here.
    std.posix.abort();
}

fn handleSegfaultWindows(info: *std.windows.EXCEPTION_POINTERS) callconv(.winapi) c_long {
    switch (info.ExceptionRecord.ExceptionCode) {
        std.windows.EXCEPTION_DATATYPE_MISALIGNMENT => handleSegfaultWindowsExtra(info, 0, "Unaligned Memory Access"),
        std.windows.EXCEPTION_ACCESS_VIOLATION => handleSegfaultWindowsExtra(info, 1, null),
        std.windows.EXCEPTION_ILLEGAL_INSTRUCTION => handleSegfaultWindowsExtra(info, 2, null),
        std.windows.EXCEPTION_STACK_OVERFLOW => handleSegfaultWindowsExtra(info, 0, "Stack Overflow"),
        else => return std.windows.EXCEPTION_CONTINUE_SEARCH,
    }
}

fn handleSegfaultWindowsExtra(info: *std.windows.EXCEPTION_POINTERS, msg: u8, label: ?[]const u8) noreturn {
    // For backends that cannot handle the language features used by this segfault handler, we have a simpler one,
    switch (builtin.zig_backend) {
        .stage2_x86_64 => if (builtin.target.ofmt == .coff) @trap(),
        else => {},
    }

    comptime std.debug.assert(std.windows.CONTEXT != void);
    nosuspend switch (panic_stage) {
        0 => {
            panic_stage = 1;
            _ = panicking.fetchAdd(1, .seq_cst);

            {
                const stderr = std.debug.lockStderrWriter(&.{});
                defer std.debug.unlockStderrWriter();

                dumpSegfaultInfoWindows(info, msg, label, stderr);
            }

            waitForOtherThreadToFinishPanicking();
        },
        1 => {
            panic_stage = 2;
            std.fs.File.stderr().writeAll("aborting due to recursive panic\n") catch {};
        },
        else => {},
    };
    std.posix.abort();
}

const native_arch = builtin.cpu.arch;
const native_os = builtin.os.tag;
fn dumpSegfaultInfoPosix(sig: i32, code: i32, addr: usize, ctx_ptr: ?*anyopaque) void {
    const stderr = std.debug.lockStderrWriter(&.{});
    defer std.debug.unlockStderrWriter();
    _ = switch (sig) {
        std.posix.SIG.SEGV => if (native_arch == .x86_64 and native_os == .linux and code == 128) // SI_KERNEL
            // x86_64 doesn't have a full 64-bit virtual address space.
            // Addresses outside of that address space are non-canonical
            // and the CPU won't provide the faulting address to us.
            // This happens when accessing memory addresses such as 0xaaaaaaaaaaaaaaaa
            // but can also happen when no addressable memory is involved;
            // for example when reading/writing model-specific registers
            // by executing `rdmsr` or `wrmsr` in user-space (unprivileged mode).
            stderr.writeAll("General protection exception (no address available)\n")
        else
            stderr.print("Segmentation fault at address 0x{x}\n", .{addr}),
        std.posix.SIG.ILL => stderr.print("Illegal instruction at address 0x{x}\n", .{addr}),
        std.posix.SIG.BUS => stderr.print("Bus error at address 0x{x}\n", .{addr}),
        std.posix.SIG.FPE => stderr.print("Arithmetic exception at address 0x{x}\n", .{addr}),
        else => unreachable,
    } catch std.posix.abort();

    switch (native_arch) {
        .x86,
        .x86_64,
        .arm,
        .armeb,
        .thumb,
        .thumbeb,
        .aarch64,
        .aarch64_be,
        => {
            // Some kernels don't align `ctx_ptr` properly. Handle this defensively.
            const ctx: *align(1) std.posix.ucontext_t = @ptrCast(ctx_ptr);
            var new_ctx: std.posix.ucontext_t = ctx.*;
            if (builtin.os.tag.isDarwin() and builtin.cpu.arch == .aarch64) {
                // The kernel incorrectly writes the contents of `__mcontext_data` right after `mcontext`,
                // rather than after the 8 bytes of padding that are supposed to sit between the two. Copy the
                // contents to the right place so that the `mcontext` pointer will be correct after the
                // `relocateContext` call below.
                new_ctx.__mcontext_data = @as(*align(1) extern struct {
                    onstack: c_int,
                    sigmask: std.c.sigset_t,
                    stack: std.c.stack_t,
                    link: ?*std.c.ucontext_t,
                    mcsize: u64,
                    mcontext: *std.c.mcontext_t,
                    __mcontext_data: std.c.mcontext_t align(@sizeOf(usize)), // Disable padding after `mcontext`.
                }, @ptrCast(ctx)).__mcontext_data;
            }
            std.debug.relocateContext(&new_ctx);
            std.debug.dumpStackTraceFromBase(&new_ctx, stderr);
        },
        else => {},
    }
}

fn dumpSegfaultInfoWindows(info: *std.windows.EXCEPTION_POINTERS, msg: u8, label: ?[]const u8, stderr: *std.io.Writer) void {
    _ = switch (msg) {
        0 => stderr.print("{s}\n", .{label.?}),
        1 => stderr.print("Segmentation fault at address 0x{x}\n", .{info.ExceptionRecord.ExceptionInformation[1]}),
        2 => stderr.print("Illegal instruction at address 0x{x}\n", .{info.ContextRecord.getRegs().ip}),
        else => unreachable,
    } catch std.posix.abort();

    std.debug.dumpStackTraceFromBase(info.ContextRecord, stderr);
}

/// Must be called only after adding 1 to `panicking`. There are three callsites.
fn waitForOtherThreadToFinishPanicking() void {
    if (panicking.fetchSub(1, .seq_cst) != 1) {
        // Another thread is panicking, wait for the last one to finish
        // and call abort()
        if (builtin.single_threaded) unreachable;

        // Sleep forever without hammering the CPU
        var futex = std.atomic.Value(u32).init(0);
        while (true) std.Thread.Futex.wait(&futex, 0);
        unreachable;
    }
}

// Protected by `std.debug.lockStderrWriter`.
pub var print_buf: [1024]u8 = undefined;

pub fn print(comptime format: []const u8, args: anytype) void {
    const w = std.debug.lockStderrWriter(&print_buf);
    defer std.debug.unlockStderrWriter();
    w.print(format, args) catch @panic("error");
}

pub fn print2(format: []const u8, args: []const cy.fmt.FmtValue) void {
    const w = std.debug.lockStderrWriter(&print_buf);
    defer std.debug.unlockStderrWriter();
    cy.fmt.print(w, format, args);
}

pub fn prints(str: []const u8) void {
    const w = std.debug.lockStderrWriter(&.{});
    defer std.debug.unlockStderrWriter();
    w.writeAll(str) catch @panic("error");
}

pub fn log(comptime format: []const u8, args: anytype) void {
    const w = std.debug.lockStderrWriter(&print_buf);
    defer std.debug.unlockStderrWriter();
    w.print(format, args) catch @panic("error");
    w.writeByte('\n') catch @panic("error");
}

pub fn log2(format: []const u8, args: []const cy.fmt.FmtValue) void {
    const w = std.debug.lockStderrWriter(&print_buf);
    defer std.debug.unlockStderrWriter();
    cy.fmt.print(w, format, args);
    w.writeByte('\n') catch @panic("error");
}

pub fn logs(str: []const u8) void {
    const w = std.debug.lockStderrWriter(&print_buf);
    defer std.debug.unlockStderrWriter();
    w.writeAll(str) catch @panic("error");
    w.writeByte('\n') catch @panic("error");
}
