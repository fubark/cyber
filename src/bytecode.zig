const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const log = cy.log.scoped(.bytecode);
const fmt = @import("fmt.zig");
const debug = @import("debug.zig");
const v = fmt.v;
const vmc = @import("vm_c.zig");

/// Holds vm instructions.
pub const ByteCodeBuffer = struct {
    alloc: std.mem.Allocator,
    ops: std.ArrayListUnmanaged(Inst),
    consts: std.ArrayListUnmanaged(Const),

    /// After compilation, consts is merged into the ops buffer.
    /// This should be used by the interpreter to read const values.
    mconsts: []const Const,

    /// Contiguous constant strings in a buffer.
    strBuf: std.ArrayListUnmanaged(u8),
    /// Tracks the start index of strings that are already in strBuf.
    strMap: std.HashMapUnmanaged(cy.IndexSlice(u32), u32, StringIndexContext, std.hash_map.default_max_load_percentage),

    /// Maps bytecode insts back to source code.
    /// Contains entries ordered by `pc`. 
    debugTable: std.ArrayListUnmanaged(DebugSym),
    debugTempIndexTable: std.ArrayListUnmanaged(u32),

    /// Ordered labels by `pc` for debugging only.
    debugMarkers: std.ArrayListUnmanaged(DebugMarker),

    /// Unwinding temp graph.
    unwindTempRegs: std.ArrayListUnmanaged(u8),
    unwindTempPrevIndexes: std.ArrayListUnmanaged(u32),

    /// The required stack size for the main frame.
    mainStackSize: u32,

    pub fn init(alloc: std.mem.Allocator) !ByteCodeBuffer {
        var new = ByteCodeBuffer{
            .alloc = alloc,
            .mainStackSize = 0,
            .ops = .{},
            .consts = .{},
            .strBuf = .{},
            .strMap = .{},
            .debugTable = .{},
            .debugTempIndexTable = .{},
            .debugMarkers = .{},
            .mconsts = &.{},
            .unwindTempRegs = .{},
            .unwindTempPrevIndexes = .{},
        };
        // Perform big allocation for instruction buffer for more consistent heap allocation.
        try new.ops.ensureTotalCapacityPrecise(alloc, 4096);
        return new;
    }

    pub fn deinit(self: *ByteCodeBuffer) void {
        self.ops.deinit(self.alloc);
        self.consts.deinit(self.alloc);
        self.strBuf.deinit(self.alloc);
        self.strMap.deinit(self.alloc);
        self.debugTable.deinit(self.alloc);
        self.debugTempIndexTable.deinit(self.alloc);
        self.debugMarkers.deinit(self.alloc);
        self.unwindTempRegs.deinit(self.alloc);
        self.unwindTempPrevIndexes.deinit(self.alloc);
    }

    pub fn clear(self: *ByteCodeBuffer) void {
        self.ops.clearRetainingCapacity();
        self.consts.clearRetainingCapacity();
        self.strBuf.clearRetainingCapacity();
        self.strMap.clearRetainingCapacity();
        self.debugTable.clearRetainingCapacity();
        self.debugTempIndexTable.clearRetainingCapacity();
        self.debugMarkers.clearRetainingCapacity();
        self.unwindTempRegs.clearRetainingCapacity();
        self.unwindTempPrevIndexes.clearRetainingCapacity();
    }

    pub inline fn len(self: *ByteCodeBuffer) usize {
        return self.ops.items.len;
    }

    pub fn pushConst(self: *ByteCodeBuffer, val: Const) !u32 {
        const start: u32 = @intCast(self.consts.items.len);
        try self.consts.resize(self.alloc, self.consts.items.len + 1);
        self.consts.items[start] = val;
        return start;
    }

    pub fn pushDebugFuncStart(self: *ByteCodeBuffer, declId: cy.sema.FuncDeclId, chunkId: u32) !void {
        try self.debugMarkers.append(self.alloc, .{
            .type = @intFromEnum(DebugMarkerType.funcStart),
            .pc = @intCast(self.ops.items.len),
            .data = .{
                .funcStart = .{
                    .declId = declId,
                    .chunkId = chunkId,
                },
            },
            .data2 = undefined,
        });
    }

    pub fn pushDebugFuncEnd(self: *ByteCodeBuffer, declId: cy.sema.FuncDeclId, chunkId: u32) !void {
        try self.debugMarkers.append(self.alloc, .{
            .type = @intFromEnum(DebugMarkerType.funcEnd),
            .pc = @intCast(self.ops.items.len),
            .data = .{
                .funcEnd = .{
                    .declId = declId,
                    .chunkId = chunkId,
                },
            },
            .data2 = undefined,
        });
    }

    pub fn pushDebugLabel(self: *ByteCodeBuffer, name: []const u8) !void {
        try self.debugMarkers.append(self.alloc, .{
            .type = @intFromEnum(DebugMarkerType.label),
            .pc = @intCast(self.ops.items.len),
            .data = .{
                .label = .{
                    .namePtr = name.ptr,
                },
            },
            .data2 = .{
                .label = .{
                    .nameLen = @intCast(name.len),
                },
            },
        });
    }

    pub fn pushDebugSym(self: *ByteCodeBuffer, pc: usize, file: u32, loc: u32, frameLoc: u32, unwindTempIdx: u32) !void {
        try self.debugTable.append(self.alloc, .{
            .pc = @intCast(pc),
            .loc = loc,
            .file = @intCast(file),
            .frameLoc = frameLoc,
        });
        try self.debugTempIndexTable.append(self.alloc, unwindTempIdx);
    }

    pub fn pushOp(self: *ByteCodeBuffer, code: OpCode) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 1);
        self.ops.items[start] = Inst.initOpCode(code);
    }

    pub fn pushOp1(self: *ByteCodeBuffer, code: OpCode, arg: u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 2);
        self.ops.items[start] = Inst.initOpCode(code);
        self.ops.items[start+1] = .{ .val = arg };
    }

    pub fn pushOp2(self: *ByteCodeBuffer, code: OpCode, arg: u8, arg2: u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 3);
        self.ops.items[start] = Inst.initOpCode(code);
        self.ops.items[start+1] = .{ .val = arg };
        self.ops.items[start+2] = .{ .val = arg2 };
    }

    pub fn pushOp3(self: *ByteCodeBuffer, code: OpCode, arg: u8, arg2: u8, arg3: u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 4);
        self.ops.items[start] = Inst.initOpCode(code);
        self.ops.items[start+1] = .{ .val = arg };
        self.ops.items[start+2] = .{ .val = arg2 };
        self.ops.items[start+3] = .{ .val = arg3 };
    }

    pub fn pushOperand(self: *ByteCodeBuffer, arg: u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 1);
        self.ops.items[start] = .{ .val = arg };
    }

    pub fn pushOperandsRaw(self: *ByteCodeBuffer, args: []const u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + args.len);
        for (args, 0..) |arg, i| {
            self.ops.items[start+i] = .{ .val = arg };
        }
    }
    
    pub fn pushOpSlice(self: *ByteCodeBuffer, code: OpCode, args: []const u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + args.len + 1);
        self.ops.items[start] = Inst.initOpCode(code);
        for (args, 0..) |arg, i| {
            self.ops.items[start+i+1] = .{ .val = arg };
        }
    }

    pub fn pushOperands(self: *ByteCodeBuffer, operands: []const u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + operands.len);
        for (operands, 0..) |operand, i| {
            self.ops.items[start+i] = .{ .val = operand };
        }
    }

    pub fn setOpArgU16(self: *ByteCodeBuffer, idx: usize, arg: u16) void {
        @as(*align(1) u16, @ptrCast(&self.ops.items[idx])).* = arg;
    }

    pub fn setOpArgs1(self: *ByteCodeBuffer, idx: usize, arg: u8) void {
        self.ops.items[idx].val = arg;
    }

    pub fn getOrPushStringConst(self: *ByteCodeBuffer, str: []const u8) !u32 {
        const val = try self.getOrPushStringValue(str);
        const idx: u32 = @intCast(self.consts.items.len);
        try self.consts.append(self.alloc, Const.init(val.val));
        return idx;
    }

    pub fn getOrPushUstring(self: *ByteCodeBuffer, str: []const u8, charLen: u32) !cy.IndexSlice(u32) {
        const ctx = StringIndexContext{ .buf = &self.strBuf };
        const insertCtx = StringIndexInsertContext{ .buf = &self.strBuf };
        const res = try self.strMap.getOrPutContextAdapted(self.alloc, str, insertCtx, ctx);
        if (res.found_existing) {
            return res.key_ptr.*;
        } else {
            // Reserve 12 bytes for charLen, mruIdx, mruCharIdx.
            try self.strBuf.ensureUnusedCapacity(self.alloc, 12);
            const start: u32 = @intCast(self.strBuf.items.len);
            @as(*align(1) u32, @ptrCast(self.strBuf.items.ptr + start)).* = charLen;
            @as(*align(1) u32, @ptrCast(self.strBuf.items.ptr + start + 4)).* = 0;
            @as(*align(1) u32, @ptrCast(self.strBuf.items.ptr + start + 8)).* = 0;
            self.strBuf.items.len += 12;
            try self.strBuf.appendSlice(self.alloc, str);
            res.key_ptr.* = cy.IndexSlice(u32).init(start + 12, @intCast(self.strBuf.items.len));
            return res.key_ptr.*;
        }
    }

    pub fn getOrPushAstring(self: *ByteCodeBuffer, str: []const u8) !cy.IndexSlice(u32) {
        const ctx = StringIndexContext{ .buf = &self.strBuf };
        const insertCtx = StringIndexInsertContext{ .buf = &self.strBuf };
        const res = try self.strMap.getOrPutContextAdapted(self.alloc, str, insertCtx, ctx);
        if (res.found_existing) {
            return res.key_ptr.*;
        } else {
            const start: u32 = @intCast(self.strBuf.items.len);
            try self.strBuf.appendSlice(self.alloc, str);
            res.key_ptr.* = cy.IndexSlice(u32).init(start, @intCast(self.strBuf.items.len));
            return res.key_ptr.*;
        }
    }

    pub fn getOrPushStringValue(self: *ByteCodeBuffer, str: []const u8) linksection(cy.CompilerSection) !cy.Value {
        if (cy.validateUtf8(str)) |charLen| {
            if (charLen == str.len) {
                const slice = try self.getOrPushAstring(str);
                return cy.Value.initStaticAstring(slice.start, @intCast(slice.end - slice.start));
            } else {
                const slice = try self.getOrPushUstring(str, @intCast(charLen));
                return cy.Value.initStaticUstring(slice.start, @intCast(slice.end - slice.start));
            }
        } else {
            return error.InvalidUtf8;
        }
    }
};

fn printStderr(comptime format: []const u8, args: anytype) void {
    if (cy.verbose and !cy.isWasm) {
        std.debug.print(format, args);
    }
}

pub fn dumpInst(pcOffset: u32, code: OpCode, pc: [*]const Inst, len: usize, extra: []const u8) void {
    switch (code) {
        .addFloat => {
            const left = pc[1].val;
            const right = pc[2].val;
            const dst = pc[3].val;
            fmt.printStderr("{} {} left={} right={} dst={}", &.{v(pcOffset), v(code), v(left), v(right), v(dst)});
        },
        .box => {
            const local = pc[1].val;
            const dst = pc[2].val;
            fmt.printStderr("{} {} local={}, dst={}", &.{v(pcOffset), v(code), v(local), v(dst)});
        },
        .captured => {
            const closureLocal = pc[1].val;
            const varIdx = pc[2].val;
            const dst = pc[3].val;
            fmt.printStderr("{} {} closureLocal={}, varIdx={}, dst={}", &.{v(pcOffset), v(code), v(closureLocal), v(varIdx), v(dst)});
        },
        .boxValue => {
            const local = pc[1].val;
            const dst = pc[2].val;
            fmt.printStderr("{} {} local={}, dst={}", &.{v(pcOffset), v(code), v(local), v(dst)});
        },
        .callObjNativeFuncIC => {
            const startLocal = pc[1].val;
            const numArgs = pc[2].val;
            const numRet = pc[3].val;
            fmt.printStderr("{} {} startLocal={}, numArgs={}, numRet={}", &.{v(pcOffset), v(code), v(startLocal), v(numArgs), v(numRet)});
        },
        .callObjSym => {
            const startLocal = pc[1].val;
            const numArgs = pc[2].val;
            const numRet = pc[3].val;
            const symId = pc[4].val;
            const funcSigId = @as(*const align(1) u16, @ptrCast(pc + 5)).*;
            fmt.printStderr("{} {} startLocal={}, numArgs={}, numRet={}, sym={}, funcSigId={}", &.{v(pcOffset), v(code), v(startLocal), v(numArgs), v(numRet), v(symId), v(funcSigId)});
        },
        .callSym => {
            const startLocal = pc[1].val;
            const numArgs = pc[2].val;
            const numRet = pc[3].val;
            const symId = @as(*const align(1) u16, @ptrCast(pc + 4)).*;
            fmt.printStderr("{} {} startLocal={}, numArgs={}, numRet={}, sym={}", &.{v(pcOffset), v(code), v(startLocal), v(numArgs), v(numRet), v(symId)});
        },
        .callFuncIC => {
            const startLocal = pc[1].val;
            const numRet = pc[3].val;
            const stackSize = pc[4].val;
            const pcPtr: [*]cy.Inst = @ptrFromInt(@as(usize, @intCast(@as(*const align(1) u48, @ptrCast(pc + 6)).*)));
            fmt.printStderr("{} {} startLocal={}, numRet={}, stackSize={}, pcPtr={}", &.{v(pcOffset), v(code), v(startLocal), v(numRet), v(stackSize), v(pcPtr)});
        },
        .call => {
            const startLocal = pc[1].val;
            const numArgs = pc[2].val;
            const numRet = pc[3].val;
            fmt.printStderr("{} {} startLocal={}, numArgs={}, numRet={}", &.{v(pcOffset), v(code), v(startLocal), v(numArgs), v(numRet)});
        },
        .closure => {
            const negFuncPcOffset = pc[1].val;
            const numParams = pc[2].val;
            const numCaptured = pc[3].val;
            const numLocals = pc[4].val;
            const funcSigId = @as(*const align(1) u16, @ptrCast(pc + 5)).*;
            const local = pc[7].val;
            const dst = pc[8].val;
            fmt.printStderr("{} {} negFuncPcOffset={}, numParams={}, numCaptured={}, numLocals={}, funcSigId={}, closureLocal={}, dst={}", &.{v(pcOffset), v(code), v(negFuncPcOffset), v(numParams), v(numCaptured), v(numLocals), v(funcSigId), v(local), v(dst)});
            printStderr(" {any}", .{std.mem.sliceAsBytes(pc[9..9+numCaptured])});
        },
        .constI8 => {
            const val: i8 = @bitCast(pc[1].val);
            const dst = pc[2].val;
            fmt.printStderr("{} {} val={} dst={}", &.{v(pcOffset), v(code), v(val), v(dst)});
        },
        .constOp => {
            const idx = @as(*const align (1) u16, @ptrCast(pc + 1)).*;
            const dst = pc[3].val;
            fmt.printStderr("{} {} constIdx={} dst={}", &.{v(pcOffset), v(code), v(idx), v(dst)});
        },
        .copy => {
            const src = pc[1].val;
            const dst = pc[2].val;
            fmt.printStderr("{} {} src={} dst={}", &.{v(pcOffset), v(code), v(src), v(dst)});
        },
        .copyReleaseDst => {
            const src = pc[1].val;
            const dst = pc[2].val;
            fmt.printStderr("{} {} src={} dst={}", &.{v(pcOffset), v(code), v(src), v(dst)});
        },
        .copyRetainSrc => {
            const src = pc[1].val;
            const dst = pc[2].val;
            fmt.printStderr("{} {} src={} dst={}", &.{v(pcOffset), v(code), v(src), v(dst)});
        },
        .end => {
            const endLocal = pc[1].val;
            fmt.printStderr("{} {} endLocal={}", &.{v(pcOffset), v(code), v(endLocal) });
        },
        .compare => {
            const left = pc[1].val;
            const right = pc[2].val;
            const dst = pc[3].val;
            fmt.printStderr("{} {} left={}, right={}, dst={}", &.{v(pcOffset), v(code), v(left), v(right), v(dst)});
        },
        .field => {
            const recv = pc[1].val;
            const dst = pc[2].val;
            const symId = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            fmt.printStderr("{} {} recv={}, dst={}, sym={}", &.{v(pcOffset), v(code), v(recv), v(dst), v(symId)});
        },
        .fieldRetain => {
            const recv = pc[1].val;
            const dst = pc[2].val;
            const symId = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            fmt.printStderr("{} {} recv={}, dst={}, sym={}", &.{v(pcOffset), v(code), v(recv), v(dst), v(symId)});
        },
        .forRangeInit => {
            const start = pc[1].val;
            const end = pc[2].val;
            const step = pc[3].val;
            const forRangeInstOffset = @as(*const align(1) u16, @ptrCast(pc + 6)).*;
            fmt.printStderr("{} {} start={}, end={}, step={}, forRangeInstOffset={}", &.{v(pcOffset), v(code), v(start), v(end), v(step), v(forRangeInstOffset)});
        },
        .forRangeReverse,
        .forRange => {
            const counter = pc[1].val;
            const step = pc[2].val;
            const end = pc[3].val;
            const userCounter = pc[4].val;
            const negOffset = @as(*const align(1) u16, @ptrCast(pc + 5)).*;
            fmt.printStderr("{} {} counter={}, step={}, end={}, userCounter={}, negOffset={}", &.{v(pcOffset), v(code), v(counter), v(step), v(end), v(userCounter), v(negOffset)});
        },
        .indexList => {
            const recv = pc[1].val;
            const index = pc[2].val;
            const dst = pc[3].val;
            fmt.printStderr("{} {} recv={}, index={}, dst={}", &.{v(pcOffset), v(code), v(recv), v(index), v(dst)});
        },
        .jump => {
            const jump = @as(*const align(1) i16, @ptrCast(pc + 1)).*;
            fmt.printStderr("{} {} offset={}", &.{v(pcOffset), v(code), v(jump)});
        },
        .jumpNotCond => {
            const jump = @as(*const align(1) u16, @ptrCast(pc + 2)).*;
            fmt.printStderr("{} {} cond={}, offset={}", &.{v(pcOffset), v(code), v(pc[1].val), v(jump)});
        },
        .jumpNotNone => {
            const jump = @as(*const align(1) i16, @ptrCast(pc + 1)).*;
            fmt.printStderr("{} {} offset={}, cond={}", &.{v(pcOffset), v(code), v(jump), v(pc[3].val)});
        },
        .lambda => {
            const negFuncPcOffset = pc[1].val;
            const numParams = pc[2].val;
            const numLocals = pc[3].val;
            const funcSigId = @as(*const align(1) u16, @ptrCast(pc + 4)).*;
            const dst = pc[6].val;
            fmt.printStderr("{} {} negFuncPcOffset={}, numParams={}, numLocals={}, funcSigId={}, dst={}", &.{v(pcOffset), v(code), v(negFuncPcOffset), v(numParams), v(numLocals), v(funcSigId), v(dst)});
        },
        .list => {
            const startLocal = pc[1].val;
            const numElems = pc[2].val;
            const dst = pc[3].val;
            fmt.printStderr("{} {} startLocal={}, numElems={}, dst={}", &.{v(pcOffset), v(code), v(startLocal), v(numElems), v(dst)});
        },
        .map => {
            const startLocal = pc[1].val;
            const numEntries = pc[2].val;
            const dst = pc[3].val;
            fmt.printStderr("{} {} startLocal={}, numEntries={}, dst={}", &.{v(pcOffset), v(code), v(startLocal), v(numEntries), v(dst)});
            const keyIdxes = pc[4..4+numEntries*2];
            printStderr(" {any}", .{std.mem.sliceAsBytes(keyIdxes)});
        },
        .object => {
            const typeId = pc[1].val;
            const startLocal = pc[2].val;
            const numFields = pc[3].val;
            const dst = pc[4].val;
            fmt.printStderr("{} {} typeId={}, startLocal={}, numFields={}, dst={}", &.{v(pcOffset), v(code),
                v(typeId), v(startLocal), v(numFields), v(dst)});
        },
        .objectSmall => {
            const typeId = pc[1].val;
            const startLocal = pc[2].val;
            const numFields = pc[3].val;
            const dst = pc[4].val;
            fmt.printStderr("{} {} typeId={}, startLocal={}, numFields={}, dst={}", &.{v(pcOffset), v(code),
                v(typeId), v(startLocal), v(numFields), v(dst)});
        },
        .release => {
            const local = pc[1].val;
            fmt.printStderr("{} {} local={}", &.{v(pcOffset), v(code), v(local)});
        },
        .setFieldRelease => {
            const recv = pc[1].val;
            const val = pc[2].val;
            const symId = pc[3].val;
            fmt.printStderr("{} {} recv={}, val={}, sym={}", &.{v(pcOffset), v(code), v(recv), v(val), v(symId)});
        },
        .setIndexList => {
            const list = pc[1].val;
            const index = pc[2].val;
            const right = pc[3].val;
            fmt.printStderr("{} {} list={}, index={}, right={}", &.{v(pcOffset), v(code), v(list), v(index), v(right)});
        },
        .init => {
            const start = pc[1].val;
            const numLocals = pc[2].val;
            fmt.printStderr("{} {} start={}, numLocals={}", &.{v(pcOffset), v(code), v(start), v(numLocals) });
        },
        .coinit => {
            const startArgs = pc[1].val;
            const numArgs = pc[2].val;
            const jump = pc[3].val;
            const initialStackSize = pc[4].val;
            const dst = pc[5].val;
            fmt.printStderr("{} {} startArgs={}, numArgs={}, jump={}, initialStackSize={}, dst={}", &.{v(pcOffset), v(code), v(startArgs), v(numArgs), v(jump), v(initialStackSize), v(dst)});
        },
        .coresume => {
            const fiberLocal = pc[1].val;
            const retLocal = pc[2].val;
            fmt.printStderr("{} {} fiberLocal={}, retLocal={}", &.{v(pcOffset), v(code), v(fiberLocal), v(retLocal) });
        },
        .sliceList => {
            const recv = pc[1].val;
            const start = pc[2].val;
            const end = pc[3].val;
            const dst = pc[4].val;
            fmt.printStderr("{} {} recv={}, start={}, end={}, dst={}", &.{v(pcOffset), v(code), v(recv), v(start), v(end), v(dst)});
        },
        .staticVar => {
            const symId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const dst = pc[3].val;
            fmt.printStderr("{} {} sym={} dst={}", &.{v(pcOffset), v(code), v(symId), v(dst)});
        },
        .pushTry => {
            const errDst = pc[1].val;
            const catchPcOffset = @as(*const align(1) u16, @ptrCast(pc + 2)).*;
            fmt.printStderr("{} {} errDst={} catchPcOffset={}", &.{v(pcOffset), v(code), v(errDst), v(catchPcOffset)});
        },
        .stringTemplate => {
            const startLocal = pc[1].val;
            const exprCount = pc[2].val;
            const dst = pc[3].val;
            fmt.printStderr("{} {} startLocal={}, exprCount={}, dst={}", &.{v(pcOffset), v(code), v(startLocal), v(exprCount), v(dst)});
        },
        else => {
            fmt.printStderr("{} {}", &.{v(pcOffset), v(code)});
            printStderr(" {any}", .{std.mem.sliceAsBytes(pc[1..len])});
        },
    }
    fmt.printStderr(" {}\n", &.{v(extra)});
}

pub const StringIndexContext = struct {
    buf: *std.ArrayListUnmanaged(u8),

    pub fn hash(self: StringIndexContext, s: cy.IndexSlice(u32)) u64 {
        return std.hash.Wyhash.hash(0, self.buf.items[s.start..s.end]);
    }

    pub fn eql(self: StringIndexContext, a: cy.IndexSlice(u32), b: cy.IndexSlice(u32)) bool {
        return std.mem.eql(u8, self.buf.items[a.start..a.end], self.buf.items[b.start..b.end]);
    }
};

pub const StringIndexInsertContext = struct {
    buf: *std.ArrayListUnmanaged(u8),

    pub fn hash(self: StringIndexInsertContext, s: []const u8) u64 {
        _ = self;
        return std.hash.Wyhash.hash(0, s);
    }

    pub fn eql(self: StringIndexInsertContext, a: []const u8, b: cy.IndexSlice(u32)) bool {
        return std.mem.eql(u8, a, self.buf.items[b.start..b.end]);
    }
};

pub const Const = extern union {
    val: u64,
    two: extern struct {
        lower: u32,
        upper: u32,
    },

    pub fn init(val: u64) Const {
        return .{ .val = val };
    }
};

pub const Inst = packed struct {
    val: u8,

    pub inline fn initOpCode(code_: OpCode) Inst {
        return .{
            .val = @intFromEnum(code_),
        };
    }

    pub inline fn opcode(self: *const Inst) OpCode {
        return @enumFromInt(self.val);
    }

    pub fn initArg(arg: u8) Inst {
        return .{
            .val = arg,
        };
    }
};

pub const DebugSym = extern struct {
    /// Start position of an inst.
    pc: u32,

    /// Points to a cy.NodeId.
    loc: u32,

    /// Points to the parent function decl's cy.NodeId or NullId if it's in the main block.
    frameLoc: u32,

    /// CompileChunkId.
    file: u32,
};

const DebugMarkerType = enum(u8) {
    label,
    funcStart,

    /// `pc` is exclusive.
    funcEnd,
};

const DebugMarker = extern struct {
    data: extern union {
        label: extern struct {
            /// Unowned.
            namePtr: [*]const u8,
        },
        funcStart: extern struct {
            chunkId: u32,
            declId: cy.sema.FuncDeclId,
        },
        funcEnd: extern struct {
            chunkId: u32,
            declId: cy.sema.FuncDeclId,
        },
    },

    /// Inst position of marker.
    pc: u32,

    data2: extern union {
        label: extern struct {
            nameLen: u16,
        },
    },

    type: u8,

    pub fn getLabelName(self: DebugMarker) []const u8 {
        return self.data.label.namePtr[0..self.data2.label.nameLen];
    }

    pub fn etype(self: *const DebugMarker) DebugMarkerType {
        return @enumFromInt(self.type);
    }
};

pub const CallObjSymInstLen = vmc.CALL_OBJ_SYM_INST_LEN;
pub const CallSymInstLen = vmc.CALL_SYM_INST_LEN;
pub const CallInstLen = vmc.CALL_INST_LEN;

test "getInstLenAt" {
    var code = Inst.initOpCode(.call);
    try t.eq(getInstLenAt(@ptrCast(&code)), CallInstLen);
}

pub fn getInstLenAt(pc: [*]const Inst) u8 {
    switch (pc[0].opcode()) {
        .ret0,
        .ret1,
        .coreturn => {
            return 1;
        },
        .not,
        .throw,
        .retain,
        .end,
        .release,
        .none,
        .true,
        .false,
        .mapEmpty => {
            return 2;
        },
        .releaseN => {
            const numVars = pc[1].val;
            return 2 + numVars;
        },
        .init,
        .popTry,
        .copy,
        .copyRetainSrc,
        .copyReleaseDst,
        .copyRetainRelease,
        .constI8,
        .jump,
        .coyield,
        .coresume,
        .box,
        .setBoxValue,
        .setBoxValueRelease,
        .boxValue,
        .boxValueRetain,
        .tagLiteral => {
            return 3;
        },
        .call,
        .captured,
        .constOp,
        .staticVar,
        .setStaticVar,
        .staticFunc,
        .setStaticFunc,
        .pushTry,
        .jumpNotNone,
        .jumpCond,
        .setField,
        .compare,
        .compareNot,
        .list,
        .tag,
        .cast,
        .castAbstract,
        .jumpNotCond => {
            return 4;
        },
        .stringTemplate => {
            const numExprs = pc[2].val;
            return 4 + numExprs + 1;
        },
        .map => {
            const numEntries = pc[2].val;
            return 4 + numEntries * 2;
        },
        .callTypeCheck,
        .object,
        .objectSmall => {
            return 5;
        },
        .match => {
            const numConds = pc[2].val;
            return 5 + numConds * 3;
        },
        .coinit => {
            return 6;
        },
        .lambda,
        .sym,
        .forRange,
        .forRangeReverse,
        .setCheckFieldRelease,
        .setFieldRelease,
        .setFieldReleaseIC => {
            return 7;
        },
        .fieldRetain,
        .fieldRetainIC,
        .field,
        .fieldIC,
        .forRangeInit => {
            return 8;
        },
        .closure => {
            const numCaptured = pc[3].val;
            return 9 + numCaptured;
        },
        .callSym,
        .callNativeFuncIC,
        .callFuncIC => {
            return CallSymInstLen;
        },
        .lessFloat,
        .greaterFloat,
        .lessEqualFloat,
        .greaterEqualFloat,
        .lessInt,
        .greaterInt,
        .lessEqualInt,
        .greaterEqualInt,
        .sliceList,
        .indexList,
        .indexMap,
        .addFloat,
        .subFloat,
        .mulFloat,
        .divFloat,
        .powFloat,
        .modFloat,
        .addInt,
        .subInt,
        .mulInt,
        .divInt,
        .powInt,
        .modInt,
        .negFloat,
        .negInt,
        .bitwiseNot,
        .bitwiseAnd,
        .bitwiseOr,
        .bitwiseXor,
        .bitwiseLeftShift,
        .bitwiseRightShift,
        .callObjSym,
        .callObjNativeFuncIC,
        .setIndexList,
        .setIndexMap,
        .callObjFuncIC => {
            return CallObjSymInstLen;
        },
    }
}

pub const OpCode = enum(u8) {
    /// Copies a constant value from `consts` to a dst local.
    /// [constIdx u16] [dst]
    constOp = vmc.CodeConstOp,
    constI8 = vmc.CodeConstI8,
    addFloat = vmc.CodeAddFloat,
    subFloat = vmc.CodeSubFloat,
    /// Push boolean onto register stack.
    true = vmc.CodeTrue,
    false = vmc.CodeFalse,
    /// Sets the `none` value to a dst local.
    none = vmc.CodeNone,
    /// Pops top register, performs not, and pushes result onto stack.
    not = vmc.CodeNot,
    /// Copies a local from src to dst.
    copy = vmc.CodeCopy,
    copyReleaseDst = vmc.CodeCopyReleaseDst,
    setIndexList = vmc.CodeSetIndexList,
    setIndexMap = vmc.CodeSetIndexMap,

    copyRetainSrc = vmc.CodeCopyRetainSrc,

    indexList = vmc.CodeIndexList,
    indexMap = vmc.CodeIndexMap,

    /// First operand points the first elem and also the dst local. Second operand contains the number of elements.
    list = vmc.CodeList,
    /// First operand points the first entry value and also the dst local. Second operand contains the number of elements.
    /// Const key indexes follow the size operand.
    map = vmc.CodeMap,
    mapEmpty = vmc.CodeMapEmpty,
    sliceList = vmc.CodeSliceList,
    /// Pops top register, if value evals to false, jumps the pc forward by an offset.
    jumpNotCond = vmc.CodeJumpNotCond,
    jumpCond = vmc.CodeJumpCond,
    /// Jumps the pc by an 16-bit integer offset.
    jump = vmc.CodeJump,

    release = vmc.CodeRelease,

    /// Exclusively used for block end to distinguish from temp releases.
    releaseN = vmc.CodeReleaseN,

    callObjSym = vmc.CodeCallObjSym,
    callObjNativeFuncIC = vmc.CodeCallObjNativeFuncIC,
    callObjFuncIC = vmc.CodeCallObjFuncIC,

    /// Runtime type check.
    callTypeCheck = vmc.CodeCallTypeCheck,

    callSym = vmc.CodeCallSym,
    callFuncIC = vmc.CodeCallFuncIC,
    callNativeFuncIC = vmc.CodeCallNativeFuncIC,
    ret1 = vmc.CodeRet1,
    ret0 = vmc.CodeRet0,

    /// Calls a lambda.
    /// [calleeLocal] [numArgs] [numRet=0/1]
    call = vmc.CodeCall,

    field = vmc.CodeField,
    fieldIC = vmc.CodeFieldIC,
    fieldRetain = vmc.CodeFieldRetain,
    fieldRetainIC = vmc.CodeFieldRetainIC,
    lambda = vmc.CodeLambda,
    closure = vmc.CodeClosure,
    compare = vmc.CodeCompare,
    lessFloat = vmc.CodeLessFloat,
    greaterFloat = vmc.CodeGreaterFloat,
    lessEqualFloat = vmc.CodeLessEqualFloat,
    greaterEqualFloat = vmc.CodeGreaterEqualFloat,
    lessInt = vmc.CodeLessInt,
    greaterInt = vmc.CodeGreaterInt,
    lessEqualInt = vmc.CodeLessEqualInt,
    greaterEqualInt = vmc.CodeGreaterEqualInt,

    mulFloat = vmc.CodeMulFloat,
    divFloat = vmc.CodeDivFloat,
    powFloat = vmc.CodePowFloat,
    modFloat = vmc.CodeModFloat,

    compareNot = vmc.CodeCompareNot,

    /// [startLocal] [exprCount] [dst] [..string consts]
    stringTemplate = vmc.CodeStringTemplate,
    negFloat = vmc.CodeNegFloat,

    /// Initialize locals starting from `startLocal` to the `none` value.
    /// init [startLocal] [numLocals]
    init = vmc.CodeInit,

    objectSmall = vmc.CodeObjectSmall,
    object = vmc.CodeObject,
    setField = vmc.CodeSetField,
    setFieldRelease = vmc.CodeSetFieldRelease,
    setFieldReleaseIC = vmc.CodeSetFieldReleaseIC,

    /// set field with runtime type check.
    setCheckFieldRelease = vmc.CodeSetCheckFieldRelease,

    coinit = vmc.CodeCoinit,
    coyield = vmc.CodeCoyield,
    coresume = vmc.CodeCoresume,
    coreturn = vmc.CodeCoreturn,
    retain = vmc.CodeRetain,
    copyRetainRelease = vmc.CodeCopyRetainRelease,

    /// Lifts a source local to a box object and stores the result in `dstLocal`.
    /// The source local is also retained.
    /// [srcLocal] [dstLocal]
    box = vmc.CodeBox,

    setBoxValue = vmc.CodeSetBoxValue,
    setBoxValueRelease = vmc.CodeSetBoxValueRelease,
    boxValue = vmc.CodeBoxValue,
    boxValueRetain = vmc.CodeBoxValueRetain,
    captured = vmc.CodeCaptured,
    /// TODO: Rename to enumOp.
    tag = vmc.CodeTag,
    /// TODO: Rename to symbol.
    tagLiteral = vmc.CodeTagLiteral,

    bitwiseAnd = vmc.CodeBitwiseAnd,
    bitwiseOr = vmc.CodeBitwiseOr,
    bitwiseXor = vmc.CodeBitwiseXor,
    bitwiseNot = vmc.CodeBitwiseNot,
    bitwiseLeftShift = vmc.CodeBitwiseLeftShift,
    bitwiseRightShift = vmc.CodeBitwiseRightShift,
    jumpNotNone = vmc.CodeJumpNotNone,
    addInt = vmc.CodeAddInt,
    subInt = vmc.CodeSubInt,
    mulInt = vmc.CodeMulInt,
    divInt = vmc.CodeDivInt,
    modInt = vmc.CodeModInt,
    powInt = vmc.CodePowInt,
    negInt = vmc.CodeNegInt,
    forRangeInit = vmc.CodeForRangeInit,
    forRange = vmc.CodeForRange,
    forRangeReverse = vmc.CodeForRangeReverse,

    /// Performs an eq comparison with a sequence of locals.
    /// The pc then jumps with the offset of the matching local, otherwise the offset from the end is used.
    /// [exprLocal] [numCases] [case1Local] [case1Jump] ... [elseJump]
    match = vmc.CodeMatch,

    /// Copies and retains a static variable to a destination local.
    /// [symId u16] [dstLocal]
    staticVar = vmc.CodeStaticVar,

    /// Copies a local register to a static variable.
    /// [symId u16] [local]
    setStaticVar = vmc.CodeSetStaticVar,

    /// Wraps a static function in a function value.
    /// [symId u16] [dstLocal]
    staticFunc = vmc.CodeStaticFunc,

    /// Copies a local register to a static function.
    /// [symId u16] [local] 
    setStaticFunc = vmc.CodeSetStaticFunc,

    /// Allocates a symbol object to a destination local.
    /// [symType] [symId] [dst]
    /// TODO: Rename to typeOp.
    sym = vmc.CodeSym,

    cast = vmc.CodeCast,
    castAbstract = vmc.CodeCastAbstract,

    pushTry = vmc.CodePushTry,
    popTry = vmc.CodePopTry,
    throw = vmc.CodeThrow,

    /// Indicates the end of the main script.
    end = vmc.CodeEnd,
};

test "bytecode internals." {
    try t.eq(std.enums.values(OpCode).len, 106);
    try t.eq(@sizeOf(Inst), 1);
    try t.eq(@sizeOf(Const), 8);
    try t.eq(@alignOf(Const), 8);
    try t.eq(@sizeOf(DebugMarker), 16);
    try t.eq(@sizeOf(DebugSym), 16);

    try t.eq(@offsetOf(ByteCodeBuffer, "alloc"), @offsetOf(vmc.ByteCodeBuffer, "alloc"));
    try t.eq(@offsetOf(ByteCodeBuffer, "mainStackSize"), @offsetOf(vmc.ByteCodeBuffer, "mainStackSize"));
    try t.eq(@offsetOf(ByteCodeBuffer, "ops"), @offsetOf(vmc.ByteCodeBuffer, "ops"));
    try t.eq(@offsetOf(ByteCodeBuffer, "consts"), @offsetOf(vmc.ByteCodeBuffer, "consts"));
    try t.eq(@offsetOf(ByteCodeBuffer, "mconsts"), @offsetOf(vmc.ByteCodeBuffer, "mconsts_buf"));
    try t.eq(@offsetOf(ByteCodeBuffer, "strBuf"), @offsetOf(vmc.ByteCodeBuffer, "strBuf"));
    try t.eq(@offsetOf(ByteCodeBuffer, "strMap"), @offsetOf(vmc.ByteCodeBuffer, "strMap"));
    try t.eq(@offsetOf(ByteCodeBuffer, "debugTable"), @offsetOf(vmc.ByteCodeBuffer, "debugTable"));
    try t.eq(@offsetOf(ByteCodeBuffer, "debugMarkers"), @offsetOf(vmc.ByteCodeBuffer, "debugMarkers"));
}