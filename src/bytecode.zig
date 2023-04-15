const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.bytecode);
const fmt = @import("fmt.zig");
const debug = @import("debug.zig");
const v = fmt.v;
const vmc = @import("vm_c.zig");

/// Holds vm instructions.
pub const ByteCodeBuffer = struct {
    alloc: std.mem.Allocator,
    /// The required stack size for the main frame.
    mainStackSize: u32,
    ops: std.ArrayListUnmanaged(InstDatum),
    consts: std.ArrayListUnmanaged(Const),

    /// After compilation, consts is merged into the ops buffer.
    /// This should be used by the interpreter to read const values.
    mconsts: []const Const,

    /// Contiguous constant strings in a buffer.
    strBuf: std.ArrayListUnmanaged(u8),
    /// Tracks the start index of strings that are already in strBuf.
    strMap: std.HashMapUnmanaged(stdx.IndexSlice(u32), u32, StringIndexContext, std.hash_map.default_max_load_percentage),

    /// Maps bytecode insts back to source code.
    /// Contains entries ordered by `pc`. 
    debugTable: std.ArrayListUnmanaged(DebugSym),

    /// Ordered labels by `pc` for debugging only.
    debugLabels: std.ArrayListUnmanaged(DebugLabel),

    pub fn init(alloc: std.mem.Allocator) !ByteCodeBuffer {
        var new = ByteCodeBuffer{
            .alloc = alloc,
            .mainStackSize = 0,
            .ops = .{},
            .consts = .{},
            .strBuf = .{},
            .strMap = .{},
            .debugTable = .{},
            .debugLabels = .{},
            .mconsts = &.{},
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
        self.debugLabels.deinit(self.alloc);
    }

    pub fn clear(self: *ByteCodeBuffer) void {
        self.ops.clearRetainingCapacity();
        self.consts.clearRetainingCapacity();
        self.strBuf.clearRetainingCapacity();
        self.strMap.clearRetainingCapacity();
        self.debugTable.clearRetainingCapacity();
        self.debugLabels.clearRetainingCapacity();
    }

    pub inline fn len(self: *ByteCodeBuffer) usize {
        return self.ops.items.len;
    }

    pub fn pushConst(self: *ByteCodeBuffer, val: Const) !u32 {
        const start = @intCast(u32, self.consts.items.len);
        try self.consts.resize(self.alloc, self.consts.items.len + 1);
        self.consts.items[start] = val;
        return start;
    }

    pub fn pushDebugLabel(self: *ByteCodeBuffer, pc: usize, name: []const u8) !void {
        try self.debugLabels.append(self.alloc, .{
            .pc = @intCast(u32, pc),
            .namePtr = name.ptr,
            .nameLen = @intCast(u32, name.len),
        });
    }

    pub fn pushDebugSym(self: *ByteCodeBuffer, pc: usize, file: u32, loc: u32, frameLoc: u32) !void {
        try self.debugTable.append(self.alloc, .{
            .pc = @intCast(u32, pc),
            .loc = loc,
            .file = file,
            .frameLoc = frameLoc,
        });
    }

    pub fn pushOp(self: *ByteCodeBuffer, code: OpCode) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 1);
        self.ops.items[start] = .{ .code = code };
    }

    pub fn pushOp1(self: *ByteCodeBuffer, code: OpCode, arg: u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 2);
        self.ops.items[start] = .{ .code = code };
        self.ops.items[start+1] = .{ .arg = arg };
    }

    pub fn pushOp2(self: *ByteCodeBuffer, code: OpCode, arg: u8, arg2: u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 3);
        self.ops.items[start] = .{ .code = code };
        self.ops.items[start+1] = .{ .arg = arg };
        self.ops.items[start+2] = .{ .arg = arg2 };
    }

    pub fn pushOp3(self: *ByteCodeBuffer, code: OpCode, arg: u8, arg2: u8, arg3: u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 4);
        self.ops.items[start] = .{ .code = code };
        self.ops.items[start+1] = .{ .arg = arg };
        self.ops.items[start+2] = .{ .arg = arg2 };
        self.ops.items[start+3] = .{ .arg = arg3 };
    }

    pub fn pushOperand(self: *ByteCodeBuffer, arg: u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 1);
        self.ops.items[start] = .{ .arg = arg };
    }

    pub fn pushOperandsRaw(self: *ByteCodeBuffer, args: []const u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + args.len);
        for (args, 0..) |arg, i| {
            self.ops.items[start+i] = .{ .arg = arg };
        }
    }
    
    pub fn pushOpSlice(self: *ByteCodeBuffer, code: OpCode, args: []const u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + args.len + 1);
        self.ops.items[start] = .{ .code = code };
        for (args, 0..) |arg, i| {
            self.ops.items[start+i+1] = .{ .arg = arg };
        }
    }

    pub fn pushOperands(self: *ByteCodeBuffer, operands: []const InstDatum) !void {
        try self.ops.appendSlice(self.alloc, operands);
    }

    pub fn setOpArgU16(self: *ByteCodeBuffer, idx: usize, arg: u16) void {
        @ptrCast(*align(1) u16, &self.ops.items[idx]).* = arg;
    }

    pub fn setOpArgs1(self: *ByteCodeBuffer, idx: usize, arg: u8) void {
        self.ops.items[idx].arg = arg;
    }

    pub fn getOrPushStringConst(self: *ByteCodeBuffer, str: []const u8) !u32 {
        const val = try self.getOrPushStringValue(str);
        const idx = @intCast(u32, self.consts.items.len);
        try self.consts.append(self.alloc, Const.init(val.val));
        return idx;
    }

    pub fn getOrPushUstring(self: *ByteCodeBuffer, str: []const u8, charLen: u32) !stdx.IndexSlice(u32) {
        const ctx = StringIndexContext{ .buf = &self.strBuf };
        const insertCtx = StringIndexInsertContext{ .buf = &self.strBuf };
        const res = try self.strMap.getOrPutContextAdapted(self.alloc, str, insertCtx, ctx);
        if (res.found_existing) {
            return res.key_ptr.*;
        } else {
            // Reserve 12 bytes for charLen, mruIdx, mruCharIdx.
            try self.strBuf.ensureUnusedCapacity(self.alloc, 12);
            const start = @intCast(u32, self.strBuf.items.len);
            @ptrCast(*align(1) u32, self.strBuf.items.ptr + start).* = charLen;
            @ptrCast(*align(1) u32, self.strBuf.items.ptr + start + 4).* = 0;
            @ptrCast(*align(1) u32, self.strBuf.items.ptr + start + 8).* = 0;
            self.strBuf.items.len += 12;
            try self.strBuf.appendSlice(self.alloc, str);
            res.key_ptr.* = stdx.IndexSlice(u32).init(start + 12, @intCast(u32, self.strBuf.items.len));
            return res.key_ptr.*;
        }
    }

    pub fn getOrPushAstring(self: *ByteCodeBuffer, str: []const u8) !stdx.IndexSlice(u32) {
        const ctx = StringIndexContext{ .buf = &self.strBuf };
        const insertCtx = StringIndexInsertContext{ .buf = &self.strBuf };
        const res = try self.strMap.getOrPutContextAdapted(self.alloc, str, insertCtx, ctx);
        if (res.found_existing) {
            return res.key_ptr.*;
        } else {
            const start = @intCast(u32, self.strBuf.items.len);
            try self.strBuf.appendSlice(self.alloc, str);
            res.key_ptr.* = stdx.IndexSlice(u32).init(start, @intCast(u32, self.strBuf.items.len));
            return res.key_ptr.*;
        }
    }

    pub fn getOrPushStringValue(self: *ByteCodeBuffer, str: []const u8) linksection(cy.CompilerSection) !cy.Value {
        if (cy.validateUtf8(str)) |charLen| {
            if (charLen == str.len) {
                const slice = try self.getOrPushAstring(str);
                return cy.Value.initStaticAstring(slice.start, @intCast(u15, slice.end - slice.start));
            } else {
                const slice = try self.getOrPushUstring(str, @intCast(u32, charLen));
                return cy.Value.initStaticUstring(slice.start, @intCast(u15, slice.end - slice.start));
            }
        } else {
            return error.InvalidUtf8;
        }
    }
};

fn printStderr(comptime format: []const u8, args: anytype) void {
    if (builtin.is_test) {
        if (@enumToInt(std.log.Level.debug) <= @enumToInt(std.testing.log_level)) {
            std.debug.print(format, args);
        }
    } else {
        if (!cy.isWasm) {
            std.debug.print(format, args);
        }
    }
}

pub fn dumpInst(pcOffset: u32, code: OpCode, pc: [*]const InstDatum, len: usize, extra: []const u8) void {
    switch (code) {
        .add => {
            const left = pc[1].arg;
            const right = pc[2].arg;
            const dst = pc[3].arg;
            fmt.printStderr("{} {} left={} right={} dst={}", &.{v(pcOffset), v(code), v(left), v(right), v(dst)});
        },
        .box => {
            const local = pc[1].arg;
            const dst = pc[2].arg;
            fmt.printStderr("{} {} local={}, dst={}", &.{v(pcOffset), v(code), v(local), v(dst)});
        },
        .captured => {
            const closureLocal = pc[1].arg;
            const varIdx = pc[2].arg;
            const dst = pc[3].arg;
            fmt.printStderr("{} {} closureLocal={}, varIdx={}, dst={}", &.{v(pcOffset), v(code), v(closureLocal), v(varIdx), v(dst)});
        },
        .boxValue => {
            const local = pc[1].arg;
            const dst = pc[2].arg;
            fmt.printStderr("{} {} local={}, dst={}", &.{v(pcOffset), v(code), v(local), v(dst)});
        },
        .callObjSym => {
            const startLocal = pc[1].arg;
            const numArgs = pc[2].arg;
            const numRet = pc[3].arg;
            const symId = pc[4].arg;
            const rFuncSigId = @ptrCast(*const align(1) u16, pc + 5).*;
            fmt.printStderr("{} {} startLocal={}, numArgs={}, numRet={}, sym={}, rFuncSigId={}", &.{v(pcOffset), v(code), v(startLocal), v(numArgs), v(numRet), v(symId), v(rFuncSigId)});
        },
        .callSym => {
            const startLocal = pc[1].arg;
            const numArgs = pc[2].arg;
            const numRet = pc[3].arg;
            const symId = @ptrCast(*const align(1) u16, pc + 4).*;
            fmt.printStderr("{} {} startLocal={}, numArgs={}, numRet={}, sym={}", &.{v(pcOffset), v(code), v(startLocal), v(numArgs), v(numRet), v(symId)});
        },
        .callFuncIC => {
            const startLocal = pc[1].arg;
            const numRet = pc[3].arg;
            const stackSize = pc[4].arg;
            const pcPtr = @intToPtr([*]cy.InstDatum, @intCast(usize, @ptrCast(*const align(1) u48, pc + 6).*));
            fmt.printStderr("{} {} startLocal={}, numRet={}, stackSize={}, pcPtr={}", &.{v(pcOffset), v(code), v(startLocal), v(numRet), v(stackSize), v(pcPtr)});
        },
        .call1 => {
            const startLocal = pc[1].arg;
            const numArgs = pc[2].arg;
            fmt.printStderr("{} {} startLocal={}, numArgs={}", &.{v(pcOffset), v(code), v(startLocal), v(numArgs)});
        },
        .closure => {
            const negFuncPcOffset = pc[1].arg;
            const numParams = pc[2].arg;
            const numCaptured = pc[3].arg;
            const numLocals = pc[4].arg;
            const rFuncSigId = @ptrCast(*const align(1) u16, pc + 5).*;
            const local = pc[7].arg;
            const dst = pc[8].arg;
            fmt.printStderr("{} {} negFuncPcOffset={}, numParams={}, numCaptured={}, numLocals={}, rFuncSigId={}, closureLocal={}, dst={}", &.{v(pcOffset), v(code), v(negFuncPcOffset), v(numParams), v(numCaptured), v(numLocals), v(rFuncSigId), v(local), v(dst)});
            printStderr(" {any}", .{std.mem.sliceAsBytes(pc[9..9+numCaptured])});
        },
        .constI8 => {
            const val = @bitCast(i8, pc[1].arg);
            const dst = pc[2].arg;
            fmt.printStderr("{} {} val={} dst={}", &.{v(pcOffset), v(code), v(val), v(dst)});
        },
        .constOp => {
            const idx = @ptrCast(*const align (1) u16, pc + 1).*;
            const dst = pc[3].arg;
            fmt.printStderr("{} {} constIdx={} dst={}", &.{v(pcOffset), v(code), v(idx), v(dst)});
        },
        .copy => {
            const src = pc[1].arg;
            const dst = pc[2].arg;
            fmt.printStderr("{} {} src={} dst={}", &.{v(pcOffset), v(code), v(src), v(dst)});
        },
        .copyReleaseDst => {
            const src = pc[1].arg;
            const dst = pc[2].arg;
            fmt.printStderr("{} {} src={} dst={}", &.{v(pcOffset), v(code), v(src), v(dst)});
        },
        .copyRetainSrc => {
            const src = pc[1].arg;
            const dst = pc[2].arg;
            fmt.printStderr("{} {} src={} dst={}", &.{v(pcOffset), v(code), v(src), v(dst)});
        },
        .end => {
            const endLocal = pc[1].arg;
            fmt.printStderr("{} {} endLocal={}", &.{v(pcOffset), v(code), v(endLocal) });
        },
        .compare => {
            const left = pc[1].arg;
            const right = pc[2].arg;
            const dst = pc[3].arg;
            fmt.printStderr("{} {} left={}, right={}, dst={}", &.{v(pcOffset), v(code), v(left), v(right), v(dst)});
        },
        .field => {
            const recv = pc[1].arg;
            const dst = pc[2].arg;
            const symId = @ptrCast(*const align(1) u16, pc + 3).*;
            fmt.printStderr("{} {} recv={}, dst={}, sym={}", &.{v(pcOffset), v(code), v(recv), v(dst), v(symId)});
        },
        .fieldRetain => {
            const recv = pc[1].arg;
            const dst = pc[2].arg;
            const symId = @ptrCast(*const align(1) u16, pc + 3).*;
            fmt.printStderr("{} {} recv={}, dst={}, sym={}", &.{v(pcOffset), v(code), v(recv), v(dst), v(symId)});
        },
        .forRangeInit => {
            const start = pc[1].arg;
            const end = pc[2].arg;
            const step = pc[3].arg;
            const forRangeInstOffset = @ptrCast(*const align(1) u16, pc + 6).*;
            fmt.printStderr("{} {} start={}, end={}, step={}, forRangeInstOffset={}", &.{v(pcOffset), v(code), v(start), v(end), v(step), v(forRangeInstOffset)});
        },
        .forRangeReverse,
        .forRange => {
            const counter = pc[1].arg;
            const step = pc[2].arg;
            const end = pc[3].arg;
            const userCounter = pc[4].arg;
            const negOffset = @ptrCast(*const align(1) u16, pc + 5).*;
            fmt.printStderr("{} {} counter={}, step={}, end={}, userCounter={}, negOffset={}", &.{v(pcOffset), v(code), v(counter), v(step), v(end), v(userCounter), v(negOffset)});
        },
        .index => {
            const recv = pc[1].arg;
            const index = pc[2].arg;
            const dst = pc[3].arg;
            fmt.printStderr("{} {} recv={}, index={}, dst={}", &.{v(pcOffset), v(code), v(recv), v(index), v(dst)});
        },
        .jump => {
            const jump = @ptrCast(*const align(1) i16, pc + 1).*;
            fmt.printStderr("{} {} offset={}", &.{v(pcOffset), v(code), v(jump)});
        },
        .jumpNotCond => {
            const jump = @ptrCast(*const align(1) u16, pc + 2).*;
            fmt.printStderr("{} {} cond={}, offset={}", &.{v(pcOffset), v(code), v(pc[1].arg), v(jump)});
        },
        .jumpNotNone => {
            const jump = @ptrCast(*const align(1) i16, pc + 1).*;
            fmt.printStderr("{} {} offset={}, cond={}", &.{v(pcOffset), v(code), v(jump), v(pc[3].arg)});
        },
        .lambda => {
            const negFuncPcOffset = pc[1].arg;
            const numParams = pc[2].arg;
            const numLocals = pc[3].arg;
            const rFuncSigId = @ptrCast(*const align(1) u16, pc + 4).*;
            const dst = pc[6].arg;
            fmt.printStderr("{} {} negFuncPcOffset={}, numParams={}, numLocals={}, rFuncSigId={}, dst={}", &.{v(pcOffset), v(code), v(negFuncPcOffset), v(numParams), v(numLocals), v(rFuncSigId), v(dst)});
        },
        .list => {
            const startLocal = pc[1].arg;
            const numElems = pc[2].arg;
            const dst = pc[3].arg;
            fmt.printStderr("{} {} startLocal={}, numElems={}, dst={}", &.{v(pcOffset), v(code), v(startLocal), v(numElems), v(dst)});
        },
        .map => {
            const startLocal = pc[1].arg;
            const numEntries = pc[2].arg;
            const dst = pc[3].arg;
            fmt.printStderr("{} {} startLocal={}, numEntries={}, dst={}", &.{v(pcOffset), v(code), v(startLocal), v(numEntries), v(dst)});
            const keyIdxes = pc[4..4+numEntries*2];
            printStderr(" {any}", .{std.mem.sliceAsBytes(keyIdxes)});
        },
        .object => {
            const typeId = pc[1].arg;
            const startLocal = pc[2].arg;
            const numFields = pc[3].arg;
            const dst = pc[4].arg;
            fmt.printStderr("{} {} typeId={}, startLocal={}, numFields={}, dst={}", &.{v(pcOffset), v(code),
                v(typeId), v(startLocal), v(numFields), v(dst)});
        },
        .objectSmall => {
            const typeId = pc[1].arg;
            const startLocal = pc[2].arg;
            const numFields = pc[3].arg;
            const dst = pc[4].arg;
            fmt.printStderr("{} {} typeId={}, startLocal={}, numFields={}, dst={}", &.{v(pcOffset), v(code),
                v(typeId), v(startLocal), v(numFields), v(dst)});
        },
        .release => {
            const local = pc[1].arg;
            fmt.printStderr("{} {} local={}", &.{v(pcOffset), v(code), v(local)});
        },
        .setFieldRelease => {
            const recv = pc[1].arg;
            const val = pc[2].arg;
            const symId = pc[3].arg;
            fmt.printStderr("{} {} recv={}, val={}, sym={}", &.{v(pcOffset), v(code), v(recv), v(val), v(symId)});
        },
        .setIndexRelease => {
            const left = pc[1].arg;
            const index = pc[2].arg;
            const right = pc[3].arg;
            fmt.printStderr("{} {} left={}, index={}, right={}", &.{v(pcOffset), v(code), v(left), v(index), v(right)});
        },
        .init => {
            const start = pc[1].arg;
            const numLocals = pc[2].arg;
            fmt.printStderr("{} {} start={}, numLocals={}", &.{v(pcOffset), v(code), v(start), v(numLocals) });
        },
        .coinit => {
            const startArgs = pc[1].arg;
            const numArgs = pc[2].arg;
            const jump = pc[3].arg;
            const initialStackSize = pc[4].arg;
            const dst = pc[5].arg;
            fmt.printStderr("{} {} startArgs={}, numArgs={}, jump={}, initialStackSize={}, dst={}", &.{v(pcOffset), v(code), v(startArgs), v(numArgs), v(jump), v(initialStackSize), v(dst)});
        },
        .coresume => {
            const fiberLocal = pc[1].arg;
            const retLocal = pc[2].arg;
            fmt.printStderr("{} {} fiberLocal={}, retLocal={}", &.{v(pcOffset), v(code), v(fiberLocal), v(retLocal) });
        },
        .slice => {
            const recv = pc[1].arg;
            const start = pc[2].arg;
            const end = pc[3].arg;
            const dst = pc[4].arg;
            fmt.printStderr("{} {} recv={}, start={}, end={}, dst={}", &.{v(pcOffset), v(code), v(recv), v(start), v(end), v(dst)});
        },
        .staticVar => {
            const symId = @ptrCast(*const align(1) u16, pc + 1).*;
            const dst = pc[3].arg;
            fmt.printStderr("{} {} sym={} dst={}", &.{v(pcOffset), v(code), v(symId), v(dst)});
        },
        .stringTemplate => {
            const startLocal = pc[1].arg;
            const exprCount = pc[2].arg;
            const dst = pc[3].arg;
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

    pub fn hash(self: StringIndexContext, s: stdx.IndexSlice(u32)) u64 {
        return std.hash.Wyhash.hash(0, self.buf.items[s.start..s.end]);
    }

    pub fn eql(self: StringIndexContext, a: stdx.IndexSlice(u32), b: stdx.IndexSlice(u32)) bool {
        return std.mem.eql(u8, self.buf.items[a.start..a.end], self.buf.items[b.start..b.end]);
    }
};

pub const StringIndexInsertContext = struct {
    buf: *std.ArrayListUnmanaged(u8),

    pub fn hash(self: StringIndexInsertContext, s: []const u8) u64 {
        _ = self;
        return std.hash.Wyhash.hash(0, s);
    }

    pub fn eql(self: StringIndexInsertContext, a: []const u8, b: stdx.IndexSlice(u32)) bool {
        return std.mem.eql(u8, a, self.buf.items[b.start..b.end]);
    }
};

pub const Const = packed union {
    val: u64,
    two: packed struct {
        lower: u32,
        upper: u32,
    },

    pub fn init(val: u64) Const {
        return .{ .val = val };
    }
};

pub const InstDatum = packed union {
    code: OpCode,
    arg: u8,

    pub fn initArg(arg: u8) InstDatum {
        return .{
            .arg = arg,
        };
    }
};

pub const DebugSym = struct {
    /// Start position of an inst.
    pc: u32,

    /// Points to a cy.NodeId.
    loc: u32,

    /// Points to the parent function decl's cy.NodeId or NullId if it's in the main block.
    frameLoc: u32,

    /// CompileChunkId.
    file: u32,
};

const DebugLabel = struct {
    /// Unowned.
    namePtr: [*]const u8,
    nameLen: u32,

    /// Start position of an inst.
    pc: u32,

    pub fn getName(self: DebugLabel) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

pub const CallObjSymInstLen = 16;
pub const CallSymInstLen = 12;
pub const CallInstLen = 3;

pub fn getInstLenAt(pc: [*]const InstDatum) u8 {
    switch (pc[0].code) {
        .ret0,
        .ret1,
        .coreturn => {
            return 1;
        },
        .neg,
        .not,
        .bitwiseNot,
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
            const numVars = pc[1].arg;
            return 2 + numVars;
        },
        .call0,
        .call1 => {
            return CallInstLen;
        },
        .init,
        .popTry,
        .copy,
        .copyRetainSrc,
        .copyReleaseDst,
        .copyRetainRelease,
        .constI8,
        .constI8Int,
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
        .captured,
        .constOp,
        .staticVar,
        .setStaticVar,
        .staticFunc,
        .setStaticFunc,
        .pushTry,
        .setIndex,
        .setIndexRelease,
        .index,
        .reverseIndex,
        .jumpNotNone,
        .jumpCond,
        .sub,
        .subInt,
        .mul,
        .div,
        .setField,
        .pow,
        .mod,
        .less,
        .lessInt,
        .greater,
        .lessEqual,
        .greaterEqual,
        .compare,
        .compareNot,
        .bitwiseAnd,
        .bitwiseOr,
        .bitwiseXor,
        .bitwiseLeftShift,
        .bitwiseRightShift,
        .list,
        .add,
        .addInt,
        .tag,
        .cast,
        .castAbstract,
        .jumpNotCond => {
            return 4;
        },
        .stringTemplate => {
            const numExprs = pc[2].arg;
            return 4 + numExprs + 1;
        },
        .map => {
            const numEntries = pc[2].arg;
            return 4 + numEntries * 2;
        },
        .slice,
        .object,
        .objectSmall => {
            return 5;
        },
        .match => {
            const numConds = pc[2].arg;
            return 5 + numConds * 3;
        },
        .coinit => {
            return 6;
        },
        .lambda,
        .sym,
        .forRange,
        .forRangeReverse,
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
            const numCaptured = pc[3].arg;
            return 9 + numCaptured;
        },
        .callSym,
        .callNativeFuncIC,
        .callFuncIC => {
            return CallSymInstLen;
        },
        .callObjSym,
        .callObjNativeFuncIC,
        .callObjFuncIC => {
            return CallObjSymInstLen;
        },
    }
}

pub const OpCode = enum(u8) {
    /// Copies a constant value from `consts` to a dst local.
    /// [constIdx u16] [dst]
    constOp = vmc.CodeConstOp,

    /// Sets an immediate i8 value as a number to a dst local.
    constI8 = vmc.CodeConstI8,
    /// Sets an immediate i8 value as an integer to a dst local.
    constI8Int = vmc.CodeConstI8Int,
    /// Add first two locals and stores result to a dst local.
    add = vmc.CodeAdd,
    // addNumber,
    /// Subtracts second local from first local and stores result to a dst local.
    sub = vmc.CodeSub,
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

    /// [leftLocal] [indexLocal] [rightLocal]
    setIndex = vmc.CodeSetIndex,
    /// setIndex in addition to a release on leftLocal.
    setIndexRelease = vmc.CodeSetIndexRelease,

    copyRetainSrc = vmc.CodeCopyRetainSrc,

    /// [leftLocal] [indexLocal] Retains the result of an index operation.
    index = vmc.CodeIndex,

    /// [leftLocal] [indexLocal] Retains the result of a reverse index operation.
    reverseIndex = vmc.CodeReverseIndex,

    /// First operand points the first elem and also the dst local. Second operand contains the number of elements.
    list = vmc.CodeList,
    /// First operand points the first entry value and also the dst local. Second operand contains the number of elements.
    /// Const key indexes follow the size operand.
    map = vmc.CodeMap,
    mapEmpty = vmc.CodeMapEmpty,
    slice = vmc.CodeSlice,
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
    callSym = vmc.CodeCallSym,
    callFuncIC = vmc.CodeCallFuncIC,
    callNativeFuncIC = vmc.CodeCallNativeFuncIC,
    ret1 = vmc.CodeRet1,
    ret0 = vmc.CodeRet0,

    /// Calls a lambda and ensures 0 return values.
    /// [calleeLocal] [numArgs]
    call0 = vmc.CodeCall0,

    /// Calls a lambda and ensures 1 return value.
    /// [calleeLocal] [numArgs]
    call1 = vmc.CodeCall1,

    field = vmc.CodeField,
    fieldIC = vmc.CodeFieldIC,
    fieldRetain = vmc.CodeFieldRetain,
    fieldRetainIC = vmc.CodeFieldRetainIC,
    lambda = vmc.CodeLambda,
    closure = vmc.CodeClosure,
    compare = vmc.CodeCompare,
    less = vmc.CodeLess,
    // lessNumber,
    greater = vmc.CodeGreater,
    lessEqual = vmc.CodeLessEqual,
    greaterEqual = vmc.CodeGreaterEqual,

    /// Multiplies first two locals and stores result to a dst local.
    mul = vmc.CodeMul,
    /// Divides second local from first local and stores result to a dst local.
    div = vmc.CodeDiv,
    /// Raises first local's power to the value of the second local and stores result to a dst local.
    pow = vmc.CodePow,
    /// Perform modulus on the two locals and stores result to a dst local.
    mod = vmc.CodeMod,

    compareNot = vmc.CodeCompareNot,

    /// [startLocal] [exprCount] [dst] [..string consts]
    stringTemplate = vmc.CodeStringTemplate,
    neg = vmc.CodeNeg,

    /// Initialize locals starting from `startLocal` to the `none` value.
    /// init [startLocal] [numLocals]
    init = vmc.CodeInit,

    objectSmall = vmc.CodeObjectSmall,
    object = vmc.CodeObject,
    setField = vmc.CodeSetField,
    setFieldRelease = vmc.CodeSetFieldRelease,
    setFieldReleaseIC = vmc.CodeSetFieldReleaseIC,
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
    lessInt = vmc.CodeLessInt,
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

test "Internals." {
    try t.eq(std.enums.values(OpCode).len, 98);
    try t.eq(@sizeOf(InstDatum), 1);
    try t.eq(@sizeOf(Const), 8);
    try t.eq(@alignOf(Const), 8);
}