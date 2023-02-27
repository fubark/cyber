const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.bytecode);
const fmt = @import("fmt.zig");
const debug = @import("debug.zig");
const v = fmt.v;

/// Holds vm instructions.
pub const ByteCodeBuffer = struct {
    alloc: std.mem.Allocator,
    /// The required stack size for the main frame.
    mainStackSize: u32,
    ops: std.ArrayListUnmanaged(OpData),
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

    pub fn pushOperands(self: *ByteCodeBuffer, operands: []const OpData) !void {
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

pub fn dumpInst(pcOffset: u32, code: OpCode, pc: [*]const OpData, len: usize, extra: []const u8) void {
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
            fmt.printStderr("{} {} startLocal={}, numArgs={}, numRet={}, sym={}", &.{v(pcOffset), v(code), v(startLocal), v(numArgs), v(numRet), v(symId)});
        },
        .callSym => {
            const startLocal = pc[1].arg;
            const numArgs = pc[2].arg;
            const numRet = pc[3].arg;
            const symId = pc[4].arg;
            fmt.printStderr("{} {} startLocal={}, numArgs={}, numRet={}, sym={}", &.{v(pcOffset), v(code), v(startLocal), v(numArgs), v(numRet), v(symId)});
        },
        .closure => {
            const negFuncPcOffset = pc[1].arg;
            const numParams = pc[2].arg;
            const numCaptured = pc[3].arg;
            const numLocals = pc[4].arg;
            const dst = pc[5].arg;
            fmt.printStderr("{} {} negFuncPcOffset={}, numParams={}, numCaptured={}, numLocals={}, dst={}", &.{v(pcOffset), v(code), v(negFuncPcOffset), v(numParams), v(numCaptured), v(numLocals), v(dst)});
            printStderr(" {any}", .{std.mem.sliceAsBytes(pc[6..6+numCaptured])});
        },
        .constI8 => {
            const val = @bitCast(i8, pc[1].arg);
            const dst = pc[2].arg;
            fmt.printStderr("{} {} val={} dst={}", &.{v(pcOffset), v(code), v(val), v(dst)});
        },
        .constOp => {
            const idx = pc[1].arg;
            const dst = pc[2].arg;
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
        .field => {
            const recv = pc[1].arg;
            const dst = pc[2].arg;
            const symId = pc[3].arg;
            fmt.printStderr("{} {} recv={}, dst={}, sym={}", &.{v(pcOffset), v(code), v(recv), v(dst), v(symId)});
        },
        .fieldRetain => {
            const recv = pc[1].arg;
            const dst = pc[2].arg;
            const symId = pc[3].arg;
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
            const jump = @ptrCast(*const align(1) u16, pc + 1).*;
            fmt.printStderr("{} {} offset={}, cond={}", &.{v(pcOffset), v(code), v(jump), v(pc[3].arg)});
        },
        .jumpNotNone => {
            const jump = @ptrCast(*const align(1) i16, pc + 1).*;
            fmt.printStderr("{} {} offset={}, cond={}", &.{v(pcOffset), v(code), v(jump), v(pc[3].arg)});
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
            const keyIdxes = pc[4..4+numEntries];
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
        .setInitN => {
            const numLocals = pc[1].arg;
            const locals = pc[2..2+numLocals];
            fmt.printStderr("{} {} numLocals={}, locals=", &.{v(pcOffset), v(code), v(numLocals) });
            for (locals) |local| {
                fmt.printStderr("{} ", &.{v(local.arg)});
            }
        },
        .slice => {
            const recv = pc[1].arg;
            const start = pc[2].arg;
            const end = pc[3].arg;
            fmt.printStderr("{} {} recv={}, start={}, end={}", &.{v(pcOffset), v(code), v(recv), v(start), v(end)});
        },
        .staticVar => {
            const symId = pc[1].arg;
            const dst = pc[2].arg;
            fmt.printStderr("{} {} sym={} dst={}", &.{v(pcOffset), v(code), v(symId), v(dst)});
        },
        .stringTemplate => {
            const startLocal = pc[1].arg;
            const exprCount = pc[2].arg;
            const dst = pc[3].arg;
            fmt.printStderr("{} {} startLocal={}, exprCount={}, dst={}", &.{v(pcOffset), v(code), v(startLocal), v(exprCount), v(dst)});
        },
        .tryValue => {
            const local = pc[1].arg;
            const dst = pc[2].arg;
            const jump = @ptrCast(*const align(1) u16, pc + 3).*;
            fmt.printStderr("{} {} local={} dst={} jump={}", &.{v(pcOffset), v(code), v(local), v(dst), v(jump)});
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

/// TODO: Rename to InstData.
pub const OpData = packed union {
    code: OpCode,
    arg: u8,

    pub fn initArg(arg: u8) OpData {
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

pub fn getInstLenAt(pc: [*]const OpData) u8 {
    switch (pc[0].code) {
        .ret0,
        .ret1,
        .coreturn => {
            return 1;
        },
        .retain,
        .end,
        .release,
        .none,
        .true,
        .false,
        .mapEmpty => {
            return 2;
        },
        .releaseN,
        .setInitN => {
            const numVars = pc[1].arg;
            return 2 + numVars;
        },
        .copy,
        .not,
        .bitwiseNot,
        .neg,
        .copyRetainSrc,
        .copyReleaseDst,
        .copyRetainRelease,
        .constI8,
        .constI8Int,
        .call0,
        .call1,
        .jump,
        .coyield,
        .coresume,
        .box,
        .setBoxValue,
        .setBoxValueRelease,
        .boxValue,
        .boxValueRetain,
        .tagLiteral,
        .staticFunc,
        .staticVar,
        .setStaticVar,
        .setStaticFunc,
        .constOp => {
            return 3;
        },
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
        .jumpNotCond => {
            return 4;
        },
        .stringTemplate => {
            const numExprs = pc[2].arg;
            return 4 + numExprs + 1;
        },
        .map => {
            const numEntries = pc[2].arg;
            return 4 + numEntries;
        },
        .slice,
        .object,
        .objectSmall,
        .tryValue,
        .lambda => {
            return 5;
        },
        .match => {
            const numConds = pc[2].arg;
            return 5 + numConds * 3;
        },
        .coinit => {
            return 6;
        },
        .closure => {
            const numCaptured = pc[3].arg;
            return 6 + numCaptured;
        },
        .sym,
        .forRange,
        .forRangeReverse,
        .setFieldRelease,
        .setFieldReleaseIC,
        .fieldRetain,
        .fieldRetainIC,
        .field,
        .fieldIC => {
            return 7;
        },
        .forRangeInit => {
            return 8;
        },
        .callSym,
        .callNativeFuncIC,
        .callFuncIC => {
            return 11;
        },
        .callObjSym,
        .callObjNativeFuncIC,
        .callObjFuncIC => {
            return 14;
        },
        else => {
            stdx.panicFmt("unsupported {}", .{pc[0].code});
        },
    }
}

pub const OpCode = enum(u8) {
    /// Copies a constant value from `consts` to a dst local.
    constOp,
    /// Sets an immediate i8 value as a number to a dst local.
    constI8,
    /// Sets an immediate i8 value as an integer to a dst local.
    constI8Int,
    /// Add first two locals and stores result to a dst local.
    add,
    // addNumber,
    /// Subtracts second local from first local and stores result to a dst local.
    sub,
    /// Push boolean onto register stack.
    true,
    false,
    /// Sets the `none` value to a dst local.
    none,
    /// Pops top register, performs not, and pushes result onto stack.
    not,
    /// Copies a local from src to dst.
    copy,
    copyReleaseDst,

    /// [leftLocal] [indexLocal] [rightLocal]
    setIndex,
    /// setIndex in addition to a release on leftLocal.
    setIndexRelease,

    copyRetainSrc,

    /// [leftLocal] [indexLocal] Retains the result of an index operation.
    index,

    /// [leftLocal] [indexLocal] Retains the result of a reverse index operation.
    reverseIndex,

    /// First operand points the first elem and also the dst local. Second operand contains the number of elements.
    list,
    /// First operand points the first entry value and also the dst local. Second operand contains the number of elements.
    /// Const key indexes follow the size operand.
    map,
    mapEmpty,
    slice,
    /// Pops top register, if value evals to false, jumps the pc forward by an offset.
    jumpNotCond,
    jumpCond,
    /// Jumps the pc by an 16-bit integer offset.
    jump,

    release,
    releaseN,
    callObjSym,
    callObjNativeFuncIC,
    callObjFuncIC,
    callSym,
    callFuncIC,
    callNativeFuncIC,
    ret1,
    ret0,

    /// Calls a lambda and ensures 0 return values.
    /// [calleeLocal] [numArgs]
    call0,

    /// Calls a lambda and ensures 1 return value.
    /// [calleeLocal] [numArgs]
    call1,

    field,
    fieldIC,
    fieldRetain,
    fieldRetainIC,
    fieldRelease,
    lambda,
    closure,
    compare,
    less,
    // lessNumber,
    greater,
    lessEqual,
    greaterEqual,

    /// Multiplies first two locals and stores result to a dst local.
    mul,
    /// Divides second local from first local and stores result to a dst local.
    div,
    /// Raises first local's power to the value of the second local and stores result to a dst local.
    pow,
    /// Perform modulus on the two locals and stores result to a dst local.
    mod,

    compareNot,

    /// [startLocal] [exprCount] [dst] [..string consts]
    stringTemplate,
    neg,
    setInitN,
    objectSmall,
    object,
    setField,
    setFieldRelease,
    setFieldReleaseIC,
    coinit,
    coyield,
    coresume,
    coreturn,
    retain,
    copyRetainRelease,

    /// Lifts a source local to a box object and stores the result in `dstLocal`.
    /// The source local is also retained.
    /// [srcLocal] [dstLocal]
    box,

    setBoxValue,
    setBoxValueRelease,
    boxValue,
    boxValueRetain,
    tag,
    tagLiteral,

    /// Copies a non error value to a local or jumps to end of the function.
    /// [srcLocal] [dstLocal] [jumpOffset: u16]
    tryValue,

    bitwiseAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseNot,
    bitwiseLeftShift,
    bitwiseRightShift,
    jumpNotNone,
    addInt,
    subInt,
    lessInt,
    forRangeInit,
    forRange,
    forRangeReverse,

    /// Performs an eq comparison with a sequence of locals.
    /// The pc then jumps with the offset of the matching local, otherwise the offset from the end is used.
    /// [exprLocal] [numCases] [case1Local] [case1Jump] ... [elseJump]
    match,

    /// Wraps a static function in a function value.
    /// [symId] [dstLocal]
    staticFunc,

    /// Copies and retains a static variable to a destination local.
    /// [symId] [dstLocal]
    staticVar,

    /// Copies a local register to a static variable.
    /// [symId] [local]
    setStaticVar,

    /// Copies a local register to a static function.
    /// [symId] [local]
    setStaticFunc,

    /// Allocates a symbol object to a destination local.
    /// [symType] [symId] [dst]
    sym,

    /// Indicates the end of the main script.
    end,
};

test "Internals." {
    try t.eq(std.enums.values(OpCode).len, 94);
    try t.eq(@sizeOf(OpData), 1);
    try t.eq(@sizeOf(Const), 8);
    try t.eq(@alignOf(Const), 8);
}