const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.bytecode);

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

    /// Maps ops back to source code.
    /// The end pc of an instruction is mapped since the interpreter prefers
    /// to advance the pc right after reading the opcode and operands.
    debugTable: std.ArrayListUnmanaged(OpDebug),

    pub fn init(alloc: std.mem.Allocator) !ByteCodeBuffer {
        var new = ByteCodeBuffer{
            .alloc = alloc,
            .mainStackSize = 0,
            .ops = .{},
            .consts = .{},
            .strBuf = .{},
            .strMap = .{},
            .debugTable = .{},
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
    }

    pub fn clear(self: *ByteCodeBuffer) void {
        self.ops.clearRetainingCapacity();
        self.consts.clearRetainingCapacity();
        self.strBuf.clearRetainingCapacity();
        self.strMap.clearRetainingCapacity();
        self.debugTable.clearRetainingCapacity();
    }

    pub fn pushConst(self: *ByteCodeBuffer, val: Const) !u32 {
        const start = @intCast(u32, self.consts.items.len);
        try self.consts.resize(self.alloc, self.consts.items.len + 1);
        self.consts.items[start] = val;
        return start;
    }

    pub fn pushDebugSym(self: *ByteCodeBuffer, pc: usize, file: u16, loc: u32, frameLoc: u32) !void {
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
        for (args) |arg, i| {
            self.ops.items[start+i] = .{ .arg = arg };
        }
    }
    
    pub fn pushOpSlice(self: *ByteCodeBuffer, code: OpCode, args: []const u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + args.len + 1);
        self.ops.items[start] = .{ .code = code };
        for (args) |arg, i| {
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

    pub fn pushStringConst(self: *ByteCodeBuffer, str: []const u8) !u32 {
        const slice = try self.getStringConst(str);
        const idx = @intCast(u32, self.consts.items.len);
        const val = cy.Value.initConstStr(slice.start, @intCast(u15, slice.end - slice.start));
        try self.consts.append(self.alloc, Const.init(val.val));
        return idx;
    }

    pub fn getStringConst(self: *ByteCodeBuffer, str: []const u8) !stdx.IndexSlice(u32) {
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

    pub fn getStringConstValue(self: *ByteCodeBuffer, str: []const u8) !cy.Value {
        const slice = try self.getStringConst(str);
        return cy.Value.initConstStr(slice.start, @intCast(u15, slice.end - slice.start));
    }

    fn println(comptime fmt: []const u8, args: anytype) void {
        if (builtin.is_test) {
            log.info(fmt, args);
        } else {
            std.debug.print(fmt ++ "\n", args);
        }
    }

    pub fn dump(self: ByteCodeBuffer) !void {
        var pc: usize = 0;
        const ops = self.ops.items;

        println("Bytecode:", .{});
        while (pc < ops.len) {
            const name = @tagName(ops[pc].code);
            const len = getInstLenAt(self.ops.items.ptr + pc);
            switch (ops[pc].code) {
                .jumpNotCond => {
                    const jump = @ptrCast(*const align(1) u16, &ops[pc + 1]).*;
                    println("{} {s} offset={}, cond={}", .{pc, name, jump, ops[pc + 3].arg});
                    pc += len;
                },
                else => {
                    println("{} {s} {any}", .{pc, name, std.mem.sliceAsBytes(ops[pc+1..pc+len])});
                    pc += len;
                },
            }
        }

        println("\nConstants:", .{});
        for (self.mconsts) |extra| {
            const val = cy.Value{ .val = extra.val };
            if (val.isNumber()) {
                println("{}", .{val.asF64()});
            } else {
                println("{}", .{extra});
            }
        }
    }
};

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

const ConstStringTag: u2 = 0b00;

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

pub const OpDebug = struct {
    pc: u32,
    loc: u32,
    frameLoc: u32,
    file: u16,
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
        .setIndex,
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
        .jumpBack,
        .jump,
        .coyield,
        .coresume,
        .box,
        .setBoxValue,
        .setBoxValueRelease,
        .boxValue,
        .boxValueRetain,
        .tagLiteral,
        .tryValue,
        .varSym,
        .constOp => {
            return 3;
        },
        .setCapValToFuncSyms => {
            const numFuncSyms = pc[2].arg;
            return 3 + numFuncSyms * 2;
        },
        .indexRetain,
        .reverseIndexRetain,
        .index,
        .reverseIndex,
        .jumpNotNone,
        .jumpCond,
        .minus,
        .minusInt,
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
        .lambda => {
            return 5;
        },
        .coinit => {
            return 6;
        },
        .closure => {
            const numCaptured = pc[3].arg;
            return 6 + numCaptured;
        },
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
    /// TODO: Rename to sub.
    minus,
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
    /// Pops right, index, left registers, sets right value to address of left[index].
    setIndex,
    copyRetainSrc,
    index,
    indexRetain,
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
    /// Jumps the pc forward by an offset.
    jump,
    jumpBack,

    // releaseMany,
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
    call0,
    /// Calls a lambda and ensures 1 return value.
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

    reverseIndex,
    reverseIndexRetain,
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
    box,
    setBoxValue,
    setBoxValueRelease,
    boxValue,
    boxValueRetain,
    setCapValToFuncSyms,
    tag,
    tagLiteral,
    tryValue,
    bitwiseAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseNot,
    bitwiseLeftShift,
    bitwiseRightShift,
    jumpNotNone,
    addInt,
    minusInt,
    lessInt,
    varSym,
    forRangeInit,
    forRange,
    forRangeReverse,

    /// Indicates the end of the main script.
    end,
};

test "Internals." {
    try t.eq(std.enums.values(OpCode).len, 92);
    try t.eq(@sizeOf(OpData), 1);
    try t.eq(@sizeOf(Const), 8);
    try t.eq(@alignOf(Const), 8);
}