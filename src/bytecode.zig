const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.bytecode);

/// Holds vm instructions.
pub const ByteCodeBuffer = struct {
    alloc: std.mem.Allocator,
    /// The number of local vars in the main block to reserve space for.
    mainLocalSize: u32,
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
            .mainLocalSize = 0,
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
        const val = cy.Value.initConstStr(slice.start, @intCast(u16, slice.end - slice.start));
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

    pub fn dump(self: ByteCodeBuffer) !void {
        var pc: usize = 0;
        const ops = self.ops.items;

        var buf = std.ArrayList(u8).init(self.alloc);
        defer buf.deinit();
        const w = buf.writer();
            
        try w.print("Bytecode:\n", .{});

        while (pc < ops.len) {
            const name = @tagName(ops[pc].code);
            try w.print("{} {s} ", .{pc, name});
            switch (ops[pc].code) {
                .minus,
                .mul,
                .div,
                .pow,
                .mod,
                .less,
                .compare,
                .compareNot,
                .list,
                .add => {
                    try w.print("{} {} {}", .{ops[pc+1].arg, ops[pc+2].arg, ops[pc+3].arg});
                    pc += 4;
                },
                .greater,
                .lessEqual,
                .greaterEqual,
                // .ret2,
                .ret0,
                .ret1,
                .coreturn => {
                    pc += 1;
                },
                .setIndex,
                .copy,
                .not,
                .neg,
                .copyRetainSrc,
                .copyReleaseDst,
                .constI8,
                .call0,
                .call1,
                .constOp => {
                    try w.print("{} {}", .{ops[pc+1].arg, ops[pc+2].arg});
                    pc += 3;
                },
                .setField,
                .retain,
                .end,
                .release,
                .none,
                .true,
                .false,
                .mapEmpty => {
                    try w.print("{}", .{ops[pc+1].arg});
                    pc += 2;
                },
                .indexRetain,
                .reverseIndexRetain,
                .setFieldRelease,
                .fieldRetain,
                .fieldRetainRelease,
                .field,
                .callSym0,
                .callSym1,
                .callObjSym0,
                .callObjSym1,
                .stringTemplate,
                // .jumpCondNone,
                .jumpCond,
                .jumpNotCond => {
                    try w.print("{} {} {}", .{ops[pc+1].arg, ops[pc+2].arg, ops[pc+3].arg});
                    pc += 4;
                },
                .cont,
                .jumpBack,
                .jump,
                .pushCostart,
                .coyield => {
                    try w.print("{} {}", .{ops[pc+1].arg, ops[pc+2].arg});
                    pc += 3;
                },
                .slice,
                .lambda => {
                    try w.print("{} {} {} {}", .{ops[pc+1].arg, ops[pc+2].arg, ops[pc+3].arg, ops[pc+4].arg});
                    pc += 5;
                },
                .closure => {
                    try w.print("{} {} {} {}", .{ops[pc+1].arg, ops[pc+2].arg, ops[pc+3].arg, ops[pc+4].arg});
                    pc += 7;
                },
                .forIter => {
                    try w.print("{} {} {} {} {}", .{ops[pc+1].arg, ops[pc+2].arg, ops[pc+3].arg, ops[pc+4].arg, ops[pc+5].arg});
                    pc += 6;
                },
                .map => {
                    const startLocal = ops[pc+1].arg;
                    const numEntries = ops[pc+2].arg;
                    const dst = ops[pc+3].arg;
                    try w.print("{} {} {}", .{startLocal, numEntries, dst});
                    pc += 4 + numEntries;
                },
                .structSmall => {
                    _ = ops[pc+1].arg;
                    const numEntries = ops[pc+3].arg;
                    try w.print("{}", .{numEntries});
                    pc += 5 + numEntries;
                },
                .setInitN => {
                    const numVars = ops[pc+1].arg;
                    try w.print("{}", .{numVars});
                    pc += 2 + numVars;
                },
                .forRange => {
                    try w.print("{} {} {} {} {} {}", .{ops[pc+1].arg, ops[pc+2].arg, ops[pc+3].arg, ops[pc+4].arg, ops[pc+5].arg, ops[pc+6].arg});
                    pc += 7;
                },
                else => {
                    stdx.panicFmt("unsupported {}", .{ops[pc].code});
                },
            }
            _ = try w.write("\n");
        }
        if (builtin.is_test) {
            log.info("{s}", .{buf.items});
        } else {
            std.debug.print("{s}\n", .{buf.items});
        }

        buf.clearRetainingCapacity();
        for (self.mconsts) |extra| {
            const ww = buf.writer();
            const val = cy.Value{ .val = extra.val };
            if (val.isNumber()) {
                try ww.print("{}\n", .{val.asF64()});
            } else {
                try w.print("{}\n", .{extra});
            }
        }
        if (builtin.is_test) {
            log.info("Constants:\n{s}", .{buf.items});
        } else {
            std.debug.print("Constants:\n{s}", .{buf.items});
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

pub const OpCode = enum(u8) {
    /// Copies a constant value from `consts` to a dst local.
    constOp,
    /// Sets an immediate i8 value as a number to a dst local.
    constI8,
    /// Add first two locals and stores result to a dst local.
    add,
    // addNumber,
    /// Subtracts second local from first local and stores result to a dst local.
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
    cont,

    // releaseMany,
    release,
    /// Num args includes the receiver.
    callObjSym0,
    callObjSym1,
    callSym0,
    callSym1,
    // ret2,
    ret1,
    ret0,
    /// Calls a lambda and ensures 0 return values.
    call0,
    /// Calls a lambda and ensures 1 return value.
    call1,
    field,
    lambda,
    closure,
    compare,
    less,
    // lessNumber,
    greater,
    lessEqual,
    greaterEqual,
    forRange,
    forIter,

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
    stringTemplate,
    neg,
    setInitN,
    structSmall,
    setField,
    setFieldRelease,
    bitwiseAnd,
    fieldRetain,
    fieldRetainRelease,
    fieldRelease,
    pushCostart,
    coyield,
    coreturn,
    retain,
    copyRetainRelease,
    // jumpCondNone,

    /// Indicates the end of the main script.
    end,
};

test "Internals." {
    try t.eq(@enumToInt(OpCode.end), 64);
    try t.eq(@sizeOf(OpData), 1);
    try t.eq(@sizeOf(Const), 8);
    try t.eq(@alignOf(Const), 8);
}