const std = @import("std");
const stdx = @import("stdx");
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.bytecode);

/// Holds vm instructions.
pub const ByteCodeBuffer = struct {
    alloc: std.mem.Allocator,
    /// The number of local vars in the main block to reserve space for.
    mainLocalSize: u32,
    ops: std.ArrayListUnmanaged(OpData),
    consts: std.ArrayListUnmanaged(Const),
    /// Contiguous constant strings in a buffer.
    strBuf: std.ArrayListUnmanaged(u8),
    /// Tracks the start index of strings that are already in strBuf.
    strMap: std.HashMapUnmanaged(stdx.IndexSlice(u32), u32, StringIndexContext, std.hash_map.default_max_load_percentage),

    pub fn init(alloc: std.mem.Allocator) ByteCodeBuffer {
        return .{
            .alloc = alloc,
            .mainLocalSize = 0,
            .ops = .{},
            .consts = .{},
            .strBuf = .{},
            .strMap = .{},
        };
    }

    pub fn deinit(self: *ByteCodeBuffer) void {
        self.ops.deinit(self.alloc);
        self.consts.deinit(self.alloc);
        self.strBuf.deinit(self.alloc);
        self.strMap.deinit(self.alloc);
    }

    pub fn clear(self: *ByteCodeBuffer) void {
        self.ops.clearRetainingCapacity();
        self.consts.clearRetainingCapacity();
        self.strBuf.clearRetainingCapacity();
        self.strMap.clearRetainingCapacity();
    }

    pub fn pushConst(self: *ByteCodeBuffer, val: Const) !u32 {
        const start = @intCast(u32, self.consts.items.len);
        try self.consts.resize(self.alloc, self.consts.items.len + 1);
        self.consts.items[start] = val;
        return start;
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

    pub fn setOpArgs1(self: *ByteCodeBuffer, idx: usize, arg: u8) void {
        self.ops.items[idx].arg = arg;
    }

    pub fn pushStringConst(self: *ByteCodeBuffer, str: []const u8) !u32 {
        const slice = try self.getStringConst(str);
        const idx = @intCast(u32, self.consts.items.len);
        const val = cy.Value.initConstStr(slice.start, @intCast(u16, slice.end - slice.start));
        try self.consts.append(self.alloc, .{ .val = val.val });
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

        while (pc < ops.len) {
            const name = @tagName(ops[pc].code);
            try w.print("{} {s} ", .{pc, name});
            switch (ops[pc].code) {
                .pushLess,
                .pushGreater,
                .pushLessEqual,
                .pushGreaterEqual,
                .pushOr,
                .pushAnd,
                .pushMultiply,
                .pushAdd,
                .pushMinus,
                .pushDivide,
                .pushMod,
                .cont,
                // .ret2,
                .ret1,
                .ret0,
                .end,
                .pushCompare,
                .pushNotCompare,
                .pushNot,
                .pushNone,
                .pushIndex,
                .pushReverseIndex,
                .pushMapEmpty,
                .pushSlice,
                .setIndex,
                .pushStringTemplate,
                .pushTrue,
                .pushFalse => {
                    pc += 1;
                },
                .releaseSet,
                .set,
                .addSet,
                .pushList,
                .load,
                .jumpBack,
                .jump,
                .jumpNotCond,
                .release,
                .pushCall0,
                .pushCall1,
                .loadRetain,
                .pushField,
                .pushConst => {
                    try w.print("{}", .{ops[pc+1].arg});
                    pc += 2;
                },
                .pushMap,
                .callObjSym,
                .pushCallSym0,
                .pushCallSym1,
                .forIter,
                .forRange,
                .pushMinus2,
                .pushMinus1 => {
                    try w.print("{} {}", .{ops[pc+1].arg, ops[pc+2].arg});
                    pc += 3;
                },
                .call => {
                    stdx.unsupported();
                },
                .pushLambda => {
                    try w.print("{} {} {}", .{ops[pc+1].arg, ops[pc+2].arg, ops[pc+3].arg});
                    pc += 4;
                },
                .pushClosure => {
                    try w.print("{} {} {} {}", .{ops[pc+1].arg, ops[pc+2].arg, ops[pc+3].arg, ops[pc+4].arg});
                    pc += 5;
                },
                else => {
                    stdx.panicFmt("unsupported {}", .{ops[pc].code});
                },
            }
            _ = try w.write("\n");
        }
        log.info("{s}", .{buf.items});

        for (self.consts.items) |extra| {
            log.info("extra {}", .{extra});
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
};

const ConstStringTag: u2 = 0b00;

pub const OpData = packed union {
    code: OpCode,
    arg: u8,
};

pub const OpCode = enum(u8) {
    /// Push constant value onto register stack.
    pushConst,
    /// Loads a value from address relative to the local frame onto the register stack.
    load,
    /// Pops top two registers, performs addition, and pushes result onto stack.
    pushAdd,
    pushMinus,
    pushMinus1,
    /// Push boolean onto register stack.
    pushTrue,
    pushFalse,
    /// Push none value onto register stack.
    pushNone,
    /// Pops top two registers, performs or, and pushes result onto stack.
    pushOr,
    /// Pops top two registers, performs and, and pushes result onto stack.
    pushAnd,
    /// Pops top register, performs not, and pushes result onto stack.
    pushNot,
    /// Pops top register and copies value to address relative to the local frame.
    set,
    releaseSet,
    /// Pops right, index, left registers, sets right value to address of left[index].
    setIndex,
    loadRetain,
    pushIndex,
    /// Pops specifc number of registers to allocate a new list on the heap. Pointer to new list is pushed onto the stack.
    pushList,
    pushMap,
    pushMapEmpty,
    pushSlice,
    /// Pops top register, if value evals to false, jumps the pc forward by an offset.
    jumpNotCond,
    /// Jumps the pc forward by an offset.
    jump,
    jumpBack,
    cont,

    // releaseMany,
    release,
    /// Like pushCall but does not push the result onto the stack.
    call,
    /// Num args includes the receiver.
    callStr,
    /// Num args includes the receiver.
    callObjSym,
    pushCallSym0,
    pushCallSym1,
    // ret2,
    ret1,
    ret0,
    /// Pops callee and args, performs a function call, and ensures no return values.
    pushCall0,
    /// Pops callee and args, performs a function call, and ensures one return value.
    pushCall1,
    pushField,
    pushLambda,
    pushClosure,
    addSet,
    pushCompare,
    pushLess,
    pushGreater,
    pushLessEqual,
    pushGreaterEqual,
    pushMinus2,
    forRange,
    forIter,
    pushMultiply,
    pushDivide,
    pushPower,
    pushMod,
    pushReverseIndex,
    pushNotCompare,
    pushStringTemplate,

    /// Indicates the end of the main script.
    end,
};

comptime {
    const end = @enumToInt(OpCode.end);
    if (end != 53) {
        @compileError(std.fmt.comptimePrint("Unexpected end op code {}", .{end}));
    }
}