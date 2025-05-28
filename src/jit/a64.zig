const std = @import("std");
const stdx = @import("stdx");
const cy = @import("../cyber.zig");
const t = stdx.testing;

pub const Register = enum(u8) {
    // 64-bit general-purpose registers
    x0, x1, x2, x3, x4, x5, x6, x7,
    x8, x9, x10, x11, x12, x13, x14, x15,
    x16, x17, x18, x19, x20, x21, x22, x23,
    x24, x25, x26, x27, x28, x29, x30, xzr,

    pub fn enc(self: Register) u5 {
        return switch (@intFromEnum(self)) {
            @intFromEnum(Register.x0)...@intFromEnum(Register.xzr) => @as(u5, @intCast(@intFromEnum(self) - @intFromEnum(Register.x0))),
            else => unreachable,
        };
    }

    pub fn bitSize(self: Register) u8 {
        return switch (@intFromEnum(self)) {
            @intFromEnum(Register.x0)...@intFromEnum(Register.xzr) => 64,
            else => unreachable,
        };
    }

    pub fn encBitSize(self: Register) u1 {
        return switch (self.bitSize()) {
            32 => 0,
            64 => 1,
            else => unreachable,
        };
    }
};

pub const NoOp = packed struct {
    fixed: u32 = 0b1101010100_0_00_011_0010_0000_000_11111,

    pub fn bitCast(self: NoOp) u32 {
        return @bitCast(self);
    }

    pub fn init() NoOp {
        return NoOp{};
    }
};

pub const PcRelAddr = packed struct {
    rd: u5,
    immhi: u19,
    fixed: u5 = 0b10000,
    immlo: u2,
    op: u1,

    pub fn bitCast(self: PcRelAddr) u32 {
        return @bitCast(self);
    }

    pub fn adrFrom(rd: Register, from: usize, to: usize) PcRelAddr {
        const offset: i21 = @intCast(@as(isize, @bitCast(to -% from)));
        return adr(rd, offset);
    }

    pub fn adr(rd: Register, imm21: i21) PcRelAddr {
        std.debug.assert(rd.bitSize() == 64);
        const imm21_u = @as(u21, @bitCast(imm21));
        return PcRelAddr{
            .rd = rd.enc(),
            .immlo = @as(u2, @truncate(imm21_u)),
            .immhi = @as(u19, @truncate(imm21_u >> 2)),
            .op = 0b0,
        };
    }

    pub fn setOffsetFrom(self: *PcRelAddr, from: usize, to: usize) void {
        const offset: i21 = @intCast(@as(isize, @bitCast(to -% from)));
        const imm21_u: u21 = @bitCast(offset);
        self.immlo = @truncate(imm21_u);
        self.immhi = @truncate(imm21_u >> 2);
    }
};

pub const Cond = enum(u4) {
    eq = 0,
    ne = 1,
    ge = 10,
    lt = 11,
};

pub const AddSubShifted = packed struct {
    rd: u5,
    rn: u5,
    imm6: u6,
    rm: u5,
    fixed_1: u1 = 0b0,
    shift: u2,
    fixed_2: u5 = 0b01011,
    s: u1,
    op: u1,
    sf: u1,

    pub fn bitCast(self: AddSubShifted) u32 {
        return @bitCast(self);
    }

    pub fn cmp(rn: Register, rm: Register) AddSubShifted {
        return .{
            .s = 0b1,
            .op = 0b1,
            .rd = Register.xzr.enc(),
            .rn = rn.enc(),
            .imm6 = 0,
            .rm = rm.enc(),
            .shift = 0b00, // lsl
            .sf = Register.xzr.encBitSize(),
        };
    }
};

pub const AddSubImm = packed struct {
    rd: u5,
    rn: u5,
    imm12: u12,
    sh: u1,
    fixed: u6 = 0b100010,
    s: u1,
    op: u1,
    sf: u1,

    pub fn bitCast(self: AddSubImm) u32 {
        return @bitCast(self);
    }

    pub fn add(rd: Register, rn: Register, imm: u12) AddSubImm {
        return .{
            .rd = rd.enc(),
            .rn = rn.enc(),
            .imm12 = imm,
            .sh = 0,
            .s = 0b0,
            .op = 0b0,
            .sf = rd.encBitSize(),
        };
    }
};

pub const BrImm = packed struct {
    imm26: u26,
    fixed: u5 = 0b00101,
    op: u1,

    pub fn bitCast(self: BrImm) u32 {
        return @bitCast(self);
    }

    pub fn setOffset(self: *BrImm, offset: i28) void {
        self.imm26 = @bitCast(@as(i26, @intCast(offset >> 2)));
    }

    pub fn setOffsetFrom(self: *BrImm, from: usize, to: usize) void {
        const offset: i26 = @intCast(@as(isize, @bitCast(to -% from)) >> 2);
        self.imm26 = @bitCast(offset);
    }

    pub fn b(offset: i28) BrImm {
        const imm: u26 = @bitCast(@as(i26, @intCast(offset >> 2)));
        return .{ .imm26 = imm, .op = 0 };
    }

    pub fn bl(offset: i28) BrImm {
        const imm: u26 = @bitCast(@as(i26, @intCast(offset >> 2)));
        return .{ .imm26 = imm, .op = 1 };
    }
};

pub const Br = packed struct {
    op4: u5,
    rn: u5,
    op3: u6,
    op2: u5,
    opc: u4,
    fixed: u7 = 0b1101_011,

    pub fn bitCast(self: Br) u32 {
        return @bitCast(self);
    }

    pub fn ret() Br {
        return .{
            .opc = 0b0010,
            .op2 = 0b11111,
            .op3 = 0b000000,
            .rn = Register.x30.enc(),
            .op4 = 0b0000,
        };
    }

    pub fn br(rn: Register) Br {
        return .{
            .opc = 0b0000,
            .op2 = 0b11111,
            .op3 = 0b000000,
            .rn = rn.enc(),
            .op4 = 0b0000,
        };
    }

    pub fn blr(rn: Register) Br {
        return .{
            .opc = 0b0001,
            .op2 = 0b11111,
            .op3 = 0b000000,
            .rn = rn.enc(),
            .op4 = 0b0000,
        };
    }
};

pub const BrCond = packed struct {
    cond: u4,
    o0: u1 = 0,
    imm19: u19,
    o1: u1 = 0,
    fixed: u7 = 0b0101010,

    pub fn bitCast(self: BrCond) u32 {
        return @bitCast(self);
    }

    pub fn init(cond: Cond, imm: i19) BrCond {
        return .{ .cond = @intFromEnum(cond), .imm19 = @bitCast(imm) };
    }
};

pub const MovWideImm = packed struct {
    rd: u5,
    imm16: u16,
    hw: u2,
    fixed: u6 = 0b100101,
    opc: u2,
    sf: u1,

    pub fn setShift(self: *MovWideImm, shift: u6) void {
        self.hw = @as(u2, @intCast(shift / 16));
    }

    pub fn movn(dst: Register, imm: u16, shift: u6) MovWideImm {
        return .{
            .rd = dst.enc(),
            .imm16 = imm,
            .hw = @as(u2, @intCast(shift / 16)),
            .opc = 0b00,
            .sf = dst.encBitSize(),
        };
    }

    pub fn movz(dst: Register, imm: u16, shift: u6) MovWideImm {
        return .{
            .rd = dst.enc(),
            .imm16 = imm,
            .hw = @as(u2, @intCast(shift / 16)),
            .opc = 0b10,
            .sf = dst.encBitSize(),
        };
    }

    pub fn movk(dst: Register, imm: u16, shift: u6) MovWideImm {
        return .{
            .rd = dst.enc(),
            .imm16 = imm,
            .hw = @as(u2, @intCast(shift / 16)),
            .opc = 0b11,
            .sf = dst.encBitSize(),
        };
    }
};

pub const LogicalImm = packed struct {
    rd: u5,
    rn: u5,
    imms: u6,
    immr: u6,
    n: u1,
    fixed: u6 = 0b100100,
    opc: u2,
    sf: u1,

    pub fn andImm(rd: Register, rn: Register, imms: u6, immr: u6, n: u1) LogicalImm {
        return .{
            .rd = rd.enc(),
            .rn = rn.enc(),
            .imms = imms,
            .immr = immr,
            .n = n,
            .opc = 0b00,
            .sf = rd.encBitSize(),
        };
    }

    pub fn orrImm(rd: Register, rn: Register, imms: u6, immr: u6, n: u1) LogicalImm {
        return .{
            .rd = rd.enc(),
            .rn = rn.enc(),
            .imms = imms,
            .immr = immr,
            .n = n,
            .opc = 0b01,
            .sf = rd.encBitSize(),
        };
    }

    pub fn eorImm(rd: Register, rn: Register, imms: u6, immr: u6, n: u1) LogicalImm {
        return .{
            .rd = rd.enc(),
            .rn = rn.enc(),
            .imms = imms,
            .immr = immr,
            .n = n,
            .opc = 0b10,
            .sf = rd.encBitSize(),
        };
    }

    pub fn andsImm(rd: Register, rn: Register, imms: u6, immr: u6, n: u1) LogicalImm {
        return .{
            .rd = rd.enc(),
            .rn = rn.enc(),
            .imms = imms,
            .immr = immr,
            .n = n,
            .opc = 0b11,
            .sf = rd.encBitSize(),
        };
    }
};

pub const LoadStore = packed struct {
    rt: u5,
    rn: u5,
    offset: u12,
    opc: u2,
    op1: u2,
    v: u1,
    fixed: u3 = 0b111,
    size: u2,

    pub fn bitCast(self: LoadStore) u32 {
        return @bitCast(self);
    }

    pub fn strImmOff(rn: Register, off: u12, rt: Register) LoadStore {
        return .{
            .rt = rt.enc(),
            .rn = rn.enc(),
            .v = 0,
            .size = if (rt.bitSize() == 64) 0b11 else 0b10,
            .offset = off,
            .opc = 0b00,
            .op1 = 0b01,
        };
    }

    pub fn ldrImmOff(rn: Register, off: u12, rt: Register) LoadStore {
        return .{
            .rt = rt.enc(),
            .rn = rn.enc(),
            .v = 0,
            .size = if (rt.bitSize() == 64) 0b11 else 0b10,
            .offset = off,
            .opc = 0b01,
            .op1 = 0b01,
        };
    }
};

pub const Exception = packed struct {
    ll: u2,
    op2: u3,
    imm16: u16,
    opc: u3,
    fixed: u8 = 0b1101_0100,

    pub fn bitCast(self: Exception) u32 {
        return @bitCast(self);
    }

    pub fn brk(imm16: u16) Exception {
        return .{
            .ll = 0b00,
            .op2 = 0b000,
            .imm16 = imm16,
            .opc = 0b001,
        };
    }
};

test "A64 insts" {
    const inst = PcRelAddr.adr(.x2, 0x8).bitCast();
    try t.eq(inst, 0b0_00_10000_0000000000000000010_00010);
}
