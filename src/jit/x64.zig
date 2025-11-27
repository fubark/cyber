const std = @import("std");
const stdx = @import("stdx");
const gen = @import("gen.zig");
const t = stdx.testing;

/// Based on: https://github.com/kubkon/zig-dis-x86_64
pub const jg: u8 = 0x8f;
pub const jge: u8 = 0x8d;
pub const jl: u8 = 0x8c;
pub const jle: u8 = 0x8e;

pub const Register = enum(u8) {
    // 64-bit general-purpose registers.
    rax,
    rcx,
    rdx,
    rbx,
    rsp,
    rbp,
    rsi,
    rdi,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,

    pub fn enc(self: Register) u4 {
        return switch (@intFromEnum(self)) {
            @intFromEnum(Register.rax)...@intFromEnum(Register.r15) => {
                const base = @intFromEnum(Register.rax);
                return @truncate(@intFromEnum(self) - base);
            },
            else => unreachable,
        };
    }

    fn lowEnc(self: Register) u3 {
        return @truncate(self.enc());
    }

    pub fn bitSize(self: Register) u8 {
        return switch (@intFromEnum(self)) {
            @intFromEnum(Register.rax)...@intFromEnum(Register.r15) => 64,
            else => unreachable,
        };
    }

    fn isExtended(self: Register) bool {
        return switch (@intFromEnum(self)) {
            @intFromEnum(Register.r8)...@intFromEnum(Register.r15) => true,
            else => false,
        };
    }

    // pub fn encBitSize(self: Register) u1 {
    //     return switch (self.bitSize()) {
    //         32 => 0,
    //         64 => 1,
    //         else => unreachable,
    //     };
    // }
};

pub const Encoder = struct {
    alloc: std.mem.Allocator,
    buf: *std.ArrayListAlignedUnmanaged(u8, std.mem.Alignment.fromByteUnits(std.heap.page_size_min)),

    fn ensureUnusedCap(self: Encoder, size: usize) !void {
        _ = try gen.ensureUnusedCap(self.buf, self.alloc, size);
    }

    /// TODO: Support rel8.
    pub fn jumpCond(self: Encoder, code: u8, offset: i32) !void {
        try self.ensureUnusedCap(6);
        const i = self.buf.items.len;
        self.buf.items.ptr[i] = 0x0f;
        self.buf.items.ptr[i + 1] = code;

        @memcpy(self.buf.items.ptr[i + 2 .. i + 2 + 4], std.mem.asBytes(&offset));
        self.buf.items.len += 6;
    }

    /// TODO: Support rel8.
    pub fn jumpRel(self: Encoder, offset: i32) !void {
        try self.ensureUnusedCap(5);
        const i = self.buf.items.len;
        self.buf.items.ptr[i] = 0xe9;

        @memcpy(self.buf.items.ptr[i + 1 .. i + 1 + 4], std.mem.asBytes(&offset));
        self.buf.items.len += 5;
    }

    pub fn jumpReg(self: Encoder, reg: Register) !void {
        const enc = Encoding.init(.m, 4, .none, .none);
        const out = try self.prepInstBuf();
        var len = self.encodeHeader(out, enc, &.{0xff}, &.{Op.reg(reg)});
        self.encodeMOp(out, &len, enc, Op.reg(reg));
        self.buf.items.len += len;
    }

    pub fn cmp(self: Encoder, left: Register, right: Register) !void {
        const enc = Encoding.init(.mr, 0, .long, .none);
        const out = try self.prepInstBuf();
        var len = self.encodeHeader(out, enc, &.{0x3b}, &.{ Op.reg(left), Op.reg(right) });
        self.encodeRMOps(out, &len, enc, Op.reg(left), Op.reg(right));
        self.buf.items.len += len;
    }

    pub fn lea(self: Encoder, dst: Register, src: Memory) !void {
        const enc = Encoding.init(.rm, 0, .long, .none);
        try self.encode(enc, &.{0x8d}, &.{ Op.reg(dst), Op.mem(src) });
    }

    pub fn pushReg(self: Encoder, r: Register) !void {
        const enc = Encoding.init(.o, 0, .none, .none);
        const out = try self.prepInstBuf();
        const len = self.encodeHeader(out, enc, &.{0x50}, &.{Op.reg(r)});
        self.buf.items.len += len;
    }

    pub fn movImm(self: Encoder, dst: Register, imm: u64) !void {
        const enc = Encoding.init(.oi, 0, .long, .none);
        const out = try self.prepInstBuf();
        var len = self.encodeHeader(out, enc, &.{0xb8}, &.{ Op.reg(dst), Op.imm(imm) });
        encodeImm64(out, &len, imm);
        self.buf.items.len += len;
    }

    pub fn movReg(self: Encoder, dst: Register, src: Register) !void {
        const enc = Encoding.init(.rm, 0, .long, .none);
        const out = try self.prepInstBuf();
        var len = self.encodeHeader(out, enc, &.{0x8b}, &.{ Op.reg(dst), Op.reg(src) });
        self.encodeRMOps(out, &len, enc, Op.reg(dst), Op.reg(src));
        self.buf.items.len += len;
    }

    pub fn movMem(self: Encoder, dst: Register, mem: Memory) !void {
        const enc = Encoding.init(.rm, 0, .long, .none);
        const out = try self.prepInstBuf();
        var len = self.encodeHeader(out, enc, &.{0x8b}, &.{ Op.reg(dst), Op.mem(mem) });
        self.encodeRMOps(out, &len, enc, Op.reg(dst), Op.mem(mem));
        self.buf.items.len += len;
    }

    pub fn movToMem(self: Encoder, mem: Memory, dst: Register) !void {
        const enc = Encoding.init(.mr, 0, .long, .none);
        const out = try self.prepInstBuf();
        var len = self.encodeHeader(out, enc, &.{0x89}, &.{ Op.mem(mem), Op.reg(dst) });
        self.encodeRMOps(out, &len, enc, Op.reg(dst), Op.mem(mem));
        self.buf.items.len += len;
    }

    pub fn callRel(self: Encoder, offset: i32) !void {
        try self.ensureUnusedCap(5);
        const i = self.buf.items.len;
        self.buf.items.ptr[i] = 0xe8;

        @memcpy(self.buf.items.ptr[i + 1 .. i + 1 + 4], std.mem.asBytes(&offset));
        self.buf.items.len += 5;
    }

    pub fn callReg(self: Encoder, reg: Register) !void {
        const enc = Encoding.init(.m, 2, .none, .none);
        const out = try self.prepInstBuf();
        var len = self.encodeHeader(out, enc, &.{0xff}, &.{Op.reg(reg)});
        self.encodeMOp(out, &len, enc, Op.reg(reg));
        self.buf.items.len += len;
    }

    pub fn int3(self: Encoder) !void {
        try self.ensureUnusedCap(1);
        self.buf.items.ptr[self.buf.items.len] = 0xcc;
        self.buf.items.len += 1;
    }

    pub fn ret(self: Encoder) !void {
        try self.ensureUnusedCap(1);
        self.buf.items.ptr[self.buf.items.len] = 0xc3;
        self.buf.items.len += 1;
    }

    fn prepInstBuf(self: Encoder) ![]u8 {
        try self.ensureUnusedCap(32);
        return self.buf.items.ptr[self.buf.items.len .. self.buf.items.len + 32];
    }

    fn encodeHeader(_: Encoder, out: []u8, enc: Encoding, opc: []const u8, ops: []const Op) usize {
        // const prefix = Prefix.none;
        var len: usize = 0;

        var hasMandatoryPrefix = false;
        if (mandatoryPrefix(opc)) |byte| {
            out[len] = byte;
            len += 1;
            hasMandatoryPrefix = true;
        }

        encodeRexPrefix(out, &len, enc, ops);

        // Encode opcode.
        const first = @intFromBool(hasMandatoryPrefix);
        const final = opc.len - 1;
        const finalOpc = opc[first..final];
        if (finalOpc.len > 0) {
            @memcpy(out[len .. len + finalOpc.len], finalOpc);
            len += finalOpc.len;
        }
        if (enc.opEn == .oi or enc.opEn == .o) {
            // First assign to var to get around miscompilation.
            out[len] = opc[final] | ops[0].data.reg.lowEnc();
            len += 1;
        } else {
            out[len] = opc[final];
            len += 1;
        }
        return len;
    }

    fn encodeMOp(_: Encoder, out: []u8, len: *usize, enc: Encoding, op: Op) void {
        const rm = enc.modRmExt();
        switch (op.type) {
            .reg => {
                out[len.*] = ModRM.direct(rm, op.data.reg.lowEnc());
                len.* += 1;
            },
            .mem => |_| {
                unreachable;
            },
            else => unreachable,
        }
    }

    fn encodeRMOps(_: Encoder, out: []u8, len: *usize, enc: Encoding, r: Op, m: Op) void {
        switch (m.type) {
            .reg => {
                const rm = r.data.reg.lowEnc();
                out[len.*] = ModRM.direct(rm, m.data.reg.lowEnc());
                len.* += 1;
            },
            .mem => {
                encodeMemory(out, len, enc, m.data.mem, r);
            },
            else => unreachable,
        }
    }

    fn encode(self: Encoder, enc: Encoding, opc: []const u8, ops: []const Op) !void {
        var out = try self.prepInstBuf();
        var len = self.encodeHeader(out, enc, opc, ops);

        // Encode operands.
        switch (enc.opEn) {
            .rm, .rmi => {
                const memop = ops[1];
                switch (memop.type) {
                    .reg => {
                        const rm = switch (enc.opEn) {
                            .rm, .rmi => ops[0].data.reg.lowEnc(),
                            else => unreachable,
                        };
                        out[len] = ModRM.direct(rm, memop.data.reg.lowEnc());
                        len += 1;
                    },
                    .mem => {
                        const op = ops[0];
                        encodeMemory(out, &len, enc, memop.data.mem, op);
                    },
                    else => return error.Unexpected,
                }

                switch (enc.opEn) {
                    // .rmi => encodeImm(out, &len, ops[2].imm, ops[2].immBitSize()),
                    else => {},
                }
            },
            else => return error.Unexpected,
        }

        // Increase buffer length by inst len.
        self.buf.items.len += len;
    }
};

const SIBByte = packed struct {
    base: u3,
    index: u3,
    scale: u2,

    fn disp32() u8 {
        return @bitCast(SIBByte{ .base = 5, .index = 4, .scale = 0 });
    }

    fn baseDisp8(base: u3) u8 {
        return @bitCast(SIBByte{ .base = base, .index = 4, .scale = 0 });
    }

    fn initBase(base: u3) u8 {
        return @bitCast(SIBByte{ .base = base, .index = 4, .scale = 0 });
    }
};

const ModRM = packed struct {
    rm: u3,
    regOrOpx: u3,
    mod: u2,

    fn direct(regOrOpx: u3, rm: u3) u8 {
        return @bitCast(ModRM{ .rm = rm, .regOrOpx = regOrOpx, .mod = 0b11 });
    }

    fn sibDisp0(regOrOpx: u3) u8 {
        return @bitCast(ModRM{ .rm = 0b100, .regOrOpx = regOrOpx, .mod = 0b00 });
    }

    fn sibDisp8(regOrOpx: u3) u8 {
        return @bitCast(ModRM{ .rm = 0b100, .regOrOpx = regOrOpx, .mod = 0b01 });
    }

    fn indirectDisp0(regOrOpx: u3, rm: u3) u8 {
        return @bitCast(ModRM{ .rm = rm, .regOrOpx = regOrOpx, .mod = 0b00 });
    }

    fn indirectDisp8(regOrOpx: u3, rm: u3) u8 {
        return @bitCast(ModRM{ .rm = rm, .regOrOpx = regOrOpx, .mod = 0b01 });
    }

    fn indirectDisp32(regOrOpx: u3, rm: u3) u8 {
        return @bitCast(ModRM{ .rm = rm, .regOrOpx = regOrOpx, .mod = 0b10 });
    }

    fn ripDisp32(regOrOpx: u3) u8 {
        return @bitCast(ModRM{ .rm = 0b101, .regOrOpx = regOrOpx, .mod = 0b00 });
    }
};

const Rex = struct {
    w: bool = false,
    r: bool = false,
    x: bool = false,
    b: bool = false,
    present: bool = false,

    fn isSet(self: Rex) bool {
        return self.w or self.r or self.x or self.b;
    }
};

fn encodeImm64(buf: []u8, len: *usize, imm: u64) void {
    @memcpy(buf[len.* .. len.* + 8], std.mem.asBytes(&imm));
    len.* += 8;
}

fn encodeMemory(buf: []u8, len: *usize, enc: Encoding, mem: Memory, op: Op) void {
    const opEnc: u3 = switch (op.type) {
        .reg => op.data.reg.lowEnc(),
        .none => enc.modRmExt(),
        else => unreachable,
    };

    switch (mem) {
        .sib => |sib| {
            switch (sib.base) {
                .none => {
                    buf[len.*] = ModRM.sibDisp0(opEnc);
                    len.* += 1;

                    if (sib.scaleIndex.scale > 0) {
                        unreachable;
                    } else {
                        buf[len.*] = SIBByte.disp32();
                        len.* += 1;
                    }

                    @memcpy(buf[len.* .. len.* + 4], std.mem.asBytes(&sib.disp));
                    len.* += 4;
                },
                .reg => |base| {
                    if (false) {
                        // TODO: base.class() == .segment
                    } else {
                        const baseEnc = base.lowEnc();
                        if (baseEnc == 4 or sib.scaleIndex.scale > 0) {
                            if (sib.disp == 0 and baseEnc != 5) {
                                buf[len.*] = ModRM.sibDisp0(opEnc);
                                len.* += 1;
                                if (sib.scaleIndex.scale > 0) {
                                    unreachable;
                                } else {
                                    buf[len.*] = SIBByte.initBase(baseEnc);
                                    len.* += 1;
                                }
                            } else if (std.math.cast(i8, sib.disp)) |disp| {
                                buf[len.*] = ModRM.sibDisp8(opEnc);
                                len.* += 1;

                                if (sib.scaleIndex.scale > 0) {
                                    unreachable;
                                } else {
                                    buf[len.*] = SIBByte.baseDisp8(baseEnc);
                                    len.* += 1;
                                }

                                buf[len.*] = @bitCast(disp);
                                len.* += 1;
                            } else {
                                unreachable;
                            }
                        } else {
                            if (sib.disp == 0 and baseEnc != 5) {
                                buf[len.*] = ModRM.indirectDisp0(opEnc, baseEnc);
                                len.* += 1;
                            } else if (std.math.cast(i8, sib.disp)) |disp| {
                                buf[len.*] = ModRM.indirectDisp8(opEnc, baseEnc);
                                len.* += 1;

                                buf[len.*] = @bitCast(disp);
                                len.* += 1;
                            } else {
                                buf[len.*] = ModRM.indirectDisp32(opEnc, baseEnc);
                                len.* += 1;

                                @memcpy(buf[len.* .. len.* + 4], std.mem.asBytes(&sib.disp));
                                len.* += 4;
                            }
                        }
                    }
                },
                else => unreachable,
            }
        },
        .rip => |rip| {
            buf[len.*] = ModRM.ripDisp32(opEnc);
            len.* += 1;

            @memcpy(buf[len.* .. len.* + 4], std.mem.asBytes(&rip));
            len.* += 4;
        },
        else => unreachable,
    }
}

fn encodeRexPrefix(buf: []u8, len: *usize, enc: Encoding, ops: []const Op) void {
    var rex = Rex{
        .present = enc.mode == .rex,
        .w = enc.mode == .long,
    };

    switch (enc.opEn) {
        .o, .oi => {
            rex.b = ops[0].data.reg.isExtended();
        },
        .m, .mr, .rm, .rmi => {
            const rop = switch (enc.opEn) {
                .mr => ops[1],
                .m, .rm, .rmi => ops[0],
                else => unreachable,
            };
            rex.r = rop.isBaseExtended();
            const bxop = switch (enc.opEn) {
                .m, .mr => ops[0],
                else => ops[1],
            };
            rex.b = bxop.isBaseExtended();
            rex.x = bxop.isIndexExtended();
        },
    }

    if (!rex.present and !rex.isSet()) return;
    var byte: u8 = 0b0100_0000;
    if (rex.w) byte |= 0b1000;
    if (rex.r) byte |= 0b0100;
    if (rex.x) byte |= 0b0010;
    if (rex.b) byte |= 0b0001;

    buf[len.*] = byte;
    len.* += 1;
}

const Prefix = enum(u3) {
    none,
};

const OpType = enum(u8) {
    none,
    reg,
    mem,
    imm,
};

pub const Op = struct {
    type: OpType,
    data: union {
        reg: Register,
        mem: Memory,
        imm: Immediate,
    },

    pub fn sibBase(base: Base, disp: i32) Op {
        return .{ .type = .mem, .data = .{ .mem = Memory.sibBase(base, disp) } };
    }

    pub fn reg(r: Register) Op {
        return .{ .type = .reg, .data = .{ .reg = r } };
    }

    pub fn mem(m: Memory) Op {
        return .{ .type = .mem, .data = .{ .mem = m } };
    }

    pub fn imm(u: u64) Op {
        return .{ .type = .imm, .data = .{ .imm = Immediate.u(u) } };
    }

    fn isBaseExtended(self: Op) bool {
        return switch (self.type) {
            .none, .imm => false,
            .reg => self.data.reg.isExtended(),
            .mem => {
                const base = self.data.mem.base();
                if (base == .reg) {
                    return base.reg.isExtended();
                } else return false;
            },
        };
    }

    fn isIndexExtended(self: Op) bool {
        return switch (self.type) {
            .none, .reg, .imm => false,
            .mem => {
                if (self.data.mem == .sib) {
                    if (self.data.mem.sib.scaleIndex.scale > 0) {
                        return self.data.mem.sib.scaleIndex.index.isExtended();
                    }
                }
                return false;
            },
        };
    }
};

pub const Memory = union(enum) {
    sib: Sib,
    moffs: Moffs,
    rip: i32,

    pub fn sibBase(base_: Base, disp: i32) Memory {
        return .{ .sib = Sib{ .base = base_, .disp = disp, .scaleIndex = ScaleIndex.none } };
    }

    pub fn initRip(disp: i32) Memory {
        return .{ .rip = disp };
    }

    fn base(self: Memory) Base {
        return switch (self) {
            .moffs => |moffs| .{ .reg = moffs.seg },
            .sib => |sib| sib.base,
            .rip => .none,
        };
    }
};

const ScaleIndex = struct {
    scale: u4,
    index: Register,

    const none = ScaleIndex{ .scale = 0, .index = undefined };
};

pub const Base = union(enum) {
    none,
    reg: Register,
    frame: FrameIndex,

    pub fn initReg(r: Register) Base {
        return .{ .reg = r };
    }
};

const FrameIndex = enum(u32) {
    _,
};

const Sib = struct {
    base: Base,
    scaleIndex: ScaleIndex,
    disp: i32,
};

const Moffs = struct {
    seg: Register,
    offset: u64,
};

const Immediate = union(enum) {
    unsigned: u64,

    fn u(x: u64) Immediate {
        return .{ .unsigned = x };
    }
};

const modrm_ext = u3;

const Encoding = struct {
    opEn: OpEn,
    modrm_ext: modrm_ext,
    mode: Mode,
    feature: Feature,

    fn init(opEn_: OpEn, modrm_ext_: modrm_ext, mode_: Mode, feature_: Feature) Encoding {
        const new = Encoding{
            .opEn = opEn_,
            .modrm_ext = modrm_ext_,
            .mode = mode_,
            .feature = feature_,
        };
        return new;
    }

    fn modRmExt(self: Encoding) u3 {
        return switch (self.opEn) {
            .m => self.modrm_ext,
            else => unreachable,
        };
    }
};

fn mandatoryPrefix(opc: []const u8) ?u8 {
    const prefix = opc[0];
    return switch (prefix) {
        0x66, 0xf2, 0xf3 => prefix,
        else => null,
    };
}

const OpEn = enum(u8) {
    m,
    mr,
    rm,
    rmi,
    oi,
    o,
};

const Mode = enum(u8) {
    none,
    rex,
    long,
};

const Feature = enum(u8) {
    none,
};

test "x64 encoding" {
    var buf: std.ArrayListAlignedUnmanaged(u8, .fromByteUnits(std.heap.page_size_min)) = .{};
    defer buf.deinit(t.alloc);
    const encoder = Encoder{ .alloc = t.alloc, .buf = &buf };

    buf.clearRetainingCapacity();
    try encoder.pushReg(.rbp);
    try t.eqSlice(u8, buf.items, &.{0x55});

    buf.clearRetainingCapacity();
    try encoder.movReg(.rbp, .rsp);
    try t.eqSlice(u8, buf.items, &.{ 0x48, 0x8b, 0xec });

    buf.clearRetainingCapacity();
    try encoder.cmp(.rdx, .rcx);
    try t.eqSlice(u8, buf.items, &.{ 0x48, 0x3b, 0xd1 });

    buf.clearRetainingCapacity();
    try encoder.jumpRel(100);
    try t.eqSlice(u8, buf.items, &.{ 0xe9, 0x64, 0x00, 0x00, 0x00 });

    buf.clearRetainingCapacity();
    try encoder.jumpReg(.rax);
    try t.eqSlice(u8, buf.items, &.{ 0xff, 0xe0 });

    buf.clearRetainingCapacity();
    try encoder.jumpCond(jge, 100);
    try t.eqSlice(u8, buf.items, &.{ 0x0f, 0x8d, 0x64, 0x00, 0x00, 0x00 });

    buf.clearRetainingCapacity();
    try encoder.lea(.rcx, Memory.sibBase(Base{ .reg = .rdx }, 100));
    try t.eqSlice(u8, buf.items, &.{ 0x48, 0x8d, 0x4a, 0x64 });

    buf.clearRetainingCapacity();
    try encoder.lea(.rax, Memory{ .rip = 16 });
    try t.eqSlice(u8, buf.items, &.{ 0x48, 0x8d, 0x05, 0x10, 0x00, 0x00, 0x00 });

    buf.clearRetainingCapacity();
    try encoder.movMem(.rcx, Memory.sibBase(Base{ .reg = .rbp }, 8));
    try t.eqSlice(u8, buf.items, &.{ 0x48, 0x8b, 0x4d, 0x08 });

    buf.clearRetainingCapacity();
    try encoder.movToMem(Memory.sibBase(Base{ .reg = .rbp }, 8), .rcx);
    try t.eqSlice(u8, buf.items, &.{ 0x48, 0x89, 0x4d, 0x08 });

    buf.clearRetainingCapacity();
    try encoder.movImm(.rdx, 0x7ffc000100000001);
    try t.eqSlice(u8, buf.items, &.{ 0x48, 0xba, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0xfc, 0x7f });

    buf.clearRetainingCapacity();
    try encoder.callRel(100);
    try t.eqSlice(u8, buf.items, &.{ 0xe8, 0x64, 0x00, 0x00, 0x00 });

    buf.clearRetainingCapacity();
    try encoder.callReg(.rax);
    try t.eqSlice(u8, buf.items, &.{ 0xff, 0xd0 });

    buf.clearRetainingCapacity();
    try encoder.ret();
    try t.eqSlice(u8, buf.items, &.{0xc3});

    buf.clearRetainingCapacity();
    try encoder.int3();
    try t.eqSlice(u8, buf.items, &.{0xcc});
}
