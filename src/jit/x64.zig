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

    pub fn lowEnc(self: Register) u3 {
        return @truncate(self.enc());
    }

    pub fn bitSize(self: Register) u8 {
        return switch (@intFromEnum(self)) {
            @intFromEnum(Register.rax)...@intFromEnum(Register.r15) => 64,
            else => unreachable,
        };
    }

    pub fn isExtended(self: Register) bool {
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

pub const SIBByte = packed struct {
    base: u3,
    index: u3,
    scale: u2,

    pub fn disp32() u8 {
        return @bitCast(SIBByte{ .base = 5, .index = 4, .scale = 0 });
    }

    pub fn baseDisp8(base: u3) u8 {
        return @bitCast(SIBByte{ .base = base, .index = 4, .scale = 0 });
    }

    pub fn initBase(base: u3) u8 {
        return @bitCast(SIBByte{ .base = base, .index = 4, .scale = 0 });
    }
};

pub const ModRM = packed struct {
    rm: u3,
    regOrOpx: u3,
    mod: u2,

    pub fn direct(regOrOpx: u3, rm: u3) u8 {
        return @bitCast(ModRM{ .rm = rm, .regOrOpx = regOrOpx, .mod = 0b11 });
    }

    pub fn sibDisp0(regOrOpx: u3) u8 {
        return @bitCast(ModRM{ .rm = 0b100, .regOrOpx = regOrOpx, .mod = 0b00 });
    }

    pub fn sibDisp8(regOrOpx: u3) u8 {
        return @bitCast(ModRM{ .rm = 0b100, .regOrOpx = regOrOpx, .mod = 0b01 });
    }

    pub fn indirectDisp0(regOrOpx: u3, rm: u3) u8 {
        return @bitCast(ModRM{ .rm = rm, .regOrOpx = regOrOpx, .mod = 0b00 });
    }

    pub fn indirectDisp8(regOrOpx: u3, rm: u3) u8 {
        return @bitCast(ModRM{ .rm = rm, .regOrOpx = regOrOpx, .mod = 0b01 });
    }

    pub fn indirectDisp32(regOrOpx: u3, rm: u3) u8 {
        return @bitCast(ModRM{ .rm = rm, .regOrOpx = regOrOpx, .mod = 0b10 });
    }

    pub fn ripDisp32(regOrOpx: u3) u8 {
        return @bitCast(ModRM{ .rm = 0b101, .regOrOpx = regOrOpx, .mod = 0b00 });
    }
};

pub const Rex = struct {
    w: bool = false,
    r: bool = false,
    x: bool = false,
    b: bool = false,
    present: bool = false,

    pub fn isSet(self: Rex) bool {
        return self.w or self.r or self.x or self.b;
    }
};

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

    pub fn isBaseExtended(self: Op) bool {
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

    pub fn isIndexExtended(self: Op) bool {
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

pub const Encoding = struct {
    opEn: OpEn,
    modrm_ext: modrm_ext,
    mode: Mode,
    feature: Feature,

    pub fn init(opEn_: OpEn, modrm_ext_: modrm_ext, mode_: Mode, feature_: Feature) Encoding {
        const new = Encoding{
            .opEn = opEn_,
            .modrm_ext = modrm_ext_,
            .mode = mode_,
            .feature = feature_,
        };
        return new;
    }

    pub fn modRmExt(self: Encoding) u3 {
        return switch (self.opEn) {
            .m => self.modrm_ext,
            else => unreachable,
        };
    }
};

pub fn mandatoryPrefix(opc: []const u8) ?u8 {
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