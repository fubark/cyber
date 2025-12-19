const std = @import("std");
const stdx = @import("stdx");
const cy = @import("../cyber.zig");
const t = stdx.testing;
const Slot = u16;
const assm = @import("assembler.zig");

const X64 = @import("x64.zig");
pub const BaseReg = X64.Base.reg;
pub const MemSibBase = X64.Memory.sibBase;
const MemRip = X64.Memory.rip;

const LRegister = assm.LRegister;
const Register = X64.Register;
const gen = @import("gen.zig");
const CodeBuffer = gen.CodeBuffer;
const Op = X64.Op;
const Encoding = X64.Encoding;
const Memory = X64.Memory;

comptime {
    std.testing.refAllDecls(X64);
}

pub const FpReg: X64.Register = .rsi;

fn encodeRexPrefix(buf: []u8, len: *usize, enc: Encoding, ops: []const Op) void {
    var rex = X64.Rex{
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

fn encodeHeader(out: []u8, enc: Encoding, opc: []const u8, ops: []const Op) usize {
    // const prefix = Prefix.none;
    var len: usize = 0;

    var hasMandatoryPrefix = false;
    if (X64.mandatoryPrefix(opc)) |byte| {
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
                    buf[len.*] = X64.ModRM.sibDisp0(opEnc);
                    len.* += 1;

                    if (sib.scaleIndex.scale > 0) {
                        unreachable;
                    } else {
                        buf[len.*] = X64.SIBByte.disp32();
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
                                buf[len.*] = X64.ModRM.sibDisp0(opEnc);
                                len.* += 1;
                                if (sib.scaleIndex.scale > 0) {
                                    unreachable;
                                } else {
                                    buf[len.*] = X64.SIBByte.initBase(baseEnc);
                                    len.* += 1;
                                }
                            } else if (std.math.cast(i8, sib.disp)) |disp| {
                                buf[len.*] = X64.ModRM.sibDisp8(opEnc);
                                len.* += 1;

                                if (sib.scaleIndex.scale > 0) {
                                    unreachable;
                                } else {
                                    buf[len.*] = X64.SIBByte.baseDisp8(baseEnc);
                                    len.* += 1;
                                }

                                buf[len.*] = @bitCast(disp);
                                len.* += 1;
                            } else {
                                unreachable;
                            }
                        } else {
                            if (sib.disp == 0 and baseEnc != 5) {
                                buf[len.*] = X64.ModRM.indirectDisp0(opEnc, baseEnc);
                                len.* += 1;
                            } else if (std.math.cast(i8, sib.disp)) |disp| {
                                buf[len.*] = X64.ModRM.indirectDisp8(opEnc, baseEnc);
                                len.* += 1;

                                buf[len.*] = @bitCast(disp);
                                len.* += 1;
                            } else {
                                buf[len.*] = X64.ModRM.indirectDisp32(opEnc, baseEnc);
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
            buf[len.*] = X64.ModRM.ripDisp32(opEnc);
            len.* += 1;

            @memcpy(buf[len.* .. len.* + 4], std.mem.asBytes(&rip));
            len.* += 4;
        },
        else => unreachable,
    }
}

fn encodeRMOps(out: []u8, len: *usize, enc: Encoding, r: Op, m: Op) void {
    switch (m.type) {
        .reg => {
            const rm = r.data.reg.lowEnc();
            out[len.*] = X64.ModRM.direct(rm, m.data.reg.lowEnc());
            len.* += 1;
        },
        .mem => {
            encodeMemory(out, len, enc, m.data.mem, r);
        },
        else => unreachable,
    }
}

fn encode(buf: *CodeBuffer, enc: Encoding, opc: []const u8, ops: []const Op) !void {
    var out = try prepInstBuf(buf);
    var len = encodeHeader(out, enc, opc, ops);

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
                    out[len] = X64.ModRM.direct(rm, memop.data.reg.lowEnc());
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
    buf.buf.items.len += len;
}

fn prepInstBuf(buf: *CodeBuffer) ![]u8 {
    _ = try buf.ensureUnusedCap(32);
    return buf.buf.items.ptr[buf.buf.items.len .. buf.buf.items.len + 32];
}

pub fn push_int3(buf: *CodeBuffer) !void {
    try buf.push(&.{0xcc});
}

pub fn push_ret(buf: *CodeBuffer) !void {
    try buf.push(&.{0xc3});
}

pub fn push_call_rel(buf: *CodeBuffer, offset: i32) !void {
    const out = try buf.reserve_slice(5);
    out[0] = 0xe8;
    @memcpy(out[1..5], std.mem.asBytes(&offset));
}

pub fn push_call_reg(buf: *CodeBuffer, reg: Register) !void {
    const enc = Encoding.init(.m, 2, .none, .none);
    const out = try prepInstBuf(buf);
    var len = encodeHeader(out, enc, &.{0xff}, &.{Op.reg(reg)});
    encodeMOp(out, &len, enc, Op.reg(reg));
    buf.buf.items.len += len;
}

/// TODO: Support rel8.
pub fn push_jump_cond(buf: *CodeBuffer, code: u8, offset: i32) !void {
    try buf.push(&.{0x0f, code});
    try buf.push_u32(@bitCast(offset));
}

/// TODO: Support rel8.
pub fn push_jump_rel(buf: *CodeBuffer, offset: i32) !void {
    try buf.push(&.{0xe9});
    try buf.push_u32(@bitCast(offset));
}

pub fn push_jump_reg(buf: *CodeBuffer, reg: Register) !void {
    const enc = Encoding.init(.m, 4, .none, .none);
    const out = try prepInstBuf(buf);
    var len = encodeHeader(out, enc, &.{0xff}, &.{Op.reg(reg)});
    encodeMOp(out, &len, enc, Op.reg(reg));
    buf.buf.items.len += len;
}

fn encodeMOp(out: []u8, len: *usize, enc: Encoding, op: Op) void {
    const rm = enc.modRmExt();
    switch (op.type) {
        .reg => {
            out[len.*] = X64.ModRM.direct(rm, op.data.reg.lowEnc());
            len.* += 1;
        },
        .mem => |_| {
            unreachable;
        },
        else => unreachable,
    }
}

pub fn push_lea(buf: *CodeBuffer, dst: Register, src: Memory) !void {
    const enc = Encoding.init(.rm, 0, .long, .none);
    try encode(buf, enc, &.{0x8d}, &.{ Op.reg(dst), Op.mem(src) });
}

pub fn push_cmp(buf: *CodeBuffer, left: Register, right: Register) !void {
    const enc = Encoding.init(.mr, 0, .long, .none);
    const out = try prepInstBuf(buf);
    var len = encodeHeader(out, enc, &.{0x3b}, &.{ Op.reg(left), Op.reg(right) });
    encodeRMOps(out, &len, enc, Op.reg(left), Op.reg(right));
    buf.buf.items.len += len;
}

pub fn pushReg(buf: *CodeBuffer, r: Register) !void {
    const enc = Encoding.init(.o, 0, .none, .none);
    const out = try prepInstBuf(buf);
    const len = encodeHeader(out, enc, &.{0x50}, &.{Op.reg(r)});
    buf.buf.items.len += len;
}

fn encodeImm64(buf: []u8, len: *usize, imm: u64) void {
    @memcpy(buf[len.* .. len.* + 8], std.mem.asBytes(&imm));
    len.* += 8;
}

pub fn push_imm(buf: *CodeBuffer, dst: Register, imm: u64) !void {
    const enc = Encoding.init(.oi, 0, .long, .none);
    const out = try prepInstBuf(buf);
    var len = encodeHeader(out, enc, &.{0xb8}, &.{ Op.reg(dst), Op.imm(imm) });
    encodeImm64(out, &len, imm);
    buf.buf.items.len += len;
}

pub fn push_store(buf: *CodeBuffer, dst: Memory, reg: Register) !void {
    const enc = Encoding.init(.mr, 0, .long, .none);
    const out = try prepInstBuf(buf);
    var len = encodeHeader(out, enc, &.{0x89}, &.{ Op.mem(dst), Op.reg(reg) });
    encodeRMOps(out, &len, enc, Op.reg(reg), Op.mem(dst));
    buf.buf.items.len += len;
}

pub fn push_mov(buf: *CodeBuffer, dst: Register, src: Register) !void {
    const enc = Encoding.init(.rm, 0, .long, .none);
    const out = try prepInstBuf(buf);
    var len = encodeHeader(out, enc, &.{0x8b}, &.{ Op.reg(dst), Op.reg(src) });
    encodeRMOps(out, &len, enc, Op.reg(dst), Op.reg(src));
    buf.buf.items.len += len;
}

pub fn push_load(buf: *CodeBuffer, dst: Register, mem: Memory) !void {
    const enc = Encoding.init(.rm, 0, .long, .none);
    const out = try prepInstBuf(buf);
    var len = encodeHeader(out, enc, &.{0x8b}, &.{ Op.reg(dst), Op.mem(mem) });
    encodeRMOps(out, &len, enc, Op.reg(dst), Op.mem(mem));
    buf.buf.items.len += len;
}

pub fn gen_load_slot(buf: *CodeBuffer, dst: LRegister, src: Slot) !void {
    try push_load(buf, toReg(dst), .sibBase(.initReg(FpReg), src * 8));
}

pub fn gen_store_slot(buf: *CodeBuffer, dst: Slot, src: LRegister) !void {
    try push_store(buf, .sibBase(.initReg(FpReg), dst * 8), toReg(src));
}

pub fn gen_add(buf: *CodeBuffer, dst: LRegister, src: LRegister) !void {
    const enc = Encoding.init(.rm, 0, .long, .none);
    try encode(buf, enc, &.{0x03}, &.{ Op.reg(toReg(dst)), Op.reg(toReg(src)) });
}

pub fn gen_add_imm(buf: *CodeBuffer, dst: LRegister, src: LRegister, imm: u64) !void {
    try push_lea(buf, toReg(dst), .sibBase(.initReg(toReg(src)), @intCast(imm)));
}

pub fn gen_sub(buf: *CodeBuffer, dst: LRegister, src: LRegister) !void {
    const enc = Encoding.init(.rm, 0, .long, .none);
    try encode(buf, enc, &.{0x2b}, &.{ Op.reg(toReg(dst)), Op.reg(toReg(src)) });
}

pub fn gen_sub_imm(buf: *CodeBuffer, dst: LRegister, src: LRegister, imm: u64) !void {
    const disp: i32 = @intCast(imm);
    try push_lea(buf, toReg(dst), .sibBase(.initReg(toReg(src)), -disp));
}

pub fn gen_imm(buf: *CodeBuffer, dst: LRegister, imm: u64) !void {
    try push_imm(buf, toReg(dst), imm);
}

pub fn genJumpCond(buf: *CodeBuffer, cond: assm.LCond, offset: i32) !void {
    try push_jump_cond(buf, toCond(cond), offset);
}

pub fn patchJumpCond(buf: *CodeBuffer, pc: usize, to: usize) void {
    const jumpInstLen = 6;
    const offset: i32 = @intCast(@as(isize, @bitCast(to -% (pc + jumpInstLen))));
    // Displacement bytes start at the 3rd byte.
    const disp = buf.buf.items[pc+2..pc+2+4];
    @memcpy(disp, std.mem.asBytes(&offset));
}

pub fn genPatchableJumpRel(c: *cy.Chunk) !void {
    try c.x64Enc.jumpRel(0);
}

pub fn patch_jump_rel(buf: *CodeBuffer, pc: usize, to: usize) void {
    const jumpInstLen = 5;
    const offset: i32 = @intCast(@as(isize, @bitCast(to -% (pc + jumpInstLen))));
    // Displacement bytes start at the 2nd byte.
    const disp = buf.buf.items[pc+1..pc+1+4];
    @memcpy(disp, std.mem.asBytes(&offset));
}

pub fn genCmp(buf: *CodeBuffer, left: LRegister, right: LRegister) !void {
    try push_cmp(buf, toReg(left), toReg(right));
}

pub fn genMovPcRel(c: *cy.Chunk, dst: LRegister, to: usize) !void {
    const pc = c.jitGetPos();
    const instLen = 7;
    const offset: i32 = @intCast(@as(isize, @bitCast(to -% (pc + instLen))));
    try c.x64Enc.lea(toReg(dst), MemRip(offset));
}

pub fn genPatchableMovPcRel(c: *cy.Chunk, dst: LRegister) !void {
    try c.x64Enc.lea(toReg(dst), MemRip(0));
}

pub fn patchMovPcRelTo(c: *cy.Chunk, pc: usize, to: usize) void {
    // Length of lea inst.
    const instLen = 7;
    const offset: i32 = @intCast(@as(isize, @bitCast(to -% (pc + instLen))));
    // Displacement bytes start at the 4th byte.
    const disp = c.jitBuf.buf.items[pc+3..pc+3+4];
    @memcpy(disp, std.mem.asBytes(&offset));
}

pub fn genStoreSlotImm(c: *cy.Chunk, dst: Slot, imm: u64) !void {
    try c.x64Enc.movImm(.rax, imm);
    try c.x64Enc.movToMem(MemSibBase(BaseReg(FpReg), dst * 8), .rax);
}

pub fn genMainReturn(c: *cy.Chunk) !void {
    // // Load return addr into rax.
    // try genLoadSlot(c, .temp, 0);

    // // Jump to rax.
    // try c.x64Enc.jumpReg(.rax);

    // Assumes rbp and rsp were preserved.
    try c.x64Enc.ret();
}

pub fn genCallFunc(buf: *CodeBuffer, func: *cy.Func) !void {
    // // Skip ret info.
    // // Skip bc pc slot.
    // try gen_store_slot(buf, ret + 3, .fp);

    // // Advance fp.
    // try gen_add_imm(buf, .fp, .fp, 8 * ret);

    // Save pc to rax. Callee saves it to the stack.
    const jumpInstLen = 5;
    try push_lea(buf, .rax, .initRip(jumpInstLen));

    // Push empty call.
    const jumpPc = buf.pos();
    try buf.relocs.append(buf.gpa, .{ .type = .jumpToFunc, .data = .{ .jumpToFunc = .{
        .func = func,
        .pc = @intCast(jumpPc),
    }}});
    try push_jump_rel(buf, 0);
}

pub fn genCallFuncPtr(c: *cy.Chunk, ptr: *const anyopaque) !void {
    // No reloc, copy address to rax.
    try c.x64Enc.movImm(.rax, @intFromPtr(ptr));
    try c.x64Enc.callReg(.rax);
}

pub fn gen_func_ret(buf: *CodeBuffer, ret_size: u16) !void {
    // Load return addr into rax.
    try gen_load_slot(buf, .temp, ret_size + 1);

    // Load prev fp.
    try push_load(buf, FpReg, .sibBase(.initReg(FpReg), (ret_size + 2) * 8));

    // Jump to rax.
    try push_jump_reg(buf, .rax);
}

pub fn genBreakpoint(buf: *CodeBuffer) !void {
    try push_int3(buf);
}

fn toReg(reg: LRegister) Register {
    return switch (reg) {
        .arg0 => .rdx,
        .arg1 => .rcx,
        .arg2 => .r8,
        .arg3 => .r9,
        .fp => FpReg,
        .temp => .rax,
        .temp2 => .rcx,
        //.vm => .rdi,
    };
}

fn toCond(cond: assm.LCond) u8 {
    return switch (cond) {
        .ge => X64.jge,
        else => unreachable,
    };
}

test "x64 encoding" {
    var buf = gen.CodeBuffer.init(t.alloc);
    defer buf.deinit();

    buf.buf.clearRetainingCapacity();
    try pushReg(buf, .rbp);
    try t.eqSlice(u8, buf.raw(), &.{0x55});

    buf.buf.clearRetainingCapacity();
    try push_mov(buf, .rbp, .rsp);
    try t.eqSlice(u8, buf.raw(), &.{ 0x48, 0x8b, 0xec });

    buf.buf.clearRetainingCapacity();
    try push_cmp(buf, .rdx, .rcx);
    try t.eqSlice(u8, buf.raw(), &.{ 0x48, 0x3b, 0xd1 });

    buf.buf.clearRetainingCapacity();
    try push_jump_rel(buf, 100);
    try t.eqSlice(u8, buf.raw(), &.{ 0xe9, 0x64, 0x00, 0x00, 0x00 });

    buf.buf.clearRetainingCapacity();
    try push_jump_reg(buf, .rax);
    try t.eqSlice(u8, buf.raw(), &.{ 0xff, 0xe0 });

    buf.buf.clearRetainingCapacity();
    try push_jump_cond(buf, X64.jge, 100);
    try t.eqSlice(u8, buf.raw(), &.{ 0x0f, 0x8d, 0x64, 0x00, 0x00, 0x00 });

    buf.buf.clearRetainingCapacity();
    try push_lea(buf, .rcx, Memory.sibBase(.Base{ .reg = .rdx }, 100));
    try t.eqSlice(u8, buf.raw(), &.{ 0x48, 0x8d, 0x4a, 0x64 });

    buf.buf.clearRetainingCapacity();
    try push_lea(buf, .rax, Memory{ .rip = 16 });
    try t.eqSlice(u8, buf.raw(), &.{ 0x48, 0x8d, 0x05, 0x10, 0x00, 0x00, 0x00 });

    buf.buf.clearRetainingCapacity();
    try push_load(buf, .rcx, Memory.sibBase(.Base{ .reg = .rbp }, 8));
    try t.eqSlice(u8, buf.raw(), &.{ 0x48, 0x8b, 0x4d, 0x08 });

    buf.buf.clearRetainingCapacity();
    try push_store(buf, Memory.sibBase(.Base{ .reg = .rbp }, 8), .rcx);
    try t.eqSlice(u8, buf.raw(), &.{ 0x48, 0x89, 0x4d, 0x08 });

    buf.buf.clearRetainingCapacity();
    try push_imm(buf, .rdx, 0x7ffc000100000001);
    try t.eqSlice(u8, buf.raw(), &.{ 0x48, 0xba, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0xfc, 0x7f });

    buf.buf.clearRetainingCapacity();
    try push_call_rel(100);
    try t.eqSlice(u8, buf.raw(), &.{ 0xe8, 0x64, 0x00, 0x00, 0x00 });

    buf.buf.clearRetainingCapacity();
    try push_call_reg(buf, .rax);
    try t.eqSlice(u8, buf.raw(), &.{ 0xff, 0xd0 });

    buf.buf.clearRetainingCapacity();
    try push_ret(buf);
    try t.eqSlice(u8, buf.raw(), &.{0xc3});

    buf.buf.clearRetainingCapacity();
    try push_int3(buf);
    try t.eqSlice(u8, buf.raw(), &.{0xcc});
}
