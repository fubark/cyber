const std = @import("std");
const stdx = @import("stdx");
const cy = @import("../cyber.zig");
const gen = @import("gen.zig");
const t = stdx.testing;

/// Assembler for basic operations such as copying immediates, load/store regs to memory.
/// Most machine code is still being generated from stencils.

pub const A64 = struct {

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

        pub fn setOffset(self: *BrImm, offset: i28) void {
            self.imm26 = @bitCast(@as(i26, @intCast(offset >> 2)));
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

        pub fn init(cond: Cond, imm: u19) BrCond {
            return .{ .cond = @intFromEnum(cond), .imm19 = imm };
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

        pub fn brk(imm16: u16) Exception {
            return .{
                .ll = 0b00,
                .op2 = 0b000,
                .imm16 = imm16,
                .opc = 0b001,
            };
        }
    };

    pub fn breakpoint(c: *cy.Chunk) !void {
        const inst = Exception.brk(0xf000);
        try c.jitPush(std.mem.asBytes(&inst));
    }

    pub fn copyToPtrImmOff(c: *cy.Chunk, ptrReg: Register, off: usize, reg: Register) !void {
        const inst = LoadStore.strImmOff(ptrReg, @intCast(off), reg);
        try c.jitPush(std.mem.asBytes(&inst));
    }

    /// Ported from https://github.com/llvm-mirror/llvm/blob/master/lib/Target/AArch64/AArch64ExpandImm.cpp
    pub fn copyImm64(c: *cy.Chunk, reg: Register, imm: u64) !void {
        const BitSize: u32 = 64;
        const Mask: u32 = 0xFFFF;

        // Scan the immediate and count the number of 16-bit chunks which are either
        // all ones or all zeros.
        var oneChunks: u32 = 0;
        var zeroChunks: u32 = 0;
        var shift: u32 = 0;
        while (shift < BitSize) : (shift += 16) {
            const chunk: u32 = @intCast((imm >> @intCast(shift)) & Mask);
            if (chunk == Mask) {
                oneChunks += 1;
            } else if (chunk == 0) {
                zeroChunks += 1;
            }
        }

        // Prefer MOVZ/MOVN over ORR because of the rules for the "mov" alias.
        if (((BitSize / 16) - oneChunks <= 1) or ((BitSize / 16) - zeroChunks <= 1)) {
            try copyImm64Simple(c, reg, imm, oneChunks, zeroChunks);
            return;
        }

        // // Try a single ORR.
        // const uimm: u64 = imm << (64 - BitSize) >> (64 - BitSize);
        // var encoding: u64 = undefined;
        // if (AArch64_AM::processLogicalImmediate(UImm, BitSize, Encoding)) {

        //     unsigned Opc = (BitSize == 32 ? AArch64::ORRWri : AArch64::ORRXri);
        //     Insn.push_back({ Opc, 0, Encoding });
        //     return;
        // }

        // One to up three instruction sequences.
        //
        // Prefer MOVZ/MOVN followed by MOVK; it's more readable, and possibly the
        // fastest sequence with fast literal generation.
        if ((oneChunks >= (BitSize / 16) - 2) or (zeroChunks >= (BitSize / 16) - 2)) {
            try copyImm64Simple(c, reg, imm, oneChunks, zeroChunks);
            return;
        }

        // Try other two-instruction sequences.

      // // 64-bit ORR followed by MOVK.
      // // We try to construct the ORR immediate in three different ways: either we
      // // zero out the chunk which will be replaced, we fill the chunk which will
      // // be replaced with ones, or we take the bit pattern from the other half of
      // // the 64-bit immediate. This is comprehensive because of the way ORR
      // // immediates are constructed.
      // for (unsigned Shift = 0; Shift < BitSize; Shift += 16) {
      //   uint64_t ShiftedMask = (0xFFFFULL << Shift);
      //   uint64_t ZeroChunk = UImm & ~ShiftedMask;
      //   uint64_t OneChunk = UImm | ShiftedMask;
      //   uint64_t RotatedImm = (UImm << 32) | (UImm >> 32);
      //   uint64_t ReplicateChunk = ZeroChunk | (RotatedImm & ShiftedMask);
      //   if (AArch64_AM::processLogicalImmediate(ZeroChunk, BitSize, Encoding) ||
      //       AArch64_AM::processLogicalImmediate(OneChunk, BitSize, Encoding) ||
      //       AArch64_AM::processLogicalImmediate(ReplicateChunk, BitSize,
      //                                           Encoding)) {
      //     // Create the ORR-immediate instruction.
      //     Insn.push_back({ AArch64::ORRXri, 0, Encoding });

      //     // Create the MOVK instruction.
      //     const unsigned Imm16 = getChunk(UImm, Shift / 16);
      //     Insn.push_back({ AArch64::MOVKXi, Imm16,
      //              AArch64_AM::getShifterImm(AArch64_AM::LSL, Shift) });
      //     return;
      //   }
      // }

      // // Attempt to use a sequence of two ORR-immediate instructions.
      // if (tryOrrOfLogicalImmediates(Imm, Insn))
      //   return;

      // // Attempt to use a sequence of ORR-immediate followed by AND-immediate.
      // if (tryAndOfLogicalImmediates(Imm, Insn))
      //   return;

      // // Attempt to use a sequence of ORR-immediate followed by EOR-immediate.
      // if (tryEorOfLogicalImmediates(UImm, Insn))
      //   return;

      // // FIXME: Add more two-instruction sequences.

      // // Three instruction sequences.
      // //
      // // Prefer MOVZ/MOVN followed by two MOVK; it's more readable, and possibly
      // // the fastest sequence with fast literal generation. (If neither MOVK is
      // // part of a fast literal generation pair, it could be slower than the
      // // four-instruction sequence, but we won't worry about that for now.)
      // if (OneChunks || ZeroChunks) {
      //   expandMOVImmSimple(Imm, BitSize, OneChunks, ZeroChunks, Insn);
      //   return;
      // }

      // // Check for identical 16-bit chunks within the constant and if so materialize
      // // them with a single ORR instruction. The remaining one or two 16-bit chunks
      // // will be materialized with MOVK instructions.
      // if (BitSize == 64 && tryToreplicateChunks(UImm, Insn))
      //   return;

      // // Check whether the constant contains a sequence of contiguous ones, which
      // // might be interrupted by one or two chunks. If so, materialize the sequence
      // // of contiguous ones with an ORR instruction. Materialize the chunks which
      // // are either interrupting the sequence or outside of the sequence with a
      // // MOVK instruction.
      // if (BitSize == 64 && trySequenceOfOnes(UImm, Insn))
      //   return;

        // We found no possible two or three instruction sequence; use the general
        // four-instruction sequence.
        try copyImm64Simple(c, reg, imm, oneChunks, zeroChunks);
    }

    /// Expand to MOVZ or MOVN of width 64 followed by up to 3 MOVK instructions.
    fn copyImm64Simple(c: *cy.Chunk, dst: Register, imm_: u64, oneChunks: u32, zeroChunks: u32) !void {
        const Mask: u32 = 0xFFFF;

        var imm = imm_;

        // Use a MOVZ or MOVN instruction to set the high bits, followed by one or
        // more MOVK instructions to insert additional 16-bit portions into the
        // lower bits.
        var isNeg = false;

        // Use MOVN to materialize the high bits if we have more all one chunks
        // than all zero chunks.
        if (oneChunks > zeroChunks) {
            isNeg = true;
            imm = ~imm;
        }

        var firstInst: MovWideImm = undefined;
        if (isNeg) {
            firstInst = MovWideImm.movn(dst, 0, 0);
        } else {
            firstInst = MovWideImm.movz(dst, 0, 0);
        }
        var shift: u32 = 0;     // LSL amount for high bits with MOVZ/MOVN
        var lastShift: u32 = 0; // LSL amount for last MOVK
        if (imm != 0) {
            const lz: u32 = @clz(imm);
            const tz: u32 = @ctz(imm);
            shift = (tz / 16) * 16;
            lastShift = ((63 - lz) / 16) * 16;
        }
        var imm16: u16 = @intCast((imm >> @intCast(shift)) & Mask);

        firstInst.imm16 = imm16;
        firstInst.setShift(@intCast(shift));
        try c.jitPush(std.mem.asBytes(&firstInst));

        if (shift == lastShift) {
            return;
        }

        // If a MOVN was used for the high bits of a negative value, flip the rest
        // of the bits back for use with MOVK.
        if (isNeg) {
            imm = ~imm;
        }

        var inst = MovWideImm.movk(dst, 0, 0);
        while (shift < lastShift) {
            shift += 16;
            imm16 = @intCast((imm >> @intCast(shift)) & Mask);
            if (imm16 == (if (isNeg) Mask else 0)) {
                continue; // This 16-bit portion is already set correctly.
            }

            inst.imm16 = imm16;
            inst.setShift(@intCast(shift));
            try c.jitPush(std.mem.asBytes(&inst));
        }
    }

    // Tests extracted from: llvm-project/llvm/test/CodeGen/AArch64/arm64-movi.ll
    test "copyImm64" {
        var buf = gen.CodeBuffer.init();
        defer buf.buf.deinit(t.alloc);

        var c: cy.Chunk = undefined;
        c.jitBuf = &buf;
        c.alloc = t.alloc;

        // 64-bit immed with 32-bit pattern size, rotated by 0.
        buf.buf.clearRetainingCapacity();
        // try copyImm64(&c, .x0, 30064771079);
        // var insts = std.mem.bytesAsSlice(u32, buf.buf.items);
        // try t.eqSlice(u32, insts, &[_]u32{
        //     @bitCast(A64.LogicalImm.orrImm(.x0, .xzr, 2, 0, 0)),
        // });
        // ; CHECK-NEXT:    mov x0, #30064771079

        // 3 movk
        buf.buf.clearRetainingCapacity();
        try copyImm64(&c, .x0, 1427392313513592);
        var insts = std.mem.bytesAsSlice(u32, buf.buf.items);
        try t.eqSlice(u32, insts, &[_]u32{
            @bitCast(A64.MovWideImm.movz(.x0, 22136, 0)),
            @bitCast(A64.MovWideImm.movk(.x0, 43981, 16)),
            @bitCast(A64.MovWideImm.movk(.x0, 4660, 32)),
            @bitCast(A64.MovWideImm.movk(.x0, 5, 48)),
        });

        // movz movk skip1
        buf.buf.clearRetainingCapacity();
        try copyImm64(&c, .x0, 22601072640);
        insts = std.mem.bytesAsSlice(u32, buf.buf.items);
        try t.eqSlice(u32, insts, &[_]u32{
            @bitCast(A64.MovWideImm.movz(.x0, 0x4321, 16)),
            @bitCast(A64.MovWideImm.movk(.x0, 5, 32)),
        });

        // movz skip1 movk
        buf.buf.clearRetainingCapacity();
        try copyImm64(&c, .x0, 147695335379508);
        insts = std.mem.bytesAsSlice(u32, buf.buf.items);
        try t.eqSlice(u32, insts, &[_]u32{
            @bitCast(A64.MovWideImm.movz(.x0, 4660, 0)),
            @bitCast(A64.MovWideImm.movk(.x0, 34388, 32)),
        });

        // movn 
        buf.buf.clearRetainingCapacity();
        try copyImm64(&c, .x0, @bitCast(@as(i64, -42)));
        insts = std.mem.bytesAsSlice(u32, buf.buf.items);
        try t.eqSlice(u32, insts, &[_]u32{
            @bitCast(A64.MovWideImm.movn(.x0, 0x29, 0)),
        });

        // movn skip1 movk
        buf.buf.clearRetainingCapacity();
        try copyImm64(&c, .x0, @bitCast(@as(i64, -176093720012)));
        insts = std.mem.bytesAsSlice(u32, buf.buf.items);
        try t.eqSlice(u32, insts, &[_]u32{
            @bitCast(A64.MovWideImm.movn(.x0, 0xedcb, 0)),
            @bitCast(A64.MovWideImm.movk(.x0, 65494, 32)),
        });

// ; 64-bit immed with 32-bit pattern size, rotated by 2.
// define i64 @test64_32_rot2() nounwind {
// ; CHECK-LABEL: test64_32_rot2:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-4611686002321260541
// ; CHECK-NEXT:    ret
//   ret i64 13835058071388291075
// }

// ; 64-bit immed with 4-bit pattern size, rotated by 3.
// define i64 @test64_4_rot3() nounwind {
// ; CHECK-LABEL: test64_4_rot3:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-1229782938247303442
// ; CHECK-NEXT:    ret
//   ret i64 17216961135462248174
// }

// ; 64-bit immed with 64-bit pattern size, many bits.
// define i64 @test64_64_manybits() nounwind {
// ; CHECK-LABEL: test64_64_manybits:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #4503599627304960
// ; CHECK-NEXT:    ret
//   ret i64 4503599627304960
// }

// ; 64-bit immed with 64-bit pattern size, one bit.
// define i64 @test64_64_onebit() nounwind {
// ; CHECK-LABEL: test64_64_onebit:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #274877906944
// ; CHECK-NEXT:    ret
//   ret i64 274877906944
// }

// ; 32-bit immed with 32-bit pattern size, rotated by 16.
// define i32 @test32_32_rot16() nounwind {
// ; CHECK-LABEL: test32_32_rot16:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov w0, #16711680
// ; CHECK-NEXT:    ret
//   ret i32 16711680
// }

// ; 32-bit immed with 2-bit pattern size, rotated by 1.
// define i32 @test32_2_rot1() nounwind {
// ; CHECK-LABEL: test32_2_rot1:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov w0, #-1431655766
// ; CHECK-NEXT:    ret
//   ret i32 2863311530
// }

// ;==--------------------------------------------------------------------------==
// ; Tests for MOVZ with MOVK.
// ;==--------------------------------------------------------------------------==

// define i32 @movz() nounwind {
// ; CHECK-LABEL: movz:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov w0, #5
// ; CHECK-NEXT:    ret
//   ret i32 5
// }

// define i64 @orr_lsl_pattern() nounwind {
// ; CHECK-LABEL: orr_lsl_pattern:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-6148914691236517206
// ; CHECK-NEXT:    and x0, x0, #0x1fffffffe0
// ; CHECK-NEXT:    ret
//   ret i64 45812984480
// }

// ; FIXME: prefer "mov x0, #-16639; lsl x0, x0, #24"
// define i64 @mvn_lsl_pattern() nounwind {
// ; CHECK-LABEL: mvn_lsl_pattern:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #16777216
// ; CHECK-NEXT:    movk x0, #65471, lsl #32
// ; CHECK-NEXT:    movk x0, #65535, lsl #48
// ; CHECK-NEXT:    ret
//   ret i64 -279156097024
// }

// ; FIXME: prefer "mov w0, #-63; movk x0, #17, lsl #32"
// define i64 @mvn32_pattern_2() nounwind {
// ; CHECK-LABEL: mvn32_pattern_2:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #65473
// ; CHECK-NEXT:    movk x0, #65535, lsl #16
// ; CHECK-NEXT:    movk x0, #17, lsl #32
// ; CHECK-NEXT:    ret
//   ret i64 77309411265
// }

// ;==--------------------------------------------------------------------------==
// ; Tests for ORR with MOVK.
// ;==--------------------------------------------------------------------------==
// ; rdar://14987673

// define i64 @orr_movk1() nounwind {
// ; CHECK-LABEL: orr_movk1:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #72056494543077120
// ; CHECK-NEXT:    movk x0, #57005, lsl #16
// ; CHECK-NEXT:    ret
//   ret i64 72056498262245120
// }

// define i64 @orr_movk2() nounwind {
// ; CHECK-LABEL: orr_movk2:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #72056494543077120
// ; CHECK-NEXT:    movk x0, #57005, lsl #48
// ; CHECK-NEXT:    ret
//   ret i64 -2400982650836746496
// }

// define i64 @orr_movk3() nounwind {
// ; CHECK-LABEL: orr_movk3:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #72056494543077120
// ; CHECK-NEXT:    movk x0, #57005, lsl #32
// ; CHECK-NEXT:    ret
//   ret i64 72020953688702720
// }

// define i64 @orr_movk4() nounwind {
// ; CHECK-LABEL: orr_movk4:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #72056494543077120
// ; CHECK-NEXT:    movk x0, #57005
// ; CHECK-NEXT:    ret
//   ret i64 72056494543068845
// }

// ; rdar://14987618
// define i64 @orr_movk5() nounwind {
// ; CHECK-LABEL: orr_movk5:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-71777214294589696
// ; CHECK-NEXT:    movk x0, #57005, lsl #16
// ; CHECK-NEXT:    ret
//   ret i64 -71777214836900096
// }

// define i64 @orr_movk6() nounwind {
// ; CHECK-LABEL: orr_movk6:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-71777214294589696
// ; CHECK-NEXT:    movk x0, #57005, lsl #16
// ; CHECK-NEXT:    movk x0, #57005, lsl #48
// ; CHECK-NEXT:    ret
//   ret i64 -2400982647117578496
// }

// define i64 @orr_movk7() nounwind {
// ; CHECK-LABEL: orr_movk7:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-71777214294589696
// ; CHECK-NEXT:    movk x0, #57005, lsl #48
// ; CHECK-NEXT:    ret
//   ret i64 -2400982646575268096
// }

// define i64 @orr_movk8() nounwind {
// ; CHECK-LABEL: orr_movk8:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-71777214294589696
// ; CHECK-NEXT:    movk x0, #57005
// ; CHECK-NEXT:    movk x0, #57005, lsl #48
// ; CHECK-NEXT:    ret
//   ret i64 -2400982646575276371
// }

// ; rdar://14987715
// define i64 @orr_movk9() nounwind {
// ; CHECK-LABEL: orr_movk9:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #1152921435887370240
// ; CHECK-NEXT:    movk x0, #65280
// ; CHECK-NEXT:    movk x0, #57005, lsl #16
// ; CHECK-NEXT:    ret
//   ret i64 1152921439623315200
// }

// define i64 @orr_movk10() nounwind {
// ; CHECK-LABEL: orr_movk10:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #1152921504606846720
// ; CHECK-NEXT:    movk x0, #57005, lsl #16
// ; CHECK-NEXT:    ret
//   ret i64 1152921504047824640
// }

// define i64 @orr_movk11() nounwind {
// ; CHECK-LABEL: orr_movk11:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-65281
// ; CHECK-NEXT:    movk x0, #57005, lsl #16
// ; CHECK-NEXT:    movk x0, #65520, lsl #48
// ; CHECK-NEXT:    ret
//   ret i64 -4222125209747201
// }

// define i64 @orr_movk12() nounwind {
// ; CHECK-LABEL: orr_movk12:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-4503599627370241
// ; CHECK-NEXT:    movk x0, #57005, lsl #32
// ; CHECK-NEXT:    ret
//   ret i64 -4258765016661761
// }

// define i64 @orr_movk13() nounwind {
// ; CHECK-LABEL: orr_movk13:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #17592169267200
// ; CHECK-NEXT:    movk x0, #57005
// ; CHECK-NEXT:    movk x0, #57005, lsl #48
// ; CHECK-NEXT:    ret
//   ret i64 -2401245434149282131
// }

// ; rdar://13944082
// define i64 @g() nounwind {
// ; CHECK-LABEL: g:
// ; CHECK:       // %bb.0: // %entry
// ; CHECK-NEXT:    mov x0, #2
// ; CHECK-NEXT:    movk x0, #65535, lsl #48
// ; CHECK-NEXT:    ret
// entry:
//   ret i64 -281474976710654
// }

// define i64 @orr_movk14() nounwind {
// ; CHECK-LABEL: orr_movk14:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-549755813888
// ; CHECK-NEXT:    movk x0, #2048, lsl #16
// ; CHECK-NEXT:    ret
//   ret i64 -549621596160
// }

// define i64 @orr_movk15() nounwind {
// ; CHECK-LABEL: orr_movk15:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #549755813887
// ; CHECK-NEXT:    movk x0, #63487, lsl #16
// ; CHECK-NEXT:    ret
//   ret i64 549621596159
// }

// define i64 @orr_movk16() nounwind {
// ; CHECK-LABEL: orr_movk16:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #2147483646
// ; CHECK-NEXT:    orr x0, x0, #0x7fffe0007fffe0
// ; CHECK-NEXT:    ret
//   ret i64 36028661727494142
// }

// define i64 @orr_movk17() nounwind {
// ; CHECK-LABEL: orr_movk17:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-1099511627776
// ; CHECK-NEXT:    movk x0, #65280, lsl #16
// ; CHECK-NEXT:    ret
//   ret i64 -1095233437696
// }

// define i64 @orr_movk18() nounwind {
// ; CHECK-LABEL: orr_movk18:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #137438887936
// ; CHECK-NEXT:    movk x0, #65473
// ; CHECK-NEXT:    ret
//   ret i64 137438953409
// }

// define i64 @orr_and() nounwind {
// ; CHECK-LABEL: orr_and:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #72340172838076673
// ; CHECK-NEXT:    and x0, x0, #0xffffffffff00
// ; CHECK-NEXT:    ret
//   ret i64 1103823438080
// }

// ; FIXME: prefer "mov w0, #-1431655766; movk x0, #9, lsl #32"
// define i64 @movn_movk() nounwind {
// ; CHECK-LABEL: movn_movk:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #43690
// ; CHECK-NEXT:    movk x0, #43690, lsl #16
// ; CHECK-NEXT:    movk x0, #9, lsl #32
// ; CHECK-NEXT:    ret
//   ret i64 41518017194
// }

// ; FIXME: prefer "mov w0, #-13690; orr x0, x0, #0x1111111111111111"
// define i64 @movn_orr() nounwind {
// ; CHECK-LABEL: movn_orr:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-51847
// ; CHECK-NEXT:    movk x0, #4369, lsl #32
// ; CHECK-NEXT:    movk x0, #4369, lsl #48
// ; CHECK-NEXT:    ret
//   ret i64 1229782942255887737
// }

// ; FIXME: prefer "mov w0, #-305397761; eor x0, x0, #0x3333333333333333"
// define i64 @movn_eor() nounwind {
// ; CHECK-LABEL: movn_eor:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #3689348814741910323
// ; CHECK-NEXT:    movk x0, #52428
// ; CHECK-NEXT:    movk x0, #8455, lsl #16
// ; CHECK-NEXT:    ret
//   ret i64 3689348814437076172
// }

// define i64 @orr_orr_64() nounwind {
// ; CHECK-LABEL: orr_orr_64:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #536866816
// ; CHECK-NEXT:    orr x0, x0, #0x3fff800000000000
// ; CHECK-NEXT:    ret
//   ret i64 4611545281475899392
// }

// define i64 @orr_orr_32() nounwind {
// ; CHECK-LABEL: orr_orr_32:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #558551907040256
// ; CHECK-NEXT:    orr x0, x0, #0x1c001c001c001c00
// ; CHECK-NEXT:    ret
//   ret i64 2018171185438784512
// }

// define i64 @orr_orr_16() nounwind {
// ; CHECK-LABEL: orr_orr_16:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #1152939097061330944
// ; CHECK-NEXT:    orr x0, x0, #0x1000100010001
// ; CHECK-NEXT:    ret
//   ret i64 1153220576333074433
// }

// define i64 @orr_orr_8() nounwind {
// ; CHECK-LABEL: orr_orr_8:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #144680345676153346
// ; CHECK-NEXT:    orr x0, x0, #0x1818181818181818
// ; CHECK-NEXT:    ret
//   ret i64 1880844493789993498
// }

// define i64 @orr_64_orr_8() nounwind {
// ; CHECK-LABEL: orr_64_orr_8:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov x0, #-6148914691236517206
// ; CHECK-NEXT:    orr x0, x0, #0xfffff0000000000
// ; CHECK-NEXT:    ret
//   ret i64 -5764607889538110806
// }

// define i64 @orr_2_eor_16() nounwind {
// ; CHECK-LABEL: orr_2_eor_16:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #6148914691236517205
// ; CHECK-NEXT:    eor  x0, x0, #0x3000300030003000
// ; CHECK-NEXT:    ret
//   ret i64 7301853788297848149
// }

// define i64 @orr_2_eor_32() nounwind {
// ; CHECK-LABEL: orr_2_eor_32:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #6148914691236517205
// ; CHECK-NEXT:    eor  x0, x0, #0x1fffc0001fffc0
// ; CHECK-NEXT:    ret
//   ret i64 6145912199858268821
// }

// define i64 @orr_2_eor_64() nounwind {
// ; CHECK-LABEL: orr_2_eor_64:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #6148914691236517205
// ; CHECK-NEXT:    eor  x0, x0, #0x1fffffffffc00
// ; CHECK-NEXT:    ret
//   ret i64 6148727041252043093
// }

// define i64 @orr_4_eor_8() nounwind {
// ; CHECK-LABEL: orr_4_eor_8:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #2459565876494606882
// ; CHECK-NEXT:    eor  x0, x0, #0x8f8f8f8f8f8f8f8f
// ; CHECK-NEXT:    ret
//   ret i64 12514849900987264429
// }

// define i64 @orr_4_eor_16() nounwind {
// ; CHECK-LABEL: orr_4_eor_16:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #4919131752989213764
// ; CHECK-NEXT:    eor  x0, x0, #0xf00ff00ff00ff00f
// ; CHECK-NEXT:    ret
//   ret i64 12991675787320734795
// }

// define i64 @orr_4_eor_32() nounwind {
// ; CHECK-LABEL: orr_4_eor_32:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #4919131752989213764
// ; CHECK-NEXT:    eor  x0, x0, #0x1ff800001ff80000
// ; CHECK-NEXT:    ret
//   ret i64 6610233413460575300
// }

// define i64 @orr_4_eor_64() nounwind {
// ; CHECK-LABEL: orr_4_eor_64:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #1229782938247303441
// ; CHECK-NEXT:    eor  x0, x0, #0xfff80000000
// ; CHECK-NEXT:    ret
//   ret i64 1229798183233720593
// }

// define i64 @orr_8_eor_16() nounwind {
// ; CHECK-LABEL: orr_8_eor_16:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #3472328296227680304
// ; CHECK-NEXT:    eor  x0, x0, #0x1f801f801f801f80
// ; CHECK-NEXT:    ret
//   ret i64 3436298949444513712
// }

// define i64 @orr_8_eor_32() nounwind {
// ; CHECK-LABEL: orr_8_eor_32:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #1157442765409226768
// ; CHECK-NEXT:    eor  x0, x0, #0xffff8001ffff8001
// ; CHECK-NEXT:    ret
//   ret i64 17289195901212921873
// }

// define i64 @orr_8_eor_64() nounwind {
// ; CHECK-LABEL: orr_8_eor_64:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #3472328296227680304
// ; CHECK-NEXT:    eor  x0, x0, #0x3ffffffff00000
// ; CHECK-NEXT:    ret
//   ret i64 3463215129921859632
// }

// define i64 @orr_16_eor_32() nounwind {
// ; CHECK-LABEL: orr_16_eor_32:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #1143931760365539296
// ; CHECK-NEXT:    eor  x0, x0, #0xffff0001ffff0001
// ; CHECK-NEXT:    ret
//   ret i64 17302565756451360737
// }

// define i64 @orr_16_eor_64() nounwind {
// ; CHECK-LABEL: orr_16_eor_64:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #9214505439794855904
// ; CHECK-NEXT:    eor  x0, x0, #0xfe000
// ; CHECK-NEXT:    ret
//   ret i64 9214505439795847136
// }

// define i64 @orr_32_eor_64() nounwind {
// ; CHECK-LABEL: orr_32_eor_64:
// ; CHECK:       // %bb.0:
// ; CHECK-NEXT:    mov  x0, #1030792151280
// ; CHECK-NEXT:    eor  x0, x0, #0xffff8000003fffff
// ; CHECK-NEXT:    ret
//   ret i64 18446604367017541391
// }
    }
};