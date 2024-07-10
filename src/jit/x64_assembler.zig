const std = @import("std");
const stdx = @import("stdx");
const cy = @import("../cyber.zig");
const t = stdx.testing;
const Slot = u8;
const assm = @import("assembler.zig");

const X64 = @import("x64.zig");
pub const BaseReg = X64.Base.reg;
pub const MemSibBase = X64.Memory.sibBase;
const MemRip = X64.Memory.rip;

const LRegister = assm.LRegister;
const Register = X64.Register;
const gen = @import("gen.zig");

comptime {
    std.testing.refAllDecls(X64);
}

pub const FpReg: X64.Register = .rsi;

pub fn genLoadSlot(c: *cy.Chunk, dst: LRegister, src: Slot) !void {
    try c.x64Enc.movMem(toReg(dst), MemSibBase(BaseReg(FpReg), src * 8));
}

pub fn genStoreSlot(c: *cy.Chunk, dst: Slot, src: LRegister) !void {
    try c.x64Enc.movToMem(MemSibBase(BaseReg(FpReg), dst * 8), toReg(src));
}

pub fn genAddImm(c: *cy.Chunk, dst: LRegister, src: LRegister, imm: u64) !void {
    try c.x64Enc.lea(toReg(dst), MemSibBase(BaseReg(toReg(src)), @intCast(imm)));
}

pub fn genMovImm(c: *cy.Chunk, dst: LRegister, imm: u64) !void {
    try c.x64Enc.movImm(toReg(dst), imm);
}

pub fn genJumpCond(c: *cy.Chunk, cond: assm.LCond, offset: i32) !void {
    try c.x64Enc.jumpCond(toCond(cond), offset);
}

pub fn patchJumpCond(c: *cy.Chunk, pc: usize, to: usize) void {
    const jumpInstLen = 6;
    const offset: i32 = @intCast(@as(isize, @bitCast(to -% (pc + jumpInstLen))));
    // Displacement bytes start at the 3rd byte.
    const disp = c.jitBuf.buf.items[pc+2..pc+2+4];
    @memcpy(disp, std.mem.asBytes(&offset));
}

pub fn genPatchableJumpRel(c: *cy.Chunk) !void {
    try c.x64Enc.jumpRel(0);
}

pub fn patchJumpRel(c: *cy.Chunk, pc: usize, to: usize) void {
    const jumpInstLen = 5;
    const offset: i32 = @intCast(@as(isize, @bitCast(to -% (pc + jumpInstLen))));
    // Displacement bytes start at the 2nd byte.
    const disp = c.jitBuf.buf.items[pc+1..pc+1+4];
    @memcpy(disp, std.mem.asBytes(&offset));
}

pub fn genCmp(c: *cy.Chunk, left: LRegister, right: LRegister) !void {
    try c.x64Enc.cmp(toReg(left), toReg(right));
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

pub fn genCallFunc(c: *cy.Chunk, ret: Slot, func: *cy.Func) !void {
    // Skip ret info.
    // Skip bc pc slot.
    try genStoreSlot(c, ret + 3, .fp);

    // Advance fp.
    try genAddImm(c, .fp, .fp, 8 * ret);

    // Save pc to rax. Callee saves it to the stack.
    const jumpInstLen = 5;
    try c.x64Enc.lea(.rax, MemRip(jumpInstLen));

    // Push empty call.
    const jumpPc = c.jitGetPos();
    try c.jitBuf.relocs.append(c.alloc, .{ .type = .jumpToFunc, .data = .{ .jumpToFunc = .{
        .func = func,
        .pc = @intCast(jumpPc),
    }}});
    try c.x64Enc.jumpRel(0);
}

pub fn genCallFuncPtr(c: *cy.Chunk, ptr: *const anyopaque) !void {
    // No reloc, copy address to rax.
    try c.x64Enc.movImm(.rax, @intFromPtr(ptr));
    try c.x64Enc.callReg(.rax);
}

pub fn genFuncReturn(c: *cy.Chunk) !void {
    // Load return addr into rax.
    try genLoadSlot(c, .temp, 2);

    // Load prev fp.
    try c.x64Enc.movMem(FpReg, MemSibBase(BaseReg(FpReg), 3 * 8));

    // Jump to rax.
    try c.x64Enc.jumpReg(.rax);
}

pub fn genBreakpoint(c: *cy.Chunk) !void {
    try c.x64Enc.int3();
}

fn toReg(reg: LRegister) Register {
    return switch (reg) {
        .arg0 => .rdx,
        .arg1 => .rcx,
        .arg2 => .r8,
        .arg3 => .r9,
        .fp => FpReg,
        .temp => .rax,
        //.vm => .rdi,
    };
}

fn toCond(cond: assm.LCond) u8 {
    return switch (cond) {
        .ge => X64.jge,
        else => unreachable,
    };
}