const builtin = @import("builtin");
const cy = @import("../cyber.zig");
const Slot = u16;
const CodeBuffer = cy.jitgen.CodeBuffer;

const a64 = @import("a64_assembler.zig");
const x64 = @import("x64_assembler.zig");

/// Provides a common interface for assembling machine code related to stencils.
/// Most machine code is still being generated from stencils.

/// Logical register.
pub const LRegister = enum {
    fp,
    arg0,
    arg1,
    arg2,
    arg3,
    temp,
};

pub const LCond = enum(u8) {
    ge,
    _,
};

pub fn genLoadSlot(buf: *CodeBuffer, dst: LRegister, src: Slot) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_load_slot(buf, dst, src),
        // .x86_64 => try x64.genLoadSlot(c, dst, src),
        else => return error.Unsupported,
    }
}

pub fn genStoreSlot(c: *cy.Chunk, dst: Slot, src: LRegister) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genStoreSlot(c, dst, src),
        .x86_64 => try x64.genStoreSlot(c, dst, src),
        else => return error.Unsupported,
    }
}

pub fn genAddImm(c: *cy.Chunk, dst: LRegister, src: LRegister, imm: u64) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genAddImm(c, dst, src, imm),
        .x86_64 => try x64.genAddImm(c, dst, src, imm),
        else => return error.Unsupported,
    }
}

pub fn genMovImm(buf: *CodeBuffer, dst: LRegister, imm: u64) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_imm(buf, dst, imm),
        // .x86_64 => try x64.genMovImm(c, dst, imm),
        else => return error.Unsupported,
    }
}

pub fn genJumpCond(c: *cy.Chunk, cond: LCond, offset: i32) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genJumpCond(c, cond, offset),
        .x86_64 => try x64.genJumpCond(c, cond, offset),
        else => return error.Unsupported,
    }
}

pub fn patchJumpCond(c: *cy.Chunk, pc: usize, to: usize) void {
    switch (builtin.cpu.arch) {
        .aarch64 => a64.patchJumpCond(c, pc, to),
        .x86_64 => x64.patchJumpCond(c, pc, to),
        else => unreachable,
    }
}

pub fn genPatchableJumpRel(c: *cy.Chunk) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genPatchableJumpRel(c),
        .x86_64 => try x64.genPatchableJumpRel(c),
        else => return error.Unsupported,
    }
}

pub fn patch_imm64(buf: *cy.jitgen.CodeBuffer, pc: usize, reg: LRegister, value: u64) void {
    switch (builtin.cpu.arch) {
        .aarch64 => a64.patch_imm64(buf, pc, reg, value),
        // .x86_64 => x64.patchJumpRel(c, pc, to),
        else => unreachable,
    }
}

pub fn patch_jump_rel(buf: *cy.jitgen.CodeBuffer, pc: usize, to: usize) void {
    switch (builtin.cpu.arch) {
        .aarch64 => a64.patch_jump_rel(buf, pc, to),
//        .x86_64 => x64.patchJumpRel(buf, pc, to),
        else => unreachable,
    }
}

pub fn genCmp(c: *cy.Chunk, left: LRegister, right: LRegister) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genCmp(c, left, right),
        .x86_64 => try x64.genCmp(c, left, right),
        else => return error.Unsupported,
    }
}

pub fn genMovPcRel(c: *cy.Chunk, dst: LRegister, to: usize) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genMovPcRel(c, dst, to),
        .x86_64 => try x64.genMovPcRel(c, dst, to),
        else => return error.Unsupported,
    }
}

pub fn genPatchableMovPcRel(c: *cy.Chunk, dst: LRegister) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genPatchableMovPcRel(c, dst),
        .x86_64 => try x64.genPatchableMovPcRel(c, dst),
        else => return error.Unsupported,
    }
}

pub fn patchMovPcRelTo(c: *cy.Chunk, pc: usize, to: usize) void {
    switch (builtin.cpu.arch) {
        .aarch64 => a64.patchMovPcRelTo(c, pc, to),
        .x86_64 => x64.patchMovPcRelTo(c, pc, to),
        else => unreachable,
    }
}

pub fn genStoreSlotImm(c: *cy.Chunk, dst: Slot, imm: u64) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genStoreSlotImm(c, dst, imm),
        .x86_64 => try x64.genStoreSlotImm(c, dst, imm),
        else => return error.Unsupported,
    }
}

pub fn genStoreSlotValue(c: *cy.Chunk, dst: Slot, val: cy.Value) !void {
    return genStoreSlotImm(c, dst, val.val);
}

pub fn genBreakpoint(c: *cy.Chunk) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genBreakpoint(c),
        .x86_64 => try x64.genBreakpoint(c),
        else => return error.Unsupported,
    }
}

pub fn gen_log(buf: *cy.jitgen.CodeBuffer, msg: []const u8) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_log(buf, msg),
        else => return error.Unsupported,
    }
}

pub fn genCallFunc(c: *cy.Chunk, func: *cy.Func) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genCallFunc(c, func),
//        .x86_64 => try x64.genCallFunc(c, func),
        else => return error.Unsupported,
    }
}

pub fn genCallFuncPtr(c: *cy.Chunk, ptr: *const anyopaque) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genCallFuncPtr(c, ptr),
        .x86_64 => try x64.genCallFuncPtr(c, ptr),
        else => return error.Unsupported,
    }
}

pub fn genFuncReturn(c: *cy.Chunk) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genFuncReturn(c),
        .x86_64 => try x64.genFuncReturn(c),
        else => return error.Unsupported,
    }
}

pub fn genMainReturn(c: *cy.Chunk) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genMainReturn(c),
        .x86_64 => try x64.genMainReturn(c),
        else => return error.Unsupported,
    }
}
