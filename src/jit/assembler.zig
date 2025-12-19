const builtin = @import("builtin");
const cy = @import("../cyber.zig");
const Slot = u16;
const CodeBuffer = cy.jitgen.CodeBuffer;

const A64 = @import("a64.zig");
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
    temp2,
};

pub const LCond = enum(u8) {
    ge,
    gt,
    le,
    lt,
    _,
};

pub fn genLoadSlot(buf: *CodeBuffer, dst: LRegister, src: Slot) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_load_slot(buf, dst, src),
        .x86_64 => try x64.gen_load_slot(buf, dst, src),
        else => return error.Unsupported,
    }
}

pub fn genStoreSlot(buf: *CodeBuffer, dst: Slot, src: LRegister) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_store_slot(buf, dst, src),
        .x86_64 => try x64.gen_store_slot(buf, dst, src),
        else => return error.Unsupported,
    }
}

pub fn gen_add(buf: *CodeBuffer, dst: LRegister, src: LRegister) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_add(buf, dst, dst, src),
        .x86_64 => try x64.gen_add(buf, dst, src),
        else => return error.Unsupported,
    }
}

pub fn gen_add_imm(buf: *CodeBuffer, dst: LRegister, src: LRegister, imm: u64) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_add_imm(buf, dst, src, imm),
        .x86_64 => try x64.gen_add_imm(buf, dst, src, imm),
        else => return error.Unsupported,
    }
}

pub fn gen_sub(buf: *CodeBuffer, dst: LRegister, src: LRegister) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_sub(buf, dst, dst, src),
        .x86_64 => try x64.gen_sub(buf, dst, src),
        else => return error.Unsupported,
    }
}

pub fn gen_sub_imm(buf: *CodeBuffer, dst: LRegister, src: LRegister, imm: u64) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_sub_imm(buf, dst, src, imm),
        .x86_64 => try x64.gen_sub_imm(buf, dst, src, imm),
        else => return error.Unsupported,
    }
}

pub fn gen_imm(buf: *CodeBuffer, dst: LRegister, imm: u64) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_imm(buf, dst, imm),
        .x86_64 => try x64.gen_imm(buf, dst, imm),
        else => return error.Unsupported,
    }
}

pub fn genJumpCond(buf: *CodeBuffer, cond: LCond, offset: i32) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genJumpCond(buf, cond, offset),
        .x86_64 => try x64.genJumpCond(buf, cond, offset),
        else => return error.Unsupported,
    }
}

pub fn patchJumpCond(buf: *CodeBuffer, pc: usize, to: usize) void {
    switch (builtin.cpu.arch) {
        .aarch64 => a64.patch_jump_cond(buf, pc, to),
        .x86_64 => x64.patchJumpCond(buf, pc, to),
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

pub fn patch_imm64(buf: *CodeBuffer, pc: usize, reg: LRegister, value: u64) void {
    switch (builtin.cpu.arch) {
        .aarch64 => a64.patch_imm64(buf, pc, reg, value),
        .x86_64 => x64.patch_imm64(buf, pc, reg, value),
        else => unreachable,
    }
}

pub fn patch_jump_rel(buf: *CodeBuffer, pc: usize, to: usize) void {
    switch (builtin.cpu.arch) {
        .aarch64 => a64.patch_jump_rel(buf, pc, to),
        .x86_64 => x64.patch_jump_rel(buf, pc, to),
        else => unreachable,
    }
}

pub fn genCmp(buf: *CodeBuffer, left: LRegister, right: LRegister) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genCmp(buf, left, right),
        .x86_64 => try x64.genCmp(buf, left, right),
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

pub fn gen_nop(buf: *CodeBuffer) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try buf.push_u32(A64.NoOp.init().bitCast()),
        .x86_64 => try x64.push_nop(buf),
        else => return error.Unsupported,
    }
}

pub fn genBreakpoint(buf: *CodeBuffer) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genBreakpoint(buf),
        .x86_64 => try x64.genBreakpoint(buf),
        else => return error.Unsupported,
    }
}

pub fn gen_log(buf: *CodeBuffer, msg: []const u8) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_log(buf, msg),
        else => return error.Unsupported,
    }
}

pub fn genCallFunc(buf: *CodeBuffer, func: *cy.Func) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genCallFunc(buf, func),
        .x86_64 => try x64.genCallFunc(buf, func),
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

pub fn gen_func_ret(buf: *CodeBuffer, ret_size: u16) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.gen_func_ret(buf, ret_size),
        .x86_64 => try x64.gen_func_ret(buf, ret_size),
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
