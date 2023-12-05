const builtin = @import("builtin");
const cy = @import("../cyber.zig");
const Slot = cy.register.RegisterId;

const a64 = @import("a64_assembler.zig");

/// Provides a common interface for assembling machine code related to stencils.
/// Most machine code is still being generated from stencils.

pub const VRegister = enum {
    fp,
    arg0,
    arg1,
    arg2,
    arg3,
    temp,
};

pub fn genLoadSlot(c: *cy.Chunk, dst: VRegister, src: Slot) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genLoadSlot(c, dst, src),
        else => return error.Unsupported,
    }
}

pub fn genStoreSlot(c: *cy.Chunk, dst: Slot, src: VRegister) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genStoreSlot(c, dst, src),
        else => return error.Unsupported,
    }
}

pub fn genAddImm(c: *cy.Chunk, dst: VRegister, src: VRegister, imm: u64) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genAddImm(c, dst, src, imm),
        else => return error.Unsupported,
    }
}

pub fn genMovImm(c: *cy.Chunk, dst: VRegister, imm: u64) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genMovImm(c, dst, imm),
        else => return error.Unsupported,
    }
}

pub fn genMovPcRel(c: *cy.Chunk, dst: VRegister, offset: i32) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genMovPcRel(c, dst, offset),
        else => return error.Unsupported,
    }
}

pub fn patchMovPcRelTo(c: *cy.Chunk, pc: usize, to: usize) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.patchMovPcRelTo(c, pc, to),
        else => return error.Unsupported,
    }
}

pub fn genStoreSlotImm(c: *cy.Chunk, dst: Slot, imm: u64) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genStoreSlotImm(c, dst, imm),
        else => return error.Unsupported,
    }
}

pub fn genStoreSlotValue(c: *cy.Chunk, dst: Slot, val: cy.Value) !void {
    return genStoreSlotImm(c, dst, val.val);
}

pub fn genBreakpoint(c: *cy.Chunk) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genBreakpoint(c),
        else => return error.Unsupported,
    }
}

pub fn genCallFuncPtr(c: *cy.Chunk, ptr: *const anyopaque) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genCallFuncPtr(c, ptr),
        else => return error.Unsupported,
    }
}

pub fn genFuncReturn(c: *cy.Chunk) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genFuncReturn(c),
        else => return error.Unsupported,
    }
}

pub fn genMainReturn(c: *cy.Chunk) !void {
    switch (builtin.cpu.arch) {
        .aarch64 => try a64.genMainReturn(c),
        else => return error.Unsupported,
    }
}