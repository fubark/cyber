const std = @import("std");
const stdx = @import("stdx");
const cy = @import("cyber.zig");

const NullReg = 255;
pub const RegisterId = u8;

/// Virtual register allocator.
/// There are a maximum of 255 registers starting at index 0.
/// The register at index 255 isn't used and represents Null.
/// The registers in the front are reserved for the call convention, params, and local vars.
/// The rest are used for local temps. The number of used temps grow and shrink like a stack during codegen.
pub const Allocator = struct {
    tempStart: u8,
    nextTemp: u8,

    /// Track the max number of registers used to know the stack size required by the block.
    maxTemp: u8,

    compiler: *cy.VMcompiler,
    chunkId: cy.ChunkId,

    pub fn init(c: *cy.VMcompiler, chunkId: cy.ChunkId) Allocator {
        return .{
            .tempStart = undefined,
            .nextTemp = undefined,
            .maxTemp = undefined,
            .compiler = c,
            .chunkId = chunkId,
        };
    }

    pub fn resetState(self: *Allocator, tempStart: u8) void {
        self.tempStart = tempStart;
        self.nextTemp = tempStart;
        self.maxTemp = tempStart;
    }

    pub fn restoreState(self: *Allocator, tempStart: u8, nextTemp: u8, maxTemp: u8) void {
        self.tempStart = tempStart;
        self.nextTemp = nextTemp;
        self.maxTemp = maxTemp;
    }

    pub fn resetNextTemp(self: *Allocator) void {
        self.nextTemp = self.tempStart;
    }

    pub fn getNextTemp(self: *Allocator) u8 {
        return self.nextTemp;
    }

    pub fn setNextTemp(self: *Allocator, nextTemp: u8) void {
        stdx.debug.dassert(nextTemp >= self.tempStart);
        self.nextTemp = nextTemp;
    }

    pub fn setNextTempAfterOr(self: *Allocator, afterMaybeTemp: u8, nextTemp: u8) void {
        if (afterMaybeTemp >= self.tempStart) {
            self.nextTemp = afterMaybeTemp + 1;
        } else {
            self.nextTemp = nextTemp;
        }
    }

    pub fn selectFromLocalVar(self: *Allocator, cstr: RegisterCstr, local: u8) !u8 {
        switch (cstr.type) {
            .exact => {
                return cstr.reg;
            },
            .simple,
            .prefer => {
                return local;
            },
            .temp => {
                return self.consumeNextTemp();
            },
        }
    }

    /// Selecting from a non local var expr.
    pub fn selectFromNonLocalVar(self: *Allocator, cstr: RegisterCstr, willRetainHint: bool) !u8 {
        switch (cstr.type) {
            .exact => {
                return cstr.reg;
            },
            .prefer => {
                if (willRetainHint) {
                    return self.consumeNextTemp();
                } else {
                    return cstr.reg;
                }
            },
            .temp,
            .simple => {
                return self.consumeNextTemp();
            },
        }
    }

    pub fn consumeNextTemp(self: *Allocator) !u8 {
        if (self.nextTemp == NullReg) {
            return self.compiler.chunks.items[self.chunkId].reportError("Exceeded max locals.", &.{});
        }
        defer {
            // Advance to the next free temp.
            self.nextTemp += 1;
            if (self.nextTemp > self.maxTemp) {
                self.maxTemp = self.nextTemp;
            }
        }
        return self.nextTemp;
    }
};

const RegisterCstrType = enum {
    /// 1. Prefers local var register first.
    /// 2. Prefers a given register if the inst does not result in a +1 retain.
    /// 3. Allocates the next temp.
    prefer,

    /// 1. Prefers local var register first.
    /// 2. Allocates the next temp.
    simple, 

    /// Allocates the next temp.
    temp,

    /// Must select given `RegisterCstr.reg`.
    exact,
};

/// Preference on which register to use or allocate.
pub const RegisterCstr = struct {
    type: RegisterCstrType,
    reg: u8,

    /// `mustRetain` indicates the value should be retained by 1 refcount. (eg. call args)
    /// If the expr already does a +1 retain (eg. map literal), then the requirement is satisfied.
    /// If the expr does not result in +1 retain by default, an inst variant is chosen
    /// or an additional inst is used to ensure +1 retain.
    /// If `mustRetain` is false, the expr can still be retained by +1 if required
    /// by the generated inst to stay alive.
    mustRetain: bool, 

    /// TODO: provide hint whether the allocated reg will be used or not.
    /// eg. For expr statements, the top level expr reg isn't used.
    /// If it's not used and the current expr does not produce side-effects, it can omit generating its code.

    pub const simple = RegisterCstr{
        .type = .simple,
        .reg = undefined,
        .mustRetain = false,
    };

    pub const simpleMustRetain = RegisterCstr{
        .type = .simple,
        .reg = undefined,
        .mustRetain = true,
    };

    pub const temp = RegisterCstr{
        .type = .temp,
        .reg = undefined,
        .mustRetain = false,
    };

    pub const tempMustRetain = RegisterCstr{
        .type = .temp,
        .reg = undefined,
        .mustRetain = true,
    };

    pub fn initSimple(mustRetain: bool) RegisterCstr {
        return .{
            .type = .simple,
            .reg = undefined,
            .mustRetain = mustRetain,
        };
    }

    pub fn initExact(reg: u8, mustRetain: bool) RegisterCstr {
        return .{
            .type = .exact,
            .reg = reg,
            .mustRetain = mustRetain,
        };
    }

    pub fn exact(reg: u8) RegisterCstr {
        return .{
            .type = .exact,
            .reg = reg,
            .mustRetain = false,
        };
    }

    pub fn exactMustRetain(reg: u8) RegisterCstr {
        return .{
            .type = .exact,
            .reg = reg,
            .mustRetain = true,
        };
    }

    pub fn prefer(reg: u8) RegisterCstr {
        return .{
            .type = .prefer,
            .reg = reg,
            .mustRetain = false,
        };
    }

    pub fn preferIf(reg: u8, cond: bool) RegisterCstr {
        if (cond) {
            return prefer(reg);
        } else {
            return simple;
        }
    }
};
