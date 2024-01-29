const std = @import("std");
const stdx = @import("stdx");
const cy = @import("cyber.zig");
const log = cy.log.scoped(.reg);

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

    /// This should only be used at the beginning of a block.
    /// Otherwise, statements/exprs should save and restore the next temp.
    /// Doing so allows temp locals to be reserved for some statements like `for iter`.
    pub fn resetNextTemp(self: *Allocator) void {
        self.nextTemp = self.tempStart;
    }

    pub fn getNextTemp(self: *Allocator) u8 {
        return self.nextTemp;
    }

    pub fn selectForLocalInst(self: *Allocator, cstr: RegisterCstr, local: u8, localRetained: bool) !CopyInst {
        switch (cstr.type) {
            .varSym => {
                if (localRetained) {
                    return .{
                        .dst = local,
                        .retainSrc = true,
                        .releaseDst = false,
                        .finalDst = cstr,
                    };
                } else {
                    return .{
                        .dst = local,
                        .retainSrc = false,
                        .releaseDst = false,
                        .finalDst = cstr,
                    };
                }
            },
            .captured => {
                return .{
                    .dst = local,
                    .retainSrc = localRetained,
                    .releaseDst = false,
                    .finalDst = cstr,
                };
            },
            .local => {
                const retainSrc = cstr.data.local.reg != local and localRetained;
                return .{
                    .dst = cstr.data.local.reg,
                    .retainSrc = retainSrc,
                    .releaseDst = cstr.data.local.retained,
                    .finalDst = null,
                };
            },
            .boxedLocal => {
                const retainSrc = cstr.data.local.reg != local and localRetained;
                return .{
                    .dst = local,
                    .retainSrc = retainSrc,
                    .releaseDst = false,
                    .finalDst = cstr,
                };
            },
            .exact => {
                return .{
                    .dst = cstr.data.exact,
                    .retainSrc = cstr.mustRetain and localRetained,
                    .releaseDst = cstr.dstRetained,
                    .finalDst = null,
                };
            },
            .simple,
            .prefer => {
                return .{
                    .dst = local,
                    .retainSrc = cstr.mustRetain,
                    .releaseDst = false,
                    .finalDst = null,
                };
            },
            .temp => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .retainSrc = cstr.mustRetain,
                    .releaseDst = false,
                    .finalDst = null,
                };
            },
            .none => return error.NoneType,
        }
    }

    pub fn selectForTemp(self: *Allocator, cstr: RegisterCstr) !CopyInst {
        _ = self;
        switch (cstr.type) {
            .exact => {
                return .{
                    .dst = cstr,
                };
            },
            .prefer => {
                return error.Unexpected;
            },
            .simple,
            .temp => {
                return error.Unexpected;
            },
            .none => return error.NoneType,
        }
    }

    /// Selecting for a non local inst that can not fail.
    /// A required dst can be retained but `requiresPreRelease` will be set to true.
    pub fn selectForNoErrInst(self: *Allocator, cstr: RegisterCstr, instCouldRetain: bool) !NoErrInst {
        switch (cstr.type) {
            .exact => {
                return .{
                    .dst = cstr.data.exact,
                    .requiresPreRelease = cstr.dstRetained,
                    .finalDst = null,
                };
            },
            .varSym => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .requiresPreRelease = false,
                    .finalDst = cstr,
                };
            },
            .local => {
                return .{
                    .dst = cstr.data.local.reg,
                    .requiresPreRelease = cstr.data.local.retained,
                    .finalDst = null,
                };
            },
            .boxedLocal => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .requiresPreRelease = false,
                    .finalDst = cstr,
                };
            },
            .captured => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .requiresPreRelease = false,
                    .finalDst = cstr,
                };
            },
            .prefer => {
                if (instCouldRetain) {
                    return .{
                        .dst = try self.consumeNextTemp(),
                        .requiresPreRelease = false,
                        .finalDst = null,
                    };
                }
                return .{
                    .dst = cstr.data.prefer,
                    .requiresPreRelease = cstr.dstRetained,
                    .finalDst = null,
                };
            },
            .temp,
            .simple => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .requiresPreRelease = false,
                    .finalDst = null,
                };
            },
            .none => return error.NoneType,
        }
    }

    /// Selecting for a non local inst with a dst operand.
    /// A required dst can not be retained or a temp register is allocated and `requiresCopyRelease` will be set true.
    pub fn selectForDstInst(self: *Allocator, cstr: RegisterCstr, instCouldRetain: bool) !DstInst {
        switch (cstr.type) {
            .varSym => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .finalDst = cstr,
                };
            },
            .exact => {
                if (cstr.dstRetained) {
                    return .{
                        .dst = try self.consumeNextTemp(),
                        .finalDst = cstr,
                    };
                } else {
                    return .{
                        .dst = cstr.data.exact,
                        .finalDst = null,
                    };
                }
            },
            .captured => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .finalDst = cstr,
                };
            },
            .local => {
                if (!cstr.data.local.retained) {
                    return .{
                        .dst = cstr.data.local.reg,
                        .finalDst = null,
                    };
                } else {
                    return .{
                        .dst = try self.consumeNextTemp(),
                        .finalDst = cstr,
                    };
                }
            },
            .boxedLocal => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .finalDst = cstr,
                };
            },
            .prefer => {
                if (instCouldRetain) {
                    return .{
                        .dst = try self.consumeNextTemp(),
                        .finalDst = null,
                    };
                }
                if (cstr.dstRetained) {
                    return .{
                        .dst = try self.consumeNextTemp(),
                        .finalDst = null,
                    };
                } else {
                    return .{
                        .dst = cstr.data.prefer,
                        .finalDst = null,
                    };
                }
            },
            .temp,
            .simple => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .finalDst = null,
                };
            },
            .none => return error.NoneType,
        }
    }

    // Encapsulate to attach logging.
    pub fn freeTemps(self: *Allocator, n: u8) void {
        log.tracev("free temps: {} -{}", .{self.nextTemp, n});
        self.nextTemp -= n;
    }

    pub fn consumeNextTemp(self: *Allocator) !u8 {
        if (self.nextTemp == NullReg) {
            return self.compiler.chunks.items[self.chunkId].reportError("Exceeded max locals.", &.{});
        }
        defer {
            // Advance to the next free temp.
            log.tracev("consume temp: {} +1", .{self.nextTemp});
            self.nextTemp += 1;
            if (self.nextTemp > self.maxTemp) {
                self.maxTemp = self.nextTemp;
            }
        }
        return self.nextTemp;
    }
};

pub const NoErrInst = struct {
    dst: RegisterId,
    requiresPreRelease: bool,
    finalDst: ?RegisterCstr,
};

pub const CopyInst = struct {
    dst: RegisterId,
    retainSrc: bool,
    releaseDst: bool,
    finalDst: ?RegisterCstr,
};

pub const DstInst = struct {
    dst: RegisterId,
    finalDst: ?RegisterCstr,
};

const RegisterCstrType = enum(u8) {
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

    /// Var sym.
    varSym,

    /// Local.
    local,

    /// Boxed local.
    boxedLocal,

    /// Captured.
    captured,

    /// No register selection.
    none,
};

/// TODO: Rename to DstCstr
/// Preference on which register to use or allocate.
pub const RegisterCstr = struct {
    type: RegisterCstrType,

    /// `mustRetain` indicates the value should be retained by 1 refcount.
    /// If the expr already does a +1 retain (eg. map literal), then the requirement is satisfied.
    /// If the expr does not result in +1 retain and is a non-rc value, then the requirement is satisfied.
    /// If the expr is a local, then a copy inst variant is chosen to satisfy the requirement.
    /// If `mustRetain` is false, the expr can still be retained by +1 if required
    /// by the generated inst to stay alive.
    mustRetain: bool, 

    /// Whether the `dst` is retained.
    dstRetained: bool = false,
    data: union {
        prefer: RegisterId,
        exact: RegisterId,
        // Runtime id.
        varSym: u32,
        local: struct {
            reg: RegisterId,
            retained: bool,
        },
        boxedLocal: struct {
            reg: RegisterId,
            retained: bool,
        },
        captured: struct {
            idx: u8,
            retained: bool,
        },
        uninit: void,
        simple: void,
    } = .{ .uninit = {} },

    /// Only relevant for constraints that allow temps: prefer, exact, simple
    jitPreferCondFlag: bool = false,
    jitPreferConstant: bool = false,

    /// TODO: provide hint whether the allocated reill be used or not.
    /// eg. For expr statements, the top level expr reg isn't used.
    /// If it's not used and the current expr does not produce side-effects, it can omit generating its code.

    pub const none = RegisterCstr{
        .type = .none,
        .mustRetain = false,
    };

    pub const simple = RegisterCstr{
        .type = .simple,
        .mustRetain = false,
        .data = .{ .simple = {} }
    };

    pub const simpleMustRetain = RegisterCstr{
        .type = .simple,
        .mustRetain = true,
        .data = .{ .simple = {} }
    };

    pub const temp = RegisterCstr{
        .type = .temp,
        .mustRetain = false,
    };

    pub const tempMustRetain = RegisterCstr{
        .type = .temp,
        .mustRetain = true,
    };

    pub fn toCaptured(idx: u8, retained: bool) RegisterCstr {
        return .{
            .type = .captured,
            .mustRetain = false,
            .data = .{ .captured = .{ .idx = idx, .retained = retained } },
        };
    }

    pub fn toVarSym(id: u32) RegisterCstr {
        return .{
            .type = .varSym,
            .mustRetain = true,
            .data = .{ .varSym = id },
        };
    }

    pub fn initSimple(mustRetain: bool) RegisterCstr {
        return .{
            .type = .simple,
            .mustRetain = mustRetain,
            .data = .{ .simple = {} }
        };
    }

    pub fn initExact(reg: u8, mustRetain: bool) RegisterCstr {
        return .{
            .type = .exact,
            .data = .{ .exact = reg },
            .mustRetain = mustRetain,
        };
    }

    pub fn toRetainedDst(reg: u8) RegisterCstr {
        return .{
            .type = .exact,
            .data = .{ .exact = reg },
            .mustRetain = true,
            .dstRetained = true,
        };
    }

    pub fn toLocal(reg: u8, retained: bool) RegisterCstr {
        return .{
            .type = .local,
            .data = .{ .local = .{
                .reg = reg,
                .retained = retained,
            }},
            .mustRetain = true,
            .dstRetained = retained,
        };
    }

    pub fn toBoxedLocal(reg: u8, retained: bool) RegisterCstr {
        return .{
            .type = .boxedLocal,
            .data = .{ .boxedLocal = .{
                .reg = reg,
                .retained = retained,
            }},
            .mustRetain = true,
            .dstRetained = true,
        };
    }

    pub fn exact(reg: u8) RegisterCstr {
        return .{
            .type = .exact,
            .data = .{ .exact = reg },
            .mustRetain = false,
        };
    }

    pub fn exactMustRetain(reg: u8) RegisterCstr {
        return .{
            .type = .exact,
            .data = .{ .exact = reg },
            .mustRetain = true,
        };
    }

    pub fn prefer(reg: u8) RegisterCstr {
        return .{
            .type = .prefer,
            .data = .{ .prefer = reg },
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
