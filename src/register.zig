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

    compiler: *cy.Compiler,
    chunkId: cy.ChunkId,

    pub fn init(c: *cy.Compiler, chunkId: cy.ChunkId) Allocator {
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

    /// Selecting for a non local inst that can not fail.
    /// A required dst can be retained but `requiresPreRelease` will be set to true.
    pub fn selectForNoErrNoDepInst(self: *Allocator, cstr: Cstr, instCouldRetain: bool, nodeId: cy.NodeId) !NoErrInst {
        switch (cstr.type) {
            .varSym => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .requiresPreRelease = false,
                    .finalDst = cstr,
                    .nodeId = nodeId,
                };
            },
            .localReg,
            .tempReg => {
                return .{
                    .dst = cstr.data.reg.dst,
                    .requiresPreRelease = cstr.data.reg.releaseDst,
                    .finalDst = null,
                    .nodeId = nodeId,
                };
            },
            .liftedLocal => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .requiresPreRelease = false,
                    .finalDst = cstr,
                    .nodeId = nodeId,
                };
            },
            .captured => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .requiresPreRelease = false,
                    .finalDst = cstr,
                    .nodeId = nodeId,
                };
            },
            .preferVolatile => {
                if (instCouldRetain) {
                    return .{
                        .dst = try self.consumeNextTemp(),
                        .requiresPreRelease = false,
                        .finalDst = null,
                        .nodeId = nodeId,
                    };
                }
                return .{
                    .dst = cstr.data.preferVolatile,
                    .requiresPreRelease = false,
                    .finalDst = null,
                    .nodeId = nodeId,
                };
            },
            .simple => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .requiresPreRelease = false,
                    .finalDst = null,
                    .nodeId = nodeId,
                };
            },
            .none => return error.Unsupported,
        }
    }

    /// Selecting for a non local inst with a dst operand.
    /// A required dst can not be retained or a temp register is allocated and `requiresCopyRelease` will be set true.
    ///
    /// Handles the case where the inst depends and assigns to the same local:
    /// > my node = [Node ...]
    /// > node = node.val
    /// `node` rec gets +1 retain and saves to temp since the `node` cstr has `releaseDst=true`.
    pub fn selectForDstInst(self: *Allocator, cstr: Cstr, instCouldRetain: bool, nodeId: cy.NodeId) !DstInst {
        switch (cstr.type) {
            .varSym => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .finalDst = cstr,
                    .nodeId = nodeId,
                };
            },
            .captured => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .finalDst = cstr,
                    .nodeId = nodeId,
                };
            },
            .localReg,
            .tempReg => {
                if (cstr.data.reg.releaseDst or (cstr.data.reg.retain and !instCouldRetain)) {
                    return .{
                        .dst = try self.consumeNextTemp(),
                        .finalDst = cstr,
                        .nodeId = nodeId,
                    };
                } else {
                    return .{
                        .dst = cstr.data.reg.dst,
                        .finalDst = null,
                        .nodeId = nodeId,
                    };
                }
            },
            .liftedLocal => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .finalDst = cstr,
                    .nodeId = nodeId,
                };
            },
            .preferVolatile => {
                if (instCouldRetain) {
                    return .{
                        .dst = try self.consumeNextTemp(),
                        .finalDst = null,
                        .nodeId = nodeId,
                    };
                }
                return .{
                    .dst = cstr.data.preferVolatile,
                    .finalDst = null,
                    .nodeId = nodeId,
                };
            },
            .simple => {
                return .{
                    .dst = try self.consumeNextTemp(),
                    .finalDst = null,
                    .nodeId = nodeId,
                };
            },
            .none => return error.Unsupported,
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
    finalDst: ?Cstr,
    nodeId: cy.NodeId,
};

pub const DstInst = struct {
    dst: RegisterId,
    finalDst: ?Cstr,
    nodeId: cy.NodeId,
};

const CstrType = enum(u8) {
    /// 1. Prefers local register first.
    /// 2. Allocates the next temp.
    simple,

    /// 1. Prefers local register first.
    /// 2. Prefers a given register that will end up being overwritten.
    ///    Only selected if this inst does not result in a +1 retain.
    /// 3. Allocates the next temp.
    preferVolatile,

    /// To a temp register.
    tempReg,

    /// To a local register.
    localReg,

    /// Var sym.
    varSym,

    /// Lifted local.
    liftedLocal,

    /// Captured.
    captured,

    /// No register selection.
    none,
};

/// Constraints on where a value should go to.
///
/// Some constraints have a `retain` flag.
/// If `retain` is true, the caller expects the value to have increased it's RC by 1.
///     * If the value already has a +1 retain (eg. map literal), the requirement is satisfied.
///     * If the value comes from a local, then a retain copy is generated.
///     * If the value does not have a RC, the requirement is satisfied.
/// If `retain` is false, the value can still be retained by +1 to keep it alive.
pub const Cstr = struct {
    type: CstrType,

    data: union {
        simple: struct {
            retain: bool,
        },
        preferVolatile: RegisterId,
        reg: struct {
            dst: RegisterId,
            retain: bool,
            releaseDst: bool,

            /// Whether to check src type before copy to dst.
            check_type: ?cy.TypeId,
        },
        // Runtime id.
        varSym: u32,
        liftedLocal: struct {
            reg: RegisterId,
            /// This shouldn't change after initialization.
            rcCandidate: bool,
        },
        captured: struct {
            idx: u8,
        },
        uninit: void,
    } = .{ .uninit = {} },

    /// Only relevant for constraints that allow temps: preferVolatile, exact, simple
    jitPreferCondFlag: bool = false,
    jitPreferConstant: bool = false,

    /// TODO: provide hint whether the allocated reill be used or not.
    /// eg. For expr statements, the top level expr reg isn't used.
    /// If it's not used and the current expr does not produce side-effects, it can omit generating its code.

    pub const none = Cstr{
        .type = .none,
    };

    pub const simple = Cstr{ .type = .simple, .data = .{ .simple = .{
        .retain = false,
    }}};

    pub const simpleRetain = Cstr{ .type = .simple, .data = .{ .simple = .{
        .retain = true,
    }}};

    pub const ret = Cstr{ .type = .localReg, .data = .{ .reg = .{
        .dst = 0,
        .retain = true,
        .releaseDst = false,
        .check_type = null,
    }}};

    pub fn toCaptured(idx: u8) Cstr {
        return .{ .type = .captured, .data = .{ .captured = .{
            .idx = idx,
        }}};
    }

    pub fn toVarSym(id: u32) Cstr {
        return .{ .type = .varSym, .data = .{
            .varSym = id
        }};
    }

    pub fn toRetained(self: Cstr) Cstr {
        switch (self.type) {
            .simple => {
                var res = self;
                res.data.simple.retain = true;
                return res;
            },
            .localReg, 
            .tempReg => {
                var res = self;
                res.data.reg.retain = true;
                return res;
            },
            else => {
                return self;
            },
        }
    }

    pub fn initLocalOrTemp(mustRetain: bool) Cstr {
        return .{
            .type = .localOrTemp,
            .mustRetain = mustRetain,
            .data = .{ .localOrTemp = {} }
        };
    }

    pub fn toTempRetain(reg: u8) Cstr {
        return .{ .type = .tempReg, .data = .{ .reg = .{
            .dst = reg,
            .retain = true,
            .releaseDst = false,
            .check_type = null,
        }}};
    }

    pub fn toTemp(reg: u8) Cstr {
        return .{ .type = .tempReg, .data = .{ .reg = .{
            .dst = reg,
            .retain = false,
            .releaseDst = false,
            .check_type = null,
        }}};
    }

    pub fn toRetainedTemp(reg: u8) Cstr {
        return .{ .type = .tempReg, .data = .{ .reg = .{
            .dst = reg,
            .retain = true,
            .releaseDst = true,
            .check_type = null,
        }}};
    }

    pub fn toLocal(reg: u8, rcCandidate: bool) Cstr {
        return .{ .type = .localReg, .data = .{ .reg = .{
            .dst = reg,
            .retain = true,
            .releaseDst = rcCandidate,
            .check_type = null,
        }}};
    }

    pub fn toLiftedLocal(reg: u8, rcCandidate: bool) Cstr {
        return .{ .type = .liftedLocal, .data = .{ .liftedLocal = .{
            .reg = reg,
            .rcCandidate = rcCandidate,
        }}};
    }

    pub fn preferVolatile(reg: u8) Cstr {
        return .{ .type = .preferVolatile, .data = .{ .preferVolatile = reg }};
    }

    pub fn preferVolatileIf(cond: bool, reg: u8) Cstr {
        if (cond) {
            return preferVolatile(reg);
        } else {
            return simple;
        }
    }

    pub fn isExact(self: Cstr) bool {
        return self.type != .simple and self.type != .preferVolatile;
    }
};
