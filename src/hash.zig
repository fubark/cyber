const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const sema = cy.sema;
const rt = cy.rt;
const vmc = cy.vmc;

pub const KeyU64 = extern union {
    val: u64,
    modFuncKey: extern struct {
        modSymId: cy.SymId,
        funcSigId: sema.FuncSigId,
    },
    rtTypeMethodGroupKey: extern struct {
        typeId: cy.TypeId,
        mgId: vmc.MethodGroupId,
    },
    rtFieldTableKey: extern struct {
        typeId: cy.TypeId,
        fieldId: rt.FieldId,
    },

    pub fn initModFuncKey(modSymId: cy.SymId, funcSigId: sema.FuncSigId) KeyU64 {
        return .{
            .modFuncKey = .{
                .modSymId = modSymId,
                .funcSigId = funcSigId,
            },
        };
    }

    pub fn initVarKey(parentSymId: sema.SymbolId, nameId: sema.NameSymId) KeyU64 {
        return .{
            .rtVarKey = .{
                .parentSymId = parentSymId,
                .nameId = nameId,
            }
        };
    }

    pub fn initResolvedSymKey(parentSymId: sema.SymbolId, nameId: sema.NameSymId) KeyU64 {
        return .{
            .resolvedSymKey = .{
                .parentSymId = parentSymId,
                .nameId = nameId,
            },
        };
    }

    pub fn initTypeMethodGroupKey(typeId: cy.TypeId, mgId: vmc.MethodGroupId) KeyU64 {
        return .{
            .rtTypeMethodGroupKey = .{
                .typeId = typeId,
                .mgId = mgId,
            },
        };
    }

    pub fn initMethodKey(nameId: sema.NameSymId, numParams: u32) KeyU64 {
        return .{
            .rtMethodKey = .{
                .nameId = nameId,
                .numParams = numParams,
            },
        };
    }

    pub fn initFieldTableKey(typeId: cy.TypeId, fieldId: rt.FieldId) KeyU64 {
        return .{
            .rtFieldTableKey = .{
                .typeId = typeId,
                .fieldId = fieldId,
            },
        };
    }
};

pub const KeyU64Context = struct {
    pub fn hash(_: @This(), key: KeyU64) u64 {
        return std.hash.Wyhash.hash(0, std.mem.asBytes(&key.val));
    }
    pub fn eql(_: @This(), a: KeyU64, b: KeyU64) bool {
        return a.val == b.val;
    }
};

pub const KeyU96 = extern union {
    val: extern struct {
        a: u64,
        b: u32,
    },

    pub fn initModFuncSigKey(sym: *cy.Sym, funcSigId: cy.sema.FuncSigId) KeyU96 {
        return .{ .val = .{
            .a = @intFromPtr(sym),
            .b = funcSigId,
        }};
    }
};

pub const KeyU96Context = struct {
    pub fn hash(_: @This(), key: KeyU96) u64 {
        var hasher = std.hash.Wyhash.init(0);
        @call(.always_inline, std.hash.Wyhash.update, .{&hasher, std.mem.asBytes(&key.val.a)});
        @call(.always_inline, std.hash.Wyhash.update, .{&hasher, std.mem.asBytes(&key.val.b)});
        return hasher.final();
    }
    pub fn eql(_: @This(), a: KeyU96, b: KeyU96) bool {
        return a.val.a == b.val.a and a.val.b == b.val.b;
    }
};

test "hash internals." {
    try t.eq(@sizeOf(KeyU64), 8);
}