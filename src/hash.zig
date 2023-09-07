const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const sema = cy.sema;
const rt = cy.rt;
const vmc = cy.vmc;

pub const KeyU64 = extern union {
    val: u64,
    objectMemberKey: extern struct {
        objSymId: sema.SymbolId,
        memberNameId: sema.NameSymId,
    },
    resolvedSymKey: extern struct {
        parentSymId: u32,
        nameId: u32,
    },
    resolvedFuncSymKey: extern struct {
        symId: sema.SymbolId,
        funcSigId: sema.FuncSigId,
    },
    localSymKey: extern struct {
        nameId: sema.NameSymId,
        funcSigId: cy.Nullable(sema.FuncSigId),
    },
    relModuleSymKey: extern struct {
        nameId: sema.NameSymId,
        funcSigId: sema.FuncSigId,
    },
    rtVarKey: extern struct {
        parentSymId: sema.SymbolId,
        nameId: sema.NameSymId,
    },
    rtTypeKey: extern struct {
        parentSymId: sema.SymbolId,
        nameId: sema.NameSymId,
    },
    rtTypeMethodGroupKey: extern struct {
        typeId: rt.TypeId,
        mgId: vmc.MethodGroupId,
    },
    rtFieldTableKey: extern struct {
        typeId: rt.TypeId,
        fieldId: rt.FieldId,
    },

    pub fn initLocalSymKey(nameId: sema.NameSymId, funcSigId: ?sema.FuncSigId) KeyU64 {
        return .{
            .localSymKey = .{
                .nameId = nameId,
                .funcSigId = funcSigId orelse cy.NullId,
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

    pub fn initTypeMethodGroupKey(typeId: rt.TypeId, mgId: vmc.MethodGroupId) KeyU64 {
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

    pub fn initTypeKey(parentSymId: sema.SymbolId, nameId: sema.NameSymId) KeyU64 {
        return .{
            .rtTypeKey = .{
                .parentSymId = parentSymId,
                .nameId = nameId,
            },
        };
    }

    pub fn initFieldTableKey(typeId: rt.TypeId, fieldId: rt.FieldId) KeyU64 {
        return .{
            .rtFieldTableKey = .{
                .typeId = typeId,
                .fieldId = fieldId,
            },
        };
    }
};

pub const KeyU64Context = struct {
    pub fn hash(_: @This(), key: KeyU64) linksection(cy.Section) u64 {
        return std.hash.Wyhash.hash(0, std.mem.asBytes(&key.val));
    }
    pub fn eql(_: @This(), a: KeyU64, b: KeyU64) linksection(cy.Section) bool {
        return a.val == b.val;
    }
};

test "hash internals." {
    try t.eq(@sizeOf(KeyU64), 8);
}