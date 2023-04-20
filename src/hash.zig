const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const sema = cy.sema;
const rt = cy.rt;

pub const KeyU64 = extern union {
    val: u64,
    absResolvedSymKey: extern struct {
        rParentSymId: u32,
        nameId: u32,
    },
    absResolvedFuncSymKey: extern struct {
        rSymId: sema.ResolvedSymId,
        rFuncSigId: sema.ResolvedFuncSigId,
    },
    relLocalSymKey: extern struct {
        nameId: sema.NameSymId,
        rFuncSigId: sema.ResolvedFuncSigId,
    },
    rtMethodKey: extern struct {
        nameId: sema.NameSymId,
        /// Method syms are keyed by numParams since an exact signature
        /// can miss a compatible typed function.
        /// numParams does not include the self param to match callObjSym's numParams.
        numParams: u32,
    },
    relModuleSymKey: extern struct {
        nameId: sema.NameSymId,
        rFuncSigId: sema.ResolvedFuncSigId,
    },
    rtVarKey: extern struct {
        rParentSymId: sema.ResolvedSymId,
        nameId: sema.NameSymId,
    },
    rtTypeKey: extern struct {
        rParentSymId: sema.ResolvedSymId,
        nameId: sema.NameSymId,
    },
    rtMethodTableKey: extern struct {
        typeId: rt.TypeId,
        methodId: rt.MethodId,
    },
    rtFieldTableKey: extern struct {
        typeId: rt.TypeId,
        fieldId: rt.FieldId,
    },

    pub fn initVarKey(rParentSymId: sema.ResolvedSymId, nameId: sema.NameSymId) KeyU64 {
        return .{
            .rtVarKey = .{
                .rParentSymId = rParentSymId,
                .nameId = nameId,
            }
        };
    }

    pub fn initAbsResolvedSymKey(parentSymId: sema.ResolvedSymId, nameId: sema.NameSymId) KeyU64 {
        return .{
            .absResolvedSymKey = .{
                .rParentSymId = parentSymId,
                .nameId = nameId,
            },
        };
    }

    pub fn initMethodTableKey(typeId: rt.TypeId, methodId: rt.MethodId) KeyU64 {
        return .{
            .rtMethodTableKey = .{
                .typeId = typeId,
                .methodId = methodId,
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

    pub fn initTypeKey(rParentSymId: sema.ResolvedSymId, nameId: sema.NameSymId) KeyU64 {
        return .{
            .rtTypeKey = .{
                .rParentSymId = rParentSymId,
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

test "Internals." {
    try t.eq(@sizeOf(KeyU64), 8);
}