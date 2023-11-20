const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const cc = @import("clib.zig");
const rt = cy.rt;
const sema = cy.sema;
const fmt = @import("fmt.zig");
const v = fmt.v;
const vmc = @import("vm_c.zig");
const log = cy.log.scoped(.types);

pub const TypeId = u32;

pub const Type = struct {
    sym: *cy.Sym,
    // Duped to avoid lookup from `sym`.
    symType: cy.sym.SymType,
    data: union {
        uninit: void,
        // This is duped from ObjectType so that object creation/destruction avoids the lookup from `sym`.
        numFields: u16,

        // Even though this increases the size of other type entries, it might not be worth
        // separating into another table since it would add another indirection.
        hostObject: struct {
            getChildrenFn: cc.ObjectGetChildrenFn,
            finalizerFn: cc.ObjectFinalizerFn,
        },
    },
};

pub const CompactType = packed struct {
    /// Should always be a static typeId.
    id: u31,
    dynamic: bool,

    pub fn init(id: TypeId) CompactType {
        if (id == bt.Dynamic) {
            return CompactType.initDynamic(bt.Any);
        } else {
            return CompactType.initStatic(id);
        }
    }

    pub fn init2(id: TypeId, dynamic: bool) CompactType {
        return .{
            .id = @intCast(id),
            .dynamic = dynamic,
        };
    }

    pub fn initStatic(id: TypeId) CompactType {
        return .{ .id = @intCast(id), .dynamic = false };
    }

    pub fn initDynamic(id: TypeId) CompactType {
        return .{ .id = @intCast(id), .dynamic = true };
    }

    pub fn toDeclType(self: CompactType) TypeId {
        if (self.dynamic) {
            return bt.Dynamic;
        } else {
            return self.id;
        }
    }

    pub fn toStaticDeclType(self: CompactType) TypeId {
        if (self.dynamic) {
            return bt.Any;
        } else {
            return self.id;
        }
    }
};

pub const PrimitiveEnd: TypeId = vmc.PrimitiveEnd;

const bt = BuiltinTypes;
pub const BuiltinTypes = struct {
    pub const Any: TypeId = vmc.TYPE_ANY;
    pub const Boolean: TypeId = vmc.TYPE_BOOLEAN;
    pub const Placeholder1: TypeId = vmc.TYPE_PLACEHOLDER1;
    pub const Placeholder2: TypeId = vmc.TYPE_PLACEHOLDER2;
    pub const Placeholder3: TypeId = vmc.TYPE_PLACEHOLDER3;
    pub const Float: TypeId = vmc.TYPE_FLOAT;
    pub const Integer: TypeId = vmc.TYPE_INTEGER;
    pub const String: TypeId = vmc.TYPE_STRING;
    pub const Array: TypeId = vmc.TYPE_ARRAY;
    pub const Symbol: TypeId = vmc.TYPE_SYMBOL;
    pub const Tuple: TypeId = vmc.TYPE_TUPLE;
    pub const List: TypeId = vmc.TYPE_LIST;
    pub const ListIter: TypeId = vmc.TYPE_LIST_ITER;
    pub const Map: TypeId = vmc.TYPE_MAP;
    pub const MapIter: TypeId = vmc.TYPE_MAP_ITER;
    pub const Pointer: TypeId = vmc.TYPE_POINTER;
    pub const None: TypeId = vmc.TYPE_NONE;
    pub const Error: TypeId = vmc.TYPE_ERROR;
    pub const Fiber: TypeId = vmc.TYPE_FIBER;
    pub const MetaType: TypeId = vmc.TYPE_METATYPE;
    pub const Closure: TypeId = vmc.TYPE_CLOSURE;
    pub const Lambda: TypeId = vmc.TYPE_LAMBDA;
    pub const Box: TypeId = vmc.TYPE_BOX;
    pub const HostFunc: TypeId = vmc.TYPE_HOST_FUNC;
    pub const TccState: TypeId = vmc.TYPE_TCC_STATE;

    /// Used to indicate no type value.
    // pub const Undefined: TypeId = vmc.TYPE_UNDEFINED;

    /// A dynamic type does not have a static type.
    /// This is not the same as bt.Any which is a static type.
    pub const Dynamic: TypeId = vmc.TYPE_DYNAMIC;

    // pub const End: TypeId = vmc.NumSemaTypes;
};

pub const SemaExt = struct {

    pub fn pushType(s: *cy.Sema) !TypeId {
        const typeId = s.types.items.len;
        try s.types.append(s.alloc, .{
            .sym = undefined,
            .symType = .uninit,
            .data = .{ .uninit = {}},
        });
        return @intCast(typeId);
    }

    pub fn getTypeName(s: *cy.Sema, id: TypeId) []const u8 {
        return s.types.items[id].sym.name();
    }

    pub fn getTypeSym(s: *cy.Sema, id: TypeId) *cy.Sym {
        return s.types.items[id].sym;
    }

    pub fn isEnumType(s: *cy.Sema, typeId: TypeId) bool {
        if (typeId < PrimitiveEnd) {
            return false;
        }
        log.tracev("{}", .{s.types.items[typeId].symType});
        return s.types.items[typeId].symType == .enumType;
    }

    pub fn isRcCandidateType(s: *cy.Sema, id: TypeId) bool {
        switch (id) {
            bt.String,
            bt.Array,
            bt.List,
            bt.ListIter,
            bt.Map,
            bt.MapIter,
            bt.Pointer,
            bt.Fiber,
            bt.MetaType,
            bt.Dynamic,
            bt.Any => return true,
            bt.Integer,
            bt.Float,
            bt.Symbol,
            bt.None,
            bt.Error,
            // bt.Undefined,
            bt.Boolean => return false,
            else => {
                const sym = s.getTypeSym(id);
                switch (sym.type) {
                    .hostObjectType,
                    .object => return true,
                    .enumType => return false,
                    else => {
                        cy.panicFmt("Unexpected sym type: {} {}", .{id, sym.type});
                    }
                }
            }
        }
    }
};

pub const ChunkExt = struct {

    pub fn checkForZeroInit(c: *cy.Chunk, typeId: TypeId, nodeId: cy.NodeId) !void {
        var res = hasZeroInit(c, typeId);
        if (res == .missingEntry) {
            const sym = c.sema.getTypeSym(typeId);
            res = try visitTypeHasZeroInit(c, sym.cast(.object));
        }
        switch (res) {
            .hasZeroInit => return,
            .missingEntry => cy.unexpected(),
            .unsupported => {
                const name = c.sema.getTypeName(typeId);
                return c.reportErrorAt("Unsupported zero initializer for `{}`.", &.{v(name)}, nodeId);
            },
            .circularDep => {
                const name = c.sema.getTypeName(typeId);
                return c.reportErrorAt("Can not zero initialize `{}` because of circular dependency.", &.{v(name)}, nodeId);
            }
        }
    }
};

pub fn isAnyOrDynamic(id: TypeId) bool {
    return id == bt.Any or id == bt.Dynamic;
}

/// Check type constraints on target func signature.
pub fn isTypeFuncSigCompat(c: *cy.VMcompiler, args: []const CompactType, ret: TypeId, targetId: sema.FuncSigId) bool {
    const target = c.sema.getFuncSig(targetId);
    if (cy.Trace) {
        const sigStr = c.sema.formatFuncSig(targetId, &cy.tempBuf) catch cy.fatal();
        log.tracev("matching against: {s}", .{sigStr});
    }

    // First check params length.
    if (args.len != target.paramLen) {
        return false;
    }

    // Check each param type. Attempt to satisfy constraints.
    for (target.params(), args) |cstrType, argType| {
        if (isTypeSymCompat(c, argType.id, cstrType)) {
            continue;
        }
        if (argType.dynamic) {
            continue;
        }
        log.tracev("`{s}` not compatible with param `{s}`", .{c.sema.getTypeName(argType.id), c.sema.getTypeName(cstrType)});
        return false;
    }

    // Check return type. Target is the source return type.
    return isTypeSymCompat(c, target.ret, ret);
}

pub fn isTypeSymCompat(_: *cy.VMcompiler, typeId: TypeId, cstrType: TypeId) bool {
    if (typeId == cstrType) {
        return true;
    }
    if (cstrType == bt.Any or cstrType == bt.Dynamic) {
        return true;
    }
    return false;
}

/// Check type constraints on target func signature.
pub fn isFuncSigCompat(c: *cy.VMcompiler, id: sema.FuncSigId, targetId: sema.FuncSigId) bool {
    const src = c.sema.getFuncSig(id);
    const target = c.sema.getFuncSig(targetId);

    // First check params length.
    if (src.paramLen != target.paramLen) {
        return false;
    }

    // Check each param type. Attempt to satisfy constraints.
    for (target.params(), src.params()) |cstsymId, typeSymId| {
        if (!isTypeSymCompat(c, typeSymId, cstsymId)) {
            return false;
        }
    }

    // Check return type. Source return type is the constraint.
    return isTypeSymCompat(c, target.retSymId, src.retSymId);
}

pub fn toRtConcreteType(typeId: TypeId) ?cy.TypeId {
    return switch (typeId) {
        bt.Dynamic,
        bt.Any => null,
        else => return typeId,
    };
}

pub fn typeEqualOrChildOf(a: TypeId, b: TypeId) bool {
    if (b == bt.Any) {
        return true;
    }
    if (a == b) {
        return true;
    }
    // TODO: Check if a is a child type of b.
    return false;
}

pub fn isSameType(t1: TypeId, t2: TypeId) bool {
    return t1 == t2;
}

pub fn unionOf(c: *cy.VMcompiler, a: TypeId, b: TypeId) TypeId {
    _ = c;
    if (a == b) {
        return a;
    } else {
        if (a == bt.Dynamic or b == bt.Dynamic) {
            return bt.Dynamic;
        } else {
            return bt.Any;
        }
    }
}

const ZeroInitResult = enum {
    hasZeroInit,
    missingEntry,
    unsupported,
    circularDep,
};

fn hasZeroInit(c: *cy.Chunk, typeId: TypeId) ZeroInitResult {
    switch (typeId) {
        bt.Dynamic,
        bt.Any,
        bt.Boolean,
        bt.Integer,
        bt.Float,
        bt.List,
        bt.Map,
        bt.Array,
        bt.String => return .hasZeroInit,
        else => {
            const sym = c.sema.getTypeSym(typeId);
            if (sym.type == .object) {
                if (c.typeDepsMap.get(sym)) |entryId| {
                    if (!c.typeDeps.items[entryId].visited) {
                        // Still being visited, which indicates a circular reference.
                        return .circularDep;
                    }
                    if (c.typeDeps.items[entryId].hasCircularDep) {
                        return .circularDep;
                    }
                    if (c.typeDeps.items[entryId].hasUnsupported) {
                        return .unsupported;
                    }
                    return .hasZeroInit;
                } else {
                    return .missingEntry;
                }
            }
            return .unsupported;
        },
    }
}

fn visitTypeHasZeroInit(c: *cy.Chunk, obj: *cy.sym.ObjectType) !ZeroInitResult {
    const entryId = c.typeDeps.items.len;
    try c.typeDeps.append(c.alloc, .{ .visited = false, .hasCircularDep = false, .hasUnsupported = false });
    try c.typeDepsMap.put(c.alloc, @ptrCast(obj), @intCast(entryId));

    var finalRes = ZeroInitResult.hasZeroInit;
    for (obj.fields[0..obj.numFields]) |field| {
        var res = hasZeroInit(c, field.type);
        if (res == .missingEntry) {
            const childSym = c.sema.getTypeSym(field.type).cast(.object);
            res = try visitTypeHasZeroInit(c, childSym);
        }
        switch (res) {
            .hasZeroInit => continue,
            .missingEntry => cy.unexpected(),
            .unsupported => {
                if (finalRes == .hasZeroInit) {
                    finalRes = .unsupported;
                }
            },
            .circularDep => {
                if (finalRes == .hasZeroInit) {
                    finalRes = .circularDep;
                }
            },
        }
    }

    if (finalRes == .circularDep) {
        c.typeDeps.items[entryId].hasCircularDep = true;
    } else if (finalRes == .unsupported) {
        c.typeDeps.items[entryId].hasUnsupported = true;
    }
    c.typeDeps.items[entryId].visited = true;
    return finalRes;
}