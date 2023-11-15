const std = @import("std");
const builtin = @import("builtin");
const cy = @import("cyber.zig");
const vmc = cy.vmc;
const bt = cy.types.BuiltinTypes;
const stdx = @import("stdx");
const t = stdx.testing;
const log = cy.log.scoped(.sym);
const fmt = cy.fmt;
const v = fmt.v;

const Metadata = packed struct {
    isNameOwned: bool,
    padding: u15,
};

/// Base symbol. All symbols stem from the same header.
/// Some symbols can contain their own `cy.Module`.
pub const Sym = extern struct {
    parent: ?*Sym align(8),
    type: SymType,
    metadata: u16,
    nameLen: u16,
    namePtr: [*]const u8,

    pub fn init(symT: SymType, parent: ?*Sym, name_: []const u8) Sym {
        return .{
            .namePtr = name_.ptr,
            .nameLen = @intCast(name_.len),
            .metadata = @bitCast(Metadata{ .padding = undefined, .isNameOwned = false }),
            .type = symT,
            .parent = parent,
        };
    }

    pub fn deinit(self: *Sym, alloc: std.mem.Allocator) void {
        if (self.getMetadata().isNameOwned) {
            alloc.free(self.name());
        }
    }

    pub fn getMetadata(self: Sym) Metadata {
        return @bitCast(self.metadata);
    }

    pub fn setNameOwned(self: *Sym, nameOwned: bool) void {
        var metadata = self.getMetadata();
        metadata.isNameOwned = nameOwned;
        self.metadata = @bitCast(metadata);
    }

    pub fn isVariable(self: Sym) bool {
        return self.type == .userVar or self.type == .hostVar;
    }

    pub fn isType(self: Sym) bool {
        return self.type == .object or self.type == .enumType;
    }

    pub fn name(self: Sym) []const u8 {
        return self.namePtr[0..self.nameLen];
    }

    // TODO: Rename to `as`.
    pub fn cast(self: *Sym, comptime symT: SymType) *SymChild(symT) {
        if (cy.Trace) {
            if (symT != self.type) {
                cy.panicFmt("Invalid sym cast from {} to {}", .{self.type, symT});
            }
        }
        return @ptrCast(self);
    }

    pub fn isDistinct(self: *Sym) bool {
        return self.type != .func or self.cast(.func).numFuncs == 1;
    }

    pub fn getMod(self: *Sym) ?*cy.Module {
        switch (self.type) {
            .chunk          => return @ptrCast(&self.cast(.chunk).mod),
            .enumType       => return @ptrCast(&self.cast(.enumType).mod),
            .object         => return @ptrCast(&self.cast(.object).mod),
            .hostObjectType => return @ptrCast(&self.cast(.hostObjectType).mod),
            .predefinedType => return @ptrCast(&self.cast(.predefinedType).mod),
            .import         => return self.cast(.import).sym.getMod(),
            .typeAlias,
            .enumMember,
            .func,
            .userVar,
            .field,
            .hostVar => {
                return null;
            },
            .uninit => cy.unexpected(),
        }
    }

    pub fn getRootMod(self: *Sym) *cy.Module {
        if (self.parent) |parent| {
            return parent.getRootMod();
        } else {
            return self.getMod().?;
        }
    }

    pub fn getFirstFunc(self: *Sym) ?*Func {
        switch (self.type) {
            .func => {
                return self.cast(.func).first;
            },
            else => return null,
        }
    }

    pub fn getValueType(self: *Sym) !cy.TypeId {
        switch (self.type) {
            .userVar    => return self.cast(.userVar).type,
            .hostVar    => return self.cast(.hostVar).type,
            .enumMember => return self.cast(.enumMember).type,
            .predefinedType,
            .typeAlias,
            .object     => return bt.MetaType,
            .func => {
                const func = self.cast(.func);
                if (func.numFuncs == 1) {
                    return getFuncType(func.first);
                } else {
                    return error.AmbiguousSymbol;
                }
            },
            else => return bt.Any,
        }
    }

    pub fn getStaticType(self: *Sym) ?cy.TypeId {
        switch (self.type) {
            .predefinedType => return self.cast(.predefinedType).type,
            .enumType       => return self.cast(.enumType).type,
            .object         => return self.cast(.object).type,
            .hostObjectType => return self.cast(.hostObjectType).type,
            else            => return null,
        }
    }

    pub fn resolved(self: *Sym) *Sym {
        if (self.type == .import) {
            return self.cast(.import).sym;
        } else {
            return self;
        }
    }

    pub fn formatAbsPath(self: *const Sym, buf: []u8) ![]const u8 {
        var fbuf = std.io.fixedBufferStream(buf);
        try self.writeAbsPath(fbuf.writer());
        return fbuf.getWritten();
    }

    pub fn allocAbsPath(self: *const Sym, alloc: std.mem.Allocator) ![]const u8 {
        const parent = self.parent orelse {
            return alloc.dupe(u8, self.name());
        };

        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(alloc);
        const w = buf.writer(alloc);
        try parent.writeAbsPath(w);
        try buf.append(alloc, '.');
        try buf.appendSlice(alloc, self.name());
        return buf.toOwnedSlice(alloc);
    }

    fn writeAbsPath(self: *const Sym, w: anytype) !void {
        const parent = self.parent orelse {
            try w.writeAll(self.name());
            return;
        };
        try parent.writeAbsPath(w);
        try w.writeByte('.');
        try w.writeAll(self.name());
    }

    pub fn dump(self: *Sym, opts: SymDumpOptions) !void {
        if (!cy.Trace) return;
        const path = try self.writeAbsPathToBuf(&cy.tempBuf);
        fmt.printStderr("{}{} {}\n", &.{fmt.repeat(' ', opts.indent), v(path), v(@tagName(self.type))});
        if (opts.dumpModule) {
            if (self.getMod()) |mod| {
                for (mod.syms.items) |childSym| {
                    try childSym.dump(.{ .indent = 2, .dumpModule = false });
                }
            }
        }
    }
};

const SymDumpOptions = struct {
    indent: u32 = 0,
    dumpModule: bool = false,
};

/// Assumes sym contains a module.
pub fn deinitModSym(vm: *cy.VM, modSym: *Sym) void {
    const alloc = vm.alloc;
    const mod = modSym.getMod().?;
    mod.deinit(alloc);

    switch (modSym.type) {
        .object => {
            const obj = modSym.cast(.object);
            alloc.free(obj.fields[0..obj.numFields]);
        },
        .enumType => {
            const enumType = modSym.cast(.enumType);
            alloc.free(enumType.members[0..enumType.numMembers]);
        },
        else => {},
    }
}

pub fn createSym(alloc: std.mem.Allocator, comptime symT: SymType, init: SymChild(symT)) !*SymChild(symT) {
    const sym = try alloc.create(SymChild(symT));
    sym.* = init;
    return sym;
}

fn getFuncType(func: *Func) cy.TypeId {
    _ = func;
    return bt.Any;
}

fn SymChild(comptime symT: SymType) type {
    return switch (symT) {
        .func => FuncSym,
        .userVar => UserVar,
        .hostVar => HostVar,
        .object => ObjectType,
        .hostObjectType => HostObjectType,
        .predefinedType => PredefinedType,
        .enumType => EnumType,
        .enumMember => EnumMember,
        .typeAlias => TypeAlias,
        .field => Field,
        .import => Import,
        .chunk => Chunk,
        .uninit => void,
    };
}

pub const SymType = enum(u8) {
    userVar,
    hostVar,
    func,
    hostObjectType,
    // TODO: Rename to objectType.
    object,
    predefinedType,
    import,
    chunk,
    enumType,
    enumMember,
    typeAlias,
    field,

    // No sym.
    uninit,
};

pub const HostVar = extern struct {
    head: Sym,
    declId: cy.Nullable(cy.NodeId),
    type: cy.TypeId,
    val: cy.Value,

    // Index into `retainedVars`.
    retainedIdx: u16,
};

pub const UserVar = extern struct {
    head: Sym,
    declId: cy.NodeId,
    type: cy.TypeId,
};

/// Sym that links to overloaded functions.
pub const FuncSym = extern struct {
    head: Sym,
    first: *Func,
    last: *Func,
    numFuncs: u16,

    /// Duped to perform uniqueness check without dereferencing the first ModuleFunc.
    firstFuncSig: cy.sema.FuncSigId,
};

/// Type aliases are lazily loaded.
const TypeAlias = extern struct {
    head: Sym,
    declId: cy.NodeId,
    type: cy.TypeId,
    sym: *Sym,
};

pub const Field = extern struct {
    head: Sym,
    idx: u32,
    type: cy.TypeId,
};

pub const FieldInfo = packed struct {
    symId: cy.module.ModuleSymId,
    type: cy.TypeId,
};

pub const ObjectType = extern struct {
    head: Sym,
    type: cy.TypeId,
    declId: cy.NodeId,
    fields: [*]const FieldInfo,
    numFields: u32,
    mod: vmc.Module,

    pub fn getMod(self: *ObjectType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const HostObjectType = extern struct {
    head: Sym,
    type: cy.TypeId,
    declId: cy.NodeId,
    getChildrenFn: ?cy.ObjectGetChildrenFn,
    finalizerFn: ?cy.ObjectFinalizerFn,
    mod: vmc.Module,
};

pub const PredefinedType = extern struct {
    head: Sym,
    type: cy.TypeId,
    mod: vmc.Module,
};

pub const EnumType = extern struct {
    head: Sym,
    type: cy.TypeId,
    members: [*]const cy.module.ModuleSymId,
    numMembers: u32,
    mod: vmc.Module,

    pub fn getValueSym(self: *EnumType, val: u16) *Sym {
        const symId = self.members[val];
        return self.getMod().syms.items[symId];
    }

    pub fn getMod(self: *EnumType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const EnumMember = extern struct {
    head: Sym,
    type: cy.TypeId,
    val: u32,
};

const Import = extern struct {
    head: Sym,
    declId: cy.NodeId,
    sym: *Sym,
};

pub const Chunk = extern struct {
    head: Sym,
    mod: vmc.Module,

    pub fn getMod(self: *Chunk) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const FuncType = enum {
    hostFunc,
    hostInlineFunc,
    userFunc,
    userLambda,
};

pub const Func = struct {
    type: FuncType,
    next: ?*Func,
    sym: ?*FuncSym,
    funcSigId: cy.sema.FuncSigId,
    numParams: u8,
    isMethod: bool,
    declId: cy.NodeId,
    retType: cy.TypeId,
    data: union {
        hostFunc: struct {
            ptr: cy.ZHostFuncFn,
        },
        hostInlineFunc: struct {
            ptr: cy.InlineFuncFn,
        },
    },

    pub fn isGenerated(self: Func) bool {
        return self.declId == cy.NullId;
    }

    pub fn isStatic(self: Func) bool {
        return self.type != .userLambda;
    }

    pub fn hasStaticInitializer(self: Func) bool {
        return self.type == .hostFunc;
    }

    pub fn name(self: Func) []const u8 {
        if (self.type == .userLambda) {
            return "lambda";
        }
        return self.sym.?.head.name();
    }
};

test "sym internals" {
    if (builtin.mode == .ReleaseFast) {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(Sym), 16);
            try t.eq(@sizeOf(Func), 28);
        } else {
            try t.eq(@sizeOf(Sym), 24);
            try t.eq(@sizeOf(Func), 40);
        }
    } else {
        try t.eq(@sizeOf(Sym), 24);
        if (cy.is32Bit) {
            try t.eq(@sizeOf(Func), 8);
        } else {
            try t.eq(@sizeOf(Func), 48);
        }
    }

    try t.eq(@offsetOf(Sym, "type"), @offsetOf(vmc.SemaSym, "type"));
    try t.eq(@offsetOf(Sym, "parent"), @offsetOf(vmc.SemaSym, "parent"));
    try t.eq(@offsetOf(Sym, "namePtr"), @offsetOf(vmc.SemaSym, "namePtr"));
    try t.eq(@offsetOf(Sym, "nameLen"), @offsetOf(vmc.SemaSym, "nameLen"));
    try t.eq(@offsetOf(Sym, "metadata"), @offsetOf(vmc.SemaSym, "metadata"));
}