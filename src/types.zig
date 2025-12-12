const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const rt = cy.rt;
const ir = cy.ir;
const sema = cy.sema;
const fmt = @import("fmt.zig");
const v = fmt.v;
const vmc = @import("vmc");
const log = cy.log.scoped(.types);
const ast = cy.ast;

pub const TypeId = u32;

pub const TypeKind = enum(u8) {
    null = vmc.TYPE_KIND_NULL,
    bool = vmc.TYPE_KIND_BOOL,
    int = vmc.TYPE_KIND_INT,
    raw = vmc.TYPE_KIND_RAW,
    float = vmc.TYPE_KIND_FLOAT,
    enum_t = vmc.TYPE_KIND_ENUM,
    choice = vmc.TYPE_KIND_CHOICE,
    struct_t = vmc.TYPE_KIND_STRUCT,
    option = vmc.TYPE_KIND_OPTION,
    generic_trait = vmc.TYPE_KIND_GENERIC_TRAIT,
    bare = vmc.TYPE_KIND_BARE,
    vector = vmc.TYPE_KIND_VECTOR,
    partial_vector = vmc.TYPE_KIND_PARTIAL_VECTOR,
    func_ptr = vmc.TYPE_KIND_FUNC_PTR,
    func = vmc.TYPE_KIND_FUNC,
    func_sym = vmc.TYPE_KIND_FUNC_SYM,
    pointer = vmc.TYPE_KIND_PTR,

    // An `eval_ref` is an rc type used during const eval that is always 8 bytes regardless of the compilation target.
    eval_ref = vmc.TYPE_KIND_EVAL_REF,
    borrow = vmc.TYPE_KIND_BORROW,
    void = vmc.TYPE_KIND_VOID,
    generic = vmc.TYPE_KIND_GENERIC,
    result = vmc.TYPE_KIND_RESULT,
    ref_trait = vmc.TYPE_KIND_REF_TRAIT,
    borrow_trait = vmc.TYPE_KIND_BORROW_TRAIT,
    eval_int = vmc.TYPE_KIND_EVAL_INT,
    generic_vector = vmc.TYPE_KIND_GENERIC_VECTOR,
    never = vmc.TYPE_KIND_NEVER,
    c_variadic = vmc.TYPE_KIND_CVARIADIC,
    c_union = vmc.TYPE_KIND_CUNION,
    ex_borrow = vmc.TYPE_KIND_EXBORROW,
    dyn_trait = vmc.TYPE_KIND_DYN_TRAIT,
};

pub const TypeInfo = packed struct {
    /// Type can only be used at compile-time.
    ct: bool = false,

    /// Type is a borrow or contains a borrow reference.
    borrow_only: bool = false,

    managed: bool = false,

    /// Relys on builtin `@copy` (not considering a user defined @copy) instead of a memcpy.
    copy_base: bool = false,

    /// Has a user defined `@copy`.
    /// When `copy_ctor and !copy_user`, the compiler can reduce a `@copy` call to a `retain` for example.
    copy_user: bool = false,

    /// Fully resolved. Memory layout and copy semantics have been determined.
    resolved: bool = false,

    /// Either implements `NoCopy` or has a member that implements `NoCopy`.
    no_copy: bool = false,

    /// Must be determined without resolving the type.
    generic: bool = false,
    
    padding: u8 = 0,
};

pub const Type = extern struct {
    kind_: TypeKind,
    sym_: *cy.sym.TypeSym,
    id_: TypeId,
    info: TypeInfo,

    /// If `null`, the destructor is a no-op.
    dtor: ?*cy.Func = null,

    deinit_obj: ?*cy.Func = null,

    /// If `null`, the copy constructor is a memcpy.
    copy_ctor: ?*cy.Func = null,

    pub fn id(self: *Type) TypeId {
        return self.id_;
    }

    pub fn kind(self: *Type) TypeKind {
        return self.kind_;
    }

    pub fn name(self: *Type) []const u8 {
        return self.sym_.head.name();
    }

    pub fn canCCast(self: *Type) bool {
        switch (self.kind()) {
            .option => {
                const option = self.cast(.option);
                return option.zero_union;
            },
            .float,
            .bool,
            .raw,
            .int,
            .pointer => return true,
            else => return false,
        }
    }

    pub fn has_embeddings(self: *Type) bool {
        return self.kind() == .struct_t and self.cast(.struct_t).hasEmbeddings();
    }

    pub fn isCZeroEligible(self: *Type) bool {
        switch (self.id()) {
            else => {
                switch (self.kind()) {
                    .raw,
                    .int => {
                        return true;
                    },
                    .struct_t => {
                        return self.cast(.struct_t).cstruct;
                    },
                    else => {},
                }
            }
        }
        return false;
    }

    /// What is const eligible is a subset of what is eval eligible.
    /// The type must be immutable.
    pub fn isConstEligible(self: *Type) bool {
        switch (self.id()) {
            bt.I8,
            bt.I16,
            bt.I32,
            bt.I64,
            bt.R8,
            bt.R16,
            bt.R32,
            bt.R64,
            bt.Bool,
            bt.F64,
            bt.F32,
            bt.EvalStr => {
                return true;
            },
            else => {},
        }
        if (self.isManaged()) {
            return false;
        }
        switch (self.kind()) {
            .pointer => {
                const pointer = self.cast(.pointer);
                return !pointer.ref;
            },
            .struct_t => {
                // Allow containers if they are not managed and all members are const eligible.
                // TODO: Prevent method calling that are by pass by mutable receiver.
                // TODO: Prevent mutation to any inner field.
                std.debug.assert(self.isResolved());
                const struct_t = self.cast(.struct_t);
                for (struct_t.fields()) |field| {
                    if (!field.type.isConstEligible()) {
                        return false;
                    }
                }
                return true;
            },
            else => {
                return false;
            },
        }
    }

    pub fn isPrimitive(self: *Type) bool {
        switch (self.kind()) {
            .pointer => {
                return !self.cast(.pointer).ref;
            },
            .eval_ref,
            .ref_trait,
            .func,
            .result,
            .choice,
            .vector,
            .partial_vector,
            .generic_trait,
            .struct_t => {
                return false;
            },
            .option => {
                const option = self.cast(.option);
                return option.zero_union and option.child_t.isPrimitive();
            },
            else => {
                return true;
            }
        }
    }

    pub fn reg_size(self: *Type) usize {
        return (self.size() + 7) >> 3;
    }

    /// Whether `cy.Value` is boxed.
    pub fn is_cte_boxed(self: *Type) bool {
        switch (self.kind()) {
            .borrow_trait,
            .result,
            .choice,
            .vector,
            .partial_vector,
            .c_union,
            .struct_t => {
                return true;
            },
            .option => {
                return !self.cast(.option).zero_union;
            },
            else => {
                return false;
            }
        }
    }

    pub fn isVmManagedRef(self: *Type) bool {
        switch (self.kind()) {
            .pointer => {
                return self.cast(.pointer).ref;
            },
            .func,
            .choice,
            .result,
            .vector,
            .partial_vector,
            .struct_t => {
                return true;
            },
            .option => {
                const option_t = self.cast(.option);
                if (!option_t.zero_union) {
                    return true;
                }
                return option_t.child_t.isRefPointer();
            },
            else => {
                return false;
            }
        }
    }

    pub fn isRefLike(self: *Type) bool {
        switch (self.kind()) {
            .borrow,
            .ex_borrow,
            .pointer => {
                return true;
            },
            else => {
                return false;
            }
        }
    }

    pub fn isObject(self: *Type) bool {
        return self.id() == bt.Object;
    }

    pub fn isObjectLike(self: *Type) bool {
        switch (self.kind()) {
            .pointer => {
                return self.cast(.pointer).ref;
            },
            .eval_ref,
            .func => {
                return true;
            },
            else => {
                return false;
            }
        }
    }

    pub fn is_generic(self: *Type) bool {
        return self.info.generic;
    }

    pub fn is_stack_ptr(self: *Type) bool {
        switch (self.kind()) {
            .pointer => {
                return !self.cast(.pointer).ref;
            },
            .borrow => return true,
            else => {
                return false;
            },
        }
    }

    pub fn is_min_resolved(self: *Type) bool {
        return self.info.shallow_resolved;
    }

    pub fn isResolved(self: *Type) bool {
        return self.info.resolved;
    }

    pub fn isManaged(self: *Type) bool {
        if (cy.Trace) {
            if (!self.info.resolved) {
                std.debug.panic("Expected resolved type {s}.", .{self.name()});
            }
        }
        return self.info.managed;
    }

    pub fn has_copy_ctor(self: *Type) bool {
        if (cy.Trace) {
            if (!self.info.resolved) {
                std.debug.panic("Expected resolved type {s}.", .{self.name()});
            }
        }
        return self.info.copy_base or self.info.copy_user;
    }

    pub fn is_sendable(self: *Type) bool {
        switch (self.kind()) {
        //     .struct_t => return self.cast(.struct_t).fields(),
        //     .c_union => return self.cast(.c_union).fields(),
        //     .choice => return self.cast(.choice).fields(),
        //     .option => return self.cast(.option).fields(),
        //     .result => return self.cast(.result).fields(),
        //     else => return null, 
            .int => return true,
            else => return false,
        }
    }

    pub fn is_copyable(self: *Type) bool {
        return self.hasCopyCtor() or !self.info.no_copy;
    }

    pub fn hasCopyCtor(self: *Type) bool {
        if (cy.Trace) {
            if (!self.info.resolved) {
                std.debug.panic("Expected resolved type {s}.", .{self.name()});
            }
        }
        return self.info.copy_base or self.info.copy_user;
    }

    pub fn isCStruct(self: *Type) bool {
        return self.kind() == .struct_t and self.cast(.struct_t).cstruct;
    }

    pub fn pointeeOrSelf(self: *Type) *Type {
        if (self.kind() == .pointer) {
            return self.cast(.pointer).child_t;
        } else if (self.kind() == .borrow) {
            return self.cast(.borrow).child_t;
        } else if (self.kind() == .ex_borrow) {
            return self.cast(.ex_borrow).child_t;
        } else {
            return self;
        }
    }

    pub fn alignment(self: *Type) u8 {
        switch (self.kind()) {
            .c_union => return self.cast(.c_union).alignment,
            .struct_t => return self.cast(.struct_t).alignment,
            .result => return self.cast(.result).alignment,
            .option => return self.cast(.option).alignment,
            .choice => return self.cast(.choice).alignment,
            .vector => return self.cast(.vector).elem_t.alignment(),
            .partial_vector => return 8,
            .raw => return @intCast(self.cast(.raw).bits / 8),
            .int => return @intCast(self.cast(.int).bits / 8),
            .float => return @intCast(self.cast(.float).bits / 8),
            .eval_ref => return 8,
            .pointer => return @sizeOf(usize),
            .borrow => return 8,
            .ex_borrow => return 8,
            .borrow_trait,
            .ref_trait => return 8,
            .bool => return 1,
            .enum_t => return 8,
            .never => return 1,
            .void => return 1,
            .func_ptr => return 8,
            .func => return 8,
            .generic => return 1,
            else => std.debug.panic("TODO: {} size", .{self.kind()}),
        }
    }

    /// Returns the size in bytes.
    pub fn size(self: *Type) usize {
        switch (self.kind()) {
            .c_union => return self.cast(.c_union).size,
            .struct_t => return self.cast(.struct_t).size,
            .result => return self.cast(.result).size,
            .option => return self.cast(.option).size,
            .choice => return self.cast(.choice).size,
            .vector => return @intCast(self.cast(.vector).size),
            .partial_vector => return @intCast(self.cast(.partial_vector).size),
            .raw => return self.cast(.raw).bits / 8,
            .int => return self.cast(.int).bits / 8,
            .float => return self.cast(.float).bits / 8,
            .eval_ref => return 8,

            // TODO: Size should be determined during resolving which considers the compilation target.
            .pointer => return @sizeOf(usize),
            .borrow => return @sizeOf(usize),
            .ex_borrow => return @sizeOf(usize),
            .borrow_trait,
            .ref_trait => return 16,
            .bool => return 1,
            .enum_t => return 8,
            .never => return 0,
            .void => return 0,
            .func_ptr => return 8,
            .func => return 8,
            .generic => return 0,
            else => std.debug.panic("TODO: {} size", .{self.kind()}),
        }
    }

    // pub fn wordSize(self: *Type) usize {
    //     return std.math.divCeil(usize, self.size(), 8) catch unreachable;
    // }

    pub fn sym(self: *Type) *cy.sym.TypeSym {
        if (cy.Trace) {
            if (self.kind() == .null) {
                @panic("Null type does not have a symbol.");
            }
        }
        return self.sym_;
    }

    pub fn cast(self: *Type, comptime tkind: TypeKind) *TypeImpl(tkind) {
        if (cy.Trace) {
            if (self.kind() != tkind) {
                std.debug.panic("Expected {}, found {}.", .{tkind, self.kind()});
            }
        }
        return @ptrCast(@alignCast(self));
    }

    pub fn destroy(self: *Type, alloc: std.mem.Allocator) void {
        switch (self.kind()) {
            .null => {},
            .choice => {
                const impl = self.cast(.choice);
                impl.deinit(alloc);
                alloc.destroy(impl);
            },
            .c_union => {
                const impl = self.cast(.c_union);
                impl.deinit(alloc);
                alloc.destroy(impl);
            },
            .enum_t => {
                const impl = self.cast(.enum_t);
                impl.deinit(alloc);
                alloc.destroy(impl);
            },
            .struct_t => {
                const impl = self.cast(.struct_t);
                impl.deinit(alloc);
                alloc.destroy(impl);
            },
            .dyn_trait => {
                const impl = self.cast(.dyn_trait);
                alloc.destroy(impl);
            },
            .ref_trait => {
                const impl = self.cast(.ref_trait);
                alloc.destroy(impl);
            },
            .borrow_trait => {
                const impl = self.cast(.borrow_trait);
                alloc.destroy(impl);
            },
            .generic_trait => {
                const impl = self.cast(.generic_trait);
                impl.deinit(alloc);
                alloc.destroy(impl);
            },
            .pointer => {
                const impl = self.cast(.pointer);
                alloc.destroy(impl);
            },
            .eval_ref => {
                const impl = self.cast(.eval_ref);
                alloc.destroy(impl);
            },
            .borrow => {
                const impl = self.cast(.borrow);
                alloc.destroy(impl);
            },
            .ex_borrow => {
                const impl = self.cast(.ex_borrow);
                alloc.destroy(impl);
            },
            .eval_int => {
                const impl = self.cast(.eval_int);
                alloc.destroy(impl);
            },
            .c_variadic => {
                const impl = self.cast(.c_variadic);
                alloc.destroy(impl);
            }, 
            .func => {
                const impl = self.cast(.func);
                alloc.destroy(impl);
            },
            .func_sym => {
                const impl = self.cast(.func_sym);
                alloc.destroy(impl);
            },
            .func_ptr => {
                const impl = self.cast(.func_ptr);
                alloc.destroy(impl);
            },
            .generic => {
                const impl = self.cast(.generic);
                alloc.destroy(impl);
            },
            .void => {
                const impl = self.cast(.void);
                alloc.destroy(impl);
            },
            .never => {
                const impl = self.cast(.never);
                alloc.destroy(impl);
            },
            .bool => {
                const impl = self.cast(.bool);
                alloc.destroy(impl);
            },
            .raw => {
                const impl = self.cast(.raw);
                alloc.destroy(impl);
            },
            .int => {
                const impl = self.cast(.int);
                alloc.destroy(impl);
            },
            .float => {
                const impl = self.cast(.float);
                alloc.destroy(impl);
            },
            .partial_vector => {
                const impl = self.cast(.partial_vector);
                alloc.destroy(impl);
            },
            .vector => {
                const impl = self.cast(.vector);
                alloc.destroy(impl);
            },
            .generic_vector => {
                const impl = self.cast(.generic_vector);
                alloc.destroy(impl);
            },
            .bare => {
                const impl = self.cast(.bare);
                alloc.destroy(impl);
            },
            .option => {
                const impl = self.cast(.option);
                alloc.destroy(impl);
            },
            .result => {
                const impl = self.cast(.result);
                alloc.destroy(impl);
            },
        }
    }

    pub fn isInstanceOf(self: *Type, tmpl: *cy.sym.Template) bool {
        const variant = self.sym().instance orelse return false;
        return variant.data.sym.template == tmpl;
    }

    pub fn instanceOf(self: *Type, tmpl: *cy.sym.Template) ?*cy.Instance {
        const variant = self.sym().instance orelse return null;
        if (variant.data.sym.template == tmpl) {
            return variant;
        } else {
            return null;
        }
    }

    /// Assumes types and functions are already resolved.
    pub fn get_trait_method_impl(self: *cy.Type, member: cy.types.TraitMember) ?*cy.Func {
        const func_sym = self.sym().getMod().getSym(member.func.name()) orelse return null;
        if (func_sym.type != .func) {
            return null;
        }

        var next: ?*cy.Func = func_sym.cast(.func).first;
        while (next) |func| {
            if (!func.isMethod()) {
                next = func.next;
                continue;
            }
            const trait_sig = member.func.sig;

            // Skip receiver.
            const trait_params = trait_sig.params()[1..];
            const func_params = func.sig.params()[1..];
            for (trait_params, 0..) |trait_param, i| {
                if (trait_param != func_params[i]) {
                    next = func.next;
                    continue;
                }
            }
            if (trait_sig.ret != func.sig.ret) {
                next = func.next;
                continue;
            }
            return func;
        }
        return null;
    }

    pub fn eqUnderlyingType(self: *Type, other: *Type) bool {
        if (self.kind() != other.kind()) {
            return false;
        }
        switch (self.kind()) {
            .pointer => {
                const pointer_t = self.cast(.pointer);
                const other_t = other.cast(.pointer);
                return pointer_t.ref == other_t.ref and pointer_t.child_t == other_t.child_t;
            },
            .raw => {
                const a = self.cast(.raw);
                const b = other.cast(.raw);
                return a.bits == b.bits;
            },
            .int => {
                const a = self.cast(.int);
                const b = other.cast(.int);
                return a.bits == b.bits;
            },
            else => return false,
        }
    }

    pub fn isRefPointer(self: *Type) bool {
        return self.kind() == .pointer and self.cast(.pointer).ref;
    }

    pub fn isPointer(self: *Type) bool {
        return self.kind() == .pointer and !self.cast(.pointer).ref;
    }

    pub fn getBaseType(self: *Type) *cy.Type {
        if (self.getRefLikeChild()) |child_t| {
            return child_t.getBaseType();
        } else {
            return self;
        }
    }

    pub fn getRefLikeChild(self: *Type) ?*Type {
        if (self.kind() == .pointer) {
            return self.cast(.pointer).child_t;
        }
        if (self.kind() == .borrow) {
            return self.cast(.borrow).child_t;
        }
        if (self.kind() == .ex_borrow) {
            return self.cast(.ex_borrow).child_t;
        }
        return null;
    }

    pub fn cases(self: *Type) ?[]const Case {
        switch (self.kind()) {
            .choice => return self.cast(.choice).cases(),
            .result => return &self.cast(.result).cases_arr,
            else => return null, 
        }
    }

    pub fn fields(self: *Type) ?[]const Field {
        switch (self.kind()) {
            .struct_t => return self.cast(.struct_t).fields(),
            .c_union => return self.cast(.c_union).fields(),
            .choice => return self.cast(.choice).fields(),
            .option => return self.cast(.option).fields(),
            .result => return self.cast(.result).fields(),
            else => return null, 
        }
    }
};

fn TypeImpl(comptime kind: TypeKind) type {
    return switch (kind) {
        .generic_trait => GenericTrait,
        .dyn_trait,
        .borrow_trait,
        .ref_trait  => Trait,
        .option     => Option,
        .result     => Result,
        .choice     => Choice,
        .c_union    => CUnion,
        .enum_t     => Enum,
        .vector => Vector,
        .partial_vector => PartialVector,
        .generic_vector => GenericVector,
        .struct_t   => Struct,
        .pointer    => Pointer,
        .borrow     => Borrow,
        .ex_borrow  => Borrow,
        .func_ptr   => FuncPtr,
        .func_sym,
        .func       => Func,
        .raw        => Raw,
        .int        => Int,
        .float      => Float,
        .eval_ref,
        .eval_int,
        .c_variadic,
        .generic,
        .null,
        .bare,
        .bool,
        .never,
        .void       => Empty,
    };
}

pub const Float = extern struct {
    base: Type = undefined,

    bits: u32,
};

pub const Int = extern struct {
    base: Type = undefined,

    bits: u32,
};

pub const Raw = extern struct {
    base: Type = undefined,

    bits: u32,
    distinct: bool,
};

pub const Empty = extern struct {
    base: Type = undefined,
};

pub const Enum = extern struct {
    base: Type = undefined,

    cases_ptr: [*]*cy.sym.EnumCase = undefined,
    cases_len: u32 = 0,

    pub fn deinit(self: *Enum, alloc: std.mem.Allocator) void {
        alloc.free(self.cases());
    }

    pub fn getCaseByIdx(self: *Enum, idx: u32) *cy.sym.EnumCase {
        return self.cases_ptr[idx];
    }

    pub fn getCaseByTag(self: *Enum, tag: u32) *cy.sym.EnumCase {
        for (self.cases()) |case| {
            if (case.val == tag) {
                return case;
            }
        }
        std.debug.panic("Can not find case: {}", .{tag});
    }

    pub fn cases(self: *Enum) []*cy.sym.EnumCase {
        return self.cases_ptr[0..self.cases_len];
    }

    pub fn getCase(self: *Enum, name: []const u8) ?*cy.sym.EnumCase {
        const mod = self.base.sym().getMod();
        if (mod.getSym(name)) |res| {
            if (res.type == .enum_case) {
                return res.cast(.enum_case);
            }
        }
        return null;
    }

    pub fn getCaseTag(self: *Enum, name: []const u8) ?u32 {
        const case = self.getCase(name) orelse return null;
        return case.val;
    }
};

pub const Result = extern struct {
    base: Type = undefined,

    child_t: *Type,

    /// Byte size.
    size: cy.Nullable(u32) = cy.NullId,

    alignment: u8 = 0,

    cases_arr: [2]Case = undefined,
    fields_arr: [2]Field = undefined,

    pub fn fields(self: *Result) []const Field {
        return &self.fields_arr;
    }
};

pub const Option = extern struct {
    base: Type = undefined,

    child_t: *Type,

    /// Byte size.
    size: cy.Nullable(u32) = cy.NullId,

    alignment: u8 = 0,

    /// Reference types do not need a separate tag field and relies on the value being 0 to indicate `none`.
    zero_union: bool,

    fields_arr: [2]Field = undefined,

    pub fn fields(self: *Option) []const Field {
        return &self.fields_arr;
    }
};

pub const CUnion = extern struct {
    base: Type = undefined,

    /// Byte size. Maximum payload type size.
    size: cy.Nullable(u32) = cy.NullId,

    cases_ptr: [*]CUnionCase = undefined,
    cases_len: u32 = 0,

    alignment: u8 = 0,

    fields_arr: [1]Field = undefined,

    pub fn deinit(self: *CUnion, alloc: std.mem.Allocator) void {
        alloc.free(self.cases());
    }

    pub fn cases(self: *CUnion) []CUnionCase {
        return self.cases_ptr[0..self.cases_len];
    }

    pub fn getCaseByIdx(self: *CUnion, idx: u32) *cy.sym.UnionCase {
        return self.cases_ptr[idx].sym.?;
    }

    pub fn getCase(self: *CUnion, name: []const u8) ?*cy.sym.UnionCase {
        const mod = self.base.sym().getMod();
        if (mod.getSym(name)) |res| {
            if (res.type == .cunion_case) {
                return res.cast(.cunion_case);
            }
        }
        return null;
    }

    pub fn fields(self: *CUnion) []const Field {
        return &self.fields_arr;
    }
};

pub const Choice = extern struct {
    base: Type = undefined,

    /// Byte size. Maximum payload type size in addition to the tag size.
    size: cy.Nullable(u32) = cy.NullId,

    cases_ptr: [*]Case = undefined,
    cases_len: u32 = 0,

    alignment: u8 = 0,

    tag_t: *Type,

    fields_arr: [2]Field = undefined,

    pub fn deinit(self: *Choice, alloc: std.mem.Allocator) void {
        alloc.free(self.cases());
    }

    pub fn cases(self: *Choice) []Case {
        return self.cases_ptr[0..self.cases_len];
    }

    pub fn getCaseByIdx(self: *Choice, idx: u32) *cy.sym.ChoiceCase {
        return self.cases_ptr[idx].sym.?;
    }

    pub fn getCaseByTag(self: *Choice, tag: u32) *cy.sym.ChoiceCase {
        for (self.cases()) |case| {
            if (case.val == tag) {
                return case.sym.?;
            }
        }
        std.debug.panic("Can not find case: {}", .{tag});
    }

    pub fn getCase(self: *Choice, name: []const u8) ?*cy.sym.ChoiceCase {
        const mod = self.base.sym().getMod();
        if (mod.getSym(name)) |res| {
            if (res.type == .choice_case) {
                return res.cast(.choice_case);
            }
        }
        return null;
    }

    pub fn getCaseTag(self: *Choice, name: []const u8) ?u32 {
        if (self.getCase(name)) |sym| {
            return sym.val;
        } else {
            return null;
        }
    }

    pub fn fields(self: *Choice) []const Field {
        return &self.fields_arr;
    }
};

/// Tracks embedded fields for automatic member surfacing
pub const EmbeddedFieldInfo = struct {
    /// Index into the fields array
    field_idx: u32,
    
    /// TypeId of the embedded type
    embedded_type: *cy.Type,
};

/// TODO: Hash fields for static casting.
pub const Struct = extern struct {
    base: Type = undefined,

    /// Top level fields.
    fields_ptr: [*]Field = undefined,
    fields_len: u32 = cy.NullId,
    fields_owned: bool = true,

    // Embedded field tracking
    embedded_fields_ptr: [*]const EmbeddedFieldInfo = undefined,
    embedded_fields_len: u32 = 0,

    /// Recursive. Field states only includes struct members.
    field_state_len: usize = 0,

    /// Byte size.
    size: cy.Nullable(u32) = cy.NullId,

    /// Linear lookup since most types don't have many impls.
    impls_ptr: [*]Impl = undefined,
    impls_len: u32 = cy.NullId,

    alignment: u8 = 0,
    cstruct: bool,
    opaque_t: bool,
    tuple: bool,

    /// Only relevant for structs/cstructs.
    /// Used to detect circular dependency.
    resolving_struct: bool = false,

    next_dead_ref_fn: ?*const fn(vm: *cy.VM, obj: *cy.HeapObject, start: bool) ?*anyopaque = null,
    deinit_fn: ?*const fn(vm: *cy.VM, obj: *cy.HeapObject) void = null,

    fn deinit(self: *Struct, alloc: std.mem.Allocator) void {
        if (self.base.isResolved()) {
            if (self.fields_owned) {
                alloc.free(self.fields());
            }

            for (self.impls()) |impl| {
                impl.deinit(alloc);
            }
            alloc.free(self.impls());

            if (self.embedded_fields_len > 0) {
                const embedded = self.embedded_fields_ptr[0..self.embedded_fields_len];
                alloc.free(embedded);
            }
        }
    }

    pub fn fields(self: *Struct) []Field {
        return self.fields_ptr[0..self.fields_len];
    }

    pub fn impls(self: *Struct) []Impl {
        return self.impls_ptr[0..self.impls_len];
    }

    pub fn implements(self: *Struct, trait_t: *GenericTrait) bool {
        for (self.impls()) |impl| {
            if (impl.trait == trait_t) {
                return true;
            }
        }
        return false;
    }

    pub fn getEmbeddedFields(self: *Struct) []const EmbeddedFieldInfo {
        return self.embedded_fields_ptr[0..self.embedded_fields_len];
    }
    
    pub fn hasEmbeddings(self: *Struct) bool {
        return self.embedded_fields_len > 0;
    }
};

pub const Impl = struct {
    trait: *GenericTrait,

    funcs: []*cy.Func,

    fn deinit(self: Impl, alloc: std.mem.Allocator) void {
        alloc.free(self.funcs);
    }
};

pub const TraitMember = struct {
    func: *cy.Func,
};

const ZHashMap = extern struct {
    metadata: ?*anyopaque,
    size: u32,
    available: u32,
    pointer_stability: u64,
};

pub const GenericTrait = extern struct {
    base: Type = undefined,

    members_ptr: [*]const TraitMember = undefined,
    members_len: u32 = 0,

    predicate: ?*cy.Func = null,

    impls: ZHashMap,

    pub fn zimpls(self: *GenericTrait) *std.AutoHashMapUnmanaged(*cy.Type, void) {
        return @ptrCast(&self.impls);
    }

    pub fn deinit(self: *GenericTrait, alloc: std.mem.Allocator) void {
        // Functions are owned by trait since they are defined together.
        for (self.members()) |member| {
            member.func.destroy(alloc);
        }
        alloc.free(self.members());

        self.zimpls().deinit(alloc);
    }

    pub fn members(self: *GenericTrait) []const TraitMember {
        return self.members_ptr[0..self.members_len];
    }
};

pub const Trait = extern struct {
    base: Type = undefined,

    generic: *GenericTrait,
};

pub const FuncPtr = extern struct {
    base: Type = undefined,

    sig: *cy.FuncSig,

    /// Useful for the VM backend to know which function pointers to generate dispatch code.
    extern_is_called: bool = false, 
};

pub const Func = extern struct {
    base: Type = undefined,

    sig: *cy.FuncSig,
    opaque_: bool = false,
};

pub const GenericVector = extern struct {
    base: Type = undefined,

    elem_t: *cy.Type,
};

pub const PartialVector = extern struct {
    base: Type = undefined,

    n: usize,
    elem_t: *cy.Type,

    /// Byte size.
    size: usize = 0,
};

pub const Vector = extern struct {
    base: Type = undefined,

    n: usize,
    elem_t: *cy.Type,

    /// Byte size.
    size: usize = 0,
};

pub const VaList = extern struct {
    base: Type = undefined,

    elem: ?*Type,
};

pub const Borrow = extern struct {
    base: Type = undefined,

    child_t: *Type,
};

pub const Pointer = extern struct {
    base: Type = undefined,

    ref: bool,
    child_t: *Type,
};

pub const CUnionCase = extern struct {
    sym: ?*cy.sym.UnionCase,
    name_ptr: [*]const u8,
    name_len: u32,
    payload_t: *cy.Type,

    pub fn name(self: *const CUnionCase) []const u8 {
        return self.name_ptr[0..self.name_len];
    }
};

pub const Case = extern struct {
    sym: ?*cy.sym.ChoiceCase,
    name_ptr: [*]const u8,
    name_len: u32,
    val: u32,
    payload_t: *cy.Type,

    pub fn name(self: *const Case) []const u8 {
        return self.name_ptr[0..self.name_len];
    }
};

pub const Field = extern struct {
    sym: *cy.sym.Field,
    type: *cy.Type,

    /// For struct/cstruct. Field offset in bytes from the start of the parent.
    offset: u32,

    /// Considers nested struct fields only.
    state_offset: u32 = 0,

    /// Default initializer node.
    init_n: ?*ast.Node = null,
};

test "types internals." {
}

pub const BuiltinEnd: TypeId = vmc.BuiltinEnd;

const bt = BuiltinTypes;
pub const BuiltinTypes = struct {
    pub const Object: TypeId = vmc.TYPE_OBJECT;
    pub const Bool: TypeId = vmc.TYPE_BOOL;
    pub const I8: TypeId = vmc.TYPE_I8;
    pub const I16: TypeId = vmc.TYPE_I16;
    pub const I32: TypeId = vmc.TYPE_I32;
    pub const I64: TypeId = vmc.TYPE_I64;
    pub const EvalInt: TypeId = vmc.TYPE_EVAL_INT;
    pub const R8: TypeId = vmc.TYPE_R8;
    pub const R16: TypeId = vmc.TYPE_R16;
    pub const R32: TypeId = vmc.TYPE_R32;
    pub const R64: TypeId = vmc.TYPE_R64;
    pub const F64: TypeId = vmc.TYPE_F64;
    pub const F32: TypeId = vmc.TYPE_F32;
    pub const Str: TypeId = vmc.TYPE_STR;
    pub const EvalStr: TypeId = vmc.TYPE_EVAL_STR;
    pub const MutStr: TypeId = vmc.TYPE_MUT_STR;
    pub const NoCopy: TypeId = vmc.TYPE_NO_COPY;
    pub const Thread: TypeId = vmc.TYPE_THREAD;
    pub const PartialStructLayout: TypeId = vmc.TYPE_PARTIAL_STRUCT_LAYOUT;
    pub const Symbol: TypeId = vmc.TYPE_SYMBOL;
    pub const Never: TypeId = vmc.TYPE_NEVER;
    pub const Any: TypeId = vmc.TYPE_ANY;
    pub const Code: TypeId = vmc.TYPE_CODE;
    pub const Void: TypeId = vmc.TYPE_VOID;
    pub const Null: TypeId = vmc.TYPE_NULL;
    pub const Error: TypeId = vmc.TYPE_ERROR;
    pub const Type: TypeId = vmc.TYPE_TYPE;
    pub const StrBuffer: TypeId = vmc.TYPE_STR_BUFFER;
    pub const FuncSig: TypeId = vmc.TYPE_FUNC_SIG;
    pub const Dependent: TypeId = vmc.TYPE_DEPENDENT;
    pub const Infer: TypeId = vmc.TYPE_INFER;
    pub const TccState: TypeId = vmc.TYPE_TCC_STATE;
    pub const Range: TypeId = vmc.TYPE_RANGE;
    pub const Table: TypeId = vmc.TYPE_TABLE;

    // Used to indicate no type value.
    // pub const Undefined: TypeId = vmc.TYPE_UNDEFINED;
};

pub var NullSym = cy.sym.TypeSym{
    .head = cy.Sym.init(.null, null, "<null>"),
    .decl = null,
    .type = undefined,
    .instance = null,
    .impls_ = .{},
    .mod = undefined,
};

pub var NullType = Type{
    .kind_ = .null,
    .sym_ = &NullSym,
    .id_ = 0,
    .info = undefined,
};

pub fn getType(s: *cy.Sema, id: TypeId) *cy.Type {
    return s.types.items[id];
}

/// Assumes `src_t` is fully resolved.
pub fn createTypeCopy(s: *cy.Sema, id: TypeId, src_t: *cy.Type) !*Type {
    var new_t: *cy.Type = undefined;
    switch (src_t.kind()) {
        .raw => {
            const impl = src_t.cast(.raw);
            new_t = try s.createTypeWithId(.raw, id, .{ .bits = impl.bits });
        },
        .int => {
            const impl = src_t.cast(.int);
            new_t = try s.createTypeWithId(.int, id, .{ .bits = impl.bits });
        },
        .pointer => {
            const impl = src_t.cast(.pointer);
            new_t = try s.createTypeWithId(.pointer, id, .{ .ref = impl.ref, .child_t = impl.child_t });
        },
        .struct_t => {
            const impl = src_t.cast(.struct_t);
            new_t = try s.createTypeWithId(.struct_t, id, .{
                .size = impl.size,
                .alignment = impl.alignment,
                .cstruct = impl.cstruct,
                .opaque_t = impl.opaque_t,
                .tuple = impl.tuple,
                .fields_ptr = impl.fields_ptr,
                .fields_len = impl.fields_len,
                .fields_owned = false,
            });
        },
        .vector => {
            const impl = src_t.cast(.vector);
            new_t = try s.createTypeWithId(.vector, id, .{
                .n = impl.n,
                .elem_t = impl.elem_t,
                .size = impl.size,
            });
        },
        .partial_vector => {
            const impl = src_t.cast(.partial_vector);
            new_t = try s.createTypeWithId(.partial_vector, id, .{
                .n = impl.n,
                .elem_t = impl.elem_t,
                .size = impl.size,
            });
        },
        else => {
            std.debug.panic("TODO: {}", .{src_t.kind()});
        },
    }
    new_t.info.distinct = true;
    return new_t;
}

/// Type creation updates the sema type table as well as the runtime type table.
/// Some helpers at compile-time rely on an updated runtime type table. 
pub fn createType(s: *cy.Sema, comptime kind: TypeKind, data: TypeImpl(kind)) !*Type {
    const new = try s.alloc.create(TypeImpl(kind));
    new.* = data;
    const id = s.types.items.len;
    new.base = .{
        .sym_ = undefined,
        .kind_ = kind,
        .info = .{},
        .id_ = @intCast(id),
    };
    try s.types.append(s.alloc, @ptrCast(new));

    if (cy.Trace) {
        // s.compiler.vm.c.types = s.types.items.ptr;
        // s.compiler.vm.c.types_len = s.types.items.len;
    }
    return @ptrCast(new);
}

pub fn createTypeWithId(s: *cy.Sema, comptime kind: TypeKind, id: TypeId, data: TypeImpl(kind)) !*Type {
    const new = try s.alloc.create(TypeImpl(kind));
    new.* = data;
    new.base = .{
        .sym_ = undefined,
        .kind_ = kind,
        .info = .{},
        .id_ = id,
    };
    if (id >= s.types.items.len) {
        const old_len = s.types.items.len;
        try s.types.resize(s.alloc, id + 1);
        for (old_len..id) |i| {
            s.types.items[i] = &NullType;
        }

        if (cy.Trace) {
            // s.compiler.vm.c.types = s.types.items.ptr;
            // s.compiler.vm.c.types_len = s.types.items.len;
        }
    } else {
        if (s.types.items[id].kind() != .null) {
            return error.DuplicateTypeId;
        }
    }
    s.types.items[id] = @ptrCast(new);
    return @ptrCast(new);
}

pub fn reserveType(s: *cy.Sema) !TypeId {
    const id = s.types.items.len;
    try s.types.append(s.alloc, undefined);
    s.types.items[id] = &NullType;

    if (cy.Trace) {
        // s.compiler.vm.c.types = s.types.items.ptr;
        // s.compiler.vm.c.types_len = s.types.items.len;
    }
    return @intCast(id);
}

// TODO: Move to `Type.allocName`.
pub fn allocTypeName(s: *cy.Sema, type_: *Type) ![]const u8 {
    var buf = std.Io.Writer.Allocating.init(s.alloc);
    defer buf.deinit();

    try s.writeTypeName(&buf.writer, type_, null);
    return buf.toOwnedSlice();
}

pub fn allocSymName(s: *cy.Sema, sym: *cy.Sym) ![]const u8 {
    var buf = std.Io.Writer.Allocating.init(s.alloc);
    defer buf.deinit();

    try cy.sym.writeSymName(s, &buf.writer, sym, .{ .from = null });
    return buf.toOwnedSlice();
}

pub fn writeTypeName(s: *cy.Sema, w: *std.Io.Writer, type_: *Type, from: ?*cy.Chunk) !void {
    if (type_.kind() == .null) {
        try w.writeByte('_');
        return;
    }
    try cy.sym.writeSymName(s, w, @ptrCast(type_.sym()), .{ .from = from });
}

pub fn getRtCompareType(s: *cy.Sema, type_: *Type) u32 {
    _ = s;
    if (type_.kind() == .pointer and type_.cast(.pointer).ref) {
        return 0x80000000 | type_.cast(.pointer).child_t.id();
    }
    return type_.id();
}

pub fn isAnyOrDynamic(id: TypeId) bool {
    return id == bt.Object or id == bt.Dyn;
}

pub fn isRtTypeCompat(c: *cy.Compiler, shape_t: TypeId, is_ref: bool, cstr_t: TypeId, rec: bool) bool {
    if (is_ref) {
        if (cstr_t == bt.Object or cstr_t == bt.Dyn) {
            return true;
        }
        const cstr_te = c.sema.getType(cstr_t);
        if (cstr_te.kind == .pointer and cstr_te.data.pointer.ref and cstr_te.data.pointer.child_t == shape_t) {
            return true;
        } 
    } else {
        if (shape_t == cstr_t) {
            return true;
        }
        if (cstr_t == bt.Object or cstr_t == bt.Dyn) {
            return true;
        }
        if (rec) {
            const cstr_te = c.sema.getType(cstr_t);
            if (cstr_te.kind == .pointer and cstr_te.data.pointer.ref and cstr_te.data.pointer.child_t == shape_t) {
                return true;
            } 
        }
    }
    return false;
}

pub fn isTypeCompat(c: *cy.Compiler, src_t: *Type, cstr_t: *Type) bool {
    if (src_t == cstr_t) {
        return true;
    }
    // if (cstr_t.id() == bt.Object and src_t.isRefLike()) {
    //     return true;
    // }
    if (cstr_t.id() == c.sema.ptr_void_t.id() and src_t.isPointer()) {
        return true;
    }
    if (cstr_t.kind() == .c_variadic) {
        return true;
    }
    return false;
}

/// Check type constraints on target func signature.
pub fn isFuncSigCompat(c: *cy.Compiler, id: sema.FuncSigId, targetId: sema.FuncSigId) bool {
    const src = c.sema.getFuncSig(id);
    const target = c.sema.getFuncSig(targetId);

    // First check params length.
    if (src.params_len != target.params_len) {
        return false;
    }

    // Check each param type. Attempt to satisfy constraints.
    for (target.params(), src.params()) |cstsymId, typeSymId| {
        if (!isTypeCompat(c, typeSymId, cstsymId)) {
            return false;
        }
    }

    // Check return type. Source return type is the constraint.
    return isTypeCompat(c, target.retSymId, src.retSymId);
}

pub fn toRtConcreteType(type_: *cy.Type) ?*cy.Type {
    return switch (type_.id()) {
        bt.Object => null,
        else => return type_,
    };
}

pub fn typeEqualOrChildOf(a: TypeId, b: TypeId) bool {
    if (b == bt.Object) {
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

pub fn unionOf(c: *cy.Compiler, a: *Type, b: *Type) *Type {
    if (a == b) {
        return a;
    } else {
        return c.sema.object_t;
    }
}