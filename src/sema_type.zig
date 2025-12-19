const std = @import("std");
const cy = @import("cyber.zig");
const sema = cy.sema;
const cte = cy.cte;
const ast = cy.ast;
const ct_inline = cy.ct_inline;
const C = @import("capi.zig");
const v = cy.fmt.v;
const bt = cy.types.BuiltinTypes;

const log = cy.log.scoped(.sema_type);

pub fn resolve_type_const(c: *cy.Chunk, sym: *cy.sym.TypeConst) !void {
    if (sym.resolved) {
        return;
    }
    try sema.pushSymResolveContext(c, @ptrCast(sym), @ptrCast(sym.decl));
    defer sema.popResolveContext(c);

    sema.getResolveContext(c).is_inline_eval = true;

    const res = try cte.evalFuncBody(c, sym.decl.stmts.slice(), c.sema.type_t, @ptrCast(sym.decl));
    const res_t = res.asPtr(*cy.Type);
    sym.type = res_t;

    res_t.sym().head.parent = sym.head.parent.?;
    res_t.sym().head.namePtr = sym.head.namePtr;
    res_t.sym().head.nameLen = sym.head.nameLen;
    res_t.sym().instance = sym.instance;
    sym.resolved = true;
}

/// Explicit `decl` for specialization declarations.
pub fn resolve_custom_type(c: *cy.Chunk, decl: *ast.CustomTypeDecl, opt_variant: ?*cy.Instance) !*cy.Sym {
    var name: ?[]const u8 = null;
    var has_host_attr = false;
    if (ast.findAttr(decl.attrs.slice(), .bind)) |attr| {
        has_host_attr = true;
        name = try attr.getString();
    }
    if (!has_host_attr) {
        return c.reportErrorFmt("Custom type requires a `@host` attribute.", &.{}, @ptrCast(decl));
    }

    var src_c = c;
    if (opt_variant) |variant| {
        src_c = c.compiler.chunks.items[variant.data.sym.template.head.declNode().src()];
    }

    const decl_name = decl.name.declName();
    const bind_name = name orelse decl_name;

    const type_sym = try c.createTypeSym(&src_c.sym.head, decl_name, null, @ptrCast(decl));
    type_sym.instance = opt_variant;

    try sema.pushSymResolveContext(c, @ptrCast(type_sym), null);
    defer sema.popResolveContext(c);

    log.tracev("Invoke type loader for: {s}", .{bind_name});
    const binding = src_c.host_types.get(bind_name) orelse {
        return c.reportErrorFmt("Failed to load custom type `{}`.", &.{v(bind_name)}, @ptrCast(decl));
    };
    const new_t = try resolveCustomTypeResult(c, decl, binding);
    type_sym.type = new_t;
    new_t.sym_ = type_sym;
    return @ptrCast(type_sym);
}

fn resolveCustomTypeResult(c: *cy.Chunk, decl: *ast.CustomTypeDecl, binding: C.BindType) !*cy.Type {
    switch (binding.type) {
        C.BindTypeCreate => {
            const ctype = binding.data.create.create_fn.?(@ptrCast(c.vm), c.sym.head.toC(), ast.Node.to_c(@ptrCast(decl))) orelse {
                return error.CompileError;
            };
            return @ptrCast(@alignCast(ctype));
        },
        else => return error.Unsupported,
    }
}

pub fn resolve_trait_type(c: *cy.Chunk, trait_t: *cy.types.GenericTrait, node: *ast.Node) !void {
    const sym = trait_t.base.sym();
    try sema.pushSymResolveContext(c, @ptrCast(sym), node);
    defer sema.popResolveContext(c);

    const decl = sym.decl.?.cast(.trait_decl);

    const members = try c.alloc.alloc(cy.types.TraitMember, decl.funcs.len);
    errdefer c.alloc.free(members);
    for (decl.funcs.slice(), 0..) |func_decl, i| {
        const func = try sema.reserveImplicitTraitMethod(c, @ptrCast(sym), func_decl, i);
        try sema.ensure_resolved_func(c, func, false, @ptrCast(func_decl));
        members[i] = .{
            .func = func,
        };
    }

    if (sym.getMod().getSym("@predicate")) |func_sym| {
        trait_t.predicate = func_sym.cast(.func).first;
    }

    trait_t.members_ptr = members.ptr;
    trait_t.members_len = @intCast(members.len);

    trait_t.base.info.managed = false;
    trait_t.base.info.copy_base = false;
    trait_t.base.info.copy_user = false;
    trait_t.base.info.resolved = true;
}

pub fn ensure_resolved_type(c: *cy.Chunk, type_: *cy.Type, node: *ast.Node) anyerror!void {
    if (type_.isResolved()) {
        return;
    }
    if (cy.Trace) {
        const name = try c.sema.allocTypeName(type_);
        defer c.alloc.free(name);
        log.tracev("resolve type: {s} {}", .{name, type_.id()});
    }
    switch (type_.kind()) {
        .never,
        .eval_int,
        .eval_ref,
        .void,
        .func_ptr,
        .func_sym,
        .c_variadic,
        .generic_vector,
        .generic => {
            type_.info.managed = false;
            type_.info.copy_base = false;
            type_.info.copy_user = false;
            type_.info.resolved = true;
        },
        .ex_borrow,
        .borrow => {
            type_.info.borrow_only = true;
            type_.info.managed = false;
            type_.info.copy_base = false;
            type_.info.copy_user = false;
            type_.info.resolved = true;
        },
        .generic_trait => {
            try resolve_trait_type(c, type_.cast(.generic_trait), node);
        },
        .borrow_trait => {
            type_.info.managed = false;
            type_.info.copy_base = false;
            type_.info.copy_user = false;
            type_.info.resolved = true;
        },
        .dyn_trait,
        .ref_trait,
        .func => {
            type_.info.managed = true;
            type_.info.copy_base = true;
            type_.info.copy_user = false;
            type_.info.resolved = true;
        },
        .pointer => {
            const pointer = type_.cast(.pointer);
            var managed = false;
            if (pointer.ref) {
                managed = true;
            } else if (type_.sym().getMod().getSym("@deinit") != null) {
                managed = true;
            }
            type_.info.managed = managed;
            type_.info.copy_base = pointer.ref;
            type_.info.copy_user = false;
            type_.info.resolved = true;
        },
        .bool,
        .raw,
        .int,
        .float => {
            var managed = false;
            if (type_.sym().getMod().getSym("@deinit") != null) {
                managed = true;
            }
            type_.info.managed = managed;
            type_.info.copy_base = false;
            type_.info.copy_user = false;
            type_.info.resolved = true;
        },
        .result => {
            const result_t = type_.cast(.result);
            try resolveResultType(c, result_t, node);
        },
        .option => {
            const option_t = type_.cast(.option);
            try resolveOptionType(c, option_t, node);
        },
        .enum_t => {
            const enum_t = type_.cast(.enum_t);
            try resolve_enum_type(c, enum_t, node);
        },
        .choice => {
            const choice_t = type_.cast(.choice);
            try resolve_choice_type(c, choice_t, node);
        },
        .c_union => {
            const c_union = type_.cast(.c_union);
            try resolve_cunion_type(c, c_union, node);
        },
        .partial_vector => {
            const vector_t = type_.cast(.partial_vector);
            try resolvePartialVectorType(c, vector_t, node);
        },
        .vector => {
            const vector_t = type_.cast(.vector);
            try resolveVectorType(c, vector_t, node);
        },
        .struct_t => {
            const struct_t = type_.cast(.struct_t);
            if (struct_t.resolving_struct) {
                return c.reportError("Structs can not contain a circular dependency.", node);
            }
            try resolveStructType(c, struct_t, node);
        },
        else => {
            return c.reportErrorFmt("TODO: {}", &.{v(type_.kind())}, node);
        },
    }
}

pub fn reserve_type_const(c: *cy.Chunk, node: *ast.TypeConstDecl) !*cy.sym.TypeConst {
    const name = node.name.as_name();
    return c.reserve_type_const(&c.sym.head, name, node);
}

pub fn reserve_type_alias(c: *cy.Chunk, node: *ast.TypeAliasDecl) !*cy.sym.TypeAlias {
    const name = node.name.as_name();
    const decl_path = try sema.ensureDeclNamePath(c, @ptrCast(c.sym), node.parent, node.name);
    return c.reserveTypeAlias(decl_path.parent, name, node);
}

pub fn resolve_type_alias(c: *cy.Chunk, sym: *cy.sym.TypeAlias) anyerror!void {
    if (sym.resolved) {
        return;
    }
    try sema.pushSymResolveContext(c, @ptrCast(sym), @ptrCast(sym.decl));
    defer sema.popResolveContext(c);

    if (sym.decl.?.target.type() != .void_lit) {
        const type_ = try cte.evalType(c, sym.decl.?.target);
        sym.sym = type_.sym();
        sym.resolved = true;
    } else {
        try resolveCustomTypeAlias(c, sym);
    }
}

pub fn resolveCustomTypeAlias(c: *cy.Chunk, sym: *cy.sym.TypeAlias) anyerror!void {
    const decl = sym.decl.?;
    var name: ?[]const u8 = null;
    var has_host_attr = false;
    if (ast.findAttr(decl.attrs.slice(), .bind)) |attr| {
        has_host_attr = true;
        name = try attr.getString();
    }
    if (!has_host_attr) {
        return c.reportErrorFmt("Custom type alias requires a `@host` attribute.", &.{}, @ptrCast(decl));
    }

    const bind_name = name orelse b: {
        c.tempBufU8.clearRetainingCapacity();
        const w = c.tempBufU8.writer(c.alloc);
        try sema.writeBindName(w, &sym.head);
        break :b c.tempBufU8.items;
    };

    const src_id = @as(*ast.Node, @ptrCast(sym.decl)).src();
    const src_chunk = c.compiler.chunks.items[src_id];
    const binding = src_chunk.host_types.get(bind_name) orelse {
        return c.reportErrorFmt("Failed to load host type `{}`.", &.{v(bind_name)}, @ptrCast(decl));
    };

    if (binding.type != C.BindTypeAlias) {
        return c.reportErrorFmt("Expected type alias binding when loading `{}`.", &.{v(bind_name)}, @ptrCast(decl));
    }
    const res = binding.data.alias.resolve_fn.?(@ptrCast(c.vm), sym.head.toC()) orelse {
        return error.CompileError;
    };
    sym.sym = @ptrCast(@alignCast(res));
    sym.resolved = true;
}

pub fn reserve_enum(c: *cy.Chunk, decl: *ast.EnumDecl) !*cy.sym.TypeSym {
    const name = decl.name.declName();

    const type_id = try resolveTypeIdFromDecl(c, c, name, &.{}, @ptrCast(decl));
    const new_t = try c.sema.createTypeWithId(.enum_t, type_id, .{});
    const sym = try c.reserveTypeSym(@ptrCast(c.sym), name, new_t, @ptrCast(decl));
    return sym;
}

pub fn resolve_enum_type(c: *cy.Chunk, enum_t: *cy.types.Enum, node: *ast.Node) !void {
    const sym = enum_t.base.sym();
    try sema.pushSymResolveContext(c, @ptrCast(sym), node);
    defer sema.popResolveContext(c);

    const decl = sym.decl.?.cast(.enumDecl);
    try declare_enum_cases(c, enum_t, decl);
}

/// Explicit `decl` node for distinct type declarations. Must belong to `c`.
pub fn declare_enum_cases(c: *cy.Chunk, enum_t: *cy.types.Enum, decl: *ast.EnumDecl) !void {
    if (enum_t.base.isResolved()) {
        return;
    }
    const cases = try c.alloc.alloc(*cy.sym.EnumCase, decl.members.len);
    for (decl.members.slice(), 0..) |member, i| {
        const mName = member.name.as_name();
        cases[i] = try c.declareEnumCase(@ptrCast(enum_t.base.sym()), mName, @ptrCast(enum_t), @intCast(i), member);
    }
    enum_t.cases_ptr = cases.ptr;
    enum_t.cases_len = @intCast(cases.len);

    var managed = false;
    if (enum_t.base.sym().getMod().getSym("@deinit") != null) {
        managed = true;
    }
    enum_t.base.info.managed = managed;
    enum_t.base.info.copy_base = false;
    enum_t.base.info.copy_user = false;
    enum_t.base.info.resolved = true;
}

pub fn reserve_choice(c: *cy.Chunk, decl: *ast.EnumDecl) !*cy.sym.TypeSym {
    const name = decl.name.declName();

    const tag_t = try c.sema.createType(.enum_t, .{});

    const type_id = try resolveTypeIdFromDecl(c, c, name, &.{}, @ptrCast(decl));
    const new_t = try c.sema.createTypeWithId(.choice, type_id, .{ .tag_t = tag_t });
    const sym = try c.reserveTypeSym(@ptrCast(c.sym), name, new_t, @ptrCast(decl));

    _ = try c.reserveTypeSym(&sym.head, "Tag", tag_t, @ptrCast(decl));
    return sym;
}

pub fn resolve_cunion_type(c: *cy.Chunk, c_union: *cy.types.CUnion, node: *ast.Node) !void {
    if (c_union.base.isResolved()) {
        return;
    }

    const sym = c_union.base.sym();
    try sema.pushSymResolveContext(c, @ptrCast(sym), node);
    defer sema.popResolveContext(c);

    var alignment: u8 = 1;
    var max_payload_size: usize = 0;
    var max_payload_t: *cy.Type = c.sema.void_t;

    const decl = sym.decl.?.cast(.cunion_decl);
    const cases = try c.alloc.alloc(cy.types.CUnionCase, decl.cases.len);
    for (decl.cases.slice(), 0..) |case, i| {
        const case_name = case.name.as_name();
        const payload_t = try cte.eval_type2(c, true, case.payload_t);
        try ensure_resolved_type(c, payload_t, @ptrCast(case.payload_t));

        if (payload_t.isManaged()) {
            return c.reportError("A `cunion` type cannot contain a managed type.", case.payload_t);
        }
        if (payload_t.has_copy_ctor()) {
            return c.reportError("A `cunion` type cannot contain a managed type.", case.payload_t);
        }

        const payload_size = payload_t.size();
        if (payload_size > max_payload_size) {
            max_payload_size = payload_size;
            max_payload_t = payload_t;
        }

        if (payload_t.alignment() > alignment) {
            alignment = payload_t.alignment();
        }

        const case_sym = try c.declareUnionCase(@ptrCast(c_union.base.sym()), case_name,
            @ptrCast(c_union), @intCast(i), payload_t, @ptrCast(case));
        cases[i] = .{
            .sym = case_sym,
            .name_ptr = case_name.ptr,
            .name_len = @intCast(case_name.len),
            .payload_t = payload_t,
        };
    }
    c_union.cases_ptr = cases.ptr;
    c_union.cases_len = @intCast(cases.len);

    var size = max_payload_size;
    const rem = size % alignment;
    if (rem > 0) {
        size += alignment - rem;
    }

    c_union.size = @intCast(size);
    c_union.alignment = alignment;
    c_union.fields_arr[0] = .{ .sym = undefined, .type = c.sema.void_t, .offset = 0 };

    c_union.base.info.managed = false;
    c_union.base.info.copy_base = false;
    c_union.base.info.copy_user = false;
    c_union.base.info.resolved = true;
}

pub fn resolve_choice_type(c: *cy.Chunk, choice_t: *cy.types.Choice, node: *ast.Node) !void {
    const sym = choice_t.base.sym();
    try sema.pushSymResolveContext(c, @ptrCast(sym), node);
    defer sema.popResolveContext(c);

    const decl = sym.decl.?.cast(.enumDecl);
    try declare_choice_cases(c, choice_t, decl);

    const tag_t = sym.getMod().getSym("Tag").?.cast(.type).type;
    try declare_enum_cases(c, tag_t.cast(.enum_t), decl);
}

pub fn declare_choice_cases(c: *cy.Chunk, choice_t: *cy.types.Choice, decl: *ast.EnumDecl) !void {
    if (choice_t.base.isResolved()) {
        return;
    }

    const alignment: u8 = 8;
    var max_payload_size: usize = 0;
    var max_payload_t: *cy.Type = c.sema.void_t;

    var has_managed_payload = false;
    var has_copy_payload = false;
    var has_no_copy_payload = false;
    var has_ct_child = false;
    const cases = try c.alloc.alloc(cy.types.Case, decl.members.len);
    for (decl.members.slice(), 0..) |member, i| {
        const mName = member.name.as_name();
        var payload_t: *cy.Type = undefined;
        if (member.typeSpec != null) {
            payload_t = try cte.eval_type2(c, true, member.typeSpec.?);
            try ensure_resolved_type(c, payload_t, @ptrCast(member.typeSpec));
        } else {
            payload_t = c.sema.void_t;
            try ensure_resolved_type(c, payload_t, member.name);
        }
        if (payload_t.isManaged()) {
            has_managed_payload = true;
        }
        if (payload_t.has_copy_ctor()) {
            has_copy_payload = true;
        }
        if (payload_t.info.ct) {
            has_ct_child = true;
        }
        if (payload_t.info.no_copy) {
            has_no_copy_payload = true;
        }

        const payload_size = payload_t.size();
        if (payload_size > max_payload_size) {
            max_payload_size = payload_size;
            max_payload_t = payload_t;
        }

        if (payload_t.alignment() > alignment) {
            std.debug.panic("Unsupported alignment.", .{});
        }

        const case_sym = try c.declareChoiceCase(@ptrCast(choice_t.base.sym()), mName, @ptrCast(choice_t), @intCast(i), @intCast(i), payload_t, @ptrCast(member));
        cases[i] = .{
            .sym = case_sym,
            .name_ptr = mName.ptr,
            .name_len = @intCast(mName.len),
            .payload_t = payload_t,
            .val = @intCast(i),
        };
    }
    choice_t.cases_ptr = cases.ptr;
    choice_t.cases_len = @intCast(cases.len);

    var size = 8 + max_payload_size;
    const rem = size % alignment;
    if (rem > 0) {
        size += alignment - rem;
    }

    const sym = choice_t.base.sym();
    choice_t.size = @intCast(size);
    choice_t.alignment = alignment;
    choice_t.fields_arr[0] = .{ .sym = undefined, .type = c.sema.i64_t, .offset = 0 };
    choice_t.fields_arr[1] = .{ .sym = undefined, .type = c.sema.void_t, .offset = 8 };

    choice_t.base.info.managed = has_managed_payload;

    try resolve_type_info(c, sym, &choice_t.base.info, has_copy_payload, has_no_copy_payload, decl.name);

    if (has_ct_child) {
        choice_t.base.info.ct = true;
    }
    choice_t.base.info.resolved = true;
}

fn resolveTypeIdFromDecl(c: *cy.Chunk, src_chunk: *cy.Chunk, default_name: []const u8, attrs: []const *ast.Attribute, decl: ?*ast.Node) !cy.TypeId {
    if (ast.findAttr(attrs, .bind)) |attr| {
        const name = (try attr.getString()) orelse default_name;
        return load_type(c, src_chunk, name, decl);
    }
    return c.sema.reserveType();
}

const PreTypeInfo = struct {
    type_id: cy.TypeId,
    host_t: ?C.BindType,
};

/// Only allows binding a predefined host type id (BIND_TYPE_DECL).
pub fn load_type(c: *cy.Chunk, src_chunk: *cy.Chunk, bind_name: []const u8, node: ?*ast.Node) !cy.TypeId {
    log.tracev("Invoke type loader for: {s}", .{bind_name});
    const binding = src_chunk.host_types.get(bind_name) orelse {
        return c.reportErrorFmt("Failed to load @host type `{}`.", &.{v(bind_name)}, node);
    };

    if (binding.type != C.BindTypeDecl) {
        return error.Unsupported;
    }

    var type_id: cy.TypeId = undefined;
    if (binding.data.decl.type_id != cy.NullId) {
        type_id = binding.data.decl.type_id;
    } else {
        type_id = try c.sema.reserveType();
    }
    return type_id;
}

/// Allow an explicit `opt_header_decl` so that template specialization can use it to override @host attributes.
pub fn reserve_template_instance(c: *cy.Chunk, template: *cy.sym.Template, opt_header_decl: ?*ast.Node, instance: *cy.Instance) !*cy.sym.Sym {
    const name = template.head.name();

    switch (template.kind) {
        .struct_t => {
            const decl = template.decl.child_decl.cast(.struct_decl);
            const src_chunk = c.compiler.chunks.items[template.head.declNode().src()];

            const header_decl = opt_header_decl orelse template.decl.child_decl;
            const type_id = try resolveTypeIdFromDecl(c, src_chunk, name, header_decl.cast(.struct_decl).attrs.slice(), header_decl);
            const new_t = try c.sema.createTypeWithId(.struct_t, type_id, .{ .cstruct = false, .opaque_t = false, .tuple = decl.is_tuple });
            const sym = try c.createTypeSym(@ptrCast(template), name, new_t, @ptrCast(decl));
            sym.instance = instance;
            return &sym.head;
        },
        .custom_t => {
            const header_decl = opt_header_decl orelse template.decl.child_decl;
            const custom_t = try resolve_custom_type(c, header_decl.cast(.custom_type_decl), instance);
            return custom_t;
        },
        .trait_t => {
            const decl = template.decl.child_decl;
            const header_decl = opt_header_decl orelse template.decl.child_decl;

            const type_id = try resolveTypeIdFromDecl(c, c, name, header_decl.cast(.trait_decl).attrs.slice(), header_decl);
            const new_t = try c.sema.createTypeWithId(.generic_trait, type_id, .{
                .impls = undefined,
            });
            new_t.cast(.generic_trait).zimpls().* = .{};
            new_t.info.generic = true;
            const sym = try c.createTypeSym(@ptrCast(template), name, new_t, decl);
            sym.instance = instance;
            return &sym.head;
        },
        .enum_t => {
            const decl = template.decl.child_decl.cast(.enumDecl);
            var sym: *cy.sym.TypeSym = undefined;
            if (decl.isChoiceType) {
                const tag_t = try c.sema.createType(.enum_t, .{});
                const new_t = try c.sema.createType(.choice, .{ .tag_t = tag_t });
                sym = try c.createTypeSym(@ptrCast(template), name, new_t, @ptrCast(decl));

                _ = try c.reserveTypeSym(&sym.head, "Tag", tag_t, @ptrCast(decl));
            } else {
                const new_t = try c.sema.createType(.enum_t, .{});
                sym = try c.createTypeSym(@ptrCast(template), name, new_t, @ptrCast(decl));
            }
            sym.instance = instance;

            return @ptrCast(sym);
        },
        .type_alias => {
            const decl = template.decl.child_decl.cast(.type_alias_decl);
            const sym = try c.createTypeAlias(@ptrCast(template), name, decl.hidden, decl);
            sym.instance = instance;
            return @ptrCast(sym);
        },
        .type_const => {
            const decl = template.decl.child_decl.cast(.type_const_decl);
            const sym = try c.createTypeConst(@ptrCast(template), name, decl.hidden, decl);
            sym.instance = instance;
            return @ptrCast(sym);
        },
    }
}

pub fn resolve_all_types_after(c: *cy.Chunk, start: usize, node: *ast.Node) !void {
    var idx = start;
    while (idx < c.syms.items.len) : (idx += 1) {
        const sym = c.syms.items[idx];
        switch (sym.type) {
            .type => {
                const type_sym = sym.cast(.type);
                try ensure_resolved_type(c, type_sym.type, node);
            },
            .type_alias => {
                try resolve_type_alias(c, @ptrCast(sym));
            },
            .type_const => {
                try resolve_type_const(c, sym.cast(.type_const));
            },
            else => {},
        }
    }
}

pub fn resolve_type_sym(c: *cy.Chunk, sym: *cy.Sym, node: *ast.Node) anyerror!void {
    switch (sym.type) {
        .type => {
            const type_sym = sym.cast(.type);
            try ensure_resolved_type(c, type_sym.type, node);
        },
        .type_alias => {
            try resolve_type_alias(c, @ptrCast(sym));
        },
        .type_const => {
            try resolve_type_const(c, sym.cast(.type_const));
        },
        else => {
            return error.Unsupported;
        },
    }
}

pub fn reserveCUnion(c: *cy.Chunk, node: *ast.CUnionDecl) !*cy.sym.TypeSym {
    const name = node.name.as_name();
    const type_id = try resolveTypeIdFromDecl(c, c, name, node.attrs.slice(), @ptrCast(node));
    const new_t = try c.sema.createTypeWithId(.c_union, type_id, .{
    });
    const sym = try c.reserveTypeSym(@ptrCast(c.sym), name, new_t, @ptrCast(node));
    return @ptrCast(sym);
}

pub fn reserveStruct(c: *cy.Chunk, node: *ast.StructDecl, cstruct: bool) !*cy.sym.TypeSym {
    const name = node.name.as_name();
    const type_id = try resolveTypeIdFromDecl(c, c, name, node.attrs.slice(), @ptrCast(node));
    const new_t = try c.sema.createTypeWithId(.struct_t, type_id, .{
        .cstruct = cstruct,
        .opaque_t = false,
        .tuple = node.is_tuple,
    });
    const sym = try c.reserveTypeSym(@ptrCast(c.sym), name, new_t, @ptrCast(node));
    return @ptrCast(sym);
}

pub const StructField = extern struct {
    name_ref: u64,
    type: *cy.Type,
    offset: i64,
    state_offset: i64,

    pub fn name(self: *const StructField) *cy.heap.EvalStr {
        const addr: usize = @intCast(self.name_ref);
        return @ptrFromInt(addr);
    }
};

pub fn reifyStructType(c: *cy.Chunk, struct_t: *cy.types.Struct, user_fields: []const StructField, node: *ast.Node) !void {
    // Alignment follows C-ABI rules.
    var has_managed_child = false;
    var alignment: u8 = 1;
    var field_state_len: usize = 0;
    const fields = try c.alloc.alloc(cy.types.Field, user_fields.len);
    errdefer c.alloc.free(fields);

    for (user_fields, 0..) |field, i| {
        const field_name = field.name().slice();
        const field_t = field.type;
        try ensure_resolved_type(c, field_t, node);
        if (field_t.isManaged()) {
            has_managed_child = true;
        }

        const member_alignment = field_t.alignment();
        if (member_alignment > alignment) {
            alignment = member_alignment;
        }

        const name_dup = try c.alloc.dupe(u8, field_name);
        try c.parser.ast.strs.append(c.alloc, name_dup);
        const sym = try c.declareField(@ptrCast(struct_t.base.sym()), name_dup, i, field_t, node);
        fields[i] = .{
            .sym = @ptrCast(sym),
            .type = field_t,
            .offset = 0,
            .state_offset = @intCast(field_state_len),
        };
        // if (field.init) |init| {
        //     // Ensure compile-time value.
        //     const init_val = try cte.evalCheck(c, init, field_t);
        //     defer c.heap.destructValue(init_val);

        //     // Generate reusable IR.
        //     const expr = try ct_inline.value(c, init_val, null, init);
        //     fields[i].init = expr.ir;
        //     fields[i].init_n = init;
        // }

        if (field_t.kind() == .struct_t) {
            field_state_len += field_t.cast(.struct_t).field_state_len;
        } else {
            field_state_len += 1;
        }
    }

    // After alignment is determined, compute size and field offsets.
    var offset: usize = 0;
    for (fields) |*field| {
        const field_alignment = field.type.alignment();
        const rem = offset % field_alignment;
        if (rem != 0) {
            // Align forward.
            offset += field_alignment - rem;
        }
        field.offset = @intCast(offset);
        offset += field.type.size();
    }

    const rem = offset % alignment;
    if (rem > 0) {
        offset += alignment - rem;
    }

    struct_t.alignment = alignment;
    struct_t.size = @intCast(offset);
    struct_t.fields_ptr = fields.ptr;
    struct_t.fields_len = @intCast(fields.len);
    struct_t.field_state_len = field_state_len;
    struct_t.resolving_struct = false;

    const mod = struct_t.base.sym().getMod();
    const has_user_deinit = mod.getSym("@deinit") != null;
    struct_t.base.info.managed = has_managed_child or has_user_deinit;

    const has_user_copy = mod.getSym("@copy") != null;
    struct_t.base.info.copy_base = has_managed_child;
    struct_t.base.info.copy_user = has_user_copy;

    struct_t.impls_ptr = undefined;
    struct_t.impls_len = 0;

    struct_t.base.info.resolved = true;
}

pub fn resolveStructType(c: *cy.Chunk, struct_t: *cy.types.Struct, node: *ast.Node) !void {
    if (struct_t.base.isResolved()) {
        return;
    }
    const sym = struct_t.base.sym();
    try sema.pushSymResolveContext(c, @ptrCast(sym), node);
    defer sema.popResolveContext(c);

    var decl: *ast.StructDecl = undefined;
    if (struct_t.cstruct) {
        decl = sym.decl.?.cast(.cstruct_decl);
    } else {
        decl = sym.decl.?.cast(.struct_decl);
    }
    try resolve_type_impls(c, &struct_t.base, decl.impls.slice());
    try resolveStructFields(c, struct_t, decl); 
    struct_t.base.info.resolved = true;

    // TODO: Create builtin @default_ctor that accepts non-default fields as a tuple.
}

fn indexOfTypedField(fields: []const *ast.Field, start: usize) ?usize {
    for (fields[start..], start..) |field, i| {
        if (field.typeSpec != null) {
            return i;
        }
    }
    return null;
}

/// Explicit `decl` node for distinct type declarations. Must belong to `c`.
pub fn resolveStructFields(c: *cy.Chunk, struct_t: *cy.types.Struct, decl: *ast.StructDecl) !void {
    struct_t.resolving_struct = true;
    std.debug.assert(!struct_t.base.isResolved());

    // Load fields.

    // Alignment follows C-ABI rules.
    var has_borrow_child = false;
    var has_managed_child = false;
    var has_copy_child = false;
    var has_no_copy_child = false;
    var has_ct_child = false;
    var alignment: u8 = 1;
    var field_state_len: usize = 0;
    const fields = try c.alloc.alloc(cy.types.Field, decl.fields.len);
    errdefer c.alloc.free(fields);

    // Track embedded fields separately
    var embedded_i: usize = 0;
    var embedded_fields = try c.alloc.alloc(cy.types.EmbeddedFieldInfo, decl.num_embedded_fields);
    errdefer c.alloc.free(embedded_fields);

    var field_group_t: ?*cy.Type = null;
    var field_group_end: usize = undefined;
    for (decl.fields.slice(), 0..) |field, i| {
        const fieldName = field.name.as_name();
        var field_t: *cy.Type = undefined;

        if (field.typeSpec == null) {
            if (field_group_t == null or i > field_group_end) {
                // Attempt to find group type.
                field_group_end = indexOfTypedField(decl.fields.slice(), i + 1) orelse {
                    return c.reportError("Expected field type.", @ptrCast(field));
                };
                const type_field = decl.fields.ptr[field_group_end];
                field_group_t = try cte.eval_type2(c, true, type_field.typeSpec.?);
                try ensure_resolved_type(c, field_group_t.?, @ptrCast(type_field.typeSpec));
            }
            field_t = field_group_t.?;
        } else {
            field_t = try cte.eval_type2(c, true, field.typeSpec.?);
            try ensure_resolved_type(c, field_t, @ptrCast(field.typeSpec));
        }

        if (field_t.isManaged()) {
            has_managed_child = true;
        }
        if (field_t.has_copy_ctor()) {
            has_copy_child = true;
        }
        if (field_t.info.no_copy) {
            has_no_copy_child = true;
        }
        if (field_t.info.ct) {
            has_ct_child = true;
        }
        if (field_t.info.borrow_only) {
            has_borrow_child = true;
        }

        const member_alignment = field_t.alignment();
        if (member_alignment > alignment) {
            alignment = member_alignment;
        }

        // Validate embedded type is an object type
        if (field.embedded) {
            if (field_t.kind() != .struct_t) {
                return c.reportErrorFmt(
                    "Embedded field must be a struct type, got {s}",
                    &.{v(@tagName(field_t.kind()))},
                    @ptrCast(field)
                );
            }
            
            embedded_fields[embedded_i] = .{
                .field_idx = @intCast(i),
                .embedded_type = field_t,
            };
            embedded_i += 1;
        }

        const sym = try c.declareField(@ptrCast(struct_t.base.sym()), fieldName, i, field_t, @ptrCast(field));
        fields[i] = .{
            .sym = @ptrCast(sym),
            .type = field_t,
            .offset = 0,
            .state_offset = @intCast(field_state_len),
            .init_n = field.init,
        };

        if (field_t.kind() == .struct_t) {
            field_state_len += field_t.cast(.struct_t).field_state_len;
        } else {
            field_state_len += 1;
        }
    }

    // After alignment is determined, compute size and field offsets.
    var offset: usize = 0;
    for (fields) |*field| {
        const field_alignment = field.type.alignment();
        const rem = offset % field_alignment;
        if (rem != 0) {
            // Align forward.
            offset += field_alignment - rem;
        }
        field.offset = @intCast(offset);
        offset += field.type.size();
    }

    const rem = offset % alignment;
    if (rem > 0) {
        offset += alignment - rem;
    }

    struct_t.alignment = alignment;
    struct_t.size = @intCast(offset);
    struct_t.fields_ptr = fields.ptr;
    struct_t.fields_len = @intCast(fields.len);
    struct_t.field_state_len = field_state_len;
    struct_t.embedded_fields_ptr = embedded_fields.ptr;
    struct_t.embedded_fields_len = @intCast(embedded_fields.len);
    struct_t.resolving_struct = false;

    struct_t.base.info.borrow_only = has_borrow_child;

    const sym = struct_t.base.sym();
    const has_user_deinit = sym.has_decl("@deinit");
    struct_t.base.info.managed = has_managed_child or has_user_deinit;

    try resolve_type_info(c, sym, &struct_t.base.info, has_copy_child, has_no_copy_child, decl.name);

    if (has_ct_child) {
        struct_t.base.info.ct = true;
    }
}

/// For Option/Result.
fn resolve_type_info2(c: *cy.Chunk, info: *cy.types.TypeInfo, has_copy_child: bool, has_no_copy_child: bool) !void {
    _ = c;
    if (has_no_copy_child) {
        info.no_copy = true;
    }

    if (!info.no_copy) {
        info.copy_base = has_copy_child;
        info.copy_user = false;
    }
}

fn resolve_type_info(c: *cy.Chunk, sym: *cy.sym.TypeSym, info: *cy.types.TypeInfo, has_copy_child: bool, has_no_copy_child: bool, node: *ast.Node) !void {
    const has_user_copy = sym.has_decl("@copy");

    if (info.no_copy) {
        if (has_user_copy) {
            return c.reportError("Cannot override `@copy` if type implements `NoCopy`.", node);
        }
    } else {
        if (has_no_copy_child) {
            info.no_copy = true;
        }
    }

    if (!info.no_copy) {
        info.copy_base = has_copy_child;
        info.copy_user = has_user_copy;
    }
}

pub fn resolve_type_impls(c: *cy.Chunk, type_: *cy.Type, impl_decls: []*ast.ImplDecl) !void {
    const impls = try c.alloc.alloc(cy.types.Impl, impl_decls.len);
    errdefer c.alloc.free(impls);

    var no_copy = false;
    for (impl_decls, 0..) |impl, i| {
        const trait_t = try cte.eval_type2(c, false, impl.trait);
        if (trait_t.kind() != .generic_trait) {
            return c.reportErrorFmt("Expected `{}` to be trait type. Found {}.", &.{v(trait_t.name()), v(trait_t.kind())}, @ptrCast(impl.trait));
        }
        if (trait_t.id() == bt.NoCopy) {
            no_copy = true;
        }

        impls[i] = .{
            .trait = trait_t.cast(.generic_trait),
            .funcs = &.{},
        };

        // Ensure the methods are reserved if this is a template type instance.
        if (type_.sym().instance != null) {
            try ensure_resolved_type(c, trait_t, impl.trait);
            for (trait_t.cast(.generic_trait).members()) |member| {
                _ = try c.getResolvedSym(&type_.sym().head, member.func.name(), impl.trait);
            }
        }
    }

    type_.info.no_copy = no_copy;

    switch (type_.kind()) {
        .struct_t => {
            const struct_t = type_.cast(.struct_t);
            struct_t.impls_ptr = impls.ptr;
            struct_t.impls_len = @intCast(impls.len);
        },
        else => {
            return error.TODO;
        },
    }
}

pub fn resolveFuncType(c: *cy.Chunk, func_type: *ast.FuncType) !*cy.FuncSig {
    const start = c.func_param_stack.items.len;
    defer c.func_param_stack.items.len = start;

    for (func_type.params.slice()) |param| {
        var param_t: *cy.Type = undefined;
        if (param.type) |type_spec| {
            param_t = try cte.eval_type2(c, false, type_spec);
        } else {
            param_t = try cte.eval_type2(c, false, param.name_type);
        }

        try c.func_param_stack.append(c.alloc, .init(param_t));
    }

    // Get return type.
    const ret_t = try resolveReturnType(c, func_type.ret);
    return c.sema.ensureFuncSig2(@ptrCast(c.func_param_stack.items[start..]), ret_t, func_type.extern_ != null, null);
}

pub fn resolvePartialVectorType(c: *cy.Chunk, array_t: *cy.types.PartialVector, decl: *ast.Node) !void {
    if (array_t.base.isResolved()) {
        return;
    }

    try ensure_resolved_type(c, array_t.elem_t, decl);
    const has_managed_child = array_t.elem_t.isManaged();
    const has_copy_child = array_t.elem_t.has_copy_ctor();
    const elem_size = array_t.elem_t.size();
    array_t.size = 8 + elem_size * array_t.n;

    array_t.base.info.managed = has_managed_child;
    array_t.base.info.copy_base = has_copy_child;
    array_t.base.info.copy_user = false;
    array_t.base.info.resolved = true;
}

pub fn resolveVectorType(c: *cy.Chunk, array_t: *cy.types.Vector, decl: *ast.Node) !void {
    if (array_t.base.isResolved()) {
        return;
    }

    try ensure_resolved_type(c, array_t.elem_t, decl);
    const has_managed_child = array_t.elem_t.isManaged();
    const has_copy_child = array_t.elem_t.has_copy_ctor();
    const elem_size = array_t.elem_t.size();
    array_t.size = elem_size * array_t.n;

    array_t.base.info.managed = has_managed_child;
    array_t.base.info.copy_base = has_copy_child;
    array_t.base.info.copy_user = false;
    array_t.base.info.resolved = true;
}

pub fn resolveResultType(c: *cy.Chunk, res_t: *cy.types.Result, decl: *ast.Node) !void {
    if (res_t.base.isResolved()) {
        return;
    }

    // Tag field.
    const alignment: u8 = 8;
    var size: usize = 8;

    // Payload field.
    try ensure_resolved_type(c, res_t.child_t, decl);
    const has_managed_payload = res_t.child_t.isManaged();
    const has_copy_payload = res_t.child_t.has_copy_ctor();
    const has_no_copy_payload = res_t.child_t.info.no_copy;
    var payload_size = res_t.child_t.size();
    if (payload_size < 8) {
        payload_size = 8;
    }
    size += payload_size;

    if (res_t.child_t.alignment() > alignment) {
        std.debug.panic("Unsupported alignment.", .{});
    }

    const rem = size % alignment;
    if (rem > 0) {
        size += alignment - rem;
    }

    const sym = res_t.base.sym();
    _ = sym;
    // _ = try c.declareChoiceCase(@ptrCast(sym), "none", @ptrCast(res_t), 0, c.sema.void_t, decl);
    // _ = try c.declareChoiceCase(@ptrCast(sym), "some", @ptrCast(res_t), 1, res_t.child_t, decl);

    res_t.size = @intCast(size);
    res_t.alignment = alignment;
    res_t.fields_arr[0] = .{ .sym = undefined, .type = c.sema.i64_t, .offset = 0 };
    res_t.fields_arr[1] = .{ .sym = undefined, .type = res_t.child_t, .offset = 8 };

    res_t.cases_arr[0] = .{ .sym=null, .name_ptr="error", .name_len="error".len, .payload_t = c.sema.error_t, .val = 0 };
    res_t.cases_arr[1] = .{ .sym=null, .name_ptr="result", .name_len="result".len, .payload_t = res_t.child_t, .val = 1 };

    res_t.base.info.managed = has_managed_payload;

    try resolve_type_info2(c, &res_t.base.info, has_copy_payload, has_no_copy_payload);

    res_t.base.info.resolved = true;
}

pub fn resolveOptionType(c: *cy.Chunk, option_t: *cy.types.Option, decl: *ast.Node) !void {
    if (option_t.base.isResolved()) {
        return;
    }

    if (option_t.zero_union) {
        // Payload field.
        try ensure_resolved_type(c, option_t.child_t, decl);
        std.debug.assert(option_t.child_t.size() == 8 or option_t.child_t.size() == 4);
        option_t.size = @intCast(option_t.child_t.size());
        option_t.alignment = option_t.child_t.alignment();
    } else {
        // Tag field.
        const alignment: u8 = 8;
        var size: usize = 8;

        // Payload field.
        try ensure_resolved_type(c, option_t.child_t, decl);
        size += option_t.child_t.size();

        if (option_t.child_t.alignment() > alignment) {
            std.debug.panic("Unsupported alignment.", .{});
        }

        const rem = size % alignment;
        if (rem > 0) {
            size += alignment - rem;
        }
        option_t.size = @intCast(size);
        option_t.alignment = alignment;
        option_t.fields_arr[0] = .{ .sym = undefined, .type = c.sema.i64_t, .offset = 0 };
        option_t.fields_arr[1] = .{ .sym = undefined, .type = option_t.child_t, .offset = 8 };
    }
    const has_managed_child = option_t.child_t.isManaged();
    const has_copy_child = option_t.child_t.has_copy_ctor();
    const has_no_copy_payload = option_t.child_t.info.no_copy;

    option_t.base.info.managed = has_managed_child;

    try resolve_type_info2(c, &option_t.base.info, has_copy_child, has_no_copy_payload);

    option_t.base.info.resolved = true;
}

pub fn reserve_trait_type(c: *cy.Chunk, decl: *ast.TraitDecl) !*cy.sym.TypeSym {
    const name = decl.name.as_name();

    const type_id = try resolveTypeIdFromDecl(c, c, name, decl.attrs.slice(), @ptrCast(decl));
    const new_t = try c.sema.createTypeWithId(.generic_trait, type_id, .{
        .impls = undefined,
    });
    new_t.info.generic = true;
    new_t.cast(.generic_trait).zimpls().* = .{};
    const sym = try c.reserveTypeSym(@ptrCast(c.sym), name, new_t, @ptrCast(decl));
    return sym;
}

fn findInferParams(c: *cy.Chunk, node: *ast.Node) !void {
    switch (node.type()) {
        .infer_param => {
            const name = node.cast(.infer_param).name.as_name();
            const ctx = sema.getResolveContext(c);

            // Building template params for `resolveFuncSig`.
            try c.func_template_param_stack.append(c.alloc, .{
                .anonymous = false,
                .name = name,
                .type_idx = undefined,
            });

            const value = cy.TypeValue.init(c.sema.type_t, cy.Value.initPtr(c.sema.any_t));
            try ctx.initCtParam(c.alloc, name, value);
        },
        .index_expr => {
            const index_expr = node.cast(.index_expr);
            for (index_expr.args.slice()) |arg| {
                try findInferParams(c, arg);
            }
        },
        .ref => {
            const ref = node.cast(.ref);
            try findInferParams(c, ref.child);
        },
        .borrow => {
            const borrow = node.cast(.borrow);
            try findInferParams(c, borrow.child);
        },
        .span_type => {
            const span_type = node.cast(.span_type);
            try findInferParams(c, span_type.child);
        },
        .slice_type => {
            const slice_type = node.cast(.slice_type);
            try findInferParams(c, slice_type.child);
        },
        .vector_type => {
            const vector_t = node.cast(.vector_type);
            try findInferParams(c, vector_t.child);
            try findInferParams(c, vector_t.n);
        },
        .partial_vector_type => {
            const vector_t = node.cast(.partial_vector_type);
            try findInferParams(c, vector_t.child);
            try findInferParams(c, vector_t.n);
        },
        else => {
        },
    }
}

/// Handles generic and dependent types.
pub fn resolveAnyParamType(c: *cy.Chunk, param_idx: usize, resolve_new_type: bool, node: *ast.Node, comptime variant: bool) !*cy.Type {
    const res = cte.eval_type2(c, resolve_new_type, node) catch |err| {
        if (err == error.FoundDependentType) {
            try findInferParams(c, node);
            return c.sema.dependent_t;
        } else if (err == error.FoundInferParam) {
            try findInferParams(c, node);
            return c.sema.infer_t;
        } else {
            return err;
        }
    };
    if (res.is_generic()) {
        if (!variant) {
            // Push a canonical name based on the param index.
            try c.func_template_param_stack.append(c.alloc, .{
                .anonymous = true,
                .name = try std.fmt.allocPrint(c.alloc, "{}", .{param_idx}),
                .type_idx = undefined,
            });
        } else {
            // Lookup anonymous parameter.
            var buf: [2]u8 = undefined;
            const anonymous_name = try std.fmt.bufPrint(&buf, "{}", .{param_idx});
            return sema.getResolveContext(c).data.func.instance.?.getParam(anonymous_name).?.value.asPtr(*cy.Type);
        }
    }
    return res;
}

pub fn resolveReturnType(c: *cy.Chunk, node: ?*ast.Node) anyerror!*cy.Type {
    const n = node orelse {
        return c.sema.void_t;
    };
    return cte.evalType(c, n) catch |err| {
        if (err == error.FoundDependentType) {
            return c.sema.dependent_t;
        } else {
            return err;
        }
    };
}

/// Handles generic and dependent types.
pub fn resolve_return_type_spec(c: *cy.Chunk, node: ?*ast.Node, resolve_new_type: bool) anyerror!*cy.Type {
    const n = node orelse {
        return c.sema.void_t;
    };
    return cte.eval_type2(c, resolve_new_type, n) catch |err| {
        if (err == error.FoundDependentType) {
            return c.sema.dependent_t;
        } else {
            return err;
        }
    };
}

fn resolveCopiedType(c: *cy.Chunk, sym: *cy.sym.TypeSym, src_t: *cy.Type, node: *ast.Node) !void {
    switch (src_t.kind()) {
        .partial_vector,
        .vector,
        .pointer,
        .hostobj,
        .int => {},
        .struct_t => {
            const struct_t = src_t.cast(.struct_t);
            // Declare fields in new sym's module.
            for (struct_t.fields(), 0..) |field, i| {
                const field_name = field.sym.head.name();
                _ = try c.declareField(@ptrCast(sym), field_name, i, field.type, node);
            }

            const copy_t = sym.type.cast(.struct_t);
            copy_t.impls_len = 0;
        },
        else => {
            return c.reportErrorFmt("TODO: {}", &.{v(src_t.kind())}, node);
        },
    }
}

pub fn implements(c: *cy.Chunk, impl_t: *cy.Type, trait: *cy.types.GenericTrait, node: *ast.Node) !bool {
    const impls = trait.zimpls();
    if (impls.contains(impl_t)) {
        return true;
    }

    var res = false;
    switch (impl_t.kind()) {
        .struct_t => {
            res = impl_t.cast(.struct_t).implements(trait);
        },
        else => {
        },
    }

    if (!res) {
        if (trait.members_len == 0) {
            // Check implicit trait.
            if (trait.predicate) |predicate| {
                const ct_res = try cte.evalCall(c, predicate, &.{cy.Value.initPtr(impl_t)}, node);
                if (ct_res.resType != .ct_value) {
                    return c.reportErrorFmt("Expected compile-time value.", &.{}, node);
                }
                if (ct_res.data.ct_value.value.asBool()) {
                    res = true;
                }
            }
        }
    }
    if (res) {
        try impls.put(c.alloc, impl_t, {});
    }
    return res;
}

pub fn ensure_eval_eligible_type(c: *cy.Chunk, type_: *cy.Type, node: *ast.Node) !void {
    if (type_.isManaged()) {
        const name = try c.sema.allocTypeName(type_);
        defer c.alloc.free(name);
        return c.reportErrorFmt("Expected eval eligible type, found `{}`.", &.{v(name)}, node);
    }
}
