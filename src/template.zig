const std = @import("std");
const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypes;
const ast = cy.ast;
const sema = cy.sema;
const sema_type = cy.sema_type;
const cte = cy.cte;
const v = cy.fmt.v;

const InstanceType = enum(u8) {
    sym,
    func,
};

pub const Instance = struct {
    type: InstanceType,

    param_types: []const *cy.Type,

    /// Owned params. Can be used to print params along with the template type.
    /// Separated from param names since `params` is used as a variant cache key.
    params: []const cy.Value,

    map: union {
        two: struct {
            first: []const u8,
            second: []const u8,
        },
        /// A map is created if more than 2 params.
        map: std.StringHashMapUnmanaged(usize),
    } = undefined,

    data: union {
        sym: struct {
            /// Back link to the template.
            /// Can be used to check what template the symbol comes from. Also used for pushing the variant context.
            template: *cy.sym.Template,
            sym: *cy.Sym,
        },
        func: struct {
            template: *cy.sym.FuncTemplate,
            func: *cy.Func,
        },
    },

    pub fn initFuncParams(self: *Instance, alloc: std.mem.Allocator, params: []const cy.Value, template_params: []const cy.sym.FuncTemplateParam) !void {
        self.params = params;
        if (self.params.len > 2) {
            self.map = .{ .map = .{} };
            for (0..params.len) |i| {
                try self.map.map.put(alloc, template_params[i].name, i);
            }
        } else {
            self.map = .{ .two = undefined };
            self.map.two.first = template_params[0].name;
            if (self.params.len == 2) {
                self.map.two.second = template_params[1].name;
            }
        }
    }

    pub fn initParams(self: *Instance, alloc: std.mem.Allocator, params: []const cy.Value, template_params: []const cy.sym.TemplateParam) !void {
        self.params = params;
        if (self.params.len > 2) {
            self.map = .{ .map = .{} };
            for (0..self.params.len) |i| {
                try self.map.map.put(alloc, template_params[i].name, i);
            }
        } else {
            self.map = .{ .two = undefined };
            self.map.two.first = template_params[0].name;
            if (self.params.len == 2) {
                self.map.two.second = template_params[1].name;
            }
        }
    }

    pub fn getParamAt(self: *Instance, idx: usize) cy.TypeValue {
        return cy.TypeValue.init(self.param_types[idx], self.params[idx]);
    }

    pub fn getParamValue(self: *Instance, name: []const u8) ?cy.Value {
        const res = self.getParam(name) orelse return null;
        return res.value;
    }

    pub fn getParam(self: *Instance, name: []const u8) ?cy.TypeValue {
        if (self.params.len > 2) {
            const idx = self.map.map.get(name) orelse return null;
            return cy.TypeValue.init(self.param_types[idx], self.params[idx]);
        } else {
            if (std.mem.eql(u8, self.map.two.first, name)) {
                return cy.TypeValue.init(self.param_types[0], self.params[0]);
            }
            if (self.params.len == 2) {
                if (std.mem.eql(u8, self.map.two.second, name)) {
                    return cy.TypeValue.init(self.param_types[1], self.params[1]);
                }
            }
            return null;
        }
    }

    pub fn getSymTemplate(self: *Instance) *cy.sym.Template {
        if (self.type == .sym) {
            return self.data.sym.template;
        } else {
            std.debug.panic("Unexpected {}.", .{self.type});
        }
    }

    pub fn deinitValues(heap: *cy.Heap, variant: *Instance) void {
        for (0..variant.params.len) |i| {
            const param = variant.getParamAt(i);
            heap.destructValue(param);
        }
        if (variant.params.len > 2) {
            variant.map.map.deinit(heap.alloc);
        }
        heap.alloc.free(variant.param_types);
        heap.alloc.free(variant.params);
        variant.params = &.{};
    }
};

pub const VariantKey = struct {
    /// Param types know how the params are interpreted.
    param_types: []const *cy.Type,
    params: []const cy.Value,
};

pub const VariantKeyContext = struct {
    pub fn hash(_: VariantKeyContext, key: VariantKey) u64 {
        var c = std.hash.Wyhash.init(0);
        const param_types = key.param_types;
        for (key.params, 0..) |val, i| {
            const param_t = param_types[i];
            switch (param_t.id()) {
                bt.Type => {
                    const typeId = val.asPtr(*cy.Type).id();
                    c.update(std.mem.asBytes(&typeId));
                },
                bt.PartialStructLayout => {
                    const typeId = val.asPtr(*sema.PartialStructLayout);
                    c.update(std.mem.asBytes(&typeId));
                },
                bt.FuncSig => {
                    c.update(std.mem.asBytes(&val.asPtr(*cy.FuncSig).id));
                },
                bt.I32 => {
                    c.update(std.mem.asBytes(&val.asI32()));
                },
                bt.I64 => {
                    c.update(std.mem.asBytes(&val.asInt()));
                },
                bt.EvalInt => {
                    c.update(std.mem.asBytes(&val.as_eval_int()));
                },
                bt.EvalStr => {
                    c.update(val.as_eval_str());
                },
                else => {
                    switch (param_t.kind()) {
                        .func_sym => {
                            c.update(std.mem.asBytes(&val.asPtr(*cy.Func)));
                        },
                        .enum_t => {
                            c.update(std.mem.asBytes(&val.asInt()));
                        },
                        else => {
                            std.debug.panic("Unsupported value hash: {} {}", .{param_t.kind(), param_t.id()});
                        },
                    }
                }
            }
        }
        return c.final();
    }

    pub fn eql(_: VariantKeyContext, a: VariantKey, b: VariantKey) bool {
        if (a.params.len != b.params.len) {
            return false;
        }
        for (a.params, 0..) |av, i| {
            const param_t = a.param_types[i];
            if (param_t != b.param_types[i]) {
                return false;
            }
            switch (param_t.id()) {
                bt.Type => {
                    if (av.asPtr(*cy.Type) != b.params[i].asPtr(*cy.Type)) {
                        return false;
                    }
                },
                bt.PartialStructLayout => {
                    if (av.asPtr(*sema.PartialStructLayout) != b.params[i].asPtr(*sema.PartialStructLayout)) {
                        return false;
                    }
                },
                bt.FuncSig => {
                    return av.asPtr(*cy.FuncSig) == b.params[i].asPtr(*cy.FuncSig);
                },
                bt.I64 => {
                    return av.asInt() == b.params[i].asInt();
                },
                bt.EvalInt => {
                    return av.as_eval_int() == b.params[i].as_eval_int();
                },
                bt.EvalStr => {
                    return std.mem.eql(u8, av.as_eval_str(), b.params[i].as_eval_str());
                },
                else => {
                    switch (param_t.kind()) {
                        .func_sym => {
                            return av.asPtr(*cy.Func) == b.params[i].asPtr(*cy.Func);
                        },
                        .enum_t => {
                            return av.asInt() == b.params[i].asInt();
                        },
                        else => {
                            std.debug.panic("Unsupported value comparison: {}", .{param_t.id()});
                        },
                    }
                },
            }
        }
        return true;
    }
};

// /// Only certain types can be used as template parameter values.
// pub fn checkTemplate(c: *cy.Chunk, value: cy.TypeValue) !ParamValue {
//     switch (value.type.id()) {
//         bt.Type => {
//             const type_ = value.value.asPtr(*cy.Type);
//             return ParamValue.initType(type_);
//         },
//         bt.Str => {
//             const new = try c.heap.alloc.dupe(u8, value.value.asString());
//             return ParamValue.initStr(new);
//         },
//         bt.I64 => {
//             return ParamValue.initInt(value.value.asInt());
//         },
//         bt.I32 => {
//             return ParamValue.initI32(value.value.asI32());
//         },
//         bt.GenericInt => {
//             return ParamValue.initGenericInt(value.value.asGenericInt());
//         },
//         else => {
//             switch (value.type.kind()) {
//                 .func_sym => {
//                     return ParamValue.initFnsym(value.value.asPtr(*cy.Func));
//                 },
//                 .enum_t => {
//                     return ParamValue.initInt(value.value.asInt());
//                 },
//                 else => {
//                     const name = try c.sema.allocTypeName(value.type);
//                     std.debug.panic("TODO: {s}", .{name});
//                 },
//             }
//         },
//     }
// }

pub fn expand_template(c: *cy.Chunk, template: *cy.sym.Template, arg_types: []const *cy.Type, args: []const cy.Value) anyerror!*cy.Sym {
    return expand_template2(c, template, arg_types, args, true, null);
}

pub fn expand_template2(c: *cy.Chunk, template: *cy.sym.Template, arg_types: []const *cy.Type, args: []const cy.Value, resolve_new_types: bool, node: ?*ast.Node) !*cy.Sym {
    try sema.ensureResolvedTemplate(c, template);

    // Ensure variant type.
    const key = VariantKey{ .param_types = arg_types, .params = args };
    const res = try template.instance_cache.getOrPutContext(c.alloc, key, .{});
    if (!res.found_existing) {
        // Copy args.
        const params = try c.alloc.alloc(cy.Value, args.len);
        for (0..params.len) |i| {
            params[i] = try c.heap.copyValue2(arg_types[i], args[i]);
        }
        const param_types = try c.alloc.dupe(*cy.Type, arg_types);

        // Generate type instance.
        const instance = try c.alloc.create(Instance);
        instance.* = .{
            .type = .sym,
            .param_types = param_types,
            .params = &.{},
            .map = undefined,
            .data = .{ .sym = .{
                .template = template,
                .sym = undefined,
            }},
        };
        try instance.initParams(c.alloc, params, template.params);
        res.key_ptr.* = .{ .param_types = param_types, .params = params };
        res.value_ptr.* = instance;
        try template.instances.append(c.alloc, instance);

        const new_sym = try sema_type.reserve_template_instance(c, template, template.decl.child_decl, instance);

        // Allow circular reference by resolving after the new symbol has been added to the cache.
        instance.data.sym.sym = new_sym;

        // When expanded inside a function body, the type is expected to be resolved.
        // Declaration sites only need the type handle and therefore don't need to be fully resolved.
        if (resolve_new_types) {
            const i = c.syms.items.len;
            try sema_type.resolve_type_sym(c, new_sym, node orelse template.decl.child_decl);
            if (c.syms.items.len > i) {
                try sema_type.resolve_all_types_after(c, i, node orelse template.decl.child_decl);
            }
        } else if (new_sym.type == .type_alias) {
            // Always resolve type alias so it points to a valid type.
            // This doesn't resolve the type itself.
            try sema_type.resolve_type_sym(c, new_sym, node orelse template.decl.child_decl);
        }

        return new_sym;
    } 

    const variant = res.value_ptr.*;
    return variant.data.sym.sym;
}

pub fn resolveTemplateArgs(c: *cy.Chunk, args: []const *ast.Node, template: *cy.sym.Template, template_ctx: *sema.ResolveContext, node: *ast.Node) !void {
    if (args.len != template.params.len) {
        return c.reportErrorFmt("Expected {} template parameters.", &.{v(template.params.len)}, node);
    }

    var param_group_t: ?*cy.Type = null;
    var param_group: *ast.FuncParam = undefined;
    var param_group_end: usize = undefined;
    for (args, 0..) |arg, i| {
        var param_t: *cy.Type = undefined;
        var eff_param: *ast.FuncParam = undefined;
        {
            try sema.pushSavedResolveContext(c, template_ctx.*);
            defer template_ctx.* = sema.saveResolveContext(c);

            const param = template.decl.params[i];
            if (param.type == null) {
                if (param_group_t == null or i > param_group_end) {
                    // Attempt to find group type.
                    param_group_end = sema.indexOfTypedParam(template.decl.params, i + 1) orelse {
                        return c.reportError("Expected parameter type.", @ptrCast(param));
                    };
                    param_group_t = try cte.eval_type2(c, false, template.decl.params[param_group_end].type.?);
                    param_group = template.decl.params[param_group_end];
                }
                param_t = param_group_t.?;
                eff_param = param_group;
            } else {
                param_t = try cte.eval_type2(c, false, param.type.?);
                eff_param = param;
            }
        }

        if (!eff_param.const_param and !param_t.is_generic()) {
            const name = try c.sema.allocTypeName(param_t);
            defer c.alloc.free(name);
            return c.reportErrorFmt("Expected generic type, found `{}`.", &.{v(name)}, eff_param.type);
        }

        const res = try evalParamValueCheck(c, arg, param_t, eff_param.const_param);

        try c.valueStack.append(c.alloc, res.value);

        if (eff_param.const_param) {
            try c.typeStack.append(c.alloc, param_t);
        } else {
            try c.typeStack.append(c.alloc, c.sema.type_t);
        }

        // Persist for subsequent parameters that depend on previously resolved parameters.
        const name = template.params[i].name;

        try template_ctx.initCtParam(c.alloc, name, res);
    }
}

pub fn evalParamValueCheck(c: *cy.Chunk, node: *ast.Node, target_t: *cy.Type, const_param: bool) !cy.TypeValue {
    const cstr: cte.ExprCstr = if (const_param) .init_check(target_t) else .init_check_generic(target_t);
    const expr = try cte.evalExpr(c, node, cstr);
    switch (expr.kind) {
        .ct_var,
        .sym,
        .func => {
            return error.Unexpected;
        },
        .value => {
            return expr.data.value;
        },
    }
}

pub fn expand_template_ast(c: *cy.Chunk, template: *cy.sym.Template, args: []const *ast.Node, node: *ast.Node) !*cy.Sym {
    return expand_template_ast2(c, template, args, true, node);
}

pub fn expand_template_ast2(c: *cy.Chunk, template: *cy.sym.Template, args: []const *ast.Node, resolve_new_type: bool, node: *ast.Node) !*cy.Sym {
    try sema.ensureResolvedTemplate(c, template);

    // Prepare template context.
    try sema.pushSymResolveContext(c, @ptrCast(template), @ptrCast(template.decl));
    var template_ctx = sema.saveResolveContext(c);
    template_ctx.has_ct_params = true;
    template_ctx.prefer_ct_type = true;
    defer template_ctx.deinit(c);

    // Accumulate params.
    const param_start = c.valueStack.items.len;
    const param_type_start = c.typeStack.items.len;
    defer {
        const values = c.valueStack.items[param_start..];
        const param_types = c.typeStack.items[param_type_start..];
        for (values, 0..) |val, i| {
            c.heap.destructValue2(param_types[i], val);
        }
        c.valueStack.items.len = param_start;
        c.typeStack.items.len = param_type_start;
    }

    try resolveTemplateArgs(c, args, template, &template_ctx, node);

    const arg_types = c.typeStack.items[param_type_start..];
    const arg_vals = c.valueStack.items[param_start..];
    return expand_template2(c, template, arg_types, arg_vals, resolve_new_type, node);
}

pub fn expandTemplateFuncForArgs(c: *cy.Chunk, template: *cy.sym.FuncTemplate, args: []const *ast.Node, node: *ast.Node) !*cy.Func {
    try sema.ensureResolvedFuncTemplate(c, template);

    if (template.params.len != args.len) {
        return c.reportErrorFmt("Expected {} arguments. Found {}.", &.{v(template.params.len), v(args.len)}, node);
    }

    // Accumulate compile-time args.
    const value_start = c.valueStack.items.len;
    const param_type_start = c.typeStack.items.len;
    defer {
        const values = c.valueStack.items[value_start..];
        const param_types = c.typeStack.items[param_type_start..];
        for (values, 0..) |val, i| {
            c.heap.destructValue2(param_types[i], val);
        }
        c.valueStack.items.len = value_start;
        c.typeStack.items.len = param_type_start;
    }

    if (template.callable) {
        return c.reportErrorFmt("Can not expand generic function.", &.{}, node);
    }
    const decl = template.decl.cast(.template);
    for (args, 0..) |arg, i| {
        const type_idx = template.params[i].type_idx;
        const param_t = try cte.eval_type2(c, false, decl.params[type_idx].type.?);
        const res = try evalParamValueCheck(c, arg, param_t, decl.params[type_idx].const_param);
        try c.valueStack.append(c.alloc, res.value);

        if (decl.params[type_idx].const_param) {
            try c.typeStack.append(c.alloc, param_t);
        } else {
            try c.typeStack.append(c.alloc, c.sema.type_t);
        }
    }

    const arg_types = c.typeStack.items[param_type_start..];
    const arg_vals = c.valueStack.items[value_start..];
    return expandResolvedFuncTemplate(c, template, arg_types, arg_vals, true, node);
}

pub fn expandFuncTemplate(c: *cy.Chunk, template: *cy.sym.FuncTemplate, param_types: []const *cy.Type, args: []const cy.Value, node: ?*ast.Node) !*cy.Func {
    try sema.ensureResolvedFuncTemplate(c, template);
    return expandResolvedFuncTemplate(c, template, param_types, args, true, node);
}

pub fn expandResolvedFuncTemplate(
    c: *cy.Chunk, template: *cy.sym.FuncTemplate,
    arg_types: []const *cy.Type, args: []const cy.Value, resolve_new_types: bool, node: ?*ast.Node,
) !*cy.Func {
    // Ensure variant func.
    const key = VariantKey{ .param_types = arg_types, .params = args };
    const res = try template.instance_cache.getOrPutContext(c.alloc, key, .{});
    if (!res.found_existing) {
        // Dupe args and copy.
        const params = try c.alloc.alloc(cy.Value, args.len);
        for (0..params.len) |i| {
            params[i] = try c.heap.copyValue2(arg_types[i], args[i]);
        }
        const param_types = try c.alloc.dupe(*cy.Type, arg_types);

        if (template.with_cstrs.len > 0) {
            for (template.with_cstrs, 0..) |cstr, i| {
                _ = cstr;
                _ = i;
                return error.TODO;
                // if (!param_types[i].implements(cstr.cast(.generic_trait))) {
                //     const impl_name = try c.sema.allocTypeName(param_types[i]);
                //     defer c.alloc.free(impl_name);
                //     const trait_name = try c.sema.allocTypeName(cstr);
                //     defer c.alloc.free(trait_name);
                //     return c.reportErrorFmt("Expected `{}` to implement `{}`.", &.{v(impl_name), v(trait_name)}, node);
                // }
            }
        }

        // Generate variant type.
        const variant = try c.alloc.create(Instance);
        variant.* = .{
            .type = .func,
            .param_types = param_types,
            .params = &.{},
            .map = undefined,
            .data = .{ .func = .{
                .template = template,
                .func = undefined,
            }},
        };
        try variant.initFuncParams(c.alloc, params, template.params);

        const new_func = try sema.reserve_func_instance(c, template, variant);

        // Allow circular reference by resolving after the new symbol has been added to the cache.
        variant.data.func.func = new_func;
        res.key_ptr.* = .{ .param_types = param_types, .params = params };
        res.value_ptr.* = variant;
        try template.instances.append(c.alloc, variant);

        try sema.resolveFuncVariant(c, new_func, resolve_new_types, node);

        return new_func;
    } 

    const variant = res.value_ptr.*;
    return variant.data.func.func;
}
