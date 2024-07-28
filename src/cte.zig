const std = @import("std");
const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypes;
const sema = cy.sema;
const v = cy.fmt.v;
const log = cy.log.scoped(.cte);
const ast = cy.ast;

const cte = @This();

pub fn expandTemplateOnCallExpr(c: *cy.Chunk, node: *ast.CallExpr) !*cy.Sym {
    var callee = try cy.sema.resolveSym(c, node.callee);
    if (callee.type != .template) {
        return c.reportErrorFmt("Expected template symbol.", &.{}, node.callee);
    }
    return cte.expandTemplateOnCallArgs(c, callee.cast(.template), node.args, @ptrCast(node));
}

pub fn pushNodeValues(c: *cy.Chunk, args: []const *ast.Node) !void {
    for (args) |arg| {
        const res = try resolveCtValue(c, arg);
        try c.typeStack.append(c.alloc, res.type);
        try c.valueStack.append(c.alloc, res.value);
    }
}

pub fn expandTemplateOnCallArgs(c: *cy.Chunk, template: *cy.sym.Template, args: []const *ast.Node, node: *ast.Node) !*cy.Sym {
    // Accumulate compile-time args.
    const typeStart = c.typeStack.items.len;
    const valueStart = c.valueStack.items.len;
    defer {
        c.typeStack.items.len = typeStart;

        // Values need to be released.
        const values = c.valueStack.items[valueStart..];
        for (values) |val| {
            c.vm.release(val);
        }
        c.valueStack.items.len = valueStart;
    }

    try pushNodeValues(c, args);

    const argTypes = c.typeStack.items[typeStart..];
    const arg_vals = c.valueStack.items[valueStart..];

    // Check against template signature.
    if (!cy.types.isTypeFuncSigCompat(c.compiler, @ptrCast(argTypes), .not_void, template.sigId)) {
        const sig = c.sema.getFuncSig(template.sigId);
        const params_s = try c.sema.allocFuncParamsStr(sig.params(), c);
        defer c.alloc.free(params_s);
        return c.reportErrorFmt(
            \\Expected template signature `{}[{}]`.
        , &.{v(template.head.name()), v(params_s)}, node);
    }

    return expandTemplate(c, template, arg_vals);
}

pub fn expandFuncTemplateOnCallArgs(c: *cy.Chunk, template: *cy.Func, args: []const *ast.Node, node: *ast.Node) !*cy.Func {
    // Accumulate compile-time args.
    const typeStart = c.typeStack.items.len;
    const valueStart = c.valueStack.items.len;
    defer {
        c.typeStack.items.len = typeStart;

        // Values need to be released.
        const values = c.valueStack.items[valueStart..];
        for (values) |val| {
            c.vm.release(val);
        }
        c.valueStack.items.len = valueStart;
    }

    try pushNodeValues(c, args);

    const argTypes = c.typeStack.items[typeStart..];
    const arg_vals = c.valueStack.items[valueStart..];

    // Check against template signature.
    const func_template = template.data.template;
    if (!cy.types.isTypeFuncSigCompat(c.compiler, @ptrCast(argTypes), .not_void, func_template.sig)) {
        const sig = c.sema.getFuncSig(func_template.sig);
        const params_s = try c.sema.allocFuncParamsStr(sig.params(), c);
        defer c.alloc.free(params_s);
        return c.reportErrorFmt(
            \\Expected template expansion signature `{}[{}]`.
        , &.{v(template.name()), v(params_s)}, node);
    }

    return expandFuncTemplate(c, template, arg_vals);
}

pub fn expandFuncTemplate(c: *cy.Chunk, tfunc: *cy.sym.Func, args: []const cy.Value) !*cy.Func {
    const template = tfunc.data.template;

    // Ensure variant func.
    const res = try template.variant_cache.getOrPut(c.alloc, args);
    if (!res.found_existing) {
        // Dupe args and retain
        const args_dupe = try c.alloc.dupe(cy.Value, args);
        for (args_dupe) |param| {
            c.vm.retain(param);
        }

        // Generate variant type.
        const variant = try c.alloc.create(cy.sym.FuncVariant);
        variant.* = .{
            .args = args_dupe,
            .template = tfunc.data.template,
            .func = undefined,
        };

        const tchunk = tfunc.chunk();
        const new_func = try sema.reserveFuncTemplateVariant(tchunk, tfunc, tfunc.decl, variant);
        variant.func = new_func;
        res.key_ptr.* = args_dupe;
        res.value_ptr.* = variant;
        try template.variants.append(c.alloc, variant);

        // Allow circular reference by resolving after the new symbol has been added to the cache.
        try sema.resolveFuncVariant(tchunk, new_func);

        return new_func;
    } 
    const variant = res.value_ptr.*;
    return variant.func;
}

pub fn expandTemplate(c: *cy.Chunk, template: *cy.sym.Template, args: []const cy.Value) !*cy.Sym {
    const root_template = template.root();

    // Ensure variant type.
    const res = try root_template.variant_cache.getOrPut(c.alloc, args);
    if (!res.found_existing) {
        // Dupe args and retain
        const args_dupe = try c.alloc.dupe(cy.Value, args);
        for (args_dupe) |param| {
            c.vm.retain(param);
        }

        var ct_infer = false;
        var ct_ref = false;
        for (args) |arg| {
            if (arg.getTypeId() == bt.Type) {
                const type_id = arg.asHeapObject().type.type;
                const type_e = c.sema.types.items[type_id];
                ct_infer = ct_infer or type_e.info.ct_infer;
                ct_ref = ct_ref or type_e.info.ct_ref;
            }
        }

        // Generate variant type.
        const variant = try c.alloc.create(cy.sym.Variant);
        variant.* = .{
            .type = .sym,
            .root_template = root_template,
            .args = args_dupe,
            .data = .{ .sym = undefined },
        };
        res.key_ptr.* = args_dupe;
        res.value_ptr.* = variant;
        try root_template.variants.append(c.alloc, variant);

        const new_sym = try sema.reserveTemplateVariant(c, root_template, root_template.child_decl, variant);
        variant.data.sym = new_sym;

        const new_type = new_sym.getStaticType().?;
        c.sema.types.items[new_type].info.ct_infer = ct_infer;
        c.sema.types.items[new_type].info.ct_ref = ct_ref;

        // Allow circular reference by resolving after the new symbol has been added to the cache.
        // In the case of a distinct type, a new sym is returned after resolving.
        const final_sym = try sema.resolveTemplateVariant(c, root_template, new_sym);
        variant.data.sym = final_sym;

        if (root_template == template) {
            return final_sym;
        } else {
            // Access the child expansion from the root.
            return template.getExpandedSymFrom(root_template, final_sym);
        }
    } 

    const variant = res.value_ptr.*;
    if (variant.type == .specialization) {
        const child_decl = variant.data.specialization;
        if (child_decl.type() != .custom_decl) {
            return error.Unsupported;
        }
        const custom_decl = child_decl.cast(.custom_decl);
        if (custom_decl.funcs.len > 0) {
            return error.Unsupported;
        }

        const new_sym = try sema.reserveTemplateVariant(c, template, variant.data.specialization, variant);
        variant.type = .sym;
        variant.data = .{ .sym = new_sym };

        const final_sym = try sema.resolveTemplateVariant(c, template, new_sym);
        variant.data.sym = new_sym;
        return final_sym;
    }

    if (root_template == template) {
        return variant.data.sym;
    } else {
        return template.getExpandedSymFrom(root_template, variant.data.sym);
    }
}

/// Visit each top level ctNode, perform template param substitution or CTE,
/// and generate new nodes. Return the root of each resulting node.
fn execTemplateCtNodes(c: *cy.Chunk, template: *cy.sym.Template, params: []const cy.Value) ![]const *ast.Node {
    // Build name to template param.
    var paramMap: std.StringHashMapUnmanaged(cy.Value) = .{};
    defer paramMap.deinit(c.alloc);
    for (template.params, 0..) |param, i| {
        try paramMap.put(c.alloc, param.name, params[i]);
    }

    const tchunk = template.chunk();
    const res = try c.alloc.alloc(*ast.Node, template.ctNodes.len);
    for (template.ctNodes, 0..) |ctNodeId, i| {
        const node = tchunk.ast.node(ctNodeId);

        // Check for simple template param replacement.
        const child = tchunk.ast.node(node.data.comptimeExpr.child);
        if (child.type() == .ident) {
            const name = tchunk.ast.nodeString(child);
            if (paramMap.get(name)) |param| {
                res[i] = try cte.genNodeFromValue(tchunk, param, node.srcPos);
                continue;
            }
        }
        // General CTE.
        return error.TODO;
    }

    tchunk.updateAstView(tchunk.parser.ast.view());
    return res;
}

fn genNodeFromValue(c: *cy.Chunk, val: cy.Value, srcPos: u32) !*ast.Node {
    switch (val.getTypeId()) {
        bt.Type => {
            const node = try c.parser.ast.pushNode(c.alloc, .semaSym, srcPos);
            const sym = c.sema.getTypeSym(val.asHeapObject().type.type);
            c.parser.ast.setNodeData(node, .{ .semaSym = .{
                .sym = sym,
            }});
            return node;
        },
        else => return error.TODO,
    }
}

pub const CtValue = struct {
    type: cy.TypeId,
    value: cy.Value,
};

// TODO: Evaluate const expressions.
pub fn resolveCtValue(c: *cy.Chunk, expr: *ast.Node) !CtValue {
    switch (expr.type()) {
        .decLit => {
            const literal = c.ast.nodeString(expr);
            const val = try std.fmt.parseInt(i64, literal, 10);
            return .{
                .type = bt.Integer,
                .value = try c.vm.allocInt(val),
            };
        },
        .ident => {
            const name = c.ast.nodeString(expr);

            // Look in ct context.
            var resolve_ctx_idx = c.resolve_stack.items.len-1;
            while (true) {
                const ctx = c.resolve_stack.items[resolve_ctx_idx];
                if (ctx.ct_params.size > 0) {
                    if (ctx.ct_params.get(name)) |param| {
                        c.vm.retain(param);
                        return .{
                            .type = param.getTypeId(),
                            .value = param,
                        };
                    }
                }
                if (!ctx.has_parent_ctx) {
                    break;
                }
                resolve_ctx_idx -= 1;
            }

            const sym = try sema.resolveSym(c, expr);
            if (sym.getStaticType()) |type_id| {
                return CtValue{
                    .type = bt.Type,
                    .value = try c.vm.allocType(type_id),
                };
            }
            return c.reportErrorFmt("Unsupported conversion to compile-time value: {}", &.{v(sym.type)}, expr);
        },
        .ptr,
        .array_expr => {
            const type_id = try sema.resolveSymType(c, expr);
            return CtValue{
                .type = bt.Type,
                .value = try c.vm.allocType(type_id),
            };
        },
        .void => {
            return CtValue{
                .type = bt.Type,
                .value = try c.vm.allocType(bt.Void),
            };
        },
        .comptimeExpr => {
            const ctx = sema.getResolveContext(c);
            if (ctx.parse_ct_inferred_params) {
                const ct_expr = expr.cast(.comptimeExpr);
                if (ct_expr.child.type() != .ident) {
                    return c.reportErrorFmt("Expected identifier.", &.{}, ct_expr.child);
                }
                const param_name = c.ast.nodeString(ct_expr.child);

                const param_idx = ctx.ct_params.size;
                const ref_t = try c.sema.ensureCtRefType(param_idx);
                const ref_v = try c.vm.allocType(ref_t);
                try sema.setResolveCtParam(c, param_name, ref_v);

                const infer_t = try c.sema.ensureCtInferType(param_idx);
                return CtValue{
                    .type = bt.Type,
                    .value = try c.vm.allocType(infer_t),
                };
            }

            if (ctx.expand_ct_inferred_params) {
                const ct_expr = expr.cast(.comptimeExpr);
                const param_name = c.ast.nodeString(ct_expr.child);
                const val = ctx.ct_params.get(param_name) orelse {
                    return c.reportErrorFmt("Could not find the compile-time parameter `{}`.", &.{v(param_name)}, ct_expr.child);
                };
                c.vm.retain(val);
                return CtValue{
                    .type = val.getTypeId(),
                    .value = val,
                };
            }
            return c.reportErrorFmt("Unexpected compile-time expression.", &.{}, expr);
        },
        else => {
            return c.reportErrorFmt("Unsupported expr: `{}`", &.{v(expr.type())}, expr);
        }
    }
}
