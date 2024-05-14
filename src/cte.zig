const std = @import("std");
const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypes;
const sema = cy.sema;
const v = cy.fmt.v;
const log = cy.log.scoped(.cte);
const ast = cy.ast;

const cte = @This();

pub fn expandTemplateOnCallExpr(c: *cy.Chunk, node: *ast.CallExpr) !*cy.Sym {
    const calleeRes = try c.semaExprSkipSym(node.callee);
    if (calleeRes.resType != .sym) {
        return c.reportErrorFmt("Expected template symbol.", &.{}, node.callee);
    }
    const sym = calleeRes.data.sym;
    if (sym.type != .template) {
        return c.reportErrorFmt("Expected template symbol.", &.{}, node.callee);
    }
    return cte.expandTemplateOnCallArgs(c, sym.cast(.template), node.args, @ptrCast(node));
}

pub fn pushNodeValues(c: *cy.Chunk, args: []const *ast.Node) !void {
    for (args) |arg| {
        const res = try nodeToCtValue(c, arg);
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

    return expandTemplate(c, template, arg_vals, argTypes) catch |err| {
        if (err == error.IncompatSig) {
            const sig = c.sema.getFuncSig(template.sigId);
            const params_s = try c.sema.allocFuncParamsStr(sig.params(), c);
            defer c.alloc.free(params_s);
            return c.reportErrorFmt(
                \\Expected template signature `{}[{}]`.
            , &.{v(template.head.name()), v(params_s)}, node);
        } else {
            return err;
        }
    };
}

pub fn expandTemplate(c: *cy.Chunk, template: *cy.sym.Template, args: []const cy.Value, arg_types: []const cy.TypeId) !*cy.Sym {
    // Check against template signature.
    if (!cy.types.isTypeFuncSigCompat(c.compiler, @ptrCast(arg_types), .not_void, template.sigId)) {
        return error.IncompatSig;
    }

    // Ensure variant type.
    const res = try template.variant_cache.getOrPut(c.alloc, args);
    if (!res.found_existing) {
        // Dupe args and retain
        const args_dupe = try c.alloc.dupe(cy.Value, args);
        for (args_dupe) |param| {
            c.vm.retain(param);
        }

        // Generate variant type.
        const variant = try c.alloc.create(cy.sym.Variant);
        variant.* = .{
            .type = .sym,
            .template = template,
            .args = args_dupe,
            .data = .{ .sym = undefined },
        };

        const new_sym = try sema.reserveTemplateVariant(c, template, template.child_decl, variant);
        variant.data.sym = new_sym;
        res.key_ptr.* = args_dupe;
        res.value_ptr.* = variant;

        // Allow circular reference by resolving after the new symbol has been added to the cache.
        try sema.resolveTemplateVariant(c, template, new_sym);
        return new_sym;
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

        try sema.resolveTemplateVariant(c, template, new_sym);
        return new_sym;
    }

    return variant.data.sym;
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

const CtValue = struct {
    type: cy.TypeId,
    value: cy.Value,
};

fn nodeToCtValue(c: *cy.Chunk, node: *ast.Node) anyerror!CtValue {
    // TODO: Evaluate const expressions.
    const sym = try sema.resolveSym(c, node);
    if (sym.getStaticType()) |type_id| {
        return CtValue{
            .type = bt.Type,
            .value = try c.vm.allocType(type_id),
        };
    }

    return c.reportErrorFmt("Unsupported conversion to compile-time value: {}", &.{v(sym.type)}, node);
}