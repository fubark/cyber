const std = @import("std");
const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypes;
const sema = cy.sema;
const v = cy.fmt.v;

const cte = @This();

pub fn callTemplate2(c: *cy.Chunk, template: *cy.sym.TypeTemplate, argHead: cy.NodeId, nodeId: cy.NodeId) !*cy.Sym {
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
    var arg: cy.NodeId = argHead;
    while (arg != cy.NullNode) {
        const res = try nodeToCtValue(c, arg);
        try c.typeStack.append(c.alloc, res.type);
        try c.valueStack.append(c.alloc, res.value);
        arg = c.ast.node(arg).next();
    }
    const argTypes = c.typeStack.items[typeStart..];
    const args = c.valueStack.items[valueStart..];

    // Check against template signature.
    if (!cy.types.isTypeFuncSigCompat(c.compiler, @ptrCast(argTypes), bt.Type, template.sigId)) {
        const expSig = try c.sema.allocFuncSigStr(template.sigId);
        defer c.alloc.free(expSig);
        return c.reportErrorAt(
            \\Expected template signature `{}{}`.
        , &.{v(template.head.name()), v(expSig)}, nodeId);
    }

    // Ensure variant type.
    const res = try template.variantCache.getOrPut(c.alloc, args);
    if (!res.found_existing) {
        const patchNodes = try execTemplateCtNodes(c, template, args);

        // Dupe args and retain
        const params = try c.alloc.dupe(cy.Value, args);
        for (params) |param| {
            c.vm.retain(param);
        }

        // Generate variant type.
        const id = template.variants.items.len;
        try template.variants.append(c.alloc, .{
            .patchNodes = patchNodes,
            .params = params,
            .sym = undefined,
        });

        const newSym = try sema.declareTemplateVariant(c, template, @intCast(id));
        template.variants.items[id].sym = newSym;

        res.key_ptr.* = params;
        res.value_ptr.* = @intCast(id);
    }

    const variantId = res.value_ptr.*;
    const variantSym = template.variants.items[variantId].sym;
    return variantSym;
}

pub fn callTemplate(c: *cy.Chunk, nodeId: cy.NodeId) !*cy.Sym {
    const node = c.ast.node(nodeId);
    const calleeRes = try c.semaExprSkipSym(node.data.callTemplate.callee);
    if (calleeRes.resType != .sym) {
        return c.reportErrorAt("Expected template symbol.", &.{}, node.data.callTemplate.callee);
    }
    const sym = calleeRes.data.sym;
    if (sym.type != .typeTemplate) {
        return c.reportErrorAt("Expected template symbol.", &.{}, node.data.callTemplate.callee);
    }
    return cte.callTemplate2(c, sym.cast(.typeTemplate), node.data.callTemplate.argHead, nodeId);
}

/// Visit each top level ctNode, perform template param substitution or CTE,
/// and generate new nodes. Return the root of each resulting node.
fn execTemplateCtNodes(c: *cy.Chunk, template: *cy.sym.TypeTemplate, params: []const cy.Value) ![]const cy.NodeId {
    // Build name to template param.
    var paramMap: std.StringHashMapUnmanaged(cy.Value) = .{};
    defer paramMap.deinit(c.alloc);
    for (template.params, 0..) |param, i| {
        try paramMap.put(c.alloc, param.name, params[i]);
    }

    const tchunk = template.chunk();
    const res = try c.alloc.alloc(cy.NodeId, template.ctNodes.len);
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

fn genNodeFromValue(c: *cy.Chunk, val: cy.Value, srcPos: u32) !cy.NodeId {
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

fn nodeToCtValue(c: *cy.Chunk, nodeId: cy.NodeId) !CtValue {
    const node = c.ast.node(nodeId);
    switch (node.type()) {
        .ident => {
            // For now just assume node refereare types.
            const name = c.ast.nodeString(node);
            const res = try sema.getOrLookupVar(c, name, true, nodeId);
            switch (res) {
                .local => |id| {
                    _ = id;
                
                    // Check for constant var.
                    return error.TODO;
                },
                .static => |csymId| {
                    const sym_r = try sema.symbol(c, csymId, nodeId, false);
                    switch (sym_r.data.sym.type) {
                        .struct_t,
                        .predefinedType,
                        .object_t => {
                            return CtValue{
                                .type = bt.Type,
                                .value = try c.vm.allocType(sym_r.data.sym.getStaticType().?),
                            };
                        },
                        else => {
                            return c.reportErrorAt("Unsupported conversion to compile-time value: {}", &.{v(sym_r.data.sym.type)}, nodeId);
                        },
                    }
                },
            }
        },
        else => {
            return error.TODO;
        },
    }
}