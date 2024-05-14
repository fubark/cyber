const std = @import("std");
const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypes;
const sema = cy.sema;
const v = cy.fmt.v;

const cte = @This();

pub fn expandTemplateOnCallExpr(c: *cy.Chunk, nodeId: cy.NodeId) !*cy.Sym {
    const node = c.ast.node(nodeId);
    const calleeRes = try c.semaExprSkipSym(node.data.callExpr.callee);
    if (calleeRes.resType != .sym) {
        return c.reportErrorFmt("Expected template symbol.", &.{}, node.data.callExpr.callee);
    }
    const sym = calleeRes.data.sym;
    if (sym.type != .template) {
        return c.reportErrorFmt("Expected template symbol.", &.{}, node.data.callExpr.callee);
    }
    return cte.expandTemplateOnCallArgs(c, sym.cast(.template), node.data.callExpr.argHead, nodeId);
}

pub fn expandTemplateOnCallArgs(c: *cy.Chunk, template: *cy.sym.Template, argHead: cy.NodeId, nodeId: cy.NodeId) !*cy.Sym {
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

    return expandTemplate(c, template, args, argTypes) catch |err| {
        if (err == error.IncompatSig) {
            const expSig = try c.sema.allocFuncSigStr(template.sigId, true);
            defer c.alloc.free(expSig);
            return c.reportErrorFmt(
                \\Expected template signature `{}{}`.
            , &.{v(template.head.name()), v(expSig)}, nodeId);
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
    const res = try template.variantCache.getOrPut(c.alloc, args);
    if (!res.found_existing) {
        // Dupe args and retain
        const params = try c.alloc.dupe(cy.Value, args);
        for (params) |param| {
            c.vm.retain(param);
        }

        // Generate variant type.
        const id = template.variants.items.len;
        try template.variants.append(c.alloc, .{
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

/// Visit each top level ctNode, perform template param substitution or CTE,
/// and generate new nodes. Return the root of each resulting node.
fn execTemplateCtNodes(c: *cy.Chunk, template: *cy.sym.Template, params: []const cy.Value) ![]const cy.NodeId {
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
            const name = c.ast.nodeString(node);

            if (c.semaProcs.items.len > 0) {
                const res = try sema.getOrLookupVar(c, name, nodeId);
                switch (res) {
                    .local => |id| {
                        _ = id;
                    
                        // Check for constant var.
                        return error.TODO;
                    },
                    else => {},
                }
            }

            const sym = (try sema.getResolvedLocalSym(c, name, nodeId, true)) orelse {
                return c.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, nodeId);
            };

            if (sym.getStaticType()) |type_id| {
                return CtValue{
                    .type = bt.Type,
                    .value = try c.vm.allocType(type_id),
                };
            } else {
                return c.reportErrorFmt("Unsupported conversion to compile-time value: {}", &.{v(sym.type)}, nodeId);
            }
        },
        else => {
            return error.TODO;
        },
    }
}