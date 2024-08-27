const std = @import("std");
const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypes;
const sema = cy.sema;
const v = cy.fmt.v;
const log = cy.log.scoped(.cte);
const ast = cy.ast;
const bcgen = @import("bc_gen.zig");

const cte = @This();

pub fn expandTemplateOnCallExpr(c: *cy.Chunk, node: *ast.CallExpr) !*cy.Sym {
    var callee = try resolveCtSym(c, node.callee);
    if (callee.type != .template) {
        return c.reportErrorFmt("Expected template symbol.", &.{}, node.callee);
    }
    return cte.expandTemplateOnCallArgs(c, callee.cast(.template), node.args, @ptrCast(node));
}

pub fn pushNodeValuesCstr(c: *cy.Chunk, args: []const *ast.Node, template: *cy.sym.Template, ct_arg_start: usize, node: *ast.Node) !void {
    const params = c.sema.getFuncSig(template.sigId).params();
    if (args.len != params.len) {
        const params_s = try c.sema.allocFuncParamsStr(params, c);
        defer c.alloc.free(params_s);
        return c.reportErrorFmt(
            \\Expected template signature `{}[{}]`.
        , &.{v(template.head.name()), v(params_s)}, node);
    }
    for (args, 0..) |arg, i| {
        const param = params[i];
        const exp_type = try resolveTemplateParamType(c, param.type, c.valueStack.items.ptr + ct_arg_start);
        const res = try resolveCtValue(c, arg);
        try c.valueStack.append(c.alloc, res.value);

        if (res.type != exp_type) {
            const cstrName = try c.sema.allocTypeName(exp_type);
            defer c.alloc.free(cstrName);
            const typeName = try c.sema.allocTypeName(res.type);
            defer c.alloc.free(typeName);
            return c.reportErrorFmt("Expected type `{}`, got `{}`.", &.{v(cstrName), v(typeName)}, arg);
        }
    }
}

/// This is similar to `sema_func.resolveTemplateParamType` except it only cares about ct_ref.
fn resolveTemplateParamType(c: *cy.Chunk, type_id: cy.TypeId, ct_args: [*]const cy.Value) !cy.TypeId {
    const type_e = c.sema.types.items[type_id];
    if (type_e.kind == .ct_ref) {
        const ct_arg = ct_args[type_e.data.ct_infer.ct_param_idx];
        if (ct_arg.getTypeId() != bt.Type) {
            return error.TODO;
        }
        return ct_arg.asHeapObject().type.type;
    } else if (type_e.info.ct_ref) {
        // Contains nested ct-dependent arg.
        if (type_e.sym.getVariant()) |variant| {
            const template = variant.getSymTemplate();

            const value_start = c.valueStack.items.len;
            defer {
                const values = c.valueStack.items[value_start..];
                for (values) |val| {
                    c.vm.release(val);
                }
                c.valueStack.items.len = value_start;
            }

            for (variant.args) |arg| {
                if (arg.getTypeId() == bt.Type) {
                    const arg_t = arg.asHeapObject().type.type;
                    const rarg_t = try resolveTemplateParamType(c, arg_t, ct_args);
                    const rarg_t_v = try c.vm.allocType(rarg_t);
                    try c.valueStack.append(c.alloc, rarg_t_v);
                } else if (arg.getTypeId() == bt.FuncSig) {
                    const sig = c.sema.getFuncSig(@intCast(arg.asBoxInt()));

                    const type_start = c.typeStack.items.len;
                    defer c.typeStack.items.len = type_start;

                    for (sig.params()) |param| {
                        const param_te = c.sema.getType(param.type);
                        if (param_te.info.ct_ref) {
                            const param_t = try resolveTemplateParamType(c, param.type, ct_args);
                            try c.typeStack.append(c.alloc, param_t);
                        } else {
                            try c.typeStack.append(c.alloc, param.type);
                        }
                    }
                    var ret_t = sig.ret;
                    const ret_te = c.sema.getType(sig.ret);
                    if (ret_te.info.ct_ref) {
                        ret_t = try resolveTemplateParamType(c, ret_t, ct_args);
                    }
                    const new_sig = try c.sema.ensureFuncSig(@ptrCast(c.typeStack.items[type_start..]), ret_t);
                    const new_sigv = try c.vm.allocFuncSig(new_sig);
                    try c.valueStack.append(c.alloc, new_sigv);
                } else {
                    return error.TODO;
                }
            }

            const args = c.valueStack.items[value_start..];
            const type_sym = try cte.expandTemplate(c, template, args);
            return type_sym.getStaticType().?;
        } else {
            return error.Unexpected;
        }
    } else {
        return type_id;
    }
}

pub fn pushNodeValues(c: *cy.Chunk, args: []const *ast.Node) !void {
    for (args) |arg| {
        const res = try resolveCtValue(c, arg);
        try c.typeStack.append(c.alloc, res.type);
        try c.valueStack.append(c.alloc, res.value);
    }
}

pub fn expandTemplateOnCallArgs(c: *cy.Chunk, template: *cy.sym.Template, args: []const *ast.Node, node: *ast.Node) !*cy.Sym {
    try sema.ensureResolvedTemplate(c, template);

    // Accumulate compile-time args.
    const valueStart = c.valueStack.items.len;
    defer {
        // Values need to be released.
        const values = c.valueStack.items[valueStart..];
        for (values) |val| {
            c.vm.release(val);
        }
        c.valueStack.items.len = valueStart;
    }

    try pushNodeValuesCstr(c, args, template, valueStart, node);
    const arg_vals = c.valueStack.items[valueStart..];
    return expandTemplate(c, template, arg_vals);
}

pub fn expandCtFuncTemplateOnCallArgs(c: *cy.Chunk, template: *cy.sym.Template, args: []const *ast.Node, node: *ast.Node) !CtValue {
    // Accumulate compile-time args.
    const valueStart = c.valueStack.items.len;
    defer {
        // Values need to be released.
        const values = c.valueStack.items[valueStart..];
        for (values) |val| {
            c.vm.release(val);
        }
        c.valueStack.items.len = valueStart;
    }

    try pushNodeValuesCstr(c, args, template, valueStart, node);
    const arg_vals = c.valueStack.items[valueStart..];
    return expandValueTemplate(c, template, arg_vals);
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
    const res = try template.variant_cache.getOrPutContext(c.alloc, args, .{ .sema = c.sema });
    if (!res.found_existing) {
        // Dupe args and retain
        const args_dupe = try c.alloc.dupe(cy.Value, args);
        for (args_dupe) |param| {
            c.vm.retain(param);
        }

        // Generate variant type.
        const variant = try c.alloc.create(cy.sym.Variant);
        variant.* = .{
            .type = .func,
            .args = args_dupe,
            .data = .{ .func = .{
                .template = tfunc.data.template,
                .func = undefined,
            }},
        };

        const tchunk = tfunc.chunk();
        const new_func = try sema.reserveFuncTemplateVariant(tchunk, tfunc, tfunc.decl, variant);
        variant.data.func.func = new_func;
        res.key_ptr.* = args_dupe;
        res.value_ptr.* = variant;
        try template.variants.append(c.alloc, variant);

        // Allow circular reference by resolving after the new symbol has been added to the cache.
        try sema.resolveFuncVariant(tchunk, new_func);

        return new_func;
    } 
    const variant = res.value_ptr.*;
    return variant.data.func.func;
}

fn compileFuncDeep(c: *cy.Chunk, func: *cy.Func, queued: *std.AutoHashMapUnmanaged(*cy.Func, void)) !void {
    if (func.emitted_deps) {
        return;
    }
    if (queued.contains(func)) {
        return;
    }
    try queued.putNoClobber(c.alloc, func, {});
    defer _ = queued.remove(func);

    // Resolve signature.
    const src_chunk = func.chunk();
    if (func.variant != null) {
        try sema.resolveFuncVariant(src_chunk, func);
    } else {
        try sema.resolveFunc2(src_chunk, func, false);
    }

    // Perform sema.
    const loc = src_chunk.ir.buf.items.len;
    try sema.funcDecl(src_chunk, func);

    // Perform bc gen.
    const loc_n = src_chunk.ir.getNode(loc);
    src_chunk.buf = &c.compiler.buf;
    // TODO: defer restore bc state.
    try bcgen.prepareFunc(src_chunk.compiler, null, func);
    switch (func.type) {
        .hostFunc => {
            // Nop. Already setup from `prepareFunc`.
        },
        .userFunc => {
            try bcgen.funcBlock(src_chunk, loc, loc_n);

            // Analyze IR for func deps.
            var visitor = try src_chunk.ir.visitStmt(c.alloc, @intCast(loc));
            defer visitor.deinit();
            while (try visitor.next()) |entry| {
                if (entry.is_stmt) {
                    continue;
                }
                const code = src_chunk.ir.getExprCode(entry.loc);
                switch (code) {
                    .call_sym => {
                        const data = src_chunk.ir.getExprData(entry.loc, .call_sym);
                        try compileFuncDeep(c, data.func, queued);
                    },
                    else => {},
                }
            }
            func.data = .{ .userFunc = .{ .loc = @intCast(loc) }};
        },
        else => {
            return error.Unexpected;
        }
    }
    func.emitted_deps = true;
}

/// Call function with arguments at compile-time.
pub fn callFunc(c: *cy.Chunk, func: *cy.Func, args: []const cy.Value) !CtValue {
    var queued: std.AutoHashMapUnmanaged(*cy.Func, void) = .{};
    defer queued.deinit(c.alloc);
    try compileFuncDeep(c, func, &queued);

    const rt_id = c.compiler.genSymMap.get(func).?.func.id;
    const rt_func = c.vm.funcSyms.buf[rt_id];
    const func_val = try cy.heap.allocFunc(c.vm, rt_func);
    defer c.vm.release(func_val);

    try c.vm.prepCtEval(&c.compiler.buf);
    const retv = try c.vm.callFunc(func_val, args, .{});
    return CtValue{
        .type = retv.getTypeId(),
        .value = retv,
    };
}

pub fn expandValueTemplate(c: *cy.Chunk, template: *cy.sym.Template, args: []const cy.Value) !CtValue {
    // Ensure variant type.
    const res = try template.variant_cache.getOrPutContext(c.alloc, args, .{ .sema = c.sema });
    if (!res.found_existing) {
        // Dupe args and retain
        const args_dupe = try c.alloc.dupe(cy.Value, args);
        for (args_dupe) |param| {
            c.vm.retain(param);
        }

        // Generate variant type.
        const variant = try c.alloc.create(cy.sym.Variant);
        variant.* = .{
            .type = .ct_val,
            .args = args_dupe,
            .data = .{ .ct_val = .{
                .template = template,
                .ct_val = cy.Value.Void,
            }},
        };
        res.key_ptr.* = args_dupe;
        res.value_ptr.* = variant;
        try template.variants.append(c.alloc, variant);

        // Resolve dependent return type.
        var ret_t = c.sema.getFuncSig(template.sigId).getRetType();
        const ret_te = c.sema.getType(ret_t);
        if (ret_te.info.ct_ref) {
            ret_t = try resolveTemplateParamType(c, ret_t, variant.args.ptr);
        }

        // Resolve dependent func sig. There are no rt params since they are compile-time params.
        const sig = try c.sema.ensureFuncSig(&.{}, ret_t);

        // Generate ct func. Can assume not a `@host` func.
        const func = try c.createFunc(.userFunc, @ptrCast(template), @ptrCast(template.decl.child_decl), false);
        defer {
            // Remove references to the func and invalidate the sema func block.
            _ = c.compiler.genSymMap.remove(@ptrCast(func));
            const src_chunk = func.chunk();
            const data = src_chunk.ir.getStmtDataPtr(func.data.userFunc.loc, .funcBlock);
            data.skip = true;
            c.vm.alloc.destroy(func);
        }
        const func_sig = c.sema.getFuncSig(sig);
        func.funcSigId = sig;
        func.retType = func_sig.getRetType();
        func.reqCallTypeCheck = func_sig.info.reqCallTypeCheck;
        func.numParams = @intCast(func_sig.params_len);
        func.variant = variant;

        var queued: std.AutoHashMapUnmanaged(*cy.Func, void) = .{};
        defer queued.deinit(c.alloc);
        try compileFuncDeep(c, func, &queued);

        const rt_id = c.compiler.genSymMap.get(func).?.func.id;
        const rt_func = c.vm.funcSyms.buf[rt_id];
        const func_val = try cy.heap.allocFunc(c.vm, rt_func);
        defer c.vm.release(func_val);

        try c.vm.prepCtEval(&c.compiler.buf);
        const retv = try c.vm.callFunc(func_val, &.{}, .{});
        c.vm.retain(retv);
        variant.data.ct_val.ct_val = retv;

        return CtValue{
            .type = retv.getTypeId(),
            .value = retv,
        };
    } 

    const variant = res.value_ptr.*;
    c.vm.retain(variant.data.ct_val.ct_val);
    return CtValue{
        .type = variant.data.ct_val.ct_val.getTypeId(),
        .value = variant.data.ct_val.ct_val,
    };
}

pub fn expandTemplate(c: *cy.Chunk, template: *cy.sym.Template, args: []const cy.Value) !*cy.Sym {
    try sema.ensureResolvedTemplate(c, template);
    
    // Ensure variant type.
    const res = try template.variant_cache.getOrPutContext(c.alloc, args, .{ .sema = c.sema });
    if (!res.found_existing) {
        // Dupe args and retain
        const args_dupe = try c.alloc.dupe(cy.Value, args);
        for (args_dupe) |param| {
            c.vm.retain(param);
        }

        var ct_infer = false;
        var ct_dep = false;
        for (args) |arg| {
            if (arg.getTypeId() == bt.Type) {
                const type_id = arg.asHeapObject().type.type;
                const type_e = c.sema.types.items[type_id];
                ct_infer = ct_infer or type_e.info.ct_infer;
                ct_dep = ct_dep or type_e.info.ct_ref;
            } else if (arg.getTypeId() == bt.FuncSig) {
                const sig = c.sema.getFuncSig(@intCast(arg.asHeapObject().integer.val));
                ct_infer = ct_infer or sig.info.ct_infer;
                ct_dep = ct_dep or sig.info.ct_dep;
            }
        }

        // Generate variant type.
        const variant = try c.alloc.create(cy.sym.Variant);
        variant.* = .{
            .type = .sym,
            .args = args_dupe,
            .data = .{ .sym = .{
                .template = template,
                .sym = undefined,
            }},
        };
        res.key_ptr.* = args_dupe;
        res.value_ptr.* = variant;
        try template.variants.append(c.alloc, variant);

        const new_sym = try sema.reserveTemplateVariant(c, template, template.decl.child_decl, variant);
        variant.data.sym.sym = new_sym;

        const new_type = new_sym.getStaticType().?;
        c.sema.types.items[new_type].info.ct_infer = ct_infer;
        c.sema.types.items[new_type].info.ct_ref = ct_dep;

        // Allow circular reference by resolving after the new symbol has been added to the cache.
        // In the case of a distinct type, a new sym is returned after resolving.
        const final_sym = try sema.resolveTemplateVariant(c, template, new_sym);
        variant.data.sym.sym = final_sym;

        return final_sym;
    } 

    const variant = res.value_ptr.*;
    return variant.data.sym.sym;
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

pub fn resolveCtValueOpt(c: *cy.Chunk, expr: *ast.Node) anyerror!?CtValue {
    const res = (try resolveCtExprOpt(c, expr)) orelse {
        return null;
    };
    switch (res.type) {
        .value => return res.data.value,
        .sym => {
            const sym = res.data.sym;
            if (sym.getStaticType()) |type_id| {
                return CtValue{
                    .type = bt.Type,
                    .value = try c.vm.allocType(type_id),
                };
            }
            if (sym.type == .func) {
                const func_sym = sym.cast(.func);
                if (func_sym.numFuncs == 1) {
                    const func_t = try cy.sema.getFuncSymType(c, func_sym.first.funcSigId);
                    return CtValue{
                        .type = func_t,
                        .value = try c.vm.allocFuncSym(func_t, func_sym.first),
                    };
                }
            }
            return c.reportErrorFmt("Unsupported conversion to compile-time value: {}", &.{v(sym.type)}, expr);
        },
    }
}

pub fn resolveCtValue(c: *cy.Chunk, expr: *ast.Node) anyerror!CtValue {
    return (try resolveCtValueOpt(c, expr)) orelse {
        return c.reportError("Expected compile-time value.", expr);
    };
}

pub fn resolveCtSym(c: *cy.Chunk, node: *ast.Node) anyerror!*cy.Sym {
    const ct_expr = (try resolveCtExprOpt(c, node)) orelse {
        return c.reportError("Expected symbol.", node);
    };
    return cte.deriveCtExprSym(c, ct_expr, node);
}

fn deriveCtExprSym(c: *cy.Chunk, expr: CtExpr, node: *ast.Node) !*cy.Sym {
    switch (expr.type) {
        .sym => {
            return expr.data.sym;
        },
        .value => {
            defer c.vm.release(expr.data.value.value);
            if (expr.data.value.type == bt.Type) {
                const type_id = expr.data.value.value.asHeapObject().type.type;
                return c.sema.getTypeSym(type_id);
            }
            const type_n = try c.sema.allocTypeName(expr.data.value.type);
            defer c.alloc.free(type_n);
            return c.reportErrorFmt("Can not derive symbol from compile-time value: {}", &.{v(type_n)}, node);
        },
    }
}

fn deriveCtExprType(c: *cy.Chunk, expr: CtExpr, node: *ast.Node) !cy.TypeId {
    switch (expr.type) {
        .sym => {
            const sym = expr.data.sym;
            if (sym.getStaticType()) |type_id| {
                return type_id;
            }
            if (sym.type == .func) {
                const func_sym = sym.cast(.func);
                if (func_sym.numFuncs == 1) {
                    return sema.getFuncSymType(c, func_sym.first.funcSigId);
                }
            }

            if (sym.type == .template) {
                return c.reportErrorFmt("Expected a type symbol. `{}` is a type template and must be expanded to a type first.", &.{v(sym.name())}, node);
            }
            return c.reportErrorFmt("Can not derive type from `{}`. Found `{}`.", &.{v(sym.name()), v(sym.type)}, node);
        },
        .value => {
            defer c.vm.release(expr.data.value.value);
            if (expr.data.value.type == bt.Type) {
                return expr.data.value.value.asHeapObject().type.type;
            }
            const type_n = try c.sema.allocTypeName(expr.data.value.type);
            defer c.alloc.free(type_n);
            return c.reportErrorFmt("Can not derive type from compile-time value: {}", &.{v(type_n)}, node);
        },
    }
}

pub fn resolveCtType(c: *cy.Chunk, expr: *ast.Node) !cy.TypeId {
    const ct_expr = (try resolveCtExprOpt(c, expr)) orelse {
        return c.reportError("Expected type.", expr);
    };
    return cte.deriveCtExprType(c, ct_expr, expr);
}

pub fn resolveCtExpr(c: *cy.Chunk, expr: *ast.Node) anyerror!CtExpr {
    return (try resolveCtExprOpt(c, expr)) orelse {
        return c.reportError("Expected compile-time expression.", expr);
    };
}

const CtExprType = enum(u8) {
    value,
    sym,
};

const CtExpr = struct {
    type: CtExprType,
    data: union {
        value: CtValue,
        sym: *cy.Sym,
    },

    fn initValue(value: CtValue) CtExpr {
        return .{ .type = .value, .data = .{ .value = value }};
    }

    fn initSym(sym: *cy.Sym) CtExpr {
        return .{ .type = .sym, .data = .{ .sym = sym }};
    }
};

pub fn resolveCtExprOpt(c: *cy.Chunk, expr: *ast.Node) anyerror!?CtExpr {
    switch (expr.type()) {
        .raw_string_lit => {
            const str = c.ast.nodeString(expr);
            return CtExpr.initValue(.{
                .type = bt.String,
                .value = try c.vm.allocString(str),
            });
        },
        .floatLit => {
            const literal = c.ast.nodeString(expr);
            const val = try std.fmt.parseFloat(f64, literal);
            return CtExpr.initValue(.{
                .type = bt.Float,
                .value = cy.Value.initF64(val),
            });
        },
        .decLit => {
            const literal = c.ast.nodeString(expr);
            const val = try std.fmt.parseInt(i64, literal, 10);
            return CtExpr.initValue(.{
                .type = bt.Integer,
                .value = try c.vm.allocInt(val),
            });
        },
        .ident => {
            const name = c.ast.nodeString(expr);
            const res = try sema.lookupIdent(c, name, expr);
            switch (res) {
                .global, .local => return null,
                .static => |sym| {
                    return CtExpr.initSym(sym);
                },
                .ct_value => |ct_value| {
                    return CtExpr.initValue(ct_value);
                },
            }
        },
        .array_expr => {
            const array_expr = expr.cast(.array_expr);
            var left = try resolveCtSym(c, array_expr.left);
            if (left.type != .template) {
                return c.reportErrorFmt("Unsupported array expression.", &.{}, expr);
            }
            const template = left.cast(.template);
            if (template.kind == .ct_func) {
                const val = try cte.expandCtFuncTemplateOnCallArgs(c, template, array_expr.args, expr);
                return CtExpr.initValue(val);
            } else {
                const sym = try cte.expandTemplateOnCallArgs(c, template, array_expr.args, expr);
                return CtExpr.initSym(sym);
            }
        },
        .void => {
            const sym = c.sema.getTypeSym(bt.Void);
            return CtExpr.initSym(sym);
        },
        .ptr_slice => {
            const ptr_slice = expr.cast(.ptr_slice);
            const sym = try cte.expandTemplateOnCallArgs(c, c.sema.ptr_slice_tmpl, &.{ ptr_slice.elem }, expr);
            return CtExpr.initSym(sym);
        },
        .ptr => {
            const ptr = expr.cast(.ptr);
            const sym = try cte.expandTemplateOnCallArgs(c, c.sema.pointer_tmpl, &.{ ptr.elem }, expr);
            return CtExpr.initSym(sym);
        },
        .ref => {
            const ref = expr.cast(.ref);
            const sym = try cte.expandTemplateOnCallArgs(c, c.sema.ref_tmpl, &.{ ref.elem }, expr);
            return CtExpr.initSym(sym);
        },
        .ref_slice => {
            const ref_slice = expr.cast(.ref_slice);
            const sym = try cte.expandTemplateOnCallArgs(c, c.sema.ref_slice_tmpl, &.{ ref_slice.elem }, expr);
            return CtExpr.initSym(sym);
        },
        .expandOpt => {
            const expand_opt = expr.cast(.expandOpt);
            const sym =try cte.expandTemplateOnCallArgs(c, c.sema.option_tmpl, &.{expand_opt.param}, expr);
            return CtExpr.initSym(sym);
        },
        .accessExpr => {
            const access_expr = expr.cast(.accessExpr);
            const parent = try resolveCtSym(c, access_expr.left);
            if (access_expr.right.type() != .ident) {
                return c.reportErrorFmt("Expected identifier.", &.{}, access_expr.right);
            }
            const name = c.ast.nodeString(access_expr.right);
            const sym = try c.getResolvedDistinctSym(parent, name, access_expr.right, true);
            return CtExpr.initSym(sym);
        },
        .callExpr => {
            const call = expr.cast(.callExpr);
            if (!call.ct) {
                return c.reportError("Expected compile-time function call.", expr);
            }
            const expr_cstr = sema.Expr.init(expr);
            const res = try c.semaCallExpr(expr_cstr, true);
            if (res.resType != .ct_value) {
                return error.Unexpected;
            }
            return CtExpr.initValue(res.data.ct_value);
        },
        .group => {
            const group = expr.cast(.group);
            return try resolveCtExprOpt(c, group.child);
        },
        .func_type => {
            const func_type = expr.cast(.func_type);
            const sig = try sema.resolveFuncType(c, func_type);
            const arg = try c.vm.allocFuncSig(sig);
            defer c.vm.release(arg);
            
            const resolve_ctx = sema.getResolveContext(c);
            var sym: *cy.Sym = undefined;
            if (resolve_ctx.prefer_ct_type) {
                sym = try cte.expandTemplate(c, c.sema.func_sym_tmpl, &.{ arg });
            } else {
                if (func_type.is_union) {
                    sym = try cte.expandTemplate(c, c.sema.func_union_tmpl, &.{ arg });
                } else {
                    sym = try cte.expandTemplate(c, c.sema.func_ptr_tmpl, &.{ arg });
                }
            }
            return CtExpr.initSym(sym);
        },
        .comptimeExpr => {
            const ctx = sema.getResolveContext(c);
            const ct_expr = expr.cast(.comptimeExpr);
            if (ctx.parse_ct_inferred_params) {
                if (ct_expr.child.type() != .ident) {
                    return c.reportErrorFmt("Expected identifier.", &.{}, ct_expr.child);
                }
                const param_name = c.ast.nodeString(ct_expr.child);

                const param_idx = ctx.ct_params.size;
                const ref_t = try c.sema.ensureCtRefType(param_idx);
                const ref_v = try c.vm.allocType(ref_t);
                try sema.setResolveCtParam(c, param_name, ref_v);

                const infer_t = try c.sema.ensureCtInferType(param_idx);
                const sym = c.sema.getTypeSym(infer_t);
                return CtExpr.initSym(sym);
            }

            if (ctx.expand_ct_inferred_params) {
                const param_name = c.ast.nodeString(ct_expr.child);
                const val = ctx.ct_params.get(param_name) orelse {
                    return c.reportErrorFmt("Could not find the compile-time parameter `{}`.", &.{v(param_name)}, ct_expr.child);
                };
                if (val.getTypeId() != bt.Type) {
                    c.vm.retain(val);
                    return CtExpr.initValue(.{
                        .type = val.getTypeId(),
                        .value = val,
                    });
                } else {
                    const type_id = val.asHeapObject().type.type;
                    const sym = c.sema.getTypeSym(type_id);
                    return CtExpr.initSym(sym);
                }
            }

            // Check for ct builtins.
            const mod = c.compiler.ct_builtins_chunk.?.sym.getMod();
            if (ct_expr.child.type() == .ident) {
                const name = c.ast.nodeString(ct_expr.child);
                if (mod.getSym(name)) |sym| {
                    if (sym.getStaticType()) |type_id| {
                        return CtExpr.initValue(.{
                            .type = bt.Type,
                            .value = try c.vm.allocType(type_id),
                        });
                    }
                }
            }

            return c.reportErrorFmt("Unexpected compile-time expression.", &.{}, expr);
        },
        .objectDecl => {
            // Unnamed object.
            const sym: *cy.Sym = @ptrCast(expr.cast(.objectDecl).name);
            return CtExpr.initSym(sym);
        },
        .semaSym => {
            const sym = expr.cast(.semaSym).sym;
            return CtExpr.initSym(sym);
        },
        else => {
            return c.reportErrorFmt("Unsupported compile-time expression: `{}`", &.{v(expr.type())}, expr);
        }
    }
}
