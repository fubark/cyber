const std = @import("std");
const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypes;
const ast = cy.ast;
const sema = cy.sema;
const cte = cy.cte;
const log = cy.log.scoped(.sema_func);

pub const FuncSymResult = struct {
    dyn_call: bool,
    func: *cy.Func,
    args_loc: u32,
    nargs: u32,
};

pub const FuncResult = struct {
    func: *cy.Func,
    args_loc: u32,
    nargs: u32,
};

const ArgType = enum {
    standard,
    pre_resolved,
    skip,
};

const ResolveType = enum {
    null,
    rt,
    ct,
    incompat,
};

pub const Argument = struct {
    type: ArgType,
    resolve_t: ResolveType,
    res: sema.ExprResult,
    node: *ast.Node,

    pub fn init(node: *ast.Node) Argument {
        return .{ .type = .standard, .resolve_t = .null, .res = undefined, .node = node };
    }

    pub fn initSkip() Argument {
        return .{ .type = .skip, .resolve_t = .null, .res = undefined, .node = undefined };
    }

    pub fn initPreResolved(node: *ast.Node, res: sema.ExprResult) Argument {
        return .{
            .type = .pre_resolved,
            .resolve_t = .rt,
            .res = res,
            .node = node,
        };
    }
};

pub fn matchFunc(c: *cy.Chunk, func: *cy.sym.Func, arg_start: usize, nargs: usize, ret_cstr: cy.types.ReturnCstr, node: *ast.Node) !FuncResult {
    const func_sig = c.sema.getFuncSig(func.funcSigId);
    const params = func_sig.params();
    if (params.len != nargs) {
        return reportIncompatCallFunc(c, func, arg_start, ret_cstr, node);
    }

    const loc = try c.ir.pushEmptyArray(c.alloc, u32, nargs);

    const ct_arg_start = c.valueStack.items.len;
    defer {
        for (c.valueStack.items[ct_arg_start..]) |arg| {
            c.vm.release(arg);
        }
        c.valueStack.items.len = ct_arg_start;
    }

    var rt_arg_idx: u32 = 0;
    for (0..nargs) |i| {
        const arg = c.arg_stack.items[arg_start + i];
        if (arg.type == .skip) {
            rt_arg_idx += 1;
            continue;
        }
        const final_arg = try matchArg(c, arg, params[i], ct_arg_start, true);
        c.arg_stack.items[arg_start + i] = final_arg;

        if (final_arg.resolve_t == .rt) {
            c.ir.setArrayItem(loc, u32, rt_arg_idx, final_arg.res.irIdx);
            rt_arg_idx += 1;
        } else if (final_arg.resolve_t == .incompat) {
            return reportIncompatCallFunc(c, func, arg_start, ret_cstr, arg.node);
        }
    }

    // Check return type.
    if (!cy.types.isValidReturnType(c.compiler, func.retType, ret_cstr)) {
        return reportIncompatCallFunc(c, func, arg_start, ret_cstr, node);
    }

    if (func.type == .template) {
        // Generate function.
        const ct_args = c.valueStack.items[ct_arg_start..];
        const new_func = try cte.expandFuncTemplate(c, func, ct_args);
        return .{
            .func = new_func,
            .args_loc = loc,
            .nargs = rt_arg_idx,
        };
    } else {
        return .{
            .func = func,
            .args_loc = loc,
            .nargs = rt_arg_idx,
        };
    }
}

pub fn matchFuncSym(c: *cy.Chunk, func_sym: *cy.sym.FuncSym, arg_start: usize, nargs: usize, ret_cstr: cy.types.ReturnCstr, node: *ast.Node) !FuncSymResult {
    if (func_sym.numFuncs == 1) {
        const res = try matchFunc(c, func_sym.first, arg_start, nargs, ret_cstr, node);
        return .{
            .dyn_call = false,
            .func = res.func,
            .args_loc = res.args_loc,
            .nargs = res.nargs,
        };
    }

    const loc = try c.ir.pushEmptyArray(c.alloc, u32, nargs);

    const ct_arg_start = c.valueStack.items.len;
    defer {
        for (c.valueStack.items[ct_arg_start..]) |arg| {
            c.vm.release(arg);
        }
        c.valueStack.items.len = ct_arg_start;
    }

    var next: ?*cy.Func = func_sym.first;
    while (next) |func| {
        if (try matchOverloadedFunc(c, func, arg_start, nargs, ct_arg_start, ret_cstr, loc)) |res| {
            return res;
        }
        next = func.next;
    }

    return reportIncompatCallFuncSym(c, func_sym, arg_start, ret_cstr, node);
}

fn matchOverloadedFunc(c: *cy.Chunk, func: *cy.Func, arg_start: usize, nargs: usize, ct_arg_start: usize, ret_cstr: cy.types.ReturnCstr, args_loc: u32) !?FuncSymResult {
    const func_sig = c.sema.getFuncSig(func.funcSigId);
    const params = func_sig.params();

    if (params.len != nargs) {
        return null;
    }

    var has_dyn_arg = false;
    var rt_arg_idx: u32 = 0;
    for (0..nargs) |i| {
        const arg = c.arg_stack.items[arg_start + i];
        if (arg.type == .skip) {
            rt_arg_idx += 1;
            continue;
        }
        const final_arg = try matchArg(c, arg, params[i], ct_arg_start, false);
        c.arg_stack.items[arg_start + i] = final_arg;

        if (final_arg.resolve_t == .rt) {
            // Runtime arg.
            has_dyn_arg = has_dyn_arg or final_arg.res.type.isDynAny();
            c.ir.setArrayItem(args_loc, u32, rt_arg_idx, final_arg.res.irIdx);
            rt_arg_idx += 1;
        } else if (final_arg.resolve_t == .incompat) {
            return null;
        }
    }

    // Check return type.
    if (!cy.types.isValidReturnType(c.compiler, func.retType, ret_cstr)) {
        return null;
    }

    if (func.type == .template) {
        // Generate function.
        const ct_args = c.valueStack.items[ct_arg_start..];
        const new_func = try cte.expandFuncTemplate(c, func, ct_args);
        return .{
            .dyn_call = false,
            .func = new_func,
            .args_loc = args_loc,
            .nargs = rt_arg_idx,
        };
    } else {
        return .{
            // Becomes a dynamic call if this is an overloaded function with a dynamic arg.
            .dyn_call = has_dyn_arg,
            .func = func,
            .args_loc = args_loc,
            .nargs = rt_arg_idx,
        };
    }
}

fn resolveRtArg(c: *cy.Chunk, arg: Argument, node: *ast.Node, opt_target_t: ?cy.TypeId, single_func: bool) !sema.ExprResult {
    if (arg.resolve_t == .rt) {
        return arg.res;
    }
    switch (arg.type) {
        .standard => {
            if (opt_target_t) |target_t| {
                if (single_func) {
                    return c.semaExprTarget(node, target_t);
                } else {
                    return c.semaExpr(node, .{
                        .target_t = target_t,
                        .req_target_t = false,
                        .fit_target = true,
                        .fit_target_unbox_dyn = false,
                    });
                }
            } else {
                return c.semaExpr(node, .{});
            }
        },
        .pre_resolved => {
            return arg.res;
        },
        .skip => {
            return error.Unexpected;
        },
    }
}

fn matchArg(c: *cy.Chunk, arg: Argument, param: sema.FuncParam, ct_arg_start: usize, single_func: bool) !Argument {
    if (param.ct) {
        // Compile-time param.
        const ct_value = try cte.resolveCtValue(c, arg.node);
        if (ct_value.type != param.type) {
            c.vm.release(ct_value.value);
            var new_arg = arg;
            new_arg.resolve_t = .incompat;
            new_arg.res = sema.ExprResult.init(cy.NullId, cy.types.CompactType.initStatic(ct_value.type));
            return new_arg;
        }
        try c.valueStack.append(c.alloc, ct_value.value);
        var new_arg = arg;
        new_arg.resolve_t = .ct;
        new_arg.res = sema.ExprResult.init(cy.NullId, cy.types.CompactType.initStatic(ct_value.type));
        return new_arg;
    }

    var target_t = try resolveTargetParam(c, param.type, ct_arg_start);
    var res = try resolveRtArg(c, arg, arg.node, target_t, single_func);

    const type_e = c.sema.types.items[param.type];
    if (type_e.info.ct_infer) {
        if (!try inferCtArgs(c, res.type.id, param.type)) {
            var new_arg = arg;
            new_arg.resolve_t = .incompat;
            new_arg.res = res;
            return new_arg;
        }
        target_t = (try resolveTemplateParamType(c, param.type, ct_arg_start, true)) orelse {
            return error.Unexpected;
        };
    }

    const ct_compat = cy.types.isTypeSymCompat(c.compiler, res.type.id, target_t.?);
    const rt_compat = res.type.isDynAny();
    if (!ct_compat and !rt_compat) {
        var new_arg = arg;
        new_arg.resolve_t = .incompat;
        new_arg.res = res;
        return new_arg;
    }

    if (rt_compat and single_func) {
        // Insert rt arg type check. 
        const loc = try c.unboxOrCheck(target_t.?, res, arg.node);
        res.irIdx = loc;
        res.type = cy.types.CompactType.init(target_t.?);
    }

    var resolved_arg = arg;
    resolved_arg.res = res;
    resolved_arg.resolve_t = .rt;
    return resolved_arg;
}

/// CtInfer types are extracted from `arg_t`.
fn inferCtArgs(c: *cy.Chunk, arg_t: cy.TypeId, infer_t: cy.TypeId) !bool {
    const infer_e = c.sema.types.items[infer_t];
    if (infer_e.kind == .ct_infer) {
        const ct_arg = try c.vm.allocType(arg_t);
        try c.valueStack.append(c.alloc, ct_arg);
        return true;
    }

    // Infer nested. eg. `A[#B]`.
    const type_e = c.sema.types.items[arg_t];
    const variant = type_e.sym.getVariant() orelse {
        return false;
    };

    const infer_variant = infer_e.sym.getVariant().?;
    if (variant.root_template != infer_variant.root_template) {
        // Parent templates don't match.
        return false;
    }

    for (infer_variant.args, 0..) |arg, i| {
        if (!try inferCtArgValues(c, variant.args[i], arg)) {
            return false;
        }
    }
    return true;
}

fn inferCtArgValues(c: *cy.Chunk, arg: cy.Value, infer: cy.Value) !bool {
    if (inferCtArgValueEq(c, arg, infer)) {
        return true;
    }

    if (infer.getTypeId() != bt.Type) {
        return false;
    }

    const infer_t = infer.asHeapObject().type.type;
    const infer_e = c.sema.types.items[infer_t];
    if (infer_e.kind == .ct_infer) {
        c.vm.retain(arg);
        try c.valueStack.append(c.alloc, arg);
        return true;
    }

    if (!infer_e.info.ct_infer) {
        return false;
    }

    if (arg.getTypeId() != bt.Type) {
        return false;
    }

    const type_e = c.sema.types.items[arg.asHeapObject().type.type];
    const variant = type_e.sym.getVariant() orelse {
        return false;
    };

    const infer_variant = infer_e.sym.getVariant().?;
    if (variant.root_template != infer_variant.root_template) {
        // Parent templates don't match.
        return false;
    }

    for (infer_variant.args, 0..) |infer_arg, i| {
        if (!try inferCtArgValues(c, variant.args[i], infer_arg)) {
            return false;
        }
    }
    return true;
}

fn inferCtArgValueEq(c: *cy.Chunk, arg: cy.Value, infer: cy.Value) bool {
    _ = c;
    if (arg.getTypeId() != infer.getTypeId()) {
        return false;
    }

    switch (infer.getTypeId()) {
        bt.Type => {
            return infer.asHeapObject().type.type == arg.asHeapObject().type.type;
        },
        else => {
            // TODO: Support more arg types.
            return false;
        }
    }
}

/// Returns the target param type at index. Returns null if type should be deduced from arg.
/// resolveSymType isn't used because that performs resolving on a node rather than a type.
fn resolveTargetParam(c: *cy.Chunk, param_t: cy.TypeId, ct_arg_start: usize) !?cy.TypeId {
    const type_e = c.sema.types.items[param_t];
    if (type_e.info.ct_ref or type_e.info.ct_infer) {
        return try resolveTemplateParamType(c, param_t, ct_arg_start, false);
    } else {
        return param_t;
    }
}

fn resolveTemplateParamType(c: *cy.Chunk, param_t: cy.TypeId, ct_arg_start: usize, resolve_infer: bool) !?cy.TypeId {
    const type_e = c.sema.types.items[param_t];
    if (type_e.info.ct_infer and !resolve_infer) {
        return null;
    }
    return try resolveTemplateParamType2(c, param_t, ct_arg_start);
}

fn resolveTemplateParamType2(c: *cy.Chunk, type_id: cy.TypeId, ct_arg_start: usize) !cy.TypeId {
    const type_e = c.sema.types.items[type_id];
    if (type_e.kind == .ct_infer) {
        const ct_arg = c.valueStack.items[ct_arg_start + type_e.data.ct_infer.ct_param_idx];
        if (ct_arg.getTypeId() != bt.Type) {
            return error.TODO;
        }
        return ct_arg.asHeapObject().type.type;
    }

    if (type_e.kind == .ct_ref) {
        const ct_arg = c.valueStack.items[ct_arg_start + type_e.data.ct_ref.ct_param_idx];
        if (ct_arg.getTypeId() != bt.Type) {
            return error.TODO;
        }
        return ct_arg.asHeapObject().type.type;
    }

    // Contains nested ct_infer, ct_refs.
    if (type_e.sym.getVariant()) |variant| {
        const template = variant.root_template;

        const value_start = c.valueStack.items.len;
        defer {
            const values = c.valueStack.items[value_start..];
            for (values) |val| {
                c.vm.release(val);
            }
            c.valueStack.items.len = value_start;
        }

        for (variant.args) |arg| {
            if (arg.getTypeId() != bt.Type) {
                return error.TODO;
            }

            const arg_t = arg.asHeapObject().type.type;
            const rarg_t = try resolveTemplateParamType2(c, arg_t, ct_arg_start);
            const rarg_t_v = try c.vm.allocType(rarg_t);
            try c.valueStack.append(c.alloc, rarg_t_v);
        }

        const args = c.valueStack.items[value_start..];
        const type_sym = try cte.expandTemplate(c, template, args);
        return type_sym.getStaticType().?;
    } else {
        return error.Unexpected;
    }
}

fn reportIncompatCallFuncSym(c: *cy.Chunk, sym: *cy.sym.FuncSym, arg_start: usize, ret_cstr: cy.types.ReturnCstr, node: *ast.Node) anyerror {
    const type_start = c.typeStack.items.len;
    defer c.typeStack.items.len = type_start;

    const args = c.arg_stack.items[arg_start..];
    for (args) |arg| {
        if (arg.resolve_t == .null) {
            try c.typeStack.append(c.alloc, cy.NullId);
        } else {
            try c.typeStack.append(c.alloc, arg.res.type.id);
        }
    }
    const types = c.typeStack.items[type_start..];
    return sema.reportIncompatCallFuncSym(c, sym, types, ret_cstr, node);
}

fn reportIncompatCallFunc(c: *cy.Chunk, func: *cy.Func, arg_start: usize, ret_cstr: cy.types.ReturnCstr, node: *ast.Node) anyerror {
    const type_start = c.typeStack.items.len;
    defer c.typeStack.items.len = type_start;

    const args = c.arg_stack.items[arg_start..];
    for (args) |arg| {
        if (arg.resolve_t == .null) {
            try c.typeStack.append(c.alloc, cy.NullId);
        } else {
            try c.typeStack.append(c.alloc, arg.res.type.id);
        }
    }
    const types = c.typeStack.items[type_start..];
    return sema.reportIncompatCallFunc(c, func, types, ret_cstr, node);
}