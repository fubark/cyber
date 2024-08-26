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
    data: FuncResultUnion,
};

const FuncResultUnion = union {
    rt: struct {
        args_loc: u32,
        nargs: u32,
    },
    ct: cte.CtValue,
};

pub const FuncResult = struct {
    func: *cy.Func,
    data: FuncResultUnion,
};

const ArgType = enum {
    standard,
    pre_resolved,
    skip,
};

const ResolvedArgType = enum {
    null,
    rt,
    template,
    ct,
    incompat,
};

pub const Argument = struct {
    type: ArgType,

    /// This is wasteful since only pre_resolved uses `data`.
    /// The resolved arg result is stored together to avoid building two lists.
    data: union {
        pre_resolved: sema.ExprResult,
    },
    resolve_t: ResolvedArgType,
    res: union {
        rt: sema.ExprResult,
        ct: cte.CtValue,
        incompat: cy.TypeId,
    },
    node: *ast.Node,

    pub fn init(node: *ast.Node) Argument {
        return .{ .type = .standard, .data = undefined, .resolve_t = .null, .res = undefined, .node = node };
    }

    pub fn initSkip() Argument {
        return .{ .type = .skip, .data = undefined, .resolve_t = .null, .res = undefined, .node = undefined };
    }

    pub fn initPreResolved(node: *ast.Node, res: sema.ExprResult) Argument {
        return .{
            .type = .pre_resolved,
            .data = .{ .pre_resolved = res },
            .resolve_t = .null,
            .res = undefined,
            .node = node,
        };
    }
};

pub fn matchFunc(c: *cy.Chunk, func: *cy.sym.Func, arg_start: usize, nargs: usize, cstr: CallCstr, node: *ast.Node) !FuncResult {
    const func_sig = c.sema.getFuncSig(func.funcSigId);
    const params = func_sig.params();
    if (params.len != nargs) {
        return reportIncompatCallFunc(c, func, arg_start, cstr.ret, node);
    }

    const loc = try c.ir.pushEmptyArray(c.alloc, u32, nargs);

    // ct/rt args share the same index counter.
    var rt_arg_idx: u32 = 0;

    var ct_arg_start: usize = undefined;
    if (cstr.ct_call) {
        // First reserve the ct args since the exact amount is known.
        ct_arg_start = c.valueStack.items.len;
        try c.valueStack.resize(c.alloc, ct_arg_start + nargs);
    }
    const template_arg_start = c.valueStack.items.len;
    defer {
        if (cstr.ct_call) {
            for (c.valueStack.items[ct_arg_start..ct_arg_start+rt_arg_idx]) |arg| {
                c.vm.release(arg);
            }
            for (c.valueStack.items[template_arg_start..]) |arg| {
                c.vm.release(arg);
            }
            c.valueStack.items.len = ct_arg_start;
        } else {
            for (c.valueStack.items[template_arg_start..]) |arg| {
                c.vm.release(arg);
            }
            c.valueStack.items.len = template_arg_start;
        }
    }

    const config = MatchConfig{
        .template_arg_start = template_arg_start,
        .single_func = true,
        .ct_call = cstr.ct_call,
    };
    for (0..nargs) |i| {
        const arg = c.arg_stack.items[arg_start + i];
        if (arg.type == .skip) {
            rt_arg_idx += 1;
            continue;
        }
        const final_arg = try matchArg(c, arg, params[i], config);
        c.arg_stack.items[arg_start + i] = final_arg;

        if (final_arg.resolve_t == .rt) {
            c.ir.setArrayItem(loc, u32, rt_arg_idx, final_arg.res.rt.irIdx);
            rt_arg_idx += 1;
        } else if (final_arg.resolve_t == .ct) {
            c.valueStack.items[ct_arg_start + rt_arg_idx] = final_arg.res.ct.value;
            rt_arg_idx += 1;
        } else if (final_arg.resolve_t == .incompat) {
            return reportIncompatCallFunc(c, func, arg_start, cstr.ret, arg.node);
        }
    }

    // Check return type.
    if (!cy.types.isValidReturnType(c.compiler, func.retType, cstr.ret)) {
        return reportIncompatCallFunc(c, func, arg_start, cstr.ret, node);
    }

    var final_func = func;
    if (func.type == .template) {
        // Generate function.
        const template_args = c.valueStack.items[template_arg_start..];
        final_func = try cte.expandFuncTemplate(c, func, template_args);
    }
    if (cstr.ct_call) {
        const ct_args = c.valueStack.items[ct_arg_start..];
        const res = try cte.callFunc(c, final_func, ct_args);
        return .{
            .func = final_func,
            .data = .{ .ct = res },
        };
    } else {
        return .{
            .func = final_func,
            .data = .{ .rt = .{
                .args_loc = loc,
                .nargs = rt_arg_idx,
            }}
        };
    }
}

pub const CallCstr = struct {
    ret: cy.types.ReturnCstr,
    ct_call: bool = false,
};

pub fn matchFuncSym(c: *cy.Chunk, func_sym: *cy.sym.FuncSym, arg_start: usize, nargs: usize,
    cstr: CallCstr, node: *ast.Node) !FuncSymResult {

    if (func_sym.numFuncs == 1) {
        const res = try matchFunc(c, func_sym.first, arg_start, nargs, cstr, node);
        return .{
            .dyn_call = false,
            .func = res.func,
            .data = res.data,
        };
    }

    const loc = try c.ir.pushEmptyArray(c.alloc, u32, nargs);

    const template_arg_start = c.valueStack.items.len;
    defer {
        for (c.valueStack.items[template_arg_start..]) |arg| {
            c.vm.release(arg);
        }
        c.valueStack.items.len = template_arg_start;
    }

    var next: ?*cy.Func = func_sym.first;
    while (next) |func| {
        if (try matchOverloadedFunc(c, func, arg_start, nargs, template_arg_start, cstr, loc)) |res| {
            return res;
        }
        next = func.next;
    }

    return reportIncompatCallFuncSym(c, func_sym, arg_start, cstr.ret, node);
}

fn matchOverloadedFunc(c: *cy.Chunk, func: *cy.Func, arg_start: usize, nargs: usize, template_arg_start: usize, cstr: CallCstr, args_loc: u32) !?FuncSymResult {
    const func_sig = c.sema.getFuncSig(func.funcSigId);
    const params = func_sig.params();

    if (params.len != nargs) {
        return null;
    }

    const config = MatchConfig{
        .template_arg_start = template_arg_start,
        .single_func = false,
        .ct_call = cstr.ct_call,
    };

    var has_dyn_arg = false;
    var rt_arg_idx: u32 = 0;
    for (0..nargs) |i| {
        const arg = c.arg_stack.items[arg_start + i];
        if (arg.type == .skip) {
            rt_arg_idx += 1;
            continue;
        }
        const final_arg = try matchArg(c, arg, params[i], config);
        c.arg_stack.items[arg_start + i] = final_arg;

        if (final_arg.resolve_t == .rt) {
            // Runtime arg.
            has_dyn_arg = has_dyn_arg or final_arg.res.rt.type.isDynAny();
            c.ir.setArrayItem(args_loc, u32, rt_arg_idx, final_arg.res.rt.irIdx);
            rt_arg_idx += 1;
        } else if (final_arg.resolve_t == .incompat) {
            return null;
        }
    }

    // Check return type.
    if (!cy.types.isValidReturnType(c.compiler, func.retType, cstr.ret)) {
        return null;
    }

    var final_func = func;
    if (func.type == .template) {
        // Generate function.
        const template_args = c.valueStack.items[template_arg_start..];
        final_func = try cte.expandFuncTemplate(c, func, template_args);
        has_dyn_arg = false;
    }
    if (cstr.ct_call) {
        return error.TODO;
    } else {
        return .{
            // Becomes a dynamic call if this is an overloaded function with a dynamic arg.
            .dyn_call = has_dyn_arg,
            .func = final_func,
            .data = .{ .rt = .{
                .args_loc = args_loc,
                .nargs = rt_arg_idx,
            }},
        };
    }
}

fn resolveRtArg(c: *cy.Chunk, arg: Argument, node: *ast.Node, opt_target_t: ?cy.TypeId, single_func: bool) !sema.ExprResult {
    if (arg.resolve_t == .rt) {
        return arg.res.rt;
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
            return arg.data.pre_resolved;
        },
        .skip => {
            return error.Unexpected;
        },
    }
}

const MatchConfig = struct {
    template_arg_start: usize,
    single_func: bool,

    /// Whether the function is intended to be invoked at compile-time.
    ct_call: bool,
};

fn matchArg(c: *cy.Chunk, arg: Argument, param: sema.FuncParam, config: MatchConfig) !Argument {
    if (param.template or config.ct_call) {
        // Compile-time param.
        const ct_value = (try cte.resolveCtValueOpt(c, arg.node)) orelse {
            if (config.ct_call) {
                return c.reportError("Expected compile-time argument.", arg.node);
            }
            var new_arg = arg;
            new_arg.resolve_t = .incompat;
            new_arg.res = .{ .incompat = cy.NullId };
            return new_arg;
        };
        if (ct_value.type != param.type) {
            c.vm.release(ct_value.value);
            var new_arg = arg;
            new_arg.resolve_t = .incompat;
            new_arg.res = .{ .incompat = ct_value.type };
            return new_arg;
        }
        var new_arg = arg;
        if (param.template) {
            try c.valueStack.append(c.alloc, ct_value.value);
            new_arg.resolve_t = .template;
        } else {
            new_arg.resolve_t = .ct;
            new_arg.res = .{ .ct = ct_value };
        }
        return new_arg;
    }

    var target_t = try resolveTargetParam(c, param.type, config.template_arg_start);
    var res = try resolveRtArg(c, arg, arg.node, target_t, config.single_func);

    const type_e = c.sema.types.items[param.type];
    if (type_e.info.ct_infer) {
        if (!try inferCtArgs(c, res.type.id, param.type)) {
            var new_arg = arg;
            new_arg.resolve_t = .incompat;
            new_arg.res = .{ .incompat = res.type.id };
            return new_arg;
        }
        target_t = (try resolveTemplateParamType(c, param.type, config.template_arg_start, true)) orelse {
            return error.Unexpected;
        };
    }

    const ct_compat = cy.types.isTypeSymCompat(c.compiler, res.type.id, target_t.?);
    const rt_compat = res.type.isDynAny();
    if (!ct_compat and !rt_compat) {
        var new_arg = arg;
        new_arg.resolve_t = .incompat;
        new_arg.res = .{ .incompat = res.type.id };
        return new_arg;
    }

    if (rt_compat and config.single_func) {
        // Insert rt arg type check. 
        const loc = try c.unboxOrCheck(target_t.?, res, arg.node);
        res.irIdx = loc;
        res.type = cy.types.CompactType.init(target_t.?);
    }

    var resolved_arg = arg;
    resolved_arg.res = .{ .rt = res };
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
    if (variant.getSymTemplate() != infer_variant.getSymTemplate()) {
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
    if (variant.getSymTemplate() != infer_variant.getSymTemplate()) {
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
        } else if (arg.resolve_t == .rt) {
            try c.typeStack.append(c.alloc, arg.res.rt.type.id);
        } else if (arg.resolve_t == .ct) {
            try c.typeStack.append(c.alloc, arg.res.ct.type);
        } else {
            try c.typeStack.append(c.alloc, arg.res.incompat);
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
        } else if (arg.resolve_t == .rt) {
            try c.typeStack.append(c.alloc, arg.res.rt.type.id);
        } else if (arg.resolve_t == .ct) {
            try c.typeStack.append(c.alloc, arg.res.ct.type);
        } else {
            try c.typeStack.append(c.alloc, arg.res.incompat);
        }
    }
    const types = c.typeStack.items[type_start..];
    return sema.reportIncompatCallFunc(c, func, types, ret_cstr, node);
}