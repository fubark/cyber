const std = @import("std");
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const bt = cy.types.BuiltinTypes;
const ast = cy.ast;
const sema = cy.sema;
const sema_type = cy.sema_type;
const cte = cy.cte;
const log = cy.log.scoped(.sema_func);
const fmt = cy.fmt;
const v = fmt.v;
const ir = cy.ir;
const Value = cy.Value;

const FuncResultKind = enum(u8) {
    rt,
    rt_sema,
    ct,

    // NOTE: See if most functions can return the final ExprResult to avoid bubbling up args. Try to remove `rt`, `rt_sema`.
    expr_res,
};

pub const FuncSymResult = struct {
    func: *cy.Func,
    kind: FuncResultKind,
    data: FuncResultUnion,
};

const FuncResultUnion = union {
    rt: struct {
        args: [*]*ir.Expr,
        nargs: u32,
        ret_parent_scope_local: ?u32,
    },
    expr_res: sema.ExprResult,
    ct: sema.ExprResult,
};

const FuncSigCtResult = struct {
    arg_start: usize,
    argt_start: usize,
};

pub const FuncResult = struct {
    func: *cy.Func,
    kind: FuncResultKind,
    data: FuncResultUnion,
};

const ArgType = enum {
    standard,
    receiver,
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
        receiver: sema.ExprResult,
    },
    resolve_t: ResolvedArgType,
    res: union {
        rt: sema.ExprResult,
        ct: cy.TypeValue,
        template: cy.TypeValue,
        incompat: void,
    },
    node: *ast.Node,

    pub fn init(node: *ast.Node) Argument {
        return .{ .type = .standard, .data = undefined, .resolve_t = .null, .res = undefined, .node = node };
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

    pub fn initReceiver(node: *ast.Node, res: sema.ExprResult) Argument {
        return .{
            .type = .receiver,
            .data = .{ .receiver = res },
            .resolve_t = .null,
            .res = undefined,
            .node = node,
        };
    }
};

const MatchGenericFuncCtResult = struct {
    func: *cy.Func,
    res: sema.ExprResult,
};

pub fn matchGenericFuncCt(c: *cy.Chunk, template: *cy.sym.FuncTemplate,
    pre_args: []const Argument, end_args: []const *ast.Node, cstr: cte.CallCstr,
    match_err: *FuncMatchError, node: *ast.Node) !?MatchGenericFuncCtResult {

    try sema.ensureResolvedFuncTemplate(c, template);

    const nargs = pre_args.len + end_args.len;
    
    if (template.func_params.len != nargs) {
        match_err.* = FuncMatchError.init(undefined, 0, undefined);
        return null;
    }

    // Prepare template context.
    try sema.pushSymResolveContext(c, @ptrCast(template), node);
    var template_ctx = sema.saveResolveContext(c);
    template_ctx.has_ct_params = true;
    defer template_ctx.deinit(c);

    const arg_start = c.valueStack.items.len;
    const argt_start = c.typeStack.items.len;

    // First reserve the ct args since the upper bound is known.
    try c.valueStack.resize(c.alloc, arg_start + nargs);
    try c.typeStack.resize(c.alloc, argt_start + nargs);

    var arg_idx: u32 = 0;
    defer {
        for (0..arg_idx) |i| {
            const arg = c.valueStack.items[arg_start + i];
            const arg_t = c.typeStack.items[argt_start + i];

            // Only reference args are owned by the callsite.
            if (arg_t.isObjectLike()) {
                c.heap.destructValue2(arg_t, arg);
            }
        }
        c.valueStack.items.len = arg_start;
        c.typeStack.items.len = argt_start;
    }

    const params = template.sig.params();
    for (0..nargs) |i| {
        var arg: Argument = undefined;
        if (i < pre_args.len) {
            arg = pre_args[i];
        } else {
            arg = Argument.init(end_args[i - pre_args.len]);
        }
        const final_arg = try matchTemplateArgCt(c, params[i], arg, template, &template_ctx, i, cstr.req_value);
        if (final_arg.resolve_t == .ct) {
            c.valueStack.items[arg_start + arg_idx] = final_arg.res.ct.value;
            c.typeStack.items[argt_start + arg_idx] = final_arg.res.ct.type;
            arg_idx += 1;
        } else if (final_arg.resolve_t == .template) {
            const name = template.func_params[i].name_type.name();
            try template_ctx.initCtParam(c.alloc, name, final_arg.res.template);
        } else if (final_arg.resolve_t == .incompat) {
            return error.Unexpected;
        }
    }

    const gen_func = try genFunc(c, template, &template_ctx, node);

    const ct_args = c.valueStack.items[arg_start..arg_start+arg_idx];
    const res = try cte.evalCall(c, gen_func, ct_args, node);
    return .{
        .func = gen_func,
        .res = res,
    };
}

const MatchGenericFuncRt = struct {
    func: *cy.Func,
    nargs: usize,
    ret_parent_scope_local: ?u32,
};

pub fn matchGenericFuncRt(c: *cy.Chunk, template: *cy.sym.FuncTemplate,
    pre_args: []const Argument, end_args: []const *ast.Node, args: []*ir.Expr,
    overloaded: bool, match_err: *FuncMatchError, node: *ast.Node) !?MatchGenericFuncRt {
    _ = overloaded;

    try sema.ensureResolvedFuncTemplate(c, template);

    const nargs = pre_args.len + end_args.len;
    
    if (template.func_params.len != nargs) {
        match_err.* = FuncMatchError.init(undefined, 0, undefined);
        return null;
    }

    // Prepare template context.
    try sema.pushSymResolveContext(c, @ptrCast(template), node);
    var template_ctx = sema.saveResolveContext(c);
    template_ctx.has_ct_params = true;
    defer template_ctx.deinit(c);

    var arg_idx: u32 = 0;

    const params = template.sig.params();
    for (0..nargs) |i| {
        var arg: Argument = undefined;
        if (i < pre_args.len) {
            arg = pre_args[i];
        } else {
            arg = Argument.init(end_args[i - pre_args.len]);
        }
        const final_arg = try matchTemplateArg(c, params[i], arg, template, &template_ctx, i, false);
        if (final_arg.resolve_t == .rt) {
            args[arg_idx] = final_arg.res.rt.ir;
            arg_idx += 1;
        } else if (final_arg.resolve_t == .template) {
            const name = template.func_params[i].name_type.name();
            try template_ctx.initCtParam(c.alloc, name, final_arg.res.template);
        } else if (final_arg.resolve_t == .incompat) {
            return error.Unexpected;
        }
    }

    const func = try genFunc(c, template, &template_ctx, node);

    return .{
        .func = func,
        .nargs = arg_idx,
        .ret_parent_scope_local = null,
    };
}

// pub fn matchGenericFunc(c: *cy.Chunk, template: *cy.sym.FuncTemplate,
//     pre_args: []const Argument, end_args: []const *ast.Node, req_value: bool, node: *ast.Node) !sema.ExprResult {

//     const ct_call = req_value or template.ct;
//     var match_err: FuncMatchError = undefined;
//     if (ct_call) {
//         const call_cstr = cte.CallCstr{
//             .req_value = req_value,
//         };
//         const res = (try matchGenericFuncCt(c, template, pre_args, end_args, call_cstr, &match_err, node)) orelse {
//             return reportCallFuncMismatch(c, pre_args, end_args, template.head.name(), template.sig, match_err, node);
//         };
//         return res.res;
//     } else {
//         const args = try c.ir.allocArray(*ir.Expr, pre_args.len + end_args.len);
//         const res = (try matchGenericFuncRt(c, template, pre_args, end_args, args, false, &match_err, node)) orelse {
//             return reportCallFuncMismatch(c, pre_args, end_args, template.head.name(), template.sig, match_err, node);
//         };
//         const call_ir = try c.ir.newExpr(.call, res.func.sig.ret, node, .{
//             .func = res.func,
//             .numArgs = @intCast(res.nargs),
//             .args = args.ptr,
//         });
//         return sema.ExprResult.initOwned(call_ir);
//     }
// }

fn genFunc(c: *cy.Chunk, template: *cy.sym.FuncTemplate, template_ctx: *sema.ResolveContext, node: *ast.Node) !*cy.Func {
    // Build template args from context.
    const arg_start = c.valueStack.items.len;
    const argt_start = c.typeStack.items.len;
    defer {
        // for (c.valueStack.items[arg_start..], 0..) |value, i| {
        //     const val_t = c.typeStack.items[argt_start + i];
        //     c.heap.destructValue2(val_t, value);
        // }
        c.valueStack.items.len = arg_start;
        c.typeStack.items.len = argt_start;
    }
    for (template.params) |tparam| {
        const param = template_ctx.ct_params.get(tparam.name).?;
        const param_t = param.getType();
        try c.typeStack.append(c.alloc, param_t);
        try c.valueStack.append(c.alloc, param.value);
    }

    // Generate function.
    const args = c.valueStack.items[arg_start..];
    const arg_types = c.typeStack.items[argt_start..];
    return cy.template.expandResolvedFuncTemplate(c, template, arg_types, args, true, node);
}

fn getVmCVariantFunc(c: *cy.Chunk, extern_func: *cy.Func, args: []*ir.Expr, node: *ast.Node) !*cy.Func {
    // Specialize on variadic signature.
    const param_start = c.func_param_stack.items.len;
    defer c.func_param_stack.items.len = param_start;

    const params = extern_func.sig.params();
    for (params[0..params.len-1]) |param| {
        try c.func_param_stack.append(c.alloc, param);
    }
    for (args[params.len-1..]) |varg| {
        try c.func_param_stack.append(c.alloc, sema.FuncParam.init(varg.type));
    }
    const sig = try c.sema.ensureFuncSig(c.func_param_stack.items[param_start..], extern_func.sig.ret);

    const res = try c.sema.vm_c_variadic_funcs.getOrPut(c.alloc, .{ .func = extern_func, .sig = sig });
    if (!res.found_existing) {
        const config = cy.sym.FuncConfig{
            .is_method = false,
            .extern_ = true,
        };
        const func = try c.createFunc(.vm_extern_variant, extern_func.parent, config, extern_func.decl);
        func.data = .{
            .vm_extern_variant = .{ .extern_func = extern_func },
        };
        c.resolveFuncInfo(func, sig);

        try c.funcs.append(c.alloc, func);

        try sema.resolveFuncVariant(c, func, false, node);
        res.value_ptr.* = func;

        if (!c.has_extern_func) {
            c.has_extern_func = true;
        }
    }
    return res.value_ptr.*;
}

pub fn matchFuncRt(c: *cy.Chunk, func: *cy.Func, pre_args: []const Argument,
    end_args: []const *ast.Node, node: *ast.Node) !FuncResult {

    var match_err: FuncMatchError = undefined;

    switch (func.type) {
        .reserved => {
            const name = try c.sema.newFuncName(c.alloc, func, .{});
            defer c.alloc.free(name);
            return c.reportErrorFmt("Cannot invoke the reserved function `{}`.", &.{v(name)}, node);
        },
        .generic => {
            const args = try c.ir.allocArray(*ir.Expr, pre_args.len + end_args.len);
            const res = (try matchGenericFuncRt(c, func.data.generic, pre_args, end_args, args, false, &match_err, node)) orelse {
                return reportCallFuncMismatch2(c, pre_args, end_args, func, match_err, node);
            };
            const nargs = res.nargs;
            return .{
                .func = res.func,
                .kind = .rt,
                .data = .{ .rt = .{
                    .args = args.ptr,
                    .nargs = @intCast(nargs),
                    .ret_parent_scope_local = null,
                }},
            };
        },
        .host_builtin => {
            std.debug.assert(pre_args.len == 0);
            if (end_args.len != func.sig.params_len) {
                const name = try c.sema.newFuncName(c.alloc, func, .{});
                defer c.alloc.free(name);
                return c.reportErrorFmt("Expected {} arguments for `{}`, found {}.", &.{v(func.sig.params_len), v(func.name()), v(end_args.len)}, node);
            }
            const ctx = cy.BuiltinContext{
                .func = func, 
                .args = end_args.ptr,
                .node = node,
            };
            var res: sema.ExprResult = undefined;
            if (!func.data.host_builtin.ptr(c, &ctx, &res)) {
                return error.CompileError;
            }
            return .{
                .func = func,
                .kind = .expr_res,
                .data = .{ .expr_res = res },
            };
        },
        else => {
            const args = try c.ir.allocArray(*ir.Expr, pre_args.len + end_args.len);
            const nargs = args.len;
            const res = (try matchFuncSig(c, func.sig, pre_args, end_args, args, false, &match_err)) orelse {
                return reportCallFuncMismatch2(c, pre_args, end_args, func, match_err, node);
            };
            var final_func = func;
            if (func.type == .extern_ and func.data.extern_.vm_variadic) {
                final_func = try getVmCVariantFunc(c, func, args, node);
            }
            return .{
                .func = final_func,
                .kind = .rt,
                .data = .{ .rt = .{
                    .args = args.ptr,
                    .nargs = @intCast(nargs),
                    .ret_parent_scope_local = res.ret_parent_scope_local,
                }},
            };
        },
    }
}

pub fn matchFunc(c: *cy.Chunk, func: *cy.sym.Func, pre_args: []const Argument, end_args: []const *ast.Node, req_value: bool, node: *ast.Node) !FuncResult {
    // Assumes resolved func for `const_eval_only` and signature.
    const ct_call = req_value or func.info.const_eval_only;
    if (ct_call) {
        const call_cstr = cte.CallCstr{
            .req_value = req_value,
        };
        return matchFuncCt(c, func, pre_args, end_args, call_cstr, node);
    } else {
        return matchFuncRt(c, func, pre_args, end_args, node);
    }
}

pub const FuncMatchError = struct {
    // 0 indicates wrong number of arguments.
    // 1 indicates the first argument is incompatible.
    arg: usize,
    arg_t: *cy.Type,
    param: sema.FuncParam,

    fn init(param: sema.FuncParam, arg: usize, arg_t: *cy.Type) FuncMatchError {
        return .{
            .arg = arg,
            .arg_t = arg_t,
            .param = param,
        };
    }
};

pub fn matchFuncSigCt(c: *cy.Chunk, sig: *cy.FuncSig, pre_args: []const Argument,
    args: []const *ast.Node, cstr: cte.CallCstr, match_err: *FuncMatchError, node: *ast.Node) !?FuncSigCtResult {

    _ = node;

    const params = sig.params();
    if (params.len != pre_args.len + args.len) {
        match_err.* = FuncMatchError.init(undefined, 0, undefined);
        return null;
    }

    const arg_start = c.valueStack.items.len;
    const argt_start = c.typeStack.items.len;

    var arg_idx: usize = 0;
    var matched = false;

    // First reserve the ct args since the exact amount is known.
    try c.valueStack.resize(c.alloc, arg_start + params.len);
    try c.typeStack.resize(c.alloc, argt_start + params.len);
    defer {
        if (!matched) {
            for (0..arg_idx) |i| {
                const arg = c.valueStack.items[arg_start + i];
                const arg_t = c.typeStack.items[argt_start + i];

                c.heap.destructValue2(arg_t, arg);
            }
            c.valueStack.items.len = arg_start;
            c.typeStack.items.len = argt_start;
        }
    }

    while (arg_idx < pre_args.len) : (arg_idx += 1) {
        var matched_arg: bool = undefined;
        const value = try matchPreArgCt(c, pre_args[arg_idx], params[arg_idx], cstr.req_value, &matched_arg);

        if (!matched_arg) {
            match_err.* = FuncMatchError.init(params[arg_idx], arg_idx + 1, value.type);
            return null;
        }
        c.valueStack.items[arg_start + arg_idx] = value.value;
        c.typeStack.items[argt_start + arg_idx] = value.type;
    }

    while (arg_idx < params.len) : (arg_idx += 1) {
        var matched_arg: bool = undefined;
        const value = try matchArgCt(c, args[arg_idx - pre_args.len], params[arg_idx], cstr.req_value, &matched_arg);

        if (!matched_arg) {
            match_err.* = FuncMatchError.init(params[arg_idx], arg_idx + 1, value.type);
            return null;
        }
        c.valueStack.items[arg_start + arg_idx] = value.value;
        c.typeStack.items[argt_start + arg_idx] = value.type;
    }

    matched = true;
    return .{
        .arg_start = arg_start,
        .argt_start = argt_start,
    };
}

const MatchFuncSigResult = struct {
    ret_parent_scope_local: ?u32,
};

pub fn matchFuncSig(c: *cy.Chunk, sig: *cy.FuncSig, pre_args: []const Argument, end_args: []const *ast.Node,
    args: []*ir.Expr, overloaded: bool, match_err: *FuncMatchError) !?MatchFuncSigResult {

    const params = sig.params();
    const nargs = pre_args.len + end_args.len;
    if (params.len != nargs) {
        // Continue if at least has variadic minimum.
        if (params.len == 0 or !(params[params.len-1].get_type().kind() == .c_variadic and nargs >= params.len - 1)) {
            match_err.* = FuncMatchError.init(undefined, 0, undefined);
            return null;
        }
    }

    const capture_param_scope = sig.scope_param_idx != cy.NullU8;
    var ret_parent_scope_local: ?u32 = null;

    var arg_idx: u32 = 0;

    for (0..nargs) |i| {
        var arg: Argument = undefined;
        if (i < pre_args.len) {
            arg = pre_args[i];
            if (arg.type == .skip) {
                arg_idx += 1;
                continue;
            }
        } else {
            arg = Argument.init(end_args[i - pre_args.len]);
        }

        var param: sema.FuncParam = undefined;
        if (i >= params.len) {
            // C variadic arg.
            param = params[params.len-1];
        } else {
            param = params[i];
        }

        var match: bool = undefined;
        const arg_res = try matchArg(c, arg, param, overloaded, &match);
        if (!match) {
            match_err.* = FuncMatchError.init(param, arg_idx + 1, arg_res.type);
            return null;
        }
        if (capture_param_scope and sig.scope_param_idx == i) {
            ret_parent_scope_local = arg_res.recParentLocal();
        }
        args[arg_idx] = arg_res.ir;
        arg_idx += 1;
    }

    return .{
        .ret_parent_scope_local = ret_parent_scope_local,
    };
}

pub fn matchFuncSym2(c: *cy.Chunk, func_sym: *cy.sym.FuncSym, pre_args: []const Argument, end_args: []const *ast.Node,
    req_value: bool, node: *ast.Node) anyerror!sema.ExprResult {

    try sema.referenceSym(c, @ptrCast(func_sym), node);

    const res = try matchFuncSym(c, func_sym, pre_args, end_args, req_value, node);
    if (res.kind == .ct) {
        return res.data.ct;
    }

    switch (res.func.type) {
        .host_builtin => {
            return res.data.expr_res;
        },
        else => {
            const expr = try c.ir.newExpr(.call, res.func.sig.ret, node, .{ 
                .func = res.func,
                .numArgs = @as(u8, @intCast(res.data.rt.nargs)),
                .args = res.data.rt.args,
            });
            var expr_res = sema.ExprResult.initOwned(expr);
            expr_res.data.value.parent_local = res.data.rt.ret_parent_scope_local;
            return expr_res;
        }
    }
}

pub fn matchFuncSym(c: *cy.Chunk, func_sym: *cy.sym.FuncSym, pre_args: []const Argument, end_args: []const *ast.Node,
    req_value: bool, node: *ast.Node) anyerror!FuncSymResult {

    if (func_sym.numFuncs == 1) {
        const res = try matchFunc(c, func_sym.first, pre_args, end_args, req_value, node);
        return .{
            .func = res.func,
            .kind = res.kind,
            .data = res.data,
        };
    }

    const nargs = pre_args.len + end_args.len;
    var loc: []*ir.Expr = undefined;
    if (!req_value) {
        loc = try c.ir.allocArray(*ir.Expr, nargs);
    }

    var best_func = func_sym.first;
    // `node` if 0 otherwise (best_err_arg-1) indexes into `pre_args` and `end_args`.
    // if == nargs, there is not enough arguments.
    var best_match_err = FuncMatchError.init(undefined, 0, undefined);

    var next: ?*cy.Func = func_sym.first;
    while (next) |func| {
        var match_err: FuncMatchError = undefined;

        const ct_call = req_value or func.info.const_eval_only;
        if (ct_call) {
            if (try matchOverloadedFuncCt(c, func, pre_args, end_args, req_value, &match_err, node)) |res| {
                return .{
                    .func = func,
                    .kind = res.kind,
                    .data = res.data,
                };
            }
            if (match_err.arg > best_match_err.arg) {
                best_func = func;
                best_match_err = match_err;
            }
            next = func.next;
        } else {
            // Save last statement to clear any that were added on a mismatch.
            const start_stmt = c.ir.curBlock().last;
            const var_start = c.varStack.items.len;

            if (try matchOverloadedFunc(c, func, pre_args, end_args, loc, &match_err, node)) |res| {
                const sig = try c.sema.allocFuncSigStr(func.sig, true, null);
                defer c.alloc.free(sig);
                return .{
                    .func = res.func,
                    .kind = .rt,
                    .data = .{ .rt = .{
                        .args = loc.ptr,
                        .nargs = @intCast(res.nargs),
                        .ret_parent_scope_local = res.ret_parent_scope_local,
                    }},
                };
            }

            // Discard any locals created during matching.
            if (var_start < c.varStack.items.len) {
                c.varStack.items.len = var_start;
            }

            if (start_stmt != null) {
                // TODO: Also remove the statements and expressions generated.
                start_stmt.?.next = null;
                c.ir.curBlock().last = start_stmt.?;
            } else {
                c.ir.curBlock().last = null;
            }

            if (match_err.arg > best_match_err.arg) {
                best_func = func;
                best_match_err = match_err;
            }
            next = func.next;
        }
    }

    return reportCallFuncMismatch2(c, pre_args, end_args, best_func, best_match_err, node);
}

pub fn matchFuncCt(c: *cy.Chunk, func: *cy.Func, pre_args: []const Argument, end_args: []const *ast.Node, cstr: cte.CallCstr, node: *ast.Node) !FuncResult {
    try sema.ensure_resolved_func(c, func, true, node);
    switch (func.type) {
        .generic => {
            var match_err: FuncMatchError = undefined;
            const res = (try matchGenericFuncCt(c, func.data.generic, pre_args, end_args, cstr, &match_err, node)) orelse {
                return reportCallFuncMismatch2(c, pre_args, end_args, func, match_err, node);
            };
            return .{
                .func = res.func,
                .kind = .ct,
                .data = .{ .ct = res.res },
            };
        },
        .reserved => {
            const name = try c.sema.newFuncName(c.alloc, func, .{});
            defer c.alloc.free(name);
            return c.reportErrorFmt("Cannot invoke the reserved function `{}`.", &.{v(name)}, node);
        },
        .host_builtin => {
            const ptr = func.data.host_builtin.eval orelse {
                const name = try c.sema.newFuncName(c.alloc, func, .{});
                defer c.alloc.free(name);
                return c.reportErrorFmt("Unsupported const eval of `{}`.", &.{v(func.name())}, node);
            };
            std.debug.assert(pre_args.len == 0);
            if (end_args.len != func.sig.params_len) {
                const name = try c.sema.newFuncName(c.alloc, func, .{});
                defer c.alloc.free(name);
                return c.reportErrorFmt("Expected {} arguments for `{}`, found {}.", &.{v(func.sig.params_len), v(func.name()), v(end_args.len)}, node);
            }
            const ctx = cy.BuiltinContext{
                .func = func, 
                .args = end_args.ptr,
                .node = node,
            };
            const res = ptr(c, &ctx);
            if (res.type.id() == bt.Null) {
                return error.CompileError;
            }
            try sema_type.ensure_eval_eligible_type(c, res.type, node);
            return .{
                .func = func,
                .kind = .ct,
                .data = .{ .ct = sema.ExprResult.initCtValue(res) },
            };
        },
        else => {
            var match_err: FuncMatchError = undefined;
            const res = (try matchFuncSigCt(c, func.sig, pre_args, end_args, cstr, &match_err, node)) orelse {
                return reportCallFuncMismatch2(c, pre_args, end_args, func, match_err, node);
            };
            const ct_args = c.valueStack.items[res.arg_start..];
            defer {
                // No destruction after const eval call.
                // Unlike runtime, compile-time callsite doesn't try to manage an eval_ref (all arguments are copied to the callee).
                c.valueStack.items.len = res.arg_start;
                c.typeStack.items.len = res.argt_start;
            }
            const ct_res = try cte.evalCall(c, func, ct_args, node);
            if (cstr.req_value and ct_res.resType != .ct_value) {
                return c.reportErrorFmt("Expected compile-time value.", &.{}, node);
            }
            return .{
                .func = func,
                .kind = .ct,
                .data = .{ .ct = ct_res },
            };
        }
    }
}

fn matchOverloadedFuncCt(c: *cy.Chunk, func: *cy.Func, pre_args: []const Argument, end_args: []const *ast.Node, req_value: bool, match_err: *FuncMatchError, node: *ast.Node) !?FuncResult {
    try sema.ensure_resolved_func(c, func, true, node);
    const call_cstr = cte.CallCstr{
        .req_value = req_value,
    };

    if (func.type == .generic) {
        const res = try matchGenericFuncCt(c, func.data.generic, pre_args, end_args, call_cstr, match_err, node) orelse {
            return null;
        };
        return .{
            .func = res.func,
            .kind = .ct,
            .data = .{ .ct = res.res },
        };
    } else {
        const res = (try matchFuncSigCt(c, func.sig, pre_args, end_args, call_cstr, match_err, node)) orelse {
            return null;
        };
        defer {
            const args = c.valueStack.items[res.arg_start..];
            for (args, 0..) |arg, i| {
                const arg_t = c.typeStack.items[res.argt_start + i];
                // Only reference args are owned by the callsite.
                if (arg_t.isObjectLike()) {
                    c.heap.destructValue2(arg_t, arg);
                }
            }
            c.valueStack.items.len = res.arg_start;
            c.typeStack.items.len = res.argt_start;
        }

        const ct_res = try cte.evalCall(c, func, c.valueStack.items[res.arg_start..], node);
        if (req_value and ct_res.resType != .ct_value) {
            return c.reportErrorFmt("Expected compile-time value.", &.{}, node);
        }
        return .{
            .func = func,
            .kind = .ct,
            .data = .{ .ct = ct_res },
        };
    }
}

const MatchOverloadedFuncResult = struct {
    func: *cy.Func,
    nargs: usize,
    ret_parent_scope_local: ?u32,
};

fn matchOverloadedFunc(c: *cy.Chunk, func: *cy.Func, pre_args: []const Argument,
    end_args: []const *ast.Node, args: []*ir.Expr, match_err: *FuncMatchError, node: *ast.Node) !?MatchOverloadedFuncResult {

    if (func.type == .generic) {
        const res = (try matchGenericFuncRt(c, func.data.generic, pre_args, end_args, args, true, match_err, node)) orelse {
            return null;
        };
        return .{
            .func = res.func,
            .nargs = res.nargs,
            .ret_parent_scope_local = null,
        };
    } else {
        const res = (try matchFuncSig(c, func.sig, pre_args, end_args, args, true, match_err)) orelse {
            return null;
        };
        return .{
            .func = func,
            .nargs = args.len,
            .ret_parent_scope_local = res.ret_parent_scope_local,
        };
    }
}

pub fn ownOrManageArg(c: *cy.Chunk, arg: sema.ExprResult, node: *ast.Node) !sema.ExprResult {
    if (arg.resType == .infer_error) {
        return arg;
    }
    // Object types are managed from the caller side.
    // Other types are copied and managed from the callee side.
    if (arg.type.isObjectLike()) {
        if (arg.resType == .value_local) {
            return arg;
        } else {
            const res = try sema.semaOwn(c, arg, node);
            return sema.semaManage(c, res, node);
        }
    } else {
        return sema.semaOwn(c, arg, node);
    }
}

fn resolveRtArg(c: *cy.Chunk, arg: Argument, node: *ast.Node, opt_target: ?sema.FuncParam, overloaded: bool) !sema.ExprResult {
    if (arg.resolve_t == .rt) {
        return arg.res.rt;
    }
    switch (arg.type) {
        .standard => {
            if (opt_target) |target| {
                const target_t = target.get_type();
                var res = try c.semaExpr(node, .{
                    .target_t = target_t,
                    .reqTypeCstr = false,
                    .fit_target = true,
                    .return_infer_error = overloaded,
                });
                if (res.resType == .infer_error) {
                    return res;
                }

                if (target.sink) {
                    if (res.resType == .value_local) {
                        // move owner.
                        c.varStack.items[res.data.value_local].type = .dead;
                    }
                    res.owned = true;
                    return res;
                }
                return res;
            } else {
                const res = try c.semaExpr(node, .{
                    .return_infer_error = overloaded,
                });
                return res;
            }
        },
        .pre_resolved => {
            return arg.data.pre_resolved;
        },
        .receiver => {
            if (opt_target) |target| {
                if (target.sink) {
                    if (arg.data.receiver.type.id() == target.get_type().id()) {
                        if (arg.data.receiver.resType == .value_local) {
                            // move owner.
                            c.varStack.items[arg.data.receiver.data.value_local].type = .dead;
                        }
                        var res = arg.data.receiver;
                        res.owned = true;
                        return res;
                    }
                }
                const target_t = target.get_type();
                if (target_t.kind() == .pointer) {
                    if (target_t.cast(.pointer).ref) {
                        const child_t = target_t.cast(.pointer).child_t;
                        if (arg.data.receiver.type != target_t) {
                            // Not the exact pointer type but matches a distinct pointer type.
                            const rec_t = arg.data.receiver.type;
                            if (rec_t.kind() == .pointer) {
                                const rec_pt = rec_t.cast(.pointer);
                                if (rec_pt.ref and rec_pt.child_t == child_t) {
                                    return sema.ExprResult.init(arg.data.receiver.ir, target_t);
                                }
                            }
                        }
                    } else {
                        const child_t = target_t.cast(.pointer).child_t;
                        if (arg.data.receiver.type != target_t) {
                            const rec_t = arg.data.receiver.type;
                            if (rec_t.kind() == .pointer) {
                                const rec_pt = rec_t.cast(.pointer);
                                if (!rec_pt.ref and rec_pt.child_t == child_t) {
                                    return sema.ExprResult.init(arg.data.receiver.ir, target_t);
                                }
                            }
                        }
                    }
                } else if (target_t.kind() == .borrow) {
                    const target_child_t = target_t.cast(.borrow).child_t;
                    if (arg.data.receiver.type == target_child_t) {
                        // Implicit T -> &T
                        const rec = try sema.semaManage(c, arg.data.receiver, node);
                        const addressable = try sema.semaEnsureAddressable(c, rec, node);
                        return sema.semaAddressOf(c, addressable, target_t, node);
                    }
                    if (arg.data.receiver.type != target_t) {
                        // Not the exact pointer type but matches a distinct pointer type.
                        const rec_t = arg.data.receiver.type;
                        // Implicit ^T -> &T
                        // Implicit *T -> &T
                        if (rec_t.kind() == .pointer) {
                            const rec_pt = rec_t.cast(.pointer);
                            if (rec_pt.child_t == target_child_t) {
                                var new_rec = try sema.semaManage(c, arg.data.receiver, node);
                                new_rec.type = target_t;
                                return new_rec;
                            }
                        }
                        // NOTE: This implicit conversion is only possible if the function doesn't have a `&|T` parameter.
                        //       Multiple `&T` are allowed because they are guaranteed to not outlive the receiver `&|T`.
                        // TODO: Check that no other parameter accepts `&|T`.
                        // &>T -> &T
                        if (rec_t.kind() == .ex_borrow) {
                            const borrow = rec_t.cast(.ex_borrow);
                            if (borrow.child_t == target_child_t) {
                                return sema.ExprResult.init(arg.data.receiver.ir, target_t);
                            }
                        }
                    }
                } else if (target_t.kind() == .ex_borrow) {
                    const target_child_t = target_t.cast(.ex_borrow).child_t;
                    if (arg.data.receiver.type == target_child_t) {
                        // T -> &>T
                        const rec = try sema.semaManage(c, arg.data.receiver, node);
                        return sema.sema_ex_borrow2(c, rec, target_t, node);
                    }
                    if (arg.data.receiver.type.kind() == .pointer) {
                        const pointer = arg.data.receiver.type.cast(.pointer);

                        if (pointer.child_t == target_child_t) {
                            if (!pointer.ref) {
                                // *T -> &>T
                                var new_rec = try sema.semaManage(c, arg.data.receiver, node);
                                new_rec.type = target_t;
                                return new_rec;
                            }
                        }
                    }
                } else if (target_t.kind() == .generic_trait) {
                    if (arg.data.receiver.type.kind() == .ref_trait) {
                        if (arg.data.receiver.type.cast(.ref_trait).generic.base.id() == target_t.id()) {
                            return sema.ExprResult.init(arg.data.receiver.ir, target_t);
                        }
                    } else if (arg.data.receiver.type.kind() == .borrow_trait) {
                        if (arg.data.receiver.type.cast(.borrow_trait).generic.base.id() == target_t.id()) {
                            return sema.ExprResult.init(arg.data.receiver.ir, target_t);
                        }
                    }
                }

                return arg.data.receiver;
            } else {
                return arg.data.receiver;
            }
        },
        .skip => {
            return error.Unexpected;
        },
    }
}

fn getFuncTemplateParamType(c: *cy.Chunk, template: *cy.sym.FuncTemplate, ctx: *sema.ResolveContext, idx: usize) !*cy.Type {
    if (idx == 0 and std.mem.eql(u8, template.func_params[idx].name_type.name(), "self")) {
        return template.head.parent.?.getStaticType().?;
    } else {
        const spec_idx = sema.indexOfTypedParam(template.func_params, idx).?;
        try sema.pushSavedResolveContext(c, ctx.*);
        defer ctx.* = sema.saveResolveContext(c);

        return try cte.eval_type2(c, true, template.func_params[spec_idx].type.?);
    }
}

fn matchTemplateArgCt(c: *cy.Chunk, param: sema.FuncParam, arg: Argument, template: *cy.sym.FuncTemplate,
    template_ctx: *sema.ResolveContext, param_idx: usize, ct_return: bool) !Argument {

    _ = ct_return;

    const param_t = param.get_type();
    const ast_param = template.func_params[param_idx];

    if (ast_param.template_param) {
        // (%T type)
        var tparam_t: *cy.Type = undefined;
        {
            try sema.pushSavedResolveContext(c, template_ctx.*);
            defer template_ctx.* = sema.saveResolveContext(c);
            const type_idx = sema.indexOfTypedParam(template.func_params, param_idx).?;
            tparam_t = try cte.eval_type2(c, false, template.func_params[type_idx].type.?);
        }

        const ct_value = try cte.evalCheck(c, arg.node, tparam_t);
        var new_arg = arg;
        new_arg.resolve_t = .template;
        new_arg.res = .{ .template = ct_value };
        return new_arg;
    }
    if (param_t.id() != bt.Infer) {
        var target_t: *cy.Type = param_t;
        if (param_t.id() == bt.Dependent) {
            target_t = try getFuncTemplateParamType(c, template, template_ctx, param_idx);
        }
        var ct_value: cy.TypeValue = undefined;

        switch (arg.type) {
            .receiver => {
                if (arg.data.receiver.type.id() != target_t.id()) {
                    if (target_t.kind() == .borrow) {
                        const borrow_target_t = target_t.cast(.borrow);
                        if (arg.data.receiver.type.id() == borrow_target_t.child_t.id()) {
                            // T -> &T.
                            if (arg.data.receiver.type.is_cte_boxed()) {
                                const ref = arg.data.receiver.data.ct_value.value;
                                ct_value = cy.TypeValue.init(target_t, ref);
                                var new_arg = arg;
                                new_arg.resolve_t = .ct;
                                new_arg.res = .{ .ct = ct_value };
                                return new_arg;
                            } else {
                                return c.reportError("TODO", arg.node);
                            }
                        }
                    }
                    return c.reportError("TODO", arg.node);
                } else {
                    ct_value = arg.data.receiver.data.ct_value;
                    var new_arg = arg;
                    new_arg.resolve_t = .ct;
                    new_arg.res = .{ .ct = ct_value };
                    return new_arg;
                }
            },
            else => {
                if (target_t == c.sema.code_t) {
                    const val = cy.Value.initPtr(arg.node);
                    ct_value = cy.TypeValue.init(c.sema.code_t, val);
                } else {
                    ct_value = try cte.evalCheck(c, arg.node, target_t);
                }
                var new_arg = arg;
                new_arg.resolve_t = .ct;
                new_arg.res = .{ .ct = ct_value };
                return new_arg;
            }
        }
    }

    var ct_value: cy.TypeValue = undefined;
    if (param_t.id() == bt.Infer) {
        ct_value = try cte.eval(c, arg.node);
        const targ = cy.TypeValue.init(c.sema.type_t, Value.initPtr(ct_value.type));
        try inferCtArgs(c, targ, template, template_ctx, ast_param.type.?, arg.node);
    } else {
        return error.Unexpected;
    }

    var new_arg = arg;
    new_arg.resolve_t = .ct;
    new_arg.res = .{ .ct = ct_value };
    return new_arg;
}

fn matchTemplateArg(c: *cy.Chunk, param: sema.FuncParam, arg: Argument, template: *cy.sym.FuncTemplate,
    template_ctx: *sema.ResolveContext, param_idx: usize, comptime sema_func: bool) !Argument {

    const param_t = param.get_type();
    const ast_param = template.func_params[param_idx];

    if (ast_param.template_param) {
        var tparam_t: *cy.Type = undefined;
        {
            try sema.pushSavedResolveContext(c, template_ctx.*);
            defer template_ctx.* = sema.saveResolveContext(c);
            const type_idx = sema.indexOfTypedParam(template.func_params, param_idx).?;
            tparam_t = try cte.eval_type2(c, false, template.func_params[type_idx].type.?);
        }

        const ct_value = try cte.evalCheck(c, arg.node, tparam_t);
        var new_arg = arg;
        new_arg.resolve_t = .template;
        new_arg.res = .{ .template = ct_value };
        return new_arg;
    }

    var target: ?sema.FuncParam = null;

    if (param_t.id() != bt.Infer) {
        if (param_t.id() == bt.Dependent) {
            const target_t = try getFuncTemplateParamType(c, template, template_ctx, param_idx);
            target = sema.FuncParam.init(target_t);
        } else {
            target = param;
        }
    }
    var res = try resolveRtArg(c, arg, arg.node, target, false);
    if (!sema_func) {
        res = try ownOrManageArg(c, res, arg.node);
    }

    const generic_param = param_t.id() == bt.Infer or param_t.is_generic();
    if (generic_param) {
        if (param_t.id() == bt.Infer) {
            // Infer template params.
            const targ = cy.TypeValue.init(c.sema.type_t, Value.initPtr(res.type));
            try inferCtArgs(c, targ, template, template_ctx, ast_param.type.?, arg.node);
            try sema_type.ensure_resolved_type(c, res.type, arg.node);
            target = sema.FuncParam.init(res.type);
        }
    }

    if (param_t.is_generic()) {
        var buf: [2]u8 = undefined;
        const anonymous_name = try std.fmt.bufPrint(&buf, "{}", .{param_idx});
        const idx = template.indexOfParam(anonymous_name).?;
        try template_ctx.initCtParam2(c.alloc, template.params[idx].name, c.sema.type_t, Value.initPtr(res.type));
        if (!try sema_type.implements(c, res.type, param.get_type().cast(.generic_trait), arg.node)) {
            return sema.reportIncompatType(c, param.get_type(), res.type, arg.node);
        }
    } else {
        const ct_compat = cy.types.isTypeCompat(c.compiler, res.type, target.?.get_type());
        if (!ct_compat) {
            return sema.reportIncompatType(c, target.?.get_type(), res.type, arg.node);
        }
    }

    var resolved_arg = arg;
    resolved_arg.res = .{ .rt = res };
    resolved_arg.resolve_t = .rt;
    return resolved_arg;
}

// Owns a copy of the argument, unless its obtaining a borrow to a receiver.
fn matchPreArgCt(c: *cy.Chunk, arg: Argument, param: sema.FuncParam, req_value: bool, out_match: *bool) !cy.TypeValue {
    const param_t = param.get_type();
    var ct_value: cy.TypeValue = undefined;
    if (arg.type == .receiver) {
        if (arg.data.receiver.resType == .value) {
            if (req_value) {
                return error.TODO;
            }
            if (arg.data.receiver.type.id() == param_t.id()) {
                out_match.* = true;
                const val = cy.Value.initPtr(arg.data.receiver.ir);
                return cy.TypeValue.init(param_t, val);
            }

            if (param_t.kind() == .borrow) {
                const param_borrow_t = param_t.cast(.borrow);
                // T -> &T
                if (param_borrow_t.child_t.id() == arg.data.receiver.type.id()) {
                    const new = try sema.semaEnsureAddressable(c, arg.data.receiver, arg.node);
                    out_match.* = true;
                    const val = cy.Value.initPtr(new.ir);
                    return cy.TypeValue.init(param_t, val);
                }
            }
            out_match.* = false;
            return cy.TypeValue.init(arg.data.receiver.type, undefined);
        } else if (arg.data.receiver.resType == .ct_value) {
            const rec = arg.data.receiver.data.ct_value;

            // T -> T
            if (rec.type.id() == param_t.id()) {
                out_match.* = true;
                return c.heap.copy_value(rec);
            }

            if (param_t.kind() == .borrow) {
                const param_borrow_t = param_t.cast(.borrow);

                if (param_borrow_t.child_t.id() == rec.type.id()) {
                    // T -> &T
                    out_match.* = true;
                    if (param_borrow_t.child_t.is_cte_boxed()) {
                        return cy.TypeValue.init(param_t, rec.value);
                    } else {
                        // Find some way to have a stable primitive argument.
                        return c.reportError("TODO", arg.node);
                    }
                }

                // ^T -> &T
                // *T -> &T
                if (rec.type.kind() == .pointer) {
                    const ptr_t = rec.type.cast(.pointer);
                    if (param_borrow_t.child_t.id() == ptr_t.child_t.id()) {
                        out_match.* = true;
                        return cy.TypeValue.init(param_t, rec.value);
                    }
                }
            }

            out_match.* = false;
            // On mismatch, only the type is needed.
            return cy.TypeValue.init(arg.data.receiver.data.ct_value.type, undefined);
        } else {
            const val = cy.Value.initPtr(arg.data.receiver.ir);
            ct_value = cy.TypeValue.init(param_t, val);
        }
    } else if (arg.type == .pre_resolved) {
        switch (arg.data.pre_resolved.resType) {
            .value_field,
            .varSym,
            .value,
            .value_local => {
                if (req_value) {
                    return error.TODO;
                }
                const val = cy.Value.initPtr(arg.data.pre_resolved.ir);
                ct_value = cy.TypeValue.init(param_t, val);
            },
            .ct_value => {
                if (!req_value) {
                    return error.TODO;
                }
                if (arg.data.pre_resolved.type.id() != param_t.id()) {
                    out_match.* = false;
                    return arg.data.pre_resolved.data.ct_value;
                }
                ct_value = try c.heap.copy_value(arg.data.pre_resolved.data.ct_value);
            },
            else => {
                std.debug.panic("TODO: {}", .{arg.data.pre_resolved.resType});
            },
        }
    } else if (arg.type == .standard) {
        if (req_value) {
            const res = try cte.evalTarget(c, arg.node, param_t);
            if (param_t.id() != res.type.id()) {
                out_match.* = false;
                return cy.TypeValue.init(param_t, undefined);
            }
            ct_value = res;
        } else {
            const res = try c.semaExprTarget(arg.node, param_t);
            if (res.type.id() != param_t.id()) {
                out_match.* = false;
                return cy.TypeValue.init(param_t, undefined);
            }
            const val = cy.Value.initPtr(res.ir);
            ct_value = cy.TypeValue.init(param_t, val);
        }
    } else {
        return error.TODO;
        // ct_value = cy.TypeValue.init(c.sema.void_t, cy.Value.Void);
        // out_match.* = false;
        // return ct_value;
    }
    out_match.* = true;
    return ct_value;
}

fn matchArgCt(c: *cy.Chunk, arg: *ast.Node, param: sema.FuncParam, req_value: bool, out_match: *bool) !cy.TypeValue {
    const param_t = param.get_type();
    _ = req_value;
    var ct_value: cy.TypeValue = undefined;
    if (param_t == c.sema.code_t) {
        const val = cy.Value.initPtr(arg);
        ct_value = cy.TypeValue.init(c.sema.code_t, val);
    } else {
        ct_value = try cte.evalTarget(c, arg, param_t);
        if (ct_value.type != param_t) {
            out_match.* = false;
            return cy.TypeValue.init(ct_value.type, undefined);
        }
    }
    out_match.* = true;
    return ct_value;
}

fn matchArg(c: *cy.Chunk, arg: Argument, param: sema.FuncParam, overloaded: bool, out_match: *bool) !sema.ExprResult {
    var res = try resolveRtArg(c, arg, arg.node, param, overloaded);
    res = try ownOrManageArg(c, res, arg.node);

    const ct_compat = cy.types.isTypeCompat(c.compiler, res.type, param.get_type());
    if (!ct_compat) {
        // Check to fit
        out_match.* = false;
        return res;
    }

    out_match.* = true;
    return res;
}

pub fn reportCallFuncMismatch2(c: *cy.Chunk, pre_args: []const Argument, end_args: []const *ast.Node, func: *cy.Func, match_err: FuncMatchError, node: *ast.Node) anyerror {
    return reportCallFuncMismatch(c, pre_args, end_args, func.name(), func.sig, match_err, node);
}

pub fn reportCallFuncMismatch(c: *cy.Chunk, pre_args: []const Argument, end_args: []const *ast.Node, func_name: []const u8, sig: *cy.FuncSig, match_err: FuncMatchError, node: *ast.Node) anyerror {
    const sig_str = try c.sema.allocFuncSigStr(sig, true, c);
    defer c.alloc.free(sig_str);

    const num_call_args = pre_args.len + end_args.len;
    // if (err_arg == num_call_args) {
    //     std.debug.assert(err_arg > 0);
    //     const arg_idx = err_arg - 1;
    //     const err_node = if (arg_idx < pre_args.len) pre_args[arg_idx].node else end_args[arg_idx];
    //     return c.reportErrorFmt("Missing subsequent argument to match the function `fn {}{}`", &.{v(name), v(sig_str)}, err_node);
    // }

    if (match_err.arg == 0) {
        return c.reportErrorFmt("Expected `{}` arguments, found `{}`, when calling the function `fn {}{}`.", &.{v(sig.params_len), v(num_call_args), v(func_name), v(sig_str)}, node);
    }

    const arg_idx = match_err.arg - 1;
    const err_node = if (arg_idx < pre_args.len) pre_args[arg_idx].node else end_args[arg_idx-pre_args.len];

    const paramt_name = try c.sema.allocTypeName(match_err.param.get_type());
    defer c.alloc.free(paramt_name);
    const argt_name = try c.sema.allocTypeName(match_err.arg_t);
    defer c.alloc.free(argt_name);
    return c.reportErrorFmt("Expected argument `{}`, found `{}`, when calling `fn {}{}`.", &.{v(paramt_name), v(argt_name), v(func_name), v(sig_str)}, err_node);
}

fn expectTypeFromTemplate(c: *cy.Chunk, type_: *cy.Type, exp: *cy.sym.Template, node: *ast.Node) !*cy.Instance {
    const variant = type_.sym().instance orelse {
        const name = try c.sema.allocTypeName(type_);
        defer c.alloc.free(name);
        return c.reportErrorFmt("Expected `{}` to be an expanded type.", &.{v(name)}, node);
    };

    if (variant.type != .sym or variant.data.sym.template != exp) {
        return c.reportErrorFmt("Expected template `{}`. Found `{}`.", &.{v(exp.head.name()), v(variant.data.sym.template.head.name())}, node);
    }
    return variant;
}

/// CtInfer types are extracted from `arg_t`.
fn inferCtArgs(c: *cy.Chunk, arg: cy.TypeValue, template: *cy.sym.FuncTemplate, template_ctx: *sema.ResolveContext, template_n: *ast.Node, node: *ast.Node) !void {
    switch (template_n.type()) {
        .infer_param => {
            const name = template_n.cast(.infer_param).name.name();
            const new = try c.heap.copyValue2(arg.type, arg.value);
            try template_ctx.initCtParam(c.alloc, name, cy.TypeValue.init(arg.type, new));
        },
        .ident => {
            // Resolve `template_n` from the template's context.
            try sema.pushSavedResolveContext(c, template_ctx.*);
            defer template_ctx.* = sema.saveResolveContext(c);

            const act_v = try cte.eval(c, template_n);
            defer c.heap.destructValue(act_v);
            try assertValueEq(c, arg, act_v, template_n);
        },
        .span_type => {
            const span_type = template_n.cast(.span_type);
            if (arg.type.id() != bt.Type) {
                return c.reportErrorFmt("Expected type.", &.{}, node);
            }
            const arg_t = arg.value.asPtr(*cy.Type);
            const variant = try expectTypeFromTemplate(c, arg_t, c.sema.span_tmpl, node);
            try inferCtArgs(c, variant.getParamAt(0), template, template_ctx, span_type.child, node);
        },
        .slice_type => {
            const slice_type = template_n.cast(.slice_type);
            const arg_t = arg.value.asPtr(*cy.Type);
            const variant = try expectTypeFromTemplate(c, arg_t, c.sema.slice_tmpl, node);
            try inferCtArgs(c, variant.getParamAt(0), template, template_ctx, slice_type.child, node);
        },
        .vector_type => {
            const vector_t = template_n.cast(.vector_type);
            const arg_t = arg.value.asPtr(*cy.Type);
            const variant = try expectTypeFromTemplate(c, arg_t, c.sema.vector_tmpl, node);
            try inferCtArgs(c, variant.getParamAt(0), template, template_ctx, vector_t.child, node);
            try inferCtArgs(c, variant.getParamAt(1), template, template_ctx, vector_t.n, node);
        },
        .partial_vector_type => {
            const vector_t = template_n.cast(.partial_vector_type);
            const arg_t = arg.value.asPtr(*cy.Type);
            const variant = try expectTypeFromTemplate(c, arg_t, c.sema.partial_vector_tmpl, node);
            try inferCtArgs(c, variant.getParamAt(0), template, template_ctx, vector_t.child, node);
            try inferCtArgs(c, variant.getParamAt(1), template, template_ctx, vector_t.n, node);
        },
        .ref => {
            const ref = template_n.cast(.ref);
            const arg_t = arg.value.asPtr(*cy.Type);
            const variant = try expectTypeFromTemplate(c, arg_t, c.sema.ref_tmpl, node);
            try inferCtArgs(c, variant.getParamAt(0), template, template_ctx, ref.child, node);
        },
        .borrow => {
            const borrow = template_n.cast(.borrow);
            const arg_t = arg.value.asPtr(*cy.Type);
            const variant = try expectTypeFromTemplate(c, arg_t, c.sema.borrow_tmpl, node);
            try inferCtArgs(c, variant.getParamAt(0), template, template_ctx, borrow.child, node);
        },
        .index_expr => {
            // Infer nested. (e.g. `A[%T]`)
            const index_expr = template_n.cast(.index_expr);
            const template_t = try cte.evalSym(c, index_expr.left);
            if (template_t.type != .template) {
                return c.reportErrorFmt("Expected template. Found `{}`.", &.{v(template_t.type)}, index_expr.left);
            }

            if (arg.type.id() == bt.Type) {
                const arg_t = arg.value.asPtr(*cy.Type);

                const variant = arg_t.sym().instance orelse {
                    const name = try c.sema.allocTypeName(arg_t);
                    defer c.alloc.free(name);
                    return c.reportErrorFmt("Expected `{}` to be an expanded type.", &.{v(name)}, node);
                };

                if (variant.type != .sym or variant.data.sym.template != template_t.cast(.template)) {
                    return c.reportErrorFmt("Expected template `{}`. Found `{}`.", &.{v(template_t.name()), v(variant.data.sym.template.head.name())}, node);
                }
                if (variant.params.len != index_expr.args.len) {
                    return c.reportErrorFmt("Expected `{}` args. Found `{}`.", &.{v(variant.params.len), v(index_expr.args.len)}, template_n);
                }
                for (0..variant.params.len) |i| {
                    try inferCtArgs(c, variant.getParamAt(i), template, template_ctx, index_expr.args.ptr[i], node);
                }
            } else {
               return c.reportErrorFmt("TODO: {}.", &.{v(arg.type.name())}, node);
            }
        },
        else => {
            return c.reportErrorFmt("Unsupported infer node `{}`.", &.{v(template_n.type())}, template_n);
        }
    }
}

fn assertValueEq(c: *cy.Chunk, exp: cy.TypeValue, act: cy.TypeValue, node: *ast.Node) !void {
    if (exp.type.id() != act.type.id()) {
        return sema.reportIncompatType(c, exp.type, act.type, node);
    }

    switch (exp.type.id()) {
        bt.Type => {
            const act_t = act.value.asPtr(*cy.Type);
            if (exp.value.asPtr(*cy.Type).id() != act_t.id()) {
                return sema.reportIncompatType(c, exp.value.asPtr(*cy.Type), act_t, node);
            }
        },
        else => {
            const type_name = try c.sema.allocTypeName(exp.value.asPtr(*cy.Type));
            defer c.alloc.free(type_name);
            return c.reportErrorFmt("Unsupport value type: `{s}`", &.{v(type_name)}, node);
        }
    }
}
