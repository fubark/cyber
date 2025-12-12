const std = @import("std");
const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypes;
const sema = cy.sema;
const sema_type = cy.sema_type;
const sema_func = cy.sema_func;
const v = cy.fmt.v;
const log = cy.log.scoped(.cte);
const ast = cy.ast;
const ir = cy.ir;
const C = @import("capi.zig");
const bcgen = @import("bc_gen.zig");
const Value = cy.Value;
const TypeValue = cy.TypeValue;

const cte = @This();

const EvalStmtResult = struct {
    ret: bool,
    ret_value: Value,
};

pub fn evalFunc(c: *cy.Chunk, func: *cy.Func, args: []const Value, node: *ast.Node) !Value {
    try sema.pushFuncResolveContext(c, func, node);
    const ctx = sema.getResolveContext(c);
    ctx.has_ct_params = true;
    ctx.is_inline_eval = true;
    const start = push_ct_block(c);
    defer {
        pop_ct_block(c, start);
        sema.popResolveContext(c);
    }

    // Push param arguments.
    const params = func.sig.params();
    const param_nodes = func.decl.?.cast(.funcDecl).params.slice();
    var param_node_idx: usize = 0;
    for (args, 0..) |arg, i| {
        while (param_nodes[param_node_idx].template_param) {
            param_node_idx += 1;
        }
        const param_node = param_nodes[param_node_idx];
        param_node_idx += 1;
        const name = param_node.name_type.name();
        try ctx.setCtParam(c.heap, name, params[i].get_type(), arg);
        try ctx.ct_locals.append(c.alloc, name);
    }

    const stmts = func.decl.?.cast(.funcDecl).stmts.slice();
    return evalFuncBody(c, stmts, func.sig.ret, @ptrCast(func.decl.?));
}

pub fn evalFuncBody(c: *cy.Chunk, stmts: []const *ast.Node, ret: *cy.Type, node: *ast.Node) !Value {
    const eval_ctx = EvalContext{ .ret = ret };
    const res = try evalStmts(c, eval_ctx, stmts);
    if (res.ret) {
        return res.ret_value;
    }
    if (ret == c.sema.void_t) {
        return Value.Void;
    } else {
        return c.reportErrorFmt("Inline eval: Expected return.", &.{}, node);
    }
}

fn evalStmts(c: *cy.Chunk, ctx: EvalContext, stmts: []const *ast.Node) !EvalStmtResult {
    for (stmts) |stmt| {
        const res = try evalStmt(c, ctx, stmt);
        if (res.ret) {
            return res;
        }
    }
    return .{
        .ret = false,
        .ret_value = undefined,
    };
}

const EvalContext = struct {
    ret: *cy.Type,
};

pub fn localDecl(c: *cy.Chunk, node: *ast.VarDecl) !void {
    const name = node.name.cast(.ident).name.slice();
    const init = try cte.eval(c, node.right);
    try localDecl2(c, name, init.type, init.value);
}

pub fn localDecl2(c: *cy.Chunk, name: []const u8, val_t: *cy.Type, val: Value) !void {
    const ctx = sema.getResolveContext(c);
    try ctx.initCtVar2(c.alloc, name, val_t, val);
    try ctx.ct_locals.append(c.alloc, name);
}

pub fn switchStmt(c: *cy.Chunk, switch_stmt: *ast.SwitchBlock, req_ct_case: bool, cb: EvalCaseFn, data: ?*anyopaque, opt_target: ?*cy.Type) !void {
    const control = try cte.eval(c, switch_stmt.expr);
    defer c.heap.destructValue(control);

    if (control.type.kind() == .choice) {
        const choice_t = control.type.cast(.choice);
        const tag = control.value.asPtr(*cy.heap.Object).getValue(0).asInt();

        for (switch_stmt.cases.slice()) |ct_case| {
            var case: *ast.CaseStmt = undefined;
            if (req_ct_case) {
                if (ct_case.type() != .ct_stmt) {
                    return c.reportErrorFmt("Expected compile-time `case` statement.", &.{}, ct_case);
                }
                case = ct_case.cast(.ct_stmt).child.cast(.case_stmt);
            } else {
                if (ct_case.type() == .case_stmt) {
                    case = ct_case.cast(.case_stmt);
                } else {
                    const ct_stmt = ct_case.cast(.ct_stmt);
                    if (ct_stmt.child.type() == .for_iter_stmt) {
                        return c.reportError("TODO", @ptrCast(switch_stmt));
                    }
                    case = ct_case.cast(.ct_stmt).child.cast(.case_stmt);
                }
            }

            switch (case.kind) {
                .else_ =>{
                    const local_start = push_ct_block(c);

                    try cb(c, case, data, opt_target);

                    pop_ct_block(c, local_start);
                    return;
                },
                .case => {
                    for (case.data.case.conds.slice()) |cond| {
                        if (cond.type() != .dot_lit) {
                            const targetTypeName = choice_t.base.name();
                            return c.reportErrorFmt("Expected enum literal to match `{}`", &.{v(targetTypeName)}, @ptrCast(cond));
                        }
                        const case_name = cond.cast(.dot_lit).as_dot_infer_name();
                        const choice_case = choice_t.getCase(case_name) orelse {
                            return c.reportErrorFmt("Expected valid case.", &.{}, @ptrCast(cond));
                        };
                        if (tag == choice_case.val) {
                            const local_start = push_ct_block(c);

                            if (case.data.case.capture) |capture| {
                                if (capture.type() != .ident) {
                                    return c.reportErrorFmt("Expected variable identifier.", &.{}, @ptrCast(capture));
                                }
                                const capturev = try c.heap.unwrapChoice(control.type, control.value, case_name);
                                try localDecl2(c, capture.name(), choice_case.payload_t, capturev);
                            }

                            try cb(c, case, data, opt_target);

                            pop_ct_block(c, local_start);
                            return;
                        }
                    }
                },
            }
        }
    } else {
        for (switch_stmt.cases.slice(), 0..) |case_n, i| {
            if (case_n.type() == .ct_stmt) {
                const ct_stmt = case_n.cast(.ct_stmt);
                if (ct_stmt.child.type() == .for_iter_stmt) {
                    const for_stmt = ct_stmt.child.cast(.for_iter_stmt);
                    if (for_stmt.stmts.len != 1) {
                        return c.reportError("Expected 1 case statement.", case_n);
                    }
                    if (for_stmt.stmts.ptr[0].type() != .case_stmt) {
                        return c.reportError("Expected 1 case statement.", case_n);
                    }
                    const for_case = for_stmt.stmts.ptr[0].cast(.case_stmt);

                    const iter_n = for_stmt.iterable;
                    const iterable = try cte.eval(c, iter_n);
                    defer c.heap.destructValue(iterable);

                    // Obtain iterator.
                    const iterator_sym = try c.accessResolvedSymOrFail(iterable.type, "iterator", iter_n);
                    var func_sym = try sema.requireFuncSym(c, iterator_sym, iter_n);
                    var iter = try cte.callFuncSymRec(c, func_sym, iter_n, sema.ExprResult.initCtValue(iterable), &.{}, iter_n);
                    defer c.heap.destructValue(iter);

                    // while iter.next()
                    const next_sym = try c.accessResolvedSymOrFail(iterable.type, "next", iter_n);
                    func_sym = try sema.requireFuncSym(c, next_sym, iter_n);
                    const borrow_t = try sema.getBorrowType(c, iter.type);
                    var iter_borrow: TypeValue = undefined;
                    if (iter.type.is_cte_boxed()) {
                        iter_borrow = TypeValue.init(borrow_t, iter.value);
                    } else {
                        iter_borrow = TypeValue.init(borrow_t, Value.initPtr(&iter.value));
                    }
                    const args = [_]sema_func.Argument{
                        sema_func.Argument.initReceiver(iter_n, sema.ExprResult.initCtValue(iterable)),
                        sema_func.Argument.initPreResolved(iter_n, sema.ExprResult.initCtValue(iter_borrow)),
                    };
                    while (true) {
                        const next = try cte.callFuncSym(c, func_sym, &args, &.{}, iter_n);
                        defer c.heap.destructValue(next);

                        const value = try c.heap.unwrap_option_or(next.type.cast(.option), next.value) orelse {
                            break;
                        };
                        const value_t = next.type.cast(.option).child_t;

                        // NOTE: Only one variable before case block so avoid pushing another ct block.
                        if (for_stmt.each) |each| {
                            const cx = sema.getResolveContext(c);
                            try cx.initCtVar2(c.alloc, each.name(), value_t, value);
                        }
                        defer {
                            if (for_stmt.each) |each| {
                                const cx = sema.getResolveContext(c);
                                cx.unsetCtParam(c.heap, each.name());
                            }
                        }

                        const cond = for_case.data.case.conds.ptr[0];
                        const cond_v = try evalCheck(c, cond, control.type);
                        defer c.heap.destructValue(cond_v);

                        if (try evalCompare(c, control.type, control.value, cond_v.value, cond)) {
                            try evalCase(c, for_case, null, cb, data, opt_target);
                            return;
                        }
                    }
                    continue;
                }
            }
            var case = try getCaseStmt(c, case_n, req_ct_case);
            switch (case.kind) {
                .else_ =>{
                    const local_start = push_ct_block(c);

                    try cb(c, case, data, opt_target);

                    pop_ct_block(c, local_start);
                    return;
                },
                .case => {
                    for (case.data.case.conds.slice()) |cond| {
                        var exp: cy.Value = undefined;
                        if (control.type.kind() == .enum_t) {
                            const enum_t = control.type.cast(.enum_t);
                            if (cond.type() != .dot_lit) {
                                const targetTypeName = enum_t.base.name();
                                return c.reportErrorFmt("Expected enum literal to match `{}`", &.{v(targetTypeName)}, @ptrCast(cond));
                            }

                            const case_name = cond.cast(.dot_lit).as_dot_infer_name();
                            const enum_case = enum_t.getCase(case_name) orelse {
                                return c.reportErrorFmt("Expected valid case.", &.{}, @ptrCast(cond));
                            };
                            exp = Value.initInt(@intCast(enum_case.val));
                        } else {
                            const condv = try evalCheck(c, cond, control.type);
                            exp = condv.value;
                        }
                        defer c.heap.destructValue2(control.type, exp);
                        if (try evalCompare(c, control.type, exp, control.value, cond)) {
                            if (case.body_kind == .fallthrough) {
                                for (switch_stmt.cases.slice()[i+1..]) |stmt| {
                                    case = try getCaseStmt(c, stmt, req_ct_case);
                                    if (case.body_kind == .fallthrough) {
                                        continue;
                                    }
                                    if (case.kind == .case) {
                                        try evalCase(c, case, null, cb, data, opt_target);
                                        return;
                                    } else {
                                        return error.TODO;
                                    }
                                    return;
                                }
                            }
                            try evalCase(c, case, null, cb, data, opt_target);
                            return;
                        }
                    }
                },
            }
        }
    }
}

const EvalCaseFn = *const fn(*cy.Chunk, *ast.CaseStmt, ?*anyopaque, ?*cy.Type) anyerror!void;

fn evalCase(c: *cy.Chunk, case: *ast.CaseStmt, captured_opt: ?cy.TypeValue, cb: EvalCaseFn, data: ?*anyopaque, opt_target: ?*cy.Type) !void {
    const local_start = push_ct_block(c);

    if (captured_opt) |captured| {
        const capture_n = case.data.case.capture.?;
        try localDecl2(c, capture_n.name(), captured.type, captured.value);
    }

    try cb(c, case, data, opt_target);

    pop_ct_block(c, local_start);
}

fn getCaseStmt(c: *cy.Chunk, stmt: *ast.Node, req_ct_case: bool) !*ast.CaseStmt { 
    if (req_ct_case) {
        if (stmt.type() != .ct_stmt) {
            return c.reportErrorFmt("Expected compile-time `case` statement.", &.{}, stmt);
        }
        return stmt.cast(.ct_stmt).child.cast(.case_stmt);
    } else {
        if (stmt.type() == .case_stmt) {
            return stmt.cast(.case_stmt);
        } else {
            return stmt.cast(.ct_stmt).child.cast(.case_stmt);
        }
    }
}

pub fn forRangeStmt(c: *cy.Chunk, node: *ast.ForRangeStmt, cb: *const fn(*cy.Chunk, *ast.ForRangeStmt, ?*anyopaque) anyerror!void, data: ?*anyopaque) !void {
    const start = try cte.evalTarget(c, node.start, c.sema.i64_t);
    if (start.type.kind() != .int) {
        return c.reportErrorFmt("Expected integer.", &.{}, node.start);
    }
    const end = try cte.evalCheck(c, node.end, start.type);
    var i = start.value.asInt();
    var ctx = sema.getResolveContext(c);
    var each_name: ?[]const u8 = null;
    if (node.each) |each| {
        each_name = each.name();
        if (ctx.hasCtParam(each_name.?)) {
            return c.reportErrorFmt("Redeclaration of compile-time variable `{}`.", &.{v(each_name.?)}, each);
        }
    }

    while (i < end.value.asInt()) {
        if (each_name) |name| {
            ctx = sema.getResolveContext(c);
            try ctx.setCtParam(c.heap, name, start.type, Value.initInt(i));
        }

        const local_start = push_ct_block(c);

        try cb(c, node, data);

        pop_ct_block(c, local_start);

        i += 1;
    }
}

pub fn push_ct_block(c: *cy.Chunk) usize {
    const cx = sema.getResolveContext(c);
    return cx.pushCtBlock();
}

pub fn pop_ct_block(c: *cy.Chunk, local_start: usize) void {
    const cx = sema.getResolveContext(c);
    cx.popCtBlock(c, local_start);
}

fn forRangeInner(c: *cy.Chunk, for_range: *ast.ForRangeStmt, data: ?*anyopaque) anyerror!void {
    const ctx: *EvalContextAndResult = @ptrCast(@alignCast(data));
    ctx.res = try evalStmts(c, ctx.ctx, for_range.stmts);
}

fn switchCaseInner(c: *cy.Chunk, case_stmt: *ast.CaseStmt, data: ?*anyopaque, opt_target: ?*cy.Type) anyerror!void {
    _ = opt_target;
    const ctx: *EvalContextAndResult = @ptrCast(@alignCast(data));
    if (case_stmt.body_kind != .block) {
        return error.Unsupported;
    }
    ctx.res = try evalStmts(c, ctx.ctx, case_stmt.body_data.block.slice());
}

fn switchExprCaseInner(c: *cy.Chunk, case_stmt: *ast.CaseStmt, data: ?*anyopaque, opt_target: ?*cy.Type) anyerror!void {
    const ctx: *EvalContextAndResult = @ptrCast(@alignCast(data));
    if (case_stmt.body_kind != .expr) {
        return error.Unsupported;
    }
    var res: cy.TypeValue = undefined;
    if (opt_target) |target_t| {
        res = try evalTarget(c, case_stmt.body_data.expr, target_t);
    } else {
        res = try eval(c, case_stmt.body_data.expr);
    }
    ctx.res.ret = true;
    ctx.res.ret_value = res.value;
    ctx.ctx.ret = res.type;
}

const EvalContextAndResult = struct {
    ctx: EvalContext,
    res: EvalStmtResult,
};

const RecOp = struct {
    node: *ast.Node,
    left: *ast.Node,
    op: ast.BinaryExprOp,
};

fn eval_assign_right(c: *cy.Chunk, right: *ast.Node, rec_op_opt: ?RecOp) !TypeValue {
    if (rec_op_opt) |rec_op| {
        return eval_bin_expr(c, rec_op.left, rec_op.op, right, rec_op.node);
    } else {
        return eval(c, right);
    }
}

fn eval_assign(c: *cy.Chunk, left_n: *ast.Node, right_n: *ast.Node, op_opt: ?ast.BinaryExprOp, node: *ast.Node) !void {
    const rec_op: ?RecOp = if (op_opt) |op| .{ .left = left_n, .op = op, .node = node } else null;
    switch (left_n.type()) {
        .ident => {
            const name = left_n.name();
            const left = try sema.lookupStaticIdent(c, name, left_n) orelse {
                return c.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, left_n);
            };
            switch (left) {
                .capture,
                .local => {
                    @panic("Unexpected.");
                },
                .ct_var => |ct_var| {
                    const var_t = ct_var.getType();
                    const right = try evalCheck(c, right_n, var_t);
                    c.heap.destructValue2(var_t, ct_var.value);
                    ct_var.* = sema.CtParam.initVar(right);
                },
                else => {
                    return c.reportErrorFmt("Unsupported {}", &.{v(@tagName(left))}, left_n);
                },
            }
        },
        .deref => {
            const deref = left_n.cast(.deref);
            const rec = try eval(c, deref.left);
            defer c.heap.destructValue(rec);
            const right = try eval_assign_right(c, right_n, rec_op);

            const child_t = rec.type.getRefLikeChild() orelse {
                return c.reportError("Expected reference type.", left_n);
            };

            const dst = rec.value.asBytes();
            c.heap.destructValueAt(child_t, dst);
            c.heap.moveValueTo(child_t, dst, right.value);
        },
        .index_expr => {
            const index_expr = left_n.cast(.index_expr);

            const rec = try evalExprInfer(c, index_expr.left);
            defer deinitExpr(c, rec);

            switch (rec.kind) {
                .ct_var => {
                    const rec_t = rec.data.ct_var.getType();
                    if (rec_t.kind() == .vector) {
                        const vector_t = rec_t.cast(.vector);
                        const idx_val = try evalCheck(c, index_expr.args.ptr[0], c.sema.i64_t);
                        const idx: usize = @intCast(idx_val.value.asInt());

                        const right = try evalCheck(c, right_n, vector_t.elem_t);

                        const dst = rec.data.ct_var.value.asPtr([*]u8) + idx*vector_t.elem_t.size();
                        c.heap.destructValueAt(vector_t.elem_t, dst);
                        c.heap.moveValueTo(vector_t.elem_t, dst, right.value);
                    } else {
                        return c.reportErrorFmt("Unsupported {}", &.{v(rec_t.name())}, index_expr.left);
                    }
                },
                else => {
                    return c.reportErrorFmt("Unsupported {}", &.{v(rec.kind)}, index_expr.left);
                }
            }
        },
        else => {
            return c.reportErrorFmt("Unsupported {}.", &.{v(left_n.type())}, left_n);
        },
    }
}

fn evalStmt(c: *cy.Chunk, ctx: EvalContext, stmt: *ast.Node) !EvalStmtResult {
    log.tracev("evalStmt {}", .{stmt.type()});
    switch (stmt.type()) {
        .op_assign_stmt => {
            const assign_stmt = stmt.cast(.op_assign_stmt);
            try eval_assign(c, assign_stmt.left, assign_stmt.right, assign_stmt.op, stmt);
        },
        .assign_stmt => {
            const assign_stmt = stmt.cast(.assign_stmt);
            try eval_assign(c, assign_stmt.left, assign_stmt.right, null, stmt);
        },
        .var_decl => {
            try cte.localDecl(c, stmt.cast(.var_decl));
        },
        .for_range_stmt => {
            var mut_ctx = EvalContextAndResult{ .ctx = ctx, .res = undefined };
            try forRangeStmt(c, stmt.cast(.for_range_stmt), forRangeInner, &mut_ctx);
            return mut_ctx.res;
        },
        .switch_stmt => {
            var mut_ctx = EvalContextAndResult{ .ctx = ctx, .res = undefined };
            try switchStmt(c, stmt.cast(.switch_stmt), false, switchCaseInner, &mut_ctx, null);
            return mut_ctx.res;
        },
        .if_stmt => {
            const if_stmt = stmt.cast(.if_stmt);
            var cond_res = try cte.evalCheck(c, if_stmt.cond, c.sema.bool_t);
            if (cond_res.value.asBool()) {
                // const start = try sema.getResolveContext(c).pushCtBlock(c, @ptrCast(if_stmt));
                return evalStmts(c, ctx, if_stmt.stmts);
                // try sema.getResolveContext(c).popCtBlock(c, start);
            } else {
                for (if_stmt.else_blocks) |block_| {
                    var block = block_;
                    if (block.type() == .ct_stmt) {
                        block = block.cast(.ct_stmt).child;
                    }
                    if (block.type() == .elseif_block) {
                        const elseif_block = block.cast(.elseif_block);
                        cond_res = try cte.evalCheck(c, elseif_block.cond, c.sema.bool_t);
                        if (cond_res.value.asBool()) {
                            return evalStmts(c, ctx, elseif_block.stmts);
                        }
                    } else if (block.type() == .else_block) {
                        const else_block = block.cast(.else_block);
                        return evalStmts(c, ctx, else_block.stmts);
                    } else {
                        return error.Unsupported;
                    }
                }
            }
        },
        .returnExprStmt => {
            const return_stmt = stmt.cast(.returnExprStmt);
            const res = try evalCheck(c, return_stmt.child, ctx.ret);
            return .{
                .ret = true,
                .ret_value = res.value,
            };
        },
        .exprStmt => {
            const expr_stmt = stmt.cast(.exprStmt);
            const res = try eval(c, expr_stmt.child);
            defer c.heap.destructValue(res);
        },
        .ct_stmt => {
            const ct_stmt = stmt.cast(.ct_stmt);
            return evalStmt(c, ctx, ct_stmt.child);
        },
        else => {
            return c.reportErrorFmt("Inline eval: Unsupported {}.", &.{v(stmt.type())}, stmt);
        }
    }
    return .{
        .ret = false,
        .ret_value = undefined,
    };
}

pub fn eval(c: *cy.Chunk, node: *ast.Node) anyerror!TypeValue {
    const res = try evalExpr(c, node, ExprCstr.initInfer());
    return exprToValue(c, res, node);
}

pub fn evalTarget(c: *cy.Chunk, node: *ast.Node, target_t: ?*cy.Type) anyerror!TypeValue {
    const res = try evalExpr(c, node, ExprCstr.initTarget(target_t));
    return exprToValue(c, res, node);
}

pub fn evalHint(c: *cy.Chunk, node: *ast.Node, target_t: *cy.Type) anyerror!TypeValue {
    const res = try evalExpr(c, node, ExprCstr.initHint(target_t));
    return exprToValue(c, res, node);
}

pub fn evalCheck(c: *cy.Chunk, node: *ast.Node, target_t: *cy.Type) anyerror!TypeValue {
    const res = try evalExpr(c, node, ExprCstr.init_check(target_t));
    return exprToValue(c, res, node);
}

pub fn eval_check_template(c: *cy.Chunk, node: *ast.Node, template: *cy.sym.Template) anyerror!TypeValue {
    const expr = try evalExpr(c, node, ExprCstr.initInfer());
    const value = try exprToValue(c, expr, node);
    if (!value.type.isInstanceOf(template)) {
        const type_name = try c.sema.allocTypeName(value.type);
        defer c.alloc.free(type_name);
        return c.reportErrorFmt("Expected type instance of `{}`, found `{}`.", &.{v(template.head.name()), v(type_name)}, node);
    }
    return value;
}

pub fn evalCompare(c: *cy.Chunk, type_: *cy.Type, a: Value, b: Value, node: *ast.Node) !bool { 
    switch (type_.id()) {
        bt.EvalStr => {
            return std.mem.eql(u8, a.as_eval_str(), b.as_eval_str());
        },
        bt.Type => {
            const a_t = a.asPtr(*cy.Type);
            const b_t = b.asPtr(*cy.Type);
            return a_t == b_t;
        },
        bt.EvalInt,
        bt.I64 => {
            return a.val == b.val;
        },
        bt.I32 => {
            return a.u32 == b.u32;
        },
        else => {
            switch (type_.kind()) {
                .enum_t => {
                    return a.val == b.val;
                },
                else => {
                    const name = try c.sema.allocTypeName(type_);
                    defer c.alloc.free(name);
                    return c.reportErrorFmt("Unsupported {}.", &.{v(name)}, node);
                },
            }
        },
    }
}

fn evalCallExpr(c: *cy.Chunk, call: *ast.CallExpr) !Expr {
    const node: *ast.Node = @ptrCast(call);
    if (call.callee.type() == .accessExpr) {
        const callee = call.callee.cast(.accessExpr);

        const left = try evalExprInfer(c, callee.left);
        defer deinitExpr(c, left);

        switch (left.kind) {
            .sym => {
                const sym = left.data.sym;
                if (sym.type == .func) {
                    return c.reportErrorFmt("Can not access function symbol `{}`.", &.{v(sym.name())}, callee.right);
                }
                if (sym.isValue()) {
                    return error.Unexpected;
                }

                // Look for sym under left module.
                const right_name = callee.right.name();
                const right_sym = try c.getResolvedSymOrFail(sym, right_name, callee.right);

                const res = try sema.callSym(c, right_sym, callee.right, call.args.slice(), true, node);
                return Expr.initValue(res.data.ct_value);
            },
            .value => {
                const left_value = left.data.value;
                // Look for sym under left type's module.
                const right_name = callee.right.name();
                const right_sym = try c.accessResolvedSymOrFail(left_value.type, right_name, callee.right);

                if (right_sym.type == .func) {
                    const res = try c.semaCallFuncSymRec(right_sym.cast(.func), callee.left, sema.ExprResult.initCtValue(left_value), call.args.slice(), true, node);
                    return Expr.initValue(res.data.ct_value);
                // } else if (rightSym.type == .func_template) {
                //     const template = rightSym.cast(.func_template);
                //     if (template.callable) {
                //         return c.semaCallGenericFuncRec(template, callee.left, leftRes, call.args, node);
                //     } else {
                //         return c.reportErrorFmt("Function template must be expanded first.", &.{v(right_name)}, callee.left);
                //     }
                // } else {
                //     const callee_v = try c.semaExpr(call.callee, .{});
                //     return c.semaCallValue(callee_v, call.callee, call.args, callee.right);
                } else {
                    return c.reportErrorFmt("Unsupported {}", &.{v(right_sym.type)}, node);
                }
            },
            else => {
                return c.reportErrorFmt("Unsupported {}", &.{v(left.kind)}, node);
            },
        }
    } else if (call.callee.type() == .ident or call.callee.type() == .at_lit) {

        const name = call.callee.name();

        const callee = try sema.lookupStaticIdent(c, name, node) orelse {
            return c.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, node);
        };
        switch (callee) {
            .capture,
            .local => {
                @panic("Unexpected.");
            },
            .static => |sym| {
                const res = try sema.callSym(c, sym, call.callee, call.args.slice(), true, node);
                return Expr.initValue(res.data.ct_value);
            },
            // .ct_value => |ct_value| {
            //     defer c.heap.release(ct_value.value);
            //     if (ct_value.type.id() == bt.Type) {
            //         const type_ = ct_value.value.asPtr(*cy.Type);
            //         const sym = type_.sym();
            //         return callSym(c, @ptrCast(sym), call.callee, call.args, req_value, node);
            //     } else {
            //         if (ct_value.type.kind() == .func_sym) {
            //             const func = ct_value.value.asPtr(*cy.Func);
            //             return c.semaCallFunc(func, call.args, node);
            //         } else {
            //             return error.TODO;
            //         }
            //     }
            // },
            // .param_value => |param_value| {
            //     defer cy.TypeParamValue.deinit(c.heap, param_value);
            //     switch (param_value.type.id()) {
            //         else => {
            //             if (param_value.type.kind() == .func_sym) {
            //                 return c.semaCallFunc(param_value.value.fnsym, call.args, node);
            //             } else if (param_value.type.id() == bt.Type) {
            //                 return callSym(c, &param_value.value.type.sym().head, call.callee, call.args, req_value, node);
            //             }
            //             std.debug.panic("TODO: {s}", .{param_value.type.name()});
            //         }
            //     }
            //     return error.TODO;
            // },
            else => {
                return c.reportErrorFmt("Unsupported {}", &.{v(@tagName(callee))}, node);
            },
        }
    } else {
        const callee = try evalExprInfer(c, call.callee);
        defer deinitExpr(c, callee);
        switch (callee.kind) {
            .sym => {
                const sym = callee.data.sym;
                if (sym.type == .func) {
                    return c.reportErrorFmt("Expected function `{}`.", &.{v(sym.name())}, call.callee);
                }
                if (sym.isValue()) {
                    return error.Unexpected;
                }
                const res = try sema.callSym(c, sym, call.callee, call.args.slice(), true, node);
                return Expr.initValue(res.data.ct_value);
            },
            .func => {
                // return c.semaCallFunc(calleeRes.data.func, call.args, @ptrCast(node));
            },
            .value => {
                // return c.semaCallValue(calleeRes, call.callee, call.args, node);
            },
            else => {
            },
        }
        return c.reportErrorFmt("Unsupported {}", &.{v(call.callee.type())}, node);
    }

    // const res = try c.semaCallExpr(true, node);
    // if (res.resType != .ct_value) {
    //     return error.Unexpected;
    // }
    // return Expr.initValue(res.data.ct_value);
}

pub const CallCstr = struct {
    req_value: bool,
};

pub fn evalCall(c: *cy.Chunk, func: *cy.Func, args: []const Value, node: *ast.Node) !sema.ExprResult {
    log.tracev("evalCall: {s}", .{func.name()});
    switch (func.type) {
        .host_const_eval => {
            const ctx = cy.ConstEvalContext{
                .func = func, 
                .args = args.ptr,
                .node = node,
            };

            const res = func.data.host_const_eval.ptr(c, &ctx);
            if (res.type.id() == bt.Null) {
                return error.CompileError;
            }
            try sema_type.ensure_eval_eligible_type(c, res.type, node);
            return sema.ExprResult.initCtValue(res);
        },
        .host_builtin => {
            @panic("unexpected");
        },
        .hostFunc => {
            if (func.data.hostFunc.eval) |eval_ptr| {
                const ctx = cy.ConstEvalContext{
                    .func = func, 
                    .args = args.ptr,
                    .node = node,
                };
                const res = eval_ptr(c, &ctx);
                if (res.type.id() == bt.Null) {
                    return error.CompileError;
                }
                return sema.ExprResult.initCtValue(res);
            }
        },
        .userFunc => {
            if (func.data.userFunc.eval) |eval_ptr| {
                const ctx = cy.ConstEvalContext{
                    .func = func, 
                    .args = args.ptr,
                    .node = node,
                };
                const res = eval_ptr(c, &ctx);
                if (res.type.id() == bt.Null) {
                    return error.CompileError;
                }
                return sema.ExprResult.initCtValue(res);
            }
            const val = try evalFunc(c, func, args, node);
            return sema.ExprResult.initCtValue2(func.sig.ret, val);
        },
        else => {},
    }
    return c.reportErrorFmt("Inline eval: Unsupported function: `{}`", &.{v(func.type)}, node);
}

pub const ExprCstr = struct {
    type: ?*cy.Type,

    /// For templates params, the target is `c.sema.type` and `type` contains a generic type constraint.
    match_generic_type: bool = false,

    check: bool,
    fit_target: bool,

    /// Declaration sites do not need a fully resolved type.
    /// Const eval and IR generation do.
    /// IR generation can assume all declared types have been resolved already.
    /// This only affects newly created types (instance types).
    resolve_new_type: bool = true,

    fn initTarget(type_: ?*cy.Type) ExprCstr {
        return .{
            .check = false,
            .type = type_,
            .fit_target = true,
        };
    }

    fn initHint(type_: *cy.Type) ExprCstr {
        return .{
            .check = false,
            .type = type_,
            .fit_target = false,
        };
    }

    pub fn init_check_generic(type_: *cy.Type) ExprCstr {
        return .{
            .check = true,
            .match_generic_type = true,
            .type = type_,
            .fit_target = true,
        };
    }

    pub fn init_check(type_: *cy.Type) ExprCstr {
        return .{
            .check = true,
            .type = type_,
            .fit_target = true,
        };
    }

    fn initInfer() ExprCstr {
        return .{
            .check = false,
            .type = null,
            .fit_target = false,
        };
    }
};

pub fn exprToValue(c: *cy.Chunk, expr: Expr, node: *ast.Node) !TypeValue {
    switch (expr.kind) {
        .ct_var => {
            const val_t = expr.data.ct_var.getType();
            const val = expr.data.ct_var.value;
            const deref = try c.heap.copyValue2(val_t, val);
            return TypeValue.init(val_t, deref);
        },
        .value => return expr.data.value,
        .func => {
            const func = expr.data.func;
            const func_t = try cy.sema.getFuncSymType(c, func.sig);
            return TypeValue{
                .type = func_t,
                .value = Value.initPtr(func),
            };
        },
        .sym => {
            const sym = expr.data.sym;
            switch (sym.type) {
                .userVar => {
                    return c.reportErrorFmt("Cannot reference runtime variable at compile-time.", &.{}, node);
                },
                .func => {
                    const func_sym = sym.cast(.func);
                    if (func_sym.numFuncs == 1) {
                        const func_t = try cy.sema.getFuncSymType(c, func_sym.first.sig);
                        return TypeValue{
                            .type = func_t,
                            .value = Value.initPtr(func_sym.first),
                        };
                    }
                },
                .choice_case => {
                    const choice_case = sym.cast(.choice_case);
                    if (choice_case.payload_t.id() == bt.Void) {
                        return TypeValue{
                            .type = choice_case.type,
                            .value = try c.heap.newChoice(choice_case.type, choice_case.head.name(), cy.Value.Void),
                        };
                    }
                },
                .enum_case => {
                    const enum_case = sym.cast(.enum_case);
                    return TypeValue{
                        .type = enum_case.type,
                        .value = Value.initInt(@intCast(enum_case.val)),
                    };
                },
                else => {
                    if (sym.getStaticType()) |type_| {
                        return TypeValue{
                            .type = c.sema.type_t,
                            .value = Value.initPtr(type_),
                        };
                    }
                },
            }
            return c.reportErrorFmt("Unsupported conversion to compile-time value: {}", &.{v(sym.type)}, node);
        },
    }
}

pub fn evalSym(c: *cy.Chunk, node: *ast.Node) anyerror!*cy.Sym {
    const res = try evalExprInfer(c, node);
    return cte.deriveCtExprSym(c, res, node);
}

fn deriveCtExprSym(c: *cy.Chunk, expr: Expr, node: *ast.Node) !*cy.Sym {
    switch (expr.kind) {
        .sym => {
            return expr.data.sym;
        },
        .ct_var,
        .func => {
            return error.TODO;
        },
        .value => {
            defer c.heap.destructValue(expr.data.value);
            if (expr.data.value.type.id() == bt.Type) {
                const type_ = expr.data.value.value.asPtr(*cy.Type);
                return @ptrCast(type_.sym());
            }
            const type_n = try c.sema.allocTypeName(expr.data.value.type);
            defer c.alloc.free(type_n);
            return c.reportErrorFmt("Can not derive symbol from compile-time value: {}", &.{v(type_n)}, node);
        },
    }
}

pub fn deriveCtExprType2(c: *cy.Chunk, expr: Expr) !*cy.Type {
    switch (expr.kind) {
        .sym => {
            const sym = expr.data.sym;
            if (sym.getStaticType()) |type_| {
                return type_;
            }
            if (sym.type == .func) {
                const func_sym = sym.cast(.func);
                if (func_sym.numFuncs == 1) {
                    return sema.getFuncSymType(c, func_sym.first.sig);
                }
            }

            if (sym.type == .template) {
                return error.TemplateSym;
            } else {
                return error.BadSym;
            }
        },
        .func => {
            return error.TODO;
        },
        .ct_var => {
            if (expr.data.ct_var.getType().id() == bt.Type) {
                return expr.data.ct_var.value.asPtr(*cy.Type);
            }
            return error.BadValue;
        },
        .value => {
            defer c.heap.destructValue(expr.data.value);
            if (expr.data.value.type.id() == bt.Type) {
                return expr.data.value.value.asPtr(*cy.Type);
            }
            return error.BadValue;
        },
    }
}

fn deriveCtExprType(c: *cy.Chunk, expr: Expr, node: *ast.Node) !*cy.Type {
    return deriveCtExprType2(c, expr) catch |err| {
        if (err == error.TemplateSym) {
            return c.reportErrorFmt("Expected a type symbol. `{}` is a type template and must be expanded to a type first.", &.{
                v(expr.data.sym.name())}, node);
        } else if (err == error.BadSym) {
            const sym = expr.data.sym;
            return c.reportErrorFmt("Can not derive type from `{}`. Found `{}`.", &.{
                v(sym.name()), v(sym.type)}, node);
        } else if (err == error.BadValue) {
            const type_n = try c.sema.allocTypeName(expr.data.value.type);
            defer c.alloc.free(type_n);
            return c.reportErrorFmt("Can not derive type from compile-time value: {}", &.{v(type_n)}, node);
        } else {
            return err;
        }
    };
}

pub fn evalType(c: *cy.Chunk, node: *ast.Node) !*cy.Type {
    const res = try evalExprInfer(c, node);
    return cte.deriveCtExprType(c, res, node);
}

/// Resolves a type specifier node to a `Type`.
pub fn eval_type2(c: *cy.Chunk, resolve_new_type: bool, node: *ast.Node) !*cy.Type {
    var cstr = ExprCstr.initInfer();
    cstr.resolve_new_type = resolve_new_type;
    const res = try evalExpr(c, node, cstr);
    return cte.deriveCtExprType(c, res, node);
}

// Borrows `tval`.
pub fn evalFitTarget(c: *cy.Chunk, tval: TypeValue, target_t: *cy.Type, node: *ast.Node) !?Value {
    _ = node;
    if (target_t.kind() == .option) {
        const option = target_t.cast(.option);
        if (option.child_t.id() == tval.type.id()) {
            const copy = try c.heap.copy_value(tval);
            return try c.heap.newSome(option, copy.value);
        }
    }

    if (target_t.id() == bt.I64) {
        if (tval.type.id() == bt.EvalInt) {
            return Value.initInt(tval.value.as_eval_int());
        }
    }

    if (target_t.id() == bt.EvalInt) {
        if (tval.type.id() == bt.I64) {
            return Value.initGenericInt(tval.value.asInt());
        }
    }

    return null;
}

pub fn evalExpr(c: *cy.Chunk, node: *ast.Node, cstr: ExprCstr) anyerror!Expr {
    if (cy.Trace) {
        const node_str = c.encoder.formatTrunc(node);
        log.tracev("evalExpr {}: {s} \"{s}\"", .{c.trace_eval_expr_depth, @tagName(node.type()), node_str});
        c.trace_eval_expr_depth += 1;
    }
    defer {
        if (cy.Trace) {
            c.trace_eval_expr_depth -= 1;
            log.tracev("evalExpr {}: end", .{c.trace_eval_expr_depth});
        }
    }

    const res = try evalExprNoCheck(c, node, cstr);
    if (!cstr.fit_target and !cstr.check) {
        return res;
    }

    // Convert to TypeValue.
    // TODO: Maybe `Expr` should always be a TypeValue.
    const res_val = try exprToValue(c, res, node);
    const target_t = cstr.type orelse {
        return Expr.initValue(res_val);
    };

    if (cstr.match_generic_type) {
        if (res_val.type.id() == bt.Type) {
            switch (target_t.kind()) {
                .generic => {
                    return Expr.initValue(res_val);
                },
                .generic_trait => {
                    const _type = res_val.value.asPtr(*cy.Type);
                    // try sema_type.ensure_resolved_type(c, _type, node);
                    if (try sema_type.implements(c, _type, target_t.cast(.generic_trait), node)) {
                        return Expr.initValue(res_val);
                    }
                },
                else => {},
            }
        }
    } else {
        if (res_val.type.id() == target_t.id()) {
            return Expr.initValue(res_val);
        }
    }

    if (cstr.fit_target) {
        if (try evalFitTarget(c, res_val, target_t, node)) |new| {
            c.heap.destructValue(res_val);
            return Expr.initValue2(target_t, new);
        }
    }

    if (cstr.check) {
        const exp_name = try c.sema.allocTypeName(target_t);
        defer c.alloc.free(exp_name);
        const act_name = try c.sema.allocTypeName(res_val.type);
        defer c.alloc.free(act_name);
        return c.reportErrorFmt("Expected type `{}`. Found `{}`.", &.{v(exp_name), v(act_name)}, node);
    }
    return res;
}

pub fn evalExprInfer(c: *cy.Chunk, node: *ast.Node) !Expr {
    return evalExpr(c, node, ExprCstr.initInfer());
}

pub fn evalExprCheck(c: *cy.Chunk, node: *ast.Node, type_: *cy.Type) !Expr {
    return evalExpr(c, node, ExprCstr.init_check(type_));
}

pub fn deinitExpr(c: *cy.Chunk, expr: Expr) void {
    switch (expr.kind) {
        .ct_var,
        .sym,
        .func => {},
        .value => {
            c.heap.destructValue(expr.data.value);
        },
    }
}

const ExprKind = enum(u8) {
    ct_var,
    value,
    sym,
    func,
};

const Expr = struct {
    kind: ExprKind,
    data: union {
        ct_var: *sema.CtParam,
        value: TypeValue,
        sym: *cy.Sym,
        func: *cy.Func,
    },    

    fn initCtVar(ptr: *sema.CtParam) Expr {
        return .{ .kind = .ct_var, .data = .{ .ct_var = ptr }};
    }

    fn initValue2(type_: *cy.Type, value: cy.Value) Expr {
        return .{ .kind = .value, .data = .{ .value = cy.TypeValue.init(type_, value) }};
    }

    fn initValue(value: TypeValue) Expr {
        return .{ .kind = .value, .data = .{ .value = value }};
    }

    fn initSym(sym: *cy.Sym) Expr {
        return .{ .kind = .sym, .data = .{ .sym = sym }};
    }

    fn initFunc(func: *cy.Func) Expr {
        return .{ .kind = .func, .data = .{ .func = func }};
    }
};

fn getStructField(c: *cy.Chunk, struct_t: *cy.types.Struct, name: []const u8, node: *ast.Node) !cy.types.Field {
    const field_sym = struct_t.base.sym().getMod().getSym(name) orelse {
        return c.reportErrorFmt("Inline eval: No such field `{}`.", &.{v(name)}, node);
    };
    if (field_sym.type != .field) {
        return c.reportErrorFmt("Inline eval: `{}` is not a field.", &.{v(name)}, node);
    }
    return struct_t.fields()[field_sym.cast(.field).idx];
}

// Mimics `semaInitStruct`.
pub fn evalInitStruct(c: *cy.Chunk, struct_t: *cy.types.Struct, init_lit: *ast.InitLit) !Expr {
    const node: *ast.Node = @ptrCast(init_lit);
    const sym = struct_t.base.sym();

    try sema_type.ensure_resolved_type(c, &struct_t.base, @ptrCast(init_lit));

    // Set up a temp buffer to map initializer entries to type fields.
    const fieldsDataStart = c.listDataStack.items.len;
    try c.listDataStack.resize(c.alloc, c.listDataStack.items.len + struct_t.fields_len);
    defer c.listDataStack.items.len = fieldsDataStart;

    // Initially set to NullId so missed mappings are known from a linear scan.
    const fieldNodes = c.listDataStack.items[fieldsDataStart..];
    @memset(fieldNodes, .{ .node = null });

    for (init_lit.args.slice()) |arg| {
        const pair = arg.cast(.keyValue);
        const fieldName = pair.key.name();
        const info = try sema.checkSymInitField(c, @ptrCast(sym), fieldName, pair.key);
        fieldNodes[info.idx] = .{ .node = pair.value };
    }

    const fields = struct_t.fields();
    const arg_start = c.valueStack.items.len;
    defer {
        for (c.valueStack.items[arg_start..], 0..) |arg, i| {
            const field = fields[i];
            c.heap.destructValue2(field.type, arg);
        }
        c.valueStack.items.len = arg_start;
    }

    for (0..fieldNodes.len) |i| {
        const item = c.listDataStack.items[fieldsDataStart+i];
        if (item.node == null) {
            if (fields[i].init_n) |init_n| {
                const arg = try evalCheck(c, init_n, fields[i].type);
                try c.valueStack.append(c.alloc, arg.value);
            } else {
                if (!struct_t.cstruct) {
                    return c.reportErrorFmt("Initialization requires the field `{}`.", &.{v(struct_t.fields_ptr[i].sym.head.name())}, node);
                }
                const arg = try evalCZero(c, fields[i].type, node);
                try c.valueStack.append(c.alloc, arg);
            }
        } else {
            const arg = try evalCheck(c, item.node.?, fields[i].type);
            try c.valueStack.append(c.alloc, arg.value);
        }
    }

    const new = try c.heap.new_object_undef(struct_t.base.id(), struct_t.size);
    const dst = new.object.getBytePtr();

    for (fields, 0..) |field, i| {
        const arg = c.valueStack.items[arg_start + i];
        try c.heap.copyValueTo2(field.type, dst + field.offset, arg);
    }
    return Expr.initValue2(@ptrCast(struct_t), Value.initPtr(new));
}

pub fn initPartialVector(c: *cy.Chunk, vector_t: *cy.types.PartialVector, init_n: *ast.InitLit) !Expr {
    const nargs = init_n.args.len;
    if (nargs > vector_t.n) {
        return c.reportErrorFmt("Expected at most `{}` elements, found `{}`.", &.{v(vector_t.n), v(nargs)}, @ptrCast(init_n));
    }

    const arr = try c.heap.new_object_undef(vector_t.base.id(), vector_t.size);
    arr.object.firstValue = Value.initInt(@intCast(nargs));
    const dst: [*]u8 = arr.object.getBytePtr() + 8;
    for (init_n.args.slice(), 0..) |arg, i| {
        const res = try evalCheck(c, arg, vector_t.elem_t);
        defer c.heap.destructValue(res);
        try c.heap.copyValueTo2(vector_t.elem_t, dst + vector_t.elem_t.size() * i, res.value);
    }

    return Expr.initValue2(&vector_t.base, Value.initPtr(arr));
}

pub fn initVector(c: *cy.Chunk, vector_t: *cy.types.Vector, init_n: *ast.InitLit) !Expr {
    const nargs = init_n.args.len;
    if (nargs != vector_t.n) {
        return c.reportErrorFmt("Expected `{}` elements, found `{}`.", &.{v(vector_t.n), v(nargs)}, @ptrCast(init_n));
    }

    const arr = try c.heap.new_object_undef(vector_t.base.id(), vector_t.size);
    const dst: [*]u8 = @ptrCast(arr);
    for (init_n.args.slice(), 0..) |arg, i| {
        const res = try evalCheck(c, arg, vector_t.elem_t);
        defer c.heap.destructValue(res);
        try c.heap.copyValueTo2(vector_t.elem_t, dst + vector_t.elem_t.size() * i, res.value);
    }

    return Expr.initValue2(&vector_t.base, Value.initPtr(arr));
}

pub fn evalExprNoCheck(c: *cy.Chunk, node: *ast.Node, cstr: ExprCstr) anyerror!Expr {
    const opt_target = cstr.type;
    switch (node.type()) {
        // .enumMemberSym => {
        //     const data = c.ir.getExprData(loc, .enumMemberSym);
        //     const val_t = c.ir.getExprType(loc);
        //     const val = cy.Value.initInt(@intCast(data.val));
        //     return TypeValue.init(val_t, val);
        // },
        .as_expr => {
            const as_expr = node.cast(.as_expr);
            var target_t: *cy.Type = undefined;
            if (as_expr.target) |target| {
                target_t = try eval_type2(c, false, target);
            } else {
                target_t = opt_target orelse {
                    return c.reportError("Expected target type for auto cast.", node);
                };
            }

            const expr = try evalHint(c, as_expr.expr, target_t);
            if (target_t.id() == expr.type.id()) {
                return Expr.initValue2(target_t, expr.value);
            }

            if (target_t.kind() == .float) {
                if (expr.type.kind() == .int) {
                    if (expr.type.cast(.int).bits == target_t.cast(.float).bits) {
                        return Expr.initValue2(target_t, expr.value);
                    }
                }
            }
            if (target_t.isPointer()) {
                if (expr.type.kind() == .int) {
                    if (expr.type.cast(.int).bits == 64) {
                        return Expr.initValue2(target_t, expr.value);
                    }
                }
                if (expr.type.kind() == .raw) {
                    if (expr.type.cast(.raw).bits == 64) {
                        return Expr.initValue2(target_t, expr.value);
                    }
                }
            }
            if (target_t.isPointer()) {
                if (expr.type.kind() == .int) {
                    try sema_type.ensure_resolved_type(c, target_t, node);
                    const src_bits = expr.type.cast(.int).bits;
                    if (src_bits == target_t.size() * 8) {
                        return Expr.initValue2(target_t, expr.value);
                    } else if (src_bits > target_t.size() * 8) {
                        const src = expr.value.as_raw_lit(src_bits);
                        if (src > cy.value.ones(target_t.size() * 8)) {
                            return c.reportError("Loss of bits when casting integer to pointer.", node);
                        }
                        return Expr.initValue2(target_t, Value.initRaw(src));
                    }
                }
            }
            return c.reportError("Unsupported auto cast.", node);
        },
        .if_expr => {
            const if_expr = node.cast(.if_expr);
            const cond = try cte.evalCheck(c, if_expr.cond, c.sema.bool_t);
            if (cond.value.asBool()) {
                const res = try cte.evalTarget(c, if_expr.body, opt_target);
                return Expr.initValue(res);
            } else {
                const res = try cte.evalTarget(c, if_expr.else_expr, opt_target);
                return Expr.initValue(res);
            }
        },
        .undef_lit => {
            return c.reportErrorFmt("Undefined values are not allowed at compile-time.", &.{}, node);
        },
        .noneLit => {
            const target_t = opt_target orelse {
                return c.reportErrorFmt("Could not determine optional type for `none`.", &.{}, node);
            };
            if (target_t.kind() == .option) {
                const option_t = target_t.cast(.option);
                if (option_t.zero_union) {
                    const value = Value.initInt(0);
                    return Expr.initValue2(target_t, value);
                } else {
                    const value = try c.heap.newNone(option_t);
                    return Expr.initValue2(target_t, value);
                }
            }
            if (target_t.isPointer()) {
                return Expr.initValue2(target_t, Value.initPtr(null));
            }
            return c.reportErrorFmt("Cannot infer `none`. Expected `Option` target type.", &.{}, node);
        },
        .init_expr => {
            const init_expr = node.cast(.init_expr);
            const sym = try evalSym(c, init_expr.left);
            switch (sym.type) {
                .type => {
                    const type_ = sym.cast(.type).type;

                    try sema_type.ensure_eval_eligible_type(c, type_, node);

                    if (init_expr.lit.array_like) {
                        if (try c.getResolvedSym(sym, "@init_sequence", node)) |init_elems| {
                            _ = init_elems;
                        
                            return error.TODO;
                            // return evalInitArray(c, init_elems, node.lit);
                        }
                    } 

                    switch (type_.kind()) {
                        .partial_vector => {
                            return initPartialVector(c, type_.cast(.partial_vector), init_expr.lit);
                        },
                        .vector => {
                            return initVector(c, type_.cast(.vector), init_expr.lit);
                        },
                        .struct_t => {
                            if (!init_expr.lit.array_like or init_expr.lit.args.len == 0) {
                                return evalInitStruct(c, type_.cast(.struct_t), init_expr.lit);
                            }
                        },
                        .generic_vector => {
                            const vector_t = type_.cast(.generic_vector);
                            const array_t = try sema.getVectorType(c, vector_t.elem_t, @intCast(init_expr.lit.args.len));
                            return initVector(c, array_t.cast(.vector), init_expr.lit);
                        },
                        else => {
                            return c.reportErrorFmt("TODO: {}", &.{v(type_.kind())}, node);
                        },
                    }
                },
                else => {},
            }
        },
        .init_lit => {
            const init_lit = node.cast(.init_lit);

            if (opt_target) |target_t| {
                try sema_type.ensure_eval_eligible_type(c, target_t, node);

                if (target_t.kind() == .struct_t) {
                    if (init_lit.args.len == 0) {
                        // TODO: Replace special cases with calls to @init_sequence, @init_record.
                        if (target_t.isInstanceOf(c.sema.slice_tmpl)) {
                            const new = try c.heap.newInstance(target_t, &.{
                                C.toFieldInit("buf", @bitCast(Value.initPtr(null))),
                                C.toFieldInit("ptr", @bitCast(Value.initPtr(null))),
                                C.toFieldInit("_len", @bitCast(Value.initInt(0))),
                                C.toFieldInit("header", @bitCast(Value.initInt(@bitCast(@as(u64, 1) << 63)))),
                            });
                            return Expr.initValue2(target_t, new);
                        }
                        if (target_t.isInstanceOf(c.sema.span_tmpl)) {
                            const new = try c.heap.newInstance(target_t, &.{
                                C.toFieldInit("base", @bitCast(Value.initPtr(null))),
                                C.toFieldInit("length", @bitCast(Value.initInt(0))),
                            });
                            return Expr.initValue2(target_t, new);
                        }
                        if (target_t.isInstanceOf(c.sema.array_tmpl)) {
                            const new = try c.heap.newInstance(target_t, &.{
                                C.toFieldInit("buf", @bitCast(Value.initPtr(null))),
                                C.toFieldInit("length", @bitCast(Value.initInt(0))),
                                C.toFieldInit("header", @bitCast(Value.initInt(0))),
                            });
                            return Expr.initValue2(target_t, new);
                        }
                        if (target_t.isInstanceOf(c.sema.map_tmpl)) {
                            const generic_map_t = target_t.cast(.struct_t).fields()[0].type;
                            const base = try c.heap.newInstance(generic_map_t, &.{
                                C.toFieldInit("meta", @bitCast(Value.initPtr(null))),
                                C.toFieldInit("_keys", @bitCast(Value.initPtr(null))),
                                C.toFieldInit("vals", @bitCast(Value.initPtr(null))),
                                C.toFieldInit("nvac", @bitCast(Value.initInt(0))),
                                C.toFieldInit("len", @bitCast(Value.initInt(0))),
                                C.toFieldInit("cap", @bitCast(Value.initInt(0))),
                            });
                            const new = try c.heap.newInstance(target_t, &.{
                                C.toFieldInit("base", @bitCast(base)),
                            });
                            return Expr.initValue2(target_t, new);
                        }
                        if (target_t.isInstanceOf(c.sema.hash_map_tmpl)) {
                            const new = try c.heap.newInstance(target_t, &.{
                                C.toFieldInit("meta", @bitCast(Value.initPtr(null))),
                                C.toFieldInit("_keys", @bitCast(Value.initPtr(null))),
                                C.toFieldInit("vals", @bitCast(Value.initPtr(null))),
                                C.toFieldInit("nvac", @bitCast(Value.initInt(0))),
                                C.toFieldInit("len", @bitCast(Value.initInt(0))),
                                C.toFieldInit("cap", @bitCast(Value.initInt(0))),
                            });
                            return Expr.initValue2(target_t, new);
                        }
                    }
                    if (!init_lit.array_like or init_lit.args.len == 0) {
                        return evalInitStruct(c, target_t.cast(.struct_t), init_lit);
                    }
                } else if (target_t.kind() == .partial_vector) {
                    return initPartialVector(c, target_t.cast(.partial_vector), init_lit);
                } else if (target_t.kind() == .vector) {
                    return initVector(c, target_t.cast(.vector), init_lit);
                } else {
                    // TODO.
                }
            }
        },
        .sq_string_lit => {
            // TODO: Escape.
            const str = node.cast(.sq_string_lit).asString();
            return evalStringLiteral(c, str, opt_target);
        },
        .string_lit => {
            // TODO: Escape.
            const str = node.cast(.string_lit).asString();
            return evalStringLiteral(c, str, opt_target);
        },
        .raw_string_lit => {
            const lit = node.cast(.raw_string_lit).asRawString();
            return Expr.initValue(.{
                .type = c.sema.str_t,
                .value = try c.heap.newStr(lit),
            });
        },
        .raw_string_multi_lit => {
            const lit = node.cast(.raw_string_multi_lit).asRawStringMulti();
            return Expr.initValue(.{
                .type = c.sema.str_t,
                .value = try c.heap.newStr(lit),
            });
        },
        .special_string_lit => {
            const lit = node.cast(.special_string_lit);
            _ = lit;
        },
        .floatLit => {
            const literal = node.cast(.floatLit).value.slice();
            if (opt_target) |target_t| {
                if (target_t.id() == bt.F32) {
                    const val = try std.fmt.parseFloat(f32, literal);
                    return Expr.initValue(.{
                        .type = c.sema.f32_t,
                        .value = Value.initFloat32(val),
                    });
                }
            }
            const val = try std.fmt.parseFloat(f64, literal);
            return Expr.initValue(.{
                .type = c.sema.f64_t,
                .value = Value.initFloat64(val),
            });
        },
        .trueLit => {
            return Expr.initValue(.{
                .type = c.sema.bool_t,
                .value = Value.True,
            });
        },
        .falseLit => {
            return Expr.initValue(.{
                .type = c.sema.bool_t,
                .value = Value.False,
            });
        },
        .dot_lit => {
            const dot_lit = node.cast(.dot_lit);
            const name = dot_lit.as_dot_infer_name();
            if (opt_target) |target_t| {
                if (target_t.kind() == .enum_t) {
                    try sema_type.ensure_resolved_type(c, target_t, node);
                    const case = target_t.cast(.enum_t).getCase(name) orelse {
                        return c.reportErrorFmt("Could not infer `{}` enum case from `{}`.", &.{v(target_t.name()), v(name)}, node);
                    };
                    return Expr.initValue(.{
                        .type = target_t,
                        .value = Value.initInt(@intCast(case.val)),
                    });
                }
            }
        },
        .at_lit => {
            const at_lit = node.cast(.at_lit);
            const name = at_lit.as_at_infer_name();
            return Expr.initValue(.{
                .type = c.sema.symbol_t,
                .value = try c.sema.initSymbol(name),
            });
        },
        .hexLit => {
            const literal = node.cast(.hexLit).asHex();
            if (opt_target) |target_t| {
                if (try evalUnsignedLiteral(c, literal, target_t, 16, node)) |res| {
                    return res;
                }
            }
            const val = try std.fmt.parseInt(u64, literal, 16);
            return Expr.initValue(.{
                .type = c.sema.i64_t,
                .value = Value.initInt(@bitCast(val)),
            });
        },
        .octLit => {
            const literal = node.cast(.octLit).asOct();
            if (opt_target) |target_t| {
                if (try evalUnsignedLiteral(c, literal, target_t, 8, node)) |res| {
                    return res;
                }
            }
            const val = try std.fmt.parseInt(u64, literal, 8);
            return Expr.initValue(.{
                .type = c.sema.i64_t,
                .value = Value.initInt(@bitCast(val)),
            });
        },
        .dec_u => {
            const literal = node.cast(.dec_u).asDecU();
            if (opt_target) |target_t| {
                if (try evalUnsignedLiteral(c, literal, target_t, 10, node)) |res| {
                    return res;
                }
            }
            const val = try std.fmt.parseInt(u64, literal, 10);
            return Expr.initValue(.{
                .type = c.sema.i64_t,
                .value = Value.initInt(@bitCast(val)),
            });
        },
        .decLit => {
            const literal = node.cast(.decLit).value.slice();
            if (opt_target) |target_t| {
                if (try evalSignedLiteral(c, literal, target_t, 10, node)) |res| {
                    return res;
                }
            }
            const val = try std.fmt.parseInt(i64, literal, 10);
            return Expr.initValue(.{
                .type = c.sema.i64_t,
                .value = Value.initInt(val),
            });
        },
        .ident => {
            const name = node.cast(.ident).name.slice();
            return eval_ident(c, name, node);
        },
        .expand_lit => {
            const expand_lit = node.cast(.expand_lit);
            const val = try cte.eval(c, expand_lit.child);
            return Expr.initValue(val);
        },
        .infer_param => {
            const name = node.cast(.infer_param).name.name();

            const ctx = sema.getResolveContext(c);
            if (ctx.data.func.instance != null) {
                // Resolving variant func.
                const res = try sema.lookupStaticIdent(c, name, node) orelse {
                    return c.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, node);
                };
                switch (res) {
                    .capture,
                    .local => {
                        return c.reportErrorFmt("Expected compile-time expression.", &.{}, node);
                    },
                    .static => |sym| {
                        return Expr.initSym(sym);
                    },
                    .ct_var => {
                        return error.TODO;
                    },
                    .ct_value => |ct_value| {
                        return Expr.initValue(ct_value);
                    },
                }
            } else {
                // Exit eval and visit type spec node to find all infer params not just this one.
                return error.FoundInferParam;
            }
        },
        .generic_expand => {
            const ctx = sema.getResolveContext(c);
            if (ctx.getSelfSym()) |sym| {
                return Expr.initSym(sym);
            }
        },
        .unwrap_or => {
            const unwrap = node.cast(.unwrap_or);
            const option = try eval(c, unwrap.opt);
            defer c.heap.destructValue(option);
            if (option.type.kind() != .option) {
                const name = try c.sema.allocTypeName(option.type);
                defer c.alloc.free(name);
                return c.reportErrorFmt("Expected `Option` type, found `{}`.", &.{v(name)}, node);
            }

            const option_t = option.type.cast(.option);
            const ptr = option.value.asPtr([*]const i64);
            if (ptr[0] == 1) {
                const payload = try c.heap.copyValue3(option_t.child_t, @ptrCast(ptr + 1));
                return Expr.initValue2(option_t.child_t, payload);
            } else {
                const default = try eval(c, unwrap.default);
                return Expr.initValue(default);
            }
        },
        .unwrap_choice => {
            const unwrap = node.cast(.unwrap_choice);
            const choice = try eval(c, unwrap.left);
            defer c.heap.destructValue(choice);

            const choice_t = choice.type.cast(.choice);
            const name = unwrap.right.name();
            const case = choice_t.getCase(name) orelse {
                return c.reportErrorFmt("Choice case `{}` does not exist.", &.{v(name)}, unwrap.right);
            };

            const tag = choice.value.asPtr(*i64).*;
            if (tag == @as(i64, @intCast(case.val))) {
                const res = try c.heap.copyValue3(case.payload_t, @ptrCast(choice.value.asPtr([*]u8) + 8));
                return Expr.initValue2(case.payload_t, res);
            } else {
                const act = choice_t.getCaseByTag(@intCast(tag));
                return c.reportErrorFmt("Expected choice tag `{}`, found `{}`.", &.{v(name), v(act.head.name())}, unwrap.right);
            }
        },
        .switchExpr => {
            const block = node.cast(.switchExpr);
            const eval_ctx = EvalContext{
                .ret = c.sema.void_t,
            };
            var mut_ctx = EvalContextAndResult{ .ctx = eval_ctx, .res = undefined };
            try switchStmt(c, block, false, switchExprCaseInner, &mut_ctx, opt_target);
            if (!mut_ctx.res.ret) {
                return c.reportError("Expected expression value.", node);
            }
            return Expr.initValue2(mut_ctx.ctx.ret, mut_ctx.res.ret_value);
        },
        .deref => {
            const deref = node.cast(.deref);

            const rec = try eval(c, deref.left);
            defer c.heap.destructValue(rec);

            const child_t = rec.type.getRefLikeChild() orelse {
                return c.reportError("Expected reference type.", node);
            };
            const res = try c.heap.copyValue3(child_t, rec.value.asBytes());
            return Expr.initValue2(child_t, res);
        },
        .index_expr => {
            const index_expr = node.cast(.index_expr);

            const rec_expr = try evalExprInfer(c, index_expr.left);
            defer deinitExpr(c, rec_expr);

            switch (rec_expr.kind) {
                .ct_var => {
                    return error.TODO;
                },
                .value => {
                    const rec = rec_expr.data.value;
                    if (rec.type.kind() == .pointer) {
                        const pointer = rec.type.cast(.pointer);

                        if (pointer.child_t.kind() == .struct_t) {
                            if (pointer.child_t.sym().instance) |instance| {
                                if (instance.data.sym.template == c.sema.raw_buffer_tmpl) {
                                    const elem_t = instance.params[0].asPtr(*cy.Type);
                                    const idxv = try evalTarget(c, index_expr.args.ptr[0], c.sema.i64_t);
                                    if (idxv.type.kind() != .int) {
                                        return c.reportError("Expected integer.", index_expr.args.ptr[0]);
                                    }
                                    const idx: usize = @intCast(idxv.value.asInt());
                                    const buf = rec.value.asPtr(*cy.heap.RawBuffer);
                                    const src = buf.getElemPtr(idx, elem_t.size());

                                    const str: *cy.heap.Str = @ptrCast(@alignCast(src));
                                    _ = str;

                                    const res = try c.heap.copyValue3(elem_t, @ptrCast(@alignCast(src)));
                                    return Expr.initValue2(elem_t, res);
                                }
                            }
                        }
                    }

                    if (try c.accessResolvedSym(rec.type, "@index_addr", node)) |sym| {
                        const func_sym = try sema.requireFuncSym(c, sym, node);
                        const addr = try callFuncSymRec(c, func_sym, index_expr.left, sema.ExprResult.initCtValue(rec), &.{ index_expr.args.ptr[0] }, node);

                        if (addr.type.isPointer() or addr.type.kind() == .borrow) {
                            const child_t = addr.type.getRefLikeChild().?;
                            const res = try c.heap.copyValue3(child_t, addr.value.asBytes());
                            return Expr.initValue2(child_t, res);
                        } else {
                            return c.reportErrorFmt("Expected `Ptr`, `Borrow` return type for `@index_addr`.", &.{}, node);
                        }
                    }
                    return c.reportErrorFmt("Inline eval: Cannot index receiver.", &.{}, index_expr.args.ptr[0]);
                },
                .func => {
                    return error.TODO;
                },
                .sym => {
                    const sym = rec_expr.data.sym;
                    if (sym.type == .template) {
                        const template = sym.cast(.template);
                        const new = try cy.template.expand_template_ast2(c, template, index_expr.args.slice(), cstr.resolve_new_type, node);
                        return Expr.initSym(new);
                    } else if (sym.type == .func_template) {
                        const template = sym.cast(.func_template);
                        const new = try cy.template.expandTemplateFuncForArgs(c, template, index_expr.args.slice(), node);
                        return Expr.initFunc(new);
                    } else {
                        return c.reportErrorFmt("Unsupported expand expression.", &.{}, node);
                    }
                },
            }
        },
        .borrow => {
            const borrow = node.cast(.borrow);
            const sym = try cy.template.expand_template_ast(c, c.sema.borrow_tmpl, &.{ borrow.child }, node);
            return Expr.initSym(sym);
        },
        .ex_borrow => {
            const borrow = node.cast(.ex_borrow);
            const sym = try cy.template.expand_template_ast(c, c.sema.ex_borrow_tmpl, &.{ borrow.child }, node);
            return Expr.initSym(sym);
        },
        .ref => {
            const ref = node.cast(.ref);
            const child = try eval(c, ref.child);
            defer c.heap.destructValue(child);

            if (child.type.id() != bt.Type) {
                return c.reportErrorFmt("Expected child type. Cannot lift values during const evaluation.", &.{}, node);
            }
            const sym = try cy.template.expand_template_ast(c, c.sema.ref_tmpl, &.{ ref.child }, node);
            return Expr.initSym(sym);
        },
        .option_type => {
            const option_type = node.cast(.option_type);
            const sym = try cy.template.expand_template_ast(c, c.sema.option_tmpl, &.{option_type.child}, node);
            return Expr.initSym(sym);
        },
        .partial_vector_type => {
            const vector_t = node.cast(.partial_vector_type);
            const sym = try cy.template.expand_template_ast(c, c.sema.partial_vector_tmpl, &.{vector_t.child, vector_t.n}, node);
            return Expr.initSym(sym);
        },
        .vector_type => {
            const vector_t = node.cast(.vector_type);
            const sym = try cy.template.expand_template_ast(c, c.sema.vector_tmpl, &.{vector_t.child, vector_t.n}, node);
            return Expr.initSym(sym);
        },
        .generic_vector_type => {
            const vector_t = node.cast(.generic_vector_type);
            const sym = try cy.template.expand_template_ast(c, c.sema.generic_vector_tmpl, &.{ vector_t.elem_t }, node);
            return Expr.initSym(sym);
        },
        .span_type => {
            const span_type = node.cast(.span_type);
            const sym = try cy.template.expand_template_ast2(c, c.sema.span_tmpl, &.{span_type.child}, cstr.resolve_new_type, node);
            return Expr.initSym(sym);
        },
        .slice_type => {
            const slice_type = node.cast(.slice_type);
            const sym = try cy.template.expand_template_ast(c, c.sema.slice_tmpl, &.{slice_type.child}, node);
            return Expr.initSym(sym);
        },
        .accessExpr => {
            const access_expr = node.cast(.accessExpr);
            const parent = try evalExprInfer(c, access_expr.left);
            defer deinitExpr(c, parent);

            if (access_expr.right.type() != .ident) {
                return c.reportErrorFmt("Expected identifier.", &.{}, access_expr.right);
            }
            const name = access_expr.right.name();
            switch (parent.kind) {
                .value => {
                    return accessValue(c, parent.data.value, name, access_expr.right);
                },
                .ct_var => {
                    const recv = cy.TypeValue.init(parent.data.ct_var.getType(), parent.data.ct_var.value);
                    return accessValue(c, recv, name, access_expr.right);
                },
                .func => {
                    return c.reportError("TODO", access_expr.left);
                },
                .sym => {
                    const sym = try c.getResolvedSymOrFail(parent.data.sym, name, access_expr.right);
                    try c.checkResolvedSymIsDistinct(sym, access_expr.right);
                    if (sym.type == .const_) {
                        const const_ = sym.cast(.const_);
                        try sema.resolveConst(c, const_);
                        const dupe = try c.heap.copyValue2(const_.type, const_.value);
                        return Expr.initValue2(const_.type, dupe);
                    }
                    return Expr.initSym(sym);
                },
            }
        },
        .callExpr => {
            const call_expr = node.cast(.callExpr);
            return evalCallExpr(c, call_expr);
        },
        .group => {
            const group = node.cast(.group);
            return evalExprNoCheck(c, group.child, cstr);
        },
        .fn_type => {
            const func_type = node.cast(.fn_type);
            const sig = try sema_type.resolveFuncType(c, func_type);
            const arg = cy.Value.initFuncSig(sig);
            const sym = try cy.template.expand_template(c, c.sema.func_ptr_tmpl, &.{c.sema.funcsig_t}, &.{ arg });
            return Expr.initSym(sym);
        },
        .fnsym_type => {
            const func_type = node.cast(.fnsym_type);
            const sig = try sema_type.resolveFuncType(c, func_type);
            const arg = cy.Value.initFuncSig(sig);
            const sym = try cy.template.expand_template(c, c.sema.func_sym_tmpl, &.{c.sema.funcsig_t}, &.{ arg });
            return Expr.initSym(sym);
        },
        .struct_decl => {
            // Unnamed struct.
            const sym: *cy.Sym = @ptrCast(@alignCast(node.cast(.struct_decl).name));
            return Expr.initSym(sym);
        },
        .unary_expr => {
            const unary = node.cast(.unary_expr);
            return eval_un_expr(c, opt_target, unary);
        },
        .binExpr => {
            const expr = node.cast(.binExpr);
            const res = try eval_bin_expr(c, expr.left, expr.op, expr.right, node);
            return Expr.initValue(res);
        },
        else => {},
    }
    return c.reportErrorFmt("Unsupported compile-time expression: `{}`", &.{v(node.type())}, node);
}

fn eval_ident(c: *cy.Chunk, name: []const u8, node: *ast.Node) !Expr {
    const res = try sema.lookupStaticIdent(c, name, node) orelse {
        return c.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, node);
    };
    switch (res) {
        .capture,
        .local => {
            @panic("Unexpected.");
        },
        .static => |sym| {
            if (sym.type == .const_) {
                const const_ = sym.cast(.const_);
                try sema.resolveConst(c, const_);
                const dupe = try c.heap.copyValue2(const_.type, const_.value);
                return Expr.initValue2(const_.type, dupe);
            }
            return Expr.initSym(sym);
        },
        .ct_var => |ct_var| {
            return Expr.initCtVar(ct_var);
        },
        .ct_value => |ct_value| {
            return Expr.initValue(ct_value);
        },
    }
}

fn eval_un_expr(c: *cy.Chunk, opt_target: ?*cy.Type, unary: *ast.Unary) !Expr {
    const node: *ast.Node = @ptrCast(unary);
    switch (unary.op) {
        .minus => {
            if (unary.child.type() == .decLit) {
                var buf: [32]u8 = undefined;
                buf[0] = '-';
                const literal = unary.child.cast(.decLit).value.slice();
                @memcpy(buf[1..1+literal.len], literal);
                const neg_literal = buf[0..literal.len+1];
                if (opt_target) |target_t| {
                    if (try evalSignedLiteral(c, neg_literal, target_t, 10, node)) |res| {
                        return res;
                    }
                }
                const val = try sema.semaParseInt(c, i64, neg_literal, 10, node);
                return Expr.initValue(.{
                    .type = c.sema.i64_t,
                    .value = Value.initInt(val),
                });
            }

            const child = try eval(c, unary.child);
            defer c.heap.destructValue(child);

            switch (child.type.id()) {
                bt.F32 => {
                    const res = cy.TypeValue.init(c.sema.f32_t, Value.initFloat32(-child.value.asF32()));
                    return Expr.initValue(res);
                },
                bt.F64 => {
                    const res = cy.TypeValue.init(c.sema.f64_t, Value.initFloat64(-child.value.asF64()));
                    return Expr.initValue(res);
                },
                bt.I64 => {
                    const res = cy.TypeValue.init(c.sema.i64_t, Value.initInt(-child.value.asInt()));
                    return Expr.initValue(res);
                },
                bt.I32 => {
                    const res = cy.TypeValue.init(c.sema.i32_t, Value.initInt32(-child.value.asI32()));
                    return Expr.initValue(res);
                },
                bt.I16 => {
                    const res = cy.TypeValue.init(c.sema.i16_t, Value.initInt16(-child.value.asI16()));
                    return Expr.initValue(res);
                },
                bt.I8 => {
                    const res = cy.TypeValue.init(c.sema.i8_t, Value.initInt8(-child.value.asI8()));
                    return Expr.initValue(res);
                },
                else => {},
            }

            const arg = cy.sema_func.Argument.initPreResolved(unary.child, sema.ExprResult.initCtValue(child));

            // Look for sym under child type's module.
            const childTypeSym = child.type.sym();
            const sym = try c.getResolvedSymOrFail(@ptrCast(childTypeSym), unary.op.name(), node);
            const func_sym = try sema.requireFuncSym(c, sym, node);
            const res = try callFuncSym(c, func_sym, &.{arg}, &.{}, node);
            return Expr.initValue(res);
        },
        .lnot => {
            const child = try eval(c, unary.child);
            defer c.heap.destructValue(child);

            switch (child.type.id()) {
                bt.Type => {
                    const sym = try cy.template.expand_template(c, c.sema.result_tmpl, &.{c.sema.type_t}, &.{child.value});
                    return Expr.initSym(sym);
                },
                bt.Bool => {
                    const res = cy.TypeValue.init(c.sema.bool_t, Value.initBool(!child.value.asBool()));
                    return Expr.initValue(res);
                },
                else => {},
            }

            const arg = cy.sema_func.Argument.initPreResolved(unary.child, sema.ExprResult.initCtValue(child));

            const sym = try c.getResolvedSymOrFail(&child.type.sym().head, unary.op.name(), node);
            const func_sym = try sema.requireFuncSym(c, sym, node);
            const res = try callFuncSym(c, func_sym, &.{arg}, &.{}, node);
            return Expr.initValue(res);
        },
        .bitwiseNot => {
            const child = try evalTarget(c, unary.child, opt_target);
            defer c.heap.destructValue(child);

            switch (child.type.id()) {
                bt.R8,
                bt.R16,
                bt.R32,
                bt.R64,
                bt.I8,
                bt.I16,
                bt.I32,
                bt.I64 => {
                    const res = cy.TypeValue.init(child.type, Value.initRaw(~child.value.val));
                    return Expr.initValue(res);
                },
                else => {},
            }

            const arg = cy.sema_func.Argument.initPreResolved(unary.child, sema.ExprResult.initCtValue(child));

            const childTypeSym = child.type.sym();
            const sym = try c.getResolvedSymOrFail(@ptrCast(childTypeSym), unary.op.name(), node);
            const func_sym = try sema.requireFuncSym(c, sym, node);
            const res = try callFuncSym(c, func_sym, &.{arg}, &.{}, node);
            return Expr.initValue(res);
        },
        else => {},
    }
    return c.reportErrorFmt("Unsupported compile-time expression: `{}`", &.{v(node.type())}, node);
}

fn eval_bin_expr2(c: *cy.Chunk, left: cy.TypeValue, left_n: *ast.Node, right: cy.TypeValue, right_n: *ast.Node, op: cy.BinaryExprOp, node: *ast.Node) !TypeValue {
    switch (left.type.id()) {
        bt.F64 => {
            switch (op) {
                .star => {
                    const left_v = left.value.asF64();
                    const right_v = right.value.asF64();
                    return cy.TypeValue.init(c.sema.f64_t, Value.initFloat64(left_v * right_v));
                },
                else => {
                    return c.reportErrorFmt("Unsupported inline eval: {}", &.{v(op)}, node);
                },
            }
        },
        bt.F32 => {
            switch (op) {
                .star => {
                    const left_v = left.value.asF32();
                    const right_v = right.value.asF32();
                    return cy.TypeValue.init(c.sema.f32_t, Value.initFloat32(left_v * right_v));
                },
                else => {
                    return c.reportErrorFmt("Unsupported inline eval: {}", &.{v(op)}, node);
                },
            }
        },
        bt.R64 => {
            switch (op) {
                .plus => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(left.type, Value.initR64(left_v +% right_v));
                },
                .minus => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(left.type, Value.initR64(left_v -% right_v));
                },
                .star => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(left.type, Value.initR64(left_v *% right_v));
                },
                .bitwiseRightShift => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(left.type, Value.initR64(left_v >> @intCast(right_v)));
                },
                .bitwiseLeftShift => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(left.type, Value.initR64(left_v << @intCast(right_v)));
                },
                .less_equal => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v <= right_v));
                },
                .less => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v < right_v));
                },
                .greater_equal => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v >= right_v));
                },
                .greater => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v > right_v));
                },
                else => {
                    return c.reportErrorFmt("Unsupported inline eval: {}", &.{v(op)}, node);
                },
            }
        },
        bt.R8 => {
            switch (op) {
                .less_equal => {
                    const left_v = left.value.asU8();
                    const right_v = right.value.asU8();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v <= right_v));
                },
                .less => {
                    const left_v = left.value.asU8();
                    const right_v = right.value.asU8();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v < right_v));
                },
                .greater_equal => {
                    const left_v = left.value.asU8();
                    const right_v = right.value.asU8();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v >= right_v));
                },
                .greater => {
                    const left_v = left.value.asU8();
                    const right_v = right.value.asU8();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v > right_v));
                },
                else => {
                    return c.reportErrorFmt("Unsupported inline eval: {}", &.{v(op)}, node);
                },
            }
        },
        bt.R16 => {
            switch (op) {
                .less_equal => {
                    const left_v = left.value.asU16();
                    const right_v = right.value.asU16();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v <= right_v));
                },
                .less => {
                    const left_v = left.value.asU16();
                    const right_v = right.value.asU16();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v < right_v));
                },
                .greater_equal => {
                    const left_v = left.value.asU16();
                    const right_v = right.value.asU16();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v >= right_v));
                },
                .greater => {
                    const left_v = left.value.asU16();
                    const right_v = right.value.asU16();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v > right_v));
                },
                else => {
                    return c.reportErrorFmt("Unsupported inline eval: {}", &.{v(op)}, node);
                },
            }
        },
        bt.R32 => {
            switch (op) {
                .less_equal => {
                    const left_v = left.value.asU32();
                    const right_v = right.value.asU32();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v <= right_v));
                },
                .less => {
                    const left_v = left.value.asU32();
                    const right_v = right.value.asU32();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v < right_v));
                },
                .greater_equal => {
                    const left_v = left.value.asU32();
                    const right_v = right.value.asU32();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v >= right_v));
                },
                .greater => {
                    const left_v = left.value.asU32();
                    const right_v = right.value.asU32();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v > right_v));
                },
                else => {
                    return c.reportErrorFmt("Unsupported inline eval: {}", &.{v(op)}, node);
                },
            }
        },
        bt.I8 => {
            switch (op) {
                .greater => {
                    const left_v = left.value.asI8();
                    const right_v = right.value.asI8();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v > right_v));
                },
                .less => {
                    const left_v = left.value.asI8();
                    const right_v = right.value.asI8();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v < right_v));
                },
                else => {
                    return c.reportErrorFmt("Unsupported inline eval: {}", &.{v(op)}, node);
                },
            }
        },
        bt.I16 => {
            switch (op) {
                .greater => {
                    const left_v = left.value.asI16();
                    const right_v = right.value.asI16();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v > right_v));
                },
                .less => {
                    const left_v = left.value.asI16();
                    const right_v = right.value.asI16();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v < right_v));
                },
                else => {
                    return c.reportErrorFmt("Unsupported inline eval: {}", &.{v(op)}, node);
                },
            }
        },
        bt.I32 => {
            switch (op) {
                .greater => {
                    const left_v = left.value.asI32();
                    const right_v = right.value.asI32();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v > right_v));
                },
                .less => {
                    const left_v = left.value.asI32();
                    const right_v = right.value.asI32();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v < right_v));
                },
                else => {
                    return c.reportErrorFmt("Unsupported inline eval: {}", &.{v(op)}, node);
                },
            }
        },
        bt.I64 => {
            switch (op) {
                .plus => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(left.type, Value.initR64(left_v +% right_v));
                },
                .minus => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(left.type, Value.initR64(left_v -% right_v));
                },
                .star => {
                    const left_v = left.value.asInt();
                    const right_v = right.value.asInt();
                    return cy.TypeValue.init(left.type, Value.initInt(left_v *% right_v));
                },
                .slash => {
                    const left_v = left.value.asInt();
                    const right_v = right.value.asInt();
                    return cy.TypeValue.init(left.type, Value.initInt(@divTrunc(left_v, right_v)));
                },
                .greater => {
                    const left_v = left.value.asInt();
                    const right_v = right.value.asInt();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v > right_v));
                },
                .less => {
                    const left_v = left.value.asInt();
                    const right_v = right.value.asInt();
                    return cy.TypeValue.init(c.sema.bool_t, Value.initBool(left_v < right_v));
                },
                .bitwiseRightShift => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(left.type, Value.initR64(left_v >> @intCast(right_v)));
                },
                .bitwiseLeftShift => {
                    const left_v = left.value.asUint();
                    const right_v = right.value.asUint();
                    return cy.TypeValue.init(left.type, Value.initR64(left_v << @intCast(right_v)));
                },
                else => {
                    return c.reportErrorFmt("Unsupported inline eval: {}", &.{v(op)}, node);
                }
            }
        },
        else => {
            const leftTypeSym = left.type.sym();
            const sym = try c.getResolvedSymOrFail(@ptrCast(leftTypeSym), op.name(), node);
            const func_sym = try sema.requireFuncSym(c, sym, node);
            const args = [_]sema_func.Argument{
                sema_func.Argument.initReceiver(left_n, sema.ExprResult.initCtValue(left)),
                sema_func.Argument.initPreResolved(right_n, sema.ExprResult.initCtValue(right)),
            };
            return callFuncSym(c, func_sym, &args, &.{}, node);
        },
    }
}

fn eval_bin_expr(c: *cy.Chunk, left_n: *ast.Node, op: ast.BinaryExprOp, right_n: *ast.Node, node: *ast.Node) !TypeValue {
    switch (op) {
        .bang_equal => {
            const left = try eval(c, left_n);
            defer c.heap.destructValue(left);
            const right = try evalCheck(c, right_n, left.type);
            defer c.heap.destructValue(right);

            const res = try evalCompare(c, left.type, left.value, right.value, node);
            return TypeValue.init(c.sema.bool_t, Value.initBool(!res));
        },
        .equal_equal => {
            const left = try eval(c, left_n);
            defer c.heap.destructValue(left);
            const right = try evalCheck(c, right_n, left.type);
            defer c.heap.destructValue(right);

            const res = try evalCompare(c, left.type, left.value, right.value, node);
            return TypeValue.init(c.sema.bool_t, Value.initBool(res));
        },
        .star,
        .slash,
        .pow,
        .less,
        .less_equal,
        .greater,
        .greater_equal,
        .bitwiseLeftShift,
        .plus,
        .minus => {        
            switch (left_n.type()) {
                .floatLit => {
                    // Infer type from right.
                    const right = try eval(c, right_n);
                    const left = try evalHint(c, left_n, right.type);
                    return eval_bin_expr2(c, left, left_n, right, right_n, op, node);
                },
                else => {},
            }
            const left = try eval(c, left_n);
            defer c.heap.destructValue(left);
            if (left.type.kind() == .int or left.type.kind() == .raw or left.type.kind() == .float) {
                const right = try evalCheck(c, right_n, left.type);
                defer c.heap.destructValue(right);
                return eval_bin_expr2(c, left, left_n, right, right_n, op, node);
            }

            const leftTypeSym = left.type.sym();
            const sym = try c.getResolvedSymOrFail(@ptrCast(leftTypeSym), op.name(), node);
            const func_sym = try sema.requireFuncSym(c, sym, node);
            return callFuncSymRec(c, func_sym, left_n, sema.ExprResult.initCtValue(left), &.{ right_n }, node);
        },
        .or_op => {
            const left = try evalCheck(c, left_n, c.sema.bool_t);
            if (left.value.asBool()) {
                return TypeValue.init(c.sema.bool_t, Value.True);
            }
            return evalCheck(c, right_n, c.sema.bool_t);
        },
        .and_op => {
            const left = try evalCheck(c, left_n, c.sema.bool_t);
            if (!left.value.asBool()) {
                return TypeValue.init(c.sema.bool_t, Value.False);
            }
            return evalCheck(c, right_n, c.sema.bool_t);
        },
        else => {
            return c.reportErrorFmt("Inline eval: Unsupported op: `{}`\n", &.{v(op)}, node);
        },
    }
}

fn evalStringLiteral(c: *cy.Chunk, str: []const u8, opt_target: ?*cy.Type) !Expr {
    if (opt_target) |target_t| {
        // if (target_t.isPointer()) {
        //     const target_ptr = target_t.cast(.pointer);
        //     if (target_ptr.child_t.id() == bt.R8 or target_ptr.child_t.id() == bt.I8) {
        //         return Expr.initValue(.{
        //             .type = target_t,
        //             .value = try c.heap.newStrZ(str),
        //         });
        //     }
        // }

        if (target_t.id() == bt.EvalInt) {
            if (try sema.semaTryStringLitCodepoint(c, str, 64)) |val| {
                return Expr.initValue(.{
                    .type = c.sema.eval_int_t,
                    .value = val,
                });
            }
        }

        if (target_t.kind() == .int) {
            if (try sema.semaTryStringLitCodepoint(c, str, target_t.cast(.int).bits)) |val| {
                return Expr.initValue(.{
                    .type = target_t,
                    .value = val,
                });
            }
        } else if (target_t.kind() == .raw) {
            if (try sema.semaTryStringLitCodepoint(c, str, target_t.cast(.raw).bits)) |val| {
                return Expr.initValue(.{
                    .type = target_t,
                    .value = val,
                });
            }
        }
    }

    return Expr.initValue(.{
        .type = c.sema.eval_str_t,
        .value = try c.heap.init_eval_str(str),
    });
}

fn accessValue(c: *cy.Chunk, recv: cy.TypeValue, name: []const u8, name_n: *ast.Node) !Expr {
    if (recv.type.id() == bt.Type) {
        const type_ = recv.value.asPtr(*cy.Type);
        const sym = try c.getResolvedSymOrFail(@ptrCast(type_.sym()), name, name_n);
        try c.checkResolvedSymIsDistinct(sym, name_n);
        return Expr.initSym(sym);
    }
    switch (recv.type.kind()) {
        .struct_t => {
            const struct_t = recv.type.cast(.struct_t);
            const field = try getStructField(c, struct_t, name, name_n);
            const src = recv.value.asPtr([*]const u8);
            const res = try c.heap.copyValue3(field.type, src + field.offset);
            return Expr.initValue2(field.type, res);
        },
        .pointer => {
            const pointer = recv.type.cast(.pointer);
            if (pointer.ref) {
                if (pointer.child_t.kind() == .struct_t) {
                    const struct_t = pointer.child_t.cast(.struct_t);
                    const field = try getStructField(c, struct_t, name, name_n);
                    const res = try c.heap.copyValue3(field.type, recv.value.asPtr([*]const u8) + field.offset);
                    return Expr.initValue2(field.type, res);
                }
            }
        },
        .borrow => {
            const borrow = recv.type.cast(.borrow);
            if (borrow.child_t.kind() == .struct_t) {
                const struct_t = borrow.child_t.cast(.struct_t);
                const field = try getStructField(c, struct_t, name, name_n);
                const res = try c.heap.copyValue3(field.type, recv.value.asPtr([*]const u8) + field.offset);
                return Expr.initValue2(field.type, res);
            }
        },
        else => {}
    }
    return c.reportErrorFmt("Inline eval: Cannot access receiver. {}", &.{v(recv.type.kind())}, name_n);
}

pub fn callFuncSym(c: *cy.Chunk, sym: *cy.sym.FuncSym, pre_args: []const cy.sema_func.Argument, args: []*ast.Node, node: *ast.Node) !TypeValue {
    const res = try cy.sema_func.matchFuncSym2(c, sym, pre_args, args, true, node);
    if (res.resType != .ct_value) {
        return error.Unexpected;
    }
    return res.data.ct_value;
}

pub fn callFuncSymRec(c: *cy.Chunk, sym: *cy.sym.FuncSym, rec: *ast.Node, rec_res: sema.ExprResult,
    args: []const *ast.Node, node: *ast.Node) !TypeValue {
    const rec_arg = cy.sema_func.Argument.initReceiver(rec, rec_res);
    if (sym.first.type == .trait) {
        return error.TODO;
    } else {
        const res = try cy.sema_func.matchFuncSym2(c, sym, &.{rec_arg}, args, true, node);
        if (res.resType != .ct_value) {
            return error.Unexpected;
        }
        return res.data.ct_value;
    }
}

pub fn evalCZero(c: *cy.Chunk, type_: *cy.Type, node: *ast.Node) !Value {
    switch (type_.id()) {
        bt.Bool  => return Value.False,
        bt.R8,
        bt.I8  => return Value.initInt8(0),
        bt.R16,
        bt.I16 => return Value.initInt16(0),
        bt.R32,
        bt.F32,
        bt.I32   => return Value.initInt32(0),
        bt.R64,
        bt.I64,
        bt.F64 => return Value.initInt(0),
        else => {
            switch (type_.kind()) {
                .struct_t => {
                    const struct_t = type_.cast(.struct_t);
                    if (!struct_t.cstruct) {
                        const name = try c.sema.allocTypeName(type_);
                        defer c.alloc.free(name);
                        return c.reportErrorFmt("Unsupported zero initializer for `{}`. Can only zero initialize a `cstruct` type.", &.{v(name)}, node);
                    }

                    const fields = struct_t.fields();
                    const arg_start = c.valueStack.items.len;
                    defer {
                        for (c.valueStack.items[arg_start..], 0..) |arg, i| {
                            const field = fields[i];
                            c.heap.destructValue2(field.type, arg);
                        }
                        c.valueStack.items.len = arg_start;
                    }

                    for (fields) |field| {
                        const arg = try evalCZero(c, field.type, node);
                        try c.valueStack.append(c.alloc, arg);
                    }

                    const new = try c.heap.new_object_undef(struct_t.base.id(), struct_t.size);
                    const dst = new.object.getBytePtr();

                    for (fields, 0..) |field, i| {
                        const arg = c.valueStack.items[arg_start + i];
                        try c.heap.copyValueTo2(field.type, dst + field.offset, arg);
                    }
                    return Value.initPtr(new);
                },
                // .fixed_array => {
                //     const array_t = type_.cast(.fixed_array);
                //     const args = try c.ir.allocArray(*ir.Expr, array_t.n);
                //     for (0..array_t.n) |i| {
                //         const arg = try semaCZero(c, array_t.elem_t, node);
                //         args[i] = arg.ir;
                //     }
                //     return semaFixedArray(c, type_, args, node);
                // },
                // .pointer => {
                //     const pointer = type_.cast(.pointer);
                //     if (!pointer.ref) {
                //         return c.semaConst(0, type_, node);
                //     }
                // },
                .func_ptr => {
                    const func_ptr = type_.cast(.func_ptr);
                    if (func_ptr.sig.extern_) {
                        return Value.initInt(0);
                    }
                },
                else => {},
            }
            const name = try c.sema.allocTypeName(type_);
            defer c.alloc.free(name);
            return c.reportErrorFmt("Unsupported zero initializer for `{}`.", &.{v(name)}, node);
        },
    }
}

fn evalSignedLiteral(c: *cy.Chunk, literal: []const u8, target_t: *cy.Type, base: u8, node: *ast.Node) !?Expr {
    switch (target_t.id()) {
        bt.R8 => {
            const val = try sema.semaParseInt(c, u8, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.r8_t,
                .value = Value.initR8(val),
            });
        },
        bt.I8 => {
            const val = try sema.semaParseInt(c, i8, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.i8_t,
                .value = Value.initInt8(val),
            });
        },
        bt.R16 => {
            const val = try sema.semaParseInt(c, u16, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.r16_t,
                .value = Value.initR16(val),
            });
        },
        bt.I16 => {
            const val = try sema.semaParseInt(c, i16, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.i16_t,
                .value = Value.initInt16(val),
            });
        },
        bt.R32 => {
            const val = try sema.semaParseInt(c, u32, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.r32_t,
                .value = Value.initR32(val),
            });
        },
        bt.I32 => {
            const val = try sema.semaParseInt(c, i32, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.i32_t,
                .value = Value.initInt32(val),
            });
        },
        bt.EvalInt => {
            const val = try sema.semaParseInt(c, u64, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.eval_int_t,
                .value = Value.initGenericInt(@bitCast(val)),
            });
        },
        bt.F32 => {
            const val = try std.fmt.parseFloat(f32, literal);
            return Expr.initValue(.{
                .type = c.sema.f32_t,
                .value = Value.initFloat32(val),
            });
        },
        bt.F64 => {
            const val = try std.fmt.parseFloat(f64, literal);
            return Expr.initValue(.{
                .type = c.sema.f64_t,
                .value = Value.initFloat64(val),
            });
        },
        bt.R64 => {
            const val = try sema.semaParseInt(c, u64, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.r64_t,
                .value = Value.initR64(val),
            });
        },
        else => {
            return null;
        },
    }
}

fn evalUnsignedLiteral(c: *cy.Chunk, literal: []const u8, target_t: *cy.Type, base: u8, node: *ast.Node) !?Expr {
    switch (target_t.id()) {
        bt.R8 => {
            const val = try sema.semaParseInt(c, u8, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.r8_t,
                .value = Value.initR8(val),
            });
        },
        bt.I8 => {
            const val = try sema.semaParseInt(c, u8, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.i8_t,
                .value = Value.initR8(val),
            });
        },
        bt.R16 => {
            const val = try sema.semaParseInt(c, u16, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.r16_t,
                .value = Value.initR16(val),
            });
        },
        bt.I16 => {
            const val = try sema.semaParseInt(c, u16, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.i16_t,
                .value = Value.initR16(val),
            });
        },
        bt.R32 => {
            const val = try sema.semaParseInt(c, u32, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.r32_t,
                .value = Value.initR32(val),
            });
        },
        bt.I32 => {
            const val = try sema.semaParseInt(c, u32, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.i32_t,
                .value = Value.initR32(val),
            });
        },
        bt.EvalInt => {
            const val = try sema.semaParseInt(c, u64, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.eval_int_t,
                .value = Value.initGenericInt(@bitCast(val)),
            });
        },
        bt.F32 => {
            if (base != 10) {
                return null;
            }
            const val = try sema.semaParseFloat(c, f32, literal, node);
            return Expr.initValue(.{
                .type = c.sema.f32_t,
                .value = Value.initFloat32(val),
            });
        },
        bt.F64 => {
            if (base != 10) {
                return null;
            }
            const val = try sema.semaParseFloat(c, f64, literal, node);
            return Expr.initValue(.{
                .type = c.sema.f64_t,
                .value = Value.initFloat64(val),
            });
        },
        bt.R64 => {
            const val = try sema.semaParseInt(c, u64, literal, base, node);
            return Expr.initValue(.{
                .type = c.sema.r64_t,
                .value = Value.initR64(val),
            });
        },
        else => {
            return null;
        },
    }
}