const std = @import("std");
const cy = @import("cyber.zig");
const ir = cy.ir;

/// Traverse main and mark visited functions.
pub fn performDCE(c: *cy.Compiler) !void {

    // First disable generation for all functions and reset visited flag.
    for (c.chunks.items) |chunk| {
        for (chunk.funcs.items) |func| {
            func.info.gen = false;
            func.info.visited = false;
        }
        for (chunk.deferred_funcs.items) |func| {
            func.info.gen = false;
            func.info.visited = false;
        }
    }

    // Visit from main and re-enable used functions.
    const Context = struct {
        alloc: std.mem.Allocator,
        stack: std.ArrayListUnmanaged(*ir.Stmt),
    };
    const S = struct {
        fn visitStmt(ctx: *ir.VisitContext, stmt: *ir.Stmt) !bool {
            _ = ctx;
            _ = stmt;
            return true;
        }

        fn visitFunc(ctx_: *ir.VisitContext, func: *cy.Func) !void {
            if (func.info.visited) {
                return;
            }
            func.info.visited = true;
            func.info.gen = true;

            const ctx: *Context = @ptrCast(@alignCast(ctx_.ctx));
            switch (func.type) {
                .userFunc => {
                    try ctx.stack.append(ctx.alloc, func.data.userFunc.loc);
                },
                .userLambda => {
                    try ctx.stack.append(ctx.alloc, func.data.userLambda.func_block_ir);
                },
                else => {},
            }
        }

        fn visitExpr(ctx_: *ir.VisitContext, expr: *ir.Expr) !bool {
            switch (expr.code) {
                .func_ptr => {
                    const data = expr.cast(.func_ptr);
                    try visitFunc(ctx_, data.func);
                },
                .call => {
                    const data = expr.cast(.call);
                    try visitFunc(ctx_, data.func);
                },
                else => {},
            }
            return true;
        }
    };
    var ctx = Context{ .alloc = c.alloc, .stack = .{} };
    var visit_ctx = ir.VisitContext{
        .ctx = &ctx,
        .visit_stmt = S.visitStmt,
        .visit_expr = S.visitExpr,
    };
    defer ctx.stack.deinit(c.alloc);

    // Visit main.
    try ctx.stack.append(c.alloc, c.main_ir);

    // Visit all destructors that were instantiated.
    for (c.sema.deinit_obj_tmpl.variants.items) |variant| {
        const func = variant.data.func.func;
        func.info.visited = true;
        func.info.gen = true;
        try ctx.stack.append(c.alloc, func.data.userFunc.loc);
    }
    for (c.sema.dtor_fn.variants.items) |variant| {
        const func = variant.data.func.func;
        func.info.visited = true;
        func.info.gen = true;
        try ctx.stack.append(c.alloc, func.data.userFunc.loc);
    }

    // Visit trait impls.
    for (c.sema.types.items) |type_| {
        if (type_.kind() == .struct_t) {
            const struct_t = type_.cast(.struct_t);
            for (struct_t.impls()) |impl| {
                for (impl.funcs) |func| {
                    func.info.visited = true;
                    func.info.gen = true;
                    try ctx.stack.append(c.alloc, func.data.userFunc.loc);
                }
            }
        }
    }

    while (ctx.stack.items.len > 0) {
        const head = ctx.stack.pop().?;
        _ = try ir.visitStmts(&visit_ctx, head);
    }
}