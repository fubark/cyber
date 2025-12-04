const std = @import("std");
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const ast = cy.ast;
const sema = cy.sema;
const cte = cy.cte;
const v = cy.fmt.v;
const Value = cy.Value;
const bt = cy.types.BuiltinTypes;
const ir = cy.ir;

pub fn value(c: *cy.Chunk, val_: cy.TypeValue, target_opt: ?*cy.Type, node: *ast.Node) !sema.ExprResult {
    var val = val_;
    if (target_opt) |target_t| {
        if (target_t.id() != val_.type.id()) {
            if (try cte.evalFitTarget(c, val, target_t, node)) |new| {
                val = cy.TypeValue.init(target_t, new);
            }
        }
    }
    switch (val.type.id()) {
        bt.Void => {
            return c.semaVoid(node);
        },
        bt.Bool => {
            if (val.value.isTrue()) {
                return c.semaTrue(node);
            } else {
                return c.semaFalse(node);
            }
        },
        bt.R8,
        bt.I8 => {
            return c.semaConst8(val.value.asU8(), val.type, node);
        },
        bt.R16,
        bt.I16 => {
            return c.semaConst16(val.value.asU16(), val.type, node);
        },
        bt.R32,
        bt.F32,
        bt.I32 => {
            return c.semaConst32(val.value.asU32(), val.type, node);
        },
        bt.R64,
        bt.F64,
        bt.I64 => {
            return c.semaConst(val.value.asUint(), val.type, node);
        },
        bt.IntLit => {
            if (target_opt) |target_t| {
                const i = val.value.as_int_lit();
                switch (target_t.id()) {
                    bt.R8 => {
                        if (i >> 8 == 0) {
                            const u: u64 = @bitCast(i);
                            return c.semaConst8(@truncate(u), c.sema.r8_t, node);
                        }
                    },
                    bt.R16 => {
                        if (i >> 16 == 0) {
                            const u: u64 = @bitCast(i);
                            return c.semaConst16(@truncate(u), c.sema.r16_t, node);
                        }
                    },
                    bt.R32 => {
                        if (i >> 32 == 0) {
                            const u: u64 = @bitCast(i);
                            return c.semaConst32(@truncate(u), c.sema.r32_t, node);
                        }
                    },
                    else => {},
                }
            }
            return c.semaConst(@bitCast(val.value.as_int_lit()), c.sema.i64_t, node);
        },
        bt.StrLit => {
            // Ensure ct string outlives sema.
            const slice = val.value.asPtr(*cy.heap.StrLit).slice();
            const dupe = try c.ir.alloc.dupe(u8, slice);

            if (target_opt) |target_t| {
                if (target_t.isPointer()) {
                    const target_ptr = target_t.cast(.pointer);
                    if (target_ptr.child_t.id() == bt.R8 or target_ptr.child_t.id() == bt.I8) {
                        return c.semaRawStringAs(dupe, target_t, node);
                    }
                }
            }

            return c.semaRawString(dupe, node);
        },
        bt.Str => {
            // Ensure ct string outlives sema.
            const dupe = try c.ir.alloc.dupe(u8, val.value.asString());

            // Already a raw string.
            return c.semaRawString(dupe, node);
        },
        bt.Symbol => {
            const id = val.value.asSymbol();
            return c.semaConst(id, c.sema.symbol_t, node);
        },
        else => {
            switch (val.type.kind()) {
                .partial_vector => {
                    const vector_t = val.type.cast(.partial_vector);
                    const len = val.value.asPtr(*usize).*;
                    const args = try c.ir.allocArray(*ir.Expr, len + 1);
                    args[0] = (try sema.semaConst(c, len, c.sema.i64_t, node)).ir;
                    const src = val.value.asPtr([*]const u8) + 8;

                    for (0..len) |i| {
                        const copy = try c.heap.copyValue3(vector_t.elem_t, src + i * vector_t.elem_t.size());
                        defer c.heap.destructValue2(vector_t.elem_t, copy);
                        const arg = try value(c, cy.TypeValue.init(vector_t.elem_t, copy), null, node);
                        args[i+1] = arg.ir;
                    }
                    return sema.semaPartialVector(c, val.type, args, node);
                },
                .vector => {
                    const vector_t = val.type.cast(.vector);
                    const args = try c.ir.allocArray(*ir.Expr, vector_t.n);
                    const src = val.value.asPtr([*]const u8);

                    for (0..vector_t.n) |i| {
                        const copy = try c.heap.copyValue3(vector_t.elem_t, src + i * vector_t.elem_t.size());
                        defer c.heap.destructValue2(vector_t.elem_t, copy);
                        const arg = try value(c, cy.TypeValue.init(vector_t.elem_t, copy), null, node);
                        args[i] = arg.ir;
                    }
                    return sema.semaVector(c, val.type, args, node);
                },
                .enum_t => {
                    return c.semaConst(val.value.val, val.type, node);
                },
                .option => {
                    const option = val.type.cast(.option);
                    if (option.zero_union) {
                        if (val.value.val == 0) {
                            return c.semaConst(0, val.type, node);
                        }
                    }
                },
                .borrow => {
                    // Only emit if address is 0.
                    if (val.value.val == 0) {
                        return c.semaConst(0, val.type, node);
                    }
                },
                .pointer => {
                    // Should be ok to allow pointer as const as long as pointers are forbidden in sandbox mode.
                    return c.semaConst(@intFromPtr(val.value.ptr), val.type, node);
                },
                .func_ptr => {
                    if (val.type.cast(.func_ptr).sig.extern_) {
                        if (val.value.val == 0) {
                            return c.semaConst(0, val.type, node);
                        }
                    }
                },
                .struct_t => {
                    // TODO: Only emit nested IR if type is determined to contain pointer values
                    //       or values that depend on heap memory.
                    //       Otherwise, it should simply read a copy from static data.
                    const struct_t = val.type.cast(.struct_t);
                    std.debug.assert(val.type.isResolved());

                    const fields = struct_t.fields();
                    const ir_args = try c.ir.allocArray(*ir.Expr, fields.len);
                    const src = val.value.asPtr([*]const u8);

                    for (fields, 0..) |field, i| {
                        const copy = try c.heap.copyValue3(field.type, src + field.offset);
                        defer c.heap.destructValue2(field.type, copy);
                    
                        const arg = try value(c, cy.TypeValue.init(field.type, copy), null, node);
                        ir_args[i] = arg.ir;
                    }

                    const res = try c.ir.newExpr(.init, @ptrCast(struct_t), node, .{
                        .nargs = @as(u8, @intCast(struct_t.fields_len)),
                        .args = ir_args.ptr,
                    });
                    return sema.ExprResult.init2(res);
                },
                else => {},
            }
            const type_n = try c.sema.allocTypeName(val.type);
            defer c.alloc.free(type_n);
            return c.reportErrorFmt("Unsupported expansion of compile-time value of type: `{}`.", &.{v(type_n)}, node);
        },
    }
}

pub fn evalTopStmts(c: *cy.Chunk, stmts: []const *ast.Node) !void {
    for (stmts) |stmt| {
        switch (stmt.type()) {
            .trait_decl,
            .struct_decl,
            .cstruct_decl,
            .cunion_decl,
            .passStmt,
            .const_decl,
            .global_decl,
            .custom_type_decl,
            .type_alias_decl,
            .template,
            .enumDecl,
            .use_alias,
            .import_stmt,
            .funcDecl => {
                // Nop.
            },
            .ct_stmt => {
                const ct_stmt = stmt.cast(.ct_stmt);
                try evalStmt(c, ct_stmt);
            },
            else => {
                if (c.compiler.main_chunk != c) {
                    return c.reportError("Only compile-time statements are allowed in this context.", stmt);
                }
            },
        }
    }
}

pub fn evalStmts(c: *cy.Chunk, stmts: []const *ast.Node) anyerror!void {
    const ctx = sema.getResolveContext(c);
    if (ctx.allow_rt_stmts) {
        // Function body evaluation.
        for (stmts) |stmt| {
            switch (stmt.type()) {
                .trait_decl,
                .struct_decl,
                .cstruct_decl,
                .cunion_decl,
                .const_decl,
                .global_decl,
                .custom_type_decl,
                .type_alias_decl,
                .use_alias,
                .import_stmt,
                .template,
                .enumDecl,
                .funcDecl => {
                    return c.reportError("Declaration statement is not allowed here.", stmt);
                },
                else => {
                    try sema.semaStmt(c, stmt);
                },
            }
        }
    } else {
        for (stmts) |stmt| {
            switch (stmt.type()) {
                .trait_decl,
                .struct_decl,
                .cstruct_decl,
                .cunion_decl,
                .passStmt,
                .const_decl,
                .global_decl,
                .custom_type_decl,
                .type_alias_decl,
                .template,
                .enumDecl,
                .use_alias,
                .funcDecl => {
                    // Nop.
                },
                .ct_stmt => {
                    const ct_stmt = stmt.cast(.ct_stmt);
                    try evalStmt(c, ct_stmt);
                },
                .import_stmt => {
                    try sema.declareUseImport(c, stmt.cast(.import_stmt));
                },
                else => {
                    return c.reportError("Only compile-time statements are allowed in this context.", stmt);
                },
            }
        }
    }
}

pub fn switchCaseInner(c: *cy.Chunk, case_stmt: *ast.CaseStmt, _: ?*anyopaque, _: ?*cy.Type) anyerror!void {
    try sema.semaStmts(c, case_stmt.body_data.block.slice());
}

fn forRangeInner(c: *cy.Chunk, for_range: *ast.ForRangeStmt, _: ?*anyopaque) anyerror!void {
    try sema.semaStmts(c, for_range.stmts);
}

pub fn evalStmt(c: *cy.Chunk, ct_stmt: *ast.ComptimeStmt) !void {
    switch (ct_stmt.child.type()) {
        .if_stmt => {
            try ifStmt(c, ct_stmt.child.cast(.if_stmt));
        },
        .switch_stmt => {
            try cte.switchStmt(c, ct_stmt.child.cast(.switch_stmt), true, switchCaseInner, null, null);
        },
        .var_decl => {
            try cte.localDecl(c, ct_stmt.child.cast(.var_decl));
        },
        .for_range_stmt => {
            try cte.forRangeStmt(c, ct_stmt.child.cast(.for_range_stmt), forRangeInner, null);
        },
        .exprStmt => {
            const expr_stmt = ct_stmt.child.cast(.exprStmt);
            const val = try cte.eval(c, expr_stmt.child);
            defer c.heap.destructValue(val);
        },
        else => {
            return c.reportErrorFmt("Unsupported compile-time statement. {}", &.{v(ct_stmt.child.type())}, @ptrCast(ct_stmt));
        },
    }
}

pub fn ifStmt(c: *cy.Chunk, if_stmt: *ast.IfStmt) !void {
    var cond_res = try cte.evalCheck(c, if_stmt.cond, c.sema.bool_t);
    if (cond_res.value.asBool()) {
        const start = try pushCtBlock(c, @ptrCast(if_stmt));
        const res = try evalStmts(c, if_stmt.stmts);
        _ = res;
        try popInlineCtBlock(c, start);
        return;
    } else {
        for (if_stmt.else_blocks) |ct_else| {
            if (ct_else.type() != .ct_stmt) {
                return c.reportErrorFmt("Expected compile-time `else` statement.", &.{}, ct_else);
            }
            const else_b = ct_else.cast(.ct_stmt).child;
            if (else_b.type() == .elseif_block) {
                const else_ = else_b.cast(.elseif_block);
                cond_res = try cte.evalCheck(c, else_.cond, c.sema.bool_t);
                if (!cond_res.value.asBool()) {
                    continue;
                }
                const start = try pushCtBlock(c, @ptrCast(else_b));
                try sema.semaStmts(c, else_.stmts);
                try popInlineCtBlock(c, start);
            } else {
                const else_ = else_b.cast(.else_block);
                const start = try pushCtBlock(c, @ptrCast(else_b));
                try sema.semaStmts(c, else_.stmts);
                try popInlineCtBlock(c, start);
            }
            return;
        }
    }
}

fn pushCtBlock(c: *cy.Chunk, node: *ast.Node) !usize {
    _ = node;
    const ctx = sema.getResolveContext(c);
    return ctx.pushCtBlock();
}

fn popInlineCtBlock(c: *cy.Chunk, local_start: usize) !void {
    const ctx = sema.getResolveContext(c);
    ctx.popCtBlock(c, local_start);
}

    // } else if (std.mem.eql(u8, "dumpLocals", name)) {
    //     const proc = c.proc();
    //     try c.dumpLocals(proc);
    // } else if (std.mem.eql(u8, "dumpBytecode", name)) {
    //     _ = try c.ir.pushStmt(c.alloc, .dumpBytecode, @ptrCast(node), {});
    // } else if (std.mem.eql(u8, "verbose", name)) {
    //     C.setVerbose(true);
    //     _ = try c.ir.pushStmt(c.alloc, .verbose, @ptrCast(node), .{ .verbose = true });
    // } else if (std.mem.eql(u8, "verboseOff", name)) {
    //     C.setVerbose(false);
    //     _ = try c.ir.pushStmt(c.alloc, .verbose, @ptrCast(node), .{ .verbose = false });
    // } else if (std.mem.eql(u8, "err", name)) {
    //     if (call.args.len != 1) {
    //         return c.reportErrorFmt("Expected 1 argument.", &.{}, @ptrCast(node));
    //     }
    //     const val = try cte.eval(c, call.args[0]);
    //     defer c.vm.destructValue(val);
    //     var buf: std.ArrayListUnmanaged(u8) = .{};
    //     defer buf.deinit(c.alloc);
    //     _ = try c.vm.writeValue(buf.writer(c.alloc), val.type.id(), val.value, false);
    //     return c.reportErrorFmt("{}", &.{v(buf.items)}, @ptrCast(node));
    // }
