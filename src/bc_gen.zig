const std = @import("std");
const builtin = @import("builtin");
const cy = @import("cyber.zig");
const jitgen = @import("jit/gen.zig");
const log = cy.log.scoped(.bc_gen);
const ir = cy.ir;
const sema = cy.sema;
const cc = @import("capi.zig");
const types = cy.types;
const bt = types.BuiltinTypes;
const v = cy.fmt.v;
const vmc = cy.vmc;
const ast = cy.ast;
const bc = @This();
const Root = @This();

pub const Reg = u16;
const TypeId = types.TypeId;

const Chunk = cy.chunk.Chunk;

const emit_nop = true;

pub const VtableKey = packed struct {
    type: cy.TypeId,
    trait: cy.TypeId,
};

const Intrinsic = enum {
    null,
    dtor_str,
    cmp_str,
};

pub fn genAll(c: *cy.Compiler) !void {
    // Prepare types.
    for (c.newTypes(), c.type_start..) |stype, typeId| {
        _ = typeId;
    
        if (stype.kind() == .null) {
            // Skip placeholders.
            continue;
        }
        log.tracev("prep type: {s}", .{stype.name()});

        switch (stype.kind()) {
            .eval_ref,
            .eval_int,
            .c_variadic,
            .c_union,
            .never,
            .generic_vector,
            .generic,
            .generic_trait,
            .struct_t,
            .dyn_trait,
            .borrow_trait,
            .ref_trait,
            .void,
            .bool,
            .raw,
            .int,
            .float,
            .partial_vector,
            .vector,
            .option,
            .result,
            .func,
            .func_sym,
            .func_ptr,
            .choice,
            .pointer,
            .borrow,
            .ex_borrow,
            .enum_t => {},
            .null,
            .bare => {
                return error.Unexpected;
            },
        }
    }

    // Register intrinsics.
    c.sema.str_t.dtor.?.info.gen_intrinsic = @intFromEnum(Intrinsic.dtor_str);
    (try sema.getEqFunc(c.main_chunk, c.sema.str_t, null)).info.gen_intrinsic = @intFromEnum(Intrinsic.cmp_str);

    for (c.newChunks()) |chunk| {
        log.tracev("prep chunk", .{});

        // Perform big allocation for instruction buffer for more consistent heap allocation.
        try chunk.buf.ops.ensureTotalCapacityPrecise(c.alloc, 4096 / 2);

        for (chunk.syms.items) |sym| {
            try prepareSym(c, sym);
        }
        for (chunk.funcs.items) |func| {
            try prepareFunc(c, func);
        }
    }

    for (c.newChunks()) |chunk| {
        for (chunk.syms.items) |sym| {
            switch (sym.type) {
                .type => {
                    const type_sym = sym.cast(.type);
                    if (type_sym.type.kind() == .struct_t) {
                        // rt funcs have been reserved. Create impl vtables.
                        const struct_t = type_sym.type.cast(.struct_t);
                        for (struct_t.impls()) |impl| {
                            const vtable = try c.alloc.alloc(u64, impl.funcs.len);
                            const vtable_idx = c.vm.vtables.items.len;
                            try c.vm.vtables.append(c.alloc, vtable);
                            try c.gen_vtables.put(c.alloc, VtableKey{ .type = struct_t.base.id(), .trait = impl.trait.base.id() }, @intCast(vtable_idx));
                        }
                    }
                },
                else => {},
            }
        }
    }

    for (c.newChunks()) |chunk| {
        log.tracev("Perform codegen for chunk{}: {s}", .{chunk.id, chunk.srcUri});

        try sema.pushChunkResolveContext(chunk, @ptrCast(chunk.ast.root));
        defer sema.popResolveContext(chunk);

        try genChunk(chunk);
        log.tracev("Done. performChunkCodegen {s}", .{chunk.srcUri});
    }

    // Perform static relocations.
    for (c.newChunks()) |chunk| {
        for (chunk.relocations.items) |rel| {
            const ptr: *align(2) [6]u8 = @ptrCast(&chunk.buf.ops.items[rel.pc]);
            switch (rel.kind) {
                .extern_func_ptr => {
                    return error.Unexpected;
                },
                .func => {
                    const addr: u48 = @intCast(@intFromPtr(c.genSymMap.get(rel.data.func).?.func.static_pc));
                    std.mem.writeInt(u48, ptr, addr, .little);
                },
                .global => {
                    const addr: u48 = @intCast(@intFromPtr(c.vm.globals.items[rel.data.global].value.ptr));
                    std.mem.writeInt(u48, ptr, addr, .little);
                },
            }
        }

        for (chunk.syms.items) |sym| {
            switch (sym.type) {
                .type => {
                    const type_sym = sym.cast(.type);
                    if (type_sym.type.kind() == .struct_t) {
                        const struct_t = type_sym.type.cast(.struct_t);
                        for (struct_t.impls()) |impl| {
                            const vtable_idx = c.gen_vtables.get(VtableKey{ .type = struct_t.base.id(), .trait = impl.trait.base.id() }).?;
                            const vtable = c.vm.vtables.items[vtable_idx];
                            for (impl.funcs, 0..) |func, i| {
                                var ptr: u64 = @intFromPtr(&chunk.buf.ops.items[c.genSymMap.get(func).?.func.pc]);
                                if (func.type == .hostFunc) {
                                    ptr |= 1 << 63;
                                }
                                vtable[i] = ptr;
                            }
                        }
                    }
                },
                else => {},
            }
        }
    }

    // Perform JIT gen after BC gen.
    var jit_gen = false;
    for (c.newChunks()) |chunk| {
        if (chunk.jit_funcs.items.len > 0) {
            for (chunk.buf.labels.items) |bc_pc| {
                // Updated with jit buffer pos during `gen_funcs`.
                try c.jitBuf.labels.putNoClobber(c.alloc, .{.chunk = chunk, .bc_pc=bc_pc}, 0);
            }

            jit_gen = true;
            try jitgen.gen_funcs(chunk, chunk.jit_funcs.items);
        }
    }

    if (jit_gen) {
        try jitgen.relocate(&c.jitBuf);
        try c.jitBuf.set_executable();
    }

    // Update runtime type info.
    if (cy.Trace) {
        c.vm.c.rw_lock.write_lock();
        defer c.vm.c.rw_lock.write_unlock();

        if (c.vm.types.items.len == 0) {
            // Reserve null type.
            const name = try c.alloc.dupe(u8, "<null>");
            try c.vm.types.append(c.alloc, .{ .name_ptr = name.ptr, .name_len = name.len });
        }
        for (c.newTypes()) |type_| {
            const name = try c.sema.allocTypeName(type_);
            try c.vm.types.append(c.alloc, .{ .name_ptr = name.ptr, .name_len = name.len });
        }

        c.vm.c.types_ptr = @ptrCast(c.vm.types.items.ptr);
        c.vm.c.types_len = c.vm.types.items.len;
    }
}

fn prepareSym(c: *cy.Compiler, sym: *cy.Sym) !void {
    log.tracev("prep sym: {s}", .{sym.name()});
    switch (sym.type) {
        .hostVar => {
            const id = c.vm.globals.items.len;
            const global_sym = try c.alloc.create(cy.vm.GlobalSym);
            var val: []align(8) u8 = &.{};
            val.ptr = @ptrCast(@alignCast(sym.cast(.hostVar).val.?));
            global_sym.* = .init(val, sym);
            try c.vm.globals.append(c.alloc, global_sym);
            try c.genSymMap.putNoClobber(c.alloc, sym, .{ .varSym = .{ .id = @intCast(id) }});
        },
        .extern_var => {
            const id = c.vm.globals.items.len;
            const global_sym = try c.alloc.create(cy.vm.GlobalSym);
            // value ptr is relocated at runtime.
            global_sym.* = .init(&.{}, sym);
            try c.vm.globals.append(c.alloc, global_sym);
            try c.genSymMap.putNoClobber(c.alloc, sym, .{ .varSym = .{ .id = @intCast(id) }});
        },
        .userVar => {
            const id = c.vm.globals.items.len;
            const global_sym = try c.alloc.create(cy.vm.GlobalSym);
            const global_t = sym.getValueType().?;
            const value = try c.alloc.alignedAlloc(u8, .@"8", global_t.size());
            global_sym.* = .init(value, sym);
            try c.vm.globals.append(c.alloc, global_sym);
            try c.genSymMap.putNoClobber(c.alloc, sym, .{ .varSym = .{ .id = @intCast(id) }});
        },
        .func,
        .const_,
        .template,
        .func_template,
        .ct_value,
        .type,
        .type_const,
        .chunk,
        .field,
        .variant_func,
        .variant_member,
        .enum_case,
        .choice_case,
        .union_case,
        .type_alias,
        .use_alias,
        .module_alias => {},
        else => {
            log.tracev("{}", .{sym.type});
            return error.Unsupported;
        }
    }
}

pub fn prepareFunc(c: *cy.Compiler, func: *cy.Func) !void {
    switch (func.type) {
        .unresolved,
        .reserved,
        .vm_extern_variant,
        .host_builtin,
        .host_const_eval,
        .generic,
        .trait => return,
        .userLambda => {
            if (cy.Trace) {
                const symPath = try cy.sym.allocSymName(&c.sema, c.alloc, func.parent, .{});
                defer c.alloc.free(symPath);
                log.tracev("prep lambda in: {s}", .{symPath});
            }
            _ = try ensureFunc(c, func);
        },
        .hostFunc => {
            if (cy.Trace) {
                const symPath = try cy.sym.allocSymName(&c.sema, c.alloc, func.parent, .{});
                defer c.alloc.free(symPath);
                log.tracev("prep func: {s}", .{symPath});
            }
            const rtFunc = cy.vm.FuncSymbol.initHostFunc(@ptrCast(func.data.hostFunc.ptr));
            const id = try ensureFunc(c, func);
            try c.vm.host_funcs.put(c.alloc, @constCast(@ptrCast(func.data.hostFunc.ptr)), id);
            if (c.vm.funcSyms.items[id].type == .null) {
                completeFunc(c, id, func, rtFunc);
            }
        },
        .userFunc => {
            if (cy.Trace) {
                const symPath = try cy.sym.allocSymName(&c.sema, c.alloc, func.parent, .{});
                defer c.alloc.free(symPath);
                log.tracev("prep func: {s}", .{symPath});
            }
            _ = try ensureFunc(c, func);
            // Func is patched later once funcPc and stackSize is obtained.
        },
        .extern_ => {
            _ = try ensureExternFunc(c, func);
        },
    }
}

pub const ExternFunc = struct {
    id: u32,
    vm_id: u32,

    // For externs with user implementation.
    vm_pc: u32,
};

pub fn ensureExternFunc(c: *cy.Compiler, func: *cy.Func) !ExternFunc {
    const res = try c.genSymMap.getOrPut(c.alloc, func);
    if (!res.found_existing) {
        const id = try c.vm.addFunc(func, func.name(), func.sig, cy.vm.FuncSymbol.initNull());
        const vm_id = try c.vm.addFunc(func, func.name(), func.sig, cy.vm.FuncSymbol.initNull());
        res.value_ptr.* = .{ .extern_func = .{ .id = id, .vm_id = vm_id, .vm_pc = cy.NullId }};
    }
    return res.value_ptr.extern_func;
}

pub fn ensureFunc(c: *cy.Compiler, func: *cy.Func) !u32 {
    const res = try c.genSymMap.getOrPut(c.alloc, func);
    if (!res.found_existing) {
        const id = try c.vm.addFunc(func, func.name(), func.sig, cy.vm.FuncSymbol.initNull());
        res.value_ptr.* = .{ .func = .{ .static_pc = undefined, .id = id, .pc = 0, .end_pc = 0 }};
    }
    return res.value_ptr.func.id;
}

pub fn ensureExternFuncPtrDispatch(c: *cy.Compiler, func_ptr: *cy.types.FuncPtr) !u32 {
    const res = try c.genSymMap.getOrPut(c.alloc, func_ptr);
    if (!res.found_existing) {
        const id = try c.vm.addFunc(null, "extern_func_ptr", func_ptr.sig, cy.vm.FuncSymbol.initNull());
        res.value_ptr.* = .{ .func = .{ .static_pc = undefined, .id = id, .pc = 0, .end_pc = 0 }};
    }
    return res.value_ptr.func.id;
}

pub fn completeFunc(c: *cy.Compiler, id: u32, func: *cy.Func, sym: cy.vm.FuncSymbol) void {
    log.tracev("complete func gen: {s} {}", .{func.name(), id});
    if (c.vm.funcSyms.items[id].type != .null) {
        std.debug.panic("Func already completed: {s} {}", .{func.name(), id});
    }
    c.vm.funcSyms.items[id] = sym;
}

fn genChunk(c: *Chunk) !void {
    genChunkInner(c) catch |err| {
        if (err != error.CompileError) {
            // Wrap all other errors as a CompileError.
            return c.reportErrorFmt("error.{}", &.{v(err)}, c.curNode);
        } else return err;
    };
}

fn genStmts(c: *Chunk, head: ?*ir.Stmt) !void {
    var stmt_opt = head;
    while (stmt_opt) |stmt| {
        try genStmt(c, stmt);
        stmt_opt = stmt.next;
    }
}

fn genStmt(c: *Chunk, stmt: *ir.Stmt) anyerror!void {
    const node = stmt.node;
    c.curNode = node;
    if (cy.Trace) {
        const contextStr = c.encoder.formatTrunc(node);
        if (cc.verbose()) {
            log.log("{f}| {s}: `{s}` reg_end={}", .{
                cy.fmt.Repeat{.s=" ", .count=c.indent * 4}, @tagName(stmt.code), contextStr,
                c.cur_proc.reg_end,
            });
        }
    }

    switch (stmt.code) {
        .await_             => try gen_await(c, stmt.cast(.await_), node),
        .block              => try gen_block(c, stmt.cast(.block), node),
        .break_             => try gen_break(c, stmt.cast(.break_), node),
        .breakpoint         => try gen_breakpoint(c, node),
        .continue_          => try gen_continue(c, stmt.cast(.continue_), node),
        .declare_local      => try gen_declare_local(c, stmt.cast(.declare_local), node),
        .discard            => try gen_discard(c, stmt.cast(.discard), node),
        .forRangeStmt       => try gen_for_range_stmt(c, stmt.cast(.forRangeStmt), node),
        .funcBlock          => return error.Unexpected,
        .if_block           => try gen_if_block(c, stmt.cast(.if_block), node),
        .loop_block         => try gen_loop_block(c, stmt.cast(.loop_block), node),
        .mainBlock          => return error.Unexpected,
        .nop_label          => try gen_nop_label(c, stmt.cast(.nop_label), node),
        .pop_locals         => try gen_pop_locals(c, stmt.cast(.pop_locals), node),
        .release            => try gen_release(c, stmt.cast(.release), node),
        .ret_expr           => try gen_ret_expr(c, stmt.cast(.ret_expr), node),
        .ret_gen            => try gen_ret_gen(c, stmt.cast(.ret_gen), node),
        .ret                => try gen_ret(c, stmt.cast(.ret), node),
        .setCaptured        => try gen_set_captured(c, stmt.cast(.setCaptured), node),
        .set_deref          => try gen_set_deref(c, stmt.cast(.set_deref), node),
        .set_field          => try gen_set_field(c, stmt.cast(.set_field), node),
        .set_global         => try gen_set_global(c, stmt.cast(.set_global), node),
        .set_local          => try gen_set_local(c, stmt.cast(.set_local), node),
        .set_ret            => try gen_set_ret(c, stmt.cast(.set_ret), node),
        .switch_stmt        => try gen_switch_stmt(c, stmt.cast(.switch_stmt), node),
        .trap               => try gen_trap(c, node),
        .tryStmt            => try gen_try_stmt(c, stmt.cast(.tryStmt), node),
        .verbose            => try verbose(c, stmt.cast(.verbose), node),
        .yield              => try gen_yield_stmt(c, stmt.cast(.yield), node),
        //         .dumpBytecode => {
        //             try cy.debug.dumpBytecode(c.compiler.vm, null);
        //         },
        else => {
            log.log("{}", .{stmt.code});
            return error.TODO;
        }
    }

    log.tracev("{f}| end {s} reg_end={}", .{
        cy.fmt.Repeat{.s=" ", .count=c.indent * 4}, @tagName(stmt.code),
        c.cur_proc.reg_end,
    });
}

fn genChunkInner(c: *Chunk) !void {
    c.listDataStack.clearRetainingCapacity();

    c.indent = 0;

    for (c.ir.func_blocks.items) |block| {
        if (block.code == .funcBlock) {
            try genFuncBlock(c, block.cast(.funcBlock), block.node);
        } else {
            try mainBlock(c, block.cast(.mainBlock), block.node);
        }
    }

    // Get stable func pcs after final buffer size.
    for (c.ir.func_blocks.items) |block| {
        if (block.code == .funcBlock) {
            const func = block.cast(.funcBlock).func;
            if (func.type == .extern_) {
                std.debug.assert(func.data.extern_.has_impl);
                const res = c.compiler.genSymMap.get(func).?;
                const id = res.extern_func.vm_id;
                const pc = res.extern_func.vm_pc;
                c.vm.funcSyms.items[id].data.func.pc = c.buf.ops.items.ptr + pc;
            } else {
                const res = c.compiler.genSymMap.getPtr(func).?;
                const id = res.func.id;
                res.func.static_pc = c.buf.ops.items.ptr + res.func.pc;
                c.vm.funcSyms.items[id].data.func.pc = c.buf.ops.items.ptr + res.func.pc;
            }
        }
    }

    // Insert debug table entry in order.
    const S = struct {
        fn compare(context: [*]cy.Inst, item: cy.DebugTableEntry) std.math.Order {
            return std.math.order(@intFromPtr(context), @intFromPtr(item.ptr));
        }
    };
    const idx = std.sort.lowerBound(cy.DebugTableEntry, c.compiler.debug_tables.items, c.buf.ops.items.ptr, S.compare);
    try c.compiler.debug_tables.insert(c.alloc, idx, .{
        .chunk = c,
        .ptr = c.buf.ops.items.ptr,
    });
}

fn genExpr(c: *Chunk, expr: *ir.Expr, cstr: Cstr) anyerror!GenValue {
    const node = expr.node;
    if (cy.Trace) {
        c.indent += 1;
        const contextStr = c.encoder.formatTrunc(node);
        if (cc.verbose()) {
            log.log("{f}( {s}: `{s}` {s} reg_end={}", .{
                cy.fmt.Repeat{.s=" ", .count=c.indent * 4}, @tagName(expr.code), contextStr, @tagName(cstr.type),
                c.cur_proc.reg_end,
            });
        }
    }

    const res = try switch (expr.code) {
        .add                => genBinOp2(c, expr.cast(.add), .add, cstr, node),
        .address_of         => gen_addr_of(c, expr.cast(.address_of), cstr, node),
        .and_op             => genBinOp2(c, expr.cast(.and_op), .and_, cstr, node),
        .binary_op          => genBinOp(c, expr.cast(.binary_op), cstr, node),
        .bitcast            => gen_bitcast(c, expr.cast(.bitcast), cstr, node),
        .call               => gen_call(c, expr.cast(.call), cstr, node),
        .call_ptr           => gen_call_ptr(c, expr.cast(.call_ptr), cstr, node),
        .call_union         => gen_call_union(c, expr.cast(.call_union), cstr, node),
        .call_trait         => gen_call_trait(c, expr.cast(.call_trait), cstr, node),
        .captured           => gen_captured(c, expr.cast(.captured), cstr, node),
        .case               => gen_case(c, expr.cast(.case), cstr, node),
        .cast               => gen_cast(c, expr.cast(.cast), cstr, node),
        .cmp                => genCompare(c, expr.cast(.cmp), cstr, node),
        .const8             => gen_const8(c, expr.cast(.const8), cstr, node),
        .const16            => gen_const16(c, expr.cast(.const16), cstr, node),
        .const32            => gen_const32(c, expr.cast(.const32), cstr, node),
        .const64            => gen_const64(c, expr.cast(.const64), cstr, node),
        .deref              => gen_deref(c, expr.cast(.deref), cstr, node),
        .div                => genBinOp2(c, expr.cast(.div), .div, cstr, node),
        .f2i                => gen_f2i(c, expr.cast(.f2i), cstr, node),
        .fabs               => gen_fabs(c, expr.cast(.fabs), cstr, node),
        .fadd               => gen_fadd(c, expr.cast(.fadd), cstr, node),
        .falsev             => gen_false(c, cstr, node),
        .fdiv               => gen_fdiv(c, expr.cast(.fdiv), cstr, node),
        .field              => gen_field(c, expr.cast(.field), cstr, node),
        .fmod               => gen_fmod(c, expr.cast(.fmod), cstr, node),
        .fmul               => gen_fmul(c, expr.cast(.fmul), cstr, node),
        .fneg               => gen_fneg(c, expr.cast(.fneg), cstr, node),
        .fsub               => gen_fsub(c, expr.cast(.fsub), cstr, node),
        .func_ptr           => gen_funcptr(c, expr.cast(.func_ptr), cstr, node),
        .func               => genFuncUnion(c, expr.cast(.func), cstr, node),
        .gen_end            => gen_gen_end(c, expr.cast(.gen_end), cstr, node),
        .gen_next           => gen_gen_next(c, expr.cast(.gen_next), cstr, node),
        .global             => gen_global(c, expr.cast(.global), cstr, node),
        .i2f                => gen_i2f(c, expr.cast(.i2f), cstr, node),
        .idiv               => genBinOp2(c, expr.cast(.idiv), .idiv, cstr, node),
        .imod               => genBinOp2(c, expr.cast(.imod), .imod, cstr, node),
        .imul               => genBinOp2(c, expr.cast(.imul), .imul, cstr, node),
        .init               => gen_init(c, expr.cast(.init), cstr, node),
        .init_case          => gen_init_case(c, expr.cast(.init_case), cstr, node),
        .init_zero          => gen_init_zero(c, expr.cast(.init_zero), cstr, node),
        .is_zero            => genIsZero(c, expr.cast(.is_zero), cstr, node),
        .lambda             => gen_lambda(c, expr.cast(.lambda), cstr, node),
        .lift               => gen_lift(c, expr.cast(.lift), cstr, node),
        .local              => gen_local(c, expr.cast(.local), cstr, node),
        .lsl                => genBinOp2(c, expr.cast(.lsl), .lsl, cstr, node),
        .lsr                => genBinOp2(c, expr.cast(.lsr), .lsr, cstr, node),
        .mod                => genBinOp2(c, expr.cast(.mod), .mod, cstr, node),
        .mul                => genBinOp2(c, expr.cast(.mul), .mul, cstr, node),
        .neg                => genUnOp2(c, expr.cast(.neg), .neg, cstr, node),
        .not                => genUnOp2(c, expr.cast(.not), .not, cstr, node),
        .or_op              => genBinOp2(c, expr.cast(.or_op), .or_, cstr, node),
        .retain             => gen_retain(c, expr.cast(.retain), cstr, node),
        .sext               => gen_sext(c, expr.cast(.sext), cstr, node),
        .string             => gen_string(c, expr.cast(.string), cstr, node),
        .sub                => genBinOp2(c, expr.cast(.sub), .sub, cstr, node),
        .trait              => gen_trait(c, expr.cast(.trait), cstr, node),
        .truev              => gen_true(c, cstr, node),
        .trunc              => gen_trunc(c, expr.cast(.trunc), cstr, node),
        .unary_op           => genUnOp(c, expr.cast(.unary_op), cstr, node),
        .undef              => gen_undef(c, expr.cast(.undef), cstr, node),
        .unwrap_addr        => genUnwrapAddr(c, expr.cast(.unwrap_addr), cstr, node),
        .unwrap_nz          => genUnwrapNZ(c, expr.cast(.unwrap_nz), cstr, node),
        .voidv              => gen_void(c, cstr, node),
        .xor                => genBinOp2(c, expr.cast(.xor), .xor, cstr, node),
        .zext               => gen_zext(c, expr.cast(.zext), cstr, node),
        else => {
            log.log("{}", .{expr.code});
            return error.TODO;
        }
    };
    if (cy.Trace) {
        if (cc.verbose()) {
            log.log("{f}) end {s} reg_end={}", .{
                cy.fmt.Repeat{.s=" ", .count=c.indent * 4}, @tagName(expr.code),
                c.cur_proc.reg_end,
            });
        }
        c.indent -= 1;
    }
    return res;
}

fn gen_release(c: *Chunk, stmt: *ir.Release, node: *ast.Node) !void {
    const reg = toLocalReg(c, @intCast(stmt.reg));
    try pushRelease(c, reg, stmt.optional, stmt.deinit_obj, node);
}

fn gen_trap(c: *Chunk, node: *ast.Node) !void {
    try c.pushFCode(.trap, &.{}, node);
}

fn gen_nop_label(c: *Chunk, stmt: *ir.NopLabel, node: *ast.Node) !void {
    if (!emit_nop) {
        return;
    }
    const label = c.ir.buf.items[stmt.label_idx..stmt.label_idx + stmt.label_len];
    log.tracev("nop: {s}", .{label});
    try genNopLabel2(c, label, node);
}

fn genNopLabel2(c: *Chunk, label: []const u8, node: *ast.Node) !void {
    const res = try c.buf.getOrPushConstString(label);
    const ptr_len = (@as(u64, @intFromPtr(res.slice.ptr)) & 0xffffffffffff) | (@as(u64, res.slice.len) << 48);
    const pc = c.buf.ops.items.len;
    try c.pushCode(.nops, &.{ 0, 0, 0, 0 }, node);
    c.buf.setOpArgU64(pc + 1, ptr_len);
}

fn mainBlock(c: *Chunk, stmt: *ir.MainBlock, node: *ast.Node) !void {
    log.tracev("main block:", .{});

    try pushProc(c, .main, c.id, null, "main", node);

    try reserveMainRegs(c);

    const main_pc = c.buf.ops.items.len;

    var child_opt = stmt.bodyHead;
    while (child_opt) |child| {
        try genStmt(c, child);
        child_opt = child.next;
    }

    try mainEnd(c, node);
    try popProc(c);

    c.buf.mainStackSize = c.getMaxUsedRegisters();
    c.buf.main_pc = @intCast(main_pc);
}

pub fn genFuncBlock(c: *Chunk, stmt: *ir.FuncBlock, node: *ast.Node) !void {
    const func = stmt.func;

    // Skip if marked as skip (usually for one-off compile-time functions)
    if (stmt.skip) {
        return;
    }

    // Reserve a nop32 inst to hold a function id to access debug info.
    const header_pc = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.nop32, &.{0, 0});

    const pc = c.buf.ops.items.len;
    try c.buf.markers.put(c.alloc, @intCast(pc), func);

    if (func.info.jit) {
        try c.pushCode(.enter_jit, &.{0, 0, 0}, node);
        try c.compiler.jitBuf.relocs.append(c.alloc, .{ .type = .jit_entry, .data = .{ .jit_entry = .{
            .chunk = c,
            .bc_pc = @intCast(pc + 1),
            .func = func,
        }}});
        try c.jit_funcs.append(c.alloc, func);
    }

    var id: u32 = undefined;
    if (func.type == .extern_) {
        std.debug.assert(func.data.extern_.has_impl);
        const entry = try ensureExternFunc(c.compiler, func);
        id = entry.vm_id;
        c.compiler.genSymMap.getPtr(func).?.extern_func.vm_pc = @intCast(pc);
    } else {
        id = try ensureFunc(c.compiler, func);
        c.compiler.genSymMap.getPtr(func).?.func.pc = @intCast(pc);
    }
    c.buf.setOpArgU32(header_pc + 1, id);
    c.vm.log_tracev("gen {s} {}", .{stmt.func.name(), id});
    
    std.debug.assert(c.vm.funcSyms.items[id].type == .null);

    const params = stmt.params[0..func.sig.params_len];

    try pushFuncBlock(c, stmt, params, node);

    const chk_stk_pc = c.buf.ops.items.len;
    try c.buf.func_table.append(c.alloc, chk_stk_pc);
    try c.pushFCode(.chk_stk, &.{ 0, 0 }, node);

    try genStmts(c, stmt.bodyHead);

    c.buf.ops.items[chk_stk_pc + 1].val = c.cur_proc.ret_size;

    const stack_size = c.getMaxUsedRegisters();
    c.buf.ops.items[chk_stk_pc + 2].val = stack_size;

    if (func.type != .extern_) {
        c.compiler.genSymMap.getPtr(func).?.func.end_pc = @intCast(c.buf.ops.items.len);
    }

    const rt_func = cy.vm.FuncSymbol.initFunc(undefined);
    completeFunc(c.compiler, id, func, rt_func);
    try popFuncBlockCommon(c, func);
}

fn gen_breakpoint(c: *Chunk, node: *ast.Node) !void {
    try c.pushFCode(.trap_debug, &.{}, node);
}

fn gen_await(c: *Chunk, stmt: *ir.Await, node: *ast.Node) !void {
    const childv = try genExpr(c, stmt.expr, Cstr.localOrTemp());
    try c.pushCode(.await_op, &.{childv.reg}, node);
    try pop_temp_value(c, childv, node);
}

fn gen_yield_stmt(c: *Chunk, stmt: *ir.Yield, node: *ast.Node) !void {
    const pc = c.buf.ops.items.len;
    try c.pushCode(.ret_y, &.{@intCast(stmt.ret_opt_t.reg_size()), 0, @intFromBool(stmt.end)}, node);
    try genStmts(c, stmt.deinit_head);
    c.buf.ops.items[pc + 2].val = @intCast(c.buf.ops.items.len - pc);
}

fn gen_gen_next(c: *Chunk, expr: *ir.GenNext, cstr: Cstr, node: *ast.Node) !GenValue {
    // Assumes expr.base.type is the option next type.
    const inst = try beginCall(c, cstr, expr.base.type, false, node);
    const gen = try genExpr(c, expr.gen, Cstr.localOrTemp());
    try c.pushCode(.gen_next, &.{inst.ret.reg, inst.base, gen.reg}, node);
    try pop_temp_value(c, gen, node);
    return endCall(c, inst, expr.base.type, node);
}

fn gen_gen_end(c: *Chunk, expr: *ir.GenEnd, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    const gen_t = expr.gen.type.cast(.pointer).child_t;
    const ret_t = gen_t.sym().instance.?.params[0].asPtr(*cy.Type).cast(.func_ptr).sig.ret;
    const next_t = try cy.sema.getOptionType(c, ret_t);

    const gen = try genExpr(c, expr.gen, Cstr.localOrTemp());
    const ret = c.cur_proc.reg_end;
    try c.pushCode(.gen_end, &.{ret, @intCast(ret + next_t.reg_size()), gen.reg}, node);
    try pop_temp_value(c, gen, node);
    return GenValue.discard();
}

fn gen_cast(c: *Chunk, expr: *ir.Cast, cstr: Cstr, node: *ast.Node) !GenValue {
    if (!expr.isRtCast) {
        return genExpr(c, expr.expr, cstr);
    }

    const dst = try bc.selectReg(c, expr.base.type, cstr, node);

    // TODO: If inst.dst is a temp, this should have a cstr of localOrExact.
    const childv = try genExpr(c, expr.expr, Cstr.localOrTemp());

    if (types.toRtConcreteType(expr.type)) |type_| {
        const shape_t = type_.pointeeOrSelf();
        try c.pushFCode(.cast, &.{ dst.reg, childv.reg, @intCast(shape_t.id()), @intFromBool(expr.type.isRefPointer()) }, node);
    } else {
        // Cast to abstract type.
        try c.pushFCode(.castAbstract, &.{ dst.reg, childv.reg, @intCast(expr.type.id()) }, node);
    }

    try pop_temp_value(c, childv, node);
    return dst;
} 

fn gen_addr_of(c: *Chunk, expr: *ir.AddressOf, cstr: Cstr, node: *ast.Node) !GenValue {
    return genAddressOf2(c, expr.expr, cstr, node);
}

fn gen_addr_global(c: *Chunk, global: *cy.Sym, cstr: Cstr, node: *ast.Node) !GenValue {
    const global_id = c.compiler.genSymMap.get(global).?.varSym.id;

    const dst = try selectReg(c, c.sema.ptr_void_t, cstr, node);
    if (cy.Trace) {
        var buf: [128]u8 = undefined;
        const label = try std.fmt.bufPrint(&buf, "&global {s}", .{global.name()});
        try genNopLabel2(c, label, node);
    }
    const pc = c.buf.len();
    try c.pushCode(.const_64, &.{dst.reg, 0, 0, 0, 0}, node);
    if (global.type == .extern_var) {
        try c.rt_relocations.append(c.alloc, .{ .kind = .global, .pc = pc + 2, .data = .{ .global = global_id } });
    } else {
        try c.relocations.append(c.alloc, .{ .kind = .global, .pc = pc + 2, .data = .{ .global = global_id } });
    }
    return dst;
}

fn push_addr(c: *Chunk, dst: Reg, reg: Reg, node: *ast.Node) !void {
    try c.pushCode(.addr, &.{ dst, reg }, node);
}

fn genAddressOf2(c: *Chunk, expr: *ir.Expr, cstr: Cstr, node: *ast.Node) !GenValue {
    switch (expr.code) {
        .global => {
            const data = expr.cast(.global);
            return gen_addr_global(c, data.sym, cstr, node);
        },
        .local => {
            const data = expr.cast(.local);
            const reg = toLocalReg(c, data.id);

            const dst = try selectReg(c, c.sema.ptr_void_t, cstr, node);
            try push_addr(c, dst.reg, reg, node);
            return dst;
        },
        .case => {
            const data = expr.cast(.case);
            return genAddressOfField(c, data.child.cast(.field), cstr, node);
        },
        .field => {
            const data = expr.cast(.field);
            return genAddressOfField(c, data, cstr, node);
        },
        .deref => {
            const deref = expr.cast(.deref);
            return genExpr(c, deref.expr, cstr);
        },
        .call => {
            if (expr.type.isPointer()) {
                return gen_call(c, expr.cast(.call), cstr, node);
            }
            return c.reportError("TODO", node);
        },
        .bitcast => {
            return genAddressOf2(c, expr.cast(.bitcast).expr, cstr, node);
        },
        else => {
            return c.reportErrorFmt("TODO: {}", &.{v(expr.code)}, node);
        },
    }
}

fn genAddressOfField(c: *cy.Chunk, data: *ir.Field, cstr: Cstr, node: *ast.Node) !GenValue {
    const offset = data.rec.type.pointeeOrSelf().fields().?[data.idx].offset;
    const rec = fold_rec_offset(data.rec);

    const dst = try bc.selectReg(c, c.sema.ptr_void_t, cstr, node);

    const rec_t = rec.rec.type;

    var recv: GenValue = undefined;
    if (rec_t.kind() == .pointer) {
        recv = try genExpr(c, rec.rec, Cstr.localOrTemp());
    } else if (rec_t.kind() == .borrow) {
        recv = try genExpr(c, rec.rec, Cstr.localOrTemp());
    } else {
        recv = try genAddressOf2(c, rec.rec, Cstr.localOrTemp(), node);
    }

    try c.pushCode(.add_i16, &.{ dst.reg, recv.reg, @intCast(rec.off + offset)}, node);

    try pop_temp_value(c, recv, node);
    return dst;
}

fn gen_deref(c: *Chunk, expr: *ir.Deref, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const srcv = try genExpr(c, expr.expr, Cstr.localOrTemp());
    try push_load(c, dst.reg, srcv.reg, 0, expr.base.type, node);
    try pop_temp_value(c, srcv, node);
    return dst;
}

fn gen_case(c: *Chunk, expr: *ir.Case, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = node;
    return genExpr(c, expr.child, cstr);
}

const RecOffset = struct {
    rec: *ir.Expr,
    off: usize,
};

fn fold_rec_offset(init_rec: *ir.Expr) RecOffset {
    var rec = init_rec;
    var offset: usize = 0;

    // Check to fold with previous field access.
    while (true) {
        if (rec.type.kind() == .pointer or rec.type.kind() == .borrow) {
            break;
        }
        if (rec.code == .field) {
            const field = rec.cast(.field);
            rec = field.rec;
            offset += rec.type.pointeeOrSelf().fields().?[field.idx].offset;
        } else if (rec.code == .case) {
            rec = rec.cast(.case).child;
        } else if (rec.code == .deref) {
            rec = rec.cast(.deref).expr;
        } else {
            break;
        }
    }
    return .{ .rec = rec, .off = offset };
}

fn gen_field(c: *Chunk, expr: *ir.Field, cstr: Cstr, node: *ast.Node) !GenValue {
    const ret_t = expr.base.type;

    const offset = expr.rec.type.pointeeOrSelf().fields().?[expr.idx].offset;
    const rec = fold_rec_offset(expr.rec);

    const dst = try bc.selectReg(c, expr.base.type, cstr, node);

    const temp_start = c.cur_proc.reg_end;
    const recv = try genExpr(c, rec.rec, Cstr.localOrTemp());
    var src_ptr = recv.reg;
    if (!rec.rec.type.isRefLike()) {
        src_ptr = try reserve_reg(c, c.sema.ptr_void_t);
        try push_addr(c, src_ptr, recv.reg, node);
    }
    try push_load(c, dst.reg, src_ptr, @intCast(rec.off + offset), ret_t, node);
    try pop_regs(c, temp_start, node);
    return dst;
}

fn gen_lift(c: *Chunk, expr: *ir.Lift, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    if (expr.child.code == .init) {
        const child_t = expr.child.type;
        try push_new(c, dst.reg, child_t, @intCast(child_t.size()), node);
        try gen_init_to(c, dst.reg, expr.child.cast(.init), node);
        return dst;
    } else {
        const src = try genExpr(c, expr.child, Cstr.localOrTemp());
        const child_t = expr.child.type;

        try push_new(c, dst.reg, child_t, @intCast(child_t.size()), node);
        try push_store(c, dst.reg, 0, src.reg, child_t, node);
        try pop_temp_value(c, src, node);
        return dst;
    }
}

fn gen_init_case(c: *Chunk, expr: *ir.InitCase, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = node;
    return genExpr(c, expr.child, cstr);
}

fn gen_init_to(c: *Chunk, dst_ptr: Reg, expr: *ir.Init, node: *ast.Node) !void {
    const temp_start = c.cur_proc.reg_end;
    const arg_start = c.u16_stack.items.len;
    defer c.u16_stack.items.len = arg_start;
    const args = expr.args[0..expr.nargs];
    for (args) |arg_expr| {
        const arg = try genExpr(c, arg_expr, Cstr.localOrTemp());
        try c.u16_stack.append(c.alloc, arg.reg);
    }
    const regs = c.u16_stack.items[arg_start..];

    switch (expr.base.type.kind()) {
        .option => {
            const option_t = expr.base.type.cast(.option);
            const fields = &[_]cy.types.Field{
                option_t.fields()[0],
                .{ .sym = undefined, .type = args[1].type, .offset = 8 },
            };
            try push_struct_init_to(c, dst_ptr, fields, regs, node);
        },
        .choice => {
            const choice_t = expr.base.type.cast(.choice);
            std.debug.assert(args[1].code == .init_case);
            const fields = &[_]cy.types.Field{
                choice_t.fields()[0],
                .{ .sym = undefined, .type = args[1].cast(.init_case).child.type, .offset = 8 },
            };
            try push_struct_init_to(c, dst_ptr, fields, regs, node);
        },
        .c_union => {
            std.debug.assert(args[0].code == .init_case);
            const fields = &[_]cy.types.Field{
                .{ .sym = undefined, .type = args[0].cast(.init_case).child.type, .offset = 0 },
            };
            try push_struct_init_to(c, dst_ptr, fields, regs, node);
        },
        .result => {
            const res_t = expr.base.type.cast(.result);
            std.debug.assert(args[1].code == .init_case);
            const fields = &[_]cy.types.Field{
                res_t.fields()[0],
                .{ .sym = undefined, .type = args[1].cast(.init_case).child.type, .offset = 8 },
            };
            try push_struct_init_to(c, dst_ptr, fields, regs, node);
        },
        .struct_t => {
            const struct_t = expr.base.type.cast(.struct_t);
            try push_struct_init_to(c, dst_ptr, struct_t.fields(), regs, node);
        },
        .partial_vector => {
            const vector_t = expr.base.type.cast(.partial_vector);
            try push_store(c, dst_ptr, 0, regs[0], c.sema.i64_t, node);
            for (0..args.len-1) |i| {
                const offset = 8 + vector_t.elem_t.size() * i;
                try push_store(c, dst_ptr, @intCast(offset), regs[i+1], vector_t.elem_t, node);
            }
        },
        .vector => {
            const vector_t = expr.base.type.cast(.vector);
            for (0..args.len) |i| {
                const offset = vector_t.elem_t.size() * i;
                try push_store(c, dst_ptr, @intCast(offset), regs[i], vector_t.elem_t, node);
            }
        },
        else => {
            return c.reportErrorFmt("Unsupported: {}", &.{v(expr.base.type.kind())}, node);
        },
    }
    try pop_regs(c, temp_start, node);
}

fn gen_init(c: *Chunk, expr: *ir.Init, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try selectReg(c, expr.base.type, cstr, node);

    const temp_start = c.cur_proc.reg_end;

    // Reserve reg for dst addr.
    const dst_ptr = try reserve_reg(c, c.sema.ptr_void_t);
    try push_addr(c, dst_ptr, dst.reg, node);

    try gen_init_to(c, dst_ptr, expr, node);

    try pop_regs(c, temp_start, node);
    return dst;
}

fn gen_init_zero(c: *Chunk, expr: *ir.InitZero, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);

    const dst_ptr = try reserve_reg(c, c.sema.ptr_void_t);
    try push_addr(c, dst_ptr, dst.reg, node);

    try c.pushCode(.memsetz, &.{ dst_ptr, 0, @intCast(expr.size) }, node);

    try pop_regs(c, dst_ptr, node);
    return dst;
}

fn gen_break(c: *Chunk, stmt: *ir.Break, node: *ast.Node) !void {
    const pc = try c.pushEmptyJump(node);
    try c.blockJumpStack.append(c.alloc, .{ .jumpT = .brk, .pc = pc, .block_offset = stmt.block_offset });
}

fn gen_continue(c: *Chunk, stmt: *ir.Continue, node: *ast.Node) !void {
    const pc = try c.pushEmptyJump(node);
    try c.blockJumpStack.append(c.alloc, .{ .jumpT = .cont, .pc = pc, .block_offset = stmt.block_offset });
}

fn gen_set_deref(c: *Chunk, stmt: *ir.SetDeref, node: *ast.Node) !void {
    const ptrv = try genExpr(c, stmt.ptr, Cstr.localOrTemp());
    const rightv = try genExpr(c, stmt.right, Cstr.localOrTemp());
    const right_t = stmt.right.type;

    try push_store(c, ptrv.reg, 0, rightv.reg, right_t, node);
    try pop_temp_value(c, rightv, node);
    try pop_temp_value(c, ptrv, node);
}

fn gen_set_field(c: *Chunk, stmt: *ir.SetField, node: *ast.Node) !void {
    const fieldData = stmt.field.cast(.field);

    // const rec_is_pointer = c.sema.isPointerType(type_id);

    const val_t = stmt.field.type;

    const offset = fieldData.rec.type.pointeeOrSelf().fields().?[fieldData.idx].offset;
    const rec = fold_rec_offset(fieldData.rec);

    const temp_start = c.cur_proc.reg_end;
    var dst_ptr: GenValue = undefined;

    if (!rec.rec.type.isRefLike()) {
        dst_ptr = try genAddressOf2(c, rec.rec, Cstr.localOrTemp(), node);
    } else {
        dst_ptr = try genExpr(c, rec.rec, Cstr.localOrTemp());
    }

    const rightv = try genExpr(c, stmt.right, Cstr.localOrTemp());
    try push_store(c, dst_ptr.reg, @intCast(rec.off + offset), rightv.reg, val_t, node);
    try pop_regs(c, temp_start, node);
}

fn push_store(c: *cy.Chunk, dst_ptr: Reg, offset: u16, src: Reg, src_t: *cy.Type, node: *ast.Node) !void {
    const size = src_t.size();
    switch (size) {
        32 => {
            try c.pushCode(.store_4w, &.{ dst_ptr, offset, src }, node);
        },
        24 => {
            try c.pushCode(.store_3w, &.{ dst_ptr, offset, src }, node);
        },
        16 => {
            try c.pushCode(.store_2w, &.{ dst_ptr, offset, src }, node);
        },
        8 => {
            try c.pushCode(.store_64, &.{ dst_ptr, offset, src }, node);
        },
        4 => {
            try c.pushCode(.store_32, &.{ dst_ptr, offset, src }, node);
        },
        2 => {
            try c.pushCode(.store_16, &.{ dst_ptr, offset, src }, node);
        },
        1 => {
            try c.pushCode(.store_8, &.{ dst_ptr, offset, src }, node);
        },
        0 => {},
        else => {
            try c.pushCode(.store_n, &.{ dst_ptr, offset, src, @intCast(src_t.size()) }, node);
        },
    }
}

fn gen_bitcast(c: *Chunk, expr: *ir.Bitcast, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = node;
    return genExpr(c, expr.expr, cstr);
}

fn genUnwrapAddr(c: *Chunk, expr: *ir.UnwrapAddr, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const choice = try genExpr(c, expr.choice, Cstr.localOrTemp());

    try c.pushFCode(.unwrap_addr, &.{ dst.reg, choice.reg, expr.tag, 8 }, node);
    try pop_temp_value(c, choice, node);
    return dst;
}

fn genUnwrapNZ(c: *Chunk, expr: *ir.UnwrapNZ, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const option = try genExpr(c, expr.option, Cstr.localOrTemp());
    try c.pushFCode(.unwrap_nz, &.{ dst.reg, option.reg}, node);
    try pop_temp_value(c, option, node);
    return dst;
}

fn gen_i2f(c: *Chunk, expr: *ir.UnOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const src = try genExpr(c, expr.expr, Cstr.localOrTemp());
    if (expr.base.type.id() == bt.F64) {
        try c.pushCode(.i2f, &.{ dst.reg, src.reg}, node);
    } else {
        try c.pushCode(.i2f32, &.{ dst.reg, src.reg}, node);
    }
    try pop_temp_value(c, src, node);
    return dst;
}

fn gen_trunc(c: *Chunk, expr: *ir.Trunc, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = node;
    // Trunc just leaves the significant bits undefined.
    return genExpr(c, expr.expr, cstr);
}

fn gen_fabs(c: *Chunk, expr: *ir.UnOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const src = try genExpr(c, expr.expr, Cstr.localOrTemp());
    if (expr.base.type.id() == bt.F64) {
        try c.pushCode(.fabs, &.{dst.reg, src.reg}, node);
    } else {
        try c.pushCode(.f32abs, &.{ dst.reg, src.reg}, node);
    }
    try pop_temp_value(c, src, node);
    return dst;
}

fn gen_f2i(c: *Chunk, expr: *ir.UnOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const src = try genExpr(c, expr.expr, Cstr.localOrTemp());
    if (expr.expr.type.id() == bt.F64) {
        try c.pushCode(.f2i, &.{ dst.reg, src.reg}, node);
    } else {
        try c.pushCode(.f32_2i, &.{ dst.reg, src.reg}, node);
    }
    try pop_temp_value(c, src, node);
    return dst;
}

fn gen_sext(c: *Chunk, expr: *ir.Widen, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const src = try genExpr(c, expr.expr, Cstr.localOrTemp());
    try push_sext(c, dst.reg, src.reg, @intCast(expr.src_bits), node);
    try pop_temp_value(c, src, node);
    return dst;
}

fn gen_zext(c: *Chunk, expr: *ir.Widen, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const src = try genExpr(c, expr.expr, Cstr.localOrTemp());
    try push_zext(c, dst.reg, src.reg, @intCast(expr.src_bits), node);
    try pop_temp_value(c, src, node);
    return dst;
}

fn gen_void(c: *Chunk, cstr: Cstr, node: *ast.Node) !GenValue {
    if (cstr.type == .discard) {
        return GenValue.discard();
    }
    const dst = try bc.selectReg(c, c.sema.void_t, cstr, node);
    // Does not generate inst.
    return dst;
}

fn gen_undef(c: *Chunk, expr: *ir.SimpleExpr, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    // Nop.
    return dst;
}

fn gen_true(c: *Chunk, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, c.sema.bool_t, cstr, node);
    try c.pushCode(.true, &.{dst.reg}, node);
    return dst;
}

fn gen_false(c: *Chunk, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, c.sema.bool_t, cstr, node);
    try c.pushCode(.false, &.{dst.reg}, node);
    return dst;
}

fn gen_const8(c: *Chunk, expr: *ir.Const8, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    try c.pushCode(.const_16, &.{ dst.reg, expr.val }, node);
    return dst;
}

fn gen_const16(c: *Chunk, expr: *ir.Const16, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    try c.pushCode(.const_16, &.{ dst.reg, expr.val }, node);
    return dst;
}

fn gen_const32(c: *Chunk, expr: *ir.Const32, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const pc = c.buf.ops.items.len;
    try c.pushCode(.const_32, &.{ dst.reg, 0, 0 }, node);
    c.buf.setOpArgU32(pc + 2, expr.val);
    return dst;
}

fn genIsZero(c: *Chunk, expr: *ir.IsZero, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);

    const childv = try genExpr(c, expr.child, Cstr.localOrTemp());

    try c.pushCode(.is_zero, &.{dst.reg, childv.reg}, node);
    try pop_temp_value(c, childv, node);
    return dst;
}

fn gen_retain(c: *Chunk, expr: *ir.Retain, cstr: Cstr, node: *ast.Node) !GenValue {
    const child = try genExpr(c, expr.expr, cstr);

    try c.pushCode(.retain, &.{ child.reg }, node);
    return child;
}

fn gen_string(c: *Chunk, expr: *ir.String, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    if (expr.base.type.id() == bt.Str) {
        try pushConstString(c, expr.raw(), dst.reg, node);
    } else if (expr.base.type.isPointer()) {
        try pushConstStrZPtr(c, expr.raw(), dst.reg, node);
    } else {
        return error.Unexpected;
    }
    return dst;
}

fn pushConstStrZPtr(c: *Chunk, str: []const u8, dst: Reg, node: *ast.Node) !void {
    const res = try c.buf.getOrPushConstString(str);
    try genConst64_(c, @intFromPtr(res.slice.ptr), dst, node);
}

fn pushConstString(c: *Chunk, str: []const u8, dst: Reg, node: *ast.Node) !void {
    const res = try c.buf.getOrPushConstString(str);
    const ptr_len = (@as(u64, @intFromPtr(res.slice.ptr)) & 0xffffffffffff) | (@as(u64, res.slice.len) << 48);
    const pc = c.buf.ops.items.len;
    try c.pushCode(.const_str, &.{ dst, 0, 0, 0, 0, @intFromBool(res.ascii) }, node);
    c.buf.setOpArgU64(pc + 2, ptr_len);
}

fn genUnOp2(c: *Chunk, expr: *ir.UnOp2, op: cy.OpCode, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const childv = try genExpr(c, expr.expr, Cstr.localOrTemp());
    try pushInlineUnExpr(c, op, dst.reg, childv.reg, node);
    try pop_temp_value(c, childv, node);
    return dst;
}

fn genUnOp(c: *Chunk, expr: *ir.UnOp, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);

    const childv = try genExpr(c, expr.expr, Cstr.localOrTemp());

    switch (expr.op) {
        .lnot => {
            try c.pushCode(.lnot, &.{dst.reg, childv.reg}, node);
        },
        .minus,
        .bitwiseNot => {
            if (expr.childT == c.sema.i64_t) {
                try pushInlineUnExpr(c, getIntUnaryOpCode(expr.op), dst.reg, childv.reg, node);
            } else if (expr.childT == c.sema.f64_t) {
                try pushInlineUnExpr(c, getFloatUnaryOpCode(expr.op), dst.reg, childv.reg, node);
            } else return error.Unexpected;
            // Builtin unary expr do not have retained child.
        },
        else => {
            return c.reportErrorFmt("Unsupported op: {}", &.{v(expr.op)}, node);
        }
    }

    try pop_temp_value(c, childv, node);
    return dst;
}

fn gen_call_trait(c: *Chunk, expr: *ir.CallTrait, cstr: Cstr, node: *ast.Node) !GenValue {
    const ret_t = expr.base.type;
    const inst = try beginCall(c, cstr, ret_t, true, node);

    // Trait struct is reserved over the callee and the first argument (the receiver).
    var temp = try reserve_reg(c, expr.trait.type);
    _ = try genExpr(c, expr.trait, Cstr.toTemp(temp));

    // Args. Skip impl receiver.
    const args = expr.args[1..expr.nargs];
    for (args) |arg| {
        temp = try reserve_reg(c, arg.type);
        _ = try genExpr(c, arg, Cstr.toTemp(temp));
    }

    try c.pushFCode(.call_trait, &.{inst.base, @intCast(expr.vtable_idx) }, node);

    return endCall(c, inst, ret_t, node);
}

fn genCallIntrinsic(c: *Chunk, call: *ir.Call, cstr: Cstr, intrinsic: Intrinsic, node: *ast.Node) !GenValue {
    switch (intrinsic) {
        .null => {
            return error.Unexpected;
        },
        .dtor_str => {
            const borrow = try genExpr(c, call.args[0], Cstr.localOrTemp());
            try c.pushCode(.dtor_str, &.{borrow.reg}, node);
            try pop_temp_value(c, borrow, node);
            return GenValue.discard();
        },
        .cmp_str => {
            const dst = try bc.selectReg(c, call.base.type, cstr, node);
            const borrow_a = try genExpr(c, call.args[0], Cstr.localOrTemp());
            const borrow_b = try genExpr(c, call.args[1], Cstr.localOrTemp());
            try c.pushCode(.cmp_str, &.{dst.reg, borrow_a.reg, borrow_b.reg }, node);
            try pop_temp_value(c, borrow_b, node);
            try pop_temp_value(c, borrow_a, node);
            return dst;
        },
    }
}

fn genCallExternPtr(c: *Chunk, expr: *ir.CallPtr, func_ptr: *cy.types.FuncPtr, cstr: Cstr, node: *ast.Node) !GenValue {
    const ret_t = expr.base.type;
    const inst = try beginCall(c, cstr, ret_t, false, node);

    const args = expr.args[0..expr.nargs];

    // Push func pointer as the first arg.
    var temp = try reserve_reg(c, expr.callee.type);
    _ = try genExpr(c, expr.callee, Cstr.toTemp(temp));

    var exp_arg_reg = c.cur_proc.reg_end;
    for (args) |arg| {
        temp = try reserve_reg(c, arg.type);
        if (cy.Trace) {
            if (temp != exp_arg_reg) return error.Unexpected;
            exp_arg_reg += @intCast(arg.type.reg_size());
        }
        _ = try genExpr(c, arg, Cstr.toTemp(temp));
    }

    _ = try ensureExternFuncPtrDispatch(c.compiler, func_ptr);

    // Invoke extern function pointer dispatch.
    const start = c.buf.ops.items.len;
    try c.pushFCode(.call_host, &.{ inst.base, 0, 0, 0 }, node);
    try c.rt_relocations.append(c.alloc, .{.kind = .extern_func_ptr, .pc = start + 2, .data = .{.extern_func_ptr = func_ptr}});

    return endCall(c, inst, ret_t, node);
}

fn gen_call(c: *Chunk, expr: *ir.Call, cstr: Cstr, node: *ast.Node) !GenValue {
    log.tracev("call {s}", .{expr.func.name()});

    if (expr.func.info.gen_intrinsic > 0) {
        return genCallIntrinsic(c, expr, cstr, @enumFromInt(expr.func.info.gen_intrinsic), node);
    }

    const ret_t = expr.base.type;
    var callee_ret_t = ret_t;
    if (expr.func.info.generator) {
        const gen_t = ret_t.cast(.pointer).child_t;
        const val_t = gen_t.sym().instance.?.params[0].asPtr(*cy.Type).cast(.func_ptr).sig.ret;
        const next_t = try cy.sema.getOptionType(c, val_t);
        if (next_t.size() > ret_t.size()) {
            callee_ret_t = next_t;
        }
    }
    const inst = try beginCall(c, cstr, callee_ret_t, false, node);

    const args = expr.args[0..expr.numArgs];

    var exp_arg_reg = c.cur_proc.reg_end;
    for (args) |arg| {
        const temp = try reserve_reg(c, arg.type);
        if (cy.Trace) {
            if (temp != exp_arg_reg) return error.Unexpected;
            exp_arg_reg += @intCast(arg.type.reg_size());
        }
        _ = try genExpr(c, arg, Cstr.toTemp(temp));
    }

    if (expr.func.type == .extern_) {
        // const rt_id = (try ensureExternFunc(c.compiler, expr.func)).vm_id;
        try pushCall(c, inst.base, expr.func, node);
    } else {
        // const rt_id = try ensureFunc(c.compiler, expr.func);
        try pushCall(c, inst.base, expr.func, node);
    }

    return endCall(c, inst, ret_t, node);
}

fn gen_call_union(c: *Chunk, expr: *ir.CallUnion, cstr: Cstr, node: *ast.Node) !GenValue {
    const ret_t = expr.base.type;
    const inst = try beginCall(c, cstr, ret_t, true, node);

    // Callee.
    const arg_start = c.cur_proc.reg_end;
    var temp = try reserve_reg(c, expr.callee.type);
    _ = try genExpr(c, expr.callee, Cstr.toTemp(temp));

    // Args.
    var exp_arg_reg = arg_start + 1;
    const args = expr.args[0..expr.nargs];
    for (args) |arg| {
        temp = try reserve_reg(c, arg.type);
        if (cy.Trace) {
            if (temp != exp_arg_reg) return error.Unexpected;
            exp_arg_reg += @intCast(arg.type.reg_size());
        }
        _ = try genExpr(c, arg, Cstr.toTemp(temp));
    }

    try c.pushFCode(.call_union, &.{inst.base}, node);

    try pop_regs(c, arg_start, node);
    return endCall(c, inst, ret_t, node);
}

fn gen_call_ptr(c: *Chunk, expr: *ir.CallPtr, cstr: Cstr, node: *ast.Node) !GenValue {
    const func_ptr = expr.callee.type.cast(.func_ptr);
    if (func_ptr.sig.extern_) {
        return genCallExternPtr(c, expr, func_ptr, cstr, node);
    }

    const ret_t = expr.base.type;
    const inst = try beginCall(c, cstr, ret_t, true, node);

    // Callee.
    var temp = try reserve_reg(c, expr.callee.type);
    _ = try genExpr(c, expr.callee, Cstr.toTemp(temp));

    // Args.
    const args = expr.args[0..expr.nargs];
    for (args) |arg| {
        temp = try reserve_reg(c, arg.type);
        _ = try genExpr(c, arg, Cstr.toTemp(temp));
    }

    try c.pushFCode(.call_ptr, &.{inst.base}, node);

    return endCall(c, inst, ret_t, node);
}

fn pushCmp(c: *Chunk, val_t: *cy.Type, dst: Reg, left: Reg, right: Reg, node: *ast.Node) !void {
    switch (val_t.id()) {
        bt.Void => {
            try c.pushCode(.true, &.{dst}, node);
        },
        bt.Bool,
        bt.R8,
        bt.I8 => {
            try c.pushCode(.cmp_8, &.{dst, left, right}, node);
        },
        bt.F32 => {
            try c.pushCode(.feq32, &.{dst, left, right}, node);
        },
        bt.F64 => {
            try c.pushCode(.feq, &.{dst, left, right}, node);
        },
        bt.R32,
        bt.I32 => {
            try push_zext(c, left, left, 32, node);
            try push_zext(c, right, right, 32, node);
            try c.pushCode(.cmp, &.{dst, left, right}, node);
        },
        bt.R16,
        bt.I16 => {
            try push_zext(c, left, left, 16, node);
            try push_zext(c, right, right, 16, node);
            try c.pushCode(.cmp, &.{dst, left, right}, node);
        },
        bt.R64,
        bt.Symbol,
        bt.Error,
        bt.I64 => {
            try c.pushCode(.cmp, &.{dst, left, right}, node);
        },
        else => {
            switch (val_t.kind()) {
                .pointer,
                .func_ptr,
                .enum_t => {
                    try c.pushCode(.cmp, &.{dst, left, right}, node);
                },
                else => {
                    return c.reportErrorFmt("Unsupported comparison: {}", &.{v(val_t.name())}, node);
                }
            }
        }
    }
}

fn genCompare(c: *Chunk, expr: *ir.Compare, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const left = try genExpr(c, expr.left, Cstr.localOrTemp());
    const right = try genExpr(c, expr.right, Cstr.localOrTemp());
    if (expr.left.type.kind() == .float) {
        if (expr.left.type.id() == bt.F64) {
            const code: cy.OpCode = switch (expr.cond) {
                .lt => .flt,
                .le => .fle,
                .gt => .fgt,
                .ge => .fge,
                else => return error.TODO,
            };
            try c.pushCode(code, &.{ dst.reg, left.reg, right.reg }, node);
        } else {
            const code: cy.OpCode = switch (expr.cond) {
                .lt => .flt32,
                .le => .fle32,
                .gt => .fgt32,
                .ge => .fge32,
                else => return error.TODO,
            };
            try c.pushCode(code, &.{ dst.reg, left.reg, right.reg }, node);
        }
    } else {
        var bits: u32 = undefined;
        if (expr.left.type.kind() == .raw) {
            bits = expr.left.type.cast(.raw).bits;
        } else {
            bits = expr.left.type.cast(.int).bits;
        }
        if (bits < 64) {
            const unsigned = expr.cond.isUnsigned();
            if (unsigned) {
                try push_zext(c, left.reg, left.reg, @intCast(bits), node);
                try push_zext(c, right.reg, right.reg, @intCast(bits), node);
            } else {
                try push_sext(c, left.reg, left.reg, @intCast(bits), node);
                try push_sext(c, right.reg, right.reg, @intCast(bits), node);
            }
        }
        const code: cy.OpCode = switch (expr.cond) {
            .ult => .ult,
            .lt => .lt,
            .ule => .ule,
            .le => .le,
            .ugt => .ugt,
            .gt => .gt,
            .uge => .uge,
            .ge => .ge,
            else => return error.TODO,
        };
        try c.pushCode2(code, dst.isTemp(), &.{ dst.reg, left.reg, right.reg }, node);
    }
    try pop_temp_value(c, right, node);
    try pop_temp_value(c, left, node);
    return dst;
}

fn gen_fneg(c: *Chunk, expr: *ir.UnOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const childv = try genExpr(c, expr.expr, Cstr.localOrTemp());
    if (expr.base.type.id() == bt.F64) {
        try pushInlineUnExpr(c, .fneg, dst.reg, childv.reg, node);
    } else {
        try pushInlineUnExpr(c, .fneg32, dst.reg, childv.reg, node);
    }
    try pop_temp_value(c, childv, node);
    return dst;
}

fn gen_fmod(c: *Chunk, expr: *ir.BinOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const leftv = try genExpr(c, expr.left, Cstr.localOrTemp());
    const rightv = try genExpr(c, expr.right, Cstr.localOrTemp());
    if (expr.base.type.id() == bt.F64) {
        try pushInlineBinExpr(c, .fmod, dst, leftv.reg, rightv.reg, node);
    } else {
        try pushInlineBinExpr(c, .fmod32, dst, leftv.reg, rightv.reg, node);
    }
    try pop_temp_value(c, rightv, node);
    try pop_temp_value(c, leftv, node);
    return dst;
}

fn gen_fdiv(c: *Chunk, expr: *ir.BinOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const leftv = try genExpr(c, expr.left, Cstr.localOrTemp());
    const rightv = try genExpr(c, expr.right, Cstr.localOrTemp());
    if (expr.base.type.id() == bt.F64) {
        try pushInlineBinExpr(c, .fdiv, dst, leftv.reg, rightv.reg, node);
    } else {
        try pushInlineBinExpr(c, .fdiv32, dst, leftv.reg, rightv.reg, node);
    }
    try pop_temp_value(c, rightv, node);
    try pop_temp_value(c, leftv, node);
    return dst;
}

fn gen_fmul(c: *Chunk, expr: *ir.BinOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const leftv = try genExpr(c, expr.left, Cstr.localOrTemp());
    const rightv = try genExpr(c, expr.right, Cstr.localOrTemp());
    if (expr.base.type.id() == bt.F64) {
        try pushInlineBinExpr(c, .fmul, dst, leftv.reg, rightv.reg, node);
    } else {
        try pushInlineBinExpr(c, .fmul32, dst, leftv.reg, rightv.reg, node);
    }
    try pop_temp_value(c, rightv, node);
    try pop_temp_value(c, leftv, node);
    return dst;
}

fn gen_fsub(c: *Chunk, expr: *ir.BinOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const leftv = try genExpr(c, expr.left, Cstr.localOrTemp());
    const rightv = try genExpr(c, expr.right, Cstr.localOrTemp());
    if (expr.base.type.id() == bt.F64) {
        try pushInlineBinExpr(c, .fsub, dst, leftv.reg, rightv.reg, node);
    } else {
        try pushInlineBinExpr(c, .fsub32, dst, leftv.reg, rightv.reg, node);
    }
    try pop_temp_value(c, rightv, node);
    try pop_temp_value(c, leftv, node);
    return dst;
}

fn gen_fadd(c: *Chunk, expr: *ir.BinOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const leftv = try genExpr(c, expr.left, Cstr.localOrTemp());
    const rightv = try genExpr(c, expr.right, Cstr.localOrTemp());
    if (expr.base.type.id() == bt.F64) {
        try pushInlineBinExpr(c, .fadd, dst, leftv.reg, rightv.reg, node);
    } else {
        try pushInlineBinExpr(c, .fadd32, dst, leftv.reg, rightv.reg, node);
    }
    try pop_temp_value(c, rightv, node);
    try pop_temp_value(c, leftv, node);
    return dst;
}

fn genBinOp2(c: *Chunk, expr: *ir.BinOp2, op: cy.OpCode, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const leftv = try genExpr(c, expr.left, Cstr.localOrTemp());
    const rightv = try genExpr(c, expr.right, Cstr.localOrTemp());
    try pushInlineBinExpr(c, op, dst, leftv.reg, rightv.reg, node);
    try pop_temp_value(c, rightv, node);
    try pop_temp_value(c, leftv, node);
    return dst;
}

fn genBinOp(c: *Chunk, expr: *ir.BinOp, cstr: Cstr, node: *ast.Node) !GenValue {
    // Most builtin binOps do not retain.
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);

    const leftv = try genExpr(c, expr.left, Cstr.localOrTemp());
    const rightv = try genExpr(c, expr.right, Cstr.localOrTemp());

    switch (expr.op) {
        .bitwiseXor => {
            if (expr.leftT.id() == bt.I64) {
                try pushInlineBinExpr(c, getIntOpCode(expr.op), dst, leftv.reg, rightv.reg, node);
            } else return error.Unexpected;
        },
        .pow => {
            if (expr.leftT.id() == bt.F64) {
                try pushInlineBinExpr(c, getFloatOpCode(expr.op), dst, leftv.reg, rightv.reg, node);
            } else if (expr.leftT.id() == bt.I64) {
                try pushInlineBinExpr(c, getIntOpCode(expr.op), dst, leftv.reg, rightv.reg, node);
            } else return error.Unexpected;
        },
        .bang_equal,
        .equal_equal => {
            try pushCmp(c, expr.leftT, dst.reg, leftv.reg, rightv.reg, node);           
        },
        else => {
            return c.reportErrorFmt("Unsupported op: {}", &.{v(expr.op)}, node);
        },
    }

    if (expr.op == .bang_equal) {
        try c.pushFCode(.lnot, &.{dst.reg, dst.reg}, node);
    }

    try pop_temp_value(c, rightv, node);
    try pop_temp_value(c, leftv, node);

    return dst;
}

fn gen_trait(c: *Chunk, expr: *ir.Trait, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const ref = try genExpr(c, expr.ref, Cstr.localOrTemp());

    const key = VtableKey{ .type = expr.impl_t.id(), .trait = expr.generic_t.base.id() };
    const vtable_idx = c.compiler.gen_vtables.get(key).?;

    try c.pushFCode(.trait, &.{ dst.reg, ref.reg, @intCast(expr.trait_t.id()), @intCast(vtable_idx) }, node);

    try pop_temp_value(c, ref, node);
    return dst;
}

fn gen_captured(c: *Chunk, expr: *ir.Captured, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);

    try c.pushCode(.captured, &.{ dst.reg, c.cur_proc.closure_local, expr.idx }, node);
    return dst;
}

pub fn toLocalReg(c: *Chunk, ir_id: u8) Reg {
    if (ir_id >= c.bc_local_map.items.len) {
        std.debug.panic("expected mapping to IR local `{}`.", .{ir_id});
    }
    return c.bc_local_map.items.ptr[ir_id];
}

fn push_load_32(c: *cy.Chunk, dst: Reg, src: Reg, src_off: u16, debugNode: *ast.Node) !void {
    try c.pushCode(.load_32, &.{ dst, src, src_off }, debugNode);
}

fn push_load_16(c: *cy.Chunk, dst: Reg, src: Reg, src_off: u16, debugNode: *ast.Node) !void {
    try c.pushCode(.load_16, &.{ dst, src, src_off }, debugNode);
}

fn push_load_8(c: *cy.Chunk, dst: Reg, src: Reg, src_off: u16, debugNode: *ast.Node) !void {
    try c.pushCode(.load_8, &.{ dst, src, src_off }, debugNode);
}

fn push_load_64(c: *cy.Chunk, dst: Reg, src: Reg, offset: u16, debugNode: *ast.Node) !void {
    try c.pushCode(.load_64, &.{ dst, src, offset }, debugNode);
}

fn push_load(c: *cy.Chunk, dst: Reg, src_ptr: Reg, src_off: u16, src_t: *cy.Type, node: *ast.Node) !void {
    const size = src_t.size();
    switch (size) {
        32 => {
            try c.pushCode(.load_4w, &.{ dst, src_ptr, src_off }, node);
        },
        24 => {
            try c.pushCode(.load_3w, &.{ dst, src_ptr, src_off }, node);
        },
        16 => {
            try c.pushCode(.load_2w, &.{ dst, src_ptr, src_off }, node);
        },
        8 => {
            try push_load_64(c, dst, src_ptr, src_off, node);
        },
        4 => {
            try push_load_32(c, dst, src_ptr, src_off, node);
        },
        2 => {
            try push_load_16(c, dst, src_ptr, src_off, node);
        },
        1 => {
            try push_load_8(c, dst, src_ptr, src_off, node);
        },
        0 => {},
        else => {
            try c.pushCode(.load_n, &.{ dst, src_ptr, src_off, @intCast(src_t.size()) }, node);
        },
    }
}

fn gen_local(c: *Chunk, expr: *ir.Local, cstr: Cstr, node: *ast.Node) !GenValue {
    const reg = toLocalReg(c, expr.id);
    if (cstr.type == .simple) {
        return GenValue.initLocal(reg);
    } else {
        const dst = try selectReg(c, expr.base.type, cstr, node);
        try push_mov(c, dst.reg, reg, @intCast(expr.base.type.reg_size()), node);
        return dst;
    }
}

fn gen_funcptr2(c: *Chunk, dst: Reg, func: *cy.Func, node: *ast.Node) !void {
    switch (func.type) {
        .extern_ => {
            const rt_id = c.compiler.genSymMap.get(func).?.extern_func.id;
            try c.pushCode(.extern_func, &.{ dst, @intCast(rt_id) }, node);
        },
        .hostFunc => {
            const start = c.buf.ops.items.len;
            try c.pushCode(.fn_host, &.{dst, 0, 0, 0}, node);
            c.buf.setOpArgU48(start + 2, @intCast(@intFromPtr(func.data.hostFunc.ptr)));
        },
        .userLambda,
        .userFunc => {
            const start = c.buf.ops.items.len;
            try c.pushCode(.fn_vm, &.{dst, 0, 0, 0}, node);
            try c.relocations.append(c.alloc, .{.kind = .func, .pc = start + 2, .data = .{.func = func}});
        },
        else => {
            @panic("unexpected");
        },
    }
}

fn gen_funcptr(c: *Chunk, expr: *ir.FuncPtr, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    try gen_funcptr2(c, dst.reg, expr.func, node);
    return dst;
}

fn genFuncUnion(c: *Chunk, expr: *ir.Func, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    const childv = try genExpr(c, expr.expr, Cstr.localOrTemp());

    try c.pushCode(.fn_union, &.{ dst.reg, childv.reg, @intCast(expr.base.type.id()) }, node);
    try pop_temp_value(c, childv, node);
    return dst;
}

const NullSlot = cy.NullU8;

/// Reserve params and captured vars.
/// Function frame layout:
/// [ret 1+] [ret_info 1] [ret_pc 1] [ret_fp 1] [callee 1] [params...] [locals...]
/// `callee` is reserved so that function values can call static functions with the same call convention.
/// Captured vars are accessed from the closure in the callee slot.
fn reserveFuncSlots(c: *Chunk, params: []align(1) const ir.FuncParam, ret_t: *cy.Type) !void {
    // Reserve return.
    log.tracev("reserve func ret reg size: {}", .{ret_t.reg_size()});
    c.cur_proc.reg_end += @intCast(ret_t.reg_size());
    c.cur_proc.ret_size = @intCast(ret_t.reg_size());

    // Reserve return info, return address, previous frame pointer, and optional callee.
    c.cur_proc.reg_end += 4;

    // Closure local is the callee local.
    c.cur_proc.closure_local = @intCast(c.cur_proc.reg_end - 1);

    c.buf.ptr_layout_builder.clearRetainingCapacity();
    try c.buf.ptr_layout_builder.resize(c.alloc, c.cur_proc.reg_end);
    @memset(c.buf.ptr_layout_builder.items, false);

    c.bc_local_map.clearRetainingCapacity();

    // Reserve func params.
    for (params, 0..) |param, i| {
        log.tracev("reserve param {s} {}", .{param.name(), i});
        _ = try reserve_local(c, i, param.declType, c.cur_proc.debugNode);
    }

    c.cur_proc.max_regs = c.cur_proc.reg_end;
}

fn reserve_local(c: *Chunk, local_id: usize, local_t: *cy.Type, node: *ast.Node) !Reg {
    if (cy.Trace) {
        if (local_id != c.bc_local_map.items.len) {
            return c.reportErrorFmt("Expected next IR local to be {}, got {}.", &.{v(c.bc_local_map.items.len), v(local_id)}, node);
        }
    }
    std.debug.assert(local_id == c.bc_local_map.items.len);

    const reg = try reserve_reg(c, local_t);
    try c.bc_local_map.append(c.alloc, reg);
    return reg;
}

fn reserve_reg(c: *Chunk, local_t: *cy.Type) !Reg {
    const reg = c.cur_proc.reg_end;
    log.tracev("reserve reg: {} {s}", .{reg, local_t.name()});
    const reg_size = type_reg_size(local_t);
    try c.buf.ptr_layout_builder.resize(c.alloc, reg + reg_size);

    c.cur_proc.reg_end += @intCast(reg_size);
    if (c.cur_proc.reg_end > c.cur_proc.max_regs) {
        c.cur_proc.max_regs = c.cur_proc.reg_end;
    }

    if (local_t.is_stack_ptr()) {
        c.buf.ptr_layout_builder.items[reg] = true;
        c.cur_proc.num_ptr_locals += 1;
        // Dump where ptr regs are reserved on the stack.
        // if (cy.Trace) {
        //     const buf = try std.fmt.allocPrint(c.alloc, "ptr reg {}", .{reg});
        //     defer c.alloc.free(buf);
        //     try genNopLabel2(c, buf, c.ast.null_node);
        // }
    } else {
        if (reg_size == 1) {
            c.buf.ptr_layout_builder.items[reg] = false;
        } else {
            @memset(c.buf.ptr_layout_builder.items[reg..reg+reg_size], false);
        }
    }
    return reg;
}

fn gen_global(c: *Chunk, expr: *ir.Global, cstr: Cstr, node: *ast.Node) !GenValue {
    // const varId = c.compiler.genSymMap.get(expr.sym).?.varSym.id;

    const dst = try bc.selectReg(c, expr.base.type, cstr, node);

    const ptr = try reserve_reg(c, c.sema.ptr_void_t);
    _ = try gen_addr_global(c, expr.sym, Cstr.toTemp(ptr), node);

    try push_load(c, dst.reg, ptr, 0, expr.base.type, node);
    try pop_regs(c, ptr, node);

    // if (expr.sym.type == .extern_var) {
    //     try push_load_64(c, dst.reg, 0, dst.reg, node);
    // }
    return dst;
}

fn gen_set_global(c: *Chunk, stmt: *ir.SetGlobal, node: *ast.Node) !void {
    const ptr = try reserve_reg(c, c.sema.ptr_void_t);
    _ = try gen_addr_global(c, stmt.sym, Cstr.toTemp(ptr), node);

    const expr = try genExpr(c, stmt.expr, Cstr.localOrTemp());
    try push_store(c, ptr, 0, expr.reg, stmt.expr.type, node);
    try pop_regs(c, ptr, node);
}

pub fn type_reg_size(type_: *cy.Type) usize {
    return (type_.size() + 7) >> 3;
}

fn gen_discard(c: *Chunk, stmt: *ir.Discard, node: *ast.Node) !void {
    _ = node;
    const cstr = Cstr.discard();
    _ = try genExpr(c, stmt.expr, cstr);
}

fn gen_declare_local(c: *Chunk, stmt: *ir.DeclareLocal, node: *ast.Node) !void {
    // _ = node;
    const reg = try reserve_local(c, stmt.id, stmt.decl_t, node);
    log.tracev("declare local {} -> %{}, type: {s}", .{stmt.id, reg, stmt.decl_t.name()});

    if (stmt.init) |init| {
        const cstr = Cstr.toLocal(reg);
        _ = try genExpr(c, init, cstr);
    }
}

fn gen_set_captured(c: *Chunk, stmt: *ir.SetGeneric, node: *ast.Node) !void {
    _ = node;
    const capData = stmt.left.cast(.captured);

    // RHS.
    // const dstRetained = c.sema.isBoxedType(data.leftT.id);
    _ = try genExpr(c, stmt.right, Cstr.toCaptured(capData.idx, stmt.right.type.is_stack_ptr()));
}

fn gen_set_ret(c: *Chunk, stmt: *ir.SetReturn, node: *ast.Node) !void {
    _ = node;
    const dst = Cstr.toLocal(0);
    _ = try genExpr(c, stmt.right, dst);
}

fn gen_set_local(c: *Chunk, stmt: *ir.SetLocal, node: *ast.Node) !void {
    _ = node;
    const reg = toLocalReg(c, stmt.id);
    _ = try genExpr(c, stmt.right, Cstr.toLocal(reg));
}

fn gen_ret(c: *Chunk, stmt: *ir.Return, node: *ast.Node) !void {
    _ = stmt;
    if (c.cur_proc.type == .main) {
        try c.pushCode(.end, &.{}, node);
    } else {
        try push_ret(c, c.cur_proc.ret_size, node);
    }
}

fn push_ret(c: *Chunk, ret_size: u16, node: *ast.Node) !void {
    if (ret_size == 0) {
        try c.pushCode(.ret_0, &.{}, node);
    } else if (c.cur_proc.ret_size == 1) {
        try c.pushCode(.ret, &.{}, node);
    } else {
        try c.pushCode(.ret_n, &.{c.cur_proc.ret_size}, node);
    }
}

fn gen_ret_gen(c: *Chunk, stmt: *ir.ReturnGen, node: *ast.Node) !void {
    const pc = c.buf.ops.items.len;

    var reg_size = stmt.gen_t.reg_size();
    if (stmt.next_t.reg_size() > reg_size) {
        reg_size = stmt.next_t.reg_size();
    }

    try c.pushFCode(.ret_gen, &.{ @intCast(stmt.gen_t.id()), @intCast(reg_size), c.cur_proc.reg_end, 0 }, node);
    try genStmts(c, stmt.deinit_head);
    c.buf.ops.items[pc + 4].val = @intCast(c.buf.ops.items.len - pc);
}

fn gen_ret_expr(c: *Chunk, stmt: *ir.ReturnExpr, node: *ast.Node) !void {
    if (c.cur_proc.type == .main) {
        // Main block.
        var childv: GenValue = undefined;
        childv = try genExpr(c, stmt.expr, Cstr.ret);
        try pop_temp_value(c, childv, node);

        try c.pushCode(.end, &.{}, node);
    } else {
        _ = try genExpr(c, stmt.expr, Cstr.toLocal(0));
        try push_ret(c, c.cur_proc.ret_size, node);
    }
}

pub fn reserveMainRegs(c: *Chunk) !void {
    // Reserve the first slot for BC main frame check and the JIT return addr.
    c.cur_proc.reg_end += 1;

    c.buf.ptr_layout_builder.clearRetainingCapacity();
    try c.buf.ptr_layout_builder.resize(c.alloc, c.cur_proc.reg_end);
    @memset(c.buf.ptr_layout_builder.items, false);

    c.bc_local_map.clearRetainingCapacity();
}

pub const CallInst = struct {
    temp_start: Reg,

    // Reg for return info.
    base: Reg,

    ret: GenValue,
    has_final_dst: bool,
    final_dst: GenValue,
};

/// Returns gen strategy and advances the temp local.
pub fn beginCall(c: *Chunk, cstr: Cstr, ret_t: *cy.Type, hasCalleeValue: bool, node: *ast.Node) !CallInst {
    _ = node;

    var ret: GenValue = undefined;
    var has_final_dst = false;
    var final_dst: GenValue = undefined;
    var allocTempRet = true;

    const ret_reg_size = type_reg_size(ret_t);

    // Optimization: Check to use dst cstr as ret.
    if (cstr.type == .tempReg or cstr.type == .localReg) {
        if (cstr.data.slot.dst == c.cur_proc.reg_end - ret_reg_size) {
            if (cstr.data.slot.dst != 0) {
                if (cstr.type == .tempReg) {
                    ret = GenValue.initTemp(cstr.data.slot.dst);
                } else {
                    ret = GenValue.initLocal(cstr.data.slot.dst);
                }
                allocTempRet = false;

            }
        }
        if (allocTempRet) {
            has_final_dst = true;
            final_dst = GenValue.initTemp(cstr.data.slot.dst);
        }
    } else if (cstr.type == .discard) {
        ret = GenValue.discard();
        allocTempRet = false;
    }

    var temp_start = c.cur_proc.reg_end;
    if (allocTempRet) {
        const temp = try reserve_reg(c, ret_t);
        ret = GenValue.initTemp(temp);
        if (cy.Trace) std.debug.assert(ret.reg != 0);
    }

    const base = c.cur_proc.reg_end;
    if (!has_final_dst) {
        temp_start = base;
    }
    // Reserve registers for return info.
    _ = try reserve_reg(c, c.sema.i64_t);
    _ = try reserve_reg(c, c.sema.i64_t);
    _ = try reserve_reg(c, c.sema.i64_t);

    if (!hasCalleeValue) {
        // Reserve callee reg.
        _ = try reserve_reg(c, c.sema.i64_t);
    }

    return .{
        .temp_start = temp_start,
        .base = base,
        .ret = ret,
        .has_final_dst = has_final_dst,
        .final_dst = final_dst,
    };
}

pub fn push_mov(c: *Chunk, dst: Reg, src: Reg, reg_size: u16, node: *ast.Node) !void {
    if (dst == src) {
        return;
    }
    switch (reg_size) {
        0 => {},
        1 => {
            try c.pushCode(.mov, &.{dst, src}, node);
        },
        2 => {
            try c.pushCode(.mov_2, &.{dst, src}, node);
        },
        3 => {
            try c.pushCode(.mov_3, &.{dst, src}, node);
        },
        4 => {
            try c.pushCode(.mov_4, &.{dst, src}, node);
        },
        else => {
            try c.pushCode(.mov_n, &.{dst, src, reg_size}, node);
        },
    }
}

// In the case of generators, calls reserve the union return which can be bigger than the actual `ret_t`.
pub fn endCall(c: *Chunk, inst: CallInst, ret_t: *cy.Type, node: *ast.Node) !GenValue {
    if (inst.has_final_dst) {
        try push_mov(c, inst.final_dst.reg, inst.ret.reg, @intCast(ret_t.reg_size()), node);
        try pop_regs(c, inst.temp_start, node);
        return inst.final_dst;
    }
    try pop_regs(c, inst.temp_start, node);
    return inst.ret;
}

fn gen_loop_block(c: *cy.Chunk, stmt: *ir.LoopBlock, node: *ast.Node) !void {
    const top_pc = c.buf.ops.items.len;
    const jump_start: u32 = @intCast(c.blockJumpStack.items.len);

    try pushBlock(c, node);
    {
        try genStmts(c, stmt.body_head);
        c.blockJumpStack.items.len = c.patchForBlockJumps(jump_start, c.cur_proc.block_depth-1, c.buf.ops.items.len, top_pc);
    } 
    try popLoopBlock(c);
}

fn gen_for_range_stmt(c: *Chunk, stmt: *ir.ForRangeStmt, node: *ast.Node) !void {
    // Reserve vars until end of block, hidden from user.
    const counter = try reserve_reg(c, c.sema.i64_t);
    const cond = try reserve_reg(c, c.sema.bool_t);
    const rangeEnd = try bc.reserve_reg(c, c.sema.i64_t);

    // Range start.
    const startv = try genExpr(c, stmt.start, Cstr.localOrTemp());

    // Range end.
    const endv = try genExpr(c, stmt.end, Cstr.toTemp(rangeEnd));
    _ = endv;

    // Initialize counter.
    try push_mov(c, counter, startv.reg, 1, node);

    // Begin sub-block.
    try pushBlock(c, node);

    const iter_pc = c.buf.ops.items.len;

    // Perform comparison.
    if (stmt.increment) {
        if (stmt.end_inclusive) {
            try pushInlineBinExpr(c, .le, GenValue.initTemp(cond), counter, rangeEnd, node);
        } else {
            try pushInlineBinExpr(c, .lt, GenValue.initTemp(cond), counter, rangeEnd, node);
        }
    } else {
        if (stmt.end_inclusive) {
            try pushInlineBinExpr(c, .ge, GenValue.initTemp(cond), counter, rangeEnd, node);
        } else {
            try pushInlineBinExpr(c, .gt, GenValue.initTemp(cond), counter, rangeEnd, node);
        }
    }

    // Jump to end if cond is false.
    const exit_jump = try c.pushEmptyJumpNotCond(cond, node);

    // Copy counter.
    // TODO: Shouldn't be necessary since the counter will be read-only.
    var eachLocal: Reg = undefined;
    if (stmt.has_each_local) {
        try genStmts(c, stmt.declHead);
        eachLocal = toLocalReg(c, stmt.eachLocal);
        try push_mov(c, eachLocal, counter, 1, node);
    }

    const jumpStackSave = c.blockJumpStack.items.len;

    try genStmts(c, stmt.bodyHead);

    const block_offset = c.cur_proc.block_depth - 1;
    try popLoopBlock(c);

    const cont_pc = c.buf.ops.items.len;

    // Update counter.
    var inc: u16 = undefined;
    if (stmt.increment) {
        inc = 1;
    } else {
        inc = @bitCast(@as(i16, -1));
    }
    try c.pushCode(.add_i16, &.{ counter, counter, inc}, node);

    try c.pushJumpBackTo(iter_pc, node);
    c.patchJumpNotCondToCurPc(exit_jump);

    c.blockJumpStack.items.len = c.patchForBlockJumps(jumpStackSave, block_offset, c.buf.ops.items.len, cont_pc);

    try pop_regs(c, counter, node);
}

fn verbose(c: *cy.Chunk, stmt: *ir.Verbose, node: *ast.Node) !void {
    _ = c;
    _ = node;
    cc.setVerbose(stmt.verbose);
}

fn gen_try_stmt(c: *cy.Chunk, stmt: *ir.TryStmt, node: *ast.Node) !void {
    try pushBlock(c, node);
    try genStmts(c, stmt.bodyHead);
    try popBlock(c);

    const catch_pc = c.buf.ops.items.len;
    _ = catch_pc;
    // try c.pushCode(.catch_op, &.{0, 0, 0, 0}, node);

    try pushBlock(c, node);
    try genStmts(c, stmt.catchBodyHead);
    var errReg: Reg = cy.NullU8;
    if (stmt.hasErrLocal) {
        errReg = toLocalReg(c, stmt.errLocal);
    }
    try popBlock(c);

    // // Patch pushTry with errReg.
    // c.buf.setOpArgs1(catch_pc + 3, errReg);

    // c.buf.setOpArgU16(catch_pc + 1, @intCast(c.buf.ops.items.len - catch_pc));
}

fn gen_if_block(c: *cy.Chunk, stmt: *ir.IfBlock, node: *ast.Node) !void {
    const cond_n = stmt.cond_expr.node;
    const condv = try genExpr(c, stmt.cond_expr, Cstr.localOrTemp());
    const jump_miss = try c.pushEmptyJumpNotCond(condv.reg, node);

    try pushBlock(c, node);
    try genStmts(c, stmt.body_head);
    try popBlock(c);

    c.patchJumpNotCondToCurPc(jump_miss);

    try pop_temp_value(c, condv, cond_n);
}

fn elseBlocks(c: *cy.Chunk, else_head: ?*ir.Expr) !void {
    const bodyEndJumpsStart = c.listDataStack.items.len;
    defer c.listDataStack.items.len = bodyEndJumpsStart;

    var else_opt = else_head;
    while (else_opt) |else_block| {
        const else_nid = else_block.node;
        const else_b = else_block.cast(.else_block);

        if (else_b.cond_expr != null) {
            if (else_b.cond_head != null) {
                try pushBlock(c, else_nid);
                try genStmts(c, else_b.cond_head);
                try popBlock(c);
            }

            const condNodeId = else_b.cond_expr.?.node;
            const condv = try genExpr(c, else_b.cond_expr.?, Cstr.localOrTemp);

            const jump_miss = try c.pushEmptyJumpNotCond(condv.reg);

            try pushBlock(c, else_nid);
            try genStmts(c, else_b.body_head);
            try popBlock(c);

            if (else_b.else_block != null) {
                const jump_end = try c.pushEmptyJump();
                try c.listDataStack.append(c.alloc, .{ .pc = jump_end });
            }

            c.patchJumpNotCondToCurPc(jump_miss);

            try pop_temp_value(c, condv, condNodeId);
        } else {
            try pushBlock(c, else_nid);
            try genStmts(c, else_b.body_head);
            try popBlock(c);
        }
        else_opt = else_b.else_block;
    }

    // Jump here from all body ends.
    const bodyEndJumps = c.listDataStack.items[bodyEndJumpsStart..];
    for (bodyEndJumps) |jump| {
        c.patchJumpToCurPc(jump.pc);
    }
}

fn gen_switch_stmt(c: *Chunk, stmt: *ir.SwitchStmt, node: *ast.Node) !void {
    const childBreakJumpsStart: u32 = @intCast(c.blockJumpStack.items.len);
    _ = childBreakJumpsStart;

    const cases = stmt.cases[0..stmt.numCases];

    const caseBodyEndJumpsStart = c.listDataStack.items.len;

    var condMatchJumpsStart = c.listDataStack.items.len;

    var prevCaseMissJump: u32 = cy.NullId;
    for (cases) |case| {
        const case_n = case.node;
        const case_data = case.cast(.switch_case);
        const isElse = case_data.numConds == 0;

        // Jump here from prev case miss.
        if (prevCaseMissJump != cy.NullId) {
            c.patchJumpToCurPc(prevCaseMissJump);
        }

        if (!isElse) {
            // const condMatchJumpsStart = c.listDataStack.items.len;
            const conds = case_data.conds[0..case_data.numConds];
            for (conds) |cond_loc| {
                const cond = cond_loc.cast(.case_cond);
                const cond_n = cond_loc.node;

                try genStmts(c, cond.body_head);
                const condv = try genExpr(c, cond.expr, Cstr.localOrTemp());

                const miss_jump = try c.pushEmptyJumpNotCond(condv.reg, cond_n);
                const match_jump = try c.pushEmptyJump(cond_n);
                try c.listDataStack.append(c.alloc, .{ .pc = match_jump });
                c.patchJumpNotCondToCurPc(miss_jump);
                // Miss continues to next cond.

                try pop_temp_value(c, condv, cond_n);
            }

            // No cond matches. Jump to next case.
            prevCaseMissJump = try c.pushEmptyJump(case_n);

            if (!case_data.fallthrough) {
                // Jump here from all matching conds.
                for (c.listDataStack.items[condMatchJumpsStart..]) |pc| {
                    c.patchJumpToCurPc(pc.pc);
                }
                c.listDataStack.items.len = condMatchJumpsStart;
            } else {
                continue;
            }
        } else {
            prevCaseMissJump = cy.NullId;
        }

        try pushBlock(c, case_n);
        try genStmts(c, case_data.body_head);
        try popBlock(c);

        const caseBodyEndJump = try c.pushEmptyJump(node);
        try c.listDataStack.append(c.alloc, .{ .jumpToEndPc = caseBodyEndJump });

        condMatchJumpsStart = c.listDataStack.items.len;
    }

    // Jump here from prev case miss.
    if (prevCaseMissJump != cy.NullId) {
        c.patchJumpToCurPc(prevCaseMissJump);
    }

    // Jump here from case body ends.
    for (c.listDataStack.items[caseBodyEndJumpsStart..]) |pc| {
        c.patchJumpToCurPc(pc.jumpToEndPc);
    }
    c.listDataStack.items.len = caseBodyEndJumpsStart;
}

const ProcType = enum(u8) {
    main,
    func,
};

pub const Sym = union {
    varSym: struct {
        id: u32,
    },
    func: struct {
        static_pc: [*]cy.Inst,

        id: u32,
        // Offset from chunk's buffer to be used afterwards for relocation.
        pc: u32,
        end_pc: u32,
    },
    hostFunc: struct {
        id: u32,
        ptr: vmc.HostFn,
    },
    extern_func: bc.ExternFunc,
};

pub fn pushFuncBlock(c: *Chunk, data: *ir.FuncBlock, params: []align(1) const ir.FuncParam, node: *ast.Node) !void {
    log.tracev("push func block: params={}, method={}, {}", .{data.func.sig.params_len, data.func.isMethod(), node});
    try pushFuncBlockCommon(c, params, data.func, node);
}

fn pushFuncBlockCommon(c: *Chunk, params: []align(1) const ir.FuncParam, func: *cy.Func, node: *ast.Node) !void {
    const src_chunk = func.decl.?.src();
    try pushProc(c, .func, src_chunk, func, func.name(), node);

    // `reserveFuncRegs` may emit copy and box insts.
    var ret_t = func.sig.ret;
    if (func.info.generator) {
        const gen_t = ret_t.cast(.pointer).child_t;
        const val_t = gen_t.sym().instance.?.params[0].asPtr(*cy.Type).cast(.func_ptr).sig.ret;
        const next_t = try cy.sema.getOptionType(c, val_t);
        if (next_t.size() > ret_t.size()) {
            ret_t = next_t;
        }
    }
    try reserveFuncSlots(c, params, ret_t);
}

pub fn popFuncBlockCommon(c: *Chunk, func: *cy.Func) !void {
    _ = func;

    try popProc(c);
}

fn gen_lambda(c: *Chunk, expr: *ir.Lambda, cstr: Cstr, node: *ast.Node) !GenValue {
    const func = expr.func;

    const dst = try bc.selectReg(c, expr.base.type, cstr, node);

    const rt_id = try ensureFunc(c.compiler, func);
    _ = rt_id;
    if (expr.numCaptures == 0) {
        try gen_funcptr2(c, dst.reg, func, node);
    } else {
        const captures = expr.captures[0..expr.numCaptures];
        const start = c.buf.ops.items.len;
        const len_data: u16 = @as(u16, @intCast(captures.len)) | (@as(u16, @intFromBool(expr.pinned_closure)) << 15);
        try c.pushCodeExt(.closure, &.{
            dst.reg, @intCast(expr.base.type.id()), 0, 0, 0, len_data, 
        }, captures.len, node);
        try c.relocations.append(c.alloc, .{.kind = .func, .pc = start + 3, .data = .{.func = func}});
        for (captures, 0..) |irVar, i| {
            const reg = toLocalReg(c, irVar);
            c.buf.ops.items[start + 7 + i].val = reg;
        }
    }
    return dst;
}

fn gen_pop_locals(c: *Chunk, stmt: *ir.PopLocals, node: *ast.Node) !void {
    _ = node;
    const reg_start = c.bc_local_map.items[stmt.local_end];
    c.bc_local_map.items.len = stmt.local_end;

    var num_ptr_locals: usize = 0;
    for (c.buf.ptr_layout_builder.items[reg_start..c.cur_proc.reg_end]) |ptr| {
        if (ptr) num_ptr_locals += 1;
    }
    if (num_ptr_locals > 0) {
        c.cur_proc.num_ptr_locals -= num_ptr_locals;
    }
    c.cur_proc.reg_end = reg_start;
}

fn gen_block(c: *Chunk, stmt: *ir.Block, node: *ast.Node) !void {
    try pushBlock(c, node);
    const jump_stack_start = c.blockJumpStack.items.len;
    try genStmts(c, stmt.bodyHead);
    c.blockJumpStack.items.len = c.patchBreaks(jump_stack_start, c.cur_proc.block_depth - 1, c.buf.ops.items.len);
    try popBlock(c);
}

pub fn pushProc(c: *Chunk, btype: ProcType, src_chunk: u32, func: ?*cy.Func, name: []const u8, debugNode: *ast.Node) !void {
    const id = c.funcs_debug.items.len;
    try c.funcs_debug.append(c.alloc, .{ .func = func, .name = name, .src_chunk = src_chunk });
    try c.proc_stack.append(c.alloc, Proc.init(btype, id, c.id));
    c.cur_proc = &c.proc_stack.items[c.proc_stack.items.len-1];
    c.cur_proc.debugNode = debugNode;

    try pushBlock(c, debugNode);
}

pub fn popProc(c: *Chunk) !void {
    log.tracev("pop gen block", .{});

    try popBlock(c);

    std.debug.assert(c.cur_proc.reg_end == 0);

    var last = c.proc_stack.pop().?;
    std.debug.assert(last.num_ptr_locals == 0);

    last.deinit(c.alloc);
    if (c.proc_stack.items.len > 0) {
        c.cur_proc = &c.proc_stack.items[c.proc_stack.items.len-1];
    }
}

pub fn pushBlock(c: *Chunk, node: *ast.Node) !void {
    log.tracev("push block {}", .{c.cur_proc.block_depth});
    c.cur_proc.block_depth += 1;

    try c.blocks.append(c.alloc, .{
        .node = node,
        .local_start = c.bc_local_map.items.len,
        .reg_start = c.cur_proc.reg_end,
        .num_ptr_locals = 0,
        .saved_num_ptr_locals = c.cur_proc.num_ptr_locals,
    });

    if (cy.Trace) {
        c.indent += 1;
    }
}

pub fn popBlock(c: *Chunk) !void {
    log.tracev("pop block {}", .{c.cur_proc.block_depth});

    const b = c.blocks.pop().?;
    if (c.cur_proc.num_ptr_locals > b.saved_num_ptr_locals) {
        @memset(c.buf.ptr_layout_builder.items[b.reg_start..c.cur_proc.reg_end], false);
    }

    c.bc_local_map.items.len = b.local_start;
    c.cur_proc.reg_end = b.reg_start;
    c.cur_proc.num_ptr_locals = b.saved_num_ptr_locals;
    c.cur_proc.block_depth -= 1;

    if (cy.Trace) {
        c.indent -= 1;
    }
}

pub fn popLoopBlock(c: *Chunk) !void {
    try popBlock(c);
}

pub const Block = struct {
    node: *ast.Node,

    local_start: usize,
    reg_start: Reg,

    // Alive pointer locals.
    num_ptr_locals: usize,

    saved_num_ptr_locals: usize,

    last_ptr_layout: []const bool = &.{},
    last_ptr_layout_id: usize = 0,
};

pub const Proc = struct {
    type: ProcType,

    /// Index into `Chunk.funcs_debug`.
    id: u32,

    /// Chunk that the function belongs to.
    chunk_id: u32,

    /// If the function body belongs to a closure, the callee reg
    /// contains the closure's value which is then used to perform captured var lookup.
    closure_local: Reg,

    /// End of the current reg range.
    reg_end: Reg,

    ret_size: u16,

    /// Track the max number of registers used to know the stack size required by the block.
    max_regs: Reg,

    debugNode: *ast.Node,

    block_depth: u32,

    // Alive pointer locals.
    num_ptr_locals: usize,

    /// LLVM
    // funcRef: if (cy.hasJIT) llvm.ValueRef else void = undefined,

    fn init(btype: ProcType, id: usize, chunk_id: u32) Proc {
        return .{
            .id = @intCast(id),
            .chunk_id = chunk_id,
            .type = btype,
            .closure_local = cy.NullU8,
            .max_regs = 0,
            .reg_end = 0,
            .ret_size = 0,
            .debugNode = undefined,
            .block_depth = 0,
            .num_ptr_locals = 0,
        };
    }

    fn deinit(self: *Proc, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }
};

fn genConst64_(c: *cy.Chunk, val: u64, dst: Reg, node: *ast.Node) !void {
    const pc = c.buf.len();
    try c.pushCode(.const_64, &.{ dst, 0, 0, 0, 0 }, node);
    c.buf.setOpArgU64(pc + 2, val);
}

const LocalId = u8;

const GenValueType = enum {
    discard,
    constant,
    local,
    temp,
};

/// Returned from `genExpr`.
pub const GenValue = struct {
    type: GenValueType = undefined,

    /// Where the value was generated to.
    reg: Reg = undefined,

    data: union {
        constant: struct {
            val: cy.Value,
        },
    } = undefined,

    pub fn initConstant(val: cy.Value) GenValue {
        return .{ .type = .constant, 
            .reg = undefined, 
            .data = .{ .constant = .{
                .val = val,
            }
        }};
    }

    fn discard() GenValue {
        return .{
            .type = .discard,
            .reg = 255,
        };
    }

    pub fn initLocal(reg: Reg) GenValue {
        return .{
            .type = .local,
            .reg = reg,
        };
    }

    pub fn initTemp(reg: Reg) GenValue {
        return .{
            .type = .temp,
            .reg = reg,
        };
    }

    pub fn isTemp(self: GenValue) bool {
        return self.type == .temp;
    }
};

fn pushRelease(c: *Chunk, slot_id: Reg, optional: bool, deinit_obj: *cy.Func, node: *ast.Node) !void {
    const pc = c.buf.ops.items.len;
    if (optional) {
        try c.pushCode(.release_opt, &.{slot_id, 0}, node);
    } else {
        try c.pushCode(.release, &.{slot_id, 0}, node);
    }

    // const name = try c.sema.allocTypeName(val_t);
    // defer c.alloc.free(name);
    // try genNopLabel2(c, name, node);

    const ret = c.cur_proc.reg_end;
    try push_mov(c, ret + 4, slot_id, 1, node);
    // const func = try ensureFunc(c.compiler, deinit_obj);
    try pushCall(c, @intCast(ret), deinit_obj, node);

    // Patch jump.
    c.buf.ops.items[pc + 2].val = @intCast(c.buf.ops.items.len - pc);
}

fn gen_const64(c: *Chunk, expr: *ir.Const64, cstr: Cstr, node: *ast.Node) !GenValue {
    const dst = try bc.selectReg(c, expr.base.type, cstr, node);
    switch (expr.base.type.id()) {
        bt.I64 => {
            try genConstInt(c, @bitCast(expr.val), dst, node);
        },
        else => {
            try genConst64_(c, expr.val, dst.reg, node);
        },
    }
    return dst;
}

fn genConstInt(c: *Chunk, val: i64, dst: GenValue, node: *ast.Node) !void {
    // TODO: Can be constU8.
    if (val >= 0 and val <= std.math.maxInt(i16)) {
        try c.pushCode2(.const_16s, dst.isTemp(), &.{ dst.reg, @bitCast(@as(i16, @intCast(val))) }, node);
        return;
    }
    // const idx = try c.buf.getOrPushConst(false, cy.Value.initInt(@intCast(val)));
    try genConst64_(c, @bitCast(val), dst.reg, node);
}

fn get_uniq_ptr_layout(c: *cy.Chunk, layout: []const bool) !usize {
    const b = c.genBlock();
    if (std.mem.eql(bool, b.last_ptr_layout, layout)) {
        return b.last_ptr_layout_id;
    }

    const res = try c.buf.ptr_layout_map.getOrPut(c.alloc, layout);
    if (!res.found_existing) {
        const dupe = try c.alloc.dupe(bool, layout);
        const id = c.buf.ptr_layouts.items.len;
        try c.buf.ptr_layouts.append(c.alloc, dupe);
        res.key_ptr.* = dupe;
        res.value_ptr.* = id;
    }
    b.last_ptr_layout = res.key_ptr.*;
    b.last_ptr_layout_id = res.value_ptr.*;
    return res.value_ptr.*;
}

fn pushCall(c: *cy.Chunk, base: Reg, func: *cy.Func, node: *ast.Node) !void {
    const start = c.buf.ops.items.len;
    switch (func.type) {
        .userFunc => {
            try c.pushFCode(.call, &.{ base, 0, 0, 0 }, node);
            try c.relocations.append(c.alloc, .{.kind = .func, .pc = start + 2, .data = .{.func = func}});
        },
        .hostFunc => {
            try c.pushFCode(.call_host, &.{ base, 0, 0, 0 }, node);
            c.buf.setOpArgU48(start + 2, @intCast(@intFromPtr(func.data.hostFunc.ptr)));
        },
        .extern_ => {
            try c.pushFCode(.call_host, &.{ base, 0, 0, 0 }, node);
            try c.rt_relocations.append(c.alloc, .{.kind = .func, .pc = start + 2, .data = .{.func = func}});
        },
        .vm_extern_variant => {
            try c.pushFCode(.call_host, &.{ base, 0, 0, 0 }, node);
            try c.rt_relocations.append(c.alloc, .{.kind = .func, .pc = start + 2, .data = .{.func = func}});
        },
        else => {
            std.debug.panic("Unexpected: {}", .{func.type});
        },
    } 

    if (c.cur_proc.num_ptr_locals > 0) {
        const layout = try get_uniq_ptr_layout(c, c.buf.ptr_layout_builder.items);
        try c.buf.ptr_table.append(c.alloc, .{
            .pc = @intCast(start),
            .layout = @intCast(layout),
        });
    }
}

fn pushInlineUnExpr(c: *cy.Chunk, code: cy.OpCode, dst: Reg, child: Reg, node: *ast.Node) !void {
    try c.pushFCode(code, &.{ dst, child }, node);
}

fn pushInlineBinExpr(c: *cy.Chunk, code: cy.OpCode, dst: GenValue, left: Reg, right: Reg, node: *ast.Node) !void {
    try c.pushFCode2(code, dst.isTemp(), &.{ dst.reg, left, right }, node);
}

fn mainEnd(c: *cy.Chunk, node: *ast.Node) !void {
    try c.pushCode(.end, &.{}, node);
}

fn getIntUnaryOpCode(op: cy.UnaryOp) cy.OpCode {
    return switch (op) {
        .bitwiseNot => .not,
        .minus => .neg,
        else => cy.fatal(),
    };
}

fn getFloatUnaryOpCode(op: cy.UnaryOp) cy.OpCode {
    return switch (op) {
        .minus => .fneg,
        else => cy.fatal(),
    };
}

fn getFloatOpCode(op: cy.BinaryExprOp) cy.OpCode {
    return switch (op) {
        .less => .flt,
        .greater => .fgt,
        .less_equal => .fle,
        .greater_equal => .fge,
        .minus => .fsub,
        .plus => .fadd,
        .slash => .fdiv,
        .percent => .fmod,
        .star => .fmul,
        else => cy.fatal(),
    };
}

fn getIntOpCode(op: cy.BinaryExprOp) cy.OpCode {
    return switch (op) {
        .less => .lt,
        .greater => .gt,
        .less_equal => .le,
        .greater_equal => .ge,
        .minus => .sub,
        .plus => .add,
        .slash => .div,
        .percent => .mod,
        .star => .mul,
        .pow => .pow,
        .bitwiseAnd => .and_,
        .bitwiseOr => .or_,
        .bitwiseXor => .xor,
        .bitwiseLeftShift => .lsl,
        .bitwiseRightShift => .lsr,
        else => cy.fatal(),
    };
}

fn push_new(c: *cy.Chunk, dst: Reg, type_: *cy.Type, val_size: u16, node: *ast.Node) !void {
    try c.pushCode(.new, &.{ dst, @intCast(type_.id()), val_size }, node);
}

fn push_struct_init_to(c: *cy.Chunk, dst_ptr: Reg, fields: []const cy.types.Field, args: []const Reg, node: *ast.Node) !void {
    for (fields, 0..) |field, i| {
        try push_store(c, dst_ptr, @intCast(field.offset), args[i], field.type, node);
    }
}

fn push_new_init(c: *cy.Chunk, dst: Reg, type_: *cy.Type, val_size: u16, fields: []const cy.types.Field, args: []const Reg, node: *ast.Node) !void {
    try c.pushCode(.new, &.{ dst, @intCast(type_.id()), val_size }, node);
    for (fields, 0..) |field, i| {
        try push_store(c, dst, @intCast(field.offset), args[i], field.type, node);
    }
}

/// Selecting for a non local inst with a dst operand.
/// A required dst can not be retained or a temp register is allocated and `requiresCopyRelease` will be set true.
///
/// Handles the case where the inst depends and assigns to the same local:
/// > let node = [Node ...]
/// > node = node.val
/// `node` rec gets +1 retain and saves to temp since the `node` cstr has `releaseDst=true`.
pub fn selectForDstInst(c: *cy.Chunk, cstr: Cstr, node: *ast.Node) !DstInst {
    switch (cstr.type) {
        .captured => {
            return .{
                .dst = try bc.reserveTemp(c, cstr.data.captured.is_stack_ptr),
                .cstr = cstr,
                .node = node,
            };
        },
        .localReg,
        .tempReg => {
            return .{
                .dst = cstr.data.slot.dst,
                .cstr = cstr,
                .node = node,
            };
        },
        .simple => {
            return .{
                .dst = try bc.reserveTemp(c, cstr.data.simple.is_stack_ptr),
                .cstr = cstr,
                .node = node,
            };
        },
        .none => return error.Unsupported,
    }
}

pub fn selectReg(c: *Chunk, val_t: *cy.Type, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = node;
    switch (cstr.type) {
        .localReg => {
            return GenValue.initLocal(cstr.data.slot.dst);
        },
        .tempReg => {
            return GenValue.initTemp(cstr.data.slot.dst);
        },
        .simple => {
            const reg = try reserve_reg(c, val_t);
            return GenValue.initTemp(reg);
        },
        .discard => {
            return GenValue.discard();
        },
        else => {
            return error.Unsupported;
        },
    }
}

pub const DstInst = struct {
    dst: Reg,
    cstr: Cstr,
    node: *ast.Node,
};

pub const NoErrInst = struct {
    dst: Reg,
    requiresPreRelease: bool,
    cstr: Cstr,
    has_final_dst: bool,
    node: *ast.Node,
};

const CstrType = enum(u8) {
    /// 1. Prefers local register first.
    /// 2. Allocates the next temp.
    simple,

    /// To a temp register.
    tempReg,

    /// To a local register.
    localReg,

    /// Captured.
    captured,

    discard,

    /// No register selection.
    none,
};

/// Constraints on where a value should go to.
///
/// Some constraints have a `retain` flag.
/// If `retain` is true, the caller expects the value to have increased it's RC by 1.
///     * If the value already has a +1 retain (e.g. a lifted rvalue), the requirement is satisfied.
///     * If the value comes from a local, then a retain copy is generated.
///     * If the value does not have a RC, the requirement is satisfied.
/// If `retain` is false, the value can still be retained by +1 to keep it alive.
pub const Cstr = struct {
    type: CstrType,

    data: union {
        simple: struct {
        },
        slot: struct {
            dst: Reg,
        },
        varSym: struct {
            // Runtime id.
            id: u32,
        },
        captured: struct {
            is_stack_ptr: bool,
            idx: u8,
        },
        uninit: void,
    } = .{ .uninit = {} },

    /// TODO: provide hint whether the allocated reill be used or not.
    /// e.g. For expr statements, the top level expr reg isn't used.
    /// If it's not used and the current expr does not produce side-effects, it can omit generating its code.

    pub const none = Cstr{
        .type = .none,
    };

    pub fn localOrTemp() Cstr {
        return .{
            .type = .simple,
            .data = .{ .simple = .{}},
        };
    }

    pub const ret = Cstr{ .type = .localReg, .data = .{ .slot = .{
        .dst = 0,
    }}};

    pub fn discard() Cstr {
        return .{ .type = .discard, .data = undefined };
    }

    pub fn toCaptured(idx: u8, is_stack_ptr: bool) Cstr {
        return .{ .type = .captured, .data = .{ .captured = .{
            .is_stack_ptr = is_stack_ptr,
            .idx = idx,
        }}};
    }

    pub fn toTemp(reg: Reg) Cstr {
        return .{ .type = .tempReg, .data = .{ .slot = .{
            .dst = reg,
        }}};
    }

    pub fn toLocal(reg: Reg) Cstr {
        return .{ .type = .localReg, .data = .{ .slot = .{
            .dst = reg,
        }}};
    }
};

pub fn pop_temp_value(c: *Chunk, val: GenValue, node: *ast.Node) !void {
    if (val.type == .temp) {
        try pop_regs(c, val.reg, node);
    }
}

pub fn pop_regs(c: *Chunk, end: Reg, node: *ast.Node) !void {
    log.tracev("trunc regs to: {}, cur={}", .{end, c.cur_proc.reg_end});
    if (end > c.cur_proc.reg_end) {
        return c.reportErrorFmt("Expected to trunc regs to {}, but found {}.", &.{v(end), v(c.cur_proc.reg_end)}, node);
    }
    if (end + 1 == c.cur_proc.reg_end) {
        if (c.buf.ptr_layout_builder.items[end]) {
            c.cur_proc.num_ptr_locals -= 1;
        }
        c.buf.ptr_layout_builder.items.len -= 1;
    } else {
        var num_stack_ptrs: usize = 0;
        for (c.buf.ptr_layout_builder.items[end..c.cur_proc.reg_end]) |ptr| {
            if (ptr) {
                num_stack_ptrs += 1;
            }
        }
        if (num_stack_ptrs > 0) {
            c.cur_proc.num_ptr_locals -= num_stack_ptrs;
        }
        c.buf.ptr_layout_builder.items.len -= c.cur_proc.reg_end - end;
    }
    c.cur_proc.reg_end = end;
}

pub const RelocationKind = enum(u8) {
    func,
    extern_func_ptr,
    global,
};

pub const Relocation = struct {
    kind: RelocationKind,
    // relative pos to a *align(2) u48
    pc: usize,
    data: union {
        extern_func_ptr: *cy.types.FuncPtr,
        func: *cy.Func,
        global: usize,
    },
}; 

fn push_zext(c: *cy.Chunk, dst: Reg, src: Reg, bits: u8, node: *ast.Node) !void {
    try c.pushCode(.zext, &.{ dst, src, bits }, node);
}

fn push_sext(c: *cy.Chunk, dst: Reg, src: Reg, bits: u8, node: *ast.Node) !void {
    try c.pushCode(.sext, &.{ dst, src, bits }, node);
}
