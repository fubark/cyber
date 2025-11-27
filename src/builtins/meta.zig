const std = @import("std");
const builtin = @import("builtin");
const build_config = @import("build_config");
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const bt = cy.types.BuiltinTypes;
const zErrFunc = cy.core.zErrFunc;
const cFunc = cy.core.cFunc;
const zErrCtFunc = cy.core.zErrCtFunc;
const sema = cy.sema;
const sema_type = cy.sema_type;
const ir = cy.ir;
const v = cy.fmt.v;
const ast = cy.ast;
const Value = cy.Value;
const TypeValue = cy.TypeValue;

pub const Src = @embedFile("meta.cy");

const types = [_]struct{[]const u8, C.BindType}{
    .{"type",           C.TYPE_CREATE(createTypeType)},
    .{"PartialStructLayout", C.TYPE_CREATE(create_partial_struct_layout_type)},
    .{"Code",           C.TYPE_CREATE(createCodeType)},

    .{"CasePayload",    C.TYPE_ALIAS(resolveCasePayload)},
    .{"OptionChild",    C.TYPE_ALIAS(resolveOptionChild)},
    .{"ResultChild",    C.TYPE_ALIAS(resolveResultChild)},
};

const funcs = [_]struct{[]const u8, C.BindFunc}{
    .{"stack_trace",     zErrFunc(stack_trace)},
    .{"get_closure_data", zErrFunc(get_closure_data)},
    .{"access",          zErrCtFunc(access, null)},
    .{"access_result_payload", zErrCtFunc(access_result_payload, null)},
    .{"access_result_error", zErrCtFunc(access_result_error, null)},
    .{"access_choice_case", zErrCtFunc(access_choice_case, null)},
    .{"access_option_payload", zErrCtFunc(access_option_payload, null)},
    .{"event",           zErrFunc(event)},
    .{"choice_tag",      zErrCtFunc(choice_tag, choice_tag_eval)},
    .{"init_choice",     zErrCtFunc(init_choice, null)},
    .{"is_none",         zErrCtFunc(is_none, null)},
    .{"is_result_error", zErrCtFunc(is_result_error, null)},
    .{"enum_int_values", zErrCtFunc(null, enum_int_values)},
    .{"enum_values",     zErrCtFunc(null, enum_values)},
    .{"enum_case",       zErrCtFunc(null, enum_case)},
    .{"eval",            zErrCtFunc(eval, null)},
    .{"log",             zErrCtFunc(null, log_)},
    .{"error",           zErrCtFunc(null, error_)},
    .{"init_type",       zErrCtFunc(init_type, init_type_eval)},
    .{"cy_full_version", zErrCtFunc(null, cy_full_version)},
    .{"build_flag",      zErrCtFunc(null, build_flag)},
    .{"build_option",    zErrCtFunc(null, build_option)},
    .{"build_mode",      zErrCtFunc(null, build_mode)},
    .{"mod_uri",         zErrCtFunc(null, mod_uri)},
    .{"load",            zErrCtFunc(null, load_)},
    .{"cpu",             zErrCtFunc(null, cpu)},
    .{"system",          zErrCtFunc(null, system)},
    .{"is_vm_target",    zErrCtFunc(null, is_vm_target)},
    .{"is_inline_eval",  zErrCtFunc(null, is_inline_eval)},
    .{"new_struct",      zErrCtFunc(null, new_struct)},
    .{"endian",          zErrCtFunc(null, endian_)},
    .{"has_decl",        zErrCtFunc(null, has_decl)},
    .{"reachable",       zErrCtFunc(null, reachable)},
    .{"dump_frame",      zErrFunc(dump_frame)},
    .{"trace_retains",   cFunc(trace_retains)},
    .{"trace_releases",  cFunc(trace_releases)},

    // PartialStructLayout
    .{"PartialStructLayout.is_field_active", zErrCtFunc(null, PartialStructLayout_is_field_active)},
    .{"PartialStructLayout.field_layout", zErrCtFunc(null, PartialStructLayout_field_layout)},

    // type
    .{"type.is_instance_of", zErrCtFunc(null, type_is_instance_of)},
    .{"type.is_copyable",  zErrCtFunc(null, type_is_copyable)},
    .{"type.is_const",     zErrCtFunc(null, type_is_const)},
    .{"type.implements",   zErrCtFunc(null, type_implements)},
    .{"type.id",           zErrCtFunc(null, type_id_)},
    .{"type.size",         zErrCtFunc(null, type_size)},
    .{"type.of",           zErrCtFunc(null, type_of)},
    .{"type.@init",        zErrCtFunc(null, type_init)},
    .{"type.info",         zErrCtFunc(null, type_info)},
    .{"type.name",         zErrCtFunc(null, type_name)},
    .{"type.name_rt",      zErrFunc(type_name_rt)},
    .{"type.fn_ret",       zErrCtFunc(null, type_fn_ret)},
    .{"type.struct_state_len", zErrCtFunc(null, type_struct_state_len)},
    .{"type.managed",      zErrCtFunc(null, type_managed)},
    .{"type.field",        zErrCtFunc(null, type_field)},
};

comptime {
    @export(&bind, .{ .name = "cl_mod_bind_meta", .linkage = .strong });
}

pub fn bind(_: *C.VM, mod: *C.Sym) callconv(.c) void {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }

    for (types) |e| {
        C.mod_add_type(mod, e.@"0", e.@"1");
    }
}

fn resolveCasePayload(c_: ?*C.VM, sym_: ?*C.Sym) callconv(.c) ?*C.Sym {
    const c: *cy.Chunk = @ptrCast(@alignCast(c_));
    _ = c;
    const sym: *cy.sym.TypeAlias = @ptrCast(@alignCast(sym_));
    const instance = sym.instance.?;
    const type_ = instance.getParamValue("ChoiceT").?.asPtr(*cy.Type);
    if (type_.kind() != .choice) {
        return null;
    }
    const tag = instance.getParamValue("Tag").?.asInt();

    const case = type_.cast(.choice).getCaseByTag(@intCast(tag));
    return @ptrCast(case.payload_t.sym());
}

fn resolveOptionChild(c_: ?*C.VM, sym_: ?*C.Sym) callconv(.c) ?*C.Sym {
    const c: *cy.Chunk = @ptrCast(@alignCast(c_));
    _ = c;
    const sym: *cy.sym.TypeAlias = @ptrCast(@alignCast(sym_));
    const instance = sym.instance.?;
    const type_ = instance.getParamValue("OptionT").?.asPtr(*cy.Type);
    if (type_.kind() != .option) {
        return null;
    }
    return @ptrCast(type_.cast(.option).child_t.sym());
}

fn resolveResultChild(c_: ?*C.VM, sym_: ?*C.Sym) callconv(.c) ?*C.Sym {
    const c: *cy.Chunk = @ptrCast(@alignCast(c_));
    _ = c;
    const sym: *cy.sym.TypeAlias = @ptrCast(@alignCast(sym_));
    const instance = sym.instance.?;
    const type_ = instance.getParamValue("Result").?.asPtr(*cy.Type);
    if (type_.kind() != .result) {
        return null;
    }
    return @ptrCast(type_.cast(.result).child_t.sym());
}

fn create_partial_struct_layout_type(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    const new_t = c.sema.createTypeWithId(.pointer, bt.PartialStructLayout, .{ .ref = false, .child_t = c.sema.void_t }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createCodeType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    const new_t = c.sema.createTypeWithId(.pointer, bt.Code, .{ .ref = false, .child_t = c.sema.void_t }) catch @panic("error");
    new_t.info.ct = true;
    return @ptrCast(new_t);
}

fn createTypeType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.pointer, bt.Type, .{ .ref = false, .child_t = c.sema.void_t }) catch @panic("error");
    new_t.info.ct = true;
    return @ptrCast(new_t);
}

pub fn stack_trace(t: *cy.Thread) anyerror!C.Ret {
    // Append frames from current call-site.
    try t.recordCurFrames();

    // Remove top two frames since it contains the `stackTrace` call.
    _ = t.compact_trace.pop();
    _ = t.compact_trace.pop();

    const trace = try cy.debug.allocStackTrace(t.c.vm, t.c.stack(), t.compact_trace.items);
    defer t.alloc.free(trace);

    var buf = std.Io.Writer.Allocating.init(t.alloc);
    defer buf.deinit();

    try cy.debug.writeStackFrames(t.c.vm, &buf.writer, trace);

    const ret = t.ret(cy.heap.Str);
    ret.* = try t.heap.init_str(try buf.toOwnedSlice());
    return C.RetOk;
}

pub fn get_closure_data(t: *cy.Thread) !C.Ret {
    const ret = t.ret(?*anyopaque);
    const func = t.param(*cy.heap.Func);
    if (func.kind == .closure) {
        ret.* = &func.data;
    } else {
        ret.* = null;
    }
    return C.RetOk;
}

pub fn access(c: *cy.Chunk, ctx: *cy.CtFuncContext, res: *sema.ExprResult) !void {
    const rec_n = ctx.args[0].asPtr(*cy.ast.Node);
    const name = ctx.args[1].asPtr(*cy.heap.Str).slice();
    defer c.heap.release(ctx.args[1]);
    res.* = try sema.semaAccessExpr2(c, rec_n, name, ctx.node, false, null);
}

pub fn access_result_error(c: *cy.Chunk, ctx: *cy.CtFuncContext, res: *sema.ExprResult) !void {
    const result_n = ctx.args[0].asPtr(*cy.ast.Node);
    const result = try c.semaExpr(result_n, .{});

    const result_t = result.type.getBaseType();

    const field = try c.ir.newExpr(.field, c.sema.error_t, ctx.node, .{
        .idx = 1,
        .rec = result.ir,
    });
    const case = try c.ir.newExpr(.case, c.sema.error_t, ctx.node, .{
        .union_t = result_t,
        .case = 1,
        .child = field,
    });
    res.* = sema.ExprResult.init(case, c.sema.error_t);
    res.addressable = true;
}

pub fn access_result_payload(c: *cy.Chunk, ctx: *cy.CtFuncContext, res: *sema.ExprResult) !void {
    const result_t = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    const result_n = ctx.args[0].asPtr(*cy.ast.Node);
    const result = try c.semaExpr(result_n, .{});

    const field = try c.ir.newExpr(.field, ctx.func.sig.ret, ctx.node, .{
        .idx = 1,
        .rec = result.ir,
    });
    const case = try c.ir.newExpr(.case, ctx.func.sig.ret, ctx.node, .{
        .union_t = result_t,
        .case = 1,
        .child = field,
    });
    res.* = sema.ExprResult.init(case, ctx.func.sig.ret);
    res.addressable = true;
}

pub fn access_option_payload(c: *cy.Chunk, ctx: *cy.CtFuncContext, res: *sema.ExprResult) !void {
    const option_t = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    const option_n = ctx.args[0].asPtr(*cy.ast.Node);
    const option = try c.semaExpr(option_n, .{});

    // Expect type or ref to type.
    if (option.type == option_t) {
        if (option_t.cast(.option).zero_union) {
            res.* = try sema.semaBitcast(c, option, ctx.func.sig.ret, ctx.node);
            return;
        } else {
            res.* = try sema.semaField(c, option, option_n, 1, ctx.func.sig.ret, ctx.node);
            return;
        }
    }

    if (option.type.getRefLikeChild()) |child_t| {
        if (child_t == option_t) {
            if (option_t.cast(.option).zero_union) {
                const ptr_t = try sema.getPtrType(c, ctx.func.sig.ret);
                const ptr = try sema.semaBitcast(c, option, ptr_t, ctx.node);
                res.* = try sema.semaDeref(c, ptr, ctx.node);
                return;
            } else {
                res.* = try sema.semaField(c, option, option_n, 1, ctx.func.sig.ret, ctx.node);
                return;
            }
        }
    }

    const name = try c.sema.allocTypeName(option_t);
    defer c.alloc.free(name);
    return c.reportErrorFmt("Expected `{}` or a reference type.", &.{v(name)}, ctx.node);
}

pub fn access_choice_case(c: *cy.Chunk, ctx: *cy.CtFuncContext, res: *sema.ExprResult) !void {
    const choice_t = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    const choice_n = ctx.args[0].asPtr(*cy.ast.Node);
    const choice = try c.semaExpr(choice_n, .{});

    // Expect type or ref to type.
    if (choice.type != choice_t) {
        if (choice.type.kind() == .pointer) {
            if (choice.type.cast(.pointer).child_t != choice_t) {
                const name = try c.sema.allocTypeName(choice_t);
                defer c.alloc.free(name);
                return c.reportErrorFmt("Expected `{}` or a reference of it.", &.{v(name)}, ctx.node);
            }
        }
    }

    const loc = try c.ir.newExpr(.field, ctx.func.sig.ret, ctx.node, .{
        .idx = 1,
        .rec = choice.ir,
    });
    res.* = sema.ExprResult.init(loc, ctx.func.sig.ret);
    res.addressable = true;
}

pub fn dump_frame(t: *cy.Thread) !C.Ret {
    _ = t.ret(void);

    const prev_pc = t.c.fp[1].retPcPtr;
    const prev_fp = t.c.fp[2].retFramePtr;

    const func_info = cy.debug.get_pc_func_metadata(t.c.vm, prev_pc);
    const frame = prev_fp[0..func_info.stack_size];

    var buf: [1024]u8 = undefined;
    var w = std.Io.Writer.fixed(&buf);
    for (0..func_info.ret_size) |i| {
        try w.print("%{}={}, ", .{i, frame[i].val});
    }
    std.debug.print("frame: {s} info={}, ret_pc={*}, ret_fp={*}\n", .{
        w.buffered(),
        frame[func_info.ret_size].val,
        frame[func_info.ret_size+1].retPcPtr,
        frame[func_info.ret_size+2].retFramePtr,
    });
    w.end = 0;
    for (func_info.ret_size+4..frame.len) |i| {
        try w.print("%{}={}, ", .{i, frame[i].val});
    }
    std.debug.print("  locals: {s}\n", .{w.buffered()});
    return C.RetOk;
}

pub fn event(t: *cy.Thread) !C.Ret {
    const ret = t.ret(i64);
    ret.* = @intCast(cy.event_id);
    return C.RetOk;
}

pub fn init_choice(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *sema.ExprResult) !void {
    const choice_t = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    const tag = ctx.args[0].asInt();

    const case = choice_t.cast(.choice).getCaseByTag(@intCast(tag));

    const arg = ctx.args[1].asPtr(*cy.ast.Node);
    var payload = try c.semaExprCstr(arg, case.payload_t);
    payload = try sema.semaOwn(c, payload, arg);
    out.* = try sema.semaInitChoice(c, case.type, case.idx, case.val, payload, ctx.node);
}

pub fn choice_tag_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const arg = ctx.args[0].asPtr(*cy.heap.HeapObject);
    return cy.TypeValue.init(c.sema.i64_t, arg.object.getValuesPtr()[0]);
}

pub fn choice_tag(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *sema.ExprResult) !void {
    const rec = ctx.args[0].asPtr(*ir.Expr);
    const tag_t = ctx.func.sig.ret;
    const loc = try c.ir.newExpr(.field, tag_t, ctx.node, .{
        .idx = 0,
        .rec = rec,
    });
    out.* = sema.ExprResult.init2(loc);
}

pub fn is_result_error(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *sema.ExprResult) !void {
    const arg = ctx.args[0].asPtr(*cy.ast.Node);
    const res = try c.semaExpr(arg, .{});
    const loc = try c.ir.newExpr(.field, c.sema.i64_t, ctx.node, .{
        .idx = 0,
        .rec = res.ir,
    });
    const tag = sema.ExprResult.init(loc, c.sema.i64_t);
    out.* = try c.semaIsZero(tag, ctx.node);
}

pub fn is_none(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *sema.ExprResult) !void {
    const arg = ctx.args[0].asPtr(*cy.ast.Node);
    var res = try c.semaExpr(arg, .{});
    res = try sema.semaManage(c, res, ctx.node);
    out.* = try c.semaIsNone(res, ctx.node);
}

pub fn enum_int_values(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const enum_t = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    if (enum_t.kind() != .enum_t) {
        return c.reportError("Expected enum type.", ctx.node);
    }
    const cases = enum_t.cast(.enum_t).cases();
    const buffer_t = ctx.func.sig.ret;
    const res = try c.heap.new_buffer_undef(buffer_t, cases.len);
    for (cases, 0..) |case, i| {
        var val = cy.Value.initInt(@intCast(case.val));
        try res.asHeapObject().buffer.initElem(c.heap, c.sema.i64_t, i, @ptrCast(&val));
    }
    return cy.TypeValue.init(ctx.func.sig.ret, res);
}

pub fn enum_values(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const enum_t = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    if (enum_t.kind() != .enum_t) {
        return c.reportError("Expected enum type.", ctx.node);
    }
    const cases = enum_t.cast(.enum_t).cases();
    const buffer_t = ctx.func.sig.ret;
    const res = try c.heap.new_buffer_undef(buffer_t, cases.len);
    for (cases, 0..) |case, i| {
        var val = cy.Value.initInt(@intCast(case.val));
        try res.asHeapObject().buffer.initElem(c.heap, enum_t, i, @ptrCast(&val));
    }
    return cy.TypeValue.init(ctx.func.sig.ret, res);
}

pub fn enum_case(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const T = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    if (T.kind() != .enum_t) {
        return c.reportError("Expected enum type.", ctx.node);
    }
    const enum_val = ctx.args[0].asInt();

    const case = T.cast(.enum_t).getCaseByTag(@intCast(enum_val));
    const case_t = (try c.vm.findType("meta.EnumCase")).?;
    const case_v = try c.heap.newInstance(case_t, &.{
        field_init("name", try c.heap.newStr(case.head.name())),
    });
    return cy.TypeValue.init(case_t, case_v);
}

pub fn eval(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *sema.ExprResult) !void {
    _ = c;
    _ = ctx;
    _ = out;
    @panic("TODO");
}

pub fn error_(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const msg = ctx.args[0].asString();
    defer c.heap.release(ctx.args[0]);
    return c.reportError(msg, ctx.node);
}

pub fn log_(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const val_t = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    const val = ctx.args[0];
    defer c.heap.destructValue2(val_t, val);

    var w = std.Io.Writer.Allocating.init(c.alloc);
    defer w.deinit();
    _ = try c.heap.writeValue(&w.writer, val_t.id(), val, false);
    try w.writer.writeByte('\n');
    c.vm.log(w.written());
    return cy.TypeValue.init(c.sema.void_t, cy.Value.Void);
}

pub fn init_type_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const node = ctx.args[0].asPtr(*cy.ast.Node);
    const type_ = ctx.func.instance.?.params[0].asPtr(*cy.Type);

    if (node.type() != .init_lit) {
        return c.reportErrorFmt("Expected init literal, found `{}`", &.{v(node.type())}, ctx.node);
    }

    const init_n = node.cast(.init_lit);
    switch (type_.kind()) {
        .struct_t => {
            const struct_t = type_.cast(.struct_t);
            const expr = try cy.cte.evalInitStruct(c, struct_t, init_n);
            return cy.cte.exprToValue(c, expr, ctx.node);
        },
        else => {
            return error.TODO;
        }
    }
}

pub fn init_type(c: *cy.Chunk, ctx: *cy.CtFuncContext, out: *sema.ExprResult) !void {
    const node = ctx.args[0].asPtr(*cy.ast.Node);
    const type_ = ctx.func.instance.?.params[0].asPtr(*cy.Type);

    if (node.type() != .init_lit) {
        return c.reportErrorFmt("Expected init literal, found `{}`", &.{v(node.type())}, ctx.node);
    }

    const init_n = node.cast(.init_lit);
    switch (type_.kind()) {
        .struct_t => {
            const struct_t = type_.cast(.struct_t);

            if (init_n.array_like) {
                if (struct_t.tuple) {
                    out.* = try c.semaTupleInit(type_, struct_t.fields(), init_n);
                    return;
                } else {
                    return c.reportError("Expected record initializer.", @ptrCast(init_n));
                }
            }
            out.* = try c.semaInitStruct(struct_t, init_n);
            return;
        },
        .enum_t => {
            return c.reportErrorFmt("Only enum members can be used as initializers.", &.{}, @ptrCast(init_n));
        },
        // .hostobj => {
        //     if (type_sym.getMod().getSym("$initPairs") == null) {
        //         const type_name = try c.sema.allocTypeName(type_);
        //         defer c.alloc.free(type_name);
        //         return c.reportErrorFmt("Can not initialize the type `{}`. `$initPairs` is not defined for the type.", &.{v(type_name)}, node.left);
        //     }
        //     return semaInitPairs(c, @ptrCast(type_.sym()), node.lit);
        // },
        .vector => {
            out.* = try c.semaInitFixedArray(type_.cast(.vector), init_n);
            return;
        },
        // .pointer => {
        //     if (type_sym.getMod().getSym("$initPairs") == null) {
        //         const type_name = try c.sema.allocTypeName(type_);
        //         defer c.alloc.free(type_name);
        //         return c.reportErrorFmt("Can not initialize the type `{}`. `$initPairs` is not defined for the type.", &.{v(type_name)}, node.left);
        //     }
        //     return semaInitPairs(c, @ptrCast(type_.sym()), node.lit);
        // },
        else => {
            return error.TODO;
        }
    }
}

pub fn build_mode(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = ctx;
    const res_t = (try c.vm.findType("meta.BuildMode")).?;
    return cy.TypeValue.init(res_t, Value.initR64(0));
}

pub fn build_flag(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    defer c.heap.release(ctx.args[0]);
    const has_flag = c.compiler.build_flags.contains(ctx.args[0].asString());
    return cy.TypeValue.init(c.sema.bool_t, cy.Value.initBool(has_flag));
}

pub fn build_option(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    defer c.heap.release(ctx.args[0]);
    const option_t = (try c.vm.findType("?str")).?;
    const s = c.compiler.build_options.get(ctx.args[0].asString()) orelse {
        const res = try c.heap.newNone(option_t.cast(.option));
        return cy.TypeValue.init(option_t, res);
    };
    const str = try c.heap.newStr(s);
    const res = try c.heap.newSome(option_t.cast(.option), str);
    return cy.TypeValue.init(option_t, res);
}

pub fn cy_full_version(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = ctx;
    const str = try c.heap.newStr(build_config.full_version);
    return cy.TypeValue.init(c.sema.str_t, str);
}

pub fn load_(c: *cy.Chunk, cx: *cy.CtFuncContext) !cy.TypeValue {
    const path = cx.args[0].asString();
    defer c.heap.release(cx.args[0]);

    const root_path = std.fs.path.dirname(c.compiler.main_chunk.srcUri) orelse {
        return c.reportError("Cannot determine root path.", cx.node);
    };
    var dir = try std.fs.openDirAbsolute(root_path, .{});
    defer dir.close();

    const data = try dir.readFileAlloc(c.alloc, path, 1e10);
    defer c.alloc.free(data);

    const str = try c.heap.newStr(data);
    return cy.TypeValue.init(c.sema.str_t, str);
}

pub fn mod_uri(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = ctx;
    const str = try c.heap.newStr(c.srcUri);
    return cy.TypeValue.init(c.sema.str_t, str);
}

const StructConfig = extern struct {
    tuple: bool,
};

pub fn new_struct(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const fields_ptr = ctx.args[0].asPtr([*]sema_type.StructField);
    const num_fields: usize = @intCast(ctx.func.instance.?.params[0].asInt());
    const fields = fields_ptr[0..num_fields];
    defer c.heap.release(ctx.args[0]);

    const config = ctx.args[1].asPtr(*StructConfig);
    defer c.heap.release(ctx.args[1]);

    const new_t = try c.sema.createType(.struct_t, .{
        .cstruct = false,
        .opaque_t = false,
        .tuple = config.tuple,
    });
    _ = try c.createTypeSym(&c.sym.head, "Struct", new_t, ctx.node);
    try sema_type.reifyStructType(c, new_t.cast(.struct_t), fields, ctx.node);

    return cy.TypeValue.init(c.sema.type_t, cy.Value.initPtr(new_t));
}

pub fn PartialStructLayout_is_field_active(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const layout = ctx.args[0].asPtr(*sema.PartialStructLayout);
    const state_offset = ctx.args[1].asInt();
    const res = Value.initBool(layout.fields[@intCast(state_offset)]);
    return TypeValue.init(c.sema.bool_t, res);
}

pub fn PartialStructLayout_field_layout(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const layout = ctx.args[0].asPtr(*sema.PartialStructLayout);
    const state_offset: usize = @intCast(ctx.args[1].asInt());
    const state_len: usize = @intCast(ctx.args[2].asInt());
    const option_t = try sema.getOptionType(c, c.sema.getType(bt.PartialStructLayout));
    const actives = layout.fields[state_offset..state_offset + state_len];
    if (std.mem.indexOfScalar(bool, actives, false) == null) {
        // No inactives.
        const res = try c.heap.newNone(option_t.cast(.option));
        return TypeValue.init(option_t, res);
    }

    const field_layout = try sema.ensure_partial_struct_layout(c, actives);
    const res = try c.heap.newSome(option_t.cast(.option), Value.initPtr(field_layout));
    return TypeValue.init(option_t, res);
}

pub fn is_inline_eval(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = ctx;
    const rctx = sema.getResolveContext(c);
    const res = cy.Value.initBool(rctx.is_inline_eval);
    return cy.TypeValue.init(c.sema.bool_t, res);
}

pub fn is_vm_target(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = ctx;
    const res = cy.Value.initBool(c.compiler.config.backend == C.BackendVM);
    return cy.TypeValue.init(c.sema.bool_t, res);
}

pub const SystemKind = enum {
    linux,
    macos,
    windows,
};

pub fn system(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = c;
    const kind = std.enums.nameCast(SystemKind, builtin.os.tag);
    return cy.TypeValue.init(ctx.func.sig.ret, cy.Value.initInt(@intFromEnum(kind)));
}

pub fn cpu(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = ctx;
    const str = try c.heap.newStr(@tagName(builtin.cpu.arch));
    return cy.TypeValue.init(c.sema.str_t, str);
}

pub fn endian_(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = c;
    if (builtin.cpu.arch.endian() == .little) {
        return cy.TypeValue.init(ctx.func.sig.ret, cy.Value.initInt(0));
    } else {
        return cy.TypeValue.init(ctx.func.sig.ret, cy.Value.initInt(1));
    }
}

fn reachable(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    return cy.TypeValue.init(ctx.func.sig.ret, cy.Value.initBool(c.block().endReachable));
}

fn has_decl(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const parent_n = ctx.args[0].asPtr(*ast.Node);
    const name = ctx.args[1].asString();
    defer c.heap.release(ctx.args[1]);
    const parent = try cy.cte.evalSym(c, parent_n);
    const res = try c.getResolvedSym(parent, name, ctx.node);
    return cy.TypeValue.init(ctx.func.sig.ret, cy.Value.initBool(res != null));
}

pub fn type_id_(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = c;
    const type_ = ctx.args[0].asPtr(*cy.Type);
    const val = cy.Value.initInt(@intCast(type_.id()));
    return cy.TypeValue.init(ctx.func.sig.ret, val);
}

pub fn type_managed(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = c;
    const type_ = ctx.args[0].asPtr(*cy.Type);
    const managed = type_.isManaged();
    return cy.TypeValue.init(ctx.func.sig.ret, cy.Value.initBool(managed));
}

pub fn type_fn_ret(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const type_ = ctx.args[0].asPtr(*cy.Type);
    if (type_.kind() == .func_ptr) {
        const ret = type_.cast(.func_ptr).sig.ret;
        return cy.TypeValue.init(ctx.func.sig.ret, Value.initPtr(ret));
    } else if (type_.kind() == .func_sym) {
        const ret = type_.cast(.func_sym).sig.ret;
        return cy.TypeValue.init(ctx.func.sig.ret, Value.initPtr(ret));
    } else {
        return c.reportError("Expected funcptr type.", ctx.node);
    }
}

pub fn type_struct_state_len(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const type_ = ctx.args[0].asPtr(*cy.Type);
    if (type_.kind() != .struct_t) {
        return c.reportError("Expected struct type.", ctx.node);
    }
    const state_len = type_.cast(.struct_t).field_state_len;
    return cy.TypeValue.init(ctx.func.sig.ret, Value.initGenericInt(@intCast(state_len)));
}

pub fn type_name_rt(t: *cy.Thread) !C.Ret {
    if (!cy.Trace) {
        return t.ret_panic("Requires `TRACE` mode.");
    }

    const ret = t.ret(cy.heap.Str);
    const id: cy.TypeId = @intCast(t.param(i64));

    t.c.vm.c.rw_lock.read_lock();
    defer t.c.vm.c.rw_lock.read_unlock();

    const name_ptr = t.c.vm.c.types_ptr[id].name_ptr;
    const name_len = t.c.vm.c.types_ptr[id].name_len;
    ret.* = try t.heap.init_str(name_ptr[0..name_len]);
    return C.RetOk;
}

pub fn type_name(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const type_ = ctx.args[0].asPtr(*cy.Type);
    const name = try c.sema.allocTypeName(type_);
    defer c.alloc.free(name);
    const val = try c.heap.newStr(name);
    return cy.TypeValue.init(ctx.func.sig.ret, val);
}

pub fn type_size(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    _ = c;
    const type_ = ctx.args[0].asPtr(*cy.Type);
    const val = cy.Value.initGenericInt(@intCast(type_.size()));
    return cy.TypeValue.init(ctx.func.sig.ret, val);
}

pub fn type_of(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const expr = ctx.args[0].asPtr(*cy.ast.Node);
    const res = try c.semaExpr(expr, .{});
    const val = cy.Value.initPtr(res.type);
    return cy.TypeValue.init(ctx.func.sig.ret, val);
}

pub fn type_init(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const name = ctx.args[0].asString();
    defer c.heap.release(ctx.args[0]);
    const res = try sema.lookupStaticIdent(c, name, ctx.node) orelse {
        return c.reportErrorFmt("Could not find the symbol `{}`.", &.{v(name)}, ctx.node);
    };
    switch (res) {
        .capture,
        .local => {
            @panic("Unexpected.");
        },
        .static => |sym| {
            // if (sym.type == .const_) {
            //     const const_ = sym.cast(.const_);
            //     try sema.resolveConst(c, const_);
            //     const dupe = try c.heap.copyValue2(const_.type, const_.value);
            //     return Expr.initValue2(const_.type, dupe);
            // }
            if (sym.type == .type) {
                return cy.TypeValue.init(c.sema.type_t, cy.Value.initPtr(sym.cast(.type).type));
            } else {
                return c.reportErrorFmt("Expected type, found `{}`.", &.{v(sym.type)}, ctx.node);
            }
        },
        .ct_var => |ct_var| {
            _ = ct_var;
            return error.TODO;
            // return Expr.initCtVar(ct_var);
        },
        .ct_value => |ct_value| {
            _ = ct_value;
            return error.TODO;
            // return Expr.initValue(ct_value);
        },
    }
}

const field_init = cy.heap.field_init;

fn type_info2(c: *cy.Chunk, type_: *cy.Type) !cy.Value {
    const info_t = (try c.vm.findType("meta.TypeInfo")).?;
    switch (type_.id()) {
        bt.Never => {
            return c.heap.newChoice(info_t, "never", cy.Value.Void);
        },
        bt.Void => {
            return c.heap.newChoice(info_t, "void", cy.Value.Void);
        },
        bt.Type => {
            return c.heap.newChoice(info_t, "type", cy.Value.Void);
        },
        bt.Error => {
            return c.heap.newChoice(info_t, "error", cy.Value.Void);
        },
        else => {
            switch (type_.kind()) {
                .float => {
                    const bits = type_.cast(.float).bits;
                    const float_info_t = (try c.vm.findType("meta.FloatInfo")).?;
                    const float_info = try c.heap.newInstance(float_info_t, &.{
                        field_init("bits", cy.Value.initInt(bits)),
                    });
                    return c.heap.newChoice(info_t, "float", float_info);
                },
                .ref_trait => {
                    const case_t = (try c.vm.findType("meta.RefTraitInfo")).?;
                    const trait_ref = type_.cast(.ref_trait);
                    const child_t = cy.Value.initPtr(trait_ref.generic);
                    const case = try c.heap.newInstance(case_t, &.{
                        field_init("child", child_t),
                    });
                    return c.heap.newChoice(info_t, "ref_trait", case);
                },
                .borrow_trait => {
                    const case_t = (try c.vm.findType("meta.BorrowTraitInfo")).?;
                    const borrow_trait = type_.cast(.borrow_trait);
                    const child_t = cy.Value.initPtr(borrow_trait.generic);
                    const case = try c.heap.newInstance(case_t, &.{
                        field_init("child", child_t),
                    });
                    return c.heap.newChoice(info_t, "borrow_trait", case);
                },
                .borrow => {
                    const borrow_info_t = (try c.vm.findType("meta.BorrowInfo")).?;
                    const borrow = type_.cast(.borrow);
                    const child_t = cy.Value.initPtr(borrow.child_t);
                    const borrow_info = try c.heap.newInstance(borrow_info_t, &.{
                        field_init("child", child_t),
                    });
                    return c.heap.newChoice(info_t, "borrow", borrow_info);
                },
                .pointer => {
                    const ptr_info_t = (try c.vm.findType("meta.PointerInfo")).?;
                    const pointer = type_.cast(.pointer);
                    const child_t = cy.Value.initPtr(pointer.child_t);
                    const ptr_info = try c.heap.newInstance(ptr_info_t, &.{
                        field_init("child", child_t),
                    });
                    if (pointer.ref) {
                        return c.heap.newChoice(info_t, "ref", ptr_info);
                    } else {
                        return c.heap.newChoice(info_t, "ptr", ptr_info);
                    }
                },
                .raw => {
                    const raw_t = type_.cast(.raw);
                    const raw_info_t = (try c.vm.findType("meta.RawInfo")).?;
                    const raw_info = try c.heap.newInstance(raw_info_t, &.{
                        field_init("bits", cy.Value.initInt(raw_t.bits)),
                    });
                    return c.heap.newChoice(info_t, "raw", raw_info);
                },
                .int => {
                    const int_t = type_.cast(.int);
                    const int_info_t = (try c.vm.findType("meta.IntInfo")).?;
                    const int_info = try c.heap.newInstance(int_info_t, &.{
                        field_init("bits", cy.Value.initInt(int_t.bits)),
                    });
                    return c.heap.newChoice(info_t, "int", int_info);
                },
                .bool => {
                    return c.heap.newChoice(info_t, "bool", cy.Value.Void);
                },
                .generic_trait => {
                    const name = type_.name();
                    const trait_info_t = (try c.vm.findType("meta.GenTraitInfo")).?;
                    const trait_info = try c.heap.newInstance(trait_info_t, &.{
                        field_init("name", try c.heap.newStr(name)),
                    });
                    return c.heap.newChoice(info_t, "gen_trait", trait_info);
                },
                .partial_vector => {
                    const vector_t = type_.cast(.partial_vector);
                    const vector_info_t = (try c.vm.findType("meta.VectorInfo")).?;
                    const vector_info = try c.heap.newInstance(vector_info_t, &.{
                        field_init("len", cy.Value.initInt(@intCast(vector_t.n))),
                        field_init("elem", cy.Value.initPtr(vector_t.elem_t)),
                    });
                    return c.heap.newChoice(info_t, "partial_vector", vector_info);
                },
                .vector => {
                    const vector_t = type_.cast(.vector);
                    const vector_info_t = (try c.vm.findType("meta.VectorInfo")).?;
                    const vector_info = try c.heap.newInstance(vector_info_t, &.{
                        field_init("len", cy.Value.initInt(@intCast(vector_t.n))),
                        field_init("elem", cy.Value.initPtr(vector_t.elem_t)),
                    });
                    return c.heap.newChoice(info_t, "vector", vector_info);
                },
                .option => {
                    const opt_info_t = (try c.vm.findType("meta.OptionInfo")).?;
                    const elem_t = cy.Value.initPtr(type_.sym().instance.?.params[0].asPtr(*cy.Type));
                    const opt_info = try c.heap.newInstance(opt_info_t, &.{
                        field_init("child", @bitCast(elem_t)),
                    });
                    return c.heap.newChoice(info_t, "option", opt_info);
                },
                .result => {
                    const res_info_t = (try c.vm.findType("meta.ResultInfo")).?;
                    const elem_t = cy.Value.initPtr(type_.sym().instance.?.params[0].asPtr(*cy.Type));
                    const opt_info = try c.heap.newInstance(res_info_t, &.{
                        field_init("child", @bitCast(elem_t)),
                    });
                    return c.heap.newChoice(info_t, "result", opt_info);
                },
                .choice => {
                    const choice_t = type_.cast(.choice);
                    const choice_info_t = (try c.vm.findType("meta.ChoiceInfo")).?;
                    const choice_case_t = (try c.vm.findType("meta.ChoiceCase")).?;
                    const buffer_t = (try c.vm.findType("Buffer[meta.ChoiceCase]")).?;

                    const cases = choice_t.cases();
                    const buffer = try c.heap.new_buffer_undef(buffer_t, cases.len);
                    for (cases, 0..) |case, i| {
                        const case_v = try c.heap.newInstance(choice_case_t, &.{
                            field_init("name", try c.heap.newStr(case.name())),
                            field_init("type", cy.Value.initPtr(case.payload_t)),
                        });
                        defer c.heap.release(case_v);
                        try buffer.asHeapObject().buffer.initElem(c.heap, choice_case_t, i, case_v.asHeapObject().object.getBytePtr());
                    }
                    const name = type_.name();

                    const str_t = (try c.vm.findType("?str")).?.cast(.option);
                    const namev = try c.heap.newSome(str_t, try c.heap.newStr(name));
                    const choice_info = try c.heap.newInstance(choice_info_t, &.{
                        field_init("name", namev),
                        field_init("cases", buffer),
                    });
                    return c.heap.newChoice(info_t, "choice", choice_info);
                },
                .enum_t => {
                    const enum_t = type_.cast(.enum_t);
                    const enum_info_t = (try c.vm.findType("meta.EnumInfo")).?;
                    const enum_case_t = (try c.vm.findType("meta.EnumCase")).?;
                    const buffer_t = (try c.vm.findType("Buffer[meta.EnumCase]")).?;

                    const cases = enum_t.cases();
                    const buffer = try c.heap.new_buffer_undef(buffer_t, cases.len);
                    for (cases, 0..) |case, i| {
                        const case_v = try c.heap.newInstance(enum_case_t, &.{
                            field_init("name", try c.heap.newStr(case.head.name())),
                        });
                        defer c.heap.release(case_v);
                        try buffer.asHeapObject().buffer.initElem(c.heap, enum_case_t, i, case_v.asHeapObject().object.getBytePtr());
                    }
                    const name = type_.name();
                    const str_t = (try c.vm.findType("?str")).?.cast(.option);
                    const namev = try c.heap.newSome(str_t, try c.heap.newStr(name));
                    const enum_info = try c.heap.newInstance(enum_info_t, &.{
                        field_init("name", namev),
                        field_init("cases", buffer),
                    });
                    return c.heap.newChoice(info_t, "enum", enum_info);
                },
                .c_union => {
                    const c_union = type_.cast(.c_union);
                    const case_t = (try c.vm.findType("meta.CUnionCase")).?;
                    const buffer_t = (try c.vm.findType("Buffer[meta.CUnionCase]")).?;
                    const cases = c_union.cases();
                    const buffer = try c.heap.new_buffer_undef(buffer_t, cases.len);
                    for (cases, 0..) |case, i| {
                        const case_v = try c.heap.newInstance(case_t, &.{
                            field_init("name", try c.heap.newStr(case.name())),
                            field_init("type", cy.Value.initPtr(case.payload_t)),
                        });
                        defer c.heap.release(case_v);
                        try buffer.asHeapObject().buffer.initElem(c.heap, case_t, i, case_v.asHeapObject().object.getBytePtr());
                    }
                    const name = type_.name();

                    const str_t = (try c.vm.findType("?str")).?.cast(.option);
                    const namev = try c.heap.newSome(str_t, try c.heap.newStr(name));
                    const cunion_info_t = (try c.vm.findType("meta.CUnionInfo")).?;
                    const info = try c.heap.newInstance(cunion_info_t, &.{
                        field_init("name", namev),
                        field_init("cases", buffer),
                    });
                    return c.heap.newChoice(info_t, "cunion", info);
                },
                .struct_t => {
                    const struct_t = type_.cast(.struct_t);
                    const field_t = (try c.vm.findType("meta.StructField")).?;
                    const buffer_t = (try c.vm.findType("Buffer[meta.StructField]")).?;
                    const fields = struct_t.fields();
                    const buffer = try c.heap.new_buffer_undef(buffer_t, fields.len);
                    for (fields, 0..) |field, i| {
                        const f = try c.heap.newInstance(field_t, &.{
                            field_init("name", try c.heap.newStr(field.sym.head.name())),
                            field_init("type", cy.Value.initPtr(field.type)),
                            field_init("offset", cy.Value.initInt(@intCast(field.offset))),
                            field_init("state_offset", cy.Value.initInt(@intCast(field.state_offset))),
                        });
                        defer c.heap.release(f);
                        try buffer.asHeapObject().buffer.initElem(c.heap, field_t, i, f.asHeapObject().object.getBytePtr());
                    }
                    const name = type_.name();
                    const str_t = (try c.vm.findType("?str")).?.cast(.option);
                    const namev = try c.heap.newSome(str_t, try c.heap.newStr(name));
                    if (struct_t.cstruct) {
                        const struct_info_t = (try c.vm.findType("meta.CStructInfo")).?;
                        const struct_info = try c.heap.newInstance(struct_info_t, &.{
                            field_init("name", @bitCast(namev)),
                            field_init("fields", buffer),
                        });
                        return c.heap.newChoice(info_t, "cstruct", struct_info);
                    } else {
                        const struct_info_t = (try c.vm.findType("meta.StructInfo")).?;
                        const struct_info = try c.heap.newInstance(struct_info_t, &.{
                            field_init("name", @bitCast(namev)),
                            field_init("fields", buffer),
                        });
                        return c.heap.newChoice(info_t, "struct", struct_info);
                    }
                },
                .func_ptr => {
                    const func_info = try newFuncInfo(c, 0, type_.cast(.func_ptr).sig);
                    return c.heap.newChoice(info_t, "func", func_info);
                },
                .func => {
                    const func_info = try newFuncInfo(c, 1, type_.cast(.func).sig);
                    return c.heap.newChoice(info_t, "func", func_info);
                },
                .func_sym => {
                    const func_info = try newFuncInfo(c, 2, type_.cast(.func_sym).sig);
                    return c.heap.newChoice(info_t, "func", func_info);
                },
                else => {
                    std.debug.panic("Unsupported: {}", .{type_.kind()});
                }
            }
        },
    }
}

pub fn type_field(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const type_ = ctx.args[0].asPtr(*cy.Type);
    const name = ctx.args[1].asString();
    defer c.heap.release(ctx.args[1]);

    try sema_type.ensure_resolved_type(c, type_, ctx.node);

    if (type_.kind() != .struct_t) {
        return c.reportError("Expected `struct` type.", ctx.node);
    }
    const sym = type_.sym().getMod().getSym(name) orelse {
        return c.reportErrorFmt("No such field `{}`.", &.{v(name)}, ctx.node);
    };
    if (sym.type != .field) {
        return c.reportError("Expected field.", ctx.node);
    }
    const field = sym.cast(.field);
    const field_t = (try c.findResolvedType("meta.StructField", ctx.node)).?;
    const struct_field = type_.cast(.struct_t).fields()[field.idx];
    const val = try c.heap.newInstance(field_t, &.{
        field_init("name", try c.heap.newStr(name)),
        field_init("type", cy.Value.initPtr(struct_field.type)),
        field_init("offset", cy.Value.initInt(@intCast(struct_field.offset))),
        field_init("state_offset", cy.Value.initInt(@intCast(struct_field.state_offset))),
    });
    return cy.TypeValue.init(field_t, val);
}

pub fn type_info(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const type_ = ctx.args[0].asPtr(*cy.Type);
    const val = try type_info2(c, type_);
    return cy.TypeValue.init(ctx.func.sig.ret, val);
}

fn newFuncInfo(c: *cy.Chunk, kind: u8, sig: *cy.FuncSig) !cy.Value {
    const func_info_t = (try c.vm.findType("meta.FuncInfo")).?;
    const param_t = (try c.vm.findType("meta.FuncParam")).?;
    const buffer_t = (try c.vm.findType("Buffer[meta.FuncParam]")).?;
    const params = sig.params();

    const buffer = try c.heap.new_buffer_undef(buffer_t, params.len);
    for (params, 0..) |param, i| {
        const p = try c.heap.newInstance(param_t, &.{
            field_init("type", cy.Value.initPtr(param.get_type())),
        });
        defer c.heap.release(p);
        try buffer.asHeapObject().buffer.initElem(c.heap, param_t, i, p.asHeapObject().object.getBytePtr());
    }

    return c.heap.newInstance(func_info_t, &.{
        field_init("kind", cy.Value.initInt(kind)),
        field_init("ret", cy.Value.initPtr(sig.ret)),
        field_init("params", buffer),
    });
}

fn type_implements(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const type_ = ctx.args[0].asPtr(*cy.Type);
    const trait_t = ctx.args[1].asPtr(*cy.Type);
    if (trait_t.kind() != .generic_trait) {
        return c.reportError("Expected trait type.", ctx.node);
    }
    const implements = try sema_type.implements(c, type_, trait_t.cast(.generic_trait), ctx.node);
    return cy.TypeValue.init(c.sema.bool_t, cy.Value.initBool(implements));
}

fn type_is_const(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const type_ = ctx.args[0].asPtr(*cy.Type);
    try sema_type.ensure_resolved_type(c, type_, ctx.node);
    return cy.TypeValue.init(c.sema.bool_t, cy.Value.initBool(type_.isConstEligible()));
}

fn type_is_copyable(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const type_ = ctx.args[0].asPtr(*cy.Type);
    try sema_type.ensure_resolved_type(c, type_, ctx.node);
    return cy.TypeValue.init(c.sema.bool_t, cy.Value.initBool(type_.is_copyable()));
}

fn type_is_instance_of(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const type_ = ctx.args[0].asPtr(*cy.Type);
    const template_n = ctx.args[1].asPtr(*ast.Node);
    const sym = try cy.cte.evalSym(c, template_n);
    if (sym.type != .template) {
        return c.reportError("Expected template symbol.", template_n);
    }
    const template = sym.cast(.template);
    const is_instance = type_.isInstanceOf(template);
    return cy.TypeValue.init(c.sema.bool_t, cy.Value.initBool(is_instance));
}

fn trace_retains(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(i64);

    if (!cy.Trace) return t.ret_panic("Requires TRACE.");

    ret.* = t.heap.c.numRetains;
    return C.RetOk;
}

fn trace_releases(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(i64);

    if (!cy.Trace) return t.ret_panic("Requires TRACE.");

    ret.* = t.heap.c.numReleases;
    return C.RetOk;
}