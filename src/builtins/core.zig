const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const fatal = cy.fatal;
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const Value = cy.Value;
const TypeValue = cy.TypeValue;
const bindings = @import("bindings.zig");
const cy_mod = @import("cy.zig");
const Symbol = bindings.Symbol;
const fmt = @import("../fmt.zig");
const v = fmt.v;
const ast = cy.ast;
const ir = cy.ir;
const cte = cy.cte;
const bt = cy.types.BuiltinTypes;
const sema = cy.sema;
const sema_type = cy.sema_type;
const vmc = cy.vmc;
const string = @import("string.zig");

const logger = cy.log.scoped(.core);

const Src = @embedFile("core.cy");

pub const BuiltinsData = struct {
    OptionString: *cy.types.Option,
    PtrVoid: *cy.Type,
    PtrSpanByte: *cy.Type,
};

const types = [_]struct{[]const u8, C.BindType}{
    .{"void",           C.TYPE_CREATE(createVoidType)},
    .{"never",          C.TYPE_CREATE(createNeverType)},
    .{"bool",           C.TYPE_CREATE(createBoolType)},
    .{"symbol",         C.TYPE_CREATE(createSymbolType)},
    .{"error",          C.TYPE_CREATE(createErrorType)},
    .{"EvalBuffer",     C.TYPE_CREATE(create_eval_buffer_type)},
    .{"EvalInt",        C.TYPE_CREATE(create_eval_int_type)},
    .{"EvalStr",        C.TYPE_CREATE(create_eval_str_type)},
    .{"Raw",            C.TYPE_CREATE(createRawType)},
    .{"Int",            C.TYPE_CREATE(createIntType)},
    .{"Float",          C.TYPE_CREATE(createFloatType)},
    // .{"placeholder1",   CS.TYPE_RESERVE_HOBJ(bt.Placeholder1, &vm.sema.placeholder1_t)},
    .{"Object",         C.TYPE_RESERVE_DECL(bt.Object)},
    .{"Table",          C.TYPE_RESERVE_DECL(bt.Table)},
    // .{"Table",          CS.TYPE_RESERVE_DECL(bt.Table, &vm.sema.table_t)},
    .{"StrBuffer",      C.TYPE_RESERVE_DECL(bt.StrBuffer)},
    .{"Str",            C.TYPE_RESERVE_DECL(bt.MutStr)},
    .{"str",            C.TYPE_RESERVE_DECL(bt.Str)},
    .{"Range",          C.TYPE_RESERVE_DECL(bt.Range)},
    .{"TccState",       C.TYPE_RESERVE_DECL(bt.TccState)},
    .{"NoCopy",         C.TYPE_RESERVE_DECL(bt.NoCopy)},
    .{"PartialVector",  C.TYPE_CREATE(createPartialVectorType)},
    .{"Vector",         C.TYPE_CREATE(createVectorType)},
    .{"GenericVector",  C.TYPE_CREATE(createGenericVectorType)},
    .{"Ptr",            C.TYPE_CREATE(createPtrType)},
    .{"Ref",            C.TYPE_CREATE(createRefType)},
    .{"Borrow",         C.TYPE_CREATE(createBorrowType)},
    .{"ExBorrow",       C.TYPE_CREATE(createExBorrowType)},
    .{"Dyn",            C.TYPE_CREATE(create_dyn_type)},
    .{"Thread",         C.TYPE_CREATE(create_thread_type)},
    .{"FuncSig",        C.TYPE_CREATE(createFuncSigType)},
    .{"funcptr_t",      C.TYPE_CREATE(createFuncPtrType)},
    .{"Func",           C.TYPE_CREATE(createFuncType)},
    .{"OpaqueFunc",     C.TYPE_CREATE(createOpaqueFuncType)},
    .{"funcsym_t",      C.TYPE_CREATE(createFuncSymType)},
    .{"Option",         C.TYPE_CREATE(createOptionType)},
    .{"Result",         C.TYPE_CREATE(createResultType)},
    .{"Any",            C.TYPE_CREATE(create_any_type)},
    .{"dependent",      C.TYPE_CREATE(createDependentType)},
    .{"Infer",          C.TYPE_CREATE(createInferType)},
};

const funcs = [_]struct{[]const u8, C.BindFunc}{
    // Utils.
    .{"alloc_",         zErrFunc(alloc)},
    .{"eprints_",       cFunc(eprints)},
    .{"free",           zErrFunc(free)},
    .{"panic",          zErrFunc(panic)},
    .{"abort",          zErrFunc(abort)},
    .{"prints_",        cFunc(prints)}, 
    .{"log_",           cFunc(log)},
    .{"memory",         zErrFunc(memory)},

    .{"@bitTrunc",      zErrBuiltinFunc(bitTrunc, bitTrunc_eval)},
    .{"@sext",          zErrBuiltinFunc(sext, null)},
    .{"@zext",          zErrBuiltinFunc(zext, null)},
    .{"@floatToInt",    zErrBuiltinFunc(floatToInt, null)},
    .{"@intToFloat",    zErrBuiltinFunc(intToFloat, null)},
    .{"@consume",       zErrBuiltinFunc(consume, null)},
    .{"@cmp",           zErrBuiltinFunc(cmp, null)},
    .{"@copyStruct",    zErrBuiltinFunc(copyStruct, null)},
    .{"@freeObject",    cFunc(freeObject)},
    .{"@getDeinitObject", zErrFunc(getDeinitObject)},
    .{"@getTypeDtor",   zErrFunc(getTypeDtor)},
    .{"@new_object_undef", zErrFunc(new_object_undef)},
    .{"@new_object_undef2", zErrFunc(new_object_undef2)},
    .{"@releaseOnly",   zErrFunc(releaseOnly)},
    .{"@retain",        zErrFunc(retain)},
    .{"@refCount",      zErrFunc(refCount)},
    .{"@isUniqueRef",   zErrFunc(isUniqueRef)},
    .{"@unsafeCast",    zErrBuiltinFunc(unsafeCast, null)},
    .{"@scope_ptr_to_borrow", zErrBuiltinFunc(scope_ptr_to_borrow, scope_ptr_to_borrow_eval)},
    .{"@bitCast",       zErrBuiltinFunc(bitCast, bitCast_eval)},
    .{"@nop",           zErrConstEvalFunc(nop)},
    .{"@breakpoint",    zErrBuiltinFunc(breakpoint, null)},
    .{"@memcpy",        zErrFunc(memcpy)},
    .{"@memset",        zErrFunc(memset)},
    .{"@memmove",       zErrFunc(memmove)},
    .{"@notifyFutureComplete", zErrFunc(notifyFutureComplete)},
    .{"@await_",        zErrBuiltinFunc(await_, null)},
    .{"@trackMainLocal", zErrFunc(trackMainLocal)},
    .{"@compute_int_max", zErrConstEvalFunc(compute_int_max)},
    .{"@compute_int_min", zErrConstEvalFunc(compute_int_min)},
    .{"@ptr_init",      zErrBuiltinFunc(ptr_init, null)},
    .{"@ref_addr",      zErrFunc(ref_addr)},
    .{"@new_thread",    zErrFunc(new_thread)},
    .{"@start_thread",  zErrFunc(start_thread)},
    .{"@new_shared_state", zErrFunc(new_shared_state)},
    .{"@call",          zErrBuiltinFunc(call, null)},

    .{"EvalBuffer[].@index_addr", zErrConstEvalFunc(EvalBuffer_index_addr)},
    .{"EvalBuffer[].len", zErrConstEvalFunc(EvalBuffer_len)},

    .{"EvalStr.@init", zErrConstEvalFunc(EvalStr_init)},
    .{"EvalStr.+", zErrConstEvalFunc(EvalStr_concat)},
    .{"EvalStr.len", zErrConstEvalFunc(EvalStr_len)},
    .{"EvalStr.@index", zErrConstEvalFunc(EvalStr_index)},

    // symbol
    .{"symbol.name",    zErrFunc(symbol_name)},

    // error

    // Raw
    .{"raw_fmt",      zErrFunc(raw_fmt)},

    // Int
    .{"Int[].@init",  zDeclFunc(Int_init)},
    .{"@asr",    zErrBuiltinFunc(bindings.asr, null)},

    .{"int_fmt",      zErrFunc2(int_fmt, int_fmt_eval)},

    // .{"int.fmt2",       zErrFunc(int_fmt2)},

    .{"@fabs",  zErrBuiltinFunc(bindings.fabs, null)},
    .{"atof",         zErrFunc(atof)},
    .{"atof32",       zErrFunc(atof32)},
    .{"float_pow",    cFunc(bindings.float_pow)},
    .{"float_pow32",  cFunc(bindings.float_pow32)},
    .{"f32_to_f64",   cFunc(f32_to_f64)},
    .{"f64_to_f32",   cFunc(f64_to_f32)},
    .{"f64_fmt",      zErrFunc(f64_fmt)},
    .{"f32_fmt",      zErrFunc(f32_fmt)},

    // Table

    // str
    .{"str.initRune",    zErrFunc(str_initRune)},
    .{"str.initPtrSpan", zErrFunc(string.initPtrSpan)},
    .{"str.+",           zErrFunc(string.concat)},
    .{"str.concat",      zErrFunc(string.concat)},
    .{"str.count",       cFunc(string.count)},
    // func("str.decode",       String_decode),
    // func("str.decode2",      String_decode2),
    .{"str.ends_with",   cFunc(string.ends_with)},
    .{"str.index",       zErrFunc(string.index)},
    .{"str.index_byte",  zErrFunc(str_index_byte)},
    .{"str.index_rune",  zErrFunc(string.index_rune)},
    .{"str.index_newline", zErrFunc(string.index_newline)},
    .{"str.index_any_byte", zErrFunc(str_index_any_byte)},
    .{"str.index_any_rune", zErrFunc(string.index_any_rune)},
    .{"str.fmt_bytes",    zErrFunc(str_fmt_bytes)},
    .{"str.rune_at",     zErrFunc(string.rune_at)},
    .{"str.@index",      zErrFunc(str_byteAt)},
    .{"str.insert",      zErrFunc(string.insertFn)},
    .{"str.insert_byte", zErrFunc(string.insertByte)},
    .{"str.isAscii",     cFunc(string.isAscii)},
    .{"str.len",         cFunc(string.lenFn)},
    .{"str.less",        cFunc(string.less)},
    .{"str.lower",       zErrFunc(string.lower)},
    .{"str.replace",     zErrFunc(string.stringReplace)},
    .{"str.repeat",      zErrFunc(string.repeat)},
    .{"str.seek_pos",    zErrFunc(string.seek_pos)},
    .{"str.runeStrAt",   zErrFunc(string.runeStrAt)},
    .{"str.@slice",      cFunc(string.sliceFn)},
    .{"str.@slice2",     cFunc(string.slice2)},
    .{"str.starts_with", cFunc(string.starts_with)},
    .{"str.upper",       zErrFunc(string.upper)},

    .{"Object.type",     zErrFunc(Object_type)},

    // Vector
    // func("Vector.+",      arrayConcat),
    // func("Vector.concat", arrayConcat),
    // func("Vector.insert", zErrFunc(arrayInsert)),
    // func("Vector.repeat", zErrFunc(arrayRepeat)),
    .{"Vector[].ct_repeat", zErrConstEvalFunc(Vector_ct_repeat)},

    .{"@result_unwrap",      zErrBuiltinFunc(result_unwrap, null)},
    .{"@result_unwrap_error", zErrBuiltinFunc(result_unwrap_error, null)},

    // Ptr
    .{"Ptr[].+",          zDeclFunc(Ptr_add_eval)},

    .{"Func.@size",       cFunc(Func_size)},
    .{"OpaqueFunc.@size", cFunc(Func_size)},

    // FuncSig
    .{"FuncSig.num_params", zErrConstEvalFunc(FuncSig_num_params)},
    .{"FuncSig.param_at", zErrConstEvalFunc(FuncSig_param_at)},
    .{"FuncSig.ret",      zErrConstEvalFunc(FuncSig_ret)},

    // Generator
    .{"Generator[].deinit", zErrFunc(Generator_deinit)},
    .{"Generator[].@size",  cFunc(Generator_size)},
    .{"Generator[].status", cFunc(Generator_status)},
    .{"@gen_next",   zErrBuiltinFunc(gen_next, null)},
    .{"@gen_end",    zErrBuiltinFunc(gen_end, null)},
};

comptime {
    @export(&bind, .{ .name = "cl_mod_bind_core", .linkage = .strong });
}

pub fn bind(_: *C.VM, mod: *C.Sym) callconv(.c) C.Bytes {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }

    for (types) |e| {
        C.mod_add_type(mod, e.@"0", e.@"1");
    }

    C.mod_on_load(mod, onLoad);

    return C.to_bytes(Src);
}

fn onLoad(vm: ?*C.VM, mod: ?*C.Sym) callconv(.c) void {
    onLoadZ(@ptrCast(@alignCast(vm)), @ptrCast(@alignCast(mod))) catch @panic("error");
}

fn onLoadZ(vm: *cy.VM, mod: *cy.Sym) !void {
    logger.tracev("builtins: on load", .{});
    const chunk_sym = mod.cast(.chunk);
    const b = bindings.ModuleBuilder.init(vm.compiler, @ptrCast(chunk_sym));
    _ = b;

    const data = vm.getData(*BuiltinsData, "builtins");

    const option_tmpl = chunk_sym.getMod().getSym("Option").?.cast(.template);

    const assert = std.debug.assert;
    _ = assert;

    const string_t = cy.Value.initType(vm.sema.str_t);
    data.OptionString = (try vm.expandTypeTemplate(option_tmpl, &.{vm.sema.type_t}, &.{string_t})).cast(.option);

    const ptr_tmpl = chunk_sym.getMod().getSym("Ptr").?.cast(.template);

    const void_t = cy.Value.initType(vm.sema.void_t);
    data.PtrVoid = (try vm.expandTypeTemplate(ptr_tmpl, &.{vm.sema.type_t}, &.{void_t}));

    // Verify all core types have been initialized.
    if (cy.Trace) {
        if (vm.sema.types.items[0].kind() != .null) {
            cy.panicFmt("Expected null type.", .{});
        }
        for (1..cy.types.BuiltinEnd) |i| {
            const type_e = vm.sema.types.items[i];
            if (type_e.kind() == .null) {
                cy.panicFmt("Type {} is uninited.", .{i});
            }
        }
    }
}

pub fn zErrConstEvalFunc(comptime f: fn(*cy.Chunk, *cy.ConstEvalContext) anyerror!TypeValue) C.BindFunc {
    return C.BindFunc{
        .kind = C.BindFuncConstEval,
        .ptr = @ptrCast(@constCast(zErrConstEvalFunc2(f))),
        .ptr2 = null,
    };
}

pub fn zErrConstEvalFunc2(comptime f: fn(*cy.Chunk, *cy.ConstEvalContext) anyerror!cy.TypeValue) cy.ZConstEvalFn {
    const S = struct {
        pub fn genFunc(c: *cy.Chunk, ctx: *cy.ConstEvalContext) callconv(.c) cy.TypeValue {
            return @call(.always_inline, f, .{c, ctx}) catch |err| {
                if (cy.compiler.dumpCompileErrorStackTrace and !C.silent()) {
                    std.debug.dumpStackTrace(@errorReturnTrace().?.*);
                }
                if (err != error.CompileError) {
                    _ = c.addReportFmt("error.{}", &.{v(err)}, ctx.node) catch @panic("");
                }
                return cy.TypeValue{ .type = &cy.types.NullType, .value = Value.Void };
            };
        }
    };
    return @ptrCast(@constCast(&S.genFunc));
}

pub fn zDeclFunc(comptime eval_f: ?fn (*cy.Chunk, *cy.ConstEvalContext) anyerror!cy.TypeValue) C.BindFunc {
    var res = C.BindFunc{
        .kind = C.BindFuncDecl,
        .ptr = null,
        .ptr2 = null,
    };
    if (eval_f) |eval_f_| {
        res.ptr2 = @ptrCast(@constCast(zErrConstEvalFunc2(eval_f_)));
    }
    return res;
}

pub fn zErrBuiltinFunc(comptime f: fn(*cy.Chunk, *cy.BuiltinContext, *sema.ExprResult) anyerror!void, comptime f2: ?fn(*cy.Chunk, *cy.BuiltinContext) anyerror!TypeValue) C.BindFunc {
    var res = C.BindFunc{
        .kind = C.BindFuncBuiltin,
        .ptr = @ptrCast(zErrBuiltinFunc2(f)),
        .ptr2 = null,
    };
    if (f2) |const_eval| {
        res.ptr2 = @ptrCast(zErrBuiltinEvalFunc(const_eval));
    }
    return res;
}

pub fn zErrBuiltinFunc2(comptime f: fn(*cy.Chunk, *cy.BuiltinContext, *sema.ExprResult) anyerror!void) cy.ZBuiltinFn {
    const S = struct {
        pub fn genFunc(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) callconv(.c) bool {
            @call(.always_inline, f, .{c, ctx, out}) catch |err| {
                if (cy.compiler.dumpCompileErrorStackTrace and !C.silent()) {
                    std.debug.dumpStackTrace(@errorReturnTrace().?.*);
                    std.debug.print("evalFunc: {}\n", .{err});
                }
                if (err != error.CompileError) {
                    _ = c.addReportFmt("error.{}", &.{v(err)}, ctx.node) catch @panic("");
                }
                return false;
            };
            return true;
        }
    };
    return @ptrCast(@constCast(&S.genFunc));
}

pub fn zErrBuiltinEvalFunc(comptime f: fn(*cy.Chunk, *cy.BuiltinContext) anyerror!cy.TypeValue) cy.ZBuiltinEvalFn {
    const S = struct {
        pub fn genFunc(c: *cy.Chunk, ctx: *cy.BuiltinContext) callconv(.c) cy.TypeValue {
            return @call(.always_inline, f, .{c, ctx}) catch |err| {
                if (cy.compiler.dumpCompileErrorStackTrace and !C.silent()) {
                    std.debug.dumpStackTrace(@errorReturnTrace().?.*);
                }
                if (err != error.CompileError) {
                    _ = c.addReportFmt("error.{}", &.{v(err)}, ctx.node) catch @panic("");
                }
                return cy.TypeValue{ .type = &cy.types.NullType, .value = Value.Void };
            };
        }
    };
    return @ptrCast(@constCast(&S.genFunc));
}

pub fn funcDecl() C.BindFunc {
    return .{
        .kind = C.BindFuncDecl,
        .ptr = undefined,
    };
}

pub fn cFunc(f: *const fn(t: *cy.Thread) callconv(.c) C.Ret) C.BindFunc {
    return .{
        .kind = C.BindFuncVm,
        .ptr = @ptrCast(@constCast(f)),
    };
}

pub fn unsupported(t: *cy.Thread) callconv(.c) Value {
    return t.ret_panic("Unsupported.");
}

pub fn zErrFunc(comptime f: fn (t: *cy.Thread) anyerror!C.Ret) C.BindFunc {
    const S = struct {
        pub fn genFunc(t: *cy.Thread) callconv(.c) C.Ret {
            return @call(.always_inline, f, .{t}) catch |err| {
                return @call(.never_inline, prepPanicZError, .{t, err, @errorReturnTrace()});
            };
        }
    };
    return .{
        .kind = C.BindFuncVm,
        .ptr = @ptrCast(@constCast(&S.genFunc)),
    };
}

pub fn zErrFunc2(comptime f: fn (t: *cy.Thread) anyerror!C.Ret, comptime eval_f: ?fn (*cy.Chunk, *cy.ConstEvalContext) anyerror!cy.TypeValue) C.BindFunc {
    const S = struct {
        pub fn genFunc(t: *cy.Thread) callconv(.c) C.Ret {
            return @call(.always_inline, f, .{t}) catch |err| {
                return @call(.never_inline, prepPanicZError, .{t, err, @errorReturnTrace()});
            };
        }
    };
    var res = C.BindFunc{
        .kind = C.BindFuncVm,
        .ptr = @ptrCast(@constCast(&S.genFunc)),
    };
    if (eval_f) |eval_f_| {
        res.ptr2 = @ptrCast(@constCast(zErrConstEvalFunc2(eval_f_)));
    }
    return res;
}

pub fn prepPanicZError(t: *cy.Thread, err: anyerror, optTrace: ?*std.builtin.StackTrace) C.Ret {
    if (!cy.isFreestanding and C.verbose()) {
        logger.tracev("{}", .{err});
        if (optTrace) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }
    }
    return t.ret_panic(@errorName(err));
}

fn createFuncSymType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;
    const sig = instance.getParamValue("SIG").?.asPtr(*cy.FuncSig);

    const new_t = c.sema.createType(.func_sym, .{ .sig = sig }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createOpaqueFuncType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) ?*C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;

    if (instance.getParam("SIG").?.type.id() != bt.Type) {
        return null;
    }
    const fn_t = instance.getParamValue("SIG").?.asPtr(*cy.Type);
    if (fn_t.kind() != .func_ptr) {
        return null;
    }
    const sig = fn_t.cast(.func_ptr).sig;

    const new_t = c.sema.createType(.func, .{ .sig = sig, .opaque_ = true }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createFuncType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) ?*C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;

    if (instance.getParam("SIG").?.type.id() != bt.Type) {
        return null;
    }
    const fn_t = instance.getParamValue("SIG").?.asPtr(*cy.Type);
    if (fn_t.kind() != .func_ptr) {
        return null;
    }
    const sig = fn_t.cast(.func_ptr).sig;

    const new_t = c.sema.createType(.func, .{ .sig = sig }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createExternFuncPtrType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;
    const sig = instance.getParamValue("SIG").?.asPtr(*cy.FuncSig);

    const new_t = c.sema.createType(.func_ptr, .{ .sig = sig, .extern_ = true }) catch @panic("error");
    return @ptrCast(new_t);
}

fn create_thread_type(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    const new_t = c.sema.createTypeWithId(.pointer, bt.Thread, .{ .ref = false, .child_t = c.sema.void_t }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createFuncSigType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    const new_t = c.sema.createTypeWithId(.pointer, bt.FuncSig, .{ .ref = false, .child_t = c.sema.void_t }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createFuncPtrType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;
    const sig = instance.getParamValue("SIG").?.asPtr(*cy.FuncSig);

    const new_t = c.sema.createType(.func_ptr, .{ .sig = sig }) catch @panic("error");
    return @ptrCast(new_t);
}

fn create_dyn_type(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) ?*C.Type {
    _ = vm;
    const node: *ast.Node = @ptrCast(@alignCast(decl));
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;
    const child_t = instance.getParamValue("T").?.asPtr(*cy.Type);
    if (child_t.kind() != .generic_trait) {
        c.reportError("Expected generic trait type.", node) catch {};
        return null;
    }

    const new_t = c.sema.createType(.dyn_trait, .{ .generic = child_t.cast(.generic_trait) }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createRefType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;
    const child_t = instance.getParamValue("T").?.asPtr(*cy.Type);
    if (child_t.kind() == .dyn_trait) {
        const new_t = c.sema.createType(.ref_trait, .{ .generic = child_t.cast(.dyn_trait).generic }) catch @panic("error");
        return @ptrCast(new_t);
    }

    const new_t = c.sema.createType(.pointer, .{ .ref = true, .child_t = child_t }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createExBorrowType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;
    const child_t = instance.getParamValue("T").?.asPtr(*cy.Type);
    if (child_t.kind() == .generic_trait) {
        @panic("TODO");
    }

    const new_t = c.sema.createType(.ex_borrow, .{ .child_t = child_t }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createBorrowType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;
    const child_t = instance.getParamValue("T").?.asPtr(*cy.Type);
    if (child_t.kind() == .dyn_trait) {
        const new_t = c.sema.createType(.borrow_trait, .{ .generic = child_t.cast(.dyn_trait).generic }) catch @panic("error");
        return @ptrCast(new_t);
    }

    const new_t = c.sema.createType(.borrow, .{ .child_t = child_t }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createPtrType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;
    const child_t = instance.getParamValue("T").?.asPtr(*cy.Type);

    const new_t = c.sema.createType(.pointer, .{ .ref = false, .child_t = child_t }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createRawType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.cast(.type).instance.?;
    const bits = instance.getParamValue("Bits").?.as_eval_int();
    const type_id = switch (bits) {
        64 => bt.R64,
        32 => bt.R32,
        16 => bt.R16,
        8  => bt.R8,
        else => {
            std.debug.panic("Unsupported. `{}` bits is not supported for `Byte`.", .{bits});
        },
    };
    const new_t = c.sema.createTypeWithId(.raw, type_id, .{ .bits = @intCast(bits), .distinct = false }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createErrorType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    const new_t = c.sema.createTypeWithId(.raw, bt.Error, .{ .bits = 64, .distinct = true }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createSymbolType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    const new_t = c.sema.createTypeWithId(.raw, bt.Symbol, .{ .bits = 64, .distinct = true }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createIntType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.cast(.type).instance.?;
    const bits = instance.getParamValue("Bits").?.as_eval_int();
    const type_id = switch (bits) {
        64 => bt.I64,
        32 => bt.I32,
        16 => bt.I16,
        8  => bt.I8,
        else => {
            std.debug.panic("Unsupported. `{}` bits is not supported for `Int`.", .{bits});
        },
    };
    const new_t = c.sema.createTypeWithId(.int, type_id, .{ .bits = @intCast(bits) }) catch @panic("error");
    return @ptrCast(new_t);
}

fn create_eval_str_type(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.eval_ref, bt.EvalStr, .{}) catch @panic("error");
    new_t.info.ct = true;
    return @ptrCast(new_t);
}

fn create_eval_buffer_type(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createType(.eval_ref, .{}) catch @panic("error");
    new_t.info.ct = true;
    return @ptrCast(new_t);
}

fn create_eval_int_type(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.eval_int, bt.EvalInt, .{}) catch @panic("error");
    new_t.info.ct = true;
    return @ptrCast(new_t);
}

fn createFloatType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.cast(.type).instance.?;
    const bits = instance.getParamValue("Bits").?.as_eval_int();
    const type_id = switch (bits) {
        64 => bt.F64,
        32 => bt.F32,
        else => {
            std.debug.panic("Unsupported. `{}` bits is not supported for `Float`.", .{bits});
        },
    };
    const new_t = c.sema.createTypeWithId(.float, type_id, .{ .bits = @intCast(bits) }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createNeverType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.never, bt.Never, .{}) catch @panic("error");
    c.sema.never_t = new_t;
    return @ptrCast(new_t);
}

fn createVoidType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.void, bt.Void, .{}) catch @panic("error");
    c.sema.void_t = new_t;
    return @ptrCast(new_t);
}

fn createBoolType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.bool, bt.Bool, .{}) catch @panic("error");
    c.sema.bool_t = new_t;
    return @ptrCast(new_t);
}

fn createInferType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.generic, bt.Infer, .{}) catch @panic("error");
    return @ptrCast(new_t);
}

fn createDependentType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.generic, bt.Dependent, .{}) catch @panic("error");
    return @ptrCast(new_t);
}

fn create_any_type(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.generic, bt.Any, .{}) catch @panic("error");
    new_t.info.generic = true;
    return @ptrCast(new_t);
}

fn createResultType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;
    const child_t = instance.getParamValue("T").?.asPtr(*cy.Type);

    const new_t = c.sema.createType(.result, .{
        .child_t = child_t,
    }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createOptionType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.instance().?;
    const child_t = instance.getParamValue("T").?.asPtr(*cy.Type);

    const zero_union = child_t.kind() == .pointer or child_t.kind() == .func_ptr or child_t.kind() == .eval_ref;
    const new_t = c.sema.createType(.option, .{
        .child_t = child_t,
        .zero_union = zero_union,
    }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createGenericVectorType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) ?*C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.cast(.type).instance.?;
    const elem_t = instance.getParamValue("T").?.asPtr(*cy.Type);
    const new_t = c.sema.createType(.generic_vector, .{ .elem_t = elem_t }) catch @panic("error");

    return @ptrCast(new_t);
}

fn createPartialVectorType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) ?*C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.cast(.type).instance.?;
    const n: usize = @intCast(instance.getParamValue("N").?.asInt());
    const elem_t = instance.getParamValue("T").?.asPtr(*cy.Type);
    const new_t = c.sema.createType(.partial_vector, .{ .n = n, .elem_t = elem_t }) catch @panic("error");

    return @ptrCast(new_t);
}

fn createVectorType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) ?*C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = sema.getResolveContext(c);
    const instance = ctx.data.sym.cast(.type).instance.?;
    const n: usize = @intCast(instance.getParamValue("N").?.asInt());
    const elem_t = instance.getParamValue("T").?.asPtr(*cy.Type);
    const new_t = c.sema.createType(.vector, .{ .n = n, .elem_t = elem_t }) catch @panic("error");

    return @ptrCast(new_t);
}

pub fn copyStruct(c: *cy.Chunk, ctx: *cy.BuiltinContext, res: *sema.ExprResult) !void {
    const struct_t = (try cte.evalType(c, ctx.args[0])).cast(.struct_t);

    const ptr_n = ctx.args[1];
    const ptr = try c.sema_expr_cstr_template(ptr_n, c.sema.ptr_tmpl);

    var b: sema.InitBuilder = .{ .c = c };
    const fields = struct_t.fields();
    try b.begin(@ptrCast(struct_t), fields.len, ctx.node);
    for (fields, 0..) |field, i| {
        var arg = try sema.semaField(c, ptr, ptr_n, i, field.type, ctx.node);
        arg = try sema.semaOwn(c, arg, ctx.node);
        try b.pushArg(arg, ctx.node);
    }
    const loc = b.end();
    res.* = sema.ExprResult.initOwned(loc);
}

pub fn bitTrunc_eval(c: *cy.Chunk, ctx: *cy.BuiltinContext) !cy.TypeValue {
    const target_t = try cte.evalType(c, ctx.args[0]);
        const child = try cy.cte.eval(c, ctx.args[1]);
    return cy.TypeValue.init(target_t, child.value);
}

pub fn bitTrunc(c: *cy.Chunk, ctx: *cy.BuiltinContext, res: *sema.ExprResult) !void {
    const target_t = try cte.evalType(c, ctx.args[0]);
    const src = try c.semaExpr(ctx.args[1], .{});
    var src_bits: u32 = undefined;
    if (src.type.kind() == .int) {
        src_bits = src.type.cast(.int).bits;
    } else if (src.type.kind() == .raw) {
        src_bits = src.type.cast(.raw).bits;
    } else {
        return c.reportError("Expected `Int` or `Raw` source type.", ctx.node);
    }
    var target_bits: u32 = undefined;
    if (target_t.kind() == .int) {
        target_bits = target_t.cast(.int).bits;
    } else if (target_t.kind() == .raw) {
        target_bits = target_t.cast(.raw).bits;
    } else {
        return c.reportError("Expected `Int` or `Raw` target type.", ctx.node);
    }
    const expr = try c.ir.newExpr(.trunc, target_t, ctx.node, .{
        .expr = src.ir,
        .from_bits = @intCast(src_bits),
        .to_bits = @intCast(target_bits),
    });
    res.* = sema.ExprResult.init2(expr);
}

pub fn zext(c: *cy.Chunk, ctx: *cy.BuiltinContext, res: *sema.ExprResult) !void {
    const target_t = try cte.evalType(c, ctx.args[0]);
    if (target_t.kind() != .int and target_t.kind() != .raw) {
        return c.reportError("Expected `Int` or `Raw` target type.", ctx.node);
    }

    const src = try c.semaExpr(ctx.args[1], .{});
    var src_bits: u32 = undefined;
    if (src.type.kind() == .int) {
        src_bits = src.type.cast(.int).bits;
    } else if (src.type.kind() == .raw) {
        src_bits = src.type.cast(.raw).bits;
    } else {
        return c.reportError("Expected `Int` or `Raw` source type.", ctx.node);
    }
    const expr = try c.ir.newExpr(.zext, target_t, ctx.node, .{
        .expr = src.ir,
        .src_bits = src_bits,
    });
    res.* = sema.ExprResult.init2(expr);
}

pub fn sext(c: *cy.Chunk, ctx: *cy.BuiltinContext, res: *sema.ExprResult) !void {
    const target_t = try cte.evalType(c, ctx.args[0]);
    if (target_t.kind() != .int and target_t.kind() != .raw) {
        return c.reportError("Expected `Int` or `Raw` target type.", ctx.node);
    }

    const src = try sema.semaExpr(c, ctx.args[1], .{});
    var src_bits: u32 = undefined;
    if (src.type.kind() == .int) {
        src_bits = src.type.cast(.int).bits;
    } else if (src.type.kind() == .raw) {
        src_bits = src.type.cast(.raw).bits;
    } else {
        return c.reportError("Expected `Int` or `Raw` source type.", ctx.node);
    }
    const expr = try c.ir.newExpr(.sext, target_t, ctx.node, .{
        .expr = src.ir,
        .src_bits = src_bits,
    });
    res.* = sema.ExprResult.init2(expr);
}

pub fn intToFloat(c: *cy.Chunk, ctx: *cy.BuiltinContext, res: *sema.ExprResult) !void {
    const target_t = try cte.evalType(c, ctx.args[0]);
    if (target_t.kind() != .float) {
        return c.reportError("Expected float type.", ctx.node);
    }
    const int_n = ctx.args[1];
    const int = try c.semaExprCstr(int_n, c.sema.i64_t);
    res.* = try sema.semaIntToFloat(c, target_t, int, ctx.node);
}

pub fn floatToInt(c: *cy.Chunk, ctx: *cy.BuiltinContext, res: *sema.ExprResult) !void {
    const float_t = try cte.evalType(c, ctx.args[0]);
    if (float_t.kind() != .float) {
        return c.reportError("Expected float type.", ctx.node);
    }
    const float_n = ctx.args[1];
    const float = try c.semaExprCstr(float_n, float_t);
    const expr = try c.ir.newExpr(.f2i, c.sema.i64_t, ctx.node, .{
        .expr = float.ir,
    });
    res.* = sema.ExprResult.init2(expr);
}

pub fn consume(c: *cy.Chunk, ctx: *cy.BuiltinContext, res: *sema.ExprResult) !void {
    const expr_n = ctx.args[0];
    const expr = try c.semaExpr(expr_n, .{});
    if (expr.resType != .value_local) {
        return c.reportError("Expected local.", expr_n);
    }
    const local = expr.data.value_local;
    c.varStack.items[local].type = .dead;
    res.* = sema.ExprResult.initVoid(c.sema.void_t);
}

pub fn cmp(c: *cy.Chunk, ctx: *cy.BuiltinContext, res: *sema.ExprResult) !void {
    const a_n = ctx.args[0];
    const a = try c.semaExpr(a_n, .{});

    if (a.type.kind() != .int) {
        return c.reportErrorFmt("Expected comparable primitive type. Found `{}`.", &.{v(a.type.kind())}, a_n);
    }

    const b_n = ctx.args[0];
    const b = try c.semaExprCstr(b_n, a.type);

    const loc = try c.ir.newExpr(.binary_op, c.sema.bool_t, ctx.node, .{
        .leftT = a.type,
        .rightT = b.type,
        .op = .equal_equal,
        .left = a.ir,
        .right = b.ir,
    });
    res.* = sema.ExprResult.init(loc, c.sema.bool_t);
}

pub fn nop(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const label = ctx.args[0].asPtr(*cy.heap.EvalStr).slice();
    defer c.heap.release(ctx.args[0]);
    try sema.semaNop(c, label, ctx.node);
    return cy.TypeValue.init(c.sema.void_t, Value.Void);
}

/// Compile-time version of Int conversions. It should be faster than interpreting `Int[].@init`.
pub fn Int_init(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const rec_bits = ctx.func.parent.parent.?.cast(.type).instance.?.params[0].as_eval_int();
    const val_t = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    switch (rec_bits) {
        8 => {
            return c.reportError("TODO", ctx.node);
        },
        16 => {
            switch (val_t.id()) {
                bt.I64 => {
                    const i = ctx.args[0].asInt();
                    if (i < std.math.minInt(i16) or i > std.math.maxInt(i16)) {
                        return c.reportErrorFmt("`i16` cannot represent: {}", &.{v(i)}, ctx.node);
                    }
                    return cy.TypeValue.init(c.sema.i16_t, Value.initInt16(@intCast(i)));
                },
                else => {
                    return c.reportError("TODO", ctx.node);
                },
            }
        },
        32 => {
            switch (val_t.id()) {
                bt.I64 => {
                    const i = ctx.args[0].asInt();
                    if (i < std.math.minInt(i32) or i > std.math.maxInt(i32)) {
                        return c.reportErrorFmt("`i32` cannot represent: {}", &.{v(i)}, ctx.node);
                    }
                    return cy.TypeValue.init(c.sema.i32_t, Value.initInt32(@intCast(i)));
                },
                else => {
                    return c.reportError("TODO", ctx.node);
                },
            }
        },
        64 => {
            switch (val_t.id()) {
                else => {
                    return c.reportError("TODO", ctx.node);
                },
            }
        },
        else => {
            return error.Unexpected;
        },
    }
}

pub fn bitCast_eval(c: *cy.Chunk, ctx: *cy.BuiltinContext) !cy.TypeValue {
    const target_t = try cte.evalType(c, ctx.args[0]);
    // Extract S from Expr[S].
    const src = try cte.eval(c, ctx.args[1]);
    const src_t = src.type;

    if (target_t.kind() == .int) {
        const target_int_t = target_t.cast(.int);
        if (src_t.kind() == .int and src_t.cast(.int).bits == target_int_t.bits) {
            return cy.TypeValue.init(target_t, src.value);
        }
    }

    const src_name = try c.sema.allocTypeName(src_t);
    defer c.alloc.free(src_name);
    const target_name = try c.sema.allocTypeName(target_t);
    defer c.alloc.free(target_name);
    return c.reportErrorFmt("Cannot bitcast `{}` to `{}`.", &.{v(src_name), v(target_name)}, ctx.node);
}

pub fn breakpoint(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    _ = try c.ir.pushStmt(.breakpoint, ctx.node, .{});
    out.* = try c.semaVoid(ctx.node);
}

pub fn bitCast(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    const target_t = try cte.evalType(c, ctx.args[0]);
    const src = try c.semaExpr(ctx.args[1], .{});

    var can_bitcast = true;
    b: {
        switch (target_t.kind()) {
            .pointer => {
                if (target_t.cast(.pointer).ref) {
                    can_bitcast = false;
                    break :b;
                }
            },
            .vector => {
                const target_vector_t = target_t.cast(.vector);
                if (src.type.kind() == .vector) {
                    const vector_t = src.type.cast(.vector);
                    if (target_vector_t.n == vector_t.n and target_vector_t.elem_t.id() == vector_t.elem_t.id()) {
                        break :b;
                    }
                }
                can_bitcast = false;
                break :b;
            },
            .bool,
            .enum_t,
            .float,
            .raw,
            .int => {},
            else => {
                can_bitcast = false;
                break :b;
            },
        }

        switch (src.type.kind()) {
            .pointer => {
                if (src.type.cast(.pointer).ref) {
                    can_bitcast = false;
                    break :b;
                }
            },
            .bool,
            .enum_t,
            .raw,
            .float,
            .int => {},
            else => {
                can_bitcast = false;
                break :b;
            },
        }
    }

    if (can_bitcast) {
        out.* = try sema.semaBitcast(c, src, target_t, ctx.node);
        out.owned = true;
        return;
    } else {
        const src_name = try c.sema.allocTypeName(src.type);
        defer c.alloc.free(src_name);
        const target_name = try c.sema.allocTypeName(target_t);
        defer c.alloc.free(target_name);
        return c.reportErrorFmt("Cannot bitcast `{}` to `{}`.", &.{v(src_name), v(target_name)}, ctx.node);
    }
}

pub fn unsafeCast(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    const cast_t = try cte.evalType(c, ctx.args[0]);
    const src_n = ctx.args[1];
    const src = try c.semaExpr(src_n, .{});

    if (cast_t.kind() == .int) {
        if (src.type.kind() == .borrow) {
            if (src.type.size() < cast_t.size()) {
                out.* = try sema.semaZext(c, cast_t, @intCast(src.type.size() * 8), src, ctx.node);
                return;
            }
        }
    }

    out.* = try sema.semaBitcast(c, src, cast_t, ctx.node);
    out.*.owned = true;
}

pub fn panic(t: *cy.Thread) anyerror!C.Ret {
    _ = t.ret(void);
    const msg = t.param(cy.heap.Str).slice();
    return t.ret_panic(msg);
}

pub fn abort(t: *cy.Thread) anyerror!C.Ret {
    _ = t.ret(void);
    const msg = t.param(cy.heap.Str).slice();
    @panic(msg);
}

pub fn str_initRune(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Str);
    const num = t.param(u64);
    if (num >= 2 << 21) {
        return t.ret_panic("InvalidRune");
    }
    const rune: u21 = @intCast(num);
    if (std.unicode.utf8ValidCodepoint(rune)) {
        var buf: [4]u8 = undefined;
        const len = try std.unicode.utf8Encode(rune, &buf);
        ret.* = try t.heap.init_str(buf[0..len]);
        return C.RetOk;
    } else {
        return t.ret_panic("InvalidRune");
    }
}

pub fn memory(t: *cy.Thread) !C.Ret {
    _ = t;
    // const DefaultAllocatorT = core.getSym("DefaultAllocator").?.getStaticType().?;
    // const impl = try c.vm.allocObjectSmall(DefaultAllocatorT.id(), &.{});
    // const AllocatorT = core.getSym("Allocator").?.getStaticType().?;
    // const vtable_idx = c.gen_vtables.get(bc.VtableKey{ .type = DefaultAllocatorT.id(), .trait = AllocatorT.id() }).?;
    // const imem = try c.vm.allocTrait(AllocatorT.id(), @intCast(vtable_idx), impl);
    // const mem = try c.vm.allocObjectSmall(c.sema.memory_t.id(), &.{ imem });
    return error.TODO;
}

pub fn refCount(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(i64);
    const obj = t.param(*cy.HeapObject);
    ret.* = @intCast(obj.rc());
    return C.RetOk;
}

pub fn isUniqueRef(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(bool);
    const obj = t.param(*cy.HeapObject);
    ret.* = obj.rc() == 1;
    return C.RetOk;
}

pub fn log(t: *cy.Thread) callconv(.c) C.Ret {
    _ = t.ret(void);
    const str = t.param(cy.heap.Str);
    defer t.heap.destructStr(&str);
    t.c.vm.log(str.slice());
    return C.RetOk;
}

pub fn eprints(t: *cy.Thread) callconv(.c) C.Ret {
    _ = t.ret(void);
    const str = t.param(cy.heap.Str);
    defer t.heap.destructStr(&str);
    t.c.vm.print_err.?(@ptrCast(t.c.vm), C.to_bytes(str.slice()));
    return C.RetOk;
}

pub fn prints(t: *cy.Thread) callconv(.c) C.Ret {
    _ = t.ret(void);
    const str = t.param(cy.heap.Str);
    defer t.heap.destructStr(&str);
    t.c.vm.print.?(@ptrCast(t.c.vm), C.to_bytes(str.slice()));
    return C.RetOk;
}

// pub fn queue_task(t: *cy.Thread) anyerror!C.Ret {
//     _ = t.ret(void);

//     const cb = t.param(cy.Value);
//     t.heap.retain(cb);
//     const task = cy.heap.AsyncTask{
//         .type = .callback,
//         .data = .{ .callback = cb },
//     };
//     try t.c.vm.ready_tasks.writeItem(task);
//     return C.RetOk;
// }

pub fn result_unwrap(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    const val_t = try cte.evalType(c, ctx.args[0]);
    const res_t = try sema.getResultType(c, val_t);

    const res_n = ctx.args[1];
    const borrow_res_t = try sema.getBorrowType(c, res_t);
    const res = try c.semaExprCstr(res_n, borrow_res_t);

    const ptr_t = try sema.getPtrType(c, val_t);
    const loc = try c.ir.newExpr(.unwrap_addr, ptr_t, ctx.node, .{
        .choice = res.ir,
        .tag = 1,
    });
    out.* = sema.ExprResult.init(loc, ptr_t);
    out.* = try sema.semaDeref(c, out.*, ctx.node);
    out.* = try sema.semaOwn(c, out.*, ctx.node);
}

pub fn result_unwrap_error(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    const res = try c.semaExpr(ctx.args[0], .{});
    const ptr_t = try sema.getPtrType(c, c.sema.error_t);
    const loc = try c.ir.newExpr(.unwrap_addr, ptr_t, ctx.node, .{
        .choice = res.ir,
        .tag = 0,
    });
    out.* = sema.ExprResult.init2(loc);
    out.* = try sema.semaDeref(c, out.*, ctx.node);
    out.* = try sema.semaOwn(c, out.*, ctx.node);
}

pub fn freeObject(t: *cy.Thread) callconv(.c) C.Ret {
    _ = t.ret(void);
    const obj = t.param(*cy.HeapObject);
    const size: usize = @intCast(t.getR64(1));
    logger.tracev("free type={}({s}) {*} {}", .{ obj.getTypeId(), t.heap.getType(obj.getTypeId()).name(), obj, size });
    t.heap.freeObject(obj, size);
    return C.RetOk;
}

fn EvalStr_init(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const type_ = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    const value = ctx.args[0];
    defer c.heap.destructValue2(type_, value);
    switch (type_.id()) {
        bt.I64 => {
            const res = try std.fmt.allocPrint(c.alloc, "{}", .{value.asInt()});
            defer c.alloc.free(res);
            return cy.TypeValue.init(ctx.func.sig.ret, try c.heap.init_eval_str(res));
        },
        else => {
            const name = try c.sema.allocTypeName(type_);
            defer c.alloc.free(name);
            return c.reportErrorFmt("Unsupported conversion from `{}` to `EvalStr`.", &.{v(name)}, ctx.node);
        },
    }
}

fn EvalStr_concat(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const left = ctx.args[0].asPtr(*cy.heap.EvalStr);
    defer c.heap.release(ctx.args[0]);

    const right = ctx.args[1].asPtr(*cy.heap.EvalStr);
    defer c.heap.release(ctx.args[1]);

    const ascii = left.ascii() and right.ascii();
    const res = try c.heap.init_eval_str_concat(left.slice(), right.slice(), ascii);
    return cy.TypeValue.init(ctx.func.sig.ret, res);
}

fn EvalStr_len(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const rec = ctx.args[0].asPtr(*cy.heap.EvalStr);
    defer c.heap.release(ctx.args[0]);
    return cy.TypeValue.init(ctx.func.sig.ret, Value.initInt(@intCast(rec.len())));
}

fn EvalStr_index(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const rec = ctx.args[0].asPtr(*cy.heap.EvalStr);
    defer c.heap.release(ctx.args[0]);

    const idx = ctx.args[1].asUint();
    if (idx >= rec.len()) {
        return c.reportError("Out of bounds.", ctx.node);
    }
    return TypeValue.init(ctx.func.sig.ret, Value.initR8(rec.slice()[@intCast(idx)]));
}

fn EvalBuffer_len(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const buf = ctx.args[0].asPtr(*cy.heap.EvalBuffer);
    defer c.heap.release(ctx.args[0]);
    return TypeValue.init(ctx.func.sig.ret, Value.initR64(buf.len));
}

fn EvalBuffer_index_addr(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const buf = ctx.args[0].asPtr(*cy.heap.EvalBuffer);
    defer c.heap.release(ctx.args[0]);

    const elem_t = ctx.func.sig.ret.cast(.pointer).child_t;
    const idx = ctx.args[1].asUint();
    if (idx >= buf.len) {
        return c.reportError("Out of bounds.", ctx.node);
    }
    return TypeValue.init(ctx.func.sig.ret, Value.initRaw(buf.ptr + idx * elem_t.size()));
}

fn compute_int_min(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const type_ = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    if (type_.kind() != .int) {
        return c.reportErrorFmt("Expected integer type, found `{}`.", &.{v(type_.name())}, ctx.node);
    }
    switch (type_.id()) {
        bt.I64 => {
            const val = Value.initInt(std.math.minInt(i64));
            return cy.TypeValue.init(c.sema.i64_t, val);
        },
        bt.I32 => {
            const val = Value.initInt32(std.math.minInt(i32));
            return cy.TypeValue.init(c.sema.i32_t, val);
        },
        bt.I16 => {
            const val = Value.initInt16(std.math.minInt(i16));
            return cy.TypeValue.init(c.sema.i16_t, val);
        },
        bt.I8 => {
            const val = Value.initInt8(std.math.minInt(i8));
            return cy.TypeValue.init(c.sema.i8_t, val);
        },
        else => {
            @panic("unexpected");
        },
    }
}

fn compute_int_max(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const type_ = ctx.func.instance.?.params[0].asPtr(*cy.Type);
    if (type_.kind() != .int) {
        return c.reportErrorFmt("Expected integer type, found `{}`.", &.{v(type_.name())}, ctx.node);
    }
    switch (type_.id()) {
        bt.I64 => {
            const val = Value.initInt(std.math.maxInt(i64));
            return cy.TypeValue.init(c.sema.i64_t, val);
        },
        bt.I16 => {
            const val = Value.initInt16(std.math.maxInt(i16));
            return cy.TypeValue.init(c.sema.i16_t, val);
        },
        bt.I32 => {
            const val = Value.initInt32(std.math.maxInt(i32));
            return cy.TypeValue.init(c.sema.i32_t, val);
        },
        bt.I8 => {
            const val = Value.initInt8(std.math.maxInt(i8));
            return cy.TypeValue.init(c.sema.i8_t, val);
        },
        else => {
            return c.reportError("TODO", ctx.node);
        },
    }
}

pub fn completeFuture(t: *cy.Thread, future: cy.heap.Future) !void {
    // Copy continuations to the ready queue.
    var opt_node = future.inner.cont_head;
    while (opt_node != cy.NullId) {
        const node = &t.c.vm.task_nodes.items[@intCast(opt_node)];
        // try t.c.vm.ready_tasks.writeItem(node.task);
        opt_node = node.next;
        node.task.type = .dead;
    }
}

pub fn call(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    const func_ptr = try c.sema_expr_cstr_template(ctx.args[0], c.sema.func_ptr_tmpl);
    var args = try c.semaExpr(ctx.args[1], .{});
    const tuple_t = args.type.cast(.borrow).child_t;
    if (!tuple_t.isInstanceOf(c.sema.fn_tuple_tmpl)) {
        return c.reportError("Expected FnTuple.", ctx.args[1]);
    }
    const sig = func_ptr.type.cast(.func_ptr).sig;
    if (tuple_t.cast(.struct_t).base.sym().instance.?.params[0].asPtr(*cy.FuncSig) != sig) {
        return c.reportError("Function signature mismatch.", ctx.args[1]);
    }
    args = try sema.semaEnsureAddressable(c, args, ctx.args[1]);

    const ir_args = try c.ir.allocArray(*ir.Expr, sig.params_len);
    for (sig.params(), 0..) |param, i| {
        var arg = try sema.semaField(c, args, ctx.node, i, param.get_type(), ctx.node);
        arg = try cy.sema_func.ownOrManageArg(c, arg, ctx.node);
        // arg = try sema.semaOwn(c, arg, ctx.node);
        ir_args[i] = arg.ir;
    }

    const expr = try c.ir.newExpr(.call_ptr, sig.ret, ctx.node, .{
        .callee = func_ptr.ir,
        .nargs = @intCast(ir_args.len),
        .args = ir_args.ptr,
    });
    out.* = sema.ExprResult.initOwned(expr);
}

pub fn new_shared_state(t: *cy.Thread) !C.Ret {
    t.c.vm.shared_state_mutex.lock();
    defer t.c.vm.shared_state_mutex.unlock();

    const ret = t.ret(*u8);
    const state = try t.alloc.create(u8);
    try t.c.vm.shared_states.append(t.alloc, state);
    ret.* = state;
    return C.RetOk;
}

pub fn start_thread(t: *cy.Thread) !C.Ret {
    _ = t.ret(void);
    const thread = t.param(*cy.Thread);
    const entry_func = t.param(cy.Value);
    const func = t.param(cy.Value);
    const args_size: usize = @intCast(t.param(i64));
    const func_args = t.param_sized(args_size);
    const future = t.param(*cy.heap.FutureValue);
    const ret_size: usize = @intCast(t.param(i64));
    const ret_reg_size = (ret_size + 7) >> 3;

    // Prepare entry func call arguments.
    thread.c.stack_ptr[ret_reg_size + 4] = Value.initPtr(t);
    thread.c.stack_ptr[ret_reg_size + 5] = func;
    @memcpy(thread.c.stack_ptr[ret_reg_size+6..ret_reg_size+6+func_args.len], func_args);
    try t.c.vm.worker_pool.add_thread_task(thread, entry_func, future, ret_size);

    return C.RetOk;
}

pub fn await_(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    const fut = try c.semaExpr(ctx.args[0], .{});
    _ = try c.ir.pushStmt(.await_, ctx.node, .{ .expr = fut.ir });
    out.* = try c.semaVoid(ctx.node);
}

pub fn notifyFutureComplete(t: *cy.Thread) anyerror!C.Ret {
    _ = t.ret(void);
    const future = t.param(cy.heap.Future);

    try completeFuture(t, future);
    return C.RetOk;
}

pub fn symbol_name(t: *cy.Thread) !C.Ret {
    const ret = t.ret(cy.heap.Str);
    const sym = t.param(u64);
    ret.* = try t.heap.init_str(t.heap.getSymbolName(sym));
    return C.RetOk;
}

pub fn readCodepoint(t: *cy.Thread) !Value {
    return t.ret_panic("Unsupported.");
}

pub fn trackMainLocal(t: *cy.Thread) !C.Ret {
    _ = t.ret(void);
    const name = t.param(cy.heap.Str).slice();
    const type_id = t.param(i64);
    const val = t.param([*]Value);
    const val_t = t.heap.getType(@intCast(type_id));
    const reg_size = (val_t.size() + 7) >> 3;
    const res = try t.c.vm.env_local_saves_map.getOrPut(t.alloc, name);
    if (!res.found_existing) {
        const name_dup = try t.alloc.dupe(u8, name);
        res.key_ptr.* = name_dup;
        res.value_ptr.* = t.c.vm.env_local_saves.items.len;
        try t.c.vm.env_local_saves.append(t.alloc, .{
            .name = name_dup,
            .val_t = val_t,
            .value = val[0..reg_size],
        });
    } else {
        const name_dup = try t.alloc.dupe(u8, name);
        res.key_ptr.* = name_dup;
        t.c.vm.env_local_saves.items[res.value_ptr.*].deinit(t.alloc);
        t.c.vm.env_local_saves.items[res.value_ptr.*] = .{
            .name = name_dup,
            .val_t = val_t,
            .value = val[0..reg_size],
        };
    }
    return C.RetOk;
}

pub fn memset(t: *cy.Thread) !C.Ret {
    _ = t.ret(void);
    const dst = t.param([*]u8);
    const val = t.param([*]u8);
    const val_len: usize = @intCast(t.param(u64));
    const n: usize = @intCast(t.param(u64));
    if (val_len == 1) {
        @memset(dst[0..n], val[0]);
    } else {
        for (0..n) |i| {
            @memcpy(dst[i*val_len..(i+1)*val_len], val[0..val_len]);
        }
    }
    return C.RetOk;
}

pub fn memcpy(t: *cy.Thread) !C.Ret {
    _ = t.ret(void);
    const dst = t.param([*]u8);
    const src = t.param([*]u8);
    const len: usize = @intCast(t.param(u64));
    if (len == 0) {
        return C.RetOk;
    }
    @memcpy(dst[0..len], src[0..len]);
    return C.RetOk;
}

pub fn ref_addr(t: *cy.Thread) !C.Ret {
    const ret = t.ret(u64);
    const ref = t.param(*anyopaque);
    ret.* = @intFromPtr(ref);
    return C.RetOk;
}

pub fn new_thread(t: *cy.Thread) !C.Ret {
    const ret = t.ret(*cy.Thread);
    var thread = try t.alloc.create(cy.Thread);
    const id = t.c.vm.next_thread_id;
    try thread.init(id, t.alloc, t.c.vm);
    t.c.vm.next_thread_id += 1;
    ret.* = thread;
    return C.RetOk;
}

pub fn memmove(t: *cy.Thread) !C.Ret {
    _ = t.ret(void);
    const dst = t.param([*]u8);
    const src = t.param([*]u8);
    const len: usize = @intCast(t.param(u64));
    std.mem.copyForwards(u8, dst[0..len], src[0..len]);
    return C.RetOk;
}

pub fn new_object_undef(t: *cy.Thread) !C.Ret {
    const ret = t.ret(*cy.HeapObject);
    const type_id: cy.TypeId = @intCast(t.param(i64));
    const size: usize = @intCast(t.param(u64));
    ret.* = try t.heap.new_object_undef(type_id, size);
    return C.RetOk;
}

pub fn new_object_undef2(t: *cy.Thread) !C.Ret {
    const ret = t.ret(*cy.HeapObject);
    const thread = t.param(*cy.Thread);
    const type_id: cy.TypeId = @intCast(t.param(i64));
    const size: usize = @intCast(t.param(u64));
    ret.* = try thread.heap.new_object_undef(type_id, size);
    return C.RetOk;
}

pub fn alloc(t: *cy.Thread) !C.Ret {
    const ret = t.ret(?[*]align(8) u8);
    const size: usize = @intCast(t.param(i64));
    if (size == 0) {
        ret.* = null;
        return C.RetOk;
    }
    const ptr = try t.heap.new_byte_buffer(size);
    ret.* = ptr;
    return C.RetOk;
}

pub fn free(t: *cy.Thread) anyerror!C.Ret {
    _ = t.ret(void);
    const ptr = t.param(?[*]align(8) u8);
    t.heap.free_byte_buffer(ptr);
    return C.RetOk;
}

fn arrayConcat(t: *cy.Thread) callconv(.c) Value {
    const slice = t.getArray(0);
    const rslice = t.getArray(1);
    return t.heap.allocArrayConcat(slice, rslice) catch fatal();
}

pub fn intAsIndex(i: i64, len: usize) !usize {
    if (i < 0) {
        return error.OutOfBounds;
    }
    const u: u64 = @bitCast(i);
    if (u >= len) {
        return error.OutOfBounds;
    }
    return @intCast(i);
}

fn arrayInsert(t: *cy.Thread) anyerror!Value {
    const slice = t.getArray(0);
    const idx = try intAsIndex(t.getInt(1), slice.len+1);
    const insert = t.getArray(2);
    const new = t.heap.allocUnsetArrayObject(slice.len + insert.len) catch cy.fatal();
    const buf = new.array.getMutSlice();
    @memcpy(buf[0..idx], slice[0..idx]);
    @memcpy(buf[idx..idx+insert.len], insert);
    @memcpy(buf[idx+insert.len..], slice[idx..]);
    return Value.initNoCycPtr(new);
}

fn String_decode(t: *cy.Thread) callconv(.c) Value {
    t.setSymbol(1, @intFromEnum(Symbol.utf8));
    return String_decode2(t);
}

fn String_decode2(t: *cy.Thread) callconv(.c) Value {
    const obj = t.getPtr(*cy.heap.Array, 0);

    const encoding = bindings.getBuiltinSymbol(t.getSymbol(1)) orelse {
        return t.ret_panic("InvalidArgument");
    };
    if (encoding != Symbol.utf8) {
        return t.ret_panic("InvalidArgument");
    }

    const parent = obj.getParent();
    const slice = obj.getSlice();
    if (cy.string.validateUtf8(slice)) |size| {
        // Since the bytes are validated, just return a slice view of the bytes.
        if (size == slice.len) {
            t.heap.retainObject(parent);
            return t.heap.newAstrSlice(slice, parent) catch fatal();
        } else {
            t.heap.retainObject(parent);
            return t.heap.newUstrSlice(slice, parent) catch fatal();
        }
    } else {
        return t.ret_panic("Unicode");
    }
}

fn FuncSig_ret(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const sig = ctx.args[0].asPtr(*cy.FuncSig);
    return cy.TypeValue.init(c.sema.type_t, Value.initPtr(sig.ret));
}

fn FuncSig_num_params(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const sig = ctx.args[0].asPtr(*cy.FuncSig);
    return cy.TypeValue.init(c.sema.i64_t, Value.initInt(@intCast(sig.numParams())));
}

fn FuncSig_param_at(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const sig = ctx.args[0].asPtr(*cy.FuncSig);
    const idx = ctx.args[1].asInt();

    if (idx < 0 and idx >= sig.numParams()) {
        return c.reportError("Out of bounds.", ctx.node);
    }

    const param_t = ctx.func.sig.ret;
    const param = try c.heap.newInstance(param_t, &.{
        field_init("type", cy.Value.initPtr(sig.params_ptr[@intCast(idx)].get_type())),
    });
    return cy.TypeValue.init(param_t, param);
}

const field_init = cy.heap.field_init;

fn Vector_ct_repeat(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const array_t = ctx.func.parent.parent.?.cast(.type).type.cast(.vector);
    const repeat: usize = @intCast(ctx.func.instance.?.params[0].asInt());
    const self = ctx.args[0];

    const new_arr_t = ctx.func.sig.ret;
    const new_arr = try c.heap.new_object_undef(new_arr_t.id(), new_arr_t.size());
    const dst: [*]u8 = @ptrCast(new_arr);
    for (0..repeat) |i| {
        try c.heap.copyValueTo2(&array_t.base, dst + i * array_t.size, self);
    }
    return cy.TypeValue.init(new_arr_t, Value.initPtr(new_arr));
}

pub fn scope_ptr_to_borrow_eval(c: *cy.Chunk, cx: *cy.BuiltinContext) !cy.TypeValue {
    const scope_param = try cte.evalCheck(c, cx.args[0], c.sema.eval_str_t);
    defer c.heap.release(scope_param.value);
    const param_name = scope_param.value.as_eval_str();
    _ = param_name;
    const ptrv = try cte.eval_check_template(c, cx.args[1], c.sema.ptr_tmpl);
    const borrow_t = try sema.getBorrowType(c, ptrv.type.cast(.pointer).child_t);
    const ptr = ptrv.value.asPtr(*anyopaque);
    return cy.TypeValue.init(borrow_t, Value.initPtr(ptr));
}

fn scope_ptr_to_borrow(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    const scope_param = try cte.evalCheck(c, ctx.args[0], c.sema.eval_str_t);
    defer c.heap.release(scope_param.value);
    const param_name = scope_param.value.as_eval_str();

    const res = c.proc().var_cache_entries.get(param_name) orelse {
        return c.reportErrorFmt("Expected param `{}`.", &.{v(param_name)}, ctx.node);
    };
    const ptr = try c.sema_expr_cstr_template(ctx.args[1], c.sema.ptr_tmpl);
    const borrow_t = try sema.getBorrowType(c, ptr.type.cast(.pointer).child_t);

    out.* = try sema.semaBitcast(c, ptr, borrow_t, ctx.node);
    out.*.data.value.parent_local = res.data.local.varId;
}

fn str_byteAt(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(u8);
    const slice = t.param(*cy.heap.Str).slice();
    const idx = t.param(i64);

    if (idx < 0 or idx >= slice.len) return error.OutOfBounds;
    ret.* = slice[@intCast(idx)];
    return C.RetOk;
}

const Endian = enum {
    little,
    big,
};

fn str_index_any_byte(t: *cy.Thread) !C.Ret {
    const ret = t.ret(cy.value.Option(i64));
    const slice = t.param(*cy.heap.Str).slice();
    const set = t.param(cy.heap.Span).items(u8);
    const setIsAscii = cy.string.isAstring(set);
    if (setIsAscii) {
        if (cy.string.indexOfAsciiSet(slice, set)) |idx| {
            ret.* = cy.value.Option(i64).some(@intCast(idx));
            return C.RetOk;
        }
    } else {
        // Slow check against every byte.
        var minIndex: u32 = cy.NullId;
        for (set) |byte| {
            if (cy.string.indexOfChar(slice, byte)) |idx| {
                if (idx < minIndex) {
                    minIndex = @intCast(idx);
                }
            }
        }
        if (minIndex != cy.NullId) {
            ret.* = cy.value.Option(i64).some(@intCast(minIndex));
            return C.RetOk;
        }
    }
    ret.* = cy.value.Option(i64).none();
    return C.RetOk;
}

fn str_index_byte(t: *cy.Thread) !C.Ret {
    const ret = t.ret(cy.value.Option(i64));
    const slice = t.param(*cy.heap.Str).slice();
    const byte = t.param(u8);
    if (cy.string.indexOfChar(slice, @intCast(byte))) |idx| {
        ret.* = cy.value.Option(i64).some(@intCast(idx));
        return C.RetOk;
    }
    ret.* = cy.value.Option(i64).none();
    return C.RetOk;
}

fn str_fmt_bytes(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Str);
    const obj = t.param(*cy.HeapObject);
    const str = obj.string.slice();
    const kind = try std.meta.intToEnum(NumberFormat, t.param(i64));
    if (kind == .asc) {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(t.alloc);

        for (str) |byte| {
            if (byte < 0 or byte > 127) {
                return error.InvalidArgument;
            }
        }
        t.heap.retainObject(obj);
        ret.* = obj.string;
        return C.RetOk;
    } else {
        var base: u8 = undefined;
        var width: u8 = undefined;
        switch (kind) {
            .bin => {
                base = 2;
                width = 8;
            },
            .oct => {
                base = 8;
                width = 3;
            },
            .dec => {
                base = 10;
                width = 3;
            },
            .hex => {
                base = 16;
                width = 2;
            },
            else => return error.InvalidArgument,
        }

        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(t.alloc);
        var adapter = buf.writer(t.alloc).adaptToNewApi(&.{});
        for (str) |byte| {
            try adapter.new_interface.printInt(byte, base, .lower, .{ .width = width, .fill = '0' });
        }
        ret.* = try t.heap.init_astr(buf.items);
        return C.RetOk;
    }
}

fn arrayRepeat(t: *cy.Thread) anyerror!Value {
    const obj = t.getPtr(*cy.heap.Array, 0);
    const slice = obj.slice();

    const n = t.getInt(1);
    if (n < 0) {
        return t.ret_panic("InvalidArgument");
    }

    const un: u32 = @intCast(n);
    const len = un * slice.len;
    if (un > 1 and len > 0) {
        const new = try t.heap.allocUnsetArrayObject(len);
        const buf = new.array.getMutSlice();

        // This is already quite fast since it has good cache locality.
        // Might be faster if the front of the buffer up to a certain size was used to memcpy instead of just 1 `str`.
        var i: u32 = 0;
        var dst: u32 = 0;
        while (i < un) : (i += 1) {
            @memcpy(buf[dst..dst + slice.len], slice);
            dst += @intCast(slice.len);
        }

        return Value.initNoCycPtr(new);
    } else {
        if (un == 0) {
            return t.heap.allocVector("");
        } else {
            t.heap.retainObject(@ptrCast(obj));
            return Value.initNoCycPtr(obj);
        }
    }
}

pub fn gen_next(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    var gen = try c.sema_expr_cstr_template(ctx.args[0], c.sema.generator_tmpl);
    gen = try sema.semaEnsureAddressable(c, gen, ctx.args[0]);

    const ptr_t = try sema.getPtrType(c, gen.type);
    const addr = try sema.semaAddressOf(c, gen, ptr_t, ctx.args[0]);

    const fn_ptr_t = gen.type.cast(.struct_t).base.sym().instance.?.params[0].asPtr(*cy.Type);
    const ret_t = try sema.getOptionType(c, fn_ptr_t.cast(.func_ptr).sig.ret);
    const next = try c.ir.newExpr(.gen_next, ret_t, ctx.node, .{
        .gen = addr.ir,
    });
    out.* = sema.ExprResult.initOwned(next);
}

pub fn gen_end(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    var gen = try c.sema_expr_cstr_template(ctx.args[0], c.sema.generator_tmpl);
    gen = try sema.semaEnsureAddressable(c, gen, ctx.args[0]);

    const ptr_t = try sema.getPtrType(c, gen.type);
    const addr = try sema.semaAddressOf(c, gen, ptr_t, ctx.args[0]);

    const end = try c.ir.newExpr(.gen_end, c.sema.void_t, ctx.node, .{
        .gen = addr.ir,
    });
    out.* = sema.ExprResult.init2(end);
}

pub fn Generator_deinit(t: *cy.Thread) !C.Ret {
    _ = t.ret(void);
    const gen = t.param(*vmc.Generator);
    std.debug.assert(getGeneratorStatus(t, gen) == .done);

    const frame = @as([*]Value, @ptrCast(gen.frame_ptr))[0..gen.frame_len];
    t.heap.alloc.free(frame);

    return C.RetOk;
}

const GeneratorStatus = enum {
    paused,
    running,
    done,
};

fn Generator_status(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(i64);
    const gen = t.param(*vmc.Generator);
    const status = getGeneratorStatus(t, gen);
    ret.* = @intFromEnum(status);
    return C.RetOk;
}

fn Generator_size(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(i64);
    ret.* = @intCast(@sizeOf(vmc.Generator));
    return C.RetOk;
}

fn getGeneratorStatus(t: *cy.Thread, gen: *vmc.Generator) GeneratorStatus {
    _ = t;
    if (gen.done) {
        return .done;
    }
    if (gen.running) {
        return .running;
    }
    return .paused;
}

pub fn ptr_init(c: *cy.Chunk, ctx: *cy.BuiltinContext, out: *sema.ExprResult) !void {
    // const elem_t = ctx.func.parent.parent.?.cast(.type).type.cast(.pointer).child_t;
    const ptr = try c.semaExpr(ctx.args[0], .{});
    var right = try c.semaExpr(ctx.args[1], .{});
    right = try sema.semaOwn(c, right, right.ir.node);

    _ = try c.ir.pushStmt(.set_deref, ctx.node, .{
        .ptr = ptr.ir,
        .right = right.ir,
    });
    out.* = sema.ExprResult.initVoid(c.sema.void_t);
}

pub fn Ptr_add_eval(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    _ = c;
    const child_t = ctx.func.sig.params_ptr[0].get_type().cast(.pointer).child_t;
    const addr: usize = @intFromPtr(ctx.args[0].asPtr(?*anyopaque));
    const right = @as(usize, @intCast(ctx.args[1].asInt())) * child_t.size();
    return cy.TypeValue.init(ctx.func.sig.ret, Value.initPtr(@ptrFromInt(addr + right)));
}

fn Func_size(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(i64);
    const func = t.getPtr(*cy.heap.Func, 0);
    if (func.kind == .closure) {
        ret.* = @intCast(24 + func.data.closure.numCaptured * 8);
        return C.RetOk;
    } else {
        ret.* = 24;
        return C.RetOk;
    }
}

fn f32_fmt(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Str);
    const val = t.param(f32);
    const BufSize = std.fmt.float.bufferSize(.decimal, f32);
    var buf: [BufSize]u8 = undefined;
    const str = try std.fmt.float.render(&buf, val, .{ .mode = .decimal });
    ret.* = try t.heap.init_astr(str);
    return C.RetOk;
}

fn f64_fmt(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Str);
    const val = t.param(f64);
    const BufSize = std.fmt.float.bufferSize(.decimal, f64);
    var buf: [BufSize]u8 = undefined;
    const str = try std.fmt.float.render(&buf, val, .{ .mode = .decimal });
    ret.* = try t.heap.init_astr(str);
    return C.RetOk;
}

fn atof(t: *cy.Thread) !C.Ret {
    const ret = t.ret(f64);
    const str = t.param(cy.heap.Str).slice();
    const res = std.fmt.parseFloat(f64, str) catch {
        ret.* = 0;
        return C.RetOk;
    };
    ret.* = res;
    return C.RetOk;
}

fn atof32(t: *cy.Thread) !C.Ret {
    const ret = t.ret(f32);
    const str = t.param(cy.heap.Str).slice();
    const res = std.fmt.parseFloat(f32, str) catch {
        ret.* = 0;
        return C.RetOk;
    };
    ret.* = res;
    return C.RetOk;
}

pub const NumberFormat = enum {
    asc,
    bin,
    dec,
    hex,
    oct,
};

fn raw_fmt(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Str);
    const val = t.param(u64);
    const format = try std.meta.intToEnum(NumberFormat, t.param(i64));
    const config = t.param(cy.heap.NumberFormatConfig);
    ret.* = try rawFmtExt(t, val, format, config);
    return C.RetOk;
}

pub fn int_fmt_eval(c: *cy.Chunk, ctx: *cy.ConstEvalContext) !cy.TypeValue {
    const val = ctx.args[0].asInt();
    const format = try std.meta.intToEnum(NumberFormat, ctx.args[1].asInt());
    const res = try intFmtExt(c.heap, val, format, .{});
    const box = try c.heap.lift(bt.Str, res);
    return TypeValue.init(c.sema.str_t, box);
}

fn int_fmt(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Str);
    const val = t.param(i64);
    const format = try std.meta.intToEnum(NumberFormat, t.param(i64));
    ret.* = try intFmtExt(&t.heap, val, format, .{});
    return C.RetOk;
}

fn int_fmt2(t: *cy.Thread) anyerror!Value {
    const val = t.getInt(0);
    const format = try std.meta.intToEnum(NumberFormat, t.getInt(1));

    const config = t.getPtr(*cy.heap.NumberFormatConfig, 2);
    return intFmtExt(t, val, format, config);
}

pub fn intFmtExt(heap: *cy.Heap, val: i64, format: NumberFormat, opts: cy.heap.NumberFormatConfig) !cy.heap.Str {
    if (format == .asc) {
        if (val < 0 or val > 127) {
            return error.InvalidArgument;
        }
        const uchar: u8 = @intCast(val);
        return heap.init_astr(&.{uchar});
    } else {
        const base: u8 = switch (format) {
            .bin => 2,
            .oct => 8,
            .dec => 10,
            .hex => 16,
            else => return error.InvalidArgument,
        };
        var buf: [64]u8 = undefined;
        var end: usize = undefined;
        const fill: u8 = opts.pad.from() orelse ' ';
        const width: ?usize = if (opts.width.from()) |width| @intCast(width) else null;
        if (format == .dec and val < 0) {
            end = std.fmt.printInt(&buf, val, base, .lower, .{ .fill = fill, .width = width });
        } else {
            // Format as u64 to prevent emitting the `+` character.
            end = std.fmt.printInt(&buf, @as(u64, @bitCast(val)), base, .lower, .{ .fill = fill, .width = width });
        }
        return heap.init_astr(buf[0..end]);
    }
}

pub fn rawFmtExt(t: *cy.Thread, val: u64, format: NumberFormat, opts: cy.heap.NumberFormatConfig) !cy.heap.Str {
    if (format == .asc) {
        if (val < 0 or val > 127) {
            return error.InvalidArgument;
        }
        const uchar: u8 = @intCast(val);
        return t.heap.init_astr(&.{uchar});
    } else {
        const base: u8 = switch (format) {
            .bin => 2,
            .oct => 8,
            .dec => 10,
            .hex => 16,
            else => return error.InvalidArgument,
        };
        var buf: [64]u8 = undefined;
        const fill: u8 = opts.pad.from() orelse ' ';
        const width: ?usize = if (opts.width.from()) |width| @intCast(width) else null;
        const end = std.fmt.printInt(&buf, val, base, .lower, .{ .fill = fill, .width = width });
        return t.heap.init_astr(buf[0..end]);
    }
}

pub fn getDeinitObject(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(u64);
    const obj = t.param(*cy.HeapObject);
    const type_ = t.c.vm.sema.getType(obj.getTypeId());
    const func_ = type_.deinit_obj orelse {
        return t.ret_panic("Missing deinitObject.");
    };
    const pc = t.c.vm.compiler.genSymMap.get(func_).?.func.static_pc;
    ret.* = @intFromPtr(pc);
    return C.RetOk;
}

pub fn getTypeDtor(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(?*anyopaque);
    const type_id: cy.TypeId = @intCast(t.param(i64));
    const type_ = t.c.vm.sema.getType(type_id);
    const dtor = type_.dtor orelse {
        ret.* = null;
        return C.RetOk;
    };
    const pc = t.c.vm.compiler.genSymMap.get(dtor).?.func.static_pc;
    ret.* = pc;
    return C.RetOk;
}

const Object = extern struct {
    inner: *cy.HeapObject,
};

pub fn Object_type(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(i64);
    const obj = t.param(*Object);
    ret.* = @intCast(obj.inner.getTypeId());
    return C.RetOk;
}

pub fn retain(t: *cy.Thread) anyerror!C.Ret {
    _ = t.ret(void);
    const obj = t.param(*cy.HeapObject);
    t.heap.retainObject(obj);
    return C.RetOk;
}

pub fn releaseOnly(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(bool);
    const obj = t.param(*cy.HeapObject);
    const free_ = t.heap.releaseOnly(obj);
    ret.* = free_;
    return C.RetOk;
}

pub fn f32_to_f64(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f64);
    ret.* = @floatCast(t.param(f32));
    return C.RetOk;
}

pub fn f64_to_f32(t: *cy.Thread) callconv(.c) C.Ret {
    const ret = t.ret(f32);
    ret.* = @floatCast(t.param(f64));
    return C.RetOk;
}
