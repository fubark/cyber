const std = @import("std");
const builtin = @import("builtin");
const cy = @import("../cyber.zig");
const sema = cy.sema;
const tcc = @import("tcc");
const C = @import("../capi.zig");
const core = @import("../builtins/core.zig");
const os = @import("../std/os.zig");
const bc = @import("../bc_gen.zig");
const os_mod = @import("../std/os.zig");

pub const Src = @embedFile("c.cy");

const types = [_]struct { []const u8, C.BindType }{
    .{ "u32", C.TYPE_CREATE(createU32Type) },
    .{ "variadic", C.TYPE_CREATE(createVariadicType) },
};

comptime {
    @export(&bind, .{ .name = "cl_mod_bind_c", .linkage = .strong });
}

pub fn bind(_: *C.VM, mod: *C.Sym) callconv(.c) void {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }

    for (types) |e| {
        C.mod_add_type(mod, e.@"0", e.@"1");
    }
}

const funcs = [_]struct { []const u8, C.BindFunc }{
    .{ "@initBindLib", core.zErrFunc(initBindLib) },
    .{ "include", core.zErrCtFunc(null, import) },
    .{ "flag", core.zErrCtFunc(null, flag) },
    .{ "bind_lib", core.zErrCtFunc(null, bind_lib) },
    .{ "from_strz", core.zErrFunc(from_strz) },
    .{ "to_strz", core.zErrFunc(to_strz) },
};

/// Call `dlopen` for each chunk that needs it and assume `dlopen` caches duplicate libraries.
fn getDynLib(vm: *cy.VM, c: *cy.Chunk) !*std.DynLib {
    const lib = try vm.alloc.create(std.DynLib);
    errdefer vm.alloc.destroy(lib);
    if (c.bind_lib) |path| {
        var final_path: C.Bytes = undefined;
        if (!vm.compiler.dl_resolver.?(@ptrCast(vm), C.to_bytes(path), &final_path)) {
            @panic("Failed to load dynamic library.");
        }
        defer C.vm_freeb(@ptrCast(vm), C.from_bytes(final_path));
        lib.* = os.dlopen(C.from_bytes(final_path)) catch |err| {
            std.debug.print("{s}\nCannot dlopen {s}.\n", .{ c.srcUri, path });
            return err;
        };
    } else {
        if (builtin.os.tag == .macos) {
            const exe = try std.fs.selfExePathAlloc(vm.alloc);
            defer vm.alloc.free(exe);
            lib.* = os.dlopen(exe) catch |err| {
                std.debug.print("Cannot dlopen {s}.\n", .{exe});
                return err;
            };
        } else if (builtin.os.tag == .windows) {
            // Windows: Load C runtime library for standard C functions
            // Try Universal C Runtime first (Windows 10+)
            if (os.dlopen("ucrtbase.dll")) |ucrt_lib| {
                lib.* = ucrt_lib;
            } else |_| {
                // Fallback to older C runtime (Windows 7/8)
                lib.* = os.dlopen("msvcrt.dll") catch |err| {
                    std.debug.print("Cannot load Windows C runtime.\n", .{});
                    std.debug.print("Tried: ucrtbase.dll, msvcrt.dll\n", .{});
                    return err;
                };
            }
        } else {
            lib.* = os.dlopen("") catch |err| {
                std.debug.print("Cannot dlopen main.\n", .{});
                return err;
            };
        }
    }
    try vm.dyn_libs.append(vm.alloc, lib);
    return lib;
}

fn getTccState(vm: *cy.VM) *tcc.TCCState {
    return vm.tcc_state orelse {
        const state = tcc.tcc_new();
        // Don't include libtcc1.a.
        _ = tcc.tcc_set_options(state, "-nostdlib");
        _ = tcc.tcc_set_output_type(state, tcc.TCC_OUTPUT_MEMORY);
        vm.tcc_state = state;
        return @ptrCast(state);
    };
}

fn genFunc(c: *cy.cgen.Chunk, lib: *std.DynLib, state: *tcc.TCCState, w: anytype, func: *cy.Func) !void {
    if (func.type == .extern_) {
        try cy.cgen.genFuncForwardDecl(c, w, func);
        if (func.data.extern_.has_impl) {
            if (func.data.extern_.vm_variadic) {
                return error.TODO;
            }
            try cy.cgen.genExternToVmFunc(c, w, func, @ptrCast(func.decl));
        } else {
            const extern_name = func.data.extern_.externName();
            const extern_namez = try c.alloc.dupeZ(u8, extern_name);
            defer c.alloc.free(extern_namez);
            const ptr = lib.lookup(*anyopaque, extern_namez) orelse {
                std.debug.print("Missing symbol `{s}`.", .{extern_name});
                return error.MissingSymbol;
            };
            _ = tcc.tcc_add_symbol(state, extern_namez, ptr);

            if (!func.data.extern_.vm_variadic) {
                try cy.cgen.genVmToExternFunc(c, w, func, func, @ptrCast(func.decl));
            }
        }
    } else if (func.type == .vm_extern_variant) {
        try cy.cgen.genFuncForwardDecl(c, w, func.data.vm_extern_variant.extern_func);
        try cy.cgen.genVmToExternFunc(c, w, func, func.data.vm_extern_variant.extern_func, @ptrCast(func.decl));
    } else {
        return;
    }
}

fn bindExternFuncPtrDispatch(c: *cy.cgen.Compiler, state: *tcc.TCCState, func_ptr: *cy.types.FuncPtr) !void {
    var buf: [32]u8 = undefined;
    const cname = try std.fmt.bufPrintZ(&buf, "cb_func_ptr{}", .{func_ptr.sig.id});
    const ptr = tcc.tcc_get_symbol(state, cname.ptr) orelse {
        cy.panic("Failed to get symbol.");
    };
    const rt_id = try bc.ensureExternFuncPtrDispatch(c.base, func_ptr);
    const rt_func = cy.vm.FuncSymbol.initHostFunc(@ptrCast(@alignCast(ptr)));
    c.base.vm.funcSyms.items[rt_id] = rt_func;
}

fn bindFunc(c: *cy.cgen.Compiler, state: *tcc.TCCState, func: *cy.Func) !void {
    if (func.type == .extern_ and !func.data.extern_.vm_variadic) {
        const cname = try c.alloc.dupeZ(u8, func.data.extern_.externName());
        defer c.alloc.free(cname);
        const func_ptr = tcc.tcc_get_symbol(state, cname.ptr) orelse {
            cy.panic("Failed to get symbol.");
        };

        const entry = try bc.ensureExternFunc(c.base, func);
        // Bind extern ptr.
        const rt_func = cy.vm.FuncSymbol.initHostFunc(@ptrCast(@alignCast(func_ptr)));
        bc.completeFunc(c.base, entry.id, func, rt_func);
        try c.base.vm.host_funcs.put(c.alloc, @ptrCast(@alignCast(func_ptr)), entry.id);

        if (!func.data.extern_.has_impl) {
            const vm_func_name = try std.fmt.allocPrintSentinel(c.alloc, "CB_{s}", .{func.data.extern_.externName()}, 0);
            defer c.alloc.free(vm_func_name);
            const vm_func_ptr = tcc.tcc_get_symbol(state, vm_func_name.ptr) orelse {
                cy.panic("Failed to get symbol.");
            };

            // Bind VM host to extern call.
            const vm_func = cy.vm.FuncSymbol.initHostFunc(@ptrCast(@alignCast(vm_func_ptr)));
            bc.completeFunc(c.base, entry.vm_id, func, vm_func);
            try c.base.vm.host_funcs.put(c.alloc, @ptrCast(@alignCast(vm_func_ptr)), entry.vm_id);
        }
    } else if (func.type == .vm_extern_variant) {
        const csym = c.syms.get(func).?;
        const vm_func_name = try std.fmt.allocPrintSentinel(c.alloc, "{s}", .{csym.name()}, 0);
        defer c.alloc.free(vm_func_name);

        const func_ptr = tcc.tcc_get_symbol(state, vm_func_name.ptr) orelse {
            cy.panic("Failed to get symbol.");
        };

        const id = try bc.ensureFunc(c.base, func);
        const rt_func = cy.vm.FuncSymbol.initHostFunc(@ptrCast(@alignCast(func_ptr)));
        bc.completeFunc(c.base, id, func, rt_func);
        try c.base.vm.host_funcs.put(c.alloc, @ptrCast(@alignCast(func_ptr)), id);
    } else {
        return;
    }
}

/// First compile C bindings and link with dll symbols.
/// Second iteration, gets the generated symbols and binds them to the VM symbols.
pub fn initBindLib(t: *cy.Thread) !C.Ret {
    if (!cy.hasFFI) {
        return C.RetOk;
    }
    var compiler = cy.cgen.Compiler{
        .base = t.c.vm.compiler,
        .alloc = t.alloc,
        .syms = .{},
        .c_names = .{},
        .vtables = .{},
        .next_vtable_id = 0,
    };
    defer compiler.deinit();

    // First register the types and functions that will be needed for the C compiler.
    try compiler.createPredefinedSym(compiler.base.sema.bool_t.sym(), "CB_bool");
    try compiler.createPredefinedSym(compiler.base.sema.i32_t.sym(), "CB_i32");
    try compiler.createPredefinedSym(compiler.base.sema.i16_t.sym(), "CB_i16");
    try compiler.createPredefinedSym(compiler.base.sema.i8_t.sym(), "CB_i8");
    try compiler.createPredefinedSym(compiler.base.sema.r8_t.sym(), "CB_byte");
    try compiler.createPredefinedSym(compiler.base.sema.r32_t.sym(), "CB_r32");
    try compiler.createPredefinedSym(compiler.base.sema.r64_t.sym(), "CB_r64");
    try compiler.createPredefinedSym((try t.c.vm.findType("c.variadic")).?.sym(), "...");
    try compiler.createPredefinedSym(compiler.base.sema.void_t.sym(), "CB_void");
    for (t.c.vm.compiler.chunks.items) |c| {
        if (!c.has_extern_func) {
            continue;
        }

        // Platform-specific binding check:
        // - has_bind_lib=false, bind_lib=null: Never called -> PROCESS (fallback)
        // - has_bind_lib=true, bind_lib=null: Called bind_lib(none) -> SKIP
        if (c.has_bind_lib and c.bind_lib == null) {
            continue;
        }

        for (c.funcs.items) |func| {
            if (func.type == .extern_) {
                const extern_name = func.data.extern_.externName();
                try compiler.createSymExact(func, extern_name, false);
            } else if (func.type == .vm_extern_variant) {
                try compiler.createSym(func, func.name());
            } else {
                continue;
            }
            const params = func.sig.params();
            for (params) |param| {
                try compiler.ensureSym(&param.get_type().sym().head);
            }
            try compiler.ensureSym(&func.sig.ret.sym().head);
        }
    }

    // Generate c code.
    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(t.alloc);
    const w = csrc.writer(t.alloc);

    try cy.cgen.genVmHeaders(w);

    // const builtins_c = vm.compiler.chunk_map.get("core").?;
    // const core_sym: *cy.Sym = @ptrCast(builtins_c.sym);
    // _ = core_sym;
    // try compiler.mapType(builtins_c, "str", "CB_str");
    // try compiler.mapType(builtins_c, "StrBuffer", "CB_StrBuffer");
    // try compiler.mapType(builtins_c, "int", "CB_int");
    // try compiler.mapType(builtins_c, "bool", "CB_bool");
    // try compiler.mapType(builtins_c, "error", "CB_error");
    // try compiler.mapType(builtins_c, "symbol", "CB_symbol");
    // try compiler.mapType(builtins_c, "float", "CB_float");
    // try compiler.mapType(builtins_c, "Object", "CB_Object");
    // compiler.markDeclared(builtins_c, "Object");
    // try compiler.mapType(builtins_c, "NumberFormat", "CB_NumberFormat");
    // try compiler.mapType(builtins_c, "MetaType", "CB_MetaType");
    // try compiler.mapFunc2(core_sym, "free", "cb_free");
    // try compiler.mapFunc2(core_sym, "alloc_", "cb_alloc");
    // try compiler.mapFunc(core_sym, "eprint", "cb_eprint");
    // try compiler.mapFunc2(core_sym, "print", "cb_print");
    // try compiler.mapFunc2(core_sym, "utf8Check", "cb_utf8_check");
    // try compiler.mapFunc2(core_sym, "utf8Decode", "cb_utf8_decode");
    // try compiler.mapFunc2(core_sym, "utf8SeqLen", "cb_utf8_seq_len");
    // try compiler.mapFunc2(core_sym, "$newObjectUndef", "cb_object_undef");
    // try compiler.mapFunc2(core_sym, "$getDeinitObject", "cb_get_deinit_object");
    // try compiler.mapFunc(core_sym, "$newAstrUndef", "cb_astr_undef");
    // try compiler.mapFunc(core_sym, "$newUstrUndef", "cb_ustr_undef");
    // try compiler.mapTypeFuncVariants(self.sema.func_tmpl, "$size", "cb_func_union_size");
    // if (self.chunk_map.get("test")) |chunk| {
    //     _ = chunk;
    //     // try compiler.mapFunc(@ptrCast(chunk.sym), "eq", "cb_test_eq");
    // }
    // try compiler.mapFunc2(core_sym, "$freeObject", "cb_free_object");
    // try compiler.mapFunc2(core_sym, "$releaseOnly", "cb_release_only");
    // try compiler.mapFunc(core_sym, "panic", "cb_panic");
    // try compiler.mapFunc(core_sym, "$memcpy", "cb_memcpy");

    // try cgen.genStructDecls(w);
    // try cgen.genArrayConvDecls(w);

    // try cgen.genStructConvs(w);
    // try cgen.genArrayConvs(vm, w);

    // Begin func binding generation.

    const state = getTccState(t.c.vm);

    for (t.c.vm.compiler.chunks.items) |c| {
        if (!c.has_extern_func) {
            continue;
        }

        // Platform-specific binding check:
        // - has_bind_lib=false, bind_lib=null: Never called -> PROCESS (fallback)
        // - has_bind_lib=true, bind_lib=null: Called bind_lib(none) -> SKIP
        if (c.has_bind_lib and c.bind_lib == null) {
            continue;
        }

        const lib = try getDynLib(t.c.vm, c);

        var chunk_gen = cy.cgen.Chunk.init(&compiler, c);

        for (c.syms.items) |sym| {
            if (sym.type == .extern_var) {
                const extern_var = sym.cast(.extern_var);
                const extern_namez = try t.alloc.dupeZ(u8, extern_var.externName());
                defer t.alloc.free(extern_namez);
                const ptr = lib.lookup(*anyopaque, extern_namez) orelse {
                    std.debug.print("Missing symbol `{s}`.", .{extern_var.externName()});
                    return error.MissingSymbol;
                };

                const rt_id = c.compiler.genSymMap.get(sym).?.varSym.id;
                t.c.vm.globals.items[rt_id].value.ptr = @ptrCast(@alignCast(ptr));
            }
        }

        for (c.funcs.items) |func| {
            try genFunc(&chunk_gen, lib, state, w, func);
        }
    }
    var chunk_gen = cy.cgen.Chunk.init(&compiler, compiler.base.main_chunk);
    for (compiler.base.sema.types.items) |type_| {
        if (type_.kind() == .func_ptr) {
            const func_ptr = type_.cast(.func_ptr);
            if (func_ptr.extern_is_called) {
                try cy.cgen.gen_extern_func_ptr_wrapper(&chunk_gen, w, func_ptr);
            }
        }
    }

    try w.writeByte(0);
    if (cy.cgen.DumpCGen) {
        try cy.cgen.dumpSource(csrc.items);
    }

    if (tcc.tcc_compile_string(state, csrc.items.ptr) == -1) {
        cy.panic("Failed to compile c source.");
    }

    // const __floatundisf = @extern(*anyopaque, .{ .name = "__floatundisf", .linkage = .Strong });
    if (builtin.cpu.arch != .aarch64) {
        _ = tcc.tcc_add_symbol(state, "__fixunsdfdi", __fixunsdfdi);
        _ = tcc.tcc_add_symbol(state, "__floatundidf", __floatundidf);
    }
    // _ = tcc.tcc_add_symbol(state, "__floatundisf", __floatundisf);
    // _ = tcc.tcc_add_symbol(state, "printf", std.c.printf);
    // _ = tcc.tcc_add_symbol(state, "exit", std.c.exit);
    // _ = tcc.tcc_add_symbol(state, "breakpoint", breakpoint);
    // _ = tcc.tcc_add_symbol(state, "icyAllocObject", cAllocObject);
    _ = tcc.tcc_add_symbol(state, "host_new_obj_init", cy.cgen.host_new_obj_init);
    _ = tcc.tcc_add_symbol(state, "host_call_func", cy.cgen.host_call_func);
    _ = tcc.tcc_add_symbol(state, "host_ensure_thread", cy.cgen.host_ensure_thread);
    // _ = tcc.tcc_add_symbol(state, "printValue", cPrintValue);
    if (builtin.cpu.arch == .aarch64) {
        _ = tcc.tcc_add_symbol(state, "memmove", cy.cgen.memmove);
    }

    if (tcc.tcc_relocate(state, tcc.TCC_RELOCATE_AUTO) < 0) {
        cy.panic("Failed to relocate compiled code.");
    }

    for (t.c.vm.compiler.chunks.items) |c| {
        if (!c.has_extern_func) {
            continue;
        }

        // Platform-specific binding check:
        // - has_bind_lib=false, bind_lib=null: Never called -> PROCESS (fallback)
        // - has_bind_lib=true, bind_lib=null: Called bind_lib(none) -> SKIP
        if (c.has_bind_lib and c.bind_lib == null) {
            continue;
        }

        // Bind VM function pointers.
        for (c.funcs.items) |func| {
            try bindFunc(&compiler, state, func);
        }
    }

    for (compiler.base.sema.types.items) |type_| {
        if (type_.kind() == .func_ptr) {
            const func_ptr = type_.cast(.func_ptr);
            if (func_ptr.extern_is_called) {
                try bindExternFuncPtrDispatch(&compiler, state, func_ptr);
            }
        }
    }

    // Perform runtime relocations.
    for (t.c.vm.compiler.chunks.items) |c| {
        for (c.rt_relocations.items) |rel| {
            const ptr: *align(1) [6]u8 = @ptrCast(&c.buf.ops.items[rel.pc]);
            switch (rel.kind) {
                .extern_func_ptr => {
                    const rt_id = c.compiler.genSymMap.get(rel.data.extern_func_ptr).?.func.id;
                    const func_ptr = c.vm.funcSyms.items[rt_id].data.host_func;
                    const addr: u48 = @intCast(@intFromPtr(func_ptr));
                    std.mem.writeInt(u48, ptr, addr, .little);
                },
                .func => {
                    if (rel.data.func.type == .extern_) {
                        if (rel.data.func.data.extern_.has_impl) {
                            const id = c.compiler.genSymMap.get(rel.data.func).?.extern_func.id;
                            const func_ptr = c.vm.funcSyms.items[id].data.host_func;
                            const addr: u48 = @intCast(@intFromPtr(func_ptr));
                            std.mem.writeInt(u48, ptr, addr, .little);
                        } else {
                            const id = c.compiler.genSymMap.get(rel.data.func).?.extern_func.vm_id;
                            const func_ptr = c.vm.funcSyms.items[id].data.host_func;
                            const addr: u48 = @intCast(@intFromPtr(func_ptr));
                            std.mem.writeInt(u48, ptr, addr, .little);
                        }
                    } else {
                        const id = c.compiler.genSymMap.get(rel.data.func).?.func.id;
                        const func_ptr = c.vm.funcSyms.items[id].data.host_func;
                        const addr: u48 = @intCast(@intFromPtr(func_ptr));
                        std.mem.writeInt(u48, ptr, addr, .little);
                    }
                },
                .global => {
                    const addr: u48 = @intCast(@intFromPtr(c.vm.globals.items[rel.data.global].value.ptr));
                    std.mem.writeInt(u48, ptr, addr, .little);
                },
            }
        }
    }

    return C.RetOk;
}

fn createVariadicType(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createType(.c_variadic, .{}) catch @panic("error");
    return @ptrCast(new_t);
}

fn createU32Type(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createType(.int, .{ .bits = 32 }) catch @panic("error");
    return @ptrCast(new_t);
}

pub fn from_strz(t: *cy.Thread) !C.Ret {
    if (cy.isWasm) return t.ret_panic("Unsupported.");
    const ret = t.ret(cy.heap.Str);
    const ptr = t.param([*]const u8);
    const bytes = std.mem.span(@as([*:0]const u8, @ptrCast(ptr)));
    ret.* = try t.heap.init_str(bytes);
    return C.RetOk;
}

fn to_strz(t: *cy.Thread) !C.Ret {
    if (cy.isWasm) return t.ret_panic("Unsupported.");
    const ret = t.ret([*]u8);
    const str = t.param(cy.heap.Str);
    defer t.heap.destructStr(&str);
    const slice = str.slice();
    const new: [*]u8 = @ptrCast(std.c.malloc(slice.len + 1));
    @memcpy(new[0..slice.len], slice);
    new[slice.len] = 0;
    ret.* = new;
    return C.RetOk;
}

pub fn bind_lib(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const path = ctx.args[0].asPtr(*cy.HeapObject);
    defer c.heap.release(ctx.args[0]);
    c.has_bind_lib = true;
    if (c.bind_lib) |dl_bind| {
        c.alloc.free(dl_bind);
    }
    c.bind_lib = null;
    if (path.object.firstValue.asInt() == 1) {
        const path_str: *cy.heap.Str = @ptrCast(&path.object.getValuesPtr()[1]);
        const new_dl_bind = try c.alloc.dupe(u8, path_str.slice());
        c.bind_lib = new_dl_bind;
    }
    return cy.TypeValue.init(c.sema.void_t, cy.Value.Void);
}

pub fn import(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const spec = ctx.args[0].asString();
    defer c.heap.release(ctx.args[0]);
    const dupe = try c.alloc.dupe(u8, spec);
    try c.compiler.c_includes.append(c.alloc, dupe);
    return cy.TypeValue.init(c.sema.void_t, cy.Value.Void);
}

pub fn flag(c: *cy.Chunk, ctx: *cy.CtFuncContext) !cy.TypeValue {
    const str = ctx.args[0].asString();
    defer c.heap.release(ctx.args[0]);
    const dupe = try c.alloc.dupe(u8, str);
    try c.compiler.c_flags.append(c.alloc, dupe);
    return cy.TypeValue.init(c.sema.void_t, cy.Value.Void);
}

pub extern fn __floatundidf(u64) f64;
pub extern fn __fixunsdfdi(f64) u64;
