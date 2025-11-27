const std = @import("std");
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const bt = cy.types.BuiltinTypes;
const build_options = @import("build_options");
const rt = cy.rt;
const log = cy.log.scoped(.cy);
const ast = cy.ast;

pub const Src = @embedFile("cy.cy");
const zErrFunc = cy.core.zErrFunc;

fn create_vm_type(vm: ?*C.VM, c_mod: ?*C.Sym, decl: ?*C.Node) callconv(.c) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createType(.pointer, .{ .ref = false, .child_t = c.sema.void_t }) catch @panic("error");
    return @ptrCast(new_t);
}

comptime {
    @export(&bind, .{ .name = "cl_mod_bind_cy", .linkage = .strong });
}

pub fn bind(_: *C.VM, mod: *C.Sym) callconv(.c) void {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }

    for (types) |e| {
        C.mod_add_type(mod, e.@"0", e.@"1");
    }
}

const types = [_]struct{[]const u8, C.BindType}{
    // .{"VM",         C.TYPE_CREATE(create_vm_type)},
};

const funcs = [_]struct{[]const u8, C.BindFunc}{
    .{"parse_",             zErrFunc(parse)},
    .{"_parser_comments",   zErrFunc(_parser_comments)},
    .{"_new_parser",        zErrFunc(_new_parser)},
    .{"destroy_parser",     zErrFunc(destroy_parser)},

    .{"Value.object_type",  zErrFunc(UserValue_object_type)}, 

    .{"VM.@init",           zErrFunc(UserVM_init)},
    .{"VM.@deinit",         zErrFunc(UserVM_deinit)},
    .{"VM.eval",            zErrFunc(UserVM_eval)},
    .{"VM.compile_error_summary", zErrFunc(UserVM_compile_error_summary)},
    .{"VM.panic_summary",   zErrFunc(UserVM_panic_summary)},
    .{"VM.value_desc",      zErrFunc(UserVM_value_desc)}, 
    .{"VM.main_thread",     zErrFunc(UserVM_main_thread)}, 
    .{"Thread.deinit_str",  zErrFunc(UserThread_deinit_str)}, 
};

const UserThread = struct {
    t: *C.Thread,
};

pub fn UserThread_deinit_str(t: *cy.Thread) anyerror!C.Ret {
    _ = t.ret(void);
    const ut = t.param(*UserThread).t;
    const str: *C.str = @ptrCast(t.param(UserValue).val);
    C.str_deinit(ut, str);
    return C.RetOk;
}

const UserVM = struct {
    vm: *cy.VM,
};

pub fn UserVM_init(t: *cy.Thread) anyerror!C.Ret {
    // Create an isolated VM.
    const new = C.vm_init();

    // Use the same printers as the parent VM.
    C.vm_set_printer(new, C.vm_printer(@ptrCast(t.c.vm)));
    C.vm_set_eprinter(new, C.vm_eprinter(@ptrCast(t.c.vm)));

    const ret = t.ret(UserVM);
    ret.* = .{ .vm = @ptrCast(@alignCast(new)) };
    return C.RetOk;
}

pub fn UserVM_compile_error_summary(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Str);

    const uvm = t.param(*UserVM);
    const cvm: *C.VM = @ptrCast(uvm.vm);
    const summary = C.vm_compile_error_summary(cvm);
    defer C.vm_freeb(cvm, summary);
    ret.* = try t.heap.init_str(summary);
    return C.RetOk;
}

pub fn UserVM_panic_summary(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Str);
    const uvm = t.param(*UserVM);
    const summary = C.thread_panic_summary(@ptrCast(uvm.vm.main_thread));
    defer C.vm_freeb(@ptrCast(uvm.vm), summary);
    ret.* = try t.heap.init_str(summary);
    return C.RetOk;
}

pub fn UserVM_main_thread(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(UserThread);
    const uvm: *C.VM = @ptrCast(t.param(*UserVM).vm);
    const ut = C.vm_main_thread(uvm);
    ret.* = .{ .t = @ptrCast(ut) };
    return C.RetOk;
}

pub fn UserVM_value_desc(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Str);
    const uvm = t.param(*UserVM);
    const cvm: *C.VM = @ptrCast(uvm.vm);
    const val_t: cy.TypeId = @intCast(t.param(i64));
    const uval = t.param(UserValue);

    const str = C.value_desc(cvm, val_t, @ptrCast(uval.val));
    defer C.vm_freeb(cvm, str);
    ret.* = try t.heap.init_str(str);
    return C.RetOk;
}

pub fn UserValue_object_type(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(i64);
    const uval = t.param(*UserValue);
    ret.* = @intCast(uval.val.asHeapObject().getTypeId());
    return C.RetOk;
}

const UserEvalConfig = extern struct {
    single_run: bool,
    gen_all_debug_syms: bool, 
    backend: u64,
    spawn_exe: bool,
    persist_main: bool,
};

pub fn UserVM_eval(t: *cy.Thread) !C.Ret {
    const ret = t.ret(EvalResult);

    const uvm = t.param(*UserVM);
    const uri = t.param(cy.heap.Str).slice();
    const src = t.param(cy.heap.Str).slice();
    const config = t.param(UserEvalConfig);

    const config_c = C.EvalConfig{
        .single_run = config.single_run,
        .gen_all_debug_syms = config.gen_all_debug_syms,
        .backend = @intCast(config.backend),
        .spawn_exe = config.spawn_exe,
        .persist_main = config.persist_main,
    };

    var res: C.EvalResult = undefined;
    const zvm: *C.VM = @ptrCast(uvm.vm);
    const code = C.vm_evalx(zvm, uri, src, config_c, &res);

    if (code == C.Success) {
        ret.* = .{
            .code = cy.Value.initInt(@intCast(code)),
            .val_t = res.res_t,
            .value = .{
                .val = @ptrCast(res.res),
            },
        };
    } else {
        ret.* = .{
            .code = cy.Value.initInt(@intCast(code)),
            .val_t = res.res_t,
            .value = .{
                .val = undefined,
            },
        };
    }
    return C.RetOk;
}

pub fn UserVM_deinit(t: *cy.Thread) !C.Ret {
    _ = t.ret(void);
    const uvm = t.param(*UserVM);
    // if (fatal) {
    //     _ = try cy.thread.freeHeapPages(uvm.vm);
    // }
    C.vm_deinit(@ptrCast(uvm.vm));
    return C.RetOk;
}

const UserValue = extern struct {
    val: *cy.Value, 
};

const EvalResult = extern struct {
    code: cy.Value,
    val_t: u64,
    value: UserValue,
};

pub fn _new_parser(t: *cy.Thread) anyerror!C.Ret {
    var parser = try t.alloc.create(cy.Parser);
    try parser.init(t.alloc);

    const ret = t.ret(*cy.Parser);
    ret.* = parser;
    return C.RetOk;
}

pub fn destroy_parser(t: *cy.Thread) anyerror!C.Ret {
    _ = t.ret(void);
    const parser = t.param(*cy.Parser);
    parser.deinit();
    t.alloc.destroy(parser);
    return C.RetOk;
}

const Comment = extern struct {
    pos: i64,
    end: i64,
};

pub fn _parser_comments(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(cy.heap.Slice);
    const parser = t.param(*cy.Parser);

    const buffer_t: cy.TypeId = @intCast(t.param(i64));

    var args: std.ArrayListUnmanaged(Comment) = .{};
    defer args.deinit(t.alloc);

    for (parser.ast.comments.items) |comment| {
        try args.append(t.alloc, .{ .pos = comment.start, .end = comment.end });
    }

    const slice = try t.heap.init_slice_undef(buffer_t, args.items.len, @sizeOf(Comment));
    @memcpy(slice.items(Comment), args.items);
    ret.* = slice;
    return C.RetOk;
}

fn parserReportFn(p: *cy.Parser, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror {
    const msg = try cy.fmt.allocFormat(p.alloc, format, args);
    defer p.alloc.free(msg);

    std.debug.print("{s}\n", .{msg});

    var wa = std.Io.Writer.Allocating.init(p.alloc);
    defer wa.deinit();
    try cy.debug.write_user_error_trace2(&wa.writer, p.ast.view(), "<memory>", pos);
    std.debug.print("{s}\n", .{wa.written()});
    return error.ParseError;
}

pub fn parse(t: *cy.Thread) anyerror!C.Ret {
    const ret = t.ret(*anyopaque);
    const parser = t.param(*cy.Parser);
    const src = t.param(cy.heap.Str);
    defer t.heap.destructStr(&src);

    parser.reportFn = parserReportFn;
    parser.ctx = t;
    const res = try parser.parse(src.slice(), 0, .{ .parseComments = true });
    if (res.has_error) {
        return error.ParseError;
    }
    ret.* = res.ast.root.?;
    return C.RetOk;
}

const ParseCyberState = struct {
    comments: []const cy.IndexSlice(u32),
    sb: std.ArrayListUnmanaged(u8),
    commentIdx: u32,
    pos: u32,
    node: *ast.Node,
};

fn genTypeSpecString(vm: *cy.VM, view: ast.AstView, opt_expr: ?*ast.Node) !cy.Value {
    if (opt_expr) |expr| {
        var sb: std.ArrayListUnmanaged(u8) = .{};
        defer sb.deinit(vm.alloc);

        var enc = ast.Encoder{ .ast = view };
        try enc.write(sb.writer(vm.alloc), expr);

        return try vm.retainOrAllocAstring(sb.items);
    } else {
        return try vm.retainOrAllocAstring("");
    }
}

fn genNodeValue(vm: *cy.VM, view: ast.AstView, node: *ast.Node) !cy.Value {
    const map_t = (try vm.findType("MapValue{string, any}")).?;
    const res = try vm.allocEmptyMap(map_t.id());
    const map = res.castHeapObject(*cy.heap.Map);
    switch (node.type()) {
        .func_param => {
            const param = node.cast(.func_param);
            const name = view.nodeString(param.name_type);
            try vm.mapSet(map, try vm.retainOrAllocAstring("name"), try vm.heap.newStr(name));

            const typeSpec = try genTypeSpecString(vm, view, param.type);
            try vm.mapSet(map, try vm.retainOrAllocAstring("typeSpec"), typeSpec);
        },
        else => {},
    }
    return res;
}

pub const IReplReadLine = struct {
    ptr: *anyopaque,
    read: *const fn(ptr: *anyopaque, prefix: [:0]const u8) anyerror![]const u8,
    free: *const fn(ptr: *anyopaque, line: []const u8) void,
};

const VmReadLine = struct {
    vm: *cy.VM,
    read_line: cy.Value,

    fn read(ptr: *anyopaque, prefix: [:0]const u8) anyerror![]const u8 {
        const self: *@This() = @ptrCast(@alignCast(ptr));
        const vm_prefix = try self.vm.heap.newStr(prefix);
        defer self.vm.release(vm_prefix);
        const line = try self.vm.callFunc(self.read_line, &.{ vm_prefix }, .{ .from_external = false });
        if (line.isInterrupt()) {
            return error.ReadLineError;
        }
        defer self.vm.release(line);
        return self.vm.alloc.dupe(u8, line.asString());
    }

    fn free(ptr: *anyopaque, line: []const u8) void {
        const self: *@This() = @ptrCast(@alignCast(ptr));
        self.vm.alloc.free(line);
    }
};