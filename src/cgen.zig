const std = @import("std");
const builtin = @import("builtin");
const build_config = @import("build_config");
const cy = @import("cyber.zig");
const cc = @import("capi.zig");
const ir = cy.ir;
const ast = cy.ast;
const bt = cy.types.BuiltinTypes;
const log = cy.log.scoped(.cgen);
const v = cy.fmt.v;
const tcc = @import("tcc");
const vmc = @import("vmc");

pub const DumpCGen = builtin.mode == .Debug and false;

const emit_nop = true;

const SymbolId = u32;

pub const VtableKey = struct {
    type: cy.TypeId,
    trait: cy.TypeId,
};

pub const Compiler = struct {
    alloc: std.mem.Allocator,
    base: *cy.Compiler,

    /// Can be cy.Sym or cy.Func.
    syms: std.AutoHashMapUnmanaged(*anyopaque, Sym),

    /// Cyber name to normalized C name's next unique suffix.
    c_names: std.StringHashMapUnmanaged(u32),

    vtables: std.AutoHashMapUnmanaged(VtableKey, u32),
    next_vtable_id: u32,

    pub fn deinit(c: *Compiler) void {
        {
            var iter = c.syms.valueIterator();
            while (iter.next()) |val| {
                c.alloc.free(val.name());
            }
            c.syms.deinit(c.alloc);
        }

        c.c_names.deinit(c.alloc);
        c.vtables.deinit(c.alloc);
    }

    fn markDeclared(c: *Compiler, chunk: *cy.Chunk, name: []const u8) void {
        const sym = chunk.sym.getMod().getSym(name).?;
        c.syms.getPtr(sym).?.declared = true;
    }

    fn mapTypeFuncVariants(c: *Compiler, template: *cy.sym.Template, name: []const u8, c_name: []const u8) !void {
        for (template.instances.items) |variant| {
            if (variant.data.sym.sym.cast(.type).getMod().getSym(name)) |sym| {
                const func = sym.cast(.func).first;
                const c_sym = c.syms.getPtr(func).?;
                c.alloc.free(c_sym.name());
                const dupe = try c.alloc.dupe(u8, c_name);
                c_sym.name_ptr = dupe.ptr;
                c_sym.name_len = @intCast(dupe.len);
                c_sym.forward_declared = true;
            }
        }
    }

    pub fn mapType(c: *Compiler, chunk: *cy.Chunk, name: []const u8, c_name: []const u8) !void {
        const sym = chunk.sym.getMod().getSym(name).?;
        const c_sym = c.syms.getPtr(sym).?;
        c.alloc.free(c_sym.name());
        const dupe = try c.alloc.dupe(u8, c_name);
        c_sym.name_ptr = dupe.ptr;
        c_sym.name_len = @intCast(dupe.len);
        c_sym.forward_declared = true;
    }

    fn mapFunc2(c: *Compiler, parent: *cy.Sym, name: []const u8, c_name: []const u8) !void {
        const func_sym = parent.getMod().?.getSym(name).?.cast(.func);
        if (func_sym.numFuncs > 1) {
            return error.OverloadedFunction;
        }
        const c_sym = c.syms.getPtr(func_sym.first).?;
        c.alloc.free(c_sym.name());
        const dupe = try c.alloc.dupe(u8, c_name);
        c_sym.name_ptr = dupe.ptr;
        c_sym.name_len = @intCast(dupe.len);
    }

    fn mapFunc(c: *Compiler, parent: *cy.Sym, name: []const u8, c_name: []const u8) !void {
        const func_sym = parent.getMod().?.getSym(name).?.cast(.func);
        if (func_sym.numFuncs > 1) {
            return error.OverloadedFunction;
        }
        const c_sym = c.syms.getPtr(func_sym.first).?;
        c.alloc.free(c_sym.name());
        const dupe = try c.alloc.dupe(u8, c_name);
        c_sym.name_ptr = dupe.ptr;
        c_sym.name_len = @intCast(dupe.len);
        c_sym.forward_declared = true;
    }

    fn mapOverloadedFunc(c: *Compiler, parent: *cy.Sym, name: []const u8, idx: usize, c_name: []const u8) !void {
        const func_sym = parent.getMod().?.getSym(name).?.cast(.func);
        var func = func_sym.first;
        for (0..idx) |_| {
            func = func.next.?;
        }
        const c_sym = c.syms.getPtr(func).?;
        c.alloc.free(c_sym.name());
        const dupe = try c.alloc.dupe(u8, c_name);
        c_sym.name_ptr = dupe.ptr;
        c_sym.name_len = @intCast(dupe.len);
    }

    /// This is useful to include only relevant syms for `@initBindLib`.
    pub fn ensureSym(c: *Compiler, sym: *cy.Sym) !void {
        const res = try c.syms.getOrPut(c.alloc, sym);
        if (res.found_existing) {
            return;
        }
        try c.createSym2(sym.name(), res.value_ptr);

        // Ensure child symbols.
        if (sym.type == .type) {
            const type_ = sym.cast(.type).type;
            switch (type_.kind()) {
                .pointer => {
                    const pointer = type_.cast(.pointer);
                    try c.ensureSym(&pointer.child_t.sym().head);
                },
                .struct_t => {
                    const struct_t = type_.cast(.struct_t);
                    for (struct_t.fields()) |field| {
                        try c.ensureSym(&field.type.sym().head);
                    }
                },
                else => {
                },
            }
        }
    }

    pub fn createSym(c: *Compiler, sym_ptr: *anyopaque, sym_name_opt: ?[]const u8) !void {
        var new: Sym = undefined;
        try c.createSym2(sym_name_opt, &new);
        try c.syms.putNoClobber(c.alloc, sym_ptr, new);
    }

    /// Creates a mapping between sema sym and the gen sym.
    pub fn createSym2(c: *Compiler, sym_name_opt: ?[]const u8, dst: *Sym) !void {
        const sym_name = sym_name_opt orelse {
            const name = "";
            dst.* = .{
                .name_ptr = name.ptr,
                .name_len = @intCast(name.len),
                .declared = false,
                .forward_declared = false,
            };
            return;
        };
        var buf: [1024]u8 = undefined;
        const name_n = try normalizeSymName(&buf, sym_name);

        var new_name: []const u8 = undefined;
        const res = try c.c_names.getOrPut(c.alloc, sym_name);
        if (res.found_existing) {
            new_name = try std.fmt.allocPrint(c.alloc, "{s}_{}", .{ name_n, res.value_ptr.* });
            res.value_ptr.* += 1;
        } else {
            new_name = try std.fmt.allocPrint(c.alloc, "{s}_0", .{name_n});
            res.value_ptr.* = 1;
        }
        dst.* = .{
            .name_ptr = new_name.ptr,
            .name_len = @intCast(new_name.len),
            .declared = false,
            .forward_declared = false,
        };
    }

    pub fn createPredefinedSym(c: *Compiler, sym_ptr: *anyopaque, sym_name: []const u8) !void {
        return c.createSymExact(sym_ptr, sym_name, true);
    }

    pub fn createSymExact(c: *Compiler, sym_ptr: *anyopaque, sym_name: []const u8, declared: bool) !void {
        const dupe = try c.alloc.dupe(u8, sym_name);
        try c.syms.putNoClobber(c.alloc, sym_ptr, .{
            .name_ptr = dupe.ptr,
            .name_len = @intCast(dupe.len),
            .declared = declared,
            .forward_declared = false,
        });
    }
};

fn normalizeSymName(buf: []u8, name: []const u8) ![]const u8 {
    const ReplaceChars = "&|%^*/+-<>=~";
    if (std.mem.indexOfAny(u8, name, ReplaceChars)) |idx| {
        // Normalize to C name.
        var fbuf = std.io.fixedBufferStream(buf);
        const w = fbuf.writer();
        try w.print("{s}{s}", .{ name[0..idx], normalizeChar(name[idx]) });

        var rest = name[idx + 1 ..];
        while (std.mem.indexOfAny(u8, rest, ReplaceChars)) |idx2| {
            try w.print("{s}{s}", .{ rest[0..idx2], normalizeChar(rest[idx2]) });
            rest = rest[idx2 + 1 ..];
        }
        try w.writeAll(rest);
        return fbuf.getWritten();
    } else {
        return name;
    }
}

fn normalizeChar(char: u8) []const u8 {
    return switch (char) {
        '&' => "_and",
        '|' => "_or",
        '^' => "_pow",
        '%' => "_mod",
        '*' => "_mul",
        '/' => "_div",
        '-' => "_sub",
        '+' => "_add",
        '<' => "_lt",
        '>' => "_gt",
        '=' => "_eq",
        '~' => "_not",
        else => &.{char},
    };
}

const Sym = struct {
    name_ptr: [*]const u8,
    name_len: u16,

    // Track whether a type was declared.
    declared: bool,

    // For base types. Pointer types check for this.
    forward_declared: bool,

    pub fn name(self: Sym) []const u8 {
        return self.name_ptr[0..self.name_len];
    }
};

const Symbol = struct {
    name: []const u8,
};

pub const Chunk = struct {
    alloc: std.mem.Allocator,
    ir: *cy.ir.Buffer,
    encoder: cy.ast.Encoder,
    out: std.ArrayListUnmanaged(u8),
    outw: std.ArrayListUnmanaged(u8).Writer,
    buf: std.ArrayListUnmanaged(u8),
    bufw: std.ArrayListUnmanaged(u8).Writer,
    proc: Proc,

    /// Alias for `compiler.syms`. Assumes all needed syms are registered by compiler.
    syms: *const std.AutoHashMapUnmanaged(*anyopaque, Sym),

    compiler: *Compiler,
    base: *cy.Chunk,
    vm: *cy.VM,
    sema: *cy.Sema,
    errNode: ?*ast.Node,
    indent: u32,
    emitSourceMap: bool,
    ast: cy.ast.AstView,
    srcUri: []const u8,
    last_ret_name: []const u8,

    pub fn init(compiler: *Compiler, c: *cy.Chunk) Chunk {
        const alloc = compiler.alloc;
        var new = Chunk{
            .alloc = alloc,
            .ir = &c.ir,
            .encoder = c.encoder,
            .out = .{},
            .outw = undefined,
            .buf = .{},
            .bufw = undefined,
            .syms = &compiler.syms,
            .proc = .{},
            .base = c,
            .vm = c.vm,
            .sema = c.sema,
            .compiler = compiler,
            .errNode = null,
            .indent = 0,
            .emitSourceMap = compiler.base.config.emit_source_map,
            .ast = c.ast,
            .srcUri = c.srcUri,
            .last_ret_name = "",
        };
        new.outw = new.out.writer(alloc);
        new.bufw = new.buf.writer(alloc);
        return new;
    }

    fn deinit(c: *Chunk) void {
        c.out.deinit(c.alloc);
        c.buf.deinit(c.alloc);
        c.proc.deinit(c.alloc);
        c.alloc.free(c.last_ret_name);
    }

    fn pushBlock(c: *Chunk) void {
        c.indent += 1;
    }

    fn popBlock(c: *Chunk) void {
        c.indent -= 1;
    }

    fn beginLine(c: *Chunk, node_opt: ?*ast.Node) !void {
        if (c.emitSourceMap) {
            if (node_opt) |node| {
                try c.pushIndent();
                var line: u32 = undefined;
                var col: u32 = undefined;
                var lineStart: u32 = undefined;
                c.ast.computeLinePos(node.pos(), &line, &col, &lineStart);
                try c.pushSpanFmt("#line {} \"{s}\"", .{line, c.srcUri});
                try c.pushNewLine();
            }
        }
        try c.pushIndent();
    }

    fn pushNewLine(c: *Chunk) !void {
        try c.out.append(c.alloc, '\n');
    }

    fn pushLine(c: *Chunk, line: []const u8, node: *ast.Node) !void {
        try c.beginLine(node);
        try c.out.appendSlice(c.alloc, line);
        try c.pushSpan("\n");
    }

    fn pushLineNoMapping(c: *Chunk, line: []const u8) !void {
        try c.pushIndent();
        try c.out.appendSlice(c.alloc, line);
        try c.out.append(c.alloc, '\n');
    }

    fn bufStart(c: *Chunk) u32 {
        return @intCast(c.buf.items.len);
    }

    fn bufPushFmt(c: *Chunk, comptime format: []const u8, args: anytype) !void {
        try c.bufw.print(format, args);
    }

    fn bufPush(c: *Chunk, str: []const u8) !void {
        try c.bufw.writeAll(str);
    }

    fn bufPop(c: *Chunk, start: u32) []const u8 {
        defer c.buf.items.len = start;
        return c.buf.items[start..];
    }

    fn pushSpanFmt(c: *Chunk, comptime format: []const u8, args: anytype) !void {
        try c.outw.print(format, args);
    }

    fn pushSpan(c: *Chunk, span: []const u8) !void {
        try c.out.appendSlice(c.alloc, span);
    }

    fn pushIndent(c: *Chunk) !void {
        for (0..c.indent) |_| {
            try c.out.append(c.alloc, '\t');
        }
    }

    fn cFuncName(c: *Chunk, func: *cy.Func) []const u8 {
        if (c.syms.get(@ptrCast(func))) |csym| {
            return csym.name();
        }
        std.debug.panic("Missing csym: {s}", .{func.name()});
    }

    fn cSymName(c: *Chunk, sym: *cy.Sym) []const u8 {
        if (c.syms.get(sym)) |csym| {
            return csym.name();
        }
        const name = c.sema.allocSymName(sym) catch @panic("");
        defer c.alloc.free(name);
        std.debug.panic("Missing csym: {s}", .{name});
    }

    fn typeName(c: *Chunk, type_: *cy.Type) TypeName {
        return TypeName.init(c, type_);
    }

    fn typeNameRet(c: *Chunk, type_: *cy.Type) TypeName {
        var new = TypeName.init(c, type_);
        new.ret = true;
        return new;
    }

    fn fieldName(c: *Chunk, field: *cy.types.Field) []const u8 {
        _ = c;
        const name = field.sym.head.name();
        return field_rewrite.get(name) orelse return name;
    }

    fn caseName(c: *Chunk, name: []const u8) []const u8 {
        _ = c;
        return field_rewrite.get(name) orelse return name;
    }
};

const field_rewrite = std.StaticStringMap([]const u8).initComptime(.{
    .{ "float", "float_" },
    .{ "enum", "enum_" },
    .{ "void", "void_" },
    .{ "int", "int_" },
    .{ "bool", "bool_" },
    .{ "struct", "struct_" },
});

const ProcType = enum {
    main,
    func,
};

const Proc = struct {
    type: ProcType = .main,

    var_names: std.StringHashMapUnmanaged(u32) = .{},
    locals: std.ArrayListUnmanaged(Local) = .{},

    resetVerboseOnEnd: bool = false,

    /// Track the highest try block suffix.
    maxTrySuffix: u32 = 0,

    data: union {
        func: *cy.Func,
    } = undefined,

    fn setMain(self: *Proc, alloc: std.mem.Allocator) void {
        self.reset(alloc);
        self.type = .main;
        self.resetVerboseOnEnd = false;
        self.maxTrySuffix = 0;
        self.data = undefined;
    }

    fn setFunc(self: *Proc, alloc: std.mem.Allocator, func: *cy.Func) void {
        self.reset(alloc);
        self.type = .func;
        self.resetVerboseOnEnd = false;
        self.maxTrySuffix = 0;
        self.data = .{
            .func = func,
        };
    }

    fn reset(self: *Proc, alloc: std.mem.Allocator) void {
        self.var_names.clearRetainingCapacity();
        for (self.locals.items) |local| {
            if (local.kind == .local) {
                alloc.free(local.data.local.name);
            }
        }
        self.locals.clearRetainingCapacity();
    }

    fn deinit(self: *Proc, alloc: std.mem.Allocator) void {
        self.locals.deinit(alloc);
        self.var_names.deinit(alloc);
    }

    fn getLocal(p: *Proc, id: u32) *Local {
        return &p.locals.items[id];
    }
};

const LocalKind = enum {
    null,
    local,
};

pub const Local = struct {
    kind: LocalKind,
    data: union {
        null: void,
        local: struct {
            name: []const u8,
        },
    },
};

const Cstr = struct {
    const none = Cstr{};

    fn init() Cstr {
        return .{};
    }
};

const GenValue = struct {
};

pub fn gen(self: *cy.Compiler) !cy.compiler.AotCompileResult {
    var compiler = Compiler{
        .base = self,
        .alloc = self.alloc,
        .syms = .{},
        .c_names = .{},
        .vtables = .{},
        .next_vtable_id = 0,
    };
    defer compiler.deinit();

    for (self.chunks.items) |chunk| {
        // Generate unique C name for each symbol.

        for (chunk.syms.items) |sym| {
            switch (sym.type) {
                .chunk => {
                    // Get stem name.
                    // e.g. "/a/b/c/d.cy" is reduced to "d"
                    var name = chunk.sym.head.name();
                    if (name[0] == '/') {
                        name = std.fs.path.stem(name);
                    }
                    try compiler.createSym(sym, name);
                },
                .type => {
                    const type_sym = sym.cast(.type);
                    switch (type_sym.type.kind()) {
                        .pointer => {
                            try compiler.createSym(sym, null);
                        },
                        .borrow => {
                            try compiler.createSym(sym, null);
                        },
                        else => {
                            try compiler.createSym(sym, sym.name());
                        },
                    }
                },
                .hostVar,
                .userVar => {
                    try compiler.createSym(sym, sym.name());
                },
                else => {},
            }
        }

        for (chunk.funcs.items) |func| {
            if (func.type == .extern_) {
                try compiler.createSymExact(func, func.data.extern_.externName(), false);
            } else {
                try compiler.createSym(func, func.name());
            }
        }
    }

    // Sym/func overrides to map to definitions in `aot.h`.
    const builtins_c = self.chunk_map.get("core").?;
    const core_sym: *cy.Sym = @ptrCast(builtins_c.sym);
    try compiler.mapType(builtins_c, "str", "CB_str");
    try compiler.mapType(builtins_c, "StrBuffer", "CB_StrBuffer");
    try compiler.mapType(builtins_c, "int", "CB_int");
    try compiler.mapType(builtins_c, "i32", "CB_i32");
    try compiler.mapType(builtins_c, "r32", "CB_r32");
    try compiler.mapType(builtins_c, "r64", "CB_r64");
    try compiler.mapType(builtins_c, "void", "CB_void");
    try compiler.mapType(builtins_c, "bool", "CB_bool");
    try compiler.mapType(builtins_c, "error", "CB_error");
    try compiler.mapType(builtins_c, "symbol", "CB_symbol");
    try compiler.mapType(builtins_c, "byte", "CB_byte");
    try compiler.mapType(builtins_c, "float", "CB_float");
    try compiler.mapType(builtins_c, "Object", "CB_Object");
    compiler.markDeclared(builtins_c, "Object");
    try compiler.mapType(builtins_c, "NumberFormat", "CB_NumberFormat");
    try compiler.mapType(builtins_c, "MetaType", "CB_MetaType");
    try compiler.mapFunc2(core_sym, "free", "cb_free");
    try compiler.mapFunc2(core_sym, "alloc_", "cb_alloc");
    try compiler.mapFunc(core_sym, "eprint", "cb_eprint");
    try compiler.mapFunc2(core_sym, "print", "cb_print");
    try compiler.mapFunc2(core_sym, "utf8Check", "cb_utf8_check");
    try compiler.mapFunc2(core_sym, "utf8Decode", "cb_utf8_decode");
    try compiler.mapFunc2(core_sym, "utf8SeqLen", "cb_utf8_seq_len");
    try compiler.mapFunc2(core_sym, "$newObjectUndef", "cb_object_undef");
    try compiler.mapFunc2(core_sym, "$getDeinitObject", "cb_get_deinit_object");
    try compiler.mapFunc(core_sym, "$newAstrUndef", "cb_astr_undef");
    try compiler.mapFunc(core_sym, "$newUstrUndef", "cb_ustr_undef");
    try compiler.mapTypeFuncVariants(self.sema.func_tmpl, "$size", "cb_func_union_size");
    if (self.chunk_map.get("test")) |chunk| {
        _ = chunk;
        // try compiler.mapFunc(@ptrCast(chunk.sym), "eq", "cb_test_eq");
    }
    try compiler.mapFunc2(core_sym, "$freeObject", "cb_free_object");
    try compiler.mapFunc2(core_sym, "$releaseOnly", "cb_release_only");
    try compiler.mapFunc(core_sym, "panic", "cb_panic");
    try compiler.mapFunc(core_sym, "$memcpy", "cb_memcpy");

    const symbol_sym: *cy.Sym = @ptrCast(self.sema.symbol_t.sym());
    try compiler.mapFunc2(symbol_sym, "name", "cb_symbol_name");

    const int_sym: *cy.Sym = @ptrCast(self.sema.i64_t.sym());
    try compiler.mapFunc2(int_sym, "uge", "cb_int_uge");
    try compiler.mapFunc2(int_sym, "ugt", "cb_int_ugt");
    try compiler.mapFunc2(int_sym, "umul", "cb_int_umul");
    try compiler.mapOverloadedFunc(int_sym, "fmt", 1, "cb_int_fmt");

    const i32_sym: *cy.Sym = @ptrCast(self.sema.i32_t.sym());
    try compiler.mapOverloadedFunc(i32_sym, "fmt", 1, "cb_i32_fmt");

    const byte_sym: *cy.Sym = @ptrCast(self.sema.i8_t.sym());
    try compiler.mapOverloadedFunc(byte_sym, "fmt", 1, "cb_byte_fmt");

    const float_sym: *cy.Sym = @ptrCast(self.sema.f64_t.sym());
    try compiler.mapFunc2(float_sym, "$infix**", "cb_float_pow");
    try compiler.mapFunc2(float_sym, "fmt", "cb_float_fmt");

    const str_sym: *cy.Sym = @ptrCast(self.sema.str_t.sym());
    try compiler.mapFunc2(str_sym, "len", "cb_str_len");

    const object_sym: *cy.Sym = @ptrCast(self.sema.object_t.sym());
    try compiler.mapFunc2(object_sym, "type", "cb_object_type");
    try compiler.mapFunc(object_sym, "toString", "cb_object_string");

    var chunks = try self.alloc.alloc(Chunk, self.chunks.items.len);
    for (self.chunks.items, 0..) |chunk, i| {
        chunks[i] = Chunk.init(&compiler, chunk);
    }
    defer {
        for (chunks) |*chunk| {
            chunk.deinit();
        }
        self.alloc.free(chunks);
    }

    // Generate head at the end since it relies on chunk passes.
    var head: std.ArrayListUnmanaged(u8) = .{};
    const headw = head.writer(self.alloc);
    try genHeader(&compiler, headw, chunks);
    defer head.deinit(self.alloc);

    for (self.chunks.items, 0..) |chunk, i| {
        log.tracev("Perform codegen for chunk{}: {s}", .{chunk.id, chunk.srcUri});

        try cy.sema.pushChunkResolveContext(chunk, @ptrCast(chunk.ast.root));
        defer cy.sema.popResolveContext(chunk);

        try genChunk(&chunks[i]);
        log.tracev("Done. performChunkCodegen {s}", .{chunk.srcUri});
    }

    // All chunks are merged into one C output.
    // TODO: Support spawning multiple `cc` processes and then linking them. Could be faster for larger projects and also necessary for incremental compilation.

    // Ensure that `out` directory exists.
    std.fs.cwd().makeDir("out") catch |err| {
        if (err != error.PathAlreadyExists) {
            return err;
        }
    };

    const outName = std.fs.path.basename(self.chunks.items[0].srcUri);
    const outPath = try std.fmt.allocPrintSentinel(self.alloc, "out/{s}.c", .{outName}, 0);
    defer self.alloc.free(outPath);

    const outFile = try std.fs.cwd().createFile(outPath, .{ .truncate = true });
    defer outFile.close();

    try outFile.writeAll(head.items);

    for (chunks) |chunk| {
        try outFile.writeAll(chunk.out.items);
    }

    var w = outFile.writer(&.{}).interface;
    try genFooter(&compiler, &w, chunks);

    var exePath: [:0]const u8 = undefined;
    const stemName = std.fs.path.stem(outName);
    if (builtin.os.tag == .windows) {
        exePath = try std.fmt.allocPrintSentinel(self.alloc, "out/{s}.exe", .{stemName}, 0);
    } else {
        exePath = try std.fmt.allocPrintSentinel(self.alloc, "out/{s}", .{stemName}, 0);
    }
    errdefer self.alloc.free(exePath);

    if (self.config.backend == cc.BackendTCC) {
        if (!build_config.ffi) {
            return error.TCCError;
        }

        // const src = try std.fs.cwd().readFileAllocOptions(self.alloc, outPath, 1e9, null, @alignOf(u8), 0);
        // defer self.alloc.free(src);

        const state = tcc.tcc_new();
        defer tcc.tcc_delete(state);

        if (tcc.tcc_set_output_type(state, tcc.TCC_OUTPUT_EXE) == -1) {
            return error.TCCError;
        }

        // // Don't include libtcc1.a.
        // if (tcc.tcc_set_options(state, "-nostdlib") == -1) {
        //     return error.TCCError;
        // }

        if (builtin.os.tag == .macos) {
            if (tcc.tcc_add_include_path(state, "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include") == -1) {
                return error.TCCError;
            }
        }

        // In case we need to load libtcc1.a
        // if (tcc.tcc_add_library_path(state, "./out") == -1) {
        //     return error.TCCError;
        // }

        // Temporary way to use builtin host functions.
        // Only linking with elf library is supported.
        // if (tcc.tcc_add_file(state, "./zig-out/lib/libcyber.a") == -1) {
        //     return error.TCCError;
        // }

        if (tcc.tcc_add_file(state, outPath.ptr) == -1) {
            // if (tcc.tcc_compile_string(state, src.ptr) == -1) {
            return error.TCCError;
        }

        // const _src =
        //     \\#include <stdio.h>
        //     \\int main() {
        //     \\  printf("hello world\n");
        //     \\  return 0;
        //     \\}
        //     \\
        //     ;
        // if (tcc.tcc_compile_string(state, _src.ptr) == -1) {
        //     return error.TCCError;
        // }
        if (tcc.tcc_output_file(state, exePath.ptr) == -1) {
            return error.TCCError;
        }
    } else {
        var args: std.ArrayListUnmanaged([]const u8) = .{};
        defer args.deinit(self.alloc);

        try args.appendSlice(self.alloc, &.{"cc", "-o", exePath, outPath});
        try args.append(self.alloc, "-Wno-parentheses-equality");
        try args.append(self.alloc, "-Wno-unused-value");
        try args.append(self.alloc, "-Wno-return-type");
        try args.appendSlice(self.alloc, self.c_flags.items);

        const res = try std.process.Child.run(.{
            .allocator = self.alloc,
            .argv = args.items,
        });
        defer self.alloc.free(res.stdout);
        if (res.term != .Exited or res.term.Exited != 0) {
            _ = try self.addReportConsume(.compile_err, res.stderr, null, 0);
            return error.CompileError;
        }
    }
    return .{
        .exePath = exePath,
    };
}

pub fn genFuncForwardDecl(c: *Chunk, w: anytype, func: *cy.Func) !void {
    const c_sym = c.syms.getPtr(func).?;
    if (c_sym.forward_declared) {
        return;
    }

    // Declare dependencies.
    const params = func.sig.params();
    for (params) |param| {
        try declareType(c, param.get_type(), w);
    }
    try declareType(c, func.sig.ret, w);

    c_sym.forward_declared = true;
    switch (func.type) {
        .userLambda => {
            try w.print("{f} {s}(CB_Thread*", .{
                c.typeNameRet(func.sig.ret), c.cFuncName(func),
            });
            if (func.data.userLambda.is_closure) {
                try w.writeAll(", CB_FuncUnion*");
            }
            for (params) |param| {
                try w.print(", {f}", .{ c.typeName(param.get_type()) });
            }
            try w.writeAll(");\n");
        },
        .hostFunc => {
            try w.print("{f} {s}(CB_Thread*", .{
                c.typeNameRet(func.sig.ret), c.cFuncName(func),
            });
            for (params) |param| {
                try w.print(", {f}", .{ c.typeName(param.get_type()) });
            }
            try w.writeAll(");\n");
        },
        .userFunc => {
            try w.print("{f} {s}(CB_Thread*", .{
                c.typeNameRet(func.sig.ret), c.cFuncName(func),
            });
            for (params) |param| {
                try w.print(", {f}", .{ c.typeName(param.get_type()) });
            }
            try w.writeAll(");\n");
        },
        .extern_ => {
            try w.print("extern {f} {s}(", .{
                c.typeNameRet(func.sig.ret), c.cFuncName(func),
            });
            if (params.len > 0) {
                try w.print("{f}", .{ c.typeName(params[0].get_type()) });
                for (params[1..]) |param| {
                    try w.print(", {f}", .{ c.typeName(param.get_type()) });
                }
            }
            try w.writeAll(");\n");
        },
        else => {},
    }
}

fn genFooter(self: *Compiler, w: *std.Io.Writer, chunks: []Chunk) !void {
    _ = self;
    _ = chunks;
    const aot_c = @embedFile("aot.c");
    try w.writeAll(aot_c);
}

pub fn genVmHeaders(w: anytype) !void {
    try w.print(
        \\#define bool _Bool
        \\#define i64 long long
        \\#define u64 unsigned long long
        \\#define Value u64
        // TODO: Check for 32bit addressing.
        \\#define size_t u64
        \\#define i8 signed char
        \\#define u8 unsigned char
        \\#define byte unsigned char
        \\#define i16 short
        \\#define u16 unsigned short
        \\#define i32 int
        \\#define u32 unsigned int
        \\#define usize size_t
        \\#define f64 double
        \\#define f32 float
        \\
        \\typedef bool CB_bool;
        \\typedef i8  CB_i8;
        \\typedef i16 CB_i16;
        \\typedef i32 CB_i32;
        \\typedef i64 CB_int;
        \\typedef u8  CB_byte;
        \\typedef u16 CB_r16;
        \\typedef u32 CB_r32;
        \\typedef u64 CB_r64;
        \\typedef u8 Ret;
        \\
        \\#define PointerMask 0xFFFE000000000000
        \\typedef struct ZAllocator {{
        \\    void* ptr;
        \\    void* vtable;
        \\}} ZAllocator;
        \\typedef struct Thread {{
        \\    size_t id;
        \\    void* vm;
        \\    u16* pc;
        \\    u64* fp;
        \\    u64* fp_end;
        \\    Value* stack_ptr;
        \\    size_t stack_len;
        \\    Value* stack_end;
        \\}} Thread;
        \\typedef struct ZThread {{
        \\    u8 padding[16];
        \\    Thread c;
        \\}} ZThread;
        // \\typedef struct ZVM {{
        // \\    ZAllocator alloc;
        // \\    VMC c;
        // \\}} VM;
        \\extern u64 icyAllocObject(ZThread*, u32);
        \\extern Value host_new_obj_init(ZThread*, u32, u8*, usize);
        \\extern void* host_call_func(ZThread*, usize, usize);
        \\extern ZThread* host_ensure_thread();
        //\\extern int printf(char* fmt, ...);
        \\#define CALL_ARG_START 5
        \\void* thread_ret(ZThread* t, size_t reg_size) {{
        \\    t->c.fp_end = t->c.fp + 4;
        \\    return t->c.fp - reg_size;
        \\}}
        \\void* thread_param(ZThread* t, size_t reg_size) {{
        \\    void* res = t->c.fp_end;
        \\    t->c.fp_end = t->c.fp_end + reg_size;
        \\    return res;
        \\}} 
        // \\extern void exit(int code);
        \\
    , .{});
}

fn genHeader(self: *Compiler, w: anytype, chunks: []Chunk) !void {
    if (cy.Trace) {
        try w.writeAll("#define TRACE 1\n");
    }

    for (self.base.c_includes.items) |import| {
        try w.print("#include {s}\n", .{import});
    }

    const aot_h = @embedFile("aot.h");
    try w.writeAll(aot_h);

    // Predefines.
    try w.writeAll(
        \\
        \\
    );

    // Declare types. Forward declare functions.
    for (chunks) |*c| {
        for (c.base.syms.items) |sym| {
            switch (sym.type) {
                .type => {
                    const type_sym = sym.cast(.type);
                    const type_ = type_sym.type;
                    try declareType(c, type_, w);
                },
                .userVar => {
                    const user_var = sym.cast(.userVar);
                    try declareType(c, user_var.type, w);
                    try w.print("{f} {s};\n", .{ c.typeName(user_var.type), c.cSymName(sym) });
                },
                .hostVar => {
                    const host_var = sym.cast(.hostVar);
                    try declareType(c, host_var.type, w);
                    try w.print("{f} {s};\n", .{ c.typeName(host_var.type), c.cSymName(sym) });
                },
                else => {},
            }
        }

        for (c.base.funcs.items) |func| {
            try genFuncForwardDecl(c, w, func);
        }
    }
    try w.writeByte('\n');


    // Symbols.
    try w.print("const char* cbi_symbols[{}] = {{\n", .{self.base.sema.symbols.items.len});
    for (self.base.sema.symbols.items) |sym| {
        try w.print("\t\"{s}\",\n", .{sym});
    }
    try w.writeAll("};\n");

    // Type tables.
    const c = &chunks[0];
    try w.print("CBI_TypeTable cbi_type_tables[{}] = {{\n", .{self.base.sema.types.items.len});
    try w.writeAll("    { 0, \"NULL\", NULL, NULL, 0 },\n");
    for (self.base.sema.types.items[1..]) |type_| {
        if (self.syms.get(type_.sym())) |sym| {
            if (sym.declared) {
                try w.print("    {{ sizeof({f}), \"{s}\"", .{ c.typeName(type_), type_.name() });
                if (type_.deinit_obj) |obj_deinit| {
                    try w.print(", &{s}", .{ c.cFuncName(obj_deinit) });
                } else {
                    try w.writeAll(", NULL");
                }
                if (type_.dtor) |dtor| {
                    try w.print(", (void*)&{s}", .{ c.cFuncName(dtor) });
                } else {
                    try w.writeAll(", NULL");
                }
                try w.print(", {} }}, // id={}\n", .{@intFromEnum(type_.kind()), type_.id()});
                continue;
            }
        }
        try w.writeAll("    { 0, \"NULL\", NULL, NULL, 0 },\n");
    }
    try w.writeAll("};\n");

    // Virtual tables.
    for (self.base.sema.types.items) |type_| {
        if (type_.kind() == .struct_t) {
            const struct_t = type_.cast(.struct_t);

            // Generate implementation vtables.
            for (struct_t.impls()) |impl| {
                const table_id = c.compiler.next_vtable_id;
                c.compiler.next_vtable_id += 1;
                const key = VtableKey{ .type = struct_t.base.id(), .trait = impl.trait.base.id() };
                try c.compiler.vtables.put(c.alloc, key, table_id);
                try w.print("{f}Table vtable{} = {{\n", .{ c.typeName(&impl.trait.base), table_id });
                for (impl.funcs) |func| {
                    try w.print("    (void*)&{s},\n", .{ c.cFuncName(func) });
                }
                try w.writeAll("};\n");
            }
        }
    }
}

/// DFS declaration to ensure type dependencies are declared first.
fn declareType(c: *Chunk, type_: *cy.Type, w: anytype) !void {
    try c.compiler.ensureSym(&type_.sym().head);
    const c_sym = c.syms.getPtr(type_.sym()).?;
    if (c_sym.declared) {
        return;
    }
    c_sym.declared = true;

    switch (type_.kind()) {
        .func_ptr => {
            // Extract name since `c_sym` can be invalidated.
            const name = c_sym.name();

            const func_ptr = type_.cast(.func_ptr);
            const params = func_ptr.sig.params();
            for (params) |param| {
                try declareType(c, param.get_type(), w);
            }
            try declareType(c, func_ptr.sig.ret, w);

            try w.writeAll("typedef ");
            try writeFuncPtrType(c, w, name, func_ptr.sig);
            try w.writeAll(";\n");
        },
        .func => {
            const func_union = type_.cast(.func);
            try w.print("typedef CB_FuncUnion* {f};\n", .{ c.typeName(type_) });

            // Generate the dispatch function.
            try w.print("{f} cb_dispatch_{f}(CB_Thread* ctx, {f} func", .{ c.typeName(func_union.sig.ret), c.typeName(type_), c.typeName(type_) });
            const params = func_union.sig.params();
            for (params, 0..) |param, i| {
                try w.print(", {f} arg{}", .{c.typeName(param.get_type()), i});
            }
            try w.writeAll(") {\n");
            try w.writeAll("    if (func->kind == 2) {\n");
            try w.print("        {f} (*func_ptr)(CB_Thread*, {f}", .{c.typeName(func_union.sig.ret), c.typeName(type_)});
            for (params) |param| {
                try w.print(", {f}", .{c.typeName(param.get_type())});
            }
            try w.writeAll(") = func->ptr;\n");
            try w.writeAll("        return func_ptr(ctx, func");
            for (0..params.len) |i| {
                try w.print(", arg{}", .{i});
            }
            try w.writeAll(");\n");
            try w.writeAll("    } else {\n");
            try w.print("        {f} (*func_ptr)(CB_Thread*", .{c.typeName(func_union.sig.ret)});
            for (params) |param| {
                try w.print(", {f}", .{c.typeName(param.get_type())});
            }
            try w.writeAll(") = func->ptr;\n");
            try w.writeAll("        return func_ptr(ctx");
            for (0..params.len) |i| {
                try w.print(", arg{}", .{i});
            }
            try w.writeAll(");\n");
            try w.writeAll("    }\n");
            try w.writeAll("}\n");
        },
        .bool => {
            try w.print("typedef bool {f};\n", .{ c.typeName(type_) });
        },
        .float => {
            const float_t = type_.cast(.float);
            switch (float_t.bits) {
                32 => {
                    try w.print("typedef float {f};\n", .{ c.typeName(type_) });
                },
                64 => {
                    try w.print("typedef double {f};\n", .{ c.typeName(type_) });
                },
                else => {
                    std.debug.panic("TODO: {}", .{float_t.bits});
                },
            }
        },
        .raw => {
            const raw_t = type_.cast(.raw);
            switch (raw_t.bits) {
                8 => {
                    try w.print("typedef u8 {f};\n", .{ c.typeName(type_) });
                },
                16 => {
                    try w.print("typedef u16 {f};\n", .{ c.typeName(type_) });
                },
                32 => {
                    try w.print("typedef u32 {f};\n", .{ c.typeName(type_) });
                },
                64 => {
                    try w.print("typedef u64 {f};\n", .{ c.typeName(type_) });
                },
                else => {
                    @panic("unexpected");
                },
            }
        },
        .int => {
            const int_t = type_.cast(.int);
            switch (int_t.bits) {
                8 => {
                    try w.print("typedef i8 {f};\n", .{ c.typeName(type_) });
                },
                16 => {
                    try w.print("typedef i16 {f};\n", .{ c.typeName(type_) });
                },
                32 => {
                    try w.print("typedef i32 {f};\n", .{ c.typeName(type_) });
                },
                64 => {
                    try w.print("typedef i64 {f};\n", .{ c.typeName(type_) });
                },
                else => {
                    @panic("unexpected");
                },
            }
        },
        .vector => {
            const vector_t = type_.cast(.vector);
            try declareType(c, vector_t.elem_t, w);

            try w.print("typedef struct {f} {{\n", .{c.typeName(type_)});
            try w.print("    {f} arr[{}];\n", .{ c.typeName(vector_t.elem_t), vector_t.n });
            try w.print("}} {f};\n", .{ c.typeName(type_) });
        },
        .pointer => {
            const base_t = type_.getBaseType();
            const base_info = c.syms.getPtr(@ptrCast(base_t.sym())).?;
            if (!base_info.declared and !base_info.forward_declared) {
                if (base_t.isCStruct()) {
                    // Forward declare type for pointer type.
                    try w.print("typedef struct {f} {f};\n", .{ c.typeName(base_t), c.typeName(base_t) });
                    base_info.forward_declared = true;
                } else {
                    try declareType(c, base_t, w);
                }
            }
        },
        .borrow => {
            const base_t = type_.getBaseType();
            const base_info = c.syms.getPtr(@ptrCast(base_t.sym())).?;
            if (!base_info.declared and !base_info.forward_declared) {
                try w.print("typedef struct {f} {f};\n", .{ c.typeName(base_t), c.typeName(base_t) });
                base_info.forward_declared = true;
            }
        },
        .generic_trait => {
            const trait_t = type_.cast(.generic_trait);
            for (trait_t.members()) |member| {
                try declareType(c, member.func.sig.ret, w);
                for (member.func.sig.params()[1..]) |param| {
                    try declareType(c, param.get_type(), w);
                }
            }

            try w.print("typedef struct {f}Table {{\n", .{ c.typeName(type_) });
            for (trait_t.members()) |member| {
                try w.writeAll("\t");

                try writeTraitFuncType(c, w, member.func.name(), member.func.sig);
                try w.writeAll(";\n");
            }
            try w.print("}} {f}Table;\n", .{ c.typeName(type_)});

            try w.print("typedef struct {f}Ptr {{\n", .{ c.typeName(type_) });
            try w.print("\tvoid* ptr;\n", .{});
            try w.print("\t{f}Table* vtable;\n", .{ c.typeName(type_) });
            try w.print("}} {f}Ptr;\n", .{ c.typeName(type_) });
            try w.print("typedef void* {f};\n", .{ c.typeName(type_) });
        },
        .ref_trait => {
            const trait_ref = type_.cast(.ref_trait);
            try w.print("typedef {f}Ptr {f};\n", .{ c.typeName(&trait_ref.generic.base), c.typeName(type_) });
        },
        .borrow_trait => {
            const borrow_trait = type_.cast(.borrow_trait);
            try w.print("typedef {f}Ptr {f};\n", .{ c.typeName(&borrow_trait.generic.base), c.typeName(type_) });
        },
        .struct_t => {
            const struct_t = type_.cast(.struct_t);
            for (struct_t.fields()) |field| {
                try declareType(c, field.type, w);
            }

            try w.print("typedef struct {f} {{\n", .{ c.typeName(type_) });
            for (struct_t.fields()) |*field| {
                if (field.type.id() == bt.Void) {
                    try w.print("   u64 {s};\n", .{ c.fieldName(field) });
                } else {
                    try w.print("   {f} {s};\n", .{ c.typeName(field.type), c.fieldName(field) });
                }
            }
            try w.print("}} {f};\n", .{ c.typeName(type_) });
        },
        .c_union => {
            const c_union = type_.cast(.c_union);
            for (c_union.cases()) |case| {
                try declareType(c, case.payload_t, w);
            }

            try w.print("typedef union {f} {{\n", .{ c.typeName(type_) });
            for (c_union.cases()) |*case| {
                try w.print("   {f} {s};\n", .{ c.typeName(case.payload_t), c.caseName(case.name()) });
            }
            try w.print("}} {f};\n", .{ c.typeName(type_) });
        },
        .option => {
            const option_t = type_.cast(.option);
            try declareType(c, option_t.child_t, w);
            if (option_t.zero_union) {
                try w.print("typedef {f} {f};\n", .{ c.typeName(option_t.child_t), c.typeName(type_) });
            } else {
                try w.print(
                    \\typedef struct {f} {{
                    \\    u64 tag;
                    \\    {f} payload;
                    \\}} {f};
                    \\
                , .{ c.typeName(type_), c.typeName(option_t.child_t), c.typeName(type_) });
            }
        },
        .result => {
            const result_t = type_.cast(.result);
            try declareType(c, result_t.child_t, w);

            try w.print("typedef struct {f} {{\n", .{ c.typeName(type_) });
            try w.writeAll("\tu64 tag;\n");
            try w.writeAll("\tunion {\n");
            try w.print("\t\t{f} result;\n", .{ c.typeName(result_t.child_t) });
            try w.print("\t\t{f} error;\n", .{ c.typeName(c.sema.error_t) });
            try w.writeAll("\t} payload;\n");
            try w.print("}} {f};\n", .{ c.typeName(type_) });
        },
        .never,
        .void => {
            try w.print("typedef i64 {f};\n", .{ c.typeName(type_) });
        },
        .enum_t => {
            try w.print("typedef i64 {f};\n", .{ c.typeName(type_) });
        },
        .choice => {
            const choice_t = type_.cast(.choice);
            for (choice_t.cases()) |case| {
                try declareType(c, case.payload_t, w);
            }
            try w.print("typedef struct {f} {{\n", .{ c.typeName(type_) });
            try w.writeAll("    u64 tag;\n");
            try w.writeAll("    union {\n");
            for (choice_t.cases()) |*case| {
                try w.print("        {f} {s};\n", .{ c.typeName(case.payload_t), c.caseName(case.name()) });
            }
            try w.writeAll("    } payload;\n");
            try w.print("}} {f};\n", .{ c.typeName(type_) });
        },
        else => {
            const name = try c.sema.allocTypeName(type_);
            std.debug.panic("Unsupported: {s}", .{name});
        },
    }
}

fn genChunk(c: *Chunk) !void {
    genChunkInner(c) catch |err| {
        if (err != error.CompileError) {
            // Wrap all other errors as a CompileError.
            return c.base.reportErrorFmt("error.{}", &.{v(err)}, c.errNode);
        } else return err;
    };
}

fn genChunkInner(c: *Chunk) !void {
    for (c.ir.func_blocks.items) |block| {
        if (block.code == .funcBlock and !block.cast(.funcBlock).func.info.gen) {
            continue;
        }
        try genStmt(c, block);
    }
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
    errdefer if (c.errNode == null) { c.errNode = node; };

    if (cy.Trace) {
        const contextStr = c.encoder.format(node);
        log.log("--{f}{s}: {s} {*}", .{
            cy.fmt.Repeat{.s=" ", .count=c.indent * 4}, @tagName(stmt.code), contextStr, stmt,
        });
    }
    switch (stmt.code) {
        .await_             => try genAwait(c, stmt.cast(.await_), node),
        .block              => try genBlock(c, stmt.cast(.block), node),
        .break_             => try genBreak(c, stmt.cast(.break_), node),
        .continue_          => try genContinue(c, stmt.cast(.continue_), node),
        .declare_local      => try genDeclareLocal(c, stmt.cast(.declare_local), node),
        .forRangeStmt       => try genForRangeStmt(c, stmt.cast(.forRangeStmt), node),
        .funcBlock          => try genFuncBlock(c, stmt.cast(.funcBlock), node),
        .if_block           => try genIfBlock(c, stmt.cast(.if_block), node),
        .loop_block         => try genLoopBlock(c, stmt.cast(.loop_block), node),
        .mainBlock          => try mainBlock(c, stmt.cast(.mainBlock), node),
        .nop_label          => try genNopLabel(c, stmt.cast(.nop_label), node),
        .ret_expr           => try genRetExpr(c, stmt.cast(.ret_expr), node),
        .ret                => try genRet(c, stmt.cast(.ret), node),
        .set_deref          => try genSetDeref(c, stmt.cast(.set_deref), node),
        .set_field          => try genSetField(c, stmt.cast(.set_field), node),
        .set_local          => try genSetLocal(c, stmt.cast(.set_local), node),
        .set_ret            => try genSetReturn(c, stmt.cast(.set_ret), node),
        .set_global         => try genSetStatic(c, stmt.cast(.set_global), node),
        .switch_stmt        => try genSwitchStmt(c, stmt.cast(.switch_stmt), node),
        .release            => try genRelease(c, stmt.cast(.release), node),
        .verbose            => {
            if (cy.Trace and !cc.verbose()) {
                cc.setVerbose(true);
                c.proc.resetVerboseOnEnd = true;
            }
        },
        else => {
            std.debug.panic("TODO: {}", .{stmt.code});
        }
    }

    if (cy.Trace) {
        log.log("--{f}{s}: end", .{
            cy.fmt.Repeat{.s=" ", .count=c.indent * 4}, @tagName(stmt.code), 
        });
    }
}

fn genExpr(c: *Chunk, expr: *ir.Expr, cstr: Cstr) anyerror!GenValue {
    const node = expr.node;
    if (cy.Trace) {
        const contextStr = c.encoder.format(node);
        log.tracev("{s}: {{{s}}}", .{@tagName(expr.code), contextStr});
    }

    const res = try switch (expr.code) {
        .add                => genBinOpU64(c, expr.cast(.add), " + ", cstr, node),
        .address_of         => genAddressOf(c, expr.cast(.address_of), cstr, node),
        .and_op             => genBinOp2(c, expr.cast(.and_op), " & ", cstr, node),
        .binary_op          => genBinOp(c, expr.cast(.binary_op), cstr, .{}, node),
        .bitcast            => genBitcast(c, expr.cast(.bitcast), cstr, node),
        .call               => genCall(c, expr.cast(.call), cstr, node),
        .call_ptr           => genCallPtr(c, expr.cast(.call_ptr), cstr, node),
        .call_trait         => genCallTrait(c, expr.cast(.call_trait), cstr, node),
        .call_union         => genCallUnion(c, expr.cast(.call_union), cstr, node),
        .captured           => genCaptured(c, expr.cast(.captured), cstr, node),
        .case               => genCase(c, expr.cast(.case), cstr, node),
        .cast               => genCast(c, expr.cast(.cast), cstr, node),
        // .coinitCall         => genCoinitCall(c, idx, cstr, node),
        // .coresume           => genCoresume(c, idx, cstr, node),
        // .coyield            => genCoyield(c, idx, cstr, node),
        .cmp                => genCompare(c, expr.cast(.cmp), cstr, node),
        .const8             => genConst8(c, expr.cast(.const8), cstr, node),
        .const32            => genConst32(c, expr.cast(.const32), cstr, node),
        .const64            => genConst64(c, expr.cast(.const64), cstr, node),
        .deref              => genDeref(c, expr.cast(.deref), cstr, node),
        .div                => genBinOp2(c, expr.cast(.div), " / ", cstr, node),
        .f2i                => genFloatToInt(c, expr.cast(.f2i), cstr, node),
        .fabs               => genFloatAbs(c, expr.cast(.fabs), cstr, node),
        .fadd               => genBinOp2(c, expr.cast(.fadd), " + ", cstr, node),
        .falsev             => genFalse(c, cstr, node),
        .fdiv               => genBinOp2(c, expr.cast(.fdiv), " / ", cstr, node),
        .field              => genField(c, expr.cast(.field), cstr, node),
        .fmul               => genBinOp2(c, expr.cast(.fmul), " * ", cstr, node),
        .fneg               => genFloatNeg(c, expr.cast(.fneg), cstr, node),
        .fsub               => genBinOp2(c, expr.cast(.fsub), " - ", cstr, node),
        .func_ptr           => genFuncPtr(c, expr.cast(.func_ptr), cstr, node),
        .func               => genFuncUnion(c, expr.cast(.func), cstr, node),
        .init_case          => genInitCase(c, expr.cast(.init_case), cstr, node),
        .i2f                => genIntToFloat(c, expr.cast(.i2f), cstr, node),
        .is_zero            => genIsZero(c, expr.cast(.is_zero), cstr, node),
        .lambda             => genLambda(c, expr.cast(.lambda), cstr, node),
        .lift               => genLift(c, expr.cast(.lift), cstr, node),
        .lsl                => genBinOp2(c, expr.cast(.lsl), " << ", cstr, node),
        .lsr                => genLsr(c, expr.cast(.lsr), cstr, node),
        .local              => genLocal(c, expr.cast(.local), cstr, node),
        .mod                => genBinOp2(c, expr.cast(.mod), " % ", cstr, node),
        .mul                => genBinOp2(c, expr.cast(.mul), " * ", cstr, node),
        .neg                => genNeg(c, expr.cast(.neg), cstr, node),
        .not                => genNot(c, expr.cast(.not), cstr, node),
        .init               => genInit(c, expr.cast(.init), cstr, node),
        .or_op              => genBinOp2(c, expr.cast(.or_op), " | ", cstr, node),
        .retain             => genRetain(c, expr.cast(.retain), cstr, node),
        .sext               => genSignExt(c, expr.cast(.sext), cstr, node),
        .global             => genStatic(c, expr.cast(.global), cstr, node),
        .string             => genString(c, expr.cast(.string), cstr, node),
        .sub                => genBinOpU64(c, expr.cast(.sub), " - ", cstr, node),
        .trait              => genTrait(c, expr.cast(.trait), cstr, node),
        .truev              => genTrue(c, cstr, node),
        .trunc              => genTrunc(c, expr.cast(.trunc), cstr, node),
        // .tryExpr            => genTryExpr(c, idx, cstr, node),
        .unary_op           => genUnOp(c, expr.cast(.unary_op), cstr, node),
        .unwrap_addr        => genUnwrapAddr(c, expr.cast(.unwrap_addr), cstr, node),
        .voidv              => genVoid(c, cstr, node),
        .xor                => genBinOp2(c, expr.cast(.xor), " ^ ", cstr, node),
        .zext               => genZeroExtInt(c, expr.cast(.zext), cstr, node),
        else => {
            log.log("{}", .{expr.code});
            return error.TODO;
        },
    };
    log.tracev("{s}: end", .{@tagName(expr.code)});
    return res;
}

fn genFuncPtr(c: *Chunk, expr: *ir.FuncPtr, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpanFmt("&{s}", .{ c.cFuncName(expr.func) });
    return .{};
}

fn genFuncUnion(c: *Chunk, expr: *ir.Func, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpanFmt("cbi_func_union(ctx, {}, ", .{ expr.base.type.id() });
    _ = try genExpr(c, expr.expr, Cstr.none);
    try c.pushSpan(")");
    return .{};
}

fn genNopLabel(c: *Chunk, stmt: *ir.NopLabel, node: *ast.Node) !void {
    const label = c.ir.buf.items[stmt.label_idx..stmt.label_idx + stmt.label_len];
    log.tracev("nop: {s}", .{label});
    try genNopLabel2(c, label, node);
}

fn genNopLabel2(c: *Chunk, label: []const u8, node: *ast.Node) !void {
    if (!emit_nop) {
        return;
    }
    try c.beginLine(node);
    try c.pushSpanFmt("// nop: {s}\n", .{label});
}

fn mainBlock(c: *Chunk, stmt: *ir.MainBlock, node: *ast.Node) !void {
    log.tracev("main block", .{});

    try c.beginLine(node);
    try c.pushSpan("CB_Object cbi_main_user(CB_Thread* ctx) {\n");
    c.proc.setMain(c.alloc);
    c.pushBlock();

    // try c.proc.locals.resize(c.alloc, stmt.maxLocals);
    // if (cy.Trace) {
    //     // Fill with und tag.
    //     @memset(c.proc.locals.items[0..stmt.maxLocals], .{ .kind = .null, .data = .{.null = {} }});
    // }

    try genStmts(c, stmt.bodyHead);
    try c.pushLine("return NULL;", node);
    c.popBlock();
    try c.pushLineNoMapping("}");
    try c.pushNewLine();
    // try bcgen.popBlock(c);

    // c.buf.mainStackSize = c.getMaxUsedRegisters();
}

fn genDeclareLocal(c: *Chunk, stmt: *ir.DeclareLocal, node: *ast.Node) !void {
    try c.beginLine(node);
    const new_name = try allocLocalName(c, stmt.name());
    try reserveLocal(c, stmt.id, new_name, stmt.decl_t);

    if (stmt.init) |init| {
        if (stmt.decl_t.id() != bt.Void) {
            try c.pushSpanFmt("{f} {s} = ", .{c.typeName(stmt.decl_t), new_name});
            _ = try genExpr(c, init, Cstr.none);
            try c.pushSpan(";\n");
        } else {
            try c.pushSpanFmt("{f} {s};\n", .{c.typeName(stmt.decl_t), new_name});
            try c.beginLine(node);
            _ = try genExpr(c, init, Cstr.none);
            try c.pushSpan(";\n");
        }
    } else {
        try c.pushSpanFmt("{f} {s};\n", .{c.typeName(stmt.decl_t), new_name});
    }
}

fn getBinOpName(op: cy.BinaryExprOp) []const u8 {
    return switch (op) {
        .index => "$index",
        .less => "$infix<",
        .greater => "$infix>",
        .less_equal => "$infix<=",
        .greater_equal => "$infix>=",
        .minus => "$infix-",
        .plus => "$infix+",
        .star => "$infix*",
        .slash => "$infix/",
        .percent => "$infix%",
        .caret => "$prefix~",
        .bitwiseAnd => "$infix&",
        .bitwiseOr => "$infix|",
        .bitwiseXor => "$infix~",
        .bitwiseLeftShift => "$infix<<",
        .bitwiseRightShift => "$infix>>",
        else => @panic("Unsupported op"),
    };
}

fn genCallUnion(c: *Chunk, expr: *ir.CallUnion, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    const union_t = expr.callee.type;

    try c.pushSpanFmt("cb_dispatch_{f}(ctx, ", .{ c.typeName(union_t) });
    _ = try genExpr(c, expr.callee, Cstr.none);
    const args = expr.args[0..expr.nargs];
    for (args) |arg| {
        try c.pushSpan(", ");
        _ = try genExpr(c, arg, Cstr.none);
    }
    try c.pushSpan(")");
    return GenValue{};
}

fn genCallPtr(c: *Chunk, expr: *ir.CallPtr, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("(");
    _ = try genExpr(c, expr.callee, Cstr.none);
    try c.pushSpan(")");
    try c.pushSpan("(ctx");
    const args = expr.args[0..expr.nargs];
    for (args) |arg| {
        try c.pushSpan(", ");
        _ = try genExpr(c, arg, Cstr.none);
    }
    try c.pushSpan(")");
    return GenValue{};
}

fn genCallTrait(c: *Chunk, expr: *ir.CallTrait, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    const func = expr.trait.type.cast(.ref_trait).generic.members()[expr.vtable_idx].func;
    _ = try genExpr(c, expr.trait, Cstr.none);
    try c.pushSpanFmt(".vtable->{s}(ctx, ", .{ func.name() });
    _ = try genExpr(c, expr.trait, Cstr.none);
    try c.pushSpan(".ptr");
    const args = expr.args[1..expr.nargs];
    for (args) |arg| {
        try c.pushSpan(", ");
        _ = try genExpr(c, arg, Cstr.none);
    }
    try c.pushSpan(")");

    return .{};
}

fn genCall(c: *Chunk, expr: *ir.Call, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    const args = expr.args[0..expr.numArgs];
    switch (expr.func.type) {
        .userFunc => {
            try c.pushSpanFmt("{s}(ctx", .{c.cFuncName(expr.func)});
            for (args) |idx| {
                try c.pushSpan(", ");
                _ = try genExpr(c, idx, Cstr.none);
            }
        },
        .hostFunc => {
            try c.pushSpanFmt("{s}(ctx", .{c.cFuncName(expr.func)});
            for (args) |idx| {
                try c.pushSpan(", ");
                _ = try genExpr(c, idx, Cstr.none);
            }
        },
        .extern_ => {
            try c.pushSpanFmt("{s}(", .{c.cFuncName(expr.func)});
            if (args.len > 0) {
                _ = try genExpr(c, args[0], Cstr.none);
                for (args[1..]) |idx| {
                    try c.pushSpan(", ");
                    _ = try genExpr(c, idx, Cstr.none);
                }
            }
        },
        else => return error.TODO,
    }

    try c.pushSpan(")");
    return GenValue{};
}

fn genString(c: *Chunk, expr: *ir.String, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    if (cy.string.isAstring(expr.raw())) {
        try c.pushSpan("cbi_astr_static(ctx, \"");
    } else {
        try c.pushSpan("cbi_ustr_static(ctx, \"");
    }
    try writeEscaped(c.outw, expr.raw());
    try c.pushSpanFmt("\", {})", .{expr.raw().len});
    return GenValue{};
}

fn genSignExt(c: *Chunk, expr: *ir.Widen, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpanFmt("({f})", .{c.typeName(expr.base.type)});
    _ = try genExpr(c, expr.expr, Cstr.none);
    return GenValue{};
}

fn genZeroExtInt(c: *Chunk, expr: *ir.Widen, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpanFmt("({f})", .{c.typeName(expr.base.type)});
    _ = try genExpr(c, expr.expr, Cstr.none);
    return GenValue{};
}

fn genVoid(c: *Chunk, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("0");
    return GenValue{};
}

fn genTrait(c: *Chunk, expr: *ir.Trait, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    const key = VtableKey{ .type = expr.impl_t.id(), .trait = expr.generic_t.base.id() };
    const table_id = c.compiler.vtables.get(key).?;
    try c.pushSpanFmt("({f}){{", .{ c.typeName(expr.trait_t) });
    _ = try genExpr(c, expr.ref, Cstr.none);
    try c.pushSpanFmt(", &vtable{}}}", .{table_id});
    return .{};
}

fn genIsZero(c: *Chunk, expr: *ir.IsZero, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("(");
    _ = try genExpr(c, expr.child, Cstr.none);
    try c.pushSpan(" == 0)");
    return GenValue{};
}

fn genFalse(c: *Chunk, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("false");
    return GenValue{};
}

fn genTrue(c: *Chunk, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("true");
    return GenValue{};
}

fn genTrunc(c: *Chunk, expr: *ir.Trunc, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    if (expr.to_bits == 8) {
        try c.pushSpan("(u8)");
        _ = try genExpr(c, expr.expr, Cstr.none);
    } else {
        try c.pushSpanFmt("({f})", .{c.typeName(expr.base.type)});
        _ = try genExpr(c, expr.expr, Cstr.none);
    }
    return GenValue{};
}

fn genConst64(c: *Chunk, expr: *ir.Const64, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = node;
    _ = cstr;
    switch (expr.base.type.id()) {
        bt.F64 => {
            try c.pushSpanFmt("BITCAST(f64, 0x{x}ULL)", .{expr.val});
        },
        else => {
            try genInt2(c, @bitCast(expr.val));
        },
    }
    return GenValue{};
}

fn genInt2(c: *Chunk, val: i64) !void {
    if (val == std.math.minInt(i64)) {
        try c.pushSpan("(-9223372036854775807LL - 1)");
    } else {
        try c.pushSpanFmt("{}LL", .{val});
    }
}

fn genConst32(c: *Chunk, expr: *ir.Const32, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    switch (expr.base.type.id()) {
        bt.F64 => {
            try c.pushSpanFmt("BITCAST(f32, 0x{x}UL)", .{expr.val});
        },
        else => {
            try c.pushSpanFmt("0x{x}L", .{expr.val});
        },
    }
    return GenValue{};
}

fn allocLocalName(c: *Chunk, name: []const u8) ![]const u8 {
    const res = try c.proc.var_names.getOrPut(c.alloc, name);
    if (res.found_existing) {
        res.value_ptr.* += 1;
        return std.fmt.allocPrint(c.alloc, "v_{s}_{}", .{name, res.value_ptr.*});
    } else {
        res.value_ptr.* = 1;
        return std.fmt.allocPrint(c.alloc, "v_{s}_0", .{name});
    }
}

fn reserveLocal(c: *Chunk, ir_id: u8, name: []const u8, declType: *cy.Type) !void {
    _ = declType;
    c.proc.locals.items[ir_id] = .{ .kind = .local, .data = .{ .local = .{
        .name = name,
    }}};
}

fn genSetDeref(c: *Chunk, stmt: *ir.SetDeref, node: *ast.Node) !void {
    try c.beginLine(node);
    try c.pushSpan("*");
    _ = try genExpr(c, stmt.ptr, Cstr.none);
    try c.pushSpan(" = ");
    _ = try genExpr(c, stmt.right, Cstr.none);
    try c.pushSpan(";\n");
}

fn genSetLocal(c: *Chunk, stmt: *ir.SetLocal, node: *ast.Node) !void {
    const local = c.proc.getLocal(stmt.id);

    try c.beginLine(node);
    if (stmt.right.type.id() != bt.Void) {
        try c.pushSpanFmt("{s} = ", .{local.data.local.name});
        _ = try genExpr(c, stmt.right, Cstr.none);
    }
    try c.pushSpan(";\n");
}

fn genSetReturn(c: *Chunk, stmt: *ir.SetReturn, node: *ast.Node) !void {
    const name = try allocLocalName(c, "ret");
    c.last_ret_name = name;

    try c.beginLine(node);
    try c.pushSpanFmt("{f} {s} = ", .{c.typeName(stmt.right.type), name});
    _ = try genExpr(c, stmt.right, Cstr.none);
    try c.pushSpan(";\n");
}

fn genSetField(c: *Chunk, stmt: *ir.SetField, node: *ast.Node) !void {
    try c.beginLine(node);
    _ = try genExpr(c, stmt.field, Cstr.none);
    try c.pushSpan(" = ");
    _ = try genExpr(c, stmt.right, Cstr.none);
    try c.pushSpan(";\n");
}

fn genLoopBlock(c: *Chunk, stmt: *ir.LoopBlock, node: *ast.Node) !void {
    try c.pushLine("while (true) {", node);
    c.pushBlock();
    try genStmts(c, stmt.body_head);
    try c.pushLine("break;", node);
    c.popBlock();
    try c.pushLineNoMapping("}");
}

fn genForRangeStmt(c: *Chunk, stmt: *ir.ForRangeStmt, node: *ast.Node) !void {
    const counter = try allocLocalName(c, "i");

    try c.beginLine(node);
    try c.pushSpanFmt("for (i64 {s} = ", .{counter});
    _ = try genExpr(c, stmt.start, Cstr.none);
    if (stmt.increment) {
        if (stmt.end_inclusive) {
            try c.pushSpanFmt("; {s} <= ", .{counter});
        } else {
            try c.pushSpanFmt("; {s} < ", .{counter});
        }
    } else {
        if (stmt.end_inclusive) {
            try c.pushSpanFmt("; {s} >= ", .{counter});
        } else {
            try c.pushSpanFmt("; {s} > ", .{counter});
        }
    }
    _ = try genExpr(c, stmt.end, Cstr.none);

    if (stmt.increment) {
        try c.pushSpanFmt("; {s} += 1) {{\n", .{counter});
    } else {
        try c.pushSpanFmt("; {s} -= 1) {{\n", .{counter});
    }

    if (stmt.has_each_local) {
        try reserveLocal(c, stmt.eachLocal, counter, c.sema.i64_t);
    } else {
        c.alloc.free(counter);
    }

    c.pushBlock();
    try genStmts(c, stmt.bodyHead);
    c.popBlock();
    try c.pushLineNoMapping("}");
}

fn genConst8(c: *Chunk, expr: *ir.Const8, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpanFmt("({f}){}", .{ c.typeName(expr.base.type), expr.val});
    return GenValue{};
}

fn genLift(c: *Chunk, expr: *ir.Lift, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpanFmt("({f})cbi_lift(ctx, &", .{ c.typeName(expr.base.type) });
    _ = try genExpr(c, expr.child, Cstr.none);
    const child_t = expr.base.type.cast(.pointer).child_t;
    try c.pushSpanFmt(", {}, sizeof({f}))", .{child_t.id(), c.typeName(child_t)});
    return GenValue{};
}

fn genFloatNeg(c: *Chunk, expr: *ir.UnOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("-(");
    _ = try genExpr(c, expr.expr, Cstr.none);
    try c.pushSpan(")");
    return GenValue{};
}

fn genNeg(c: *Chunk, expr: *ir.UnOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("-(");
    _ = try genExpr(c, expr.expr, Cstr.none);
    try c.pushSpan(")");
    return GenValue{};
}

fn genNot(c: *Chunk, expr: *ir.UnOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("~");
    _ = try genExpr(c, expr.expr, Cstr.none);
    return GenValue{};
}

fn genInit(c: *Chunk, expr: *ir.Init, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    const args = expr.args[0..expr.nargs];
    const val_t = expr.base.type;

    switch (val_t.kind()) {
        .result,
        .option,
        .choice,
        .struct_t => {
            try c.pushSpanFmt("({f}){{", .{c.typeName(val_t)});
            if (args.len > 0) {
                _ = try genExpr(c, args[0], Cstr.none);
                for (args[1..]) |arg| {
                    try c.pushSpan(", ");
                    _ = try genExpr(c, arg, Cstr.none);
                }
            }
            try c.pushSpan("}");
        },
        .vector => {
            try c.pushSpanFmt("({f}){{{{", .{ c.typeName(expr.base.type) });
            if (args.len > 0) {
                _ = try genExpr(c, args[0], Cstr.none);
                for (args[1..]) |arg| {
                    try c.pushSpan(", ");
                    _ = try genExpr(c, arg, Cstr.none);
                }
            }
            try c.pushSpan("}}");
        },
        else => {
            std.debug.panic("TODO: {s}", .{val_t.name()});
        },
    }
    return GenValue{};
}

fn genCase(c: *Chunk, expr: *ir.Case, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    const case = &expr.union_t.cases().?[expr.case];
    _ = try genExpr(c, expr.child, Cstr.none);
    try c.pushSpanFmt(".{s}", .{case.name()});
    return .{};
}

fn genInitCase(c: *Chunk, expr: *ir.InitCase, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    const case = &expr.base.type.cases().?[expr.case];
    try c.pushSpanFmt("{{.{s}=", .{case.name()});
    if (case.payload_t.id() == bt.Void) {
        try c.pushSpan("0");
    } else {
        _ = try genExpr(c, expr.child, Cstr.none);
    }
    try c.pushSpan("}");
    return .{};
}

fn genFloatAbs(c: *Chunk, expr: *ir.UnOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("fabs(");
    _ = try genExpr(c, expr.expr, Cstr.none);
    try c.pushSpan(")");
    return GenValue{};
}

fn genIntToFloat(c: *Chunk, expr: *ir.UnOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("(double)");
    _ = try genExpr(c, expr.expr, Cstr.none);
    return GenValue{};
}

fn genFloatToInt(c: *Chunk, expr: *ir.UnOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("(i64)");
    _ = try genExpr(c, expr.expr, Cstr.none);
    return GenValue{};
}

fn genCompare(c: *Chunk, expr: *ir.Compare, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    _ = try genExpr(c, expr.left, Cstr.none);
    const op = switch (expr.cond) {
        .ule,
        .le => " <= ",
        .ult,
        .lt => " < ",
        .uge,
        .ge => " >= ",
        .ugt,
        .gt => " > ",
        .eq => " == ",
        .ne => " != ",
    };
    try c.pushSpan(op);
    _ = try genExpr(c, expr.right, Cstr.none);
    return GenValue{};
}

fn genDeref(c: *Chunk, expr: *ir.Deref, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("*");
    _ = try genExpr(c, expr.expr, Cstr.none);
    return GenValue{};
}

fn genBitcast(c: *Chunk, expr: *ir.Bitcast, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = node;
    const target_t = expr.base.type;
    if (target_t.canCCast()) {
        if (target_t.isRefLike()) {
            // Wrap in parens to ensure precedence over postfix operators.
            try c.pushSpanFmt("(({f})", .{c.typeName(target_t)});
            _ = try genExpr(c, expr.expr, cstr);
            try c.pushSpan(")");
        } else {
            try c.pushSpanFmt("BITCAST({f}, ", .{c.typeName(target_t)});
            _ = try genExpr(c, expr.expr, cstr);
            try c.pushSpan(")");
        }
    } else {
        try c.pushSpanFmt("*({f}*)&", .{c.typeName(target_t)});
        _ = try genExpr(c, expr.expr, cstr);
    }
    return GenValue{};
}

fn genField(c: *Chunk, expr: *ir.Field, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;

    const rec_t = expr.rec.type;
    var shape_t = rec_t;
    if (rec_t.getRefLikeChild()) |child_t| {
        shape_t = child_t;
    }

    if (shape_t.kind() == .choice and expr.idx == 1) {
        // Cast and deref by payload type.
        try c.pushSpanFmt("*({f}*)&", .{c.typeName(expr.base.type)});
    }

    _ = try genExpr(c, expr.rec, Cstr.none);

    if (rec_t.getRefLikeChild() != null) {
        try c.pushSpan("->");
    } else {
        try c.pushSpan(".");
    }
    switch (shape_t.kind()) {
        .struct_t => {
            const field = &shape_t.cast(.struct_t).fields()[expr.idx];
            try c.pushSpan(c.fieldName(field));
        },
        .option => {
            if (expr.idx == 0) {
                try c.pushSpan("tag");
            } else {
                try c.pushSpan("payload");
            }
        },
        .choice => {
            if (expr.idx == 0) {
                try c.pushSpan("tag");
            } else {
                try c.pushSpan("payload");
            }
        },
        .result => {
            if (expr.idx == 0) {
                try c.pushSpan("tag");
            } else {
                try c.pushSpan("payload");
            }
        },
        else => {
            const field = shape_t.fields().?[expr.idx];
            try c.pushSpan(field.sym.head.name());
        },
    }
    return GenValue{};
}

fn genCast(c: *Chunk, expr: *ir.Cast, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = node;
    const ret_t = expr.base.type;
    if (!expr.isRtCast) {
        return genExpr(c, expr.expr, cstr);
    }

    try c.pushSpanFmt("({f})cbi_cast(ctx, ", .{c.typeName(ret_t)});
    _ = try genExpr(c, expr.expr, Cstr.none);
    if (expr.type.kind() == .pointer) {
        try c.pushSpanFmt(", {})", .{expr.type.cast(.pointer).child_t.id()});
    } else {
        std.debug.panic("Unexpected {s}", .{expr.type.name()});
    }
    return GenValue{};
} 

fn genCaptured(c: *Chunk, expr: *ir.Captured, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    const expr_t = expr.base.type;
    try c.pushSpanFmt("(({f}*)&closure->captures)[{}]", .{ c.typeName(expr_t), expr.idx});
    return GenValue{};
}

fn genLocal(c: *Chunk, expr: *ir.Local, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    log.tracev("local: {}", .{expr.id});
    const local = c.proc.locals.items[expr.id];

    try c.pushSpan(local.data.local.name);
    return GenValue{};
}

fn genLambda(c: *Chunk, expr: *ir.Lambda, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    if (expr.numCaptures == 0) {
        try c.pushSpanFmt("&{s}", .{c.cFuncName(expr.func)});
    } else {
        const func_union_t = expr.base.type;
        try c.pushSpanFmt("cbi_closure(ctx, {}, &{s}, (void*[]){{", .{ func_union_t.id(), c.cFuncName(expr.func)});
        const captures = expr.captures[0..expr.numCaptures];
        var local = c.proc.locals.items[captures[0]];
        try c.pushSpanFmt("(void*){s}", .{local.data.local.name});
        for (captures[1..]) |ir_var| {
            local = c.proc.locals.items[ir_var];
            try c.pushSpanFmt(", (void*){s}", .{local.data.local.name});
        }
        try c.pushSpanFmt("}}, {})", .{expr.numCaptures});
    }
    return GenValue{};
}

pub fn genExternToVmFunc(c: *Chunk, w: anytype, func: *cy.Func, node: *ast.Node) !void {
    _ = node;
    const params = func.sig.params();
    try w.print("{f} {s}(", .{c.typeName(func.sig.ret), c.cFuncName(func)});

    if (params.len > 0) {
        try w.print("{f} p0", .{c.typeName(params[0].get_type())});
        for (params[1..], 1..) |param, i| {
            try w.print(", {f} p{}", .{c.typeName(param.get_type()), i});
        }
    }
    try w.print(") {{\n", .{});

    var reg_size: usize = 0;
    for (func.sig.params()) |param| {
        reg_size += param.get_type().reg_size();
    }
    // TODO: check stack size.

    try w.print("  ZThread* t = host_ensure_thread();\n", .{});

    // Set args for calling back to VM.
    var reg: usize = func.sig.ret.reg_size() + 4;
    for (params, 0..) |param, i| {
        try w.print("  *({f}*)&t->c.fp_end[{}] = p{};\n", .{c.typeName(param.get_type()), reg, i});
        reg += param.get_type().reg_size();
    }

    try w.print("  return *({f}*)host_call_func(t, {}, {});\n", .{
        c.typeName(func.sig.ret),
        func.sig.ret.reg_size(),
        c.compiler.base.genSymMap.get(func).?.extern_func.vm_id,
    });
    try w.print("}}\n", .{});
}

pub extern fn memmove(dst: *anyopaque, src: *anyopaque, num: usize) *anyopaque;

pub fn host_new_obj_init(t: *cy.Thread, id: cy.TypeId, src: [*]u8, size: usize) callconv(.c) cy.Value {
    const obj = t.heap.new_object_undef(id, size) catch cy.fatal();
    const dst: [*]u8 = @ptrCast(obj);
    @memcpy(dst[0..size], src[0..size]);
    return cy.Value.initPtr(obj);
}

pub fn host_ensure_thread() callconv(.c) *cy.Thread {
    const ptr = vmc.cur_thread orelse {
        std.debug.panic("expected existing thread.", .{});
    };
    return @ptrCast(ptr);
}

pub fn host_call_func(t: *cy.Thread, ret_size: usize, func: cy.Value) callconv(.c) ?*anyopaque {
    const func_id: usize = @intCast(func.val);
    const func_ptr = t.c.vm.funcSyms.items[func_id].to_vm_ptr();
    const ret = t.c.fp_end orelse @panic("expected frame end marker");

    return t.callFunc2(ret, @intCast(ret_size), func_ptr) catch |err| {
        // For now, force an exit since the host may decide to ignore a panic.
        // TODO: A better approach might be returning the error but disable the thread until
        //       the error has been acknowledged by the host.
        if (err == error.Panic) {
            t.handleExecResult(vmc.RES_PANIC) catch {};
            const summary = cy.debug.allocLastUserPanicError(t) catch @panic("");
            defer t.alloc.free(summary);
            std.debug.print("{s}", .{summary});
        }
        std.debug.panic("`host_call_func`: {}.", .{err});
    };
}

pub fn gen_extern_func_ptr_wrapper(c: *Chunk, w: anytype, func_ptr: *cy.types.FuncPtr) !void {
    try declareType(c, &func_ptr.base, w);

    // Emit extern wrapper function that calls extern function.
    try w.print("Ret cb_func_ptr{}(ZThread* t) {{\n", .{func_ptr.sig.id});

    if (func_ptr.sig.ret.id() == bt.Void) {
        try w.print("  thread_ret(t, 0);\n", .{});
    } else {
        try w.print("  {f}* ret = thread_ret(t, {});\n", .{c.typeName(func_ptr.sig.ret), func_ptr.sig.ret.reg_size()});
    }

    // Temps.
    try w.print("  {f} ptr = *({f}*)thread_param(t, 1);\n", .{c.typeName(&func_ptr.base), c.typeName(&func_ptr.base)});
    const params = func_ptr.sig.params();
    for (params, 1..) |param, i| {
        const param_t = param.get_type();
        switch (param_t.kind()) {
            else => {
                try w.print("  {f} arg_{} = *({f}*)thread_param(t, {});\n", .{
                    c.typeName(param.get_type()), i,
                    c.typeName(param.get_type()), param.get_type().reg_size()});
            },
        }
    }

    // Gen call.
    try w.print("  ", .{});
    if (func_ptr.sig.ret.id() == bt.Void or func_ptr.sig.ret.id() == bt.Never) {
        try w.print("ptr(", .{});
    } else {
        try w.print("*ret = ptr(", .{});
    }

    // Gen args.
    if (params.len > 0) {
        try w.print("arg_1", .{});
        for (params[1..], 2..) |param, i| {
            _ = param;
            try w.print(", arg_{}", .{i});
        }
    }
    // End of args.
    try w.print(");\n", .{});

    // Gen return.
    try w.print("  return 0;\n", .{});
    try w.print("}}\n", .{});
}

/// If `func` is an extern variant (for variadic), `call_func` is the extern function.
/// Otherwise, `func == call_func`.
pub fn genVmToExternFunc(c: *Chunk, w: anytype, func: *cy.Func, call_func: *cy.Func, node: *ast.Node) !void {
    _ = node;

    const params = func.sig.params();

    const variant = func != call_func;
    if (variant) {
        // Emit extern variant wrapper function that calls extern function.
        try w.print("Ret {s}(ZThread* t) {{\n", .{c.cFuncName(func)});
    } else {
        // Emit extern wrapper function that calls extern function.
        try w.print("Ret CB_{s}(ZThread* t) {{\n", .{c.cFuncName(func)});
    } 

    if (func.sig.ret.id() == bt.Void) {
        try w.print("  thread_ret(t, 0);\n", .{});
    } else {
        try w.print("  {f}* ret = thread_ret(t, {});\n", .{c.typeName(func.sig.ret), func.sig.ret.reg_size()});
    }

    // Temps.
    for (params, 0..) |param, i| {
        switch (param.get_type().kind()) {
            // .struct_t => |struct_t| {
            //     try w.print("  ", .{});
            //     const arg_name = try std.fmt.bufPrint(&buf, "arg_{}", .{i});
            //     try writeNamedCType(w, param, arg_name);
            //     try w.print(";\n", .{});
            //     try w.print("  Value* src_{} = (Value*)(cy_get_value(vm, {}) & ~PointerMask) + 1;\n", .{i, i});
            //     try w.print("  arg_{} = toStruct{}(vm, src_{});\n", .{i, struct_t.id(), i});
            // },
            // .arr => |arr| {
            //     const elem = arr.child.*;
            //     const elemName = try fmtBaseTypeName(&cy.tempBuf, elem);

            //     try w.print("  ", .{});
            //     const arg_name = try std.fmt.bufPrint(&buf, "arg_{}", .{i});
            //     try writeNamedCType(w, param, arg_name);
            //     try w.print(";\n", .{});

            //     try w.print("  Value* src_{} = (Value*)(cy_get_value(vm, {}) & ~PointerMask) + 1;\n", .{i, i});
            //     try w.print("  writeToArray{s}(vm, &arg_{}[0], src_{}, {});\n", .{elemName, i, i, arr.n});
            // },
            // .sym => {
            //     try w.print("  Value param_{} = cy_get_value(vm, {});\n", .{i, i});
            //     try w.print("  ", .{});
            //     try writeCType(w, param);
            //     try w.print(" arg_{} = ", .{i});
            //     const param_val = try std.fmt.bufPrint(&buf, "param_{}", .{i});
            //     try writeToCValue(w, param_val, param, false);
            //     try w.print(";\n", .{});
            // },
            else => {
                try w.print("  {f} arg_{} = *({f}*)thread_param(t, {});\n", .{
                    c.typeName(param.get_type()), i,
                    c.typeName(param.get_type()), param.get_type().reg_size()});
            },
        }
    }

    // Gen call.
    try w.print("  ", .{});
    if (func.sig.ret.id() == bt.Void or func.sig.ret.id() == bt.Never) {
        try w.print("{s}(", .{c.cFuncName(func)});
    } else {
        try w.print("*ret = {s}(", .{c.cFuncName(call_func)});
    }

    // Gen args.
    if (params.len > 0) {
        try w.print("arg_0", .{});
        for (params[1..], 1..) |param, i| {
            _ = param;
            try w.print(", arg_{}", .{i});
        }
    }
    // End of args.
    try w.print(");\n", .{});

    // Gen return.
    try w.print("  return 0;\n", .{});
    try w.print("}}\n", .{});
}

/// From C value to `Value`.
fn writeFromCValue(w: anytype, cval: []const u8, type_: *cy.Type) !void {
    switch (type_.id()) {
        bt.F32 => {
            try w.print("*(u32*)&{s}", .{cval});
        },
        bt.F64 => {
            try w.print("*(Value*)&{s}", .{cval});
        },
        bt.Bool,
        bt.R8,
        bt.R16,
        bt.R32,
        bt.R64,
        bt.I8,
        bt.I16,
        bt.I32,
        bt.I64 => {
            try w.print("{s}", .{cval});
        },
        bt.Void => {
            try w.print("0", .{});
        },
        bt.Never => {
            try w.print("0", .{});
        },
        else => {
            switch (type_.kind()) {
                .struct_t => {
                    try w.print("host_new_obj_init(t, {}, (u8*)&{s}, {})", .{ type_.id(), cval, type_.size() });
                    return;
                },
                // .arr => |arr| {
                //     const elemName = try fmtBaseTypeName(&cy.tempBuf, arr.child.*);
                //     try w.print("newFromArray{s}(vm, {s}, {})", .{elemName, cval, arr.n});
                // },
                // .sym => |sym| {
                //     try writeToCyValueForSym(w, cval, sym, must_box);
                // },
                .pointer => {
                    const pointer = type_.cast(.pointer);
                    std.debug.assert(!pointer.ref);
                    try w.print("(Value){s}", .{cval});
                    return;
                },
                .func_ptr => {
                    const func_ptr = type_.cast(.func_ptr);
                    if (func_ptr.sig.extern_) {
                        try w.print("(Value){s}", .{cval});
                        return;
                    }
                },
                else => {},
            }
            std.debug.print("Unsupported arg type: {s}, typeid={}\n", .{ type_.name(), type_.id() });
            return error.InvalidArgument;
        },
    }
}

/// From `Value` to C value. 
fn writeToCValue(c: *Chunk, w: anytype, val: []const u8, type_: *cy.Type) !void {
    switch (type_.id()) {
        bt.Bool => {
            try w.print("*(bool*)&{s}", .{val});
        },
        bt.R8,
        bt.I8 => {
            try w.print("*(i8*)&{s}", .{val});
        },
        bt.R16,
        bt.I16 => {
            try w.print("*(i16*)&{s}", .{val});
        },
        bt.R32,
        bt.I32 => {
            try w.print("*(i32*)&{s}", .{val});
        },
        bt.R64,
        bt.I64 => {
            try w.print("*(i64*)&{s}", .{val});
        },
        bt.F32 => {
            try w.print("*(f32*)&{s}", .{val});
        },
        bt.F64 => {
            try w.print("*(f64*)&{s}", .{val});
        },
        else => {
            switch (type_.kind()) {
                .struct_t => {
                    try w.print("*({f}*){s}", .{c.typeName(type_), val});
                },
                // .sym => |sym| {
                //     try writeToCValueForSym(w, val, sym, unbox);
                // },
                .pointer => {
                    const pointer = type_.cast(.pointer);
                    std.debug.assert(!pointer.ref);
                    try w.print("({f}){s}", .{c.typeName(type_), val});
                },
                .func_ptr => {
                    try w.print("({f}){s}", .{c.typeName(type_), val});
                },
                .vector => {
                    const vector_t = type_.cast(.vector);
                    try w.print("*({f}*[{}]){s}", .{c.typeName(vector_t.elem_t), vector_t.n, val});
                },
                else => {
                    std.debug.print("Unsupported arg type: {s} {}\n", .{ type_.name(), type_.id() });
                    return error.InvalidArgument;
                }
            }
        },
    }
}

fn genFuncBlock(c: *Chunk, stmt: *ir.FuncBlock, node: *ast.Node) !void {
    const func = stmt.func;
    const params = stmt.params[0..func.sig.params_len];

    // try pushFuncBlock(c, data, params, node);

    try c.beginLine(node);
    if (func.type == .userLambda and func.data.userLambda.is_closure) {
        try c.pushSpanFmt("{f} {s}(CB_Thread* ctx, CB_FuncUnion* closure", .{ c.typeNameRet(func.sig.ret), c.cFuncName(func) });
    } else {
        try c.pushSpanFmt("{f} {s}(CB_Thread* ctx", .{ c.typeNameRet(func.sig.ret), c.cFuncName(func) });
    }
    c.proc.setFunc(c.alloc, func);
    c.pushBlock();

    const start = c.proc.locals.items.len;
    // try c.proc.locals.resize(c.alloc, start + stmt.maxLocals);
    // if (cy.Trace) {
    //     // Fill with und tag.
    //     @memset(c.proc.locals.items[start..], .{ .kind = .null, .data = .{.null = {} }});
    // }

    // Reserve func params.
    for (params, 0..) |param, i| {
        const new_name = try allocLocalName(c, param.name());
        c.proc.locals.items[start + i] = .{ .kind = .local, .data = .{
            .local = .{
                .name = new_name,
            },
        }};
        try c.pushSpanFmt(", {f} {s}", .{ c.typeName(param.declType), new_name});
        log.tracev("reserve param: {}", .{i});
    }

    try c.pushSpan(") {\n");

    try genStmts(c, stmt.bodyHead);

    // // Get stack size.
    // const stackSize = c.getMaxUsedRegisters();

    // // Patch empty func sym slot.
    // const rtId = c.compiler.genSymMap.get(func).?.funcSym.id;
    // const rtFunc = rt.FuncSymbol.initFunc(funcPc, stackSize, func.numParams, func.funcSigId, func.reqCallTypeCheck);
    // c.compiler.vm.funcSyms.buf[rtId] = rtFunc;

    // // Add method entry.
    // if (func.isMethod) {
    //     const mgId = try c.compiler.vm.ensureMethodGroup(func.name());
    //     const funcSig = c.compiler.sema.getFuncSig(func.funcSigId);
    //     if (funcSig.reqCallTypeCheck) {
    //         const m = rt.MethodInit.initTyped(func.funcSigId, funcPc, stackSize, func.numParams);
    //         try c.compiler.vm.addMethod(data.parentType, mgId, m);
    //     } else {
    //         const m = rt.MethodInit.initUntyped(func.funcSigId, funcPc, stackSize, func.numParams);
    //         try c.compiler.vm.addMethod(data.parentType, mgId, m);
    //     }
    // }

    c.popBlock();
    try c.pushLineNoMapping("}");
    try c.pushNewLine();
}

fn genBlock(c: *Chunk, stmt: *ir.Block, node: *ast.Node) !void {
    _ = node;
    c.pushBlock();
    try genStmts(c, stmt.bodyHead);
    c.popBlock();
}

fn genBreak(c: *Chunk, stmt: *ir.Break, node: *ast.Node) !void {
    _ = stmt;
    // // Release from startLocal of the first parent loop block.
    // var idx = c.blocks.items.len-1;
    // while (true) {
    //     const b = c.blocks.items[idx];
    //     if (b.isLoopBlock) {
    //         try genReleaseLocals(c, b.nextLocalReg, node);
    //         break;
    //     }
    //     idx -= 1;
    // }

    try c.pushLine("break;", node);
}

fn genContinue(c: *Chunk, stmt: *ir.Continue, node: *ast.Node) !void {
    _ = stmt;
    // // Release from startLocal of the first parent loop block.
    // var idx = c.blocks.items.len-1;
    // while (true) {
    //     const b = c.blocks.items[idx];
    //     if (b.isLoopBlock) {
    //         try genDestructSlots(c, c.cur_proc.slot_start, b.slot_off, node);
    //         break;
    //     }
    //     idx -= 1;
    // }
    try c.pushLine("continue;", node);
}

fn genRelease(c: *Chunk, stmt: *ir.Release, node: *ast.Node) !void {
    const local = c.proc.getLocal(stmt.reg);

    try c.beginLine(node);
    if (stmt.optional) {
        try c.pushSpanFmt("cb_release_opt(ctx, {s});\n", .{local.data.local.name});
    } else {
        try c.pushSpanFmt("cb_release(ctx, {s});\n", .{local.data.local.name});
    }
}

fn genSetStatic(c: *Chunk, stmt: *ir.SetGlobal, node: *ast.Node) !void {
    const var_sym = c.syms.get(stmt.sym).?;
    try c.beginLine(node);
    try c.pushSpanFmt("{s} = ", .{var_sym.name()});
    _ = try genExpr(c, stmt.expr, Cstr.none);
    try c.pushSpan(";\n");
}

fn genRetain(c: *Chunk, expr: *ir.Retain, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpanFmt("cbi_retain(ctx, ", .{});
    _ = try genExpr(c, expr.expr, Cstr.none);
    try c.pushSpan(")");
    return GenValue{};
}

fn genStatic(c: *Chunk, expr: *ir.Global, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    const var_sym = c.syms.get(expr.sym).?;
    try c.pushSpan(var_sym.name());
    return GenValue{};
}

fn genSwitchStmt(c: *Chunk, stmt: *ir.SwitchStmt, node: *ast.Node) !void {
    // const ret_t = c.ir.getExprType(loc);

    const cases = stmt.cases[0..stmt.numCases];

    const expr = try allocLocalName(c, "expr");
    defer c.alloc.free(expr);

    const endswitch_name = try allocLocalName(c, "endswitch");

    for (cases) |case| {
        const case_data = case.cast(.switch_case);
        const isElse = case_data.numConds == 0;

        if (!isElse) {
            const conds = case_data.conds[0..case_data.numConds];
            if (conds.len == 1) {
                const cond = conds[0].cast(.case_cond);

                try genStmts(c, cond.body_head);

                try c.beginLine(node);
                try c.pushSpan("if (");
                _ = try genExpr(c, cond.expr, Cstr.none);
                try c.pushSpan(") {\n");

                c.pushBlock();
                try genStmts(c, case_data.body_head);
                try c.beginLine(node);
                try c.pushSpanFmt("goto {s};\n", .{endswitch_name}); 
                c.popBlock();

                try c.beginLine(null);
                try c.pushSpan("}\n");
            } else {
                const case_name = try allocLocalName(c, "case");
                for (conds) |cond_n| {
                    const cond = cond_n.cast(.case_cond);

                    try genStmts(c, cond.body_head);

                    try c.beginLine(node);
                    try c.pushSpan("if (");
                    _ = try genExpr(c, cond.expr, Cstr.none);
                    try c.pushSpanFmt(") goto {s};\n", .{case_name}); 
                }
                // No conditions matched. Skip to end of case.
                try c.beginLine(node);
                try c.pushSpanFmt("goto end{s};\n", .{case_name}); 

                try c.beginLine(node);
                try c.pushSpanFmt("{s}:;\n", .{case_name}); 
                c.pushBlock();
                try genStmts(c, case_data.body_head);
                try c.pushSpanFmt("goto {s};\n", .{endswitch_name}); 
                c.popBlock();
                try c.pushSpanFmt("end{s}:;\n", .{case_name}); 
            }
        } else {
            c.pushBlock();
            try genStmts(c, case_data.body_head);
            c.popBlock();
        }
    }
    try c.pushSpanFmt("{s}:;\n", .{endswitch_name}); 
}

fn genIfBlock(c: *Chunk, stmt: *ir.IfBlock, node: *ast.Node) !void {
    // var cond_nid = stmt.cond_expr.node;
    // _ = cond_nid;

    try c.beginLine(node);
    try c.pushSpan("if (");
    _ = try genExpr(c, stmt.cond_expr, Cstr.init());
    try c.pushSpan(") {\n");

    // // ARC cleanup for true case.
    // if (unwindAndFreeTemp(c, condv)) {
    //     try pushRelease(c, condv.local, condNodeId);
    // }

    c.pushBlock();
    try genStmts(c, stmt.body_head);
    // var has_else_if = false;
    // var endif_name: []const u8 = undefined;
    // if (stmt.else_block != null) {
    //     const else_data = stmt.else_block.?.cast(.else_block);
    //     if (else_data.cond_expr != null) {
    //         endif_name = try allocLocalName(c, "endif");
    //         try c.beginLine(node);
    //         try c.pushSpanFmt("goto {s};\n", .{endif_name});
    //         has_else_if = true;
    //     }
    // }
    c.popBlock();

    // if (stmt.else_block != null) {
    //     var else_opt = stmt.else_block;
    //     while (else_opt) |else_block| {
    //         const else_nid = else_block.node;
    //         const else_data = else_block.cast(.else_block);

    //         if (else_data.cond_expr) |cond_expr| {
    //             cond_nid = cond_expr.node;

    //             // Close previous block.
    //             try c.pushLineNoMapping("}");

    //             try c.beginLine(else_nid);
    //             try c.pushSpan("if (");
    //             _ = try genExpr(c, cond_expr, Cstr.init());
    //             try c.pushSpan(") {\n");

    //             c.pushBlock();
    //             try genStmts(c, else_data.body_head);
    //             if (else_data.else_block != null) {
    //                 const peek_else = else_data.else_block.?.cast(.else_block);
    //                 if (peek_else.cond_expr != null) {
    //                     try c.beginLine(node);
    //                     try c.pushSpanFmt("goto {s};\n", .{endif_name});
    //                 }
    //             }
    //             c.popBlock();
    //             else_opt = else_data.else_block;
    //             continue;
    //         } else {
    //             // Close previous block.
    //             try c.pushLineNoMapping("} else {");
    //             c.pushBlock();
    //             try genStmts(c, else_data.body_head);
    //             c.popBlock();
    //             break;
    //         }
    //     }
    // }
    try c.pushLineNoMapping("}");
    // if (has_else_if) {
    //     try c.beginLine(null);
    //     try c.pushSpanFmt("{s}:;\n", .{endif_name});
    // }
}

fn genUnwrapAddr(c: *Chunk, expr: *ir.UnwrapAddr, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpanFmt("({f})cbi_unwrap_addr(ctx, ", .{ c.typeName(expr.base.type) });
    _ = try genExpr(c, expr.choice, Cstr.none);
    try c.pushSpanFmt(", {})", .{expr.tag});
    return .{};
}

fn genUnOp(c: *Chunk, expr: *ir.UnOp, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    switch (expr.op) {
        .lnot => {
            try c.pushSpan("!");
        },
        .minus => {
            try c.pushSpan("-");
        },
        .bitwiseNot => {
            try c.pushSpan("~");
        },
        else => {
            return c.base.reportErrorFmt("Unsupported op: {}", &.{v(expr.op)}, node);
        }
    }

    _ = try genExpr(c, expr.expr, Cstr.init());
    return GenValue{};
}

fn genAddressOf(c: *Chunk, expr: *ir.AddressOf, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("(&");
    _ = try genExpr(c, expr.expr, Cstr.none);
    try c.pushSpan(")");
    // if (expr.expr.type.kind() == .fixed_array) {
    //     try c.pushSpan(".arr");
    // }
    return GenValue{};
}

fn genBinOpU32(c: *Chunk, expr: *ir.BinOp2, op: []const u8, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("(BITCAST(u32, ");
    _ = try genExpr(c, expr.left, Cstr.init());
    try c.pushSpan(")");
    try c.pushSpan(op);
    try c.pushSpan("BITCAST(u32, ");
    _ = try genExpr(c, expr.right, Cstr.init());
    try c.pushSpan("))");
    return GenValue{};
}

fn genBinOpU64(c: *Chunk, expr: *ir.BinOp2, op: []const u8, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("(BITCAST(u64, ");
    _ = try genExpr(c, expr.left, Cstr.init());
    try c.pushSpan(")");
    try c.pushSpan(op);
    try c.pushSpan("BITCAST(u64, ");
    _ = try genExpr(c, expr.right, Cstr.init());
    try c.pushSpan("))");
    return GenValue{};
}

fn genBinOp2(c: *Chunk, expr: *ir.BinOp2, op: []const u8, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = cstr;
    _ = node;
    try c.pushSpan("(");
    _ = try genExpr(c, expr.left, Cstr.none);
    try c.pushSpan(op);
    _ = try genExpr(c, expr.right, Cstr.none);
    try c.pushSpan(")");
    return GenValue{};
}

fn genAwait(c: *Chunk, stmt: *ir.Await, node: *ast.Node) !void {
    _ = node;
    try c.pushSpan("cbi_await(ctx, ");
    _ = try genExpr(c, stmt.expr, Cstr.none);
    try c.pushSpan(")");
}

fn genLsr(c: *Chunk, expr: *ir.BinOp2, cstr: Cstr, node: *ast.Node) !GenValue {
    switch (expr.base.type.id()) {
        bt.I64 => return genBinOpU64(c, expr, " >> ", cstr, node),
        bt.I32 => return genBinOpU32(c, expr, " >> ", cstr, node),
        bt.I8 => return genBinOp2(c, expr, " >> ", cstr, node),
        else => {
            return error.TODO;
        },
    }
}

const BinOpOptions = struct {
    left: ?GenValue = null,
};

fn genBinOp(c: *Chunk, expr: *ir.BinOp, cstr: Cstr, opts: BinOpOptions, node: *ast.Node) !GenValue {
    _ = cstr;
    // log.tracev("binop {} {}", .{data.op, data.leftT});

    if (expr.op == .and_op) {
        try c.pushSpan("(");
        _ = try genExpr(c, expr.left, Cstr.none);
        try c.pushSpan(" && ");
        _ = try genExpr(c, expr.right, Cstr.none);
        try c.pushSpan(")");
        return GenValue{};
    } else if (expr.op == .or_op) {
        try c.pushSpan("(");
        _ = try genExpr(c, expr.left, Cstr.none);
        try c.pushSpan(" || ");
        _ = try genExpr(c, expr.right, Cstr.none);
        try c.pushSpan(")");
        return GenValue{};
    }

    if (expr.leftT.id() == bt.Str) {
        if (expr.op == .bang_equal) {
            try c.pushSpan("!");
        }
        try c.pushSpan("cb_cmp_str(");
        _ = try genExpr(c, expr.left, Cstr.none);
        try c.pushSpan(", ");
        _ = try genExpr(c, expr.right, Cstr.none);
        try c.pushSpan(")");
        return GenValue{};
    }

    if (expr.leftT.id() == bt.Void) {
        if (expr.op == .bang_equal) {
            try c.pushSpan("false");
        } else {
            try c.pushSpan("true");
        }
        return GenValue{};
    }

    try c.pushSpan("(");

    // Lhs.
    var leftv: GenValue = undefined;
    if (opts.left) |left| {
        leftv = left;
    } else {
        leftv = try genExpr(c, expr.left, Cstr.init());
    }

    switch (expr.op) {
        .index => {
            return error.TODO;
        },
        .greater,
        .greater_equal,
        .less,
        .less_equal,
        .star,
        .slash,
        .percent,
        .pow,
        .plus,
        .minus => {
            if (expr.leftT.id() == bt.F64) {
                if (expr.rightT.id() == bt.F64) {
                    try c.pushSpan(cBinOpLit(expr.op));
                } else {
                    return error.TODO;
                }
            } else if (expr.leftT.id() == bt.I64) {
                if (expr.rightT.id() == bt.I64) {
                    try c.pushSpan(cBinOpLit(expr.op));
                } else {
                    return error.TODO;
                }
            } else return error.Unexpected;
        },
        .equal_equal => {
            switch (expr.leftT.id()) {
                bt.Error,
                bt.F64,
                bt.I8,
                bt.Bool,
                bt.Symbol,
                bt.I32,
                bt.I64 => {
                    try c.pushSpan(" == ");
                },
                else => {
                    switch (expr.leftT.kind()) {
                        .pointer,
                        .enum_t,
                        .func_ptr => {
                            try c.pushSpan(" == ");
                        },
                        else => {
                            std.debug.panic("TODO: {s}", .{expr.leftT.name()});
                        },
                    }
                },
            }
        },
        .bang_equal => {
            switch (expr.leftT.id()) {
                bt.I64 => {
                    try c.pushSpan(" != ");
                },
                else => {
                    std.debug.panic("TODO: {s}", .{expr.leftT.name()});
                },
            }
        },
        else => {
            return c.base.reportErrorFmt("Unsupported op: {}", &.{v(expr.op)}, node);
        },
    }

    // Rhs.
    const rightv = try genExpr(c, expr.right, Cstr.init());
    _ = rightv;

    switch (expr.op) {
        .index => {
            // if (data.leftT.id() == bt.ListDyn) {
            //     try c.bufPush(")");
            // }
        },
        else => {},
    }

    try c.pushSpan(")");
    return GenValue{};
}

fn cBinOpLit(op: cy.BinaryExprOp) []const u8 {
    return switch (op) {
        .less => " < ",
        .greater => " > ",
        .less_equal => " <= ",
        .greater_equal => " >= ",
        .minus => " - ",
        .plus => " + ",
        .slash => " / ",
        .percent => " % ",
        .star => " * ",
        .pow => cy.fatal(),
        else => cy.fatal(),
    };
}

fn genRet(c: *Chunk, stmt: *ir.Return, node: *ast.Node) !void {
    if (stmt.return_value) {
        try c.beginLine(node);
        try c.pushSpanFmt("return {s};\n", .{ c.last_ret_name });
        c.alloc.free(c.last_ret_name);
        c.last_ret_name = "";
    } else {
        if (c.proc.type == .main) {
            try c.pushLine("return NULL;", node);
        } else {
            try c.pushLine("return;", node);
        }
    }
}

fn genRetExpr(c: *Chunk, stmt: *ir.ReturnExpr, node: *ast.Node) !void {
    try c.beginLine(node);

    // TODO: If the returned expr is a local, consume the local after copying to reg 0.
    try c.pushSpan("return ");
    var childv: GenValue = undefined;
    if (c.proc.type == .main) {
        // // Main block.
        // childv = try genExpr(c, data.expr, RegisterCstr.simpleMustRetain);
        return error.TODO;
    } else {
        childv = try genExpr(c, stmt.expr, Cstr.init());
    }
    try c.pushSpan(";\n");

    // _ = unwindAndFreeTemp(c, childv);

    // try genBlockReleaseLocals(c);
    // if (c.curBlock.type == .main) {
    //     try c.buf.pushOp1(.end, @intCast(childv.local));
    // } else {
    //     try c.buf.pushOp(.ret1);
    // }
}

fn writeEscaped(w: anytype, str: []const u8) !void {
    var prev_hex_escape = false;
    for (str) |ch| {
        prev_hex_escape = try writeEscapedChar(w, ch, prev_hex_escape);
    }
}

fn writeEscapedChar(w: anytype, ch: u8, prev_hex_escape: bool) !bool {
    if (ch >= 0x80) {
        try w.print("\\x{x}", .{ch});
        return true;
    } else {
        if (prev_hex_escape) {
            try w.writeAll("\" \"");
        }
    }
    switch (ch) {
        '\n' => {
            try w.writeAll("\\n");
        },
        '\r' => {
            try w.writeAll("\\r");
        },
        '\t' => {
            try w.writeAll("\\t");
        },
        '\\' => {
            try w.writeAll("\\\\");
        },
        '"' => {
            try w.writeAll("\\\"");
        },
        else => {
            try w.writeByte(ch);
        },
    }
    return false;
}

pub fn cStringLit(self: *Chunk, raw: []const u8) ![]const u8 {
    // Big enough to hold escaped C literal.
    try self.base.tempBufU8.resize(self.alloc, raw.len * 2);

    // Escape to C literal.
    const ReplaceChars = "\\\"";
    if (std.mem.indexOfAny(u8, raw, ReplaceChars)) |idx| {
        var fbuf = std.io.fixedBufferStream(self.base.tempBufU8.items[0..]);
        const w = fbuf.writer();
        try w.writeAll(raw[0..idx]);
        try writeEscapedChar(w, raw[idx]);

        var rest = raw[idx + 1 ..];
        while (std.mem.indexOfAny(u8, rest, ReplaceChars)) |idx2| {
            try w.writeAll(rest[0..idx2]);
            try writeEscapedChar(w, rest[idx2]);
            rest = rest[idx2+1..];
        }
        try w.writeAll(rest);
        return fbuf.getWritten();
    } else {
        return raw;
    }
}

fn writeTraitFuncType(c: *Chunk, w: anytype, name: []const u8, sig: *cy.FuncSig) !void {
    try w.print("{f} (*{s})(CB_Thread*, void*", .{ c.typeNameRet(sig.ret), name });
    if (sig.params_len > 1) {
        for (sig.params()[1..]) |param| {
            try w.writeAll(", ");
            try writeTypeName(c, w, param.get_type());
        }
    }
    try w.writeAll(")");
}

fn writeFuncPtrType(c: *Chunk, w: anytype, name: []const u8, sig: *cy.FuncSig) !void {
    if (sig.extern_) {
        try w.print("{f} (*{s})(", .{ c.typeNameRet(sig.ret), name });
        if (sig.params_len > 0) {
            const params = sig.params();
            try writeTypeName(c, w, params[0].get_type());
            for (params[1..]) |param| {
                try w.writeAll(", ");
                try writeTypeName(c, w, param.get_type());
            }
        }
    } else {
        try w.print("{f} (*{s})(CB_Thread*", .{ c.typeNameRet(sig.ret), name });
        if (sig.params_len > 0) {
            for (sig.params()) |param| {
                try w.writeAll(", ");
                try writeTypeName(c, w, param.get_type());
            }
        }
    }
    try w.writeAll(")");
}

fn writeTypeName(c: *Chunk, w: anytype, type_: *cy.Type) !void {
    switch (type_.kind()) {
        .pointer => {
            const child_t = type_.cast(.pointer).child_t;
            if (child_t.id() == bt.Void) {
                try w.writeAll("void*");
            } else {
                try writeTypeName(c, w, type_.cast(.pointer).child_t);
                try w.writeByte('*');
            }
        },
        .borrow => {
            try writeTypeName(c, w, type_.cast(.borrow).child_t);
            try w.writeByte('*');
        },
        else => {
            const name = c.cSymName(@ptrCast(type_.sym()));
            try w.writeAll(name);
        }
    }
}

const TypeName = struct {
    c: *Chunk,
    type: *cy.Type,
    ret: bool = false,

    fn init(c: *Chunk, type_: *cy.Type) TypeName {
        return .{
            .c = c,
            .type = type_,
        };
    }

    pub fn format(self: TypeName, w: *std.Io.Writer) !void {
        if (self.ret) {
            switch (self.type.id()) {
                bt.Void, bt.Never => {
                    try w.writeAll("void");
                },
                else => {
                    try writeTypeName(self.c, w, self.type);
                },
            }
        } else {
            try writeTypeName(self.c, w, self.type);
        }
    }
};

pub fn dumpSource(src: []const u8) !void {
    var iter = std.mem.splitScalar(u8, src, '\n');
    var count: usize = 1;
    while (iter.next()) |line| {
        std.debug.print("{} {s}\n", .{count, line});
        count += 1;
    }
}
