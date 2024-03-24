const std = @import("std");
const builtin = @import("builtin");
const cy = @import("cyber.zig");
const cc = @import("capi.zig");
const rt = cy.rt;
const ir = cy.ir;
const bt = cy.types.BuiltinTypes;
const log = cy.log.scoped(.cgen);
const v = cy.fmt.v;
const tcc = @import("tcc");

const SymbolId = u32;

const Compiler = struct {
    alloc: std.mem.Allocator,
    base: *cy.Compiler,

    /// Can be cy.Sym or cy.Func.
    syms: std.AutoHashMapUnmanaged(*anyopaque, Sym),

    /// Cyber name to normalized C name's next unique suffix.
    c_names: std.StringHashMapUnmanaged(u32),

    pub fn deinit(c: *Compiler) void {
        {
            var iter = c.syms.valueIterator();
            while (iter.next()) |val| {
                c.alloc.free(val.name());
            }
            c.syms.deinit(c.alloc);
        }

        c.c_names.deinit(c.alloc);
    }

    fn overrideSymName(c: *Compiler, chunk: *cy.Chunk, name: []const u8, override: []const u8) !void {
        const sym = chunk.sym.getMod().getSym(name).?;
        if (c.syms.getPtr(sym)) |c_sym| {
            c.alloc.free(c_sym.name());
            const dupe = try c.alloc.dupe(u8, override);
            c_sym.name_ptr = dupe.ptr;
            c_sym.name_len = @intCast(dupe.len);
        }
    }

    fn overrideFuncName(c: *Compiler, chunk: *cy.Chunk, name: []const u8, override: []const u8) !void {
        const sym = chunk.sym.getMod().getSym(name).?.cast(.func).first;
        if (c.syms.getPtr(sym)) |c_sym| {
            c.alloc.free(c_sym.name());
            const dupe = try c.alloc.dupe(u8, override);
            c_sym.name_ptr = dupe.ptr;
            c_sym.name_len = @intCast(dupe.len);
        }
    }

    fn genSymName(c: *Compiler, sym_ptr: *anyopaque, sym_name: []const u8) !void {
        var buf: [1024]u8 = undefined;
        const name_n = try normalizeSymName(&buf, sym_name);

        var new_name: []const u8 = undefined;
        const res = try c.c_names.getOrPut(c.alloc, sym_name);
        if (res.found_existing) {
            new_name = try std.fmt.allocPrint(c.alloc, "{s}_{}", .{name_n, res.value_ptr.*});
            res.value_ptr.* += 1;
        } else {
            new_name = try std.fmt.allocPrint(c.alloc, "{s}_0", .{name_n});
            res.value_ptr.* = 1;
        }

        try c.syms.putNoClobber(c.alloc, sym_ptr, .{
            .name_ptr = new_name.ptr,
            .name_len = @intCast(new_name.len),
            .declared = false,
        });
    }
};

fn normalizeSymName(buf: []u8, name: []const u8) ![]const u8 {
    const ReplaceChars = "&|%^*/+-<>=~";
    if (std.mem.indexOfAny(u8, name, ReplaceChars)) |idx| {
        // Normalize to C name.
        var fbuf = std.io.fixedBufferStream(buf);
        const w = fbuf.writer();
        try w.print("{s}{s}", .{name[0..idx], normalizeChar(name[idx])});

        var rest = name[idx+1..];
        while (std.mem.indexOfAny(u8, rest, ReplaceChars)) |idx2| {
            try w.print("{s}{s}", .{rest[0..idx2], normalizeChar(rest[idx2])});
            rest = rest[idx2+1..];
        }
        try w.writeAll(rest);
        return fbuf.getWritten();
    } else {
        return name;
    }
}

fn normalizeChar(char: u8) []const u8 {
    return switch (char) {
        '&' => "_am",
        '|' => "_vl",
        '^' => "_ca",
        '%' => "_pe",
        '*' => "_as",
        '/' => "_sl",
        '-' => "_hy",
        '+' => "_pl",
        '<' => "_lt",
        '>' => "_gt",
        '=' => "_eq",
        '~' => "_ti",
        else => &.{char},
    };
}

const Sym = struct {
    name_ptr: [*]const u8,
    name_len: u16,

    declared: bool,

    fn name(self: Sym) []const u8 {
        return self.name_ptr[0..self.name_len];
    }
};

const Symbol = struct {
    name: []const u8,
};

const TypeName = struct {
    name: []const u8,
    is_pointer: bool,

    fn init(name_: []const u8) TypeName {
        return .{
            .name = name_,
            .is_pointer = false,
        };
    }

    fn initPtr(name_: []const u8) TypeName {
        return .{
            .name = name_,
            .is_pointer = true,
        };
    }

    pub fn format(self: TypeName, comptime fmt: []const u8, opts: std.fmt.FormatOptions, w: anytype) !void {
        _ = fmt;
        _ = opts;
        try w.writeAll(self.name);
        if (self.is_pointer) {
            try w.writeByte('*');
        }
    }
};

const Chunk = struct {
    alloc: std.mem.Allocator,
    ir: cy.ir.Buffer,
    encoder: cy.ast.Encoder,
    out: std.ArrayListUnmanaged(u8),
    outw: std.ArrayListUnmanaged(u8).Writer,
    buf: std.ArrayListUnmanaged(u8),
    bufw: std.ArrayListUnmanaged(u8).Writer,
    procs: std.ArrayListUnmanaged(Proc),

    /// Alias for `compiler.syms`.
    syms: std.AutoHashMapUnmanaged(*anyopaque, Sym),

    compiler: *Compiler,
    base: *cy.Chunk,
    vm: *cy.VM,
    sema: *cy.Sema,
    errNodeId: cy.NodeId,
    localStack: std.ArrayListUnmanaged(Local),
    tryStack: std.ArrayListUnmanaged(Try),
    indent: u32,
    emitSourceMap: bool,
    ast: cy.ast.AstView,
    srcUri: []const u8,

    fn deinit(c: *Chunk) void {
        c.procs.deinit(c.alloc);
        c.out.deinit(c.alloc);
        c.buf.deinit(c.alloc);
        c.localStack.deinit(c.alloc);
        c.tryStack.deinit(c.alloc);
    }

    fn proc(c: *Chunk) *Proc {
        return &c.procs.items[c.procs.items.len-1];
    }

    fn pushProc(c: *Chunk, b: Proc) !void {
        var b_ = b;
        b_.localStart = @intCast(c.localStack.items.len);
        b_.tryStart = @intCast(c.tryStack.items.len);
        try c.procs.append(c.alloc, b_);
        c.indent += 1;
    }

    fn popProc(c: *Chunk) void {
        _ = c.procs.pop();
        c.indent -= 1;
    }

    fn pushBlock(c: *Chunk) void {
        c.indent += 1;
    }

    fn popBlock(c: *Chunk) void {
        c.indent -= 1;
    }

    fn beginLine(c: *Chunk, nodeId: cy.NodeId) !void {
        if (c.emitSourceMap) {
            try c.pushIndent();
            var line: u32 = undefined;
            var col: u32 = undefined;
            var lineStart: u32 = undefined;
            const node = c.ast.node(nodeId);
            c.ast.computeLinePos(node.srcPos, &line, &col, &lineStart);
            try c.pushSpanFmtEnd("#line {} \"{s}\"", .{line, c.srcUri});
        }
        try c.pushIndent();
    }

    fn pushLine(c: *Chunk, line: []const u8, nodeId: cy.NodeId) !void {
        try c.beginLine(nodeId);
        try c.out.appendSlice(c.alloc, line);
        try c.out.append(c.alloc, '\n');
    }

    fn pushLineNoMapping(c: *Chunk, line: []const u8) !void {
        try c.pushIndent();
        try c.out.appendSlice(c.alloc, line);
        try c.out.append(c.alloc, '\n');
    }

    fn pushNewLine(c: *Chunk) !void {
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

    fn pushSpanFmtEnd(c: *Chunk, comptime format: []const u8, args: anytype) !void {
        try c.outw.print(format, args);
        try c.out.append(c.alloc, '\n');
    }

    fn pushSpanEnd(c: *Chunk, span: []const u8) !void {
        try c.out.appendSlice(c.alloc, span);
        try c.out.append(c.alloc, '\n');
    }

    fn pushSpan(c: *Chunk, span: []const u8) !void {
        try c.out.appendSlice(c.alloc, span);
    }

    fn bufPushTrySpan(c: *Chunk) !void {
        const b = c.proc();
        if (c.tryStack.items.len > b.tryStart) {
            const last = c.tryStack.getLast();
            try c.bufPushFmt("TRY_GOTO({}, ", .{last.suffix});
        } else {
            if (b.type == .main) {
                try c.bufPush("TRY_PANIC(");
            } else {
                try c.bufPush("TRY_RET(");
            }
        }
    }

    fn pushIndent(c: *Chunk) !void {
        for (0..c.indent) |_| {
            try c.out.append(c.alloc, '\t');
        }
    }

    fn cSymName(c: *Chunk, ptr: *anyopaque) []const u8 {
        return c.syms.get(ptr).?.name();
    }

    fn cTypeName(c: *Chunk, id: cy.TypeId) !TypeName {
        return switch (id) {
            // bt.Pointer,
            // bt.Fiber,
            // bt.MetaType,
            bt.Dynamic => TypeName.init("CbAny"),
            // bt.ExternFunc,
            bt.Any => TypeName.init("CbAny"),
            bt.Integer => TypeName.init("i64"),
            bt.Float => TypeName.init("f64"),
            bt.Symbol => TypeName.init("CbSymbol"),
            bt.Void => TypeName.init("void"),
            bt.Error => TypeName.init("CbError"),
            // // bt.Undefined,
            bt.Boolean => TypeName.init("bool"),
            else => {
                if (id > bt.Type) {
                    const sym = c.sema.getTypeSym(id);
                    const name = c.cSymName(sym);
                    if (sym.type == .object_t) {
                        return TypeName.initPtr(name);
                    } else {
                        return TypeName.init(name);
                    }
                } else {
                    rt.printErrorZFmt(c.sema.compiler.vm, "Unsupported sym type: {}\n", .{id});
                    return error.TODO;
                }
            }
        };
    }

    fn bufPushBoxMacro(c: *Chunk, id: cy.TypeId) !void {
        return switch (id) {
            bt.Integer => try c.bufw.writeAll("BOX_INT("),
            else => {
                const type_e = c.sema.types.items[id];
                if (type_e.kind == .object) {
                    const c_name = c.cSymName(type_e.sym);
                    try c.bufPushFmt("BOX_OBJ({s}, ", .{ c_name });
                } else {
                    const name = c.sema.getTypeBaseName(id);
                    return c.base.reportErrorFmt("Unsupported box macro: {}", &.{v(name)}, null);
                }
            },
        };
    }
};

const ProcType = enum {
    main,
    func,
};

const Proc = struct {
    type: ProcType,

    /// Starting `Local` in `localStack`.
    localStart: u32,

    /// Starting `Try` in `tryStack`.
    tryStart: u32,

    resetVerboseOnEnd: bool,

    /// Track the highest `tmp` suffix for user declared vars.
    maxTmpSuffix: u32,

    /// Track the highest try block suffix.
    maxTrySuffix: u32,

    data: union {
        func: *cy.Func,
    },

    fn initMain() Proc {
        return .{
            .type = .main,
            .resetVerboseOnEnd = false,
            .localStart = undefined,
            .tryStart = undefined,
            .maxTmpSuffix = 0,
            .maxTrySuffix = 0,
            .data = undefined,
        };
    }

    fn initFunc(func: *cy.Func) Proc {
        return .{
            .type = .func,
            .resetVerboseOnEnd = false,
            .localStart = undefined,
            .tryStart = undefined,
            .maxTmpSuffix = 0,
            .maxTrySuffix = 0,
            .data = .{
                .func = func,
            },
        };
    }

    fn nextTemp(b: *Proc) u32 {
        b.maxTmpSuffix += 1;
        return b.maxTmpSuffix;
    }
};

const Try = struct {
    suffix: u32,
};

pub const Local = union {
    some: struct {
        name: []const u8,

        /// If `boxed` is true, this refers to the child value.
        /// This is updated by assignments and explicit updateLocalType.
        rcCandidate: bool,

        /// Whether the local is owned by the block. eg. Read-only func params would not be owned.
        owned: bool,

        /// Whether it was lifted to an Indirect value.
        lifted: bool,

        /// The current type of the local. This can change during its lifetime for dynamic vars.
        type: cy.TypeId,
    },
    null: void,
};

const Cstr = struct {
    const none = Cstr{};

    fn init() Cstr {
        return .{};
    }
};

const Value = struct {
};

pub fn gen(self: *cy.Compiler) !cy.compiler.AotCompileResult {
    var compiler = Compiler{
        .base = self,
        .alloc = self.alloc,
        .syms = .{},
        .c_names = .{},
    };
    defer compiler.deinit();

    for (self.chunks.items) |chunk| {
        // Generate unique C name for each symbol.

        for (chunk.syms.items) |sym| {
            switch (sym.type) {
                .chunk => {
                    // Get stem name.
                    // eg. "/a/b/c/d.cy" is reduced to "d"
                    var name = chunk.sym.head.name();
                    if (name[0] == '/') {
                        name = std.fs.path.stem(name);
                    }
                    try compiler.genSymName(sym, name);
                },
                .bool_t,
                .custom_object_t,
                .object_t => {
                    try compiler.genSymName(sym, sym.name());
                },
                else => {},
            }
        }

        for (chunk.funcs.items) |func| {
            try compiler.genSymName(func, func.name());
        }
    }

    // Sym overrides to match rt library.
    const builtins_c = self.chunkMap.get("builtins").?;
    try compiler.overrideSymName(builtins_c, "int", "CbInt");
    try compiler.overrideSymName(builtins_c, "bool", "CbBool");
    try compiler.overrideSymName(builtins_c, "none", "CbNone");
    try compiler.overrideSymName(builtins_c, "error", "CbError");
    try compiler.overrideSymName(builtins_c, "placeholder1", "CbPlaceholder1");
    try compiler.overrideSymName(builtins_c, "placeholder2", "CbPlaceholder2");
    try compiler.overrideSymName(builtins_c, "placeholder3", "CbPlaceholder3");
    try compiler.overrideSymName(builtins_c, "symbol", "CbSymbol");
    try compiler.overrideSymName(builtins_c, "float", "CbFloat");
    try compiler.overrideSymName(builtins_c, "dynamic", "CbDynamic");
    try compiler.overrideSymName(builtins_c, "any", "CbAny");
    try compiler.overrideSymName(builtins_c, "type", "CbType");
    try compiler.overrideFuncName(builtins_c, "print", "cb_print");
    if (self.chunkMap.get("test")) |chunk| {
        try compiler.overrideFuncName(chunk, "eq", "cb_test_eq");
    }

    var chunks = try self.alloc.alloc(Chunk, self.chunks.items.len);
    for (self.chunks.items, 0..) |chunk, i| {
        chunks[i] = .{
            .alloc = self.alloc,
            .ir = chunk.ir,
            .encoder = chunk.encoder,
            .out = .{},
            .outw = undefined,
            .buf = .{},
            .bufw = undefined,
            .syms = compiler.syms,
            .procs = .{},
            .base = chunk,
            .vm = chunk.vm,
            .sema = chunk.sema,
            .localStack = .{},
            .tryStack = .{},
            .compiler = &compiler,
            .errNodeId = cy.NullId,
            .indent = 0,
            .emitSourceMap = self.config.emit_source_map,
            .ast = chunk.ast,
            .srcUri = chunk.srcUri,
        };
        chunks[i].outw = chunks[i].out.writer(self.alloc);
        chunks[i].bufw = chunks[i].buf.writer(self.alloc);
    }
    defer {
        for (chunks) |*chunk| {
            chunk.deinit();
        }
        self.alloc.free(chunks);
    }

    for (self.chunks.items, 0..) |chunk, i| {
        log.tracev("Perform codegen for chunk{}: {s}", .{chunk.id, chunk.srcUri});
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
    const outPath = try std.fmt.allocPrintZ(self.alloc, "out/{s}.c", .{outName});
    defer self.alloc.free(outPath);

    const outFile = try std.fs.cwd().createFile(outPath, .{ .truncate = true });
    defer outFile.close();

    // Generate head at the end since it relies on chunk passes.
    var head: std.ArrayListUnmanaged(u8) = .{};
    const headw = head.writer(self.alloc);
    try genHead(&compiler, headw, chunks);
    defer head.deinit(self.alloc);

    try outFile.writeAll(head.items);

    for (chunks) |chunk| {
        try outFile.writeAll(chunk.out.items);
    }

    var exePath: [:0]const u8 = undefined;
    const stemName = std.fs.path.stem(outName);
    if (builtin.os.tag == .windows) {
        exePath = try std.fmt.allocPrintZ(self.alloc, "out/{s}.exe", .{stemName});
    } else {
        exePath = try std.fmt.allocPrintZ(self.alloc, "out/{s}", .{stemName});
    }
    errdefer self.alloc.free(exePath); 

    if (self.config.backend == cc.BackendTCC) {
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
        var res = try std.ChildProcess.exec(.{
            .allocator = self.alloc,
            .argv = &.{"clang", "-o", exePath, outPath, "zig-out/lib/librt.a"},
        });
        defer self.alloc.free(res.stderr);
        defer self.alloc.free(res.stdout);

        if (res.term != .Exited or res.term.Exited != 0) {
            rt.printError(self.vm, res.stderr);
            return error.CCError;
        }
    }
    return .{
        .exePath = exePath,
    };
}

fn genHead(c: *Compiler, w: std.ArrayListUnmanaged(u8).Writer, chunks: []Chunk) !void {
    _ = c;
    const head = @embedFile("pm.h");
    try w.writeAll(head);

    // Predefines.
    try w.writeAll(
        \\CbRT cb_rt;
        \\int cb_panic() {
        \\    // TODO: Dump stack trace
        \\    fprintf(stderr, "panic.\n");
        \\    exit(1);
        \\}
        \\int cb_panic_msg(const char* msg) {
        \\    fprintf(stderr, "panic: %s\n", msg);
        \\    exit(1);
        \\}
        \\
        \\
    );

    // Forward declarations.
    // Gen recursively since C cannot declare struct fields with incomplete types.
    for (chunks) |*chunk| {
        const base = chunk.base;

        for (base.syms.items) |sym| {
            switch (sym.type) {
                .bool_t => {
                    const c_name = chunk.cSymName(sym);
                    const name = try cStringLit(chunk, sym.name());
                    try w.print(
                        \\CbTypeTable {s}TypeTable = {{
                        \\    .size = sizeof(bool),
                        \\    .name = "{s}",
                        \\    .toPrintString = 0,
                        \\    .kind = CbTypeStruct,
                        \\}};
                        \\
                    , .{ c_name, name });
                },
                .object_t => {
                    try genObjectDecl(chunk, sym, w);
                },
                else => {},
            }
        }

        for (base.funcs.items) |func| {
            if (func.type == .userLambda) {
                continue;
            }
            if (func.type == .hostFunc) {
                try w.print("extern {} {s}(CbRT*", .{
                    try chunk.cTypeName(func.retType), chunk.cSymName(func),
                });
                const params = base.sema.getFuncSig(func.funcSigId).params();
                if (params.len > 0) {
                    for (params) |param| {
                        try w.print(", {}", .{ try chunk.cTypeName(param) });
                    }
                }
                try w.writeAll(");\n");
            } else {
                try w.print("{} {s}(CbRT* rt", .{
                    try chunk.cTypeName(func.retType), chunk.cSymName(func),
                });
                const funcSig = base.sema.getFuncSig(func.funcSigId);
                const params = funcSig.params();
                if (params.len > 0) {
                    for (params) |param| {
                        try w.print(", {}", .{ try chunk.cTypeName(param) });
                    }
                }
                try w.writeAll(");\n");
            }
        }
    }
    try w.writeByte('\n');
}

fn genObjectDecl(c: *Chunk, sym: *cy.Sym, w: std.ArrayListUnmanaged(u8).Writer) !void {
    const c_sym = c.syms.getPtr(sym).?;
    if (c_sym.declared) {
        return;
    }
    c_sym.declared = true;

    const object_t = sym.cast(.object_t);
    for (object_t.getFields()) |field| {
        if (field.sym.type == .struct_t) {
        } else if (field.sym.type == .object_t) {
            try genObjectDecl(c, field.sym, w);
        }
    }

    const c_name = c.cSymName(sym);
    try w.print("typedef struct {s} {{\n", .{ c_name });
    for (object_t.getFields()) |field| {
        try w.print("    {} {s};\n", .{
            try c.cTypeName(field.type), field.sym.name(),
        });
    }
    try w.print("}} {s};\n", .{ c_name });

    const name = try cStringLit(c, sym.name());
    try w.print(
        \\CbTypeTable {s}TypeTable = {{
        \\    .size = sizeof(uintptr_t),
        \\    .name = "{s}",
        \\    .toPrintString = 0,
        \\    .kind = CbTypeObject,
        \\}};
        \\
        , .{ c_name, name },
    );
    try w.print(
        \\{s}* new{s}(CbRT* rt, {s} init) {{
        \\    {s}* new;
        \\    if (sizeof({s}) <= CB_MAX_POOL_OBJECT_SIZE) {{
        \\        {s}* new = ({s}*)cbNewPoolObj(rt);
        \\    }} else {{
        \\        {s}* new = ({s}*)cbNewObj(rt, sizeof({s}));
        \\    }}
        \\    *new = init;
        \\    return new;
        \\}}
        \\
        \\
        , .{ c_name, c_name, c_name, c_name, c_name, c_name, c_name, c_name, c_name, c_name },
    );
}

fn genChunk(c: *Chunk) !void {
    genChunkInner(c) catch |err| {
        if (err != error.CompileError) {
            // Wrap all other errors as a CompileError.
            return c.base.reportErrorFmt("error.{}", &.{v(err)}, c.errNodeId);
        } else return err;
    };
}

fn genChunkInner(c: *Chunk) !void {
    const code = c.ir.getStmtCode(0);
    if (code != .root) return error.Unexpected;

    const data = c.ir.getStmtData(0, .root);
    try genStmts(c, data.bodyHead);
}

fn genStmts(c: *Chunk, idx: u32) !void {
    var stmt = idx;
    while (stmt != cy.NullId) {
        try genStmt(c, stmt);
        stmt = c.ir.getStmtNext(stmt);
    }
}

fn genStmt(c: *Chunk, loc: u32) anyerror!void {
    const code = c.ir.getStmtCode(loc);
    const nodeId = c.ir.getNode(loc);
    errdefer if (c.errNodeId == cy.NullId) { c.errNodeId = nodeId; };

    if (cy.Trace) {
        const contextStr = try c.encoder.format(nodeId, &cy.tempBuf);
        log.tracev("----{s}: {{{s}}}", .{@tagName(code), contextStr});
    }
    switch (code) {
        .breakStmt          => try breakStmt(c, nodeId),
        // .contStmt           => try contStmt(c, nodeId),
        .declareLocal       => try declareLocal(c, loc, nodeId),
        .declareLocalInit   => try declareLocalInit(c, loc, nodeId),
        // .destrElemsStmt     => try destrElemsStmt(c, idx, nodeId),
        .exprStmt           => try exprStmt(c, loc, nodeId),
        // .forIterStmt        => try forIterStmt(c, idx, nodeId),
        .forRangeStmt       => try forRangeStmt(c, loc, nodeId),
        .funcBlock          => try funcBlock(c, loc, nodeId),
        .ifStmt             => try ifStmt(c, loc, nodeId),
        .loopStmt           => try loopStmt(c, loc, nodeId),
        .mainBlock          => try mainBlock(c, loc, nodeId),
        .opSet              => try opSet(c, loc, nodeId),
        // .pushDebugLabel     => try pushDebugLabel(c, idx),
        .retExprStmt        => try retExprStmt(c, loc, nodeId),
        // .retStmt            => try retStmt(c),
        .setCallObjSymTern  => try setCallObjSymTern(c, loc, nodeId),
        // .setCaptured        => try setCaptured(c, idx, nodeId),
        .setField           => try setField(c, loc, nodeId),
        // .setFuncSym         => try setFuncSym(c, idx, nodeId),
        .setIndex           => try setIndex(c, loc, nodeId),
        .setLocal           => try setLocal(c, loc, nodeId),
        // .setObjectField     => try setObjectField(c, idx, .{}, nodeId),
        // .setVarSym          => try setVarSym(c, idx, nodeId),
        // .setLocalType       => try setLocalType(c, idx),
        // .switchStmt         => try switchStmt(c, idx, nodeId),
        // .tryStmt            => try tryStmt(c, idx, nodeId),
        .verbose            => {
            if (cy.Trace and !cc.verbose()) {
                cc.setVerbose(true);
                c.proc().resetVerboseOnEnd = true;
            }
        },
        else => {
            return error.TODO;
        }
    }

    log.tracev("----{s}: end", .{@tagName(code)});
}

fn genExprAndBox(c: *Chunk, loc: usize, cstr: Cstr) anyerror!Value {
    const expr_t = c.ir.getExprType(loc);
    const box = expr_t.id != bt.Any;
    if (box) {
        try c.bufPushBoxMacro(expr_t.id);
    }
    const res = try genExpr(c, loc, cstr);
    if (box) {
        try c.bufPush(")");
    }
    return res;
}

fn genExpr(c: *Chunk, loc: usize, cstr: Cstr) anyerror!Value {
    const code = c.ir.getExprCode(loc);
    const nodeId = c.ir.getNode(loc);

    if (cy.Trace) {
        const contextStr = try c.encoder.format(nodeId, &cy.tempBuf);
        log.tracev("{s}: {{{s}}}", .{@tagName(code), contextStr});
    }

    const res = try switch (code) {
        .box                => genBox(c, loc, cstr, nodeId),
        // .captured           => genCaptured(c, idx, cstr, nodeId),
        // .cast               => genCast(c, idx, cstr, nodeId),
        // .coinitCall         => genCoinitCall(c, idx, cstr, nodeId),
        // .coresume           => genCoresume(c, idx, cstr, nodeId),
        // .coyield            => genCoyield(c, idx, cstr, nodeId),
        // .enumMemberSym      => genEnumMemberSym(c, idx, cstr, nodeId),
        // .errorv             => genError(c, idx, cstr, nodeId),
        // .falsev             => genFalse(c, cstr, nodeId),
        // .fieldDynamic       => genFieldDynamic(c, idx, cstr, .{}, nodeId),
        .field              => genField(c, loc, cstr, nodeId),
        // .float              => genFloat(c, idx, cstr, nodeId),
        // .funcSym            => genFuncSym(c, idx, cstr, nodeId),
        // .ifExpr             => genIfExpr(c, idx, cstr, nodeId),
        .int                => genInt(c, loc, cstr, nodeId),
        // .lambda             => genLambda(c, idx, cstr, nodeId),
        // .list               => genList(c, idx, cstr, nodeId),
        .local              => genLocal(c, loc, cstr, nodeId),
        // .map                => genMap(c, idx, cstr, nodeId),
        .object_init        => genObjectInit(c, loc, cstr, nodeId),
        // .pre                => return error.Unexpected,
        .preBinOp           => genBinOp(c, loc, cstr, .{}, nodeId),
        .preCallDyn         => genCallDyn(c, loc, cstr, nodeId),
        .preCallFuncSym     => genCallFuncSym(c, loc, cstr, nodeId),
        // .preCallObjSym      => genCallObjSym(c, idx, cstr, nodeId),
        // .preUnOp            => genUnOp(c, idx, cstr, nodeId),
        .string             => genString(c, loc, cstr, nodeId),
        // .stringTemplate     => genStringTemplate(c, idx, cstr, nodeId),
        // .switchBlock        => genSwitchBlock(c, idx, cstr, nodeId),
        .symbol             => genSymbol(c, loc, cstr, nodeId),
        // .throw              => genThrow(c, idx, nodeId),
        // .truev              => genTrue(c, cstr, nodeId),
        // .tryExpr            => genTryExpr(c, idx, cstr, nodeId),
        // .typeSym            => genTypeSym(c, idx, cstr, nodeId),
        // .varSym             => genVarSym(c, idx, cstr, nodeId),
        else => {
            rt.printErrorZFmt(c.vm, "{}\n", .{code});
            return error.TODO;
        },
    };
    log.tracev("{s}: end", .{@tagName(code)});
    return res;
}

fn mainBlock(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .mainBlock);
    log.tracev("main block: {}", .{data.maxLocals});

    try c.beginLine(nodeId);
    try c.pushSpanEnd("int main() {");
    try c.pushProc(Proc.initMain());

    try c.localStack.resize(c.alloc, data.maxLocals);
    if (cy.Trace) {
        // Fill with und tag.
        @memset(c.localStack.items[0..data.maxLocals], .{ .null = {} });
    }

    // c.curBlock.frameLoc = 0;

    // Initialize.
    try c.pushLine("CbRT* rt = &cb_rt;", nodeId);

    var child = data.bodyHead;
    while (child != cy.NullId) {
        try genStmt(c, child);
        child = c.ir.getStmtNext(child);
    }

    // if (bcgen.shouldGenMainScopeReleaseOps(c.compiler)) {
        // try genBlockReleaseLocals(c);
    // }

    try c.pushLineNoMapping("return 0;");
    c.popProc();
    try c.pushLineNoMapping("}");
    try c.pushNewLine();
    // try bcgen.popBlock(c);

    // c.buf.mainStackSize = c.getMaxUsedRegisters();
}

fn declareLocalInit(c: *Chunk, idx: u32, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .declareLocalInit);

    // // Don't advance nextLocalReg yet since the rhs hasn't generated so the
    // // alive locals should not include this declaration.
    // const reg = try bcgen.reserveLocalReg(c, data.id, data.declType, data.isBoxed, nodeId, false);

    reserveLocal(c, data.id, data.name(), data.declType, data.lifted);

    const start = c.bufStart();
    try c.bufPushFmt("{} {s} = ", .{try c.cTypeName(data.declType), data.name()});

    const val = try genExpr(c, data.init, Cstr.init());
    _ = val;

    try c.beginLine(nodeId);
    try c.pushSpan(c.bufPop(start));
    try c.pushSpanEnd(";");

    // const local = bcgen.getLocalInfoPtr(c, reg);

    // // if (local.some.boxed) {
    // //     try c.pushOptionalDebugSym(nodeId);
    // //     try c.buf.pushOp2(.box, reg, reg);
    // // }
    // local.some.rcCandidate = val.retained;

    // // rhs has generated, increase `nextLocalReg`.
    // c.curBlock.nextLocalReg += 1;
    // log.tracev("declare {}, rced: {} ", .{val.local, local.some.rcCandidate});
}

fn declareLocal(c: *Chunk, idx: u32, nodeId: cy.NodeId) !void {
    _ = c;
    _ = idx;
    _ = nodeId;

    // const reg = try bcgen.reserveLocalReg(c, data.id, data.declType, data.isBoxed, nodeId, true);

    // // Not yet initialized, so it does not have a refcount.
    // bcgen.getLocalInfoPtr(c, reg).some.rcCandidate = false;
    return error.TODO;
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
        .bitwiseXor => "$infix||",
        .bitwiseLeftShift => "$infix<<",
        .bitwiseRightShift => "$infix>>",
        else => @panic("Unsupported op"),
    };
}

fn genCallDyn(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;
    const data = c.ir.getExprData(loc, .preCallDyn).callDyn;
    const args = c.ir.getArray(data.args, u32, data.numArgs);

    try c.bufPush("cbCallDyn(rt, ");
    _ = try genExpr(c, data.callee, Cstr.init());
    try c.bufPush(", (CbAny[]){");
    if (args.len > 0) {
        _ = try genExpr(c, args[0], Cstr.init());
        for (args[1..]) |idx| {
            try c.bufPush(", ");
            _ = try genExpr(c, idx, Cstr.init());
        }
    }
    try c.bufPushFmt("}}, {})", .{args.len});
    return Value{};
}

fn genCallFuncSym(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;

    const data = c.ir.getExprData(loc, .preCallFuncSym).callFuncSym;

    // if (data.hasDynamicArg) {
    //     try genCallTypeCheck(c, inst.ret + cy.vm.CallArgStart, data.numArgs, data.func.funcSigId, nodeId);
    // }

    const args = c.ir.getArray(data.args, u32, data.numArgs);

    switch (data.func.type) {
        .userFunc => {
            try c.bufPushFmt("{s}(rt", .{c.cSymName(data.func)});
            if (args.len > 0) {
                for (args) |idx| {
                    try c.bufPush(", ");
                    _ = try genExpr(c, idx, Cstr.init());
                }
            }
        },
        .hostFunc => {
            // try c.bufPushTrySpan();

            try c.bufPushFmt("{s}(rt, ", .{c.cSymName(data.func)});
            if (args.len > 0) {
                _ = try genExpr(c, args[0], Cstr.init());
                for (args[1..]) |idx| {
                    try c.bufPush(", ");
                    _ = try genExpr(c, idx, Cstr.init());
                }
            }

            // try c.bufPush(")");
        },
        else => return error.TODO,
    }

    try c.bufPush(")");

    // const rtId = c.compiler.genSymMap.get(data.func).?.funcSym.id;
    // try pushCallSym(c, inst.ret, data.numArgs, 1, rtId, nodeId);

    // const argvs = popValues(c, data.numArgs);
    // try checkArgs(argStart, argvs);

    // const retained = unwindTemps(c, argvs);
    // try pushReleaseVals(c, retained, nodeId);

    // const retRetained = c.sema.isRcCandidateType(data.func.retType);
    // return endCall(c, inst, retRetained);
    return Value{};
}

fn genString(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;
    const data = c.ir.getExprData(loc, .string);
    // const inst = try c.rega.selectForNoErrNoDepInst(cstr, true, nodeId);
    // if (inst.requiresPreRelease) {
    //     try pushRelease(c, inst.dst, nodeId);
    // }
    const c_lit = try cStringLit(c, data.raw);

    try c.bufPushFmt("STRING(\"{s}\")", .{c_lit});

    // try pushStringConst(c, str, inst.dst, nodeId);
    // return finishNoErrNoDepInst(c, inst, true);
    return Value{};
}

fn genSymbol(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;
    const data = c.ir.getExprData(loc, .symbol);
    try c.bufPushFmt("CB_SYM(\"{s}\", {})", .{data.name, data.name.len});
    return Value{};
}

fn genInt(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;

    const data = c.ir.getExprData(loc, .int);

    // const inst = try c.rega.selectForNoErrInst(cstr, false);
    // if (inst.requiresPreRelease) {
    //     try pushRelease(c, inst.dst, nodeId);
    // }

    try c.bufPushFmt("{}", .{data.val});
    // const value = try genConstIntExt(c, data.val, inst.dst, c.desc(nodeId));
    // return finishInst(c, value, inst.finalDst);

    return Value{};
}

fn reserveLocal(c: *Chunk, ir_id: u8, name: []const u8, declType: cy.TypeId, lifted: bool) void {
    c.localStack.items[c.proc().localStart + ir_id] = .{ .some = .{
        .name = name,
        .owned = true,
        .rcCandidate = c.sema.isRcCandidateType(declType),
        .lifted = lifted,
        .type = declType,
    }};
}

fn setLocal(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .setLocal).generic;
    const local_loc = c.ir.advanceStmt(loc, .setLocal); 
    const local_data = c.ir.getExprData(local_loc, .local);

    const start = c.bufStart();
    const b = c.proc();
    const local = c.localStack.items[b.localStart + local_data.id];
    try c.bufPushFmt("{s} = ", .{local.some.name});
    _ = try genExpr(c, data.right, Cstr.init());

    try c.beginLine(nodeId);
    try c.pushSpan(c.bufPop(start));
    try c.pushSpanEnd(";");
}

fn opSet(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const setIdx = c.ir.advanceStmt(loc, .opSet);
    try genStmt(c, @intCast(setIdx));
}

fn setCallObjSymTern(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .setCallObjSymTern).callObjSymTern;

    const start = c.bufStart();
    try c.bufPush("cbCallMethDyn(rt, ");
    _ = try genExprAndBox(c, data.rec, Cstr.init());
    try c.bufPushFmt(", \"{s}\", {}, (CbAny[]){{", .{data.name, data.name.len});
    _ = try genExprAndBox(c, data.index, Cstr.init());
    try c.bufPush(", ");
    _ = try genExprAndBox(c, data.right, Cstr.init());

    try c.beginLine(nodeId);
    try c.pushSpan(c.bufPop(start));
    try c.pushSpanEnd("}, 3);");
}

fn setField(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .setField).generic;
    // const requireTypeCheck = data.left_t.id != bt.Any and data.right_t.dynamic;
    const field_data = c.ir.getExprData(data.left, .field);

    const start = c.bufStart();
    _ = try genExpr(c, field_data.rec, Cstr.init());

    const rec_t = c.ir.getExprType(field_data.rec).id;
    const rec_sym = c.sema.getTypeSym(rec_t);
    if (rec_sym.type == .object_t) {
        try c.bufPush("->");
        const obj_t = c.sema.getTypeSym(rec_t).cast(.object_t);
        const field = obj_t.fields[field_data.idx];
        try c.bufPush(field.sym.name());
    } else {
        try c.bufPush(".");
        const struct_t = c.sema.getTypeSym(rec_t).cast(.struct_t);
        const field = struct_t.fields[field_data.idx];
        try c.bufPush(field.sym.name());
    }

    // try pushUnwindValue(c, recv);

    try c.bufPush(" = ");

    _ = try genExpr(c, data.right, Cstr.init());
    // try pushUnwindValue(c, rightv);

    try c.beginLine(nodeId);
    try c.pushSpan(c.bufPop(start));
    try c.pushSpanEnd(";");
}

fn setIndex(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .setIndex).index;
    if (data.recvT != bt.List and data.recvT != bt.Map) {
        return error.Unexpected;
    }

    const start = c.bufStart();
    const sym = c.sema.getTypeSym(data.recvT);
    try c.bufPushFmt("{s}(rt, ", .{
        c.cSymName(sym.getMod().?.getFirstFunc("$setIndex").?),
    });

    _ = try genExpr(c, data.rec, Cstr.init());
    try c.bufPush(", ");
    _ = try genExpr(c, data.index, Cstr.init());
    try c.bufPush(", ");
    _ = try genExpr(c, data.right, Cstr.init());

    try c.beginLine(nodeId);
    try c.pushSpan(c.bufPop(start));
    try c.pushSpanEnd(");");
}

fn loopStmt(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .loopStmt);

    try c.pushLine("while (true) {", nodeId);
    c.pushBlock();
    try genStmts(c, data.body_head);
    c.popBlock();
    try c.pushLineNoMapping("}");
}

fn forRangeStmt(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .forRangeStmt);

    const start = c.bufStart();
    try c.bufPush("for (i64 i = ");
    _ = try genExpr(c, data.start, Cstr.init());
    try c.bufPush("; i < ");
    _ = try genExpr(c, data.end, Cstr.init());

    try c.beginLine(nodeId);
    try c.pushSpan(c.bufPop(start));
    try c.pushSpanEnd("; i += 1) {");

    if (data.eachLocal) |var_id| {
        reserveLocal(c, var_id, "i", bt.Integer, false);
    }

    c.pushBlock();
    try genStmts(c, data.bodyHead);
    c.popBlock();
    try c.pushLineNoMapping("}");
}

fn exprStmt(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .exprStmt);

    // const cstr = RegisterCstr.initSimple(data.returnMain);

    const b = c.proc();

    const expr_t = c.ir.getExprType(data.expr).id;
    const start = c.bufStart();
    try c.bufPushFmt("{} tmp{} = ", .{
        try c.cTypeName(expr_t), b.nextTemp(),
    });
    const exprv = try genExpr(c, data.expr, Cstr.none);
    _ = exprv;

    try c.beginLine(nodeId);
    try c.pushSpan(c.bufPop(start));
    try c.pushSpanEnd(";");

    // if (unwindAndFreeTemp(c, exprv)) {
    //     // ARC cleanup.
    //     if (!data.returnMain) {
    //         // TODO: Merge with previous release inst.
    //         try pushRelease(c, exprv.local, nodeId);
    //     }
    // }

    // if (data.returnMain) {
    //     c.curBlock.endLocal = exprv.local;
    // }
}

fn genBox(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;

    const data = c.ir.getExprData(loc, .box);
    const expr_t = c.ir.getExprType(data.expr).id;
    try c.bufPushBoxMacro(expr_t);
    _ = try genExpr(c, data.expr, Cstr.init());
    try c.bufPush(")");
    return Value{};
}

fn genObjectInit(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;

    const data = c.ir.getExprData(loc, .object_init);
    const args = c.ir.getArray(data.args, u32, data.numArgs);

    const typ = c.sema.types.items[data.typeId];
    switch (typ.kind) {
        .@"struct" => {
            return error.TODO;
        },
        .object => {
            // const obj: *cy.sym.ObjectType = if (typ.kind == .object) typ.sym.cast(.object_t) else typ.sym.cast(.struct_t);
            // if (data.numFieldsToCheck > 0) {
            //     try c.pushFCode(.objectTypeCheck, &.{ argStart , @as(u8, @intCast(data.numFieldsToCheck)) }, nodeId);

            //     const checkFields = c.ir.getArray(data.fieldsToCheck, u8, data.numFieldsToCheck);

            //     for (checkFields) |fidx| {
            //         const start = c.buf.ops.items.len;
            //         try c.buf.pushOperands(&.{ @as(u8, @intCast(fidx)), 0, 0, 0, 0 });
            //         c.buf.setOpArgU32(start + 1, obj.fields[fidx].type);
            //     }
            // }

            const sym = c.sema.getTypeSym(data.typeId);
            const name = c.cSymName(sym);
            try c.bufPushFmt("CB_NEW({s}, {{", .{name});
            if (args.len > 0) {
                _ = try genExpr(c, args[0], Cstr.init());
                for (args[1..]) |arg| {
                    try c.bufPush(", ");
                    _ = try genExpr(c, arg, Cstr.init());
                }
            }
            try c.bufPush("})");
        },
        .choice => {
            return error.TODO;
        },
        else => return error.Unexpected,
    }

    // const argvs = popValues(c, data.numArgs);
    // try popTempAndUnwinds(c, argvs);
    return Value{};
}

fn genField(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;

    const data = c.ir.getExprData(idx, .field);

    const rec_t = c.ir.getExprType(data.rec).id;
    _ = try genExpr(c, data.rec, Cstr.init());

    const rec_sym = c.sema.getTypeSym(rec_t);
    if (rec_sym.type == .object_t) {
        try c.bufPush("->");
        const obj_t = c.sema.getTypeSym(rec_t).cast(.object_t);
        const field = obj_t.fields[data.idx];
        try c.bufPush(field.sym.name());
    } else {
        try c.bufPush(".");
        const struct_t = c.sema.getTypeSym(rec_t).cast(.struct_t);
        const field = struct_t.fields[data.idx];
        try c.bufPush(field.sym.name());
    }

    return Value{};
}

fn genLocal(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;

    const data = c.ir.getExprData(loc, .local);
    const b = c.proc();
    log.tracev("local: {}", .{data.id});
    const local = c.localStack.items[b.localStart + data.id];

    if (!local.some.lifted) {
        // const inst = try c.rega.selectForLocalInst(cstr, reg, local.some.rcCandidate);
        // if (inst.dst != reg) {
        //     if (inst.retainSrc) {
        //         if (inst.releaseDst) {
        //             try c.buf.pushOp2Ext(.copyRetainRelease, reg, inst.dst, c.desc(nodeId));
        //         } else {
        //             try c.buf.pushOp2Ext(.copyRetainSrc, reg, inst.dst, c.desc(nodeId));
        //         }
        //     } else {
        //         if (inst.releaseDst) {
        //             try c.buf.pushOp2Ext(.copyReleaseDst, reg, inst.dst, c.desc(nodeId));
        //         } else {
        //             try c.buf.pushOp2Ext(.copy, reg, inst.dst, c.desc(nodeId));
        //         }
        //     }
        // } else {
        //     // Nop. When the cstr allows returning the local itself.
        //     if (inst.retainSrc) {
        //         try c.buf.pushOp1Ext(.retain, reg, c.desc(nodeId));
        //     } else {
        //         // Nop.
        //     }
        // }
        // const val = genValue(c, inst.dst, inst.retainSrc);
        // return finishInst(c, val, inst.finalDst);

        try c.bufPush(local.some.name);
        return Value{};
    } else {
        // // Special case when src local is boxed.
        // const retainSrc = local.some.rcCandidate and (cstr.mustRetain or cstr.type == .local or cstr.type == .boxedLocal);
        // const inst = try c.rega.selectForDstInst(cstr, retainSrc);

        // if (retainSrc) {
        //     try c.buf.pushOp2Ext(.boxValueRetain, reg, inst.dst, c.desc(nodeId));
        // } else {
        //     try c.buf.pushOp2Ext(.boxValue, reg, inst.dst, c.desc(nodeId));
        // }

        // const val = genValue(c, inst.dst, retainSrc);
        // return finishInst(c, val, inst.finalDst);
        return error.TODO;
    }
}

fn funcBlock(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .funcBlock);
    const func = data.func;
    const paramsIdx = c.ir.advanceStmt(loc, .funcBlock);
    const params = c.ir.getArray(paramsIdx, ir.FuncParam, func.numParams);

    // try pushFuncBlock(c, data, params, nodeId);

    try c.beginLine(nodeId);
    try c.pushSpanFmt("{} {s}(CbRT* rt", .{ try c.cTypeName(func.retType), c.cSymName(func) });
    if (params.len > 0) {
        for (params) |param| {
            try c.pushSpanFmt(", {} {s}", .{ try c.cTypeName(param.declType), param.name()});
        }
    }
    try c.pushSpanEnd(") {");
    try c.pushProc(Proc.initFunc(func));

    // c.curBlock.frameLoc = nodeId;

    // if (c.compiler.config.genDebugFuncMarkers) {
    //     try c.compiler.buf.pushDebugFuncStart(func, c.id);
    // }

    // // `reserveFuncRegs` may emit copy and box insts.
    // try reserveFuncRegs(c, maxIrLocals, numParamCopies, params);

    const start = c.localStack.items.len;
    try c.localStack.resize(c.alloc, start + data.maxLocals);
    if (cy.Trace) {
        // Fill with und tag.
        @memset(c.localStack.items[start..], .{ .null = {} });
    }

    // Reserve func params.
    for (params, 0..) |param, i| {
        if (param.isCopy) {
            // // Forward reserve the param copy.
            // const reg: RegisterId = @intCast(4 + 1 + numParams + paramCopyIdx);
            // c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + i] = reg;

            // c.genLocalStack.items[c.curBlock.localStart + 4 + 1 + numParams + paramCopyIdx] = .{
            //     .some = .{
            //         .owned = true,
            //         .rcCandidate = c.sema.isRcCandidateType(param.declType),
            //         .boxed = param.isBoxed,
            //     },
            // };

            // // Copy param to local.
            // if (param.isBoxed) {
            //     try c.pushFailableDebugSym(c.curBlock.debugNodeId);
            //     // Retain param and box.
            //     try c.buf.pushOp1(.retain, nextReg);
            //     try c.buf.pushOp2(.box, nextReg, reg);
            // } else {
            //     try c.buf.pushOp2(.copyRetainSrc, nextReg, reg);
            // }

            // paramCopyIdx += 1;
            return error.TODO;
        } else {
            c.localStack.items[start + i] = .{
                .some = .{
                    .name = param.name(),
                    .owned = false,
                    .rcCandidate = c.sema.isRcCandidateType(param.declType),
                    .lifted = false,
                    .type = param.declType,
                },
            };
        }
        log.tracev("reserve param: {}", .{i});
    }

    try genStmts(c, data.bodyHead);

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

    // try popFuncBlockCommon(c, func);
    c.popProc();
    try c.pushLineNoMapping("}");
    try c.pushNewLine();
}

fn breakStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    // // Release from startLocal of the first parent loop block.
    // var idx = c.blocks.items.len-1;
    // while (true) {
    //     const b = c.blocks.items[idx];
    //     if (b.isLoopBlock) {
    //         try genReleaseLocals(c, b.nextLocalReg, nodeId);
    //         break;
    //     }
    //     idx -= 1;
    // }

    // const pc = try c.pushEmptyJumpExt(c.desc(nodeId));
    // try c.blockJumpStack.append(c.alloc, .{ .jumpT = .brk, .pc = pc });

    try c.pushLine("break;", nodeId);
}

fn ifStmt(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .ifStmt);

    const start = c.bufStart();
    try c.bufPush("if (");

    const cond_t = c.ir.getExprType(data.cond);
    if (cond_t.id == bt.Any) {
        try c.bufPush("TRY_UNBOX_BOOL(");
    }

    var cond_nid = c.ir.getNode(data.cond);
    _ = try genExpr(c, data.cond, Cstr.init());

    try c.beginLine(nodeId);
    try c.pushSpan(c.bufPop(start));
    if (cond_t.id == bt.Any) {
        try c.pushSpan(")");
    }
    try c.pushSpanEnd(") {");

    // // ARC cleanup for true case.
    // if (unwindAndFreeTemp(c, condv)) {
    //     try pushRelease(c, condv.local, condNodeId);
    // }

    c.pushBlock();
    try genStmts(c, data.body_head);
    c.popBlock();

    if (data.else_block != cy.NullId) {
        var else_loc = data.else_block;
        while (else_loc != cy.NullId) {
            try c.pushLineNoMapping("} else {");
            c.pushBlock();

            const else_nid = c.ir.getNode(else_loc);
            const else_data = c.ir.getExprData(else_loc, .else_block);

            if (else_data.cond != cy.NullId) {
                cond_nid = c.ir.getNode(else_data.cond);

                const elseif_start = c.bufStart();
                try c.bufPush("if (");
                _ = try genExpr(c, else_data.cond, Cstr.init());

                try c.beginLine(else_nid);
                try c.pushSpan(c.bufPop(elseif_start));
                try c.pushSpanEnd(") {");

                c.pushBlock();
                try genStmts(c, else_data.body_head);
                c.popBlock();

                // try pushUnwindValue(c, condv);

                // // ARC cleanup for true case.
                // try popTempAndUnwind(c, condv);
                // try releaseTempValue(c, condv, cond_nid);
            } else {
                try genStmts(c, else_data.body_head);
            }
            c.popBlock();
        }
    }
    try c.pushLineNoMapping("}");
}

const BinOpOptions = struct {
    left: ?Value = null,
};

fn genBinOp(c: *Chunk, loc: usize, cstr: Cstr, opts: BinOpOptions, nodeId: cy.NodeId) !Value {
    _ = cstr;
    const data = c.ir.getExprData(loc, .preBinOp).binOp;
    log.tracev("binop {} {}", .{data.op, data.leftT});

    if (data.op == .and_op) {
        return error.TODO;
        // return genAndOp(c, idx, data, cstr, nodeId);
    } else if (data.op == .or_op) {
        return error.TODO;
        // return genOr(c, idx, data, cstr, nodeId);
    }

    // // Most builtin binOps do not retain.
    // var willRetain = false;
    // switch (data.op) {
    //     .index => {
    //         willRetain = true;
    //     },
    //     else => {},
    // }
    // const inst = try c.rega.selectForDstInst(cstr, willRetain);

    switch (data.op) {
        .index => {
            if (data.leftT == bt.List) {
                const sym = c.sema.getTypeSym(bt.List);
                try c.bufPushFmt("{s}(rt, ", .{
                    c.cSymName(sym.getMod().?.getFirstFunc("$index").?),
                });
            }
        },
        else => {},
    }

    // Lhs.
    var leftv: Value = undefined;
    if (opts.left) |left| {
        leftv = left;
    } else {
        leftv = try genExpr(c, data.left, Cstr.init());
    }

    var retained = false;
    switch (data.op) {
        .index => {
            if (data.leftT == bt.List) {
                try c.bufPush(", ");
            // } else if (data.leftT == bt.Tuple) {
            //     try pushInlineBinExpr(c, .indexTuple, leftv.local, rightv.local, inst.dst, nodeId);
            // } else if (data.leftT == bt.Map) {
            //     try pushInlineBinExpr(c, .indexMap, leftv.local, rightv.local, inst.dst, nodeId);
            } else return error.TODO;
            retained = true;
        },
        .bitwiseAnd,
        .bitwiseOr,
        .bitwiseXor,
        .bitwiseLeftShift,
        .bitwiseRightShift => {
            // if (data.leftT == bt.Integer) {
            //     try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.local, rightv.local, inst.dst, nodeId);
            // } else return error.Unexpected;
            return error.TODO;
        },
        .greater,
        .greater_equal,
        .less,
        .less_equal,
        .star,
        .slash,
        .percent,
        .caret,
        .plus,
        .minus => {
            if (data.leftT == bt.Float) {
                if (data.rightT == bt.Float) {
                    try c.bufPush(cBinOpLit(data.op));
                } else {
                    return error.TODO;
                }
            } else if (data.leftT == bt.Integer) {
                if (data.rightT == bt.Integer) {
                    try c.bufPush(cBinOpLit(data.op));
                } else {
                    return error.TODO;
                }
            } else return error.Unexpected;
        },
        .equal_equal => {
            // try c.pushOptionalDebugSym(nodeId);
            // try c.buf.pushOp3Ext(.compare, leftv.local, rightv.local, inst.dst, c.desc(nodeId));
            return error.TODO;
        },
        .bang_equal => {
            // try c.pushOptionalDebugSym(nodeId);
            // try c.buf.pushOp3Ext(.compareNot, leftv.local, rightv.local, inst.dst, c.desc(nodeId));
            return error.TODO;
        },
        else => {
            return c.base.reportErrorFmt("Unsupported op: {}", &.{v(data.op)}, nodeId);
        },
    }

    // Rhs.
    const rightv = try genExpr(c, data.right, Cstr.init());
    _ = rightv;

    switch (data.op) {
        .index => {
            if (data.leftT == bt.List) {
                try c.bufPush(")");
            }
        },
        else => {},
    }

    // const leftRetained = if (opts.left == null) unwindTempKeepDst(c, leftv, inst.dst) else false;
    // const rightRetained = unwindTempKeepDst(c, rightv, inst.dst);

    // // ARC cleanup.
    // try pushReleaseOpt2(c, leftRetained, leftv.local, rightRetained, rightv.local, nodeId);

    // const val = genValue(c, inst.dst, retained);
    // return finishInst(c, val, inst.finalDst);
    return Value{};
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
        .caret => cy.fatal(),
        else => cy.fatal(),
    };
}

fn retExprStmt(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .retExprStmt);

    // TODO: If the returned expr is a local, consume the local after copying to reg 0.
    const start = c.bufStart();
    try c.bufPush("return ");
    var childv: Value = undefined;
    const b = c.proc();
    if (b.type == .main) {
        // // Main block.
        // childv = try genExpr(c, data.expr, RegisterCstr.simpleMustRetain);
        return error.TODO;
    } else {
        childv = try genExpr(c, data.expr, Cstr.init());
    }

    try c.beginLine(nodeId);
    try c.pushSpan(c.bufPop(start));
    try c.pushSpanEnd(";");

    // _ = unwindAndFreeTemp(c, childv);

    // try genBlockReleaseLocals(c);
    // if (c.curBlock.type == .main) {
    //     try c.buf.pushOp1(.end, @intCast(childv.local));
    // } else {
    //     try c.buf.pushOp(.ret1);
    // }
}

pub fn cStringLit(self: *Chunk, raw: []const u8) ![]const u8 {
    // Big enough to hold escaped C literal.
    try self.base.tempBufU8.resize(self.alloc, raw.len * 2);

    // Escape to C literal.
    const ReplaceChars = "\\\"";
    const S = struct {
        fn replacement(char: u8) []const u8 {
            return switch (char) {
                '\\' => "\\\\",
                '"' => "\\\"",
                else => &.{char},
            };
        }
    };
    if (std.mem.indexOfAny(u8, raw, ReplaceChars)) |idx| {
        var fbuf = std.io.fixedBufferStream(self.base.tempBufU8.items[0..]);
        const w = fbuf.writer();
        try w.print("{s}{s}", .{raw[0..idx], S.replacement(raw[idx])});

        var rest = raw[idx+1..];
        while (std.mem.indexOfAny(u8, rest, ReplaceChars)) |idx2| {
            try w.print("{s}{s}", .{rest[0..idx2], S.replacement(rest[idx2])});
            rest = rest[idx2+1..];
        }
        try w.writeAll(rest);
        return fbuf.getWritten();
    } else {
        return raw;
    }
}