const std = @import("std");
const builtin = @import("builtin");
const cy = @import("cyber.zig");
const rt = cy.rt;
const ir = cy.ir;
const bt = cy.types.BuiltinTypes;
const log = cy.log.scoped(.cgen);
const v = cy.fmt.v;
const tcc = @import("tcc");

const SymbolId = u32;

const Compiler = struct {
    alloc: std.mem.Allocator,
    base: *cy.VMcompiler,
    syms: std.ArrayListUnmanaged(Symbol),
    symSignatures: std.StringHashMapUnmanaged(SymbolId),

    // Can be cy.Sym or cy.Func.
    cSymNameOverrides: std.AutoHashMapUnmanaged(*anyopaque, SymName),

    pub fn deinit(c: *Compiler) void {
        c.syms.deinit(c.alloc);
        c.symSignatures.deinit(c.alloc);
        c.cSymNameOverrides.deinit(c.alloc);
    }

    pub fn ensureSym(c: *Compiler, name: []const u8) !SymbolId {
        const res = try c.symSignatures.getOrPut(c.alloc, name);
        if (!res.found_existing) {
            const id: u32 = @intCast(c.syms.items.len);
            try c.syms.append(c.alloc, .{
                .name = name,
            });
            res.value_ptr.* = id;
            return id;
        } else {
            return res.value_ptr.*;
        }
    }

    fn shortenChunkSymName(c: *Compiler, sym: *cy.Sym) !void {
        var name = sym.name();
        if (name[0] == '/') {
            name = std.fs.path.stem(name);
            try c.cSymNameOverrides.put(c.alloc, sym, .{
                .namePtr = name.ptr,
                .nameLen = @intCast(name.len),
                .owned = false,
            });
        }
    }
};

const SymName = struct {
    namePtr: [*]const u8,
    nameLen: u16,
    owned: bool,

    fn name(self: SymName) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

const Symbol = struct {
    name: []const u8,
};

const Chunk = struct {
    alloc: std.mem.Allocator,
    ir: cy.ir.Buffer,
    encoder: cy.ast.Encoder,
    tmpOut: std.ArrayListUnmanaged(u8),
    out: std.ArrayListUnmanaged(u8),
    outw: std.ArrayListUnmanaged(u8).Writer,
    blocks: std.ArrayListUnmanaged(Block),
    compiler: *Compiler,
    base: *cy.Chunk,
    vm: *cy.VM,
    sema: *cy.Sema,
    errNodeId: cy.NodeId,
    localStack: std.ArrayListUnmanaged(Local),
    tryStack: std.ArrayListUnmanaged(Try),
    indent: u32,
    emitSourceMap: bool,
    ast: cy.ast.Source,
    srcUri: []const u8,

    fn deinit(c: *Chunk) void {
        c.blocks.deinit(c.alloc);
        c.tmpOut.deinit(c.alloc);
        c.out.deinit(c.alloc);
        c.localStack.deinit(c.alloc);
        c.tryStack.deinit(c.alloc);
    }

    fn block(c: *Chunk) *Block {
        return &c.blocks.items[c.blocks.items.len-1];
    }

    fn pushBlock(c: *Chunk, b: Block) !void {
        var b_ = b;
        b_.localStart = @intCast(c.localStack.items.len);
        b_.tryStart = @intCast(c.tryStack.items.len);
        try c.blocks.append(c.alloc, b_);
        c.indent += 1;
    }

    fn popBlock(c: *Chunk) void {
        _ = c.blocks.pop();
        c.indent -= 1;
    }

    fn pushSubBlock(c: *Chunk) void {
        c.indent += 1;
    }

    fn popSubBlock(c: *Chunk) void {
        c.indent -= 1;
    }

    fn beginLine(c: *Chunk, nodeId: cy.NodeId) !void {
        if (c.emitSourceMap) {
            try c.pushIndent();
            var line: u32 = undefined;
            var col: u32 = undefined;
            var lineStart: u32 = undefined;
            const node = c.ast.nodes[nodeId];
            const token = c.ast.tokens[node.start_token];
            cy.debug.computeLinePos(c.ast.src, token.pos(), &line, &col, &lineStart);
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

    fn pushTrySpan(c: *Chunk) !void {
        const b = c.block();
        if (c.tryStack.items.len > b.tryStart) {
            const last = c.tryStack.getLast();
            try c.pushSpanFmt("TRY_GOTO({}, ", .{last.suffix});
        } else {
            if (b.type == .main) {
                try c.pushSpan("TRY_PANIC(");
            } else {
                try c.pushSpan("TRY_RET(");
            }
        }
    }

    fn pushIndent(c: *Chunk) !void {
        for (0..c.indent) |_| {
            try c.out.append(c.alloc, '\t');
        }
    }

    fn patchTmpStmts(c: *Chunk) !void {
        if (c.tmpOut.items.len > 0) {
            return error.TODO;
        }
    }
};

const BlockType = enum {
    main,
    func,
};

const Block = struct {
    type: BlockType,

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

    fn initMain() Block {
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

    fn initFunc(func: *cy.Func) Block {
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

    fn nextTemp(b: *Block) u32 {
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

        /// Whether it was declared to be a box value.
        boxed: bool,

        /// The current type of the local. This can change during its lifetime for dynamic vars.
        type: cy.TypeId,
    },
    und: void,
};

const Cstr = struct {
    dstType: ?cy.TypeId,

    const none = Cstr{ .dstType = null };

    fn init(dstType: cy.TypeId) Cstr {
        return .{
            .dstType = dstType,
        };
    }
};

const Value = struct {
};

pub fn gen(self: *cy.VMcompiler) !cy.vm_compiler.AotCompileResult {
    var compiler = Compiler{
        .base = self,
        .alloc = self.alloc,
        .syms = .{},
        .symSignatures = .{},
        .cSymNameOverrides = .{},
    };
    defer compiler.deinit();

    // Symbols used by std. Once std is self-hosted, it can just emit used symbols.
    for (std.enums.values(cy.bindings.Symbol)) |sym| {
        _ = try compiler.ensureSym(@tagName(sym));
    }

    // Prepare host funcs.
    for (self.chunks.items) |chunk| {
        const mod = chunk.sym.getMod();
        for (mod.funcs.items) |func| {
            _ = func;
            // try jitgen.prepareFunc(self, func);
        }

        for (chunk.modSyms.items) |modSym| {
            const mod2 = modSym.getMod().?;
            for (mod2.funcs.items) |func| {
                _ = func;
                // try jitgen.prepareFunc(self, func);
            }
        }
    }

    var chunks = try self.alloc.alloc(Chunk, self.chunks.items.len);
    for (self.chunks.items, 0..) |chunk, i| {
        chunks[i] = .{
            .alloc = self.alloc,
            .ir = chunk.ir,
            .encoder = chunk.encoder,
            .tmpOut = .{},
            .out = .{},
            .outw = undefined,
            .blocks = .{},
            .base = chunk,
            .vm = chunk.vm,
            .sema = chunk.sema,
            .localStack = .{},
            .tryStack = .{},
            .compiler = &compiler,
            .errNodeId = cy.NullId,
            .indent = 0,
            .emitSourceMap = self.config.emitSourceMap,
            .ast = chunk.ast,
            .srcUri = chunk.srcUri,
        };
        chunks[i].outw = chunks[i].out.writer(self.alloc);

        try compiler.shortenChunkSymName(&chunk.sym.head);
    }
    defer {
        for (chunks) |*chunk| {
            chunk.deinit();
        }
        self.alloc.free(chunks);
    }

    for (self.chunks.items, 0..) |chunk, i| {
        if (chunk.id != 0) {
            // Skip other chunks for now.
            continue;
        }

        log.tracev(self.vm, "Perform codegen for chunk{}: {s}", .{chunk.id, chunk.srcUri});

        try genChunk(&chunks[i]);
        log.tracev(self.vm, "Done. performChunkCodegen {s}", .{chunk.srcUri});
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
        if (chunk.base.id != 0) {
            // Skip other chunks for now.
            continue;
        }
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

    if (self.config.backend == .tcc) {
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
            .argv = &.{"clang", "-o", exePath, outPath, "zig-out/lib/libcyber.a"},
        });
        defer self.alloc.free(res.stderr);
        defer self.alloc.free(res.stdout);

        if (res.term != .Exited or res.term.Exited != 0) {
            rt.log(self.vm, res.stderr);
            return error.CCError;
        }
    }
    return .{
        .exePath = exePath,
    };
}

fn genHead(c: *Compiler, w: std.ArrayListUnmanaged(u8).Writer, chunks: []Chunk) !void {

    const head = @embedFile("pm.h");
    try w.writeAll(head);

    // Predefines.
    try w.writeAll("Symbol cy_syms[] = {\n");
    for (c.syms.items) |sym| {
        try w.print("\t(Symbol){{ .name = STR_INIT(\"{s}\", {}) }},\n", .{sym.name, sym.name.len});
    }
    try w.writeAll("};\n");
    try w.writeAll(
        \\PM pm;
        \\Fiber mainFiber;
        \\Value cy_panic() {
        \\    // TODO: Dump stack trace
        \\    fprintf(stderr, "panic.\n");
        \\    exit(1);
        \\}
        \\
        \\
    );

    // Forward declarations.
    for (chunks) |*chunk| {
        const base = chunk.base;
        const modName = cSymName(chunk, &base.sym.head);
        const isStd = std.mem.eql(u8, modName, "builtins") or
            std.mem.eql(u8, modName, "test");

        for (base.sym.getMod().funcs.items) |func| {
            if (func.type == .userLambda) {
                continue;
            }
            if (isStd) {
                try w.print("extern Value {s}_{s}(Fiber*, Value*, uint8_t);\n", .{ modName, func.name() });
            } else {
                try w.print("{s} {s}_{s}(Fiber* f", .{ try cTypeName(&c.base.sema, func.retType), modName, func.name() });
                const funcSig = base.sema.getFuncSig(func.funcSigId);
                const params = funcSig.params();
                if (params.len > 0) {
                    for (params) |param| {
                        try w.print(", {s}", .{ try cTypeName(&c.base.sema, param) });
                    }
                }
                try w.writeAll(");\n");
            }
        }
    }
    try w.writeByte('\n');
}

fn genChunk(c: *Chunk) !void {
    genChunkInner(c) catch |err| {
        if (err != error.CompileError) {
            // Wrap all other errors as a CompileError.
            log.tracev(c.vm, "{*}", .{c.base});
            try c.base.setErrorFmtAt("error.{}", &.{v(err)}, c.errNodeId);
            log.tracev(c.vm, "{*}", .{c.base});
            return error.CompileError;
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

fn genStmt(c: *Chunk, idx: u32) anyerror!void {
    const code = c.ir.getStmtCode(idx);
    const nodeId = c.ir.getNode(idx);
    errdefer if (c.errNodeId == cy.NullId) { c.errNodeId = nodeId; };

    if (cy.Trace) {
        const contextStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        log.tracev(c.vm, "----{s}: {{{s}}}", .{@tagName(code), contextStr});
    }
    switch (code) {
        // .breakStmt          => try breakStmt(c, nodeId),
        // .contStmt           => try contStmt(c, nodeId),
        .declareLocal       => try declareLocal(c, idx, nodeId),
        // .destrElemsStmt     => try destrElemsStmt(c, idx, nodeId),
        .exprStmt           => try exprStmt(c, idx, nodeId),
        // .forIterStmt        => try forIterStmt(c, idx, nodeId),
        // .forRangeStmt       => try forRangeStmt(c, idx, nodeId),
        .funcDecl           => try funcDecl(c, idx, nodeId),
        .ifStmt             => try ifStmt(c, idx, nodeId),
        .mainBlock          => try mainBlock(c, idx, nodeId),
        // .opSet              => try opSet(c, idx, nodeId),
        // .pushDebugLabel     => try pushDebugLabel(c, idx),
        .retExprStmt        => try retExprStmt(c, idx, nodeId),
        // .retStmt            => try retStmt(c),
        // .setCallObjSymTern  => try setCallObjSymTern(c, idx, nodeId),
        // .setCaptured        => try setCaptured(c, idx, nodeId),
        // .setField           => try setField(c, idx, .{}, nodeId),
        // .setFuncSym         => try setFuncSym(c, idx, nodeId),
        // .setIndex           => try setIndex(c, idx, nodeId),
        // .setLocal           => try irSetLocal(c, idx, nodeId),
        // .setObjectField     => try setObjectField(c, idx, .{}, nodeId),
        // .setVarSym          => try setVarSym(c, idx, nodeId),
        // .setLocalType       => try setLocalType(c, idx),
        // .switchStmt         => try switchStmt(c, idx, nodeId),
        // .tryStmt            => try tryStmt(c, idx, nodeId),
        .verbose            => {
            if (cy.Trace and !cy.verbose) {
                cy.verbose = true;
                c.block().resetVerboseOnEnd = true;
            }
        },
        // .whileCondStmt      => try whileCondStmt(c, idx, nodeId),
        // .whileInfStmt       => try whileInfStmt(c, idx, nodeId),
        // .whileOptStmt       => try whileOptStmt(c, idx, nodeId),
        else => {
            return error.TODO;
        }
    }

    log.tracev(c.vm, "----{s}: end", .{@tagName(code)});
}

fn genTopExpr(c: *Chunk, idx: usize, cstr: Cstr) !Value {
    c.tmpOut.clearRetainingCapacity();
    const val = try genExpr(c, idx, cstr);
    try c.patchTmpStmts();
    return val;
}

fn genExpr(c: *Chunk, idx: usize, cstr: Cstr) anyerror!Value {

    const code = c.ir.getExprCode(idx);
    const nodeId = c.ir.getNode(idx);

    if (cy.Trace) {
        const contextStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        log.tracev(c.vm, "{s}: {{{s}}}", .{@tagName(code), contextStr});
    }

    const res = try switch (code) {
        // .captured           => genCaptured(c, idx, cstr, nodeId),
        // .cast               => genCast(c, idx, cstr, nodeId),
        // .coinitCall         => genCoinitCall(c, idx, cstr, nodeId),
        // .condExpr           => genCondExpr(c, idx, cstr, nodeId),
        // .coresume           => genCoresume(c, idx, cstr, nodeId),
        // .coyield            => genCoyield(c, idx, cstr, nodeId),
        // .enumMemberSym      => genEnumMemberSym(c, idx, cstr, nodeId),
        // .errorv             => genError(c, idx, cstr, nodeId),
        // .falsev             => genFalse(c, cstr, nodeId),
        // .fieldDynamic       => genFieldDynamic(c, idx, cstr, .{}, nodeId),
        // .fieldStatic        => genFieldStatic(c, idx, cstr, .{}, nodeId),
        // .float              => genFloat(c, idx, cstr, nodeId),
        // .funcSym            => genFuncSym(c, idx, cstr, nodeId),
        .int                => genInt(c, idx, cstr, nodeId),
        // .lambda             => genLambda(c, idx, cstr, nodeId),
        // .list               => genList(c, idx, cstr, nodeId),
        .local              => genLocal(c, idx, cstr, nodeId),
        // .map                => genMap(c, idx, cstr, nodeId),
        // .none               => genNone(c, cstr, nodeId),
        // .objectInit         => genObjectInit(c, idx, cstr, nodeId),
        // .pre                => return error.Unexpected,
        .preBinOp           => genBinOp(c, idx, cstr, .{}, nodeId),
        // .preCall            => genCall(c, idx, cstr, nodeId),
        .preCallFuncSym     => genCallFuncSym(c, idx, cstr, nodeId),
        // .preCallObjSym      => genCallObjSym(c, idx, cstr, nodeId),
        // .preCallObjSymBinOp => genCallObjSymBinOp(c, idx, cstr, nodeId),
        // .preCallObjSymUnOp  => genCallObjSymUnOp(c, idx, cstr, nodeId),
        // .preSlice           => genSlice(c, idx, cstr, nodeId),
        // .preUnOp            => genUnOp(c, idx, cstr, nodeId),
        // .string             => genString(c, idx, cstr, nodeId),
        // .stringTemplate     => genStringTemplate(c, idx, cstr, nodeId),
        // .switchBlock        => genSwitchBlock(c, idx, cstr, nodeId),
        .symbol                => genSymbol(c, idx, cstr, nodeId),
        // .throw              => genThrow(c, idx, nodeId),
        // .truev              => genTrue(c, cstr, nodeId),
        // .tryExpr            => genTryExpr(c, idx, cstr, nodeId),
        // .typeSym            => genTypeSym(c, idx, cstr, nodeId),
        // .varSym             => genVarSym(c, idx, cstr, nodeId),
        else => return error.TODO,
    };
    log.tracev(c.vm, "{s}: end", .{@tagName(code)});
    return res;
}

fn mainBlock(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .mainBlock);
    log.tracev(c.vm, "main block: {}", .{data.maxLocals});

    try c.beginLine(nodeId);
    try c.pushSpanEnd("int main() {");
    try c.pushBlock(Block.initMain());

    try c.localStack.resize(c.alloc, data.maxLocals);
    if (cy.Trace) {
        // Fill with und tag.
        @memset(c.localStack.items[0..data.maxLocals], .{ .und = {} });
    }

    // c.curBlock.frameLoc = 0;

    // Initialize.
    try c.pushLine("pm.syms = cy_syms;", nodeId);
    try c.pushLine("mainFiber.pm = &pm;", nodeId);
    try c.pushLine("Fiber* f = &mainFiber;", nodeId);

    var child = data.bodyHead;
    while (child != cy.NullId) {
        try genStmt(c, child);
        child = c.ir.getStmtNext(child);
    }

    // if (bcgen.shouldGenMainScopeReleaseOps(c.compiler)) {
        // try genBlockReleaseLocals(c);
    // }

    try c.pushLineNoMapping("return 0;");
    c.popBlock();
    try c.pushLineNoMapping("}");
    try c.pushNewLine();
    // try bcgen.popBlock(c);

    // c.buf.mainStackSize = c.getMaxUsedRegisters();
}

fn cIsBoxedType(typeId: cy.TypeId) bool {
    return switch (typeId) {
        bt.Integer => false,
        else => true,
    };
}

fn cBoxMacro(typeId: cy.TypeId) []const u8 {
    return switch (typeId) {
        bt.Integer => "BOX_INT48",
        else => "BOX_UNKNOWN",
    };
}

fn declareLocal(c: *Chunk, idx: u32, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .declareLocal);
    if (data.assign) {
        // // Don't advance nextLocalReg yet since the rhs hasn't generated so the
        // // alive locals should not include this declaration.
        // const reg = try bcgen.reserveLocalReg(c, data.id, data.declType, data.isBoxed, nodeId, false);

        const b = c.block();
        c.localStack.items[b.localStart + data.id] = .{ .some = .{
            .name = data.name(),
            .owned = true,
            .rcCandidate = c.sema.isRcCandidateType(data.declType),
            .lifted = data.lifted,
            .boxed = cIsBoxedType(data.declType),
            .type = data.declType,
        }};

        try c.beginLine(nodeId);
        try c.pushSpanFmt("{s} {s} = ", .{try cTypeName(c.sema, data.declType), data.name()});
        const exprIdx = c.ir.advanceStmt(idx, .declareLocal);
        const val = try genTopExpr(c, exprIdx, Cstr.init(data.declType));
        _ = val;
        try c.pushSpanEnd(";");

        // const local = bcgen.getLocalInfoPtr(c, reg);

        // // if (local.some.boxed) {
        // //     try c.pushOptionalDebugSym(nodeId);
        // //     try c.buf.pushOp2(.box, reg, reg);
        // // }
        // local.some.rcCandidate = val.retained;

        // // rhs has generated, increase `nextLocalReg`.
        // c.curBlock.nextLocalReg += 1;
        // log.tracev(c.vm, "declare {}, rced: {} ", .{val.local, local.some.rcCandidate});
    } else {
        // const reg = try bcgen.reserveLocalReg(c, data.id, data.declType, data.isBoxed, nodeId, true);

        // // Not yet initialized, so it does not have a refcount.
        // bcgen.getLocalInfoPtr(c, reg).some.rcCandidate = false;
        return error.TODO;
    }
}

fn genCallFuncSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;

    const data = c.ir.getExprData(idx, .preCallFuncSym).callFuncSym;

    if (data.func.type == .hostInlineFunc) {
        // // TODO: Make this handle all host inline funcs.
        // if (@intFromPtr(data.func.data.hostInlineFunc.ptr) == @intFromPtr(cy.builtins.appendList)) {
        //     const inst = try c.rega.selectForDstInst(cstr, false);
        //     const args = c.ir.getArray(data.args, u32, data.numArgs);
        //     const recv = try genExpr(c, args[0], RegisterCstr.simple);
        //     const itemv = try genExpr(c, args[1], RegisterCstr.simple);

        //     if (data.hasDynamicArg) {
        //         try pushTypeCheck(c, recv.local, bt.List, nodeId);
        //     }

        //     try pushInlineBinExpr(c, .appendList, recv.local, itemv.local, inst.dst, nodeId);

        //     const recvRetained = unwindAndFreeTemp(c, recv);
        //     const itemvRetained = unwindAndFreeTemp(c, itemv);

        //     // ARC cleanup.
        //     try pushReleaseOpt2(c, recvRetained, recv.local, itemvRetained, itemv.local, nodeId);

        //     const val = genValue(c, inst.dst, false);
        //     return finishInst(c, val, inst.finalDst);
        // } else {
        //     return error.UnsupportedInline;
        // }
        return error.TODO;
    } else {

        // if (data.hasDynamicArg) {
        //     try genCallTypeCheck(c, inst.ret + cy.vm.CallArgStart, data.numArgs, data.func.funcSigId, nodeId);
        // }

        const args = c.ir.getArray(data.args, u32, data.numArgs);

        switch (data.func.type) {
            .userFunc => {
                const name = data.func.sym.?.head.name();
                const funcSig = c.sema.getFuncSig(data.func.funcSigId);
                const params = funcSig.params();
                const modName = cSymName(c, data.func.sym.?.head.parent.?);
                try c.pushSpanFmt("{s}_{s}(f", .{modName, name});

                if (args.len > 0) {
                    for (args, 0..) |argIdx, i| {
                        try c.pushSpan(", ");
                        _ = try genExpr(c, argIdx, Cstr.init(params[i]));
                    }
                }
            },
            .hostFunc => {
                try c.pushTrySpan();

                const sym = data.func.sym.?.head;
                const funcSig = c.sema.getFuncSig(data.func.funcSigId);
                const params = funcSig.params();
                try c.pushSpanFmt("{s}_{s}(f, (Value[]){{", .{sym.parent.?.name(), sym.name()});
                if (args.len > 0) {
                    _ = try genExpr(c, args[0], Cstr.init(params[0]));
                    for (args[1..], 1..) |argIdx, i| {
                        try c.pushSpan(", ");
                        _ = try genExpr(c, argIdx, Cstr.init(params[i]));
                    }
                }
                try c.pushSpanFmt("}}, {}", .{args.len});

                try c.pushSpan(")");
            },
            else => return error.TODO,
        }

        try c.pushSpan(")");

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
}

fn genSymbol(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = cstr;
    _ = nodeId;

    const data = c.ir.getExprData(idx, .symbol);

    const symId = try c.compiler.ensureSym(data.name);
    // const inst = try c.rega.selectForNoErrInst(cstr, false);
    // if (inst.requiresPreRelease) {
    //     try pushRelease(c, inst.dst, nodeId);
    // }

    try c.pushSpanFmt("BOX_SYM({})", .{symId});
    
    // const val = genValue(c, inst.dst, false);
    // return finishInst(c, val, inst.finalDst);
    return Value{};
}

fn genInt(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = nodeId;

    const data = c.ir.getExprData(idx, .int);

    // const inst = try c.rega.selectForNoErrInst(cstr, false);
    // if (inst.requiresPreRelease) {
    //     try pushRelease(c, inst.dst, nodeId);
    // }

    const toVal = cstr.dstType == bt.Any;
    if (toVal) {
        try c.pushSpan("BOX_INT(");
    }

    try c.pushSpanFmt("{}", .{data.val});
    // const value = try genConstIntExt(c, data.val, inst.dst, c.desc(nodeId));
    // return finishInst(c, value, inst.finalDst);

    if (toVal) {
        try c.pushSpan(")");
    }

    return Value{}  ;
}

fn exprStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .exprStmt);
    _ = data;

    // const cstr = RegisterCstr.initSimple(data.returnMain);

    try c.beginLine(nodeId);
    const b = c.block();
    try c.pushSpanFmt("Value tmp{} = ", .{ b.nextTemp() });
    const expr = c.ir.advanceStmt(idx, .exprStmt);
    const exprv = try genTopExpr(c, expr, Cstr.none);
    _ = exprv;
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

fn genLocal(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !Value {
    _ = nodeId;

    const data = c.ir.getExprData(idx, .local);
    const b = c.block();
    log.tracev(c.vm, "local: {}", .{data.id});
    const local = c.localStack.items[b.localStart + data.id];

    if (!local.some.boxed) {
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

        const toVal = if (cstr.dstType) |dstType| b: {
            break :b cIsBoxedType(dstType) and !local.some.boxed;
        } else false;
        if (toVal) {
            try c.pushSpanFmt("{s}(", .{ cBoxMacro(local.some.type) });
        }

        try c.pushSpan(local.some.name);

        if (toVal) {
            try c.pushSpan(")");
        }
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

fn funcDecl(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .funcDecl);
    const func = data.func;
    const paramsIdx = c.ir.advanceStmt(idx, .funcDecl);
    const params = c.ir.getArray(paramsIdx, ir.FuncParam, func.numParams);

    // try pushFuncBlock(c, data, params, nodeId);

    try c.beginLine(nodeId);
    const modName = cSymName(c, func.sym.?.head.parent.?);
    try c.pushSpanFmt("{s} {s}_{s}(Fiber* f", .{ try cTypeName(c.sema, func.retType), modName, func.sym.?.head.name() });
    if (params.len > 0) {
        for (params) |param| {
            try c.pushSpanFmt(", {s} {s}", .{try cTypeName(c.sema, param.declType), param.name()});
        }
    }
    try c.pushSpanEnd(") {");
    try c.pushBlock(Block.initFunc(func));

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
        @memset(c.localStack.items[start..], .{ .und = {} });
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
                    .boxed = cIsBoxedType(param.declType),
                    .type = param.declType,
                },
            };
        }
        log.tracev(c.vm, "reserve param: {}", .{i});
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
    c.popBlock();
    try c.pushLineNoMapping("}");
    try c.pushNewLine();
}

fn ifStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .ifStmt);
    // const bodyEndJumpsStart = c.listDataStack.items.len;
    // _ = bodyEndJumpsStart;

    try c.beginLine(nodeId);
    try c.pushSpan("if (");
    var condIdx = c.ir.advanceStmt(idx, .ifStmt);
    var condNodeId = c.ir.getNode(condIdx);
    _ = condNodeId;
    var condv = try genTopExpr(c, condIdx, Cstr.init(bt.Boolean));
    _ = condv;
    try c.pushSpanEnd(") {");

    // // ARC cleanup for true case.
    // if (unwindAndFreeTemp(c, condv)) {
    //     try pushRelease(c, condv.local, condNodeId);
    // }

    c.pushSubBlock();
    try genStmts(c, data.bodyHead);
    c.popSubBlock();
    try c.pushLineNoMapping("}");

    var hasElse = false;
    _ = hasElse;

    if (data.numElseBlocks > 0) {
        // const elseBlocks = c.ir.getArray(data.elseBlocks, u32, data.numElseBlocks);

        // for (elseBlocks) |elseIdx| {
        //     const elseBlockNodeId = c.ir.getNode(elseIdx);
        //     const elseBlock = c.ir.getExprData(elseIdx, .elseBlock);

        //     const bodyEndJump = try c.pushEmptyJump();
        //     try c.listDataStack.append(c.alloc, .{ .pc = bodyEndJump });

        //     // Jump here from prev case miss.
        //     c.patchJumpNotCondToCurPc(prevCaseMissJump);

        //     if (!elseBlock.isElse) {
        //         condIdx = c.ir.advanceExpr(elseIdx, .elseBlock);
        //         condNodeId = c.ir.getNode(condIdx);
        //         condv = try genExpr(c, condIdx, RegisterCstr.simple);
        //         prevCaseMissJump = try c.pushEmptyJumpNotCond(condv.local);

        //         // ARC cleanup for true case.
        //         if (unwindAndFreeTemp(c, condv)) {
        //             try pushRelease(c, condv.local, condNodeId);
        //         }
        //     } else {
        //         hasElse = true;
        //     }

        //     try pushSubBlock(c, false, elseBlockNodeId);
        //     try genStmts(c, elseBlock.bodyHead);
        //     try popSubBlock(c);
        // }
        return error.TODO;
    }

    // // Jump here from all body ends.
    // const bodyEndJumps = c.listDataStack.items[bodyEndJumpsStart..];
    // for (bodyEndJumps) |jump| {
    //     c.patchJumpToCurPc(jump.pc);
    // }
    // c.listDataStack.items.len = bodyEndJumpsStart;
}

const BinOpOptions = struct {
    left: ?Value = null,
};

fn genBinOp(c: *Chunk, idx: usize, cstr: Cstr, opts: BinOpOptions, nodeId: cy.NodeId) !Value {
    _ = cstr;
    const data = c.ir.getExprData(idx, .preBinOp).binOp;
    log.tracev(c.vm, "binop {} {}", .{data.op, data.leftT});

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

    // var prefer = PreferDst{
    //     .dst = inst.dst,
    //     .canUseDst = !c.isParamOrLocalVar(inst.dst),
    // };

    // Lhs.
    var leftv: Value = undefined;
    if (opts.left) |left| {
        leftv = left;
    } else {
        const leftIdx = c.ir.advanceExpr(idx, .preBinOp);
        leftv = try genExpr(c, leftIdx, Cstr.init(data.leftT));
    }

    var retained = false;
    _ = retained;
    switch (data.op) {
        .index => {
            // if (data.leftT == bt.List) {
            //     try pushInlineBinExpr(c, .indexList, leftv.local, rightv.local, inst.dst, nodeId);
            // } else if (data.leftT == bt.Tuple) {
            //     try pushInlineBinExpr(c, .indexTuple, leftv.local, rightv.local, inst.dst, nodeId);
            // } else if (data.leftT == bt.Map) {
            //     try pushInlineBinExpr(c, .indexMap, leftv.local, rightv.local, inst.dst, nodeId);
            // } else return error.Unexpected;
            // retained = true;
            return error.TODO;
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
                    try c.pushSpan(cBinOpLit(data.op));
                } else {
                    return error.TODO;
                }
            } else if (data.leftT == bt.Integer) {
                if (data.rightT == bt.Integer) {
                    try c.pushSpan(cBinOpLit(data.op));
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
            return c.base.reportErrorAt("Unsupported op: {}", &.{v(data.op)}, nodeId);
        },
    }

    // Rhs.
    const rightv = try genExpr(c, data.right, Cstr.init(data.rightT));
    _ = rightv;

    // const leftRetained = if (opts.left == null) unwindTempKeepDst(c, leftv, inst.dst) else false;
    // const rightRetained = unwindTempKeepDst(c, rightv, inst.dst);

    // // ARC cleanup.
    // try pushReleaseOpt2(c, leftRetained, leftv.local, rightRetained, rightv.local, nodeId);

    // const val = genValue(c, inst.dst, retained);
    // return finishInst(c, val, inst.finalDst);
    return Value{};
}

/// Get shortened C symbol name.
/// eg. "/a/b/c/d.cy" is reduced to "d"
pub fn cSymName(c: *Chunk, sym: *cy.Sym) []const u8 {
    const res = c.compiler.cSymNameOverrides.get(sym) orelse return sym.name();
    return res.name();
}

fn cTypeName(sema: *cy.Sema, id: cy.TypeId) ![]const u8 {
    return switch (id) {
        // bt.String,
        // bt.Array,
        // bt.List,
        // bt.ListIter,
        // bt.Map,
        // bt.MapIter,
        // bt.Pointer,
        // bt.Fiber,
        // bt.MetaType,
        // bt.Dynamic,
        // bt.ExternFunc,
        // bt.Any => return true,
        bt.Integer => "i48",
        // bt.Float,
        // bt.Symbol,
        // bt.None,
        // bt.Error,
        // // bt.Undefined,
        // bt.Boolean => return false,
        // else => {
        //     const sym = s.getTypeSym(id);
        //     switch (sym.type) {
        //         .hostObjectType,
        //         .object => return true,
        //         .enumType => return false,
        //         else => {
        //             cy.panicFmt("Unexpected sym type: {} {}", .{id, sym.type});
        //         }
        //     }
        // }
        else => {
            rt.logZFmt(sema.compiler.vm, "Unsupported sym type: {}", .{id});
            return error.TODO;
        }
    };
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

fn retExprStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const childIdx = c.ir.advanceStmt(idx, .retExprStmt);

    // TODO: If the returned expr is a local, consume the local after copying to reg 0.
    try c.beginLine(nodeId);
    try c.pushSpan("return ");
    var childv: Value = undefined;
    const b = c.block();
    if (b.type == .main) {
        // // Main block.
        // childv = try genExpr(c, childIdx, RegisterCstr.simpleMustRetain);
        return error.TODO;
    } else {
        childv = try genTopExpr(c, childIdx, Cstr.init(b.data.func.retType));
    }
    try c.pushSpanEnd(";");

    // _ = unwindAndFreeTemp(c, childv);

    // try genBlockReleaseLocals(c);
    // if (c.curBlock.type == .main) {
    //     try c.buf.pushOp1(.end, @intCast(childv.local));
    // } else {
    //     try c.buf.pushOp(.ret1);
    // }
}