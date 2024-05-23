const std = @import("std");
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const bt = cy.types.BuiltinTypes;
const build_options = @import("build_options");
const rt = cy.rt;
const log = cy.log.scoped(.cy);
const ast = cy.ast;

const Src = @embedFile("cy.cy");
const zErrFunc = cy.builtins.zErrFunc;

pub fn create(vm: *cy.VM, r_uri: []const u8) C.Module {
    const core_data = vm.getData(*cy.builtins.CoreData, "core");
    _ = core_data;
    const mod = C.createModule(@ptrCast(vm), C.toStr(r_uri), C.toStr(Src));

    var config = C.ModuleConfig{
        .funcs = C.toSlice(C.HostFuncEntry, &funcs),
    };
    C.setModuleConfig(@ptrCast(vm), mod, &config);
    return mod;
}

const func = cy.hostFuncEntry;
const funcs = [_]C.HostFuncEntry{
    func("eval",           zErrFunc(eval)),
    func("parse",          zErrFunc(parse)),
    func("parseCyon",      zErrFunc(parseCyon)),
    func("repl",           zErrFunc(repl)),
    func("toCyon",         zErrFunc(toCyon)),
};

pub fn parse(vm: *cy.VM, args: [*]const cy.Value, _: u8) anyerror!cy.Value {
    const src = args[0].asString();

    var parser: cy.Parser = undefined;
    try parser.init(vm.alloc);
    defer parser.deinit();
    _ = try parser.parse(src, .{ .parseComments = true });

    return parseCyberGenResult(vm, &parser);
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
    const res = try vm.allocEmptyMap();
    const map = res.castHeapObject(*cy.heap.Map);
    switch (node.type()) {
        .funcParam => {
            const param = node.cast(.funcParam);
            const name = view.funcParamName(param);
            try vm.mapSet(map, try vm.retainOrAllocAstring("name"), try vm.allocString(name));

            const typeSpec = try genTypeSpecString(vm, view, param.typeSpec);
            try vm.mapSet(map, try vm.retainOrAllocAstring("typeSpec"), typeSpec);
        },
        else => {},
    }
    return res;
}

fn genImplicitFuncDeclEntry(vm: *cy.VM, view: ast.AstView, node: *ast.FuncDecl, state: *ParseCyberState) !cy.Value {
    const entryv = try vm.allocEmptyMap();
    const entry = entryv.castHeapObject(*cy.heap.Map);
    const name = view.getNamePathInfo(node.name).name_path;
    
    const params = try vm.allocEmptyListDyn();
    for (node.params) |param| {
        const param_v = try genNodeValue(vm, view, @ptrCast(param));
        try params.asHeapObject().list.append(vm.alloc, param_v);
    }
    try vm.mapSet(entry, try vm.retainOrAllocAstring("params"), params);

    const ret = try genTypeSpecString(vm, view, node.ret);
    try vm.mapSet(entry, try vm.retainOrAllocAstring("ret"), ret);

    const hidden = cy.Value.initBool(node.hidden);
    try vm.mapSet(entry, try vm.retainOrAllocAstring("hidden"), hidden);

    state.pos = node.pos;
    state.node = @ptrCast(node);

    // Find doc comments.
    if (try genDocComment(vm, view, .funcDecl, state)) |docStr| {
        try vm.mapSet(entry, try vm.retainOrAllocAstring("docs"), docStr);
    }

    try vm.mapSet(entry, try vm.retainOrAllocAstring("name"), try vm.retainOrAllocAstring(name));
    try vm.mapSet(entry, try vm.retainOrAllocAstring("pos"), cy.Value.initInt(@intCast(node.pos)));
    return entryv;
}

fn genDeclEntry(vm: *cy.VM, view: ast.AstView, decl: *ast.Node, state: *ParseCyberState) !cy.Value {
    const entryv = try vm.allocEmptyMap();
    const entry = entryv.castHeapObject(*cy.heap.Map);
    const name = try view.declNamePath(decl);
    try vm.mapSet(entry, try vm.retainOrAllocAstring("type"), try vm.retainOrAllocAstring(@tagName(decl.type())));

    switch (decl.type()) {
        .staticDecl => {
            const static_decl = decl.cast(.staticDecl);
            const typeSpec = try genTypeSpecString(vm, view, static_decl.typeSpec);
            try vm.mapSet(entry, try vm.retainOrAllocAstring("typeSpec"), typeSpec);
        },
        .enumDecl,
        .use_alias,
        .import_stmt,
        .specialization,
        .typeAliasDecl => {},
        .template => {
            const template = decl.cast(.template);
            const child = try genDeclEntry(vm, view, template.decl, state);
            try vm.mapSet(entry, try vm.retainOrAllocAstring("child"), child);
        },
        .funcDecl => {
            const func_decl = decl.cast(.funcDecl);

            const params = try vm.allocEmptyListDyn();
            for (func_decl.params) |param| {
                const param_v = try genNodeValue(vm, view, @ptrCast(param));
                try params.asHeapObject().list.append(vm.alloc, param_v);
            }
            try vm.mapSet(entry, try vm.retainOrAllocAstring("params"), params);

            const hidden = cy.Value.initBool(func_decl.hidden);
            try vm.mapSet(entry, try vm.retainOrAllocAstring("hidden"), hidden);

            const ret = try genTypeSpecString(vm, view, func_decl.ret);
            try vm.mapSet(entry, try vm.retainOrAllocAstring("ret"), ret);
        },
        .table_decl,
        .structDecl,
        .objectDecl => {
            const object_decl = decl.cast(.objectDecl);
            const funcs_ = try vm.allocEmptyListDyn();
            for (object_decl.funcs) |func_decl| {
                const f = try genImplicitFuncDeclEntry(vm, view, func_decl, state);
                try funcs_.asHeapObject().list.append(vm.alloc, f);
            }
            try vm.mapSet(entry, try vm.retainOrAllocAstring("funcs"), funcs_);
        },
        .custom_decl => {
            const custom_decl = decl.cast(.custom_decl);
            const funcs_ = try vm.allocEmptyListDyn();
            for (custom_decl.funcs) |func_decl| {
                const f = try genImplicitFuncDeclEntry(vm, view, func_decl, state);
                try funcs_.asHeapObject().list.append(vm.alloc, f);
            }
            try vm.mapSet(entry, try vm.retainOrAllocAstring("funcs"), funcs_);
        },
        .distinct_decl => {
            const distinct_decl = decl.cast(.distinct_decl);
            const funcs_ = try vm.allocEmptyListDyn();
            for (distinct_decl.funcs) |func_decl| {
                const f = try genImplicitFuncDeclEntry(vm, view, func_decl, state);
                try funcs_.asHeapObject().list.append(vm.alloc, f);
            }
            try vm.mapSet(entry, try vm.retainOrAllocAstring("funcs"), funcs_);
        },
        else => {
            log.tracev("{}", .{decl.type()});
            return error.Unsupported;
        },
    }
    state.pos = decl.pos();
    state.node = decl;

    // Find doc comments.
    if (try genDocComment(vm, view, decl.type(), state)) |docStr| {
        try vm.mapSet(entry, try vm.retainOrAllocAstring("docs"), docStr);
    }

    try vm.mapSet(entry, try vm.retainOrAllocAstring("name"), try vm.retainOrAllocAstring(name));
    try vm.mapSet(entry, try vm.retainOrAllocAstring("pos"), cy.Value.initInt(@intCast(decl.pos())));
    return entryv;
}

fn genDocComment(vm: *cy.VM, view: ast.AstView, decl_type: ast.NodeType, state: *ParseCyberState) !?cy.Value {
    const comments = state.comments;
    if (state.commentIdx < comments.len) {
        var docStartIdx = state.commentIdx;
        var docEndIdx = state.commentIdx;
        while (state.commentIdx < comments.len) {
            var commentPos = comments[state.commentIdx];
            if (commentPos.start > state.pos) {
                break;
            }
            state.commentIdx += 1;
            docEndIdx = state.commentIdx;
            if (commentPos.len() < 3 or !std.mem.eql(u8, "--|", view.src[commentPos.start..commentPos.start+3])) {
                // Not a doc comment, reset.
                docStartIdx = state.commentIdx;
                continue;
            }
            // Check it is connected to last comment.
            if (docEndIdx > docStartIdx + 1) {
                const last = comments[docEndIdx - 2];
                if (!view.isAdjacentLine(last.end, commentPos.start)) {
                    // Reset.
                    docStartIdx = state.commentIdx;
                    continue;
                }
            }
        }
        if (docEndIdx > docStartIdx) {
            // Check it is connected to last comment.
            const last = comments[docEndIdx - 1];

            var posWithModifiers = state.pos;
            switch (decl_type) {
                .staticDecl => {
                    const decl = state.node.cast(.staticDecl);
                    if (decl.attrs.len > 0) {
                        posWithModifiers = decl.attrs[0].pos - 1;
                    }
                },
                .funcDecl => {
                    const decl = state.node.cast(.funcDecl);
                    if (decl.attrs.len > 0) {
                        posWithModifiers = decl.attrs[0].pos - 1;
                    }
                },
                .objectDecl => {
                    const decl = state.node.cast(.objectDecl);
                    if (decl.attrs.len > 0) {
                        posWithModifiers = decl.attrs[0].pos - 1;
                    }
                },
                else => {},
            }

            if (view.isAdjacentLine(last.end, posWithModifiers)) {
                for (comments[docStartIdx..docEndIdx]) |docPos| {
                    try state.sb.appendSlice(vm.alloc, view.src[docPos.start+3..docPos.end]);
                    try state.sb.append(vm.alloc, ' ');
                }
                const finalStr = std.mem.trim(u8, state.sb.items, " ");
                defer state.sb.clearRetainingCapacity();
                return try vm.allocString(finalStr);
            }
        }
    }
    return null;
}

fn parseCyberGenResult(vm: *cy.VM, parser: *const cy.Parser) !cy.Value {
    const root = try vm.allocEmptyMap();
    errdefer vm.release(root);

    const map = root.asHeapObject().map.map();

    const decls = try vm.allocEmptyListDyn();
    errdefer vm.release(decls);

    const declsList = decls.asHeapObject().list.getList();

    var state = ParseCyberState{
        .comments = parser.ast.comments.items,
        .commentIdx = 0,
        .sb = .{},
        .pos = undefined,
        .node = undefined,
    };
    defer state.sb.deinit(vm.alloc);

    const view = parser.ast.view();

    for (parser.staticDecls.items) |decl| {
        const entry = try genDeclEntry(vm, view, decl, &state);
        try declsList.append(vm.alloc, entry);
    }
    try map.put(vm.alloc, try vm.retainOrAllocAstring("decls"), decls);

    return root;
}

pub fn parseCyon(vm: *cy.VM, args: [*]const cy.Value, _: u8) anyerror!cy.Value {
    const src = args[0].asString();

    var parser: cy.Parser = undefined;
    try parser.init(vm.alloc);
    defer parser.deinit();
    const val = try cy.decodeCyon(vm.alloc, &parser, src);
    return fromCyonValue(vm, val);
}

fn fromCyonValue(vm: *cy.VM, val: cy.DecodeValueIR) !cy.Value {
    switch (val.getValueType()) {
        .list => {
            var dlist = try val.getList();
            defer dlist.deinit();
            const elems = try vm.alloc.alloc(cy.Value, dlist.arr.len);
            for (elems, 0..) |*elem, i| {
                elem.* = try fromCyonValue(vm, dlist.getIndex(i));
            }
            return try cy.heap.allocOwnedList(vm, elems);
        },
        .map => {
            var dmap = try val.getMap();
            defer dmap.deinit();
            var iter = dmap.iterator();

            const mapVal = try vm.allocEmptyMap();
            const map = mapVal.asHeapObject();
            while (iter.next()) |entry| {
                const child = try fromCyonValue(vm, dmap.getValue(entry.key_ptr.*));
                const key = try vm.allocString(entry.key_ptr.*);
                try map.map.setConsume(vm, key, child);
            }
            return mapVal;
        },
        .string => {
            const str = try val.allocString();
            defer val.alloc.free(str);
            // TODO: Use allocOwnedString
            return try vm.allocString(str);
        },
        .integer => {
            return cy.Value.initInt(try val.getInt());
        },
        .float => {
            return cy.Value.initF64(try val.getF64());
        },
        .bool => {
            return cy.Value.initBool(val.getBool());
        },
    }
}

pub fn toCyon(vm: *cy.VM, args: [*]const cy.Value, _: u8) anyerror!cy.Value {
    const res = try allocToCyon(vm, vm.alloc, args[0]);
    defer vm.alloc.free(res);
    return vm.allocString(res);
}

pub fn allocToCyon(vm: *cy.VM, alloc: std.mem.Allocator, root: cy.Value) ![]const u8 {
    const S = struct {
        fn encodeTable(ctx: *cy.EncodeTableContext, val: cy.Value) anyerror!void {
            const uservm = cy.ptrAlignCast(*cy.VM, ctx.user_ctx);
            var iter = val.asHeapObject().table.map().iterator();
            while (iter.next()) |e| {
                const key = try uservm.getOrBufPrintValueStr(&cy.tempBuf, e.key);
                switch (e.value.getTypeId()) {
                    bt.Float => {
                        try ctx.encodeFloat(key, e.value.asF64());
                    },
                    bt.Integer => {
                        try ctx.encodeInt(key, e.value.asInteger());
                    },
                    bt.String => {
                        const keyDupe = try uservm.alloc.dupe(u8, key);
                        defer uservm.alloc.free(keyDupe);
                        const str = try uservm.getOrBufPrintValueStr(&cy.tempBuf, e.value);
                        try ctx.encodeString(keyDupe, str);
                    },
                    bt.Boolean => {
                        try ctx.encodeBool(key, e.value.asBool());
                    },
                    bt.Table => {
                        try ctx.encodeTable(key, e.value, encodeTable);
                    },
                    bt.ListDyn => {
                        try ctx.encodeList(key, e.value, encodeList);
                    },
                    else => {},
                }
            }
        }
        fn encodeList(ctx: *cy.EncodeListContext, val: cy.Value) anyerror!void {
            const items = val.asHeapObject().list.items();
            for (items) |it| {
                try ctx.indent();
                switch (it.getTypeId()) {
                    bt.Float => {
                        try ctx.encodeFloat(it.asF64());
                        _ = try ctx.writer.write(",\n");
                    },
                    bt.Integer => {
                        try ctx.encodeInt(it.asInteger());
                        _ = try ctx.writer.write(",\n");
                    },
                    bt.String => {
                        const uservm = cy.ptrAlignCast(*cy.VM, ctx.user_ctx);
                        const str = try uservm.getOrBufPrintValueStr(&cy.tempBuf, it);
                        try ctx.encodeString(str);
                        _ = try ctx.writer.write(",\n");
                    },
                    bt.Boolean => {
                        try ctx.encodeBool(it.asBool());
                        _ = try ctx.writer.write(",\n");
                    },
                    bt.Table => {
                        try ctx.encodeTable(it, encodeTable);
                        _ = try ctx.writer.write(",\n");
                    },
                    bt.ListDyn => {
                        try ctx.encodeList(it, encodeList);
                        _ = try ctx.writer.write(",\n");
                    },
                    else => {},
                }
            }
        }
        fn encodeRoot(ctx: *cy.EncodeValueContext, val: anytype) !void {
            const T = @TypeOf(val);
            if (T == cy.Value) {
                switch (val.getTypeId()) {
                    bt.Float => {
                        try ctx.encodeFloat(val.asF64());
                    },
                    bt.Integer => {
                        try ctx.encodeInt(val.asInteger());
                    },
                    bt.String => {
                        const uservm = cy.ptrAlignCast(*cy.VM, ctx.user_ctx);
                        const str = try uservm.getOrBufPrintValueStr(&cy.tempBuf, val);
                        try ctx.encodeString(str);
                    },
                    bt.Boolean => {
                        try ctx.encodeBool(val.asBool());
                    },
                    bt.ListDyn => {
                        if (val.asHeapObject().list.items().len == 0) {
                            _ = try ctx.writer.write("[]");
                        } else {
                            try ctx.encodeList(val, encodeList);
                        }
                    },
                    bt.Table => {
                        if (val.asHeapObject().table.map().size == 0) {
                            _ = try ctx.writer.write("{}");
                        } else {
                            try ctx.encodeTable(val, encodeTable);
                        }
                    },
                    else => {},
                }
            } else {
                cy.panicFmt("unsupported: {s}", .{@typeName(T)});
            }
        }
    };
    return try cy.encodeCyon(alloc, vm, root, S.encodeRoot);
}

pub const IReplReadLine = struct {
    ptr: *anyopaque,
    read: *const fn(ptr: *anyopaque, prefix: [:0]const u8) anyerror![]const u8,
    free: *const fn(ptr: *anyopaque, line: []const u8) void,
};

fn getReplPrefix(buf: []u8, indent: u32, head: []const u8) ![:0]const u8 {
    var fbuf = std.io.fixedBufferStream(buf);
    const w = fbuf.writer();
    try w.writeByteNTimes(' ', indent * 4);
    try w.writeAll(head);
    try w.writeByte(0);
    const slice = fbuf.getWritten();
    return slice[0..slice.len-1 :0];
}

pub fn repl2(vm: *cy.VM, config: C.EvalConfig, read_line: IReplReadLine) !void {
    const alloc = vm.alloc;

    // TODO: Record inputs that successfully compiled. Can then be exported to file.

    // Initial input includes `use $global`.
    // Can also include additional source if needed.
    const init_src =
        \\use $global
        \\
        ;
    var res: C.Value = undefined;
    _ = C.evalExt(@ptrCast(vm), C.toStr("repl_init"), C.toStr(init_src), config, &res);

    // Build multi-line input.
    var input_builder: std.ArrayListUnmanaged(u8) = .{};
    defer input_builder.deinit(alloc);
    var indent: u32 = 0;

    var prefix_buf: [1024]u8 = undefined;

    rt.printZFmt(vm, "{s} REPL\n", .{build_options.full_version});
    rt.print(vm, "Commands: .exit\n");
    while (true) {
        const prefix_head = if (indent == 0) "> " else "| ";
        const prefix = try getReplPrefix(&prefix_buf, indent, prefix_head);
        const read_res = try read_line.read(read_line.ptr, prefix);
        defer read_line.free(read_line.ptr, read_res);

        // Dupe line so that a consistent allocator is used.
        var input = try alloc.dupe(u8, read_res);
        defer alloc.free(input);

        if (std.mem.eql(u8, ".exit", input)) {
            break;
        }

        if (std.mem.endsWith(u8, input, ":")) {
            try input_builder.appendSlice(alloc, input);
            indent += 1;
            continue;
        }

        if (input_builder.items.len > 0) {
            if (input.len == 0) {
                indent -= 1;
                if (indent > 0) {
                    continue;
                } else {
                    // Build input and submit.
                    alloc.free(input);
                    input = try input_builder.toOwnedSlice(alloc);
                }
            } else {
                try input_builder.append(alloc, '\n');
                try input_builder.appendNTimes(alloc, ' ', indent * 4);
                try input_builder.appendSlice(alloc, input);
                continue;
            }
        }

        const val_opt: ?cy.Value = vm.eval("input", input, config) catch |err| b: {
            switch (err) {
                error.Panic => {
                    const report = C.newPanicSummary(@ptrCast(vm));
                    defer C.free(@ptrCast(vm), report);
                    rt.err(vm, C.fromStr(report));
                },
                error.CompileError => {
                    const report = C.newErrorReportSummary(@ptrCast(vm));
                    defer C.free(@ptrCast(vm), report);
                    rt.err(vm, C.fromStr(report));
                },
                else => {
                    rt.errZFmt(vm, "unexpected {}\n", .{err});
                },
            }
            break :b null;
        };

        if (val_opt) |val| {
            if (!val.isVoid()) {
                const str = C.newValueDump(@ptrCast(vm), @bitCast(val));
                defer C.free(@ptrCast(vm), str);
                rt.printZFmt(vm, "{s}\n", .{C.fromStr(str)});
            }
        }
    }
}

fn eval(vm: *cy.VM, args: [*]const cy.Value, _: u8) anyerror!cy.Value {
    // Create an isolated VM.
    const ivm: *cy.VM = @ptrCast(@alignCast(C.create()));
    defer C.destroy(@ptrCast(ivm));

    // Use the same printers as the parent VM.
    C.setPrinter(@ptrCast(ivm), C.getPrinter(@ptrCast(vm)));
    C.setErrorPrinter(@ptrCast(ivm), C.getErrorPrinter(@ptrCast(vm)));

    var config = C.defaultEvalConfig();
    config.single_run = false;
    config.file_modules = false;
    config.reload = false;
    config.backend = C.BackendVM;
    config.spawn_exe = false;

    const src = args[0].asString();
    var val: cy.Value = undefined;
    const res = C.evalExt(@ptrCast(ivm), C.toStr("eval"), C.toStr(src), config, @ptrCast(&val));
    if (res != C.Success) {
        switch (res) {
            C.ErrorCompile => {
                const report = C.newErrorReportSummary(@ptrCast(ivm));
                defer C.free(@ptrCast(ivm), report);
                rt.err(vm, C.fromStr(report));
            },
            C.ErrorPanic => {
                const report = C.newPanicSummary(@ptrCast(ivm));
                defer C.free(@ptrCast(ivm), report);
                rt.err(vm, C.fromStr(report));
            },
            else => {
                rt.err(vm, "unknown error\n");
            },
        }
        return error.EvalError;
    }

    switch (val.getTypeId()) {
        bt.Boolean,
        bt.Integer,
        bt.Float => {
            return val;
        },
        bt.String => {
            defer ivm.release(val);
            return vm.allocString(val.asString());
        },
        else => {
            defer ivm.release(val);
            return error.InvalidResult;
        }
    }
}

const VmReadLine = struct {
    vm: *cy.VM,
    read_line: cy.Value,

    fn read(ptr: *anyopaque, prefix: [:0]const u8) anyerror![]const u8 {
        const self: *@This() = @ptrCast(@alignCast(ptr));
        const vm_prefix = try self.vm.allocString(prefix);
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

pub fn repl(vm: *cy.VM, args: [*]const cy.Value, _: u8) anyerror!cy.Value {
    // TODO: Allow returning a result (primitive or String).

    // Create an isolated VM.
    const ivm: *cy.VM = @ptrCast(@alignCast(c.create()));
    defer c.destroy(@ptrCast(ivm));

    // Use the same printers as the parent VM.
    c.setPrinter(@ptrCast(ivm), c.getPrinter(@ptrCast(vm)));
    c.setErrorPrinter(@ptrCast(ivm), c.getErrorPrinter(@ptrCast(vm)));

    var config = c.defaultEvalConfig();
    config.single_run = false;
    config.file_modules = false;
    config.reload = false;
    config.backend = c.BackendVM;
    config.spawn_exe = false;

    var read_line = VmReadLine{
        .vm = vm, 
        .read_line = args[0],
    };

    try repl2(@ptrCast(ivm), config, .{
        .ptr = &read_line,
        .read = VmReadLine.read,
        .free = VmReadLine.free,
    });

    return cy.Value.Void;
}
