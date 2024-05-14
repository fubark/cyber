const std = @import("std");
const cy = @import("../cyber.zig");
const c = @import("../capi.zig");
const bt = cy.types.BuiltinTypes;
const build_options = @import("build_options");
const rt = cy.rt;

pub const Src = @embedFile("cy.cy");
const zErrFunc = cy.builtins.zErrFunc;

const NameFunc = struct { []const u8, cy.ZHostFuncFn };
const mod_funcs = [_]NameFunc{
    .{"eval",           zErrFunc(eval)},
    .{"parse",          zErrFunc(parse)},
    .{"parseCyon",      zErrFunc(parseCyon)},
    .{"repl",           zErrFunc(repl)},
    .{"toCyon",         zErrFunc(toCyon)},
};

pub fn funcLoader(_: ?*c.VM, func: c.FuncInfo, out_: [*c]c.FuncResult) callconv(.C) bool {
    const out: *c.FuncResult = out_;
    const name = c.fromStr(func.name);
    if (std.mem.eql(u8, mod_funcs[func.idx].@"0", name)) {
        out.ptr = @ptrCast(mod_funcs[func.idx].@"1");
        return true;
    }
    return false;
}

pub fn parse(vm: *cy.VM, args: [*]const cy.Value, _: u8) anyerror!cy.Value {
    const src = args[0].asString();

    var parser = try cy.Parser.init(vm.alloc);
    defer parser.deinit();
    _ = try parser.parse(src, .{ .parseComments = true });

    return parseCyberGenResult(vm, &parser);
}

const ParseCyberState = struct {
    comments: []const cy.IndexSlice(u32),
    sb: std.ArrayListUnmanaged(u8),
    commentIdx: u32,
    pos: u32,
    node: cy.Node,
};

fn genTypeSpecString(vm: *cy.VM, ast: cy.ast.AstView, exprId: cy.NodeId) !cy.Value {
    if (exprId != cy.NullNode) {
        var sb: std.ArrayListUnmanaged(u8) = .{};
        defer sb.deinit(vm.alloc);

        var enc = cy.ast.Encoder{ .ast = ast };
        try enc.write(sb.writer(vm.alloc), exprId);

        return try vm.retainOrAllocAstring(sb.items);
    } else {
        return try vm.retainOrAllocAstring("");
    }
}

fn genNodeValue(vm: *cy.VM, ast: cy.ast.AstView, nodeId: cy.NodeId) !cy.Value {
    const node = ast.node(nodeId);
    const res = try vm.allocEmptyMap();
    const map = res.castHeapObject(*cy.heap.Map);
    switch (node.type()) {
        .funcHeader => {
            const name = ast.getNamePathInfo(node.data.funcHeader.name).name_path;
            try vm.mapSet(map, try vm.retainOrAllocAstring("name"), try vm.allocString(name));

            const params = try vm.allocEmptyListDyn();
            var paramId: cy.NodeId = node.data.funcHeader.paramHead;
            while (paramId != cy.NullNode) {
                const param = try genNodeValue(vm, ast, paramId);
                try params.asHeapObject().list.append(vm.alloc, param);
                paramId = ast.node(paramId).next();
            }
            try vm.mapSet(map, try vm.retainOrAllocAstring("params"), params);

            const ret = try genTypeSpecString(vm, ast, node.funcHeader_ret());
            try vm.mapSet(map, try vm.retainOrAllocAstring("ret"), ret);
        },
        .funcParam => {
            var name = ast.nodeStringById(node.data.funcParam.name);
            try vm.mapSet(map, try vm.retainOrAllocAstring("name"), try vm.allocString(name));

            const typeSpec = try genTypeSpecString(vm, ast, node.data.funcParam.typeSpec);
            try vm.mapSet(map, try vm.retainOrAllocAstring("typeSpec"), typeSpec);
        },
        else => {},
    }
    return res;
}

fn genImplicitFuncDeclEntry(vm: *cy.VM, ast: cy.ast.AstView, node_id: cy.NodeId, state: *ParseCyberState) !cy.Value {
    const entryv = try vm.allocEmptyMap();
    const entry = entryv.castHeapObject(*cy.heap.Map);
    var name: []const u8 = undefined;
    const node = ast.node(node_id);

    const header = ast.node(node.data.func.header);
    name = ast.getNamePathInfo(header.data.funcHeader.name).name_path;
    
    const headerv = try genNodeValue(vm, ast, node.data.func.header);
    try vm.mapSet(entry, try vm.retainOrAllocAstring("header"), headerv);

    state.pos = node.srcPos;
    state.node = node;

    // Find doc comments.
    if (try genDocComment(vm, ast, .func, state)) |docStr| {
        try vm.mapSet(entry, try vm.retainOrAllocAstring("docs"), docStr);
    }

    try vm.mapSet(entry, try vm.retainOrAllocAstring("name"), try vm.retainOrAllocAstring(name));
    try vm.mapSet(entry, try vm.retainOrAllocAstring("pos"), cy.Value.initInt(@intCast(node.srcPos)));
    return entryv;
}

fn genDeclEntry(vm: *cy.VM, ast: cy.ast.AstView, decl: cy.parser.StaticDecl, state: *ParseCyberState) !cy.Value {
    const entryv = try vm.allocEmptyMap();
    const entry = entryv.castHeapObject(*cy.heap.Map);
    try vm.mapSet(entry, try vm.retainOrAllocAstring("type"), try vm.retainOrAllocAstring(@tagName(decl.declT)));
    const name = try ast.declNamePath(decl.nodeId);
    const node = ast.node(decl.nodeId);

    switch (decl.declT) {
        .variable => {
            const varSpec = ast.node(node.data.staticDecl.varSpec);
            const typeSpec = try genTypeSpecString(vm, ast, varSpec.data.varSpec.typeSpec);
            try vm.mapSet(entry, try vm.retainOrAllocAstring("typeSpec"), typeSpec);
        },
        .enum_t,
        .use_alias,
        .use_import,
        .template,
        .typeAlias => {},
        .funcInit,
        .func => {
            const headerv = try genNodeValue(vm, ast, node.data.func.header);
            try vm.mapSet(entry, try vm.retainOrAllocAstring("header"), headerv);
        },
        .table_t,
        .struct_t,
        .object => {
            const funcs = try vm.allocEmptyList();
            var cur: cy.NodeId = node.data.objectDecl.funcHead;
            while (cur != cy.NullNode) {
                const func = try genImplicitFuncDeclEntry(vm, ast, cur, state);
                try funcs.asHeapObject().list.append(vm.alloc, func);
                cur = ast.node(cur).next();
            }
            try vm.mapSet(entry, try vm.retainOrAllocAstring("funcs"), funcs);
        },
        .distinct_t => {
            const funcs = try vm.allocEmptyList();
            var cur: cy.NodeId = node.data.distinct_decl.func_head;
            while (cur != cy.NullNode) {
                const func = try genImplicitFuncDeclEntry(vm, ast, cur, state);
                try funcs.asHeapObject().list.append(vm.alloc, func);
                cur = ast.node(cur).next();
            }
            try vm.mapSet(entry, try vm.retainOrAllocAstring("funcs"), funcs);
        },
    }
    state.pos = node.srcPos;
    state.node = node;

    // Find doc comments.
    if (try genDocComment(vm, ast, decl.declT, state)) |docStr| {
        try vm.mapSet(entry, try vm.retainOrAllocAstring("docs"), docStr);
    }

    try vm.mapSet(entry, try vm.retainOrAllocAstring("name"), try vm.retainOrAllocAstring(name));
    try vm.mapSet(entry, try vm.retainOrAllocAstring("pos"), cy.Value.initInt(@intCast(node.srcPos)));
    return entryv;
}

fn genDocComment(vm: *cy.VM, ast: cy.ast.AstView, decl_type: cy.parser.StaticDeclType, state: *ParseCyberState) !?cy.Value {
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
            if (commentPos.len() < 3 or !std.mem.eql(u8, "--|", ast.src[commentPos.start..commentPos.start+3])) {
                // Not a doc comment, reset.
                docStartIdx = state.commentIdx;
                continue;
            }
            // Check it is connected to last comment.
            if (docEndIdx > docStartIdx + 1) {
                const last = comments[docEndIdx - 2];
                if (!ast.isAdjacentLine(last.end, commentPos.start)) {
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
                .variable => {
                    const varSpec = ast.node(state.node.data.staticDecl.varSpec);
                    if (varSpec.head.data.varSpec.modHead != cy.NullNode) {
                        const modifier = ast.node(varSpec.head.data.varSpec.modHead);
                        posWithModifiers = modifier.srcPos - 1;
                    }
                },
                .funcInit,
                .func => {
                    const header = ast.node(state.node.data.func.header);
                    if (header.funcHeader_modHead() != cy.NullNode) {
                        const modifier = ast.node(header.funcHeader_modHead());
                        posWithModifiers = modifier.srcPos - 1;
                    }
                },
                .object => {
                    const header = ast.node(state.node.data.objectDecl.header);
                    if (header.head.data.objectHeader.modHead != cy.NullNode) {
                        const modifier = ast.node(header.head.data.objectHeader.modHead);
                        posWithModifiers = modifier.srcPos - 1;
                    }
                },
                else => {},
            }

            if (ast.isAdjacentLine(last.end, posWithModifiers)) {
                for (comments[docStartIdx..docEndIdx]) |docPos| {
                    try state.sb.appendSlice(vm.alloc, ast.src[docPos.start+3..docPos.end]);
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

    const ast = parser.ast.view();

    for (parser.staticDecls.items) |decl| {
        const entry = try genDeclEntry(vm, ast, decl, &state);
        try declsList.append(vm.alloc, entry);
    }
    try map.put(vm.alloc, try vm.retainOrAllocAstring("decls"), decls);

    return root;
}

pub fn parseCyon(vm: *cy.VM, args: [*]const cy.Value, _: u8) anyerror!cy.Value {
    const src = args[0].asString();

    var parser = try cy.Parser.init(vm.alloc);
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

pub fn repl2(vm: *cy.VM, config: c.EvalConfig, read_line: IReplReadLine) !void {
    const alloc = vm.alloc;

    // TODO: Record inputs that successfully compiled. Can then be exported to file.

    // Initial input includes `use $global`.
    // Can also include additional source if needed.
    const init_src =
        \\use $global
        \\
        ;
    var res: c.Value = undefined;
    _ = c.evalExt(@ptrCast(vm), c.toStr("repl_init"), c.toStr(init_src), config, &res);

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
                    const report = c.newPanicSummary(@ptrCast(vm));
                    defer c.free(@ptrCast(vm), report);
                    rt.err(vm, c.fromStr(report));
                },
                error.CompileError => {
                    const report = c.newErrorReportSummary(@ptrCast(vm));
                    defer c.free(@ptrCast(vm), report);
                    rt.err(vm, c.fromStr(report));
                },
                else => {
                    rt.errZFmt(vm, "unexpected {}\n", .{err});
                },
            }
            break :b null;
        };

        if (val_opt) |val| {
            if (!val.isVoid()) {
                const str = c.newValueDump(@ptrCast(vm), @bitCast(val));
                defer c.free(@ptrCast(vm), str);
                rt.printZFmt(vm, "{s}\n", .{c.fromStr(str)});
            }
        }
    }
}

fn eval(vm: *cy.VM, args: [*]const cy.Value, _: u8) anyerror!cy.Value {
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

    const src = args[0].asString();
    var val: cy.Value = undefined;
    const res = c.evalExt(@ptrCast(ivm), c.toStr("eval"), c.toStr(src), config, @ptrCast(&val));
    if (res != c.Success) {
        switch (res) {
            c.ErrorCompile => {
                const report = c.newErrorReportSummary(@ptrCast(ivm));
                defer c.free(@ptrCast(ivm), report);
                rt.err(vm, c.fromStr(report));
            },
            c.ErrorPanic => {
                const report = c.newPanicSummary(@ptrCast(ivm));
                defer c.free(@ptrCast(ivm), report);
                rt.err(vm, c.fromStr(report));
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
