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
    const mod = C.createModule(@ptrCast(vm), C.toStr(r_uri), C.toStr(Src));

    const htype = C.hostTypeEntry;
    const types = [_]C.HostTypeEntry{
        htype("EvalConfig", C.DECL_TYPE_GET(&core_data.EvalConfigT)),
        htype("EvalResult", C.DECL_TYPE_GET(&core_data.EvalResultT)),
        htype("Value",      C.HOST_OBJECT_PRE(&core_data.ValueT, UserValue_getChildren, UserValue_finalizer)),
        htype("VM",         C.HOST_OBJECT(&core_data.VMT,    null, UserVM_finalizer)),
    };

    var config = C.ModuleConfig{
        .funcs = C.toSlice(C.HostFuncEntry, &funcs),
        .types = C.toSlice(C.HostTypeEntry, &types),
    };
    C.setModuleConfig(@ptrCast(vm), mod, &config);
    return mod;
}

const func = cy.hostFuncEntry;
const funcs = [_]C.HostFuncEntry{
    func("parse",              zErrFunc(parse)),
    func("parseCyon",          zErrFunc(parseCyon)),
    func("toCyon",             zErrFunc(toCyon)), 

    func("Value.dump",         zErrFunc(UserValue_dump)), 
    func("Value.getTypeId",    zErrFunc(UserValue_getTypeId)), 
    func("Value.toHost",       zErrFunc(UserValue_toHost)), 

    func("VM.eval",            zErrFunc(UserVM_eval)),
    func("VM.eval2",           zErrFunc(UserVM_evalExt)),
    func("VM.getErrorSummary", zErrFunc(UserVM_getErrorSummary)),
    func("VM.getPanicSummary", zErrFunc(UserVM_getPanicSummary)),
    func("VM.new",             zErrFunc(UserVM_new)),
};

const UserVM = struct {
    vm: *cy.VM,
};

pub fn UserVM_new(vm: *cy.VM) anyerror!cy.Value {
    const core_data = vm.getData(*cy.builtins.CoreData, "core");

    // Create an isolated VM.
    const new: *cy.VM = @ptrCast(@alignCast(C.create()));

    // Use the same printers as the parent VM.
    C.setPrinter(@ptrCast(new), C.getPrinter(@ptrCast(vm)));
    C.setErrorPrinter(@ptrCast(new), C.getErrorPrinter(@ptrCast(vm)));

    const uvm: *UserVM = @ptrCast(@alignCast(try cy.heap.allocHostNoCycObject(vm, core_data.VMT, @sizeOf(UserVM))));
    uvm.* = .{
        .vm = new,
    };
    return cy.Value.initHostNoCycPtr(uvm);
}

pub fn UserVM_getErrorSummary(vm: *cy.VM) anyerror!cy.Value {
    const uvm = vm.getValue(0).castHostObject(*UserVM);
    const cvm: *C.ZVM = @ptrCast(uvm.vm);
    const summary = cvm.newErrorReportSummary();
    defer cvm.free(summary);
    return vm.allocString(summary);
}

pub fn UserVM_getPanicSummary(vm: *cy.VM) anyerror!cy.Value {
    const uvm = vm.getValue(0).castHostObject(*UserVM);
    const cvm: *C.ZVM = @ptrCast(uvm.vm);
    const summary = cvm.newPanicSummary();
    defer cvm.free(summary);
    return vm.allocString(summary);
}

pub fn UserValue_dump(vm: *cy.VM) anyerror!cy.Value {
    const uval = vm.getValue(0).castHostObject(*UserValue);
    const uvm = uval.vm.castHostObject(*UserVM);
    const cvm: *C.ZVM = @ptrCast(uvm.vm);

    const str = cvm.newValueDump(uval.val);
    defer cvm.free(str);
    return vm.allocString(str);
}

pub fn UserValue_getTypeId(vm: *cy.VM) anyerror!cy.Value {
    const uval = vm.getValue(0).castHostObject(*UserValue);
    const val: cy.Value = @bitCast(uval.val);
    return cy.Value.initInt(@intCast(val.getTypeId()));
}

pub fn UserValue_toHost(vm: *cy.VM) anyerror!cy.Value {
    const uval = vm.getValue(0).castHostObject(*UserValue);
    const val: cy.Value = @bitCast(uval.val);
    switch (val.getTypeId()) {
        bt.Void => return cy.Value.False,
        bt.Integer => {
            return vm.allocInt(val.asBoxInt());
        },
        bt.Float,
        bt.Boolean => {
            return val;
        },
        bt.String => {
            return vm.allocString(val.asString());
        },
        else => {
            return error.InvalidResult;
        }
    }
}

pub fn UserVM_eval(vm: *cy.VM) anyerror!cy.Value {
    const core_data = vm.getData(*cy.builtins.CoreData, "core");
    const uvm = vm.getValue(0).castHostObject(*UserVM);
    const src = vm.getString(1);

    var res: C.Value = @bitCast(cy.Value.Void);
    const code = C.eval(@ptrCast(uvm.vm), C.toStr(src), &res);

    const value: *UserValue = @ptrCast(@alignCast(try cy.heap.allocHostNoCycObject(vm, core_data.ValueT, @sizeOf(UserValue))));
    vm.retain(vm.getValue(0));
    value.vm = vm.getValue(0);
    if (code == C.Success) {
        value.val = @bitCast(res);
    } else {
        value.val = @bitCast(cy.Value.Void);
    }

    const eval_res: *EvalResult = @ptrCast(@alignCast(try cy.heap.allocHostNoCycObject(vm, core_data.EvalResultT, @sizeOf(EvalResult))));
    eval_res.* = .{
        .code = cy.Value.initInt(@intCast(code)),
        .value = cy.Value.initHostNoCycPtr(value),
    };
    return cy.Value.initHostNoCycPtr(eval_res);
}

pub fn UserVM_evalExt(vm: *cy.VM) anyerror!cy.Value {
    const core_data = vm.getData(*cy.builtins.CoreData, "core");
    const uvm = vm.getValue(0).castHostObject(*UserVM);
    const uri = vm.getString(1);
    const src = vm.getString(2);
    const config = &vm.getValue(3).asHeapObject().object;

    const config_c = C.EvalConfig{
        .single_run = (try vm.getObjectField(config, "single_run")).asBool(),
        .file_modules = (try vm.getObjectField(config, "file_modules")).asBool(),
        .gen_all_debug_syms = (try vm.getObjectField(config, "gen_all_debug_syms")).asBool(),
        .backend = (try vm.getObjectField(config, "backend")).getEnumValue(),
        .reload = (try vm.getObjectField(config, "reload")).asBool(),
        .spawn_exe = (try vm.getObjectField(config, "spawn_exe")).asBool(),
    };

    var res: C.Value = @bitCast(cy.Value.Void);
    const code = C.evalExt(@ptrCast(uvm.vm), C.toStr(uri), C.toStr(src), config_c, &res);

    const value: *UserValue = @ptrCast(@alignCast(try cy.heap.allocHostNoCycObject(vm, core_data.ValueT, @sizeOf(UserValue))));
    vm.retain(vm.getValue(0));
    value.vm = vm.getValue(0);
    value.val = @bitCast(res);

    const eval_res: *EvalResult = @ptrCast(@alignCast(try cy.heap.allocHostNoCycObject(vm, core_data.EvalResultT, @sizeOf(EvalResult))));
    eval_res.* = .{
        .code = cy.Value.initInt(@intCast(code)),
        .value = cy.Value.initHostNoCycPtr(value),
    };
    return cy.Value.initHostNoCycPtr(eval_res);
}

pub fn UserVM_finalizer(_: ?*C.VM, obj: ?*anyopaque) callconv(.C) void {
    const uvm: *UserVM = @ptrCast(@alignCast(obj));
    @as(*C.ZVM, @ptrCast(uvm.vm)).destroy();
}

const UserValue = extern struct {
    vm: cy.Value,
    val: u64, 
};

const EvalResult = extern struct {
    code: cy.Value,
    value: cy.Value,
};

pub fn UserValue_getChildren(_: ?*C.VM, obj: ?*anyopaque) callconv(.C) C.ValueSlice {
    const value: *UserValue = @ptrCast(@alignCast(obj));
    return .{
        .ptr = @ptrCast(&value.vm),
        .len = 1,
    };
}

pub fn UserValue_finalizer(_: ?*C.VM, obj: ?*anyopaque) callconv(.C) void {
    const value: *UserValue = @ptrCast(@alignCast(obj));
    const ivm = value.vm.castHostObject(*UserVM);
    ivm.vm.release(@bitCast(value.val));
}

pub fn parse(vm: *cy.VM) anyerror!cy.Value {
    const src = vm.getString(0);

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
        .func_param => {
            const param = node.cast(.func_param);
            const name = view.nodeString(param.name_type);
            try vm.mapSet(map, try vm.retainOrAllocAstring("name"), try vm.allocString(name));

            const typeSpec = try genTypeSpecString(vm, view, param.type);
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
    try vm.mapSet(entry, try vm.retainOrAllocAstring("pos"), try vm.allocInt(@intCast(node.pos)));
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
        .typeAliasDecl => {},
        .template => {
            const template = decl.cast(.template);
            const child = try genDeclEntry(vm, view, template.child_decl, state);
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
        .structDecl => {
            const struct_decl = decl.cast(.structDecl);
            const funcs_ = try vm.allocEmptyListDyn();
            for (struct_decl.funcs) |func_decl| {
                const f = try genImplicitFuncDeclEntry(vm, view, func_decl, state);
                try funcs_.asHeapObject().list.append(vm.alloc, f);
            }
            try vm.mapSet(entry, try vm.retainOrAllocAstring("funcs"), funcs_);
        },
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
        .trait_decl => {
            const trait_decl = decl.cast(.trait_decl);
            const funcs_ = try vm.allocEmptyListDyn();
            for (trait_decl.funcs) |func_decl| {
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
    try vm.mapSet(entry, try vm.retainOrAllocAstring("pos"), try vm.allocInt(@intCast(decl.pos())));
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

pub fn parseCyon(vm: *cy.VM) anyerror!cy.Value {
    const src = vm.getString(0);

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
            return vm.allocInt(try val.getInt());
        },
        .float => {
            return cy.Value.initF64(try val.getF64());
        },
        .bool => {
            return cy.Value.initBool(val.getBool());
        },
    }
}

pub fn toCyon(vm: *cy.VM) anyerror!cy.Value {
    const res = try allocToCyon(vm, vm.alloc, vm.getValue(0));
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
                        try ctx.encodeInt(key, e.value.asBoxInt());
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
                        try ctx.encodeInt(it.asBoxInt());
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
                        try ctx.encodeInt(val.asBoxInt());
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
                            _ = try ctx.writer.write("{_}");
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